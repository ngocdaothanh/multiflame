class Room
  MAX_LAG = 5  # [s]

  # States
  NEWABLE   = 0
  NEW       = 1
  PLAY      = 2
  DRAW      = 3
  GAME_OVER = 4

  NEW_TIMEOUT = 30  # [s]

  def initialize(player, batch_game)
    @mutex_self = Mutex.new
    @players = [player]
    @batch_game = batch_game

    @state = NEWABLE

    # NEW
    @new_created_at   = nil
    @base_config      = nil
    @extended_config  = nil
    @playing_players0 = []

    # PLAY
    @playing_players = []  # Set to @playing_players0 when the game starts
    @play_actions = PlayActions.new
    if @batch_game
      @batch_move = BatchMove.new
      @last_batch_move_sent = nil
    end

    @methods = {
      Player::CMD_WILL_CLOSE   => method('will_close'),
      Player::CMD_ROOM_ENTER   => method('enter'),
      Player::CMD_ROOM_LEAVE   => method('leave'),
      Player::CMD_CHAT         => method('chat'),
      Player::CMD_NEW_INIT     => method('new_init'),
      Player::CMD_NEW_JOIN     => method('new_join'),
      Player::CMD_NEW_UNJOIN   => method('new_unjoin'),
      Player::CMD_NEW_TIMEOUT  => method('new_timeout'),
      Player::CMD_PLAY_MOVE    => method('play_move'),
      Player::CMD_PLAY_RESIGN  => method('play_resign'),
      Player::CMD_PLAY_TIMEOUT => method('play_timeout'),
      Player::CMD_GAME_OVER    => method('game_over'),
      Player::CMD_RESULT       => method('result')
    }

    player.room = self
    player.invoke(Player::CMD_ROOM_ENTER, snapshot)
  end

  def process(player, cmd, arg)
    @mutex_self.synchronize do
      m = @methods[cmd]
      if m.nil?
        $LOGGER.debug("@lobby: Invalid command: #{cmd}")
        player.close_connection
      else
        m.call(player, arg)
      end
    end
  end

  def nicks
    return @players.map { |p| p.nick }
  end

  def remote_ips
    @mutex_self.synchronize do
      @players.map { |p| p.remote_ip }
    end
  end

private

  def will_close(player, arg)
  end

  # in: iroom
  # out:
  #   for the player who entered: snapshot
  #   for the others: nick
  def enter(player, arg)
    @players.each do |p|
      p.invoke(Player::CMD_ROOM_ENTER, player.nick)
    end

    @players << player

    player.room = self
    player.invoke(Player::CMD_ROOM_ENTER, snapshot)
  end

  # out: nick
  def leave(player, arg)
    @players.delete(player)

    # Notify joining/playing players (CMD_NEW_UNJOIN or CMD_PLAY_RESIGN)
    if @playing_players.include?(player)
      if @state == NEW
        new_unjoin(player, nil)
      elsif @state == PLAY
        play_resign(player, nil)
      end
    end

    # Notify all players
    @players.each do |p|
      p.invoke(Player::CMD_ROOM_LEAVE, player.nick)
    end
  end

  def chat(player, arg)
    msg = arg
    index = @players.index(player)
    @players.each do |p|
      p.invoke(Player::CMD_CHAT, [index, msg])
    end
  end

  def new_init(player, arg)
    return if @state != NEWABLE

    a_base_config = arg[0]
    @base_config = {
      :n_players    => a_base_config[0].to_i,
      :move_sec => a_base_config[1].to_i,
      :total_min    => a_base_config[2].to_i
    }
    # Validate, just in case
    if @base_config[:n_players] < 2 or @base_config[:move_sec] < 0 or @base_config[:total_min] < 0
      logout(player)
      return
    end

    @state = NEW
    @new_created_at = Time.now
    @extended_config = arg[1]
    @playing_players0 = [player]

    @players.each do |p|
      p.invoke(Player::CMD_NEW_INIT, [player.nick, a_base_config, @extended_config])
    end
  end

  def new_join(player, arg)
    return if @state != NEW
    if @playing_players0.include?(player)
      player.close_connection
      return
    end

    @playing_players0 << player
    if @playing_players0.size == @base_config[:n_players]
      @state = PLAY
      @playing_players = @playing_players0.dup
      @play_actions.new_game
      if @batch_game
        @batch_move.new_game(@playing_players0.size)
        @last_batch_move_sent = Time.now
      end
    end

    @players.each do |p|
      p.invoke(Player::CMD_NEW_JOIN, player.nick)
    end
  end

  def new_unjoin(player, arg)
    return if @state != NEW
    unless @playing_players0.include?(player)
      player.close_connection
      return
    end

    @playing_players0.delete(player)
    @state = NEWABLE if @playing_players0.empty?

    @players.each do |p|
      p.invoke(Player::CMD_NEW_UNJOIN, player.nick)
    end
  end

  def new_timeout(player, arg)
    return if @state != NEW
    if Time.now - @new_created_at < NEW_TIMEOUT
      player.close_connection
      return
    end

    @state = NEWABLE

    @players.each do |p|
      p.invoke(Player::CMD_NEW_TIMEOUT, nil)
    end
  end

  def play_move(player, arg)
    return if @state != PLAY
    unless @playing_players.include?(player)
      player.close_connection
      return
    end

    index = @playing_players0.index(player)
    if !play_move_util(index, arg)
      @batch_game ?
      play_move_batch(index, arg) :
      play_move_immediate(index, arg)
    end
  end

  def play_resign(player, arg)
    return if @state != PLAY
    unless @playing_players.include?(player)
      player.close_connection
      return
    end

    @playing_players.delete(player)
    if @playing_players.empty?
      @state = NEWABLE
      return
    end

    index = @playing_players0.index(player)
    a = @play_actions.resign(index)

    @players.each do |p|
      p.invoke(Player::CMD_PLAY_RESIGN, a)
    end

    # Broadcast for batch game if the batch is full with the new size
    broadcast_batch if @batch_game and @batch_move.resign(index)
  end

  def play_timeout(player, arg)
    return if @state != PLAY
    unless @playing_players.include?(player)
      player.close_connection
      return
    end

    index = @playing_players0.index(player)
    if @batch_game
      play_timeout_batch(index)
    else
      play_timeout_immediate(index)
    end
  end

  def game_over(player, arg)
    return if @state != PLAY
    unless @playing_players.include?(player)
      player.close_connection
      return
    end

    @state = GAME_OVER
    @players.each do |p|
      p.invoke(Player::CMD_GAME_OVER, nil)
    end
    judge(player)
  end

  # TODO:
  # * Validate arg, the code should be in each player
  # * Judge mutiagently
  def result(player, arg)
    return if @state != GAME_OVER

    @state = NEWABLE
    @playing_players0.clear
    @playing_players.clear
    @play_actions.clear
    @batch_move.clear if @batch_game

    @players.each do |p|
      p.invoke(Player::CMD_RESULT, arg)
    end
  end

private

  # Returns [state, nicks, baseConfig, extendedConfig, playNicks0, moves]
  def snapshot
    a_base_config = [
      @base_config[:n_players],
      @base_config[:move_sec],
      @base_config[:total_min]
    ] unless @base_config.nil?
    [
      @state,
      nicks,
      a_base_config,
      @extended_config,
      @playing_players0.map { |p| p.nick },
      @play_actions.to_a
    ]
  end

  # @return true if the move is util
  def play_move_util(index, arg)
    ret = Utils.instance.check(arg)
    return false if ret.nil?

    a = @play_actions.move_immediate(index, ret)
    @players.each do |p|
      p.invoke(Player::CMD_PLAY_MOVE, a)
    end
    true
  end

  def play_move_immediate(index, arg)
    a = @play_actions.move_immediate(index, arg)
    @players.each do |p|
      p.invoke(Player::CMD_PLAY_MOVE, a)
    end
  end

  def play_move_batch(index, arg)
    if @batch_move.include?(index)
      @players[index].close_connection
      return
    end

    broadcast_batch if @batch_move.move(index, arg)
  end

  def play_timeout_immediate(index)
    a = @play_actions.timeout(index)
    @players.each do |p|
      p.invoke(Player::CMD_PLAY_TIMEOUT, a)
    end
  end

  def play_timeout_batch(index)
    player = @playing_players0[index]

    # Check if it is game timeout
    now = Time.now
    if @base_config[:total_min] > 0 and
        now - @play_actions.play_created_at >= @base_config[:total_min]*60
      game_over(player, nil)
      return
    end

    # Check if it is not move timeout
    dt = now - @last_batch_move_sent
    if dt < @base_config[:move_sec]
      player.close_connection if MAX_LAG < dt
      return
    end

    # Fill in nulls for player who did not move and broadcast the batch
    if @batch_move.timeout
      @playing_players.each do |p|
        p.close_connection
      end
    else
      broadcast_batch
    end
  end

  def broadcast_batch
    a = @play_actions.move_batch(@batch_move)
    @batch_move.clear
    @players.each do |p|
      p.invoke(Player::CMD_PLAY_MOVE, a)
    end
    @last_batch_move_sent = Time.now
  end

  def judge(reporter)
    # TODO: enhance
    judgers = @players.dup
    a_base_config = [
      @base_config[:n_players],
      @base_config[:move_sec],
      @base_config[:total_min]
    ]
    judgers.each do |j|
      j.invoke(Player::CMD_JUDGE, [
        a_base_config,
        @extended_config,
        @play_actions.to_a,
        @playing_players0.index(reporter)
      ])
    end
  end
end