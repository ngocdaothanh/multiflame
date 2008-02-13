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
    @play_created_at = nil
    if @batch_game
      @batch_move = BatchMove.new
      @last_batch_move_sent = nil
    end

    @methods = {
      Server::CMD_WILL_CLOSE   => method('will_close'),
      Server::CMD_ROOM_ENTER   => method('enter'),
      Server::CMD_ROOM_LEAVE   => method('leave'),
      Server::CMD_CHAT         => method('chat'),
      Server::CMD_NEW_INIT     => method('new_init'),
      Server::CMD_NEW_JOIN     => method('new_join'),
      Server::CMD_NEW_UNJOIN   => method('new_unjoin'),
      Server::CMD_NEW_TIMEOUT  => method('new_timeout'),
      Server::CMD_PLAY_MOVE    => method('play_move'),
      Server::CMD_PLAY_RESIGN  => method('play_resign'),
      Server::CMD_PLAY_TIMEOUT => method('play_timeout'),
      Server::CMD_GAME_OVER    => method('game_over')
    }

    player.property[:room] = self
    player.call(Server::CMD_ROOM_ENTER, snapshot)
  end

  def process(player, cmd, value)
    @mutex_self.synchronize do
      m = @methods[cmd]
      if m.nil?
        $LOGGER.debug("@lobby: Invalid command: #{cmd}")
        player.close_connection
      else
        m.call(player, value)
      end
    end
  end

  def nicks
    return @players.map { |p| p.property[:nick] }
  end

  def remote_ips
    @mutex_self.synchronize do
      @players.map { |p| p.remote_ip }
    end
  end

private

  def will_close(player, value)
  end

  # in: iroom
  # out:
  #   for the player who entered: snapshot
  #   for the others: nick
  def enter(player, value)
    @players.each do |p|
      p.call(Server::CMD_ROOM_ENTER, player.property[:nick])
    end

    @players << player

    player.property[:room] = self
    player.call(Server::CMD_ROOM_ENTER, snapshot)
  end

  # out: nick
  def leave(player, value)
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
      p.call(Server::CMD_ROOM_LEAVE, player.property[:nick])
    end
  end

  def chat(player, value)
    msg = value
    index = @players.index(player)
    @players.each do |p|
      p.call(Server::CMD_CHAT, [index, msg])
    end
  end

  def new_init(player, value)
    return if @state != NEWABLE

    a_base_config = value[0]
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
    @extended_config = value[1]
    @playing_players0 = [player]

    @players.each do |p|
      p.call(Server::CMD_NEW_INIT, [player.property[:nick], a_base_config, @extended_config])
    end
  end

  def new_join(player, value)
    return if @state != NEW
    if @playing_players0.include?(player)
      player.close_connection
      return
    end

    @playing_players0 << player
    if @playing_players0.size == @base_config[:n_players]
      @state = PLAY
      @playing_players = @playing_players0.dup
      @play_created_at = Time.now
      if @batch_game
        @batch_move.new_game(@playing_players0.size)
        @last_batch_move_sent = Time.now
      end
    end

    @players.each do |p|
      p.call(Server::CMD_NEW_JOIN, player.property[:nick])
    end
  end

  def new_unjoin(player, value)
    return if @state != NEW
    unless @playing_players0.include?(player)
      player.close_connection
      return
    end

    @playing_players0.delete(player)
    @state = NEWABLE if @playing_players0.empty?

    @players.each do |p|
      p.call(Server::CMD_NEW_UNJOIN, player.property[:nick])
    end
  end

  def new_timeout(player, value)
    return if @state != NEW
    if Time.now - @new_created_at < NEW_TIMEOUT
      player.close_connection
      return
    end

    @state = NEWABLE

    @players.each do |p|
      p.call(Server::CMD_NEW_TIMEOUT, nil)
    end
  end

  def play_move(player, value)
    return if @state != PLAY
    unless @playing_players.include?(player)
      player.close_connection
      return
    end

    index = @playing_players0.index(player)
    if !play_move_util(index, value)
      @batch_game ?
      play_move_batch(index, value) :
      play_move_immediate(index, value)
    end
  end

  def play_resign(player, value)
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
    a = [Time.now - @play_created_at, index]
    @players.each do |p|
      p.call(Server::CMD_PLAY_RESIGN, a)
    end

    # Broadcast for batch game if the batch is full with the new size
    broadcast_batch if @batch_game and @batch_move.resign(index)
  end

  def play_timeout(player, value)
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

  def game_over(player, value)
    if @state == GAME_OVER
      # TODO: phase2: collect results and judge later
      return
    end

    if @state != PLAY
      player.close_connection
      return
    end

    @state = GAME_OVER
    EventMachine::add_timer(CONFIG[:game_over_delay]) do
      @state = NEWABLE
      @players.each do |p|
        p.call(Server::CMD_GAME_OVER, nil)
      end
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
      @playing_players0.map { |p| p.property[:nick] },
      nil  # <--- TO____DO
    ]
  end

  # @return true if the move is util
  def play_move_util(index, value)
    ret = Utils.instance.check(value)
    return false if ret.nil?

    a = [Time.now - @play_created_at, index, ret]
    @players.each do |p|
      p.call(Server::CMD_PLAY_MOVE, a)
    end
    true
  end

  def play_move_immediate(index, value)
    a = [Time.now - @play_created_at, index, value]
    @players.each do |p|
      p.call(Server::CMD_PLAY_MOVE, a)
    end
  end

  def play_move_batch(index, value)
    if @batch_move.include?(index)
      @players[index].close_connection
      return
    end

    broadcast_batch if @batch_move.move(index, value)
  end

  def play_timeout_immediate(index)
    a = [Time.now - @play_created_at, index]
    @players.each do |p|
      p.call(Server::CMD_PLAY_TIMEOUT, a)
    end
  end

  def play_timeout_batch(index)
    player = @playing_players0[index]

    # Check if it is game timeout
    now = Time.now
    if @base_config[:total_min] > 0 and
        now - @play_created_at >= @base_config[:total_min]*60
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
    a = [Time.now - @play_created_at].concat(@batch_move.to_a)
    @batch_move.clear
    @players.each do |p|
      p.call(Server::CMD_PLAY_MOVE, a)
    end
    @last_batch_move_sent = Time.now
  end
end
