class Room < Scope
  # States
  NEWABLE   = 0
  NEW       = 1
  PLAY      = 2
  DRAW      = 3
  GAME_OVER = 4

  NEW_TIMEOUT = 30  # [s]

  def initialize(client, batch_game)
    super

    @clients = [client]
    @batch_game = batch_game

    @state = NEWABLE

    # NEW
    @new_created_at   = nil
    @base_config      = nil
    @extended_config  = nil
    @playing_clients0 = []

    # PLAY
    @playing_clients = []  # Set to @playing_clients0 when the game starts
    @judgers = []          # Set to @clients when the game starts
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
      Server::CMD_NEW_CONF   => method('new_config'),
      Server::CMD_NEW_JOIN     => method('new_join'),
      Server::CMD_NEW_UNJOIN   => method('new_unjoin'),
      Server::CMD_NEW_TIMEOUT  => method('new_timeout'),
      Server::CMD_PLAY_MOVE    => method('play_move'),
      Server::CMD_PLAY_RESIGN  => method('play_resign'),
      Server::CMD_PLAY_TIMEOUT => method('play_timeout'),
      Server::CMD_GAME_OVER    => method('game_over')
    }

    client.session[:room] = self
    client.call(Server::CMD_ROOM_ENTER, snapshot)
  end

  def process(client, cmd, value)
    self.synchronize do
      m = @methods[cmd]
      if m.nil?
        $logger.debug("@lobby: Invalid command: #{cmd}")
        client.close_connection
      else
        m.call(client, value)
      end
    end
  end

  def nicks
    return @clients.map { |p| p.session[:nick] }
  end

  def remote_ips
    self.synchronize do
      @clients.map { |p| p.remote_ip }
    end
  end

private

  def will_close(client, value)
  end

  # in: iroom
  # out:
  #   for the client who entered: snapshot
  #   for the others: nick
  def enter(client, value)
    @clients.each do |p|
      p.call(Server::CMD_ROOM_ENTER, client.session[:nick])
    end

    @clients << client

    client.session[:room] = self
    client.call(Server::CMD_ROOM_ENTER, snapshot)
  end

  # out: nick
  def leave(client, value)
    @clients.delete(client)
    @judgers.delete(client)

    # Notify joining/playing clients (CMD_NEW_UNJOIN or CMD_PLAY_RESIGN)
    if @playing_clients.include?(client)
      if @state == NEW
        new_unjoin(client, nil)
      elsif @state == PLAY
        play_resign(client, nil)
      end
    end

    # Notify all clients
    @clients.each do |p|
      p.call(Server::CMD_ROOM_LEAVE, client.session[:nick])
    end
  end

  def chat(client, value)
    msg = value
    index = @clients.index(client)
    @clients.each do |p|
      p.call(Server::CMD_CHAT, [index, msg])
    end
  end

  def new_config(client, value)
    return if @state != NEWABLE

    a_base_config = value[0]
    @base_config = {
      :n_clients    => a_base_config[0].to_i,
      :move_sec => a_base_config[1].to_i,
      :total_min    => a_base_config[2].to_i
    }
    # Validate, just in case
    if @base_config[:n_clients] < 2 or @base_config[:move_sec] < 0 or @base_config[:total_min] < 0
      logout(client)
      return
    end

    @state = NEW
    @new_created_at = Time.now
    @extended_config = value[1]
    @playing_clients0 = [client]

    @clients.each do |p|
      p.call(Server::CMD_NEW_CONF, [client.session[:nick], a_base_config, @extended_config])
    end
  end

  def new_join(client, value)
    return if @state != NEW
    if @playing_clients0.include?(client)
      client.close_connection
      return
    end

    @playing_clients0 << client
    if @playing_clients0.size == @base_config[:n_clients]
      @state = PLAY
      @playing_clients = @playing_clients0.dup
      @judgers = @clients.dup  # Judgers are the ones who watch the game from the start
      @play_created_at = Time.now
      @game_snapshot = nil
      if @batch_game
        @batch_move.new_game(@playing_clients0.size)
        @last_batch_move_sent = Time.now
      end
    end

    @clients.each do |p|
      p.call(Server::CMD_NEW_JOIN, client.session[:nick])
    end
  end

  def new_unjoin(client, value)
    return if @state != NEW
    unless @playing_clients0.include?(client)
      client.close_connection
      return
    end

    @playing_clients0.delete(client)
    @state = NEWABLE if @playing_clients0.empty?

    @clients.each do |p|
      p.call(Server::CMD_NEW_UNJOIN, client.session[:nick])
    end
  end

  def new_timeout(client, value)
    return if @state != NEW
    if Time.now - @new_created_at < NEW_TIMEOUT
      client.close_connection
      return
    end

    @state = NEWABLE

    @clients.each do |p|
      p.call(Server::CMD_NEW_TIMEOUT, nil)
    end
  end

  def play_move(client, value)
    return if @state != PLAY
    unless @playing_clients.include?(client)
      client.close_connection
      return
    end

    @game_snapshot = value[0]

    index = @playing_clients0.index(client)
    if !play_move_util(index, value[1])
      @batch_game ?
      play_move_batch(index, value[1]) :
      play_move_immediate(index, value[1])
    end
  end

  def play_resign(client, value)
    return if @state != PLAY
    unless @playing_clients.include?(client)
      client.close_connection
      return
    end

    @playing_clients.delete(client)
    if @playing_clients.empty?
      @state = NEWABLE
      return
    end

    index = @playing_clients0.index(client)
    a = [Time.now - @play_created_at, index]
    @clients.each do |p|
      p.call(Server::CMD_PLAY_RESIGN, a)
    end

    @last_action = [Server::CMD_PLAY_RESIGN, index, nil]

    # Broadcast for batch game if the batch is full with the new size
    broadcast_batch if @batch_game and @batch_move.resign(index)
  end

  def play_timeout(client, value)
    return if @state != PLAY
    unless @playing_clients.include?(client)
      client.close_connection
      return
    end

    index = @playing_clients0.index(client)

    @last_action = [Server::CMD_PLAY_TIMEOUT, index, nil]

    if @batch_game
      play_timeout_batch(index)
    else
      play_timeout_immediate(index)
    end
  end

  def game_over(client, value)
    if @state == GAME_OVER
      unless @judgers.include?(client)
        client.close_connection
        return
      end

      # TODO: phase2: collect results and judge later
      #collect_results(client, value)
      return
    end

    if @state != PLAY
      client.close_connection
      return
    end

    @state = GAME_OVER
    # TODO: phase2: collect results and judge later
    #collect_results(client, value)
    EventMachine::add_timer(CONF[:game_over_delay]) do
      # TODO: phase2: collect results and judge later
      #judge_results
      @state = NEWABLE
      @clients.each do |p|
        p.call(Server::CMD_GAME_OVER, nil)
      end
    end
  end

private

  # Returns [state, nicks, baseConfig, extendedConfig, playNicks0, gameSnapshot, actions]
  def snapshot
    a_base_config = [
      @base_config[:n_clients],
      @base_config[:move_sec],
      @base_config[:total_min]
    ] unless @base_config.nil?
    [
      @state,
      nicks,
      a_base_config,
      @extended_config,
      @playing_clients0.map { |p| p.session[:nick] },
      nil,
      []
    ]
  end

  # @return true if the move is util
  def play_move_util(index, value)
    ret = Utils.instance.check(value)
    return false if ret.nil?

    a = [Time.now - @play_created_at, index, ret]
    @clients.each do |p|
      p.call(Server::CMD_PLAY_MOVE, a)
    end
    true
  end

  def play_move_immediate(index, value)
    a = [Time.now - @play_created_at, index, value]
    @clients.each do |p|
      p.call(Server::CMD_PLAY_MOVE, a)
    end
  end

  def play_move_batch(index, value)
    if @batch_move.include?(index)
      @clients[index].close_connection
      return
    end

    broadcast_batch if @batch_move.move(index, value)
  end

  # Only broadcast for playing clients.
  def play_timeout_immediate(index)
    a = [Time.now - @play_created_at, index]
    @playing_clients.each do |p|
      p.call(Server::CMD_PLAY_TIMEOUT, a)
    end
  end

  # Fill in nils for client who did not move and broadcast the batch move.
  def play_timeout_batch(index)
    client = @playing_clients0[index]

    # Check if it is game timeout
    now = Time.now
    if @base_config[:total_min] > 0 and
        now - @play_created_at >= @base_config[:total_min]*60
      game_over(client, nil)
      return
    end

    # Check if it is not move timeout
    dt = now - @last_batch_move_sent
    if dt < @base_config[:move_sec]
      client.close_connection if CONF[:max_lag] < dt
      return
    end

    # Prevent too many successive default moves
    if @batch_move.timeout
      @playing_clients.each do |p|
        p.close_connection
      end
    else
      broadcast_batch
    end
  end

  def broadcast_batch
    a = [Time.now - @play_created_at].concat(@batch_move.to_a)
    @batch_move.clear
    @clients.each do |p|
      p.call(Server::CMD_PLAY_MOVE, a)
    end
    @last_batch_move_sent = Time.now
  end


    # Called when a client enters a room (from the lobby).
  # in: iroom
  # out: [iroom, nick]
  def join(client)
    @clients.synchronize do
      @clients.each do |c|
        c.invoke(Server::CMD_ROOM_ENTER, [iroom, client.session[:nick]])
      end
      @clients << client
    end
  end

  # Called when a client leaves a room (to enter the lobby).
  def room_leave(client, iroom)
    @clients.each do |p|
      p.call(Server::CMD_ROOM_LEAVE, [iroom, client.session[:nick]])
    end
    @clients << client
  end

  # out: [[nicks in lobby], [nicks in room0], [nicks in room1]...]
  def snapshot
    a = [@lobby.nicks]
    a.concat(@rooms.map { |r| r.nicks })
    a
  end

  def chat(client, message)
    @clients.synchronize do
      index = @clientss.index(client)
      @clients.each do |c|
        c.invoke('chat', [index, message])
      end
    end
  end

end
