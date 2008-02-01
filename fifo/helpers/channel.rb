class Channel
  CLASS_TURN_BASED = 0
  CLASS_BATCH      = 1
  CLASS_REALTIME   = 2

  # Login result codes
  LOGIN_OK                          = 0
  LOGIN_CONNECTION_ERROR            = 1
  LOGIN_WRONG_CAPTCHA               = 2
  LOGIN_DIFFERENT_CONTAINER_VERSION = 3
  LOGIN_DIFFERENT_GAME_VERSION      = 4
  LOGIN_DUPLICATE_NICK              = 5

  # Not used here, used by the Fifo manager
  #LOGIN_OLD_CONTAINER_VERSION       = 6
  #LOGIN_NO_GAME                     = 7
  #LOGIN_OLD_GAME_VERSION            = 8

  NICK_MAX    = 32
  NICK_FORMAT = /^([a-zA-Z0-9_-])+$/

  @@channels = {}

  # ----------------------------------------------------------------------------

  # out: [code, snapshot]
  # Returns the channel this player has logged in
  def self.login(player, arg)
    container_version = arg[0].to_i
    game_id           = arg[1].to_i
    game_version      = arg[2].to_i
    channel_name      = arg[3].to_s
    captcha_code      = arg[4].to_s
    encrypted_code    = arg[5].to_s
    nick              = arg[6].to_s.strip
    batch_game        = (arg[7] == CLASS_BATCH)? true : false

    code = validate(player, container_version, game_id, game_version,
        captcha_code, encrypted_code, nick)

    if code == LOGIN_OK
      player.nick = nick
      name = "#{game_id}/#{channel_name}"
      channel = @@channels[name]
      if channel.nil?
        channel = Channel.new(name, player, container_version, game_version, batch_game)
        @@channels[name] = channel
      else
        code = channel.login(player, container_version, game_version, batch_game)
      end
    end

    if code != LOGIN_OK
      player.invoke(Player::CMD_LOGIN, [code, nil])
      player.close_connection_after_writing
      return nil
    else
      return channel
    end
  end

  # Returns login code.
  def self.validate(player, container_version, game_id, game_version,
      captcha_code, encrypted_code, nick)
    # Security
    # game_id can be negative when developing games on localhost
    if game_id < 0 and player.remote_ip != '127.0.0.1'
      player.close_connection
      return LOGIN_NO_GAME
    end

    code = LOGIN_OK

    # Nick
    code = LOGIN_DUPLICATE_NICK if nick.empty? or nick.length > NICK_MAX or nick !~ NICK_FORMAT

    # Captcha
    code = LOGIN_WRONG_CAPTCHA if code == LOGIN_OK and !Captcha.instance.correct?(captcha_code, encrypted_code)

    # Versions and game
    begin
      code = Proxy.instance.check_login(container_version, game_id, game_version) if code == LOGIN_OK and game_id >= 0
    rescue DRb::DRbConnError
      code = LOGIN_SERVER_ERROR
    end

    code
  end

  # ----------------------------------------------------------------------------

  def initialize(name, player, container_version, game_version, batch_game)
    @name = name  # Used to delete this channel
    @lobby = Lobby.new(player)
    player.room = @lobby
    @rooms = []

    @container_version = container_version
    @game_version      = game_version
    @batch_game        = batch_game

    player.invoke(Player::CMD_LOGIN, [LOGIN_OK, snapshot])
  end

  # Returns login code.
  def login(player, container_version, game_version, batch_game)
    return LOGIN_DUPLICATE_NICK if @lobby.nicks.include?(player.nick)
    @rooms.each { |r| return LOGIN_DUPLICATE_NICK if r.nicks.include?(player.nick) }
    return LOGIN_DIFFERENT_CONTAINER_VERSION if container_version != @container_version
    return LOGIN_DIFFERENT_GAME_VERSION if game_version != @game_version
    return LOGIN_DIFFERENT_GAME_VERSION if batch_game != @batch_game

    @lobby.process(player, Player::CMD_LOGIN, nil)
    player.room = @lobby
    player.invoke(Player::CMD_LOGIN, [LOGIN_OK, snapshot])
    LOGIN_OK
  end

  # ----------------------------------------------------------------------------

  def process(player, cmd, arg)
    # Cross-room related commands are processed here, the others are processed
    # in the room the player is currently in (lobby or game room)
    if cmd == Player::CMD_LOGOUT
      logout(player)
    elsif cmd == Player::CMD_ROOM_ENTER
      room_enter(player, arg)
    elsif cmd == Player::CMD_ROOM_LEAVE
      room_leave(player, arg)
    else
      player.room.process(player, cmd, arg)
    end
  end

private

  def logout(player)
    if player.room != @lobby
      iroom = @rooms.index(player.room)

      player.room.process(player, Player::CMD_ROOM_LEAVE, nil)
      if player.room.nicks.empty?
        @rooms.delete_at(iroom)
      end

      @lobby.process(player, Player::CMD_LOGOUT, iroom)
    else
      @lobby.process(player, Player::CMD_LOGOUT, nil)
    end

    if @rooms.empty? and @lobby.nicks.empty?
      @@channels.delete(@name)
    end
  end

  # in: iroom, negative to create a new room
  # out:
  # * For the players who are in the room:  nick
  # * For the player who entered the room:  room snapshot
  # * For the players who are in the lobby: [iroom, nick]
  def room_enter(player, arg)
    iroom = arg
    if iroom < 0
      room = Room.new(player, @batch_game)
      @rooms << room
      iroom = @rooms.size - 1
    else
      room = @rooms[iroom]
      room.process(player, Player::CMD_ROOM_ENTER, nil)
    end
    @lobby.process(player, Player::CMD_ROOM_ENTER, iroom)
  end

  # out:
  # * For the players who are in the room:  nick
  # * For the players who are in the lobby: [iroom, nick]
  # * For the player who left:              room snapshot
  def room_leave(player, arg)
    if player.room == @lobby
      $LOGGER.debug('@channel: room_leave: No room to leave')
      player.close_connection
      return
    end

    iroom = @rooms.index(player.room)

    player.room.process(player, Player::CMD_ROOM_LEAVE, nil)
    if player.room.nicks.empty?
      @rooms.delete_at(iroom)
    end

    @lobby.process(player, Player::CMD_ROOM_LEAVE, iroom)

    player.room = @lobby
    player.invoke(Player::CMD_ROOM_LEAVE, snapshot)
  end

  # out: [[nicks in lobby], [nicks in room0], [nicks in room1]...]
  def snapshot
    a = [@lobby.nicks]
    a.concat(@rooms.map { |r| r.nicks })
    a
  end
end