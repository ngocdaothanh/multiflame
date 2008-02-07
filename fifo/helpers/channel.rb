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
  LOGIN_OLD_CONTAINER_VERSION       = 6
  LOGIN_NO_GAME                     = 7
  LOGIN_OLD_GAME_VERSION            = 8
  LOGIN_WRONG_NICK_OR_PASSWORD      = 9
  LOGIN_NOT_FRIENDS                 = 10
  LOGIN_REDIRECT                    = 11

  NICK_MAX    = 32
  NICK_FORMAT = /^([a-zA-Z0-9_-])+$/

  @@channels = {}

  # ----------------------------------------------------------------------------

  def self.key(game_id, channel_name)
    "#{game_id}/#{channel_name}"
  end

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

    return unless validate(player, container_version, game_id, game_version,
      captcha_code, encrypted_code, nick)

    player.nick = nick
    key = self.key(game_id, channel_name)
    channel = @@channels[key]
    if channel.nil?
      if game_id < 0 and player.remote_ip == '127.0.0.1'
        Channel.new(key, [player], container_version, game_version, batch_game)
      else
        PendedChannel.login(key, player, container_version, game_version, batch_game)
      end
    else
      channel.login(player, container_version, game_version, batch_game)
    end
  end

  # Returns login code.
  def self.validate(player, container_version, game_id, game_version, captcha_code, encrypted_code, nick)
    # Security check
    # game_id can be negative when developing games on localhost
    if game_id < 0 and player.remote_ip != '127.0.0.1'
      player.close_connection
      return
    end

    code = LOGIN_OK
    if nick.empty? or nick.length > NICK_MAX or nick !~ NICK_FORMAT
      code = LOGIN_DUPLICATE_NICK
    elsif !Captcha.instance.correct?(captcha_code, encrypted_code)
      code = LOGIN_WRONG_CAPTCHA
    elsif game_id >= 0
      code = Proxy.instance.check_login(container_version, game_id, game_version)
    end
    return true if code == LOGIN_OK

    player.invoke(Player::CMD_LOGIN, [code, nil])
    player.close_connection_after_writing
    return false
  end

  # ----------------------------------------------------------------------------

  def self.keys
    @@channels.keys
  end

  # Returns {channel_key => remote_ips} of all channels.
  def self.remote_ips
    ret = {}
    @@channels.each do |k, c|
      ret[k] = c.remote_ips
    end
    ret
  end

  # ----------------------------------------------------------------------------

  def initialize(key, players, container_version, game_version, batch_game)
    @@channels[key] = self
    @key = key  # Used to delete this channel

    @lobby = Lobby.new(players)
    @rooms = []

    @container_version = container_version
    @game_version      = game_version
    @batch_game        = batch_game

    s = snapshot
    players.each do |p|
      p.channel = self
      p.room = @lobby
      p.invoke(Player::CMD_LOGIN, [LOGIN_OK, s])
    end
  end

  def login(player, container_version, game_version, batch_game)
    code = LOGIN_OK
    if @lobby.nicks.include?(player.nick)
      code = LOGIN_DUPLICATE_NICK
    else
      @rooms.each do |r|
        if r.nicks.include?(player.nick)
          code = LOGIN_DUPLICATE_NICK
          break
        end
      end
    end
    if code == LOGIN_OK
      if container_version != @container_version
        code = LOGIN_DIFFERENT_CONTAINER_VERSION
      elsif game_version != @game_version
        code = LOGIN_DIFFERENT_GAME_VERSION
      elsif batch_game != @batch_game
        code = LOGIN_DIFFERENT_GAME_VERSION
      end
    end

    if code != LOGIN_OK
      player.invoke(Player::CMD_LOGIN, [code, nil])
      player.close_connection_after_writing
      return
    end

    player.channel = self
    @lobby.process(player, Player::CMD_LOGIN, nil)
    player.room = @lobby
    player.invoke(Player::CMD_LOGIN, [LOGIN_OK, snapshot])
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

  def remote_ips
    ret = @lobby.remote_ips
    @rooms.each do |r|
      ret.concat(r.remote_ips)
    end
    ret
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
      Proxy.instance.fm_channel_delete(@key)
      @@channels.delete(@key)
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