class Channel
  @@channels = {}

  # ----------------------------------------------------------------------------


  # ----------------------------------------------------------------------------

  def initialize(key, players, container_version, game_version, batch_game)
    @@channels.synchronize do
      @@channels[key] = self

      @key = key  # Used to delete this channel

      @lobby = Lobby.new(players)
      @rooms = []

      @container_version = container_version
      @game_version      = game_version
      @batch_game        = batch_game

      s = snapshot
      players.each do |p|
        p.session[:channel] = self
        p.session[:room] = @lobby
        p.result(Server::CMD_LOGIN, [LOGIN_OK, s])
      end
    end
  end

  def login(player, container_version, game_version, batch_game)
    self.synchronize do
      code = LOGIN_OK
      if @lobby.nicks.include?(player.session[:nick])
        code = LOGIN_DUPLICATE_NICK
      else
        @rooms.each do |r|
          if r.nicks.include?(player.session[:nick])
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
        player.result(Server::CMD_LOGIN, [code, nil], true)
        return
      end

      player.session[:channel] = self
      @lobby.process(player, Server::CMD_LOGIN, nil)
      player.session[:room] = @lobby
      player.result(Server::CMD_LOGIN, [LOGIN_OK, snapshot])
    end
  end

  # ----------------------------------------------------------------------------

  def process(player, cmd, value)
    # Cross-room related commands are processed here, the others are processed
    # in the room the player is currently in (lobby or game room)
    if cmd == Server::CMD_LOGOUT
      logout(player)
    elsif cmd == Server::CMD_ROOM_ENTER
      room_enter(player, value)
    elsif cmd == Server::CMD_ROOM_LEAVE
      room_leave(player, value)
    else
      player.session[:room].process(player, cmd, value)
    end
  end

  def remote_ips
    self.synchronize do
      ret = @lobby.remote_ips
      @rooms.each do |r|
        ret.concat(r.remote_ips)
      end
      return ret
    end
  end

private

  def logout(player)
    self.synchronize do
      if player.session[:room] != @lobby
        iroom = @rooms.index(player.session[:room])

        player.session[:room].process(player, Server::CMD_ROOM_LEAVE, nil)
        if player.session[:room].nicks.empty?
          @rooms.delete_at(iroom)
        end

        @lobby.process(player, Server::CMD_LOGOUT, iroom)
      else
        @lobby.process(player, Server::CMD_LOGOUT, nil)
      end

      @@channels.synchronize do
        if @rooms.empty? and @lobby.nicks.empty?
          @@channels.delete(@key)
          Proxy.instance.fm_channel_delete(@key)
        end
      end
    end
  end

  # in: iroom, negative to create a new room
  # out:
  # * For the players who are in the room:  nick
  # * For the player who entered the room:  room snapshot
  # * For the players who are in the lobby: [iroom, nick]
  def room_enter(player, value)
    self.synchronize do
      iroom = value
      if iroom < 0
        room = Room.new(player, @batch_game)
        @rooms << room
        iroom = @rooms.size - 1
      else
        room = @rooms[iroom]
        room.process(player, Server::CMD_ROOM_ENTER, nil)
      end
      @lobby.process(player, Server::CMD_ROOM_ENTER, iroom)
    end
  end

  # out:
  # * For the players who are in the room:  nick
  # * For the players who are in the lobby: [iroom, nick]
  # * For the player who left:              room snapshot
  def room_leave(player, value)
    self.synchronize do
      if player.session[:room] == @lobby
        $logger.debug('@channel: room_leave: No room to leave')
        player.close_connection
        return
      end

      iroom = @rooms.index(player.session[:room])

      player.session[:room].process(player, Server::CMD_ROOM_LEAVE, nil)
      if player.session[:room].nicks.empty?
        @rooms.delete_at(iroom)
      end

      @lobby.process(player, Server::CMD_ROOM_LEAVE, iroom)

      player.session[:room] = @lobby
      player.call(Server::CMD_ROOM_LEAVE, snapshot)
    end
  end


end
