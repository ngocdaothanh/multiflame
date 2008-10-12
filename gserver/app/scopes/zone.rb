# A zone acts as a lobby.
class Zone < Scope
  def initialize(clients, container_version, game_version, batch_game)
    super

    self.synchronize do
      @clients = clients
      @rooms = []

      @container_version = container_version
      @game_version      = game_version
      @batch_game        = batch_game

      s = snapshot
      clients.each do |c|
        c.session[:scope] = self
        c.result('login', [Gate::LOGIN_OK, s])
      end
    end
  end

  def login(client, container_version, game_version, batch_game)
    self.synchronize do
      code = Gate::LOGIN_OK
      if nicks.include?(client.session[:nick])
        code = Gate::LOGIN_DUPLICATE_NICK
      else
        @rooms.each do |r|
          if r.nicks.include?(player.session[:nick])
            code = Gate::LOGIN_DUPLICATE_NICK
            break
          end
        end
      end

      if code == Gate::LOGIN_OK
        if container_version != @container_version
          code = Gate::LOGIN_DIFFERENT_CONTAINER_VERSION
        elsif game_version != @game_version
          code = Gate::LOGIN_DIFFERENT_GAME_VERSION
        elsif batch_game != @batch_game
          code = Gate::LOGIN_DIFFERENT_GAME_VERSION
        end
      end
      if code != Gate::LOGIN_OK
        client.result('login', [code, nil], true)
        return
      end

      @clients.each do |c|
        c.invoke('join', c.session[:nick])
      end

      @clients << client
      client.session[:scope] = self
      client.result('login', [Gate::LOGIN_OK, snapshot])
    end
  end

  def logout(client)
  end

  def room_create(client)

  end

  def room_join(client, iroom)
  end

  # ----------------------------------------------------------------------------

  # Called by a room after a client has left that room.
  #
  # out:
  # * For the players who are in the lobby: [iroom, nick]
  # * For the player who left:              room snapshot
  def room_on_leave(client)
    self.synchronize do
      iroom = @rooms.index(client.session[:scope])

      player.session[:room].process(player, Server::CMD_ROOM_LEAVE, nil)
      if player.session[:room].nicks.empty?
        @rooms.delete_at(iroom)
      end

      @lobby.process(player, Server::CMD_ROOM_LEAVE, iroom)

      client.session[:scope] = self
      client.call('room_leave', snapshot)
    end
  end


  def nicks
    @clients.map { |c| c.session[:nick] }
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

  # Returns {channel_key => remote_ips} of all channels.
  def remote_ips
    @@channels.synchronize do
      ret = {}
      @@channels.each do |k, c|
        ret[k] = c.remote_ips
      end
      return ret
    end
  end

end
