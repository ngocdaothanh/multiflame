class LobbyEntity < Entity
  def callables
    ['captcha', 'game_info']
  end

  def enter

  end

  def initialize(players)
    @players = players
    @methods = {
      Server::CMD_LOGIN      => method('login'),
      Server::CMD_WILL_CLOSE => method('will_close'),
      Server::CMD_LOGOUT     => method('logout'),
      Server::CMD_ROOM_ENTER => method('room_enter'),
      Server::CMD_ROOM_LEAVE => method('room_leave'),
      Server::CMD_CHAT       => method('chat')
    }
  end

  def process(player, cmd, value)
    self.synchronize do
      m = @methods[cmd]
      if m.nil?
        $logger.debug("@lobby: Invalid command: #{cmd}")
        player.close_connection
      else
        m.call(player, value)
      end
    end
  end

  def nicks
    self.synchronize do
      return @players.map { |p| p.session[:nick] }
    end
  end

  def remote_ips
    self.synchronize do
      return @players.map { |p| p.remote_ip }
    end
  end

  def login(player)
    @players.each do |p|
      p.call(Server::CMD_LOGIN, player.session[:nick])
    end
    @players << player
  end

  def will_close(player, value)
  end

  # The lobby needs to know the player who has just logged out to update player
  # list of each room.
  #
  # value: room index, nil if this room is the lobby.
  def logout(player, iroom)
    @players.delete(player)
    @players.each do |p|
      p.call(Server::CMD_LOGOUT, [iroom, player.session[:nick]])
    end
  end




end
