# Lobby is only a kind of room (waiting room). A player can only be inside a
# room at a time.
#
# Although there's illusion that the lobby contains many rooms, it doesn't.
# A channel has one lobby and many rooms. Cross-room management is done by the
# channel.
class Lobby
  def initialize(players)
    @mutex_self = Mutex.new
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
    @mutex_self.synchronize do
      return @players.map { |p| p.property[:nick] }
    end
  end

  def remote_ips
    @mutex_self.synchronize do
      return @players.map { |p| p.remote_ip }
    end
  end

private

  def login(player, value)
    @players.each do |p|
      p.call(Server::CMD_LOGIN, player.property[:nick])
    end
    @players << player
  end

  def will_close(player, value)
  end

  def logout(player, value)
    iroom = value
    @players.delete(player)
    @players.each do |p|
      p.call(Server::CMD_LOGOUT, [iroom, player.property[:nick]])
    end
  end

  # in: iroom
  # out: [iroom, nick]
  def room_enter(player, value)
    iroom = value
    @players.delete(player)
    @players.each do |p|
      p.call(Server::CMD_ROOM_ENTER, [iroom, player.property[:nick]])
    end
  end

  def room_leave(player, value)
    iroom = value
    @players.each do |p|
      p.call(Server::CMD_ROOM_LEAVE, [iroom, player.property[:nick]])
    end

    @players << player
  end

  def chat(player, value)
    msg = value
    index = @players.index(player)
    @players.each do |p|
      p.call(Server::CMD_CHAT, [index, msg])
    end
  end
end
