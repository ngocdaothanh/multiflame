# Lobby is only a kind of room (waiting room). A player can only be inside a
# room at a time.
#
# Although there's illusion that the lobby contains many rooms, it doesn't.
# A channel has one lobby and many rooms. Cross-room management is done by the
# channel.
class Lobby
  def initialize(players)
    @players = players
    @methods = {
      Player::CMD_LOGIN      => method('login'),
      Player::CMD_WILL_CLOSE => method('will_close'),
      Player::CMD_LOGOUT     => method('logout'),
      Player::CMD_ROOM_ENTER => method('room_enter'),
      Player::CMD_ROOM_LEAVE => method('room_leave'),
      Player::CMD_CHAT       => method('chat')
    }
  end

  def process(player, cmd, arg)
    m = @methods[cmd]
    if m.nil?
      $LOGGER.debug("@lobby: Invalid command: #{cmd}")
      player.close_connection
    else
      m.call(player, arg)
    end
  end

  def nicks
    @players.map { |p| p.nick }
  end

private

  def login(player, arg)
    @players.each do |p|
      p.invoke(Player::CMD_LOGIN, player.nick)
    end
    @players << player
  end

  def will_close(player, arg)
  end

  def logout(player, arg)
    @players.delete(player)
    @players.each do |p|
      p.invoke(Player::CMD_LOGOUT, player.nick)
    end
  end

  # in: iroom
  # out: [iroom, nick]
  def room_enter(player, arg)
    iroom = arg
    @players.delete(player)
    @players.each do |p|
      p.invoke(Player::CMD_ROOM_ENTER, [iroom, player.nick])
    end
  end

  def room_leave(player, arg)
    iroom = arg
    @players.each do |p|
      p.invoke(Player::CMD_ROOM_LEAVE, [iroom, player.nick])
    end

    @players << player
  end

  def chat(player, arg)
    msg = arg
    index = @players.index(player)
    @players.each do |p|
      p.invoke(Player::CMD_CHAT, [index, msg])
    end
  end
end