# Interface for a scope.
class Scope
  def name
    self.to_s.downcase.gsub(/Scope$/, '')
  end

  # Returns an array of callable method names.
  def callables
    []
  end

  # ticket: a hash.
  # Returns true if the client is allowed to enter.
  def enter_from_outer_scope(client, ticket)
    true
  end

  def enter_from_inner_scope(client)
    true
  end

  def on_leave(client)
  end

  # Called when a player enters a room (from the lobby).
  # in: iroom
  # out: [iroom, nick]
  def room_enter(player, iroom)
    @players.delete(player)
    @players.each do |p|
      p.call(Server::CMD_ROOM_ENTER, [iroom, player.session[:nick]])
    end
  end

  # Called when a player leaves a room (to enter the lobby).
  def room_leave(player, iroom)
    @players.each do |p|
      p.call(Server::CMD_ROOM_LEAVE, [iroom, player.session[:nick]])
    end
    @players << player
  end

  # out: [[nicks in lobby], [nicks in room0], [nicks in room1]...]
  def snapshot
    a = [@lobby.nicks]
    a.concat(@rooms.map { |r| r.nicks })
    a
  end

  def chat(player, message)
    index = @players.index(player)
    @players.each do |p|
      p.call(Server::CMD_CHAT, [index, message])
    end
  end
end
