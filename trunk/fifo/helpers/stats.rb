class Stats
  include Singleton

  def initialize
    @players = 0
    @players_by_game = {}
    @players_by_channel = {}
  end
  
  def players
    @players
  end

  def players_by_game(id)
    @players_by_game[id] || 0
  end

  def players_by_channel(game_id, owner_nick)
    @players_by_channel["#{game_id}/#{owner_nick}"] || 0
  end

  # ----------------------------------------------------------------------------

  def channels
    @players_by_channel.size
  end

  # ----------------------------------------------------------------------------

  def inc_player(game_id, owner_nick)
    @players += 1

    @players_by_game[game_id] = 0 if @players_by_game[game_id].nil?
    @players_by_game[game_id] += 1

    name = "#{game_id}/#{owner_nick}"
    @players_by_channel[name] = 0 if @players_by_channel[name].nil?
    @players_by_channel[name] += 1
  end

  def dec_player(game_id, owner_nick)
    @players -= 1
    @players_by_game[game_id] -= 1
    @players_by_channel["#{game_id}/#{owner_nick}"] -= 1
  end
end