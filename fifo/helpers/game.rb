class Game
  def self.info(id, locale)
    if id < 0
      {'name' => 'My killer game'}
    else
      Proxy.instance.game_info(id, locale)
    end
  end
end