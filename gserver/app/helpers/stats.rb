class Stats
  include Singleton

  attr_reader :players_limit, :channel_keys

  def initialize
    @players_limit = CONF[:players_limit]
    @channel_keys = []
  end
end