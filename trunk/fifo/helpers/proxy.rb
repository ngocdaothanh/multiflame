require 'drb'

class Proxy
  include Singleton

  PORT = 1935

  def initialize
    DRb.start_service
    @fifo_manager = DRbObject.new(nil, "druby://localhost:#{PORT}")
  end

  def game_info(id, locale)
    @fifo_manager.game_info(id, locale)
  end

  def check_login(container_version, game_id, game_version)
    @fifo_manager.check_login(container_version, game_id, game_version)
  end

  def save_result(game_id, nicks, points)
    @fifo_manager.save_result(game_id, nicks, points)
  end
end