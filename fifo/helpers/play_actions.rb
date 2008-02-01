class PlayActions
  MOVE    = 0
  RESIGN  = 1
  TIMEOUT = 2  # This player asked the game to check that some player (maybe himself) has timed out

  attr_reader :play_created_at

  def initialize
    @actions = []
  end

  def clear
    @actions.clear
  end

  def new_game
    @play_created_at = Time.now
  end

  def move_immediate(index, data)
    a = [Time.now - @play_created_at, index, data]
    @actions << a
    a
  end

  def move_batch(batch_move)
    a = [Time.now - @play_created_at].concat(batch_move.to_a)
    @actions << a
    a
  end

  def resign(index)
    a = [Time.now - @play_created_at, index]
    @actions << a
    a
  end

  def timeout(index)
    timestamp = Time.now - @play_created_at
    @actions << [timestamp, -(index + 1)]
    [timestamp, index]
  end

  # TODO: zlib compress
  # @return [[timestamp, index, data, index, data...], [timestamp, index], [timestamp, -(index + 1)]...]
  #
  # [timestamp, index, data, index, data...]:  MOVE
  # [timestamp, index]:                        RESIGN
  # [timestamp, -(index + 1)]:                 TIMEOUT
  def to_a
    @actions
  end
end