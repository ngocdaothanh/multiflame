# Container for game snapshot and actions.
class GameSnapshotActions
  def initialize
    @snapshot = nil
    @actions = []
  end

  def move(snapshot, index, data)
  end

  def resign(index)
  end

  def timeout(index)
  end
end
