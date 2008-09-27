# Interface for a scope.
class Scope
  def initialize(parent = nil)
    @parent = parent
    @clients = []
  end

  def on_close(client)
    @client.synchronize do
      @clients.delete(client)
    end
    parent.on_close(client) unless parent.nil?
  end

  # Returns an array of invokable method names.
  def invokables
    []
  end
end
