require 'revent'

class FifoManager
  include ReventClient

  CMD_WM_WHICH_HOST       = 0
  CMD_WM_CG_RELOAD        = 1

  attr_reader :result_or_error, :result, :error

  def initialize(cmd, arg)
    @cmd = cmd
    @arg = arg
    t = Thread.new do
      EventMachine::run do
        connect(CONFIG[:fifo_manager_host], CONFIG[:fifo_manager_port])
      end
    end
    t.join
  end

  def on_connect
    call(@cmd, @arg)
  end

  def on_result(cmd, result)
p result
    @result_or_error = :result
    @result = result
    close
  end

  def on_error(cmd, error)
puts error.backtrace.join("\n")
    @result_or_error = :error
    @error = error
    close
  end

  def on_close
    EventMachine::stop_event_loop
    Thread.pass
  end

  # ----------------------------------------------------------------------------

  def destination(controller, condition_variable)
    return nil unless connected?
  end
end