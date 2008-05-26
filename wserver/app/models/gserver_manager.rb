require 'revent/r_r'

class GserverManager
  include Revent::RRClient

  CMD_WM_WHICH_FIFO       = 0
  CMD_WM_CG_RELOAD        = 1
  CMD_WM_SNAPSHOT_CREATE  = 2

  def self.channel_key(game_id, channel_name)
    "#{game_id}/#{channel_name}"
  end

  # ----------------------------------------------------------------------------

  attr_reader :result_or_error, :result, :error

  def initialize(cmd, arg)
    @cmd = cmd
    @arg = arg
    t = Thread.new do
      EventMachine::run do
        connect(CONF[:fifo_manager_host], CONF[:fifo_manager_port])
      end
    end
    t.join
  end

  def on_connect
    call(@cmd, @arg)
  end

  def on_result(cmd, result)
    @result_or_error = :result
    @result = result
    close_connection
  end

  def on_error(cmd, error)
    logger.error.backtrace.join("\n")
    @result_or_error = :error
    @error = error
    close_connection
  end

  def on_close
    EventMachine::stop_event_loop
    Thread.pass
  end
end