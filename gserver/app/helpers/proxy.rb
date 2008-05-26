require 'zlib'

# Proxy that talks to the manager.
class Proxy
  include Singleton
  include Revent::RRClient

  # gserver -> manager
  CMD_GM_CHANNEL_KEYS_SET = 10
  CMD_GM_CHANNEL_CREATE   = 11
  CMD_GM_CHANNEL_DELETE   = 12

  # Manager -> gserver
  CMD_MG_READY_SET        = 0
  CMD_MG_CG_SET           = 1
  CMD_MG_REMOTE_IPS_GET   = 2

  def initialize
    @on_calls = {
      CMD_MG_READY_SET        => method(:on_call_mf_ready_set),
      CMD_MG_CG_SET           => method(:on_call_mf_cg_set),
      CMD_MG_REMOTE_IPS_GET   => method(:on_call_mf_remote_ips_get)
    }
    @on_results = {
      CMD_GM_CHANNEL_CREATE => method(:on_result_fm_channel_create)
    }

    begin
      connect(CONF[:manager_host], CONF[:manager_port])
    rescue
      reconnect
    end
  end

  def on_connect
    LOGGER.info("Connected to the manager at #{CONF[:manager_host]}:#{CONF[:manager_port]}")
    fm_channel_keys_set
  end

  def on_close
    @manager_ready = false
    reconnect
  end

  def on_call(cmd, value)
    m = @on_calls[cmd]
    m.call(value)
  end

  def on_result(cmd, value)
    m = @on_results[cmd]
    m.call(value) unless m.nil?
  end

  def on_error(cmd, value)
    backtrace = value.backtrace.join("\n")
    LOGGER.error("@proxy: cmd = #{cmd}, error = #{backtrace}")
  end

  # ----------------------------------------------------------------------------

  # Raise if not connected with the manager.
  def game_info(id, locale)
    raise unless @manager_ready
    g = @cg[:g][id]
    return nil if g.nil?
    {'name' => g[:names][locale]}
  end

  def check_login(container_version, game_id, game_version)
    return Channel::LOGIN_CONNECTION_ERROR unless @manager_ready
    return Channel::LOGIN_OLD_CONTAINER_VERSION if container_version != @cg[:c][:version]

    g = @cg[:g][game_id]
    return Channel::LOGIN_NO_GAME if g.nil?
    return Channel::LOGIN_OLD_GAME_VERSION if game_version != g[:version]

    Channel::LOGIN_OK
  end

  # gserver -> manager ------------------------------------------------------------

  def reconnect
    LOGGER.info("Reconnect to the manager at #{CONF[:manager_host]}:#{CONF[:manager_port]} in #{CONF[:manager_reconnect_interval]} seconds")
    EventMachine::add_timer(CONF[:manager_reconnect_interval]) do
      begin
        connect(CONF[:manager_host], CONF[:manager_port])
      rescue
        reconnect
      end
    end
  end

  def fm_channel_keys_set
    property = {:swf_host => CONF[:swf_host], :swf_port => CONF[:swf_port], :players_limit => Stats.instance.players_limit}
    call(CMD_GM_CHANNEL_KEYS_SET, {:property => property, :channel_keys => Channel.keys})
  end

  def fm_channel_create(key)
    call(CMD_GM_CHANNEL_CREATE, key)
  end

  def on_result_fm_channel_create(result)
    PendedChannel.login_pended(result[0], result[1], result[2])
  end

  def fm_channel_delete(key)
    call(CMD_GM_CHANNEL_DELETE, key) if self.connected?
  end

  # Manager -> gserver ------------------------------------------------------------

  def on_call_mf_ready_set(value)
    @manager_ready = true
    nil
  end

  def on_call_mf_cg_set(value)
    @cg = value
    nil
  end

  def on_call_mf_remote_ips_get(value)
    s = Marshal.dump(Channel.remote_ips)
    Zlib::Deflate.deflate(s)
  end
end