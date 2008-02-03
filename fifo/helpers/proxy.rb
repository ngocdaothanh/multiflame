# Proxy that talks to the manager.
class Proxy
  include Singleton
  include ReventClient

  # Fifo -> manager
  CMD_FM_CHANNEL_KEYS_SET = 0

  # Manager -> fifo
  CMD_MF_READY            = 0
  CMD_MF_CG_SET           = 1
  CMD_MF_CAPTCHA_SALT_SET = 2

  def initialize
    @on_calls = {
      CMD_MF_READY  => method(:on_mf_ready),
      CMD_MF_CG_SET => method(:on_mf_cg_set),
      CMD_MF_CAPTCHA_SALT_SET => method(:on_mf_captcha_salt_set)
    }

    on_close
  end

  def on_connect
  end

  def on_close
    @manager_ready = false
    reconnect
  end

  def on_call(cmd, arg)
    m = @on_calls[cmd]
    m.call(arg)
  end

  def on_result(cmd, result)
  end

  def on_error(cmd, error)
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

private

  # Fifo -> manager ------------------------------------------------------------

  def reconnect
    $LOGGER.info("Reconnect to the manager at #{CONFIG[:manager_host]}:#{CONFIG[:manager_port]} in #{CONFIG[:manager_reconnect_interval]} seconds")
    EventMachine::add_timer(CONFIG[:manager_reconnect_interval]) do
      begin
        connect(CONFIG[:manager_host], CONFIG[:manager_port])
      rescue
        reconnect
      end
    end
  end

  def fm_channel_keys_set
    property = {:port => CONFIG[:port], :limit => Stats.instance.players_limit}
    call(CMD_FM_CHANNEL_KEYS_SET, {:property => property, :channel_keys => Stats.instance.channel_keys})
  end

  def fm_channel_create(channel_key)
    
  end

  def on_fm_channel_create_result(result)
    
  end

  # Manager -> fifo ------------------------------------------------------------

  def on_mf_ready(arg)
    @manager_ready = true
  end

  def on_mf_cg_set(arg)
    @cg = arg
  end

  def on_mf_captcha_salt_set(arg)
  end
end