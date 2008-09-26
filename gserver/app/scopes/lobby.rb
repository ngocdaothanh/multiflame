class Lobby < Scope
  TYPE_IGOUGO  = 0
  TYPE_WEGO    = 1
  TYP_REALTIME = 2

  # Login result codes
  LOGIN_OK                          = 0
  LOGIN_CONNECTION_ERROR            = 1
  LOGIN_WRONG_CAPTCHA               = 2
  LOGIN_DIFFERENT_CONTAINER_VERSION = 3
  LOGIN_DIFFERENT_GAME_VERSION      = 4
  LOGIN_DUPLICATE_NICK              = 5
  LOGIN_OLD_CONTAINER_VERSION       = 6
  LOGIN_NO_GAME                     = 7
  LOGIN_OLD_GAME_VERSION            = 8
  LOGIN_WRONG_NICK_OR_PASSWORD      = 9
  LOGIN_NOT_FRIENDS                 = 10
  LOGIN_REDIRECT                    = 11

  NICK_MAX    = 32
  NICK_FORMAT = /^([a-zA-Z0-9_-])+$/

  # out: [code, snapshot]
  # Returns the channel this player has logged in
  def enter_from_outer_scope(player, ticket)
    container_version = value[0].to_i
    game_id           = value[1].to_i
    game_version      = value[2].to_i
    channel_name      = value[3].to_s
    captcha_code      = value[4].to_s
    encrypted_code    = value[5].to_s
    nick              = value[6].to_s.strip
    batch_game        = (value[7] == TYPE_WEGO)? true : false

    return unless validate(player, container_version, game_id, game_version,
      captcha_code, encrypted_code, nick)

    player.session[:nick] = nick
    k = key(game_id, channel_name)
    channel = nil
    @@channels.synchronize do
      channel = @@channels[k]
    end
    if channel.nil?
      if game_id < 0 && player.remote_ip == '127.0.0.1'
        Channel.new(key, [player], container_version, game_version, batch_game)
      else
        PendedChannel.login(key, player, container_version, game_version, batch_game)
      end
    else
      channel.login(player, container_version, game_version, batch_game)
    end
  end

  def key(game_id, channel_name)
    "#{game_id}/#{channel_name}"
  end

  # Returns true if the validation was passed.
  def validate(player, container_version, game_id, game_version, captcha_code, encrypted_code, nick)
    # Security check
    # game_id can be negative when developing games on localhost
    if game_id < 0 and player.remote_ip != '127.0.0.1'
      player.close_connection
      return false
    end

    code = LOGIN_OK
    if nick.empty? or nick.length > NICK_MAX or nick !~ NICK_FORMAT
      code = LOGIN_DUPLICATE_NICK
    elsif !CAPTCHA.correct?(captcha_code, encrypted_code)
      code = LOGIN_WRONG_CAPTCHA
    elsif game_id >= 0
      code = Proxy.instance.check_login(container_version, game_id, game_version)
    end
    return true if code == LOGIN_OK

    player.result(Server::CMD_LOGIN, [code, nil], true)
    false
  end

  # ----------------------------------------------------------------------------

  def keys
    @@channels.synchronize do
      return @@channels.keys
    end
  end

  # Returns {channel_key => remote_ips} of all channels.
  def remote_ips
    @@channels.synchronize do
      ret = {}
      @@channels.each do |k, c|
        ret[k] = c.remote_ips
      end
      return ret
    end
  end
end
