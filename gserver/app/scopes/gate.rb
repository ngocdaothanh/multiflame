class Gate < Scope
  def initialize(parent = nil)
    super(parent)
    @captcha_factory = Revent::CaptchaFactory.new(CONF[:captcha_key], CONF[:captcha_length], CONF[:captcha_valid_period])
    @zones = {}
  end

  def invokables
    ['captcha', 'game_info', 'login']
  end

  # out: [encrypted code with timestamp, image]
  def captcha(client)
    c = @captcha_factory.new
    ba1 = RubyAMF::IO::ByteArray.new(c.encrypted_code_with_timestamp)
    ba2 = RubyAMF::IO::ByteArray.new(c.img)
    client.result('captcha', [ba1, ba2], true)
  end

  #-----------------------------------------------------------------------------

  GAME_INFO_OK               = 0
  GAME_INFO_CONNECTION_ERROR = 1
  GAME_INFO_NO_GAME          = 2

  # out: [code, info]
  def game_info(client, id, locale)
    if development_client?(client, id)
      info = {'name' => 'My killer game'}
      code = GAME_INFO_OK
    else
      begin
        info = Proxy.instance.game_info(id, locale)
        code = info.nil? ? GAME_INFO_NO_GAME : GAME_INFO_OK
      rescue
        info = nil
        code = GAME_INFO_CONNECTION_ERROR
      end
    end

    client.result(CMD_GAME_INFO, [code, info], true)
  end

  def development_client?(client, game_id)
    client.remote_ip == '127.0.0.1' && game_id < 0
  end

  #-----------------------------------------------------------------------------

  TYPE_IGOUGO   = 0
  TYPE_WEGO     = 1
  TYPE_REALTIME = 2

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
  def login(client, container_version, game_id, game_version, game_type, zone, nick, captcha_code, encrypted_code)
    nick = nick.strip
    return unless validate(client, container_version, game_id, game_version, nick, captcha_code, encrypted_code)
    client.session[:nick] = nick

    @zones.synchronize do
      zone = @zones[zone]
      batch_game = (game_type == TYPE_WEGO)? true : false
      if zone.nil?
        if development_client?(client, game_id)
          @zones[zone] = Zone.new(zone, [client], container_version, game_version, batch_game)
        else
          PendedZone.login(zone, client, container_version, game_version, batch_game)
        end
      else
        zone.login(client, container_version, game_version, batch_game)
      end
    end
  end

  # Returns true if the validation was passed.
  def validate(client, container_version, game_id, game_version, nick, captcha_code, encrypted_code)
    code = LOGIN_OK
    if nick.empty? || nick.length > NICK_MAX || nick !~ NICK_FORMAT
      code = LOGIN_DUPLICATE_NICK
    elsif !@captcha_factory.correct?(captcha_code, encrypted_code)
      code = LOGIN_WRONG_CAPTCHA
    elsif game_id >= 0
      code = Proxy.instance.check_login(container_version, game_id, game_version)
    end

    if code == LOGIN_OK
      return true
    else
      client.result('login', [code, nil], true)
      false
    end
  end
end
