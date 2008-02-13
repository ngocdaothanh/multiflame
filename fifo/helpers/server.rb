class Server
  include Revent::ASRServer

  # Because this is a FIFO (queue), CMD_xxx has 2 meanings: command invocation
  # and notification.

  CMD_CAPTCHA      = 0
  CMD_GAME_INFO    = 1

  # There are 2 types of notification: result to the caller and broadcast to
  # the other players. The arguments are different for the 2 types.
  CMD_LOGIN        = 2

  # While out in the lobby
  #CMD_LOGIN
  CMD_WILL_CLOSE   = 3
  CMD_LOGOUT       = 4  # Fifo -> container
  CMD_ROOM_ENTER   = 5
  CMD_ROOM_LEAVE   = 6
  CMD_CHAT         = 7

  # While in a room
  #CMD_WILL_CLOSE
  #CMD_ROOM_ENTER
  #CMD_ROOM_LEAVE
  #CMD_CHAT
  CMD_NEW_INIT     = 8
  CMD_NEW_JOIN     = 9
  CMD_NEW_UNJOIN   = 10
  CMD_NEW_TIMEOUT  = 11
  CMD_PLAY_MOVE    = 12
  CMD_PLAY_RESIGN  = 13
  CMD_PLAY_TIMEOUT = 14
  CMD_GAME_OVER    = 15

  GAME_INFO_OK               = 0
  GAME_INFO_CONNECTION_ERROR = 1
  GAME_INFO_NO_GAME          = 2

  def initialize(host, port)
    self.logger = $LOGGER
    start_server(host, port)
  end

  def on_connect(client)
    client.property = {}
  end

  def on_close(client)
    return if client.property[:channel].nil?
    client.property[:channel].process(client, CMD_LOGOUT, nil)
  rescue
    $LOGGER.error($!)
  end

  def on_call(client, cmd, value)
    $LOGGER.debug('on_call----')
    $LOGGER.debug('cmd: ' + cmd.inspect)
    $LOGGER.debug('value: ' + value.inspect)

    if cmd == CMD_CAPTCHA
      if client.property[:channel].nil?
        captcha(client)
      else
        $LOGGER.debug('@player: CMD_CAPTCHA but already logged in')
        client.close_connection
      end
      return
    end

    if cmd == CMD_GAME_INFO
      if client.property[:channel].nil?
        game_info(client, value)
      else
        $LOGGER.debug('@player: CMD_GAME_INFO but already logged in')
        client.close_connection
      end
      return
    end

    if cmd == CMD_LOGIN
      if client.property[:channel].nil?
        Channel.login(client, value)
      else
        $LOGGER.debug('@player: CMD_LOGIN but already logged in')
        client.close_connection
      end
      return
    end

    if cmd > CMD_LOGOUT
      unless client.property[:channel].nil?
        client.property[:channel].process(client, cmd, value)
      else
        $LOGGER.debug("@player: command = #{cmd} but not logged in")
        client.close_connection
      end
    else
      $LOGGER.debug("@player: Invalid command #{cmd}")
      client.close_connection
    end
  end

  # ----------------------------------------------------------------------------

  # in: none
  # out: encrypted code length, image size, encrypted code, image
  def captcha(client)
    encrypted_code, img = Captcha.instance.new
    bytes = img.unpack('c*')
    client.result(CMD_CAPTCHA, [encrypted_code, bytes], true)
  end

  # in: [id, locale]
  # out: [code, info]
  def game_info(client, value)
    id, locale = value

    if id < 0 and client.remote_ip == '127.0.0.1'
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
end
