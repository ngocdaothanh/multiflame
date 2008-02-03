# Commands are separated by \0. Except for special case, commands and results
# are in JSON format:
# [<command number>, <argument object>]
# Array is used extensively instead of hash to shorten the JSON text.
module Player
  # Because this is a FIFO (queue), CMD_xxx has 2 meanings: command invocation
  # and notification.

  CMD_POLICY          = '<policy-file-request/>'  # Called by Flash player
  CMD_CAPTCHA         = '0'  # Command is not in JSON, result is JPEG data
  CMD_GAME_INFO       = 1

  # There are 2 types of notification: result to the caller and broadcast to
  # the other players. The arguments are different for the 2 types.
  CMD_LOGIN           = 2

  # While out in the lobby
  #CMD_LOGIN
  CMD_WILL_CLOSE      = 3
  CMD_LOGOUT          = 4  # Fifo -> container
  CMD_ROOM_ENTER      = 5
  CMD_ROOM_LEAVE      = 6
  CMD_CHAT            = 7

  # While in a room
  #CMD_WILL_CLOSE
  #CMD_ROOM_ENTER
  #CMD_ROOM_LEAVE
  #CMD_CHAT
  CMD_NEW_INIT        = 8
  CMD_NEW_JOIN        = 9
  CMD_NEW_UNJOIN      = 10
  CMD_NEW_TIMEOUT     = 11
  CMD_PLAY_MOVE       = 12
  CMD_PLAY_RESIGN     = 13
  CMD_PLAY_TIMEOUT    = 14
  CMD_GAME_OVER       = 15
  CMD_JUDGE           = 16  # Command: Fifo -> container, result: container -> Fifo
  CMD_RESULT          = 17  # Fifo -> container

  POLICY = <<EOF
<cross-domain-policy>
<allow-access-from domain="*" to-ports="#{CONFIG[:port]}" />
</cross-domain-policy>
EOF

  CAPTCHA_ENCRYPTED_CODE_LENGTH_DIGITS = 2
  CAPTCHA_IMG_SIZE_DIGITS              = 5

  GAME_INFO_OK               = 0
  GAME_INFO_CONNECTION_ERROR = 1
  GAME_INFO_NO_GAME          = 2

  # Read and write by the channel
  attr_accessor :nick, :channel, :room

  # Called by the network connection -------------------------------------------

  def post_init
    @sz = ''  # Zero-terinated string
  end

  def receive_data(data)
    @sz << data
    if @sz.length > CONFIG[:max_cmd_length]
      close_connection
      return
    end

    while true
      len = @sz.index(0)
      return if len.nil?

      cmd_sz = @sz.slice!(0, len)
      @sz.slice!(0, 1)  # Remove the leading \0

      $LOGGER.debug("in: #{cmd_sz}")
      process_sz(cmd_sz)
    end
  rescue
    $LOGGER.error($!)
    close_connection
  end

  def unbind
    return if @channel.nil?
    @channel.process(self, CMD_LOGOUT, nil)
  rescue
    $LOGGER.error($!)
  end

  # Called by the channel/room -------------------------------------------------

  # Invoke a command on the container
  def invoke(cmd, arg)
    sz = [cmd, arg].to_json  + "\0"
    $LOGGER.debug("out: #{sz}")
    send_data(sz)
  end

  # IP of the container
  def remote_ip
    a = get_peername[2,6].unpack("nC4")
    a.delete_at(0)
    a.join('.')
  end

private

  def process_sz(cmd_sz)
    if cmd_sz == CMD_POLICY
      send_data(POLICY + "\0")
      close_connection_after_writing
      return
    end

    if cmd_sz == CMD_CAPTCHA
      if @channel.nil?
        captcha
      else
        $LOGGER.debug('@player: CMD_CAPTCHA but already logged in')
        close_connection
      end
      return
    end

    cmd, arg = JSON.parse(cmd_sz)

    if cmd == CMD_GAME_INFO
      if @channel.nil?
        game_info(arg)
      else
        $LOGGER.debug('@player: CMD_GAME_INFO but already logged in')
        close_connection
      end
      return
    end

    if cmd == CMD_LOGIN
      if @channel.nil?
        Channel.login(self, arg)
      else
        $LOGGER.debug('@player: CMD_LOGIN but already logged in')
        close_connection
      end
      return
    end

    if cmd > CMD_LOGOUT
      unless @channel.nil?
        @channel.process(self, cmd, arg)
      else
        $LOGGER.debug("@player: command = #{cmd} but not logged in")
        close_connection
      end
    else
      $LOGGER.debug("@player: Invalid command #{cmd}")
      close_connection
    end
  end

  # ----------------------------------------------------------------------------

  # in: none
  # out: encrypted code length, image size, encrypted code, image
  def captcha
    encrypted_code, img = Captcha.instance.new

    # Header
    send_data(sprintf("%0#{CAPTCHA_ENCRYPTED_CODE_LENGTH_DIGITS}d", encrypted_code.size))
    send_data(sprintf("%0#{CAPTCHA_IMG_SIZE_DIGITS}d", img.size))

    # Data
    send_data(encrypted_code)
    send_data(img)

    close_connection_after_writing
  end

  # in: [id, locale]
  # out: [code, info]
  def game_info(arg)
    id, locale = arg

    if id < 0 and remote_ip == '127.0.0.1'
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

    invoke(CMD_GAME_INFO, [code, info])
    close_connection_after_writing
  end
end