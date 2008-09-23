class GateController < Scope
  GAME_INFO_OK               = 0
  GAME_INFO_CONNECTION_ERROR = 1
  GAME_INFO_NO_GAME          = 2

  def callables
    ['captcha', 'game_info']
  end

  # Returns: [encrypted code with timestamp, image]
  def captcha(client)
    encrypted_code_with_timestamp, img = CAPTCHA.new
    ba1 = RubyAMF::IO::ByteArray.new(encrypted_code_with_timestamp)
    ba2 = RubyAMF::IO::ByteArray.new(img)

    client.result(CMD_CAPTCHA, [ba1, ba2], true)
  end

  # Returns: [code, info]
  def game_info(client, id, locale)
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
