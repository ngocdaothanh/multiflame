class Server
  include Revent::ASRServer

  # Because this is a FIFO (queue), CMD_xxx has 2 meanings: command invocation
  # and notification.

  CMD_CAPTCHA      = 0
  CMD_GAME_INFO    = 1

  # There are 2 types of notification: result to the caller and broadcast to
  # the other players. The arguments are different for the 2 types.
  CMD_LOGIN        = 2

  # While being out in the lobby
  #CMD_LOGIN
  CMD_WILL_CLOSE   = 3
  CMD_LOGOUT       = 4  # gserver -> container
  CMD_ROOM_ENTER   = 5
  CMD_ROOM_LEAVE   = 6
  CMD_CHAT         = 7

  # While being in a room
  #CMD_WILL_CLOSE
  #CMD_ROOM_ENTER
  #CMD_ROOM_LEAVE
  #CMD_CHAT
  CMD_NEW_CONF     = 8
  CMD_NEW_JOIN     = 9
  CMD_NEW_UNJOIN   = 10
  CMD_NEW_TIMEOUT  = 11
  CMD_PLAY_MOVE    = 12
  CMD_PLAY_RESIGN  = 13
  CMD_PLAY_TIMEOUT = 14
  CMD_GAME_OVER    = 15

  def initialize(host, port)
    @scope_names = SCOPE_ORDER.split('/')

    @scopes = {}
    @scope_names.each do |scope_name|
      class_name = "#{scope_name.capitalize}Scope"
      klass = eval(class_name)
      @scopes[scope_name] = klass.new
    end

    self.logger = $logger
    start_server(host, port)
  end

  def on_connect(client)
    scope = @scopes.first
    client.session = {:scopes => [scope]}
    scope.enter_from_outer_scope(client)
  end

  def on_close(client)
    client.session[:scopes].reverse_each do |scope|
      scope.on_leave(client)
    end
  rescue
    $logger.error($!)
    $logger.error($!.backtrace.join("\n"))
  end

  def on_call(client, cmd, value)
    $logger.debug("on_call, cmd: #{cmd}, value: #{value.inspect}")

    if cmd == 'enter'
      scope = client.session[:scopes].last
      inner_scope_name, ticket = value

      # Validate: inner_scope_name must be behind scope_name
      if @scope_names.index(scope.name) != @scope_names.index(inner_scope_name) - 1
        client.close_connection
        return
      end

      inner_scope = @scopes[inner_scope_name]
      if inner_scope.enter_from_outer_scope(client, ticket)
        scope.on_leave(client)
        client.session[:scopes] << inner_scope
      end
    elsif cmd == 'leave'
      scope = client.session[:scopes].pop
      scope.on_leave(client)

      scope = client.session[:scopes].last
      if scope.nil?
        client.close_connection
      else
        scope.enter_from_inner_scope(client)
      end
    else
      scope = client.session[:scopes].last
      if scope.callables.include?(cmd)
        scope.send(cmd, *value)
      else
        client.close_connection
      end
    end
  rescue
    $logger.error($!)
    $logger.error($!.backtrace.join("\n"))
    client.close_connection
  end
end
