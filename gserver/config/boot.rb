require 'rubygems'
require 'revent/as_r'
require 'revent/r_r'
require 'revent/captcha'
require 'singleton'
require 'logger'

require "#{FIFO_ROOT}/config/environment"
Dir.glob("#{FIFO_ROOT}/app/**/*.rb").each do |f|
  require f
end

#-------------------------------------------------------------------------------

if ARGV[0] == 'production'
  $logger = Logger.new("#{FIFO_ROOT}/log/production.log", 'daily')
  $logger.level = Logger::INFO
else
  $logger = Logger.new(STDOUT)
end

#-------------------------------------------------------------------------------

class Server
  include Revent::ASRServer

  def initialize(host, port)
    self.logger = $logger
    @gate = Gate.new
    start_server(host, port)
  end

  def on_connect(client)
    client.session = {:scope => @gate}
  end

  def on_close(client)
    client.session[:scope].on_close(client)
  rescue
    $logger.error($!)
    $logger.error($!.backtrace.join("\n"))
  end

  def on_invoke(client, cmd, value)
    $logger.debug("on_invoke, cmd: #{cmd}, value: #{value.inspect}")

    scope = client.session[:scope]
    if scope.invokables.include?(cmd)
      scope.send(cmd, *value)
    else
      raise('Invalid command')
    end
  rescue
    $logger.error($!)
    $logger.error($!.backtrace.join("\n"))
    client.close_connection
  end
end

#-------------------------------------------------------------------------------

EventMachine::run do
  Proxy.instance
  Server.new(CONF[:host], CONF[:port])
  $logger.info("gserver started on #{CONF[:host]}:#{CONF[:port]}")
end
