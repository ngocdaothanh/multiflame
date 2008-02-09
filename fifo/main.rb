require 'rubygems'
require 'revent/as_r'
require 'revent/r_r'
require 'singleton'
require 'logger'

require 'config'
require 'helpers/server'
require 'helpers/captcha'
require 'helpers/channel'
require 'helpers/pended_channel'
require 'helpers/lobby'
require 'helpers/room'
require 'helpers/utils'
require 'helpers/play_actions'
require 'helpers/batch_move'
require 'helpers/proxy'
require 'helpers/stats'

$LOGGER = Logger.new(STDOUT, 'daily')

EventMachine::run do
  Server.new(CONFIG[:host], CONFIG[:port])
  $LOGGER.info("Fifo started on #{CONFIG[:host]}:#{CONFIG[:port]}")

  Proxy.instance
end
