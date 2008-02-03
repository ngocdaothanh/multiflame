require 'rubygems'
require 'eventmachine'
require 'revent'
require 'json'
require 'singleton'
require 'logger'

require 'config'
require 'helpers/player'
require 'helpers/captcha'
require 'helpers/channel'
require 'helpers/lobby'
require 'helpers/room'
require 'helpers/utils'
require 'helpers/play_actions'
require 'helpers/batch_move'
require 'helpers/proxy'
require 'helpers/stats'

$LOGGER = Logger.new(STDOUT, 'daily')

EventMachine::run do
  $LOGGER.info('TODO: synchronize everything')

  EventMachine::start_server(CONFIG[:host], CONFIG[:port], Player)
  $LOGGER.info("Fifo started on #{CONFIG[:host]}:#{CONFIG[:port]}")

  Proxy.instance
end
