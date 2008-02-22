require 'rubygems'
require 'revent/as_r'
require 'revent/r_r'
require 'revent/captcha'
require 'singleton'
require 'logger'

require 'config'
require 'helpers/server'
require 'helpers/channel'
require 'helpers/pended_channel'
require 'helpers/lobby'
require 'helpers/room'
require 'helpers/utils'
require 'helpers/batch_move'
require 'helpers/proxy'
require 'helpers/stats'

#$LOGGER = Logger.new('log/fifo.log', 'daily')
$LOGGER = Logger.new(STDOUT, 'daily')

$CAPTCHA = Revent::Captcha.new(CONFIG[:captcha_key], CONFIG[:captcha_length], CONFIG[:captcha_valid_period_in_sec])

EventMachine::run do
  Server.new(CONFIG[:host], CONFIG[:port])
  $LOGGER.info("Fifo started on #{CONFIG[:host]}:#{CONFIG[:port]}")

  Proxy.instance
end
