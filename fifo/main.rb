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

require 'actionmailer'
require 'helpers/jpg_mailer'

abort "Usage: #{__FILE__} <development|production>" unless ARGV[0]
if ARGV.size == 1 and ARGV[0] == 'production'
  $LOGGER = Logger.new('log/fifo.log', 'daily')
else
  $LOGGER = Logger.new(STDOUT)
end

$CAPTCHA = Revent::Captcha.new(CONFIG[:captcha_key], CONFIG[:captcha_length], CONFIG[:captcha_valid_period])

EventMachine::run do
  Proxy.instance
  Server.new(CONFIG[:host], CONFIG[:port])
  $LOGGER.info("Fifo started on #{CONFIG[:host]}:#{CONFIG[:port]}")
end
