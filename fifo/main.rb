require 'rubygems'
require 'eventmachine'
require 'json'
require 'singleton'
require 'logger'

require 'config'
require 'helpers/player'
require 'helpers/game'
require 'helpers/captcha'
require 'helpers/channel'
require 'helpers/lobby'
require 'helpers/room'
require 'helpers/utils'
require 'helpers/play_actions'
require 'helpers/batch_move'
require 'helpers/proxy'
require 'helpers/stats'

logdev = STDOUT
$LOGGER = Logger.new(logdev, 'daily')

EventMachine::run do
  puts 'TODO: synchronize everything'
  puts "Fifo started on #{CONFIG[:host]}:#{CONFIG[:port]}"
  EventMachine::start_server(CONFIG[:host], CONFIG[:port], Player)
end