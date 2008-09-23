ENV['ASONR'] = (ARGV[0] == 'production')? 'production' : 'development'
FIFO_ROOT = RAILS_ROOT = "#{File.dirname(__FILE__)}/.."

require 'rubygems'
require 'revent/as_r'
require 'revent/r_r'
require 'revent/captcha'
require 'singleton'
require 'logger'

require "#{FIFO_ROOT}/config/environment"
Dir.glob("#{FIFO_ROOT}/app/helpers/**/*.rb").each do |f|
  require f
end

Dir.glob("#{FIFO_ROOT}/config/initializers/**/*.rb").each do |f|
  require f
end

if ENV['ASONR'] == 'production'
  $logger = Logger.new("#{FIFO_ROOT}/log/production.log", 'daily')
  $logger.level = Logger::INFO
else
  $logger = Logger.new(STDOUT)
end

CAPTCHA = Revent::Captcha.new(CONF[:captcha_key], CONF[:captcha_length], CONF[:captcha_valid_period])

EventMachine::run do
  Proxy.instance
  Server.new(CONF[:host], CONF[:port])
  $logger.info("gserver started on #{CONF[:host]}:#{CONF[:port]}")
end
