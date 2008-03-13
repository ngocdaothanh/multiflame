ENV = {}
ENV['FIFO_ENV'] = (ARGV[0] == 'production')? 'production' : 'development'
FIFO_ROOT = RAILS_ROOT = "#{File.dirname(__FILE__)}/.."

require 'rubygems'
require 'revent/as_r'
require 'revent/r_r'
require 'revent/captcha'
require 'singleton'
require 'logger'
require 'actionmailer'

require "#{FIFO_ROOT}/config/environment"
Dir.glob("#{FIFO_ROOT}/helpers/**/*.rb").each do |f|
  require f
end

$: << "#{FIFO_ROOT}/vendor/plugins/action_mailer_tls/lib"
require "#{FIFO_ROOT}/vendor/plugins/action_mailer_tls/init"

Dir.glob("#{FIFO_ROOT}/config/initializers/**/*.rb").each do |f|
  require f
end

if ENV['FIFO_ENV'] == 'production'
  LOGGER = Logger.new("#{FIFO_ROOT}/log/fifo.log", 'daily')
else
  LOGGER = Logger.new(STDOUT)
end

CAPTCHA = Revent::Captcha.new(CONFIG[:captcha_key], CONFIG[:captcha_length], CONFIG[:captcha_valid_period])

EventMachine::run do
  Proxy.instance
  Server.new(CONFIG[:host], CONFIG[:port])
  LOGGER.info("Fifo started on #{CONFIG[:host]}:#{CONFIG[:port]}")
end
