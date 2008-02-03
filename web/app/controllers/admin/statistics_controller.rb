require 'revent'

class X
  # Web -> manager
  CMD_WM_CHANNEL_KEYS_GET = 0
  CMD_WM_CG_RELOAD        = 1

  include ReventClient
  def initialize
    connect('localhost', 1943)
  end

  attr_reader :x
  def on_connect
    call(CMD_WM_CHANNEL_KEYS_GET, nil)
  end

  def on_result(cmd, arg)
    @x = arg
    EventMachine::stop_event_loop
  end
end

module Admin
  class StatisticsController < ApplicationController
    layout 'admin'
    before_filter :check_login_admin

    def index
      @x = nil
      x = nil
      EventMachine::run do
        x = X.new
      end
      @x = x.x
    end

    def login
    end
  end
end