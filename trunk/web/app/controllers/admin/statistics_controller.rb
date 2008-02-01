module Admin
  class StatisticsController < ApplicationController
    layout 'admin'
    before_filter :check_login_admin

    def index
    end

    def login
    end
  end
end