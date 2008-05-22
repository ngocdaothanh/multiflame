module Admin
  class AdminController < ApplicationController
    layout 'admin'
    before_filter :check_login_admin

  protected

    def check_login_admin
      # FIXME
      redirect_to(root_path) if request.remote_ip != '127.0.0.1'
    end
  end
end
