# Filters added to this controller apply to all controllers in the application.
# Likewise, all the methods added will be available for all controllers.

class ApplicationController < ActionController::Base
  before_init_gettext :set_lang
  init_gettext 'app'

  def set_lang
    cookies[:lang] = params[:lang] if (params[:lang])
  end

  def check_login_admin
    # FIXME
    redirect_to home_path if request.remote_ip != '127.0.0.1'
  end
end