class ApplicationController < ActionController::Base
  before_init_gettext :set_lang
  init_gettext 'app'

  def rescue_404
    render(:template => 'errors/404', :layout => true, :status => 404)
  end

protected

  def set_lang
    cookies[:lang] = params[:lang] if params[:lang]
  end

  def rescue_action_in_public(exception)
    render(:template => 'errors/500', :layout => true, :status => 500)
  end
end
