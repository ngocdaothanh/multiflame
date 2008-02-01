module Admin
  class GameContainerController < ApplicationController
    layout 'admin'
    before_filter :check_login_admin

    def edit
    end

    def update
      container = GameContainer.instance
      if container.upload_and_update(params)
        flash[:notice] = _('Container was updated')
        redirect_to admin_games_path
      else
        render :action => 'edit'
      end
    end
  end
end