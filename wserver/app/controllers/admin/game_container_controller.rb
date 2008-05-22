module Admin
  class GameContainerController < AdminController
    def edit
    end

    def update
      container = GameContainer.instance
      if container.upload_and_update(params)
        FifoManager.new(FifoManager::CMD_WM_CG_RELOAD, nil)
        flash[:notice] = _('Container was updated')
        redirect_to admin_games_path
      else
        render :action => 'edit'
      end
    end
  end
end