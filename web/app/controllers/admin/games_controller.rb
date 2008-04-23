module Admin
  class GamesController < AdminController
    def index
      @games = Game.find(:all, :order => 'id')
    end

    def new
    end

    def create
      @game = Game.new
      if @game.upload_and_create(params)
        FifoManager.new(FifoManager::CMD_WM_CG_RELOAD, nil)
        flash[:notice] = _('%s was uploaded') % ERB::Util.html_escape(@game.name)
        redirect_to :action => 'index'
      else
        render :action => 'new'
      end
    end

    def edit
      @game = Game.find(params[:id])
    end

    def update
      @game = Game.find(params[:id])
      if @game.upload_and_update(params)
        FifoManager.new(FifoManager::CMD_WM_CG_RELOAD, nil)
        flash[:notice] = _('%s was updated') % ERB::Util.html_escape(@game.name)
        redirect_to :action => 'index'
      else
        render :action => 'edit'
      end
    end
  end
end
