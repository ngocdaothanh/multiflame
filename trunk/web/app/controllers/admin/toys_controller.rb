module Admin
  class ToysController < ApplicationController
    layout 'admin'

    def index
      @toys = Toy.find(:all, :order => 'id')
    end

    def new
    end

    def create
      @toy = Toy.new
      if @toy.upload_and_create(params)
        flash[:notice] = _('%s was uploaded') % ERB::Util.html_escape(@toy.name)
        redirect_to :action => 'index'
      else
        render :action => 'new'
      end
    end

    def edit
      @toy = Toy.find(params[:id])
    end

    def update
      @toy = Toy.find(params[:id])
      if @toy.upload_and_update(params)
        flash[:notice] = _('%s was updated') % ERB::Util.html_escape(@toy.name)
        redirect_to :action => 'index'
      else
        render :action => 'edit'
      end
    end
  end
end