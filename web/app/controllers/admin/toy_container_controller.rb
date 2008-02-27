module Admin
  class ToyContainerController < ApplicationController
    layout 'admin'
    before_filter :check_login_admin

    def edit
    end

    def update
      container = ToyContainer.instance
      if container.upload_and_update(params)
        flash[:notice] = _('Container was updated')
        redirect_to admin_toys_path
      else
        render :action => 'edit'
      end
    end
  end
end