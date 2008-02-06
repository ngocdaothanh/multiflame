class SwfController < ApplicationController
  skip_before_filter :init_gettext
  skip_after_filter  :init_content_type
  skip_before_filter :set_lang

  def game_container_without_versions
    game_container = GameContainer.instance
    game = Game.find(params[:id])

    fm = FifoManager.new(FifoManager::CMD_WM_WHICH_FIFO,
      FifoManager.channel_key(game.id, params[:channel]))
    if fm.result_or_error == :result
      host = fm.result[1]
      port = fm.result[2]
    else
      host = 0
      port = 0
    end

    redirect_to game_container_with_versions_path(
      :id                => game.id,
      :channel           => Base64.b64encode(params[:channel]).strip,
      :locale            => params[:locale],
      :container_version => game_container.updated_at.to_i,
      :game_version      => game.updated_at.to_i,
      :h                 => Base64.b64encode(host).strip,
      :p                 => port
    )
  end

  def game_container_with_versions
    game_container = GameContainer.instance
    game = Game.find(params[:id])
    if params[:container_version] != game_container.updated_at.to_i.to_s or
        params[:game_version] != game.updated_at.to_i.to_s
      render :nothing => true, :status => 404
    else
      send_file("#{RAILS_ROOT}/public/games/container.swf",
        :filename    => 'container.swf',
        :type        => 'application/x-shockwave-flash',
        :disposition => 'inline',
        :stream      => false)
    end
  end

  def game_with_version
    game = Game.find(params[:id])
    if params[:version] != game.updated_at.to_i.to_s
      render :nothing => true, :status => 404
    else
      # Security check: make sure that this is an integer
      id = params[:id].to_i
      send_file("#{RAILS_ROOT}/public/games/#{id}/game.swf",
        :filename    => 'game.swf',
        :type        => 'application/x-shockwave-flash',
        :disposition => 'inline',
        :stream      => false)
    end
  end

  def toy_with_config
    toy = Toy.find(params[:id])
    # Security check: make sure that this is an integer
    id = params[:id].to_i
    send_file("#{RAILS_ROOT}/public/toys/#{id}/toy.swf",
      :filename    => 'toy.swf',
      :type        => 'application/x-shockwave-flash',
      :disposition => 'inline',
      :stream      => false)
  end
end