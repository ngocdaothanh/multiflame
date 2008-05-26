class SwfController < ApplicationController
  skip_before_filter :init_gettext
  skip_after_filter  :init_content_type
  skip_before_filter :set_lang

  def game_container_without_versions
    game_container = GameContainer.instance
    game = Game.find(params[:id])

    gm = GserverManager.new(GserverManager::CMD_WM_WHICH_FIFO,
      GserverManager.channel_key(game.id, params[:channel]))
    if !gm.nil? and gm.result_or_error == :result and !gm.result.nil?
      host = gm.result[1]
      port = gm.result[2]
    else
      # The game container will fail to connect and display message that the server is down
      host = '0'
      port = 0
    end

    redirect_to(game_container_with_versions_path(
      :id                => game.id,
      :channel           => Base64.encode64(CGI.escape(params[:channel])).strip,
      :locale            => params[:locale],
      :container_version => game_container.updated_at.to_i,
      :game_version      => game.updated_at.to_i,

      # Not "host" and "port" to avoid conflict with Rails
      :h                 => Base64.encode64(CGI.escape(host)).strip,
      :p                 => Base64.encode64(port.to_s).strip
    ))
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

  def toy_container
    toy = Toy.find(params[:id])
    send_file("#{RAILS_ROOT}/public/toys/container.swf",
      :filename    => 'toy.swf',
      :type        => 'application/x-shockwave-flash',
      :disposition => 'inline',
      :stream      => false)
  end
end