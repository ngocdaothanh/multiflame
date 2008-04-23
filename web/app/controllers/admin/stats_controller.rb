module Admin
  class StatsController < AdminController
    def index
      stat = Stat.find(:first, :order => 'created_at DESC')
      if stat.nil?
        render :text => _('No data yet'), :layout => true
        return
      end
      @created_at = stat.created_at
      @snapshot = stat.unzipped_snapshot
      @num_players = @snapshot.values.inject(0) { |sum, a| sum += a.size }
    end

    def create
      fm = FifoManager.new(FifoManager::CMD_WM_SNAPSHOT_CREATE, nil)
      if fm.result_or_error == :result
        if fm.result == 0
          flash[:notice] = _('No fifo for now')
        else
          sleep(3)
        end
      else
        flash[:notice] = _('Error creating new snapshot')
      end
      redirect_to(:action => 'index')
    end

    def show
      # Search back the selected fifo
      hostport = params[:id]
      stat = Stat.find(:first, :order => 'created_at DESC')
      if stat.nil?
        redirect_to(:action => 'index')
        return
      end
      snapshot = stat.unzipped_snapshot
      @fifo = nil
      snapshot.each_key do |k|
        if "#{k[:host].delete('.')}#{k[:port]}" == hostport
          @fifo = k
          break
        end
      end
      if @fifo.nil? or @fifo.keys.empty?
        redirect_to(:action => 'index')
        return
      end

      @created_at = stat.created_at

      # Take out game -> channel names

      # Sort by game id (because the keys are in the format "game_id/channel_name")
      key_ips = snapshot[@fifo]
      keys = key_ips.keys.sort

      # Find the first game with valid id (because game id maybe become invalid when
      # games are deleted etc., or < 0 when the container does not run in browser)
      nonnil_game_index = 0
      game = channel_name = nil
      (0...keys.size).each do |i|
        nonnil_game_index = i
        game, channel_name = parse_channel_key(keys[i])
        break unless game.nil?
      end
      if game.nil?
        flash[:notice] = _('All specified games have become invalid')
        redirect_to(:action => 'index')
        return
      end

      @gc = [{
        :game => game,
        :name_nums => [{:name => channel_name, :num => key_ips[keys[nonnil_game_index]].size}]
      }]

      ((nonnil_game_index + 1)...keys.size).each do |i|
        game, channel_name = parse_channel_key(keys[i])
        next if game.nil?
        if game.id == @gc[@gc.size - 1][:game].id
          @gc[@gc.size - 1][:name_nums] << {:name => channel_name, :num => key_ips[keys[i]].size}
        else
          @gc << {:game => game, :name_nums => [{:name => channel_name, :num => key_ips[keys[i]].size}]}
        end
      end
    end

  private

    # Returns [game, channel_name]
    def parse_channel_key(key)
      i = key.index('/')
      game_id = key[0...i]
      channel_name = key[i + 1, key.length]

      game = Game.find_by_id(game_id)
      [game, channel_name]
    end
  end
end
