module Admin
  class StatsController < ApplicationController
    layout 'admin'
    before_filter :check_login_admin

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
      key_ips = snapshot[@fifo]
      keys = key_ips.keys.sort
      game, channel_name = parse_channel_key(keys[0])
      @gc = [{:game => game, :name_nums => [{:name => channel_name, :num => key_ips[keys[0]].size}]}]
      (1...keys.size).each do |i|
        game, channel_name = parse_channel_key(keys[i])
        if game.id == @gc[@gc.size - 1][:game].id
          @gc[@gc.size - 1][:name_nums] << {:name => channel_name, :num => key_ips[keys[i]].size}
        else
          @gc << {:game => game, :name_nums => [{:name => channel_name, :num => key_ips[keys[0]].size}]}
        end
      end
    end

  private

    # Returns [game, channel_name]
    def parse_channel_key(key)
      i = key.index('/')
      game_id = key[0...i]
      channel_name = key[i + 1, key.length]

      game = Game.find(game_id)
      [game, channel_name]
    end
  end
end