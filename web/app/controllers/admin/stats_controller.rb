module Admin
  class StatsController < ApplicationController
    layout 'admin'
    before_filter :check_login_admin

    def index
      stat = Stat.find(:first, :order => 'created_at DESC')
      @snapshot = stat.unzipped_snapshot
    end

    def show
      # Search back the selected host
      hostport = params[:id]
      stat = Stat.find(:first, :order => 'created_at DESC')
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

      # Take out game -> channel names
      keys = snapshot[@fifo][:keys].sort
      game, channel_name = parse_channel_key(keys[0])
      @gc = [{:game => game, :channel_names => [channel_name]}]
      (1...keys.size).each do |i|
        game, channel_name = parse_channel_key(keys[i])
        if game.id == @gc[@gc.size - 1][:game].id
          @gc[@gc.size - 1][:channel_names] << channel_name
        else
          @gc << {:game => game, :channel_names => [channel_name]}
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