class PendedChannel
  @@channels = {}

  def self.login(key, player, container_version, game_version, batch_game)
    @@channels.synchronize do
      channel = @@channels[key]
      if channel.nil?
        PendedChannel.new(key, player, container_version, game_version, batch_game)
        Proxy.instance.fm_channel_create(key)
      else
        channel.login(player, container_version, game_version, batch_game)
      end
    end
  end

  def self.login_pended(key, host, port)
    @@channels.synchronize do
      channel = @@channels[key]
      return if channel.nil?

      unless host.nil?
        channel.players.each do |p|
          p.call(Server::CMD_LOGIN, [Channel::LOGIN_REDIRECT, [host, port]], true)
        end
      else
        Channel.new(key, channel.players, channel.container_version, channel.game_version, channel.batch_game)
      end
      @@channels.delete(key)
    end
  end

  attr_reader :players, :container_version, :game_version, :batch_game

  def initialize(key, player, container_version, game_version, batch_game)
    @@channels[key] = self

    player.session[:channel] = self
    @players = [player]

    @container_version = container_version
    @game_version      = game_version
    @batch_game        = batch_game
  end

  def login(player, container_version, game_version, batch_game)
    code = Channel::LOGIN_OK
    if nicks.include?(player.session[:nick])
      code = Channel::LOGIN_DUPLICATE_NICK 
    elsif container_version != @container_version
      code = Channel::LOGIN_DIFFERENT_CONTAINER_VERSION 
    elsif game_version != @game_version or batch_game != @batch_game
      code = Channel::LOGIN_DIFFERENT_GAME_VERSION
    end

    if code == Channel::LOGIN_OK
      player.session[:channel] = self
      @players << player
    else
      player.call(Server::CMD_LOGIN, [code, nil], true)
    end
  end

  def nicks
    @players.map { |p| p.session[:nick] }
  end

  def process(player, cmd, arg)
    @players.delete(player)
    player.close_connection if cmd != Server::CMD_LOGOUT
  end
end