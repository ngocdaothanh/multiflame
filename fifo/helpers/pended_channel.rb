class PendedChannel
  @@channels = {}
  @@mutex_channels = Mutex.new

  def self.login(key, player, container_version, game_version, batch_game)
    @@mutex_channels.synchronize do
      channel = @@channels[key]
      if channel.nil?
        channel = PendedChannel.new(key, player, container_version, game_version, batch_game)
        @@channels[key] = channel
        Proxy.instance.fm_channel_create(key)
      else
        channel.login(player, container_version, game_version, batch_game)
      end
    end
  end

  def self.login_pended(key, destination)
    @@mutex_channels.synchronize do
      channel = @@channels[key]
      return if channel.nil?

      unless destination.nil?
        channel.players.each do |p|
          p.invoke(Player::CMD_LOGIN, [Channel::LOGIN_REDIRECT, [destination[:host], destination[:port]]])
          p.close_connection_after_writing
        end
      else
        Channel.new(key, channel.players, channel.container_version, channel.game_version, channel.batch_game)
      end
      @@channels.delete(key)
    end
  end

  attr_reader :players, :container_version, :game_version, :batch_game

  def initialize(key, player, container_version, game_version, batch_game)
    player.channel = self
    @players = [player]

    @container_version = container_version
    @game_version      = game_version
    @batch_game        = batch_game
  end

  def login(player, container_version, game_version, batch_game)
    code = Channel::LOGIN_OK
    if nicks.include?(player.nick)
      code = Channel::LOGIN_DUPLICATE_NICK 
    elsif container_version != @container_version
      code = Channel::LOGIN_DIFFERENT_CONTAINER_VERSION 
    elsif game_version != @game_version or batch_game != @batch_game
      code = Channel::LOGIN_DIFFERENT_GAME_VERSION
    end

    if code == Channel::LOGIN_OK
      player.channel = self
      @players << player
    else
      player.invoke(Player::CMD_LOGIN, [code, nil])
      player.close_connection_after_writing
    end
  end

  def nicks
    @players.map { |p| p.nick }
  end

  def process(player, cmd, arg)
    @players.delete(player)
    player.close_connection if cmd != Player::CMD_LOGOUT
  end
end