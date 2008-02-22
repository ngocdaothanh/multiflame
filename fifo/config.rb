CONFIG = {
  :captcha_key                 => 'captcha_key',
  :captcha_length              => 3,
  :captcha_valid_period_in_sec => 15,

  # Address on the machine that this fifo runs
  :host => '0.0.0.0',
  :port => 443,

  # Address for game containers to connect
  :swf_host => 'localhost',
  :swf_port => 443,

  :manager_host => 'localhost',
  :manager_port => 1943,
  :manager_reconnect_interval => 60,  # [sec]

  :players_limit  => 5000,  # Initial value, can be changed by the Fifo manager

  :max_lag         => 2,  # [sec]
  :game_over_delay => 2   # [sec]
}.freeze
