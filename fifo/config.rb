CONFIG = {
  # Address on the machine that this fifo runs
  :host => '0.0.0.0',
  :port => 443,

  # Address for game containers to connect
  :swf_host => 'localhost',
  :swf_port => 443,

  :manager_host => 'localhost',
  :manager_port => 1943,
  :manager_reconnect_interval => 1,  # [sec]

  :players_limit => 5000,  # Initial value, can be changed by the Fifo manager
  :max_cmd_length => 1024  # Can't be too long, for security
}.freeze