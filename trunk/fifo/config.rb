CONFIG = {
  :host => '0.0.0.0',
  :port => 443,
  :fifo_manager_host => 'localhost',
  :fifo_manager_port => 1935,
  :players_limit => 5000,  # Initial value, can be changed by the Fifo manager
  :max_cmd_length => 1024  # Can't be too long, for security
}.freeze