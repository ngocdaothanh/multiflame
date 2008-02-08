# Used by both the web and script/fifo_manager, things only used by the web
# should be put in environment.rb.

CONFIG = {
  :locales               => ['en', 'ja', 'vi'],
  :app_title             => 'web20games.net',
  :copyright             => '&copy; 2008 web20games.net',
  :new_threshold         => 3,  # Days, to determine if something is "NEW"
  :google_analytics_code => '',
  :swf_width             => 500,
  :swf_height            => 560,

  :fifo_manager_host              => 'localhost',
  :fifo_manager_port              => 1943,
  :fifo_manager_startup_delay     => 5,   # [sec], wait for the fifos to reconnect
  :fifo_manager_snapshot_interval => 10,  # [min], snapshots of the whole system, for statistics
  :captcha_salt_renew_interval    => 1    # [min]
}.freeze

# Make Active Record use UTC-base instead of local time
ActiveRecord::Base.default_timezone = :utc