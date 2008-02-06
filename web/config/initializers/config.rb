$KCODE = 'u'
require 'jcode'
require 'gettext/rails'

WEB_SITE = 'web20games.net'

LOCALES = [
  'en',
  'ja',
  'vi'
].freeze

CONFIG = {
  :app_title             => 'web20games.net',
  :copyright             => '&copy; 2008 web20games.net',

  :new_threshold         => 3,  # Days, to determine if something is "NEW"

  :google_analytics_code => '',

  :swf_width             => 500,
  :swf_height            => 560,

  :fifo_manager_host              => 'localhost',
  :fifo_manager_port              => 1943,
  :fifo_manager_startup_delay     => 5,  # [sec], wait for the fifos to reconnect
  :fifo_manager_snapshot_interval => 10  # [min], snapshots of the whole system, for statistics
}.freeze