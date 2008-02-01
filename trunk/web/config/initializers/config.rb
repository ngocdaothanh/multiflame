$KCODE = 'u'
require 'jcode'
require 'gettext/rails'

WEB_SITE = 'web20games.net'

# Nicks of admins
ADMINS = [
  'ngocdaothanh'
].freeze

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
  :swf_height            => 560
}.freeze