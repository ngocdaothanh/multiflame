require 'gettext/utils'

# Reopen to make RubyGettext's ERB parser parse .html.erb files
module GetText
  module ErbParser
    @config = {
      :extnames => ['.rhtml', '.erb']
    }
  end
end

require "#{RAILS_ROOT}/vendor/plugins/haml/lib/haml"
module HamlParser
  module_function

  def target?(file)
    File.extname(file) == '.haml'
  end

  def parse(file, ary = [])
    haml = Haml::Engine.new(IO.readlines(file).join)
    code = haml.precompiled.split(/$/)
    RubyParser.parse_lines(file, code, ary)
  end
end
GetText::RGetText.add_parser(HamlParser)

namespace :gettext do
  desc 'Create mo-files for L10n'
  task :makemo do
    GetText.create_mofiles(true, 'po', 'locale')
  end

  desc 'Update pot/po files to match new version'
  task :updatepo do
    TEXT_DOMAIN = 'app'
    APP_VERSION = 'latest'
    GetText.update_pofiles(TEXT_DOMAIN, Dir.glob('{app}/**/*.*'), APP_VERSION)
  end
end