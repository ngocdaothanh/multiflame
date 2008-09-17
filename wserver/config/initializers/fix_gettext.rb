# RubyGettext 0.92.0: there will be error if the code below is put in environment.rb

$KCODE = 'u'
require 'jcode'
require 'gettext/rails'

# http://zargony.com/2008/02/12/edge-rails-and-gettext-undefined-method-file_exists-nomethoderror
module ActionView
  class Base
    delegate :file_exists?, :to => :finder unless respond_to?(:file_exists?)
  end
end
