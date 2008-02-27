ActionController::Routing::Routes.draw do |map|
  map.home    '', :controller => 'games'
  map.privacy 'privacy', :controller => 'games', :action => 'privacy'

  # URLs for the SWF files, fixed in agreement with other parts of the system
  map.game_container_without_versions 'cwov/:id/:channel/:locale',
    :controller => 'swf', :action => 'game_container_without_versions'
  map.game_container_with_versions 'cwv/:id/:channel/:locale/:container_version/:game_version/:h/:p',
    :controller => 'swf', :action => 'game_container_with_versions'
  map.game_with_version 'gwv/:id/:version',
    :controller => 'swf', :action => 'game_with_version'
  map.toy_container_without_config 'toys/:id/:locale',
    :controller => 'swf', :action => 'toy_container'
  map.toy_container_with_config 'toys/:id/:locale/:config',
    :controller => 'swf', :action => 'toy_container'

  map.play      'games/:id/:channel', :controller => 'games', :action => 'play'
  map.play_lang 'games/:id/:channel/:lang', :controller => 'games', :action => 'play'

  map.resources :games
  map.resources :toys, :collection => {:captcha => :get, :mail => :post}

  map.connect 'admin', :controller => 'admin/stats'
  map.namespace :admin do |admin|
    admin.resources :stats
    admin.resource  :game_container, :controller => 'game_container'
    admin.resources :games
    admin.resource  :toy_container, :controller => 'toy_container'
    admin.resources :toys
  end

  # Install the default route as the lowest priority.
  map.connect ':controller/:action/:id.:format'
  map.connect ':controller/:action/:id'
end