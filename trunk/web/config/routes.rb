ActionController::Routing::Routes.draw do |map|
  # URLs for the SWF files, fixed in agreement with other parts of the system
  map.game_container_without_versions 'cwov/:id/:channel/:locale',
    :controller => 'swf', :action => 'game_container_without_versions'
  map.game_container_with_versions 'cwv/:id/:channel/:locale/:container_version/:game_version',
    :controller => 'swf', :action => 'game_container_with_versions'
  map.game_with_version 'gwv/:id/:version',
    :controller => 'swf', :action => 'game_with_version'
  map.toy_with_config 'twc/:id/:config',
    :controller => 'swf', :action => 'toy_with_config'

  map.home      '', :controller => 'games'
  map.play      'games/:id/:channel', :controller => 'games', :action => 'play'
  map.play_lang 'games/:id/:channel/:lang', :controller => 'games', :action => 'play'

  map.resources :games
  map.resources :toys

  map.connect 'admin', :controller => 'admin/statistics'
  map.namespace :admin do |admin|
    admin.resources :statistics
    admin.resource  :game_container, :controller => 'game_container'
    admin.resources :games
    admin.resources :toys
  end

  # Install the default route as the lowest priority.
  map.connect ':controller/:action/:id.:format'
  map.connect ':controller/:action/:id'
end