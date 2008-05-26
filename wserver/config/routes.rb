ActionController::Routing::Routes.draw do |map|
  map.root    :controller => 'games', :action => 'index'
  map.terms   'terms',   :controller => 'pr', :action => 'terms'
  map.privacy 'privacy', :controller => 'pr', :action => 'privacy'

  #-----------------------------------------------------------------------------

  # URLs for the SWF files, fixed in agreement with other parts of the system
  map.game_container_without_versions 'gcwov/:id/:channel/:locale',
    :controller => 'swf', :action => 'game_container_without_versions'
  map.game_container_with_versions 'gcwv/:id/:channel/:locale/:container_version/:game_version/:h/:p',
    :controller => 'swf', :action => 'game_container_with_versions'
  map.game_with_version 'gwv/:id/:version',
    :controller => 'swf', :action => 'game_with_version'

  map.play      'games/:id/:channel', :controller => 'games', :action => 'play'
  map.play_lang 'games/:id/:channel/:lang', :controller => 'games', :action => 'play'

  map.resources :games

  #-----------------------------------------------------------------------------

  if CONF[:toy]
    map.toy_container_without_config 'toys/:id/:locale',
      :controller => 'swf', :action => 'toy_container'
    map.toy_container_with_config 'toys/:id/:locale/:config',
      :controller => 'swf', :action => 'toy_container'
    map.resources :toys
  end

  #-----------------------------------------------------------------------------

  map.connect 'admin', :controller => 'admin/stats'
  map.namespace :admin do |admin|
    admin.resources :stats
    admin.resource  :game_container, :controller => 'game_container'
    admin.resources :games

    if CONF[:toy]
      admin.resource  :toy_container, :controller => 'toy_container'
      admin.resources :toys
    end
  end

  unless ::ActionController::Base.consider_all_requests_local
    map.connect '*path', :controller => 'application', :action => 'rescue_404'
  end
end
