# Methods added to this helper will be available to all templates in the application.
module ApplicationHelper
  def flags_to_locales
    ret = ''
    destination = @flag_destination || url_for
    CONF[:locales].each do |l|
      ret << "<div>#{link_to image_tag("flags/#{l}.png", :alt => ''), "#{destination}?lang=#{l}"}</div>"
    end
    ret
  end

  def render_games(games)
    now = Time.now
    notice = _('Please install %{getflashplayer}') % {:getflashplayer => link_to(_('Flash player 9 or above'), 'http://www.adobe.com/go/getflashplayer')}
    ret = ''
    games.each do |g|
      ret << '<div>'
      ret << '<h2>' + link_to(g.name, game_path(g))
      ret << image_tag('new.gif') if (now - g.updated_at) <= CONF[:new_threshold]*24*60*60
      ret << '</h2>'
      ret << "<div id='game_flash#{g.id}' class='notice'>#{notice}</div>"
      ret << '</div>'
    end
    ret
  end

  def render_toys(game, toys)
    now = Time.now
    notice = _('Please install %{getflashplayer}') % {:getflashplayer => link_to(_('Flash player 9 or above'), 'http://www.adobe.com/go/getflashplayer')}
    ret = ''
    toys.each do |t|
      ret << '<div>'
      ret << '<h2>' + link_to(t.name, toy_path(t))
      if (now - t.updated_at) <= CONF[:new_threshold]*24*60*60
        ret << image_tag('new.gif')
      end
      ret << '</h2>'
      ret << "<div id='toy_flash#{t.id}' class='notice'>#{notice}</div>"
      ret << '</div>'
    end
    ret
  end

  # attributes: Array of attributes to be displayed in order
  def error_messages_for(object, attributes)
    return '' if object.nil? or object.errors.count == 0

    html = {}
    [:id, :class].each do |key|
      html[key] = 'errorExplanation'
    end

    error_messages = []
    attributes = object.attribute_names if attributes.nil?
    attributes.unshift(:base)
    attributes.each do |a|
      msgs = object.errors.on(a)
      next if msgs.blank?
      if msgs.is_a?(String)
        error_messages << content_tag(:li, msgs)
      else
        if a != :base
          error_messages << content_tag(:li, msgs[0])
        else
          msgs.each do |msg|
            error_messages << content_tag(:li, msg)
          end
        end
      end
    end

    contents = ''
    contents << content_tag(:h2, _('Error'))
    contents << content_tag(:ul, error_messages)
    content_tag(:div, contents, html)
  end
end