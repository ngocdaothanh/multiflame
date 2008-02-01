class GameContainer
  include Singleton

  # Returns the modified time of the file, which is used as version number
  def updated_at
    File.stat("#{RAILS_ROOT}/public/games/container.swf").mtime
  end

  # Validates and updates client, returns false if there is error
  def upload_and_update(params)
    files = {'container' => 'container.swf'}
    locales = LOCALES.dup;
    locales.delete('en')
    locales.each do |l|
      files[l] = "#{l}.mo"
    end
    files.each do |k, v|
      data = params[k]
      unless data.blank?
        f = File.new("#{RAILS_ROOT}/public/games/#{v}", 'wb')
        f.write(data.read)
        f.close
      end
    end
    true
  end
end