class Toy < ActiveRecord::Base
  serialize :names

  # Returns the modified time of the file, which is used as version number
  def updated_at
    File.stat("#{RAILS_ROOT}/public/toys/#{self.id}/toy.swf").mtime
  end

  # Validates and creates game, returns false if there is error
  def upload_and_create(params)
    files = {'toy' => 'toy.swf'}
    locales = CONFIG[:locales].dup;
    locales.delete('en')
    locales.each do |l|
      files["mo_#{l}"] = "#{l}.mo"
    end

    self.names = names = {}
    CONFIG[:locales].each do |l|
      names[l] = params["name_#{l}"]
      self.names[l] = names[l] unless names[l].blank?
    end

    # Validate
    if params['toy'].blank?
      self.errors.add_to_base(N_('Please provide toy.swf'))
    end
    if names['en'].blank?
      self.errors.add_to_base(N_('Please provide toy name in English'))
    end
    return false unless self.errors.empty?

    # Save the names
    self.save

    FileUtils.rm_r("#{RAILS_ROOT}/public/toys/#{self.id}", :force => true)
    FileUtils.mkdir("#{RAILS_ROOT}/public/toys/#{self.id}")
    files.each do |k, v|
      data = params[k]
      unless data.blank?
        f = File.new("#{RAILS_ROOT}/public/toys/#{self.id}/#{v}", 'wb')
        f.write(data.read)
        f.close
      end
    end
    true
  end

  # Validates and updates game, returns false if there is error
  def upload_and_update(params)
    files = {'toy' => 'toy.swf'}
    CONFIG[:locales].each do |l|
      files["mo_#{l}"] = "#{l}.mo"
    end

    self.names = names = {}
    CONFIG[:locales].each do |l|
      names[l] = params["name_#{l}"]
      self.names[l] = names[l] unless names[l].blank?
    end

    # Validate
    if names['en'].blank?
      self.errors.add_to_base(N_('Please provide toy name in English'))
      return false
    end

    # Save the names
    self.save

    files.each do |k, v|
      data = params[k]
      unless data.blank?
        f = File.new("#{RAILS_ROOT}/public/toys/#{self.id}/#{v}", 'wb')
        f.write(data.read)
        f.close
      end
    end
    true
  end

  # Returns name in the specified locale or current locale, falls back to English
  def name(locale = nil)
    locale = GetText.locale.to_s if locale.nil?
    ret = self.names[locale]
    if ret.blank?
      ret = self.id.nil? ? '' : self.names['en']
    end
    ret
  end
end
