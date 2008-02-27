class ToyContainer
  include Singleton

  # Validates and updates client, returns false if there is error
  def upload_and_update(params)
    data = params['container']
    unless data.blank?
      f = File.new("#{RAILS_ROOT}/public/toys/container.swf", 'wb')
      f.write(data.read)
      f.close
    end
    true
  end
end