require 'zlib'

class Stat < ActiveRecord::Base
  # Returns an array of Fifo servers.
  def fifos
    obj.keys
  end

private

  # Returns the unzipped unmarshaled snapshot
  def obj
    return @obj if @obj

    @obj = Marshal.load(Zlib::Inflate.inflate(snapshot))
    @obj
  end
end