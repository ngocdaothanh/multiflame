require 'zlib'

class Stat < ActiveRecord::Base
  # Returns the unzipped unmarshaled snapshot
  def unzipped_snapshot
    return @unzipped_snapshot if @unzipped_snapshot

    @unzipped_snapshot = Marshal.load(Zlib::Inflate.inflate(snapshot))
    @unzipped_snapshot
  end
end