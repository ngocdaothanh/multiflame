class Schema < ActiveRecord::Migration
  def self.up
    create_table :games do |t|
      t.text :names, :null => false  # Hash of {locale => name}, serialized using YML
    end

    create_table :toys do |t|
      t.text :names, :null => false  # Hash of {locale => name}, serialized using YML
    end

    create_table :stats do |t|
      t.binary :snapshot,     :null => false  # Compressed by zlib
      t.datetime :created_at, :null => false
    end
  end

  def self.down
    drop_table :games
    drop_table :toys
    drop_table :stats
  end
end
