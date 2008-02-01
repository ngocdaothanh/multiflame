class CreateSchema < ActiveRecord::Migration
  def self.up
    create_table :games do |t|
      t.text :names, :null => false  # Hash of locale => name, serialized using YML
    end

    create_table :toys do |t|
      t.text :names, :null => false  # Hash of locale => name, serialized using YML
    end
  end

  def self.down
    drop_table :games
    drop_table :toys
  end
end