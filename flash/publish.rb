require 'flash_path'

unless File.exist?(FLASH_PATH)
  puts "Please change FLASH_PATH constant in flash_path.rb"
  abort
end

if ARGV.size > 1
  puts 'Usage:'
  puts '* To compile only one file:'
  puts 'publish.rb <path to the .fla file>'
  puts '* To compile all files in the current directory and its subdirectories:'
  puts 'publish.rb'
  abort
end

flas = []
if ARGV.size == 1
  flas << ARGV[0]
  unless File.exist?(flas[0])
    puts "#{flas[0]} does not exist"
    abort
  end
else
  flas.concat(Dir.glob('./**/*.fla'))
end

# Convert to absolute path
dir = File.dirname(__FILE__)
flas = flas.map { |f| File.expand_path(f) }

# Build .jsfl
jsfl = %q{
fl.outputPanel.clear();
}

flas.each do |f|
  dir = File.dirname(f)
  base = File.basename(f)
  swf =
    dir + '/' +
    base[0, base.length - 4] +  # Remove the ".fla" part
    '.swf'

  jsfl += %Q{
var doc = fl.openDocument('file://#{f}');
fl.outputPanel.trace('Publishing SWF - #{f}');
doc.exportSWF('file://#{swf}', true);
fl.closeDocument(doc, false);
}
end

jsfl_file = 'publish.jsfl'
File.open(jsfl_file, 'w') do |f|
  f.write(jsfl)
end
system("'#{FLASH_PATH}' '#{File.expand_path(jsfl_file)}'")
File.delete(jsfl_file)
