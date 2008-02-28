FLASH = 'C:/Program Files/Adobe/Adobe Flash CS3/Flash.exe'

unless File.exist?(FLASH)
  puts "Please change FLASH constant in publish.rb"
  abort
end

if ARGV.size > 1
  puts 'Usage: publish.rb [path to a .fla file]'
  abort
end

flas = []
if ARGV.size == 1
  flas = ARGV
  unless File.exist?(flas[0])
    puts "#{flas[0]} does not exist"
    abort
  end
else
  flas = Dir.glob('**/*.fla')
end

# Convert to absolute path
dir = File.dirname(__FILE__)
flas = flas.map { |f| File.expand_path(f) }

# Build .jsfl
jsfl =<<EOL
fl.outputPanel.clear();

EOL

flas.each do |f|
  dir = File.dirname(f)
  base = File.basename(f)
  swf = dir + '/' +
    base[0, base.length - 4] +  # Delete '.fla'
    '.swf'

  jsfl +=<<EOL
var doc = fl.openDocument('file:///#{f}');
fl.outputPanel.trace('Publishing SWF - #{f}');
doc.exportSWF('file:///#{swf}', true);
fl.closeDocument(doc, false);

EOL
end

jsfl_file = 'publish.jsfl'
File.open(jsfl_file, 'w') do |f|
  f.write(jsfl)
end
system("#{FLASH} #{File.expand_path(jsfl_file)}")
File.delete(jsfl_file)
