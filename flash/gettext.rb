require 'gettext/utils'

module GetText
  module_function

  def msgmerge_all(textdomain, app_version, po_root = 'po', refpot = 'tmp.pot')
    FileUtils.mkdir_p(po_root) unless FileTest.exist? po_root
    msgmerge("#{po_root}/#{textdomain}.pot", refpot, app_version)

    Dir.glob("#{po_root}/*.po"){ |f|
      lang = /#{po_root}\/(.*)\.po/.match(f).to_a[1]
      msgmerge("#{po_root}/#{lang}.po", refpot, app_version)
    }
  end

  def update_pofiles(textdomain, files, app_version, po_root = 'po', refpot = 'tmp.pot')
    files2 = '"' + files.join('" "') + '"'
    system("xgettext #{files2} -L Python --from-code=UTF-8 --output=#{refpot} --no-wrap --sort-output")
    if !FileTest.exist?(refpot)
      puts 'No GetText string extracted'
      return
    end

    msgmerge_all(textdomain, app_version, po_root, refpot)
    File.delete(refpot)
  end

  def create_mofiles(verbose = false, podir = './po', targetdir = './data/locale', targetpath_rule = '%s/LC_MESSAGES')
    modir = File.join(targetdir, targetpath_rule)
    Dir.glob(File.join(podir, "*.po")) do |file|
      lang = /\/([^\/]+?)\.po/.match(file[podir.size..-1]).to_a[1]
      FileUtils.mkdir_p(targetdir) unless File.directory?(targetdir)
      rmsgfmt(file, File.join(targetdir, "#{lang}.mo"))
      if verbose
        $stderr.puts %Q[#{file} -> #{File.join(targetdir, "#{lang}.mo")}]
      end
    end
  end
end

def updatepo
  puts 'Update pot/po files'
  GetText.update_pofiles('locale', Dir.glob('**/*.as'), '')
end

def makemo
  puts 'Create mo-files'
  GetText.create_mofiles(true, 'po', 'mo')
end

def main
  if ARGV.size != 1
    puts 'Usage: gettext.rb <updatepo|makemo>'
  elsif ARGV[0] == 'updatepo'
    updatepo
  elsif ARGV[0] == 'makemo'
    makemo
  else
    puts 'gettext.rb <updatepo|makemo>'
  end
end
main
