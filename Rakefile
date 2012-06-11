
srcdir = File.dirname(__FILE__)
homedir = ENV['HOME']

symlinked_files = FileList.new('.*', 'emacs', 'bin').exclude('.git', '.', '..')

task :clean => symlinked_files do |t|
  for f in t.prerequisites do
    src = File.join(srcdir, f)
    puts "Deleting #{src}"
    File.delete(File.join(homedir, f))
  end
end

task :build_symlinks => symlinked_files do |t|
  for f in t.prerequisites do
    if not File.exists?(File.join(homedir, f))
      src = File.join(srcdir, f)
      dst = File.join(homedir, f)
      puts "Symlinking #{src} => #{dst}"
      File.symlink(src, dst)
    end
  end
end

task :default => :build_symlinks
