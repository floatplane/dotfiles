
srcdir = File.dirname(__FILE__)
homedir = ENV['HOME']

symlinked_files = FileList.new('*').exclude("Rakefile", "prezto")
prezto_symlinks = FileList.new('prezto/runcoms/z*')
compiled_elisp = FileList.new('emacs/**/*.el').sub!(/.el$/, '.elc')

def try_delete_file(f)
  if File.exists? f
    puts "Deleting #{f}"
    File.delete(f)
  end
end

def make_symlink(src, dst)
  if not File.exists? dst
    puts "Symlinking #{src} => #{dst}"
    File.symlink(src, dst)
  end
end

desc "clean up everything"
task :clean do |t|
  for f in symlinked_files
    try_delete_file File.join(homedir, f)
  end
  for f in prezto_symlinks
    try_delete_file File.join(homedir, "." + File.basename(f))
  end
  try_delete_file File.join(homedir, ".zprezto")
  for f in compiled_elisp
    try_delete_file File.join(srcdir, f)
  end
end

task :build_normal_symlinks => symlinked_files do |t|
  t.prerequisites.each do |f|
    add_dot = !['bin', 'Brewfile'].include?(f)
    src = File.join(srcdir, f)
    dst = File.join(homedir, "#{add_dot ? '.' : ''}#{f}")
    make_symlink src, dst
  end
end

desc "build symlinks in home dir pointing to prezto init files"
task :build_prezto_symlinks => prezto_symlinks do |t|
  for f in t.prerequisites do
    src = File.join(srcdir, f)
    dst = File.join(homedir, "." + File.basename(f))
    make_symlink src, dst
  end
  src = File.join(srcdir, "prezto")
  dst = File.join(homedir, ".zprezto")
  make_symlink src, dst
end

desc "build symlinks in home dir pointing to these files"
task :build_symlinks => [:build_normal_symlinks, :build_prezto_symlinks]

desc "run build_symlinks"
task :default => [:build_symlinks]
