
require 'fileutils'

srcdir = File.dirname(__FILE__)
homedir = ENV['HOME']

# Trees whose git-tracked files are symlinked file-by-file into a target
# directory that also holds untracked runtime state, so the directory itself
# must stay a real directory (e.g. ~/.claude has sessions and caches,
# ~/.config has dozens of unmanaged apps).
tree_symlinks = {
  'claude' => '.claude',
  'codex' => '.codex',
  'config' => '.config',
}

symlinked_files = FileList.new('*')
  .exclude('Rakefile', 'prezto', *tree_symlinks.keys)
  .exclude('#*#') # emacs autosave files
prezto_symlinks = FileList.new('prezto/runcoms/z*')
compiled_elisp = FileList.new('emacs/**/*.el').sub!(/.el$/, '.elc')

def tracked_tree_files(srcdir, src_root)
  `git -C '#{srcdir}' ls-files -z -- '#{src_root}'`.split("\0")
end

def try_delete_file(f)
  if File.symlink?(f) || File.exist?(f)
    puts "Deleting #{f}"
    File.delete(f)
  end
end

def make_symlink(src, dst)
  if File.symlink?(dst)
    return if File.readlink(dst) == src
    puts "Warning: #{dst} points to #{File.readlink(dst)}, expected #{src}; skipping"
  elsif File.exist?(dst)
    puts "Warning: #{dst} exists and is not a symlink; skipping"
  else
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
  tree_symlinks.each do |src_root, dst_root|
    for f in tracked_tree_files(srcdir, src_root)
      dst = File.join(homedir, dst_root, f.delete_prefix("#{src_root}/"))
      try_delete_file(dst) if File.symlink?(dst)
    end
  end
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

desc "symlink git-tracked files from tree dirs into their $HOME targets"
task :build_tree_symlinks do
  tree_symlinks.each do |src_root, dst_root|
    for f in tracked_tree_files(srcdir, src_root)
      src = File.join(srcdir, f)
      dst = File.join(homedir, dst_root, f.delete_prefix("#{src_root}/"))
      FileUtils.mkdir_p(File.dirname(dst))
      make_symlink src, dst
    end
  end
end

desc "build symlinks in home dir pointing to these files"
task :build_symlinks => [:build_normal_symlinks, :build_prezto_symlinks, :build_tree_symlinks]

desc "run build_symlinks"
task :default => [:build_symlinks]
