
srcdir = File.dirname(__FILE__)
homedir = ENV['HOME']

symlinked_files = FileList.new('.*', 'emacs', 'bin').exclude(/^\.git$/, /^\.gitmodules$/, /^\.$/, /^\.\.$/)
compiled_elisp = FileList.new('emacs/**/*.el').sub!(/.el$/, '.elc')

def try_delete_file(f)
  if File.exists? f
    puts "Deleting #{f}"
    File.delete(f)
  end
end

desc "clean up everything"
task :clean do |t|
  for f in symlinked_files
    try_delete_file File.join(homedir, f)
  end
  for f in compiled_elisp
    try_delete_file File.join(srcdir, f)
  end
end

desc "build symlinks in home dir pointing to these files"
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

# rule '.elc' => '.el' do |t|
#   sh "emacs -q -batch -f batch-byte-compile #{t.source}"
# end

desc "byte compile all elisp."
task :byte_compile_elisp do |t|
  sh "emacs -q -batch -f batch-byte-compile-if-not-done #{FileList.new('emacs/**/*.el').to_a.join(' ')}"
end

desc "run build_symlinks and byte_compile_elisp"
task :default => [:build_symlinks, :byte_compile_elisp]
