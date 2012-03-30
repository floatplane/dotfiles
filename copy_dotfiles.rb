#!/usr/bin/ruby
#

def runCommand(cmd)
  puts cmd
  system(cmd)
end

srcdir = File.dirname(__FILE__)
basename = File.basename(__FILE__)
runCommand "rsync --recursive --exclude=\".git\" --exclude=\"#{basename}\" --progress --human-readable --archive \"#{srcdir}\" \"#{ENV['HOME']}\""
runCommand "find \"#{ENV['HOME']}/emacs\" -name \"*.el\" | xargs emacs -batch -f batch-byte-compile-if-not-done"
