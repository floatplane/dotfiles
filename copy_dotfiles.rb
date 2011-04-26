#!/usr/bin/ruby
#

srcdir = File.dirname(__FILE__)
basename = File.basename(__FILE__)
cmd = "rsync --recursive --exclude=\".git\" --exclude=\"#{basename}\" --progress --human-readable --archive \"#{srcdir}\" \"#{ENV['HOME']}\""
puts cmd
system(cmd)
