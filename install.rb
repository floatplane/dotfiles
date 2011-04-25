#!/usr/bin/ruby
#

src = File.dirname(__FILE__)
cmd = "rsync --recursive --exclude=\".git\" --progress --human-readable --archive \"#{src}\" \"#{ENV['HOME']}\""
puts cmd
`#{cmd}`
