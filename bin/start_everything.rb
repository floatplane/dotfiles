#!/usr/bin/env ruby

def execute cmd
  puts cmd
  `#{cmd}`
end

# Set up link to mysql socket file
unless File.exist?('/tmp/mysql.sock')
  puts "Make link to mysql.sock..."
  execute "sudo ln -s /var/run/mysqld/mysqld.sock /tmp/mysql.sock"
  puts "done."
end

# Create our pid directory if need be
unless File.directory?('/var/run/jambool') 
  puts "Create pid directory..."
  execute "sudo mkdir -p /var/run/jambool && sudo chmod a+rwX /var/run/jambool"
  puts "done."
end

unless `ps ax | grep haproxy`.match("[0-9]+ haproxy") # regex will find "haproxy" but not "grep haproxy" in result from ps
  puts "Starting haproxy..."
  Dir.chdir("#{ENV["HOME"]}/src/svn/ops/trunk/dev_tools")
  execute "sudo ./setup_haproxy.rb"
  puts "done."
end

unless `ps x | grep memcached`.match("[0-9]+ memcached")
  puts "Starting memcached..."
  execute "memcached -d"
  puts "done."
end

unless `ps x | grep beanstalk`.match("[0-9]+ beanstalkd")
  puts "Starting beanstalkd..."
  execute "beanstalkd -d"
  puts "done."
end

# Start up all the needed services

services = [{ :windowTitle => 'Services',
              :commandLine => 'ruby -I. ./service_monitoring/services_monitor.rb development',
              :workingDirectory => "#{ENV["HOME"]}/src/jambool/trunk"
              },

            { :windowTitle => 'Analytics/CC/Inventory',
              :commandLine => 'rake service:stop service:start && read -p \"Press any key...\" -n 1 && echo',
              :workingDirectory => "#{ENV["HOME"]}/src/jambool/trunk"
              },

            { :windowTitle => 'Jambool website',
              :commandLine => 'script/server webrick development -p 3001',
              :workingDirectory => "#{ENV["HOME"]}/src/jambool/trunk/website/jambool"
              },

            { :windowTitle => 'API website',
              :commandLine => 'script/server webrick development -p 3002 -u',
              :workingDirectory => "#{ENV["HOME"]}/src/api"
              },

            { :windowTitle => 'Socialgold website',
              :commandLine => 'script/server webrick development -p 3000 -u',
              :workingDirectory => "#{ENV["HOME"]}/src/socialgold/trunk"
              }
           ];

count = services.length
cols = 6
row = 0
col = 0
termSize = "100x30"
termXOffset = 80
termYOffset = 80

# require 'rubygems'
# require 'ruby-debug'
# Debugger.start
# debugger

# services.each do |x|
#   fork {
#     Dir.chdir(x[:workingDirectory]);
#     cmd = "xterm -T \"#{x[:windowTitle]}\" -geometry #{termSize}+#{termXOffset * count}+#{termYOffset * count} -sb -e 'bash -i -c \"#{x[:commandLine]}\"'"
#     puts cmd
#     exec cmd
#   }
#   count -= 1
#   col += 1;
#   if (col == cols) then
#     col = 0;
#     row += 1;
#   end
# end

option_string = ""

services.each do |x|
  option_string += "--tab-with-profile=Default --working-directory=#{x[:workingDirectory]} -t \"#{x[:windowTitle]}\" -e 'bash -i -c \"#{x[:commandLine]}\"' "
end

fork {
  cmd = "gnome-terminal #{option_string}"
  puts cmd
  exec cmd
}
