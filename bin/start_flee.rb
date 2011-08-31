#!/usr/bin/env ruby

def execute cmd
  puts cmd
  `#{cmd}`
end

# Set up link to mysql socket file
GOOGLE3 = "#{ENV["HOME"]}/clients/picnik/google3"

# Start up all the needed services

services = [{ :windowTitle => 'Closure test server',
              :commandLine => './flee.sh build serve && read -p \"Press any key...\" -n 1 && echo',
              :workingDirectory => "#{GOOGLE3}/javascript/apps/picnik/flee"
              },

            { :windowTitle => 'Java frontend',
              :commandLine => './flee.sh build serve && read -p \"Press any key...\" -n 1 && echo',
              :workingDirectory => "#{GOOGLE3}/java/com/google/apps/picnik/flee"
              },

            { :windowTitle => 'Render server',
              :commandLine => './flee.sh build runserver && read -p \"Press any key...\" -n 1 && echo',
              :workingDirectory => "#{GOOGLE3}/apps/picnik/flee"
              },

           ];

# require 'rubygems'
# require 'ruby-debug'
# Debugger.start
# debugger

option_string = ""

services.each do |x|
  option_string += "--tab-with-profile=Default --working-directory=#{x[:workingDirectory]} -t \"#{x[:windowTitle]}\" -e 'bash -i -c \"#{x[:commandLine]}\"' "
end

fork {
  cmd = "gnome-terminal #{option_string}"
  puts cmd
  exec cmd
}
