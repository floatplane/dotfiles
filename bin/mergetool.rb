#!/usr/bin/env ruby

require 'optparse'
require 'pp'

# require 'rubygems'
# require 'ruby-debug'
# Debugger.start
# debugger

base = ARGV[0]
yours = ARGV[1]
theirs = ARGV[2]
merged = ARGV[3]

# ARGV.each { |x| puts x }

# TODO: allow configuration of alternate diff/merge tools, could do this:
# [mergetool "mergetool.rb"]
#	alternatediff = opendiff
# and get that config setting via
# `git config mergetool.`git config merge.tool`.alternatediff`
# note the nested backticks

raise "Error" unless File.exist?(yours) && File.exist?(theirs) && File.exist?(merged)

# Get terminal into immediate (unbuffered) mode
term_state = `stty -g`.chomp

begin
  `stty raw -echo`

  selection = ' '

  while !['y','t','m','d','T','Y','a','q'].include? selection do
    print "Enter one of the following: (y)ours/(t)heirs/(m)erge/(d)iff/diff(T)heirs/diff(Y)ours/(a)ccept/(q)uit: "
    selection = case STDIN.getc.chr
                when 'y'
                  `cp "#{yours}" "#{merged}"`
                  exit #{$?.exitstatus}
                when "t" 
                  `cp "#{theirs}" "#{merged}"`
                  exit #{$?.exitstatus}
                when "d" 
                  ancestor_arguments = "-ancestor \"#{base}\"" if File.exist?(base)
                  print "\r\nLaunching diff..."
                  `opendiff "#{yours}" "#{theirs}" #{ancestor_arguments}`
                  print "\r\n"
                when "m" 
                  ancestor_arguments = "-ancestor \"#{base}\"" if File.exist?(base)
                  print "\r\nLaunching merge..."
                  `opendiff "#{yours}" "#{theirs}" #{ancestor_arguments} -merge "#{merged}"`
                  print "\r\nopendiff merge returned #{$?.exitstatus}\r\n"
                when "T"
                  print "\r\nDiff theirs (#{theirs}) against base (#{base})..."
                  `opendiff "#{base}" "#{theirs}"`
                  print "\r\n"
                when "Y"
                  print "\r\nDiff yours (#{yours}) against base (#{base})..."
                  `opendiff "#{base}" "#{yours}"`
                  print "\r\n"
                when 'a'
                  exit 0
                when "q" 
                  exit -1
                else
                  print "\r\n"
                end
  end
ensure
  print "\r\n"
  `stty "#{term_state}"`
end
