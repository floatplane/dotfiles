#!/usr/bin/ruby

while 1 do
  out = `curl https://www.mtbagel.com/may-12 2> /dev/null`
  if out.include? '<title>Mt. Bagel &mdash; Secure'
    for i in 0..59 do
      print "Locked, waiting #{60 - i} seconds...\r"
      sleep 1
    end
    puts "Locked, waiting 1 seconds...done."
  else
    puts "\a"
    `osascript -e 'display notification "Bagel time"'`
    break
  end
end
