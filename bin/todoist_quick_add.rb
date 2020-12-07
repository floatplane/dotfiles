#!/usr/bin/ruby
require 'erb'

def notify(str)
  puts str
  if ENV['alfred_version'].nil?
    cmd = "osascript -e 'display notification \"#{str}\"'"
    puts `#{cmd}`
  end
end

input = ARGV[0].strip
api_key = File.read(File.join(ENV['HOME'],'.todoist_api_key')).strip

# curl --silent "https://api.todoist.com/rest/v1/tasks" \
#     -X POST \
#     --data '{"content": "#{input}"}' \
#     -H "Content-Type: application/json" \
#     -H "Authorization: Bearer #{api_key}"

cmd = <<EOS
curl https://api.todoist.com/sync/v8/quick/add \
    -d token=#{api_key} \
    -d text='#{ERB::Util.url_encode input}'
EOS

`#{cmd}`

if $?.success?
  notify "Added '#{input}' to Todoist"
else
  notify "Failed to add to Todoist :("
end
