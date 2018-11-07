#! /usr/bin/env ruby

last_cmd = ARGV[0]
last_cmd_words = last_cmd.split
ARGV.shift
aliases = ARGF.readlines
best_match = ''
aliases.each do |a|
  # Split the alias from its definition
  alias_, aliased_cmd = a.strip.split '='
  # Strip leading and trailing quotes from definition
  aliased_cmd = aliased_cmd.sub(/^'/, '').sub(/'$/, '')
  match = true
  # Walk through alias defnition and command word by word,
  # seeing if we have an incremental match
  aliased_cmd.split.zip(last_cmd_words).each do |pair|
    match &&= pair[0] == pair[1]
  end
  # Keep the alias that removes the most from the original command line
  if (match && alias_.length < aliased_cmd.length && aliased_cmd.length > best_match.length)
    best_match = alias_
  end
end

puts best_match if best_match.length > 0


# Needs more work:
#
# if we detect that the first word is git, we could also look for common aliases? There could be wins there.
#
# How to get more clever (example: if I type `g push`, that's good but not as good as `gp`)
# Track what was typed, and what it expands to
# Go through the match sequence on the expanded text, and see if we can find something shorter than what was typed
