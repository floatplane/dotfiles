ENV['ALLOW_TEST_AUTOLOADING'] = "yes" if ENV["INSIDE_EMACS"]

if defined?(PryByebug)
  Pry.commands.alias_command 'c', 'continue'
  Pry.commands.alias_command 's', 'step'
  Pry.commands.alias_command 'n', 'next'
  Pry.commands.alias_command 'f', 'finish'
  Pry.commands.alias_command 'out', 'finish'
  Pry.commands.alias_command 'bt', 'backtrace'
end

# Hit Enter to repeat last command
Pry::Commands.command /^$/, "repeat last command" do
  _pry_.run_command Pry.history.to_a.last
end

Pry.config.collision_warning = true
