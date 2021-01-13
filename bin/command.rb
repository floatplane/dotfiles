module Command
  def self.run(cmd, silent: false)
    puts cmd unless silent
    result = `#{cmd}`
    unless $? == 0
      puts "Command failed with exit code #{$?}: `#{cmd}`"
      puts result
      exit $?.exitstatus
    end
    puts result unless silent
    result
  end
end
