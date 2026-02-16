module Command
  def self.run(cmd, silent: false, fatal: true)
    puts cmd unless silent
    result = `#{cmd}`
    unless $? == 0
      puts "Command failed with exit code #{$?}: `#{cmd}`"
      puts result
      exit $?.exitstatus if fatal
      return nil
    end
    puts result unless silent
    result
  end
end
