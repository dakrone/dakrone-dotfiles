%w[myirb engine color rails commands gist].each do |lib|
  begin
    require File.join(ENV["HOME"], ".config", "irb", lib)
  rescue LoadError
    puts "\033[0;33mCould not load feature \033[1;33m'#{lib}'\033[0;33m: #{$!.message}.\033[0m"
  end
end

MyIRB.start
