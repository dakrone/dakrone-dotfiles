module MyIRB
  
  UNABLE_TO_LOAD = []
  
  # Original
  #libs = %w[
    #irb/completion irb/ext/save-history thread
    #yaml English fileutils date open-uri pp monitor
    #rubygems map_by_method what_methods
    #english/array english/inflect english/string
    #english/style english/style_orm ruby2ruby
    #hpricot stringio mechanize stored_hash
    #duration highline ]

  libs = %w[
    irb/completion irb/ext/save-history thread yaml fileutils date open-uri pp
    monitor rubygems map_by_method what_methods english/inflect ruby2ruby
    hpricot stringio mechanize looksee duration highline wirble hirb]

  def catch_stdout
    verbose  = $VERBOSE
    $VERBOSE = nil
    stdout   = STDOUT
    io       = StringIO.new
    eval "::STDOUT = $stdout = io"
    yield
    eval "::STDOUT = $stdout = stdout"
    $VERBOSE = verbose
    io.rewind
    io.read
  end

  module_function :catch_stdout

  print "Loading Libraries: \033[1m[\033[0m"
  print(" " * libs.size)
  print "\033[1m]\033[0m "

  while lib = libs.shift
    info = "  (loading #{lib})"
    print "\033[0;37m" + info
    print "\033[#{3 + info.length + libs.size}D"
    begin
      require lib
      print "\033[1;32m|"
    rescue LoadError
      UNABLE_TO_LOAD << lib
      print "\033[1;31m|"
    end
   # print "\033[1;33m|"
    print "\033[#{2 + libs.size}C"
    print(" " * info.length)
    print "\033[#{info.length}D"
  end

  print "\033[0m"
  
  unless UNABLE_TO_LOAD.empty?
    print "Unable to load #{UNABLE_TO_LOAD.size} libraries. See UNABLE_TO_LOAD."
  end

  puts

  if defined? Duration
    IRB_START_TIME = Time.now
    at_exit do
      puts "\nIRB session duration: #{Duration.new(Time.now - IRB_START_TIME)}"
    end
  end

  include FileUtils::Verbose

  def self.prompt
    @normal_prompt ||= {
      :AUTO_INDENT => true,
      :PROMPT_I    => in_purple(impl) + in_lgray(" >> "),
      :PROMPT_S    => in_lred(impl) + in_lgray(" %l> "),
      :PROMPT_C    => in_lred(impl) + in_lgray(" ?> "),
      :PROMPT_N    => in_lred(impl) + in_lgray(" ?> "),
      :RETURN      => in_lpurple(impl) + in_lgray(" => ") + "%s\n"
      #:RETURN      => in_lgray("=> ") + "%s\n"
    }
  end

  def self.start
    IRB.conf[:PROMPT][:MY_PROMPT] = prompt
    IRB.conf.merge!(
      :PROMPT_MODE  => :MY_PROMPT,
      :SAVE_HISTORY => 1000,
      :HISTORY_FILE => "#{ENV['HOME']}/.irb_history_#{RUBY_ENGINE}",
      :AUTO_INDENT  => true
    )
    IRB.conf[:IRB_RC] = Proc.new { rc_procs.each { |proc| proc.call } }
  end

  def self.rc_procs
    @rc_procs ||= []
  end

  def self.when_started &block
    rc_procs << block
  end

  def method_pattern pattern, &block
    @@method_patterns ||= {}
    @@method_patterns[pattern] = block
  end

  def method_missing n, *p, &b
    @@method_patterns ||= {}
    pattern = @@method_patterns.keys.detect { |r| n.to_s =~ r }
    return @@method_patterns[pattern].call(n, *p, &b) if pattern
    super(n, *p, &b)
  end

  module_function :method_pattern

  def needs(*objects)
    object = objects.shift
    if object
      raise LoadError, "needs #{object}" unless eval("defined? #{object}")
      needs(*objects)
    end
  end

  module_function :needs

end

include MyIRB
