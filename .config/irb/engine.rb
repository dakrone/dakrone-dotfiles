module MyIRB

  # Place a general implementation of implementation specific methods in here
  # and overwrite it in engine/*.rb

  include Rubinius if defined? Rubinius

  unless defined? RUBY_ENGINE
    if defined? JRUBY_VERSION
      ::RUBY_ENGINE = "jruby"
    elsif defined? Rubinius
      ::RUBY_ENGINE = "rbx"
    elsif
      ::RUBY_ENGINE = "ruby"
    end
  end
  unless RUBY_ENGINE.frozen?
    RUBY_ENGINE.replace "rbx" if RUBY_ENGINE == "rubinius"
    RUBY_ENGINE.downcase!
    RUBY_ENGINE.freeze
  end

  ::RUBY_ENGINE_VERSION = const_get("#{RUBY_ENGINE.upcase}_VERSION")

  unless defined? RUBY_DESCRIPTION
    ::RUBY_DESCRIPTION = "#{RUBY_ENGINE} #{RUBY_ENGINE_VERSION} "
    unless RUBY_ENGINE == "ruby"
      ::RUBY_DESCRIPTION << "(ruby #{RUBY_VERSION}"
      ::RUBY_DESCRIPTION << " patchlevel #{RUBY_PATCHLEVEL}" if defined? RUBY_PATCHLEVEL
      ::RUBY_DESCRIPTION << ") "
    end
    if defined? RUBY_RELEASE_DATE
      ::RUBY_DESCRIPTION << "("
      ::RUBY_DESCRIPTION << BUILDREV[0..8] << " " if defined? BUILDREV
      ::RUBY_DESCRIPTION << RUBY_RELEASE_DATE << ") "
    end
    ::RUBY_DESCRIPTION << "[#{RUBY_PLATFORM}]"
  end

  puts RUBY_DESCRIPTION

  # This should be changed in other setups / operation systems.
  def ruby_binary
    @@ruby_binary ||= [ "/usr/bin/#{RUBY_ENGINE}#{RUBY_ENGINE_VERSION}",
      "/usr/bin/#{RUBY_ENGINE}#{RUBY_ENGINE_VERSION[/^\d+\.\d+/]}",
      "/usr/bin/#{RUBY_ENGINE}" ].detect do |bin|
      bin.freeze
      File.exists? bin
    end
  end
  
  def jruby?;    RUBY_ENGINE == "jruby";    end
  def mri?;      RUBY_ENGINE == "ruby";     end
  def rbx?;      RUBY_ENGINE == "rbx";      end
  def ironruby?; RUBY_ENGINE == "ironruby"; end
  def macruby?;  RUBY_ENGINE == "macruby";  end

  alias rubinius? rbx?

  def ruby_engine pretty = true
    return RUBY_ENGINE unless pretty
    case RUBY_ENGINE
    when "ruby"  then "CRuby"
    when "rbx"   then "Rubinius"
    when /ruby$/ then RUBY_ENGINE.capitalize.gsub("ruby", "Ruby")
    end
  end

  module_function :ruby_engine

  # this is used for the prompt. should go there.
  def self.impl
    return @impl if @impl
    #@impl = ruby_engine << " " << RUBY_ENGINE_VERSION
    @impl = RUBY_ENGINE_VERSION
  end

  def get_source an_object
    raise ArgumentError, "Don't know how to get source of #{an_object}" unless an_object.is_a?String
    an_object
  end

  def self.engine_should_be version
    unless version
      $stderr.puts "I dont't think this works properly in #{ruby_engine}."
      return
    end
    splited = version.split "."
    update  = false
    RUBY_ENGINE_VERSION.split(".").each_with_index do |v,i|
      case v.to_i <=> splited[i].to_i
      when -1
        update = true
        break
      when 1
        break
      end
    end
    $stderr.puts "\033[1;31mYou should update #{ruby_engine} to at least #{version}.\033[0m" if update
  end

  begin
    require File.join(File.dirname(__FILE__), "engine", RUBY_ENGINE)
  rescue LoadError
  end
  
end
