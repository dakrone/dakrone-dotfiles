module MyIRB

  %w[ls cat ping wget bash].each do |cmd|
    define_method(cmd) { |*args| sh(cmd, *args) }
  end

  def sh *args
    system(
      args.inject("") do |cmd, arg|
        cmd << " " if cmd.length > 0
        str = arg.to_s
        if arg.is_a? Symbol
          cmd << "-" if str.length > 1
          cmd << "-"
        end
        cmd << str
      end
    )
  end

  @@editor_mutex = Mutex.new
  @@editor       = "vi"
  @@editor_stack = []

  def editor(format = nil, default = nil, editor = nil, do_format = true)
    raise RuntimeError, "Does not work with JRuby" if jruby?
    @@editor_mutex.synchronize do
      default, format = format, default unless default or format.is_a? Symbol
      format ||= editor_format_for default
      editor ||= @@editor
      file = "/tmp/vi_irb.#{format}"
      while File.exists? file
        i  ||= -1
        i   += 1
        file = "/tmp/vi_irb_#{i}.#{format}"
      end
      File.write(file, do_format ? editor_preload(format, default) : default)
      system "#{editor} #{file}"
      if File.exists? file
        @@editor_stack << [format, File.read(file), editor, false]
        result = editor_eval
        FileUtils.rm file
        result
      end
    end
  end

  def editor_preload format, object
    case format
    when :yaml then object.to_yaml
    when :rb   then object ? get_source(object) : ""
    end
  end

  def editor_eval
    format, source = @@editor_stack.last
    case format
    when :yaml then YAML.load source
    when :rb   then Object.class_eval source
    else source
    end
  end

  def editor_format_for a_value
    if not a_value or (defined? Ruby2Ruby and a_value.is_a? Module or a_value.is_a? Method)
      :rb
    else
      :yaml
    end
  end

  def get_edit name = nil
    @@edits        ||= {}
    @@editor_stack ||= {}
    raise ArgumentError, "No edits so far." if @@editor_stack.empty?
    if name
      raise ArgumentError, "No edit called #{name}." unless @@edits[name]
      @@edits[name]
    else
      @@editor_stack[-1]
    end
  end

  def get_editor_source name = nil
    get_edit(name)[1]
  end

  def last_edit name = nil
    result = editor(*get_edit(name))
    name_edit name if name
    result
  end

  def edit_to_file name, file = nil
    name, file = nil, name unless file
    File.write file, get_editor_source(name)
  end

  def edit_to_gist name = nil
    Gist.post name
  end

  def edit_to_clipboad name = nil
    begin
      require 'win32/clipboard'
      Win32::Clipboard.set_data get_editor_source(name)
    rescue LoadError
      RUBY_PLATFORM =~ /darwin/ ? cmd = "pbcopy" : cmd = "xclip"
      IO.popen(cmd, "w") { |c| c.write get_editor_source(name) }
    end
  end

  def name_edit name
    @@edits ||= {}
    @@edits[name] = @@editor_stack.last
    name
  end

  ["vi", "vim", "gvim -f", "evim -f", "ex", "joe", "kate", "nano", "e3vi"].each do |cmd|
    eval %[
      def #{cmd[/^[^ ]*/]} format = nil, default = nil
        editor format, default, "#{cmd} 2>/dev/null"
      end
    ]
  end
  
  def gem_command *args
    if mri?
      ruby_binary.gsub(/ruby(.*)$/, 'gem\1')
    else
      cmd = ruby_binary.dup
      cmd << " -S" if jruby?
      cmd << " gem"
    end
  end

  method_pattern(/^gem_.*$/) do |name, *args|
    sh(gem_command, name.to_s[/[^_]*$/], *args)
  end

end

class Class
  def publicize_methods
    saved_private_instance_methods = self.private_instance_methods
    class_eval { public(*saved_private_instance_methods) }
    yield self
    class_eval { private(*saved_private_instance_methods) }
  end
end

class Object
  unless respond_to? :tap
    def tap
      yield(self)
    end
  end
end

class << ENV
  def to_yaml
    to_hash.to_yaml
  end
end

class File
  def self.write file, content
    open(file, "w") { |f| f.write content }
  end
end
