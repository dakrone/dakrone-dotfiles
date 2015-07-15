module MyIRB

  @@color_inspect = false

  FG_COLORS = { :black      => "\033[0;30m", :gray      => "\033[1;30m",
                :lgray      => "\033[0;37m", :white     => "\033[1;37m",
                :red        => "\033[0;31m", :lred      => "\033[1;31m",
                :green      => "\033[0;32m", :lgreen    => "\033[1;32m",
                :brown      => "\033[0;33m", :yellow    => "\033[1;33m",
                :blue       => "\033[0;34m", :lblue     => "\033[1;34m",
                :purple     => "\033[0;35m", :lpurple   => "\033[1;35m",
                :cyan       => "\033[0;36m", :lcyan     => "\033[1;36m"   }
  BG_COLORS = { :black      => "\033[40m",   :red       => "\033[41m",
                :green      => "\033[42m",   :yellow    => "\033[43m",
                :blue       => "\033[44m",   :purple    => "\033[45m",
                :cyan       => "\033[46m",   :gray      => "\033[47m"     }
  ANSI_MISC = { :reset      => "\033[0m",    :bold      => "\033[1m",
                :underscore => "\033[4m",    :blink     => "\033[5m",
                :reverse    => "\033[7m",    :concealed => "\033[8m"      }

  def underlined *params
    return ANSI_MISC[:underscore] + params.join("\n") + ANSI_MISC[:reset]
  end

  FG_COLORS.each do |color, ansi|
    eval %{
      def in_#{color} *params
        FG_COLORS[:#{color}] + params.join("\n") + ANSI_MISC[:reset]
      end
    }
    module_function "in_#{color}"
  end

  def direct_output(a_string)
    a_string = a_string.dup
    class << a_string
      alias inspect to_s
    end
    a_string
  end

  module_function :direct_output

  def show_regexp(a, re)
    if (a =~ re)
      direct_output(
        in_green('"' + $`) + in_lgreen(underlined($&)) + in_green($' + '"')
      )
    else
      "no match"
    end
  end

  module_function :show_regexp

  def color_inspect?
    @@color_inspect
  end

  def in_color
    (@@color_monitor ||= Monitor.new).synchronize do
      @@color_inspect = true
      result = yield
      @@color_inspect = false
      result
    end
  end

  def inspect_in color, output
    return send("in_#{color}", output) if color_inspect?
    output
  end

  def color_inspect &block
    return if @color_inspect
    if self.is_a? Class
      @color_inspect = block
      self.class_eval do
        alias nocolor_inspect inspect
        def inspect
          block = self.class.instance_variable_get("@color_inspect")
          # TODO: why doens't this work with Rails?
          if color_inspect? and block
          #if color_inspect? and block and !rails?
            block.call(self)
          else
            nocolor_inspect
          end
        end
      end
    else
      a_class = class << self; self; end
      a_class.color_inspect(&block)
    end
  end

end

module Enumerable

  MAX_INSPECT  = 15
  SHOW_INSPECT = 10

  def short_inspect?
    @short_inspect = true if @short_inspect == nil
    @short_inspect
  end

  def show_all
    old_value      = @short_inspect
    @short_inspect = false
    output = direct_output(in_color { inspect })
    @short_inspect = old_value
    output
  end

  def show_all!
    @short_inspect = false
    self
  end

  def short_inspect!
    @short_inspect = true
    self
  end

  def smart_inspect(open, close, &block)
    block ||= proc { |e| e.inspect }
    if short_inspect? and length > MAX_INSPECT
      first_elements = ""
      each_with_index do |e, i|
        break unless i < SHOW_INSPECT
        first_elements << block.call(e) << in_lblue(", ")
      end
      in_lblue(open) +
        first_elements +
        in_lgray("... #{length - SHOW_INSPECT} elements") +
        in_lblue(close)
    else
      in_lblue(open) +
        collect(&block).join(in_lblue(", ")) +
        in_lblue(close)
    end
  end
  
end

# For some reasons String.color_inspect makes "foo".inspect retrun
# #<String:...> instead of "\"foo\"", same for Symbol.
if rubinius?
  class String
    alias orig_inspect inspect
    def inspect
      inspect_in :green, orig_inspect
    end
  end
  class Symbol
    alias orig_inspect inspect
    def inspect
      inspect_in :lgreen, orig_inspect
    end
  end
  [nil, false, true].each do |var|
    class << var
      alias orig_inspect inspect
      def inspect
        inspect_in :cyan, orig_inspect
      end
    end
  end
  %w[Fixnum Bignum Float].each do |c|
    eval %[
      class #{c}
        alias orig_inspect inspect
        def inspect
          inspect_in :purple, orig_inspect
        end
      end
    ]
  end
else
  String.color_inspect  { |o| in_green o.nocolor_inspect  }
  Symbol.color_inspect  { |o| in_lgreen o.nocolor_inspect }
  Fixnum.color_inspect  { |o| in_cyan o.nocolor_inspect }
  Bignum.color_inspect  { |o| in_cyan o.nocolor_inspect }
  Float.color_inspect   { |o| in_cyan o.nocolor_inspect }
  [NilClass, TrueClass, FalseClass].each do |a_class|
    a_class.color_inspect { |o| in_cyan o.nocolor_inspect }
  end
end

Array.color_inspect   { |o| o.smart_inspect "[", "]" }
Range.color_inspect   { |o| o.min.inspect + in_lblue("..") + o.max.inspect }
Tuple.color_inspect   { |o| o.smart_inspect "<< ", " >>" } if defined? Tuple

Hash.color_inspect do |o|
  o.smart_inspect "{", "}" do |element|
    element.collect { |e| e.inspect }.join in_lblue(" => ")
  end
end

Regexp.color_inspect do |o|
  out = ""
  escaped = false
  o.nocolor_inspect.each_char do |char|
    if escaped
      escaped = false
      out << in_gray(char)
    elsif char == '\\'
      escaped = true
      out << in_gray(char)
    elsif %w{* ? + [ ] ^ $ | .}.include? char
      out << in_white(ANSI_MISC[:bold] + char)
    elsif %w{/ ( )}.include? char
      out << in_yellow(ANSI_MISC[:bold] + char)
    else
      out << in_gray(char)
    end
  end
  out
end

class << ENV
  def inspect
    to_hash.inspect
  end
end

class PrettyPrint
  alias orig_text text
  def text obj, *whatever
    orig_text in_lblue(obj), *whatever
  end
end

class << PP
  alias orig_pp pp
  def pp *args
    in_color { orig_pp(*args) }
  end
end

class IRB::Irb
  alias orig_output_value output_value
  def output_value
    in_color { orig_output_value }
  end
end
