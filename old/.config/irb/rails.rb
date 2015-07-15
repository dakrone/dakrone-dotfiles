# encoding: UTF-8
module MyIRB

  class << self
    alias normal_prompt prompt
    alias normal_start start
  end

  def self.prompt
    is_rails? ? rails_prompt : normal_prompt
  end

  def self.start
    normal_start
    rails_start if is_rails?
  end

  def self.rails_start
    when_started do
      ActiveRecord::Base.logger = Logger.new STDOUT
      ActiveRecord::Base.instance_eval { alias :[] :find }
    end
  end

  def is_rails?
    ENV.include? 'RAILS_ENV'
  end

  alias rails? is_rails?

  module_function :is_rails?

  def self.rails_prompt
    pre = in_purple(impl) + in_brown(" #{ENV['RAILS_ENV']}")
    @normal_prompt ||= {
      :AUTO_INDENT => true,
      :PROMPT_I    => pre + in_lgray(" >> "),
      :PROMPT_S    => pre + in_lgray(" %l> "),
      :PROMPT_C    => pre + in_lgray(" ?> "),
      :PROMPT_N    => pre + in_lgray(" ?> "),
      #:RETURN      => pre + in_cyan(" => ") + "%s\n"
      :RETURN      => in_lgray("=> ") + "%s\n"
    }
  end

end
