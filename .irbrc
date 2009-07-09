require 'rubygems'
require 'wirble'
require 'pp'
require 'sketches'
require 'looksee/shortcuts'

Wirble.init
Wirble.colorize

# Use gvim as our sketches editor
Sketches.config :editor => 'gvim'

# Easily print methods local to an object's class
class Object
  def local_methods
    (methods - Object.instance_methods).sort
  end
end

# Log to STDOUT if in Rails
if ENV.include?('RAILS_ENV') && !Object.const_defined?('RAILS_DEFAULT_LOGGER')
  require 'logger'
  RAILS_DEFAULT_LOGGER = Logger.new(STDOUT)
end

