module MyIRB

  raise LoadError, "This needs to be JRuby" unless jruby?
  engine_should_be "1.0"

end