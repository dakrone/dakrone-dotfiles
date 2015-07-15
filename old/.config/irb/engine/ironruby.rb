module MyIRB

  raise LoadError, "This needs to be IronRuby" unless ironruby?
  engine_should_be nil

end
