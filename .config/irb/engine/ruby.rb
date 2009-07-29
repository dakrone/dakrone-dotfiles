module MyIRB

  engine_should_be "1.8.7"

  def get_source an_object
    begin
      case an_object
      when String then an_object
      when Module then Ruby2Ruby.translate an_object
      when Method then Ruby2Ruby.translate an_object.owner, object.name
      else raise NoMethodError
      end
    rescue NoMethodError
      raise ArgumentError, "Don't know how to get source of #{an_object}"
    end
  end

end