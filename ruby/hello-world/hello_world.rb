module HelloWorld
  def self.hello(name = nil)
    if name.nil?
      'Hello, World!'
    else
      'Hello, %s!' % name
    end
  end
end
