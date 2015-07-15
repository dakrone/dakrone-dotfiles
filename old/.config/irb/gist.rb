needs "WWW::Mechanize", "HighLine", "editor"

module MyIRB

  # FIXME:
  # - This only handles single file gists.
  # - This is not thread safe!
  class Gist

    attr_reader :url

    COOKIE_FILE = File.join ENV["HOME"], ".irb_cookies"

    def self.load_cookies
      agent.cookie_jar.load COOKIE_FILE if File.exists? COOKIE_FILE
    end

    def self.save_cookies
      puts "save cookies in #{COOKIE_FILE}"
      agent.cookie_jar.save_as COOKIE_FILE
    end

    def self.hl
      HighLine.new
    end

    def self.agent
      @agent ||= WWW::Mechanize.new
    end

    def self.post name = nil
      @gists       ||= {}
      edit           = get_edit name
      @gists[name] ||= Gist.new if name
      gist           = @gists[name] || Gist.new
      gist.text      = edit[1]
      gist.format    = ".rb" if edit[0] == :rb
      gist.commit
    end

    def self.login
      name = hl.ask "Login or mail: "
      pwd  = hl.ask("Password (will not be displayed): ") { |q| q.echo = "*" }
      form = agent.get("https://gist.github.com/login?return_to=gist").forms.first
      form.field("login").value    = name
      form.field("password").value = pwd
      result = form.submit
      if result.uri.to_s =~ /session$/
        puts in_red("login failed")
        false
      else
        save_cookies
        true
      end
    end

    def agent
      self.class.agent
    end

    def load_cookies
      self.class.load_cookies
    end

    def load_cookies
      self.class.load_cookies
    end

    def initialize src = nil
      load_cookies
      @form = agent.get("http://gist.github.com").forms.first
      self.text = src if src
    end

    def get_field(name)
      @form.fields.detect { |f| f.name =~ /^#{name}/ }
    end

    def text= src
      get_field("file_contents").value = src
    end

    def text
      get_field("file_contents").value
    end

    def format
      get_field("file_ext").value
    end

    def format= ext
      get_field("file_ext").value = ext
    end

    def commit
      page  = @form.submit
      @url  = page.uri.to_s
      @form = agent.get("http://gist.github.com/gists/#{gist_id}/edit").forms.first
      url
    end

    def has_url?
      not @url.nil?
    end

    def gist_id
      return unless has_url?
      @gist_id ||= url[/\d+$/].to_i
    end

    def inspect
      "#<#{self.class.name}:#{self.object_id} @gist_id=#{gist_id.inspect}>"
    end

  end
  
end