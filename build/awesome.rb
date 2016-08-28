require 'open-uri'

module AwesomeCheatsheets

  def self.get_url(uri)
    page = URI.encode(uri)
  end
end
