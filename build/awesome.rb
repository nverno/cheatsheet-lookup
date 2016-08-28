require 'open-uri'
require 'openssl'
require 'json'

module AwesomeCheatsheets

  URL_BASE = "https://raw.githubusercontent.com/detailyang/awesome-cheatsheet/master/README.md"
  OpenSSL::SSL::VERIFY_PEER = OpenSSL::SSL::VERIFY_NONE

  @@sheets = {}

  def self.parse_md(uri)
    section = nil
    indent = nil
    open(uri) { |f|
      f.each_line { |line|
        line.chomp!
        if /^(\s*)-\s*([[:alnum:] ]+$)/.match(line)
          section = $2.downcase
        elsif /^(\s*)-\s*\[(.*?)\]\((http.*)\)/.match(line)
          if not $1.nil?
            indent = $1.length if indent.nil?
            name = $2
            uri = $3
            section = $2[/[^-_]+/].downcase unless $1.length > indent
          end
          if @@sheets[section].nil?
            @@sheets[section] = {}
            @@sheets[section][name] = uri
          else
            @@sheets[section][name] = uri
          end
          # p "Section #{section}: #{name}, #{uri}"
        end
      }
    }
  end

  def self.output_json(file)
    parse_md URL_BASE
    File.open(file, 'w') { |f| f.write(@@sheets.to_json) }
  end

end

AwesomeCheatsheets.output_json("awesome.json")
