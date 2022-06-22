#!/usr/bin/env ruby
# frozen_string_literal: true

require 'open-uri'
require 'openssl'
require 'json'

module AwesomeCheatsheets

  URL_BASE = "https://raw.githubusercontent.com/detailyang/awesome-cheatsheet/master/README.md"
  OpenSSL::SSL::VERIFY_PEER ||= OpenSSL::SSL::VERIFY_NONE

  @@sheets = {}

  def self.parse_md(uri)
    section = nil
    indent = nil
    URI.open(uri) { |f|
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

  def self.output_json(out, infile = URL_BASE)
    parse_md infile
    File.open(out, 'w') { |f| f.write(@@sheets.to_json) }
  end

end

if __FILE__ == $PROGRAM_NAME
  AwesomeCheatsheets.output_json(
    ARGV[0] || "awesome.json",
    ARGV[1] || AwesomeCheatsheets::URL_BASE
  )
end
