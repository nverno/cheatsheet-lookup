require 'nokogiri'
require 'open-uri'
require 'openssl'

module AwesomeCheatsheets

  URL_BASE = "https://raw.githubusercontent.com/detailyang/awesome-cheatsheet/master/README.md"
  OpenSSL::SSL::VERIFY_PEER = OpenSSL::SSL::VERIFY_NONE

  sections = {}

  def self.parse_md(uri)
    current = nil
    indent = 0
    open(uri) { |f|
      f.each_line { |line|
        /^(\s*)?-\s*\[(.*)\]\((.*)\)/.match(line.chomp)
        current = $2[/[^-]+/] unless $1.nil? || $1.length > indent
        indent = $1.length unless $1.nil? || $1.length > indent
        p "Section #{current}: #{$2}, #{$3}" unless $2.nil?
      }
    }
  end

  def self.print_lines()
    parse_md URL_BASE
  end
end

AwesomeCheatsheets.print_lines
