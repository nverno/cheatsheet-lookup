#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'

def get_sources(file = 'sources.json')
  JSON.load(File.read(file)).each do |out, opts|
    prog = opts["indexer"]
    `./#{prog["program"]} "#{out}" "#{opts["source"]}"`
  end
end

if __FILE__ == $PROGRAM_NAME
  get_sources
end
