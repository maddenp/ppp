#!/usr/bin/env ruby

ENV['GEM_PATH']=Dir.pwd+'/gems'

$: << '.'

require 'rubygems'
require 'treetop'
require 'normalize_nodes.rb'
require 'normalize_parser.rb'

def clean(s)
  s.gsub!(/\s*!.*$/,'') # remove comment-only lines
  s.gsub!(/\s+$/,'')    # remove trailing whitespace
  s.gsub!(/^\s+/,'')    # left-justify line
  s.gsub!(/&$\n&?/,'')  # join continuation lines
  s.gsub!(/\n\n/,"\n")  # remove empty lines
  s
end

def fail(msg)
  puts msg
  exit 1
end

def normalize(s)
  tree=NormalizeParser.new.parse(clean(s))
  fail "#{p.failure_reason}: #{p.failure_line}:#{p.failure_column}" if tree.nil?
  clean(tree.to_s)
end

source=ARGV[0]
if source
  fail "Cannot read input file: #{source}" unless File.readable?(source)
  s=File.open(source,'rb').read
else
  s=STDIN.read
end

puts normalize(s)
