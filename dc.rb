#!/usr/bin/env ruby

ENV['GEM_PATH']=Dir.pwd+'/gems'

$: << '.'

require 'rubygems'
require 'treetop'
require 'dc_nodes.rb'
require 'dc_parser.rb'

def clean(s)
  s.gsub!(/\t/,' ')    # tabs to spaces
  s.gsub!(/\s+$/,'')   # remove trailing whitespace
  s.gsub!(/^\s+&/,'&') # left-justify continuation lines
  s.gsub!(/&$\n&?/,'') # join continuation lines
  s.gsub!(/^\s+/,'')   # remove leading whitespace
  s.gsub!(/\n\n/,"\n") # remove empty lines
  s
end

def fail(msg)
  puts msg
  exit 1
end

def normalize(s)
  p=DowncaseParser.new
  clean(s)
  s=parse(p,s)
  clean(s)
end

def parse(p,s)
  tree=p.parse(s)
  fail "#{p.failure_reason}: #{p.failure_line}:#{p.failure_column}" if tree.nil?
  tree.to_s
end

infile=ARGV[0]
if infile
  unless File.readable?(infile)
    puts "Cannot read input file: #{infile}"
    exit 1
  end
  s=File.open(infile,'rb').read
else
  s=STDIN.read
end

puts normalize(s)
