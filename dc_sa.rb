#!/usr/bin/env ruby

$: << '.'

ENV['GEM_PATH']=Dir.pwd+'/gems'

require 'rubygems'
require 'treetop'
require 'dc_nodes.rb'
require 'dc_grammar.tt'

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

p=DowncaseParser.new
tree=p.parse(s)

if tree.nil?
  puts "#{p.failure_reason}: #{p.failure_line}:#{p.failure_column}"
else
  puts "#{tree}"
end
