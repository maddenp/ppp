#!/usr/bin/env ruby

ENV['GEM_PATH']=Dir.pwd+'/gems'

$: << '.'

require 'rubygems'
require 'treetop'
require 'normalize_nodes.rb'
require 'normalize_parser.rb'

def clean(s)
  s.gsub!(/\s*!.*$/,'') # remove comment lines
  s.gsub!(/\s+$/,'')    # remove trailing whitespace
  s.gsub!(/^\s+/,'')    # left-justify line
  s.gsub!(/&$\n&?/,'')  # join continuation lines
  s.gsub!(/\n\n/,"\n")  # remove blank lines
  s
end

def fail(msg)
  puts "\n#{msg}\n\n"
  exit 1
end

def assemble(s,seen)
  a=''
  msg="While processing includes, "
  r=Regexp.new('\s*include\s*(\'[^\']+\'|\"[^\"]+\").*',true)
  s.split("\n").each do |line|
    m=r.match(line)
    if m
      incfile=m[1][1..-2]
      if seen.include?(incfile)
        msg+="file #{seen.last} included #{incfile} recursively "
        msg+=incchain(seen,incfile)
        fail(msg)
      end
      unless File.readable?(incfile)
        msg+="could not read file #{incfile} "
        msg+=incchain(seen,incfile)
        fail(msg)
      end
      a+=assemble(File.open(incfile,'rb').read,seen+[incfile])
    else
      a+="#{line}\n"
    end
  end
  a
end

def incchain(seen,incfile)
  '( '+(seen+[incfile]).join(' < ')+' )'
end

def normalize(s)
  tree=NormalizeParser.new.parse(clean(s))
  fail "#{p.failure_reason}: #{p.failure_line}:#{p.failure_column}" if tree.nil?
  clean(tree.to_s)
end

def usage
  "usage: #{File.basename(__FILE__)} source"
end

srcfile=ARGV[0]
fail(usage) unless srcfile
fail("Cannot read file: #{srcfile}") unless File.readable?(srcfile)
s=File.open(srcfile,'rb').read

puts normalize(assemble(s,[srcfile]))
