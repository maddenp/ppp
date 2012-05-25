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

def assemble(s,incdirs,seen)
  current=seen.last
  a=''
  r=Regexp.new('\s*include\s*(\'[^\']+\'|\"[^\"]+\").*',true)
  s.split("\n").each do |line|
    m=r.match(line)
    if m
      incfile=m[1][1..-2]
      if incfile[0]=='/' or incfile[0]=='.'
        incfile=File.expand_path(File.join(File.dirname(current),incfile))
        unless File.exist?(incfile)
          fail "Could not find included file #{incfile}"
        end
      else
        found=false
        incdirs.each do |d|
          maybe=File.expand_path(File.join(d,incfile))
          if File.exist?(maybe)
            found=true
            incfile=maybe
            break
          end
        end
        unless found
          fail "Could not find included file #{incfile} on search path"
        end
      end
      if seen.include?(incfile)
        msg="File #{current} includes #{incfile} recursively:\n"
        msg+=incchain(seen,incfile)
        fail(msg)
      end
      unless File.readable?(incfile)
        msg="Could not read file #{incfile} "
        msg+=incchain(seen,incfile)
        fail(msg)
      end
      a+=assemble(File.open(incfile,'rb').read,incdirs,seen+[incfile])
    else
      a+="#{line}\n"
    end
  end
  a
end

def incchain(seen,incfile)
  "\n  "+(seen+[incfile]).join(" includes\n  ")
end

def normalize(s)
  tree=NormalizeParser.new.parse(clean(s))
  fail "#{p.failure_reason}: #{p.failure_line}:#{p.failure_column}" if tree.nil?
  clean(tree.to_s)
end

def usage
  f=File.basename(__FILE__)
  "usage: #{f} [-I dir[:dir:...]] source"
end

srcfile=ARGV.pop
fail(usage) unless srcfile
srcfile=File.expand_path(srcfile)
fail("Cannot read file: #{srcfile}") unless File.readable?(srcfile)

incdirs=['.']
ARGV.reverse!
while opt=ARGV.pop
  case opt
  when '-I'
    dirlist=ARGV.pop
    fail(usage) unless dirlist
    dirlist.split(':').each do |d|
      fail("No such directory: #{d}") unless File.directory?(d)
      incdirs << d
    end
  else
    fail(usage)
  end
end

s=File.open(srcfile,'rb').read
s=assemble(s,incdirs,[srcfile])
s=normalize(s)

puts s
