#!/usr/bin/env ruby

require 'fileutils'

def docmd(cmd)
  out=IO.popen(cmd+' 2>&1') { |x| x.readlines.reduce('') { |s,e| s+=e } }
  [($?.exitstatus==0)?(true):(false),out]
end

def exe(bin)
  cmd=bin
  stat,out=docmd(cmd)
  fail(out,cmd) unless stat
  out
end

def fail(msg=nil,cmd=nil)
  puts
  puts "Command: #{cmd}" if cmd
  puts msg if msg
  puts "FAIL"
  exit(1)
end

tdir='tests'
tests=(ARGV[0])?(["#{tdir}/#{ARGV[0]}"]):(Dir.glob("#{tdir}/t*").sort)
exe('make')
load 'ppp'
tests.each do |test|
  print '.'
  exe("make -C #{test} clean")
  exe("make -C #{test} bin")
  stdout=exe("cd #{test} && ./a.out")
  control=File.open(File.join(test,'control'),"rb").read
  unless stdout==control
    msg="#{test} output expected:\n--begin--\n"
    msg+=control
    msg+="-- end --\n#{test} output actual:\n--begin--\n"
    msg+=stdout
    msg+="-- end --"
    fail msg
  end
  exe("make -C #{test} clean")
end
puts "\nOK (#{tests.size} tests)"

# paul.a.madden@noaa.gov
