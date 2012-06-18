#!/usr/bin/env ruby

require 'fileutils'

def compile(f)
  fc="gfortran -pedantic-errors -std=f95"
  b=stem(f)
  cmd="#{fc} -o #{b} #{f}"
  stat,out=docmd(cmd)
  fail(out,cmd) unless stat
  b
end

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

def stem(f)
  f.chomp(File.extname(f))
end

def tests
  Dir.glob("tests/*").sort
end

def translate(test)
  base=File.basename(test)
  src=Dir.glob(["#{test}/#{base}.f","#{test}/#{base}.f90"])
  fail "Remove either #{src[0]} or #{src[1]}" if src.size > 1
  src=src.first
  dst="#{stem(src)}_ppp#{File.extname(src)}"
  cmd="./ppp #{src} > #{dst}"
  stat,out=docmd(cmd)
  fail(out,cmd) unless stat
  dst
end

exe('make')
load 'ppp'
tests.each do |test|
  print '.'
  translation=translate(test)
  bin=compile(translation)
  stdout=exe(bin)
  control=File.open(File.join(test,'control'),"rb").read
  fail unless stdout==control
  FileUtils.rm_f([translation,bin])
end
puts "\nOK"
