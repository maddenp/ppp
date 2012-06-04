#!/usr/bin/env ruby

load    'ppp'
require 'fileutils'

def compile(f)
  fc="gfortran -pedantic-errors -std=f95"
  b=stem(f)
  cmd="#{fc} -o #{b} #{f}"
  stat,out=docmd(cmd)
  fail(cmd,out) unless stat
  b
end

def docmd(cmd)
  out=IO.popen(cmd+' 2>&1') { |x| x.readlines.reduce('') { |s,e| s+=e } }
  [($?.exitstatus==0)?(true):(false),out]
end

def exe(bin)
  cmd=bin
  stat,out=docmd(cmd)
  fail(cmd,out) unless stat
  out
end

def fail(cmd=nil,msg=nil)
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
  Dir.glob("tests/*").sort.find_all { |e| e=~/t#{'.'*4}\.f90/ }
end

def translate(f)
  t="#{stem(f)}_ppp#{File.extname(f)}"
  cmd="./ppp #{f} > #{t}"
  stat,out=docmd(cmd)
  fail(cmd,out) unless stat
  t
end

tests.each do |s|
  print '.'
  bin_c=compile(s)
  out_c=exe(bin_c)
  t=translate(s)
  bin_e=compile(t)
  out_e=exe(bin_e)
  fail unless out_c==out_e
  FileUtils.rm_f([bin_c,bin_e,out_c,out_e,t])
end
puts "\nOK"
