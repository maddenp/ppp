$: << File.dirname($0)
require "version"
version_check
require "driver"
GC.disable
Driver.new.go($0,ARGV)
