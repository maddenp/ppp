$: << File.dirname($0)
require "version"
version_check
require "translator"
GC.disable
Translator.new.go($0,ARGV)
