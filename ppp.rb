$: << File.dirname($0)
require "translator"
GC.disable
Translator.new.go($0,ARGV)
