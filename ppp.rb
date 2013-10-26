$: << File.dirname($0)
require "translator"
Translator.new.go($0,ARGV)
