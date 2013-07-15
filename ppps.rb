$: << File.dirname($0)
require "translator"
Translator.new.server(ARGV[0])
