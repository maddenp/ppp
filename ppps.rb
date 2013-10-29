$: << File.dirname($0)
require "version"
version_check
require "translator"
Translator.new.server(ARGV[0])
