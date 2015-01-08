$: << File.dirname($0)
require "version"
version_check
require "driver"
Driver.new.server(ARGV[0])
