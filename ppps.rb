#!/usr/bin/env jruby

load File.join(File.dirname($0),"common.rb")
server("/tmp/socket")
