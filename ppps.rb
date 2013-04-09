load File.join(File.dirname($0),"common.rb")
wrapper=File.basename($ARGV[0])
socket=$ARGV[1]
fail "Usage: #{wrapper} socket" unless socket
fail "Socket #{socket} already exists" if File.exist?(socket)
server(socket)
