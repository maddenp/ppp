debug=false
server_mode=true
threads=8

def pppts_exe(cmd)
  out=IO.popen(cmd+" 2>&1") { |e| e.readlines.reduce("") { |s,e| s+=e } }
  pppts_fail(out,cmd) unless $?.exitstatus==0
  out
end

def pppts_fail(msg=nil,cmd=nil)
  @lock.synchronize do
    return if @failure
    puts "\nFAIL"+((cmd)?(": #{cmd}\n"):(""))
    puts msg.lines.reduce("") { |m,e| m+="      #{e}" } if msg
    @failure=true
  end
end

def pppts_run_all(threads,socket,debug)
  def go(q,socket,debug) 
    pppts_test(q.deq,socket,debug) until q.empty?
  end
  tdir="tests"
  q=Queue.new
  tests=(ARGV[0])?(["#{tdir}/#{ARGV[0]}"]):(Dir.glob("#{tdir}/t*").sort)
  tests.each { |e| q.enq(e) }
  runners=(1..threads).reduce([]) do |m,e|
    m << Thread.new { go(q,socket,debug) }
  end
  runners.each { |e| e.join }
  tests.size
end

def pppts_server_start
  socket=File.expand_path("./socket.#{$$}")
  clear_socket(socket)
  s=Thread.new { server(socket,true) }
  sleep 1 until File.exist?(socket)
  [s,socket]
end

def pppts_test(t,socket,debug)
  @lock.synchronize do
    return if @failure
    print "trying #{File.basename(t)}..." if debug
  end
  pppts_exe("make -C #{t} clean")
  x=(socket)?(" SOCKET=#{socket} "):(" ")
  pppts_exe("make -C #{t}#{x}comp")
  pppts_exe("make -C #{t} clean")
  @lock.synchronize { (debug)?(puts " ok"):(print ".") unless @failure }
end

threads=1 if debug
@failure=false
@lock=Mutex.new
pppts_fail("Tests require at least one thread") unless threads>0
pppts_exe("make")
load File.join(File.dirname($0),"requires.rb")
srvr,socket=(server_mode)?(pppts_server_start):(nil)
n=pppts_run_all(threads,socket,debug)
puts "\nOK (#{n} tests)" unless @failure
if srvr then srvr.raise(Interrupt); srvr.join end
exit((@failure)?(0):(1))
