server_mode=true
threads=8

$: << File.dirname($0)

require "version"
version_check
require "fileutils"
require "thread"

class PPPTS

  def initialize(debug,server_mode,threads,args)
    @failure=false
    @lock=Mutex.new
    threads=1 if debug
    die("Tests require at least one thread") unless threads>0
    exe("make")
    require "translator"
    t=Translator.new
    srvr,socket=(server_mode)?(server_start(t)):(nil)
    n=run_all(threads,socket,debug,args)
    puts "\nOK (#{n} tests)" unless @failure
    if srvr then srvr.raise(Interrupt); srvr.join end
    exit((@failure)?(0):(1))
  end

  def exe(cmd)
    out=IO.popen(cmd+" 2>&1") { |e| e.readlines.reduce("") { |s,e| s+=e } }
    die(out,cmd) unless $?.exitstatus==0
    out
  end

  def die(msg=nil,cmd=nil)
    @lock.synchronize do
      return if @failure
      puts "\nFAIL"+((cmd)?(": #{cmd}\n"):(""))
      puts msg.lines.reduce("") { |m,e| m+="      #{e}" } if msg
      @failure=true
    end
  end

  def run_all(threads,socket,debug,args)
    def go(q,socket,debug) test(q.deq,socket,debug) until q.empty? end
    def num(s) s.scan(/[0-9]+/).first.to_i end
    tdir="tests"
    tests=Dir.glob("#{tdir}/t*").sort
    if (filter=args[0])
      tests.delete_if { |x| num(x) < num(filter) }
      tests.delete_if { |x| x != "#{tdir}/#{filter}" } unless filter[0]=="+"
    end
    q=Queue.new
    tests.each { |e| q.enq(e) }
    runners=(1..threads).reduce([]) do |m,e|
      m << Thread.new { go(q,socket,debug) }
    end
    runners.each { |e| e.join }
    tests.size
  end

  def server_start(t)
    socket=File.expand_path("./socket.#{$$}")
    FileUtils.rm_f(socket)
    die "Socket file #{socket} in use, please free it" if File.exist?(socket)
    s=Thread.new { t.server(socket,true) }
    sleep 1 until File.exist?(socket)
    [s,socket]
  end

  def test(t,socket,debug)
    @lock.synchronize do
      return if @failure
      print "#{File.basename(t)}..." if debug
    end
    exe("make -C #{t} clean")
    x=(socket)?(" SOCKET=#{socket} "):(" ")
    exe("make -C #{t}#{x}comp")
    exe("make -C #{t} clean")
    @lock.synchronize { (debug)?(puts " ok"):(print ".") unless @failure }
  end

end

ARGV.shift if (debug=(ARGV[0]=="debug"))
PPPTS.new(debug,server_mode,threads,ARGV)
