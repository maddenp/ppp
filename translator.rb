module Translator

  include Fortran

  class XFortranParser < FortranParser

    # Clifford Heath's mechanism (http://goo.gl/62pJ6)

    class InputProxy

      attr_accessor :envstack,:srcfile

      def initialize(input,envstack)
        @input=input
        @envstack=envstack
      end

      def method_missing(method,*args)
        @input.send(method,*args)
      end

    end

    def initialize(srcfile,incdirs)
      super()
      @access="_default"
      @dolabels=[]
      @envstack=[{}]
      @incdirs=incdirs
      @srcfile=srcfile
    end

    def parse(input,options={})
      input=InputProxy.new(input,@envstack)
      super(input,options)
    end

  end

  def clear_socket(socket)
    FileUtils.rm_f(socket)
    fail "Socket file #{socket} in use, please free it" if File.exist?(socket)
  end

  def default_opts
    {
      :debug=>false,
      :incdirs=>[],
      :nl=>true,
      :normalize=>false,
      :translate=>true
    }
  end
  
  def directive
    unless @directive
      f=File.join(File.dirname(File.expand_path($0)),"sentinels")
      d=File.open(f,"rb").read.gsub(/\$/,'\$').split("\n").join("|")
      @directive=Regexp.new("^\s*!((#{d}).*)",true)
    end
    @directive
  end

  def fail(msg,die=true,srcfile=nil)
    s="#{msg}"
    s+=": #{srcfile}" if srcfile
    $stderr.puts s
    exit(1) if die
  end

  def go(wrapper)
    @wrapper=wrapper
    fail usage unless srcfile=ARGV.pop
    srcfile=File.expand_path(srcfile)
    fail "Cannot read file: #{srcfile}" unless File.readable?(srcfile)
    s=File.open(srcfile,"rb").read
    opts=unpack({},ARGV)
    puts out(s,:program_units,srcfile,opts)
  end

  def normalize(s,newline)
    np=NormalizerParser.new
    load "normfree.rb"
    s=s.gsub(directive,'@\1')          # hide directives
    s=s.gsub(/^\s+/,"")                # remove leading whitespace
    s=s.gsub(/[ \t]+$/,"")             # remove trailing whitespace
    s=s.gsub(/^!.*\n/,"")              # remove full-line comments
    s=np.parse(np.parse(s).to_s).to_s  # two normalize passes
    s=s.sub(/^\n+/,"")                 # remove leading newlines
    s+="\n" if s[-1]!="\n" and newline # ensure final newline
    s=s.gsub(/^@(.*)/i,'!\1')          # show directives
  end

  def out(s,root,srcfile,opts={})
    opts=default_opts.merge(opts)
    translated_source,raw_tree,translated_tree=process(s,root,srcfile,opts)
    translated_source
  end

  def process(s,root,srcfile,opts={})

    def assemble(s,seen,incdirs=[])
      current=seen.last
      a=""
      r=Regexp.new("^\s*include\s*(\'[^\']+\'|\"[^\"]+\").*",true)
      s.split("\n").each do |line|
        m=r.match(line)
        if m
          incfile=m[1][1..-2]
          if incfile[0]=="/" or incfile[0]=="."
            incfile=File.expand_path(File.join(File.dirname(current),incfile))
            unless File.exist?(incfile)
              fail "Could not find included file #{incfile}"
            end
          else
            found=false
            incdirs.each do |d|
              maybe=File.expand_path(File.join(d,incfile))
              if File.exist?(maybe)
                found=true
                incfile=maybe
                break
              end
            end
            unless found
              fail "Could not find included file #{incfile} on search path"
            end
          end
          if seen.include?(incfile)
            msg="File #{current} includes #{incfile} recursively:\n"
            msg+=incchain(seen,incfile)
            fail msg
          end
          unless File.readable?(incfile)
            msg="Could not read file #{incfile} "
            msg+=incchain(seen,incfile)
            fail msg
          end
          a+=assemble(File.open(incfile,"rb").read,seen+[incfile],incdirs)
        else
          a+="#{line}\n"
        end
      end
      a
    end

    def cppcheck(s)
      r=Regexp.new("^\s*#")
      i=1
      s.split("\n").each do |line|
        m=r.match(line)
        fail "Detected cpp directive:\n\n#{i}: #{line.strip}" if m
        i+=1
      end
    end

    def fixed2free(s)
      np=NormfixedParser.new
      s=np.parse(np.parse(s).to_s).to_s
      p="\(.*?\)\(^[^\n]+\)\n[ \t]{5}[^0 \t]\(.*\)"
      r=Regexp.new(p,Regexp::MULTILINE)
      while m=r.match(s)
        blanks=' '*(72-m[2].length)
        s=m[1]+m[2]+blanks+m[3]
      end
      s
    end

    def incchain(seen,incfile)
      "\n  "+(seen+[incfile]).join(" includes\n  ")
    end

    def wrap(s)

      def directive?(s)
        s=~directive
      end

      max=132
      a=s.split("\n")
      (0..a.length-1).each do |n|
        e=a[n].chomp
        unless directive?(e)
          if e.length>max
            e=~/^( *).*$/
            i=$1.length+2
            t=""
            begin
              r=[max-2,e.length-1].min
              t+=e[0..r]+"&\n"
              e=" "*i+"&"+e[r+1..-1]
            end while e.length>max
            t+=e
            a[n]=t
          end
        end
      end
      a.join("\n")
    end

    opts=default_opts.merge(opts)
    debug=opts[:debug]
    fp=XFortranParser.new(srcfile,opts[:incdirs])
    s=prepsrc(s) if defined? prepsrc
    s=assemble(s,[srcfile],opts[:incdirs])
    cppcheck(s)
    puts "RAW SOURCE\n\n#{s}\n" if debug
    puts "NORMALIZED SOURCE\n" if debug
    s=fixed2free(s) if opts[:fixed]
    n=normalize(s,opts[:nl])
    puts "\n#{n}" if debug or opts[:normalize]
    unless opts[:normalize]
      raw_tree=fp.parse(n,:root=>root)
      raw_tree.instance_variable_set(:@srcfile,srcfile)
      raw_tree=raw_tree.post_top if raw_tree # post-process raw tree
      if debug
        puts "\nRAW TREE\n\n"
        p raw_tree
      end
      re=Regexp.new("^(.+?):in `([^\']*)'$")
      srcmsg=(re.match(caller[0])[2]=="raw")?(": See #{caller[1]}"):("")
      unless raw_tree
        na=n.split("\n")
        na.each_index { |i| $stderr.puts "#{i+1} #{na[i]}" }
        failmsg=""
        failmsg+="#{fp.failure_reason.split("\n")[0]}\n"
        failmsg+="Original source: #{srcfile}\n"
        failmsg+="PARSE FAILED"
        failmsg+="#{srcmsg}"
        fail failmsg
        return # if in server mode and did not exit in fail()
      end
      translated_tree=(opts[:translate])?(raw_tree.translate_top):(nil)
      if debug
        puts "\nTRANSLATED TREE\n\n"
        p translated_tree
      end
      t=wrap(translated_tree.to_s)
      puts "\nTRANSLATED SOURCE\n\n" if debug
    end
    [t,raw_tree,translated_tree]
  end

  def raw(s,root,srcfile,opts={})
    opts=default_opts.merge(opts)
    opts[:translate]=false
    translated_source,raw_tree,translated_tree=process(s,root,srcfile,opts)
    raw_tree
  end

  def server(socket,quiet=false)
    srcfile='unknown'
    clear_socket(socket)
    trap('INT')  { raise Interrupt }
    trap('TERM') { raise Interrupt }
    monitor=Thread.new { server_mon(Process.getpgrp,socket) }
    UNIXServer.open(socket) do |server|
      while true
        begin
          opts={}
          client=server.accept
          srcfile=client.gets.chomp
          dirlist=client.gets.chomp
          lensrc=client.gets.chomp.to_i
          s=client.read(lensrc)
          unless File.exist?(srcfile)
            fail("No such file: #{srcfile}",false,srcfile)
          end
          srcdir=File.dirname(File.expand_path(srcfile))
          opts[:incdirs]=[srcdir]
          dirlist.split(":").each do |d|
            d=File.join(srcdir,d) if Pathname.new(d).relative?
            unless File.directory?(d)
              fail("No such directory: #{d}",false,srcfile)
            end
            opts[:incdirs].push(d)
          end
          puts "Translating #{srcfile}" unless quiet
          client.puts(out(s,:program_units,srcfile,opts))
          client.close
        rescue Errno::EPIPE
          # Handle broken pipe (i.e. other end of the socket dies)
        rescue Interrupt=>ex
          server_stop(socket,false)
          return
        rescue SystemExit=>ex
          monitor.join
        rescue Exception=>ex
          s="Caught exception '#{ex.class}':\n"
          s+="#{ex.message}\n"
          s+=ex.backtrace.reduce(s) { |m,x| m+="#{x}\n" }
          fail s
          server_stop(socket,1)
        end
      end
    end
  end

  def server_mon(psgroup,socket)
    while true
      begin
        # Try to send no-op signal 0 to the process that started the server. If
        # that process is no longer running, catch the exception informing us so
        # and shut down the server.
        Process.kill(0,psgroup)
      rescue Errno::ESRCH
        server_stop(socket,0)
      end
      sleep 10
    end
  end

  def server_stop(socket,status)
    FileUtils.rm_f(socket)
    exit(status) unless status==false
  end

  def tree(s,root,srcfile,opts={})
    opts=default_opts.merge(opts)
    translated_source,raw_tree,translated_tree=process(s,root,srcfile,opts)
    translated_tree
  end

  def unpack(opts,args)
    opts[:incdirs]=["."]
    while opt=args.shift
      case opt
      when "-I"
        dirlist=args.shift
        fail usage unless dirlist
        dirlist.split(":").each do |d|
          fail "No such directory: #{d}" unless File.directory?(d)
          opts[:incdirs].push(d)
        end
      when "fixed"
        opts[:fixed]=true
      when "debug"
        opts[:debug]=true
      when "normalize"
        opts[:normalize]=true
      else
        fail usage
      end
    end
    opts
  end

  def usage
    opts=[]
    opts.push("-I dir[:dir:...]]")
    opts.push("debug")
    opts.push("fixed")
    opts.push("normalize")
    "#{File.basename(@wrapper)} [ #{opts.join(" | ")} ] source"
  end

end
