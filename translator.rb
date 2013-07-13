module Translator

  include Fortran

  class Stringmap

    def initialize
      @index=1
      @map={}
    end

    def get(k)
      @map[k]
    end

    def set(s)
      k="##{@index}#"
      @map[k]=s
      @index+=1
      k
    end

  end
      
  # X*Parser scheme due to Clifford Heath (http://goo.gl/62pJ6)

  class XFortranParser < FortranParser

    class Xinput

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
      input=Xinput.new(input,@envstack)
      super(input,options)
    end

  end

  class XNormalizerParser < NormalizerParser

    class Xinput

      attr_accessor :op,:stringmap

      def initialize(input,op,stringmap)
        @input=input
        @op=op
        @stringmap=stringmap
      end

      def method_missing(method,*args)
        @input.send(method,*args)
      end

    end

    def parse(input,op,stringmap)
      input=Xinput.new(input,op,stringmap)
      super(input)
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

  def dehollerith(s)
    # Convert instances of Hollerith(-esque) constants to quoted strings.
    def replace(s,re)
      r=Regexp.new(re,true) # true => case-insensitivity
      while m=r.match(s)
        first=m[1]
        strlen=m[2].to_i-1
        string=m[3][0..strlen]
        rest=m[3].sub(/^#{string}/,"")
        s=s.gsub(m[0],"#{first}'#{string}'#{rest}")
      end
      s
    end
    w  = "[ \t]*"                  # whitespace
    lm = "^(#{w}[0-9]{1,5}#{w}"    # label (mandatory)
    lo = "^(#{w}[0-9]{0,5}#{w}"    # label (optional)
    n  = ")([0-9]+)#{w}"           # number
    o  = ".*?"                     # other
    r  = "(.*)"                    # rest (including hollerith constant)
    v  = "#{w}[a-z][a-z0-9_]*#{w}" # variable name
    res=[]
    res.push(lm+"format"+w+"\\("+o+n+"h"+r) # F90:R1016 char-string-edit-desc
    res.push(lo+"data"+v+"/"+o+n+"h"+r)     # Hollerith in data-stmt
    res.each { |re| s=replace(s,re) }
    s
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

  def fpn(s,parser,op=nil,stringmap=nil)
    # fixed-point normalization
    s0=nil
    while s=parser.parse(s,op,stringmap).to_s
      s=s.gsub(/^$[ \t]*\n/,'')
      return s if s==s0
      s0=s
    end
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

  def restore_strings(s,stringmap)
    # Source-program string literals may contain our string placeholder token,
    # so we must avoid processing restored strings (e.g. via gsub), as their
    # contents could incorrectly be 'restored'. So, split the source on the
    # placeholder token and iterate *once* over it, replacing token keys with
    # their corresponding saved values.
    r=Regexp.new("(#[0-9]+#)")
    a=s.split(r)
    a.map! { |e| (e=~r)?(stringmap.get(e)):(e) }
    s=a.join
  end

  def normalize(s,newline)
    np=XNormalizerParser.new
    np.update(Normfree)
    m=Stringmap.new
    s=s.gsub(directive,'@\1')           # hide directives
    s=s.gsub(/^[ \t]+/,"")              # remove leading whitespace
    s=s.gsub(/[ \t]+$/,"")              # remove trailing whitespace
    s=s.gsub(/^[ \t]*!.*$\n/,"")        # remove full-line comments
    s=fpn(s,np,1,m)                     # string-aware transform 1
    s=s.gsub(/&[ \t]*\n[ \t]*&?/,"")    # join continuation lines
    s=np.parse(s,2,m).to_s              # hide original strings
    s=dehollerith(s)                    # replace holleriths
    s=np.parse(s,2,m).to_s              # hide dehollerith'ed strings
    s=s.downcase                        # lower-case text only
    s=s.gsub(/[ \t]+/,"")               # remove whitespace
    s=restore_strings(s,m)              # restore strings
    s=s.sub(/^\n+/,"")                  # remove leading newlines
    s=s+"\n" if s[-1]!="\n" and newline # append final newline if required
    s=s.chomp unless newline            # remove final newline if forbidden
    s=s.gsub(/^@(.*)/i,'!\1')           # show directives
    s=s.gsub(/^[ \t]*\n/,'')            # remove blank lines
    s
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
      np=XNormalizerParser.new
      np.update(Normfixed)
      unless /\n[ \t]*\t/ !~ s
        fail ("ERROR: NO SUPPORT FOR TABS IN LEADING WHITESPACE")
      end
      s=s.gsub(/^[ \t]*\n/,'')                 # remove blank lines
      a=s.split("\n")                          # split file into an array by line
      a=a.map {|e| (e+(" "*72))[0..71]}        # pad each line with 72 blanks, truncate at column 72
      s=a.join("\n")                           # join array into string
      s=s.gsub(/^(c|C|\*)/,"!")                # replace fixed form comment indicators with "!"
      s=s.gsub(directive,'@\1')                # hide directives
      s=s.gsub(/\n[ \t]{5}[^ \t0]/,"\n     a") # replace any continuation character with generic "a"
      s=s.gsub(/^[ \t]*!.*$\n?/,"")            # remove full-line comments
      s=fpn(s,np)                              # string-aware transform
      s=s.gsub(/\n[ \t]{5}a/,"")               # join continuation lines
      s=s.gsub(/^@(,*)/i,'!\1')                # show directives
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
    s=prepsrc_fixed(s) if defined? prepsrc_fixed and opts[:fixed]
    s=prepsrc_free(s) if defined? prepsrc_free
    s=assemble(s,[srcfile],opts[:incdirs])
    cppcheck(s)
    if debug
      puts "RAW #{(opts[:fixed])?("FIXED"):("FREE")}-FORM SOURCE\n\n#{s}\n"
    end
    if opts[:fixed]
      puts "FREE-FORM TRANSLATION\n\n" if debug
      s=fixed2free(s)
      puts "#{s}\n\n" if debug
    end
    puts "NORMALIZED FORM\n" if debug
    n=normalize(s,opts[:nl])
    puts "\n#{n}" if debug or opts[:normalize]
    unless opts[:normalize]
      raw_tree=fp.parse(n,{:root=>root})
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
          form=client.gets.chomp
          dirlist=client.gets.chomp
          lensrc=client.gets.chomp.to_i
          s=client.read(lensrc)
          unless File.exist?(srcfile)
            fail("No such file: #{srcfile}",false,srcfile)
          end
          srcdir=File.dirname(File.expand_path(srcfile))
          opts[:incdirs]=[srcdir]
          opts[:fixed]=(form=="fixed")
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
      when "free"
        nil # default behavior
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
