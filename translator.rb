$: << (basedir=File.dirname($0))

require "fileutils"
require "pathname"
require "socket"
require "thread"

require "dehollerizer"
require "fortran"
require "fortran_parser"
require "sms_fortran"
require "sms_fortran_parser"
require "normalizer"
require "normalizer_parser"
require "sms_normalizer"
require "sms_normalizer_parser"
require "normfixed"
require "normfree"

class Translator

  include Fortran

  class Stringmap

    attr_reader :re

    def initialize
      @index=0
      @map={}
      @re=Regexp.new("(#[0-9]+#)")
    end

    def get(k)
      @map[k]
    end

    def set(s)
      k="##{@index+=1}#"
      @map[k]=s
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

      def to_s
        @input.to_s
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

      def to_s
        @input.to_s
      end

    end

    def parse(input,op,stringmap)
      input=Xinput.new(input,op,stringmap)
      super(input)
    end

  end

  def clear_socket(socket)
    FileUtils.rm_f(socket)
    die "Socket file #{socket} in use, please free it" if File.exist?(socket)
  end

  def chkparse(s)
    die "NORMALIZING PARSE FAILED" if s.nil? or s=~/^\s*$/
    s
  end
      
  def default_opts
    {
      :debug=>false,
      :incdirs=>[],
      :modinfo=>false,
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

  def die(msg,die=true,srcfile=nil)
    s="#{msg}"
    s+=": #{srcfile}" if srcfile
    $stderr.puts s
    exit(1) if die
  end

  def fix_pt_norm(s,parser,op=nil,stringmap=nil)
    # fixed-point normalization
    s0=nil
    while s=parser.parse(s,op,stringmap).to_s and not s.nil?
      s=s.gsub(/^$ *\n/,'')
      return s if s==s0
      s0=s
    end
  end

  def go(wrapper,args)
    @wrapper=wrapper
    die usage unless srcfile=args.pop
    srcfile=File.expand_path(srcfile)
    die "Cannot read file: #{srcfile}" unless File.readable?(srcfile)
    s=File.open(srcfile,"rb").read
    conf=unpack({},args)
    puts out(s,:program_units,srcfile,conf)
  end

  def normalize(s,conf)
    np=XNormalizerParser.new
    np.update(Normfree)
    unless conf.fixed
      @m=Stringmap.new
      d=Dehollerizer.new
      s=d.process(@m,s,conf)            # mask holleriths (only if not already done in fixed2free)
    end
    s=s.gsub(directive,'@\1')           # hide directives
    s=s.gsub(/\t/," ")                  # tabs to spaces
    s=s.gsub(/^ +/,"")                  # remove leading whitespace
    s=s.gsub(/ +$/,"")                  # remove trailing whitespace
    s=s.gsub(/^ *!.*$\n/,"")            # remove full-line comments
    s=s.gsub(/^[ \t]*\n/,'')            # remove blank lines (continuation statement issue fix)
    s=chkparse(fix_pt_norm(s,np,1,@m))  # string-aware transform
    s=s.gsub(/& *\n *&?/,"")            # join continuation lines
    s=chkparse(np.parse(s,2,@m).to_s)   # mask original strings
    s=s.downcase                        # lower-case text only
    s=s.gsub(/ +/,"")                   # remove spaces
    s=restore_strings(s,@m)             # restore strings
    s=s.sub(/^\n+/,"")                  # remove leading newlines
    s=s+"\n" if s[-1]!="\n" and conf.nl # append final newline if required
    s=s.chomp unless conf.nl            # remove final newline if forbidden
    s=s.gsub(/^@(.*)/i,'!\1')           # show directives
    s=s.gsub(/^ *\n/,"")                # remove blank lines
    s
  end

  def out(s,root,srcfile,conf)
    conf=ostruct_default_merge(conf)
    translated_source,raw_tree,translated_tree=process(s,root,srcfile,conf)
    unless conf.normalize
      t=vertspace(translated_source)
    end
    t
  end

  def process(s,root,srcfile,conf)

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
              die "Could not find included file #{incfile}"
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
              die "Could not find included file #{incfile} on search path"
            end
          end
          if seen.include?(incfile)
            msg="File #{current} includes #{incfile} recursively:\n"
            msg+=incchain(seen,incfile)
            die msg
          end
          unless File.readable?(incfile)
            msg="Could not read file #{incfile} "
            msg+=incchain(seen,incfile)
            die msg
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
        die "Detected cpp directive:\n\n#{i}: #{line.strip}" if m
        i+=1
      end
    end

    def detabify(s)
      # Intel tab-format scheme for fixed-format source
      # labeled statement
      while (m=Regexp.new("^([0-9 ]{1,5})\t").match(s))
        label=m[1].delete(" ")
        s=s.sub(m[0],label+(" "*(5-(label.size-1))))
      end
      # comment line
      s=s.gsub(/^[cC*]\t/,'! ')
      # continuation line
      s=s.gsub(/^\t[1-9]/,'     a')
      # unlabeled statement
      s=s.gsub(/^\t/,'      ')
      s
    end

    def fixed2free(s,conf)
      # Normalizer from fixed form to free form
      np=XNormalizerParser.new
      np.update(Normfixed)
      s=detabify(s)
      s=s.gsub(/^[ \t]*\n/,'')                 # remove blank lines
      a=s.split("\n")                          # split file into an array by line
      a=a.map {|e| (e+(" "*72))[0..71]}        # pad each line with 72 blanks, truncate at column 72
      s=a.join("\n")                           # join array into string
      s=s.gsub(/^(c|C|\*)/,"!")                # replace fixed form comment indicators with "!"
      s=s.gsub(directive,'@\1')                # hide directives
      if s=~/\n[ \t]{5}\#/                     # gives user more information about problem
        $stderr.puts "ERROR:'#' in column six is a cpp directive, not\na valid fixed-form continuation character\n\n"
      end                                      # will always end in die() from cpp-check
      s=s.gsub(/\n[ \t]{5}[^ \t0]/,"\n     a") # replace any continuation character with generic "a"
      s=s.gsub(/^[ \t]*!.*$\n?/,"")            # remove full-line comments
      @m=Stringmap.new                        
      d=Dehollerizer.new
      s=d.process(@m,s,conf)                   # mask holleriths (also removes hollerith-important continuations)
      s=chkparse(fix_pt_norm(s,np))            # string-aware transform & parse error checking
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

      maxcols=132 # columns
      maxcont=39  # continuation lines
      a=s.split("\n")
      (0..a.length-1).each do |n|
        cont=0
        e=a[n].chomp
        unless directive?(e)
          if e.length>maxcols
            e=~/^( *).*$/
            i=$1.length+2
            t=""
            begin
              r=[maxcols-2,e.length-1].min
              t+=e[0..r]+"&\n"
              e=" "*i+"&"+e[r+1..-1]
              cont+=1
            end while e.length>maxcols
            t+=e
            if cont>maxcont
              die "ERROR: More than #{maxcont} continuation lines:\n\n#{t}"
            end
            a[n]=t
          end
        end
      end
      s=a.join("\n")
      s
    end

    def vertspace(t)
      t=t.gsub(/\n\n\n+/,"\n\n") # Collapse multiple blank lines into one
      t[0]=t[0].sub(/^\n/,"")    # Remove blank first line
      t
    end
    
    conf=ostruct_default_merge(conf)
    fp=XFortranParser.new(srcfile,conf.incdirs)
    s0=nil
    while s!=s0 and not s.nil?                                     # Fixed point treatment of prepsrc() and assemble()
      s0=s                                                         # for cases in which files added by 'include'
      s=prepsrc_fixed(s) if defined?(prepsrc_fixed) and conf.fixed # statements or '!sms$insert include' statements
      s=prepsrc_free(s) if defined?(prepsrc_free)                  # also contain such a statement; this follows the path
      s=assemble(s,[srcfile],conf.incdirs)                         # until all souce has been appropriately inserted
    end
    if conf.debug
      puts "RAW #{(conf.fixed)?("FIXED"):("FREE")}-FORM SOURCE\n\n#{s}"
    end
    if conf.fixed
      puts "\nFREE-FORM TRANSLATION\n\n" if conf.debug
      s=fixed2free(s,conf)
      puts "#{s}\n\n" if conf.debug
    end
    cppcheck(s) # When a continuation warning in fixed2free is shown, cppcheck will exit in die()
    puts "NORMALIZED FORM\n" if conf.debug
    n=normalize(s,conf)
    puts "\n#{n}" if conf.debug or conf.normalize
    unless conf.normalize
      raw_tree=fp.parse(n,{:root=>root})
      exit if conf.modinfo
      raw_tree.instance_variable_set(:@srcfile,srcfile)
      raw_tree=raw_tree.post_top if raw_tree # post-process raw tree
      if conf.debug
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
        die failmsg
        return # if in server mode and did not exit in die()
      end
      translated_tree=(conf.translate)?(raw_tree.translate_top):(nil)
      if conf.debug
        puts "\nTRANSLATED TREE\n\n"
        p translated_tree
      end
      t=wrap(translated_tree.to_s)
      puts "\nTRANSLATED SOURCE\n\n" if conf.debug
    end
    [t,raw_tree,translated_tree]
  end

  def raw(s,root,srcfile,conf=OpenStruct.new)
    conf=ostruct_default_merge(conf)
    conf.translate=false
    translated_source,raw_tree,translated_tree=process(s,root,srcfile,conf)
    raw_tree
  end

  def restore_strings(s,stringmap)
    # Source-program string literals may contain our string placeholder token,
    # so we must avoid processing restored strings (e.g. via gsub), as their
    # contents could incorrectly be 'restored'. So, split the source on the
    # placeholder token and iterate *once* over it, replacing token keys with
    # their corresponding saved values.
    r=stringmap.re
    a=s.split(r)
    a.map! { |e| (e=~r)?(stringmap.get(e)):(e) }
    s=a.join
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
          conf=OpenStruct.new
          client=server.accept
          srcfile=client.gets.chomp
          form=client.gets.chomp
          dirlist=client.gets.chomp
          lensrc=client.gets.chomp.to_i
          s=client.read(lensrc)
          unless File.exist?(srcfile)
            die("No such file: #{srcfile}",false,srcfile)
          end
          srcdir=File.dirname(File.expand_path(srcfile))
          conf.incdirs=[srcdir]
          conf.fixed=(form=="fixed")
          dirlist.split(":").each do |d|
            d=File.join(srcdir,d) if Pathname.new(d).relative?
            unless File.directory?(d)
              die("No such directory: #{d}",false,srcfile)
            end
            conf.incdirs.push(d)
          end
          puts "Translating #{srcfile}" unless quiet
          client.puts(out(s,:program_units,srcfile,conf))
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
          die s
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

  def tree(s,root,srcfile,conf)
    conf=ostruct_default_merge(conf)
    translated_source,raw_tree,translated_tree=process(s,root,srcfile,conf)
    translated_tree
  end

  def ostruct_default_merge(a)
    a=a.marshal_dump if a.is_a?(OpenStruct)
    OpenStruct.new(default_opts.merge(a))
  end

  def unpack(conf,args)
    conf=OpenStruct.new(conf)
    conf.incdirs=["."]
    while opt=args.shift
      case opt
      when "-I"
        dirlist=args.shift
        die usage unless dirlist
        dirlist.split(":").each do |d|
          die "No such directory: #{d}" unless File.directory?(d)
          conf.incdirs.push(d)
        end
      when "fixed"
        conf.fixed=true
      when "free"
        nil # default behavior
      when "debug"
        conf.debug=true
      when "normalize"
        conf.normalize=true
      when "modinfo"
        conf.modinfo=true
      else
        die usage
      end
      if conf.modinfo and conf.normalize
        die ("ERROR: 'normalize' and 'modinfo' are mutually exclusive\n"+usage)
      end
    end
    conf
  end

  def usage
    x=[]
    x.push("-I dir[:dir:...]")
    x.push("debug")
    x.push("fixed")
    x.push("modinfo")
    x.push("normalize")
    "#{File.basename(@wrapper)} [ #{x.join(" | ")} ] source"
  end

end
