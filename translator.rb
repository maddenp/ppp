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
require "exceptions"

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

      attr_accessor :dstfile,:envstack,:srcfile

      def initialize(input,srcfile,dstfile,envstack)
        @input=input
        @srcfile=srcfile
        @dstfile=dstfile
        @envstack=envstack
      end

      def method_missing(method,*args)
        @input.send(method,*args)
      end

    end

    def initialize(srcfile,dstfile,incdirs)
      super()
      @access="_default"
      @dolabels=[]
      @envstack=[]
      @incdirs=incdirs
      @srcfile=srcfile
      @dstfile=dstfile
    end

    def parse(input,env,options={})
      env||={}
      env[:global]||={:srcfile=>@srcfile,:dstfile=>@dstfile}
      @envstack.push(env)
      input=Xinput.new(input,@srcfile,@dstfile,@envstack)
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
    fail "NORMALIZING PARSE FAILED" if s.nil? or s=~/^\s*$/
    s
  end

  def default_opts
    {
      :debug=>false,
      :form=>:free,
      :incdirs=>["."],
      :nl=>true,
      :product=>:translated_source
    }
  end

  def directive
    return @@directive if defined?(@@directive)
    f=File.join(File.dirname(File.expand_path($0)),"sentinels")
    d=File.open(f,"rb").read.gsub(/\$/,'\$').split("\n").join("|")
    @@directive=Regexp.new("^\s*!((#{d}).*)",true)
  end

  def die(msg,quit=true,srcfile=nil)
    s="#{msg}"
    s+=": #{srcfile}" if srcfile
    $stderr.puts s
    exit(1) if quit
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
    die usage unless dstfile=args.pop # a directory when modinfo requested
    die usage unless srcfile=args.pop
    srcfile=File.expand_path(srcfile)
    die "Cannot read file: #{srcfile}" unless File.readable?(srcfile)
    s=File.open(srcfile,"rb").read
    conf=unpack(args)
    if conf[:product]==:modinfo and not File.directory?(dstfile)
      die "'modinfo' requires output directory, not #{dstfile}"
    end
    begin
      translation=process(s,:program_units,srcfile,dstfile,conf)
      File.open(dstfile,'w').write(translation) unless conf[:product]==:modinfo
    rescue Exceptions::TranslatorException
      # suppress exception info display
    end
  end

  def normalize(s,conf)
    safe=conf[:safe] # only true for internal parses
    np=XNormalizerParser.new
    np.update(Normfree)
    unless conf[:form]==:fixed
      @m=Stringmap.new
      Dehollerizer.new.dehollerize(@m,s,conf[:form]==:fixed) unless safe
    end
    nq=(not s=~/['"]/)         # no quotes?
    unless safe
      s=s.gsub(/\t/," ")       # tabs to spaces
      s=s.gsub(/^ +/,"")       # remove leading whitespace
    end
    s=s.gsub(directive,'@\1')  # hide directives
    unless safe
      s=s.gsub(/ +$/,"")       # remove trailing whitespace
      s=s.gsub(/^ *!.*$\n/,"") # remove full-line comments
      s=s.gsub(/^[ \t]*\n/,'') # remove blank lines
    end
    s=chkparse(fix_pt_norm(s,np,1,@m)) unless safe and nq # string-aware transform
    s=s.gsub(/& *\n *&?/,"") unless safe                  # join continuations
    s=chkparse(np.parse(s,2,@m).to_s) unless safe and nq  # mask strings
    s=s.downcase unless safe                              # lower-case text only
    s=s.gsub(/ +/,"")                                     # remove spaces
    s=restore_strings(s,@m) unless safe and nq            # restore strings
    s=s.sub(/^\n+/,"") unless safe                        # rm leading newlines
    s=s+"\n" if s[-1]!="\n" and conf[:nl]                 # add final newline?
    s=s.chomp unless conf[:nl]                            # del final newline?
    s=s.gsub(/^@(.*)/i,'!\1')                             # show directives
    s=s.gsub(/^ *\n/,"")                                  # remove blank lines
    s
  end

  def process(s,root,srcfile,dstfile,conf)

    def assemble(s,seen,incdirs=[])
      current=seen.last
      assembled=""
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
          assembled+=assemble(File.open(incfile,"rb").read,seen+[incfile],incdirs)
        else
          assembled+="#{line}\n"
        end
      end
      assembled
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
      np=XNormalizerParser.new
      np.update(Normfixed)
      d=Dehollerizer.new
      @m=Stringmap.new
      s=detabify(s)                            # convert leading tabs
      s=s.gsub(/^[ \t]*\n/,'')                 # remove blank lines
      a=s.split("\n")                          # string -> array of lines
      a=a.map {|e| (e+(" "*72))[0..71]}        # blank-pad lines to column 72
      s=a.join("\n")                           # array of lines -> string
      s=s.gsub(/^(c|C|\*)/,"!")                # all comment markers -> '!'
      s=s.gsub(directive,'@\1')                # hide directives
      s=s.gsub(/\n[ \t]{5}[^ \t0]/,"\n     a") # all continuation markers -> 'a'
      s=s.gsub(/^[ \t]*!.*$\n?/,"")            # remove full-line comments
      d.dehollerize(@m,s,conf[:form]==:fixed)  # mask holleriths
      s=chkparse(fix_pt_norm(s,np))            # string-aware transform
      s=s.gsub(/\n[ \t]{5}a/,"")               # join continuations
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
#           if cont>maxcont
#             die "ERROR: More than #{maxcont} continuation lines:\n\n#{t}"
#           end
            a[n]=t
          end
        end
      end
      s=a.join("\n")
      s
    end

    def vertspace(t)
      empty=/^$/
      a=t.split("\n")
      a.delete_at(0) while a[0]=~empty
      b=[]
      a.each_index { |i| b.push(a[i]) unless a[i]=~empty and a[i+1]=~empty }
      t=b.join("\n")
      t
    end

    conf=default_opts.merge(conf)
    fixed=(conf[:form]==:fixed)
    fp=XFortranParser.new(srcfile,dstfile,conf[:incdirs])
    s0=nil
    unless conf[:safe]
      while s!=s0 and not s.nil?                                # fixed point treatment of prepsrc() and assemble()
        s0=s                                                    # for cases in which files added by 'include'
        s=prepsrc_fixed(s) if defined?(prepsrc_fixed) and fixed # statements or '!sms$insert include' statements
        s=prepsrc_free(s) if defined?(prepsrc_free)             # also contain such a statement; this follows the path
        s=assemble(s,[srcfile],conf[:incdirs])                  # until all souce has been inserted
      end
    end
    puts "RAW #{(fixed)?("FIXED"):("FREE")}-FORM SOURCE\n\n#{s}" if conf[:debug]
    if fixed
      puts "\nFREE-FORM TRANSLATION\n\n" if conf[:debug]
      s=fixed2free(s,conf)
      puts "#{s}\n\n" if conf[:debug]
    end
    cppcheck(s) unless conf[:safe]
    puts "\nNORMALIZED FORM\n" if conf[:debug]
    n=normalize(s,conf)
    return "\n#{n}" if conf[:product]==:normalized_source
    puts "\n#{n}" if conf[:debug]
    raw_tree=fp.parse(n,conf[:env],{:root=>root})
    return if conf[:product]==:modinfo
    unless raw_tree
      re=Regexp.new("^(.+?):in `([^\']*)'$")
      na=n.split("\n")
      na.each_index { |i| $stderr.puts "#{i+1} #{na[i]}" }
      indent=na.size.to_s.size
      $stderr.puts "#{'^'*indent} See line #{fp.failure_line}"
      if re.match(caller[0])[2]=="raw"
        $stderr.puts "#{' '*indent} Internally-generated code, see #{caller[1]}"
      else
        $stderr.puts "#{' '*indent} Source: #{srcfile}"
      end
      $stderr.puts "PARSE FAILED"
      fail Exceptions::TranslatorException
    end
    raw_tree.instance_variable_set(:@srcfile,srcfile)
    raw_tree=raw_tree.transform_top(:post) # post-process raw tree
    if conf[:debug]
      puts "\nRAW TREE\n\n"
      p raw_tree
    end
    case conf[:product]
    when :raw_tree
      return raw_tree
    when :raw_source
      $INDENTED=true
      return vertspace(wrap(raw_tree.to_s))
    when :translated_source
      translated_tree=raw_tree.transform_top(:translate)
      fail "TRANSLATION FAILED" unless translated_tree
      if conf[:debug]
        puts "\nTRANSLATED TREE\n\n"
        p translated_tree
      end
      $INDENTED=true
      t=vertspace(wrap(translated_tree.to_s))
      puts "\nTRANSLATED SOURCE\n\n" if conf[:debug]
      return t
    else
      fail "ERROR: Unknown product '#{conf[:product]}'"
    end

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
          conf={}
          client=server.accept
          srcfile=client.gets.chomp
          dstfile=client.gets.chomp
          action=client.gets.chomp
          form=client.gets.chomp
          dirlist=client.gets.chomp
          lensrc=client.gets.chomp.to_i
          s=client.read(lensrc)
          unless File.exist?(srcfile)
            die("No such file: #{srcfile}",false,srcfile)
          end
          srcdir=File.dirname(File.expand_path(srcfile))
          conf[:incdirs]=[srcdir]
          if action=="translate"
            conf[:product]=:translated_source
          elsif action=="passthrough"
            conf[:product]=:raw_source
          elsif action=="modinfo"
            conf[:product]=:modinfo
          else
            puts "ERROR: Unknown action '#{action}'" unless quiet
            raise Exceptions::TranslatorException
          end
          if form=="fixed"
            conf[:form]=:fixed
          elsif form=="free"
            conf[:form]=:free
          else
            puts "ERROR: Unknown form '#{form}'" unless quiet
          end
          dirlist.split(":").each do |d|
            d=File.join(srcdir,d) if Pathname.new(d).relative?
            unless File.directory?(d)
              die("No such directory: #{d}",false,srcfile)
            end
            conf[:incdirs].push(d)
          end
          product=(conf[:product]==:modinfo)?("modinfo"):("translation")
          puts "Producing #{product} for #{srcfile}" unless quiet
          translation=process(s,:program_units,srcfile,dstfile,conf)
          client.puts(translation)
          client.close
        rescue Errno::EPIPE
          # Handle broken pipe (i.e. other end of the socket dies)
        rescue Interrupt=>ex
          server_stop(socket,false)
          return
        rescue SystemExit=>ex
          monitor.join
        rescue Exceptions::TranslatorException=>ex
          begin
            client.puts("\x00") # C null
            client.close
          rescue Exception=>ex
            $stderr.puts "Error writing response to client"
          end
        rescue Exception=>ex
          $stderr.puts "Caught exception '#{ex.class}':"
          $stderr.puts "#{ex.message}"
          ex.backtrace.each { |x| $stderr.puts x }
          server_stop(socket,1)
          exit(1)
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

  def unpack(args)
    conf={}
    conf[:incdirs]=["."]
    while opt=args.shift
      case opt
      when "-I"
        dirlist=args.shift
        die usage unless dirlist
        dirlist.split(":").each do |d|
          die "No such directory: #{d}" unless File.directory?(d)
          conf[:incdirs].push(d)
        end
      when "fixed"
        conf[:form]=:fixed
      when "debug"
        conf[:debug]=true
      when "normalize"
        conf[:product]=:normalized_source
      when "modinfo"
        conf[:product]=:modinfo
      when "passthrough"
        conf[:product]=:raw_source
      else
        die usage
      end
    end
    conf
  end

  def usage
    x=[]
    x.push("-I <dir>[:<dir>:...]")
    x.push("debug")
    x.push("fixed")
    x.push("modinfo")
    x.push("normalize")
    x.push("passthrough")
    "#{File.basename(@wrapper,'.rb')} [ #{x.join(" | ")} ] <infile> <outfile>|<moddir>"
  end

end
