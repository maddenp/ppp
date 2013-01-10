module PPP

  require "treetop"
  require "fortran"
  require "fortran_parser"
  require "normalize"
  require "normalize_parser"
  require "ostruct"
  require "yaml"

  include Fortran

  @@fp=nil # fortran parser
  @@np=nil # normalize parser

  def default_props
    {:debug=>false,:incdirs=>[],:nl=>true,:normalize=>false,:srcfile=>nil}
  end
  
  def directive
    unless @directive
      f=File.join(File.dirname(File.expand_path($0)),"sentinels")
      d=File.open(f,"rb").read.gsub(/\$/,'\$').split("\n").push('sms\$').join("|")
      @directive=Regexp.new("^\s*!((#{d}).*)",true)
    end
    @directive
  end

  def fail(msg)
    $stderr.puts "\n#{msg}\n"
    unless __FILE__=="(irb)"
      puts "\n"
      exit 1
    end
  end

  def normalize(s,newline)
    @@np||=NormalizeParser.new
    s=s.gsub(directive,'@\1')             # hide directives
    s=s.gsub(/^\s+/,"")                   # remove leading whitespace
    s=s.gsub(/[ \t]+$/,"")                # remove trailing whitespace
    s=s.gsub(/^!.*\n/,"")                 # remove full-line comments
    s=@@np.parse(@@np.parse(s).to_s).to_s # two normalize passes
    s=s.sub(/^\n+/,"")                    # remove leading newlines
    s+="\n" if s[-1]!="\n" and newline    # ensure final newline
    s=s.gsub(/^@(.*)/i,'!\1')             # show directives
  end

  def out(s,root=:program_units,override={})
    props=default_props.merge(override)
    translated_source,raw_tree,translated_tree=process(s,root,props)
    translated_source
  end

  def process(s,root=:program_units,override={})

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
            fail(msg)
          end
          unless File.readable?(incfile)
            msg="Could not read file #{incfile} "
            msg+=incchain(seen,incfile)
            fail(msg)
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

    def incchain(seen,incfile)
      "\n  "+(seen+[incfile]).join(" includes\n  ")
    end

    def wrap(s)

      def directive?(s)
        s=~directive
      end

      max=80
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

    props=default_props.merge(override)
    debug=props[:debug]
    @@fp||=FortranParser.new
    s=s.gsub(/^\s*!sms\$insert */i,"")                           # process inserts
    s=s.gsub(/^\s*!sms\$remove +begin.*?!sms\$remove +end/im,"") # process removes
    s=assemble(s,[props[:srcfile]],props[:incdirs])
    cppcheck(s)
    puts "RAW SOURCE\n\n#{s}\n" if debug
    puts "NORMALIZED SOURCE\n\n" if debug
    s=normalize(s,props[:nl])
    unless props[:normalize]
      puts s if debug
      @@incdirs=props[:incdirs]
      raw_tree=@@fp.parse(s,:root=>root)
      if debug
        puts "\nRAW TREE\n\n"
        p raw_tree
      end
      re=Regexp.new("^(.+?):in `([^\']*)'$")
      srcmsg=(re.match(caller[0])[2]=="raw")?(": See #{caller[1]}"):("")
      fail "PARSE FAILED#{srcmsg}" if raw_tree.nil?
      translated_tree=raw_tree.translate
      if debug
        puts "\nTRANSLATED TREE\n\n"
        p translated_tree
      end
      s=wrap(translated_tree.to_s)
      puts "\nTRANSLATED SOURCE\n\n" if debug
    end
    [s,raw_tree,translated_tree]
  end

  def raw(s,root=:program_units,override={})
    props=default_props.merge(override)
    s,raw_tree,translated_tree=process(s,root,props)
    raw_tree
  end

  def tree(s,root=:program_units,override={})
    props=default_props.merge(override)
    s,raw_tree,translated_tree=process(s,root,props)
    translated_tree
  end

end

# paul.a.madden@noaa.gov
