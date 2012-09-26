module PPP

  require 'treetop'
  require 'fortran'
  require 'fortran_parser'
  require 'normalize'
  require 'normalize_parser'

  include Fortran

  def assemble(s,seen,incdirs=[])
    current=seen.last
    a=''
    r=Regexp.new('^\s*include\s*(\'[^\']+\'|\"[^\"]+\").*',true)
    s.split("\n").each do |line|
      m=r.match(line)
      if m
        incfile=m[1][1..-2]
        if incfile[0]=='/' or incfile[0]=='.'
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
        a+=assemble(File.open(incfile,'rb').read,seen+[incfile],incdirs)
      else
        a+="#{line}\n"
      end
    end
    a
  end

  def cppcheck(s)
    r=Regexp.new('^\s*#')
    i=1
    s.split("\n").each do |line|
      m=r.match(line)
      fail "Detected cpp directive:\n\n#{i}: #{line.strip}" if m
      i+=1
    end
  end

  def defprops
    {:debug=>false,:incdirs=>[],:normalize=>false,:srcfile=>nil}
  end
    
  def directive
    unless @directive
      f=File.join(File.dirname(File.expand_path($0)),'sentinels')
      d=File.open(f,'rb').read.gsub(/\$/,'\$').split("\n").<<('sms\$').join('|')
      @directive=Regexp.new("^\s*!((#{d}).*)",true)
    end
    @directive
  end

  def directive?(s)
    s=~directive
  end

  def fail(msg)
    $stderr.puts "\n#{msg}\n"
    exit 1 if __FILE__==$0
  end

  def incchain(seen,incfile)
    "\n  "+(seen+[incfile]).join(" includes\n  ")
  end

  def normalize(s)
    np=NormalizeParser.new
    s=s.gsub(directive,'@\1')         # hide directives
    s=s.gsub(/^\s+/,'')               # left-justify lines
    s=s.gsub(/^!.*\n/,'')             # remove full-line comments
    s=np.parse(np.parse(s).to_s).to_s # two normalize passes
    s=s.sub(/^\n+/,'')                # remove leading newlines
    s << "\n"  unless s[-1]=="\n"     # ensure final newline
    s=s.gsub(/^@(.*)/i,'!\1')         # show directives
  end

  def out(s,root=:program_units,props=defprops)
    s,tree=process(s,root,props)
    s
  end

  def process(s,root=:program_units,props=defprops)
    debug=props[:debug]
    fp=FortranParser.new
    s=s.gsub(/^\s*!sms\$insert */i,'')                           # process inserts
    s=s.gsub(/^\s*!sms\$remove +begin.*?!sms\$remove +end/im,'') # process removes
    s=assemble(s,[props[:srcfile]],props[:incdirs])
    cppcheck(s)
    puts "normalized:\n\n" if debug
    s=normalize(s)
    unless props[:normalize]
      puts s if debug
      puts "\nparsed:\n\n" if debug
      tree=fp.parse(s,:root=>root)
      p tree if debug
      s=tree.to_s
      fail "Parse failed." if s.empty?
      s=wrap(s)
    end
    [s,tree]
  end

  def tree(s,root=:program_units,props=defprops)
    s,tree=process(s,root,props)
    tree
  end
  
  def wrap(s)
    max=80
    a=s.split("\n")
    (0..a.length-1).each do |n|
      e=a[n].chomp
      unless directive?(e)
        if e.length>max
          e=~/^( *).*$/
          i=$1.length+2
          t=''
          begin
            r=[max-2,e.length-1].min
            t+=e[0..r]+"&\n"
            e=' '*i+'&'+e[r+1..-1]
          end while e.length>max
          t+=e
          a[n]=t
        end
      end
    end
    a.join("\n")
  end

end

# paul.a.madden@noaa.gov
