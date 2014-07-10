require "exceptions"
require "sentinels"

module Common

  include Sentinels

  def directive
    return @@directive if defined?(@@directive)
    s=sentinels.map { |x| x.gsub(/\$/,'\$') }.join("|")
    @@directive=Regexp.new("^\s*!((#{s}).*)",true)
  end

  def wrap(s,alt=nil)

    def directive?(s)
      s=~directive
    end

    maxcols=132 # columns
    maxcont=39  # continuation lines
    a=s.split("\n")
    (0..a.length-1).each do |n|
      cont=0
      e=a[n].chomp
      unless directive?(e) and not alt
        if e.length>maxcols
          e=~/^( *).*$/
          i=$1.length+2
          t=""
          begin
            r=[maxcols-2,e.length-1].min
            t+=e[0..r]+"&\n"
            prefix=(alt)?(alt):(" "*i+"&")
            e=prefix+e[r+1..-1]
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

  def array_attrs(array_spec,_attrs,distribute)
    dims=0
    array_spec.abstract_boundslist.each_index do |i|
      arrdim=i+1
      _attrs["lb#{arrdim}"]=array_spec.abstract_boundslist[i].alb
      _attrs["ub#{arrdim}"]=array_spec.abstract_boundslist[i].aub
      if distribute and (x=distribute["dim"].index(arrdim))
        decdim=x+1
        _attrs["decomp"]=distribute["decomp"]
        _attrs["dim#{arrdim}"]=decdim
      end
      dims+=1
    end
    _attrs["dims"]||=dims
    _attrs
  end

  def attrchk(node,attr)
    node.respond_to?(attr) && node.send(attr)
  end

  def deepcopy(o)
    Marshal.load(Marshal.dump(o))
  end

  def envext
    ".sms"
  end

  def envfile(m,d)
    File.join(File.expand_path(d),"#{m}#{envext}")
  end

  def sms_halo_comp
    env[:sms_halo_comp]
  end

  def sms_ignore
    env[:sms_ignore]
  end

  def sms_parallel
    env[:sms_parallel]
  end

  def sms_serial
    env[:sms_serial]
  end

  def sms_serial_info
    env[:sms_serial_info]
  end

  def sms_to_local
    env[:sms_to_local]
  end

  def use_localnames(modulename)
    e=(is_a?(Fortran::T))?(use_part.env):(env)
    return [] unless e[:uses]
    e[:uses][modulename].map { |x| x[0] }
  end

  def use_part
    specification_part.e[0]
  end

  def uses?(modname,usename)
    e=(is_a?(Fortran::T))?(use_part.env):(env)
    return false unless e[:uses]
    (e[:uses][modname])?(use_localnames(modname).include?(usename)):(false)
  end

  def write_envfile(modulename,_env)
    modinfo=deepcopy(_env)
    # Do not export symbol keys, which are for internal purposes only
    modinfo.delete_if { |k,v| k.is_a?(Symbol) }
    # Do not export info on private objects
    modinfo.delete_if { |k,v| v["access"]=="private" }
    d=env[:global][:dstfile]
    d=File.dirname(d) unless File.directory?(d)
    unless File.directory?(d)
      $stderr.puts "Output directory '#{d}' not found"
      raise Exceptions::TranslatorException
    end
    f=envfile("#{modulename}",d)
    begin
      File.delete(f) if File.exist?(f)
      unless modinfo.empty?
        File.open(f,"w") { |f| f.write(YAML.dump(modinfo)) }
      end
    rescue Exception=>ex
      $stderr.puts "Could not write module file '#{f}'"
      raise Exceptions::TranslatorException
    end
  end

end
