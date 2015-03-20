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

  def nest_check(inner,outer,cond)
    efail "#{inner} may not appear inside #{outer}" if cond
  end

  def deepcopy(o)
    Marshal.load(Marshal.dump(o))
  end

  def efail(msg)
    fail "#{msg}"
  end

  def envext
    ".sms"
  end

  def envfile(m,d)
    File.join(File.expand_path(d),"#{m}#{envext}")
  end

  def ifail(msg)
    fail "INTERNAL ERROR: #{msg}"
  end

  def sms_halo_comp(node=self)
    node.env[:sms_halo_comp]
  end

  def sms_ignore(node=self)
    node.env[:sms_ignore]
  end

  def sms_parallel(node=self)
    node.env[:sms_parallel]
  end

  def sms_serial(node=self)
    node.env[:sms_serial]
  end

  def sms_serial_info(node=self)
    node.env[:sms_serial_info]
  end

  def sms_to_local(node=self)
    node.env[:sms_to_local]
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

end
