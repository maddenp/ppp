require "set"

module Fortran

  def sp_do_construct(do_construct)
    iterator=do_construct.do_stmt.do_variable
    if (p=env[:sms_parallel])
      p.vars.each do |x|
        if x.include?("#{iterator}")
          do_construct.metadata[:parallel]=true
        end
      end
    end
    true
  end

  def sp_sms_distribute_begin(sms_decomp_name,sms_distribute_dims)

    # Do not push an environment here. The declarations that appear inside a
    # distribute region belong to the environment belonging to the enclosing
    # scoping unit.

    fail "ERROR: Already inside sms$distribute region" if @distribute
    @distribute={"decomp"=>"#{sms_decomp_name}","dim"=>[]}
    sms_distribute_dims.dims.each { |x| @distribute["dim"].push(x) }
    true
  end

  def sp_sms_distribute_end

    # Do not pop the environment stack here, because the matching 'begin' does
    # not push one.

    fail "ERROR: Not inside sms$distribute region" unless @distribute
    @distribute=nil
    true
  end

  def sp_sms_halo_comp
    envpop(false)
    true
  end

  def sp_sms_halo_comp_begin(halo_comp_pairs)
    fail "ERROR: sms$halo_comp invalid outside sms$parallel region" unless sms_parallel
    fail "ERROR: Already inside sms$halo_comp region" if sms_halo_comp
    envpush(false)
    dims={}
    dims[1]=halo_comp_pairs.e[0]
    dims[2]=halo_comp_pairs.e[1].e[1] if halo_comp_pairs.e[1].e
    dims[3]=halo_comp_pairs.e[2].e[1] if halo_comp_pairs.e[2].e
    env[:sms_halo_comp]={}
    dims.each { |k,v| sms_halo_comp[k]=OpenStruct.new({:lo=>"#{v.lo}",:up=>"#{v.up}"}) }
    true
  end

  def sp_sms_halo_comp_end
    fail "ERROR: Not inside sms$halo_comp region" unless sms_halo_comp
    true
  end

  def sp_sms_ignore
    envpop(false)
    true
  end

  def sp_sms_ignore_begin
    fail "ERROR: Already inside sms$ignore region" if sms_ignore
    fail "ERROR: sms$ignore region may not appear inside sms$serial region" if sms_serial
    envpush(false)
    env[:sms_ignore]=true
    true
  end

  def sp_sms_ignore_end
    fail "ERROR: Not inside sms$ignore region" unless sms_ignore
    true
  end

  def sp_sms_parallel
    envpop(false)
    true
  end

  def sp_sms_parallel_begin(sms_decomp_name,sms_parallel_var_lists)
    fail "ERROR: Already inside sms$parallel region" if sms_parallel
    envpush(false)
    env[:sms_parallel]=OpenStruct.new({:decomp=>"#{sms_decomp_name}",:vars=>sms_parallel_var_lists.vars})
    true
  end

  def sp_sms_parallel_end
    fail "ERROR: Not inside sms$parallel region" unless sms_parallel
    true
  end

  def sp_sms_serial
    envpop(false)
    true
  end

  def sp_sms_serial_begin
    fail "ERROR: Already inside sms$serial region" if sms_serial
    envpush(false)
    env[:sms_serial]=true
    true
  end

  def sp_sms_serial_end
    fail "ERROR: Not inside sms$serial region" unless sms_serial
    true
  end

  def sp_sms_to_local
    envpop(false)
    true
  end

  def sp_sms_to_local_begin(sms_decomp_name,sms_to_local_lists)
    fail "ERROR: Already inside sms$to_local region" if sms_to_local
    envpush(false)
    env[:sms_to_local]=sms_to_local_lists.vars.each do |var,props|
      props.dh="#{sms_decomp_name}"
    end
    true
  end

  def sp_sms_to_local_end
    fail "ERROR: Not inside sms$to_local region" unless sms_to_local
    true
  end

  # Modules

  module Array_Translation

    def translate

      def getbound(var,dim,lu,cb=nil)
        varenv=env[var]
        return "#{cb}" unless (dd=decdim(varenv,dim))
        dh=varenv["decomp"]
        nl="#{dh}__nestlevel"
        a1="#{dh}__#{(lu==:l)?('s'):('e')}1"
        a2="#{dh}__#{(lu==:l)?('low'):('upper')}bounds(#{dd},#{nl})"
        "#{a1}("+((cb)?("#{cb}"):("#{a2}"))+",0,#{nl})"
      end

      return if sms_ignore or sms_parallel_loop or sms_serial
      var="#{name}"
      if inside?(Assignment_Stmt,Where_Construct,Where_Stmt)
        if (fn=ancestor(Function_Reference))           # we're an actual arg
          return if not (treatment=intrinsic(fn.name)) # fn is not intrinsic
          return if treatment==:complete               # complete array arg ok
        end
        fail "ERROR: '#{var}' not found in environment" unless (varenv=env[var])
        return unless varenv["decomp"]
        if defined?(treatment) and treatment==:error
          fail "ERROR: Distributed-arrary argument '#{var}' incompatible with intrinsic procedure '#{fn.name}'"
        end
        bounds=[]
        sl=subscript_list
        (1..varenv["dims"]).each do |dim|
          if sl and (s=sl[dim-1])
            if s.is_a?(Subscript)
              bounds[dim-1]="#{s.subscript}"
            elsif s.is_a?(Subscript_Triplet)
              b=getbound(var,dim,:l,s.lower)+":"+getbound(var,dim,:u,s.upper)
              b+=":#{s.stride}" if s.stride and "#{s.stride}"!="1"
              bounds[dim-1]=b
            elsif s.is_a?(Vector_Subscript)
              fail "INTERNAL ERROR: in Array_Translation#translate: Please report to developers."
            else
              bounds[dim-1]="#{s}"
            end
          else
            bounds[dim-1]=getbound(var,dim,:l)+":"+getbound(var,dim,:u)
          end
        end
        boundslist=(1..varenv["dims"]).map{ |dim| bounds[dim-1] }.join(",")
        code="#{var}(#{boundslist})"
        replace_element(code,:variable)
      elsif (iostmt=ancestor(Io_Stmt))
        if known_distributed(var)
          subscript="#{self}".sub(/^#{Regexp.escape(var)}/,"")
          iostmt.register_io_var(:globals,var)
          code=sms_global_name(var)+subscript
          replace_element(code,:expr)
        else
          iostmt.register_io_var(:locals,var)
        end
      end
    end

  end

  # Generic classes

  class T < Treetop::Runtime::SyntaxNode

    def code_alloc_dealloc_globals(globals)
      code_alloc=[]
      code_dealloc=[]
      globals.sort.each do |var|
        varenv=varenv_get(var)
        d=(":"*varenv["dims"]).split("")
        t=varenv["type"]
        k=varenv["kind"]
        l=(t=="character")?("len(#{var})"):(nil)
        props={:attrs=>["allocatable"],:dims=>d,:kind=>k,:len=>l}
        declare(t,sms_global_name(var),props)
        dims=varenv["dims"]
        bounds_root=[]
        (1..dims).each do |i|
          bounds_root.push((decdim(varenv,i))?("#{fixbound(varenv,var,i,:l)}:#{fixbound(varenv,var,i,:u)}"):("lbound(#{var},#{i}):ubound(#{var},#{i})"))
        end
        bounds_root=bounds_root.join(",")
        bounds_nonroot=("1"*dims).split("").join(",")
        gvar=sms_global_name(var)
        svar=sms_statusvar
        code_alloc.push("if (#{sms_rootcheck}()) then")
        code_alloc.push("allocate(#{gvar}(#{bounds_root}),stat=#{svar})")
        code_alloc.push("else")
        code_alloc.push("allocate(#{gvar}(#{bounds_nonroot}),stat=#{svar})")
        code_alloc.push("endif")
        code_alloc.push(check_allocate(gvar,svar))
        code_dealloc.push("deallocate(#{gvar},stat=#{svar})")
        code_dealloc.push(check_deallocate(gvar,svar))
      end
      [code_alloc,code_dealloc]
    end

    def check_allocate(datavar,statusvar)
      check_op(statusvar,1,"Allocation of '#{datavar}' failed")
    end

    def check_deallocate(datavar,statusvar)
      check_op(statusvar,1,"Deallocation of '#{datavar}' failed")
    end

    def check_op(statusvar,retcode,msg)
      declare("integer",sms_rankvar)
      code=""
      code+="if (#{statusvar}.ne.0) then\n"
      code+="call sms__comm_rank(#{sms_rankvar})\n"
      code+="write (*,'(a,i0)') \"#{msg} on MPI rank \",#{sms_rankvar}\n"
      code+="#{sms_abort(retcode)}\n"
      code+="endif"
    end

    def code_bcast(vars,iostat=nil)
      code=[]
      vars.each do |var|
        varenv=varenv_get(var)
        sort=varenv["sort"]
        type=varenv["type"]
        if type=="character"
          arg2=(sort=="scalar")?("1"):("size(#{var})")
          code.push("if (#{iostat}.eq.0) then") if iostat
          code.push(sms_bcast_char(var,arg2))
          code.push(sms_chkstat)
          code.push("endif") if iostat
        else
          if sort=="scalar"
            dims="1"
            sizes="(/1/)"
          else
            dims=varenv["dims"]
            sizes="(/"+(1..dims.to_i).map { |r| "size(#{var},#{r})" }.join(",")+"/)"
          end
          kind=varenv["kind"]
          code.push("if (#{iostat}.eq.0) then") if iostat
          code.push(sms_bcast(var,sms_type(var),sizes,dims))
          code.push(sms_chkstat)
          code.push("endif") if iostat
        end
      end
      code
    end

    def code_decomp(dh,sort)
      unless sort==:array or sort==:scalar
        fail "ERROR: sort must be :array or :scalar (was '#{sort}')"
      end
      dh=(dh)?("#{dh}(#{dh}__nestlevel)"):("sms__not_decomposed")
      dh="(/#{dh}/)" if sort==:array
      dh
    end

    def code_gather(vars)
      code=[]
      vars.each do |var|
        varenv=varenv_get(var)
        dh=varenv["decomp"]
        dims=varenv["dims"]
        type=code_type(var,varenv,:array)
        gllbs=code_global_lower_bounds(varenv,var,dims)
        glubs=code_global_upper_bounds(varenv,var,dims)
        gstrt=gllbs
        gstop=glubs
        perms=code_perms(varenv)
        decomp=code_decomp(dh,:array)
        args=[]
        args.push("#{maxrank}")
        args.push("1")
        args.push("#{gllbs}")
        args.push("#{glubs}")
        args.push("#{gstrt}")
        args.push("#{gstop}")
        args.push("#{perms}")
        args.push("#{decomp}")
        args.push("#{type}")
        args.push(".false.") # but why?
        args.push("#{var}")
        args.push(sms_global_name(var))
        args.push(sms_statusvar)
        code.push("call sms__gather(#{args.join(",")})")
        code.push(sms_chkstat)
      end
      code
    end

    def code_global_lower_bounds(varenv,var,dims)
      if dims
        "(/"+ranks.map { |r| (r>dims)?(1):(fixbound(varenv,var,r,:l)) }.join(",")+"/)"
      else
        "(/"+ranks.map { |r| 1 }.join(",")+"/)"
      end
    end

    def code_global_upper_bounds(varenv,var,dims)
      if dims
        "(/"+ranks.map { |r| (r>dims)?(1):(fixbound(varenv,var,r,:u)) }.join(",")+"/)"
        else
        "(/"+ranks.map { |r| 1 }.join(",")+"/)"
      end
    end

    def code_local_bound(dh,dd,lu)
      fail "ERROR: lu must be :l or :u (was '#{lu}')" unless lu==:l or lu==:u
      "#{dh}__local_#{lu}b(#{dd},#{dh}__nestlevel)"
    end

    def code_perms(varenv)
      "(/"+ranks.map { |r| decdim(varenv,r)||0 }.join(",")+"/)"
    end

    def code_scatter(vars,iostat=nil)
      code=[]
      vars.each do |var|
        tag=sms_commtag
        varenv=varenv_get(var)
        dh=varenv["decomp"]
        dims=varenv["dims"]
        type=code_type(var,varenv,:array)
        gllbs=code_global_lower_bounds(varenv,var,dims)
        glubs=code_global_upper_bounds(varenv,var,dims)
        gstrt=gllbs
        gstop=glubs
        halol="(/"+ranks.map { |r| (dd=decdim(varenv,r))?("#{dh}__halosize(#{dd},#{dh}__nestlevel)"):("0") }.join(",")+"/)"
        halou="(/"+ranks.map { |r| (dd=decdim(varenv,r))?("#{dh}__halosize(#{dd},#{dh}__nestlevel)"):("0") }.join(",")+"/)"
        perms=code_perms(varenv)
        decomp=code_decomp(dh,:array)
        args=[]
        args.push("#{maxrank}")
        args.push("1")
        args.push("#{tag}")
        args.push("#{gllbs}")
        args.push("#{glubs}")
        args.push("#{gstrt}")
        args.push("#{gstop}")
        args.push("#{perms}")
        args.push("#{halol}")
        args.push("#{halou}")
        args.push("#{decomp}")
        args.push("#{type}")
        args.push(sms_global_name(var))
        args.push("#{var}")
        args.push(sms_statusvar)
        stmt=""
        stmt+="if (#{iostat}.eq.0) " if iostat
        stmt+="call sms__scatter(#{args.join(",")})"
        code.push(stmt)
        code.push(sms_chkstat)
      end
      code
    end

    def code_type(var,varenv,sort)
      unless sort==:array or sort==:scalar
        fail "ERROR: sort must be :array or :scalar (was '#{sort}')"
      end
      code=""
      code+="(/" if sort==:array
      code+=sms_type(var)
      code+="/)" if sort==:array
      code
    end

    def decdim(varenv,r)
      varenv["dim#{r}"]
    end

    def declare(type,var,props={})
      su=scoping_unit
      varenv=varenv_get(var,su,false)
      if varenv
        fail "ERROR: Variable #{var} is already defined" unless varenv["pppvar"]
      else
        varenv_del(var,su,false)
        lenopt=""
        if props[:len]
          unless type=="character"
            fail "ERROR: 'len' property incompatible with type '#{type}'"
          end
          lenopt="(len=#{props[:len]})"
        end
        kind=props[:kind]
        kind=([nil,"default"].include?(kind))?(""):("(kind=#{kind})")
        attrs=props[:attrs]||[]
        attrs=[attrs] unless attrs.is_a?(Array)
        attrs=(attrs.empty?)?(""):(",#{attrs.sort.join(",")}")
        code="#{type}#{kind}#{lenopt}#{attrs}::#{var}"
        dims=props[:dims]
        code+="(#{dims.join(',')})" if dims
        init=props[:init]
        code+="=#{init}" if init
        t=raw(code,:type_declaration_stmt,input.srcfile,input.dstfile,{:env=>env})
        newenv=t.input.envstack.last
        newenv[var]["pppvar"]=true
        dc=declaration_constructs
        t.parent=dc
        dc.e.push(t)
        node=self
        begin
          node.envref[var]=newenv[var] if node.respond_to?(:envref)
          break if node==su
          node=node.parent
        end while true
        if su.is_a?(Module)
          write_envfile(su.name,su.envref)
        end
      end
      var
    end

    def distribute_array_bounds(spec_list,varenv)
      return unless spec_list
      return unless dh=varenv["decomp"]
      ok=[Assumed_Shape_Spec_List,Explicit_Shape_Spec_List,Implicit_Shape_Spec_List]
      return unless ok.include?(spec_list.class)
      newbounds=[]
      if spec_list.is_a?(Explicit_Shape_Spec_List)
        cb=spec_list.concrete_boundslist
        cb.each_index do |i|
          b=cb[i]
          arrdim=i+1
          if (dd=decdim(varenv,arrdim))
            use(sms_decompmod)
            s=code_local_bound(dh,dd,:l)+":"+code_local_bound(dh,dd,:u)
          else
            s=(b.clb=="1")?(b.cub):("#{b.clb}:#{b.cub}")
          end
          newbounds.push(s)
        end
      elsif spec_list.is_a?(Assumed_Shape_Spec_List) or spec_list.is_a?(Implicit_Shape_Spec_List)
        (1..varenv["dims"].to_i).each do |i|
          arrdim=i
          if (dd=decdim(varenv,arrdim)) and not varenv["lb#{arrdim}"]=="deferred"
            use(sms_decompmod)
            s=code_local_bound(dh,dd,:l)+":"
          else
            s=":"
          end
          newbounds.push(s)
        end
      end
      code=newbounds.join(",")
      replace_element(code,:array_spec,spec_list)
    end

    def fixbound(varenv,var,dim,x)
      bound=varenv["#{x}b#{dim}"]
      fail "ERROR: Bad upper bound: #{bound}" if bound=="default" and x==:u
      return 1 if bound=="default" and x==:l
      if ["assumed","deferred","explicit"].include?(bound)
        if (dd=decdim(varenv,dim))
          dh=varenv["decomp"]
          lu=(x==:l)?("low"):("upper")
          return "#{dh}__#{lu}bounds(#{dd},#{dh}__nestlevel)"
        else
          return "#{x}bound(#{var},#{dim})"
        end
      end
      bound
    end

    def halo_offsets(dd)
      halo_lo=0
      halo_up=0
      if halocomp=sms_halo_comp
        offsets=halocomp[dd]
        halo_lo=offsets.lo
        halo_up=offsets.up
      end
      OpenStruct.new({:lo=>"#{halo_lo}",:up=>"#{halo_up}"})
    end

    def intrinsic(function_name)
      intrinsics["#{function_name}"]
    end

    def known(var)
      varenv_get(var,self,expected=false)
    end

    def known_array(var)
      (varenv=known(var)) and varenv["sort"]=="array"
    end

    def known_distributed(var)
      (varenv=known(var)) and varenv["decomp"]
    end

    def known_pppvar(var)
      (varenv=known(var)) and varenv["pppvar"]
    end

    def known_scalar(var)
      (varenv=known(var)) and varenv["sort"]=="scalar"
    end

    def known_uservar(var)
      (varenv=known(var)) and not varenv["pppvar"]
    end

    def marker
      s=env[:global]
      s[:marker]||=0
      m=(s[:marker]+=1)
      f=File.basename(env[:global][:dstfile])
      "#{f} marker #{m}"
    end

    def maxrank
      7
    end

    def ranks
      (1..maxrank)
    end

    def sms(s)
      "#{e[0]}#{e[1]} #{s}\n"
    end

    def sms_abort(retcode,msg=nil)
      use(sms_decompmod)
      msg="#{marker}"+((msg)?(" [ #{msg} ]"):(""))
      "call sms__abort(#{retcode},'#{msg}')"
    end

    def sms_bcast(var,type,sizes,dims)
      "call sms__bcast(#{var},#{type},#{sizes},#{dims},#{sms_statusvar})"
    end

    def sms_bcast_char(var,arg2)
      "call sms__bcast_char(#{var},#{arg2},#{sms_statusvar})"
    end

    def sms_chkstat
      "call sms__chkstat('#{marker}',' ',#{sms_statusvar},sms__abort_on_error,#{sms_statusvar})"
    end

    def sms_commtag
      s=env[:global]
      s[:tag]||=-1
      name="sms__tag_#{s[:tag]+=1}"
      declare("integer",name,{:attrs=>"save",:init=>"0"})
      name
    end

    def sms_decompmod
      "sms__decomp"
    end

    def sms_global_name(name)
      p="sms__global_"
      n="#{name}"
      maxnamelen=31
      tokeep=maxnamelen-p.size-1
      return "#{p}#{n}" if n.size<=tokeep
      @@global_names={} unless defined?(@@global_names)
      return @@global_names[n] if @@global_names[n]
      @@global_index=0 unless defined?(@@global_index)
      p="#{p}#{@@global_index+=1}_"
      tokeep=maxnamelen-p.size-1
      @@global_names[n]=p+n[0..tokeep]
    end

    def sms_maxvars
      25
    end

    def sms_parallel_loop(node=self)
      return node if node.is_a?(Do_Construct) and node.metadata[:parallel]
      while (node=node.ancestor(Do_Construct))
        return node if node.metadata[:parallel]
      end
      nil
    end

    def sms_rankvar
      "sms__rank"
    end

    def sms_rootcheck
      "sms__i_am_root"
    end

    def sms_serial_region(node=self)
      return node if node.is_a?(SMS_Serial)
      node.ancestor(SMS_Serial)
    end

    def sms_statusvar
      "sms__status"
    end

    def sms_stop
      use(sms_decompmod)
      "call sms__stop"
    end

    def sms_type(var)
      "sms__typeget(#{var})"
    end

  end # class T

  # Out-of-order class definitions (must be defined before subclassed)

  class SMS < NT
  end

  class SMS_Getter < SMS

    def str0
      sms("#{e[2]}#{e[3]}#{e[4]}")
    end

    def translate_with_options(description,function)
      # Check the sort and type of the indicated variable, if it exists in the
      # environment. If not, carry on and hope for the best.
      if (varenv=varenv_get(var,self,false))
        unless varenv["sort"]=="scalar" and varenv["type"]=="integer"
          fail "ERROR: #{description} query's argument must be an integer scalar"
        end
      end
      code=[]
      code.push("#{self}")
      code.push("call #{function}(#{var})")
      replace_statement(code)
    end

    def var
      e[3]
    end

  end

  # Grammar-supporting subclasses

  class Allocate_Shape_Spec < NT

    def translate
      var="#{name}"
      varenv=varenv_get(var,self,false)
      if varenv and (dh=varenv["decomp"])
        use(sms_decompmod)
        arrdim=dim
        if (dd=decdim(varenv,arrdim))
          code=code_local_bound(dh,dd,:l)+":"+code_local_bound(dh,dd,:u)
          replace_element(code,:allocate_shape_spec)
        end
      end
    end

  end

  class Array_Name_And_Spec < NT

    def translate
      var="#{e[0]}"
      spec_list=e[2].spec_list
      varenv=varenv_get(var)
      distribute_array_bounds(spec_list,varenv)
    end

  end

  class Array_Section < NT
    include Array_Translation
  end

  class Array_Variable_Name < Variable_Name
    include Array_Translation
  end

  class Close_Stmt < Io_Stmt

    def translate
      return if sms_ignore or sms_serial
      io_stmt_init
      io_stmt_common
    end

  end

  class Do_Construct < NT

    def translate
      if metadata[:parallel]
        node=raw("sms__in_parallel=.true.",:assignment_stmt,input.srcfile,@dstfile)
        body.e.insert(0,node)
        code=[]
        code.push(self)
        code.push("sms__in_parallel=.false.")
        replace_statement(code)
      end
    end

  end

  class Do_Stmt < Stmt

    def translate
      unless sms_ignore or sms_serial
        if (p=sms_parallel)
          dd=nil
          [0,1,2].each do |i|
            if p.vars[i].include?("#{do_variable}")
              dd=i+1
              break
            end
          end
          if dd
            dh=p.decomp
            halo_lo=halo_offsets(dd).lo
            halo_up=halo_offsets(dd).up
            if loop_control.is_a?(Loop_Control_1)
              code="#{dh}__s#{dd}(#{loop_control.e[3]},#{halo_lo},#{dh}__nestlevel)"
              replace_element(code,:scalar_numeric_expr,loop_control.e[3])
              code=",#{dh}__e#{dd}(#{loop_control.e[4].value},#{halo_up},#{dh}__nestlevel)"
              replace_element(code,:loop_control_pair,loop_control.e[4])
              use(sms_decompmod)
            end
          end
        end
      end
    end

  end

  class Entity_Decl_1 < Entity_Decl

    def translate
      var="#{name}"
      varenv=varenv_get(var)
      spec_list=nil
      if varenv["sort"]=="array"
        if (entity_decl_array_spec=e[1]).is_a?(Entity_Decl_Array_Spec)
          # entity_decl_array_spec case
          spec_list=entity_decl_array_spec.array_spec.spec_list
        else
          attr_spec_option=ancestor(Type_Declaration_Stmt).e[2]
          if attr_spec_option.is_a?(Attr_Spec_Option)
            if (d=attr_spec_option.dimension?)
              # dimension attribute case
              spec_list=d.spec_list
            end
          end
        end
      end
      distribute_array_bounds(spec_list,varenv)
    end

  end

  class Entry_Stmt < Stmt

    def translate
      fail "ERROR: 'entry' statement may not appear inside sms$serial region" if sms_serial
    end

  end

  class If_Stmt < Stmt

    def translate
      code=[]
      code.push("#{sa(label)}#{prefix} then")
      code.push("#{action}")
      code.push("endif")
      replace_statement(code)
    end

  end

  class Io_Spec

    def pppvar_prefix
      "sms__io_"
    end

  end

  class Io_Spec_End < Io_Spec

    def pppvar
      declare("logical","#{pppvar_prefix}end")
    end

  end

  class Io_Spec_Eor < Io_Spec

    def pppvar
      declare("logical","#{pppvar_prefix}eor")
    end

  end

  class Io_Spec_Err < Io_Spec

    def pppvar
      declare("logical","#{pppvar_prefix}err")
    end

  end

  class Io_Stmt < NT

    def add_serial_region_vars(serial_intent)
      if (namelist_name=nml)
        nmlenv=varenv_get(namelist_name,self,expected=true)
        nmlenv["objects"].each do |x|
          varenv=varenv_get(x,self,expected=true)
          if serial_intent==:out or varenv["decomp"]
            sms_serial_info.vars_in_region.add([x,serial_intent])
          end
        end
      end
    end

    def io_stmt_bcasts
      @need_decompmod=true unless @var_bcast.empty?
      @code_bcast.concat(code_bcast(@var_bcast,@iostat))
    end

    def io_stmt_branch_to_logic
      [
        :err,
        :end,
        :eor
      ].each do |x|
        # :err has precedence, per F90 9.4.1.6, 9.4.1.7
        if (spec=send(x))
          label_old,label_new=spec.send(:relabel)
          pppvar=spec.send(:pppvar)
          @spec_var_false.push("#{pppvar}=.false.")
          @spec_var_bcast.push(sms_bcast(pppvar,sms_type(pppvar),"(/1/)",1))
          @spec_var_bcast.push(sms_chkstat)
          @spec_var_true.push("#{label_new} #{pppvar}=.true.")
          @spec_var_goto.push("if (#{pppvar}) goto #{label_old}")
          @success_label=label_create unless @success_label
          @need_decompmod=true
        end
      end
    end

    def io_stmt_codegen
      use(sms_decompmod) if @need_decompmod
      code=[]
      if @onroot
        my_label=(label.empty?)?(nil):(label)
        my_label=label_delete if my_label
        code.push("#{my_label} continue") if my_label
      end
      code.concat(@code_alloc)
      code.concat(@code_gather)
      code.push("if (#{sms_rootcheck}()) then") if @onroot
      code.concat(@spec_var_false)
      code.push("#{self}".chomp)
      code.push("goto #{@success_label}") if @success_label
      code.concat(@spec_var_true)
      code.push("#{sa(@success_label)}endif") if @onroot
      code.concat(@spec_var_bcast)
      code.concat(@spec_var_goto)
      code.concat(@code_scatter)
      code.concat(@code_bcast)
      code.concat(@code_dealloc)
      replace_statement(code)
    end

    def io_stmt_common(treatment=nil)
      if treatment
        unless [:in,:out].include?(treatment)
          fail "INTERNAL ERROR: treatment '#{treatment}' neither :in nor :out"
        end
        globals=metadata[:globals]||SortedSet.new
        @onroot=true unless globals.empty?
        @onroot=false if sms_parallel_loop
        globals.each do |global|
          ((treatment==:in)?(@var_gather):(@var_scatter)).add("#{global}")
        end
        if treatment==:out and (locals=metadata[:locals])
          locals.each { |local| @var_bcast.add(local) if @onroot }
        end
      end
      unless is_a?(Print_Stmt)
        io_stmt_branch_to_logic
        io_stmt_var_set_logic
      end
      declare("logical",sms_rootcheck) if @onroot
      @code_alloc,@code_dealloc=code_alloc_dealloc_globals(SortedSet.new(@var_gather+@var_scatter))
      io_stmt_gathers
      io_stmt_scatters
      io_stmt_bcasts
      io_stmt_codegen
    end

    def io_stmt_gathers
      @need_decompmod=true unless @var_gather.empty?
      @code_gather.concat(code_gather(@var_gather))
    end

    def io_stmt_init
      @code_bcast=[]
      @code_gather=[]
      @code_scatter=[]
      @iostat=nil
      @need_decompmod=false
      @onroot=true
      @spec_var_bcast=[]
      @spec_var_false=[]
      @spec_var_goto=[]
      @spec_var_true=[]
      @success_label=nil
      @var_bcast=SortedSet.new
      @var_gather=SortedSet.new
      @var_scatter=SortedSet.new
    end

    def io_stmt_scatters
      @need_decompmod=true unless @var_scatter.empty?
      @code_scatter.concat(code_scatter(@var_scatter,@iostat))
    end

    def io_stmt_var_set_logic
      [
        :access,
        :action,
        :blank,
        :delim,
        :direct,
        :exist,
        :form,
        :formatted,
        :iostat,
        :name,
        :named,
        :nextrec,
        :number,
        :opened,
        :pad,
        :position,
        :read,
        :readwrite,
        :recl,
        :sequential,
        :size,
        :unformatted,
        :write
      ].each do |x|
        if (spec=send(x))
          var=spec.rhs
          varenv=varenv_get(var)
          @spec_var_bcast.push(sms_bcast(var,sms_type(var),"(/1/)",1))
          @spec_var_bcast.push(sms_chkstat)
          @need_decompmod=true
          @iostat=var if x==:iostat
        end
      end
    end

    def register_io_var(key,value)
      (metadata[key]||=SortedSet.new).add(value)
    end

  end

  class Label_Do_Stmt < Do_Stmt
  end

  class Label_Stmt < NT

    def errmsg_parallel(in_parallel_loop)
      io=(in_parallel_loop)?("out"):("in")
      "ERROR: Branch to statement labeled '#{self}' from #{io}side parallel loop"
    end

    def errmsg_serial(in_serial_region)
      io=(in_serial_region)?("out"):("in")
      "ERROR: Branch to statement labeled '#{self}' from #{io}side serial region"
    end

    def translate
      my_serial_region=sms_serial_region
      my_parallel_loop=sms_parallel_loop
      # Handle branches via numeric labels.
      (env[:branch_targets]["#{self}"]||[]).each do |label|
        unless sms_serial_region(label)==my_serial_region
          fail errmsg_serial(my_serial_region)
        end
        unless sms_parallel_loop(label)==my_parallel_loop
          fail errmsg_parallel(my_parallel_loop)
        end
      end
      # Handle branches via assigned goto statements.
      agt=env[:assigned_goto_targets]
      (env[:assign_map]["#{self}"]||[]).each do |var|
        if (targets=agt[var])
          targets.each do |target|
            unless sms_serial_region(target)==my_serial_region
              fail errmsg_serial(my_serial_region)
            end
          end
        end
      end
    end

  end

  class Main_Program < Scoping_Unit

    def translate
      check_static
    end

  end

  class Module < Scoping_Unit

    def translate
      check_static
    end

  end

  class Name < NT

    def globalize
      code=sms_global_name(self)
      replace_element(code,:name)
    end

    def name
      return @name if defined?(@name)
      @name="#{self}"
    end

    def size
      length
    end

    def translate
      # Handle to_local
      if tolocal=sms_to_local and p=tolocal["#{name}"]
        case "#{p.key}"
        when "lbound"
          se="s#{p.dd}"
          halo_offset="#{halo_offsets(p.dd.to_s).lo}"
        when "ubound"
          se="e#{p.dd}"
          halo_offset="#{halo_offsets(p.dd.to_s).up}"
        else
          fail "ERROR: Unrecognized sms$to_local key: #{p.key}"
        end
        code="#{p.dh}__#{se}(#{name},#{halo_offset},#{p.dh}__nestlevel)"
        replace_element(code,:expr)
      end
      # Handle serial
      if sms_serial
        unless inside?(SMS_Serial_Begin,Subroutine_Name) or intrinsic(name) or derived_type? or structure_component?
          varenv=varenv_get(name,self,expected=false)||{}
          unless varenv["subprogram"] or varenv["parameter"]
            sms_serial_info.vars_in_region.add([name,nil])
          end
        end
      end
    end

  end

  class Nonlabel_Do_Stmt < Do_Stmt
  end

  class OMP_Parallel_Do < NT
  end

  class OMP_Parallel_Do_Begin < NT

    def str0
      "#{e[0]} #{e[1]} #{e[2]} #{e[3]}"
    end

  end

  class OMP_Parallel_Do_End < NT

    def str0
      "#{e[0]} #{e[1]} #{e[2]} #{e[3]} #{e[4]}"
    end

  end

  class Open_Stmt < Io_Stmt

    def translate
      return if sms_ignore or sms_serial
      io_stmt_init
      io_stmt_common
    end

  end

  class Print_Stmt < Io_Stmt

    def translate
      return if sms_ignore or sms_serial
      io_stmt_init
      io_stmt_common(:in)
    end

  end

  class Read_Stmt < Io_Stmt

    def translate
      return if sms_ignore
      if sms_serial
        add_serial_region_vars(:out)
      else
        io_stmt_init
        @onroot=false if unit.is_a?(Internal_File_Unit)
        if (namelist_name=nml)
          @onroot=true
          nmlenv=varenv_get(namelist_name,self,expected=true)
          nmlenv["objects"].each do |x|
            var=(x.respond_to?(:name))?("#{x.name}"):("#{x}")
            if (varenv=varenv_get(var,self,expected=false))
              if varenv["decomp"]
                @var_scatter.add(var)
                replace_input_item(x,sms_global_name(var))
              else
                @var_bcast.add(var)
              end
            end
          end
        end
        input_items.each do |x|
          var="#{x}"
          if known_scalar(var) and known_uservar(var)
            @var_bcast.add(var) if @onroot
          end
        end
        io_stmt_common(:out)
      end
    end

  end

  class Scoping_Unit < NT

    def check_static

      # Only for use with Main_Program & Module. Iterate over environment items,
      # skipping any whose keys are symbols (i.e. ppp metadata, not program
      # variables), are scalars or are not decomposed. If any explicit bounds
      # are found, exit with error. In the declaration sections of main programs
      # or modules, distributed arrays must be allocatable: Their translated
      # bounds contain references to non-static data structures that have no
      # compile-time values.

      env.each do |k,v|
        next if k.is_a?(Symbol) or not v["sort"]=="array" or not v["decomp"]
        (1..v["dims"].to_i).each do |dim|
          ["lb","ub"].each do |lub|
            if (b=v["#{lub}#{dim}"]) and b=="explicit"
              fail "ERROR: Static distributed array ('#{k}') not supported"
            end
          end
        end
      end
    end

  end

  class SMS_Region < SMS
  end

  class SMS_Barrier < SMS

    def translate
      fail "ERROR: sms$barrier may not appear inside sms$serial region" if sms_serial
      use(sms_decompmod)
      code=[]
      code.push("#{self}")
      code.push("call sms__barrier(#{sms_statusvar})")
      code.push(sms_chkstat)
      replace_statement(code)
    end

  end

  class SMS_Comm_Rank < SMS_Getter

    def translate
      translate_with_options("comm rank","sms__comm_rank")
    end

  end

  class SMS_Comm_Size < SMS_Getter

    def translate
      translate_with_options("comm size","sms__comm_size")
    end

  end

  class SMS_Compare_Var < SMS

    def str0
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]}")
    end

    def translate
      fail "ERROR: sms$compare_var may not appear inside sms$serial region" if sms_serial
      use(sms_decompmod)
      declare("logical","sms__debugging_on")
      var="#{e[3].name}"
      varenv=varenv_get(var)
      dims=varenv["dims"]
      str="#{e[5]}"
      type=code_type(var,varenv,:scalar)
      gllbs=code_global_lower_bounds(varenv,var,dims)
      glubs=code_global_upper_bounds(varenv,var,dims)
      perms=code_perms(varenv)
      dh=code_decomp(varenv["decomp"],:scalar)
      dims||="1"
      code=[]
      code.push("#{self}")
      code.push("if (sms__debugging_on()) then")
      code.push("call sms__compare_var(#{dh},#{var},#{type},#{glubs},#{perms},#{gllbs},#{glubs},#{gllbs},#{dims},'#{var}',#{str},#{sms_statusvar})")
      code.push(sms_chkstat)
      code.push("endif")
      replace_statement(code)
    end

  end

  class SMS_Create_Decomp < SMS

    def decomp
      e[3]
    end

    def global
      e[5].vars
    end

    def halo
      e[7].vars
    end

    def regionsize
      e[8]
    end

    def str0
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]}#{e[7]}#{e[8]}#{e[9]}")
    end

    def translate
      fail "ERROR: sms$create_decomp may not appear inside sms$serial region" if sms_serial
      max=3
      d="#{decomp}"
      n="#{decomp}__nestlevel"
      use(sms_decompmod)
      declare("integer","sms__periodicusedlower",{:dims=>%W[sms__max_decomposed_dims]})
      declare("integer","sms__periodicusedupper",{:dims=>%W[sms__max_decomposed_dims]})
      code=[]
      code.push("#{self}")
      code.push("#{n}=1")
      code.push("#{d}__nregions=1")
      max.times do |i|
        dim=i+1
        g=global[i]
        h=halo[i]
        if g
          code.push("allocate(#{d}__s#{dim}(1:1,0:1,#{d}__maxnests))")
          code.push("allocate(#{d}__e#{dim}(#{g}:#{g},0:1,#{d}__maxnests))")
        end
        code.push("#{d}__globalsize(#{dim},#{n})=#{(g)?(g):(1)}")
        code.push("#{d}__localsize(#{dim},#{n})=0")
        code.push("#{d}__halosize(#{dim},#{n})=#{(h)?(h):(0)}")
        code.push("#{d}__boundarytype(#{dim})=sms__nonperiodic_bdy")
        code.push("#{d}__lowbounds(#{dim},#{n})=1")
      end
      max.times do |i|
        dim=i+1
        code.push("#{d}__upperbounds(#{dim},#{n})=#{d}__globalsize(#{dim},#{n})+#{d}__lowbounds(#{dim},#{n})-1")
      end
      max.times do |i|
        dim=i+1
        g=global[i]
        if g
          code.push("sms__periodicusedlower(:)=#{d}__lowbounds(:,#{dim})")
          code.push("sms__periodicusedupper(:)=#{d}__upperbounds(:,#{dim})")
        end
      end
      code.push("#{d}__decompname='#{d}'")
      args=[
        "sms__decomp_1",
        "#{d}__boundarytype",
        "#{d}__globalsize(1,#{n})",
        "#{d}__halosize(1,#{n})",
        "#{d}__lowbounds(1,#{n})",
        "sms__null_decomp",
        "#{d}__localsize(1,#{n})",
        "sms__periodicusedlower(1)",
        "sms__periodicusedupper(1)",
        code_local_bound(d,1,:l),
        code_local_bound(d,1,:u),
        "#{d}__decompname",
        "#{d}(#{n})",
        "sms__max_decomposed_dims",
        "sms__unstructured",
        "regionsize", # WHAT IS THIS?
        sms_statusvar
      ]
      code.push("call sms__create_decomp(#{args.join(',')})")
      code.push(sms_chkstat)
      code.push("do #{d}__index=0,0")
      args=[
        "#{d}(#{n})",
        "1",
        "#{d}__halosize(1,#{n})-#{d}__index",
        "#{d}__s1(1,#{d}__index,#{n})",
        "#{d}__e1(#{global[0]},#{d}__index,#{n})",
        "1",
        "1",
        "#{d}__nregions",
        sms_statusvar
      ]
      code.push("call sms__loops_op(#{args.join(',')})")
      code.push(sms_chkstat)
      code.push("end do")
      replace_statement(code)
    end

  end

  class SMS_Create_Decomp_Global < SMS

    def vars
      e[1].vars
    end

  end

  class SMS_Create_Decomp_Halo < SMS

    def vars
      e[1].vars
    end

  end

  class SMS_Create_Decomp_Regionsize < SMS

    def regionsize
      e[3]
    end

  end

  class SMS_Declare_Decomp < SMS

    def str0
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]}#{e[7]}")
    end

    def translate
      fail "ERROR: sms$declare_decomp may not appear inside sms$serial region" if sms_serial
      use("sms__module")
      dh="#{e[3]}"
      declare("integer","#{dh}__maxnests",{:attrs=>"parameter",:init=>"1"})
      declare("integer","#{dh}__ppp_max_regions",{:attrs=>"parameter",:init=>"1"})
      declare("character*32","#{dh}__decompname")
      declare("integer","#{dh}__boundarytype",{:dims=>%W[sms__max_decomposed_dims]})
      declare("integer","#{dh}__e1",{:attrs=>["allocatable"],:dims=>%W[: : :]})
      declare("integer","#{dh}__globalsize",  {:dims=>%W[sms__max_decomposed_dims #{dh}__maxnests]})
      declare("integer","#{dh}__halosize",    {:dims=>%W[sms__max_decomposed_dims #{dh}__maxnests]})
      declare("integer","#{dh}__ignore")
      declare("integer","#{dh}__index")
      declare("integer","#{dh}__local_lb",    {:dims=>%W[sms__max_decomposed_dims #{dh}__maxnests]})
      declare("integer","#{dh}__local_ub",    {:dims=>%W[sms__max_decomposed_dims #{dh}__maxnests]})
      declare("integer","#{dh}__localhalosize")
      declare("integer","#{dh}__localsize",   {:dims=>%W[sms__max_decomposed_dims #{dh}__maxnests]})
      declare("integer","#{dh}__lowbounds",   {:dims=>%W[sms__max_decomposed_dims #{dh}__maxnests]})
      declare("integer","#{dh}__nestlevel")
      declare("integer","#{dh}__nestlevels",  {:dims=>%W[#{dh}__maxnests]})
      declare("integer","#{dh}__nregions")
      declare("integer","#{dh}__s1",{:attrs=>["allocatable"],:dims=>%W[: : :]})
      declare("integer","#{dh}__upperbounds", {:dims=>%W[sms__max_decomposed_dims #{dh}__maxnests]})
      declare("integer",dh,{:dims=>%W[1]})
    end

  end

  class SMS_Declare_Decomp_Unstructured_Option < SMS
  end

  class SMS_Decomp_Name < SMS
  end

  class SMS_Distribute < SMS_Region
  end

  class SMS_Distribute_Begin < SMS

    def str0
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]} #{e[7]}")
    end

  end

  class SMS_Distribute_End < SMS

    def str0
      sms("#{e[2]}")
    end

  end

  class SMS_Distribute_Dims_1 < SMS

    def dims
      ["#{e[0]}".to_i,"#{e[2]}".to_i]
    end

  end

  class SMS_Distribute_Dims_2 < SMS

    def dims
      [nil,"#{e[1]}".to_i]
    end

  end

  class SMS_Distribute_Dims_3 < SMS

    def dims
      ["#{e[0]}".to_i,nil]
    end

  end

  class SMS_Exchange < SMS

    def str0
      sms("#{e[2]}#{e[3]}#{e[4].e.reduce("") { |m,x| m+="#{x.e[0]}#{x.e[1]}" }}#{e[5]}")
    end

    def translate

      fail "ERROR: sms$exchange may not appear inside sms$serial region" if sms_serial

      use(sms_decompmod)
      v=e[4].e.reduce([e[3]]) { |m,x| m.push(x.e[1]) }
      nvars=v.size
      maxnamelen=v.reduce(0) { |m,x| m=(x.name.length>m)?(x.name.length):(m) }
      code=[]

      code.push("#{self}")

      pre="sms__exchange_"
      gllbs="#{pre}gllbs"
      glubs="#{pre}glubs"
      strts="#{pre}gstrt"
      stops="#{pre}gstop"
      perms="#{pre}perms"
      types="#{pre}types"
      dcmps="#{pre}dcmps"
      names="#{pre}names"

      (nvars+1..25).each { |x| declare("integer","sms__x#{x}",{:dims=>%W[1],:init=>"0"}) }

      declare("integer",gllbs,{:dims=>%W[#{maxrank} #{sms_maxvars}]})
      declare("integer",glubs,{:dims=>%W[#{maxrank} #{sms_maxvars}]})
      declare("integer",strts,{:dims=>%W[#{maxrank} #{sms_maxvars}]})
      declare("integer",stops,{:dims=>%W[#{maxrank} #{sms_maxvars}]})
      declare("integer",perms,{:dims=>%W[#{maxrank} #{sms_maxvars}]})
      declare("integer",dcmps,{:dims=>%W[           #{sms_maxvars}]})
      declare("integer",types,{:dims=>%W[           #{sms_maxvars}]})

      declare("character(len=32)",names,{:dims=>%W[#{sms_maxvars}]})

      code.push("#{gllbs}= 1")
      code.push("#{glubs}= 1")
      code.push("#{strts}= 1")
      code.push("#{stops}= 1")
      code.push("#{perms}= 0")
      code.push("#{types}=-1")
      code.push("#{dcmps}=-1")
      code.push("#{names}=char(0)")

      (0..nvars-1).each do |i|
        this=v[i]

        # Derived types are not currently supported.

        if this.derived_type?
          fail "ERROR: Derived type instance '#{this}' may not be exchanged"
        end

        arrdim=i+1
        var=this.name
        varenv=varenv_get(var)
        dims=varenv["dims"]
        fail "ERROR: Scalar variable '#{var}' may not be exchanged" unless dims
        dh=varenv["decomp"]
        fail "ERROR: Non-decomposed variable '#{var}' may not be exchanged" unless dh
        sl=this.subscript_list
        unless sl.empty?
          unless sl.size==dims.to_i
            fail "ERROR: '#{this}' subscript list must be rank #{dims}"
          end
        end
        (1..dims).each do |r|
          x=sl[r-1]
          lower=(x and x.lower)?(x.lower):(fixbound(varenv,var,r,:l))
          upper=(x and x.upper)?(x.upper):(fixbound(varenv,var,r,:u))
          code.push("#{gllbs}(#{r},#{arrdim})=#{fixbound(varenv,var,r,:l)}")
          code.push("#{glubs}(#{r},#{arrdim})=#{fixbound(varenv,var,r,:u)}")
          code.push("#{strts}(#{r},#{arrdim})=#{lower}")
          code.push("#{stops}(#{r},#{arrdim})=#{upper}")
          code.push("#{perms}(#{r},#{arrdim})=1") if decdim(varenv,r)
        end
        code.push("#{dcmps}(#{arrdim})=#{dh}(#{dh}__nestlevel)")
        code.push("#{types}(#{arrdim})=#{sms_type(var)}")
        code.push("#{names}(#{arrdim})='#{var}'//char(0)")
      end

      tag=sms_commtag
      vars=(1..sms_maxvars).reduce([]) { |m,x| m.push((x>nvars)?("sms__x#{x}"):("#{v[x-1].name}")) }.join(",")
      code.push("call sms__exchange(#{nvars},#{tag},#{gllbs},#{glubs},#{strts},#{stops},#{perms},#{dcmps},#{types},#{names},#{sms_statusvar},#{vars})")
      code.push(sms_chkstat)
      replace_statement(code)

    end

  end

  class SMS_Executable_SMS_Halo_Comp < SMS
  end

  class SMS_Executable_SMS_Parallel < SMS
  end

  class SMS_Executable_SMS_Serial < SMS
  end

  class SMS_Executable_SMS_To_Local < SMS
  end

  class SMS_Get_Communicator < SMS_Getter

    def translate
      translate_with_options("communicator","sms__get_communicator")
    end

  end

  class SMS_Halo_Comp < SMS_Region

    def str0
      "#{e[0]}#{e[1]}#{e[2]}"
    end

    def translate
      fail "ERROR: sms$halo_comp may not appear inside sms$serial region" if sms_serial
    end
    
  end

  class SMS_Halo_Comp_Begin < SMS

    def str0
      sms("#{e[2]}#{e[3]}#{e[4]} #{e[5]}")
    end

  end

  class SMS_Halo_Comp_End < SMS

    def str0
      sms("#{e[2]}")
    end

  end

  class SMS_Halo_Comp_Pair < SMS

    def lo
      e[1]
    end

    def up
      e[3]
    end

  end

  class SMS_Halo_Comp_Pairs < SMS

    def str0
      dim1="#{e[0]}"
      dim2=(e[1].e)?("#{e[1].e[1]}"):(nil)
      dim3=(e[2].e)?("#{e[2].e[1]}"):(nil)
      dims=[dim1,dim2,dim3]
      dims.delete_if { |x| x.nil? }
      dims.join(",")
    end

  end

  class SMS_Ignore < SMS_Region

    def translate
      fail "ERROR: sms$ignore may not appear inside sms$serial region" if sms_serial
    end
    
  end

  class SMS_Ignore_Begin < SMS

    def str0
      sms("#{e[2]}")
    end

  end

  class SMS_Ignore_End < SMS

    def str0
      sms("#{e[2]}")
    end

  end

  class SMS_Parallel < SMS_Region

    def str0
      "#{e[0]}#{e[1]}#{e[2]}"
    end

    def translate
      fail "ERROR: sms$parallel may not appear inside sms$serial region" if sms_serial
    end

  end

  class SMS_Parallel_Begin < SMS

    def str0
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]} #{e[7]}")
    end

  end

  class SMS_Parallel_End < SMS

    def str0
      sms("#{e[2]}")
    end

  end

  class SMS_Parallel_Var_List_1 < SMS

    def str0
      s="#{e[0]}#{e[1]}"
      s+=e[2].e.reduce("") { |m,x| m+="#{x.e[1]}" } if e[2].e
      s+="#{e[3]}"
    end

    def vars
      ["#{e[1]}"]+((e[2].e)?(e[2].e.reduce([]) { |m,x| m.push("#{x.e[1]}") }):([]))
    end

  end

  class SMS_Parallel_Var_List_2 < SMS

    def str0
      "#{e[0]}"
    end

    def vars
      ["#{e[0]}"]
    end

  end

  class SMS_Parallel_Var_Lists_001 < SMS

    def vars
      [[],[],e[2].vars]
    end

  end

  class SMS_Parallel_Var_Lists_010 < SMS

    def vars
      [[],e[1].vars,[]]
    end

  end

  class SMS_Parallel_Var_Lists_011 < SMS

    def vars
      [[],e[1].vars,e[3].vars]
    end

  end

  class SMS_Parallel_Var_Lists_100 < SMS

    def vars
      [e[0].vars,[],[]]
    end

  end

  class SMS_Parallel_Var_Lists_101 < SMS

    def vars
      [e[0].vars,[],e[3].vars]
    end

  end

  class SMS_Parallel_Var_Lists_110 < SMS

    def vars
      [e[0].vars,e[2].vars,[]]
    end

  end

  class SMS_Parallel_Var_Lists_111 < SMS

    def vars
      [e[0].vars,e[2].vars,e[4].vars]
    end

  end

  class SMS_Reduce < SMS

    def op
      e[5]
    end

    def str0
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]}")
    end

    def translate
      fail "ERROR: sms$reduce may not appear inside sms$serial region" if sms_serial
      nvars=vars.size
      fail "ERROR: sms$reduce supports reduction of #{sms_maxvars} variables max" if nvars>sms_maxvars
      use(sms_decompmod)
      sizes=[]
      types=[]
      nvars.times do |i|
        var=vars[i]
        varenv=varenv_get(var)
        fail "ERROR: sms$reduce inapplicable to distributed array '#{var}'" if varenv["decomp"]
        sizes.push((varenv["sort"]=="array")?("size(#{var})"):("1"))
        types.push(sms_type(var))
      end
      sizes="(/#{sizes.join(",")}/)"
      types="(/#{types.join(",")}/)"
      code=[]
      code.push("#{self}")
      code.push("call sms__reduce_#{nvars}(#{sizes},#{types},sms__op_#{op},#{sms_statusvar},#{vars.join(',')})")
      code.push(sms_chkstat)
      replace_statement(code)
    end

    def vars
      e[3].vars
    end

  end

  class SMS_Reduce_Varlist < SMS

    def vars
      list_str.split(",")
    end

    def str0
      list_str
    end

  end

  class SMS_Serial < SMS_Region

    def str0
      "#{e[0]}#{e[1]}#{e[2]}"
    end

    def not_in_env(name,x)
      fail "ERROR: sms$serial-region '#{x}' variable '#{name}' not found in environment"
    end

    def not_in_region(name,x)
      fail "ERROR: variable '#{name}' not found in sms$serial region '#{x}'"
    end

    def translate

      fail "ERROR: sms$serial may not appear inside another sms$serial region" if inside?(SMS_Serial)
      fail "ERROR: sms$erial regions may not appear inside sms$parallel loops" if sms_parallel_loop

      serial_begin=e[0]
      oldblock=e[1]
      serial_end=e[2]
      return if oldblock.e.empty?
      use(sms_decompmod)
      declare("logical",sms_rootcheck)

      # Initially, we don't know which variables will need to be gathered,
      # scattered, or broadcast.

      bcasts=[]
      gathers=[]
      scatters=[]

      # Globally-sized variables must be allocated/deallocated for distributed
      # arrays appearing within serial regions. Here's a set, initially empty,
      # to track them.

      globals=SortedSet.new
      
      # Get the serial info recorded when the serial_begin statement was parsed.

      si=sms_serial_info

      # The vars_in_region set contains names occurring in the serial-region
      # body (rather than in the sms$serial directive itself) and, optionally,
      # an inferred :in or :out treatment. A specific variable name may exist
      # more than once in the set, with different (or no) treatment specified.
      # An instance of a name with 'in' or 'out' treatment is overriden by an
      # instance lacking this specificity. That is: If ["a",:in] and ["a"] both
      # exist, the former is a weaker requirement than the latter (which implies
      # 'inout' treatment) and so should be removed, which we do here.

      si.vars_in_region.delete_if do |name|
        name.last and si.vars_in_region.include?(name.first)
      end

      # Reject variables specified in the sms$ignore directive that are not
      # actually present in the serial region.

      def presence_check(directive_set,region_set,treatment)
        directive_set.each do |directive_var|
          found=false
          region_set.each do |region_var|
            if "#{region_var.first}"=="#{directive_var}"
              found=true
              break
            end
          end
          unless found
            fail "ERROR: sms$serial '#{treatment}' variable "+
              "'#{directive_var}' not present in serial region"
          end
        end
      end

      vars_inout=Set.new(si.vars_in).intersection(Set.new(si.vars_out))
      presence_check(vars_inout,si.vars_in_region,"inout")
      presence_check(si.vars_ignore,si.vars_in_region,"ignore")
      presence_check(si.vars_in,si.vars_in_region,"in")
      presence_check(si.vars_out,si.vars_in_region,"out")

      # Iterate over the set of names that occurred in the region.

      si.vars_in_region.each do |name|

        # If in/out treatment was specified when this name was registered in the
        # serial region, extract that information now.

        name,treatment=name

        # Extract variable information.

        varenv=varenv_get(name,self,expected=false)
        dh=(varenv)?(varenv["decomp"]):(nil)
        sort=(varenv)?(varenv["sort"]):(nil)

        # Ignore namelist names.

        next if sort=="namelist"

        # Handle 'ingore' variables. A decomposed variable must be globalized
        # even if it is not gathered/scattered.

        globals.add(name) if dh and si.vars_ignore.include?(name)

        # Conservatively assume that the default intent will apply to this
        # variable.

        default=true

        # Handle 'in' variables.

        if si.vars_in.include?(name)

          not_in_env(name,"in") unless varenv

          # Ensure that conflicting 'ignore' intent was not specified.

          if si.vars_ignore.include?(name)
            fail "ERROR: '#{name}' cannot be both 'ignore' and 'out' in serial region"
          end

          # Gather the variable if it is decomposed, and note that the default
          # intent no longer applies.

          if dh
            gathers.push(name) unless treatment==:out
            globals.add(name)
          end
          default=false
        end

        # Handle 'out' variables.

        if si.vars_out.include?(name)

          not_in_env(name,"out") unless varenv

          # Ensure that conflicting 'ignore' intent was not specified.

          if si.vars_ignore.include?(name)
            fail "ERROR: '#{name}' cannot be both 'ignore' and 'in' in serial region"
          end

          # Broadcast non-decomposed and scatter decomposed arrays, and note
          # that the default intent no longer applies.

          ((sort=="scalar"||!dh)?(bcasts):(scatters)).push(name) unless treatment==:in
          globals.add(name) if dh
          default=false
        end

        # Handle variables that fall under 'default' treatment.

        if default and not si.vars_ignore.include?(name)

          # If no explicit intent was specified, the default applies. Treatment
          # of scalars, non-decomposed arrays and decomposed arrays is the same
          # as described above. Also, all decomposed arrays appearing in the
          # serial region must be globalized.

          d="#{si.default}"
          not_in_env(name,d) unless varenv or d=="ignore"
          gathers.push(name) if dh and (d=="in" or d=="inout") and not treatment==:out
          if d=="out" or d=="inout"
            ((sort=="scalar"||!dh)?(bcasts):(scatters)).push(name) unless treatment==:in
          end
          globals.add(name) if dh
        end
      end

      # Walk the subtree representing the serial region's body and replace the
      # names of all scattered/gathered variables with their global versions.

      def globalize(node,to_globalize)
        node.e.each { |x| globalize(x,to_globalize) } if node.e
        node.globalize if node.is_a?(Name) and to_globalize.include?("#{node}")
      end

      globalize(oldblock,globals)

      # Declaration of globally-sized variables

      globals.sort.each do |var|
        varenv=varenv_get(var)
        dims=(":"*varenv["dims"]).split("")
        kind=varenv["kind"]
        type=varenv["type"]
        len=(type=="character")?("len(#{var})"):(nil)
        props={:attrs=>["allocatable"],:dims=>dims,:kind=>kind,:len=>len}
        declare(type,sms_global_name(var),props)
      end

      code=[]

      # Collect code for allocation and deallocation of globals.

      code_alloc,code_dealloc=code_alloc_dealloc_globals(globals)

      # Concatenate code.

      code.concat(code_alloc)
      code.concat(code_gather(gathers))
      code.push("if (#{sms_rootcheck}()) then")
      code.push("sms__in_serial=.true.")
      code.push(serial_begin)
      code.push(oldblock)
      code.push(serial_end)
      code.push("sms__in_serial=.false.")
      code.push("endif")
      code.concat(code_scatter(scatters))
      code.concat(code_bcast(bcasts))
      code.concat(code_dealloc)

      # Replace serial region with new code block.

      env[:sms_serial]=false
      replace_statement(code)

    end

  end

  class SMS_Serial_Begin < SMS

    def str0
      sms("#{sa(e[2])}#{e[3]}")
    end

    def translate
      si=sms_serial_info=OpenStruct.new
      si.default=("#{e[2]}".empty?)?("inout"):("#{e[2].default}")
      si.vars_in_region=SortedSet.new
      si.vars_ignore=("#{e[2]}".empty?)?([]):(e[2].vars_ignore)
      si.vars_in=("#{e[2]}".empty?)?([]):(e[2].vars_in)
      si.vars_out=("#{e[2]}".empty?)?([]):(e[2].vars_out)
      parent.env[:sms_serial_info]=sms_serial_info
    end

  end

  class SMS_Serial_Control < SMS

    def default
      e[1].default
    end

    def vars_ignore
      e[1].vars_ignore
    end

    def vars_in
      e[1].vars_in
    end

    def vars_out
      e[1].vars_out
    end

  end

  class SMS_Serial_Control_Option_1 < SMS

    def default
      (e[1].e&&e[1].e[1].respond_to?(:intent))?("#{e[1].e[1].intent}"):("inout")
    end

    def str0
      s="#{e[0]}"
      s+="#{e[1].e[0]}#{e[1].e[1]}" if e[1].e
      s
    end

    def vars_ignore
      e[0].vars_ignore
    end

    def vars_in
      e[0].vars_in
    end

    def vars_out
      e[0].vars_out
    end

  end

  class SMS_Serial_Control_Option_2 < SMS

    def default
      e[0].intent
    end

    def vars_ignore
      []
    end

    def vars_in
      []
    end

    def vars_out
      []
    end

  end

  class SMS_Serial_Default < SMS

    def intent
      e[2]
    end

  end

  class SMS_Serial_End < SMS

    def str0
      sms("#{e[2]}")
    end

  end

  class SMS_Serial_Intent_List < SMS

    def intent
      e[3]
    end

    def vars
      e[1].vars
    end

  end

  class SMS_Serial_Intent_Lists < SMS

    def vars_with_intent(intent)
      vars=("#{e[0].intent}"=="#{intent}")?(e[0].vars):([])
      e[1].e.each { |x| vars+=x.e[1].vars if "#{x.e[1].intent}"=="#{intent}" }
      vars
    end

    def str0
      "#{e[0]}"+e[1].e.reduce("") { |m,x| m+"#{x.e[0]}#{x.e[1]}" }
    end

    def vars_ignore
      vars_with_intent("ignore")
    end

    def vars_in
      vars_with_intent("inout")+vars_with_intent("in")
    end

    def vars_out
      vars_with_intent("inout")+vars_with_intent("out")
    end

  end

  class SMS_Serial_Varlist < SMS

    def str0
      list_str
    end

    def vars
      list_str.split(",")
    end

  end

  class SMS_Set_Communicator < SMS

    def str0
      sms("#{e[2]}#{e[3]}#{e[4]}")
    end

    def translate
      fail "ERROR: sms$set_communicator may not appear inside sms$serial region" if sms_serial
      use(sms_decompmod)
      code=[]
      code.push("#{self}")
      code.push("call sms__set_communicator(#{e[3]},#{sms_statusvar})")
      code.push(sms_chkstat)
      replace_statement(code)
    end

  end

  class SMS_Start < SMS

    def translate
      fail "ERROR: sms$start may not appear inside sms$serial region" if sms_serial
      use(sms_decompmod)
      code=[]
      code.push("#{self}")
      code.push("call sms__start(#{sms_statusvar})")
      code.push(sms_chkstat)
      replace_statement(code)
    end

  end

  class SMS_Stop < SMS

    def translate
      fail "ERROR: sms$stop may not appear inside sms$serial region" if sms_serial
      code=[]
      code.push("#{self}")
      code.push(sms_stop)
      replace_statement(code)
    end

  end

  class SMS_To_Local < SMS_Region

    def str0
      "#{e[0]}#{e[1]}#{e[2]}"
    end

    def translate
      fail "ERROR: sms$to_local may not appear inside sms$serial region" if sms_serial
    end

  end

  class SMS_To_Local_Begin < SMS

    def str0
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]} #{e[7]}")
    end

  end

  class SMS_To_Local_End < SMS

    def str0
      sms("#{e[2]}")
    end

  end

  class SMS_To_Local_List < SMS

    def dd
      e[1]
    end

    def key
      e[5]
    end

    def idx
      "#{dd}".to_i
    end

    def vars
      e[3].vars
    end

  end

  class SMS_To_Local_Lists < SMS

    def str0
      s="#{e[0]}"
      if p=e[1].e
        s+="#{p[0]}#{p[1]}"
        if p=p[2].e
          s+="#{p[0]}#{p[1]}"
        end
      end
      s
    end

    def vars
      def rec(list,v)
        list.vars.each do |x|
          v[x]=OpenStruct.new({:dd=>list.idx,:key=>"#{list.key}"})
        end
      end
      v={}
      rec(e[0],v)
      if p=e[1].e
        rec(p[1],v)
        rec(p[1],v) if p=p[2].e
      end
      v
    end

  end

  class SMS_Var_List < SMS

    def str0
      v=["#{e[0]}"]
      e[1].e.reduce(v) { |m,x| m.push("#{x.e[1]}") } if e[1].e
      v.join(",")
    end

    def vars
      ["#{e[0]}"]+((e[1].e)?(e[1].e.reduce([]) { |m,x| m.push("#{x.e[1]}") }):([]))
    end

  end

  class SMS_Unstructured_Grid < SMS

    def str0
      sms("#{e[2]}#{e[3]}#{e[4]}")
    end

    def translate
      fail "ERROR: sms$unstructured_grid may not appear inside sms$serial region" if sms_serial
      var="#{e[3]}"
      fail "ERROR: No module info found for variable '#{var}'" unless (varenv=varenv_get(var))
      fail "ERROR: No decomp info found for variable '#{var}'" unless (dh=varenv["decomp"])
      use(sms_decompmod)
      code=[]
      code.push("#{self}")
      code.push("call sms__unstructuredgrid(#{dh},size(#{var},1),#{var})")
      code.push("call sms__get_collapsed_halo_size(#{dh}(#{dh}__nestlevel),1,1,#{dh}__localhalosize,#{sms_statusvar})")
      code.push(sms_chkstat)
      code.push("#{dh}__s1(1,1,#{dh}__nestlevel)=#{dh}__s1(1,0,#{dh}__nestlevel)")
      code.push("#{dh}__e1(#{dh}__globalsize(1,#{dh}__nestlevel),1,#{dh}__nestlevel)=#{dh}__e1(#{dh}__globalsize(1,#{dh}__nestlevel),0,#{dh}__nestlevel)+#{dh}__localhalosize")
      replace_statement(code)
    end

  end

  class SMS_Unstructured_Print_Timers < SMS

    def translate
      code=[]
      code.push("#{self}")
      code.push("call sms__unstructured_print_timers")
      replace_statement(code)
    end

  end

  class SMS_Varlist3D_1 < SMS

    def vars
      [e[0],e[2],e[4]]
    end

  end

  class SMS_Varlist3D_2 < SMS

    def vars
      [e[1],e[3]]
    end

  end

  class SMS_Varlist3D_3 < SMS

    def vars
      [e[2]]
    end

  end

  class SMS_Varlist3D_4 < SMS

    def vars
      [e[0],e[2]]
    end

  end

  class SMS_Varlist3D_5 < SMS

    def vars
      [e[1]]
    end

  end

  class SMS_Varlist3D_6 < SMS
    def vars
      [e[0]]
    end

  end

  class SMS_Zerotimers < SMS

    def translate
      fail "ERROR: sms$zerotimers may not appear inside sms$serial region" if sms_serial
      code=[]
      code.push("#{self}")
      code.push("call sms__zerotimers")
      replace_statement(code)
    end

  end

  class Stop_Stmt < Stmt

    def translate
      return if sms_ignore
      l=label_delete unless (l=label).empty?
      retcode=(stop_code and stop_code.numeric?)?("#{stop_code}"):("0")
      msg=(stop_code and stop_code.character?)?("#{stop_code}"):(nil)
      msg=msg.sub(/^['"]/,"").sub(/['"]$/,"") if msg
      code=[]
      code.push(sms_abort(retcode,msg))
      replace_statement(code)
    end

  end

  class Write_Stmt < Io_Stmt

    def translate
      return if sms_ignore
      if sms_serial
        add_serial_region_vars(:in)
      else        
        io_stmt_init
        function=(env["#{unit}"] and env["#{unit}"]["subprogram"]=="function")
        @onroot=false if unit.is_a?(Internal_File_Unit) or function
        if (namelist_name=nml)
          @onroot=true
          nmlenv=varenv_get(namelist_name,self,expected=true)
          nmlenv["objects"].each do |x|
            var=(x.respond_to?(:name))?("#{x.name}"):("#{x}")
            varenv=varenv_get(var,self,expected=true)
            if varenv["decomp"]
              @var_gather.add(var)
              replace_input_item(x,sms_global_name(var))
            end
          end
        end
        io_stmt_common(:in)
      end
    end

  end

end # module Fortran

class Translator

  def prepsrc_free(s)

    # Process SMS continuations, inserts and removes

    s=s.gsub(/^\s*(!sms\$.*)&\s*\n\s*!sms\$&(.*)/im,'\1\2')
    s=s.gsub(/^\s*!sms\$insert\s*/i,"")
    s=s.gsub(/^\s*!sms\$remove\s+begin.*?!sms\$remove\s+end/im,"")

    # Join OpenMP (END) PARALLEL DO directives

    a=s.split("\n")
    a.each_index do |i|
      s0=a[i]
      j=i
      while s0=~/^\s*!\$omp.*&\s*$/i
        j+=1
        s0=(s0.sub(/&\s*$/,'')+a[j].sub(/^\s*!\$omp/i,'')).gsub(/\s+/,' ')
      end
      if s0=~/^\s*!\$omp (end )?parallel do/i
        a[i]=s0.downcase
        (j-i).times { a.delete_at(i+1) }
      end
    end
    s=a.join("\n")
    
    s

  end

  def prepsrc_fixed(s)

    # Process SMS continuations, inserts and removes

    s=s.gsub(/^([c!\*]sms\$.*)&\s*\n[c\*]sms\$&(.*)/im,'\1\2')
    s=s.gsub(/^[c!\*]sms\$insert\s*/i,"")
    s=s.gsub(/^[c!\*]sms\$remove\s+begin.*?[c\*]sms\$remove\s+end/im,"")

    # Join OpenMP (END) PARALLEL DO directives

    a=s.split("\n")
    a.each_index do |i|
      s0=a[i]
      j=i
      while s0=~/^[c!\*]\$omp.*&\s*$/i
        j+=1
        s0=(s0.sub(/&\s*$/,'')+a[j].sub(/^[c!\*]\$omp/i,'')).gsub(/\s+/,' ')
      end
      if s0=~/^!\$omp (end )?parallel do/i
        a[i]=s0.downcase
        (j-i).times { a.delete_at(i+1) }
      end
    end
    s=a.join("\n")
    
    s

  end

end
