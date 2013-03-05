require 'fortran'
require 'sms_fortran_parser'

module Fortran

  def sms(s)
    "#{e[0]}#{e[1]} #{s}\n"
  end

  def sp_sms_distribute_begin(sms_decomp_name,sms_distribute_dims)
    fail "Already inside distribute region" if @@distribute
    @@distribute={"decomp"=>"#{sms_decomp_name}","dim"=>[]}
    sms_distribute_dims.dims.each { |x| @@distribute["dim"].push(x) }
    true
  end

  def sp_sms_distribute_end
    fail "Not inside distribute region" unless @@distribute
    @@distribute=nil
    true
  end

  def sp_sms_halo_comp_begin(halo_comp_pairs)
    fail "Halo computation invalid outside parallel region" unless @@parallel
    fail "Already inside halo-computation region" if @@halocomp
    envpush
    dims={}
    dims[1]=halo_comp_pairs.e[0]
    dims[2]=halo_comp_pairs.e[1].e[1] if halo_comp_pairs.e[1].e
    dims[3]=halo_comp_pairs.e[2].e[1] if halo_comp_pairs.e[2].e
    env[:halocomp]={}
    dims.each { |k,v| env[:halocomp][k]=OpenStruct.new({:lo=>v.lo,:up=>v.up}) }
    @@halocomp=true
    true
  end

  def sp_sms_halo_comp_end
    fail "Not inside halo-computation region" unless @@halocomp
    @@halocomp=false
    envpop
    true
  end

  def sp_sms_parallel_begin(sms_decomp_name,sms_parallel_var_lists)
    fail "Already inside parallel region" if @@parallel
    envpush
    env[:parallel]=OpenStruct.new({:dh=>"#{sms_decomp_name}",:vars=>sms_parallel_var_lists.vars})
    @@parallel=true
    true
  end

  def sp_sms_parallel_end
    fail "Not inside parallel region" unless @@parallel
    @@parallel=false
    envpop
    true
  end

  def sp_sms_serial_begin
    fail "Already inside serial region" if @@serial
    envpush
    env[:serial]=true
    @@serial=true
    true
  end

  def sp_sms_serial_end
    fail "Not inside serial region" unless @@serial
    @@serial=false
    envpop
    true
  end

  def sp_sms_to_local_begin(sms_decomp_name,sms_to_local_lists)
    fail "Already inside to_local region" if @@tolocal
    envpush
    env[:tolocal]=sms_to_local_lists.vars.each do |var,props|
      props.dh="#{sms_decomp_name}"
    end
    @@tolocal=true
    true
  end

  def sp_sms_to_local_end
    fail "Not inside to_local region" unless @@tolocal
    @@tolocal=false
    envpop
    true
  end

  def smstype(type,kind)
    kind=nil if kind=="_default"
    case type
    when "integer"
      (kind)?("nnt_i#{kind}"):("nnt_integer")
    when "real"
      (kind)?("nnt_r#{kind}"):("nnt_real")
    when "doubleprecision"
      "nnt_doubleprecision"
    when "complex"
      (kind)?("nnt_c#{kind}"):("nnt_complex")
    when "logical"
      (kind)?("nnt_l#{kind}"):("nnt_logical")
    end
  end

  class Array_Name_And_Spec < E

    def translate
      envget
      var="#{e[0]}"
      spec=e[2].spec
      varenv=env[var]
      distribute_array_bounds(spec,varenv)
    end

  end

  class Entity_Decl_1 < Entity_Decl

    def translate
      envget
      var="#{e[0]}"
      fail "'#{var}' not found in environment" unless (varenv=env[var])
      spec=nil
      if varenv["rank"]=="array"
        if (entity_decl_array_spec=e[1]).is_a?(Entity_Decl_Array_Spec)
          # entity_decl_array_spec case
          spec=entity_decl_array_spec.e[1].spec
        else
          attr_spec_option=ancestor(Type_Declaration_Stmt).e[2]
          if attr_spec_option.is_a?(Attr_Spec_Option)
            if (d=attr_spec_option.dimension?)
              # dimension attribute case
              spec=d.spec
            end
          end
        end
      end
      distribute_array_bounds(spec,varenv)
    end

  end

  class Name < T

    def translate
      envget
      if tolocal=env[:tolocal] and p=tolocal[name]
        case p.key
        when "lbound"
          se="s#{p.decdim}"
          halo_offset=halo_offsets(p.decdim).lo
        when "ubound"
          se="e#{p.decdim}"
          halo_offset=halo_offsets(p.decdim).up
        else
          fail "Unrecognized SMS$TO_LOCAL key: #{p.key}"
        end
        code="#{p.dh}__#{se}(#{name},#{halo_offset},#{p.dh}__nestlevel)"
        replace_element(code,:expr)
      end
    end

  end

  class Nonlabel_Do_Stmt < T

    def translate
      envget
      unless env[:serial]
        if parallel=env[:parallel]
          loop_control=e[3]
          loop_var="#{loop_control.e[1]}"
          decdim=nil
          [0,1,2].each do |i|
            if parallel.vars[i].include?(loop_var)
              decdim=i+1
              break
            end
          end
          if decdim
            halo_lo=halo_offsets(decdim).lo
            halo_up=halo_offsets(decdim).up
            if loop_control.is_a?(Loop_Control_1)
              lo=raw("dh__s#{decdim}(#{loop_control.e[3]},#{halo_lo},dh__nestlevel)",:scalar_numeric_expr,{:nl=>false})
              lo.parent=loop_control
              up=raw(",dh__e#{decdim}(#{loop_control.e[4].value},#{halo_up},dh__nestlevel)",:loop_control_pair,{:nl=>false})
              up.parent=loop_control
              loop_control.e[3]=lo
              loop_control.e[4]=up
            end
          end
        end
      end
    end

  end

  class SMS < T

    def fixbound(varenv,var,dim,x)
      bound=varenv["#{x}b#{dim}"]
      fail "Bad upper bound: #{bound}" if bound=="_default" and x==:u
      return 1 if bound=="_default" and x==:l
      if ["_assumed","_deferred","_explicit"].include?(bound)
        if decdim=varenv["dim#{dim}"]
          lu=(x==:l)?("low"):("upper")
          return "dh__#{lu}bounds(#{decdim},dh__nestlevel)"
        else
          return "#{x}bound(#{var},#{dim})"
        end
      end
      bound
    end

    def ranks
      (1..7)
    end

  end

  class SMS_Barrier < SMS

    def translate
      use("nnt_types_module")
      code="call ppp_barrier(ppp__status)"
      replace_statement(code,:call_stmt)
    end

  end

  class SMS_Compare_Var < SMS

    def to_s
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]}")
    end

    def translate
      envget
      use("module_decomp")
      use("nnt_types_module")
      declare("logical","sms_debugging_on")
      var="#{e[3].name}"
      fail "'#{var}' not found in environment" unless (varenv=env[var])
      dims=varenv["dims"]
      str="#{e[5]}"
      type=smstype(varenv["type"],varenv["kind"])
      gllbs="(/"+ranks.map { |i| (i>dims)?(1):(fixbound(varenv,var,i,:l)) }.join(",")+"/)"
      glubs="(/"+ranks.map { |i| (i>dims)?(1):(fixbound(varenv,var,i,:u)) }.join(",")+"/)"
      perms="(/"+ranks.map { |i| varenv["dim#{i}"]||0 }.join(",")+"/)"
      if dh=varenv["decomp"]
        decomp="#{dh}(dh__nestlevel)"
      else
        decomp="ppp_not_decomposed"
      end
      code="if (sms_debugging_on()) call ppp_compare_var(#{decomp},#{var},#{type},#{glubs},#{perms},#{gllbs},#{glubs},#{gllbs},#{dims},'#{var}',#{str},ppp__status)"
      replace_statement(code,:if_stmt)
    end

  end

  class SMS_Create_Decomp < SMS
    def to_s() sms(e[2].e.map { |x| x.text_value }.join) end
  end

  class SMS_Distribute_Begin < SMS
    def to_s() sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]} #{e[7]}") end
  end

  class SMS_Distribute_End < SMS
    def to_s() sms("#{e[2]}") end
  end

  class SMS_Distribute_Dims_1 < SMS
    def dims() ["#{e[0]}".to_i,"#{e[2]}".to_i] end
  end

  class SMS_Distribute_Dims_2 < SMS
    def dims() [nil,"#{e[1]}".to_i] end
  end

  class SMS_Distribute_Dims_3 < SMS
    def dims() ["#{e[0]}".to_i,nil] end
  end

  class SMS_Exchange < SMS

    def to_s
      sms("#{e[2]}#{e[3]}#{e[4].e.reduce("") { |m,x| m+="#{x.e[0]}#{x.e[1]}" }}#{e[5]}")
    end

    def translate
      envget
      use("module_decomp")
      use("nnt_types_module")
      tag="ppp__tag_#{@@tag+=1}"
      declare("integer",tag,["save"])
      v=[e[3]]+e[4].e.reduce([]) { |m,x| m.push(x.e[1]) }
      nvars=v.size
      vars=v.reduce([]) { |m,x| m.push("#{x}") }.join(",")
      names=v.reduce([]) { |m,x| m.push("'#{x}'") }.join(",")
      cornerdepth=[]
      dectypes=[]
      gllbs=[]
      glubs=[]
      halol=[]
      halou=[]
      perms=[]
      types=[]
      varenv=nil
      (0..nvars-1).each do |i|
        var=v[i].name
        fail "'#{var}' not found in environment" unless (varenv=env[var])
        dims=varenv["dims"]
        dh=varenv["decomp"]
        dectypes.push("#{dh}(dh__nestlevel)")
        cornerdepth.push("9999")
        ranks.each { |j| gllbs.push((j>dims)?(1):(fixbound(varenv,var,j,:l))) }
        ranks.each { |j| glubs.push((j>dims)?(1):(fixbound(varenv,var,j,:u))) }
        ranks.each { |j| halol.push((j>dims)?(0):("dh__halosize(1,dh__nestlevel)")) }
        ranks.each { |j| halou.push((j>dims)?(0):("dh__halosize(1,dh__nestlevel)")) }
        ranks.each { |j| perms.push(varenv["dim#{j}"]||0) }
        types.push(smstype(varenv["type"],varenv["kind"]))
      end
      cornerdepth="(/#{cornerdepth.join(",")}/)"
      dectypes="(/#{dectypes.join(",")}/)"
      gllbs="reshape((/#{gllbs.join(",")}/),(/#{nvars},7/))"
      glubs="reshape((/#{glubs.join(",")}/),(/#{nvars},7/))"
      halol="reshape((/#{halol.join(",")}/),(/#{nvars},7/))"
      halou="reshape((/#{halou.join(",")}/),(/#{nvars},7/))"
      perms="reshape((/#{perms.join(",")}/),(/#{nvars},7/))"
      types="(/#{types.join(",")}/)"
      code="call ppp_exchange_#{nvars}(#{tag},#{gllbs},#{glubs},#{gllbs},#{glubs},#{perms},#{halol},#{halou},#{cornerdepth},#{dectypes},#{types},ppp__status,#{vars},#{names})"
      replace_statement(code,:call_stmt)
    end

  end

  class SMS_Halo_Comp_Begin < SMS

    def to_s
      sms("#{e[2]}#{e[3]}#{e[4]} #{e[5]}")
    end

    def translate
      remove
    end

  end

  class SMS_Halo_Comp_End < SMS

    def to_s
      sms("#{e[2]}")
    end

    def translate
      remove
    end

  end

  class SMS_Halo_Comp_Pair < E
    def lo() "#{e[1]}" end
    def up() "#{e[3]}" end
  end

  class SMS_Halo_Comp_Pairs < E
    def to_s
      dim1="#{e[0]}"
      dim2=(e[1].e)?("#{e[1].e[1]}"):(nil)
      dim3=(e[2].e)?("#{e[2].e[1]}"):(nil)
      dims=[dim1,dim2,dim3]
      dims.delete_if { |x| x.nil? }
      dims.join(",")
    end
  end

  class SMS_Ignore_Begin < SMS
    def to_s() sms("#{e[2]}") end
  end

  class SMS_Ignore_End < SMS
    def to_s() sms("#{e[2]}") end
  end

  class SMS_Parallel_Begin < SMS

    def to_s
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]} #{e[7]}")
    end

    def translate
      remove
    end

  end

  class SMS_Parallel_End < SMS

    def to_s
      sms("#{e[2]}")
    end

    def translate
      remove
    end

  end

  class SMS_Parallel_Var_List_1 < E

    def to_s
      s="#{e[0]}#{e[1]}"
      s+=e[2].e.reduce("") { |m,x| m+="#{x.e[1]}" } if e[2].e
      s+="#{e[3]}"
    end

    def vars
      ["#{e[1]}"]+((e[2].e)?(e[2].e.reduce([]) { |m,x| m.push("#{x.e[1]}") }):([]))
    end

  end

  class SMS_Parallel_Var_List_2 < E

    def to_s
      "#{e[0]}"
    end

    def vars
      ["#{e[0]}"]
    end

  end

  class SMS_Parallel_Var_Lists_001 < T
    def vars() [[],[],e[2].vars] end
  end

  class SMS_Parallel_Var_Lists_010 < T
    def vars() [[],e[1].vars,[]] end
  end

  class SMS_Parallel_Var_Lists_011 < T
    def vars() [[],e[1].vars,e[3].vars] end
  end

  class SMS_Parallel_Var_Lists_100 < T
    def vars() [e[0].vars,[],[]] end
  end

  class SMS_Parallel_Var_Lists_101 < T
    def vars() [e[0].vars,[],e[3].vars] end
  end

  class SMS_Parallel_Var_Lists_110 < T
    def vars() [e[0].vars,e[2].vars,[]] end
  end

  class SMS_Parallel_Var_Lists_111 < T
    def vars() [e[0].vars,e[2].vars,e[4].vars] end
  end

  class SMS_Reduce < SMS
    def to_s() sms("#{e[2]}") end
  end

  class SMS_Serial_Begin < SMS
    def to_s() sms("#{sa(e[2])}#{e[3]}") end
  end

  class SMS_Serial_End < SMS
    def to_s() sms("#{e[2]}") end
  end

  class SMS_Set_Communicator < SMS

    def to_s
      sms("#{e[2]}#{e[3]}#{e[4]}")
    end

    def translate
      use("nnt_types_module")
      code="call sms_set_communicator(#{e[3]},ppp__status)"
      replace_statement(code,:call_stmt)
    end

  end

  class SMS_To_Local_Begin < SMS

    def to_s
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]} #{e[7]}")
    end

    def translate
      remove
    end

  end

  class SMS_To_Local_End < SMS

    def to_s
      sms("#{e[2]}")
    end

    def translate
      remove
    end

  end

  class SMS_To_Local_List < E
    def decdim() "#{e[1]}" end
    def key() "#{e[5]}" end
    def idx() decdim.to_i end
    def vars() e[3].vars end
  end

  class SMS_To_Local_Lists < T

    def to_s
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
          v[x]=OpenStruct.new({:decdim=>list.idx,:key=>list.key})
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

  class SMS_Var_List < E

    def to_s
      v=["#{e[0]}"]
      e[1].e.reduce(v) { |m,x| m.push("#{x.e[1]}") } if e[1].e
      v.join(",")
    end

    def vars
      ["#{e[0]}"]+((e[1].e)?(e[1].e.reduce([]) { |m,x| m.push("#{x.e[1]}") }):([]))
    end

  end

  class SMS_Unstructured_Grid < SMS

    def to_s
      sms("#{e[2]}#{e[3]}#{e[4]}")
    end

    def translate
      envget
      var="#{e[3]}"
      fail "No module info found for variable '#{var}'" unless (varenv=env[var])
      fail "No decomp info found for variable '#{var}'" unless (dh=varenv["decomp"])
      use("nnt_types_module")
      stmts=[]
      stmts.push(["call sms_unstructuredgrid(#{dh},size(#{var},1),#{var})",:call_stmt])
      stmts.push(["call ppp_get_collapsed_halo_size(#{dh}(#{dh}__nestlevel),1,1,#{dh}__localhalosize,ppp__status)",:call_stmt])
      stmts.push(["#{dh}__s1(1,1,#{dh}__nestlevel)=#{dh}__s1(1,0,#{dh}__nestlevel)",:assignment_stmt])
      stmts.push(["#{dh}__e1(#{dh}__globalsize(1,#{dh}__nestlevel),1,#{dh}__nestlevel)=#{dh}__e1(#{dh}__globalsize(1,#{dh}__nestlevel),0,#{dh}__nestlevel)+#{dh}__localhalosize",:assignment_stmt])
      replace_statements(stmts)
    end

  end

  class T < Treetop::Runtime::SyntaxNode

    def distribute_array_bounds(spec,varenv)
      if (dh=varenv["decomp"])
        if spec and spec.is_a?(Explicit_Shape_Spec_List)
          cb=spec.concrete_boundslist
          newbounds=[]
          cb.each_index do |i|
            b=cb[i]
            arrdim=i+1
            if (decdim=varenv["dim#{arrdim}"])
              s="#{dh}__local_lb(#{decdim},dh__nestlevel):"+
                "#{dh}__local_ub(#{decdim},dh__nestlevel)"
            else
              s=(b.clb=="1")?(b.cub):("#{b.clb}:#{b.cub}")
            end
            newbounds.push(s)
          end
          code=newbounds.join(",")
#         replace_element(code,:array_spec,spec)
        end
      end
    end

    def use(modulename,usenames=[])
      unless uses?(modulename,:all)
        code="use #{modulename}"
        unless usenames.empty?
          list=[]
          usenames.each do |x|
            h=x.is_a?(Hash)
            localname=(h)?(x.keys.first):(nil)
            usename=(h)?(x.values.first):(x)
            unless uses?(modulename,usename)
              list.push(((h)?("#{localname}=>#{usename}"):("#{usename}")))
            end
          end
          code=((list.empty?)?(nil):("#{code},only:#{list.join(",")}"))
        end
        if code
          p=use_part
# HACK start
          t=raw("!sms$ignore begin",:sms_ignore_begin)
          t.parent=p
          p.e.push(t)
# HACK end
          t=raw(code,:use_stmt)
          t.parent=p
          p.e.push(t)
# HACK start
          t=raw("!sms$ignore end",:sms_ignore_end)
          t.parent=p
          p.e.push(t)
# HACK end
        end
      end
    end

  end

end

module Translator

  def prepsrc(s)
    s=s.gsub(/^\s*!sms\$insert */i,"")                           # process inserts
    s=s.gsub(/^\s*!sms\$remove +begin.*?!sms\$remove +end/im,"") # process removes
    s
  end

end

# paul.a.madden@noaa.gov
