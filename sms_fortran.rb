require "fortran"
require "set"
require "sms_fortran_parser"

module Fortran

  def sms(s)
    "#{e[0]}#{e[1]} #{s}\n"
  end

  def sp_sms_distribute_begin(sms_decomp_name,sms_distribute_dims)
    fail "Already inside distribute region" if @distribute
    @distribute={"decomp"=>"#{sms_decomp_name}","dim"=>[]}
    sms_distribute_dims.dims.each { |x| @distribute["dim"].push(x) }
    true
  end

  def sp_sms_distribute_end
    fail "Not inside distribute region" unless @distribute
    @distribute=nil
    true
  end

  def sp_sms_halo_comp
    envpop
    true
  end

  def sp_sms_halo_comp_begin(halo_comp_pairs)
    fail "Halo computation invalid outside parallel region" unless env[:sms_parallel]
    fail "Already inside halo-computation region" if env[:sms_halo_comp]
    envpush
    dims={}
    dims[1]=halo_comp_pairs.e[0]
    dims[2]=halo_comp_pairs.e[1].e[1] if halo_comp_pairs.e[1].e
    dims[3]=halo_comp_pairs.e[2].e[1] if halo_comp_pairs.e[2].e
    env[:sms_halo_comp]={}
    dims.each { |k,v| env[:sms_halo_comp][k]=OpenStruct.new({:lo=>v.lo,:up=>v.up}) }
    true
  end

  def sp_sms_halo_comp_end
    fail "Not inside halo-computation region" unless env[:sms_halo_comp]
    true
  end

  def sp_sms_ignore_begin
    fail "Already inside SMS$IGNORE region" if env[:sms_ignore]
    env[:sms_ignore]=true
    true
  end

  def sp_sms_ignore_end
    fail "Not inside SMS$IGNORE region" unless env[:sms_ignore]
    env.delete(:sms_ignore)
    true
  end

  def sp_sms_parallel
    envpop
    true
  end

  def sp_sms_parallel_begin(sms_decomp_name,sms_parallel_var_lists)
    fail "Already inside parallel region" if env[:sms_parallel]
    envpush
    env[:sms_parallel]=OpenStruct.new({:decomp=>"#{sms_decomp_name}",:vars=>sms_parallel_var_lists.vars})
    true
  end

  def sp_sms_parallel_end
    fail "Not inside parallel region" unless env[:sms_parallel]
    true
  end

  def sp_sms_serial
    envpop
    true
  end

  def sp_sms_serial_begin
    fail "Already inside serial region" if env[:sms_serial]
    envpush
    env[:sms_serial]=true
    true
  end

  def sp_sms_serial_end
    fail "Not inside serial region" unless env[:sms_serial]
    true
  end

  def sp_sms_to_local
    envpop
    true
  end

  def sp_sms_to_local_begin(sms_decomp_name,sms_to_local_lists)
    fail "Already inside to_local region" if env[:sms_to_local]
    envpush
    env[:sms_to_local]=sms_to_local_lists.vars.each do |var,props|
      props.dh="#{sms_decomp_name}"
    end
    true
  end

  def sp_sms_to_local_end
    fail "Not inside to_local region" unless env[:sms_to_local]
    true
  end

  def smstype(type,kind)
    kind=nil if kind=="_default"
    case type
    when "character"
      return "nnt_bytes"
    when "complex"
      return (kind)?("nnt_c#{kind}"):("nnt_complex")
    when "doubleprecision"
      return "nnt_doubleprecision"
    when "integer"
      return (kind)?("nnt_i#{kind}"):("nnt_integer")
    when "logical"
      return (kind)?("nnt_l#{kind}"):("nnt_logical")
    when "real"
      return (kind)?("nnt_r#{kind}"):("nnt_real")
    end
    fail "No NNT type defined for '#{type}#{kind}'"
  end

  class T < Treetop::Runtime::SyntaxNode

    def declare(type,name,props={})
      dc=declaration_constructs
      varenv=getvarenv(name,dc,false)
      if varenv
        fail "Variable #{name} is already defined" unless varenv["pppvar"]
      else
        attrs=props[:attrs]||[]
        attrs=[attrs] unless attrs.is_a?(Array)
        attrs=(attrs.empty?)?(""):(",#{attrs.sort.join(",")}")
        code="#{type}#{attrs}::#{name}"
        dims=props[:dims]
        code+="(#{dims.join(',')})" if dims
        init=props[:init]
        code+="=#{init}" if init
        t=raw(code,:type_declaration_stmt,root.srcfile)
        t.parent=dc
        dc.e[0].e.insert(0,t) # prefer "dc.e.push(t)" -- see TODO
        dc.env[name]||={}
        dc.env[name]["pppvar"]=true
      end
    end

    def env
      (envsrc=ancestor(Scoping_Unit,SMS_Region))?(envsrc.envref):(self.envref)
    end

    def halo_offsets(decdim)
      halo_lo=0
      halo_up=0
      if halocomp=self.env[:sms_halo_comp]
        offsets=halocomp[decdim]
        halo_lo=offsets.lo
        halo_up=offsets.up
      end
      OpenStruct.new({:lo=>halo_lo,:up=>halo_up})
    end

# HACK start

# This overrides T#use in fortran.rb to provide sms$ignore wraps for peaceful
# coexistence with legacy ppp, for now. Remove this HACK when legacy ppp is
# removed from build chain.

    def use(modname,usenames=[])
      unless uses?(modname,:all)
        new_usenames=[]
        code="use #{modname}"
        unless usenames.empty?
          list=[]
          usenames.each do |x|
            h=x.is_a?(Hash)
            localname=(h)?(x.keys.first):(nil)
            usename=(h)?(x.values.first):(x)
            unless uses?(modname,usename)
              list.push(((h)?("#{localname}=>#{usename}"):("#{usename}")))
              new_usenames.push([localname||usename,usename])
            end
          end
          code+=((list.empty?)?(""):(",only:#{list.join(",")}"))
        end
        up=use_part
        new_usenames=[[:all]] if new_usenames.empty?
        new_uses={modname=>new_usenames}
        old_uses=up.env[:uses]
        up.env[:uses]=(old_uses)?(old_uses.merge(new_uses)):(new_uses)
  # sub HACK start
        code=("!sms$ignore begin\n#{code}\n!sms$ignore end")
        t=raw(code,:sms_ignore_use,@srcfile)
        t.parent=up
        up.e.push(t)
  # sub HACK end
      end
    end
  end

# HACK end

  class Allocate_Object < E

    def translate
      if inside?(Allocate_Stmt)
        allocate_object=e[1]
        if allocate_object.is_a?(Data_Ref)
          data_ref=allocate_object
          part_ref=data_ref.rightmost
          var=part_ref.name
# Not sure what to do here yet. Normally we could use the following two lines
# instead of the two after that, but if we're looking at e.g.
#   allocate(a%b(nip))
# then 'b' is not going to be found in the environment. We'd need to find 'a' in
# the environment and then... Can we have a decomposed array inside a derived
# type? A decomposed array *of* dervived type, yes -- but the components?

#         varenv=getvarenv(var)
#         if varenv["decomp"]

          varenv=getvarenv(var,self,false)
          if varenv and (decomp=varenv["decomp"])
            subscript_list=part_ref.subscript_list
            newdims=[]
            subscript_list.each_index do |i|
              arrdim=i+1
              if (decdim=varenv["dim#{arrdim}"])
                newdims.push("#{decomp}__local_lb(#{decdim},#{decomp}__nestlevel):#{decomp}__local_ub(#{decdim},#{decomp}__nestlevel)")
              else
                newdims.push("#{subscript_list[i]}")
              end
            end
            code="#{var}(#{newdims.join(",")})"
            replace_element(code,:allocate_object)

# HACK start

            # See comment for class Allocate_Stmt, below. Here we just mark the
            # parent allocate statemnt to hide later, since it contains at least
            # one set of translated array bounds.

            allocate_stmt=ancestor(Allocate_Stmt)
            allocate_stmt.instance_variable_set(:@smsignore,true)

# HACK end

          end
        else
          fail "Did not expect to parse a variable_name here -- thought it was broken!"
        end
      else
        # Do not translate inside a deallocate_stmt
      end
    end

  end

# HACK start

  # For now, legacy ppp still needs to see sms$distribute directives, so it
  # may think that it needs to translate allocate statements with distributed
  # arrays, though the translation will have already been done in class
  # Allocate_Object, above. So, wrap the allocate statement in an sms$ignore
  # block, and summarize in a do-block to we can do a one-for-one statement-
  # tree replacement.

  class Allocate_Stmt < StmtC

    def translate
      if self.instance_variable_get(:@smsignore)
        code=[]
        code.push("do")
        code.push("!sms$ignore begin")
        code.push("#{self}")
        code.push("!sms$ignore end")
        code.push("exit")
        code.push("enddo")
        code=code.join("\n")
        replace_statement(code,:block_do_construct)
      end
    end

  end

# HACK end

  class Array_Name_And_Spec < E

    def translate
      var="#{e[0]}"
      spec=e[2].spec
      varenv=getvarenv(var)
      distribute_array_bounds(spec,varenv)
    end

  end

  class Entity_Decl_1 < Entity_Decl

    def translate
      var="#{e[0]}"
      varenv=getvarenv(var)
      spec=nil
      if varenv["rank"]=="_array"
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
      # sms$to_local handling
      if tolocal=self.env[:sms_to_local] and p=tolocal[name]
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
      # sms$serial handling
      if inside?(SMS_Serial) and not inside?(SMS_Serial_Begin)
        self.env[:sms_serial_info].names_in_region.add("#{self}")
      end
    end

  end

  class Nonlabel_Do_Stmt < T

    def translate
      unless self.env[:sms_serial]
        if parallel=self.env[:sms_parallel]
          loop_control=e[3]
          loop_var="#{loop_control.e[1]}"
          decdim=nil
          [0,1,2].each do |i|
            if parallel.vars[i].include?(loop_var)
              decdim=i+1
              break
            end
          end
          decomp=parallel.decomp
          if decdim
            halo_lo=halo_offsets(decdim).lo
            halo_up=halo_offsets(decdim).up
            if loop_control.is_a?(Loop_Control_1)
              lo=raw("#{decomp}__s#{decdim}(#{loop_control.e[3]},#{halo_lo},#{decomp}__nestlevel)",:scalar_numeric_expr,"",{:nl=>false})
              lo.parent=loop_control
              up=raw(",#{decomp}__e#{decdim}(#{loop_control.e[4].value},#{halo_up},#{decomp}__nestlevel)",:loop_control_pair,"",{:nl=>false})
              up.parent=loop_control
              loop_control.e[3]=lo
              loop_control.e[4]=up
            end
          end
        end
      end
    end

  end

  class SMS < E

    def fixbound(varenv,var,dim,x)
      bound=varenv["#{x}b#{dim}"]
      fail "Bad upper bound: #{bound}" if bound=="_default" and x==:u
      return 1 if bound=="_default" and x==:l
      if ["_assumed","_deferred","_explicit"].include?(bound)
        if (decdim=varenv["dim#{dim}"])
          decomp=varenv["decomp"]
          lu=(x==:l)?("low"):("upper")
          return "#{decomp}__#{lu}bounds(#{decdim},#{decomp}__nestlevel)"
        else
          return "#{x}bound(#{var},#{dim})"
        end
      end
      bound
    end

    def maxrank
      7
    end

    def ranks
      (1..maxrank)
    end

  end

  class SMS_Region < SMS
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
      use("module_decomp")
      use("nnt_types_module")
      declare("logical","sms_debugging_on")
      var="#{e[3].name}"
      varenv=getvarenv(var)
      dims=varenv["dims"]
      str="#{e[5]}"
      type=smstype(varenv["type"],varenv["kind"])
      gllbs="(/"+ranks.map { |r| (r>dims)?(1):(fixbound(varenv,var,r,:l)) }.join(",")+"/)"
      glubs="(/"+ranks.map { |r| (r>dims)?(1):(fixbound(varenv,var,r,:u)) }.join(",")+"/)"
      perms="(/"+ranks.map { |r| varenv["dim#{r}"]||0 }.join(",")+"/)"
      if (decomp=varenv["decomp"])
        decomp="#{decomp}(#{decomp}__nestlevel)"
      else
        decomp="ppp_not_decomposed"
      end
      code="if (sms_debugging_on()) call ppp_compare_var(#{decomp},#{var},#{type},#{glubs},#{perms},#{gllbs},#{glubs},#{gllbs},#{dims},'#{var}',#{str},ppp__status)"
      replace_statement(code,:if_stmt)
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

    def to_s
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]}#{e[7]}#{e[8]}#{e[9]}")
    end

    def translate
      max=3
      d="#{decomp}"
      n="#{decomp}__nestlevel"
      use("module_decomp")
      use("nnt_types_module")
      declare("integer","ppp__periodicusedupper",{:dims=>%W[ppp_max_decomposed_dims]})
      declare("integer","ppp__periodicusedlower",{:dims=>%W[ppp_max_decomposed_dims]})
      stmts=[]
      stmts.push(["#{n}=1",:assignment_stmt])
      stmts.push(["#{d}__nregions=1",:assignment_stmt])
      # See TODO about this multiple-loop HACK. Merge these loops when legacy ppp is gone...
      # See TODO about this being essentially hardwired to work with FIM
      max.times do |i|
        dim=i+1
        g=global[i]
        if g
          stmts.push(["allocate(#{d}__s#{dim}(1:1,0:1,#{d}__maxnests))",:allocate_stmt])
          stmts.push(["allocate(#{d}__e#{dim}(#{g}:#{g},0:1,#{d}__maxnests))",:allocate_stmt])
        end
        stmts.push(["#{d}__globalsize(#{dim},#{n})=#{(g)?(g):(1)}",:assignment_stmt])
      end
      max.times do |i|
        dim=i+1
        stmts.push(["#{d}__localsize(#{dim},#{n})=0",:assignment_stmt])
      end
      max.times do |i|
        dim=i+1
        h=halo[i]
        stmts.push(["#{d}__halosize(#{dim},#{n})=#{(h)?(h):(0)}",:assignment_stmt])
      end
      max.times do |i|
        dim=i+1
        stmts.push(["#{d}__boundarytype(#{dim})=nnt_nonperiodic_bdy",:assignment_stmt])
      end
      max.times do |i|
        dim=i+1
        stmts.push(["#{d}__lowbounds(#{dim},#{n})=1",:assignment_stmt])
      end
      max.times do |i|
        dim=i+1
        stmts.push(["#{d}__upperbounds(#{dim},#{n})=#{d}__globalsize(#{dim},#{n})+#{d}__lowbounds(#{dim},#{n})-1",:assignment_stmt])
      end
      max.times do |i|
        dim=i+1
        g=global[i]
        if g
          stmts.push(["ppp__periodicusedlower(:)=#{d}__lowbounds(:,#{dim})",:assignment_stmt])
          stmts.push(["ppp__periodicusedupper(:)=#{d}__upperbounds(:,#{dim})",:assignment_stmt])
        end
      end
      stmts.push(["#{d}__decompname='#{d}'",:assignment_stmt])
      args=[
        "nnt_decomp_1",
        "#{d}__boundarytype",
        "#{d}__globalsize(1,#{n})",
        "#{d}__halosize(1,#{n})",
        "#{d}__lowbounds(1,#{n})",
        "ppp_null_decomp",
        "#{d}__localsize(1,#{n})",
        "ppp__periodicusedlower(1)",
        "ppp__periodicusedupper(1)",
        "#{d}__local_lb(1,#{n})",
        "#{d}__local_ub(1,#{n})",
        "#{d}__decompname",
        "#{d}(#{n})",
        "ppp_max_decomposed_dims",
        "sms_unstructured",
        "regionsize",
        "ppp__status"
      ]
      stmts.push(["call ppp_decomp(#{args.join(',')})",:call_stmt])
      s=""
      s+="do #{d}__index=0,0\n"
      args=[
        "#{d}(#{n})",
        "1",
        "#{d}__halosize(1,#{n})-#{d}__index",
        "#{d}__s1(1,#{d}__index,#{n})",
        "#{d}__e1(#{global[0]},#{d}__index,#{n})",
        "1",
        "1",
        "#{d}__nregions",
        "ppp__status"
      ]
      s+="call ppp_loops_op(#{args.join(',')})\n"
      s+="end do\n"
      stmts.push([s,:block_do_construct])
      replace_statements(stmts)
    end

  end

  class SMS_Create_Decomp_Global < SMS
    def vars() e[1].vars end
  end

  class SMS_Create_Decomp_Halo < SMS
    def vars() e[1].vars end
  end

  class SMS_Create_Decomp_Regionsize < SMS
    def regionsize() e[3] end
  end

  class SMS_Declare_Decomp < SMS

    def to_s
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]}#{e[7]}#{e[8]}")
    end

    def translate

      # NOTE: Currently, declare() inserts new declarations at the *top* of the
      #       decl section, so that they will appear in the opposite of the
      #       order of declare() calls. This is stupid and has to be fixed. For
      #       now, take care below that parameters are declared before they are
      #       used in specification expressions.

      use("nnt_types_module")
      decomp="#{e[3]}"
      declare("integer","#{decomp}__s3",{:attrs=>["allocatable"],:dims=>%W[: : :]})
      declare("integer","#{decomp}__s2",{:attrs=>["allocatable"],:dims=>%W[: : :]})
      declare("integer","#{decomp}__s1",{:attrs=>["allocatable"],:dims=>%W[: : :]})
      declare("integer","#{decomp}__e3",{:attrs=>["allocatable"],:dims=>%W[: : :]})
      declare("integer","#{decomp}__e2",{:attrs=>["allocatable"],:dims=>%W[: : :]})
      declare("integer","#{decomp}__e1",{:attrs=>["allocatable"],:dims=>%W[: : :]})
      declare("integer","#{decomp}__upperbounds", {:dims=>%W[ppp_max_decomposed_dims #{decomp}__maxnests]})
      declare("integer","#{decomp}__lowbounds",   {:dims=>%W[ppp_max_decomposed_dims #{decomp}__maxnests]})
      declare("integer","#{decomp}__localsize",   {:dims=>%W[ppp_max_decomposed_dims #{decomp}__maxnests]})
      declare("integer","#{decomp}__local_ub",    {:dims=>%W[ppp_max_decomposed_dims #{decomp}__maxnests]})
      declare("integer","#{decomp}__local_lb",    {:dims=>%W[ppp_max_decomposed_dims #{decomp}__maxnests]})
      declare("integer","#{decomp}__halosize",    {:dims=>%W[ppp_max_decomposed_dims #{decomp}__maxnests]})
      declare("integer","#{decomp}__globalsize",  {:dims=>%W[ppp_max_decomposed_dims #{decomp}__maxnests]})
      declare("integer","#{decomp}__boundarytype",{:dims=>%W[ppp_max_decomposed_dims]})
      declare("integer","#{decomp}__nestlevels",  {:dims=>%W[#{decomp}__maxnests]})
      declare("integer",decomp,{:dims=>%W[1]})
      declare("integer","#{decomp}__nregions")
      declare("integer","#{decomp}__nestlevel")
      declare("integer","#{decomp}__localhalosize")
      declare("integer","#{decomp}__index")
      declare("integer","#{decomp}__ignore")
      declare("character*32","#{decomp}__decompname")
      # HACK start: Do parameters last so they will appear first
      declare("integer","#{decomp}__maxnests",{:attrs=>"parameter",:init=>"1"})
      declare("integer","#{decomp}__ppp_max_regions",{:attrs=>"parameter",:init=>"1"})
      # HACK end
      remove
    end

  end

  class SMS_Decomp_Name < SMS
  end

  class SMS_Distribute_Begin < SMS

    def to_s
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]} #{e[7]}")
    end

# HACK start

# Uncomment when legacy ppp doesn't neeed to translate distribute

#   def translate
#     remove
#   end

# HACK end

  end

  class SMS_Distribute_End < SMS

    def to_s
      sms("#{e[2]}")
    end

# HACK start

# Uncomment when legacy ppp doesn't neeed to translate distribute

#   def translate
#     remove
#   end

# HACK end

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
      use("module_decomp")
      use("nnt_types_module")
      tag="ppp__tag_#{newtag}"
      declare("integer",tag,{:attrs=>"save"})
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
        varenv=getvarenv(var)
        dims=varenv["dims"]
        decomp=varenv["decomp"]
        dectypes.push("#{decomp}(#{decomp}__nestlevel)")
        cornerdepth.push("9999")
        ranks.each { |r| gllbs.push((r>dims)?(1):(fixbound(varenv,var,r,:l))) }
        ranks.each { |r| glubs.push((r>dims)?(1):(fixbound(varenv,var,r,:u))) }
        ranks.each { |r| halol.push((r>dims)?(0):("#{decomp}__halosize(1,#{decomp}__nestlevel)")) }
        ranks.each { |r| halou.push((r>dims)?(0):("#{decomp}__halosize(1,#{decomp}__nestlevel)")) }
        ranks.each { |r| perms.push(varenv["dim#{r}"]||0) }
        types.push(smstype(varenv["type"],varenv["kind"]))
      end
      cornerdepth="(/#{cornerdepth.join(",")}/)"
      dectypes="(/#{dectypes.join(",")}/)"
      gllbs="reshape((/#{gllbs.join(",")}/),(/#{nvars},#{maxrank}/))"
      glubs="reshape((/#{glubs.join(",")}/),(/#{nvars},#{maxrank}/))"
      halol="reshape((/#{halol.join(",")}/),(/#{nvars},#{maxrank}/))"
      halou="reshape((/#{halou.join(",")}/),(/#{nvars},#{maxrank}/))"
      perms="reshape((/#{perms.join(",")}/),(/#{nvars},#{maxrank}/))"
      types="(/#{types.join(",")}/)"
      code="call ppp_exchange_#{nvars}(#{tag},#{gllbs},#{glubs},#{gllbs},#{glubs},#{perms},#{halol},#{halou},#{cornerdepth},#{dectypes},#{types},ppp__status,#{vars},#{names})"
      replace_statement(code,:call_stmt)
    end

  end

  class SMS_Halo_Comp < SMS_Region
    def to_s() "#{e[0]}#{e[1]}#{e[2]}" end
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

  class SMS_Ignore < SMS_Region
  end

  class SMS_Ignore_Begin < SMS

    def to_s
      sms("#{e[2]}")
    end

    def translate
    end

  end

  class SMS_Ignore_End < SMS

    def to_s
      sms("#{e[2]}")
    end

    def translate
    end

  end

  class SMS_Parallel < SMS_Region
    def to_s() "#{e[0]}#{e[1]}#{e[2]}" end
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

    def op
      e[5]
    end

    def to_s
      sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]}")
    end

    def translate
      nvars=vars.size
      fail "SMS$REDUCE supports reduction of 25 variables max" if nvars>25
      use("module_decomp")
      sizes=[]
      types=[]
      nvars.times do |i|
        var=vars[i]
        varenv=getvarenv(var)
        fail "SMS$REDUCE inapplicable to distributed array '#{var}'" if varenv["decomp"]
        sizes.push("1") # but why?
        types.push(smstype(varenv["type"],varenv["kind"]))
      end
      sizes="(/#{sizes.join(",")}/)"
      types="(/#{types.join(",")}/)"
      code="call ppp_reduce_#{nvars}(#{sizes},#{types},nnt_#{op},ppp__status,#{vars.join(',')})"
      replace_statement(code,:call_stmt)
    end

    def vars
      e[3].vars
    end

  end

  class SMS_Reduce_Varlist < SMS

    def vars
      list_to_s.split(",")
    end

    def to_s
      list_to_s
    end

  end

  class SMS_Serial < SMS_Region

    def to_s
      "#{e[0]}#{e[1]}#{e[2]}"
    end

    def translate
      use("module_decomp")
      declare("logical","iam_root")
      # Initially, we don't know which variables will need to be gathered,
      # scattered, or broadcast.
      bcasts=[]
      gathers=[]
      scatters=[]
      # Get the serial info recorded when the SMS$SERIAL ... BEGIN statement.
      # was parsed.
      si=self.env[:sms_serial_info]
      # Iterate over the set of names that occurred in the region. Note that
      # we do not yet know whether the names are variables (they might e.g. be
      # function or subroutine names.
      si.names_in_region.each do |name|
        # Skip names with no entries in the environemnt, i.e. that are not
        # variables. HACK: Also skip names with no type information, on the
        # (probably naive) assumption that they are function or subroutine names
        # that are in the environment due to access specification.
        next unless (varenv=getvarenv(name,self,false)) and varenv["type"]
        decomp=varenv["decomp"]
        rank=varenv["rank"]
        # Conservatively assume that the default intent will apply to this
        # variable.
        default=true
        if si.vars_in.include?(name)
          # Explicit 'in' intent was specified for this variable: Ensure that
          # conflicting 'ignore' intent was not specified; schedule the variable
          # for a gather *if* it is decomposed; and note that the default intent
          # does not apply.
          if si.vars_ignore.include?(name)
            fail "'#{name}' cannot be both 'ignore' and 'out' in SMS$SERIAL"
          end
          gathers.push(name) if decomp
          default=false
        end
        if si.vars_out.include?(name)
          # Explicit 'out' intent was specified for this variable. Ensure that
          # conflicting 'ignore' intent was not specified; schedule scalars and
          # non-decomposed arrays for broadcast, and decomposed arrays for a
          # scatter; and note that the default intent does not apply.
          if si.vars_ignore.include?(name)
            fail "'#{name}' cannot be both 'ignore' and 'in' SMS$SERIAL"
          end
          ((rank=="_scalar"||!decomp)?(bcasts):(scatters)).push(name)
          default=false
        end
        if default and not si.vars_ignore.include?(name)
          # If no explicit intent was specified, the default applies. Treatment
          # of scalars, non-decomposed arrays and decomposed arrays is the same
          # as described above.
          d=si.default
          gathers.push(name) if decomp and (d=="in" or d=="inout")
          if d=="out" or d=="inout"
            ((rank=="_scalar"||!decomp)?(bcasts):(scatters)).push(name)
          end
        end
      end
      # Wrap old block in conditional. Note that 'begin' and 'end' nodes have
      # already been removed, so the only element remaining is the block.
      oldblock=e[0]
      code=[]
      code.push("if (iam_root()) then")
      code.push("#{oldblock}")
      code.push("endif")
      code=code.join("\n")
      t=replace_statement(code,:block,oldblock)
      oldblock=t.e[0].e[1].e[1]
      newblock=e[0]
      # Insert statements for the necessary gathers, scatters and broadcasts.
      # Declaration of globally-sized variables
      globals=Set.new(gathers+scatters)
      globals.sort.each do |var|
        varenv=getvarenv(var)
        dims=(":"*varenv["dims"]).split("")
        declare(varenv["type"],"ppp__g_#{var}",{:attrs=>["allocatable"],:dims=>dims})
      end
      # Gathers
      gathers.each do |var|
        varenv=getvarenv(var)
        decomp=varenv["decomp"]
        dims=varenv["dims"]
        type=smstype(varenv["type"],varenv["kind"])
        gllbs="(/"+ranks.map { |r| (r>dims)?(1):(fixbound(varenv,var,r,:l)) }.join(",")+"/)"
        glubs="(/"+ranks.map { |r| (r>dims)?(1):(fixbound(varenv,var,r,:u)) }.join(",")+"/)"
        gstop="(/"+ranks.map { |r| (r>dims)?(0):("ppp__globalupper") }.join(",")+"/)"
        gstrt="(/"+ranks.map { |r| (r>dims)?(0):("ppp__globallower") }.join(",")+"/)"
        perms="(/"+ranks.map { |r| varenv["dim#{r}"]||0 }.join(",")+"/)"
        if dh=varenv["decomp"]
          decomp="#{dh}(#{decomp}__nestlevel)"
        else
          decomp="ppp_not_decomposed"
        end
        args=[]
        args.push("ppp_max_rank")
        args.push("ppp_max_vars")
        args.push("#{gllbs}")
        args.push("#{glubs}")
        args.push("#{gstrt}")
        args.push("#{gstop}")
        args.push("#{perms}")
        args.push("#{decomp}")
        args.push("#{type}")
        args.push(".false.") # but why?
        args.push("#{var}")
        args.push("ppp__g_#{var}")
        args.push("ppp__status")
        code="call sms_gather(#{args.join(",")})"
        insert_statement_before(code,:call_stmt,newblock.e.first)
      end
      # Allocation of globally-sized variables. On non-root tasks, allocate all
      # dimensions at unit size. On the root task, allocate the non-decomposed
      # dimensions at local size, and the decomposed dimensions at the global
      # size indicated by the decomposition info.
      globals.sort.each do |var|
        varenv=getvarenv(var)
        decomp=varenv["decomp"]
        dims=varenv["dims"]
        bounds_root=[]
        (1..dims).each do |i|
          bounds_root.push((decdim=varenv["dim#{i}"])?("#{decomp}__globalsize(#{decdim},#{decomp}__nestlevel)"):("size(#{var},#{i})"))
        end
        bounds_root=bounds_root.join(",")
        bounds_nonroot=("1"*dims).split("").join(",")
        code=[]
        code.push("if (iam_root()) then")
        code.push("allocate(ppp_g_#{var}(#{bounds_root}),stat=ppp__status)")
        code.push("else")
        code.push("allocate(ppp_g_#{var}(#{bounds_nonroot}),stat=ppp__status)")
        code.push("endif")
        code=code.join("\n")
        insert_statement_before(code,:if_construct,newblock.e.first)
      end
      # Scatters
      scatters.each do |var|
        tag="ppp__tag_#{newtag}"
        declare("integer",tag,{:attrs=>"save"})
        varenv=getvarenv(var)
        decomp=varenv["decomp"]
        dims=varenv["dims"]
        type=smstype(varenv["type"],varenv["kind"])
        gllbs="(/"+ranks.map { |r| (r>dims)?(1):(fixbound(varenv,var,r,:l)) }.join(",")+"/)"
        glubs="(/"+ranks.map { |r| (r>dims)?(1):(fixbound(varenv,var,r,:u)) }.join(",")+"/)"
        gstop="(/"+ranks.map { |r| (r>dims)?(0):("ppp__globalupper") }.join(",")+"/)"
        gstrt="(/"+ranks.map { |r| (r>dims)?(0):("ppp__globallower") }.join(",")+"/)"
        halol="(/"+ranks.map { |r| (r>dims)?(0):("#{decomp}__halosize(1,#{decomp}__nestlevel)") }.join(",")+"/)"
        halou="(/"+ranks.map { |r| (r>dims)?(0):("#{decomp}__halosize(1,#{decomp}__nestlevel)") }.join(",")+"/)"
        perms="(/"+ranks.map { |r| varenv["dim#{r}"]||0 }.join(",")+"/)"
        if dh=varenv["decomp"]
          decomp="#{dh}(#{decomp}__nestlevel)"
        else
          decomp="ppp_not_decomposed"
        end
        args=[]
        args.push("ppp_max_rank")
        args.push("ppp_max_vars")
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
        args.push(".false.") # but why?
        args.push("ppp__g_#{var}")
        args.push("#{var}")
        args.push("ppp__status")
        code="call sms_scatter(#{args.join(",")})"
        insert_statement_after(code,:call_stmt,newblock.e.last)
      end
      # Broadcasts
      bcasts.each do |var|
        varenv=getvarenv(var)
        if varenv["rank"]=="_scalar"
          dims="1"
          sizes="(/1/)"
        else
          dims=varenv["dims"]
          sizes="(/#{(1..dims).reduce([]) { |m,x| m.push("size(a,#{x})") }.join(",")}/)"
        end
        code="call ppp_bcast(#{var},#{smstype(varenv["type"],varenv["kind"])},#{sizes},#{dims},ppp__status)"
        insert_statement_after(code,:call_stmt,newblock.e.last)
      end
      # Deallocation of globally-sized variables
      globals.sort.each do |var|
        code="deallocate(ppp__g_#{var})"
        insert_statement_after(code,:deallocate_stmt,newblock.e.last)
      end
    end

  end

  class SMS_Serial_Begin < SMS

    def to_s
      sms("#{sa(e[2])}#{e[3]}")
    end

    def translate
      si=self.env[:sms_serial_info]=OpenStruct.new
      si.default=("#{e[2]}".empty?)?("inout"):(e[2].default)
      si.names_in_region=Set.new
      si.vars_ignore=("#{e[2]}".empty?)?([]):(e[2].vars_ignore)
      si.vars_in=("#{e[2]}".empty?)?([]):(e[2].vars_in)
      si.vars_out=("#{e[2]}".empty?)?([]):(e[2].vars_out)
      parent.env[:sms_serial_info]=self.env[:sms_serial_info]
      remove
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
      (e[1].e&&e[1].e[1].respond_to?(:intent))?(e[1].e[1].intent):("inout")
    end

    def to_s
      "#{e[0]}#{e[1].cat}"
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
      "#{e[2]}"
    end

  end

  class SMS_Serial_End < SMS

    def to_s
      sms("#{e[2]}")
    end

    def translate
      remove
    end

  end

  class SMS_Serial_Intent_List < SMS

    def intent
      "#{e[3]}"
    end

    def vars
      e[1].vars
    end

  end

  class SMS_Serial_Intent_Lists < SMS

    def vars_with_intent(intent)
      vars=(e[0].intent==intent)?(e[0].vars):([])
      e[1].e.each { |x| vars+=x.e[1].vars if x.e[1].intent==intent }
      vars
    end

    def to_s
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

    def vars
      list_to_s.split(",")
    end

    def to_s
      list_to_s
    end

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

  class SMS_To_Local < SMS_Region
    def to_s() "#{e[0]}#{e[1]}#{e[2]}" end
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
      var="#{e[3]}"
      fail "No module info found for variable '#{var}'" unless (varenv=getvarenv(var))
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

  class SMS_Varlist3D_1 < SMS
    def vars() [e[0],e[2],e[4]] end
  end

  class SMS_Varlist3D_2 < SMS
    def vars() [e[1],e[3]] end
  end

  class SMS_Varlist3D_3 < SMS
    def vars() [e[2]] end
  end

  class SMS_Varlist3D_4 < SMS
    def vars() [e[0],e[2]] end
  end

  class SMS_Varlist3D_5 < SMS
    def vars() [e[1]] end
  end

  class SMS_Varlist3D_6 < SMS
    def vars() [e[0]] end
  end

  class T < Treetop::Runtime::SyntaxNode

    def distribute_array_bounds(spec,varenv)
      if (decomp=varenv["decomp"])
        if spec and spec.is_a?(Explicit_Shape_Spec_List)
          cb=spec.concrete_boundslist
          newbounds=[]
          cb.each_index do |i|
            b=cb[i]
            arrdim=i+1
            if (decdim=varenv["dim#{arrdim}"])
              s="#{decomp}__local_lb(#{decdim},#{decomp}__nestlevel):"+
                "#{decomp}__local_ub(#{decdim},#{decomp}__nestlevel)"
            else
              s=(b.clb=="1")?(b.cub):("#{b.clb}:#{b.cub}")
            end
            newbounds.push(s)
          end
          code=newbounds.join(",")

# HACK start

# Uncomment when legacy ppp doesn't neeed to translate distribute

#         replace_element(code,:array_spec,spec)

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
