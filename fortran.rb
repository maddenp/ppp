module Fortran

  @@access="_default"
  @@current_name=nil
  @@distribute=nil
  @@dolabels=[]
  @@envstack=[{}]
  @@halocomp=false
  @@incdirs=[]
  @@level=0
  @@levelstack=[]
  @@parallel=false
  @@serial=false
  @@tolocal=false
  @@tag=-1
  @@uses={}

  def array_props(array_spec,_props)
    dims=0
    array_spec.boundslist.each_index do |i|
      arrdim=i+1
      _props["lb#{arrdim}"]=bounds=array_spec.boundslist[i].lb
      _props["ub#{arrdim}"]=bounds=array_spec.boundslist[i].ub
      if @@distribute and decompdim=@@distribute["dim"].index(arrdim)
        _props["decomp"]=@@distribute["decomp"]
        _props["dim#{arrdim}"]=decompdim+1
      end
      dims+=1
    end
    _props["dims"]||=dims
    _props
  end

  def attrany(attr,e=nil)
    (e||self.e).reduce(false) { |m,x| m||=attrchk(x,attr) }
  end

  def attrchk(node,attr)
    node.respond_to?(attr) && node.send(attr)
  end

  def bb(s)
    @@level+=1
    s
  end

  def be
    @@level-=1 if @@level>0
  end

  def cat(f=nil)
    send(f) unless f.nil?
    self.e.map { |x| x.to_s }.join
  end

  def dolabel_dupe?
    "#{@@dolabels[-1]}"=="#{@@dolabels[-2]}"
  end

  def dolabel_pop(label)
    @@dolabels.pop
  end

  def dolabel_push(label)
    @@dolabels.push(label)
  end

  def env
    @@envstack.last
  end

  def envpop
    @@envstack.pop
    @@envstack=[{}] if @@envstack.empty?
  end

  def envpush(base=env.dup)
    @@envstack.push(base)
  end

  def envswap(new)
    @@envstack.pop
    @@envstack.push(new)
  end

  def indent(s)
    " "*2*@@level+s
  end

  def lr
    @@level=@@levelstack.pop
  end

  def ls
    @@levelstack.push(@@level)
  end

  def mn(p,c,v)
    (p.to_s!=c)?(v):(p)
  end

  def mp(p,c,v)
    (p.to_s==c)?(v):(p)
  end

  def modenv(m)
    if d=@@incdirs.find_all { |x| File.exist?(smsfile(m,x)) }[0]
      f=smsfile(m,d)
      begin
        return YAML.load(File.open(f))
      rescue Exception=>ex
        ex.backtrace.each { |x| puts x }
        fail "Error reading #{f}"
      end
    end
    {}
  end

  def msg(s)
    $stderr.write(">|#{s}|<\n")
  end

  def nonblock_do_end?(node)
    return false unless node.respond_to?(:label)
    return false if node.label.to_s.empty?
    ("#{node.label}"=="#{@@dolabels.last}")?(true):(false)
  end

  def nonblock_do_end!(node)
    @@dolabels.pop if nonblock_do_end?(node)
  end

  def sa(e)
    (e.to_s=="")?(""):("#{e} ")
  end

  def sb(e)
    (e.to_s=="")?(""):(" #{e}")
  end

  def sms(s)
    "#{e[0]}#{e[1]} #{s}\n"
  end

  def smsfile(m,d=".")
    File.join(File.expand_path(d),"#{m}.sms")
  end

  def sp_access_stmt(access_spec,access_stmt_option)
    if access_spec.private?
      p="private"
    elsif access_spec.public?
      p="public"
    else
      p="_default"
    end
    if access_stmt_option.is_a?(Access_Stmt_Option)
      access_stmt_option.names.each { |x| varsetprop(x,"access",p) }
    else
      @@access=p
      env.each do |n,h|
        varsetprop(n,"access",p) if vargetprop(n,"access")=="_default"
      end
    end
    true
  end

  def sp_assumed_shape_spec_list(assumed_shape_spec_list)
    return false unless env[:args] and env[:args].include?(@@current_name)
    true
  end

  def sp_block_data_stmt
    envpush
    true
  end

  def sp_deferred_shape_spec_list(deferred_shape_spec_list)
    true
  end

  def sp_dimension_stmt(array_names_and_specs)
    array_names_and_specs.e.each do |x|
      if x.is_a?(Array_Name_And_Spec)
        key="#{x.e[0]}"
        array_spec=x.e[2].e[0]
        env[key]||={}
        env[key].merge!(array_props(array_spec,{}))
      end
    end
    array_names_and_specs.names.each { |x|varsetprop(x,"rank","array") }
    true
  end

  def sp_end_block_data_stmt
    envpop
    true
  end

  def sp_end_function_stmt
    envpop
    true
  end

  def sp_end_program_stmt
    envpop
    true
  end

  def sp_end_subroutine_stmt
    envpop
    true
  end

  def sp_explicit_shape_spec_list(explicit_shape_spec_list)
    true
  end

  def sp_function_stmt(dummy_arg_name_list)
    envpush
    if dummy_arg_name_list.is_a?(Dummy_Arg_Name_List)
      first=dummy_arg_name_list.e[0].e[0]
      rest=dummy_arg_name_list.e[1].e
      env[:args]=["#{first}"]+rest.reduce([]) { |m,x| m.push("#{x.e[1]}") }
    end
    true
  end

  def sp_is_array?(node)
    return false unless node.respond_to?(:name)
    vargetprop(node.name,"rank")=="array"
  end

  def sp_module(_module)
    modulename=_module.e[0].name
    modinfo=env.delete_if { |k,v| v["access"]=="private" }
    unless modinfo.empty?
      File.open(smsfile(modulename),"w") { |f| f.write(YAML.dump(modinfo)) }
    end
    envpop
    @@access="_default"
    true
  end

  def sp_module_stmt
    envpush
    true
  end

  def sp_name(first,rest)
    @@current_name="#{first}"+rest.e.reduce("") { |m,x| m+="#{x}" }
    true
  end

  def sp_nonlabel_do_stmt(nonlabel_do_stmt)
    nonlabel_do_stmt.myenv=env if @@parallel
    true
  end

  def sp_program_stmt
    envpush
    true
  end

  def sp_sms_declarative(sms_declarative)
    sms_declarative.myenv=env
    true
  end

  def sp_sms_distribute_begin(sms_decomp_name,sms_distribute_dims)
    @@distribute={"decomp"=>"#{sms_decomp_name}","dim"=>[]}
    sms_distribute_dims.dims.each { |x| @@distribute["dim"].push(x) }
    true
  end

  def sp_sms_distribute_end
    @@distribute=nil
    true
  end

  def sp_sms_executable(sms_executable)
    sms_executable.myenv=env
    true
  end

  def sp_sms_halo_comp_begin(halo_comp_pairs)
    fail "Halo computation invalid outside parallel region" unless @@parallel
    fail "Already inside halo-computation region" if @@halocomp
    envpush
    dims={}
    dims[1]=halo_comp_pairs.e[0]
    dims[2]=halo_comp_pairs.e[1].e[1] unless halo_comp_pairs.e[1].e.nil?
    dims[3]=halo_comp_pairs.e[2].e[1] unless halo_comp_pairs.e[2].e.nil?
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

  def sp_subroutine_stmt(dummy_arg_list_option)
    envpush
    if dummy_arg_list_option.e
      if dummy_arg_list_option.e[1].is_a?(Dummy_Arg_List)
        dummy_arg_list=dummy_arg_list_option.e[1]
        first=dummy_arg_list.e[0].e[0]
        rest=dummy_arg_list.e[1].e
        env[:args]=["#{first}"]+rest.reduce([]) { |m,x| m.push("#{x.e[1]}") }
      end
    end
    true
  end

  def sp_type_declaration_stmt(type_spec,attr_spec_option,entity_decl_list)
    varprops=entity_decl_list.varprops
    varprops.each do |v,p|
      p["type"]=type_spec.type
      p["kind"]=type_spec.kind
    end
    if x=attrchk(attr_spec_option,:dimension?)
      array_spec=x.e[0]
      varprops.each do |v,p|
        p["rank"]="array"
        array_props(array_spec,p)
      end
    end
    if attrchk(attr_spec_option,:private?)
      varprops.each { |v,p| p["access"]="private" }
    elsif attrchk(attr_spec_option,:public?)
      varprops.each { |v,p| p["access"]="public" }
    else
      varprops.each { |v,p| p["access"]=@@access }
    end
    varprops.each do |v,p|
      varenv=env["#{v}"]||={}
      ["access","rank"].each { |x| p.delete(x) if varenv.include?(x) }
      varenv.merge!(p)
    end
    true
  end

  def sp_use_stmt(modulename,list)
    m="#{modulename}"
    if list.respond_to?(:usenames)
      use_add(m,list.usenames,list.localnames)
    else
      @@uses[m]=[[:all]]
    end
    modenv(m).each do |x|
      varname=x[0]
      varprop=x[1]
      localname=use_localname(m,varname)
      if uses?(m,:all) or uses?(m,localname)
        env[localname]=varprop
      end
    end
    true
  end

  def space(all=false)
    a=(all)?(self.e):(self.e[1..-1])
    a.map { |x| x.to_s }.join(" ").strip
  end

  def stmt(s,f=nil)
    send(f) unless f.nil?
    indent(("#{sa(e[0])}"+s.chomp).strip)+"\n"
  end

  def translate()
    e.each { |x| x.translate } unless e.nil?
    self
  end

  def use_add(modulename,usenames,localnames)
    names=localnames.zip(usenames)
    if @@uses[modulename].nil?
      @@uses[modulename]=names
    else
      unless uses?(modulename,:all)
        names.each do |x|
          @@uses[modulename].push(x) unless uses?(modulename,x)
        end
      end
    end
  end

  def use_localname(modulename,usename)
    if i=use_usenames(modulename).index(usename)
      return use_localnames(modulename)[i]
    end
    return usename
  end

  def use_localnames(modulename)
    @@uses[modulename].map { |x| x[0] }
  end

  def use_usenames(modulename)
    @@uses[modulename].map { |x| x[1] }
  end

  def use_part
    specification_part.e[0]
  end

  def uses?(modulename,usename)
    (@@uses[modulename])?(use_localnames(modulename).include?(usename)):(false)
  end

  def vargetprop(n,k)
    return nil unless env["#{n}"]
    env["#{n}"]["#{k}"]||nil
  end

  def varsetprop(n,k,v)
    env["#{n}"]||={}
    env["#{n}"]["#{k}"]=v
  end

  # Extension of SyntaxNode class

  class Treetop::Runtime::SyntaxNode

    alias e elements

    def to_s() "" end

  end

  # Generic Subclasses

  class T < Treetop::Runtime::SyntaxNode

    attr_accessor :myenv

    def initialize(a="",b=(0..0),c=[])
      super(a,b,c)
      @myenv=env
    end

    def declaration_constructs
      specification_part.e[2]
    end

    def declare(type,name,attrs=[])
      p=declaration_constructs
      envget(p)
      varenv=env[name]
      if varenv
        fail "Variable #{name} is already defined" unless varenv["pppvar"]
      else
        attrs=(attrs.empty?)?(""):(",#{attrs.join(",")}")
        code="#{type}#{attrs}::#{name}"
        t=raw(code,:type_declaration_stmt)
        t.parent=p
        p.e.insert(0,t) # prefer "p.e.push(t)" -- see TODO
        env[name]["pppvar"]=true
      end
      envget
    end

    def enclosing(classes)
      nearest(classes,self.parent)
    end

    def envget(node=self)
      envswap(node.myenv) unless node.myenv.object_id==env.object_id
    end

    def execution_part
      scoping_unit.e[2]
    end

    def halo_offsets(decdim)
      halo_lo=0
      halo_up=0
      if halocomp=env[:halocomp]
        offsets=halocomp[decdim]
        halo_lo=offsets.lo
        halo_up=offsets.up
      end
      OpenStruct.new({:lo=>halo_lo,:up=>halo_up})
    end

    def insert_statement(code,rule,predecessor)
      tree=raw(code,rule)
      tree.parent=predecessor.parent
      block=predecessor.parent.e
      block.insert(block.index(predecessor)+1,tree)
      tree
    end

    def inside?(classes)
      (enclosing(classes))?(true):(false)
    end

    def nearest(classes,n=self)
      begin
        return n if classes.any? { |x| n.is_a?(x) }
      end while n=n.parent
      nil
    end

    def remove
      self.parent.e.delete(self)
    end

    def replace_element(code,rule)
      tree=raw(code,rule,{:nl=>false})
      node=self
      node=node.parent while "#{node}"=="#{node.parent}"
      tree.parent=node.parent
      block=node.parent.e
      block[block.index(node)]=tree
    end

    def replace_statement(code,rule)
      tree=raw(code,rule)
      tree.parent=self.parent
      block=self.parent.e
      block[block.index(self)]=tree
    end

    def replace_statements(stmt_pairs)
      p=stmt_pairs.shift
      s=replace_statement(p[0],p[1])
      stmt_pairs.each { |p| s=insert_statement(p[0],p[1],s) }
    end

    def scoping_unit
      enclosing([Scoping_Unit])
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

    def specification_part
      scoping_unit.e[1]
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
        unless code.nil?
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

    def to_s()
      text_value
    end

  end

  class E < T
    def to_s() cat end
  end

  class J < T
    def to_s() space(true) end
  end

  class Scoping_Unit < E
  end

  class StmtC < T
    def to_s() stmt(e[1..-1].map { |x| x.to_s }.join) end
  end

  class StmtJ < T
    def to_s() stmt(space) end
  end

  # Specific Subclasses

  class Access_Id_List < T
    def names() [e[0].name]+e[1].e.inject([]) { |m,x| m.push(x.name) } end
  end

  class Access_Id_List_Pair < T
    def name() "#{e[1]}" end
  end

  class Access_Spec < T
    def private?() "#{text_value}"=="private" end
    def public?() "#{text_value}"=="public" end
  end

  class Access_Stmt < StmtC
  end

  class Access_Stmt_Option < T
    def names() e[1].names end
    def to_s() "#{mn(e[0],"::"," ")}#{e[1]}" end
  end

  class Allocatable_Stmt < T
    def to_s() stmt("#{e[1]}#{mp(e[2],""," ")}#{e[3]}") end
  end

  class Arithmetic_If_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]} #{e[5]}#{e[6]}#{e[7]}#{e[8]}#{e[9]}") end
  end

  class Array_Name_And_Spec < E
    def name() "#{e[0]}" end
  end

  class Array_Name_And_Spec_Pair < E
    def name() e[1].name end
  end

  class Array_Names_And_Specs < T
    def names() [e[0].name]+e[1].e.inject([]) { |m,x| m.push(x.name) } end
  end

  class Array_Section < E
    def name() e[0].name end
  end

  class Assigned_Goto_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{mn(e[3],","," "+e[3].to_s)}") end
  end

  class Assumed_Shape_Spec < T
    def bounds() OpenStruct.new({:lb=>lb,:ub=>ub}) end
    def boundslist() ex+[bounds] end
    def ex() (e[0].respond_to?(:boundslist))?(e[0].boundslist):([]) end
    def lb() (e[0].respond_to?(:lb))?(e[0].lb):("_default") end
    def ub() "_assumed" end
  end

  class Assumed_Shape_Spec_List < T
    def boundslist() e[1].e.reduce([e[0].bounds]) { |m,x| m.push(x.bounds) } end
  end

  class Assumed_Shape_Spec_List_Pair < T
    def bounds() e[1].bounds end
  end

  class Assumed_Size_Spec < T
    def bounds() OpenStruct.new({:lb=>lb,:ub=>ub}) end
    def boundslist() ex+[bounds] end
    def ex() (e[0].respond_to?(:boundslist))?(e[0].boundslist):([]) end
    def lb() ("#{e[1]}".empty?)?("_default"):(e[1].lb) end
    def ub() "_assumed" end
  end

  class Assumed_Size_Spec_Pair < T
    def boundslist() e[0].boundslist end
  end

  class Attr_Spec_Base < T
    def dimension?() attrany(:dimension?) end
    def private?() attrany(:private?) end
    def public?() attrany(:public?) end
  end

  class Attr_Spec_Dimension < T
    def dimension?() e[2] end
  end

  class Attr_Spec_List < Attr_Spec_Base
  end

  class Attr_Spec_List_Pair < Attr_Spec_Base
  end

  class Attr_Spec_List_Pairs < Attr_Spec_Base
    def dimension?() (e[0])?(attrany(:dimension?,e[0].e)):(false) end
    def private?() (e[0])?(attrany(:private?,e[0].e)):(false) end
    def public?() (e[0])?(attrany(:public?,e[0].e)):(false) end
  end

  class Attr_Spec_Option < Attr_Spec_Base
  end

  class Block_Data < Scoping_Unit
  end

  class Block_Data_Stmt < T
    def to_s() bb(stmt(space)) end
  end

  class Call_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}") end
  end

  class Case_Stmt < T
    def to_s() bb(stmt(space,:be)) end
  end

  class Common_Block_Name_And_Object_List < T
    def to_s() "#{mp(e[0],""," ")}#{e[1]}#{e[2]}" end
  end

  class Common_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}") end
  end

  class Component_Def_Stmt < T
    def to_s() stmt("#{e[1]}#{mp(e[2],""," ")}#{e[3]}") end
  end

  class Computed_Goto_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}#{mp(e[5],""," ")}#{e[6]}") end
  end

  class Contains_Stmt < T
    def to_s() bb(stmt(space,:be)) end
  end

  class Data_Ref < T
    def name() (e[1].e.empty?)?(e[0].name):(e[1].e[-1].e[1].name) end
  end

  class Deferred_Shape_Spec < T
    def bounds() OpenStruct.new({:lb=>lb,:ub=>ub}) end
    def lb() "_deferred" end
    def ub() "_deferred" end
  end

  class Deferred_Shape_Spec_List < T
    def boundslist() e[1].e.reduce([e[0].bounds]) { |m,x| m.push(x.bounds) } end
  end

  class Deferred_Shape_Spec_List_Pair < T
    def bounds() e[1].bounds end
  end

  class Derived_Type_Stmt < T
    def to_s() bb(stmt("#{e[1]}#{sb(e[2])} #{e[3]}")) end
  end

  class Dimension_Stmt < T
    def to_s() stmt("#{e[1]}#{mp(e[2],""," ")}#{e[3]}") end
  end

  class Do_Term_Action_Stmt < T
    def to_s() stmt(space,:be) end
  end

  class Do_Term_Shared_Stmt < T
    def to_s() cat(:be) end
  end

  class Double_Colon < T
  end

  class Dummy_Arg_List < T
  end

  class Dummy_Arg_Name_List < T
  end

  class Else_If_Stmt < T
    def to_s() bb(stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]} #{e[5]}",:be)) end
  end

  class Else_Stmt < T
    def to_s() bb(stmt(space,:be)) end
  end

  class Elsewhere_Stmt < T
    def to_s() bb(stmt(space,:be)) end
  end

  class End_Block_Data_Option < T
    def to_s() space(true) end
  end

  class End_Block_Data_Stmt < T
    def to_s() stmt(space,:be) end
  end

  class End_Do_Stmt < T
    def to_s() stmt(space,:be) end
  end

  class End_Function_Stmt < T
    def to_s() stmt(space,:be) end
  end

  class End_If_Stmt < T
    def to_s() stmt(space,:be) end
  end

  class End_Interface_Stmt < T
    def to_s() stmt(space,:be) end
  end

  class End_Module_Option < T
    def to_s() space(true) end
  end

  class End_Module_Stmt < T
    def to_s() stmt(space,:be) end
  end

  class End_Program_Stmt < T
    def to_s() stmt("#{e[1]}#{sb(e[3])}#{sb(e[4])}",:be) end
  end

  class End_Select_Stmt < T
    def to_s() stmt(space,:lr) end
  end

  class End_Subroutine_Stmt < T
    def to_s() stmt(space,:be) end
  end

  class End_Type_Stmt < T
    def to_s() stmt(space,:be) end
  end

  class End_Where_Stmt < T
    def to_s() stmt(space,:be) end
  end

  class Entity_Decl < T
    def name() "#{e[0]}" end
    def props()
      _props={}
      if e[1].is_a?(Entity_Decl_Array_Spec)
        array_props(e[1].e[1].e[0],_props)
      end
      _props["rank"]=((array?)?("array"):("scalar"))
      {name=>_props}
    end
  end

  class Entity_Decl_1 < Entity_Decl
    def array?() e[1].is_a?(Entity_Decl_Array_Spec) end
  end

  class Entity_Decl_2 < Entity_Decl
    def array?() false end # need to determine array/scalar spec of named function here?
  end

  class Entity_Decl_Array_Spec < T
  end

  class Entity_Decl_List < T
    def varprops
      e[0].props.merge(e[1].props)
    end
  end

  class Entity_Decl_List_Pair < T
    def array?() e[1].array? end
    def name() e[1].name end
    def props() e[1].props end
  end

  class Entity_Decl_List_Pairs < T
    def props() e.reduce({}) { |m,x| m.merge(x.props) } end
  end

  class Entry_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}#{sb(e[4])}") end
  end

  class Execution_Part < E
  end

  class Explicit_Shape_Spec < T
    def bounds() OpenStruct.new({:lb=>lb,:ub=>ub}) end
    def lb() "_explicit" end
    def ub() "_explicit" end
  end

  class Explicit_Shape_Spec_List < T
    def boundslist() e[1].e.reduce([e[0].bounds]) { |m,x| m.push(x.bounds) } end
  end

  class Explicit_Shape_Spec_List_Pair < T
    def bounds() e[1].bounds end
  end

  class Function_Reference < T
  end

  class Function_Stmt < T
    def to_s() bb(stmt("#{sa(e[1])}#{e[2]} #{e[3]}#{e[4]}#{e[5]}#{e[6]}#{sb(e[7])}")) end
  end

  class Function_Subprogram < Scoping_Unit
  end

  class Generic_Spec < T
    def localname() usename end
    def name() usename end
    def usename() "#{e[2]}" end
  end

  class If_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]} #{e[5].to_s.strip}") end
  end

  class If_Then_Stmt < T
    def to_s() bb(stmt("#{e[1]} #{e[2]} #{e[3]}#{e[4]}#{e[5]} #{e[6]}")) end
  end

  class Implicit_None_Stmt < E
    def to_s() stmt(space) end
  end

  class Implicit_Stmt < E
    def to_s() stmt(space) end
  end

  class Inner_Shared_Do_Construct < T
    def to_s() cat(:be) end
  end

  class Intent_Stmt < T
    def to_s() stmt("#{e[1]}#{e[2]}#{e[3]}#{e[4]}#{mn(e[5],"::"," ")}#{e[6]}") end
  end

  class Interface_Body < E
  end

  class Interface_Body_1 < Interface_Body
  end

  class Interface_Body_2 < Interface_Body
  end

  class Interface_Stmt < T
    def to_s() bb(stmt(space)) end
  end

  class Kind_Selector < T
    def kind() "#{e[2]}" end
  end

  class Label_Do_Stmt < T
    def to_s() bb(stmt("#{sa(e[1])}#{e[2]} #{e[3]}#{e[4]}")) end
  end

  class Loop_Control < T
  end

  class Loop_Control_1 < Loop_Control
    def to_s() "#{mp(e[0],""," ")}#{e[1]}#{e[2]}#{e[3]}#{e[4]}#{e[5]}" end
  end

  class Loop_Control_2 < Loop_Control
    def to_s() "#{mp(e[0],""," ")}#{e[1]} #{e[2]}#{e[3]}#{e[4]}" end
  end

  class Loop_Control_Pair < T
    def value() e[1] end
  end

  class Lower_Bound_Pair < T
    def lb() "#{e[0]}" end
  end

  class Main_Program < Scoping_Unit
  end

  class Module < Scoping_Unit
  end

  class Module_Stmt < T
    def name() "#{e[2]}" end
    def to_s() bb(stmt(space)) end
  end

  class Module_Subprogram_Part < T
    def to_s() "#{e[0]}#{e[1].e.reduce("") { |m,x| m+="#{x}" } }" end
  end

  class Name < T

    def name
      to_s
    end

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

  class Namelist_Group_Set_Pair < T
    def to_s() "#{mp(e[0],""," ")}#{e[1]}" end
  end

  class Namelist_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}") end
  end

  class Nonlabel_Do_Stmt < T

    def to_s
      bb(stmt("#{sa(e[1])}#{e[2]}#{e[3]}"))
    end

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

  class Only < E
    def localname() (e[0].is_a?(Only_Option))?(e[0].localname):(usename) end
    def usename() "#{e[1]}" end
  end

  class Only_List < T
    def localnames() e[1].e.reduce([e[0].localname]) { |m,x| m.push(x.localname) } end
    def usenames() e[1].e.reduce([e[0].usename]) { |m,x| m.push(x.usename) } end
  end

  class Only_List_Pair < T
    def localname() e[1].localname end
    def usename() e[1].usename end
  end

  class Only_Option < T
    def localname() "#{e[0]}" end
  end

  class Optional_Stmt < T
    def to_s() stmt("#{e[1]}#{mn(e[2],"::"," ")}#{e[3]}") end
  end

  class Parenthesized_Explicit_Shape_Spec_List < T
    def boundslist() e[1].boundslist end
  end

  class Part_Ref < T
    def name() e[0].name end
  end

  class Pointer_Stmt < T
    def to_s() stmt("#{e[1]}#{mp(e[2],""," ")}#{e[3]}") end
  end

  class Print_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}") end
  end

  class Program_Stmt < T
    def to_s() bb(stmt(space)) end
  end

  class Program_Units < T
    def to_s() e.reduce("") { |m,x| m+="#{x}\n" }.chomp end
  end

  class Read_Stmt < T
  end

  class Read_Stmt_1 < Read_Stmt
    def to_s() stmt("#{e[1]}#{e[2]}#{e[3]}#{e[4]}#{sb(e[5])}") end
  end

  class Read_Stmt_2 < Read_Stmt
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}") end
  end

  class Rename < E
    def localname() "#{e[0]}" end
    def usename() "#{e[2]}" end
  end

  class Rename_List < E
    def localnames() e[1].e.reduce([e[0].localname]) { |m,x| m.push(x.localname) } end
    def usenames() e[1].e.reduce([e[0].usename]) { |m,x| m.push(x.usename) } end
  end

  class Rename_List_Pair < E
    def localname() e[1].localname end
    def usename() e[1].usename end
  end

  class Rename_List_Option < T
    def localnames() e[1].localnames end
    def usenames() e[1].usenames end
  end

  class Save_Stmt < T
    def to_s() stmt("#{e[1]}#{e[2]}") end
  end

  class Save_Stmt_Entity_List < T
    def to_s() "#{mp(e[0],""," ")}#{e[1]}" end
  end

  class Select_Case_Stmt < T
    def to_s() bb(bb(stmt("#{sa(e[1])}#{e[2]} #{e[3]} #{e[4]}#{e[5]}#{e[6]}",:ls))) end
  end

  class Specification_Part < E
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
      fail "'#{var}' not found in environment" unless varenv=env[var]
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

  class SMS_Distribute < SMS
    def to_s() "#{e[0]}#{e[1].e.reduce("") { |m,x| m+=x.to_s }}#{e[2]}" end
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
        fail "'#{var}' not found in environment" unless varenv=env[var]
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
      fail "No module info found for variable '#{var}'" unless varenv=env[var]
      fail "No decomp info found for variable '#{var}'" unless dh=varenv["decomp"]
      use("nnt_types_module")
      stmts=[]
      stmts.push(["call sms_unstructuredgrid(#{dh},size(#{var},1),#{var})",:call_stmt])
      stmts.push(["call ppp_get_collapsed_halo_size(#{dh}(#{dh}__nestlevel),1,1,#{dh}__localhalosize,ppp__status)",:call_stmt])
      stmts.push(["#{dh}__s1(1,1,#{dh}__nestlevel)=#{dh}__s1(1,0,#{dh}__nestlevel)",:assignment_stmt])
      stmts.push(["#{dh}__e1(#{dh}__globalsize(1,#{dh}__nestlevel),1,#{dh}__nestlevel)=#{dh}__e1(#{dh}__globalsize(1,#{dh}__nestlevel),0,#{dh}__nestlevel)+#{dh}__localhalosize",:assignment_stmt])
      replace_statements(stmts)
    end

  end

  class Star_Int < T
    def kind() "#{e[1]}" end
  end

  class Subroutine_Subprogram < Scoping_Unit
  end

  class Subroutine_Stmt < T
    def to_s() bb(stmt("#{sa(e[1])}#{e[2]} #{e[3]}#{e[4]}")) end
  end

  class Substring < T
    def name() e[0].name end
  end

  class Target_Stmt < T
    def to_s() stmt("#{e[1]}#{mp(e[2],""," ")}#{e[3]}") end
  end

  class Type_Declaration_Stmt < T
    def to_s() stmt("#{e[1]}#{mp(e[2],"",mn(e[1],","," "))}#{e[3]}") end
  end

  class Type_Spec < E
    def derived?() "#{e[0]}"=="type" end
    def kind() return (e[1].respond_to?(:kind))?(e[1].kind):("_default") end
    def type() (derived?)?("#{e[2]}"):("#{e[0]}") end
  end

  class Upper_Bound < T
    def ub() "#{e[0]}" end
  end

  class Use_Part < E
  end

  class Use_Stmt < T
    def modulename() "#{e[2]}" end
  end

  class Use_Stmt_1 < Use_Stmt
    def localnames() e[3].localnames end
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}") end
    def usenames() e[3].usenames end
  end

  class Use_Stmt_2 < Use_Stmt
    def localnames() e[6].localnames end
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]}") end
    def usenames() e[6].usenames end
  end

  class Where_Construct_Stmt < T
    def to_s() bb(stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}")) end
  end

  class Where_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]} #{e[6].to_s.strip}") end
  end

  class Write_Stmt < T
    def to_s() stmt("#{e[1]}#{e[2]}#{e[3]}#{e[4]}#{sb(e[5])}") end
  end

end

# paul.a.madden@noaa.gov
