module Fortran

  @@access="default"
  @@current_name=nil
  @@distribute=nil
  @@dolabels=[]
  @@envstack=[{}]
  @@incdirs=[]
  @@level=0
  @@levelstack=[]
  @@uses={}

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

  def decomp_props(array_spec,_props)
    if @@distribute
      dims=0
      array_spec.boundslist.each_index do |i|
        unless _props["decomp"]
          arrdim=i+1
          _props["lb#{arrdim}"]=bounds=array_spec.boundslist[i].lb
          _props["ub#{arrdim}"]=bounds=array_spec.boundslist[i].ub
          if decdim=@@distribute["dim"].index(arrdim)
            _props["decomp"]=@@distribute["decomp"]
            _props["dim#{arrdim}"]=decdim+1
          end
        end
        dims+=1
      end
      _props["dims"]||=dims
    end
    _props
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

  def envinit
    @@envstack=[{}]
  end

  def envpop
    @@envstack.pop
    envinit if @@envstack.empty?
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

  def is_array?(node)
    vargetprop(node.function_name,"rank")=="array"
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

  def proc_access_stmt(access_spec,access_stmt_option)
    if access_spec.private?
      p="private"
    elsif access_spec.public?
      p="public"
    else
      p="default"
    end
    if access_stmt_option.is_a?(Access_Stmt_Option)
      access_stmt_option.names.each { |x| varsetprop(x,"access",p) }
    else
      @@access=p
      env.each do |n,h|
        varsetprop(n,"access",p) if vargetprop(n,"access")=="default"
      end
    end
    true
  end

  def proc_assumed_shape_spec_list(assumed_shape_spec_list)
    return false unless env['args'] and env['args'].include?(@@current_name)
    true
  end

  def proc_deferred_shape_spec_list(deferred_shape_spec_list)
    true
  end

  def proc_dimension_stmt(array_names_and_specs)
    array_names_and_specs.e.each do |x|
      if x.is_a?(Array_Name_And_Spec)
        key="#{x.e[0]}"
        array_spec=x.e[2].e[0]
        env[key]||={}
        env[key].merge!(decomp_props(array_spec,{}))
      end
    end
    array_names_and_specs.names.each { |x|varsetprop(x,"rank","array") }
    true
  end

  def proc_end_block_data_stmt
    envpop
    true
  end

  def proc_end_function_stmt
    envpop
    true
  end

  def proc_end_program_stmt
    envpop
    true
  end

  def proc_end_subroutine_stmt
    envpop
    true
  end

  def proc_module(_module)
    modulename=_module.e[0].name
    File.open(smsfile(modulename),"w") { |f| f.write(YAML.dump(env)) }
    envpop
    @@access="default"
    true
  end

  def proc_block_data_stmt
    envpush
    true
  end

  def proc_function_stmt(dummy_arg_name_list)
    envpush
    if dummy_arg_name_list.is_a?(Dummy_Arg_Name_List)
      first=dummy_arg_name_list.e[0].e[0]
      rest=dummy_arg_name_list.e[1].elements
      env['args']=["#{first}"]+rest.reduce([]) { |m,x| m.push("#{x.e[1]}") }
    end
    true
  end

  def proc_module_stmt
    envpush
    true
  end

  def proc_name(first,rest)
    @@current_name="#{first}"+rest.elements.reduce("") { |m,x| m+="#{x}" }
    true
  end

  def proc_program_stmt
    envpush
    true
  end

  def proc_sms_distribute_begin(sms_decomp_name,sms_distribute_dims)
    @@distribute={"decomp"=>"#{sms_decomp_name}","dim"=>[]}
    sms_distribute_dims.dims.each { |x| @@distribute["dim"].push(x) }
    true
  end

  def proc_sms_distribute_end
    @@distribute=nil
    true
  end

  def proc_sms_declarative(sms_declarative)
    sms_declarative.myenv=env
    true
  end

  def proc_sms_executable(sms_executable)
    sms_executable.myenv=env
    true
  end

  def proc_subroutine_stmt(dummy_arg_list_option)
    envpush
    if dummy_arg_list_option.elements
      dummy_arg_list=dummy_arg_list_option.e[1]
      first=dummy_arg_list.e[0].e[0]
      rest=dummy_arg_list.e[1].elements
      env['args']=["#{first}"]+rest.reduce([]) { |m,x| m.push("#{x.e[1]}") }
    end
    true
  end

  def proc_type_declaration_stmt(type_spec,attr_spec_option,entity_decl_list)
    varprops=entity_decl_list.varprops
    varprops.each { |v,p| p["type"]=type_spec.type }
    if array_spec=attrchk(attr_spec_option,:dimension?)
      varprops.each do |v,p|
        p["rank"]="array"
        decomp_props(array_spec,p)
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

  def proc_use_stmt(modulename,list)
    m="#{modulename}"
    if list.respond_to?(:usenames)
      use_add(m,list.usenames,list.localnames)
    else
      @@uses[m]=[:all]
    end
    modenv(m).each do |x|
      varname=x[0]
      varprop=x[1]
      localname=use_localname(m,varname)
      if uses?(m,:all) or uses?(m,localname)
        env[localname]=varprop unless varprop["access"]=="private"
      end
    end
    true
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
    end

    def declaration_constructs
      specification_part.e[2]
    end

    def declare(type,name)
      envset
      v=env[name]
      if v.nil?
        code="#{type}::#{name}"
        t=tree(code,:type_declaration_stmt)
        p=declaration_constructs
        t.parent=p
        p.e.insert(0,t) # prefer "p.e.push(t)" -- see TODO
      else
        if v["type"]!=type
          fail "#{name} is already declared with type #{v['type']}"
        end
      end
    end

    def enclosing(classes)
      nearest(classes,self.parent)
    end

    def envset
      if x=nearest([SMS])
        envswap(x.myenv) unless x.myenv.object_id==env.object_id
      else
        envinit
      end
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

    def scoping_unit
      enclosing([Scoping_Unit])
    end

    def smstype(fortran_type)
      case fortran_type
      when "integer"
        "nnt_integer"
      when "real"
        "nnt_real"
      when "doubleprecision"
        "nnt_doubleprecision"
      when "complex"
        "nnt_complex"
      when "logical"
        "nnt_logical"
      end
    end

    def specification_part
      scoping_unit.e[1]
    end

    def sub(node,tree)
      tree.parent=node.parent
      block=node.parent.e
      block[block.index(node)]=tree
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
          code=((list.empty?)?(nil):("#{code},only:#{list.join(',')}"))
        end
        unless code.nil?
          p=use_part
# HACK start
          t=tree("!sms$ignore begin",:sms_ignore_begin)
          t.parent=p
          p.e.push(t)
# HACK end
          t=tree(code,:use_stmt)
          t.parent=p
          p.e.push(t)
# HACK start
          t=tree("!sms$ignore end",:sms_ignore_end)
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
    def to_s() "#{mn(e[0],'::',' ')}#{e[1]}" end
  end

  class Allocatable_Stmt < T
    def to_s() stmt("#{e[1]}#{mp(e[2],'',' ')}#{e[3]}") end
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

  class Assigned_Goto_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{mn(e[3],',',' '+e[3].to_s)}") end
  end

  class Assumed_Shape_Spec < T
    def bounds() OpenStruct.new({:lb=>lb,:ub=>ub}) end
    def boundslist() ex+[bounds] end
    def ex() (e[0].respond_to?(:boundslist))?(e[0].boundslist):([]) end
    def lb() (e[0].respond_to?(:lb))?(e[0].lb):("_default_") end
    def ub() "_assumed_" end
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
    def lb() ("#{e[1]}".empty?)?("_default_"):(e[1].lb) end
    def ub() "_assumed_" end
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
    def to_s() "#{mp(e[0],'',' ')}#{e[1]}#{e[2]}" end
  end

  class Common_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}") end
  end

  class Component_Def_Stmt < T
    def to_s() stmt("#{e[1]}#{mp(e[2],'',' ')}#{e[3]}") end
  end

  class Computed_Goto_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}#{mp(e[5],'',' ')}#{e[6]}") end
  end

  class Contains_Stmt < T
    def to_s() bb(stmt(space,:be)) end
  end

  class Deferred_Shape_Spec < T
    def bounds() OpenStruct.new({:lb=>lb,:ub=>ub}) end
    def lb() "_deferred_" end
    def ub() "_deferred_" end
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
    def to_s() stmt("#{e[1]}#{mp(e[2],'',' ')}#{e[3]}") end
  end

  class Do_Term_Action_Stmt < T
    def to_s() stmt(space,:be) end
  end

  class Do_Term_Shared_Stmt < T
    def to_s() cat(:be) end
  end

  class Double_Colon < T
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
        decomp_props(e[1].e[1].e[0],_props)
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

  class Explicit_Shape_Spec < T
    def bounds() OpenStruct.new({:lb=>lb,:ub=>ub}) end
    def lb() ("#{e[0]}".empty?)?("_default_"):(e[0].lb) end
    def ub() e[1].ub end
  end

  class Explicit_Shape_Spec_List < T
    def boundslist() e[1].e.reduce([e[0].bounds]) { |m,x| m.push(x.bounds) } end
  end

  class Explicit_Shape_Spec_List_Pair < T
    def bounds() e[1].bounds end
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
    def to_s() stmt("#{e[1]}#{e[2]}#{e[3]}#{e[4]}#{mn(e[5],'::',' ')}#{e[6]}") end
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

  class Label_Do_Stmt < T
    def to_s() bb(stmt("#{sa(e[1])}#{e[2]} #{e[3]}#{e[4]}")) end
  end

  class Loop_Control < T
  end

  class Loop_Control_1 < Loop_Control
    def to_s() "#{mp(e[0],'',' ')}#{e[1]}#{e[2]}#{e[3]}#{e[4]}#{e[5]}" end
  end

  class Loop_Control_2 < Loop_Control
    def to_s() "#{mp(e[0],'',' ')}#{e[1]} #{e[2]}#{e[3]}#{e[4]}" end
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
    def to_s() "#{e[0]}#{e[1].e.reduce('') { |m,x| m+="#{x}" } }" end
  end

  class Name < T
    def name() to_s end
#   def translate()
#     if inside?([Entity_Decl])
#       envset
#       if me=env[self.to_s]
#         if me['decomp']
#           puts "### var #{to_s} env #{me}"
#         end
#       end
#     end
#   end
  end

  class Namelist_Group_Set_Pair < T
    def to_s() "#{mp(e[0],'',' ')}#{e[1]}" end
  end

  class Namelist_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}") end
  end

  class Nonlabel_Do_Stmt < T
    def to_s() bb(stmt("#{sa(e[1])}#{e[2]}#{e[3]}")) end
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
    def to_s() stmt("#{e[1]}#{mn(e[2],'::',' ')}#{e[3]}") end
  end

  class Parenthesized_Explicit_Shape_Spec_List < T
    def boundslist() e[1].boundslist end
  end

  class Pointer_Stmt < T
    def to_s() stmt("#{e[1]}#{mp(e[2],'',' ')}#{e[3]}") end
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
    def to_s() "#{mp(e[0],'',' ')}#{e[1]}" end
  end

  class Select_Case_Stmt < T
    def to_s() bb(bb(stmt("#{sa(e[1])}#{e[2]} #{e[3]} #{e[4]}#{e[5]}#{e[6]}",:ls))) end
  end

  class Specification_Part < E
  end

  ## SMS ##

  class SMS < T
  end

  class SMS_Barrier < SMS
    def translate
      use("nnt_types_module")
      sub(parent,raw("call ppp_barrier(ppp__status)",:call_stmt))
    end
  end

  class SMS_Compare_Var < SMS
    def to_s() sms("#{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]}") end
    def translate()
      use("module_decomp")
      use("nnt_types_module")
      envset
      var="#{e[3]}"
      varenv=env[var]
#     return unless varenv # HACK until modules are being loaded into env
      if decomp=varenv['decomp']
        dims=varenv["dims"]
        msg="#{e[5]}"
        type=smstype(varenv['type'])
        gllbs="(/"+(1..7).map { |i| (i>dims)?(1):((varenv["lb#{i}"]=="_default_")?(1):(varenv["lb#{i}"])) }.join(",")+"/)"
        glubs="(/"+(1..7).map { |i| (i>dims)?(1):(varenv["ub#{i}"]) }.join(",")+"/)"
        perms="(/"+(1..7).map { |i| varenv["dim#{i}"]||0 }.join(",")+"/)"
        code="if (sms_debugging_on()) call ppp_compare_var(#{decomp}(dh__nestlevel),#{var},#{type},#{glubs},#{perms},#{gllbs},#{glubs},#{gllbs},#{dims},'#{var}','#{msg}',ppp__status)"
#       puts "### #{code}"
      end
#     sub(parent,raw(code,:if_stmt))
#     p raw(code,:if_stmt)
    end
  end

  class SMS_Create_Decomp < SMS
    def to_s() sms(e[2].e.map { |x| x.text_value }.join) end
  end

  class SMS_Distribute < SMS
    def to_s() "#{e[0]}#{e[1].e.reduce('') { |m,x| m+=x.to_s }}#{e[2]}" end
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
    def to_s() sms(e[2].e.map { |x| x.text_value }.join) end
  end

  class SMS_Halo_Comp_Begin < SMS
    def to_s() sms("#{e[2]} #{e[3]}") end
  end

  class SMS_Halo_Comp_End < SMS
    def to_s() sms("#{e[2]}") end
  end

  class SMS_Ignore_Begin < SMS
    def to_s() sms("#{e[2]}") end
  end

  class SMS_Ignore_End < SMS
    def to_s() sms("#{e[2]}") end
  end

  class SMS_Parallel_Begin < SMS
    def to_s() sms("#{e[2]} #{e[3]}") end
  end

  class SMS_Parallel_End < SMS
    def to_s() sms("#{e[2]}") end
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
    def to_s() sms("#{e[2]}") end
  end

  class SMS_To_Local_Begin < SMS
    def to_s() sms("#{e[2]} #{e[3]}") end
  end

  class SMS_To_Local_End < SMS
    def to_s() sms("#{e[2]}") end
  end

  class SMS_Unstructured_Grid < SMS
    def to_s() sms("#{e[2]}") end
  end

  ## SMS ##

  class Subroutine_Subprogram < Scoping_Unit
  end

  class Subroutine_Stmt < T
    def to_s() bb(stmt("#{sa(e[1])}#{e[2]} #{e[3]}#{e[4]}")) end
  end

  class Target_Stmt < T
    def to_s() stmt("#{e[1]}#{mp(e[2],'',' ')}#{e[3]}") end
  end

  class Type_Declaration_Stmt < T
    def to_s() stmt("#{e[1]}#{mp(e[2],'',mn(e[1],',',' '))}#{e[3]}") end
  end

  class Type_Spec < E
    def derived?() "#{e[0]}"=="type" end
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
