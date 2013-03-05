module Fortran

  @@access="_default"
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
    array_spec.abstract_boundslist.each_index do |i|
      arrdim=i+1
      _props["lb#{arrdim}"]=array_spec.abstract_boundslist[i].alb
      _props["ub#{arrdim}"]=array_spec.abstract_boundslist[i].aub
      if @@distribute and (decompdim=@@distribute["dim"].index(arrdim))
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
    # block begin
    @@level+=1
    s
  end

  def be
    # block end
    @@level-=1 if @@level>0
  end

  def cat(f=nil)
    # concatenate elements' string representations
    send(f) if f
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

  def envfile(m,d=".")
    File.join(File.expand_path(d),"#{m}.env")
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
    # level reset
    @@level=@@levelstack.pop
  end

  def ls
    # level set
    @@levelstack.push(@@level)
  end

  def ik(e,c,a)
    # identity keep: If the [e]lement's string form equals the [c]ontrol string,
    # return the element itself; otherwise return the [a]lternate.
    (e.to_s==c)?(e):(a)
  end

  def ir(e,c,a)
    # identity replace: If the [e]lement's string form equals the [c]ontrol
    # string, return the [a]lternate; otherwise return the element itself.
    (e.to_s==c)?(a):(e)
  end

  def modenv(m)
    if d=@@incdirs.find_all { |x| File.exist?(envfile(m,x)) }[0]
      f=envfile(m,d)
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

  def post
    if e
      e.each { |x| x.post }
      e.compact!
    end
    self
  end

  def sa(e)
    # space after: If the [e]lement's string form is empty, return that; else
    # return its string form with a trailing space appended.
    (e.to_s=="")?(""):("#{e} ")
  end

  def sb(e)
    # space before: If the [e]lement's string form is empty, return that; else
    # return its string form with a prepended space.
    (e.to_s=="")?(""):(" #{e}")
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

  def sp_allocatable_stmt(array_names_and_deferred_shape_spec_lists)
    array_names_and_deferred_shape_spec_lists.names.each do |x|
      varsetprop(x,"rank","array")
      env[x].keys.each { |k| env[x][k]="_deferred" if k=~/[lu]b\d+/ }
    end
    true
  end

  def sp_block_data_stmt
    envpush
    true
  end

  def sp_dimension_stmt(array_names_and_specs)
    array_names_and_specs.e.each do |x|
      if x.is_a?(Array_Name_And_Spec)
        var=x.name
        array_spec=x.spec.e[0]
        env[var]||={}
        env[var].merge!(array_props(array_spec,{}))
      end
    end
    array_names_and_specs.names.each { |x| varsetprop(x,"rank","array") }
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
    modinfo=env.dup.delete_if { |k,v| v["access"]=="private" }
    unless modinfo.empty?
      File.open(envfile(modulename),"w") { |f| f.write(YAML.dump(modinfo)) }
    end
    envpop
    @@access="_default"
    true
  end

  def sp_module_stmt
    envpush
    true
  end

  def sp_program_stmt
    envpush
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

  def sp_target_stmt(target_object_list)
    target_object_list.objects.each do |x|
      if x.is_a?(Array_Name_And_Spec)
        var=x.name
        varsetprop(var,"rank","array")
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
    send(f) if f
    indent(("#{sa(e[0])}"+s.chomp).strip)+"\n"
  end

  def translate
    if e
      e.each { |x| x.translate }
      e.compact!
    end
    self
  end

  def use_add(modulename,usenames,localnames)
    names=localnames.zip(usenames)
    unless @@uses[modulename]
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

    def descendants(class_or_classes)
      c=(class_or_classes.is_a?(Array))?(class_or_classes):([class_or_classes])
      if self.e
        mine=self.e.find_all { |x| c.include?(x.class) }
        return self.e.reduce(mine) { |m,x| m+x.descendants(c) }
      end
      []
    end

    def to_s
      ""
    end

  end

  # Generic Subclasses

  class T < Treetop::Runtime::SyntaxNode

    attr_accessor :myenv

    def initialize(a="",b=(0..0),c=[])
      super(a,b,c)
      @myenv=env
    end

    def ancestor(class_or_classes)
      c=(class_or_classes.is_a?(Array))?(class_or_classes):([class_or_classes])
      n=self
      begin
        return n if c.any? { |x| n.is_a?(x) }
      end while n=n.parent
      nil
    end

    def declaration_constructs
      specification_part.e[2]
    end

    def declare(type,name,attrs=[])
      attrs=[attrs] unless attrs.is_a?(Array)
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

    def inside?(class_or_classes)
      (ancestor(class_or_classes))?(true):(false)
    end

    def remove
      self.parent.e[self.parent.e.index(self)]=nil
    end

    def replace_element(code,rule,node=self)
      tree=raw(code,rule,{:nl=>false})
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
      ancestor(Scoping_Unit)
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
        if code
          p=use_part
          t=raw(code,:use_stmt)
          t.parent=p
          p.e.push(t)
        end
      end
    end

    def to_s
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
    def to_s() "#{ik(e[0],"::"," ")}#{e[1]}" end
  end

  class Allocatable_Stmt < T
    def to_s() stmt("#{e[1]}#{ir(e[2],""," ")}#{e[3]}") end
  end

  class Arithmetic_If_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]} #{e[5]}#{e[6]}#{e[7]}#{e[8]}#{e[9]}") end
  end

  class Array_Name_And_Spec < E

    def name
      "#{e[0]}"
    end

    def spec
      e[2]
    end

  end

  class Array_Name_And_Spec_Pair < E
    def name() e[1].name end
  end

  class Array_Names_And_Deferred_Shape_Spec_Lists < T

    def items
      e[1].e.reduce([e[0]]) { |m,x| m.push(x.e[1]) }
    end

    def names
      e[1].e.reduce(["#{e[0].e[0]}"]) { |m,x| m.push("#{x.e[1].e[0]}") }
    end

  end

  class Array_Names_And_Specs < E
    def names() [e[0].name]+e[1].e.inject([]) { |m,x| m.push(x.name) } end
  end

  class Array_Section < E
    def name() e[0].name end
  end

  class Array_Spec < T
    def spec() e[0] end
  end

  class Assigned_Goto_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{ik(e[3],","," "+e[3].to_s)}") end
  end

  class Assumed_Shape_Spec < T

    def abstract_bounds
      OpenStruct.new({:alb=>alb,:aub=>aub})
    end

    def abstract_boundslist
      abstract_bounds
    end

    def alb
      # abstract lower bound
      (e[0].respond_to?(:alb))?(e[0].alb):("_default")
    end

    def aub
      # abstract upper bound
      "_assumed"
    end

  end

  class Assumed_Shape_Spec_List < Array_Spec

    def abstract_boundslist
      e[1].e.reduce([e[0].abstract_bounds]) { |m,x| m.push(x.abstract_bounds) }
    end

    def post
# NEED TO HANDLE DIMENSION STMT & DIMENSTION ATTR TOO !!!
      envget
      if (entity_decl=self.ancestor(Entity_Decl))
        array_name=entity_decl.name
        p1=(env[:args] and env[:args].include?(array_name))?(true):(false)
        unless p1
          code="#{self}"
          replace_element(code,:deferred_shape_spec_list)
        end
      end
    end

  end

  class Assumed_Shape_Spec_List_Pair < T

    def abstract_bounds
      e[1].abstract_bounds
    end

  end

  class Assumed_Size_Spec < Array_Spec

    def abstract_bounds
      OpenStruct.new({:alb=>alb,:aub=>aub})
    end

    def abstract_boundslist
      abstract_explicit_boundslist+[abstract_bounds]
    end

    def abstract_explicit_boundslist
      (e[0].respond_to?(:abstract_boundslist))?(e[0].abstract_boundslist):([])
    end

    def alb
      # abstract lower bound
      ("#{e[1]}".empty?)?("_default"):(e[1].alb)
    end

    def aub
      # abstract upper bound
      "_assumed"
    end

  end

  class Assumed_Size_Spec_Pair < T

    def abstract_boundslist
      e[0].abstract_boundslist
    end

  end

  class Attr_Spec_Base < E
    def dimension?() attrany(:dimension?) end
    def private?() attrany(:private?) end
    def public?() attrany(:public?) end
  end

  class Attr_Spec_Dimension < E
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
    def dimension?() e[1].dimension? end
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
    def to_s() "#{ir(e[0],""," ")}#{e[1]}#{e[2]}" end
  end

  class Common_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}") end
  end

  class Component_Def_Stmt < T
    def to_s() stmt("#{e[1]}#{ir(e[2],""," ")}#{e[3]}") end
  end

  class Computed_Goto_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}#{ir(e[5],""," ")}#{e[6]}") end
  end

  class Contains_Stmt < T
    def to_s() bb(stmt(space,:be)) end
  end

  class Data_Ref < T
    def name() (e[1].e.empty?)?(e[0].name):(e[1].e[-1].e[1].name) end
  end

  class Declaration_Constructs < T
    def to_s() e[0].e.reduce("") { |m,x| m+"#{x}" } end
  end

  class Deferred_Shape_Spec < T

    def abstract_bounds
      OpenStruct.new({:alb=>alb,:aub=>aub})
    end

    def alb
      # abstract lower bound
      "_deferred"
    end

    def aub
      # abstract upper bound
      "_deferred"
    end

  end

  class Deferred_Shape_Spec_List < Array_Spec

    def abstract_boundslist
      e[1].e.reduce([e[0].abstract_bounds]) { |m,x| m.push(x.abstract_bounds) }
    end

  end

  class Deferred_Shape_Spec_List_Pair < T

    def abstract_bounds
      e[1].abstract_bounds
    end

  end

  class Derived_Type_Stmt < T
    def to_s() bb(stmt("#{e[1]}#{sb(e[2])} #{e[3]}")) end
  end

  class Dimension_Stmt < T
    def to_s() stmt("#{e[1]}#{ir(e[2],""," ")}#{e[3]}") end
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

  class Entity_Decl < E

    def name
      "#{e[0]}"
    end

    def props
      _props={}
      if e[1].is_a?(Entity_Decl_Array_Spec)
        array_props(e[1].e[1].e[0],_props)
      end
      _props["rank"]=((array?)?("array"):("scalar"))
      {name=>_props}
    end

  end

  class Entity_Decl_1 < Entity_Decl

    def array?
      e[1].is_a?(Entity_Decl_Array_Spec)
    end

  end

  class Entity_Decl_2 < Entity_Decl
    def array?() false end # need to determine array/scalar spec of named function here?
  end

  class Entity_Decl_Array_Spec < E
  end

  class Entity_Decl_List < E
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

    def abstract_bounds
      OpenStruct.new({:alb=>alb,:aub=>aub})
    end

    def alb
      # abstract lower bound
      "_explicit"
    end

    def aub
      # abstract upperbound
      "_explicit"
    end

    def concrete_bounds
      OpenStruct.new({:clb=>clb,:cub=>cub})
    end

    def clb
      (e[0].respond_to?(:clb))?(e[0].clb):("1")
    end

    def cub
      e[1].cub
    end

  end

  class Explicit_Shape_Spec_List < Array_Spec

    def abstract_boundslist
      e[1].e.reduce([e[0].abstract_bounds]) { |m,x| m.push(x.abstract_bounds) }
    end

    def concrete_boundslist
      e[1].e.reduce([e[0].concrete_bounds]) { |m,x| m.push(x.concrete_bounds) }
    end

  end

  class Explicit_Shape_Spec_List_Pair < T

    def abstract_bounds
      e[1].abstract_bounds
    end

    def concrete_bounds
      e[1].concrete_bounds
    end

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

  class Initialization < T
    def to_s() "#{e[0]}#{e[1]}" end
  end

  class Initialization_1 < Initialization
  end

  class Initialization_2 < Initialization
  end

  class Inner_Shared_Do_Construct < T
    def to_s() cat(:be) end
  end

  class Intent_Stmt < T
    def to_s() stmt("#{e[1]}#{e[2]}#{e[3]}#{e[4]}#{ik(e[5],"::"," ")}#{e[6]}") end
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
    def to_s() "#{ir(e[0],""," ")}#{e[1]}#{e[2]}#{e[3]}#{e[4]}#{e[5]}" end
  end

  class Loop_Control_2 < Loop_Control
    def to_s() "#{ir(e[0],""," ")}#{e[1]} #{e[2]}#{e[3]}#{e[4]}" end
  end

  class Loop_Control_Pair < T
    def value() e[1] end
  end

  class Lower_Bound_Pair < T

    def alb
      # abstract lower bound
      clb
    end

    def clb
      # concrete lower bound
      "#{e[0]}"
    end

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

  class Mult_Operand < T
  end

  class Name < T

    def name
      to_s
    end

  end

  class Namelist_Group_Set_Pair < T
    def to_s() "#{ir(e[0],""," ")}#{e[1]}" end
  end

  class Namelist_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}") end
  end

  class Nonlabel_Do_Stmt < T

    def to_s
      bb(stmt("#{sa(e[1])}#{e[2]}#{e[3]}"))
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
    def to_s() stmt("#{e[1]}#{ik(e[2],"::"," ")}#{e[3]}") end
  end

  class Parenthesized_Explicit_Shape_Spec_List < T
    def abstract_boundslist() e[1].abstract_boundslist end
  end

  class Part_Ref < T
    def name() e[0].name end
  end

  class Pointer_Stmt < T
    def to_s() stmt("#{e[1]}#{ir(e[2],""," ")}#{e[3]}") end
  end

  class Power_Op < T
  end

  class Print_Stmt < T
    def to_s() stmt("#{e[1]} #{e[2]}#{e[3]}") end
  end

  class Print_Stmt_Output_Item_List < T
    def to_s() "#{e[0]}#{e[1]}" end
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
    def to_s() "#{ir(e[0],""," ")}#{e[1]}" end
  end

  class Select_Case_Stmt < T
    def to_s() bb(bb(stmt("#{sa(e[1])}#{e[2]} #{e[3]} #{e[4]}#{e[5]}#{e[6]}",:ls))) end
  end

  class Specification_Part < E
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

  class Target_Object_List < T

    def objects
      e[1].e.reduce([e[0]]) { |m,x| m.push(x.e[1]) }
    end

    def to_s
      e[1].e.reduce("#{e[0]}") { |m,x| m+"#{x}" }
    end

  end

  class Target_Object_List_Pair < T
  end

  class Target_Stmt < T
    def to_s() stmt("#{e[1]}#{ir(e[2],""," ")}#{e[3]}") end
  end

  class Type_Declaration_Stmt < T
    def to_s() stmt("#{e[1]}#{ir(e[2],"",ik(e[1],","," "))}#{e[3]}") end
  end

  class Type_Spec < E
    def derived?() "#{e[0]}"=="type" end
    def kind() return (e[1].respond_to?(:kind))?(e[1].kind):("_default") end
    def type() (derived?)?("#{e[2]}"):("#{e[0]}") end
  end

  class Upper_Bound < T

    def aub
      # abstract upper bound
      cub
    end

    def cub
      # concrete upper bound
      "#{e[0]}"
    end

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
