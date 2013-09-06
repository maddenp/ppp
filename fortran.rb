$: << (basedir=File.dirname($0)) << File.join(basedir,"lib")

require "ostruct"
require "yaml"

require "treetop/runtime"
require "common"

module Fortran

  include Common

  def env
    @envstack.last
  end

  def envfile(m,d=nil)
    d=(defined?(@srcfile))?(File.dirname(@srcfile)):(".") if d.nil?
    File.join(File.expand_path(d),"#{m}.env")
  end

  def envpop
    oldenv=@envstack.pop
    @envstack.push({}) if @envstack.empty?
    env[:static]=oldenv[:static]
    oldenv
  end

  def envpush
    static=env.delete(:static)
    @envstack.push(deepcopy(env))
    env[:static]=static
  end

  def modenv(m)
    if d=@incdirs.find_all { |x| File.exist?(envfile(m,x)) }[0]
      f=envfile(m,d)
      begin
        return YAML.load(File.open(f))
      rescue Exception=>ex
        s="#{ex.message}\n"
        s+=ex.backtrace.reduce(s) { |m,x| m+="#{x}\n" }
        s+="Error reading #{f}"
        fail s
      end
    end
    {}
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
      @access=p
      env.each do |n,h|
        varsetprop(n,"access",p) if vargetprop(n,"access")=="_default"
      end
    end
    true
  end

  def sp_allocatable_stmt(array_names_and_deferred_shape_spec_lists)
    a=array_names_and_deferred_shape_spec_lists
    env[:allocatable]||=[]
    a.names.each do |var|
      # Record that this var has been marked allocatable, which means that it
      # must be an array with deferred bounds.
      env[:allocatable].push(var)
      varsetprop(var,"sort","_array")
      # Correct for the case where this array has already been seen and its
      # bounds incorrectly marked as assumed.
      env[var].keys.each { |k| env[var][k]="_deferred" if k=~/[lu]b\d+/ }
    end
    # In case this array has not previously been seen, record its array specs.
    a.items.each do |x|
      if x.array_spec
        env[x.name].merge!(array_props(x.array_spec,{},@distribute))
      end
    end
    true
  end

  def sp_block_data
    envpop
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
        env[var].merge!(array_props(array_spec,{},@distribute))
      end
    end
    array_names_and_specs.names.each { |x| varsetprop(x,"sort","_array") }
    true
  end

  def sp_dolabel_pop
    # F90:R817 block-do-construct's do-stmt always pushes a label: either the
    # actual label (when do-stmt is a label-do-stmt), or the symbol :nolabel
    # (when do-stmt is a nonlabel-do-stmt). F90:R826 nonblock-do-construct
    # requires label-do-stmt components, each of whose labels will be pushed
    # onto the label stack. But these labels may be repeated, to match a single
    # do-term-shared-stmt.
    @dolabels.pop
    true
  end

  def sp_dolabel_push(label)
    # A non-label-do-stmt pushes the symbol :nolabel onto the stack, which must
    # not be converted to a string, to avoid matching a potential literal label
    # 'nolabel'. If the passed-in label is not a symbol, push its string version
    # onto the label stack.
    @dolabels.push((label.is_a?(Symbol))?(label):("#{label}"))
    true
  end

  def sp_dolabel_repeat?
    # Report whether the top two labels on the label stack match. Some grammar
    # rules only match when this is not the case; others require it. But if the
    # topmost label is the symbol :nolabel, never consider that a repeat, as
    # nonlabel versions of block-do-construct may have arbitrarily deep nesting.
    return false if @dolabels.last==:nolabel
    "#{@dolabels[-1]}"=="#{@dolabels[-2]}"
  end

  def sp_function_stmt(dummy_arg_name_list)
    envpush
    if dummy_arg_name_list.is_a?(Dummy_Arg_Name_List)
      first="#{dummy_arg_name_list.e[0]}"
      rest=dummy_arg_name_list.e[1].e
      env[:args]=rest.reduce([first]) { |m,x| m.push("#{x.e[1]}") }
    end
    true
  end

  def sp_function_subprogram(function_subprogram)
    var="#{function_subprogram.function_name}"
    env[var]||={}
    env[var]["access"]="_default" unless env[var]["access"]
    env[var]["sort"]="_scalar"
    if (type_spec=function_subprogram.function_stmt_type_spec)
      env[var]["kind"]="#{type_spec.kind}"
      env[var]["type"]="#{type_spec.type}"
    end
    envpop
    true
  end

  def sp_hollerith_check_count
    @hollerith_count+=1
    (@hollerith_count>@hollerith_size)?(false):(true)
  end

  def sp_hollerith_reset_count
    @hollerith_count=0
    true
  end

  def sp_hollerith_set_length(digit_string)
    @hollerith_size=Integer(digit_string.to_s)
    true
  end

  def sp_is_array?(node)
    return false unless node.respond_to?(:name)
    vargetprop(node.name,"sort")=="_array"
  end

  def sp_label(label)
    n=label[0].e.reduce("") { |m,x| m+"#{x}" }.to_i
    (env[:static].labels||=Set.new).add(n)
    true
  end

  def sp_main_program
    envpop
    true
  end

  def sp_module(_module)
    modulename=_module.e[0].name
    # Do not export symbol keys, which are for internal purposes only
    modinfo=deepcopy(env).delete_if { |k,v| k.is_a?(Symbol) }
    # Do not export info on private objects
    modinfo.delete_if { |k,v| v["access"]=="private" }
    unless modinfo.empty?
      File.open(envfile(modulename),"w") { |f| f.write(YAML.dump(modinfo)) }
    end
    envpop
    @access="_default"
    true
  end

  def sp_namelist_stmt(namelist_stmt)
    namelist_stmt.sets.each do |x|
      name=x[0]
      objects=x[1]
      env[name]||={}
      env[name]["sort"]="_namelist"
      env[name]["objects"]=objects
      env[name]["access"]||=@access
    end
    true
  end

  def sp_nonblock_do_end?(node)
    # F90:R826 requires that the label on the terminating action-stmt match that
    # of the matching label-do-stmt. If this isn't the case, this node cannot be
    # the end of a nonblock-do-construct.
    return false unless node.respond_to?(:label)
    return false if node.label.to_s.empty?
    ("#{node.label}"==@dolabels.last)?(true):(false)
  end

  def sp_module_stmt
    envpush
    true
  end

  def sp_parameter_stmt(named_constant_def_list)
    named_constant_def_list.names.each do |x|
      varsetprop(x,"parameter","_true")
    end
    true
  end

  def sp_subroutine_stmt(dummy_arg_list_option)
    envpush
    if dummy_arg_list_option.e
      if dummy_arg_list_option.e[1].is_a?(Dummy_Arg_List)
        dummy_arg_list=dummy_arg_list_option.e[1]
        first="#{dummy_arg_list.e[0]}"
        rest=dummy_arg_list.e[1].e
        env[:args]=rest.reduce([first]) { |m,x| m.push("#{x.e[1]}") }
      end
    end
    true
  end

  def sp_subroutine_subprogram
    envpop
    true
  end

  def sp_target_stmt(target_object_list)
    target_object_list.objects.each do |x|
      if x.is_a?(Array_Name_And_Spec)
        var=x.name
        varsetprop(var,"sort","_array")
      end
    end
    true
  end

  def sp_type_declaration_stmt(type_spec,attr_spec_option,entity_decl_list)
    varprops=entity_decl_list.varprops(@distribute)
    if x=attrchk(attr_spec_option,:dimension?)
      array_spec=x.e[0]
      varprops.each do |v,p|
        p["sort"]="_array"
        array_props(array_spec,p,@distribute)
      end
    end
    if attrchk(attr_spec_option,:parameter?)
      varprops.each { |v,p| p["parameter"]="_true" }
    end
    if attrchk(attr_spec_option,:private?)
      varprops.each { |v,p| p["access"]="private" }
    elsif attrchk(attr_spec_option,:public?)
      varprops.each { |v,p| p["access"]="public" }
    else
      varprops.each { |v,p| p["access"]=@access }
    end
    varprops.each do |v,p|
      varenv=(env[v]||={})
      ["access","sort"].each { |x| p.delete(x) if varenv.include?(x) }
      p["type"]=type_spec.type
      p["kind"]=type_spec.kind
      if env[:allocatable] and env[:allocatable].include?(v)
        p.keys.each { |k| p[k]="_deferred" if k=~/[lu]b\d+/ }
      end
      varenv.merge!(p)
    end
    true
  end

  def sp_use_stmt(modulename,list)
    def use_add(modulename,usenames,localnames)
      env[:uses]||={}
      names=localnames.zip(usenames)
      unless env[:uses][modulename]
        env[:uses][modulename]=names
      else
        unless uses?(modulename,:all)
          names.each do |x|
            env[:uses][modulename].push(x) unless uses?(modulename,x)
          end
        end
      end
    end
    m="#{modulename}"
    if list.respond_to?(:usenames)
      use_add(m,list.usenames,list.localnames)
    else
      env[:uses]||={}
      env[:uses][m]=[[:all]]
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

  def use_localname(modulename,usename)
    if i=use_usenames(modulename).index(usename)
      return use_localnames(modulename)[i]
    end
    return usename
  end

  def use_usenames(modulename)
    e=(self.is_a?(T))?(use_part.env):(env)
    return [] unless e[:uses]
    e[:uses][modulename].map { |x| x[1] }
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

    def cat
      # concatenate elements' string representations
      (e)?(self.e.map { |x| "#{x}" }.join):("")
    end

    def post_common
      post_children
      post if respond_to?(:post)
    end

    def post_children
      if e
        e.each { |x| x.post_common }
        e.compact!
      end
      self
    end

    def post_top
      post_common
      self
    end

    def to_s
      ""
    end

    def translate_common
      translate_children
      translate if respond_to?(:translate)
    end

    def translate_children
      if e
        e.each { |x| x.translate_common if x }
        e.compact!
      end
      self
    end

    def translate_top
      translate_common
      self
    end

    alias e elements

  end

  # Generic subclasses

  class T < Treetop::Runtime::SyntaxNode

    include Common

    attr_accessor :envref,:srcfile

    def initialize(*args)
      super(*args)
      @envref=input.envstack.last
      @srcfile=nil
    end

    def ancestor(*classes)
      n=self.parent
      begin
        return n if classes.any? { |x| n.is_a?(x) }
      end while n=n.parent
      nil
    end

    def attrany(attr,e=nil)
      (e||self.e).reduce(false) { |m,x| m||=attrchk(x,attr) }
    end

    def declaration_constructs
      specification_part.e[2]
    end

    def declare(type,name,props={})
      # Override in *_fortran.rb, restrictions on allowable definitions may
      # differ between applications. It may be acceptable to simply ignore
      # a request to define an already-defined variable, in which case a single
      # method suitable for all applications could be defined here.
    end

    def env
      self.envref
    end

    def execution_part
      scoping_unit.e[2]
    end

    def getvarenv(name,node=self,expected=true)
      unless (varenv=node.env["#{name}"])
        fail "'#{name}' not found in environment" if expected
      end
      varenv
    end

    def ik(e,c,a)
      # identity keep: If the [e]lement's string form equals the [c]ontrol
      # string, return the element itself; otherwise return the [a]lternate.
      (e.to_s==c)?(e):(a)
    end

    def ir(e,c,a)
      # identity replace: If the [e]lement's string form equals the [c]ontrol
      # string, return the [a]lternate; otherwise return the element itself.
      (e.to_s==c)?(a):(e)
    end

    def indent
      r=root
      l=root.level
      r.instance_variable_set(:@level,l+1)
    end

    def insert_statement(code,rule,node,offset)
      tree=self.raw(code,rule,@srcfile,{:env=>self.env})
      tree.parent=node.parent
      block=node.parent.e
      block.insert(block.index(node)+offset,tree)
      tree
    end

    def insert_statement_after(code,rule,predecessor)
      insert_statement(code,rule,predecessor,1)
    end

    def insert_statement_before(code,rule,successor)
      insert_statement(code,rule,successor,0)
    end

    def inside?(*class_or_classes)
      (ancestor(*class_or_classes))?(true):(false)
    end

    def label_create
      labels=(self.env[:static].labels||=Set.new)
      99999.downto(1).each do |n|
        unless labels.include?(n)
          labels.add(n)
          return n
        end
      end
      fail "No unused labels available"
    end

    def label_delete
      return nil if (label=self.label).empty?
      self.e[0]=Treetop::Runtime::SyntaxNode.new("",nil)
      label
    end

    def level
      r=root
      unless r.instance_variable_defined?(:@level)
        r.instance_variable_set(:@level,0)
      end
      r.instance_variable_get(:@level)
    end

    def list_to_s
      s="#{e[0]}"
      s=e[1].e.reduce(s) { |m,x| m+"#{x.e[0]}#{x.e[1]}" } if e[1].e
      s
    end

    def newtag
      r=root
      t=0
      t=r.instance_variable_get(:@tag)+1 if r.instance_variable_defined?(:@tag)
      r.instance_variable_set(:@tag,t)
    end

    def raw(code,rule,srcfile,opts={})
      Translator.new.raw(code,rule,srcfile,opts)
    end

    def remove
      self.parent.e[self.parent.e.index(self)]=nil
    end

    def replace_element(code,rule,node=self)
      tree=self.raw(code,rule,@srcfile,{"nl"=>false,:env=>node.env})
      node=node.parent while "#{node}"=="#{node.parent}"
      tree.parent=node.parent
      block=node.parent.e
      block[block.index(node)]=tree
    end

    def replace_statement(code,rule,node=self)
      tree=self.raw(code,rule,@srcfile,{:env=>node.env})
      tree.parent=node.parent
      block=node.parent.e
      block[block.index(node)]=tree
    end

    def replace_statements(stmt_pairs,node=self)
      p=stmt_pairs.shift
      s=replace_statement(p[0],p[1],node)
      stmt_pairs.each { |p| s=insert_statement_after(p[0],p[1],s) }
    end

    def root
      n=self
      n=n.parent while n.parent
      n
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

    def scoping_unit
      ancestor(Scoping_Unit)
    end

    def space(all=false)
      a=(all)?(self.e):(self.e[1..-1])
      a.map { |x| "#{x}" }.join(" ").strip
    end

    def specification_part
      scoping_unit.e[1]
    end

    def stmt(s)
      l=level||0
      (" "*2*l)+(("#{sa(e[0])}"+s.chomp).strip)+"\n"
    end

    def unindent
      r=root
      l=root.level
      r.instance_variable_set(:@level,l-1) if l>0
    end

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
          code=((list.empty?)?(nil):("#{code},only:#{list.join(",")}"))
        end
        if code
          up=use_part
          new_usenames=[[:all]] if new_usenames.empty?
          new_uses={modname=>new_usenames}
          old_uses=up.env[:uses]
          up.env[:uses]=(old_uses)?(old_uses.merge(new_uses)):(new_uses)
          t=self.raw(code,:use_stmt,@srcfile,{:env=>self.env})
          t.parent=up
          up.e.push(t)
        end
      end
    end

    def to_s
      text_value
    end

  end

  class E < T
    
		def to_s
			cat
		end

  end

  class J < T
    
		def to_s
			space(true)
		end

  end

  class Scoping_Unit < E
  end

  class StmtC < T
    
		def to_s
			stmt(e[1..-1].map { |x| "#{x}" }.join)
		end

  end

  class StmtJ < T
    
		def to_s
			stmt(space)
		end

  end

  # Out-of-order class definitions (must be defined before subclassed)

  class IO_Spec_List < T

    def end
      list_item(IO_Spec_End)
    end

    def eor
      list_item(IO_Spec_Eor)
    end

    def err
      list_item(IO_Spec_Err)
    end

    def iostat
      list_item(IO_Spec_Iostat)
    end

    def list_item(spec)
      return e[0] if e[0].is_a?(spec)
      e[1].e.each { |x| return x.e[1] if x.e[1].is_a?(spec) } if e[1]
      nil
    end

    def size
      list_item(IO_Spec_Size)
    end

    def to_s
      list_to_s
    end

    def unit
      if (io_spec_unit=list_item(IO_Spec_Unit))
        io_spec_unit.rhs
      else
        # If 'unit=' does not appear, the unit *must* be the first list item
        e[0].e[0]
      end
    end

  end

  class IO_Stmt < T

    def end
      list.end
    end

    def eor
      list.eor
    end

    def err
      list.err
    end

    def iostat
      list.iostat
    end

    def output_items
      (e[5].is_a?(Output_Item_List))?(e[5].output_items):([])
    end

    def list
      e[3]
    end

    def replace_item(old,new)
      output_items.each do |x|
        replace_element(new,:expr,old) if "#{x}"=="#{old}"
      end
    end

    def size
      list.size
    end

    def unit
      list.unit
    end

  end

  # Grammar-supporting subclasses

  class AC_Implied_Do_Control < T

    def to_s
      s="#{e[0]}#{e[1]}#{e[2]}"
      s+="#{e[3].e[0]}#{e[3].e[1]}" if e[3].e
      s
    end

  end

  class AC_Value_List < T
    
		def to_s
			list_to_s
		end

  end

  class Access_Id_List < T

    def names
      [e[0].name]+e[1].e.reduce([]) { |m,x| m.push(x.name) }
		end

  end

  class Access_Id_List_Pair < T

    def name
      "#{e[1]}"
		end

  end

  class Access_Spec < T

    def private?
      "#{text_value}"=="private"
    end

    def public?
      "#{text_value}"=="public"
    end

  end

  class Access_Stmt < StmtC
  end

  class Access_Stmt_Option < T

    def names
      e[1].names
    end

    def to_s
      "#{ik(e[0],"::"," ")}#{e[1]}"
    end

  end

  class Actual_Arg_Spec_List < T
    
		def to_s
			list_to_s
		end

  end

  class Add_Operand < E

    def to_s
      "#{e[0]}"+((e[1].e)?("#{e[1].e[0]}#{e[1].e[1]}"):(""))
    end

  end

  class Allocatable_Stmt < T
    
		def to_s
			stmt("#{e[1]}#{ir(e[2],""," ")}#{e[3]}")
		end

  end

  class Allocate_Object < E

    def item
      e[1]
    end

    def name
      e[1].name
    end

  end

  class Allocate_Object_List < T

    def items
      e[1].e.reduce([e[0].item]) { |m,x| m.push(x.e[1].item) }
    end

    def names
      e[1].e.reduce([e[0].name]) { |m,x| m.push(x.e[1].name) }
    end

    def to_s
      list_to_s
    end

  end

  class Allocate_Object_List_Pair < T

    def item
      e[1].item
    end

    def name
      e[1].name
    end

  end

  class Allocate_Shape_Spec_List < T
    
		def to_s
			list_to_s
		end

  end

  class Allocate_Stmt < StmtC

    def items
      e[3].items
    end

    def names
      e[3].names
    end

  end

  class Allocation < E

    def name
      e[0].name
    end

  end

  class Allocation_List < T

    def items
      e[1].e.reduce([e[0]]) { |m,x| m.push(x.e[1]) }
    end

    def names
      e[1].e.reduce([e[0].name]) { |m,x| m.push(x.e[1].name) }
    end

    def to_s
      list_to_s
    end

  end

  class Arithmetic_If_Stmt < T
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]} #{e[5]}#{e[6]}#{e[7]}#{e[8]}#{e[9]}")
		end

  end

  class Array_Name < E
  end

  class Array_Name_And_Deferred_Shape_Spec_List < E

    def array_spec
      (e[1].respond_to?(:array_spec))?(e[1].array_spec):(nil)
    end

    def name
      "#{e[0]}"
    end

  end

  class Array_Name_And_Deferred_Shape_Spec_List_Pair < E
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

    def name
      e[1].name
		end

  end

  class Array_Names_And_Deferred_Shape_Spec_Lists < T

    def items
      e[1].e.reduce([e[0]]) { |m,x| m.push(x.e[1]) }
    end

    def names
      e[1].e.reduce(["#{e[0].e[0]}"]) { |m,x| m.push("#{x.e[1].e[0]}") }
    end

  end

  class Array_Names_And_Specs < T

    def items
      e[1].e.reduce([e[0]]) { |m,x| m.push(x) }
    end

    def names
      e[1].e.reduce([e[0].name]) { |m,x| m.push(x.name) }
    end

    def to_s
      list_to_s
    end

  end

  class Array_Section < E

    def name
      e[0].name
		end

  end

  class Array_Spec < T

    def spec
      e[0]
		end

  end

  class Assigned_Goto_Stmt < T
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{ik(e[3],","," "+e[3].to_s)}")
		end

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
      ("#{e[0]}".empty?)?("_assumed"):("_offset")
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
      ok=true
      if (entity_decl=self.ancestor(Entity_Decl))
        array_name=entity_decl.name
        ok=(self.env[:args] and self.env[:args].include?(array_name))?(true):(false)
      elsif (entity_decl=self.ancestor(Array_Name_And_Spec))
        array_name=entity_decl.name
        ok=(self.env["#{array_name}"]["lb1"]=="_deferred")?(false):(true)
      end
      unless ok
        code="#{self}"
        replace_element(code,:deferred_shape_spec_list)
        varenv=getvarenv(array_name)
        varenv.keys.each { |k| varenv[k]="_deferred" if k=~/[lu]b\d+/ }
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

    def dimension?
      attrany(:dimension?)
    end

    def parameter?
      attrany(:parameter?)
    end

    def private?
      attrany(:private?)
    end

    def public?
      attrany(:public?)
    end

  end

  class Attr_Spec_Dimension < E

    def dimension?
      e[2]
		end

  end

  class Attr_Spec_List < Attr_Spec_Base
  end

  class Attr_Spec_List_Pair < Attr_Spec_Base
  end

  class Attr_Spec_List_Pairs < Attr_Spec_Base

    def dimension?
      (e[0])?(attrany(:dimension?,e[0].e)):(false)
    end

    def parameter?
      (e[0])?(attrany(:parameter?,e[0].e)):(false)
    end

    def private?
      (e[0])?(attrany(:private?,e[0].e)):(false)
    end

    def public?
      (e[0])?(attrany(:public?,e[0].e)):(false)
    end

  end

  class Attr_Spec_Option < Attr_Spec_Base

    def dimension?
      e[1].dimension?
    end

    def parameter?
      e[1].parameter?
    end

  end

  class Attr_Spec_Parameter < E

    def parameter?
      true
		end

  end

  class Backspace_Stmt_1 < IO_Stmt
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}")
		end

  end

  class Backspace_Stmt_2 < IO_Stmt
    
		def to_s
			stmt("#{e[1]} #{e[2]}")
		end

  end

  class Block_Data < Scoping_Unit
  end

  class Block_Data_Name < E
  end

  class Block_Data_Stmt < T
    def to_s
      s=stmt(space)
      indent
      s
    end
  end

  class Block_Do_Construct < E
  end

  class Call_Stmt < T
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}")
		end

  end

  class Case_Construct_Name < E
  end

  class Case_Stmt < T
    def to_s
      unindent
      s=stmt(space)
      indent
      s
    end
  end

  class Case_Value_Range_List < T
    
		def to_s
			list_to_s
		end

  end

  class Backspace_Stmt < IO_Stmt
  end

  class Close_Spec_List < IO_Spec_List
  end

  class Close_Spec_List_Pair < E
  end

  class Close_Stmt < IO_Stmt
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}")
		end

  end

  class Common_Block_Name_And_Object_List < T
    
		def to_s
			"#{ir(e[0],""," ")}#{e[1]}#{e[2]}"
		end

  end

  class Common_Block_Name < E
  end

  class Common_Stmt < T
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}")
		end

  end

  class Component_Attr_Spec_List < T
    
		def to_s
			list_to_s
		end

  end

  class Component_Decl_List < T
    
		def to_s
			list_to_s
		end

  end

  class Component_Def_Stmt < T
    
		def to_s
			stmt("#{e[1]}#{ir(e[2],""," ")}#{e[3]}")
		end

  end

  class Component_Name < E
  end

  class Computed_Goto_Stmt < T
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}#{ir(e[5],""," ")}#{e[6]}")
		end

  end

  class Connect_Spec_List < IO_Spec_List
  end

  class Connect_Spec_List_Pair < E
  end

  class Contains_Stmt < T

    def to_s
      unindent
      s="\n#{stmt(space)}"
      indent
      s
    end

  end

  class Data_I_Do_Object_List < T
    
		def to_s
			list_to_s
		end

  end

  class Data_Ref < E

    def name
      (e[1].e.empty?)?(e[0].name):(e[1].e[-1].e[1].name)
    end

    def rightmost
      if e[1].e.empty?
        e[0]
      else
        e[1].e[-1].e[1]
      end
    end

    def to_s
      e[1].e.reduce("#{e[0]}") { |m,x| m+"#{x.e[0]}#{x.e[1]}" }
    end

  end

  class Data_Stmt < T
    
		def to_s
			stmt("#{e[1]} #{e[2]}")
		end

  end

  class Data_Stmt_Object_List < T
    
		def to_s
			list_to_s
		end

  end

  class Data_Stmt_Value_List < T
    
		def to_s
			list_to_s
		end

  end

  class Deallocate_Stmt < StmtC
  end

  class Declaration_Constructs < T

    def to_s
      e.reduce("") { |m,x| m+"#{x}" }
    end

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
    def to_s
      s=stmt("#{e[1]}#{sb(e[2])} #{e[3]}")
      indent
      s
    end
  end

  class Dimension_Stmt < T
    
		def to_s
			stmt("#{e[1]}#{ir(e[2],""," ")}#{e[3]}")
		end

  end

  class Do_Body < E
  end

  class Do_Construct_Name < E
  end

  class Do_Term_Action_Stmt < T

    def to_s
      unindent
      stmt(space)
    end

  end

  class Do_Term_Shared_Stmt < T

    def label
      "#{e[1].e[0]}"
    end

    def to_s
      unindent
      n=self
      while (n=n.ancestor(Outer_Shared_Do_Construct))
        unindent if n.label==self.label
      end
      cat
    end

  end

  class Double_Colon < T
  end

  class Dummy_Arg_List < T
  end

  class Dummy_Arg_Name < E
  end

  class Dummy_Arg_Name_List < T

    def to_s
      e[1].e.reduce("#{e[0]}") { |m,x| m+"#{x.e[0]}#{x.e[1]}" }
    end

  end

  class Else_If_Stmt < T

    def to_s
      unindent
      s="\n"+stmt("#{e[2]} #{e[3]}#{e[4]}#{e[5]} #{e[6]}")
      indent
      s
    end

  end

  class Else_Stmt < T

    def to_s
      unindent
      s="#{stmt(space)}"
      indent
      s
    end

  end

  class Elsewhere_Stmt < T

    def to_s
      unindent
      s=stmt(space)
      indent
      s
    end

  end

  class End_Block_Data_Option < T
    
		def to_s
			space(true)
		end

  end

  class End_Block_Data_Stmt < T

    def to_s
      unindent
      stmt(space)
    end

  end

  class End_Do_Stmt < T

    def to_s
      unindent
      "#{stmt(space)}"
    end

  end

  class End_Function_Stmt < T

    def to_s
      unindent
      stmt(space)+"\n"
    end

  end

  class End_If_Stmt < T

    def to_s
      unindent
      "#{stmt(space)}"
    end

  end

  class End_Interface_Stmt < T

    def to_s
      unindent
      stmt("#{e[1]} #{e[2]}#{sb(e[3])}")+"\n"
    end

  end

  class End_Module_Option < T
    
		def to_s
			space(true)
		end

  end

  class End_Module_Stmt < T

    def to_s
      unindent
      "#{stmt(space)}\n"
    end

  end

  class End_Program_Stmt < T

    def to_s
      unindent
      "\n"+stmt("#{e[1]}#{sb(e[3])}#{sb(e[4])}")
    end

  end

  class End_Select_Stmt < T

    def to_s
      unindent
      unindent
      stmt(space)
    end

  end

  class End_Subroutine_Stmt < T

    def to_s
      unindent
      "\n#{stmt(space)}\n"
    end

  end

  class End_Type_Stmt < T

    def to_s
      unindent
      stmt(space)
    end

  end

  class End_Where_Stmt < T

    def to_s
      unindent
      stmt(space)
    end

  end

  class Endfile_Stmt_1 < IO_Stmt
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}")
		end

  end

  class Endfile_Stmt_2 < IO_Stmt
    
		def to_s
			stmt("#{e[1]} #{e[2]}")
		end

  end

  class Entity_Decl < E

    def name
      "#{e[0]}"
    end

    def props(distribute)
      _props={}
      if e[1].is_a?(Entity_Decl_Array_Spec)
        array_props(e[1].e[1].e[0],_props,distribute)
      end
      _props["sort"]=((array?)?("_array"):("_scalar"))
      {name=>_props}
    end

  end

  class Entity_Decl_1 < Entity_Decl

    def array?
      e[1].is_a?(Entity_Decl_Array_Spec)
    end

  end

  class Entity_Decl_2 < Entity_Decl

    def array?
      # need to determine array/scalar spec of named function here?
      false
		end
  end

  class Entity_Decl_Array_Spec < E
  end

  class Entity_Decl_List < E

    def varprops(distribute)
      e[0].props(distribute).merge(e[1].props(distribute))
    end

  end

  class Entity_Decl_List_Pair < T

    def array?
      e[1].array?
    end

    def name
      e[1].name
    end

    def props(distribute)
      e[1].props(distribute)
    end

  end

  class Entity_Decl_List_Pairs < T

    def props(distribute)
      e.reduce({}) { |m,x| m.merge(x.props(distribute)) }
    end

  end

  class Entry_Name < E
  end

  class Entry_Stmt < T
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{sb(e[4])}")
		end

  end

  class Equivalence_Set_List < T
    
		def to_s
			list_to_s
		end

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

  class Expr < E

    def to_s
      "#{e[0]}"+((e[1].e)?("#{e[1].e[0]}#{e[1].e[1]}"):(""))
    end

  end

  class Expr_List < T
    
		def to_s
			list_to_s
		end

  end

  class External_Name < E
  end

  class External_Name_List < T
    
		def to_s
			list_to_s
		end

  end

  class Function_Name < E
  end

  class Function_Prefix < T

    def any?(o)
      fail "Expected string or class" unless o.is_a?(Symbol) or o.is_a?(Class)
      e.each do |x|
        return x if ((o.is_a?(Symbol))?("#{x}"=="#{o}"):(x.is_a?(o)))
      end
      nil
    end

    def elemental?
      any?(:elemental)
    end

    def pure?
      any?(:pure)
    end

    def recursive?
      any?(:recursive)
    end

    def type_spec
      any?(Type_Spec)
    end

		def to_s
			e.map { |x| "#{x}" }.join(" ")
		end

  end

  class Function_Reference < E
  end

  class Function_Stmt < T

    def elemental?
      function_prefix.elemental?
    end

    def function_name
      e[3]
    end

    def function_prefix
      e[1]
    end

    def pure?
      function_prefix.pure?
    end

    def recursive?
      function_prefix.recursive?
    end

    def to_s
      s="\n"+stmt("#{sa(e[1])}#{e[2]} #{e[3]}#{e[4]}#{e[5]}#{e[6]}#{sb(e[7])}")
      indent
      s
    end

    def type_spec
      (function_prefix.is_a?(Function_Prefix))?(function_prefix.type_spec):(nil)
    end

  end

  class Function_Subprogram < Scoping_Unit

    def elemental?
      function_stmt.elemental?
    end

    def function_name
      function_stmt.function_name
    end

    def function_prefix
      function_stmt.function_prefix
    end

    def function_stmt
      e[0]
    end

    def pure?
      function_stmt.pure?
    end

    def recursive?
      function_stmt.recursive?
    end

    def function_stmt_type_spec
      function_stmt.type_spec
    end

  end

  class Generic_Name < E
  end

  class Generic_Spec < T

    def localname
      usename
    end

    def name
      usename
    end

    def to_s
      "#{e[0]} #{e[1]}#{e[2]}#{e[3]}"
    end

    def usename
      "#{e[2]}"
    end

  end

  class If_Construct < E
  end

  class If_Construct_Name < E
  end

  class If_Stmt < T

    def action
      "#{e[5].to_s.strip}"
    end

    def prefix
      "#{e[1]} #{e[2]}#{e[3]}#{e[4]}"
    end

    def to_s
      stmt("#{prefix} #{action}")
    end

  end

  class If_Then_Stmt < T

    def to_s
      s=stmt("#{sa(e[1])}#{e[2]} #{e[3]}#{e[4]}#{e[5]} #{e[6]}")
      indent
      s
    end

  end

  class Implicit_None_Stmt < E
    
		def to_s
			stmt(space)+"\n"
		end

  end

  class Implicit_Spec_List < T
    
		def to_s
			list_to_s
		end

  end

  class Implicit_Stmt < E
    
		def to_s
			stmt(space)
		end

  end

  class Initialization < T
    
		def to_s
			"#{e[0]}#{e[1]}"
		end

  end

  class Initialization_1 < Initialization
  end

  class Initialization_2 < Initialization
  end

  class Inner_Shared_Do_Construct < T

    def label
      e[0].label
    end

    def to_s
      cat
    end

  end

  class Input_Item_List < T
    
		def to_s
			list_to_s
		end

  end

  class Input_Item_List_Pair < E

    def input_item
      e[1]
		end

  end

  class Inquire_Spec_List < T
    
		def to_s
			list_to_s
		end

  end

  class Inquire_Stmt_1 < T
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}")
		end

  end

  class Inqurie_Stmt_2 < T
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]}#{e[7]}")
		end

  end

  class Intent_Stmt < T
    
		def to_s
			stmt("#{e[1]}#{e[2]}#{e[3]}#{e[4]}#{ik(e[5],"::"," ")}#{e[6]}")
		end

  end

  class Interface_Body < E
  end

  class Interface_Body_1 < Interface_Body
  end

  class Interface_Body_2 < Interface_Body
  end

  class Interface_Stmt < T

    def to_s
      s="\n#{stmt(space)}"
      indent
      s
    end

  end

  class Internal_Subprograms < T


    def to_s
      e.map { |x| "#{x}" }.join
    end

  end

  class Intrinsic_Procedure_Name < E
  end

  class Intrinsic_Procedure_Name_List < T
    
		def to_s
			list_to_s
		end

  end

  class IO_Control_Spec_List < IO_Spec_List
  end

  class IO_Control_Spec_List_Pair < E

    def io_control_spec
      e[1]
    end

  end

  class IO_Implied_Do_Object_List < T
    
		def to_s
			list_to_s
		end

  end

  class IO_Spec < E

    def relabel_spec(spec)
      old=self.rhs
      new=label_create
      replace_element("#{new}",:label,self.e[2])
      [old,new]
    end

    def rhs
      "#{e[2]}"
    end

  end

  class IO_Spec_End < IO_Spec

    def relabel
      relabel_spec(:end)
    end

  end

  class IO_Spec_Eor < IO_Spec

    def relabel
      relabel_spec(:eor)
    end

  end

  class IO_Spec_Err < IO_Spec

    def relabel
      relabel_spec(:err)
    end

  end

  class IO_Spec_Iostat < IO_Spec
  end

  class IO_Spec_Size < IO_Spec
  end

  class IO_Spec_Unit < IO_Spec

    def unit
      e[2]
    end

  end

  class Kind_Selector < T

    def kind
      "#{e[2]}"
		end

  end

  class Label < T
  end

  class Label_Do_Stmt < T

    def label
      "#{e[3]}"
    end

    def to_s
      s=stmt("#{sa(e[1])}#{e[2]} #{e[3]}#{e[4]}")
      indent
      s
    end

  end

  class Label_List < T
    
		def to_s
			list_to_s
		end

  end

  class Letter_Spec_List < T
    
		def to_s
			list_to_s
		end

  end

  class Local_Name < E
  end

  class Loop_Control < T
  end

  class Loop_Control_1 < Loop_Control
    
		def to_s
			"#{ir(e[0],""," ")}#{e[1]}#{e[2]}#{e[3]}#{e[4]}#{e[5]}"
		end

  end

  class Loop_Control_2 < Loop_Control
    
		def to_s
			"#{ir(e[0],""," ")}#{e[1]} #{e[2]}#{e[3]}#{e[4]}"
		end

  end

  class Loop_Control_Pair < T

    def value
      e[1]
		end

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

  class Module_Name < E
  end

  class Module_Stmt < T

    def name
      "#{e[2]}"
    end

    def to_s
      s="\n#{stmt(space)}"
      indent
      s
    end

  end

  class Module_Subprogram_Part < T
    
		def to_s
			"#{e[0]}#{e[1].e.reduce("") { |m,x| m+="#{x}" } }"
		end

  end

  class Mult_Operand < E

    def to_s
      "#{e[0]}#{e[1]}"
    end

  end

  class Name < T

    def name
      to_s
    end

  end

  class Named_Constant < E
  end

  class Named_Constant_Def < E

    def name
      e[0]
		end

  end

  class Named_Constant_Def_List < T

    def names
      [e[0].name]+e[1].e.reduce([]) { |m,x| m.push(x.e[1].name) }
    end

    def to_s
      list_to_s
    end

  end

  class Namelist_Group_Name < E
  end

  class Namelist_Group_Object_List < T

    def objects
      e[1].e.reduce(["#{e[0]}"]) { |m,x| m.push("#{x.e[1]}") }
    end

    def to_s
      list_to_s
    end

  end

  class Namelist_Group_Set < T

    def set
      ["#{e[1]}",e[3].objects]
    end

    def to_s
      "#{e[0]}#{e[1]}#{e[2]} #{e[3]}"
    end

  end

  class Namelist_Group_Sets < E

    def sets
      e.reduce([]) { |m,x| m.push(x.set) }
    end

  end

  class Namelist_Group_Set_Pair < T

    def set
      e[1].set
    end

    def to_s
      "#{ir(e[0],""," ")}#{e[1]}"
    end

  end

  class Namelist_Stmt < T

    def sets
      e[3].sets.push(e[2].set)
    end

    def to_s
      stmt("#{e[1]} #{e[2]}#{e[3]}")
    end

  end

  class Nonblock_Do_Construct < E

    def label
      e[0].label
    end

  end

  class Nonlabel_Do_Stmt < T

    def to_s
      s=stmt("#{sa(e[1])}#{e[2]}#{e[3]}")
      indent
      s
    end

  end

  class Object_Name < E
  end

  class Only < E

    def localname
      (e[0].is_a?(Only_Option))?(e[0].localname):(usename)
    end

    def usename
      "#{e[1]}"
    end

  end

  class Only_List < T

    def localnames
      e[1].e.reduce([e[0].localname]) { |m,x| m.push(x.localname) }
    end

    def usenames
      e[1].e.reduce([e[0].usename]) { |m,x| m.push(x.usename) }
    end

  end

  class Only_List_Pair < T

    def localname
      e[1].localname
    end

    def usename
      e[1].usename
    end

  end

  class Only_Option < T

    def localname
      "#{e[0]}"
		end

  end

  class Open_Stmt < IO_Stmt
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}")
		end

  end

  class Optional_Stmt < T
    
		def to_s
			stmt("#{e[1]}#{ik(e[2],"::"," ")}#{e[3]}")
		end

  end

  class Outer_Shared_Do_Construct < E

    def label
      e[0].label
		end

  end

  class Output_Item_List < T
    
		def to_s
			list_to_s
		end

    def output_items
      [e[0]]+e[1].e.reduce([]) { |m,x| m.push(x.output_item) }
    end

  end

  class Output_Item_List_Pair < E

    def output_item
      e[1]
		end

  end

  class Parenthesized_Args < E
  end

  class Parenthesized_Deferred_Shape_Spec_List < T

    def abstract_boundslist
      e[1].abstract_boundslist
    end

    def array_spec
      e[1]
    end

  end

  class Parenthesized_Explicit_Shape_Spec_List < T

    def abstract_boundslist
      e[1].abstract_boundslist
		end

  end

  class Parenthesized_Section_Subscript_List < E

    def subscript_list
      e[1].subscript_list
    end

  end

  class Part_Name < E
  end

  class Part_Ref < E

    def name
      e[0].name
    end

    def subscript_list
      (e[1].is_a?(Parenthesized_Section_Subscript_List))?(e[1].subscript_list):([])
    end

  end

  class Pointer_Object_List < T
    
		def to_s
			list_to_s
		end

  end

  class Pointer_Stmt < T
    
		def to_s
			stmt("#{e[1]}#{ir(e[2],""," ")}#{e[3]}")
		end

  end

  class Position_Spec_List < T
    
		def to_s
			list_to_s
		end

  end

  class Power_Op < T
  end

  class Power_Op_Option < E
  end

  class Print_Stmt < T
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}")
		end

  end

  class Print_Stmt_Output_Item_List < T
    
		def to_s
			"#{e[0]}#{e[1]}"
		end

  end

  class Procedure_Name < E
  end

  class Procedure_Name_List < T
    
		def to_s
			list_to_s
		end

  end

  class Program_Name < E
  end

  class Program_Stmt < T

    def to_s
      s=stmt(space)
      indent
      s
    end

  end

  class Program_Units < T
    
		def to_s
			e.reduce("") { |m,x| m+="#{x}\n" }.chomp
		end

  end

  class Read_Stmt < T
  end

  class Read_Stmt_1 < Read_Stmt
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}#{sb(e[5])}")
		end

  end

  class Read_Stmt_2 < Read_Stmt
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}")
		end

  end

  class Read_Stmt_Input_Item_List_Option

    def items
      e[1].items
    end

  end

  class Rename < E

    def localname
      "#{e[0]}"
    end

    def usename
      "#{e[2]}"
    end

  end

  class Rename_List < E

    def localnames
      e[1].e.reduce([e[0].localname]) { |m,x| m.push(x.localname) }
    end

    def usenames
      e[1].e.reduce([e[0].usename]) { |m,x| m.push(x.usename) }
    end

  end

  class Rename_List_Pair < E

    def localname
      e[1].localname
    end

    def usename
      e[1].usename
    end

  end

  class Rename_List_Option < T

    def localnames
      e[1].localnames
    end

    def usenames
      e[1].usenames
    end

  end

  class Result_Name < E
  end

  class Rewind_Stmt_1 < IO_Stmt
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}")
		end

  end

  class Rewind_Stmt_2 < IO_Stmt
    
		def to_s
			stmt("#{e[1]} #{e[2]}")
		end

  end
  class Save_Stmt < T
    
		def to_s
			stmt("#{e[1]}#{e[2]}")
		end

  end

  class Save_Stmt_Entity_List < T
    
		def to_s
			"#{ir(e[0],""," ")}#{e[1]}"
		end

  end

  class Saved_Entity_List < T
    
		def to_s
			list_to_s
		end

  end

  class Section_Subscript_List < E

    def subscript_list
      e[1].elements.reduce([e[0]]) { |m,x| m.push(x.e[1]) }
    end

    def to_s
      list_to_s
    end

  end

  class Select_Case_Stmt < T

    def to_s
      s=stmt("#{sa(e[1])}#{e[2]} #{e[3]} #{e[4]}#{e[5]}#{e[6]}")
      indent
      indent
      s
    end

  end

  class Specification_Part < T
    
		def to_s
			"\n#{cat}\n"
		end

  end

  class Star_Int < T

    def kind
      "#{e[1]}"
		end

  end

  class Stmt_Function_Stmt < StmtC

    def name
      "#{e[1]}"
		end

  end

  class Stop_Stmt < StmtJ
  end

  class Subroutine_Name < E
  end

  class Subroutine_Prefix < T
    
		def to_s
			e.map { |x| "#{x}" }.join(" ")
		end

  end

  class Subroutine_Subprogram < Scoping_Unit
  end

  class Subroutine_Stmt < T

    def to_s
      s="\n"+stmt("#{sa(e[1])}#{e[2]} #{e[3]}#{e[4]}")
      indent
      s
    end

  end

  class Substring < T

    def name
      e[0].name
		end

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
    
		def to_s
			stmt("#{e[1]}#{ir(e[2],""," ")}#{e[3]}")
		end

  end

  class Type_Declaration_Stmt < T
    
		def to_s
			stmt("#{e[1]}#{ir(e[2],"",ik(e[1],","," "))}#{e[3]}")
		end

  end

  class Type_Name < E
  end

  class Type_Spec < E

    def derived?
      "#{e[0]}"=="type"
    end

    def kind
      return (e[1].respond_to?(:kind))?(e[1].kind):("_default")
    end

    def type
      (derived?)?("#{e[2]}"):("#{e[0]}")
    end

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

  class Use_Name < E
  end

  class Use_Part < T
    
		def to_s
			"\n#{cat}\n"
		end

  end

  class Use_Stmt < T

    def modulename
      "#{e[2]}"
		end

  end

  class Use_Stmt_1 < Use_Stmt

    def localnames
      e[3].localnames
    end

    def to_s
      stmt("#{e[1]} #{e[2]}#{e[3]}")
    end

    def usenames
      e[3].usenames
    end

  end

  class Use_Stmt_2 < Use_Stmt

    def localnames
      e[6].localnames
    end

    def to_s
      stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}#{e[5]}#{e[6]}")
    end

    def usenames
      e[6].usenames
    end

  end

  class Variable_Name < E
  end

  class Where_Construct_Stmt < T

    def to_s
      s=stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}")
      indent
      s
    end

  end

  class Where_Stmt < T
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]} #{e[6].to_s.strip}")
		end

  end

  class Write_Stmt < IO_Stmt
    
		def to_s
			stmt("#{e[1]} #{e[2]}#{e[3]}#{e[4]}#{sb(e[5])}")
    end

  end

end
