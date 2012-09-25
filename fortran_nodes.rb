module Fortran

  @@env={}
  @@dolabels=[]
  @@level=0
  @@levelstack=[]
  @@uses={}

  ### PM ####

  def uses?(module_name,use_name)
    @@uses[module_name].include?(use_name)
  end

  def use_add(module_name,use_names)
    if @@uses[module_name].nil?
      @@uses[module_name]=use_names
    else
      unless uses?(module_name,:all)
        use_names.each do |e|
          @@uses[module_name] << e unless uses?(module_name,e)
        end
      end
    end
  end

  def use_update_1(module_name,rename_list_option)
    m="#{module_name}"
    if rename_list_option.is_a?(Fortran::Rename_List_Option)
      use_add(m,rename_list_option.usenames)
    else
      @@uses[m]=[:all]
    end
    true
  end

  def use_update_2(module_name,only_list)
    m="#{module_name}"
    if only_list.is_a?(Fortran::Only_List)
      use_add(m,only_list.usenames)
    else
      @@uses[m]=[:all]
    end
    true
  end

  ### PM ####

  def bb(s)
    @@level+=1
    s
  end

  def be
    @@level-=1 if @@level>0
  end

  def cat(f=nil)
    send(f) unless f.nil?
    self.elements.map { |e| e.to_s }.join
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

  def envget(k)
    @@env[k.to_s]||{}
  end

  def envset(k,v)
    @@env[k.to_s]=v
  end

  def fail(s)
    puts "\nERROR: "+s+"\n\nbacktrace:\n\n"
    begin
      raise
    rescue => e
      puts e.backtrace
    end
    puts
    exit(1)
  end

  def findabove(node,classes)
    n=node
    while p=n.parent
      return p if classes.any? { |e| p.kind_of?(e) }
      n=p
    end
    nil
  end

  def indent(s)
    ' '*2*@@level+s
  end

  def is_array?(node)
    envget(node.function_name)[:array]
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
    (e.to_s=='')?(''):("#{e} ")
  end

  def sb(e)
    (e.to_s=='')?(''):(" #{e}")
  end

  def scoping_unit(node)
    findabove(node,[Fortran::Scoping_Unit])
  end

  def sms(s)
    "#{e0}#{e1} "+s+"\n"
  end

  def space(x=nil)
    a=(x.nil?)?(self.elements[1..-1]):(self.elements)
    a.map { |e| e.to_s }.join(' ').strip
  end

  def specification_part(node)
    scoping_unit(node).e1
  end

  def stmt(s,f=nil)
    send(f) unless f.nil?
    indent(("#{sa(e0)}"+s.chomp).strip)+"\n"
  end

  def typeinfo(type_spec,attr_spec_option,entity_decl_list)
    props=entity_decl_list.props
    type=type_spec.type
    props.each { |k,v| v[:type]=type }
    if attr_spec_option.is_a?(Attr_Spec_Option) and attr_spec_option.dimension?
      props.each { |k,v| v[:array]=true }
    end
    props.each { |k,v| envset(k,v) }
    true
  end

  def use(node,tree)
    use_part(node).elements << tree
  end

  def use_part(node)
    specification_part(node).e0
  end

  # Extension of SyntaxNode class

  class Treetop::Runtime::SyntaxNode

    def to_s
      ''
    end

    def method_missing(m,*a)
      if m=~/e(\d+)/
        elements[$~[1].to_i]
      else
        fail "method_missing cannot find method '#{m}'"
      end
    end

  end

  # Generic Subclasses

  class T < Treetop::Runtime::SyntaxNode

    def initialize(a='',b=(0..0),c=[])
      super(a,b,c)
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
      space(:all)
    end
  end

  class Scoping_Unit < E
  end

  class StmtC < T
    def to_s
      stmt(elements[1..-1].map { |e| e.to_s }.join)
    end
  end

  class StmtJ < T
    def to_s
      stmt(space)
    end
  end

  # Specific Subclasses

  class Access_Stmt_Option < T
    def to_s() "#{mn(e0,'::',' ')}#{e1}" end
  end

  class Allocatable_Stmt < T
    def to_s() stmt("#{e1}#{mp(e2,'',' ')}#{e3}") end
  end

  class Arithmetic_If_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{e3}#{e4} #{e5}#{e6}#{e7}#{e8}#{e9}") end
  end

  class Assigned_Goto_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{mn(e3,',',' '+e3.to_s)}") end
  end

  class Attr_Spec_Dimension < T
  end

  class Attr_Spec_List < T
    def dimension?()
      e0.is_a?(Attr_Spec_Dimension) or (e1 and e1.dimension?)
    end
  end

  class Attr_Spec_List_Pair < T
    def dimension?() e1.is_a?(Attr_Spec_Dimension) end
  end

  class Attr_Spec_List_Pairs < T
    def dimension?()
      elements.each { |e| return true if e.dimension? }
      false
    end
  end

  class Attr_Spec_Option < T
    def dimension?() e1.dimension? end
  end

  class Block_Data < Scoping_Unit
  end

  class Block_Data_Stmt < T
    def to_s() bb(stmt(space)) end
  end

  class Call_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{e3}") end
  end

  class Case_Stmt < T
    def to_s() bb(stmt(space,:be)) end
  end

  class Common_Block_Name_And_Object_List < T
    def to_s() "#{mp(e0,'',' ')}#{e1}#{e2}" end
  end

  class Common_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{e3}#{e4}") end
  end

  class Component_Def_Stmt < T
    def to_s() stmt("#{e1}#{mp(e2,'',' ')}#{e3}") end
  end

  class Computed_Goto_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{e3}#{e4}#{mp(e5,'',' ')}#{e6}") end
  end

  class Contains_Stmt < T
    def to_s() bb(stmt(space,:be)) end
  end

  class Derived_Type_Stmt < T
    def to_s() bb(stmt("#{e1}#{sb(e2)} #{e3}")) end
  end

  class Dimension_Stmt < T
    def to_s() stmt("#{e1}#{mp(e2,'',' ')}#{e3}") end
  end

  class Do_Term_Action_Stmt < T
    def to_s() stmt(space,:be) end
  end

  class Do_Term_Shared_Stmt < T
    def to_s() cat(:be) end
  end

  class Double_Colon < T
  end

  class Else_If_Stmt < T
    def to_s() bb(stmt("#{e1} #{e2}#{e3}#{e4} #{e5}",:be)) end
  end

  class Else_Stmt < T
    def to_s() bb(stmt(space,:be)) end
  end

  class Elsewhere_Stmt < T
    def to_s() bb(stmt(space,:be)) end
  end

  class End_Block_Data_Option < T
    def to_s() space(:all) end
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
    def to_s() space(:all) end
  end

  class End_Module_Stmt < T
    def to_s() stmt(space,:be) end
  end

  class End_Program_Stmt < T
    def to_s() stmt("#{e1}#{sb(e3)}#{sb(e4)}",:be) end
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
    def name() "#{e0}" end
    def props() {:name=>name,:array=>array?} end
  end

  class Entity_Decl_1 < Entity_Decl
    def array?() e1.is_a?(Fortran::Entity_Decl_Array_Spec) end
  end

  class Entity_Decl_2 < Entity_Decl
    def array?() false end
  end

  class Entity_Decl_Array_Spec < T
  end

  class Entity_Decl_List < T
    def props
      x={e0.props[:name]=>e0.props}
      e1.props.each { |e| x[e[:name]]=e } unless e1.nil?
      x
    end
  end

  class Entity_Decl_List_Pair < T
    def array?() e1.array? end
    def name() "#{e1.name}" end
    def props() e1.props end
  end

  class Entity_Decl_List_Pairs < T
    def props() elements.inject([]) { |m,e| m << e.props } end
  end

  class Entry_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{e3}#{sb(e4)}") end
  end

  class Function_Stmt < T
    def to_s() bb(stmt("#{sa(e1)}#{e2} #{e3}#{e4}#{e5}#{e6}#{sb(e7)}")) end
  end

  class Function_Subprogram < Scoping_Unit
  end

  class If_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{e3}#{e4} #{e5.to_s.strip}") end
  end

  class If_Then_Stmt < T
    def to_s() bb(stmt("#{e1} #{e2} #{e3}#{e4}#{e5} #{e6}")) end
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
    def to_s() stmt("#{e1}#{e2}#{e3}#{e4}#{mn(e5,'::',' ')}#{e6}") end
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
    def to_s() bb(stmt("#{sa(e1)}#{e2} #{e3}#{e4}")) end
  end

  class Loop_Control < T
  end

  class Loop_Control_1 < Loop_Control
    def to_s() "#{mp(e0,'',' ')}#{e1}#{e2}#{e3}#{e4}#{e5}" end
  end

  class Loop_Control_2 < Loop_Control
    def to_s() "#{mp(e0,'',' ')}#{e1} #{e2}#{e3}#{e4}" end
  end

  class Main_Program < Scoping_Unit
  end

  class Module < Scoping_Unit
  end

  class Module_Stmt < T
    def to_s() bb(stmt(space)) end
  end

  class Module_Subprogram_Part < T
    def to_s() "#{e0}#{elements[1].elements.reduce('') { |m,e| m << "#{e}" }}" end
  end

  class Name < T
  end

  class Namelist_Group_Set_Pair < T
    def to_s() "#{mp(e0,'',' ')}#{e1}" end
  end

  class Namelist_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{e3}") end
  end

  class Nonlabel_Do_Stmt < T
    def to_s() bb(stmt("#{sa(e1)}#{e2}#{e3}")) end
  end

  class Only < E
    def localname() (e0.is_a?(Only_Option))?(e0.localname):(usename) end
    def usename() "#{e1}" end
  end

  class Only_List < T
    def localnames() e1.elements.reduce([e0.localname]) { |m,e| m << e.localname } end
    def usenames() e1.elements.reduce([e0.usename]) { |m,e| m << e.usename } end
  end

  class Only_List_Pair < T
    def localname() e1.localname end
    def usename() e1.usename end
  end

  class Only_Option < T
    def localname() "#{e0}" end
  end

  class Optional_Stmt < T
    def to_s() stmt("#{e1}#{mn(e2,'::',' ')}#{e3}") end
  end

  class Pointer_Stmt < T
    def to_s() stmt("#{e1}#{mp(e2,'',' ')}#{e3}") end
  end

  class Print_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{e3}") end
  end

  class Program_Stmt < T
    def to_s() bb(stmt(space)) end
  end

  class Program_Units < T
    def to_s() elements.reduce('') { |m,e| m << "#{e}\n" }.chomp end
  end

  class Read_Stmt < T
  end

  class Read_Stmt_1 < Read_Stmt
    def to_s() stmt("#{e1}#{e2}#{e3}#{e4}#{sb(e5)}") end
  end

  class Read_Stmt_2 < Read_Stmt
    def to_s() stmt("#{e1} #{e2}#{e3}") end
  end

  class Rename < E
    def localname() "#{e0}" end
    def usename() "#{e2}" end
  end

  class Rename_List < E
    def localnames() e1.elements.reduce([e0.localname]) { |m,e| m << e.localname } end
    def usenames() e1.elements.reduce([e0.usename]) { |m,e| m << e.usename } end
  end

  class Rename_List_Pair < E
    def localname() e1.localname end
    def usename() e1.usename end
  end

  class Rename_List_Option < T
    def localnames() e1.localnames end
    def usenames() e1.usenames end
  end

  class Save_Stmt < T
    def to_s() stmt("#{e1}#{e2}") end
  end

  class Save_Stmt_Entity_List < T
    def to_s() "#{mp(e0,'',' ')}#{e1}" end
  end

  class Select_Case_Stmt < T
    def to_s() bb(bb(stmt("#{sa(e1)}#{e2} #{e3} #{e4}#{e5}#{e6}",:ls))) end
  end

  class Specification_Part < E
  end

  ## SMS ##

  class SMS_Distribute_Begin < T
    def to_s() sms("#{e2} #{e3}") end
  end

  class SMS_Distribute_End < T
    def to_s() sms("#{e2}") end
  end

  class SMS_Halo_Comp_Begin < T
    def to_s() sms("#{e2} #{e3}") end
  end

  class SMS_Halo_Comp_End < T
    def to_s() sms("#{e2}") end
  end

  class SMS_Ignore_Begin < T
    def to_s() sms("#{e2} #{e3}") end
  end

  class SMS_Ignore_End < T
    def to_s() sms("#{e2}") end
  end

  class SMS_Parallel_Begin < T
    def to_s() sms("#{e2} #{e3}") end
  end

  class SMS_Parallel_End < T
    def to_s() sms("#{e2}") end
  end

  class SMS_Serial_Begin < T
    def to_s() sms("#{e2} #{e3}") end
  end

  class SMS_Serial_End < T
    def to_s() sms("#{e2}") end
  end

  class SMS_To_Local_Begin < T
    def to_s() sms("#{e2} #{e3}") end
  end

  class SMS_To_Local_End < T
    def to_s() sms("#{e2}") end
  end

  ## SMS ##

  class Subroutine_Subprogram < Scoping_Unit
  end

  class Subroutine_Stmt < T
    def to_s() bb(stmt("#{sa(e1)}#{e2} #{e3}#{e4}")) end
  end

  class Target_Stmt < T
    def to_s() stmt("#{e1}#{mp(e2,'',' ')}#{e3}") end
  end

  class Type_Declaration_Stmt < T
    def to_s() stmt("#{e1}#{mp(e2,'',mn(e1,',',' '))}#{e3}") end
  end

  class Type_Spec < E
    def derived?() "#{e0}"=="type" end
    def type() (derived?)?("#{e2}"):("#{e0}") end
  end

  class Use_Part < E
  end

  class Use_Stmt < T
  end

  class Use_Stmt_1 < Use_Stmt
    def modulename() "#{e2}" end
    def localnames() e3.localnames end
    def usenames() e3.usenames end
    def to_s() stmt("#{e1} #{e2}#{e3}") end
  end

  class Use_Stmt_2 < Use_Stmt
    def modulename() "#{e2}" end
    def to_s() stmt("#{e1} #{e2}#{e3}#{e4}#{e5}#{e6}") end
  end

  class Where_Construct_Stmt < T
    def to_s() bb(stmt("#{e1} #{e2}#{e3}#{e4}")) end
  end

  class Where_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{e3}#{e4} #{e6.to_s.strip}") end
  end

  class Write_Stmt < T
    def to_s() stmt("#{e1}#{e2}#{e3}#{e4}#{sb(e5)}") end
  end

end

# paul.a.madden@noaa.gov
