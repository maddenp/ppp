module Fortran

  @@attrs={}
  @@dolabels=[]
  @@level=0
  @@levelstack=[]

  # Module methods

  def dolabel_dupe?
    "#{@@dolabels[-1]}"=="#{@@dolabels[-2]}"
  end
  
  def dolabel_pop(label)
    @@dolabels.pop
  end

  def dolabel_push(label)
    @@dolabels.push(label)
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

  # Extension of SyntaxNode class

  class Treetop::Runtime::SyntaxNode
    def to_s
      ''
    end
  end

  # Generic Subclasses

  class T < Treetop::Runtime::SyntaxNode

    def bb(s)
      @@level+=1
      s
    end

    def be
      @@level-=1 if @@level>0
    end

    def cat
      elements.map { |e| e.to_s }.join
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

    def get(k)
      @@attrs[k]
    end

    def indent(s)
      ' '*2*@@level+s
    end

    def initialize(a='',b=(0..0),c=[])
      super(a,b,c)
    end

    def levelreset
      @@level=@@levelstack.pop
    end

    def levelset
      @@levelstack.push(@@level)
    end

    def method_missing(m,*a)
      if m=~/e(\d+)/
        e=elements[$~[1].to_i]
        (e.to_s=='')?(nil):(e)
      else
        fail "method_missing cannot find method '#{m}'"
      end
    end

    def mn(p,c,v)
      (p.to_s!=c)?(v):(p)
    end
  
    def mp(p,c,v)
      (p.to_s==c)?(v):(p)
    end
  
    def sa(e)
      (e.to_s=='')?(''):("#{e} ")
    end

    def sb(e)
      (e.to_s=='')?(''):(" #{e}")
    end

    def set(k,v)
      @@attrs[k]=v
    end

    def space(x=nil)
      ((x.nil?)?(elements[1..-1]):(elements)).map { |e| e.to_s }.join(' ').strip
    end

    def stmt(s)
      indent(("#{sa(e0)}"+s.chomp).strip)+"\n"
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
      space
    end
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

  class Allocatable_Stmt < T
    def to_s() stmt("#{e1}#{mp(e2,'',' ')}#{e3}") end
  end
  
  class Arithmetic_If_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{e3}#{e4} #{e5}#{e6}#{e7}#{e8}#{e9}") end
  end

  class Assigned_Goto_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{mn(e3,',',' '+e3.to_s)}") end
  end

  class Block_Data_Stmt < T
    def to_s() bb(stmt(space)) end
  end
  
  class Call_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{e3}") end
  end
  
  class Case_Stmt < T
    def to_s
      be
      bb(stmt(space))
    end
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

  class Derived_Type_Stmt < T
    def to_s() bb(stmt(space)) end
  end

  class Dimension_Stmt < T
    def to_s() stmt("#{e1}#{mp(e2,'',' ')}#{e3}") end
  end
  
  class Do_Term_Action_Stmt < T
    def to_s
      be
      stmt(space)
    end
  end

  class Do_Term_Shared_Stmt < T
    def to_s
      be
      cat
    end
  end

  class Else_If_Stmt < T
    def to_s
      be
      bb(stmt(space))
    end
  end

  class Else_Stmt < T
    def to_s
      be
      bb(stmt(space))
    end
  end

  class Elsewhere_Stmt < T
    def to_s
      be
      bb(stmt(space))
    end
  end

  class End_Block_Data_Option < T
    def to_s() space(:all) end
  end

  class End_Block_Data_Stmt < T
    def to_s
      be
      stmt(space)
    end
  end
  
  class End_Do_Stmt < T
    def to_s
      be
      stmt(space)
    end
  end

  class End_If_Stmt < T
    def to_s
      be
      stmt(space)
    end
  end

  class End_Module_Option < T
    def to_s() space(:all) end
  end

  class End_Module_Stmt < T
    def to_s
      be
      stmt(space)
    end
  end
  
  class End_Program_Stmt < T
    def to_s
      be
      stmt(space)
    end
  end

  class End_Select_Stmt < T
    def to_s
      levelreset
      stmt(space)
    end
  end
  
  class End_Type_Stmt < T
    def to_s
      be
      stmt(space)
    end
  end

  class End_Where_Stmt < T
    def to_s
      be
      stmt(space)
    end
  end

  class Entity_Decl < T
    def name() space(:all) end
  end

  class Entity_Decl_List < T
    def names() [e0.name]+((e1.nil?)?([]):(e1.names)) end
  end

  class Entity_Decl_List_Pair < T
    def name() "#{e1.name}" end
  end

  class Entity_Decl_List_Pairs < T
    def names() elements.map { |e| e.name } end
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
    def to_s
      be
      cat
    end
  end

  class Label_Do_Stmt < T
    def to_s() bb(stmt("#{sa(e1)}#{e2} #{e3}#{e4}")) end
  end

  class Loop_Control_1 < T
    def to_s() "#{mp(e0,'',' ')}#{e1}#{e2}#{e3}#{e4}#{e5}" end
  end

  class Loop_Control_2 < T
    def to_s() "#{mp(e0,'',' ')}#{e1} #{e2}#{e3}#{e4}" end
  end

  class Module_Stmt < T
    def to_s() bb(stmt(space)) end
  end
  
  class Namelist_Group_Set_Pair < T
    def to_s() "#{mp(e0,'',' ')}#{e1}" end
  end

  class Namelist_Stmt < T
    def to_s() stmt("#{e1} #{e2}#{e3}") end
  end

  class Nonlabel_Do_Stmt < T
    def to_s() bb(stmt(space)) end
  end

  class Parameter_Stmt <T
    def to_s() stmt("#{e1} #{e2}#{e3}#{e4}") end
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
  
  class Read_Stmt_1 < T
    def to_s() stmt("#{e1}#{e2}#{e3}#{e4}#{sb(e5)}") end
  end

  class Read_Stmt_2 < T
    def to_s() stmt("#{e1} #{e2}#{e3}") end
  end

  class Save_Stmt < T
    def to_s() stmt("#{e1}#{e2}") end
  end

  class Save_Stmt_Entity_List < T
    def to_s() "#{mp(e0,'',' ')}#{e1}" end
  end
  
  class Select_Case_Stmt < T
    def to_s
      levelset
      bb(bb(stmt("#{sa(e1)}#{e2} #{e3} #{e4}#{e5}#{e6}")))
    end
  end
  
  class Target_Stmt < T
    def to_s() stmt("#{e1}#{mp(e2,'',' ')}#{e3}") end
  end
  
  class Type_Declaration_Stmt < T
    def to_s
      sep=(e1.to_s[-1]==",")?(""):(" ")
      stmt("#{e1}#{mp(e2,'',sep)}#{e3}")
    end
  end

  class Type_Spec < E
    def derived?() "#{e0}"=="type" end
    def type() (derived?)?("#{e2}"):("#{e0}") end
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
  
  #PM#
  class Contains_Stmt < T
    def to_s
      be
      bb(stmt(space))
    end
  end

  class Function_Stmt < T
    def to_s() bb(stmt("#{sa(e1)}#{e2} #{e3}#{e4}#{e5}#{e6}#{sb(e7)}")) end
  end

  class End_Function_Stmt < T
    def to_s
      be
      stmt(space)
    end
  end
  
  #PM#

end

# paul.a.madden@noaa.gov
