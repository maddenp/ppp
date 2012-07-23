module Fortran

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
    $stderr.write("### #{s}\n")
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

    def blockbegin
      @@level+=1
    end

    def blockend
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
      (@attrs.nil?)?(nil):(@attrs[k])
    end

    def indent(s)
      ' '*2*@@level+s
    end

    def initialize(a='',b=(0..0),c=[])
      super(a,b,c)
    end

    def join
      elements.map { |e| e.to_s }.join(' ').strip
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

    def sa(e)
      (e.to_s=='')?(''):("#{e} ")
    end

    def sb(e)
      (e.to_s=='')?(''):(" #{e}")
    end

    def set(k,v)
      (@attrs.nil?)?(@attrs={k=>v}):(@attrs[k]=v)
    end

    def stmt(s)
      indent(s.chomp.strip)+"\n"
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

  class StmtC < T
    def to_s
      stmt(sa(elements[0])+elements[1..-1].map { |e| e.to_s }.join)
    end
  end

  class StmtJ < T
    def to_s
      stmt(join)
    end
  end

  # Specific Subclasses

  class Arithmetic_If_Stmt < T
    def to_s
      stmt("#{sa(e0)}#{e1} #{e2}#{e3}#{e4} #{e5}#{e6}#{e7}#{e8}#{e9}")
    end
  end

  class Assigned_Goto_Stmt < T
    def to_s
      stmt("#{sa(e0)}#{e1} #{e2}#{(e3.to_s[0]==',')?(e3):(' '+e3.to_s)}")
    end
  end

  class Case_Stmt < T
    def to_s
      blockend
      s=stmt("#{sa(e0)}#{e1} #{e2}")
      blockbegin
      s
    end
  end
  
  class Computed_Goto_Stmt < T
    def to_s
      stmt("#{sa(e0)}#{e1} #{e2}#{e3}#{e4}#{(e5.to_s=='')?(' '):(e5)}#{e6}")
    end
  end

  class Do_Term_Action_Stmt < T
    def to_s
      blockend
      stmt(join)
    end
  end

  class Do_Term_Shared_Stmt < T
    def to_s
      blockend
      cat
    end
  end

  class Else_If_Stmt < T
    def to_s
      blockend
      s=stmt(join)
      blockbegin
      s
    end
  end

  class Else_Stmt < T
    def to_s
      blockend
      s=stmt(join)
      blockbegin
      s
    end
  end

  class Elsewhere_Stmt < T
    def to_s
      blockend
      s=stmt("#{sa(e0)}#{e1}")
      blockbegin
      s
    end
  end

  class End_Do_Stmt < T
    def to_s
      blockend
      stmt(join)
    end
  end

  class End_If_Stmt < T
    def to_s
      blockend
      stmt("#{sa(e0)}#{e1}#{sb(e2)}")
    end
  end

  class End_Program_Stmt < T
    def to_s
      blockend
      stmt(join)
    end
  end

  class End_Select_Stmt < T
    def to_s
      levelreset
      stmt("#{sa(e0)}#{e1} #{e2}#{sb(e3)}")
    end
  end
  
  class End_Where_Stmt < T
    def to_s
      blockend
      stmt("#{sa(e0)}#{e1}")
    end
  end

  class If_Stmt < T
    def to_s
      stmt("#{sa(e0)}#{e1} #{e2}#{e3}#{e4} #{e5.to_s.strip}")
    end
  end

  class If_Then_Stmt < T
    def to_s
      s=stmt("#{sa(e0)}#{e1} #{e2} #{e3}#{e4}#{e5} #{e6}")
      blockbegin
      s
    end
  end

  class Inner_Shared_Do_Construct < T
    def to_s
      blockend
      cat
    end
  end

  class Label_Do_Stmt < T
    def to_s
      s=stmt("#{sa(e0)}#{sa(e1)}#{e2} #{e3}#{e4}")
      blockbegin
      s
    end
  end

  class Loop_Control_1 < T
    def to_s
      "#{(e0)?(e0):(' ')}#{e1}#{e2}#{e3}#{e4}#{e5}"
    end
  end

  class Loop_Control_2 < T
    def to_s
      "#{(e0)?(e0):(' ')}#{e1} #{e2}#{e3}#{e4}"
    end
  end

  class Nonlabel_Do_Stmt < T
    def to_s
      s=stmt("#{sa(e0)}#{sa(e1)}#{e2}#{e3}")
      blockbegin
      s
    end
  end

  class Print_Stmt < T
    def to_s
      stmt("#{sa(e0)}#{e1} #{e2}#{e3}")
    end
  end

  class Program_Stmt < T
    def to_s
      s=stmt(join)
      blockbegin
      s
    end
  end

  class Select_Case_Stmt < T
    def to_s
      s=stmt("#{sa(e0)}#{sa(e1)}#{e2} #{e3} #{e4}#{e5}#{e6}")
      levelset
      blockbegin
      blockbegin
      s
    end
  end
  
  class Type_Declaration_Stmt < T
    def to_s
      stmt("#{sa(e0)}#{e1}#{(e2.to_s.empty?)?(' '):(e2)}#{e3}")
    end
  end

  class Where_Construct_Stmt < T
    def to_s
      s=stmt("#{sa(e0)}#{e1} #{e2}#{e3}#{e4}")
      blockbegin
      s
    end
  end

  class Where_Stmt < T
    def to_s
      stmt("#{sa(e0)}#{e1} #{e2}#{e3}#{e4} #{e6.to_s.strip}")
    end
  end

  #PM#
  #PM#

end

# paul.a.madden@noaa.gov

# TODO: stmt() to handle optional label instead of each class?
