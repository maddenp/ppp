module Fortran

  @@level=0

  def nonblock_do_begin(node)
    @ndls||=[]
    @ndls.push(node.dolabel)
  end

  def nonblock_do_end?(node)
    return false unless defined?(@ndls) and node.respond_to?(:label)
    ("#{node.label}"=="#{@ndls.last}")?(true):(false)
  end

  def nonblock_do_end!(node)
    @ndls.pop if nonblock_do_end?(node)
  end

  class Treetop::Runtime::SyntaxNode
    def to_s
      ''
    end
  end

  class ASTNode < Treetop::Runtime::SyntaxNode

    def blockbegin
      @@level+=1
    end

    def blockend
      @@level-=1 if @@level>0
    end

    def cat
      elements.map { |e| e.to_s }.join
    end

    def fail(msg)
      puts "\nERROR: "+msg+"\n\nbacktrace:\n\n"
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
      cat
    end

    def verbatim
      text_value
    end

  end

  # General Subclasses

  class Stmt < ASTNode
    def to_s
      stmt(join)
    end
  end

  class Verbatim < ASTNode
    def to_s
      verbatim
    end
  end

  # Specific Subclasses

# TODO auto-gen empty classes?

  class Action_Term_Do_Construct < ASTNode
  end

  class Action_Term_Do_Construct_Label_Do_Stmt < ASTNode
    def to_s
      blockend
      "#{e0}"
    end
  end

  class Arithmetic_If_Stmt < ASTNode
    def to_s
      stmt("#{sa(e0)}#{e1} #{e2}#{e3}#{e4} #{e5}#{e6}#{e7}#{e8}#{e9}")
    end
  end

  class Assigned_Goto_Stmt < ASTNode
    def to_s
      stmt("#{sa(e0)}#{e1} #{e2}#{(e3.to_s[0]==',')?(e3):(' '+e3.to_s)}")
    end
  end

  class Assignment_Stmt < ASTNode
    def to_s
      stmt("#{sa(e0)}#{e1}#{e2}#{e3}")
    end
  end

  class Block_Do_Construct < ASTNode
  end
  
  class Computed_Goto_Stmt < ASTNode
    def to_s
      stmt("#{sa(e0)}#{e1} #{e2}#{e3}#{e4}#{(e5.to_s=='')?(' '):(e5)}#{e6}")
    end
  end

  class Do_Term_Action_Stmt < ASTNode
    def to_s
      blockend
      s=stmt(join)
      s
    end
  end

  class Else_Construct < ASTNode
  end

  class Else_If_Construct < ASTNode
  end

  class Else_If_Construct_Element < ASTNode
  end

  class Else_If_Stmt < ASTNode
    def to_s
      blockend
      s=stmt(join)
      blockbegin
      s
    end
  end

  class Else_Stmt < ASTNode
    def to_s
      blockend
      s=stmt(join)
      blockbegin
      s
    end
  end

  class End_Do_Stmt < ASTNode
    def to_s
      blockend
      s=stmt(join)
      s
    end
  end

  class End_If_Stmt < ASTNode
    def to_s
      blockend
      s=stmt("#{sa(e0)}#{e1}#{sb(e2)}")
      s
    end
  end

  class End_Program_Stmt < ASTNode
    def to_s
      blockend
      s=stmt(join)
      s
    end
  end

  class Execution_Part < ASTNode
  end

  class Execution_Part_Construct < ASTNode
  end

  class If_Construct < ASTNode
  end

  class If_Stmt < ASTNode
    def to_s
      stmt("#{sa(e0)}#{e1} #{e2}#{e3}#{e4} #{e5.to_s.strip}")
    end
  end

  class If_Then_Construct < ASTNode
  end

  class If_Then_Stmt < ASTNode
    def to_s
      s=stmt("#{sa(e0)}#{e1} #{e2} #{e3}#{e4}#{e5} #{e6}")
      blockbegin
      s
    end
  end

  class Label_Do_Stmt < ASTNode
    def to_s
      s=stmt("#{sa(e0)}#{sa(e1)}#{e2} #{e3}#{e4}")
      blockbegin
      s
    end
  end

  class Loop_Control_1 < ASTNode
    def to_s
      "#{(e0)?(e0):(' ')}#{e1}#{e2}#{e3}#{e4}#{e5}"
    end
  end

  class Loop_Control_2 < ASTNode
    def to_s
      "#{(e0)?(e0):(' ')}#{e1} #{e2}#{e3}#{e4}"
    end
  end

  class Main_Program < ASTNode
  end

  class Nonlabel_Do_Stmt < ASTNode
    def to_s
      s=stmt("#{sa(e0)}#{sa(e1)}#{e2}#{e3}")
      blockbegin
      s
    end
  end

  class Print_Stmt < ASTNode
    def to_s
      stmt("#{sa(e0)}#{e1} #{e2}#{e3}")
    end
  end

  class Program_Stmt < ASTNode
    def to_s
      s=stmt(join)
      blockbegin
      s
    end
  end

  class Specification_Part < ASTNode
  end

  #PM#
  #PM#

end

# paul.a.madden@noaa.gov
