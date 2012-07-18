module Fortran

  @@level=0

  def say(msg)
    $stderr.write("#PM# #{msg}\n")
  end

  def dolabel_push(label)
    @dolabel_stack||=[]
    @dolabel_stack.push(label)
  end

  def dolabel_pop(label)
    @dolabel_stack||=[]
    @dolabel_stack.pop
  end

  def dolabel_dupe?
    "#{@dolabel_stack[-1]}"=="#{@dolabel_stack[-2]}"
  end
  
  def nonblock_do_end?(node)
    return false unless defined?(@dolabel_stack) and node.respond_to?(:label)
    ("#{node.label}"=="#{@dolabel_stack.last}")?(true):(false)
  end

  def nonblock_do_end!(node)
    @dolabel_stack.pop if nonblock_do_end?(node)
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

  class Cat < ASTNode
    def to_s
      cat
    end
  end

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

  class If_Stmt < ASTNode
    def to_s
      stmt("#{sa(e0)}#{e1} #{e2}#{e3}#{e4} #{e5.to_s.strip}")
    end
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

  #PM#
  #PM#

end

# paul.a.madden@noaa.gov
