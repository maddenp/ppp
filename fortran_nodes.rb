module Fortran

  @@level=0
  
  class Treetop::Runtime::SyntaxNode
    def to_s() '' end
  end

  class ASTNode < Treetop::Runtime::SyntaxNode

    def blockbegin
      @@level+=1
    end

    def blockend
      @@level-=1 if @@level>0
    end

    def cat()
      elements.map { |e| e.to_s }.join
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

    def join()
      elements.map { |e| e.to_s }.join(' ').strip
    end

    def label()
      (e0.empty?)?(''):("#{e0} ")
    end

    def method_missing(m)
      (m=~/e(\d+)/)?(elements[$~[1].to_i]):(nil)
    end

    def set(k,v)
      (@attrs.nil?)?(@attrs={k=>v}):(@attrs[k]=v)
    end

    def stmt(s)
      indent(s.chomp.strip)+"\n"
    end

    def to_s()
      cat
    end

    def verbatim()
      text_value
    end

  end

  # General Subclasses

  class Stmt < ASTNode
    def to_s() stmt(join) end
  end

  class Verbatim < ASTNode
    def to_s() verbatim end
  end

  # Specific Subclasses

# TODO auto-gen empty classes?

  class Assigned_Goto_Stmt < ASTNode
    def to_s()
      s=stmt(label+"#{e1} #{e2}#{(e3.to_s[0]==',')?(e3):(' '+e3.to_s)}")
      s
    end
  end

  class Assignment_Stmt < ASTNode
    def to_s() stmt(label+"#{e1}#{e2}#{e3}") end
  end

  class Computed_Goto_Stmt < ASTNode
    def to_s()
      s=stmt(label+"#{e1} #{e2}#{e3}#{e4}#{(e5=='')?(' '):(e5)}#{e6}")
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
    def to_s()
      blockend
      s=stmt(join)
      blockbegin
      s
    end
  end

  class Else_Stmt < ASTNode
    def to_s()
      blockend
      s=stmt(join)
      blockbegin
      s
    end
  end

  class End_If_Stmt < ASTNode
    def to_s()
      blockend
      s=stmt(label+"#{e1}"+((e2)?(" #{e2}"):('')))
      s
    end
  end

  class End_Program_Stmt < ASTNode
    def name() e2 end
    def to_s() blockend; stmt(join) end
  end

  class Execution_Part < ASTNode
  end

  class Execution_Part_Construct < ASTNode
  end

  class If_Construct < ASTNode
  end

  class If_Stmt < ASTNode
    def to_s()
      s=stmt(label+"#{e1} #{e2}#{e3}#{e4} #{e5.to_s.strip}")
      s
    end
  end

  class If_Then_Construct < ASTNode
  end

  class If_Then_Stmt < ASTNode
    def to_s()
      s=stmt(label+"#{e1} #{e2} #{e3}#{e4}#{e5} #{e6}")
      blockbegin
      s
    end
  end

  class Main_Program < ASTNode
  end

  class Print_Stmt < ASTNode
    def to_s() stmt(label+"#{e1} #{e2}#{e3}") end
  end

  class Program_Stmt < ASTNode
    def name()
      e1
    end
    def to_s()
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
