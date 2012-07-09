module Fortran

  class Treetop::Runtime::SyntaxNode
    def to_s() '' end
  end

  class ASTNode < Treetop::Runtime::SyntaxNode

    @@level=0

    def blockbegin(s)
      @@level+=1
      s
    end

    def blockend(s)
      @@level-=1 unless @@level==0
      s.sub(/^  /,'')
    end

    def cat()
      elements.map { |e| e.to_s }.join
    end

    def get(k)
      (@attrs.nil?)?(nil):(@attrs[k])
    end

    def indent(s)
      '  '*@@level+s
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
      (m=~/e(\d+)/)?(elements[$~[1].to_i]):('')
    end

    def set(k,v)
      (@attrs.nil?)?(@attrs={k=>v}):(@attrs[k]=v)
    end

    def stmt(s)
      indent(s.chomp)+"\n"
    end

    def to_s()
      cat
    end

    def verbatim()
      text_value
    end

  end

  # TODO auto-gen empty classes?

  class Assign_Stmt < ASTNode
    def to_s() stmt(join) end
  end

  class End_Program_Stmt < ASTNode
    def name() e2 end
    def to_s() blockend(stmt(join)) end
  end

  class Execution_Part < ASTNode
  end

  class Execution_Part_Construct < ASTNode
  end

  class Format_Stmt < ASTNode
    def to_s() stmt(join) end
  end

  class Main_Program < ASTNode
  end

  class Name < ASTNode
    def to_s() verbatim end
  end

  class Print_Stmt < ASTNode
    def to_s() stmt(label+"#{e1} #{e2}#{e3}") end
  end

  class Program_Stmt < ASTNode
    def name() e1 end
    def to_s() blockbegin(stmt(join)) end
  end

  class Specification_Part < ASTNode
  end

  class Verbatim < ASTNode
    def to_s() verbatim end
  end

end

# paul.a.madden@noaa.gov
