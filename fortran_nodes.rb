module Fortran

  class ASTNode < Treetop::Runtime::SyntaxNode
    def initialize(a='',b=(0..0),c=[]) super(a,b,c) end
    def get(k) (@attrs.nil?)?(nil):(@attrs[k]) end
    def set(k,v) (@attrs.nil?)?(@attrs={k=>v}):(@attrs[k]=v) end
    def to_s() "### #{this.class} has no to_s, please fix ###" end
  end

  class Main_Program < ASTNode
    def to_s() elements.join end
  end

  class End_Program_Stmt < ASTNode
    def program_name() '' end
  end

  class End_Program_Stmt_0 < End_Program_Stmt
    def program_name() elements[1].text_value.downcase end
    def to_s() "end program #{program_name}\n" end
  end

  class End_Program_Stmt_1 < End_Program_Stmt
    def to_s() "end program\n" end
  end

  class End_Program_Stmt_2 < End_Program_Stmt
    def to_s() "end\n" end
  end

  class Program_Stmt < ASTNode
    def program_name() '' end
  end

  class Program_Stmt_0 < Program_Stmt
    def program_name() elements[1].text_value.downcase end
    def to_s() "program #{program_name}\n" end
  end

  class Program_Stmt_1 < Program_Stmt
    def to_s() "" end
  end

end
