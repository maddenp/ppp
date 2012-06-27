module Fortran

  # TODO ASTNode's to_s() should do elements.join as default?

  class Treetop::Runtime::SyntaxNode
    def to_s() '' end
  end

  class ASTNode < Treetop::Runtime::SyntaxNode
    def initialize (a='',b=(0..0),c=[]) super(a,b,c)               end
    def get(k)     (@attrs.nil?)?(nil):(@attrs[k])                 end
    def out()      elements.join(' ')                              end
    def set(k,v)   (@attrs.nil?)?(@attrs={k=>v}):(@attrs[k]=v)     end
    def to_s()     "!!! #{this.class} has no to_s, please fix"     end
    def verbatim() text_value                                      end
  end

  class Main_Program < ASTNode
    def to_s() elements.join end
  end

  class End_Program_Stmt < ASTNode
    def name() elements[2] end
    def to_s() out end
  end

  class Name < ASTNode
    def to_s() verbatim end
  end

  class Program_Stmt < ASTNode
    def name() elements[1] end
    def to_s() out end
  end

  class Terminal < ASTNode
    def to_s() verbatim end
  end

end
