module Fortran

  # TODO ASTNode's to_s() should do elements.join as default?

  class Treetop::Runtime::SyntaxNode
    def to_s() '' end
  end

  class ASTNode < Treetop::Runtime::SyntaxNode
    def cat()             elements.join                               end
    def get(k)            (@attrs.nil?)?(nil):(@attrs[k])             end
    def initialize        (a='',b=(0..0),c=[]) super(a,b,c)           end
    def join()            elements.join(' ')                          end
    def method_missing(m) (m=~/e(\d+)/)?(elements[$~[1]]):()          end
    def set(k,v)          (@attrs.nil?)?(@attrs={k=>v}):(@attrs[k]=v) end
    def to_s()            "!!! #{this.class} has no to_s, please fix" end
    def verbatim()        text_value                                  end
  end

  class Main_Program < ASTNode
    def to_s() cat end
  end

  class End_Program_Stmt < ASTNode
    def name() e2 end
    def to_s() join end
  end

  class Name < ASTNode
    def to_s() verbatim end
  end

  class Program_Stmt < ASTNode
    def name() e1 end
    def to_s() join end
  end

  class Terminal < ASTNode
    def to_s() verbatim end
  end

end
