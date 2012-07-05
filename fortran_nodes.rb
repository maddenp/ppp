module Fortran

  # TODO ASTNode's to_s() should do elements.join as default?

  class Treetop::Runtime::SyntaxNode
    def to_s() '' end
  end

  class ASTNode < Treetop::Runtime::SyntaxNode
    @@level=0
    def blockend(s) @@level-=1 unless @@level==0; s.sub(/^  /,'') end
    def blockbegin(s) @@level+=1; s end
    def cat() elements.map { |e| e.to_s }.join end
    def get(k) (@attrs.nil?)?(nil):(@attrs[k]) end
    def indent(s) '  '*@@level+s end
    def initialize(a='',b=(0..0),c=[]) super(a,b,c) end
    def join() elements.map { |e| e.to_s }.join(' ') end
    def method_missing(m) (m=~/e(\d+)/)?(elements[$~[1].to_i]):('') end
    def set(k,v) (@attrs.nil?)?(@attrs={k=>v}):(@attrs[k]=v) end
    def stmt(s) indent(s.chomp)+"\n" end
    def to_s() "!!! #{this.class} has no to_s, please fix" end
    def verbatim() text_value end
  end

  class End_Program_Stmt < ASTNode
    def name() e2 end
    def to_s() blockend(stmt(join)) end
  end

  class Execution_Part < ASTNode
    def to_s() cat end
  end

  class Main_Program < ASTNode
    def to_s() cat end
  end

  class Name < ASTNode
    def to_s() verbatim end
  end

  class Print_Stmt < ASTNode
    def to_s() stmt("#{e0} #{e1}#{e2}") end
  end

  class Program_Stmt < ASTNode
    def name() e1 end
    def to_s() blockbegin(stmt(join)) end
  end

  class Verbatim < ASTNode
    def to_s() verbatim end
  end

end
