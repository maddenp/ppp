module Normfixed

  class Quoted < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      s
    end
  end

  class Unquoted < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      s=s.gsub(/\n[ \t]{5}_/,"") # join continuation lines
      s
    end
  end

end
