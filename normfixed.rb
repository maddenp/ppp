module Normfixed

  class Comment < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      s
    end
  end

  class Quoted < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      s
    end
  end

  class Unquoted < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      s=s.gsub(/\n[ \t]{5}a/,"") # join continuation lines
      s
    end
  end

end
