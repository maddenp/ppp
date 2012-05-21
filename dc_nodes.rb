module Downcase

  class Text < Treetop::Runtime::SyntaxNode
    def to_s
      elements.reduce('') { |s,e| s+="#{e}" }
    end
  end

  class Delete < Treetop::Runtime::SyntaxNode
    def to_s
      ''
    end
  end

  class Downcase < Treetop::Runtime::SyntaxNode
    def to_s
      text_value.downcase
    end
  end

  class Verbatim < Treetop::Runtime::SyntaxNode
    def to_s
      text_value
    end
  end

end
