class Treetop::Runtime::SyntaxNode
  def to_s
    text_value
  end
end

module Downcase

  class Text < Treetop::Runtime::SyntaxNode
    def to_s
      elements.reduce('') { |s,e| s+="#{e}" }
    end
  end

  class Comment < Treetop::Runtime::SyntaxNode
  end

  class QuotedText < Treetop::Runtime::SyntaxNode
  end

  class SMS < Treetop::Runtime::SyntaxNode
    def to_s
      text_value.downcase
    end
  end

  class UnquotedText < Treetop::Runtime::SyntaxNode
    def to_s
      text_value.downcase
    end
  end

end
