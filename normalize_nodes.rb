module Normalize

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

  class Normalize < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.downcase
      t=t.gsub(/\t/,' ')
      t=t.gsub(/  */,'')
      t=t.gsub(/;/,"\n")
    end
  end

  class Verbatim < Treetop::Runtime::SyntaxNode
    def to_s
      text_value
    end
  end

end
