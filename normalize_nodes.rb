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
      t=t.upcase
      t=t.gsub(/\t/,' ') # convert tabs to spaces
      t=t.gsub(/  */,'') # remove spaces
      t=t.gsub(/;/,"\n") # split semicolon-delimited statemtns
    end
  end

  class Verbatim < Treetop::Runtime::SyntaxNode
    def to_s
      text_value
    end
  end

end
