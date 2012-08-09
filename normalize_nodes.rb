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
      t=t.downcase       # make upper-case characters lower-case
      t=t.gsub(/\t/,' ') # convert tabs to spaces
      t=t.gsub(/  */,'') # remove spaces
      t=t.gsub(/;/,"\n") # split semicolon-delimited statement lines
    end
  end

  class Verbatim < Treetop::Runtime::SyntaxNode
    def to_s
      text_value
    end
  end

end

# paul.a.madden@noaa.gov
