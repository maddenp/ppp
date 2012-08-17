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

  class Directive < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.gsub(/^\s+/,'') # left-justify lines
    end
  end

  class Normalize < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.downcase          # make upper-case characters lower-case
      t=t.gsub(/\t/,' ')    # convert tabs to spaces
      t=t.gsub(/  */,'')    # remove spaces
      t=t.gsub(/;/,"\n")    # split semicolon-delimited statement lines
      t=t.gsub(/&$\n&?/,'') # join continuation lines
      t=t.gsub(/\n+/,"\n")  # no blank lines
    end
  end

  class Quoted < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.gsub(/^\s+/,'')   # left-justify lines
      t=t.gsub(/^!.*/,'')   # no comment lines
      t=t.gsub(/\n+/,"\n")  # no blank lines
      t=t.gsub(/&$\n&?/,'') # join continuation lines
    end
  end

  class SMS < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.gsub(/^\s+/,'') # left-justify lines
    end
  end

end

# paul.a.madden@noaa.gov
