module Normfree

  class Quoted < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.gsub(/^!.*/,"")      # remove comment lines
      t=t.gsub(/\n\n+/,"\n")   # remove blank lines
      t=t.gsub(/&$\n&?/,"")    # join continuation lines
    end
  end

  class Unquoted < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.gsub(/&$(\n&?)?/,"") # join continuation lines
      t=t.gsub(/\s*;\s*/,"\n") # split semicolon-delimited statement lines
      t=Normalizer.csedfix(t)  # fix F90:1016 'h' edit descriptors
      t=t.gsub(/\n\n+/,"\n")   # remove blank lines
    end
  end

end
