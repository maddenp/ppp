module Normfixed

  class Quoted < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
    end
  end

  class Unquoted < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.gsub(/$\n[ \t]{5}[^0 \t]/,"") # join continuation lines
      t=Normalizer.csedfix(t,false)     # fix F90:1016 'h' edit descriptors
      t=t.gsub(/\n\n+/,"\n")            # remove blank lines
      t
    end
  end

end
