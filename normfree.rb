module Normfree

  class Normalize < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.gsub(/&$(\n&?)?/,"")   # join continuation lines
      t=t.gsub(/\s*;\s*/,"\n")   # split semicolon-delimited statement lines
      t=fix_h_edit_descriptor(t) # fix F90:1016 'h' edit descriptors
      t=t.gsub(/\n\n+/,"\n")     # remove blank lines
    end
  end

  class Quoted < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.gsub(/^!.*/,"")        # remove comment lines
      t=t.gsub(/\n\n+/,"\n")     # remove blank lines
      t=t.gsub(/&$\n&?/,"")      # join continuation lines
    end
  end

end
