require "normcommon"

include Normcommon

module Normfixed

  class Normalize < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.gsub(/$\n[ \t]{5}[^0 \t]/,"") # join continuation lines
      t=fix_h_edit_descriptor(t)        # fix F90:1016 'h' edit descriptors
      t=t.gsub(/\n\n+/,"\n")            # remove blank lines
    end
  end

  class Quoted < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
    end
  end

end
