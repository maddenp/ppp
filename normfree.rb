module Normfree

  class Quoted < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      case input.op
      when 1
        s=s.gsub(/^!.*/,"")    # remove comment lines
        s=s.gsub(/\n\n+/,"\n") # remove blank lines
        s=s.gsub(/&$\n&?/,"")  # join continuation lines
      when 2
        s=input.stringmap.set(s)
      end
      s
    end
  end

  class Unquoted < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      case input.op
      when 1
        s=s.gsub(/& *$\n *&?/,"") # join continuation lines
        s=s.gsub(/ *; */,"\n")    # split semicolon-delimited lines
      end
      s
    end
  end

end
