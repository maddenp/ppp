module Normfree

  class Quoted < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      case input.control.op
      when 1
        s=s.gsub(/^!.*/,"")    # remove comment lines
        s=s.gsub(/\n\n+/,"\n") # remove blank lines
        s=s.gsub(/&$\n&?/,"")  # join continuation lines
      when 2
        s=input.control.stringmap.set(s)
      end
      s
    end
  end

  class Unquoted < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      case input.control.op
      when 1
        s=s.gsub(/&[ \t]*$\n[ \t]*&?/,"") # join continuation lines
        s=s.gsub(/[ \t]*;[ \t]*/,"\n")    # split semicolon-delimited lines
      end
      s
    end
  end

end
