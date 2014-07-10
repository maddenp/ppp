module Normalizer

  class OMP < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      case input.op
      when 2
        s=input.stringmap.set(s)
      end
      s
    end
  end

  class SMS < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      s=s.downcase          # lower-case upper-case characters
      s=s.gsub(/[ \t]+/,"") # remove tabs & spaces
      s
    end
  end

end
