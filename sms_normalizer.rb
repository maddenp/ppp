module Normalizer
 
  class OMP < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      if s=~/^\s*@\$omp (end )?parallel do/i
        s=s.downcase          # lower-case upper-case characters
        s=s.gsub(/[ \t]+/,"") # remove tabs & spaces
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
