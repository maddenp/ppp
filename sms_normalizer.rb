module Normalizer

  class SMS < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      s=s.downcase          # lower-case upper-case characters
      s=s.gsub(/[ \t]+/,"") # remove tabs & spaces
      s
    end
  end

end
