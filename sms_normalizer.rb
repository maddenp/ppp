module Normalizer

  class SMS < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      s=s.downcase          # make upper-case characters lower-case
      s=s.gsub(/[ \t]+/,"") # remove tabs & spaces
      s
    end
  end

end
