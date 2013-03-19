require "normfree"
require "sms_normfree_parser"

module Normfree

  class SMS < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.downcase          # make upper-case characters lower-case
      t=t.gsub(/[ \t]+/,"") # remove tabs & spaces
      t="\n"+t
    end
  end

end

# paul.a.madden@noaa.gov
