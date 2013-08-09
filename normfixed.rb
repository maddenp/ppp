module Normfixed

  class Comment < Treetop::Runtime::SyntaxNode
    def to_s
      # SyntaxNode#input and SyntaxNode#interval access the underlying text and
      # substring matched by this node, respectively. The 'comment' rule matches
      # through the newline, so looking one character past the interval puts us
      # on the next line. If the next line is a continuation (i.e. five spaces
      # followed by an 'a' character, then the current Comment must be removed
      # to allow the continuation to be joined correctly. Otherwise, what appears
      # to be a comment may actually be part of a hollerith, so leave it alone.
      # Note that (1) any tabs in leading whitespace have already been converted
      # to spaces by Translator#detabify; (2) column-six continuation characters
      # have already been converted to 'a' by Translator#fixed2free; and (3) the
      # hollerith-masking procedure will run before final parsing is attempted.

      s=("#{input}"[interval.max+1..interval.max+7]=~/^     a/)?("\n"):(text_value)
      s
    end
  end

  class Quoted < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      s
    end
  end

  class Unquoted < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      s=s.gsub(/\n[ \t]{5}a/,"") # join continuation lines
      s
    end
  end

end
