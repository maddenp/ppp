module Normalize

  class Text < Treetop::Runtime::SyntaxNode
    def to_s
      elements.reduce('') { |s,e| s+="#{e}" }
    end
  end

  class Delete < Treetop::Runtime::SyntaxNode
    def to_s
      ''
    end
  end

  class Directive < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.gsub(/^\s+/,'') # left-justify lines
    end
  end

  class Normalize < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.gsub(/&\s*$(\n\s*&?)?/,'') # join continuation lines
      t=t.gsub(/\s*;\s*/,"\n") # split semicolon-delimited statement lines
      # Convert instances of F90:1016 char-string-edit-desc to quoted strings to
      # preserve case and whitespace.
      a=t.split("\n")
      a.each_index do |i|
        l=a[i]
#       h=false
#       p="\(.*?[0-9]{1,5}[ \t]format[ \t]*\\(.*?\)\([0-9]+\)[ \t]*[hH]\(.*?\)\\)\(.*\)"
#       r=Regexp.new(p,Regexp::IGNORECASE|Regexp::MULTILINE)
#       while m=r.match(l)
#         h=true
#         p1=m[3][0..m[2].to_i-1]
#         p2=m[3].sub(/^#{p1}/,'')
#         l="#{m[1]}'#{p1}'#{p2})#{m[4]}"
#       end
        # If a F90:1016 conversion occurred, quoted strings have been introduced
        # and it is no longer safe to change case or whitespace. Note, though,
        # that these conversions can only happen in the first normalization pass,
        # so that the second pass can normalize case and whitespace.
#       unless h
          l=l.downcase # make upper-case characters lower-case
          l=l.gsub(/[ \t]+/,'') # remove tabs & spaces
#       end
        a[i]=l
      end
      t=a.join("\n")
      t=t.gsub(/\n\n+/,"\n"); # no blank lines
    end
  end

  class Quoted < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.gsub(/^!.*/,'')    # no comment lines
      t=t.gsub(/\n\n+/,"\n") # no blank lines
      t=t.gsub(/&$\n&?/,'')  # join continuation lines
    end
  end

  class SMS < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      t=t.gsub(/^\s+/,'')   # left-justify lines
      t=t.downcase          # make upper-case characters lower-case
      t=t.gsub(/[ \t]+/,'') # remove tabs & spaces
      t="\n"+t
    end
  end

end

# paul.a.madden@noaa.gov
