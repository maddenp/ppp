module Normfixed

  class Text < Treetop::Runtime::SyntaxNode
    def to_s
      elements.reduce("") { |s,e| s+="#{e}" }
    end
  end

  class Delete < Treetop::Runtime::SyntaxNode
    def to_s
      ""
    end
  end

  class Directive < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
    end
  end

  class Normalize < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      # Join continuation lines
      t=t.gsub(/$\n[ \t]{5}[^0 \t]/,"")
      # Attempting a multiline match of the regular expression below on a string
      # representing a huge source file is incredibly slow. Breaking the string
      # into lines improves performance significantly. 
      a=t.split("\n")
      a.each_index do |i|
        l=a[i]
        # Convert instances of F90:1016 char-string-edit-desc to quoted strings
        # to preserve case and whitespace.
        p="\(.*?[0-9]{1,5}[ \t]*format[ \t]*\\(.*?\)\([0-9]+\)[ \t]*[hH]\(.*?\)\\)\(.*\)"
        r=Regexp.new(p,true)
        while m=r.match(l)
          p1=m[3][0..m[2].to_i-1]
          p2=m[3].sub(/^#{p1}/,"")
          l="#{m[1]}'#{p1}'#{p2})#{m[4]}"
        end
        a[i]=l
      end
      t=a.join("\n")
      # Remove blank lines
      t=t.gsub(/\n\n+/,"\n")
    end
  end

  class Quoted < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
    end
  end

end
