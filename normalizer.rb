module Normalizer
 
  def update(m1)
    to_update=["Quoted","Unquoted"]
    m0=Normalizer
    to_update.each do |e|
      m0.send(:remove_const,e) if m0.const_defined?(e)
      m0.const_set(e,m1.const_get(e.to_sym))
    end
  end

  class Comment < Treetop::Runtime::SyntaxNode
    def to_s
      s="\n"
      s
    end
  end

  class Directive < Treetop::Runtime::SyntaxNode
    def to_s
      s=text_value
      case input.op
      when 1
        s=input.stringmap.set(s)
      end
      s
    end
  end

  class Text < Treetop::Runtime::SyntaxNode
    def to_s
      s=elements.reduce("") { |s,e| s+="#{e}" }
      s
    end
  end

end
