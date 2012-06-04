module Fortran

  class Program < Treetop::Runtime::SyntaxNode
    def to_s
      elements.join
    end
  end

  class Program_Start_1 < Treetop::Runtime::SyntaxNode
    def to_s
      "program #{elements[1].text_value.downcase}\n"
    end
  end

  class Program_Start_2 < Treetop::Runtime::SyntaxNode
    def to_s
      ""
    end
  end

  class Program_End_1 < Treetop::Runtime::SyntaxNode
    def to_s
      "end program #{elements[1].text_value.downcase}\n"
    end
  end

  class Program_End_2 < Treetop::Runtime::SyntaxNode
    def to_s
      "end program\n"
    end
  end

  class Program_End_3 < Treetop::Runtime::SyntaxNode
    def to_s
      "end\n"
    end
  end

end
