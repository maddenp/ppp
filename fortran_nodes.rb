module Fortran

  class ASTNode < Treetop::Runtime::SyntaxNode
    def to_s
      "### undefined to_s, please fix ###"
    end
  end

  class Program < ASTNode
    def to_s
      elements.join
    end
  end

  class Program_End < ASTNode
  end

  class Program_End_1 < Program_End
    def to_s
      program_name=elements[1].text_value.downcase
      "end program #{program_name}\n"
    end
  end

  class Program_End_2 < Program_End
    def to_s
      "end program\n"
    end
  end

  class Program_End_3 < Program_End
    def to_s
      "end\n"
    end
  end


  class Program_Start < ASTNode
  end

  class Program_Start_1 < Program_Start
    def to_s
      program_name=elements[1].text_value.downcase
      "program #{program_name}\n"
    end
  end

  class Program_Start_2 < Program_Start
    def to_s
      ""
    end
  end

end
