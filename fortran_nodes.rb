module Fortran

  class Program_End_1 < Treetop::Runtime::SyntaxNode
    def to_s
#     elements.each { |e| puts "element: #{e}" }
      "end program"
    end
  end

  class Program_End_2 < Treetop::Runtime::SyntaxNode
    def to_s
      "end program"
    end
  end

  class Program_End_3 < Treetop::Runtime::SyntaxNode
    def to_s
      "end"
    end
  end

end
