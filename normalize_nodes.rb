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

  class Normalize < Treetop::Runtime::SyntaxNode
    def to_s
      t=text_value
      # Basic transformations: Per [std:f90:3.1.1], lowercase is optional, so
      # use uppercase internally; Convert tabs to spaces for convenience; Per
      # [std:f90:3.3.1], multiple spaces are treated as one, so squeeze to
      # single space for convenience; Split semicolon-delimited lines
      # [std:f90:3.3.1.2].
      t=t.downcase
      t=t.gsub(/\t/,' ')
#     t=t.gsub(/  */,' ')
      t=t.gsub(/  */,'')
      t=t.gsub(/;/,"\n")
      # Remove optional spaces from keywords [std:f90:3.3.1].
      t=t.gsub(/BLOCK DATA/,'BLOCKDATA')
      t=t.gsub(/DOUBLE PRECISION/,'DOUBLEPRECISION')
      t=t.gsub(/ELSE IF/,'ELSEIF')
      t=t.gsub(/END BLOCKDATA/,'ENDBLOCKDATA')
      t=t.gsub(/END DO/,'ENDDO')
      t=t.gsub(/END FILE/,'ENDFILE')
      t=t.gsub(/END FUNCTION/,'ENDFUNCTION')
      t=t.gsub(/END IF/,'ENDIF')
      t=t.gsub(/END INTERFACE/,'ENDINTERFACE')
      t=t.gsub(/END MODULE/,'ENDMODULE')
      t=t.gsub(/END PROGRAM/,'ENDPROGRAM')
      t=t.gsub(/END SELECT/,'ENDSELECT')
      t=t.gsub(/END SUBROUTINE/,'ENDSUBROUTINE')
      t=t.gsub(/END TYPE/,'ENDTYPE')
      t=t.gsub(/END WHERE/,'ENDWHERE')
      t=t.gsub(/GO TO/,'GOTO')
      t=t.gsub(/IN OUT/,'INOUT')
      t=t.gsub(/SELECT CASE/,'SELECTCASE')
    end
  end

  class Verbatim < Treetop::Runtime::SyntaxNode
    def to_s
      text_value
    end
  end

end
