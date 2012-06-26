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
      t=t.gsub(/block data/,'blockdata')
      t=t.gsub(/double precision/,'doubleprecision')
      t=t.gsub(/else if/,'elseif')
      t=t.gsub(/end blockdata/,'endblockdata')
      t=t.gsub(/end do/,'enddo')
      t=t.gsub(/end file/,'endfile')
      t=t.gsub(/end function/,'endfunction')
      t=t.gsub(/end if/,'endif')
      t=t.gsub(/end interface/,'endinterface')
      t=t.gsub(/end module/,'endmodule')
      t=t.gsub(/end program/,'endprogram')
      t=t.gsub(/end select/,'endselect')
      t=t.gsub(/end subroutine/,'endsubroutine')
      t=t.gsub(/end type/,'endtype')
      t=t.gsub(/end where/,'endwhere')
      t=t.gsub(/go to/,'goto')
      t=t.gsub(/in out/,'inout')
      t=t.gsub(/select case/,'selectcase')
    end
  end

  class Verbatim < Treetop::Runtime::SyntaxNode
    def to_s
      text_value
    end
  end

end
