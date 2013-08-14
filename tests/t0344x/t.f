      program t
C test for nested information in format-item-list
      integer::ierr
 1234 format ()
 5678 format (((((a)))))
      write (*,1234,iostat=ierr) 'hello'
      if (ierr .ne. 0) then
         print *,'ok'
      endif
      write (*,5678) 'yes'
      endprogram t
