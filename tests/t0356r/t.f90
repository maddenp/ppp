program t
  implicit none
  integer,parameter::lun=6
  write (6,'(a)') 'x'
  flush (6)
  write (lun,'(a)') 'x'
  flush (lun)
end program t
