program t1
  implicit none
! io-control-spec read stmt err label (with error)
character*4::out

open (95, file='infile', status='old', access='direct', recl=3)

  read (95, rec=1, err=100) out

  print *,'i wish this'
  
  100 print *,'test was successful'
endprogram t1

