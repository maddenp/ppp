program t

! from Tom Henderson

  implicit none
  character (len=100) :: lawyers
  integer :: x, y, zzz

  x = 2
  y = 1
  zzz = x + &
        y
  print *, 'zzz = ',zzz
  zz&
  &z = x * &
       y
  print *, 'zzz = ',zzz
  zzz = x -&
y
  print *, 'zzz = ',zzz
  zzz = x -&
&y
  print *, 'zzz = ',zzz

  lawyers = 'Jones & Clay & &
  &Davis'
  print *,'LAWYERS_1 = <',trim(lawyers),'>'

  lawyers = 'Jones! &! Clay! &! &
  &Davis!'
  print *,'LAWYERS_2 = <',trim(lawyers),'>'

! suspicious...  is this outside of the standard?  
!  lawyers = 'Jones! &! Clay! &! &    ! a comment
!  &Davis!'
!  print *,'LAWYERS_3 = <',trim(lawyers),'>'

  lawyers = 'Jones & Clay & &

  &Davis'
  print *,'LAWYERS_4 = <',trim(lawyers),'>'

  lawyers = 'Jones & Clay & &
  &Davis&
  &'
  print *,'LAWYERS_5 = <',trim(lawyers),'>'

  lawyers = 'Jones & ''Clay'' & &
  &Davis'
  print *,'LAWYERS_6 = <',trim(lawyers),'>'

  lawyers = 'Jones & ""Clay"" & &
  &Davis'
  print *,'LAWYERS_7 = <',trim(lawyers),'>'
  
  lawyers = "Jones & ""Clay"" & &
  &Davis"
  print *,'LAWYERS_8 = <',trim(lawyers),'>'

  lawyers = "Jones & ''Clay'' & &
  &Davis"
  print *,'LAWYERS_9 = <',trim(lawyers),'>'

  lawyers = 'Jones & Clay & &
  & &
  &Davis'
  print *,'LAWYERS_10 = <',trim(lawyers),'>'

  lawyers = 'Jones & Clay & &
  &&
  &Davis'
  print *,'LAWYERS_11 = <',trim(lawyers),'>'

  lawyers = &  ! a comment
  'Jones & Clay & Da&
  &vis'
  print *,'LAWYERS_12 = <',trim(lawyers),'>'

  lawyers = 'Jones & Clay & Davis'
  print *,'LAWYERS_13 = <<',trim(lawyers),">&
  &>"

! We don't know if we're in a character context until we process the 
! next line (i.e. for processing leading space in the continuation line). 
! Not a problem for finding comments though...  
! Do all compilers like this?  (pgf90 does!)
  lawyers = 'Jones & ''Clay'&  ! a comment
  &' & Davis'  ! another comment
  print *,'LAWYERS_14 = <',trim(lawyers),'>'

! Tom's code had the following, which is non-standard (but apprently accepted
! by some compiler(s). The second line needs a leading '&' to be correct, since
! the continuation happens inside a character context. See Fortran 90 standard
! section 3.3.1.3.2.

! lawyers = 'Jones & ''Clay'&
!' & Davis'
! print *,'LAWYERS_15 = <',trim(lawyers),'>'

! Corrected version:

  lawyers = 'Jones & ''Clay'&
&' & Davis'
  print *,'LAWYERS_15 = <',trim(lawyers),'>'

!!! Syntax errors below...  
!
!  lawyers = 'Jones & ''Clay' ' & Davis'
!  print *,'LAWYERS_15 = <',trim(lawyers),'>'
!
!  lawyers = 'Jones & Clay & &
!  &
!  &Davis'
!  print *,'LAWYERS_5 = <',trim(lawyers),'>'
!
!  lawyers = 'Jones & Clay & &
!  &  ! just a comment?  
!  &Davis'
!  print *,'LAWYERS_4 = <',trim(lawyers),'>'

end program t
