program t
  implicit none
  character(len=80)::msg
  integer,parameter::lun=6
  integer::stat
  flush 6
  flush lun
  flush (6)
  flush (lun)
  flush (unit=6)
  flush (unit=lun)
  flush (6,iostat=stat)
  flush (iomsg=msg,unit=6)
  flush (err=99,unit=6)
  flush (unit=6,iostat=stat,iomsg=msg)
99 continue
end program t
