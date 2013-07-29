      program t
      implicit none
      integer::i=0
      
      if_label: if (i.eq.1) then
         print *,'bad'
      else if (i.eq.2) then if_label
      else if_label
         print *,'good'
      endif if_label
      
      endprogram t
