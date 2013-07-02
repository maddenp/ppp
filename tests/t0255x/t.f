      program t
 100  format ( 58 H Hello 
     3World)
 200  format (2hA )
 300  format (2h B)
 400  format (a,1HC)
 500  format (a,1HD,a)
 600  format (1HE,a)
      write (*,100)
      write (*,200)
      write (*,300)
      write (*,400) '_'
      write (*,500) '_','_'
      write (*,600) '_'
      end program t
      
