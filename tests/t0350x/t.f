      p r o g r a m  t
c test for continued holleriths in call, data, and do statements 
      c h a r a c t e r 
     *(l e n = 6 ) a _ b ( 3 )
      c h a r a c t e r * 6 2
c Comment

     ': : x
      c h a r a c t e r * 5 : : l , m , n , o , p , q
      d a

*Comment
      
     ct a x /62HAbCdEfGhIjKlMnOpQrStUvWxYz1234567890aBcDeFgHiJkLmNoPqRsT

     auVwXyZ/

      character *60::a
      data a/60H!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ccomment
     !!!!!/
      print *,a



      d a t a l , m , n , o /5hHeLlo,5hwORLD,5hFrOm ,5hMaTT /,  p 
     !/5hwHy!?/ , q / 5hnO!!!/

      p r i n t * , l , m , o , p , q

      p r i n t *,x
      d a t a a _ 
     *b / 'Hello ' , 5 
     *HWorld , 1 h! /
      d a t a  c  / 5 H!!!!! /
      p r i n t * , a _ b  ! comment
1 0 0 f o r m a t ( " Hello ", 5 HWorld, 
     *2 h ! )
      p r i n t  1 0 0
      c a 
     *l l  s ( 1 h; )
      p r i n t * , c
      e n d p r o g r a m  t

      s u b r o u t i n e  s ( c )
      i n t e g e r  c ( 1 )
      p r 
     *i n
     *t * , c
      e n d  s u b r o u t i n e  s
