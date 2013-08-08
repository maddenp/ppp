program t
11111 FORMAT (5hHello) ; print 11111
22222 FORMAT (11hHello World) ; print 22222
33333 FORMAT ('Hello ',5HWorld) ; print 33333
44444 FORMAT ('Hello ',&
        5hWorld) ; print 44444
55555 FORMAT ('Hello ',&
  &5hWorld) ; print 55555
66666 FORMAT ('Hel&
        &lo ',&
  &5hWorld) ; print 66666
77777 FORMAT ('#2#',5HWorld) ; print 77777
end program t
