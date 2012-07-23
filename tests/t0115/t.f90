program p
  integer,pointer::i
  integer,target::j=3
100 i=>j
200 print *,i
300 nullify(i)
end program p
