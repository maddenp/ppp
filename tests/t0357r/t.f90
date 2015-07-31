program t
  implicit none
  integer::i,j
  integer::a(2)=0
  integer::b(2,2)=0
  integer::c(4)=0
  integer::d(2)=0
  integer::e(2)=0
  integer::f(3,3)=0
  integer,parameter::stride=2
  type ttt
    integer,pointer::p=>null()
  end type ttt
  type(ttt)::arr_of_ttt(2)
  integer,target::x=88
11 forall (i=1:2) a(i)=i
  print *,a
22 forall (i=1:2,j=1:2) b(i,j)=i+j
  print *,b
33 forall (i=1:4:2) c(i)=i
  print *,c
44 forall (i=1:2,.true.) d(i)=i
  print *,d
55 forall (i=1:2,mod(i,2).eq.0) e(i)=i
  print *,e
66 forall (i=1:3:stride,j=1:3:1,.true.) f(i,j)=i+j
  print *,f
77 forall (i=1:2) arr_of_ttt(i)%p => x
  do i=1,2
    print *,arr_of_ttt(i)%p
  end do
end program t
