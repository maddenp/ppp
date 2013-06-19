program t
  integer::i(:)
  pointer::i
  nullify(i)
  print *,associated(i)
end program t
