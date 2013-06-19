program t

  TYPE PERSON
    INTEGER AGE
  END TYPE PERSON

  TYPE OTHER
    TYPE(PERSON) P
  END TYPE OTHER
print *,kind (age) ,kind(p)
end program t
