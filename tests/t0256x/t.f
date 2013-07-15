      program t
      implicit none
c use omp_lib
      integer::id,nthreads,omp_get_thread_num
*$omp parallel private(nthreads,id)
      id=omp_get_thread_num()
      if (id.eq.1) then
         print *,'second thread'
      end if
!$omp end parallel
      end program t
