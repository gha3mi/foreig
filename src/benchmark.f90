program benchmark

   use kinds
   use foreig, only: eig
   use fortime, only: timer
   
   implicit none

   real(rk), dimension(:,:), allocatable :: A
   real(rk), dimension(:,:), allocatable :: eig_vec
   real(rk), dimension(:),   allocatable :: eig_val
   integer                               :: m, i, ntests
   type(timer)                           :: t

   m = 100

   allocate(A(m,m), eig_vec(m,m), eig_val(m))
   call random_number(A)
   A = A*10.0_rk


   ntests = 5

   call t%timer_start()
   do i = 1, ntests
      call eig(A, eig_vec, eig_val, method='syev')
   end do
   call t%timer_stop(nloops=ntests, message='Elapsed time (syev): ')


   call t%timer_start()
   do i = 1, ntests
      call eig(A, eig_vec, eig_val, method='geev')
   end do
   call t%timer_stop(nloops=ntests, message='Elapsed time (geev): ')


   call t%timer_start()
   do i = 1, ntests
      call eig(A, eig_vec, eig_val, method='ggev')
   end do
   call t%timer_stop(nloops=ntests, message='Elapsed time (ggev): ')


   deallocate(A, eig_vec, eig_val)

end program benchmark
