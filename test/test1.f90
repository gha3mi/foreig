program test1

   use kinds
   use foreig, only: eig
   
   implicit none

   real(rk), dimension(:,:), allocatable :: A
   real(rk), dimension(:,:), allocatable :: eig_vec
   real(rk), dimension(:),   allocatable :: eig_val

   allocate(A(5,5), eig_vec(5,5), eig_val(5))
   call random_number(A)

   call eig(A, eig_vec, eig_val)

   print*, eig_vec
   print*, eig_val

   deallocate(A, eig_vec, eig_val)

end program test1
