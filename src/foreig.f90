module foreig

  use kinds

  implicit none

  private
  public :: eig

   !===============================================================================
  interface eig
     procedure :: eig_rel
  end interface
  !===============================================================================

contains

  !===============================================================================
  !> author: Seyed Ali Ghasemi
  pure subroutine eig_rel(matrix, eig_vec, eig_val, method)
     real(rk), dimension(:,:),              intent(in)           :: matrix
     character(*),                          intent(in), optional :: method
     real(rk), dimension(:,:), allocatable, intent(out)          :: eig_vec
     real(rk), dimension(:),   allocatable, intent(out)          :: eig_val

     if (.not. present(method)) then
        call dsyev_rel(matrix, eig_vec, eig_val)
     else

        select case (method)
         case ('syev')
           call dsyev_rel(matrix, eig_vec, eig_val)
        end select

     end if

  end subroutine eig_rel
  !===============================================================================


  !===============================================================================
  !> author: Seyed Ali Ghasemi
  pure subroutine dsyev_rel(matrix, eig_vec, eig_val)
     real(rk), dimension(:,:),              intent(in)  :: matrix
     real(rk), dimension(:,:), allocatable, intent(out) :: eig_vec
     real(rk), dimension(:),   allocatable, intent(out) :: eig_val
     real(rk), dimension(:),   allocatable              :: work
     real(rk), dimension(size(matrix,1),size(matrix,1)) :: A
     integer                                            :: i, lwork, m, info
     real(rk)                                           :: work1(1)

     interface
        pure subroutine dsyev(fjobz, fuplo, fn, fA, flda, fw, fwork, flwork, finfo)
           import rk
           character, intent(in)    :: fjobz, fuplo
           integer,   intent(in)    :: fn
           integer,   intent(in)    :: flda
           integer,   intent(in)    :: flwork
           integer,   intent(out)   :: finfo
           real(rk),  intent(inout) :: fA(flda,*)
           real(rk),  intent(out)   :: fw(*)
           real(rk),  intent(out)   :: fwork(*)
        end subroutine
     end interface

     m = size(matrix,1)
     A = matrix

     call dsyev('V', 'U', m, A, m, eig_val, work1, -1, info)
     lwork = nint(work1(1))
     allocate(work(lwork))

     allocate(eig_vec(m,m), eig_val(m))

     call dsyev('V', 'U', m, A, m, eig_val, work, lwork, info)

     eig_vec = A

     deallocate(work)
  end subroutine dsyev_rel
  !===============================================================================

end module foreig
