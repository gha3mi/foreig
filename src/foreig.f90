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
   pure subroutine eig_rel(matrix, eig_vecr, eig_val, eig_vecl, method)
      real(rk), dimension(:,:),              intent(in)            :: matrix
      character(*),                          intent(in), optional  :: method
      real(rk), dimension(:,:), allocatable, intent(out)           :: eig_vecr
      real(rk), dimension(:,:), allocatable, intent(out), optional :: eig_vecl
      real(rk), dimension(:),   allocatable, intent(out)           :: eig_val

      if (.not. present(method)) then
         call dgeev_rel(matrix, eig_vecr, eig_val)
      else

         select case (method)
          case ('syev')
            call dsyev_rel(matrix, eig_vecr, eig_val)
          case ('geev')
            call dgeev_rel(matrix, eig_vecr, eig_val, eig_vecl)
          case ('ggev')
            call dggev_rel(matrix, eig_vecr, eig_val, eig_vecl)
         end select

      end if

   end subroutine eig_rel
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine dsyev_rel(matrix, eig_vecr, eig_val)
      real(rk), dimension(:,:),              intent(in)  :: matrix
      real(rk), dimension(:,:), allocatable, intent(out) :: eig_vecr
      real(rk), dimension(:),   allocatable, intent(out) :: eig_val
      real(rk), dimension(:),   allocatable              :: work
      real(rk), dimension(size(matrix,1),size(matrix,1)) :: A
      integer                                            :: lwork, m, info
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

      allocate(eig_vecr(m,m), eig_val(m))

      call dsyev('V', 'U', m, A, m, eig_val, work, lwork, info)

      eig_vecr = A

      deallocate(work)
   end subroutine dsyev_rel
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine dgeev_rel(matrix, eig_vecr, eig_val, eig_vecl)
      real(rk), dimension(:,:), intent(in)                         :: matrix
      real(rk), dimension(:),   allocatable, intent(out)           :: eig_val
      real(rk), dimension(:,:), allocatable, intent(out)           :: eig_vecr
      real(rk), dimension(:,:), allocatable, intent(out), optional :: eig_vecl
      real(rk), dimension(size(matrix,1))                          :: wr, wi
      real(rk), dimension(size(matrix,1),size(matrix,1))           :: vl, vr
      real(rk), dimension(size(matrix,1),size(matrix,1))           :: A
      real(rk), dimension(:), allocatable                          :: work
      integer                                                      :: m, lwork, info
      real(rk)                                                     :: work1(1)

      interface
         pure subroutine dgeev(fjobvl, fjobvr, fn, fA, flda, fwr, fwi, fvl, fldvl, fvr, fldvr, fwork, flwork, finfo)
            import :: rk
            character, intent(in)    :: fjobvl, fjobvr
            integer,   intent(in)    :: fn, flda, fldvl, fldvr, flwork, finfo
            real(rk),  intent(inout) :: fA(flda, *)
            real(rk),  intent(out)   :: fwr(fn), fwi(fn), fvl(fldvl, *), fvr(fldvr, *), fwork(flwork)
         end subroutine
      end interface

      m = size(matrix, 1)
      A = matrix

      if (present(eig_vecl)) then
         eig_vecl = matrix
         call dgeev('V', 'V', m, A, m, wr, wi, vl, m, vr, m, work1, -1, info)
      else
         call dgeev('N', 'V', m, A, m, wr, wi, vl, m, vr, m, work1, -1, info)
      end if

      lwork = nint(work1(1))
      allocate(work(lwork))

      if (present(eig_vecl)) then
         call dgeev('V', 'V', m, A, m, wr, wi, vl, m, vr, m, work, lwork, info)
         eig_vecl = vl
      else
         call dgeev('N', 'V', m, A, m, wr, wi, vl, m, vr, m, work, lwork, info)
      end if

      eig_val  = wr
      eig_vecr = vr

      deallocate(work)

   end subroutine dgeev_rel
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine dggev_rel(matrix, eig_vecr, eig_val, eig_vecl)
      real(rk), dimension(:,:), intent(in)                         :: matrix
      real(rk), dimension(:),   allocatable, intent(out)           :: eig_val
      real(rk), dimension(:,:), allocatable, intent(out)           :: eig_vecr
      real(rk), dimension(:,:), allocatable, intent(out), optional :: eig_vecl
      real(rk), dimension(size(matrix,1))                          :: alphar, alphai, beta
      real(rk), dimension(size(matrix,1),size(matrix,1))           :: vl, vr
      real(rk), dimension(:), allocatable                          :: work
      integer                                                      :: m, lwork, info
      real(rk)                                                     :: work1(1)
      real(rk), dimension(size(matrix,1),size(matrix,1))           :: A

      interface
         pure subroutine dggev(fjobvl, fjobvr, fn, fa, flda, fb, ldb, &
            falphar, falphai, fbeta, fvl, fldvl, fvr, fldvr, fwork, flwork, finfo)
            import :: rk
            character, intent(in)    :: fjobvl, fjobvr
            integer,   intent(in)    :: fn, flda, ldb, fldvl, fldvr, flwork, finfo
            real(rk),  intent(inout) :: fa(flda, *), fb(ldb, *)
            real(rk),  intent(out)   :: falphar(fn), falphai(fn), fbeta(fn)
            real(rk),  intent(inout) :: fvl(fldvl, *), fvr(fldvr, *)
            real(rk),  intent(out)   :: fwork(flwork)
         end subroutine
      end interface

      m = size(matrix, 1)
      allocate(eig_val(m), eig_vecr(m, m))
      eig_vecr = matrix
      A = matrix

      if (present(eig_vecl)) then
         eig_vecl = matrix
         call dggev('V', 'V', m, eig_vecl, m,&
            eig_vecr, m, alphar, alphai, beta, vl, m, vr, m, work1, -1, info)
      else
         call dggev('N', 'V', m, eig_vecr, m,&
            A, m, alphar, alphai, beta, vl, m, vr, m, work1, -1, info)
      end if

      lwork = nint(work1(1))
      allocate(work(lwork))

      if (present(eig_vecl)) then
         call dggev('V', 'V', m, eig_vecl, m, &
            eig_vecr, m, alphar, alphai, beta, vl, m, vr, m, work, lwork, info)
      else
         call dggev('N', 'V', m, eig_vecr, m,&
            A, m, alphar, alphai, beta, vl, m, vr, m, work, lwork, info)
      end if

      eig_val = alphar / beta

      deallocate(work)

   end subroutine dggev_rel
   !===============================================================================

end module foreig
