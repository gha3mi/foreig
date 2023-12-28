var tipuesearch = {"pages":[{"title":" foreig ","text":"foreig ForEig : A Fortran library for eigenvalue and eigenvector calculations. Usage use foreig , only : eig call eig ( A , eig_vec , eig_val , method = 'ggev' ) ! method='syev' .or. 'geev' .or. 'ggev' fpm dependency If you want to use ForEig as a dependency in your own fpm project,\nyou can easily include it by adding the following line to your fpm.toml file: [dependencies] forieig = { git = \"https://github.com/gha3mi/forieig.git\" } How to run tests Reuirements: Fortran Compiler, LAPACK or MKL Libraries Clone the repository: You can clone the ForEig repository from GitHub using the following command: git clone https://github.com/gha3mi/forieig.git cd forieig Intel Fortran Compiler (ifort) fpm @ifort-test Intel Fortran Compiler (ifx) fpm @ifx-test GNU Fortran Compiler (gfortran) fpm @gfortran-test NVIDIA Compiler (nvfortran) fpm @nvfortran-test API documentation The most up-to-date API documentation for the master branch is available here .\nTo generate the API documentation for ForEig using ford run the following\ncommand: ford ford.yml Contributing Contributions to ForEig are welcome!\nIf you find any issues or would like to suggest improvements, please open an issue. Developer Info Seyed Ali Ghasemi","tags":"home","loc":"index.html"},{"title":"eig_rel – foreig","text":"private pure subroutine eig_rel(matrix, eig_vecr, eig_val, eig_vecl, method) Arguments Type Intent Optional Attributes Name real(kind=rk), intent(in), dimension(:,:) :: matrix real(kind=rk), intent(out), dimension(:,:), allocatable :: eig_vecr real(kind=rk), intent(out), dimension(:), allocatable :: eig_val real(kind=rk), intent(out), optional, dimension(:,:), allocatable :: eig_vecl character(len=*), intent(in), optional :: method Calls proc~~eig_rel~~CallsGraph proc~eig_rel foreig::eig_rel proc~dgeev_rel foreig::dgeev_rel proc~eig_rel->proc~dgeev_rel proc~dggev_rel foreig::dggev_rel proc~eig_rel->proc~dggev_rel proc~dsyev_rel foreig::dsyev_rel proc~eig_rel->proc~dsyev_rel Help × Graph Key Nodes of different colours represent the following: Graph Key Subroutine Subroutine Function Function Interface Interface Type Bound Procedure Type Bound Procedure Unknown Procedure Type Unknown Procedure Type Program Program This Page's Entity This Page's Entity Solid arrows point from a procedure to one which it calls. Dashed \narrows point from an interface to procedures which implement that interface.\nThis could include the module procedures in a generic interface or the\nimplementation in a submodule of an interface in a parent module. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Called by proc~~eig_rel~~CalledByGraph proc~eig_rel foreig::eig_rel interface~eig foreig::eig interface~eig->proc~eig_rel program~benchmark benchmark program~benchmark->interface~eig program~test1 test1 program~test1->interface~eig Help × Graph Key Nodes of different colours represent the following: Graph Key Subroutine Subroutine Function Function Interface Interface Type Bound Procedure Type Bound Procedure Unknown Procedure Type Unknown Procedure Type Program Program This Page's Entity This Page's Entity Solid arrows point from a procedure to one which it calls. Dashed \narrows point from an interface to procedures which implement that interface.\nThis could include the module procedures in a generic interface or the\nimplementation in a submodule of an interface in a parent module. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Source Code pure subroutine eig_rel ( matrix , eig_vecr , eig_val , eig_vecl , method ) real ( rk ), dimension (:,:), intent ( in ) :: matrix character ( * ), intent ( in ), optional :: method real ( rk ), dimension (:,:), allocatable , intent ( out ) :: eig_vecr real ( rk ), dimension (:,:), allocatable , intent ( out ), optional :: eig_vecl real ( rk ), dimension (:), allocatable , intent ( out ) :: eig_val if (. not . present ( method )) then call dgeev_rel ( matrix , eig_vecr , eig_val ) else select case ( method ) case ( 'syev' ) call dsyev_rel ( matrix , eig_vecr , eig_val ) case ( 'geev' ) call dgeev_rel ( matrix , eig_vecr , eig_val , eig_vecl ) case ( 'ggev' ) call dggev_rel ( matrix , eig_vecr , eig_val , eig_vecl ) end select end if end subroutine eig_rel","tags":"","loc":"proc/eig_rel.html"},{"title":"dsyev_rel – foreig","text":"private pure subroutine dsyev_rel(matrix, eig_vecr, eig_val) Arguments Type Intent Optional Attributes Name real(kind=rk), intent(in), dimension(:,:) :: matrix real(kind=rk), intent(out), dimension(:,:), allocatable :: eig_vecr real(kind=rk), intent(out), dimension(:), allocatable :: eig_val Called by proc~~dsyev_rel~~CalledByGraph proc~dsyev_rel foreig::dsyev_rel proc~eig_rel foreig::eig_rel proc~eig_rel->proc~dsyev_rel interface~eig foreig::eig interface~eig->proc~eig_rel program~benchmark benchmark program~benchmark->interface~eig program~test1 test1 program~test1->interface~eig Help × Graph Key Nodes of different colours represent the following: Graph Key Subroutine Subroutine Function Function Interface Interface Type Bound Procedure Type Bound Procedure Unknown Procedure Type Unknown Procedure Type Program Program This Page's Entity This Page's Entity Solid arrows point from a procedure to one which it calls. Dashed \narrows point from an interface to procedures which implement that interface.\nThis could include the module procedures in a generic interface or the\nimplementation in a submodule of an interface in a parent module. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Source Code pure subroutine dsyev_rel ( matrix , eig_vecr , eig_val ) real ( rk ), dimension (:,:), intent ( in ) :: matrix real ( rk ), dimension (:,:), allocatable , intent ( out ) :: eig_vecr real ( rk ), dimension (:), allocatable , intent ( out ) :: eig_val real ( rk ), dimension (:), allocatable :: work real ( rk ), dimension ( size ( matrix , 1 ), size ( matrix , 1 )) :: A integer :: lwork , m , info real ( rk ) :: work1 ( 1 ) interface pure subroutine dsyev ( fjobz , fuplo , fn , fA , flda , fw , fwork , flwork , finfo ) import rk character , intent ( in ) :: fjobz , fuplo integer , intent ( in ) :: fn integer , intent ( in ) :: flda integer , intent ( in ) :: flwork integer , intent ( out ) :: finfo real ( rk ), intent ( inout ) :: fA ( flda , * ) real ( rk ), intent ( out ) :: fw ( * ) real ( rk ), intent ( out ) :: fwork ( * ) end subroutine end interface m = size ( matrix , 1 ) A = matrix call dsyev ( 'V' , 'U' , m , A , m , eig_val , work1 , - 1 , info ) lwork = nint ( work1 ( 1 )) allocate ( work ( lwork )) allocate ( eig_vecr ( m , m ), eig_val ( m )) call dsyev ( 'V' , 'U' , m , A , m , eig_val , work , lwork , info ) eig_vecr = A deallocate ( work ) end subroutine dsyev_rel","tags":"","loc":"proc/dsyev_rel.html"},{"title":"dgeev_rel – foreig","text":"private pure subroutine dgeev_rel(matrix, eig_vecr, eig_val, eig_vecl) Arguments Type Intent Optional Attributes Name real(kind=rk), intent(in), dimension(:,:) :: matrix real(kind=rk), intent(out), dimension(:,:), allocatable :: eig_vecr real(kind=rk), intent(out), dimension(:), allocatable :: eig_val real(kind=rk), intent(out), optional, dimension(:,:), allocatable :: eig_vecl Called by proc~~dgeev_rel~~CalledByGraph proc~dgeev_rel foreig::dgeev_rel proc~eig_rel foreig::eig_rel proc~eig_rel->proc~dgeev_rel interface~eig foreig::eig interface~eig->proc~eig_rel program~benchmark benchmark program~benchmark->interface~eig program~test1 test1 program~test1->interface~eig Help × Graph Key Nodes of different colours represent the following: Graph Key Subroutine Subroutine Function Function Interface Interface Type Bound Procedure Type Bound Procedure Unknown Procedure Type Unknown Procedure Type Program Program This Page's Entity This Page's Entity Solid arrows point from a procedure to one which it calls. Dashed \narrows point from an interface to procedures which implement that interface.\nThis could include the module procedures in a generic interface or the\nimplementation in a submodule of an interface in a parent module. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Source Code pure subroutine dgeev_rel ( matrix , eig_vecr , eig_val , eig_vecl ) real ( rk ), dimension (:,:), intent ( in ) :: matrix real ( rk ), dimension (:), allocatable , intent ( out ) :: eig_val real ( rk ), dimension (:,:), allocatable , intent ( out ) :: eig_vecr real ( rk ), dimension (:,:), allocatable , intent ( out ), optional :: eig_vecl real ( rk ), dimension ( size ( matrix , 1 )) :: wr , wi real ( rk ), dimension ( size ( matrix , 1 ), size ( matrix , 1 )) :: vl , vr real ( rk ), dimension ( size ( matrix , 1 ), size ( matrix , 1 )) :: A real ( rk ), dimension (:), allocatable :: work integer :: m , lwork , info real ( rk ) :: work1 ( 1 ) interface pure subroutine dgeev ( fjobvl , fjobvr , fn , fA , flda , fwr , fwi , fvl , fldvl , fvr , fldvr , fwork , flwork , finfo ) import :: rk character , intent ( in ) :: fjobvl , fjobvr integer , intent ( in ) :: fn , flda , fldvl , fldvr , flwork , finfo real ( rk ), intent ( inout ) :: fA ( flda , * ) real ( rk ), intent ( out ) :: fwr ( fn ), fwi ( fn ), fvl ( fldvl , * ), fvr ( fldvr , * ), fwork ( flwork ) end subroutine end interface m = size ( matrix , 1 ) A = matrix if ( present ( eig_vecl )) then eig_vecl = matrix call dgeev ( 'V' , 'V' , m , A , m , wr , wi , vl , m , vr , m , work1 , - 1 , info ) else call dgeev ( 'N' , 'V' , m , A , m , wr , wi , vl , m , vr , m , work1 , - 1 , info ) end if lwork = nint ( work1 ( 1 )) allocate ( work ( lwork )) if ( present ( eig_vecl )) then call dgeev ( 'V' , 'V' , m , A , m , wr , wi , vl , m , vr , m , work , lwork , info ) eig_vecl = vl else call dgeev ( 'N' , 'V' , m , A , m , wr , wi , vl , m , vr , m , work , lwork , info ) end if eig_val = wr eig_vecr = vr deallocate ( work ) end subroutine dgeev_rel","tags":"","loc":"proc/dgeev_rel.html"},{"title":"dggev_rel – foreig","text":"private pure subroutine dggev_rel(matrix, eig_vecr, eig_val, eig_vecl) Arguments Type Intent Optional Attributes Name real(kind=rk), intent(in), dimension(:,:) :: matrix real(kind=rk), intent(out), dimension(:,:), allocatable :: eig_vecr real(kind=rk), intent(out), dimension(:), allocatable :: eig_val real(kind=rk), intent(out), optional, dimension(:,:), allocatable :: eig_vecl Called by proc~~dggev_rel~~CalledByGraph proc~dggev_rel foreig::dggev_rel proc~eig_rel foreig::eig_rel proc~eig_rel->proc~dggev_rel interface~eig foreig::eig interface~eig->proc~eig_rel program~benchmark benchmark program~benchmark->interface~eig program~test1 test1 program~test1->interface~eig Help × Graph Key Nodes of different colours represent the following: Graph Key Subroutine Subroutine Function Function Interface Interface Type Bound Procedure Type Bound Procedure Unknown Procedure Type Unknown Procedure Type Program Program This Page's Entity This Page's Entity Solid arrows point from a procedure to one which it calls. Dashed \narrows point from an interface to procedures which implement that interface.\nThis could include the module procedures in a generic interface or the\nimplementation in a submodule of an interface in a parent module. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Source Code pure subroutine dggev_rel ( matrix , eig_vecr , eig_val , eig_vecl ) real ( rk ), dimension (:,:), intent ( in ) :: matrix real ( rk ), dimension (:), allocatable , intent ( out ) :: eig_val real ( rk ), dimension (:,:), allocatable , intent ( out ) :: eig_vecr real ( rk ), dimension (:,:), allocatable , intent ( out ), optional :: eig_vecl real ( rk ), dimension ( size ( matrix , 1 )) :: alphar , alphai , beta real ( rk ), dimension ( size ( matrix , 1 ), size ( matrix , 1 )) :: vl , vr real ( rk ), dimension (:), allocatable :: work integer :: m , lwork , info real ( rk ) :: work1 ( 1 ) real ( rk ), dimension ( size ( matrix , 1 ), size ( matrix , 1 )) :: A interface pure subroutine dggev ( fjobvl , fjobvr , fn , fa , flda , fb , ldb , & falphar , falphai , fbeta , fvl , fldvl , fvr , fldvr , fwork , flwork , finfo ) import :: rk character , intent ( in ) :: fjobvl , fjobvr integer , intent ( in ) :: fn , flda , ldb , fldvl , fldvr , flwork , finfo real ( rk ), intent ( inout ) :: fa ( flda , * ), fb ( ldb , * ) real ( rk ), intent ( out ) :: falphar ( fn ), falphai ( fn ), fbeta ( fn ) real ( rk ), intent ( inout ) :: fvl ( fldvl , * ), fvr ( fldvr , * ) real ( rk ), intent ( out ) :: fwork ( flwork ) end subroutine end interface m = size ( matrix , 1 ) allocate ( eig_val ( m ), eig_vecr ( m , m )) eig_vecr = matrix A = matrix if ( present ( eig_vecl )) then eig_vecl = matrix call dggev ( 'V' , 'V' , m , eig_vecl , m ,& eig_vecr , m , alphar , alphai , beta , vl , m , vr , m , work1 , - 1 , info ) else call dggev ( 'N' , 'V' , m , eig_vecr , m ,& A , m , alphar , alphai , beta , vl , m , vr , m , work1 , - 1 , info ) end if lwork = nint ( work1 ( 1 )) allocate ( work ( lwork )) if ( present ( eig_vecl )) then call dggev ( 'V' , 'V' , m , eig_vecl , m , & eig_vecr , m , alphar , alphai , beta , vl , m , vr , m , work , lwork , info ) else call dggev ( 'N' , 'V' , m , eig_vecr , m ,& A , m , alphar , alphai , beta , vl , m , vr , m , work , lwork , info ) end if eig_val = alphar / beta deallocate ( work ) end subroutine dggev_rel","tags":"","loc":"proc/dggev_rel.html"},{"title":"eig – foreig","text":"public interface eig Calls interface~~eig~~CallsGraph interface~eig foreig::eig proc~eig_rel foreig::eig_rel interface~eig->proc~eig_rel proc~dgeev_rel foreig::dgeev_rel proc~eig_rel->proc~dgeev_rel proc~dggev_rel foreig::dggev_rel proc~eig_rel->proc~dggev_rel proc~dsyev_rel foreig::dsyev_rel proc~eig_rel->proc~dsyev_rel Help × Graph Key Nodes of different colours represent the following: Graph Key Subroutine Subroutine Function Function Interface Interface Type Bound Procedure Type Bound Procedure Unknown Procedure Type Unknown Procedure Type Program Program This Page's Entity This Page's Entity Solid arrows point from a procedure to one which it calls. Dashed \narrows point from an interface to procedures which implement that interface.\nThis could include the module procedures in a generic interface or the\nimplementation in a submodule of an interface in a parent module. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Called by interface~~eig~~CalledByGraph interface~eig foreig::eig program~benchmark benchmark program~benchmark->interface~eig program~test1 test1 program~test1->interface~eig Help × Graph Key Nodes of different colours represent the following: Graph Key Subroutine Subroutine Function Function Interface Interface Type Bound Procedure Type Bound Procedure Unknown Procedure Type Unknown Procedure Type Program Program This Page's Entity This Page's Entity Solid arrows point from a procedure to one which it calls. Dashed \narrows point from an interface to procedures which implement that interface.\nThis could include the module procedures in a generic interface or the\nimplementation in a submodule of an interface in a parent module. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Module Procedures private pure subroutine eig_rel (matrix, eig_vecr, eig_val, eig_vecl, method) Author Seyed Ali Ghasemi Arguments Type Intent Optional Attributes Name real(kind=rk), intent(in), dimension(:,:) :: matrix real(kind=rk), intent(out), dimension(:,:), allocatable :: eig_vecr real(kind=rk), intent(out), dimension(:), allocatable :: eig_val real(kind=rk), intent(out), optional, dimension(:,:), allocatable :: eig_vecl character(len=*), intent(in), optional :: method","tags":"","loc":"interface/eig.html"},{"title":"foreig – foreig","text":"Uses kinds module~~foreig~~UsesGraph module~foreig foreig kinds kinds module~foreig->kinds Help × Graph Key Nodes of different colours represent the following: Graph Key Module Module Submodule Submodule Subroutine Subroutine Function Function Program Program This Page's Entity This Page's Entity Solid arrows point from a submodule to the (sub)module which it is\ndescended from. Dashed arrows point from a module or program unit to \nmodules which it uses. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Used by module~~foreig~~UsedByGraph module~foreig foreig program~benchmark benchmark program~benchmark->module~foreig program~test1 test1 program~test1->module~foreig Help × Graph Key Nodes of different colours represent the following: Graph Key Module Module Submodule Submodule Subroutine Subroutine Function Function Program Program This Page's Entity This Page's Entity Solid arrows point from a submodule to the (sub)module which it is\ndescended from. Dashed arrows point from a module or program unit to \nmodules which it uses. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Interfaces public        interface eig private pure subroutine eig_rel (matrix, eig_vecr, eig_val, eig_vecl, method) Author Seyed Ali Ghasemi Arguments Type Intent Optional Attributes Name real(kind=rk), intent(in), dimension(:,:) :: matrix real(kind=rk), intent(out), dimension(:,:), allocatable :: eig_vecr real(kind=rk), intent(out), dimension(:), allocatable :: eig_val real(kind=rk), intent(out), optional, dimension(:,:), allocatable :: eig_vecl character(len=*), intent(in), optional :: method Subroutines private pure subroutine eig_rel (matrix, eig_vecr, eig_val, eig_vecl, method) Author Seyed Ali Ghasemi Arguments Type Intent Optional Attributes Name real(kind=rk), intent(in), dimension(:,:) :: matrix real(kind=rk), intent(out), dimension(:,:), allocatable :: eig_vecr real(kind=rk), intent(out), dimension(:), allocatable :: eig_val real(kind=rk), intent(out), optional, dimension(:,:), allocatable :: eig_vecl character(len=*), intent(in), optional :: method private pure subroutine dsyev_rel (matrix, eig_vecr, eig_val) Author Seyed Ali Ghasemi Arguments Type Intent Optional Attributes Name real(kind=rk), intent(in), dimension(:,:) :: matrix real(kind=rk), intent(out), dimension(:,:), allocatable :: eig_vecr real(kind=rk), intent(out), dimension(:), allocatable :: eig_val private pure subroutine dgeev_rel (matrix, eig_vecr, eig_val, eig_vecl) Author Seyed Ali Ghasemi Arguments Type Intent Optional Attributes Name real(kind=rk), intent(in), dimension(:,:) :: matrix real(kind=rk), intent(out), dimension(:,:), allocatable :: eig_vecr real(kind=rk), intent(out), dimension(:), allocatable :: eig_val real(kind=rk), intent(out), optional, dimension(:,:), allocatable :: eig_vecl private pure subroutine dggev_rel (matrix, eig_vecr, eig_val, eig_vecl) Author Seyed Ali Ghasemi Arguments Type Intent Optional Attributes Name real(kind=rk), intent(in), dimension(:,:) :: matrix real(kind=rk), intent(out), dimension(:,:), allocatable :: eig_vecr real(kind=rk), intent(out), dimension(:), allocatable :: eig_val real(kind=rk), intent(out), optional, dimension(:,:), allocatable :: eig_vecl","tags":"","loc":"module/foreig.html"},{"title":"benchmark – foreig","text":"Uses foreig kinds fortime program~~benchmark~~UsesGraph program~benchmark benchmark fortime fortime program~benchmark->fortime kinds kinds program~benchmark->kinds module~foreig foreig program~benchmark->module~foreig module~foreig->kinds Help × Graph Key Nodes of different colours represent the following: Graph Key Module Module Submodule Submodule Subroutine Subroutine Function Function Program Program This Page's Entity This Page's Entity Solid arrows point from a submodule to the (sub)module which it is\ndescended from. Dashed arrows point from a module or program unit to \nmodules which it uses. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Calls program~~benchmark~~CallsGraph program~benchmark benchmark interface~eig foreig::eig program~benchmark->interface~eig timer_start timer_start program~benchmark->timer_start timer_stop timer_stop program~benchmark->timer_stop proc~eig_rel foreig::eig_rel interface~eig->proc~eig_rel proc~dgeev_rel foreig::dgeev_rel proc~eig_rel->proc~dgeev_rel proc~dggev_rel foreig::dggev_rel proc~eig_rel->proc~dggev_rel proc~dsyev_rel foreig::dsyev_rel proc~eig_rel->proc~dsyev_rel Help × Graph Key Nodes of different colours represent the following: Graph Key Subroutine Subroutine Function Function Interface Interface Type Bound Procedure Type Bound Procedure Unknown Procedure Type Unknown Procedure Type Program Program This Page's Entity This Page's Entity Solid arrows point from a procedure to one which it calls. Dashed \narrows point from an interface to procedures which implement that interface.\nThis could include the module procedures in a generic interface or the\nimplementation in a submodule of an interface in a parent module. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Variables Type Attributes Name Initial real(kind=rk), dimension(:,:), allocatable :: A real(kind=rk), dimension(:,:), allocatable :: eig_vec real(kind=rk), dimension(:), allocatable :: eig_val integer :: m integer :: i integer :: ntests type(timer) :: t Source Code program benchmark use kinds use foreig , only : eig use fortime , only : timer implicit none real ( rk ), dimension (:,:), allocatable :: A real ( rk ), dimension (:,:), allocatable :: eig_vec real ( rk ), dimension (:), allocatable :: eig_val integer :: m , i , ntests type ( timer ) :: t m = 100 allocate ( A ( m , m ), eig_vec ( m , m ), eig_val ( m )) call random_number ( A ) A = A * 1 0.0_rk ntests = 5 call t % timer_start () do i = 1 , ntests call eig ( A , eig_vec , eig_val , method = 'syev' ) end do call t % timer_stop ( nloops = ntests , message = 'Elapsed time (syev): ' ) call t % timer_start () do i = 1 , ntests call eig ( A , eig_vec , eig_val , method = 'geev' ) end do call t % timer_stop ( nloops = ntests , message = 'Elapsed time (geev): ' ) call t % timer_start () do i = 1 , ntests call eig ( A , eig_vec , eig_val , method = 'ggev' ) end do call t % timer_stop ( nloops = ntests , message = 'Elapsed time (ggev): ' ) deallocate ( A , eig_vec , eig_val ) end program benchmark","tags":"","loc":"program/benchmark.html"},{"title":"test1 – foreig","text":"Uses foreig kinds program~~test1~~UsesGraph program~test1 test1 kinds kinds program~test1->kinds module~foreig foreig program~test1->module~foreig module~foreig->kinds Help × Graph Key Nodes of different colours represent the following: Graph Key Module Module Submodule Submodule Subroutine Subroutine Function Function Program Program This Page's Entity This Page's Entity Solid arrows point from a submodule to the (sub)module which it is\ndescended from. Dashed arrows point from a module or program unit to \nmodules which it uses. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Calls program~~test1~~CallsGraph program~test1 test1 interface~eig foreig::eig program~test1->interface~eig proc~eig_rel foreig::eig_rel interface~eig->proc~eig_rel proc~dgeev_rel foreig::dgeev_rel proc~eig_rel->proc~dgeev_rel proc~dggev_rel foreig::dggev_rel proc~eig_rel->proc~dggev_rel proc~dsyev_rel foreig::dsyev_rel proc~eig_rel->proc~dsyev_rel Help × Graph Key Nodes of different colours represent the following: Graph Key Subroutine Subroutine Function Function Interface Interface Type Bound Procedure Type Bound Procedure Unknown Procedure Type Unknown Procedure Type Program Program This Page's Entity This Page's Entity Solid arrows point from a procedure to one which it calls. Dashed \narrows point from an interface to procedures which implement that interface.\nThis could include the module procedures in a generic interface or the\nimplementation in a submodule of an interface in a parent module. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Variables Type Attributes Name Initial real(kind=rk), dimension(:,:), allocatable :: A real(kind=rk), dimension(:,:), allocatable :: eig_vec real(kind=rk), dimension(:), allocatable :: eig_val Source Code program test1 use kinds use foreig , only : eig implicit none real ( rk ), dimension (:,:), allocatable :: A real ( rk ), dimension (:,:), allocatable :: eig_vec real ( rk ), dimension (:), allocatable :: eig_val allocate ( A ( 5 , 5 ), eig_vec ( 5 , 5 ), eig_val ( 5 )) call random_number ( A ) call eig ( A , eig_vec , eig_val ) print * , eig_vec print * , eig_val deallocate ( A , eig_vec , eig_val ) end program test1","tags":"","loc":"program/test1.html"},{"title":"foreig.f90 – foreig","text":"Files dependent on this one sourcefile~~foreig.f90~~AfferentGraph sourcefile~foreig.f90 foreig.f90 sourcefile~benchmark.f90 benchmark.f90 sourcefile~benchmark.f90->sourcefile~foreig.f90 sourcefile~test1.f90 test1.f90 sourcefile~test1.f90->sourcefile~foreig.f90 Help × Graph Key Nodes of different colours represent the following: Graph Key Source File Source File This Page's Entity This Page's Entity Solid arrows point from a file to a file which it depends on. A file\nis dependent upon another if the latter must be compiled before the former\ncan be. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Source Code module foreig use kinds implicit none private public :: eig !=============================================================================== interface eig procedure :: eig_rel end interface !=============================================================================== contains !=============================================================================== !> author: Seyed Ali Ghasemi pure subroutine eig_rel ( matrix , eig_vecr , eig_val , eig_vecl , method ) real ( rk ), dimension (:,:), intent ( in ) :: matrix character ( * ), intent ( in ), optional :: method real ( rk ), dimension (:,:), allocatable , intent ( out ) :: eig_vecr real ( rk ), dimension (:,:), allocatable , intent ( out ), optional :: eig_vecl real ( rk ), dimension (:), allocatable , intent ( out ) :: eig_val if (. not . present ( method )) then call dgeev_rel ( matrix , eig_vecr , eig_val ) else select case ( method ) case ( 'syev' ) call dsyev_rel ( matrix , eig_vecr , eig_val ) case ( 'geev' ) call dgeev_rel ( matrix , eig_vecr , eig_val , eig_vecl ) case ( 'ggev' ) call dggev_rel ( matrix , eig_vecr , eig_val , eig_vecl ) end select end if end subroutine eig_rel !=============================================================================== !=============================================================================== !> author: Seyed Ali Ghasemi pure subroutine dsyev_rel ( matrix , eig_vecr , eig_val ) real ( rk ), dimension (:,:), intent ( in ) :: matrix real ( rk ), dimension (:,:), allocatable , intent ( out ) :: eig_vecr real ( rk ), dimension (:), allocatable , intent ( out ) :: eig_val real ( rk ), dimension (:), allocatable :: work real ( rk ), dimension ( size ( matrix , 1 ), size ( matrix , 1 )) :: A integer :: lwork , m , info real ( rk ) :: work1 ( 1 ) interface pure subroutine dsyev ( fjobz , fuplo , fn , fA , flda , fw , fwork , flwork , finfo ) import rk character , intent ( in ) :: fjobz , fuplo integer , intent ( in ) :: fn integer , intent ( in ) :: flda integer , intent ( in ) :: flwork integer , intent ( out ) :: finfo real ( rk ), intent ( inout ) :: fA ( flda , * ) real ( rk ), intent ( out ) :: fw ( * ) real ( rk ), intent ( out ) :: fwork ( * ) end subroutine end interface m = size ( matrix , 1 ) A = matrix call dsyev ( 'V' , 'U' , m , A , m , eig_val , work1 , - 1 , info ) lwork = nint ( work1 ( 1 )) allocate ( work ( lwork )) allocate ( eig_vecr ( m , m ), eig_val ( m )) call dsyev ( 'V' , 'U' , m , A , m , eig_val , work , lwork , info ) eig_vecr = A deallocate ( work ) end subroutine dsyev_rel !=============================================================================== !=============================================================================== !> author: Seyed Ali Ghasemi pure subroutine dgeev_rel ( matrix , eig_vecr , eig_val , eig_vecl ) real ( rk ), dimension (:,:), intent ( in ) :: matrix real ( rk ), dimension (:), allocatable , intent ( out ) :: eig_val real ( rk ), dimension (:,:), allocatable , intent ( out ) :: eig_vecr real ( rk ), dimension (:,:), allocatable , intent ( out ), optional :: eig_vecl real ( rk ), dimension ( size ( matrix , 1 )) :: wr , wi real ( rk ), dimension ( size ( matrix , 1 ), size ( matrix , 1 )) :: vl , vr real ( rk ), dimension ( size ( matrix , 1 ), size ( matrix , 1 )) :: A real ( rk ), dimension (:), allocatable :: work integer :: m , lwork , info real ( rk ) :: work1 ( 1 ) interface pure subroutine dgeev ( fjobvl , fjobvr , fn , fA , flda , fwr , fwi , fvl , fldvl , fvr , fldvr , fwork , flwork , finfo ) import :: rk character , intent ( in ) :: fjobvl , fjobvr integer , intent ( in ) :: fn , flda , fldvl , fldvr , flwork , finfo real ( rk ), intent ( inout ) :: fA ( flda , * ) real ( rk ), intent ( out ) :: fwr ( fn ), fwi ( fn ), fvl ( fldvl , * ), fvr ( fldvr , * ), fwork ( flwork ) end subroutine end interface m = size ( matrix , 1 ) A = matrix if ( present ( eig_vecl )) then eig_vecl = matrix call dgeev ( 'V' , 'V' , m , A , m , wr , wi , vl , m , vr , m , work1 , - 1 , info ) else call dgeev ( 'N' , 'V' , m , A , m , wr , wi , vl , m , vr , m , work1 , - 1 , info ) end if lwork = nint ( work1 ( 1 )) allocate ( work ( lwork )) if ( present ( eig_vecl )) then call dgeev ( 'V' , 'V' , m , A , m , wr , wi , vl , m , vr , m , work , lwork , info ) eig_vecl = vl else call dgeev ( 'N' , 'V' , m , A , m , wr , wi , vl , m , vr , m , work , lwork , info ) end if eig_val = wr eig_vecr = vr deallocate ( work ) end subroutine dgeev_rel !=============================================================================== !=============================================================================== !> author: Seyed Ali Ghasemi pure subroutine dggev_rel ( matrix , eig_vecr , eig_val , eig_vecl ) real ( rk ), dimension (:,:), intent ( in ) :: matrix real ( rk ), dimension (:), allocatable , intent ( out ) :: eig_val real ( rk ), dimension (:,:), allocatable , intent ( out ) :: eig_vecr real ( rk ), dimension (:,:), allocatable , intent ( out ), optional :: eig_vecl real ( rk ), dimension ( size ( matrix , 1 )) :: alphar , alphai , beta real ( rk ), dimension ( size ( matrix , 1 ), size ( matrix , 1 )) :: vl , vr real ( rk ), dimension (:), allocatable :: work integer :: m , lwork , info real ( rk ) :: work1 ( 1 ) real ( rk ), dimension ( size ( matrix , 1 ), size ( matrix , 1 )) :: A interface pure subroutine dggev ( fjobvl , fjobvr , fn , fa , flda , fb , ldb , & falphar , falphai , fbeta , fvl , fldvl , fvr , fldvr , fwork , flwork , finfo ) import :: rk character , intent ( in ) :: fjobvl , fjobvr integer , intent ( in ) :: fn , flda , ldb , fldvl , fldvr , flwork , finfo real ( rk ), intent ( inout ) :: fa ( flda , * ), fb ( ldb , * ) real ( rk ), intent ( out ) :: falphar ( fn ), falphai ( fn ), fbeta ( fn ) real ( rk ), intent ( inout ) :: fvl ( fldvl , * ), fvr ( fldvr , * ) real ( rk ), intent ( out ) :: fwork ( flwork ) end subroutine end interface m = size ( matrix , 1 ) allocate ( eig_val ( m ), eig_vecr ( m , m )) eig_vecr = matrix A = matrix if ( present ( eig_vecl )) then eig_vecl = matrix call dggev ( 'V' , 'V' , m , eig_vecl , m ,& eig_vecr , m , alphar , alphai , beta , vl , m , vr , m , work1 , - 1 , info ) else call dggev ( 'N' , 'V' , m , eig_vecr , m ,& A , m , alphar , alphai , beta , vl , m , vr , m , work1 , - 1 , info ) end if lwork = nint ( work1 ( 1 )) allocate ( work ( lwork )) if ( present ( eig_vecl )) then call dggev ( 'V' , 'V' , m , eig_vecl , m , & eig_vecr , m , alphar , alphai , beta , vl , m , vr , m , work , lwork , info ) else call dggev ( 'N' , 'V' , m , eig_vecr , m ,& A , m , alphar , alphai , beta , vl , m , vr , m , work , lwork , info ) end if eig_val = alphar / beta deallocate ( work ) end subroutine dggev_rel !=============================================================================== end module foreig","tags":"","loc":"sourcefile/foreig.f90.html"},{"title":"benchmark.f90 – foreig","text":"This file depends on sourcefile~~benchmark.f90~~EfferentGraph sourcefile~benchmark.f90 benchmark.f90 sourcefile~foreig.f90 foreig.f90 sourcefile~benchmark.f90->sourcefile~foreig.f90 Help × Graph Key Nodes of different colours represent the following: Graph Key Source File Source File This Page's Entity This Page's Entity Solid arrows point from a file to a file which it depends on. A file\nis dependent upon another if the latter must be compiled before the former\ncan be. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Source Code program benchmark use kinds use foreig , only : eig use fortime , only : timer implicit none real ( rk ), dimension (:,:), allocatable :: A real ( rk ), dimension (:,:), allocatable :: eig_vec real ( rk ), dimension (:), allocatable :: eig_val integer :: m , i , ntests type ( timer ) :: t m = 100 allocate ( A ( m , m ), eig_vec ( m , m ), eig_val ( m )) call random_number ( A ) A = A * 1 0.0_rk ntests = 5 call t % timer_start () do i = 1 , ntests call eig ( A , eig_vec , eig_val , method = 'syev' ) end do call t % timer_stop ( nloops = ntests , message = 'Elapsed time (syev): ' ) call t % timer_start () do i = 1 , ntests call eig ( A , eig_vec , eig_val , method = 'geev' ) end do call t % timer_stop ( nloops = ntests , message = 'Elapsed time (geev): ' ) call t % timer_start () do i = 1 , ntests call eig ( A , eig_vec , eig_val , method = 'ggev' ) end do call t % timer_stop ( nloops = ntests , message = 'Elapsed time (ggev): ' ) deallocate ( A , eig_vec , eig_val ) end program benchmark","tags":"","loc":"sourcefile/benchmark.f90.html"},{"title":"test1.f90 – foreig","text":"This file depends on sourcefile~~test1.f90~~EfferentGraph sourcefile~test1.f90 test1.f90 sourcefile~foreig.f90 foreig.f90 sourcefile~test1.f90->sourcefile~foreig.f90 Help × Graph Key Nodes of different colours represent the following: Graph Key Source File Source File This Page's Entity This Page's Entity Solid arrows point from a file to a file which it depends on. A file\nis dependent upon another if the latter must be compiled before the former\ncan be. Where possible, edges connecting nodes are\ngiven different colours to make them easier to distinguish in\nlarge graphs. Source Code program test1 use kinds use foreig , only : eig implicit none real ( rk ), dimension (:,:), allocatable :: A real ( rk ), dimension (:,:), allocatable :: eig_vec real ( rk ), dimension (:), allocatable :: eig_val allocate ( A ( 5 , 5 ), eig_vec ( 5 , 5 ), eig_val ( 5 )) call random_number ( A ) call eig ( A , eig_vec , eig_val ) print * , eig_vec print * , eig_val deallocate ( A , eig_vec , eig_val ) end program test1","tags":"","loc":"sourcefile/test1.f90.html"}]}