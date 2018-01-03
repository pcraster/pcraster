!************************************************************************
!                     COMMON_TYPES
!************************************************************************

! ... FILE COMMON_TYPES, PCGN SOLVER
! ... Modules for shared information 
! ...
! ... R.L. Naff 
! ...
! ... VERSION 1.0 NOVEMBER 2007
! ... 

module common_parameters
  implicit none
  double precision  ::DBLKIND
  ! ... kv: precision of variables used in assembly
!  integer, parameter :: kv=selected_real_kind(p=10)
  integer, parameter :: kv=kind(DBLKIND)
  ! ... common numbers
  real(kind=kv), parameter :: n0=0.0_kv, n1=1.0_kv, n2=2.0_kv, n3=3.0_kv, &
       n4=4.0_kv, n5=5.0_kv, n6=6.0_kv, n7=7.0_kv, n8=8.0_kv, n9=9.0_kv, &
       n10=10.0_kv, n100=100.0_kv
  ! ... common fractions
  real(kind=kv), parameter :: f2=0.5_kv, f3=n1/n3, f4=0.25_kv, f5=0.2_kv, &
       f6=n1/n6, f7=n1/n7, f8=0.125_kv, f9=n1/n9, f10=0.1_kv
  ! ... machine smallest number
  real(kind=kv), parameter :: machine_epsilon=epsilon(n0)
  real(kind=kv), parameter :: small=n100*machine_epsilon
  real(kind=kv), parameter :: MZ=tiny(n0)
end module common_parameters

module common_solver_types
  use common_parameters
  implicit none
  integer, save :: dim, nx, ny, nz, n_rows
  ! ... arrays
  real(kind=kv), dimension(:), pointer :: DD, DX, DY, DZ
  real(kind=kv), dimension(:), pointer :: dx0, dy0, dz0       
  real(kind=kv), dimension(:), pointer :: dx1, dy1, dz1
  real(kind=kv), dimension(:), pointer :: diag
end module common_solver_types

!************************************************************************
!                     MATRIX-VECTOR MULTIPLY
!************************************************************************

module MAT_VEC_MULT
  ! ... Subroutine for matrix-vector multiply: A*X=rhs
  ! ... 
  ! ... R.L. Naff 
  ! ... version 1.1, 08/2008
  ! ... openmp directives added
  ! ... 
  use common_parameters
  use common_solver_types
  ! ... DD, DX, DY, DZ, dim
  implicit none
  private; public ::  a_multiply
contains

  subroutine a_multiply(X_i, rhs)
    ! ...  Post multiply matrix A by vector B: AxB
    ! ...  
    ! ...  A is in sparse compressed diagonal format
    ! ...  for the upper triangle of a spd matrix.
    ! ...
    ! ...  argument list
    ! ...
    real(kind=kv), intent(in), dimension(:) :: X_i
    real(kind=kv), intent(out), dimension(:) :: rhs
    ! ...  
    ! ...  local variables
    ! ...
    integer :: node, nxy, m
    integer, save :: mod_no=8
    real(kind=kv) :: bnp0, bnp1, bnp2, bnp3, bnp4, bnp5, bnp6, bnp7
    !......................................................................
    select case(dim)
    case(3)
       nxy=nx*ny
       ! ... full 3-D
       ! ... top and bottom layer excluded (k=1,nz)
       m=mod(n_rows-2*nxy, mod_no)
       !$OMP PARALLEL DEFAULT(NONE) &
       !$OMP SHARED(rhs,X_i,DD,DX,DY,DZ) &
       !$OMP PRIVATE(node,bnp0,bnp1,bnp2,bnp3,bnp4,bnp5,bnp6,bnp7) &
       !$OMP FIRSTPRIVATE(m,mod_no,nxy,nx,n_rows)
       if (m>0) then
          !$OMP DO           
          do node=nxy+1, nxy+m
             rhs(node)=X_i(node)*DD(node)+X_i(node-1)*DX(node-1) &
                  + X_i(node-nx)*DY(node-nx)+X_i(node-nxy)*DZ(node-nxy) &
                  + X_i(node+1)*DX(node)+X_i(node+nx)*DY(node) &
                  + X_i(node+nxy)*DZ(node) 
          enddo
          !$OMP END DO
       endif
       if (n_rows-2*nxy>=mod_no) then
          !$OMP DO           
          do node=nxy+m+1, n_rows-nxy, mod_no
             bnp0=X_i(node); bnp1=X_i(node+1); bnp2=X_i(node+2)
             bnp3=X_i(node+3); bnp4=X_i(node+4); bnp5=X_i(node+5)
             bnp6=X_i(node+6); bnp7=X_i(node+7)
             rhs(node)=bnp0*DD(node)+X_i(node-1)*DX(node-1) &
                  + X_i(node-nx)*DY(node-nx)+X_i(node-nxy)*DZ(node-nxy) &
                  + bnp1*DX(node)+X_i(node+nx)*DY(node) &
                  + X_i(node+nxy)*DZ(node) 
             rhs(node+1)=bnp1*DD(node+1)+bnp0*DX(node) &
                  + X_i(node-nx+1)*DY(node-nx+1) &
                  + X_i(node-nxy+1)*DZ(node-nxy+1)+bnp2*DX(node+1) &
                  + X_i(node+nx+1)*DY(node+1)+X_i(node+nxy+1)*DZ(node+1) 
             rhs(node+2)=bnp2*DD(node+2)+bnp1*DX(node+1) &
                  + X_i(node-nx+2)*DY(node-nx+2) &
                  + X_i(node-nxy+2)*DZ(node-nxy+2)+bnp3*DX(node+2) &
                  + X_i(node+nx+2)*DY(node+2)+X_i(node+nxy+2)*DZ(node+2) 
             rhs(node+3)=bnp3*DD(node+3)+bnp2*DX(node+2) &
                  + X_i(node-nx+3)*DY(node-nx+3) &
                  + X_i(node-nxy+3)*DZ(node-nxy+3)+bnp4*DX(node+3) &
                  + X_i(node+nx+3)*DY(node+3)+X_i(node+nxy+3)*DZ(node+3) 
             rhs(node+4)=bnp4*DD(node+4)+bnp3*DX(node+3) &
                  + X_i(node-nx+4)*DY(node-nx+4) &
                  + X_i(node-nxy+4)*DZ(node-nxy+4)+bnp5*DX(node+4) &
                  + X_i(node+nx+4)*DY(node+4)+X_i(node+nxy+4)*DZ(node+4) 
             rhs(node+5)=bnp5*DD(node+5)+bnp4*DX(node+4) &
                  + X_i(node-nx+5)*DY(node-nx+5) &
                  + X_i(node-nxy+5)*DZ(node-nxy+5)+bnp6*DX(node+5) &
                  + X_i(node+nx+5)*DY(node+5)+X_i(node+nxy+5)*DZ(node+5) 
             rhs(node+6)=bnp6*DD(node+6)+bnp5*DX(node+5) &
                  + X_i(node-nx+6)*DY(node-nx+6) &
                  + X_i(node-nxy+6)*DZ(node-nxy+6)+bnp7*DX(node+6) &
                  + X_i(node+nx+6)*DY(node+6)+X_i(node+nxy+6)*DZ(node+6) 
             rhs(node+7)=bnp7*DD(node+7)+bnp6*DX(node+6) &
                  + X_i(node-nx+7)*DY(node-nx+7) &
                  + X_i(node-nxy+7)*DZ(node-nxy+7)+X_i(node+8)*DX(node+7) &
                  + X_i(node+nx+7)*DY(node+7)+X_i(node+nxy+7)*DZ(node+7) 
          enddo
          !$OMP END DO
       endif
       ! ... bottom k layer:  k=1
       ! ... row j=1 excluded
       m=mod(nxy-nx, mod_no)
       if (m>0) then
          !$OMP DO           
          do node=nx+1, nx+m
             rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
                  + X_i(node+nx)*DY(node)+X_i(node+nxy)*DZ(node) &
                  + X_i(node-1)*DX(node-1)+X_i(node-nx)*DY(node-nx)
          enddo
          !$OMP END DO
       endif
       if (nxy-nx>=mod_no) then
          !$OMP DO           
          do node=nx+m+1, nxy, mod_no
             rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
                  + X_i(node+nx)*DY(node)+X_i(node+nxy)*DZ(node) &
                  + X_i(node-1)*DX(node-1)+X_i(node-nx)*DY(node-nx)
             rhs(node+1)=X_i(node+1)*DD(node+1)+X_i(node+2)*DX(node+1) &
                  + X_i(node+nx+1)*DY(node+1)+X_i(node+nxy+1)*DZ(node+1) &
                  + X_i(node)*DX(node)+X_i(node-nx+1)*DY(node-nx+1)
             rhs(node+2)=X_i(node+2)*DD(node+2)+X_i(node+3)*DX(node+2) &
                  + X_i(node+nx+2)*DY(node+2)+X_i(node+nxy+2)*DZ(node+2) &
                  + X_i(node+1)*DX(node+1)+X_i(node-nx+2)*DY(node-nx+2)
             rhs(node+3)=X_i(node+3)*DD(node+3)+X_i(node+4)*DX(node+3) &
                  + X_i(node+nx+3)*DY(node+3)+X_i(node+nxy+3)*DZ(node+3) &
                  + X_i(node+2)*DX(node+2)+X_i(node-nx+3)*DY(node-nx+3)
             rhs(node+4)=X_i(node+4)*DD(node+4)+X_i(node+5)*DX(node+4) &
                  + X_i(node+nx+4)*DY(node+4)+X_i(node+nxy+4)*DZ(node+4) &
                  + X_i(node+3)*DX(node+3)+X_i(node-nx+4)*DY(node-nx+4)
             rhs(node+5)=X_i(node+5)*DD(node+5)+X_i(node+6)*DX(node+5) &
                  + X_i(node+nx+5)*DY(node+5)+X_i(node+nxy+5)*DZ(node+5) &
                  + X_i(node+4)*DX(node+4)+X_i(node-nx+5)*DY(node-nx+5)
             rhs(node+6)=X_i(node+6)*DD(node+6)+X_i(node+7)*DX(node+6) &
                  + X_i(node+nx+6)*DY(node+6)+X_i(node+nxy+6)*DZ(node+6) &
                  + X_i(node+5)*DX(node+5)+X_i(node-nx+6)*DY(node-nx+6)
             rhs(node+7)=X_i(node+7)*DD(node+7)+X_i(node+8)*DX(node+7) &
                  + X_i(node+nx+7)*DY(node+7)+X_i(node+nxy+7)*DZ(node+7) &
                  + X_i(node+6)*DX(node+6)+X_i(node-nx+7)*DY(node-nx+7)
          enddo
          !$OMP END DO
       endif
       ! ... bottom k layer:  k=1, j=1
       ! ... cell i=1 excluded
       !$OMP DO           
       do node=2, nx
          rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
               + X_i(node+nx)*DY(node)+X_i(node+nxy)*DZ(node) &
               + X_i(node-1)*DX(node-1)
       enddo
       !$OMP END DO
       ! ... bottom k layer:  k=1, j=1, i=1
       rhs(1)=X_i(1)*DD(1)+X_i(2)*DX(1)+X_i(nx+1)*DY(1)+X_i(nxy+1)*DZ(1)
       ! ... top k layer: k=nz
       ! ... row j=ny excluded
       if (m>0) then
          !$OMP DO           
          do node=n_rows-nxy+1, n_rows-nxy+m
             rhs(node)=X_i(node)*DD(node)+X_i(node-1)*DX(node-1) &
                  + X_i(node-nx)*DY(node-nx)+X_i(node-nxy)*DZ(node-nxy) &
                  + X_i(node+1)*DX(node)+X_i(node+nx)*DY(node)
          enddo
          !$OMP END DO
       endif
       if (nxy-nx>=mod_no) then
          !$OMP DO           
          do node=n_rows-nxy+m+1, n_rows-nx, mod_no
             rhs(node)=X_i(node)*DD(node)+X_i(node-1)*DX(node-1) &
                  + X_i(node-nx)*DY(node-nx)+X_i(node-nxy)*DZ(node-nxy) &
                  + X_i(node+1)*DX(node)+X_i(node+nx)*DY(node)
             rhs(node+1)=X_i(node+1)*DD(node+1)+X_i(node)*DX(node) &
                  + X_i(node-nx+1)*DY(node-nx+1) &
                  + X_i(node-nxy+1)*DZ(node-nxy+1) &
                  + X_i(node+2)*DX(node+1)+X_i(node+nx+1)*DY(node+1)
             rhs(node+2)=X_i(node+2)*DD(node+2)+X_i(node+1)*DX(node+1) &
                  + X_i(node-nx+2)*DY(node-nx+2) &
                  + X_i(node-nxy+2)*DZ(node-nxy+2) &
                  + X_i(node+3)*DX(node+2)+X_i(node+nx+2)*DY(node+2)
             rhs(node+3)=X_i(node+3)*DD(node+3)+X_i(node+2)*DX(node+2) &
                  + X_i(node-nx+3)*DY(node-nx+3) &
                  + X_i(node-nxy+3)*DZ(node-nxy+3) &
                  + X_i(node+4)*DX(node+3)+X_i(node+nx+3)*DY(node+3)
             rhs(node+4)=X_i(node+4)*DD(node+4)+X_i(node+3)*DX(node+3) &
                  + X_i(node-nx+4)*DY(node-nx+4) &
                  + X_i(node-nxy+4)*DZ(node-nxy+4) &
                  + X_i(node+5)*DX(node+4)+X_i(node+nx+4)*DY(node+4)
             rhs(node+5)=X_i(node+5)*DD(node+5)+X_i(node+4)*DX(node+4) &
                  + X_i(node-nx+5)*DY(node-nx+5) &
                  + X_i(node-nxy+5)*DZ(node-nxy+5) &
                  + X_i(node+6)*DX(node+5)+X_i(node+nx+5)*DY(node+5)
             rhs(node+6)=X_i(node+6)*DD(node+6)+X_i(node+5)*DX(node+5) &
                  + X_i(node-nx+6)*DY(node-nx+6) &
                  + X_i(node-nxy+6)*DZ(node-nxy+6) &
                  + X_i(node+7)*DX(node+6)+X_i(node+nx+6)*DY(node+6)
             rhs(node+7)=X_i(node+7)*DD(node+7)+X_i(node+6)*DX(node+6) &
                  + X_i(node-nx+7)*DY(node-nx+7) &
                  + X_i(node-nxy+7)*DZ(node-nxy+7) &
                  + X_i(node+8)*DX(node+7)+X_i(node+nx+7)*DY(node+7)
          enddo
          !$OMP END DO
       endif
       ! ... top k layer: k=nz, j=ny
       ! ... cell i=nx excluded
       !$OMP DO           
       do node=n_rows-nx+1, n_rows-1
          rhs(node)=X_i(node)*DD(node)+X_i(node-1)*DX(node-1) &
               + X_i(node-nx)*DY(node-nx)+X_i(node-nxy)*DZ(node-nxy) &
               + X_i(node+1)*DX(node)
       enddo
       !$OMP END DO
       !$OMP END PARALLEL
       ! ... top k layer: k=nz, j=ny, i=nx
       rhs(n_rows)=X_i(n_rows)*DD(n_rows)+X_i(n_rows-1)*DX(n_rows-1) &
            + X_i(n_rows-nx)*DY(n_rows-nx)+X_i(n_rows-nxy)*DZ(n_rows-nxy)
    case(2)
       ! ... 2-D problem (based on nx>1, ny>1)
       ! ... exclude rows: j=1,ny
       m=mod(n_rows-2*nx, mod_no)
       !$OMP PARALLEL DEFAULT(NONE) &
       !$OMP SHARED(rhs,X_i,DD,DX,DY) PRIVATE(node) &
       !$OMP FIRSTPRIVATE(m,mod_no,nx,n_rows)
       if (m>0) then
          !$OMP DO           
          do node=nx+1, nx+m
             rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
                  + X_i(node+nx)*DY(node)+X_i(node-1)*DX(node-1) &
                  + X_i(node-nx)*DY(node-nx)
          enddo
          !$OMP END DO
       endif
       if (n_rows-2*nx>=mod_no) then
          !$OMP DO           
          do node=nx+m+1, n_rows-nx, mod_no
             rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
                  + X_i(node+nx)*DY(node)+X_i(node-1)*DX(node-1) &
                  + X_i(node-nx)*DY(node-nx)
             rhs(node+1)=X_i(node+1)*DD(node+1)+X_i(node+2)*DX(node+1) &
                  + X_i(node+nx+1)*DY(node+1)+X_i(node)*DX(node) &
                  + X_i(node-nx+1)*DY(node-nx+1)
             rhs(node+2)=X_i(node+2)*DD(node+2)+X_i(node+3)*DX(node+2) &
                  + X_i(node+nx+2)*DY(node+2)+X_i(node+1)*DX(node+1) &
                  + X_i(node-nx+2)*DY(node-nx+2)
             rhs(node+3)=X_i(node+3)*DD(node+3)+X_i(node+4)*DX(node+3) &
                  + X_i(node+nx+3)*DY(node+3)+X_i(node+2)*DX(node+2) &
                  + X_i(node-nx+3)*DY(node-nx+3)
             rhs(node+4)=X_i(node+4)*DD(node+4)+X_i(node+5)*DX(node+4) &
                  + X_i(node+nx+4)*DY(node+4)+X_i(node+3)*DX(node+3) &
                  + X_i(node-nx+4)*DY(node-nx+4)
             rhs(node+5)=X_i(node+5)*DD(node+5)+X_i(node+6)*DX(node+5) &
                  + X_i(node+nx+5)*DY(node+5)+X_i(node+4)*DX(node+4) &
                  + X_i(node-nx+5)*DY(node-nx+5)
             rhs(node+6)=X_i(node+6)*DD(node+6)+X_i(node+7)*DX(node+6) &
                  + X_i(node+nx+6)*DY(node+6)+X_i(node+5)*DX(node+5) &
                  + X_i(node-nx+6)*DY(node-nx+6)
             rhs(node+7)=X_i(node+7)*DD(node+7)+X_i(node+8)*DX(node+7) &
                  + X_i(node+nx+7)*DY(node+7)+X_i(node+6)*DX(node+6) &
                  + X_i(node-nx+7)*DY(node-nx+7)
          enddo
          !$OMP END DO
       endif
       ! ... j=1
       ! ... cell i=1 excluded
       !$OMP DO           
       do node=2, nx
          rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
               + X_i(node+nx)*DY(node)+X_i(node-1)*DX(node-1)
       enddo
       !$OMP END DO
       ! ... j=1, i=1 
       rhs(1)=X_i(1)*DD(1)+X_i(2)*DX(1)+X_i(1+nx)*DY(1)
       ! ... j=ny
       ! ... cell i=nx excluded
       !$OMP DO           
       do node=n_rows-nx+1, n_rows-1
          rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
               + X_i(node-1)*DX(node-1)+X_i(node-nx)*DY(node-nx)
       enddo
       !$OMP END DO
       !$OMP END PARALLEL
       ! ... j=ny, i=nx
       rhs(n_rows)=X_i(n_rows)*DD(n_rows)+X_i(n_rows-1)*DX(n_rows-1) &
            + X_i(n_rows-nx)*DY(n_rows-nx)
    case(1)
       ! ... 1-D problem (based on nx>1)
       ! ... exclude i=1, i=nx
       do node=2, nx-1
          rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
               + X_i(node-1)*DX(node-1)
       enddo
       ! ... i=1
       rhs(1)=X_i(1)*DD(1)+X_i(2)*DX(1)
       ! ... i=nx
       rhs(nx)=X_i(nx)*DD(nx)+X_i(nx-1)*DX(nx-1)
    end select
    ! ... 
  end subroutine a_multiply

end module MAT_VEC_MULT

!************************************************************************
!             MODIFIED INCOMPLETE CHOLESKY ALGORITHM
!************************************************************************

module MiUDU
  ! ... MODIFIED INCOMPLETE CHOLESKY (MIC) ALGORITHM.
  ! ... last modified: R. L. Naff, July 2007
  ! ... 
  ! ... Let M be the MIC approximation of A: then module contains
  ! ... subroutines to solve M*x=rhs.
  ! ... 
  ! ... non-zero errors:
  ! ...  error>0: nodal location of zero pivot returned.
  ! ...  error<0: negative of nodal location of negative pivot returned.
  ! ... 
  ! ... R.L. Naff 
  ! ... version 1.0, 11/2007
  ! ...
  use common_parameters
  use common_solver_types
  ! ... DD, DX, DY, DZ, dim, diag, dx0, dy0, dz0, dx1, dy1, dz1 
  implicit none
  real(kind=kv), dimension(:), pointer :: XX
  private; public :: diag_factor_0, diag_factor_1, iUDU_solve_0, &
       iUDU_solve_1, XX
contains

  !********************************************************************
  ! PRECONDITIONER FUNCTIONS
  !********************************************************************

  function iUDU_solve_0()
    ! ...
    ! ... Incomplete Cholesky inversion with zero fill
    ! ...
    ! ... argument list
    ! ...
    integer :: iUDU_solve_0
    ! ...
    ! ...................................................................
    iUDU_solve_0=0
    call U_tranpose_solve_0(XX)
    call U_solve_0(XX)
    iUDU_solve_0=1
  end function iUDU_solve_0
  
  function iUDU_solve_1()
    ! ...
    ! ... Incomplete Cholesky inversion with fill level 0ne
    ! ...
    ! ... argument list
    ! ...
    integer :: iUDU_solve_1
    ! ...
    ! ...................................................................
    iUDU_solve_1=0
    call U_tranpose_solve_1(XX)
    call U_solve_1(XX)
    iUDU_solve_1=1
  end function iUDU_solve_1

  !********************************************************************
  ! SUBROUTINES FOR FILL LEVEL ZERO
  !********************************************************************

  subroutine diag_factor_0(omega,error)
    ! ... MiUDU(0), modified incomplete UDU factorization with no fill.
    ! ... Diagonal factors for symmetric, incomplete u^tdu factorization.
    ! ... omega: relaxation factor for modified iUDU.
    ! ... 3-D algorithm for 7 point stencil.
    ! ... Numbering order: x before y before z.
    ! ... 2-D and 1-D cases, for above numbering order, follow automatically.
    ! ...
    ! ... argument list
    ! ...
    real(kind=kv), intent(in) :: omega
    integer, intent(inout)  :: error
    ! ...
    ! ... local variables
    ! ...
    integer :: node, nxy
    ! ...
    ! ...................................................................
    ! ...
    ! ... find diagonal entries
    ! ...
    error=0
    nxy=nx*ny
    diag(1)=DD(1)
    ! ... 
    select case(dim)
    case(3)
       ! ... 3D
       ! ... k=1, j=1
       ! ... cell i=1 excluded
       do node=2, nx
          diag(node)=DD(node)-DX(node-1)*(DX(node-1) &
               + omega*(DY(node-1)+DZ(node-1)))/diag(node-1)
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
       ! ... k=1
       ! ... row j=1 excluded
       do node=nx+1, nxy
          diag(node)=DD(node)-(DX(node-1)*(DX(node-1) &
               + omega*(DY(node-1)+DZ(node-1)))/diag(node-1) &
               + DY(node-nx)*(DY(node-nx)+omega*(DX(node-nx) &
               + DZ(node-nx)))/diag(node-nx))
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
       ! ... layer k=1 excluded
       do node=nxy+1, n_rows
          diag(node)=DD(node)-(DX(node-1)*(DX(node-1) &
               + omega*(DY(node-1)+DZ(node-1)))/diag(node-1) &
               + DY(node-nx)*(DY(node-nx)+omega*(DX(node-nx) &
               + DZ(node-nx)))/diag(node-nx) &
               + DZ(node-nxy)*(DZ(node-nxy)+omega*(DX(node-nxy) &
               + DY(node-nxy)))/diag(node-nxy))
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
    case(2)
       ! ... 2D, based on nx>1, ny>1
       ! ... cell i=1 excluded
       do node=2, nx
          diag(node)=DD(node)-DX(node-1)*(DX(node-1) &
               + omega*DY(node-1))/diag(node-1)
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
       ! ... row j=1 excluded
       do node=nx+1, nxy
          diag(node)=DD(node)-(DX(node-1)*(DX(node-1) &
               + omega*DY(node-1))/diag(node-1) &
               + DY(node-nx)*(DY(node-nx) &
               + omega*DX(node-nx))/diag(node-nx))
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
    case(1)
       ! ... 1D, based on nx>1
       ! ... cell i=1 excluded
       do node=2, nx
          diag(node)=DD(node)-DX(node-1)**2/diag(node-1)
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
    end select
    ! ...
  end subroutine diag_factor_0

  subroutine U_tranpose_solve_0(rhs)
    ! ... MiUDU(0), modified incomplete UDU factorization with no fill.
    ! ... Invert transposed upper against rhs using elimination.
    ! ... Approximates Y in U^T*D*Y=RHS where Y is U*X.
    ! ... 3-D algorithm for 7 point stencil.
    ! ... This subroutine modified to include diagonal D.
    ! ... Numbering order: x before y before z. 
    ! ... 2-D and 1-D cases, for above numbering order, follow automatically.
    ! ...
    ! ... argument list
    ! ...
    real(kind=kv), dimension(:), intent(inout) :: rhs
    ! ...
    ! ... local variables
    ! ...
    integer :: node, nxy, m
    integer, save :: mod_no=8
    ! ...
    ! ....................................................................
    ! ...
    ! ... columns begin with first entry above diagonal
    ! ...
    rhs(1)=rhs(1)/diag(1)
    ! ... 
    ! ... layer k=1, row j=1
    do node=2, nx
       rhs(node)=(rhs(node)-rhs(node-1)*DX(node-1))/diag(node) 
    enddo
    ! ... layer k=1
    ! ... row j=1 excluded
    nxy=nx*ny
    m=mod(nxy-nx, mod_no)
    if (m>0) then
       do node=nx+1, nx+m
          rhs(node)=(rhs(node)-rhs(node-1)*DX(node-1) &
               - rhs(node-nx)*DY(node-nx))/diag(node)
       enddo
    endif
    if (nxy-nx>=mod_no) then
       do node=nx+m+1, nxy, mod_no
          rhs(node)=(rhs(node)-rhs(node-1)*DX(node-1) &
               - rhs(node-nx)*DY(node-nx))/diag(node)
          rhs(node+1)=(rhs(node+1)-rhs(node)*DX(node) &
               - rhs(node-nx+1)*DY(node-nx+1))/diag(node+1)
          rhs(node+2)=(rhs(node+2)-rhs(node+1)*DX(node+1) &
               - rhs(node-nx+2)*DY(node-nx+2))/diag(node+2)
          rhs(node+3)=(rhs(node+3)-rhs(node+2)*DX(node+2) &
               - rhs(node-nx+3)*DY(node-nx+3))/diag(node+3)
          rhs(node+4)=(rhs(node+4)-rhs(node+3)*DX(node+3) &
               - rhs(node-nx+4)*DY(node-nx+4))/diag(node+4)
          rhs(node+5)=(rhs(node+5)-rhs(node+4)*DX(node+4) &
               - rhs(node-nx+5)*DY(node-nx+5))/diag(node+5)
          rhs(node+6)=(rhs(node+6)-rhs(node+5)*DX(node+5) &
               - rhs(node-nx+6)*DY(node-nx+6))/diag(node+6)
          rhs(node+7)=(rhs(node+7)-rhs(node+6)*DX(node+6) &
               - rhs(node-nx+7)*DY(node-nx+7))/diag(node+7)
       enddo
    endif
    ! ... layer k=1 excluded
    m=mod(n_rows-nxy, mod_no)
    if (m>0) then
       do node=nxy+1, nxy+m
          rhs(node)=(rhs(node)-rhs(node-1)*DX(node-1) &
               - rhs(node-nx)*DY(node-nx) &
               - rhs(node-nxy)*DZ(node-nxy))/diag(node)
       enddo
    endif
    if (n_rows-nxy>=mod_no) then
       do node=nxy+m+1, n_rows, mod_no
          rhs(node)=(rhs(node)-rhs(node-1)*DX(node-1) &
               - rhs(node-nx)*DY(node-nx) &
               - rhs(node-nxy)*DZ(node-nxy))/diag(node)
          rhs(node+1)=(rhs(node+1)-rhs(node)*DX(node) &
               - rhs(node-nx+1)*DY(node-nx+1) &
               - rhs(node-nxy+1)*DZ(node-nxy+1))/diag(node+1)
          rhs(node+2)=(rhs(node+2)-rhs(node+1)*DX(node+1) &
               - rhs(node-nx+2)*DY(node-nx+2) &
               - rhs(node-nxy+2)*DZ(node-nxy+2))/diag(node+2)
          rhs(node+3)=(rhs(node+3)-rhs(node+2)*DX(node+2) &
               - rhs(node-nx+3)*DY(node-nx+3) &
               - rhs(node-nxy+3)*DZ(node-nxy+3))/diag(node+3)
          rhs(node+4)=(rhs(node+4)-rhs(node+3)*DX(node+3) &
               - rhs(node-nx+4)*DY(node-nx+4) &
               - rhs(node-nxy+4)*DZ(node-nxy+4))/diag(node+4)
          rhs(node+5)=(rhs(node+5)-rhs(node+4)*DX(node+4) &
               - rhs(node-nx+5)*DY(node-nx+5) &
               - rhs(node-nxy+5)*DZ(node-nxy+5))/diag(node+5)
          rhs(node+6)=(rhs(node+6)-rhs(node+5)*DX(node+5) &
               - rhs(node-nx+6)*DY(node-nx+6) &
               - rhs(node-nxy+6)*DZ(node-nxy+6))/diag(node+6)
          rhs(node+7)=(rhs(node+7)-rhs(node+6)*DX(node+6) &
               - rhs(node-nx+7)*DY(node-nx+7) &
               - rhs(node-nxy+7)*DZ(node-nxy+7))/diag(node+7)
       enddo
    endif
    ! ... 
  end subroutine U_tranpose_solve_0

  subroutine U_solve_0(rhs)
    ! ... MiUDU(0), modified incomplete UDU factorization with no fill.
    ! ... Invert upper against rhs using elimination.
    ! ... Approximates X in U*X=RHS.
    ! ... 3-D algorithm for 7 point stencil.
    ! ... Numbering order: x before y before z.
    ! ... 2-D and 1-D cases, for above numbering order, follow automatically.
    ! ...
    ! ... argument list
    ! ...
    real(kind=kv), dimension(:), intent(inout) :: rhs
    ! ...
    ! ... local variables
    ! ...
    integer :: node, nxy, m
    integer, save :: mod_no=8
    ! ... 
    ! ....................................................................
    ! ...
    ! ... reverse order for accessing columns
    ! ...
    ! ... layer k=nz, row j=ny
    ! ... cell i=nx excluded
    do node=n_rows-1, n_rows-nx+1, -1
       rhs(node)=rhs(node)-rhs(node+1)*DX(node)/diag(node)
    enddo
    ! ... layer k=nz
    ! ... row j=ny excluded
    nxy=nx*ny
    m=mod(nxy-nx, mod_no)
    if (m>0) then
       do node=n_rows-nx, n_rows-nx-m+1, -1
          rhs(node)=rhs(node)-(rhs(node+1)*DX(node) &
               + rhs(node+nx)*DY(node))/diag(node)
       enddo
    endif
    if (nxy-nx>=mod_no) then
       do node=n_rows-nx-m, n_rows-nxy+1, -mod_no
          rhs(node)=rhs(node)-(rhs(node+1)*DX(node) &
               + rhs(node+nx)*DY(node))/diag(node)
          rhs(node-1)=rhs(node-1)-(rhs(node)*DX(node-1) &
               + rhs(node+nx-1)*DY(node-1))/diag(node-1)
          rhs(node-2)=rhs(node-2)-(rhs(node-1)*DX(node-2) &
               + rhs(node+nx-2)*DY(node-2))/diag(node-2)
          rhs(node-3)=rhs(node-3)-(rhs(node-2)*DX(node-3) &
               + rhs(node+nx-3)*DY(node-3))/diag(node-3)
          rhs(node-4)=rhs(node-4)-(rhs(node-3)*DX(node-4) &
               + rhs(node+nx-4)*DY(node-4))/diag(node-4)
          rhs(node-5)=rhs(node-5)-(rhs(node-4)*DX(node-5) &
               + rhs(node+nx-5)*DY(node-5))/diag(node-5)
          rhs(node-6)=rhs(node-6)-(rhs(node-5)*DX(node-6) &
               + rhs(node+nx-6)*DY(node-6))/diag(node-6)
          rhs(node-7)=rhs(node-7)-(rhs(node-6)*DX(node-7) &
               + rhs(node+nx-7)*DY(node-7))/diag(node-7)
       enddo
    endif
    ! ... layer k=nz excluded
    m=mod(n_rows-nxy, mod_no)
    if (m>0) then
       do node=n_rows-nxy, n_rows-nxy-m+1, -1
          rhs(node)=rhs(node)-(rhs(node+1)*DX(node) &
               + rhs(node+nx)*DY(node) &
               + rhs(node+nxy)*DZ(node))/diag(node)
       enddo
    endif
    if (n_rows-nxy>=mod_no) then
       do node=n_rows-nxy-m, 1, -mod_no
          rhs(node)=rhs(node)-(rhs(node+1)*DX(node) &
               + rhs(node+nx)*DY(node) &
               + rhs(node+nxy)*DZ(node))/diag(node)
          rhs(node-1)=rhs(node-1)-(rhs(node)*DX(node-1) &
               + rhs(node+nx-1)*DY(node-1) &
               + rhs(node+nxy-1)*DZ(node-1))/diag(node-1)
          rhs(node-2)=rhs(node-2)-(rhs(node-1)*DX(node-2) &
               + rhs(node+nx-2)*DY(node-2) &
               + rhs(node+nxy-2)*DZ(node-2))/diag(node-2)
          rhs(node-3)=rhs(node-3)-(rhs(node-2)*DX(node-3) &
               + rhs(node+nx-3)*DY(node-3) &
               + rhs(node+nxy-3)*DZ(node-3))/diag(node-3)
          rhs(node-4)=rhs(node-4)-(rhs(node-3)*DX(node-4) &
               + rhs(node+nx-4)*DY(node-4) &
               + rhs(node+nxy-4)*DZ(node-4))/diag(node-4)
          rhs(node-5)=rhs(node-5)-(rhs(node-4)*DX(node-5) &
               + rhs(node+nx-5)*DY(node-5) &
               + rhs(node+nxy-5)*DZ(node-5))/diag(node-5)
          rhs(node-6)=rhs(node-6)-(rhs(node-5)*DX(node-6) &
               + rhs(node+nx-6)*DY(node-6) &
               + rhs(node+nxy-6)*DZ(node-6))/diag(node-6)
          rhs(node-7)=rhs(node-7)-(rhs(node-6)*DX(node-7) &
               + rhs(node+nx-7)*DY(node-7) &
               + rhs(node+nxy-7)*DZ(node-7))/diag(node-7)
       enddo
    endif
    ! ... 
  end subroutine U_solve_0

  !********************************************************************
  ! SUBROUTINES FOR FILL LEVEL ONE
  !********************************************************************

  subroutine diag_factor_1(omega,error)
    ! ... MiUDU(1), modified incomplete UDU factorization with fill level one.
    ! ... omega: relaxation factor for modified iUDU.
    ! ... Diagonal and off diagonal factors for incomplete factorization.
    ! ... 3-D algorithm for 7 point stencil.
    ! ... Numbering order: x before y before z. 
    ! ... 2-D and 1-D cases, for above numbering order, follow automatically.
    ! ...
    ! ... The MiUDU(1) does not always produce positive values for the diag
    ! ... array; for poorly conditioned problems, elements of diag may be 
    ! ... negative..
    ! ...
    ! ... argument list
    ! ...
    real(kind=kv), intent(in) :: omega
    integer, intent(inout) :: error
    ! ...
    ! ... local variables
    ! ...
    integer :: node, nxy
    ! ...
    ! ...................................................................
    ! ...
    ! ... find scaled factors
    ! ...
    ! ... k=1, j=1, i=1
    error=0
    diag(1)=DD(1)
    select case(dim)
    case(3)
       ! ... DZ0=>DZ
       DX0(1)=DX(1); DY0(1)=DY(1)
       nxy=nx*ny
       ! ... k=1, j=1
       ! ... cell i=1 excluded
       do node=2, nx
          diag(node)=DD(node)-DX0(node-1)*(DX0(node-1) &
               +omega*(DX1(node-1)+DZ1(node-1)))/diag(node-1)
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
          DX0(node)=DX(node)
          DY0(node)=DY(node)
          DX1(node)=-DX0(node-1)*DY0(node-1)/diag(node-1)
          DZ1(node)=-DX0(node-1)*DZ0(node-1)/diag(node-1)
       enddo
       ! ... k=1
       ! ... row j=1 excluded
       do node=nx+1, nxy
          diag(node)=DD(node)-DX0(node-1)*(DX0(node-1) &
               +omega*(DX1(node-1)+DY1(node-1)+DZ1(node-1)))/diag(node-1) &
               -DX1(node-nx+1)*(DX1(node-nx+1) &
               +omega*(DX0(node-nx+1)+DZ0(node-nx+1)))/diag(node-nx+1) &
               -DY0(node-nx)*(DY0(node-nx) & 
               +omega*(DY1(node-nx)+DZ1(node-nx)))/diag(node-nx) !B
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
          DX0(node)=DX(node) &
               -DX1(node-nx+1)*DY0(node-nx+1)/diag(node-nx+1) 
          DY0(node)=DY(node)
          DX1(node)=-DX0(node-1)*DY0(node-1)/diag(node-1)
          DY1(node)=-DY0(node-nx)*DZ0(node-nx)/diag(node-nx) &
               -DX1(node-nx+1)*DZ1(node-nx+1)/diag(node-nx+1)
          DZ1(node)=-DX0(node-1)*DZ0(node-1)/diag(node-1)
       enddo
       ! ... layer k=1 excluded
       do node=nxy+1, n_rows
          diag(node)=DD(node)-DX0(node-1)*(DX0(node-1) &
               +omega*(DX1(node-1)+DY1(node-1)+DZ1(node-1)))/diag(node-1) &
               -DX1(node-nx+1)*(DX1(node-nx+1) &
                +omega*(DX0(node-nx+1)+DZ0(node-nx+1)))/diag(node-nx+1) &
               -DY0(node-nx)*(DY0(node-nx) & 
               +omega*(DY1(node-nx)+DZ1(node-nx)))/diag(node-nx) & !B
               -DY1(node-nxy+nx)*(DY1(node-nxy+nx) &
               +omega*(DX0(node-nxy+nx)+DY0(node-nxy+nx)))/diag(node-nxy+nx) & 
               -DZ1(node-nxy+1)*(DZ1(node-nxy+1) &
               +omega*(DX0(node-nxy+1)+DY0(node-nxy+1)))/diag(node-nxy+1) & !B
               -DZ0(node-nxy)*(DZ0(node-nxy) &
               +omega*(DX1(node-nxy)))/diag(node-nxy) !B
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
          DX0(node)=DX(node) &
               -DX1(node-nx+1)*DY0(node-nx+1)/diag(node-nx+1) &
               -DZ1(node-nxy+1)*DZ0(node-nxy+1)/diag(node-nxy+1) 
          DY0(node)=DY(node) &
               -DY1(node-nxy+nx)*DZ0(node-nxy+nx)/diag(node-nxy+nx)
          DZ0(node)=DZ(node)
          DX1(node)=-DX0(node-1)*DY0(node-1)/diag(node-1) &
               -DY1(node-nxy+nx)*DZ1(node-nxy+nx)/diag(node-nxy+nx)
          DY1(node)=-DY0(node-nx)*DZ0(node-nx)/diag(node-nx) &
               -DX1(node-nx+1)*DZ1(node-nx+1)/diag(node-nx+1)
          DZ1(node)=-DX0(node-1)*DZ0(node-1)/diag(node-1)
       enddo
    case(2)
       DX0(1)=DX(1)
       ! ... cell i=1 excluded
       do node=2, nx
          diag(node)=DD(node)-DX0(node-1)*(DX0(node-1) &
               +omega*DX1(node-1))/diag(node-1)
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
          DX0(node)=DX(node)
          DX1(node)=-DX0(node-1)*DY0(node-1)/diag(node-1)
       enddo
       ! ... row j=1 excluded
       do node=nx+1, n_rows
          diag(node)=DD(node)-DX0(node-1)*(DX0(node-1) &
               + omega*DX1(node-1))/diag(node-1) &
               - DY0(node-nx)**2/diag(node-nx) &
               - DX1(node-nx+1)*(DX1(node-nx+1) &
               + omega*DX0(node-nx+1))/diag(node-nx+1) 
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
          DX0(node)=DX(node) &
               -DX1(node-nx+1)*DY0(node-nx+1)/diag(node-nx+1) 
          DX1(node)=-DX0(node-1)*DY0(node-1)/diag(node-1)
       enddo
    case(1)
       ! ... 1-D, based on nx>1
       ! ... cell i=1 excluded
       do node=2, nx
          diag(node)=DD(node)-DX0(node-1)**2/diag(node-1)
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
    end select
    ! ...
  end subroutine diag_factor_1

  subroutine U_tranpose_solve_1(rhs)
    ! ... MiUDU(1), modified incomplete UDU factorization with fill level one.
    ! ... Invert transposed upper against rhs using elimination.
    ! ... Approximates Y in U^T*D*Y=RHS where Y is U*X.
    ! ... 3-D algorithm for 7 point stencil.
    ! ... This subroutine modified to include diagonal D.
    ! ... Numbering order: x before y before z.
    ! ... 2-D and 1-D cases, for above numbering order, follow automatically.
    ! ...
    ! ... argument list
    ! ...
    real(kind=kv), dimension(:), intent(inout) :: rhs
    ! ...
    ! ... local variables
    ! ...
    integer :: node, nxy, m
    integer, save :: mod_no=8
    ! ...
    ! ....................................................................
    ! ...
    ! ... columns begin with first entry above diagonal
    ! ...
    rhs(1)=rhs(1)/diag(1)
    ! ... 
    ! ... layer k=1, row j=1
    do node=2, nx
       rhs(node)=(rhs(node)-rhs(node-1)*DX0(node-1))/diag(node)
    enddo
    ! ... layer k=1
    ! ... row j=1 excluded
    nxy=nx*ny
    m=mod(nxy-nx, mod_no)
    if (m>0) then
       do node=nx+1, nx+m
          rhs(node)=(rhs(node)-rhs(node-1)*DX0(node-1) &
               - rhs(node-nx+1)*DX1(node-nx+1) &
               - rhs(node-nx)*DY0(node-nx))/diag(node)
       enddo
    endif
    if (nxy-nx>=mod_no) then
       do node=nx+m+1, nxy, mod_no
          rhs(node)=(rhs(node)-rhs(node-1)*DX0(node-1) &
               - rhs(node-nx+1)*DX1(node-nx+1) &
               - rhs(node-nx)*DY0(node-nx))/diag(node)
          rhs(node+1)=(rhs(node+1)-rhs(node)*DX0(node) &
               - rhs(node-nx+2)*DX1(node-nx+2) &
               - rhs(node-nx+1)*DY0(node-nx+1))/diag(node+1)
          rhs(node+2)=(rhs(node+2)-rhs(node+1)*DX0(node+1) &
               - rhs(node-nx+3)*DX1(node-nx+3) &
               - rhs(node-nx+2)*DY0(node-nx+2))/diag(node+2)
          rhs(node+3)=(rhs(node+3)-rhs(node+2)*DX0(node+2) &
               - rhs(node-nx+4)*DX1(node-nx+4) &
               - rhs(node-nx+3)*DY0(node-nx+3))/diag(node+3)
          rhs(node+4)=(rhs(node+4)-rhs(node+3)*DX0(node+3) &
               - rhs(node-nx+5)*DX1(node-nx+5) &
               - rhs(node-nx+4)*DY0(node-nx+4))/diag(node+4)
          rhs(node+5)=(rhs(node+5)-rhs(node+4)*DX0(node+4) &
               - rhs(node-nx+6)*DX1(node-nx+6) &
               - rhs(node-nx+5)*DY0(node-nx+5))/diag(node+5)
          rhs(node+6)=(rhs(node+6)-rhs(node+5)*DX0(node+5) &
               - rhs(node-nx+7)*DX1(node-nx+7) &
               - rhs(node-nx+6)*DY0(node-nx+6))/diag(node+6)
          rhs(node+7)=(rhs(node+7)-rhs(node+6)*DX0(node+6) &
               - rhs(node-nx+8)*DX1(node-nx+8) &
               - rhs(node-nx+7)*DY0(node-nx+7))/diag(node+7)
       enddo
    endif
    ! ... layer k=1 excluded
    m=mod(n_rows-nxy, mod_no)
    if (m>0) then
       do node=nxy+1, nxy+m
          rhs(node)=(rhs(node)-rhs(node-1)*DX0(node-1) &
               - rhs(node-nx+1)*DX1(node-nx+1) &
               - rhs(node-nx)*DY0(node-nx) &
               - rhs(node-nxy+nx)*DY1(node-nxy+nx) &
               - rhs(node-nxy+1)*DZ1(node-nxy+1) &
               - rhs(node-nxy)*DZ0(node-nxy))/diag(node)
       enddo
    endif
    if (n_rows-nxy>=mod_no) then
       do node=nxy+m+1, n_rows, mod_no
          rhs(node)=(rhs(node)-rhs(node-1)*DX0(node-1) &
               - rhs(node-nx+1)*DX1(node-nx+1) &
               - rhs(node-nx)*DY0(node-nx) &
               - rhs(node-nxy+nx)*DY1(node-nxy+nx) &
               - rhs(node-nxy+1)*DZ1(node-nxy+1) &
               - rhs(node-nxy)*DZ0(node-nxy))/diag(node)
          rhs(node+1)=(rhs(node+1)-rhs(node)*DX0(node) &
               - rhs(node-nx+2)*DX1(node-nx+2) &
               - rhs(node-nx+1)*DY0(node-nx+1) &
               - rhs(node-nxy+nx+1)*DY1(node-nxy+nx+1) &
               - rhs(node-nxy+2)*DZ1(node-nxy+2) &
               - rhs(node-nxy+1)*DZ0(node-nxy+1))/diag(node+1)
          rhs(node+2)=(rhs(node+2)-rhs(node+1)*DX0(node+1) &
               - rhs(node-nx+3)*DX1(node-nx+3) &
               - rhs(node-nx+2)*DY0(node-nx+2) &
               - rhs(node-nxy+nx+2)*DY1(node-nxy+nx+2) &
               - rhs(node-nxy+3)*DZ1(node-nxy+3) &
               - rhs(node-nxy+2)*DZ0(node-nxy+2))/diag(node+2)
          rhs(node+3)=(rhs(node+3)-rhs(node+2)*DX0(node+2) &
               - rhs(node-nx+4)*DX1(node-nx+4) &
               - rhs(node-nx+3)*DY0(node-nx+3) &
               - rhs(node-nxy+nx+3)*DY1(node-nxy+nx+3) &
               - rhs(node-nxy+4)*DZ1(node-nxy+4) &
               - rhs(node-nxy+3)*DZ0(node-nxy+3))/diag(node+3)
          rhs(node+4)=(rhs(node+4)-rhs(node+3)*DX0(node+3) &
               - rhs(node-nx+5)*DX1(node-nx+5) &
               - rhs(node-nx+4)*DY0(node-nx+4) &
               - rhs(node-nxy+nx+4)*DY1(node-nxy+nx+4) &
               - rhs(node-nxy+5)*DZ1(node-nxy+5) &
               - rhs(node-nxy+4)*DZ0(node-nxy+4))/diag(node+4)
          rhs(node+5)=(rhs(node+5)-rhs(node+4)*DX0(node+4) &
               - rhs(node-nx+6)*DX1(node-nx+6) &
               - rhs(node-nx+5)*DY0(node-nx+5) &
               - rhs(node-nxy+nx+5)*DY1(node-nxy+nx+5) &
               - rhs(node-nxy+6)*DZ1(node-nxy+6) &
               - rhs(node-nxy+5)*DZ0(node-nxy+5))/diag(node+5)
          rhs(node+6)=(rhs(node+6)-rhs(node+5)*DX0(node+5) &
               - rhs(node-nx+7)*DX1(node-nx+7) &
               - rhs(node-nx+6)*DY0(node-nx+6) &
               - rhs(node-nxy+nx+6)*DY1(node-nxy+nx+6) &
               - rhs(node-nxy+7)*DZ1(node-nxy+7) &
               - rhs(node-nxy+6)*DZ0(node-nxy+6))/diag(node+6)
          rhs(node+7)=(rhs(node+7)-rhs(node+6)*DX0(node+6) &
               - rhs(node-nx+8)*DX1(node-nx+8) &
               - rhs(node-nx+7)*DY0(node-nx+7) &
               - rhs(node-nxy+nx+7)*DY1(node-nxy+nx+7) &
               - rhs(node-nxy+8)*DZ1(node-nxy+8) &
               - rhs(node-nxy+7)*DZ0(node-nxy+7))/diag(node+7)
       enddo
    endif
    ! ... 
  end subroutine U_tranpose_solve_1

  subroutine U_solve_1(rhs)
    ! ... MiUDU(1), modified incomplete UDU factorization with no fill.
    ! ... Invert upper against rhs using elimination.
    ! ... Approximates X in U*X=RHS.
    ! ... 3-D algorithm for 7 point stencil.
    ! ... Numbering order: x before y before z.
    ! ... 2-D and 1-D cases, for above numbering order, follow automatically.
    ! ...
    ! ... argument list
    ! ...
    real(kind=kv), dimension(:), intent(inout) :: rhs
    ! ...
    ! ... local variables
    ! ...
    integer :: node, nxy, m
    integer, save :: mod_no=8
    ! ... 
    ! ....................................................................
    ! ...
    ! ... reverse order for accessing columns
    ! ...
    ! ... layer k=nz, row j=ny
    ! ... cell i=nx excluded
    ! ... No contribution from DX1 as DX1(n_rows-nx+1)=0
    do node=n_rows-1, n_rows-nx+1, -1
       rhs(node)=rhs(node)-rhs(node+1)*DX0(node)/diag(node)
    enddo
    ! ... layer k=nz
    ! ... row j=ny excluded
    ! ... No contribution from DY1 as DY1(n_rows-nxy+nx)... DY1(n_rows-nxy+1)=0
    ! ... No contribution from DZ1 as DZ1(n_rows-nxy+1)=0
    nxy=nx*ny
    m=mod(nxy-nx, mod_no)
    if (m>0) then
       do node=n_rows-nx, n_rows-nx-m+1, -1
          rhs(node)=rhs(node)-(rhs(node+1)*DX0(node) &
               + rhs(node+nx-1)*DX1(node) &
               + rhs(node+nx)*DY0(node))/diag(node)
       enddo
    endif
    if (nxy-nx>=mod_no) then
       do node=n_rows-nx-m, n_rows-nxy+1, -mod_no
          rhs(node)=rhs(node)-(rhs(node+1)*DX0(node) &
               + rhs(node+nx-1)*DX1(node) &
               + rhs(node+nx)*DY0(node))/diag(node)
          rhs(node-1)=rhs(node-1)-(rhs(node)*DX0(node-1) &
               + rhs(node+nx-2)*DX1(node-1) &
               + rhs(node+nx-1)*DY0(node-1))/diag(node-1)
          rhs(node-2)=rhs(node-2)-(rhs(node-1)*DX0(node-2) &
               + rhs(node+nx-3)*DX1(node-2) &
               + rhs(node+nx-2)*DY0(node-2))/diag(node-2)
          rhs(node-3)=rhs(node-3)-(rhs(node-2)*DX0(node-3) &
               + rhs(node+nx-4)*DX1(node-3) &
               + rhs(node+nx-3)*DY0(node-3))/diag(node-3)
          rhs(node-4)=rhs(node-4)-(rhs(node-3)*DX0(node-4) &
               + rhs(node+nx-5)*DX1(node-4) &
               + rhs(node+nx-4)*DY0(node-4))/diag(node-4)
          rhs(node-5)=rhs(node-5)-(rhs(node-4)*DX0(node-5) &
               + rhs(node+nx-6)*DX1(node-5) &
               + rhs(node+nx-5)*DY0(node-5))/diag(node-5)
          rhs(node-6)=rhs(node-6)-(rhs(node-5)*DX0(node-6) &
               + rhs(node+nx-7)*DX1(node-6) &
               + rhs(node+nx-6)*DY0(node-6))/diag(node-6)
          rhs(node-7)=rhs(node-7)-(rhs(node-6)*DX0(node-7) &
               + rhs(node+nx-8)*DX1(node-7) &
               + rhs(node+nx-7)*DY0(node-7))/diag(node-7)
       enddo
    endif
    ! ... layer k=nz excluded
    m=mod(n_rows-nxy, mod_no)
    if (m>0) then
       do node=n_rows-nxy, n_rows-nxy-m+1, -1
          rhs(node)=rhs(node)-(rhs(node+1)*DX0(node) &
               + rhs(node+nx-1)*DX1(node) &
               + rhs(node+nx)*DY0(node) &
               + rhs(node+nxy-nx)*DY1(node) &
               + rhs(node+nxy-1)*DZ1(node) &
               + rhs(node+nxy)*DZ0(node))/diag(node)
       enddo
    endif
    if (n_rows-nxy>=mod_no) then
       do node=n_rows-nxy-m, 1, -mod_no
          rhs(node)=rhs(node)-(rhs(node+1)*DX0(node) &
               + rhs(node+nx-1)*DX1(node) &
               + rhs(node+nx)*DY0(node) &
               + rhs(node+nxy-nx)*DY1(node) &
               + rhs(node+nxy-1)*DZ1(node) &
               + rhs(node+nxy)*DZ0(node))/diag(node)
          rhs(node-1)=rhs(node-1)-(rhs(node)*DX0(node-1) &
               + rhs(node+nx-2)*DX1(node-1) &
               + rhs(node+nx-1)*DY0(node-1) &
               + rhs(node+nxy-nx-1)*DY1(node-1) &
               + rhs(node+nxy-2)*DZ1(node-1) &
               + rhs(node+nxy-1)*DZ0(node-1))/diag(node-1)
          rhs(node-2)=rhs(node-2)-(rhs(node-1)*DX0(node-2) &
               + rhs(node+nx-3)*DX1(node-2) &
               + rhs(node+nx-2)*DY0(node-2) &
               + rhs(node+nxy-nx-2)*DY1(node-2) &
               + rhs(node+nxy-3)*DZ1(node-2) &
               + rhs(node+nxy-2)*DZ0(node-2))/diag(node-2)
          rhs(node-3)=rhs(node-3)-(rhs(node-2)*DX0(node-3) &
               + rhs(node+nx-4)*DX1(node-3) &
               + rhs(node+nx-3)*DY0(node-3) &
               + rhs(node+nxy-nx-3)*DY1(node-3) &
               + rhs(node+nxy-4)*DZ1(node-3) &
               + rhs(node+nxy-3)*DZ0(node-3))/diag(node-3)
          rhs(node-4)=rhs(node-4)-(rhs(node-3)*DX0(node-4) &
               + rhs(node+nx-5)*DX1(node-4) &
               + rhs(node+nx-4)*DY0(node-4) &
               + rhs(node+nxy-nx-4)*DY1(node-4) &
               + rhs(node+nxy-5)*DZ1(node-4) &
               + rhs(node+nxy-4)*DZ0(node-4))/diag(node-4)
          rhs(node-5)=rhs(node-5)-(rhs(node-4)*DX0(node-5) &
               + rhs(node+nx-6)*DX1(node-5) &
               + rhs(node+nx-5)*DY0(node-5) &
               + rhs(node+nxy-nx-5)*DY1(node-5) &
               + rhs(node+nxy-6)*DZ1(node-5) &
               + rhs(node+nxy-5)*DZ0(node-5))/diag(node-5)
          rhs(node-6)=rhs(node-6)-(rhs(node-5)*DX0(node-6) &
               + rhs(node+nx-7)*DX1(node-6) &
               + rhs(node+nx-6)*DY0(node-6) &
               + rhs(node+nxy-nx-6)*DY1(node-6) &
               + rhs(node+nxy-7)*DZ1(node-6) &
               + rhs(node+nxy-6)*DZ0(node-6))/diag(node-6)
          rhs(node-7)=rhs(node-7)-(rhs(node-6)*DX0(node-7) &
               + rhs(node+nx-8)*DX1(node-7) &
               + rhs(node+nx-7)*DY0(node-7) &
               + rhs(node+nxy-nx-7)*DY1(node-7) &
               + rhs(node+nxy-8)*DZ1(node-7) &
               + rhs(node+nxy-7)*DZ0(node-7))/diag(node-7)
       enddo
    endif
    ! ... 
  end subroutine U_solve_1

end module MiUDU

!************************************************************************
!            PRECONDITIONED CONJUGATE GRADIENT ALGORITHM
!************************************************************************

module PCG_SOLVE
  ! ... PRECONDITIONED CONJUGATE GRADIENT ALGORITHM MODULE
  ! ... last modified: R. L. Naff, July 2007
  ! ... This version uses an incomplete UDU with fill levels 0 and 1.
  ! ... 
  ! ... solve Ax=b
  ! ... A(nxn), x(nx1), b(nx1); A symmetric
  ! ... A is assumed to be be stored in compressed diagonal scheme.
  ! ... only diagonals of upper triangle stored.
  ! ... storage is row ordered, smallest row number to largest.
  ! ...
  ! ... compressed diagonal storage arrays:
  ! ... 
  ! ... dd: diagonal column of A.
  ! ... dx: first off-diagonal column of a corresponding to x connection.
  ! ... dy: second off-diagonal column of a corresponding to y connection.
  ! ... dz: third off-diagonal column of a corresponding to z connection.
  ! ... 
  ! ... nx, ny, nz: x, y and z cell dimensions of grid
  ! ...
  ! ... pcg arrays:
  ! ... X(:): initial guess for solution vector (zero if unspecified).
  ! ... res(:): initial residual: res=b-Ax_o
  ! ...
  ! ... dimensions:
  ! ... dd, xx, rhs:  nx*ny*nz
  ! ... dx: nx*ny*nz if nx>1; otherwise 1
  ! ... dy: nx*ny*nz if ny>1; otherwise 1
  ! ... dz: nx*ny*nz if nz>1; otherwise 1
  ! ...
  ! ... Preconditioner is modified incomplete Cholesky for 5 or 7 point 
  ! ... stencil.
  ! ...
  ! ... R.L. Naff 
  ! ...
  ! ... version 1.0.1, 10/2011
  ! ... 11/2007: release of version 1.0
  ! ... 10/2011: replaced some whole-array operations in conjugate gradient 
  ! ... loop with BLAS-like subroutine calls; see subroutines update_1 and 
  ! ... update_2. 
  ! ...
  use common_parameters
  use common_solver_types
  use MiUDU
  ! ... XX: X=A^{-1}*rhs
  ! ... in: XX=rhs; out: XX=X
  use mat_vec_mult
  ! ... rhs, r_v: A*r_v=rhs
  implicit none
  real(kind=kv), dimension(:), pointer :: x_dev, r_dev
  real(kind=kv) :: nu_i
contains

  subroutine CG_iter(precond_fn, convg_fn, X, res, max_iter, eps_i, &
       eps_o, p_unit)
    ! ... preconditioned conjugate gradient algorithm based on 
    ! ... Golub and Van Loan, 1983.
    ! ... 
    ! ... precond_fn: specify preconditioning function upon call.
    ! ...   iUDU_solve_0: zero-fill incomplete Cholesky.
    ! ...   iUDU_solve_1: one-fill incomplete Cholesky.
    ! ... X: unknown vector.
    ! ... res: initial residual; res=rhs-A*X_o, X_o: initial guess.
    ! ... max_iter (in): maximum allowed iterations.
    ! ... max_iter (out): iterations to convergence.
    ! ... eps_i (in): specified convergence criterion, CG algorithm.
    ! ... eps_i (out): final L2 norm on leaving conjugate gradient loop.
    ! ... eps_o(in): flag to activate adaptive relative convergence and 
    ! ...            threshold for activation.
    ! ... eps_o(out): entry L2 norm 
    ! ... p_unit>0: print convergence progress to file associated with p_unit.
    ! ... 
    ! ... Preconditioning function: precond_fn
    ! ... i_val: dummy variable for preconditioning functions
    ! ... XX: preconditioner variable
    ! ... XX(in): RHS to A*x=r
    ! ... XX(out): approximate solution to A*x=r
    ! ... 
    ! ... argument list
    ! ... 
    integer, external :: precond_fn
    integer, intent(in) :: p_unit
    integer, intent(inout) :: max_iter
    real(kind=kv), intent(inout) :: eps_i
    real(kind=kv), intent(inout) :: eps_o
    real(kind=kv), intent(inout), dimension(:) :: X, res
    logical, external :: convg_fn
    ! ... 
    ! ... local variables
    ! ... 
    integer :: i, i_val
    real(kind=kv) :: mu, nu, nu_p, ratio_l
    real(kind=kv), save :: nu_f=n0
    ! ... 
    ! ...................................................................
    ! ... 
    if (p_unit>0) write(unit=p_unit,fmt=665) 
    ! ... i=0: x_dev=r_dev
    XX=>x_dev
    x_dev=res
    ! ... x_dev=M^{-1}res
    i_val=precond_fn()
    nu=dot_product(x_dev,res)
    nu_i=nu
    ! ... 
    ! ... adaptive convergence
    ! ... ratio_l: measure of linearity of problem; linear, ratio_l=1
    ! ... eps_o: on input, minimum threshold for adaptive convergence 
    ! ...        algorithm.
    ! ...
    if (eps_o>small) then
       ratio_l=nu_f/nu_i
       if (ratio_l<eps_i) then
          if (ratio_l>eps_o) then
             eps_i=ratio_l
          else
             eps_i=eps_o
          endif
       endif
    endif
    XX=>r_dev      
    if (p_unit>0) write(unit=p_unit,fmt=666) 0, nu
    i=0
    do ! max_iter loop
       i=i+1
       ! ... r_dev=A*x_dev
       call a_multiply(x_dev,r_dev)
       mu=nu/dot_product(x_dev,r_dev)
       ! ... update solution
       call update_1(X,x_dev,mu)
       ! ... update residuals
       call update_1(res,r_dev,-mu)
       r_dev=res
       ! ... r_dev=M^{-1}res
       i_val=precond_fn()
       nu_p=nu
       nu=dot_product(res,r_dev)
       ! xxx if (nu<0) print*,'i=',i,'nu=',nu
       if (p_unit>0) write(unit=p_unit,fmt=666) i, nu
       if (convg_fn(eps_i,nu).or.i==max_iter) then
          eps_o=nu_i; eps_i=nu; nu_f=nu
          nullify(XX)
          if (i==max_iter) then
             max_iter=-i
          else
             max_iter=i
          endif
          return
       endif
       call update_2(x_dev,r_dev,nu/nu_p)
    enddo
    ! ...
665 format(/tr3,'inner',tr10,'L2'/tr1,'iteration',tr7,'norm'/tr1,'---------',tr4,'-----------')
666 format(tr2,i4,tr8,1pe12.5)
    ! ...
  end subroutine CG_iter

  subroutine CG_iter_xt(precond_fn, convg_fn, X, res, max_iter, eps_i, &
       eps_o, p_unit)
    ! ... preconditioned conjugate gradient algorithm based on 
    ! ... Golub and Van Loan, 1983.
    ! ... Extended version in which solution is updated one additional time.
    ! ... 
    ! ... precond_fn: specify preconditioning function upon call.
    ! ...   iUDU_solve_0: zero-fill incomplete Cholesky.
    ! ...   iUDU_solve_1: one-fill incomplete Cholesky.
    ! ... X: unknown vector.
    ! ... res: initial residual; res=rhs-A*X_o, X_o: initial guess.
    ! ... max_iter (in): maximum allowed iterations.
    ! ... max_iter (out): iterations to convergence.
    ! ... eps_i (in): specified convergence criterion, CG algorithm.
    ! ... eps_i (out): final L2 norm on leaving conjugate gradient loop.
    ! ... eps_o: entry L2 norm 
    ! ... p_unit>0: print convergence progress to fine associated with p_unit.
    ! ... 
    ! ... Preconditioning function: precond_fn
    ! ... i_val: dummy variable for preconditioning functions
    ! ... XX: preconditioner variable
    ! ... XX(in): RHS to A*x=r
    ! ... XX(out): approximate solution to A*x=r
    ! ... 
    ! ... argument list
    ! ... 
    integer, external :: precond_fn
    integer, intent(in) :: p_unit
    integer, intent(inout) :: max_iter
    real(kind=kv), intent(inout) :: eps_i
    real(kind=kv), intent(out) :: eps_o
    real(kind=kv), intent(inout), dimension(:) :: X, res
    logical, external :: convg_fn
    ! ... 
    ! ... local variables
    ! ... 
    integer :: i, i_val
    real(kind=kv) :: mu, nu, nu_p
    ! ... 
    ! ...................................................................
    ! ... 
    if (p_unit>0) write(unit=p_unit,fmt=665) 
    ! ... i=0: x_dev=r_dev
    XX=>x_dev
    x_dev=res
    i_val=precond_fn()
    nu=dot_product(x_dev,res)
    nu_i=nu
    XX=>r_dev      
    if (p_unit>0) write(unit=p_unit,fmt=666) 0, nu
    do i=1, max_iter
       ! ... r_dev=A*x_dev
       call a_multiply(x_dev,r_dev)
       mu=nu/dot_product(x_dev,r_dev)
       ! ... update solution
       call update_1(X,x_dev,mu)
       if (convg_fn(eps_i,nu)) then
          max_iter=i; eps_o=nu_i; eps_i=nu
          nullify(XX)
          return
       endif
       ! ... update residuals
       call update_1(res,r_dev,-mu)
       r_dev=res
       ! ... r_dev=M^{-1}res
       i_val=precond_fn()
       nu_p=nu
       nu=dot_product(res,r_dev)
       ! xxx if (nu<0) print*,'i=',i,'nu=',nu
       if (p_unit>0) write(unit=p_unit,fmt=666) i, nu
       call update_2(x_dev,r_dev,nu/nu_p)
    enddo
    ! ... max_iter exit
    nullify(XX)
    max_iter=-max_iter; eps_o=nu_i; eps_i=nu
    ! ...
665 format(/tr3,'inner',tr10,'L2'/tr1,'iteration',tr7,'norm'/tr1,'---------',tr4,'-----------')
666 format(tr2,i4,tr8,1pe12.5)
    ! ...
  end subroutine CG_iter_xt

  function abs_convg(eps,nu)
    ! ... absolute convergence function
    ! ... based on eps<sqrt(dot_product(X_c,res));
    ! ... equivalently: eps**2<dot_product(X_c,res)
    ! ... eps must be squared prior to calling function
    real(kind=kv) :: eps,nu
    logical :: abs_convg
    abs_convg=.false.
    if (nu<=eps) abs_convg=.true.
  end function abs_convg

  function rel_convg(eps,nu)
    ! ... relative convergence function
    ! ... based on dot_product(X_c,res)_final<eps*dot_product(X_c,res)_initial
    real(kind=kv) :: eps,nu
    logical :: rel_convg
    rel_convg=.false.
    if (nu<=eps*nu_i) rel_convg=.true.
  end function rel_convg

  subroutine update_1(y,x,r)
    ! ... MODEL: y=y+r*x
    ! ... y, x: arrays
    ! ... r: scalar
    real(kind=kv), dimension(:), intent(inout) :: y
    real(kind=kv), dimension(:), intent(in) :: x
    real(kind=kv), intent(in) :: r
    integer, save :: mod_no=8
    integer :: ii, m, asize
    asize=size(y)
    m=mod(asize,mod_no)
    !$OMP PARALLEL DEFAULT(NONE) &
    !$OMP SHARED(y,x,r) &
    !$OMP PRIVATE(ii) &
    !$OMP FIRSTPRIVATE(m,mod_no,asize)
    if (m>0) then
       !$OMP DO           
       do ii=1, m
          y(ii)=y(ii)+r*x(ii)
       enddo
       !$OMP END DO
    endif
    if (asize>mod_no) then
       !$OMP DO           
       do ii=m+1, asize, mod_no
          y(ii)=y(ii)+r*x(ii)
          y(ii+1)=y(ii+1)+r*x(ii+1)
          y(ii+2)=y(ii+2)+r*x(ii+2)
          y(ii+3)=y(ii+3)+r*x(ii+3)
          y(ii+4)=y(ii+4)+r*x(ii+4)
          y(ii+5)=y(ii+5)+r*x(ii+5)
          y(ii+6)=y(ii+6)+r*x(ii+6)
          y(ii+7)=y(ii+7)+r*x(ii+7)
       enddo
       !$OMP END DO
    endif
    !$OMP END PARALLEL
    ! ...
  end subroutine update_1

  subroutine update_2(y,x,r)
    ! ... MODEL: y=r*y+x
    ! ... y, x: arrays
    ! ... r: scalar
    real(kind=kv), dimension(:), intent(inout) :: y
    real(kind=kv), dimension(:), intent(in) :: x
    real(kind=kv), intent(in) :: r
    integer, save :: mod_no=8
    integer :: ii, m, asize
    asize=size(y)
    m=mod(asize,mod_no)
    !$OMP PARALLEL DEFAULT(NONE) &
    !$OMP SHARED(y,x,r) &
    !$OMP PRIVATE(ii) &
    !$OMP FIRSTPRIVATE(m,mod_no,asize)
    if (m>0) then
       !$OMP DO           
       do ii=1, m
          y(ii)=r*y(ii)+x(ii)
       enddo
       !$OMP END DO
    endif
    if (asize>mod_no) then
       !$OMP DO           
       do ii=m+1, asize, mod_no
          y(ii)=r*y(ii)+x(ii)
          y(ii+1)=r*y(ii+1)+x(ii+1)
          y(ii+2)=r*y(ii+2)+x(ii+2)
          y(ii+3)=r*y(ii+3)+x(ii+3)
          y(ii+4)=r*y(ii+4)+x(ii+4)
          y(ii+5)=r*y(ii+5)+x(ii+5)
          y(ii+6)=r*y(ii+6)+x(ii+6)
          y(ii+7)=r*y(ii+7)+x(ii+7)
       enddo
       !$OMP END DO
    endif
    !$OMP END PARALLEL
    ! ...
  end subroutine update_2

end module PCG_SOLVE

!************************************************************************
!                   MAIN FOR PCG SOLVER
!************************************************************************

module PCG_MAIN
  ! ... FRONT END FOR PCG SOLVER
  ! ... last modified: R. L. Naff, July 2007
  ! ... 
  ! ... Calls subroutines in module pcg_solve
  ! ... 
  ! ... Preconditioner is modified incomplete Cholesky for 5 or 7 point 
  ! ... stencil.
  ! ... 
  ! ... solve Ax=b
  ! ... A(nxn), x(nx1), b(nx1); A symmetric
  ! ... A is assumed to be be stored in compressed diagonal scheme.
  ! ... Only diagonals of upper triangle stored.
  ! ... Storage is row ordered, smallest row number to largest.
  ! ...
  ! ... Compressed diagonal storage arrays:
  ! ... 
  ! ... dd: diagonal column of A.
  ! ... dx: first off-diagonal column of a corresponding to x connection.
  ! ... dy: second off-diagonal column of a corresponding to y connection.
  ! ... dz: third off-diagonal column of a corresponding to z connection.
  ! ... 
  ! ... nx, ny, nz: x, y and z cell dimensions of grid
  ! ...
  ! ... PCG arrays:
  ! ... xx(:): initial guess for solution vector (zero if unspecified).
  ! ... rhs(:): right-hand-side vector, b.
  ! ... If xx present, solution vector returned in xx; 
  ! ... otherwise it is returned in rhs.
  ! ...
  ! ... Dimensions:
  ! ... dd, xx, rhs:  nx*ny*nz
  ! ... dx: nx*ny*nz if nx>1; otherwise 1
  ! ... dy: nx*ny*nz if ny>1; otherwise 1
  ! ... dz: nx*ny*nz if nz>1; otherwise 1
  ! ...
  ! ... Options:
  ! ...   Relaxation parameter: 0<=omega<=1
  ! ...   Incomplete Cholesky precondition with either 0 or 1 level of fill.
  ! ...
  ! ... See headers of modules pcg_solve and MiUDU for error messages 
  ! ... and warnings.
  ! ...
  ! ... err_stat and/or error codes
  ! ...   10: allocation error
  ! ...   15: pivot problem
  ! ...   20: dimension mismatch
  ! ...
  ! ... R.L. Naff
  ! ...
  ! ... version 1.0, 11/2007
  ! ...
  use common_parameters
  use common_solver_types
  ! ... common_solver_types: DD, DX, DY, DZ, dim, nx, ny, nz, n_rows
  use MiUDU
  use mat_vec_mult
  use pcg_solve
  implicit none
  integer, save :: fill_level, x_flag, pc_unit, ts_unit
  real(kind=kv), save :: omega, eps=small, eps_t
  character(len=32), dimension(:), pointer :: marker_name
  private; public :: PCG, PCG_init, PCG_fin
contains

  subroutine PCG_init(fill, n_x, n_y, n_z, d_d, d_x, d_y, d_z, &
       relax, flag_x, unit_ts, unit_pc, mrkr_name, err_stat)
    ! ... 
    ! ... Initialize PCG solver
    ! ... 
    ! ... fill: level of fill for iUDU preconditioner (0,1)
    ! ... fill_level=fill
    ! ... 
    ! ... n_x, n_y, n_z: x, y and z cell dimensions of grid
    ! ... 
    ! ... d_d: diagonal column of A.
    ! ... d_x: first off-diagonal column of a corresponding to x connection.
    ! ... d_y: second off-diagonal column of a corresponding to y connection.
    ! ... d_z: third off-diagonal column of a corresponding to z connection.
    ! ... 
    ! ... relax: relaxation parameter (0.0<=relax<=1.0); optional.
    ! ... omega=relax
    ! ... 
    ! ... flag_x=1: call CG_iter
    ! ... flag_x=2: call CG_iter_xt
    ! ... x_flag=flag_x
    ! ... 
    ! ... unit_ts>0: obtain and print elapsed time in solver.
    ! ... unit_ps>0: print convergence of inner iteration (CG_iter).
    ! ... ts_unit=unit_ts and pc_unit=unit_pc
    ! ... 
    ! ... mrkr_name(3): Allows up to three marker names corresponding to 
    ! ...                 markers; optional.
    ! ... 
    ! ... err_stat(2): returns error number plus info; optional.
    ! ... 
    ! ... argument list
    ! ... 
    integer, intent(in) :: fill, n_x, n_y, n_z, flag_x, unit_ts, unit_pc
    character(len=32), dimension(1:3), optional, intent(in), target :: &
         mrkr_name
    real(kind=kv), optional, intent(in) :: relax
    real(kind=kv), dimension(:), intent(in), target :: d_d, d_x, &
         d_y, d_z
    integer, dimension(1:2), intent(inout), optional :: err_stat
    ! ... 
    ! ... local variables
    ! ... 
    integer :: i, error
    ! ... 
    ! .....................................................................
    ! ... 
    error=0; n_rows=n_x*n_y*n_z
    ts_unit=unit_ts; pc_unit=unit_pc; x_flag=flag_x
    if (present(err_stat)) err_stat=0
    if (present(mrkr_name)) marker_name=>mrkr_name
    ! ...
    ! ... rotate and set dimensions and vectors
    ! ...
    dd=>d_d
    if (n_x>1.and.n_y>1.and.n_z>1) then
       dim=3
       nx=n_x; ny=n_y; nz=n_z
       dx=>d_x; dy=>d_y; dz=>d_z
    elseif (n_x>1.and.n_y>1.or.n_y>1.and.n_z>1.or.n_x>1.and.n_z>1) then
       dim=2
       if (n_z==1) then
          nx=n_x; ny=n_y; nz=n_z
          dx=>d_x; dy=>d_y; dz=>d_z
       elseif (n_y==1) then
          nx=n_x; ny=n_z; nz=n_y
          dx=>d_x; dy=>d_z; dz=>d_y
       else ! n_x=1
          nx=n_y; ny=n_z; nz=n_x
          dx=>d_y; dy=>d_z; dz=>d_x
       endif
    else
       dim=1
       if (n_x>1) then
          nx=n_x; ny=n_y; nz=n_z
          dx=>d_x; dy=>d_y; dz=>d_z
       elseif (n_y>1) then
          nx=n_y; ny=n_z; nz=n_x
          dx=>d_y; dy=>d_z; dz=>d_x
       else ! n_z>1          
          nx=n_z; ny=n_x; nz=n_y
          dx=>d_z; dy=>d_y; dz=>d_x
       endif
    endif
    ! ...
    ! ... Allocate arrays per fill level
    ! ...
    fill_level=fill
    allocate (diag(1:n_rows), stat=error)
    if (error/=0) then
       if (present(err_stat)) then
          err_stat=(/10,error/); return
       else
          write(*,500)
          call pcg_fin; stop
       endif
    endif
    if (fill_level==1) then
       select case(dim)
       case(3)
          ! ... 3-D
          allocate (dx0(1:n_rows),dy0(1:n_rows),dx1(1:n_rows), &
               dy1(1:n_rows),dz1(1:n_rows), stat=error)
          if (error/=0) then
             if (present(err_stat)) then
                err_stat=(/10,error/); return
             else
                write(*,500)
                call pcg_fin; stop
             endif
          endif
          dz0=>dz
          dx0=n0; dy0=n0; dx1=n0; dy1=n0; dz1=n0
       case(2)
          ! ... 2-D
          allocate (dx0(1:n_rows),dx1(1:n_rows), stat=error)
          if (error/=0) then
             if (present(err_stat)) then
                err_stat=(/10,error/); return
             else
                write(*,500)
                call pcg_fin; stop
             endif
          endif
          dy0=>dy
          dx0=n0; dx1=n0
       case(1)
          ! ... 1-D
          dx0=>dx
       end select
    endif
    ! ...
    ! ... relaxation parameter
    ! ... 
    if (present(relax)) then
       omega=relax
    else
       omega=0.99_kv
    endif
    ! ...
500 format(1x,'Allocation error in subroutine PCG_init')
    ! ...
  end subroutine PCG_init

  subroutine PCG(resid, convg_i, xx, max_iter, c_flag, convg_o, marker, &
       err_stat)
    ! ... 
    ! ... main entry point for pcg algorithm
    ! ... 
    ! ... resid: residual vector; return for solution if xx not present.
    ! ... xx: initial guess and solution vector; optional.
    ! ... 
    ! ... convg_i(in): convergence criterion, inner iteration.
    ! ... convg_i(out): L2 norm at convergence, inner iteration.
    ! ... max_iter(in): maximum allowed iterations; optional.
    ! ... max_iter(out): iterations required to convergence; optional.
    ! ...
    ! ... c_flag=1: use absolute convergence
    ! ... c_flag=2: use relative convergence
    ! ...
    ! ... convg_o(in): adaptive relative convergence flag for 
    ! ... extended cg algorithm
    ! ... convg_o(out): returns weighted L2 norm at entry to cg loop
    ! ... convg_o should be returned in conjunction with c_flag>1, but 
    ! ...    applies principally to the standard cg algorithm.
    ! ...
    ! ... marker(3): Allows up to three integer markers to be placed in 
    ! ...            ts_unit and/or pc_unit files; optional.
    ! ... 
    ! ... err_stat(2): returns error number plus info; optional.
    ! ... 
    ! ... argument list
    ! ... 
    real(kind=kv), dimension(:), intent(inout) :: resid
    real(kind=kv), intent(inout) :: convg_i
    real(kind=kv), dimension(:), optional, intent(inout), target :: xx
    integer, intent(inout), optional :: max_iter
    integer, intent(in), optional :: c_flag
    real(kind=kv), intent(inout), optional :: convg_o
    integer, dimension(1:3), optional, intent(in) :: marker
    integer, dimension(1:2), intent(inout), optional :: err_stat
    ! ... 
    ! ... local variables
    ! ... 
    integer :: ii, jj, kk
    real(kind=kv) :: eps_i, eps_o
    integer :: error, iter_max, iter_no, i, start_time, end_time, solv_time
    integer :: cum_time=0
    integer, external :: elapsed_time
    real(kind=kv), dimension(:), pointer :: X
    ! ... 
    ! .....................................................................
    ! ... 
    nullify (X)
    error=0; iter_no=0
    if (present(err_stat)) err_stat=0
    ! ... 
    ! ... initial array size checking
    ! ... 
    if (size(resid)/=size(dd)) then
       error=20
    elseif (present(xx)) then
       if (size(xx)/=size(dd)) error=20
    endif
    if (error/=0) then
       if (present(err_stat)) then
          err_stat=(/error,0/)
          return
       else
          write(*,520)
          call pcg_fin; stop
       endif
    endif
    ! ... 
    ! ... set diagonal per fill level
    ! ...
    if (fill_level==0) then
       call diag_factor_0(omega,error)
    else
       call diag_factor_1(omega,error)
    endif
    ! ... 
    if (error/=0) then
       call pcg_fin
       if (present(err_stat)) then
          err_stat=(/15,error/)
          return
       else
          if (error>0) then
             ii=(mod(error,nx*ny)-1)/ny+1
             jj=mod(mod(error,nx*ny)-1,ny)+1
             kk=(error-1)/(nx*ny)+1
             write(*,500) jj, ii, kk
          else
             ! ... negative error indicates negative pivot
             error=-error
             ii=(mod(error,nx*ny)-1)/ny+1
             jj=mod(mod(error,nx*ny)-1,ny)+1
             kk=(error-1)/(nx*ny)+1
             write(*,505) jj, ii, kk
          endif
          call pcg_fin; stop
       endif
    endif
    ! ... 
    if (present(max_iter)) then
       iter_max=max_iter
    else
       iter_max=max_iter_approx()
    end if
    ! ... print markers for time and/or progress files
    ! ... NOTE: Calling program should OPEN ts_unit and/or pc_unit
    if (ts_unit>0) call marker_print(ts_unit, marker, marker_name)
    if (pc_unit>0) call marker_print(pc_unit, marker, marker_name)
    ! ...
    ! ... solve Ax=rhs by pcg
    ! ...
    if (ts_unit>0) start_time=elapsed_time(2)
    if (present(xx)) then
       X=>xx
    else
       allocate (X(1:n_rows), stat=error)
       if (error/=0) then
          if (present(err_stat)) then
             err_stat=(/10,error/); return
          else
             write(*,515)
             call pcg_fin; stop
          endif
       endif
       X=n0
    endif
    ! ...
    ! ... adaptive relative convergence flag and threshold
    ! ...
    if (present(convg_o).and.x_flag==1.and.c_flag/=1) then
       eps_o=convg_o
    else
       ! ... turn off adaptive convergence
       eps_o=-n1
    endif
    ! ...
    allocate (x_dev(1:n_rows), r_dev(1:n_rows), stat=error)
    if (error/=0) then
       if (present(err_stat)) then
          err_stat=(/10,error/); return
       else
          write(*,515)
          call pcg_fin; stop
       endif
    endif
    ! ...
    if (x_flag==1) then 
       ! ...  standard cg algorithm 
       if (c_flag==1) then
          ! ... abs convergence
          eps_i=convg_i**2
          if (fill_level==0) then
             call CG_iter(iUDU_solve_0, abs_convg, X, resid, iter_max, &
                  eps_i, eps_o, pc_unit)
          else
             call CG_iter(iUDU_solve_1, abs_convg, X, resid, iter_max, &
                  eps_i, eps_o,  pc_unit)
          endif
          convg_i=sqrt(eps_i)
       else ! c_flag>1
          ! ...  rel convergence
          if (fill_level==0) then
             call CG_iter(iUDU_solve_0, rel_convg, X, resid, iter_max, &
                  convg_i, eps_o,  pc_unit)
          else
             call CG_iter(iUDU_solve_1, rel_convg, X, resid, iter_max, &
                  convg_i, eps_o,  pc_unit)
          endif
       endif
    else ! x_flag>1
       ! ...  extended cg algorithm 
       if (c_flag==1) then
          ! ... abs convergence
          eps_i=convg_i**2
          if (fill_level==0) then
             call CG_iter_xt(iUDU_solve_0, abs_convg, X, resid, iter_max, &
                  eps_i, eps_o, pc_unit)
          else
             call CG_iter_xt(iUDU_solve_1, abs_convg, X, resid, iter_max, &
                  eps_i, eps_o, pc_unit)
          endif
          convg_i=sqrt(eps_i)
       else ! c_flag>1    
          ! ...  rel convergence
          if (fill_level==0) then
             call CG_iter_xt(iUDU_solve_0, rel_convg, X, resid, iter_max, &
                  convg_i, eps_o, pc_unit)
          else
             call CG_iter_xt(iUDU_solve_1, rel_convg, X, resid, iter_max, &
                  convg_i, eps_o, pc_unit)
          endif
       endif
    endif
    ! ...
    if (present(convg_o)) convg_o=eps_o
    deallocate (x_dev,r_dev)
    ! ...
    if (present(max_iter)) max_iter=iter_max
    if (ts_unit>0) then
       solv_time=elapsed_time(2)-start_time
       cum_time=cum_time+solv_time
       write(unit=ts_unit,fmt=510) iter_max, real(solv_time)/1000., &
            real(cum_time)/1000. 
    endif
    if (.not.present(xx)) then
       resid=X
       deallocate(X)
    else
       nullify(X)
    endif
    ! ...
500 format(1x,'ARRAY DIAG CONTAINS A ZERO ELEMENT AT NODE',/, &
         1x,'mesh location: j=',i4,', i=',i4,', k=',i4,/, &
         1x,'***EXECUTION TERMINATED***')
505 format(1x,'ARRAY DIAG CONTAINS A NEGATIVE ELEMENT AT NODE',/, &
         1x,'mesh location: j=',i4,', i=',i4,', k=',i4,/, &
         1x,'***EXECUTION TERMINATED***')
510 format (2x,i6,' iterations required ',g12.5,' sec execution time',/, &
         2x,'cumulative time in solver:',g12.5,' sec')
515 format(1x,'Allocation error in subroutine PCG')
520 format(1x,'Dimension mismatch in subroutine PCG')
    ! ...
  end subroutine PCG

  subroutine PCG_fin
    ! ... 
    ! ... deallocate arrays, nullify pointers
    ! ... 
    if (associated(diag)) deallocate(diag)
    nullify (dd, dx, dy, dz)
    if (fill_level==1) then
       select case(dim)
       case(3)
          if (associated(dx0)) deallocate(dx0)
          if (associated(dy0)) deallocate(dy0)
          if (associated(dx1)) deallocate(dx1)
          if (associated(dy1)) deallocate(dy1)
          if (associated(dz1)) deallocate(dz1)
          nullify (dz0)
       case(2)
          if (associated(dx0)) deallocate (dx0)
          if (associated(dx1)) deallocate (dx1)
          nullify (dy0)
       case(1)
          nullify (dx0)
       end select
    endif
    if (ts_unit>0) close(unit=ts_unit)
    if (pc_unit>0) close(unit=pc_unit)
    ! ... 
  end subroutine PCG_fin

  subroutine marker_print(unit_no, marker, marker_name)
    ! ... Print markers for time and/or progress files.
    ! ... Only prints markers when marker variable has 
    ! ... non-zero content; see below.
    ! ... 
    ! ... argument list
    ! ... 
    integer, intent(in) :: unit_no
    integer, dimension(1:3), intent(in) :: marker
    character(len=32), dimension(1:3), intent(in) :: marker_name
    ! ... 
    if (marker(1)>0.and.marker(2)>0.and.marker(3)>0) then
       write (unit=unit_no, fmt=500) trim(marker_name(1)), marker(1), &
            trim(marker_name(2)), marker(2), trim(marker_name(3)), marker(3)
    elseif (marker(1)>0.and.marker(2)>0) then
       write (unit=unit_no, fmt=505) trim(marker_name(1)), marker(1), &
            trim(marker_name(2)), marker(2)
    elseif (marker(1)>0.and.marker(3)>0) then
       write (unit=unit_no, fmt=505) trim(marker_name(1)), marker(1), &
            trim(marker_name(3)), marker(3)
    elseif (marker(2)>0.and.marker(3)>0) then
       write (unit=unit_no, fmt=505) trim(marker_name(2)), marker(2), &
            trim(marker_name(3)), marker(3)
    elseif (marker(1)>0) then
       write (unit=unit_no, fmt=510) trim(marker_name(1)), marker(1)
    elseif (marker(2)>0) then
       write (unit=unit_no, fmt=510) trim(marker_name(2)), marker(2)
    elseif (marker(3)>0) then
       write (unit=unit_no, fmt=510) trim(marker_name(3)), marker(3)
    endif
500 format (/,1x,'AT ',A,1x,i6,', ',A,1x,i6,', ',A,1x,i6,/)
505 format (/,1x,'AT ',A,1x,i6,', ',A,1x,i6,/)
510 format (/,1x,'AT ',A,1x,i6,/)
    ! ... 
  end subroutine marker_print

  function max_iter_approx() result(iter_max)
    ! ... Approximated maximum number of iteration for PCG.
    ! ... result
    integer :: iter_max
    ! ... local variables
    integer :: ponent, a_iter, o_iter, a_nodes, o_nodes
    ! ...................................................................
    ponent=nint(log10(real(n_rows)))
    if (ponent==0) then
       iter_max=n_rows
       return
    endif
    a_nodes=10**ponent
    a_iter=10*2**ponent-10
    if (n_rows<a_nodes) then
       o_iter=10*2**(ponent-1)-10
       o_nodes=10**(ponent-1)
    else
       o_iter=10*2**(ponent+1)-10
       o_nodes=10**(ponent+1)
    endif
    iter_max=nint(real(n_rows-o_nodes)*real(a_iter-o_iter) &
         /real(a_nodes-o_nodes))+o_iter
    ! ...
  end function max_iter_approx

end module PCG_MAIN


function elapsed_time(t_opt) result(e_time)
  ! ... Purpose: calculate elapsed execution time, tenths of a second.
  ! ... Assumes that  execution time is no more than one month.
  ! ... First call sets reference time; subsequent calls return elapsed 
  ! ... time relative to reference time.
  ! ... Returns:
  ! ... t_opt=0: returns time in deci-seconds
  ! ... t_opt=1: returns time in centi-seconds
  ! ... t_opt=2: returns time in milli-seconds
  ! ... t_opt=other: returns time in seconds
  ! ... argument list
  ! ... 
  integer :: t_opt
  ! ... 
  ! ... result
  ! ... 
  integer :: e_time
  ! ... 
  ! ... local variables
  ! ... 
  integer, dimension(1:8) :: t_part
  integer, save, dimension(1:12) :: no_dapmo= &
       (/31,28,31,30,31,30,31,31,30,31,30,31/)
  integer, save :: mo_s, da_s, h_s, mi_s, sec_s, msec_s
  logical, save :: initial=.true., flip
  integer :: mo_d, da_d, h_d, mi_d, sec_d, msec_d, t_sec, t_msec
  ! .....................................................................
  ! ... year=>t_part(1); month=>t_part(2); day=>t_part(3); hour=>t_part(5)
  ! ... minute=>t_part(6); second=>t_part(7); msecond=>t_part(8)
  call date_and_time(values=t_part)
  if (initial) then
     initial=.false.
     mo_s=t_part(2); da_s=t_part(3); h_s=t_part(5)
     mi_s=t_part(6); sec_s=t_part(7); msec_s=t_part(8)
     if (mod(t_part(1),4)==0) no_dapmo(2)=29
     e_time=0
     return
  else
     flip=.false.
     t_sec=0
     mo_d=t_part(2)-mo_s-1
     da_d=t_part(3)-da_s-1
     h_d=t_part(5)-h_s-1
     mi_d=t_part(6)-mi_s-1
     sec_d=t_part(7)-sec_s-1
     msec_d=t_part(8)-msec_s
     if (mo_d/=-1) then
        t_sec=t_sec+(no_dapmo(mo_s)+da_d)*86400
        flip=.true.
     elseif (da_d>-1) then
        t_sec=t_sec+da_d*86400
        flip=.true.
     endif
     if (flip) then
        t_sec=t_sec+(24+h_d)*3600
     elseif (h_d>-1) then
        t_sec=t_sec+h_d*3600
        flip=.true.
     endif
     if (flip) then
        t_sec=t_sec+(60+mi_d)*60
     elseif (mi_d>-1) then
        t_sec=t_sec+mi_d*60
        flip=.true.
     endif
     if (flip) then
        t_sec=t_sec+60+sec_d
     elseif (sec_d>-1) then
        t_sec=t_sec+sec_d
        flip=.true.
     endif
     ! ... milliseconds
     if (flip) then
        t_msec=1000+msec_d
     else
        t_msec=msec_d
     endif
     ! ... add second and milliseconds to form time measure
     if (t_opt==0) then
        e_time=t_sec*10+t_msec/100
        if (mod(t_msec,100)>50) e_time=e_time+1
     elseif (t_opt==1) then
        e_time=t_sec*100+t_msec/10
        if (mod(t_msec,10)>5) e_time=e_time+1
     elseif (t_opt==2) then
        e_time=t_sec*1000+t_msec
     else
        e_time=t_sec+t_msec/1000
        if (mod(t_msec,1000)>500) e_time=e_time+1
     endif
     ! ... 
  endif
  ! ... 
end function elapsed_time
