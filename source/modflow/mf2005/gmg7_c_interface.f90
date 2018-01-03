!--------------------------------------------------------------------
! Requires the Fortran 2003 ISO_C_BINDING Intrinsic Module
!--------------------------------------------------------------------
Module GMG_C_INTERFACE
  Interface

!
! Allocates GMG data
!
  Subroutine MF2KGMG_ALLOCATE( GMGID, &
                               NCOL,  &
                               NROW,  &
                               NLAY,  &
                               IPREC, &
                               ISM,   &
                               ISC,   &
                               RELAX, &
                               ISIZ,  &
                               IERR ) &
  Bind ( C, Name='MF2KGMG_ALLOCATE' )
    Use ISO_C_BINDING
    Type    ( C_PTR    ), Intent (   OUT ) :: GMGID
    Integer ( C_INT    ), Intent ( IN    ) :: NCOL
    Integer ( C_INT    ), Intent ( IN    ) :: NROW
    Integer ( C_INT    ), Intent ( IN    ) :: NLAY
    Integer ( C_INT    ), Intent ( IN    ) :: IPREC
    Integer ( C_INT    ), Intent ( IN    ) :: ISM
    Integer ( C_INT    ), Intent ( IN    ) :: ISC
    Real    ( C_DOUBLE ), Intent ( IN    ) :: RELAX
    Integer ( C_INT    ), Intent (   OUT ) :: ISIZ
    Integer ( C_INT    ), Intent (   OUT ) :: IERR
  End Subroutine MF2KGMG_ALLOCATE

!
! Deallocates GMG data; necessary when MF2K is run in batch mode.
!
  Subroutine MF2KGMG_FREE( GMGID ) &
  Bind ( C, Name='MF2KGMG_FREE' )
    Use ISO_C_BINDING
    Type    ( C_PTR    ), Intent ( IN    ) :: GMGID
  End Subroutine MF2KGMG_FREE

! Assemble GMG data:
! Arguments that are pointer to void are either single precision
! or double precision and are resolved at run time.
!
  Subroutine MF2KGMG_ASSEMBLE( GMGID,  &
                               BIGR0,  &
                               CC,     &
                               CR,     &
                               CV,     &
                               HCOF,   &
                               HNEW,   &
                               RHS, &
                               HNOFLO, &
                               IBOUND, &
                               IERR ) &
  Bind ( C, Name='MF2KGMG_ASSEMBLE' )
    Use ISO_C_BINDING
    Type    ( C_PTR    ), Intent ( IN    ) :: GMGID
    Real    ( C_DOUBLE ), Intent (   OUT ) :: BIGR0
    ! Note: CC, CR, CV, HCOF, RHS, and HNOFLO are all either single or
    ! double precision ( Real( C_FLOAT ) or Real( C_DOUBLE ) )
    Type    ( C_PTR    ), Value            :: CC     ! use C_LOC( CC )
    Type    ( C_PTR    ), Value            :: CR     ! use C_LOC( CR )
    Type    ( C_PTR    ), Value            :: CV     ! use C_LOC( CV )
    Type    ( C_PTR    ), Value            :: HCOF   ! use C_LOC( HCOF )
    Real    ( C_DOUBLE ), Intent ( INOUT ) :: HNEW(*)
    Type    ( C_PTR    ), Value            :: RHS    ! use C_LOC( RHS )
    Type    ( C_PTR    ), Value            :: HNOFLO ! use C_LOC( HNOFLO )
    Integer ( C_INT    ), Intent ( INOUT ) :: IBOUND(*)
    Integer ( C_INT    ), Intent (   OUT ) :: IERR
  End Subroutine MF2KGMG_ASSEMBLE

! MF2KGMG_EVAL: Computes head change returning l2-norm of residual (BIGR)
!  and max head change (BIGH).  Absolute value of BIGH is max-norm of
!  head-change.
!
  Subroutine MF2KGMG_EVAL( GMGID,   &
                           ITER,    &
                           BIGR,    &
                           DRCLOSE, &
                           IITER,   &
                           IOUTGMG, &
                           IOUT ) &
  Bind ( C, Name='MF2KGMG_EVAL' )
    Use ISO_C_BINDING
    Type    ( C_PTR    ), Intent ( IN    ) :: GMGID
    Integer ( C_INT    ), Intent (   OUT ) :: ITER
    Real    ( C_DOUBLE ), Intent (   OUT ) :: BIGR
    Real    ( C_DOUBLE ), Intent ( IN    ) :: DRCLOSE
    Integer ( C_INT    ), Intent ( IN    ) :: IITER
    Integer ( C_INT    ), Intent ( IN    ) :: IOUTGMG
    Integer ( C_INT    ), Intent ( IN    ) :: IOUT
  End Subroutine MF2KGMG_EVAL

!
!  MF2KGMG_UPDATE: Adds damped head change to current approximation.
!
  Subroutine MF2KGMG_UPDATE( GMGID, &
                             HNEW,  &
                             DDAMP ) &
  Bind ( C, Name='MF2KGMG_UPDATE' )
    Use ISO_C_BINDING
    Type    ( C_PTR    ), Intent ( IN    ) :: GMGID
    Real    ( C_DOUBLE ), Intent ( INOUT ) :: HNEW(*)
    Real    ( C_DOUBLE ), Intent ( IN    ) :: DDAMP
  End Subroutine MF2KGMG_UPDATE

! Calculate l2-norm of residual and return location
! of max residual.
!
  Subroutine MF2KGMG_BIGR( GMGID, &
                           BIGR,  &
                           IBIGR, &
                           JBIGR, &
                           KBIGR ) &
  Bind ( C, Name='MF2KGMG_BIGR' )
    Use ISO_C_BINDING
    Type    ( C_PTR    ), Intent ( IN    ) :: GMGID
    Real    ( C_DOUBLE ), Intent (   OUT ) :: BIGR
    Integer ( C_INT    ), Intent (   OUT ) :: IBIGR
    Integer ( C_INT    ), Intent (   OUT ) :: JBIGR
    Integer ( C_INT    ), Intent (   OUT ) :: KBIGR
  End Subroutine MF2KGMG_BIGR

!
! Calculate max head change and return location of max head change.
! Absolute value of max head change is max-norm of head change.
!
  Subroutine MF2KGMG_BIGH( GMGID, &
                           BIGH,  &
                           IBIGH, &
                           JBIGH, &
                           KBIGH ) &
  Bind ( C, Name='MF2KGMG_BIGH' )
    Use ISO_C_BINDING
    Type    ( C_PTR    ), Intent ( IN    ) :: GMGID
    Real    ( C_DOUBLE ), Intent (   OUT ) :: BIGH
    Integer ( C_INT    ), Intent (   OUT ) :: IBIGH
    Integer ( C_INT    ), Intent (   OUT ) :: JBIGH
    Integer ( C_INT    ), Intent (   OUT ) :: KBIGH
  End Subroutine MF2KGMG_BIGH

!
! CALLED FROM THE SOLVER FOR OUTPUT OF REDUCTION HISTORY.
!

  Subroutine RESPRINT( IOUT, &
                       I,    &
                       RES,  &
                       CFAC) &
  Bind ( C, Name='RESPRINT' )
    Use ISO_C_BINDING
    Integer ( C_INT    ), Intent ( IN    ) :: IOUT
    Integer ( C_INT    ), Intent ( IN    ) :: I
    Real    ( C_DOUBLE ), Intent ( IN    ) :: RES
    Real    ( C_DOUBLE ), Intent ( IN    ) :: CFAC
  End Subroutine RESPRINT

  End Interface
End Module GMG_C_INTERFACE
