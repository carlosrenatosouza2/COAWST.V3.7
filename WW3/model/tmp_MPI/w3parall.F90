      MODULE W3PARALL
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/   01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Parallel routines for implicit solver
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
!
 
      REAL, ALLOCATABLE    :: AC_tot(:,:)
      INTEGER, ALLOCATABLE :: ListISPnextDir(:), ListISPprevDir(:)
      INTEGER, ALLOCATABLE :: ListISPnextFreq(:), ListISPprevFreq(:)
      INTEGER, PARAMETER   :: IMEM = 2
 
      REAL,  PARAMETER     :: ONESIXTH  = 1.0d0/6.0d0
      REAL,  PARAMETER     :: ONETHIRD  = 1.0d0/3.0d0
      REAL,  PARAMETER     :: ZERO      = 0.0d0
 
      REAL,  PARAMETER     :: THR8      = TINY(1.0)
      REAL,  PARAMETER     :: THR       = TINY(1.0)
!!/S      CALL STRACE (IENT, 'W3XXXX')
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE WAV_MY_WTIME(eTime)
!/ ------------------------------------------------------------------- /
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/   01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose :
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
       IMPLICIT NONE
      INTEGER mpimode
      REAL(8), intent(out) :: eTime
      REAL(8) mpi_wtime
      mpimode=0
      mpimode=1
      eTime=mpi_wtime()
      IF (mpimode .eq. 0) THEN
        CALL CPU_TIME(eTime)
      END IF
!/
!/ End of JACOBI_INIT ------------------------------------------------ /
!/
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      SUBROUTINE PRINT_MY_TIME(string)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/   01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Print timings
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3ODATMD, ONLY : IAPROC
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
!/
!/ ------------------------------------------------------------------- /
!
      character(*), intent(in) :: string
      REAL(8) :: eTime
      CALL WAV_MY_WTIME(eTime)
      WRITE(740+IAPROC,*) 'TIMING time=', eTime, ' at step ', string
!/
!/ End of JACOBI_INIT ------------------------------------------------ /
!/
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      SUBROUTINE PROP_REFRACTION_PR1(ISEA,DTG, CAD)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/   01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Compute refraction part in matrix
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
     USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
                          EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
                          CTMAX, DMIN, DTH, CTHG0S, MAPSF
      USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, DDDX,   &
                          DDDY, DW
      USE W3IDATMD, ONLY: FLCUR
      USE W3ODATMD, only : IAPROC
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
!/
!/ ------------------------------------------------------------------- /
!/
!/
      REAL, intent(out) :: CAD(NSPEC)
      INTEGER, intent(in) :: ISEA
      REAL, intent(in) :: DTG
      INTEGER :: ISP, IK, ITH, IX, IY
      REAL :: FRK(NK), FRG(NK), DSDD(0:NK+1)
      REAL :: FACTH, DCXY, DCYX, DCXXYY, DTTST
      REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eDDDX, eDDDY, eCTHG0
      REAL :: VCFLT(NSPEC), DEPTH, FDG
      REAL :: FDDMAX
      IX=MAPSF(ISEA,1)
      IY=MAPSF(ISEA,2)
      eDDDX=DDDX(IY,IX)
      eDDDY=DDDY(IY,IX)
      eCTHG0 = CTHG0S(ISEA)
      FACTH  = DTG / DTH
      !
      FDG    = FACTH * eCTHG0
      DEPTH  = MAX ( DMIN , DW(ISEA) )
      DO IK=0, NK+1
        IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
          DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / DEPTH
        ELSE
          DSDD(IK) = 0.
        END IF
      END DO
      FDDMAX=0
      DO ITH=1, NTH
        FDDMAX = MAX ( FDDMAX , ABS(ESIN(ITH)*eDDDX - ECOS(ITH)*eDDDY ) )
      END DO
      DO IK=1, NK
        FRK(IK) = FACTH * DSDD(IK) / WN(IK,ISEA)
        !FRK(IK) = FRK(IK) / MAX ( 1. , FRK(IK)*FDDMAX/CTMAX )
        FRG(IK) = FDG * CG(IK,ISEA)
      END DO
      DO ISP=1, NSPEC
        VCFLT(ISP) = FRG(MAPWN(ISP)) * ECOS(ISP) +                     &
           FRK(MAPWN(ISP)) * ( ESIN(ISP)*eDDDX - ECOS(ISP)*eDDDY )
      END DO
!
      IF ( FLCUR ) THEN
        eDCXDX=DCXDX(IY,IX)
        eDCXDY=DCXDY(IY,IX)
        eDCYDX=DCYDX(IY,IX)
        eDCYDY=DCYDY(IY,IX)
        DCYX   = FACTH *   eDCYDX
        DCXXYY = FACTH * ( eDCXDX - eDCYDY )
        DCXY   = FACTH *   eDCXDY
        DO ISP=1, NSPEC
          VCFLT(ISP) = VCFLT(ISP) + ES2(ISP)*DCYX  + ESC(ISP)*DCXXYY - EC2(ISP)*DCXY
        END DO
      END IF
      DO ISP=1,NSPEC
        CAD(ISP)=DBLE(VCFLT(ISP))
      END DO
!/
!/ End of JACOBI_INIT ------------------------------------------------ /
!/
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!
      SUBROUTINE PROP_REFRACTION_PR3(IP, ISEA, DTG, CAD, DoLimiter)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/   01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Compute refraction part in matrix alternative approach
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS, ONLY : LPDLIB
      USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
                          EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
                          CTMAX, DMIN, DTH, CTHG0S, MAPSF
      USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, DDDX,   &
                          DDDY, DW
      USE W3IDATMD, ONLY: FLCUR
      USE W3ODATMD, only : IAPROC
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      REAL, intent(out) :: CAD(NSPEC)
      INTEGER, intent(in) :: ISEA, IP
      REAL, intent(in) :: DTG
      logical, intent(in) :: DoLimiter
      INTEGER :: ISP, IK, ITH, IX, IY
      REAL :: FRK(NK), FRG(NK), DSDD(0:NK+1)
      REAL :: FACTH, DCXY, DCYX, DCXXYY, DTTST
      REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eDDDX, eDDDY, eCTHG0
      REAL :: VCFLT(NSPEC), DEPTH, FDG
      REAL :: FDDMAX, CFLTHMAX, VELNOFILT, CTMAX_eff
      IX=MAPSF(ISEA,1)
      IY=MAPSF(ISEA,2)
      IF (LPDLIB) THEN
        eDDDX=DDDX(1,IP)
        eDDDY=DDDY(1,IP)
      ELSE
        eDDDX=DDDX(IY,IX)
        eDDDY=DDDY(IY,IX)
      ENDIF
      eCTHG0 = CTHG0S(ISEA)
      FACTH  = 1.0 / DTH
      !
      FDG    = FACTH * eCTHG0
      DEPTH  = MAX ( DMIN , DW(ISEA) )
      DO IK=0, NK+1
        IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
          DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / DEPTH
        ELSE
          DSDD(IK) = 0.
        END IF
      END DO
      DO IK=1, NK
        FRK(IK) = FACTH * DSDD(IK) / WN(IK,ISEA)
        FRG(IK) = FDG * CG(IK,ISEA)
      END DO
      IF (FLCUR) THEN
        IF (LPDLIB) THEN
          eDCXDX = DCXDX(1,IP)
          eDCXDY = DCXDY(1,IP)
          eDCYDX = DCYDX(1,IP)
          eDCYDY = DCYDY(1,IP)
        ELSE
          eDCXDX = DCXDX(IY,IX)
          eDCXDY = DCXDY(IY,IX)
          eDCYDX = DCYDX(IY,IX)
          eDCYDY = DCYDY(IY,IX)
        ENDIF
        DCYX   = FACTH *   eDCYDX
        DCXXYY = FACTH * ( eDCXDX - eDCYDY )
        DCXY   = FACTH *   eDCXDY
        DO ISP=1, NSPEC
          VCFLT(ISP) = ES2(ISP)*DCYX  + ESC(ISP)*DCXXYY - EC2(ISP)*DCXY
        END DO
      ELSE
        VCFLT=0
      END IF
!
      CTMAX_eff=CTMAX/DTG
      DO ISP=1, NSPEC
        VELNOFILT = VCFLT(ISP)                                       &
           + FRG(MAPWN(ISP)) * ECOS(ISP)                             &
           + FRK(MAPWN(ISP)) * (ESIN(ISP)*eDDDX - ECOS(ISP)*eDDDY)
!
! Puts filtering on total velocity (including currents and great circle effects)
! the filtering limits VCFLT to be less than CTMAX
! this modification was proposed by F. Ardhuin 2011/03/06
!
        IF (DoLimiter .eqv. .TRUE.) THEN
          VCFLT(ISP)=SIGN(MIN(ABS(VELNOFILT),CTMAX_eff),VELNOFILT)
        ELSE
          VCFLT(ISP)=VELNOFILT
        END IF
      END DO
      DO ISP=1,NSPEC
        CAD(ISP)=DBLE(VCFLT(ISP))
      END DO
!/
!/ End of JACOBI_INIT ------------------------------------------------ /
!/
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      SUBROUTINE PROP_FREQ_SHIFT(IP, ISEA, CAS, DMM, DTG)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/   01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Compute freq. shift in matrix
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS, ONLY : LPDLIB
      USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
                          EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
                          CTMAX, DMIN, DTH, MAPSF
      USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, CX, CY, DDDX, DDDY, DW
      USE W3ODATMD, only : IAPROC
      IMPLICIT NONE
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER, intent(in) :: ISEA, IP
      REAL, intent(out) :: DMM(0:NK2)
      REAL, intent(in) :: DTG
      REAL, intent(out) :: CAS(NSPEC)
      REAL :: DB(NK2), DSDD(0:NK+1)
      REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eCX, eCY, eDDDX, EDDDY
      REAL :: DCXX, DCXYYX, DCYY, FKD, FACK
      REAL :: VELNOFILT, VELFAC, DEPTH
      REAL :: CFLK(NK2,NTH), FKC(NTH), FKD0
      INTEGER :: IK, ITH, ISP, IY, IX
!
      IF (LPDLIB) THEN
        eDCXDX = DCXDX(1,IP)
        eDCXDY = DCXDY(1,IP)
        eDCYDX = DCYDX(1,IP)
        eDCYDY = DCYDY(1,IP)
        eDDDX  = DDDX(1,IP)
        eDDDY  = DDDY(1,IP)
      ELSE
        IX=MAPSF(ISEA,1)
        IY=MAPSF(ISEA,2)
        eDCXDX=DCXDX(IY,IX)
        eDCXDY=DCXDY(IY,IX)
        eDCYDX=DCYDX(IY,IX)
        eDCYDY=DCYDY(IY,IX)
        eDDDX=DDDX(IY,IX)
        eDDDY=DDDY(IY,IX)
      ENDIF
      eCX=CX(ISEA)
      eCY=CY(ISEA)
      DCXX   =  -   eDCXDX
      DCXYYX =  - ( eDCXDY + eDCYDX )
      DCYY   =  -   eDCYDY
      FKD    =    ( eCX*eDDDX + eCY*eDDDY )
      FACK = DTG
      DO ITH=1, NTH
        FKC(ITH) = EC2(ITH)*DCXX + ESC(ITH)*DCXYYX + ES2(ITH)*DCYY
      END DO
      DO IK=0, NK
        DB(IK+1) = DSIP(IK) / CG(IK,ISEA)
        DMM(IK+1) = DBLE(WN(IK+1,ISEA) - WN(IK,ISEA))
      END DO
      DB(NK+2) = DSIP(NK+1) / CG(NK+1,ISEA)
      DMM(NK+2) = ZERO
      DMM(0)=DMM(1)
!
      DEPTH  = MAX ( DMIN , DW(ISEA) )
      DO IK=0, NK+1
        IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
          DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / DEPTH
        ELSE
          DSDD(IK) = 0.
        END IF
      END DO
      DO IK=0, NK+1
        FKD0   = FKD / CG(IK,ISEA) * DSDD(IK)
        VELFAC =  FACK/DB(IK+1)
        DO ITH=1, NTH
          VELNOFILT = ( FKD0 + WN(IK,ISEA)*FKC(ITH) ) * VELFAC
          CFLK(IK+1,ITH) = VELNOFILT/VELFAC
        END DO
      END DO
      DO IK=1,NK
        DO ITH=1,NTH
          ISP=ITH + (IK-1)*NTH
          CAS(ISP)=DBLE(CFLK(IK,ITH))
        END DO
      END DO
!/
!/ End of JACOBI_INIT ------------------------------------------------ /
!/
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      SUBROUTINE PROP_FREQ_SHIFT_M2(IP, ISEA, CWNB_M2, DWNI_M2, DTG)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/   01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Compute freq. shift alternative approach
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE CONSTANTS, ONLY : LPDLIB
      USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
                          EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
                          CTMAX, DMIN, DTH, MAPSF
      USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, CX, CY, DDDX, DDDY, DW
      USE W3ODATMD, only : IAPROC
 
      IMPLICIT NONE
 
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
 
      INTEGER, intent(in) :: ISEA, IP
      REAL, intent(out) :: CWNB_M2(1-NTH:NSPEC)
      REAL, intent(out) :: DWNI_M2(NK)
      REAL, intent(in) :: DTG
      !
      REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eCX, eCY, eDDDX, EDDDY
      REAL :: DCXX, DCXYYX, DCYY, FKD, FACK
      REAL :: DEPTH
      REAL :: FKC(NTH), FKD0
      REAL :: VCWN(1-NTH:NSPEC+NTH)
      REAL :: DSDD(0:NK+1)
      REAL :: sumDiff, sumDiff1, sumDiff2, sumDiff3
      REAL :: sumDiff0, sumDiff4, sumDiff5
      INTEGER :: IK, ITH, ISP, IY, IX
 
!/ ------------------------------------------------------------------- /
 
 
      IF (LPDLIB) THEN
        eDCXDX = DCXDX(1,IP)
        eDCXDY = DCXDY(1,IP)
        eDCYDX = DCYDX(1,IP)
        eDCYDY = DCYDY(1,IP)
        eDDDX  = DDDX(1,IP)
        eDDDY  = DDDY(1,IP)
      ELSE
        IX=MAPSF(ISEA,1)
        IY=MAPSF(ISEA,2)
        eDCXDX=DCXDX(IY,IX)
        eDCXDY=DCXDY(IY,IX)
        eDCYDX=DCYDX(IY,IX)
        eDCYDY=DCYDY(IY,IX)
        eDDDX=DDDX(IY,IX)
        eDDDY=DDDY(IY,IX)
      ENDIF
 
      eCX = CX(ISEA)
      eCY = CY(ISEA)
      FACK = DTG
      DCXX   =  - FACK *   eDCXDX
      DCXYYX =  - FACK * ( eDCXDY + eDCYDX )
      DCYY   =  - FACK *   eDCYDY
      FKD    =    FACK * ( eCX*eDDDX + eCY*eDDDY )
 
      DO ITH=1, NTH
        FKC(ITH) = EC2(ITH)*DCXX + ESC(ITH)*DCXYYX + ES2(ITH)*DCYY
      END DO
!
      DEPTH  = MAX ( DMIN , DW(ISEA) )
      DO IK=0, NK+1
        IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
          DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / DEPTH
        ELSE
          DSDD(IK) = 0.
        END IF
      END DO
      ISP = -NTH
      DO IK=0, NK+1
        FKD0   = FKD / CG(IK,ISEA) * DSDD(IK)
        DO ITH=1, NTH
          ISP = ISP + 1
          VCWN(ISP) = FKD0 + WN(IK,ISEA)*FKC(ITH)
        END DO
      END DO
 
      sumDiff=0
      DO ISP=1-NTH,NSPEC
        CWNB_M2(ISP) = DBLE(0.5 * ( VCWN(ISP) + VCWN(ISP+NTH) ))
        sumDiff = sumDiff + MAX(CWNB_M2(ISP), ZERO)
      END DO
      DO IK=1,NK
        DWNI_M2(IK) = DBLE( CG(IK,ISEA) / DSIP(IK) )
      END DO
!/
!/ End of JACOBI_INIT ------------------------------------------------ /
!/
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      SUBROUTINE SYNCHRONIZE_IPGL_ETC_ARRAY(IMOD, IsMulti)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/   01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Sync global local arrays
!  2. Method :
! 		All the process need to have IPGL_tot and IPGL_TO_PROC
! 		This is especially the case for the output process.
! 		So we need some painful exportation business
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
          IMPLICIT NONE
          INTEGER, intent(in) :: IMOD
          logical, intent(in) :: IsMulti
 
 
!/
!/ End of JACOBI_INIT ------------------------------------------------ /
!/
      END SUBROUTINE
!/ ....................----------------------------------------------- /
      SUBROUTINE SET_UP_NSEAL_NSEALM(NSEALout, NSEALMout)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/   01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Setup nseal, nsealm in contect of pdlib
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
!/ ------------------------------------------------------------------- /
      USE W3ADATMD, ONLY: MPI_COMM_WAVE, MPI_COMM_WCMP
      USE CONSTANTS, ONLY : LPDLIB
      USE W3GDATMD, ONLY: NSEA
      USE W3ODATMD, ONLY: NTPROC, NAPROC, IAPROC
      IMPLICIT NONE
      INTEGER, intent(out) :: NSEALout, NSEALMout
!/ Local parameters
!/
!/
!/ ------------------------------------------------------------------- /
!/
!!/PDLIB      WRITE(*,*) 'LPDLIB=', LPDLIB
!!/PDLIB      WRITE(*,*) 'GTYPE=', GTYPE, ' UNGTYPE=', UNGTYPE
 
!
      IF (.NOT. LPDLIB ) THEN
        IF ( IAPROC .LE. NAPROC ) THEN
          NSEALout  = 1 + (NSEA-IAPROC)/NAPROC
        ELSE
          NSEALout  = 0
        END IF
        NSEALMout = 1 + (NSEA-1)/NAPROC
      ELSE
      ENDIF
!/
!/ End of JACOBI_INIT ------------------------------------------------ /
!/
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      SUBROUTINE INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
!/ ------------------------------------------------------------------- /
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/   01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Set Jsea for all schemes
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
      USE W3GDATMD, ONLY: GTYPE, UNGTYPE, MAPSF
      USE CONSTANTS, ONLY : LPDLIB
      IMPLICIT NONE
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
!/
!/ ------------------------------------------------------------------- /
      INTEGER, intent(in) :: ISEA
      INTEGER, intent(out) :: JSEA, ISPROC
      INTEGER IP_glob
!!/DEBUG      WRITE(740+IAPROC,*) 'PDLIB=', PDLIB
!!/DEBUG      WRITE(740+IAPROC,*) 'GTYPE=', GTYPE, ' UNGTYPE=', UNGTYPE
!!/DEBUG      FLUSH(740+IAPROC)
      IF (.NOT. LPDLIB) THEN
        JSEA   = 1 + (ISEA-1)/NAPROC
        ISPROC = ISEA - (JSEA-1)*NAPROC
      ELSE
      ENDIF
!/
!/ End of JACOBI_INIT ------------------------------------------------ /
!/
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      SUBROUTINE GET_JSEA_IBELONG(ISEA, JSEA, IBELONG)
!/ ------------------------------------------------------------------- /
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/   01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Set belongings of jsea in context of pdlib
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
      USE W3GDATMD, ONLY: GTYPE, UNGTYPE, MAPSF
      USE CONSTANTS, ONLY : LPDLIB
      IMPLICIT NONE
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER, intent(in) :: ISEA
      INTEGER, intent(out) :: JSEA, IBELONG
      INTEGER ISPROC, IX, JX
      IF (.NOT. LPDLIB) THEN
        JSEA   = 1 + (ISEA-1)/NAPROC
        ISPROC = ISEA - (JSEA-1)*NAPROC
        IF (ISPROC .eq. IAPROC) THEN
          IBELONG=1
        ELSE
          IBELONG=0
        END IF
      ELSE
      ENDIF
!/
!/ End of INIT_GET_ISEA ---------------------------------------------- /
!/
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      SUBROUTINE INIT_GET_ISEA(ISEA, JSEA)
!/ ------------------------------------------------------------------- /
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/   01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Set Isea for all schemes
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
      USE W3GDATMD, ONLY: GTYPE, UNGTYPE
      USE CONSTANTS, ONLY : LPDLIB
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
!/ ------------------------------------------------------------------- /
!/
!/
!/ ------------------------------------------------------------------- /
!/
!/ ------------------------------------------------------------------- /
!
      USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
      USE W3GDATMD, ONLY: GTYPE, UNGTYPE
      USE CONSTANTS, ONLY : LPDLIB
      IMPLICIT NONE
      INTEGER, intent(in) :: JSEA
      INTEGER, intent(out) :: ISEA
          IF (.NOT. LPDLIB) THEN
            ISEA         = IAPROC + (JSEA-1)*NAPROC
          ELSE
          ENDIF
!/
!/ End of INIT_GET_ISEA ------------------------------------------------ /
!/
      END SUBROUTINE
!**********************************************************************
!*  An array of size (NSEA) is send but only the (1:NSEAL) values     *
!*  are correct. The program synchonizes everything on all nodes.     *
!**********************************************************************
      SUBROUTINE SYNCHRONIZE_GLOBAL_ARRAY(TheVar)
!/ ------------------------------------------------------------------- /
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/   01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Sync global array in context of pdlib
!  2. Method :
!			An array of size (NSEA) is send but only the (1:NSEAL) values
! 			are correct. The program synchonizes everything on all nodes.
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE W3GDATMD, ONLY: NSEAL, NSEA, NX
      IMPLICIT NONE
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
!/
!/ ------------------------------------------------------------------- /
!/
      INCLUDE "mpif.h"
      INTEGER ISEA, JSEA, Status(NX), rStatus(NX)
      INTEGER IPROC, I, ierr, IP, IX, IP_glob
      REAL*8, intent(inout) :: TheVar(NX)
      REAL*8  rVect(NX)
      Status=0
!/
!/ End of JACOBI_INIT ------------------------------------------------ /
!/
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      END MODULE W3PARALL
!/ ------------------------------------------------------------------- /
 
