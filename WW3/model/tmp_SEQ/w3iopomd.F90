#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3IOPOMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         05-Jun-2018 |
!/                  +-----------------------------------+
!/
!/    25-Jan-2001 : Origination.                        ( version 2.00 )
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    11-Jun-2001 : Clean-up.                           ( version 2.11 )
!/    10-Nov-2004 : Multiple grid version.              ( version 3.06 )
!/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    25-Jul-2006 : Adding grid ID per point.           ( version 3.10 )
!/    01-May-2007 : Move O7a output from W3INIT.        ( version 3.11 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    29-Oct-2010 : Implement unstructured grid         ( version 3.14.4 )
!/                  (A. Roland and F. Ardhuin)
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    12-Jun-2012 : Add /RTD option or rotated grid option.
!/                  (Jian-Guo Li)                       ( version 4.06 )
!/    02-Sep-2012 : Clean up of open BC for UG grids    ( version 4.07 )
!/    25-Feb-2013 : ITOUT=0 bug correction for UG grids ( version 4.08 )
!/    11-Nov-2013 : SMC and rotated grid incorporated in the main
!/                  trunk                               ( version 4.13 )
!/    05-Jun-2018 : Add SETUP                           ( version 6.04 )
!/    04-Oct-2019 : Optional one file per output stride ( version 7.00 )
!/                  (R. Padilla-Hernandez & J.H. Alves)
!/
!/    Copyright 2009 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Process point output.
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      VEROPT    C*10  Private  Point output file version number.
!      IDSTR     C*32  Private  Point output file ID string.
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3IOPP    Subr. Public   Preprocessing of point output req.
!      W3IOPE    Subr. Public   Extract point data from grid.
!      W3IOPO    Subr. Public   Point data IO.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETO    Subr. W3ODATMD Data structure management.
!      W3SETG    Subr. W3GDATMD Data structure management.
!      W3SETW    Subr. W3WDATMD Data structure management.
!      W3DMO2    Subr. W3ODATMD Data structure management.
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      EXTCDE    Subr. W3SERVMD Program abort with exit code.
!      MPI_STARTALL, MPIWAITALL
!                Subr.          MPI persistent communication routines.
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!     - Allocation of allocatable arrays takes place at different
!       places throughout the code, in W3IOPP on write, and in
!       W3IOPO on read.
!
!  6. Switches :
!
!       !/S     Enable subroutine tracing.
!       !/T     Enable test output.
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!       !/MPI   MPI message passing.
!
!       !/O7a   Diagnostic output for output points.
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
      PUBLIC
!/
!/ Private parameter statements (ID strings)
!/
      CHARACTER(LEN=10), PARAMETER, PRIVATE :: VEROPT = '2020-07-30'
      CHARACTER(LEN=31), PARAMETER, PRIVATE ::                        &
                           IDSTR = 'WAVEWATCH III POINT OUTPUT FILE'
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3IOPP ( NPT, XPT, YPT, PNAMES, IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         02-Sep-2012 |
!/                  +-----------------------------------+
!/
!/    14-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    30-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    09-Nov-2004 : Multiple grid version.              ( version 3.06 )
!/    25-Jul-2006 : Adding grid ID per point.           ( version 3.10 )
!/    01-May-2007 : Move O7a output from W3INIT.        ( version 3.11 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    12-Jun-2012 : Add /RTD option or rotated grid option.
!/                  (Jian-Guo Li)                       ( version 4.06 )
!/    02-Sep-2012 : Clean up of open BC for UG grids    ( version 4.07 )
!/    01-Mar-2018 : Add option to unrotate spectra      ( version 6.02 )
!/                  from RTD grid models
!/
!  1. Purpose :
!
!     Preprocessing of point output.
!
!  2. Method :
!
!     Check location of points in grid and calculate interpolation
!     factors.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NPT     Int.   I   Number of output points in input.
!       XPT     R.A.  I/O  X (longitude) coordinates of output points.
!       YPT     R.A.  I/O  Id. Y.
!       PNAMES  C*40   I   Names of output points.
!       IMOD    Int.   I   Grid ID number.
!     ----------------------------------------------------------------
!
!     Local data
!     ----------------------------------------------------------------
!       ACC     Real  "Accuracy" factor to determine if output point
!                     is grid point.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3INIT    Subr. W3INITMD Wave model initialization routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     - Warnings for points out of the grid or on land.
!
!  7. Remarks :
!
!     - The output points are obtained by bi-linear interpolation from
!       the spectra at the grid points. Given the possibility of ice
!       coverage, the actual interpolation factors can only be
!       determined at the actual output time. Hence only the basic
!       bilinear interpolation factors are stored.
!     - Implementation of the /O7a diagnostic output section is
!       currently incomplete and non-functional for curvilinear grids
!       and/or tripole grids
!
!  8. Structure :
!
!     -------------------------------------------
!      Determine grid range
!      do for all defined points
!      -----------------------------------------
!        Check if point within grid
!        Calculate interpolation data
!        Check if point not on land
!        Store interpolation data
!     -------------------------------------------
!
!  9. Switches :
!
!       !/S     Enable subroutine tracing.
!       !/T     Test output.
!
!       !/O7a   Diagnostic output for output points.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GSRUMD
      USE W3GDATMD, ONLY: NTH, NK, NSPEC, NX, NY, X0, Y0, SX, GSU,&
                          RLGTYPE, CLGTYPE, UNGTYPE, GTYPE, FLAGLL,   &
                          ICLOSE,ICLOSE_NONE,ICLOSE_SMPL,ICLOSE_TRPL, &
                          MAPSTA, MAPFS, FILEXT, ZB, TRNX, TRNY
      USE W3GDATMD, ONLY: XYB, TRIGP,MAXX, MAXY, DXYMAX
      USE W3ODATMD, ONLY: W3DMO2
      USE W3ODATMD, ONLY: NDSE, NDST, IAPROC, NAPERR, NAPOUT, SCREEN, &
                          NOPTS, PTLOC, PTNME, GRDID, IPTINT, PTIFAC
      USE W3SERVMD, ONLY: EXTCDE
      USE W3TRIAMD
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)          :: NPT, IMOD
      REAL, INTENT(INOUT)          :: XPT(NPT), YPT(NPT)
      CHARACTER(LEN=40),INTENT(IN) :: PNAMES(NPT)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      LOGICAL                 :: INGRID
      INTEGER                 :: IPT, J, K
      INTEGER                 :: IX1, IY1, IXS, IYS
      INTEGER                 :: IX(4), IY(4)   ! Indices of points used in interp.
      REAL                    :: RD(4)          ! Interpolation coefficient
      REAL, PARAMETER         :: ACC = 0.05
      REAL                    :: FACTOR
      INTEGER                 :: ITOUT          ! Triangle index in unstructured grids
!
!/
!/
!/ ------------------------------------------------------------------- /
!/
!
      IF ( FLAGLL ) THEN
          FACTOR = 1.
        ELSE
          FACTOR = 1.E-3
        END IF
!
      CALL W3DMO2 ( IMOD, NDSE, NDST, NPT )
      GRDID  = FILEXT
!
      NOPTS  = 0
!
!   Removed by F.A. 2011/04/04  /T      CALL W3GSUP( GSU, NDST )
!
! Loop over output points
!
      DO IPT=1, NPT
!
!     Check if point within grid and compute interpolation weights
!
        IF (GTYPE .NE. UNGTYPE) THEN
          INGRID = W3GRMP( GSU, XPT(IPT), YPT(IPT), IX, IY, RD )
        ELSE
          CALL IS_IN_UNGRID(IMOD, XPT(IPT), YPT(IPT), itout, IX, IY, RD)
          INGRID = (ITOUT.GT.0)
          END IF
!
        IF ( .NOT.INGRID ) THEN
          IF ( IAPROC .EQ. NAPERR ) THEN
            IF ( FLAGLL ) THEN
              WRITE (NDSE,1000) XPT(IPT), YPT(IPT), PNAMES(IPT)
            ELSE
              WRITE (NDSE,1001) XPT(IPT), YPT(IPT), PNAMES(IPT)
              END IF
            END IF
          CYCLE
          END IF
!
!     Check if point not on land
!
        IF ( MAPSTA(IY(1),IX(1)) .EQ. 0 .AND. &
             MAPSTA(IY(2),IX(2)) .EQ. 0 .AND. &
             MAPSTA(IY(3),IX(3)) .EQ. 0 .AND. &
             MAPSTA(IY(4),IX(4)) .EQ. 0 ) THEN
          IF ( IAPROC .EQ. NAPERR ) THEN
            IF ( FLAGLL ) THEN
              WRITE (NDSE,1002) XPT(IPT), YPT(IPT), PNAMES(IPT)
            ELSE
              WRITE (NDSE,1003) XPT(IPT), YPT(IPT), PNAMES(IPT)
              END IF
            END IF
          CYCLE
          END IF
!
!     Store interpolation data
!
        NOPTS  = NOPTS + 1
!
        PTLOC (1,NOPTS) = XPT(IPT)
        PTLOC (2,NOPTS) = YPT(IPT)
!
        DO K = 1,4
          IPTINT(1,K,NOPTS) = IX(K)
          IPTINT(2,K,NOPTS) = IY(K)
          PTIFAC(K,NOPTS) = RD(K)
          END DO
 
        PTNME(NOPTS) = PNAMES(IPT)
!
        END DO ! End loop over output points (IPT).
!
! Diagnostic output
!
 
!
      RETURN
!
! Formats
!
 1000 FORMAT (/' *** WAVEWATCH-III WARNING :'/                   &
               '     OUTPUT POINT OUT OF GRID : ',2F10.3,2X,A/   &
               '     POINT SKIPPPED '/)
 1001 FORMAT (/' *** WAVEWATCH-III WARNING :'/                   &
               '     OUTPUT POINT OUT OF GRID : ',2E10.3,2X,A/   &
               '     POINT SKIPPPED '/)
!
 1002 FORMAT (/' *** WAVEWATCH-III WARNING :'/                   &
               '     OUTPUT POINT ON LAND : ',2F10.3,2X,A/       &
               '     POINT SKIPPPED '/)
 1003 FORMAT (/' *** WAVEWATCH-III WARNING :'/                   &
               '     OUTPUT POINT ON LAND : ',2E10.3,2X,A/       &
               '     POINT SKIPPPED '/)
!
!/
!/ End of W3IOPP ----------------------------------------------------- /
!/
      END SUBROUTINE W3IOPP
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3IOPE ( A )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         12-Jun-2012 |
!/                  +-----------------------------------+
!/
!/    12-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    25-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    11-Jun-2001 : Clean-up.                           ( version 2.11 )
!/    09-Nov-2004 : Multiple grid version.              ( version 3.06 )
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.4 )
!/                  (A. Roland and F. Ardhuin)
!/    12-Jun-2012 : Add /RTD option or rotated grid option.
!/                  (Jian-Guo Li)                       ( version 4.06 )
!/    01-Mar-2018 : Add option to unrotate spectra      ( version 6.02 )
!/                  from RTD grid models
!/
!  1. Purpose :
!
!     Extract point output data and store in output COMMONs. This
!     action is taken from an earlier version of W3IOPO so that the
!     point output postprocessor does not need the full sea-point
!     grid to be able to run.
!       Note that the output spectrum is F(f,theta). Interpolation
!     is performed for this spectrum.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       A       R.A.   I   Action spectra on storage grid.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!     - To allow for dynamic ice edges, interpolation factors are
!       calculated for every time step separately.
!     - Wind current and depth data are interpolated ignoring ice,
!       spectrum is interpolated removing ice points.
!     - Spectra are left in par list to allow for change of shape of
!       arrays.
!     - IMOD is not passed to this routine. Since it is used only
!       in W3WAVE, it is assumed that the pointer are set
!       appropriately outside this routine.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/SHRD  Switch for shared / distributed memory architecture.
!     !/DIST  Id.
!     !/MPI   Switch for message passing method.
!
!     !/S     Enable subroutine tracing.
!     !/T     Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
      USE W3GDATMD, ONLY: NK, NTH, SIG, NX, NY, NSEA, NSEAL,          &
                          MAPSTA, MAPFS
      USE W3WDATMD, ONLY: ICE, ICEH, ICEF
      USE W3ADATMD, ONLY: CG, DW, UA, UD, AS, CX, CY,                 &
                          SP => SPPNT
      USE W3ODATMD, ONLY: NDST, NOPTS, IPTINT, PTIFAC, IL, IW, II,    &
                          DPO, WAO, WDO, ASO, CAO, CDO, ICEO, ICEHO,  &
                          ICEFO, SPCO, NAPROC
      USE W3SERVMD, ONLY: EXTCDE
!
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(IN)        :: A(NTH,NK,0:NSEAL)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: I, IX1, IY1, IX(4), IY(4), J, IS(4), &
                                 IM(4), IK, ITH, ISP
      REAL                    :: RD(4), RDS, RDI, FACRD,              &
                                 WNDX, WNDY, CURX, CURY, FAC1(NK),    &
                                 FAC2(NK), FAC3(NK), FAC4(NK)
      INTEGER                 :: JSEA, ISEA
!/
!/ ------------------------------------------------------------------- /
!/
!
      CX(0)  = 0.
      CY(0)  = 0.
!
! Loop over spectra -------------------------------------------------- *
!
      DO I=1, NOPTS
!
! Unpack interpolation data
!
        IX(:)  = IPTINT(1,:,I)
        IY(:)  = IPTINT(2,:,I)
        RD(:)  = PTIFAC(:,I)
!
! Correct for land and ice and get sea point counters
!
        IL(I)  = 0
        IW(I)  = 0
        II(I)  = 0
        RDS    = 0.
        RDI    = 0.
!
        DO J=1, 4
          IS(J)  = MAPFS (IY(J),IX(J))
          IM(J)  = MAPSTA(IY(J),IX(J))
          IF ( IM(J).GT.0 ) THEN
              IW(I)  = IW(I) + 1
              RDS    = RDS + RD(J)
            ELSE
              IF ( IM(J).LT.0 ) THEN
                  II(I)  = II(I) + 1
                  RDI    = RDI + RD(J)
                ELSE
                  IL(I)  = IL(I) + 1
                  RD(J)  = 0.
                END IF
            END IF
          END DO
!
! Depth, wind and current, ignore ice
!
        IF ( RDS+RDI .GT. 1.E-7 ) THEN
            FACRD  = 1. / (RDS+RDI)
            RD     = RD * FACRD
          END IF
!
! Interpolate ice depth wind and current
!
        IF (.NOT. LPDLIB) THEN
          ICEFO(I) = 0
          DO J=1, 4
            ISEA = MAPFS(IY(J),IX(J))
          JSEA = ISEA
            ICEFO(I) = ICEFO(I) + RD(J)*ICEF(JSEA)
          END DO
        ELSE
          ICEFO(I) = RD(1)*ICEF(IS(1)) + RD(2)*ICEF(IS(2)) +          &
                   RD(3)*ICEF(IS(3)) + RD(4)*ICEF(IS(4))
        END IF
 
        ICEO(I) = RD(1)*ICE(IS(1)) + RD(2)*ICE(IS(2)) +               &
                 RD(3)*ICE(IS(3)) + RD(4)*ICE(IS(4))
 
        ICEHO(I) = RD(1)*ICEH(IS(1)) + RD(2)*ICEH(IS(2)) +            &
                 RD(3)*ICEH(IS(3)) + RD(4)*ICEH(IS(4))
!
        DPO(I) = RD(1)*DW(IS(1)) + RD(2)*DW(IS(2)) +                  &
                 RD(3)*DW(IS(3)) + RD(4)*DW(IS(4))
!
        WNDX   = RD(1) * UA(IS(1)) * COS(UD(IS(1))) +                 &
                 RD(2) * UA(IS(2)) * COS(UD(IS(2))) +                 &
                 RD(3) * UA(IS(3)) * COS(UD(IS(3))) +                 &
                 RD(4) * UA(IS(4)) * COS(UD(IS(4)))
        WNDY   = RD(1) * UA(IS(1)) * SIN(UD(IS(1))) +                 &
                 RD(2) * UA(IS(2)) * SIN(UD(IS(2))) +                 &
                 RD(3) * UA(IS(3)) * SIN(UD(IS(3))) +                 &
                 RD(4) * UA(IS(4)) * SIN(UD(IS(4)))
!
        WAO(I) = SQRT ( WNDX**2 + WNDY**2 )
        IF ( WAO(I).GT.1.E-7 ) THEN
            WDO(I) = ATAN2(WNDY,WNDX)
          ELSE
            WDO(I) = 0.
          END IF
!
        ASO(I) = RD(1)*AS(IS(1)) + RD(2)*AS(IS(2)) +                  &
                 RD(3)*AS(IS(3)) + RD(4)*AS(IS(4))
!
        CURX   = RD(1)*CX(IS(1)) + RD(2)*CX(IS(2)) +                  &
                 RD(3)*CX(IS(3)) + RD(4)*CX(IS(4))
        CURY   = RD(1)*CY(IS(1)) + RD(2)*CY(IS(2)) +                  &
                 RD(3)*CY(IS(3)) + RD(4)*CY(IS(4))
!
        CAO(I) = SQRT ( CURX**2 + CURY**2 )
        IF ( CAO(I).GT.1.E-7 ) THEN
            CDO(I) = ATAN2(CURY,CURX)
          ELSE
            CDO(I) = 0.
          END IF
!
! Interp. weights for spectra, no ice points (spectra by def. zero)
!
        IF ( RDS .GT. 1.E-7 ) THEN
            FACRD  = (RDS+RDI) / RDS
            RD     = RD * FACRD
          END IF
!
! Extract spectra, shared memory version
!        (done in separate step for MPP compatibility)
!
        DO J=1, 4
          DO IK=1, NK
            DO ITH=1, NTH
              SP(ITH,IK,J) = A(ITH,IK,IS(J))
              END DO
            END DO
          END DO
!
! Extract spectra, distributed memory version(s)
!
! Interpolate spectrum
!
        DO IK=1, NK
          FAC1(IK) = TPI * SIG(IK) / CG(IK,IS(1))
          FAC2(IK) = TPI * SIG(IK) / CG(IK,IS(2))
          FAC3(IK) = TPI * SIG(IK) / CG(IK,IS(3))
          FAC4(IK) = TPI * SIG(IK) / CG(IK,IS(4))
          END DO
!
        DO IK=1,NK
          DO ITH=1,NTH
            ISP    = ITH + (IK-1)*NTH
            SPCO(ISP,I) = RD(1) * SP(ITH,IK,1) * FAC1(IK)             &
                        + RD(2) * SP(ITH,IK,2) * FAC2(IK)             &
                        + RD(3) * SP(ITH,IK,3) * FAC3(IK)             &
                        + RD(4) * SP(ITH,IK,4) * FAC4(IK)
            END DO
          END DO
!
 
! FA COMMENTED OUT: BUG
!At line 1974 of file w3arrymd.f90
!Fortran runtime error: Index '52' of dimension 1 of array 'pnum2' above upper bound of 51
!
        END DO
!
      RETURN
!
! Formats
!
!/
!/ End of W3IOPE ----------------------------------------------------- /
!/
      END SUBROUTINE W3IOPE
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3IOPO ( INXOUT, NDSOP, IOTST, IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         25-Jul-2006 |
!/                  +-----------------------------------+
!/
!/    07-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    30-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    10-Nov-2004 : Multiple grid version.              ( version 3.06 )
!/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    25-Jul-2006 : Adding grid ID per point.           ( version 3.10 )
!/    27-Aug-2015 : Adding interpolation for the ice.   ( version 5.10 )
!/
!  1. Purpose :
!
!     Read/write point output.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       INXOUT  C*(*)  I   Test string for read/write, valid are:
!                          'READ' and 'WRITE'.
!       NDSOP   Int.   I   File unit number.
!       IOTST   Int.   O   Test indictor for reading.
!                           0 : Data read.
!                          -1 : Past end of file.
!       IMOD    I(O)   I   Model number for W3GDAT etc.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!      WW3_OUTP  Prog.   N/A    Postprocessing for point output.
!      GX_OUTP   Prog.   N/A    Grads postprocessing for point output.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       Tests on INXOUT, file status and on array dimensions.
!
!  7. Remarks :
!
!     - The output file has the pre-defined name 'out_pnt.FILEXT'.
!     - In MPP version of model data is supposed to be gatherd at the
!       correct processor before the routine is called.
!     - No error output filtering needed.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/SHRD  Switch for shared / distributed memory architecture.
!     !/DIST  Id.
!
!     !/S     Enable subroutine tracing.
!     !/T     Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: W3SETG
      USE W3WDATMD, ONLY: W3SETW
      USE W3ODATMD, ONLY: W3SETO, W3DMO2
!/
      USE W3GDATMD, ONLY: NTH, NK, NSPEC, FILEXT
      USE W3WDATMD, ONLY: TIME
      USE W3ODATMD, ONLY: NDST, NDSE, IPASS => IPASS2, NOPTS, IPTINT, &
                          IL, IW, II, PTLOC, PTIFAC, DPO, WAO, WDO,   &
                          ASO, CAO, CDO, SPCO, PTNME, O2INIT, FNMPRE, &
                          GRDID, ICEO, ICEHO, ICEFO
      USE W3ODATMD, ONLY :  OFILES
!/
!/
      USE W3SERVMD, ONLY: EXTCDE
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)           :: NDSOP
      INTEGER, INTENT(OUT)          :: IOTST
      INTEGER, INTENT(IN), OPTIONAL :: IMOD
      CHARACTER, INTENT(IN)         :: INXOUT*(*)
!/
!/ ------------------------------------------------------------------- /
!/ local parameters
!/
      INTEGER                 :: IGRD, IERR, MK, MTH, I, J
      LOGICAL,SAVE            :: WRITE
      CHARACTER(LEN=31)       :: IDTST
      CHARACTER(LEN=10)       :: VERTST
!/
      CHARACTER(LEN=15) :: TIMETAG
!/
!/ ------------------------------------------------------------------- /
!/
      IPASS  = IPASS + 1
      IOTST  = 0
!
! test input parameters ---------------------------------------------- *
!
      IF ( PRESENT(IMOD) ) THEN
          IGRD   = IMOD
        ELSE
          IGRD   = 1
        END IF
!
      CALL W3SETO ( IGRD, NDSE, NDST )
      CALL W3SETG ( IGRD, NDSE, NDST )
      CALL W3SETW ( IGRD, NDSE, NDST )
!
      IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE' ) THEN
          WRITE (NDSE,900) INXOUT
          CALL EXTCDE ( 1 )
        END IF
!
!      IF ( IPASS.EQ.1 ) THEN
      IF ( IPASS.EQ.1  .AND. OFILES(2) .EQ. 0) THEN
          WRITE  = INXOUT.EQ.'WRITE'
        ELSE
          IF ( WRITE .AND. INXOUT.EQ.'READ' ) THEN
              WRITE (NDSE,901) INXOUT
              CALL EXTCDE ( 2 )
            END IF
        END IF
!
! open file ---------------------------------------------------------- *
!
      IF ( IPASS.EQ.1 .AND. OFILES(2) .EQ. 0 ) THEN
!
          I      = LEN_TRIM(FILEXT)
          J      = LEN_TRIM(FNMPRE)
!
          IF ( WRITE ) THEN
              OPEN (NDSOP,FILE=FNMPRE(:J)//'out_pnt.'//FILEXT(:I),    &
                    FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
            ELSE
              OPEN (NDSOP,FILE=FNMPRE(:J)//'out_pnt.'//FILEXT(:I),    &
                    FORM='UNFORMATTED',ERR=800,IOSTAT=IERR,STATUS='OLD')
            END IF
!
          REWIND ( NDSOP )
!
! test info ---------------------------------------------------------- *
! ( IPASS = 1 )
!
          IF ( WRITE ) THEN
              WRITE (NDSOP)                                           &
                IDSTR, VEROPT, NK, NTH, NOPTS
            ELSE
              READ (NDSOP,END=801,ERR=802,IOSTAT=IERR)                &
                IDTST, VERTST, MK, MTH, NOPTS
!
              IF ( IDTST .NE. IDSTR ) THEN
                  WRITE (NDSE,902) IDTST, IDSTR
                  CALL EXTCDE ( 10 )
                END IF
              IF ( VERTST .NE. VEROPT ) THEN
                  WRITE (NDSE,903) VERTST, VEROPT
                  CALL EXTCDE ( 11 )
                END IF
              IF (NK.NE.MK .OR. NTH.NE.MTH) THEN
                  WRITE (NDSE,904) MK, MTH, NK, NTH
                  CALL EXTCDE ( 12 )
                END IF
              IF ( .NOT. O2INIT )                                     &
                  CALL W3DMO2 ( IGRD, NDSE, NDST, NOPTS )
            END IF
!
! Point specific info ------------------------------------------------ *
! ( IPASS = 1 )
!
          IF ( WRITE ) THEN
              WRITE (NDSOP)                                           &
                    ((PTLOC(J,I),J=1,2),I=1,NOPTS), (PTNME(I),I=1,NOPTS)
           ELSE
              READ  (NDSOP,END=801,ERR=802,IOSTAT=IERR)               &
                    ((PTLOC(J,I),J=1,2),I=1,NOPTS), (PTNME(I),I=1,NOPTS)
            END IF
!
        END IF
!
      IF ( IPASS.GE. 1  .AND. OFILES(2) .EQ. 1) THEN
          WRITE  = INXOUT.EQ.'WRITE'
        ELSE
          IF ( WRITE .AND. INXOUT.EQ.'READ' ) THEN
              WRITE (NDSE,901) INXOUT
              CALL EXTCDE ( 2 )
            END IF
        END IF
 
! open file ---------------------------------------------------------- *
!
      IF ( IPASS.GE.1 .AND. OFILES(2) .EQ. 1) THEN
!
          I      = LEN_TRIM(FILEXT)
          J      = LEN_TRIM(FNMPRE)
 
! Create TIMETAG for file name using YYYYMMDD.HHMMS prefix
          WRITE(TIMETAG,"(i8.8,'.'i6.6)")TIME(1),TIME(2)
!
          IF ( WRITE ) THEN
              OPEN (NDSOP,FILE=FNMPRE(:J)//TIMETAG//'.out_pnt.'   &
                    //FILEXT(:I),FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
            END IF
!
          REWIND ( NDSOP )
!
! test info ---------------------------------------------------------- *
! ( IPASS GE.1 .AND. OFILES(2) .EQ. 1)
!
          IF ( WRITE ) THEN
              WRITE (NDSOP)                                           &
                IDSTR, VEROPT, NK, NTH, NOPTS
            ELSE
              READ (NDSOP,END=801,ERR=802,IOSTAT=IERR)                &
                IDTST, VERTST, MK, MTH, NOPTS
!
              IF ( IDTST .NE. IDSTR ) THEN
                  WRITE (NDSE,902) IDTST, IDSTR
                  CALL EXTCDE ( 10 )
                END IF
              IF ( VERTST .NE. VEROPT ) THEN
                  WRITE (NDSE,903) VERTST, VEROPT
                  CALL EXTCDE ( 11 )
                END IF
              IF (NK.NE.MK .OR. NTH.NE.MTH) THEN
                  WRITE (NDSE,904) MK, MTH, NK, NTH
                  CALL EXTCDE ( 12 )
                END IF
              IF ( .NOT. O2INIT )                                     &
                  CALL W3DMO2 ( IGRD, NDSE, NDST, NOPTS )
            END IF
!
! Point specific info ------------------------------------------------ *
! ( IPASS GE.1 .AND. OFILES(2) .EQ. 1)
!
          IF ( WRITE ) THEN
              WRITE (NDSOP)                                           &
                    ((PTLOC(J,I),J=1,2),I=1,NOPTS), (PTNME(I),I=1,NOPTS)
           ELSE
              READ  (NDSOP,END=801,ERR=802,IOSTAT=IERR)               &
                    ((PTLOC(J,I),J=1,2),I=1,NOPTS), (PTNME(I),I=1,NOPTS)
            END IF
!
        END IF
!
! TIME --------------------------------------------------------------- *
!
      IF ( WRITE ) THEN
          WRITE (NDSOP)                            TIME
       ELSE
          READ (NDSOP,END=803,ERR=802,IOSTAT=IERR) TIME
        END IF
!
! Loop over spectra -------------------------------------------------- *
!
      DO I=1, NOPTS
!
        IF ( WRITE ) THEN
             ! set IW, II and IL to 0 because it is not used and gives &
             ! outlier values in out_pnt.points
             IW(I) = 0
             II(I) = 0
             IL(I) = 0
             WRITE (NDSOP)                                            &
                    IW(I), II(I), IL(I), DPO(I), WAO(I), WDO(I),      &
                    ASO(I), CAO(I), CDO(I), ICEO(I), ICEHO(I),        &
                    ICEFO(I), GRDID(I), (SPCO(J,I),J=1,NSPEC)
          ELSE
             READ (NDSOP,END=801,ERR=802,IOSTAT=IERR)                 &
                    IW(I), II(I), IL(I), DPO(I), WAO(I), WDO(I),      &
                    ASO(I), CAO(I), CDO(I), ICEO(I), ICEHO(I),        &
                    ICEFO(I), GRDID(I), (SPCO(J,I),J=1,NSPEC)
          END IF
!
        END DO
      IF (OFILES(2) .EQ. 1)  CLOSE (NDSOP)
!
      RETURN
!
! Escape locations read errors
!
  800 CONTINUE
      WRITE (NDSE,1000) IERR
      CALL EXTCDE ( 20 )
!
  801 CONTINUE
      WRITE (NDSE,1001)
      CALL EXTCDE ( 21 )
!
  802 CONTINUE
      WRITE (NDSE,1002) IERR
      CALL EXTCDE ( 22 )
!
  803 CONTINUE
      IOTST  = -1
      RETURN
!
! Formats
!
  900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
               '     ILEGAL INXOUT VALUE: ',A/)
  901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
               '     MIXED READ/WRITE, LAST REQUEST: ',A/)
  902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
               '     ILEGAL IDSTR, READ : ',A/                        &
               '                  CHECK : ',A/)
  903 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
               '     ILEGAL VEROPT, READ : ',A/                       &
               '                   CHECK : ',A/)
  904 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
               '     ERROR IN SPECTRA, MK, MTH : ',2I8/               &
               '              ARRAY DIMENSIONS : ',2I8/)
!
 1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO : '/               &
               '     ERROR IN OPENING FILE'/                          &
               '     IOSTAT =',I5/)
 1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO : '/               &
               '     PREMATURE END OF FILE'/)
 1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO : '/               &
               '     ERROR IN READING FROM FILE'/                     &
               '     IOSTAT =',I5/)
!
 
!
!/
!/ End of W3IOPO ----------------------------------------------------- /
!/
      END SUBROUTINE W3IOPO
!/
!/ End of module W3IOPOMD -------------------------------------------- /
!/
      END MODULE W3IOPOMD
