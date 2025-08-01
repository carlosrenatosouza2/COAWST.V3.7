

!#include "w3macros.h"
























































































































































































































































































!/ ------------------------------------------------------------------- /
      MODULE W3UPDTMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         13-Jan-2016 |
!/                  +-----------------------------------+
!/
!/    21-Jan-2000 : Origination.                        ( version 2.00 )
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    02-Apr-2001 : Adding sub-grid obstacles.          ( version 2.10 )
!/    18-May-2001 : Clean up and bug fixes.             ( version 2.11 )
!/    11-Jan-2002 : Sub-grid ice.                       ( version 2.15 )
!/    30-Apr-2002 : Water level fixes.                  ( version 2.20 )
!/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
!/    26-Dec-2002 : Moving grid wind correction.        ( version 3.02 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    15-Jul-2005 : Adding MAPST2.                      ( version 3.07 )
!/    07-Sep-2005 : Upgrading W3UBPT.                   ( version 3.08 )
!/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
!/    11-Jan-2007 : Clean-up W3UTRN boundary points.    ( version 3.10 )
!/    11-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    17-Aug-2010 : ABPI0-N(:,0) init. bug fix.         ( version 3.14 )
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    05-Apr-2011 : Place holder for XGR in UNGTYPE     ( version 4.04 ) 
!/                  (A. Roland/F. Ardhuin)
!/    13-Mar-2012 : Add initialization of UST on re-    ( version 4.07 )
!/                  activation of grid point.
!/    06-Jun-2012 : Porting bugfixes from 3.14 to 4.07  ( version 4.07 )
!/    12-Jun-2012 : Add /RTD option or rotated grid option. 
!/                  (Jian-Guo Li)                       ( version 4.07 )
!/    26-Sep-2012 : Adding update from tidal analysis   ( version 4.08 )
!/                  (F. Ardhuin)
!/    16-Sep-2013 : Add Arctic part for SMC grid.       ( version 4.11 )
!/    11-Nov-2013 : SMC and rotated grid incorporated in the main 
!/                  trunk                               ( version 4.13 )
!/    13-Nov-2013 : Moved reflection from ww3_grid.ftn  ( version 4.13 )
!/    27-May-2014 : Ading OMPG parallelizations dir,    ( version 5.02 )
!/    08-May-2014 : Implement tripolar grid for first order propagation
!/                  scheme                              ( version 5.03 )
!/                  (W. E. Rogers, NRL)
!/    27-Aug-2015 : New function to update variables    ( version 5.08 )
!/                  ICEF and ICEDMAX at the first time step
!/                  and add ICEH initialization in W3UICE.
!/    13-Jan-2016 : Changed initial value of ICEDMAX    ( version 5.08 )
!/    26-Mar-2018 : Sea-point only Wnd/Cur input.  JGLi ( version 6.04 )
!/    07-Oct-2019 : RTD option with standard lat-lon
!/                  grid when nesting to rotated grid   ( version 7.11 )
!/
!/    Copyright 2009-2014 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Bundles all input updating routines for WAVEWATCH III.
!
!  2. Variables and types :
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3UCUR    Subr. Public   Update current fields.
!      W3UWND    Subr. Public   Update wind fields.
!      W3UINI    Subr. Public   Update initial conditions.
!      W3UBPT    Subr. Public   Update boundary conditions.
!      W3UICE    Subr. Public   Update ice concentrations.
!      W3ULEV    Subr. Public   Update water levels.
!      W3UTRN    Subr. Public   Update cell boundary transparancies.
!      W3DZXY    Subr. Public   Calculate derivatives of a field.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      DSEC21    Func. W3TIMEMD Difference in time.
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      EXTCDE    Subr. W3SERVMD Exit program with error code.
!      PRTBLK    Subr. W3ARRYMD Print plot output.
!      PRT2DS    Subr. W3ARRYMD Print plot output.
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!  6. Switches :
!
!       !/SHRD   Switch for shared / distributed memory architecture.
!       !/DIST   Id.
!
!       !/OMPG   OpenMP compiler directives.
!
!       !/CRT0   No current interpolation.
!       !/CRT1   Linear current interpolation.
!       !/CRT2   Quasi-quadratic current interpolation.
!
!       !/WNT0   No wind interpolation.
!       !/WNT1   Linear wind interpolation.
!       !/WNT2   Energy conservation in wind interpolation.
!
!       !/RWND   Use wind speeds relative to currents.
!
!       !/STAB2  Calculate effective wind speed factor for stability
!                to be used with !/ST2.
!
!       !/S      Enable subroutine tracing.
!       !/Tn     Test output
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
      USE W3ODATMD, ONLY: NDSE, NDST, NAPROC, IAPROC, NAPERR
!/S      USE W3SERVMD, ONLY : STRACE
      USE W3TIMEMD, ONLY: DSEC21
!/
!/ ------------------------------------------------------------------- /
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3UCUR ( FLFRST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         15-Dec-2004 |
!/                  +-----------------------------------+
!/
!/    09-Dec-1996 : Final FORTRAN 77                    ( version 1.18 )
!/    20-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    27-Aug-2015 : Rename DT0,DTT by DT0T,DT0N         ( version 5.10 )
!/    23-Mar-2016 : SMC grid Arctic part adjustment.    ( version 5.18 )
!/    26-Mar-2018 : Sea-point only current on SMC grid. ( version 6.02 )
!/
!  1. Purpose :
!
!     Interpolate the current field to the present time.
!
!  2. Method :
!
!     Linear interpolation of speed and direction, with optionally
!     a correction to get approximate quadratic interpolation of speed
!     only.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       FLFRST  Log.  I   Flag for first pass through routine.
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
!     None.
!
!  7. Remarks :
!
!     - Only currents at sea points are considered.
!     - Time ranges checked in W3WAVE.
!     - Currents are stored by components to save on the use of
!       SIN and COS functions. The actual interpolations, however
!       are by absolute value and direction.
!
!  8. Structure :
!
!     --------------------------------------
!      1.  Prepare auxiliary arrays.
!      2.  Calculate interpolation factors.
!      3.  Get actual winds.
!     --------------------------------------
!
!  9. Switches :
!
!     !/CRT0  No current interpolation.
!     !/CRT1  Linear current interpolation.
!     !/CRT2  Quasi-quadratic current interpolation.
!
!     !/S     Enable subroutine tracing.
!     !/T     Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSF
!/ARC      USE W3GDATMD, ONLY: NARC, NGLO, ANGARC 
!/SMC      USE W3GDATMD, ONLY: FSWND 
      USE W3WDATMD, ONLY: TIME
      USE W3ADATMD, ONLY: CX, CY, CA0, CAI, CD0, CDI
      USE W3IDATMD, ONLY: TC0, CX0, CY0, TCN, CXN, CYN
!/TIDE      USE W3GDATMD, ONLY: YGRD
!/TIDE      USE W3TIMEMD
!/TIDE      USE W3IDATMD, ONLY: FLCURTIDE, CXTIDE, CYTIDE, NTIDE
!/TIDE      USE W3TIDEMD
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      LOGICAL, INTENT(IN)     :: FLFRST
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: ISEA, IX, IY
!/S      INTEGER, SAVE           :: IENT = 0
      REAL                    :: D0, DN, DD, DT0N, DT0T, RD, CABS, CDIR
!/CRT2      REAL                    :: RD2, CI2
!/TIDE      INTEGER          :: J,K
!/TIDE      INTEGER(KIND=4)  :: TIDE_KD0, INT24, INTDYS       ! "Gregorian day constant"
!/TIDE      REAL             :: WCURTIDEX, WCURTIDEY, TIDE_ARGX, TIDE_ARGY
!/TIDE      REAL(KIND=8)     :: d1,h,TIDE_HOUR,HH,pp,s,p,enp,dh,dpp,ds,dp,dnp,tau
!/TIDE      REAL             :: FX(44),UX(44),VX(44)
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3UCUR')
!
! 1.  Prepare auxiliary arrays
!
      IF ( FLFRST ) THEN
          DO ISEA=1, NSEA
!/SMC !!Li  For sea-point SMC grid current, the 1-D current is stored on
!/SMC !!Li  2-D CX0(NSEA, 1) variable. 
!/SMC        IF( FSWND ) THEN
!/SMC            IX = ISEA 
!/SMC            IY = 1
!/SMC        ELSE
            IX        = MAPSF(ISEA,1)
            IY        = MAPSF(ISEA,2)
!/SMC        ENDIF

            CA0(ISEA) = SQRT ( CX0(IX,IY)**2 + CY0(IX,IY)**2 )
            CAI(ISEA) = SQRT ( CXN(IX,IY)**2 + CYN(IX,IY)**2 )
            IF ( CA0(ISEA) .GT. 1.E-7) THEN
                D0     = MOD ( TPI+ATAN2(CY0(IX,IY),CX0(IX,IY)) , TPI )
              ELSE
                D0     = 0
              END IF
            IF ( CAI(ISEA) .GT. 1.E-7) THEN
                DN     = MOD ( TPI+ATAN2(CYN(IX,IY),CXN(IX,IY)) , TPI )
              ELSE
                DN     = D0
              END IF
            IF ( CA0(ISEA) .GT. 1.E-7) THEN
                CD0(ISEA) = D0
              ELSE
                CD0(ISEA) = DN
              END IF
            DD     = DN - CD0(ISEA)
            IF (ABS(DD).GT.PI) DD = DD - TPI*SIGN(1.,DD)
            CDI(ISEA) = DD
            CAI(ISEA) = CAI(ISEA) - CA0(ISEA)
            END DO
        END IF
!
! 2.  Calculate interpolation factor
!
      DT0N    = DSEC21 ( TC0, TCN )
      DT0T    = DSEC21 ( TC0, TIME )
!
!/CRT0      RD     = 0.
!/CRT1      RD     = DT0T / MAX ( 1.E-7 , DT0N )
!/CRT2      RD     = DT0T / MAX ( 1.E-7 , DT0N )
!/CRT2      RD2    = 1. - RD
!/OASOCM     RD     = 1.
!
!/T      WRITE (NDST,9000) DT0N, DT0T, RD

!/TIDE        IF (FLCURTIDE) THEN 
!/TIDE!          WRITE(6,*) 'TIME CUR:',TIME, '##',TC0, '##',TCN
!/TIDE          TIDE_HOUR = TIME2HOURS(TIME)
!/TIDE!
!/TIDE!*  THE ASTRONOMICAL ARGUMENTS ARE CALCULATED BY LINEAR APPROXIMATION
!/TIDE!*  AT THE MID POINT OF THE ANALYSIS PERIOD.
!/TIDE          d1=TIDE_HOUR/24.d0
!/TIDE          TIDE_KD0= 2415020
!/TIDE          d1=d1-dfloat(TIDE_kd0)-0.5d0
!/TIDE          call astr(d1,h,pp,s,p,enp,dh,dpp,ds,dp,dnp)
!/TIDE          INT24=24
!/TIDE          INTDYS=int((TIDE_HOUR+0.00001)/INT24)
!/TIDE          HH=TIDE_HOUR-dfloat(INTDYS*INT24)
!/TIDE          TAU=HH/24.D0+H-S
!/TIDE        END IF
!/TIDE!
!/TIDE!  ONLY THE FRACTIONAL PART OF A SOLAR DAY NEED BE RETAINED FOR COMPU-
!/TIDE!  TING THE LUNAR TIME TAU.
!/TIDE!

!
! 3.  Actual currents for all grid points
!
      DO ISEA=1, NSEA
!/TIDE        IF (FLCURTIDE) THEN  ! could move IF test outside of ISEA loop ... 
!/TIDE! VUF should only be updated in latitude changes significantly ...
!/TIDE          IX        = MAPSF(ISEA,1)
!/TIDE          IY        = MAPSF(ISEA,2)
!/TIDE          CALL SETVUF_FAST(h,pp,s,p,enp,dh,dpp,ds,dp,dnp,tau,YGRD(IY,IX),FX,UX,VX)
!/TIDE          WCURTIDEX = CXTIDE(IX,IY,1,1)
!/TIDE          WCURTIDEY = CYTIDE(IX,IY,1,1)
!/TIDE 
!/TIDE            DO J=2,TIDE_MF
!/TIDE              TIDE_ARGX=(VX(J)+UX(J))*twpi-CXTIDE(IX,IY,J,2)*DERA
!/TIDE              TIDE_ARGY=(VX(J)+UX(J))*twpi-CYTIDE(IX,IY,J,2)*DERA
!/TIDE              WCURTIDEX = WCURTIDEX+FX(J)*CXTIDE(IX,IY,J,1)*COS(TIDE_ARGX)
!/TIDE              WCURTIDEY = WCURTIDEY+FX(J)*CYTIDE(IX,IY,J,1)*COS(TIDE_ARGY)                    
!/TIDE              END DO
!/TIDE     

!/TIDET !Verification 
!/TIDET         IF (ISEA.EQ.1) THEN 
!/TIDET
!/TIDET            TIDE_AMPC(1:NTIDE,1)=CXTIDE(IX,IY,1:NTIDE,1)
!/TIDET            TIDE_PHG(1:NTIDE,1 )=CXTIDE(IX,IY,1:NTIDE,2)
!/TIDET            TIDE_AMPC(1:NTIDE,2)=CYTIDE(IX,IY,1:NTIDE,1)
!/TIDET            TIDE_PHG(1:NTIDE,2) =CYTIDE(IX,IY,1:NTIDE,2)
!/TIDET
!/TIDET              WRITE(993,'(A,F20.2,13F8.3)') 'TEST ISEA 0:',    &
!/TIDET                          d1,H,S,TAU,pp,s,p,enp,dh,dpp,ds,dp,dnp,YGRD(IY,IX)
!/TIDET
!/TIDET              DO J=1,TIDE_MF
!/TIDET                WRITE(993,'(A,4I9,F12.0,3F8.3,I4,X,A)') 'TEST ISEA 1:',IX,J,TIME,TIDE_HOUR,    &
!/TIDET                            FX(J),UX(J),VX(J),TIDE_INDEX2(J),TIDECON_ALLNAMES(TIDE_INDEX2(J))
!/TIDET                END DO
!/TIDET          DO K=1,2
!/TIDET            DO J=1,TIDE_MF
!/TIDET              WRITE(993,'(A,5I9,F12.0,5F8.3)') 'TEST ISEA 2:',IX,K,J,TIME,TIDE_HOUR,    &
!/TIDET                         FX(J),UX(J),VX(J),TIDE_AMPC(J,K),TIDE_PHG(J,K)
!/TIDET             END DO
!/TIDET             END DO
!/TIDET
!/TIDET            WRITE(993,'(A,2F8.4,A,2F8.4)') '#:',CX0(IX,IY),CY0(IX,IY),'##',WCURTIDEX,WCURTIDEY
!/TIDET            CLOSE(993)
!/TIDET           END IF
!/TIDET ! End of verification
!/TIDE            CX(ISEA) = WCURTIDEX
!/TIDE            CY(ISEA) = WCURTIDEY
!/TIDE          ELSE 

        CABS    = CA0(ISEA) + RD * CAI(ISEA)
!/CRT2        CI2      = SQRT ( RD2 *      CA0(ISEA)**2 +             &
!/CRT2                          RD  *(CA0(ISEA)+CAI(ISEA))**2 )
!/CRT2        CABS    = CABS * MIN( 1.25 , CI2/MAX(1.E-7,CABS) )
        CDIR    = CD0(ISEA) + RD * CDI(ISEA)

!/ARC  !Li   Rotate curreent direction by ANGARC for Arctic part cells.  JGLi23Mar2016
!/ARC        IF( ISEA .GT. NGLO ) THEN
!/ARC            DN = CDIR + ANGARC( ISEA - NGLO )*DERA
!/ARC            CDIR = MOD ( TPI + DN, TPI )
!/ARC        ENDIF

        CX(ISEA) = CABS * COS(CDIR)
        CY(ISEA) = CABS * SIN(CDIR)
!/TIDE  !        IF (ISEA.EQ.1)  WRITE(6,'(A,4F8.4,A,4F8.4)') 'CUR#:',RD,CA0(ISEA),CAI(ISEA),CABS,'##', &
!/TIDE  !                                      CX(ISEA), CY(ISEA),WCURTIDEX, WCURTIDEY        
!/TIDE          END IF
!
        END DO
!
      RETURN
!
! Formats
!
!/T 9000 FORMAT (' TEST W3UCUR : DT0N, DT0T, RD :',2F8.1,F6.3)
!/
!/ End of W3UCUR ----------------------------------------------------- /
!/
      END SUBROUTINE W3UCUR
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3UWND ( FLFRST, VGX, VGY )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         27-May-2014 |
!/                  +-----------------------------------+
!/
!/    03-Dec-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    20-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
!/    26-Dec-2002 : Moving grid wind correction.        ( version 3.02 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
!/    16-Sep-2013 : Rotating wind for Arctic part.      ( version 4.11 )
!/    27-May-2014 : Adding OMPG parallelizations dir.   ( version 5.02 )
!/    27-Aug-2015 : Rename DT0,DTT by DT0T,DT0N         ( version 5.10 )
!/    26-Mar-2018 : Sea-point only wind for SMC grid.   ( version 6.07 )
!/
!  1. Purpose :
!
!     Interpolate wind fields to the given time.
!
!  2. Method :
!
!     Linear interpolation of wind speed and direction, with a simple
!     correction to obtain quasi-conservation of energy.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       FLFRST  Log.  I   Flag for first pass through routine.
!       VGX/Y   Real  I   Grid velocity                      (!/MGW)
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
!     - Only winds over sea points are considered.
!     - Time ranges checked in W3WAVE.
!
!  8. Structure :
!
!     --------------------------------------
!      1.  Prepare auxiliary arrays.
!      2.  Calculate interpolation factors
!      3.  Get actual winds
!      4.  Correct for currents
!      5.  Convert to stresses
!      6.  Stability correction
!     --------------------------------------
!
!  9. Switches :
!
!     !/OMPG   OpenMP compiler directives.
!
!     !/WNT0   No wind interpolation.
!     !/WNT1   Linear wind interpolation.
!     !/WNT2   Energy conservation in wind interpolation.
!
!     !/RWND   Use wind speeds relative to currents.
!     !/MGW    Moving grid wind correction.
!
!     !/STAB2  Calculate effective wind speed factor for stability
!              to be used with !/ST2.
!
!     !/S      Enable subroutine tracing.
!     !/T      Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSF
!/WCOR      USE W3GDATMD, ONLY:   WWCOR
!/RWND      USE W3GDATMD, ONLY:   RWINDC
!/ST2      USE W3GDATMD, ONLY: ZWIND, OFSTAB, FFNG, FFPS, CCNG, CCPS, SHSTAB
!/ARC      USE W3GDATMD, ONLY: NARC, NGLO, ANGARC 
!/SMC      USE W3GDATMD, ONLY: FSWND 
      USE W3WDATMD, ONLY: TIME, ASF
      USE W3ADATMD, ONLY: DW, CX, CY, UA, UD, U10, U10D, AS,          &
                          UA0, UAI, UD0, UDI, AS0, ASI
      USE W3IDATMD, ONLY: TW0, WX0, WY0, DT0, TWN, WXN, WYN, DTN, FLCUR
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(IN)        :: VGX, VGY
      LOGICAL, INTENT(IN)     :: FLFRST
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: ISEA, IX, IY
!/S      INTEGER, SAVE           :: IENT = 0
      REAL                    :: D0, DN, DD, DT0N, DT0T, RD, UI2,      &
                                 UXR, UYR
!/WNT2      REAL                    :: RD2
!/STAB2      REAL                    :: STAB0, STAB, THARG1, THARG2, COR1, COR2
      REAL                    :: UDARC
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3UWND')
!
! 1.  Prepare auxiliary arrays
!
      IF ( FLFRST ) THEN
          DO ISEA=1, NSEA
!/SMC !!Li  For sea-point only SMC grid wind 1-D wind is stored on
!/SMC !!Li  2-D WX0(NSEA, 1) variable.
!/SMC        IF( FSWND ) THEN
!/SMC            IX = ISEA 
!/SMC            IY = 1
!/SMC        ELSE
            IX        = MAPSF(ISEA,1)
            IY        = MAPSF(ISEA,2)
!/SMC        ENDIF

            UA0(ISEA) = SQRT ( WX0(IX,IY)**2 + WY0(IX,IY)**2 )
            UAI(ISEA) = SQRT ( WXN(IX,IY)**2 + WYN(IX,IY)**2 )
            IF ( UA0(ISEA) .GT. 1.E-7) THEN
                D0     = MOD ( TPI+ATAN2(WY0(IX,IY),WX0(IX,IY)) , TPI )
              ELSE
                D0     = 0
              END IF
            IF ( UAI(ISEA) .GT. 1.E-7) THEN
                DN     = MOD ( TPI+ATAN2(WYN(IX,IY),WXN(IX,IY)) , TPI )
              ELSE
                DN     = D0
              END IF
            IF ( UA0(ISEA) .GT. 1.E-7) THEN
                UD0(ISEA) = D0
              ELSE
                UD0(ISEA) = DN
              END IF
            DD     = DN - UD0(ISEA)
            IF (ABS(DD).GT.PI) DD = DD - TPI*SIGN(1.,DD)
            UDI(ISEA) = DD
            UAI(ISEA) = UAI(ISEA) - UA0(ISEA)
            AS0(ISEA) = DT0(IX,IY)
            ASI(ISEA) = DTN(IX,IY) - DT0(IX,IY)
            END DO
        END IF
!
! 2.  Calculate interpolation factor
!
      DT0N    = DSEC21 ( TW0, TWN )
      DT0T   = DSEC21 ( TW0, TIME )
!
!/WNT0      RD     = 0.
!/WNT1      RD     = DT0T / MAX ( 1.E-7 , DT0N )
!/WNT2      RD     = DT0T / MAX ( 1.E-7 , DT0N )
!/WNT2      RD2    =  1. - RD
!/OASACM     RD     = 1.
!
!/T      WRITE (NDST,9000) DT0N, DT0T, RD
!
! 3.  Actual wind for all grid points
!
!/OMPG/!$OMP PARALLEL DO PRIVATE (ISEA,UI2,UXR,UYR,UDARC)
!
      DO ISEA=1, NSEA
!
! jcw call to stop wind interpolation.
!/MGW        UXR        = UA(ISEA)*COS(UD(ISEA)) + VGX
!/MGW        UYR        = UA(ISEA)*SIN(UD(ISEA)) + VGY
!/MGW        UA(ISEA) = MAX ( 0.001 , SQRT(UXR**2+UYR**2) )
!/MGW        UD(ISEA) = MOD ( TPI+ATAN2(UYR,UXR) , TPI )
!/ARC  !Li   Rotate wind direction by ANGARC for Arctic part cells.
!/ARC        IF( ISEA .GT. NGLO ) THEN
!/ARC            UDARC = UD(ISEA) + ANGARC( ISEA - NGLO )*DERA
!/ARC            UD(ISEA) = MOD ( TPI + UDARC, TPI )
!/ARC        ENDIF
!
        AS(ISEA) = AS0(ISEA) + RD * ASI(ISEA)
!        IF (UA(ISEA).NE.UA(ISEA)) WRITE(6,*) 'BUG WIND:',ISEA,UA(ISEA),MAPSF(ISEA,1), MAPSF(ISEA,2),UA0(ISEA),RD,UAI(ISEA)
!        IF (UD(ISEA).NE.UD(ISEA)) WRITE(6,*) 'BUG WIN2:',ISEA,UD(ISEA),MAPSF(ISEA,1), MAPSF(ISEA,2)
!
        END DO
!
!/OMPG/!$OMP END PARALLEL DO
!
! 3.b  Bias correction ( !/WCOR )
!/WCOR      WHERE ( UA .GE. WWCOR(1) ) UA = UA+(UA-WWCOR(1))*WWCOR(2)

!
! 4.  Correct for currents and grid motion
!
!/RWND      IF ( FLCUR ) THEN
!
!/RWND          DO ISEA=1, NSEA
!/RWND            UXR        = UA(ISEA)*COS(UD(ISEA)) - RWINDC*CX(ISEA)
!/RWND            UYR        = UA(ISEA)*SIN(UD(ISEA)) - RWINDC*CY(ISEA)
!/RWND            U10 (ISEA) = MAX ( 0.001 , SQRT(UXR**2+UYR**2) )
!/RWND            U10D(ISEA) = MOD ( TPI+ATAN2(UYR,UXR) , TPI )
!/RWND            END DO
!
!/RWND        ELSE
!
!/OMPG/!$OMP PARALLEL DO PRIVATE (ISEA)
!
          DO ISEA=1, NSEA
            U10 (ISEA) = MAX ( UA(ISEA) , 0.001 )
            U10D(ISEA) = UD(ISEA)
            END DO
!
!/OMPG/!$OMP END PARALLEL DO
!
!/RWND        END IF
!
! 5.  Stability correction ( !/STAB2 )
!     Original settings :
!
!     SHSTAB =    1.4
!     OFSTAB =   -0.01
!     CCNG   =   -0.1
!     CCPS   =    0.1
!     FFNG   = -150.
!     FFPS   =  150.
!
!/STAB2      STAB0  = ZWIND * GRAV / 273.
!
!/STAB2      DO ISEA=1, NSEA
!/STAB2        STAB   = STAB0 * AS(ISEA) / MAX(5.,U10(ISEA))**2
!/STAB2        STAB   = MAX ( -1. , MIN ( 1. , STAB ) )
!
!/STAB2        THARG1 = MAX ( 0. , FFNG*(STAB-OFSTAB))
!/STAB2        THARG2 = MAX ( 0. , FFPS*(STAB-OFSTAB))
!/STAB2        COR1   = CCNG * TANH(THARG1)
!/STAB2        COR2   = CCPS * TANH(THARG2)
!
!/STAB2        ASF(ISEA) = SQRT ( (1.+COR1+COR2)/SHSTAB )
!/STAB2        U10(ISEA) = U10(ISEA) / ASF(ISEA)
!/STAB2        END DO
!
      RETURN
!
! Formats
!
!/T 9000 FORMAT (' TEST W3UWND : DT0N, DT0T, RD :',2F8.1,F6.3)
!/
!/ End of W3UWND ----------------------------------------------------- /
!/
      END SUBROUTINE W3UWND
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3UINI ( A )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Jun-2018 |
!/                  +-----------------------------------+
!/
!/    19-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    20-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    18-May-2001 : Fix CG declaration.                 ( version 2.11 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/    06-Jun-2018 : use W3PARALL and INIT_GET_ISEA      ( version 6.04 )
!/
!  1. Purpose :
!
!     Initialize the wave field with fetch-limited spectra before the
!     actual calculation start. (Named as an update routine due to
!     placement in code.)
!
!  2. Method :
!
!     Fetch-limited JONSWAP spectra with a cosine^2 directional
!     distribution and a mean direction taken from the wind.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       A       R.A.   O   Action density spectra.
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
!     - Wind speeds filtered by U10MIN and U10MAX (DATA statements)
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!
!       !/S     Enable subroutine tracing.
!       !/T     General test output.
!       !/T1    Parameters at grid points.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, MAPSF,                 &
                          NK, NTH, TH, SIG, DTH, DSIP, UNGTYPE,       &
                          RLGTYPE, CLGTYPE, GTYPE, FLAGLL,            &
                          HPFAC, HQFAC
      USE W3ADATMD, ONLY: U10, U10D, CG
      USE W3PARALL, only : INIT_GET_JSEA_ISPROC, INIT_GET_ISEA
      USE W3PARALL, only : GET_JSEA_IBELONG
!/T      USE W3ARRYMD, ONLY : PRTBLK
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(OUT)       :: A(NTH,NK,0:NSEAL)
!/
!/ ------------------------------------------------------------------- /
!/ Local variables
!/
      INTEGER                 :: IX, IY, ISEA, JSEA, IK, ITH, ISPROC
!/S      INTEGER, SAVE           :: IENT = 0
!/T      INTEGER                 :: IX0, IXN, MAPOUT(NX,NY)
!/T      INTEGER                 :: NXP = 60
      REAL                    :: ALFA(NSEAL), FP(NSEAL), YLN(NSEAL),  &
                                 AA, BB, CC
      REAL                    :: XGR, U10C, U10DIR, XSTAR, FSTAR,     &
                                 GAMMA, FR, D1(NTH), D1INT, F1, F2
      REAL                    :: ETOT, E1I
      REAL                    :: U10MIN =  1.
      REAL                    :: U10MAX = 20.
!/T      REAL                    :: HSIG(NX,NY)
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3UINI')
!
!
! Pre-process JONSWAP data for all grid points ----------------------- *
!
!/T1      WRITE (NDST,9010)
!
!  this is not clear what is going on betwen w3init and this ... 
      A(:,:,:)=0
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        IF (GTYPE.EQ.UNGTYPE) THEN 
          XGR=1.  ! to be fixed later 
        ELSE
          IX     = MAPSF(ISEA,1)
          IY     = MAPSF(ISEA,2)
          XGR    = 0.5 * SQRT(HPFAC(IY,IX)**2+HQFAC(IY,IX)**2)
          END IF
        IF ( FLAGLL ) THEN
            XGR    = XGR * RADIUS * DERA
          END IF
!
        U10C   = MAX ( MIN(U10(ISEA),U10MAX) , U10MIN )
!
        XSTAR  = GRAV * XGR / U10C**2
        FSTAR  = 3.5 / XSTAR**(0.33)
        GAMMA  = MAX ( 1. , 7.0 / XSTAR**(0.143) )
!
        ALFA(JSEA) = 0.076 / XSTAR**(0.22)
        FP  (JSEA) = FSTAR * GRAV / U10C
        YLN (JSEA) = LOG ( GAMMA )
!
!/T1        WRITE (NDST,9011) ISEA, U10C, XSTAR,                      &
!/T1                          ALFA(JSEA), FP(JSEA), GAMMA
!
        END DO
!
! 1-D spectrum at location ITH = NTH --------------------------------- *
!
      DO IK=1, NK
        FR     = SIG(IK) * TPIINV
        DO JSEA=1, NSEAL
!
!/ ----- INLINED EJ5P (REDUCED) -------------------------------------- /
!
          AA     = ALFA(JSEA) * 0.06175/FR**5
          BB     = MAX( -50. , -1.25*(FP(JSEA)/FR)**4 )
          CC     = MAX( -50. , -0.5*((FR-FP(JSEA))/(0.07*FP(JSEA)))**2 )
          A(NTH,IK,JSEA)                                              &
                 = AA * EXP(BB + EXP(CC) * YLN(JSEA))
!
!/ ----- INLINED EJ5P (END) ------------------------------------------ /
!
          END DO
        END DO
!
! Apply directional distribution ------------------------------------- *
!
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        U10DIR = U10D(ISEA)
        D1INT  = 0.
!
        DO ITH=1, NTH
          D1(ITH) = ( MAX ( 0. , COS(TH(ITH)-U10DIR) ) )**2
          D1INT   = D1INT + D1(ITH)
          END DO
!
        D1INT  = D1INT * DTH
        F1     = TPIINV / D1INT
!
        DO IK=1, NK
          F2     = F1 * A(NTH,IK,JSEA) * CG(IK,ISEA) / SIG(IK)
          DO ITH=1, NTH
            A(ITH,IK,JSEA) = F2 * D1(ITH)
            END DO
          END DO
!
        END DO
!
! Test output -------------------------------------------------------- *
!
!/T      HSIG   = 0.
!/T      MAPOUT = 0
!
!/T      DO ISEA=IAPROC, NSEA, NAPROC
!/T        JSEA   = 1 + (ISEA-1)/NAPROC
!/T        ETOT   = 0.
!/T        DO IK=1, NK
!/T          E1I    = 0.
!/T          DO ITH=1, NTH
!/T            E1I    = E1I + A(ITH,IK,JSEA)
!/T            END DO
!/T          ETOT   = ETOT + E1I * DSIP(IK) * SIG(IK) / CG(IK,ISEA)
!/T          END DO
!/T        IX            = MAPSF(ISEA,1)
!/T        IY            = MAPSF(ISEA,2)
!/T        HSIG  (IX,IY) = 4. * SQRT ( ETOT * DTH )
!/T        MAPOUT(IX,IY) = 1
!/T        END DO
!
!/T      IX0    = 1
!/T      DO
!/T        IXN    = MIN ( NX , IX0+NXP-1 )
!/T        CALL PRTBLK (NDST, NX, NY, NX, HSIG, MAPOUT, 0, 0.,        &
!/T                     IX0, IXN, 1, 1, NY, 1, 'Hs', 'm')
!/T        IF ( IXN .EQ. NX ) EXIT
!/T        IX0    = IX0 + NXP
!/T        END DO
!
      RETURN
!
! Formats
!
!/T 9000 FORMAT (' TEST W3UINI : XGR = ',E10.3)
!
!/T1 9010 FORMAT (' TEST W3UINI :  ISEA, U10C, XSTAR, ALPHA, FP, GAMMA')
!/T1 9011 FORMAT ('   ',I6,F8.2,F10.1,2F6.3,F6.2)
!/
!/ End of W3UINI ----------------------------------------------------- /
!/
      END SUBROUTINE W3UINI
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3UBPT
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Jun-2018 |
!/                  +-----------------------------------+
!/
!/    19-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    20-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    07-Sep-2005 : Moving update to end of time step.  ( version 3.08 )
!/    17-Aug-2010 : Add initialization ABPI0-N(:,0).  ( version 3.14.5 )
!/    12-Jun-2012 : Add /RTD option or rotated grid option. 
!/                  (Jian-Guo Li)                       ( version 4.06 )
!/    06-Jun-2018 : Add DEBUGIOBC/SETUP/DEBUGW3ULEV     ( version 6.04 )
!/    13-Jun-2019 : Rotation only if POLAT<90 (C.Hansen)( version 7.11 ) 
!/
!  1. Purpose :
!
!     Update spectra at the active boundary points.
!
!  2. Method :
!
!     Spectra are read and interpolated in space and time from the
!     data read by W3IOBC.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!       STRACE, DSEC21
!                Service routines.
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!     - The data arrays contain sigma spectra to assure conservation
!       when changing grids.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S     Enable subroutine tracing.
!     !/T0    Test output of wave heights.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NSPEC, MAPWN, SIG2, DDEN
!/RTD !!   Use rotation angle and action conversion sub.  JGLi12Jun2012
!/RTD      USE W3GDATMD, ONLY: NK, NTH, NSPEC, AnglD, PoLat
!/RTD      USE W3SERVMD, ONLY: W3ACTURN
      USE W3ADATMD, ONLY: CG
      USE W3ODATMD, ONLY: NBI, ABPI0, ABPIN, ISBPI, IPBPI, RDBPI,     &
                          BBPI0, BBPIN
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: IBI, ISP, ISEA
!/S      INTEGER, SAVE           :: IENT = 0
!/T0      REAL                    :: HS1, HS2
!/RTD !!    Declare a temporary spectr variable.  JGLi12Jun2012 
!/RTD       REAL :: Spectr(NSPEC), AnglBP
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3UBPT')
!
! 1.  Process BBPI0 -------------------------------------------------- *
! 1.a First intialization

!/DEBUGIOBC     WRITE(740+IAPROC,*) 'Beginning of W3UBPT'
!/DEBUGIOBC     FLUSH(740+IAPROC)

!
      IF ( BBPI0(1,0) .EQ. -1. ) THEN
!
          BBPI0(:,0) = 0.
          BBPIN(:,0) = 0.
          ABPI0(:,0) = 0.
          ABPIN(:,0) = 0.
!
          DO IBI=1, NBI
            ISEA   = ISBPI(IBI)
            DO ISP=1, NSPEC
              BBPI0(ISP,IBI) = CG(MAPWN(ISP),ISEA) / SIG2(ISP) *      &
                           ( RDBPI(IBI,1) * ABPI0(ISP,IPBPI(IBI,1))   &
                           + RDBPI(IBI,2) * ABPI0(ISP,IPBPI(IBI,2))   &
                           + RDBPI(IBI,3) * ABPI0(ISP,IPBPI(IBI,3))   &
                           + RDBPI(IBI,4) * ABPI0(ISP,IPBPI(IBI,4)) )
              END DO
            END DO
!
! 1.b Shift BBPIN
!
        ELSE
          BBPI0 = BBPIN
        END IF
!
! 2.  Process BBPIN -------------------------------------------------- *
!
      DO IBI=1, NBI
        ISEA   = ISBPI(IBI)
        DO ISP=1, NSPEC
          BBPIN(ISP,IBI) = CG(MAPWN(ISP),ISEA) / SIG2(ISP) *          &
                       ( RDBPI(IBI,1) * ABPIN(ISP,IPBPI(IBI,1))       &
                       + RDBPI(IBI,2) * ABPIN(ISP,IPBPI(IBI,2))       &
                       + RDBPI(IBI,3) * ABPIN(ISP,IPBPI(IBI,3))       &
                       + RDBPI(IBI,4) * ABPIN(ISP,IPBPI(IBI,4)) )
          END DO
!
!/RTD !!  Rotate the spectra if model is on rotated grid.  JGLi12Jun2012
!/RTD !!  PoLat == 90. if the grid is standard lat/lon (C. Hansen 20190613)
!/RTD        IF ( PoLat < 90. ) THEN
!/RTD          Spectr = BBPIN(:,IBI)
!/RTD          AnglBP = AnglD(ISEA)
!/RTD          CALL  W3ACTURN( NTH, NK, AnglBP, Spectr )
!/RTD          BBPIN(:,IBI) = Spectr
!/RTD        END IF
!/RTD
!
        END DO

! 3.  Wave height test output ---------------------------------------- *
!
!/T0      WRITE (NDST,9000)
!/T0      DO IBI=1, NBI
!/T0        HS1    = 0.
!/T0        HS2    = 0.
!/T0        DO ISP=1, NSPEC
!/T0          HS1    = HS1 + BBPI0(ISP,IBI) * DDEN(MAPWN(ISP)) /       &
!/T0                                          CG(MAPWN(ISP),ISBPI(IBI))
!/T0          HS2    = HS2 + BBPIN(ISP,IBI) * DDEN(MAPWN(ISP)) /       &
!/T0                                          CG(MAPWN(ISP),ISBPI(IBI))
!/T0          END DO
!/T0        HS1    = 4. * SQRT ( HS1 )
!/T0        HS2    = 4. * SQRT ( HS2 )
!/T0        WRITE (NDST,9001) IBI, ISBPI(IBI), HS1, HS2
!/T0        END DO
!
!/DEBUGIOBC     WRITE(740+IAPROC,*) 'sum(abs(ABPI0))=', sum(abs(ABPI0))
!/DEBUGIOBC     WRITE(740+IAPROC,*) 'sum(abs(ABPIN))=', sum(abs(ABPIN))
!/DEBUGIOBC     WRITE(740+IAPROC,*) 'sum(abs(BBPI0))=', sum(abs(BBPI0))
!/DEBUGIOBC     WRITE(740+IAPROC,*) 'sum(abs(BBPIN))=', sum(abs(BBPIN))
!/DEBUGIOBC     WRITE(740+IAPROC,*) 'End of W3UBPT'
!/DEBUGIOBC     FLUSH(740+IAPROC)
      RETURN
!
! Formats
!
!/T0 9000 FORMAT ( ' TEST W3UBPT : WAVE HEIGHTS BBPI0/N (NO TAIL)')
!/T0 9001 FORMAT ( '         ',2I8,2X,2F8.2)

!/
!/ End of W3UBPT ----------------------------------------------------- /
!/
      END SUBROUTINE W3UBPT
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3UIC1( FLFRST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           C. Sevigny              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         27-Aug-2015 |
!/                  +-----------------------------------+
!/
!/    27-Aug-2015 : Creation                            ( version 5.10 )   
!/
!  1. Purpose :
!
!     Update ice thickness in the wave model.
!
!  2. Method :
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!      FLFRST   L.    I     Spectra in 1-D or 2-D representation
!                           (points to same address).
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
!     None.
!
!  7. Remarks :
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
!     !/S  Enable subroutine tracing.
!     !/T  Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /   
      USE W3GDATMD, ONLY: NSEA, NSEA, MAPSF, IICEHMIN, IICEHFAC
      USE W3WDATMD, ONLY: TIME, TIC1, ICEH
      USE W3IDATMD, ONLY: TI1, ICEP1, FLIC1
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
      LOGICAL, INTENT(IN)     :: FLFRST
!/    
!/ ------------------------------------------------------------------- /
!/ Local variables
!/      
      INTEGER                 :: IX, IY, ISEA
!/
!/
! 1.  Preparations --------------------------------------------------- *
! 1.a Update times
!
!/T      WRITE (NDST,9010) TIME, TIC1, TI1
      TIC1(1) = TI1(1)
      TIC1(2) = TI1(2)

! 2.  Main loop over sea points -------------------------------------- *

      DO ISEA=1, NSEA
!
        IX        = MAPSF(ISEA,1)
        IY        = MAPSF(ISEA,2)
        ICEH(ISEA) = MAX(IICEHMIN,IICEHFAC*ICEP1(IX,IY))
      END DO
!
      RETURN
!/T 9010 FORMAT ( ' TEST W3UIC1 : TIME     :',I9.8,I7.6/              &
!/T               '               OLD TICE :',I9.8,I7.6/              &
!/T               '               NEW TICE :',I9.8,I7.6)
!/
!/ End of W3UIC1 ----------------------------------------------------- /
!/
      END SUBROUTINE W3UIC1
!/ ------------------------------------------------------------------- /  
      SUBROUTINE W3UIC5( FLFRST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |  C. Sevigny  & F. Ardhuin         |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         13-Jan-2016 |
!/                  +-----------------------------------+
!/
!/    27-Aug-2015 : Creation                            ( version 5.08 )   
!/    13-Jan-2016 : Changed initial value of ICEDMAX    ( version 5.08 )   
!/
!  1. Purpose :
!
!     Update ice floe mean and max diameters in the wave model.
!
!  2. Method :
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!      FLFRST   L.    I     Spectra in 1-D or 2-D representation
!                           (points to same address).
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
!     None.
!
!  7. Remarks :
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
!     !/S  Enable subroutine tracing.
!     !/T  Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /   
      USE W3IDATMD, ONLY: TI5, ICEP5
      USE W3GDATMD, ONLY: NSEA, MAPSF
      USE W3WDATMD, ONLY: TIME, TIC5, ICE, ICEH, ICEF, ICEDMAX
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
      LOGICAL, INTENT(IN)     :: FLFRST
!/    
!/
!/ ------------------------------------------------------------------- /
!/ Local variables
!/      
      INTEGER                 :: IX, IY, ISEA
      LOGICAL                 :: FLFLOE
!/
!/
! 1.  Preparations --------------------------------------------------- *
! 1.a Update times
!
!/T      WRITE (NDST,9010) TIME, TIC5, TI5
      TIC5(1) = TI5(1)
      TIC5(2) = TI5(2)

! 2.  Main loop over sea points -------------------------------------- *

      DO ISEA=1, NSEA 
!
        IX        = MAPSF(ISEA,1)
        IY        = MAPSF(ISEA,2)
        FLFLOE = ICE(ISEA) .EQ. 0 .OR. ICEH(ISEA) .EQ. 0
        IF ( FLFLOE) THEN          
          ICEF(ISEA) = 0.0
          ICEDMAX(ISEA) = 1000.0
        ELSE
          ICEF(ISEA) = ICEP5(IX,IY)
          ICEDMAX(ISEA) = ICEP5(IX,IY)          
        END IF
      END DO
!
      RETURN
!/T 9010 FORMAT ( ' TEST W3UIC5 : TIME     :',I9.8,I7.6/              &
!/T               '               OLD TICE :',I9.8,I7.6/              &
!/T               '               NEW TICE :',I9.8,I7.6)

!/
!/
!/ End of W3UIC5 ----------------------------------------------------- /
!/
      END SUBROUTINE W3UIC5
!/ ------------------------------------------------------------------- /      
      
      SUBROUTINE W3UICE ( A, VA )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         28-Mar-2014 |
!/                  +-----------------------------------+
!/
!/    19-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    20-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    11-Jan-2002 : Sub-grid ice.                       ( version 2.15 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    28-Jun-2005 : Adding MAPST2.                      ( version 3.07 )
!/                  Taking out initilization.
!/    11-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    15-May-2010 : Adding second field for icebergs    ( version 3.14 )
!/    13-Mar-2012 : Add initialization of UST on re-    ( version 4.07 )
!/                  activation of grid point.
!/    06-Jun-2012 : Porting bugfixes from 3.14 to 4.07  ( version 4.07 )
!/    28-Mar-2014 : Adapting to ICx source terms        ( version 4.18 )
!/
!  1. Purpose :
!
!     Update ice map in the wave model.
!
!  2. Method :
!
!     Points with an ice concentration larger than FICEN are removed
!     from the sea map in the wave model. Such points are identified
!     by negative numbers is the grid status map MAPSTA. For ice
!     points spectra are set to zero. Points from wich ice disappears
!     are initialized with a "small" JONSWAP spectrum, based on the
!     frequency SIG(NK-1) and the local wind direction.
!
!     In the case of icebergs, the iceberg attenuation coefficient is
!     added to the subgrid obstruction map.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!      (V)A     R.A.  I/O   Spectra in 1-D or 2-D representation
!                           (points to same address).
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
!     None.
!
!  7. Remarks :
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
!     !/S  Enable subroutine tracing.
!     !/T  Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, MAPSF, MAPSTA, MAPST2, &
                          NTH, NK, NSPEC, SIG, TH, DTH, FICEN
      USE W3WDATMD, ONLY: TIME, TICE, ICE, BERG, UST
!!    USE W3ADATMD, ONLY: U10, U10D, CG
      USE W3ADATMD, ONLY: CG
      USE W3IDATMD, ONLY: TIN, ICEI, BERGI
      USE W3PARALL, only : INIT_GET_JSEA_ISPROC, INIT_GET_ISEA
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(INOUT)     :: A(NTH,NK,0:NSEAL), VA(NSPEC,0:NSEAL)
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: IK, ITH, ISEA, JSEA, IX, IY, ISP
!/S      INTEGER, SAVE            :: IENT = 0
      INTEGER                 :: MAPICE(NY,NX), ISPROC
      LOGICAL                 :: LOCAL
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3UICE')
!
      LOCAL   = IAPROC .LE. NAPROC
!
!/T      WRITE (NDST,9000) FICEN
!/T      IF ( .NOT. LOCAL ) WRITE (NDST,9001)
!
! 1.  Preparations --------------------------------------------------- *
! 1.a Update times
!
!/T      WRITE (NDST,9010) TIME, TICE, TIN
      TICE(1) = TIN(1)
      TICE(2) = TIN(2)
!
! 1.b Process maps
!
!/IC0      MAPICE = MOD(MAPST2,2)
!/IC0      MAPST2 = MAPST2 - MAPICE
!
! 2.  Main loop over sea points -------------------------------------- *
!
      DO ISEA=1, NSEA
!
! 2.a Get grid counters
!
        IX        = MAPSF(ISEA,1)
        IY        = MAPSF(ISEA,2)
        ICE(ISEA) = ICEI(IX,IY)
        BERG(ISEA)= BERGI(IX,IY)
!
! 2.b Sea point to be de-activated..
!
!/IC0        IF ( ICEI(IX,IY).GE.FICEN .AND. MAPICE(IY,IX).EQ.0 ) THEN
!/IC0            MAPSTA(IY,IX) = - ABS(MAPSTA(IY,IX))
!/IC0            MAPICE(IY,IX) = 1
!AR: Take care here situation is not totally clear!
!/IC0            CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
!/IC0            IF (LOCAL .AND. (IAPROC .eq. ISPROC)) THEN
!/T                WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX),     &
!/T                                  ICEI(IX,IY), 'ICE (NEW)'
!/IC0                VA(:,JSEA) = 0.
!/IC0              ELSE
!/T                WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX),     &
!/T                                  ICEI(IX,IY), 'ICE (NEW X)'
!/IC0              END IF
!
!/IC0          ELSE IF ( ICEI(IX,IY).GE.FICEN ) THEN
!/T            WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX),         &
!/T                              ICEI(IX,IY), 'ICE'
!/IC0          END IF
!
! 2.b Ice point to be re-activated.
!
!/IC0        IF ( ICEI(IX,IY).LT.FICEN .AND. MAPICE(IY,IX).EQ.1 ) THEN
!
!/IC0            MAPICE(IY,IX) = 0
!/IC0            UST(ISEA)     = 0.05
!
!/IC0            IF ( MAPST2(IY,IX) .EQ. 0 ) THEN
!/IC0                MAPSTA(IY,IX) = ABS(MAPSTA(IY,IX))
!
!/IC0                CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
!/IC0                IF ( LOCAL .AND. (IAPROC .eq. ISPROC) ) THEN
!/T                    WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX), &
!/T                                      ICEI(IX,IY), 'SEA (NEW)'
!/IC0                    VA(:,JSEA) = 0.
!
!/IC0                  ELSE
!/T                    WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX), &
!/T                                      ICEI(IX,IY), 'SEA (NEW X)'
!/IC0                  END IF
!
!/IC0              ELSE
!/T                WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX),     &
!/T                                  ICEI(IX,IY), 'DIS'
!/IC0              END IF
!
!/IC0             ELSE IF ( ICEI(IX,IY).LT.FICEN ) THEN
!/T                WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX),     &
!/T                                  ICEI(IX,IY), 'SEA'
!
!/IC0          END IF
!
        END DO
!
! 3.  Update MAPST2 -------------------------------------------------- *
!
!/IC0      MAPST2 = MAPST2 + MAPICE
!
      RETURN
!
! Formats
!
!/T 9000 FORMAT ( ' TEST W3UICE : FICEN    :',F9.3)
!/T 9001 FORMAT ( ' TEST W3UICE : NO LOCAL SPECTRA')
!
!/T 9010 FORMAT ( ' TEST W3UICE : TIME     :',I9.8,I7.6/              &
!/T               '               OLD TICE :',I9.8,I7.6/              &
!/T               '               NEW TICE :',I9.8,I7.6)
!
!/T 9020 FORMAT ( ' TEST W3UICE : ISEA, IX, IY, MAP, ICE, STATUS :')
!/T 9021 FORMAT ( '           ',I8,3I4,F6.2,2X,A)
!/
!/ End of W3UICE ----------------------------------------------------- /
!/
      END SUBROUTINE W3UICE
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3ULEV ( A, VA )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         26-Sep-2012 |
!/                  +-----------------------------------+
!/
!/    15-Jan-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    21-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    30-Apr-2002 : Water level fixes.                  ( version 2.20 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    15-Jul-2005 : Adding drying out of points.        ( version 3.07 )
!/    11-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    23-Aug-2011 : Bug fix for UG grids : new boundary ( version 4.04 )
!/    13-Mar-2012 : Add initialization of UST on re-    ( version 4.07 )
!/                  activation of grid point.
!/    06-Jun-2012 : Porting bugfixes from 3.14 to 4.07  ( version 4.07 )
!/    26-Sep-2012 : Adding update from tidal analysis   ( version 4.08 )
!/
!  1. Purpose :
!
!     Update the water level.
!
!  2. Method :
!
!     The wavenumber grid is modified without modyfying the spectrum
!     (conservative linear interpolation to new grid).
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!      (V)A     R.A.  I/O  2-D and 1-D represetation of the spectra.
!     ----------------------------------------------------------------
!
!     Local variables
!     ----------------------------------------------------------------
!       KDMAX   Real   Deep water cut-off for kd.
!       WNO     R.A.   Old wavenumbers.
!       CGO     R.A.   Old group velocities.
!       OWN     R.A.   Old wavenumber band width.
!       DWN     R.A.   New wavenumber band width.
!       TA      R.A.   Auxiliary spectrum.
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
!     None.
!
!  7. Remarks :
!
!     - The grid is updated only if KDmin > KDMAX.
!     - The grid is updated for inactive points too.
!     - The local wavenumber bandwidth is DSIGMA/CG.
!     - The local spectrum is updated only if the grid is updated,
!       the grid point is not disabled (MAPST2) and if the change of
!       the lowest wavenumber exceeds RDKMIN times the band width.
!     - No spectral initialization for newly wet points.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S     Enable subroutine tracing.
!     !/T     Basic test output.
!     !/T2    Output of minimum relative depth per grid point.
!     !/T3    Spectra before and after
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, MAPSF, MAPSTA, MAPST2, &
                          ZB, DMIN, NK, NTH, NSPEC, SIG, DSIP,        &
                          MAPWN, MAPTH, FACHFA, GTYPE, UNGTYPE, W3SETREF
      USE W3WDATMD, ONLY: TIME, TLEV, WLV, UST
      USE W3ADATMD, ONLY: CG, WN, DW
      USE W3IDATMD, ONLY: TLN, WLEV
      USE W3SERVMD, ONLY: EXTCDE
      USE W3DISPMD, ONLY: WAVNU1
      USE W3TRIAMD, ONLY: SETUGIOBP
      USE W3TIMEMD
      USE W3PARALL, only : INIT_GET_JSEA_ISPROC, INIT_GET_ISEA
      USE W3PARALL, only : GET_JSEA_IBELONG
      USE W3DISPMD, ONLY: WAVNU1
!/TIDE      USE W3GDATMD, ONLY: YGRD
!/TIDE      USE W3IDATMD, ONLY: FLLEVTIDE, WLTIDE, NTIDE
!/TIDE      USE W3TIDEMD
!/SETUP      USE W3WDATMD, ONLY: ZETA_SETUP
!/SETUP      USE W3GDATMD, ONLY : DO_CHANGE_WLV


!/T3      USE W3ARRYMD, ONLY: PRT2DS
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(INOUT)     :: A(NTH,NK,0:NSEAL), VA(NSPEC,0:NSEAL)
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: ISEA, JSEA, IX, IY, IK, I1, I2,      &
                                 ISPEC, IK0, ITH
!/S      INTEGER, SAVE           :: IENT = 0
      INTEGER                 :: MAPDRY(NY,NX), ISPROC
      REAL                    :: DWO(NSEA), KDCHCK, WNO(0:NK+1),      &
                                 CGO(0:NK+1), DEPTH,                  &
                                 RDK, RD1, RD2, TA(NTH,NK),           &
                                 OWN(NK), DWN(NK)
      REAL                    :: KDMAX = 4., RDKMIN = 0.05
      REAL                    :: WLVeff
!/T3      REAL                    :: OUT(NK,NTH)
      LOGICAL                 :: LOCAL
      INTEGER                 :: IBELONG
!
!/TIDE      INTEGER          :: J
!/TIDE      INTEGER(KIND=4)  :: TIDE_KD0, INT24, INTDYS       ! "Gregorian day constant"
!/TIDE      REAL             :: WLEVTIDE, TIDE_ARG, WLEVTIDE2(1)
!/TIDE      REAL(KIND=8)     :: d1,h,TIDE_HOUR,HH,pp,s,p,enp,dh,dpp,ds,dp,dnp,tau
!/TIDE      REAL             :: FX(44),UX(44),VX(44)
!/DEBUGW3ULEV           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 1'
!/DEBUGW3ULEV           FLUSH(740+IAPROC)
      
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3ULEV')
!
      LOCAL   = IAPROC .LE. NAPROC
!/DEBUGW3ULEV           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 2'
!/DEBUGW3ULEV           FLUSH(740+IAPROC)
!
!/T      WRITE (NDST,9000) KDMAX, RDKMIN
!
! 1.  Preparations --------------------------------------------------- *
! 1.a Check NK
!
!/DEBUGW3ULEV           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 3'
!/DEBUGW3ULEV           FLUSH(740+IAPROC)
      IF ( NK .LT. 2 ) THEN
          IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000)
          CALL EXTCDE ( 1 )
        END IF
!
! 1.b Update times
!
!/T      WRITE (NDST,9010) TIME, TLEV
      TLEV = TLN
!/T      WRITE (NDST,9011) TLEV
!
! 1.c Extract dry point map, and residual MAPST2
!
      MAPDRY = MOD(MAPST2/2,2)
      MAPST2 = MAPST2 - 2*MAPDRY
!/DEBUGW3ULEV           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 4'
!/DEBUGW3ULEV           FLUSH(740+IAPROC)
!
! 1.d Update water levels and save old
!
!/TIDE        IF (FLLEVTIDE) THEN 
!/TIDE!          WRITE(6,*) 'TIME:',TIME
!/TIDE          TIDE_HOUR = TIME2HOURS(TIME)
!/TIDE!
!/TIDE!*  THE ASTRONOMICAL ARGUMENTS ARE CALCULATED BY LINEAR APPROXIMATION
!/TIDE!*  AT THE MID POINT OF THE ANALYSIS PERIOD.
!/TIDE          d1=TIDE_HOUR/24.d0
!/TIDE          TIDE_KD0= 2415020
!/TIDE          d1=d1-dfloat(TIDE_kd0)-0.5d0
!/TIDE          call astr(d1,h,pp,s,p,enp,dh,dpp,ds,dp,dnp)
!/TIDE          INT24=24
!/TIDE          INTDYS=int((TIDE_HOUR+0.00001)/INT24)
!/TIDE          HH=TIDE_HOUR-dfloat(INTDYS*INT24)
!/TIDE          TAU=HH/24.D0+H-S
!/TIDE        END IF
!/TIDE!
!/TIDE!  ONLY THE FRACTIONAL PART OF A SOLAR DAY NEED BE RETAINED FOR COMPU-
!/TIDE!  TING THE LUNAR TIME TAU.
!/TIDE!
!/DEBUGW3ULEV           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 5'
!/DEBUGW3ULEV           FLUSH(740+IAPROC)
      DO ISEA=1, NSEA
        IX     = MAPSF(ISEA,1)
        IY     = MAPSF(ISEA,2)
        DWO(ISEA) = DW(ISEA)
!
!/TIDE        IF (FLLEVTIDE) THEN 
!/TIDE! VUF should be updated only if latitude changes significantly ...
!/TIDE          CALL SETVUF_FAST(h,pp,s,p,enp,dh,dpp,ds,dp,dnp,tau,YGRD(IY,IX),FX,UX,VX)
!/TIDE          WLEVTIDE = WLTIDE(IX,IY,1,1)
!/TIDE !Verification 
!/TIDE !          IF (ISEA.EQ.1) THEN 
!/TIDE
!/TIDE            TIDE_AMPC(1:NTIDE,1)=WLTIDE(IX,IY,1:NTIDE,1)
!/TIDE            TIDE_PHG(1:NTIDE,1)=WLTIDE(IX,IY,1:NTIDE,2)
!/TIDE!
!/TIDE   !           WRITE(991,'(A,F20.2,13F8.3)') 'TEST ISEA 0:',    &
!/TIDE   !                       d1,H,S,TAU,pp,s,p,enp,dh,dpp,ds,dp,dnp,YGRD(IY,IX)
!/TIDE              J=1
!/TIDE   !           WRITE(991,'(A,4I9,F12.0,3F8.3,I4,X,A)') 'TEST ISEA 1:',IX,J,TIME,TIDE_HOUR,    &
!/TIDE   !                       FX(J),UX(J),VX(J),TIDE_INDEX2(J),TIDECON_ALLNAMES(TIDE_INDEX2(J))
!/TIDE            DO J=2,TIDE_MF
!/TIDE              TIDE_ARG=(VX(J)+UX(J))*twpi-WLTIDE(IX,IY,J,2)*DERA
!/TIDE              WLEVTIDE =WLEVTIDE+FX(J)*WLTIDE(IX,IY,J,1)*COS(TIDE_ARG)
!/TIDE   !           WRITE(991,'(A,4I9,F12.0,3F8.3,I4,X,A)') 'TEST ISEA 1:',IX,J,TIME,TIDE_HOUR,    &
!/TIDE   !                       FX(J),UX(J),VX(J),TIDE_INDEX2(J),TIDECON_ALLNAMES(TIDE_INDEX2(J))
!/TIDE              END DO
!/TIDE            DO J=1,TIDE_MF
!/TIDE   !           WRITE(991,'(A,4I9,F12.0,5F8.3)') 'TEST ISEA 2:',IX,J,TIME,TIDE_HOUR,    &
!/TIDE   !                       FX(J),UX(J),VX(J),TIDE_AMPC(J,1),TIDE_PHG(J,1)
!/TIDE              END DO
!/TIDE   !         WRITE(991,'(A,3F7.3)') '#:',WLEV(IX,IY),WLEVTIDE,WLEV(IX,IY)-WLEVTIDE

!/TIDE   !         CLOSE(991)
!/TIDE   !         END IF
!/TIDE ! End of verification
!/TIDE            WLV(ISEA) = WLEVTIDE
!/TIDE          ELSE 
!
        WLV(ISEA) = WLEV(IX,IY)
        WLVeff=WLV(ISEA)
!/SETUP     IF (DO_CHANGE_WLV) THEN
!/SETUP       WLVeff=WLVeff + ZETA_SETUP(ISEA)
!/SETUP     END IF
!/TIDE          ENDIF 
        DW (ISEA) = MAX ( 0. , WLVeff-ZB(ISEA) )
        END DO
!/DEBUGW3ULEV           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 6'
!/DEBUGW3ULEV           FLUSH(740+IAPROC)
!
! 2.  Loop over all sea points --------------------------------------- *
!
!/T2      WRITE (NDST,9020)
!
      DO ISEA=1, NSEA
!
        IX     = MAPSF(ISEA,1)
        IY     = MAPSF(ISEA,2)
!
! 2.a Check if deep water
!
        KDCHCK = WN(1,ISEA) * MIN( DWO(ISEA) , DW(ISEA) )
        IF ( KDCHCK .LT. KDMAX ) THEN
!
! 2.b Update grid and save old grid
!
            DEPTH  = MAX ( DMIN, DW(ISEA) )
!
            DO IK=0, NK+1
              WNO(IK) = WN(IK,ISEA)
              CGO(IK) = CG(IK,ISEA)
!
!             Calculate wavenumbers and group velocities.
              CALL WAVNU1(SIG(IK),DEPTH,WN(IK,ISEA),CG(IK,ISEA))
!
              END DO
!
            DO IK=1, NK
              OWN(IK) = DSIP(IK) / CGO(IK)
              DWN(IK) = DSIP(IK) / CG(IK,ISEA)
              END DO
!
! 2.c Process dry points
!
            IF ( WLV(ISEA)-ZB(ISEA) .LE.0. ) THEN
                IF ( MAPDRY(IY,IX) .EQ. 0 ) THEN
                    CALL GET_JSEA_IBELONG(ISEA, JSEA, IBELONG)
                    IF ( LOCAL .AND. (IBELONG .eq. 1) ) THEN
                        VA(:,JSEA) = 0.
                      END IF
                    MAPDRY(IY,IX) = 1
                    MAPSTA(IY,IX) = -ABS(MAPSTA(IY,IX))
!/T2                    WRITE (NDST,9021) ISEA, WLV(ISEA)-ZB(ISEA),   &
!/T2                                      0., 0., '  (NEW DRY)'
!/T2                  ELSE
!/T2                    WRITE (NDST,9021) ISEA, WLV(ISEA)-ZB(ISEA),   &
!/T2                                      0., 0., '  (DRY)'
                  ENDIF
                CYCLE
             END IF
!
! 2.d Process new wet point
!
            IF (WLV(ISEA)-ZB(ISEA).GT.0. .AND. MAPDRY(IY,IX).EQ.1) THEN
                MAPDRY(IY,IX) = 0
!
! Resets the spectrum to zero
!
                    CALL GET_JSEA_IBELONG(ISEA, JSEA, IBELONG)
                    IF ( LOCAL .AND. (IBELONG .eq. 1) ) THEN
                        VA(:,JSEA) = 0.
                      END IF
!
                UST(ISEA)     = 0.05
                IF ( MAPST2(IY,IX) .EQ. 0 ) THEN
                    MAPSTA(IY,IX) = ABS(MAPSTA(IY,IX))
!/T2                    WRITE (NDST,9021) ISEA, WLV(ISEA)-ZB(ISEA),   &
!/T2                                      0., 0., '  (NEW WET)'
!/T2                  ELSE
!/T2                    WRITE (NDST,9021) ISEA, WLV(ISEA)-ZB(ISEA),   &
!/T2                                      0., 0., '  (NEW WET INACTIVE)'
                  END IF
                CYCLE
             END IF
!
! 2.e Check if ice on grid point, or if grid changes negligible
!
            RDK    = ABS(WNO(1)-WN(1,ISEA)) / DWN(1)
!
!/T2            IF ( MAPSTA(IY,IX) .LT. 0 ) THEN
!/T2                WRITE (NDST,9021)                                 &
!/T2                       ISEA, DW(ISEA), KDCHCK, RDK, '  (INACTIVE)'
!/T2              ELSE IF ( RDK .LT. RDKMIN ) THEN
!/T2                WRITE (NDST,9021)                                 &
!/T2                       ISEA, DW(ISEA), KDCHCK, RDK, '  (NEGL)'
!/T2              ELSE
!/T2                WRITE (NDST,9021)                                 &
!/T2                       ISEA, DW(ISEA), KDCHCK, RDK, ' '
!/T2              END IF
!
            IF ( RDK.LT.RDKMIN .OR. MAPSTA(IY,IX).LT.0 ) CYCLE
            CALL GET_JSEA_IBELONG(ISEA, JSEA, IBELONG)
            IF ( IBELONG .eq. 0) CYCLE
!
            IF ( .NOT. LOCAL ) CYCLE
!
! 2.d Save discrete actions and clean spectrum
!
            DO IK=1, NK
              DO ITH=1, NTH
!/T3                OUT(IK,ITH) = A(ITH,IK,JSEA) * SIG(IK) / CGO(IK)
                TA(ITH,IK) = A(ITH,IK,JSEA) * OWN(IK)
                END DO
              END DO
!
            VA(:,JSEA) = 0.
!
!/T3            CALL PRT2DS ( NDST, NK, NK, NTH, OUT, SIG, ' ',    &
!/T3                 TPI, 0., 1.E-5, 'F(f,th)', 'm2s', 'Before' )
!
! 2.e Redistribute discrete action density
!
            IF ( WNO(1) .LT. WN(1,ISEA) ) THEN
                IK0    = 1
                I1     = 0
                I2     = 1
  220           CONTINUE
                IK0    = IK0 + 1
                IF ( IK0 .GT. NK+1 ) GOTO 251
                IF ( WNO(IK0) .GE. WN(1,ISEA) ) THEN
                     IK0    = IK0 - 1
                   ELSE
                     GOTO 220
                   END IF
              ELSE
                IK0    = 1
                I1     = 1
                I2     = 2
              END IF
!
            DO 250, IK=IK0, NK
!
  230         CONTINUE
              IF ( WNO(IK) .GT. WN(I2,ISEA) ) THEN
                  I1     = I1 + 1
                  IF ( I1 .GT. NK ) GOTO 250
                  I2     = I1 + 1
                  GOTO 230
                END IF
!
              IF ( I1 .EQ. 0 ) THEN
                  RD1    = ( WN(1,ISEA) - WNO(IK) ) / DWN(1)
                  RD2    = 1. - RD1
                ELSE
                  RD1    = ( WN(I2,ISEA) - WNO(IK) ) /                &
                           ( WN(I2,ISEA) - WN(I1,ISEA) )
                  RD2    = 1. - RD1
                END IF
!
                IF ( I1 .GE. 1 ) THEN
                    DO ITH=1, NTH
                      A(ITH,I1,JSEA) = A(ITH,I1,JSEA) + RD1*TA(ITH,IK)
                      END DO
                  END IF
!
                IF ( I2 .LE. NK ) THEN
                    DO ITH=1, NTH
                      A(ITH,I2,JSEA) = A(ITH,I2,JSEA) + RD2*TA(ITH,IK)
                      END DO
                  END IF
!
  250         CONTINUE
  251       CONTINUE
!
! 2.f Convert discrete action densities to spectrum
!
            DO ISPEC=1, NSPEC
              VA(ISPEC,JSEA) = VA(ISPEC,JSEA) / DWN(MAPWN(ISPEC))
              END DO
!
! 2.f Add tail if necessary
!
            IF ( I2.LE.NK .AND. RD2.LE.0.95 ) THEN
                DO IK=MAX(I2,2), NK
                  DO ITH=1, NTH
                    A(ITH,IK,JSEA) = FACHFA * A(ITH,IK-1,JSEA)
                    END DO
                  END DO
              END IF
!
!/T3            DO ISPEC=1, NSPEC
!/T3              IK          = MAPWN(ISPEC)
!/T3              ITH         = MAPTH(ISPEC)
!/T3              OUT(IK,ITH) = A(ITH,IK,JSEA) * SIG(IK) / CG(IK,ISEA)
!/T3              END DO
!
!/T3            CALL PRT2DS ( NDST, NK, NK, NTH, OUT, SIG, ' ',    &
!/T3                 TPI, 0., 1.E-5, 'F(f,th)', 'm2s', 'After' )
!
!/T2          ELSE
!/T2            WRITE (NDST,9021) ISEA, KDCHCK, '  (DEEP)'
          END IF
!
        END DO
!/DEBUGW3ULEV           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 7'
!/DEBUGW3ULEV           FLUSH(740+IAPROC)
!
! 3.  Reconstruct new MAPST2 ----------------------------------------- *
!
      MAPST2 = MAPST2 + 2*MAPDRY
!
! 4. Re-generates the boundary data ---------------------------------- *
!
!/DEBUGW3ULEV           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 8'
!/DEBUGW3ULEV           FLUSH(740+IAPROC)
      IF (GTYPE.EQ.UNGTYPE) THEN 
!/DEBUGW3ULEV           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 9'
!/DEBUGW3ULEV           FLUSH(740+IAPROC)
        CALL SETUGIOBP
!/DEBUGW3ULEV           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 10'
!/DEBUGW3ULEV           FLUSH(740+IAPROC)
!/REF1      ELSE 
!/REF1        CALL W3SETREF
      ENDIF 
!/DEBUGW3ULEV           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 11'
!/DEBUGW3ULEV           FLUSH(740+IAPROC)
!
      RETURN
!
! Formats
!
 1000 FORMAT (/' *** ERROR W3ULEV *** '/                              &
               '     THIS ROUTINE REQUIRES NK > 1 '/)
!
!/T 9000 FORMAT ( ' TEST W3ULEV : KDMAX    :',F6.1/                   &
!/T               '               RDKMIN   :',F8.3)
!
!/T 9010 FORMAT ( ' TEST W3ULEV : TIME     :',I9.8,I7.6/              &
!/T               '               OLD TLEV :',I9.8,I7.6)
!/T 9011 FORMAT ( '               NEW TLEV :',I9.8,I7.6)
!
!/T2 9020 FORMAT ( ' TEST W3ULEV : LOOP OVER ALL POINTS:',            &
!/T2               ' ISEA, DW, KDMIN, RDK : ')
!/T2 9021 FORMAT ( '             ',I6,F8.2,F6.2,F7.3,A)
!/
!/ End of W3ULEV ----------------------------------------------------- /
!/
      END SUBROUTINE W3ULEV
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3UTRN ( TRNX, TRNY )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         30-Oct-2009 |
!/                  +-----------------------------------+
!/
!/    02-Apr-2001 : Origination.                        ( version 2.10 )
!/    11-Jan-2002 : Sub-grid ice.                       ( version 2.15 )
!/    30-Apr-2002 : Change to ICE on storage grid.      ( version 2.20 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    11-Jan-2007 : Clean-up for boundary points.       ( version 3.10 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/
!  1. Purpose :
!
!     Update cell boundary transparencies for general use in propagation
!     routines.
!
!  2. Method :
!
!     Two arrays are generated with the size (NY*NX,-1:1). The value 
!     at (IXY,-1) indicates the transparency to be used if the lower
!     or left boundary is an inflow boundary. (IXY,1) is used if the
!     upper or right boundary is an inflow boundary. (IXY,0) is used
!     for all other cases (by definition full transparency).
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       TRNX/Y  R.A.   I   Transparencies from model defintion file.
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
!     None.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/S      Enable subroutine tracing.
!       !/T      Basic test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSTA, MAPSF,                &
                          TRFLAG, FICE0, FICEN, FICEL,                &
                          RLGTYPE, CLGTYPE, GTYPE, FLAGLL,            &
                          HPFAC, HQFAC, FFACBERG
      USE W3WDATMD, ONLY: ICE, BERG
      USE W3ADATMD, ONLY: ATRNX, ATRNY
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/    
      REAL, INTENT(IN)        :: TRNX(NY*NX), TRNY(NY*NX)
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: ISEA, IX, IY, IXY, IXN, IXP, IYN, IYP
!/S      INTEGER, SAVE           :: IENT = 0
!/T      INTEGER                 :: ILEV, NLEV

      REAL                    :: TRIX(NY*NX), TRIY(NY*NX), DX, DY,    &
                                 LICE0, LICEN
!/T      REAL                    :: LEVS(0:10)
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3UTRN')
!/T      WRITE (NDST,9000) TRFLAG
!
! 1.  Preparations --------------------------------------------------- *
!
      ATRNX = 1.
      ATRNY = 1.
!/T      WRITE (NDST,9001) 'INITIALIZING ATRNX/Y'
!
! 2.  Filling arrays from TRNX/Y for obstructions -------------------- *
! 2.a TRFLAG = 0, no action needed
      IF ( TRFLAG .EQ. 0 ) THEN
!/T          WRITE (NDST,9001) 'NO FURTHER ACTION REQUIRED'
          RETURN
!
! 2.b TRFLAG = 1,3: TRNX/Y defined at boundaries
!
        ELSE IF ( TRFLAG.EQ.1 .OR. TRFLAG.EQ.3 .OR. TRFLAG.EQ.5 ) THEN
!/T          WRITE (NDST,9001) 'DATA APPLIED AT CELL BOUNDARIES'
!/T          LEVS   = 0.
!
          DO ISEA=1, NSEA
!
            IX            = MAPSF(ISEA,1)
            IY            = MAPSF(ISEA,2)
            IXY           = MAPSF(ISEA,3)
            IF ( IX .EQ. 1 ) THEN
                ATRNX(IXY,-1) = TRNX(IY+(NX-1)*NY)
                ATRNX(IXY, 1) = TRNX(IXY)
              ELSE IF ( IX .EQ. NX ) THEN
                ATRNX(IXY,-1) = TRNX(IXY-NY)
                ATRNX(IXY, 1) = TRNX(IY)
              ELSE
                ATRNX(IXY,-1) = TRNX(IXY-NY)
                ATRNX(IXY, 1) = TRNX(IXY)
              END IF
            ATRNY(IXY,-1) = TRNY(IXY-1)
            ATRNY(IXY, 1) = TRNY(IXY)
!
!/T            ILEV          = NINT(10.*MIN(TRNX(IXY),TRNY(IXY)))
!/T            LEVS(ILEV)    = LEVS(ILEV) + 1.
!
            END DO
!
! 2.c TRFLAG = 2,4: TRNX/Y defined at cell centers
!
        ELSE
!/T          WRITE (NDST,9001) 'DATA APPLIED AT CELL CENTERS'
!/T          LEVS   = 0.
!
          DO ISEA=1, NSEA
!
            IX            = MAPSF(ISEA,1)
            IY            = MAPSF(ISEA,2)
            IXY           = MAPSF(ISEA,3)
!
            IF ( IX .EQ. 1 ) THEN
                IXN    = IY + (NX-1)*NY
                IXP    = IY +  IX   *NY
              ELSE IF ( IX .EQ. NX ) THEN
                IXN    = IY + (IX-2)*NY
                IXP    = IY
              ELSE
                IXN    = IY + (IX-2)*NY
                IXP    = IY +  IX   *NY
              END IF
!
            IF ( IY .EQ. 1 ) THEN
                IYN    = IXY
                IYP    = IXY + 1
              ELSE IF ( IY .EQ. NY ) THEN
                IYN    = IXY - 1
                IYP    = IXY
              ELSE
                IYN    = IXY - 1
                IYP    = IXY + 1
              END IF
!
! factors 0.5 in first term and 2. in second term cancel
!
            ATRNX(IXY,-1) = (1.+TRNX(IXY)) * TRNX(IXN)/(1.+TRNX(IXN))
            ATRNX(IXY, 1) = (1.+TRNX(IXY)) * TRNX(IXP)/(1.+TRNX(IXP))
            ATRNY(IXY,-1) = (1.+TRNY(IXY)) * TRNY(IYN)/(1.+TRNY(IYN))
            ATRNY(IXY, 1) = (1.+TRNY(IXY)) * TRNY(IYP)/(1.+TRNY(IYP))
!
            IF ( MAPSTA(IY,IX) .EQ. 2 ) THEN
                IF ( IX .EQ. 1  ) THEN
                    ATRNX(IXY,-1) = 1.
                  ELSE IF ( MAPSTA( IY ,IX-1) .LE. 0 ) THEN
                    ATRNX(IXY,-1) = 1.
                  END IF
                IF ( IX .EQ. NX ) THEN
                     ATRNX(IXY, 1) = 1.
                  ELSE IF ( MAPSTA( IY ,IX+1) .LE. 0 ) THEN
                     ATRNX(IXY, 1) = 1.
                  END IF
                IF ( IY .EQ. 1  ) THEN
                     ATRNY(IXY,-1) = 1.
                  ELSE IF ( MAPSTA(IY-1, IX ) .LE. 0 ) THEN
                     ATRNY(IXY,-1) = 1.
                  END IF
                IF ( IY .EQ. NY ) THEN
                     ATRNY(IXY, 1) = 1.
                  ELSE IF ( MAPSTA(IY+1, IX ) .LE. 0 ) THEN
                     ATRNY(IXY, 1) = 1.
                  END IF
              END IF
!
!/T            ILEV          = NINT(10.*MIN(TRNX(IXY),TRNY(IXY)))
!/T            LEVS(ILEV)    = LEVS(ILEV) + 1.
!
            END DO
        END IF
!
!/T      WRITE(NDST,9010) 'ISLANDS'
!/T      NLEV   = 0
!/T      DO ILEV=0, 10
!/T        WRITE (NDST,9011) ILEV, LEVS(ILEV)/REAL(NSEA)
!/T        NLEV = NLEV + NINT(LEVS(ILEV))
!/T        END DO
!
! 3.  Adding ice to obstructions ------------------------------------- *
! 3.a TRFLAG < 3, no action needed
!
      IF ( TRFLAG.LT.3 .OR. FICEN-FICE0.LT.1.E-6 ) THEN
!/T          WRITE (NDST,9001) 'NO ICE ACTION REQUIRED'
          RETURN
!
! 3.b TRFLAG = 3,4: Calculate ice transparencies
!
        ELSE
!/T          WRITE (NDST,9001) 'CALCULATE ICE TRANSPARENCIES'
!/T          LEVS   = 0.
          TRIX   = 1.
          TRIY   = 1.
!
          DO ISEA=1, NSEA
!
            IX     = MAPSF(ISEA,1)
            IY     = MAPSF(ISEA,2)
            IXY    = MAPSF(ISEA,3)
!
            DX     = HPFAC(IY,IX)
            DY     = HQFAC(IY,IX)
            IF ( FLAGLL ) THEN
                DX     = DX * RADIUS * DERA
                DY     = DY * RADIUS * DERA
              END IF

!
!/IC0            IF (ICE(ISEA).GT.0) THEN
!/IC0              IF (FICEL.GT.0.) THEN
!/IC0                TRIX(IXY) = EXP(-ICE(ISEA)*DX/FICEL)
!/IC0                TRIY(IXY) = EXP(-ICE(ISEA)*DY/FICEL)
!/IC0              ELSE 
! Otherwise: original Tolman expression (Tolman 2003)
!/IC0                LICE0  = FICE0*DX
!/IC0                LICEN  = FICEN*DX
!/IC0                TRIX(IXY) = ( LICEN - ICE(ISEA)*DX ) / ( LICEN - LICE0 )

! begin temporary notes
!                TRIX = (   LICEN    - ICE(ISEA)*DX ) / (   LICEN -      LICE0 )
!    thus, it is TRIX=  ( (FICEN*DX) - ICE(ISEA)*DX ) / ( (FICEN*DX) - (FICE0*DX) )
!    thus, it is TRIX=  (   FICEN -    ICE(ISEA) )   /  (  FICEN     -   FICE0 )
!    in other words, the variables DX DY are not used
!                    and the variables LICE0 LICEN are not necessary.
! end temporary notes

!/IC0                LICE0  = FICE0*DY
!/IC0                LICEN  = FICEN*DY
!/IC0                TRIY(IXY) = ( LICEN - ICE(ISEA)*DY ) / ( LICEN - LICE0 )
!/IC0                END IF
!
!/IC0              TRIX(IXY) = MAX ( 0. , MIN ( 1. , TRIX(IXY) ) )
!/IC0              TRIY(IXY) = MAX ( 0. , MIN ( 1. , TRIY(IXY) ) ) 
!/IC0              END IF          
!
!  Adding iceberg attenuation
!
            IF (BERG(ISEA).GT.0) THEN
              TRIX(IXY) = TRIX(IXY)*EXP(-BERG(ISEA)*FFACBERG  *DX*0.0001)
              TRIY(IXY) = TRIY(IXY)*EXP(-BERG(ISEA)*FFACBERG  *DY*0.0001)
              END IF
!
!/T            ILEV          = NINT(10.*MIN(TRIX(IXY),TRIY(IXY)))
!/T            LEVS(ILEV)    = LEVS(ILEV) + 1.
!
          END DO
!
!/T      WRITE(NDST,9010) 'ICE'
!/T      NLEV   = 0
!/T      DO ILEV=0, 10
!/T        WRITE (NDST,9011) ILEV, LEVS(ILEV)/REAL(NSEA)
!/T        NLEV = NLEV + NINT(LEVS(ILEV))
!/T        END DO
!
! 3.c Combine transparencies, ice always defined at cell center !
!
          DO ISEA=1, NSEA
!
            IX            = MAPSF(ISEA,1)
            IY            = MAPSF(ISEA,2)
            IXY           = MAPSF(ISEA,3)
!
            IF ( IX .EQ. 1 ) THEN
                IXN    = IY + (NX-1)*NY
                IXP    = IY +  IX   *NY
              ELSE IF ( IX .EQ. NX ) THEN
                IXN    = IY + (IX-2)*NY
                IXP    = IY
              ELSE
                IXN    = IY + (IX-2)*NY
                IXP    = IY +  IX   *NY
              END IF
!
            IF ( IY .EQ. 1 ) THEN
                IYN    = IXY
                IYP    = IXY + 1
              ELSE IF ( IY .EQ. NY ) THEN
                IYN    = IXY - 1
                IYP    = IXY
              ELSE
                IYN    = IXY - 1
                IYP    = IXY + 1
              END IF
!
            ATRNX(IXY,-1) = ATRNX(IXY,-1)                             &
                          * (1.+TRIX(IXY)) * TRIX(IXN)/(1.+TRIX(IXN))
            ATRNX(IXY, 1) = ATRNX(IXY, 1)                             &
                          * (1.+TRIX(IXY)) * TRIX(IXP)/(1.+TRIX(IXP))
            ATRNY(IXY,-1) = ATRNY(IXY,-1)                             &
                          * (1.+TRIY(IXY)) * TRIY(IYN)/(1.+TRIY(IYN))
            ATRNY(IXY, 1) = ATRNY(IXY, 1)                             &
                          * (1.+TRIY(IXY)) * TRIY(IYP)/(1.+TRIY(IYP))
!
            END DO
!
        END IF
!
      RETURN
!
! Formats
!
!/T 9000 FORMAT ( ' TEST W3UTRN : TRFLAG = ',I3)
!/T 9001 FORMAT ( ' TEST W3UTRN : ',A)
!/T 9010 FORMAT ( ' TEST W3UTRN : OBSTRICTION LEVELS FOR ',A,' :')
!/T 9011 FORMAT ( '             ',I4,F8.5)
!/
!/ End of W3UTRN ----------------------------------------------------- /
!/
      END SUBROUTINE W3UTRN
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3DZXY( ZZ, ZUNIT, DZZDX, DZZDY )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |         W. E. Rogers, NRL         |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Dec-2010 |
!/                  +-----------------------------------+
!/
!/    30-Oct-2009 : Origination.                        ( version 3.14 )
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/
!  1. Purpose :
!
!     Calculate derivatives of a field.
!
!  2. Method :
!
!     Derivatives are calculated in m/m from the longitude/latitude
!     grid, central in space for iternal points, one-sided for
!     coastal points.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       ZZ      R.A.  I   Field to calculate derivatives of.
!       ZUNIT   R.A.  I   Units of ZZ (used for test output).
!       DZZDX   R.A.  O   Derivative in X-direction (W-E).
!       DZZDY   R.A.  O   Derivative in Y-direction (S-N).
!       IXP: IX plus 1 (with branch cut incorporated)
!       IYP, IXM, IYM: ditto
!       IXPS: value to use for IXP if IXPS is not masked. 
!             (use IX if masked)
!       IYPS, IXMS, IYMS : ditto
!       IXTRPL : in case of needing IY+1 for IY=NY, IX needs to be
!                modified (tripole grid only)
!       IXTRPLS : value to use for IXTRPL if IXTRPLS is not masked
!             (use IX if masked)
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
!     This routine replaces the functionality of W3DDXY and W3DCXY.
!     NB: subroutine "W3CGDM" has a similar purpose.
!     Output arrays are always initialized to zero.
!
!  8. Structure :
!
!     ----------------------------------------
!      1.  Preparations
!        a Initialize arrays
!        b Set constants
!      2.  Derivatives in X-direction (W-E).
!      3.  Derivatives in Y-direction (S-N).
!     ----------------------------------------
!
!  9. Switches :
!
!       !/S   Enable subroutine tracing.
!       !/T   Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSTA, MAPFS, MAPFS, &
             DPDX, DPDY, DQDX, DQDY, FLAGLL, ICLOSE,          &
             ICLOSE_NONE, ICLOSE_SMPL, ICLOSE_TRPL
      USE W3ODATMD, ONLY: NDSE, IAPROC, NAPERR, NAPROC
      USE W3SERVMD, ONLY: EXTCDE
!/T      USE W3ARRYMD, ONLY : PRTBLK
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      REAL, INTENT(IN)        :: ZZ(NSEA)
      CHARACTER, INTENT(IN)   :: ZUNIT*(*)
      REAL, INTENT(OUT)       :: DZZDX(NY,NX), DZZDY(NY,NX)
      INTEGER                 :: ISEA, IX, IY, IXP, IXM, IYP, IYM
!/T      INTEGER                 :: ISX, ISY, MAPOUT(NX,NY)
!/S      INTEGER, SAVE           :: IENT = 0
!/T      INTEGER, SAVE           :: NXS = 49
      REAL                    :: DFAC , STX, STY
      INTEGER                 :: IXPS,IYPS,IXMS,IYMS,IXTRPL,IXTRPLS
      INTEGER                 :: IXSTART,IXEND
!/T      REAL                    :: XOUT(NX,NY)
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3DZXY')
!
! 1.  Preparations --------------------------------------------------- *

! 1.a Initialize arrays
!
      DZZDX = 0.
      DZZDY = 0.
!
! 1.b Set constants
!

      IF ( FLAGLL ) THEN
         DFAC   = 1. / ( DERA * RADIUS )
      ELSE
         DFAC   = 1.
      END IF

!
! 2.  Derivatives in X-direction (W-E) and Y-direction (S-N) ----- *
!

! 2a. All points previously done in 2a,2b,2c of v4.18 done in 2a now:
      IF ( ICLOSE.EQ.ICLOSE_NONE ) THEN
         IXSTART=2
         IXEND=NX-1
      ELSE
         IXSTART=1
         IXEND=NX
      ENDIF

      DO IY=2, NY-1
         DO IX=IXSTART,IXEND
            IF ( MAPSTA(IY,IX) .NE. 0 ) THEN
               STX    = 0.5
               IF (IX.EQ.NX)THEN
                  IXPS=1
               ELSE
                  IXPS=IX+1
               ENDIF

               IF (MAPSTA(IY,IXPS).EQ.0) THEN
                  IXP    = IX
                  STX    = 1.0
               ELSE
                  IXP    = IXPS
               END IF

               IF(IX.EQ.1)THEN
                  IXMS=NX
               ELSE
                  IXMS=IX-1
               ENDIF

               IF (MAPSTA(IY,IXMS).EQ.0) THEN
                  IXM    = IX
                  STX    = 1.0
               ELSE
                  IXM    = IXMS
               END IF
               STY    = 0.5
               IYPS=IY+1
               IF (MAPSTA(IYPS,IX).EQ.0) THEN
                  IYP    = IY
                  STY    = 1.0
               ELSE
                  IYP    = IYPS
               END IF
               IYMS=IY-1
               IF (MAPSTA(IYMS,IX).EQ.0) THEN
                  IYM    = IY
                  STY    = 1.0
               ELSE
                  IYM    = IYMS
               END IF
               DZZDX(IY,IX) = (ZZ(MAPFS(IY ,IXP))-ZZ(MAPFS(IY ,IXM))) * STX * DPDX(IY,IX) &
                    + (ZZ(MAPFS(IYP,IX ))-ZZ(MAPFS(IYM,IX ))) * STY * DQDX(IY,IX)
               DZZDY(IY,IX) = (ZZ(MAPFS(IY ,IXP))-ZZ(MAPFS(IY ,IXM))) * STX * DPDY(IY,IX) &
                    + (ZZ(MAPFS(IYP,IX ))-ZZ(MAPFS(IYM,IX ))) * STY * DQDY(IY,IX)
               DZZDX(IY,IX) = DZZDX(IY,IX) * DFAC
               DZZDY(IY,IX) = DZZDY(IY,IX) * DFAC
            END IF
         END DO
      END DO

! 2b. column IY=NY for tripole case
! This is more complex, since for these two points: (IYP,IX) (IYM,IX),
!     not only is the first index different (IYP.NE.IYM), but also the
!     second index is different (IX.NE.IX)!
      IF ( ICLOSE.EQ.ICLOSE_TRPL ) THEN

         IY=NY
         DO IX=1, NX
            IF ( MAPSTA(IY,IX) .NE. 0 ) THEN

               STX    = 0.5
               
               IF (IX.EQ.NX)THEN
                  IXPS=1
               ELSE
                  IXPS=IX+1
               ENDIF
               IF (MAPSTA(IY,IXPS).EQ.0) THEN
                  IXP    = IX
                  STX    = 1.0
               ELSE
                  IXP    = IXPS
               END IF
               
               IF(IX.EQ.1)THEN
                  IXMS=NX
               ELSE
                  IXMS=IX-1
               ENDIF
               IF (MAPSTA(IY,IXMS).EQ.0) THEN
                  IXM    = IX
                  STX    = 1.0
               ELSE
                  IXM    = IXMS
               END IF

               STY    = 0.5

!..............next point: j+1: tripole: j==>j+1==>j and i==>ni-i+1
!..............i.e. target point is MAPFS(IY,(NX-IX+1))
               IXTRPLS=NX-IX+1
               IF (MAPSTA(IY,IXTRPLS).EQ.0) THEN
                  IXTRPL = IX
                  STY    = 1.0
               ELSE
                  IXTRPL=IXTRPLS
               END IF

               IYMS=IY-1
               IF (MAPSTA(IYMS,IX).EQ.0) THEN
                  IYM    = IY
                  STY    = 1.0
               ELSE
                  IYM    = IYMS
               END IF

! tripole grid: (IYP,IX) is replaced with (IY,IXTRPL)
               DZZDX(IY,IX) = (ZZ(MAPFS(IY ,IXP))-ZZ(MAPFS(IY ,IXM))) * STX * DPDX(IY,IX) &
                          + (ZZ(MAPFS(IY,IXTRPL))-ZZ(MAPFS(IYM,IX ))) * STY * DQDX(IY,IX)
               DZZDY(IY,IX) = (ZZ(MAPFS(IY ,IXP))-ZZ(MAPFS(IY ,IXM))) * STX * DPDY(IY,IX) &
                          + (ZZ(MAPFS(IY,IXTRPL))-ZZ(MAPFS(IYM,IX ))) * STY * DQDY(IY,IX)
               DZZDX(IY,IX) = DZZDX(IY,IX) * DFAC
               DZZDY(IY,IX) = DZZDY(IY,IX) * DFAC
            END IF
         END DO

      END IF ! IF ( ICLOSE.EQ.ICLOSE_TRPL ) THEN

!
! 3.  Test output of fields ------------------------------------------ *
!
!/T      WRITE (NDST,9010)
!/T      ISX   = 1 + NX/NXS
!/T      ISY   = 1 + NY/NXS
!/T      DO IY=1, NY
!/T        DO IX=1, NX
!/T          MAPOUT(IX,IY) = MAPSTA(IY,IX)
!/T          IF ( MAPFS(IY,IX) .NE. 0 )                               &
!/T               XOUT(IX,IY) = ZZ(MAPFS(IY,IX))
!/T          END DO
!/T        END DO
!/T      CALL PRTBLK (NDST, NX, NY, NX, XOUT, MAPOUT, 0, 0.,          &
!/T                      1, NX, ISX, 1, NY, ISY, 'ZZ', ZUNIT)
!/T      DO IY=1, NY
!/T        DO IX=1, NX
!/T          XOUT(IX,IY) = DZZDX(IY,IX)
!/T          END DO
!/T        END DO
!/T      CALL PRTBLK (NDST, NX, NY, NX, XOUT, MAPOUT, 0, 0.,          &
!/T                      1, NX, ISX, 1, NY, ISY, 'DZZDX',TRIM(ZUNIT)//'/m')
!/T      DO IY=1, NY
!/T        DO IX=1, NX
!/T          XOUT(IX,IY) = DZZDY(IY,IX)
!/T          END DO
!/T        END DO
!/T      CALL PRTBLK (NDST, NX, NY, NX, XOUT, MAPOUT, 0, 0.,          &
!/T                      1, NX, ISX, 1, NY, ISY, 'DZZDY',TRIM(ZUNIT)//'/m')
!
      RETURN
!
! Formats
!
!/T 9000 FORMAT (' TEST W3DZXY : DX0I, DY0I : ',2E12.5)
!/T 9010 FORMAT (' TEST W3DZXY : FIELDS ')
!/
!/ End of W3DZXY ----------------------------------------------------- /
!/
      END SUBROUTINE W3DZXY
!/ ------------------------------------------------------------------- /
!/ End of module W3UPDTMD -------------------------------------------- /
!/
      END MODULE W3UPDTMD
