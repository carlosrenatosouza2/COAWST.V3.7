#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      PROGRAM W3STRT
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Jun-2018 |
!/                  +-----------------------------------+
!/
!/    15-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
!/    18-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    11-Jan-2001 : Flat grid version                   ( version 2.06 )
!/    11-Jun-2001 : Clean up.                           ( version 2.11 )
!/    30-Apr-2002 : Updated W3IORS.                     ( version 2.20 )
!/    13-Nov-2002 : Updated W3IORS.                     ( version 3.00 )
!/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    28-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    08-May-2007 : Starting from calm as an option.    ( version 3.11 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    31-Oct-2010 : Implement unstructured grid         ( version 3.14 )
!/                  (A. Roland and F. Ardhuin)
!/    05-Jul-2011 : Revert to X-Y gaussian shape        ( version 4.01 )
!/    06-Mar-2012 : Hardening output.                   ( version 4.07 )
!/    06-Jun-2018 : Add DEBUGINIT/EXPORTWWM             ( version 6.04 )
!/
!/
!/    Copyright 2009-2012 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Generation of initial conditions for a "cold start" of
!     WAVEWATCH III.
!
!  2. Method :
!
!     General model information os obtained from the model definition
!     file using W3IOGR. The type of the initial field is read
!     from the input file WW3_strt.inp (NDSI). Three types of initial
!     conditions are available.
!       1) Gaussian distribution in longitude, latitude and frequency,
!          cos power in directions. Can default to single spectral
!          bin.
!       2) Predefined JONSWAP spectrum, Gaussian height distribution
!          in space.
!       3) Fetch-limited JONSWAP spectrum based on the actual wind
!          speed. To avoid the need of reading a wind field, the
!          restart file is a "dummy", and the actual initial field
!          is constructed in the initialization routine W3INIT.
!       4) User defined spectrum throughout the model.
!       5) Starting from rest.
!     The initial conditions are written to the restart.WW3 using the
!     subroutine W3IORS. Note that the name of the restart file is set
!     in W3IORS.
!
!  3. Parameters :
!
!     Local parameters.
!     ----------------------------------------------------------------
!       NDSI    Int.  Input unit number ("ww3_strt.inp").
!       ITYPE   Int.  Type of field (see section 2).
!       FP,SIP  Real  Peak frequency (Hz) and spread.  \
!       XM,SIX  Real  Id. X (degr.).                   |
!       YM,SIY  Real  Id. Y (degr.).                   |   ITYPE = 1
!       HMAX    Real  Maximum wave height.             |
!       NCOS    Real  Cosine power in dir. distr.      |
!       THM     Real  Mean direction (cart. degr.)     / \
!       ALFA    Real  Energy level of PM spectrum.       |
!       FP      Real  Peak frequency (Hz).               | ITYPE = 2
!       GAMMA   Real  Peak enhancement factor            |
!       SIGA/B  Real  Spread with GAMA.                  /
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3NMOD    Subr. W3GDATMD Set number of model.
!      W3SETG    Subr.   Id.    Point to selected model.
!      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
!      W3SETW    Subr.   Id.    Point to selected model for wave data.
!      W3DIMW    Subr.   Id.    Set array dims for wave data.
!      W3NAUX    Subr. W3ADATMD Set number of model for aux data.
!      W3SETA    Subr.   Id.    Point to selected model for aux data.
!      W3NOUT    Subr. W3ODATMD Set number of model for output.
!      W3SETO    Subr.   Id.    Point to selected model for output.
!      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
!      STRACE    Subr.   Id.    Subroutine tracing.
!      NEXTLN    Subr.   Id.    Get next line from input filw
!      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
!      EJ5P      Func.   Id.    Five parameter JONSWAP spectrum.
!      PRT1DS    Subr. W3ARRYMD Print plot of 1-D spectrum.
!      PRT2DS    Subr.   Id.    Print plot of 2-D spectrum.
!      PRTBLK    Subr.   Id.    Print plot of array.
!      WAVNU1    Subr. W3DISPMD Solve dispersion relation.
!      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
!      W3IORS    Subr. W3IORSMD Reading/writing restart files.
!      W3DIST    Subr. W3GSRUMD Compute distance between two points.
!      MPI_xxx   Subr. mpif.h   Standard MPI routines.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     None, stand-alone program.
!
!  6. Error messages :
!
!  7. Remarks :
!
!     - While reading the restart file W3IORS will recognize the
!       need for checking the time, as the restart file contains
!       information on the origine of the file ("cold" or "hot").
!     - User input for x-wise gaussian spread control, SIX, is
!       now available again (option for SIX.NE.SIY available.)
!       If user desires a distribution that is circular in real
!       distances, user should input a negative number for SIX.
!
!  8. Structure :
!
!     ----------------------------------------------------
!        1.a  Set up data structures.
!                            ( W3NMOD , W3NDAT , W3NOUT
!                              W3SETG , W3SETW , W3SETO )
!          b  I-O setup.
!          b  Print heading(s).
!        2.a  Read model defintion file with base model
!             data.                            ( W3IOGR )
!          b  MPP initializations.
!        3.   Get field type from the input file.
!        4.   ITYPE = 1, Gaussian, cosine.
!          a  Read parameters.
!          b  Set-up 1-D spectrum.
!          c  Set-up directional distribution.
!          d  Normalize spectrum with Hmax.
!          e  Distribute over grid.
!        5.   ITYPE = 2, pre-defined JONSWAP.
!          a  Read parameters.
!          b  Set-up 1-D spectrum.
!          c  2-D energy spectrum.
!          d  Distribute over grid.
!        6.   ITYPE = 3, fetch limited JONSWAP.
!        7.   ITYPE = 4, user-defined spectrum.
!          a  Read scale factor.
!          b  Read and rescale spectrum.
!          c  Distribute over grid.
!        8.   ITYPE = 5, start from calm conditions.
!        9.   Convert energy to action
!       10.   Write restart file.              ( W3IORS )
!     ----------------------------------------------------
!
!  9. Switches :
!
!     !/SHRD  Switch for shared / distributed memory architecture.
!     !/DIST  Id.
!
!     !/SHRD  Switch for message passing method.
!     !/MPI   Id.
!
!     !/S     Enable subroutine tracing.
!
!     !/O4    Output normalized 1-D energy spectrum.
!     !/O5    Output normalized 2-D energy spectrum.
!     !/O6    Output normalized wave heights (not MPP adapted).
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
!/
!     USE W3GDATMD, ONLY: W3NMOD, W3SETG
!     USE W3WDATMD, ONLY: W3NDAT, W3SETW, W3DIMW
      USE W3ADATMD, ONLY: W3NAUX, W3SETA
      USE W3ODATMD, ONLY: W3NOUT, W3SETO
      USE W3SERVMD, ONLY: ITRACE, NEXTLN, EJ5P, EXTCDE
      USE W3ARRYMD, ONLY : PRT1DS
      USE W3ARRYMD, ONLY : PRT2DS
      USE W3ARRYMD, ONLY : PRTBLK
      USE W3DISPMD, ONLY : WAVNU1
      USE W3IOGRMD, ONLY: W3IOGR
      USE W3IORSMD, ONLY: W3IORS
      USE W3GSRUMD, ONLY: W3DIST
!/
      USE W3GDATMD
      USE W3WDATMD
      USE W3ODATMD, ONLY: NDSE, NDST, NDSO, NAPROC, IAPROC,           &
                          NAPOUT, NAPERR, FNMPRE
!/
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NDSI, NDSM, NDSR, NDSTRC, NTRACE,    &
                                 NDSEN, IERR, ITYPE, NCOS, IKM, IK,   &
                                 ITHM, ITH, JSEA, ISEA, IX, IY, J
      INTEGER                 :: NSX, NSY
      INTEGER, ALLOCATABLE    :: MAPO(:,:)
      REAL                    :: FP, SIP, THM, XM, SIX, YM, SIY, HMAX,&
                                 CHSIP, FRREL, ETOT, E1I, FACTOR, X,  &
                                 Y, RDSQR, ALFA, GAMMA, SIGA, SIGB,   &
                                 YLN, FR, BETA, FRR, S, SUMD, ANG,    &
                                 ARG, FACS, DEPTH, WN, CG, HPQMAX
      REAL, ALLOCATABLE       :: E1(:), DD(:), E2(:,:), E21(:), FINP(:,:)
      REAL, ALLOCATABLE       :: E2OUT(:,:)
      REAL, ALLOCATABLE       :: HSIG(:,:)
      CHARACTER               :: COMSTR*1, INXOUT*4
      LOGICAL                 :: FLONE,NOSIX
!/
!/ ------------------------------------------------------------------- /
!
! 1.a Initialize data structure
!
      CALL W3NMOD ( 1, 6, 6 )
      CALL W3SETG ( 1, 6, 6 )
      CALL W3NDAT (    6, 6 )
      CALL W3SETW ( 1, 6, 6 )
      CALL W3NAUX (    6, 6 )
      CALL W3SETA ( 1, 6, 6 )
      CALL W3NOUT (    6, 6 )
      CALL W3SETO ( 1, 6, 6 )
!
! 1.b IO set-up.
!
      NDSI   = 10
      NDSM   = 20
      NDSR   = 20
!
      NDSTRC =  6
      NTRACE = 10
      CALL ITRACE ( NDSTRC, NTRACE )
!
! 1.c MPP initializations
!
      NAPROC = 1
      IAPROC = 1
!
      IF ( IAPROC .EQ. NAPERR ) THEN
          NDSEN  = NDSE
        ELSE
          NDSEN  = -1
        END IF
!
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,900)
!
      J      = LEN_TRIM(FNMPRE)
      OPEN (NDSI,FILE=FNMPRE(:J)//'ww3_strt.inp',STATUS='OLD',        &
            ERR=800,IOSTAT=IERR)
      REWIND (NDSI)
      READ (NDSI,'(A)',END=801,ERR=802) COMSTR
      IF (COMSTR.EQ.' ') COMSTR = '$'
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,901) COMSTR
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Read model definition file and mpp initializations.
! 2.a Reading file
!
      CALL W3IOGR ( 'READ', NDSM )
!
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,902) GNAME
!
! 2.b MPP initializations
!
      NSEAL  = NSEA
!
      CALL W3DIMW ( 1, NDSE, NDST )
      ALLOCATE ( E1(NK), DD(NTH), E2(NTH,NK), E21(NSPEC),           &
                 FINP(NK,NTH) )
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.  Read type from input file.
!
      CALL NEXTLN ( COMSTR , NDSI , NDSEN )
      READ (NDSI,*,END=801,ERR=802) ITYPE
      IF ( ITYPE.LT.1 .OR. ITYPE.GT.5 ) THEN
          IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1010) ITYPE
          CALL EXTCDE ( 1 )
        END IF
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,930) ITYPE
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 4.  ITYPE = 1, Gaussian, cosine.
!
      IF ( ITYPE .EQ. 1 ) THEN
          INXOUT = 'COLD'
!
! 4.a Read parameters.
!
          CALL NEXTLN ( COMSTR , NDSI , NDSEN )
          READ (NDSI,*,END=801,ERR=802)                            &
                FP, SIP, THM, NCOS, XM, SIX, YM, SIY, HMAX
          FP     = MAX ( 0.5 * TPIINV * SIG(1) , FP )
          SIP    = MAX ( 0. , SIP )
          DO
            IF ( THM .LT. 0. ) THEN
                THM    = THM + 360.
              ELSE
                EXIT
              END IF
            END DO
          THM    = MOD ( THM , 360. )
          NCOS   = MAX ( 0 , 2*(NCOS/2) )
 
          NOSIX=.FALSE.
          IF(SIX.LT.0.0)THEN
             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,903)
             NOSIX=.TRUE.
          END IF
 
          HPQMAX=-999.0
          DO JSEA=1, NSEAL
            ISEA   = JSEA
            IX     = MAPSF(ISEA,1)
            IY     = MAPSF(ISEA,2)
            IF(HPFAC(IY,IX).GT.HPQMAX)THEN
               HPQMAX=HPFAC(IY,IX)
            ENDIF
          END DO
          SIX = MAX(0.01*HPQMAX,SIX)
 
          HPQMAX=-999.0
          DO JSEA=1, NSEAL
            ISEA   = JSEA
            IX     = MAPSF(ISEA,1)
            IY     = MAPSF(ISEA,2)
            IF(HQFAC(IY,IX).GT.HPQMAX)THEN
               HPQMAX=HQFAC(IY,IX)
            ENDIF
          END DO
          SIY = MAX(0.01*HPQMAX,SIY)
 
          HMAX   = MAX ( 0. , HMAX )
!
          IF ( IAPROC .EQ. NAPOUT ) THEN
              IF ( FLAGLL ) THEN
                  FACTOR = 1.
                  WRITE (NDSO,940) FP, SIP, THM, NCOS, &
                    FACTOR*XM, MIN(9999.99,FACTOR*SIX), FACTOR*YM,  &
                    MIN(9999.99,FACTOR*SIY), HMAX
                ELSE
                  FACTOR = 1.E-3
                  WRITE (NDSO,941) FP, SIP, THM, NCOS, &
                    FACTOR*XM, MIN(9999.99,FACTOR*SIX), FACTOR*YM,  &
                    MIN(9999.99,FACTOR*SIY), HMAX
                END IF
            END IF
!
          FP     = FP  * TPI
          SIP    = SIP * TPI
          THM    = MOD ( 630. - THM , 360. ) * DERA
!
! 4.b Make 1-D spectrum.
!
          CHSIP  = 0.1 * DSIP(1)
          FLONE  = SIP .LT. CHSIP
          IKM    = NINT ( 1. + (LOG(FP)-LOG(FR1*TPI))/LOG(XFR) )
          IKM    = MAX ( 1 , MIN ( NK , IKM ) )
!
          DO IK=1, NK
            IF ( FLONE ) THEN
                IF (IK.EQ.IKM) THEN
                    E1(IK) = 1.
                  ELSE
                    E1(IK) = 0.
                  END IF
              ELSE
                FRREL  = (SIG(IK)-FP)/SIP
                IF (ABS(FRREL).LT.10) THEN
                    E1(IK) = EXP ( -0.125 * FRREL**2 )
                  ELSE
                    E1(IK) = 0.
                  END IF
              END IF
            END DO
!
           IF ( IAPROC .EQ. NAPOUT ) CALL PRT1DS                      &
                 (NDSO, NK, E1, SIG(1:), '  ', 10, 0.,                &
                  'Unscaled 1-D', ' ', 'TEST E(f)')
!
! 4.c Make directional distribution.
!
          FLONE  = NCOS .GT. 20
          ITHM   = 1 + NINT ( THM / DTH )
          DO ITH=1, NTH
            IF (FLONE) THEN
                IF ( ITH .EQ. ITHM ) THEN
                    DD(ITH) = 1.
                  ELSE
                    DD(ITH) = 0.
                  END IF
              ELSE
                DD(ITH) = MAX ( COS(TH(ITH)-THM) , 0. )**NCOS
              END IF
            END DO
!
! 4.d 2-D energy spectrum.
!
          ETOT   = 0.
          DO IK=1, NK
            E1I    = 0.
            DO ITH=1, NTH
              E2(ITH,IK) = E1(IK) * DD(ITH)
              E1I        = E1I + E2(ITH,IK)
              END DO
            ETOT   = ETOT + E1I * DSIP(IK)
            END DO
          ETOT   = ETOT * DTH
          FACTOR = HMAX**2 / ( 16. * ETOT )
!
          E2     = FACTOR * E2
!
          ALLOCATE ( E2OUT(NK,NTH) )
          DO ITH=1, NTH
            DO IK=1, NK
              E2OUT(IK,ITH) = TPI * E2(ITH,IK)
              END DO
            END DO
!
          IF ( IAPROC .EQ. NAPOUT ) CALL PRT2DS                       &
                  ( NDSO, NK, NK, NTH, E2OUT, SIG(1:), ' ', DERA*TPI, &
                    0., 0.0001, 'Energy', 'm2s', 'TEST 2-D')
          DEALLOCATE ( E2OUT )
!
! 4.e Distribute over grid.
!
 
          DO IK=1, NK
            E21(1+(IK-1)*NTH:IK*NTH) = E2(:,IK)
            END DO
!
          DO JSEA=1, NSEAL
!
            ISEA   = JSEA
            IF (GTYPE .EQ. UNGTYPE) THEN
               IX     = MAPSF(ISEA,1)
               X      = XYB(IX,1)
               Y      = XYB(IX,2)
            ELSE
               IX     = MAPSF(ISEA,1)
               IY     = MAPSF(ISEA,2)
               X      = XGRD(IY,IX)
               Y      = YGRD(IY,IX)
            ENDIF
            IF(NOSIX)THEN
               RDSQR  =(W3DIST(FLAGLL,X,Y,XM,YM)/SIY)**2
            ELSE
               RDSQR  =((X-XM)/SIX)**2 + ((Y-YM)/SIY)**2
            ENDIF
            IF ( RDSQR .GT. 40. ) THEN
                FACTOR = 0.
              ELSE
                FACTOR = EXP ( -0.5 * RDSQR )
              END IF
!
            VA(:,JSEA) = FACTOR * E21
!
 
!
            END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 5.  ITYPE = 2, pre-defined JONSWAP.
!
        ELSE IF ( ITYPE .EQ. 2 ) THEN
          INXOUT = 'COLD'
!
! 5.a Read parameters.
!
          CALL NEXTLN ( COMSTR , NDSI , NDSEN )
          READ (NDSI,*,END=801,ERR=802)                               &
                ALFA, FP, THM, GAMMA, SIGA, SIGB, XM, SIX, YM, SIY
!
          IF (ALFA.LE.0.) ALFA = 0.0081
          IF (FP  .LE.0.) FP   = 0.10
          IF (SIGA.LE.0.) SIGA = 0.07
          IF (SIGB.LE.0.) SIGB = 0.09
          FP     = MAX ( 0.5 * TPIINV * SIG(1) , FP )
          FP     = MIN ( TPIINV * SIG(NK) , FP )
 
          NOSIX=.FALSE.
          IF(SIX.LT.0.0)THEN
             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,903)
             NOSIX=.TRUE.
          END IF
 
          HPQMAX=-999.0
          DO JSEA=1, NSEAL
            ISEA   = JSEA
            IX     = MAPSF(ISEA,1)
            IY     = MAPSF(ISEA,2)
            IF(HPFAC(IY,IX).GT.HPQMAX)THEN
               HPQMAX=HPFAC(IY,IX)
            ENDIF
          END DO
          SIX = MAX(0.01*HPQMAX,SIX)
 
          HPQMAX=-999.0
          DO JSEA=1, NSEAL
            ISEA   = JSEA
            IX     = MAPSF(ISEA,1)
            IY     = MAPSF(ISEA,2)
            IF(HQFAC(IY,IX).GT.HPQMAX)THEN
               HPQMAX=HQFAC(IY,IX)
            ENDIF
          END DO
          SIY = MAX(0.01*HPQMAX,SIY)
 
          DO
            IF ( THM .LT. 0. ) THEN
                THM    = THM + 360.
              ELSE
                EXIT
              END IF
            END DO
          THM    = MOD ( THM , 360. )
          GAMMA  = MAX (GAMMA,1.)
          YLN    = LOG(GAMMA)
!
          IF ( IAPROC .EQ. NAPOUT ) THEN
              IF ( FLAGLL ) THEN
                  FACTOR = 1.
                  WRITE (NDSO,950) ALFA, FP, THM, GAMMA, SIGA, SIGB,  &
                      FACTOR*XM, FACTOR*SIX, FACTOR*YM, FACTOR*SIY
                ELSE
                  FACTOR = 1.E-3
                  WRITE (NDSO,951) ALFA, FP, THM, GAMMA, SIGA, SIGB,  &
                      FACTOR*XM, FACTOR*SIX, FACTOR*YM, FACTOR*SIY
                END IF
            END IF
          THM    = MOD ( 630. - THM , 360. ) * DERA
!
! 5.b Make 1-D spectrum.
!
          DO IK=1, NK
            FR     = SIG(IK) * TPIINV
            E1(IK) = EJ5P (FR, ALFA, FP, YLN, SIGA, SIGB )
            END DO
!
          IF ( IAPROC .EQ. NAPOUT ) CALL PRT1DS                   &
                       (NDSO, NK, E1, SIG(1:), '  ', 18, 0.,      &
                       'E(f)', ' ', 'TEST 1-D')
!
! 5.c 2-D energy spectrum.
!     Factor 2pi to go to E(sigma,theta)
!
          DO IK = 1,NK
            FR     = SIG(IK) * TPIINV
            IF (FR.LT.FP) THEN
                BETA =  4.06
              ELSE
                BETA = -2.34
              END IF
            FRR    = MIN ( 2.5 , FR/FP )
            S      = 9.77 * FRR**BETA
            SUMD   = 0.
            DO ITH = 1,NTH
              ANG    = COS( 0.5 * ( THM - TH(ITH) ) )**2
              DD(ITH) = 0.
              IF(ANG.GT.1.E-20) THEN
                  ARG    = S * LOG(ANG)
                  IF(ARG.GT.-170) DD(ITH) = EXP(ARG)
                END IF
              SUMD    = SUMD + DD(ITH)
              END DO
            FACTOR = 1. / (TPI*SUMD*DTH)
            DO ITH = 1,NTH
              E2(ITH,IK) = FACTOR * E1(IK) * DD(ITH)
              END DO
            END DO
!
          ALLOCATE ( E2OUT(NK,NTH) )
          DO ITH=1, NTH
            DO IK=1, NK
              E2OUT(IK,ITH) = TPI * E2(ITH,IK)
              END DO
            END DO
!
          IF ( IAPROC .EQ. NAPOUT ) CALL PRT2DS                   &
                   (NDSO, NK, NK, NTH, E2OUT, SIG(1:), ' ', 1.,   &
                    0., 0.0001, 'E(f,theta)', 'm2s', 'TEST 2-D')
          DEALLOCATE ( E2OUT )
!
! 5.d Distribute over grid.
!
 
          DO IK=1, NK
            E21(1+(IK-1)*NTH:IK*NTH) = E2(:,IK)
            END DO
!
          DO JSEA=1, NSEAL
!
            ISEA   = JSEA
            IF (GTYPE .EQ. UNGTYPE) THEN
               IX     = MAPSF(ISEA,1)
               X      = XYB(IX,1)
               Y      = XYB(IX,2)
            ELSE
               IX     = MAPSF(ISEA,1)
               IY     = MAPSF(ISEA,2)
               X      = XGRD(IY,IX)
               Y      = YGRD(IY,IX)
            ENDIF
            IF(NOSIX)THEN
               RDSQR  =(W3DIST(FLAGLL,X,Y,XM,YM)/SIY)**2
            ELSE
               RDSQR  =((X-XM)/SIX)**2 + ((Y-YM)/SIY)**2
            ENDIF
            IF ( RDSQR .GT. 40. ) THEN
                FACTOR = 0.
              ELSE
                FACTOR = EXP ( -0.5 * RDSQR )
              END IF
!
            VA(:,JSEA) = FACTOR * E21
!
            END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 6.  ITYPE = 3, fetch limited JONSWAP.
!
        ELSE IF ( ITYPE .EQ. 3 ) THEN
          INXOUT = 'WIND'
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,960)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 7.  ITYPE = 4, User defined.
!
        ELSE IF ( ITYPE .EQ. 4 ) THEN
          INXOUT = 'COLD'
!
! 7.a Read parameters.
!
          CALL NEXTLN ( COMSTR , NDSI , NDSEN )
          READ (NDSI,*,END=801,ERR=802) FACS
          IF ( FACS .LE. 0. ) FACS = 1.
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,970) FACS
!
! 7.b Read and rescale spectrum.
!
          CALL NEXTLN ( COMSTR , NDSI , NDSEN )
          READ (NDSI,*,END=801,ERR=802)                               &
               ((FINP(IK,ITH),IK=1,NK),ITH=1,NTH)
!
          FINP = FINP * FACS / TPI
!
          IF ( IAPROC .EQ. NAPOUT ) CALL PRT2DS                   &
                  (NDSO, NK, NK, NTH, FINP, SIG(1:), ' ', TPI,    &
                   0., 0.0001, 'Energy', 'm2s', 'TEST 2-D')
!
! 7.c Distribute over grid.
!
          DO JSEA=1, NSEAL
!
            ISEA   = JSEA
            DO IK=1, NK
              DO ITH=1, NTH
                VA(ITH+(IK-1)*NTH,JSEA) = FINP(IK,ITH)
                END DO
              END DO
            END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 8.  ITYPE = 5, fetch limited JONSWAP.
!
        ELSE
          INXOUT = 'CALM'
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,980)
!
        END IF
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 9.  Convert E(sigma) to N(k)
!
      IF ( ITYPE.NE.3 .AND. ITYPE.NE.5 ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,990)
!
          ALLOCATE ( HSIG(NX,NY) )
          HSIG   = 0.
!
          DO JSEA=1, NSEAL
            ISEA   = JSEA
            DEPTH  = MAX ( DMIN , -ZB(ISEA) )
            ETOT   = 0.
            DO IK=1, NK
              CALL WAVNU1 ( SIG(IK), DEPTH, WN, CG )
              E1I    = 0.
              DO ITH=1, NTH
                E1I    = E1I + VA(ITH+(IK-1)*NTH,JSEA)
                VA(ITH+(IK-1)*NTH,JSEA) = VA(ITH+(IK-1)*NTH,JSEA) *   &
                                               CG / SIG(IK)
                END DO
              ETOT   = ETOT + E1I*DSIP(IK)
              END DO
            IX     = MAPSF(ISEA,1)
            IY     = MAPSF(ISEA,2)
            HSIG(IX,IY) = 4. * SQRT ( ETOT * DTH )
          END DO
!
          ALLOCATE ( MAPO(NX,NY) )
          DO IX=1, NX
            DO IY=1, NY
              MAPO(IX,IY) = MAPSTA(IY,IX)
              END DO
            END DO
!
          NSX    = 1 + NX/35
          NSY    = 1 + NY/35
          IF ( IAPROC .EQ. NAPOUT ) CALL PRTBLK                   &
                      (NDSO, NX, NY, NX, HSIG, MAPO, 0, 0.,       &
                       1, NX, NSX, 1, NY, NSY, 'Hs', 'm')
!
        END IF
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!10.  Write restart file.
!
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,995)
      CALL W3IORS ( INXOUT, NDSR, SIG(NK) )
!
      GOTO 888
!
! Escape locations read errors :
!
  800 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) IERR
      CALL EXTCDE ( 10 )
!
  801 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001)
      CALL EXTCDE ( 11 )
!
  802 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002) IERR
      CALL EXTCDE ( 12 )
!
  888 CONTINUE
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,999)
!
! Formats
!
  900 FORMAT (/15X,'   *** WAVEWATCH III  Initial conditions ***   '/ &
               15X,'==============================================='/)
  901 FORMAT ( '  Comment character is ''',A,''''/)
  902 FORMAT ( '  Grid name : ',A/)
  903 FORMAT ( '  Negative SIX was provided by user.         '/       &
               '  WW3 will create a gaussian distribution    '/       &
               '  that is circular in real space. ')
!
  930 FORMAT (/'  Initial field ITYPE =',I2/                          &
               ' --------------------------------------------------')
!
  940 FORMAT ( '       Gaussian / cosine power spectrum '//           &
               '       Peak frequency and spread (Hz)    :',2X,2F8.4/ &
               '       Mean direction (Naut., degr.)     :',F7.1/     &
               '       Cosine power of dir. distribution :',I5/       &
               '       Mean longitude and spread (degr.) :',2F8.2/    &
               '       Mean latitude and spread (degr.)  :',2F8.2/    &
               '       Maximum wave height               :',F8.2/)
!
  950 FORMAT ( '       JONSWAP spectrum'//                            &
               '       alfa                          (-) : ',F12.5/   &
               '       Peak frequecy                (Hz) : ',F11.4/   &
               '       Mean direction       (Naut.,deg.) : ',F 8.1/   &
               '       gamma                         (-) : ',F 9.2/   &
               '       sigma-A                       (-) : ',F11.4/   &
               '       sigma-B                       (-) : ',F11.4/   &
               '       Mean longitude and spread (degr.) : ',2F9.2/   &
               '       Mean latitude and spread  (degr.) : ',2F9.2)
  941 FORMAT ( '       Gaussian / cosine power spectrum '//           &
               '       Peak frequency and spread (Hz)    :',2X,2F8.4/ &
               '       Mean direction (Naut., degr.)     :',F7.1/     &
               '       Cosine power of dir. distribution :',I5/       &
               '       Mean X and spread (km)            :',2F8.2/    &
               '       Mean Y and spread (km)            :',2F8.2/    &
               '       Maximum wave height               :',F8.2/)
!
  951 FORMAT ( '       JONSWAP spectrum'//                            &
               '       alfa                      (-) : ',F12.5/   &
               '       Peak frequecy            (Hz) : ',F11.4/   &
               '       Mean direction   (Naut.,deg.) : ',F 8.1/   &
               '       gamma                     (-) : ',F 9.2/   &
               '       sigma-A                   (-) : ',F11.4/   &
               '       sigma-B                   (-) : ',F11.4/   &
               '       Mean X and spread        (km) : ',2F9.2/   &
               '       Mean Y and spread        (km) : ',2F9.2)
!
  960 FORMAT ( '       Fetch-limited JONSWAP spectra based on local '/ &
               '       wind speed (fetch related to grid increment).')
!
  970 FORMAT ( '       User-defined energy spectrum F(f,theta).'//    &
               '       Scale factor             (-) : ',E12.4/)
!
  980 FORMAT ( '       Starting from calm conditions (Hs = 0)')
!
  990 FORMAT (/'       Converting energy to action ... ')
  995 FORMAT (/'       Writing restart file  ... '/)
!
  999 FORMAT (/'  End of program '/                                   &
               ' ========================================='/          &
               '         WAVEWATCH III Initial conditions '/)
!
 1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3STRT : '/               &
               '     ERROR IN OPENING INPUT FILE'/                    &
               '     IOSTAT =',I5/)
!
 1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3STRT : '/               &
               '     PREMATURE END OF INPUT FILE'/)
!
 1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3STRT : '/               &
               '     ERROR IN READING FROM INPUT FILE'/               &
               '     IOSTAT =',I5/)
!
 1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3STRT : '/               &
               '     ILLEGAL TYPE, ITYPE =',I4/)
!
!/
!/ End of W3STRT ----------------------------------------------------- /
!/
      END PROGRAM W3STRT
