#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3IOSFMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         25-Jul-2018 |
!/                  +-----------------------------------+
!/
!/    27-Jun-2006 : Origination.                        ( version 3.09 )
!/    02-Nov-2006 : Origination W3CPRT and W3IOSF.      ( version 3.10 )
!/    24-Mar-2007 : Add pars for entire spectrum.       ( version 3.11 )
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Fix unitialized dtsiz in w3iosf.    ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    06-Mar-2012 : Reparing test output under MPI.     ( version 4.07 )
!/    08-Jun-2018 : use W3ADATMD, W3PARALL, INIT_GET_ISEA and
!/                            INIT_GET_JSEA_ISPROC      ( version 6.04 )
!/    25-Jul-2018 : Changed DIMXP size for partitioning ( version 6.05 )
!/                  methods 4 and 5. (C Bunney, UKMO)
!/
!/    Copyright 2009-2012 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     I/O and computational routines for the wave-field separation
!     output.
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      VERPRT    C*10  Private  Partition file version number.
!      IDSTR     C*35  Private  Partition file ID string.
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3CPRT    Subr. Public   Partition all requested local spectra.
!      W3IOSF    Subr. Public   Processing and output of partitioned
!                               wave data.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3PART    Subr. W3PARTMD Spectral partition for single spectrum.
!      STRACE    Sur.  W3SERVMD Subroutine tracing.
!      EXTCDE    Subr.   Id.    Program abort.
!      MPI_SEND, MPI_RECV
!                               MPI send and recieve routines
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!  6. Switches :
!
!     !/S    Enable subroutine tracing.
!     !/T    Enable test output
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
      PUBLIC
!/
!/ Private parameter statements (ID strings)
!/
      CHARACTER(LEN=10), PARAMETER, PRIVATE :: VERPRT = '2018-07-25'
      CHARACTER(LEN=35), PARAMETER, PRIVATE ::                        &
                         IDSTR = 'WAVEWATCH III PARTITIONED DATA FILE'
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3CPRT ( IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         25-Jul-2018 !
!/                  +-----------------------------------+
!/
!/    30-Oct-2006 : Origination.                        ( version 3.10 )
!/    24-Mar-2007 : Add pars for entire spectrum.       ( version 3.11 )
!/    25-Jul-2018 : Changed DIMXP size for partitioning ( version 6.05 )
!/                  methods 4 and 5. (C Bunney, UKMO)
!/
!  1. Purpose :
!
!     Partitioning of spectra into fields for all grid points that
!     are locally stored.
!
!  2. Method :
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Grid number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3PART    Subr. W3PARTMD Spectral partition for single spectrum.
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
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
!  7. Remarks :
!
!     - Although a sparse (IX,IY) grid is looked for, th major loop
!       is still over NSEAL to simplify storage.
!
!  8. Structure :
!
!  9. Switches :
!
!     !/S    Enable subroutine tracing.
!     !/T    Enable test output
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      USE CONSTANTS
!
      USE W3PARTMD, ONLY: W3PART
!
      USE W3GDATMD, ONLY: NSEA, NSEAL, MAPSF, MAPSTA, NK, NTH, SIG
      USE W3ADATMD, ONLY: WN, CG, U10, U10D, DW
      USE W3ODATMD, ONLY: IAPROC, NAPROC, OUTPTS, O6INIT,       &
                          ICPRT, DTPRT, DIMP, PTMETH
      USE W3WDATMD, ONLY: VA, ASF
      USE W3ADATMD, ONLY: NSEALM
      USE W3PARALL, ONLY: INIT_GET_ISEA, INIT_GET_JSEA_ISPROC
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: IMOD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: DIMXP, JSEA, ISEA, IX, IY,     &
                                 IK, ITH, NP, TMPSIZ, OLDSIZ, FINSIZ
      INTEGER, SAVE           :: TSFAC = 7
      REAL                    :: UABS, UDIR, DEPTH, FACT, E2(NK,NTH)
      REAL, ALLOCATABLE       :: XP(:,:), TMP(:,:), TMP2(:,:)
!/
!
! -------------------------------------------------------------------- /
! 0.  Initializations
!
      IF(PTMETH .EQ. 4 .OR. PTMETH .EQ. 5) THEN
        ! Partitioning methods 4 and 5 only ever create 2 partitions
        ! C. Bunney, 25-Jul-18
        DIMXP = 2
      ELSE
        DIMXP  = ((NK+1)/2) * ((NTH-1)/2)
      ENDIF
 
      ALLOCATE ( XP(DIMP,0:DIMXP) )
!
      IF ( O6INIT ) THEN
          DEALLOCATE ( OUTPTS(IMOD)%OUT6%DTPRT )
        ELSE
          ALLOCATE ( OUTPTS(IMOD)%OUT6%ICPRT(NSEALM+1,2) )
          ICPRT => OUTPTS(IMOD)%OUT6%ICPRT
          O6INIT = .TRUE.
        END IF
      ICPRT  = 0
      ICPRT(1,2) = 1
!
      TMPSIZ = TSFAC * NSEAL
      ALLOCATE ( TMP(DIMP,TMPSIZ) )
!
! -------------------------------------------------------------------- /
! 1.  Loop over sea points
!
      DO JSEA=1, NSEAL
!
! -------------------------------------------------------------------- /
! 2.  Check need for processing
!
        CALL INIT_GET_ISEA(ISEA, JSEA)
        IX     = MAPSF(ISEA,1)
        IY     = MAPSF(ISEA,2)
        ICPRT(JSEA+1,2) = ICPRT(JSEA,2)
!
        IF ( MAPSTA(IY,IX) .LT. 0 ) CYCLE
!
! -------------------------------------------------------------------- /
! 3.  Prepare for partitioning
!
        UABS   = U10(ISEA)*ASF(ISEA)
        UDIR   = U10D(ISEA)*RADE
        DEPTH  = DW(ISEA)
!
        DO IK=1, NK
          FACT   = TPI * SIG(IK) / CG(IK,ISEA)
          DO ITH=1, NTH
            E2(IK,ITH) = VA(ITH+(IK-1)*NTH,JSEA) * FACT
            END DO
          END DO
!
! -------------------------------------------------------------------- /
! 4.  perform partitioning
!
!AR: NaN checks should results in immediate stop after trace ...
        IF (DEPTH.NE.DEPTH) THEN
          WRITE(6,*) 'IOSF:',ISEA,IX,IY,DW(ISEA),DEPTH
          WRITE(*,*) 'FOUND NaN in depth'
          STOP 'CRITICAL ERROR IN DEPTH ARRAY'
        END IF
        CALL W3PART ( E2, UABS, UDIR, DEPTH, WN(1:NK,ISEA),           &
                      NP, XP, DIMXP )
!
! -------------------------------------------------------------------- /
! 5.  Store results (temp)
!
        IF ( NP .GE. 0 ) THEN
            ICPRT( JSEA ,1) = NP + 1
            ICPRT(JSEA+1,2) = ICPRT(JSEA,2) + NP + 1
!
            IF ( ICPRT(JSEA,2)+NP .GT. TMPSIZ ) THEN
                ALLOCATE ( TMP2(DIMP,TMPSIZ) )
                TMP2   = TMP
                DEALLOCATE ( TMP )
                OLDSIZ = TMPSIZ
                TMPSIZ = TMPSIZ + MAX ( TSFAC*NSEAL , DIMXP )
                ALLOCATE ( TMP(DIMP,TMPSIZ) )
                TMP(:,1:OLDSIZ) = TMP2(:,1:OLDSIZ)
                TMP(:,OLDSIZ+1:) = 0.
                DEALLOCATE ( TMP2 )
              END IF
!
           TMP(:,ICPRT(JSEA,2):ICPRT(JSEA,2)+NP) = XP(:,0:NP)
!
         END IF
!
! -------------------------------------------------------------------- /
! 6.  End of loop and clean up
!
        END DO
!
      FINSIZ = ICPRT(NSEAL+1,2) - 1
!
      ALLOCATE ( OUTPTS(IMOD)%OUT6%DTPRT(DIMP,MAX(1,FINSIZ)) )
      DTPRT => OUTPTS(IMOD)%OUT6%DTPRT
      IF ( FINSIZ .GT. 0 ) THEN
          DTPRT = TMP(:,1:FINSIZ)
        ELSE
          DTPRT = 0.
        END IF
!
      DEALLOCATE ( XP, TMP )
!
      RETURN
!
! Formats
!
!/
!/ End of W3CPRT ----------------------------------------------------- /
!/
      END SUBROUTINE W3CPRT
 
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3IOSF ( NDSPT, IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         30-Oct-2009 |
!/                  +-----------------------------------+
!/
!/    02-Nov-2006 : Origination.                        ( version 1.10 )
!/    24-Mar-2007 : Add pars for entire spectrum.       ( version 3.11 )
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    30-Oct-2009 : Fix unitialized dtsiz error.        ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/
!  1. Purpose :
!
!     Write partitioned spectrakl data to file. Unlike other
!     WAVEWATCH III IO routines, this one writes only.
!     First ad-hoc version.
!
!  2. Method :
!
!     Writing to formatted or unformatted file with ID headers.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NDSPT   Int.   I   Unit number.
!       IMOD    Int.   I   Grid number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      EXTCDE    Subr.   Id.    Program abort.
!      MPI_SEND, MPI_RECV
!                               MPI send and recieve routines
!     ----------------------------------------------------------------
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
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
!     !/S    Enable subroutine tracing.
!     !/T    Enable test output
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      USE CONSTANTS
      USE W3SERVMD, ONLY: EXTCDE
!
      USE W3GDATMD, ONLY: FILEXT, NSEA, XGRD, YGRD, MAPSF, FLAGLL
      USE W3GDATMD, ONLY: NSEAL
      USE W3WDATMD, ONLY: TIME, ASF
      USE W3ODATMD, ONLY: NDSE, IAPROC, NAPROC, NAPPRT, NAPERR, &
                          IPASS => IPASS6, FLFORM, FNMPRE, OUTPTS,    &
                          IX0, IXN, IXS, IY0, IYN, IYS, DIMP
      USE W3ADATMD, ONLY: DW, U10, U10D, CX, CY
      USE W3ADATMD, ONLY: NSEALM
      USE W3PARALL, ONLY: INIT_GET_JSEA_ISPROC
      USE W3ADATMD, ONLY: MPI_COMM_WAVE
      USE W3ODATMD, ONLY: ICPRT, DTPRT, IT0PRT
!
      IMPLICIT NONE
!
      INCLUDE "mpif.h"
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: NDSPT, IMOD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: I, J, IERR, ISEA, JSEA, JAPROC,      &
                                 IX, IY, IP, IOFF, DTSIZ=0
      INTEGER                 :: ICSIZ, IERR_MPI, IT,            &
                                 STATUS(MPI_STATUS_SIZE,1), JSLM
      INTEGER, POINTER        :: ICP(:,:)
      REAL                    :: X, Y, DEPTH, UABS, UDIR, CABS, CDIR
      REAL, POINTER           :: DTP(:,:)
!
      TYPE PROCS
        INTEGER, POINTER      :: ICPRT(:,:)
        REAL, POINTER         :: DTPRT(:,:)
      END TYPE PROCS
!
      TYPE(PROCS), TARGET, ALLOCATABLE :: PROC(:)
!
! -------------------------------------------------------------------- /
! 0.  Initializations
!
      IPASS  = IPASS + 1
      ICSIZ  = 2 * ( NSEALM + 1 )
!
! -------------------------------------------------------------------- /
! 1.  Set up file ( IPASS = 1 and proper processor )
!
      IF ( IPASS.EQ.1 .AND. IAPROC.EQ.NAPPRT ) THEN
!
! 1.a Open file
!
          I      = LEN_TRIM(FILEXT)
          J      = LEN_TRIM(FNMPRE)
!
          IF ( FLFORM ) THEN
              OPEN (NDSPT,FILE=FNMPRE(:J)//'partition.'//FILEXT(:I),   &
                    ERR=800,IOSTAT=IERR)
            ELSE
              OPEN (NDSPT,FILE=FNMPRE(:J)//'partition.'//FILEXT(:I),   &
                    FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
            END IF
!
          REWIND (NDSPT)
!
! 1.b Header info
!
          IF ( FLFORM ) THEN
              WRITE (NDSPT,910) IDSTR, VERPRT
              IF ( FLAGLL ) THEN
                  WRITE (NDSPT,911) ' yyyymmdd hhmmss     '//         &
                                    'lat     lon   name       nprt'// &
                                    ' depth ubas  udir cabs  cdir'
                ELSE
                  WRITE (NDSPT,911) ' yyyymmdd hhmmss     '//         &
                                    'X       Y     name       nprt'// &
                                    ' depth ubas  udir cabs  cdir'
                END IF
              WRITE (NDSPT,911) '        hs     tp     lp  '//    &
                                '     theta     sp      wf'
            ELSE
              WRITE (  NDSPT  ) IDSTR, VERPRT
              IF ( FLAGLL ) THEN
                  WRITE (  NDSPT  ) ' yyyymmdd hhmmss     '//         &
                                    'lat     lon   name       nprt'// &
                                    ' depth ubas  udir cabs  cdir'
                ELSE
                  WRITE (  NDSPT  ) ' yyyymmdd hhmmss     '//         &
                                    'X       Y     name       nprt'// &
                                    ' depth ubas  udir cabs  cdir'
                END IF
              WRITE (  NDSPT  ) '        hs     tp     lp  '//    &
                                '     theta     sp      wf'
            END IF
!
        END IF
!
! -------------------------------------------------------------------- /
! 2.  Send data if output is non-local ( MPI only )
!     Leave routine after send
!
      IF ( IAPROC.NE.NAPPRT .AND. IAPROC.LE.NAPROC ) THEN
!
          IT     = IT0PRT + IAPROC - 1
          CALL MPI_SEND ( ICPRT, ICSIZ, MPI_REAL, NAPPRT-1, IT,  &
                          MPI_COMM_WAVE, IERR_MPI )
          DTSIZ  = ICPRT(NSEAL+1,2) - 1
!
          IT     = IT0PRT + NAPROC + IAPROC - 1
          IF ( DTSIZ .GT. 0 ) CALL MPI_SEND                      &
                        ( DTPRT, 6*DTSIZ, MPI_REAL, NAPPRT-1,    &
                          IT, MPI_COMM_WAVE, IERR_MPI )
!
        END IF
!
      IF ( IAPROC .NE. NAPPRT ) RETURN
!
! -------------------------------------------------------------------- /
! 3.  Point to and/or gather data
! 3.a Set up storage
!
      ALLOCATE ( PROC(NAPROC) )
!
! 3.b Point to local data
!
      IF ( IAPROC .LE. NAPROC ) THEN
          PROC(IAPROC)%ICPRT => OUTPTS(IMOD)%OUT6%ICPRT
          PROC(IAPROC)%DTPRT => OUTPTS(IMOD)%OUT6%DTPRT
        END IF
!
! 3.c Allocate and get counters and arrrays
!
      DO JAPROC=1, NAPROC
        IF ( IAPROC .EQ. JAPROC ) CYCLE
!
            ALLOCATE ( PROC(JAPROC)%ICPRT(NSEALM+1,2) )
            ICP   => PROC(JAPROC)%ICPRT
            IT     = IT0PRT + JAPROC - 1
            CALL MPI_RECV ( ICP, ICSIZ, MPI_REAL, JAPROC-1, IT,  &
                            MPI_COMM_WAVE, STATUS, IERR_MPI )
            JSLM   = 1 + (NSEA-JAPROC)/NAPROC
            DTSIZ  = ICP(JSLM+1,2) - 1
!
            ALLOCATE ( PROC(JAPROC)%DTPRT(DIMP,MAX(1,DTSIZ)) )
            DTP   => PROC(JAPROC)%DTPRT
            IT     = IT0PRT + NAPROC + JAPROC - 1
            IF ( DTSIZ .GT. 0 ) CALL MPI_RECV                    &
                          ( DTP, DIMP*DTSIZ, MPI_REAL, JAPROC-1, &
                            IT, MPI_COMM_WAVE, STATUS, IERR_MPI )
!
        END DO
!
! -------------------------------------------------------------------- /
! 4.  Write all data for which partitions are found
! 4.a General loop over all sea points
!
      DO ISEA=1, NSEA
!
! 4.b Check for partitioned data at sea point
!
        CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, JAPROC)
!
        ICP   => PROC(JAPROC)%ICPRT
        DTP   => PROC(JAPROC)%DTPRT
!
        IF ( ICP(JSEA,1) .EQ. 0 ) CYCLE
!
! 4.c Process point ID line
!
        IX     = MAPSF(ISEA,1)
        IY     = MAPSF(ISEA,2)
        IF ( IX.LT.IX0 .OR. IX.GT.IXN .OR. MOD(IX-IX0,IXS).NE.0 ) CYCLE
        IF ( IY.LT.IY0 .OR. IY.GT.IYN .OR. MOD(IY-IY0,IYS).NE.0 ) CYCLE
        X      = XGRD(IY,IX)
        Y      = YGRD(IY,IX)
        DEPTH   = DW(ISEA)
        UABS   = U10(ISEA)*ASF(ISEA)
        UDIR   = MOD ( 270. - U10D(ISEA)*RADE , 360. )
        CABS   = SQRT ( CX(ISEA)**2 + CY(ISEA)**2 )
        IF ( CABS .LT. 1.E-3 ) THEN
            CDIR   = 0.
          ELSE
            CDIR   = ATAN2 ( CY(ISEA), CX(ISEA) ) * RADE
            CDIR   = MOD ( 270. - CDIR , 360. )
          END IF
!
        IF ( FLFORM ) THEN
            IF ( FLAGLL ) THEN
                WRITE (NDSPT,940) TIME, Y, X,                        &
                                 'grid_point', ICP(JSEA,1) - 1,      &
                                  DEPTH, UABS, UDIR, CABS, CDIR
              ELSE
                WRITE (NDSPT,941) TIME, X*1.E-3, Y*1.E-3,            &
                                 'grid_point', ICP(JSEA,1) - 1,      &
                                  DEPTH, UABS, UDIR, CABS, CDIR
              END IF
          ELSE
            IF ( FLAGLL ) THEN
                WRITE (  NDSPT  ) TIME, Y, X,                        &
                                 'grid_point', ICP(JSEA,1) - 1,      &
                                  DEPTH, UABS, UDIR, CABS, CDIR
              ELSE
                WRITE (  NDSPT  ) TIME, X*1.E-3, Y*1.E-3,            &
                                 'grid_point', ICP(JSEA,1) - 1,      &
                                  DEPTH, UABS, UDIR, CABS, CDIR
              END IF
          END IF
!
! 4.d Process partitions for this point
!
        IOFF   = ICP(JSEA,2)
!
        IF ( FLFORM ) THEN
            DO IP=0, ICP(JSEA,1) - 1
              WRITE (NDSPT,942) IP, DTP(:,IP+IOFF)
              END DO
          ELSE
            DO IP=0, ICP(JSEA,1) - 1
              WRITE (  NDSPT  ) IP, DTP(:,IP+IOFF)
              END DO
          END IF
!
        END DO
!
! -------------------------------------------------------------------- /
! 5.  Clean up data structure
!
      DO JAPROC=1, NAPROC
        IF ( IAPROC .EQ. JAPROC ) CYCLE
        DEALLOCATE ( PROC(JAPROC)%ICPRT, PROC(JAPROC)%DTPRT )
        END DO
!
      DEALLOCATE ( PROC )
!
      RETURN
!
! Escape locations read errors --------------------------------------- *
!
  800 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) IERR
      CALL EXTCDE ( 1 )
!
! Formats
!
  910 FORMAT (A,1X,A)
  911 FORMAT (A)
!
  940 FORMAT (1X,I8.8,1X,I6.6,2F8.3,2X,'''',A10,'''',            &
              1X,I2,F7.1,F5.1,f6.1,F5.2,F6.1)
  941 FORMAT (1X,I8.8,1X,I6.6,2(F8.1,'E3'),2X,'''',A10,'''',     &
              1X,I2,F7.1,F5.1,f6.1,F5.2,F6.1)
  942 FORMAT (I3,3F8.2,2F9.2,F7.2)
!
 1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOSF : '/               &
               '     ERROR IN OPENING FILE'/                          &
               '     IOSTAT =',I5/)
!
!/
!/ End of W3IOSF ----------------------------------------------------- /
!/
      END SUBROUTINE W3IOSF
!/
!/ End of module W3IOSFMD -------------------------------------------- /
!/
      END MODULE W3IOSFMD
