#include "w3macros.h"
      PROGRAM W3SBS1
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |            A. Chawla              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         05-Dec-2012 |
!/                  +-----------------------------------+
!/
!/    04-May-2005 : Origination.                        ( version 3.07 )
!/    11-Aug-2010 : Upgrade for operations and inclusion in svn.
!/                                                    ( version 3.14.4 )
!/    05-Dec-2012 : Making sleep a system call.       ( version 4.11   )
!/
!/    Copyright 2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Program shell or driver to run the multi-grid wave model in
!     'half-coupled' mode, that is running side-by-side with a weather
!     model while waiting for wind field to become available.
!
!     This version is set up for running at NCEP with a single input
!     wind file, and requires an additional input file.
!       times.inp  Input file with time stamps. Add to this input file
!                  a time stamp after the field has been properly
!                  added to the wind.ww3 or equavalent file.
!     This file should have the time stamps of fields available in
!     the first auxiliary wind input file (grid).
!
!     Apart from management of the time stepping, this code is
!     identical to ww3_multi.ftn, and reads the corresponding input
!     file ww3_multi.inp
!
!     Note hardwired options and system dependent parts as identified
!     in Section 7.
!
!  2. Method :
!
!     Calling WMWAVE in a loop as wind data become available, with
!     test on wind file.
!
!     In order for this to work properly, the user needs to increment
!     then main wind input file (wind.XXXX) as data become avalable
!     while this program is running. After a new field is added to
!     wind file, the corresponding time stamp in YYYYMMDD YYMMSS
!     format is concatenated to the times.inp file. As the code
!     reads the new time stamp (1X,I8,1Z,I6 format), a test read of
!     the wind file is performed until the file is readable, after
!     which the wave model is run until the new time stamp and the
!     process is repeated. The test reading of the wind file proved
!     essential on the NCEP IBM systems to deal with file system
!     latencies and buffer flushing.
!
!  3. Parameters :
!
!     Local parameters.
!     ----------------------------------------------------------------
!       SLEEP1  I.P.   Sleep time for testing times file.
!       SLEEP2  I.P.   Sleep time for testing winds file.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      WMINIT    Subr. WMINITMD Multi-grid model initialization.
!      WMWAVE    Subr. WMWAVEMD Multi-grid model subroutine.
!      WMFINL    Subr. WMFINLMD Multi-grid model finalization.
!      EXTCDE    Subr. W3SERVMD Abort program as graceful as possible.
!      W3SETG    Subr. W3GDATMD Point to Grid data data structure.
!      W3SETI    Subr. W3IDATMD Point to input fields data structure.
!      WMUGET    Subr. WMUNITMD Automatic unit number assignement.
!      WMUSET    Subr.          Automatic unit number assignement.
!
!      MPI_INIT, MPI_COMM_SIZE, MPI_COMM_RANK, MPI_BARRIER,
!         MPI_FINALIZE
!                Subr.          Standard MPI routines.
!
!      RDTIME    Subr. W3MLT    Get next wind time.
!      RDWIND    Subr. W3MLT    Test read next wind field.
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
!     - Assumptions in this program:
!        1) WNS or WND option with 3 or two data fields is set in the
!           decaration and initialization of the C*3 TYPE.
!        2) Single wind file to be tested, this is the first aux grid
!           with wind defined.
!        3) Needs system SLEEP command, now behind SBS switch.
!/    05-Dec-2012 : Making sleep a system call.       ( version 4.11   )
 
!
!  8. Structure :
!
!     ----------------------------------------------------------------
!      0.  Initialization necessary for driver
!        a General I/O: (implicit in wmmdatmd)
!        b MPI environment
!        c Identifying output to "screen" unit
!      1.  Initialization of all wave models / grids       ( WMINIT )
!      2.  Open and prepare test files.
!      3.  Run the multi-grid models
!        a Preparations
!        b Catch up with starting time of model            ( RDTIME )
!        c Catch up with test reading of file              ( DWINDE )
!        d Run wave model                                  ( WMWAVE )
!      4.  Finalization of wave model                      ( WMFINL )
!      5.  Finalization of driver
!     ----------------------------------------------------------------
!
!  9. Switches :
!
!       !/F90   F90 extensions.
!
!       !/MPI   Including MPI routines / environment.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE WMINITMD, ONLY: WMINIT
      USE WMWAVEMD, ONLY: WMWAVE
      USE WMFINLMD, ONLY: WMFINL
      USE W3SERVMD, ONLY: EXTCDE
      USE W3GDATMD, ONLY: W3SETG
      USE W3GDATMD, ONLY: NGRIDS, NAUXGR, NX, NY, GNAME, FILEXT
      USE W3IDATMD, ONLY: W3SETI
      USE W3IDATMD, ONLY: FLWIND
      USE WMMDATMD, ONLY: MDSF
      USE WMUNITMD, ONLY: WMUGET, WMUSET
      USE W3TIMEMD
!/
      USE WMMDATMD, ONLY: MDSE, MDST, MDSS, NMPROC, IMPROC, NMPSCR,   &
                          NRGRD, STIME, ETIME
!/
      IMPLICIT NONE
!
      INCLUDE "mpif.h"
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER              :: MPI_COMM = -99, IERR, NDST1, NDST2 = -1,&
                              NXW = -1, NYW = -1, TNEXT(2), TOLD(2),  &
                              I
      INTEGER              :: IERR_MPI
      INTEGER, PARAMETER   :: SLEEP1 = 10 , SLEEP2 = 10
      INTEGER, ALLOCATABLE :: TEND(:,:)
      REAL                 :: DTTST
!      CHARACTER(LEN=3)     :: TSFLD, TYPE = 'WNS'
      CHARACTER(LEN=3)     :: TSFLD, TYPE = 'WND'
      CHARACTER(LEN=13)    :: TSSTR
!/
!/ ------------------------------------------------------------------- /
! 0.  Initialization necessary for driver
! 0.a General I/O: all can start with initialization in wmmdatmd
!
! 0.b MPI environment: Here, we use MPI_COMM_WORLD
!
      CALL MPI_INIT      ( IERR_MPI )
      MPI_COMM = MPI_COMM_WORLD
      CALL MPI_COMM_SIZE ( MPI_COMM, NMPROC, IERR_MPI )
      CALL MPI_COMM_RANK ( MPI_COMM, IMPROC, IERR_MPI )
      IMPROC = IMPROC + 1
!
! 0.c Identifying output to "screen" unit
!
      IF ( IMPROC .EQ. NMPSCR ) WRITE (*,900)
!
!/ ------------------------------------------------------------------- /
! 1.  Initialization of all wave models / grids
!     Use only one of the calls ....
!
! ... Log and screen output, no separate test output file
!
!     CALL WMINIT ( 8, 9, 6, 6, 6, 'ww3_multi.inp', MPI_COMM )
!
! ... Screen output disabled
!
!     CALL WMINIT ( 8, 9, 9, 6, 6, 'ww3_multi.inp', MPI_COMM )
!
! ... Separate test output file and file preamble defined
!
!     CALL WMINIT ( 8, 9, 6, 10, 6, 'ww3_multi.inp', MPI_COMM,        &
!                   './data/' )
!
! ... Separate test output file
!
      CALL WMINIT ( 8, 9, 6, 10, 6, 'ww3_multi.inp', MPI_COMM )
!
!/ ------------------------------------------------------------------- /
! 2.  Setting up test files
!
      CALL WMUGET ( MDSE, MDST, NDST1, 'INP' )
      CALL WMUSET ( MDSE, MDST, NDST1, .TRUE., 'I/O',                 &
                    NAME='times.inp',                                 &
                    DESC='times file for sbs driver' )
      OPEN (NDST1,FILE='times.inp',STATUS='OLD',ERR=820,IOSTAT=IERR)
!
      DO I=-1, -NAUXGR, -1
        CALL W3SETG ( I, MDSE, MDST )
        CALL W3SETI ( I, MDSE, MDST )
        IF ( FLWIND ) THEN
            IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,920) FILEXT
            NXW    = NX
            NYW    = NY
            NDST2  = MDSF(I,3)
            EXIT
          END IF
        END DO
!
      IF ( NXW .EQ. -1 ) GOTO 825
      IF ( NDST2 .EQ. -1 ) GOTO 825
!
!/ ------------------------------------------------------------------- /
! 3.  Run the wave model
! 3.a Prepping, initial time stamp
!
      ALLOCATE ( TEND(2,NRGRD) )
!
      CALL RDTIME ( NDST1, TNEXT )
      IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,930) TNEXT
!
! 3.b Catch up with starting time as needed
!
      DO
        TOLD   = TNEXT
        CALL RDTIME ( NDST1, TNEXT )
        DTTST  = DSEC21 ( TNEXT , STIME )
        IF ( DTTST .GT. 0. ) THEN
            IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,930) TNEXT
          ELSE IF ( DTTST .EQ. 0. ) THEN
            IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,930) TNEXT
            EXIT
          ELSE
            BACKSPACE NDST1
            TNEXT  = TOLD
            EXIT
          END IF
        END DO
!
! 3.c Test readig of initial fields
!
      CALL RDWIND ( NDST2, TNEXT, NXW, NYW, .FALSE. )
!
! 3.d Loop to run the model
!
      DO
!
        CALL RDTIME ( NDST1, TNEXT )
        IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,930) TNEXT
        CALL RDWIND ( NDST2, TNEXT, NXW, NYW, .TRUE. )
        IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,931)
!
        DTTST  = DSEC21 ( TNEXT , ETIME )
        IF ( DTTST .LT. 0. ) THEN
            TNEXT  = ETIME
            DTTST  = 0.
          END IF
!
        DO I=1, NRGRD
          TEND(:,I) = TNEXT(:)
          END DO
!
        CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
        CALL WMWAVE ( TEND )
!
        DTTST  = DSEC21 ( TNEXT , ETIME )
        IF ( DTTST .LE. 0 ) EXIT
!
        END DO
!
      DEALLOCATE ( TEND )
!
!/ ------------------------------------------------------------------- /
! 4.  Finalize the wave model
!
      CALL WMFINL
!
!/ ------------------------------------------------------------------- /
! 5   Finalize the driver
!
      IF ( IMPROC .EQ. NMPSCR ) WRITE (*,999)
!
      CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
      CALL MPI_FINALIZE  ( IERR_MPI )
!
      GO TO 888
!
  820 CONTINUE
      WRITE (MDSS,1020) IERR
      CALL EXTCDE ( 20 )
!
  825 CONTINUE
      WRITE (MDSS,1025) NDST2
      CALL EXTCDE ( 25 )
!
  888 CONTINUE
!
! Formats
!
  900 FORMAT (/15X,'     *** WAVEWATCH III Multi-grid shell ***    '/ &
               15X,'================================================='/&
               15X,'                           side-by-side version'/)
!
  920 FORMAT ( '     WIND DATA FILE USED IS wind.',A)
  930 FORMAT (/'     WIND DATA FOUND AT TIME : ',I8.8,1X,I6.6)
  931 FORMAT (' ')
!
  999 FORMAT (//'  End of program '/                                  &
               ' ========================================'/           &
               '          WAVEWATCH III Multi-grid shell '/)
!
 1020 FORMAT (/' *** WAVEWATCH-III ERROR IN W3SBS1 : '/               &
               '     ERROR IN OPENING TIMES FILE'/                    &
               '     IOSTAT =',I5/)
!
 1025 FORMAT (/' *** WAVEWATCH-III ERROR IN W3SBS1 : '/               &
               '     WIND FILE NOT FOUND, NDST2 =  ',I8/)
!
!/
!/ Internal subroutines RDTIME and RDWIND ---------------------------- /
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE RDTIME ( NDS, TIME )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         05-Dec-2012 !
!/                  +-----------------------------------+
!/
!/    10-Aug-2010 : Origination.                      ( version 3.14.4 )
!/    05-Dec-2012 : Making sleep a system call.       ( version 4.11   )
!/
!  1. Purpose :
!
!     Internal subroutine to get next time in time file, including
!     waiting until the file is there.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NDS     Int.   I   Unit number for times file.
!       TIME    I.A.   O   Next time in times file.
!     ----------------------------------------------------------------
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: NDS
      INTEGER, INTENT(OUT)    :: TIME(2)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
!/
! -------------------------------------------------------------------- /
! 1.  Reading loop
!
      DO
!
        READ (NDS,910,END=110,ERR=810,IOSTAT=IERR) TIME
        EXIT
!
  110   CONTINUE
        IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS, 911 )
        BACKSPACE NDS
!
        END DO
!
      RETURN
!
! Escape locations read errors --------------------------------------- *
!
  810 CONTINUE
      WRITE (MDSS,1010) IERR
      CALL EXTCDE ( 10 )
!
! Formats
!
  910 FORMAT (1X,I8,1X,I6)
!
  911 FORMAT (/'    END OF TIMES FILE REACHED FOR WIND DATA '/        &
               '       WAITING BEFORE CHECKING AGAIN')
!
 1010 FORMAT (/' *** WAVEWATCH-III ERROR IN W3SBS1/RDTIME : '/        &
                '     ERROR IN OPENING TIMES FILE'/                   &
                '     IOSTAT =',I5/)
!/
!/ End of RDTIME ----------------------------------------------------- /
!/
      END SUBROUTINE RDTIME
!/ ------------------------------------------------------------------- /
      SUBROUTINE RDWIND ( NDS, TIME, NX, NY, REWIND )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         05-Dec-2012 !
!/                  +-----------------------------------+
!/
!/    10-Aug-2010 : Origination.                      ( version 3.14.4 )
!/    05-Dec-2012 : Making sleep a system call.       ( version 4.11   )
!/
!  1. Purpose :
!
!     Internal subroutine to test readnext wind fields from the data
!     file, including testing to see if file was read properly.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NDS     Int.   I   Unit number for times file.
!       TIME    I.A.   I   Next time in times file.
!       NX,NY   Int.   I   Grid size.
!       REWIND  Log.   I   Flag for wind file rewind.
!     ----------------------------------------------------------------
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: NDS, TIME(2), NX, NY
      LOGICAL, INTENT(IN)     :: REWIND
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: TTIME(2), IX, IY
      INTEGER, SAVE           :: NREW  = 0
      REAL                    :: DTTST, XXX(NX,NY)
!
! -------------------------------------------------------------------- /
! 1.  Loops
!
      DO
!
! ... Inner loop reading
!
        DO
!
          NREW   = NREW + 1
          READ (NDS,END=140,ERR=140) TTIME
!
          NREW   = NREW + 1
          READ (NDS,END=130,ERR=130) ((XXX(IX,IY),IX=1,NX),IY=1,NY)
!
          NREW   = NREW + 1
          READ (NDS,END=120,ERR=120) ((XXX(IX,IY),IX=1,NX),IY=1,NY)
!
          IF ( TYPE .EQ. 'WNS' ) THEN
              NREW   = NREW + 1
              READ (NDS,END=110,ERR=110) ((XXX(IX,IY),IX=1,NX),IY=1,NY)
            END IF
!
          EXIT
!
  110     CONTINUE
          BACKSPACE NDS
          NREW   = NREW - 1
  120     CONTINUE
          BACKSPACE NDS
          NREW   = NREW - 1
  130     CONTINUE
          BACKSPACE NDS
          NREW   = NREW - 1
  140     CONTINUE
          BACKSPACE NDS
          NREW   = NREW - 1
!
          IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,900)
!
          END DO
!
! ... Outer loop catching up
!
        DTTST  = DSEC21 ( TIME , TTIME )
!
        IF ( DTTST .LT. 0. ) THEN
            IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,901) TTIME
          ELSE IF ( DTTST .EQ. 0. ) THEN
            EXIT
          ELSE
            GOTO 800
          END IF
!
        END DO
!
! ... Rewind all
!
      IF ( REWIND ) THEN
!
         IF ( IMPROC.EQ.NMPSCR .AND. NREW.GT.4 ) WRITE (MDSS,902) NREW
!
          DO I=1, NREW
            BACKSPACE NDS
            END DO
!
          NREW   = 0
!
        END IF
!
      RETURN
!
! Escape locations read errors --------------------------------------- *
!
  800 CONTINUE
      WRITE (MDSS,1010)
      CALL EXTCDE ( 10 )
!
! Formats
!
  900 FORMAT ('       FILE NOT YET COMPLETE ... ')
  901 FORMAT ('         SKIPPING FILE FOR ',I8.8,I7.6)
  902 FORMAT ('         REWINDING FILE BY ',I4,' RECORDS')
!
 1010 FORMAT (/' *** WAVEWATCH-III ERROR IN W3SBS1/RDWIND : '/        &
               '     FILE READ PAST EXPECTED TIME '/)
!
!/
!/ End of RDWIND ----------------------------------------------------- /
!/
      END SUBROUTINE RDWIND
!/
!/ End of W3SBS1 ----------------------------------------------------- /
!/
      END PROGRAM W3SBS1
