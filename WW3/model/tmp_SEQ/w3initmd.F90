#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3INITMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         25-Sep-2020 |
!/                  +-----------------------------------+
!/
!/    28-Dec-2004 : Origination (out of W3WAVEMD).      ( version 3.06 )
!/                  Multiple grid version.
!/    03-Jan-2005 : Add US2x to MPI communication.      ( version 3.06 )
!/    04-Jan-2005 : Add grid output flags to W3INIT.    ( version 3.06 )
!/    07-Feb-2005 : Combined vs. separate test output.  ( version 3.07 )
!/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
!/    21-Jul-2005 : Add output fields.                  ( version 3.07 )
!/    09-Nov-2005 : Drying out of points added.         ( version 3.08 )
!/    13-Jun-2006 : Splitting STORE in G/SSTORE.        ( version 3.09 )
!/    26-Jun-2006 : adding wiring for output type 6.    ( version 3.09 )
!/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
!/    02-Aug-2006 : Adding W3MPIP.                      ( version 3.10 )
!/    02-Nov-2006 : Adding partitioning options.        ( version 3.10 )
!/    11-Jan-2007 : Updating IAPPRO computation.        ( version 3.10 )
!/    02-Apr-2007 : Add partitioned field data.         ( version 3.11 )
!/                  Add user-defined field data.
!/    01-May-2007 : Move O7a output to W3IOPP.          ( version 3.11 )
!/    08-May-2007 : Starting from calm as an option.    ( version 3.11 )
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
!/    29-Feb-2008 : Add NEC compiler directives.        ( version 3.13 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    23-Jul-2009 : Implement unstructured grids        ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    02-Sep.2012 : Set up for > 999 test files.        ( version 4.10 )
!/                  Reset UST initialization.
!/    03-Sep-2012 : Switch test file on/off (TSTOUT)    ( version 4.10 )
!/    03-Sep-2012 : Clean up of UG grids                ( version 4.08 )
!/    30-Sep-2012 : Implemetation of tidal constituents ( version 4.09 )
!/    07-Dec-2012 : Initialize UST non-zero.            ( version 4.11 )
!/    12-Dec-2012 : Changes for SMC grid.  JG_Li        ( version 4.11 )
!/    26-Dec-2012 : Modify field output MPI for new     ( version 4.11 )
!/                  structure and smaller memory footprint.
!/    02-Jul-2013 : Bug fix MPI_FLOAT -> MPI_REAL.      ( version 4.11 )
!/    10-Oct-2013 : CG and WN values at DMIN for ISEA=0 ( version 4.12 )
!/    14-Nov-2013 : Remove UST(DIR) initialization.     ( version 4.13 )
!/    15-Dec-2013 : Adds fluxes to ice                  ( version 5.01 )
!/    01-May-2017 : Adds directional MSS parameters     ( version 6.04 )
!/    05-Jun-2018 : Adds PDLIB/MEMCHECK/DEBUG           ( version 6.04 )
!/    21-Aug-2018 : Add WBT parameter                   ( version 6.06 )
!/    26-Aug-2018 : UOST (Mentaschi et al. 2015, 2018)  ( version 6.06 )
!/    25-Sep-2020 : Extra fields for coupling restart   ( version 7.10 )
!/
!/    Copyright 2009-2013 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!/    Note: Changes in version numbers not logged above.
!/
!  1. Purpose :
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      CRITOS    R.P.  Public   Critical percentage of resources used
!                               for output to trigger warning.
!      WWVER     C*10  Public   Model version number.
!      SWITCHES  C*256 Public   switches taken from bin/switch
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3INIT    Subr. Public   Wave model initialization.
!      W3MPII    Subr. Public   Initialize MPI data transpose.
!      W3MPIO    Subr. Public   Initialize MPI output gathering.
!      W3MPIP    Subr. Public   Initialize MPI point output gathering.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!     See subroutine documentation.
!
!  5. Remarks :
!
!  6. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!       !/MPI   Id.
!
!       !/S     Enable subroutine tracing.
!       !/Tn    Enable test output.
!       !/MPIT  Enable test output (MPI).
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
      PUBLIC
!/
      REAL, PARAMETER                :: CRITOS = 15.
      CHARACTER(LEN=10), PARAMETER   :: WWVER  = '7.12  '
      CHARACTER(LEN=512), PARAMETER  :: SWITCHES  = &
                    'F90 NOGRB NC4 SHRD PR3 UQ FLX0 LN1 ST4 STAB0 NL1 BT0 DB0 MLIM TR0 BS0 IC0 IS0 REF1 XX0 WNT2 WNX1 RWN' // &
                    'D CRT1 CRX1 IG1 TRKNC COAWST O0 O1 O2 O2a O2b O2c O3 O4 O5 O6 O7' // &
                    ''
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3INIT ( IMOD, IsMulti, FEXT, MDS, MTRACE, ODAT      &
                          , FLGRD,                               &
                           FLGR2, FLGD, FLG2, NPT, XPT, YPT, PNAMES,   &
                          IPRT, PRTFRM, MPI_COMM, FLAGSTIDEIN)
 
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         03-Sep-2012 |
!/                  +-----------------------------------+
!/
!/    17-Mar-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    13-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    14-Feb-2000 : Exact-NL added.                     ( version 2.01 )
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    24-Jan-2002 : Zero time step for data ass.        ( version 2.17 )
!/    18-Feb-2002 : Point output diagnostics added.     ( version 2.18 )
!/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
!/    20-Aug-2003 : Output server options added.        ( version 3.04 )
!/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/                  Taken out of W3WAVE.
!/    04-Jan-2005 : Add grid output flags to par list.  ( version 3.06 )
!/    07-Feb-2005 : Combined vs. separate test output.  ( version 3.07 )
!/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
!/    09-Nov-2005 : Drying out of points added.         ( version 3.08 )
!/    26-Jun-2006 : adding wiring for output type 6.    ( version 3.09 )
!/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    02-Aug-2006 : Adding W3MPIP.                      ( version 3.10 )
!/    02-Nov-2006 : Adding partitioning options.        ( version 3.10 )
!/    11-Jan-2007 : Updating IAPPRO computation.        ( version 3.10 )
!/    01-May-2007 : Move O7a output to W3IOPP.          ( version 3.11 )
!/    08-May-2007 : Starting from calm as an option.    ( version 3.11 )
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    21-Jun-2007 : Dedicated output processes.         ( version 3.11
!/    13-Sep-2009 : Add coupling option                 ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.1 )
!/                  (A. Roland and F. Ardhuin)
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    02-Sep.2012 : Set up for > 999 test files.        ( version 4.10 )
!/    03-Sep-2012 : Switch test file on/off (TSTOUT)    ( version 4.10 )
!/    03-Sep-2012 : Clean up of UG grids                ( version 4.08 )
!/
!  1. Purpose :
!
!     Initialize WAVEWATCH III.
!
!  2. Method :
!
!     Initialize data structure and wave fields from data files.
!     Initialize grid from local and instantaneous data.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number.
!       FEXT    Char   I   Extension of data files.
!       MDS     I.A.   I   Array with dataset numbers (see below),
!                          saved as NDS in W3ODATMD.
!                           1: General output unit number ("log file").
!                           2: Error output unit number.
!                           3: Test output unit number.
!                           4: "screen", i.e., direct output location,
!                              can be the screen or the output file of
!                              the shell.
!                           5: Model definition file unit number.
!                           6: Restart file unit number.
!                           7: Grid output file unit number.
!                           8: Point output file unit number.
!                           9: Input boundary data file unit number.
!                          10: Output boundary data file unit number
!                              (first).
!                          11: Track information file unit number.
!                          12: Track output file unit number.
!       MTRACE  I.A.   I   Array with subroutine tracing information.
!                           1: Output unit number for trace.
!                           2: Maximum number of trace prints.
!       ODAT    I.A.   I   Output data, five parameters per output type
!                           1-5  Data for OTYPE = 1; gridded fields.
!                                1 YYYMMDD for first output.
!                                2 HHMMSS for first output.
!                                3 Output interval in seconds.
!                                4 YYYMMDD for last output.
!                                5 HHMMSS for last output.
!                           6-10 Id. for OTYPE = 2; point output.
!                          11-15 Id. for OTYPE = 3; track point output.
!                          16-20 Id. for OTYPE = 4; restart files.
!                          21-25 Id. for OTYPE = 5; boundary data.
!                          31-35 Id. for OTYPE = 7; coupling data.
!                          36-40 Id. for OTYPE = 8; second restart file
!       FLGRD   L.A.   I   Flags for gridded output.
!       FLGR2   L.A.   I   Flags for coupling output.
!       NPT     Int.   I   Number of output points
!       X/YPT   R.A.   I   Coordinates of output points.
!       PNAMES  C.A.   I   Output point names.
!       IPRT    I.A.   I   Partitioning grid info.
!       PRTFRM  I.A.   I   Partitioning format flag.
!       MPI_COMM Int.  I   MPI communicator to be used for model.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETG    Subr. W3GDATMD Point to data structure.
!      W3SETW    Subr. W3WDATMD Point to data structure.
!      W3DIMW    Subr.   Id.    Set array sizes in data structure.
!      W3SETA    Subr. W3ADATMD Point to data structure.
!      W3DIMA    Subr.   Id.    Set array sizes in data structure.
!      W3SETI    Subr. W3IDATMD Point to data structure.
!      W3DIMI    Subr.   Id.    Set array sizes in data structure.
!      W3SETO    Subr. W3ODATMD Point to data structure.
!      W3DMO5    Subr.   Id.    Set array sizes in data structure.
!      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
!      STRACE    Subr.   Id.    Subroutine tracing.
!      EXTCDE    Subr.   Id.    Program abort.
!      WWDATE    Subr.   Id.    System date.
!      WWTIME    Subr.   Id.    System time.
!      DSEC21    Func. W3TIMEMD Compute time difference.
!      TICK21    Func.   Id.    Advance the clock.
!      STME21    Func.   Id.    Print the time readable.
!      PRTBLK    Func. W3ARRYMD Print plot of array.
!      W3IOGR    Subr. W3IOGRMD Read/write model definition file.
!      W3IORS    Subr. W3IORSMD Read/write restart file.
!      W3IOPP    Subr. W3IOPOMD Preprocess point output.
!      CALL MPI_COMM_SIZE, CALL MPI_COMM_RANK
!                Subr. mpif.h   Standard MPI routines.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Any program shell or integrated model which uses WAVEWATCH III.
!
!  6. Error messages :
!
!     On opening of log file only. Other error messages are generated
!     by W3IOGR and W3IORS.
!
!  7. Remarks :
!
!     - The log file is called 'log.FEXT', where FEXT is passed to
!       the routine.
!     - The test output file is called 'test.FEXT' in shared memory
!       version or testNNN.FEXT in distributed memory version.
!     - A water level and ice coverage are transferred with the
!       restart file. To assure consistency within the model, the
!       water level and ice coverage are re-evaluated at the 0th
!       time step in the actual wave model routine.
!     - When running regtests in cases where disk is non-local
!       (i.e. NFS used), there can be a huge improvment in compute
!       time by using /var/tmp/ for log files.
!       See commented line at "OPEN (MDS(1),FILE=..."
!
!  8. Structure :
!
!     ----------------------------------------------------
!      1.  Set-up of idata structures and I/O.
!        a Point to proper data structures.
!        b Number of processors and processor number.
!        c Open files.
!        d Dataset unit numbers
!        e Subroutine tracing
!        f Initial and test outputs
!      2.  Model definition.
!        a Read model definition file         ( W3IOGR )
!        b Save MAPSTA.
!        c MPP preparation
!      3.  Model initialization.
!        a Read restart file.                 ( W3IORS )
!        b Compare grid and restart MAPSTA.
!        c Initialize with winds if requested (set flag).
!        d Initialize calm conditions if requested.
!        e Preparations for prop. scheme.
!      4.  Set-up output times.
!        a Unpack ODAT.
!        b Check if output available.
!        c Get first time per output and overall.
!        d Prepare point output               ( W3IOPP )
!      5.  Define wavenumber grid.
!        a Calculate depth.
!        b Fill wavenumber and group velocity arrays.
!      6.  Initialize arrays.
!      7.  Write info to log file.
!      8.  Final MPI set up  ( W3MPII , W3MPIO , W3MPIP )
!     ----------------------------------------------------
!
!  9. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!       !/MPI   Id.
!
!       !/S     Enable subroutine tracing.
!       !/Tn    Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
!/
      USE W3GDATMD, ONLY: W3SETG, RSTYPE
      USE W3WDATMD, ONLY: W3SETW, W3DIMW
      USE W3ADATMD, ONLY: W3SETA, W3DIMA
      USE W3IDATMD, ONLY: W3SETI, W3DIMI
      USE W3ODATMD, ONLY: W3SETO, W3DMO5
      USE W3IOGOMD, ONLY: W3FLGRDUPDT
      USE W3IOGRMD, ONLY: W3IOGR
      USE W3IORSMD, ONLY: W3IORS
      USE W3IOPOMD, ONLY: W3IOPP
      USE W3SERVMD, ONLY: ITRACE, EXTCDE, WWDATE, WWTIME
      USE W3TIMEMD, ONLY: DSEC21, TICK21, STME21
      USE W3ARRYMD, ONLY: PRTBLK
!/
      USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, MAPSTA, MAPST2, MAPFS, &
                          MAPSF, FLAGLL,   &
                          ICLOSE, ZB, TRNX, TRNY, DMIN, DTCFL, DTMAX, &
                          FLCK, NK, NTH, NSPEC, SIG, GNAME
      USE W3WDATMD, ONLY: TIME, TLEV, TICE, WLV, UST, USTDIR, VA
      USE W3ODATMD, ONLY: NDSO, NDSE, NDST, SCREEN, NDS, NTPROC,      &
                          NAPROC, IAPROC, NAPLOG, NAPOUT, NAPERR,     &
                          NAPFLD, NAPPNT, NAPTRK, NAPRST, NAPBPT,     &
                          NAPPRT, TOFRST, DTOUT, TONEXT, TOLAST,      &
                          FLOUT, FLOGRD, FLBPO, NOPTS, PTNME,         &
                          PTLOC, IPTINT, PTIFAC, UNDEF, IDOUT, FLBPI, &
                          OUTPTS, FNMPRE, IX0, IXN, IXS, IY0, IYN,    &
                          IYS, FLFORM, IOSTYP, UNIPTS, UPPROC, NOTYPE,&
                          FLOGR2, NOGRP, NGRPP, FLOGD, FLOG2
      USE W3ADATMD, ONLY: NSEALM, IAPPRO, FLCOLD, FLIWND, DW, CG, WN, &
                          UA, UD, U10, U10D, AS
      USE W3IDATMD, ONLY: FLLEV, FLCUR, FLWIND, FLICE, FLMDN, FLMTH,  &
                          FLMVS, FLIC1, FLIC2, FLIC3, FLIC4, FLIC5
      USE W3DISPMD, ONLY: WAVNU1, WAVNU3
      USE W3PARALL, ONLY : AC_tot
      USE W3PARALL, ONLY: SET_UP_NSEAL_NSEALM
     USE W3GDATMD, ONLY: GTYPE, UNGTYPE
      USE W3GDATMD, ONLY: FSN,FSPSI,FSFCT,FSNIMP, FSTOTALIMP, FSTOTALEXP
      USE W3GDATMD, ONLY: FSREFRACTION, FSFREQSHIFT
      USE W3PARALL, ONLY: INIT_GET_JSEA_ISPROC, INIT_GET_ISEA
!/
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)           :: IMOD, MDS(13), MTRACE(2),      &
                                       ODAT(40),NPT, IPRT(6),&
                                       MPI_COMM
      LOGICAL, INTENT(IN)           :: IsMulti
      REAL, INTENT(INOUT)           :: XPT(NPT), YPT(NPT)
      LOGICAL, INTENT(INOUT)        :: FLGRD(NOGRP,NGRPP), FLGD(NOGRP),&
                                       FLGR2(NOGRP,NGRPP), FLG2(NOGRP),&
                                       PRTFRM
      CHARACTER, INTENT(IN)         :: FEXT*(*)
      CHARACTER(LEN=40), INTENT(IN) :: PNAMES(NPT)
      LOGICAL, INTENT(IN), OPTIONAL :: FLAGSTIDEIN(4)
      INTEGER                       :: NSEALout, NSEALMout
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      integer :: IRANK, I, ISTAT
      INTEGER                 :: IE, IFL, IFT, IERR, NTTOT, NTLOC,    &
                                 NTTARG, IK, IP, ITH, IX, IY, &
                                 J, J0, TOUT(2), TLST(2), ISEA, IS,   &
                                 K, I1, I2, JSEA, NTTMAX
      INTEGER, ALLOCATABLE    :: NT(:), MAPTST(:,:)
      REAL                    :: DTTST, DEPTH, FRACOS
      REAL                    :: FACTOR
      REAL                    :: WLVeff
      LOGICAL                 :: OPENED
      CHARACTER(LEN=8)        :: STTIME
      CHARACTER(LEN=10)       :: STDATE
      INTEGER                 :: ISPROC
      CHARACTER(LEN=23)       :: DTME21
      CHARACTER(LEN=30)       :: LFILE, TFILE
!/
!/ ------------------------------------------------------------------- /
!
! 1.  Set-up of data structures and I/O  ----------------------------- /
! 1.a Point to proper data structures.
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 1")
 
      CALL W3SETO ( IMOD, MDS(2), MDS(3) )
      CALL W3SETG ( IMOD, MDS(2), MDS(3) )
      CALL W3SETW ( IMOD, MDS(2), MDS(3) )
      CALL W3SETA ( IMOD, MDS(2), MDS(3) )
      CALL W3SETI ( IMOD, MDS(2), MDS(3) )
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 2")
 
 
!
! 1.b Number of processors and processor number.
!     Overwrite some initializations from W3ODATMD.
!
!     *******************************************************
!     *** NOTE : OUTPUT PROCESSOR ASSIGNMENT NEEDS TO BE  ***
!     ***        CONSISTENT WITH ASSIGNMENT IN WMINIT.    ***
!     *******************************************************
!
      NTPROC = 1
      NAPROC = 1
      IAPROC = 1
      IOSTYP = 1
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 3")
      IF ( IOSTYP .LE. 1 ) THEN
!
          NAPFLD = MAX(1,NAPROC-1)
          NAPPNT = MAX(1,NAPROC-2)
          NAPTRK = MAX(1,NAPROC-5)
          NAPRST = NAPROC
          NAPBPT = MAX(1,NAPROC-3)
          NAPPRT = MAX(1,NAPROC-4)
!
        ELSE
!
          NAPPNT = NAPROC
          IF ( UNIPTS .AND. UPPROC ) NAPROC = MAX(1,NTPROC - 1)
          NAPFLD = NAPROC
          NAPRST = NAPROC
          NAPBPT = NAPROC
          NAPTRK = NAPROC
          NAPPRT = NAPROC
!
          IF ( IOSTYP .EQ. 2 ) THEN
              NAPROC = MAX(1,NAPROC-1)
            ELSE IF ( IOSTYP .EQ. 3 ) THEN
!
! For field or coupling output
!
              IF ( ODAT( 3).GT.0 .OR.  ODAT(33).GT.0 ) THEN
                  NAPFLD =       NAPROC
                  NAPROC = MAX(1,NAPROC-1)
                END IF
              IF ( ODAT(13).GT.0 ) THEN
                  NAPTRK =       NAPROC
                  NAPROC = MAX(1,NAPROC-1)
                END IF
              IF ( ODAT(28).GT.0 ) THEN
                  NAPPRT =       NAPROC
                  NAPROC = MAX(1,NAPROC-1)
                END IF
              IF ( ODAT( 8).GT.0 ) NAPPNT = NAPROC
              IF ( ODAT(18).GT.0 ) NAPRST = NAPROC
              IF ( ODAT(23).GT.0 ) NAPBPT = NAPROC
              IF ( ( ODAT( 8).GT.0 .OR. ODAT(18).GT.0 .OR.            &
                     ODAT(23).GT.0 ) ) NAPROC = MAX(1,NAPROC-1)
            END IF
        END IF
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 4")
      FRACOS = 100. * REAL(NTPROC-NAPROC) / REAL(NTPROC)
      IF ( FRACOS.GT.CRITOS .AND. IAPROC.EQ.NAPERR )                  &
                                           WRITE (NDSE,8002) FRACOS
!
!!!/PDLIB    CALL W3SETG(IMOD, NDSE, NDST)
!
           LPDLIB = .FALSE.
           IF (FSTOTALIMP .and. .NOT. LPDLIB) THEN
             WRITE(NDSE,*) 'IMPTOTAL is selected'
             WRITE(NDSE,*) 'But PDLIB is not'
             STOP 'Stop, case 1'
           ELSE IF (FSTOTALEXP .and. .NOT. LPDLIB) THEN
             WRITE(NDSE,*) 'EXPTOTAL is selected'
             WRITE(NDSE,*) 'But PDLIB is not'
             STOP 'Stop, case 1'
           END IF
!
! 1.c Open files without unpacking MDS ,,,
!
      IE     = LEN_TRIM(FEXT)
      LFILE  = 'log.' // FEXT(:IE)
      IFL    = LEN_TRIM(LFILE)
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 5")
       TFILE  = 'test.' // FEXT(:IE)
      IFT    = LEN_TRIM(TFILE)
      J      = LEN_TRIM(FNMPRE)
!
      IF ( OUTPTS(IMOD)%IAPROC .EQ. OUTPTS(IMOD)%NAPLOG )             &
          OPEN (MDS(1),FILE=FNMPRE(:J)//LFILE(:IFL),ERR=888,IOSTAT=IERR)
!
      IF ( MDS(3).NE.MDS(1) .AND. MDS(3).NE.MDS(4) .AND. TSTOUT ) THEN
          INQUIRE (MDS(3),OPENED=OPENED)
          IF ( .NOT. OPENED ) OPEN                                    &
               (MDS(3),FILE=FNMPRE(:J)//TFILE(:IFT),ERR=889,IOSTAT=IERR)
        END IF
!
! 1.d Dataset unit numbers
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 6")
      NDS    = MDS
      NDSO   = NDS(1)
      NDSE   = NDS(2)
      NDST   = NDS(3)
      SCREEN = NDS(4)
!
! 1.e Subroutine tracing
!
      CALL ITRACE ( MTRACE(1), MTRACE(2) )
!
! 1.f Initial and test outputs
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 7")
 
      IF ( IAPROC .EQ. NAPLOG ) THEN
          CALL WWDATE ( STDATE )
          CALL WWTIME ( STTIME )
          WRITE (NDSO,900) WWVER, STDATE, STTIME
        END IF
 
!
! 2.  Model defintition ---------------------------------------------- /
! 2.a Read model defintition file
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 8")
      CALL W3IOGR ( 'READ', NDS(5), IMOD, FEXT )
 
! Update of output parameter flags based on mod_def parameters (for 3D arrays)
      CALL W3FLGRDUPDT ( NDSO, NDSE, FLGRD, FLGR2, FLGD, FLG2 )
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 9")
 
      IF ( FLAGLL ) THEN
          FACTOR = 1.
        ELSE
          FACTOR = 1.E-3
        END IF
      IF ( IAPROC .EQ. NAPLOG ) WRITE (NDSO,920)
!
! 2.b Save MAPSTA
!
      ALLOCATE ( MAPTST(NY,NX) )
      MAPTST  = MAPSTA
 
!
! 2.c MPP preparation
! 2.c.1 Set simple counters and variables
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 10")
      CALL SET_UP_NSEAL_NSEALM(NSEALout, NSEALMout)
      NSEAL=NSEALout
      NSEALM=NSEALMout
 
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 11")
 
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 12")
!
! 2.c.2 Allocate arrays
!
      IF ( IAPROC .LE. NAPROC ) THEN
          CALL W3DIMW ( IMOD, NDSE, NDST )
        ELSE
          CALL W3DIMW ( IMOD, NDSE, NDST, .FALSE. )
        END IF
      CALL W3DIMA ( IMOD, NDSE, NDST )
      CALL W3DIMI ( IMOD, NDSE, NDST , FLAGSTIDEIN )
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 13")
 
!
! 2.c.3 Calculated expected number of prop. calls per processor
!
      NTTOT  = 0
      DO IK=1, NK
        NTLOC  = 1 + INT(DTMAX/(DTCFL*SIG(IK)/SIG(1))-0.001)
        NTTOT  = NTTOT + NTLOC*NTH
        END DO
      NTTARG = 1 + (NTTOT-1)/NAPROC
      NTTARG = NTTARG + INT(DTMAX/(DTCFL*SIG(NK)/SIG(1))-0.001)
      NTTMAX = NTTARG + 5
!
! 2.c.4 Initialize IAPPRO
!
      IAPPRO = 1
      ALLOCATE ( NT(NSPEC) )
      NT     = NTTOT
!
! 2.c.5 First sweep filling IAPPRO
!
! 2.c.6 Second sweep filling IAPPRO
!
! 2.c.7 Check if all served
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 14")
! 2.c.8 Test output
!
! 2.c.9 Test if any spectral points are left out
!
      DEALLOCATE ( NT )
!
! 3.  Model initialization ------------------------------------------- /
! 3.a Read restart file
!
      VA(:,:) = 0.
      CALL W3IORS ( 'READ', NDS(6), SIG(NK), IMOD)
 
      FLCOLD = RSTYPE.LE.1  .OR. RSTYPE.EQ.4
      IF ( IAPROC .EQ. NAPLOG ) THEN
          IF (RSTYPE.EQ.0) THEN
              WRITE (NDSO,930) 'cold start (idealized).'
            ELSE IF ( RSTYPE .EQ. 1 ) THEN
              WRITE (NDSO,930) 'cold start (wind).'
            ELSE IF ( RSTYPE .EQ. 4 ) THEN
              WRITE (NDSO,930) 'cold start (calm).'
            ELSE
              WRITE (NDSO,930) 'full restart.'
            END IF
        END IF
 
!
! 3.b Compare MAPSTA from grid and restart
!
      DO IX=1, NX
        DO IY=1, NY
          IF ( ABS(MAPSTA(IY,IX)).EQ.2 .OR.                           &
               ABS(MAPTST(IY,IX)).EQ.2 ) THEN
              MAPSTA(IY,IX) = SIGN ( MAPTST(IY,IX) , MAPSTA(IY,IX) )
            END IF
          END DO
        END DO
 
!
! 3.b2 Set MAPSTA associated to PDLIB
!
! 3.c Initialization from wind fields
!
      FLIWND = RSTYPE.EQ.1
!
! 3.d Initialization with calm conditions
!
      IF ( RSTYPE .EQ. 4 ) THEN
          VA(:,:) = 0.
        END IF
 
!
! 3.e Prepare propagation scheme
!
      IF ( .NOT. FLCUR ) FLCK = .FALSE.
!
! 4.  Set-up output times -------------------------------------------- *
! 4.a Unpack ODAT
!
      DO J=1, NOTYPE
        J0 = (J-1)*5
        TONEXT(1,J) =        ODAT(J0+1)
        TONEXT(2,J) =        ODAT(J0+2)
        DTOUT (  J) = REAL ( ODAT(J0+3) )
        TOLAST(1,J) =        ODAT(J0+4)
        TOLAST(2,J) =        ODAT(J0+5)
      END DO
!
! J=8, second stream of restart files
        J=8
        J0 = (J-1)*5
      IF(ODAT(J0+1) .NE. 0) THEN
        TONEXT(1,J) =        ODAT(J0+1)
        TONEXT(2,J) =        ODAT(J0+2)
        DTOUT (  J) = REAL ( ODAT(J0+3) )
        TOLAST(1,J) =        ODAT(J0+4)
        TOLAST(2,J) =        ODAT(J0+5)
        FLOUT(8) = .TRUE.
      ELSE
        FLOUT(8) = .FALSE.
      END IF
!
! 4.b Check if output available
!
      FLOUT(1) = .FALSE.
      FLOGRD   = FLGRD
      FLOGD    = FLGD
      DO J=1, NOGRP
        DO K=1, NGRPP
          FLOUT(1) = FLOUT(1) .OR. FLOGRD(J,K)
        END DO
      END DO
!
      FLOUT(7) = .FALSE.
      FLOGR2   = FLGR2
      FLOG2    = FLG2
      DO J=1, NOGRP
        DO K=1, NGRPP
          FLOUT(7) = FLOUT(7) .OR. FLOGR2(J,K)
        END DO
      END DO
!
      FLOUT(2) = NPT .GT. 0
!
      FLOUT(3) = .TRUE.
!
      FLOUT(4) = .TRUE.
!
      FLOUT(5) = FLBPO
      IF ( FLBPO ) THEN
          CALL W3DMO5 ( IMOD, NDSE, NDST, 4 )
        ELSE
          DTOUT(5) = 0.
        END IF
!
      IX0    = MAX (  1, IPRT(1) )
      IXN    = MIN ( NX, IPRT(2) )
      IXS    = MAX (  1, IPRT(3) )
      IY0    = MAX (  1, IPRT(4) )
      IYN    = MIN ( NY, IPRT(5) )
      IYS    = MAX (  1, IPRT(6) )
      FLFORM = PRTFRM
      FLOUT(6) = IX0.LE.IXN .AND. IY0.LE.IYN
!
! 4.c Get first time per output and overall.
!
      TOFRST(1) = -1
      TOFRST(2) =  0
!
!      WRITE(*,*) 'We set NOTYPE=0 just for DEBUGGING'
!      NOTYPE=0 ! ONLY FOR DEBUGGING PURPOSE
      DO J=1, NOTYPE
!
! ... check time step
!
        DTOUT(J) = MAX ( 0. , DTOUT(J) )
        FLOUT(J) = FLOUT(J) .AND. ( DTOUT(J) .GT. 0.5 )
!
! ... get first time
!
        IF ( FLOUT(J) ) THEN
            TOUT = TONEXT(:,J)
            TLST = TOLAST(:,J)
!
            DO
              DTTST   = DSEC21 ( TIME , TOUT )
              IF ( ( J.NE.4 .AND. DTTST.LT.0. ) .OR.                  &
                   ( J.EQ.4 .AND. DTTST.LE.0. ) ) THEN
                  CALL TICK21 ( TOUT, DTOUT(J) )
                ELSE
                  EXIT
                END IF
              END DO
!
! ... reset first time
!
            TONEXT(:,J) = TOUT
!
! ... check last time
!
            DTTST  = DSEC21 ( TOUT , TLST )
            IF ( DTTST.LT.0.) FLOUT(J) = .FALSE.
!
! ... check overall first time
!
            IF ( FLOUT(J) ) THEN
                IF ( TOFRST(1).EQ.-1 ) THEN
                    TOFRST = TOUT
                  ELSE
                    DTTST  = DSEC21 ( TOUT , TOFRST )
                    IF ( DTTST.GT.0.) THEN
                        TOFRST = TOUT
                      END IF
                  END IF
              END IF
!
          END IF
!
        END DO
!
! J=8, second stream of restart files
!
      J=8
!
! ... check time step
!
        DTOUT(J) = MAX ( 0. , DTOUT(J) )
        FLOUT(J) = FLOUT(J) .AND. ( DTOUT(J) .GT. 0.5 )
!
! ... get first time
!
        IF ( FLOUT(J) ) THEN
            TOUT = TONEXT(:,J)
            TLST = TOLAST(:,J)
!
            DO
              DTTST   = DSEC21 ( TIME , TOUT )
              IF ( ( J.NE.4 .AND. DTTST.LT.0. ) .OR.                  &
                   ( J.EQ.4 .AND. DTTST.LE.0. ) ) THEN
                  CALL TICK21 ( TOUT, DTOUT(J) )
                ELSE
                  EXIT
                END IF
              END DO
!
! ... reset first time
!
            TONEXT(:,J) = TOUT
!
! ... check last time
!
            DTTST  = DSEC21 ( TOUT , TLST )
            IF ( DTTST.LT.0.) FLOUT(J) = .FALSE.
!
! ... check overall first time
!
            IF ( FLOUT(J) ) THEN
                IF ( TOFRST(1).EQ.-1 ) THEN
                    TOFRST = TOUT
                  ELSE
                    DTTST  = DSEC21 ( TOUT , TOFRST )
                    IF ( DTTST.GT.0.) THEN
                        TOFRST = TOUT
                      END IF
                  END IF
              END IF
!
          END IF
! END J=8
!
 
!
! 4.d Preprocessing for point output.
!
      IF ( FLOUT(2) ) CALL W3IOPP ( NPT, XPT, YPT, PNAMES, IMOD )
!
! 5.  Define wavenumber grid ----------------------------------------- *
! 5.a Calculate depth
!
      MAPTST = MOD(MAPST2/2,2)
      MAPST2 = MAPST2 - 2*MAPTST
 
!
!Li   For multi-resolution SMC grid, these 1-NX and 1-NY nested loops
!Li   may miss the refined cells as they are not 1-1 corresponding to
!Li   the (Nx,NY) regular grid.  The loop is now modified to run over
!Li   full NSEA points.   JGLi24Jan2012
!Li   DO IY=1, NY
!Li     DO IX=1, NX
!Li       ISEA   = MAPFS(IY,IX)
      DO ISEA=1, NSEA
        IX = MAPSF(ISEA,1)
        IY = MAPSF(ISEA,2)
!Li     IF ( ISEA .NE. 0) THEN
          WLVeff=WLV(ISEA)
          DW(ISEA) = MAX ( 0. , WLVeff-ZB(ISEA) )
          IF ( WLVeff-ZB(ISEA) .LE.0. ) THEN
            MAPTST(IY,IX) = 1
            MAPSTA(IY,IX) = -ABS(MAPSTA(IY,IX))
!!/DEBUGINIT     WRITE(740+IAPROC,*) 'ISEA=', ISEA, ' JSEA=', JSEA
!!/DEBUGINIT     WRITE(740+IAPROC,*) 'NSEA=', NSEA, ' NSEAL=', NSEAL
!!/DEBUGINIT     WRITE(740+IAPROC,*) 'IAPROC=', IAPROC, ' ISPROC=', ISPROC
!!/DEBUGINIT     FLUSH(740+IAPROC)
          END IF
!Li     END IF
      END DO
!Li   END DO
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        WLVeff=WLV(ISEA)
        DW(ISEA) = MAX ( 0. , WLVeff-ZB(ISEA) )
        IF ( WLVeff-ZB(ISEA) .LE.0. ) THEN
!!/DEBUGINIT     WRITE(740+IAPROC,*) 'ISEA=', ISEA, ' JSEA=', JSEA
!!/DEBUGINIT     WRITE(740+IAPROC,*) 'NSEA=', NSEA, ' NSEAL=', NSEAL
!!/DEBUGINIT     WRITE(740+IAPROC,*) 'IAPROC=', IAPROC, ' ISPROC=', ISPROC
!!/DEBUGINIT     FLUSH(740+IAPROC)
          VA(:,JSEA) = 0.
        END IF
      END DO
 
!
      MAPST2 = MAPST2 + 2*MAPTST
!
      DEALLOCATE ( MAPTST )
 
!
! 5.b Fill wavenumber and group velocity arrays.
!
      DO IS=0, NSEA
        IF (IS.GT.0) THEN
          DEPTH  = MAX ( DMIN , DW(IS) )
        ELSE
          DEPTH = DMIN
          END IF
!
        DO IK=0, NK+1
!
!         Calculate wavenumbers and group velocities.
          CALL WAVNU1(SIG(IK),DEPTH,WN(IK,IS),CG(IK,IS))
!
          END DO
        END DO
!
! Commented by FA with version 4.12
!      DO IK=1, NK
!        CG(IK,0) = CG(IK,1)
!        WN(IK,0) = WN(IK,1)
!        END DO
!
! 6.  Initialize arrays ---------------------------------------------- /
!     Some initialized in W3IORS
!
      UA     = 0.
      UD     = 0.
      U10    = 0.
      U10D   = 0.
!
      AS     = UNDEF
!
      AS    (0) = 0.
      DW    (0) = 0.
!
! 7.  Write info to log file ----------------------------------------- /
!
      IF ( IAPROC .EQ. NAPLOG ) THEN
!
          WRITE (NDSO,970) GNAME
          IF (   FLLEV    ) WRITE (NDSO,971) 'Prescribed'
          IF (.NOT. FLLEV ) WRITE (NDSO,971) 'No'
          IF (   FLCUR    ) WRITE (NDSO,972) 'Prescribed'
          IF (.NOT. FLCUR ) WRITE (NDSO,972) 'No'
          IF (   FLWIND   ) WRITE (NDSO,973) 'Prescribed'
          IF (.NOT. FLWIND) WRITE (NDSO,973) 'No'
          IF (   FLICE    ) WRITE (NDSO,974) 'Prescribed'
          IF (.NOT. FLICE ) WRITE (NDSO,974) 'No'
!
          IF (   FLMDN    ) WRITE (NDSO,9972) 'Prescribed'
          IF (.NOT. FLMDN ) WRITE (NDSO,9972) 'No'
          IF (   FLMTH    ) WRITE (NDSO,9971) 'Prescribed'
          IF (.NOT. FLMTH ) WRITE (NDSO,9971) 'No'
          IF (   FLMVS    ) WRITE (NDSO,9970) 'Prescribed'
          IF (.NOT. FLMVS ) WRITE (NDSO,9970) 'No'
 
          IF (   FLIC1    ) WRITE (NDSO,9973) 'Prescribed'
          IF (.NOT. FLIC1 ) WRITE (NDSO,9973) 'No'
          IF (   FLIC2    ) WRITE (NDSO,9974) 'Prescribed'
          IF (.NOT. FLIC2 ) WRITE (NDSO,9974) 'No'
          IF (   FLIC3    ) WRITE (NDSO,9975) 'Prescribed'
          IF (.NOT. FLIC3 ) WRITE (NDSO,9975) 'No'
          IF (   FLIC4    ) WRITE (NDSO,9976) 'Prescribed'
          IF (.NOT. FLIC4 ) WRITE (NDSO,9976) 'No'
          IF (   FLIC5    ) WRITE (NDSO,9977) 'Prescribed'
          IF (.NOT. FLIC5 ) WRITE (NDSO,9977) 'No'
 
          IF ( FLOUT(1) ) THEN
              WRITE (NDSO,975)
              DO J=1,NOGRP
              DO K=1,NGRPP
                IF ( FLOGRD(J,K) ) WRITE (NDSO,976) IDOUT(J,K)
                END DO
                END DO
            END IF
!
          IF ( FLOUT(7) ) THEN
              WRITE (NDSO,987)
              DO J=1,NOGRP
              DO K=1,NGRPP
                IF ( FLOGR2(J,K) ) WRITE (NDSO,976) IDOUT(J,K)
                END DO
                END DO
            END IF
!
          IF ( FLOUT(2) ) THEN
              WRITE (NDSO,977) NOPTS
              IF ( NOPTS .EQ. 0 ) THEN
                  WRITE (NDSO,978)
                ELSE
                  IF ( FLAGLL ) THEN
                      WRITE (NDSO,979)
                    ELSE
                      WRITE (NDSO,985)
                    END IF
                  DO IP=1, NOPTS
                    IF ( FLAGLL ) THEN
                        WRITE (NDSO,980) IP, FACTOR*PTLOC(1,IP),      &
                                         FACTOR*PTLOC(2,IP), PTNME(IP)
                      ELSE
                        WRITE (NDSO,986) IP, FACTOR*PTLOC(1,IP),      &
                                         FACTOR*PTLOC(2,IP), PTNME(IP)
                      END IF
                    END DO
                END IF
            END IF
!
          CALL STME21 ( TIME , DTME21 )
          WRITE (NDSO,981) DTME21
          IF (FLLEV) THEN
              CALL STME21 ( TLEV , DTME21 )
              WRITE (NDSO,982) DTME21
            END IF
          IF (FLICE) THEN
              CALL STME21 ( TICE , DTME21 )
              WRITE (NDSO,983) DTME21
            END IF
!
          WRITE (NDSO,984)
!
        END IF
!
      IF ( NOPTS .EQ. 0 ) FLOUT(2) = .FALSE.
 
!
! Boundary set up for the directions
!
!!/PDLIB         CALL VA_SETUP_IOBPD
!
! 8.  Final MPI set up ----------------------------------------------- /
!
      RETURN
!
! Escape locations read errors :
!
 
!
  888 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,8000) IERR
      CALL EXTCDE ( 1 )
!
  889 CONTINUE
! === no process number filtering for test file !!! ===
      WRITE (NDSE,8001) IERR
      CALL EXTCDE ( 2 )
!
! Formats
!
  900 FORMAT ( ' WAVEWATCH III log file            ',                 &
               '                     version ',A/                     &
               ' ==================================',                 &
               '==================================='/                 &
               50X,'date : ',A10/50X,'time :  ',A8)
  920 FORMAT (/' Model definition file read.')
  930 FORMAT ( ' Restart file read; ',A)
!
  970 FORMAT (/' Grid name : ',A)
  971 FORMAT (/' ',A,' water levels.')
  972 FORMAT ( ' ',A,' curents.')
  973 FORMAT ( ' ',A,' winds.')
  974 FORMAT ( ' ',A,' ice fields.')
  9972 FORMAT( ' ',A,' mud density.')
  9971 FORMAT( ' ',A,' mud thickness.')
  9970 FORMAT( ' ',A,' mud viscosity.')
  9973 FORMAT( ' ',A,' ice parameter 1')
  9974 FORMAT( ' ',A,' ice parameter 2')
  9975 FORMAT( ' ',A,' ice parameter 3')
  9976 FORMAT( ' ',A,' ice parameter 4')
  9977 FORMAT( ' ',A,' ice parameter 5')
 
!
  975 FORMAT (/' Gridded output fields : '/                           &
               '--------------------------------------------------')
  976 FORMAT ( '     ',A)
!
  977 FORMAT (/' Point output requested for',I6,' points : '/         &
               '------------------------------------------')
  978 FORMAT (/'      Point output disabled')
  979 FORMAT                                                     &
        (/'      point  |  longitude  |   latitude  |  name  '/  &
     '     --------|-------------|-------------|----------------')
  985 FORMAT                                                     &
        (/'      point  |      X      |      Y      |  name  '/  &
     '     --------|-------------|-------------|----------------')
  980 FORMAT ( 5X,I5,'   |',2(F10.2,'   |'),2X,A)
  986 FORMAT ( 5X,I5,'   |',2(F8.1,'E3   |'),2X,A)
!
  981 FORMAT (/' Initial time     : ',A)
  982 FORMAT ( ' Water level time : ',A)
  983 FORMAT ( ' Ice field time   : ',A)
!
  984 FORMAT (//                                                      &
        37X,'  |       input       |     output    |'/                &
        37X,'  |-------------------|---------------|'/                &
         2X,'   step | pass |    date      time   |',                 &
              ' b w l c i i1 i5 d | g p t r b f c |'/                 &
         2X,'--------|------|---------------------|',                 &
            '-------------------|---------------|'/                   &
         2X,'--------+------+---------------------+',                 &
            '-------------------+---------------+')
  987 FORMAT (/' Coupling output fields : '/                          &
               '--------------------------------------------------')
!
 8000 FORMAT (/' *** WAVEWATCH III ERROR IN W3INIT : '/               &
               '     ERROR IN OPENING LOG FILE'/                      &
               '     IOSTAT =',I5/)
 8001 FORMAT (/' *** WAVEWATCH III ERROR IN W3INIT : '/               &
               '     ERROR IN OPENING TEST FILE'/                     &
               '     IOSTAT =',I5/)
 8002 FORMAT (/' *** WAVEWATCH III WARNING IN W3INIT : '/             &
               '     SIGNIFICANT PART OF RESOURCES RESERVED FOR',     &
                   ' OUTPUT :',F6.1,'%'/)
!
!/
!/ End of W3INIT ----------------------------------------------------- /
!/
      END SUBROUTINE W3INIT
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3MPII ( IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         11-May-2007 |
!/                  +-----------------------------------+
!/
!/    04-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    13-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/                  Taken out of W3WAVE.
!/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
!/    13-Jun-2006 : Splitting STORE in G/SSTORE.        ( version 3.09 )
!/    11-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/
!  1. Purpose :
!
!     Perform initializations for MPI version of model.
!     Data transpose only.
!
!  2. Method :
!
!     Some derived data types are defined.  All communiction in
!     W3GATH, W3SCAT and W3WAVE are initialized so that all
!     communication can be performed with single MPI_STARTALL,
!     MPI_TESTALL and MPI_WAITALL calls.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!
!      MPI_TYPE_VECTOR, MPI_TYPE_COMMIT
!                Subr. mpif.h   MPI derived data type routines.
!
!      MPI_SEND_INIT, MPI_RECV_INIT
!                Subr. mpif.h   MPI persistent communication calls.
!     ----------------------------------------------------------------
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
!     None.
!
!  7. Remarks :
!
!     - Basic MPP set up partially performed in W3INIT.
!     - Each processor has to be able to send out individual error
!       messages in this routine !
!     - No testing on IMOD, since only called by W3INIT.
!     - In version 3.09 STORE was split into a send and receive
!       buffer, to avoid/reduce possible conflicts between the FORTRAN
!       and MPI standards when a gather is posted in a given buffer
!       right after a send is completed.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!       !/MPI   MPI communication calls.
!
!       !/S     Subroutine tracing,
!       !/T     Test output, general.
!       !/MPIT  Test output, MPI communications details.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE W3GDATMD, ONLY: NSEA
      USE W3ADATMD, ONLY: NSEALM
      USE W3GDATMD, ONLY: GTYPE, UNGTYPE
      USE CONSTANTS, ONLY: LPDLIB
      USE W3ODATMD, ONLY: NDST, NAPROC, IAPROC
!/
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: IMOD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NXXXX
!/
!/ ------------------------------------------------------------------- /
!/
!
! 1.  Set up derived data types -------------------------------------- /
!
      NXXXX  = NSEALM * NAPROC
!
! 2.  Set up scatters and gathers for W3WAVE ------------------------- /
!     ( persistent communication calls )
!
! 3.  Set up scatters and gathers for W3SCAT and W3GATH -------------- /
!     Also set up buffering of data.
!
! 3.a Loop over local spectral components
!
! 3.b Loop over non-local processes
!
! ... End of loops
!
! 4.  Initialize buffer management ----------------------------------- /
!
      RETURN
!
! Format statements
!
!/
!/ End of W3MPII ----------------------------------------------------- /
!/
      END SUBROUTINE W3MPII
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3MPIO ( IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         11-Nov-2015 |
!/                  +-----------------------------------+
!/
!/    17-Mar-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    11-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    20-Aug-2003 : Output server options added.        ( version 3.04 )
!/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/                  Taken out of W3WAVE.
!/    03-Jan-2005 : Add US2x to MPI communication.      ( version 3.06 )
!/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
!/    21-Jul-2005 : Add output fields.                  ( version 3.07 )
!/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
!/    02-Aug-2006 : W3MPIP split off.                   ( version 3.10 )
!/    02-Apr-2007 : Add partitioned field data.         ( version 3.11 )
!/                  Add user-defined field data.
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
!/    25-Dec-2012 : Modify field output MPI for new     ( version 4.11 )
!/                  structure and smaller memory footprint.
!/    02-Jul-2013 : Bug fix MPI_FLOAT -> MPI_REAL.      ( version 4.11 )
!/    11-Nov-2015 : Added ICEF                          ( version 5.08 )
!/
!  1. Purpose :
!
!     Prepare MPI persistent communication needed for WAVEWATCH I/O
!     routines.
!
!  2. Method :
!
!     Create handles as needed.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3XDMA    Subr. W3ADATMD Dimension expanded output arrays.
!      W3SETA    Subr.    "     Set pointers for output arrays
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!
!      MPI_SEND_INIT, MPI_RECV_INIT
!                Subr. mpif.h   MPI persistent communication calls.
!     ----------------------------------------------------------------
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
!  7. Remarks :
!
!     - The communication as set up in W3MPII uses tags with number
!       ranging from 1 through NSPEC. New and unique tags for IO
!       related communication are assigned here dynamically.
!     - No testing on IMOD, since only called by W3INIT.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/MPI   MPI communication calls.
!
!       !/S     Enable subroutine tracing.
!       !/MPIT  Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE W3SERVMD, ONLY: EXTCDE
!/
      USE W3GDATMD, ONLY: NSEA
      USE W3ADATMD, ONLY: NSEALM
 
 
 
      USE W3GDATMD, ONLY: GTYPE, UNGTYPE
      USE CONSTANTS, ONLY: LPDLIB
!/
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: IMOD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
!/
!/ ------------------------------------------------------------------- /
!/
!
! 1.  Set-up for W3IOGO ---------------------------------------------- /
!
! NRQMAX is the maximum number of fields, it is the sum of the
! sizes of scalar fields (Hs) + 2-component vectors (CUR) + 3-comp ...
!
! 1.a Sends of fields
!
! 1.b Setting up expanded arrays
!
! 1.c Receives of fields
!
! 2.  Set-up for W3IORS ---------------------------------------------- /
! 2.a General preparations
!
! 2.b Fields at end of file (always)
!
! 2.c Data server mode
!
! 3.  Set-up for W3IOBC ( SENDs ) ------------------------------------ /
!
! 3.a Loops over files and points
!
! 3.b Residence processor of point
!
! 3.c If stored locally, send data
!
! ... End of loops 4.a
!
! 3.d Set-up for W3IOBC ( RECVs ) ------------------------------------ /
!
! 3.e Loops over files and points
!
! 3.f Residence processor of point
!
! 3.g Receive in correct array
!
! ... End of loops 4.e
!
! 4.  Set-up for W3IOTR ---------------------------------------------- /
!
! 4.a U*
!
! 5.  Set-up remaining counters -------------------------------------- /
!
      RETURN
!
!     Formats :
!
!/
!/ End of W3MPIO ----------------------------------------------------- /
!/
      END SUBROUTINE W3MPIO
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3MPIP ( IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         30-Oct-2009 |
!/                  +-----------------------------------+
!/
!/    02-Aug-2006 : Origination.                        ( version 3.10 )
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/
!  1. Purpose :
!
!     Prepare MPI persistent communication needed for WAVEWATCH I/O
!     routines.
!
!  2. Method :
!
!     Create handles as needed.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!
!      MPI_SEND_INIT, MPI_RECV_INIT
!                Subr. mpif.h   MPI persistent communication calls.
!     ----------------------------------------------------------------
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
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/MPI   MPI communication calls.
!
!       !/S     Enable subroutine tracing.
!       !/MPIT  Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
!/
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: IMOD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
        INTEGER                 :: itout
!/
!/ ------------------------------------------------------------------- /
!/
!
! 1.  Set-up for W3IOPE/O ( SENDs ) ---------------------------------- /
!
! 1.a Loop over output locations
!
! 1.b Loop over corner points
!
! 1.c Send if point is stored here
!
! ... End of loop 1.b
!
! ... End of loop 1.a
!
! 1.d Set-up for W3IOPE/O ( RECVs ) ---------------------------------- /
!
! 2.e Loop over output locations
!
! 1.g Receive in correct array
!
! ... End of loop 1.f
!
! ... End of loop 1.e
!
! 1.h Base tag number for track output
!
      RETURN
!
!     Formats :
!
!/
!/ End of W3MPIP ----------------------------------------------------- /
!/
      END SUBROUTINE W3MPIP
!/
!/ End of module W3INITMD -------------------------------------------- /
!/
      END MODULE W3INITMD
