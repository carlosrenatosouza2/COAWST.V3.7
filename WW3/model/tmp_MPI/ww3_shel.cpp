

!#include "w3macros.h"
























































































































































































































































































!/ ------------------------------------------------------------------- /
!/COAWST      SUBROUTINE WW3_init (MyCOMM)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         25-Sep-2020 |
!/                  +-----------------------------------+
!/
!/    19-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
!/    19-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    08-Mar-2000 : Fix time managament bug.            ( version 2.04 )
!/    09-Jan-2001 : Fix FOUT allocation bug.            ( version 2.05 )
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    25-Jan-2002 : Data assimilation set up.           ( version 2.17 )
!/    08-May-2002 : Clean up for timers.                ( version 2.21 )
!/    26-Aug-2002 : Generalizing timer.                 ( version 2.22 )
!/    26-Dec-2002 : Continuously moving grid.           ( version 3.02 )
!/    01-Aug-2003 : Continuously moving grid, input.    ( version 3.03 )
!/    07-Oct-2003 : Fixed NHMAX test.                   ( version 3.05 )
!/    05-Jan-2005 : Multiple grid version.              ( version 3.06 )
!/    04-May-2005 : Change to MPI_COMM[_WAVE.           ( version 3.07 )
!/    26-Jun-2006 : Add wiring for output type 6.       ( version 3.07 )
!/    28-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    28-Oct-2006 : Adding partitioning options.        ( version 3.10 )
!/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Fix format statement 2945.          ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    13-Sep-2009 : Add coupling option                 ( version 3.14_SHOM )
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.4 )
!/                  (A. Roland and F. Ardhuin) 
!/    23-Nov-2011 : Comments clean up                   ( version 4.04 )
!/    06-Mar-2012 : Repairing test output.              ( version 4.07 )
!/    03-Sep-2012 : Output initialization time.         ( version 4.10 )
!/    27-Sep-2012 : Implement use of tidal constituents ( version 4.08 )
!/    04-Feb-2014 : Switched clock to DATE_AND_TIME     ( version 4.18 )
!/                  (A. Chawla and Mark Szyszka)      
!/    23-Apr-2015 : Adding NCEP Coupler                 ( version 5.06 )
!/                  (A. Chawla and Dmitry Sheinin)
!/    24-Apr-2015 : Adding OASIS coupling calls         ( version 5.07 )
!/                  (M. Accensi & F. Ardhuin, IFREMER)
!/    11-May-2015 : Checks dates for output types       ( version 5.08 )
!/    26-Mar-2018 : Sea-point only Wnd/Cur input. JGLi  ( version 6.02 )
!/    15-May-2018 : Update namelist                     ( version 6.05 )
!/    06-Jun-2018 : Add PDLIB/MEMCHECK/NETCDF_QAD/DEBUGINIT ( version 6.04 )
!/    14-Sep-2018 : Remove PALM implementation          ( version 6.06 )
!/    04-Oct-2019 : Inline Output implementation        ( version 6.07 )
!/                  (Roberto Padilla-Hernandez)
!/    16-Jul-2020 : Variable coupling time step         ( version 7.08 )
!/    25-Sep-2020 : Oasis coupling at T+0               ( version 7.10 )
!/
!/    Copyright 2009-2012 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     A generic shell for WAVEWATCH III, using preformatted
!     input fields.
!
!  2. Method :
!
!     Driver for the actual wave model (W3WAVE).
!
!     Files : ww3_shel.inp  Input commands for shell.
!             level.ww3     Water level fields (optional).
!             current.ww3   Current fields (optional).
!             wind.ww3      Wind fields (optional).
!             muddens.ww3   Mud parameter (optional)
!             mudthk.ww3    Mud parameter (optional)
!             mudvisc.ww3   Mud parameter (optional)
!             ice(n).ww3    Ice parameters (n=1 to 5) (optional)
!             ice.ww3       ice concentration fields (optional).
!             data0.ww3     Files with assimilation data (optional).
!             data1.ww3
!             data2.ww3
!
!     The file names of the input files are set in W3FLDO
!
!  3. Parameters :
!
!     Local parameters.
!     ----------------------------------------------------------------
!       NHMAX   I.P.  Maximum number of homogeneous fields.
!
!       NDSI    Int.  General input unit number (shell only).
!       NDSS    Int.  Scratch file.
!       NDSO    Int.  General output unit number (shell only).
!       NDSE    Int.  Error output unit number (shell only).
!       NDST    Int.  Test output unit number (shell only).
!       NDSF    I.A.  Field files unit numbers (shell only).
!       FLH     L.A.  Flags for homogeneous fields.
!       FLAGSC  L.A.  Flags for coupling fields
!       FLAGSCI Log.  Flags for ice ic1 ic5 coupling
!       NH      I.A.  Number of times for homogeneous fields.
!       THO     I.A.  Times of homogeneous fields.
!       TIME0   I.A.  Starting time.
!       TIMEN   I.A.  Ending time.
!     ----------------------------------------------------------------
!
!       NDS, NTRACE, ..., see W3WAVE
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3NMOD    Subr. W3GDATMD Set nummber of data structures
!      W3SETG    Subr.   Id.    Point to data structure.
!      W3NDAT    Subr. W3WDATMD Set nummber of data structures
!      W3SETW    Subr.   Id.    Point to data structure.
!      W3NMOD    Subr. W3ADATMD Set nummber of data structures
!      W3NAUX    Subr.   Id.    Point to data structure.
!      W3NOUT    Subr. W3ODATMD Set nummber of data structures
!      W3SETO    Subr.   Id.    Point to data structure.
!      W3NINP    Subr. W3IDATMD Set nummber of data structures
!      W3SETI    Subr.   Id.    Point to data structure.
!
!      NEXTLN    Subr. W3SERVMD Skip to next input line.
!      STME21    Subr. W3TIMEMD Print date and time readable.
!      DSEC21    Func.   Id.    Difference between times.
!      TICK21    Subr.   Id.    Increment time.
!
!      W3FLDO    Subr. W3FLDSMD Opens and checks input files.
!      W3FLDG    Subr.   Id.    Reads from input files.
!      W3FLDD    Subr.   Id.    Reads from data files.
!      W3FLDH    Subr.   Id.    Udates homogeneous fields.
!
!      W3INIT    Subr. W3INITMD Wave model initialization.
!      W3READFLGRD Subr. W3IOGOMD Reading output fields flags. 
!      W3WAVE    Subr. W3WAVEMD Wave model.
!      W3WDAS    Subr. W3WDASMD Data assimilation interface.
!
!      MPI_INIT, MPI_COMM_SIZE, MPI_COMM_RANK, MPI_BARRIER,
!         MPI_FINALIZE
!                Subr.          Standard MPI routines.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     None, stand-alone program.
!
!  6. Error messages :
!
!     - Checks on I-O.
!     - Check on time interval.
!
!  7. Remarks :
!
!     - A rigourous input check is made in W3INIT.
!     - See W3WDAS for documentation on the set-up of the data
!       assimilation.
!     - in "7.a.2 Check if update is needed"
!       Field is updated when compute time is past old input time, and
!       (in case of homogeneous input field),  grabs field value at next
!       input time, which may in fact be far in the future from current
!       compute time. Example: user says
!       field=1   on 19680101 000000 and 
!       field=100 on 20160101 000000 
!       then on if 7.a.2 is reached on 19680101 010000, WW3 will set 
!       field to 100.
!
!  8. Structure :
!
!     ----------------------------------------------------------------
!        0.   Set up data structures.                ( W3NMOD, etc. )
!        1.   I-O setup.
!          a  For shell.
!          b  For WAVEWATCH III.
!          c  Local parameters.
!        2.   Define input fields
!        3.   Set time frame.
!        4.   Define output
!          a  Loop over types, do
!        +--------------------------------------------------------+
!        | b    Process standard line                             |
!        | c    If type 1: fields of mean wave parameters         |
!        | d    If type 2: point output                           |
!        | e    If type 3: track output                           |
!        | f    If type 4: restart files                          |
!        | g    If type 5: boundary output                        |
!        | h    If type 6: separated wave fields                  |
!        | i    If type 7: coupling fields                        |
!        +--------------------------------------------------------+
!        5.   Initialzations
!          a  Wave model.                              ( W3INIT )
!          b  Read homogeneous field data.
!          c  Prepare input files.                     ( W3FLDO )
!          d  Set field times.
!        6.   If no input fields required, run model in a single
!             sweep and exit.                          ( W3WAVE )
!        7.   Run model with input
!             Do until end time is reached
!        +--------------------------------------------------------+
!        | a  Determine next time interval and input fields.      |
!        |   1  Preparation                                       |
!        |      Loop over input fields                            |
!        | +------------------------------------------------------|
!        | | 2  Check if update is needed                         |
!        | | 3  Update time and fields                 ( W3FLDG ) |
!        | |                                           ( W3FLDH ) |
!        | | 4  Update next ending time                           |
!        | +------------------------------------------------------|
!        | b  Run wave model.                          ( W3WAVE ) |
!        | c  If requested, data assimilation.         ( W3WDAS ) |
!        | d  Final output if needed.                  ( W3WAVE ) |
!        | e  Check time                                          |
!        +--------------------------------------------------------+
!     ----------------------------------------------------------------
!
!  9. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!       !/MPI   Id.
!
!       !/MGW   Moving grid wind correction.
!       !/MGP   Moving grid propagation correction.
!
!       !/T     Enable test output.
!       !/O7    Echo input homogeneous fields.
!
!       !/NCO   NCEP NCO modifications for operational implementation.
!
!       !/F90   Timer function included for F90.
!
!       !/NCC   Ncep Coupler
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/PDLIB      USE CONSTANTS, ONLY: LPDLIB
      USE W3GDATMD
      USE W3WDATMD, ONLY: TIME, VA, W3NDAT, W3DIMW, W3SETW
!/OASIS      USE W3WDATMD, ONLY: TIME00, TIMEEND
!/COAWST      USE W3WDATMD, ONLY: TIMEEND
      USE W3ADATMD, ONLY: W3NAUX, W3DIMA, W3SETA
!/MEMCHECK   USE W3ADATMD, ONLY: MALLINFOS
      USE W3IDATMD
!/OASIS      USE W3ODATMD, ONLY: DTOUT, FLOUT
      USE W3ODATMD, ONLY: W3NOUT, W3SETO
      USE W3ODATMD, ONLY: NAPROC, IAPROC, NAPOUT, NAPERR, NOGRP,      &
                          NGRPP, IDOUT, FNMPRE, IOSTYP, NOTYPE
      USE W3ODATMD, ONLY: FLOGRR, FLOGR, OFILES
!/
      USE W3FLDSMD
      USE W3INITMD
      USE W3WAVEMD
      USE W3WDASMD
!/
      USE W3IOGRMD, ONLY: W3IOGR
      USE W3IOGOMD, ONLY: W3READFLGRD, FLDOUT, W3FLGRDFLAG
      USE W3IORSMD, ONLY: OARST
      USE W3IOPOMD
      USE W3SERVMD, ONLY : NEXTLN, EXTCDE
      USE W3TIMEMD
!/MEMCHECK      USE MallocInfo_m
!/NETCDF_QAD      USE W3NETCDF, only : TIME0_NETCDF_QAD
!/NETCDF_QAD      USE W3NETCDF, only : TIMEN_NETCDF_QAD


!/NCC      USE WW_cc, ONLY: MPI_COMM_WW
!/OASIS USE W3OACPMD, ONLY: CPL_OASIS_INIT, CPL_OASIS_GRID,            &
!/OASIS                     CPL_OASIS_DEFINE, CPL_OASIS_FINALIZE,      &
!/OASIS                     ID_OASIS_TIME, CPLT0
!/OASOCM      USE W3OGCMMD, ONLY: SND_FIELDS_TO_OCEAN
!/OASACM      USE W3AGCMMD, ONLY: SND_FIELDS_TO_ATMOS
!/OASICM      USE W3IGCMMD, ONLY: SND_FIELDS_TO_ICE
!/COAWST      USE CWSTWVCP
!/COAWST      USE MCT_COUPLER_PARAMS
!/TIDE      USE W3TIDEMD
!
      USE W3NMLSHELMD
      IMPLICIT NONE
!
!/MPI      INCLUDE "mpif.h"
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETER statements
!/
      INTEGER, PARAMETER  :: NHMAX =    200
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      TYPE(NML_DOMAIN_T)       :: NML_DOMAIN
      TYPE(NML_INPUT_T)        :: NML_INPUT
      TYPE(NML_OUTPUT_TYPE_T)  :: NML_OUTPUT_TYPE
      TYPE(NML_OUTPUT_DATE_T)  :: NML_OUTPUT_DATE
      TYPE(NML_HOMOG_COUNT_T)  :: NML_HOMOG_COUNT
      TYPE(NML_HOMOG_INPUT_T), ALLOCATABLE  :: NML_HOMOG_INPUT(:)
!
      INTEGER             :: NDSI, NDSI2, NDSS, NDSO, NDSE, NDST, NDSL,&
                             NDSEN, IERR, J, I, ILOOP, IPTS, NPTS,     &
                             NDTNEW, MPI_COMM = -99,                   &
                             FLAGTIDE, COUPL_COMM, IH, N_TOT
      INTEGER             :: NDSF(-7:7), NDS(13), NTRACE(2), NDT(5:7), &
                             TIME0(2), TIMEN(2), TTIME(2), TTT(2),     &
                             NH(-7:8), THO(2,-7:8,NHMAX), RCLD(5:7),   &
                             NODATA(5:7), ODAT(40), IPRT(6) = 0,       &
                             STARTDATE(8), STOPDATE(8), IHH(-7:8)      
!
!/OASIS      INTEGER             :: OASISED
!/COAWST      INTEGER :: COAWSTED, ccount
!/COU      INTEGER             :: OFL
!/F90      INTEGER             :: CLKDT1(8), CLKDT2(8), CLKDT3(8) 
!/MPI      INTEGER             :: IERR_MPI
!/COAWST      INTEGER            :: MyCOMM
!
      REAL                :: FACTOR, DTTST, XX, YY,                    &
                             HA(NHMAX,-7:8), HD(NHMAX,-7:8),           &
                             HS(NHMAX,-7:8)
!/F90      REAL                :: CLKFIN, CLKFEL
      REAL, ALLOCATABLE   :: X(:), Y(:), XXX(:,:), DATA0(:,:),         &
                             DATA1(:,:), DATA2(:,:)
!
      DOUBLE PRECISION    :: STARTJULDAY, STOPJULDAY
!
      CHARACTER(LEN=1)    :: COMSTR, FLAGTFC(-7:8)
      CHARACTER(LEN=3)    :: IDSTR(-7:8), IDTST
      CHARACTER(LEN=6)    :: YESXNO
      CHARACTER(LEN=40)   :: PN
      CHARACTER(LEN=40),                                               &
              ALLOCATABLE :: PNAMES(:)
      CHARACTER(LEN=13)   :: IDFLDS(-7:8)
      CHARACTER(LEN=20)   :: STRNG
      CHARACTER(LEN=23)   :: DTME21
      CHARACTER(LEN=30)   :: IDOTYP(8)
      CHARACTER(LEN=80)   :: LINE
      CHARACTER(LEN=256)  :: TMPLINE, TEST
      CHARACTER(LEN=1024) :: FLDIN
      CHARACTER(LEN=1024) :: FLDRST=''
      CHARACTER(LEN=80)   :: LINEIN
      CHARACTER(LEN=8)    :: WORDS(7)=''

!/COU      CHARACTER(LEN=30)       :: OFILE
!
      LOGICAL             :: FLLSTL, FLLSTI, FLFLG, FLHOM, TFLAGI,     &
                             PRTFRM, FLAGSCI, FLGNML
      LOGICAL             :: FLGRD(NOGRP,NGRPP), FLGD(NOGRP),          &
                             FLGR2(NOGRP,NGRPP), FLG2(NOGRP),          &
                             FLAGSTIDE(4), FLH(-7:8), FLGDAS(3),       &
                             FLLST_ALL(-7:8)
      LOGICAL             :: DEBUG_NCC = .FALSE.
!/NCC      LOGICAL             :: CFLAG(10) 
!/MPI      LOGICAL             :: FLHYBR = .FALSE.
!/OMPH      INTEGER             :: THRLEV
!/OASIS      LOGICAL             :: L_MASTER    
!
!/
!/ ------------------------------------------------------------------- /
!/
      DATA IDFLDS / 'ice param. 1 ' , 'ice param. 2 ' ,               &
                    'ice param. 3 ' , 'ice param. 4 ' ,               &
                    'ice param. 5 ' ,                                 &
                    'mud density  ' , 'mud thkness  ' ,               &
                    'mud viscos.  ' ,                                 &
                    'water levels ' , 'currents     ' ,               &
                    'winds        ' , 'ice fields   ' ,               &
                    'mean param.  ' , '1D spectra   ' ,               &
                    '2D spectra   ' , 'moving grid  ' /
      DATA IDOTYP / 'Fields of mean wave parameters' ,                &
                    'Point output                  ' ,                &
                    'Track point output            ' ,                &
                    'Restart files                 ' ,                &
                    'Nesting data                  ' ,                &
                    'Partitioned wave field data   ' ,                &
                    'Fields for coupling           ' ,                &
                    'Restart files second request  '/
      DATA IDSTR  / 'IC1', 'IC2', 'IC3', 'IC4', 'IC5', 'MDN', 'MTH',  &
                    'MVS', 'LEV', 'CUR', 'WND', 'ICE', 'DT0', 'DT1',  &
                    'DT2', 'MOV' /
!
      FLGR2 = .FALSE.  
      FLAGSTIDE(:) = .FALSE.
      FLH(:)       = .FALSE.
!
!/T      PRTFRM = .TRUE.
!/T      DEBUG_NCC = .TRUE. 
!
!/F90      CALL DATE_AND_TIME ( VALUES=CLKDT1 )
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 0.  Set up data structures
!
!/OASIS     OASISED=1
!/COAWST      COAWSTED=1
!/PDLIB     LPDLIB = .TRUE.
!  
      CALL W3NMOD ( 1, 6, 6 )
      CALL W3NDAT (    6, 6 )
      CALL W3NAUX (    6, 6 )
      CALL W3NOUT (    6, 6 )
      CALL W3NINP (    6, 6 )
!
      CALL W3SETG ( 1, 6, 6 )
      CALL W3SETW ( 1, 6, 6 )
      CALL W3SETA ( 1, 6, 6 )
      CALL W3SETO ( 1, 6, 6 )
      CALL W3SETI ( 1, 6, 6 )

!/MEMCHECK      write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 1'
!/MEMCHECK      call getMallocInfo(mallinfos)
!/MEMCHECK      call printMallInfo(IAPROC,mallInfos)
!
!/SHRD      NAPROC = 1
!/SHRD      IAPROC = 1
!
!/OMPH      FLHYBR = .TRUE.

!/OASIS IF (OASISED.EQ.1) THEN
!/OASIS   CALL CPL_OASIS_INIT(MPI_COMM)
!/OASIS ELSE
!/COAWST      IF (COAWSTED.EQ.0) THEN
!/DEBUGINIT      write(740+IAPROC,*), 'Before MPI_INIT, ww3_shel'
!/OMPH       ! For hybrid MPI-OpenMP specify required thread level. JGLi06Sep2019
!/OMPH       IF( FLHYBR ) THEN
!/OMPH         CALL MPI_INIT_THREAD( MPI_THREAD_FUNNELED, THRLEV, IERR_MPI)
!/OMPH       ELSE
!/MPI      CALL MPI_INIT      ( IERR_MPI )
!/OMPH       ENDIF
!/DEBUGINIT      write(740+IAPROC,*), 'After MPI_INIT, ww3_shel'
!/MPI      MPI_COMM = MPI_COMM_WORLD
!/COAWST      END IF
!/OASIS END IF
!/COAWST      MPI_COMM = MyCOMM
!/COAWST      WAV_COMM_WORLD = MyCOMM
!
!/NCC      CALL WW_CMP_START
!/NCC      MPI_COMM = MPI_COMM_WW
!
!/MPI      CALL MPI_COMM_SIZE ( MPI_COMM, NAPROC, IERR_MPI )
!/DEBUGINIT      write(740+IAPROC,*) 'After MPI_COMM_SIZE, NAPROC=', NAPROC
!/MPI      CALL MPI_COMM_RANK ( MPI_COMM, IAPROC, IERR_MPI )
!/MPI      IAPROC = IAPROC + 1
!
!/NCO/!     IF ( IAPROC .EQ. 1 ) CALL W3TAGB                         &
!/NCO/!                         ('WAVEFCST',1998,0007,0050,'NP21   ')

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 2'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  IO set-up
! 1.a For shell
!
!/DEBUGINIT      WRITE(740+IAPROC,*) 'ww3_shel, step 1'
!/DEBUGINIT      FLUSH(740+IAPROC)
      NDSI   = 10
      NDSS   = 90
      NDSO   =  6
      NDSE   =  6
      NDST   =  6
      NDSL   = 50
!/COU      NDSO   =  333
!/COU      NDSE   =  333
!/COU      NDST   =  333


      NDSF(-7)  = 1008
      NDSF(-6)  = 1009
      NDSF(-5)  = 1010
      NDSF(-4)  = 1011
      NDSF(-3)  = 1012
      NDSF(-2)  = 1013
      NDSF(-1)  = 1014
      NDSF(0)   = 1015

      NDSF(1)  = 11
      NDSF(2)  = 12
      NDSF(3)  = 13
      NDSF(4)  = 14
      NDSF(5)  = 15
      NDSF(6)  = 16
      NDSF(7)  = 17
!/DEBUGINIT      WRITE(740+IAPROC,*) 'ww3_shel, step 2'
!/DEBUGINIT      FLUSH(740+IAPROC)
!
!/NCO/!
!/NCO/! Redo according to NCO
!/NCO/!
!/NCO      NDSI   = 11
!/NCO      NDSS   = 90
!/NCO      NDSO   =  6
!/NCO      NDSE   = NDSO
!/NCO      NDST   = NDSO
!/NCO      NDSF(1)  = 12
!/NCO      NDSF(2)  = 13
!/NCO      NDSF(3)  = 14
!/NCO      NDSF(4)  = 15
!/NCO      NDSF(5)  = 16
!/NCO      NDSF(6)  = 17
!/NCO      NDSF(7)  = 18
!
      NAPOUT = 1
      NAPERR = 1
!
!/COU      OFILE  = 'output.ww3'
!/COU      OFL    = LEN_TRIM(OFILE)
!/COU      J      = LEN_TRIM(FNMPRE)
!/COU      IF ( IAPROC .EQ. NAPOUT )             &
!/COU        OPEN (333,FILE=FNMPRE(:J)//OFILE(:OFL),ERR=2008,IOSTAT=IERR)

      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,900)
!
      IF ( IAPROC .EQ. NAPERR ) THEN
        NDSEN  = NDSE
      ELSE
        NDSEN  = -1
      END IF
!/OMPH      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,905) &
!/OMPH                                        MPI_THREAD_FUNNELED, THRLEV
!

!
! 1.b For WAVEWATCH III (See W3INIT)
!
      NDS( 1) = 20
      NDS( 2) =  6
      NDS( 3) = 21
      NDS( 4) =  6
      NDS( 5) = 30
      NDS( 6) = 30
      NDS( 7) = 31
      NDS( 8) = 32
      NDS( 9) = 33
      NDS(10) = 35
      NDS(11) = 22
      NDS(12) = 23
      NDS(13) = 34
!
      NTRACE(1) =  NDS(3)
      NTRACE(2) =  10
!
!/NCO/!
!/NCO/! Redo according to NCO
!/NCO/!
!/NCO      NDS( 1) = 51
!/NCO      NDS( 2) = NDSO
!/NCO      NDS( 3) = NDSO
!/NCO      NDS( 4) = NDSO
!/NCO      NDS( 5) = 20
!/NCO      NDS( 6) = 21
!/NCO      NDS( 7) = 52
!/NCO      NDS( 8) = 53
!/NCO      NDS( 9) = 22
!/NCO      NDS(10) = 71
!/NCO      NDS(11) = 23
!/NCO      NDS(12) = 54
!/NCO      NDS(13) = 55
!/NCO      NTRACE(1) = NDSO
!
!/T      WRITE (NDST,9000) (NDS(I),I=1,12)
!/T      WRITE (NDST,9001) (NTRACE(I),I=1,2)
!
! 1.c Local parameters
!
! Default COMSTR to "$" (for when using nml input files)
      COMSTR = "$"
!
! inferred from context: these flags (FL) are to indicate that the last (LST) 
!   field has been read from a file.
      FLLSTL = .FALSE. ! This is associated with J.EQ.1 (wlev)
      FLLSTI = .FALSE. ! This is associated with J.EQ.4 (ice)
      FLLST_ALL = .FALSE. ! For all
!/DEBUGINIT      WRITE(740+IAPROC,*) 'ww3_shel, step 3'
!/DEBUGINIT      FLUSH(740+IAPROC)

! If using experimental mud or ice physics, additional lines will
!  be read in from ww3_shel.inp and applied, so JFIRST is changed from
!  its initialization setting "JFIRST=1" to some lower value.
!/IC1      JFIRST=-7
!/IC2      JFIRST=-7
!/IS2      JFIRST=-7
!/IC3      JFIRST=-7
!/BT8      JFIRST=-7
!/BT9      JFIRST=-7
!/IC4      JFIRST=-7
!/IC5      JFIRST=-7

!/DEBUGINIT      WRITE(740+IAPROC,*) 'ww3_shel, step 4'
!/DEBUGINIT      WRITE(740+IAPROC,*) 'JFIRST=', JFIRST
!/DEBUGINIT      FLUSH(740+IAPROC)

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 2a'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Define input fields
!

!
! process ww3_prnc namelist
!
      INQUIRE(FILE=TRIM(FNMPRE)//"ww3_shel.nml", EXIST=FLGNML) 
      IF (FLGNML) THEN
        ! Read namelist
        CALL W3NMLSHEL (MPI_COMM, NDSI, TRIM(FNMPRE)//'ww3_shel.nml',  &
                        NML_DOMAIN, NML_INPUT, NML_OUTPUT_TYPE,        &
                        NML_OUTPUT_DATE, NML_HOMOG_COUNT,             &
                        NML_HOMOG_INPUT, IERR)

! 2.1 forcing flags

        FLH(-7:8)=.FALSE.
        FLAGTFC(-7)=TRIM(NML_INPUT%FORCING%ICE_PARAM1)
        FLAGTFC(-6)=TRIM(NML_INPUT%FORCING%ICE_PARAM2)
        FLAGTFC(-5)=TRIM(NML_INPUT%FORCING%ICE_PARAM3)
        FLAGTFC(-4)=TRIM(NML_INPUT%FORCING%ICE_PARAM4)
        FLAGTFC(-3)=TRIM(NML_INPUT%FORCING%ICE_PARAM5)
        FLAGTFC(-2)=TRIM(NML_INPUT%FORCING%MUD_DENSITY)
        FLAGTFC(-1)=TRIM(NML_INPUT%FORCING%MUD_THICKNESS)
        FLAGTFC(0)=TRIM(NML_INPUT%FORCING%MUD_VISCOSITY)
        FLAGTFC(1)=TRIM(NML_INPUT%FORCING%WATER_LEVELS)
        FLAGTFC(2)=TRIM(NML_INPUT%FORCING%CURRENTS)
        FLAGTFC(3)=TRIM(NML_INPUT%FORCING%WINDS)
        FLAGTFC(4)=TRIM(NML_INPUT%FORCING%ICE_CONC)
        FLAGTFC(5)=TRIM(NML_INPUT%ASSIM%MEAN)
        FLAGTFC(6)=TRIM(NML_INPUT%ASSIM%SPEC1D)
        FLAGTFC(7)=TRIM(NML_INPUT%ASSIM%SPEC2D)

        IF (TRIM(NML_INPUT%FORCING%ICE_PARAM1) .EQ. 'H') THEN
          FLAGTFC(-7)='T'
          FLH(-7)=.TRUE.
        END IF
        IF (TRIM(NML_INPUT%FORCING%ICE_PARAM2) .EQ. 'H') THEN
          FLAGTFC(-6)='T'
          FLH(-6)=.TRUE.
        END IF
        IF (TRIM(NML_INPUT%FORCING%ICE_PARAM3) .EQ. 'H') THEN
          FLAGTFC(-5)='T'
          FLH(-5)=.TRUE.
        END IF
        IF (TRIM(NML_INPUT%FORCING%ICE_PARAM4) .EQ. 'H') THEN
          FLAGTFC(-4)='T'
          FLH(-4)=.TRUE.
        END IF
        IF (TRIM(NML_INPUT%FORCING%ICE_PARAM5) .EQ. 'H') THEN
          FLAGTFC(-3)='T'
          FLH(-3)=.TRUE.
        END IF
        IF (TRIM(NML_INPUT%FORCING%MUD_DENSITY) .EQ. 'H') THEN
          FLAGTFC(-2)='T'
          FLH(-2)=.TRUE.
        END IF
        IF (TRIM(NML_INPUT%FORCING%MUD_THICKNESS) .EQ. 'H') THEN
          FLAGTFC(-1)='T'
          FLH(-1)=.TRUE.
        END IF
        IF (TRIM(NML_INPUT%FORCING%MUD_VISCOSITY) .EQ. 'H') THEN
          FLAGTFC(0)='T'
          FLH(0)=.TRUE.
        END IF
        IF (TRIM(NML_INPUT%FORCING%WATER_LEVELS) .EQ. 'H') THEN
          FLAGTFC(1)='T'
          FLH(1)=.TRUE.
        END IF
        IF (TRIM(NML_INPUT%FORCING%CURRENTS) .EQ. 'H') THEN
          FLAGTFC(2)='T'
          FLH(2)=.TRUE.
        END IF
        IF (TRIM(NML_INPUT%FORCING%WINDS) .EQ. 'H') THEN
          FLAGTFC(3)='T'
          FLH(3)=.TRUE.
        END IF
        IF (TRIM(NML_INPUT%FORCING%ICE_CONC) .EQ. 'H') THEN
          FLAGTFC(4)='T'
          FLH(4)=.TRUE.
        END IF

        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,920)
        DO J=JFIRST, 7
          IF ( J .LE. 4 ) THEN
            IF (FLAGTFC(J).EQ.'T') THEN 
              INFLAGS1(J)=.TRUE.
              FLAGSC(J)=.FALSE.
            END IF
            IF (FLAGTFC(J).EQ.'F') THEN 
              INFLAGS1(J)=.FALSE.
              FLAGSC(J)=.FALSE.
            END IF
            IF (FLAGTFC(J).EQ.'C') THEN 
              INFLAGS1(J)=.TRUE.
              FLAGSC(J)=.TRUE.
            END IF
            FLH(J) = FLH(J) .AND. INFLAGS1(J)
          ELSE
            FLH(J) = .FALSE.
            IF (FLAGTFC(J).EQ.'T') THEN 
              INFLAGS1(J)=.TRUE.
              FLAGSC(J)=.FALSE.
            END IF
            IF (FLAGTFC(J).EQ.'F') THEN 
              INFLAGS1(J)=.FALSE.
              FLAGSC(J)=.FALSE.
            END IF
            IF (FLAGTFC(J).EQ.'C') THEN 
              INFLAGS1(J)=.TRUE.
              FLAGSC(J)=.TRUE.
            END IF
          END IF
          IF ( INFLAGS1(J) ) THEN
            YESXNO = 'YES/--'
          ELSE
            YESXNO = '---/NO'
          END IF
          IF ( FLH(J) ) THEN
            STRNG  = '(homogeneous field) '
          ELSE IF ( FLAGSC(J) ) THEN
            STRNG  = '(coupling field) '
          ELSE
            STRNG  = '                    '
          END IF
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,921) IDFLDS(J), YESXNO, STRNG
        END DO
!/COU          IF (FLAGSC(1) .AND. INFLAGS1(2) .AND. .NOT. FLAGSC(2)) GOTO 2102
!/COU          IF (FLAGSC(2) .AND. INFLAGS1(1) .AND. .NOT. FLAGSC(1)) GOTO 2102

        INFLAGS1(8) = .FALSE.
        FLH(8)   = .FALSE.
!/MGW        INFLAGS1(8) = .TRUE.
!/MGP        INFLAGS1(8) = .TRUE.
!/MGW        FLH(8)   = .TRUE.
!/MGP        FLH(8)   = .TRUE.
        IF ( INFLAGS1(8) .AND. IAPROC.EQ.NAPOUT )                          &
             WRITE (NDSO,921) IDFLDS(8), 'YES/--', ' '
!
        FLFLG  = INFLAGS1(-7) .OR. INFLAGS1(-6) .OR. INFLAGS1(-5) .OR. INFLAGS1(-4) &
                 .OR. INFLAGS1(-3) .OR. INFLAGS1(-2) .OR. INFLAGS1(-1)           &
                 .OR. INFLAGS1(0)  .OR. INFLAGS1(1)  .OR. INFLAGS1(2)            &
                 .OR. INFLAGS1(3)  .OR. INFLAGS1(4)  .OR. INFLAGS1(5)            &
                 .OR. INFLAGS1(6)  .OR. INFLAGS1(7)
        FLHOM  = FLH(-7) .OR. FLH(-6) .OR. FLH(-5) .OR. FLH(-4)       &
                 .OR. FLH(-3) .OR. FLH(-2) .OR. FLH(-1) .OR. FLH(0)   &
                 .OR. FLH(1) .OR. FLH(2) .OR. FLH(3) .OR. FLH(4) .OR. FLH(8)
!
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,922)
!
!       INFLAGS2 is just "initial value of INFLAGS1", i.e. does *not* get
!          changed when model reads last record of ice.ww3
        INFLAGS2=INFLAGS1

!/T        WRITE (NDST,9020) FLFLG, INFLAGS1, FLHOM, FLH



! 2.2 Time setup

        READ(NML_DOMAIN%START,*) TIME0
        CALL T2D(TIME0,STARTDATE,IERR)
        CALL D2J(STARTDATE,STARTJULDAY,IERR)
        READ(NML_DOMAIN%STOP,*) TIMEN
        CALL T2D(TIMEN,STOPDATE,IERR)
        CALL D2J(STOPDATE,STOPJULDAY,IERR)

! 2.3 Domain setup

        IOSTYP = NML_DOMAIN%IOSTYP
        CALL W3IOGR ( 'GRID', NDSF(5) )
        IF ( FLAGLL ) THEN
          FACTOR = 1.
        ELSE
          FACTOR = 1.E-3
        END IF

! 2.4 Output dates

        READ(NML_OUTPUT_DATE%FIELD%START, *)   ODAT(1), ODAT(2)
        READ(NML_OUTPUT_DATE%FIELD%STRIDE, *)  ODAT(3)
        READ(NML_OUTPUT_DATE%FIELD%STOP, *)    ODAT(4), ODAT(5)

        READ(NML_OUTPUT_DATE%FIELD%OUTFFILE, *)  OFILES(1)
!        OUTPTS(I)%OUTSTRIDE(1)=ODAT(3,I)              

        READ(NML_OUTPUT_DATE%POINT%START, *)   ODAT(6), ODAT(7)
        READ(NML_OUTPUT_DATE%POINT%STRIDE, *)  ODAT(8)
        READ(NML_OUTPUT_DATE%POINT%STOP, *)    ODAT(9), ODAT(10)

        READ(NML_OUTPUT_DATE%POINT%OUTFFILE, *)  OFILES(2)
!        OUTPTS(I)%OUTSTRIDE(2)=ODAT(8,I)             

        READ(NML_OUTPUT_DATE%TRACK%START, *)   ODAT(11), ODAT(12)
        READ(NML_OUTPUT_DATE%TRACK%STRIDE, *)  ODAT(13)
        READ(NML_OUTPUT_DATE%TRACK%STOP, *)    ODAT(14), ODAT(15)
        READ(NML_OUTPUT_DATE%RESTART%START, *)   ODAT(16), ODAT(17)
        READ(NML_OUTPUT_DATE%RESTART%STRIDE, *)  ODAT(18)
        READ(NML_OUTPUT_DATE%RESTART%STOP, *)    ODAT(19), ODAT(20)
        READ(NML_OUTPUT_DATE%RESTART2%START, *)   ODAT(36), ODAT(37)
        READ(NML_OUTPUT_DATE%RESTART2%STRIDE, *)  ODAT(38)
        READ(NML_OUTPUT_DATE%RESTART2%STOP, *)    ODAT(39), ODAT(40)
        READ(NML_OUTPUT_DATE%BOUNDARY%START, *)   ODAT(21), ODAT(22)
        READ(NML_OUTPUT_DATE%BOUNDARY%STRIDE, *)  ODAT(23)
        READ(NML_OUTPUT_DATE%BOUNDARY%STOP, *)    ODAT(24), ODAT(25)
        READ(NML_OUTPUT_DATE%PARTITION%START, *)   ODAT(26), ODAT(27)
        READ(NML_OUTPUT_DATE%PARTITION%STRIDE, *)  ODAT(28)
        READ(NML_OUTPUT_DATE%PARTITION%STOP, *)    ODAT(29), ODAT(30)
        READ(NML_OUTPUT_DATE%COUPLING%START, *)   ODAT(31), ODAT(32)
        READ(NML_OUTPUT_DATE%COUPLING%STRIDE, *)  ODAT(33)
        READ(NML_OUTPUT_DATE%COUPLING%STOP, *)    ODAT(34), ODAT(35)

        ! set the time stride at 0 or more
        ODAT(3) = MAX ( 0 , ODAT(3) )
        ODAT(8) = MAX ( 0 , ODAT(8) )
        ODAT(13) = MAX ( 0 , ODAT(13) )
        ODAT(18) = MAX ( 0 , ODAT(18) )
        ODAT(23) = MAX ( 0 , ODAT(23) )
        ODAT(28) = MAX ( 0 , ODAT(28) )
        ODAT(33) = MAX ( 0 , ODAT(33) )
        ODAT(38) = MAX ( 0 , ODAT(38) )
!
!/COU        ! Test the validity of the coupling time step
!/COU        IF (ODAT(33) == 0) THEN
!/COU          IF ( IAPROC .EQ. NAPOUT ) THEN
!/COU            WRITE(NDSO,1010) ODAT(33), INT(DTMAX)
!/COU          END IF
!/COU          ODAT(33) = INT(DTMAX)
!/COU        ELSE IF (MOD(ODAT(33),INT(DTMAX)) .NE. 0) THEN
!/COU          GOTO 2009
!/COU        END IF
!
! 2.5 Output types

        NPTS   = 0
        NOTYPE = 6
!/COU        NOTYPE = 7
        DO J = 1, NOTYPE
!          OUTPTS(I)%OFILES(J)=OFILES(J)              
          IF ( ODAT(5*(J-1)+3) .NE. 0 ) THEN

! Type 1: fields of mean wave parameters
            IF ( J .EQ. 1 ) THEN
              FLDOUT = NML_OUTPUT_TYPE%FIELD%LIST
              CALL W3FLGRDFLAG ( NDSO, NDSO, NDSE, FLDOUT, FLGD,     &
                                 FLGRD, IAPROC, NAPOUT, IERR )
              IF ( IERR .NE. 0 ) GOTO 2222


! Type 2: point output
            ELSE IF ( J .EQ. 2 ) THEN
              OPEN (NDSL, FILE=TRIM(FNMPRE)//TRIM(NML_OUTPUT_TYPE%POINT%FILE), &
                    FORM='FORMATTED', STATUS='OLD', ERR=2104, IOSTAT=IERR)

              ! first loop to count the number of points
              ! second loop to allocate the array and store the points
              IPTS = 0
              DO ILOOP=1,2
                REWIND (NDSL)
!
                IF ( ILOOP.EQ.2) THEN
                  NPTS = IPTS
                  IF ( NPTS.GT.0 ) THEN
                    ALLOCATE ( X(NPTS), Y(NPTS), PNAMES(NPTS) )
                    IPTS = 0 ! reset counter to be reused for next do loop
                  ELSE
                    ALLOCATE ( X(1), Y(1), PNAMES(1) )
                    GOTO 2054
                  END IF
                END IF
!
                DO
                  READ (NDSL,*,ERR=2004,IOSTAT=IERR) TMPLINE
                  ! if end of file or stopstring, then exit
                  IF ( IERR.NE.0 .OR. INDEX(TMPLINE,"STOPSTRING").NE.0 ) EXIT
                  ! leading blanks removed and placed on the right
                  TEST = ADJUSTL ( TMPLINE )
                  IF ( TEST(1:1).EQ.COMSTR .OR. LEN_TRIM(TEST).EQ.0 ) THEN
                    ! if comment or blank line, then skip
                    CYCLE
                  ELSE
                    ! otherwise, backup to beginning of line
                    BACKSPACE ( NDSL, ERR=2004, IOSTAT=IERR)
                    READ (NDSL,*,ERR=2004,IOSTAT=IERR) XX, YY, PN
                  END IF
                  IPTS = IPTS + 1
                  IF ( ILOOP .EQ. 1 ) CYCLE
                  IF ( ILOOP .EQ. 2 ) THEN
                    X(IPTS)      = XX
                    Y(IPTS)      = YY
                    PNAMES(IPTS) = PN 
                    IF ( IAPROC .EQ. NAPOUT ) THEN
                      IF ( FLAGLL ) THEN
                        IF ( IPTS .EQ. 1 ) THEN
                          WRITE (NDSO,2945)                     &
                                 FACTOR*XX, FACTOR*YY, PN
                        ELSE
                          WRITE (NDSO,2946) IPTS,               &
                                 FACTOR*XX, FACTOR*YY, PN
                        END IF
                      ELSE
                        IF ( IPTS .EQ. 1 ) THEN
                          WRITE (NDSO,2955)                     &
                                 FACTOR*XX, FACTOR*YY, PN
                        ELSE
                          WRITE (NDSO,2956) IPTS,               &
                                 FACTOR*XX, FACTOR*YY, PN
                        END IF
                      END IF
                    END IF
                  END IF ! ILOOP.EQ.2
                END DO ! end of file                      
              END DO ! ILOOP
              CLOSE(NDSL)

! Type 3: track output
            ELSE IF ( J .EQ. 3 ) THEN
              TFLAGI = NML_OUTPUT_TYPE%TRACK%FORMAT
              IF ( .NOT. TFLAGI ) NDS(11) = -NDS(11)
              IF ( IAPROC .EQ. NAPOUT ) THEN
                IF ( .NOT. TFLAGI ) THEN
                  WRITE (NDSO,3945) 'input', 'UNFORMATTED'
                ELSE
                  WRITE (NDSO,3945) 'input', 'FORMATTED'
                END IF
              END IF

! Type 6: partitioning
            ELSE IF ( J .EQ. 6 ) THEN
              IPRT(1) = NML_OUTPUT_TYPE%PARTITION%X0
              IPRT(2) = NML_OUTPUT_TYPE%PARTITION%XN
              IPRT(3) = NML_OUTPUT_TYPE%PARTITION%NX
              IPRT(4) = NML_OUTPUT_TYPE%PARTITION%Y0
              IPRT(5) = NML_OUTPUT_TYPE%PARTITION%YN
              IPRT(6) = NML_OUTPUT_TYPE%PARTITION%NY
              PRTFRM = NML_OUTPUT_TYPE%PARTITION%FORMAT
!
              IF ( IAPROC .EQ. NAPOUT ) THEN
                IF ( PRTFRM ) THEN
                  YESXNO = 'YES/--'
                ELSE
                  YESXNO = '---/NO'
                END IF
                WRITE (NDSO,6945) IPRT, YESXNO
              END IF

!/COU ! Type 7: coupling
!/COU            ELSE IF ( J .EQ. 7 ) THEN
!/COU              FLDOUT = NML_OUTPUT_TYPE%COUPLING%SENT
!/COU              CALL W3FLGRDFLAG ( NDSO, NDSO, NDSE, FLDOUT, FLG2,  &
!/COU                                 FLGR2, IAPROC, NAPOUT, IERR )
!/COU              IF ( IERR .NE. 0 ) GOTO 2222
!/COU              FLDIN = NML_OUTPUT_TYPE%COUPLING%RECEIVED
!/COU              CPLT0 = NML_OUTPUT_TYPE%COUPLING%COUPLET0

            END IF ! J
          END IF ! ODAT
        END DO ! J

        ! Extra fields to be written in the restart
        FLDRST = NML_OUTPUT_TYPE%RESTART%EXTRA
        CALL W3FLGRDFLAG ( NDSO, NDSO, NDSE, FLDRST, FLOGR,  &
                           FLOGRR, IAPROC, NAPOUT, IERR )
        IF ( IERR .NE. 0 ) GOTO 2222

        ! force minimal allocation to avoid memory seg fault
        IF ( .NOT.ALLOCATED(X) .AND. NPTS.EQ.0 ) ALLOCATE ( X(1), Y(1), PNAMES(1) )

! 2.6 Homogeneous field data

        IF ( FLHOM ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951)                   &
                          'Homogeneous field data (and moving grid) ...'

          NH(-7) = NML_HOMOG_COUNT%N_IC1
          NH(-6) = NML_HOMOG_COUNT%N_IC2
          NH(-5) = NML_HOMOG_COUNT%N_IC3
          NH(-4) = NML_HOMOG_COUNT%N_IC4
          NH(-3) = NML_HOMOG_COUNT%N_IC5
          NH(-2) = NML_HOMOG_COUNT%N_MDN
          NH(-1) = NML_HOMOG_COUNT%N_MTH
          NH(0)  = NML_HOMOG_COUNT%N_MVS
          NH(1)  = NML_HOMOG_COUNT%N_LEV
          NH(2)  = NML_HOMOG_COUNT%N_CUR
          NH(3)  = NML_HOMOG_COUNT%N_WND
          NH(4)  = NML_HOMOG_COUNT%N_ICE
          NH(8)  = NML_HOMOG_COUNT%N_MOV
!
          N_TOT = NML_HOMOG_COUNT%N_TOT
!
          DO J=JFIRST,8
            IF ( NH(J) .GT. NHMAX ) GOTO 2006
          END DO


          ! Store homogeneous fields
          IF ( N_TOT .GT. 0 ) THEN
            IHH(:)=0
            DO IH=1,N_TOT
              READ(NML_HOMOG_INPUT(IH)%NAME,*) IDTST
              SELECT CASE (IDTST)
              CASE ('IC1')
                J=-7
              CASE ('IC2')
                J=-6
              CASE ('IC3')
                J=-5
              CASE ('IC4')
                J=-4
              CASE ('IC5')
                J=-3
              CASE ('MDN')
                J=-2
              CASE ('MTH')
                J=-1
              CASE ('MVS')
                J=0
              CASE ('LEV')
                J=1
              CASE ('CUR')
                J=2
              CASE ('WND')
                J=3
              CASE ('ICE')
                J=4
              CASE ('MOV')
                J=8
              CASE DEFAULT
                GOTO 2062
              END SELECT
              IHH(J)=IHH(J)+1
              READ(NML_HOMOG_INPUT(IH)%DATE,*) THO(:,J,IHH(J))
              HA(IHH(J),J) = NML_HOMOG_INPUT(IH)%VALUE1
              HD(IHH(J),J) = NML_HOMOG_INPUT(IH)%VALUE2
              HS(IHH(J),J) = NML_HOMOG_INPUT(IH)%VALUE3
            END DO
          END IF

!/O7          DO J=JFIRST, 8
!/O7            IF ( FLH(J) .AND. IAPROC.EQ.NAPOUT ) THEN
!/O7              WRITE (NDSO,952) NH(J), IDFLDS(J)
!/O7              DO I=1, NH(J)
!/O7                IF ( ( J .LE. 1 ) .OR. ( J .EQ. 4 ) ) THEN
!/O7                  WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
!/O7                                   HA(I,J)
!/O7                ELSE IF ( ( J .EQ. 2 ) .OR. (J .EQ. 8) ) THEN
!/O7                  WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
!/O7                                   HA(I,J), HD(I,J)
!/O7                ELSE IF ( J .EQ. 3 ) THEN
!/O7                  WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
!/O7                                   HA(I,J), HD(I,J), HS(I,J)
!/O7                END IF
!/O7              END DO
!/O7            END IF
!/O7          END DO
!
          IF ( ( FLH(-7) .AND. (NH(-7).EQ.0) ) .OR.                     &
               ( FLH(-6) .AND. (NH(-6).EQ.0) ) .OR.                     &
               ( FLH(-5) .AND. (NH(-5).EQ.0) ) .OR.                     &
               ( FLH(-4) .AND. (NH(-4).EQ.0) ) .OR.                     &
               ( FLH(-3) .AND. (NH(-3).EQ.0) ) .OR.                     &
               ( FLH(-2) .AND. (NH(-2).EQ.0) ) .OR.                     &
               ( FLH(-1) .AND. (NH(-1).EQ.0) ) .OR.                     &
               ( FLH(0)  .AND. (NH(0).EQ.0)  ) .OR.                     &
               ( FLH(1)  .AND. (NH(1).EQ.0)  ) .OR.                     &
               ( FLH(2)  .AND. (NH(2).EQ.0)  ) .OR.                     &
               ( FLH(3)  .AND. (NH(3).EQ.0)  ) .OR.                     &
               ( FLH(4)  .AND. (NH(4).EQ.0)  ) .OR.                     &
               ( FLH(8) .AND. (NH(8).EQ.0) ) ) GOTO 2007
!
        END IF ! FLHOM


      END IF ! FLGNML



!
! process old ww3_shel.inp format
!
      IF (.NOT. FLGNML) THEN

!/DEBUGINIT        WRITE(740+IAPROC,*) ' FNMPRE=', TRIM(FNMPRE)
!/DEBUGINIT        FLUSH(740+IAPROC)
        OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_shel.inp',STATUS='OLD',IOSTAT=IERR)
        REWIND (NDSI)
!/DEBUGINIT        WRITE(740+IAPROC,*) 'Before read 2002, case 1'
!/DEBUGINIT        FLUSH(740+IAPROC)
!AR: I changed the error handling for err=2002, see commit message ...
        READ (NDSI,'(A)') COMSTR
!/DEBUGINIT        WRITE(740+IAPROC,*) ' COMSTR=', COMSTR
!/DEBUGINIT        WRITE(740+IAPROC,*) ' After read 2002, case 1'
!/DEBUGINIT        FLUSH(740+IAPROC)
        IF (COMSTR.EQ.' ') COMSTR = '$'
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,901) COMSTR

! 2.1 forcing flags

        DO J=JFIRST, 7
          CALL NEXTLN ( COMSTR , NDSI , NDSEN )
          IF ( J .LT. 4 ) THEN
!/DEBUGINIT      WRITE(740+IAPROC,*) 'Before read 2002, case 2'
!/DEBUGINIT      FLUSH(740+IAPROC)
            READ (NDSI,*) FLAGTFC(J), FLH(J)
!/DEBUGINIT      WRITE(740+IAPROC,*) '     J=', J, ' FLAGTFC=', FLAGTFC(J), ' FLH=', FLH(J)
!/DEBUGINIT      WRITE(740+IAPROC,*) ' After read 2002, case 2'
!/DEBUGINIT      FLUSH(740+IAPROC)
          ELSE
!/DEBUGINIT      WRITE(740+IAPROC,*) 'Before read 2002, case 3'
!/DEBUGINIT      FLUSH(740+IAPROC)
            READ (NDSI,*) FLAGTFC(J)
!/DEBUGINIT      WRITE(740+IAPROC,*) '     J=', J, ' FLAGTFC=', FLAGTFC(J)
!/DEBUGINIT      WRITE(740+IAPROC,*) ' After read 2002, case 3'
!/DEBUGINIT      FLUSH(740+IAPROC)
            FLH(J) = .FALSE.
          END IF
        END DO

        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,920)
        DO J=JFIRST, 7
          IF ( J .LT. 4 ) THEN
            IF (FLAGTFC(J).EQ.'T') THEN 
              INFLAGS1(J)=.TRUE.
              FLAGSC(J)=.FALSE.
            END IF
            IF (FLAGTFC(J).EQ.'F') THEN 
              INFLAGS1(J)=.FALSE.
              FLAGSC(J)=.FALSE.
            END IF
            IF (FLAGTFC(J).EQ.'C') THEN 
              INFLAGS1(J)=.TRUE.
              FLAGSC(J)=.TRUE.
            END IF
            FLH(J) = FLH(J) .AND. INFLAGS1(J)
          ELSE
            FLH(J) = .FALSE.
            IF (FLAGTFC(J).EQ.'T') THEN 
              INFLAGS1(J)=.TRUE.
              FLAGSC(J)=.FALSE.
            END IF
            IF (FLAGTFC(J).EQ.'F') THEN 
              INFLAGS1(J)=.FALSE.
              FLAGSC(J)=.FALSE.
            END IF
            IF (FLAGTFC(J).EQ.'C') THEN 
              INFLAGS1(J)=.TRUE.
              FLAGSC(J)=.TRUE.
            END IF
          END IF
          IF ( INFLAGS1(J) ) THEN
            YESXNO = 'YES/--'
          ELSE
            YESXNO = '---/NO'
          END IF
          IF ( FLH(J) ) THEN
            STRNG  = '(homogeneous field) '
          ELSE IF ( FLAGSC(J) ) THEN
            STRNG  = '(coupling field) '
          ELSE
            STRNG  = '                    '
          END IF
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,921) IDFLDS(J), YESXNO, STRNG
        END DO
!/COU          IF (FLAGSC(1) .AND. INFLAGS1(2) .AND. .NOT. FLAGSC(2)) GOTO 2102
!/COU          IF (FLAGSC(2) .AND. INFLAGS1(1) .AND. .NOT. FLAGSC(1)) GOTO 2102

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 2b'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!/DEBUGINIT      WRITE(740+IAPROC,*) 'ww3_shel, step 5'
!/DEBUGINIT      FLUSH(740+IAPROC)
!
        INFLAGS1(8) = .FALSE.
        FLH(8)   = .FALSE.
!/MGW        INFLAGS1(8) = .TRUE.
!/MGP        INFLAGS1(8) = .TRUE.
!/MGW        FLH(8)   = .TRUE.
!/MGP        FLH(8)   = .TRUE.
        IF ( INFLAGS1(8) .AND. IAPROC.EQ.NAPOUT )                          &
             WRITE (NDSO,921) IDFLDS(8), 'YES/--', ' '
!
        FLFLG  = INFLAGS1(-7) .OR. INFLAGS1(-6) .OR. INFLAGS1(-5) .OR. INFLAGS1(-4) &
                 .OR. INFLAGS1(-3) .OR. INFLAGS1(-2) .OR. INFLAGS1(-1)           &
                 .OR. INFLAGS1(0)  .OR. INFLAGS1(1)  .OR. INFLAGS1(2)            &
                 .OR. INFLAGS1(3)  .OR. INFLAGS1(4)  .OR. INFLAGS1(5)            &
                 .OR. INFLAGS1(6)  .OR. INFLAGS1(7)
        FLHOM  = FLH(-7) .OR. FLH(-6) .OR. FLH(-5) .OR. FLH(-4)       &
                 .OR. FLH(-3) .OR. FLH(-2) .OR. FLH(-1) .OR. FLH(0)   &
                 .OR. FLH(1) .OR. FLH(2) .OR. FLH(3) .OR. FLH(4) .OR. FLH(8)
!
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,922)
!
!       INFLAGS2 is just "initial value of INFLAGS1", i.e. does *not* get
!          changed when model reads last record of ice.ww3
        INFLAGS2=INFLAGS1

!/T        WRITE (NDST,9020) FLFLG, INFLAGS1, FLHOM, FLH


! 2.2 Time setup

        CALL NEXTLN ( COMSTR , NDSI , NDSEN )
!/DEBUGINIT      write(740+IAPROC,*), 'Before read 2002, case 4'
        READ (NDSI,*) TIME0
!/DEBUGINIT      write(740+IAPROC,*), ' After read 2002, case 4'

!/NETCDF_QAD      TIME0_NETCDF_QAD = TIME0

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 2c'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

        CALL NEXTLN ( COMSTR , NDSI , NDSEN )
!/DEBUGINIT      write(740+IAPROC,*), 'Before read 2002, case 5'
        READ (NDSI,*) TIMEN
!/DEBUGINIT      write(740+IAPROC,*), ' After read 2002, case 5'
!/NETCDF_QAD      TIMEN_NETCDF_QAD = TIMEN

!/DEBUGINIT      write(740+IAPROC,*), 'ww3_shel, step 6'
!
!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 2d'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

! 2.3 Domain setup

!/DEBUGINIT      write(740+IAPROC,*), 'ww3_shel, step 7'
        CALL NEXTLN ( COMSTR , NDSI , NDSEN )
!/DEBUGINIT      write(740+IAPROC,*), 'Before read 2002, case 6'
        READ (NDSI,*) IOSTYP
!/DEBUGINIT      write(740+IAPROC,*), ' After read 2002, case 6'
        CALL W3IOGR ( 'GRID', NDSF(5) )
        IF ( FLAGLL ) THEN
          FACTOR = 1.
        ELSE
          FACTOR = 1.E-3
        END IF

!/DEBUGINIT      write(740+IAPROC,*), 'ww3_shel, step 8'

! 2.4 Output dates

        NPTS   = 0
        NOTYPE = 6
!/COU        NOTYPE = 7
!/DEBUGINIT      write(740+IAPROC,*), 'Before NOTYPE loop'
        DO J = 1, NOTYPE
!/DEBUGINIT        write(740+IAPROC,*), 'J=', J, '/ NOTYPE=', NOTYPE
          CALL NEXTLN ( COMSTR , NDSI , NDSEN )
!/DEBUGINIT        write(740+IAPROC,*), 'Before read 2002, case 7'
!
! CHECKPOINT
        IF(J .EQ. 4) THEN
          ODAT(38)=0 
          WORDS(1:7)=''
          READ (NDSI,'(A)') LINEIN
          READ(LINEIN,*,iostat=ierr) WORDS
          READ(WORDS( 1 ), * ) ODAT(16)
          READ(WORDS( 2 ), * ) ODAT(17)
          READ(WORDS( 3 ), * ) ODAT(18)
          READ(WORDS( 4 ), * ) ODAT(19)
          READ(WORDS( 5 ), * ) ODAT(20)
          IF (WORDS(6) .EQ. 'T') THEN
            CALL NEXTLN ( COMSTR , NDSI , NDSEN )
            READ (NDSI,*,END=2001,ERR=2002)(ODAT(I),I=5*(8-1)+1,5*8)
            WRITE(*,*)(ODAT(I),I=5*(8-1)+1,5*8)
          END IF
          IF (WORDS(7) .EQ. 'T') THEN
            CALL NEXTLN ( COMSTR , NDSI , NDSEN )
            READ (NDSI,*,END=2001,ERR=2002) FLDRST
          END IF
          CALL W3FLGRDFLAG ( NDSO, NDSO, NDSE, FLDRST, FLOGR,  &
                             FLOGRR, IAPROC, NAPOUT, IERR )
          IF ( IERR .NE. 0 ) GOTO 2222
        ELSE
!
!INLINE NEW VARIABLE TO READ IF PRESENT OFILES(J), IF NOT ==0
!          READ (NDSI,*) (ODAT(I),I=5*(J-1)+1,5*J)
!          READ (NDSI,*,IOSTAT=IERR) (ODAT(I),I=5*(J-1)+1,5*J),OFILES(J)
        IF(J .LE. 2) THEN
          WORDS(1:6)=''
!          READ (NDSI,*,END=2001,ERR=2002)(ODAT(I),I=5*(J-1)+1,5*J),OFILES(J)
          READ (NDSI,'(A)') LINEIN
          READ(LINEIN,*,iostat=ierr) WORDS
!
          IF(J .EQ. 1) THEN
            READ(WORDS( 1 ), * ) ODAT(1)
            READ(WORDS( 2 ), * ) ODAT(2)
            READ(WORDS( 3 ), * ) ODAT(3)
            READ(WORDS( 4 ), * ) ODAT(4)
            READ(WORDS( 5 ), * ) ODAT(5)
          ELSE
            READ(WORDS( 1 ), * ) ODAT(6)
            READ(WORDS( 2 ), * ) ODAT(7)
            READ(WORDS( 3 ), * ) ODAT(8)
            READ(WORDS( 4 ), * ) ODAT(9)
            READ(WORDS( 5 ), * ) ODAT(10)
          END IF

          IF (WORDS(6) .NE. '0' .AND. WORDS(6) .NE. '1') THEN
            OFILES(J)=0
          ELSE
            READ(WORDS( 6 ), * ) OFILES(J)
          END IF


!/COU        ELSE IF(J .EQ. 7) THEN
!/COU          WORDS(1:6)=''
!/COU          READ (NDSI,'(A)') LINEIN
!/COU          READ(LINEIN,*,iostat=ierr) WORDS
!/COU 
!/COU          READ(WORDS( 1 ), * ) ODAT(31)
!/COU          READ(WORDS( 2 ), * ) ODAT(32)
!/COU          READ(WORDS( 3 ), * ) ODAT(33)
!/COU          READ(WORDS( 4 ), * ) ODAT(34)
!/COU          READ(WORDS( 5 ), * ) ODAT(35)
!/COU 
!/COU          IF (WORDS(6) .EQ. 'T') THEN
!/COU            CPLT0 = .TRUE.
!/COU          ELSE
!/COU            CPLT0 = .FALSE.
!/COU          END IF
        ELSE
          OFILES(J)=0
          READ (NDSI,*,END=2001,ERR=2002)(ODAT(I),I=5*(J-1)+1,5*J)
        END IF
!          WRITE(*,*) 'OFILES(J)= ', OFILES(J),J
!
!/DEBUGINIT        write(740+IAPROC,*), ' After read 2002, case 7'
          ODAT(5*(J-1)+3) = MAX ( 0 , ODAT(5*(J-1)+3) )
!
!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL NOTTYPE', J
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)
!

! 2.5 Output types

          IF ( ODAT(5*(J-1)+3) .NE. 0 ) THEN

! Type 1: fields of mean wave parameters
!/DEBUGINIT            write(740+IAPROC,*), 'Case analysis'
            IF ( J .EQ. 1 ) THEN
              CALL W3READFLGRD ( NDSI, NDSO, 9, NDSEN, COMSTR, FLGD,   &
                                 FLGRD, IAPROC, NAPOUT, IERR )
              IF ( IERR .NE. 0 ) GOTO 2222



! Type 2: point output
            ELSE IF ( J .EQ. 2 ) THEN
              DO ILOOP=1,2
                IF ( ILOOP .EQ. 1 ) THEN
                  NDSI2  = NDSI
                  IF ( IAPROC .EQ. 1 ) OPEN                       &
                       (NDSS,FILE=TRIM(FNMPRE)//'ww3_shel.scratch')
                ELSE
                  NDSI2  = NDSS
!/MPI                  CALL MPI_BARRIER (MPI_COMM,IERR_MPI)
                  OPEN (NDSS,FILE=TRIM(FNMPRE)//'ww3_shel.scratch')
                  REWIND (NDSS)
!
                  IF ( .NOT.ALLOCATED(X) ) THEN
                    IF ( NPTS.GT.0 ) THEN
                      ALLOCATE ( X(NPTS), Y(NPTS), PNAMES(NPTS) )
                    ELSE
                      ALLOCATE ( X(1), Y(1), PNAMES(1) )
                      GOTO 2054 
                    END IF
                  END IF
                END IF
!
                NPTS   = 0
                DO
                  CALL NEXTLN ( COMSTR , NDSI , NDSEN )
!/DEBUGINIT                  write(740+IAPROC,*), 'Before read 2002, case 8'
                  READ (NDSI2,*) XX, YY, PN
!/DEBUGINIT                  write(740+IAPROC,*), ' After read 2002, case 8'
                  IF ( ILOOP.EQ.1 .AND. IAPROC.EQ.1 ) THEN
                    BACKSPACE (NDSI)
                    READ (NDSI,'(A)') LINE
                    WRITE (NDSS,'(A)') LINE
                  END IF
                  IF ( INDEX(PN,"STOPSTRING").NE.0 ) EXIT
                  NPTS   = NPTS + 1
                  IF ( ILOOP .EQ. 1 ) CYCLE
                  X(NPTS)      = XX
                  Y(NPTS)      = YY
                  PNAMES(NPTS) = PN
                  IF ( IAPROC .EQ. NAPOUT ) THEN
                    IF ( FLAGLL ) THEN
                      IF ( NPTS .EQ. 1 ) THEN
                        WRITE (NDSO,2945)                     &
                               FACTOR*XX, FACTOR*YY, PN
                      ELSE
                        WRITE (NDSO,2946) NPTS,               &
                               FACTOR*XX, FACTOR*YY, PN
                      END IF
                    ELSE
                      IF ( NPTS .EQ. 1 ) THEN
                        WRITE (NDSO,2955)                     &
                               FACTOR*XX, FACTOR*YY, PN
                      ELSE
                        WRITE (NDSO,2956) NPTS,               &
                               FACTOR*XX, FACTOR*YY, PN
                      END IF
                    END IF
                  END IF
                END DO
!
                IF ( IAPROC.EQ.1 .AND. ILOOP.EQ.1 ) CLOSE (NDSS)
              END DO
!
              IF ( NPTS.EQ.0 .AND. IAPROC.EQ.NAPOUT )               &
                   WRITE (NDSO,2947)
              IF ( IAPROC .EQ. 1 ) THEN
!/MPI                CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
                CLOSE (NDSS,STATUS='DELETE')
              ELSE
                CLOSE (NDSS)
!/MPI                CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
              END IF
!


! Type 3: track output
            ELSE IF ( J .EQ. 3 ) THEN
              CALL NEXTLN ( COMSTR , NDSI , NDSEN )
!/DEBUGINIT              write(740+IAPROC,*), 'Before read 2002, case 9'
              READ (NDSI,*) TFLAGI
!/DEBUGINIT              write(740+IAPROC,*), ' After read 2002, case 9'
!
              IF ( .NOT. TFLAGI ) NDS(11) = -NDS(11)
              IF ( IAPROC .EQ. NAPOUT ) THEN
                IF ( .NOT. TFLAGI ) THEN
                  WRITE (NDSO,3945) 'input', 'UNFORMATTED'
                ELSE
                  WRITE (NDSO,3945) 'input', 'FORMATTED'
                END IF
              END IF


! Type 6: partitioning
            ELSE IF ( J .EQ. 6 ) THEN
!             IPRT: IX0, IXN, IXS, IY0, IYN, IYS
              CALL NEXTLN ( COMSTR , NDSI , NDSEN )
!/DEBUGINIT              write(740+IAPROC,*), 'Before reading IPRT'
!/DEBUGINIT              write(740+IAPROC,*), 'Before read 2002, case 10'
              READ (NDSI,*) IPRT, PRTFRM
!/DEBUGINIT              write(740+IAPROC,*), ' After read 2002, case 10'
!
              IF ( IAPROC .EQ. NAPOUT ) THEN
                IF ( PRTFRM ) THEN
                  YESXNO = 'YES/--'
                ELSE
                  YESXNO = '---/NO'
                END IF
                WRITE (NDSO,6945) IPRT, YESXNO
              END IF


!/COU ! Type 7: coupling
!/COU            ELSE IF ( J .EQ. 7 ) THEN
!/COU              CALL W3READFLGRD ( NDSI, NDSO, NDSS, NDSEN, COMSTR, FLG2,     &
!/COU                                 FLGR2, IAPROC, NAPOUT, IERR )
!/COU              IF ( IERR .NE. 0 ) GOTO 2222
!/COU              CALL NEXTLN ( COMSTR , NDSI , NDSEN )
!/COU              READ (NDSI,'(A)',END=2001,ERR=2002,IOSTAT=IERR) FLDIN

            END IF ! J
          END IF ! ODAT
        END IF ! IF J=4
        END DO ! J

        ! force minimal allocation to avoid memory seg fault
        IF ( .NOT.ALLOCATED(X) .AND. NPTS.EQ.0 ) ALLOCATE ( X(1), Y(1), PNAMES(1) )

! 2.6 Homogeneous field data

        IF ( FLHOM ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951)                  &
                          'Homogeneous field data (and moving grid) ...'
          NH     = 0
!
          ! Start of loop
          DO
            CALL NEXTLN ( COMSTR , NDSI , NDSEN )
!/DEBUGINIT            write(740+IAPROC,*), 'Before read 2002, case 11'
            READ (NDSI,*) IDTST
!/DEBUGINIT            write(740+IAPROC,*), ' After read 2002, case 11'


            ! Exit if illegal id
            IF ( IDTST.NE.IDSTR(-7) .AND. IDTST.NE.IDSTR(-6) .AND.   &
                 IDTST.NE.IDSTR(-5) .AND. IDTST.NE.IDSTR(-4) .AND.   &
                 IDTST.NE.IDSTR(-3) .AND. IDTST.NE.IDSTR(-2) .AND.   &
                 IDTST.NE.IDSTR(-1) .AND. IDTST.NE.IDSTR(0)  .AND.   &
                 IDTST.NE.IDSTR(1)  .AND. IDTST.NE.IDSTR(2)  .AND.   &
                 IDTST.NE.IDSTR(3)  .AND. IDTST.NE.IDSTR(4)  .AND.   &
                 IDTST.NE.IDSTR(8)  .AND. IDTST.NE.'STP' ) GOTO 2005

            ! Stop conditions
            IF ( IDTST .EQ. 'STP' ) THEN
              EXIT
            ELSE
              BACKSPACE ( NDSI )
            END IF

            ! Store data
            DO J=LBOUND(IDSTR,1), 8
              IF ( IDTST .EQ. IDSTR(J) ) THEN
                NH(J)    = NH(J) + 1
                IF ( NH(J) .GT. NHMAX ) GOTO 2006
                IF ( J .LE. 1  ) THEN ! water levels, etc. : get HA
!/DEBUGINIT                     write(740+IAPROC,*), 'Before read 2002, case 12'
                  READ (NDSI,*) IDTST,           &
                        THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                        HA(NH(J),J)
!/DEBUGINIT                     write(740+IAPROC,*), ' After read 2002, case 12'
                ELSE IF ( J .EQ. 2 ) THEN ! currents: get HA and HD
!/DEBUGINIT                     write(740+IAPROC,*), 'Before read 2002, case 13'
                  READ (NDSI,*) IDTST,           &
                        THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                        HA(NH(J),J), HD(NH(J),J)
!/DEBUGINIT                     write(740+IAPROC,*), ' After read 2002, case 13'
                ELSE IF ( J .EQ. 3 ) THEN ! wind: get HA HD and HS
!/DEBUGINIT                     write(740+IAPROC,*), 'Before read 2002, case 14'
                  READ (NDSI,*) IDTST,           &
                        THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                        HA(NH(J),J), HD(NH(J),J), HS(NH(J),J)
!/DEBUGINIT                     write(740+IAPROC,*), ' After read 2002, case 14'
                ELSE IF ( J .EQ. 4 ) THEN ! ice
!/DEBUGINIT                     write(740+IAPROC,*), 'Before read 2002, case 15'
                  READ (NDSI,*) IDTST,           &
                        THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                        HA(NH(J),J)
!/DEBUGINIT                     write(740+IAPROC,*), ' After read 2002, case 15'
                ELSE IF ( J .EQ. 8 ) THEN ! mov: HA and HD
!/DEBUGINIT                     write(740+IAPROC,*), 'Before read 2002, case 16'
                  READ (NDSI,*) IDTST,           &
                        THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                        HA(NH(J),J), HD(NH(J),J)
!/DEBUGINIT                     write(740+IAPROC,*), ' After read 2002, case 16'
                END IF
              END IF
            END DO
          END DO

!/MEMCHECK      write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 3'
!/MEMCHECK      call getMallocInfo(mallinfos)
!/MEMCHECK      call printMallInfo(IAPROC,mallInfos)
!

!/O7          DO J=JFIRST, 8
!/O7            IF ( FLH(J) .AND. IAPROC.EQ.NAPOUT ) THEN
!/O7              WRITE (NDSO,952) NH(J), IDFLDS(J)
!/O7              DO I=1, NH(J)
!/O7                IF ( ( J .LE. 1 ) .OR. ( J .EQ. 4 ) ) THEN
!/O7                  WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
!/O7                                   HA(I,J)
!/O7                ELSE IF ( ( J .EQ. 2 ) .OR. (J .EQ. 8) ) THEN
!/O7                  WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
!/O7                                   HA(I,J), HD(I,J)
!/O7                ELSE IF ( J .EQ. 3 ) THEN
!/O7                  WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
!/O7                                   HA(I,J), HD(I,J), HS(I,J)
!/O7                END IF
!/O7              END DO
!/O7            END IF
!/O7          END DO
!
!
          IF ( ( FLH(-7) .AND. (NH(-7).EQ.0) ) .OR.                     &
               ( FLH(-6) .AND. (NH(-6).EQ.0) ) .OR.                     &
               ( FLH(-5) .AND. (NH(-5).EQ.0) ) .OR.                     &
               ( FLH(-4) .AND. (NH(-4).EQ.0) ) .OR.                     &
               ( FLH(-3) .AND. (NH(-3).EQ.0) ) .OR.                     &
               ( FLH(-2) .AND. (NH(-2).EQ.0) ) .OR.                     &
               ( FLH(-1) .AND. (NH(-1).EQ.0) ) .OR.                     &
               ( FLH(0)  .AND. (NH(0).EQ.0)  ) .OR.                     &
               ( FLH(1)  .AND. (NH(1).EQ.0)  ) .OR.                     &
               ( FLH(2)  .AND. (NH(2).EQ.0)  ) .OR.                     &
               ( FLH(3)  .AND. (NH(3).EQ.0)  ) .OR.                     &
               ( FLH(4)  .AND. (NH(4).EQ.0)  ) .OR.                     &
               ( FLH(8) .AND. (NH(8).EQ.0) ) ) GOTO 2007
!
        END IF ! FLHOM

      END IF





!
! ----------------
!

! 2.1 input fields

! 2.1.a Opening field and data files

      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,950)
      IF ( FLFLG ) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951)                  &
                                         'Preparing input files ...'
!

        DO J=JFIRST, 4
!/DEBUGINIT     write(740+IAPROC,*), 'J=',J,'INFLAGS1(J)=',INFLAGS1(J), 'FLAGSC(J)=', FLAGSC(J) 
          IF ( INFLAGS1(J) .AND. .NOT. FLAGSC(J)) THEN
            IF ( FLH(J) ) THEN
              IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,954) IDFLDS(J)
            ELSE
              FLAGTIDE = 0
              CALL W3FLDO ('READ', IDSTR(J), NDSF(J), NDST,     &
                            NDSEN, NX, NY, GTYPE,               &
                            IERR, FPRE=TRIM(FNMPRE), TIDEFLAGIN=FLAGTIDE )
              IF ( IERR .NE. 0 ) GOTO 2222
!/TIDE              IF (FLAGTIDE.GT.0.AND.J.EQ.1) FLAGSTIDE(1)=.TRUE.
!/TIDE              IF (FLAGTIDE.GT.0.AND.J.EQ.2) FLAGSTIDE(2)=.TRUE.
              IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,955) IDFLDS(J)
            END IF
          ELSE
            IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,954) IDFLDS(J)
          END IF
        END DO
!
        DO J=5, 7
          IF ( INFLAGS1(J) .AND. .NOT. FLAGSC(J)) THEN
            CALL W3FLDO ('READ', IDSTR(J), NDSF(J), NDST, NDSEN, &
                         RCLD(J), NY, NODATA(J),                 &
                         IERR, FPRE=TRIM(FNMPRE) )
            IF ( IERR .NE. 0 ) GOTO 2222
            IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,956) IDFLDS(J),&
                                             RCLD(J), NODATA(J)
          ELSE
            IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,954) IDFLDS(J)
          END IF
        END DO
!
      END IF ! FLFLG


!/MEMCHECK      write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 4'
!/MEMCHECK      call getMallocInfo(mallinfos)
!/MEMCHECK      call printMallInfo(IAPROC,mallInfos)


! 2.2 Time setup

      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,930)
      CALL STME21 ( TIME0 , DTME21 )
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,931) DTME21
      TIME = TIME0
      CALL STME21 ( TIMEN , DTME21 )
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,932) DTME21
!/OASIS      TIME00 = TIME0
!/OASIS      TIMEEND = TIMEN
!/COAWST      TIMEEND = TIMEN
!
      DTTST  = DSEC21 ( TIME0 , TIMEN )
      IF ( DTTST .LE. 0. ) GOTO 2003


! 2.3 Domain setup

      IOSTYP = MAX ( 0 , MIN ( 3 , IOSTYP ) )
      IF ( IAPROC .EQ. NAPOUT ) THEN
        IF ( IOSTYP .EQ. 0 ) THEN
          WRITE (NDSO,940) 'No dedicated output process, ' //   &
                           'parallel file system required.'
        ELSE IF ( IOSTYP .EQ. 1 ) THEN
          WRITE (NDSO,940) 'No dedicated output process, ' //   &
                           'any file system.'
        ELSE IF ( IOSTYP .EQ. 2 ) THEN
          WRITE (NDSO,940) 'Single dedicated output process.'
        ELSE IF ( IOSTYP .EQ. 3 ) THEN
          WRITE (NDSO,940) 'Multiple dedicated output processes.'
        ELSE
          WRITE (NDSO,940) 'IOSTYP NOT RECOGNIZED'
        END IF
      END IF


! 2.4 Output dates

      DO J = 1, NOTYPE
!
        IF ( ODAT(5*(J-1)+3) .NE. 0 ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,941) J, IDOTYP(J)
          TTIME(1) = ODAT(5*(J-1)+1)
          TTIME(2) = ODAT(5*(J-1)+2)
          CALL STME21 ( TTIME , DTME21 )
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,942) DTME21
          TTIME(1) = ODAT(5*(J-1)+4)
          TTIME(2) = ODAT(5*(J-1)+5)
          CALL STME21 ( TTIME , DTME21 )
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) DTME21
          TTIME(1) = 0
          TTIME(2) = 0
          DTTST    = REAL ( ODAT(5*(J-1)+3) )
!jcw if J=1, then DTTST above is the output time step for mean wave field params.
!/COAWST             IF (J.EQ.1) THEN
!/COAWST               DTTST=MIN(DTTST,REAL(TI_OCN2WAV))
!/COAWST             END IF
!/COAWST             IF (J.EQ.1) THEN
!/COAWST               DTTST=MIN(DTTST,REAL(TI_ATM2WAV))
!/COAWST             END IF
!
          CALL TICK21 ( TTIME , DTTST  )
          CALL STME21 ( TTIME , DTME21 )
          IF ( ( ODAT(5*(J-1)+1) .NE. ODAT(5*(J-1)+4) .OR.          &
                 ODAT(5*(J-1)+2) .NE. ODAT(5*(J-1)+5) ) .AND.       &
                 IAPROC .EQ. NAPOUT ) THEN
            IF ( DTME21(9:9) .NE. '0' ) THEN
              WRITE (NDSO,1944) DTME21( 9:19)
            ELSE IF ( DTME21(10:10) .NE. '0' ) THEN
              WRITE (NDSO,2944) DTME21(10:19)
            ELSE
              WRITE (NDSO,3944) DTME21(12:19)
            END IF
          END IF
        END IF
      END DO
!
! CHECKPOINT
      J=8
      IF (ODAT(38) .NE. 0) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,941) J, IDOTYP(J)
        TTIME(1) = ODAT(5*(J-1)+1)
        TTIME(2) = ODAT(5*(J-1)+2)
        CALL STME21 ( TTIME , DTME21 )
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,942) DTME21
        TTIME(1) = ODAT(5*(J-1)+4)
        TTIME(2) = ODAT(5*(J-1)+5)
        CALL STME21 ( TTIME , DTME21 )
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) DTME21
        TTIME(1) = 0
        TTIME(2) = 0
        DTTST    = REAL ( ODAT(5*(J-1)+3) )
        CALL TICK21 ( TTIME , DTTST  )
        CALL STME21 ( TTIME , DTME21 )
        IF ( ( ODAT(5*(J-1)+1) .NE. ODAT(5*(J-1)+4) .OR.          &
               ODAT(5*(J-1)+2) .NE. ODAT(5*(J-1)+5) ) .AND.       &
               IAPROC .EQ. NAPOUT ) THEN
          IF ( DTME21(9:9) .NE. '0' ) THEN
            WRITE (NDSO,1944) DTME21( 9:19)
          ELSE IF ( DTME21(10:10) .NE. '0' ) THEN
            WRITE (NDSO,2944) DTME21(10:19)
          ELSE
            WRITE (NDSO,3944) DTME21(12:19)
          END IF
        END IF
      END IF
!
! 2.5 Output types

!/T      WRITE (NDST,9040) ODAT
!/T      WRITE (NDST,9041) FLGRD
!/T      WRITE (NDST,9042) IPRT, PRTFRM

!
! For outputs with non-zero time step, check dates :
! If output ends before run start OR output starts after run end,
! deactivate output cleanly with output time step = 0
! This is usefull for IOSTYP=3 (Multiple dedicated output processes)
! to avoid the definition of dedicated proc. for unused output.
!
      DO J = 1, NOTYPE
        DTTST  = DSEC21 ( TIME0 , ODAT(5*(J-1)+4:5*(J-1)+5) )
        IF ( DTTST .LT. 0 ) THEN
          ODAT(5*(J-1)+3) = 0
          IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,8945) TRIM(IDOTYP(J))
          CONTINUE
        END IF
        DTTST  = DSEC21 ( ODAT(5*(J-1)+1:5*(J-1)+2), TIMEN )
        IF ( DTTST .LT. 0 ) THEN
          ODAT(5*(J-1)+3) = 0
          IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,8945) TRIM(IDOTYP(J))
          CONTINUE
        END IF
      END DO
!
! CHECKPOINT
      J = 8
      DTTST  = DSEC21 ( TIME0 , ODAT(5*(J-1)+4:5*(J-1)+5) )
      IF ( DTTST .LT. 0 ) THEN
        ODAT(5*(J-1)+3) = 0
        IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,8945) TRIM(IDOTYP(J))
        CONTINUE
      END IF
      DTTST  = DSEC21 ( ODAT(5*(J-1)+1:5*(J-1)+2), TIMEN )
      IF ( DTTST .LT. 0 ) THEN
        ODAT(5*(J-1)+3) = 0
        IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,8945) TRIM(IDOTYP(J))
        CONTINUE
      END IF
!
!/MEMCHECK      write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 5'
!/MEMCHECK      call getMallocInfo(mallinfos)
!/MEMCHECK      call printMallInfo(IAPROC,mallInfos)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 5.  Initializations
!

      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951) 'Wave model ...'
!
!/TIDE      IF (FLAGSTIDE(1).OR.FLAGSTIDE(2)) THEN 
!/TIDE        CALL VUF_SET_PARAMETERS
!/TIDE        IF (FLAGSTIDE(1)) CALL W3FLDTIDE1 ( 'READ',  NDSF(1), NDST, NDSEN, NX, NY, IDSTR(1), IERR )
!/TIDE        IF (FLAGSTIDE(2)) CALL W3FLDTIDE1 ( 'READ',  NDSF(2), NDST, NDSEN, NX, NY, IDSTR(2), IERR )
!/TIDE      END IF
! 
!/COU      ! Sent coupled fields must be written in the restart when coupling at T+0
!/COU      IF (CPLT0) THEN
!/COU        DO J=1, NOGRP
!/COU          FLOGR(J)  = FLOGR(J)  .OR. FLG2(J)
!/COU          DO I=1, NGRPP
!/COU            FLOGRR(J,I) = FLOGRR(J,I) .OR. FLGR2(J,I)
!/COU          END DO
!/COU        END DO
!/COU      ENDIF
!
      OARST = ANY(FLOGR)
!
     CALL W3INIT ( 1, .FALSE., 'ww3', NDS, NTRACE, ODAT, FLGRD, FLGR2, FLGD,    &
                   FLG2, NPTS, X, Y, PNAMES, IPRT, PRTFRM, MPI_COMM,   &
                   FLAGSTIDEIN=FLAGSTIDE )
!
!      IF (MINVAL(VA) .LT. 0.) THEN
!        WRITE(740+IAPROC,*) 'NEGATIVE ACTION SHELL 5', MINVAL(VA)
!        CALL FLUSH(740+IAPROC)
!        CALL EXTCDE(665)
!      ENDIF
!      IF (SUM(VA) .NE. SUM(VA)) THEN
!        WRITE(740+IAPROC,*) 'NAN in ACTION SHEL1', SUM(VA)
!        CALL FLUSH(740+IAPROC)
!        CALL EXTCDE(666)
!      ENDIF


!/MEMCHECK      write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 5'
!/MEMCHECK      call getMallocInfo(mallinfos)
!/MEMCHECK      call printMallInfo(IAPROC,mallInfos)
!
!/TIDE      IF (FLAGSTIDE(1)) CALL W3FLDTIDE2 ( 'READ',  NDSF(1), NDST, NDSEN, NX, NY, IDSTR(1), 1, IERR )
!/TIDE      IF (FLAGSTIDE(2)) CALL W3FLDTIDE2 ( 'READ',  NDSF(2), NDST, NDSEN, NX, NY, IDSTR(2), 1, IERR )
!/TIDE      ALLOCATE(V_ARG(170,1),F_ARG(170,1),U_ARG(170,1))  ! to be removed later ...
!
      ALLOCATE ( XXX(NX,NY) )
!

!
!/MPI      CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
!
      IF ( IAPROC .EQ. NAPOUT ) THEN
!/F90        CALL DATE_AND_TIME ( VALUES=CLKDT2 )
      END IF
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!/OASIS ! Initialize L_MASTER, COUPL_COMM
!/OASIS      IF ( IAPROC .EQ. 1) THEN
!/OASIS        L_MASTER = .TRUE.
!/OASIS      ELSE
!/OASIS        L_MASTER = .FALSE.
!/OASIS      ENDIF 
!/OASIS ! Estimate the weights for the spatial interpolation
!/OASIS      IF (DTOUT(7).NE.0) THEN
!/OASIS        CALL CPL_OASIS_GRID(L_MASTER,MPI_COMM)
!/OASIS        CALL CPL_OASIS_DEFINE(NDSO, FLDIN, FLDOUT)
!/OASIS      END IF

!jcw
!/COAWST      CALL INIT_WVCP (1)
!/COAWST      CALL INITIALIZE_WAV_ROUTERS
!/COAWST      ccount=0
!/COAWST      CALL COAWST_CPL (ccount)

!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 6.  Model without input
!
!      IF (MINVAL(VA) .LT. 0.) THEN
!        WRITE(740+IAPROC,*) 'NEGATIVE ACTION SHELL 6', MINVAL(VA)
!        CALL FLUSH(740+IAPROC)
!        CALL EXTCDE(665)
!      ENDIF
!      IF (SUM(VA) .NE. SUM(VA)) THEN
!        WRITE(740+IAPROC,*) 'NAN in ACTION SHEL2', SUM(VA)
!        CALL FLUSH(740+IAPROC)
!        CALL EXTCDE(666)
!      ENDIF

!/MEMCHECK      write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 6'
!/MEMCHECK      call getMallocInfo(mallinfos)
!/MEMCHECK      call printMallInfo(IAPROC,mallInfos)

      IF ( .NOT. FLFLG ) THEN
!
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,960)
        CALL W3WAVE ( 1, ODAT, TIMEN                      &
!/OASIS                     , .TRUE., .FALSE., MPI_COMM, TIMEN     &
                    )
!
        GOTO 2222
!
      END IF
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 7.  Model with input
!
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,970)
!
!/NCC      CFLAG = .FALSE. 
!/NCC      CALL WW_INIT(1,GRIDS(1)%NX,GRIDS(1)%NY,            &
!/NCC      GRIDS(1)%X0,GRIDS(1)%Y0,GRIDS(1)%SX,GRIDS(1)%SY)
!/NCC      CALL WW_RECVDTC
!/NCC      CALL WW_SENDGRIDS
!/NCC      CALL WW_SENDSLM(GRIDS(1)%MAPSTA)
!/NCC      IF ( IAPROC .EQ. NAPOUT .AND. DEBUG_NCC ) THEN
!/NCC         OPEN(9751,FILE='debug_writeout_wind',           &
!/NCC              FORM='UNFORMATTED',STATUS='UNKNOWN')
!/NCC         OPEN(9752,FILE='debug_writeout_ice',            &
!/NCC              FORM='UNFORMATTED',STATUS='UNKNOWN')
!/NCC         OPEN(9753,FILE='data_forDC',                    &
!/NCC              FORM='UNFORMATTED',STATUS='UNKNOWN')
!/NCC         WRITE(9753) GRIDS(1)%NX,GRIDS(1)%NY,            &
!/NCC           GRIDS(1)%X0,GRIDS(1)%Y0,GRIDS(1)%SX,GRIDS(1)%SY
!/NCC         WRITE(9753) GRIDS(1)%MAPSTA
!/NCC      END IF

!/OASIS      ! Send coupling fields at the initial time step
!/OASIS      IF ( FLOUT(7) .AND. CPLT0 ) THEN
!/OASACM       CALL SND_FIELDS_TO_ATMOS()
!/OASOCM       CALL SND_FIELDS_TO_OCEAN()
!/OASICM       CALL SND_FIELDS_TO_ICE()
!/OASIS      END IF

  700 CONTINUE
!
!/NCC      IF ( IAPROC .EQ. NAPOUT .AND. DEBUG_NCC ) WRITE(9753) TIME0
!/NCC      CALL WW_TSTEP_INIT(TIME0)
!/NCC      CALL WW_RECV_SBC
!/NCC      CALL WW_RECV_PSEUDOICE
!
! 7.a Determine next time interval and input fields
! 7.a.1 Preparation
!
      TTIME  = TIMEN
!
      CALL STME21 ( TIME0 , DTME21 )
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,971) DTME21
!
!/T      WRITE (NDST,9070) '0-N', TIME0, TTIME,           &
!/T                        IDSTR(-7), INFLAGS1(-7), TI1,     &
!/T                        IDSTR(-6), INFLAGS1(-6), TI2,     &
!/T                        IDSTR(-5), INFLAGS1(-5), TI3,     &
!/T                        IDSTR(-4), INFLAGS1(-4), TI4,     &
!/T                        IDSTR(-3), INFLAGS1(-3), TI5,     &
!/T                        IDSTR(-2), INFLAGS1(-2), TZN,     &
!/T                        IDSTR(-1), INFLAGS1(-1), TTN,     &
!/T                        IDSTR(0), INFLAGS1(0), TVN,       &
!/T                        IDSTR(1), INFLAGS1(1), TLN,       &
!/T                        IDSTR(2), INFLAGS1(2), TC0, TCN,  &
!/T                        IDSTR(3), INFLAGS1(3), TW0, TWN,  &
!/T                        IDSTR(4), INFLAGS1(4), TIN,       &
!/T                        IDSTR(5), INFLAGS1(5), T0N,       &
!/T                        IDSTR(6), INFLAGS1(6), T1N,       &
!/T                        IDSTR(7), INFLAGS1(7), T2N,       &
!/T                        IDSTR(8), INFLAGS1(8), TG0, TGN
!
!/MEMCHECK      write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 7'
!/MEMCHECK      call getMallocInfo(mallinfos)
!/MEMCHECK      call printMallInfo(IAPROC,mallInfos)


      DO J=JFIRST,8
!
!/MEMCHECK      write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL UPDATE', J
!/MEMCHECK      call getMallocInfo(mallinfos)
!/MEMCHECK      call printMallInfo(IAPROC,mallInfos)

        IF ( INFLAGS1(J) ) THEN
!
! 7.a.2 Check if update is needed
!
          IF (.NOT.FLAGSC(J)) THEN 
            TTT(1) = TFN(1,J)
            TTT(2) = TFN(2,J)
            IF ( TTT(1) .EQ. -1 ) THEN
              DTTST  = 0.
            ELSE
              DTTST  = DSEC21 ( TIME0 , TTT )
            END IF
!/OASIS          ELSE 
!/OASIS            ID_OASIS_TIME = NINT(DSEC21 ( TIME00 , TIME ))
!/OASIS            IF ( (DTOUT(7).NE.0) .AND.                               &
!/OASIS                 (MOD(ID_OASIS_TIME, NINT(DTOUT(7))) .EQ. 0 ) .AND. &
!/OASIS                 (DSEC21 (TIME, TIMEEND) .GT. 0.0)) DTTST=0.
!/COAWST         ELSE
!/COAWST         IF (DSEC21 (TIME, TIMEEND) .GT. 0.0) DTTST=0.
          END IF
!
!/T          WRITE (NDST,9071) IDSTR(J), DTTST
!
! 7.a.3 Update time and fields / data
!
          IF ( DTTST .LE. 0. ) THEN

!/TIDE            IF ((FLLEVTIDE .AND.(J.EQ.1)).OR.(FLCURTIDE.AND.(J.EQ.2))) THEN 
!/TIDE              IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,974) IDFLDS(J)
!/TIDE            ELSE
            IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,972) IDFLDS(J)
!/TIDE            END IF
!
! IC1 : (in context of IC3 & IC2, this is ice thickness)
            IF ( J .EQ. -7 ) THEN
              IF ( FLH(J) ) THEN
                CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
                             TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
                             TTT, XXX, XXX, XXX, TI1, XXX, XXX, ICEP1, IERR)
              ELSE
!/OASIS                COUPL_COMM = MPI_COMM
!/OASICM                IF (FLAGSC(J)) FLAGSCI = .TRUE.
!/OASICM                IF (.NOT.FLAGSCI) ID_OASIS_TIME = -1
                CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
                             NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
                             TTT, XXX, XXX, XXX, TI1, XXX, XXX, ICEP1,  &
                             IERR, FLAGSC(J)                            &
!/OASICM                             , COUPL_COMM                       &
                             )
              END IF
              IF ( IERR .LT. 0 ) FLLST_ALL(J) = .TRUE.

! IC2 : (in context of IC3, this is ice viscosity)
            ELSE IF ( J .EQ. -6 ) THEN
              IF ( FLH(J) ) THEN
                CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
                             TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
                             TTT, XXX, XXX, XXX, TI2, XXX, XXX, ICEP2, IERR)
              ELSE
                CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
                             NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
                             TTT, XXX, XXX, XXX, TI2, XXX, XXX, ICEP2,  &
                             IERR, FLAGSC(J))
              END IF
              IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.

! IC3 : (in context of IC3, this is ice density)
            ELSE IF ( J .EQ. -5 ) THEN
              IF ( FLH(J) ) THEN
                CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
                             TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
                             TTT, XXX, XXX, XXX, TI3, XXX, XXX, ICEP3, IERR)
              ELSE
                CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
                             NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
                             TTT, XXX, XXX, XXX, TI3, XXX, XXX, ICEP3,  &
                             IERR, FLAGSC(J))
              END IF
              IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.

! IC4 : (in context of IC3, this is ice modulus)
            ELSE IF ( J .EQ. -4 ) THEN
              IF ( FLH(J) ) THEN
                CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
                             TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
                             TTT, XXX, XXX, XXX, TI4, XXX, XXX, ICEP4, IERR)
              ELSE
                CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
                             NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
                             TTT, XXX, XXX, XXX, TI4, XXX, XXX, ICEP4,  &
                             IERR, FLAGSC(J))
              END IF
              IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.

! IC5 : ice flow diam.
            ELSE IF ( J .EQ. -3 ) THEN
              IF ( FLH(J) ) THEN
                CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
                             TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
                             TTT, XXX, XXX, XXX, TI5, XXX, XXX, ICEP5, IERR)
              ELSE
!/OASIS                COUPL_COMM = MPI_COMM
!/OASICM                IF (FLAGSC(J)) FLAGSCI = .TRUE.
!/OASICM                IF (.NOT.FLAGSCI) ID_OASIS_TIME = -1
                CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
                             NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
                             TTT, XXX, XXX, XXX, TI5, XXX, XXX, ICEP5,  &
                             IERR, FLAGSC(J)                            &
!/OASICM                             , COUPL_COMM                       &
                             )
              END IF
              IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.

! MUD1 : mud density
            ELSE IF ( J .EQ. -2 ) THEN
              IF ( FLH(J) ) THEN
                CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
                             TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
                             TTT, XXX, XXX, XXX, TZN, XXX, XXX, MUDD, IERR)
              ELSE
                CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
                             NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
                             TTT, XXX, XXX, XXX, TZN, XXX, XXX, MUDD,   &
                             IERR, FLAGSC(J))
              END IF
              IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.

! MUD2 : mud thickness
            ELSE IF ( J .EQ. -1 ) THEN
              IF ( FLH(J) ) THEN
                CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
                             TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
                             TTT, XXX, XXX, XXX, TTN, XXX, XXX, MUDT, IERR)
              ELSE
                CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
                             NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
                             TTT, XXX, XXX, XXX, TTN, XXX, XXX, MUDT,   &
                             IERR, FLAGSC(J))
              END IF
              IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.

! MUD3 : mud viscosity
            ELSE IF ( J .EQ. 0 ) THEN
              IF ( FLH(J) ) THEN
                CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
                             TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
                             TTT, XXX, XXX, XXX, TVN, XXX, XXX, MUDV, IERR)
              ELSE
                CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
                             NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
                             TTT, XXX, XXX, XXX, TVN, XXX, XXX, MUDV,   &
                             IERR, FLAGSC(J))
              END IF
              IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.

! LEV : water levels
            ELSE IF ( J .EQ. 1 ) THEN
              IF ( FLH(J) ) THEN
                CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
                             TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
                             TTT, XXX, XXX, XXX, TLN, XXX, XXX, WLEV, IERR)
              ELSE
!/TIDE                IF ( FLLEVTIDE ) THEN 
!/TIDE                  IERR=0
!/TIDE                  IF ( TLN(1) .EQ. -1 ) THEN 
!/TIDE                    TLN = TIME 
!/TIDE                  ELSE 
!/TIDE                    CALL TICK21 ( TLN, TIDE_DT )
!/TIDE                  END IF
!/TIDE                ELSE
!/OASIS                COUPL_COMM = MPI_COMM
!/OASOCM                IF (.NOT.FLAGSC(J)) ID_OASIS_TIME = -1
!/COAWST              TFN(1,J)=TIME0(1)
!/COAWST              TFN(2,J)=TIME0(2)
!/COAWST              CALL TICK21(TFN(:,J),REAL(TI_OCN2WAV))
!/TIDE                END IF
              END IF
              IF ( IERR .LT. 0 ) FLLSTL = .TRUE.
!could be:    IF ( IERR .LT. 0 ) FLLST_ALL(J) = .TRUE.

! CUR : currents
            ELSE IF ( J .EQ. 2 ) THEN
              IF ( FLH(J) ) THEN
                CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
                             TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
                             TC0, CX0, CY0, XXX, TCN, CXN, CYN, XXX, IERR)
!
!/SMC !!Li  Reshape the CX0/N CY0/N space for sea-point only current. 
!/SMC !!Li              JGLi26Jun2018. 
!/SMC              ELSE IF( FSWND ) THEN
!/SMC                 CALL W3FLDG ('READ', IDSTR(J), NDSF(J), NDST,    &
!/SMC                      NDSEN, NSEA, 1, NSEA, 1, TIME0, TIMEN, TC0, &
!/SMC                      CX0, CY0, XXX, TCN, CXN, CYN, XXX, IERR) 
!/SMC !!Li  
              ELSE
!/TIDE                IF ( FLCURTIDE ) THEN 
!/TIDE                  IERR=0
!/TIDE                  IF ( TCN(1) .EQ. -1 ) THEN 
!/TIDE                    TCN = TIME 
!/TIDE                  END IF
!/TIDE                  TC0(:) = TCN(:)               
!/TIDE                  CALL TICK21 ( TCN, TIDE_DT )
!/TIDE                ELSE
!/OASIS                COUPL_COMM = MPI_COMM
!/OASOCM                IF (.NOT.FLAGSC(J)) ID_OASIS_TIME = -1
!/COAWST              TFN(1,J)=TIME0(1)
!/COAWST              TFN(2,J)=TIME0(2)
!/COAWST              CALL TICK21(TFN(:,J),REAL(TI_OCN2WAV))
!/TIDE                END IF
              END IF

! WND : winds
            ELSE IF ( J .EQ. 3 ) THEN
              IF ( FLH(J) ) THEN
                CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
                             TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
                             TW0, WX0, WY0, DT0, TWN, WXN, WYN, DTN, IERR)
!
!/SMC !!Li  Reshape the WX0/N WY0/N space for sea-point only wind. 
!/SMC !!Li              JGLi26Jun2018. 
!/SMC              ELSE IF( FSWND ) THEN
!/SMC                 CALL W3FLDG ('READ', IDSTR(J), NDSF(J), NDST,    &
!/SMC                      NDSEN, NSEA, 1, NSEA, 1, TIME0, TIMEN, TW0, &
!/SMC                      WX0, WY0, DT0, TWN, WXN, WYN, DTN, IERR)  
!/SMC !!Li  
              ELSE
!/OASIS                COUPL_COMM = MPI_COMM
!/OASACM                IF (.NOT.FLAGSC(J)) ID_OASIS_TIME = -1
!/NCC                IF (.NOT.CFLAG(J)) THEN
!/COAWST              TFN(1,J)=TIME0(1)
!/COAWST              TFN(2,J)=TIME0(2)
!/COAWST              CALL TICK21(TFN(:,J),REAL(TI_ATM2WAV))
              END IF

! ICE : ice conc.
            ELSE IF ( J .EQ. 4 ) THEN
              IF ( FLH(J) ) THEN
                CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
                             TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
                             TW0, WX0, WY0, DT0, TWN, WXN, WYN, DTN, IERR)
              ELSE
!/OASIS                COUPL_COMM = MPI_COMM
!/OASICM                IF (FLAGSC(J)) FLAGSCI = .TRUE.
!/OASICM                IF (.NOT.FLAGSCI) ID_OASIS_TIME = -1
!/NCC                IF (.NOT.CFLAG(J)) THEN
                CALL W3FLDG ('READ', IDSTR(J), NDSF(J),            &
                             NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN,    &
                             TTT, XXX, XXX, XXX, TIN, XXX, BERGI, ICEI,    &
                             IERR, FLAGSC(J)                               &
!/OASICM                             , COUPL_COMM                          &
                            )
                IF ( IERR .LT. 0 ) FLLSTI = .TRUE.
!could be:      IF ( IERR .LT. 0 ) FLLST_ALL(J) = .TRUE.
!/NCC                END IF
!/NCC                CALL WW_CDETECT(CFLAG(J))
!/NCC                IF ( IAPROC .EQ. NAPOUT .AND. DEBUG_NCC )    &
!/NCC                     WRITE(9753) TIN
!/NCC                CALL WW_UPDATE_PSEUDOICE(TIN,ICEI)
!/NCC                IF ( IAPROC .EQ. NAPOUT .AND. DEBUG_NCC )    &
!/NCC                     WRITE(9752) TIN, ICEI
              END IF

! Assim data
            ELSE IF ( J .EQ. 5 ) THEN
              CALL W3FLDD ('SIZE', IDSTR(J), NDSF(J), NDST,      &
                           NDSEN, TIME0, T0N, RCLD(J), NDT(J),           &
                           NDTNEW, DATA0, IERR )
              IF ( IERR .LT. 0 ) THEN
                INFLAGS1(J) = .FALSE.
                IF ( ALLOCATED(DATA0) ) DEALLOCATE(DATA0)
              ELSE
                NDT(J) = NDTNEW
                IF ( ALLOCATED(DATA0) ) DEALLOCATE(DATA0)
                ALLOCATE ( DATA0(RCLD(J),NDT(J)) )
                CALL W3FLDD ('READ', IDSTR(J), NDSF(J), NDST, &
                             NDSEN, TIME0, T0N, RCLD(J), NDT(J),      &
                             NDTNEW, DATA0, IERR )
              END IF

! Assim data
            ELSE IF ( J .EQ. 6 ) THEN
              CALL W3FLDD ('SIZE', IDSTR(J), NDSF(J), NDST,      &
                           NDSEN, TIME0, T1N, RCLD(J), NDT(J),           &
                           NDTNEW, DATA1, IERR )
              IF ( IERR .LT. 0 ) THEN
                INFLAGS1(J) = .FALSE.
                IF ( ALLOCATED(DATA1) ) DEALLOCATE(DATA1)
              ELSE
                NDT(J) = NDTNEW
                IF ( ALLOCATED(DATA1) ) DEALLOCATE(DATA1)
                ALLOCATE ( DATA1(RCLD(J),NDT(J)) )
                CALL W3FLDD ('READ', IDSTR(J), NDSF(J), NDST, &
                             NDSEN, TIME0, T1N, RCLD(J), NDT(J),      &
                             NDTNEW, DATA1, IERR )
              END IF

! Assim data
            ELSE IF ( J .EQ. 7 ) THEN
              CALL W3FLDD ('SIZE', IDSTR(J), NDSF(J), NDST,      &
                           NDSEN, TIME0, T2N, RCLD(J), NDT(J),           &
                           NDTNEW, DATA2, IERR )
              IF ( IERR .LT. 0 ) THEN
                INFLAGS1(J) = .FALSE.
                IF ( ALLOCATED(DATA2) ) DEALLOCATE(DATA2)
              ELSE
                NDT(J) = NDTNEW
                IF ( ALLOCATED(DATA2) ) DEALLOCATE(DATA2)
                ALLOCATE ( DATA2(RCLD(J),NDT(J)) )
                CALL W3FLDD ('READ', IDSTR(J), NDSF(J), NDST, &
                             NDSEN, TIME0, T2N, RCLD(J), NDT(J),      &
                             NDTNEW, DATA2, IERR )
              END IF

! Track
            ELSE IF ( J .EQ. 8 ) THEN
              CALL W3FLDM (4, NDST, NDSEN, TIME0, TIMEN, NH(4),  &
                           NHMAX, THO, HA, HD, TG0, GA0, GD0,         &
                           TGN, GAN, GDN, IERR)
            END IF
!
            IF ( IERR.GT.0 ) GOTO 2222
            IF ( IERR.LT.0 .AND. IAPROC.EQ.NAPOUT ) WRITE (NDSO,973) IDFLDS(J)
                           

          END IF ! DTTST .LE. 0.
!
! 7.a.4 Update next ending time
!
          IF ( INFLAGS1(J) ) THEN
            TTT    = TFN(:,J)
            DTTST  = DSEC21 ( TTT , TTIME )
            IF ( DTTST.GT.0. .AND. .NOT.                          &
                   ( (FLLSTL .AND. J.EQ.1) .OR.                   &
                     (FLLST_ALL(J) .AND. J.EQ.-7) .OR.            &
                     (FLLST_ALL(J) .AND. J.EQ.-6) .OR.            &
                     (FLLST_ALL(J) .AND. J.EQ.-5) .OR.            &
                     (FLLST_ALL(J) .AND. J.EQ.-4) .OR.            &
                     (FLLST_ALL(J) .AND. J.EQ.-3) .OR.            &
                     (FLLST_ALL(J) .AND. J.EQ.-2) .OR.            &
                     (FLLST_ALL(J) .AND. J.EQ.-1) .OR.            &
                     (FLLST_ALL(J) .AND. J.EQ.0 ) .OR.            &
                     (FLLSTI .AND. J.EQ.4) ) ) THEN
              TTIME  = TTT
! notes: if model has run out beyond field input, then this line should not 
!    be reached. 
            END IF
          END IF
!
        END IF ! INFLAGSC1(J)
!
      END DO ! J=JFIRST,8
!
! update the next assimilation data time
!

!/MEMCHECK      write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 8'
!/MEMCHECK      call getMallocInfo(mallinfos)
!/MEMCHECK      call printMallInfo(IAPROC,mallInfos)

      TDN = TTIME
      CALL TICK21 ( TDN, 1. )
      DO J=5, 7
        IF ( INFLAGS1(J) ) THEN
          TTT    = TFN(:,J)
          DTTST  = DSEC21 ( TTT , TDN )
          IF ( DTTST.GT.0. ) TDN = TTT
        END IF
      END DO
!
!/T      WRITE (NDST,9072) '0-N', TIME0, TTIME,           &
!/T                        IDSTR(-7), INFLAGS1(-7), TI1,     &
!/T                        IDSTR(-6), INFLAGS1(-6), TI2,     &
!/T                        IDSTR(-5), INFLAGS1(-5), TI3,     &
!/T                        IDSTR(-4), INFLAGS1(-4), TI4,     &
!/T                        IDSTR(-3), INFLAGS1(-3), TI5,     &
!/T                        IDSTR(-2), INFLAGS1(-2), TZN,     &
!/T                        IDSTR(-1), INFLAGS1(-1), TTN,     &
!/T                        IDSTR(0), INFLAGS1(0), TVN,       &
!/T                        IDSTR(1), INFLAGS1(1), TLN,       &
!/T                        IDSTR(2), INFLAGS1(2), TC0, TCN,  &
!/T                        IDSTR(3), INFLAGS1(3), TW0, TWN,  &
!/T                        IDSTR(4), INFLAGS1(4), TIN,       &
!/T                        IDSTR(5), INFLAGS1(5), T0N,       &
!/T                        IDSTR(6), INFLAGS1(6), T1N,       &
!/T                        IDSTR(7), INFLAGS1(7), T2N, TDN,  &
!/T                        IDSTR(8), INFLAGS1(8), TG0, TGN
!
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,*) ' '
!
! 7.b Run the wave model for the given interval
!
      TIME0  = TTIME
!
      CALL W3WAVE ( 1, ODAT, TIME0                                    &
!/OASIS             , .TRUE., .FALSE., MPI_COMM, TIMEN                         &
                  )

!/MEMCHECK      write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 9'
!/MEMCHECK      call getMallocInfo(mallinfos)
!/MEMCHECK      call printMallInfo(IAPROC,mallInfos)
!
      ! The following lines prevents us from trying to read past the end 
      ! of the files. This feature existed in v3.14.
      ! "1" is for water levels
      ! "4" is for ice concentration: 
      IF ( FLLSTL ) INFLAGS1(1) = .FALSE.
      IF ( FLLSTI ) INFLAGS1(4) = .FALSE.

      ! We include something like this for mud and ice parameters also:
      DO J=-7,0
        IF (FLLST_ALL(J))THEN
          INFLAGS1(J)=.FALSE.
        END IF
      END DO

!
! 7.c Run data assimilation at ending time
!
      DTTST  = DSEC21 ( TIME , TDN )
      IF ( DTTST .EQ. 0 ) THEN
        CALL STME21 ( TIME0 , DTME21 )
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,975) DTME21
!
        FLGDAS(1) = DSEC21(TIME,T0N) .EQ. 0.
        FLGDAS(2) = DSEC21(TIME,T1N) .EQ. 0.
        FLGDAS(3) = DSEC21(TIME,T2N) .EQ. 0.
!
        CALL W3WDAS ( FLGDAS, RCLD, NDT, DATA0, DATA1, DATA2 )
!
! 7.d Call wave model again after data assimilation for output only
!
        DTTST  = DSEC21 ( TIME , TIMEN )

        IF ( DTTST .EQ. 0. ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,*) ' '
          CALL W3WAVE ( 1, ODAT, TIME0                                 & 
!/OASIS                       , .TRUE., .FALSE., MPI_COMM, TIMEN              &
                      )
        END IF
      END IF
!
! 7.e Check times
!
!/MEMCHECK      write(740+IAPROC,*) 'memcheck_____:', 'WW3_SHEL SECTION 10'
!/MEMCHECK      call getMallocInfo(mallinfos)


      DTTST  = DSEC21 ( TIME0 , TIMEN )
      IF ( DTTST .GT. 0. ) GOTO 700
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     End of shel
!
      GOTO 2222
!
! Error escape locations
!
 2000 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) IERR
      CALL EXTCDE ( 1000 )
!
 2001 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001)
      CALL EXTCDE ( 1001 )
!
 2002 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002) IERR
      CALL EXTCDE ( 1002 )
!
 2102 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1102)
      CALL EXTCDE ( 1102 )
!
 2003 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1003)
      CALL EXTCDE ( 1003 )
!
 2104 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1104) IERR
      CALL EXTCDE ( 1104 )
!
 2004 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1004) IERR
      CALL EXTCDE ( 1004 )
!
 2005 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1005) IDTST
      CALL EXTCDE ( 1005 )
!
 2006 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1006) IDTST, NH(J)
      CALL EXTCDE ( 1006 )
!
 2062 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1062) IDTST
      CALL EXTCDE ( 1062 )
!
 2007 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1007)
      CALL EXTCDE ( 1007 )
!
 2008 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1008) IERR
      CALL EXTCDE ( 1008 )
!
!/COU 2009 CONTINUE
!/COU      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1009) ODAT(33), NINT(DTMAX)
!/COU      CALL EXTCDE ( 1009 )
!
 2054 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1054)
      CALL EXTCDE ( 1054 )
 2222 CONTINUE
!
!/MPI      CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
!
      IF ( IAPROC .EQ. NAPOUT ) THEN
!/F90          CALL DATE_AND_TIME ( VALUES=CLKDT3 )
!/F90          CLKFIN = MAX(TDIFF ( CLKDT1,CLKDT2 ), 0.)
!/F90          CLKFEL = MAX(TDIFF ( CLKDT1,CLKDT3 ), 0.)
!/F90          WRITE (NDSO,997) CLKFIN
!/F90          WRITE (NDSO,998) CLKFEL
!/F90          IF ( NDSO .NE. NDS(1) ) THEN
!/F90            WRITE (NDS(1),997) CLKFIN
!/F90            WRITE (NDS(1),998) CLKFEL
!/F90          END IF
        WRITE (NDSO,999)
      END IF
!
!/NCO/!     IF ( IAPROC .EQ. 1 ) CALL W3TAGE('WAVEFCST')
!/OASIS      IF (OASISED.EQ.1) THEN
!/OASIS        CALL CPL_OASIS_FINALIZE
!/OASIS      ELSE
!/COAWST      IF (COAWSTED.EQ.0) THEN
!/MPI        CALL MPI_FINALIZE  ( IERR_MPI )
!/COAWST      END IF
!/OASIS      END IF
!/COAWST      CALL FINALIZE_WAV_COUPLING(1)
!
!
! Formats
!
  900 FORMAT (/15X,'      *** WAVEWATCH III Program shell ***      '/ &
               15X,'==============================================='/)
  901 FORMAT ( '  Comment character is ''',A,''''/)
!
!/OMPH  905 FORMAT ( '  Hybrid MPI/OMP thread support level:'/        &
!/OMPH               '     Requested: ', I2/                          &
!/OMPH               '      Provided: ', I2/ )
  920 FORMAT (/'  Input fields : '/                                   &
               ' --------------------------------------------------')
  921 FORMAT ( '       ',A,2X,A,2X,A)
  922 FORMAT ( ' ' )
!
  930 FORMAT (/'  Time interval : '/                                  &
               ' --------------------------------------------------')
  931 FORMAT ( '       Starting time : ',A)
  932 FORMAT ( '       Ending time   : ',A/)
!
  940 FORMAT (/'  Output requests : '/                                &
               ' --------------------------------------------------'/ &
               '       ',A)
  941 FORMAT (/'       Type',I2,' : ',A/                              &
               '      -----------------------------------------')
  942 FORMAT ( '            From     : ',A)
  943 FORMAT ( '            To       : ',A)
 1944 FORMAT ( '            Interval : ', 8X,A11/)
 2944 FORMAT ( '            Interval : ', 9X,A10/)
 3944 FORMAT ( '            Interval : ',11X,A8/)
 2945 FORMAT ( '            Point  1 : ',2F8.2,2X,A)
 2955 FORMAT ( '            Point  1 : ',2(F8.1,'E3'),2X,A)
 2946 FORMAT ( '              ',I6,' : ',2F8.2,2X,A)
 2956 FORMAT ( '              ',I6,' : ',2(F8.1,'E3'),2X,A)
 2947 FORMAT ( '            No points defined')
 3945 FORMAT ( '            The file with ',A,' data is ',A,'.')
 6945 FORMAT ( '            IX first,last,inc :',3I5/                 &
               '            IY first,last,inc :',3I5/                 &
               '            Formatted file    :    ',A)
 8945 FORMAT ( '            output dates out of run dates : ', A,     &
               ' deactivated')
!
  950 FORMAT (/'  Initializations :'/                                 &
               ' --------------------------------------------------')
  951 FORMAT ( '       ',A)
!/O7  952 FORMAT ( '       ',I6,2X,A)
!/O7  953 FORMAT ( '          ',I6,I11.8,I7.6,3E12.4)
  954 FORMAT ( '            ',A,': file not needed')
  955 FORMAT ( '            ',A,': file OK')
  956 FORMAT ( '            ',A,': file OK, recl =',I3,               &
               '  undef = ',E10.3)
!
  960 FORMAT (/'  Running model without input fields'/                &
               ' --------------------------------------------------'/)
!
  970 FORMAT (/'  Running model with input fields'/                   &
               ' --------------------------------------------------')
  971 FORMAT (/'  Updating input at ',A)
  972 FORMAT ( '     Updating ',A)
  973 FORMAT ( '        Past last ',A)
!/TIDE  974 FORMAT ( '     Updating ',A,'using tidal constituents')
  975 FORMAT (/'  Data assimmilation at ',A)
!
!/F90  997 FORMAT (/'  Initialization time :',F10.2,' s')
!/F90  998 FORMAT ( '  Elapsed time        :',F10.2,' s')
!
  999 FORMAT(/'  End of program '/                                    &
               ' ===================================='/               &
               '         WAVEWATCH III Program shell '/)
!
 1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     ERROR IN OPENING INPUT FILE'/                    &
               '     IOSTAT =',I5/)
!
 1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     PREMATURE END OF INPUT FILE'/)
!
 1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     ERROR IN READING FROM INPUT FILE'/               &
               '     IOSTAT =',I5/)
!
 1102 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     LEVEL AND CURRENT ARE MIXING COUPLED AND FORCED'/&
               '     IT MUST BE FULLY COUPLED OR DISABLED '/)
!
 1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     ILLEGAL TIME INTERVAL'/)
!
 1104 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     ERROR IN OPENING POINT FILE'/                    &
               '     IOSTAT =',I5/)
!
 1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     ERROR IN READING FROM POINT FILE'/               &
               '     IOSTAT =',I5/)
!
 1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     ILLEGAL ID STRING HOMOGENEOUS FIELD : ',A/)
!
 1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     TOO MANY HOMOGENEOUS FIELDS : ',A,1X,I4/)
!
 1062 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : ***'/             &
               '     HOMOGENEOUS NAME NOT RECOGNIZED : ', A/)
!
 1007 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     INSUFFICIENT DATA FOR HOMOGENEOUS FIELDS'/)
!
 1008 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     ERROR IN OPENING OUTPUT FILE'/                   &
               '     IOSTAT =',I5/)
!
!/COU 1009 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
!/COU               '     COUPLING TIME STEP NOT MULTIPLE OF'/             &
!/COU               '     MODEL TIME STEP: ',I6, I6/)
!
!/COU 1010 FORMAT (/' *** WAVEWATCH III WARNING IN W3SHEL : *** '/         &
!/COU               '     COUPLING TIME STEP NOT DEFINED, '/               &
!/COU               '     IT WILL BE OVERRIDEN TO DEFAULT VALUE'/          &
!/COU               '     FROM ',I6, ' TO ',I6/)
!
 1054 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     POINT OUTPUT ACTIVATED BUT NO POINTS DEFINED'/)
!
!
!/T 9000 FORMAT ( ' TEST W3SHEL : UNIT NUMBERS  :',12I4)
!/T 9001 FORMAT ( ' TEST W3SHEL : SUBR. TRACING :',2I4)
!
!/T 9020 FORMAT ( ' TEST W3SHEL : FLAGS DEF / HOM  : ',9L2,2X,9L2)
!
!/T 9040 FORMAT ( ' TEST W3SHEL : ODAT   : ',I9.8,I7.6,I7,I9.8,I7.6,  &
!/T                                   4(/24X,I9.8,I7.6,I7,I9.8,I7.6) )
!/T 9041 FORMAT ( ' TEST W3SHEL : FLGRD  : ',20L2)
!/T 9042 FORMAT ( ' TEST W3SHEL : IPR, PRFRM : ',6I6,1X,L1)
!
!/T 9070 FORMAT ( ' TEST W3SHEL : ',A,3X,2(I10.8,I7.6)/                &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/             &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/             &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/             &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/             &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/             &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/             &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/             &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/             &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/             &
!/T               '               ',A,L3,2(I10.8,I7.6)/                &
!/T               '               ',A,L3,2(I10.8,I7.6)/                &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/             &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/             &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/             &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/             &
!/T               '               ',A,L3,2(I10.8,I7.6))
!/T 9071 FORMAT ( ' TEST W3SHEL : ',A,', DTTST = ',E10.3)
!/T 9072 FORMAT ( ' TEST W3SHEL : ',A,3X,2(I10.8,I7.6)/               &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/            &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/            &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/            &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/            &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/            &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/            &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/            &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/            &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/            &
!/T               '               ',A,L3,2(I10.8,I7.6)/               &
!/T               '               ',A,L3,2(I10.8,I7.6)/               &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/            &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/            &
!/T               '               ',A,L3,17X,(I10.8,I7.6)/            &
!/T               '               ',A,L3,17X,2(I10.8,I7.6)/           &
!/T               '               ',A,L3,2(I10.8,I7.6))
!/
!/ End of W3SHEL ----------------------------------------------------- /
!/
!/COAWST      END SUBROUTINE WW3_init
