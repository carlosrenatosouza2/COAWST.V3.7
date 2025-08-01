

!#include "w3macros.h"
























































































































































































































































































!/ ------------------------------------------------------------------- /
      MODULE W3WAVEMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         27-Aug-2015 |
!/                  +-----------------------------------+
!/
!/    04-Feb-2000 : Origination.                        ( version 2.00 )
!/                  For upgrades see subroutines.
!/    14-Feb-2000 : Exact-NL added.                     ( version 2.01 )
!/    05-Jan-2001 : Bug fix to allow model to run       ( version 2.05 )
!/                  without output.
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    09-Feb-2001 : Third propagation scheme added.     ( version 2.08 )
!/    23-Feb-2001 : Check for barrier after source
!/                  terms added ( W3NMIN ).     ( delayed version 2.07 )
!/    16-Mar-2001 : Fourth propagation scheme added.    ( version 2.09 )
!/    30-Mar-2001 : Sub-grid obstacles added.           ( version 2.10 )
!/    23-May-2001 : Clean up and bug fixes.             ( version 2.11 )
!/    10-Dec-2001 : Sub-grid obstacles for UQ schemes.  ( version 2.14 )
!/    11-Jan-2002 : Sub-grid ice.                       ( version 2.15 )
!/    24-Jan-2002 : Zero time step dor data ass.        ( version 2.17 )
!/    18-Feb-2002 : Point output diagnostics added.     ( version 2.18 )
!/    30-Apr-2002 : Add field output types 17-18.       ( version 2.20 )
!/    09-May-2002 : Switch clean up.                    ( version 2.21 )
!/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
!/    26-Dec-2002 : Moving grid version.                ( version 3.02 )
!/    01-Aug-2003 : Moving grid GSE correction.         ( version 3.03 )
!/    20-Aug-2003 : Output server options added.        ( version 3.04 )
!/    07-Oct-2003 : Output options for NN training.     ( version 3.05 )
!/    29-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/                  W3INIT, W3MPII-O and WWVER moved to w3initmd.ftn
!/    04-Feb-2005 : Add STAMP to par list of W3WAVE.    ( version 3.07 )
!/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
!/    28-Jun-2005 : Adding map recalc for W3ULEV call.  ( version 3.07 )
!/    07-Sep-2005 : Updated boundary conditions.        ( version 3.08 )
!/                  Fix NRQSG1/2 = 0 array bound issue.
!/    13-Jun-2006 : Split STORE in G/SSTORE             ( version 3.09 )
!/    26-Jun-2006 : Add output type 6.                  ( version 3.09 )
!/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
!/    18-Oct-2006 : Partitioned spectral data output.   ( version 3.10 )
!/    02-Feb-2007 : Add FLAGST test.                    ( version 3.10 )
!/    02-Apr-2007 : Add partitioned field data.         ( version 3.11 )
!/    07-May-2007 : Bug fix SKIP_O treatment.           ( version 3.11 )
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    08-Oct-2007 : Adding AS CX-Y to W3SRCE par. list. ( version 3.13 )
!/    22-Feb-2008 : Initialize VGX-Y properly.          ( version 3.13 )
!/    10-Apr-2008 : Bug fix writing log file (MPI).     ( version 3.13 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    29-Mar-2010 : Adding coupling, ice in W3SRCE.     ( version 3.14_SHOM )
!/    16-May-2010 : Adding transparencies in W3SCRE     ( version 3.14_SHOM )
!/    23-Jun-2011 : Movable bed bottom friction BT4     ( version 4.04 )
!/    03-Nov-2011 : Shoreline reflection on unst. grids ( version 4.04 )
!/    02-Jul-2011 : Update for PALM coupling            ( version 4.07 )
!/    06-Mar-2012 : Initializing ITEST as needed.       ( version 4.07 )
!/    02-Jul-2012 : Update for PALM coupling            ( version 4.07 )
!/    02-Sep-2012 : Clean up of open BC for UG grids    ( version 4.08 )
!/    03-Sep-2012 : Fix format 902.                     ( version 4.10 )
!/    07-Dec-2012 : Wrap W3SRCE with TMPn to limit WARN ( version 4.OF )
!/    10-Dec-2012 : Modify field output MPI for new     ( version 4.OF )
!/                  structure and smaller memory footprint.
!/    12-Dec-2012 : Adding SMC grid.  JG_Li             ( version 4.08 )
!/    26-Dec-2012 : Move FIELD init. to W3GATH.         ( version 4.OF )
!/    16-Sep-2013 : Add Arctic part for SMC grid.       ( version 4.11 )
!/    11-Nov-2013 : SMC and rotated grid incorporated in the main 
!/                  trunk                               ( version 4.13 )
!/    14-Nov-2013 : Remove orphaned work arrays.        ( version 4.13 )
!/    27-Nov-2013 : Fixes for OpenMP versions.          ( version 4.15 )
!/    23-May-2014 : Adding ice fluxes to W3SRCE         ( version 5.01 )
!/    27-May-2014 : Move to OMPG/X switch.              ( version 5.02 )
!/    24-Apr-2015 : Adding OASIS coupling calls         ( version 5.07 )
!/                  (M. Accensi & F. Ardhuin, IFREMER)
!/    27-Aug-2015 : Update for ICEH, ICEF               ( version 5.08 )
!/    14-Sep-2018 : Remove PALM implementation          ( version 6.06 )
!/    15-Sep-2020 : Bugfix FIELD allocation. Remove     ( version 7.11 )
!/                  defunct OMPX switches.
!/
!/    Copyright 2009-2014 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!  2. Variables and types :
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. Public   Actual wave model.
!      W3GATH    Subr. Public   Data transpose before propagation.
!      W3SCAT    Subr. Public   Data transpose after propagation.
!      W3NMIN    Subr. Public   Calculate minimum number of sea
!                               points per processor.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETx    Subr. W3xDATMD Point to data structure.
!
!      W3UCUR    Subr. W3UPDTMD Interpolate current fields in time.
!      W3UWND    Subr. W3UPDTMD Interpolate wind fields in time.
!      W3UINI    Subr. W3UPDTMD Update initial conditions if init.
!                               with initial wind conditions.
!      W3UBPT    Subr. W3UPDTMD Update boundary points.
!      W3UICE    Subr. W3UPDTMD Update ice coverage.
!      W3ULEV    Subr. W3UPDTMD Transform the wavenumber grid.
!      W3DDXY    Subr. W3UPDTMD Calculate dirivatives of the depth.
!      W3DCXY    Subr. W3UPDTMD Calculate dirivatives of the current.
!
!      W3MAPn    Subr. W3PROnMD Preparation for  ropagation schemes.
!      W3XYPn    Subr. W3PROnMD Longitude-latitude ("XY") propagation.
!      W3KTPn    Subr. W3PROnMD Intra-spectral ("k-theta") propagation.
!
!      W3SRCE    Subr. W3SRCEMD Source term integration and calculation.
!
!      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
!      W3OUTG    Subr. W3IOGOMD Generate gridded output fields.
!      W3IOGO    Subr. W3IOGOMD Read/write gridded output.
!      W3IOPE    Subr. W3IOPOMD Extract point output.
!      W3IOPO    Subr. W3IOPOMD Read/write point output.
!      W3IOTR    Subr. W3IOTRMD Process spectral output along tracks.
!      W3IORS    Subr. W3IORSMD Read/write restart files.
!      W3IOBC    Subr. W3IOBCMD Read/write boundary conditions.
!      W3CPRT    Subr. W3IOSFMD Partition spectra.
!      W3IOSF    Subr.   Id.    Write partitioned spectral data.
!
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      WWTIME    Subr.   Id.    System time in readable format.
!      EXTCDE    Subr.   Id.    Program abort.
!
!      TICK21    Subr. W3TIMEMD Advance the clock.
!      DSEC21    Func.   Id.    Difference between times.
!      STME21    Subr.   Id.    Time in readable format.
!
!      MPI_BARRIER, MPI_STARTALL, MPI_WAITALL
!                Subr.          Basic MPI routines.
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!  6. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!       !/MPI   Id.
!       !/OMPG  Id.
!
!       !/PR1   First order propagation schemes.
!       !/PR2   ULTIMATE QUICKEST scheme.
!       !/PR3   Averaged ULTIMATE QUICKEST scheme.
!       !/PRX   User-defined scheme.
!       !/SMC   UNO2 scheme on SMC grid.
!
!       !/S     Enable subroutine tracing.
!       !/T     Test output.
!       !/MPIT  Test output for MPI specific code.
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
!/MPI      USE W3ADATMD, ONLY: MPIBUF
!
      PUBLIC
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3WAVE ( IMOD, ODAT, TEND, STAMP, NO_OUT & 
!/OASIS                  ,ID_LCOMM, TIMEN                 &
                         )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         25-Sep-2020 |
!/                  +-----------------------------------+
!/
!/    17-Mar-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    05-Jan-2001 : Bug fix to allow model to run       ( version 2.05 )
!/                  without output.
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    09-Feb-2001 : Third propagation scheme added.     ( version 2.08 )
!/    23-Feb-2001 : Check for barrier after source
!/                  terms added ( W3NMIN ).     ( delayed version 2.07 )
!/    16-Mar-2001 : Fourth propagation scheme added.    ( version 2.09 )
!/    30-Mar-2001 : Sub-grid obstacles added.           ( version 2.10 )
!/    23-May-2001 : Barrier added for dry run, changed  ( version 2.10 )
!/                  declaration of FLIWND.
!/    10-Dec-2001 : Sub-grid obstacles for UQ schemes.  ( version 2.14 )
!/    11-Jan-2002 : Sub-grid ice.                       ( version 2.15 )
!/    24-Jan-2002 : Zero time step dor data ass.        ( version 2.17 )
!/    09-May-2002 : Switch clean up.                    ( version 2.21 )
!/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
!/    26-Dec-2002 : Moving grid version.                ( version 3.02 )
!/    01-Aug-2003 : Moving grid GSE correction.         ( version 3.03 )
!/    07-Oct-2003 : Output options for NN training.     ( version 3.05 )
!/    29-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    04-Feb-2005 : Add STAMP to par list.              ( version 3.07 )
!/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
!/    28-Jun-2005 : Adding map recalc for W3ULEV call.  ( version 3.07 )
!/    07-Sep-2005 : Updated boundary conditions.        ( version 3.08 )
!/    26-Jun-2006 : Add output type 6.                  ( version 3.09 )
!/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
!/    18-Oct-2006 : Partitioned spectral data output.   ( version 3.10 )
!/    02-Feb-2007 : Add FLAGST test.                    ( version 3.10 )
!/    02-Apr-2007 : Add partitioned field data.         ( version 3.11 )
!/                  Improve MPI_WAITALL call tests/allocations.
!/    07-May-2007 : Bug fix SKIP_O treatment.           ( version 3.11 )
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    08-Oct-2007 : Adding AS CX-Y to W3SRCE par. list. ( version 3.13 )
!/    22-Feb-2008 : Initialize VGX-Y properly.          ( version 3.13 )
!/    10-Apr-2008 : Bug fix writing log file (MPI).     ( version 3.13 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    31-Mar-2010 : Add reflections                     ( version 3.14.4 )
!/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.4 )
!/                  (A. Roland and F. Ardhuin) 
!/    06-Mar-2011 : Output of max. CFL (F.Ardhuin)      ( version 3.14.4 )
!/    05-Apr-2011 : Implement iteration for DTMAX <1s   ( version 3.14.4 )
!/    02-Jul-2012 : Update for PALM coupling            ( version 4.07 )
!/    02-Sep-2012 : Clean up of open BC for UG grids    ( version 4.08 )
!/    03-Sep-2012 : Fix format 902.                     ( version 4.10 )
!/    10-Dec-2012 : Modify field output MPI for new     ( version 4.OF )
!/                  structure and smaller memory footprint.
!/    16-Nov-2013 : Allows reflection on curvi. grids   ( version 4.13 )
!/    27-Nov-2013 : Fixes for OpenMP versions.          ( version 4.15 )
!/    23-May-2014 : Adding ice fluxes to W3SRCE         ( version 5.01 )
!/    27-May-2014 : Move to OMPG/X switch.              ( version 5.02 )
!/    24-Apr-2015 : Adding OASIS coupling calls         ( version 5.07 )
!/                  (M. Accensi & F. Ardhuin, IFREMER)
!/    27-Aug-2015 : Update for ICEH, ICEF               ( version 5.10 )
!/    31-Mar-2016 : Current option for smc grid.        ( version 5.18 )
!/    06-Jun-2018 : Add PDLIB/MEMCHECK/SETUP/NETCDF_QAD/TIMING
!/                  OASIS/DEBUGINIT/DEBUGSRC/DEBUGRUN/DEBUGCOH
!/                  DEBUGIOBP/DEBUGIOBC                 ( version 6.04 )
!/    14-Sep-2018 : Remove PALM implementation          ( version 6.06 )
!/    25-Sep-2020 : Oasis coupling at T+0               ( version 7.10 )
!/
!  1. Purpose :
!
!     Run WAVEWATCH III for a given time interval.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number.
!       TEND    I.A.   I   Ending time of integration.
!       STAMP   Log.   I   WRITE(*,*)time stamp (optional, defaults to T).
!       NO_OUT  Log.   I   Skip output (optional, defaults to F).
!                          Skip at ending time only!
!     ----------------------------------------------------------------
!
!     Local parameters : Flags
!     ----------------------------------------------------------------
!       FLOUTG  Log.  Flag for running W3OUTG.
!       FLPART  Log.  Flag for running W3CPRT.
!       FLZERO  Log.  Flag for zero time interval.
!       FLAG0   Log.  Flag for processors without tasks.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!     Any program shell or integrated model which uses WAVEWATCH III.
!
!  6. Error messages :
!
!  7. Remarks :
!
!     - Currents are updated before winds as currents are used in wind
!       and USTAR processing.
!     - Ice and water levels can be updated only once per call.
!     - If ice or water level time are undefined, the update
!       takes place asap, otherwise around the "half-way point"
!       betweem the old and new times.
!     - To increase accuracy, the calculation of the intra-spectral
!       propagation is performed in two parts around the spatial propagation.
!
!  8. Structure :
!
!     -----------------------------------------------------------
!       0.  Initializations
!         a Point to data structures
!         b Subroutine tracing
!         c Local parameter initialization
!         d Test output
!       1.  Check the consistency of the input.
!         a Ending time versus initial time.
!         b Water level time.
!         c Current time interval.
!         d Wind time interval.
!         e Ice time.
!       2.  Determine next time from ending and output
!           time and get corresponding time step.
!       3.  Loop over time steps (see below).
!       4.  Perform output to file if requested.
!         a Check if time is output time.
!         b Processing and MPP preparations.  ( W3CPRT, W3OUTG )
!         c Reset next output time.
!        -------------- loop over output types ------------------
!         d Perform output.                           ( W3IOxx )
!         e Update next output time.
!        -------------------- end loop --------------------------
!       5.  Update log file.
!       6.  If time is not ending time, branch back to 2.
!     -----------------------------------------------------------
!
!      Section 3.
!     ----------------------------------------------------------
!       3.1  Interpolate winds and currents. ( W3UCUR, W3DCXY )
!                                                    ( W3UWND )
!                                                    ( W3UINI )
!       3.2  Update boundary conditions.     ( W3IOBC, W3UBPT )
!       3.3  Update ice coverage (if new ice map).   ( W3UICE )
!       3.4  Transform grid (if new water level).    ( W3ULEV )
!       3.5  Update maps and dirivatives.    ( W3MAPn, W3DDXY )
!                                            ( W3NMIN, W3UTRN )
!            Update grid advection vector.
!       3.6  Perform propagation
!          a Preparations.
!          b Intra spectral part 1.                  ( W3KTPn )
!          c Longitude-latitude       ( W3GATH, W3XYPn W3SCAT )
!          b Intra spectral part 2.                  ( W3KTPn )
!       3.7  Calculate and integrate source terms.   ( W3SRCE )
!       3.8  Update global time step.
!     ----------------------------------------------------------
!
!  9. Switches :
!
!     See module documentation.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
!/
      USE W3GDATMD
      USE W3WDATMD
      USE W3ADATMD
      USE W3IDATMD
      USE W3ODATMD
!/
      USE W3UPDTMD
      USE W3SRCEMD
!/PR1      USE W3PRO1MD
!/PR2      USE W3PRO2MD
!/PR3      USE W3PRO3MD
!/PRX      USE W3PROXMD
!/SMC      USE W3PSMCMD
!
!/PR1      USE W3PROFSMD
!/PR2      USE W3PROFSMD
!/PR3      USE W3PROFSMD
!/PRX      USE W3PROFSMD
!/
      USE W3TRIAMD
      USE W3IOGRMD
      USE W3IOGOMD
      USE W3IOPOMD
      USE W3IOTRMD
      USE W3IORSMD
      USE W3IOBCMD
      USE W3IOSFMD
!/PDLIB      USE PDLIB_W3PROFSMD, only : APPLY_BOUNDARY_CONDITION_VA
!/PDLIB      USE PDLIB_W3PROFSMD, only : PDLIB_W3XYPUG, PDLIB_W3XYPUG_BLOCK_IMPLICIT, PDLIB_W3XYPUG_BLOCK_EXPLICIT
!/PDLIB      USE PDLIB_W3PROFSMD, only : ALL_VA_INTEGRAL_PRINT, ALL_VAOLD_INTEGRAL_PRINT, ALL_FIELD_INTEGRAL_PRINT
!/PDLIB      USE W3PARALL, only : PDLIB_NSEAL, PDLIB_NSEALM
!/PDLIB      USE yowNodepool, only: npa, iplg
!/
      USE W3SERVMD
      USE W3TIMEMD
!/IC3      USE W3SIC3MD
!/IS2      USE W3SIS2MD
!/UOST      USE W3UOSTMD, ONLY: UOST_SETGRID
      USE W3PARALL, ONLY : INIT_GET_ISEA
!/MEMCHECK      USE MallocInfo_m
!/SETUP      USE W3WAVSET, only : WAVE_SETUP_COMPUTATION
!/NETCDF_QAD      USE W3NETCDF, only : OUTPUT_NETCDF_QUICK_AND_DIRTY

!/OASIS      USE W3OACPMD, ONLY: ID_OASIS_TIME, CPLT0
!/OASOCM      USE W3OGCMMD, ONLY: SND_FIELDS_TO_OCEAN
!/OASACM      USE W3AGCMMD, ONLY: SND_FIELDS_TO_ATMOS
!/OASICM      USE W3IGCMMD, ONLY: SND_FIELDS_TO_ICE
!/COAWST      USE CWSTWVCP
!/PDLIB      USE PDLIB_FIELD_VEC, only : DO_OUTPUT_EXCHANGES
!/TIMINGS    USE W3PARALL, only : PRINT_MY_TIME
!
      IMPLICIT NONE
!
!/MPI      INCLUDE "mpif.h"
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)           :: IMOD, TEND(2),ODAT(35)
      LOGICAL, INTENT(IN), OPTIONAL :: STAMP, NO_OUT
!/OASIS INTEGER, INTENT(IN), OPTIONAL :: ID_LCOMM
!/OASIS INTEGER, INTENT(IN), OPTIONAL :: TIMEN(2) 
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters :
!/
!/T      INTEGER                 :: ILEN
!/S      INTEGER, SAVE           :: IENT = 0
      INTEGER                 :: IP
      INTEGER                 :: TCALC(2), IT, IT0, NT, ITEST,        &
                                 ITLOC, ITLOCH, NTLOC, ISEA, JSEA,    &
                                 IX, IY, ISPEC, J, TOUT(2), TLST(2),  &
                                 REFLED(6), IK, ITH, IS, NKCFL
      INTEGER                 :: ISP, IP_glob
      INTEGER                 :: TTEST(2),DTTEST
      REAL                    :: ICEDAVE
!
!/MPI      LOGICAL                 :: SBSED
!/SEC1      INTEGER                 :: ISEC1
!/SBS      INTEGER                 :: JJ, NDSOFLG
!/MPI      INTEGER                 :: IERR_MPI, NRQMAX
!/MPI      INTEGER, ALLOCATABLE    :: STATCO(:,:), STATIO(:,:)
      INTEGER                 :: IXrel
      REAL                    :: DTTST, DTTST1, DTTST2, DTTST3,       &
                                 DTL0, DTI0, DTI10, DTI50,            &
                                 DTGA, DTG, DTGpre, DTRES,            &
                                 FAC, VGX, VGY, FACK, FACTH,          &
                                 FACX, XXX, REFLEC(4),                &
                                 DELX, DELY, DELA, DEPTH, D50, PSIC
     REAL                     :: VSioDummy(NSPEC), VDioDummy(NSPEC), VAoldDummy(NSPEC)
     LOGICAL                  :: SHAVETOTioDummy
!/SEC1     REAL                    :: DTGTEMP
!
      REAL, ALLOCATABLE       :: FIELD(:)
      REAL                    :: TMP1(4), TMP2(3), TMP3(2), TMP4(2)
!/IC3 REAL, ALLOCATABLE       :: WN_I(:)
!/REFRX REAL, ALLOCATABLE       :: CIK(:)
!
! Orphaned arrays from old data structure
!
      REAL, ALLOCATABLE       :: TAUWX(:), TAUWY(:)
!
      LOGICAL                 :: FLACT, FLZERO, FLFRST, FLMAP, TSTAMP,&
                                 SKIP_O, FLAG_O, FLDDIR, READBC,      &
                                 FLAG0 = .FALSE., FLOUTG, FLPFLD,     &
                                 FLPART, LOCAL, FLOUTG2
!
!Li   Logical variable to control regular gird lines in conflict with SMC option.
!AR   SMC option is in conflict with lofical variables for regular grid ... chicken ... egg ... stuff 
      LOGICAL                 :: RGLGRD = .TRUE., ARCTIC = .FALSE.
!!Li
!/MPI      LOGICAL                 :: FLGMPI(0:8)
!/IC3      REAL                    :: FIXEDVISC,FIXEDDENS,FIXEDELAS
!/IC3      REAL                    :: USE_CHENG, USE_CGICE, HICE
      LOGICAL                 :: UGDTUPDATE    ! true if time step should be updated for UG schemes
      CHARACTER(LEN=8)        :: STTIME
      CHARACTER(LEN=17)       :: IDACT 
      CHARACTER(LEN=13)       :: OUTID
      CHARACTER(LEN=23)       :: IDTIME
      INTEGER eIOBP
      INTEGER ITH_F
!/PDLIB     REAL ::             VS_SPEC(NSPEC)
!/PDLIB     REAL ::             VD_SPEC(NSPEC)

!
!/SBS      CHARACTER(LEN=30)       :: FOUTNAME
!
!/T     REAL             :: INDSORT(NSEA), DTCFL1(NSEA)
!/
!/ARC  !Li   Temperature spectra for Arctic boundary update.
!/ARC      REAL, ALLOCATABLE       :: BACSPEC(:)
!/ARC      REAL                    :: BACANGL
!/ARC

!/ ------------------------------------------------------------------- /
! 0.  Initializations

!/SMC !!Li  Switch off lat-lon grid lines in conflict with SMC option.
!/SMC        RGLGRD = .FALSE.
!
!/ARC !!Li  Switch on lines related to Arctic part on SMC grid.
!/ARC        ARCTIC = .TRUE.
!
! 0.a Set pointers to data structure
!
!/COU      SCREEN   =  333
!
!/DEBUGINIT      WRITE(740+IAPROC,*) 'W3WAVE, step 1'
!/DEBUGSRC      WRITE(740+IAPROC,*) 'Step 1 : max(UST)=', maxval(UST)
!/DEBUGINIT      FLUSH(740+IAPROC)
      IF ( IOUTP  .NE. IMOD ) CALL W3SETO ( IMOD, NDSE, NDST )
      IF ( IGRID  .NE. IMOD ) CALL W3SETG ( IMOD, NDSE, NDST )
      IF ( IWDATA .NE. IMOD ) CALL W3SETW ( IMOD, NDSE, NDST )
      IF ( IADATA .NE. IMOD ) CALL W3SETA ( IMOD, NDSE, NDST )
      IF ( IIDATA .NE. IMOD ) CALL W3SETI ( IMOD, NDSE, NDST )
!/UOST     CALL UOST_SETGRID(IMOD)

!/DEBUGRUN      DO JSEA = 1, NSEAL
!/DEBUGRUN        DO IS = 1, NSPEC
!/DEBUGRUN          IF (VA(IS, JSEA) .LT. 0.) THEN
!/DEBUGRUN            WRITE(740+IAPROC,*) 'NEGATIVE ACTION 1', IS, JSEA, VA(IS,JSEA)
!/DEBUGRUN            CALL FLUSH(740+IAPROC)
!/DEBUGRUN            CALL EXTCDE(666)
!/DEBUGRUN          ENDIF
!/DEBUGRUN        ENDDO
!/DEBUGRUN      ENDDO
!/DEBUGRUN      IF (SUM(VA) .NE. SUM(VA)) THEN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'NAN in ACTION 1', SUM(VA)
!/DEBUGRUN        CALL FLUSH(740+IAPROC)
!/DEBUGRUN        CALL EXTCDE(666)
!/DEBUGRUN      ENDIF


!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3WAVEMD, step 1")
!/PDLIB!/DEBUGIOBP         IF (NX .ge. 10210) WRITE(*,*) 'CRIT 1:', MAPSTA(1,10210), IOBP(10210)

!
      ALLOCATE(TAUWX(NSEAL), TAUWY(NSEAL))
!/REFRX      ALLOCATE(CIK(NSEAL))
!
      IF ( PRESENT(STAMP) ) THEN
          TSTAMP = STAMP
        ELSE
          TSTAMP = .TRUE.
        END IF
!
      IF ( PRESENT(NO_OUT) ) THEN
          SKIP_O = NO_OUT
        ELSE
          SKIP_O = .FALSE.
        END IF
!/DEBUGINIT      WRITE(740+IAPROC,*) 'W3WAVE, step 2'
!/DEBUGINIT      FLUSH(740+IAPROC)
!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3WAVEMD, step 2")
!
! 0.b Subroutine tracing
!
!/S      CALL STRACE (IENT, 'W3WAVE')
!
!
! 0.c Local parameter initialization
!
      IPASS  = IPASS + 1
      IDACT  = '                 '
      OUTID  = '           '
      FLACT  = ITIME .EQ. 0
      FLMAP  = ITIME .EQ. 0
      FLDDIR = ITIME .EQ. 0 .AND. ( FLCTH .OR. FSREFRACTION        &
          .OR. FLCK .OR. FSFREQSHIFT )
!
      FLPFLD = .FALSE.
      DO J=1,NOGE(4)
        FLPFLD = FLPFLD .OR. FLOGRD(4,J) .OR. FLOGR2(4,J)
        END DO
!
      IF ( IAPROC .EQ. NAPLOG ) BACKSPACE ( NDSO )
!
      IF ( FLCOLD ) THEN
          DTDYN = 0.
          FCUT  = SIG(NK) * TPIINV
        END IF
!
      IF( RGLGRD ) ALLOCATE ( FIELD(1-NY:NY*(NX+2)) )
!
!/SMC !!Li   Otherwise use sea point only field
!/SMC       ALLOCATE ( FIELD(NCel) )
!
      LOCAL   = IAPROC .LE. NAPROC
      UGDTUPDATE = .FALSE.
      IF (FLAGLL) THEN 
        FACX   =  1./(DERA * RADIUS) 
      ELSE 
        FACX   =  1.
        END IF
!
!/SBS      NDSOFLG = 99
!/MPI      SBSED = .FALSE.
!/SBS      SBSED = .TRUE.
!
      TAUWX  = 0.
      TAUWY  = 0.
!
! 0.d Test output
!
!/T      ILEN   = LEN_TRIM(FILEXT)
!/T      WRITE (NDST,9000) IMOD, FILEXT(:ILEN), TEND
!
! 1.  Check the consistency of the input ----------------------------- /
! 1.a Ending time versus initial time
!
      DTTST  = DSEC21 ( TIME , TEND )
!/DEBUGRUN      WRITE(740+IAPROC,*) '1 : DTTST=', DTTST, TIME, TEND
      FLZERO = DTTST .EQ. 0.
!/T      WRITE (NDST,9010) DTTST, FLZERO
      IF ( DTTST .LT. 0. ) THEN
          IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000)
          CALL EXTCDE ( 1 )
        END IF
!
! 1.b Water level time
!
      IF ( FLLEV ) THEN
          IF ( TLEV(1) .GE. 0. ) THEN
              DTL0   = DSEC21 ( TLEV , TLN )
            ELSE
              DTL0   = 1.
            END IF
!/T          WRITE (NDST,9011) DTL0
          IF ( DTL0 .LT. 0. ) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001)
              CALL EXTCDE ( 2 )
            END IF
        ELSE
          DTL0   = 0.
        END IF
!/DEBUGINIT      WRITE(740+IAPROC,*) 'W3WAVE, step 4'
!/DEBUGINIT      FLUSH(740+IAPROC)
!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3WAVEMD, step 4")
!
! 1.c Current interval
!
      IF ( FLCUR ) THEN
          DTTST1 = DSEC21 ( TC0 , TCN )
          DTTST2 = DSEC21 ( TC0 , TIME )
          DTTST3 = DSEC21 ( TEND , TCN )
!/T          WRITE (NDST,9012) DTTST1, DTTST2, DTTST3
          IF ( DTTST1.LT.0. .OR. DTTST2.LT.0. .OR. DTTST3.LT.0. ) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002)
              CALL EXTCDE ( 3 )
            END IF
          IF ( DTTST2.EQ.0..AND. ITIME.EQ.0 ) THEN
              IDACT(7:7) = 'F'
              TOFRST = TIME
            END IF
        END IF
!
! 1.d Wind interval
!
      IF ( FLWIND ) THEN
          DTTST1 = DSEC21 ( TW0 , TWN )
          DTTST2 = DSEC21 ( TW0 , TIME )
          DTTST3 = DSEC21 ( TEND , TWN )
!/T          WRITE (NDST,9013) DTTST1, DTTST2, DTTST3
          IF ( DTTST1.LT.0. .OR. DTTST2.LT.0. .OR. DTTST3.LT.0. ) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1003)
              CALL EXTCDE ( 4 )
            END IF
          IF ( DTTST2.EQ.0..AND. ITIME.EQ.0 ) THEN
              IDACT(3:3) = 'F'
              TOFRST = TIME
            END IF
        END IF
!/DEBUGINIT      WRITE(740+IAPROC,*) 'W3WAVE, step 5'
!/DEBUGINIT      FLUSH(740+IAPROC)
!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3WAVEMD, step 5")
!
! 1.e Ice concentration interval
!
      IF ( FLICE ) THEN
          IF ( TICE(1) .GE. 0 ) THEN
              DTI0   = DSEC21 ( TICE , TIN )
            ELSE
              DTI0   = 1.
            END IF
!/T          WRITE (NDST,9014) DTI0
          IF ( DTI0 .LT. 0. ) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1004)
              CALL EXTCDE ( 5 )
            END IF
        ELSE
          DTI0   = 0.
        END IF
!/DEBUGINIT      WRITE(740+IAPROC,*) 'W3WAVE, step 6'
!/DEBUGINIT      FLUSH(740+IAPROC)
!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3WAVEMD, step 6")
!
! 1.e Ice thickness interval
!
      IF ( FLIC1 ) THEN
          IF ( TIC1(1) .GE. 0 ) THEN
              DTI10   = DSEC21 ( TIC1 , TI1 )
            ELSE
              DTI10   = 1.
            END IF
!/T          WRITE (NDST,9015) DTI10
          IF ( DTI10 .LT. 0. ) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1005)
              CALL EXTCDE ( 5 )
            END IF
        ELSE
          DTI10   = 0.
        END IF
!
! 1.e Ice floe interval
!
!/IS2      IF ( FLIC5 ) THEN
!/IS2          IF ( TIC5(1) .GE. 0 ) THEN
!/IS2              DTI50   = DSEC21 ( TIC5 , TI5 )
!/IS2            ELSE
!/IS2              DTI50   = 1.
!/IS2            END IF
!/IS2!/T          WRITE (NDST,9016) DTI50
!/IS2          IF ( DTI50 .LT. 0. ) THEN
!/IS2              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1006)
!/IS2              CALL EXTCDE ( 5 )
!/IS2            END IF
!/IS2        ELSE
!/IS2          DTI50   = 0.
!/IS2        END IF
!
! 2.  Determine next time from ending and output --------------------- /
!     time and get corresponding time step.
!
      FLFRST = .TRUE.
      DO
!/DEBUGRUN        WRITE(740+IAPROC,*) 'First entry in the TIME LOOP'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/TIMINGS         CALL PRINT_MY_TIME("First entry in the TIME LOOP")
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.1'
!/DEBUGRUN        FLUSH(740+IAPROC)
!      DO JSEA = 1, NSEAL
!        DO IS = 1, NSPEC
!          IF (VA(IS, JSEA) .LT. 0.) THEN
!            WRITE(740+IAPROC,*) 'TEST W3WAVE 2', VA(IS,JSEA)
!            CALL FLUSH(740+IAPROC)
!          ENDIF
!        ENDDO
!      ENDDO
!      IF (SUM(VA) .NE. SUM(VA)) THEN
!        WRITE(740+IAPROC,*) 'NAN in ACTION 2', IX, IY, SUM(VA)
!        CALL FLUSH(740+IAPROC)
!        STOP
!      ENDIF


!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3WAVEMD, step 6.1")
!
!
! 2.a Pre-calculate table for IC3 ------------------------------------ /
!/IC3        USE_CHENG=IC3PARS(9)
!/IC3        IF( USE_CHENG==1.0 )THEN
!/IC3           FIXEDVISC=IC3PARS(14)
!/IC3           FIXEDDENS=IC3PARS(15)
!/IC3           FIXEDELAS=IC3PARS(16)
!/IC3           IF ( (FIXEDVISC.LT.0.0).OR.(FIXEDDENS.LT.0.0) .OR. &
!/IC3              (FIXEDELAS.LT.0.0) ) THEN
!/IC3               IF ( IAPROC .EQ. NAPERR )                          &
!/IC3               WRITE(NDSE,*)'Cheng method requires stationary',   &
!/IC3                            ' and uniform rheology from namelist.'
!/IC3               CALL EXTCDE(2)
!/IC3           END IF
!/IC3           IF (CALLEDIC3TABLE==0) THEN
!/IC3             CALL IC3TABLE_CHENG(FIXEDVISC,FIXEDDENS,FIXEDELAS)
!/IC3             CALLEDIC3TABLE = 1
!/IC3           ENDIF   
!/IC3        ENDIF

! 2.b Update group velocity and wavenumber from ice parameters ------- /
!     from W3SIC3MD module. ------------------------------------------ /
!     Note: "IF FLFRST" can be added for efficiency, but testing req'd

         JSEA=1 ! no switch (intentional)

!/IC3        USE_CGICE=IC3PARS(12)
!/IC3        IF ( USE_CGICE==1.0 ) THEN
!/IC3          IF ( IAPROC .EQ. NAPERR ) WRITE(SCREEN,920)

!/IC3          DO JSEA=1,NSEAL
!/DIST           ISEA   = IAPROC + (JSEA-1)*NAPROC
!/SHRD           ISEA   = JSEA
!/IC3            ALLOCATE(WN_I(SIZE(WN(:,ISEA))))
!/IC3            WN_I(:) = 0.
!/IC3            DEPTH  = MAX( DMIN , DW(ISEA) )
!/IC3            IX     = MAPSF(ISEA,1)
!/IC3            IY     = MAPSF(ISEA,2)

! 2.b.1 Using Cheng method: requires stationary/uniform rheology. 
!       However, ice thickness may be input by either method

!/IC3            IF ( USE_CHENG==1.0 ) THEN
!/IC3               IF (FLIC1) THEN
!/IC3                  HICE=ICEP1(IX,IY)
!/IC3               ELSEIF (IC3PARS(13).GE.0.0)THEN
!/IC3                  HICE=IC3PARS(13)
!/IC3               ELSE
!/IC3                  IF ( IAPROC .EQ. NAPERR )                       &
!/IC3                  WRITE(NDSE,*)'ICE THICKNESS NOT AVAILABLE ',    &
!/IC3                               'FOR CG CALC'
!/IC3                  CALL EXTCDE(2)
!/IC3               ENDIF
!/IC3               IF (HICE > 0.0) THEN ! non-zero ice
!/IC3                  CALL W3IC3WNCG_CHENG(WN(:,ISEA),WN_I(:),        &
!/IC3                    CG(:,ISEA),HICE,FIXEDVISC,                    &
!/IC3                    FIXEDDENS, FIXEDELAS, DEPTH)
!/IC3               END IF ! non-zero ice

!/IC3            ELSE ! not using Cheng method          
! 2.b.2 If not using Cheng method: require FLIC1 to FLIC4 (not strictly
!       necesssary, but makes code simpler)

!/IC3               IF (FLIC1.AND.FLIC2.AND.FLIC3.AND.FLIC4) THEN
!/IC3                  IF (ICEP1(IX,IY)>0.0) THEN ! non-zero ice
!/IC3                     CALL W3IC3WNCG_V1(WN(:,ISEA),WN_I(:),        &
!/IC3                       CG(:,ISEA),ICEP1(IX,IY),ICEP2(IX,IY),      &
!/IC3                       ICEP3(IX,IY),ICEP4(IX,IY),DEPTH)
!/IC3                  END IF ! non-zero ice
!/IC3               ELSE
!/IC3                  IF ( IAPROC .EQ. NAPERR )                       &
!/IC3                  WRITE(NDSE,*)'ICE PARAMETERS NOT AVAILABLE ',   &
!/IC3                               'FOR CG CALC'
!/IC3                  CALL EXTCDE(2)
!/IC3               END IF      
!/IC3            ENDIF ! IF USE_CHENG...

!/IC3            DEALLOCATE(WN_I)
!/IC3          END DO ! DO JSEA=1,NSEAL
!/IC3        END IF !  IF USE_CGICE ...
!
        IF ( TOFRST(1) .GT. 0 ) THEN
            DTTST  = DSEC21 ( TEND , TOFRST )
          ELSE
            DTTST  = 0.
          ENDIF
!/DEBUGRUN      WRITE(740+IAPROC,*) '2 : DTTST=', DTTST, TEND, TOFRST
!
        IF ( DTTST.GE.0. ) THEN
            TCALC = TEND
          ELSE
            TCALC = TOFRST
          END IF
!
        DTTST  = DSEC21 ( TIME , TCALC )
!/DEBUGRUN      WRITE(740+IAPROC,*) '3 : DTTST=', DTTST, TEND, TOFRST
        NT     = 1 + INT ( DTTST / DTMAX - 0.001 )
        DTGA   = DTTST / REAL(NT)
!/DEBUGRUN      WRITE(740+IAPROC,*) 'DTTST=', DTTST, ' NT=', NT
        IF ( DTTST .EQ. 0. ) THEN
            IT0    = 0
            IF ( .NOT.FLZERO ) ITIME  = ITIME - 1
            NT     = 0
          ELSE
            IT0    = 1
          END IF

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!
!/T        WRITE (NDST,9020) IT0, NT, DTGA
!
! ==================================================================== /
!
! 3.  Loop over time steps
!
        DTRES  = 0.

!/DEBUGRUN      DO JSEA = 1, NSEAL
!/DEBUGRUN        DO IS = 1, NSPEC
!/DEBUGRUN          IF (VA(IS, JSEA) .LT. 0.) THEN
!/DEBUGRUN            WRITE(740+IAPROC,*) 'TEST W3WAVE 3', VA(IS,JSEA)
!/DEBUGRUN            CALL FLUSH(740+IAPROC)
!/DEBUGRUN          ENDIF
!/DEBUGRUN        ENDDO
!/DEBUGRUN      ENDDO
!/DEBUGRUN      IF (SUM(VA) .NE. SUM(VA)) THEN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'NAN in ACTION 3', IX, IY, SUM(VA)
!/DEBUGRUN        CALL FLUSH(740+IAPROC)
!/DEBUGRUN        STOP
!/DEBUGRUN      ENDIF
!/DEBUGRUN        WRITE(740+IAPROC,*) 'IT0=', IT0, ' NT=', NT
!/DEBUGRUN        FLUSH(740+IAPROC)
!
        DO IT=IT0, NT
!/TIMINGS         CALL PRINT_MY_TIME("Begin of IT loop")
!/SETUP     CALL WAVE_SETUP_COMPUTATION
! copy old values 
!/PDLIB     DO IP=1,NSEAL
!/PDLIB       DO ISPEC=1,NSPEC
!/PDLIB         VAOLD(ISPEC,IP)=VA(ISPEC,IP)
!/PDLIB       END DO
!/PDLIB     END DO
!
!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "Beginning time loop")
!/TIMINGS         CALL PRINT_MY_TIME("After assigning VAOLD")
!
!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 0'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)
!
          ITIME  = ITIME + 1
!
          DTG    = REAL(NINT(DTGA+DTRES+0.0001))
          DTRES  = DTRES + DTGA - DTG
          IF ( ABS(DTRES) .LT. 0.001 ) DTRES  = 0.
          CALL TICK21 ( TIME , DTG )
!
!/DEBUGRUN      WRITE(740+IAPROC,*) 'DTGA=', DTGA, ' DTRES=', DTRES
!/DEBUGRUN      WRITE(740+IAPROC,*) 'DTG 1 : DTG=', DTG
!/DEBUGRUN      FLUSH(740+IAPROC)
!
!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 1'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

          IF ( TSTAMP .AND. SCREEN.NE.NDSO .AND. IAPROC.EQ.NAPOUT ) THEN
              CALL WWTIME ( STTIME )
              CALL STME21 ( TIME , IDTIME )
              WRITE (SCREEN,950) IDTIME, STTIME
            END IF

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 2'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!/DEBUGRUN      DO JSEA = 1, NSEAL
!/DEBUGRUN        DO IS = 1, NSPEC
!/DEBUGRUN          IF (VA(IS, JSEA) .LT. 0.) THEN
!/DEBUGRUN            WRITE(740+IAPROC,*) 'TEST W3WAVE 4', VA(IS,JSEA)
!/DEBUGRUN            CALL FLUSH(740+IAPROC)
!/DEBUGRUN          ENDIF
!/DEBUGRUN        ENDDO
!/DEBUGRUN      ENDDO
!/DEBUGRUN      IF (SUM(VA) .NE. SUM(VA)) THEN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'NAN in ACTION 4', IX, IY, SUM(VA)
!/DEBUGRUN        CALL FLUSH(740+IAPROC)
!/DEBUGRUN        STOP
!/DEBUGRUN      ENDIF
!
          VGX = 0.
          VGY = 0. 
          IF(INFLAGS1(8)) THEN
              DTTST1 = DSEC21 ( TIME, TGN ) 
              DTTST2 = DSEC21 ( TG0, TGN )
              FAC    = DTTST1 / MAX ( 1. , DTTST2 )
              VGX    = (FAC*GA0+(1.-FAC)*GAN) *                       &
                            COS(FAC*GD0+(1.-FAC)*GDN)
              VGY    = (FAC*GA0+(1.-FAC)*GAN) *                       &
                            SIN(FAC*GD0+(1.-FAC)*GDN)
            END IF
!/TIMINGS         CALL PRINT_MY_TIME("After VGX/VGY assignation")
!
!/T        WRITE (NDST,9021) ITIME, IT, TIME, FLMAP, FLDDIR,          &
!/T                          VGX, VGY, DTG, DTRES
!/DEBUGSRC      WRITE(740+IAPROC,*) 'DTG 2 : DTG=', DTG
!/DEBUGSRC      WRITE(740+IAPROC,*) 'max(UST)=', maxval(UST)
!/DEBUGSRC      FLUSH(740+IAPROC)
!
! 3.1 Interpolate winds and currents.
!     (Initialize wave fields with winds)
!
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLCUR=', FLCUR
!/DEBUGRUN        FLUSH(740+IAPROC)
!/DEBUGDCXDX       WRITE(740+IAPROC,*) 'Debug DCXDX FLCUR=', FLCUR
!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 3a '
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

          IF ( FLCUR  ) THEN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.4'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "Before UCUR")
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.4.1'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/TIMINGS         CALL PRINT_MY_TIME("W3WAVE, step 6.4.1")

!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.4.2 before W3UCUR'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.4.1 after W3UCUR'
!/DEBUGRUN        FLUSH(740+IAPROC)

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 3b '
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)
            IF (GTYPE .NE. UNGTYPE) THEN       
              IF( RGLGRD ) THEN
              CALL W3DZXY(CX(1:UBOUND(CX,1)),'m/s',DCXDX, DCXDY) !CX GRADIENT
              CALL W3DZXY(CY(1:UBOUND(CY,1)),'m/s',DCYDX, DCYDY) !CY GRADIENT
              ENDIF
!/SMC !!Li  Use new sub for DCXDX/Y and DCYDX/Y assignment.  
!/SMC         CALL SMCDCXY 
            ELSE
!/DEBUGDCXDX       WRITE(740+IAPROC,*) 'Before call to UG_GRADIENT for assigning DCXDX/DCXDY array'
              CALL UG_GRADIENTS(CX, DCXDX, DCXDY)
              CALL UG_GRADIENTS(CY, DCYDX, DCYDY)
              CALL GET_INTERFACE 
              UGDTUPDATE=.TRUE.
              CFLXYMAX = 0.
            END IF

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 4'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)
!
            ELSE IF ( FLFRST ) THEN
              UGDTUPDATE=.TRUE.
              CFLXYMAX = 0.
              CX = 0.
              CY = 0.
              END IF ! FLCUR 
!/TIMINGS         CALL PRINT_MY_TIME("After CX/CY assignation")
!
!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 5'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

          IF ( FLWIND ) THEN
            IF ( FLFRST ) ASF = 1.
            CALL W3UWND ( FLFRST, VGX, VGY )
          ELSE IF ( FLFRST ) THEN
            U10    = 0.01
            U10D   = 0.
            UST    = 0.05
            USTDIR = 0.05
          END IF

!      DO JSEA = 1, NSEAL
!        DO IS = 1, NSPEC
!          IF (VA(IS, JSEA) .LT. 0.) THEN
!            WRITE(740+IAPROC,*) 'TEST W3WAVE 5', VA(IS,JSEA)
!            CALL FLUSH(740+IAPROC)
!          ENDIF
!        ENDDO
!      ENDDO
!      IF (SUM(VA) .NE. SUM(VA)) THEN
!        WRITE(740+IAPROC,*) 'NAN in ACTION 5', IX, IY, SUM(VA)
!        CALL FLUSH(740+IAPROC)
!        STOP
!      ENDIF

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 6'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!/TIMINGS         CALL PRINT_MY_TIME("After U10, etc. assignation")
!
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.5'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "Before call to W3UINI")
!/TIMINGS         CALL PRINT_MY_TIME("Before call W3UINI")
          IF ( FLIWND .AND. LOCAL ) CALL W3UINI ( VA )
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.5.1 DTG=', DTG
!/DEBUGRUN        FLUSH(740+IAPROC)
!
! 3.2 Update boundary conditions if boundary flag is true (FLBPI)
!
!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "Before boundary update")
!/TIMINGS         CALL PRINT_MY_TIME("Before boundary update")
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLBPI=', FLBPI
!/DEBUGRUN        WRITE(740+IAPROC,*) 'LOCAL=', LOCAL
!/DEBUGRUN        FLUSH(740+IAPROC)
!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 7'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

          IF ( FLBPI .AND. LOCAL ) THEN
!
              DO
                IF ( TBPIN(1) .EQ. -1 ) THEN
                    READBC = .TRUE.
                    IDACT(1:1) = 'F'
                ELSE
                    READBC = DSEC21(TIME,TBPIN).LT.0.
                    IF (READBC.AND.IDACT(1:1).EQ.' ') IDACT(1:1) = 'X'
                END IF
                FLACT  = READBC .OR. FLACT
!/DEBUGIOBC     WRITE(740+IAPROC,*) 'READBC=', READBC
!/DEBUGIOBC     FLUSH(740+IAPROC)

                IF ( READBC ) THEN
!/DEBUGIOBC       WRITE(740+IAPROC,*) 'Before call to W3IOBC'
!/DEBUGIOBC       FLUSH(740+IAPROC)
                  CALL W3IOBC ( 'READ', NDS(9), TBPI0, TBPIN,       &
                                ITEST, IMOD )
!/DEBUGIOBC       WRITE(740+IAPROC,*) 'After call to W3IOBC'
!/DEBUGIOBC       WRITE(740+IAPROC,*) 'ITEST=', ITEST
!/DEBUGIOBC       FLUSH(740+IAPROC)
                  IF ( ITEST .NE. 1 ) CALL W3UBPT 
                ELSE
                  ITEST  = 0
                END IF
                IF ( ITEST .LT. 0 ) IDACT(1:1) = 'L'
                IF ( ITEST .GT. 0 ) IDACT(1:1) = ' '
                IF ( .NOT. (READBC.AND.FLBPI) ) EXIT
                END DO

          END IF

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 7'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)


!/PDLIB          CALL APPLY_BOUNDARY_CONDITION_VA
!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "After FLBPI and LOCAL")
!/TIMINGS         CALL PRINT_MY_TIME("After FLBPI and LOCAL")

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 8'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!
! 3.3.1 Update ice coverage (if new ice map).
!     Need to be run on output nodes too, to update MAPSTx
!
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLICE=', FLICE
!/DEBUGRUN        WRITE(740+IAPROC,*) 'DTI0=', DTI0
!/DEBUGRUN        FLUSH(740+IAPROC)
          IF ( FLICE .AND. DTI0.NE.0. ) THEN
!
              IF ( TICE(1).GE.0 ) THEN
                  IF ( DTI0 .LT. 0. ) THEN
                      IDACT(9:9) = 'B'
                    ELSE
                      DTTST  = DSEC21 ( TIME, TIN )
                      IF ( DTTST .LE. 0.5*DTI0 ) IDACT(9:9) = 'U'
                    END IF
                ELSE
                  IDACT(9:9) = 'I'
                END IF
!
              IF ( IDACT(9:9).NE.' ' ) THEN
                  CALL W3UICE ( VA, VA )
                  DTI0   = 0.
                  FLACT  = .TRUE.
                  FLMAP  = .TRUE.
              END IF
          END IF
!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "After FLICE and DTI0")
!/TIMINGS         CALL PRINT_MY_TIME("After FLICE and DTI0")
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.7 DTG=', DTG
!/DEBUGRUN        FLUSH(740+IAPROC)
!/PDLIB!/DEBUGIOBP         IF (NX .ge. 10210) WRITE(*,*) 'Before W3ULEV:', MAPSTA(1,10210), IOBP(10210)

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 9'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)
!
! 3.3.2 Update ice thickness
!
          IF ( FLIC1 .AND. DTI10.NE.0. ) THEN
!
              IF ( TIC1(1).GE.0 ) THEN
                  IF ( DTI10 .LT. 0. ) THEN
                      IDACT(11:11) = 'B'
                    ELSE
                      DTTST  = DSEC21 ( TIME, TI1 )
                      IF ( DTTST .LE. 0.5*DTI10 ) IDACT(11:11) = 'U'
                    END IF
                ELSE
                  IDACT(11:11) = 'I'
                END IF
  
!
              IF ( IDACT(11:11).NE.' ' ) THEN
                  CALL W3UIC1 ( FLFRST )
                  DTI10   = 0.
                  FLACT  = .TRUE.
                  FLMAP  = .TRUE.
                END IF
!
            END IF

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 10'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!
! 3.3.3 Update ice floe diameter
!
!/IS2          IF ( FLIC5 .AND. DTI50.NE.0. ) THEN
!
!/IS2              IF ( TIC5(1).GE.0 ) THEN
!/IS2                  IF ( DTI50 .LT. 0. ) THEN
!/IS2                      IDACT(14:14) = 'B'
!/IS2                    ELSE
!/IS2                      DTTST  = DSEC21 ( TIME, TI5 )
!/IS2                      IF ( DTTST .LE. 0.5*DTI50 ) IDACT(14:14) = 'U'
!/IS2                    END IF
!/IS2                ELSE
!/IS2                  IDACT(14:14) = 'I'
!/IS2                END IF
!
!/IS2              IF ( IDACT(14:14).NE.' ' ) THEN
!/IS2               CALL W3UIC5( FLFRST )
!/IS2                  DTI50   = 0.
!/IS2                  FLACT  = .TRUE.
!/IS2                  FLMAP  = .TRUE.
!/IS2                END IF
!
!/IS2            END IF

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 11a'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)
!
! 3.4 Transform grid (if new water level).
!
!          write(740+IAPROC,*) 'TEST ARON', FLLEV, DTL0, TLEV(1), IDACT(5:5), DSEC21 ( TIME, TLN ), TIME, TLN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLLEV=', FLLEV, ' DTL0=', DTL0
!/DEBUGRUN        FLUSH(740+IAPROC)
          IF ( FLLEV .AND. DTL0 .NE.0. ) THEN
!
!/DEBUGRUN        WRITE(740+IAPROC,*) 'Before time works'
!/DEBUGRUN        FLUSH(740+IAPROC)
              IF ( TLEV(1) .GE. 0 ) THEN
                  IF ( DTL0 .LT. 0. ) THEN
                      IDACT(5:5) = 'B'
                    ELSE
                      DTTST  = DSEC21 ( TIME, TLN )
                      IF ( DTTST .LE. 0.5*DTL0 ) IDACT(5:5) = 'U'
                    END IF
                ELSE
                  IDACT(5:5) = 'I'
                END IF
!/DEBUGRUN        WRITE(740+IAPROC,*) 'After time works'
!/DEBUGRUN        FLUSH(740+IAPROC)
!
              IF ( IDACT(5:5).NE.' ' ) THEN

!/DEBUGRUN        WRITE(740+IAPROC,*) 'Before W3ULEV'
!/DEBUGRUN        FLUSH(740+IAPROC)
                  CALL W3ULEV ( VA, VA )
!/DEBUGRUN        WRITE(740+IAPROC,*) 'After W3ULEV'
!/DEBUGRUN        FLUSH(740+IAPROC)

                  UGDTUPDATE=.TRUE.
                  CFLXYMAX = 0.
                  DTL0   = 0.
                  FLACT  = .TRUE.
                  FLMAP  = .TRUE.
                  FLDDIR = FLDDIR .OR.  FLCTH .OR. FSREFRACTION        &
                        .OR. FLCK .OR. FSFREQSHIFT
              END IF
!/DEBUGRUN        WRITE(740+IAPROC,*) 'After IDACT if test'
!/DEBUGRUN        FLUSH(740+IAPROC)
          END IF
!/DEBUGRUN        WRITE(740+IAPROC,*) 'After FLLEV test'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "After FFLEV and DTL0")
!/PDLIB!/DEBUGIOBP         IF (NX .ge. 10210) WRITE(*,*) ' After W3ULEV:', MAPSTA(1,10210), IOBP(10210)
!/TIMINGS         CALL PRINT_MY_TIME("After FFLEV and DTL0")
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLMAP=', FLMAP
!/DEBUGRUN        FLUSH(740+IAPROC)

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 11b'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!
! 3.5 Update maps and derivatives.
!
          IF ( FLMAP .AND. RGLGRD ) THEN
!/PR1              CALL W3MAP1 ( MAPSTA )
!/PR2              CALL W3MAP2
!/PR3              CALL W3MAP3
!/PRX              CALL W3MAPX
              CALL W3UTRN ( TRNX, TRNY )
!/PR3              CALL W3MAPT
              CALL W3NMIN ( MAPSTA, FLAG0 )
              IF ( FLAG0 .AND. IAPROC.EQ.NAPERR ) WRITE (NDSE,1030) IMOD
              FLMAP  = .FALSE.
          END IF
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.1 DTG=', DTG
!/DEBUGRUN        FLUSH(740+IAPROC)
!
!/SMC  !!Li   The sea point nubmer per pe is also checked for SMC grid.
!/SMC          IF ( FLMAP ) THEN
!/SMC              CALL W3NMIN ( MAPSTA, FLAG0 )
!/SMC              IF ( FLAG0 .AND. IAPROC.EQ.NAPERR ) WRITE (NDSE,1030) IMOD
!/SMC              FLMAP  = .FALSE.
!/SMC            END IF
!
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.2 DTG=', DTG
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLDDIR=', FLDDIR
!/DEBUGRUN        FLUSH(740+IAPROC)
          IF ( FLDDIR ) THEN
            IF (GTYPE .NE. UNGTYPE) THEN  
!!Li          CALL W3DZXY(DW(1:NSEA),'m',DDDX,DDDY) !DEPTH (DW) GRADIENT
              IF( RGLGRD ) CALL W3DZXY(DW(1:UBOUND(DW,1)),'m',DDDX,DDDY) 
!/SMC !!Li  Use new sub for DDDX and DDDY assignment.  
!/SMC         CALL SMCDHXY 
!/SMCT        WRITE (NDST,*) " * SMCDHXY completed IT DTG =", IT, DTG
!
            ELSE
              CALL UG_GRADIENTS(DW, DDDX, DDDY)
              END IF 
              FLDDIR = .FALSE.
          END IF

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 12'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.3 DTG=', DTG
!/DEBUGRUN        FLUSH(740+IAPROC)
!
!         Calculate PHASE SPEED GRADIENT.
          DCDX = 0.
          DCDY = 0.
!/REFRX   CIK  = 0.
!/REFRX!
!/REFRX          IF (GTYPE .NE. UNGTYPE) THEN
!/REFRX            DO IK=0,NK+1
!/REFRX               CIK = SIG(IK) / WN(IK,1:NSEA)
!/REFRX               CALL W3DZXY(CIK,'m/s',DCDX(IK,:,:),DCDY(IK,:,:))
!/REFRX            END DO
!/REFRX          ELSE
!/REFRX            WRITE (NDSE,1040)
!/REFRX            CALL EXTCDE(2)
!/REFRX     !      CALL UG_GRADIENTS(CMN, DCDX, DCDY) !/ Stefan, to be confirmed!
!/REFRX          END IF
!
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.4'
!/DEBUGRUN        FLUSH(740+IAPROC)
!
          FLIWND = .FALSE.
          FLFRST = .FALSE.
!
!/PDLIB!/DEBUGSRC          WRITE(740+IAPROC,*) 'ITIME=', ITIME, ' IT=', IT
!/PDLIB!/DEBUGSRC          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA before W3SRCE_IMP_PRE")
!/PDLIB!/DEBUGSRC          CALL ALL_FIELD_INTEGRAL_PRINT(VSTOT, "VSTOT before W3SRCE_IMP_PRE")
!/PDLIB!/DEBUGSRC          CALL ALL_FIELD_INTEGRAL_PRINT(VDTOT, "VDTOT before W3SRCE_IMP_PRE")
!/PDLIB!/DEBUGSRC          IF (DEBUG_NODE .le. NSEAL) THEN
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     Values for DEBUG_NODE=', DEBUG_NODE
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     sum(VA)=', sum(VA(:,DEBUG_NODE))
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     sum(VSTOT)=', sum(VSTOT(:,DEBUG_NODE))
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     sum(VDTOT)=', sum(VDTOT(:,DEBUG_NODE))
!/PDLIB!/DEBUGSRC          END IF
!/PDLIB       IF (IT .eq. 0) THEN
!/PDLIB         DTGpre = 1.
!/PDLIB       ELSE
!/PDLIB         DTGpre = DTG
!/PDLIB       END IF

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 13'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)
!
        IF ( FLSOU .and. LPDLIB) THEN
!
!/OMP0/!$OMP PARALLEL DO PRIVATE (JSEA,ISEA,IX,IY) SCHEDULE (DYNAMIC,1)
          D50=0.0002
          REFLEC(:)=0.
          REFLED(:)=0
          PSIC=0.
!/PDLIB          VSTOT = 0.
!/PDLIB          VDTOT = 0.

          DO JSEA=1, NSEAL
            CALL INIT_GET_ISEA(ISEA, JSEA)
            IX     = MAPSF(ISEA,1)
            IY     = MAPSF(ISEA,2)
            DELA=1. 
            DELX=1. 
            DELY=1. 
!/REF1                IF (GTYPE.EQ.RLGTYPE) THEN 
!/REF1                  DELX=SX*CLATS(ISEA)/FACX
!/REF1                  DELY=SY/FACX
!/REF1                  DELA=DELX*DELY
!/REF1                END IF
!/REF1                IF (GTYPE.EQ.CLGTYPE) THEN 
!/REF1! Maybe what follows works also for RLGTYPE ... to be verified
!/REF1                  DELX=HPFAC(IY,IX)/ FACX
!/REF1                  DELY=HQFAC(IY,IX)/ FACX 
!/REF1                  DELA=DELX*DELY
!/REF1                END IF
!
!/REF1       REFLEC=REFLC(:,ISEA)
!/REF1       REFLEC(4)=BERG(ISEA)*REFLEC(4)
!/REF1       REFLED=REFLD(:,ISEA)
!/BT4        D50=SED_D50(ISEA)
!/BT4        PSIC=SED_PSIC(ISEA)
!/REF1       REFLEC=REFLC(:,ISEA)
!/REF1       REFLEC(4)=BERG(ISEA)*REFLEC(4)
!/REF1       REFLED=REFLD(:,ISEA)
!/BT4        D50=SED_D50(ISEA)
!/BT4        PSIC=SED_PSIC(ISEA)
!
!/DEBUGRUN        DO IS = 1, NSPEC
!/DEBUGRUN          IF (VA(IS, JSEA) .LT. 0.) THEN
!/DEBUGRUN            WRITE(740+IAPROC,*) 'TEST W3WAVE 7', VA(IS,JSEA)
!/DEBUGRUN            CALL FLUSH(740+IAPROC)
!/DEBUGRUN          ENDIF
!/DEBUGRUN        ENDDO
!
            IF ( MAPSTA(IY,IX) .EQ. 1 .AND. FLAGST(ISEA)) THEN
              IF (FSSOURCE) THEN
!/PDLIB!/DEBUGSRC         IF (IX .eq. DEBUG_NODE) THEN
!/PDLIB!/DEBUGSRC           WRITE(740+IAPROC,*) 'NODE_SRCE_IMP_PRE : IX=', IX, ' JSEA=', JSEA
!/PDLIB!/DEBUGSRC         END IF
!/PDLIB!/DEBUGSRC      WRITE(740+IAPROC,*) 'IT/IX/IY/IMOD=', IT, IX, IY, IMOD
!/PDLIB!/DEBUGSRC      WRITE(740+IAPROC,*) 'ISEA/JSEA=', ISEA, JSEA
!/PDLIB!/DEBUGSRC      WRITE(740+IAPROC,*) 'Before sum(VA)=', sum(VA(:,JSEA))
!/PDLIB!/DEBUGSRC      FLUSH(740+IAPROC)
!/PDLIB                CALL W3SRCE(srce_imp_pre, IT, JSEA, IX, IY, IMOD, &
!/PDLIB                   VAoldDummy, VA(:,JSEA),                        &
!/PDLIB                   VSTOT(:,JSEA), VDTOT(:,JSEA), SHAVETOT(JSEA),  &
!/PDLIB                   ALPHA(1:NK,JSEA), WN(1:NK,ISEA),               &
!/PDLIB                   CG(1:NK,ISEA), DW(ISEA), U10(ISEA),            &
!/PDLIB                   U10D(ISEA), AS(ISEA), UST(ISEA),               &
!/PDLIB                   USTDIR(ISEA), CX(ISEA), CY(ISEA),              &
!/PDLIB                   ICE(ISEA), ICEH(ISEA), ICEF(ISEA),             &
!/PDLIB                   ICEDMAX(ISEA),                                 &
!/PDLIB                   REFLEC, REFLED, DELX, DELY, DELA,              &
!/PDLIB                   TRNX(IY,IX), TRNY(IY,IX), BERG(ISEA),          &
!/PDLIB                   FPIS(ISEA), DTDYN(JSEA),                       &
!/PDLIB                   FCUT(JSEA), DTGpre, TAUWX(JSEA), TAUWY(JSEA),  &
!/PDLIB                   TAUOX(JSEA), TAUOY(JSEA), TAUWIX(JSEA),        &
!/PDLIB                   TAUWIY(JSEA), TAUWNX(JSEA),                    &
!/PDLIB                   TAUWNY(JSEA),  PHIAW(JSEA), CHARN(JSEA),       &
!/PDLIB                   TWS(JSEA), PHIOC(JSEA), TMP1, D50, PSIC, TMP2, &
!/PDLIB                   PHIBBL(JSEA), TMP3, TMP4, PHICE(JSEA),         &
!/PDLIB                   ASF(ISEA))
!/PDLIB!/DEBUGSRC      WRITE(740+IAPROC,*) 'After sum(VA)=', sum(VA(:,JSEA))
!/PDLIB!/DEBUGSRC      WRITE(740+IAPROC,*) '   sum(VSTOT)=', sum(VSTOT(:,JSEA))
!/PDLIB!/DEBUGSRC      WRITE(740+IAPROC,*) '   sum(VDTOT)=', sum(VDTOT(:,JSEA))
!/PDLIB!/DEBUGSRC      WRITE(740+IAPROC,*) '     SHAVETOT=', SHAVETOT(JSEA)
!/PDLIB!/DEBUGSRC      FLUSH(740+IAPROC)
              ENDIF
            ELSE
              UST   (ISEA) = UNDEF
              USTDIR(ISEA) = UNDEF
              DTDYN (JSEA) = UNDEF
              FCUT  (JSEA) = UNDEF
            END IF
          END DO ! JSEA 
        END IF ! PDLIB 
!/PDLIB!/DEBUGSRC          WRITE(740+IAPROC,*) 'ITIME=', ITIME, ' IT=', IT
!/PDLIB!/DEBUGSRC          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA after W3SRCE_IMP_PRE")
!/PDLIB!/DEBUGSRC          CALL ALL_FIELD_INTEGRAL_PRINT(VSTOT, "VSTOT after W3SRCE_IMP_PRE")
!/PDLIB!/DEBUGSRC          CALL ALL_FIELD_INTEGRAL_PRINT(VDTOT, "VDTOT after W3SRCE_IMP_PRE")
!/PDLIB!/DEBUGSRC          IF (DEBUG_NODE .le. NSEAL) THEN
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     Values for DEBUG_NODE=', DEBUG_NODE
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     sum(VA)=', sum(VA(:,DEBUG_NODE))
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     sum(VSTOT)=', sum(VSTOT(:,DEBUG_NODE))
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     sum(VDTOT)=', sum(VDTOT(:,DEBUG_NODE))
!/PDLIB!/DEBUGSRC          END IF

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 14'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

          IF ( FLZERO ) THEN
!/T              WRITE (NDST,9022)
              GOTO 400
            END IF
          IF ( IT.EQ.0 ) THEN 
            DTG = 1.
!            DTG = 60.
            GOTO 370
          END IF
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.5'
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLDRY=', FLDRY
!/DEBUGRUN        FLUSH(740+IAPROC)
          IF ( FLDRY .OR. IAPROC.GT.NAPROC ) THEN
!/T              WRITE (NDST,9023)
!/DEBUGRUN              WRITE(740+IAPROC,*) 'Jump to 380'
!/DEBUGRUN              FLUSH(740+IAPROC)
              GOTO 380
          END IF
!
! Estimation of the local maximum CFL for XY propagation
!
!/T           WRITE(NDSE,*) 'Computing CFLs .... ',NSEAL
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLOGRD(9,3) = ', FLOGRD(9,3)
!/DEBUGRUN        WRITE(740+IAPROC,*) 'UGDTUPDATE=', UGDTUPDATE
!/DEBUGRUN        FLUSH(740+IAPROC)
                IF ( FLOGRD(9,3).AND. UGDTUPDATE ) THEN 
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.6'
!/DEBUGRUN        FLUSH(740+IAPROC)
                  IF (FSTOTALIMP .eqv. .FALSE.) THEN
                      NKCFL=NK
!/T                   NKCFL=1
!
!/OMPG/!$OMP PARALLEL DO PRIVATE (JSEA,ISEA) SCHEDULE (DYNAMIC,1)
!
                      DO JSEA=1, NSEAL
                        CALL INIT_GET_ISEA(ISEA, JSEA)
!/PR3                      IF (GTYPE .EQ. UNGTYPE) THEN
!/PR3                        IF ( FLOGRD(9,3) ) THEN
!/T                          IF (MOD(ISEA,100).EQ.0) WRITE(NDSE,*) 'COMPUTING CFL FOR NODE:',ISEA
!/PDLIB                      IF (.NOT. LPDLIB) THEN
!/PR3                          CALL W3CFLUG ( ISEA, NKCFL, FACX, FACX, DTG,  &
!/PR3                                         MAPFS,  CFLXYMAX(JSEA), VGX, VGY )
!/PDLIB                      ENDIF
!/PR3                        END IF
!/PR3                      ELSE 
!/PR3                        CALL W3CFLXY ( ISEA, DTG, MAPSTA, MAPFS,      &
!/PR3                                       CFLXYMAX(JSEA), VGX, VGY )
!/PR3                      END IF
                      END DO
!
!/OMPG/!$OMP END PARALLEL DO
!
                    END IF
                END IF
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.7'
!/DEBUGRUN        FLUSH(740+IAPROC)

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 15'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)
!
!/DEBUGRUN      DO JSEA = 1, NSEAL
!/DEBUGRUN        DO IS = 1, NSPEC
!/DEBUGRUN          IF (VA(IS, JSEA) .LT. 0.) THEN
!/DEBUGRUN            WRITE(740+IAPROC,*) 'TEST W3WAVE 8', VA(IS,JSEA)
!/DEBUGRUN            CALL FLUSH(740+IAPROC)
!/DEBUGRUN          ENDIF
!/DEBUGRUN        ENDDO
!/DEBUGRUN      ENDDO
!/DEBUGRUN      IF (SUM(VA) .NE. SUM(VA)) THEN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'NAN in ACTION 6 ', IX, IY, SUM(VA)
!/DEBUGRUN        CALL FLUSH(740+IAPROC)
!/DEBUGRUN        STOP
!/DEBUGRUN      ENDIF

!
!/T       IF (GTYPE .EQ. UNGTYPE) THEN 
!/T         IF ( FLOGRD(9,3) ) THEN
!/T           DTCFL1(:)=1.
!/T           DO JSEA=1,NSEAL
!/T             INDSORT(JSEA)=FLOAT(JSEA)
!/T             DTCFL1(JSEA)=DTG/CFLXYMAX(JSEA)
!/T           END DO
!/T           CALL SSORT1 (DTCFL1, INDSORT, NSEAL, 2)
!/T           IF ( IAPROC .EQ. NAPERR ) WRITE(NDSE,*) 'Nodes requesting smallest timesteps:'
!/T           IF ( IAPROC .EQ. NAPERR ) WRITE(NDSE,'(A,10I10)')   'Nodes      ',NINT(INDSORT(1:10))
!/T           IF ( IAPROC .EQ. NAPERR ) WRITE(NDSE,'(A,10F10.2)') 'time steps ',DTCFL1(1:10)
!/T           DO JSEA = 1, MIN(NSEAL,200) 
!/T             ISEA   = NINT(INDSORT(JSEA))            ! will not work with MPI
!/T             IX     = MAPSF(ISEA,1)
!/T             IF (JSEA.EQ.1) &
!/T               WRITE(995,*) '       IP  dtmax_exp(ip)        x-coord        y-coord        z-coord'
!/T             WRITE(995,'(I10,F10.2,3F10.4)') IX,  DTCFL1(JSEA), XYB(IX,1), XYB(IX,2), XYB(IX,3)  
!/T           END DO ! JSEA
!/T           CLOSE(995)
!/T         END IF
!/T       END IF

!
! 3.6 Perform Propagation = = = = = = = = = = = = = = = = = = = = = = =
! 3.6.1 Preparations
!
!/SEC1      DTGTEMP=DTG
!/SEC1      DTG=DTG/NITERSEC1
!/SEC1      DO ISEC1=1,NITERSEC1
          NTLOC  = 1 + INT( DTG/DTCFLI - 0.001 )
!/SEC1    IF ( IAPROC .EQ. NAPOUT )    WRITE(NDSE,'(A,I4,A,I4)') '   SUBSECOND STEP:',ISEC1,' out of ',NITERSEC1
!
          FACTH  = DTG / (DTH*REAL(NTLOC))
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, DTCFLI=', DTCFLI
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, DTG=', DTG
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, DTH=', DTH
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, NTLOC=', NTLOC
!/DEBUGRUN        FLUSH(740+IAPROC)
          FACK   = DTG / REAL(NTLOC)

          TTEST(1) = TIME(1)
          TTEST(2) = 0
          DTTEST = DSEC21(TTEST,TIME)
          ITLOCH = ( NTLOC + 1 - MOD(NINT(DTTEST/DTG),2) ) / 2
!
! 3.6.2 Intra-spectral part 1
!
!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "Before intraspectral part 1")
!/TIMINGS         CALL PRINT_MY_TIME("Before intraspectral")
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.10'
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLCTH=', FLCTH, ' FLCK=', FLCK
!/DEBUGRUN        FLUSH(740+IAPROC)
          IF ( FLCTH .OR. FLCK ) THEN
              DO ITLOC=1, ITLOCH
!
!/OMPG/!$OMP PARALLEL PRIVATE (JSEA,ISEA,IX,IY,DEPTH,IXrel)
!/OMPG/!$OMP DO SCHEDULE (DYNAMIC,1)
!
!/DEBUGRUN                WRITE(740+IAPROC,*) ' ITLOC=', ITLOC
!/DEBUGRUN                WRITE(740+IAPROC,*) ' 1: Before call to W3KTP1 / W3KTP2 / W3KTP3'
                DO JSEA=1, NSEAL
                  CALL INIT_GET_ISEA(ISEA, JSEA)
                  IX     = MAPSF(ISEA,1)
                  IY     = MAPSF(ISEA,2)

!/DEBUGRUN                  IF (JSEA == DEBUG_NODE) WRITE(*,*) 'W3WAVE TEST', SUM(VA(:,JSEA))
                  IF ( GTYPE .EQ. UNGTYPE ) THEN
                    IF (IOBP(ISEA) .NE. 1) CYCLE
                  ENDIF

                  IF ( MAPSTA(IY,IX) .EQ. 1 ) THEN
                           DEPTH  = MAX ( DMIN , DW(ISEA) )
                           IF (LPDLIB) THEN
                             IXrel = JSEA
                           ELSE
                             IXrel = IX
                           END IF
!/PR1                      CALL W3KTP1 ( ISEA, FACTH, FACK, CTHG0S(ISEA), &
!/PR1                           CG(:,ISEA), WN(:,ISEA), DEPTH,            &
!/PR1                           DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
!/PR1                           CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
!/PR1                           DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
!/PR1                           DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA))
!/PR2                      CALL W3KTP2 ( ISEA, FACTH, FACK, CTHG0S(ISEA), &
!/PR2                           CG(:,ISEA), WN(:,ISEA), DEPTH,            &
!/PR2                           DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
!/PR2                           CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
!/PR2                           DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
!/PR2                           DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA))
!/PR3                      CALL W3KTP3 ( ISEA, FACTH, FACK, CTHG0S(ISEA), &
!/PR3                           CG(:,ISEA), WN(:,ISEA), DEPTH,            &
!/PR3                           DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
!/PR3                           CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
!/PR3                           DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
!/PR3                           DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA), &
!/PR3                           CFLTHMAX(JSEA), CFLKMAX(JSEA) )  
!/PRX                      CALL W3KTPX
!
!/SMC  !!Li    Refraction and GCT in theta direction is done by rotation.
!/SMC                      CALL W3KRTN ( ISEA, FACTH, FACK, CTHG0S(ISEA), &
!/SMC                           CG(:,ISEA), WN(:,ISEA), DEPTH,            &
!/SMC                           DHDX(ISEA), DHDY(ISEA), DHLMT(:,ISEA),    &
!/SMC                           CX(ISEA), CY(ISEA), DCXDX(IY,IX),         &
!/SMC                           DCXDY(IY,IX), DCYDX(IY,IX), DCYDY(IY,IX), & 
!/SMC                           DCDX(:,IY,IX), DCDY(:,IY,IX), VA(:,JSEA) )  
!
                    END IF
                  END DO
!
!/OMPG/!$OMP END DO
!/OMPG/!$OMP END PARALLEL
!
              END DO
          END IF

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 16'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!/PDLIB!/DEBUGCOH  CALL ALL_VA_INTEGRAL_PRINT(IMOD, "Before spatial advection")
!/TIMINGS         CALL PRINT_MY_TIME("Before spatial advection")
!
! 3.6.3 Longitude-latitude
!       (time step correction in routine)
!
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12'
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FSN=', FSN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FSPSI=', FSPSI
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FSFCT=', FSFCT
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FSNIMP=', FSNIMP
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLCTH=', FLCTH
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FSREFRACTION=', FSREFRACTION
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLCK=', FLCK
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FSFREQSHIFT=', FSFREQSHIFT
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLSOU=', FLSOU
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FSSOURCE=', FSSOURCE
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FSTOTALIMP=', FSTOTALIMP
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FSTOTALEXP=', FSTOTALEXP
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLCUR=', FLCUR
!/DEBUGRUN        WRITE(740+IAPROC,*) 'PDLIB=', LPDLIB
!/DEBUGRUN        WRITE(740+IAPROC,*) 'GTYPE=', GTYPE
!/DEBUGRUN        WRITE(740+IAPROC,*) 'UNGTYPE=', UNGTYPE
!/DEBUGRUN        WRITE(740+IAPROC,*) 'NAPROC=', NAPROC, 'NTPROC=', NTPROC
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FLCX=', FLCX, ' FLCY=', FLCY
!/DEBUGRUN        FLUSH(740+IAPROC)
!
!/NETCDF_QAD      CALL OUTPUT_NETCDF_QUICK_AND_DIRTY(IMOD, DTG)
!
        IF (GTYPE .EQ. UNGTYPE) THEN
          IF (FLAGLL) THEN
            FACX   =  1./(DERA * RADIUS) 
          ELSE 
            FACX   =  1.
          END IF
        END IF
        IF ((GTYPE .EQ. UNGTYPE) .and. LPDLIB) THEN
!
!/PDLIB       IF ((FSTOTALIMP .eqv. .FALSE.).and.(FLCX .or. FLCY)) THEN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12.1'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/PDLIB         DO ISPEC=1,NSPEC
!/PDLIB           CALL PDLIB_W3XYPUG ( ISPEC, FACX, FACX, DTG,           &
!/PDLIB                                VGX, VGY, UGDTUPDATE )
!/PDLIB         END DO
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12.2'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/PDLIB       END IF
!
!/PDLIB       IF (FSTOTALIMP .and. (IT .ne. 0)) THEN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12.3A'
!/DEBUGRUN        WRITE(*,*), 'W3WAVE, step 6.12.3A'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/PDLIB         CALL PDLIB_W3XYPUG_BLOCK_IMPLICIT (FACX, FACX, DTG, VGX, VGY)
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12.4A'
!/DEBUGRUN        WRITE(*,*), 'W3WAVE, step 6.12.4A'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/PDLIB       ELSE IF(FSTOTALEXP .and. (IT .ne. 0)) THEN 
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12.3B'
!/DEBUGRUN        WRITE(*,*), 'W3WAVE, step 6.12.3B'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/PDLIB         CALL PDLIB_W3XYPUG_BLOCK_EXPLICIT(FACX, FACX, DTG, VGX, VGY)
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12.4B'
!/DEBUGRUN        WRITE(*,*), 'W3WAVE, step 6.12.4B'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/PDLIB       ENDIF
        ELSE
          IF (FLCX .or. FLCY) THEN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.13'
!/DEBUGRUN        FLUSH(740+IAPROC)
!
!/MPI       IF ( NRQSG1 .GT. 0 ) THEN
!/MPI         CALL MPI_STARTALL (NRQSG1, IRQSG1(1,1), IERR_MPI)
!/MPI         CALL MPI_STARTALL (NRQSG1, IRQSG1(1,2), IERR_MPI)
!/MPI       END IF
!
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.14'
!/DEBUGRUN        FLUSH(740+IAPROC)
!
! Initialize FIELD variable 
             FIELD = 0.
!
            DO ISPEC=1, NSPEC
              IF ( IAPPRO(ISPEC) .EQ. IAPROC ) THEN
                IF (.NOT.LPDLIB .AND. RGLGRD) CALL W3GATH ( ISPEC, FIELD )
!/SMC !!Li   Otherwise use SMC sub to gether field
!/SMC                CALL W3GATHSMC ( ISPEC, FIELD )
!
                IF (GTYPE .NE. UNGTYPE) THEN
!/PR1             CALL W3XYP1 ( ISPEC, DTG, MAPSTA, FIELD, VGX, VGY )
!/PR2             CALL W3XYP2 ( ISPEC, DTG, MAPSTA, MAPFS, FIELD, VGX, VGY )
!/PR3             CALL W3XYP3 ( ISPEC, DTG, MAPSTA, MAPFS, FIELD, VGX, VGY )
!/PRX             CALL W3XYPX
!/SMC   !!Li   Propagation on SMC grid uses UNO2 scheme.
!/SMC             CALL W3PSMC ( ISPEC, DTG, FIELD )
!
                ELSE IF (GTYPE .EQ. UNGTYPE) THEN
!/MPI             IF (.NOT. LPDLIB) THEN
!/PR1               CALL W3XYPUG ( ISPEC, FACX, FACX, DTG,           &
!/PR1                              FIELD, VGX, VGY, UGDTUPDATE )
!/PR2               CALL W3XYPUG ( ISPEC, FACX, FACX, DTG,           &
!/PR2                              FIELD, VGX, VGY, UGDTUPDATE )
!/PR3               CALL W3XYPUG ( ISPEC, FACX, FACX, DTG,           &
!/PR3                              FIELD, VGX, VGY, UGDTUPDATE )
!/PRX               CALL W3XYPUG ( ISPEC, FACX, FACX, DTG,           &
!/PRX                              FIELD, VGX, VGY, UGDTUPDATE )
!/MPI             END IF
                END IF
                IF (.NOT.LPDLIB .AND. RGLGRD) CALL W3SCAT ( ISPEC, MAPSTA, FIELD )
!/SMC !!Li   Otherwise use SMC sub to scatter field
!/SMC           CALL W3SCATSMC ( ISPEC, MAPSTA, FIELD )
!
              END IF
            END DO
!
!/MPI       IF ( NRQSG1 .GT. 0 ) THEN
!/MPI         ALLOCATE ( STATCO(MPI_STATUS_SIZE,NRQSG1) )
!/MPI         CALL MPI_WAITALL (NRQSG1, IRQSG1(1,1), STATCO, &
!/MPI                           IERR_MPI)
!/MPI         CALL MPI_WAITALL (NRQSG1, IRQSG1(1,2), STATCO, &
!/MPI                           IERR_MPI)
!/MPI         DEALLOCATE ( STATCO )
!/MPI         END IF

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 17'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)
!
!Li   Initialise IK IX IY in case ARC option is not used to avoid warnings.
              IK=1
              IX=1
              IY=1
!/ARC  !Li    Find source boundary spectra and assign to SPCBAC
!/ARC  !Li    Have to use mpi options so will be empty if ARC not selected.
!/ARC        DO IK = 1, NBAC
!/ARC           IF( IK .LE. (NBAC-NBGL) ) THEN
!/ARC                   IY = ICLBAC(IK)
!/ARC           ELSE
!/ARC                   IY = NGLO + IK 
!/ARC           ENDIF 
!/ARC
!/ARC  !Li    Work out root PE (ISPEC) and JSEA numbers for IY 
!
          IF( ARCTIC ) THEN
!/DIST            ISPEC = MOD( IY-1, NAPROC ) 
!/DIST             JSEA = 1 + (IY - ISPEC - 1)/NAPROC
!/SHRD            ISPEC = 0 
!/SHRD             JSEA = IY 
          ENDIF
!
!/ARC !!Li   Assign boundary cell spectra. 
!/ARC              IF( IAPROC .EQ. ISPEC+1 ) THEN
!/ARC                   SPCBAC(:,IK)=VA(:,JSEA)
!/ARC              ENDIF
!
!/MPI  !!Li   Broadcast local SPCBAC(:,IK) to all other PEs.
!/MPI         IF( ARCTIC ) THEN
!/MPI         CALL MPI_BCAST(SPCBAC(1,IK),NSPEC,MPI_REAL,ISPEC,MPI_COMM_WAVE,IERR_MPI)
!/MPI         CALL MPI_BARRIER (MPI_COMM_WAVE,IERR_MPI)
!/MPI         ENDIF
!
!/ARC  !!Li   Boundary cell loop IK ends.
!/ARC         END DO
!
!/ARC  !Li    Update Arctic boundary cell spectra if within local range
!/ARC           ALLOCATE ( BACSPEC(NSPEC) )
!/ARC        DO IK = 1, NBAC
!/ARC           IF( IK .LE. (NBAC-NBGL) ) THEN
!/ARC                   IX = NGLO + IK 
!/ARC                   BACANGL = ANGARC(IK)
!/ARC           ELSE
!/ARC                   IX = ICLBAC(IK)
!/ARC                   BACANGL = - ANGARC(IK)
!/ARC           ENDIF 
!/ARC
!/ARC  !Li    Work out boundary PE (ISPEC) and JSEA numbers for IX 

          IF( ARCTIC ) THEN
!/DIST            ISPEC = MOD( IX-1, NAPROC ) 
!/DIST             JSEA = 1 + (IX - ISPEC - 1)/NAPROC
!/SHRD            ISPEC = 0 
!/SHRD             JSEA = IX 
          ENDIF
!
!/ARC              IF( IAPROC .EQ. ISPEC+1 ) THEN
!/ARC                   BACSPEC = SPCBAC(:,IK)
!/ARC                  
!/ARC                   CALL w3acturn( NTH, NK, BACANGL, BACSPEC )
!/ARC
!/ARC                   VA(:,JSEA) = BACSPEC 
!/ARC  !Li              WRITE(NDSE,*) "IAPROC, IX, JSEAx, IK=", IAPROC, IX, JSEA, IK 
!/ARC              ENDIF
!/ARC
!/ARC  !!Li   Boundary cell loop IK ends.
!/ARC         END DO
!/ARC         DEALLOCATE ( BACSPEC )
!
! End of test FLCX.OR.FLCY
!
          END IF
        END IF

!/PDLIB!/DEBUGCOH        CALL ALL_VA_INTEGRAL_PRINT(IMOD, "After spatial advection")
!/TIMINGS         CALL PRINT_MY_TIME("After spatial advection")
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.16'
!/DEBUGRUN        WRITE(740+IAPROC,*) 'NTLOC=', NTLOC
!/DEBUGRUN        WRITE(740+IAPROC,*) 'ITLOCH=', ITLOCH
!/DEBUGRUN        FLUSH(740+IAPROC)
!
! 3.6.4 Intra-spectral part 2
!
          IF ( FLCTH .OR. FLCK ) THEN
              DO ITLOC=ITLOCH+1, NTLOC
!
!/OMPG/!$OMP PARALLEL PRIVATE (JSEA,ISEA,IX,IY,DEPTH,IXrel)
!/OMPG/!$OMP DO SCHEDULE (DYNAMIC,1)
!
!/DEBUGRUN                WRITE(740+IAPROC,*) ' ITLOC=', ITLOC
!/DEBUGRUN                WRITE(740+IAPROC,*) ' 2: Before call to W3KTP1 / W3KTP2 / W3KTP3'
                DO JSEA = 1, NSEAL

                  CALL INIT_GET_ISEA(ISEA, JSEA)
                  IX     = MAPSF(ISEA,1)
                  IY     = MAPSF(ISEA,2)
!/DEBUGRUN                  IF (JSEA == DEBUG_NODE) WRITE(*,*) 'W3WAVE TEST', SUM(VA(:,JSEA))
                  DEPTH  = MAX ( DMIN , DW(ISEA) )

                  IF ( GTYPE .EQ. UNGTYPE ) THEN 
                    IF (IOBP(ISEA) .NE. 1) CYCLE
                  ENDIF

                  IF ( MAPSTA(IY,IX) .EQ. 1 ) THEN
                           IF (LPDLIB) THEN
                             IXrel = JSEA
                           ELSE
                             IXrel = IX
                           END IF
!/PR1                      CALL W3KTP1 ( ISEA, FACTH, FACK, CTHG0S(ISEA),       &
!/PR1                           CG(:,ISEA), WN(:,ISEA), DEPTH,                  &
!/PR1                           DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
!/PR1                           CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
!/PR1                           DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
!/PR1                           DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA))
!/PR2                      CALL W3KTP2 ( ISEA, FACTH, FACK, CTHG0S(ISEA),       &
!/PR2                           CG(:,ISEA), WN(:,ISEA), DEPTH,                  &
!/PR2                           DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
!/PR2                           CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
!/PR2                           DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
!/PR2                           DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA))
!/PR3                      CALL W3KTP3 ( ISEA, FACTH, FACK, CTHG0S(ISEA),       &
!/PR3                           CG(:,ISEA), WN(:,ISEA), DEPTH,                  &
!/PR3                           DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
!/PR3                           CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
!/PR3                           DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
!/PR3                           DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA), &
!/PR3                           CFLTHMAX(JSEA), CFLKMAX(JSEA) )
!/PRX                      CALL W3KTPX
!
!/SMC  !!Li    Refraction and GCT in theta direction is done by rotation.
!/SMC                      CALL W3KRTN ( ISEA, FACTH, FACK, CTHG0S(ISEA), &
!/SMC                           CG(:,ISEA), WN(:,ISEA), DEPTH,            &
!/SMC                           DHDX(ISEA), DHDY(ISEA), DHLMT(:,ISEA),    &
!/SMC                           CX(ISEA), CY(ISEA), DCXDX(IY,IX),         &
!/SMC                           DCXDY(IY,IX), DCYDX(IY,IX), DCYDY(IY,IX), & 
!/SMC                           DCDX(:,IY,IX), DCDY(:,IY,IX), VA(:,JSEA) )  
!
                    END IF
                  END DO
!
!/OMPG/!$OMP END DO
!/OMPG/!$OMP END PARALLEL
!
                END DO
            END IF
!/PDLIB!/DEBUGCOH       CALL ALL_VA_INTEGRAL_PRINT(IMOD, "After intraspectral adv.")
!/TIMINGS         CALL PRINT_MY_TIME("fter intraspectral adv.")
!
          UGDTUPDATE = .FALSE.
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.17'
!/DEBUGRUN        WRITE(740+IAPROC,*) 'FSSOURCE=', FSSOURCE
!/DEBUGRUN        FLUSH(740+IAPROC)
! 
! 3.6 End propapgation  = = = = = = = = = = = = = = = = = = = = = = = =

! 3.7 Calculate and integrate source terms.
!
  370     CONTINUE
          IF ( FLSOU ) THEN
!
            D50=0.0002
            REFLEC(:)=0.
            REFLED(:)=0
            PSIC=0.
!/PDLIB!/DEBUGSRC          WRITE(740+IAPROC,*) 'ITIME=', ITIME, ' IT=', IT
!/PDLIB!/DEBUGSRC          CALL ALL_VAOLD_INTEGRAL_PRINT("VAOLD before W3SRCE_IMP_POST")
!/PDLIB!/DEBUGSRC          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA before W3SRCE_IMP_POST")
!/PDLIB!/DEBUGSRC          IF (DEBUG_NODE .le. NSEAL) THEN
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     Values for DEBUG_NODE=', DEBUG_NODE
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     sum(VA)=', sum(VA(:,DEBUG_NODE))
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     sum(VAOLD)=', sum(VAOLD(:,DEBUG_NODE))
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     sum(VSTOT)=', sum(VSTOT(:,DEBUG_NODE))
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     sum(VDTOT)=', sum(VDTOT(:,DEBUG_NODE))
!/PDLIB!/DEBUGSRC          END IF
!
!/OMPG/!$OMP PARALLEL PRIVATE (JSEA,ISEA,IX,IY,DELA,DELX,DELY,        &
!/OMPG/!$OMP&                  REFLEC,REFLED,D50,PSIC,TMP1,TMP2,TMP3,TMP4)
!/OMPG/!$OMP DO SCHEDULE (DYNAMIC,1)
!
              DO JSEA=1, NSEAL
                CALL INIT_GET_ISEA(ISEA, JSEA)
                IX     = MAPSF(ISEA,1)
                IY     = MAPSF(ISEA,2)
                DELA=1. 
                DELX=1. 
                DELY=1. 
!/REF1                IF (GTYPE.EQ.RLGTYPE) THEN 
!/REF1                  DELX=SX*CLATS(ISEA)/FACX
!/REF1                  DELY=SY/FACX
!/REF1                  DELA=DELX*DELY
!/REF1                END IF
!/REF1                IF (GTYPE.EQ.CLGTYPE) THEN 
!/REF1! Maybe what follows works also for RLGTYPE ... to be verified
!/REF1                  DELX=HPFAC(IY,IX)/ FACX
!/REF1                  DELY=HQFAC(IY,IX)/ FACX 
!/REF1                  DELA=DELX*DELY
!/REF1                END IF
!
!/REF1          REFLEC=REFLC(:,ISEA)
!/REF1          REFLEC(4)=BERG(ISEA)*REFLEC(4)
!/REF1          REFLED=REFLD(:,ISEA)
!/BT4           D50=SED_D50(ISEA)
!/BT4           PSIC=SED_PSIC(ISEA)

!/DEBUGRUN          IF (JSEA == DEBUG_NODE) WRITE(*,*) 'W3WAVE TEST', ISEA, JSEA, SUM(VA(:,JSEA))

                IF ( MAPSTA(IY,IX) .EQ. 1 .AND. FLAGST(ISEA)) THEN
                     TMP1   = WHITECAP(JSEA,1:4)
                     TMP2   = BEDFORMS(JSEA,1:3)
                     TMP3   = TAUBBL(JSEA,1:2)
                     TMP4   = TAUICE(JSEA,1:2)
!/PDLIB              IF (FSSOURCE) THEN
!/PDLIB                   CALL W3SRCE(srce_imp_post,IT,JSEA,IX,IY,IMOD,  &
!/PDLIB                      VAOLD(:,JSEA), VA(:,JSEA),                  &
!/PDLIB                      VSTOT(:,JSEA),VDTOT(:,JSEA),SHAVETOT(JSEA), &
!/PDLIB                      ALPHA(1:NK,JSEA), WN(1:NK,ISEA),            &
!/PDLIB                      CG(1:NK,ISEA), DW(ISEA), U10(ISEA),         &
!/PDLIB                      U10D(ISEA), AS(ISEA), UST(ISEA),            &
!/PDLIB                      USTDIR(ISEA), CX(ISEA), CY(ISEA),           &
!/PDLIB                      ICE(ISEA), ICEH(ISEA), ICEF(ISEA),          &
!/PDLIB                      ICEDMAX(ISEA),                              &
!/PDLIB                      REFLEC, REFLED, DELX, DELY, DELA,           &
!/PDLIB                      TRNX(IY,IX), TRNY(IY,IX), BERG(ISEA),       &
!/PDLIB                      FPIS(ISEA), DTDYN(JSEA),                    &
!/PDLIB                      FCUT(JSEA), DTG, TAUWX(JSEA), TAUWY(JSEA),  &
!/PDLIB                      TAUOX(JSEA), TAUOY(JSEA), TAUWIX(JSEA),     &
!/PDLIB                      TAUWIY(JSEA), TAUWNX(JSEA),                 &
!/PDLIB                      TAUWNY(JSEA),  PHIAW(JSEA), CHARN(JSEA),    &
!/PDLIB                      TWS(JSEA),PHIOC(JSEA), TMP1, D50, PSIC, TMP2,&
!/PDLIB                      PHIBBL(JSEA), TMP3, TMP4, PHICE(JSEA),      &
!/PDLIB                      ASF(ISEA))
!/PDLIB              ELSE
                       CALL W3SRCE(srce_direct, IT, JSEA, IX, IY, IMOD, &
                            VAoldDummy, VA(:,JSEA),                     &
                            VSioDummy, VDioDummy, SHAVETOTioDummy,      &
                            ALPHA(1:NK,JSEA), WN(1:NK,ISEA),            &
                            CG(1:NK,ISEA), DW(ISEA), U10(ISEA),         &
                            U10D(ISEA), AS(ISEA), UST(ISEA),            &
                            USTDIR(ISEA), CX(ISEA), CY(ISEA),           &
                            ICE(ISEA), ICEH(ISEA), ICEF(ISEA),          &
                            ICEDMAX(ISEA),                              &
                            REFLEC, REFLED, DELX, DELY, DELA,           &
                            TRNX(IY,IX), TRNY(IY,IX), BERG(ISEA),       &
                            FPIS(ISEA), DTDYN(JSEA),                    &
                            FCUT(JSEA), DTG, TAUWX(JSEA), TAUWY(JSEA),  &
                            TAUOX(JSEA), TAUOY(JSEA), TAUWIX(JSEA),     &
                            TAUWIY(JSEA), TAUWNX(JSEA),                 &
                            TAUWNY(JSEA),  PHIAW(JSEA), CHARN(JSEA),    &
                            TWS(JSEA), PHIOC(JSEA), TMP1, D50, PSIC,TMP2,&
                            PHIBBL(JSEA), TMP3, TMP4 , PHICE(JSEA),     &
                            ASF(ISEA))
!/PDLIB              END IF
                     WHITECAP(JSEA,1:4) = TMP1
                     BEDFORMS(JSEA,1:3) = TMP2
                     TAUBBL(JSEA,1:2) = TMP3
                     TAUICE(JSEA,1:2) = TMP4
                ELSE
                    UST   (ISEA) = UNDEF
                    USTDIR(ISEA) = UNDEF
                    DTDYN (JSEA) = UNDEF
                    FCUT  (JSEA) = UNDEF
!                    VA(:,JSEA)  = 0.
                END IF
!/DEBUGRUN        WRITE(740+IAPROC,*) 'RET: min/max/sum(VA)=',minval(VA(:,JSEA)),maxval(VA(:,JSEA)),sum(VA(:,JSEA))
              END DO
!/DEBUGRUN        WRITE(740+IAPROC,*) 'min/max/sum(VAtot)=', minval(VA), maxval(VA), sum(VA)
!/DEBUGRUN        FLUSH(740+IAPROC)

!/DEBUGRUN      DO JSEA = 1, NSEAL
!/DEBUGRUN        DO IS = 1, NSPEC
!/DEBUGRUN          IF (VA(IS, JSEA) .LT. 0.) THEN
!/DEBUGRUN            WRITE(740+IAPROC,*) 'TEST W3WAVE 9', VA(IS,JSEA)
!/DEBUGRUN            CALL FLUSH(740+IAPROC)
!/DEBUGRUN          ENDIF
!/DEBUGRUN        ENDDO
!/DEBUGRUN      ENDDO
!/DEBUGRUN      IF (SUM(VA) .NE. SUM(VA)) THEN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'NAN in ACTION 7', IX, IY, SUM(VA)
!/DEBUGRUN        CALL FLUSH(740+IAPROC)
!/DEBUGRUN        STOP
!/DEBUGRUN      ENDIF
!
!/OMPG/!$OMP END DO
!/OMPG/!$OMP END PARALLEL
!
!/PDLIB!/DEBUGSRC          WRITE(740+IAPROC,*) 'ITIME=', ITIME, ' IT=', IT
!/PDLIB!/DEBUGSRC          CALL ALL_VAOLD_INTEGRAL_PRINT("VAOLD after W3SRCE_IMP_PRE_POST")
!/PDLIB!/DEBUGSRC          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA after W3SRCE_IMP_PRE_POST")
!/PDLIB!/DEBUGSRC          IF (DEBUG_NODE .le. NSEAL) THEN
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     Values for DEBUG_NODE=', DEBUG_NODE
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     sum(VA)=', sum(VA(:,DEBUG_NODE))
!/PDLIB!/DEBUGSRC            WRITE(740+IAPROC,*) '     min/max(VA)=', minval(VA(:,DEBUG_NODE)), maxval(VA(:,DEBUG_NODE))
!/PDLIB!/DEBUGSRC          END IF

!
! This barrier is from older code versions. It has been removed in 3.11
! to optimize IO2/3 settings. May be needed on some systems still
!
!!/MPI              IF (FLAG0) CALL MPI_BARRIER (MPI_COMM_WCMP,IERR_MPI)
!!/MPI            ELSE
!!/MPI              CALL MPI_BARRIER (MPI_COMM_WCMP,IERR_MPI)
!
            END IF
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.18'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/PDLIB!/DEBUGCOH          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "After source terms")
!/TIMINGS         CALL PRINT_MY_TIME("After source terms")
!
! End of interations for DTMAX < 1s
!
!/SEC1       IF (IT.EQ.0) EXIT
!/SEC1       END DO
!/SEC1       IF (IT.GT.0) DTG=DTGTEMP
!
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.19'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/DEBUGRUN      DO JSEA = 1, NSEAL
!/DEBUGRUN        DO IS = 1, NSPEC
!/DEBUGRUN          IF (VA(IS, JSEA) .LT. 0.) THEN
!/DEBUGRUN            WRITE(740+IAPROC,*) 'TEST W3WAVE 10', VA(IS,JSEA)
!/DEBUGRUN            CALL FLUSH(740+IAPROC)
!/DEBUGRUN          ENDIF
!/DEBUGRUN        ENDDO
!/DEBUGRUN     ENDDO
!/DEBUGRUN      IF (SUM(VA) .NE. SUM(VA)) THEN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'NAN in ACTION 8', IX, IY, SUM(VA)
!/DEBUGRUN        CALL FLUSH(740+IAPROC)
!/DEBUGRUN        STOP
!/DEBUGRUN      ENDIF
!
! 3.8 Update global time step.
!     (Branch point FLDRY, IT=0)
!
  380     CONTINUE
!
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.20'
!/DEBUGRUN        FLUSH(740+IAPROC)
          IF (IT.NE.NT) THEN
              DTTST  = DSEC21 ( TIME , TCALC )
              DTG    = DTTST / REAL(NT-IT)
            END IF
!
          IF ( FLACT .AND. IT.NE.NT .AND. IAPROC.EQ.NAPLOG ) THEN
              CALL STME21 ( TIME , IDTIME )
              IF ( IDLAST .NE. TIME(1) ) THEN
                  WRITE (NDSO,900) ITIME, IPASS, IDTIME(01:19),       &
                                   IDACT, OUTID
                  IDLAST = TIME(1)
                ELSE
                  WRITE (NDSO,901) ITIME, IPASS, IDTIME(12:19),       &
                                   IDACT, OUTID
                END IF
              FLACT  = .FALSE.
              IDACT  = '         '
          END IF
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.21'
!/DEBUGRUN        FLUSH(740+IAPROC)
!
!/PDLIB!/DEBUGCOH            CALL ALL_VA_INTEGRAL_PRINT(IMOD, "end of time loop")
!/TIMINGS         CALL PRINT_MY_TIME("end of time loop")
!
!
        END DO

!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.21.1'
!/DEBUGRUN        FLUSH(740+IAPROC)
!/TIMINGS         CALL PRINT_MY_TIME("W3WAVE, step 6.21.1")
!
!/T      WRITE (NDST,9030)
!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE END TIME LOOP'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)
!
!     End of loop over time steps
! ==================================================================== /
!
  400 CONTINUE

!          jcw bottom of wavemd calling the coupler
!/COAWST      IF (ITIME.gt.0) THEN
!/COAWST        CALL COAWST_CPL (ITIME)
!/COAWST      END IF

!
! 4.  Perform output to file if requested ---------------------------- /
! 4.a Check if time is output time
!     Delay if data assimilation time.
!
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.21.2'
!/DEBUGRUN        FLUSH(740+IAPROC)
!
        IF ( TOFRST(1)  .EQ. -1 ) THEN
            DTTST  = 1.
          ELSE
            DTTST   = DSEC21 ( TIME, TOFRST )
          END IF
!
        IF ( TDN(1)  .EQ. -1 ) THEN
            DTTST1 = 1.
          ELSE
            DTTST1  = DSEC21 ( TIME, TDN )
          END IF
!
        DTTST2 = DSEC21 ( TIME, TEND )
        FLAG_O = .NOT.SKIP_O .OR. ( SKIP_O .AND. DTTST2.NE.0. )
!
!/T        WRITE (NDST,9040) TOFRST, TDN, DTTST, DTTST1, FLAG_O
!
!/DEBUGRUN        WRITE(740+IAPROC,*) 'W3WAVE, step 6.21.3'
!/DEBUGRUN        FLUSH(740+IAPROC)
        IF ( DTTST.LE.0. .AND. DTTST1.NE.0. .AND. FLAG_O ) THEN
!/DEBUGRUN            WRITE(740+IAPROC,*) 'W3WAVE, step 6.21.4'
!/DEBUGRUN            FLUSH(740+IAPROC)
!
!/T          WRITE (NDST,9041)
!
! 4.b Processing and MPP preparations
!
            IF ( FLOUT(1) ) THEN
                FLOUTG = DSEC21(TIME,TONEXT(:,1)).EQ.0.
              ELSE
                FLOUTG = .FALSE.
              END IF
!
            IF ( FLOUT(7) ) THEN
                FLOUTG2 = DSEC21(TIME,TONEXT(:,7)).EQ.0.
              ELSE
                FLOUTG2 = .FALSE.
              END IF
!
          FLPART = .FALSE.
          IF ( FLOUT(1) .AND. FLPFLD )                               &
               FLPART = FLPART .OR. DSEC21(TIME,TONEXT(:,1)).EQ.0.
!/DEBUGRUN            WRITE(740+IAPROC,*) 'W3WAVE, step 6.21.7'
!/DEBUGRUN            FLUSH(740+IAPROC)
          IF ( FLOUT(6) )                                            &
               FLPART = FLPART .OR. DSEC21(TIME,TONEXT(:,6)).EQ.0.
!/DEBUGRUN            WRITE(740+IAPROC,*) 'W3WAVE, step 6.21.8'
!/DEBUGRUN            FLUSH(740+IAPROC)
!
!/T            WRITE (NDST,9042) LOCAL, FLPART, FLOUTG
!
            IF ( LOCAL .AND. FLPART ) CALL W3CPRT ( IMOD )
            IF ( LOCAL .AND. (FLOUTG .OR. FLOUTG2) )                   &
                 CALL W3OUTG ( VA, FLPFLD, FLOUTG, FLOUTG2 )
!
!/MPI            FLGMPI = .FALSE.
!/MPI            NRQMAX = 0
!
!/MPI     IF ( ( (DSEC21(TIME,TONEXT(:,1)).EQ.0.) .AND. FLOUT(1) ) .OR. &
!/MPI          (  (DSEC21(TIME,TONEXT(:,7)).EQ.0.) .AND. FLOUT(7) .AND. &
!/MPI             SBSED ) ) THEN
!/MPI       IF (.NOT. LPDLIB .or. (GTYPE.ne.UNGTYPE)) THEN
!/MPI         IF (NRQGO.NE.0 ) THEN
!/DEBUGRUN      WRITE(740+IAPROC,*) 'BEFORE STARTALL NRQGO.NE.0 , step 0', &
!/DEBUGRUN                 NRQGO, IRQGO, GTYPE, UNGTYPE, .NOT. LPDLIB .or. (GTYPE.ne.UNGTYPE)
!/DEBUGRUN      FLUSH(740+IAPROC)
!/MPI           CALL MPI_STARTALL ( NRQGO, IRQGO , IERR_MPI )
!/DEBUGRUN      WRITE(740+IAPROC,*) 'AFTER STARTALL NRQGO.NE.0, step 0'
!/DEBUGRUN      FLUSH(740+IAPROC)

!/MPI           FLGMPI(0) = .TRUE.
!/MPI           NRQMAX    = MAX ( NRQMAX , NRQGO )
!/MPIT          WRITE (NDST,9043) '1a', NRQGO, NRQMAX, NAPFLD
!/MPI         END IF
!
!/MPI         IF (NRQGO2.NE.0 ) THEN
!/DEBUGRUN      WRITE(740+IAPROC,*) 'BEFORE STARTALL NRQGO2.NE.0, step 0', &
!/DEBUGRUN              NRQGO2, IRQGO2, GTYPE, UNGTYPE, .NOT. LPDLIB .or. (GTYPE.ne.UNGTYPE)
!/DEBUGRUN      FLUSH(740+IAPROC)
!/MPI           CALL MPI_STARTALL ( NRQGO2, IRQGO2, IERR_MPI )
!/DEBUGRUN      WRITE(740+IAPROC,*) 'AFTER STARTALL NRQGO2.NE.0, step 0'
!/DEBUGRUN      FLUSH(740+IAPROC)
!/MPI           FLGMPI(1) = .TRUE.
!/MPI           NRQMAX    = MAX ( NRQMAX , NRQGO2 )
!/MPIT          WRITE (NDST,9043) '1b', NRQGO2, NRQMAX, NAPFLD
!/MPI         END IF
!/MPI       ELSE
!/DEBUGRUN      WRITE(740+IAPROC,*) 'BEFORE DO_OUTPUT_EXCHANGES, step 0'
!/DEBUGRUN      FLUSH(740+IAPROC)
!/PDLIB       CALL DO_OUTPUT_EXCHANGES(IMOD)
!/MPI       END IF
!/MPI     END IF

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE AFTER TIME LOOP 1'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!
!/DEBUGRUN      WRITE(740+IAPROC,*) 'After DO_OUTPUT_EXCHANGES, step 1'
!/DEBUGRUN      FLUSH(740+IAPROC)
!/MPI            IF ( FLOUT(2) .AND. NRQPO.NE.0 ) THEN
!/MPI                IF ( DSEC21(TIME,TONEXT(:,2)).EQ.0. ) THEN
!/MPI                    CALL MPI_STARTALL ( NRQPO, IRQPO1, IERR_MPI )
!/MPI                    FLGMPI(2) = .TRUE.
!/MPI                    NRQMAX    = MAX ( NRQMAX , NRQPO )
!/MPIT                    WRITE (NDST,9043) '2 ', NRQPO, NRQMAX, NAPPNT
!/MPI                  END IF
!/MPI              END IF
!
!/DEBUGRUN      WRITE(740+IAPROC,*) 'After DO_OUTPUT_EXCHANGES, step 2'
!/DEBUGRUN      FLUSH(740+IAPROC)
!/MPI            IF ( FLOUT(4) .AND. NRQRS.NE.0 ) THEN
!/MPI                IF ( DSEC21(TIME,TONEXT(:,4)).EQ.0. ) THEN
!/MPI                    CALL MPI_STARTALL ( NRQRS, IRQRS , IERR_MPI )
!/MPI                    FLGMPI(4) = .TRUE.
!/MPI                    NRQMAX    = MAX ( NRQMAX , NRQRS )
!/MPIT                    WRITE (NDST,9043) '4 ', NRQRS, NRQMAX, NAPRST
!/MPI                  END IF
!/MPI              END IF
!
!/DEBUGRUN      WRITE(740+IAPROC,*) 'After DO_OUTPUT_EXCHANGES, step 2'
!/DEBUGRUN      FLUSH(740+IAPROC)
!/MPI            IF ( FLOUT(8) .AND. NRQRS.NE.0 ) THEN
!/MPI                IF ( DSEC21(TIME,TONEXT(:,8)).EQ.0. ) THEN
!/MPI                    CALL MPI_STARTALL ( NRQRS, IRQRS , IERR_MPI )
!/MPI                    FLGMPI(8) = .TRUE.
!/MPI                    NRQMAX    = MAX ( NRQMAX , NRQRS )
!/MPIT                    WRITE (NDST,9043) '8 ', NRQRS, NRQMAX, NAPRST
!/MPI                  END IF
!/MPI              END IF
!
!/DEBUGRUN      WRITE(740+IAPROC,*) 'After DO_OUTPUT_EXCHANGES, step 3'
!/DEBUGRUN      FLUSH(740+IAPROC)
!/MPI            IF ( FLOUT(5) .AND. NRQBP.NE.0 ) THEN
!/MPI                IF ( DSEC21(TIME,TONEXT(:,5)).EQ.0. ) THEN
!/MPI                    CALL MPI_STARTALL ( NRQBP , IRQBP1, IERR_MPI )
!/MPI                    FLGMPI(5) = .TRUE.
!/MPI                    NRQMAX    = MAX ( NRQMAX , NRQBP )
!/MPIT                    WRITE (NDST,9043) '5a', NRQBP, NRQMAX, NAPBPT
!/MPI                  END IF
!/MPI              END IF
!
!/DEBUGRUN      WRITE(740+IAPROC,*) 'After DO_OUTPUT_EXCHANGES, step 4'
!/DEBUGRUN      FLUSH(740+IAPROC)
!/MPI            IF ( FLOUT(5) .AND. NRQBP2.NE.0 .AND.                &
!/MPI                 IAPROC.EQ.NAPBPT) THEN
!/MPI                IF ( DSEC21(TIME,TONEXT(:,5)).EQ.0. ) THEN
!/MPI                    CALL MPI_STARTALL (NRQBP2,IRQBP2,IERR_MPI)
!/MPI                    NRQMAX    = MAX ( NRQMAX , NRQBP2 )
!/MPIT                    WRITE (NDST,9043) '5b', NRQBP2, NRQMAX, NAPBPT
!/MPI                  END IF
!/MPI              END IF
!
!/DEBUGRUN      WRITE(740+IAPROC,*) 'After DO_OUTPUT_EXCHANGES, step 5'
!/DEBUGRUN      FLUSH(740+IAPROC)
!/MPI           IF ( NRQMAX .NE. 0 ) ALLOCATE                         &
!/MPI                                 ( STATIO(MPI_STATUS_SIZE,NRQMAX) )

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE AFTER TIME LOOP 2'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!
! 4.c Reset next output time

!/DEBUGRUN      IF (MINVAL(VA) .LT. 0.) THEN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'TEST W3WAVE 12', SUM(VA), MINVAL(VA), MAXVAL(VA)
!/DEBUGRUN        CALL FLUSH(740+IAPROC)
!/DEBUGRUN        STOP
!/DEBUGRUN      ENDIF
!/DEBUGRUN      IF (SUM(VA) .NE. SUM(VA)) THEN
!/DEBUGRUN        WRITE(740+IAPROC,*) 'NAN in ACTION 9', IX, IY, SUM(VA)
!/DEBUGRUN        CALL FLUSH(740+IAPROC)
!/DEBUGRUN        STOP
!/DEBUGRUN      ENDIF
!
            TOFRST(1) = -1
            TOFRST(2) =  0
!

!          jcw bottom of wavemd calling the coupler
!/COOOOAWST      IF (ITIME.gt.0) THEN
!/COOOOAWST        CALL COAWST_CPL (ITIME)
!/COOOOAWST      END IF

            DO J=1, NOTYPE
!/DEBUGRUN      WRITE(740+IAPROC,*) 'NOTYPE, J=', J
!/DEBUGRUN      FLUSH(740+IAPROC)

              IF ( FLOUT(J) ) THEN
!
!/DEBUGRUN      WRITE(740+IAPROC,*) 'Matching FLOUT(J)'
!/DEBUGRUN      FLUSH(740+IAPROC)
!
! 4.d Perform output
!
                  TOUT(:) = TONEXT(:,J)
                  DTTST   = DSEC21 ( TIME, TOUT )
!
                  IF ( DTTST .EQ. 0. ) THEN
                      IF ( ( J .EQ. 1 )              &
!/SBS                           .OR. ( J .EQ. 7 )         &
                                        ) THEN
                          IF ( IAPROC .EQ. NAPFLD ) THEN
!/MPI                              IF ( FLGMPI(1) ) CALL MPI_WAITALL  &
!/MPI                                 ( NRQGO2, IRQGO2, STATIO, IERR_MPI )
!/MPI                              FLGMPI(1) = .FALSE.
!
!/SBS                              IF ( J .EQ. 1 ) THEN
                                CALL W3IOGO( 'WRITE', NDS(7), ITEST, IMOD )
!/SBS                              ENDIF
!
!/SBS !
!/SBS !     Generate output flag file for fields and SBS coupling.
!/SBS !
!/SBS                              JJ = LEN_TRIM ( FILEXT )
!/SBS                              CALL STME21 ( TIME, IDTIME )
!/SBS                              FOUTNAME = 'Field_done.' // IDTIME(1:4) &
!/SBS                                       // IDTIME(6:7) // IDTIME(9:10) &
!/SBS                                      // IDTIME(12:13) // '.' // FILEXT(1:JJ)
!
!/SBS                              OPEN( UNIT=NDSOFLG, FILE=FOUTNAME)
!/SBS                              CLOSE( NDSOFLG )
                            END IF
!
                        ELSE IF ( J .EQ. 2 ) THEN
!
!   Point output
!
                          IF ( IAPROC .EQ. NAPPNT ) THEN
!
!   Gets the necessary spectral data
!
                            CALL W3IOPE ( VA )
                            CALL W3IOPO ( 'WRITE', NDS(8), ITEST, IMOD )
                            END IF
!
                        ELSE IF ( J .EQ. 3 ) THEN
!
! Track output
!
                          CALL W3IOTR ( NDS(11), NDS(12), VA, IMOD )
                        ELSE IF ( J .EQ. 4 ) THEN
                          CALL W3IORS ('HOT', NDS(6), XXX, IMOD, FLOUT(8) )
                          ITEST = RSTYPE
                        ELSE IF ( J .EQ. 5 ) THEN
                          IF ( IAPROC .EQ. NAPBPT ) THEN
!/MPI                              IF (NRQBP2.NE.0) CALL MPI_WAITALL  &
!/MPI                                ( NRQBP2, IRQBP2,STATIO, IERR_MPI )
                              CALL W3IOBC ( 'WRITE', NDS(10),         &
                                            TIME, TIME, ITEST, IMOD )
                            END IF
                        ELSE IF ( J .EQ. 6 ) THEN
                          CALL W3IOSF ( NDS(13), IMOD )
!/OASIS                      ELSE IF ( J .EQ. 7 ) THEN
!/OASIS                        !
!/OASIS                        ! Send variables to atmospheric or ocean circulation or ice model
!/OASIS                        !
!/OASIS                        IF (DTOUT(7).NE.0) THEN
!/OASIS                          IF ( (MOD(ID_OASIS_TIME/DTOUT(7),1.0)  .LT. 1.E-7 ) .AND. &
!/OASIS                               (DSEC21 (TIME00, TIME) .GT. 0.0) ) THEN 
!/OASIS                            IF ( (CPLT0 .AND. (DSEC21 (TIME, TIMEN) .GT. 0.0)) .OR. &
!/OASIS                                  .NOT. CPLT0 ) THEN
!/OASIS                              IF (CPLT0) ID_OASIS_TIME = NINT(DSEC21 ( TIME00 , TIME ))
!/OASIS
!/OASACM                             CALL SND_FIELDS_TO_ATMOS()
!/OASOCM                             CALL SND_FIELDS_TO_OCEAN()
!/OASICM                             CALL SND_FIELDS_TO_ICE()
!/OASIS                              IF (.NOT. CPLT0) ID_OASIS_TIME = NINT(DSEC21 ( TIME00 , TIME ))
!/OASIS                            ENDIF
!/OASIS                          ENDIF
!/OASIS                        ENDIF
                        END IF
!
                      CALL TICK21 ( TOUT, DTOUT(J) )
                      TONEXT(:,J) = TOUT
                      TLST        = TOLAST(:,J)
                      DTTST       = DSEC21 ( TOUT , TLST )
                      FLOUT(J)    = DTTST.GE.0.
                      IF ( FLOUT(J) ) THEN
                          OUTID(2*J-1:2*J-1) = 'X'
!/OASIS                          IF ( (DTOUT(7).NE.0) .AND.           &
!/OASIS                               (DSEC21(TIME,TIME00).EQ.0 .OR.  &
!/OASIS                                DSEC21(TIME,TIMEEND).EQ.0) ) OUTID(13:13) = ' '
                        ELSE
                          OUTID(2*J-1:2*J-1) = 'L'
                        END IF
                    END IF
!
! 4.e Update next output time
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


! If there is a second stream of restart files then J=8 and FLOUT(8)=.TRUE.
            J=8
            IF ( FLOUT(J) ) THEN
!
!/DEBUGRUN      WRITE(740+IAPROC,*) 'Matching FLOUT(J)'
!/DEBUGRUN      FLUSH(740+IAPROC)
!
! 4.d Perform output
!
              TOUT(:) = TONEXT(:,J)
              DTTST   = DSEC21 ( TIME, TOUT )
              IF ( DTTST .EQ. 0. ) THEN
                CALL W3IORS ('HOT', NDS(6), XXX, IMOD, FLOUT(8) )
                 ITEST = RSTYPE
                 CALL TICK21 ( TOUT, DTOUT(J) )
                 TONEXT(:,J) = TOUT
                 TLST        = TOLAST(:,J)
                 DTTST       = DSEC21 ( TOUT , TLST )
                 FLOUT(J)    = DTTST.GE.0.
                 IF ( FLOUT(J) ) THEN
                   OUTID(2*J-1:2*J-1) = 'X'
!/OASIS                     IF ( (DTOUT(7).NE.0) .AND.           &
!/OASIS                        (DSEC21(TIME,TIME00).EQ.0 .OR.  &
!/OASIS                        DSEC21(TIME,TIMEEND).EQ.0) ) OUTID(13:13) = ' '
                  ELSE
                    OUTID(2*J-1:2*J-1) = 'L'
                  END IF
              END IF
!
! 4.e Update next output time
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
            END IF
!        END OF CHECKPOINT
!
!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE AFTER TIME LOOP 3'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!
!/MPI            IF ( FLGMPI(0) ) CALL MPI_WAITALL                    &
!/MPI                             ( NRQGO, IRQGO , STATIO, IERR_MPI )
!/MPI            IF ( FLGMPI(2) ) CALL MPI_WAITALL                    &
!/MPI                             ( NRQPO, IRQPO1, STATIO, IERR_MPI )
!/MPI            IF ( FLGMPI(4) ) CALL MPI_WAITALL                    &
!/MPI                             ( NRQRS, IRQRS , STATIO, IERR_MPI )
!/MPI            IF ( FLGMPI(8) ) CALL MPI_WAITALL                    &
!/MPI                             ( NRQRS, IRQRS , STATIO, IERR_MPI )
!/MPI            IF ( FLGMPI(5) ) CALL MPI_WAITALL                    &
!/MPI                             ( NRQBP, IRQBP1, STATIO, IERR_MPI )
!/MPI            IF ( NRQMAX .NE. 0 ) DEALLOCATE ( STATIO )
!
!/T          WRITE (NDST,9044)
!
! This barrier is from older code versions. It has been removed in 3.11
! to optimize IO2/3 settings. May be needed on some systems still
!
!!/MPI            IF (FLDRY) CALL MPI_BARRIER (MPI_COMM_WAVE,IERR_MPI) 
!
          END IF
!/TIMINGS         CALL PRINT_MY_TIME("Before update log file")

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE AFTER TIME LOOP 4'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)

!
! 5.  Update log file ------------------------------------------------ /

!      IF (MINVAL(VA) .LT. 0.) THEN
!        WRITE(740+IAPROC,*) 'TEST W3WAVE 13', SUM(VA), MINVAL(VA), MAXVAL(VA)
!        CALL FLUSH(740+IAPROC)
!        STOP
!      ENDIF
!
        IF ( IAPROC.EQ.NAPLOG ) THEN
!
            CALL STME21 ( TIME , IDTIME )
            IF ( FLCUR ) THEN
                DTTST  = DSEC21 ( TIME , TCN )
                IF ( DTTST .EQ. 0. ) IDACT(7:7) = 'X'
              END IF
            IF ( FLWIND ) THEN
                DTTST  = DSEC21 ( TIME , TWN )
                IF ( DTTST .EQ. 0. ) IDACT(3:3) = 'X'
              END IF
            IF ( TDN(1) .GT. 0  ) THEN
                DTTST  = DSEC21 ( TIME , TDN )
                IF ( DTTST .EQ. 0. ) IDACT(17:17) = 'X'
              END IF
!
            IF ( IDLAST.NE.TIME(1) ) THEN
                WRITE (NDSO,900) ITIME, IPASS, IDTIME(1:19),          &
                                 IDACT, OUTID
                IDLAST = TIME(1)
              ELSE 
                WRITE (NDSO,901) ITIME, IPASS, IDTIME(12:19),         &
                                 IDACT, OUTID
              END IF
!
          END IF
!
        IDACT  = '         '
        OUTID  = '           '
        FLACT  = .FALSE.
!
! 6.  If time is not ending time, branch back to 2 ------------------- /
!
        DTTST  = DSEC21 ( TIME, TEND )
        IF ( DTTST .EQ. 0. ) EXIT
!/TIMINGS         CALL PRINT_MY_TIME("Continuing the loop")
      END DO

!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE AFTER TIME LOOP 5'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)
!

      IF ( TSTAMP .AND. SCREEN.NE.NDSO .AND. IAPROC.EQ.NAPOUT ) THEN
         CALL WWTIME ( STTIME )
         WRITE (SCREEN,951) STTIME
      END IF

      IF ( IAPROC .EQ. NAPLOG ) WRITE (NDSO,902)
!
      DEALLOCATE(FIELD)
      DEALLOCATE(TAUWX, TAUWY)
!
!/MEMCHECK       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE END W3WAVE'
!/MEMCHECK       call getMallocInfo(mallinfos)
!/MEMCHECK       call printMallInfo(IAPROC,mallInfos)
!
      RETURN
!
! Formats
!
  900 FORMAT (4X,I6,'|',I6,'| ', A19  ,' | ',A,' | ',A,' |')
  901 FORMAT (4X,I6,'|',I6,'| ',11X,A8,' | ',A,' | ',A,' |')
  902 FORMAT (2X,'--------+------+---------------------+'             &
                ,'-------------------+---------------+')
!
!/IC3  920 FORMAT ('     Updating k and Cg from ice param. 1,2,3,4.'/)
  950 FORMAT ('  WAVEWATCH III calculating for ',A,' at ',A)
  951 FORMAT ('  WAVEWATCH III reached the end of a computation',     &
               ' loop at ',A)
 1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     ENDING TIME BEFORE STARTING TIME '/)
 1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     NEW WATER LEVEL BEFORE OLD WATER LEVEL '/)
 1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     ILLEGAL CURRENT INTERVAL '/)
 1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     ILLEGAL WIND INTERVAL '/)
 1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     NEW ICE FIELD BEFORE OLD ICE FIELD '/)
 1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     NEW IC1 FIELD BEFORE OLD IC1 FIELD '/)
!/IS2 1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
!/IS2               '     NEW IC5 FIELD BEFORE OLD IC5 FIELD '/)
 1030 FORMAT (/' *** WAVEWATCH III WARING IN W3WAVE :'/               &
               '     AT LEAST ONE PROCESSOR HAS 0 ACTIVE POINTS',     &
                   ' IN GRID',I3)
!/REFRX 1040 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
!/REFRX               '     EXPERIMENTAL FEATURE !/REFRX NOT FULLY IMPLEMENTED.'/)
!
!/T 9000 FORMAT (                                                     &
!/T   '============================================================', &
!/T   '===================='/                                         &
!/T   ' TEST W3WAVE : RUN MODEL',I3,' FILEXT [',A,                    &
!/T                    '] UP TO ',I8.8,I7.6 /                         &
!/T   '====================',                                         &
!/T '============================================================')
!/T 9010 FORMAT (' TEST W3WAVE : DT INT. =',F12.1,'   FLZERO = ',L1)
!/T 9011 FORMAT (' TEST W3WAVE : DT LEV. =',F12.1)
!/T 9012 FORMAT (' TEST W3WAVE : DT CUR. =',F12.1/                    &
!/T              '                        ',F12.1/                    &
!/T              '                        ',F12.1)
!/T 9013 FORMAT (' TEST W3WAVE : DT WIND =',F12.1/                    &
!/T              '                        ',F12.1/                    &
!/T              '                        ',F12.1)
!/T 9014 FORMAT (' TEST W3WAVE : DT ICE  =',F12.1)
!/T 9015 FORMAT (' TEST W3WAVE : DT IC1  =',F12.1)
!/T 9016 FORMAT (' TEST W3WAVE : DT IC5  =',F12.1)
!/T 9020 FORMAT (' TEST W3WAVE : IT0, NT, DTG :',2I4,F8.1)
!/T 9021 FORMAT (' TEST W3WAVE : ITIME etc',I6,I4,I10.8,I7.6,1X,2L1,  &
!/T                                         2F6.2,F7.1,F6.2)
!/T 9022 FORMAT (' TEST W3WAVE : SKIP TO 400 IN 3.5')
!/T 9023 FORMAT (' TEST W3WAVE : SKIP TO 380 IN 3.5')
!/T 9030 FORMAT (' TEST W3WAVE : END OF COMPUTATION LOOP')
!/T 9040 FORMAT (' TEST W3WAVE : CHECKING FOR OUTPUT'/                &
!/T              '               TOFRST           :',I9.8,I7.6/       &
!/T              '               TND              :',I9.8,I7.6/       &
!/T              '               DTTST[1], FLAG_O :',2F8.1,L4)
!/T 9041 FORMAT (' TEST W3WAVE : PERFORMING OUTPUT')
!/T 9042 FORMAT (' TEST W3WAVE : OUTPUT COMPUTATION FLAGS: ',3L2)
!/MPIT 9043 FORMAT (' TEST W3WAVE : TYPE, NRQ, NRQMAX, NA : ',A2,3I6)
!/T 9044 FORMAT (' TEST W3WAVE : END OF OUTPUT')
!/
!/ End of W3WAVE ----------------------------------------------------- /
!/
      END SUBROUTINE W3WAVE
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3GATH ( ISPEC, FIELD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         26-Dec-2012 |
!/                  +-----------------------------------+
!/
!/    04-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    13-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    29-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    13-Jun-2006 : Split STORE in G/SSTORE             ( version 3.09 )
!/    26-Dec-2012 : Move FIELD init. to W3GATH.         ( version 4.OF )
!/
!  1. Purpose :
!
!     Gather spectral bin information into a propagation field array.
!
!  2. Method :
!
!     Direct copy or communication calls (MPP version).
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       ISPEC   Int.   I   Spectral bin considered.
!       FIELD   R.A.   O   Full field to be propagated.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!
!      MPI_STARTALL, MPI_WAITALL
!                Subr. mpif.h   MPI persistent comm. routines (!/MPI).
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
!       None.
!
!  7. Remarks :
!
!     - The field is extracted but not converted.
!     - MPI version requires posing of send and receive calls in
!       W3WAVE to match local calls.
!     - MPI version does not require an MPI_TESTALL call for the
!       posted gather operation as MPI_WAITALL is mandatory to
!       reset persistent communication for next time step.
!     - MPI version allows only two new pre-fetch postings per
!       call to minimize chances to be slowed down by gathers that
!       are not yet needed, while maximizing the pre-loading
!       during the early (low-frequency) calls to the routine
!       where the amount of calculation needed for proagation is
!       the largest.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/SHRD  Switch for message passing method.
!     !/MPI   Id.
!
!     !/S     Enable subroutine tracing.
!     !/MPIT  MPI test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/S      USE W3SERVMD, ONLY: STRACE
!/
      USE W3GDATMD, ONLY: NSPEC, NX, NY, NSEA, NSEAL, MAPSF, DMIN
      USE W3PARALL, ONLY: INIT_GET_ISEA
      USE W3WDATMD, ONLY: A => VA
!/MPI      USE W3ADATMD, ONLY: MPIBUF, BSTAT, IBFLOC, ISPLOC, BISPL, &
!/MPI                          NSPLOC, NRQSG2, IRQSG2, GSTORE
!/MPI      USE W3ODATMD, ONLY: NDST, IAPROC, NAPROC, NOTYPE
!/
      IMPLICIT NONE
!
!/MPI      INCLUDE "mpif.h"
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: ISPEC
      REAL, INTENT(OUT)       :: FIELD(1-NY:NY*(NX+2))
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
!/SHRD      INTEGER                 :: ISEA, IXY
!/MPI      INTEGER                 :: STATUS(MPI_STATUS_SIZE,NSPEC),  &
!/MPI                                 IOFF, IERR_MPI, JSEA, ISEA,     &
!/MPI                                 IXY, IS0, IB0, NPST, J
!/S      INTEGER, SAVE           :: IENT
!/MPIT      CHARACTER(LEN=15)       :: STR(MPIBUF), STRT
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3GATH')
!
       FIELD  = 0.
!
! 1.  Shared memory version ------------------------------------------ /
!
!/SHRD      DO ISEA=1, NSEA
!/SHRD        IXY        = MAPSF(ISEA,3)
!/SHRD        FIELD(IXY) = A(ISPEC,ISEA)
!/SHRD        END DO
!
!/SHRD      RETURN
!
! 2.  Distributed memory version ( MPI ) ----------------------------- /
! 2.a Update counters
!
!/MPI      ISPLOC = ISPLOC + 1
!/MPI      IBFLOC = IBFLOC + 1
!/MPI      IF ( IBFLOC .GT. MPIBUF ) IBFLOC = 1
!
!/MPIT      IF ( ISPLOC .EQ. 1 ) THEN
!/MPIT          STR = '--------------+'
!/MPIT          WRITE (NDST,9000) STR
!/MPIT        END IF
!/MPIT      STR    = '              |'
!/MPIT      STRT   = STR(IBFLOC)
!/MPIT      STRT(9:9) = 'A'
!
! 2.b Check status of present buffer
! 2.b.1 Scatter (send) still in progress, wait to end
!
!/MPI      IF ( BSTAT(IBFLOC) .EQ. 2 ) THEN
!/MPI          IOFF =  1 + (BISPL(IBFLOC)-1)*NRQSG2
!/MPI          IF ( NRQSG2 .GT. 0 ) CALL                              &
!/MPI               MPI_WAITALL ( NRQSG2, IRQSG2(IOFF,2),             &
!/MPI                             STATUS, IERR_MPI )
!/MPI          BSTAT(IBFLOC) = 0
!/MPIT          STRT(13:13) = 'S'
!/MPI        END IF
!
! 2.b.2 Gather (recv) not yet posted, post now
!
!/MPI      IF ( BSTAT(IBFLOC) .EQ. 0 ) THEN
!/MPI          BSTAT(IBFLOC) = 1
!/MPI          BISPL(IBFLOC) = ISPLOC
!/MPI          IOFF =  1 + (ISPLOC-1)*NRQSG2
!/MPI          IF ( NRQSG2 .GT. 0 ) CALL                              &
!/MPI               MPI_STARTALL ( NRQSG2, IRQSG2(IOFF,1), IERR_MPI )
!/MPIT          STRT(10:10) = 'g'
!/MPI        END IF
!
! 2.c Put local spectral densities in store
!
!/MPI      DO JSEA=1, NSEAL
!/MPI        CALL INIT_GET_ISEA(ISEA, JSEA)
!/MPI        GSTORE(ISEA,IBFLOC) = A(ISPEC,JSEA)
!/MPI        END DO
!
! 2.d Wait for remote spectral densities
!
!/MPI      IOFF =  1 + (BISPL(IBFLOC)-1)*NRQSG2
!/MPI      IF ( NRQSG2 .GT. 0 ) CALL                                  &
!/MPI           MPI_WAITALL ( NRQSG2, IRQSG2(IOFF,1), STATUS, IERR_MPI )
!
!/MPIT      STRT(11:11) = 'G'
!/MPIT      WRITE (STRT(1:7),'(I2,I5)') BSTAT(IBFLOC), ISPLOC
!/MPIT      STR(IBFLOC) = STRT
!
! 2.e Convert storage array to field.
!
!/MPI      DO ISEA=1, NSEA
!/MPI        IXY        = MAPSF(ISEA,3)
!/MPI        FIELD(IXY) = GSTORE(ISEA,IBFLOC)
!/MPI        END DO
!
! 2.f Pre-fetch data in available buffers
!
!/MPI      IS0    = ISPLOC
!/MPI      IB0    = IBFLOC
!/MPI      NPST   = 0
!
!/MPI      DO J=1, MPIBUF-1
!/MPI        IS0    = IS0 + 1
!/MPI        IF ( IS0 .GT. NSPLOC ) EXIT
!/MPI        IB0    = 1 + MOD(IB0,MPIBUF)
!/MPI        IF ( BSTAT(IB0) .EQ. 0 ) THEN
!/MPI            BSTAT(IB0) = 1
!/MPI            BISPL(IB0) = IS0
!/MPI            IOFF       = 1 + (IS0-1)*NRQSG2
!/MPI            IF ( NRQSG2 .GT. 0 ) CALL                            &
!/MPI                 MPI_STARTALL ( NRQSG2, IRQSG2(IOFF,1), IERR_MPI )
!/MPI            NPST       = NPST + 1
!/MPIT            STRT        = STR(IB0)
!/MPIT            STRT(10:10) = 'g'
!/MPIT            WRITE (STRT(1:7),'(I2,I5)') BSTAT(IB0), BISPL(IB0)
!/MPIT            STR(IB0)    = STRT
!/MPI          END IF
!/MPI        IF ( NPST .GE. 2 ) EXIT
!/MPI        END DO
!
! 2.g Test output
!
!/MPIT      DO IB0=1, MPIBUF
!/MPIT        STRT   = STR(IB0)
!/MPIT        IF ( STRT(2:2) .EQ. ' ' ) THEN
!/MPIT            IF ( BSTAT(IB0) .EQ. 0 ) THEN
!/MPIT                WRITE (STRT(1:2),'(I2)') BSTAT(IB0)
!/MPIT              ELSE
!/MPIT                WRITE (STRT(1:7),'(I2,I5)') BSTAT(IB0), BISPL(IB0)
!/MPIT              END IF
!/MPIT            STR(IB0) = STRT
!/MPIT          END IF
!/MPIT        END DO
!/MPIT      WRITE (NDST,9010) ISPLOC, STR
!
!/MPI      RETURN
!
! Formats
!
!/MPIT 9000 FORMAT ( ' TEST OF BUFFER MANAGEMENT MPI :'/              &
!/MPIT               ' -------------------------------'/              &
!/MPIT      '      RECORDS ALTERNATELY WRITTEN BY W3GATH AND W3SCAT'/ &
!/MPIT      '      FRIST COLLUMN  : LOCAL ISPEC'/                     &
!/MPIT      '      OTHER COLLUMNS : BUFFER STATUS INDICATOR '/        &
!/MPIT      '                        0 : INACTIVE'/                   &
!/MPIT      '                        1 : RECEIVING'/                  &
!/MPIT      '                        2 : SENDING'/                    &
!/MPIT      '                       LOCAL ISPEC FOR BUFFER'/          &
!/MPIT      '                       A  : ACTIVE BUFFER'/              &
!/MPIT      '                       g/G: START/FINISH RECIEVE'/       &
!/MPIT      '                       s/S: START/FINISH SEND'/          &
!/MPIT      ' +-----+',8A15)
!/MPIT 9010 FORMAT ( ' |',I4,' |',8A15)
!/
!/ End of W3GATH ----------------------------------------------------- /
!/
      END SUBROUTINE W3GATH
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3SCAT ( ISPEC, MAPSTA, FIELD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         13-Jun-2006 |
!/                  +-----------------------------------+
!/
!/    04-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    13-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    07-Sep-2005 : Updated boundary conditions.        ( version 3.08 )
!/    13-Jun-2006 : Split STORE in G/SSTORE             ( version 3.09 )
!/
!  1. Purpose :
!
!     'Scatter' data back to spectral storage after propagation.
!
!  2. Method :
!
!     Direct copy or communication calls (MPP version).
!     See also W3GATH.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       ISPEC   Int.   I   Spectral bin considered.
!       MAPSTA  I.A.   I   Status map for spatial grid.
!       FIELD   R.A.   I   Full field to be propagated.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!
!      MPI_STARTALL, MPI_WAITALL, MPI_TESTALL
!                Subr. mpif.h   MPI persistent comm. routines (!/MPI).
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
!     None.
!
!  7. Remarks :
!
!     - The field is put back but not converted !
!     - MPI persistent communication calls initialize in W3MPII.
!     - See W3GATH and W3MPII for additional comments on data
!       buffering.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/SHRD  Switch for message passing method.
!     !/MPI   Id.
!
!     !/S     Enable subroutine tracing.
!     !/MPIT  MPI test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/S      USE W3SERVMD, ONLY: STRACE
!/
      USE W3GDATMD, ONLY: NSPEC, NX, NY, NSEA, NSEAL, MAPSF
      USE W3WDATMD, ONLY: A => VA
!/MPI      USE W3ADATMD, ONLY: MPIBUF, BSTAT, IBFLOC, ISPLOC, BISPL, &
!/MPI                          NSPLOC, NRQSG2, IRQSG2, SSTORE
      USE W3ODATMD, ONLY: NDST
!/MPI      USE W3ODATMD, ONLY: IAPROC, NAPROC
      USE CONSTANTS, ONLY : LPDLIB
      USE W3PARALL, only: INIT_GET_ISEA
!/
      IMPLICIT NONE
!
!/MPI      INCLUDE "mpif.h"
!/ 
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: ISPEC, MAPSTA(NY*NX)
      REAL, INTENT(IN)        :: FIELD(1-NY:NY*(NX+2))
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
!/SHRD      INTEGER                 :: ISEA, IXY
!/MPI      INTEGER                 :: ISEA, IXY, IOFF, IERR_MPI, J,   &
!/MPI                                 STATUS(MPI_STATUS_SIZE,NSPEC),  &
!/MPI                                 JSEA, IB0
!/S      INTEGER, SAVE           :: IENT
!/MPIT      CHARACTER(LEN=15)       :: STR(MPIBUF), STRT
!/MPI      LOGICAL                 :: DONE
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3SCAT')
!
! 1.  Shared memory version ------------------------------------------ *
!
!/SHRD      DO ISEA=1, NSEA
!/SHRD        IXY           = MAPSF(ISEA,3)
!/SHRD        IF ( MAPSTA(IXY) .GE. 1 ) A(ISPEC,ISEA) = FIELD(IXY)
!/SHRD        END DO
!
!/SHRD      RETURN
!
! 2.  Distributed memory version ( MPI ) ----------------------------- *
! 2.a Initializations
!
!/MPIT      DO IB0=1, MPIBUF
!/MPIT        STR(IB0) = '              |'
!/MPIT        END DO
!
!/MPIT      STRT   = STR(IBFLOC)
!/MPIT      STRT(9:9) = 'A'
!
! 2.b Convert full grid to sea grid, active points only
!
!/MPI      DO ISEA=1, NSEA
!/MPI        IXY    = MAPSF(ISEA,3)
!/MPI        IF ( MAPSTA(IXY) .GE. 1 ) SSTORE(ISEA,IBFLOC) = FIELD(IXY)
!/MPI        END DO
!
! 2.c Send spectral densities to appropriate remote
!
!/MPI      IOFF   = 1 + (ISPLOC-1)*NRQSG2
!/MPI      IF ( NRQSG2 .GT. 0 ) CALL                                  &
!/MPI           MPI_STARTALL ( NRQSG2, IRQSG2(IOFF,2), IERR_MPI )
!/MPI      BSTAT(IBFLOC) = 2
!/MPIT      STRT(12:12) = 's'
!/MPIT      WRITE (STRT(1:7),'(I2,I5)') BSTAT(IBFLOC), ISPLOC
!/MPIT      STR(IBFLOC) = STRT
!
! 2.d Save locally stored results
!
!/MPI      DO JSEA=1, NSEAL
!/MPI        CALL INIT_GET_ISEA(ISEA, JSEA)
!/MPI        IXY    = MAPSF(ISEA,3)
!/MPI        IF (MAPSTA(IXY) .GE. 1) A(ISPEC,JSEA) = SSTORE(ISEA,IBFLOC)
!/MPI        END DO
!
! 2.e Check if any sends have finished
!
!/MPI      IB0    = IBFLOC
!
!/MPI      DO J=1, MPIBUF
!/MPI        IB0    = 1 + MOD(IB0,MPIBUF)
!/MPI        IF ( BSTAT(IB0) .EQ. 2 ) THEN
!/MPI            IOFF   = 1 + (BISPL(IB0)-1)*NRQSG2
!/MPI            IF ( NRQSG2 .GT. 0 ) THEN
!/MPI               CALL MPI_TESTALL ( NRQSG2, IRQSG2(IOFF,2), DONE,  &
!/MPI                                 STATUS, IERR_MPI )
!/MPI              ELSE
!/MPI                DONE   = .TRUE.
!/MPI              END IF
!/MPI            IF ( DONE .AND. NRQSG2.GT.0 ) CALL                   &
!/MPI                     MPI_WAITALL ( NRQSG2, IRQSG2(IOFF,2),       &
!/MPI                                   STATUS, IERR_MPI )
!/MPI            IF ( DONE ) THEN
!/MPI                BSTAT(IB0) = 0
!/MPIT                STRT        = STR(IB0)
!/MPIT                WRITE (STRT(1:7),'(I2,I5)') BSTAT(IB0), BISPL(IB0)
!/MPIT                STRT(13:13) = 'S'
!/MPIT                STR(IB0)    = STRT
!/MPI              END IF
!/MPI          END IF
!/MPI        END DO
!
! 2.f Last component, finish message passing, reset buffer control
!
!/MPI      IF ( ISPLOC .EQ. NSPLOC ) THEN
!
!/MPI          DO IB0=1, MPIBUF
!/MPI            IF ( BSTAT(IB0) .EQ. 2 ) THEN
!/MPI                IOFF   = 1 + (BISPL(IB0)-1)*NRQSG2
!/MPI                IF ( NRQSG2 .GT. 0 ) CALL                        &
!/MPI                     MPI_WAITALL ( NRQSG2, IRQSG2(IOFF,2),       &
!/MPI                                   STATUS, IERR_MPI )
!/MPI                BSTAT(IB0) = 0
!/MPIT                STRT        = STR(IB0)
!/MPIT                WRITE (STRT(1:7),'(I2,I5)') BSTAT(IB0), BISPL(IB0)
!/MPIT                STRT(13:13) = 'S'
!/MPIT                STR(IB0)    = STRT
!/MPI              END IF
!/MPI            END DO
!
!/MPI          ISPLOC = 0
!/MPI          IBFLOC = 0
!
!/MPI        END IF
!
! 2.g Test output
!
!/MPIT      DO IB0=1, MPIBUF
!/MPIT        STRT   = STR(IB0)
!/MPIT        IF ( STRT(2:2) .EQ. ' ' ) THEN
!/MPIT            IF ( BSTAT(IB0) .EQ. 0 ) THEN
!/MPIT                WRITE (STRT(1:2),'(I2)') BSTAT(IB0)
!/MPIT              ELSE
!/MPIT                WRITE (STRT(1:7),'(I2,I5)') BSTAT(IB0), BISPL(IB0)
!/MPIT              END IF
!/MPIT            STR(IB0) = STRT
!/MPIT          END IF
!/MPIT        END DO
!
!/MPIT      WRITE (NDST,9000) STR
!
!/MPIT      IF ( ISPLOC .EQ. 0 ) THEN
!/MPIT          DO IB0=1, MPIBUF
!/MPIT            STR(IB0) = '--------------+'
!/MPIT            END DO
!/MPIT          WRITE (NDST,9010) STR
!/MPIT          WRITE (NDST,*)
!/MPIT        END IF
!
!/MPI      RETURN
!
! Formats
!
!/MPIT 9000 FORMAT ( ' |     |',8A15)
!/MPIT 9010 FORMAT ( ' +-----+',8A15)
!/
!/ End of W3SCAT ----------------------------------------------------- /
!/
      END SUBROUTINE W3SCAT
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3NMIN ( MAPSTA, FLAG0 )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         28-Dec-2004 |
!/                  +-----------------------------------+
!/
!/    23-Feb-2001 : Origination.                        ( version 2.07 )
!/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/
!  1. Purpose :
!
!     Check minimum number of active sea points at given processor to
!     evaluate the need for a MPI_BARRIER call.
!
!  2. Method :
!
!     Evaluate mapsta.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       MAPSTA  I.A.   I   Status map for spatial grid.
!       FLAG0   log.   O   Flag to identify 0 as minimum.
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
!     !/S     Enable subroutine tracing.
!     !/T     Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/S      USE W3SERVMD, ONLY: STRACE
!/
      USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSF
      USE W3ODATMD, ONLY: NDST, NAPROC
      USE W3PARALL, ONLY: INIT_GET_JSEA_ISPROC
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: MAPSTA(NY*NX)
      LOGICAL, INTENT(OUT)    :: FLAG0
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NMIN, IPROC, NLOC, ISEA, IXY
      INTEGER                 :: JSEA, ISPROC
!/S      INTEGER, SAVE           :: IENT
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3NMIN')
!
      NMIN   = NSEA
!
      DO IPROC=1, NAPROC
        NLOC   = 0
        DO ISEA=1, NSEA
          CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
          IF (ISPROC .eq. IPROC) THEN
            IXY    = MAPSF(ISEA,3)
            IF ( MAPSTA(IXY) .EQ. 1 ) NLOC = NLOC + 1
          END IF
        END DO
!/SMC !!Li   For SMC grid, local sea points are equally NSEA/NAPROC
!/SMC !!Li   so the NLOC is overwirte by a constant.  
!/SMC        NLOC = NSEA/NAPROC
!
!/T        WRITE (NDST,9000) IPROC, NLOC
        NMIN   = MIN ( NMIN , NLOC )
        END DO
!
      FLAG0  = NMIN .EQ. 0
!/T      WRITE (NDST,9001) NMIN, FLAG0
!
      RETURN
!
! Formats
!
!/T 9000 FORMAT ( ' TEST W3NMIN : IPROC =',I3,'  NLOC =',I5)
!/T 9001 FORMAT ( ' TEST W3NMIN : NMIN =',I5,'  FLAG0 =',L2)
!/
!/ End of W3NMIN ----------------------------------------------------- /
!/
      END SUBROUTINE W3NMIN
!/
!/ End of module W3WAVEMD -------------------------------------------- /
!/
      END MODULE W3WAVEMD
