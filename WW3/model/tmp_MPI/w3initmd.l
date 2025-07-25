                                                         w3initmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3INITMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         25-Sep-2020 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    28-Dec-2004 : Origination (out of W3WAVEMD).      ( version 3.06 )
   13 !/                  Multiple grid version.
   14 !/    03-Jan-2005 : Add US2x to MPI communication.      ( version 3.06 )
   15 !/    04-Jan-2005 : Add grid output flags to W3INIT.    ( version 3.06 )
   16 !/    07-Feb-2005 : Combined vs. separate test output.  ( version 3.07 )
   17 !/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
   18 !/    21-Jul-2005 : Add output fields.                  ( version 3.07 )
   19 !/    09-Nov-2005 : Drying out of points added.         ( version 3.08 )
   20 !/    13-Jun-2006 : Splitting STORE in G/SSTORE.        ( version 3.09 )
   21 !/    26-Jun-2006 : adding wiring for output type 6.    ( version 3.09 )
   22 !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   23 !/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
   24 !/    02-Aug-2006 : Adding W3MPIP.                      ( version 3.10 )
   25 !/    02-Nov-2006 : Adding partitioning options.        ( version 3.10 )
   26 !/    11-Jan-2007 : Updating IAPPRO computation.        ( version 3.10 )
   27 !/    02-Apr-2007 : Add partitioned field data.         ( version 3.11 )
   28 !/                  Add user-defined field data.
   29 !/    01-May-2007 : Move O7a output to W3IOPP.          ( version 3.11 )
   30 !/    08-May-2007 : Starting from calm as an option.    ( version 3.11 )
   31 !/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
   32 !/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
   33 !/    29-Feb-2008 : Add NEC compiler directives.        ( version 3.13 )
   34 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   35 !/    23-Jul-2009 : Implement unstructured grids        ( version 3.14 )
   36 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   37 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   38 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
   39 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   40 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
   41 !/                  specify index closure for a grid.   ( version 3.14 )
   42 !/                  (T. J. Campbell, NRL)
   43 !/    02-Sep.2012 : Set up for > 999 test files.        ( version 4.10 )
   44 !/                  Reset UST initialization.
   45 !/    03-Sep-2012 : Switch test file on/off (TSTOUT)    ( version 4.10 )
   46 !/    03-Sep-2012 : Clean up of UG grids                ( version 4.08 )
   47 !/    30-Sep-2012 : Implemetation of tidal constituents ( version 4.09 )
   48 !/    07-Dec-2012 : Initialize UST non-zero.            ( version 4.11 )
   49 !/    12-Dec-2012 : Changes for SMC grid.  JG_Li        ( version 4.11 )
   50 !/    26-Dec-2012 : Modify field output MPI for new     ( version 4.11 )
   51 !/                  structure and smaller memory footprint.
   52 !/    02-Jul-2013 : Bug fix MPI_FLOAT -> MPI_REAL.      ( version 4.11 )
   53 !/    10-Oct-2013 : CG and WN values at DMIN for ISEA=0 ( version 4.12 )
   54 !/    14-Nov-2013 : Remove UST(DIR) initialization.     ( version 4.13 )
   55 !/    15-Dec-2013 : Adds fluxes to ice                  ( version 5.01 )
 
 
                                                         w3initmd.F90  page   2
 
 
   56 !/    01-May-2017 : Adds directional MSS parameters     ( version 6.04 )
   57 !/    05-Jun-2018 : Adds PDLIB/MEMCHECK/DEBUG           ( version 6.04 )
   58 !/    21-Aug-2018 : Add WBT parameter                   ( version 6.06 )
   59 !/    26-Aug-2018 : UOST (Mentaschi et al. 2015, 2018)  ( version 6.06 )
   60 !/    25-Sep-2020 : Extra fields for coupling restart   ( version 7.10 )
   61 !/
   62 !/    Copyright 2009-2013 National Weather Service (NWS),
   63 !/       National Oceanic and Atmospheric Administration.  All rights
   64 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   65 !/       No unauthorized use without permission.
   66 !/
   67 !/    Note: Changes in version numbers not logged above.
   68 !/
   69 !  1. Purpose :
   70 !
   71 !  2. Variables and types :
   72 !
   73 !      Name      Type  Scope    Description
   74 !     ----------------------------------------------------------------
   75 !      CRITOS    R.P.  Public   Critical percentage of resources used
   76 !                               for output to trigger warning.
   77 !      WWVER     C*10  Public   Model version number.
   78 !      SWITCHES  C*256 Public   switches taken from bin/switch
   79 !     ----------------------------------------------------------------
   80 !
   81 !  3. Subroutines and functions :
   82 !
   83 !      Name      Type  Scope    Description
   84 !     ----------------------------------------------------------------
   85 !      W3INIT    Subr. Public   Wave model initialization.
   86 !      W3MPII    Subr. Public   Initialize MPI data transpose.
   87 !      W3MPIO    Subr. Public   Initialize MPI output gathering.
   88 !      W3MPIP    Subr. Public   Initialize MPI point output gathering.
   89 !     ----------------------------------------------------------------
   90 !
   91 !  4. Subroutines and functions used :
   92 !
   93 !     See subroutine documentation.
   94 !
   95 !  5. Remarks :
   96 !
   97 !  6. Switches :
   98 !
   99 !       !/SHRD  Switch for shared / distributed memory architecture.
  100 !       !/DIST  Id.
  101 !       !/MPI   Id.
  102 !
  103 !       !/S     Enable subroutine tracing.
  104 !       !/Tn    Enable test output.
  105 !       !/MPIT  Enable test output (MPI).
  106 !
  107 !  7. Source code :
  108 !
  109 !/ ------------------------------------------------------------------- /
  110       PUBLIC
 
 
                                                         w3initmd.F90  page   3
 
 
  111 !/
  112       REAL, PARAMETER                :: CRITOS = 15.
  113       CHARACTER(LEN=10), PARAMETER   :: WWVER  = '7.12  '
  114       CHARACTER(LEN=512), PARAMETER  :: SWITCHES  = &
  115                     'F90 NOGRB NC4 DIST MPI PR3 UQ FLX0 LN1 ST4 STAB0 NL1>
  116                     ' RWND CRT1 CRX1 IG1 TRKNC COAWST O0 O1 O2 O2a O2b O2>
  117                     ''
  118 !/
  119       CONTAINS
  120 !/ ------------------------------------------------------------------- /
  121       SUBROUTINE W3INIT ( IMOD, IsMulti, FEXT, MDS, MTRACE, ODAT      &
  122                           , FLGRD,                               &
  123                            FLGR2, FLGD, FLG2, NPT, XPT, YPT, PNAMES,   &
  124                           IPRT, PRTFRM, MPI_COMM, FLAGSTIDEIN)
  125                                                               
  126 !/
  127 !/                  +-----------------------------------+
  128 !/                  | WAVEWATCH III           NOAA/NCEP |
  129 !/                  |           H. L. Tolman            |
  130 !/                  |                        FORTRAN 90 |
  131 !/                  | Last update :         03-Sep-2012 |
  132 !/                  +-----------------------------------+
  133 !/
  134 !/    17-Mar-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
  135 !/    13-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
  136 !/                  Major changes to logistics.
  137 !/    14-Feb-2000 : Exact-NL added.                     ( version 2.01 )
  138 !/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
  139 !/    24-Jan-2002 : Zero time step for data ass.        ( version 2.17 )
  140 !/    18-Feb-2002 : Point output diagnostics added.     ( version 2.18 )
  141 !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
  142 !/    20-Aug-2003 : Output server options added.        ( version 3.04 )
  143 !/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
  144 !/                  Taken out of W3WAVE.
  145 !/    04-Jan-2005 : Add grid output flags to par list.  ( version 3.06 )
  146 !/    07-Feb-2005 : Combined vs. separate test output.  ( version 3.07 )
  147 !/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
  148 !/    09-Nov-2005 : Drying out of points added.         ( version 3.08 )
  149 !/    26-Jun-2006 : adding wiring for output type 6.    ( version 3.09 )
  150 !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
  151 !/    02-Aug-2006 : Adding W3MPIP.                      ( version 3.10 )
  152 !/    02-Nov-2006 : Adding partitioning options.        ( version 3.10 )
  153 !/    11-Jan-2007 : Updating IAPPRO computation.        ( version 3.10 )
  154 !/    01-May-2007 : Move O7a output to W3IOPP.          ( version 3.11 )
  155 !/    08-May-2007 : Starting from calm as an option.    ( version 3.11 )
  156 !/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
  157 !/    21-Jun-2007 : Dedicated output processes.         ( version 3.11
  158 !/    13-Sep-2009 : Add coupling option                 ( version 3.14 )
  159 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
  160 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  161 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
  162 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  163 !/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.1 )
  164 !/                  (A. Roland and F. Ardhuin)
  165 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
 
 
                                                         w3initmd.F90  page   4
 
 
  166 !/                  specify index closure for a grid.   ( version 3.14 )
  167 !/                  (T. J. Campbell, NRL)
  168 !/    02-Sep.2012 : Set up for > 999 test files.        ( version 4.10 )
  169 !/    03-Sep-2012 : Switch test file on/off (TSTOUT)    ( version 4.10 )
  170 !/    03-Sep-2012 : Clean up of UG grids                ( version 4.08 )
  171 !/
  172 !  1. Purpose :
  173 !
  174 !     Initialize WAVEWATCH III.
  175 !
  176 !  2. Method :
  177 !
  178 !     Initialize data structure and wave fields from data files.
  179 !     Initialize grid from local and instantaneous data.
  180 !
  181 !  3. Parameters :
  182 !
  183 !     Parameter list
  184 !     ----------------------------------------------------------------
  185 !       IMOD    Int.   I   Model number.
  186 !       FEXT    Char   I   Extension of data files.
  187 !       MDS     I.A.   I   Array with dataset numbers (see below),
  188 !                          saved as NDS in W3ODATMD.
  189 !                           1: General output unit number ("log file").
  190 !                           2: Error output unit number.
  191 !                           3: Test output unit number.
  192 !                           4: "screen", i.e., direct output location,
  193 !                              can be the screen or the output file of
  194 !                              the shell.
  195 !                           5: Model definition file unit number.
  196 !                           6: Restart file unit number.
  197 !                           7: Grid output file unit number.
  198 !                           8: Point output file unit number.
  199 !                           9: Input boundary data file unit number.
  200 !                          10: Output boundary data file unit number
  201 !                              (first).
  202 !                          11: Track information file unit number.
  203 !                          12: Track output file unit number.
  204 !       MTRACE  I.A.   I   Array with subroutine tracing information.
  205 !                           1: Output unit number for trace.
  206 !                           2: Maximum number of trace prints.
  207 !       ODAT    I.A.   I   Output data, five parameters per output type
  208 !                           1-5  Data for OTYPE = 1; gridded fields.
  209 !                                1 YYYMMDD for first output.
  210 !                                2 HHMMSS for first output.
  211 !                                3 Output interval in seconds.
  212 !                                4 YYYMMDD for last output.
  213 !                                5 HHMMSS for last output.
  214 !                           6-10 Id. for OTYPE = 2; point output.
  215 !                          11-15 Id. for OTYPE = 3; track point output.
  216 !                          16-20 Id. for OTYPE = 4; restart files.
  217 !                          21-25 Id. for OTYPE = 5; boundary data.
  218 !                          31-35 Id. for OTYPE = 7; coupling data.
  219 !                          36-40 Id. for OTYPE = 8; second restart file
  220 !       FLGRD   L.A.   I   Flags for gridded output.
 
 
                                                         w3initmd.F90  page   5
 
 
  221 !       FLGR2   L.A.   I   Flags for coupling output.
  222 !       NPT     Int.   I   Number of output points
  223 !       X/YPT   R.A.   I   Coordinates of output points.
  224 !       PNAMES  C.A.   I   Output point names.
  225 !       IPRT    I.A.   I   Partitioning grid info.
  226 !       PRTFRM  I.A.   I   Partitioning format flag.
  227 !       MPI_COMM Int.  I   MPI communicator to be used for model.
  228 !     ----------------------------------------------------------------
  229 !
  230 !  4. Subroutines used :
  231 !
  232 !      Name      Type  Module   Description
  233 !     ----------------------------------------------------------------
  234 !      W3SETG    Subr. W3GDATMD Point to data structure.
  235 !      W3SETW    Subr. W3WDATMD Point to data structure.
  236 !      W3DIMW    Subr.   Id.    Set array sizes in data structure.
  237 !      W3SETA    Subr. W3ADATMD Point to data structure.
  238 !      W3DIMA    Subr.   Id.    Set array sizes in data structure.
  239 !      W3SETI    Subr. W3IDATMD Point to data structure.
  240 !      W3DIMI    Subr.   Id.    Set array sizes in data structure.
  241 !      W3SETO    Subr. W3ODATMD Point to data structure.
  242 !      W3DMO5    Subr.   Id.    Set array sizes in data structure.
  243 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
  244 !      STRACE    Subr.   Id.    Subroutine tracing.
  245 !      EXTCDE    Subr.   Id.    Program abort.
  246 !      WWDATE    Subr.   Id.    System date.
  247 !      WWTIME    Subr.   Id.    System time.
  248 !      DSEC21    Func. W3TIMEMD Compute time difference.
  249 !      TICK21    Func.   Id.    Advance the clock.
  250 !      STME21    Func.   Id.    Print the time readable.
  251 !      PRTBLK    Func. W3ARRYMD Print plot of array.
  252 !      W3IOGR    Subr. W3IOGRMD Read/write model definition file.
  253 !      W3IORS    Subr. W3IORSMD Read/write restart file.
  254 !      W3IOPP    Subr. W3IOPOMD Preprocess point output.
  255 !      CALL MPI_COMM_SIZE, CALL MPI_COMM_RANK
  256 !                Subr. mpif.h   Standard MPI routines.
  257 !     ----------------------------------------------------------------
  258 !
  259 !  5. Called by :
  260 !
  261 !     Any program shell or integrated model which uses WAVEWATCH III.
  262 !
  263 !  6. Error messages :
  264 !
  265 !     On opening of log file only. Other error messages are generated
  266 !     by W3IOGR and W3IORS.
  267 !
  268 !  7. Remarks :
  269 !
  270 !     - The log file is called 'log.FEXT', where FEXT is passed to
  271 !       the routine.
  272 !     - The test output file is called 'test.FEXT' in shared memory
  273 !       version or testNNN.FEXT in distributed memory version.
  274 !     - A water level and ice coverage are transferred with the
  275 !       restart file. To assure consistency within the model, the
 
 
                                                         w3initmd.F90  page   6
 
 
  276 !       water level and ice coverage are re-evaluated at the 0th
  277 !       time step in the actual wave model routine.
  278 !     - When running regtests in cases where disk is non-local
  279 !       (i.e. NFS used), there can be a huge improvment in compute
  280 !       time by using /var/tmp/ for log files.
  281 !       See commented line at "OPEN (MDS(1),FILE=..."
  282 !
  283 !  8. Structure :
  284 !
  285 !     ----------------------------------------------------
  286 !      1.  Set-up of idata structures and I/O.
  287 !        a Point to proper data structures.
  288 !        b Number of processors and processor number.
  289 !        c Open files.
  290 !        d Dataset unit numbers
  291 !        e Subroutine tracing
  292 !        f Initial and test outputs
  293 !      2.  Model definition.
  294 !        a Read model definition file         ( W3IOGR )
  295 !        b Save MAPSTA.
  296 !        c MPP preparation
  297 !      3.  Model initialization.
  298 !        a Read restart file.                 ( W3IORS )
  299 !        b Compare grid and restart MAPSTA.
  300 !        c Initialize with winds if requested (set flag).
  301 !        d Initialize calm conditions if requested.
  302 !        e Preparations for prop. scheme.
  303 !      4.  Set-up output times.
  304 !        a Unpack ODAT.
  305 !        b Check if output available.
  306 !        c Get first time per output and overall.
  307 !        d Prepare point output               ( W3IOPP )
  308 !      5.  Define wavenumber grid.
  309 !        a Calculate depth.
  310 !        b Fill wavenumber and group velocity arrays.
  311 !      6.  Initialize arrays.
  312 !      7.  Write info to log file.
  313 !      8.  Final MPI set up  ( W3MPII , W3MPIO , W3MPIP )
  314 !     ----------------------------------------------------
  315 !
  316 !  9. Switches :
  317 !
  318 !       !/SHRD  Switch for shared / distributed memory architecture.
  319 !       !/DIST  Id.
  320 !       !/MPI   Id.
  321 !
  322 !       !/S     Enable subroutine tracing.
  323 !       !/Tn    Enable test output.
  324 !
  325 ! 10. Source code :
  326 !
  327 !/ ------------------------------------------------------------------- /
  328       USE CONSTANTS
  329 !/
  330       USE W3GDATMD, ONLY: W3SETG, RSTYPE
 
 
                                                         w3initmd.F90  page   7
 
 
  331       USE W3WDATMD, ONLY: W3SETW, W3DIMW
  332       USE W3ADATMD, ONLY: W3SETA, W3DIMA
  333       USE W3IDATMD, ONLY: W3SETI, W3DIMI
  334       USE W3ODATMD, ONLY: W3SETO, W3DMO5
  335       USE W3IOGOMD, ONLY: W3FLGRDUPDT
  336       USE W3IOGRMD, ONLY: W3IOGR
  337       USE W3IORSMD, ONLY: W3IORS
  338       USE W3IOPOMD, ONLY: W3IOPP
  339       USE W3SERVMD, ONLY: ITRACE, EXTCDE, WWDATE, WWTIME
  340       USE W3TIMEMD, ONLY: DSEC21, TICK21, STME21
  341       USE W3ARRYMD, ONLY: PRTBLK
  342 !/
  343       USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, MAPSTA, MAPST2, MAPFS, &
  344                           MAPSF, FLAGLL,   &
  345                           ICLOSE, ZB, TRNX, TRNY, DMIN, DTCFL, DTMAX, &
  346                           FLCK, NK, NTH, NSPEC, SIG, GNAME
  347       USE W3WDATMD, ONLY: TIME, TLEV, TICE, WLV, UST, USTDIR, VA
  348       USE W3ODATMD, ONLY: NDSO, NDSE, NDST, SCREEN, NDS, NTPROC,      &
  349                           NAPROC, IAPROC, NAPLOG, NAPOUT, NAPERR,     &
  350                           NAPFLD, NAPPNT, NAPTRK, NAPRST, NAPBPT,     &
  351                           NAPPRT, TOFRST, DTOUT, TONEXT, TOLAST,      &
  352                           FLOUT, FLOGRD, FLBPO, NOPTS, PTNME,         &
  353                           PTLOC, IPTINT, PTIFAC, UNDEF, IDOUT, FLBPI, &
  354                           OUTPTS, FNMPRE, IX0, IXN, IXS, IY0, IYN,    &
  355                           IYS, FLFORM, IOSTYP, UNIPTS, UPPROC, NOTYPE,&
  356                           FLOGR2, NOGRP, NGRPP, FLOGD, FLOG2
  357       USE W3ADATMD, ONLY: NSEALM, IAPPRO, FLCOLD, FLIWND, DW, CG, WN, &
  358                           UA, UD, U10, U10D, AS
  359       USE W3ADATMD, ONLY: MPI_COMM_WAVE, MPI_COMM_WCMP
  360       USE W3IDATMD, ONLY: FLLEV, FLCUR, FLWIND, FLICE, FLMDN, FLMTH,  &
  361                           FLMVS, FLIC1, FLIC2, FLIC3, FLIC4, FLIC5
  362       USE W3DISPMD, ONLY: WAVNU1, WAVNU3
  363       USE W3PARALL, ONLY : AC_tot
  364       USE W3PARALL, ONLY: SET_UP_NSEAL_NSEALM
  365      USE W3GDATMD, ONLY: GTYPE, UNGTYPE
  366       USE W3GDATMD, ONLY: FSN,FSPSI,FSFCT,FSNIMP, FSTOTALIMP, FSTOTALEXP
  367       USE W3GDATMD, ONLY: FSREFRACTION, FSFREQSHIFT
  368       USE W3PARALL, ONLY: INIT_GET_JSEA_ISPROC, INIT_GET_ISEA
  369 !/
  370       IMPLICIT NONE
  371 !
  372       INCLUDE "mpif.h"
  373 !/
  374 !/ ------------------------------------------------------------------- /
  375 !/ Parameter list
  376 !/
  377       INTEGER, INTENT(IN)           :: IMOD, MDS(13), MTRACE(2),      &
  378                                        ODAT(40),NPT, IPRT(6),&
  379                                        MPI_COMM
  380       LOGICAL, INTENT(IN)           :: IsMulti
  381       REAL, INTENT(INOUT)           :: XPT(NPT), YPT(NPT)
  382       LOGICAL, INTENT(INOUT)        :: FLGRD(NOGRP,NGRPP), FLGD(NOGRP),&
  383                                        FLGR2(NOGRP,NGRPP), FLG2(NOGRP),&
  384                                        PRTFRM
  385       CHARACTER, INTENT(IN)         :: FEXT*(*)
 
 
                                                         w3initmd.F90  page   8
 
 
  386       CHARACTER(LEN=40), INTENT(IN) :: PNAMES(NPT)
  387       LOGICAL, INTENT(IN), OPTIONAL :: FLAGSTIDEIN(4)
  388       INTEGER                       :: NSEALout, NSEALMout
  389 !/
  390 !/ ------------------------------------------------------------------- /
  391 !/ Local parameters
  392 !/
  393       integer :: IRANK, I, ISTAT
  394       INTEGER                 :: IE, IFL, IFT, IERR, NTTOT, NTLOC,    &
  395                                  NTTARG, IK, IP, ITH, IX, IY, &
  396                                  J, J0, TOUT(2), TLST(2), ISEA, IS,   &
  397                                  K, I1, I2, JSEA, NTTMAX
  398       INTEGER                 :: ISTEP, ISP, IW
  399       INTEGER                 :: IERR_MPI, BGROUP, LGROUP
  400       INTEGER, ALLOCATABLE    :: TMPRNK(:)
  401       INTEGER, ALLOCATABLE    :: NT(:), MAPTST(:,:)
  402       REAL                    :: DTTST, DEPTH, FRACOS
  403       REAL                    :: FACTOR
  404       REAL                    :: WLVeff
  405       LOGICAL                 :: OPENED
  406       CHARACTER(LEN=8)        :: STTIME
  407       CHARACTER(LEN=10)       :: STDATE
  408       INTEGER                 :: ISPROC
  409       CHARACTER(LEN=12)       :: FORMAT
  410       CHARACTER(LEN=23)       :: DTME21
  411       CHARACTER(LEN=30)       :: LFILE, TFILE
  412 !/
  413 !/ ------------------------------------------------------------------- /
  414 !
  415 ! 1.  Set-up of data structures and I/O  ----------------------------- /
  416 ! 1.a Point to proper data structures.
  417 !
  418 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 1")
  419                                               
  420       CALL W3SETO ( IMOD, MDS(2), MDS(3) )
  421       CALL W3SETG ( IMOD, MDS(2), MDS(3) )
  422       CALL W3SETW ( IMOD, MDS(2), MDS(3) )
  423       CALL W3SETA ( IMOD, MDS(2), MDS(3) )
  424       CALL W3SETI ( IMOD, MDS(2), MDS(3) )
  425 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 2")
  426                                               
  427                                               
  428 !
  429 ! 1.b Number of processors and processor number.
  430 !     Overwrite some initializations from W3ODATMD.
  431 !
  432 !     *******************************************************
  433 !     *** NOTE : OUTPUT PROCESSOR ASSIGNMENT NEEDS TO BE  ***
  434 !     ***        CONSISTENT WITH ASSIGNMENT IN WMINIT.    ***
  435 !     *******************************************************
  436 !
  437       MPI_COMM_WAVE = MPI_COMM
  438       CALL MPI_COMM_SIZE ( MPI_COMM_WAVE, NTPROC, IERR_MPI )
  439       NAPROC = NTPROC
  440       CALL MPI_COMM_RANK ( MPI_COMM_WAVE, IAPROC, IERR_MPI )
 
 
                                                         w3initmd.F90  page   9
 
 
  441       IAPROC = IAPROC + 1
  442 !
  443 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 3")
  444       IF ( IOSTYP .LE. 1 ) THEN
  445 !
  446           NAPFLD = MAX(1,NAPROC-1)
  447           NAPPNT = MAX(1,NAPROC-2)
  448           NAPTRK = MAX(1,NAPROC-5)
  449           NAPRST = NAPROC
  450           NAPBPT = MAX(1,NAPROC-3)
  451           NAPPRT = MAX(1,NAPROC-4)
  452 !
  453         ELSE
  454 !
  455           NAPPNT = NAPROC
  456           IF ( UNIPTS .AND. UPPROC ) NAPROC = MAX(1,NTPROC - 1)
  457           NAPFLD = NAPROC
  458           NAPRST = NAPROC
  459           NAPBPT = NAPROC
  460           NAPTRK = NAPROC
  461           NAPPRT = NAPROC
  462 !
  463           IF ( IOSTYP .EQ. 2 ) THEN
  464               NAPROC = MAX(1,NAPROC-1)
  465             ELSE IF ( IOSTYP .EQ. 3 ) THEN
  466 !
  467 ! For field or coupling output
  468 !
  469               IF ( ODAT( 3).GT.0 .OR.  ODAT(33).GT.0 ) THEN
  470                   NAPFLD =       NAPROC
  471                   NAPROC = MAX(1,NAPROC-1)
  472                 END IF
  473               IF ( ODAT(13).GT.0 ) THEN
  474                   NAPTRK =       NAPROC
  475                   NAPROC = MAX(1,NAPROC-1)
  476                 END IF
  477               IF ( ODAT(28).GT.0 ) THEN
  478                   NAPPRT =       NAPROC
  479                   NAPROC = MAX(1,NAPROC-1)
  480                 END IF
  481               IF ( ODAT( 8).GT.0 ) NAPPNT = NAPROC
  482               IF ( ODAT(18).GT.0 ) NAPRST = NAPROC
  483               IF ( ODAT(23).GT.0 ) NAPBPT = NAPROC
  484               IF ( ( ODAT( 8).GT.0 .OR. ODAT(18).GT.0 .OR.            &
  485                      ODAT(23).GT.0 ) ) NAPROC = MAX(1,NAPROC-1)
  486             END IF
  487         END IF
  488 !
  489 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 4")
  490       FRACOS = 100. * REAL(NTPROC-NAPROC) / REAL(NTPROC)
  491       IF ( FRACOS.GT.CRITOS .AND. IAPROC.EQ.NAPERR )                  &
  492                                            WRITE (NDSE,8002) FRACOS
  493 !
  494       IF ( NAPROC .EQ. NTPROC ) THEN
  495           MPI_COMM_WCMP = MPI_COMM_WAVE
 
 
                                                         w3initmd.F90  page  10
 
 
  496         ELSE
  497           CALL MPI_COMM_GROUP ( MPI_COMM_WAVE, BGROUP, IERR_MPI )
  498           ALLOCATE ( TMPRNK(NAPROC) )
  499           DO J=1, NAPROC
  500             TMPRNK(J) = J - 1
  501             END DO
  502           CALL MPI_GROUP_INCL ( BGROUP, NAPROC, TMPRNK, LGROUP,  &
  503                                 IERR_MPI )
  504           CALL MPI_COMM_CREATE ( MPI_COMM_WAVE, LGROUP,          &
  505                                  MPI_COMM_WCMP, IERR_MPI )
  506           CALL MPI_GROUP_FREE ( LGROUP, IERR_MPI )
  507           CALL MPI_GROUP_FREE ( BGROUP, IERR_MPI )
  508           DEALLOCATE ( TMPRNK )
  509       END IF
  510 !!!/PDLIB    CALL W3SETG(IMOD, NDSE, NDST)
  511 !
  512            LPDLIB = .FALSE.
  513            IF (FSTOTALIMP .and. .NOT. LPDLIB) THEN
  514              WRITE(NDSE,*) 'IMPTOTAL is selected'
  515              WRITE(NDSE,*) 'But PDLIB is not'
  516              STOP 'Stop, case 1'
  517            ELSE IF (FSTOTALEXP .and. .NOT. LPDLIB) THEN
  518              WRITE(NDSE,*) 'EXPTOTAL is selected'
  519              WRITE(NDSE,*) 'But PDLIB is not'
  520              STOP 'Stop, case 1'
  521            END IF
  522 !
  523 ! 1.c Open files without unpacking MDS ,,,
  524 !
  525       IE     = LEN_TRIM(FEXT)
  526       LFILE  = 'log.' // FEXT(:IE)
  527       IFL    = LEN_TRIM(LFILE)
  528 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 5")
  529       IW     = 1 + INT ( LOG10 ( REAL(NAPROC) + 0.5 ) )
  530       IW     = MAX ( 3 , MIN ( 9 , IW ) )
  531       WRITE (FORMAT,'(A5,I1.1,A1,I1.1,A4)')                     &
  532                    '(A4,I', IW, '.', IW, ',2A)'
  533       WRITE (TFILE,FORMAT) 'test',                             &
  534                     OUTPTS(IMOD)%IAPROC, '.', FEXT(:IE)
  535       IFT    = LEN_TRIM(TFILE)
  536       J      = LEN_TRIM(FNMPRE)
  537 !
  538       IF ( OUTPTS(IMOD)%IAPROC .EQ. OUTPTS(IMOD)%NAPLOG )             &
  539           OPEN (MDS(1),FILE=FNMPRE(:J)//LFILE(:IFL),ERR=888,IOSTAT=IERR)
  540 !
  541       IF ( MDS(3).NE.MDS(1) .AND. MDS(3).NE.MDS(4) .AND. TSTOUT ) THEN
  542           INQUIRE (MDS(3),OPENED=OPENED)
  543           IF ( .NOT. OPENED ) OPEN                                    &
  544                (MDS(3),FILE=FNMPRE(:J)//TFILE(:IFT),ERR=889,IOSTAT=IERR)
  545         END IF
  546 !
  547 ! 1.d Dataset unit numbers
  548 !
  549 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 6")
  550       NDS    = MDS
 
 
                                                         w3initmd.F90  page  11
 
 
  551       NDSO   = NDS(1)
  552       NDSE   = NDS(2)
  553       NDST   = NDS(3)
  554       SCREEN = NDS(4)
  555 !
  556 ! 1.e Subroutine tracing
  557 !
  558       CALL ITRACE ( MTRACE(1), MTRACE(2) )
  559 !
  560 ! 1.f Initial and test outputs
  561 !
  562 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 7")
  563                                               
  564       IF ( IAPROC .EQ. NAPLOG ) THEN
  565           CALL WWDATE ( STDATE )
  566           CALL WWTIME ( STTIME )
  567           WRITE (NDSO,900) WWVER, STDATE, STTIME
  568         END IF
  569               
  570 !
  571 ! 2.  Model defintition ---------------------------------------------- /
  572 ! 2.a Read model defintition file
  573 !
  574 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 8")
  575       CALL W3IOGR ( 'READ', NDS(5), IMOD, FEXT )
  576                                                 
  577 ! Update of output parameter flags based on mod_def parameters (for 3D ar>
  578       CALL W3FLGRDUPDT ( NDSO, NDSE, FLGRD, FLGR2, FLGD, FLG2 )
  579 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 9")
  580                                               
  581       IF ( FLAGLL ) THEN
  582           FACTOR = 1.
  583         ELSE
  584           FACTOR = 1.E-3
  585         END IF
  586       IF ( IAPROC .EQ. NAPLOG ) WRITE (NDSO,920)
  587 !
  588 ! 2.b Save MAPSTA
  589 !
  590       ALLOCATE ( MAPTST(NY,NX) )
  591       MAPTST  = MAPSTA
  592                       
  593 !
  594 ! 2.c MPP preparation
  595 ! 2.c.1 Set simple counters and variables
  596 !
  597 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 10")
  598       CALL SET_UP_NSEAL_NSEALM(NSEALout, NSEALMout)
  599       NSEAL=NSEALout
  600       NSEALM=NSEALMout
  601                       
  602 !
  603 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 11")
  604                                                
  605         IF ( NSEA .LT. NAPROC ) GOTO 820
 
 
                                                         w3initmd.F90  page  12
 
 
  606         IF ((LPDLIB .eqv. .FALSE.).or.(GTYPE .NE. UNGTYPE)) THEN
  607           IF ( NSPEC .LT. NAPROC ) GOTO 821
  608         END IF
  609 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 12")
  610 !
  611 ! 2.c.2 Allocate arrays
  612 !
  613       IF ( IAPROC .LE. NAPROC ) THEN
  614           CALL W3DIMW ( IMOD, NDSE, NDST )
  615         ELSE
  616           CALL W3DIMW ( IMOD, NDSE, NDST, .FALSE. )
  617         END IF
  618       CALL W3DIMA ( IMOD, NDSE, NDST )
  619       CALL W3DIMI ( IMOD, NDSE, NDST , FLAGSTIDEIN )
  620 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 13")
  621                                                
  622 !
  623 ! 2.c.3 Calculated expected number of prop. calls per processor
  624 !
  625       NTTOT  = 0
  626       DO IK=1, NK
  627         NTLOC  = 1 + INT(DTMAX/(DTCFL*SIG(IK)/SIG(1))-0.001)
  628         NTTOT  = NTTOT + NTLOC*NTH
  629         END DO
  630       NTTARG = 1 + (NTTOT-1)/NAPROC
  631       NTTARG = NTTARG + INT(DTMAX/(DTCFL*SIG(NK)/SIG(1))-0.001)
  632       NTTMAX = NTTARG + 5
  633 !
  634 ! 2.c.4 Initialize IAPPRO
  635 !
  636       IAPPRO = 1
  637       ALLOCATE ( NT(NSPEC) )
  638       NT     = NTTOT
  639     IF ((LPDLIB .eqv. .FALSE.).or.(GTYPE .NE. UNGTYPE)) THEN
  640 !
  641       DO
  642 !
  643 ! 2.c.5 First sweep filling IAPPRO
  644 !
  645         DO IP=1, NAPROC
  646           ISTEP  = IP
  647           ISP    = 0
  648           NT(IP) = 0
  649           DO J=1, 1+NSPEC/NAPROC
  650             ISP    = ISP + ISTEP
  651             IF ( MOD(J,2) .EQ. 1 ) THEN
  652                 ISTEP  = 2*(NAPROC-IP) + 1
  653               ELSE
  654                 ISTEP  = 2*IP - 1
  655               END IF
  656             IF ( ISP .LE. NSPEC ) THEN
  657                 IK     = 1 + (ISP-1)/NTH
  658                 NTLOC  = 1 + INT(DTMAX/(DTCFL*SIG(IK)/SIG(1))-0.001)
  659                 IF ( NT(IP)+NTLOC .LE. NTTARG ) THEN
  660                     IAPPRO(ISP) = IP
 
 
                                                         w3initmd.F90  page  13
 
 
  661                     NT(IP)      = NT(IP) + NTLOC
  662                   ELSE
  663                     IAPPRO(ISP) = -1
  664                  END IF
  665               END IF
  666             END DO
  667           END DO
  668 !
  669 ! 2.c.6 Second sweep filling IAPPRO
  670 !
  671         DO IP=1, NAPROC
  672           IF ( NT(IP) .LT. NTTARG ) THEN
  673               DO ISP=1, NSPEC
  674                 IF ( IAPPRO(ISP) .EQ. -1 ) THEN
  675                     IK     = 1 + (ISP-1)/NTH
  676                     NTLOC  = 1 + INT(DTMAX/(DTCFL*SIG(IK)/SIG(1))-0.001)
  677                   IF ( NT(IP)+NTLOC .LE. NTTARG ) THEN
  678                         IAPPRO(ISP) = IP
  679                         NT(IP)      = NT(IP) + NTLOC
  680                      END IF
  681                   END IF
  682                 END DO
  683             END IF
  684           END DO
  685 !
  686 ! 2.c.7 Check if all served
  687 !
  688         IF ( MINVAL(IAPPRO(1:NSPEC)) .GT. 0 ) THEN
  689             EXIT
  690           ELSE
  691             NTTARG = NTTARG + 1
  692             IF ( NTTARG .GE. NTTMAX ) EXIT
  693             IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,8028)
  694           END IF
  695 !
  696         END DO
  697       END IF
  698 !
  699 !!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 14")
  700 ! 2.c.8 Test output
  701 !
  702 ! 2.c.9 Test if any spectral points are left out
  703 !
  704     IF ((LPDLIB .eqv. .FALSE.).or.(GTYPE .NE. UNGTYPE)) THEN
  705       DO ISP=1, NSPEC
  706         IF ( IAPPRO(ISP) .EQ. -1. ) GOTO 829
  707         END DO
  708     END IF
  709 !
  710       DEALLOCATE ( NT )
  711 !
  712 ! 3.  Model initialization ------------------------------------------- /
  713 ! 3.a Read restart file
  714 !
  715       VA(:,:) = 0.
 
 
                                                         w3initmd.F90  page  14
 
 
  716       CALL W3IORS ( 'READ', NDS(6), SIG(NK), IMOD)
  717                                                   
  718       FLCOLD = RSTYPE.LE.1  .OR. RSTYPE.EQ.4
  719       IF ( IAPROC .EQ. NAPLOG ) THEN
  720           IF (RSTYPE.EQ.0) THEN
  721               WRITE (NDSO,930) 'cold start (idealized).'
  722             ELSE IF ( RSTYPE .EQ. 1 ) THEN
  723               WRITE (NDSO,930) 'cold start (wind).'
  724             ELSE IF ( RSTYPE .EQ. 4 ) THEN
  725               WRITE (NDSO,930) 'cold start (calm).'
  726             ELSE
  727               WRITE (NDSO,930) 'full restart.'
  728             END IF
  729         END IF
  730               
  731 !
  732 ! 3.b Compare MAPSTA from grid and restart
  733 !
  734       DO IX=1, NX
  735         DO IY=1, NY
  736           IF ( ABS(MAPSTA(IY,IX)).EQ.2 .OR.                           &
  737                ABS(MAPTST(IY,IX)).EQ.2 ) THEN
  738               MAPSTA(IY,IX) = SIGN ( MAPTST(IY,IX) , MAPSTA(IY,IX) )
  739             END IF
  740           END DO
  741         END DO
  742               
  743 !
  744 ! 3.b2 Set MAPSTA associated to PDLIB
  745 !
  746 ! 3.c Initialization from wind fields
  747 !
  748       FLIWND = RSTYPE.EQ.1
  749 !
  750 ! 3.d Initialization with calm conditions
  751 !
  752       IF ( RSTYPE .EQ. 4 ) THEN
  753           VA(:,:) = 0.
  754         END IF
  755               
  756 !
  757 ! 3.e Prepare propagation scheme
  758 !
  759       IF ( .NOT. FLCUR ) FLCK = .FALSE.
  760 !
  761 ! 4.  Set-up output times -------------------------------------------- *
  762 ! 4.a Unpack ODAT
  763 !
  764       DO J=1, NOTYPE
  765         J0 = (J-1)*5
  766         TONEXT(1,J) =        ODAT(J0+1)
  767         TONEXT(2,J) =        ODAT(J0+2)
  768         DTOUT (  J) = REAL ( ODAT(J0+3) )
  769         TOLAST(1,J) =        ODAT(J0+4)
  770         TOLAST(2,J) =        ODAT(J0+5)
 
 
                                                         w3initmd.F90  page  15
 
 
  771       END DO
  772 !
  773 ! J=8, second stream of restart files
  774         J=8
  775         J0 = (J-1)*5
  776       IF(ODAT(J0+1) .NE. 0) THEN
  777         TONEXT(1,J) =        ODAT(J0+1)
  778         TONEXT(2,J) =        ODAT(J0+2)
  779         DTOUT (  J) = REAL ( ODAT(J0+3) )
  780         TOLAST(1,J) =        ODAT(J0+4)
  781         TOLAST(2,J) =        ODAT(J0+5)
  782         FLOUT(8) = .TRUE.
  783       ELSE
  784         FLOUT(8) = .FALSE.
  785       END IF
  786 !
  787 ! 4.b Check if output available
  788 !
  789       FLOUT(1) = .FALSE.
  790       FLOGRD   = FLGRD
  791       FLOGD    = FLGD
  792       DO J=1, NOGRP
  793         DO K=1, NGRPP
  794           FLOUT(1) = FLOUT(1) .OR. FLOGRD(J,K)
  795         END DO
  796       END DO
  797 !
  798       FLOUT(7) = .FALSE.
  799       FLOGR2   = FLGR2
  800       FLOG2    = FLG2
  801       DO J=1, NOGRP
  802         DO K=1, NGRPP
  803           FLOUT(7) = FLOUT(7) .OR. FLOGR2(J,K)
  804         END DO
  805       END DO
  806 !
  807       FLOUT(2) = NPT .GT. 0
  808 !
  809       FLOUT(3) = .TRUE.
  810 !
  811       FLOUT(4) = .TRUE.
  812 !
  813       FLOUT(5) = FLBPO
  814       IF ( FLBPO ) THEN
  815           CALL W3DMO5 ( IMOD, NDSE, NDST, 4 )
  816         ELSE
  817           DTOUT(5) = 0.
  818         END IF
  819 !
  820       IX0    = MAX (  1, IPRT(1) )
  821       IXN    = MIN ( NX, IPRT(2) )
  822       IXS    = MAX (  1, IPRT(3) )
  823       IY0    = MAX (  1, IPRT(4) )
  824       IYN    = MIN ( NY, IPRT(5) )
  825       IYS    = MAX (  1, IPRT(6) )
 
 
                                                         w3initmd.F90  page  16
 
 
  826       FLFORM = PRTFRM
  827       FLOUT(6) = IX0.LE.IXN .AND. IY0.LE.IYN
  828 !
  829 ! 4.c Get first time per output and overall.
  830 !
  831       TOFRST(1) = -1
  832       TOFRST(2) =  0
  833 !
  834 !      WRITE(*,*) 'We set NOTYPE=0 just for DEBUGGING'
  835 !      NOTYPE=0 ! ONLY FOR DEBUGGING PURPOSE
  836       DO J=1, NOTYPE
  837 !
  838 ! ... check time step
  839 !
  840         DTOUT(J) = MAX ( 0. , DTOUT(J) )
  841         FLOUT(J) = FLOUT(J) .AND. ( DTOUT(J) .GT. 0.5 )
  842 !
  843 ! ... get first time
  844 !
  845         IF ( FLOUT(J) ) THEN
  846             TOUT = TONEXT(:,J)
  847             TLST = TOLAST(:,J)
  848 !
  849             DO
  850               DTTST   = DSEC21 ( TIME , TOUT )
  851               IF ( ( J.NE.4 .AND. DTTST.LT.0. ) .OR.                  &
  852                    ( J.EQ.4 .AND. DTTST.LE.0. ) ) THEN
  853                   CALL TICK21 ( TOUT, DTOUT(J) )
  854                 ELSE
  855                   EXIT
  856                 END IF
  857               END DO
  858 !
  859 ! ... reset first time
  860 !
  861             TONEXT(:,J) = TOUT
  862 !
  863 ! ... check last time
  864 !
  865             DTTST  = DSEC21 ( TOUT , TLST )
  866             IF ( DTTST.LT.0.) FLOUT(J) = .FALSE.
  867 !
  868 ! ... check overall first time
  869 !
  870             IF ( FLOUT(J) ) THEN
  871                 IF ( TOFRST(1).EQ.-1 ) THEN
  872                     TOFRST = TOUT
  873                   ELSE
  874                     DTTST  = DSEC21 ( TOUT , TOFRST )
  875                     IF ( DTTST.GT.0.) THEN
  876                         TOFRST = TOUT
  877                       END IF
  878                   END IF
  879               END IF
  880 !
 
 
                                                         w3initmd.F90  page  17
 
 
  881           END IF
  882 !
  883         END DO
  884 !
  885 ! J=8, second stream of restart files
  886 !
  887       J=8
  888 !
  889 ! ... check time step
  890 !
  891         DTOUT(J) = MAX ( 0. , DTOUT(J) )
  892         FLOUT(J) = FLOUT(J) .AND. ( DTOUT(J) .GT. 0.5 )
  893 !
  894 ! ... get first time
  895 !
  896         IF ( FLOUT(J) ) THEN
  897             TOUT = TONEXT(:,J)
  898             TLST = TOLAST(:,J)
  899 !
  900             DO
  901               DTTST   = DSEC21 ( TIME , TOUT )
  902               IF ( ( J.NE.4 .AND. DTTST.LT.0. ) .OR.                  &
  903                    ( J.EQ.4 .AND. DTTST.LE.0. ) ) THEN
  904                   CALL TICK21 ( TOUT, DTOUT(J) )
  905                 ELSE
  906                   EXIT
  907                 END IF
  908               END DO
  909 !
  910 ! ... reset first time
  911 !
  912             TONEXT(:,J) = TOUT
  913 !
  914 ! ... check last time
  915 !
  916             DTTST  = DSEC21 ( TOUT , TLST )
  917             IF ( DTTST.LT.0.) FLOUT(J) = .FALSE.
  918 !
  919 ! ... check overall first time
  920 !
  921             IF ( FLOUT(J) ) THEN
  922                 IF ( TOFRST(1).EQ.-1 ) THEN
  923                     TOFRST = TOUT
  924                   ELSE
  925                     DTTST  = DSEC21 ( TOUT , TOFRST )
  926                     IF ( DTTST.GT.0.) THEN
  927                         TOFRST = TOUT
  928                       END IF
  929                   END IF
  930               END IF
  931 !
  932           END IF
  933 ! END J=8
  934 !
  935  
 
 
                                                         w3initmd.F90  page  18
 
 
  936 !
  937 ! 4.d Preprocessing for point output.
  938 !
  939       IF ( FLOUT(2) ) CALL W3IOPP ( NPT, XPT, YPT, PNAMES, IMOD )
  940 !
  941 ! 5.  Define wavenumber grid ----------------------------------------- *
  942 ! 5.a Calculate depth
  943 !
  944       MAPTST = MOD(MAPST2/2,2)
  945       MAPST2 = MAPST2 - 2*MAPTST
  946                                 
  947 !
  948 !Li   For multi-resolution SMC grid, these 1-NX and 1-NY nested loops
  949 !Li   may miss the refined cells as they are not 1-1 corresponding to
  950 !Li   the (Nx,NY) regular grid.  The loop is now modified to run over
  951 !Li   full NSEA points.   JGLi24Jan2012
  952 !Li   DO IY=1, NY
  953 !Li     DO IX=1, NX
  954 !Li       ISEA   = MAPFS(IY,IX)
  955       DO ISEA=1, NSEA
  956         IX = MAPSF(ISEA,1)
  957         IY = MAPSF(ISEA,2)
  958 !Li     IF ( ISEA .NE. 0) THEN
  959           WLVeff=WLV(ISEA)
  960           DW(ISEA) = MAX ( 0. , WLVeff-ZB(ISEA) )
  961           IF ( WLVeff-ZB(ISEA) .LE.0. ) THEN
  962             MAPTST(IY,IX) = 1
  963             MAPSTA(IY,IX) = -ABS(MAPSTA(IY,IX))
  964 !!/DEBUGINIT     WRITE(740+IAPROC,*) 'ISEA=', ISEA, ' JSEA=', JSEA
  965 !!/DEBUGINIT     WRITE(740+IAPROC,*) 'NSEA=', NSEA, ' NSEAL=', NSEAL
  966 !!/DEBUGINIT     WRITE(740+IAPROC,*) 'IAPROC=', IAPROC, ' ISPROC=', ISPROC
  967 !!/DEBUGINIT     FLUSH(740+IAPROC)
  968           END IF
  969 !Li     END IF
  970       END DO
  971 !Li   END DO
  972       DO JSEA=1, NSEAL
  973         CALL INIT_GET_ISEA(ISEA, JSEA)
  974         WLVeff=WLV(ISEA)
  975         DW(ISEA) = MAX ( 0. , WLVeff-ZB(ISEA) )
  976         IF ( WLVeff-ZB(ISEA) .LE.0. ) THEN
  977 !!/DEBUGINIT     WRITE(740+IAPROC,*) 'ISEA=', ISEA, ' JSEA=', JSEA
  978 !!/DEBUGINIT     WRITE(740+IAPROC,*) 'NSEA=', NSEA, ' NSEAL=', NSEAL
  979 !!/DEBUGINIT     WRITE(740+IAPROC,*) 'IAPROC=', IAPROC, ' ISPROC=', ISPROC
  980 !!/DEBUGINIT     FLUSH(740+IAPROC)
  981           VA(:,JSEA) = 0.
  982         END IF
  983       END DO
  984             
  985 !
  986       MAPST2 = MAPST2 + 2*MAPTST
  987 !
  988       DEALLOCATE ( MAPTST )
  989                            
  990 !
 
 
                                                         w3initmd.F90  page  19
 
 
  991 ! 5.b Fill wavenumber and group velocity arrays.
  992 !
  993       DO IS=0, NSEA
  994         IF (IS.GT.0) THEN
  995           DEPTH  = MAX ( DMIN , DW(IS) )
  996         ELSE
  997           DEPTH = DMIN
  998           END IF
  999 !
 1000         DO IK=0, NK+1
 1001 !
 1002 !         Calculate wavenumbers and group velocities.
 1003           CALL WAVNU1(SIG(IK),DEPTH,WN(IK,IS),CG(IK,IS))
 1004 !
 1005           END DO
 1006         END DO
 1007 !
 1008 ! Commented by FA with version 4.12
 1009 !      DO IK=1, NK
 1010 !        CG(IK,0) = CG(IK,1)
 1011 !        WN(IK,0) = WN(IK,1)
 1012 !        END DO
 1013 !
 1014 ! 6.  Initialize arrays ---------------------------------------------- /
 1015 !     Some initialized in W3IORS
 1016 !
 1017       UA     = 0.
 1018       UD     = 0.
 1019       U10    = 0.
 1020       U10D   = 0.
 1021 !
 1022       AS     = UNDEF
 1023 !
 1024       AS    (0) = 0.
 1025       DW    (0) = 0.
 1026 !
 1027 ! 7.  Write info to log file ----------------------------------------- /
 1028 !
 1029       IF ( IAPROC .EQ. NAPLOG ) THEN
 1030 !
 1031           WRITE (NDSO,970) GNAME
 1032           IF (   FLLEV    ) WRITE (NDSO,971) 'Prescribed'
 1033           IF (.NOT. FLLEV ) WRITE (NDSO,971) 'No'
 1034           IF (   FLCUR    ) WRITE (NDSO,972) 'Prescribed'
 1035           IF (.NOT. FLCUR ) WRITE (NDSO,972) 'No'
 1036           IF (   FLWIND   ) WRITE (NDSO,973) 'Prescribed'
 1037           IF (.NOT. FLWIND) WRITE (NDSO,973) 'No'
 1038           IF (   FLICE    ) WRITE (NDSO,974) 'Prescribed'
 1039           IF (.NOT. FLICE ) WRITE (NDSO,974) 'No'
 1040 !
 1041           IF (   FLMDN    ) WRITE (NDSO,9972) 'Prescribed'
 1042           IF (.NOT. FLMDN ) WRITE (NDSO,9972) 'No'
 1043           IF (   FLMTH    ) WRITE (NDSO,9971) 'Prescribed'
 1044           IF (.NOT. FLMTH ) WRITE (NDSO,9971) 'No'
 1045           IF (   FLMVS    ) WRITE (NDSO,9970) 'Prescribed'
 
 
                                                         w3initmd.F90  page  20
 
 
 1046           IF (.NOT. FLMVS ) WRITE (NDSO,9970) 'No'
 1047                                                   
 1048           IF (   FLIC1    ) WRITE (NDSO,9973) 'Prescribed'
 1049           IF (.NOT. FLIC1 ) WRITE (NDSO,9973) 'No'
 1050           IF (   FLIC2    ) WRITE (NDSO,9974) 'Prescribed'
 1051           IF (.NOT. FLIC2 ) WRITE (NDSO,9974) 'No'
 1052           IF (   FLIC3    ) WRITE (NDSO,9975) 'Prescribed'
 1053           IF (.NOT. FLIC3 ) WRITE (NDSO,9975) 'No'
 1054           IF (   FLIC4    ) WRITE (NDSO,9976) 'Prescribed'
 1055           IF (.NOT. FLIC4 ) WRITE (NDSO,9976) 'No'
 1056           IF (   FLIC5    ) WRITE (NDSO,9977) 'Prescribed'
 1057           IF (.NOT. FLIC5 ) WRITE (NDSO,9977) 'No'
 1058                                                   
 1059           IF ( FLOUT(1) ) THEN
 1060               WRITE (NDSO,975)
 1061               DO J=1,NOGRP
 1062               DO K=1,NGRPP
 1063                 IF ( FLOGRD(J,K) ) WRITE (NDSO,976) IDOUT(J,K)
 1064                 END DO
 1065                 END DO
 1066             END IF
 1067 !
 1068           IF ( FLOUT(7) ) THEN
 1069               WRITE (NDSO,987)
 1070               DO J=1,NOGRP
 1071               DO K=1,NGRPP
 1072                 IF ( FLOGR2(J,K) ) WRITE (NDSO,976) IDOUT(J,K)
 1073                 END DO
 1074                 END DO
 1075             END IF
 1076 !
 1077           IF ( FLOUT(2) ) THEN
 1078               WRITE (NDSO,977) NOPTS
 1079               IF ( NOPTS .EQ. 0 ) THEN
 1080                   WRITE (NDSO,978)
 1081                 ELSE
 1082                   IF ( FLAGLL ) THEN
 1083                       WRITE (NDSO,979)
 1084                     ELSE
 1085                       WRITE (NDSO,985)
 1086                     END IF
 1087                   DO IP=1, NOPTS
 1088                     IF ( FLAGLL ) THEN
 1089                         WRITE (NDSO,980) IP, FACTOR*PTLOC(1,IP),      &
 1090                                          FACTOR*PTLOC(2,IP), PTNME(IP)
 1091                       ELSE
 1092                         WRITE (NDSO,986) IP, FACTOR*PTLOC(1,IP),      &
 1093                                          FACTOR*PTLOC(2,IP), PTNME(IP)
 1094                       END IF
 1095                     END DO
 1096                 END IF
 1097             END IF
 1098 !
 1099           CALL STME21 ( TIME , DTME21 )
 1100           WRITE (NDSO,981) DTME21
 
 
                                                         w3initmd.F90  page  21
 
 
 1101           IF (FLLEV) THEN
 1102               CALL STME21 ( TLEV , DTME21 )
 1103               WRITE (NDSO,982) DTME21
 1104             END IF
 1105           IF (FLICE) THEN
 1106               CALL STME21 ( TICE , DTME21 )
 1107               WRITE (NDSO,983) DTME21
 1108             END IF
 1109 !
 1110           WRITE (NDSO,984)
 1111 !
 1112         END IF
 1113 !
 1114       IF ( NOPTS .EQ. 0 ) FLOUT(2) = .FALSE.
 1115                                             
 1116 !
 1117 ! Boundary set up for the directions
 1118 !
 1119 !!/PDLIB         CALL VA_SETUP_IOBPD
 1120 !
 1121 ! 8.  Final MPI set up ----------------------------------------------- /
 1122 !
 1123       CALL W3MPII ( IMOD )
 1124       CALL W3MPIO ( IMOD )
 1125       IF ( FLOUT(2) ) CALL W3MPIP ( IMOD )
 1126 !
 1127       RETURN
 1128 !
 1129 ! Escape locations read errors :
 1130 !
 1131   820 CONTINUE
 1132       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,8020) NSEA, NAPROC
 1133       CALL EXTCDE ( 820 )
 1134 !
 1135   821 CONTINUE
 1136       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,8021) NSPEC, NAPROC
 1137       CALL EXTCDE ( 821 )
 1138 !
 1139   829 CONTINUE
 1140       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,8029)
 1141       CALL EXTCDE ( 829 )
 1142                          
 1143 !
 1144   888 CONTINUE
 1145       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,8000) IERR
 1146       CALL EXTCDE ( 1 )
 1147 !
 1148   889 CONTINUE
 1149 ! === no process number filtering for test file !!! ===
 1150       WRITE (NDSE,8001) IERR
 1151       CALL EXTCDE ( 2 )
 1152 !
 1153 ! Formats
 1154 !
 1155   900 FORMAT ( ' WAVEWATCH III log file            ',                 &
 
 
                                                         w3initmd.F90  page  22
 
 
 1156                '                     version ',A/                     &
 1157                ' ==================================',                 &
 1158                '==================================='/                 &
 1159                50X,'date : ',A10/50X,'time :  ',A8)
 1160   920 FORMAT (/' Model definition file read.')
 1161   930 FORMAT ( ' Restart file read; ',A)
 1162 !
 1163   970 FORMAT (/' Grid name : ',A)
 1164   971 FORMAT (/' ',A,' water levels.')
 1165   972 FORMAT ( ' ',A,' curents.')
 1166   973 FORMAT ( ' ',A,' winds.')
 1167   974 FORMAT ( ' ',A,' ice fields.')
 1168   9972 FORMAT( ' ',A,' mud density.')
 1169   9971 FORMAT( ' ',A,' mud thickness.')
 1170   9970 FORMAT( ' ',A,' mud viscosity.')
 1171   9973 FORMAT( ' ',A,' ice parameter 1')
 1172   9974 FORMAT( ' ',A,' ice parameter 2')
 1173   9975 FORMAT( ' ',A,' ice parameter 3')
 1174   9976 FORMAT( ' ',A,' ice parameter 4')
 1175   9977 FORMAT( ' ',A,' ice parameter 5')
 1176                                         
 1177 !
 1178   975 FORMAT (/' Gridded output fields : '/                           &
 1179                '--------------------------------------------------')
 1180   976 FORMAT ( '     ',A)
 1181 !
 1182   977 FORMAT (/' Point output requested for',I6,' points : '/         &
 1183                '------------------------------------------')
 1184   978 FORMAT (/'      Point output disabled')
 1185   979 FORMAT                                                     &
 1186         (/'      point  |  longitude  |   latitude  |  name  '/  &
 1187      '     --------|-------------|-------------|----------------')
 1188   985 FORMAT                                                     &
 1189         (/'      point  |      X      |      Y      |  name  '/  &
 1190      '     --------|-------------|-------------|----------------')
 1191   980 FORMAT ( 5X,I5,'   |',2(F10.2,'   |'),2X,A)
 1192   986 FORMAT ( 5X,I5,'   |',2(F8.1,'E3   |'),2X,A)
 1193 !
 1194   981 FORMAT (/' Initial time     : ',A)
 1195   982 FORMAT ( ' Water level time : ',A)
 1196   983 FORMAT ( ' Ice field time   : ',A)
 1197 !
 1198   984 FORMAT (//                                                      &
 1199         37X,'  |       input       |     output    |'/                &
 1200         37X,'  |-------------------|---------------|'/                &
 1201          2X,'   step | pass |    date      time   |',                 &
 1202               ' b w l c i i1 i5 d | g p t r b f c |'/                 &
 1203          2X,'--------|------|---------------------|',                 &
 1204             '-------------------|---------------|'/                   &
 1205          2X,'--------+------+---------------------+',                 &
 1206             '-------------------+---------------+')
 1207   987 FORMAT (/' Coupling output fields : '/                          &
 1208                '--------------------------------------------------')
 1209 !
 1210  8000 FORMAT (/' *** WAVEWATCH III ERROR IN W3INIT : '/               &
 
 
                                                         w3initmd.F90  page  23
 
 
 1211                '     ERROR IN OPENING LOG FILE'/                      &
 1212                '     IOSTAT =',I5/)
 1213  8001 FORMAT (/' *** WAVEWATCH III ERROR IN W3INIT : '/               &
 1214                '     ERROR IN OPENING TEST FILE'/                     &
 1215                '     IOSTAT =',I5/)
 1216  8002 FORMAT (/' *** WAVEWATCH III WARNING IN W3INIT : '/             &
 1217                '     SIGNIFICANT PART OF RESOURCES RESERVED FOR',     &
 1218                    ' OUTPUT :',F6.1,'%'/)
 1219  8020 FORMAT (/' *** WAVEWATCH III ERROR IN W3INIT : '/         &
 1220          '     NUMBER OF SEA POINTS LESS THAN NUMBER OF PROC.'/ &
 1221          '     NSEA, NAPROC =',2I8/)
 1222  8021 FORMAT (/' *** WAVEWATCH III ERROR IN W3INIT : '/         &
 1223     '     NUMBER OF SPECTRAL POINTS LESS THAN NUMBER OF PROC.'/ &
 1224          '     NSPEC, NAPROC =',2I8/)
 1225  8028 FORMAT (/' *** WAVEWATCH III WARNING IN W3INIT : '/       &
 1226          '     INCREASING TARGET IN MPP PROPAGATION MAP.'/      &
 1227          '     IMBALANCE BETWEEN OVERALL AND CFL TIME STEPS'/)
 1228  8029 FORMAT (/' *** WAVEWATCH III ERROR IN W3INIT : '/         &
 1229          '     SOMETHING WRONG WITH MPP PROPAGATION MAP.'/      &
 1230          '     CALL HENDRIK !!!'/)
 1231 !
 1232 !/
 1233 !/ End of W3INIT ----------------------------------------------------- /
 1234 !/
 1235       END SUBROUTINE W3INIT
 1236 !/ ------------------------------------------------------------------- /
 1237       SUBROUTINE W3MPII ( IMOD )
 1238 !/
 1239 !/                  +-----------------------------------+
 1240 !/                  | WAVEWATCH III           NOAA/NCEP |
 1241 !/                  |           H. L. Tolman            |
 1242 !/                  |                        FORTRAN 90 |
 1243 !/                  | Last update :         11-May-2007 |
 1244 !/                  +-----------------------------------+
 1245 !/
 1246 !/    04-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
 1247 !/    13-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
 1248 !/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
 1249 !/                  Taken out of W3WAVE.
 1250 !/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
 1251 !/    13-Jun-2006 : Splitting STORE in G/SSTORE.        ( version 3.09 )
 1252 !/    11-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
 1253 !/
 1254 !  1. Purpose :
 1255 !
 1256 !     Perform initializations for MPI version of model.
 1257 !     Data transpose only.
 1258 !
 1259 !  2. Method :
 1260 !
 1261 !     Some derived data types are defined.  All communiction in
 1262 !     W3GATH, W3SCAT and W3WAVE are initialized so that all
 1263 !     communication can be performed with single MPI_STARTALL,
 1264 !     MPI_TESTALL and MPI_WAITALL calls.
 1265 !
 
 
                                                         w3initmd.F90  page  24
 
 
 1266 !  3. Parameters :
 1267 !
 1268 !     Parameter list
 1269 !     ----------------------------------------------------------------
 1270 !       IMOD    Int.   I   Model number.
 1271 !     ----------------------------------------------------------------
 1272 !
 1273 !  4. Subroutines used :
 1274 !
 1275 !      Name      Type  Module   Description
 1276 !     ----------------------------------------------------------------
 1277 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1278 !
 1279 !      MPI_TYPE_VECTOR, MPI_TYPE_COMMIT
 1280 !                Subr. mpif.h   MPI derived data type routines.
 1281 !
 1282 !      MPI_SEND_INIT, MPI_RECV_INIT
 1283 !                Subr. mpif.h   MPI persistent communication calls.
 1284 !     ----------------------------------------------------------------
 1285 !
 1286 !  5. Called by :
 1287 !
 1288 !      Name      Type  Module   Description
 1289 !     ----------------------------------------------------------------
 1290 !      W3INIT    Subr. W3INITMD Wave model initialization routine.
 1291 !     ----------------------------------------------------------------
 1292 !
 1293 !  6. Error messages :
 1294 !
 1295 !     None.
 1296 !
 1297 !  7. Remarks :
 1298 !
 1299 !     - Basic MPP set up partially performed in W3INIT.
 1300 !     - Each processor has to be able to send out individual error
 1301 !       messages in this routine !
 1302 !     - No testing on IMOD, since only called by W3INIT.
 1303 !     - In version 3.09 STORE was split into a send and receive
 1304 !       buffer, to avoid/reduce possible conflicts between the FORTRAN
 1305 !       and MPI standards when a gather is posted in a given buffer
 1306 !       right after a send is completed.
 1307 !
 1308 !  8. Structure :
 1309 !
 1310 !     See source code.
 1311 !
 1312 !  9. Switches :
 1313 !
 1314 !       !/SHRD  Switch for shared / distributed memory architecture.
 1315 !       !/DIST  Id.
 1316 !       !/MPI   MPI communication calls.
 1317 !
 1318 !       !/S     Subroutine tracing,
 1319 !       !/T     Test output, general.
 1320 !       !/MPIT  Test output, MPI communications details.
 
 
                                                         w3initmd.F90  page  25
 
 
 1321 !
 1322 ! 10. Source code :
 1323 !
 1324 !/ ------------------------------------------------------------------- /
 1325 !
 1326       USE W3GDATMD, ONLY: NSEA
 1327       USE W3ADATMD, ONLY: NSEALM
 1328       USE W3GDATMD, ONLY: GTYPE, UNGTYPE
 1329       USE CONSTANTS, ONLY: LPDLIB
 1330       USE W3GDATMD, ONLY: NSPEC
 1331       USE W3WDATMD, ONLY: VA
 1332       USE W3ADATMD, ONLY: MPI_COMM_WAVE, WW3_FIELD_VEC,         &
 1333                           WW3_SPEC_VEC, IAPPRO, WADATS,         &
 1334                           NRQSG1, IRQSG1, NRQSG2, IRQSG2,       &
 1335                           GSTORE, SSTORE, MPIBUF, BSTAT,        &
 1336                           BISPL, ISPLOC, IBFLOC, NSPLOC
 1337       USE W3ODATMD, ONLY: NDST, NAPROC, IAPROC
 1338 !/
 1339       IMPLICIT NONE
 1340 !
 1341       INCLUDE "mpif.h"
 1342 !/
 1343 !/ ------------------------------------------------------------------- /
 1344 !/ Parameter list
 1345 !/
 1346       INTEGER, INTENT(IN)     :: IMOD
 1347 !/
 1348 !/ ------------------------------------------------------------------- /
 1349 !/ Local parameters
 1350 !/
 1351       INTEGER                 :: NXXXX
 1352       INTEGER                 :: IERR_MPI, ISP, IH, ITARG,       &
 1353                                  IERR1, IERR2, IP
 1354 !/
 1355 !/ ------------------------------------------------------------------- /
 1356 !/
 1357 !
 1358 ! 1.  Set up derived data types -------------------------------------- /
 1359 !
 1360       NXXXX  = NSEALM * NAPROC
 1361 !
 1362       CALL MPI_TYPE_VECTOR ( NSEALM, 1, NAPROC, MPI_REAL,        &
 1363                              WW3_FIELD_VEC, IERR_MPI )
 1364       CALL MPI_TYPE_VECTOR ( NSEALM, 1, NSPEC, MPI_REAL,         &
 1365                              WW3_SPEC_VEC, IERR_MPI )
 1366       CALL MPI_TYPE_COMMIT ( WW3_FIELD_VEC, IERR_MPI )
 1367       CALL MPI_TYPE_COMMIT ( WW3_SPEC_VEC, IERR_MPI )
 1368 !
 1369       IF( IAPROC .GT. NAPROC ) THEN
 1370           NSPLOC = 0
 1371           NRQSG1 = 0
 1372           NRQSG2 = 0
 1373           RETURN
 1374         END IF
 1375 !
 
 
                                                         w3initmd.F90  page  26
 
 
 1376 ! 2.  Set up scatters and gathers for W3WAVE ------------------------- /
 1377 !     ( persistent communication calls )
 1378 !
 1379    IF ((LPDLIB .eqv. .FALSE.).or.(GTYPE .NE. UNGTYPE)) THEN
 1380       NSPLOC = 0
 1381       DO ISP=1, NSPEC
 1382         IF ( IAPPRO(ISP) .EQ. IAPROC ) NSPLOC = NSPLOC + 1
 1383         END DO
 1384 !
 1385       NRQSG1 = NSPEC - NSPLOC
 1386       ALLOCATE ( WADATS(IMOD)%IRQSG1(MAX(1,NRQSG1),2) )
 1387       IRQSG1 => WADATS(IMOD)%IRQSG1
 1388       IH     = 0
 1389 !
 1390       DO ISP=1, NSPEC
 1391         IF ( IAPPRO(ISP) .NE. IAPROC ) THEN
 1392             ITARG  = IAPPRO(ISP) - 1
 1393             IH     = IH + 1
 1394             CALL MPI_SEND_INIT ( VA(ISP,1), 1, WW3_SPEC_VEC,     &
 1395                  ITARG, ISP, MPI_COMM_WAVE, IRQSG1(IH,1), IERR1 )
 1396             CALL MPI_RECV_INIT ( VA(ISP,1), 1, WW3_SPEC_VEC,     &
 1397                  ITARG, ISP, MPI_COMM_WAVE, IRQSG1(IH,2), IERR2 )
 1398           END IF
 1399         END DO
 1400 !
 1401 ! 3.  Set up scatters and gathers for W3SCAT and W3GATH -------------- /
 1402 !     Also set up buffering of data.
 1403 !
 1404       NRQSG2 = MAX( 1 , NAPROC-1 )
 1405       ALLOCATE ( WADATS(IMOD)%IRQSG2(NRQSG2*NSPLOC,2),           &
 1406                  WADATS(IMOD)%GSTORE(NAPROC*NSEALM,MPIBUF),      &
 1407                  WADATS(IMOD)%SSTORE(NAPROC*NSEALM,MPIBUF) )
 1408       NRQSG2 = NAPROC - 1
 1409 !
 1410       IRQSG2 => WADATS(IMOD)%IRQSG2
 1411       GSTORE => WADATS(IMOD)%GSTORE
 1412       SSTORE => WADATS(IMOD)%SSTORE
 1413 !
 1414       IH     = 0
 1415       ISPLOC = 0
 1416       IBFLOC = 0
 1417       WADATS(IMOD)%GSTORE = 0.
 1418       WADATS(IMOD)%SSTORE = 0.
 1419 !
 1420 ! 3.a Loop over local spectral components
 1421 !
 1422       DO ISP=1, NSPEC
 1423         IF ( IAPPRO(ISP) .EQ. IAPROC ) THEN
 1424 !
 1425             ISPLOC = ISPLOC + 1
 1426             IBFLOC = IBFLOC + 1
 1427             IF ( IBFLOC .GT. MPIBUF ) IBFLOC = 1
 1428 !
 1429 ! 3.b Loop over non-local processes
 1430 !
 
 
                                                         w3initmd.F90  page  27
 
 
 1431             DO IP=1, NAPROC
 1432               IF ( IP .NE. IAPROC ) THEN
 1433 !
 1434                   ITARG  = IP - 1
 1435                   IH     = IH + 1
 1436 !
 1437                   CALL MPI_RECV_INIT                             &
 1438                      ( WADATS(IMOD)%GSTORE(IP,IBFLOC), 1,        &
 1439                        WW3_FIELD_VEC, ITARG, ISP, MPI_COMM_WAVE, &
 1440                        IRQSG2(IH,1), IERR2 )
 1441                   CALL MPI_SEND_INIT                             &
 1442                      ( WADATS(IMOD)%SSTORE(IP,IBFLOC), 1,        &
 1443                        WW3_FIELD_VEC, ITARG, ISP, MPI_COMM_WAVE, &
 1444                        IRQSG2(IH,2), IERR2 )
 1445 !
 1446 ! ... End of loops
 1447 !
 1448                 END IF
 1449               END DO
 1450 !
 1451           END IF
 1452         END DO
 1453 !
 1454 ! 4.  Initialize buffer management ----------------------------------- /
 1455 !
 1456       BSTAT  = 0
 1457       BISPL  = 0
 1458       ISPLOC = 0
 1459       IBFLOC = 0
 1460 !
 1461    END IF
 1462       RETURN
 1463 !
 1464 ! Format statements
 1465 !
 1466 !/
 1467 !/ End of W3MPII ----------------------------------------------------- /
 1468 !/
 1469       END SUBROUTINE W3MPII
 1470 !/ ------------------------------------------------------------------- /
 1471       SUBROUTINE W3MPIO ( IMOD )
 1472 !/
 1473 !/                  +-----------------------------------+
 1474 !/                  | WAVEWATCH III           NOAA/NCEP |
 1475 !/                  |           H. L. Tolman            |
 1476 !/                  |                        FORTRAN 90 |
 1477 !/                  | Last update :         11-Nov-2015 |
 1478 !/                  +-----------------------------------+
 1479 !/
 1480 !/    17-Mar-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
 1481 !/    11-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
 1482 !/    20-Aug-2003 : Output server options added.        ( version 3.04 )
 1483 !/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
 1484 !/                  Taken out of W3WAVE.
 1485 !/    03-Jan-2005 : Add US2x to MPI communication.      ( version 3.06 )
 
 
                                                         w3initmd.F90  page  28
 
 
 1486 !/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
 1487 !/    21-Jul-2005 : Add output fields.                  ( version 3.07 )
 1488 !/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
 1489 !/    02-Aug-2006 : W3MPIP split off.                   ( version 3.10 )
 1490 !/    02-Apr-2007 : Add partitioned field data.         ( version 3.11 )
 1491 !/                  Add user-defined field data.
 1492 !/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
 1493 !/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
 1494 !/    25-Dec-2012 : Modify field output MPI for new     ( version 4.11 )
 1495 !/                  structure and smaller memory footprint.
 1496 !/    02-Jul-2013 : Bug fix MPI_FLOAT -> MPI_REAL.      ( version 4.11 )
 1497 !/    11-Nov-2015 : Added ICEF                          ( version 5.08 )
 1498 !/
 1499 !  1. Purpose :
 1500 !
 1501 !     Prepare MPI persistent communication needed for WAVEWATCH I/O
 1502 !     routines.
 1503 !
 1504 !  2. Method :
 1505 !
 1506 !     Create handles as needed.
 1507 !
 1508 !  3. Parameters :
 1509 !
 1510 !     Parameter list
 1511 !     ----------------------------------------------------------------
 1512 !       IMOD    Int.   I   Model number.
 1513 !     ----------------------------------------------------------------
 1514 !
 1515 !  4. Subroutines used :
 1516 !
 1517 !      Name      Type  Module   Description
 1518 !     ----------------------------------------------------------------
 1519 !      W3XDMA    Subr. W3ADATMD Dimension expanded output arrays.
 1520 !      W3SETA    Subr.    "     Set pointers for output arrays
 1521 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1522 !
 1523 !      MPI_SEND_INIT, MPI_RECV_INIT
 1524 !                Subr. mpif.h   MPI persistent communication calls.
 1525 !     ----------------------------------------------------------------
 1526 !
 1527 !  5. Called by :
 1528 !
 1529 !      Name      Type  Module   Description
 1530 !     ----------------------------------------------------------------
 1531 !      W3INIT    Subr. W3INITMD Wave model initialization routine.
 1532 !     ----------------------------------------------------------------
 1533 !
 1534 !  6. Error messages :
 1535 !
 1536 !  7. Remarks :
 1537 !
 1538 !     - The communication as set up in W3MPII uses tags with number
 1539 !       ranging from 1 through NSPEC. New and unique tags for IO
 1540 !       related communication are assigned here dynamically.
 
 
                                                         w3initmd.F90  page  29
 
 
 1541 !     - No testing on IMOD, since only called by W3INIT.
 1542 !
 1543 !  8. Structure :
 1544 !
 1545 !     See source code.
 1546 !
 1547 !  9. Switches :
 1548 !
 1549 !       !/MPI   MPI communication calls.
 1550 !
 1551 !       !/S     Enable subroutine tracing.
 1552 !       !/MPIT  Enable test output.
 1553 !
 1554 ! 10. Source code :
 1555 !
 1556 !/ ------------------------------------------------------------------- /
 1557 !
 1558       USE W3ADATMD, ONLY: W3XDMA, W3SETA, W3XETA
 1559       USE W3IORSMD, ONLY: OARST
 1560       USE W3SERVMD, ONLY: EXTCDE
 1561 !/
 1562       USE W3GDATMD, ONLY: NSEA
 1563       USE W3ADATMD, ONLY: NSEALM
 1564       USE W3GDATMD, ONLY: NX, NSPEC, MAPFS, E3DF, P2MSF, US3DF, USSPF
 1565       USE W3WDATMD, ONLY: VA, UST, USTDIR, ASF, FPIS, ICEF
 1566       USE W3ADATMD, ONLY: MPI_COMM_WAVE, WW3_FIELD_VEC
 1567       USE W3ADATMD, ONLY: HS, WLM, T02
 1568                                       
 1569                                       
 1570       USE W3ADATMD, ONLY: T0M1, THM, THS, FP0, THP0, FP1, THP1,   &
 1571                           DTDYN, FCUT, SPPNT, ABA, ABD, UBA, UBD,&
 1572                           SXX, SYY, SXY, USERO, PHS, PTP, PLP,   &
 1573                           PDIR, PSI, PWS, PWST, PNR, PHIAW, PHIOC,&
 1574                           TUSX, TUSY, TAUWIX, TAUWIY, TAUOX,     &
 1575                           TAUOY, USSX, USSY, MSSX, MSSY, MSSD,   &
 1576                           MSCX, MSCY, MSCD, PRMS, TPMS, CHARN,   &
 1577                           TWS, TAUWNX, TAUWNY, BHD, CGE,         &
 1578                           CFLXYMAX, CFLTHMAX, CFLKMAX, WHITECAP, &
 1579                           BEDFORMS, PHIBBL, TAUBBL, T01,         &
 1580                           P2SMS, US3D, EF,  TH1M, STH1M, TH2M,   &
 1581                           STH2M, HSIG, PHICE, TAUICE, USSP,      &
 1582                           STMAXE, STMAXD, HMAXE, HCMAXE, HMAXD,  &
 1583                           HCMAXD, QP, PTHP0, PQP, PPE, PGW, PSW, &
 1584                           PTM1, PT1, PT2, PEP, WBT, CX, CY
 1585                                                           
 1586       USE W3GDATMD, ONLY: NK
 1587       USE W3ODATMD, ONLY: NDST, IAPROC, NAPROC, NTPROC, FLOUT,   &
 1588                           NAPFLD, NAPPNT, NAPRST, NAPBPT, NAPTRK,&
 1589                           NOGRP, NGRPP, NOGE, FLOGRR
 1590       USE W3ODATMD, ONLY: OUTPTS, NRQGO, NRQGO2, IRQGO, IRQGO2,  &
 1591                           FLOGRD, NRQPO, NRQPO2, IRQPO1, IRQPO2, &
 1592                           NOPTS, IPTINT, NRQRS, IRQRS, NBLKRS,   &
 1593                           RSBLKS, IRQRSS, VAAUX, NRQBP, NRQBP2,  &
 1594                           IRQBP1, IRQBP2, NFBPO, NBO2, ISBPO,    &
 1595                           ABPOS, NRQTR, IRQTR, IT0PNT, IT0TRK,   &
 
 
                                                         w3initmd.F90  page  30
 
 
 1596                           IT0PRT, NOSWLL, NOEXTR, NDSE, IOSTYP,  &
 1597                           FLOGR2
 1598       USE W3PARALL, ONLY : INIT_GET_JSEA_ISPROC
 1599       USE W3GDATMD, ONLY: GTYPE, UNGTYPE
 1600       USE CONSTANTS, ONLY: LPDLIB
 1601 !/
 1602       IMPLICIT NONE
 1603 !
 1604       INCLUDE "mpif.h"
 1605 !/
 1606 !/ ------------------------------------------------------------------- /
 1607 !/ Parameter list
 1608 !/
 1609       INTEGER, INTENT(IN)     :: IMOD
 1610 !/
 1611 !/ ------------------------------------------------------------------- /
 1612 !/ Local parameters
 1613 !/
 1614       INTEGER                 :: IK, IFJ
 1615       INTEGER                 :: IH, IT0, IROOT, IT, IERR, I0,   &
 1616                                  IFROM, IX(4), IY(4), IS(4),     &
 1617                                  IP(4), I, J, JSEA, ITARG, IB,   &
 1618                                  JSEA0, JSEAN, NSEAB, IBOFF,     &
 1619                                  ISEA, ISPROC, K, NRQMAX
 1620       LOGICAL                 :: FLGRDALL(NOGRP,NGRPP)
 1621       LOGICAL                 :: FLGRDARST(NOGRP,NGRPP)
 1622 !/
 1623 !/ ------------------------------------------------------------------- /
 1624 !/
 1625 !
 1626 ! 1.  Set-up for W3IOGO ---------------------------------------------- /
 1627 !
 1628       DO J=1, NOGRP
 1629         DO K=1, NGRPP
 1630           FLGRDALL (J,K) =  (FLOGRD(J,K) .OR. FLOGR2(J,K))
 1631           FLGRDARST(J,K) =  (FLGRDALL(J,K) .OR. FLOGRR(J,K))
 1632           END DO
 1633         END DO
 1634 !
 1635       NRQGO  = 0
 1636       NRQGO2 = 0
 1637       IT0    = NSPEC
 1638       IROOT  = NAPFLD - 1
 1639 !
 1640       IF ((FLOUT(1) .OR. FLOUT(7)).and.(.not. LPDLIB .or.       &
 1641           (GTYPE .ne. UNGTYPE).or. .TRUE.)) THEN
 1642 !
 1643 ! NRQMAX is the maximum number of fields, it is the sum of the
 1644 ! sizes of scalar fields (Hs) + 2-component vectors (CUR) + 3-comp ...
 1645 !
 1646           NRQMAX = 0 + 12 + 0 + 2+(NOGE(4)-2)*(NOSWLL+1) + 10+4 + &
 1647                    7+6 + 5+5 + 2+2 + 5+0 + NOEXTR
 1648           DO IFJ=1,5
 1649             IF ( FLGRDALL( 3,IFJ)) NRQMAX = NRQMAX +               &
 1650                                     E3DF(3,IFJ) - E3DF(2,IFJ) + 1
 
 
                                                         w3initmd.F90  page  31
 
 
 1651             END DO
 1652           IF ( FLGRDALL( 6,9)) NRQMAX = NRQMAX +               &
 1653                                           P2MSF(3) - P2MSF(2) + 1
 1654           IF ( FLGRDALL( 6, 8) ) NRQMAX = NRQMAX + 2*NK
 1655           IF ( FLGRDALL( 6,12) ) NRQMAX = NRQMAX + 2*NK
 1656 !
 1657           IF ( NRQMAX .GT. 0 ) THEN
 1658               ALLOCATE ( OUTPTS(IMOD)%OUT1%IRQGO(NRQMAX) )
 1659               ALLOCATE ( OUTPTS(IMOD)%OUT1%IRQGO2(NRQMAX*NAPROC) )
 1660             END IF
 1661           IRQGO  => OUTPTS(IMOD)%OUT1%IRQGO
 1662           IRQGO2 => OUTPTS(IMOD)%OUT1%IRQGO2
 1663 !
 1664 ! 1.a Sends of fields
 1665 !
 1666           IH     = 0
 1667 !
 1668           IF ( IAPROC .LE. NAPROC ) THEN
 1669               IT     = IT0
 1670 !
 1671               IF ( FLGRDALL( 1, 9) ) THEN
 1672                   IH     = IH + 1
 1673                   IT     = IT + 1
 1674       CALL MPI_SEND_INIT (ICEF (IAPROC), 1, WW3_FIELD_VEC,    &
 1675                                 IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1676                 END IF
 1677 !
 1678               IF ( FLGRDALL( 2, 1) ) THEN
 1679                   IH     = IH + 1
 1680                   IT     = IT + 1
 1681       CALL MPI_SEND_INIT (HS   (1),NSEALM , MPI_REAL, IROOT,    &
 1682                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1683                 END IF
 1684 !
 1685               IF ( FLGRDALL( 2, 2) ) THEN
 1686                   IH     = IH + 1
 1687                   IT     = IT + 1
 1688       CALL MPI_SEND_INIT (WLM  (1),NSEALM , MPI_REAL, IROOT,    &
 1689                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1690                 END IF
 1691 !
 1692               IF ( FLGRDALL( 2, 3) ) THEN
 1693                   IH     = IH + 1
 1694                   IT     = IT + 1
 1695       CALL MPI_SEND_INIT (T02  (1),NSEALM , MPI_REAL, IROOT,    &
 1696                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1697                 END IF
 1698 !
 1699               IF ( FLGRDALL( 2, 4) ) THEN
 1700                   IH     = IH + 1
 1701                   IT     = IT + 1
 1702       CALL MPI_SEND_INIT (T0M1  (1),NSEALM , MPI_REAL, IROOT,    &
 1703                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1704                 END IF
 1705 !
 
 
                                                         w3initmd.F90  page  32
 
 
 1706               IF ( FLGRDALL( 2, 5) ) THEN
 1707                   IH     = IH + 1
 1708                   IT     = IT + 1
 1709       CALL MPI_SEND_INIT (T01  (1),NSEALM , MPI_REAL, IROOT,    &
 1710                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1711                 END IF
 1712 !
 1713               IF ( FLGRDALL( 2, 6) .OR. FLGRDALL( 2,18) ) THEN
 1714                   ! TP output shares FP0 internal field with FP
 1715                   IH     = IH + 1
 1716                   IT     = IT + 1
 1717       CALL MPI_SEND_INIT (FP0  (1),NSEALM , MPI_REAL, IROOT,    &
 1718                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1719                 END IF
 1720 !
 1721               IF ( FLGRDALL( 2, 7) ) THEN
 1722                   IH     = IH + 1
 1723                   IT     = IT + 1
 1724       CALL MPI_SEND_INIT (THM  (1),NSEALM , MPI_REAL, IROOT,    &
 1725                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1726                 END IF
 1727 !
 1728               IF ( FLGRDALL( 2, 8) ) THEN
 1729                   IH     = IH + 1
 1730                   IT     = IT + 1
 1731       CALL MPI_SEND_INIT (THS  (1),NSEALM , MPI_REAL, IROOT,    &
 1732                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1733                 END IF
 1734 !
 1735               IF ( FLGRDALL( 2, 9) ) THEN
 1736                   IH     = IH + 1
 1737                   IT     = IT + 1
 1738       CALL MPI_SEND_INIT (THP0 (1),NSEALM , MPI_REAL, IROOT,    &
 1739                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1740                 END IF
 1741 !
 1742               IF ( FLGRDALL( 2, 10) ) THEN
 1743                   IH     = IH + 1
 1744                   IT     = IT + 1
 1745       CALL MPI_SEND_INIT (HSIG (1),NSEALM , MPI_REAL, IROOT,    &
 1746                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1747                 END IF
 1748 !
 1749               IF ( FLGRDALL( 2, 11) ) THEN
 1750                   IH     = IH + 1
 1751                   IT     = IT + 1
 1752       CALL MPI_SEND_INIT (STMAXE (1),NSEALM , MPI_REAL, IROOT,    &
 1753                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1754                 END IF
 1755 !
 1756               IF ( FLGRDALL( 2, 12) ) THEN
 1757                   IH     = IH + 1
 1758                   IT     = IT + 1
 1759       CALL MPI_SEND_INIT (STMAXD (1),NSEALM , MPI_REAL, IROOT,    &
 1760                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 
 
                                                         w3initmd.F90  page  33
 
 
 1761                 END IF
 1762 !
 1763               IF ( FLGRDALL( 2, 13) ) THEN
 1764                   IH     = IH + 1
 1765                   IT     = IT + 1
 1766       CALL MPI_SEND_INIT (HMAXE (1),NSEALM , MPI_REAL, IROOT,    &
 1767                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1768                 END IF
 1769 !
 1770               IF ( FLGRDALL( 2, 14) ) THEN
 1771                   IH     = IH + 1
 1772                   IT     = IT + 1
 1773       CALL MPI_SEND_INIT (HCMAXE (1),NSEALM , MPI_REAL, IROOT,    &
 1774                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1775                 END IF
 1776 !
 1777               IF ( FLGRDALL( 2, 15) ) THEN
 1778                   IH     = IH + 1
 1779                   IT     = IT + 1
 1780       CALL MPI_SEND_INIT (HMAXD (1),NSEALM , MPI_REAL, IROOT,    &
 1781                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1782                 END IF
 1783 !
 1784               IF ( FLGRDALL( 2, 16) ) THEN
 1785                   IH     = IH + 1
 1786                   IT     = IT + 1
 1787       CALL MPI_SEND_INIT (HCMAXD (1),NSEALM , MPI_REAL, IROOT,    &
 1788                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1789                 END IF
 1790 !
 1791               IF ( FLGRDALL( 2, 17) ) THEN
 1792                   IH     = IH + 1
 1793                   IT     = IT + 1
 1794       CALL MPI_SEND_INIT (WBT  (1),NSEALM , MPI_REAL, IROOT,    &
 1795                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1796                 END IF
 1797 !
 1798               IF ( FLGRDALL( 3, 1) ) THEN
 1799                   DO IK=E3DF(2,1),E3DF(3,1)
 1800                     IH     = IH + 1
 1801                     IT     = IT + 1
 1802       CALL MPI_SEND_INIT (EF(1,IK),NSEALM , MPI_REAL, IROOT, &
 1803                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1804                     END DO
 1805                  END IF
 1806 !
 1807               IF ( FLGRDALL( 3, 2) ) THEN
 1808                   DO IK=E3DF(2,2),E3DF(3,2)
 1809                     IH     = IH + 1
 1810                     IT     = IT + 1
 1811       CALL MPI_SEND_INIT (TH1M(1,IK),NSEALM , MPI_REAL, IROOT, &
 1812                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1813                     END DO
 1814                  END IF
 1815 !
 
 
                                                         w3initmd.F90  page  34
 
 
 1816               IF ( FLGRDALL( 3, 3) ) THEN
 1817                   DO IK=E3DF(2,3),E3DF(3,3)
 1818                     IH     = IH + 1
 1819                     IT     = IT + 1
 1820       CALL MPI_SEND_INIT (STH1M(1,IK),NSEALM , MPI_REAL, IROOT, &
 1821                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1822                     END DO
 1823                  END IF
 1824 !
 1825               IF ( FLGRDALL( 3, 4) ) THEN
 1826                   DO IK=E3DF(2,4),E3DF(3,4)
 1827                     IH     = IH + 1
 1828                     IT     = IT + 1
 1829       CALL MPI_SEND_INIT (TH2M(1,IK),NSEALM , MPI_REAL, IROOT, &
 1830                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1831                     END DO
 1832                  END IF
 1833 !
 1834               IF ( FLGRDALL( 3, 5) ) THEN
 1835                   DO IK=E3DF(2,5),E3DF(3,5)
 1836                     IH     = IH + 1
 1837                     IT     = IT + 1
 1838       CALL MPI_SEND_INIT (STH2M(1,IK),NSEALM , MPI_REAL, IROOT, &
 1839                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1840                     END DO
 1841                  END IF
 1842 !
 1843               IF ( FLGRDALL( 4, 1) ) THEN
 1844                 DO K=0, NOSWLL
 1845                   IH     = IH + 1
 1846                   IT     = IT + 1
 1847       CALL MPI_SEND_INIT (PHS(1,K),NSEALM , MPI_REAL, IROOT,    &
 1848                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1849                   END DO
 1850                 END IF
 1851 !
 1852               IF ( FLGRDALL( 4, 2) ) THEN
 1853                 DO K=0, NOSWLL
 1854                   IH     = IH + 1
 1855                   IT     = IT + 1
 1856       CALL MPI_SEND_INIT (PTP(1,K),NSEALM , MPI_REAL, IROOT,    &
 1857                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1858                   END DO
 1859                 END IF
 1860 !
 1861               IF ( FLGRDALL( 4, 3) ) THEN
 1862                 DO K=0, NOSWLL
 1863                   IH     = IH + 1
 1864                   IT     = IT + 1
 1865       CALL MPI_SEND_INIT (PLP(1,K),NSEALM , MPI_REAL, IROOT,    &
 1866                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1867                   END DO
 1868                 END IF
 1869 !
 1870               IF ( FLGRDALL( 4, 4) ) THEN
 
 
                                                         w3initmd.F90  page  35
 
 
 1871                 DO K=0, NOSWLL
 1872                   IH     = IH + 1
 1873                   IT     = IT + 1
 1874       CALL MPI_SEND_INIT (PDIR(1,K),NSEALM , MPI_REAL, IROOT,    &
 1875                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1876                   END DO
 1877                 END IF
 1878 !
 1879               IF ( FLGRDALL( 4, 5) ) THEN
 1880                 DO K=0, NOSWLL
 1881                   IH     = IH + 1
 1882                   IT     = IT + 1
 1883       CALL MPI_SEND_INIT (PSI(1,K),NSEALM , MPI_REAL, IROOT,    &
 1884                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1885                   END DO
 1886                 END IF
 1887 !
 1888               IF ( FLGRDALL( 4, 6) ) THEN
 1889                 DO K=0, NOSWLL
 1890                   IH     = IH + 1
 1891                   IT     = IT + 1
 1892       CALL MPI_SEND_INIT (PWS(1,K),NSEALM , MPI_REAL, IROOT,    &
 1893                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1894                   END DO
 1895                 END IF
 1896 !
 1897               IF ( FLGRDALL( 4, 7) ) THEN
 1898                 DO K=0, NOSWLL
 1899                   IH     = IH + 1
 1900                   IT     = IT + 1
 1901       CALL MPI_SEND_INIT (PTHP0(1,K),NSEALM , MPI_REAL, IROOT,    &
 1902                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1903                   END DO
 1904                 END IF
 1905 !
 1906               IF ( FLGRDALL( 4, 8) ) THEN
 1907                 DO K=0, NOSWLL
 1908                   IH     = IH + 1
 1909                   IT     = IT + 1
 1910       CALL MPI_SEND_INIT (PQP (1,K),NSEALM , MPI_REAL, IROOT,    &
 1911                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1912                   END DO
 1913                 END IF
 1914 !
 1915               IF ( FLGRDALL( 4, 9) ) THEN
 1916                 DO K=0, NOSWLL
 1917                   IH     = IH + 1
 1918                   IT     = IT + 1
 1919       CALL MPI_SEND_INIT (PPE (1,K),NSEALM , MPI_REAL, IROOT,    &
 1920                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1921                   END DO
 1922                 END IF
 1923 !
 1924               IF ( FLGRDALL( 4,10) ) THEN
 1925                 DO K=0, NOSWLL
 
 
                                                         w3initmd.F90  page  36
 
 
 1926                   IH     = IH + 1
 1927                   IT     = IT + 1
 1928       CALL MPI_SEND_INIT (PGW (1,K),NSEALM , MPI_REAL, IROOT,    &
 1929                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1930                   END DO
 1931                 END IF
 1932 !
 1933               IF ( FLGRDALL( 4,11) ) THEN
 1934                 DO K=0, NOSWLL
 1935                   IH     = IH + 1
 1936                   IT     = IT + 1
 1937       CALL MPI_SEND_INIT (PSW (1,K),NSEALM , MPI_REAL, IROOT,    &
 1938                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1939                   END DO
 1940                 END IF
 1941 !
 1942               IF ( FLGRDALL( 4,12) ) THEN
 1943                 DO K=0, NOSWLL
 1944                   IH     = IH + 1
 1945                   IT     = IT + 1
 1946       CALL MPI_SEND_INIT (PTM1(1,K),NSEALM , MPI_REAL, IROOT,   &
 1947                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1948                   END DO
 1949                 END IF
 1950 !
 1951               IF ( FLGRDALL( 4,13) ) THEN
 1952                 DO K=0, NOSWLL
 1953                   IH     = IH + 1
 1954                   IT     = IT + 1
 1955       CALL MPI_SEND_INIT (PT1 (1,K),NSEALM , MPI_REAL, IROOT,    &
 1956                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1957                   END DO
 1958                 END IF
 1959 !
 1960               IF ( FLGRDALL( 4,14) ) THEN
 1961                 DO K=0, NOSWLL
 1962                   IH     = IH + 1
 1963                   IT     = IT + 1
 1964       CALL MPI_SEND_INIT (PT2 (1,K),NSEALM , MPI_REAL, IROOT,    &
 1965                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1966                   END DO
 1967                 END IF
 1968 !
 1969               IF ( FLGRDALL( 4,15) ) THEN
 1970                 DO K=0, NOSWLL
 1971                   IH     = IH + 1
 1972                   IT     = IT + 1
 1973       CALL MPI_SEND_INIT (PEP (1,K),NSEALM , MPI_REAL, IROOT,    &
 1974                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1975                   END DO
 1976                 END IF
 1977 !
 1978               IF ( FLGRDALL( 4,16) ) THEN
 1979                   IH     = IH + 1
 1980                   IT     = IT + 1
 
 
                                                         w3initmd.F90  page  37
 
 
 1981       CALL MPI_SEND_INIT (PWST (1),NSEALM , MPI_REAL, IROOT,    &
 1982                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1983                 END IF
 1984 !
 1985               IF ( FLGRDALL( 4,17) ) THEN
 1986                   IH     = IH + 1
 1987                   IT     = IT + 1
 1988       CALL MPI_SEND_INIT (PNR  (1),NSEALM , MPI_REAL, IROOT,    &
 1989                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 1990                 END IF
 1991 !
 1992               IF ( FLGRDALL( 5, 1) ) THEN
 1993                   IH     = IH + 1
 1994                   IT     = IT + 1
 1995       CALL MPI_SEND_INIT (UST   (IAPROC), 1, WW3_FIELD_VEC,      &
 1996                        IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR )
 1997                   IH     = IH + 1
 1998                   IT     = IT + 1
 1999       CALL MPI_SEND_INIT (USTDIR(IAPROC), 1, WW3_FIELD_VEC,       &
 2000                        IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR )
 2001                   IH     = IH + 1
 2002                   IT     = IT + 1
 2003       CALL MPI_SEND_INIT (ASF   (IAPROC), 1, WW3_FIELD_VEC,       &
 2004                        IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR )
 2005                 END IF
 2006 !
 2007               IF ( FLGRDALL( 5, 2) ) THEN
 2008                   IH     = IH + 1
 2009                   IT     = IT + 1
 2010       CALL MPI_SEND_INIT (CHARN(1),NSEALM , MPI_REAL, IROOT,    &
 2011                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2012                 END IF
 2013 !
 2014               IF ( FLGRDALL( 5, 3) ) THEN
 2015                   IH     = IH + 1
 2016                   IT     = IT + 1
 2017       CALL MPI_SEND_INIT (CGE  (1),NSEALM , MPI_REAL, IROOT,    &
 2018                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2019                 END IF
 2020 !
 2021               IF ( FLGRDALL( 5, 4) ) THEN
 2022                   IH     = IH + 1
 2023                   IT     = IT + 1
 2024       CALL MPI_SEND_INIT (PHIAW(1),NSEALM , MPI_REAL, IROOT,    &
 2025                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2026                 END IF
 2027 !
 2028               IF ( FLGRDALL( 5, 5) ) THEN
 2029                   IH     = IH + 1
 2030                   IT     = IT + 1
 2031       CALL MPI_SEND_INIT (TAUWIX(1),NSEALM , MPI_REAL, IROOT,   &
 2032                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2033                     IH     = IH + 1
 2034                     IT     = IT + 1
 2035       CALL MPI_SEND_INIT (TAUWIY(1),NSEALM , MPI_REAL, IROOT,   &
 
 
                                                         w3initmd.F90  page  38
 
 
 2036                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2037                 END IF
 2038 !
 2039               IF ( FLGRDALL( 5, 6) ) THEN
 2040                   IH     = IH + 1
 2041                   IT     = IT + 1
 2042       CALL MPI_SEND_INIT (TAUWNX(1),NSEALM , MPI_REAL, IROOT,   &
 2043                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2044                     IH     = IH + 1
 2045                     IT     = IT + 1
 2046       CALL MPI_SEND_INIT (TAUWNY(1),NSEALM , MPI_REAL, IROOT,   &
 2047                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2048                 END IF
 2049 !
 2050               IF ( FLGRDALL( 5, 7) ) THEN
 2051                   IH     = IH + 1
 2052                   IT     = IT + 1
 2053       CALL MPI_SEND_INIT (WHITECAP(1,1),NSEALM , MPI_REAL, IROOT,&
 2054                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2055                 END IF
 2056 !
 2057               IF ( FLGRDALL( 5, 8) ) THEN
 2058                   IH     = IH + 1
 2059                   IT     = IT + 1
 2060       CALL MPI_SEND_INIT (WHITECAP(1,2),NSEALM , MPI_REAL, IROOT,&
 2061                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2062                 END IF
 2063 !
 2064               IF ( FLGRDALL( 5, 9) ) THEN
 2065                   IH     = IH + 1
 2066                   IT     = IT + 1
 2067       CALL MPI_SEND_INIT (WHITECAP(1,3),NSEALM , MPI_REAL, IROOT,&
 2068                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2069                 END IF
 2070 !
 2071               IF ( FLGRDALL( 5,10) ) THEN
 2072                   IH     = IH + 1
 2073                   IT     = IT + 1
 2074       CALL MPI_SEND_INIT (WHITECAP(1,4),NSEALM , MPI_REAL, IROOT,&
 2075                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2076                 END IF
 2077 !
 2078               IF ( FLGRDALL( 5, 11) ) THEN
 2079                   IH     = IH + 1
 2080                   IT     = IT + 1
 2081       CALL MPI_SEND_INIT (TWS(1),NSEALM , MPI_REAL, IROOT,    &
 2082                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2083                 END IF
 2084 !
 2085               IF ( FLGRDALL( 6, 1) ) THEN
 2086                   IH     = IH + 1
 2087                   IT     = IT + 1
 2088       CALL MPI_SEND_INIT (SXX   (1),NSEALM , MPI_REAL, IROOT,   &
 2089                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2090                   IH     = IH + 1
 
 
                                                         w3initmd.F90  page  39
 
 
 2091                   IT     = IT + 1
 2092       CALL MPI_SEND_INIT (SYY   (1),NSEALM , MPI_REAL, IROOT,   &
 2093                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2094                   IH     = IH + 1
 2095                   IT     = IT + 1
 2096       CALL MPI_SEND_INIT (SXY   (1),NSEALM , MPI_REAL, IROOT,   &
 2097                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2098                 END IF
 2099 !
 2100               IF ( FLGRDALL( 6, 2) ) THEN
 2101                   IH     = IH + 1
 2102                   IT     = IT + 1
 2103       CALL MPI_SEND_INIT (TAUOX (1),NSEALM , MPI_REAL, IROOT,   &
 2104                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2105                   IH     = IH + 1
 2106                   IT     = IT + 1
 2107       CALL MPI_SEND_INIT (TAUOY (1),NSEALM , MPI_REAL, IROOT,   &
 2108                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2109                 END IF
 2110 !
 2111               IF ( FLGRDALL( 6, 3) ) THEN
 2112                   IH     = IH + 1
 2113                   IT     = IT + 1
 2114       CALL MPI_SEND_INIT (BHD(1),NSEALM , MPI_REAL, IROOT,   &
 2115                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2116                 END IF
 2117 !
 2118               IF ( FLGRDALL( 6, 4) ) THEN
 2119                   IH     = IH + 1
 2120                   IT     = IT + 1
 2121       CALL MPI_SEND_INIT (PHIOC (1),NSEALM , MPI_REAL, IROOT,   &
 2122                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2123                 END IF
 2124 !
 2125               IF ( FLGRDALL( 6, 5) ) THEN
 2126                   IH     = IH + 1
 2127                   IT     = IT + 1
 2128       CALL MPI_SEND_INIT (TUSX  (1),NSEALM , MPI_REAL, IROOT,   &
 2129                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2130                   IH     = IH + 1
 2131                   IT     = IT + 1
 2132       CALL MPI_SEND_INIT (TUSY  (1),NSEALM , MPI_REAL, IROOT,   &
 2133                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2134                 END IF
 2135 !
 2136               IF ( FLGRDALL( 6, 6) ) THEN
 2137                   IH     = IH + 1
 2138                   IT     = IT + 1
 2139       CALL MPI_SEND_INIT (USSX  (1),NSEALM , MPI_REAL, IROOT,   &
 2140                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2141                   IH     = IH + 1
 2142                   IT     = IT + 1
 2143       CALL MPI_SEND_INIT (USSY  (1),NSEALM , MPI_REAL, IROOT,   &
 2144                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2145                 END IF
 
 
                                                         w3initmd.F90  page  40
 
 
 2146 !
 2147               IF ( FLGRDALL( 6, 7) ) THEN
 2148                   IH     = IH + 1
 2149                   IT     = IT + 1
 2150       CALL MPI_SEND_INIT (PRMS  (1),NSEALM , MPI_REAL, IROOT,   &
 2151                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2152                   IH     = IH + 1
 2153                   IT     = IT + 1
 2154       CALL MPI_SEND_INIT (TPMS  (1),NSEALM , MPI_REAL, IROOT,   &
 2155                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2156                 END IF
 2157 !
 2158               IF ( FLGRDALL( 6, 8) ) THEN
 2159                   DO IK=1,2*NK
 2160                     IH     = IH + 1
 2161                     IT     = IT + 1
 2162       CALL MPI_SEND_INIT (US3D(1,IK),NSEALM , MPI_REAL, IROOT,  &
 2163                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2164                     END DO
 2165                 END IF
 2166 !
 2167              IF ( FLGRDALL( 6, 9) ) THEN
 2168                       DO K=P2MSF(2),P2MSF(3)
 2169                         IH     = IH + 1
 2170                         IT     = IT + 1
 2171       CALL MPI_SEND_INIT (P2SMS(1,K),NSEALM , MPI_REAL, IROOT,  &
 2172                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2173                         END DO
 2174                 END IF
 2175 !
 2176               IF ( FLGRDALL( 6,10) ) THEN
 2177                   IH     = IH + 1
 2178                   IT     = IT + 1
 2179       CALL MPI_SEND_INIT (TAUICE (1,1),NSEALM , MPI_REAL, IROOT,   &
 2180                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2181                   IH     = IH + 1
 2182                   IT     = IT + 1
 2183       CALL MPI_SEND_INIT (TAUICE (1,2),NSEALM , MPI_REAL, IROOT,   &
 2184                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2185                 END IF
 2186 !
 2187               IF ( FLGRDALL( 6,11) ) THEN
 2188                   IH     = IH + 1
 2189                   IT     = IT + 1
 2190       CALL MPI_SEND_INIT (PHICE (1),NSEALM , MPI_REAL, IROOT,   &
 2191                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2192                 END IF
 2193 !
 2194               IF ( FLGRDALL( 6, 12) ) THEN
 2195                   DO IK=1,2*NK
 2196                     IH     = IH + 1
 2197                     IT     = IT + 1
 2198       CALL MPI_SEND_INIT (USSP(1,IK),NSEALM , MPI_REAL, IROOT,  &
 2199                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2200                     END DO
 
 
                                                         w3initmd.F90  page  41
 
 
 2201                 END IF
 2202 !
 2203               IF ( FLGRDALL( 7, 1) ) THEN
 2204                   IH     = IH + 1
 2205                   IT     = IT + 1
 2206       CALL MPI_SEND_INIT (ABA   (1),NSEALM , MPI_REAL, IROOT,   &
 2207                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2208                   IH     = IH + 1
 2209                   IT     = IT + 1
 2210       CALL MPI_SEND_INIT (ABD   (1),NSEALM , MPI_REAL, IROOT,   &
 2211                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2212                 END IF
 2213 !
 2214               IF ( FLGRDALL( 7, 2) ) THEN
 2215                   IH     = IH + 1
 2216                   IT     = IT + 1
 2217       CALL MPI_SEND_INIT (UBA   (1),NSEALM , MPI_REAL, IROOT,   &
 2218                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2219                   IH     = IH + 1
 2220                   IT     = IT + 1
 2221       CALL MPI_SEND_INIT (UBD   (1),NSEALM , MPI_REAL, IROOT,   &
 2222                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2223                 END IF
 2224 !
 2225               IF ( FLGRDALL( 7, 3) ) THEN
 2226                   IH     = IH + 1
 2227                   IT     = IT + 1
 2228       CALL MPI_SEND_INIT (BEDFORMS(1,1),NSEALM , MPI_REAL,      &
 2229                          IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2230                   IH     = IH + 1
 2231                   IT     = IT + 1
 2232       CALL MPI_SEND_INIT (BEDFORMS(1,2),NSEALM , MPI_REAL,      &
 2233                          IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2234                   IH     = IH + 1
 2235                   IT     = IT + 1
 2236       CALL MPI_SEND_INIT (BEDFORMS(1,3),NSEALM , MPI_REAL,      &
 2237                          IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2238                 END IF
 2239 !
 2240               IF ( FLGRDALL( 7, 4) ) THEN
 2241                   IH     = IH + 1
 2242                   IT     = IT + 1
 2243       CALL MPI_SEND_INIT (PHIBBL(1),NSEALM , MPI_REAL, IROOT,   &
 2244                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2245                 END IF
 2246 !
 2247               IF ( FLGRDALL( 7, 5) ) THEN
 2248                   IH     = IH + 1
 2249                   IT     = IT + 1
 2250       CALL MPI_SEND_INIT (TAUBBL(1,1),NSEALM , MPI_REAL,        &
 2251                          IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2252                   IH     = IH + 1
 2253                   IT     = IT + 1
 2254       CALL MPI_SEND_INIT (TAUBBL(1,2),NSEALM , MPI_REAL,        &
 2255                          IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 
 
                                                         w3initmd.F90  page  42
 
 
 2256                 END IF
 2257 !
 2258               IF ( FLGRDALL( 8, 1) ) THEN
 2259                   IH     = IH + 1
 2260                   IT     = IT + 1
 2261       CALL MPI_SEND_INIT (MSSX  (1),NSEALM , MPI_REAL, IROOT,   &
 2262                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2263                   IH     = IH + 1
 2264                   IT     = IT + 1
 2265        CALL MPI_SEND_INIT (MSSY  (1),NSEALM , MPI_REAL, IROOT,   &
 2266                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2267                 END IF
 2268 !
 2269               IF ( FLGRDALL( 8, 2) ) THEN
 2270                   IH     = IH + 1
 2271                   IT     = IT + 1
 2272       CALL MPI_SEND_INIT (MSCX  (1),NSEALM , MPI_REAL, IROOT,   &
 2273                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2274                   IH     = IH + 1
 2275                   IT     = IT + 1
 2276       CALL MPI_SEND_INIT (MSCY  (1),NSEALM , MPI_REAL, IROOT,   &
 2277                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2278                 END IF
 2279 !
 2280               IF ( FLGRDALL( 8, 3) ) THEN
 2281                   IH     = IH + 1
 2282                   IT     = IT + 1
 2283       CALL MPI_SEND_INIT (MSSD  (1),NSEALM , MPI_REAL, IROOT,    &
 2284                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2285                 END IF
 2286 !
 2287               IF ( FLGRDALL( 8, 4) ) THEN
 2288                   IH     = IH + 1
 2289                   IT     = IT + 1
 2290       CALL MPI_SEND_INIT (MSCD  (1),NSEALM , MPI_REAL, IROOT,    &
 2291                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2292                 END IF
 2293 !
 2294               IF ( FLGRDALL( 8, 5) ) THEN
 2295                   IH     = IH + 1
 2296                   IT     = IT + 1
 2297       CALL MPI_SEND_INIT (QP    (1),NSEALM , MPI_REAL, IROOT,    &
 2298                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2299                 END IF
 2300 !
 2301               IF ( FLGRDALL( 9, 1) ) THEN
 2302                   IH     = IH + 1
 2303                   IT     = IT + 1
 2304       CALL MPI_SEND_INIT (DTDYN(1),NSEALM , MPI_REAL, IROOT,    &
 2305                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2306                 END IF
 2307 !
 2308               IF ( FLGRDALL( 9, 2) ) THEN
 2309                   IH     = IH + 1
 2310                   IT     = IT + 1
 
 
                                                         w3initmd.F90  page  43
 
 
 2311       CALL MPI_SEND_INIT (FCUT (1),NSEALM , MPI_REAL, IROOT,    &
 2312                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2313                 END IF
 2314 !
 2315               IF ( FLGRDALL( 9, 3) ) THEN
 2316                   IH     = IH + 1
 2317                   IT     = IT + 1
 2318       CALL MPI_SEND_INIT (CFLXYMAX(1),NSEALM , MPI_REAL, IROOT, &
 2319                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2320                 END IF
 2321 !
 2322               IF ( FLGRDALL( 9, 4) ) THEN
 2323                   IH     = IH + 1
 2324                   IT     = IT + 1
 2325       CALL MPI_SEND_INIT (CFLTHMAX(1),NSEALM , MPI_REAL, IROOT, &
 2326                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2327                 END IF
 2328 !
 2329               IF ( FLGRDALL( 9, 5) ) THEN
 2330                   IH     = IH + 1
 2331                   IT     = IT + 1
 2332       CALL MPI_SEND_INIT (CFLKMAX(1),NSEALM , MPI_REAL, IROOT,  &
 2333                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2334                 END IF
 2335 !
 2336               DO I=1, NOEXTR
 2337                 IF ( FLGRDALL(10, I) ) THEN
 2338                     IH     = IH + 1
 2339                     IT     = IT + 1
 2340       CALL MPI_SEND_INIT (USERO(1,I),NSEALM , MPI_REAL, IROOT,  &
 2341                                 IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
 2342                   END IF
 2343                 END DO
 2344 !
 2345                NRQGO  = IH
 2346 !
 2347             END IF
 2348 !
 2349           IF ( NRQGO .GT. NRQMAX ) THEN
 2350               WRITE (NDSE,1010) NRQGO, NRQMAX
 2351               CALL EXTCDE (10)
 2352             END IF
 2353 !
 2354           IF ( IAPROC .EQ. NAPFLD ) THEN
 2355 !
 2356 ! 1.b Setting up expanded arrays
 2357 !
 2358               IF (NAPFLD .EQ. NAPRST) THEN
 2359                 CALL W3XDMA ( IMOD, NDSE, NDST, FLGRDARST )
 2360               ELSE
 2361                 CALL W3XDMA ( IMOD, NDSE, NDST, FLGRDALL )
 2362               ENDIF
 2363 !
 2364 ! 1.c Receives of fields
 2365 !
 
 
                                                         w3initmd.F90  page  44
 
 
 2366               CALL W3XETA ( IMOD, NDSE, NDST )
 2367 !
 2368               IH     = 0
 2369 !
 2370               DO I0=1, NAPROC
 2371                 IT     = IT0
 2372                 IFROM  = I0 - 1
 2373 !
 2374                 IF ( FLGRDALL( 1, 9) ) THEN
 2375                     IH     = IH + 1
 2376                     IT     = IT + 1
 2377       CALL MPI_RECV_INIT (ICEF (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2378                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2379                   END IF
 2380 !
 2381                 IF ( FLGRDALL( 2, 1) ) THEN
 2382                     IH     = IH + 1
 2383                     IT     = IT + 1
 2384       CALL MPI_RECV_INIT (HS   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2385                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2386                   END IF
 2387 !
 2388                 IF ( FLGRDALL( 2, 2) ) THEN
 2389                     IH     = IH + 1
 2390                     IT     = IT + 1
 2391       CALL MPI_RECV_INIT (WLM  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2392                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2393                   END IF
 2394 !
 2395                 IF ( FLGRDALL( 2, 3) ) THEN
 2396                     IH     = IH + 1
 2397                     IT     = IT + 1
 2398       CALL MPI_RECV_INIT (T02  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2399                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2400                   END IF
 2401 !
 2402                 IF ( FLGRDALL( 2, 4) ) THEN
 2403                     IH     = IH + 1
 2404                     IT     = IT + 1
 2405       CALL MPI_RECV_INIT (T0M1  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2406                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2407                   END IF
 2408 !
 2409                 IF ( FLGRDALL( 2, 5) ) THEN
 2410                     IH     = IH + 1
 2411                     IT     = IT + 1
 2412       CALL MPI_RECV_INIT (T01(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2413                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2414                   END IF
 2415 !
 2416                 IF ( FLGRDALL( 2, 6) .OR. FLGRDALL( 2,18) ) THEN
 2417                     ! TP output shares FP0 internal field with FP
 2418                     IH     = IH + 1
 2419                     IT     = IT + 1
 2420       CALL MPI_RECV_INIT (FP0  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 
 
                                                         w3initmd.F90  page  45
 
 
 2421                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2422                   END IF
 2423 !
 2424                 IF ( FLGRDALL( 2, 7) ) THEN
 2425                     IH     = IH + 1
 2426                     IT     = IT + 1
 2427       CALL MPI_RECV_INIT (THM  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2428                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2429                   END IF
 2430 !
 2431                 IF ( FLGRDALL( 2, 8) ) THEN
 2432                     IH     = IH + 1
 2433                     IT     = IT + 1
 2434       CALL MPI_RECV_INIT (THS  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2435                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2436                   END IF
 2437 !
 2438                 IF ( FLGRDALL( 2, 9) ) THEN
 2439                     IH     = IH + 1
 2440                     IT     = IT + 1
 2441       CALL MPI_RECV_INIT (THP0 (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2442                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2443                   END IF
 2444 !
 2445                 IF ( FLGRDALL( 2, 10) ) THEN
 2446                     IH     = IH + 1
 2447                     IT     = IT + 1
 2448       CALL MPI_RECV_INIT (HSIG (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2449                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2450                   END IF
 2451 !
 2452                 IF ( FLGRDALL( 2, 11) ) THEN
 2453                     IH     = IH + 1
 2454                     IT     = IT + 1
 2455       CALL MPI_RECV_INIT (STMAXE (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2456                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2457                   END IF
 2458 !
 2459                 IF ( FLGRDALL( 2, 12) ) THEN
 2460                     IH     = IH + 1
 2461                     IT     = IT + 1
 2462       CALL MPI_RECV_INIT (STMAXD(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2463                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2464                   END IF
 2465 !
 2466                 IF ( FLGRDALL( 2, 13) ) THEN
 2467                     IH     = IH + 1
 2468                     IT     = IT + 1
 2469       CALL MPI_RECV_INIT (HMAXE (I0),1,WW3_FIELD_VEC, IFROM, IT, &
 2470                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2471                   END IF
 2472 !
 2473                 IF ( FLGRDALL( 2, 14) ) THEN
 2474                     IH     = IH + 1
 2475                     IT     = IT + 1
 
 
                                                         w3initmd.F90  page  46
 
 
 2476       CALL MPI_RECV_INIT (HCMAXE(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2477                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2478                   END IF
 2479 !
 2480                 IF ( FLGRDALL( 2, 15) ) THEN
 2481                     IH     = IH + 1
 2482                     IT     = IT + 1
 2483       CALL MPI_RECV_INIT (HMAXD (I0),1,WW3_FIELD_VEC, IFROM, IT, &
 2484                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2485                   END IF
 2486 !
 2487                 IF ( FLGRDALL( 2, 16) ) THEN
 2488                     IH     = IH + 1
 2489                     IT     = IT + 1
 2490       CALL MPI_RECV_INIT (HCMAXD(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2491                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2492                   END IF
 2493 !
 2494                 IF ( FLGRDALL( 2, 17) ) THEN
 2495                     IH     = IH + 1
 2496                     IT     = IT + 1
 2497       CALL MPI_RECV_INIT (WBT(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2498                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2499                   END IF
 2500 !
 2501                 IF ( FLGRDALL( 3, 1) ) THEN
 2502                     DO IK=E3DF(2,1),E3DF(3,1)
 2503                       IH     = IH + 1
 2504                       IT     = IT + 1
 2505       CALL MPI_RECV_INIT (EF(I0,IK),1,WW3_FIELD_VEC, IFROM, IT,&
 2506                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2507                       END DO
 2508                     END IF
 2509 !
 2510                 IF ( FLGRDALL( 3, 2) ) THEN
 2511                     DO IK=E3DF(2,2),E3DF(3,2)
 2512                       IH     = IH + 1
 2513                       IT     = IT + 1
 2514       CALL MPI_RECV_INIT (TH1M(I0,IK),1,WW3_FIELD_VEC, IFROM, IT,&
 2515                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2516                       END DO
 2517                     END IF
 2518 !
 2519                 IF ( FLGRDALL( 3, 3) ) THEN
 2520                     DO IK=E3DF(2,3),E3DF(3,3)
 2521                       IH     = IH + 1
 2522                       IT     = IT + 1
 2523       CALL MPI_RECV_INIT (STH1M(I0,IK),1,WW3_FIELD_VEC, IFROM, IT,&
 2524                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2525                       END DO
 2526                     END IF
 2527 !
 2528                 IF ( FLGRDALL( 3, 4) ) THEN
 2529                     DO IK=E3DF(2,4),E3DF(3,4)
 2530                       IH     = IH + 1
 
 
                                                         w3initmd.F90  page  47
 
 
 2531                       IT     = IT + 1
 2532       CALL MPI_RECV_INIT (TH2M(I0,IK),1,WW3_FIELD_VEC, IFROM, IT,&
 2533                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2534                       END DO
 2535                     END IF
 2536 !
 2537                IF ( FLGRDALL( 3, 5) ) THEN
 2538                     DO IK=E3DF(2,5),E3DF(3,5)
 2539                       IH     = IH + 1
 2540                       IT     = IT + 1
 2541       CALL MPI_RECV_INIT (STH2M(I0,IK),1,WW3_FIELD_VEC, IFROM, IT,&
 2542                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2543                       END DO
 2544                     END IF
 2545 !
 2546                 IF ( FLGRDALL( 4, 1) ) THEN
 2547                   DO K=0, NOSWLL
 2548                     IH     = IH + 1
 2549                     IT     = IT + 1
 2550       CALL MPI_RECV_INIT (PHS(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
 2551                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2552                     END DO
 2553                   END IF
 2554 !
 2555                 IF ( FLGRDALL( 4, 2) ) THEN
 2556                   DO K=0, NOSWLL
 2557                     IH     = IH + 1
 2558                     IT     = IT + 1
 2559       CALL MPI_RECV_INIT (PTP(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
 2560                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2561                     END DO
 2562                   END IF
 2563 !
 2564                 IF ( FLGRDALL( 4, 3) ) THEN
 2565                   DO K=0, NOSWLL
 2566                     IH     = IH + 1
 2567                     IT     = IT + 1
 2568       CALL MPI_RECV_INIT (PLP(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
 2569                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2570                     END DO
 2571                   END IF
 2572 !
 2573                 IF ( FLGRDALL( 4, 4) ) THEN
 2574                   DO K=0, NOSWLL
 2575                     IH     = IH + 1
 2576                     IT     = IT + 1
 2577       CALL MPI_RECV_INIT (PDIR(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
 2578                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2579                     END DO
 2580                   END IF
 2581 !
 2582                 IF ( FLGRDALL( 4, 5) ) THEN
 2583                   DO K=0, NOSWLL
 2584                     IH     = IH + 1
 2585                     IT     = IT + 1
 
 
                                                         w3initmd.F90  page  48
 
 
 2586       CALL MPI_RECV_INIT (PSI(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
 2587                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2588                     END DO
 2589                   END IF
 2590 !
 2591                 IF ( FLGRDALL( 4, 6) ) THEN
 2592                   DO K=0, NOSWLL
 2593                     IH     = IH + 1
 2594                     IT     = IT + 1
 2595       CALL MPI_RECV_INIT (PWS(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
 2596                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2597                     END DO
 2598                   END IF
 2599 !
 2600                 IF ( FLGRDALL( 4, 7) ) THEN
 2601                   DO K=0, NOSWLL
 2602                     IH     = IH + 1
 2603                     IT     = IT + 1
 2604       CALL MPI_RECV_INIT (PTHP0(I0,K),1,WW3_FIELD_VEC, IFROM, IT,&
 2605                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2606                     END DO
 2607                   END IF
 2608 !
 2609                 IF ( FLGRDALL( 4, 8) ) THEN
 2610                   DO K=0, NOSWLL
 2611                     IH     = IH + 1
 2612                     IT     = IT + 1
 2613       CALL MPI_RECV_INIT (PQP(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
 2614                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2615                     END DO
 2616                   END IF
 2617 !
 2618                 IF ( FLGRDALL( 4, 9) ) THEN
 2619                   DO K=0, NOSWLL
 2620                     IH     = IH + 1
 2621                     IT     = IT + 1
 2622       CALL MPI_RECV_INIT (PPE(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
 2623                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2624                     END DO
 2625                   END IF
 2626 !
 2627                 IF ( FLGRDALL( 4,10) ) THEN
 2628                   DO K=0, NOSWLL
 2629                     IH     = IH + 1
 2630                     IT     = IT + 1
 2631       CALL MPI_RECV_INIT (PGW(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
 2632                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2633                     END DO
 2634                   END IF
 2635 !
 2636                 IF ( FLGRDALL( 4,11) ) THEN
 2637                   DO K=0, NOSWLL
 2638                     IH     = IH + 1
 2639                     IT     = IT + 1
 2640       CALL MPI_RECV_INIT (PSW(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
 
 
                                                         w3initmd.F90  page  49
 
 
 2641                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2642                     END DO
 2643                   END IF
 2644 !
 2645                 IF ( FLGRDALL( 4,12) ) THEN
 2646                   DO K=0, NOSWLL
 2647                     IH     = IH + 1
 2648                     IT     = IT + 1
 2649       CALL MPI_RECV_INIT (PTM1(I0,K),1,WW3_FIELD_VEC, IFROM, IT,&
 2650                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2651                     END DO
 2652                   END IF
 2653 !
 2654                 IF ( FLGRDALL( 4,13) ) THEN
 2655                   DO K=0, NOSWLL
 2656                     IH     = IH + 1
 2657                     IT     = IT + 1
 2658       CALL MPI_RECV_INIT (PT1(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
 2659                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2660                     END DO
 2661                   END IF
 2662 !
 2663                 IF ( FLGRDALL( 4,14) ) THEN
 2664                   DO K=0, NOSWLL
 2665                     IH     = IH + 1
 2666                     IT     = IT + 1
 2667       CALL MPI_RECV_INIT (PT2(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
 2668                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2669                     END DO
 2670                   END IF
 2671 !
 2672                 IF ( FLGRDALL( 4,15) ) THEN
 2673                   DO K=0, NOSWLL
 2674                     IH     = IH + 1
 2675                     IT     = IT + 1
 2676       CALL MPI_RECV_INIT (PEP(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
 2677                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2678                     END DO
 2679                   END IF
 2680 !
 2681                 IF ( FLGRDALL( 4,16) ) THEN
 2682                     IH     = IH + 1
 2683                     IT     = IT + 1
 2684       CALL MPI_RECV_INIT (PWST (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2685                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2686                   END IF
 2687 !
 2688                 IF ( FLGRDALL( 4,17) ) THEN
 2689                     IH     = IH + 1
 2690                     IT     = IT + 1
 2691       CALL MPI_RECV_INIT (PNR  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2692                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2693                   END IF
 2694 !
 2695                 IF ( FLGRDALL( 5, 1) ) THEN
 
 
                                                         w3initmd.F90  page  50
 
 
 2696                     IH     = IH + 1
 2697                     IT     = IT + 1
 2698       CALL MPI_RECV_INIT (UST   (I0), 1, WW3_FIELD_VEC, IFROM,   &
 2699                              IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2700                     IH     = IH + 1
 2701                     IT     = IT + 1
 2702       CALL MPI_RECV_INIT (USTDIR(I0), 1, WW3_FIELD_VEC, IFROM,   &
 2703                              IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2704                     IH     = IH + 1
 2705                     IT     = IT + 1
 2706       CALL MPI_RECV_INIT (ASF   (I0), 1, WW3_FIELD_VEC, IFROM,   &
 2707                              IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2708                   END IF
 2709 !
 2710                 IF ( FLGRDALL( 5, 2) ) THEN
 2711                     IH     = IH + 1
 2712                     IT     = IT + 1
 2713       CALL MPI_RECV_INIT (CHARN(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2714                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2715                   END IF
 2716 !
 2717                 IF ( FLGRDALL( 5, 3) ) THEN
 2718                     IH     = IH + 1
 2719                     IT     = IT + 1
 2720       CALL MPI_RECV_INIT (CGE  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2721                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2722                   END IF
 2723 !
 2724                 IF ( FLGRDALL( 5, 4) ) THEN
 2725                     IH     = IH + 1
 2726                     IT     = IT + 1
 2727       CALL MPI_RECV_INIT (PHIAW(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2728                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2729                   END IF
 2730 !
 2731                 IF ( FLGRDALL( 5, 5) ) THEN
 2732                     IH     = IH + 1
 2733                     IT     = IT + 1
 2734       CALL MPI_RECV_INIT (TAUWIX(I0),1,WW3_FIELD_VEC, IFROM, IT, &
 2735                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2736                     IH     = IH + 1
 2737                     IT     = IT + 1
 2738       CALL MPI_RECV_INIT (TAUWIY(I0),1,WW3_FIELD_VEC, IFROM, IT, &
 2739                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2740                   END IF
 2741 !
 2742                 IF ( FLGRDALL( 5, 6) ) THEN
 2743                     IH     = IH + 1
 2744                     IT     = IT + 1
 2745       CALL MPI_RECV_INIT (TAUWNX(I0),1,WW3_FIELD_VEC, IFROM, IT, &
 2746                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2747                     IH     = IH + 1
 2748                     IT     = IT + 1
 2749       CALL MPI_RECV_INIT (TAUWNY(I0),1,WW3_FIELD_VEC, IFROM, IT, &
 2750                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 
 
                                                         w3initmd.F90  page  51
 
 
 2751                   END IF
 2752 !
 2753                 IF ( FLGRDALL( 5, 7) ) THEN
 2754                     IH     = IH + 1
 2755                     IT     = IT + 1
 2756       CALL MPI_RECV_INIT (WHITECAP(I0,1),1,WW3_FIELD_VEC, IFROM,  &
 2757                                IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2758                   END IF
 2759 !
 2760                 IF ( FLGRDALL( 5, 8) ) THEN
 2761                     IH     = IH + 1
 2762                     IT     = IT + 1
 2763       CALL MPI_RECV_INIT (WHITECAP(I0,2),1,WW3_FIELD_VEC, IFROM,  &
 2764                                IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2765                   END IF
 2766 !
 2767                 IF ( FLGRDALL( 5, 9) ) THEN
 2768                     IH     = IH + 1
 2769                     IT     = IT + 1
 2770       CALL MPI_RECV_INIT (WHITECAP(I0,3),1,WW3_FIELD_VEC, IFROM,  &
 2771                                IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2772                   END IF
 2773 !
 2774                 IF ( FLGRDALL( 5,10) ) THEN
 2775                     IH     = IH + 1
 2776                     IT     = IT + 1
 2777       CALL MPI_RECV_INIT (WHITECAP(I0,4),1,WW3_FIELD_VEC, IFROM,  &
 2778                                IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2779                   END IF
 2780 !
 2781                 IF ( FLGRDALL( 5,11) ) THEN
 2782                     IH     = IH + 1
 2783                     IT     = IT + 1
 2784       CALL MPI_RECV_INIT (TWS(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2785                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2786                   END IF
 2787 !
 2788                 IF ( FLGRDALL( 6, 1) ) THEN
 2789                     IH     = IH + 1
 2790                     IT     = IT + 1
 2791       CALL MPI_RECV_INIT (SXX   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2792                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2793                     IH     = IH + 1
 2794                     IT     = IT + 1
 2795       CALL MPI_RECV_INIT (SYY   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2796                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2797                     IH     = IH + 1
 2798                     IT     = IT + 1
 2799       CALL MPI_RECV_INIT (SXY   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2800                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2801                   END IF
 2802 !
 2803                 IF ( FLGRDALL( 6, 2) ) THEN
 2804                     IH     = IH + 1
 2805                     IT     = IT + 1
 
 
                                                         w3initmd.F90  page  52
 
 
 2806       CALL MPI_RECV_INIT (TAUOX (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2807                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2808                     IH     = IH + 1
 2809                     IT     = IT + 1
 2810       CALL MPI_RECV_INIT (TAUOY (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2811                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2812                   END IF
 2813 !
 2814                 IF ( FLGRDALL( 6, 3) ) THEN
 2815                     IH     = IH + 1
 2816                     IT     = IT + 1
 2817       CALL MPI_RECV_INIT (BHD(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2818                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2819                   END IF
 2820 !
 2821                 IF ( FLGRDALL( 6, 4) ) THEN
 2822                     IH     = IH + 1
 2823                     IT     = IT + 1
 2824       CALL MPI_RECV_INIT (PHIOC (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2825                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2826                   END IF
 2827 !
 2828                 IF ( FLGRDALL( 6, 5) ) THEN
 2829                     IH     = IH + 1
 2830                     IT     = IT + 1
 2831       CALL MPI_RECV_INIT (TUSX  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2832                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2833                     IH     = IH + 1
 2834                     IT     = IT + 1
 2835       CALL MPI_RECV_INIT (TUSY  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2836                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2837                   END IF
 2838 !
 2839                 IF ( FLGRDALL( 6, 6) ) THEN
 2840                     IH     = IH + 1
 2841                     IT     = IT + 1
 2842       CALL MPI_RECV_INIT (USSX  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2843                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2844                     IH     = IH + 1
 2845                     IT     = IT + 1
 2846       CALL MPI_RECV_INIT (USSY  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2847                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2848                   END IF
 2849 !
 2850                 IF ( FLGRDALL( 6, 7) ) THEN
 2851                     IH     = IH + 1
 2852                     IT     = IT + 1
 2853       CALL MPI_RECV_INIT (PRMS  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2854                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2855                     IH     = IH + 1
 2856                     IT     = IT + 1
 2857       CALL MPI_RECV_INIT (TPMS  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2858                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2859                   END IF
 2860 !
 
 
                                                         w3initmd.F90  page  53
 
 
 2861                 IF ( FLGRDALL( 6, 8) ) THEN
 2862                     DO IK=1,2*NK
 2863                       IH     = IH + 1
 2864                       IT     = IT + 1
 2865       CALL MPI_RECV_INIT (US3D(I0,IK),1,WW3_FIELD_VEC, IFROM, IT, &
 2866                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2867                       END DO
 2868                   END IF
 2869 !
 2870                 IF (  FLGRDALL( 6, 9) ) THEN
 2871                       DO K=P2MSF(2),P2MSF(3)
 2872                         IH     = IH + 1
 2873                         IT     = IT + 1
 2874       CALL MPI_RECV_INIT (P2SMS(I0,K),1,WW3_FIELD_VEC, IFROM, IT, &
 2875                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2876                         END DO
 2877                   END IF
 2878 !
 2879                 IF ( FLGRDALL( 6,10) ) THEN
 2880                     IH     = IH + 1
 2881                     IT     = IT + 1
 2882       CALL MPI_RECV_INIT (TAUICE (I0,1),1,WW3_FIELD_VEC, IFROM, IT,  &
 2883                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2884                     IH     = IH + 1
 2885                     IT     = IT + 1
 2886       CALL MPI_RECV_INIT (TAUICE (I0,2),1,WW3_FIELD_VEC, IFROM, IT,  &
 2887                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2888                   END IF
 2889 !
 2890                 IF ( FLGRDALL( 6,11) ) THEN
 2891                     IH     = IH + 1
 2892                     IT     = IT + 1
 2893       CALL MPI_RECV_INIT (PHICE (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2894                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2895                   END IF
 2896 !
 2897                 IF ( FLGRDALL( 6, 12) ) THEN
 2898                     DO IK=1,2*NK
 2899                       IH     = IH + 1
 2900                       IT     = IT + 1
 2901       CALL MPI_RECV_INIT (USSP(I0,IK),1,WW3_FIELD_VEC, IFROM, IT, &
 2902                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2903                       END DO
 2904                   END IF
 2905 !
 2906                 IF ( FLGRDALL( 7, 1) ) THEN
 2907                     IH     = IH + 1
 2908                     IT     = IT + 1
 2909       CALL MPI_RECV_INIT (ABA   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2910                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2911                     IH     = IH + 1
 2912                     IT     = IT + 1
 2913       CALL MPI_RECV_INIT (ABD   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2914                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2915                   END IF
 
 
                                                         w3initmd.F90  page  54
 
 
 2916 !
 2917                 IF ( FLGRDALL( 7, 2) ) THEN
 2918                     IH     = IH + 1
 2919                     IT     = IT + 1
 2920       CALL MPI_RECV_INIT (UBA   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2921                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2922                     IH     = IH + 1
 2923                     IT     = IT + 1
 2924       CALL MPI_RECV_INIT (UBD   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2925                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2926                   END IF
 2927 !
 2928                 IF ( FLGRDALL( 7, 3) ) THEN
 2929                     IH     = IH + 1
 2930                     IT     = IT + 1
 2931       CALL MPI_RECV_INIT (BEDFORMS(I0,1),1,WW3_FIELD_VEC, IFROM,  &
 2932                            IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2933                     IH     = IH + 1
 2934                     IT     = IT + 1
 2935       CALL MPI_RECV_INIT (BEDFORMS(I0,2),1,WW3_FIELD_VEC, IFROM,  &
 2936                            IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2937                     IH     = IH + 1
 2938                     IT     = IT + 1
 2939       CALL MPI_RECV_INIT (BEDFORMS(I0,3),1,WW3_FIELD_VEC, IFROM,  &
 2940                            IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2941                   END IF
 2942 !
 2943                 IF ( FLGRDALL( 7, 4) ) THEN
 2944                     IH     = IH + 1
 2945                     IT     = IT + 1
 2946       CALL MPI_RECV_INIT (PHIBBL(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2947                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2948                   END IF
 2949 !
 2950                 IF ( FLGRDALL( 7, 5) ) THEN
 2951                     IH     = IH + 1
 2952                     IT     = IT + 1
 2953       CALL MPI_RECV_INIT (TAUBBL(I0,1),1,WW3_FIELD_VEC, IFROM,    &
 2954                            IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2955                     IH     = IH + 1
 2956                     IT     = IT + 1
 2957       CALL MPI_RECV_INIT (TAUBBL(I0,2),1,WW3_FIELD_VEC, IFROM,    &
 2958                            IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2959                   END IF
 2960 !
 2961                 IF ( FLGRDALL( 8, 1) ) THEN
 2962                     IH     = IH + 1
 2963                     IT     = IT + 1
 2964       CALL MPI_RECV_INIT (MSSX  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2965                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2966                     IH     = IH + 1
 2967                     IT     = IT + 1
 2968       CALL MPI_RECV_INIT (MSSY  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2969                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2970                   END IF
 
 
                                                         w3initmd.F90  page  55
 
 
 2971 !
 2972                 IF ( FLGRDALL( 8, 2) ) THEN
 2973                     IH     = IH + 1
 2974                     IT     = IT + 1
 2975       CALL MPI_RECV_INIT (MSCX  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2976                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2977                     IH     = IH + 1
 2978                     IT     = IT + 1
 2979       CALL MPI_RECV_INIT (MSCY  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2980                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2981                   END IF
 2982 !
 2983                 IF ( FLGRDALL( 8, 3) ) THEN
 2984                     IH     = IH + 1
 2985                     IT     = IT + 1
 2986       CALL MPI_RECV_INIT (MSSD  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2987                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2988                   END IF
 2989 !
 2990                 IF ( FLGRDALL( 8, 4) ) THEN
 2991                     IH     = IH + 1
 2992                     IT     = IT + 1
 2993       CALL MPI_RECV_INIT (MSCD (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 2994                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 2995                   END IF
 2996 !
 2997                 IF ( FLGRDALL( 8, 5) ) THEN
 2998                     IH     = IH + 1
 2999                     IT     = IT + 1
 3000       CALL MPI_RECV_INIT (QP   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 3001                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 3002                   END IF
 3003 !
 3004                 IF ( FLGRDALL( 9, 1) ) THEN
 3005                     IH     = IH + 1
 3006                     IT     = IT + 1
 3007       CALL MPI_RECV_INIT (DTDYN(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 3008                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 3009                   END IF
 3010 !
 3011                 IF ( FLGRDALL( 9, 2) ) THEN
 3012                     IH     = IH + 1
 3013                     IT     = IT + 1
 3014       CALL MPI_RECV_INIT (FCUT (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
 3015                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 3016                   END IF
 3017 !
 3018                 IF ( FLGRDALL( 9, 3) ) THEN
 3019                     IH     = IH + 1
 3020                     IT     = IT + 1
 3021       CALL MPI_RECV_INIT (CFLXYMAX(I0),1,WW3_FIELD_VEC, IFROM, IT,&
 3022                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 3023                   END IF
 3024 !
 3025                 IF ( FLGRDALL( 9, 4) ) THEN
 
 
                                                         w3initmd.F90  page  56
 
 
 3026                     IH     = IH + 1
 3027                     IT     = IT + 1
 3028       CALL MPI_RECV_INIT (CFLTHMAX(I0),1,WW3_FIELD_VEC, IFROM, IT,&
 3029                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 3030                   END IF
 3031 !
 3032                 IF ( FLGRDALL( 9, 5) ) THEN
 3033                     IH     = IH + 1
 3034                     IT     = IT + 1
 3035       CALL MPI_RECV_INIT (CFLKMAX(I0),1,WW3_FIELD_VEC, IFROM, IT, &
 3036                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 3037                   END IF
 3038 !
 3039                 DO I=1, NOEXTR
 3040                   !WRITE(740+IAPROC,*) 'SECOND : I=', I, ' / ', NOEXTR, '>
 3041                   IF ( FLGRDALL(10, I) ) THEN
 3042                       IH     = IH + 1
 3043                       IT     = IT + 1
 3044       CALL MPI_RECV_INIT (USERO(I0,I),1,WW3_FIELD_VEC, IFROM, IT, &
 3045                                MPI_COMM_WAVE, IRQGO2(IH), IERR )
 3046                     END IF
 3047                   END DO
 3048 !
 3049                 END DO
 3050 !
 3051                NRQGO2 = IH
 3052 !
 3053               CALL W3SETA ( IMOD, NDSE, NDST )
 3054 !
 3055             END IF
 3056 !
 3057           IF ( NRQGO2 .GT. NRQMAX*NAPROC ) THEN
 3058               WRITE (NDSE,1011) NRQGO2, NRQMAX*NAPROC
 3059               CALL EXTCDE (11)
 3060             END IF
 3061 !
 3062       END IF
 3063 !
 3064 ! 2.  Set-up for W3IORS ---------------------------------------------- /
 3065 ! 2.a General preparations
 3066 !
 3067       NRQRS  = 0
 3068       IH     = 0
 3069       IROOT  = NAPRST - 1
 3070 !
 3071       IF ( FLOUT(4) .OR. FLOUT(8) ) THEN
 3072           IF (OARST) THEN
 3073             ALLOCATE ( OUTPTS(IMOD)%OUT4%IRQRS(30*NAPROC) )
 3074           ELSE
 3075             ALLOCATE ( OUTPTS(IMOD)%OUT4%IRQRS(3*NAPROC) )
 3076           ENDIF
 3077           IRQRS  => OUTPTS(IMOD)%OUT4%IRQRS
 3078 !
 3079 ! 2.b Fields at end of file (always)
 3080 !
 
 
                                                         w3initmd.F90  page  57
 
 
 3081           IF ( IAPROC.LE.NAPROC ) THEN
 3082             IF ( IAPROC.NE.NAPRST ) THEN
 3083 !
 3084               IH     = IH + 1
 3085               IT     = IT0 + 1
 3086               CALL MPI_SEND_INIT (UST (IAPROC), 1, WW3_FIELD_VEC, &
 3087                        IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3088 !
 3089               IH     = IH + 1
 3090               IT     = IT0 + 2
 3091               CALL MPI_SEND_INIT (USTDIR(IAPROC), 1, WW3_FIELD_VEC, &
 3092                        IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3093 !
 3094               IH     = IH + 1
 3095               IT     = IT0 + 3
 3096               CALL MPI_SEND_INIT (FPIS(IAPROC), 1, WW3_FIELD_VEC, &
 3097                        IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3098             END IF
 3099 !
 3100             IF (OARST) THEN
 3101               IF ( FLOGRR( 1, 2) ) THEN
 3102                 IH     = IH + 1
 3103                 IT     = IT0 + 4
 3104                 CALL MPI_SEND_INIT (CX(IAPROC), 1, WW3_FIELD_VEC,   &
 3105                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3106                 IH     = IH + 1
 3107                 IT     = IT0 + 5
 3108                 CALL MPI_SEND_INIT (CY(IAPROC), 1, WW3_FIELD_VEC,   &
 3109                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3110               END IF
 3111 !
 3112               IF ( FLOGRR( 1, 9) ) THEN
 3113                 IH     = IH + 1
 3114                 IT     = IT0 + 6
 3115                 CALL MPI_SEND_INIT (ICEF(IAPROC), 1, WW3_FIELD_VEC, &
 3116                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3117               END IF
 3118 !
 3119               IF ( FLOGRR( 2, 1) ) THEN
 3120                 IH     = IH + 1
 3121                 IT     = IT0 + 7
 3122                 CALL MPI_SEND_INIT (HS   (1), NSEALM, MPI_REAL,     &
 3123                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3124               END IF
 3125 !
 3126               IF ( FLOGRR( 2, 2) ) THEN
 3127                 IH     = IH + 1
 3128                 IT     = IT0 + 8
 3129                 CALL MPI_SEND_INIT (WLM  (1), NSEALM, MPI_REAL,     &
 3130                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3131               END IF
 3132 !
 3133               IF ( FLOGRR( 2, 4) ) THEN
 3134                 IH     = IH + 1
 3135                 IT     = IT0 + 9
 
 
                                                         w3initmd.F90  page  58
 
 
 3136                 CALL MPI_SEND_INIT (T0M1(1), NSEALM, MPI_REAL,      &
 3137                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3138               ENDIF
 3139 !
 3140               IF ( FLOGRR( 2, 6) ) THEN
 3141                 IH     = IH + 1
 3142                 IT     = IT0 + 10
 3143                 CALL MPI_SEND_INIT (FP0  (1), NSEALM, MPI_REAL,     &
 3144                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3145               END IF
 3146 !
 3147               IF ( FLOGRR( 2, 7) ) THEN
 3148                 IH     = IH + 1
 3149                 IT     = IT0 + 11
 3150                 CALL MPI_SEND_INIT (THM  (1), NSEALM, MPI_REAL,     &
 3151                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3152               END IF
 3153 !
 3154               IF ( FLOGRR( 5, 2) ) THEN
 3155                 IH     = IH + 1
 3156                 IT     = IT0 + 12
 3157                 CALL MPI_SEND_INIT (CHARN(1), NSEALM, MPI_REAL,     &
 3158                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3159               ENDIF
 3160 !
 3161               IF ( FLOGRR( 5, 5) ) THEN
 3162                 IH     = IH + 1
 3163                 IT     = IT0 + 13
 3164                 CALL MPI_SEND_INIT (TAUWIX(1), NSEALM, MPI_REAL,    &
 3165                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3166                 IH     = IH + 1
 3167                 IT     = IT0 + 14
 3168                 CALL MPI_SEND_INIT (TAUWIY(1), NSEALM, MPI_REAL,    &
 3169                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3170               END IF
 3171 !
 3172               IF ( FLOGRR( 5, 11) ) THEN
 3173                 IH     = IH + 1
 3174                 IT     = IT0 + 15
 3175                 CALL MPI_SEND_INIT (TWS  (1), NSEALM, MPI_REAL,     &
 3176                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3177               END IF
 3178 !
 3179               IF ( FLOGRR( 6, 2) ) THEN
 3180                 IH     = IH + 1
 3181                 IT     = IT0 + 16
 3182                 CALL MPI_SEND_INIT (TAUOX(1), NSEALM, MPI_REAL,     &
 3183                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3184                 IH     = IH + 1
 3185                 IT     = IT0 + 17
 3186                 CALL MPI_SEND_INIT (TAUOY(1), NSEALM, MPI_REAL,     &
 3187                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3188               END IF
 3189 !
 3190               IF ( FLOGRR( 6, 3) ) THEN
 
 
                                                         w3initmd.F90  page  59
 
 
 3191                 IH     = IH + 1
 3192                 IT     = IT0 + 18
 3193                 CALL MPI_SEND_INIT (BHD  (1), NSEALM, MPI_REAL,     &
 3194                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3195               END IF
 3196 !
 3197               IF ( FLOGRR( 6, 4) ) THEN
 3198                 IH     = IH + 1
 3199                 IT     = IT0 + 19
 3200                 CALL MPI_SEND_INIT (PHIOC(1), NSEALM, MPI_REAL,     &
 3201                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3202               END IF
 3203 !
 3204               IF ( FLOGRR( 6, 5) ) THEN
 3205                 IH     = IH + 1
 3206                 IT     = IT0 + 20
 3207                 CALL MPI_SEND_INIT (TUSX (1), NSEALM, MPI_REAL,     &
 3208                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3209                 IH     = IH + 1
 3210                 IT     = IT0 + 21
 3211                 CALL MPI_SEND_INIT (TUSY (1), NSEALM, MPI_REAL,     &
 3212                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3213               END IF
 3214 !
 3215               IF ( FLOGRR( 6, 6) ) THEN
 3216                 IH     = IH + 1
 3217                 IT     = IT0 + 22
 3218                 CALL MPI_SEND_INIT (USSX (1), NSEALM, MPI_REAL,     &
 3219                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3220                 IH     = IH + 1
 3221                 IT     = IT0 + 23
 3222                 CALL MPI_SEND_INIT (USSY (1), NSEALM, MPI_REAL,     &
 3223                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3224               END IF
 3225 !
 3226               IF ( FLOGRR( 6,10) ) THEN
 3227                 IH     = IH + 1
 3228                 IT     = IT0 + 24
 3229                 CALL MPI_SEND_INIT (TAUICE(1,1), NSEALM, MPI_REAL,  &
 3230                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3231                 IH     = IH + 1
 3232                 IT     = IT0 + 25
 3233                 CALL MPI_SEND_INIT (TAUICE(1,2), NSEALM, MPI_REAL,  &
 3234                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3235               END IF
 3236 !
 3237               IF ( FLOGRR( 7, 2) ) THEN
 3238                 IH     = IH + 1
 3239                 IT     = IT0 + 26
 3240                 CALL MPI_SEND_INIT (UBA  (1), NSEALM, MPI_REAL,     &
 3241                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3242                 IH     = IH + 1
 3243                 IT     = IT0 + 27
 3244                 CALL MPI_SEND_INIT (UBD  (1), NSEALM, MPI_REAL,     &
 3245                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 
 
                                                         w3initmd.F90  page  60
 
 
 3246               END IF
 3247 !
 3248               IF ( FLOGRR( 7, 4) ) THEN
 3249                 IH     = IH + 1
 3250                 IT     = IT0 + 28
 3251                 CALL MPI_SEND_INIT (PHIBBL(1), NSEALM, MPI_REAL,    &
 3252                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3253               END IF
 3254 !
 3255               IF ( FLOGRR( 7, 5) ) THEN
 3256                 IH     = IH + 1
 3257                 IT     = IT0 + 29
 3258                 CALL MPI_SEND_INIT (TAUBBL(1,1), NSEALM, MPI_REAL,  &
 3259                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3260                 IH     = IH + 1
 3261                 IT     = IT0 + 30
 3262                 CALL MPI_SEND_INIT (TAUBBL(1,2), NSEALM, MPI_REAL,  &
 3263                          IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
 3264               END IF
 3265             ENDIF
 3266 !
 3267             IF ( IAPROC .EQ. NAPRST ) THEN
 3268               IF (OARST) THEN
 3269                 IF (NAPRST .NE. NAPFLD) CALL W3XDMA ( IMOD, NDSE, NDST, F>
 3270                 CALL W3XETA ( IMOD, NDSE, NDST )
 3271               ENDIF
 3272 !
 3273               DO I0=1, NAPROC
 3274                 IFROM  = I0 - 1
 3275                 IF ( I0 .NE. IAPROC ) THEN
 3276 !
 3277                     IH     = IH + 1
 3278                     IT     = IT0 + 1
 3279                     CALL MPI_RECV_INIT (UST (I0),1,WW3_FIELD_VEC, &
 3280                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3281 !
 3282                     IH     = IH + 1
 3283                     IT     = IT0 + 2
 3284                     CALL MPI_RECV_INIT (USTDIR(I0),1,WW3_FIELD_VEC, &
 3285                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3286 !
 3287                     IH     = IH + 1
 3288                     IT     = IT0 + 3
 3289                     CALL MPI_RECV_INIT (FPIS(I0),1,WW3_FIELD_VEC, &
 3290                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3291                 END IF
 3292 !
 3293                 IF (OARST) THEN
 3294                   IF ( FLOGRR( 1, 2) ) THEN
 3295                     IH     = IH + 1
 3296                     IT     = IT0 + 4
 3297                     CALL MPI_RECV_INIT (CX   (I0),1,WW3_FIELD_VEC, &
 3298                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3299                     IH     = IT0 + 5
 3300                     IT     = IT + 1
 
 
                                                         w3initmd.F90  page  61
 
 
 3301                     CALL MPI_RECV_INIT (CY   (I0),1,WW3_FIELD_VEC, &
 3302                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3303                   END IF
 3304 !
 3305                   IF ( FLOGRR( 1, 9) ) THEN
 3306                     IH     = IH + 1
 3307                     IT     = IT0 + 6
 3308                     CALL MPI_RECV_INIT (ICEF (I0),1,WW3_FIELD_VEC, &
 3309                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3310                   END IF
 3311 !
 3312                   IF ( FLOGRR( 2, 1) ) THEN
 3313                     IH     = IH + 1
 3314                     IT     = IT0 + 7
 3315                     CALL MPI_RECV_INIT (HS   (I0),1,WW3_FIELD_VEC, &
 3316                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3317                   END IF
 3318 !
 3319                   IF ( FLOGRR( 2, 2) ) THEN
 3320                     IH     = IH + 1
 3321                     IT     = IT0 + 8
 3322                     CALL MPI_RECV_INIT (WLM  (I0),1,WW3_FIELD_VEC,  &
 3323                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3324                   END IF
 3325 !
 3326                   IF ( FLOGRR( 2, 4) ) THEN
 3327                     IH     = IH + 1
 3328                     IT     = IT0 + 9
 3329                     CALL MPI_RECV_INIT (T0M1(I0),1,WW3_FIELD_VEC,  &
 3330                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3331                   ENDIF
 3332 !
 3333                   IF ( FLOGRR( 2, 6) ) THEN
 3334                     IH     = IH + 1
 3335                     IT     = IT0 + 10
 3336                     CALL MPI_RECV_INIT (FP0  (I0),1,WW3_FIELD_VEC, &
 3337                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3338                   END IF
 3339 !
 3340                   IF ( FLOGRR( 2, 7) ) THEN
 3341                     IH     = IH + 1
 3342                     IT     = IT0 + 11
 3343                     CALL MPI_RECV_INIT (THM  (I0),1,WW3_FIELD_VEC, &
 3344                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3345                   END IF
 3346 !
 3347                   IF ( FLOGRR( 5, 2) ) THEN
 3348                     IH     = IH + 1
 3349                     IT     = IT0 + 12
 3350                     CALL MPI_RECV_INIT (CHARN(I0),1,WW3_FIELD_VEC, &
 3351                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3352                   ENDIF
 3353 !
 3354                   IF ( FLOGRR( 5, 5) ) THEN
 3355                     IH     = IH + 1
 
 
                                                         w3initmd.F90  page  62
 
 
 3356                     IT     = IT0 + 13
 3357                     CALL MPI_RECV_INIT (TAUWIX(I0),1,WW3_FIELD_VEC,&
 3358                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3359                     IH     = IH + 1
 3360                     IT     = IT0 + 14
 3361                     CALL MPI_RECV_INIT (TAUWIY(I0),1,WW3_FIELD_VEC,&
 3362                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3363                   END IF
 3364 !
 3365                   IF ( FLOGRR( 5,11) ) THEN
 3366                     IH     = IH + 1
 3367                     IT     = IT0 + 15
 3368                     CALL MPI_RECV_INIT (TWS  (I0),1,WW3_FIELD_VEC, &
 3369                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3370                   END IF
 3371 !
 3372                   IF ( FLOGRR( 6, 2) ) THEN
 3373                     IH     = IH + 1
 3374                     IT     = IT0 + 16
 3375                     CALL MPI_RECV_INIT (TAUOX(I0),1,WW3_FIELD_VEC, &
 3376                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3377                     IH     = IH + 1
 3378                     IT     = IT0 + 17
 3379                     CALL MPI_RECV_INIT (TAUOY(I0),1,WW3_FIELD_VEC, &
 3380                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3381                   END IF
 3382 !
 3383                   IF ( FLOGRR( 6, 3) ) THEN
 3384                     IH     = IH + 1
 3385                     IT     = IT0 + 18
 3386                     CALL MPI_RECV_INIT (BHD  (I0),1,WW3_FIELD_VEC, &
 3387                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3388                   END IF
 3389 !
 3390                   IF ( FLOGRR( 6, 4) ) THEN
 3391                     IH     = IH + 1
 3392                     IT     = IT0 + 19
 3393                     CALL MPI_RECV_INIT (PHIOC(I0),1,WW3_FIELD_VEC, &
 3394                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3395                   END IF
 3396 !
 3397                   IF ( FLOGRR( 6, 5) ) THEN
 3398                     IH     = IH + 1
 3399                     IT     = IT0 + 20
 3400                     CALL MPI_RECV_INIT (TUSX (I0),1,WW3_FIELD_VEC, &
 3401                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3402                     IH     = IH + 1
 3403                     IT     = IT0 + 21
 3404                     CALL MPI_RECV_INIT (TUSY (I0),1,WW3_FIELD_VEC, &
 3405                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3406                   END IF
 3407 !
 3408                   IF ( FLOGRR( 6, 6) ) THEN
 3409                     IH     = IH + 1
 3410                     IT     = IT0 + 22
 
 
                                                         w3initmd.F90  page  63
 
 
 3411                     CALL MPI_RECV_INIT (USSX (I0),1,WW3_FIELD_VEC, &
 3412                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3413                     IH     = IH + 1
 3414                     IT     = IT0 + 23
 3415                     CALL MPI_RECV_INIT (USSY (I0),1,WW3_FIELD_VEC, &
 3416                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3417                   END IF
 3418 !
 3419                   IF ( FLOGRR( 6,10) ) THEN
 3420                     IH     = IH + 1
 3421                     IT     = IT0 + 24
 3422                     CALL MPI_RECV_INIT (TAUICE(I0,1),1,WW3_FIELD_VEC,&
 3423                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3424                     IH     = IH + 1
 3425                     IT     = IT0 + 25
 3426                     CALL MPI_RECV_INIT (TAUICE(I0,2),1,WW3_FIELD_VEC,&
 3427                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3428                   END IF
 3429 !
 3430                   IF ( FLOGRR( 7, 2) ) THEN
 3431                     IH     = IH + 1
 3432                     IT     = IT0 + 26
 3433                     CALL MPI_RECV_INIT (UBA  (I0),1,WW3_FIELD_VEC, &
 3434                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3435                     IH     = IH + 1
 3436                     IT     = IT0 + 27
 3437                     CALL MPI_RECV_INIT (UBD  (I0),1,WW3_FIELD_VEC, &
 3438                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3439                   END IF
 3440 !
 3441                   IF ( FLOGRR( 7, 4) ) THEN
 3442                     IH     = IH + 1
 3443                     IT     = IT0 + 28
 3444                     CALL MPI_RECV_INIT (PHIBBL(I0),1,WW3_FIELD_VEC,&
 3445                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3446                   END IF
 3447 !
 3448                   IF ( FLOGRR( 7, 5) ) THEN
 3449                     IH     = IH + 1
 3450                     IT     = IT0 + 29
 3451                     CALL MPI_RECV_INIT (TAUBBL(I0,1),1,WW3_FIELD_VEC,&
 3452                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3453                     IH     = IH + 1
 3454                     IT     = IT0 + 30
 3455                     CALL MPI_RECV_INIT (TAUBBL(I0,2),1,WW3_FIELD_VEC,&
 3456                        IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
 3457                   END IF
 3458                 END IF
 3459 !
 3460               END DO
 3461 !
 3462               IF (OARST) CALL W3SETA ( IMOD, NDSE, NDST )
 3463             END IF
 3464           END IF
 3465 !
 
 
                                                         w3initmd.F90  page  64
 
 
 3466           NRQRS  = IH
 3467           IF (OARST) THEN
 3468             IT0    = IT0 + 30
 3469           ELSE
 3470             IT0    = IT0 + 3
 3471           ENDIF
 3472 !
 3473 ! 2.c Data server mode
 3474 !
 3475           IF ( IOSTYP .GT. 0 ) THEN
 3476 !
 3477               NBLKRS = 10
 3478               RSBLKS = MAX ( 5 , NSEALM/NBLKRS )
 3479               IF ( NBLKRS*RSBLKS .LT. NSEALM ) RSBLKS = RSBLKS + 1
 3480               NBLKRS = 1 + (NSEALM-1)/RSBLKS
 3481 !
 3482               IH     = 0
 3483 !
 3484               IF ((.NOT. LPDLIB).OR.(GTYPE .NE. UNGTYPE)) THEN
 3485               IF ( IAPROC .NE. NAPRST ) THEN
 3486 !
 3487                   ALLOCATE ( OUTPTS(IMOD)%OUT4%IRQRSS(NBLKRS) )
 3488                   IRQRSS => OUTPTS(IMOD)%OUT4%IRQRSS
 3489 !
 3490                   DO IB=1, NBLKRS
 3491                     IH     = IH + 1
 3492                     IT     = IT0 + 3 + IB
 3493                     JSEA0  = 1 + (IB-1)*RSBLKS
 3494                     JSEAN  = MIN ( NSEALM , IB*RSBLKS )
 3495                     NSEAB  = 1 + JSEAN - JSEA0
 3496                     CALL MPI_SEND_INIT (VA(1,JSEA0), NSPEC*NSEAB,&
 3497                          MPI_REAL, IROOT, IT, MPI_COMM_WAVE,    &
 3498                          IRQRSS(IH), IERR )
 3499                     END DO
 3500 !
 3501                 ELSE
 3502 !
 3503                   ALLOCATE                                       &
 3504                  ( OUTPTS(IMOD)%OUT4%IRQRSS(NAPROC*NBLKRS) ,     &
 3505                    OUTPTS(IMOD)%OUT4%VAAUX(NSPEC,2*RSBLKS,NAPROC) )
 3506 !
 3507                   IRQRSS => OUTPTS(IMOD)%OUT4%IRQRSS
 3508                   VAAUX  => OUTPTS(IMOD)%OUT4%VAAUX
 3509                   DO IB=1, NBLKRS
 3510                     IT     = IT0 + 3 + IB
 3511                     JSEA0  = 1 + (IB-1)*RSBLKS
 3512                     JSEAN  = MIN ( NSEALM , IB*RSBLKS )
 3513                     NSEAB  = 1 + JSEAN - JSEA0
 3514                     DO I0=1, NAPROC
 3515                       IF ( I0 .NE. NAPRST ) THEN
 3516                           IH     = IH + 1
 3517                           IFROM  = I0 - 1
 3518                           IBOFF  = MOD(IB-1,2)*RSBLKS
 3519                           CALL MPI_RECV_INIT (VAAUX(1,1+IBOFF,I0),&
 3520                                NSPEC*NSEAB, MPI_REAL, IFROM, IT,  &
 
 
                                                         w3initmd.F90  page  65
 
 
 3521                                MPI_COMM_WAVE, IRQRSS(IH), IERR )
 3522                         END IF
 3523                       END DO
 3524                     END DO
 3525 !
 3526                 END IF
 3527                 END IF
 3528 !
 3529               IT0    = IT0 + NBLKRS
 3530 !
 3531             END IF
 3532 !
 3533         END IF
 3534 !
 3535 ! 3.  Set-up for W3IOBC ( SENDs ) ------------------------------------ /
 3536 !
 3537       NRQBP  = 0
 3538       NRQBP2 = 0
 3539       IH     = 0
 3540       IT     = IT0
 3541       IROOT  = NAPBPT - 1
 3542 !
 3543       IF ( FLOUT(5) ) THEN
 3544           ALLOCATE ( OUTPTS(IMOD)%OUT5%IRQBP1(NBO2(NFBPO)),      &
 3545                      OUTPTS(IMOD)%OUT5%IRQBP2(NBO2(NFBPO)) )
 3546           IRQBP1 => OUTPTS(IMOD)%OUT5%IRQBP1
 3547           IRQBP2 => OUTPTS(IMOD)%OUT5%IRQBP2
 3548 !
 3549 ! 3.a Loops over files and points
 3550 !
 3551           DO J=1, NFBPO
 3552             DO I=NBO2(J-1)+1, NBO2(J)
 3553 !
 3554                IT     = IT + 1
 3555 !
 3556 ! 3.b Residence processor of point
 3557 !
 3558               ISEA   = ISBPO(I)
 3559               CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
 3560 !
 3561 ! 3.c If stored locally, send data
 3562 !
 3563               IF ( IAPROC .EQ. ISPROC ) THEN
 3564                   IH     = IH + 1
 3565                   CALL MPI_SEND_INIT (VA(1,JSEA),NSPEC,MPI_REAL, &
 3566                        IROOT, IT, MPI_COMM_WAVE, IRQBP1(IH), IERR)
 3567                 END IF
 3568 !
 3569               END DO
 3570             END DO
 3571 !
 3572 ! ... End of loops 4.a
 3573 !
 3574           NRQBP  = IH
 3575 !
 
 
                                                         w3initmd.F90  page  66
 
 
 3576 ! 3.d Set-up for W3IOBC ( RECVs ) ------------------------------------ /
 3577 !
 3578           IF ( IAPROC .EQ. NAPBPT ) THEN
 3579 !
 3580               IH     = 0
 3581               IT     = IT0
 3582 !
 3583 ! 3.e Loops over files and points
 3584 !
 3585               DO J=1, NFBPO
 3586                 DO I=NBO2(J-1)+1, NBO2(J)
 3587 !
 3588 ! 3.f Residence processor of point
 3589 !
 3590                   ISEA   = ISBPO(I)
 3591                   CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
 3592 !
 3593 ! 3.g Receive in correct array
 3594 !
 3595                   IH     = IH + 1
 3596                   IT     = IT + 1
 3597                   ITARG  = ISPROC - 1
 3598                   CALL MPI_RECV_INIT (ABPOS(1,IH),NSPEC,MPI_REAL,&
 3599                        ITARG, IT, MPI_COMM_WAVE, IRQBP2(IH), IERR)
 3600 !
 3601                   END DO
 3602                 END DO
 3603 !
 3604               NRQBP2 = IH
 3605 !
 3606 ! ... End of loops 4.e
 3607 !
 3608             END IF
 3609 !
 3610           IT0    = IT0 + NBO2(NFBPO)
 3611 !
 3612         END IF
 3613 !
 3614 ! 4.  Set-up for W3IOTR ---------------------------------------------- /
 3615 !
 3616       IH     = 0
 3617       IROOT  = NAPTRK - 1
 3618 !
 3619       IF ( FLOUT(3) ) THEN
 3620 !
 3621 ! 4.a U*
 3622 !
 3623           IF ( IAPROC .NE. NAPTRK ) THEN
 3624               ALLOCATE ( OUTPTS(IMOD)%OUT3%IRQTR(2) )
 3625               IRQTR  => OUTPTS(IMOD)%OUT3%IRQTR
 3626               IH     = IH + 1
 3627               IT     = IT0 + 1
 3628               CALL MPI_SEND_INIT (UST   (IAPROC),1,WW3_FIELD_VEC,&
 3629                    IROOT, IT, MPI_COMM_WAVE, IRQTR(IH), IERR )
 3630               IH     = IH + 1
 
 
                                                         w3initmd.F90  page  67
 
 
 3631               IT     = IT0 + 2
 3632               CALL MPI_SEND_INIT (USTDIR(IAPROC),1,WW3_FIELD_VEC,&
 3633                    IROOT, IT, MPI_COMM_WAVE, IRQTR(IH), IERR )
 3634             ELSE
 3635               ALLOCATE ( OUTPTS(IMOD)%OUT3%IRQTR(2*NAPROC) )
 3636               IRQTR  => OUTPTS(IMOD)%OUT3%IRQTR
 3637               DO I0=1, NAPROC
 3638                 IFROM  = I0 - 1
 3639                 IF ( I0 .NE. IAPROC ) THEN
 3640                     IH     = IH + 1
 3641                     IT     = IT0 + 1
 3642                     CALL MPI_RECV_INIT(UST   (I0),1,WW3_FIELD_VEC,&
 3643                          IFROM,IT,MPI_COMM_WAVE, IRQTR(IH), IERR)
 3644                     IH     = IH + 1
 3645                     IT     = IT0 + 2
 3646                     CALL MPI_RECV_INIT(USTDIR(I0),1,WW3_FIELD_VEC,&
 3647                          IFROM,IT,MPI_COMM_WAVE, IRQTR(IH), IERR)
 3648                   END IF
 3649                 END DO
 3650             END IF
 3651 !
 3652           NRQTR  = IH
 3653           IT0    = IT0 + 2
 3654 !
 3655         END IF
 3656 !
 3657 ! 5.  Set-up remaining counters -------------------------------------- /
 3658 !
 3659       IT0PRT = IT0
 3660       IT0PNT = IT0PRT + 2*NAPROC
 3661       IT0TRK = IT0PNT + 5000
 3662 !
 3663       RETURN
 3664 !
 3665 !     Formats :
 3666 !
 3667   1010 FORMAT (/' *** ERROR W3MPIO : ARRAY IRQGO TOO SMALL *** '/)
 3668   1011 FORMAT (/' *** ERROR W3MPIO : ARRAY IRQGO2 TOO SMALL *** '/)
 3669 !
 3670 !/
 3671 !/ End of W3MPIO ----------------------------------------------------- /
 3672 !/
 3673       END SUBROUTINE W3MPIO
 3674 !/ ------------------------------------------------------------------- /
 3675       SUBROUTINE W3MPIP ( IMOD )
 3676 !/
 3677 !/                  +-----------------------------------+
 3678 !/                  | WAVEWATCH III           NOAA/NCEP |
 3679 !/                  |           H. L. Tolman            |
 3680 !/                  |                        FORTRAN 90 |
 3681 !/                  | Last update :         30-Oct-2009 |
 3682 !/                  +-----------------------------------+
 3683 !/
 3684 !/    02-Aug-2006 : Origination.                        ( version 3.10 )
 3685 !/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
 
 
                                                         w3initmd.F90  page  68
 
 
 3686 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
 3687 !/                  (W. E. Rogers & T. J. Campbell, NRL)
 3688 !/
 3689 !  1. Purpose :
 3690 !
 3691 !     Prepare MPI persistent communication needed for WAVEWATCH I/O
 3692 !     routines.
 3693 !
 3694 !  2. Method :
 3695 !
 3696 !     Create handles as needed.
 3697 !
 3698 !  3. Parameters :
 3699 !
 3700 !     Parameter list
 3701 !     ----------------------------------------------------------------
 3702 !       IMOD    Int.   I   Model number.
 3703 !     ----------------------------------------------------------------
 3704 !
 3705 !  4. Subroutines used :
 3706 !
 3707 !      Name      Type  Module   Description
 3708 !     ----------------------------------------------------------------
 3709 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 3710 !
 3711 !      MPI_SEND_INIT, MPI_RECV_INIT
 3712 !                Subr. mpif.h   MPI persistent communication calls.
 3713 !     ----------------------------------------------------------------
 3714 !
 3715 !  5. Called by :
 3716 !
 3717 !      Name      Type  Module   Description
 3718 !     ----------------------------------------------------------------
 3719 !      W3INIT    Subr. W3INITMD Wave model initialization routine.
 3720 !     ----------------------------------------------------------------
 3721 !
 3722 !  6. Error messages :
 3723 !
 3724 !  7. Remarks :
 3725 !
 3726 !  8. Structure :
 3727 !
 3728 !     See source code.
 3729 !
 3730 !  9. Switches :
 3731 !
 3732 !       !/MPI   MPI communication calls.
 3733 !
 3734 !       !/S     Enable subroutine tracing.
 3735 !       !/MPIT  Enable test output.
 3736 !
 3737 ! 10. Source code :
 3738 !
 3739 !/ ------------------------------------------------------------------- /
 3740       USE W3SERVMD, ONLY: EXTCDE
 
 
                                                         w3initmd.F90  page  69
 
 
 3741 !/
 3742       USE W3GDATMD, ONLY: NX, NY, NSPEC, MAPFS
 3743       USE W3WDATMD, ONLY: VA
 3744       USE W3ADATMD, ONLY: MPI_COMM_WAVE, SPPNT
 3745       USE W3ODATMD, ONLY: NDST, NDSE, IAPROC, NAPROC, NAPPNT, FLOUT
 3746       USE W3ODATMD, ONLY: OUTPTS, NRQPO, NRQPO2, IRQPO1, IRQPO2, &
 3747                           NOPTS, IPTINT, IT0PNT, IT0TRK, O2IRQI
 3748       USE W3PARALL, ONLY: INIT_GET_JSEA_ISPROC
 3749 !/
 3750       IMPLICIT NONE
 3751 !
 3752       INCLUDE "mpif.h"
 3753 !/
 3754 !/ ------------------------------------------------------------------- /
 3755 !/ Parameter list
 3756 !/
 3757       INTEGER, INTENT(IN)     :: IMOD
 3758 !/
 3759 !/ ------------------------------------------------------------------- /
 3760 !/ Local parameters
 3761 !/
 3762       INTEGER                 :: IH, IROOT, I, J, IT, IT0, JSEA, &
 3763                                  IERR, ITARG, IX(4), IY(4),      &
 3764                                  K, IS(4), IP(4)
 3765         INTEGER                 :: itout
 3766 !/
 3767 !/ ------------------------------------------------------------------- /
 3768 !/
 3769 !
 3770       IF ( O2IRQI ) THEN
 3771           WRITE (NDSE,1001)
 3772           CALL EXTCDE (1)
 3773         END IF
 3774 !
 3775 ! 1.  Set-up for W3IOPE/O ( SENDs ) ---------------------------------- /
 3776 !
 3777       NRQPO  = 0
 3778       NRQPO2 = 0
 3779       IH     = 0
 3780       IT0    = IT0PNT
 3781       IROOT  = NAPPNT - 1
 3782 !
 3783       ALLOCATE ( OUTPTS(IMOD)%OUT2%IRQPO1(4*NOPTS),              &
 3784                  OUTPTS(IMOD)%OUT2%IRQPO2(4*NOPTS) )
 3785       IRQPO1 => OUTPTS(IMOD)%OUT2%IRQPO1
 3786       IRQPO2 => OUTPTS(IMOD)%OUT2%IRQPO2
 3787       O2IRQI = .TRUE.
 3788 !
 3789 ! 1.a Loop over output locations
 3790 !
 3791       DO I=1, NOPTS
 3792         DO K=1,4
 3793           IX(K)=IPTINT(1,K,I)
 3794           IY(K)=IPTINT(2,K,I)
 3795           END DO
 
 
                                                         w3initmd.F90  page  70
 
 
 3796 ! 1.b Loop over corner points
 3797 !
 3798         DO J=1, 4
 3799 !
 3800           IT     = IT0 + (I-1)*4 + J
 3801           IS(J)  = MAPFS (IY(J),IX(J))
 3802           IF ( IS(J) .EQ. 0 ) THEN
 3803               JSEA   = 0
 3804               IP(J)  = NAPPNT
 3805             ELSE
 3806               CALL INIT_GET_JSEA_ISPROC(IS(J), JSEA, IP(J))
 3807             END IF
 3808 !
 3809 ! 1.c Send if point is stored here
 3810 !
 3811           IF ( IP(J) .EQ. IAPROC ) THEN
 3812               IH     = IH + 1
 3813               CALL MPI_SEND_INIT ( VA(1,JSEA), NSPEC, MPI_REAL, &
 3814                    IROOT, IT, MPI_COMM_WAVE, IRQPO1(IH), IERR )
 3815             END IF
 3816 !
 3817 ! ... End of loop 1.b
 3818 !
 3819           END DO
 3820 !
 3821 ! ... End of loop 1.a
 3822 !
 3823         END DO
 3824 !
 3825       NRQPO  = IH
 3826 !
 3827 ! 1.d Set-up for W3IOPE/O ( RECVs ) ---------------------------------- /
 3828 !
 3829       IF ( IAPROC .EQ. NAPPNT ) THEN
 3830 !
 3831           IH     = 0
 3832 !
 3833 ! 2.e Loop over output locations
 3834 !
 3835           DO I=1, NOPTS
 3836             DO K=1,4
 3837               IX(K)=IPTINT(1,K,I)
 3838               IY(K)=IPTINT(2,K,I)
 3839               END DO
 3840 !
 3841             DO J=1, 4
 3842 !
 3843               IT     = IT0 + (I-1)*4 + J
 3844               IS(J)  = MAPFS (IY(J),IX(J))
 3845               IF ( IS(J) .EQ. 0 ) THEN
 3846                   JSEA   = 0
 3847                   IP(J)  = NAPPNT
 3848                 ELSE
 3849                   CALL INIT_GET_JSEA_ISPROC(IS(J), JSEA, IP(J))
 3850                 END IF
 
 
                                                         w3initmd.F90  page  71
 
 
 3851 !
 3852 ! 1.g Receive in correct array
 3853 !
 3854               IH     = IH + 1
 3855               ITARG  = IP(J) - 1
 3856               CALL MPI_RECV_INIT ( SPPNT(1,1,J), NSPEC, MPI_REAL, &
 3857                    ITARG, IT, MPI_COMM_WAVE, IRQPO2(IH), IERR )
 3858 !
 3859 ! ... End of loop 1.f
 3860 !
 3861               END DO
 3862 !
 3863 ! ... End of loop 1.e
 3864 !
 3865             END DO
 3866 !
 3867           NRQPO2 = NOPTS*4
 3868 !
 3869         END IF
 3870 !
 3871       IT0    = IT0 + 8*NOPTS
 3872 !
 3873 ! 1.h Base tag number for track output
 3874 !
 3875       IT0TRK = IT0
 3876 !
 3877       RETURN
 3878 !
 3879 !     Formats :
 3880 !
 3881   1001 FORMAT (/' *** ERROR W3MPIP : ARRAYS ALREADY ALLOCATED *** '/)
 3882 !
 3883 !/
 3884 !/ End of W3MPIP ----------------------------------------------------- /
 3885 !/
 3886       END SUBROUTINE W3MPIP
 3887 !/
 3888 !/ End of module W3INITMD -------------------------------------------- /
 3889 !/
 3890       END MODULE W3INITMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
