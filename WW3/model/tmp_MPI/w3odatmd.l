                                                         w3odatmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3ODATMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         25-Sep-2020 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    13-Dec-2004 : Origination.                        ( version 3.06 )
   13 !/    20-Jul-2005 : Adding output fields.               ( version 3.07 )
   14 !/    29-Sep-2005 : Second storage for input bound. sp. ( version 3.08 )
   15 !/                  Add FILED for the dump of data.
   16 !/    26-Jun-2006 : Add output type 6, wave field sep.  ( version 3.09 )
   17 !/                  Wiring of code only.
   18 !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   19 !/    24-Jul-2006 : Adding unified point output storage.( version 3.10 )
   20 !/    25-Jul-2006 : Originating grid ID for points.     ( version 3.10 )
   21 !/    04-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
   22 !/    30-Oct-2006 : Add pars for partitioning.          ( version 3.10 )
   23 !/    26-Mar-2007 : Add pars for partitioning.          ( version 3.11 )
   24 !/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
   25 !/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
   26 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   27 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
   28 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   29 !/    14-Jul-2010 : Fix VAAUX declaration bug.        ( version 3.14.2 )
   30 !/    27-Jul-2010 : Add NKI, NTHI, XFRI, FR1I, TH1I.  ( version 3.14.3 )
   31 !/    08-Nov-2010 : Implementing unstructured grids     ( version 3.14.4 )
   32 !/                  (A. Roland and F. Ardhuin)
   33 !/    18-Dec-2012 : New 2D field output structure,      ( version 4.11 )
   34 !/                  reducing memory footprint for fields.
   35 !/    19-Dec-2012 : Move NOSWLL to data structure.      ( version 4.11 )
   36 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
   37 !/    27-Aug-2015 : Adding interpolated ICEF (mean ice  ( version 5.10 )
   38 !/                  floe diameter), ICEH (ice thickness)
   39 !/                  and ICE (ice concentration).
   40 !/    01-Mar-2018 : Include UNDEF from constants.ftn to ( version 6.02 )
   41 !/                  avoid circular referencing in w3servmd
   42 !/    05-Jun-2018 : Add SETUP                           ( version 6.04 )
   43 !/    27-Jul-2018 : Added PTMETH and PTFCUT variables   ( version 6.05 )
   44 !/                  for alternative partition methods.
   45 !/                  (C. Bunney, UKMO)
   46 !/    25-Sep-2020 : Flags for coupling restart          ( version 7.10 )
   47 !/    15-Jan-2020 : Added TP based on existing FP       ( version 7.12 )
   48 !/                  internal fields. (C. Bunney, UKMO)
   49 !/
   50 !/    Copyright 2009-2012 National Weather Service (NWS),
   51 !/       National Oceanic and Atmospheric Administration.  All rights
   52 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   53 !/       No unauthorized use without permission.
   54 !/
   55 !  1. Purpose :
 
 
                                                         w3odatmd.F90  page   2
 
 
   56 !
   57 !     Define data structures to set up wave model grids and aliases
   58 !     to use individual grids transparently. Also includes subroutines
   59 !     to manage data structure and pointing to individual models.
   60 !     This module considers the parameters required for model output.
   61 !
   62 !  2. Variables and types :
   63 !
   64 !      Name      Type  Scope    Description
   65 !     ----------------------------------------------------------------
   66 !      NOUTP     Int.  Public   Number of models in array dim.
   67 !      IOUTP     Int.  Public   Selected model for output, init. at -1.
   68 !      IOSTYP    Int.  Public   Output data server type.
   69 !      NOGRP     I.P.  Public   Number of output field groups
   70 !      NGRPP     I.P.  Public   Max numb of parameters per output group
   71 !      NOGE      I.P.  Public   Number of output group elements
   72 !      NOTYPE    I.P.  Public   Number of output types
   73 !      NOEXTR    I.P.  Public   Number of extra (user available)
   74 !                               output fields.
   75 !      DIMP      I.P.  Public   Number of parameters in partition
   76 !                               output group
   77 !      IDOUT     C.A.  Public   ID strings for output fields.
   78 !      FNMPRE    Char  Public   File name preamble.
   79 !      UNDEF     Real  Public   Value for undefined parameters in
   80 !                               gridded output fields.
   81 !      UNIPTS    Log.  Public   Flag for unified point output (output
   82 !                               to single file).
   83 !      UPPROC    Log.  Public   FLag for dedicated point output proc.
   84 !      OUTPUT    TYPE  Public   Data structure defining output.
   85 !      OUTPTS    GRID  Public   Array of output for models.
   86 !     ----------------------------------------------------------------
   87 !
   88 !     Elements of OUTPUT are aliased to pointers with the same
   89 !     name. These pointers are defined as :
   90 !
   91 !      Name      Type  Scope    Description
   92 !     ----------------------------------------------------------------
   93 !      NDSO      Int.  Public   General output unit number ("log
   94 !                               file").
   95 !      NDSE      Int.  Public   Error output unit number.
   96 !      NDST      Int.  Public   Test output unit number.
   97 !      SCREEN    Int.  Public   Unit for 'direct' output.
   98 !      NTPROC    Int.  Public   Number of processors.
   99 !      NAPROC    Int.  Public   Number of processors for computation.
  100 !      IAPROC    Int.  Public   Actual processor number (base 1),
  101 !      NAPLOG    Int.  Public   Proc. dealing with log output.
  102 !      NAPOUT    Int.  Public   Proc. dealing with standard output.
  103 !      NAPERR    Int.  Public   Proc. dealing with error output.
  104 !      NAPFLD    Int.  Public   Proc. dealing with raw field output.
  105 !      NAPPNT    Int.  Public   Proc. dealing with raw point output.
  106 !      NAPTRK    Int.  Public   Proc. dealing with track output.
  107 !      NAPRST    Int.  Public   Proc. dealing with restart output.
  108 !      NAPBPT    Int.  Public   Proc. dealing with boundary output.
  109 !      NAPPRT    Int.  Public   Proc. dealing with partition output.
  110 !      NOSWLL    I.P.  Public   Number of swell fields from part.
 
 
                                                         w3odatmd.F90  page   3
 
 
  111 !                               to be used in field output.
  112 !      TOFRST    I.A.  Public   Times for first output.
  113 !      TONEXT    I.A.  Public   Times for next output.
  114 !      TOLAST    I.A.  Public   Times for last output.
  115 !      TBPI0     I.A   Public   Time of first set of input boundary
  116 !                               spectra.
  117 !      TBPIN     I.A   Public   Id. second set.
  118 !      NDS       I.A.  Public   Data set numbers (see W3INIT).
  119 !      DTOUT     R.A.  Public   Output intervals.
  120 !      FLOUT     L.A.  Public   Output flags.
  121 !      OUT1      TYPE  Public   Data structure of type OTYPE1 with
  122 !                               suppl. data for output type 1.
  123 !      OUT2      TYPE  Public   Data structure of type OTYPE2 with
  124 !                               suppl. data for output type 2.
  125 !      OUT3      TYPE  Public   Data structure of type OTYPE3 with
  126 !                               suppl. data for output type 3.
  127 !      OUT4      TYPE  Public   Data structure of type OTYPE4 with
  128 !                               suppl. data for output type 4.
  129 !      OUT5      TYPE  Public   Data structure of type OTYPE5 with
  130 !                               suppl. data for output type 5.
  131 !      OUT6      TYPE  Public   Data structure of type OTYPE6 with
  132 !                               suppl. data for output type 6.
  133 !      OFILES   I.A.  Public   Output in one or several files.
  134 !     ----------------------------------------------------------------
  135 !
  136 !     Elements of OUT1 are aliased to pointers with the same
  137 !     name. These pointers are defined as :
  138 !
  139 !      Name      Type  Scope    Description
  140 !     ----------------------------------------------------------------
  141 !      IPASS1    Int.  Public   Pass counter for file management,
  142 !                               renamed to IPASS in routine.
  143 !      WRITE1    Int.  Public   Write flag for file management,
  144 !                               renamed to WRITE in routine.
  145 !      NRQGO(2)  Int.  Public   Number of MPI handles W3IOGO.
  146 !      IRQGO     I.A.  Public   Array with MPI handles W3IOGO.
  147 !      FLOGRD    L.A.  Public   FLags for output fields.
  148 !      FLOGR2    L.A.  Public   FLags for coupling fields.
  149 !      FLOGRR    L.A.  Public   FLags for optional coupling restart (2D).
  150 !      FLOGD     L.A.  Public   Flags for output groups
  151 !      FLOG2     L.A.  Public   Flags for coupling groups
  152 !      FLOGR     L.A.  Public   FLags for optional coupling restart (1D).
  153 !     ----------------------------------------------------------------
  154 !
  155 !     Elements of OUT2 are aliased to pointers with the same
  156 !     name. These pointers are defined as :
  157 !
  158 !      Name      Type  Scope    Description
  159 !     ----------------------------------------------------------------
  160 !      IPASS2    Int.  Public   Pass counter for file management,
  161 !                               renamed to IPASS in routine.
  162 !      NOPTS     Int.  Public   Number of output points.
  163 !      NRQPO(2)  Int.  Public   Number of MPI handles IRQPOn. (!/MPI)
  164 !      IPTINT    I.A.  Public   (I,J)-indices of enclosing cell corner po>
  165 !      IL        I.A.  Public   Number of land points in interpola-
 
 
                                                         w3odatmd.F90  page   4
 
 
  166 !                               tion box for output point.
  167 !      IW        I.A.  Public   Id. water.
  168 !      II        I.A.  Public   Id. ice.
  169 !      IRQPO1/2  I.A.  Public   Array with MPI handles.       (!/MPI)
  170 !      PTLOC     R.A.  Public   Name of output locations.
  171 !      PTIFAC    R.A.  Public   Interpolation weights.
  172 !      DPO       R.A.  Public   Interpolated depths.
  173 !      WAO       R.A.  Public   Interpolated wind speeds.
  174 !      WDO       R.A.  Public   Interpolated wind directions.
  175 !      ASO       R.A.  Public   Interpolated air-sea temp. diff.
  176 !      CAO       R.A.  Public   Interpolated current speeds.
  177 !      CDO       R.A.  Public   Interpolated current directions.
  178 !      SPCO      R.A.  Public   Output spectra.
  179 !      ICEO      R.A.  Public   Interpolated ice concentration.
  180 !      ICEHO     R.A.  Public   Interpolated ice thickness.
  181 !      ICEFO     R.A.  Public   Interpolated ice floe.
  182 !      PTNME     C.A.  Public   Output locations.
  183 !      GRDID     C.A.  Public   Originating grid ID.
  184 !      O2INIT    Log.  Public   Flag for array initialization.
  185 !      O2IRQI    Log.  Public   Flag for array initialization.
  186 !     ----------------------------------------------------------------
  187 !
  188 !     Elements of OUT3 are aliased to pointers with the same
  189 !     name. These pointers are defined as :
  190 !
  191 !      Name      Type  Scope    Description
  192 !     ----------------------------------------------------------------
  193 !      IPASS3    Int.  Public   Pass counter for file management,
  194 !                               renamed to IPASS in routine.
  195 !      IT0PNT    Int.  Public   Base tag number of MPI communication.
  196 !      IT0TRK    Int.  Public   Base tag number of MPI communication.
  197 !      IT0PRT    Int.  Public   Base tag number of MPI communication.
  198 !      NRQTR     Int.  Public   Number of handles in IRQTR.
  199 !      IRQTR     I.A.  Public   Array with MPI handles.
  200 !      O3INIT    Log.  Public   Flag for array initialization.
  201 !      STOP      Log.  Public   Flag for end of output.
  202 !      MASKn     L.A.  Public   Mask arrays for internal use.
  203 !     ----------------------------------------------------------------
  204 !
  205 !     Elements of OUT4 are aliased to pointers with the same
  206 !     name. These pointers are defined as :
  207 !
  208 !      Name      Type  Scope    Description
  209 !     ----------------------------------------------------------------
  210 !      IFILE4    Int.  Public   File number for output files.
  211 !      NBLKRS    Int.  Public   Number of blocks in communication of
  212 !                               spectra.
  213 !      RSBLKS    Int.  Public   Corresponding block size.
  214 !      NRQSR     Int.  Public   Number of MPI handles.
  215 !      IRQRS     I.A.  Public   Array with MPI handles.
  216 !      IRQRSS    I.A.  Public   Array with MPI handles.
  217 !      VAAUX     R.A.  Public   Aux. spectra storage.
  218 !     ----------------------------------------------------------------
  219 !
  220 !     Elements of OUT5 are aliased to pointers with the same
 
 
                                                         w3odatmd.F90  page   5
 
 
  221 !     name. These pointers are defined as :
  222 !
  223 !      Name      Type  Scope    Description
  224 !     ----------------------------------------------------------------
  225 !      NBI(2)    Int.  Public   Number of input bound. points.
  226 !      NFBPO     Int.  Public   Number of files for output bound. data.
  227 !      NRQBP(2)  Int.  Public   Number of MPI handles.
  228 !      NKI,NTHI  Int.  Public   Size of input spectra
  229 !      NBO(2)    I.A.  Public   Number of output bound. pts. per file.
  230 !      NDSL      I.A.  Public   Array with unit numbers.
  231 !      IPBPI     I.A.  Public   Interpolation data input b.p.
  232 !      ISBPI     I.A.  Public   Sea point counters for input b.p.
  233 !      IRQBP1/2  I.A.  Public   Array with MPI handles.
  234 !      XFRI, FR1I, TH1I
  235 !                Real  Public   Definition of input spectra.
  236 !      X/YBPI    R.A.  Public   Location of input boundary points.
  237 !      RDBPI     R.A.  Public   Interpolation factors input b.p.
  238 !      ABPI0/N   R.A.  Public   Storage of spectra from which to
  239 !                               interpolate b.d.
  240 !      BBPI0/N   R.A.  Public   idem, secondary storage.
  241 !      ABPOS     R.A.  Public   Temporarily storage for output b.d.
  242 !      IPBPO, ISBPO, X/YBPO, RDBPO
  243 !                Misc. Public   Id. for output b.p.
  244 !      FLBPI     Log.  Public   Flag for input of boundary data.
  245 !      FLBPO     Log.  Public   Flag for output of boundary data.
  246 !      FILER/W/D Log.  Public   Read/write flags for file management.
  247 !      SPCONV    Log.  Public   Flag for change of spectral res.
  248 !      O5INIn    Log.  Public   Flag for array initializations.
  249 !     ----------------------------------------------------------------
  250 !
  251 !     Elements of OUT6 are aliased to pointers with the same
  252 !     name. These pointers are defined as :
  253 !
  254 !      Name      Type  Scope    Description
  255 !     ----------------------------------------------------------------
  256 !      IPASS6    Int.  Public   Pass counter for file management,
  257 !                               renamed to IPASS in routine.
  258 !      IHMAX     Int.  Public   Number of discrete spectral levels.
  259 !      IX0/N/S   Int.  Public   First-last-step IX counters.
  260 !      IY0/N/S   Int.  Public   Idem IY counters.
  261 !      HSPMIN    Real  Public   Minimum significant height per part.
  262 !      WSMULT    Real  Public   Multiplier for wind sea boundary.
  263 !      WSCUT     Real  Public   Cut-off wind factor for wind seas.
  264 !      ICPRT     I.A.  Public   Counters for partitions.
  265 !      DTPRT     R.A.  Public   Data from partitions.
  266 !      FLCOMB    Log.  Public   Flag for combining wind seas.
  267 !      FLFORM    Log.  Public   Flag for (un)formatted output
  268 !      O6INIT    Log.  Public   Flag for array initializations.
  269 !     ----------------------------------------------------------------
  270 !
  271 !  3. Subroutines and functions :
  272 !
  273 !      Name      Type  Scope    Description
  274 !     ----------------------------------------------------------------
  275 !      W3NOUT    Subr. Public   Set number of grids.
 
 
                                                         w3odatmd.F90  page   6
 
 
  276 !      W3DMO2    Subr. Public   Allocate arrays output type 2.
  277 !      W3DMO3    Subr. Public   Allocate arrays output type 3.
  278 !      W3DMO5    Subr. Public   Allocate arrays output type 5.
  279 !      W3SETO    Subr. Public   Point to selected grid / model.
  280 !     ----------------------------------------------------------------
  281 !
  282 !  4. Subroutines and functions used :
  283 !
  284 !      Name      Type  Module   Description
  285 !     ----------------------------------------------------------------
  286 !      W3SETG    Subr. W3GDATMD Point to proper model grid.
  287 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  288 !      EXTCDE    Subr. W3SERVMD Abort program with exit code.
  289 !     ----------------------------------------------------------------
  290 !
  291 !  5. Remarks :
  292 !
  293 !     - The number of grids is taken from W3GDATMD, and needs to be
  294 !       set first with W3DIMG.
  295 !
  296 !  6. Switches :
  297 !
  298 !     !/MPI    MPI specific calls.
  299 !     !/S      Enable subroutine tracing.
  300 !     !/T      Enable test output
  301 !
  302 !  7. Source code :
  303 !
  304 !/ ------------------------------------------------------------------- /
  305       USE CONSTANTS, ONLY : UNDEF
  306       PUBLIC
  307 !/
  308 !/ Module private variable for checking error returns
  309 !/
  310       INTEGER, PRIVATE        :: ISTAT
  311 !/
  312 !/ Conventional declarations
  313 !/
  314       INTEGER                 :: NOUTP = -1, IOUTP = -1, IOSTYP = 1
  315 !
  316       INTEGER, PARAMETER      :: NOGRP = 10
  317       INTEGER, PARAMETER      :: NGRPP = 20
  318       INTEGER, PARAMETER      :: DIMP = 15
  319       INTEGER                 :: NOGE(NOGRP)
  320       INTEGER                 :: NOTYPE
  321       INTEGER, PARAMETER      :: NOEXTR=  2
  322       CHARACTER(LEN=20)       :: IDOUT(NOGRP,NGRPP)
  323       CHARACTER(LEN=80)       :: FNMPRE = './'
  324       !Moved UNDEF to constants and included above
  325       !REAL                    :: UNDEF = -999.9
  326       LOGICAL                 :: UNIPTS = .FALSE., UPPROC = .FALSE.
  327 !/
  328 !/ Set NOGE and IDOUT identifiers in W3NOUT
  329 !/
  330 !/ Data structures
 
 
                                                         w3odatmd.F90  page   7
 
 
  331 !/
  332       TYPE OTYPE1
  333         INTEGER               :: IPASS1
  334         INTEGER               :: NRQGO, NRQGO2
  335         INTEGER, POINTER      :: IRQGO(:), IRQGO2(:)
  336         LOGICAL               :: FLOGRD(NOGRP,NGRPP), FLOGD(NOGRP),   &
  337                                  FLOGR2(NOGRP,NGRPP), FLOG2(NOGRP),   &
  338                                  FLOGRR(NOGRP,NGRPP), FLOGR(NOGRP),   &
  339                                  WRITE1
  340       END TYPE OTYPE1
  341 !/
  342       TYPE OTYPE2
  343         INTEGER               :: IPASS2, NOPTS
  344         INTEGER               :: NRQPO, NRQPO2
  345         INTEGER, POINTER      :: IPTINT(:,:,:), IL(:), IW(:), II(:)
  346         INTEGER, POINTER      :: IRQPO1(:), IRQPO2(:)
  347         REAL, POINTER         :: PTLOC(:,:), PTIFAC(:,:),             &
  348                                  DPO(:), WAO(:), WDO(:), ASO(:),      &
  349                                  CAO(:), CDO(:), ICEO(:), ICEHO(:),   &
  350                                  ICEFO(:), SPCO(:,:)
  351         REAL, POINTER         :: ZET_SETO(:)  ! For the wave setup.
  352                                                                    
  353         CHARACTER(LEN=40), POINTER :: PTNME(:)
  354         CHARACTER(LEN=13), POINTER :: GRDID(:)
  355         LOGICAL               :: O2INIT
  356         LOGICAL               :: O2IRQI
  357       END TYPE OTYPE2
  358 !/
  359       TYPE OTYPE3
  360         INTEGER               :: IPASS3
  361         INTEGER               :: IT0PNT, IT0TRK, IT0PRT, NRQTR
  362         INTEGER, POINTER      :: IRQTR(:)
  363         LOGICAL               :: O3INIT, STOP
  364         LOGICAL, POINTER      :: MASK1(:,:), MASK2(:,:)
  365         CHARACTER(LEN=32), POINTER  :: TRCKID(:,:)
  366       END TYPE OTYPE3
  367 !/
  368       TYPE OTYPE4
  369         INTEGER               :: IFILE4
  370         INTEGER               :: NRQRS, NBLKRS, RSBLKS
  371         INTEGER, POINTER      :: IRQRS(:), IRQRSS(:)
  372         REAL, POINTER         :: VAAUX(:,:,:)
  373       END TYPE OTYPE4
  374 !/
  375       TYPE OTYPE5
  376         INTEGER               :: NBI, NBI2, NFBPO, NBO(0:9),          &
  377                                  NBO2(0:9), NDSL(9), NKI, NTHI
  378         INTEGER               :: NRQBP = 0, NRQBP2 = 0
  379         INTEGER, POINTER      :: IPBPI(:,:), ISBPI(:),                &
  380                                  IPBPO(:,:), ISBPO(:)
  381         INTEGER, POINTER      :: IRQBP1(:), IRQBP2(:)
  382         REAL                  :: XFRI, FR1I, TH1I
  383         REAL, POINTER         :: XBPI(:), YBPI(:), RDBPI(:,:),        &
  384                                  XBPO(:), YBPO(:), RDBPO(:,:),        &
  385                                  ABPI0(:,:), ABPIN(:,:), ABPOS(:,:),  &
 
 
                                                         w3odatmd.F90  page   8
 
 
  386                                  BBPI0(:,:), BBPIN(:,:)
  387         LOGICAL               :: O5INI1, O5INI2, O5INI3, O5INI4
  388         LOGICAL               :: FLBPI, FLBPO, FILER, FILEW, FILED,   &
  389                                  SPCONV
  390       END TYPE OTYPE5
  391 !/
  392       TYPE OTYPE6
  393         INTEGER               :: IPASS6, IHMAX, IX0, IXN, IXS,        &
  394                                  IY0, IYN, IYS
  395         INTEGER, POINTER      :: ICPRT(:,:)
  396         REAL                  :: HSPMIN, WSMULT, WSCUT
  397         REAL, POINTER         :: DTPRT(:,:)
  398         LOGICAL               :: FLFORM, FLCOMB, O6INIT
  399         INTEGER               :: PTMETH   ! C. Bunney; Partitioning method
  400         REAL                  :: PTFCUT   ! C. Bunney; Part. 5 freq cut
  401       END TYPE OTYPE6
  402 !/
  403       TYPE OUTPUT
  404         INTEGER               :: NDSO, NDSE, NDST, SCREEN
  405         INTEGER               :: NTPROC, NAPROC, IAPROC, NAPLOG,      &
  406                                  NAPOUT, NAPERR, NAPFLD, NAPPNT,      &
  407                                  NAPTRK, NAPRST, NAPBPT, NAPPRT
  408         INTEGER               :: NOSWLL
  409         INTEGER               :: TOFRST(2), TONEXT(2,8), TOLAST(2,8), &
  410                                  TBPI0(2), TBPIN(2), NDS(13), OFILES(7)
  411         REAL                  :: DTOUT(8)
  412         LOGICAL               :: FLOUT(8)
  413         TYPE(OTYPE1)          :: OUT1
  414         TYPE(OTYPE2)          :: OUT2
  415         TYPE(OTYPE3)          :: OUT3
  416         TYPE(OTYPE4)          :: OUT4
  417         TYPE(OTYPE5)          :: OUT5
  418         TYPE(OTYPE6)          :: OUT6
  419       END TYPE OUTPUT
  420 !/
  421 !/ Data storage
  422 !/
  423       TYPE(OUTPUT), TARGET, ALLOCATABLE :: OUTPTS(:)
  424 !/
  425 !/ Data aliasses for structure OUTPUT
  426 !/
  427       INTEGER, POINTER        :: NDSO, NDSE, NDST, SCREEN
  428       INTEGER, POINTER        :: NTPROC, NAPROC, IAPROC, NAPLOG,      &
  429                                  NAPOUT, NAPERR, NAPFLD, NAPPNT,      &
  430                                  NAPTRK, NAPRST, NAPBPT, NAPPRT
  431       INTEGER, POINTER        :: NOSWLL
  432       INTEGER, POINTER        :: TOFRST(:), TONEXT(:,:), TOLAST(:,:), &
  433                                  TBPI0(:), TBPIN(:), NDS(:)
  434       INTEGER, POINTER        :: OFILES(:)
  435       REAL, POINTER           :: DTOUT(:)
  436       LOGICAL, POINTER        :: FLOUT(:)
  437 !/
  438 !/ Data aliasses for substructures for output types
  439 !/ Type 1 ...
  440 !/
 
 
                                                         w3odatmd.F90  page   9
 
 
  441       INTEGER, POINTER        :: IPASS1
  442       INTEGER, POINTER        :: NRQGO, NRQGO2
  443       INTEGER, POINTER        :: IRQGO(:), IRQGO2(:)
  444       LOGICAL, POINTER        :: FLOGRD(:,:), FLOGR2(:,:),            &
  445                                  FLOGRR(:,:),FLOGD(:), FLOG2(:),      &
  446                                  FLOGR(:), WRITE1
  447 !/
  448 !/ Type 2 ...
  449 !/
  450       INTEGER, POINTER        :: IPASS2, NOPTS
  451       INTEGER, POINTER        :: NRQPO, NRQPO2
  452       INTEGER, POINTER        :: IPTINT(:,:,:), IL(:), IW(:), II(:)
  453       INTEGER, POINTER        :: IRQPO1(:), IRQPO2(:)
  454       REAL, POINTER           :: PTLOC(:,:), PTIFAC(:,:),             &
  455                                  DPO(:), WAO(:), WDO(:), ASO(:),      &
  456                                  CAO(:), CDO(:), ICEO(:), ICEHO(:),   &
  457                                  ICEFO(:), SPCO(:,:)
  458       REAL, POINTER           :: ZET_SETO(:)
  459 !
  460       CHARACTER(LEN=40), POINTER :: PTNME(:)
  461       CHARACTER(LEN=13), POINTER :: GRDID(:)
  462       LOGICAL, POINTER        :: O2INIT
  463         LOGICAL, POINTER      :: O2IRQI
  464 !/
  465 !/ Type 3 ...
  466 !/
  467       INTEGER, POINTER        :: IPASS3
  468       INTEGER, POINTER        :: IT0PNT, IT0TRK, IT0PRT, NRQTR
  469       INTEGER, POINTER        :: IRQTR(:)
  470       LOGICAL, POINTER        :: O3INIT, STOP
  471       LOGICAL, POINTER        :: MASK1(:,:), MASK2(:,:)
  472       CHARACTER(LEN=32), POINTER   :: TRCKID(:,:)
  473 !/
  474 !/ Type 4 ...
  475 !/
  476       INTEGER, POINTER        :: IFILE4
  477       INTEGER, POINTER        :: NRQRS, NBLKRS, RSBLKS
  478       INTEGER, POINTER        :: IRQRS(:), IRQRSS(:)
  479       REAL, POINTER           :: VAAUX(:,:,:)
  480 !/
  481 !/ Type 5 ...
  482 !/
  483       INTEGER, POINTER        :: NBI, NBI2, NFBPO, NKI, NTHI
  484       INTEGER, POINTER        :: NBO(:), NBO2(:), NDSL(:)
  485       INTEGER, POINTER        :: NRQBP, NRQBP2
  486       INTEGER, POINTER        :: IPBPI(:,:), ISBPI(:),                &
  487                                  IPBPO(:,:), ISBPO(:)
  488       INTEGER, POINTER        :: IRQBP1(:), IRQBP2(:)
  489       REAL, POINTER           :: XFRI, FR1I, TH1I
  490       REAL, POINTER           :: XBPI(:), YBPI(:), RDBPI(:,:),        &
  491                                  XBPO(:), YBPO(:), RDBPO(:,:),        &
  492                                  ABPI0(:,:), ABPIN(:,:), ABPOS(:,:),  &
  493                                  BBPI0(:,:), BBPIN(:,:)
  494       LOGICAL, POINTER        :: O5INI1, O5INI2, O5INI3, O5INI4
  495       LOGICAL, POINTER        :: FLBPI, FLBPO, FILER, FILEW, FILED,   &
 
 
                                                         w3odatmd.F90  page  10
 
 
  496                                  SPCONV
  497 !/
  498 !/ Type 6 ...
  499 !/
  500       INTEGER, POINTER        :: IPASS6, IHMAX, IX0, IXN, IXS,        &
  501                                  IY0, IYN, IYS, ICPRT(:,:)
  502       REAL, POINTER           :: HSPMIN, WSMULT, WSCUT, DTPRT(:,:)
  503       LOGICAL, POINTER        :: FLFORM, FLCOMB, O6INIT
  504       INTEGER, POINTER        :: PTMETH   ! C. Bunney; Partitioning method
  505       REAL, POINTER           :: PTFCUT   ! C. Bunney; Part. 5 freq cut
  506 !/
  507       CONTAINS
  508 !/ ------------------------------------------------------------------- /
  509       SUBROUTINE W3NOUT ( NDSERR, NDSTST )
  510 !/
  511 !/                  +-----------------------------------+
  512 !/                  | WAVEWATCH III           NOAA/NCEP |
  513 !/                  |           H. L. Tolman            |
  514 !/                  |                        FORTRAN 90 |
  515 !/                  | Last update :         10-Dec-2014 |
  516 !/                  +-----------------------------------+
  517 !/
  518 !/    13-Dec-2004 : Origination.                        ( version 3.06 )
  519 !/    27-Jun-2006 : Adding file name preamble           ( version 3.09 )
  520 !/    24-Jul-2006 : Adding unified point output storage.( version 3.10 )
  521 !/    04-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
  522 !/    30-Oct-2006 : Add pars for partitioning.          ( version 3.10 )
  523 !/    26-Mar-2007 : Add pars for partitioning.          ( version 3.11 )
  524 !/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
  525 !/    18-Dec-2012 : Moving IDOUT initialization here.   ( version 4.11 )
  526 !/    19-Dec-2012 : Move NOSWLL to data structure.      ( version 4.11 )
  527 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
  528 !/
  529 !  1. Purpose :
  530 !
  531 !     Set up the number of grids to be used.
  532 !
  533 !  2. Method :
  534 !
  535 !     Use data stored in NGRIDS in W3GDATMD.
  536 !
  537 !  3. Parameters :
  538 !
  539 !     Parameter list
  540 !     ----------------------------------------------------------------
  541 !       NDSERR  Int.   I   Error output unit number.
  542 !       NDSTST  Int.   I   Test output unit number.
  543 !     ----------------------------------------------------------------
  544 !
  545 !  4. Subroutines used :
  546 !
  547 !     See module documentation below.
  548 !
  549 !  5. Called by :
  550 !
 
 
                                                         w3odatmd.F90  page  11
 
 
  551 !     Any main program that uses this grid structure.
  552 !
  553 !  6. Error messages :
  554 !
  555 !     - Error checks on previous setting of variable NGRIDS.
  556 !
  557 !  7. Remarks :
  558 !
  559 !  8. Structure :
  560 !
  561 !  9. Switches :
  562 !
  563 !     !/S    Enable subroutine tracing.
  564 !     !/T    Enable test output
  565 !
  566 ! 10. Source code :
  567 !
  568 !/ ------------------------------------------------------------------- /
  569       USE W3GDATMD, ONLY: NGRIDS, NAUXGR
  570       USE W3SERVMD, ONLY: EXTCDE
  571 !
  572       IMPLICIT NONE
  573 !/
  574 !/ ------------------------------------------------------------------- /
  575 !/ Parameter list
  576 !/
  577       INTEGER, INTENT(IN)     :: NDSERR, NDSTST
  578 !/
  579 !/ ------------------------------------------------------------------- /
  580 !/ Local parameters
  581 !/
  582       INTEGER                 :: I, NLOW, J
  583       CHARACTER(LEN=20)       :: STRING
  584 !/
  585 !
  586 ! -------------------------------------------------------------------- /
  587 ! 1.  Test input and module status
  588 !
  589       IF ( NGRIDS .EQ. -1 ) THEN
  590           WRITE (NDSERR,1001) NGRIDS
  591           CALL EXTCDE (1)
  592         END IF
  593 !
  594 ! -------------------------------------------------------------------- /
  595 ! 2.  Set variable and allocate arrays
  596 !
  597       NLOW   = MIN ( 0 , -NAUXGR )
  598       ALLOCATE ( OUTPTS(NLOW:NGRIDS), STAT=ISTAT )
  599       CHECK_ALLOC_STATUS ( ISTAT )
  600       NOUTP  = NGRIDS
  601 !
  602 ! -------------------------------------------------------------------- /
  603 ! 3.  Initialize parameters
  604 !
  605       DO I=NLOW, NGRIDS
 
 
                                                         w3odatmd.F90  page  12
 
 
  606 !
  607         OUTPTS(I)%NDSO   = 6
  608         OUTPTS(I)%NDSE   = 6
  609         OUTPTS(I)%NDST   = 6
  610         OUTPTS(I)%SCREEN = 6
  611 !
  612         OUTPTS(I)%NTPROC = 1
  613         OUTPTS(I)%NAPROC = 1
  614         OUTPTS(I)%IAPROC = 1
  615         OUTPTS(I)%NAPLOG = 1
  616         OUTPTS(I)%NAPOUT = 1
  617         OUTPTS(I)%NAPERR = 1
  618         OUTPTS(I)%NAPFLD = 1
  619         OUTPTS(I)%NAPPNT = 1
  620         OUTPTS(I)%NAPTRK = 1
  621         OUTPTS(I)%NAPRST = 1
  622         OUTPTS(I)%NAPBPT = 1
  623         OUTPTS(I)%NAPPRT = 1
  624 !
  625         OUTPTS(I)%NOSWLL = -1
  626 !
  627         OUTPTS(I)%TBPI0 = (-1,0)
  628         OUTPTS(I)%TBPIN = (-1,0)
  629 !
  630         OUTPTS(I)%OUT1%IPASS1 = 0
  631         OUTPTS(I)%OUT1%NRQGO  = 0
  632         OUTPTS(I)%OUT1%NRQGO2 = 0
  633 !
  634         OUTPTS(I)%OUT2%IPASS2 = 0
  635         OUTPTS(I)%OUT2%NOPTS  = 0
  636         OUTPTS(I)%OUT2%O2INIT = .FALSE.
  637         OUTPTS(I)%OUT2%O2IRQI = .FALSE.
  638 !
  639         OUTPTS(I)%OUT3%IPASS3 = 0
  640         OUTPTS(I)%OUT3%O3INIT = .FALSE.
  641         OUTPTS(I)%OUT3%STOP   = .FALSE.
  642         OUTPTS(I)%OUT3%NRQTR  = 0
  643 !
  644         OUTPTS(I)%OUT4%IFILE4 = 0
  645         OUTPTS(I)%OUT4%NRQRS  = 0
  646 !
  647         OUTPTS(I)%OUT5%O5INI1 = .FALSE.
  648         OUTPTS(I)%OUT5%O5INI2 = .FALSE.
  649         OUTPTS(I)%OUT5%O5INI3 = .FALSE.
  650         OUTPTS(I)%OUT5%O5INI4 = .FALSE.
  651         OUTPTS(I)%OUT5%FILER  = .TRUE.
  652         OUTPTS(I)%OUT5%FILEW  = .TRUE.
  653         OUTPTS(I)%OUT5%FILED  = .TRUE.
  654 !
  655         OUTPTS(I)%OUT6%IPASS6 = 0
  656         OUTPTS(I)%OUT6%O6INIT = .FALSE.
  657 !
  658         END DO
  659 !
  660 !     Set IDOUT
 
 
                                                         w3odatmd.F90  page  13
 
 
  661 !       Commented outlines represent reserved slots.
  662 !
  663       DO I=1, NOGRP
  664         DO J=1, NGRPP
  665           IDOUT(I,J) = 'Undefined / Not Used'
  666           END DO
  667         END DO
  668 !
  669 ! 1) Forcing fields
  670 !
  671       NOGE(1) = 7
  672 !
  673       IDOUT( 1, 1)  = 'Water depth         '
  674       IDOUT( 1, 2)  = 'Current vel.        '
  675       IDOUT( 1, 3)  = 'Wind speed          '
  676       IDOUT( 1, 4)  = 'Air-sea temp. dif.  '
  677       IDOUT( 1, 5)  = 'Water level         '
  678       IDOUT( 1, 6)  = 'Ice concentration   '
  679       IDOUT( 1, 7)  = 'Iceberg damp coeffic'
  680 !
  681 ! 2) Standard mean wave parameters
  682 !
  683       NOGE(2) = 18
  684 !
  685       IDOUT( 2, 1)  = 'Wave height         '
  686       IDOUT( 2, 2)  = 'Mean wave length    '
  687       IDOUT( 2, 3)  = 'Mean wave period(+2)'
  688       IDOUT( 2, 4)  = 'Mean wave period(-1)'
  689       IDOUT( 2, 5)  = 'Mean wave period(+1)'
  690       IDOUT( 2, 6)  = 'Peak frequency      '
  691       IDOUT( 2, 7)  = 'Mean wave dir. a1b1 '
  692       IDOUT( 2, 8)  = 'Mean dir. spr. a1b1 '
  693       IDOUT( 2, 9)  = 'Peak direction      '
  694       IDOUT( 2, 10)  = 'Infragravity height'
  695       IDOUT( 2, 11)  = 'Space-Time Max E   '
  696       IDOUT( 2, 12)  = 'Space-Time Max Std '
  697       IDOUT( 2, 13)  = 'Space-Time Hmax    '
  698       IDOUT( 2, 14)  = 'Spc-Time Hmax^crest'
  699       IDOUT( 2, 15)  = 'STD Space-Time Hmax'
  700       IDOUT( 2, 16)  = 'STD ST Hmax^crest  '
  701       IDOUT( 2, 17)  = 'Dominant wave bT   '
  702       IDOUT( 2, 18)  = 'Peak prd. (from fp)'
  703 !      IDOUT( 2,10)  = 'Mean wave dir. a2b2'
  704 !      IDOUT( 2,11)  = 'Mean dir. spr. a2b2'
  705 !      IDOUT( 2,12)  = 'Windsea height(Sin)'
  706 !      IDOUT( 2,13)  = 'Windsea peak f(Sin)'
  707 !      IDOUT( 2,14)  = 'Subrange waveheight'
  708 !
  709 ! 3) Frequency-dependent standard parameters
  710 !
  711       NOGE(3) = 6
  712 !
  713       IDOUT( 3, 1)  = '1D Freq. Spectrum   '
  714       IDOUT( 3, 2)  = 'Mean wave dir. a1b1 '
  715       IDOUT( 3, 3)  = 'Mean dir. spr. a1b1 '
 
 
                                                         w3odatmd.F90  page  14
 
 
  716       IDOUT( 3, 4)  = 'Mean wave dir. a2b2 '
  717       IDOUT( 3, 5)  = 'Mean dir. spr. a2b2 '
  718       IDOUT( 3, 6)  = 'Wavenumber array    '
  719 !
  720 ! 4) Spectral Partitions parameters
  721 !
  722       NOGE(4) = 17
  723 !
  724       IDOUT( 4, 1)  = 'Part. wave height   '
  725       IDOUT( 4, 2)  = 'Part. peak period   '
  726       IDOUT( 4, 3)  = 'Part. peak wave len.'
  727       IDOUT( 4, 4)  = 'Part. mean direction'
  728       IDOUT( 4, 5)  = 'Part. dir. spread   '
  729       IDOUT( 4, 6)  = 'Part. wind sea frac.'
  730       IDOUT( 4, 7)  = 'Part. peak direction'
  731       IDOUT( 4, 8)  = 'Part. peakedness    '
  732       IDOUT( 4, 9)  = 'Part. peak enh. fac.'
  733       IDOUT( 4,10)  = 'Part. gaussian width'
  734       IDOUT( 4,11)  = 'Part. spectral width'
  735       IDOUT( 4,12)  = 'Part. mean per. (-1)'
  736       IDOUT( 4,13)  = 'Part. mean per. (+1)'
  737       IDOUT( 4,14)  = 'Part. mean per. (+2)'
  738       IDOUT( 4,15)  = 'Part. peak density  '
  739       IDOUT( 4,16)  = 'Total wind sea frac.'
  740       IDOUT( 4,17)  = 'Number of partitions'
  741 !
  742 ! 5) Atmosphere-waves layer
  743 !
  744       NOGE(5) = 11
  745 !
  746       IDOUT( 5, 1)  = 'Friction velocity   '
  747       IDOUT( 5, 2)  = 'Charnock parameter  '
  748       IDOUT( 5, 3)  = 'Energy flux         '
  749       IDOUT( 5, 4)  = 'Wind-wave enrgy flux'
  750       IDOUT( 5, 5)  = 'Wind-wave net mom. f'
  751       IDOUT( 5, 6)  = 'Wind-wave neg.mom.f.'
  752       IDOUT( 5, 7)  = 'Whitecap coverage   '
  753       IDOUT( 5, 8)  = 'Whitecap mean thick.'
  754       IDOUT( 5, 9)  = 'Mean breaking height'
  755       IDOUT( 5,10)  = 'Dominant break prob '
  756       IDOUT( 5,11)  = 'Wind sea period' ! C.Bunney - reinstated this as i>
  757                                         ! Is it suposed to be defunct? It>
  758 !
  759 ! 6) Wave-ocean layer
  760 !
  761       NOGE(6) = 12
  762 !
  763       IDOUT( 6, 1)  = 'Radiation stresses  '
  764       IDOUT( 6, 2)  = 'Wave-ocean mom. flux'
  765       IDOUT( 6, 3)  = 'wave ind p Bern Head'
  766       IDOUT( 6, 4)  = 'Wave-ocean TKE  flux'
  767       IDOUT( 6, 5)  = 'Stokes transport    '
  768       IDOUT( 6, 6)  = 'Stokes drift at z=0 '
  769       IDOUT( 6, 7)  = '2nd order pressure  '
  770       IDOUT( 6, 8)  = 'Stokes drft spectrum'
 
 
                                                         w3odatmd.F90  page  15
 
 
  771       IDOUT( 6, 9)  = '2nd ord press spectr'
  772       IDOUT( 6,10)  = 'Wave-ice mom. flux  '
  773       IDOUT( 6,11)  = 'Wave-ice energy flux'
  774       IDOUT( 6,12)  = 'Split Surface Stokes'
  775 !
  776 ! 7) Wave-bottom layer
  777 !
  778       NOGE(7) = 5
  779 !
  780       IDOUT( 7, 1)  = 'Bottom rms ampl.    '
  781       IDOUT( 7, 2)  = 'Bottom rms velocity '
  782       IDOUT( 7, 3)  = 'Bedform parameters  '
  783       IDOUT( 7, 4)  = 'Energy diss. in WBBL'
  784       IDOUT( 7, 5)  = 'Moment. loss in WBBL'
  785 !      IDOUT( 7, 6)  = 'Bottom mean period  '
  786 !      IDOUT( 7, 7)  = 'Bottom mean direct  '
  787 !      IDOUT( 7, 8)  = 'Bottom direct spread'
  788 !      IDOUT( 7, 9)  = 'Calc grain rough K_N'
  789 !
  790 ! 8) Spectrum parameters
  791 !
  792       NOGE(8) = 5
  793 !
  794       IDOUT( 8, 1)  = 'Mean square slopes  '
  795       IDOUT( 8, 2)  = 'Phillips tail const'
  796       IDOUT( 8, 3)  = 'Slope direction     '
  797       IDOUT( 8, 4)  = 'Tail slope direction'
  798       IDOUT( 8, 5)  = 'Goda peakedness parm'
  799 !      IDOUT( 8, 3)  = 'Lx-Ly mean wvlength'
  800 !      IDOUT( 8, 4)  = 'Surf grad correl XT'
  801 !      IDOUT( 8, 5)  = 'Surf grad correl YT'
  802 !      IDOUT( 8, 6)  = 'Surf grad correl XY'
  803 !      IDOUT( 8, 7)  = 'Surface crest param'
  804 !      IDOUT( 8, 3)  = '3rd spectral moment '
  805 !      IDOUT( 8, 4)  = '4th spectral moment '
  806 !      IDOUT( 8, 6)  = 'Kurtosis            '
  807 !      IDOUT( 8, 7)  = 'Skewness            '
  808 !
  809 ! 9) Numerical diagnostics
  810 !
  811       NOGE(9) = 5
  812 !
  813       IDOUT( 9, 1)  = 'Avg. time step.     '
  814       IDOUT( 9, 2)  = 'Cut-off freq.       '
  815       IDOUT( 9, 3)  = 'Maximum spatial CFL '
  816       IDOUT( 9, 4)  = 'Maximum angular CFL '
  817       IDOUT( 9, 5)  = 'Maximum k advect CFL'
  818 !      IDOUT( 9, 6)  = 'Avg intrsp proptstep'
  819 !
  820 ! 10) User defined
  821 !
  822       NOGE(10) = NOEXTR
  823 !
  824       DO I=1, MIN ( 20 , NOEXTR )
  825         WRITE (STRING,'(A14,I2.2,A4)') 'User defined #', I, '    '
 
 
                                                         w3odatmd.F90  page  16
 
 
  826         IDOUT(10, I) = STRING
  827         END DO
  828 !
  829       RETURN
  830 !
  831 ! Formats
  832 !
  833  1001 FORMAT (/' *** ERROR W3NOUT : NGRIDS NOT YET SET *** '/         &
  834                '                    NGRIDS = ',I10/                   &
  835                '                    RUN W3NMOD FIRST'/)
  836 !
  837 !/
  838 !/ End of W3NOUT ----------------------------------------------------- /
  839 !/
  840       END SUBROUTINE W3NOUT
  841 !/ ------------------------------------------------------------------- /
  842       SUBROUTINE W3DMO2 ( IMOD, NDSE, NDST, NPT )
  843 !/
  844 !/                  +-----------------------------------+
  845 !/                  | WAVEWATCH III           NOAA/NCEP |
  846 !/                  |           H. L. Tolman            |
  847 !/                  |                        FORTRAN 90 |
  848 !/                  | Last update :         10-Dec-2014 |
  849 !/                  +-----------------------------------+
  850 !/
  851 !/    10-Nov-2004 : Origination.                        ( version 3.06 )
  852 !/    24-Jul-2006 : Adding unified point output storage.( version 3.10 )
  853 !/    25-Jul-2006 : Originating grid ID for points.     ( version 3.10 )
  854 !/    04-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
  855 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
  856 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  857 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
  858 !/
  859 !  1. Purpose :
  860 !
  861 !     Initialize an individual data storage for point output.
  862 !
  863 !  2. Method :
  864 !
  865 !     Allocate directly into the structure array. Note that
  866 !     this cannot be done through the pointer alias!
  867 !
  868 !  3. Parameters :
  869 !
  870 !     Parameter list
  871 !     ----------------------------------------------------------------
  872 !       IMOD    Int.   I   Model number to point to.
  873 !       NDSE    Int.   I   Error output unit number.
  874 !       NDST    Int.   I   Test output unit number.
  875 !       NPT     Int.   I   Array size.
  876 !     ----------------------------------------------------------------
  877 !
  878 !  4. Subroutines used :
  879 !
  880 !     See module documentation below.
 
 
                                                         w3odatmd.F90  page  17
 
 
  881 !
  882 !  5. Called by :
  883 !
  884 !      Name      Type  Module   Description
  885 !     ----------------------------------------------------------------
  886 !      W3IOPO    Subr. W3IOPOMD Point output module.
  887 !     ----------------------------------------------------------------
  888 !
  889 !  6. Error messages :
  890 !
  891 !     - Check on input parameters.
  892 !     - Check on previous allocation.
  893 !
  894 !  7. Remarks :
  895 !
  896 !     - W3SETO needs to be called after allocation to point to
  897 !       proper allocated arrays.
  898 !     - Note that NOPTS is overwritten in W3IOPP.
  899 !
  900 !  8. Structure :
  901 !
  902 !     See source code.
  903 !
  904 !  9. Switches :
  905 !
  906 !     !/S      Enable subroutine tracing.
  907 !     !/T    Enable test output
  908 !
  909 ! 10. Source code :
  910 !
  911 !/ ------------------------------------------------------------------- /
  912       USE W3GDATMD, ONLY: W3SETG, NGRIDS, NAUXGR, IGRID, NSPEC
  913       USE W3SERVMD, ONLY: EXTCDE
  914 !
  915       IMPLICIT NONE
  916 !/
  917 !/ ------------------------------------------------------------------- /
  918 !/ Parameter list
  919 !/
  920       INTEGER, INTENT(IN)           :: IMOD, NDSE, NDST, NPT
  921 !/
  922 !/ ------------------------------------------------------------------- /
  923 !/ Local parameters
  924 !/
  925       INTEGER                 :: JGRID, NLOW
  926 !/
  927 !
  928 ! -------------------------------------------------------------------- /
  929 ! 1.  Test input and module status
  930 !
  931       IF ( NGRIDS .EQ. -1 ) THEN
  932           WRITE (NDSE,1001)
  933           CALL EXTCDE (1)
  934         END IF
  935 !
 
 
                                                         w3odatmd.F90  page  18
 
 
  936       NLOW   = MIN ( 0 , -NAUXGR )
  937       IF ( IMOD.LT.NLOW .OR. IMOD.GT.NOUTP ) THEN
  938           WRITE (NDSE,1002) IMOD, NLOW, NOUTP
  939           CALL EXTCDE (2)
  940         END IF
  941 !
  942       IF ( OUTPTS(IMOD)%OUT2%O2INIT ) THEN
  943           WRITE (NDSE,1003)
  944           CALL EXTCDE (3)
  945         END IF
  946 !
  947       JGRID  = IGRID
  948       IF ( JGRID .NE. IMOD ) CALL W3SETG ( IMOD, NDSE, NDST )
  949 !
  950 ! -------------------------------------------------------------------- /
  951 ! 2.  Allocate arrays
  952 !
  953       ALLOCATE ( OUTPTS(IMOD)%OUT2%IPTINT(2,4,NPT) ,                  &
  954                  OUTPTS(IMOD)%OUT2%IL(NPT)         ,                  &
  955                  OUTPTS(IMOD)%OUT2%IW(NPT)         ,                  &
  956                  OUTPTS(IMOD)%OUT2%II(NPT)         ,                  &
  957                  OUTPTS(IMOD)%OUT2%PTIFAC(4,NPT)   ,                  &
  958                  OUTPTS(IMOD)%OUT2%PTNME(NPT)      ,                  &
  959                  OUTPTS(IMOD)%OUT2%GRDID(NPT)      ,                  &
  960                  OUTPTS(IMOD)%OUT2%DPO(NPT)        ,                  &
  961                  OUTPTS(IMOD)%OUT2%WAO(NPT)        ,                  &
  962                  OUTPTS(IMOD)%OUT2%ZET_SETO(NPT)   ,                  &
  963                  OUTPTS(IMOD)%OUT2%WDO(NPT)        ,                  &
  964                  OUTPTS(IMOD)%OUT2%ASO(NPT)        ,                  &
  965                  OUTPTS(IMOD)%OUT2%CAO(NPT)        ,                  &
  966                  OUTPTS(IMOD)%OUT2%CDO(NPT)        ,                  &
  967                  OUTPTS(IMOD)%OUT2%ICEO(NPT)       ,                  &
  968                  OUTPTS(IMOD)%OUT2%ICEHO(NPT)      ,                  &
  969                  OUTPTS(IMOD)%OUT2%ICEFO(NPT)      ,                  &
  970                  OUTPTS(IMOD)%OUT2%SPCO(NSPEC,NPT) ,                  &
  971                  OUTPTS(IMOD)%OUT2%PTLOC(2,NPT)    , STAT=ISTAT       )
  972       CHECK_ALLOC_STATUS ( ISTAT )
  973 !
  974       OUTPTS(IMOD)%OUT2%O2INIT = .TRUE.
  975 !
  976 ! -------------------------------------------------------------------- /
  977 ! 3.  Point to allocated arrays
  978 !
  979       CALL W3SETO ( IMOD, NDSE, NDST )
  980 !
  981 ! -------------------------------------------------------------------- /
  982 ! 4.  Update counters in grid
  983 !
  984       NOPTS  = NPT
  985 !
  986 ! -------------------------------------------------------------------- /
  987 ! 5.  Restore previous grid setting if necessary
  988 !
  989       IF ( JGRID .NE. IMOD ) CALL W3SETG ( JGRID, NDSE, NDST )
  990 !
 
 
                                                         w3odatmd.F90  page  19
 
 
  991       RETURN
  992 !
  993 ! Formats
  994 !
  995  1001 FORMAT (/' *** ERROR W3DMO2 : GRIDS NOT INITIALIZED *** '/      &
  996                '                    RUN W3NMOD FIRST '/)
  997  1002 FORMAT (/' *** ERROR W3DMO2 : ILLEGAL MODEL NUMBER *** '/       &
  998                '                    IMOD   = ',I10/                   &
  999                '                    NLOW   = ',I10/                   &
 1000                '                    NOUTP  = ',I10/)
 1001  1003 FORMAT (/' *** ERROR W3DMO2 : ARRAY(S) ALREADY ALLOCATED *** ')
 1002 !
 1003 !/
 1004 !/ End of W3DMO2 ----------------------------------------------------- /
 1005 !/
 1006       END SUBROUTINE W3DMO2
 1007 !/ ------------------------------------------------------------------- /
 1008       SUBROUTINE W3DMO3 ( IMOD, NDSE, NDST )
 1009 !/
 1010 !/                  +-----------------------------------+
 1011 !/                  | WAVEWATCH III           NOAA/NCEP |
 1012 !/                  |           H. L. Tolman            |
 1013 !/                  |                        FORTRAN 90 |
 1014 !/                  | Last update :         10-Dec-2014 !
 1015 !/                  +-----------------------------------+
 1016 !/
 1017 !/    24-Nov-2004 : Origination.                        ( version 3.06 )
 1018 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
 1019 !/
 1020 !  1. Purpose :
 1021 !
 1022 !     Initialize an individual data storage for track output.
 1023 !
 1024 !  2. Method :
 1025 !
 1026 !     Allocate directly into the structure array. Note that
 1027 !     this cannot be done through the pointer alias!
 1028 !
 1029 !  3. Parameters :
 1030 !
 1031 !     Parameter list
 1032 !     ----------------------------------------------------------------
 1033 !       IMOD    Int.   I   Model number to point to.
 1034 !       NDSE    Int.   I   Error output unit number.
 1035 !       NDST    Int.   I   Test output unit number.
 1036 !     ----------------------------------------------------------------
 1037 !
 1038 !  4. Subroutines used :
 1039 !
 1040 !     See module documentation below.
 1041 !
 1042 !  5. Called by :
 1043 !
 1044 !      Name      Type  Module   Description
 1045 !     ----------------------------------------------------------------
 
 
                                                         w3odatmd.F90  page  20
 
 
 1046 !      W3IOTR    Subr. W3IOTRMD Track output module.
 1047 !     ----------------------------------------------------------------
 1048 !
 1049 !  6. Error messages :
 1050 !
 1051 !     - Check on input parameters.
 1052 !     - Check on previous allocation.
 1053 !
 1054 !  7. Remarks :
 1055 !
 1056 !     - W3SETO needs to be called after allocation to point to
 1057 !       proper allocated arrays.
 1058 !
 1059 !  8. Structure :
 1060 !
 1061 !     See source code.
 1062 !
 1063 !  9. Switches :
 1064 !
 1065 !     !/SHRD, !/DIST, !/MPI
 1066 !              Shared / distributed memory model
 1067 !
 1068 !     !/S      Enable subroutine tracing.
 1069 !     !/T    Enable test output
 1070 !
 1071 ! 10. Source code :
 1072 !
 1073 !/ ------------------------------------------------------------------- /
 1074       USE W3GDATMD, ONLY: W3SETG, NGRIDS, IGRID, NX, NY
 1075       USE W3SERVMD, ONLY: EXTCDE
 1076 !
 1077       IMPLICIT NONE
 1078 !/
 1079 !/ ------------------------------------------------------------------- /
 1080 !/ Parameter list
 1081 !/
 1082       INTEGER, INTENT(IN)           :: IMOD, NDSE, NDST
 1083 !/
 1084 !/ ------------------------------------------------------------------- /
 1085 !/ Local parameters
 1086 !/
 1087       INTEGER                 :: JGRID
 1088 !/
 1089 !
 1090 ! -------------------------------------------------------------------- /
 1091 ! 1.  Test input and module status
 1092 !
 1093       IF ( NGRIDS .EQ. -1 ) THEN
 1094           WRITE (NDSE,1001)
 1095           CALL EXTCDE (1)
 1096         END IF
 1097 !
 1098       IF ( IMOD.LT.1 .OR. IMOD.GT.NOUTP ) THEN
 1099           WRITE (NDSE,1002) IMOD, NOUTP
 1100           CALL EXTCDE (2)
 
 
                                                         w3odatmd.F90  page  21
 
 
 1101         END IF
 1102 !
 1103       IF ( OUTPTS(IMOD)%OUT3%O3INIT ) THEN
 1104           WRITE (NDSE,1003)
 1105           CALL EXTCDE (3)
 1106         END IF
 1107 !
 1108       JGRID  = IGRID
 1109       IF ( JGRID .NE. IMOD ) CALL W3SETG ( IMOD, NDSE, NDST )
 1110 !
 1111 ! -------------------------------------------------------------------- /
 1112 ! 2.  Allocate arrays
 1113 !
 1114       ALLOCATE ( OUTPTS(IMOD)%OUT3%MASK1(NY,NX) ,                     &
 1115                  OUTPTS(IMOD)%OUT3%MASK2(NY,NX) ,                     &
 1116                  OUTPTS(IMOD)%OUT3%TRCKID(NY,NX), STAT=ISTAT          )
 1117       CHECK_ALLOC_STATUS ( ISTAT )
 1118 !
 1119       OUTPTS(IMOD)%OUT3%O3INIT = .TRUE.
 1120 !
 1121 ! -------------------------------------------------------------------- /
 1122 ! 3.  Point to allocated arrays
 1123 !
 1124       CALL W3SETO ( IMOD, NDSE, NDST )
 1125 !
 1126 ! -------------------------------------------------------------------- /
 1127 ! 4.  Update counters in grid
 1128 !
 1129 ! -------------------------------------------------------------------- /
 1130 ! 5.  Restore previous grid setting if necessary
 1131 !
 1132       IF ( JGRID .NE. IMOD ) CALL W3SETG ( JGRID, NDSE, NDST )
 1133 !
 1134       RETURN
 1135 !
 1136 ! Formats
 1137 !
 1138  1001 FORMAT (/' *** ERROR W3DMO3 : GRIDS NOT INITIALIZED *** '/      &
 1139                '                    RUN W3NMOD FIRST '/)
 1140  1002 FORMAT (/' *** ERROR W3DMO3 : ILLEGAL MODEL NUMBER *** '/       &
 1141                '                    IMOD   = ',I10/                   &
 1142                '                    NOUTP  = ',I10/)
 1143  1003 FORMAT (/' *** ERROR W3DMO3 : ARRAY(S) ALREADY ALLOCATED *** ')
 1144 !
 1145 !/
 1146 !/ End of W3DMO3 ----------------------------------------------------- /
 1147 !/
 1148       END SUBROUTINE W3DMO3
 1149 !/ ------------------------------------------------------------------- /
 1150       SUBROUTINE W3DMO5 ( IMOD, NDSE, NDST, IBLOCK )
 1151 !/
 1152 !/                  +-----------------------------------+
 1153 !/                  | WAVEWATCH III           NOAA/NCEP |
 1154 !/                  |           H. L. Tolman            |
 1155 !/                  |                        FORTRAN 90 |
 
 
                                                         w3odatmd.F90  page  22
 
 
 1156 !/                  | Last update :         10-Dec-2014 !
 1157 !/                  +-----------------------------------+
 1158 !/
 1159 !/    13-Dec-2004 : Origination.                        ( version 3.06 )
 1160 !/    06-Sep-2005 : Second storage for input bound. sp. ( version 3.08 )
 1161 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
 1162 !/
 1163 !  1. Purpose :
 1164 !
 1165 !     Initialize an individual data storage for boundary data.
 1166 !
 1167 !  2. Method :
 1168 !
 1169 !     Allocate directly into the structure array. Note that
 1170 !     this cannot be done through the pointer alias!
 1171 !
 1172 !  3. Parameters :
 1173 !
 1174 !     Parameter list
 1175 !     ----------------------------------------------------------------
 1176 !       IMOD    Int.   I   Model number to point to.
 1177 !       NDSE    Int.   I   Error output unit number.
 1178 !       NDST    Int.   I   Test output unit number.
 1179 !       IBLOCK  Int.   I   Select block to allocate.
 1180 !     ----------------------------------------------------------------
 1181 !
 1182 !  4. Subroutines used :
 1183 !
 1184 !     See module documentation below.
 1185 !
 1186 !  5. Called by :
 1187 !
 1188 !      Name      Type  Module   Description
 1189 !     ----------------------------------------------------------------
 1190 !      W3IOBC    Subr. W3IOBCMD Boundary data output module.
 1191 !      W3IOGR    Subr. W3IOGRMD Grid data output module.
 1192 !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
 1193 !      WW3_GRID  Prog.   N/A    Grid preprocessing program.
 1194 !     ----------------------------------------------------------------
 1195 !
 1196 !  6. Error messages :
 1197 !
 1198 !     - Check on input parameters.
 1199 !     - Check on previous allocation.
 1200 !
 1201 !  7. Remarks :
 1202 !
 1203 !  8. Structure :
 1204 !
 1205 !     See source code.
 1206 !
 1207 !  9. Switches :
 1208 !
 1209 !     !/S      Enable subroutine tracing.
 1210 !     !/T    Enable test output
 
 
                                                         w3odatmd.F90  page  23
 
 
 1211 !
 1212 ! 10. Source code :
 1213 !
 1214 !/ ------------------------------------------------------------------- /
 1215       USE W3GDATMD, ONLY: W3SETG, NGRIDS, IGRID, NX, NY, NSPEC
 1216       USE W3SERVMD, ONLY: EXTCDE
 1217 !
 1218       IMPLICIT NONE
 1219 !/
 1220 !/ ------------------------------------------------------------------- /
 1221 !/ Parameter list
 1222 !/
 1223       INTEGER, INTENT(IN)           :: IMOD, NDSE, NDST, IBLOCK
 1224 !/
 1225 !/ ------------------------------------------------------------------- /
 1226 !/ Local parameters
 1227 !/
 1228       INTEGER                 :: JGRID
 1229 !/
 1230 !
 1231 ! -------------------------------------------------------------------- /
 1232 ! 1.  Test input and module status
 1233 !
 1234       IF ( NGRIDS .EQ. -1 ) THEN
 1235           WRITE (NDSE,1001)
 1236           CALL EXTCDE (1)
 1237         END IF
 1238 !
 1239       IF ( IMOD.LT.1 .OR. IMOD.GT.NOUTP ) THEN
 1240           WRITE (NDSE,1002) IMOD, NOUTP
 1241           CALL EXTCDE (2)
 1242         END IF
 1243 !
 1244 ! -------------------------------------------------------------------- /
 1245 ! 2.  Allocate arrays and reset pointers
 1246 !
 1247       SELECT CASE (IBLOCK)
 1248 !
 1249       CASE (1)
 1250 !
 1251         ALLOCATE ( OUTPTS(IMOD)%OUT5%IPBPI(NBI,4),                    &
 1252                    OUTPTS(IMOD)%OUT5%ISBPI(NBI)  ,                    &
 1253                    OUTPTS(IMOD)%OUT5%XBPI(NBI)   ,                    &
 1254                    OUTPTS(IMOD)%OUT5%YBPI(NBI)   ,                    &
 1255                    OUTPTS(IMOD)%OUT5%RDBPI(NBI,4), STAT=ISTAT         )
 1256         CHECK_ALLOC_STATUS ( ISTAT )
 1257 !
 1258         IPBPI  => OUTPTS(IMOD)%OUT5%IPBPI
 1259         ISBPI  => OUTPTS(IMOD)%OUT5%ISBPI
 1260         XBPI   => OUTPTS(IMOD)%OUT5%XBPI
 1261         YBPI   => OUTPTS(IMOD)%OUT5%YBPI
 1262         RDBPI  => OUTPTS(IMOD)%OUT5%RDBPI
 1263 !
 1264         OUTPTS(IMOD)%OUT5%O5INI1 = .TRUE.
 1265 !
 
 
                                                         w3odatmd.F90  page  24
 
 
 1266       CASE (2)
 1267 !
 1268         ALLOCATE ( OUTPTS(IMOD)%OUT5%IPBPO(NBO(NFBPO),4),             &
 1269                    OUTPTS(IMOD)%OUT5%ISBPO(4*NBO(NFBPO)),             &
 1270                    OUTPTS(IMOD)%OUT5%XBPO(NBO(NFBPO))   ,             &
 1271                    OUTPTS(IMOD)%OUT5%YBPO(NBO(NFBPO))   ,             &
 1272                    OUTPTS(IMOD)%OUT5%RDBPO(NBO(NFBPO),4), STAT=ISTAT  )
 1273         CHECK_ALLOC_STATUS ( ISTAT )
 1274 !
 1275         IPBPO  => OUTPTS(IMOD)%OUT5%IPBPO
 1276         ISBPO  => OUTPTS(IMOD)%OUT5%ISBPO
 1277         XBPO   => OUTPTS(IMOD)%OUT5%XBPO
 1278         YBPO   => OUTPTS(IMOD)%OUT5%YBPO
 1279         RDBPO  => OUTPTS(IMOD)%OUT5%RDBPO
 1280 !
 1281         OUTPTS(IMOD)%OUT5%O5INI2 = .TRUE.
 1282 !
 1283       CASE (3)
 1284 !
 1285         ALLOCATE ( OUTPTS(IMOD)%OUT5%ABPI0(NSPEC,0:NBI2),             &
 1286                    OUTPTS(IMOD)%OUT5%ABPIN(NSPEC,0:NBI2),             &
 1287                    OUTPTS(IMOD)%OUT5%BBPI0(NSPEC,0:NBI),              &
 1288                    OUTPTS(IMOD)%OUT5%BBPIN(NSPEC,0:NBI), STAT=ISTAT   )
 1289         CHECK_ALLOC_STATUS ( ISTAT )
 1290 !
 1291         ABPI0  => OUTPTS(IMOD)%OUT5%ABPI0
 1292         ABPIN  => OUTPTS(IMOD)%OUT5%ABPIN
 1293         BBPI0  => OUTPTS(IMOD)%OUT5%BBPI0
 1294         BBPIN  => OUTPTS(IMOD)%OUT5%BBPIN
 1295         BBPI0 = -1.
 1296 !
 1297         OUTPTS(IMOD)%OUT5%O5INI3 = .TRUE.
 1298 !
 1299       CASE (4)
 1300 !
 1301         ALLOCATE ( OUTPTS(IMOD)%OUT5%ABPOS(NSPEC,0:NBO2(NFBPO)), STAT=IST>
 1302         CHECK_ALLOC_STATUS ( ISTAT )
 1303 !
 1304         ABPOS  => OUTPTS(IMOD)%OUT5%ABPOS
 1305 !
 1306         OUTPTS(IMOD)%OUT5%O5INI4 = .TRUE.
 1307 !
 1308       CASE DEFAULT
 1309         WRITE (NDSE,1010)
 1310         CALL EXTCDE (10)
 1311 !
 1312       END SELECT
 1313 !
 1314 ! -------------------------------------------------------------------- /
 1315 !
 1316       RETURN
 1317 !
 1318 ! Formats
 1319 !
 1320  1001 FORMAT (/' *** ERROR W3DMO5 : GRIDS NOT INITIALIZED *** '/      &
 
 
                                                         w3odatmd.F90  page  25
 
 
 1321                '                    RUN W3NMOD FIRST '/)
 1322  1002 FORMAT (/' *** ERROR W3DMO5 : ILLEGAL MODEL NUMBER *** '/       &
 1323                '                    IMOD   = ',I10/                   &
 1324                '                    NOUTP  = ',I10/)
 1325  1010 FORMAT (/' *** ERROR W3DMO5 : ILLEGAL BLOCK NUMBER  *** '/      &
 1326                '                    IBLOCK = ',I10/)
 1327 !
 1328 !/
 1329 !/ End of W3DMO5 ----------------------------------------------------- /
 1330 !/
 1331       END SUBROUTINE W3DMO5
 1332 !/ ------------------------------------------------------------------- /
 1333       SUBROUTINE W3SETO ( IMOD, NDSERR, NDSTST )
 1334 !/
 1335 !/                  +-----------------------------------+
 1336 !/                  | WAVEWATCH III           NOAA/NCEP |
 1337 !/                  |           H. L. Tolman            |
 1338 !/                  |                        FORTRAN 90 |
 1339 !/                  | Last update :         25-Sep-2020 |
 1340 !/                  +-----------------------------------+
 1341 !/
 1342 !/    13-Dec-2004 : Origination.                        ( version 3.06 )
 1343 !/    06-Sep-2005 : Second storage for input bound. sp. ( version 3.08 )
 1344 !/    24-Jul-2006 : Adding unified point output storage.( version 3.10 )
 1345 !/    25-Jul-2006 : Originating grid ID for points.     ( version 3.10 )
 1346 !/    04-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
 1347 !/    30-Oct-2006 : Add pars for partitioning.          ( version 3.10 )
 1348 !/    26-Mar-2007 : Add pars for partitioning.          ( version 3.11 )
 1349 !/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
 1350 !/    27-Jul-2010 : Add NKI, NTHI, XFRI, FR1I, TH1I.    ( version 3.14.3 )
 1351 !/    19-Dec-2012 : Move NOSWLL to data structure.      ( version 4.11 )
 1352 !/    12-Dec-2014 : Modify instanciation of NRQTR       ( version 5.04 )
 1353 !/    25-Sep-2020 : Flags for coupling restart          ( version 7.10 )
 1354 !/
 1355 !  1. Purpose :
 1356 !
 1357 !     Select one of the WAVEWATCH III grids / models.
 1358 !
 1359 !  2. Method :
 1360 !
 1361 !     Point pointers to the proper variables in the proper element of
 1362 !     the GRIDS array.
 1363 !
 1364 !  3. Parameters :
 1365 !
 1366 !     Parameter list
 1367 !     ----------------------------------------------------------------
 1368 !       IMOD    Int.   I   Model number to point to.
 1369 !       NDSERR  Int.   I   Error output unit number.
 1370 !       NDSTST  Int.   I   Test output unit number.
 1371 !     ----------------------------------------------------------------
 1372 !
 1373 !  4. Subroutines used :
 1374 !
 1375 !     See module documentation below.
 
 
                                                         w3odatmd.F90  page  26
 
 
 1376 !
 1377 !  5. Called by :
 1378 !
 1379 !     Many subroutines in the WAVEWATCH system.
 1380 !
 1381 !  6. Error messages :
 1382 !
 1383 !     Checks on parameter list IMOD.
 1384 !
 1385 !  7. Remarks :
 1386 !
 1387 !  8. Structure :
 1388 !
 1389 !  9. Switches :
 1390 !
 1391 !     !/MPI  MPI specific calls.
 1392 !     !/S    Enable subroutine tracing.
 1393 !     !/T    Enable test output
 1394 !
 1395 ! 10. Source code :
 1396 !
 1397 !/ ------------------------------------------------------------------- /
 1398       USE W3GDATMD, ONLY: NAUXGR
 1399       USE W3SERVMD, ONLY: EXTCDE
 1400 !
 1401       IMPLICIT NONE
 1402 !
 1403 !/
 1404 !/ ------------------------------------------------------------------- /
 1405 !/ Parameter list
 1406 !/
 1407       INTEGER, INTENT(IN)     :: IMOD, NDSERR, NDSTST
 1408 !/
 1409 !/ ------------------------------------------------------------------- /
 1410 !/ Local parameters
 1411 !/
 1412       INTEGER                 :: NLOW
 1413       INTEGER                 :: J
 1414 !/
 1415 !
 1416 ! -------------------------------------------------------------------- /
 1417 ! 1.  Test input and module status
 1418 !
 1419       IF ( NOUTP .EQ. -1 ) THEN
 1420           WRITE (NDSERR,1001)
 1421           CALL EXTCDE (1)
 1422         END IF
 1423 !
 1424       NLOW   = MIN ( 0 , -NAUXGR )
 1425       IF ( IMOD.LT.NLOW .OR. IMOD.GT.NOUTP ) THEN
 1426           WRITE (NDSERR,1002) IMOD, NLOW, NOUTP
 1427           CALL EXTCDE (2)
 1428         END IF
 1429 !
 1430 ! -------------------------------------------------------------------- /
 
 
                                                         w3odatmd.F90  page  27
 
 
 1431 ! 2.  Set model number
 1432 !
 1433       IOUTP  = IMOD
 1434 !
 1435 ! -------------------------------------------------------------------- /
 1436 ! 3.  Set pointers in structure OUTPUT
 1437 !
 1438       NDSO   => OUTPTS(IMOD)%NDSO
 1439       NDSE   => OUTPTS(IMOD)%NDSE
 1440       NDST   => OUTPTS(IMOD)%NDST
 1441       SCREEN => OUTPTS(IMOD)%SCREEN
 1442 !
 1443       NTPROC => OUTPTS(IMOD)%NTPROC
 1444       NAPROC => OUTPTS(IMOD)%NAPROC
 1445       IAPROC => OUTPTS(IMOD)%IAPROC
 1446       NAPLOG => OUTPTS(IMOD)%NAPLOG
 1447       NAPOUT => OUTPTS(IMOD)%NAPOUT
 1448       NAPERR => OUTPTS(IMOD)%NAPERR
 1449       NAPFLD => OUTPTS(IMOD)%NAPFLD
 1450       NAPPNT => OUTPTS(IMOD)%NAPPNT
 1451       NAPTRK => OUTPTS(IMOD)%NAPTRK
 1452       NAPRST => OUTPTS(IMOD)%NAPRST
 1453       NAPBPT => OUTPTS(IMOD)%NAPBPT
 1454       NAPPRT => OUTPTS(IMOD)%NAPPRT
 1455 !
 1456       NOSWLL => OUTPTS(IMOD)%NOSWLL
 1457 !
 1458       TOFRST => OUTPTS(IMOD)%TOFRST
 1459       TONEXT => OUTPTS(IMOD)%TONEXT
 1460       TOLAST => OUTPTS(IMOD)%TOLAST
 1461       TBPI0  => OUTPTS(IMOD)%TBPI0
 1462       TBPIN  => OUTPTS(IMOD)%TBPIN
 1463       NDS    => OUTPTS(IMOD)%NDS
 1464       OFILES => OUTPTS(IMOD)%OFILES
 1465 !
 1466       DTOUT  => OUTPTS(IMOD)%DTOUT
 1467       FLOUT  => OUTPTS(IMOD)%FLOUT
 1468 !
 1469       IPASS1 => OUTPTS(IMOD)%OUT1%IPASS1
 1470       WRITE1 => OUTPTS(IMOD)%OUT1%WRITE1
 1471       NRQGO  => OUTPTS(IMOD)%OUT1%NRQGO
 1472       NRQGO2 => OUTPTS(IMOD)%OUT1%NRQGO2
 1473       IF ( NRQGO  .NE. 0 ) IRQGO  => OUTPTS(IMOD)%OUT1%IRQGO
 1474       IF ( NRQGO2 .NE. 0 ) IRQGO2 => OUTPTS(IMOD)%OUT1%IRQGO2
 1475       FLOGRD => OUTPTS(IMOD)%OUT1%FLOGRD
 1476       FLOGR2 => OUTPTS(IMOD)%OUT1%FLOGR2
 1477       FLOGRR => OUTPTS(IMOD)%OUT1%FLOGRR
 1478       FLOGD   => OUTPTS(IMOD)%OUT1%FLOGD
 1479       FLOG2   => OUTPTS(IMOD)%OUT1%FLOG2
 1480       FLOGR   => OUTPTS(IMOD)%OUT1%FLOGR
 1481 !
 1482       IPASS2 => OUTPTS(IMOD)%OUT2%IPASS2
 1483       NOPTS  => OUTPTS(IMOD)%OUT2%NOPTS
 1484       NRQPO  => OUTPTS(IMOD)%OUT2%NRQPO
 1485       NRQPO2 => OUTPTS(IMOD)%OUT2%NRQPO2
 
 
                                                         w3odatmd.F90  page  28
 
 
 1486       O2INIT => OUTPTS(IMOD)%OUT2%O2INIT
 1487       O2IRQI => OUTPTS(IMOD)%OUT2%O2IRQI
 1488 !
 1489       IF ( O2INIT ) THEN
 1490           IPTINT => OUTPTS(IMOD)%OUT2%IPTINT
 1491           IL     => OUTPTS(IMOD)%OUT2%IL
 1492           IW     => OUTPTS(IMOD)%OUT2%IW
 1493           II     => OUTPTS(IMOD)%OUT2%II
 1494           PTLOC  => OUTPTS(IMOD)%OUT2%PTLOC
 1495           PTIFAC => OUTPTS(IMOD)%OUT2%PTIFAC
 1496           DPO    => OUTPTS(IMOD)%OUT2%DPO
 1497           WAO    => OUTPTS(IMOD)%OUT2%WAO
 1498           ZET_SETO => OUTPTS(IMOD)%OUT2%ZET_SETO
 1499           WDO    => OUTPTS(IMOD)%OUT2%WDO
 1500           ASO    => OUTPTS(IMOD)%OUT2%ASO
 1501           CAO    => OUTPTS(IMOD)%OUT2%CAO
 1502           CDO    => OUTPTS(IMOD)%OUT2%CDO
 1503           ICEO   => OUTPTS(IMOD)%OUT2%ICEO
 1504           ICEHO  => OUTPTS(IMOD)%OUT2%ICEHO
 1505           ICEFO  => OUTPTS(IMOD)%OUT2%ICEFO
 1506           SPCO   => OUTPTS(IMOD)%OUT2%SPCO
 1507           PTNME  => OUTPTS(IMOD)%OUT2%PTNME
 1508           GRDID  => OUTPTS(IMOD)%OUT2%GRDID
 1509         END IF
 1510 !
 1511       IF ( O2IRQI ) THEN
 1512           IRQPO1 => OUTPTS(IMOD)%OUT2%IRQPO1
 1513           IRQPO2 => OUTPTS(IMOD)%OUT2%IRQPO2
 1514         END IF
 1515 !
 1516       IPASS3 => OUTPTS(IMOD)%OUT3%IPASS3
 1517       IT0PNT => OUTPTS(IMOD)%OUT3%IT0PNT
 1518       IT0TRK => OUTPTS(IMOD)%OUT3%IT0TRK
 1519       IT0PRT => OUTPTS(IMOD)%OUT3%IT0PRT
 1520       NRQTR  => OUTPTS(IMOD)%OUT3%NRQTR
 1521       IF ( NRQTR .NE. 0 ) IRQTR  => OUTPTS(IMOD)%OUT3%IRQTR
 1522       O3INIT => OUTPTS(IMOD)%OUT3%O3INIT
 1523       STOP   => OUTPTS(IMOD)%OUT3%STOP
 1524 !
 1525       IF ( O3INIT ) THEN
 1526           MASK1  => OUTPTS(IMOD)%OUT3%MASK1
 1527           MASK2  => OUTPTS(IMOD)%OUT3%MASK2
 1528           TRCKID => OUTPTS(IMOD)%OUT3%TRCKID
 1529         END IF
 1530 !
 1531       IFILE4 => OUTPTS(IMOD)%OUT4%IFILE4
 1532       NRQRS  => OUTPTS(IMOD)%OUT4%NRQRS
 1533       NBLKRS => OUTPTS(IMOD)%OUT4%NBLKRS
 1534       RSBLKS => OUTPTS(IMOD)%OUT4%RSBLKS
 1535       IF ( NRQRS .NE. 0 ) THEN
 1536           IRQRS  => OUTPTS(IMOD)%OUT4%IRQRS
 1537         END IF
 1538       IRQRSS => OUTPTS(IMOD)%OUT4%IRQRSS
 1539       VAAUX  => OUTPTS(IMOD)%OUT4%VAAUX
 1540 !
 
 
                                                         w3odatmd.F90  page  29
 
 
 1541       NBI    => OUTPTS(IMOD)%OUT5%NBI
 1542       NBI2   => OUTPTS(IMOD)%OUT5%NBI2
 1543       NFBPO  => OUTPTS(IMOD)%OUT5%NFBPO
 1544       NRQBP  => OUTPTS(IMOD)%OUT5%NRQBP
 1545       NRQBP2 => OUTPTS(IMOD)%OUT5%NRQBP2
 1546       NBO    => OUTPTS(IMOD)%OUT5%NBO
 1547       NBO2   => OUTPTS(IMOD)%OUT5%NBO2
 1548       NDSL   => OUTPTS(IMOD)%OUT5%NDSL
 1549       NKI    => OUTPTS(IMOD)%OUT5%NKI
 1550       NTHI   => OUTPTS(IMOD)%OUT5%NTHI
 1551       XFRI   => OUTPTS(IMOD)%OUT5%XFRI
 1552       FR1I   => OUTPTS(IMOD)%OUT5%FR1I
 1553       TH1I   => OUTPTS(IMOD)%OUT5%TH1I
 1554       FLBPI  => OUTPTS(IMOD)%OUT5%FLBPI
 1555       FLBPO  => OUTPTS(IMOD)%OUT5%FLBPO
 1556       FILER  => OUTPTS(IMOD)%OUT5%FILER
 1557       FILEW  => OUTPTS(IMOD)%OUT5%FILEW
 1558       FILED  => OUTPTS(IMOD)%OUT5%FILED
 1559       SPCONV => OUTPTS(IMOD)%OUT5%SPCONV
 1560       O5INI1 => OUTPTS(IMOD)%OUT5%O5INI1
 1561       O5INI2 => OUTPTS(IMOD)%OUT5%O5INI2
 1562       O5INI3 => OUTPTS(IMOD)%OUT5%O5INI3
 1563       O5INI4 => OUTPTS(IMOD)%OUT5%O5INI4
 1564 !
 1565       IF ( O5INI1 ) THEN
 1566           IPBPI  => OUTPTS(IMOD)%OUT5%IPBPI
 1567           ISBPI  => OUTPTS(IMOD)%OUT5%ISBPI
 1568           XBPI   => OUTPTS(IMOD)%OUT5%XBPI
 1569           YBPI   => OUTPTS(IMOD)%OUT5%YBPI
 1570           RDBPI  => OUTPTS(IMOD)%OUT5%RDBPI
 1571         END IF
 1572 !
 1573       IF ( O5INI2 ) THEN
 1574           IPBPO  => OUTPTS(IMOD)%OUT5%IPBPO
 1575           ISBPO  => OUTPTS(IMOD)%OUT5%ISBPO
 1576           XBPO   => OUTPTS(IMOD)%OUT5%XBPO
 1577           YBPO   => OUTPTS(IMOD)%OUT5%YBPO
 1578           RDBPO  => OUTPTS(IMOD)%OUT5%RDBPO
 1579         END IF
 1580 !
 1581       IF ( O5INI3 ) THEN
 1582           ABPI0  => OUTPTS(IMOD)%OUT5%ABPI0
 1583           ABPIN  => OUTPTS(IMOD)%OUT5%ABPIN
 1584           BBPI0  => OUTPTS(IMOD)%OUT5%BBPI0
 1585           BBPIN  => OUTPTS(IMOD)%OUT5%BBPIN
 1586         END IF
 1587 !
 1588       IF ( O5INI4 ) THEN
 1589           ABPOS  => OUTPTS(IMOD)%OUT5%ABPOS
 1590         END IF
 1591 !
 1592       IF ( NRQBP  .NE. 0 ) IRQBP1 => OUTPTS(IMOD)%OUT5%IRQBP1
 1593       IF ( NRQBP2 .NE. 0 ) IRQBP2 => OUTPTS(IMOD)%OUT5%IRQBP2
 1594 !
 1595       IPASS6 => OUTPTS(IMOD)%OUT6%IPASS6
 
 
                                                         w3odatmd.F90  page  30
 
 
 1596       IHMAX  => OUTPTS(IMOD)%OUT6%IHMAX
 1597       HSPMIN => OUTPTS(IMOD)%OUT6%HSPMIN
 1598       WSMULT => OUTPTS(IMOD)%OUT6%WSMULT
 1599       WSCUT  => OUTPTS(IMOD)%OUT6%WSCUT
 1600       IX0    => OUTPTS(IMOD)%OUT6%IX0
 1601       IXN    => OUTPTS(IMOD)%OUT6%IXN
 1602       IXS    => OUTPTS(IMOD)%OUT6%IXS
 1603       IY0    => OUTPTS(IMOD)%OUT6%IY0
 1604       IYN    => OUTPTS(IMOD)%OUT6%IYN
 1605       IYS    => OUTPTS(IMOD)%OUT6%IYS
 1606       ICPRT  => OUTPTS(IMOD)%OUT6%ICPRT
 1607       DTPRT  => OUTPTS(IMOD)%OUT6%DTPRT
 1608       FLCOMB => OUTPTS(IMOD)%OUT6%FLCOMB
 1609       PTMETH => OUTPTS(IMOD)%OUT6%PTMETH
 1610       PTFCUT => OUTPTS(IMOD)%OUT6%PTFCUT
 1611       FLFORM => OUTPTS(IMOD)%OUT6%FLFORM
 1612       O6INIT => OUTPTS(IMOD)%OUT6%O6INIT
 1613 !
 1614       RETURN
 1615 !
 1616 ! Formats
 1617 !
 1618  1001 FORMAT (/' *** ERROR W3SETO : GRIDS NOT INITIALIZED *** '/      &
 1619                '                    RUN W3NMOD FIRST '/)
 1620  1002 FORMAT (/' *** ERROR W3SETO : ILLEGAL MODEL NUMBER *** '/       &
 1621                '                    IMOD   = ',I10/                   &
 1622                '                    NLOW   = ',I10/                   &
 1623                '                    NOUTP  = ',I10/)
 1624 !
 1625 !/
 1626 !/ End of W3SETO ----------------------------------------------------- /
 1627 !/
 1628       END SUBROUTINE W3SETO
 1629 !/
 1630 !/ End of module W3ODATMD -------------------------------------------- /
 1631 !/
 1632       END MODULE W3ODATMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
