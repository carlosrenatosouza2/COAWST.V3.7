                                                         w3strkmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3STRKMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |     A. J. van der Westhuysen      |
    8 !/                  |            Jeff Hanson            |
    9 !/                  |        Eve-Marie Devaliere        |
   10 !/                  |                        FORTRAN 95 |
   11 !/                  | Last update :         03-Mar-2016 |
   12 !/                  +-----------------------------------+
   13 !/
   14 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
   15 !/                  by Jeff Hanson & Eve-Marie Devaliere
   16 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
   17 !/    29-Nov-2013 : Remove DOC control characters,
   18 !/                  update MPI! to MPI/! (H. L. Tolman). ( version 4.15 )
   19 !/    26-Sep-2016 : Optimization updates (A. van der Westhuysen)
   20 !/                                                      ( version 5.15 )
   21 !/    03-Mar-2016 : Optimization updates for INTERSECT,
   22 !/                  UNION, UNIQUE, SORT, SETDIFF, FINDIJ
   23 !/                  (S. Zieger, BoM Australia)          ( version 5.16 )
   24 !/
   25 !/    Copyright 2009-2013 National Weather Service (NWS),
   26 !/       National Oceanic and Atmospheric Administration.  All rights
   27 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   28 !/       No unauthorized use without permission.
   29 !/
   30       IMPLICIT NONE
   31 !
   32 !  1. Purpose :
   33 !
   34 !     Module containing data structures and subroutines for spatial and
   35 !     temporal tracking (part of wave partitioning).
   36 !
   37 !  2. Method :
   38 !
   39 !     Read raw partitioning data.
   40 !     Perform tracking in space.
   41 !     Perform tracking in time.
   42 !
   43 !  3. Variables and types :
   44 !
   45 !     NOTE: In Fortran 90/95 derived types cannot contain allocatable arr>
   46 !           The same functionality is achieved here using pointers (point>
   47 !           to unnamed allocatable arrays). Can be replaced by allocatabl>
   48 !           when transitioning to the Fortran 2003 standard.
   49 !
   50 !     Name    Type       Description
   51 !     ----------------------------------------------------------------
   52 !     param   Der. type  structure of basic spectrally partitioned result>
   53 !     hs      Real arr   array of sign. wave height partitions
   54 !     tp      Real arr   array of peak period partitions
   55 !     dir     Real arr   array of mean direction partitions
 
 
                                                         w3strkmd.F90  page   2
 
 
   56 !     dspr    Real arr   array of mean directional spread (one-sided) of >
   57 !     wf      Real arr   array of wind fraction
   58 !     ipart   Int arr    array of partition indices
   59 !     sys     Int arr    array of system indices to which a given partiti>
   60 !     ngbrSys Int arr    array of system indices of neighboring grid poin>
   61 !     checked    Int      0 = geo point not checked yet (in SUBROUTINE fi>
   62 !                         1 = geo point has been checked
   63 !                        -1 = geo land point (i.e. no partitioning data f>
   64 !                        -2 = geo land point, second passing.
   65 !
   66       TYPE param
   67          REAL    :: hs(10)
   68          REAL    :: tp(10)
   69          REAL    :: dir(10)
   70          REAL    :: dspr(10)
   71 !         REAL    :: wf(10)
   72          INTEGER :: ipart(10)
   73          INTEGER :: sys(10)
   74          INTEGER :: ngbrSys(50)
   75          INTEGER :: checked
   76       END TYPE param
   77 !
   78 !     wind    Der. type     structure containing wind-related parameters
   79 !     wdir       Real       wind direction at grid point (Nautical or Car>
   80 !     wspd       Real       wind speed at grid point
   81 !
   82       TYPE wind
   83          REAL    :: wdir
   84          REAL    :: wspd
   85       END TYPE wind
   86 !
   87 !     dat2d   Der. type     2d data structure for storing raw partitioned>
   88 !     lat     Real arr      2d array of latitudes of input partitioned da>
   89 !     lon     Real arr      2d array of longitudes of input partitioned d>
   90 !     par  type(param) arr  2d array of partitioned parameter structures
   91 !     wnd  type(wind) arr   2d array of wind parameter structures
   92 !     maxi    Int           size of 2d array of raw partitioned data in i>
   93 !     maxj    Int           size of 2d array of raw partitioned data in j>
   94 !
   95       TYPE dat2d
   96          REAL*8               :: date
   97          REAL, POINTER        :: lat(:,:)
   98          REAL, POINTER        :: lon(:,:)
   99          TYPE(param), POINTER :: par(:,:)
  100          TYPE(wind), POINTER  :: wnd(:,:)
  101          INTEGER              :: maxi
  102          INTEGER              :: maxj
  103       END TYPE dat2d
  104 !
  105 !     neighbr  Der. type    structure for storing data of neighboring gri>
  106 !     par     type(param)   partitioned parameter structure at neighborin>
  107 !     i          Int        i index of neighboring grid point
  108 !     j          Int        j index of neighboring grid point
  109 !
  110       TYPE neighbr
 
 
                                                         w3strkmd.F90  page   3
 
 
  111          TYPE(param) :: par
  112          INTEGER     :: i
  113          INTEGER     :: j
  114       END TYPE neighbr
  115 !
  116 !     mtchsys  Der. type    structure for storing data of matched systems
  117 !     sysVal   Int arr      array of indices of matched systems
  118 !     tpVal    Real arr     array of peak period values of matched systems
  119 !     wfVal    Real arr     array of wind fraction values of matched syst>
  120 !
  121       TYPE mtchsys
  122          INTEGER     :: sysVal(50)
  123          REAL        :: tpVal(50)
  124          REAL        :: dirVal(50)
  125          REAL        :: hsVal(50)
  126 !         REAL        :: wfVal(50)
  127       END TYPE mtchsys
  128 !
  129 !     system   Der. type    structure for storing spatially tracked syste>
  130 !     hs       Real arr     sign wave height field assoc with wave system>
  131 !     tp       Real arr     peak period field assoc with wave system (in >
  132 !     dir      Real arr     mean direction field assoc with wave system (>
  133 !     dspr     Real arr     mean directional spread field assoc with wave>
  134 !     wf       Real arr     wind fraction assoc with wave system (in 1d a>
  135 !     i        Int arr      i index of geo grid point in wave system (in >
  136 !     j        Int arr      j index of geo grid point in wave system (in >
  137 !     lat      Real arr     latitudes of grid point in wave system (in 1d>
  138 !     lon      Real arr     longitudes of grid point in wave system (in 1>
  139 !     sysInd   Int          index of current wave system
  140 !     hsMean   Real         spatial mean sign wave height of current wave>
  141 !     tpMean   Real         spatial mean peak period of current wave syst>
  142 !     dirMean  Real         spatial mean wave direction of current wave s>
  143 !     wfMean   Real         spatial mean wind fraction of current wave sy>
  144 !     nPoints  Int          total number of grid points in current wave s>
  145 !     ngbr     Int arr      indices of neighboring wave systems
  146 !     grp      Int          time-tracked group that system is assigned to
  147 !
  148       TYPE system
  149          REAL, POINTER    :: hs(:)
  150          REAL, POINTER    :: tp(:)
  151          REAL, POINTER    :: dir(:)
  152          REAL, POINTER    :: dspr(:)
  153 !         REAL, POINTER    :: wf(:)
  154          INTEGER, POINTER :: i(:)
  155          INTEGER, POINTER :: j(:)
  156          INTEGER, POINTER :: indx(:)
  157          REAL, POINTER    :: lat(:)
  158          REAL, POINTER    :: lon(:)
  159          INTEGER          :: sysInd
  160          REAL             :: hsMean
  161          REAL             :: tpMean
  162          REAL             :: dirMean
  163 !         REAL             :: wfMean
  164          INTEGER          :: nPoints
  165          INTEGER          :: ngbr(1000)
 
 
                                                         w3strkmd.F90  page   4
 
 
  166          INTEGER          :: grp
  167       END TYPE system
  168 !
  169 !     timsys   Der. type     structure for storing time-tracked systems (>
  170 !     sys  type(system) arr  array of all spatially+temporally tracked sy>
  171 !                            time level
  172 !
  173       TYPE timsys
  174          TYPE(system), POINTER :: sys(:)
  175       END TYPE timsys
  176 !
  177 !     sysmemory  Der. type   Structure to store key characteristics of sy>
  178 !                            time levels. Used during the time tracking r>
  179 !
  180       TYPE sysmemory
  181          INTEGER :: grp
  182          INTEGER :: nPoints
  183          INTEGER, POINTER :: indx(:)
  184          INTEGER :: updated
  185          INTEGER :: length
  186          REAL    :: lonMean
  187          REAL    :: latMean
  188          REAL    :: tpMean
  189          REAL    :: dirMean
  190       END TYPE sysmemory
  191 !
  192 !  4. Subroutines and functions used :
  193 !
  194 !     a. Main subroutines for spatial/temporal tracking:
  195 !
  196 !     waveTracking_NWS_V2  main subroutine of spatial and temporal tracki>
  197 !     spiralTrackV3        performs the spatial spiral tracking for a giv>
  198 !     timeTrackingV2       performs the time tracking of all wave systems
  199 !
  200 !     b. Auxiliary subroutines and functions for tracking:
  201 !
  202 !     findWay              find direction and no. steps in spatial search>
  203 !     findNext             find next point on spatial search spiral
  204 !     findSys              find all neighboring wave systems for given gr>
  205 !     combineWaveSystems   combine wave systems, then remove small and lo>
  206 !     printFinalSys        output the final output systems for this time >
  207 !     combineSys           combine wave systems
  208 !     combinePartitionsV2  combine two partitions that have been assigned>
  209 !     func. mean_angleV2   compute the mean direction from array of direc>
  210 !     findIJV4             Find indices of system "a" that lie over or ne>
  211 !
  212 !     c. Simple data manipulation (based on Matlab intrinsic functions):
  213 !
  214 !     UNIQUE               removes duplicate reals from an vector
  215 !     SORT                 sorts the vector in ascending or descending or>
  216 !     SETDIFF              returns elements in vector1 that are not in ve>
  217 !     INTERSECT            returns elements that are mutual in vector1 an>
  218 !     UNION                returns the union of vector1 and vector2
  219 !     func. LENGTH         finds no. of indices in vector not filled with>
  220 !     func. FINDFIRST      returns index of first instance of a search va>
 
 
                                                         w3strkmd.F90  page   5
 
 
  221 !     func. STD            computes standard deviation
  222 !
  223 !  5. Called by :
  224 !
  225 !     WW3_SYSTRK (main program)
  226 !
  227 !  6. Error messages :
  228 !
  229 !  7. Remarks :
  230 !
  231 !  8. Structure :
  232 !
  233 !     The structure of the tracking algorithm is the following
  234 !     (parentheses indicate minor subroutines and functions):
  235 !
  236 !     +---- SUBROUTINE waveTracking_NWS_V2       main subroutine of spati>
  237 !     |        (CALL UNIQUE)                     removes duplicate reals >
  238 !     |        CALL spiralTrackV3                See below
  239 !     |        CALL timeTrackingV2               See below
  240 !     |
  241 !     +---+--- SUBROUTINE spiralTrackV3          performs the spatial spi>
  242 !     |   |       CALL findWay                   find direction and no. s>
  243 !     |   |       CALL findNext                  find next point on spati>
  244 !     |   |       CALL findSys                   See below
  245 !     |   |       CALL combineWaveSystems        See below
  246 !     |   |
  247 !     |   +------ SUBROUTINE findSys             find all neighboring wav>
  248 !     |   |          (CALL UNIQUE)
  249 !     |   |          CALL combinePartitionsV2    combine two partitions t>
  250 !     |   |
  251 !     |   +---+-- SUBROUTINE combineWaveSystems  combine wave systems, th>
  252 !     |       |      CALL printFinalSys          See below
  253 !     |       |      CALL combineSys             See below
  254 !     |       |
  255 !     |       +----- SUBROUTINE printFinalSys    output the final output >
  256 !     |       |         (CALL UNIQUE)
  257 !     |       |         (CALL SETDIFF)           returns elements in vect>
  258 !     |       |         (CALL SORT)              sorts the vector in asce>
  259 !     |       |
  260 !     |       +----- SUBROUTINE combineSys       combine wave systems
  261 !     |                 (CALL SORT)
  262 !     |                 (CALL UNIQUE)
  263 !     |                 (CALL UNION)             returns the union of vec>
  264 !     |                 (CALL SETDIFF)
  265 !     |                 CALL findIJV4            Find indices of system ">
  266 !     |                 CALL combinePartitionsV2
  267 !     |
  268 !     +------- SUBROUTINE timeTrackingV2         performs the time tracki>
  269 !                 (CALL SORT)
  270 !                 (CALL SETDIFF)
  271 !
  272 !  9. Switches :
  273 !
  274 !       !/SHRD  Switch for shared / distributed memory architecture.
  275 !       !/MPI   Id.
 
 
                                                         w3strkmd.F90  page   6
 
 
  276 !
  277 ! 10. Source code :
  278 !
  279 !/ ------------------------------------------------------------------- /
  280 !/
  281       CONTAINS
  282 !/ ------------------------------------------------------------------- /
  283       SUBROUTINE waveTracking_NWS_V2 (intype     ,tmax       , &
  284                                       tcur       ,filename   , &
  285                                       tstart     ,tend       , &
  286                                       dt         ,ntint      , &
  287                                       minlon     ,maxlon     , &
  288                                       minlat     ,maxlat     , &
  289                                       mxcwt      ,mycwt      , &
  290                                       dirKnob    ,             &
  291                                       perKnob    ,hsKnob     , &
  292                                       wetPts     ,seedLat    , &
  293                                       seedLon    ,dirTimeKnob, &
  294                                       tpTimeKnob ,paramFile  , &
  295                                       sysA       ,wsdat      , &
  296                                       maxSys     ,maxGroup   )
  297 !/
  298 !/                  +-----------------------------------+
  299 !/                  | WAVEWATCH III           NOAA/NCEP |
  300 !/                  |     A. J. van der Westhuysen      |
  301 !/                  |            Jeff Hanson            |
  302 !/                  |        Eve-Marie Devaliere        |
  303 !/                  |                        FORTRAN 95 |
  304 !/                  | Last update :          4-Jan-2013 |
  305 !/                  +-----------------------------------+
  306 !/
  307 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
  308 !/                  by Jeff Hanson & Eve-Marie Devaliere
  309 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
  310 !/
  311 !/    Copyright 2009-2013 National Weather Service (NWS),
  312 !/       National Oceanic and Atmospheric Administration.  All rights
  313 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  314 !/       No unauthorized use without permission.
  315 !/
  316       IMPLICIT NONE
  317 !
  318 !  1. Purpose :
  319 !
  320 !     Main subroutine of spatial and temporal tracking algorithm
  321 !
  322 !  2. Method
  323 !
  324 !     (1) Read the raw partitioning output from one of two file formats:
  325 !         (a) "partRes" format of IFP-SWAN (intype=1), or
  326 !         (b) WW3 spectral bulletin format (intype=2).
  327 !         If intype=0, the partition data is read from memory (not activa>
  328 !
  329 !     (2) Perform tracking in space by calling subroutine spiralTrackV3
  330 !     (3) Perform tracking in time by calling subroutine timeTrackingV2
 
 
                                                         w3strkmd.F90  page   7
 
 
  331 !
  332 !  3. Parameters :
  333 !
  334 !     Parameter list
  335 !     ----------------------------------------------------------------
  336 !     intype         Int       input  For coupling: Type of input (0 = me>
  337 !     tmax           Int       input  For coupling: Value of maxTs to app>
  338 !     tcur           Int       input  For coupling: Index of current time>
  339 !     filename       Char      input  File name of locally partitioned da>
  340 !     tstart         Char      input  Start time in raw partition file (i>
  341 !     tend           Char      input  End time in raw partition file (if >
  342 !     minlon         Real      input  Lower lon boundary of domain to be >
  343 !     maxlon         Real      input  Upper lon boundary of domain to be >
  344 !     minlat         Real      input  Lower lat boundary of domain to be >
  345 !     maxlat         Real      input  Upper lat boundary of domain to be >
  346 !     dirKnob        Real      input  Parameter in direction for combinin>
  347 !     perKnob        Real      input  Parameter in period for combining f>
  348 !     hsKnob         Real      input  Parameter in wave height for purgin>
  349 !     wetPts         Real      input  Percentage of wet points for purgin>
  350 !     seedLat        Real      input  Start Lat for tracking spiral (if =>
  351 !     seedLon        Real      input  Start Lon for tracking spiral (if =>
  352 !     dirTimeKnob    Real      input  Parameter in direction for combinin>
  353 !     tpTimeKnob     Real      input  Parameter in period for combining f>
  354 !     paramFile      Char      input  File name of partitioning parameters
  355 !     sys         TYPE(timsys) output Final set of spatially and temporal>
  356 !     wsdat       TYPE(dat2d)  output Final version of 2D (gridded) parti>
  357 !     maxGroup       Int       output Maximum number of wave systems ("gr>
  358 !
  359       CHARACTER    :: filename*50, paramFile*32
  360       REAL         :: dirKnob, perKnob, hsKnob, wetPts, seedLat, &
  361                       seedLon, dirTimeKnob, tpTimeKnob
  362       REAL*8       :: tstart, tend
  363       INTEGER      :: maxGroup, intype, tmax, tcur, ntint
  364       INTEGER, POINTER :: maxSys(:)
  365       TYPE(dat2d), POINTER :: wsdat(:)
  366       TYPE(timsys), POINTER :: sysA(:), sysAA(:)
  367       INTEGER      :: NumConsSys, iConsSys
  368       REAL         :: dt
  369       REAL         :: minlon, maxlon, minlat, maxlat
  370       INTEGER      :: mxcwt, mycwt
  371                                   
  372 !     Note: Variables wsdat, sysA and maxSys have IN/OUT intent so that t>
  373 !     can be manipulated outside of this subroutine, e.g. re-indexing of
  374 !     systems and groups during the simulation.
  375       INTENT (IN)  intype, tmax, tcur, filename, paramFile, &
  376                    minlon, maxlon, minlat, maxlat, &
  377                    hsKnob, wetPts, seedLat, seedLon, &
  378                    dirKnob, perKnob, dirTimeKnob, tpTimeKnob
  379       INTENT (OUT) maxGroup
  380 !      INTENT (IN OUT) wsdat, sysA, maxSys
  381 !
  382 !     Local variables
  383 !     ----------------------------------------------------------------
  384 !     llat     Real    Latitude of partition point, from input file
  385 !     llon     Real    Longitude of partition point, from input file
 
 
                                                         w3strkmd.F90  page   8
 
 
  386 !     ts       Real    Time step of partition, from input file
  387 !     hs0      Real    Wave height of partition, from input file
  388 !     tp0      Real    Peak period of partition, from input file
  389 !     dir0     Real    Mean direction of partition, from input file
  390 !     dspr0    Real    Mean directional spread of partition, from input f>
  391 !     wf0      Real    wind fraction of partition, from input file (remov>
  392 !     wndSpd0  Real    Wind speed of partition, from input file
  393 !     wndDir0  Real    Wind direction of partition, from input file
  394 !     wndFce0  Real    Wind force of partition, from input file (not, use>
  395 !     tss      Int.    Time step counter
  396 !     t0       Int     Index of first time step to compute
  397 !
  398       LOGICAL                :: file_exists, FLFORM, LOOP
  399       LOGICAL                :: testout
  400       PARAMETER (testout = .FALSE.)
  401       CHARACTER              :: dummy*10, dummyc*12
  402       CHARACTER(LEN=10)      :: VERPRT
  403       CHARACTER(LEN=35)      :: IDSTR
  404       CHARACTER(LEN=78)      :: headln1
  405       CHARACTER(LEN=51)      :: headln2
  406       INTEGER                :: line
  407       INTEGER, ALLOCATABLE   :: ts(:), tmp_i4(:)
  408       REAL, ALLOCATABLE      :: llat(:),llon(:),hs0(:), &
  409                                 tp0(:),dir0(:),dspr0(:),&
  410                                 wndSpd0(:),wndDir0(:)
  411       REAL*8, ALLOCATABLE :: date0(:),tmp_r8(:)
  412       INTEGER    :: maxTs, t0, nout1, nout2, maxI, maxJ
  413       REAL, ALLOCATABLE :: mlon(:,:), mlat(:,:), tmp_r4(:)
  414       REAL, POINTER :: uniqueTim(:),uniqueLatraw(:),uniqueLonraw(:), &
  415                        uniqueLat(:),uniqueLon(:)
  416       INTEGER    :: ioerr,ierr, i, j, k, l, alreadyIn, ok, tss, tsA
  417       INTEGER    :: maxPart, DATETIME(2)
  418       INTEGER    :: tstep, iline, numpart, skipln, readln, filesize
  419       REAL       :: x,y,wnd,wnddir
  420       REAL       :: invar1, invar2, invar3, invar4
  421       REAL       :: invar5, invar6, invar7
  422       REAL, ALLOCATABLE :: phs(:),ptp(:),pdir(:),pspr(:),pwf(:) ! current>
  423       REAL*8     :: date1, date2, ttest, ttemp
  424       INTEGER    :: ic, leng, maxpartout                                 >
  425       REAL       :: dx
  426       INTEGER    :: latind1, latind2, lonind1, lonind2
  427       REAL       :: lonext, latext
  428                                   
  429 !
  430 !  4. Subroutines used :
  431 !
  432 !      Name      Type  Module   Description
  433 !     ----------------------------------------------------------------
  434 !      UNIQUE
  435 !      spiralTrackV3
  436 !      timeTrackingV2
  437 !
  438 !  5. Subroutines calling
  439 !
  440 !      WW3_SYSTRK (main program)
 
 
                                                         w3strkmd.F90  page   9
 
 
  441 !
  442 !  6. Error messages :
  443 !
  444 !  7. Remarks :
  445 !
  446 !  8. Structure :
  447 !
  448 !     See above
  449 !
  450 !  9. Switches :
  451 !
  452 !     None defined yet.
  453 !
  454 ! 10. Source code :
  455 !
  456 !/ ------------------------------------------------------------------- /
  457                                                                         
  458       NULLIFY( sysA )
  459       NULLIFY( maxSys )
  460                        
  461 !     Select input type for raw partitioning data
  462       IF ((intype.EQ.1).OR.(intype.EQ.2)) THEN
  463 !        Raw partitioning data is coming from an input file.
  464 !        Read file here, and set up 2d array wsdat with the data.
  465          t0 = 1
  466          IF (intype.EQ.1) THEN
  467 !           Read partRes format file
  468             WRITE(20,*) 'Reading partRes partitioning file...'
  469             filesize = 7500000
  470             ALLOCATE(ts(filesize))
  471             ALLOCATE(llat(filesize))
  472             ALLOCATE(llon(filesize))
  473             ALLOCATE(hs0(filesize))
  474             ALLOCATE(tp0(filesize))
  475             ALLOCATE(dir0(filesize))
  476             ALLOCATE(dspr0(filesize))
  477 !            ALLOCATE(wf0(filesize))
  478             ALLOCATE(wndSpd0(filesize))
  479             ALLOCATE(wndDir0(filesize))
  480             ALLOCATE(date0(filesize))
  481             WRITE(20,*) '*** Max number of lines read from "partRes" ', &
  482                     'input file is = ',filesize,'!'
  483             WRITE(6,*) 'Reading partRes file...'
  484             INQUIRE(FILE=filename, EXIST=file_exists)
  485             IF (.NOT.file_exists) THEN
  486                WRITE(20,2001)
  487                WRITE(6,2001)
  488                STOP 1
  489             END IF
  490             OPEN(unit=11,file=filename,status='old')
  491             line = 1
  492             DO WHILE (.TRUE.)
  493                READ (11, *, END=113) dummyc,llat(line),llon(line),   &
  494                             ts(line),hs0(line),tp0(line),dir0(line), &
  495                             wndSpd0(line),wndDir0(line),invar7
 
 
                                                         w3strkmd.F90  page  10
 
 
  496                !partRes file does not contain the dspr variable
  497                dspr0(line) = 9999.
  498 !               wf0(line) = 9999.
  499                line = line+1
  500             ENDDO
  501  113        IERR = -1
  502             CLOSE(11)
  503             line = line-1
  504             WRITE(6,*) '... finished'
  505 !            DEALLOCATE(date0)
  506          ELSE IF (intype.EQ.2) THEN
  507 !           Read WW3 Spectral Partition format file
  508 !           Query input file to determine required array sizes
  509             INQUIRE(FILE=filename, EXIST=file_exists)
  510             IF (.NOT.file_exists) THEN
  511                WRITE(20,2001)
  512                WRITE(6,2001)
  513                STOP 1
  514             END IF
  515 !/       -------------------------------------------------
  516 !/          Test unformatted read
  517 !/       -------------------------------------------------
  518             OPEN(UNIT=11,FILE=FILENAME,FORM='UNFORMATTED',STATUS='OLD',AC>
  519             READ(11,ERR=802,IOSTAT=IOERR) I
  520             CLOSE(11)
  521 !/          --- First four-byte integer could possibly be byte-swapped,
  522 !               if ww3_shel was compiled on a different architecture. ---
  523             K = SWAPI4(I)
  524             FLFORM = .NOT.(I.EQ.(LEN(IDSTR)+LEN(VERPRT)).OR.&
  525                            K.EQ.(LEN(IDSTR)+LEN(VERPRT))    )
  526 !        ======== COUNT LOOP ===========
  527             IF (FLFORM) THEN
  528 !              Input file in formatted ASCII
  529                WRITE(6,*) 'Reading formatted ASCII file...'
  530                OPEN(unit=11,file=filename,status='old')
  531                READ(11,'(78A)') headln1
  532                IDSTR = headln1(1:LEN(IDSTR))
  533                READ(11,'(78A)') headln1
  534                READ(11,'(51A)') headln2
  535             ELSE
  536                IF (K.EQ.(LEN(IDSTR)+LEN(VERPRT))) THEN
  537 !/           --- Stop here. The file appears to be endian encoded
  538 !                different from the native machine format. And, the
  539 !                compiler option will override support for FORTRAN
  540 !                convert statements convert='big_endian' or
  541 !                convert='little_endian'. ---
  542                  WRITE(20,1200)
  543                  WRITE(6,1200)
  544                  STOP 1
  545                ELSE
  546 !                Input file in unformatted binary
  547                  WRITE(6,*) 'Reading binary formatted file...'
  548                  OPEN(unit=11,file=filename,form='UNFORMATTED', &
  549                       status='OLD')
  550                ENDIF
 
 
                                                         w3strkmd.F90  page  11
 
 
  551                REWIND(11)
  552                READ(11,ERR=802,IOSTAT=IOERR) IDSTR,VERPRT
  553                READ(11,ERR=802,IOSTAT=IOERR) headln1
  554                READ(11,ERR=802,IOSTAT=IOERR) headln2
  555             END IF
  556 !/
  557            IF (IDSTR(1:9).ne.'WAVEWATCH') THEN
  558              CLOSE(11)
  559              WRITE(20,1300)
  560              WRITE(6,1300)
  561              STOP 1
  562            ENDIF
  563 !/       -------------------------------------------------
  564 !/          Skip to start time
  565 !/       -------------------------------------------------
  566             skipln = 3
  567             ttest = 0
  568             DO WHILE (ttest.LT.tstart)
  569               IF (FLFORM) THEN
  570                  READ (11,1000,ERR=802,END=112) date1,date2,x,y, &
  571                                   numpart,wnd,wnddir,invar6,invar7
  572                  skipln = skipln+1
  573               ELSE
  574                  READ (11,ERR=802,IOSTAT=IOERR) DATETIME,x,y, &
  575                             dummy,numpart,invar1,wnd,wnddir, &
  576                             invar5,invar6
  577 ! write(*,*) '0:',DATETIME,numpart
  578                  date1=dble(DATETIME(1))
  579                  date2=dble(DATETIME(2))
  580               END IF
  581               ttest = date1 + date2*1.0E-6
  582               IF (FLFORM) THEN
  583                 DO line = 1,numpart+1
  584                   READ(11,1010,END=111,ERR=802,IOSTAT=IOERR) &
  585                             invar1,invar2,invar3,invar4
  586 ! write(*,*) '0+:',line,numpart+1,invar1,invar2,invar3,invar4
  587                   skipln = skipln+1
  588                 END DO
  589               ELSE
  590                 DO line = 1,numpart+1
  591                   READ (11,ERR=802,IOSTAT=IOERR) iline,invar1, &
  592                               invar2,invar3,invar4,invar5,invar6
  593 ! write(*,*) '0+:',line,iline,invar1,invar2,invar3,invar4,invar5,invar6
  594                 END DO
  595               END IF
  596             END DO
  597             skipln = skipln-numpart-1-1
  598 !/       -------------------------------------------------
  599 !           Read file for ntint time levels
  600 !/       -------------------------------------------------
  601             readln = numpart
  602             tstep = 1
  603             ttemp = tstart
  604             maxPart = numpart
  605             DO WHILE (tstep.LE.ntint)
 
 
                                                         w3strkmd.F90  page  12
 
 
  606               IF (readln.GT.0) THEN
  607                  IF (FLFORM) THEN
  608                    READ (11,1000,ERR=802,END=111) date1,date2,x,y, &
  609                                     numpart,wnd,wnddir,invar6,invar7
  610                  ELSE
  611                     READ (11,END=111,ERR=802,IOSTAT=IOERR) DATETIME,  &
  612                       x,y,dummy,numpart,wnd,wnddir,invar5,invar6,invar7
  613 ! write(*,*) '1:',numpart,x,y
  614                     date1=dble(DATETIME(1))
  615                     date2=dble(DATETIME(2))
  616                  END IF
  617                  maxPart = MAX(maxPart,numpart)
  618               END IF
  619                     
  620               ttest = date1 + date2*1.E-6
  621               IF (ttest.GT.ttemp) THEN
  622                  tstep = tstep+1
  623                  ttemp = ttest
  624                  IF (tstep.GT.ntint) EXIT
  625               END IF
  626               IF (FLFORM) THEN
  627                  DO line = 1,numpart+1
  628                    READ (11,1010,END=111,ERR=802,IOSTAT=IOERR)      &
  629                             invar1,invar2,invar3,invar4
  630                    readln = readln+1
  631                  END DO
  632               ELSE
  633                  DO line = 1,numpart+1
  634                    READ (11,END=111,ERR=802,IOSTAT=IOERR) iline,invar1,&
  635                        invar2,invar3,invar4,invar5,invar6
  636                    readln = readln+1
  637                  END DO
  638               END IF
  639             ENDDO
  640  111        CONTINUE
  641             CLOSE(11)
  642 !        ===== END COUNT LOOP =====
  643 !        ===== START READ LOOP =====
  644             ALLOCATE(ts(readln))
  645             ALLOCATE(llat(readln))
  646             ALLOCATE(llon(readln))
  647             ALLOCATE(hs0(readln))
  648             ALLOCATE(tp0(readln))
  649             ALLOCATE(dir0(readln))
  650             ALLOCATE(dspr0(readln))
  651 !            ALLOCATE(wf0(readln))
  652             ALLOCATE(wndSpd0(readln))
  653             ALLOCATE(wndDir0(readln))
  654             ALLOCATE(date0(readln))
  655             ts(1:readln)   = -1
  656             llat(1:readln) = 9999.
  657             llon(1:readln) = 9999.
  658             hs0(1:readln)  = 9999.
  659             tp0(1:readln)  = 9999.
  660             dir0(1:readln)  = 9999.
 
 
                                                         w3strkmd.F90  page  13
 
 
  661             dspr0(1:readln)  = 9999.
  662                                     
  663                                     
  664             IF (FLFORM) THEN
  665                OPEN(unit=11,file=filename,status='old')
  666             ELSE
  667                OPEN(unit=11,file=filename,status='old', &
  668                     form='unformatted')
  669             END IF
  670             line = 1
  671             tstep = 1
  672 !/       -------------------------------------------------
  673 !/          Skip to start time
  674 !/       -------------------------------------------------
  675             IF (FLFORM) THEN
  676                DO i = 1,skipln
  677                   READ (11, *)
  678                END DO
  679             ELSE
  680             ! --- Repeat from above since access='DIRECT'
  681             !     does not support fseek and ftell. ---
  682                READ(11,END=112,ERR=802,IOSTAT=IOERR) IDSTR,VERPRT
  683                READ(11,END=112,ERR=802,IOSTAT=IOERR) headln1
  684                READ(11,END=112,ERR=802,IOSTAT=IOERR) headln2
  685             !/ --- allocate buffer for all partition parameters
  686             !/     for a single grid point  ---
  687                IF (.NOT.ALLOCATED(PHS)) ALLOCATE(PHS(maxPart))
  688                IF (.NOT.ALLOCATED(PTP)) ALLOCATE(PTP(maxPart))
  689                IF (.NOT.ALLOCATED(PDIR)) ALLOCATE(PDIR(maxPart))
  690                IF (.NOT.ALLOCATED(PSPR)) ALLOCATE(PSPR(maxPart))
  691                IF (.NOT.ALLOCATED(PWF)) ALLOCATE(PWF(maxPart))
  692                                                               
  693                ttest = 0
  694                         
  695                DO WHILE (ttest.LT.tstart)
  696                  READ (11,END=112,ERR=802,IOSTAT=IOERR) DATETIME, &
  697                             invar1,invar2,dummy,numpart,invar3,   &
  698                             invar4,invar5,invar6,invar7
  699                  date1=dble(DATETIME(1))
  700                  date2=dble(DATETIME(2))
  701                  ttest = date1 + date2*1.0E-6
  702               !/ --- reset buffer ---
  703                  PHS(:) = 0.
  704                  PTP(:) = 0.
  705                  PDIR(:) = 0.
  706                  PSPR(:) = 0.
  707                  PWF(:) = 0.
  708                             
  709               !/ --- fill buffer with partition data ---
  710                  READ (11,END=112,ERR=802,IOSTAT=IOERR) iline,invar1, &
  711                           invar2,invar3,invar4,invar5,invar6
  712                  DO i = 1,numpart
  713                    READ (11,END=112,ERR=802,IOSTAT=IOERR) iline,      &
  714                        phs(i),ptp(i),invar3,pdir(i),pspr(i),pwf(i)
  715                  END DO
 
 
                                                         w3strkmd.F90  page  14
 
 
  716                END DO
  717              !/ --- move buffer content to data array ---
  718                DO i=1,numpart
  719                  hs0(line)   = phs(i)
  720                  tp0(line)   = ptp(i)
  721                  dir0(line)  = pdir(i)
  722                  dspr0(line) = pspr(i)
  723                  date0(line) = date1 + date2*1.0E-6
  724                  ts(line) = tstep
  725                  llat(line) = x
  726                  llon(line) = y
  727                  wndSpd0(line) = wnd
  728                  wndDir0(line) = wnddir
  729                                        
  730                  line = line + 1
  731                END DO
  732                      
  733             END IF
  734 !/       -------------------------------------------------
  735 !           Read file for ntint time levels
  736 !/       -------------------------------------------------
  737             ttemp = tstart
  738             DO WHILE (line.LE.readln)
  739               IF (FLFORM) THEN
  740                  READ (11,1000,END=112) date1,date2,x,y,numpart, &
  741                                         wnd,wnddir,invar6,invar7
  742               ELSE
  743                  READ (11,ERR=802,IOSTAT=IOERR) DATETIME,x,y,    &
  744                       dummy,numpart,wnd,wnddir,invar5,invar6,invar7
  745                  date1=dble(DATETIME(1))
  746                  date2=dble(DATETIME(2))
  747               END IF
  748                     
  749               ttest = date1 + date2*1.0E-6
  750               IF (ttest.GT.ttemp) THEN
  751                  tstep = tstep+1
  752                  ttemp = ttest
  753                  IF (tstep.GT.ntint) EXIT
  754               END IF
  755                     
  756               IF (FLFORM) THEN
  757                 READ (11,1010,END=112) invar1,invar2,invar3,invar4 ! Skip>
  758                 DO i = 1,numpart
  759                   IF (line.LE.readln) THEN
  760                     READ (11,1010,END=112) hs0(line),tp0(line), &
  761                                        dir0(line),dspr0(line)
  762                     date0(line) = ttest
  763                                        
  764                     ts(line) = tstep
  765                     llat(line) = x
  766                     llon(line) = y
  767                     wndSpd0(line) = wnd
  768                     wndDir0(line) = wnddir
  769                                           
  770                     line = line+1
 
 
                                                         w3strkmd.F90  page  15
 
 
  771                   END IF
  772                 END DO
  773               ELSE
  774                  READ (11,ERR=802,IOSTAT=IOERR) k,invar1,invar2,  &
  775                      invar3,invar4,invar5
  776                  DO i = 1,numpart
  777                    IF (line.LE.readln) THEN
  778                      READ (11,END=112,ERR=802,IOSTAT=IOERR) k,         &
  779                           hs0(line),tp0(line),invar3,dir0(line),       &
  780                           dspr0(line)
  781                      date0(line) = ttest
  782                                         
  783                      ts(line) = tstep
  784                      llat(line) = x
  785                      llon(line) = y
  786                      wndSpd0(line) = wnd
  787                      wndDir0(line) = wnddir
  788                                            
  789                      line = line+1
  790                   END IF
  791                  END DO
  792               END IF
  793             END DO
  794  110        IERR = -1
  795             CLOSE(11)
  796                      
  797  112        CONTINUE
  798             IF (line.EQ.1) THEN
  799                WRITE(20,2002)
  800                WRITE(6,2002)
  801                STOP 1
  802             END IF
  803             CLOSE(11)
  804 ! ===== READ LOOP FINISHED =====
  805             LINE=LINE-1
  806                        
  807             WRITE(6,*) '... finished'
  808                                      
  809             IF (ttest.LT.tstart) THEN
  810                WRITE(20,2003) TSTART
  811                WRITE(6,2003) TSTART
  812                STOP 1
  813             END IF
  814                   
  815             IF (ALLOCATED(PHS)) DEALLOCATE(PHS)
  816             IF (ALLOCATED(PTP)) DEALLOCATE(PTP)
  817             IF (ALLOCATED(PDIR)) DEALLOCATE(PDIR)
  818             IF (ALLOCATED(PSPR)) DEALLOCATE(PSPR)
  819             IF (ALLOCATED(PWF)) DEALLOCATE(PWF)
  820                                                
  821          END IF
  822                
  823                
  824 !        Find unique time steps (and sort in ascending order)
  825          CALL UNIQUE(REAL(ts(1:line)),line,uniqueTim,maxTs)
 
 
                                                         w3strkmd.F90  page  16
 
 
  826                                                            
  827 !        Find unique lat and lon values (and sort in ascending order)
  828          CALL UNIQUE(llat(1:line),SIZE(llat(1:line)),uniqueLatraw,nout1)
  829          CALL UNIQUE(llon(1:line),SIZE(llon(1:line)),uniqueLonraw,nout2)
  830                                                                         
  831 !--042916-----------------------
  832 !
  833 !        Redefine uniqueLatraw and uniqueLonrawto based on domain extent
  834          WRITE(20,*) 'uniqueLatraw(:) =', uniqueLatraw(:)
  835          WRITE(20,*) 'uniqueLonraw(:) =', uniqueLonraw(:)
  836                                                          
  837          WRITE(20,*) 'No. increments: Longitude, Latitue =', mxcwt, mycwt
  838          DEALLOCATE(uniqueLatraw)
  839          DEALLOCATE(uniqueLonraw)
  840          ALLOCATE(uniqueLatraw(mycwt+1))
  841          ALLOCATE(uniqueLonraw(mxcwt+1))
  842          DO i = 1,(mycwt+1)
  843             uniqueLatraw(i) =  minlat + &
  844                                (REAL(i)-1)/REAL(mycwt)*(maxlat-minlat)
  845          END DO
  846          DO i = 1,(mxcwt+1)
  847             uniqueLonraw(i) =  minlon + &
  848                                (REAL(i)-1)/REAL(mxcwt)*(maxlon-minlon)
  849          END DO
  850          WRITE(20,*) 'uniqueLatraw(:) =', uniqueLatraw(:)
  851          WRITE(20,*) 'uniqueLonraw(:) =', uniqueLonraw(:)
  852 !
  853 !--042916-----------------------
  854                                 
  855 !        Filter out lats and lons outside of domain of interest
  856          DO latind1 = 1,SIZE(uniqueLatraw)
  857             IF (uniqueLatraw(latind1).GE.minlat) EXIT
  858          END DO
  859          DO latind2 = SIZE(uniqueLatraw),1,-1
  860             IF (uniqueLatraw(latind2).LE.maxlat) EXIT
  861          END DO
  862          DO lonind1 = 1,SIZE(uniqueLonraw)
  863             IF (uniqueLonraw(lonind1).GE.minlon) EXIT
  864          END DO
  865          DO lonind2 = SIZE(uniqueLonraw),1,-1
  866             IF (uniqueLonraw(lonind2).LE.maxlon) EXIT
  867          END DO
  868          WRITE(20,*) 'latind1, latind2, lonind1, lonind2 =', &
  869                      latind1, latind2, lonind1, lonind2
  870          IF ((latind1.GE.latind2).OR.(lonind1.GE.lonind2)) THEN
  871             WRITE(20,1400)
  872             WRITE(6,1400)
  873             STOP 1
  874          END IF
  875          NULLIFY(uniqueLat)
  876          NULLIFY(uniqueLon)
  877          ALLOCATE(uniqueLat(latind2-latind1+1))
  878          ALLOCATE(uniqueLon(lonind2-lonind1+1))
  879          uniqueLat = uniqueLatraw(latind1:latind2)
  880          uniqueLon = uniqueLonraw(lonind1:lonind2)
 
 
                                                         w3strkmd.F90  page  17
 
 
  881          WRITE(20,*) 'In waveTracking_NWS_V2: Longitude range =', &
  882                      uniqueLon(1), uniqueLon(SIZE(uniqueLon))
  883          WRITE(20,*) '                        Latitude range  =', &
  884                      uniqueLat(1), uniqueLat(SIZE(uniqueLat))
  885                                                              
  886 !        Map is transposed (rotated by 90 deg), so that:
  887 !          I (matrix row) represents Longitute
  888 !          J (matrix column) represents Latitude
  889 !          i.e. from this point onwards the indices (i,j) represents Cart>
  890          ALLOCATE( mlon(SIZE(uniqueLon),SIZE(uniqueLat)) )
  891          ALLOCATE( mlat(SIZE(uniqueLon),SIZE(uniqueLat)) )
  892 !
  893          maxI = SIZE(uniqueLon)
  894          maxJ = SIZE(uniqueLat)
  895          DO I = 1,maxI
  896             DO J = 1,maxJ
  897                mlon(I,J) = uniqueLon(I)
  898                mlat(I,J) = uniqueLat(J)
  899             END DO
  900          END DO
  901                
  902                
  903 !        Allocate the wsdat structure
  904             WRITE(20,*) 'Allocating wsdat...'
  905          NULLIFY(wsdat)
  906          ALLOCATE(wsdat(maxTs))
  907             WRITE(20,*) 'SIZE(wsdat) = ',SIZE(wsdat)
  908                                                     
  909 !        Allocate and initialize the wsdat array
  910          DO tsA = 1,maxTs
  911             ALLOCATE(wsdat(tsA)%lat(maxI,maxJ))
  912             ALLOCATE(wsdat(tsA)%lon(maxI,maxJ))
  913             ALLOCATE(wsdat(tsA)%par(maxI,maxJ))
  914             ALLOCATE(wsdat(tsA)%wnd(maxI,maxJ))
  915                                                
  916             DO j = 1,maxJ
  917                DO i = 1,maxI
  918                   wsdat(tsA)%lat(i,j)=mlat(i,j)
  919                   wsdat(tsA)%lon(i,j)=mlon(i,j)
  920                   wsdat(tsA)%maxi=maxI
  921                   wsdat(tsA)%maxj=maxJ
  922                   wsdat(tsA)%par(i,j)%hs(1:10)=9999.
  923                   wsdat(tsA)%par(i,j)%tp(1:10)=9999.
  924                   wsdat(tsA)%par(i,j)%dir(1:10)=9999.
  925                   wsdat(tsA)%par(i,j)%dspr(1:10)=9999.
  926 !                  wsdat(tsA)%par(i,j)%wf(1:10)=9999.
  927                   wsdat(tsA)%par(i,j)%ipart(1:10)=0
  928                   wsdat(tsA)%par(i,j)%sys(1:10)=9999                     >
  929                   wsdat(tsA)%par(i,j)%ngbrSys(1:50)=9999
  930                   wsdat(tsA)%wnd(i,j)%wdir=9999.
  931                   wsdat(tsA)%wnd(i,j)%wspd=9999.
  932                   wsdat(tsA)%par(i,j)%checked=-1
  933                END DO
  934             END DO
  935          END DO
 
 
                                                         w3strkmd.F90  page  18
 
 
  936                
  937 !        Assign to each line in partition file an entry in wsdat
  938 !        At each time step each point contains all numpart partitions.
  939 !        Only store the first 10 partitions.
  940          l = 1
  941               
  942          DO WHILE (l.LE.line)
  943             DO j = 1,maxJ
  944                DO i = 1,maxI
  945 !>042916                  IF ( (llat(l).EQ.mlat(i,j)).AND. &
  946 !>042916                       (llon(l).EQ.mlon(i,j)) ) THEN
  947                   IF ( (ABS(llat(l)-mlat(i,j)).LT.1.E-2).AND. &
  948                        (ABS(llon(l)-mlon(i,j)).LT.1.E-2) ) THEN
  949 !                    WRITE(20,*) 'MATCHED! ',l,&
  950 !                       llat(l),mlat(i,j),ABS(llat(l)-mlat(i,j)),&
  951 !                       llon(l),mlon(i,j),ABS(llon(l)-mlon(i,j))
  952                     wsdat(ts(l))%lat(i,j) = llat(l)
  953                     wsdat(ts(l))%lon(i,j) = llon(l)
  954 !                   --- Find ALL partition values associated with
  955 !                       lat(i,j) and lon(i,j). Keep list index l
  956 !                       fixed and recycle iline as variable index. ---
  957                     iline = l
  958                     k = 1
  959                     DO WHILE (                                         &
  960                        ABS(wsdat(ts(l))%lat(i,j)-llat(iline)).LT.1.E-3 &
  961                   .AND.ABS(wsdat(ts(l))%lon(i,j)-llon(iline)).LT.1.E-3 )
  962                       IF (k.LE.10) THEN
  963                        wsdat(ts(iline))%par(i,j)%ipart(k) = k
  964                        wsdat(ts(iline))%par(i,j)%hs(k)    = hs0(iline)
  965                        wsdat(ts(iline))%par(i,j)%tp(k)    = tp0(iline)
  966                        wsdat(ts(iline))%par(i,j)%dir(k)   = dir0(iline)
  967                        wsdat(ts(iline))%par(i,j)%dspr(k)  = dspr0(iline)
  968 !                       wsdat(ts(k))%par(i,j)%wf(k) = wf0(l)
  969                        IF (k.EQ.1) THEN
  970                         wsdat(ts(iline))%date = date0(iline)
  971                         wsdat(ts(iline))%wnd(i,j)%wdir = wndDir0(iline)
  972                         wsdat(ts(iline))%wnd(i,j)%wspd = wndSpd0(iline)
  973                         wsdat(ts(iline))%par(i,j)%checked = 0
  974                        END IF
  975                       END IF
  976                       k = k + 1
  977                       iline = iline + 1
  978                       if (iline.GT.line) EXIT
  979                     END DO
  980 !                   --- Account for increment at the end of loop (400 CON>
  981 !                       and go one element back in list because of increm>
  982                     l = iline-1
  983                     GOTO 400
  984                   END IF
  985                END DO
  986             END DO
  987  400        CONTINUE
  988             IF (l+1.le.line) THEN
  989              IF (ts(l).LT.ts(l+1)) THEN
  990               K = line-l
 
 
                                                         w3strkmd.F90  page  19
 
 
  991 !             --- With each time step completed, deallocate processed 1:l
  992 !                 elements from 1d array. Create a temporary array size of
  993 !                 (l+1:line) with k elements and reallocate original arra>
  994               IF (ALLOCATED(tmp_i4)) DEALLOCATE(tmp_i4)
  995 !             --- REALLOCATE(integer arrays) ---
  996               ALLOCATE(tmp_i4(k))
  997               tmp_i4(1:k) = ts((l+1):line)
  998               DEALLOCATE(ts)
  999               ALLOCATE(ts(k))
 1000               ts(1:k) = tmp_i4(1:k)
 1001               DEALLOCATE(tmp_i4)
 1002 !             --- REALLOCATE(double precision arrays) ---
 1003               IF (ALLOCATED(tmp_r8)) DEALLOCATE(tmp_r8)
 1004               ALLOCATE(tmp_r8(k))
 1005               tmp_r8(1:k) = date0((l+1):line)
 1006               DEALLOCATE(date0)
 1007               ALLOCATE(date0(k))
 1008               date0(1:k) = tmp_r8(1:k)
 1009               DEALLOCATE(tmp_r8)
 1010 !             --- REALLOCATE(single precision arrays) ---
 1011               IF (ALLOCATED(tmp_r4)) DEALLOCATE(tmp_r4)
 1012               ALLOCATE(tmp_r4(k))
 1013               tmp_r4(1:k) = llat((l+1):line)
 1014               DEALLOCATE(llat)
 1015               ALLOCATE(llat(k))
 1016               llat(1:k) = tmp_r4(1:k)
 1017               tmp_r4(1:k) = llon((l+1):line)
 1018               DEALLOCATE(llon)
 1019               ALLOCATE(llon(k))
 1020               llon(1:k) = tmp_r4(1:k)
 1021               tmp_r4(1:k) = hs0((l+1):line)
 1022               DEALLOCATE(hs0)
 1023               ALLOCATE(hs0(k))
 1024               hs0(1:k) = tmp_r4(1:k)
 1025               tmp_r4(1:k) = tp0((l+1):line)
 1026               DEALLOCATE(tp0)
 1027               ALLOCATE(tp0(k))
 1028               tp0(1:k) = tmp_r4(1:k)
 1029               tmp_r4(1:k) = dir0((l+1):line)
 1030               DEALLOCATE(dir0)
 1031               ALLOCATE(dir0(k))
 1032               dir0(1:k) = tmp_r4(1:k)
 1033               tmp_r4(1:k) = dspr0((l+1):line)
 1034               DEALLOCATE(dspr0)
 1035               ALLOCATE(dspr0(k))
 1036               dspr0(1:k) = tmp_r4(1:k)
 1037               tmp_r4(1:k) = wndSpd0((l+1):line)
 1038               DEALLOCATE(wndSpd0)
 1039               ALLOCATE(wndSpd0(k))
 1040               wndSpd0(1:k) = tmp_r4(1:k)
 1041               tmp_r4(1:k) = wndDir0((l+1):line)
 1042               DEALLOCATE(wndDir0)
 1043               ALLOCATE(wndDir0(k))
 1044               wndDir0(1:k) = tmp_r4(1:k)
 1045               DEALLOCATE(tmp_r4)
 
 
                                                         w3strkmd.F90  page  20
 
 
 1046               line = k
 1047               l = 0
 1048              END IF
 1049             END IF
 1050             l = l + 1
 1051          END DO
 1052 !
 1053          IF (ALLOCATED(ts)) DEALLOCATE(ts)
 1054          IF (ALLOCATED(llat)) DEALLOCATE(llat)
 1055          IF (ALLOCATED(llon)) DEALLOCATE(llon)
 1056          IF (ALLOCATED(mlat)) DEALLOCATE(mlat)
 1057          IF (ALLOCATED(mlon)) DEALLOCATE(mlon)
 1058          IF (ALLOCATED(date0)) DEALLOCATE(date0)
 1059          IF (ALLOCATED(hs0)) DEALLOCATE(hs0)
 1060          IF (ALLOCATED(tp0)) DEALLOCATE(tp0)
 1061          IF (ALLOCATED(dir0)) DEALLOCATE(dir0)
 1062          IF (ALLOCATED(dspr0)) DEALLOCATE(dspr0)
 1063 !         IF (ALLOCATED(wf0)) DEALLOCATE(wf0)
 1064          IF (ALLOCATED(wndSpd0)) DEALLOCATE(wndSpd0)
 1065          IF (ALLOCATED(wndDir0)) DEALLOCATE(wndDir0)
 1066                                                     
 1067                                                     
 1068                                                     
 1069 ! ----*** Test Output *** ----------------------------------------------->
 1070       IF (testout) THEN
 1071 !-----RAW PARTITION output: Coordinates
 1072       OPEN(unit=31,file='PART_COORD.OUT',status='unknown')
 1073                                                           
 1074       WRITE(31,*) 'Longitude ='
 1075       DO j = maxJ,1,-1
 1076          DO i = 1,maxI
 1077             WRITE(31,'(F7.2)',ADVANCE='NO') wsdat(1)%lon(i,j)
 1078          END DO
 1079          WRITE(31,'(A)',ADVANCE='YES') ''
 1080       END DO
 1081             
 1082       WRITE(31,*) 'Latitude = '
 1083       DO j = maxJ,1,-1
 1084          DO i = 1,maxI
 1085             WRITE(31,'(F7.2)',ADVANCE='NO') wsdat(1)%lat(i,j)
 1086          END DO
 1087          WRITE(31,'(A)',ADVANCE='YES') ''
 1088       END DO
 1089             
 1090       CLOSE(31)
 1091                
 1092 !-----RAW PARTITION output: hs
 1093       OPEN(unit=32, file='PART_HSIGN.OUT', &
 1094            status='unknown')
 1095                             
 1096       maxpartout = 5
 1097       DO tsA = 1,SIZE(wsdat)
 1098          WRITE(32,'(I4,71x,A)') tsA,'Time step'
 1099          WRITE(32,'(I4,71x,A)') maxpartout,'Tot number of raw partitions'
 1100          DO k = 1,maxpartout
 
 
                                                         w3strkmd.F90  page  21
 
 
 1101             WRITE(32,'(I4,71x,A)') k,'System number'
 1102             WRITE(32,'(I4,71x,A)') 9999,'Number of points in system'
 1103             DO j = maxJ,1,-1
 1104                DO i = 1,maxI
 1105                   WRITE(32,'(F8.2)',ADVANCE='NO') wsdat(tsA)%par(i,j)%hs(>
 1106                END DO
 1107                WRITE(32,'(A)',ADVANCE='YES') ''
 1108             END DO
 1109          END DO
 1110       END DO
 1111             
 1112       CLOSE(32)
 1113                
 1114 !-----RAW PARTITION output: tp
 1115 !      OPEN(unit=33,recl=2147483646, file='PART_TP.OUT', &
 1116 !           status='unknown')
 1117       OPEN(unit=33, file='PART_TP.OUT', &
 1118            status='unknown')
 1119                             
 1120       DO tsA = 1,SIZE(wsdat)
 1121          WRITE(33,'(I4,71x,A)') tsA,'Time step'
 1122          WRITE(33,'(I4,71x,A)') maxpartout,'Tot number of raw partitions'
 1123          DO k = 1,maxpartout
 1124             WRITE(33,'(I4,71x,A)') k,'System number'
 1125             WRITE(33,'(I4,71x,A)') 9999,'Number of points in system'
 1126             DO j = maxJ,1,-1
 1127                DO i = 1,maxI
 1128                   WRITE(33,'(F8.2)',ADVANCE='NO') wsdat(tsA)%par(i,j)%tp(>
 1129                END DO
 1130                WRITE(33,'(A)',ADVANCE='YES') ''
 1131             END DO
 1132          END DO
 1133       END DO
 1134             
 1135       CLOSE(33)
 1136                
 1137 !-----RAW PARTITION output: dir
 1138       OPEN(unit=34, file='PART_DIR.OUT', &
 1139            status='unknown')
 1140                             
 1141       DO tsA = 1,SIZE(wsdat)
 1142          WRITE(34,'(I4,71x,A)') tsA,'Time step'
 1143          WRITE(34,'(I4,71x,A)') maxpartout,'Tot number of raw partitions'
 1144          DO k = 1,maxpartout
 1145             WRITE(34,'(I4,71x,A)') k,'System number'
 1146             WRITE(34,'(I4,71x,A)') 9999,'Number of points in system'
 1147             DO j = maxJ,1,-1
 1148                DO i = 1,maxI
 1149                   WRITE(34,'(F8.2)',ADVANCE='NO') wsdat(tsA)%par(i,j)%dir>
 1150                END DO
 1151                WRITE(34,'(A)',ADVANCE='YES') ''
 1152             END DO
 1153          END DO
 1154       END DO
 1155             
 
 
                                                         w3strkmd.F90  page  22
 
 
 1156       CLOSE(34)
 1157                
 1158 !-----RAW PARTITION output: dspr
 1159       OPEN(unit=35, file='PART_DSPR.OUT', &
 1160            status='unknown')
 1161                             
 1162       DO tsA = 1,SIZE(wsdat)
 1163          WRITE(35,'(I4,71x,A)') tsA,'Time step'
 1164          WRITE(35,'(I4,71x,A)') maxpartout,'Tot number of raw partitions'
 1165          DO k = 1,maxpartout
 1166             WRITE(35,'(I4,71x,A)') k,'System number'
 1167             WRITE(35,'(I4,71x,A)') 9999,'Number of points in system'
 1168             DO j = maxJ,1,-1
 1169                DO i = 1,maxI
 1170                   WRITE(35,'(F8.2)',ADVANCE='NO') &
 1171                      wsdat(tsA)%par(i,j)%dspr(k)
 1172                END DO
 1173                WRITE(35,'(A)',ADVANCE='YES') ''
 1174             END DO
 1175          END DO
 1176       END DO
 1177             
 1178       CLOSE(35)
 1179       END IF
 1180             
 1181 ! ------------------------------------------------------------------------
 1182                                                                           
 1183 !        Allocate the sysA structure
 1184          WRITE(20,*) 'Allocating sysA...'
 1185          ALLOCATE( sysA(maxTs) )
 1186          WRITE(20,*) 'SIZE(sysA) = ',SIZE(sysA)
 1187          WRITE(6,1020) ' Number of time levels being processed:',SIZE(sys>
 1188  1020    FORMAT(A,I4)
 1189                      
 1190 !        Allocate maxSys
 1191          ALLOCATE( maxSys(maxTs) )
 1192       ELSE
 1193 !        Raw partitioning data from wave model memory, via the array wsda>
 1194 !        Set maxTs to the time step to compute: 1=first time step, 2=othe>
 1195          maxTs = tmax
 1196          t0 = tcur
 1197                   
 1198 !        Allocate the sysA structure
 1199          ALLOCATE( sysA(1) )                                             >
 1200 !        Allocate maxSys
 1201          ALLOCATE( maxSys(1) )                                           >
 1202       END IF
 1203             
 1204 !     Big loop over all time levels
 1205          WRITE(6,*) 'Performing spatial tracking...'
 1206       DO tsA = t0,maxTs
 1207                        
 1208           WRITE(20,*) 'Call spiralTrackV3, tsA=',tsA,'...'
 1209           CALL spiralTrackV3 ( wsdat(tsA), dirKnob, perKnob, wetPts, &
 1210                                hsKnob, seedLat, seedLon, &
 
 
                                                         w3strkmd.F90  page  23
 
 
 1211                                maxSys(tsA), sysA(tsA)%sys )
 1212                                                            
 1213           WRITE(20,*) '*** SIZE(sysA(1:tsA)%sys) at end of time step', &
 1214                        tsA,':'
 1215           WRITE(20,*) SIZE(sysA(tsA)%sys)
 1216       END DO
 1217             
 1218             
 1219       WRITE(6,*) 'Performing temporal tracking...'
 1220       WRITE(20,*) 'Calling timeTrackingV2...'
 1221       lonext = wsdat(1)%lon(maxI,1)-wsdat(1)%lon(1,1)
 1222       latext = wsdat(1)%lat(1,maxJ)-wsdat(1)%lat(1,1)
 1223                                                      
 1224       CALL timeTrackingV2 (sysA, maxSys, tpTimeKnob, dirTimeKnob, 1, &
 1225                            maxGroup, dt, lonext, latext, maxI, maxJ)
 1226 !
 1227       RETURN
 1228 !
 1229   802 CONTINUE
 1230       WRITE (6,990) IOERR
 1231       STOP 1
 1232             
 1233   990 FORMAT (/' *** WAVEWATCH III ERROR IN W3STRKMD : '/            &
 1234                '     ERROR IN READING FROM PARTITION FILE'/          &
 1235                '     IOSTAT =',I5/)
 1236  1000 FORMAT (F9.0,F7.0,F8.3,F8.3,14X,I3,7X,F5.1,F6.1,F5.1,F6.1)
 1237  1010 FORMAT (3X,F8.2,F8.2,8X,F9.2,F9.2)
 1238  1200 FORMAT (/' *** WAVEWATCH III ERROR IN W3STRKMD : '/            &
 1239                '     ERROR IN READING PARTITION FILE   '/            &
 1240                '     INCOMPATIBLE ENDIANESS'/                        )
 1241  1300 FORMAT (/' *** WAVEWATCH III ERROR IN W3STRKMD : '/            &
 1242         '     ERROR IN READING PARTITION FILE   '/                   &
 1243         '     EXPECTED IDSTR "WAVEWATCH III PARTITIONED DATA FILE"'/ )
 1244  1400 FORMAT (/' *** WAVEWATCH III ERROR IN W3STRKMD : '/            &
 1245                '     ERROR IN FINDING DOMAIN TO PROCESS - '/         &
 1246                '     SPECIFIED LAT/LON LIMITS WITHIN DOMAIN '/       &
 1247                '     OF RAW PARTITION FILE?'/                        )
 1248  2001 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/            &
 1249                '     ERROR IN OPENING INPUT FILE'/                   )
 1250  2002 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/            &
 1251                '     PREMATURE END OF INPUT FILE'/                   )
 1252  2003 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/            &
 1253                '     PREMATURE END OF PARTITION FILE - '/            &
 1254                '     TSTART=',F13.4/                                 )
 1255 !
 1256       END SUBROUTINE waveTracking_NWS_V2
 1257 !/ End of waveTracking_NWS_V2 ---------------------------------------- /
 1258 !/
 1259 !/ ------------------------------------------------------------------- /
 1260       SUBROUTINE spiralTrackV3 (wsdat   ,dirKnob ,perKnob ,wetPts  , &
 1261                                 hsKnob  ,seedLat ,seedLon  , &
 1262                                 maxSys  ,sys   )
 1263 !/
 1264 !/                  +-----------------------------------+
 1265 !/                  | WAVEWATCH III           NOAA/NCEP |
 
 
                                                         w3strkmd.F90  page  24
 
 
 1266 !/                  |     A. J. van der Westhuysen      |
 1267 !/                  |            Jeff Hanson            |
 1268 !/                  |        Eve-Marie Devaliere        |
 1269 !/                  |                        FORTRAN 95 |
 1270 !/                  | Last update :          4-Jan-2013 |
 1271 !/                  +-----------------------------------+
 1272 !/
 1273 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 1274 !/                  by Jeff Hanson & Eve-Marie Devaliere
 1275 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 1276 !/
 1277 !/    Copyright 2009-2013 National Weather Service (NWS),
 1278 !/       National Oceanic and Atmospheric Administration.  All rights
 1279 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 1280 !/       No unauthorized use without permission.
 1281 !/
 1282       IMPLICIT NONE
 1283 !
 1284 !  1. Purpose :
 1285 !
 1286 !     Performs the spatial spiral tracking for a given time step
 1287 !
 1288 !  2. Method
 1289 !
 1290 !     Index convention on grid:
 1291 !
 1292 !      j
 1293 !      ^
 1294 !      |+(1,maxJ)          +(maxI,maxJ)
 1295 !      |
 1296 !      |
 1297 !      |
 1298 !      |
 1299 !      |
 1300 !      |
 1301 !      |(1,1)              +(maxI,1)
 1302 !      +----------------------> i
 1303 !
 1304 !  3. Parameters :
 1305 !
 1306 !     Parameter list
 1307 !     ----------------------------------------------------------------
 1308 !     dirKnob     Real     input  Parameter in direction for combining fi>
 1309 !     perKnob     Real     input  Parameter in period for combining field>
 1310 !     wetPts      Real     input  Percentage of wet points for purging fi>
 1311 !     hsKnob      Real     input  Parameter in wave height for purging fi>
 1312 !     seedLat     Real     input  Start Lat for tracking spiral (if =0 ce>
 1313 !     seedLon     Real     input  Start Lon for tracking spiral (if =0 ce>
 1314 !     wsdat       Real arr output Input 2d (gridded) data structure to be>
 1315 !     maxSys      Int      output Maximum number of partition systems
 1316 !     sys     Type(system) output Final set of tracked systems, for one t>
 1317 !
 1318       TYPE(dat2d)  :: wsdat
 1319       REAL         :: dirKnob,perKnob,wetPts,hsKnob,seedLat,seedLon
 1320       INTEGER      :: maxSys
 
 
                                                         w3strkmd.F90  page  25
 
 
 1321       TYPE(system), POINTER :: sys(:)
 1322                                      
 1323       INTENT (IN) wetPts,dirKnob,perKnob,hsKnob,seedLat,seedLon
 1324       INTENT (IN OUT) wsdat
 1325 !      INTENT (OUT) maxSys,sys
 1326 !
 1327 !     Local variables
 1328 !     ----------------------------------------------------------------
 1329 !     ngbrExt    Int    How far do we want the neighbour to be considered
 1330 !     combine    Int    Toggle (1=combine wave systems; 0=do not combine)
 1331 !     maxI,MaxJ  Int    Dimensions of the 2d (gridded) data wsdat
 1332 !     deltaLat   Real   Delta in kilometers between 2 pts (in latitude)
 1333 !
 1334       LOGICAL   :: first
 1335       CHARACTER :: way *1
 1336       INTEGER   :: ngbrExt, combine, maxI, maxJ, i, j, oldJ
 1337       INTEGER   :: horizStepCount, vertStepCount, checkCount, sc, &
 1338                    maxPts, landPts, horizBorder, vertBorder, m, k, &
 1339                    stepCount
 1340       REAL      :: deltaLat, minLat, maxLat, minLon, maxLon
 1341 !
 1342 !  4. Subroutines used :
 1343 !
 1344 !      Name      Type  Module   Description
 1345 !     ----------------------------------------------------------------
 1346 !      findWay
 1347 !      findNext
 1348 !      findSys
 1349 !      combineWaveSystems
 1350 !
 1351 !  5. Subroutines calling
 1352 !
 1353 !     waveTracking_NWS_V2
 1354 !
 1355 !  6. Error messages :
 1356 !
 1357 !  7. Remarks :
 1358 !
 1359 !  8. Structure :
 1360 !
 1361 !     -
 1362 !
 1363 !  9. Switches :
 1364 !
 1365 !     None defined yet.
 1366 !
 1367 ! 10. Source code :
 1368 !
 1369 !/ ------------------------------------------------------------------- /
 1370                                                                         
 1371 ! Routine starts by identifying the starting point. Choose the 'center' o>
 1372 !     Set the search distance for neighbors:
 1373 !     1: 1 row and column out, i.e. the 8 neighbors around the current po>
 1374 !     2: 2 rows and columns out... etc.
 1375       ngbrExt=1
 
 
                                                         w3strkmd.F90  page  26
 
 
 1376       combine=1
 1377       WRITE(20,*) 'In spiralTrackV3: combine = ',combine
 1378                                                         
 1379       maxI = wsdat%maxi
 1380       maxJ = wsdat%maxj
 1381       IF ( (seedLat.EQ.0).OR.(seedLon.EQ.0) ) THEN
 1382          i=NINT(REAL(maxI)/2.)
 1383          j=NINT(REAL(maxJ)/2.)
 1384          WRITE(20,*) 'In spiralTrackV3, i=NINT(maxI/2.) =',i
 1385          WRITE(20,*) 'In spiralTrackV3, j=NINT(maxJ/2.) =',j
 1386       ELSE
 1387          i=1
 1388          j=1
 1389          DO WHILE ( (wsdat%lat(1,j).LT.seedLat).AND.(j.LT.wsdat%maxj) )  >
 1390             j=j+1
 1391          END DO
 1392          DO WHILE ( (wsdat%lon(i,1).LT.seedLon).AND.(i.LT.wsdat%maxi) )
 1393             i=i+1
 1394          END DO
 1395       END IF
 1396 !     In case center point is land point...
 1397       IF (wsdat%par(i,j)%checked.EQ.-1) THEN
 1398           oldJ=j
 1399           DO WHILE (wsdat%par(i,j)%checked.EQ.-1)
 1400               j=j+1
 1401               IF (j.EQ.maxJ) THEN
 1402                   j=oldJ
 1403                   i=i+1
 1404                   oldJ=oldJ+1
 1405               END IF
 1406           END DO
 1407       END IF
 1408 !     Compute distance in km between 2 grid points (at equator)
 1409       deltaLat=(wsdat%lat(i,j)-wsdat%lat(i,j-1))*111.18
 1410                                                        
 1411 !     Starts the spiral
 1412 !     Intitiate variables
 1413       horizStepCount=0
 1414       vertStepCount=0
 1415       way='R'
 1416       first=.TRUE.
 1417       checkCount=1
 1418       maxSys=0
 1419       landPts=0
 1420                
 1421       minLat=MINVAL(wsdat%lat)
 1422       maxLat=MAXVAL(wsdat%lat)
 1423       minLon=MINVAL(wsdat%lon)
 1424       maxLon=MAXVAL(wsdat%lon)
 1425                               
 1426       horizBorder=0
 1427       vertBorder=0
 1428       DO WHILE (checkCount.LE.(maxI*maxJ-3) )
 1429 !        From the direction (way) we were going before, find which direct>
 1430 !        are going now and how many 'step' we need to take
 
 
                                                         w3strkmd.F90  page  27
 
 
 1431          CALL findWay(way, horizStepCount, vertStepCount, &
 1432                       vertBorder, horizBorder, stepCount)
 1433          IF (first) THEN
 1434            m=0
 1435            DO k=1,LENGTH(wsdat%par(i,j)%hs, &
 1436                          SIZE(wsdat%par(i,j)%hs),9999.)
 1437                IF ( (wsdat%par(i,j)%hs(k).EQ.0.).AND. &
 1438                     (wsdat%par(i,j)%tp(k).EQ.0.) ) THEN
 1439                   wsdat%par(i,j)%sys(k)=-1
 1440                ELSE
 1441                   m=m+1
 1442                   wsdat%par(i,j)%sys(k)=m
 1443                END IF
 1444            END DO
 1445                  
 1446            wsdat%par(i,j)%checked=1
 1447            checkCount=checkCount+1
 1448            first=.FALSE.
 1449          END IF
 1450          DO sc = 1, stepCount
 1451              CALL findNext (i,j,maxI,maxJ,way,vertBorder,horizBorder)
 1452              IF ( wsdat%par(i,j)%checked.EQ.-1 ) THEN
 1453 !            Land point is one of our grid points, so we need to update c>
 1454                  checkCount=checkCount+1
 1455                  landPts=landPts+1
 1456 !                So that we don't count the land points twice....
 1457                  wsdat%par(i,j)%checked=-2
 1458              ELSE IF ( wsdat%par(i,j)%checked.EQ.0 ) THEN
 1459 !            Hasn't been checked yet and is not land point
 1460                  checkCount=checkCount+1
 1461                  CALL findSys(i, j, wsdat, maxSys, ngbrExt, maxI, maxJ, &
 1462                               perKnob, dirKnob, hsKnob)
 1463              END IF
 1464          END DO
 1465       END DO
 1466 !     wetPts% of wet points
 1467       maxPts=NINT(wetPts*(maxI*maxJ-1))
 1468 !
 1469       WRITE(20,*) 'Call combineWaveSystems...'
 1470       CALL combineWaveSystems(wsdat,maxSys,maxPts,maxI,maxJ, &
 1471                               perKnob,dirKnob,hsKnob,combine,sys)
 1472                                                                  
 1473       RETURN
 1474       END SUBROUTINE spiralTrackV3
 1475 !/ End of spiralTrackV3 ---------------------------------------------- /
 1476 !/
 1477 !/ ------------------------------------------------------------------- /
 1478       SUBROUTINE timeTrackingV2 (sysA       ,maxSys     ,tpTimeKnob  , &
 1479                                  dirTimeKnob,ts0        ,maxGroup    , &
 1480                                  dt         ,lonext     ,latext      , &
 1481                                  maxI       ,maxJ                    )
 1482 !/
 1483 !/                  +-----------------------------------+
 1484 !/                  | WAVEWATCH III           NOAA/NCEP |
 1485 !/                  |     A. J. van der Westhuysen      |
 
 
                                                         w3strkmd.F90  page  28
 
 
 1486 !/                  |            Jeff Hanson            |
 1487 !/                  |        Eve-Marie Devaliere        |
 1488 !/                  |                        FORTRAN 95 |
 1489 !/                  | Last update :          4-Jan-2013 |
 1490 !/                  +-----------------------------------+
 1491 !/
 1492 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 1493 !/                  by Jeff Hanson & Eve-Marie Devaliere
 1494 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 1495 !/
 1496 !/    Copyright 2009-2013 National Weather Service (NWS),
 1497 !/       National Oceanic and Atmospheric Administration.  All rights
 1498 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 1499 !/       No unauthorized use without permission.
 1500 !/
 1501       IMPLICIT NONE
 1502 !
 1503 !  1. Purpose :
 1504 !
 1505 !     Performs the time tracking of the systems identified within
 1506 !     the subroutine spiralTrackV3.
 1507 !
 1508 !  2. Method
 1509 !
 1510 !     -
 1511 !
 1512 !  3. Parameters :
 1513 !
 1514 !     Parameter list
 1515 !     ----------------------------------------------------------------
 1516 !     Note: perKnob, dirKnob in Matlab version replaced by tpTimeKnob, di>
 1517 !
 1518 !     sysA        TYPE(timsys) in/out Final set of spatially and temporal>
 1519 !     dirTimeKnob    Real      input  Parameter in direction for combinin>
 1520 !     tpTimeKnob     Real      input  Parameter in period for combining f>
 1521 !     ts0            Int       input  Time step to which default grp valu>
 1522 !     maxSys         Int arr   input  Total number of systems per time le>
 1523 !     maxGroup       Int       output Maximum number of wave systems ("gr>
 1524 !     lonext         Real      input  Longitudinal extent of domain
 1525 !     latext         Real      input  Latitudinal extent of domain
 1526 !     maxI, maxJ     Int       input  Maximum indices of wave field
 1527 !
 1528       TYPE(timsys), POINTER :: sysA(:)
 1529       INTEGER, POINTER :: maxSys(:)
 1530       REAL         :: dirTimeKnob, tpTimeKnob
 1531       INTEGER      :: ts0, maxGroup
 1532       REAL         :: dt
 1533       REAL         :: lonext, latext
 1534       INTEGER      :: maxI, maxJ
 1535                                 
 1536       INTENT (IN) tpTimeKnob, dirTimeKnob, ts0, maxI, maxJ
 1537 !      INTENT (IN OUT) sysA
 1538       INTENT (OUT) maxGroup
 1539 !
 1540 !     Local variables
 
 
                                                         w3strkmd.F90  page  29
 
 
 1541 !     ----------------------------------------------------------------
 1542 !     ic        Int   Counter for wave systems
 1543 !     ts1       Int   Adjusted initial time step in case ts0 has only emp>
 1544 !
 1545       LOGICAL :: file_exists
 1546       CHARACTER :: dummy*23
 1547       TYPE(sysmemory) :: sysMem(50)
 1548       INTEGER :: leng, l, i, ii, j, k, kk, idir, numSys, &
 1549                  counter, new, DIFSIZE, tpMinInd, dirMinInd, used, ok
 1550       REAL    :: Tb,  deltaPer, deltaDir, tpMinVal, dirMinVal, &
 1551                  dirForTpMin, tpForDirMin
 1552       REAL, ALLOCATABLE :: sysOrdered(:), TEMP(:), dirs(:)
 1553       REAL, POINTER :: DIFARR(:)
 1554       INTEGER, ALLOCATABLE :: indSorted(:), alreadyUsed(:), allInd(:)
 1555       INTEGER, ALLOCATABLE :: ind(:), ind2(:)
 1556       INTEGER :: ts1
 1557       REAL, ALLOCATABLE :: GOF(:,:), GOFMinVal(:), GOFMinInd(:), &
 1558                            Tbsysmem(:), deltaDirsysmem(:), &
 1559                            deltaPersysmem(:),m1sysmem(:),m2sysmem(:)
 1560       REAL    :: m1, m2
 1561       REAL    :: lonmean, latmean, dmndiag
 1562       INTEGER :: npnts, npnts2
 1563       REAL, ALLOCATABLE :: mnlonlist(:), mnlatlist(:), mndist(:)
 1564       REAL, POINTER     :: dummy1(:),dummy2(:),dummy3(:)
 1565       INTEGER, ALLOCATABLE :: olsize(:)
 1566       REAL    :: TEMP1, TEMP2
 1567       INTEGER :: iii, jj, ll, idup
 1568 !
 1569 !  4. Subroutines used :
 1570 !
 1571 !      Name      Type  Module   Description
 1572 !     ----------------------------------------------------------------
 1573 !      SORT
 1574 !      SETDIFF
 1575 !
 1576 !  5. Subroutines calling
 1577 !
 1578 !     waveTracking_NWS_V2
 1579 !
 1580 !  6. Error messages :
 1581 !
 1582 !  7. Remarks :
 1583 !
 1584 !  8. Structure :
 1585 !
 1586 !     -
 1587 !
 1588 !  9. Switches :
 1589 !
 1590 !     None defined yet.
 1591 !
 1592 ! 10. Source code :
 1593 !
 1594 !/ ------------------------------------------------------------------- /
 1595                                                                         
 
 
                                                         w3strkmd.F90  page  30
 
 
 1596 !     Associate default grp value to time step 1
 1597       WRITE(20,*) 'TIME TRACKING'
 1598       WRITE(20,*) 'Inside timeTrackingV2: SIZE(sysA(1)%sys) =', &
 1599                     SIZE(sysA(1)%sys)
 1600       WRITE(20,*) 'Inside timeTrackingV2: maxSys(1) =',maxSys(1)
 1601       WRITE(20,*) 'ts0 = ',ts0
 1602                               
 1603       ts1 = ts0
 1604                
 1605 !     Skip initial time steps with empty systems (e.g. when starting from>
 1606       DO i = ts1, SIZE(sysA)
 1607          IF (SIZE(sysA(ts1)%sys).EQ.0) ts1 = ts1+1
 1608 !        No non-empty systems found
 1609          IF (ts1.GT.SIZE(sysA)) THEN
 1610             maxGroup = 0
 1611             GOTO 2000
 1612          END IF
 1613       END DO
 1614       WRITE(20,*) 'TS = ',ts1
 1615                              
 1616       IF (SIZE(sysA(ts1)%sys).GT.0) THEN
 1617 !        Initialize system memory groups
 1618          sysA(ts1)%sys(:)%grp = 9999
 1619          sysMem(:)%grp = 9999
 1620          sysMem(:)%nPoints = 0
 1621          sysMem(:)%lonMean = 9999.
 1622          sysMem(:)%latMean = 9999.
 1623          sysMem(:)%tpMean = 9999.
 1624          sysMem(:)%dirMean = 9999.
 1625          sysMem(:)%updated = -9999
 1626          sysMem(:)%length = 0
 1627          DO iii = 1,50
 1628             ALLOCATE(sysMem(iii)%indx(maxI*maxJ))
 1629             sysMem(iii)%indx = 9999
 1630          END DO
 1631                
 1632          INQUIRE(FILE="sys_restart.ww3", EXIST=file_exists)
 1633          IF (file_exists) THEN
 1634 !           Use groups from wave tracking hotfile
 1635             WRITE(20,*) '*** Using group memory hotfile'
 1636             OPEN(unit=12,file='sys_restart.ww3',status='old')
 1637             READ(12,'(A23,I10)') dummy,maxGroup
 1638             WRITE(20,*) 'Reading ',maxGroup,' systems'
 1639             DO k = 1,maxGroup
 1640                READ(12,'(A23,I10)') dummy,sysMem(k)%grp
 1641                READ(12,'(A23,I10)') dummy,sysMem(k)%nPoints
 1642                READ(12,'(A23,F10.4)') dummy,sysMem(k)%lonMean
 1643                READ(12,'(A23,F10.4)') dummy,sysMem(k)%latMean
 1644                READ(12,'(A23,F10.3)') dummy,sysMem(k)%tpMean
 1645                READ(12,'(A23,F10.3)') dummy,sysMem(k)%dirMean
 1646                READ(12,'(A23,I10)') dummy,sysMem(k)%updated
 1647                READ(12,'(A23,I10)') dummy,sysMem(k)%length
 1648                DO j = maxJ,1,-1
 1649                   READ(12,*) (sysMem(k)%indx((j-1)*maxI+i), i = 1,maxI)
 1650                END DO
 
 
                                                         w3strkmd.F90  page  31
 
 
 1651                !Reset update counter
 1652                sysMem(k)%updated = 0
 1653             END DO
 1654             CLOSE(12)
 1655             ts1 = ts1-1
 1656          ELSE
 1657 !        Set up the group number array for the first time level to be tra>
 1658          ALLOCATE( sysOrdered(maxSys(ts1)) )
 1659          ALLOCATE( indSorted(maxSys(ts1)) )
 1660          CALL SORT (REAL(sysA(ts1)%sys(1:maxSys(ts1))%nPoints), &
 1661                     maxSys(ts1),sysOrdered,indSorted,'D')
 1662          sysA(ts1)%sys(1:maxSys(ts1)) = sysA(ts1)%sys(indSorted)
 1663          IF (ALLOCATED(sysOrdered)) DEALLOCATE(sysOrdered)
 1664          IF (ALLOCATED(indSorted)) DEALLOCATE(indSorted)
 1665                                                         
 1666 !        Set the initial long-term system memory
 1667          DO i = 1, maxSys(ts1)
 1668             sysA(ts1)%sys(i)%grp = i
 1669 !           Set initial values of long-term system memory
 1670             sysMem(i)%grp = i
 1671             sysMem(i)%nPoints = sysA(ts1)%sys(i)%nPoints
 1672             sysMem(i)%lonMean = &
 1673                SUM(sysA(ts1)%sys(i)%lon(1:sysMem(i)%nPoints))/&
 1674                sysMem(i)%nPoints
 1675             sysMem(i)%latMean = &
 1676                SUM(sysA(ts1)%sys(i)%lat(1:sysMem(i)%nPoints))/&
 1677                sysMem(i)%nPoints
 1678 !070512----------- Weight averages with Hm0 ---------------------
 1679             TEMP1 = 0.
 1680             TEMP2 = 0.
 1681             DO iii = 1,sysMem(i)%nPoints
 1682                TEMP1 = TEMP1 + &
 1683                   (sysA(ts1)%sys(i)%hs(iii)**2)*sysA(ts1)%sys(i)%lon(iii)
 1684                TEMP2 = TEMP2 + &
 1685                   (sysA(ts1)%sys(i)%hs(iii)**2)*sysA(ts1)%sys(i)%lat(iii)
 1686             END DO
 1687             sysMem(i)%lonMean = TEMP1/&
 1688                   MAX(SUM(sysA(ts1)%sys(i)%hs(1:sysMem(i)%nPoints)**2),&
 1689                   0.001)
 1690             sysMem(i)%latMean = TEMP2/&
 1691                   MAX(SUM(sysA(ts1)%sys(i)%hs(1:sysMem(i)%nPoints)**2),&
 1692                   0.001)
 1693 !070512----------- Weight averages with Hm0 ---------------------
 1694             sysMem(i)%tpMean = sysA(ts1)%sys(i)%tpMean
 1695             sysMem(i)%dirMean = sysA(ts1)%sys(i)%dirMean
 1696             sysMem(i)%updated = ts1
 1697             sysMem(i)%length = 1
 1698 !071012----------- Grid point indexing --------------------------
 1699             DO iii = 1,sysMem(i)%nPoints
 1700                sysMem(i)%indx(iii) = (sysA(ts1)%sys(i)%j(iii)-1)*maxI +&
 1701                                       sysA(ts1)%sys(i)%i(iii)
 1702             END DO
 1703 !071012----------- Grid point indexing --------------------------
 1704          END DO
 1705          maxGroup = maxSys(ts1)
 
 
                                                         w3strkmd.F90  page  32
 
 
 1706 !         i = ts1
 1707          END IF
 1708                
 1709 !******** Test output ***********************
 1710          DO i = 1, maxGroup
 1711             WRITE(20,*) 'sysMem(',i,')%grp =',sysMem(i)%grp
 1712             WRITE(20,*) 'sysMem(',i,')%nPoints =',sysMem(i)%nPoints
 1713             WRITE(20,*) 'sysMem(',i,')%lonMean =',sysMem(i)%lonMean
 1714             WRITE(20,*) 'sysMem(',i,')%latMean =',sysMem(i)%latMean
 1715             WRITE(20,*) 'sysMem(',i,')%tpMean =',sysMem(i)%tpMean
 1716             WRITE(20,*) 'sysMem(',i,')%dirMean =',sysMem(i)%dirMean
 1717             WRITE(20,*) 'sysMem(',i,')%updated =',sysMem(i)%updated
 1718             WRITE(20,*) 'sysMem(',i,')%length =',sysMem(i)%length
 1719          END DO
 1720 !********************************************
 1721       END IF
 1722             
 1723 !     Loop over all time levels to track systems in time
 1724       WRITE(20,*) 'Number of time levels = ',SIZE(sysA)
 1725       DO i = (ts1+1), SIZE(sysA)
 1726          WRITE(20,*) 'TS = ',i
 1727                               
 1728          IF (SIZE(sysA(i)%sys).GT.0) THEN
 1729 !           *** Added: 02/29/12 *************************************
 1730 !           Sort groups, so that larger systems get associated first
 1731             ALLOCATE( sysOrdered(maxSys(i)) )
 1732             ALLOCATE( indSorted(maxSys(i)) )
 1733             CALL SORT (REAL(sysA(i)%sys(1:maxSys(i))%nPoints), &
 1734                     maxSys(i),sysOrdered,indSorted,'D')
 1735             sysA(i)%sys(1:maxSys(i)) = sysA(i)%sys(indSorted)
 1736             IF (ALLOCATED(sysOrdered)) DEALLOCATE(sysOrdered)
 1737             IF (ALLOCATED(indSorted)) DEALLOCATE(indSorted)
 1738 !           *** Added: 02/29/12 *************************************
 1739                                                                      
 1740 !           Initialize groups                                            >
 1741             sysA(i)%sys(:)%grp = 9999                                    >
 1742             counter = 0
 1743             leng = LENGTH(REAL(sysMem(:)%grp), &
 1744                           SIZE(sysMem(:)%grp),REAL(9999))
 1745             ALLOCATE( alreadyUsed(leng+10) )                             >
 1746             WRITE(20,*) 'sysMem(1:leng)%grp =', &
 1747                         sysMem(1:leng)%grp
 1748             ALLOCATE( allInd(leng) )
 1749             alreadyUsed(:) = 0
 1750             allInd(:) = sysMem(1:leng)%grp
 1751                                           
 1752 !071212-----GoF 2D-------------------------------
 1753          ALLOCATE( ind(SIZE(allInd)) )
 1754          ind(:) = allInd
 1755          ALLOCATE( ind2(SIZE(ind)) )
 1756          DO ii = 1, SIZE(ind)
 1757             ind2(ii) = FINDFIRST(REAL(allInd),SIZE(allInd), &
 1758                                  REAL(ind(ii)))
 1759          END DO
 1760 !        Define 2D array for evaluating best fit for systems
 
 
                                                         w3strkmd.F90  page  33
 
 
 1761          ALLOCATE( GOF(maxSys(i),maxGroup) )
 1762          ALLOCATE( GOFMinVal(maxGroup) )
 1763          ALLOCATE( GOFMinInd(maxGroup) )
 1764          ALLOCATE( Tbsysmem(maxGroup) )
 1765          ALLOCATE( deltaDirsysmem(maxGroup) )
 1766          ALLOCATE( deltaPersysmem(maxGroup) )
 1767          ALLOCATE( m1sysmem(maxGroup) )
 1768          ALLOCATE( m2sysmem(maxGroup) )
 1769 !071212-----GoF 2D-------------------------------
 1770          DO j = 1, maxSys(i)
 1771             npnts = sysA(i)%sys(j)%nPoints
 1772             lonmean = SUM(sysA(i)%sys(j)%lon(1:npnts))/npnts
 1773             latmean = SUM(sysA(i)%sys(j)%lat(1:npnts))/npnts
 1774 !070512----------- Weight averages with Hm0 ---------------------
 1775             TEMP1 = 0.
 1776             TEMP2 = 0.
 1777             DO iii = 1,npnts
 1778                TEMP1 = TEMP1 + &
 1779                   (sysA(i)%sys(j)%hs(iii)**2)*sysA(i)%sys(j)%lon(iii)
 1780                TEMP2 = TEMP2 + &
 1781                   (sysA(i)%sys(j)%hs(iii)**2)*sysA(i)%sys(j)%lat(iii)
 1782             END DO
 1783             lonmean=TEMP1/MAX(SUM(sysA(i)%sys(j)%hs(1:npnts)**2),0.001)
 1784             latmean=TEMP2/MAX(SUM(sysA(i)%sys(j)%hs(1:npnts)**2),0.001)
 1785 !070512----------- Weight averages with Hm0 ---------------------
 1786 !071012----------- Grid point indexing --------------------------
 1787             ALLOCATE(sysA(i)%sys(j)%indx(maxI*maxJ))
 1788             sysA(i)%sys(j)%indx = 9999
 1789             DO iii = 1,sysA(i)%sys(j)%nPoints
 1790                sysA(i)%sys(j)%indx(iii) = &
 1791                   (sysA(i)%sys(j)%j(iii)-1)*maxI + &
 1792                    sysA(i)%sys(j)%i(iii)
 1793             END DO
 1794 !071012----------- Grid point indexing --------------------------
 1795             WRITE(20,*) 'System no. ',j,' of ',maxSys(i)
 1796             WRITE(20,*) 'Size =', npnts
 1797             WRITE(20,*) 'lonMean =', lonmean
 1798             WRITE(20,*) 'latMean =', latmean
 1799             WRITE(20,*) 'tpMean =', sysA(i)%sys(j)%tpMean
 1800             WRITE(20,*) 'dirMean =', sysA(i)%sys(j)%dirMean
 1801             sysA(i)%sys(j)%grp = 9999                                    >
 1802                                                                           
 1803 !           Compute deltas
 1804             Tbsysmem = sysMem(1:maxGroup)%tpMean
 1805             WRITE(20,*) 'Tbsysmem(:) =       ', Tbsysmem(:)
 1806 !           Compute deltas the same way as for field combining - they sho>
 1807 !           be of the same degree of strictness as the latter, otherwise
 1808 !           the time combining will lose track!
 1809 !3stddev            m1 = -3.645*Tb + 63.211
 1810 !3stddev            m1 = MAX(m1,10.)
 1811 !3stddev            m2 = -0.346*Tb + 3.686
 1812 !3stddev            m2 = MAX(m2,0.6)
 1813 !1stddev            m1 = -2.219*Tb + 35.734
 1814 !1stddev            m1 = MAX(m1,5.)
 1815 !1stddev            m2 = -0.226*Tb + 2.213
 
 
                                                         w3strkmd.F90  page  34
 
 
 1816 !1stddev            m2 = MAX(m2,0.35)
 1817 !071412            m1 = -5.071*Tb + 90.688
 1818 !071412            m1 = MAX(m1,16.)
 1819 !071412            m2 = -0.467*Tb + 5.161
 1820 !071412            m2 = MAX(m2,1.0)
 1821 !071412            deltaDir = (m1*1. + dirTimeKnob)*1.
 1822 !071412            deltaPer = (m2*1. + tpTimeKnob)*1.
 1823             DO ii = 1,SIZE(ind2)
 1824                m1sysmem(ii) = MAX((-3.645*Tbsysmem(ii)+63.211),10.)
 1825                m2sysmem(ii) = MAX((-0.346*Tbsysmem(ii)+3.686),0.6)
 1826             END DO
 1827             deltaDirsysmem = m1sysmem(:)*1. + dirTimeKnob
 1828             deltaPersysmem = m2sysmem(:)*1. + tpTimeKnob
 1829             WRITE(20,*) 'deltaDirsysmem(:) = ',deltaDirsysmem
 1830             WRITE(20,*) 'deltaPersysmem(:) = ',deltaPersysmem
 1831                                                              
 1832 !                 Criterion 1: Mean period
 1833                   ALLOCATE( TEMP(SIZE(ind2)) )
 1834                   TEMP = ABS( sysA(i)%sys(j)%tpMean - &
 1835                               sysMem(ind2(:))%tpMean )
 1836                   WRITE(20,*) 'tpMean list =', &
 1837                               sysMem(ind2(:))%tpMean
 1838                   WRITE(20,*) 'tpMinVal list =', TEMP
 1839                   tpMinVal = MINVAL(TEMP)
 1840                   tpMinInd = FINDFIRST(TEMP,SIZE(TEMP),tpMinVal)
 1841                                                                 
 1842 !                 Criterion 2: Mean direction
 1843                   ALLOCATE( dirs(SIZE(ind2)) )
 1844                   dirs(:)=ABS( sysA(i)%sys(j)%dirMean - &
 1845                                sysMem(ind2(:))%dirMean )
 1846 !                 Deal with wrap around
 1847                   DO idir = 1, SIZE(dirs)
 1848                      IF (dirs(idir).GE.180.) dirs(idir)=360-dirs(idir)
 1849                   END DO
 1850                   WRITE(20,*) 'dirMean list =', &
 1851                               sysMem(ind2(:))%dirMean
 1852                   WRITE(20,*) 'dirMinVal list =', dirs
 1853                                                       
 1854 !                 Criterion 3: Size
 1855                   WRITE(20,*) 'Size list =', &
 1856                               sysMem(ind2(:))%nPoints
 1857                                                      
 1858 !                 Criterion 4: Distance between systems
 1859                   ALLOCATE (mnlonlist(SIZE(ind2)))
 1860                   ALLOCATE (mnlatlist(SIZE(ind2)))
 1861                   ALLOCATE (mndist(SIZE(ind2)))
 1862                   DO ii = 1,SIZE(ind2)
 1863                     mnlonlist(ii) = sysMem(ind2(ii))%lonMean
 1864                     mnlatlist(ii) = sysMem(ind2(ii))%latMean
 1865                     mndist(ii) = SQRT((lonmean-mnlonlist(ii))**2 + &
 1866                                       (latmean-mnlatlist(ii))**2)
 1867                   END DO
 1868                   dmndiag = SQRT(lonext**2+latext**2)
 1869                   WRITE(20,*) 'Distance list =',mndist(:)
 1870                   WRITE(20,*) 'Domain diagonal =',dmndiag
 
 
                                                         w3strkmd.F90  page  35
 
 
 1871                                                          
 1872 !                 Criterion 5: Overlap of systems
 1873                   ALLOCATE (olsize(SIZE(ind2)))
 1874                   DO ii = 1,SIZE(ind2)
 1875                                       
 1876                IF (sysMem(ind2(ii))%nPoints.GT.0) THEN
 1877                CALL INTERSECT(REAL(sysA(i)%sys(j)%indx(1:npnts)),npnts, &
 1878                REAL(sysMem(ind2(ii))%indx(1:sysMem(ind2(ii))%nPoints)),&
 1879                sysMem(ind2(ii))%nPoints,dummy1,olsize(ii),dummy2,dummy3)
 1880                ELSE
 1881                   olsize(ii) = 0
 1882                END IF
 1883                   END DO
 1884                         
 1885                   GOF(j,1:SIZE(ind2)) = (TEMP/deltaPersysmem(:))**2 + &
 1886                     (dirs/deltaDirsysmem(:))**2 + &
 1887 !                    (4*mndist(:)/dmndiag)**2
 1888                     ( (REAL(olsize(:)) - &
 1889                     REAL(sysMem(ind2(:))%nPoints) )/&
 1890                     (0.50*MAX(REAL(sysMem(ind2(:))%nPoints),0.001)) )**2
 1891 !                 Remove GoF entries which exceed predifined tolerances
 1892                   DO ii = 1,SIZE(ind2)
 1893                      WRITE(20,*) 'Testing: ii,olsize(ii),size,frac =',&
 1894                        ii,olsize(ii),sysMem(ind2(ii))%nPoints,&
 1895                        REAL(olsize(ii))/&
 1896                        MAX(REAL(sysMem(ind2(ii))%nPoints),0.001)
 1897                      IF ( REAL(olsize(ii)).LT.&
 1898                           0.50*REAL(sysMem(ind2(ii))%nPoints) ) THEN
 1899                        GOF(j,ii) = 9999.
 1900                      END IF
 1901                      IF ( (TEMP(ii).GT.deltaPersysmem(ii)).OR.&
 1902                           (dirs(ii).GT.deltaDirsysmem(ii)) ) THEN
 1903                        GOF(j,ii) = 9999.
 1904                      END IF
 1905                   END DO
 1906                   WRITE(20,*) 'GOF(j,:) =',GOF(j,:)
 1907                                                    
 1908                   IF (ALLOCATED(TEMP)) DEALLOCATE(TEMP)
 1909                   IF (ALLOCATED(dirs)) DEALLOCATE(dirs)
 1910                   IF (ALLOCATED(mnlonlist)) DEALLOCATE(mnlonlist)
 1911                   IF (ALLOCATED(mnlatlist)) DEALLOCATE(mnlatlist)
 1912                   IF (ALLOCATED(mndist)) DEALLOCATE(mndist)
 1913                   IF (ALLOCATED(olsize)) DEALLOCATE(olsize)
 1914                                                            
 1915 !071212-----------GoF 2D-------------
 1916          END DO
 1917          IF (ALLOCATED(Tbsysmem)) DEALLOCATE(Tbsysmem)
 1918          IF (ALLOCATED(deltaDirsysmem)) DEALLOCATE(deltaDirsysmem)
 1919          IF (ALLOCATED(deltaPersysmem)) DEALLOCATE(deltaPersysmem)
 1920          IF (ALLOCATED(m1sysmem)) DEALLOCATE(m1sysmem)
 1921          IF (ALLOCATED(m2sysmem)) DEALLOCATE(m2sysmem)
 1922                                                       
 1923          WRITE(20,*) 'GoF3:'
 1924          DO jj = 1,maxSys(i)
 1925             WRITE(20,*) GOF(jj,:)
 
 
                                                         w3strkmd.F90  page  36
 
 
 1926          END DO
 1927                
 1928 !        Find minima in GoF
 1929          DO k = 1,maxGroup
 1930             GOFMinVal(k) = MINVAL(GOF(:,k))
 1931             GOFMinInd(k) = FINDFIRST(GOF(:,k),SIZE(GOF,1),GOFMinVal(k))
 1932             IF (GOFMinVal(k).EQ.9999) THEN
 1933                GOFMinInd(k) = 0
 1934             END IF
 1935          END DO
 1936                
 1937          IF (ALLOCATED(GOF)) DEALLOCATE(GOF)
 1938                                             
 1939          DO j = 1, maxSys(i)
 1940                   new = 0
 1941 !                 Look up sysMem match for this current system. If no mat>
 1942 !                 is found, the index value 0 is returned.
 1943                   tpMinInd = 0
 1944                   TEMP1 = 9999.
 1945                   DO jj = 1, SIZE(GOFMinInd)
 1946                      IF (GOFMinInd(jj).EQ.j) THEN
 1947                         IF (GOFMinVal(jj).LT.TEMP1) THEN
 1948                            tpMinInd = jj
 1949                            TEMP1 = GOFMinVal(jj)
 1950                         END IF
 1951                      END IF
 1952                   END DO
 1953                   dirMinInd = tpMinInd
 1954                   WRITE(20,*) 'System, GOFMinInd: ',j,tpMinInd
 1955                                                               
 1956                   IF (tpMinInd.NE.0) THEN
 1957 !                    Success
 1958 !071212-----------GoF 2D-------------
 1959                                      
 1960                      counter = counter+1
 1961                      sysA(i)%sys(j)%grp = &
 1962                         sysMem(ind2(dirMinInd))%grp
 1963                      alreadyUsed(counter) = sysA(i)%sys(j)%grp
 1964                                                               
 1965                      WRITE(20,*) 'Case 1: matched this ts (',i, &
 1966                       ') sys ',sysA(i)%sys(j)%sysInd,' (tp=', &
 1967                       sysA(i)%sys(j)%tpMean,' dir=', &
 1968                       sysA(i)%sys(j)%dirMean,') with grp ', &
 1969                       sysMem(ind2(dirMinInd))%grp
 1970                       WRITE(20,*) 'Added ',alreadyUsed(counter), &
 1971                          ' in array *alreadyUsed*'
 1972                   ELSE
 1973                       new = 1
 1974                   END IF
 1975                         
 1976                   IF (new.EQ.1) THEN
 1977                      used = 0
 1978                      DO k = 1, maxGroup
 1979                         ok = 1
 1980                         WRITE(20,*) 'maxGroup,k,ok,used =', &
 
 
                                                         w3strkmd.F90  page  37
 
 
 1981                                     maxGroup,k,ok,used
 1982 !                       Make sure it hasn't been used yet (at current tim>
 1983                         IF ((i.GT.2).AND. &
 1984                             (.NOT.ANY(alreadyUsed(:).EQ.k))) THEN
 1985 !                           Make sure it hasn't been used yet (at previou>
 1986                             DO l = 1, maxGroup
 1987 !                              If last update of system was more that *6*>
 1988 !                              ago, system can be released (TO CALIBRATE)
 1989                                IF ( (sysMem(l)%grp.EQ.k).AND. &
 1990                                    ((i-sysMem(l)%updated).LT.6) ) ok = 0
 1991                                WRITE(20,*) 'l, ok = ',l,ok
 1992                             END DO
 1993                             IF (ok.EQ.1) THEN
 1994                                sysA(i)%sys(j)%grp = k
 1995                                counter = counter+1;
 1996                                alreadyUsed(counter) = k
 1997                                used = 1
 1998                                WRITE(20,*) 'k,used,counter =', &
 1999                                            k,used,counter
 2000                                EXIT
 2001                             END IF
 2002                         END IF
 2003                      END DO
 2004                      IF (used.EQ.0) THEN
 2005                         maxGroup = maxGroup+1
 2006                         sysA(i)%sys(j)%grp = maxGroup
 2007 !                       Increase sysMem by one slot
 2008                         sysMem(maxGroup)%grp = maxGroup
 2009                         counter = counter+1
 2010                         alreadyUsed(counter) = maxGroup
 2011                      END IF
 2012                    WRITE(20,*) 'counter,maxGroup,sysA(i)%sys(j)%grp =',&
 2013                                counter,maxGroup,sysA(i)%sys(j)%grp
 2014                      WRITE(20,*) 'NO GRP MATCH case 2'
 2015                   END IF
 2016                         
 2017          END DO
 2018          IF (ALLOCATED(ind)) DEALLOCATE(ind)                  !071212 Shi>
 2019          IF (ALLOCATED(ind2)) DEALLOCATE(ind2)                !071212 Shi>
 2020          IF (ALLOCATED(GOFMinVal)) DEALLOCATE(GOFMinVal)
 2021          IF (ALLOCATED(GOFMinInd)) DEALLOCATE(GOFMinInd)
 2022                                                         
 2023          IF (ALLOCATED(alreadyUsed)) DEALLOCATE(alreadyUsed)
 2024          IF (ALLOCATED(allInd)) DEALLOCATE(allInd)
 2025                                                   
 2026 !        Update sysMem
 2027          DO k = 1, maxGroup
 2028             DO kk = 1, maxSys(i)
 2029                IF (sysA(i)%sys(kk)%grp.EQ.sysMem(k)%grp) THEN
 2030                   sysMem(k)%nPoints = sysA(i)%sys(kk)%nPoints
 2031                   sysMem(k)%lonMean = &
 2032                      SUM(sysA(i)%sys(kk)%lon(1:sysMem(k)%nPoints))/&
 2033                      sysMem(k)%nPoints
 2034                   sysMem(k)%latMean = &
 2035                      SUM(sysA(i)%sys(kk)%lat(1:sysMem(k)%nPoints))/&
 
 
                                                         w3strkmd.F90  page  38
 
 
 2036                      sysMem(k)%nPoints
 2037 !070512----------- Weight averages with Hm0 ---------------------
 2038                   TEMP1 = 0.
 2039                   TEMP2 = 0.
 2040                   DO iii = 1,sysMem(k)%nPoints
 2041                      TEMP1 = TEMP1 + &
 2042                    (sysA(i)%sys(kk)%hs(iii)**2)*sysA(i)%sys(kk)%lon(iii)
 2043                      TEMP2 = TEMP2 + &
 2044                    (sysA(i)%sys(kk)%hs(iii)**2)*sysA(i)%sys(kk)%lat(iii)
 2045                   END DO
 2046                   sysMem(k)%lonMean = TEMP1/&
 2047                    MAX(SUM(sysA(i)%sys(kk)%hs(1:sysMem(k)%nPoints)**2),&
 2048                      0.001)
 2049                   sysMem(k)%latMean = TEMP2/&
 2050                    MAX(SUM(sysA(i)%sys(kk)%hs(1:sysMem(k)%nPoints)**2),&
 2051                      0.001)
 2052 !070512----------- Weight averages with Hm0 ---------------------
 2053                   sysMem(k)%tpMean = sysA(i)%sys(kk)%tpMean
 2054                   sysMem(k)%dirMean = sysA(i)%sys(kk)%dirMean
 2055 !071012----------- Grid point indexing --------------------------
 2056                   sysMem(k)%indx(:) = 9999
 2057                   DO iii = 1,sysMem(k)%nPoints
 2058                      sysMem(k)%indx(iii) = &
 2059                         (sysA(i)%sys(kk)%j(iii)-1)*maxI + &
 2060                          sysA(i)%sys(kk)%i(iii)
 2061                   END DO
 2062 !071012----------- Grid point indexing --------------------------
 2063                   sysMem(k)%updated = i
 2064                   sysMem(k)%length = sysMem(k)%length + 1
 2065                END IF
 2066             END DO
 2067             !Test for expired groups
 2068             IF ((i-sysMem(k)%updated).GE.6) THEN
 2069                sysMem(k)%nPoints = 0
 2070                sysMem(k)%lonMean = 9999.
 2071                sysMem(k)%latMean = 9999.
 2072                sysMem(k)%tpMean = 9999.
 2073                sysMem(k)%dirMean = 9999.
 2074                sysMem(k)%indx(:) = 9999
 2075                sysMem(k)%updated = -9999
 2076                sysMem(k)%length = 0
 2077             END IF
 2078          END DO
 2079 !083012  !Filter out duplicates groups that can develop
 2080          DO l = 1, maxGroup
 2081             DO ll = (l+1), maxGroup
 2082                                    
 2083                deltaDir = MAX((-3.645*sysMem(l)%tpMean+63.211),10.)*1.
 2084                deltaPer = MAX((-0.346*sysMem(l)%tpMean+3.686),0.6)*1.
 2085                                                                      
 2086                IF ( (ABS(sysMem(l)%tpMean-sysMem(ll)%tpMean).LT.&
 2087                      deltaPer).AND. &
 2088                     (ABS(sysMem(l)%dirMean-sysMem(ll)%dirMean).LT.&
 2089                      deltaDir).AND. &
 2090                     (sysMem(l)%updated.NE.sysMem(ll)%updated).AND. &
 
 
                                                         w3strkmd.F90  page  39
 
 
 2091                     (sysMem(ll)%nPoints.NE.0) ) THEN
 2092                   !Find the more recent entry, and delete from group
 2093                   IF (sysMem(ll)%length.LT.sysMem(l)%length) THEN
 2094                      idup = ll
 2095                      WRITE(20,*) 'Deleting memgroup ',ll, &
 2096                         '(updated',sysMem(ll)%updated,', length', &
 2097                         sysMem(ll)%length,'), duplicate of memgroup', &
 2098                         l,'(updated',sysMem(l)%updated,', length', &
 2099                         sysMem(l)%length,'):'
 2100                   ELSE
 2101                      idup = l
 2102                      WRITE(20,*) 'Deleting memgroup ',l, &
 2103                         '(updated',sysMem(l)%updated,', length', &
 2104                         sysMem(l)%length,'), duplicate of memgroup', &
 2105                         ll,'(updated',sysMem(ll)%updated,', length', &
 2106                         sysMem(ll)%length,'):'
 2107                   END IF
 2108                   WRITE(20,*) 'deltaPer, diff Per:',deltaPer,&
 2109                      ABS(sysMem(l)%tpMean-sysMem(ll)%tpMean)
 2110                   WRITE(20,*) 'deltaDir, diff Dir:',deltaDir,&
 2111                      ABS(sysMem(l)%dirMean-sysMem(ll)%dirMean)
 2112                   sysMem(idup)%nPoints = 0
 2113                   sysMem(idup)%lonMean = 9999.
 2114                   sysMem(idup)%latMean = 9999.
 2115                   sysMem(idup)%tpMean = 9999.
 2116                   sysMem(idup)%dirMean = 9999.
 2117                   sysMem(idup)%indx(:) = 9999
 2118                   sysMem(idup)%updated = -9999
 2119                   sysMem(idup)%length = 0
 2120                END IF
 2121             END DO
 2122          END DO
 2123          ELSE
 2124             WRITE(20,*) '*** No systems at this time level. ', &
 2125                         'No. systems =',SIZE(sysA(i)%sys)
 2126             !Test for expired groups
 2127             DO k = 1, maxGroup
 2128                IF ((i-sysMem(k)%updated).GE.6) THEN
 2129                   sysMem(k)%nPoints = 0
 2130                   sysMem(k)%lonMean = 9999.
 2131                   sysMem(k)%latMean = 9999.
 2132                   sysMem(k)%tpMean = 9999.
 2133                   sysMem(k)%dirMean = 9999.
 2134                   sysMem(k)%indx(:) = 9999
 2135                   sysMem(k)%updated = -9999
 2136                   sysMem(k)%length = 0
 2137                END IF
 2138             END DO
 2139          END IF
 2140 !        ******** Test output ***********************
 2141          DO k = 1, maxGroup
 2142             WRITE(20,*) 'sysMem(',k,')%grp =',sysMem(k)%grp
 2143             WRITE(20,*) 'sysMem(',k,')%nPoints =',sysMem(k)%nPoints
 2144             WRITE(20,*) 'sysMem(',k,')%lonMean =',sysMem(k)%lonMean
 2145             WRITE(20,*) 'sysMem(',k,')%latMean =',sysMem(k)%latMean
 
 
                                                         w3strkmd.F90  page  40
 
 
 2146             WRITE(20,*) 'sysMem(',k,')%tpMean =',sysMem(k)%tpMean
 2147             WRITE(20,*) 'sysMem(',k,')%dirMean =',sysMem(k)%dirMean
 2148             WRITE(20,*) 'sysMem(',k,')%updated =',sysMem(k)%updated
 2149             WRITE(20,*) 'sysMem(',k,')%length =',sysMem(k)%length
 2150          END DO
 2151 !        ********************************************
 2152       END DO
 2153             
 2154 !     Write hotfile of wave groups
 2155       OPEN(unit=27,file='sys_restart1.ww3',status='unknown')
 2156       WRITE(27,'(A23,I10)') 'maxGroup             =',maxGroup
 2157       DO k = 1, maxGroup
 2158          WRITE(27,'(A8,I3,A12,I10)') 'sysMem( ',k, &
 2159             ' )%grp     =',sysMem(k)%grp
 2160          WRITE(27,'(A8,I3,A12,I10)') 'sysMem( ',k, &
 2161             ' )%nPoints =',sysMem(k)%nPoints
 2162          WRITE(27,'(A8,I3,A12,F10.4)') 'sysMem( ',k, &
 2163             ' )%lonMean =',sysMem(k)%lonMean
 2164          WRITE(27,'(A8,I3,A12,F10.4)') 'sysMem( ',k, &
 2165             ' )%latMean =',sysMem(k)%latMean
 2166          WRITE(27,'(A8,I3,A12,F10.3)') 'sysMem( ',k, &
 2167             ' )%tpMean  =',sysMem(k)%tpMean
 2168          WRITE(27,'(A8,I3,A12,F10.3)') 'sysMem( ',k, &
 2169             ' )%dirMean =',sysMem(k)%dirMean
 2170          WRITE(27,'(A8,I3,A12,I10)') 'sysMem( ',k, &
 2171             ' )%updated =',sysMem(k)%updated
 2172          WRITE(27,'(A8,I3,A12,I10)') 'sysMem( ',k, &
 2173             ' )%length  =',sysMem(k)%length
 2174          DO j = maxJ,1,-1
 2175             DO i = 1,maxI
 2176               WRITE(27,'(I8)',ADVANCE='NO') sysMem(k)%indx((j-1)*maxI+i)
 2177             END DO
 2178             WRITE(27,'(A)',ADVANCE='YES') ''
 2179          END DO
 2180       END DO
 2181       CLOSE(27)
 2182                
 2183  2000 CONTINUE
 2184       RETURN
 2185       END SUBROUTINE timeTrackingV2
 2186 !/ End of timeTrackingV2 --------------------------------------------- /
 2187 !/
 2188 !/ ------------------------------------------------------------------- /
 2189       SUBROUTINE findWay (way         ,horizStepCount,vertStepCount , &
 2190                           vertBorder  ,horizBorder   ,stepCount     )
 2191 !/
 2192 !/                  +-----------------------------------+
 2193 !/                  | WAVEWATCH III           NOAA/NCEP |
 2194 !/                  |     A. J. van der Westhuysen      |
 2195 !/                  |            Jeff Hanson            |
 2196 !/                  |        Eve-Marie Devaliere        |
 2197 !/                  |                        FORTRAN 95 |
 2198 !/                  | Last update :          4-Jan-2013 |
 2199 !/                  +-----------------------------------+
 2200 !/
 
 
                                                         w3strkmd.F90  page  41
 
 
 2201 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 2202 !/                  by Jeff Hanson & Eve-Marie Devaliere
 2203 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 2204 !/
 2205 !/    Copyright 2009-2013 National Weather Service (NWS),
 2206 !/       National Oceanic and Atmospheric Administration.  All rights
 2207 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 2208 !/       No unauthorized use without permission.
 2209 !/
 2210       IMPLICIT NONE
 2211 !
 2212 !  1. Purpose :
 2213 !
 2214 !     From the direction (way) we were going before, find which direction>
 2215 !     are going now and how many 'steps' we need to take
 2216 !
 2217 !  2. Method
 2218 !
 2219 !     -
 2220 !
 2221 !  3. Parameters :
 2222 !
 2223 !     Parameter list
 2224 !     ----------------------------------------------------------------
 2225 !     way         Char     in/out Direction of spiral search
 2226 !     vertBorder  Int      input
 2227 !     horizBorder Int      input
 2228 !     stepCount   Int      output Number of steps to go in the selected d>
 2229 !
 2230       CHARACTER   :: way *1
 2231       INTEGER     :: horizStepCount, vertStepCount, &
 2232                      vertBorder, horizBorder, stepCount
 2233                                                        
 2234       INTENT (IN) vertBorder, horizBorder
 2235       INTENT (OUT) stepCount
 2236       INTENT (IN OUT) way
 2237 !
 2238 !     Local variables
 2239 !     ----------------------------------------------------------------
 2240 !     -
 2241 !
 2242 !  4. Subroutines used :
 2243 !
 2244 !      Name      Type  Module   Description
 2245 !     ----------------------------------------------------------------
 2246 !     -
 2247 !
 2248 !  5. Subroutines calling
 2249 !
 2250 !     spiralTrackV3
 2251 !
 2252 !  6. Error messages :
 2253 !
 2254 !  7. Remarks :
 2255 !
 
 
                                                         w3strkmd.F90  page  42
 
 
 2256 !  8. Structure :
 2257 !
 2258 !     See above
 2259 !
 2260 !  9. Switches :
 2261 !
 2262 !     None defined yet.
 2263 !
 2264 ! 10. Source code :
 2265 !
 2266 !/ ------------------------------------------------------------------- /
 2267       SELECT CASE (way)
 2268           CASE ('R')
 2269               way='D'
 2270               vertStepCount=vertStepCount+1
 2271               IF (horizBorder.EQ.1) THEN
 2272                   horizStepCount=horizStepCount-1
 2273               END IF
 2274               stepCount=vertStepCount
 2275           CASE ('D')
 2276               way='L'
 2277               horizStepCount=horizStepCount+1
 2278               IF (vertBorder.EQ.1) THEN
 2279                   vertStepCount=vertStepCount-1
 2280               END IF
 2281               stepCount=horizStepCount
 2282           CASE ('L')
 2283               way='U'
 2284               vertStepCount=vertStepCount+1
 2285               IF (horizBorder.EQ.1) THEN
 2286                   horizStepCount=horizStepCount-1
 2287               END IF
 2288               stepCount=vertStepCount
 2289           CASE ('U')
 2290               way='R'
 2291               horizStepCount=horizStepCount+1
 2292               IF (vertBorder.EQ.1) THEN
 2293                   vertStepCount=vertStepCount-1
 2294               END IF
 2295               stepCount=horizStepCount
 2296           CASE DEFAULT
 2297               WRITE(20,*) 'In spaTack:findWay should NOT go here!'
 2298       END SELECT
 2299                 
 2300       RETURN
 2301       END SUBROUTINE findWay
 2302 !/ End of findWay ---------------------------------------------------- /
 2303 !/
 2304 !/ ------------------------------------------------------------------- /
 2305       SUBROUTINE findNext (i      ,j          ,maxI       ,maxJ     , &
 2306                            way    ,vertBorder ,horizBorder          )
 2307 !/
 2308 !/                  +-----------------------------------+
 2309 !/                  | WAVEWATCH III           NOAA/NCEP |
 2310 !/                  |           H. L. Tolman            |
 
 
                                                         w3strkmd.F90  page  43
 
 
 2311 !/                  |            Jeff Hanson            |
 2312 !/                  |        Eve-Marie Devaliere        |
 2313 !/                  |                        FORTRAN 95 |
 2314 !/                  | Last update :          4-Jan-2013 |
 2315 !/                  +-----------------------------------+
 2316 !/
 2317 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 2318 !/                  by Jeff Hanson & Eve-Marie Devaliere
 2319 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 2320 !/
 2321 !/    Copyright 2009-2013 National Weather Service (NWS),
 2322 !/       National Oceanic and Atmospheric Administration.  All rights
 2323 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 2324 !/       No unauthorized use without permission.
 2325 !/
 2326       IMPLICIT NONE
 2327 !
 2328 !  1. Purpose :
 2329 !
 2330 !     Find next point on spatial search spiral
 2331 !
 2332 !  2. Method
 2333 !
 2334 !     -
 2335 !
 2336 !  3. Parameters :
 2337 !
 2338 !     Parameter list
 2339 !     ----------------------------------------------------------------
 2340 !     i,j         Int      in/out  Current grid indices
 2341 !     maxI, maxJ  Int      input   Maximum indices of wave field
 2342 !     way         Char     input   Direction of spiral search
 2343 !     vertBorder  Int      output  Flag indicating that vert domain edge >
 2344 !     horizBorder Int      output  Flag indicating that hor domain edge h>
 2345 !
 2346       CHARACTER   :: way
 2347       INTEGER     :: i, j, maxI, maxJ, vertBorder, horizBorder
 2348                                                               
 2349       INTENT (IN) maxI, maxJ, way
 2350       INTENT (IN OUT) i, j
 2351       INTENT (OUT) vertBorder, horizBorder
 2352 !
 2353 !     Local variables
 2354 !     ----------------------------------------------------------------
 2355 !     -
 2356 !
 2357 !  4. Subroutines used :
 2358 !
 2359 !      Name      Type  Module   Description
 2360 !     ----------------------------------------------------------------
 2361 !     -
 2362 !
 2363 !  5. Subroutines calling
 2364 !
 2365 !     spiralTrackV3
 
 
                                                         w3strkmd.F90  page  44
 
 
 2366 !
 2367 !  6. Error messages :
 2368 !
 2369 !  7. Remarks :
 2370 !
 2371 !  8. Structure :
 2372 !
 2373 !     -
 2374 !
 2375 !  9. Switches :
 2376 !
 2377 !     None defined yet.
 2378 !
 2379 ! 10. Source code :
 2380 !
 2381 !/ ------------------------------------------------------------------- /
 2382       vertBorder=0
 2383       horizBorder=0
 2384       SELECT CASE (way)
 2385           CASE ('R')
 2386               IF (i.LT.maxI) THEN
 2387                   i=i+1
 2388               ELSE
 2389 !                 Need to tell findWay that if we hit the border we don't
 2390 !                 increment stepCount...
 2391                   horizBorder=1
 2392               END IF
 2393           CASE ('D')
 2394               IF (j.GT.1) THEN
 2395                   j=j-1
 2396               ELSE
 2397                   vertBorder=1
 2398               END IF
 2399           CASE ('L')
 2400               IF (i.GT.1) THEN
 2401                   i=i-1
 2402               ELSE
 2403                   horizBorder=1
 2404               END IF
 2405           CASE ('U')
 2406               IF (j.LT.maxJ) THEN
 2407                   j=j+1
 2408               ELSE
 2409                   vertBorder=1
 2410               END IF
 2411       END SELECT
 2412                 
 2413       RETURN
 2414       END SUBROUTINE findNext
 2415 !/ End of findNext --------------------------------------------------- /
 2416 !/
 2417 !/ ------------------------------------------------------------------- /
 2418       SUBROUTINE findSys (i        ,j        ,wsdat    ,maxSys    , &
 2419                           ngbrExt  ,maxI     ,maxJ     ,perKnob   , &
 2420                           dirKnob  ,hsKnob                           )
 
 
                                                         w3strkmd.F90  page  45
 
 
 2421 !/
 2422 !/                  +-----------------------------------+
 2423 !/                  | WAVEWATCH III           NOAA/NCEP |
 2424 !/                  |     A. J. van der Westhuysen      |
 2425 !/                  |            Jeff Hanson            |
 2426 !/                  |        Eve-Marie Devaliere        |
 2427 !/                  |                        FORTRAN 95 |
 2428 !/                  | Last update :          4-Jan-2013 |
 2429 !/                  +-----------------------------------+
 2430 !/
 2431 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 2432 !/                  by Jeff Hanson & Eve-Marie Devaliere
 2433 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 2434 !/
 2435 !/    Copyright 2009-2013 National Weather Service (NWS),
 2436 !/       National Oceanic and Atmospheric Administration.  All rights
 2437 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 2438 !/       No unauthorized use without permission.
 2439 !/
 2440       IMPLICIT NONE
 2441 !
 2442 !  1. Purpose :
 2443 !
 2444 !     Find all wave systems that neighbour the grid point (i,j), and
 2445 !     match these with the systems at (i,j).
 2446 !
 2447 !  2. Method
 2448 !
 2449 !     For the given point (i,j), find all wave systems at neighbouring gr>
 2450 !     points within the reach specified by ngbrExt.
 2451 !
 2452 !  3. Parameters :
 2453 !
 2454 !     Parameter list
 2455 !     ----------------------------------------------------------------
 2456 !     i,j         Int      input   Current grid indices
 2457 !     maxI, maxJ  Int      input   Maximum indices of wave field
 2458 !     wsdat   Type(dat2d)  in/out  Input data structure to be spiral trac>
 2459 !     maxSys      Int      in/out  Maximum number of systems identified
 2460 !
 2461       TYPE(dat2d) :: wsdat
 2462       INTEGER     :: i, j, maxI, maxJ, ngbrExt, maxSys
 2463       REAL        :: perKnob ,dirKnob, hsKnob
 2464                                              
 2465       INTENT (IN) i, j, maxI, maxJ, ngbrExt, perKnob ,dirKnob
 2466       INTENT (IN OUT) wsdat, maxSys
 2467 !
 2468 !     Local variables
 2469 !     ----------------------------------------------------------------
 2470 !     tmpsys  TYPE(system)   Temporary instance of the wave system variab>
 2471 !     nngbr       Int        Number of neighbours found
 2472 !
 2473       TYPE(system), ALLOCATABLE :: tmpsys(:)
 2474       TYPE(neighbr) :: ngbr(50)
 2475       TYPE(mtchsys) :: match
 
 
                                                         w3strkmd.F90  page  46
 
 
 2476       LOGICAL       :: found
 2477       INTEGER       :: counter, ii, jj, nngbr, startCount, endCount, l,&
 2478                        nout, maxS, s, p, n, countAll, ind, minInd, &
 2479                        npart, pp, leng
 2480       INTEGER       :: allFullSys(50)
 2481       REAL, POINTER :: realarr(:)
 2482       INTEGER, ALLOCATABLE :: allSys(:)
 2483       REAL         :: hsAll(50),tpAll(50),dirAll(50),GOF(50)
 2484       REAL         :: absDir,absPer,absHs,T,&
 2485                       deltaPer,deltaDir,deltaHs,temp
 2486       REAL         :: dx, m1, m2
 2487       REAL         :: GOFMinVal
 2488       INTEGER      :: GOFMinInd
 2489 !
 2490 !  4. Subroutines used :
 2491 !
 2492 !      Name      Type  Module   Description
 2493 !     ----------------------------------------------------------------
 2494 !      UNIQUE
 2495 !      combinePartitionsV2
 2496 !
 2497 !  5. Subroutines calling
 2498 !
 2499 !     spiralTrackV3
 2500 !
 2501 !  6. Error messages :
 2502 !
 2503 !  7. Remarks :
 2504 !
 2505 !  8. Structure :
 2506 !
 2507 !     -
 2508 !
 2509 !  9. Switches :
 2510 !
 2511 !     None defined yet.
 2512 !
 2513 ! 10. Source code :
 2514 !
 2515 !/ ------------------------------------------------------------------- /
 2516       NULLIFY(realarr)
 2517 !      WRITE(20,*) 'findSys: i,j,maxSys =',i,j,maxSys
 2518                                                      
 2519 !     First find the checked neighbor
 2520       counter=1
 2521       DO ii=(i-ngbrExt), (i+ngbrExt)
 2522           DO jj=(j-ngbrExt), (j+ngbrExt)
 2523               IF ( (ii.GT.0).AND.(jj.GT.0).AND. &
 2524                    (jj.LE.maxJ).AND.(ii.LE.maxI) ) THEN
 2525                   IF ( wsdat%par(ii,jj)%checked.EQ.1 ) THEN
 2526                       ngbr(counter)%par = wsdat%par(ii,jj)               >
 2527                       ngbr(counter)%i = ii
 2528                       ngbr(counter)%j = jj
 2529                       counter=counter+1
 2530                   END IF
 
 
                                                         w3strkmd.F90  page  47
 
 
 2531               END IF
 2532           END DO
 2533       END DO
 2534 !     New variable nngbr
 2535       nngbr=counter-1
 2536                      
 2537       IF (nngbr.GT.0) THEN
 2538           allFullSys(:) = 0
 2539           startCount=1
 2540           l=1
 2541           DO WHILE (l.LE.nngbr)
 2542               leng = LENGTH(REAL(ngbr(l)%par%sys), &
 2543                      SIZE(ngbr(l)%par%sys),REAL(9999))
 2544               endCount = startCount+leng-1
 2545               allFullSys(startCount:endCount) = ngbr(l)%par%sys(1:leng)
 2546               startCount=endCount+1
 2547               l=l+1
 2548           END DO
 2549                 
 2550           IF (endCount.EQ.0) WRITE(20,*) '***1.Calling UNIQUE w. len=0!'
 2551           CALL UNIQUE (REAL(allFullSys),endCount,realarr,nout)           >
 2552           ALLOCATE(allSys(nout))
 2553           allSys = INT(realarr)                                          >
 2554           IF (ASSOCIATED(realarr)) DEALLOCATE(realarr)
 2555           maxS = MAXVAL(allSys)
 2556                                
 2557           IF (maxSys.LT.maxS) THEN
 2558               maxSys=maxS
 2559           END IF
 2560 !         Initiate sys num
 2561           ALLOCATE( tmpsys(SIZE(allSys)) )
 2562 !         Clear the wsdat%par(i,j)%sys field, new values assigned below.
 2563 !         System info temporarily stored in allSys
 2564           wsdat%par(i,j)%sys(1:10) = 9999
 2565                                          
 2566           DO s=1, SIZE(allSys)
 2567               hsAll(:) = 0.
 2568               tpAll(:) = 0.
 2569               dirAll(:) = 0.
 2570 !              wfAll(:) = 0.
 2571               n=1
 2572               countAll=0
 2573               DO WHILE (n.LE.nngbr)
 2574 !                Calculate mean of common neighbor wave system
 2575 !                for every neigbor wave system
 2576                  found = .FALSE.
 2577                  DO ind = 1, SIZE(ngbr(n)%par%sys)                       >
 2578                     IF ( ngbr(n)%par%sys(ind).EQ.allSys(s) ) THEN        >
 2579                        found = .TRUE.
 2580                        EXIT
 2581                     END IF
 2582                  END DO
 2583                        
 2584                  IF (found) THEN
 2585                      countAll=countAll+1
 
 
                                                         w3strkmd.F90  page  48
 
 
 2586                      hsAll(countAll)=ngbr(n)%par%hs(ind)
 2587                      tpAll(countAll)=ngbr(n)%par%tp(ind)
 2588                      dirAll(countAll)=ngbr(n)%par%dir(ind)
 2589 !                     wfAll(countAll)=ngbr(n)%par%wf(ind)
 2590                  ELSE
 2591                      n=n+1
 2592                      CYCLE
 2593                  END IF
 2594                  n=n+1
 2595               END DO
 2596               tmpsys(s)%hsMean = SUM(hsAll(1:countAll))/countAll
 2597               tmpsys(s)%tpMean = SUM(tpAll(1:countAll))/countAll
 2598               tmpsys(s)%dirMean = &
 2599                  mean_angleV2(dirAll(1:countAll),countAll)
 2600 !              tmpsys(s)%wfMean = SUM(wfAll(1:countAll))/countAll
 2601           END DO
 2602                 
 2603 !         Find the partition at current (i,j) point that matches previous>
 2604 !         identified wave systems if any...
 2605           wsdat%par(i,j)%ngbrSys(1:SIZE(allSys)) = allSys
 2606                                                          
 2607           npart = LENGTH(REAL(wsdat%par(i,j)%ipart), &
 2608                          SIZE(wsdat%par(i,j)%ipart),REAL(0))
 2609           DO p = 1, npart
 2610               IF ( (wsdat%par(i,j)%hs(p).LT.hsKnob).OR. &
 2611                    (wsdat%par(i,j)%tp(p).EQ.0.) ) THEN
 2612                   wsdat%par(i,j)%sys(p)=-1
 2613                   CYCLE
 2614               END IF
 2615                     
 2616               ind=0                                                      >
 2617               match%sysVal(:) = 9999
 2618               match%tpVal(:) = 9999.
 2619               match%dirVal(:) = 9999.
 2620 !              match%wfVal(:) = 9999.
 2621 !             Cycle through the neighbouring systems identified above
 2622               DO s=1,SIZE(allSys)
 2623                   absHs = ABS(wsdat%par(i,j)%hs(p)-tmpsys(s)%hsMean)
 2624                   absPer = ABS(wsdat%par(i,j)%tp(p)-tmpsys(s)%tpMean)
 2625                   absDir = ABS(wsdat%par(i,j)%dir(p)-tmpsys(s)%dirMean)
 2626 !                  absWf = ABS(wsdat%par(i,j)%wf(p)-tmpsys(s)%wfMean)
 2627                   IF (absDir.GT.180) THEN
 2628                      absDir = 360 - absDir
 2629                      IF (absDir.LT.0) THEN
 2630                         WRITE(20,*) '*** WARNING: absDir negative!'
 2631                         WRITE(20,*) 'wsdat%par(i,j)%dir(p) =', &
 2632                                     wsdat%par(i,j)%dir(p)
 2633                         WRITE(20,*) 'tmpsys(s)%dirMean) =', &
 2634                                     tmpsys(s)%dirMean
 2635                      END IF
 2636                   END IF
 2637 !                 Calculate delta dir and freq as a function of the parti>
 2638 !                 dir and freq
 2639                   T = tmpsys(s)%tpMean
 2640                   dx = 0.5*( (wsdat%lon(2,1)-wsdat%lon(1,1)) + &
 
 
                                                         w3strkmd.F90  page  49
 
 
 2641                              (wsdat%lat(1,2)-wsdat%lat(1,1)) )
 2642                   m1 = -3.645*T + 63.211
 2643                   m1 = MAX(m1,10.)
 2644                   m2 = -0.346*T + 3.686
 2645                   m2 = MAX(m2,0.6)
 2646 !1stddev                  m1 = -2.219*T + 35.734
 2647 !1stddev                  m1 = MAX(m1,5.)
 2648 !1stddev                  m2 = -0.226*T + 2.213
 2649 !1stddev                  m2 = MAX(m2,0.35)
 2650 !5stddev                  m1 = -5.071*T + 90.688
 2651 !5stddev                  m1 = MAX(m1,16.)
 2652 !5stddev                  m2 = -0.467*T + 5.161
 2653 !5stddev                  m2 = MAX(m2,1.0)
 2654                   deltaDir = m1*dx + dirKnob
 2655                   deltaPer = m2*dx + perKnob
 2656                   deltaHs = 0.25*tmpsys(s)%hsMean
 2657                   IF ((absPer.LT.deltaPer).AND.(absDir.LT.deltaDir)) THEN
 2658                       ind=ind+1
 2659                       match%sysVal(ind) = allSys(s)
 2660                       match%tpVal(ind) = absPer
 2661                       match%dirVal(ind) = absDir
 2662                       match%hsVal(ind) = absHs
 2663 !                      match%wfVal(ind) = absWf
 2664                   END IF
 2665               END DO
 2666                     
 2667               IF (ind.GT.0) THEN
 2668                   IF (ind.EQ.1) THEN
 2669                      wsdat%par(i,j)%sys(p) = match%sysVal(1)
 2670                   ELSE
 2671 !                    Take the closest match, using GoF function
 2672                      GOF(:) = 9999.
 2673                      GOF(1:ind) = (match%tpVal(1:ind)/deltaPer)**2 + &
 2674                                   (match%dirVal(1:ind)/deltaDir)**2 + &
 2675                                   (match%hsVal(1:ind)/deltaHs)**2
 2676                      GOFMinVal = MINVAL(GOF(1:ind))
 2677                      GOFMinInd = FINDFIRST(GOF(1:ind),ind,GOFMinVal)
 2678                      wsdat%par(i,j)%sys(p) = match%sysVal(GOFMinInd)     >
 2679                   END IF
 2680               END IF
 2681           END DO
 2682       END IF
 2683             
 2684 !     Now check if 2 partitions have been associated to the same wave sys>
 2685 !     so combine them
 2686       npart = LENGTH(REAL(wsdat%par(i,j)%ipart), &
 2687                        SIZE(wsdat%par(i,j)%ipart),REAL(0))
 2688       DO p = 1, (npart-1)                                                >
 2689          DO pp = (p+1), npart
 2690             IF (wsdat%par(i,j)%sys(p).EQ.wsdat%par(i,j)%sys(pp)) THEN
 2691 !              There is at least one duplicate, so combine systems
 2692                CALL combinePartitionsV2(wsdat%par(i,j))
 2693             END IF
 2694          END DO
 2695       END DO
 
 
                                                         w3strkmd.F90  page  50
 
 
 2696 !     Now that we have associated any possible partition to an existing
 2697 !     wave system, we check if any wave system is free. If so give it a
 2698 !     new wave system number
 2699       npart = LENGTH(REAL(wsdat%par(i,j)%ipart), &
 2700                      SIZE(wsdat%par(i,j)%ipart),REAL(0))
 2701                                                         
 2702       DO p = 1, npart
 2703          IF (wsdat%par(i,j)%sys(p).EQ.9999) THEN
 2704             maxSys = maxSys + 1
 2705             wsdat%par(i,j)%sys(p) = maxSys
 2706          END IF
 2707       END DO
 2708       wsdat%par(i,j)%checked=1
 2709                               
 2710       IF (ALLOCATED(allSys)) DEALLOCATE(allSys)
 2711       IF (ALLOCATED(tmpsys)) DEALLOCATE(tmpsys)
 2712                                                
 2713       RETURN
 2714       END SUBROUTINE findSys
 2715 !/ End of findSys ---------------------------------------------------- /
 2716 !/
 2717 !/ ------------------------------------------------------------------- /
 2718       SUBROUTINE combineWaveSystems (wsdat   ,maxSys  ,maxPts   , &
 2719                                      maxI    ,maxJ    ,perKnob  , &
 2720                                      dirKnob ,hsKnob  ,combine  , &
 2721                                      sys   )
 2722 !/
 2723 !/                  +-----------------------------------+
 2724 !/                  | WAVEWATCH III           NOAA/NCEP |
 2725 !/                  |     A. J. van der Westhuysen      |
 2726 !/                  |            Jeff Hanson            |
 2727 !/                  |        Eve-Marie Devaliere        |
 2728 !/                  |                        FORTRAN 95 |
 2729 !/                  | Last update :          4-Jan-2013 |
 2730 !/                  +-----------------------------------+
 2731 !/
 2732 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 2733 !/                  by Jeff Hanson & Eve-Marie Devaliere
 2734 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 2735 !/
 2736 !/    Copyright 2009-2013 National Weather Service (NWS),
 2737 !/       National Oceanic and Atmospheric Administration.  All rights
 2738 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 2739 !/       No unauthorized use without permission.
 2740 !/
 2741       IMPLICIT NONE
 2742 !
 2743 !  1. Purpose :
 2744 !
 2745 !     Combine wave systems. Then remove small and low-energy systems from>
 2746 !     based on the parameters maxPts and maxHgt.
 2747 !
 2748 !  2. Method
 2749 !
 2750 !     -
 
 
                                                         w3strkmd.F90  page  51
 
 
 2751 !
 2752 !  3. Parameters :
 2753 !
 2754 !     Parameter list
 2755 !     ----------------------------------------------------------------
 2756 !     wsdat   Type(dat2d)  output  Combined wave system data structure
 2757 !     sys     Type(system) output  Final set of tracked systems, for one >
 2758 !     maxI, maxJ  Int      input   Maximum indices of wave field
 2759 !     maxSys      Int      input   Maximum number of systems identified
 2760 !     maxPts      Int      input   Number of points req for valid system
 2761 !     hsKnob      Real     input   Parameter for identifying valid system
 2762 !     combine     Int      input   Toggle: 1=combine systems; 0=do not co>
 2763                                                                           
 2764       TYPE(dat2d) :: wsdat
 2765       TYPE(system), POINTER :: sys(:), systemp(:)
 2766       INTEGER      :: maxSys, maxPts, maxI, maxJ, combine
 2767       REAL         :: perKnob ,dirKnob, hsKnob
 2768                                               
 2769       INTENT (IN) maxPts, maxI, maxJ, hsKnob, combine
 2770       INTENT (IN OUT) wsdat, maxSys                                      >
 2771 !      INTENT (OUT) sys
 2772 !
 2773 !     Local variables
 2774 !     ----------------------------------------------------------------
 2775 !     nSys       Int   Number of wave systems (for checking iterative com>
 2776 !
 2777       LOGICAL   :: found
 2778       INTEGER, ALLOCATABLE :: sysOut(:)
 2779       INTEGER, ALLOCATABLE :: actSysInd(:)
 2780       INTEGER   :: iter, ok, nSys, mS, s, so, ss, ind, leng, &
 2781                    iw, jw, iloop
 2782       INTEGER   :: actSys
 2783       REAL      :: dev, hsCmp, maxHgt, temp(5)
 2784 !
 2785 !  4. Subroutines used :
 2786 !
 2787 !      Name      Type  Module   Description
 2788 !     ----------------------------------------------------------------
 2789 !      printFinalSys
 2790 !      combineSys
 2791 !
 2792 !  5. Subroutines calling
 2793 !
 2794 !     spiralTrackV3
 2795 !
 2796 !  6. Error messages :
 2797 !
 2798 !  7. Remarks :
 2799 !
 2800 !  8. Structure :
 2801 !
 2802 !     -
 2803 !
 2804 !  9. Switches :
 2805 !
 
 
                                                         w3strkmd.F90  page  52
 
 
 2806 !     None defined yet.
 2807 !
 2808 ! 10. Source code :
 2809 !
 2810 !/ ------------------------------------------------------------------- /
 2811                                                                         
 2812 !012912      WRITE(20,*) 'maxSys,maxPts,maxI,maxJ,hsKnob,combine =', &
 2813 !012912                  maxSys,maxPts,maxI,maxJ,hsKnob,combine
 2814                                                                
 2815 !     Set up initial index array of active systems
 2816       IF (.NOT.ALLOCATED(actSysInd)) ALLOCATE( actSysInd(maxSys) )
 2817       actSysInd(1:maxSys) = (/ (ind, ind = 1, maxSys) /)
 2818 !opt      WRITE(20,*) 'actSysInd =',actSysInd
 2819                                              
 2820       IF (combine.EQ.1) THEN
 2821 !         Combine wave systems
 2822           WRITE(20,*) 'Calling printFinalSys...'
 2823           CALL printFinalSys (wsdat,maxSys,actSysInd,maxI,maxJ,1,sys)
 2824           iter=0
 2825           ok=0
 2826 !         Keep on combining wave systems until all possible combining
 2827 !         has been carried out (based on the combining criteria)
 2828           DO WHILE (ok.EQ.0)
 2829               iter = iter+1
 2830 !             No of systems before combining
 2831               IF (ALLOCATED(actSysInd)) THEN
 2832                  nSys = SIZE(actSysInd)
 2833               ELSE
 2834                  nSys = maxSys
 2835               END IF
 2836               WRITE(20,'(A,A,I3,A,I5,A)') 'Calling combineSys for ', &
 2837                            'iteration',iter,' (maxSys =',nSys,').'
 2838                                                                   
 2839 !opt              WRITE(20,*) 'SIZE(sys)=',SIZE(sys)
 2840               CALL combineSys (wsdat,sys,maxSys,maxI,maxJ, &
 2841                                actSysInd,perKnob,dirKnob)
 2842 !             No of systems after combining
 2843 !opt              WRITE(20,*) 'maxSys,nSys,SIZE(actSysInd) =', &
 2844 !opt                           maxSys,nSys,SIZE(actSysInd)
 2845 !              IF (maxSys.EQ.nSys) ok = 1
 2846               IF (SIZE(actSysInd).EQ.nSys) ok = 1
 2847           END DO
 2848       ELSE
 2849 !         Do not combine wave systems
 2850           CALL printFinalSys (wsdat,maxSys,actSysInd,maxI,maxJ,3,sys)
 2851       END IF
 2852             
 2853 !     Remove small and low-energy systems from set, based on
 2854 !     the parameters maxPts and maxHgt.
 2855 !      ALLOCATE( sysOut(maxSys) )
 2856 !      sysOut = sys(1:maxSys)%sysInd
 2857 !      mS = maxSys
 2858       mS = SIZE(actSysInd)
 2859       ss = 1
 2860       WRITE(20,*) 'Filtering the set of',mS,'systems on size and mag.'
 
 
                                                         w3strkmd.F90  page  53
 
 
 2861                                                                       
 2862       DO so = 1, mS
 2863           s = actSysInd(so)
 2864 !opt      NOTE: if we deallocate the individual records without
 2865 !opt      compressing sys, then s and sysInd will remain the same
 2866           ss = s
 2867                 
 2868           leng = LENGTH(sys(ss)%hs,SIZE(sys(ss)%hs),9999.)
 2869           dev  = STD(sys(ss)%hs(1:leng),leng)
 2870           hsCmp = sys(ss)%hsMean + 2.*dev
 2871           maxHgt = hsKnob
 2872                          
 2873           IF ( (hsCmp.LT.maxHgt).OR.(sys(ss)%nPoints.LT.maxPts) ) THEN
 2874 !             Remove system, and shift up indices to fill the gap
 2875               DO ind = 1, maxSys
 2876 !                Find index to remove
 2877                  IF (ind.EQ.ss) THEN
 2878 !                   Shift up entries, deleting the duplicate partition
 2879 !                   REPLACE WITH CSHIFT(ARRAY, SHIFT, dim)?
 2880 !                    IF (ind.LT.maxSys) &
 2881 !                       sys( ind:(maxSys-1) ) = sys( (ind+1):maxSys )
 2882                     IF (ind.LE.maxSys) THEN
 2883 !                      Since we use pointers, we have to copy each index >
 2884 !                      field individually. Otherwise memory corruption oc>
 2885                        DO iloop = ind,ind
 2886                           sys(iloop)%sysInd = 9999
 2887                           sys(iloop)%nPoints = 0
 2888                           sys(iloop)%grp = 9999
 2889                           DEALLOCATE( sys(iloop)%hs )
 2890                           DEALLOCATE( sys(iloop)%tp )
 2891                           DEALLOCATE( sys(iloop)%dir )
 2892                           DEALLOCATE( sys(iloop)%dspr )
 2893 !                          DEALLOCATE( sys(iloop)%wf )
 2894                           DEALLOCATE( sys(iloop)%i )
 2895                           DEALLOCATE( sys(iloop)%j )
 2896                           DEALLOCATE( sys(iloop)%lat )
 2897                           DEALLOCATE( sys(iloop)%lon )
 2898 !                          DEALLOCATE( sys(iloop)%hsMean )
 2899 !                          DEALLOCATE( sys(iloop)%tpMean )
 2900 !                          DEALLOCATE( sys(iloop)%dirMean )
 2901 !                          DEALLOCATE( sys(iloop)%ngbr )
 2902                        END DO
 2903                     END IF
 2904                  END IF
 2905               END DO
 2906                     
 2907 !             Update wsdat as well
 2908               DO iw = 1, maxI
 2909                  DO jw = 1, maxJ
 2910                      leng = LENGTH(REAL(wsdat%par(iw,jw)%sys), &
 2911                             SIZE(wsdat%par(iw,jw)%sys),REAL(9999))
 2912                      ind = 1
 2913                      found = .FALSE.
 2914 !                    Identify system index (there are no duplicate
 2915 !                    systems at this point.
 
 
                                                         w3strkmd.F90  page  54
 
 
 2916                      DO WHILE (ind.LE.leng)
 2917                         IF ( wsdat%par(iw,jw)%sys(ind).EQ.s ) THEN
 2918                            found = .TRUE.
 2919                            EXIT
 2920                         END IF
 2921                         ind = ind + 1
 2922                      END DO
 2923                      IF (found) THEN
 2924 !                       Blank out used record
 2925                         wsdat%par(iw,jw)%sys(ind) = 9999
 2926                         wsdat%par(iw,jw)%ipart(ind) = 9999
 2927                      END IF
 2928                  END DO
 2929               END DO
 2930           END IF
 2931       END DO
 2932             
 2933 !     Compile array index of active systems in sys
 2934       actSys = 0
 2935       DO so = 1,maxSys
 2936          IF (sys(so)%nPoints>0) actSys = actSys + 1
 2937       END DO
 2938       IF (ALLOCATED(actSysInd)) DEALLOCATE(actSysInd)
 2939       ALLOCATE( actSysInd(actSys) )
 2940       actSys = 0
 2941       DO so = 1,maxSys
 2942          IF (sys(so)%nPoints>0) THEN
 2943             actSys = actSys + 1
 2944             actSysInd(actSys) = sys(so)%sysInd
 2945          END IF
 2946       END DO
 2947             
 2948 !opt      WRITE(20,*) 'actSysInd =',actSysInd
 2949       DO so = 1,SIZE(actSysInd)
 2950          s = actSysInd(so)
 2951 !opt         WRITE(20,*) 'sys(',s,')%sysInd =',sys(s)%sysInd
 2952       END DO
 2953             
 2954       CALL printFinalSys (wsdat,maxSys,actSysInd,maxI,maxJ,1,sys)
 2955 !opt      WRITE(20,*) 'actSysInd =',actSysInd
 2956 !opt      DO so = 1,maxSys
 2957 !opt         WRITE(20,*) 'sys(',so,')%sysInd =',sys(so)%sysInd, &
 2958 !opt                     ',  sys(',so,')%nPoints =',sys(so)%nPoints
 2959 !opt      END DO
 2960                 
 2961       RETURN
 2962       END SUBROUTINE combineWaveSystems
 2963 !/ End of combineWaveSystems ----------------------------------------- /
 2964 !/
 2965 !/ ------------------------------------------------------------------- /
 2966       SUBROUTINE printFinalSys (wsdat    ,maxSys    ,actSysInd , &
 2967                                 maxI     ,maxJ      ,flag      ,sys    )
 2968 !/
 2969 !/                  +-----------------------------------+
 2970 !/                  | WAVEWATCH III           NOAA/NCEP |
 
 
                                                         w3strkmd.F90  page  55
 
 
 2971 !/                  |     A. J. van der Westhuysen      |
 2972 !/                  |            Jeff Hanson            |
 2973 !/                  |        Eve-Marie Devaliere        |
 2974 !/                  |                        FORTRAN 95 |
 2975 !/                  | Last update :          4-Jan-2013 |
 2976 !/                  +-----------------------------------+
 2977 !/
 2978 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 2979 !/                  by Jeff Hanson & Eve-Marie Devaliere
 2980 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 2981 !/
 2982 !/    Copyright 2009-2013 National Weather Service (NWS),
 2983 !/       National Oceanic and Atmospheric Administration.  All rights
 2984 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 2985 !/       No unauthorized use without permission.
 2986 !/
 2987       IMPLICIT NONE
 2988 !
 2989 !  1. Purpose :
 2990 !
 2991 !     Output (print) the final output systems for this time step.
 2992 !
 2993 !  2. Method
 2994 !
 2995 !     -
 2996 !
 2997 !  3. Parameters :
 2998 !
 2999 !     Parameter list
 3000 !     ----------------------------------------------------------------
 3001 !     wsdat   Type(dat2d)  input   Combined data structure
 3002 !     maxI, maxJ  Int      input   Maximum indices of wave field
 3003 !     maxSys      Int      input   Maximum number of systems identified
 3004 !     flag        Int      input   Flag for printing system
 3005 !     sys     Type(system) output  Final set of tracked systems, for one >
 3006 !
 3007       TYPE(dat2d)  :: wsdat
 3008       TYPE(system), POINTER :: sys(:)
 3009       INTEGER      :: maxSys, maxI, maxJ, flag
 3010       INTEGER, ALLOCATABLE :: actSysInd(:)
 3011                                           
 3012       INTENT (IN) wsdat, actSysInd, maxI, maxJ, flag
 3013       INTENT (OUT) maxSys
 3014 !      INTENT (IN OUT) sys
 3015 !
 3016 !     Local variables
 3017 !     ----------------------------------------------------------------
 3018 !     ic        Int   Counter for wave systems
 3019 !
 3020       INTEGER   :: ic, nGuys, startInd, endInd, i, j, ind, leng, leng2
 3021       INTEGER   :: UNISIZE, DIFSIZE
 3022       REAL, ALLOCATABLE :: sysOrdered(:)
 3023       REAL, POINTER :: UNIARR(:), DIFARR(:)
 3024       INTEGER, ALLOCATABLE :: ngbrSysAll(:), sysSortedInd(:)
 3025       REAL      :: TEMP(2), TEMP1, TEMP2
 
 
                                                         w3strkmd.F90  page  56
 
 
 3026 !
 3027 !  4. Subroutines used :
 3028 !
 3029 !      Name      Type  Module   Description
 3030 !     ----------------------------------------------------------------
 3031 !      UNIQUE
 3032 !      SETDIFF
 3033 !      SORT
 3034 !
 3035 !  5. Subroutines calling
 3036 !
 3037 !     combineWaveSystems
 3038 !
 3039 !  6. Error messages :
 3040 !
 3041 !  7. Remarks :
 3042 !
 3043 !  8. Structure :
 3044 !
 3045 !     -
 3046 !
 3047 !  9. Switches :
 3048 !
 3049 !     None defined yet.
 3050 !
 3051 ! 10. Source code :
 3052 !
 3053 !/ ------------------------------------------------------------------- /
 3054                                                                         
 3055 !     Initialize sys structure
 3056       IF (flag.NE.2) THEN
 3057 !         Allocate data structure with the final wave systems
 3058           WRITE(20,*) 'In printFinalSys...'
 3059           maxSys = SIZE(actSysInd)
 3060           NULLIFY(sys)
 3061           ALLOCATE( sys(maxSys) )
 3062           WRITE(20,*) 'Allocated sys okay, SIZE(sys) =',SIZE(sys)
 3063                                                                  
 3064           ALLOCATE( ngbrSysAll(50*maxI*maxJ) )                           >
 3065           DO ic = 1, maxSys
 3066               NULLIFY( sys(ic)%hs )
 3067               NULLIFY( sys(ic)%tp )
 3068               NULLIFY( sys(ic)%dir )
 3069               NULLIFY( sys(ic)%dspr )
 3070 !              NULLIFY( sys(ic)%wf )
 3071               NULLIFY( sys(ic)%i )
 3072               NULLIFY( sys(ic)%j )
 3073               NULLIFY( sys(ic)%lat )
 3074               NULLIFY( sys(ic)%lon )
 3075               ALLOCATE( sys(ic)%hs(maxI*maxJ) )
 3076               ALLOCATE( sys(ic)%tp(maxI*maxJ) )
 3077               ALLOCATE( sys(ic)%dir(maxI*maxJ) )
 3078               ALLOCATE( sys(ic)%dspr(maxI*maxJ) )
 3079 !              ALLOCATE( sys(ic)%wf(maxI*maxJ) )
 3080               ALLOCATE( sys(ic)%i(maxI*maxJ) )
 
 
                                                         w3strkmd.F90  page  57
 
 
 3081               ALLOCATE( sys(ic)%j(maxI*maxJ) )
 3082               ALLOCATE( sys(ic)%lat(maxI*maxJ) )
 3083               ALLOCATE( sys(ic)%lon(maxI*maxJ) )
 3084               sys(ic)%hs(:) = 9999.                                      >
 3085               sys(ic)%tp(:) = 9999.
 3086               sys(ic)%dir(:) = 9999.
 3087               sys(ic)%dspr(:) = 9999.
 3088 !              sys(ic)%wf(:) = 9999.
 3089               sys(ic)%i(:) = 9999
 3090               sys(ic)%j(:) = 9999
 3091               sys(ic)%lat(:) = 9999.
 3092               sys(ic)%lon(:) = 9999.
 3093               sys(ic)%sysInd = 9999
 3094               sys(ic)%hsMean = 9999.
 3095               sys(ic)%tpMean = 9999.
 3096               sys(ic)%dirMean = 9999.
 3097               sys(ic)%nPoints = 0
 3098               sys(ic)%ngbr(:) = 9999
 3099               sys(ic)%grp = 9999
 3100               ngbrSysAll(:) = 0
 3101               startInd=1
 3102               nGuys=0
 3103                      
 3104               DO i = 1, maxI
 3105                   DO j = 1, maxJ
 3106 !                     ind=wsdat.par(i,j).sys==ic;
 3107                       DO ind = 1, SIZE(wsdat%par(i,j)%sys)               >
 3108                          IF (wsdat%par(i,j)%sys(ind).EQ.actSysInd(ic)) &
 3109                             THEN
 3110                            nGuys=nGuys+1
 3111                            sys(ic)%hs(nGuys)=wsdat%par(i,j)%hs(ind)
 3112                            sys(ic)%tp(nGuys)=wsdat%par(i,j)%tp(ind)
 3113                            sys(ic)%dir(nGuys)=wsdat%par(i,j)%dir(ind)
 3114                            sys(ic)%dspr(nGuys)=wsdat%par(i,j)%dspr(ind)
 3115 !                           sys(ic)%wf(nGuys)=wsdat%par(i,j)%wf(ind)
 3116                            sys(ic)%i(nGuys)=i
 3117                            sys(ic)%j(nGuys)=j
 3118                            sys(ic)%lat(nGuys)=wsdat%lat(i,j)
 3119                            sys(ic)%lon(nGuys)=wsdat%lon(i,j)
 3120                            leng = LENGTH(REAL(wsdat%par(i,j)%ngbrSys), &
 3121                              SIZE(wsdat%par(i,j)%ngbrSys),REAL(9999))
 3122                            endInd = startInd + leng-1
 3123                            ngbrSysAll(startInd:endInd) = &
 3124                              wsdat%par(i,j)%ngbrSys(1:leng)
 3125                            startInd=endInd+1
 3126                          END IF
 3127                       END DO
 3128                   END DO
 3129               END DO
 3130                     
 3131 !              if ~isempty(sys)
 3132               IF (nGuys.GT.0) THEN
 3133                   sys(ic)%sysInd=ic
 3134                   sys(ic)%hsMean = SUM(sys(ic)%hs(1:nGuys))/nGuys
 3135                   sys(ic)%tpMean = SUM(sys(ic)%tp(1:nGuys))/nGuys
 
 
                                                         w3strkmd.F90  page  58
 
 
 3136 !                 sys(ic)%dirMean=mean_angle_single(sys(ic).dir)         >
 3137                   sys(ic)%dirMean = &
 3138                      mean_angleV2(sys(ic)%dir(1:nGuys),nGuys)
 3139 !070512----------- Weight averages with Hm0 ---------------------
 3140                   TEMP1 = 0.
 3141                   TEMP2 = 0.
 3142                   DO i = 1,nGuys
 3143                      TEMP1 = TEMP1 + (sys(ic)%hs(i)**2)*sys(ic)%hs(i)
 3144                      TEMP2 = TEMP2 + (sys(ic)%hs(i)**2)*sys(ic)%tp(i)
 3145                   END DO
 3146                   sys(ic)%hsMean = &
 3147                      TEMP1/MAX(SUM(sys(ic)%hs(1:nGuys)**2),0.001)
 3148                   sys(ic)%tpMean = &
 3149                      TEMP2/MAX(SUM(sys(ic)%hs(1:nGuys)**2),0.001)
 3150                   sys(ic)%dirMean = mean_angleV3(sys(ic)%dir(1:nGuys), &
 3151                      sys(ic)%hs(1:nGuys),nGuys)
 3152 !070512----------- Weight averages with Hm0 ---------------------
 3153                   sys(ic)%nPoints = nGuys
 3154                   IF (endInd.GT.0) THEN
 3155                      CALL UNIQUE(REAL(ngbrSysAll(1:endInd)),endInd, &
 3156                                  UNIARR,UNISIZE)
 3157                      TEMP = (/REAL(sys(ic)%sysInd),REAL(sys(ic)%sysInd)/)
 3158                      CALL SETDIFF(REAL(UNIARR),UNISIZE, &
 3159                                   TEMP,2,DIFARR,DIFSIZE)
 3160                      DIFSIZE = MIN(DIFSIZE,SIZE(sys(ic)%ngbr))
 3161                      sys(ic)%ngbr(1:DIFSIZE) = NINT(DIFARR(1:DIFSIZE))
 3162                      IF (ASSOCIATED(UNIARR)) DEALLOCATE(UNIARR)
 3163                      IF (ASSOCIATED(DIFARR)) DEALLOCATE(DIFARR)
 3164                   END IF
 3165               ELSE
 3166                   CYCLE
 3167               END IF
 3168           END DO
 3169           IF (ALLOCATED(ngbrSysAll)) DEALLOCATE(ngbrSysAll)
 3170       END IF
 3171             
 3172 !     Print the sorted field to the screen
 3173       leng = LENGTH(REAL(sys(:)%nPoints), &
 3174                     SIZE(sys(:)%nPoints),REAL(9999))
 3175       ALLOCATE( sysOrdered(leng) )
 3176       ALLOCATE( sysSortedInd(leng) )
 3177       CALL SORT (REAL(sys(:)%nPoints),leng, &
 3178                  sysOrdered,sysSortedInd,'D')
 3179       leng = LENGTH(REAL(sysOrdered), &
 3180                             SIZE(sysOrdered),REAL(0))
 3181                                                      
 3182       DO ic = 1, leng
 3183          leng2 = LENGTH(REAL(sys(sysSortedInd(ic))%ngbr), &
 3184                        SIZE(sys(sysSortedInd(ic))%ngbr),REAL(9999))
 3185       END DO
 3186       IF (ALLOCATED(sysOrdered)) DEALLOCATE(sysOrdered)
 3187       IF (ALLOCATED(sysSortedInd)) DEALLOCATE(sysSortedInd)
 3188                                                            
 3189       RETURN
 3190       END SUBROUTINE printFinalSys
 
 
                                                         w3strkmd.F90  page  59
 
 
 3191 !/ End of printFinalSys ---------------------------------------------- /
 3192 !/
 3193 !/ ------------------------------------------------------------------- /
 3194       SUBROUTINE combineSys (wsdat    ,sys      ,maxSys   ,maxI     , &
 3195                              maxJ     ,actSysInd,perKnob  ,dirKnob  )
 3196 !/
 3197 !/                  +-----------------------------------+
 3198 !/                  | WAVEWATCH III           NOAA/NCEP |
 3199 !/                  |     A. J. van der Westhuysen      |
 3200 !/                  |            Jeff Hanson            |
 3201 !/                  |        Eve-Marie Devaliere        |
 3202 !/                  |                        FORTRAN 95 |
 3203 !/                  | Last update :          4-Jan-2013 |
 3204 !/                  +-----------------------------------+
 3205 !/
 3206 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 3207 !/                  by Jeff Hanson & Eve-Marie Devaliere
 3208 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 3209 !/
 3210 !/    Copyright 2009-2013 National Weather Service (NWS),
 3211 !/       National Oceanic and Atmospheric Administration.  All rights
 3212 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 3213 !/       No unauthorized use without permission.
 3214 !/
 3215       IMPLICIT NONE
 3216 !
 3217 !  1. Purpose :
 3218 !
 3219 !     Combine wave systems
 3220 !
 3221 !  2. Method
 3222 !
 3223 !     -
 3224 !
 3225 !  3. Parameters :
 3226 !
 3227 !     Parameter list
 3228 !     ----------------------------------------------------------------
 3229 !     wsdat   Type(dat2d)  input   Combined data structure
 3230 !     maxI, maxJ  Int      input   Maximum indices of wave field
 3231 !     sys     Type(system) output  Final set of tracked systems, for one >
 3232 !     maxSys      Int      input  Number of systems
 3233 !     dirKnob     Real     input  Parameter in direction for combining fi>
 3234 !     perKnob     Real     input  Parameter in period for combining field>
 3235 !
 3236       TYPE(dat2d)  :: wsdat                                              >
 3237       TYPE(system), POINTER :: sys(:)                                    >
 3238       INTEGER      :: maxSys, maxI, maxJ                                 >
 3239       INTEGER, ALLOCATABLE :: actSysInd(:)
 3240       REAL         :: perKnob ,dirKnob
 3241       REAL         :: dx, m1, m2
 3242                                 
 3243       INTENT (IN) maxI, maxJ, perKnob, dirKnob                           >
 3244 !      INTENT (IN OUT) wsdat, sys, maxSys                                >
 3245 !
 
 
                                                         w3strkmd.F90  page  60
 
 
 3246 !     Local variables
 3247 !     ----------------------------------------------------------------
 3248 !     ngbIndex    Int   Arr   Array of neighbours
 3249 !
 3250       INTEGER, ALLOCATABLE :: sysSortedInd(:), sysOut(:)
 3251       INTEGER, POINTER :: indSys1(:), indSys2(:)
 3252       REAL, ALLOCATABLE    :: sysOrdered(:), rounded(:)
 3253       REAL, POINTER    :: uniarr(:), difarr(:), allngbr(:)
 3254       INTEGER   :: leng, leng2, s, ss, so, ngb, lsys, lsys2, hh, i, j, &
 3255                    ii, jj, ind, ind2, nn, nbr, icEnd,ic,iii,iloop
 3256       INTEGER   :: myngbr, indMatch, matchSys, keep, replacedInd, &
 3257                    hhForIndMatch, lMatch, tot, outsize
 3258       INTEGER   :: ngbIndex(10000), keepInd(maxI*maxJ), oneLess(1000)    >
 3259 !      REAL      :: Tb,deltaPerB,deltaDirB,absDir,absPer,absHs,absWf
 3260       REAL      :: Tb,deltaPerB,deltaDirB,deltaHsB,absDir,absPer,absHs
 3261       LOGICAL   :: file_exists
 3262       INTEGER   :: MASK(maxI,maxJ)
 3263       REAL      :: lonmean, latmean, DIST
 3264 !061512 -----------------------------------------------
 3265       LOGICAL   :: ZIPMATCH
 3266       INTEGER   :: counter, count2, izp, izp2, in, jn, icnt, ngbrExt
 3267       REAL      :: T, ngb_tp, ngb_dir
 3268       REAL      :: ngbmatch(maxI*maxJ)
 3269       TYPE(neighbr) :: ngbr(50)
 3270 !061512 -----------------------------------------------
 3271       REAL      :: TEMP1, TEMP2
 3272       INTEGER   :: actSys
 3273                          
 3274 !
 3275 !  4. Subroutines used :
 3276 !
 3277 !      Name      Type  Module   Description
 3278 !     ----------------------------------------------------------------
 3279 !      SORT
 3280 !      findIJV4
 3281 !      UNIQUE
 3282 !      combinePartitionsV2
 3283 !      UNION
 3284 !      SETDIFF
 3285 !
 3286 !  5. Subroutines calling
 3287 !
 3288 !     combineWaveSystems
 3289 !
 3290 !  6. Error messages :
 3291 !
 3292 !  7. Remarks :
 3293 !
 3294 !  8. Structure :
 3295 !
 3296 !     -
 3297 !
 3298 !  9. Switches :
 3299 !
 3300 !     None defined yet.
 
 
                                                         w3strkmd.F90  page  61
 
 
 3301 !
 3302 ! 10. Source code :
 3303 !
 3304 !/ ------------------------------------------------------------------- /
 3305 !     Initialize pointer (first use)
 3306       NULLIFY(indSys1)
 3307       NULLIFY(indSys2)
 3308 !     Flag to combine systems on a point-by-point basis along boundary,
 3309 !     instead of using mean values.
 3310       ZIPMATCH = .FALSE.
 3311       ngbrExt = 1
 3312 !     Combine systems on the basis of tpMean
 3313       ALLOCATE( sysOrdered(maxSys) )
 3314       ALLOCATE( sysSortedInd(maxSys) )
 3315       ALLOCATE( sysOut(maxSys) )
 3316       ALLOCATE( rounded(maxSys) )
 3317 !     Sort in descending Tp: the following improves the iterative combini>
 3318 !     the special case that the wave period is constant over the domain, >
 3319 !     tpMean is not because of truncation errors at very high decimals.
 3320       rounded = REAL(INT(sys(1:maxSys)%tpMean*1.E4))*1.E-4
 3321       CALL SORT(rounded,maxSys,sysOrdered,sysSortedInd,'D')
 3322       sysOut=sys(sysSortedInd)%sysInd
 3323       IF (ALLOCATED(rounded)) DEALLOCATE(rounded)
 3324                                                  
 3325 !051612 --- Land mask addition
 3326       MASK(:,:) = 0
 3327       INQUIRE(FILE="sys_mask.ww3", EXIST=file_exists)
 3328       IF (file_exists) THEN
 3329          WRITE(20,*) '*** Using land mask'
 3330          OPEN(unit=13,file='sys_mask.ww3',status='old')
 3331          DO j = maxJ,1,-1
 3332             READ(13,*) (MASK(i,j), i=1,maxI)
 3333          END DO
 3334          CLOSE(13)
 3335       END IF
 3336 !051612 --- Land mask addition
 3337                               
 3338 !opt      WRITE(20,*) 'SIZE(sysOut)=',SIZE(sysOut)
 3339       DO so = 1, SIZE(sysOut)
 3340 !          WRITE(20,*) 'so =',so
 3341           s = sysOut(so)
 3342           ss = FINDFIRST(REAL(sys(:)%sysInd),SIZE(sys(:)%sysInd), &
 3343                          REAL(s))
 3344 !opt          WRITE(20,*) 's,ss=',s,ss
 3345           ngbIndex(:) = 0
 3346           ii = 1
 3347           leng = LENGTH(REAL(sys(ss)%ngbr),SIZE(sys(ss)%ngbr), &
 3348                         REAL(9999))
 3349 !         Identify the indices of all the systems that neighbour the curr>
 3350 !         store in ngbIndex(:)
 3351           DO ngb = 1, leng
 3352              IF ( sys(ss)%ngbr(ngb).NE.s ) THEN
 3353                myngbr = 1
 3354                DO WHILE (myngbr.LE.SIZE(sysOut))
 3355                  IF (sys(myngbr)%sysInd.EQ.sys(ss)%ngbr(ngb)) THEN
 
 
                                                         w3strkmd.F90  page  62
 
 
 3356                     ngbIndex(ii) = myngbr
 3357                     ii = ii+1
 3358                     IF (ii.GT.1000) &
 3359                        WRITE(20,*) '*** WARNING: ngbIndex(:) exceeded!'
 3360                  END IF
 3361                  myngbr = myngbr+1
 3362                END DO
 3363              END IF
 3364           END DO
 3365           ii = ii-1
 3366 !opt          WRITE(20,*) so,'.   sys =',s,', Tp =',sys(s)%tpMean, &
 3367 !opt                      ', size=',sys(s)%nPoints,', #neighbours=',ii
 3368                                                                       
 3369           IF ( ii.GT.0 ) THEN
 3370               DO ngb = 1, ii
 3371 !                 We first need to find the (i,j) points that are either >
 3372 !                 to both these systems, or at the boundary of the two sy>
 3373 !                 sys 1 will carry the 'ss' index and sys 2 the ngbIndex(>
 3374                   CALL findIJV4 (sys(ss),sys(ngbIndex(ngb)), &
 3375                                  maxI,maxJ,indSys1,indSys2)
 3376                   IF ((SIZE(indSys1)>10).AND.(SIZE(indSys2)>10).AND. &
 3377                       (sys(ss)%nPoints.GT.sys(ngbIndex(ngb))%nPoints)) &
 3378                       THEN
 3379                       lsys = SIZE(indSys1)
 3380                       lsys2 = SIZE(indSys2)
 3381                                            
 3382 !061512---------------Add zipper compare
 3383                       IF (ZIPMATCH) THEN
 3384 !                       Omit small systems to save time
 3385                         IF ((sys(ss)%nPoints.LT.5).OR. &
 3386                             (sys(ngbIndex(ngb))%nPoints.LT.5)) THEN
 3387                            CYCLE
 3388                         END IF
 3389                         dx=0.5*((wsdat%lon(2,1)-wsdat%lon(1,1)) + &
 3390                                 (wsdat%lat(1,2)-wsdat%lat(1,1)))
 3391                         ngbmatch(:)=0.
 3392                         DO izp = 1,lsys
 3393 !                          Find neighbors of this point
 3394                            counter=0
 3395                            DO in=(sys(ss)%i(indSys1(izp))-ngbrExt), &
 3396                                  (sys(ss)%i(indSys1(izp))+ngbrExt)
 3397                               DO jn=(sys(ss)%j(indSys1(izp))-ngbrExt), &
 3398                                     (sys(ss)%j(indSys1(izp))+ngbrExt)
 3399                                  counter=counter+1
 3400                                  ngbr(counter)%i = in
 3401                                  ngbr(counter)%j = jn
 3402                               END DO
 3403                            END DO
 3404 !                          Find these points in neighboring system
 3405                            ngb_tp = 0.
 3406                            ngb_dir = 0.
 3407                            count2 = 0
 3408                            DO izp2 = 1,lsys2
 3409                              DO icnt = 1,counter
 3410                                IF ((sys(ngbIndex(ngb))%i(indSys2(izp2)) &
 
 
                                                         w3strkmd.F90  page  63
 
 
 3411                                     .EQ.ngbr(icnt)%i).AND. &
 3412                                    (sys(ngbIndex(ngb))%j(indSys2(izp2)) &
 3413                                     .EQ.ngbr(icnt)%j)) THEN
 3414                                  count2 = count2+1
 3415                                  ngb_tp = ngb_tp + &
 3416                                           sys(ngbIndex(ngb))%tp(indSys2(i>
 3417                                  ngb_dir = ngb_dir + &
 3418                                           sys(ngbIndex(ngb))%dir(indSys2(>
 3419                                END IF
 3420                              END DO
 3421                            END DO
 3422                            IF (count2.GT.0) THEN
 3423                              absPer = ABS(sys(ss)%tp(indSys1(izp))-ngb_tp>
 3424                              absDir = ABS(sys(ss)%dir(indSys1(izp))-ngb_d>
 3425                              T = sys(ss)%tp(indSys1(izp))
 3426                              m1 = -3.645*T + 63.211
 3427                              m1 = MAX(m1,10.)
 3428                              m2 = -0.346*T + 3.686
 3429                              m2 = MAX(m2,0.6)
 3430                              deltaDirB = (m1*dx + dirKnob)*1.
 3431                              deltaPerB = (m2*dx + perKnob)*1.
 3432                              IF ( (absPer.LT.deltaPerB).AND. &
 3433                                   (absDir.LT.deltaDirB) ) THEN
 3434                                 ngbmatch(izp)=1.
 3435                              END IF
 3436                            END IF
 3437                         END DO
 3438 !                       If >80% of neighbors fall within criteria, system>
 3439                         IF ((SUM(ngbmatch(1:lsys))/lsys).GT.0.50) THEN
 3440                            indMatch = ngbIndex(ngb)
 3441                            matchSys = sys(indMatch)%sysInd
 3442                         ELSE
 3443                            CYCLE
 3444                         END IF
 3445                       ELSE
 3446 !061512---------------------------------
 3447                                         
 3448                       Tb = MAX(SUM(sys(ss)%tp(indSys1))/lsys, &
 3449                           SUM(sys(ngbIndex(ngb))%tp(indSys2))/lsys2)
 3450 !                      deltaPerB = (-0.06*Tb+2+perKnob)*1.5
 3451 !                      deltaDirB = (-Tb+(25+10*dirKnob))*1.5
 3452 !                      deltaPerB = (-0.06*Tb+2+2)*1.5
 3453 !                      deltaDirB = (-Tb+(25+10*2))*1.5
 3454                       dx=0.5*((wsdat%lon(2,1)-wsdat%lon(1,1)) + &
 3455                               (wsdat%lat(1,2)-wsdat%lat(1,1)))
 3456                       m1 = -3.523*Tb + 64.081
 3457                       m1 = MAX(m1,10.)
 3458                       m2 = -0.337*Tb + 3.732
 3459                       m2 = MAX(m2,0.6)
 3460 !1stddev                      m1 = -2.219*Tb + 35.734
 3461 !1stddev                      m1 = MAX(m1,5.)
 3462 !1stddev                      m2 = -0.226*Tb + 2.213
 3463 !1stddev                      m2 = MAX(m2,0.35)
 3464 !5stddev                      m1 = -5.071*Tb + 90.688
 3465 !5stddev                      m1 = MAX(m1,16.)
 
 
                                                         w3strkmd.F90  page  64
 
 
 3466 !5stddev                      m2 = -0.467*Tb + 5.161
 3467 !5stddev                      m2 = MAX(m2,1.0)
 3468                       deltaDirB = (m1*1. + dirKnob)*1.
 3469                       deltaPerB = (m2*1. + perKnob)*1.
 3470                       deltaHsB = 0.50*SUM(sys(ss)%hs(indSys1))/lsys
 3471 !                      deltaHsB = 0.25*SUM(sys(ss)%hs(indSys1))/lsys
 3472                                                                     
 3473 !051612               --- Land mask addition
 3474 !                     Option 1: If system centroid is near a land mask (e>
 3475 !                               increase the tolerances
 3476                       IF (ANY(MASK.EQ.1)) THEN
 3477                         lonmean = SUM(sys(ss)%lon(indSys1))/lsys
 3478                         latmean = SUM(sys(ss)%lat(indSys1))/lsys
 3479                         DO j = 1,maxJ
 3480                           DO i = 1,maxI
 3481                             IF (MASK(i,j).EQ.1) THEN
 3482 !                             Land point found. Compute distance to syste>
 3483                               DIST = SQRT((lonmean-wsdat%lon(i,j))**2 +&
 3484                                           (latmean-wsdat%lat(i,j))**2)
 3485                               IF (DIST.LT.3.) THEN
 3486 !                               System assumed to be influenced by land,
 3487 !                               increase tolerances to deltaDirB=30,delta>
 3488 !                                deltaDirB = (m1*1. + 30)*1.
 3489 !                                deltaPerB = (m2*1. + 3)*1.
 3490                                 deltaDirB = (m1*1. + 30)*1.
 3491                                 deltaPerB = (m2*1. + 3)*1.
 3492                                 !Remove dHs limitation from criteria
 3493                                 deltaHsB = 9999.
 3494                                 GOTO 500
 3495                               END IF
 3496                             END IF
 3497                           END DO
 3498                         END DO
 3499                       END IF
 3500  500                  CONTINUE
 3501 !051612               --- Land mask addition
 3502                                             
 3503                       absHs = ABS( SUM(sys(ss)%hs(indSys1))/lsys - &
 3504                         SUM(sys(ngbIndex(ngb))%hs(indSys2))/lsys2 )
 3505                       absPer = ABS( SUM(sys(ss)%tp(indSys1))/lsys - &
 3506                         SUM(sys(ngbIndex(ngb))%tp(indSys2))/lsys2 )
 3507                       absDir = ABS( &
 3508                         mean_angleV2(sys(ss)%dir(indSys1),lsys) - &
 3509                         mean_angleV2(sys(ngbIndex(ngb))%dir(indSys2), &
 3510                         lsys2) )
 3511                       IF (absDir.GT.180) absDir = 360.-absDir
 3512 !                      absWf = ABS( SUM(sys(ss)%wf(indSys1))/lsys - &
 3513 !                        SUM(sys(ngbIndex(ngb))%wf(indSys2))/lsys2 )
 3514                                                                     
 3515                       IF ( (absPer.LT.deltaPerB).AND. &
 3516                            (absDir.LT.deltaDirB).AND. &
 3517                            (absHs.LT.deltaHsB) ) THEN
 3518                          indMatch = ngbIndex(ngb)
 3519                          matchSys = sys(indMatch)%sysInd
 3520 !opt                         WRITE(20,*) '-> Matched sys',s, &
 
 
                                                         w3strkmd.F90  page  65
 
 
 3521 !opt                                     'with neighbor sys',matchSys
 3522                       ELSE
 3523                          CYCLE
 3524                       END IF
 3525 !061512---------------------------------
 3526                       END IF
 3527 !061512---------------------------------
 3528                                         
 3529                       keep = 0
 3530                       keepInd(:) = 0
 3531                                     
 3532                       DO hh = 1, sys(ss)%nPoints
 3533                          ii = sys(ss)%i(hh)
 3534                          jj = sys(ss)%j(hh)
 3535                          ind = 0
 3536                          ind = FINDFIRST(REAL(wsdat%par(ii,jj)%sys), &
 3537                                SIZE(wsdat%par(ii,jj)%sys),REAL(s))       >
 3538                          IF (ind.NE.0) THEN
 3539                             wsdat%par(ii,jj)%sys(ind)=matchSys           >
 3540                          END IF
 3541 !                        Remove the "-1" system from the set
 3542                          ind2 = 1
 3543                          oneLess(:) = 9999                               >
 3544                          leng = LENGTH(REAL(wsdat%par(ii,jj)%sys), &
 3545                                SIZE(wsdat%par(ii,jj)%sys),REAL(9999))
 3546                          DO ind = 1, leng
 3547                            IF ( wsdat%par(ii,jj)%sys(ind).NE.-1 ) THEN
 3548                              oneLess(ind2) = wsdat%par(ii,jj)%sys(ind)
 3549                              ind2 = ind2+1
 3550                            END IF
 3551                          END DO
 3552                          ind2 = ind2-1
 3553 !                        Combine any partitions assigned to the same syst>
 3554 !                        Check for duplicates
 3555                          IF (ind2.EQ.0) &
 3556                             WRITE(20,*) '***2.Calling UNIQUE w. len=0!'
 3557                          CALL UNIQUE(REAL(oneLess(1:ind2)),ind2, &
 3558                                      uniarr,outsize)
 3559                          IF (ASSOCIATED(uniarr)) DEALLOCATE(uniarr)
 3560                          IF (ind2.GT.outsize) THEN
 3561 !                          There is at least one duplicate, so combine sy>
 3562                            CALL combinePartitionsV2(wsdat%par(ii,jj))
 3563 !                          Update the combined partitions values into the>
 3564 !                          Since partitions have been combined we don't k>
 3565                            replacedInd = &
 3566                               FINDFIRST(REAL(wsdat%par(ii,jj)%sys(:)), &
 3567                               SIZE(wsdat%par(ii,jj)%sys(:)), &
 3568                               REAL(matchSys))
 3569                            hhForIndMatch = 1
 3570                            DO WHILE (hhForIndMatch.LE. &
 3571                                      sys(indMatch)%nPoints)
 3572                              IF ( (sys(indMatch)%i(hhForIndMatch) &
 3573                                    .EQ.ii).AND. &
 3574                                   (sys(indMatch)%j(hhForIndMatch) &
 3575                                    .EQ.jj) ) EXIT
 
 
                                                         w3strkmd.F90  page  66
 
 
 3576                              hhForIndMatch = hhForIndMatch + 1
 3577                            END DO
 3578                            sys(indMatch)%hs(hhForIndMatch) = &
 3579                                   wsdat%par(ii,jj)%hs(replacedInd)
 3580                            sys(indMatch)%tp(hhForIndMatch) = &
 3581                                   wsdat%par(ii,jj)%tp(replacedInd)
 3582                            sys(indMatch)%dir(hhForIndMatch) = &
 3583                                   wsdat%par(ii,jj)%dir(replacedInd)
 3584                            sys(indMatch)%dspr(hhForIndMatch) = &
 3585                                   wsdat%par(ii,jj)%dspr(replacedInd)
 3586 !                           sys(indMatch)%wf(hhForIndMatch) = &
 3587 !                                  wsdat%par(ii,jj)%wf(replacedInd)
 3588                          ELSE
 3589                             keep = keep+1
 3590                             keepInd(keep) = hh
 3591                          END IF
 3592                       END DO
 3593                       leng = LENGTH(REAL(sys(indMatch)%hs), &
 3594                                SIZE(sys(indMatch)%hs),REAL(9999.))
 3595                                                                   
 3596 !                     Update system info
 3597 !                     ------------------
 3598 !                     First need to find which points were common to both>
 3599 !                     keepInd since that means partitions have not been c>
 3600 !                     points as a result of the combination of those 2 sy>
 3601 !                     distinct points
 3602 !                      keepInd = keepInd(1:keep)
 3603                       lMatch = LENGTH(REAL(sys(indMatch)%hs), &
 3604                                SIZE(sys(indMatch)%hs),REAL(9999.))
 3605                       tot = lMatch + keep
 3606                       CALL UNION (REAL(sys(indMatch)%ngbr), &
 3607                                   SIZE(sys(indMatch)%ngbr), &
 3608                                   REAL(sys(ss)%ngbr), &
 3609                                   SIZE(sys(ss)%ngbr), &
 3610                                   allngbr,outsize)
 3611                       CALL SETDIFF(allngbr,SIZE(allngbr), &
 3612                          REAL((/sys(indMatch)%sysInd, &
 3613                          sys(ss)%sysInd/)), &
 3614                          SIZE((/sys(indMatch)%sysInd, &
 3615                          sys(ss)%sysInd/)),difarr,outsize)
 3616                       sys(indMatch)%ngbr(:) = 9999
 3617                       outsize = MIN(outsize,size(sys(indMatch)%ngbr))
 3618                       sys(indMatch)%ngbr(1:outsize) = NINT(difarr(1:outsi>
 3619                       IF (ASSOCIATED(allngbr)) DEALLOCATE(allngbr)
 3620                       IF (ASSOCIATED(difarr)) DEALLOCATE(difarr)
 3621                                                                 
 3622                       leng = LENGTH(REAL(sys(indMatch)%i), &
 3623                                SIZE(sys(indMatch)%i),REAL(9999))
 3624                       sys(indMatch)%hsMean = SUM((/ &
 3625                          sys(ss)%hs(keepInd(1:keep)), &
 3626                          sys(indMatch)%hs(1:leng) /))/tot
 3627                       sys(indMatch)%tpMean = SUM((/ &
 3628                          sys(ss)%tp(keepInd(1:keep)), &
 3629                          sys(indMatch)%tp(1:leng) /))/tot
 3630                       sys(indMatch)%dirMean = &
 
 
                                                         w3strkmd.F90  page  67
 
 
 3631                         mean_angleV2((/ sys(ss)%dir(keepInd(1:keep)), &
 3632                         sys(indMatch)%dir(1:leng) /),tot)
 3633 !070512----------- Weight averages with Hm0 ---------------------
 3634                       TEMP1 = 0.
 3635                       TEMP2 = 0.
 3636                       DO iii = 1,keep
 3637                          TEMP1 = TEMP1 + (sys(ss)%hs(keepInd(iii))**2)*&
 3638                                  sys(ss)%hs(keepInd(iii))
 3639                          TEMP2 = TEMP2 + (sys(ss)%hs(keepInd(iii))**2)*&
 3640                                  sys(ss)%tp(keepInd(iii))
 3641                       END DO
 3642                       DO iii = 1,leng
 3643                          TEMP1 = TEMP1 + (sys(indMatch)%hs(iii)**2)*&
 3644                                  sys(indMatch)%hs(iii)
 3645                          TEMP2 = TEMP2 + (sys(indMatch)%hs(iii)**2)*&
 3646                                  sys(indMatch)%tp(iii)
 3647                       END DO
 3648                       sys(indMatch)%hsMean = TEMP1/MAX(SUM((/ &
 3649                          sys(ss)%hs(keepInd(1:keep))**2, &
 3650                          sys(indMatch)%hs(1:leng)**2 /)),0.001)
 3651                       sys(indMatch)%tpMean = TEMP2/MAX(SUM((/ &
 3652                          sys(ss)%hs(keepInd(1:keep))**2, &
 3653                          sys(indMatch)%hs(1:leng)**2 /)),0.001)
 3654                       sys(indMatch)%dirMean = &
 3655                         mean_angleV3((/ sys(ss)%dir(keepInd(1:keep)), &
 3656                         sys(indMatch)%dir(1:leng) /), &
 3657                         (/ sys(ss)%hs(keepInd(1:keep)), &
 3658                         sys(indMatch)%hs(1:leng) /),tot)
 3659 !070512----------- Weight averages with Hm0 ---------------------
 3660                                                                  
 3661                       sys(indMatch)%i(1:(keep+leng))= &
 3662                          (/sys(ss)%i(keepInd(1:keep)), &
 3663                            sys(indMatch)%i(1:leng)/)
 3664                       sys(indMatch)%j(1:(keep+leng))= &
 3665                          (/sys(ss)%j(keepInd(1:keep)), &
 3666                            sys(indMatch)%j(1:leng)/)
 3667                       sys(indMatch)%lat(1:(keep+leng)) = &
 3668                          (/sys(ss)%lat(keepInd(1:keep)), &
 3669                            sys(indMatch)%lat(1:leng)/)
 3670                       sys(indMatch)%lon(1:(keep+leng)) = &
 3671                          (/sys(ss)%lon(keepInd(1:keep)), &
 3672                            sys(indMatch)%lon(1:leng)/)
 3673                       sys(indMatch)%dir(1:(keep+leng)) = &
 3674                           (/sys(ss)%dir(keepInd(1:keep)), &
 3675                             sys(indMatch)%dir(1:leng)/)
 3676                       sys(indMatch)%dspr(1:(keep+leng)) = &
 3677                           (/sys(ss)%dspr(keepInd(1:keep)), &
 3678                             sys(indMatch)%dspr(1:leng)/)
 3679 !                      sys(indMatch)%wf(1:(keep+leng)) = &
 3680 !                          (/sys(ss)%wf(keepInd(1:keep)), &
 3681 !                            sys(indMatch)%wf(1:leng)/)
 3682                       sys(indMatch)%hs(1:(keep+leng)) = &
 3683                           (/sys(ss)%hs(keepInd(1:keep)), &
 3684                             sys(indMatch)%hs(1:leng)/)
 3685                       sys(indMatch)%tp(1:(keep+leng)) = &
 
 
                                                         w3strkmd.F90  page  68
 
 
 3686                           (/sys(ss)%tp(keepInd(1:keep)), &
 3687                             sys(indMatch)%tp(1:leng)/)
 3688                       sys(indMatch)%nPoints = &
 3689                                  LENGTH(REAL(sys(indMatch)%i), &
 3690                                  SIZE(sys(indMatch)%i),REAL(9999))
 3691 !                     Clear array of system that has just been combined w>
 3692                       sys(ss)%nPoints = 0
 3693                       sys(ss)%ngbr(:) = 9999
 3694                       WRITE(20,*) 'Deallocating sys',s
 3695                       DEALLOCATE( sys(ss)%hs )                           >
 3696                       DEALLOCATE( sys(ss)%tp )                           >
 3697                       DEALLOCATE( sys(ss)%dir )                          >
 3698                       DEALLOCATE( sys(ss)%dspr )                         >
 3699 !                      DEALLOCATE( sys(ss)%wf )                          >
 3700                       DEALLOCATE( sys(ss)%i )                            >
 3701                       DEALLOCATE( sys(ss)%j )                            >
 3702                       DEALLOCATE( sys(ss)%lat )                          >
 3703                       DEALLOCATE( sys(ss)%lon )                          >
 3704 !                      DEALLOCATE( sys(ss)%hsMean )                      >
 3705 !                      DEALLOCATE( sys(ss)%tpMean )                      >
 3706 !                      DEALLOCATE( sys(ss)%dirMean )                     >
 3707                                                                           
 3708 !                     Loop through wsdat to update neighbouring system va>
 3709                       DO i = 1, maxI
 3710                         DO j = 1, maxJ
 3711                           ind = FINDFIRST(REAL(wsdat%par(i,j)%ngbrSys), &
 3712                                 SIZE(wsdat%par(i,j)%ngbrSys),REAL(s))
 3713                           IF (ind.NE.0) THEN
 3714                              wsdat%par(i,j)%ngbrSys(ind)=matchSys
 3715                           END IF
 3716                           leng = LENGTH(REAL(wsdat%par(i,j)%ngbrSys), &
 3717                              SIZE(wsdat%par(i,j)%ngbrSys),REAL(9999))
 3718                           IF (leng.GT.0) THEN
 3719                              CALL UNIQUE( &
 3720                                 REAL(wsdat%par(i,j)%ngbrSys(1:leng)), &
 3721                                 leng,uniarr,outsize)
 3722                              wsdat%par(i,j)%ngbrSys(:) = 9999
 3723                              wsdat%par(i,j)%ngbrSys(1:outsize) = &
 3724                                 NINT(uniarr)
 3725                              IF (ASSOCIATED(uniarr)) DEALLOCATE(uniarr)
 3726                           ELSE
 3727                              wsdat%par(i,j)%ngbrSys(:) = 9999
 3728                           END IF
 3729                         END DO
 3730                       END DO
 3731                             
 3732 !                     Update neigbors in sys structure
 3733                       DO nn = 1, maxSys
 3734                           nbr = FINDFIRST(REAL(sys(nn)%ngbr), &
 3735                                 SIZE(sys(nn)%ngbr),REAL(s))
 3736                           IF (nbr.NE.0) THEN
 3737 !                             WRITE(20,*) 'update'
 3738                              sys(nn)%ngbr(nbr)=matchSys
 3739                           END IF
 3740                           leng2 = LENGTH(REAL(sys(nn)%ngbr), &
 
 
                                                         w3strkmd.F90  page  69
 
 
 3741                              SIZE(sys(nn)%ngbr),REAL(9999))
 3742                           IF (leng2.GT.0) THEN
 3743                              CALL UNIQUE(REAL(sys(nn)%ngbr(1:leng2)), &
 3744                                          leng2,uniarr,outsize)
 3745                              sys(nn)%ngbr(:) = 9999
 3746                              sys(nn)%ngbr(1:outsize) = NINT(uniarr)
 3747                              IF (ASSOCIATED(uniarr)) DEALLOCATE(uniarr)
 3748 !                             WRITE(20,*) 'has now ngbr: ', &
 3749 !                                sys(nn)%ngbr(1:outsize)
 3750                           END IF
 3751                       END DO
 3752                       EXIT
 3753                   END IF
 3754                         
 3755                         
 3756                  IF (ASSOCIATED(indSys1)) DEALLOCATE(indSys1)
 3757                  IF (ASSOCIATED(indSys2)) DEALLOCATE(indSys2)
 3758               END DO
 3759           END IF
 3760       END DO
 3761             
 3762       IF (ALLOCATED(sysOrdered)) DEALLOCATE(sysOrdered)
 3763       IF (ALLOCATED(sysSortedInd)) DEALLOCATE(sysSortedInd)
 3764       IF (ALLOCATED(sysOut)) DEALLOCATE(sysOut)
 3765                                                
 3766 !     Compile array index of active systems in sys
 3767       actSys = 0
 3768       DO ic = 1,maxSys
 3769          IF (sys(ic)%nPoints>0) actSys = actSys + 1
 3770       END DO
 3771       IF (ALLOCATED(actSysInd)) DEALLOCATE(actSysInd)
 3772       ALLOCATE( actSysInd(actSys) )
 3773       actSys = 0
 3774       DO ic = 1,maxSys
 3775          IF (sys(ic)%nPoints>0) THEN
 3776             actSys = actSys + 1
 3777             actSysInd(actSys) = sys(ic)%sysInd
 3778          END IF
 3779       END DO
 3780             
 3781 !opt      WRITE(20,*) 'actSys =',actSys
 3782 !opt      WRITE(20,*) 'actSysInd =',actSysInd
 3783 !opt      DO ic = 1,SIZE(actSysInd)
 3784 !opt         s = actSysInd(ic)
 3785 !opt         WRITE(20,*) 'sys(',s,')%sysInd =',sys(s)%sysInd
 3786 !opt      END DO
 3787       WRITE(20,*) 'Leaving combineSys...'
 3788                                          
 3789       RETURN
 3790       END SUBROUTINE combineSys
 3791 !/ End of combineSys ------------------------------------------------- /
 3792 !/
 3793 !/ ------------------------------------------------------------------- /
 3794       SUBROUTINE combinePartitionsV2 (dat)
 3795 !/
 
 
                                                         w3strkmd.F90  page  70
 
 
 3796 !/                  +-----------------------------------+
 3797 !/                  | WAVEWATCH III           NOAA/NCEP |
 3798 !/                  |     A. J. van der Westhuysen      |
 3799 !/                  |            Jeff Hanson            |
 3800 !/                  |        Eve-Marie Devaliere        |
 3801 !/                  |                        FORTRAN 95 |
 3802 !/                  | Last update :          4-Jan-2013 |
 3803 !/                  +-----------------------------------+
 3804 !/
 3805 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 3806 !/                  by Jeff Hanson & Eve-Marie Devaliere
 3807 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 3808 !/
 3809 !/    Copyright 2009-2013 National Weather Service (NWS),
 3810 !/       National Oceanic and Atmospheric Administration.  All rights
 3811 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 3812 !/       No unauthorized use without permission.
 3813 !/
 3814       IMPLICIT NONE
 3815 !
 3816 !  1. Purpose :
 3817 !
 3818 !     Combine two partitions that have been assigned to the same system
 3819 !
 3820 !  2. Method
 3821 !
 3822 !     Of all the partitions associated with a certain common system,
 3823 !     add all the Hs values to the partition with the largest Hs,
 3824 !     and delete the rest. NOTE that the tp and dir values of this
 3825 !     maximum partition is not adjusted!
 3826 !
 3827 !  3. Parameters :
 3828 !
 3829 !     Parameter list
 3830 !     ----------------------------------------------------------------
 3831 !     dat    TYPE(param)   in/out  Input data structure (partitions set)
 3832 !                                  to combine
 3833 !
 3834       TYPE(param) :: dat
 3835                         
 3836       INTENT (IN OUT) dat
 3837 !
 3838 !     Local variables
 3839 !     ----------------------------------------------------------------
 3840       TYPE duplicate
 3841          INTEGER :: val
 3842          INTEGER :: ndup
 3843          INTEGER :: ind(50)
 3844       END TYPE duplicate
 3845                         
 3846       TYPE(duplicate) :: dup(100)                                        >
 3847       LOGICAL :: found
 3848       INTEGER :: nsys, ndup, p, pp, maxInd, npart, s, ss, ppp
 3849       REAL :: temp
 3850 !
 
 
                                                         w3strkmd.F90  page  71
 
 
 3851 !  4. Subroutines used :
 3852 !
 3853 !      Name      Type  Module   Description
 3854 !     ----------------------------------------------------------------
 3855 !      -
 3856 !
 3857 !  5. Subroutines calling
 3858 !
 3859 !     findSys
 3860 !
 3861 !  6. Error messages :
 3862 !
 3863 !  7. Remarks :
 3864 !
 3865 !  8. Structure :
 3866 !
 3867 !     -
 3868 !
 3869 !  9. Switches :
 3870 !
 3871 !     None defined yet.
 3872 !
 3873 ! 10. Source code :
 3874 !
 3875 !/ ------------------------------------------------------------------- /
 3876                                                                         
 3877 !     Find indices in dat%sys(:) of all partition associated with
 3878 !     the same wave system, and store them in the data structure
 3879 !     dup(1:nsys). Here nsys is the number of systems for which duplicates
 3880 !     were found, and dup(s)%ndup the number of partitions assigned
 3881 !     to the same system s.
 3882       nsys = 0
 3883       dup(:)%ndup = 0
 3884       dup(:)%val = 9999
 3885       DO s = 1,100
 3886          dup(s)%ind(:) = 0
 3887       END DO
 3888             
 3889       npart = LENGTH(REAL(dat%ipart),SIZE(dat%ipart),REAL(0))
 3890       DO p = 1, npart-1
 3891          found = .FALSE.
 3892          IF (ANY(dat%sys(p).EQ.dup(:)%val)) CYCLE                        >
 3893          DO pp = (p+1), npart
 3894             IF (dat%sys(p).EQ.dat%sys(pp)) THEN
 3895 !              First value
 3896                IF (.NOT.found) THEN
 3897                   nsys=nsys+1
 3898                   dup(nsys)%val = dat%sys(p)
 3899                   dup(nsys)%ndup = 1
 3900                   dup(nsys)%ind(dup(nsys)%ndup) = p
 3901                   found = .TRUE.
 3902                END IF
 3903 !              Subsequent duplicates
 3904                IF (.NOT.ANY(pp.EQ.dup(nsys)%ind(:))) THEN
 3905                   dup(nsys)%ndup = dup(nsys)%ndup+1
 
 
                                                         w3strkmd.F90  page  72
 
 
 3906                   dup(nsys)%ind(dup(nsys)%ndup) = pp
 3907                END IF
 3908             END IF
 3909          END DO
 3910       END DO
 3911             
 3912 !     Now go through array of duplicates for each of n systems
 3913 !     to add all the wave energy to the most energetic of the
 3914 !     duplicates, and then remove the rest.
 3915       maxInd = 0
 3916       temp = -9999.
 3917       DO s = 1, nsys
 3918 !        Find duplicate partition with the largest Hs (most energy)
 3919          DO p = 1, dup(s)%ndup
 3920             IF ( temp.LT.dat%hs(dup(s)%ind(p)) ) THEN
 3921                temp = dat%hs(dup(s)%ind(p))
 3922                maxInd = p
 3923             END IF
 3924          END DO
 3925                
 3926 !        Add all energy (Hs) to this partition
 3927          dat%hs(dup(s)%ind(maxInd)) = &
 3928             SQRT( SUM(dat%hs(dup(s)%ind(1:dup(s)%ndup))**2) )
 3929                                                              
 3930 !        Remove duplicate partitions which did not have the maximum Hs,
 3931 !        and shift up indices to fill the gap
 3932          DO p = 1, dup(s)%ndup
 3933 !           Find index to remove
 3934             IF (p.NE.maxInd) THEN
 3935 !              Shift up entries, deleting the duplicate partition
 3936 !              REPLACE WITH CSHIFT(ARRAY, SHIFT, dim) ?
 3937                dat%hs( dup(s)%ind(p):(npart-1) ) = &
 3938                        dat%hs( (dup(s)%ind(p)+1):npart)
 3939                dat%tp( dup(s)%ind(p):(npart-1) ) = &
 3940                        dat%tp( (dup(s)%ind(p)+1):npart)
 3941                dat%dir( dup(s)%ind(p):(npart-1) ) = &
 3942                        dat%dir( (dup(s)%ind(p)+1):npart)
 3943                dat%dspr( dup(s)%ind(p):(npart-1) ) = &
 3944                        dat%dspr( (dup(s)%ind(p)+1):npart)
 3945 !               dat%wf( dup(s)%ind(p):(npart-1) ) = &
 3946 !                       dat%wf( (dup(s)%ind(p)+1):npart)
 3947                dat%sys( dup(s)%ind(p):(npart-1) ) = &
 3948                         dat%sys( (dup(s)%ind(p)+1):npart)
 3949                dat%ipart( dup(s)%ind(p):(npart-1) ) = &
 3950                         dat%ipart( (dup(s)%ind(p)+1):npart)
 3951 !              Shift up indices
 3952                DO ss = 1, nsys
 3953                   DO ppp = 1, dup(ss)%ndup
 3954                      IF (dup(ss)%ind(ppp).GT.dup(s)%ind(p)) &
 3955                          dup(ss)%ind(ppp) = dup(ss)%ind(ppp)-1
 3956                   END DO
 3957                END DO
 3958 !              Add blank to end
 3959                dat%hs(npart) = 9999.
 3960                dat%tp(npart) = 9999.
 
 
                                                         w3strkmd.F90  page  73
 
 
 3961                dat%dir(npart) = 9999.
 3962                dat%dspr(npart) = 9999.
 3963 !               dat%wf(npart) = 9999.
 3964                dat%sys(npart) = 9999
 3965                dat%ipart(npart) = 0
 3966             END IF
 3967          END DO
 3968       END DO
 3969             
 3970       RETURN
 3971       END SUBROUTINE combinePartitionsV2
 3972 !/ End of combinePartitionsV2 ---------------------------------------- /
 3973 !/
 3974 !/ ------------------------------------------------------------------- /
 3975       REAL FUNCTION mean_angleV2(ang,ll)
 3976 !/
 3977 !/                  +-----------------------------------+
 3978 !/                  | WAVEWATCH III           NOAA/NCEP |
 3979 !/                  |     A. J. van der Westhuysen      |
 3980 !/                  |            Jeff Hanson            |
 3981 !/                  |        Eve-Marie Devaliere        |
 3982 !/                  |                        FORTRAN 95 |
 3983 !/                  | Last update :          4-Jan-2013 |
 3984 !/                  +-----------------------------------+
 3985 !/
 3986 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 3987 !/                  by Jeff Hanson & Eve-Marie Devaliere
 3988 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 3989 !/
 3990 !/    Copyright 2009-2013 National Weather Service (NWS),
 3991 !/       National Oceanic and Atmospheric Administration.  All rights
 3992 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 3993 !/       No unauthorized use without permission.
 3994 !/
 3995       IMPLICIT NONE
 3996 !
 3997 !  1. Purpose :
 3998 !
 3999 !     Compute the mean direction from array of directions
 4000 !
 4001 !  2. Method
 4002 !
 4003 !     ang is a column vector of angles
 4004 !     m_ang is the mean from a unit-vector average of ang
 4005 !     Assumes clockwise rotation from North = 0.
 4006 !
 4007 !  3. Parameters :
 4008 !
 4009 !     Parameter list
 4010 !     ----------------------------------------------------------------
 4011 !     ang     Real   input    Array of angles to average
 4012 !     ll      Int    input    Length of ang
 4013 !
 4014       REAL :: ang(ll)
 4015       INTEGER :: ll
 
 
                                                         w3strkmd.F90  page  74
 
 
 4016 !
 4017 !     Local variables
 4018 !     ----------------------------------------------------------------
 4019 !     u,v       Real   Arrays of u,v dir components to average
 4020 !     um,vm     Real   Mean u,v dir components
 4021 !     theta     Real   Mean direction relative to North
 4022 !
 4023       REAL      :: PI
 4024       PARAMETER  (PI = 3.1416)
 4025       REAL      :: u(ll), v(ll), vm, um, theta
 4026 !
 4027 !  4. Subroutines used :
 4028 !
 4029 !      Name      Type  Module   Description
 4030 !     ----------------------------------------------------------------
 4031 !      -
 4032 !
 4033 !  5. Subroutines calling
 4034 !
 4035 !     findSys
 4036 !
 4037 !  6. Error messages :
 4038 !
 4039 !  7. Remarks :
 4040 !
 4041 !  8. Structure :
 4042 !
 4043 !     -
 4044 !
 4045 !  9. Switches :
 4046 !
 4047 !     None defined yet.
 4048 !
 4049 ! 10. Source code :
 4050 !
 4051 !/ ------------------------------------------------------------------- /
 4052                                                                         
 4053 !     North and East components
 4054       v(:) = COS(ang(:)*(PI/180.))
 4055       u(:) = SIN(ang(:)*(PI/180.))
 4056       vm = SUM(v)/ll
 4057       um = SUM(u)/ll
 4058                     
 4059 !     Compute mean magnitude and direction relative to North (from Upolar>
 4060       theta = (ATAN2(um,vm))*(180/PI)
 4061                                      
 4062 !     Convert inputs to radians, the to the -pi to pi range
 4063 !     (incorporated from original function xunwrapV2.m)
 4064                                                        
 4065 !     Convert to radians
 4066       theta = theta*(PI/180)
 4067                             
 4068       theta = PI*((ABS(theta)/PI) - &
 4069              2*CEILING(((ABS(theta)/PI)-1)/2))*SIGN(1.,theta)
 4070                                                              
 
 
                                                         w3strkmd.F90  page  75
 
 
 4071 !     Shift the points in the -pi to 0 range to the pi to 2pi range
 4072       IF (theta.LT.0.) theta = theta + 2*PI
 4073                                            
 4074 !     Convert back to degrees and return value
 4075       mean_angleV2 = theta*(180/PI)
 4076                                    
 4077       RETURN
 4078       END FUNCTION mean_angleV2
 4079 !/ End of mean_angleV2 ----------------------------------------------- /
 4080 !/
 4081 !/ ------------------------------------------------------------------- /
 4082       REAL FUNCTION mean_angleV3(ang,hsign,ll)
 4083 !/
 4084 !/                  +-----------------------------------+
 4085 !/                  | WAVEWATCH III           NOAA/NCEP |
 4086 !/                  |     A. J. van der Westhuysen      |
 4087 !/                  |            Jeff Hanson            |
 4088 !/                  |        Eve-Marie Devaliere        |
 4089 !/                  |                        FORTRAN 95 |
 4090 !/                  | Last update :          4-Jan-2013 |
 4091 !/                  +-----------------------------------+
 4092 !/
 4093 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 4094 !/                  by Jeff Hanson & Eve-Marie Devaliere
 4095 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 4096 !/
 4097 !/    Copyright 2009-2013 National Weather Service (NWS),
 4098 !/       National Oceanic and Atmospheric Administration.  All rights
 4099 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 4100 !/       No unauthorized use without permission.
 4101 !/
 4102       IMPLICIT NONE
 4103 !
 4104 !  1. Purpose :
 4105 !
 4106 !     Compute the mean direction from array of directions,
 4107 !     INCLUDING WEIGHTING WITH HMO
 4108 !
 4109 !  2. Method
 4110 !
 4111 !     ang is a column vector of angles
 4112 !     m_ang is the mean from a unit-vector average of ang
 4113 !     Assumes clockwise rotation from North = 0.
 4114 !
 4115 !  3. Parameters :
 4116 !
 4117 !     Parameter list
 4118 !     ----------------------------------------------------------------
 4119 !     ang     Real   input    Array of angles to average
 4120 !     ll      Int    input    Length of ang
 4121 !
 4122       REAL :: ang(ll), hsign(ll)
 4123       REAL :: TEMP1, TEMP2
 4124       INTEGER :: ll
 4125 !
 
 
                                                         w3strkmd.F90  page  76
 
 
 4126 !     Local variables
 4127 !     ----------------------------------------------------------------
 4128 !     u,v       Real   Arrays of u,v dir components to average
 4129 !     um,vm     Real   Mean u,v dir components
 4130 !     theta     Real   Mean direction relative to North
 4131 !
 4132       REAL      :: PI
 4133       PARAMETER  (PI = 3.1416)
 4134       REAL      :: u(ll), v(ll), vm, um, theta
 4135       INTEGER   :: i
 4136 !
 4137 !  4. Subroutines used :
 4138 !
 4139 !      Name      Type  Module   Description
 4140 !     ----------------------------------------------------------------
 4141 !      -
 4142 !
 4143 !  5. Subroutines calling
 4144 !
 4145 !     findSys
 4146 !
 4147 !  6. Error messages :
 4148 !
 4149 !  7. Remarks :
 4150 !
 4151 !  8. Structure :
 4152 !
 4153 !     -
 4154 !
 4155 !  9. Switches :
 4156 !
 4157 !     None defined yet.
 4158 !
 4159 ! 10. Source code :
 4160 !
 4161 !/ ------------------------------------------------------------------- /
 4162                                                                         
 4163 !     North and East components
 4164       v(:) = COS(ang(:)*(PI/180.))
 4165       u(:) = SIN(ang(:)*(PI/180.))
 4166       TEMP1 = 0.
 4167       TEMP2 = 0.
 4168       DO i = 1,ll
 4169          TEMP1 = TEMP1 + (hsign(i)**2)*v(i)
 4170          TEMP2 = TEMP2 + (hsign(i)**2)*u(i)
 4171       END DO
 4172       vm = TEMP1/MAX(SUM(hsign**2),0.001)
 4173       um = TEMP2/MAX(SUM(hsign**2),0.001)
 4174                                          
 4175 !     Compute mean magnitude and direction relative to North (from Upolar>
 4176       theta = (ATAN2(um,vm))*(180/PI)
 4177                                      
 4178 !     Convert inputs to radians, the to the -pi to pi range
 4179 !     (incorporated from original function xunwrapV2.m)
 4180                                                        
 
 
                                                         w3strkmd.F90  page  77
 
 
 4181 !     Convert to radians
 4182       theta = theta*(PI/180)
 4183                             
 4184       theta = PI*((ABS(theta)/PI) - &
 4185              2*CEILING(((ABS(theta)/PI)-1)/2))*SIGN(1.,theta)
 4186                                                              
 4187 !     Shift the points in the -pi to 0 range to the pi to 2pi range
 4188       IF (theta.LT.0.) theta = theta + 2*PI
 4189                                            
 4190 !     Convert back to degrees and return value
 4191       mean_angleV3 = theta*(180/PI)
 4192                                    
 4193       RETURN
 4194       END FUNCTION mean_angleV3
 4195 !/ End of mean_angleV3 ----------------------------------------------- /
 4196 !/
 4197 !/ ------------------------------------------------------------------- /
 4198       SUBROUTINE UNIQUE (INARRAY,INSIZE,OUTARRAY,OUTSIZE)
 4199 !/
 4200 !/                  +-----------------------------------+
 4201 !/                  | WAVEWATCH III           NOAA/NCEP |
 4202 !/                  |     A. J. van der Westhuysen      |
 4203 !/                  |            Jeff Hanson            |
 4204 !/                  |        Eve-Marie Devaliere        |
 4205 !/                  |                        FORTRAN 95 |
 4206 !/                  | Last update :         22-Dec-2016 |
 4207 !/                  +-----------------------------------+
 4208 !/
 4209 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 4210 !/                  by Jeff Hanson & Eve-Marie Devaliere
 4211 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 4212 !/    12-Dec-2016 : Change algorithm from N*N to N*log(N)
 4213 !/                  (S. Zieger BoM Australia)           ( version 5.16 )
 4214 !/
 4215 !/    Copyright 2009-2013 National Weather Service (NWS),
 4216 !/       National Oceanic and Atmospheric Administration.  All rights
 4217 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 4218 !/       No unauthorized use without permission.
 4219 !/
 4220       IMPLICIT NONE
 4221 !
 4222 !  1. Purpose :
 4223 !
 4224 !     Returns the sorted elements that are unique in INARRAY.
 4225 !
 4226 !  2. Method
 4227 !
 4228 !     1. Sort input array with quicksort
 4229 !     2. Copy sequential-elements if the 'current' element
 4230 !        is not equal to the 'previous' element in array.
 4231 !
 4232 !  3. Parameters :
 4233 !
 4234 !     Parameter list
 4235 !     ----------------------------------------------------------------
 
 
                                                         w3strkmd.F90  page  78
 
 
 4236 !     INARRAY   REAL    ARR  input    Input array
 4237 !     INSIZE    INTEGER      input    Size of input array
 4238 !     OUTARRAY  REAL    ARR  output   Output array (sorted)
 4239 !     OUTSIZE   INTEGER      output   Size of output array (number of uni>
 4240 !
 4241       INTEGER, INTENT(IN)  :: INSIZE
 4242       INTEGER, INTENT(OUT) :: OUTSIZE
 4243       REAL, INTENT(IN)     :: INARRAY(INSIZE)
 4244       REAL, POINTER        :: OUTARRAY(:)
 4245 !
 4246 !     Local variables
 4247 !     ----------------------------------------------------------------
 4248       INTEGER          :: I, K
 4249       REAL             :: ARRAY(INSIZE), TEMP(INSIZE)
 4250 !
 4251 !  4. Subroutines used :
 4252 !
 4253 !      Name      Type  Scope    Description
 4254 !     ----------------------------------------------------------------
 4255 !      QSORT     Subr. Private  Quicksort algorithm
 4256 !
 4257 !  5. Subroutines calling
 4258 !
 4259 !     waveTracking_NWS_V2
 4260 !     findSys
 4261 !     printFinalSys
 4262 !     combineSys
 4263 !     findIJV4
 4264 !
 4265 !  6. Error messages :
 4266 !
 4267 !  7. Remarks :
 4268 !
 4269 !  8. Structure :
 4270 !
 4271 !     -
 4272 !
 4273 !  9. Switches :
 4274 !
 4275 !     None defined yet.
 4276 !
 4277 ! 10. Source code :
 4278 !
 4279 !/ ------------------------------------------------------------------- /
 4280       K = 1
 4281            
 4282       IF ( INSIZE.EQ.0 ) THEN
 4283         WRITE(20,*) '*** In Subr. UNIQUE: Input array has length=0!'
 4284       ELSE
 4285 !/    --- Setup input arrays and temporary arrays. ---
 4286         DO I=1,INSIZE
 4287           ARRAY(I) = INARRAY(I)
 4288           TEMP(I)  = REAL(I)
 4289         END DO
 4290 !/
 
 
                                                         w3strkmd.F90  page  79
 
 
 4291 !/      --- Sort input arrays (use temporary array to store indices). ---
 4292         CALL QSORT(ARRAY,TEMP,1,INSIZE)
 4293 !/
 4294 !/      --- Reset temporary array. ---
 4295         TEMP(:) = 9999.
 4296 !/
 4297 !/      --- Initialise first values and array index. ---
 4298         K = 1
 4299         TEMP(K) = ARRAY(K)
 4300         K = K + 1
 4301 !/      --- Iterate over elements in array. ---
 4302         DO I=2,INSIZE
 4303 !/        --- Compare sequential array values ('previous' less than 'next>
 4304 !/            and test against the last list element check in. ---
 4305           IF ( ARRAY(I).GT.ARRAY(I-1) .AND. &
 4306                ARRAY(I).GT.TEMP(K-1)        ) THEN
 4307             TEMP(K) = ARRAY(I)
 4308             K = K + 1
 4309           END IF
 4310         END DO
 4311               
 4312 !/      --- Allocate output array ---
 4313         OUTSIZE = K - 1
 4314         ALLOCATE(OUTARRAY(OUTSIZE))
 4315 !/      --- Transfer output from temporary array to output array. ---
 4316         IF ( OUTSIZE.GE.1 ) THEN
 4317           DO I=1,OUTSIZE
 4318             OUTARRAY(I) = TEMP(I)
 4319           END DO
 4320         END IF
 4321       END IF
 4322 !/
 4323       RETURN
 4324 !/
 4325       END SUBROUTINE UNIQUE
 4326 !/ End of UNIQUE ----------------------------------------------------- /
 4327 !/
 4328 !/ ------------------------------------------------------------------- /
 4329       SUBROUTINE SORT (INARRAY,INSIZE,OUTARRAY,IY,DIRECTION)
 4330 !/
 4331 !/                  +-----------------------------------+
 4332 !/                  | WAVEWATCH III           NOAA/NCEP |
 4333 !/                  |     A. J. van der Westhuysen      |
 4334 !/                  |            Jeff Hanson            |
 4335 !/                  |        Eve-Marie Devaliere        |
 4336 !/                  |                        FORTRAN 95 |
 4337 !/                  | Last update :         20-Dec-2016 |
 4338 !/                  +-----------------------------------+
 4339 !/
 4340 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 4341 !/                  by Jeff Hanson & Eve-Marie Devaliere
 4342 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 4343 !/    20-Dec-2016 : Add quicksort algorithm (S. Zieger) ( version 5.16 )
 4344 !/
 4345 !/    Copyright 2009-2013 National Weather Service (NWS),
 
 
                                                         w3strkmd.F90  page  80
 
 
 4346 !/       National Oceanic and Atmospheric Administration.  All rights
 4347 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 4348 !/
 4349       IMPLICIT NONE
 4350 !
 4351 !  1. Purpose :
 4352 !
 4353 !     Sorts the array INARRAY in ascending (Direction = 'A') or
 4354 !     descending (Direciton = 'D') order. The sorted array is
 4355 !     stored in OUTARRAY, and the sorted array of the original
 4356 !     indices is stored in IY.
 4357 !
 4358 !  2. Method
 4359 !
 4360 !     Sort algorithm based on quicksort.
 4361 !
 4362 !  3. Parameters :
 4363 !
 4364 !     Parameter list
 4365 !     ----------------------------------------------------------------
 4366 !     INARRAY   REAL    ARR  input    Input array
 4367 !     INSIZE    INTEGER      input    Size of input array
 4368 !     OUTARRAY  REAL    ARR  output   Sorted output array
 4369 !     IY        INTEGER ARR  output   Sorted array of the original indices
 4370                                                                           
 4371       CHARACTER         :: DIRECTION *1
 4372       INTEGER           :: INSIZE
 4373       INTEGER           :: IY(INSIZE)
 4374       REAL              :: INARRAY(INSIZE), OUTARRAY(INSIZE)
 4375                                                             
 4376       INTENT (IN)  INARRAY, INSIZE, DIRECTION
 4377       INTENT (OUT) OUTARRAY, IY
 4378 !
 4379 !     Local variables
 4380 !     ----------------------------------------------------------------
 4381 !      INARRAY - array of values to be sorted
 4382 !      IY -      array to be carried with X (all swaps of X elements are >
 4383 !                matched in IY .  After the sort IY(J) contains the origi>
 4384 !                postition of the value X(J) in the unsorted X array.
 4385 !      N -       number of values in array X to be sorted
 4386                                                          
 4387       INTEGER           :: I
 4388       REAL              :: IND(INSIZE)
 4389 !
 4390 !  4. Subroutines used :
 4391 !
 4392 !      Name      Type  Module   Description
 4393 !     ----------------------------------------------------------------
 4394 !      -
 4395 !
 4396 !  5. Subroutines calling
 4397 !
 4398 !     printFinalSys
 4399 !     combineSys
 4400 !     timeTrackingV2
 
 
                                                         w3strkmd.F90  page  81
 
 
 4401 !
 4402 !  6. Error messages :
 4403 !
 4404 !  7. Remarks :
 4405 !
 4406 !  8. Structure :
 4407 !
 4408 !     -
 4409 !
 4410 !  9. Switches :
 4411 !
 4412 !     None defined yet.
 4413 !
 4414 ! 10. Source code :
 4415 !
 4416 !/ ------------------------------------------------------------------- /
 4417                                                                         
 4418 !     Sort OUTARRAY in as/decending order
 4419                                          
 4420       IF (INSIZE.EQ.0) THEN
 4421          WRITE(20,*) '*** In Subr. SORT: Input array has length=0 !!!'
 4422       ELSE
 4423           
 4424         DO I = 1, INSIZE
 4425            OUTARRAY(I) = INARRAY(I)
 4426            IND(I) = REAL(I)
 4427         END DO
 4428               
 4429         IF (DIRECTION .EQ. 'A') THEN
 4430            CALL QSORT(OUTARRAY,IND,1,INSIZE)
 4431         ELSE IF (DIRECTION .EQ. 'D') THEN
 4432            CALL QSORT_DESC(OUTARRAY,IND,1,INSIZE)
 4433         END IF
 4434               
 4435       END IF
 4436 !
 4437 !/    --- Cast index array to integer. ---
 4438       DO I = 1, INSIZE
 4439          IY(I) = INT(IND(I))
 4440       END DO
 4441 !
 4442       RETURN
 4443 !
 4444       END SUBROUTINE SORT
 4445 !/ End of SORT ------------------------------------------------------- /
 4446 !/
 4447 !/ ------------------------------------------------------------------- /
 4448       SUBROUTINE SETDIFF (INARRAY1,   INSIZE1,  INARRAY2,   INSIZE2, &
 4449                           OUTARRAY,   OUTSIZE)
 4450 !/
 4451 !/                  +-----------------------------------+
 4452 !/                  | WAVEWATCH III           NOAA/NCEP |
 4453 !/                  |     A. J. van der Westhuysen      |
 4454 !/                  |            Jeff Hanson            |
 4455 !/                  |        Eve-Marie Devaliere        |
 
 
                                                         w3strkmd.F90  page  82
 
 
 4456 !/                  |                        FORTRAN 95 |
 4457 !/                  | Last update :         20-Dec-2016 |
 4458 !/                  +-----------------------------------+
 4459 !/
 4460 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 4461 !/                  by Jeff Hanson & Eve-Marie Devaliere
 4462 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 4463 !/    20-Dec-2016 : Add quicksort algorithm (S.Zieger)  ( version 5.16 )
 4464 !/
 4465 !/    Copyright 2009-2013 National Weather Service (NWS),
 4466 !/       National Oceanic and Atmospheric Administration.  All rights
 4467 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 4468 !/       No unauthorized use without permission.
 4469 !/
 4470       IMPLICIT NONE
 4471 !
 4472 !  1. Purpose :
 4473 !
 4474 !     (i)  Returns the elements in INARRAY1 that are not in INARRAY2.
 4475 !     (ii) Sort the resulting array in ascending order.
 4476 !
 4477 !  2. Method
 4478 !
 4479 !  3. Parameters :
 4480 !
 4481 !     Parameter list
 4482 !     ----------------------------------------------------------------
 4483 !     INARRAY1  REAL    ARR  input    Input array
 4484 !     INSIZE1   INTEGER      input    Size of input array
 4485 !     INARRAY2  REAL    ARR  input    Input array
 4486 !     INSIZE2   INTEGER      input    Size of input array
 4487 !     OUTARRAY  REAL    ARR  output   Output array
 4488 !     OUTSIZE   INTEGER      output   Size of output array (number of uni>
 4489                                                                           
 4490       INTEGER           :: INSIZE1, INSIZE2, OUTSIZE
 4491       REAL              :: INARRAY1(INSIZE1), INARRAY2(INSIZE2)
 4492       REAL, POINTER     :: OUTARRAY(:)
 4493                                       
 4494       INTENT (IN)  INARRAY1, INSIZE1, INARRAY2, INSIZE2
 4495       INTENT (OUT) OUTSIZE
 4496 !
 4497 !     Local variables
 4498 !     ----------------------------------------------------------------
 4499       INTEGER          :: I,J,K
 4500       REAL             :: TEMP(INSIZE1)
 4501       REAL             :: ARRAY1(INSIZE1),ARRAY2(INSIZE2)
 4502       REAL             :: ID1(INSIZE1),ID2(INSIZE2)
 4503       LOGICAL          :: LOOP
 4504 !
 4505 !  4. Subroutines used :
 4506 !
 4507 !      Name      Type  Scope    Description
 4508 !     ----------------------------------------------------------------
 4509 !      QSORT     Subr. Private  Quicksort algorithm
 4510 !
 
 
                                                         w3strkmd.F90  page  83
 
 
 4511 !  5. Subroutines calling
 4512 !
 4513 !     printFinalSys
 4514 !     combineSys
 4515 !     timeTrackingV2
 4516 !
 4517 !  6. Error messages :
 4518 !
 4519 !  7. Remarks :
 4520 !
 4521 !  8. Structure :
 4522 !
 4523 !     -
 4524 !
 4525 !  9. Switches :
 4526 !
 4527 !     None defined yet.
 4528 !
 4529 ! 10. Source code :
 4530 !
 4531 !/ ------------------------------------------------------------------- /
 4532       IF ( (INSIZE1).EQ.0 ) THEN
 4533         OUTSIZE = 0
 4534         ALLOCATE(OUTARRAY(OUTSIZE))
 4535       ELSE IF ( INSIZE2.EQ.0 ) THEN
 4536         CALL UNIQUE(INARRAY1,INSIZE1,OUTARRAY,OUTSIZE)
 4537       ELSE
 4538 !/      --- Setup input arrays. ---
 4539         DO I=1,INSIZE1
 4540           ARRAY1(I) = INARRAY1(I)
 4541           ID1(I)    = REAL(I)
 4542         END DO
 4543         DO I=1,INSIZE2
 4544           ARRAY2(I) = INARRAY2(I)
 4545           ID2(I)    = REAL(I)
 4546         END DO
 4547 !/
 4548 !/      --- Sort input arrays. ---
 4549         CALL QSORT(ARRAY1,ID1,1,INSIZE1)
 4550         CALL QSORT(ARRAY2,ID2,1,INSIZE2)
 4551 !/
 4552 !/      --- Initialise indices. ---
 4553         I = 1
 4554         J = 1
 4555         K = 1
 4556 !/
 4557 !/      --- Allocate and initialize temporary output ---
 4558         TEMP(:) = 9999.
 4559 !/
 4560 !/      --- Loop though both arrays by incrementing I,J. ---
 4561         LOOP = .TRUE.
 4562         DO WHILE ( LOOP )
 4563 !/
 4564             IF ( ARRAY1(I).LT.ARRAY2(J) .OR.   &
 4565                  ARRAY1(I).GT.ARRAY2(INSIZE2)  ) THEN
 
 
                                                         w3strkmd.F90  page  84
 
 
 4566 !/            --- Populate output array. Check for dumplicates
 4567 !/                in output array. ---
 4568               IF ( K.EQ.1 ) THEN
 4569                 TEMP(K) = ARRAY1(I)
 4570                 K = K + 1
 4571               ELSE IF ( TEMP(K-1).LT.ARRAY1(I) ) THEN
 4572                 TEMP(K) = ARRAY1(I)
 4573                 K = K + 1
 4574               END IF
 4575               I = I + 1
 4576             ELSE IF ( ARRAY2(J).LT.ARRAY1(I) ) THEN
 4577               J = J + 1
 4578             ELSE
 4579               I = I + 1
 4580               J = J + 1
 4581             END IF
 4582 !/          --- Check for exit the loop. ---
 4583             IF ( I.GT.INSIZE1 ) THEN
 4584               LOOP = .FALSE.
 4585             END IF
 4586 !/          --- Make sure array pointer I,J are within array bounds. ---
 4587             I = MIN(I,INSIZE1)
 4588             J = MIN(J,INSIZE2)
 4589 !/
 4590         END DO
 4591 !/
 4592 !/      --- Allocate output array ---
 4593         OUTSIZE = K-1
 4594         ALLOCATE(OUTARRAY(OUTSIZE))
 4595 !/        --- Transfer output from temporary array to output array. ---
 4596         DO I=1,OUTSIZE
 4597           OUTARRAY(I) = TEMP(I)
 4598         END DO
 4599       END IF
 4600 !/
 4601       RETURN
 4602 !/
 4603       END SUBROUTINE SETDIFF
 4604 !/ End of SETDIFF ---------------------------------------------------- /
 4605 !/
 4606 !/ ------------------------------------------------------------------- /
 4607       SUBROUTINE INTERSECT (INARRAY1 ,INSIZE1  ,INARRAY2 ,INSIZE2  , &
 4608                             OUTARRAY ,OUTSIZE  ,IND1     ,IND2     )
 4609 !/
 4610 !/                  +-----------------------------------+
 4611 !/                  | WAVEWATCH III           NOAA/NCEP |
 4612 !/                  |     A. J. van der Westhuysen      |
 4613 !/                  |            Jeff Hanson            |
 4614 !/                  |        Eve-Marie Devaliere        |
 4615 !/                  |                        FORTRAN 95 |
 4616 !/                  | Last update :         20-Dec-2016 |
 4617 !/                  +-----------------------------------+
 4618 !/
 4619 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 4620 !/                  by Jeff Hanson & Eve-Marie Devaliere
 
 
                                                         w3strkmd.F90  page  85
 
 
 4621 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 4622 !/    20-Dec-2016 : Add count-histogram method based on
 4623 !/                  algorithm from Mirko Velic (BoM)
 4624 !/                  (S. Zieger BoM, Australia)          ( version 5.16 )
 4625 !/
 4626 !/    Copyright 2009-2013 National Weather Service (NWS),
 4627 !/       National Oceanic and Atmospheric Administration.  All rights
 4628 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 4629 !/       No unauthorized use without permission.
 4630 !/
 4631       IMPLICIT NONE
 4632 !
 4633 !  1. Purpose :
 4634 !
 4635 !     (i)  Returns the elements that are mutual in INARRAY1 and INARRAY2.
 4636 !     (ii) Sort the resulting array in ascending order.
 4637 !
 4638 !  2. Method
 4639 !
 4640 !     Sort with counting/histogram method with input array being
 4641 !     cast as integer.
 4642 !
 4643 !  3. Parameters :
 4644 !
 4645 !     Parameter list
 4646 !     ----------------------------------------------------------------
 4647 !     INARRAY1  REAL    ARR  input    Input array
 4648 !     INSIZE1   INTEGER      input    Size of input array
 4649 !     INARRAY2  REAL    ARR  input    Input array
 4650 !     INSIZE2   INTEGER      input    Size of input array
 4651 !     OUTARRAY  REAL    ARR  output   Output array
 4652 !     OUTSIZE   INTEGER      output   Size of output array (number of
 4653 !     intersects)
 4654 !
 4655       INTEGER           :: INSIZE1, INSIZE2, OUTSIZE
 4656       REAL              :: INARRAY1(INSIZE1), INARRAY2(INSIZE2)
 4657       REAL, POINTER :: OUTARRAY(:)
 4658       REAL, POINTER :: IND1(:), IND2(:)
 4659 !
 4660       INTENT (IN)  INARRAY1, INSIZE1, INARRAY2, INSIZE2
 4661       INTENT (OUT) OUTSIZE
 4662 !
 4663 !     Local variables
 4664 !     ----------------------------------------------------------------
 4665 !     VIDX1, VIDX2 - array(s) in which the value is represented by
 4666 !                    its index (i.e. histogram with frequency 1)
 4667 !     N            - data range and size of possible intersections.
 4668 !
 4669       LOGICAL,ALLOCATABLE :: VIDX1(:),VIDX2(:)
 4670       INTEGER,ALLOCATABLE :: IPOS1(:),IPOS2(:)
 4671 !
 4672       INTEGER             :: I, J
 4673       INTEGER             :: N, IMIN, IMAX
 4674       INTEGER             :: MINV1,MAXV1, MINV2, MAXV2
 4675 !
 
 
                                                         w3strkmd.F90  page  86
 
 
 4676 !  4. Subroutines used :
 4677 !
 4678 !      Name      Type  Scope    Description
 4679 !     ----------------------------------------------------------------
 4680 !
 4681 !  5. Subroutines calling
 4682 !
 4683 !  6. Error messages :
 4684 !
 4685 !  7. Remarks :
 4686 !
 4687 !  8. Structure :
 4688 !
 4689 !     -
 4690 !
 4691 !  9. Switches :
 4692 !
 4693 !     None defined yet.
 4694 !
 4695 ! 10. Source code :
 4696 !
 4697 !/ ------------------------------------------------------------------- /
 4698 !
 4699       OUTSIZE = 0
 4700                  
 4701 !/    --- Calculate the range of the two sets. ---
 4702       MINV1  = INT(MINVAL(INARRAY1))
 4703       MAXV1  = INT(MAXVAL(INARRAY1))
 4704       MINV2  = INT(MINVAL(INARRAY2))
 4705       MAXV2  = INT(MAXVAL(INARRAY2))
 4706                                     
 4707 !/    --- Check if ranges overlap. ---
 4708       IF ( MAXV1.LT.MINV2.OR.INSIZE1.EQ.0.OR.INSIZE2.EQ.0 ) THEN
 4709         ALLOCATE(OUTARRAY(OUTSIZE))
 4710         ALLOCATE(IND1(OUTSIZE))
 4711         ALLOCATE(IND2(OUTSIZE))
 4712       ELSE
 4713 !/      --- Calculate size of temporary output arrays. Allow
 4714 !/          extra elements: ZERO, and make sure index is 1:N. ---
 4715         IMIN = MIN(MINV1,MINV2)-1
 4716         IMAX = MAX(MAXV1,MAXV2)+1
 4717                                  
 4718         N = IMAX-IMIN
 4719                      
 4720         ALLOCATE(VIDX1(N),VIDX2(N))
 4721         ALLOCATE(IPOS1(N),IPOS2(N))
 4722                                    
 4723         VIDX1(1:N) = .FALSE.
 4724         VIDX2(1:N) = .FALSE.
 4725                             
 4726         DO I=1,INSIZE1
 4727           J = INT(INARRAY1(I)-IMIN)
 4728           VIDX1(J) = .TRUE.
 4729           IPOS1(J) = I
 4730         END DO
 
 
                                                         w3strkmd.F90  page  87
 
 
 4731               
 4732         DO I=1,INSIZE2
 4733           J = INT(INARRAY2(I)-IMIN)
 4734 !/        --- Intersect arrays and check for
 4735 !/            duplicate elements in array2. ---
 4736           IF  ( VIDX1(J).AND..NOT.VIDX2(J) ) THEN
 4737             OUTSIZE = OUTSIZE + 1
 4738             VIDX2(J) = .TRUE.
 4739             IPOS2(J) = I
 4740           END IF
 4741         END DO
 4742 !/      --- Allocate output arrays. ---
 4743         ALLOCATE(OUTARRAY(OUTSIZE))
 4744         ALLOCATE(IND1(OUTSIZE))
 4745         ALLOCATE(IND2(OUTSIZE))
 4746 !/      --- Transfer contents. ---
 4747         I = 1
 4748         DO J=1,N
 4749           IF ( VIDX1(J).AND.VIDX2(J).AND.I.LE.OUTSIZE ) THEN
 4750             OUTARRAY(I) = INARRAY1(IPOS1(J))
 4751             IND1(I)     = IPOS1(J)
 4752             IND2(I)     = IPOS2(J)
 4753             I = I + 1
 4754           END IF
 4755         END DO
 4756 !/      --- Free memory. ---
 4757         DEALLOCATE(VIDX1,VIDX2)
 4758         DEALLOCATE(IPOS1,IPOS2)
 4759       END IF
 4760 !/
 4761       RETURN
 4762 !/
 4763       END SUBROUTINE INTERSECT
 4764 !/ End of INTERSECT -------------------------------------------------- /
 4765 !/
 4766 !/ ------------------------------------------------------------------- /
 4767       SUBROUTINE UNION (INARRAY1,   INSIZE1,  INARRAY2,   INSIZE2, &
 4768                         OUTARRAY,   OUTSIZE)
 4769 !/
 4770 !/                  +-----------------------------------+
 4771 !/                  | WAVEWATCH III           NOAA/NCEP |
 4772 !/                  |     A. J. van der Westhuysen      |
 4773 !/                  |            Jeff Hanson            |
 4774 !/                  |        Eve-Marie Devaliere        |
 4775 !/                  |                        FORTRAN 95 |
 4776 !/                  | Last update :          4-Jan-2013 |
 4777 !/                  +-----------------------------------+
 4778 !/
 4779 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 4780 !/                  by Jeff Hanson & Eve-Marie Devaliere
 4781 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 4782 !/    20-Dec-2016 : Add count-histogram method similarly
 4783 !/                  to INTERSECT (S. Zieger)            ( version 5.16 )
 4784 !/
 4785 !/    Copyright 2009-2013 National Weather Service (NWS),
 
 
                                                         w3strkmd.F90  page  88
 
 
 4786 !/       National Oceanic and Atmospheric Administration.  All rights
 4787 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 4788 !/       No unauthorized use without permission.
 4789 !/
 4790       IMPLICIT NONE
 4791 !
 4792 !  1. Purpose :
 4793 !
 4794 !     (i)  Returns the union of INARRAY1 and INARRAY2.
 4795 !     (ii) Sort the resulting array in ascending order.
 4796 !
 4797 !  2. Method
 4798 !
 4799 !  3. Parameters :
 4800 !
 4801 !     Parameter list
 4802 !     ----------------------------------------------------------------
 4803 !     INARRAY   REAL    ARR  input    Input array
 4804 !     INSIZE    INTEGER      input    Size of input array
 4805 !     OUTARRAY  REAL    ARR  output   Output array (sorted)
 4806 !     OUTSIZE   INTEGER      output   Size of output array (number of
 4807 !                                     unique elements)
 4808       INTEGER           :: INSIZE1, INSIZE2, OUTSIZE
 4809       REAL              :: INARRAY1(INSIZE1), INARRAY2(INSIZE2)
 4810       REAL, POINTER     :: OUTARRAY(:)
 4811 !
 4812       INTENT (IN)  INARRAY1, INSIZE1, INARRAY2, INSIZE2
 4813       INTENT (OUT) OUTSIZE
 4814 !
 4815 !     Local variables
 4816 !     ----------------------------------------------------------------
 4817 !     VIDX1, VIDX2 - array(s) in which the value is represented by
 4818 !                    its index (i.e. histogram with frequency 1)
 4819 !     N            - data range and size of possible intersections.
 4820 !
 4821       LOGICAL,ALLOCATABLE :: VIDX1(:),VIDX2(:)
 4822       INTEGER,ALLOCATABLE :: IPOS1(:),IPOS2(:)
 4823       REAL,ALLOCATABLE    :: TEMP(:)
 4824 !
 4825       INTEGER             :: I, J
 4826       INTEGER             :: N, IMIN, IMAX
 4827       INTEGER             :: MINV1,MAXV1, MINV2, MAXV2
 4828 !
 4829 !  4. Subroutines used :
 4830 !
 4831 !      Name      Type  Module   Description
 4832 !     ----------------------------------------------------------------
 4833 !      QSORT     Subr. Private  Quicksort algorithm
 4834 !
 4835 !  5. Subroutines calling
 4836 !
 4837 !     combineSys
 4838 !
 4839 !  6. Error messages :
 4840 !
 
 
                                                         w3strkmd.F90  page  89
 
 
 4841 !  7. Remarks :
 4842 !
 4843 !  8. Structure :
 4844 !
 4845 !     -
 4846 !
 4847 !  9. Switches :
 4848 !
 4849 !     None defined yet.
 4850 !
 4851 ! 10. Source code :
 4852 !
 4853 !/ ------------------------------------------------------------------- /
 4854 !/    --- Setup input arrays. ---
 4855       IF ( (INSIZE1+INSIZE2).EQ.0 ) THEN
 4856         OUTSIZE = 0
 4857         ALLOCATE(OUTARRAY(OUTSIZE))
 4858                                    
 4859       ELSEIF ( INSIZE1.EQ.0 ) THEN
 4860         OUTSIZE = INSIZE2
 4861         ALLOCATE(OUTARRAY(OUTSIZE))
 4862         ALLOCATE(TEMP(OUTSIZE))
 4863                                
 4864         DO I=1,OUTSIZE
 4865          OUTARRAY(I) = INARRAY2(I)
 4866          TEMP(I)     = REAL(I)
 4867         END DO
 4868         CALL QSORT(OUTARRAY,TEMP,1,OUTSIZE)
 4869                                            
 4870       ELSEIF ( INSIZE2.EQ.0 ) THEN
 4871         OUTSIZE = INSIZE1
 4872         ALLOCATE(OUTARRAY(OUTSIZE),TEMP(OUTSIZE))
 4873                                                  
 4874         DO I=1,OUTSIZE
 4875          OUTARRAY(I) = INARRAY1(I)
 4876          TEMP(I)     = REAL(I)
 4877         END DO
 4878         CALL QSORT(OUTARRAY,TEMP,1,OUTSIZE)
 4879                                            
 4880       ELSE
 4881         OUTSIZE = 0
 4882 !/      --- Calculate the range of the two sets. ---
 4883         MINV1  = INT(MINVAL(INARRAY1))
 4884         MAXV1  = INT(MAXVAL(INARRAY1))
 4885         MINV2  = INT(MINVAL(INARRAY2))
 4886         MAXV2  = INT(MAXVAL(INARRAY2))
 4887 !
 4888 !/  --- Allow extra elementes: ZERO, and make sure index is 1:N. ---
 4889         IMIN = MIN(MINV1,MINV2)-1
 4890         IMAX = MAX(MAXV1,MAXV2)+1
 4891                                  
 4892         N = IMAX-IMIN
 4893                      
 4894         ALLOCATE(VIDX1(N),VIDX2(N))
 4895         ALLOCATE(IPOS1(N),IPOS2(N))
 
 
                                                         w3strkmd.F90  page  90
 
 
 4896                                    
 4897         VIDX1(1:N) = .FALSE.
 4898         VIDX2(1:N) = .FALSE.
 4899         IPOS1(1:N) = -9999
 4900         IPOS2(1:N) = -9999
 4901 !/
 4902         DO I=1,INSIZE1
 4903           J = INT(INARRAY1(I)-IMIN)
 4904           IF  ( .NOT.VIDX1(J) ) THEN
 4905             OUTSIZE = OUTSIZE + 1
 4906             VIDX1(J) = .TRUE.
 4907             IPOS1(J) = I
 4908           END IF
 4909         END DO
 4910               
 4911         DO I=1,INSIZE2
 4912           J = INT(INARRAY2(I)-IMIN)
 4913           IF  ( .NOT.VIDX1(J).AND..NOT.VIDX2(J) ) THEN
 4914             OUTSIZE = OUTSIZE + 1
 4915             VIDX2(J) = .TRUE.
 4916             IPOS2(J) = I
 4917           END IF
 4918         END DO
 4919               
 4920         ALLOCATE(OUTARRAY(OUTSIZE))
 4921                                    
 4922         I = 1
 4923         DO J=1,N
 4924           IF ( VIDX1(J).AND.I.LE.OUTSIZE ) THEN
 4925             OUTARRAY(I) = INARRAY1(IPOS1(J))
 4926             I = I + 1
 4927           ELSEIF ( VIDX2(J).AND.I.LE.OUTSIZE ) THEN
 4928             OUTARRAY(I) = INARRAY2(IPOS2(J))
 4929             I = I + 1
 4930           END IF
 4931         END DO
 4932               
 4933         DEALLOCATE(VIDX1,VIDX2)
 4934         DEALLOCATE(IPOS1,IPOS2)
 4935                                
 4936       END IF
 4937 !/
 4938       RETURN
 4939 !/
 4940       END SUBROUTINE UNION
 4941 !/ End of UNION ------------------------------------------------------ /
 4942 !/
 4943 !/ ------------------------------------------------------------------- /
 4944       INTEGER FUNCTION LENGTH(ARRAY,ARRSIZE,VAL)
 4945 !/
 4946 !/                  +-----------------------------------+
 4947 !/                  | WAVEWATCH III           NOAA/NCEP |
 4948 !/                  |     A. J. van der Westhuysen      |
 4949 !/                  |            Jeff Hanson            |
 4950 !/                  |        Eve-Marie Devaliere        |
 
 
                                                         w3strkmd.F90  page  91
 
 
 4951 !/                  |                        FORTRAN 95 |
 4952 !/                  | Last update :          4-Jan-2013 |
 4953 !/                  +-----------------------------------+
 4954 !/
 4955 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 4956 !/                  by Jeff Hanson & Eve-Marie Devaliere
 4957 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 4958 !/
 4959 !/    Copyright 2009-2013 National Weather Service (NWS),
 4960 !/       National Oceanic and Atmospheric Administration.  All rights
 4961 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 4962 !/       No unauthorized use without permission.
 4963 !/
 4964       IMPLICIT NONE
 4965 !
 4966 !  1. Purpose :
 4967 !
 4968 !     Find largest index in ARRAY with a value not equal to the
 4969 !     filler value VAL.
 4970 !     E.g. If VAL = 9999. and ARRAY = [X X X X 9999. 9999. 9999.],
 4971 !     the function returns 4.
 4972 !
 4973 !  3. Parameters :
 4974 !
 4975 !     Parameter list
 4976 !     ----------------------------------------------------------------
 4977       INTEGER :: ARRSIZE
 4978       REAL :: ARRAY(ARRSIZE)
 4979       REAL :: VAL
 4980 !
 4981 !     Local variables
 4982 !     ----------------------------------------------------------------
 4983       REAL :: FIELD
 4984       INTEGER :: I
 4985 !
 4986 ! 10. Source code :
 4987 !
 4988 !/ ------------------------------------------------------------------- /
 4989       IF (ARRSIZE.GT.0) THEN
 4990          I = 1
 4991          FIELD = ARRAY(I)
 4992          DO WHILE (FIELD.NE.VAL)
 4993             I = I+1
 4994             IF (I.GT.SIZE(ARRAY)) EXIT
 4995             FIELD = ARRAY(I)
 4996          END DO
 4997          LENGTH = I-1
 4998       ELSE
 4999          LENGTH = 0
 5000       END IF
 5001             
 5002       RETURN
 5003       END FUNCTION LENGTH
 5004 !/ End of LENGTH ----------------------------------------------------- /
 5005 !/
 
 
                                                         w3strkmd.F90  page  92
 
 
 5006 !/ ------------------------------------------------------------------- /
 5007       INTEGER FUNCTION FINDFIRST(ARRAY,ARRSIZE,VAL)
 5008 !/
 5009 !/                  +-----------------------------------+
 5010 !/                  | WAVEWATCH III           NOAA/NCEP |
 5011 !/                  |     A. J. van der Westhuysen      |
 5012 !/                  |            Jeff Hanson            |
 5013 !/                  |        Eve-Marie Devaliere        |
 5014 !/                  |                        FORTRAN 95 |
 5015 !/                  | Last update :          4-Jan-2013 |
 5016 !/                  +-----------------------------------+
 5017 !/
 5018 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 5019 !/                  by Jeff Hanson & Eve-Marie Devaliere
 5020 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 5021 !/
 5022 !/    Copyright 2009-2013 National Weather Service (NWS),
 5023 !/       National Oceanic and Atmospheric Administration.  All rights
 5024 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 5025 !/       No unauthorized use without permission.
 5026 !/
 5027       IMPLICIT NONE
 5028 !
 5029 !  1. Purpose :
 5030 !
 5031 !     Fast algorithm to find the *first* index IND in ARRAY
 5032 !     for which ARRAY(IND) = VAL. Use only when there are
 5033 !     no duplicates in ARRAY!
 5034 !
 5035 !  3. Parameters :
 5036 !
 5037 !     Parameter list
 5038 !     ----------------------------------------------------------------
 5039       INTEGER :: ARRSIZE
 5040       REAL :: ARRAY(ARRSIZE)
 5041       REAL :: VAL
 5042 !
 5043 !     Local variables
 5044 !     ----------------------------------------------------------------
 5045       INTEGER :: IND
 5046 !
 5047 ! 10. Source code :
 5048 !
 5049 !/ ------------------------------------------------------------------- /
 5050       IND = 1
 5051       DO WHILE (IND.LE.ARRSIZE)
 5052          IF ( ARRAY(IND).EQ.VAL ) EXIT
 5053          IND = IND + 1
 5054       END DO
 5055       IF (IND.GT.ARRSIZE) THEN
 5056          FINDFIRST = 0
 5057       ELSE
 5058          FINDFIRST = IND
 5059       ENDIF
 5060            
 
 
                                                         w3strkmd.F90  page  93
 
 
 5061       RETURN
 5062       END FUNCTION FINDFIRST
 5063 !/ End of FINDFIRST -------------------------------------------------- /
 5064 !/
 5065 !/ ------------------------------------------------------------------- /
 5066       REAL FUNCTION STD(ARRAY,N)
 5067 !/
 5068 !/                  +-----------------------------------+
 5069 !/                  | WAVEWATCH III           NOAA/NCEP |
 5070 !/                  |     A. J. van der Westhuysen      |
 5071 !/                  |            Jeff Hanson            |
 5072 !/                  |        Eve-Marie Devaliere        |
 5073 !/                  |                        FORTRAN 95 |
 5074 !/                  | Last update :          4-Jan-2013 |
 5075 !/                  +-----------------------------------+
 5076 !/
 5077 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 5078 !/                  by Jeff Hanson & Eve-Marie Devaliere
 5079 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 5080 !/
 5081 !/    Copyright 2009-2013 National Weather Service (NWS),
 5082 !/       National Oceanic and Atmospheric Administration.  All rights
 5083 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 5084 !/       No unauthorized use without permission.
 5085 !/
 5086       IMPLICIT NONE
 5087 !
 5088 !  1. Purpose :
 5089 !
 5090 !     Computes standard deviation.
 5091 !
 5092 !  3. Parameters :
 5093 !
 5094 !     Parameter list
 5095 !     ----------------------------------------------------------------
 5096 !     ARRAY    REAL    Input array for which to compute the std dev.
 5097 !     N        INT     Size of ARRAY
 5098 !
 5099       REAL    :: ARRAY(N)
 5100       INTEGER :: N
 5101 !
 5102 !     Local variables
 5103 !     ----------------------------------------------------------------
 5104       REAL    :: MN
 5105 !
 5106 ! 10. Source code :
 5107 !
 5108 !/ ------------------------------------------------------------------- /
 5109       IF (N.GT.1) THEN
 5110          MN = SUM(ARRAY)/N
 5111          STD = SQRT( 1/(REAL(N)-1)*SUM( (ARRAY(:)-MN)**2 ) )
 5112       ELSE
 5113          STD = 0.
 5114       END IF
 5115             
 
 
                                                         w3strkmd.F90  page  94
 
 
 5116       RETURN
 5117       END FUNCTION STD
 5118 !/ End of STD -------------------------------------------------------- /
 5119 !/
 5120       RECURSIVE SUBROUTINE QSORT(ARRAY,IDX,LO,HI)
 5121 !/
 5122 !/                  +-----------------------------------+
 5123 !/                  | WAVEWATCH III           NOAA/NCEP |
 5124 !/                  |            Stefan Zieger          |
 5125 !/                  |                        FORTRAN 95 |
 5126 !/                  | Last update :          6-Sep-2016 |
 5127 !/                  +-----------------------------------+
 5128 !/
 5129 !/    06-Sep-2016 : Origination, based on code by Mirko ( version 5.16 )
 5130 !/                  Velic (BoM, Australia)
 5131 !/
 5132 !/    Copyright 2009-2013 National Weather Service (NWS),
 5133 !/       National Oceanic and Atmospheric Administration.  All rights
 5134 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 5135 !/       No unauthorized use without permission.
 5136 !/
 5137 !
 5138 !  1. Purpose :
 5139 !
 5140 !     Quicksort algorithm.
 5141 !
 5142 !  2. Method
 5143 !
 5144 !  3. Parameters :
 5145 !
 5146 !     Parameter list
 5147 !     ----------------------------------------------------------------
 5148 !     ARRAY   REAL    ARR  in/out   Input array
 5149 !     IDX     REAL    ARR  in/out   Original indices of input array
 5150 !     LO      INTEGER      input    First element
 5151 !     HI      INTEGER      input    Last element
 5152 !
 5153       IMPLICIT NONE
 5154 !/
 5155       INTEGER, INTENT(IN) :: LO,HI
 5156       REAL,INTENT(INOUT)  :: ARRAY(:),IDX(:)
 5157 !/
 5158 !     Local variables
 5159 !     ----------------------------------------------------------------
 5160       LOGICAL :: LOOP
 5161       INTEGER :: TOP, BOT
 5162       REAL    :: VAL, TMP
 5163 !
 5164 !  4. Subroutines used :
 5165 !
 5166 !      Name      Type  Scope    Description
 5167 !     ----------------------------------------------------------------
 5168 !
 5169 !  5. Subroutines calling
 5170 !
 
 
                                                         w3strkmd.F90  page  95
 
 
 5171 !  6. Error messages :
 5172 !
 5173 !  7. Remarks :
 5174 !
 5175 !  8. Structure :
 5176 !
 5177 !  9. Switches :
 5178 !
 5179 ! 10. Source code :
 5180 !
 5181 !/ ------------------------------------------------------------------- /
 5182 !
 5183 !/    --- Check array size and bounds. ---
 5184       IF ( SIZE(ARRAY).EQ. 0 ) THEN
 5185         WRITE(6,199)
 5186         CALL ABORT
 5187       ELSE IF ( SIZE(ARRAY).NE.SIZE(IDX) ) THEN
 5188         WRITE(6,201)
 5189         CALL ABORT
 5190       ELSE IF ( LBOUND(ARRAY,1).GT.LO ) THEN
 5191         WRITE(6,203)
 5192         CALL ABORT
 5193       ELSE IF ( UBOUND(ARRAY,1).LT.HI ) THEN
 5194         WRITE(6,205)
 5195         CALL ABORT
 5196       END IF
 5197 !
 5198       TOP = LO
 5199       BOT = HI
 5200       VAL = ARRAY(INT((LO+HI)/2))
 5201 !
 5202       LOOP = .TRUE.
 5203       DO WHILE ( LOOP )
 5204         DO WHILE ( ARRAY(TOP).LT.VAL )
 5205           TOP = TOP + 1
 5206         END DO
 5207         DO WHILE ( VAL.LT.ARRAY(BOT) )
 5208           BOT = BOT - 1
 5209         END DO
 5210         IF ( TOP.LT.BOT ) THEN
 5211 !/        --- Swap values at indices TOP and BOT ---
 5212           TMP = ARRAY(TOP)
 5213           ARRAY(TOP) = ARRAY(BOT)
 5214           ARRAY(BOT) = TMP
 5215 !/        --- Swap index values at indices TOP and BOT ---
 5216           TMP = IDX(TOP)
 5217           IDX(TOP) = IDX(BOT)
 5218           IDX(BOT) = TMP
 5219 !
 5220           TOP = TOP + 1
 5221           BOT = BOT - 1
 5222         ELSE
 5223           LOOP = .FALSE.
 5224         END IF
 5225               
 
 
                                                         w3strkmd.F90  page  96
 
 
 5226       END DO
 5227             
 5228 !/    --- Recursive call quicksort ---
 5229       IF (LO.LT.TOP-1) CALL QSORT(ARRAY,IDX,LO,TOP-1)
 5230       IF (BOT+1.LT.HI) CALL QSORT(ARRAY,IDX,BOT+1,HI)
 5231 !
 5232       RETURN
 5233 !/
 5234   199 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/            &
 5235                '     QSORT ARRAY IS EMPTY'                           )
 5236   201 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/            &
 5237                '     QSORT ARRAY SIZE AND INDEX ARRAY SIZE MISMATCH' )
 5238   203 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/            &
 5239                '     QSORT ARRAY INDEX OUT OF LOWER BOUND'           )
 5240   205 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/            &
 5241                '     QSORT ARRAY INDEX OUT OF UPPER BOUND'           )
 5242 !/
 5243       END SUBROUTINE QSORT
 5244 !/ ------------------------------------------------------------------- /
 5245 !/
 5246       RECURSIVE SUBROUTINE QSORT_DESC(ARRAY,IDX,LO,HI)
 5247 !/
 5248 !/                  +-----------------------------------+
 5249 !/                  | WAVEWATCH III           NOAA/NCEP |
 5250 !/                  |            Stefan Zieger          |
 5251 !/                  |                        FORTRAN 95 |
 5252 !/                  | Last update :          6-Sep-2016 |
 5253 !/                  +-----------------------------------+
 5254 !/
 5255 !/    06-Sep-2016 : Origination, based on code by Mirko ( version 5.16 )
 5256 !/                  Velic (BoM, AUstralia)
 5257 !/
 5258 !/    Copyright 2009-2013 National Weather Service (NWS),
 5259 !/       National Oceanic and Atmospheric Administration.  All rights
 5260 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 5261 !/       No unauthorized use without permission.
 5262 !/
 5263 !
 5264 !  1. Purpose :
 5265 !
 5266 !     Quicksort algorithm with descending sort order.
 5267 !
 5268 !  2. Method
 5269 !
 5270 !  3. Parameters :
 5271 !
 5272 !     Parameter list
 5273 !     ----------------------------------------------------------------
 5274 !     ARRAY   REAL    ARR  in/out   Input array
 5275 !     LO      INTEGER      input    First element
 5276 !     HI      INTEGER      input    Last element
 5277 !
 5278       IMPLICIT NONE
 5279 !/
 5280       INTEGER, INTENT(IN) :: LO,HI
 
 
                                                         w3strkmd.F90  page  97
 
 
 5281       REAL,INTENT(INOUT)  :: ARRAY(:),IDX(:)
 5282 !/
 5283 !     Local variables
 5284 !     ----------------------------------------------------------------
 5285       INTEGER :: TOP, BOT, I
 5286       REAL    :: VAL, TMP
 5287       LOGICAL :: LOOP
 5288 !
 5289 !  4. Subroutines used :
 5290 !
 5291 !      Name      Type  Scope    Description
 5292 !     ----------------------------------------------------------------
 5293 !
 5294 !  5. Subroutines calling
 5295 !
 5296 !  6. Error messages :
 5297 !
 5298 !  7. Remarks :
 5299 !
 5300 !  8. Structure :
 5301 !
 5302 !  9. Switches :
 5303 !
 5304 ! 10. Source code :
 5305 !
 5306 !/ ------------------------------------------------------------------- /
 5307 !
 5308 !/    --- Check array size and bounds. ---
 5309       IF ( SIZE(ARRAY).EQ. 0 ) THEN
 5310         WRITE(6,199)
 5311         CALL ABORT
 5312       ELSE IF ( SIZE(ARRAY).NE.SIZE(IDX) ) THEN
 5313         WRITE(6,201)
 5314         CALL ABORT
 5315       ELSE IF ( LBOUND(ARRAY,1).GT.LO ) THEN
 5316         WRITE(6,203)
 5317         CALL ABORT
 5318       ELSE IF ( UBOUND(ARRAY,1).LT.HI ) THEN
 5319         WRITE(6,205)
 5320         CALL ABORT
 5321       END IF
 5322 !
 5323       TOP = LO
 5324       BOT = HI
 5325       VAL = ARRAY(INT((LO+HI)/2))
 5326 !
 5327       LOOP = .TRUE.
 5328       DO WHILE ( LOOP )
 5329         DO WHILE ( ARRAY(TOP).GT.VAL )
 5330           TOP = TOP + 1
 5331         END DO
 5332         DO WHILE ( VAL.GT.ARRAY(BOT) )
 5333           BOT = BOT - 1
 5334         END DO
 5335         IF ( TOP.LT.BOT ) THEN
 
 
                                                         w3strkmd.F90  page  98
 
 
 5336 !/        --- Swap values at indices TOP and BOT ---
 5337           TMP = ARRAY(TOP)
 5338           ARRAY(TOP) = ARRAY(BOT)
 5339           ARRAY(BOT) = TMP
 5340 !/        --- Swap index values at indices TOP and BOT ---
 5341           TMP = IDX(TOP)
 5342           IDX(TOP) = IDX(BOT)
 5343           IDX(BOT) = TMP
 5344 !
 5345           TOP = TOP + 1
 5346           BOT = BOT - 1
 5347         ELSE
 5348           LOOP = .FALSE.
 5349         END IF
 5350               
 5351       END DO
 5352 !/    --- Recursive call quicksort ---
 5353       IF (LO.LT.TOP-1) CALL QSORT_DESC(ARRAY,IDX,LO,TOP-1)
 5354       IF (BOT+1.LT.HI) CALL QSORT_DESC(ARRAY,IDX,BOT+1,HI)
 5355 !
 5356       RETURN
 5357 !/
 5358   199 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/            &
 5359                '     QSORT ARRAY IS EMPTY'                           )
 5360   201 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/            &
 5361                '     QSORT ARRAY SIZE AND INDEX ARRAY SIZE MISMATCH' )
 5362   203 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/            &
 5363                '     QSORT ARRAY INDEX OUT OF LOWER BOUND'           )
 5364   205 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/            &
 5365                '     QSORT ARRAY INDEX OUT OF UPPER BOUND'           )
 5366 !/
 5367       END SUBROUTINE QSORT_DESC
 5368 !/ ------------------------------------------------------------------- /
 5369 !/
 5370       FUNCTION SWAPI4(INT4) RESULT(INT4SWP)
 5371 !/
 5372 !/                  +-----------------------------------+
 5373 !/                  | WAVEWATCH III           NOAA/NCEP |
 5374 !/                  |           S. Zieger               |
 5375 !/                  |                        FORTRAN 90 |
 5376 !/                  | Last update :         03-Jan-2017 |
 5377 !/                  +-----------------------------------+
 5378 !/
 5379 !/    03-Jan-2017 : Origination                         ( version 5.16 )
 5380 !/                                                        (S. Zieger)
 5381 !/
 5382 !  1. Purpose :
 5383 !
 5384 !     Return a Byte-swapped integer (size of 4 bytes)
 5385 !
 5386 !  2. Source code :
 5387 !
 5388 !/ ------------------------------------------------------------------- /
 5389       IMPLICIT NONE
 5390       INTEGER(KIND=4), INTENT(IN)  :: INT4
 
 
                                                         w3strkmd.F90  page  99
 
 
 5391       INTEGER(KIND=4)              :: INT4SWP
 5392 !/
 5393 !     Local variables
 5394 !     ----------------------------------------------------------------
 5395       INTEGER(KIND=1), DIMENSION(4) :: BYTEIN, BYTEOUT
 5396 !/
 5397       BYTEIN  = TRANSFER(INT4, BYTEIN)
 5398       BYTEOUT = (/BYTEIN(4),BYTEIN(3),BYTEIN(2),BYTEIN(1)/)
 5399       INT4SWP = TRANSFER(BYTEOUT, INT4SWP)
 5400 !/
 5401       RETURN
 5402 !/
 5403       END FUNCTION SWAPI4
 5404 !/ ------------------------------------------------------------------- /
 5405       SUBROUTINE findIJV4 (a   ,b   ,maxI,  maxJ ,indA ,indB )
 5406 !/
 5407 !/                  +-----------------------------------+
 5408 !/                  | WAVEWATCH III           NOAA/NCEP |
 5409 !/                  |     A. J. van der Westhuysen      |
 5410 !/                  |            Jeff Hanson            |
 5411 !/                  |        Eve-Marie Devaliere        |
 5412 !/                  |                        FORTRAN 95 |
 5413 !/                  | Last update :         03-Mar-2017 |
 5414 !/                  +-----------------------------------+
 5415 !/
 5416 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
 5417 !/                  by Jeff Hanson & Eve-Marie Devaliere
 5418 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
 5419 !/    03-Mar-2017 : Calls to INTERSECT and UNION        ( version 5.16 )
 5420 !/                  replaced  (S. Zieger, BoM, Australia)
 5421 !/
 5422 !/    Copyright 2009-2013 National Weather Service (NWS),
 5423 !/       National Oceanic and Atmospheric Administration.  All rights
 5424 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
 5425 !/       No unauthorized use without permission.
 5426 !/
 5427       IMPLICIT NONE
 5428 !
 5429 !  1. Purpose :
 5430 !
 5431 !     Find a(i,j) indices of system "a" that lie over or along the
 5432 !     fringes of system "b".
 5433 !
 5434 !  2. Method
 5435 !
 5436 !     (i)  Use an index matrix to map locations of wave systems in B
 5437 !     (ii) Avoid multiple use of INTERSECT and UNION as in findIJV3
 5438 !
 5439 !  3. Parameters :
 5440 !
 5441 !     Parameter list
 5442 !     ----------------------------------------------------------------
 5443 !     a, b    Type(system)  input   Final set of tracked systems, for one>
 5444 !     maxI         Int      input   Number rows indices of wave field
 5445 !     maxJ         Int      input   Number column indices of wave field
 
 
                                                         w3strkmd.F90  page 100
 
 
 5446 !     indA*, indB* Int.A.   output  Pointer array of indices for combinin>
 5447 !
 5448       TYPE(system) :: a, b
 5449       INTEGER      :: maxI, maxJ
 5450       INTEGER, POINTER :: indA(:), indB(:)
 5451 !
 5452       INTENT (IN) a, b, maxI,maxJ
 5453 !
 5454 !     Local variables
 5455 !     ----------------------------------------------------------------
 5456 !     posB         Int              Neighbour index
 5457 !     posB_MM      Int              Neighbour index (-1,-1)
 5458 !     posB_MP      Int              Neighbour index (-1,+1)
 5459 !     posB_PM      Int              Neighbour index (+1,-1)
 5460 !     posB_PP      Int              Neighbour index (+1,+1)
 5461 !     tmpA*, tmpB* Int.A.           Array of indices for combining
 5462 !     systems
 5463 !
 5464       INTEGER      :: LENG_AI,LENG_BI
 5465       INTEGER      :: OUTA,OUTB,I,J,IND,OUTDUMB
 5466       INTEGER      :: POSB,POSB_MM,POSB_PM,POSB_MP,POSB_PP
 5467       INTEGER      :: IND_B2(maxI,maxJ)
 5468       REAL,ALLOCATABLE :: TMPA(:),DUMA(:),TMPB(:)
 5469       REAL,POINTER     :: DUMB(:)
 5470       LOGICAL      :: FOUND
 5471 !
 5472 !  4. Subroutines used :
 5473 !
 5474 !      Name      Type  Scope    Description
 5475 !     ----------------------------------------------------------------
 5476 !      QSORT     Subr. Private  Quicksort algorithm
 5477 !      UNIQUE    Subr. Private  Return sorted unique numbers of an array
 5478 !
 5479 !  5. Subroutines calling
 5480 !
 5481 !  6. Error messages :
 5482 !
 5483 !  7. Remarks :
 5484 !
 5485 !  8. Structure :
 5486 !
 5487 !  9. Switches :
 5488 !
 5489 ! 10. Source code :
 5490 !
 5491 !/ ------------------------------------------------------------------- /
 5492 !
 5493       NULLIFY(DUMB)
 5494 !
 5495       IF (ASSOCIATED(INDA)) DEALLOCATE(INDA)
 5496       IF (ASSOCIATED(INDB)) DEALLOCATE(INDB)
 5497 !
 5498       LENG_AI = LENGTH(REAL(a%i),SIZE(a%i),REAL(9999))
 5499       LENG_BI = LENGTH(REAL(b%i),SIZE(b%i),REAL(9999))
 5500 !
 
 
                                                         w3strkmd.F90  page 101
 
 
 5501       ALLOCATE(TMPA(LENG_AI))
 5502       ALLOCATE(TMPB(5*LENG_AI))
 5503 !
 5504       TMPA(:) = 9999.
 5505       TMPB(:) = 9999.
 5506 !
 5507       OUTA = 0
 5508       OUTB = 0
 5509       IND_B2(:,:) = 0
 5510 !
 5511       DO IND=1,LENG_BI
 5512         I = B%I(IND)
 5513         J = B%J(IND)
 5514         IF (IND_B2(I,J).EQ.0) IND_B2(I,J) = IND
 5515       END DO
 5516 !
 5517       DO IND=1,LENG_AI
 5518         I = A%I(IND)
 5519         J = A%J(IND)
 5520         POSB = IND_B2(I,J)
 5521         POSB_MM = 0
 5522         POSB_PP = 0
 5523         POSB_MP = 0
 5524         POSB_PM = 0
 5525         IF (I.GT.1.AND.J.GT.1)       POSB_MM = IND_B2(i-1,j-1)
 5526         IF (I.GT.1.AND.J.LT.MAXJ)    POSB_MP = IND_B2(i-1,j+1)
 5527         IF (I.LT.MAXI.AND.J.LT.MAXJ) POSB_PP = IND_B2(i+1,j+1)
 5528         IF (I.LT.MAXI.AND.J.GT.1)    POSB_PM = IND_B2(i+1,j-1)
 5529                                                               
 5530         FOUND = .FALSE.
 5531         IF (POSB.NE.0) THEN
 5532           OUTB = OUTB + 1
 5533           TMPB(OUTB) = REAL(POSB)
 5534           IF (.NOT.FOUND) THEN
 5535             OUTA = OUTA + 1
 5536             TMPA(OUTA) = REAL(IND)
 5537             FOUND = .TRUE.
 5538           END IF
 5539         END IF
 5540         IF (POSB_MM.NE.0) THEN
 5541           OUTB = OUTB + 1
 5542           TMPB(OUTB) = REAL(POSB_MM)
 5543           IF (.NOT.FOUND) THEN
 5544             OUTA = OUTA + 1
 5545             TMPA(OUTA) = REAL(IND)
 5546             FOUND = .TRUE.
 5547           END IF
 5548         END IF
 5549         IF (POSB_MP.NE.0) THEN
 5550           OUTB = OUTB + 1
 5551           TMPB(OUTB) = REAL(POSB_MP)
 5552           IF (.NOT.FOUND) THEN
 5553             OUTA = OUTA + 1
 5554             TMPA(OUTA) = REAL(IND)
 5555             FOUND = .TRUE.
 
 
                                                         w3strkmd.F90  page 102
 
 
 5556           END IF
 5557         END IF
 5558         IF (POSB_PM.NE.0) THEN
 5559           OUTB = OUTB + 1
 5560           TMPB(OUTB) = REAL(POSB_PM)
 5561           IF (.NOT.FOUND) THEN
 5562             OUTA = OUTA + 1
 5563             TMPA(OUTA) = REAL(IND)
 5564             FOUND = .TRUE.
 5565           END IF
 5566         END IF
 5567         IF (POSB_PP.NE.0) THEN
 5568           OUTB = OUTB + 1
 5569           TMPB(OUTB) = REAL(POSB_PP)
 5570           IF (.NOT.FOUND) THEN
 5571             OUTA = OUTA + 1
 5572             TMPA(OUTA) = REAL(IND)
 5573             FOUND = .TRUE.
 5574           END IF
 5575         END IF
 5576               
 5577       END DO
 5578 !
 5579 !/    Compact indices for wave systems in B.
 5580 !/    Check for empty arrays first.
 5581       IF (OUTB.GT.0) THEN
 5582         CALL UNIQUE(TMPB,OUTB,DUMB,OUTDUMB)
 5583         OUTB = OUTDUMB
 5584       END IF
 5585       ALLOCATE(INDB(OUTB))
 5586       IF (OUTB.GT.0) INDB(1:OUTB) = INT(DUMB(1:OUTB))
 5587       IF (ASSOCIATED(DUMB)) DEALLOCATE(DUMB)
 5588 !
 5589 !/    Allocate output array and transfer content
 5590 !/    for wave systems in A.
 5591       ALLOCATE(INDA(OUTA))
 5592       IF (OUTA.GT.0) THEN
 5593         ALLOCATE(DUMA(OUTA))
 5594         DUMA(:) = 0
 5595         CALL QSORT(TMPA(1:OUTA),DUMA(1:OUTA),1,OUTA)
 5596         IF (ALLOCATED(DUMA)) DEALLOCATE(DUMA)
 5597         INDA(1:OUTA) = INT(TMPA(1:OUTA))
 5598       END IF
 5599 !/
 5600       IF (ALLOCATED(TMPA)) DEALLOCATE(TMPA)
 5601       IF (ALLOCATED(TMPB)) DEALLOCATE(TMPB)
 5602 !/
 5603       RETURN
 5604 !/
 5605       END SUBROUTINE findIJV4
 5606 !/ End of findIJV4 --------------------------------------------------- /
 5607 !/
 5608 !/ ------------------------------------------------------------------- /
 5609 !/
 5610       END MODULE W3STRKMD
 
 
                                                         w3strkmd.F90  page 103
 
 
 5611 !/
 5612 !/ End of module W3STRKMD -------------------------------------------- /
 5613 !/
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/opt/cray/netcdf-hdf5parallel/4.2.0/pgi/119/include -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
