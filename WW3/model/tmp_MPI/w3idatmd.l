                                                         w3idatmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3IDATMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         24-Apr-2015 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    02-Apr-2004 : Origination.                        ( version 3.06 )
   13 !/    19-Jul-2006 : Adding auxiliary grids.             ( version 3.10 )
   14 !/    04-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
   15 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   16 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
   17 !/    24-Apr-2015 : Adding OASIS coupling calls         ( version 5.07 )
   18 !/                  (M. Accensi & F. Ardhuin, IFREMER)
   19 !/    21-Jun-2018 : Add FSWND input for SMC grid. JGLi  ( version 6.04 )
   20 !/
   21 !/    Copyright 2009 National Weather Service (NWS),
   22 !/       National Oceanic and Atmospheric Administration.  All rights
   23 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   24 !/       No unauthorized use without permission.
   25 !/
   26 !  1. Purpose :
   27 !
   28 !     Define data structures to set up wave model input data for
   29 !     several models simultaneously.
   30 !
   31 !  2. Variables and types :
   32 !
   33 !      Name      Type  Scope    Description
   34 !     ----------------------------------------------------------------
   35 !      NIDATA    Int.  Public   Number of models in array dim.
   36 !      IIDATA    Int.  Public   Selected model for output, init. at -1.
   37 !      INPUT     TYPE  Public   Basic data structure.
   38 !      INPUTS    INPUT Public   Array of data structures.
   39 !     ----------------------------------------------------------------
   40 !
   41 !     All elements of INPUT are aliased to pointers with the same
   42 !     name. Some aditional pointer provide previous equivalenced
   43 !     parameters. These pointers are defined as :
   44 !
   45 !      Name      Type  Scope    Description
   46 !     ----------------------------------------------------------------
   47 !      TLN       I.A.  Public   Time for water level field.
   48 !      TC0/N     I.A.  Public   Times for current fields.
   49 !      TW0/N     I.A.  Public   Times for wind fields.
   50 !      TDN       I.A.  Public   Time for mud density field.
   51 !      TTN       I.A.  Public   Time for mud thickness field.
   52 !      TVN       I.A.  Public   Time for mud viscosity field.
   53 !      TIN       I.A.  Public   Time for ice field. (concentration)
   54 !      TI1N      I.A.  Public   Time for ice field. (parameter 1)
   55 !      TI2N      I.A.  Public   Time for ice field. (parameter 2)
 
 
                                                         w3idatmd.F90  page   2
 
 
   56 !      TI3N      I.A.  Public   Time for ice field. (parameter 3)
   57 !      TI4N      I.A.  Public   Time for ice field. (parameter 4)
   58 !      TI5N      I.A.  Public   Time for ice field. (parameter 5)
   59 !      TnN       I.A.  Public   Time for data types 1-3.
   60 !      TDN       I.A.  Public   Time for next data.
   61 !      TG0/N     I.A.  Public   Times for grid motion data.
   62 !      TFN       I.A.  Public   Array consolidating most above times.
   63 !      GA0/N     Real  Public   Norm of grid speed vector.
   64 !      GD0/N     Real  Public   Direction of grid speed vector.
   65 !      WX0/N     R.A.  Public   Cartesian X and Y wind components
   66 !      WY0/N     R.A.  Public      for both times.
   67 !      DT0/N     R.A.  Public   Corr. air-sea temperature differences.
   68 !      CX0/N     R.A.  Public   Cartesian X and Y current components
   69 !      CY0/N     R.A.  Public      for both times.
   70 !      WLEV      R.A.  Public   Next water level field.
   71 !      ICEI      R.A.  Public   Ice concentrations.
   72 !      BERGI     R.A.  Public   Iceberg damping coefficient
   73 !      IINIT     Log.  Public   Flag for array initialization.
   74 !      FLLEV     Log.  Public   Flag for water level input.
   75 !      FLCUR     Log.  Public   Flag for current input.
   76 !      FLWIND    Log.  Public   Flag for wind input.
   77 !      FLICE     Log.  Public   Flag for ice input.
   78 !      INFLAGS1  L.A.  Public   Array consolidating the above four
   79 !                               flags, as well as four additional
   80 !                               data flags.
   81 !      INFLAGS2  L.A.  Public   Like INFLAGS1 but does *not* get changed
   82 !                               when model reads last record of ice.ww3
   83 !      FLAGSC    L.A.  Public   Coupling or not for input variables
   84 !      JFIRST    Int   Public   First index of arrays related to
   85 !                               input fields.  At present this is
   86 !                               hardwired below. Field-related arrays
   87 !                               (e.g., INFLAGS1) will be allocated from
   88 !                               JFIRST:7 (e.g., ALLOCATE(INFLAGS1(JFIRST:>
   89 !       CXTIDE    R.A.  Public   Tidal constituents of X current component
   90 !       CYTIDE    R.A.  Public   Tidal constituents of Y current component
   91 !       WLTIDE    R.A.  Public   Tidal constituents of water level
   92 !       FLLEVTIDE Log.  Public   Flag for use of tidal const. in water le>
   93 !       FLCURTIDE Log.  Public   Flag for use of tidal const. in current >
   94 !     ----------------------------------------------------------------
   95 !
   96 !  3. Subroutines and functions :
   97 !
   98 !      Name      Type  Scope    Description
   99 !     ----------------------------------------------------------------
  100 !      W3NINP    Subr. Public   Set number of grids/models.
  101 !      W3DIMI    Subr. Public   Set dimensions of arrays.
  102 !      W3SETI    Subr. Public   Point to selected grid / model.
  103 !     ----------------------------------------------------------------
  104 !
  105 !  4. Subroutines and functions used :
  106 !
  107 !      Name      Type  Module   Description
  108 !     ----------------------------------------------------------------
  109 !      W3SETG    Subr. W3GDATMD Point to proper model grid.
  110 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 
 
                                                         w3idatmd.F90  page   3
 
 
  111 !      EXTCDE    Subr. W3SERVMD Abort program with exit code.
  112 !     ----------------------------------------------------------------
  113 !
  114 !  5. Remarks :
  115 !
  116 !     - The number of grids is taken from W3GDATMD, and needs to be
  117 !       set first with W3DIMG.
  118 !
  119 !     - INFLAGS1 dimensioning is hardwired as INFLAGS1(-7:12) where lowes>
  120 !       value of JFIRST is JFIRST=-7
  121 !
  122 !  6. Switches :
  123 !
  124 !     !/S    Enable subroutine tracing.
  125 !     !/T    Enable test output
  126 !     !/TIDE Use of tidal constituents
  127 !
  128 !  7. Source code :
  129 !
  130 !/ ------------------------------------------------------------------- /
  131       PUBLIC
  132 !/
  133 !/ Module private variable for checking error returns
  134 !/
  135       INTEGER, PRIVATE        :: ISTAT
  136 !/
  137 !/ Conventional declarations
  138 !/
  139       INTEGER                 :: NIDATA = -1, IIDATA = -1
  140                                                          
  141       INTEGER                 :: JFIRST = 1
  142                                            
  143 !/
  144 !/ Data structure INPUT
  145 !/
  146       TYPE INPUT
  147         INTEGER               :: TFN(2,-7:8), TC0(2), TW0(2),     &
  148                                  TDN(2), TG0(2)
  149         REAL                  :: GA0, GD0, GAN, GDN
  150         REAL, POINTER         :: WX0(:,:), WY0(:,:), DT0(:,:),        &
  151                                  WXN(:,:), WYN(:,:), DTN(:,:),        &
  152                                  CX0(:,:), CY0(:,:), CXN(:,:),        &
  153                                  CYN(:,:), WLEV(:,:), ICEI(:,:),      &
  154                                  BERGI(:,:), MUDT(:,:), MUDV(:,:),    &
  155                                  MUDD(:,:), ICEP1(:,:), ICEP2(:,:),   &
  156                                  ICEP3(:,:), ICEP4(:,:), ICEP5(:,:)
  157         LOGICAL               :: IINIT
  158 ! note that if size of INFLAGS1 is changed, then TFLAGS in wminitmd.ftn
  159 !    also must be resized.
  160         LOGICAL               :: INFLAGS1(-7:12), FLAGSC(-7:12),      &
  161                                  INFLAGS2(-7:12)
  162       END TYPE INPUT
  163 !/
  164 !/ Data storage
  165 !/
 
 
                                                         w3idatmd.F90  page   4
 
 
  166       TYPE(INPUT), TARGET, ALLOCATABLE :: INPUTS(:)
  167 !/
  168 !/ Data aliasses for structure INPUT(S)
  169 !/
  170       INTEGER, POINTER        :: TFN(:,:), TLN(:), TC0(:), TCN(:),    &
  171                                  TW0(:), TWN(:), TIN(:), T0N(:),      &
  172                                  T1N(:), T2N(:), TDN(:), TG0(:),      &
  173                                  TGN(:), TTN(:), TVN(:), TZN(:),      &
  174                                  TI1(:), TI2(:), TI3(:), TI4(:), TI5(:)
  175       REAL, POINTER           :: GA0, GD0, GAN, GDN
  176       REAL, POINTER           :: WX0(:,:), WY0(:,:), DT0(:,:),        &
  177                                  WXN(:,:), WYN(:,:), DTN(:,:),        &
  178                                  CX0(:,:), CY0(:,:), CXN(:,:),        &
  179                                  CYN(:,:), WLEV(:,:), ICEI(:,:),      &
  180                                  BERGI(:,:), MUDT(:,:), MUDV(:,:),    &
  181                                  MUDD(:,:), ICEP1(:,:), ICEP2(:,:),   &
  182                                  ICEP3(:,:), ICEP4(:,:), ICEP5(:,:)
  183       LOGICAL, POINTER        :: IINIT
  184       LOGICAL, POINTER        :: INFLAGS1(:), INFLAGS2(:), FLAGSC(:)
  185       LOGICAL, POINTER        :: FLLEV, FLCUR, FLWIND, FLICE
  186       LOGICAL, POINTER        :: FLMTH, FLMVS, FLMDN
  187       LOGICAL, POINTER        :: FLIC1, FLIC2, FLIC3, FLIC4, FLIC5
  188 !/
  189       CONTAINS
  190 !/ ------------------------------------------------------------------- /
  191       SUBROUTINE W3NINP ( NDSE, NDST )
  192 !/
  193 !/                  +-----------------------------------+
  194 !/                  | WAVEWATCH III           NOAA/NCEP |
  195 !/                  |           H. L. Tolman            |
  196 !/                  |                        FORTRAN 90 |
  197 !/                  | Last update :         10-Dec-2014 !
  198 !/                  +-----------------------------------+
  199 !/
  200 !/    02-Apr-2004 : Origination.                        ( version 3.06 )
  201 !/    19-Jul-2006 : Adding auxiliary grids.             ( version 3.10 )
  202 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
  203 !/
  204 !  1. Purpose :
  205 !
  206 !     Set up the number of grids to be used.
  207 !
  208 !  2. Method :
  209 !
  210 !     Use data stored in NGRIDS in W3GDATMD.
  211 !
  212 !  3. Parameters :
  213 !
  214 !     Parameter list
  215 !     ----------------------------------------------------------------
  216 !       NDSE    Int.   I   Error output unit number.
  217 !       NDST    Int.   I   Test output unit number.
  218 !     ----------------------------------------------------------------
  219 !
  220 !  4. Subroutines used :
 
 
                                                         w3idatmd.F90  page   5
 
 
  221 !
  222 !     See module documentation.
  223 !
  224 !  5. Called by :
  225 !
  226 !     Any program that uses this grid structure.
  227 !
  228 !  6. Error messages :
  229 !
  230 !     - Error checks on previous setting of variable NGRIDS.
  231 !
  232 !  7. Remarks :
  233 !
  234 !  8. Structure :
  235 !
  236 !  9. Switches :
  237 !
  238 !     !/S    Enable subroutine tracing.
  239 !     !/T    Enable test output
  240 !
  241 ! 10. Source code :
  242 !
  243 !/ ------------------------------------------------------------------- /
  244       USE W3GDATMD, ONLY: NGRIDS, NAUXGR
  245       USE W3SERVMD, ONLY: EXTCDE
  246 !
  247       IMPLICIT NONE
  248 !/
  249 !/ ------------------------------------------------------------------- /
  250 !/ Parameter list
  251 !/
  252       INTEGER, INTENT(IN)     :: NDSE, NDST
  253 !/
  254 !/ ------------------------------------------------------------------- /
  255 !/ Local parameters
  256 !/
  257       INTEGER                 :: I
  258 !/
  259 !
  260 ! -------------------------------------------------------------------- /
  261 ! 1.  Test input and module status
  262 !
  263       IF ( NGRIDS .EQ. -1 ) THEN
  264           WRITE (NDSE,1001) NGRIDS
  265           CALL EXTCDE (1)
  266         END IF
  267 !
  268 ! -------------------------------------------------------------------- /
  269 ! 2.  Set variable and allocate arrays
  270 !
  271       ALLOCATE ( INPUTS(-NAUXGR:NGRIDS), STAT=ISTAT )
  272       CHECK_ALLOC_STATUS ( ISTAT )
  273       NIDATA = NGRIDS
  274 !
  275 ! -------------------------------------------------------------------- /
 
 
                                                         w3idatmd.F90  page   6
 
 
  276 ! 3.  Initialize parameters
  277 !
  278       DO I=-NAUXGR, NGRIDS
  279         INPUTS(I)%TFN(1,:) = -1
  280         INPUTS(I)%TFN(2,:) =  0
  281         INPUTS(I)%TC0(1)   = -1
  282         INPUTS(I)%TC0(2)   =  0
  283         INPUTS(I)%TW0(1)   = -1
  284         INPUTS(I)%TW0(2)   =  0
  285         INPUTS(I)%TDN(1)   = -1
  286         INPUTS(I)%TDN(2)   =  0
  287         INPUTS(I)%TG0(1)   = -1
  288         INPUTS(I)%TG0(2)   =  0
  289         INPUTS(I)%IINIT    = .FALSE.
  290         INPUTS(I)%INFLAGS1 = .FALSE.
  291         INPUTS(I)%INFLAGS2 = .FALSE.
  292         INPUTS(I)%FLAGSC   = .FALSE.
  293         END DO
  294 !
  295       RETURN
  296 !
  297 ! Formats
  298 !
  299  1001 FORMAT (/' *** ERROR W3NINP : NGRIDS NOT YET SET *** '/         &
  300                '                    NGRIDS = ',I10/                   &
  301                '                    RUN W3NMOD FIRST'/)
  302 !
  303 !/
  304 !/ End of W3NINP ----------------------------------------------------- /
  305 !/
  306       END SUBROUTINE W3NINP
  307 !/ ------------------------------------------------------------------- /
  308       SUBROUTINE W3DIMI  ( IMOD, NDSE, NDST, FLAGSTIDEIN )
  309 !/
  310 !/                  +-----------------------------------+
  311 !/                  | WAVEWATCH III           NOAA/NCEP |
  312 !/                  |           H. L. Tolman            |
  313 !/                  |                        FORTRAN 90 |
  314 !/                  | Last update :         10-Dec-2014 !
  315 !/                  +-----------------------------------+
  316 !/
  317 !/    02-Apr-2004 : Origination.                        ( version 3.06 )
  318 !/    19-Jul-2006 : Adding auxiliary grids.             ( version 3.10 )
  319 !/    04-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
  320 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
  321 !/    21-Jun-2018 : Add FSWND input for SMC grid. JGLi  ( version 6.04 )
  322 !/
  323 !  1. Purpose :
  324 !
  325 !     Initialize an individual data grid at the proper dimensions.
  326 !
  327 !  2. Method :
  328 !
  329 !     Allocate directly into the structure array. Note that
  330 !     this cannot be done through the pointer alias!
 
 
                                                         w3idatmd.F90  page   7
 
 
  331 !
  332 !  3. Parameters :
  333 !
  334 !     Parameter list
  335 !     ----------------------------------------------------------------
  336 !       IMOD    Int.   I   Model number to point to.
  337 !       NDSE    Int.   I   Error output unit number.
  338 !       NDST    Int.   I   Test output unit number.
  339 !     ----------------------------------------------------------------
  340 !
  341 !  4. Subroutines used :
  342 !
  343 !     See module documentation.
  344 !
  345 !  5. Called by :
  346 !
  347 !     Main wave model drivers.
  348 !
  349 !  6. Error messages :
  350 !
  351 !     - Check on input parameters.
  352 !     - Check on previous allocation.
  353 !
  354 !  7. Remarks :
  355 !
  356 !     - W3SETI needs to be called after allocation to point to
  357 !       proper allocated arrays.
  358 !
  359 !  8. Structure :
  360 !
  361 !     See source code.
  362 !
  363 !  9. Switches :
  364 !
  365 !     !/S    Enable subroutine tracing.
  366 !     !/T    Enable test output
  367 !
  368 ! 10. Source code :
  369 !
  370 !/ ------------------------------------------------------------------- /
  371       USE W3GDATMD, ONLY: NGRIDS, NAUXGR, IGRID, W3SETG, NX, NY
  372       USE W3SERVMD, ONLY: EXTCDE
  373 !
  374       IMPLICIT NONE
  375 !/
  376 !/ ------------------------------------------------------------------- /
  377 !/ Parameter list
  378 !/
  379       INTEGER, INTENT(IN)     :: IMOD, NDSE, NDST
  380       LOGICAL, INTENT(IN), OPTIONAL     :: FLAGSTIDEIN(4)
  381 !/
  382 !/ ------------------------------------------------------------------- /
  383 !/ Local parameters
  384 !/
  385       INTEGER                 :: JGRID
 
 
                                                         w3idatmd.F90  page   8
 
 
  386       LOGICAL                 :: FLAGSTIDE(4)=.FALSE.
  387 !/
  388 !
  389 ! -------------------------------------------------------------------- /
  390 ! 1.  Test input and module status
  391 !
  392       IF ( NGRIDS .EQ. -1 ) THEN
  393           WRITE (NDSE,1001)
  394           CALL EXTCDE (1)
  395         END IF
  396 !
  397       IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NIDATA ) THEN
  398           WRITE (NDSE,1002) IMOD, -NAUXGR, NIDATA
  399           CALL EXTCDE (2)
  400         END IF
  401 !
  402       IF ( INPUTS(IMOD)%IINIT ) THEN
  403           WRITE (NDSE,1003)
  404           CALL EXTCDE (3)
  405         END IF
  406 !
  407       JGRID  = IGRID
  408       IF ( JGRID .NE. IMOD ) CALL W3SETG ( IMOD, NDSE, NDST )
  409 !
  410 ! -------------------------------------------------------------------- /
  411 ! 2.  Allocate arrays
  412 !
  413  
  414       FLIC1  => INPUTS(IMOD)%INFLAGS1(-7)
  415       FLIC2  => INPUTS(IMOD)%INFLAGS1(-6)
  416       FLIC3  => INPUTS(IMOD)%INFLAGS1(-5)
  417       FLIC4  => INPUTS(IMOD)%INFLAGS1(-4)
  418       FLIC5  => INPUTS(IMOD)%INFLAGS1(-3)
  419 !
  420       FLMDN  => INPUTS(IMOD)%INFLAGS1(-2)
  421       FLMTH  => INPUTS(IMOD)%INFLAGS1(-1)
  422       FLMVS  => INPUTS(IMOD)%INFLAGS1(0)
  423 !
  424       FLLEV  => INPUTS(IMOD)%INFLAGS1(1)
  425       FLCUR  => INPUTS(IMOD)%INFLAGS1(2)
  426                                         
  427       FLWIND => INPUTS(IMOD)%INFLAGS1(3)
  428       FLICE  => INPUTS(IMOD)%INFLAGS1(4)
  429 !
  430 ! notes: future improvement: flags for ICEPx should be
  431 !     "all or nothing" rather than 5 individual flags
  432                                                      
  433       IF ( FLIC1  ) THEN
  434           ALLOCATE ( INPUTS(IMOD)%ICEP1(NX,NY), STAT=ISTAT )
  435           CHECK_ALLOC_STATUS ( ISTAT )
  436         END IF
  437       IF ( FLIC2  ) THEN
  438           ALLOCATE ( INPUTS(IMOD)%ICEP2(NX,NY), STAT=ISTAT )
  439           CHECK_ALLOC_STATUS ( ISTAT )
  440         END IF
 
 
                                                         w3idatmd.F90  page   9
 
 
  441       IF ( FLIC3  ) THEN
  442           ALLOCATE ( INPUTS(IMOD)%ICEP3(NX,NY), STAT=ISTAT )
  443           CHECK_ALLOC_STATUS ( ISTAT )
  444         END IF
  445       IF ( FLIC4  ) THEN
  446           ALLOCATE ( INPUTS(IMOD)%ICEP4(NX,NY), STAT=ISTAT )
  447           CHECK_ALLOC_STATUS ( ISTAT )
  448         END IF
  449       IF ( FLIC5  ) THEN
  450           ALLOCATE ( INPUTS(IMOD)%ICEP5(NX,NY), STAT=ISTAT )
  451           CHECK_ALLOC_STATUS ( ISTAT )
  452         END IF
  453 !
  454       IF ( FLMDN  ) THEN
  455           ALLOCATE ( INPUTS(IMOD)%MUDD(NX,NY), STAT=ISTAT )
  456           CHECK_ALLOC_STATUS ( ISTAT )
  457         END IF
  458       IF ( FLMTH  ) THEN
  459           ALLOCATE ( INPUTS(IMOD)%MUDT(NX,NY), STAT=ISTAT )
  460           CHECK_ALLOC_STATUS ( ISTAT )
  461         END IF
  462       IF ( FLMVS  ) THEN
  463           ALLOCATE ( INPUTS(IMOD)%MUDV(NX,NY), STAT=ISTAT )
  464           CHECK_ALLOC_STATUS ( ISTAT )
  465         END IF
  466 !
  467       IF ( FLLEV  ) THEN
  468           ALLOCATE ( INPUTS(IMOD)%WLEV(NX,NY), STAT=ISTAT )
  469           CHECK_ALLOC_STATUS ( ISTAT )
  470         END IF
  471 !
  472       IF ( FLCUR  ) THEN
  473           ALLOCATE ( INPUTS(IMOD)%CX0(NX,NY) ,              &
  474                      INPUTS(IMOD)%CY0(NX,NY) ,              &
  475                      INPUTS(IMOD)%CXN(NX,NY) ,              &
  476                      INPUTS(IMOD)%CYN(NX,NY) , STAT=ISTAT )
  477           CHECK_ALLOC_STATUS ( ISTAT )
  478         END IF
  479 !
  480  
  481  
  482       IF ( FLWIND ) THEN
  483           ALLOCATE ( INPUTS(IMOD)%WX0(NX,NY) ,              &
  484                      INPUTS(IMOD)%WY0(NX,NY) ,              &
  485                      INPUTS(IMOD)%DT0(NX,NY) ,              &
  486                      INPUTS(IMOD)%WXN(NX,NY) ,              &
  487                      INPUTS(IMOD)%WYN(NX,NY) ,              &
  488                      INPUTS(IMOD)%DTN(NX,NY) , STAT=ISTAT )
  489           CHECK_ALLOC_STATUS ( ISTAT )
  490           INPUTS(IMOD)%DT0 = 0.
  491           INPUTS(IMOD)%DTN = 0.
  492         END IF
  493 !
  494       IF ( FLICE  ) THEN
  495           ALLOCATE ( INPUTS(IMOD)%ICEI(NX,NY),              &
 
 
                                                         w3idatmd.F90  page  10
 
 
  496                      INPUTS(IMOD)%BERGI(NX,NY), STAT=ISTAT )
  497           CHECK_ALLOC_STATUS ( ISTAT )
  498           INPUTS(IMOD)%BERGI = 0.
  499         END IF
  500 !
  501       INPUTS(IMOD)%IINIT  = .TRUE.
  502 !
  503 ! -------------------------------------------------------------------- /
  504 ! 3.  Point to allocated arrays
  505 !
  506       CALL W3SETI ( IMOD, NDSE, NDST )
  507 !
  508 ! -------------------------------------------------------------------- /
  509 ! 4.  Update counters in grid
  510 !
  511 ! -------------------------------------------------------------------- /
  512 ! 5.  Restore previous grid setting if necessary
  513 !
  514       IF ( JGRID .NE. IMOD ) CALL W3SETG ( JGRID, NDSE, NDST )
  515 !
  516       RETURN
  517 !
  518 ! Formats
  519 !
  520  1001 FORMAT (/' *** ERROR W3DIMI : GRIDS NOT INITIALIZED *** '/      &
  521                '                    RUN W3NMOD FIRST '/)
  522  1002 FORMAT (/' *** ERROR W3DIMI : ILLEGAL MODEL NUMBER *** '/       &
  523                '                    IMOD   = ',I10/                   &
  524                '                    NAUXGR = ',I10/                   &
  525                '                    NIDATA = ',I10/)
  526  1003 FORMAT (/' *** ERROR W3DIMI : ARRAY(S) ALREADY ALLOCATED *** ')
  527 !
  528 !/
  529 !/ End of W3DIMI ----------------------------------------------------- /
  530 !/
  531       END SUBROUTINE W3DIMI
  532 !/ ------------------------------------------------------------------- /
  533       SUBROUTINE W3SETI ( IMOD, NDSE, NDST )
  534 !/
  535 !/                  +-----------------------------------+
  536 !/                  | WAVEWATCH III           NOAA/NCEP |
  537 !/                  |           H. L. Tolman            |
  538 !/                  |                        FORTRAN 90 |
  539 !/                  | Last update :         04-Oct-2006 !
  540 !/                  +-----------------------------------+
  541 !/
  542 !/    02-Apr-2004 : Origination.                        ( version 3.06 )
  543 !/    19-Jul-2006 : Adding auxiliary grids.             ( version 3.10 )
  544 !/    04-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
  545 !/
  546 !  1. Purpose :
  547 !
  548 !     Select one of the WAVEWATCH III grids / models.
  549 !
  550 !  2. Method :
 
 
                                                         w3idatmd.F90  page  11
 
 
  551 !
  552 !     Point pointers to the proper variables in the proper element of
  553 !     the GRIDS array.
  554 !
  555 !  3. Parameters :
  556 !
  557 !     Parameter list
  558 !     ----------------------------------------------------------------
  559 !       IMOD    Int.   I   Model number to point to.
  560 !       NDSE    Int.   I   Error output unit number.
  561 !       NDST    Int.   I   Test output unit number.
  562 !     ----------------------------------------------------------------
  563 !
  564 !  4. Subroutines used :
  565 !
  566 !     See module documentation.
  567 !
  568 !  5. Called by :
  569 !
  570 !     Any subroutine.
  571 !
  572 !  6. Error messages :
  573 !
  574 !     Many subroutines in the WAVEWATCH system.
  575 !
  576 !  7. Remarks :
  577 !
  578 !  8. Structure :
  579 !
  580 !  9. Switches :
  581 !
  582 !     !/S    Enable subroutine tracing.
  583 !     !/T    Enable test output
  584 !
  585 ! 10. Source code :
  586 !
  587 !/ ------------------------------------------------------------------- /
  588       USE W3GDATMD, ONLY: NAUXGR
  589 !
  590       USE W3SERVMD, ONLY: EXTCDE
  591 !
  592       IMPLICIT NONE
  593 !/
  594 !/ ------------------------------------------------------------------- /
  595 !/ Parameter list
  596 !/
  597       INTEGER, INTENT(IN)     :: IMOD, NDSE, NDST
  598 !/
  599 !/ ------------------------------------------------------------------- /
  600 !/ Local parameters
  601 !/
  602 !/
  603 !
  604 ! -------------------------------------------------------------------- /
  605 ! 1.  Test input and module status
 
 
                                                         w3idatmd.F90  page  12
 
 
  606 !
  607       IF ( NIDATA .EQ. -1 ) THEN
  608           WRITE (NDSE,1001)
  609           CALL EXTCDE (1)
  610         END IF
  611 !
  612       IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NIDATA ) THEN
  613           WRITE (NDSE,1002) IMOD, -NAUXGR, NIDATA
  614           CALL EXTCDE (2)
  615         END IF
  616 !
  617 ! -------------------------------------------------------------------- /
  618 ! 2.  Set model numbers
  619 !
  620       IIDATA = IMOD
  621 !
  622 ! -------------------------------------------------------------------- /
  623 ! 3.  Set pointers
  624 !
  625       TFN    => INPUTS(IMOD)%TFN
  626       TC0    => INPUTS(IMOD)%TC0
  627       TW0    => INPUTS(IMOD)%TW0
  628       TG0    => INPUTS(IMOD)%TG0
  629       TDN    => INPUTS(IMOD)%TDN
  630 !
  631       TI1    => INPUTS(IMOD)%TFN(:,-7)
  632       TI2    => INPUTS(IMOD)%TFN(:,-6)
  633       TI3    => INPUTS(IMOD)%TFN(:,-5)
  634       TI4    => INPUTS(IMOD)%TFN(:,-4)
  635       TI5    => INPUTS(IMOD)%TFN(:,-3)
  636 !
  637       TZN    => INPUTS(IMOD)%TFN(:,-2)
  638       TTN    => INPUTS(IMOD)%TFN(:,-1)
  639       TVN    => INPUTS(IMOD)%TFN(:,0)
  640 !
  641       TLN    => INPUTS(IMOD)%TFN(:,1)
  642       TCN    => INPUTS(IMOD)%TFN(:,2)
  643       TWN    => INPUTS(IMOD)%TFN(:,3)
  644       TIN    => INPUTS(IMOD)%TFN(:,4)
  645       T0N    => INPUTS(IMOD)%TFN(:,5)
  646       T1N    => INPUTS(IMOD)%TFN(:,6)
  647       T2N    => INPUTS(IMOD)%TFN(:,7)
  648       TGN    => INPUTS(IMOD)%TFN(:,8)
  649 !
  650       GA0    => INPUTS(IMOD)%GA0
  651       GD0    => INPUTS(IMOD)%GD0
  652       GAN    => INPUTS(IMOD)%GAN
  653       GDN    => INPUTS(IMOD)%GDN
  654 !
  655       IINIT  => INPUTS(IMOD)%IINIT
  656       INFLAGS1  => INPUTS(IMOD)%INFLAGS1
  657       INFLAGS2  => INPUTS(IMOD)%INFLAGS2
  658       FLAGSC => INPUTS(IMOD)%FLAGSC
  659 !
  660       FLIC1  => INPUTS(IMOD)%INFLAGS1(-7)
 
 
                                                         w3idatmd.F90  page  13
 
 
  661       FLIC2  => INPUTS(IMOD)%INFLAGS1(-6)
  662       FLIC3  => INPUTS(IMOD)%INFLAGS1(-5)
  663       FLIC4  => INPUTS(IMOD)%INFLAGS1(-4)
  664       FLIC5  => INPUTS(IMOD)%INFLAGS1(-3)
  665 !
  666       FLMDN  => INPUTS(IMOD)%INFLAGS1(-2)
  667       FLMTH  => INPUTS(IMOD)%INFLAGS1(-1)
  668       FLMVS  => INPUTS(IMOD)%INFLAGS1(0)
  669 !
  670       FLLEV  => INPUTS(IMOD)%INFLAGS1(1)
  671       FLCUR  => INPUTS(IMOD)%INFLAGS1(2)
  672                                         
  673       FLWIND => INPUTS(IMOD)%INFLAGS1(3)
  674       FLICE  => INPUTS(IMOD)%INFLAGS1(4)
  675 !
  676       IF ( IINIT ) THEN
  677 !
  678           IF ( FLIC1  ) THEN
  679               ICEP1  => INPUTS(IMOD)%ICEP1
  680           END IF
  681           IF ( FLIC2  ) THEN
  682               ICEP2  => INPUTS(IMOD)%ICEP2
  683           END IF
  684           IF ( FLIC3  ) THEN
  685               ICEP3  => INPUTS(IMOD)%ICEP3
  686           END IF
  687           IF ( FLIC4  ) THEN
  688               ICEP4  => INPUTS(IMOD)%ICEP4
  689           END IF
  690           IF ( FLIC5  ) THEN
  691               ICEP5  => INPUTS(IMOD)%ICEP5
  692           END IF
  693 !
  694           IF ( FLMDN  ) THEN
  695               MUDD   => INPUTS(IMOD)%MUDD
  696           END IF
  697           IF ( FLMTH  ) THEN
  698               MUDT   => INPUTS(IMOD)%MUDT
  699           END IF
  700           IF ( FLMVS  ) THEN
  701               MUDV   => INPUTS(IMOD)%MUDV
  702           END IF
  703 !
  704           IF ( FLLEV  ) THEN
  705               WLEV   => INPUTS(IMOD)%WLEV
  706             END IF
  707 !
  708           IF ( FLCUR  ) THEN
  709               CX0    => INPUTS(IMOD)%CX0
  710               CY0    => INPUTS(IMOD)%CY0
  711               CXN    => INPUTS(IMOD)%CXN
  712               CYN    => INPUTS(IMOD)%CYN
  713             END IF
  714 !
  715  
 
 
                                                         w3idatmd.F90  page  14
 
 
  716           IF ( FLWIND  ) THEN
  717               WX0    => INPUTS(IMOD)%WX0
  718               WY0    => INPUTS(IMOD)%WY0
  719               DT0    => INPUTS(IMOD)%DT0
  720               WXN    => INPUTS(IMOD)%WXN
  721               WYN    => INPUTS(IMOD)%WYN
  722               DTN    => INPUTS(IMOD)%DTN
  723             END IF
  724 !
  725           IF ( FLICE  ) THEN
  726               ICEI   => INPUTS(IMOD)%ICEI
  727               BERGI  => INPUTS(IMOD)%BERGI
  728             END IF
  729 !
  730         END IF
  731 !
  732       RETURN
  733 !
  734 ! Formats
  735 !
  736  1001 FORMAT (/' *** ERROR W3SETI : GRIDS NOT INITIALIZED *** '/      &
  737                '                    RUN W3NMOD FIRST '/)
  738  1002 FORMAT (/' *** ERROR W3SETI : ILLEGAL MODEL NUMBER *** '/       &
  739                '                    IMOD   = ',I10/                   &
  740                '                    NAUXGR = ',I10/                   &
  741                '                    NIDATA = ',I10/)
  742 !
  743 !/
  744 !/ End of W3SETI ----------------------------------------------------- /
  745 !/
  746       END SUBROUTINE W3SETI
  747 !/
  748 !/ End of module W3IDATMD -------------------------------------------- /
  749 !/
  750       END MODULE W3IDATMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
