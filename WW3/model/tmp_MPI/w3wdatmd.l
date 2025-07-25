                                                         w3wdatmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3WDATMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         06-Jun-2018 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    22-Oct-2004 : Origination.                        ( version 3.06 )
   13 !/    13-Jun-2006 : Allocate VA consistent with MPI     ( version 3.09 )
   14 !/                  data types and initialize as needed.
   15 !/    05-Jul-2006 : Consolidate stress vector.          ( version 3.09 )
   16 !/    04-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
   17 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   18 !/    16-May-2010 : Add iceberg damping                 ( version 3.14.4 )
   19 !/    14-Nov-2013 : Initialize UST and USTDIR.          ( version 4.13 )
   20 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
   21 !/    06-Jun-2018 : Add PDLIB/SETUP/DEBUGINIT           ( version 6.04 )
   22 !/
   23 !/    Copyright 2009-2013 National Weather Service (NWS),
   24 !/       National Oceanic and Atmospheric Administration.  All rights
   25 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   26 !/       No unauthorized use without permission.
   27 !/
   28 !  1. Purpose :
   29 !
   30 !     Define data structures to set up wave model dynamic data for
   31 !     several models simultaneously.
   32 !
   33 !  2. Variables and types :
   34 !
   35 !      Name      Type  Scope    Description
   36 !     ----------------------------------------------------------------
   37 !      NWDATA    Int.  Public   Number of models in array dim.
   38 !      IWDATA    Int.  Public   Selected model for output, init. at -1.
   39 !      WDATA     TYPE  Public   Basic data structure.
   40 !      WDATAS    WDATA Public   Array of data structures.
   41 !     ----------------------------------------------------------------
   42 !
   43 !     All elements of WDATA are aliased to pointers with the same
   44 !     name. These pointers are defined as :
   45 !
   46 !      Name      Type  Scope    Description
   47 !     ----------------------------------------------------------------
   48 !      TIME      I.A.  Public   Valid time for spectra.
   49 !      TIME00    I.A.  Public   Initial time
   50 !      TIMEEND   I.A.  Public   Final time
   51 !      TLEV      I.A.  Public   Valid time for water levels.
   52 !      TICE      I.A.  Public   Valid time for ice concentration
   53 !      TIC1      I.A.  Public   Valid time for ice thickness
   54 !      TIC5      I.A.  Public   Valid time for ice floe
   55 !      VA        R.A.  Public   Storage array for spectra.
 
 
                                                         w3wdatmd.F90  page   2
 
 
   56 !      WLV       R.A.  Public   Water levels.
   57 !      ICE       R.A.  Public   Ice coverage.
   58 !      ICEH      R.A.  Public   Ice thickness.
   59 !      ICEF      R.A.  Public   Ice flow maximum diameter.
   60 !      ICEDMAX   R.A.  Public   Ice flow maximum diameter for updates.
   61 !      BERG      R.A.  Public   Iceberg damping.
   62 !      UST       R.A.  Public   Friction velocity (absolute).
   63 !      USTDIR    R.A.  Public   Friction velocity direction.
   64 !      ASF       R.A.  Public   Stability correction factor.
   65 !      FPIS      R.A.  Public   Input peak frequencies.
   66 !      DINIT     Log.  Public   Flag for array initialization.
   67 !      FL_ALL    Log.  Public   Flag for initializing all arrays,
   68 !                               otherwise VA is skipped.
   69 !     ----------------------------------------------------------------
   70 !
   71 !  3. Subroutines and functions :
   72 !
   73 !      Name      Type  Scope    Description
   74 !     ----------------------------------------------------------------
   75 !      W3NDAT    Subr. Public   Set number of grids/models.
   76 !      W3DIMW    Subr. Public   Set dimensions of arrays.
   77 !      W3SETW    Subr. Public   Point to selected grid / model.
   78 !     ----------------------------------------------------------------
   79 !
   80 !  4. Subroutines and functions used :
   81 !
   82 !      Name      Type  Module   Description
   83 !     ----------------------------------------------------------------
   84 !      W3SETG    Subr. W3GDATMD Point to proper model grid.
   85 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   86 !      EXTCDE    Subr. W3SERVMD Abort program with exit code.
   87 !     ----------------------------------------------------------------
   88 !
   89 !  5. Remarks :
   90 !
   91 !     - The number of grids is taken from W3GDATMD, and needs to be
   92 !       set first with W3DIMG.
   93 !
   94 !  6. Switches :
   95 !
   96 !     !/S    Enable subroutine tracing.
   97 !     !/T    Enable test output
   98 !
   99 !  7. Source code :
  100 !
  101 !/ ------------------------------------------------------------------- /
  102       PUBLIC
  103 !/
  104 !/ Module private variable for checking error returns
  105 !/
  106       INTEGER, PRIVATE        :: ISTAT
  107 !/
  108 !/ Conventional declarations
  109 !/
  110       INTEGER                 :: NWDATA = -1, IWDATA = -1
 
 
                                                         w3wdatmd.F90  page   3
 
 
  111 !/
  112 !/ Data structures
  113 !/
  114       TYPE WDATA
  115         INTEGER               :: TIME(2), TLEV(2), TICE(2), TIC1(2), TIC5>
  116         INTEGER               :: TIMEEND(2)
  117         REAL, POINTER         :: VA(:,:), WLV(:), ICE(:), UST(:),     &
  118                                  USTDIR(:), ASF(:), FPIS(:), BERG(:), &
  119                                  ICEH(:), ICEF(:), ICEDMAX(:)
  120 !!/PDLIB     REAL, POINTER     :: VAOLD(:,:)
  121         LOGICAL               :: DINIT, FL_ALL
  122       END TYPE WDATA
  123 !
  124 !/
  125 !/ Data storage
  126 !/
  127       TYPE(WDATA), TARGET, ALLOCATABLE :: WDATAS(:)
  128 !/
  129 !/ Data aliasses for structure WDATA(S)
  130 !/
  131       INTEGER, POINTER        :: TIME(:), TLEV(:), TICE(:), TIC1(:), TIC5>
  132       INTEGER, POINTER        :: TIMEEND(:)
  133       REAL, POINTER           :: VA(:,:), WLV(:), ICE(:), UST(:),     &
  134                                  USTDIR(:), ASF(:), FPIS(:), BERG(:), &
  135                                  ICEH(:), ICEF(:), ICEDMAX(:)
  136 !!/PDLIB      REAL, POINTER           :: VAOLD(:,:)
  137       LOGICAL, POINTER        :: DINIT, FL_ALL
  138 !/
  139       CONTAINS
  140 !/ ------------------------------------------------------------------- /
  141       SUBROUTINE W3NDAT ( NDSE, NDST )
  142 !/
  143 !/                  +-----------------------------------+
  144 !/                  | WAVEWATCH III           NOAA/NCEP |
  145 !/                  |           H. L. Tolman            |
  146 !/                  |                        FORTRAN 90 |
  147 !/                  | Last update :         10-Dec-2014 !
  148 !/                  +-----------------------------------+
  149 !/
  150 !/    31-Mar-2004 : Origination.                        ( version 3.06 )
  151 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
  152 !/
  153 !  1. Purpose :
  154 !
  155 !     Set up the number of grids to be used.
  156 !
  157 !  2. Method :
  158 !
  159 !     Use data stored in NGRIDS in W3GDATMD.
  160 !
  161 !  3. Parameters :
  162 !
  163 !     Parameter list
  164 !     ----------------------------------------------------------------
  165 !       NDSE    Int.   I   Error output unit number.
 
 
                                                         w3wdatmd.F90  page   4
 
 
  166 !       NDST    Int.   I   Test output unit number.
  167 !     ----------------------------------------------------------------
  168 !
  169 !  4. Subroutines used :
  170 !
  171 !     See module documentation.
  172 !
  173 !  5. Called by :
  174 !
  175 !     Any program that uses this grid structure.
  176 !
  177 !  6. Error messages :
  178 !
  179 !     - Error checks on previous setting of variable NGRIDS.
  180 !
  181 !  7. Remarks :
  182 !
  183 !  8. Structure :
  184 !
  185 !  9. Switches :
  186 !
  187 !     !/S    Enable subroutine tracing.
  188 !     !/T    Enable test output
  189 !
  190 ! 10. Source code :
  191 !
  192 !/ ------------------------------------------------------------------- /
  193       USE W3GDATMD, ONLY: NGRIDS
  194       USE W3SERVMD, ONLY: EXTCDE
  195 !
  196       IMPLICIT NONE
  197 !/
  198 !/ ------------------------------------------------------------------- /
  199 !/ Parameter list
  200 !/
  201       INTEGER, INTENT(IN)     :: NDSE, NDST
  202 !/
  203 !/ ------------------------------------------------------------------- /
  204 !/ Local parameters
  205 !/
  206       INTEGER                 :: I
  207 !/
  208 !
  209 ! -------------------------------------------------------------------- /
  210 ! 1.  Test input and module status
  211 !
  212       IF ( NGRIDS .EQ. -1 ) THEN
  213           WRITE (NDSE,1001) NGRIDS
  214           CALL EXTCDE (1)
  215         END IF
  216 !
  217 ! -------------------------------------------------------------------- /
  218 ! 2.  Set variable and allocate arrays
  219 !
  220       ALLOCATE ( WDATAS(0:NGRIDS), STAT=ISTAT )
 
 
                                                         w3wdatmd.F90  page   5
 
 
  221       CHECK_ALLOC_STATUS ( ISTAT )
  222       NWDATA = NGRIDS
  223 !
  224 ! -------------------------------------------------------------------- /
  225 ! 3.  Initialize parameters
  226 !
  227       DO I=0, NGRIDS
  228         WDATAS(I)%DINIT  = .FALSE.
  229         WDATAS(I)%FL_ALL = .FALSE.
  230         END DO
  231 !
  232       RETURN
  233 !
  234 ! Formats
  235 !
  236  1001 FORMAT (/' *** ERROR W3NDAT : NGRIDS NOT YET SET *** '/         &
  237                '                    NGRIDS = ',I10/                   &
  238                '                    RUN W3NMOD FIRST'/)
  239 !
  240 !/
  241 !/ End of W3NDAT ----------------------------------------------------- /
  242 !/
  243       END SUBROUTINE W3NDAT
  244 !/ ------------------------------------------------------------------- /
  245       SUBROUTINE W3DIMW  ( IMOD, NDSE, NDST, F_ONLY )
  246 !/
  247 !/                  +-----------------------------------+
  248 !/                  | WAVEWATCH III           NOAA/NCEP |
  249 !/                  |           H. L. Tolman            |
  250 !/                  |                        FORTRAN 90 |
  251 !/                  | Last update :         10-Dec-2014 !
  252 !/                  +-----------------------------------+
  253 !/
  254 !/    22-Oct-2004 : Origination.                        ( version 3.06 )
  255 !/    13-Jun-2006 : Allocate VA consistent with MPI     ( version 3.09 )
  256 !/                  data types and initialize as needed.
  257 !/    05-Jul-2006 : Consolidate stress vector.          ( version 3.09 )
  258 !/    04-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
  259 !/    14-Nov-2013 : Initialize UST and USTDIR.          ( version 4.13 )
  260 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
  261 !/
  262 !  1. Purpose :
  263 !
  264 !     Initialize an individual data grid at the proper dimensions.
  265 !
  266 !  2. Method :
  267 !
  268 !     Allocate directly into the structure array. Note that
  269 !     this cannot be done through the pointer alias!
  270 !
  271 !  3. Parameters :
  272 !
  273 !     Parameter list
  274 !     ----------------------------------------------------------------
  275 !       IMOD    Int.   I   Model number to point to.
 
 
                                                         w3wdatmd.F90  page   6
 
 
  276 !       NDSE    Int.   I   Error output unit number.
  277 !       NDST    Int.   I   Test output unit number.
  278 !       F_ONLY  L.O.   I   FLag for initializing field arrays only.
  279 !     ----------------------------------------------------------------
  280 !
  281 !  4. Subroutines used :
  282 !
  283 !     See module documentation.
  284 !
  285 !  5. Called by :
  286 !
  287 !      Name      Type  Module   Description
  288 !     ----------------------------------------------------------------
  289 !      W3IOGO    Subr. W3IOGOMD Grid output IO routine.
  290 !      W3IORS    Subr. W3IORSMD Restart file IO routine.
  291 !      WW3_SHEL  Prog.   N/A    Main wave model driver.
  292 !      WW3_STRT  Prog.   N/A    Initial conditions program.
  293 !     ----------------------------------------------------------------
  294 !
  295 !  6. Error messages :
  296 !
  297 !     - Check on input parameters.
  298 !     - Check on previous allocation.
  299 !
  300 !  7. Remarks :
  301 !
  302 !     - W3SETW needs to be called after allocation to point to
  303 !       proper allocated arrays.
  304 !
  305 !  8. Structure :
  306 !
  307 !     See source code.
  308 !
  309 !  9. Switches :
  310 !
  311 !     !/S    Enable subroutine tracing.
  312 !     !/T    Enable test output
  313 !
  314 ! 10. Source code :
  315 !
  316 !/ ------------------------------------------------------------------- /
  317       USE W3GDATMD, ONLY: NGRIDS, IGRID, W3SETG, NSPEC, NSEA, NSEAL, GRIDS
  318       USE W3ODATMD, ONLY: NAPROC, IAPROC
  319       USE W3SERVMD, ONLY: EXTCDE
  320       USE CONSTANTS, ONLY : LPDLIB
  321       USE W3PARALL, ONLY: SET_UP_NSEAL_NSEALM
  322 !
  323       IMPLICIT NONE
  324 !
  325 !/
  326 !/ ------------------------------------------------------------------- /
  327 !/ Parameter list
  328 !/
  329       INTEGER, INTENT(IN)           :: IMOD, NDSE, NDST
  330       LOGICAL, INTENT(IN), OPTIONAL :: F_ONLY
 
 
                                                         w3wdatmd.F90  page   7
 
 
  331 !/
  332 !/ ------------------------------------------------------------------- /
  333 !/ Local parameters
  334 !/
  335       INTEGER                 :: JGRID, NSEALM, NSEATM
  336       INTEGER                 :: NSEAL_DUMMY, ISEA
  337 !/
  338   
  339 !
  340 ! -------------------------------------------------------------------- /
  341 ! 1.  Test input and module status
  342 !
  343       IF ( PRESENT(F_ONLY) ) THEN
  344           FL_ALL = .NOT. F_ONLY
  345         ELSE
  346           FL_ALL = .TRUE.
  347         END IF
  348 !
  349       IF ( NGRIDS .EQ. -1 ) THEN
  350           WRITE (NDSE,1001)
  351           CALL EXTCDE (1)
  352         END IF
  353 !
  354       IF ( IMOD.LT.1 .OR. IMOD.GT.NWDATA ) THEN
  355           WRITE (NDSE,1002) IMOD, NWDATA
  356           CALL EXTCDE (2)
  357         END IF
  358 !
  359       IF ( WDATAS(IMOD)%DINIT ) THEN
  360           WRITE (NDSE,1003)
  361           CALL EXTCDE (3)
  362         END IF
  363 !
  364       JGRID  = IGRID
  365       IF ( JGRID .NE. IMOD ) CALL W3SETG ( IMOD, NDSE, NDST )
  366 !
  367 ! -------------------------------------------------------------------- /
  368 ! 2.  Allocate arrays
  369 !
  370       CALL SET_UP_NSEAL_NSEALM(NSEAL_DUMMY, NSEALM)
  371       NSEATM = NSEALM * NAPROC
  372 !
  373       IF ( FL_ALL ) THEN
  374           ALLOCATE ( WDATAS(IMOD)%VA(NSPEC,0:NSEALM), STAT=ISTAT ); WDATA>
  375           CHECK_ALLOC_STATUS ( ISTAT )
  376 !!/PDLIB          ALLOCATE ( WDATAS(IMOD)%VAOLD(NSPEC,0:NSEALM) )
  377           IF ( NSEAL .NE. NSEALM ) THEN
  378             DO ISEA=NSEAL+1,NSEALM
  379               WDATAS(IMOD)%VA(:,ISEA) = 0.
  380             END DO
  381           END IF
  382         END IF
  383 !
  384       ! ICE, ICEH, ICEF must be defined from 0:NSEA
  385       ALLOCATE ( WDATAS(IMOD)%WLV(NSEA),                              &
 
 
                                                         w3wdatmd.F90  page   8
 
 
  386                  WDATAS(IMOD)%ICE(0:NSEA),                            &
  387                  WDATAS(IMOD)%BERG(NSEA),                             &
  388                  WDATAS(IMOD)%ICEH(0:NSEA),                           &
  389                  WDATAS(IMOD)%ICEF(0:NSEA),                           &
  390                  WDATAS(IMOD)%ICEDMAX(NSEA),                          &
  391                  WDATAS(IMOD)%UST(0:NSEATM),                          &
  392                  WDATAS(IMOD)%USTDIR(0:NSEATM),                       &
  393                  WDATAS(IMOD)%ASF(NSEATM),                            &
  394                  WDATAS(IMOD)%FPIS(NSEATM), STAT=ISTAT                )
  395       CHECK_ALLOC_STATUS ( ISTAT )
  396                                   
  397       WDATAS(IMOD)%WLV   (:) = 0.
  398       WDATAS(IMOD)%ICE   (0:NSEA) = 0.
  399       WDATAS(IMOD)%BERG  (:) = 0.
  400       WDATAS(IMOD)%ICEH  (0:NSEA) = GRIDS(IMOD)%IICEHINIT
  401       WDATAS(IMOD)%ICEF  (0:NSEA) = 1000.
  402       WDATAS(IMOD)%ICEDMAX(:) = 1000.
  403       WDATAS(IMOD)%UST   (0:NSEATM) = 1.E-5
  404       WDATAS(IMOD)%USTDIR(0:NSEATM) = 0.
  405       WDATAS(IMOD)%ASF   (:) = 0.
  406       WDATAS(IMOD)%FPIS  (:) = 0.
  407       WDATAS(IMOD)%DINIT     = .TRUE.
  408       CALL W3SETW ( IMOD, NDSE, NDST )
  409 !
  410 ! -------------------------------------------------------------------- /
  411 ! 5.  Restore previous grid setting if necessary
  412 !
  413       IF ( JGRID .NE. IMOD ) CALL W3SETG ( JGRID, NDSE, NDST )
  414 !
  415       RETURN
  416 !
  417 ! Formats
  418 !
  419  1001 FORMAT (/' *** ERROR W3DIMW : GRIDS NOT INITIALIZED *** '/      &
  420                '                    RUN W3NMOD FIRST '/)
  421  1002 FORMAT (/' *** ERROR W3DIMW : ILLEGAL MODEL NUMBER *** '/       &
  422                '                    IMOD   = ',I10/                   &
  423                '                    NWDATA = ',I10/)
  424  1003 FORMAT (/' *** ERROR W3DIMW : ARRAY(S) ALREADY ALLOCATED *** ')
  425 !
  426 ! -------------------------------------------------------------------- /
  427 ! 3.  Point to allocated arrays
  428 !
  429       CALL W3SETW ( IMOD, NDSE, NDST )
  430 !
  431 ! -------------------------------------------------------------------- /
  432 ! 4.  Update counters in grid
  433 !/
  434 !/ End of W3DIMW ----------------------------------------------------- /
  435 !/
  436       END SUBROUTINE W3DIMW
  437 !/ ------------------------------------------------------------------- /
  438       SUBROUTINE W3SETW ( IMOD, NDSE, NDST )
  439 !/
  440 !/                  +-----------------------------------+
 
 
                                                         w3wdatmd.F90  page   9
 
 
  441 !/                  | WAVEWATCH III           NOAA/NCEP |
  442 !/                  |           H. L. Tolman            |
  443 !/                  |                        FORTRAN 90 |
  444 !/                  | Last update :         04-Oct-2006 !
  445 !/                  +-----------------------------------+
  446 !/
  447 !/    31-Mar-2004 : Origination.                        ( version 3.06 )
  448 !/    05-Jul-2006 : Consolidate stress vector.          ( version 3.09 )
  449 !/    04-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
  450 !/
  451 !  1. Purpose :
  452 !
  453 !     Select one of the WAVEWATCH III grids / models.
  454 !
  455 !  2. Method :
  456 !
  457 !     Point pointers to the proper variables in the proper element of
  458 !     the GRIDS array.
  459 !
  460 !  3. Parameters :
  461 !
  462 !     Parameter list
  463 !     ----------------------------------------------------------------
  464 !       IMOD    Int.   I   Model number to point to.
  465 !       NDSE    Int.   I   Error output unit number.
  466 !       NDST    Int.   I   Test output unit number.
  467 !     ----------------------------------------------------------------
  468 !
  469 !  4. Subroutines used :
  470 !
  471 !     See module documentation.
  472 !
  473 !  5. Called by :
  474 !
  475 !     Many subroutines in the WAVEWATCH system.
  476 !
  477 !  6. Error messages :
  478 !
  479 !     Checks on parameter list IMOD.
  480 !
  481 !  7. Remarks :
  482 !
  483 !  8. Structure :
  484 !
  485 !  9. Switches :
  486 !
  487 !     !/S    Enable subroutine tracing.
  488 !     !/T    Enable test output
  489 !
  490 ! 10. Source code :
  491 !
  492 !/ ------------------------------------------------------------------- /
  493       USE W3SERVMD, ONLY: EXTCDE
  494 !
  495       IMPLICIT NONE
 
 
                                                         w3wdatmd.F90  page  10
 
 
  496 !/
  497 !/ ------------------------------------------------------------------- /
  498 !/ Parameter list
  499 !/
  500       INTEGER, INTENT(IN)     :: IMOD, NDSE, NDST
  501 !/
  502 !/ ------------------------------------------------------------------- /
  503 !/ Local parameters
  504 !/
  505 !/
  506 !
  507 ! -------------------------------------------------------------------- /
  508 ! 1.  Test input and module status
  509 !
  510       IF ( NWDATA .EQ. -1 ) THEN
  511           WRITE (NDSE,1001)
  512           CALL EXTCDE (1)
  513         END IF
  514 !
  515       IF ( IMOD.LT.0 .OR. IMOD.GT.NWDATA ) THEN
  516           WRITE (NDSE,1002) IMOD, NWDATA
  517           CALL EXTCDE (2)
  518         END IF
  519 !
  520 ! -------------------------------------------------------------------- /
  521 ! 2.  Set model numbers
  522 !
  523       IWDATA = IMOD
  524 !
  525 ! -------------------------------------------------------------------- /
  526 ! 3.  Set pointers
  527 !
  528       TIME   => WDATAS(IMOD)%TIME
  529       TIMEEND => WDATAS(IMOD)%TIMEEND
  530       TLEV   => WDATAS(IMOD)%TLEV
  531       TICE   => WDATAS(IMOD)%TICE
  532       TIC1   => WDATAS(IMOD)%TIC1
  533       TIC5   => WDATAS(IMOD)%TIC5
  534       DINIT  => WDATAS(IMOD)%DINIT
  535       FL_ALL => WDATAS(IMOD)%FL_ALL
  536 !
  537       IF ( DINIT ) THEN
  538           IF ( FL_ALL ) THEN
  539             VA     => WDATAS(IMOD)%VA
  540 !!/PDLIB            VAOLD     => WDATAS(IMOD)%VAOLD
  541           END IF
  542           WLV    => WDATAS(IMOD)%WLV
  543           ICE    => WDATAS(IMOD)%ICE
  544           BERG   => WDATAS(IMOD)%BERG
  545           ICEH   => WDATAS(IMOD)%ICEH
  546           ICEF   => WDATAS(IMOD)%ICEF
  547           ICEDMAX=> WDATAS(IMOD)%ICEDMAX
  548           UST    => WDATAS(IMOD)%UST
  549           USTDIR => WDATAS(IMOD)%USTDIR
  550           ASF    => WDATAS(IMOD)%ASF
 
 
                                                         w3wdatmd.F90  page  11
 
 
  551           FPIS   => WDATAS(IMOD)%FPIS
  552         END IF
  553 !
  554       RETURN
  555 !
  556 ! Formats
  557 !
  558  1001 FORMAT (/' *** ERROR W3SETW : GRIDS NOT INITIALIZED *** '/      &
  559                '                    RUN W3NMOD FIRST '/)
  560  1002 FORMAT (/' *** ERROR W3SETW : ILLEGAL MODEL NUMBER *** '/       &
  561                '                    IMOD   = ',I10/                   &
  562                '                    NWDATA = ',I10/)
  563 !
  564 !/
  565 !/ End of W3SETW ----------------------------------------------------- /
  566 !/
  567       END SUBROUTINE W3SETW
  568 !/
  569 !/ End of module W3WDATMD -------------------------------------------- /
  570 !/
  571       END MODULE W3WDATMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
