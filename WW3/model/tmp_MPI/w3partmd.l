                                                         w3partmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3PARTMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III          USACE/NOAA |
    7 !/                  |          Barbara  Tracy           |
    8 !/                  |           H. L. Tolman            |
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :         23-Jul-2018 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    01-Nov-2006 : Origination.                        ( version 3.10 )
   14 !/    02-Nov-2006 : Adding tail to integration.         ( version 3.10 )
   15 !/    24-Mar-2007 : Bug fix IMI, adding overall field   ( version 3.11 )
   16 !/                  and sorting.
   17 !/    15-Apr-2008 : Clean up for distribution.          ( version 3.14 )
   18 !/    02-Dec-2010 : Adding a mapping PMAP between       ( version 3.14 )
   19 !/                  original and combined partitions
   20 !/                  ( M. Szyszka )
   21 !/    23-Jul-2018 : Added alternative partitioning      ( version 6.05 )
   22 !/                  methods (C. Bunney, UKMO)
   23 !  1. Purpose :
   24 !
   25 !     Spectral partitioning according to the watershed method.
   26 !
   27 !  2. Variables and types :
   28 !
   29 !      Name      Type  Scope    Description
   30 !     ----------------------------------------------------------------
   31 !      MK, MTH   Int.  Private  Dimensions of stored neighour array.
   32 !      NEIGH     I.A.  Private  Nearest Neighbor array.
   33 !     ----------------------------------------------------------------
   34 !      Note: IHMAX, HSPMIN, WSMULT, WSCUT and FLCOMB used from W3ODATMD.
   35 !
   36 !  3. Subroutines and functions :
   37 !
   38 !      Name      Type  Scope    Description
   39 !     ----------------------------------------------------------------
   40 !      W3PART    Subr. Public   Interface to watershed routines.
   41 !      PTSORT    Subr. Public   Sort discretized image.
   42 !      PTNGHB    Subr. Public   Defeine nearest neighbours.
   43 !      PT_FLD    Subr. Public   Incremental flooding algorithm.
   44 !      FIFO_ADD, FIFO_EMPTY, FIFO_FIRST
   45 !                Subr. PT_FLD   Queue management.
   46 !      PTMEAN    Subr. Public   Compute mean parameters.
   47 !     ----------------------------------------------------------------
   48 !
   49 !  4. Subroutines and functions used :
   50 !
   51 !      Name      Type  Module   Description
   52 !     ----------------------------------------------------------------
   53 !      STRACE    Subr. W3SERVMD Subroutine traceing.
   54 !      WAVNU1    Subr. W3DISPMD Wavenumber computation.
   55 !     ----------------------------------------------------------------
 
 
                                                         w3partmd.F90  page   2
 
 
   56 !
   57 !  5. Remarks :
   58 !
   59 !  6. Switches :
   60 !
   61 !     !/S    Enable subroutine tracing.
   62 !     !/T    Enable test output
   63 !
   64 !  7. Source code :
   65 !
   66 !/ ------------------------------------------------------------------- /
   67 !
   68       USE W3ODATMD, ONLY: IHMAX, HSPMIN, WSMULT, DIMP, PTMETH, PTFCUT
   69 !
   70       PUBLIC
   71 !
   72       INTEGER, PRIVATE              :: MK = -1, MTH = -1
   73       INTEGER, ALLOCATABLE, PRIVATE :: NEIGH(:,:)
   74 !/
   75       CONTAINS
   76 !/ ------------------------------------------------------------------- /
   77       SUBROUTINE W3PART ( SPEC, UABS, UDIR, DEPTH, WN, NP, XP, DIMXP )
   78 !/
   79 !/                  +-----------------------------------+
   80 !/                  | WAVEWATCH III          USACE/NOAA |
   81 !/                  |          Barbara  Tracy           |
   82 !/                  |           H. L. Tolman            |
   83 !/                  |                        FORTRAN 90 |
   84 !/                  | Last update :         02-Dec-2010 !
   85 !/                  +-----------------------------------+
   86 !/
   87 !/    28-Oct-2006 : Origination.                       ( version 3.10 )
   88 !/    02-Dec-2010 : Adding a mapping PMAP between      ( version 3.14 )
   89 !/                  original and combined partitions
   90 !/                  ( M. Szyszka )
   91 !/
   92 !  1. Purpose :
   93 !
   94 !     Interface to watershed partitioning routines.
   95 !
   96 !  2. Method :
   97 !
   98 !     Watershed Algorithm of Vincent and Soille, 1991, implemented by
   99 !     Barbara Tracy (USACE/ERDC) for NOAA/NCEP.
  100 !
  101 !  3. Parameters :
  102 !
  103 !     Parameter list
  104 !     ----------------------------------------------------------------
  105 !       SPEC    R.A.   I   2-D spectrum E(f,theta).
  106 !       UABS    Real   I   Wind speed.
  107 !       UDIR    Real   I   Wind direction.
  108 !       DEPTH   Real   I   Water depth.
  109 !       WN      R.A.   I   Wavenumebers for each frequency.
  110 !       NP      Int.   O   Number of partitions.
 
 
                                                         w3partmd.F90  page   3
 
 
  111 !                           -1 : Spectrum without minumum energy.
  112 !                            0 : Spectrum with minumum energy.
  113 !                                but no partitions.
  114 !       XP      R.A.   O   Parameters describing partitions.
  115 !                          Entry '0' contains entire spectrum.
  116 !       DIMXP   Int.   I   Second dimension of XP.
  117 !     ----------------------------------------------------------------
  118 !
  119 !  4. Subroutines used :
  120 !
  121 !      Name      Type  Module   Description
  122 !     ----------------------------------------------------------------
  123 !      STRACE    Sur.  W3SERVMD Subroutine tracing.
  124 !     ----------------------------------------------------------------
  125 !
  126 !  5. Called by :
  127 !
  128 !  6. Error messages :
  129 !
  130 !  7. Remarks :
  131 !
  132 !     - To achieve minimum storage but guaranteed storage of all
  133 !       partitions DIMXP = ((NK+1)/2) * ((NTH-1)/2) unless specified
  134 !       otherwise below.
  135 !
  136 !     This version of W3PART contains alternate Met Office partitioning
  137 !     methods, selected at runtime using the PTMETH namlist variable:
  138 !         1) Standard WW3 partitioning
  139 !         2) Met Office extended partitioning using split-partitions
  140 !            (removes the wind sea part of any swell partiton and combines
  141 !            with total wind sea partition).
  142 !         3) Met Office "wave systems" - no classification or combining of
  143 !            wind sea partitions. All partitions output and ordered simply
  144 !            by wave height.
  145 !         4) Classic, simple wave age based partitioning generating
  146 !            a single wind sea and swell partition. [DIMXP = 2]
  147 !         5) 2-band partitioning; produces hi and low freqency band parti>
  148 !            using a user-defined cutoff frequency (PTFCUT). [DIMXP = 2]
  149 !
  150 !     (Chris Bunney, UK Met Office, Jul 2018)
  151 !
  152 !  8. Structure :
  153 !
  154 !  9. Switches :
  155 !
  156 !     !/S    Enable subroutine tracing.
  157 !     !/T    Enable test output
  158 !
  159 ! 10. Source code :
  160 !
  161 !/ ------------------------------------------------------------------- /
  162 !/
  163       USE CONSTANTS
  164 !
  165       USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, TH
 
 
                                                         w3partmd.F90  page   4
 
 
  166       USE W3ODATMD, ONLY: WSCUT, FLCOMB
  167 !
  168       IMPLICIT NONE
  169 !/
  170 !/ ------------------------------------------------------------------- /
  171 !/ Parameter list
  172 !/
  173       INTEGER, INTENT(OUT)          :: NP
  174       INTEGER, INTENT(IN)           :: DIMXP
  175       REAL, INTENT(IN)              :: SPEC(NK,NTH), WN(NK), UABS,    &
  176                                        UDIR, DEPTH
  177       REAL, INTENT(OUT)             :: XP(DIMP,0:DIMXP)
  178 !/
  179 !/ ------------------------------------------------------------------- /
  180 !/ Local parameters
  181 !/
  182       INTEGER                 :: ITH, IMI(NSPEC), IMD(NSPEC),         &
  183                                  IMO(NSPEC), IND(NSPEC), NP_MAX,      &
  184                                  IP, IT(1), INDEX(DIMXP), NWS,        &
  185                                  IPW, IPT, ISP
  186       INTEGER                 :: PMAP(DIMXP)
  187       REAL                    :: ZP(NSPEC), ZMIN, ZMAX, Z(NSPEC),     &
  188                                  FACT, WSMAX, HSMAX
  189       REAL                    :: TP(DIMP,DIMXP)
  190       INTEGER                 :: IK, WIND_PART    ! ChrisB; added for new
  191       REAL                    :: C, UPAR, SIGCUT  ! UKMO partioning metho>
  192 !/
  193 !/ ------------------------------------------------------------------- /
  194 ! 0.  Initializations
  195 !
  196       NP     = 0
  197       XP     = 0.
  198 !
  199 ! -------------------------------------------------------------------- /
  200 ! 1.  Process input spectrum
  201 ! 1.a 2-D to 1-D spectrum
  202 !
  203       DO ITH=1, NTH
  204         ZP(1+(ITH-1)*NK:ITH*NK) = SPEC(:,ITH)
  205         END DO
  206               
  207 !
  208 ! PTMETH == 4 : Do simple partitioning based solely on the
  209 ! wave age criterion (produces one swell and one wind sea only):
  210 !
  211       IF( PTMETH .EQ. 4 ) THEN
  212         DO IK=1, NK
  213           DO ITH=1, NTH
  214              ISP = IK + (ITH-1) * NK ! index into partition array IMO
  215                                                                      
  216              UPAR = WSMULT * UABS * MAX(0.0, COS(TH(ITH)-DERA*UDIR))
  217              C = SIG(IK) / WN(IK)
  218                                  
  219              IF( UPAR .LE. C ) THEN
  220                ! Is swell:
 
 
                                                         w3partmd.F90  page   5
 
 
  221                IMO(ISP) = 2
  222              ELSE
  223                ! Is wind sea:
  224                IMO(ISP) = 1
  225              ENDIF
  226           ENDDO
  227         ENDDO
  228              
  229         ! We have a max of up to two partitions:
  230         NP_MAX=2
  231                 
  232         ! Calculate mean parameters:
  233         CALL PTMEAN ( NP_MAX, IMO, ZP, DEPTH, UABS, UDIR, WN,           &
  234                     NP, XP, DIMXP, PMAP )
  235                                          
  236         ! No more processing required, return:
  237         RETURN
  238       ENDIF ! PTMETH == 4
  239 !
  240 ! PTMETH == 5 : produce "high" and "low" band partitions
  241 ! using a frequency cutoff:
  242 !
  243       IF( PTMETH .EQ. 5 ) THEN
  244         SIGCUT = TPI * PTFCUT
  245         DO IK = 1, NK
  246           ! If bin center <= freq cutoff then mark as "low band".
  247           IF(SIG(IK) .LE. SIGCUT) THEN
  248             IP = 2
  249           ELSE
  250             IP = 1
  251           ENDIF
  252                
  253           DO ITH=1, NTH
  254             ISP = IK + (ITH-1) * NK ! index into partition array IMO
  255             IMO(ISP) = IP
  256           ENDDO
  257         ENDDO
  258              
  259         ! We only ever have 2 partitions:
  260         NP_MAX=2
  261                 
  262         ! Calculate mean parameters:
  263         CALL PTMEAN ( NP_MAX, IMO, ZP, DEPTH, UABS, UDIR, WN,           &
  264                     NP, XP, DIMXP, PMAP )
  265                                          
  266         ! No more processing required, return:
  267         RETURN
  268       ENDIF ! PTMETH == 5
  269 !
  270 ! 1.b Invert spectrum and 'digitize'
  271 !
  272       ZMIN   = MINVAL ( ZP )
  273       ZMAX   = MAXVAL ( ZP )
  274       IF ( ZMAX-ZMIN .LT. 1.E-9 ) RETURN
  275 !
 
 
                                                         w3partmd.F90  page   6
 
 
  276       Z      = ZMAX - ZP
  277 !
  278       FACT   = REAL(IHMAX-1) / ( ZMAX - ZMIN )
  279       IMI    = MAX ( 1 , MIN ( IHMAX , NINT ( 1. + Z*FACT ) ) )
  280 !
  281 ! 1.c Sort digitized image
  282 !
  283       CALL PTSORT ( IMI, IND, IHMAX )
  284 !
  285 ! -------------------------------------------------------------------- /
  286 ! 2.  Perform partitioning
  287 ! 2.a Update nearest neighbor info as needed.
  288 !
  289       CALL PTNGHB
  290 !
  291 ! 2.b Incremental flooding
  292 !
  293       CALL PT_FLD ( IMI, IND, IMO, ZP, NP_MAX )
  294 !
  295 ! 2.c Compute parameters per partition
  296 !     NP and NX initialized inside routine.
  297 !
  298       CALL PTMEAN ( NP_MAX, IMO, ZP, DEPTH, UABS, UDIR, WN,           &
  299                     NP, XP, DIMXP, PMAP )
  300 !
  301 ! 2.d PTMETH == 2: move the wind sea part of the partitions into a
  302 !     seperate partition and recalculate the mean parameters.
  303 !
  304       IF ( NP .GT. 0 .AND. PTMETH .EQ. 2 ) THEN
  305          WIND_PART = NP_MAX
  306                            
  307           DO IK=1, NK
  308             DO ITH=1, NTH
  309                ISP = IK + (ITH-1) * NK ! index into partition array IMO
  310                UPAR = WSMULT * UABS * MAX(0.0, COS(TH(ITH)-DERA*UDIR))
  311                C = SIG(IK) / WN(IK)
  312                                    
  313                IF( C .LT. UPAR ) THEN
  314                   ! Bin is wind forced - mark as new wind partition:
  315                   WIND_PART = NP_MAX + 1
  316                                         
  317                   ! Update status map to show new wind partition
  318                   IMO(ISP) = WIND_PART
  319                ENDIF
  320             ENDDO
  321           ENDDO
  322                
  323           IF( WIND_PART .NE. NP_MAX ) THEN
  324              ! Some bins were marked as wind sea - recalculate
  325              ! integrated parameters:
  326              NP_MAX = WIND_PART
  327              CALL PTMEAN ( NP_MAX, IMO, ZP, DEPTH, UABS, UDIR, WN,     &
  328                     NP, XP, DIMXP, PMAP )
  329           ENDIF
  330       ENDIF
 
 
                                                         w3partmd.F90  page   7
 
 
  331 !
  332 ! -------------------------------------------------------------------- /
  333 ! 3.  Sort and recombine wind seas as needed
  334 ! 3.a Sort by wind sea fraction
  335 !
  336       IF ( NP .LE. 1 ) RETURN
  337                              
  338       ! -----------------------------------------------------------------
  339       ! PTMETH == 3: Don't classify or combine any partitions as wind sea.
  340       ! Simply sort by HS and return.
  341       ! -----------------------------------------------------------------
  342       IF( PTMETH .EQ. 3 ) THEN
  343         TP(:,1:NP)  = XP(:,1:NP)
  344         XP(:,1:NP)  = 0.
  345                         
  346         DO IP=1, NP
  347           IT          = MAXLOC(TP(1,1:NP))
  348           XP(:,IP)    = TP(:,IT(1))
  349           TP(1,IT(1)) = -1.
  350         END DO
  351               
  352         RETURN ! Don't process any further
  353       ENDIF ! PTMETH == 3
  354                          
  355       ! -----------------------------------------------------------------
  356       ! PTMETH == 1: Default WW3 partitioning.
  357       ! -----------------------------------------------------------------
  358       TP(:,1:NP)  = XP(:,1:NP)
  359       XP(:,1:NP)  = 0.
  360       INDEX(1:NP) = 0
  361       NWS         = 0
  362 !
  363       DO IP=1, NP
  364         IT          = MAXLOC(TP(6,1:NP))
  365         INDEX(IP)   = IT(1)
  366         XP(:,IP)    = TP(:,INDEX(IP))
  367         IF ( TP(6,IT(1)) .GE. WSCUT ) NWS = NWS + 1
  368         TP(6,IT(1)) = -1.
  369         END DO
  370 !
  371 ! 3.b Combine wind seas as needed and resort
  372 !
  373       IF ( NWS.GT.1 .AND. FLCOMB ) THEN
  374           IPW    = PMAP(INDEX(1))
  375           DO IP=2, NWS
  376              IPT    = PMAP(INDEX(IP))
  377              DO ISP=1, NSPEC
  378                 IF ( IMO(ISP) .EQ. IPT ) IMO(ISP) = IPW
  379              END DO
  380           END DO
  381 !
  382           CALL PTMEAN ( NP_MAX, IMO, ZP, DEPTH, UABS, UDIR, WN,       &
  383                         NP, XP, DIMXP, PMAP )
  384           IF ( NP .LE. 1 ) RETURN
  385 !
 
 
                                                         w3partmd.F90  page   8
 
 
  386           TP(:,1:NP)  = XP(:,1:NP)
  387           XP(:,1:NP)  = 0.
  388           INDEX(1:NP) = 0
  389           NWS         = 0
  390 !
  391           DO IP=1, NP
  392             IT          = MAXLOC(TP(6,1:NP))
  393             INDEX(IP)   = IT(1)
  394             XP(:,IP)    = TP(:,INDEX(IP))
  395             IF ( TP(6,IT(1)) .GE. WSCUT ) NWS = NWS + 1
  396             TP(6,IT(1)) = -1.
  397             END DO
  398 !
  399         END IF
  400 !
  401 ! 3.c Sort remaining fields by wave height
  402 !
  403       NWS    = MIN ( 1 , NWS )
  404 !
  405       TP(:,1:NP)  = XP(:,1:NP)
  406       XP(:,1:NP)  = 0.
  407 !
  408       IF ( NWS .GT. 0 ) THEN
  409           XP(:,1) = TP(:,1)
  410           TP(1,1) = -1.
  411           NWS     = 1
  412         END IF
  413 !
  414       DO IP=NWS+1, NP
  415         IT          = MAXLOC(TP(1,1:NP))
  416         XP(:,IP)    = TP(:,IT(1))
  417         TP(1,IT(1)) = -1.
  418         END DO
  419 !
  420 ! -------------------------------------------------------------------- /
  421 ! 4.  End of routine
  422 !
  423       RETURN
  424 !/
  425 !/ End of W3PART ----------------------------------------------------- /
  426 !/
  427       END SUBROUTINE W3PART
  428 !/ ------------------------------------------------------------------- /
  429       SUBROUTINE PTSORT ( IMI, IND, IHMAX )
  430 !/
  431 !/                  +-----------------------------------+
  432 !/                  | WAVEWATCH III          USACE/NOAA |
  433 !/                  |          Barbara  Tracy           |
  434 !/                  |           H. L. Tolman            |
  435 !/                  |                        FORTRAN 90 |
  436 !/                  | Last update :         19-Oct-2006 !
  437 !/                  +-----------------------------------+
  438 !/
  439 !/    19-Oct-2006 : Origination.                       ( version 3.10 )
  440 !/
 
 
                                                         w3partmd.F90  page   9
 
 
  441 !  1. Purpose :
  442 !
  443 !     This subroutine sorts the image data in ascending order.
  444 !     This sort original to F.T.Tracy (2006)
  445 !
  446 !  3. Parameters :
  447 !
  448 !     Parameter list
  449 !     ----------------------------------------------------------------
  450 !       IMI     I.A.   I   Input discretized spectrum.
  451 !       IND     I.A.   O   Sorted data.
  452 !       IHMAX   Int.   I   Number of integer levels.
  453 !     ----------------------------------------------------------------
  454 !
  455 !  4. Subroutines used :
  456 !
  457 !      Name      Type  Module   Description
  458 !     ----------------------------------------------------------------
  459 !      STRACE    Sur.  W3SERVMD Subroutine tracing.
  460 !     ----------------------------------------------------------------
  461 !
  462 ! 10. Source code :
  463 !
  464 !/ ------------------------------------------------------------------- /
  465 !
  466       USE W3GDATMD, ONLY: NSPEC
  467 !
  468       IMPLICIT NONE
  469 !/
  470 !/ ------------------------------------------------------------------- /
  471 !/ Parameter list
  472 !/
  473       INTEGER, INTENT(IN)      :: IHMAX, IMI(NSPEC)
  474       INTEGER, INTENT(OUT)     :: IND(NSPEC)
  475 !/
  476 !/ ------------------------------------------------------------------- /
  477 !/ Local parameters
  478 !/
  479       INTEGER                 :: I, IN, IV
  480       INTEGER                 :: NUMV(IHMAX), IADDR(IHMAX),           &
  481                                  IORDER(NSPEC)
  482 !/
  483 !
  484 ! -------------------------------------------------------------------- /
  485 ! 1.  Occurences per height
  486 !
  487       NUMV   = 0
  488       DO I=1, NSPEC
  489         NUMV(IMI(I)) = NUMV(IMI(I)) + 1
  490         END DO
  491 !
  492 ! -------------------------------------------------------------------- /
  493 ! 2.  Starting address per height
  494 !
  495       IADDR(1) = 1
 
 
                                                         w3partmd.F90  page  10
 
 
  496       DO I=1, IHMAX-1
  497         IADDR(I+1) = IADDR(I) + NUMV(I)
  498       END DO
  499 !
  500 ! -------------------------------------------------------------------- /
  501 ! 3.  Order points
  502 !
  503       DO I=1, NSPEC
  504         IV        = IMI(I)
  505         IN        = IADDR(IV)
  506         IORDER(I) = IN
  507         IADDR(IV) = IN + 1
  508         END DO
  509 !
  510 ! -------------------------------------------------------------------- /
  511 ! 4.  Sort points
  512 !
  513       DO I=1, NSPEC
  514         IND(IORDER(I)) = I
  515         END DO
  516 !
  517       RETURN
  518 !/
  519 !/ End of PTSORT ----------------------------------------------------- /
  520 !/
  521       END SUBROUTINE PTSORT
  522 !/ ------------------------------------------------------------------- /
  523       SUBROUTINE PTNGHB
  524 !/
  525 !/                  +-----------------------------------+
  526 !/                  | WAVEWATCH III          USACE/NOAA |
  527 !/                  |          Barbara  Tracy           |
  528 !/                  |           H. L. Tolman            |
  529 !/                  |                        FORTRAN 90 |
  530 !/                  | Last update :         20-Oct-2006 !
  531 !/                  +-----------------------------------+
  532 !/
  533 !/    20-Oct-2006 : Origination.                       ( version 3.10 )
  534 !/
  535 !  1. Purpose :
  536 !
  537 !     This subroutine computes the nearest neighbors for each grid
  538 !     point. Wrapping of directional distribution (0 to 360)is taken
  539 !     care of using the nearest neighbor system
  540 !
  541 !  3. Parameters :
  542 !
  543 !     Parameter list
  544 !     ----------------------------------------------------------------
  545 !       IMI     I.A.   I   Input discretized spectrum.
  546 !       IMD     I.A.   O   Sorted data.
  547 !       IHMAX   Int.   I   Number of integer levels.
  548 !     ----------------------------------------------------------------
  549 !
  550 !  4. Subroutines used :
 
 
                                                         w3partmd.F90  page  11
 
 
  551 !
  552 !      Name      Type  Module   Description
  553 !     ----------------------------------------------------------------
  554 !      STRACE    Sur.  W3SERVMD Subroutine tracing.
  555 !     ----------------------------------------------------------------
  556 !
  557 ! 10. Source code :
  558 !
  559 !/ ------------------------------------------------------------------- /
  560 !
  561       USE W3GDATMD, ONLY: NK, NTH, NSPEC
  562 !
  563       IMPLICIT NONE
  564 !/
  565 !/ ------------------------------------------------------------------- /
  566 !/ Parameter list
  567 !/
  568 !     INTEGER, INTENT(IN)      :: IHMAX, IMI(NSPEC)
  569 !     INTEGER, INTENT(IN)      :: IMD(NSPEC)
  570 !/
  571 !/ ------------------------------------------------------------------- /
  572 !/ Local parameters
  573 !/
  574       INTEGER                 :: N, J, I, K
  575 !/
  576 !
  577 ! -------------------------------------------------------------------- /
  578 ! 1.  Check on need of processing
  579 !
  580       IF ( MK.EQ.NK .AND. MTH.EQ.NTH ) RETURN
  581 !
  582       IF ( MK.GT.0 ) DEALLOCATE ( NEIGH )
  583       ALLOCATE ( NEIGH(9,NSPEC) )
  584       MK     = NK
  585       MTH    = NTH
  586 !
  587 ! -------------------------------------------------------------------- /
  588 ! 2.  Build map
  589 !
  590       NEIGH  = 0
  591 !
  592 ! ... Base loop
  593 !
  594       DO N = 1, NSPEC
  595 !
  596         J      = (N-1) / NK + 1
  597         I      = N - (J-1) * NK
  598         K      = 0
  599 !
  600 ! ... Point at the left(1)
  601 !
  602         IF ( I .NE. 1 ) THEN
  603             K           = K + 1
  604             NEIGH(K, N) = N - 1
  605           END IF
 
 
                                                         w3partmd.F90  page  12
 
 
  606 !
  607 ! ... Point at the right (2)
  608 !
  609         IF ( I .NE. NK ) THEN
  610             K           = K + 1
  611             NEIGH(K, N) = N + 1
  612           END IF
  613 !
  614 ! ... Point at the bottom(3)
  615 !
  616         IF ( J .NE. 1 ) THEN
  617             K           = K + 1
  618             NEIGH(K, N) = N - NK
  619           END IF
  620 !
  621 ! ... ADD Point at bottom_wrap to top
  622 !
  623         IF ( J .EQ. 1 ) THEN
  624             K          = K + 1
  625             NEIGH(K,N) = NSPEC - (NK-I)
  626           END IF
  627 !
  628 ! ... Point at the top(4)
  629 !
  630         IF ( J .NE. NTH ) THEN
  631             K           = K + 1
  632             NEIGH(K, N) = N + NK
  633           END IF
  634 !
  635 ! ... ADD Point to top_wrap to bottom
  636 !
  637          IF ( J .EQ. NTH ) THEN
  638              K          = K + 1
  639              NEIGH(K,N) = N - (NTH-1) * NK
  640             END IF
  641 !
  642 ! ... Point at the bottom, left(5)
  643 !
  644         IF ( (I.NE.1) .AND. (J.NE.1) ) THEN
  645             K           = K + 1
  646             NEIGH(K, N) = N - NK - 1
  647           END IF
  648 !
  649 ! ... Point at the bottom, left with wrap.
  650 !
  651          IF ( (I.NE.1) .AND. (J.EQ.1) ) THEN
  652              K          = K + 1
  653              NEIGH(K,N) = N - 1 + NK * (NTH-1)
  654            END IF
  655 !
  656 ! ... Point at the bottom, right(6)
  657 !
  658         IF ( (I.NE.NK) .AND. (J.NE.1) ) THEN
  659             K           = K + 1
  660             NEIGH(K, N) = N - NK + 1
 
 
                                                         w3partmd.F90  page  13
 
 
  661           END IF
  662 !
  663 ! ... Point at the bottom, right with wrap
  664 !
  665         IF ( (I.NE.NK) .AND. (J.EQ.1) ) THEN
  666             K           = K + 1
  667             NEIGH(K,N) = N + 1 + NK * (NTH - 1)
  668           END  IF
  669 !
  670 ! ... Point at the top, left(7)
  671 !
  672         IF ( (I.NE.1) .AND. (J.NE.NTH) ) THEN
  673             K           = K + 1
  674             NEIGH(K, N) = N + NK - 1
  675           END IF
  676 !
  677 ! ... Point at the top, left with wrap
  678 !
  679          IF ( (I.NE.1) .AND. (J.EQ.NTH) ) THEN
  680              K           = K + 1
  681              NEIGH(K,N) = N - 1 - (NK) * (NTH-1)
  682            END IF
  683 !
  684 ! ... Point at the top, right(8)
  685 !
  686         IF ( (I.NE.NK) .AND. (J.NE.NTH) ) THEN
  687             K           = K + 1
  688             NEIGH(K, N) = N + NK + 1
  689           END IF
  690 !
  691 ! ... Point at top, right with wrap
  692 !
  693         IF ( (I.NE.NK) .AND. (J.EQ.NTH) ) THEN
  694             K           = K + 1
  695             NEIGH(K,N) = N + 1 - (NK) * (NTH-1)
  696           END IF
  697 !
  698         NEIGH(9,N) = K
  699 !
  700         END DO
  701 !
  702       RETURN
  703 !/
  704 !/ End of PTNGHB ----------------------------------------------------- /
  705 !/
  706       END SUBROUTINE PTNGHB
  707 !/ ------------------------------------------------------------------- /
  708       SUBROUTINE PT_FLD ( IMI, IND, IMO, ZP, NPART )
  709 !/
  710 !/                  +-----------------------------------+
  711 !/                  | WAVEWATCH III           NOAA/NCEP |
  712 !/                  |           H. L. Tolman            |
  713 !/                  |                        FORTRAN 90 |
  714 !/                  | Last update :         01-Nov-2006 !
  715 !/                  +-----------------------------------+
 
 
                                                         w3partmd.F90  page  14
 
 
  716 !/
  717 !/    01-Nov-2006 : Origination.                       ( version 3.10 )
  718 !/
  719 !  1. Purpose :
  720 !
  721 !     This subroutine does incremental flooding of the image to
  722 !     determine the watershed image.
  723 !
  724 !  3. Parameters :
  725 !
  726 !     Parameter list
  727 !     ----------------------------------------------------------------
  728 !       IMI     I.A.   I   Input discretized spectrum.
  729 !       IND     I.A.   I   Sorted addresses.
  730 !       IMO     I.A.   O   Output partitioned spectrum.
  731 !       ZP      R.A.   I   Spectral array.
  732 !       NPART   Int.   O   Number of partitions found.
  733 !     ----------------------------------------------------------------
  734 !
  735 !  4. Subroutines used :
  736 !
  737 !      Name      Type  Module   Description
  738 !     ----------------------------------------------------------------
  739 !      STRACE    Sur.  W3SERVMD Subroutine tracing.
  740 !     ----------------------------------------------------------------
  741 !
  742 ! 10. Source code :
  743 !
  744 !/ ------------------------------------------------------------------- /
  745 !
  746       USE W3GDATMD, ONLY: NSPEC
  747 !
  748       IMPLICIT NONE
  749 !/
  750 !/ ------------------------------------------------------------------- /
  751 !/ Parameter list
  752 !/
  753       INTEGER, INTENT(IN)     :: IMI(NSPEC), IND(NSPEC)
  754       INTEGER, INTENT(OUT)    :: IMO(NSPEC), NPART
  755       REAL, INTENT(IN)        :: ZP(NSPEC)
  756 !/
  757 !/ ------------------------------------------------------------------- /
  758 !/ Local parameters
  759 !/
  760       INTEGER                 :: MASK, INIT, IWSHED, IMD(NSPEC),      &
  761                                  IC_LABEL, IFICT_PIXEL, M, IH, MSAVE, &
  762                                  IP, I, IPP, IC_DIST, IEMPTY, IPPP,   &
  763                                  JL, JN, IPT, J
  764       INTEGER                 :: IQ(NSPEC), IQ_START, IQ_END
  765       REAL                    :: ZPMAX, EP1, DIFF
  766 !/
  767 !
  768 ! -------------------------------------------------------------------- /
  769 ! 0.  Initializations
  770 !
 
 
                                                         w3partmd.F90  page  15
 
 
  771       MASK        = -2
  772       INIT        = -1
  773       IWSHED      =  0
  774       IMO         = INIT
  775       IC_LABEL    =  0
  776       IMD         =  0
  777       IFICT_PIXEL = -100
  778 !
  779       IQ_START    =  1
  780       IQ_END      =  1
  781 !
  782       ZPMAX       = MAXVAL ( ZP )
  783 !
  784 ! -------------------------------------------------------------------- /
  785 ! 1.  Loop over levels
  786 !
  787       M      =  1
  788 !
  789       DO IH=1, IHMAX
  790         MSAVE  = M
  791 !
  792 ! 1.a Pixels at level IH
  793 !
  794         DO
  795           IP     = IND(M)
  796           IF ( IMI(IP) .NE. IH ) EXIT
  797 !
  798 !     Flag the point, if it stays flagge, it is a separate minimum.
  799 !
  800           IMO(IP) = MASK
  801 !
  802 !     Consider neighbors. If there is neighbor, set distance and add
  803 !     to queue.
  804 !
  805           DO I=1, NEIGH(9,IP)
  806             IPP    = NEIGH(I,IP)
  807             IF ( (IMO(IPP).GT.0) .OR. (IMO(IPP).EQ.IWSHED) ) THEN
  808                 IMD(IP) = 1
  809                 CALL FIFO_ADD (IP)
  810                 EXIT
  811               END IF
  812             END DO
  813 !
  814           IF ( M+1 .GT. NSPEC ) THEN
  815               EXIT
  816             ELSE
  817               M = M + 1
  818             END IF
  819 !
  820           END DO
  821 !
  822 ! 1.b Process the queue
  823 !
  824         IC_DIST = 1
  825         CALL FIFO_ADD (IFICT_PIXEL)
 
 
                                                         w3partmd.F90  page  16
 
 
  826 !
  827         DO
  828           CALL FIFO_FIRST (IP)
  829 !
  830 !     Check for end of processing
  831 !
  832           IF ( IP .EQ. IFICT_PIXEL ) THEN
  833               CALL FIFO_EMPTY (IEMPTY)
  834               IF ( IEMPTY .EQ. 1 ) THEN
  835                   EXIT
  836                 ELSE
  837                   CALL FIFO_ADD (IFICT_PIXEL)
  838                   IC_DIST = IC_DIST + 1
  839                   CALL FIFO_FIRST (IP)
  840                 END IF
  841             END IF
  842 !
  843 !     Process queue
  844 !
  845           DO I=1, NEIGH(9,IP)
  846             IPP = NEIGH(I,IP)
  847 !
  848 !     Check for labeled watersheds or basins
  849 !
  850             IF ( (IMD(IPP).LT.IC_DIST) .AND. ( (IMO(IPP).GT.0) .OR.  &
  851                  (IMO(IPP).EQ.IWSHED))) THEN
  852 !
  853                 IF ( IMO(IPP) .GT. 0 ) THEN
  854 !
  855                     IF ((IMO(IP) .EQ. MASK) .OR. (IMO(IP) .EQ. &
  856                         IWSHED)) THEN
  857                         IMO(IP) = IMO(IPP)
  858                       ELSE IF (IMO(IP) .NE. IMO(IPP)) THEN
  859                         IMO(IP) = IWSHED
  860                       END IF
  861 !
  862                   ELSE IF (IMO(IP) .EQ. MASK) THEN
  863 !
  864                     IMO(IP) = IWSHED
  865 !
  866                   END IF
  867 !
  868               ELSE IF ( (IMO(IPP).EQ.MASK) .AND. (IMD(IPP).EQ.0) ) THEN
  869 !
  870                  IMD(IPP) = IC_DIST + 1
  871                  CALL FIFO_ADD (IPP)
  872 !
  873               END IF
  874 !
  875             END DO
  876 !
  877           END DO
  878 !
  879 ! 1.c Check for mask values in IMO to identify new basins
  880 !
 
 
                                                         w3partmd.F90  page  17
 
 
  881         M = MSAVE
  882 !
  883         DO
  884           IP     = IND(M)
  885           IF ( IMI(IP) .NE. IH ) EXIT
  886           IMD(IP) = 0
  887 !
  888           IF (IMO(IP) .EQ. MASK) THEN
  889 !
  890 ! ... New label for pixel
  891 !
  892               IC_LABEL = IC_LABEL + 1
  893               CALL FIFO_ADD (IP)
  894               IMO(IP) = IC_LABEL
  895 !
  896 ! ... and all connected to it ...
  897 !
  898               DO
  899                 CALL FIFO_EMPTY (IEMPTY)
  900                 IF ( IEMPTY .EQ. 1 ) EXIT
  901                 CALL FIFO_FIRST (IPP)
  902 !
  903                 DO I=1, NEIGH(9,IPP)
  904                   IPPP   = NEIGH(I,IPP)
  905                   IF ( IMO(IPPP) .EQ. MASK ) THEN
  906                       CALL FIFO_ADD (IPPP)
  907                       IMO(IPPP) = IC_LABEL
  908                     END IF
  909                   END DO
  910 !
  911                 END DO
  912 !
  913             END IF
  914 !
  915           IF ( M + 1 .GT. NSPEC ) THEN
  916               EXIT
  917             ELSE
  918               M = M + 1
  919             END IF
  920 !
  921           END DO
  922 !
  923         END DO
  924 !
  925 ! -------------------------------------------------------------------- /
  926 ! 2.  Find nearest neighbor of 0 watershed points and replace
  927 !     use original input to check which group to affiliate with 0
  928 !     Soring changes first in IMD to assure symetry in adjustment.
  929 !
  930       DO J=1, 5
  931         IMD    = IMO
  932         DO JL=1 , NSPEC
  933           IPT    = -1
  934           IF ( IMO(JL) .EQ. 0 ) THEN
  935               EP1    = ZPMAX
 
 
                                                         w3partmd.F90  page  18
 
 
  936               DO JN=1, NEIGH (9,JL)
  937                 DIFF   = ABS ( ZP(JL) - ZP(NEIGH(JN,JL)))
  938                 IF ( (DIFF.LE.EP1) .AND. (IMO(NEIGH(JN,JL)).NE.0) ) THEN
  939                     EP1    = DIFF
  940                     IPT    = JN
  941                   END IF
  942                 END DO
  943               IF ( IPT .GT. 0 ) IMD(JL) = IMO(NEIGH(IPT,JL))
  944             END IF
  945           END DO
  946         IMO    = IMD
  947         IF ( MINVAL(IMO) .GT. 0 ) EXIT
  948         END DO
  949 !
  950       NPART = IC_LABEL
  951 !
  952       RETURN
  953 !
  954       CONTAINS
  955 !/ ------------------------------------------------------------------- /
  956       SUBROUTINE FIFO_ADD ( IV )
  957 !
  958 !     Add point to FIFO queue.
  959 !
  960       INTEGER, INTENT(IN)      :: IV
  961 !
  962       IQ(IQ_END) = IV
  963 !
  964       IQ_END = IQ_END + 1
  965       IF ( IQ_END .GT. NSPEC ) IQ_END = 1
  966 !
  967       RETURN
  968       END SUBROUTINE
  969 !/ ------------------------------------------------------------------- /
  970       SUBROUTINE FIFO_EMPTY ( IEMPTY )
  971 !
  972 !     Check if queue is empty.
  973 !
  974       INTEGER, INTENT(OUT)     :: IEMPTY
  975 !
  976       IF ( IQ_START .NE. IQ_END ) THEN
  977         IEMPTY = 0
  978       ELSE
  979         IEMPTY = 1
  980       END IF
  981 !
  982       RETURN
  983       END SUBROUTINE
  984 !/ ------------------------------------------------------------------- /
  985       SUBROUTINE FIFO_FIRST ( IV )
  986 !
  987 !     Get point out of queue.
  988 !
  989       INTEGER, INTENT(OUT)     :: IV
  990 !
 
 
                                                         w3partmd.F90  page  19
 
 
  991       IV = IQ(IQ_START)
  992 !
  993       IQ_START = IQ_START + 1
  994       IF ( IQ_START .GT. NSPEC ) IQ_START = 1
  995 !
  996       RETURN
  997       END SUBROUTINE
  998 !/
  999 !/ End of PT_FLD ----------------------------------------------------- /
 1000 !/
 1001       END SUBROUTINE PT_FLD
 1002 !/ ------------------------------------------------------------------- /
 1003       SUBROUTINE PTMEAN ( NPI, IMO, ZP, DEPTH, UABS, UDIR, WN,        &
 1004                           NPO, XP, DIMXP, PMAP )
 1005 !/
 1006 !/                  +-----------------------------------+
 1007 !/                  | WAVEWATCH III          USACE/NOAA |
 1008 !/                  |          Barbara  Tracy           |
 1009 !/                  |           H. L. Tolman            |
 1010 !/                  |                        FORTRAN 90 |
 1011 !/                  | Last update :         02-Dec-2010 !
 1012 !/                  +-----------------------------------+
 1013 !/
 1014 !/    28-Oct-2006 : Origination.                       ( version 3.10 )
 1015 !/    02-Nov-2006 : Adding tail to integration.        ( version 3.10 )
 1016 !/    24-Mar-2007 : Adding overall field.              ( version 3.11 )
 1017 !/    02-Dec-2010 : Adding a mapping PMAP between      ( version 3.14 )
 1018 !/                  original and combined partitions
 1019 !/                  ( M. Szyszka )
 1020 !/
 1021 !  1. Purpose :
 1022 !
 1023 !     Compute mean parameters per partition.
 1024 !
 1025 !  3. Parameters :
 1026 !
 1027 !     Parameter list
 1028 !     ----------------------------------------------------------------
 1029 !       NPI     Int.   I   Number of partitions found.
 1030 !       IMO     I.A.   I   Partition map.
 1031 !       ZP      R.A.   I   Input spectrum.
 1032 !       DEPTH   Real   I   Water depth.
 1033 !       UABS    Real   I   Wind speed.
 1034 !       UDIR    Real   I   Wind direction.
 1035 !       WN      R.A.   I   Wavenumebers for each frequency.
 1036 !       NPO     Int.   O   Number of partitions with mean parameters.
 1037 !       XP      R.A.   O   Array with output parameters.
 1038 !       DIMXP   int.   I   Second dimension of XP.
 1039 !       PMAP    I.A.   O   Mapping between orig. and combined partitions
 1040 !     ----------------------------------------------------------------
 1041 !
 1042 !  4. Subroutines used :
 1043 !
 1044 !      Name      Type  Module   Description
 1045 !     ----------------------------------------------------------------
 
 
                                                         w3partmd.F90  page  20
 
 
 1046 !      STRACE    Sur.  W3SERVMD Subroutine tracing.
 1047 !      WAVNU1    Subr. W3DISPMD Wavenumber computation.
 1048 !     ----------------------------------------------------------------
 1049 !
 1050 ! 10. Source code :
 1051 !
 1052 !/ ------------------------------------------------------------------- /
 1053 !
 1054       USE CONSTANTS
 1055       USE W3DISPMD, ONLY: WAVNU1
 1056 !
 1057       USE W3GDATMD, ONLY: NK, NTH, NSPEC, DTH, SIG, DSII, DSIP,       &
 1058                           ECOS, ESIN, XFR, FACHFE, TH, FTE
 1059       USE W3ODATMD, ONLY: IAPROC, NAPERR, NDSE, NDST
 1060 !
 1061       IMPLICIT NONE
 1062 !/
 1063 !/ ------------------------------------------------------------------- /
 1064 !/ Parameter list
 1065 !/
 1066       INTEGER, INTENT(IN)     :: NPI, IMO(NSPEC), DIMXP
 1067       INTEGER, INTENT(OUT)    :: NPO, PMAP(DIMXP)
 1068       REAL, INTENT(IN)        :: ZP(NSPEC), DEPTH, UABS, UDIR, WN(NK)
 1069       REAL, INTENT(OUT)       :: XP(DIMP,0:DIMXP)
 1070 !/
 1071 !/ ------------------------------------------------------------------- /
 1072 !/ Local parameters
 1073 !/
 1074       INTEGER                 :: IK, ITH, ISP, IP, IFPMAX(0:NPI)
 1075       REAL                    :: SUMF(0:NK+1,0:NPI), SUMFW(NK,0:NPI), &
 1076                                  SUMFX(NK,0:NPI), SUMFY(NK,0:NPI),    &
 1077                                  SUME(0:NPI), SUMEW(0:NPI),           &
 1078                                  SUMEX(0:NPI), SUMEY(0:NPI),          &
 1079                                  EFPMAX(0:NPI), FCDIR(NTH)
 1080       REAL,DIMENSION(0:NPI)   :: SUME1, SUME2, SUMEM1, SUMQP
 1081       REAL                    :: HS, XL, XH, XL2, XH2, EL, EH, DENOM, &
 1082                                  SIGP, WNP, CGP, UPAR, C(NK), RD, FACT
 1083       REAL                    :: QP, M0, M1, M2, MM1, FSPRD, EPM_FP, ALP_>
 1084       REAL                    :: Y, YHAT, XHAT, SUMXY, SUMYLOGY, NUMER,&
 1085                                  SUMY, SUMXXY, SUMXYLOGY, SUMEXP, SUMEYP
 1086       REAL                    :: FTEII
 1087 !/
 1088 !
 1089 ! -------------------------------------------------------------------- /
 1090 ! 1.  Check on need of processing
 1091 !
 1092       NPO    = 0
 1093       XP     = 0.
 1094 !
 1095       IF ( NPI .EQ. 0 ) RETURN
 1096 !
 1097 ! -------------------------------------------------------------------- /
 1098 ! 2.  Initialize arrays
 1099 !
 1100       SUMF   = 0.
 
 
                                                         w3partmd.F90  page  21
 
 
 1101       SUMFW  = 0.
 1102       SUMFX  = 0.
 1103       SUMFY  = 0.
 1104       SUME   = 0.
 1105 !
 1106       SUME1  = 0.  !/ first spectral moment
 1107       SUME2  = 0.  !/ second spectral moment
 1108       SUMEM1 = 0.  !/ inverse spectral moment
 1109       SUMQP  = 0.  !/ peakedness parameter
 1110 !
 1111       SUMEW  = 0.
 1112       SUMEX  = 0.
 1113       SUMEY  = 0.
 1114       IFPMAX = 0
 1115       EFPMAX = 0.
 1116 !
 1117       DO IK=1, NK
 1118         C(IK)  = SIG(IK) / WN(IK)
 1119         END DO
 1120 !
 1121       DO ITH=1, NTH
 1122         UPAR   = WSMULT * UABS * MAX(0.,COS(TH(ITH)-DERA*UDIR))
 1123         IF ( UPAR .LT. C(NK) ) THEN
 1124             FCDIR(ITH) = SIG(NK+1)
 1125           ELSE
 1126             DO IK=NK-1, 2, -1
 1127               IF ( UPAR .LT. C(IK) ) EXIT
 1128               END DO
 1129             RD     = (C(IK)-UPAR) / (C(IK)-C(IK+1))
 1130             IF ( RD .LT. 0 ) THEN
 1131                 IK     = 0
 1132                 RD     = MAX ( 0., RD+1. )
 1133               END IF
 1134             FCDIR(ITH) = RD*SIG(IK+1) + (1.-RD)*SIG(IK)
 1135           END IF
 1136         END DO
 1137 !
 1138 ! -------------------------------------------------------------------- /
 1139 ! 3.  Spectral integrals and preps
 1140 ! 3.a Integrals
 1141 !     NOTE: Factor DTH only used in Hs computation.
 1142 !
 1143       DO IK=1, NK
 1144         DO ITH=1, NTH
 1145           ISP    = IK + (ITH-1)*NK
 1146           IP     = IMO(ISP)
 1147           FACT   = MAX ( 0. , MIN ( 1. ,                              &
 1148             1. - ( FCDIR(ITH) - 0.5*(SIG(IK-1)+SIG(IK)) ) / DSIP(IK) ) )
 1149           SUMF (IK, 0) = SUMF (IK, 0) + ZP(ISP)
 1150           SUMFW(IK, 0) = SUMFW(IK, 0) + ZP(ISP) * FACT
 1151           SUMFX(IK, 0) = SUMFX(IK, 0) + ZP(ISP) * ECOS(ITH)
 1152           SUMFY(IK, 0) = SUMFY(IK, 0) + ZP(ISP) * ESIN(ITH)
 1153           IF ( IP .EQ. 0 ) CYCLE
 1154           SUMF (IK,IP) = SUMF (IK,IP) + ZP(ISP)
 1155           SUMFW(IK,IP) = SUMFW(IK,IP) + ZP(ISP) * FACT
 
 
                                                         w3partmd.F90  page  22
 
 
 1156           SUMFX(IK,IP) = SUMFX(IK,IP) + ZP(ISP) * ECOS(ITH)
 1157           SUMFY(IK,IP) = SUMFY(IK,IP) + ZP(ISP) * ESIN(ITH)
 1158           END DO
 1159         END DO
 1160       SUMF(NK+1,:) = SUMF(NK,:) * FACHFE
 1161 !
 1162       DO IP=0, NPI
 1163         DO IK=1, NK
 1164           SUME (IP) = SUME (IP) + SUMF (IK,IP) * DSII(IK)
 1165           SUMQP(IP) = SUMQP(IP) + SUMF (IK,IP)**2 * DSII(IK) * SIG(IK)
 1166           SUME1(IP) = SUME1(IP) + SUMF (IK,IP) * DSII(IK) * SIG(IK)
 1167           SUME2(IP) = SUME2(IP) + SUMF (IK,IP) * DSII(IK) * SIG(IK)**2
 1168           SUMEM1(IP) = SUMEM1(IP) + SUMF (IK,IP) * DSII(IK) / SIG(IK)
 1169                                                                      
 1170           SUMEW(IP) = SUMEW(IP) + SUMFW(IK,IP) * DSII(IK)
 1171           SUMEX(IP) = SUMEX(IP) + SUMFX(IK,IP) * DSII(IK)
 1172           SUMEY(IP) = SUMEY(IP) + SUMFY(IK,IP) * DSII(IK)
 1173           IF ( SUMF(IK,IP) .GT. EFPMAX(IP) ) THEN
 1174               IFPMAX(IP) = IK
 1175               EFPMAX(IP) = SUMF(IK,IP)
 1176             END IF
 1177           END DO
 1178                 
 1179         !SUME (IP) = SUME (IP) + SUMF (NK,IP) * FTE
 1180         !SUME1(IP) = SUME1(IP) + SUMF (NK,IP) * FTE
 1181         !SUME2(IP) = SUME2(IP) + SUMF (NK,IP) * FTE
 1182         !SUMEM1(IP) = SUMEM1(IP) + SUMF (NK,IP) * FTE
 1183         !SUMQP(IP) = SUMQP(IP) + SUMF (NK,IP) * FTE
 1184         !SUMEW(IP) = SUMEW(IP) + SUMFW(NK,IP) * FTE
 1185         !SUMEX(IP) = SUMEX(IP) + SUMFX(NK,IP) * FTE
 1186         !SUMEY(IP) = SUMEY(IP) + SUMFY(NK,IP) * FTE
 1187         ! Met Office: Proposed bugfix for tail calculations, previously
 1188         !  PT1 and PT2 values were found to be too low when using the
 1189         !  FTE scaling factor for the tail. I think there are two issues:
 1190         !  1. energy spectrum is scaled in radian frequency space above b>
 1191         !     This needs to be consistent and FTE contains a DTH*SIG(NK)
 1192         !     factor that is not used in the DSII scaled calcs above
 1193         !  2. the tail fit calcs for period parameters needs to follow
 1194         !     the form used in w3iogomd and scaling should be
 1195         !     based on the relationship between FTE and FT1, FTTR etc.
 1196         !     as per w3iogomd and ww3_grid
 1197         FTEII = FTE / (DTH * SIG(NK))
 1198         SUME (IP) = SUME (IP) + SUMF (NK,IP) * FTEII
 1199         SUME1(IP) = SUME1(IP) + SUMF (NK,IP) * SIG(NK) * FTEII * (0.3333 >
 1200         SUME2(IP) = SUME2(IP) + SUMF (NK,IP) * SIG(NK)**2 * FTEII * (0.5 >
 1201         SUMEM1(IP) = SUMEM1(IP) + SUMF (NK,IP) / SIG(NK) * FTEII * (0.2 />
 1202         SUMQP(IP) = SUMQP(IP) + SUMF (NK,IP) * FTEII
 1203         SUMEW(IP) = SUMEW(IP) + SUMFW(NK,IP) * FTEII
 1204         SUMEX(IP) = SUMEX(IP) + SUMFX(NK,IP) * FTEII
 1205         SUMEY(IP) = SUMEY(IP) + SUMFY(NK,IP) * FTEII
 1206                                                     
 1207         END DO
 1208 !
 1209 ! -------------------------------------------------------------------- /
 1210 ! 4.  Compute pars
 
 
                                                         w3partmd.F90  page  23
 
 
 1211 !
 1212       NPO    = -1
 1213 !
 1214       DO IP=0, NPI
 1215 !
 1216         SUMEXP = 0.
 1217         SUMEYP = 0.
 1218 !
 1219         M0 = SUME(IP)  * DTH * TPIINV
 1220         HS     = 4. * SQRT ( MAX( M0 , 0. ) )
 1221         IF ( HS .LT. HSPMIN ) THEN
 1222           ! For wind cutoff and 2-band partitioning methods, keep the
 1223           ! partition, but set the integrated parameters to UNDEF
 1224           ! for Hs values less that HSPMIN:
 1225           IF( PTMETH .EQ. 4 .OR. PTMETH .EQ. 5 ) THEN
 1226              NPO = NPO + 1
 1227              XP(:,NPO) = UNDEF
 1228              XP(6,NPO) = 0.0 ! Set wind sea frac to zero
 1229           ENDIF
 1230           CYCLE
 1231         ENDIF
 1232 !
 1233         IF ( NPO .GE. DIMXP ) GOTO 2000
 1234         NPO = NPO + 1
 1235         IF (IP.GT.0)THEN
 1236            IF(NPO.LT.1)CYCLE
 1237            PMAP(NPO) = IP
 1238         ENDIF
 1239 !
 1240         M1 = SUME1(IP) * DTH * TPIINV**2
 1241         M2 = SUME2(IP) * DTH * TPIINV**3
 1242         MM1 = SUMEM1(IP) * DTH
 1243         QP = SUMQP(IP) *(DTH * TPIINV)**2
 1244 !       M1 = MAX( M1, 1.E-7 )
 1245 !       M2 = MAX( M2, 1.E-7 )
 1246 !
 1247         XL     = 1. / XFR - 1.
 1248         XH     = XFR - 1.
 1249         XL2    = XL**2
 1250         XH2    = XH**2
 1251         EL     = SUMF(IFPMAX(IP)-1,IP) - SUMF(IFPMAX(IP),IP)
 1252         EH     = SUMF(IFPMAX(IP)+1,IP) - SUMF(IFPMAX(IP),IP)
 1253         DENOM  = XL*EH - XH*EL
 1254         SIGP   = SIG(IFPMAX(IP))
 1255         IF (DENOM.NE.0.) THEN
 1256            SIGP   = SIGP *( 1. + 0.5 * ( XL2*EH - XH2*EL ) &
 1257                          / SIGN ( ABS(DENOM) , DENOM ) )
 1258         END IF
 1259         CALL WAVNU1 ( SIGP, DEPTH, WNP, CGP )
 1260 !
 1261      !/ --- Parabolic fit around the spectral peak ---
 1262         IK = IFPMAX(IP)
 1263         EFPMAX(IP) = SUMF(IK,IP) * DTH
 1264         IF (IK.GT.1 .AND. IK.LT.NK) THEN
 1265           EL    = SUMF(IK-1,IP) * DTH
 
 
                                                         w3partmd.F90  page  24
 
 
 1266           EH    = SUMF(IK+1,IP) * DTH
 1267           NUMER = 0.125 * ( EL - EH )**2
 1268           DENOM = EL - 2. * EFPMAX(IP) + EH
 1269           IF (DENOM.NE.0.) EFPMAX(IP) = EFPMAX(IP)         &
 1270                           - NUMER / SIGN( ABS(DENOM),DENOM )
 1271         END IF
 1272 !
 1273      !/ --- Weighted least-squares regression to estimate frequency
 1274      !/     spread (FSPRD) to an exponential function:
 1275      !/              E(f) = A * exp(-1/2*(f-fp)/B)**2             ,
 1276      !/     where B is frequency spread and  E(f) is used for
 1277      !/     weighting to avoid greater weights to smalll values
 1278      !/     in ordinary least-square fit. ---
 1279         FSPRD     = UNDEF
 1280         SUMY      = 0.
 1281         SUMXY     = 0.
 1282         SUMXXY    = 0.
 1283         SUMYLOGY  = 0.
 1284         SUMXYLOGY = 0.
 1285 !
 1286         DO IK=1, NK
 1287           Y = SUMF(IK,IP)*DTH
 1288         ! --- sums for weighted least-squares ---
 1289           IF (Y.GE.1.E-15) THEN
 1290             YHAT = LOG(Y)
 1291             XHAT = -0.5 * ( (SIG(IK)-SIGP)*TPIINV )**2
 1292             SUMY      = SUMY + Y
 1293             SUMXY     = SUMXY + XHAT * YHAT
 1294             SUMXXY    = SUMXXY + XHAT * XHAT * Y
 1295             SUMYLOGY  = SUMYLOGY + Y * YHAT
 1296             SUMXYLOGY = SUMXYLOGY + SUMXY * YHAT
 1297           END IF
 1298         END DO
 1299 !
 1300         NUMER = SUMY * SUMXXY - SUMXY**2
 1301         DENOM = SUMY * SUMXYLOGY - SUMXY * SUMYLOGY
 1302         IF (DENOM.NE.0.)  FSPRD = SQRT( NUMER / SIGN(ABS(DENOM),NUMER) )
 1303 !
 1304         SUMEXP = SUMFX(IFPMAX(IP),IP) * DSII(IFPMAX(IP))
 1305         SUMEYP = SUMFY(IFPMAX(IP),IP) * DSII(IFPMAX(IP))
 1306 !
 1307      !/ --- Significant wave height ---
 1308         XP(1,NPO) = HS
 1309      !/ --- Peak wave period ---
 1310         XP(2,NPO) = TPI / SIGP
 1311      !/ --- Peak wave length ---
 1312         XP(3,NPO) = TPI / WNP
 1313      !/ --- Mean wave direction ---
 1314         XP(4,NPO) = MOD( 630.-ATAN2(SUMEY(IP),SUMEX(IP))*RADE , 360. )
 1315      !/ --- Mean directional spread ---
 1316         XP(5,NPO) = RADE * SQRT ( MAX ( 0. , 2. * ( 1. - SQRT ( &
 1317                 MAX(0.,(SUMEX(IP)**2+SUMEY(IP)**2)/SUME(IP)**2) ) ) ) )
 1318      !/ --- Wind sea fraction ---
 1319         XP(6,NPO) = SUMEW(IP) / SUME(IP)
 1320      !/ --- Peak wave direction ---
 
 
                                                         w3partmd.F90  page  25
 
 
 1321         XP(7,NPO) =  MOD(630.-ATAN2(SUMEYP,SUMEXP)*RADE , 360.)
 1322      !/ --- Spectral width (Longuet-Higgins 1975) ---
 1323         XP(8,NPO) = SQRT( MAX( 1. , M2*M0 / M1**2 ) - 1. )
 1324      !/ --- JONSWAP peak enhancement parameter (E(fp)/EPM(fp))---
 1325      !  EPM_FMX = ALPHA_PM_FMX * GRAV**2 * TPI * SIGP**-5 * EXP(-5/4)
 1326         ALP_PM = 0.3125 * HS**2 * (SIGP)**4
 1327         EPM_FP = ALP_PM * TPI * (SIGP**(-5)) * 2.865048E-1
 1328         XP(9,NPO) = MAX( EFPMAX(IP) / EPM_FP , 1.0 )
 1329      !/ --- peakedness parameter (Goda 1970) ---
 1330         XP(10,NPO) = 2. * QP / M0**2
 1331      !/ --- gaussian frequency width ---
 1332         XP(11,NPO) = FSPRD
 1333      !/ --- wave energy period (inverse moment) ---
 1334         XP(12,NPO) = MM1 / M0
 1335      !/ --- mean wave period (first moment) ---
 1336         XP(13,NPO) = M0 / M1
 1337      !/ --- zero-upcrossing period (second moment) ---
 1338         XP(14,NPO) = SQRT( M0 / M2 )
 1339      !/ --- peak spectral density (one-dimensional) ---
 1340         XP(15,NPO) = EFPMAX(IP)
 1341 !
 1342         END DO
 1343 !
 1344       RETURN
 1345 !
 1346 ! Escape locations read errors --------------------------------------- *
 1347 !
 1348  2000 CONTINUE
 1349       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) NPO+1
 1350       RETURN
 1351 !
 1352 ! Formats
 1353 !
 1354  1000 FORMAT (/' *** WAVEWATCH III ERROR IN PTMEAN :'/                &
 1355                '     XP ARRAY TOO SMALL AT PARTITION',I6/)
 1356 !/
 1357 !/ End of PTMEAN ----------------------------------------------------- /
 1358 !/
 1359       END SUBROUTINE PTMEAN
 1360 !/
 1361 !/ End of module W3PARTMD -------------------------------------------- /
 1362 !/
 1363       END MODULE W3PARTMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
