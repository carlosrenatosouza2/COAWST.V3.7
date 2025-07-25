                                                         w3iosfmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3IOSFMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         25-Jul-2018 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    27-Jun-2006 : Origination.                        ( version 3.09 )
   13 !/    02-Nov-2006 : Origination W3CPRT and W3IOSF.      ( version 3.10 )
   14 !/    24-Mar-2007 : Add pars for entire spectrum.       ( version 3.11 )
   15 !/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
   16 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   17 !/    30-Oct-2009 : Fix unitialized dtsiz in w3iosf.    ( version 3.14 )
   18 !/                  (T. J. Campbell, NRL)
   19 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   20 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   21 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
   22 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   23 !/    06-Mar-2012 : Reparing test output under MPI.     ( version 4.07 )
   24 !/    08-Jun-2018 : use W3ADATMD, W3PARALL, INIT_GET_ISEA and
   25 !/                            INIT_GET_JSEA_ISPROC      ( version 6.04 )
   26 !/    25-Jul-2018 : Changed DIMXP size for partitioning ( version 6.05 )
   27 !/                  methods 4 and 5. (C Bunney, UKMO)
   28 !/
   29 !/    Copyright 2009-2012 National Weather Service (NWS),
   30 !/       National Oceanic and Atmospheric Administration.  All rights
   31 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   32 !/       No unauthorized use without permission.
   33 !/
   34 !  1. Purpose :
   35 !
   36 !     I/O and computational routines for the wave-field separation
   37 !     output.
   38 !
   39 !  2. Variables and types :
   40 !
   41 !      Name      Type  Scope    Description
   42 !     ----------------------------------------------------------------
   43 !      VERPRT    C*10  Private  Partition file version number.
   44 !      IDSTR     C*35  Private  Partition file ID string.
   45 !     ----------------------------------------------------------------
   46 !
   47 !  3. Subroutines and functions :
   48 !
   49 !      Name      Type  Scope    Description
   50 !     ----------------------------------------------------------------
   51 !      W3CPRT    Subr. Public   Partition all requested local spectra.
   52 !      W3IOSF    Subr. Public   Processing and output of partitioned
   53 !                               wave data.
   54 !     ----------------------------------------------------------------
   55 !
 
 
                                                         w3iosfmd.F90  page   2
 
 
   56 !  4. Subroutines and functions used :
   57 !
   58 !      Name      Type  Module   Description
   59 !     ----------------------------------------------------------------
   60 !      W3PART    Subr. W3PARTMD Spectral partition for single spectrum.
   61 !      STRACE    Sur.  W3SERVMD Subroutine tracing.
   62 !      EXTCDE    Subr.   Id.    Program abort.
   63 !      MPI_SEND, MPI_RECV
   64 !                               MPI send and recieve routines
   65 !     ----------------------------------------------------------------
   66 !
   67 !  5. Remarks :
   68 !
   69 !  6. Switches :
   70 !
   71 !     !/S    Enable subroutine tracing.
   72 !     !/T    Enable test output
   73 !
   74 !  7. Source code :
   75 !
   76 !/ ------------------------------------------------------------------- /
   77       PUBLIC
   78 !/
   79 !/ Private parameter statements (ID strings)
   80 !/
   81       CHARACTER(LEN=10), PARAMETER, PRIVATE :: VERPRT = '2018-07-25'
   82       CHARACTER(LEN=35), PARAMETER, PRIVATE ::                        &
   83                          IDSTR = 'WAVEWATCH III PARTITIONED DATA FILE'
   84 !/
   85       CONTAINS
   86 !/ ------------------------------------------------------------------- /
   87       SUBROUTINE W3CPRT ( IMOD )
   88 !/
   89 !/                  +-----------------------------------+
   90 !/                  | WAVEWATCH III           NOAA/NCEP |
   91 !/                  |           H. L. Tolman            |
   92 !/                  |                        FORTRAN 90 |
   93 !/                  | Last update :         25-Jul-2018 !
   94 !/                  +-----------------------------------+
   95 !/
   96 !/    30-Oct-2006 : Origination.                        ( version 3.10 )
   97 !/    24-Mar-2007 : Add pars for entire spectrum.       ( version 3.11 )
   98 !/    25-Jul-2018 : Changed DIMXP size for partitioning ( version 6.05 )
   99 !/                  methods 4 and 5. (C Bunney, UKMO)
  100 !/
  101 !  1. Purpose :
  102 !
  103 !     Partitioning of spectra into fields for all grid points that
  104 !     are locally stored.
  105 !
  106 !  2. Method :
  107 !
  108 !  3. Parameters :
  109 !
  110 !     Parameter list
 
 
                                                         w3iosfmd.F90  page   3
 
 
  111 !     ----------------------------------------------------------------
  112 !       IMOD    Int.   I   Grid number.
  113 !     ----------------------------------------------------------------
  114 !
  115 !  4. Subroutines used :
  116 !
  117 !      Name      Type  Module   Description
  118 !     ----------------------------------------------------------------
  119 !      W3PART    Subr. W3PARTMD Spectral partition for single spectrum.
  120 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  121 !     ----------------------------------------------------------------
  122 !
  123 !  5. Called by :
  124 !
  125 !      Name      Type  Module   Description
  126 !     ----------------------------------------------------------------
  127 !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
  128 !     ----------------------------------------------------------------
  129 !
  130 !  6. Error messages :
  131 !
  132 !  7. Remarks :
  133 !
  134 !     - Although a sparse (IX,IY) grid is looked for, th major loop
  135 !       is still over NSEAL to simplify storage.
  136 !
  137 !  8. Structure :
  138 !
  139 !  9. Switches :
  140 !
  141 !     !/S    Enable subroutine tracing.
  142 !     !/T    Enable test output
  143 !
  144 ! 10. Source code :
  145 !
  146 !/ ------------------------------------------------------------------- /
  147 !/
  148       USE CONSTANTS
  149 !
  150       USE W3PARTMD, ONLY: W3PART
  151 !
  152       USE W3GDATMD, ONLY: NSEA, NSEAL, MAPSF, MAPSTA, NK, NTH, SIG
  153       USE W3ADATMD, ONLY: WN, CG, U10, U10D, DW
  154       USE W3ODATMD, ONLY: IAPROC, NAPROC, OUTPTS, O6INIT,       &
  155                           ICPRT, DTPRT, DIMP, PTMETH
  156       USE W3WDATMD, ONLY: VA, ASF
  157       USE W3ADATMD, ONLY: NSEALM
  158       USE W3PARALL, ONLY: INIT_GET_ISEA, INIT_GET_JSEA_ISPROC
  159 !
  160       IMPLICIT NONE
  161 !/
  162 !/ ------------------------------------------------------------------- /
  163 !/ Parameter list
  164 !/
  165       INTEGER, INTENT(IN)     :: IMOD
 
 
                                                         w3iosfmd.F90  page   4
 
 
  166 !/
  167 !/ ------------------------------------------------------------------- /
  168 !/ Local parameters
  169 !/
  170       INTEGER                 :: DIMXP, JSEA, ISEA, IX, IY,     &
  171                                  IK, ITH, NP, TMPSIZ, OLDSIZ, FINSIZ
  172       INTEGER, SAVE           :: TSFAC = 7
  173       REAL                    :: UABS, UDIR, DEPTH, FACT, E2(NK,NTH)
  174       REAL, ALLOCATABLE       :: XP(:,:), TMP(:,:), TMP2(:,:)
  175 !/
  176 !
  177 ! -------------------------------------------------------------------- /
  178 ! 0.  Initializations
  179 !
  180       IF(PTMETH .EQ. 4 .OR. PTMETH .EQ. 5) THEN
  181         ! Partitioning methods 4 and 5 only ever create 2 partitions
  182         ! C. Bunney, 25-Jul-18
  183         DIMXP = 2
  184       ELSE
  185         DIMXP  = ((NK+1)/2) * ((NTH-1)/2)
  186       ENDIF
  187            
  188       ALLOCATE ( XP(DIMP,0:DIMXP) )
  189 !
  190       IF ( O6INIT ) THEN
  191           DEALLOCATE ( OUTPTS(IMOD)%OUT6%DTPRT )
  192         ELSE
  193           ALLOCATE ( OUTPTS(IMOD)%OUT6%ICPRT(NSEALM+1,2) )
  194           ICPRT => OUTPTS(IMOD)%OUT6%ICPRT
  195           O6INIT = .TRUE.
  196         END IF
  197       ICPRT  = 0
  198       ICPRT(1,2) = 1
  199 !
  200       TMPSIZ = TSFAC * NSEAL
  201       ALLOCATE ( TMP(DIMP,TMPSIZ) )
  202 !
  203 ! -------------------------------------------------------------------- /
  204 ! 1.  Loop over sea points
  205 !
  206       DO JSEA=1, NSEAL
  207 !
  208 ! -------------------------------------------------------------------- /
  209 ! 2.  Check need for processing
  210 !
  211         CALL INIT_GET_ISEA(ISEA, JSEA)
  212         IX     = MAPSF(ISEA,1)
  213         IY     = MAPSF(ISEA,2)
  214         ICPRT(JSEA+1,2) = ICPRT(JSEA,2)
  215 !
  216         IF ( MAPSTA(IY,IX) .LT. 0 ) CYCLE
  217 !
  218 ! -------------------------------------------------------------------- /
  219 ! 3.  Prepare for partitioning
  220 !
 
 
                                                         w3iosfmd.F90  page   5
 
 
  221         UABS   = U10(ISEA)*ASF(ISEA)
  222         UDIR   = U10D(ISEA)*RADE
  223         DEPTH  = DW(ISEA)
  224 !
  225         DO IK=1, NK
  226           FACT   = TPI * SIG(IK) / CG(IK,ISEA)
  227           DO ITH=1, NTH
  228             E2(IK,ITH) = VA(ITH+(IK-1)*NTH,JSEA) * FACT
  229             END DO
  230           END DO
  231 !
  232 ! -------------------------------------------------------------------- /
  233 ! 4.  perform partitioning
  234 !
  235 !AR: NaN checks should results in immediate stop after trace ...
  236         IF (DEPTH.NE.DEPTH) THEN
  237           WRITE(6,*) 'IOSF:',ISEA,IX,IY,DW(ISEA),DEPTH
  238           WRITE(*,*) 'FOUND NaN in depth'
  239           STOP 'CRITICAL ERROR IN DEPTH ARRAY'
  240         END IF
  241         CALL W3PART ( E2, UABS, UDIR, DEPTH, WN(1:NK,ISEA),           &
  242                       NP, XP, DIMXP )
  243 !
  244 ! -------------------------------------------------------------------- /
  245 ! 5.  Store results (temp)
  246 !
  247         IF ( NP .GE. 0 ) THEN
  248             ICPRT( JSEA ,1) = NP + 1
  249             ICPRT(JSEA+1,2) = ICPRT(JSEA,2) + NP + 1
  250 !
  251             IF ( ICPRT(JSEA,2)+NP .GT. TMPSIZ ) THEN
  252                 ALLOCATE ( TMP2(DIMP,TMPSIZ) )
  253                 TMP2   = TMP
  254                 DEALLOCATE ( TMP )
  255                 OLDSIZ = TMPSIZ
  256                 TMPSIZ = TMPSIZ + MAX ( TSFAC*NSEAL , DIMXP )
  257                 ALLOCATE ( TMP(DIMP,TMPSIZ) )
  258                 TMP(:,1:OLDSIZ) = TMP2(:,1:OLDSIZ)
  259                 TMP(:,OLDSIZ+1:) = 0.
  260                 DEALLOCATE ( TMP2 )
  261               END IF
  262 !
  263            TMP(:,ICPRT(JSEA,2):ICPRT(JSEA,2)+NP) = XP(:,0:NP)
  264 !
  265          END IF
  266 !
  267 ! -------------------------------------------------------------------- /
  268 ! 6.  End of loop and clean up
  269 !
  270         END DO
  271 !
  272       FINSIZ = ICPRT(NSEAL+1,2) - 1
  273 !
  274       ALLOCATE ( OUTPTS(IMOD)%OUT6%DTPRT(DIMP,MAX(1,FINSIZ)) )
  275       DTPRT => OUTPTS(IMOD)%OUT6%DTPRT
 
 
                                                         w3iosfmd.F90  page   6
 
 
  276       IF ( FINSIZ .GT. 0 ) THEN
  277           DTPRT = TMP(:,1:FINSIZ)
  278         ELSE
  279           DTPRT = 0.
  280         END IF
  281 !
  282       DEALLOCATE ( XP, TMP )
  283 !
  284       RETURN
  285 !
  286 ! Formats
  287 !
  288 !/
  289 !/ End of W3CPRT ----------------------------------------------------- /
  290 !/
  291       END SUBROUTINE W3CPRT
  292                            
  293 !/ ------------------------------------------------------------------- /
  294       SUBROUTINE W3IOSF ( NDSPT, IMOD )
  295 !/
  296 !/                  +-----------------------------------+
  297 !/                  | WAVEWATCH III           NOAA/NCEP |
  298 !/                  |           H. L. Tolman            |
  299 !/                  |                        FORTRAN 90 |
  300 !/                  | Last update :         30-Oct-2009 |
  301 !/                  +-----------------------------------+
  302 !/
  303 !/    02-Nov-2006 : Origination.                        ( version 1.10 )
  304 !/    24-Mar-2007 : Add pars for entire spectrum.       ( version 3.11 )
  305 !/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
  306 !/    30-Oct-2009 : Fix unitialized dtsiz error.        ( version 3.14 )
  307 !/                  (T. J. Campbell, NRL)
  308 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
  309 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  310 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
  311 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  312 !/
  313 !  1. Purpose :
  314 !
  315 !     Write partitioned spectrakl data to file. Unlike other
  316 !     WAVEWATCH III IO routines, this one writes only.
  317 !     First ad-hoc version.
  318 !
  319 !  2. Method :
  320 !
  321 !     Writing to formatted or unformatted file with ID headers.
  322 !
  323 !  3. Parameters :
  324 !
  325 !     Parameter list
  326 !     ----------------------------------------------------------------
  327 !       NDSPT   Int.   I   Unit number.
  328 !       IMOD    Int.   I   Grid number.
  329 !     ----------------------------------------------------------------
  330 !
 
 
                                                         w3iosfmd.F90  page   7
 
 
  331 !  4. Subroutines used :
  332 !
  333 !      Name      Type  Module   Description
  334 !     ----------------------------------------------------------------
  335 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  336 !      EXTCDE    Subr.   Id.    Program abort.
  337 !      MPI_SEND, MPI_RECV
  338 !                               MPI send and recieve routines
  339 !     ----------------------------------------------------------------
  340 !
  341 !  5. Called by :
  342 !
  343 !      Name      Type  Module   Description
  344 !     ----------------------------------------------------------------
  345 !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
  346 !     ----------------------------------------------------------------
  347 !
  348 !  6. Error messages :
  349 !
  350 !  7. Remarks :
  351 !
  352 !  8. Structure :
  353 !
  354 !  9. Switches :
  355 !
  356 !     !/S    Enable subroutine tracing.
  357 !     !/T    Enable test output
  358 !
  359 ! 10. Source code :
  360 !
  361 !/ ------------------------------------------------------------------- /
  362 !/
  363       USE CONSTANTS
  364       USE W3SERVMD, ONLY: EXTCDE
  365 !
  366       USE W3GDATMD, ONLY: FILEXT, NSEA, XGRD, YGRD, MAPSF, FLAGLL
  367       USE W3GDATMD, ONLY: NSEAL
  368       USE W3WDATMD, ONLY: TIME, ASF
  369       USE W3ODATMD, ONLY: NDSE, IAPROC, NAPROC, NAPPRT, NAPERR, &
  370                           IPASS => IPASS6, FLFORM, FNMPRE, OUTPTS,    &
  371                           IX0, IXN, IXS, IY0, IYN, IYS, DIMP
  372       USE W3ADATMD, ONLY: DW, U10, U10D, CX, CY
  373       USE W3ADATMD, ONLY: NSEALM
  374       USE W3PARALL, ONLY: INIT_GET_JSEA_ISPROC
  375       USE W3ADATMD, ONLY: MPI_COMM_WAVE
  376       USE W3ODATMD, ONLY: ICPRT, DTPRT, IT0PRT
  377 !
  378       IMPLICIT NONE
  379 !
  380       INCLUDE "mpif.h"
  381 !/
  382 !/ ------------------------------------------------------------------- /
  383 !/ Parameter list
  384 !/
  385       INTEGER, INTENT(IN)     :: NDSPT, IMOD
 
 
                                                         w3iosfmd.F90  page   8
 
 
  386 !/
  387 !/ ------------------------------------------------------------------- /
  388 !/ Local parameters
  389 !/
  390       INTEGER                 :: I, J, IERR, ISEA, JSEA, JAPROC,      &
  391                                  IX, IY, IP, IOFF, DTSIZ=0
  392       INTEGER                 :: ICSIZ, IERR_MPI, IT,            &
  393                                  STATUS(MPI_STATUS_SIZE,1), JSLM
  394       INTEGER, POINTER        :: ICP(:,:)
  395       REAL                    :: X, Y, DEPTH, UABS, UDIR, CABS, CDIR
  396       REAL, POINTER           :: DTP(:,:)
  397 !
  398       TYPE PROCS
  399         INTEGER, POINTER      :: ICPRT(:,:)
  400         REAL, POINTER         :: DTPRT(:,:)
  401       END TYPE PROCS
  402 !
  403       TYPE(PROCS), TARGET, ALLOCATABLE :: PROC(:)
  404 !
  405 ! -------------------------------------------------------------------- /
  406 ! 0.  Initializations
  407 !
  408       IPASS  = IPASS + 1
  409       ICSIZ  = 2 * ( NSEALM + 1 )
  410 !
  411 ! -------------------------------------------------------------------- /
  412 ! 1.  Set up file ( IPASS = 1 and proper processor )
  413 !
  414       IF ( IPASS.EQ.1 .AND. IAPROC.EQ.NAPPRT ) THEN
  415 !
  416 ! 1.a Open file
  417 !
  418           I      = LEN_TRIM(FILEXT)
  419           J      = LEN_TRIM(FNMPRE)
  420 !
  421           IF ( FLFORM ) THEN
  422               OPEN (NDSPT,FILE=FNMPRE(:J)//'partition.'//FILEXT(:I),   &
  423                     ERR=800,IOSTAT=IERR)
  424             ELSE
  425               OPEN (NDSPT,FILE=FNMPRE(:J)//'partition.'//FILEXT(:I),   &
  426                     FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
  427             END IF
  428 !
  429           REWIND (NDSPT)
  430 !
  431 ! 1.b Header info
  432 !
  433           IF ( FLFORM ) THEN
  434               WRITE (NDSPT,910) IDSTR, VERPRT
  435               IF ( FLAGLL ) THEN
  436                   WRITE (NDSPT,911) ' yyyymmdd hhmmss     '//         &
  437                                     'lat     lon   name       nprt'// &
  438                                     ' depth ubas  udir cabs  cdir'
  439                 ELSE
  440                   WRITE (NDSPT,911) ' yyyymmdd hhmmss     '//         &
 
 
                                                         w3iosfmd.F90  page   9
 
 
  441                                     'X       Y     name       nprt'// &
  442                                     ' depth ubas  udir cabs  cdir'
  443                 END IF
  444               WRITE (NDSPT,911) '        hs     tp     lp  '//    &
  445                                 '     theta     sp      wf'
  446             ELSE
  447               WRITE (  NDSPT  ) IDSTR, VERPRT
  448               IF ( FLAGLL ) THEN
  449                   WRITE (  NDSPT  ) ' yyyymmdd hhmmss     '//         &
  450                                     'lat     lon   name       nprt'// &
  451                                     ' depth ubas  udir cabs  cdir'
  452                 ELSE
  453                   WRITE (  NDSPT  ) ' yyyymmdd hhmmss     '//         &
  454                                     'X       Y     name       nprt'// &
  455                                     ' depth ubas  udir cabs  cdir'
  456                 END IF
  457               WRITE (  NDSPT  ) '        hs     tp     lp  '//    &
  458                                 '     theta     sp      wf'
  459             END IF
  460 !
  461         END IF
  462 !
  463 ! -------------------------------------------------------------------- /
  464 ! 2.  Send data if output is non-local ( MPI only )
  465 !     Leave routine after send
  466 !
  467       IF ( IAPROC.NE.NAPPRT .AND. IAPROC.LE.NAPROC ) THEN
  468 !
  469           IT     = IT0PRT + IAPROC - 1
  470           CALL MPI_SEND ( ICPRT, ICSIZ, MPI_REAL, NAPPRT-1, IT,  &
  471                           MPI_COMM_WAVE, IERR_MPI )
  472           DTSIZ  = ICPRT(NSEAL+1,2) - 1
  473 !
  474           IT     = IT0PRT + NAPROC + IAPROC - 1
  475           IF ( DTSIZ .GT. 0 ) CALL MPI_SEND                      &
  476                         ( DTPRT, 6*DTSIZ, MPI_REAL, NAPPRT-1,    &
  477                           IT, MPI_COMM_WAVE, IERR_MPI )
  478 !
  479         END IF
  480 !
  481       IF ( IAPROC .NE. NAPPRT ) RETURN
  482 !
  483 ! -------------------------------------------------------------------- /
  484 ! 3.  Point to and/or gather data
  485 ! 3.a Set up storage
  486 !
  487       ALLOCATE ( PROC(NAPROC) )
  488 !
  489 ! 3.b Point to local data
  490 !
  491       IF ( IAPROC .LE. NAPROC ) THEN
  492           PROC(IAPROC)%ICPRT => OUTPTS(IMOD)%OUT6%ICPRT
  493           PROC(IAPROC)%DTPRT => OUTPTS(IMOD)%OUT6%DTPRT
  494         END IF
  495 !
 
 
                                                         w3iosfmd.F90  page  10
 
 
  496 ! 3.c Allocate and get counters and arrrays
  497 !
  498       DO JAPROC=1, NAPROC
  499         IF ( IAPROC .EQ. JAPROC ) CYCLE
  500 !
  501             ALLOCATE ( PROC(JAPROC)%ICPRT(NSEALM+1,2) )
  502             ICP   => PROC(JAPROC)%ICPRT
  503             IT     = IT0PRT + JAPROC - 1
  504             CALL MPI_RECV ( ICP, ICSIZ, MPI_REAL, JAPROC-1, IT,  &
  505                             MPI_COMM_WAVE, STATUS, IERR_MPI )
  506             JSLM   = 1 + (NSEA-JAPROC)/NAPROC
  507             DTSIZ  = ICP(JSLM+1,2) - 1
  508 !
  509             ALLOCATE ( PROC(JAPROC)%DTPRT(DIMP,MAX(1,DTSIZ)) )
  510             DTP   => PROC(JAPROC)%DTPRT
  511             IT     = IT0PRT + NAPROC + JAPROC - 1
  512             IF ( DTSIZ .GT. 0 ) CALL MPI_RECV                    &
  513                           ( DTP, DIMP*DTSIZ, MPI_REAL, JAPROC-1, &
  514                             IT, MPI_COMM_WAVE, STATUS, IERR_MPI )
  515 !
  516         END DO
  517 !
  518 ! -------------------------------------------------------------------- /
  519 ! 4.  Write all data for which partitions are found
  520 ! 4.a General loop over all sea points
  521 !
  522       DO ISEA=1, NSEA
  523 !
  524 ! 4.b Check for partitioned data at sea point
  525 !
  526         CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, JAPROC)
  527 !
  528         ICP   => PROC(JAPROC)%ICPRT
  529         DTP   => PROC(JAPROC)%DTPRT
  530 !
  531         IF ( ICP(JSEA,1) .EQ. 0 ) CYCLE
  532 !
  533 ! 4.c Process point ID line
  534 !
  535         IX     = MAPSF(ISEA,1)
  536         IY     = MAPSF(ISEA,2)
  537         IF ( IX.LT.IX0 .OR. IX.GT.IXN .OR. MOD(IX-IX0,IXS).NE.0 ) CYCLE
  538         IF ( IY.LT.IY0 .OR. IY.GT.IYN .OR. MOD(IY-IY0,IYS).NE.0 ) CYCLE
  539         X      = XGRD(IY,IX)
  540         Y      = YGRD(IY,IX)
  541         DEPTH   = DW(ISEA)
  542         UABS   = U10(ISEA)*ASF(ISEA)
  543         UDIR   = MOD ( 270. - U10D(ISEA)*RADE , 360. )
  544         CABS   = SQRT ( CX(ISEA)**2 + CY(ISEA)**2 )
  545         IF ( CABS .LT. 1.E-3 ) THEN
  546             CDIR   = 0.
  547           ELSE
  548             CDIR   = ATAN2 ( CY(ISEA), CX(ISEA) ) * RADE
  549             CDIR   = MOD ( 270. - CDIR , 360. )
  550           END IF
 
 
                                                         w3iosfmd.F90  page  11
 
 
  551 !
  552         IF ( FLFORM ) THEN
  553             IF ( FLAGLL ) THEN
  554                 WRITE (NDSPT,940) TIME, Y, X,                        &
  555                                  'grid_point', ICP(JSEA,1) - 1,      &
  556                                   DEPTH, UABS, UDIR, CABS, CDIR
  557               ELSE
  558                 WRITE (NDSPT,941) TIME, X*1.E-3, Y*1.E-3,            &
  559                                  'grid_point', ICP(JSEA,1) - 1,      &
  560                                   DEPTH, UABS, UDIR, CABS, CDIR
  561               END IF
  562           ELSE
  563             IF ( FLAGLL ) THEN
  564                 WRITE (  NDSPT  ) TIME, Y, X,                        &
  565                                  'grid_point', ICP(JSEA,1) - 1,      &
  566                                   DEPTH, UABS, UDIR, CABS, CDIR
  567               ELSE
  568                 WRITE (  NDSPT  ) TIME, X*1.E-3, Y*1.E-3,            &
  569                                  'grid_point', ICP(JSEA,1) - 1,      &
  570                                   DEPTH, UABS, UDIR, CABS, CDIR
  571               END IF
  572           END IF
  573 !
  574 ! 4.d Process partitions for this point
  575 !
  576         IOFF   = ICP(JSEA,2)
  577 !
  578         IF ( FLFORM ) THEN
  579             DO IP=0, ICP(JSEA,1) - 1
  580               WRITE (NDSPT,942) IP, DTP(:,IP+IOFF)
  581               END DO
  582           ELSE
  583             DO IP=0, ICP(JSEA,1) - 1
  584               WRITE (  NDSPT  ) IP, DTP(:,IP+IOFF)
  585               END DO
  586           END IF
  587 !
  588         END DO
  589 !
  590 ! -------------------------------------------------------------------- /
  591 ! 5.  Clean up data structure
  592 !
  593       DO JAPROC=1, NAPROC
  594         IF ( IAPROC .EQ. JAPROC ) CYCLE
  595         DEALLOCATE ( PROC(JAPROC)%ICPRT, PROC(JAPROC)%DTPRT )
  596         END DO
  597 !
  598       DEALLOCATE ( PROC )
  599 !
  600       RETURN
  601 !
  602 ! Escape locations read errors --------------------------------------- *
  603 !
  604   800 CONTINUE
  605       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) IERR
 
 
                                                         w3iosfmd.F90  page  12
 
 
  606       CALL EXTCDE ( 1 )
  607 !
  608 ! Formats
  609 !
  610   910 FORMAT (A,1X,A)
  611   911 FORMAT (A)
  612 !
  613   940 FORMAT (1X,I8.8,1X,I6.6,2F8.3,2X,'''',A10,'''',            &
  614               1X,I2,F7.1,F5.1,f6.1,F5.2,F6.1)
  615   941 FORMAT (1X,I8.8,1X,I6.6,2(F8.1,'E3'),2X,'''',A10,'''',     &
  616               1X,I2,F7.1,F5.1,f6.1,F5.2,F6.1)
  617   942 FORMAT (I3,3F8.2,2F9.2,F7.2)
  618 !
  619  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOSF : '/               &
  620                '     ERROR IN OPENING FILE'/                          &
  621                '     IOSTAT =',I5/)
  622 !
  623 !/
  624 !/ End of W3IOSF ----------------------------------------------------- /
  625 !/
  626       END SUBROUTINE W3IOSF
  627 !/
  628 !/ End of module W3IOSFMD -------------------------------------------- /
  629 !/
  630       END MODULE W3IOSFMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
