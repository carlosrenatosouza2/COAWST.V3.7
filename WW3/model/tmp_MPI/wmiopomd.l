                                                         wmiopomd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE WMIOPOMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         06-Jun-2012 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    09-Aug-2006 : Origination.                        ( version 3.10 )
   13 !/    01-May-2007 : Addd diagnostic output O7a/b.       ( version 3.11 )
   14 !/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
   15 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   16 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   17 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   18 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
   19 !/                  specify index closure for a grid.   ( version 3.14 )
   20 !/                  (T. J. Campbell, NRL)
   21 !/    06-Mar-2012 : Using MPI_COMM_NULL in checks.      ( version 4.07 )
   22 !/    06-Jun-2012 : Porting bugfixes from 3.14 to 4.07  ( version 4.07 )
   23 !/
   24 !/    Copyright 2009-2012 National Weather Service (NWS),
   25 !/       National Oceanic and Atmospheric Administration.  All rights
   26 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   27 !/       No unauthorized use without permission.
   28 !/
   29 !  1. Purpose :
   30 !
   31 !     Module for generating a single point output file for a multi-
   32 !     grid model implementation.
   33 !
   34 !  2. Variables and types :
   35 !
   36 !  3. Subroutines and functions :
   37 !
   38 !      Name      Type  Scope    Description
   39 !     ----------------------------------------------------------------
   40 !      WMIOPP    Subr  Public   Initialization routine.
   41 !      WMIOPO    Subr  Public   Gather and write routine.
   42 !     ----------------------------------------------------------------
   43 !
   44 !  4. Subroutines and functions used :
   45 !
   46 !      Name      Type  Module   Description
   47 !     ----------------------------------------------------------------
   48 !      W3SETG    Subr  W3GDATMD Point to model grid.
   49 !      W3SETW    Subr  W3WDATMD Point to model grid.
   50 !      W3SETA    Subr  W3ADATMD Point to model grid.
   51 !      W3SETO    Subr  W3ODATMD Point to model grid.
   52 !      W3DMO2    Subr     Id.   Dimention model grids output 2.
   53 !      WMSETM    Subr  WMMDATMD Point to model grid.
   54 !      W3MPIP    Subr  W3INITMD Model intiailization.
   55 !      W3IOPP    Sunr  W3IOPOMD Prepare point output for single model.
 
 
                                                         wmiopomd.F90  page   2
 
 
   56 !      W3IOPO    Sunr     Id.   Point output for single model.
   57 !      W3CSPC    Subr. W3CSPCMD Spectral grid conversion.
   58 !      STRACE    Subr  W3SERVMD Subroutine tracing.
   59 !      EXTCDE    Subr     Id.   Program abort.
   60 !      MPI_SEND, MPI_RECV
   61 !                Subr.  mpif.h  Standard MPI library routines.
   62 !     ----------------------------------------------------------------
   63 !
   64 !  5. Remarks :
   65 !
   66 !  6. Switches :
   67 !
   68 !       !/SHRD Distributed memory model.
   69 !       !/MPI
   70 !
   71 !       !O7a   Disgnostic output to NMPSCR.
   72 !       !O7b
   73 !
   74 !       !/S    Enable subroutine tracing.
   75 !       !/T    Enable test output
   76 !       !/MPIT
   77 !
   78 !  7. Source code :
   79 !
   80 !/ ------------------------------------------------------------------- /
   81       PUBLIC
   82 !/
   83       CONTAINS
   84 !/ ------------------------------------------------------------------- /
   85       SUBROUTINE WMIOPP ( NPT, XPT, YPT, PNAMES )
   86 !/
   87 !/                  +-----------------------------------+
   88 !/                  | WAVEWATCH III           NOAA/NCEP |
   89 !/                  |           H. L. Tolman            |
   90 !/                  |                        FORTRAN 90 |
   91 !/                  | Last update :         01-Sep-2012 !
   92 !/                  +-----------------------------------+
   93 !/
   94 !/    09-Aug-2006 : Origination.                        ( version 3.10 )
   95 !/    01-May-2007 : Addd diagnostic output O7a,b        ( version 3.11 )
   96 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   97 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   98 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
   99 !/                  specify index closure for a grid.   ( version 3.14 )
  100 !/                  (T. J. Campbell, NRL)
  101 !/    16-Mar-2012 : Using MPI_COMM_NULL in checks.      ( version 4.07 )
  102 !/    06-Jun-2012 : Porting bugfixes from 3.14 to 4.07  ( version 4.07 )
  103 !/    01-Sep-2012 : Added tests for unstructured grid   ( version 4.07 )
  104 !/                  (M. Dutour Sikiric, IRB & Aron Roland, Z&P)
  105 !/
  106 !  1. Purpose :
  107 !
  108 !     Initialization for unified point output.
  109 !
  110 !  2. Method :
 
 
                                                         wmiopomd.F90  page   3
 
 
  111 !
  112 !     Find highest resolution grid for each point.
  113 !
  114 !  3. Parameters :
  115 !
  116 !     Parameter list
  117 !     ----------------------------------------------------------------
  118 !       NPT     Int.   I   Number of output points in input.
  119 !       XPT     R.A.   I   X (longitude) coordinates of output points.
  120 !       YPT     R.A.   I   Id. Y.
  121 !       PNAMES  C*40   I   Names of output points.
  122 !     ----------------------------------------------------------------
  123 !       Note: all are optional, and should be given on the first call
  124 !             only, will be taken from storage after that.
  125 !             NPT needs to be ginve always, but can be dummy after
  126 !             first call.
  127 !
  128 !  4. Subroutines used :
  129 !
  130 !      Name      Type  Module   Description
  131 !     ----------------------------------------------------------------
  132 !      W3SETG    Subr  W3GDATMD Point to model grid.
  133 !      W3SETW    Subr  W3WDATMD Point to model grid.
  134 !      W3SETA    Subr  W3ADATMD Point to model grid.
  135 !      W3SETO    Subr  W3ODATMD Point to model grid.
  136 !      W3DMO2    Subr     Id.   Dimension model grids output 2.
  137 !      WMSETM    Subr  WMMDATMD Point to model grid.
  138 !      W3MPIP    Subr  W3INITMD Model intiailization.
  139 !      W3IOPP    Sunr  W3IOPOMD Point output for single model.
  140 !      STRACE    Subr  W3SERVMD Subroutine tracing.
  141 !      EXTCDE    Subr     Id.   Program abort.
  142 !     ----------------------------------------------------------------
  143 !
  144 !  5. Called by :
  145 !
  146 !      Name      Type  Module   Description
  147 !     ----------------------------------------------------------------
  148 !      WMINIT    Subr. WMINITMD Wave model initialization routine.
  149 !     ----------------------------------------------------------------
  150 !
  151 !  6. Error messages :
  152 !
  153 !  7. Remarks :
  154 !
  155 !     - The algorithm used to decide if the pont is in the grid needs
  156 !       to be strictly consistent with W3IOPP.
  157 !     - MPI communication is set up separately from W3MPIO to assure
  158 !       that data are gathered in a single processor even if this
  159 !       procesor is not part of the communicator of the individual
  160 !       model.
  161 !     - In section 2.b the soring of the grids by rand is utilized.
  162 !
  163 !  8. Structure :
  164 !
  165 !     See source code.
 
 
                                                         wmiopomd.F90  page   4
 
 
  166 !
  167 !  9. Switches :
  168 !
  169 !       !/SHRD Distributed memory model.
  170 !       !/MPI
  171 !
  172 !       !O7a   Disgnostic output to NMPSCR.
  173 !       !O7b
  174 !
  175 !       !/S    Enable subroutine tracing.
  176 !       !/T    Enable test output
  177 !
  178 ! 10. Source code :
  179 !
  180 !/ ------------------------------------------------------------------- /
  181 !
  182       USE W3GSRUMD
  183       USE W3GDATMD, ONLY: W3SETG
  184       USE W3ADATMD, ONLY: W3SETA
  185       USE W3WDATMD, ONLY: W3SETW
  186       USE W3ODATMD, ONLY: W3SETO, W3DMO2
  187       USE WMMDATMD, ONLY: WMSETM
  188       USE W3INITMD, ONLY: W3MPIP
  189       USE W3IOPOMD, ONLY: W3IOPP
  190       USE W3SERVMD, ONLY: EXTCDE
  191 !
  192       USE W3GDATMD, ONLY: NX, NY, X0, Y0, SX, MAPSTA, GRIDS,            &
  193                           FLAGLL, ICLOSE, ICLOSE_NONE, GTYPE, UNGTYPE,  &
  194                           CLGTYPE, GSU
  195       USE W3GDATMD, ONLY: XYB, TRIGP, MAXX, MAXY, DXYMAX  ! unstructured >
  196       USE W3ODATMD, ONLY: O2INIT, NOPTS, PTLOC, PTNME, GRDID, OUTPTS
  197       USE W3ODATMD, ONLY: O2IRQI
  198       USE WMMDATMD, ONLY: MDSE, MDST, NRGRD, MDATAS, IMPROC, NMPSCR,  &
  199                           NMPERR, MDSS
  200       USE W3TRIAMD
  201       USE WMMDATMD, ONLY: MPI_COMM_GRD, MPI_COMM_MWAVE
  202 !
  203       IMPLICIT NONE
  204 !
  205       INCLUDE "mpif.h"
  206 !/
  207 !/ ------------------------------------------------------------------- /
  208 !/ Parameter list
  209 !/
  210       INTEGER, INTENT(IN)                    :: NPT
  211       REAL, INTENT(IN), OPTIONAL             :: XPT(NPT), YPT(NPT)
  212       CHARACTER(LEN=40),INTENT(IN), OPTIONAL :: PNAMES(NPT)
  213 !/
  214 !/ ------------------------------------------------------------------- /
  215 !/ Local parameters
  216 !/
  217       INTEGER                 :: IPT, J, II
  218       INTEGER                 :: IX(4), IY(4)         ! created by w3grmp
  219       REAL                    :: RD(4)                ! created by w3grmp
  220       INTEGER                 :: itout, I1, I2, I3    ! unstructured grids
 
 
                                                         wmiopomd.F90  page   5
 
 
  221       INTEGER                 :: IERR_MPI
  222       REAL                    :: RX, RY, RDX, RDY
  223       REAL, PARAMETER         :: ACC = 0.05
  224       REAL, ALLOCATABLE       :: XP(:), YP(:)
  225       REAL                    :: FACTOR
  226       LOGICAL, ALLOCATABLE    :: INGRID(:,:)
  227       LOGICAL, SAVE           :: SETUP = .FALSE., FLGO7a = .FALSE.
  228       CHARACTER(LEN=40), ALLOCATABLE :: PN(:)
  229 !/
  230 !
  231 ! -------------------------------------------------------------------- /
  232 ! 0.  Initializations
  233 !
  234       CALL W3SETO ( 0, MDSE, MDST )
  235 !
  236 ! -------------------------------------------------------------------- /
  237 ! 1.  Initialize if necessary and possible
  238 !
  239       IF ( .NOT. O2INIT ) THEN
  240 !
  241           IF ( .NOT.PRESENT(XPT) .OR. .NOT.PRESENT(YPT) .OR.          &
  242                .NOT.PRESENT(PNAMES) ) THEN
  243               WRITE (MDSE,1000)
  244               CALL EXTCDE (1)
  245             END IF
  246 !
  247           CALL W3DMO2 ( 0, MDSE, MDST, NPT )
  248 !
  249           NOPTS      = NPT
  250           PTLOC(1,:) = XPT
  251           PTLOC(2,:) = YPT
  252           PTNME      = PNAMES
  253           GRDID      = 'none'
  254 !
  255         END IF
  256 !
  257 ! -------------------------------------------------------------------- /
  258 ! 2.  Locate points in grids
  259 ! 2.a Check all points for all grids
  260 !
  261       IF ( FLAGLL ) THEN
  262           FACTOR = 1.
  263         ELSE
  264           FACTOR = 1.E-3
  265         END IF
  266 !
  267       ALLOCATE ( INGRID(NRGRD,NOPTS), XP(NOPTS), YP(NOPTS) )
  268 !
  269       INGRID = .FALSE.
  270       XP     = PTLOC(1,:)
  271       YP     = PTLOC(2,:)
  272 !
  273       DO J=1, NRGRD
  274 !
  275         CALL W3SETG ( J, MDSE, MDST )
 
 
                                                         wmiopomd.F90  page   6
 
 
  276 !
  277 ! Loop over output points
  278 !
  279 ! notes.....Here, we have pulled coding for UNGTYPE and CLGTYPE from w3io>
  280 ! ..........in w3iopomd.ftn, it is "DO IPT=1, NPT" but otherwise very sim>
  281         DO IPT=1, NOPTS
  282 !
  283 !     Check if point within grid
  284 !
  285           IF (GTYPE .NE. UNGTYPE) THEN
  286             INGRID(J,IPT) = W3GRMP( GSU, XPT(IPT), YPT(IPT), IX, IY, RD )
  287             IF ( .NOT.INGRID(J,IPT) ) THEN
  288               CYCLE
  289               END IF
  290           ELSE
  291             CALL IS_IN_UNGRID(J, XPT(IPT), YPT(IPT), itout, IX, IY, RD )
  292             IF (itout.eq.0) THEN
  293               INGRID(J,IPT)=.FALSE.
  294               END IF
  295             END IF
  296 !
  297 !     Check if point not on land
  298 !
  299             IF ( MAPSTA(IY(1),IX(1)) .EQ. 0 .AND. &
  300               MAPSTA(IY(2),IX(2)) .EQ. 0 .AND. &
  301               MAPSTA(IY(3),IX(3)) .EQ. 0 .AND. &
  302               MAPSTA(IY(4),IX(4)) .EQ. 0 ) THEN
  303               INGRID(J,IPT) = .FALSE.
  304               CYCLE
  305               END IF
  306                     
  307 !.........If we've gotten to this point, then we are satisfied that
  308 !................the point is in this grid.
  309                                            
  310         END DO !        DO IPT=1, NOPTS
  311 !
  312       END DO !      DO J=1, NRGRD
  313 !
  314       DEALLOCATE ( XP, YP )
  315 !
  316 ! 2.b Select a grid for each point
  317 !     start from last, which is supposedly higher resolution
  318 !
  319       MDATAS(:)%NRUPTS = 0
  320 !
  321       DO IPT=1, NOPTS
  322         GRDID(IPT) = '...none...'
  323         DO J= NRGRD, 1, -1
  324           IF ( INGRID(J,IPT) ) THEN
  325             GRDID(IPT) = GRIDS(J)%FILEXT
  326             MDATAS(J)%NRUPTS = MDATAS(J)%NRUPTS + 1
  327             EXIT
  328             END IF
  329           END DO
  330         END DO
 
 
                                                         wmiopomd.F90  page   7
 
 
  331 !
  332 ! 2.c Diagnostic output
  333 !
  334 ! 2.d Test output
  335 !
  336       DEALLOCATE ( INGRID )
  337 !
  338 ! -------------------------------------------------------------------- /
  339 ! 3.  Initialize individual grids
  340 ! 3.a Loop over grids
  341 !
  342       DO J=1, NRGRD
  343 !
  344 ! 3.b (De)allocate map arrays
  345 !
  346         IPT      = MAX ( 1 , MDATAS(J)%NRUPTS )
  347         IF ( SETUP ) DEALLOCATE ( MDATAS(J)%UPTMAP )
  348         ALLOCATE ( MDATAS(J)%UPTMAP(IPT) )
  349 !
  350         IF ( MDATAS(J)%NRUPTS .EQ. 0 ) CYCLE
  351 !
  352         ALLOCATE ( XP(IPT), YP(IPT), PN(IPT) )
  353 !
  354 ! 3.c Set up mapping and point arrays
  355 !
  356         IPT      = 0
  357         DO II=1, NOPTS
  358           IF ( GRDID(II) .NE. GRIDS(J)%FILEXT ) CYCLE
  359           IPT      = IPT + 1
  360           MDATAS(J)%UPTMAP(IPT) = II
  361           XP(IPT)  = PTLOC(1,II)
  362           YP(IPT)  = PTLOC(2,II)
  363           PN(IPT)  = PTNME(II)
  364           END DO
  365 !
  366         IF ( FLGO7a ) CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
  367 !
  368 ! 3.d Preprocessing for output
  369 !
  370 ! 3.d.1 Shared memory version
  371 !
  372 ! 3.d.2 Distributed memory version
  373 !
  374         CALL WMSETM ( J, MDSE, MDST )
  375 !
  376         IF ( MPI_COMM_GRD .NE. MPI_COMM_NULL ) THEN
  377 !
  378             CALL W3SETO ( J, MDSE, MDST )
  379             CALL W3SETG ( J, MDSE, MDST )
  380             CALL W3SETA ( J, MDSE, MDST )
  381             CALL W3SETW ( J, MDSE, MDST )
  382 !
  383             IF ( O2INIT ) THEN
  384                 DEALLOCATE ( OUTPTS(J)%OUT2%IPTINT,               &
  385                     OUTPTS(J)%OUT2%IL   , OUTPTS(J)%OUT2%IW    ,  &
 
 
                                                         wmiopomd.F90  page   8
 
 
  386                     OUTPTS(J)%OUT2%II   , OUTPTS(J)%OUT2%PTIFAC,  &
  387                     OUTPTS(J)%OUT2%PTNME, OUTPTS(J)%OUT2%GRDID ,  &
  388                     OUTPTS(J)%OUT2%DPO  , OUTPTS(J)%OUT2%WAO   ,  &
  389                     OUTPTS(J)%OUT2%WDO  , OUTPTS(J)%OUT2%ASO   ,  &
  390                     OUTPTS(J)%OUT2%CAO  , OUTPTS(J)%OUT2%CDO   ,  &
  391                     OUTPTS(J)%OUT2%SPCO , OUTPTS(J)%OUT2%PTLOC )
  392                 O2INIT = .FALSE.
  393               END IF
  394 !
  395             CALL W3IOPP ( MDATAS(J)%NRUPTS, XP, YP, PN, J )
  396 !
  397             IF ( O2IRQI ) THEN
  398                 DEALLOCATE (OUTPTS(J)%OUT2%IRQPO1,                &
  399                             OUTPTS(J)%OUT2%IRQPO2 )
  400                 O2IRQI = .FALSE.
  401               END IF
  402 !
  403             CALL W3MPIP ( J )
  404 !
  405           END IF
  406 !
  407 ! This barrier is needed to straighten out output.
  408 !
  409 ! 3.e Reset pointers and clean up
  410 !
  411         CALL W3SETO ( 0, MDSE, MDST )
  412         DEALLOCATE ( XP, YP, PN )
  413 !
  414         END DO
  415 !
  416       IF ( FLGO7a ) CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
  417 !
  418 ! -------------------------------------------------------------------- /
  419 ! 4.  Finalize
  420 !
  421       SETUP  = .TRUE.
  422 !
  423       RETURN
  424 !
  425 ! Formats
  426 !
  427  1000 FORMAT (/' *** ERROR WMIOPP : INITALIZATION DATA NOT',          &
  428                ' AVAILABLE *** '/)
  429 !
  430 !/
  431 !/ End of WMIOPP ----------------------------------------------------- /
  432 !/
  433       END SUBROUTINE WMIOPP
  434 !/ ------------------------------------------------------------------- /
  435       SUBROUTINE WMIOPO ( TOUT )
  436 !/
  437 !/                  +-----------------------------------+
  438 !/                  | WAVEWATCH III           NOAA/NCEP |
  439 !/                  |           H. L. Tolman            |
  440 !/                  |                        FORTRAN 90 |
 
 
                                                         wmiopomd.F90  page   9
 
 
  441 !/                  | Last update :         16-Mar-2012 !
  442 !/                  +-----------------------------------+
  443 !/
  444 !/    09-Aug-2006 : Origination.                        ( version 3.10 )
  445 !/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
  446 !/    16-Mar-2012 : Using MPI_COMM_NULL in checks.      ( version 3.14 )
  447 !/
  448 !  1. Purpose :
  449 !
  450 !     Gather and write unified point output.
  451 !
  452 !  2. Method :
  453 !
  454 !     Per-grid point output is already gathered. All data are gathered
  455 !     in the porper storage, and writen using the standard W3IOPO
  456 !     routint from grid number 0.
  457 !
  458 !  3. Parameters :
  459 !
  460 !     Parameter list
  461 !     ----------------------------------------------------------------
  462 !       TOUT    I.A.   I   Time for output file.
  463 !     ----------------------------------------------------------------
  464 !
  465 !  4. Subroutines used :
  466 !
  467 !      Name      Type  Module   Description
  468 !     ----------------------------------------------------------------
  469 !      W3SETG    Subr. W3GDATMD Point to model grid.
  470 !      W3SETW    Subr. W3WDATMD Point to model grid.
  471 !      W3SETO    Subr. W3ODATMD Point to model grid.
  472 !      WMSETM    Subr. WMMDATMD Point to model grid.
  473 !      W3CSPC    Subr. W3CSPCMD Spectral grid conversion.
  474 !      W3IOPO    Subr. W3IOPOMD Point output for single model.
  475 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  476 !      MPI_SEND, MPI_RECV
  477 !                Subr.  mpif.h  Standard MPI library routines.
  478 !     ----------------------------------------------------------------
  479 !
  480 !  5. Called by :
  481 !
  482 !      Name      Type  Module   Description
  483 !     ----------------------------------------------------------------
  484 !      WMWAVE    Prog. WMWAVEMD Multi-grid wave model routine.
  485 !     ----------------------------------------------------------------
  486 !
  487 !  6. Error messages :
  488 !
  489 !  7. Remarks :
  490 !
  491 !  8. Structure :
  492 !
  493 !     See source code.
  494 !
  495 !  9. Switches :
 
 
                                                         wmiopomd.F90  page  10
 
 
  496 !
  497 !       !/MPI  Distributed memory model.
  498 !
  499 !       !/S    Enable subroutine tracing.
  500 !       !/T    Enable test output
  501 !       !/MPIT
  502 !
  503 ! 10. Source code :
  504 !
  505 !/ ------------------------------------------------------------------- /
  506 !     USE CONSTANTS
  507 !
  508       USE W3GDATMD, ONLY: W3SETG
  509       USE W3WDATMD, ONLY: W3SETW
  510       USE W3ODATMD, ONLY: W3SETO
  511       USE WMMDATMD, ONLY: WMSETM
  512       USE W3CSPCMD, ONLY: W3CSPC
  513       USE W3IOPOMD, ONLY: W3IOPO
  514 !
  515       USE W3GDATMD, ONLY: NK, NTH, NSPEC, XFR, FR1, TH, SGRDS
  516       USE W3WDATMD, ONLY: TIME
  517       USE W3ODATMD, ONLY: IAPROC, NAPROC, NAPPNT, NOPTS, SPCO, DPO,   &
  518                           WAO, WDO, ASO, CAO, CDO, OUTPTS,            &
  519                           ICEO,ICEHO,ICEFO
  520       USE WMMDATMD, ONLY: MDST, MDSE, IMPROC, NMPROC, NMPUPT, NRGRD,  &
  521                           RESPEC, UPTMAP, MDSUP
  522       USE WMMDATMD, ONLY: MPI_COMM_MWAVE, MPI_COMM_GRD, ALLPRC,  &
  523                           MTAG0
  524 !
  525       IMPLICIT NONE
  526 !
  527       INCLUDE "mpif.h"
  528 !/
  529 !/ ------------------------------------------------------------------- /
  530 !/ Parameter list
  531 !/
  532       INTEGER, INTENT(IN)     :: TOUT(2)
  533 !/
  534 !/ ------------------------------------------------------------------- /
  535 !/ Local parameters
  536 !/
  537       INTEGER                 :: J, I, II, IT0, IT, ITARG, IFROM
  538       INTEGER                 :: IERR_MPI, NMPPNT
  539       INTEGER, ALLOCATABLE    :: STATUS(:,:)
  540       REAL, POINTER           :: SPEC(:,:)
  541       REAL, POINTER           :: SPCR(:,:), DPR(:), WAR(:),      &
  542                                  WDR(:), ASR(:), CAR(:), CDR(:)
  543       REAL, POINTER           :: ICRO(:), ICRFO(:), ICRHO(:)
  544 !/
  545 !
  546 ! -------------------------------------------------------------------- /
  547 ! 0.  Initializations
  548 !
  549       IF ( IMPROC .EQ. NMPUPT ) THEN
  550           OUTPTS(0)%OUT2%SPCO  = 0.
 
 
                                                         wmiopomd.F90  page  11
 
 
  551           OUTPTS(0)%OUT2%DPO   = 1.
  552           OUTPTS(0)%OUT2%WAO   = 0.
  553           OUTPTS(0)%OUT2%WDO   = 0.
  554           OUTPTS(0)%OUT2%ASO   = 0.
  555           OUTPTS(0)%OUT2%CAO   = 0.
  556           OUTPTS(0)%OUT2%CDO   = 0.
  557           OUTPTS(0)%OUT2%ICEO  = 0.
  558           OUTPTS(0)%OUT2%ICEFO = 0.
  559           OUTPTS(0)%OUT2%ICEHO = 0.
  560         END IF
  561 !
  562 ! -------------------------------------------------------------------- /
  563 ! 1.  Loop over grids for processing local data
  564 !
  565       DO J=1, NRGRD
  566 !
  567 ! 1.a Set up loop
  568 !
  569         CALL W3SETO ( J, MDSE, MDST )
  570         CALL W3SETG ( J, MDSE, MDST )
  571         CALL WMSETM ( J, MDSE, MDST )
  572 !
  573 ! 1.b Determine if action
  574 !
  575         IF ( MPI_COMM_GRD .EQ. MPI_COMM_NULL ) THEN
  576             CYCLE
  577           END IF
  578 !
  579         IF ( NOPTS .EQ. 0 ) THEN
  580             CYCLE
  581           END IF
  582 !
  583         IF ( IAPROC .NE. NAPPNT ) THEN
  584             CYCLE
  585           END IF
  586 !
  587 ! 1.c Data here, and to remain on present processor.
  588 !
  589         IF ( IMPROC .EQ. NMPUPT ) THEN
  590 !
  591 ! 1.c.1 Spectral conversion if needed
  592 !
  593             IF ( RESPEC(0,J) ) THEN
  594                 ALLOCATE ( SPEC(SGRDS(0)%NSPEC,NOPTS) )
  595                 CALL W3CSPC ( SPCO, NK, NTH, XFR, FR1, TH(1), SPEC,   &
  596                      SGRDS(0)%NK, SGRDS(0)%NTH, SGRDS(0)%XFR,         &
  597                      SGRDS(0)%FR1, SGRDS(0)%TH(1), NOPTS, MDST, MDSE, &
  598                      SGRDS(0)%FACHFE )
  599 !
  600 ! 1.c.2 Spectral conversion not needed
  601 !
  602               ELSE
  603                 SPEC   => SPCO
  604               END IF
  605 !
 
 
                                                         wmiopomd.F90  page  12
 
 
  606 ! 1.d Store data at grid 0
  607 !
  608             DO I=1, NOPTS
  609               II     = UPTMAP(I)
  610               OUTPTS(0)%OUT2%SPCO(:,II)  = SPEC(:,I)
  611               OUTPTS(0)%OUT2%DPO(II)     = DPO(I)
  612               OUTPTS(0)%OUT2%WAO(II)     = WAO(I)
  613               OUTPTS(0)%OUT2%WDO(II)     = WDO(I)
  614               OUTPTS(0)%OUT2%ASO(II)     = ASO(I)
  615               OUTPTS(0)%OUT2%CAO(II)     = CAO(I)
  616               OUTPTS(0)%OUT2%CDO(II)     = CDO(I)
  617               OUTPTS(0)%OUT2%ICEO(II)    = ICEO(I)
  618               OUTPTS(0)%OUT2%ICEFO(II)   = ICEFO(I)
  619               OUTPTS(0)%OUT2%ICEHO(II)   = ICEHO(I)
  620             END DO
  621 !
  622             IF ( RESPEC(0,J) ) DEALLOCATE ( SPEC )
  623 !
  624 ! 1.e Data here, and to be sent to other processor.
  625 !
  626           ELSE
  627 !
  628             IT0    = MTAG0 - 7*NRGRD - 1
  629             IT     = IT0 + (J-1)*7
  630             ITARG  = NMPUPT - 1
  631 !
  632             IT     = IT + 1
  633             CALL MPI_SEND ( SPCO(1,1), NSPEC*NOPTS, MPI_REAL,    &
  634                             ITARG, IT, MPI_COMM_MWAVE, IERR_MPI )
  635             IT     = IT + 1
  636             CALL MPI_SEND ( DPO(1), NOPTS, MPI_REAL, ITARG, IT,  &
  637                             MPI_COMM_MWAVE, IERR_MPI )
  638             IT     = IT + 1
  639             CALL MPI_SEND ( WAO(1), NOPTS, MPI_REAL, ITARG, IT,  &
  640                             MPI_COMM_MWAVE, IERR_MPI )
  641             IT     = IT + 1
  642             CALL MPI_SEND ( WDO(1), NOPTS, MPI_REAL, ITARG, IT,  &
  643                             MPI_COMM_MWAVE, IERR_MPI )
  644             IT     = IT + 1
  645             CALL MPI_SEND ( ASO(1), NOPTS, MPI_REAL, ITARG, IT,  &
  646                             MPI_COMM_MWAVE, IERR_MPI )
  647             IT     = IT + 1
  648             CALL MPI_SEND ( CAO(1), NOPTS, MPI_REAL, ITARG, IT,  &
  649                             MPI_COMM_MWAVE, IERR_MPI )
  650             IT     = IT + 1
  651             CALL MPI_SEND ( CDO(1), NOPTS, MPI_REAL, ITARG, IT,  &
  652                             MPI_COMM_MWAVE, IERR_MPI )
  653 !JDM: The below should be added for points using partitioned processors
  654 ! for multigrid, however I am unsure if the IT0 (7 to 10?) should be chan>
  655 ! this is being left here commented out for now.
  656 ! There is a corresponding section to this below
  657 !!/MPI            IT     = IT + 1
  658 !!/MPI            CALL MPI_SEND ( ICEO(1), NOPTS, MPI_REAL, ITARG, IT,  &
  659 !!/MPI                            MPI_COMM_MWAVE, IERR_MPI )
  660 !!/MPIT            WRITE (MDST,9019) IT-IT0, 'ICEO'
 
 
                                                         wmiopomd.F90  page  13
 
 
  661 !!/MPI            IT     = IT + 1
  662 !!/MPI            CALL MPI_SEND ( ICEFO(1), NOPTS, MPI_REAL, ITARG, IT,  &
  663 !!/MPI                            MPI_COMM_MWAVE, IERR_MPI )
  664 !!/MPIT            WRITE (MDST,9019) IT-IT0, 'ICEFO'
  665 !!/MPI            IT     = IT + 1
  666 !!/MPI            CALL MPI_SEND ( ICEHO(1), NOPTS, MPI_REAL, ITARG, IT,  &
  667 !!/MPI                            MPI_COMM_MWAVE, IERR_MPI )
  668 !!/MPIT            WRITE (MDST,9019) IT-IT0, 'ICEHO'
  669 !
  670           END IF
  671 !
  672         END DO
  673 !
  674 ! -------------------------------------------------------------------- /
  675 ! 2.  Check if this is output processor, otherwise exit
  676 !
  677       IF ( IMPROC .NE. NMPUPT ) THEN
  678           RETURN
  679        END IF
  680 !
  681 ! -------------------------------------------------------------------- /
  682 ! 3.  Loop over grids for processing remote data
  683 !
  684 ! 3.a Loop setup
  685 !
  686       DO J=1, NRGRD
  687 !
  688         CALL W3SETO ( J, MDSE, MDST )
  689         CALL W3SETG ( J, MDSE, MDST )
  690         CALL WMSETM ( J, MDSE, MDST )
  691 !
  692         DO NMPPNT= NMPROC, 1, -1
  693           IF ( ALLPRC(NMPPNT,J) .EQ. NAPPNT ) EXIT
  694           END DO
  695 !
  696         IF ( NMPPNT.EQ.NMPUPT .OR. NOPTS.EQ.0 ) THEN
  697             CYCLE
  698           END IF
  699 !
  700 ! 3.b Receive data
  701 !
  702         IT0    = MTAG0 - 7*NRGRD - 1
  703         IT     = IT0 + (J-1)*7
  704         IFROM  = NMPPNT - 1
  705         ALLOCATE ( SPCR(NSPEC,NOPTS), STATUS(MPI_STATUS_SIZE,1),  &
  706                    DPR(NOPTS), WAR(NOPTS), WDR(NOPTS), ASR(NOPTS),&
  707                    CAR(NOPTS), CDR(NOPTS), ICRO(NOPTS),           &
  708                    ICRFO(NOPTS), ICRHO(NOPTS) )
  709 !
  710         IT     = IT + 1
  711         CALL MPI_RECV ( SPCR(1,1), NSPEC*NOPTS, MPI_REAL, IFROM,  &
  712                         IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
  713         IT     = IT + 1
  714         CALL MPI_RECV ( DPR(1), NSPEC*NOPTS, MPI_REAL, IFROM,     &
  715                         IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
 
 
                                                         wmiopomd.F90  page  14
 
 
  716         IT     = IT + 1
  717         CALL MPI_RECV ( WAR(1), NSPEC*NOPTS, MPI_REAL, IFROM,     &
  718                         IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
  719         IT     = IT + 1
  720         CALL MPI_RECV ( WDR(1), NSPEC*NOPTS, MPI_REAL, IFROM,     &
  721                         IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
  722         IT     = IT + 1
  723         CALL MPI_RECV ( ASR(1), NSPEC*NOPTS, MPI_REAL, IFROM,     &
  724                         IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
  725         IT     = IT + 1
  726         CALL MPI_RECV ( CAR(1), NSPEC*NOPTS, MPI_REAL, IFROM,     &
  727                         IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
  728         IT     = IT + 1
  729         CALL MPI_RECV ( CDR(1), NSPEC*NOPTS, MPI_REAL, IFROM,     &
  730                         IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
  731 !JDM: The below should be added for points using partitioned processors
  732 ! for multigrid, however I am unsure if the IT0 (7 to 10?) should be chan>
  733 ! this is being left here commented out for now.
  734 ! There is a corresponding section to this above
  735 !!/MPI         IT     = IT + 1
  736 !!/MPI         CALL MPI_RECV ( ICRO(1), NSPEC*NOPTS, MPI_REAL, IFROM,   &
  737 !!/MPI                        IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
  738 !!/MPIT         WRITE (MDST,9019) IT-IT0, 'ICEO'
  739 !!/MPI         IT     = IT + 1
  740 !!/MPI         CALL MPI_RECV (ICRFO(1), NSPEC*NOPTS, MPI_REAL, IFROM,   &
  741 !!/MPI                        IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
  742 !!/MPIT         WRITE (MDST,9019) IT-IT0, 'ICEFO'
  743 !!/MPI         IT     = IT + 1
  744 !!/MPI         CALL MPI_SEND (ICRHO(1), NSPEC*NOPTS, MPI_REAL, IFROM,   &
  745 !!/MPI                        IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
  746 !!/MPIT            WRITE (MDST,9019) IT-IT0, 'ICEHO'
  747 !
  748 ! 3.c Convert if necessary
  749 !
  750         IF ( RESPEC(0,J) ) THEN
  751             ALLOCATE ( SPEC(SGRDS(0)%NSPEC,NOPTS) )
  752             CALL W3CSPC ( SPCR, NK, NTH, XFR, FR1, TH(1), SPEC,   &
  753                  SGRDS(0)%NK, SGRDS(0)%NTH, SGRDS(0)%XFR,         &
  754                  SGRDS(0)%FR1, SGRDS(0)%TH(1), NOPTS, MDST, MDSE, &
  755                  SGRDS(0)%FACHFE )
  756           ELSE
  757             SPEC   => SPCR
  758           END IF
  759 !
  760 ! 3.d Store data at grid 0
  761 !
  762         DO I=1, NOPTS
  763           II     = UPTMAP(I)
  764           OUTPTS(0)%OUT2%SPCO(:,II)  = SPEC(:,I)
  765           OUTPTS(0)%OUT2%DPO(II)     = DPR(I)
  766           OUTPTS(0)%OUT2%WAO(II)     = WAR(I)
  767           OUTPTS(0)%OUT2%WDO(II)     = WDR(I)
  768           OUTPTS(0)%OUT2%ASO(II)     = ASR(I)
  769           OUTPTS(0)%OUT2%CAO(II)     = CAR(I)
  770           OUTPTS(0)%OUT2%CDO(II)     = CDR(I)
 
 
                                                         wmiopomd.F90  page  15
 
 
  771           OUTPTS(0)%OUT2%ICEO(II)    = ICEO(I)
  772           OUTPTS(0)%OUT2%ICEFO(II)    = ICEFO(I)
  773           OUTPTS(0)%OUT2%ICEHO(II)    = ICEHO(I)
  774         END DO
  775 !
  776         IF ( RESPEC(0,J) ) DEALLOCATE ( SPEC )
  777         DEALLOCATE ( SPCR, DPR, WAR, WDR, ASR, CAR, CDR, STATUS )
  778 !        !JDM add deallocates here and check the itag stuff.. really not
  779 !        sure aabout that
  780         DEALLOCATE (ICRO, ICRFO, ICRHO)
  781         END DO
  782 !
  783 ! -------------------------------------------------------------------- /
  784 ! 4.  Output data
  785 !
  786       CALL W3SETO ( 0, MDSE, MDST )
  787       CALL W3SETG ( 0, MDSE, MDST )
  788       CALL W3SETW ( 0, MDSE, MDST )
  789 !
  790       TIME   = TOUT
  791 !
  792       CALL W3IOPO ( 'WRITE', MDSUP, II, 0 )
  793 !
  794       RETURN
  795 !
  796 ! Formats
  797 !
  798  9040 FORMAT ( ' TEST WMIOPO : PERFORM OUTPUT')
  799 !/
  800 !/ End of WMIOPO ----------------------------------------------------- /
  801 !/
  802       END SUBROUTINE WMIOPO
  803 !/
  804 !/ End of module WMIOPOMD -------------------------------------------- /
  805 !/
  806       END MODULE WMIOPOMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
