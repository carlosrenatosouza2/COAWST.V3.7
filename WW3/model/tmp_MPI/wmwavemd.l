                                                         wmwavemd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE WMWAVEMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         28-Jan-2014 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    13-Jun-2005 : Origination.                        ( version 3.07 )
   13 !/    30-Jan-2006 : Add static nesting.                 ( version 3.08 )
   14 !/    25-May-2006 : Add overlapping grids.              ( version 3.09 )
   15 !/    09-Aug-2006 : Unified point output added.         ( version 3.10 )
   16 !/    22-Dec-2006 : Final algorith changes for tests.   ( version 3.10 )
   17 !/    25-Jan-2007 : Tweaking algorithm.                 ( version 3.10 )
   18 !/    02-Feb-2007 : Replacing MPI_BCAST with WMBCST.    ( version 3.10 )
   19 !/    07-Feb-2007 : Reintroduce pre-fetching.           ( version 3.10 )
   20 !/    10-May-2007 : Removing / streamlining WMBCST.     ( version 3.11 )
   21 !/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
   22 !/    20-Sep-2007 : Fix reset of GRSTAT in 0.b          ( version 3.13 )
   23 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   24 !/    20-Aug-2010 : Fix MAPSTA/MAPST2 bug.            ( version 3.14.6 )
   25 !/    12-Mar-2012 : Use MPI_COMM_NULL for checks.       ( version 3.14 )
   26 !/    28-Jan-2014 : Add memory hwm to profiling.        ( version 5.00 )
   27 !/
   28 !/    Copyright 2009-2014 National Weather Service (NWS),
   29 !/       National Oceanic and Atmospheric Administration.  All rights
   30 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   31 !/       No unauthorized use without permission.
   32 !/
   33 !  1. Purpose :
   34 !
   35 !     Running the multi-grid version of WAVEWATCH III up to a given
   36 !     ending time for each grid.
   37 !
   38 !  2. Variables and types :
   39 !
   40 !      Name      Type  Scope    Description
   41 !     ----------------------------------------------------------------
   42 !     ----------------------------------------------------------------
   43 !
   44 !  3. Subroutines and functions :
   45 !
   46 !      Name      Type  Scope    Description
   47 !     ----------------------------------------------------------------
   48 !      WMWAVE    Subr. Public   Wave model initialization.
   49 !      WMPRNT    Subr. Public   Print action table to log file.
   50 !      WMBCST    Subr. Public   Non-blocking MPI broadcast.
   51 !      WMWOUT    Subr. Public   Non-blocking MPI broadcast.
   52 !     ----------------------------------------------------------------
   53 !
   54 !  4. Subroutines and functions used :
   55 !
 
 
                                                         wmwavemd.F90  page   2
 
 
   56 !     See subroutine documentation.
   57 !
   58 !  5. Remarks :
   59 !
   60 !  6. Switches :
   61 !
   62 !     See subroutine documentation.
   63 !
   64 !  7. Source code :
   65 !
   66 !/ ------------------------------------------------------------------- /
   67       PUBLIC
   68 !/
   69       CONTAINS
   70 !/ ------------------------------------------------------------------- /
   71       SUBROUTINE WMWAVE ( TEND )
   72 !/
   73 !/                  +-----------------------------------+
   74 !/                  | WAVEWATCH III           NOAA/NCEP |
   75 !/                  |           H. L. Tolman            |
   76 !/                  |                        FORTRAN 90 |
   77 !/                  | Last update :         28-Jan-2014 |
   78 !/                  +-----------------------------------+
   79 !/
   80 !/    13-Jun-2005 : Origination.                        ( version 3.07 )
   81 !/    30-Jan-2006 : Add static nesting.                 ( version 3.08 )
   82 !/    25-May-2006 : Add overlapping grids.              ( version 3.09 )
   83 !/    09-Aug-2006 : Unified point output added.         ( version 3.10 )
   84 !/    22-Dec-2006 : Final algorith changes for tests.   ( version 3.10 )
   85 !/    25-Jan-2007 : Tweaking algorithm.                 ( version 3.10 )
   86 !/    02-Feb-2007 : Replacing MPI_BCAST with WMBCST.    ( version 3.10 )
   87 !/    07-Feb-2007 : Reintroduce pre-fetching.           ( version 3.10 )
   88 !/    10-May-2007 : Removing / streamlining WMBCST.     ( version 3.11 )
   89 !/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
   90 !/    20-Sep-2007 : Fix reset of GRSTAT in 0.b          ( version 3.13 )
   91 !/    20-Aug-2010 : Fix MAPSTA/MAPST2 bug sec. 9.a.   ( version 3.14.6 )
   92 !/    12-Mar-2012 : Use MPI_COMM_NULL for checks.       ( version 3.14 )
   93 !/    28-Jan-2014 : Add memory hwm to profiling.        ( version 5.00 )
   94 !/
   95 !  1. Purpose :
   96 !
   97 !     Run multi-grid version of WAVEWATCH III.
   98 !
   99 !  2. Method :
  100 !
  101 !     See manual.
  102 !
  103 !  3. Parameters :
  104 !
  105 !     Parameter list
  106 !     ----------------------------------------------------------------
  107 !       TEND    I.A.   I   Ending time for calculations for each grid.
  108 !     ----------------------------------------------------------------
  109 !
  110 !     Local variables
 
 
                                                         wmwavemd.F90  page   3
 
 
  111 !     ----------------------------------------------------------------
  112 !       J       Int.   Group counter.
  113 !       JJ      Int.   Grid in group counter.
  114 !       I       Int.   Grid counter.
  115 !     ----------------------------------------------------------------
  116 !
  117 !  4. Subroutines used :
  118 !
  119 !      Name      Type  Module   Description
  120 !     ----------------------------------------------------------------
  121 !      W3SETG    Subr  W3GDATMD Point to grid/model.
  122 !      W3SETW    Subr  W3WDATMD Point to grid/model.
  123 !      W3SETA    Subr  W3ADATMD Point to grid/model.
  124 !      W3SETO    Subr  W3ODATMD Point to grid/model.
  125 !      W3IOPE    Subr  W3IOPOMD Extracting point output.
  126 !      W3WAVE    Subr  W3WAVEMD Actual ave model routine.
  127 !      STRACE    Subr  W3SERVMD Subroutine tracing.
  128 !      EXTCDE    Subr    Id.    Abort program with exit code.
  129 !      WWTIME    Subr    Id.    System time in readable format.
  130 !      PRTIME    Subr    Id.    Profiling routine ( !/MPRF )
  131 !      STME21    Subr  W3TIMEMD Print date and time readable.
  132 !      DSEC21    Func    Id.    Difference between times.
  133 !      TICK21    Subr    Id.    Advance time.
  134 !      WMSETM    Subr  WMMDATMD Point to grid/model.
  135 !      WMUPDT    Subr  WMUPDTMD Update input fields at driver level.
  136 !      WMIOBG    Subr  WMINIOMD Gather staged boundary data.
  137 !      WMIOBS    Subr    Id.    Stage boundary data.
  138 !      WMIOBF    Subr    Id.    Finalize WMIOBS.            ( !/MPI )
  139 !      WMIOHS    Subr    Id.    Stage high-to-low data.
  140 !      WMIOHG    Subr    Id.    Gather staged high-to-low data.
  141 !      WMIOHF    Subr    Id.    Finalize WMIOHS.            ( !/MPI )
  142 !      WMIOES    Subr    Id.    Stage same-rank data.
  143 !      WMIOEG    Subr    Id.    Gather staged same-rank data.
  144 !      WMIOEF    Subr    Id.    Finalize WMIOES.            ( !/MPI )
  145 !      WMIOPO    Subr  WMIOPOMD Unified point output.
  146 !     ----------------------------------------------------------------
  147 !
  148 !  5. Called by :
  149 !
  150 !      Name      Type  Module   Description
  151 !     ----------------------------------------------------------------
  152 !      W3MLTI    Prog.   N/A    Multi-grid model driver.
  153 !      ....                     Any coupled model.
  154 !     ----------------------------------------------------------------
  155 !
  156 !  6. Error messages :
  157 !
  158 !     See formats 1000 and following, or escape locations 2000 and
  159 !     following.
  160 !
  161 !  7. Remarks :
  162 !
  163 !     - If no action is taken in the endless loop, an error is
  164 !       assumed (code 2099). This should never take place in the
  165 !       default driver, but may be a problem in a coupled model.
 
 
                                                         wmwavemd.F90  page   4
 
 
  166 !     - If output is requested for the initial model time, TSYNC
  167 !       is set to TIME instead of (-1,0) for GRSTAT = 0. In this case
  168 !       input is updated, after which GRSTAT is set to 6 instead
  169 !       of 1. This assures that restarts do not impact model results
  170 !       by spurious double reconciliations.
  171 !
  172 !  8. Structure :
  173 !
  174 !     See source code and manual.
  175 !
  176 !  9. Switches :
  177 !
  178 !       !/S     Enable subroutine tracing.
  179 !       !/T     Enable test output.
  180 !       !/MPIT  Enable test output (use with !/MPI only).
  181 !       !/MPRF  Profiling.
  182 !
  183 !       !/SHRD, !/DIST, !/MPI
  184 !               Shared / distributed program model.
  185 !
  186 ! 10. Source code :
  187 !
  188 !/ ------------------------------------------------------------------- /
  189       USE CONSTANTS
  190 !/
  191       USE W3GDATMD, ONLY: W3SETG
  192       USE W3WDATMD, ONLY: W3SETW
  193       USE W3ADATMD, ONLY: W3SETA
  194       USE W3ODATMD, ONLY: W3SETO, NOTYPE
  195       USE W3IOPOMD, ONLY: W3IOPE
  196       USE W3WAVEMD, ONLY: W3WAVE
  197       USE W3SERVMD, ONLY: EXTCDE, WWTIME
  198       USE W3TIMEMD, ONLY: DSEC21, STME21, TICK21
  199       USE WMMDATMD, ONLY: WMSETM
  200       USE WMUPDTMD, ONLY: WMUPDT
  201       USE WMINIOMD, ONLY: WMIOBG, WMIOBS, WMIOBF, WMIOHG, WMIOHS,     &
  202                           WMIOHF, WMIOEG, WMIOES, WMIOEF
  203       USE WMIOPOMD, ONLY: WMIOPO
  204 !/
  205       USE W3GDATMD, ONLY: DTMAX, NX, NY, MAPSTA, MAPST2
  206       USE W3WDATMD, ONLY: TIME, VA
  207       USE W3ODATMD, ONLY: FLOUT, TONEXT, DTOUT, TOLAST, IAPROC,       &
  208                           NAPPNT, NOPTS, UNIPTS
  209       USE W3ODATMD, ONLY: NRQPO, IRQPO1
  210       USE W3IDATMD, ONLY: INFLAGS1
  211       USE WMMDATMD, ONLY: MDSO, MDSS, MDST, MDSE, IMPROC,             &
  212                           NMPROC, NMPSCR, NMPERR, NMPTST, NMPLOG,     &
  213                           STIME, ETIME, NMV, TMV, AMV, DMV,           &
  214                           NRGRD, NRGRP, GRANK, INGRP, GRDHGH, GRDEQL, &
  215                           GRDLOW, TSYNC, TMAX, TOUTP, TDATA, GRSTAT,  &
  216                           FLLSTL, FLLSTI, DTRES, FLGHG1, FLGHG2,      &
  217                           MAPMSK
  218       USE WMMDATMD, ONLY: MPI_COMM_MWAVE, MPI_COMM_GRD,          &
  219                           MPI_COMM_BCT, CROOT, FBCAST
  220 !/
 
 
                                                         wmwavemd.F90  page   5
 
 
  221       IMPLICIT NONE
  222 !
  223       INCLUDE "mpif.h"
  224 !/
  225 !/ ------------------------------------------------------------------- /
  226 !/ Parameter list
  227 !/
  228       INTEGER, INTENT(IN)     :: TEND(2,NRGRD)
  229 !/
  230 !/ ------------------------------------------------------------------- /
  231 !/ Local parameters
  232 !/
  233       INTEGER                 :: J, JJ, I, JO, TPRNT(2), TAUX(2),     &
  234                                  II, JJJ, IX, IY, UPNEXT(2), UPLAST(2)
  235       INTEGER                 :: DUMMY2(35)=0
  236       INTEGER                 :: IERR_MPI, NMPSCS
  237       INTEGER, ALLOCATABLE    :: STATUS(:,:)
  238       REAL                    :: DTTST, DTMAXI
  239       CHARACTER(LEN=8)        :: WTIME
  240       CHARACTER(LEN=23)       :: MTIME
  241       LOGICAL                 :: DONE, TSTAMP, FLAGOK, DO_UPT,        &
  242                                  FLG_O1, FLG_O2
  243       LOGICAL                 :: FLAG
  244       LOGICAL, ALLOCATABLE    :: FLSYNC(:), GRSYNC(:), TMSYNC(:),     &
  245                                  FLEQOK(:)
  246       LOGICAL, ALLOCATABLE    :: PREGTB(:), PREGTH(:), PREGTE(:)
  247 !/
  248 !/ ------------------------------------------------------------------- /
  249 !
  250 ! 0.  Initializations ------------------------------------------------ /
  251 ! 0.a Initial testing
  252 !     Test GRSTAT
  253 !
  254       DO I=1, NRGRD
  255         IF ( ( GRSTAT(I).LT.0 .OR. GRSTAT(I).GT.7 ) .AND.             &
  256                GRSTAT(I).NE.99 ) GOTO 2000
  257 !
  258 !     Consistency of times for grids
  259 !
  260         IF ( TSYNC(1,I) .NE. -1 ) THEN
  261             DTTST  = DSEC21 ( TSYNC(:,I), TEND(:,I) )
  262             IF ( DTTST .LT. 0. ) GOTO 2001
  263           END IF
  264         END DO
  265 !
  266 !     Consistency of times within groups, set global TSYNC(:,0)
  267 !
  268       DO J=1, NRGRP
  269         DO JJ=2, INGRP(J,0)
  270           IF ( DSEC21(TSYNC(:,INGRP(J,1)),TSYNC(:,INGRP(J,JJ))).NE.0. &
  271           .OR. DSEC21(TEND(:,INGRP(J,1)),TEND(:,INGRP(J,JJ))).NE.0. ) &
  272                GOTO 2002
  273           END DO
  274         IF ( GRANK(INGRP(J,1)).EQ.1 .AND. TSYNC(1,0).EQ.-1 )          &
  275             TSYNC(:,0) = TSYNC(:,INGRP(J,1))
 
 
                                                         wmwavemd.F90  page   6
 
 
  276         END DO
  277 !
  278 !     Check if FLSYNC initialized
  279 !
  280       IF ( .NOT. ALLOCATED(FLSYNC) ) THEN
  281           ALLOCATE ( FLSYNC(NRGRD), GRSYNC(NRGRP), TMSYNC(NRGRD),     &
  282                      FLEQOK(NRGRD) )
  283           ALLOCATE ( PREGTB(NRGRD), PREGTH(NRGRD), PREGTE(NRGRD) )
  284           FLSYNC = .FALSE.
  285           GRSYNC = .FALSE.
  286           TMSYNC = .TRUE.
  287           FLEQOK = .FALSE.
  288           PREGTB = .FALSE.
  289           PREGTH = .FALSE.
  290           PREGTE = .FALSE.
  291         END IF
  292 !
  293 ! 0.b Reset GRSTAT as needed
  294 !
  295       DO I=1, NRGRD
  296         CALL W3SETW ( I, MDSE, MDST )
  297         DTTST  = DSEC21 ( TIME, TEND(:,I) )
  298         IF ( GRSTAT(I).EQ.99 .AND. DTTST.GT.0. ) GRSTAT(I) = 0
  299         END DO
  300 !
  301 ! 0.c Other initializations
  302 !
  303       DTRES  = 0.
  304       NMPSCS = NMPSCR
  305 !
  306       IF ( UNIPTS ) THEN
  307           CALL W3SETO ( 0, MDSE, MDST )
  308           UPNEXT = TONEXT(:,2)
  309           UPLAST = TOLAST(:,2)
  310           DO_UPT = .TRUE.
  311         ELSE
  312           UPNEXT(1) = -1
  313           UPNEXT(2) =  0
  314           DO_UPT = .FALSE.
  315         END IF
  316 !
  317 ! 0.d Output
  318 !
  319       IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
  320           CALL WMPRNT ( MDSO, NRGRD, TSYNC(:,0), GRSTAT )
  321           CALL STME21 ( TSYNC(:,0), MTIME )
  322           CALL WWTIME ( WTIME )
  323           WRITE (MDSS,901) MTIME, WTIME, MINVAL(GRSTAT), MAXVAL(GRSTAT)
  324           TPRNT  = TSYNC(:,0)
  325           TSTAMP = .TRUE.
  326         ENDIF
  327 !
  328       CALL MPI_BARRIER (MPI_COMM_MWAVE,IERR_MPI)
  329 !
  330 ! 1.  Setting up loop structure -------------------------------------- /
 
 
                                                         wmwavemd.F90  page   7
 
 
  331 !
  332       LOOP_OUTER: DO
  333 !
  334         IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC .AND.                &
  335              DSEC21(TPRNT,TSYNC(:,0)).NE.0. ) THEN
  336             IF ( .NOT. TSTAMP ) WRITE (MDSS,*)
  337             CALL WMPRNT ( MDSO, NRGRD, TSYNC(:,0), GRSTAT )
  338             CALL STME21 ( TSYNC(:,0), MTIME )
  339             CALL WWTIME ( WTIME )
  340             WRITE (MDSS,901) MTIME, WTIME, MINVAL(GRSTAT), MAXVAL(GRSTAT)
  341                                                                          
  342 !
  343             TPRNT  = TSYNC(:,0)
  344             TSTAMP = .TRUE.
  345           ENDIF
  346 !
  347         DONE   = .FALSE.
  348         TPRNT  = TSYNC(:,0)
  349 !
  350         LOOP_J: DO J=1, NRGRP
  351 !
  352           GRSYNC(J) = .FALSE.
  353           DO JJ=1, INGRP(J,0)
  354             I      = INGRP(J,JJ)
  355             CALL WMSETM ( I, MDSE, MDST )
  356             GRSYNC(J) = GRSYNC(J) .OR. FBCAST
  357             END DO
  358 !
  359           LOOP_JJ: DO JJ=1, INGRP(J,0)
  360             I      = INGRP(J,JJ)
  361             CALL WMSETM ( I, MDSE, MDST )
  362 !
  363             IF ( GRSTAT(I).EQ.0 ) TMSYNC(I) = .NOT. FBCAST
  364             IF ( FBCAST ) THEN
  365                 NMPSCR = CROOT
  366               ELSE
  367                 NMPSCR = NMPSCS
  368               END IF
  369 !
  370 ! 2.  Update input fields -------------------------------------------- /
  371 !     ( GRSTAT = 0 )
  372 !
  373 ! 2.a Check TDATA and finish step if data is still OK
  374 !
  375             IF ( GRSTAT(I).EQ.0 .AND. .NOT.FLSYNC(I) ) THEN
  376 !
  377                 IF ( TDATA(1,I) .EQ. -1 ) THEN
  378                     DTTST  = 0.
  379                   ELSE
  380                     CALL W3SETW ( I, MDSE, MDST )
  381                     DTTST  = DSEC21 ( TIME , TDATA(:,I) )
  382                   END IF
  383 !
  384                 IF ( DTTST .GT. 0. ) THEN
  385                     GRSTAT(I) = 1
 
 
                                                         wmwavemd.F90  page   8
 
 
  386                     DONE      = .TRUE.
  387                   END IF
  388 !
  389               END IF
  390 !
  391 ! 2.b Update input and TDATA
  392 !
  393             IF ( GRSTAT(I).EQ.0 .AND. .NOT.FLSYNC(I) .AND.       &
  394                 MPI_COMM_GRD .NE. MPI_COMM_NULL ) THEN
  395 !
  396                 IF ( DTTST .LE. 0 ) THEN
  397                     IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )        &
  398                          WRITE (MDSS,*)
  399                     TSTAMP = .FALSE.
  400                     CALL WMUPDT ( I, TDATA(:,I) )
  401                   END IF
  402 !
  403 ! 2.c Finish up if possible ( !/SHRD or .NOT. FBCAST or no update )
  404 !
  405                 IF ( .NOT. GRSYNC(J) ) THEN
  406                     GRSTAT(I) = 1
  407                     DONE      = .TRUE.
  408                   END IF
  409 !
  410               END IF
  411 !
  412 ! 2.d Synchronize in parts ( !/MPI )
  413 !
  414             IF ( GRSTAT(I).EQ.0 .AND. GRSYNC(J) ) THEN
  415                 DONE   = .TRUE.
  416 !
  417                 IF ( FLSYNC(I) ) THEN
  418                     IF ( FBCAST ) CALL WMBCST                    &
  419                                     ( TDATA(1,I), 2, I, NRGRD, 1 )
  420                     GRSTAT(I) = 1
  421                     FLSYNC(I) = .FALSE.
  422                     IF ( GRSYNC(J) ) CYCLE LOOP_JJ
  423                   ELSE
  424                     FLSYNC(I) = .TRUE.
  425                     CYCLE LOOP_JJ
  426                   END IF
  427 !
  428               END IF
  429 !
  430 ! 3.  Update data from lower ranked grids ---------------------------- /
  431 !     ( GRSTAT = 1 )
  432 !
  433 ! 3.a Skip for initial output only
  434 !
  435             IF ( GRSTAT(I) .EQ. 1 .AND. TSYNC(1,I) .NE. -1 ) THEN
  436                 CALL W3SETW ( I, MDSE, MDST )
  437                 DTTST  = DSEC21 ( TIME, TSYNC(:,I) )
  438                 IF ( DTTST .EQ. 0. ) THEN
  439                     GRSTAT(I) = 7
  440                     DONE      = .TRUE.
 
 
                                                         wmwavemd.F90  page   9
 
 
  441                   END IF
  442               END IF
  443 !
  444 ! 3.b Normal processing
  445 !
  446  
  447             IF ( GRSTAT(I) .EQ. 1 ) THEN
  448 !
  449 ! 3.b.1 Test if data is there
  450 !
  451                 FLAGOK = .TRUE.
  452                 CALL W3SETW ( I, MDSE, MDST )
  453                 TAUX   = TIME
  454                 DO JJJ=1, GRDLOW(I,0)
  455                   CALL W3SETW ( GRDLOW(I,JJJ), MDSE, MDST )
  456                   FLAGOK = FLAGOK .AND. DSEC21(TAUX,TIME).GT.0.       &
  457                                   .AND. GRSTAT(GRDLOW(I,JJJ)).EQ.5
  458                   END DO
  459                 CALL W3SETW ( I, MDSE, MDST )
  460 !
  461 ! 3.b.1 Get the data
  462 !
  463                 IF ( .NOT.FLAGOK .AND. .NOT.PREGTB(I) ) THEN
  464                     IF ( MPI_COMM_GRD.NE.MPI_COMM_NULL )         &
  465                          CALL WMIOBG (I,FLAG)
  466                     PREGTB(I) = .TRUE.
  467                   END IF
  468 !
  469                 IF ( FLAGOK ) THEN
  470                     IF ( MPI_COMM_GRD.NE.MPI_COMM_NULL )         &
  471                          CALL WMIOBG ( I )
  472                     PREGTB(I) = .FALSE.
  473                     GRSTAT(I) = 2
  474                     DONE      = .TRUE.
  475                   END IF
  476 !
  477               END IF
  478 !
  479 ! 4.  Update model time step ----------------------------------------- /
  480 !     ( GRSTAT = 2 )
  481 !
  482             IF ( GRSTAT(I) .EQ. 2 ) THEN
  483 !
  484 ! 4.a Check TMAX and update as necessary ( needs !/MPI synchronizaion )
  485 !
  486                 CALL W3SETW ( I, MDSE, MDST )
  487                 IF ( TMAX(1,I) .EQ. -1 ) THEN
  488                     TMAX(:,I) = TIME
  489                     DTTST  = 0.
  490                   ELSE
  491                     DTTST  = DSEC21(TIME,TMAX(:,I))
  492                   END IF
  493 !
  494                 IF ( DTTST .LE. 0 ) THEN
  495                     CALL W3SETG ( I, MDSE, MDST )
 
 
                                                         wmwavemd.F90  page  10
 
 
  496                     DTMAXI = REAL(NINT(DTMAX+DTRES(I)+0.0001))
  497                     DTRES(I)= DTRES(I)+ DTMAX - DTMAXI
  498                     IF ( ABS(DTRES(I)) .LT. 0.001 ) DTRES(I) = 0.
  499                     TMAX(:,I) = TIME
  500                     CALL TICK21 ( TMAX(:,I), DTMAXI )
  501                     TAUX   = TMAX(:,I)
  502                     IF ( TDATA(1,I) .NE. -1 ) THEN
  503                         IF ( DSEC21(TDATA(:,I),TMAX(:,I)) .GT. 0 )    &
  504                             TMAX(:,I) = TDATA(:,I)
  505                       END IF
  506                     IF ( TOUTP(1,I) .NE. -1 ) THEN
  507                         IF ( DSEC21(TOUTP(:,I),TMAX(:,I)) .GT. 0 )    &
  508                             TMAX(:,I) = TOUTP(:,I)
  509                       END IF
  510                     IF ( UNIPTS ) THEN
  511                         IF ( DSEC21(UPNEXT,TMAX(:,I)) .GT. 0 )        &
  512                             TMAX(:,I) = UPNEXT(:)
  513                       END IF
  514                     DONE   = .TRUE.
  515                     CYCLE LOOP_JJ
  516                   END IF
  517 !
  518 ! 4.b Lowest ranked grids, minimum of all TMAXes
  519 !
  520                 IF ( GRANK(I) .EQ. 1 ) THEN
  521 !
  522                     TAUX   = TMAX(:,I)
  523                     FLAGOK = .TRUE.
  524 !
  525 ! 4.b.1 Check if all grids have reached previous sync point
  526 !
  527                     DO II=1, NRGRD
  528                       CALL W3SETW ( II, MDSE, MDST )
  529                       IF ( TIME(1).NE.-1 .AND.                   &
  530                            MPI_COMM_GRD.NE.MPI_COMM_NULL ) THEN
  531                           IF ( DSEC21(TIME,TSYNC(:,0)) .NE. 0 ) THEN
  532                               FLAGOK = .FALSE.
  533                               EXIT
  534                             END IF
  535                         END IF
  536                       END DO
  537 !
  538 ! 4.b.2 Check availability of data
  539 !
  540                     DO II=1, NRGRD
  541                       IF ( GRANK(II) .EQ. 1 ) THEN
  542                           IF ( TMAX(1,II) .EQ. -1 ) THEN
  543                               FLAGOK = .FALSE.
  544                               EXIT
  545                             ELSE
  546                               IF ( DSEC21 (TAUX,TMAX(:,II)) .LT. 0. ) &
  547                                   TAUX   = TMAX(:,II)
  548                             END IF
  549                         END IF
  550                       END DO
 
 
                                                         wmwavemd.F90  page  11
 
 
  551 !
  552                     CALL W3SETW ( I, MDSE, MDST )
  553                     FLAGOK = FLAGOK .AND. DSEC21(TIME,TAUX).GT.0.
  554 !
  555 ! 4.b.3 Update TSYNC for all grids
  556 !
  557                     IF ( FLAGOK ) THEN
  558 !
  559                         TSYNC(:,0) = TAUX
  560                         DO_UPT = .TRUE.
  561                         DO II=1, NRGRD
  562                           IF ( GRANK(II) .EQ. 1 ) THEN
  563                               TSYNC(:,II) = TAUX
  564                               IF ( GRSTAT(II) .EQ. 2 ) GRSTAT(II) = 3
  565                             END IF
  566                           END DO
  567                         DONE   = .TRUE.
  568 !
  569 ! 4.b.4 Output
  570 !
  571 ! 4.b.5 Skip computations so that all grids start processing
  572 !       simultaneously.
  573 !
  574                         IF ( INGRP(J,0) .GT. 1 ) GOTO 1111
  575 !
  576                       END IF
  577 !
  578 ! 4.c Other grids, logical from relations and TMAXes
  579 !
  580                   ELSE IF ( TSYNC(1,0) .NE. -1 ) THEN
  581 !
  582                     TAUX   = TSYNC(:,0)
  583                     FLAGOK = .TRUE.
  584 !
  585 ! 4.c.1 Check availability of data within group
  586 !       Time within group needs to be the same for load balancing.
  587 !
  588                     DO JJJ=1, INGRP(J,0)
  589                       II     = INGRP(J,JJJ)
  590                       IF ( TMAX(1,II) .EQ. -1 ) THEN
  591                           FLAGOK = .FALSE.
  592                           EXIT
  593                         ELSE
  594                           IF ( DSEC21 (TAUX,TMAX(:,II)) .LT. 0. )     &
  595                               TAUX   = TMAX(:,II)
  596                         END IF
  597                       END DO
  598 !
  599 ! 4.c.2 Check with dependent lower rank grids ( TSYNC )
  600 !
  601                     DO JJJ=1, GRDLOW(I,0)
  602                       II     = GRDLOW(I,JJJ)
  603                       IF ( TSYNC(1,II) .EQ. -1 ) THEN
  604                           FLAGOK = .FALSE.
  605                           EXIT
 
 
                                                         wmwavemd.F90  page  12
 
 
  606                         ELSE
  607                           IF ( DSEC21 (TAUX,TSYNC(:,II)) .LT. 0. )     &
  608                               TAUX   = TSYNC(:,II)
  609                         END IF
  610                       END DO
  611 !
  612 ! 4.c.3 Check with dependent higher rank grids ( TSYNC )
  613 !       No check needed
  614 !
  615 ! 4.c.4 Final check against grid time
  616 !
  617                     CALL W3SETW ( I, MDSE, MDST )
  618                     FLAGOK = FLAGOK .AND. DSEC21(TIME,TAUX).GT.0.
  619 !
  620 ! 4.c.5 Update TSYNC throughout group
  621 !
  622                     IF ( FLAGOK ) THEN
  623 !
  624                         DO JJJ=1, INGRP(J,0)
  625                           II     = INGRP(J,JJJ)
  626                           TSYNC(:,II) = TAUX
  627                           IF ( GRSTAT(II) .EQ. 2 ) GRSTAT(II) = 3
  628                                                                  
  629                           END DO
  630                         DONE   = .TRUE.
  631 !
  632 ! 4.c.6 Skip computations so that all grids in group are advanced
  633 !       simultaneously.
  634                        
  635                         IF ( INGRP(J,0) .GT. 1 ) GOTO 1111
  636 !
  637                       END IF
  638 !
  639                   END IF
  640 !
  641               END IF
  642 !
  643 ! 5.  Run the wave model --------------------------------------------- /
  644 !     ( GRSTAT = 3 ) w3xdatmd data structures set in W3WAVE
  645 !
  646 ! 5.a Run model
  647 !
  648             IF ( GRSTAT(I).EQ.3 .AND.                            &
  649                  MPI_COMM_GRD .EQ. MPI_COMM_NULL ) THEN
  650                 CALL W3SETW ( I, MDSE, MDST )
  651                 TIME      = TSYNC(:,I)
  652                 GRSTAT(I) = 4
  653                 DONE      = .TRUE.
  654               ELSE IF ( GRSTAT(I).EQ.3 .AND.                     &
  655                         MPI_COMM_GRD .NE. MPI_COMM_NULL ) THEN
  656 !
  657                 CALL WMSETM ( I, MDSE, MDST )
  658                 CALL W3WAVE ( I, DUMMY2, TSYNC(:,I), .FALSE., .TRUE. )
  659                 IF ( FLLSTL ) INFLAGS1(1) = .FALSE.
  660                 IF ( FLLSTI ) INFLAGS1(4) = .FALSE.
 
 
                                                         wmwavemd.F90  page  13
 
 
  661 !
  662 ! 5.b Stage data for grids with equal rank
  663 !
  664                 CALL WMIOEF ( I )
  665                 CALL WMIOES ( I )
  666 !
  667 ! 5.c Finish up
  668 !
  669                 GRSTAT(I) = 4
  670                 DONE      = .TRUE.
  671 !
  672               END IF
  673 !
  674 ! 6.  Reconcile grids with same rank --------------------------------- /
  675 !     and stage data transfer to higher and lower ranked grids.
  676 !     ( GRSTAT = 4 )
  677 !
  678             IF ( GRSTAT(I) .EQ. 4 ) THEN
  679 !
  680 ! 6.a Test against times and statuses of dependent grids
  681 !     Note: This is done per GROUP, not per local equal grid dependence
  682 !           Therefore, it is essential that sync times per group are
  683 !           equal (4.c.1) and that all equal grid dependences are a
  684 !           subset of groups (WMGEQL 5.d)
  685 !
  686 ! 6.a.1 Check if sync point is reached
  687 !
  688                 IF ( .NOT. FLEQOK(I) ) THEN
  689 !
  690                     FLAGOK = .TRUE.
  691                     CALL W3SETW ( I, MDSE, MDST )
  692                     TAUX   = TIME
  693                     DO JJJ=1, INGRP(J,0)
  694                       CALL W3SETW ( INGRP(J,JJJ), MDSE, MDST )
  695                       FLAGOK = FLAGOK .AND. DSEC21(TAUX,TIME).EQ.0.   &
  696                                       .AND. GRSTAT(INGRP(J,JJJ)).EQ.4
  697                       END DO
  698                     CALL W3SETW ( I, MDSE, MDST )
  699 !
  700 ! 6.a.2 Point reached, set flag for all in group and cycle
  701 !
  702                     IF ( FLAGOK ) THEN
  703                         DO JJJ=1, INGRP(J,0)
  704                           FLEQOK(INGRP(J,JJJ)) = .TRUE.
  705                           END DO
  706                         DONE      = .TRUE.
  707 !
  708                         IF ( INGRP(J,0) .GT. 1 ) GOTO 1111
  709                       END IF
  710 !
  711                   END IF
  712 !
  713 ! 6.b Call gathering routine, reset FLEQOK and cycle
  714 !
  715                 IF ( .NOT.FLEQOK(I) .AND. .NOT.PREGTE(I) ) THEN
 
 
                                                         wmwavemd.F90  page  14
 
 
  716                     IF ( MPI_COMM_GRD.NE.MPI_COMM_NULL )    &
  717                          CALL WMIOEG (I,FLAG)
  718                     PREGTE(I) = .TRUE.
  719                   END IF
  720 !
  721                 IF ( FLEQOK(I) ) THEN
  722                     IF ( MPI_COMM_GRD.NE.MPI_COMM_NULL )    &
  723                          CALL WMIOEG ( I )
  724                     PREGTE(I) = .FALSE.
  725                     GRSTAT(I) = 5
  726                     FLEQOK(I) = .FALSE.
  727                     DONE      = .TRUE.
  728                   END IF
  729 !
  730 ! 6.c Stage data
  731 !
  732                 IF ( GRSTAT(I) .EQ. 5 ) THEN
  733 !
  734                     IF ( MPI_COMM_GRD .NE. MPI_COMM_NULL ) THEN
  735                         CALL WMIOBF ( I )
  736                         CALL WMIOBS ( I )
  737                       END IF
  738 !
  739                     CYCLE LOOP_JJ
  740 !
  741                   END IF
  742 !
  743               END IF
  744 !
  745 ! 7.  Reconcile with higher ranked grids ----------------------------- /
  746 !     ( GRSTAT = 5 )
  747 !
  748 !     This needs to be a little more complicated than with boundary
  749 !     data to assure proper logic in cases where data providing
  750 !     data does not get data back (e.g., as for the boundary grid
  751 !     in mww3_test_04)
  752 !
  753             IF ( GRSTAT(I) .EQ. 5 ) THEN
  754 !
  755 ! 7.a Test against times and statuses of dependent grids
  756 !
  757                 IF ( GRDHGH(I,0) .EQ. 0 ) THEN
  758                     GRSTAT(I) = 6
  759                     DONE      = .TRUE.
  760                   ELSE
  761 !
  762                     FLAGOK = .TRUE.
  763                     CALL W3SETW ( I, MDSE, MDST )
  764                     TAUX   = TIME
  765                     DO JJJ=1, GRDHGH(I,0)
  766                       CALL W3SETW ( GRDHGH(I,JJJ), MDSE, MDST )
  767                       IF ( .NOT. ( DSEC21(TAUX,TIME).EQ.0. .AND.      &
  768                                    ( GRSTAT(GRDHGH(I,JJJ)).GE.7  .OR. &
  769                                      GRSTAT(GRDHGH(I,JJJ)).LE.2 ) ) ) &
  770                            FLAGOK = .FALSE.
 
 
                                                         wmwavemd.F90  page  15
 
 
  771                       END DO
  772                     CALL W3SETW ( I, MDSE, MDST )
  773 !
  774 ! 7.b Call gathering routine
  775 !
  776                     IF ( .NOT.FLAGOK .AND. .NOT.PREGTH(I) ) THEN
  777                         IF ( MPI_COMM_GRD.NE.MPI_COMM_NULL )     &
  778                              CALL WMIOHG ( I, FLAG )
  779                         PREGTH(I) = .TRUE.
  780                       END IF
  781 !
  782                     IF ( FLAGOK ) THEN
  783                         IF ( MPI_COMM_GRD.NE.MPI_COMM_NULL )     &
  784                              CALL WMIOHG ( I )
  785                         PREGTH(I) = .FALSE.
  786                         GRSTAT(I) = 6
  787                         DONE      = .TRUE.
  788                       END IF
  789 !
  790                   END IF
  791                         
  792 !
  793 ! 7.c Stage data
  794 !
  795                 IF ( GRSTAT(I) .EQ. 6 .AND.                  &
  796                          MPI_COMM_GRD .NE. MPI_COMM_NULL ) THEN
  797                     CALL WMIOHF ( I )
  798                     CALL WMIOHS ( I )
  799                   END IF
  800 !
  801               END IF
  802 !
  803 ! 8.  Perform data assimmilation ------------------------------------- /
  804 !     ( GRSTAT = 6 ) Placeholder only .....
  805 !
  806             IF ( GRSTAT(I) .EQ. 6 ) THEN
  807                 GRSTAT(I) = 7
  808                 DONE      = .TRUE.
  809               END IF
  810 !
  811 ! 9.  Perform output ------------------------------------------------- /
  812 !     ( GRSTAT = 7 ) w3xdatmd data structures set in W3WAVE
  813 !
  814 ! 9.a Check times and finish step if no output to be made
  815 !
  816             IF ( GRSTAT(I).EQ.7 .AND. .NOT.FLSYNC(I) ) THEN
  817 !
  818                 IF ( TOUTP(1,I) .EQ. -1 ) THEN
  819                     DTTST  = 1.
  820                   ELSE
  821                     CALL W3SETW ( I, MDSE, MDST )
  822                     DTTST  = DSEC21 ( TIME , TOUTP(:,I) )
  823                   END IF
  824                 FLG_O1 = DTTST .EQ. 0.
  825 !
 
 
                                                         wmwavemd.F90  page  16
 
 
  826                 IF ( UNIPTS ) THEN
  827                     CALL W3SETW ( I, MDSE, MDST )
  828                     DTTST  = DSEC21 ( TIME , UPNEXT )
  829                     FLG_O2 = DTTST .EQ. 0.
  830                   ELSE
  831                     FLG_O2 = .FALSE.
  832                   END IF
  833 !
  834                 IF ( .NOT.FLG_O1 .AND. .NOT.FLG_O2 ) THEN
  835                     GRSTAT(I) = 8
  836                     DONE      = .TRUE.
  837                   END IF
  838 !
  839               END IF
  840 !
  841 ! 9.b Perform output
  842 !
  843             IF ( GRSTAT(I) .EQ. 7 ) THEN
  844             IF ( MPI_COMM_GRD .NE. MPI_COMM_NULL ) THEN
  845 !
  846 !!/MPRF                CALL PRTIME ( PRFT0 )
  847 !!/MPRF                CALL WMWOUT ( I, NRGRD, 3 )
  848 !!/MPRF                CALL PRTIME ( PRFTN )
  849 !!/MPRF                WRITE (MDSP,991) PRFT0, PRFTN, get_memory(),  &
  850 !!/MPRF                                'BCST',I
  851 !
  852                 IF ( FLG_O1 ) THEN
  853                     CALL W3SETG ( I, MDSE, MDST )
  854                     CALL WMSETM ( I, MDSE, MDST )
  855 !
  856                     IF ( FLGHG1 .AND. .NOT.FLGHG2 .AND.               &
  857                          GRDHGH(I,0).GT.0 ) THEN
  858                         MAPST2 = MAPST2 - 8*MAPMSK
  859                         MAPSTA = ABS(MAPSTA)
  860                         DO IX=1, NX
  861                           DO IY=1, NY
  862                             IF ( MAPST2(IY,IX) .GT. 0 )               &
  863                                  MAPSTA(IY,IX) = - MAPSTA(IY,IX)
  864                             END DO
  865                           END DO
  866 !
  867                       END IF
  868 !
  869                     CALL W3WAVE ( I, DUMMY2, TSYNC(:,I), .FALSE. )
  870 !
  871                     IF ( FLGHG1 .AND. .NOT.FLGHG2 .AND.               &
  872                          GRDHGH(I,0).GT.0 ) THEN
  873                         MAPST2 = MAPST2 + 8*MAPMSK
  874                         MAPSTA = ABS(MAPSTA)
  875                         DO IX=1, NX
  876                           DO IY=1, NY
  877                             IF ( MAPST2(IY,IX) .GT. 0 )               &
  878                                  MAPSTA(IY,IX) = - MAPSTA(IY,IX)
  879                             END DO
  880                           END DO
 
 
                                                         wmwavemd.F90  page  17
 
 
  881                       END IF
  882 !
  883                     IF ( FLLSTL ) INFLAGS1(1) = .FALSE.
  884                     IF ( FLLSTI ) INFLAGS1(4) = .FALSE.
  885 !
  886 ! 9.c Update TOUPT
  887 !
  888                     TOUTP(1,I) = -1
  889                     TOUTP(2,I) =  0
  890 !
  891                     DO JO=1, NOTYPE
  892                       IF ( .NOT.FLOUT(JO) ) CYCLE
  893                       IF ( TOUTP(1,I) .EQ. -1 ) THEN
  894                           TOUTP(:,I) = TONEXT(:,JO)
  895                         ELSE
  896                           DTTST = DSEC21 ( TOUTP(:,I) , TONEXT(:,JO) )
  897                           IF (DTTST.LT.0.) TOUTP(:,I) = TONEXT(:,JO)
  898                         ENDIF
  899                       END DO
  900 ! CHECKPOINT
  901                     JO=8
  902                       IF ( .NOT.FLOUT(JO) ) CYCLE
  903                       IF ( TOUTP(1,I) .EQ. -1 ) THEN
  904                           TOUTP(:,I) = TONEXT(:,JO)
  905                         ELSE
  906                           DTTST = DSEC21 ( TOUTP(:,I) , TONEXT(:,JO) )
  907                           IF (DTTST.LT.0.) TOUTP(:,I) = TONEXT(:,JO)
  908                         ENDIF
  909 ! END CHECKPOINT
  910 !
  911                   END IF
  912                         
  913 !
  914 ! 9.d Process unified point output for selected grid
  915 !
  916                 IF ( UNIPTS ) THEN
  917                     IF ( FLG_O2 ) THEN
  918                         CALL W3SETO ( I, MDSE, MDST )
  919 !
  920                         IF ( NRQPO.NE.0 ) CALL MPI_STARTALL      &
  921                                      ( NRQPO, IRQPO1, IERR_MPI )
  922 !
  923                         IF ( NOPTS.NE.0 .AND. IAPROC.EQ.NAPPNT ) THEN
  924                             CALL W3SETG ( I, MDSE, MDST )
  925                             CALL W3SETA ( I, MDSE, MDST )
  926                             CALL W3IOPE ( VA )
  927                           END IF
  928 !
  929                         IF ( NRQPO .NE. 0 ) THEN
  930                             ALLOCATE ( STATUS(MPI_STATUS_SIZE,NRQPO) )
  931                             CALL MPI_WAITALL                      &
  932                                ( NRQPO, IRQPO1, STATUS, IERR_MPI )
  933                             DEALLOCATE ( STATUS )
  934                           END IF
  935 !
 
 
                                                         wmwavemd.F90  page  18
 
 
  936                       END IF
  937 !
  938                   END IF
  939 !
  940 ! 9.e Update TOUPT outside communicator
  941 !
  942               ELSE IF ( FLG_O1 ) THEN
  943 !
  944                 CALL W3SETO ( I, MDSE, MDST )
  945                 CALL W3SETW ( I, MDSE, MDST )
  946 !
  947                 TIME       = TOUTP(:,I)
  948                 TOUTP(1,I) = -1
  949                 TOUTP(2,I) =  0
  950 !
  951                 DO JO=1, NOTYPE
  952 !
  953                   IF ( FLOUT(JO) ) THEN
  954                       DO
  955                         DTTST = DSEC21 ( TIME, TONEXT(:,JO) )
  956                         IF ( DTTST .LE. 0. ) THEN
  957                             CALL TICK21 ( TONEXT(:,JO), DTOUT(JO) )
  958                             DTTST = DSEC21 ( TONEXT(:,JO), TOLAST(:,JO) )
  959                             IF ( DTTST .LT. 0. ) THEN
  960                                 FLOUT(JO) = .FALSE.
  961                                 EXIT
  962                               END IF
  963                           ELSE
  964                             EXIT
  965                           END IF
  966                         END DO
  967                     END IF
  968 !
  969                   IF ( .NOT.FLOUT(JO) ) CYCLE
  970                   IF ( TOUTP(1,I) .EQ. -1 ) THEN
  971                       TOUTP(:,I) = TONEXT(:,JO)
  972                     ELSE
  973                       DTTST = DSEC21 ( TOUTP(:,I) , TONEXT(:,JO) )
  974                       IF (DTTST.LT.0.) TOUTP(:,I) = TONEXT(:,JO)
  975                     ENDIF
  976 !
  977                   END DO
  978 !
  979 ! Checkpoint
  980 !
  981                 JO=8
  982 !
  983                   IF ( FLOUT(JO) ) THEN
  984                       DO
  985                         DTTST = DSEC21 ( TIME, TONEXT(:,JO) )
  986                         IF ( DTTST .LE. 0. ) THEN
  987                             CALL TICK21 ( TONEXT(:,JO), DTOUT(JO) )
  988                             DTTST = DSEC21 ( TONEXT(:,JO), TOLAST(:,JO) )
  989                             IF ( DTTST .LT. 0. ) THEN
  990                                 FLOUT(JO) = .FALSE.
 
 
                                                         wmwavemd.F90  page  19
 
 
  991                                 EXIT
  992                               END IF
  993                           ELSE
  994                             EXIT
  995                           END IF
  996                         END DO
  997                     END IF
  998 !
  999                   IF ( .NOT.FLOUT(JO) ) CYCLE
 1000                   IF ( TOUTP(1,I) .EQ. -1 ) THEN
 1001                       TOUTP(:,I) = TONEXT(:,JO)
 1002                     ELSE
 1003                       DTTST = DSEC21 ( TOUTP(:,I) , TONEXT(:,JO) )
 1004                       IF (DTTST.LT.0.) TOUTP(:,I) = TONEXT(:,JO)
 1005                     ENDIF
 1006 !
 1007  
 1008 ! End Checkpoint
 1009               END IF
 1010 !
 1011 ! 9.f Finish up
 1012 !
 1013                 GRSTAT(I) = 8
 1014                 DONE      = .TRUE.
 1015 !
 1016               END IF
 1017 !
 1018 ! 10. Go to next time step ------------------------------------------- /
 1019 !     ( GRSTAT = 8 ) ( 9 added for diagnostic output only ... )
 1020 !                    ( Unified point output and synchronization added )
 1021 !
 1022             IF ( GRSTAT(I) .EQ. 8 ) THEN
 1023 !
 1024 ! 10.a Processing unified point output
 1025 !
 1026                 IF ( UNIPTS .AND. DO_UPT ) THEN
 1027                     CALL W3SETW ( I, MDSE, MDST )
 1028                     FLAGOK = DSEC21 ( TIME, UPNEXT ) .EQ. 0.
 1029                   ELSE
 1030                     FLAGOK = .FALSE.
 1031                   END IF
 1032 !
 1033                 IF ( FLAGOK ) THEN
 1034 !
 1035                     DO II=1, NRGRD
 1036                       CALL W3SETW ( II, MDSE, MDST )
 1037                       FLAGOK = FLAGOK .AND. GRSTAT(II).EQ.8 .AND.     &
 1038                                DSEC21(TIME,UPNEXT).EQ.0.
 1039                       END DO
 1040 !
 1041                     IF ( FLAGOK ) THEN
 1042 !
 1043                         CALL WMIOPO ( UPNEXT )
 1044                         DO_UPT = .FALSE.
 1045 !
 
 
                                                         wmwavemd.F90  page  20
 
 
 1046                         CALL W3SETO ( 0, MDSE, MDST )
 1047                         CALL TICK21 ( UPNEXT, DTOUT(2) )
 1048                         IF ( DSEC21(UPNEXT,UPLAST) .GE. 0. ) THEN
 1049                             TONEXT(:,2) = UPNEXT
 1050                           ELSE
 1051                             UNIPTS = .FALSE.
 1052                             UPNEXT(1) = -1
 1053                             UPNEXT(2) =  0
 1054                           END IF
 1055 !
 1056                         DO II=1, NRGRD
 1057                           CALL W3SETW ( II, MDSE, MDST )
 1058                           DTTST  = DSEC21 ( TIME, TEND(:,II) )
 1059                           IF ( DTTST .GT. 0. ) THEN
 1060                               GRSTAT(II) = 9
 1061                            ELSE IF ( DTTST .EQ. 0 ) THEN
 1062                               GRSTAT(II) = 99
 1063                             END IF
 1064                           TSYNC(1,II) = -1
 1065                           TSYNC(2,II) =  0
 1066                           END DO
 1067 !
 1068                         DONE      = .TRUE.
 1069                       END IF
 1070 !
 1071                   ELSE
 1072                     FLAGOK = .TRUE.
 1073                   END IF
 1074 !
 1075 ! 10.b Regular processing
 1076 !
 1077                 IF ( FLAGOK ) THEN
 1078                     CALL W3SETW ( I, MDSE, MDST )
 1079                     DTTST  = DSEC21 ( TIME, TEND(:,I) )
 1080                     IF ( DTTST .GT. 0. ) THEN
 1081                         GRSTAT(I) = 9
 1082                         DONE      = .TRUE.
 1083                      ELSE IF ( DTTST .EQ. 0 ) THEN
 1084                         GRSTAT(I) = 99
 1085                         DONE      = .TRUE.
 1086                       END IF
 1087                   END IF
 1088 !
 1089                 IF ( GRSTAT(I).EQ.9 .OR. GRSTAT(I).EQ.99 ) THEN
 1090                     TSYNC(1,I) = -1
 1091                     TSYNC(2,I) =  0
 1092                   END IF
 1093 !
 1094               END IF
 1095 !
 1096 ! ... End of loops started in 1. ------------------------------------- /
 1097 !
 1098             END DO LOOP_JJ
 1099 !
 1100  1111       CONTINUE
 
 
                                                         wmwavemd.F90  page  21
 
 
 1101 !
 1102           END DO LOOP_J
 1103 !
 1104         NMPSCR = NMPSCS
 1105         IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                    &
 1106             CALL WMPRNT ( MDSO, NRGRD, TSYNC(:,0), GRSTAT )
 1107 !
 1108         DO I=1, NRGRD
 1109           IF ( GRSTAT(I) .EQ. 9 ) GRSTAT(I) = 0
 1110           END DO
 1111 !
 1112         IF ( .NOT. DONE ) GOTO 2099
 1113         IF ( MINVAL(GRSTAT) .EQ. 99 ) EXIT LOOP_OUTER
 1114         END DO LOOP_OUTER
 1115 !
 1116 !    End of routine -------------------------------------------------- /
 1117                                                                         
 1118         IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
 1119            CALL WWTIME ( WTIME )
 1120            WRITE (MDSS,902) WTIME
 1121         ENDIF
 1122 !
 1123       DO I=1, NRGRD
 1124         CALL WMSETM ( I, MDSE, MDST )
 1125         IF ( MPI_COMM_GRD .NE. MPI_COMM_NULL ) THEN
 1126             CALL WMIOBF ( I )
 1127             CALL WMIOHF ( I )
 1128             CALL WMIOEF ( I )
 1129           END IF
 1130         END DO
 1131 !
 1132       RETURN
 1133 !
 1134 ! Escape locations
 1135 !
 1136  2000 CONTINUE
 1137       IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1000) I, GRSTAT(I)
 1138       CALL EXTCDE ( 2000 )
 1139       RETURN
 1140 !
 1141  2001 CONTINUE
 1142       IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1001) I, TSYNC(:,I),      &
 1143                                                       TEND(:,I)
 1144       CALL EXTCDE ( 2001 )
 1145       RETURN
 1146 !
 1147  2002 CONTINUE
 1148       IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1002) J, INGRP(J,1),      &
 1149            INGRP(J,JJ), TSYNC(:,INGRP(J,1)), TSYNC(:,INGRP(J,JJ)),    &
 1150                         TEND(:,INGRP(J,1)), TEND(:,INGRP(J,JJ))
 1151       CALL EXTCDE ( 2002 )
 1152       RETURN
 1153 !
 1154  2099 CONTINUE
 1155       IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1099)
 
 
                                                         wmwavemd.F90  page  22
 
 
 1156       CALL EXTCDE ( 2099 )
 1157       RETURN
 1158 !
 1159 ! Formats
 1160 !
 1161   900 FORMAT ( ' ========== STARTING WAVE MODEL (WMWAVE) ==========', &
 1162                '============================'/)
 1163   901 FORMAT ('  MWW3 calculating for ',A,' at ',A,'  status [',      &
 1164                  I2,'-',I2,']')
 1165   902 FORMAT ('  MWW3 reached the end of the computation loop at ',A)
 1166   999 FORMAT (/' ========== END OF WAVE MODEL (WMWAVE) ============', &
 1167                '============================'/)
 1168 !
 1169  1000 FORMAT (/' *** WAVEWATCH III ERROR IN WMWAVE : *** '/           &
 1170                '     GRID',I3,' HAS ILLEGAL GRSTAT :',I8/)
 1171 !
 1172  1001 FORMAT (/' *** WAVEWATCH III ERROR IN WMWAVE : *** '/           &
 1173                '     GRID',I3,' HAS ILLEGAL TSYNC / TEND '/           &
 1174                '     TSYNC :',I9.8,I7.6/                              &
 1175                '     TEND  :',I9.8,I7.6/)
 1176 !
 1177  1002 FORMAT (/' *** WAVEWATCH III ERROR IN WMWAVE : *** '/           &
 1178                '     GROUP',I3,' HAS INCOMPATIBLE TIMES ',            &
 1179                'IN GRIDS ',I3,' AND ',I3/                             &
 1180                '     TSYNC :',I9.8,I7.6,1X,I9.8,I7.6/                 &
 1181                '     TEND  :',I9.8,I7.6,1X,I9.8,I7.6/)
 1182 !
 1183 ! Note: This 1099 error can occur when multi-grid time steps are not
 1184 !       compatible.
 1185  1099 FORMAT (/' *** WAVEWATCH III ERROR IN WMWAVE : *** '/           &
 1186                '     ABORT FOR POSSIBLE ENDLESS LOOP '/)
 1187 !
 1188 !/
 1189 !/ End of WMWAVE ----------------------------------------------------- /
 1190 !/
 1191       END SUBROUTINE WMWAVE
 1192 !/ ------------------------------------------------------------------- /
 1193       SUBROUTINE WMPRNT ( MDSO, NRGRD, TSYNC, GRSTAT )
 1194 !/
 1195 !/                  +-----------------------------------+
 1196 !/                  | WAVEWATCH III           NOAA/NCEP |
 1197 !/                  |           H. L. Tolman            |
 1198 !/                  |                        FORTRAN 90 |
 1199 !/                  | Last update :         22-Feb-2005 |
 1200 !/                  +-----------------------------------+
 1201 !/
 1202 !/    22-Feb-2005 : Origination.                        ( version 3.07 )
 1203 !/
 1204 !  1. Purpose :
 1205 !
 1206 !     Print out action table in the log file log.ww3m
 1207 !
 1208 !  2. Method :
 1209 !
 1210 !  3. Parameters :
 
 
                                                         wmwavemd.F90  page  23
 
 
 1211 !
 1212 !     Parameter list
 1213 !     ----------------------------------------------------------------
 1214 !       NRGRD   Int.   I   Number of grids.
 1215 !       TSYN    I.A.   I   Synchronization time.
 1216 !       GRSTAT  I.A.   I   Status array per grid.
 1217 !     ----------------------------------------------------------------
 1218 !
 1219 !  4. Subroutines used :
 1220 !
 1221 !      Name      Type  Module   Description
 1222 !     ----------------------------------------------------------------
 1223 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1224 !      STME21    Subr. W3TIMEMD Print date and time readable.
 1225 !     ----------------------------------------------------------------
 1226 !
 1227 !  5. Called by :
 1228 !
 1229 !      Name      Type  Module   Description
 1230 !     ----------------------------------------------------------------
 1231 !      WMWAVE    Subr. WMWAVEMD Multi-grid wave model routine.
 1232 !     ----------------------------------------------------------------
 1233 !
 1234 !  6. Error messages :
 1235 !
 1236 !     None.
 1237 !
 1238 !  7. Remarks :
 1239 !
 1240 !  8. Structure :
 1241 !
 1242 !     See source code.
 1243 !
 1244 !  9. Switches :
 1245 !
 1246 !     !/S     Enable subroutine tracing.
 1247 !     !/T     Test output.
 1248 !
 1249 ! 10. Source code :
 1250 !
 1251 !/ ------------------------------------------------------------------- /
 1252       USE W3TIMEMD, ONLY: STME21
 1253 !/
 1254       IMPLICIT NONE
 1255 !/
 1256 !/ ------------------------------------------------------------------- /
 1257 !/ Parameter list
 1258 !/
 1259       INTEGER, INTENT(IN)     :: MDSO, NRGRD, TSYNC(2), GRSTAT(NRGRD)
 1260 !/
 1261 !/ ------------------------------------------------------------------- /
 1262 !/ Local parameters
 1263 !/
 1264       INTEGER, PARAMETER      :: IW = 15
 1265       INTEGER                 :: I, I0, IN
 
 
                                                         wmwavemd.F90  page  24
 
 
 1266       INTEGER, SAVE           :: IDLAST(2)
 1267       LOGICAL, SAVE           :: FIRST = .TRUE.
 1268       CHARACTER(LEN=23)       :: IDTIME
 1269       CHARACTER(LEN=3)        :: STR(IW), LNE(IW)
 1270 !/
 1271 !/ ------------------------------------------------------------------- /
 1272 !/
 1273 !
 1274       DO I=1, IW
 1275         LNE(I) = '---'
 1276         END DO
 1277 !
 1278       IF ( FIRST ) THEN
 1279           WRITE (MDSO,900) NRGRD, LNE, '-+'
 1280           FIRST  = .FALSE.
 1281           IDLAST(1) = -1
 1282           IDLAST(2) =  0
 1283         ELSE
 1284           BACKSPACE (MDSO)
 1285         END IF
 1286 !
 1287       CALL STME21 ( TSYNC, IDTIME )
 1288 !
 1289       DO I=1, MIN(IW,NRGRD)
 1290         WRITE (STR(I),'(I3)') GRSTAT(I)
 1291         END DO
 1292       DO I=1+MIN(IW,NRGRD), IW
 1293         STR(I) = '   '
 1294         END DO
 1295 !
 1296       IF ( IDLAST(1).EQ.TSYNC(1) .AND. IDLAST(2).EQ.TSYNC(2) ) THEN
 1297         ELSE IF ( IDLAST(1) .EQ. TSYNC(1) ) THEN
 1298           WRITE (MDSO,902) IDTIME(12:19), STR, ' |'
 1299         ELSE
 1300           WRITE (MDSO,901) IDTIME(01:19), STR, ' |'
 1301         END IF
 1302       IDLAST = TSYNC
 1303 !
 1304       IF ( NRGRD .GT. IW ) THEN
 1305           I0     = 1
 1306           IN     = IW
 1307           DO
 1308             I0     = I0 + IW
 1309             IN     = IN + IW
 1310             DO I=I0, MIN(IN,NRGRD)
 1311               WRITE (STR(I-I0+1),'(I3)') GRSTAT(I)
 1312               END DO
 1313             DO I=1+MIN(IN,NRGRD), IN
 1314               STR(I-I0+1) = '   '
 1315               END DO
 1316             WRITE (MDSO,903) STR, ' |'
 1317             IF ( IN .GE. NRGRD ) EXIT
 1318             END DO
 1319         END IF
 1320 !
 
 
                                                         wmwavemd.F90  page  25
 
 
 1321       WRITE (MDSO,904) LNE, '-+'
 1322 !
 1323       RETURN
 1324 !
 1325 ! Formats
 1326 !
 1327   900 FORMAT (1X,' Time (sync rank 1)  | Status for',I3,' grids'/     &
 1328               1X,'---------------------+',16A)
 1329   901 FORMAT (2X,A19,' |',16A)
 1330   902 FORMAT (2X,11X,A8,' |',16A)
 1331   903 FORMAT (21X,' |',16A)
 1332   904 FORMAT (1X,'---------------------+',16A)
 1333 !/
 1334 !/ End of WMPRNT ----------------------------------------------------- /
 1335 !/
 1336       END SUBROUTINE WMPRNT
 1337 !/ ------------------------------------------------------------------- /
 1338       SUBROUTINE WMBCST ( DATA, NR, IMOD, NMOD, ID )
 1339 !/
 1340 !/                  +-----------------------------------+
 1341 !/                  | WAVEWATCH III           NOAA/NCEP |
 1342 !/                  |           H. L. Tolman            |
 1343 !/                  |                        FORTRAN 90 |
 1344 !/                  | Last update :         02-Feb-2007 !
 1345 !/                  +-----------------------------------+
 1346 !/
 1347 !/    02-Feb-2007 : Origination.                        ( version 3.10 )
 1348 !/
 1349 !  1. Purpose :
 1350 !
 1351 !     Non-blocking broadcast, initially for times only, but made for
 1352 !     any integer array. Sending data from first process in the
 1353 !     model cummunicator to all processes that are in the overall
 1354 !     communicator but not in the model communicator.
 1355 !
 1356 !  2. Method :
 1357 !
 1358 !     Standard send and recieves using defined communicator. Send
 1359 !     form first processor in communicator.
 1360 !
 1361 !  3. Parameters :
 1362 !
 1363 !     Parameter list
 1364 !     ----------------------------------------------------------------
 1365 !       DATA    I.A.  I/O  Data to be send/received.
 1366 !       NR      Int.   I   Size of array.
 1367 !       IMOD    Int.   I   Model number.
 1368 !       NMOD    Int.   I   Number of models.
 1369 !       ID      Int.   I   ID number, used with NMOD for ITAG.
 1370 !     ----------------------------------------------------------------
 1371 !
 1372 !  4. Subroutines used :
 1373 !
 1374 !      Name      Type  Module   Description
 1375 !     ----------------------------------------------------------------
 
 
                                                         wmwavemd.F90  page  26
 
 
 1376 !      STRACE    Sur.  W3SERVMD Subroutine tracing.
 1377 !     ----------------------------------------------------------------
 1378 !
 1379 !  5. Called by :
 1380 !
 1381 !      Name      Type  Module   Description
 1382 !     ----------------------------------------------------------------
 1383 !      WMWAVE    Subr. WMWAVEMD Multi-grid wave model routine.
 1384 !     ----------------------------------------------------------------
 1385 !
 1386 !  6. Error messages :
 1387 !
 1388 !  7. Remarks :
 1389 !
 1390 !  8. Structure :
 1391 !
 1392 !  9. Switches :
 1393 !
 1394 !     !/S    Enable subroutine tracing.
 1395 !     !/MPIT Enable test output
 1396 !
 1397 ! 10. Source code :
 1398 !
 1399 !/ ------------------------------------------------------------------- /
 1400 !
 1401       USE WMMDATMD, ONLY: MDST, MTAGB, IMPROC, NMPROC, ALLPRC,   &
 1402                           CROOT, MPI_COMM_MWAVE
 1403 !
 1404       IMPLICIT NONE
 1405 !
 1406       INCLUDE "mpif.h"
 1407 !/
 1408 !/ ------------------------------------------------------------------- /
 1409 !/ Parameter list
 1410 !/
 1411       INTEGER, INTENT(IN)     :: NR, IMOD, NMOD, ID
 1412       INTEGER, INTENT(INOUT)  :: DATA(NR)
 1413 !/
 1414 !/ ------------------------------------------------------------------- /
 1415 !/ Local parameters
 1416 !/
 1417       INTEGER                 :: ITAG, IP, IERR_MPI,             &
 1418                                  STATUS(MPI_STATUS_SIZE)
 1419 !/
 1420 !
 1421 ! -------------------------------------------------------------------- /
 1422 ! 0.  Initializations
 1423 !
 1424       ITAG   = MTAGB + IMOD + ID*NMOD
 1425 !
 1426 !!/MPIT      WRITE (MDST,9000) IMOD, NMOD, ID, ITAG-MTAGB
 1427 !!/MPIT      WRITE (MDST,9001) IMPROC, NMPROC
 1428 !!/MPIT      WRITE (MDST,9002) ALLPRC(:,IMOD)
 1429 !
 1430 ! -------------------------------------------------------------------- /
 
 
                                                         wmwavemd.F90  page  27
 
 
 1431 ! 1.  Processor to send data from
 1432 !
 1433       IF ( ALLPRC(IMPROC,IMOD) .EQ. 1 ) THEN
 1434           DO IP=1, NMPROC
 1435             IF ( ALLPRC(IP,IMOD) .EQ. 0 ) THEN
 1436 !!/MPIT                WRITE (MDST,9010) ALLPRC(IMPROC,IMOD), IP
 1437                 CALL MPI_SEND ( DATA, NR, MPI_INTEGER, IP-1,     &
 1438                                 ITAG, MPI_COMM_MWAVE, IERR_MPI )
 1439               END IF
 1440             END DO
 1441 !
 1442 ! -------------------------------------------------------------------- /
 1443 ! 2.  Processor to receive data at
 1444 !
 1445         ELSE IF ( ALLPRC(IMPROC,IMOD) .EQ. 0 ) THEN
 1446 !!/MPIT          WRITE (MDST,9020) ALLPRC(IMPROC,IMOD), CROOT
 1447           CALL MPI_RECV ( DATA, NR, MPI_INTEGER, CROOT-1, ITAG,  &
 1448                           MPI_COMM_MWAVE, STATUS, IERR_MPI )
 1449                                                             
 1450 !
 1451 ! -------------------------------------------------------------------- /
 1452 ! 3.  Processor with no action
 1453 !
 1454 !!/MPIT        ELSE
 1455 !!/MPIT          WRITE (MDST,9030) ALLPRC(IMPROC,IMOD)
 1456         END IF
 1457 !
 1458       RETURN
 1459 !
 1460 ! Formats
 1461 !
 1462 !/
 1463 !/ End of WMBCST ----------------------------------------------------- /
 1464 !/
 1465       END SUBROUTINE WMBCST
 1466 !/ ------------------------------------------------------------------- /
 1467       SUBROUTINE WMWOUT ( IMOD, NMOD, ID )
 1468 !/
 1469 !/                  +-----------------------------------+
 1470 !/                  | WAVEWATCH III           NOAA/NCEP |
 1471 !/                  |           H. L. Tolman            |
 1472 !/                  |                        FORTRAN 90 |
 1473 !/                  | Last update :         21-Jun-2007 !
 1474 !/                  +-----------------------------------+
 1475 !/
 1476 !/    21-Jun-2007 : Origination.                        ( version 3.11 )
 1477 !/
 1478 !  1. Purpose :
 1479 !
 1480 !     Non-blocking broadcast using dummy parameter to have output!
 1481 !     processes wait for computations on first node to be finished.
 1482 !     Neede for profiling purposes only.
 1483 !
 1484 !  2. Method :
 1485 !
 
 
                                                         wmwavemd.F90  page  28
 
 
 1486 !     Standard send and recieves using defined communicator. Send
 1487 !     form first processor in communicator.
 1488 !
 1489 !  3. Parameters :
 1490 !
 1491 !     Parameter list
 1492 !     ----------------------------------------------------------------
 1493 !       IMOD    Int.   I   Model number.
 1494 !       NMOD    Int.   I   Number of models.
 1495 !       ID      Int.   I   ID number, used with NMOD for ITAG.
 1496 !     ----------------------------------------------------------------
 1497 !
 1498 !  4. Subroutines used :
 1499 !
 1500 !      Name      Type  Module   Description
 1501 !     ----------------------------------------------------------------
 1502 !      STRACE    Sur.  W3SERVMD Subroutine tracing.
 1503 !      W3SETO    Subr. W3ODATMD Point to data structure
 1504 !      W3SETA    Subr. W3ADATMD Point to data structure
 1505 !     ----------------------------------------------------------------
 1506 !
 1507 !  5. Called by :
 1508 !
 1509 !      Name      Type  Module   Description
 1510 !     ----------------------------------------------------------------
 1511 !      WMWAVE    Subr. WMWAVEMD Multi-grid wave model routine.
 1512 !     ----------------------------------------------------------------
 1513 !
 1514 !  6. Error messages :
 1515 !
 1516 !  7. Remarks :
 1517 !
 1518 !  8. Structure :
 1519 !
 1520 !  9. Switches :
 1521 !
 1522 !     !/S    Enable subroutine tracing.
 1523 !     !/MPIT Enable test output
 1524 !
 1525 ! 10. Source code :
 1526 !
 1527 !/ ------------------------------------------------------------------- /
 1528 !
 1529       USE W3ODATMD, ONLY: W3SETO
 1530       USE W3ADATMD, ONLY: W3SETA
 1531 !
 1532       USE W3ODATMD, ONLY: IAPROC, NAPROC, NTPROC
 1533       USE W3ADATMD, ONLY: MPI_COMM_WAVE
 1534       USE WMMDATMD, ONLY: MDST, MDSE, MTAGB
 1535 !
 1536       IMPLICIT NONE
 1537 !
 1538       INCLUDE "mpif.h"
 1539 !/
 1540 !/ ------------------------------------------------------------------- /
 
 
                                                         wmwavemd.F90  page  29
 
 
 1541 !/ Parameter list
 1542 !/
 1543       INTEGER, INTENT(IN)     :: IMOD, NMOD, ID
 1544 !/
 1545 !/ ------------------------------------------------------------------- /
 1546 !/ Local parameters
 1547 !/
 1548       INTEGER                 :: ITAG, IP, IERR_MPI,             &
 1549                                  STATUS(MPI_STATUS_SIZE)
 1550       REAL, SAVE              :: DUMMY = 999.
 1551 !/
 1552 !
 1553 ! -------------------------------------------------------------------- /
 1554 ! 0.  Initializations
 1555 !
 1556       CALL W3SETO ( IMOD, MDSE, MDST )
 1557       CALL W3SETA ( IMOD, MDSE, MDST )
 1558       ITAG   = MTAGB + IMOD + ID*NMOD
 1559 !
 1560 !!/MPIT      WRITE (MDST,9000) IMOD, NMOD, ID, ITAG-MTAGB
 1561 !!/MPIT      WRITE (MDST,9001) IAPROC, NAPROC, NTPROC
 1562       IF ( IAPROC .LT. 1 ) THEN
 1563 !!/MPIT          WRITE (MDST,9002)
 1564           RETURN
 1565         END IF
 1566 !
 1567 ! -------------------------------------------------------------------- /
 1568 ! 1.  Processor to send data from
 1569 !
 1570       IF ( IAPROC .EQ. 1 ) THEN
 1571           DO IP=NAPROC+1, NTPROC
 1572 !!/MPIT            WRITE (MDST,9010) IAPROC, IP
 1573             CALL MPI_SEND ( DUMMY, 1, MPI_INTEGER, IP-1,         &
 1574                             ITAG, MPI_COMM_WAVE, IERR_MPI )
 1575             END DO
 1576 !
 1577 ! -------------------------------------------------------------------- /
 1578 ! 2.  Processor to receive data at
 1579 !
 1580         ELSE IF ( IAPROC .GT. NAPROC ) THEN
 1581 !!/MPIT          WRITE (MDST,9020) IAPROC, 1
 1582           CALL MPI_RECV ( DUMMY, 1, MPI_INTEGER, 0, ITAG,        &
 1583                           MPI_COMM_WAVE, STATUS, IERR_MPI )
 1584 !
 1585 ! -------------------------------------------------------------------- /
 1586 ! 3.  Processor with no action
 1587 !
 1588 !!/MPIT        ELSE
 1589 !!/MPIT          WRITE (MDST,9030) IAPROC
 1590         END IF
 1591 !
 1592       RETURN
 1593 !
 1594 ! Formats
 1595 !
 
 
                                                         wmwavemd.F90  page  30
 
 
 1596 !/
 1597 !/ End of WMWOUT ----------------------------------------------------- /
 1598 !/
 1599       END SUBROUTINE WMWOUT
 1600 !/
 1601 !/ End of module WMWAVEMD -------------------------------------------- /
 1602 !/
 1603       END MODULE WMWAVEMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
