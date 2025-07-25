                                                         w3iopomd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3IOPOMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         05-Jun-2018 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    25-Jan-2001 : Origination.                        ( version 2.00 )
   13 !/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
   14 !/    11-Jun-2001 : Clean-up.                           ( version 2.11 )
   15 !/    10-Nov-2004 : Multiple grid version.              ( version 3.06 )
   16 !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   17 !/    25-Jul-2006 : Adding grid ID per point.           ( version 3.10 )
   18 !/    01-May-2007 : Move O7a output from W3INIT.        ( version 3.11 )
   19 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   20 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   21 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   22 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
   23 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   24 !/    29-Oct-2010 : Implement unstructured grid         ( version 3.14.4 )
   25 !/                  (A. Roland and F. Ardhuin)
   26 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
   27 !/                  specify index closure for a grid.   ( version 3.14 )
   28 !/                  (T. J. Campbell, NRL)
   29 !/    12-Jun-2012 : Add /RTD option or rotated grid option.
   30 !/                  (Jian-Guo Li)                       ( version 4.06 )
   31 !/    02-Sep-2012 : Clean up of open BC for UG grids    ( version 4.07 )
   32 !/    25-Feb-2013 : ITOUT=0 bug correction for UG grids ( version 4.08 )
   33 !/    11-Nov-2013 : SMC and rotated grid incorporated in the main
   34 !/                  trunk                               ( version 4.13 )
   35 !/    05-Jun-2018 : Add SETUP                           ( version 6.04 )
   36 !/    04-Oct-2019 : Optional one file per output stride ( version 7.00 )
   37 !/                  (R. Padilla-Hernandez & J.H. Alves)
   38 !/
   39 !/    Copyright 2009 National Weather Service (NWS),
   40 !/       National Oceanic and Atmospheric Administration.  All rights
   41 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   42 !/       No unauthorized use without permission.
   43 !/
   44 !  1. Purpose :
   45 !
   46 !     Process point output.
   47 !
   48 !  2. Variables and types :
   49 !
   50 !      Name      Type  Scope    Description
   51 !     ----------------------------------------------------------------
   52 !      VEROPT    C*10  Private  Point output file version number.
   53 !      IDSTR     C*32  Private  Point output file ID string.
   54 !     ----------------------------------------------------------------
   55 !
 
 
                                                         w3iopomd.F90  page   2
 
 
   56 !  3. Subroutines and functions :
   57 !
   58 !      Name      Type  Scope    Description
   59 !     ----------------------------------------------------------------
   60 !      W3IOPP    Subr. Public   Preprocessing of point output req.
   61 !      W3IOPE    Subr. Public   Extract point data from grid.
   62 !      W3IOPO    Subr. Public   Point data IO.
   63 !     ----------------------------------------------------------------
   64 !
   65 !  4. Subroutines and functions used :
   66 !
   67 !      Name      Type  Module   Description
   68 !     ----------------------------------------------------------------
   69 !      W3SETO    Subr. W3ODATMD Data structure management.
   70 !      W3SETG    Subr. W3GDATMD Data structure management.
   71 !      W3SETW    Subr. W3WDATMD Data structure management.
   72 !      W3DMO2    Subr. W3ODATMD Data structure management.
   73 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   74 !      EXTCDE    Subr. W3SERVMD Program abort with exit code.
   75 !      MPI_STARTALL, MPIWAITALL
   76 !                Subr.          MPI persistent communication routines.
   77 !     ----------------------------------------------------------------
   78 !
   79 !  5. Remarks :
   80 !
   81 !     - Allocation of allocatable arrays takes place at different
   82 !       places throughout the code, in W3IOPP on write, and in
   83 !       W3IOPO on read.
   84 !
   85 !  6. Switches :
   86 !
   87 !       !/S     Enable subroutine tracing.
   88 !       !/T     Enable test output.
   89 !
   90 !       !/SHRD  Switch for shared / distributed memory architecture.
   91 !       !/DIST  Id.
   92 !       !/MPI   MPI message passing.
   93 !
   94 !       !/O7a   Diagnostic output for output points.
   95 !
   96 !  7. Source code :
   97 !
   98 !/ ------------------------------------------------------------------- /
   99       PUBLIC
  100 !/
  101 !/ Private parameter statements (ID strings)
  102 !/
  103       CHARACTER(LEN=10), PARAMETER, PRIVATE :: VEROPT = '2020-07-30'
  104       CHARACTER(LEN=31), PARAMETER, PRIVATE ::                        &
  105                            IDSTR = 'WAVEWATCH III POINT OUTPUT FILE'
  106 !/
  107       CONTAINS
  108 !/ ------------------------------------------------------------------- /
  109       SUBROUTINE W3IOPP ( NPT, XPT, YPT, PNAMES, IMOD )
  110 !/
 
 
                                                         w3iopomd.F90  page   3
 
 
  111 !/                  +-----------------------------------+
  112 !/                  | WAVEWATCH III           NOAA/NCEP |
  113 !/                  |           H. L. Tolman            |
  114 !/                  |                        FORTRAN 90 |
  115 !/                  | Last update :         02-Sep-2012 |
  116 !/                  +-----------------------------------+
  117 !/
  118 !/    14-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
  119 !/    30-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  120 !/                  Major changes to logistics.
  121 !/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
  122 !/    09-Nov-2004 : Multiple grid version.              ( version 3.06 )
  123 !/    25-Jul-2006 : Adding grid ID per point.           ( version 3.10 )
  124 !/    01-May-2007 : Move O7a output from W3INIT.        ( version 3.11 )
  125 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
  126 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  127 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
  128 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  129 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
  130 !/                  specify index closure for a grid.   ( version 3.14 )
  131 !/                  (T. J. Campbell, NRL)
  132 !/    12-Jun-2012 : Add /RTD option or rotated grid option.
  133 !/                  (Jian-Guo Li)                       ( version 4.06 )
  134 !/    02-Sep-2012 : Clean up of open BC for UG grids    ( version 4.07 )
  135 !/    01-Mar-2018 : Add option to unrotate spectra      ( version 6.02 )
  136 !/                  from RTD grid models
  137 !/
  138 !  1. Purpose :
  139 !
  140 !     Preprocessing of point output.
  141 !
  142 !  2. Method :
  143 !
  144 !     Check location of points in grid and calculate interpolation
  145 !     factors.
  146 !
  147 !  3. Parameters :
  148 !
  149 !     Parameter list
  150 !     ----------------------------------------------------------------
  151 !       NPT     Int.   I   Number of output points in input.
  152 !       XPT     R.A.  I/O  X (longitude) coordinates of output points.
  153 !       YPT     R.A.  I/O  Id. Y.
  154 !       PNAMES  C*40   I   Names of output points.
  155 !       IMOD    Int.   I   Grid ID number.
  156 !     ----------------------------------------------------------------
  157 !
  158 !     Local data
  159 !     ----------------------------------------------------------------
  160 !       ACC     Real  "Accuracy" factor to determine if output point
  161 !                     is grid point.
  162 !     ----------------------------------------------------------------
  163 !
  164 !  4. Subroutines used :
  165 !
 
 
                                                         w3iopomd.F90  page   4
 
 
  166 !     See module documentation.
  167 !
  168 !  5. Called by :
  169 !
  170 !      Name      Type  Module   Description
  171 !     ----------------------------------------------------------------
  172 !      W3INIT    Subr. W3INITMD Wave model initialization routine.
  173 !     ----------------------------------------------------------------
  174 !
  175 !  6. Error messages :
  176 !
  177 !     - Warnings for points out of the grid or on land.
  178 !
  179 !  7. Remarks :
  180 !
  181 !     - The output points are obtained by bi-linear interpolation from
  182 !       the spectra at the grid points. Given the possibility of ice
  183 !       coverage, the actual interpolation factors can only be
  184 !       determined at the actual output time. Hence only the basic
  185 !       bilinear interpolation factors are stored.
  186 !     - Implementation of the /O7a diagnostic output section is
  187 !       currently incomplete and non-functional for curvilinear grids
  188 !       and/or tripole grids
  189 !
  190 !  8. Structure :
  191 !
  192 !     -------------------------------------------
  193 !      Determine grid range
  194 !      do for all defined points
  195 !      -----------------------------------------
  196 !        Check if point within grid
  197 !        Calculate interpolation data
  198 !        Check if point not on land
  199 !        Store interpolation data
  200 !     -------------------------------------------
  201 !
  202 !  9. Switches :
  203 !
  204 !       !/S     Enable subroutine tracing.
  205 !       !/T     Test output.
  206 !
  207 !       !/O7a   Diagnostic output for output points.
  208 !
  209 ! 10. Source code :
  210 !
  211 !/ ------------------------------------------------------------------- /
  212       USE W3GSRUMD
  213       USE W3GDATMD, ONLY: NTH, NK, NSPEC, NX, NY, X0, Y0, SX, GSU,&
  214                           RLGTYPE, CLGTYPE, UNGTYPE, GTYPE, FLAGLL,   &
  215                           ICLOSE,ICLOSE_NONE,ICLOSE_SMPL,ICLOSE_TRPL, &
  216                           MAPSTA, MAPFS, FILEXT, ZB, TRNX, TRNY
  217       USE W3GDATMD, ONLY: XYB, TRIGP,MAXX, MAXY, DXYMAX
  218       USE W3ODATMD, ONLY: W3DMO2
  219       USE W3ODATMD, ONLY: NDSE, NDST, IAPROC, NAPERR, NAPOUT, SCREEN, &
  220                           NOPTS, PTLOC, PTNME, GRDID, IPTINT, PTIFAC
 
 
                                                         w3iopomd.F90  page   5
 
 
  221       USE W3SERVMD, ONLY: EXTCDE
  222       USE W3TRIAMD
  223 !
  224       IMPLICIT NONE
  225 !/
  226 !/ ------------------------------------------------------------------- /
  227 !/ Parameter list
  228 !/
  229       INTEGER, INTENT(IN)          :: NPT, IMOD
  230       REAL, INTENT(INOUT)          :: XPT(NPT), YPT(NPT)
  231       CHARACTER(LEN=40),INTENT(IN) :: PNAMES(NPT)
  232 !/
  233 !/ ------------------------------------------------------------------- /
  234 !/ Local parameters
  235 !/
  236       LOGICAL                 :: INGRID
  237       INTEGER                 :: IPT, J, K
  238       INTEGER                 :: IX1, IY1, IXS, IYS
  239       INTEGER                 :: IX(4), IY(4)   ! Indices of points used >
  240       REAL                    :: RD(4)          ! Interpolation coefficie>
  241       REAL, PARAMETER         :: ACC = 0.05
  242       REAL                    :: FACTOR
  243       INTEGER                 :: ITOUT          ! Triangle index in unstr>
  244 !
  245 !/
  246 !/
  247 !/ ------------------------------------------------------------------- /
  248 !/
  249 !
  250       IF ( FLAGLL ) THEN
  251           FACTOR = 1.
  252         ELSE
  253           FACTOR = 1.E-3
  254         END IF
  255 !
  256       CALL W3DMO2 ( IMOD, NDSE, NDST, NPT )
  257       GRDID  = FILEXT
  258 !
  259       NOPTS  = 0
  260 !
  261 !   Removed by F.A. 2011/04/04  /T      CALL W3GSUP( GSU, NDST )
  262 !
  263 ! Loop over output points
  264 !
  265       DO IPT=1, NPT
  266 !
  267 !     Check if point within grid and compute interpolation weights
  268 !
  269         IF (GTYPE .NE. UNGTYPE) THEN
  270           INGRID = W3GRMP( GSU, XPT(IPT), YPT(IPT), IX, IY, RD )
  271         ELSE
  272           CALL IS_IN_UNGRID(IMOD, XPT(IPT), YPT(IPT), itout, IX, IY, RD)
  273           INGRID = (ITOUT.GT.0)
  274           END IF
  275 !
 
 
                                                         w3iopomd.F90  page   6
 
 
  276         IF ( .NOT.INGRID ) THEN
  277           IF ( IAPROC .EQ. NAPERR ) THEN
  278             IF ( FLAGLL ) THEN
  279               WRITE (NDSE,1000) XPT(IPT), YPT(IPT), PNAMES(IPT)
  280             ELSE
  281               WRITE (NDSE,1001) XPT(IPT), YPT(IPT), PNAMES(IPT)
  282               END IF
  283             END IF
  284           CYCLE
  285           END IF
  286 !
  287 !     Check if point not on land
  288 !
  289         IF ( MAPSTA(IY(1),IX(1)) .EQ. 0 .AND. &
  290              MAPSTA(IY(2),IX(2)) .EQ. 0 .AND. &
  291              MAPSTA(IY(3),IX(3)) .EQ. 0 .AND. &
  292              MAPSTA(IY(4),IX(4)) .EQ. 0 ) THEN
  293           IF ( IAPROC .EQ. NAPERR ) THEN
  294             IF ( FLAGLL ) THEN
  295               WRITE (NDSE,1002) XPT(IPT), YPT(IPT), PNAMES(IPT)
  296             ELSE
  297               WRITE (NDSE,1003) XPT(IPT), YPT(IPT), PNAMES(IPT)
  298               END IF
  299             END IF
  300           CYCLE
  301           END IF
  302 !
  303 !     Store interpolation data
  304 !
  305         NOPTS  = NOPTS + 1
  306 !
  307         PTLOC (1,NOPTS) = XPT(IPT)
  308         PTLOC (2,NOPTS) = YPT(IPT)
  309 !
  310         DO K = 1,4
  311           IPTINT(1,K,NOPTS) = IX(K)
  312           IPTINT(2,K,NOPTS) = IY(K)
  313           PTIFAC(K,NOPTS) = RD(K)
  314           END DO
  315                 
  316         PTNME(NOPTS) = PNAMES(IPT)
  317 !
  318         END DO ! End loop over output points (IPT).
  319 !
  320 ! Diagnostic output
  321 !
  322  
  323 !
  324       RETURN
  325 !
  326 ! Formats
  327 !
  328  1000 FORMAT (/' *** WAVEWATCH-III WARNING :'/                   &
  329                '     OUTPUT POINT OUT OF GRID : ',2F10.3,2X,A/   &
  330                '     POINT SKIPPPED '/)
 
 
                                                         w3iopomd.F90  page   7
 
 
  331  1001 FORMAT (/' *** WAVEWATCH-III WARNING :'/                   &
  332                '     OUTPUT POINT OUT OF GRID : ',2E10.3,2X,A/   &
  333                '     POINT SKIPPPED '/)
  334 !
  335  1002 FORMAT (/' *** WAVEWATCH-III WARNING :'/                   &
  336                '     OUTPUT POINT ON LAND : ',2F10.3,2X,A/       &
  337                '     POINT SKIPPPED '/)
  338  1003 FORMAT (/' *** WAVEWATCH-III WARNING :'/                   &
  339                '     OUTPUT POINT ON LAND : ',2E10.3,2X,A/       &
  340                '     POINT SKIPPPED '/)
  341 !
  342 !/
  343 !/ End of W3IOPP ----------------------------------------------------- /
  344 !/
  345       END SUBROUTINE W3IOPP
  346 !/ ------------------------------------------------------------------- /
  347       SUBROUTINE W3IOPE ( A )
  348 !/
  349 !/                  +-----------------------------------+
  350 !/                  | WAVEWATCH III           NOAA/NCEP |
  351 !/                  |           H. L. Tolman            |
  352 !/                  |                        FORTRAN 90 |
  353 !/                  | Last update :         12-Jun-2012 |
  354 !/                  +-----------------------------------+
  355 !/
  356 !/    12-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
  357 !/    25-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
  358 !/                  Major changes to logistics.
  359 !/    11-Jun-2001 : Clean-up.                           ( version 2.11 )
  360 !/    09-Nov-2004 : Multiple grid version.              ( version 3.06 )
  361 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
  362 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  363 !/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.4 )
  364 !/                  (A. Roland and F. Ardhuin)
  365 !/    12-Jun-2012 : Add /RTD option or rotated grid option.
  366 !/                  (Jian-Guo Li)                       ( version 4.06 )
  367 !/    01-Mar-2018 : Add option to unrotate spectra      ( version 6.02 )
  368 !/                  from RTD grid models
  369 !/
  370 !  1. Purpose :
  371 !
  372 !     Extract point output data and store in output COMMONs. This
  373 !     action is taken from an earlier version of W3IOPO so that the
  374 !     point output postprocessor does not need the full sea-point
  375 !     grid to be able to run.
  376 !       Note that the output spectrum is F(f,theta). Interpolation
  377 !     is performed for this spectrum.
  378 !
  379 !  3. Parameters :
  380 !
  381 !     Parameter list
  382 !     ----------------------------------------------------------------
  383 !       A       R.A.   I   Action spectra on storage grid.
  384 !     ----------------------------------------------------------------
  385 !
 
 
                                                         w3iopomd.F90  page   8
 
 
  386 !  4. Subroutines used :
  387 !
  388 !     See module documentation.
  389 !
  390 !  5. Called by :
  391 !
  392 !      Name      Type  Module   Description
  393 !     ----------------------------------------------------------------
  394 !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
  395 !     ----------------------------------------------------------------
  396 !
  397 !  6. Error messages :
  398 !
  399 !       None.
  400 !
  401 !  7. Remarks :
  402 !
  403 !     - To allow for dynamic ice edges, interpolation factors are
  404 !       calculated for every time step separately.
  405 !     - Wind current and depth data are interpolated ignoring ice,
  406 !       spectrum is interpolated removing ice points.
  407 !     - Spectra are left in par list to allow for change of shape of
  408 !       arrays.
  409 !     - IMOD is not passed to this routine. Since it is used only
  410 !       in W3WAVE, it is assumed that the pointer are set
  411 !       appropriately outside this routine.
  412 !
  413 !  8. Structure :
  414 !
  415 !     See source code.
  416 !
  417 !  9. Switches :
  418 !
  419 !     !/SHRD  Switch for shared / distributed memory architecture.
  420 !     !/DIST  Id.
  421 !     !/MPI   Switch for message passing method.
  422 !
  423 !     !/S     Enable subroutine tracing.
  424 !     !/T     Test output.
  425 !
  426 ! 10. Source code :
  427 !
  428 !/ ------------------------------------------------------------------- /
  429       USE CONSTANTS
  430       USE W3GDATMD, ONLY: NK, NTH, SIG, NX, NY, NSEA, NSEAL,          &
  431                           MAPSTA, MAPFS
  432       USE W3WDATMD, ONLY: ICE, ICEH, ICEF
  433       USE W3ADATMD, ONLY: CG, DW, UA, UD, AS, CX, CY,                 &
  434                           SP => SPPNT
  435       USE W3ODATMD, ONLY: NDST, NOPTS, IPTINT, PTIFAC, IL, IW, II,    &
  436                           DPO, WAO, WDO, ASO, CAO, CDO, ICEO, ICEHO,  &
  437                           ICEFO, SPCO, NAPROC
  438       USE W3ODATMD, ONLY: IRQPO2
  439       USE W3SERVMD, ONLY: EXTCDE
  440 !
 
 
                                                         w3iopomd.F90  page   9
 
 
  441       IMPLICIT NONE
  442 !
  443       INCLUDE "mpif.h"
  444 !/
  445 !/ ------------------------------------------------------------------- /
  446 !/ Parameter list
  447 !/
  448       REAL, INTENT(IN)        :: A(NTH,NK,0:NSEAL)
  449 !/
  450 !/ ------------------------------------------------------------------- /
  451 !/ Local parameters
  452 !/
  453       INTEGER                 :: I, IX1, IY1, IX(4), IY(4), J, IS(4), &
  454                                  IM(4), IK, ITH, ISP
  455       INTEGER                 :: IOFF, IERR_MPI
  456       INTEGER                 :: STAT(MPI_STATUS_SIZE,4*NOPTS)
  457       REAL                    :: RD(4), RDS, RDI, FACRD,              &
  458                                  WNDX, WNDY, CURX, CURY, FAC1(NK),    &
  459                                  FAC2(NK), FAC3(NK), FAC4(NK)
  460       INTEGER                 :: JSEA, ISEA
  461 !/
  462 !/ ------------------------------------------------------------------- /
  463 !/
  464 !
  465       CX(0)  = 0.
  466       CY(0)  = 0.
  467 !
  468 ! Loop over spectra -------------------------------------------------- *
  469 !
  470       DO I=1, NOPTS
  471 !
  472 ! Unpack interpolation data
  473 !
  474         IX(:)  = IPTINT(1,:,I)
  475         IY(:)  = IPTINT(2,:,I)
  476         RD(:)  = PTIFAC(:,I)
  477 !
  478 ! Correct for land and ice and get sea point counters
  479 !
  480         IL(I)  = 0
  481         IW(I)  = 0
  482         II(I)  = 0
  483         RDS    = 0.
  484         RDI    = 0.
  485 !
  486         DO J=1, 4
  487           IS(J)  = MAPFS (IY(J),IX(J))
  488           IM(J)  = MAPSTA(IY(J),IX(J))
  489           IF ( IM(J).GT.0 ) THEN
  490               IW(I)  = IW(I) + 1
  491               RDS    = RDS + RD(J)
  492             ELSE
  493               IF ( IM(J).LT.0 ) THEN
  494                   II(I)  = II(I) + 1
  495                   RDI    = RDI + RD(J)
 
 
                                                         w3iopomd.F90  page  10
 
 
  496                 ELSE
  497                   IL(I)  = IL(I) + 1
  498                   RD(J)  = 0.
  499                 END IF
  500             END IF
  501           END DO
  502 !
  503 ! Depth, wind and current, ignore ice
  504 !
  505         IF ( RDS+RDI .GT. 1.E-7 ) THEN
  506             FACRD  = 1. / (RDS+RDI)
  507             RD     = RD * FACRD
  508           END IF
  509 !
  510 ! Interpolate ice depth wind and current
  511 !
  512         IF (.NOT. LPDLIB) THEN
  513           ICEFO(I) = 0
  514           DO J=1, 4
  515             ISEA = MAPFS(IY(J),IX(J))
  516           JSEA = 1 + (ISEA-1)/NAPROC
  517             ICEFO(I) = ICEFO(I) + RD(J)*ICEF(JSEA)
  518           END DO
  519         ELSE
  520           ICEFO(I) = RD(1)*ICEF(IS(1)) + RD(2)*ICEF(IS(2)) +          &
  521                    RD(3)*ICEF(IS(3)) + RD(4)*ICEF(IS(4))
  522         END IF
  523               
  524         ICEO(I) = RD(1)*ICE(IS(1)) + RD(2)*ICE(IS(2)) +               &
  525                  RD(3)*ICE(IS(3)) + RD(4)*ICE(IS(4))
  526                                                     
  527         ICEHO(I) = RD(1)*ICEH(IS(1)) + RD(2)*ICEH(IS(2)) +            &
  528                  RD(3)*ICEH(IS(3)) + RD(4)*ICEH(IS(4))
  529 !
  530         DPO(I) = RD(1)*DW(IS(1)) + RD(2)*DW(IS(2)) +                  &
  531                  RD(3)*DW(IS(3)) + RD(4)*DW(IS(4))
  532 !
  533         WNDX   = RD(1) * UA(IS(1)) * COS(UD(IS(1))) +                 &
  534                  RD(2) * UA(IS(2)) * COS(UD(IS(2))) +                 &
  535                  RD(3) * UA(IS(3)) * COS(UD(IS(3))) +                 &
  536                  RD(4) * UA(IS(4)) * COS(UD(IS(4)))
  537         WNDY   = RD(1) * UA(IS(1)) * SIN(UD(IS(1))) +                 &
  538                  RD(2) * UA(IS(2)) * SIN(UD(IS(2))) +                 &
  539                  RD(3) * UA(IS(3)) * SIN(UD(IS(3))) +                 &
  540                  RD(4) * UA(IS(4)) * SIN(UD(IS(4)))
  541 !
  542         WAO(I) = SQRT ( WNDX**2 + WNDY**2 )
  543         IF ( WAO(I).GT.1.E-7 ) THEN
  544             WDO(I) = ATAN2(WNDY,WNDX)
  545           ELSE
  546             WDO(I) = 0.
  547           END IF
  548 !
  549         ASO(I) = RD(1)*AS(IS(1)) + RD(2)*AS(IS(2)) +                  &
  550                  RD(3)*AS(IS(3)) + RD(4)*AS(IS(4))
 
 
                                                         w3iopomd.F90  page  11
 
 
  551 !
  552         CURX   = RD(1)*CX(IS(1)) + RD(2)*CX(IS(2)) +                  &
  553                  RD(3)*CX(IS(3)) + RD(4)*CX(IS(4))
  554         CURY   = RD(1)*CY(IS(1)) + RD(2)*CY(IS(2)) +                  &
  555                  RD(3)*CY(IS(3)) + RD(4)*CY(IS(4))
  556 !
  557         CAO(I) = SQRT ( CURX**2 + CURY**2 )
  558         IF ( CAO(I).GT.1.E-7 ) THEN
  559             CDO(I) = ATAN2(CURY,CURX)
  560           ELSE
  561             CDO(I) = 0.
  562           END IF
  563 !
  564 ! Interp. weights for spectra, no ice points (spectra by def. zero)
  565 !
  566         IF ( RDS .GT. 1.E-7 ) THEN
  567             FACRD  = (RDS+RDI) / RDS
  568             RD     = RD * FACRD
  569           END IF
  570 !
  571 ! Extract spectra, shared memory version
  572 !        (done in separate step for MPP compatibility)
  573 !
  574 ! Extract spectra, distributed memory version(s)
  575 !
  576         IOFF   = 1 + 4*(I-1)
  577         CALL MPI_STARTALL ( 4, IRQPO2(IOFF), IERR_MPI )
  578         CALL MPI_WAITALL  ( 4, IRQPO2(IOFF), STAT, IERR_MPI )
  579 !
  580 ! Interpolate spectrum
  581 !
  582         DO IK=1, NK
  583           FAC1(IK) = TPI * SIG(IK) / CG(IK,IS(1))
  584           FAC2(IK) = TPI * SIG(IK) / CG(IK,IS(2))
  585           FAC3(IK) = TPI * SIG(IK) / CG(IK,IS(3))
  586           FAC4(IK) = TPI * SIG(IK) / CG(IK,IS(4))
  587           END DO
  588 !
  589         DO IK=1,NK
  590           DO ITH=1,NTH
  591             ISP    = ITH + (IK-1)*NTH
  592             SPCO(ISP,I) = RD(1) * SP(ITH,IK,1) * FAC1(IK)             &
  593                         + RD(2) * SP(ITH,IK,2) * FAC2(IK)             &
  594                         + RD(3) * SP(ITH,IK,3) * FAC3(IK)             &
  595                         + RD(4) * SP(ITH,IK,4) * FAC4(IK)
  596             END DO
  597           END DO
  598 !
  599  
  600 ! FA COMMENTED OUT: BUG
  601 !At line 1974 of file w3arrymd.f90
  602 !Fortran runtime error: Index '52' of dimension 1 of array 'pnum2' above >
  603 !
  604         END DO
  605 !
 
 
                                                         w3iopomd.F90  page  12
 
 
  606       RETURN
  607 !
  608 ! Formats
  609 !
  610 !/
  611 !/ End of W3IOPE ----------------------------------------------------- /
  612 !/
  613       END SUBROUTINE W3IOPE
  614 !/ ------------------------------------------------------------------- /
  615       SUBROUTINE W3IOPO ( INXOUT, NDSOP, IOTST, IMOD )
  616 !/
  617 !/                  +-----------------------------------+
  618 !/                  | WAVEWATCH III           NOAA/NCEP |
  619 !/                  |           H. L. Tolman            |
  620 !/                  |                        FORTRAN 90 |
  621 !/                  | Last update :         25-Jul-2006 |
  622 !/                  +-----------------------------------+
  623 !/
  624 !/    07-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
  625 !/    30-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  626 !/                  Major changes to logistics.
  627 !/    10-Nov-2004 : Multiple grid version.              ( version 3.06 )
  628 !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
  629 !/    25-Jul-2006 : Adding grid ID per point.           ( version 3.10 )
  630 !/    27-Aug-2015 : Adding interpolation for the ice.   ( version 5.10 )
  631 !/
  632 !  1. Purpose :
  633 !
  634 !     Read/write point output.
  635 !
  636 !  3. Parameters :
  637 !
  638 !     Parameter list
  639 !     ----------------------------------------------------------------
  640 !       INXOUT  C*(*)  I   Test string for read/write, valid are:
  641 !                          'READ' and 'WRITE'.
  642 !       NDSOP   Int.   I   File unit number.
  643 !       IOTST   Int.   O   Test indictor for reading.
  644 !                           0 : Data read.
  645 !                          -1 : Past end of file.
  646 !       IMOD    I(O)   I   Model number for W3GDAT etc.
  647 !     ----------------------------------------------------------------
  648 !
  649 !  4. Subroutines used :
  650 !
  651 !     See module documentation.
  652 !
  653 !  5. Called by :
  654 !
  655 !      Name      Type  Module   Description
  656 !     ----------------------------------------------------------------
  657 !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
  658 !      WW3_OUTP  Prog.   N/A    Postprocessing for point output.
  659 !      GX_OUTP   Prog.   N/A    Grads postprocessing for point output.
  660 !     ----------------------------------------------------------------
 
 
                                                         w3iopomd.F90  page  13
 
 
  661 !
  662 !  6. Error messages :
  663 !
  664 !       Tests on INXOUT, file status and on array dimensions.
  665 !
  666 !  7. Remarks :
  667 !
  668 !     - The output file has the pre-defined name 'out_pnt.FILEXT'.
  669 !     - In MPP version of model data is supposed to be gatherd at the
  670 !       correct processor before the routine is called.
  671 !     - No error output filtering needed.
  672 !
  673 !  8. Structure :
  674 !
  675 !     See source code.
  676 !
  677 !  9. Switches :
  678 !
  679 !     !/SHRD  Switch for shared / distributed memory architecture.
  680 !     !/DIST  Id.
  681 !
  682 !     !/S     Enable subroutine tracing.
  683 !     !/T     Test output.
  684 !
  685 ! 10. Source code :
  686 !
  687 !/ ------------------------------------------------------------------- /
  688       USE W3GDATMD, ONLY: W3SETG
  689       USE W3WDATMD, ONLY: W3SETW
  690       USE W3ODATMD, ONLY: W3SETO, W3DMO2
  691 !/
  692       USE W3GDATMD, ONLY: NTH, NK, NSPEC, FILEXT
  693       USE W3WDATMD, ONLY: TIME
  694       USE W3ODATMD, ONLY: NDST, NDSE, IPASS => IPASS2, NOPTS, IPTINT, &
  695                           IL, IW, II, PTLOC, PTIFAC, DPO, WAO, WDO,   &
  696                           ASO, CAO, CDO, SPCO, PTNME, O2INIT, FNMPRE, &
  697                           GRDID, ICEO, ICEHO, ICEFO
  698       USE W3ODATMD, ONLY :  OFILES
  699 !/
  700 !/
  701       USE W3SERVMD, ONLY: EXTCDE
  702 !
  703       IMPLICIT NONE
  704 !/
  705 !/ ------------------------------------------------------------------- /
  706 !/ Parameter list
  707 !/
  708       INTEGER, INTENT(IN)           :: NDSOP
  709       INTEGER, INTENT(OUT)          :: IOTST
  710       INTEGER, INTENT(IN), OPTIONAL :: IMOD
  711       CHARACTER, INTENT(IN)         :: INXOUT*(*)
  712 !/
  713 !/ ------------------------------------------------------------------- /
  714 !/ local parameters
  715 !/
 
 
                                                         w3iopomd.F90  page  14
 
 
  716       INTEGER                 :: IGRD, IERR, MK, MTH, I, J
  717       LOGICAL,SAVE            :: WRITE
  718       CHARACTER(LEN=31)       :: IDTST
  719       CHARACTER(LEN=10)       :: VERTST
  720 !/
  721       CHARACTER(LEN=15) :: TIMETAG
  722 !/
  723 !/ ------------------------------------------------------------------- /
  724 !/
  725       IPASS  = IPASS + 1
  726       IOTST  = 0
  727 !
  728 ! test input parameters ---------------------------------------------- *
  729 !
  730       IF ( PRESENT(IMOD) ) THEN
  731           IGRD   = IMOD
  732         ELSE
  733           IGRD   = 1
  734         END IF
  735 !
  736       CALL W3SETO ( IGRD, NDSE, NDST )
  737       CALL W3SETG ( IGRD, NDSE, NDST )
  738       CALL W3SETW ( IGRD, NDSE, NDST )
  739 !
  740       IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE' ) THEN
  741           WRITE (NDSE,900) INXOUT
  742           CALL EXTCDE ( 1 )
  743         END IF
  744 !
  745 !      IF ( IPASS.EQ.1 ) THEN
  746       IF ( IPASS.EQ.1  .AND. OFILES(2) .EQ. 0) THEN
  747           WRITE  = INXOUT.EQ.'WRITE'
  748         ELSE
  749           IF ( WRITE .AND. INXOUT.EQ.'READ' ) THEN
  750               WRITE (NDSE,901) INXOUT
  751               CALL EXTCDE ( 2 )
  752             END IF
  753         END IF
  754 !
  755 ! open file ---------------------------------------------------------- *
  756 !
  757       IF ( IPASS.EQ.1 .AND. OFILES(2) .EQ. 0 ) THEN
  758 !
  759           I      = LEN_TRIM(FILEXT)
  760           J      = LEN_TRIM(FNMPRE)
  761 !
  762           IF ( WRITE ) THEN
  763               OPEN (NDSOP,FILE=FNMPRE(:J)//'out_pnt.'//FILEXT(:I),    &
  764                     FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
  765             ELSE
  766               OPEN (NDSOP,FILE=FNMPRE(:J)//'out_pnt.'//FILEXT(:I),    &
  767                     FORM='UNFORMATTED',ERR=800,IOSTAT=IERR,STATUS='OLD')
  768             END IF
  769 !
  770           REWIND ( NDSOP )
 
 
                                                         w3iopomd.F90  page  15
 
 
  771 !
  772 ! test info ---------------------------------------------------------- *
  773 ! ( IPASS = 1 )
  774 !
  775           IF ( WRITE ) THEN
  776               WRITE (NDSOP)                                           &
  777                 IDSTR, VEROPT, NK, NTH, NOPTS
  778             ELSE
  779               READ (NDSOP,END=801,ERR=802,IOSTAT=IERR)                &
  780                 IDTST, VERTST, MK, MTH, NOPTS
  781 !
  782               IF ( IDTST .NE. IDSTR ) THEN
  783                   WRITE (NDSE,902) IDTST, IDSTR
  784                   CALL EXTCDE ( 10 )
  785                 END IF
  786               IF ( VERTST .NE. VEROPT ) THEN
  787                   WRITE (NDSE,903) VERTST, VEROPT
  788                   CALL EXTCDE ( 11 )
  789                 END IF
  790               IF (NK.NE.MK .OR. NTH.NE.MTH) THEN
  791                   WRITE (NDSE,904) MK, MTH, NK, NTH
  792                   CALL EXTCDE ( 12 )
  793                 END IF
  794               IF ( .NOT. O2INIT )                                     &
  795                   CALL W3DMO2 ( IGRD, NDSE, NDST, NOPTS )
  796             END IF
  797 !
  798 ! Point specific info ------------------------------------------------ *
  799 ! ( IPASS = 1 )
  800 !
  801           IF ( WRITE ) THEN
  802               WRITE (NDSOP)                                           &
  803                     ((PTLOC(J,I),J=1,2),I=1,NOPTS), (PTNME(I),I=1,NOPTS)
  804            ELSE
  805               READ  (NDSOP,END=801,ERR=802,IOSTAT=IERR)               &
  806                     ((PTLOC(J,I),J=1,2),I=1,NOPTS), (PTNME(I),I=1,NOPTS)
  807             END IF
  808 !
  809         END IF
  810 !
  811       IF ( IPASS.GE. 1  .AND. OFILES(2) .EQ. 1) THEN
  812           WRITE  = INXOUT.EQ.'WRITE'
  813         ELSE
  814           IF ( WRITE .AND. INXOUT.EQ.'READ' ) THEN
  815               WRITE (NDSE,901) INXOUT
  816               CALL EXTCDE ( 2 )
  817             END IF
  818         END IF
  819               
  820 ! open file ---------------------------------------------------------- *
  821 !
  822       IF ( IPASS.GE.1 .AND. OFILES(2) .EQ. 1) THEN
  823 !
  824           I      = LEN_TRIM(FILEXT)
  825           J      = LEN_TRIM(FNMPRE)
 
 
                                                         w3iopomd.F90  page  16
 
 
  826                                    
  827 ! Create TIMETAG for file name using YYYYMMDD.HHMMS prefix
  828           WRITE(TIMETAG,"(i8.8,'.'i6.6)")TIME(1),TIME(2)
  829 !
  830           IF ( WRITE ) THEN
  831               OPEN (NDSOP,FILE=FNMPRE(:J)//TIMETAG//'.out_pnt.'   &
  832                     //FILEXT(:I),FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
  833             END IF
  834 !
  835           REWIND ( NDSOP )
  836 !
  837 ! test info ---------------------------------------------------------- *
  838 ! ( IPASS GE.1 .AND. OFILES(2) .EQ. 1)
  839 !
  840           IF ( WRITE ) THEN
  841               WRITE (NDSOP)                                           &
  842                 IDSTR, VEROPT, NK, NTH, NOPTS
  843             ELSE
  844               READ (NDSOP,END=801,ERR=802,IOSTAT=IERR)                &
  845                 IDTST, VERTST, MK, MTH, NOPTS
  846 !
  847               IF ( IDTST .NE. IDSTR ) THEN
  848                   WRITE (NDSE,902) IDTST, IDSTR
  849                   CALL EXTCDE ( 10 )
  850                 END IF
  851               IF ( VERTST .NE. VEROPT ) THEN
  852                   WRITE (NDSE,903) VERTST, VEROPT
  853                   CALL EXTCDE ( 11 )
  854                 END IF
  855               IF (NK.NE.MK .OR. NTH.NE.MTH) THEN
  856                   WRITE (NDSE,904) MK, MTH, NK, NTH
  857                   CALL EXTCDE ( 12 )
  858                 END IF
  859               IF ( .NOT. O2INIT )                                     &
  860                   CALL W3DMO2 ( IGRD, NDSE, NDST, NOPTS )
  861             END IF
  862 !
  863 ! Point specific info ------------------------------------------------ *
  864 ! ( IPASS GE.1 .AND. OFILES(2) .EQ. 1)
  865 !
  866           IF ( WRITE ) THEN
  867               WRITE (NDSOP)                                           &
  868                     ((PTLOC(J,I),J=1,2),I=1,NOPTS), (PTNME(I),I=1,NOPTS)
  869            ELSE
  870               READ  (NDSOP,END=801,ERR=802,IOSTAT=IERR)               &
  871                     ((PTLOC(J,I),J=1,2),I=1,NOPTS), (PTNME(I),I=1,NOPTS)
  872             END IF
  873 !
  874         END IF
  875 !
  876 ! TIME --------------------------------------------------------------- *
  877 !
  878       IF ( WRITE ) THEN
  879           WRITE (NDSOP)                            TIME
  880        ELSE
 
 
                                                         w3iopomd.F90  page  17
 
 
  881           READ (NDSOP,END=803,ERR=802,IOSTAT=IERR) TIME
  882         END IF
  883 !
  884 ! Loop over spectra -------------------------------------------------- *
  885 !
  886       DO I=1, NOPTS
  887 !
  888         IF ( WRITE ) THEN
  889              ! set IW, II and IL to 0 because it is not used and gives &
  890              ! outlier values in out_pnt.points
  891              IW(I) = 0
  892              II(I) = 0
  893              IL(I) = 0
  894              WRITE (NDSOP)                                            &
  895                     IW(I), II(I), IL(I), DPO(I), WAO(I), WDO(I),      &
  896                     ASO(I), CAO(I), CDO(I), ICEO(I), ICEHO(I),        &
  897                     ICEFO(I), GRDID(I), (SPCO(J,I),J=1,NSPEC)
  898           ELSE
  899              READ (NDSOP,END=801,ERR=802,IOSTAT=IERR)                 &
  900                     IW(I), II(I), IL(I), DPO(I), WAO(I), WDO(I),      &
  901                     ASO(I), CAO(I), CDO(I), ICEO(I), ICEHO(I),        &
  902                     ICEFO(I), GRDID(I), (SPCO(J,I),J=1,NSPEC)
  903           END IF
  904 !
  905         END DO
  906       IF (OFILES(2) .EQ. 1)  CLOSE (NDSOP)
  907 !
  908       RETURN
  909 !
  910 ! Escape locations read errors
  911 !
  912   800 CONTINUE
  913       WRITE (NDSE,1000) IERR
  914       CALL EXTCDE ( 20 )
  915 !
  916   801 CONTINUE
  917       WRITE (NDSE,1001)
  918       CALL EXTCDE ( 21 )
  919 !
  920   802 CONTINUE
  921       WRITE (NDSE,1002) IERR
  922       CALL EXTCDE ( 22 )
  923 !
  924   803 CONTINUE
  925       IOTST  = -1
  926       RETURN
  927 !
  928 ! Formats
  929 !
  930   900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
  931                '     ILEGAL INXOUT VALUE: ',A/)
  932   901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
  933                '     MIXED READ/WRITE, LAST REQUEST: ',A/)
  934   902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
  935                '     ILEGAL IDSTR, READ : ',A/                        &
 
 
                                                         w3iopomd.F90  page  18
 
 
  936                '                  CHECK : ',A/)
  937   903 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
  938                '     ILEGAL VEROPT, READ : ',A/                       &
  939                '                   CHECK : ',A/)
  940   904 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
  941                '     ERROR IN SPECTRA, MK, MTH : ',2I8/               &
  942                '              ARRAY DIMENSIONS : ',2I8/)
  943 !
  944  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO : '/               &
  945                '     ERROR IN OPENING FILE'/                          &
  946                '     IOSTAT =',I5/)
  947  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO : '/               &
  948                '     PREMATURE END OF FILE'/)
  949  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO : '/               &
  950                '     ERROR IN READING FROM FILE'/                     &
  951                '     IOSTAT =',I5/)
  952 !
  953  
  954 !
  955 !/
  956 !/ End of W3IOPO ----------------------------------------------------- /
  957 !/
  958       END SUBROUTINE W3IOPO
  959 !/
  960 !/ End of module W3IOPOMD -------------------------------------------- /
  961 !/
  962       END MODULE W3IOPOMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
