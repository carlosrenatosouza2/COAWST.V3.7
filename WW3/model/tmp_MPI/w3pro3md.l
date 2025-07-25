                                                         w3pro3md.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3PRO3MD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         27-May-2014 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    27-Feb-2000 : Origination.                        ( version 2.08 )
   13 !/    17-Sep-2000 : Clean-up.                           ( version 2.13 )
   14 !/    10-Dec-2001 : Sub-grid obstructions.              ( version 2.14 )
   15 !/    16-Oct-2002 : Change INTENT for ATRN in W3XYP3.   ( version 3.00 )
   16 !/    26-Dec-2002 : Moving grid version.                ( version 3.02 )
   17 !/    01-Aug-2003 : Moving grid GSE correction.         ( version 3.03 )
   18 !/    17-Dec-2004 : Multiple grid version.              ( version 3.06 )
   19 !/    07-Sep-2005 : Upgrade XY boundary conditions.     ( version 3.08 )
   20 !/    09-Nov-2005 : Removing soft boundary option.      ( version 3.08 )
   21 !/    05-Mar-2008 : Added NEC sxf90 compiler directives.
   22 !/                  (Chris Bunney, UK Met Office)       ( version 3.13 )
   23 !/    01-Apr-2008 : Bug fix W3MAP3 MAPSTA range check.  ( version 3.13 )
   24 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   25 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
   26 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   27 !/    17-Aug-2010 : Add test output W3XYP3.           ( version 3.14.5 )
   28 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
   29 !/                  specify index closure for a grid.   ( version 3.14 )
   30 !/                  (T. J. Campbell, NRL)
   31 !/    26-Dec-2012 : More initializations.               ( version 4.11 )
   32 !/    01-Jul-2013 : Adding UQ and UNO switches to chose between third
   33 !/                  and second order schemes.           ( version 4.12 )
   34 !/    12-Sep-2013 : Add documentation for global clos.  ( version 4.12 )
   35 !/    27-May-2014 : Adding OMPH switch.                 ( version 5.02 )
   36 !/
   37 !/    Copyright 2009-2014 National Weather Service (NWS),
   38 !/       National Oceanic and Atmospheric Administration.  All rights
   39 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   40 !/       No unauthorized use without permission.
   41 !/
   42 !  1. Purpose :
   43 !
   44 !     Bundles routines for third order propagation scheme in single
   45 !     module.
   46 !
   47 !  2. Variables and types :
   48 !
   49 !      Name      Type  Scope    Description
   50 !     ----------------------------------------------------------------
   51 !      TRNMIN    R.P.  Private   Minimum transparancy for local
   52 !                                switching off of averaging.
   53 !     ----------------------------------------------------------------
   54 !
   55 !     Also work arrays for W3KTP3 (private).
 
 
                                                         w3pro3md.F90  page   2
 
 
   56 !
   57 !  3. Subroutines and functions :
   58 !
   59 !      Name      Type  Scope    Description
   60 !     ----------------------------------------------------------------
   61 !      W3MAP3    Subr. Public   Set up auxiliary maps.
   62 !      W3MAPT    Subr. Public   Set up transparency map for GSE.
   63 !      W3XYP3    Subr. Public   Third order spatial propagation.
   64 !      W3KTP3    Subr. Public   Third order spectral propagation.
   65 !     ----------------------------------------------------------------
   66 !
   67 !  4. Subroutines and functions used :
   68 !
   69 !      Name      Type  Module   Description
   70 !     ----------------------------------------------------------------
   71 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   72 !      W3QCK1    Subr. W3UQCKMD Regular grid UQ scheme.
   73 !      W3QCK2    Subr.   Id.    Irregular grid UQ scheme.
   74 !      W3QCK3    Subr.   Id.    Regular grid UQ scheme + obstructions.
   75 !      W3UNO2    Subr. W3UNO2MD UNO2 scheme for irregular grid.
   76 !      W3UNO2r   Subr.   Id.    UNO2 scheme reduced to regular grid.
   77 !      W3UNO2s   Subr.   Id.    UNO2 regular grid with subgrid
   78 !                               obstruction.
   79 !     ----------------------------------------------------------------
   80 !
   81 !  5. Remarks :
   82 !
   83 !     - The averaging is not performed around semi-transparent grid
   84 !       points to avoid that leaking through barriers occurs.
   85 !
   86 !  6. Switches :
   87 !
   88 !       !/UQ    3rd order UQ propagation scheme.
   89 !       !/UNO   2nd order UNO propagation scheme.
   90 !
   91 !       !/C90   Cray FORTRAN 90 compiler directives.
   92 !       !/NEC   NEC SXF90 compiler directives.
   93 !
   94 !       !/MGP   Moving grid corrections.
   95 !       !/MGG   Moving grid corrections.
   96 !
   97 !       !/OMPH  Hybrid OpenMP directives.
   98 !
   99 !       !/S     Enable subroutine tracing.
  100 !       !/Tn    Enable test output.
  101 !
  102 !  7. Source code :
  103 !
  104 !/ ------------------------------------------------------------------- /
  105 !/
  106 !/ Public variables
  107 !/
  108       PUBLIC
  109 !/
  110 !/ Private data
 
 
                                                         w3pro3md.F90  page   3
 
 
  111 !/
  112       REAL, PRIVATE, PARAMETER:: TRNMIN = 0.95
  113 !/
  114       CONTAINS
  115 !/ ------------------------------------------------------------------- /
  116       SUBROUTINE W3MAP3
  117 !/
  118 !/                  +-----------------------------------+
  119 !/                  | WAVEWATCH III           NOAA/NCEP |
  120 !/                  |           H. L. Tolman            |
  121 !/                  |                        FORTRAN 90 |
  122 !/                  | Last update :         01-Apr-2008 |
  123 !/                  +-----------------------------------+
  124 !/
  125 !/    27-Feb-2000 : Origination.                        ( version 2.08 )
  126 !/    10-Dec-2001 : Sub-grid obstructions.              ( version 2.14 )
  127 !/                  (array allocation only.)
  128 !/    17-Dec-2004 : Multiple grid version.              ( version 3.06 )
  129 !/    09-Nov-2005 : Removing soft boundary option.      ( version 3.08 )
  130 !/    01-Apr-2008 : Bug fix sec. 4 MAPSTA range check.  ( version 3.13 )
  131 !/
  132 !  1. Purpose :
  133 !
  134 !     Generate 'map' arrays for the ULTIMATE QUICKEST scheme.
  135 !
  136 !  2. Method :
  137 !
  138 !     MAPX2, MAPY2, MAPTH2 and MAPWN2 contain consecutive 1-D spatial
  139 !     grid counters (e.g., IXY = (IX-1)*MY + IY). The arrays are
  140 !     devided in three parts. For MAPX2, these ranges are :
  141 !
  142 !         1    - NMX0  Counters IXY for which grid point (IX,IY) and
  143 !                      (IX+1,IY) both are active grid points.
  144 !       NMX0+1 - NMX1  Id. only (IX,IY) active.
  145 !       NMX1+1 - NMX2  Id. only (IX+1,IY) active.
  146 !
  147 !     The array MAPY2 has a similar layout varying IY instead of IX.
  148 !
  149 !  3. Parameters :
  150 !
  151 !     Parameter list
  152 !     ----------------------------------------------------------------
  153 !     ----------------------------------------------------------------
  154 !
  155 !  4. Subroutines used :
  156 !
  157 !     See module documentation.
  158 !
  159 !  5. Called by :
  160 !
  161 !      Name      Type  Module   Description
  162 !     ----------------------------------------------------------------
  163 !      W3WAVE    Subr. W3WAVEMD Wave model routine.
  164 !     ----------------------------------------------------------------
  165 !
 
 
                                                         w3pro3md.F90  page   4
 
 
  166 !  6. Error messages :
  167 !
  168 !  7. Remarks :
  169 !
  170 !  8. Structure :
  171 !
  172 !     ------------------------------------------------------
  173 !      1.   Map MAPX2
  174 !        a  Range 1 to NMX0
  175 !        b  Range NMX0+1 to NMX1
  176 !        c  Range NMX1+1 to NMX2
  177 !      2.   Map MAPY2
  178 !        a  Range 1 to NMY0
  179 !        b  Range NMY0+1 to NMY1
  180 !        c  Range NMY1+1 to NMY2
  181 !      3.   Map MAPAXY
  182 !      4.   Map MAPCXY
  183 !      5.   Maps for intra-spectral propagation
  184 !        a  MAPTH2, MAPATK
  185 !        b  MAPWN2
  186 !     ------------------------------------------------------
  187 !
  188 !  9. Switches :
  189 !
  190 !     !/S   Enable subroutine tracing.
  191 !     !/T   Enable test output.
  192 !
  193 ! 10. Source code :
  194 !/ ------------------------------------------------------------------- /
  195       USE W3GDATMD, ONLY: NK, NTH, NSPEC, NX, NY, NSEA, MAPSTA, MAPSF,&
  196                           GTYPE
  197       USE W3ADATMD, ONLY: NMX0, NMX1, NMX2, NMY0, NMY1, NMY2, NACT,   &
  198                           NCENT, MAPX2, MAPY2, MAPAXY, MAPCXY,        &
  199                           MAPTH2, MAPWN2
  200       USE W3ODATMD, ONLY: NDST
  201 !/
  202       IMPLICIT NONE
  203 !/
  204 !/ ------------------------------------------------------------------- /
  205 !/ Parameter list
  206 !/
  207 !/ ------------------------------------------------------------------- /
  208 !/ Local parameters
  209 !/
  210       INTEGER                 :: IX, IY, IXY0, IX2, IY2, IX0, IY0,    &
  211                                  ISEA, IK, ITH, ISP, ISP0, ISP2, NCENTC
  212 !/
  213 !/ ------------------------------------------------------------------- /
  214 !/
  215 !
  216    IF (GTYPE .LT. 3) THEN
  217 ! 1.  Map MAPX2 ------------------------------------------------------ *
  218 ! 1.a Range 1 to NMX0
  219 !
  220       NMX0   = 0
 
 
                                                         w3pro3md.F90  page   5
 
 
  221       DO IX=1, NX
  222         IXY0   = (IX-1)*NY
  223         IX2    = 1 + MOD(IX,NX)
  224         DO IY=2, NY-1
  225           IF ( MAPSTA(IY,IX).EQ.1 .AND. MAPSTA(IY,IX2).EQ.1 ) THEN
  226               NMX0   = NMX0 + 1
  227               MAPX2(NMX0) = IXY0 + IY
  228             END IF
  229           END DO
  230         END DO
  231 !
  232 ! 1.b Range NMX0+1 to NMX1
  233 !
  234       NMX1   = NMX0
  235       DO IX=1, NX
  236         IXY0   = (IX-1)*NY
  237         IX2    = 1 + MOD(IX,NX)
  238         DO IY=2, NY-1
  239           IF ( MAPSTA(IY,IX).EQ.1 .AND. MAPSTA(IY,IX2).NE.1 ) THEN
  240               NMX1   = NMX1 + 1
  241               MAPX2(NMX1) = IXY0 + IY
  242             END IF
  243           END DO
  244         END DO
  245 !
  246 ! 1.c Range NMX1+1 to NMX2
  247 !
  248       NMX2   = NMX1
  249       DO IX=1, NX
  250         IXY0   = (IX-1)*NY
  251         IX2    = 1 + MOD(IX,NX)
  252         DO IY=2, NY-1
  253           IF ( MAPSTA(IY,IX).NE.1 .AND. MAPSTA(IY,IX2).EQ.1 ) THEN
  254               NMX2   = NMX2 + 1
  255               MAPX2(NMX2) = IXY0 + IY
  256             END IF
  257           END DO
  258         END DO
  259 !
  260 ! 2.  Map MAPY2 ------------------------------------------------------ *
  261 ! 2.a Range 1 to NMY0
  262 !
  263       NMY0   = 0
  264       DO IX=1, NX
  265         IXY0   = (IX-1)*NY
  266         DO IY=1, NY-1
  267           IY2    = IY + 1
  268           IF ( MAPSTA(IY,IX).EQ.1 .AND. MAPSTA(IY2,IX).EQ.1 ) THEN
  269               NMY0   = NMY0 + 1
  270               MAPY2(NMY0) = IXY0 + IY
  271             END IF
  272           END DO
  273         END DO
  274 !
  275 ! 2.b Range NMY0+1 to NMY1
 
 
                                                         w3pro3md.F90  page   6
 
 
  276 !
  277       NMY1   = NMY0
  278       DO IX=1, NX
  279         IXY0   = (IX-1)*NY
  280         DO IY=1, NY-1
  281           IY2    = IY + 1
  282           IF ( MAPSTA(IY,IX).EQ.1 .AND. MAPSTA(IY2,IX).NE.1 ) THEN
  283               NMY1   = NMY1 + 1
  284               MAPY2(NMY1) = IXY0 + IY
  285             END IF
  286           END DO
  287         END DO
  288 !
  289 ! 2.c Range NMY1+1 to NMY2
  290 !
  291       NMY2   = NMY1
  292       DO IX=1, NX
  293         IXY0   = (IX-1)*NY
  294         DO IY=1, NY-1
  295           IY2    = IY + 1
  296           IF ( MAPSTA(IY,IX).NE.1 .AND. MAPSTA(IY2,IX).EQ.1 ) THEN
  297               NMY2   = NMY2 + 1
  298               MAPY2(NMY2) = IXY0 + IY
  299             END IF
  300            END DO
  301          END DO
  302 !
  303 ! 3.  Map MAPAXY ----------------------------------------------------- *
  304 !
  305       NACT   = 0
  306       DO IX=1, NX
  307         IY0    = (IX-1)*NY
  308         DO IY=2, NY-1
  309           IF ( MAPSTA(IY,IX).EQ.1 ) THEN
  310               NACT         = NACT + 1
  311               MAPAXY(NACT) = IY0 + IY
  312             END IF
  313           END DO
  314         END DO
  315 !
  316 ! 4.  Map MAPCXY ----------------------------------------------------- *
  317 !
  318       NCENT  = 0
  319       NCENTC = NSEA
  320       MAPCXY = 0
  321 !
  322       DO ISEA=1,  NSEA
  323         IX      = MAPSF(ISEA,1)
  324         IX0    = IX-1
  325         IX2    = IX+1
  326         IY      = MAPSF(ISEA,2)
  327         IY0    = IY-1
  328         IY2    = IY+1
  329         IF ( IX .EQ. NX ) IX2 = 1
  330         IF ( IX .EQ. 1 ) IX0 = NX
 
 
                                                         w3pro3md.F90  page   7
 
 
  331         IF ( MAPSTA(IY,IX).EQ.2 .OR. MAPSTA(IY,IX).LT.0 ) THEN
  332             MAPCXY(NCENTC) = ISEA
  333             NCENTC = NCENTC - 1
  334           ELSE IF ( MAPSTA(IY0,IX0).GE.1 .AND.                     &
  335                     MAPSTA(IY0,IX ).GE.1 .AND.                     &
  336                     MAPSTA(IY0,IX2).GE.1 .AND.                     &
  337                     MAPSTA(IY ,IX0).GE.1 .AND.                     &
  338                     MAPSTA(IY ,IX2).GE.1 .AND.                     &
  339                     MAPSTA(IY2,IX0).GE.1 .AND.                     &
  340                     MAPSTA(IY2,IX ).GE.1 .AND.                     &
  341                     MAPSTA(IY2,IX2).GE.1 ) THEN
  342             NCENT  = NCENT + 1
  343             MAPCXY(NCENT) = ISEA
  344           ELSE
  345             MAPCXY(NCENTC) = ISEA
  346             NCENTC = NCENTC - 1
  347           END IF
  348         END DO
  349       END IF
  350 !
  351 ! 5.  Maps for intra-spectral propagation ---------------------------- *
  352 !
  353       IF ( MAPTH2(1) .NE. 0 ) RETURN
  354 !
  355 ! 5.a MAPTH2 and MAPBTK
  356 !
  357       DO IK=1, NK
  358         DO ITH=1, NTH
  359           ISP    = ITH + (IK-1)*NTH
  360           ISP2   = (IK+1) + (ITH-1)*(NK+2)
  361           MAPTH2(ISP) = ISP2
  362           END DO
  363         END DO
  364 !
  365 ! 5.b MAPWN2
  366 !
  367       ISP0   = 0
  368       DO IK=1, NK-1
  369         DO ITH=1, NTH
  370           ISP0   = ISP0 + 1
  371           ISP2   = (IK+1) + (ITH-1)*(NK+2)
  372           MAPWN2(ISP0) = ISP2
  373           END DO
  374         END DO
  375 !
  376       DO ITH=1, NTH
  377         ISP0   = ISP0 + 1
  378         ISP2   = NK+1 + (ITH-1)*(NK+2)
  379         MAPWN2(ISP0) = ISP2
  380         END DO
  381 !
  382       DO ITH=1, NTH
  383         ISP0   = ISP0 + 1
  384         ISP2   = 1 + (ITH-1)*(NK+2)
  385         MAPWN2(ISP0) = ISP2
 
 
                                                         w3pro3md.F90  page   8
 
 
  386         END DO
  387 !
  388       RETURN
  389 !
  390 ! Formats
  391 !
  392 !/
  393 !/ End of W3MAP3 ----------------------------------------------------- /
  394 !/
  395       END SUBROUTINE W3MAP3
  396 !/ ------------------------------------------------------------------- /
  397       SUBROUTINE W3MAPT
  398 !/
  399 !/                  +-----------------------------------+
  400 !/                  | WAVEWATCH III           NOAA/NCEP |
  401 !/                  |           H. L. Tolman            |
  402 !/                  |                        FORTRAN 90 |
  403 !/                  | Last update :         17-Dec-2004 |
  404 !/                  +-----------------------------------+
  405 !/
  406 !/    10-Dec-2001 : Origination.                        ( version 2.14 )
  407 !/    17-Dec-2004 : Multiple grid version.              ( version 3.06 )
  408 !/
  409 !  1. Purpose :
  410 !
  411 !     Generate 'map' arrays for the ULTIMATE QUICKEST scheme to combine
  412 !     GSE alleviation with obstructions.
  413 !
  414 !  2. Method :
  415 !
  416 !  3. Parameters :
  417 !
  418 !     Parameter list
  419 !     ----------------------------------------------------------------
  420 !     ----------------------------------------------------------------
  421 !
  422 !  4. Subroutines used :
  423 !
  424 !     See module documentation.
  425 !
  426 !  5. Called by :
  427 !
  428 !      Name      Type  Module   Description
  429 !     ----------------------------------------------------------------
  430 !      W3WAVE    Subr. W3WAVEMD Wave model routine.
  431 !     ----------------------------------------------------------------
  432 !
  433 !  6. Error messages :
  434 !
  435 !  7. Remarks :
  436 !
  437 !  8. Structure :
  438 !
  439 !     See source code.
  440 !
 
 
                                                         w3pro3md.F90  page   9
 
 
  441 !  9. Switches :
  442 !
  443 !     !/S   Enable subroutine tracing.
  444 !
  445 ! 10. Source code :
  446 !/ ------------------------------------------------------------------- /
  447       USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSF
  448       USE W3ADATMD, ONLY: ATRNX, ATRNY, MAPTRN
  449 !/
  450       IMPLICIT NONE
  451 !/
  452 !/ ------------------------------------------------------------------- /
  453 !/ Parameter list
  454 !/
  455 !/ ------------------------------------------------------------------- /
  456 !/ Local parameters
  457 !/
  458       INTEGER                 :: ISEA, IXY
  459 !/
  460 !/ ------------------------------------------------------------------- /
  461 !/
  462 !
  463 ! 1.  Map MAPTRN ----------------------------------------------------- *
  464 !
  465       DO ISEA=1, NSEA
  466         IXY         = MAPSF(ISEA,3)
  467                                    
  468 !notes: Oct 22 2012: I changed this because it *looks* like a bug.
  469 !       I have not confirmed that it is a bug.
  470 !       Old code is given (2 lines). Only the first line is
  471 !       changed.
  472                 
  473 !old    MAPTRN(IXY) = MIN( ATRNX(IXY,1) ,ATRNY(IXY,-1) ,              &
  474 !old                       ATRNY(IXY,1), ATRNY(IXY,-1) ) .LT. TRNMIN
  475                                                                     
  476         MAPTRN(IXY) = MIN( ATRNX(IXY,1) ,ATRNX(IXY,-1) ,              &
  477                            ATRNY(IXY,1), ATRNY(IXY,-1) ) .LT. TRNMIN
  478         END DO
  479 !
  480       RETURN
  481 !
  482 ! Formats
  483 !/
  484 !/ End of W3MAPT ----------------------------------------------------- /
  485 !/
  486       END SUBROUTINE W3MAPT
  487 !/ ------------------------------------------------------------------- /
  488       SUBROUTINE W3XYP3 ( ISP, DTG, MAPSTA, MAPFS, VQ, VGX, VGY )
  489 !/
  490 !/                  +-----------------------------------+
  491 !/                  | WAVEWATCH III           NOAA/NCEP |
  492 !/                  |           H. L. Tolman            |
  493 !/                  |                        FORTRAN 90 |
  494 !/                  | Last update :         27-May-2014 |
  495 !/                  +-----------------------------------+
 
 
                                                         w3pro3md.F90  page  10
 
 
  496 !/
  497 !/    27-Feb-2000 : Origination.                        ( version 2.08 )
  498 !/    17-Sep-2000 : Clean-up.                           ( version 2.13 )
  499 !/    10-Dec-2001 : Sub-grid obstructions.              ( version 2.14 )
  500 !/    16-Oct-2002 : Change INTENT for ATRNRX/Y.         ( version 3.00 )
  501 !/    26-Dec-2002 : Moving grid version.                ( version 3.02 )
  502 !/    01-Aug-2003 : Moving grid GSE correction.         ( version 3.03 )
  503 !/    17-Dec-2004 : Multiple grid version.              ( version 3.06 )
  504 !/    07-Sep-2005 : Upgrade XY boundary conditions.     ( version 3.08 )
  505 !/    09-Nov-2005 : Removing soft boundary option.      ( version 3.08 )
  506 !/    05-Mar-2008 : Added NEC sxf90 compiler directives.
  507 !/                  (Chris Bunney, UK Met Office)       ( version 3.13 )
  508 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
  509 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  510 !/    17-Aug-2010 : Add test output.                  ( version 3.14.5 )
  511 !/    30-Oct-2010 : Implement unstructured grid         ( version 3.14 )
  512 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
  513 !/                  specify index closure for a grid.   ( version 3.14 )
  514 !/                  (T. J. Campbell, NRL)
  515 !/    01-Jul-2013 : Adding UQ and UNO switches to chose between third
  516 !/                  and second order schemes.           ( version 4.12 )
  517 !/    12-Sep-2013 : Add documentation for global clos.  ( version 4.12 )
  518 !/    27-May-2014 : Adding OMPH switch.                 ( version 5.02 )
  519 !/
  520 !  1. Purpose :
  521 !
  522 !     Propagation in phyiscal space for a given spectral component.
  523 !
  524 !  2. Method :
  525 !
  526 !     Third-order ULTIMATE QUICKEST scheme with averaging.
  527 !     Curvilinear grid implementation: Fluxes are computed in index space
  528 !       and then transformed back into physical space.  The diffusion term
  529 !       is handled in physical space.  The averaging scheme is adapted for
  530 !       curvilinear grids by applying the appropriate local rotations and
  531 !       adjustments to interpolation weights which control the strength of
  532 !       the averaging in axial directions.
  533 !
  534 !  3. Parameters :
  535 !
  536 !     Parameter list
  537 !     ----------------------------------------------------------------
  538 !       ISP     Int.   I   Number of spectral bin (IK-1)*NTH+ITH
  539 !       DTG     Real   I   Total time step.
  540 !       MAPSTA  I.A.   I   Grid point status map.
  541 !       MAPFS   I.A.   I   Storage map.
  542 !       VQ      R.A.  I/O  Field to propagate.
  543 !       VGX/Y   Real   I   Speed of grid.
  544 !     ----------------------------------------------------------------
  545 !
  546 !     Local variables.
  547 !     ----------------------------------------------------------------
  548 !       NTLOC   Int   Number of local time steps.
  549 !       DTLOC   Real  Local propagation time step.
  550 !       VCFL0X  R.A.  Local courant numbers for absolute group vel.
 
 
                                                         w3pro3md.F90  page  11
 
 
  551 !                     using local X-grid step.
  552 !       VCFL0Y  R.A.  Id. in Y.
  553 !     ----------------------------------------------------------------
  554 !
  555 !  4. Subroutines used :
  556 !
  557 !       W3QCK3   Actual propagation algorithm
  558 !
  559 !       STRACE   Service routine.
  560 !
  561 !  5. Called by :
  562 !
  563 !       W3WAVE   Wave model routine.
  564 !
  565 !  6. Error messages :
  566 !
  567 !       None.
  568 !
  569 !  7. Remarks :
  570 !
  571 !     - Note that the ULTIMATE limiter does not guarantee non-zero
  572 !       energies.
  573 !     - The present scheme shows a strong distorsion when propaga-
  574 !       ting a field under an angle with the grid in a truly 2-D
  575 !       fashion. Propagation is therefore split along the two
  576 !       axes.
  577 !     - Two boundary treatments are available. The first uses real
  578 !       boundaries in each space. In this case waves will not
  579 !       penetrate in narrow straights under an angle with the grid.
  580 !       This behavior is improved by using a 'soft' option, in
  581 !       which the 'X' or 'Y' sweep allows for energy to go onto
  582 !       the land. This improves the above behavior, but implies
  583 !       that X-Y connenctions are required in barriers for them
  584 !       to become inpenetrable.
  585 !     - Note that unlike in W3XYP2, isotropic diffusion is never
  586 !       used for growth.
  587 !     - Curvilinear grid implementation. Variables FACX, FACY, CCOS, CSIN,
  588 !       CCURX, CCURY are not needed and have been removed.  FACX is accou>
  589 !       for as approriate in this subroutine.  FACX is also accounted for>
  590 !       the case of .NOT.FLCX.  Since FACX is removed, there is now a che>
  591 !       .NOT.FLCX in this subroutine.  In CFL calcs dx and dy are omitted,
  592 !       since dx=dy=1 in index space.  Curvilinear grid derivatives
  593 !       (DPDY, DQDX, etc.) and metric (GSQRT) are brought in via W3GDATMD.
  594 !     - The strength of the averaging scheme is dependent on grid resolut>
  595 !       Since grid resolution is non-uniform for curvilinear grids, this >
  596 !       that the strength of the averaging is also non-uniform. This may >
  597 !       a desirable effect. A potential future upgrade would be to add an
  598 !       additional term/factor that balances the effect of the spatial
  599 !       variation of grid resolution.
  600 !
  601 !  8. Structure :
  602 !
  603 !     ---------------------------------------------
  604 !       1.  Preparations
  605 !         a Set constants
 
 
                                                         w3pro3md.F90  page  12
 
 
  606 !         b Initialize arrays
  607 !       2.  Prepare arrays
  608 !         a Velocities and 'Q'
  609 !       3.  Loop over sub-steps
  610 !       ----------------------------------------
  611 !         a Average
  612 !         b Propagate
  613 !         c Update boundaries.
  614 !       ----------------------------------------
  615 !       4.  Store Q field in spectra
  616 !     ---------------------------------------------
  617 !
  618 !  9. Switches :
  619 !
  620 !       !/NEC   Enable NEC SXF90 compiler directives.
  621 !
  622 !       !/S     Enable subroutine tracing.
  623 !
  624 !       !/OMPH  Hybrid OpenMP directives.
  625 !
  626 !       !/MGP   Moving grid corrections.
  627 !       !/MGG   Moving grid corrections.
  628 !
  629 !       !/T     Enable general test output.
  630 !       !/T0    Dump of precalcaulted interpolation data.
  631 !       !/T1    Dump of input field and fluxes.
  632 !       !/T2    Dump of output field (before boundary update).
  633 !       !/T3    Dump of output field (final).
  634 !
  635 ! 10. Source code :
  636 !
  637 !/ ------------------------------------------------------------------- /
  638       USE CONSTANTS
  639 !
  640       USE W3TIMEMD, ONLY: DSEC21
  641 !
  642       USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSF, DTCFL, CLATS,      &
  643                           ICLOSE, FLCX, FLCY, NK, NTH, DTH, XFR,  &
  644                           ICLOSE_NONE, ICLOSE_SMPL, ICLOSE_TRPL,  &
  645                           ECOS, ESIN, SIG, WDCG, WDTH, PFMOVE,    &
  646                           FLAGLL, DPDX, DPDY, DQDX, DQDY, GSQRT
  647       USE W3WDATMD, ONLY: TIME
  648       USE W3ADATMD, ONLY: NMX0, NMX1, NMX2, NMY0, NMY1, NMY2, NACT,   &
  649                           NCENT, MAPX2, MAPY2, MAPAXY, MAPCXY,        &
  650                           MAPTRN, CG, CX, CY, ATRNX, ATRNY, ITIME
  651       USE W3IDATMD, ONLY: FLCUR
  652       USE W3ODATMD, ONLY: NDSE, NDST, FLBPI, NBI, TBPI0, TBPIN,       &
  653                           ISBPI, BBPI0, BBPIN, IAPROC, NAPERR
  654       USE W3SERVMD, ONLY: EXTCDE
  655       USE W3UQCKMD
  656 !/
  657       IMPLICIT NONE
  658 !/
  659 !/ ------------------------------------------------------------------- /
  660 !/ Parameter list
 
 
                                                         w3pro3md.F90  page  13
 
 
  661 !/
  662       INTEGER, INTENT(IN)     :: ISP, MAPSTA(NY*NX), MAPFS(NY*NX)
  663       REAL, INTENT(IN)        :: DTG, VGX, VGY
  664       REAL, INTENT(INOUT)     :: VQ(1-NY:NY*(NX+2))
  665 !/
  666 !/ ------------------------------------------------------------------- /
  667 !/ Local parameters
  668 !/
  669       INTEGER                 :: ITH, IK, NTLOC, ITLOC, ISEA, IXY, IP
  670       INTEGER                 :: IX, IY, IXC, IYC, IBI
  671       INTEGER                 :: IIXY1(NSEA), IIXY2(NSEA),            &
  672                                  IIXY3(NSEA), IIXY4(NSEA)
  673       INTEGER                 :: TTEST(2),DTTST
  674       REAL                    :: CG0, CGA, CGN, CGX, CGY, CXC, CYC,   &
  675                                  CXMIN, CXMAX, CYMIN, CYMAX
  676       REAL                    :: CGC, FGSE = 1.
  677       REAL                    :: FTH, FTHX, FTHY, FCG, FCGX, FCGY
  678       REAL                    :: DTLOC, DTRAD,                        &
  679                                  DXCGN, DYCGN, DXCGS, DYCGS, DXCGC,   &
  680                                  DYCGC
  681       REAL                    :: RDI1(NSEA), RDI2(NSEA),              &
  682                                  RDI3(NSEA), RDI4(NSEA)
  683       REAL                    :: TMPX, TMPY, RD1, RD2, RD3, RD4
  684       LOGICAL                 :: YFIRST
  685       LOGICAL                 :: GLOBAL
  686       REAL                    :: CP, CQ
  687 !/
  688 !/ Automatic work arrays
  689 !/
  690       INTEGER                 :: MAPSTX(1-2*NY:NY*(NX+2))
  691       REAL                    :: VLCFLX((NX+1)*NY), VLCFLY((NX+1)*NY),&
  692                                  AQ(1-NY:NY*(NX+2))
  693       REAL                    :: CXTOT((NX+1)*NY), CYTOT(NX*NY)
  694 !/
  695 !/ ------------------------------------------------------------------- /
  696 !/
  697 !
  698 ! 1.  Preparations --------------------------------------------------- *
  699                                                                         
  700       IF ( ICLOSE .EQ. ICLOSE_TRPL ) THEN
  701 !/ ------------------------------------------------------------------- /
  702          IF (IAPROC .EQ. NAPERR) &
  703          WRITE(NDSE,*)'SUBROUTINE W3XYP3 IS NOT YET ADAPTED FOR '//    &
  704          'TRIPOLE GRIDS. STOPPING NOW.'
  705          CALL EXTCDE ( 1 )
  706       END IF
  707             
  708 ! 1.a Set constants
  709 !
  710       GLOBAL = ICLOSE.NE.ICLOSE_NONE
  711       ITH    = 1 + MOD(ISP-1,NTH)
  712       IK     = 1 + (ISP-1)/NTH
  713 !
  714       CG0    = 0.575 * GRAV / SIG(1)
  715       CGA    = 0.575 * GRAV / SIG(IK)
 
 
                                                         w3pro3md.F90  page  14
 
 
  716       CGX    = CGA * ECOS(ITH)
  717       CGY    = CGA * ESIN(ITH)
  718       CGC    = SQRT ( CGX**2 + CGY**2 )
  719 !
  720       IF ( FLCUR ) THEN
  721           CXMIN  = MINVAL ( CX(1:NSEA) )
  722           CXMAX  = MAXVAL ( CX(1:NSEA) )
  723           CYMIN  = MINVAL ( CY(1:NSEA) )
  724           CYMAX  = MAXVAL ( CY(1:NSEA) )
  725           IF ( ABS(CGX+CXMIN) .GT. ABS(CGX+CXMAX) ) THEN
  726               CGX    = CGX + CXMIN
  727             ELSE
  728               CGX    = CGX + CXMAX
  729             END IF
  730           IF ( ABS(CGY+CYMIN) .GT. ABS(CGY+CYMAX) ) THEN
  731               CGY    = CGY + CYMIN
  732             ELSE
  733               CGY    = CGY + CYMAX
  734             END IF
  735           CXC    = MAX ( ABS(CXMIN) , ABS(CXMAX) )
  736           CYC    = MAX ( ABS(CYMIN) , ABS(CYMAX) )
  737         ELSE
  738           CXC    = 0.
  739           CYC    = 0.
  740         END IF
  741 !
  742       CGN    = MAX ( ABS(CGX) , ABS(CGY) , CXC, CYC, 0.001*CG0 )
  743 !
  744       NTLOC  = 1 + INT(DTG/(DTCFL*CG0/CGN))
  745       DTLOC  = DTG / REAL(NTLOC)
  746       DTRAD  = DTLOC
  747       IF ( FLAGLL ) DTRAD=DTRAD/(DERA*RADIUS)
  748 !
  749       TTEST(1) = TIME(1)
  750       TTEST(2) = 0
  751       DTTST = DSEC21(TTEST,TIME)
  752       YFIRST = MOD(NINT(DTTST/DTG),2) .EQ. 0
  753 !
  754 ! 1.b Initialize arrays
  755 !
  756       VLCFLX = 0.
  757       VLCFLY = 0.
  758       CXTOT  = 0.
  759       CYTOT  = 0.
  760 !
  761       MAPSTX(1:NX*NY) = MAPSTA(1:NX*NY)
  762 !
  763       IF ( GLOBAL ) THEN
  764           MAPSTX(1-2*NY:0)            = MAPSTA((NX-2)*NY+1:NX*NY)
  765           MAPSTX(NX*NY+1:NX*NY+2*NY)  = MAPSTA(1:2*NY)
  766         ELSE
  767           MAPSTX(1-2*NY:0)            = 0
  768           MAPSTX(NX*NY+1:NX*NY+2*NY)  = 0
  769         END IF
  770 !
 
 
                                                         w3pro3md.F90  page  15
 
 
  771 ! 1.c Pre-calculate interpolation info
  772 !
  773       FTH = FGSE * WDTH * DTH * DTLOC
  774       FCG = FGSE * WDCG * 0.5 * (XFR-1./XFR) * DTLOC
  775       IF ( FLAGLL ) THEN
  776           FTH = FTH / RADIUS / DERA
  777           FCG = FCG / RADIUS / DERA
  778         END IF
  779       FCG = FCG / REAL(NTLOC) !TJC: required to match original (is this c>
  780       FTHX = - FTH * ESIN(ITH)
  781       FTHY =   FTH * ECOS(ITH)
  782       FCGX =   FCG * ECOS(ITH)
  783       FCGY =   FCG * ESIN(ITH)
  784 !
  785       DO ISEA=1, NSEA
  786 !
  787         IX  = MAPSF(ISEA,1)
  788         IY  = MAPSF(ISEA,2)
  789 !
  790 ! 1.c.1 Normal and parallel width ...
  791 !
  792         TMPX   = FTHX / CLATS(ISEA)
  793         TMPY   = FTHY
  794         DXCGN  = DPDX(IY,IX)*TMPX + DPDY(IY,IX)*TMPY
  795         DYCGN  = DQDX(IY,IX)*TMPX + DQDY(IY,IX)*TMPY
  796         TMPX   = FCGX / CLATS(ISEA)
  797         TMPY   = FCGY
  798         DXCGS  = DPDX(IY,IX)*TMPX + DPDY(IY,IX)*TMPY
  799         DYCGS  = DQDX(IY,IX)*TMPX + DQDY(IY,IX)*TMPY
  800 !
  801 ! 1.c.2 "Sum" corner (and mirror image) ...
  802 !
  803         DXCGC  = DXCGN + DXCGS
  804         DYCGC  = DYCGN + DYCGS
  805 !
  806         IXC    = NY
  807         IF ( DXCGC .LT. 0. ) IXC = - IXC
  808         IYC    = 1
  809         IF ( DYCGC .LT. 0. ) IYC = - IYC
  810 !
  811         IIXY1(ISEA) = IXC + IYC
  812         IF ( ABS(DXCGC) .GT. ABS(DYCGC) ) THEN
  813             IIXY2(ISEA) = IXC
  814             RDI1 (ISEA) = ABS(DYCGC/DXCGC)
  815             RDI2 (ISEA) = ABS(DXCGC)
  816           ELSE
  817             IIXY2(ISEA) = IYC
  818             IF ( ABS(DYCGC) .GT. 1.E-5 ) THEN
  819                 RDI1(ISEA) = ABS(DXCGC/DYCGC)
  820               ELSE
  821                 RDI1(ISEA) = 1.
  822               END IF
  823             RDI2(ISEA) = ABS(DYCGC)
  824           END IF
  825 !
 
 
                                                         w3pro3md.F90  page  16
 
 
  826 ! 1.c.2 "Difference" corner (and mirror image) ...
  827 !
  828         DXCGC  = DXCGN - DXCGS
  829         DYCGC  = DYCGN - DYCGS
  830 !
  831         IXC    = NY
  832         IF ( DXCGC .LT. 0. ) IXC = - IXC
  833         IYC    = 1
  834         IF ( DYCGC .LT. 0. ) IYC = - IYC
  835 !
  836         IIXY3(ISEA) = IXC + IYC
  837         IF ( ABS(DXCGC) .GT. ABS(DYCGC) ) THEN
  838             IIXY4(ISEA) = IXC
  839             RDI3 (ISEA) = ABS(DYCGC/DXCGC)
  840             RDI4 (ISEA) = ABS(DXCGC)
  841           ELSE
  842             IIXY4(ISEA) = IYC
  843             IF ( ABS(DYCGC) .GT. 1.E-5 ) THEN
  844                 RDI3(ISEA) = ABS(DXCGC/DYCGC)
  845               ELSE
  846                 RDI3(ISEA) = 1.
  847               END IF
  848             RDI4(ISEA) = ABS(DYCGC)
  849           END IF
  850 !
  851         END DO
  852 !
  853 ! 2.  Calculate velocities and diffusion coefficients ---------------- *
  854 ! 2.a Velocities
  855 !
  856 !     Q     = ( A / CG * CLATS )
  857 !     LCFLX = ( COS*CG / CLATS ) * DT / DX
  858 !     LCFLY = (     SIN*CG )     * DT / DY
  859 !
  860       DO ISEA=1, NSEA
  861         IXY         = MAPSF(ISEA,3)
  862         VQ    (IXY) = VQ(IXY) / CG(IK,ISEA) * CLATS(ISEA)
  863         CXTOT(IXY) = ECOS(ITH) * CG(IK,ISEA) / CLATS(ISEA)
  864         CYTOT(IXY) = ESIN(ITH) * CG(IK,ISEA)
  865         END DO
  866 !
  867       IF ( FLCUR ) THEN
  868 !
  869           DO ISEA=1, NSEA
  870             IXY         = MAPSF(ISEA,3)
  871             CXTOT(IXY) = CXTOT(IXY) + CX(ISEA)/CLATS(ISEA)
  872             CYTOT(IXY) = CYTOT(IXY) + CY(ISEA)
  873            END DO
  874 !
  875         END IF
  876 !
  877       DO ISEA=1, NSEA
  878          IX     = MAPSF(ISEA,1)
  879          IY     = MAPSF(ISEA,2)
  880          IXY    = MAPSF(ISEA,3)
 
 
                                                         w3pro3md.F90  page  17
 
 
  881          CP = CXTOT(IXY)*DPDX(IY,IX) + CYTOT(IXY)*DPDY(IY,IX)
  882          CQ = CXTOT(IXY)*DQDX(IY,IX) + CYTOT(IXY)*DQDY(IY,IX)
  883          VLCFLX(IXY) = CP*DTRAD
  884          VLCFLY(IXY) = CQ*DTRAD
  885       END DO
  886 !
  887 ! 3.  Loop over sub-steps -------------------------------------------- *
  888 !
  889       DO ITLOC=1, NTLOC
  890 !
  891 ! 3.a Average
  892 !
  893         AQ     = VQ
  894         VQ     = 0.
  895 !
  896 ! 3.a.1 Central points
  897 !
  898         DO IP=1, NCENT
  899           ISEA    = MAPCXY(IP)
  900           IXY     = MAPSF(ISEA,3)
  901           IF ( MAPTRN(IXY) ) THEN
  902               VQ(IXY) = AQ(IXY)
  903             ELSE
  904               RD1     = RDI1(ISEA)
  905               RD2     = MIN ( 1. , RDI2(ISEA) * CG(IK,ISEA) )
  906               RD3     = RDI3(ISEA)
  907               RD4     = MIN ( 1. , RDI4(ISEA) * CG(IK,ISEA) )
  908               VQ(IXY          ) = VQ(IXY          )                   &
  909                                    + AQ(IXY) * (3.-RD2-RD4)/3.
  910               VQ(IXY+IIXY1(ISEA)) = VQ(IXY+IIXY1(ISEA))               &
  911                                    + AQ(IXY) * RD2*RD1/6.
  912               VQ(IXY+IIXY2(ISEA)) = VQ(IXY+IIXY2(ISEA))               &
  913                                    + AQ(IXY) * (1.-RD1)*RD2/6.
  914               VQ(IXY+IIXY3(ISEA)) = VQ(IXY+IIXY3(ISEA))               &
  915                                    + AQ(IXY) * RD4*RD3/6.
  916               VQ(IXY+IIXY4(ISEA)) = VQ(IXY+IIXY4(ISEA))               &
  917                                    + AQ(IXY) * (1.-RD3)*RD4/6.
  918               VQ(IXY-IIXY1(ISEA)) = VQ(IXY-IIXY1(ISEA))               &
  919                                    + AQ(IXY) * RD2*RD1/6.
  920               VQ(IXY-IIXY2(ISEA)) = VQ(IXY-IIXY2(ISEA))               &
  921                                    + AQ(IXY) * (1.-RD1)*RD2/6.
  922               VQ(IXY-IIXY3(ISEA)) = VQ(IXY-IIXY3(ISEA))               &
  923                                    + AQ(IXY) * RD4*RD3/6.
  924               VQ(IXY-IIXY4(ISEA)) = VQ(IXY-IIXY4(ISEA))               &
  925                                    + AQ(IXY) * (1.-RD3)*RD4/6.
  926             END IF
  927           END DO
  928 !
  929 ! 3.a.2 Near-coast points
  930 !
  931         DO IP=NCENT+1, NSEA
  932           ISEA    = MAPCXY(IP)
  933           IX      = MAPSF(ISEA,1)
  934           IXY     = MAPSF(ISEA,3)
  935           IF ( MAPSTA(IXY) .LE. 0 ) CYCLE
 
 
                                                         w3pro3md.F90  page  18
 
 
  936           IF ( MAPTRN(IXY) ) THEN
  937               VQ(IXY) = AQ(IXY)
  938             ELSE
  939               RD1     = RDI1(ISEA)
  940               RD3     = RDI3(ISEA)
  941               RD2     = MIN ( 1. , RDI2(ISEA) * CG(IK,ISEA) )
  942               RD4     = MIN ( 1. , RDI4(ISEA) * CG(IK,ISEA) )
  943               VQ(IXY          ) = VQ(IXY          )                   &
  944                                    + AQ(IXY) * (3.-RD2-RD4)/3.
  945 !
  946               IXC    = SIGN(NY,IIXY1(ISEA))
  947               IYC    = IIXY1(ISEA) - IXC
  948               IF ( MAPSTX(IXY+IIXY1(ISEA)) .GE. 1 .AND.               &
  949                    .NOT. ( MAPSTX(IXY+IXC).LE.0 .AND.                 &
  950                            MAPSTX(IXY+IYC).LE.0 ) ) THEN
  951                        VQ(IXY+IIXY1(ISEA)) = VQ(IXY+IIXY1(ISEA))      &
  952                                             + AQ(IXY) * RD2*RD1/6.
  953                 ELSE
  954                        VQ(IXY          ) = VQ(IXY          )          &
  955                                             + AQ(IXY) * RD2*RD1/6.
  956                 END IF
  957               IF ( MAPSTX(IXY-IIXY1(ISEA)) .GE. 1 .AND.               &
  958                    .NOT. ( MAPSTX(IXY-IXC).LE.0 .AND.                 &
  959                            MAPSTX(IXY-IYC).LE.0 ) ) THEN
  960                        VQ(IXY-IIXY1(ISEA)) = VQ(IXY-IIXY1(ISEA))      &
  961                                             + AQ(IXY) * RD2*RD1/6.
  962                 ELSE
  963                        VQ(IXY          ) = VQ(IXY          )          &
  964                                             + AQ(IXY) * RD2*RD1/6.
  965                 END IF
  966                       
  967               IF ( MAPSTX(IXY+IIXY2(ISEA)) .GE. 1 ) THEN
  968                        VQ(IXY+IIXY2(ISEA)) = VQ(IXY+IIXY2(ISEA))      &
  969                                             + AQ(IXY) * (1.-RD1)*RD2/6.
  970                 ELSE
  971                        VQ(IXY          ) = VQ(IXY          )          &
  972                                             + AQ(IXY) * (1.-RD1)*RD2/6.
  973                 END IF
  974               IF ( MAPSTX(IXY-IIXY2(ISEA)) .GE. 1 ) THEN
  975                        VQ(IXY-IIXY2(ISEA)) = VQ(IXY-IIXY2(ISEA))      &
  976                                             + AQ(IXY) * (1.-RD1)*RD2/6.
  977                 ELSE
  978                        VQ(IXY          ) = VQ(IXY          )          &
  979                                             + AQ(IXY) * (1.-RD1)*RD2/6.
  980                 END IF
  981 !
  982               IXC    = SIGN(NY,IIXY3(ISEA))
  983               IYC    = IIXY3(ISEA) - IXC
  984               IF ( MAPSTX(IXY+IIXY3(ISEA)) .GE. 1 .AND.               &
  985                    .NOT. ( MAPSTX(IXY+IXC).LE.0 .AND.                 &
  986                            MAPSTX(IXY+IYC).LE.0 ) ) THEN
  987                        VQ(IXY+IIXY3(ISEA)) = VQ(IXY+IIXY3(ISEA))      &
  988                                             + AQ(IXY) * RD4*RD3/6.
  989                 ELSE
  990                        VQ(IXY          ) = VQ(IXY          )          &
 
 
                                                         w3pro3md.F90  page  19
 
 
  991                                             + AQ(IXY) * RD4*RD3/6.
  992                 END IF
  993               IF ( MAPSTX(IXY-IIXY3(ISEA)) .GE. 1 .AND.               &
  994                    .NOT. ( MAPSTX(IXY-IXC).LE.0 .AND.                 &
  995                            MAPSTX(IXY-IYC).LE.0 ) ) THEN
  996                        VQ(IXY-IIXY3(ISEA)) = VQ(IXY-IIXY3(ISEA))      &
  997                                             + AQ(IXY) * RD4*RD3/6.
  998                 ELSE
  999                        VQ(IXY          ) = VQ(IXY          )          &
 1000                                             + AQ(IXY) * RD4*RD3/6.
 1001                 END IF
 1002 !
 1003               IF ( MAPSTX(IXY+IIXY4(ISEA)) .GE. 1 ) THEN
 1004                        VQ(IXY+IIXY4(ISEA)) = VQ(IXY+IIXY4(ISEA))      &
 1005                                             + AQ(IXY) * (1.-RD3)*RD4/6.
 1006                 ELSE
 1007                        VQ(IXY          ) = VQ(IXY          )          &
 1008                                             + AQ(IXY) * (1.-RD3)*RD4/6.
 1009                 END IF
 1010               IF ( MAPSTX(IXY-IIXY4(ISEA)) .GE. 1 ) THEN
 1011                        VQ(IXY-IIXY4(ISEA)) = VQ(IXY-IIXY4(ISEA))      &
 1012                                             + AQ(IXY) * (1.-RD3)*RD4/6.
 1013                 ELSE
 1014                        VQ(IXY          ) = VQ(IXY          )          &
 1015                                             + AQ(IXY) * (1.-RD3)*RD4/6.
 1016                 END IF
 1017 !
 1018             END IF
 1019 !
 1020           END DO
 1021 !
 1022 ! 3.a.3 Restore boundary data
 1023 !
 1024         DO IXY=1, NX*NY
 1025           IF ( MAPSTA(IXY).EQ.2 ) VQ(IXY) = AQ(IXY)
 1026           END DO
 1027 !
 1028 ! 3.a.4 Global closure (averaging only, propagation is closed in W3QCK3).
 1029 !
 1030         IF ( GLOBAL ) THEN
 1031             DO IY=1, NY
 1032               VQ(IY          ) = VQ(IY          ) + VQ(NX*NY+IY)
 1033               VQ((NX-1)*NY+IY) = VQ((NX-1)*NY+IY) + VQ(IY-NY)
 1034               END DO
 1035           END IF
 1036 !
 1037 ! 3.b Propagate fields
 1038 !
 1039 !     Transform VQ to straightened space
 1040 !
 1041         DO ISEA=1, NSEA
 1042           IX     = MAPSF(ISEA,1)
 1043           IY     = MAPSF(ISEA,2)
 1044           IXY    = MAPSF(ISEA,3)
 1045           VQ(IXY)= VQ(IXY) * GSQRT(IY,IX)
 
 
                                                         w3pro3md.F90  page  20
 
 
 1046           END DO
 1047 !
 1048          IF ( YFIRST ) THEN
 1049 !
 1050             IF ( FLCY ) CALL W3QCK3                               &
 1051                         (NX, NY, NX, NY, VLCFLY, ATRNY, VQ,       &
 1052                          .FALSE., 1, MAPAXY, NACT, MAPY2, NMY0,   &
 1053                          NMY1, NMY2, NDSE, NDST )
 1054             IF ( FLCX ) CALL W3QCK3                               &
 1055                         (NX, NY, NX, NY, VLCFLX, ATRNX, VQ,       &
 1056                          GLOBAL, NY, MAPAXY, NACT, MAPX2, NMX0,   &
 1057                          NMX1, NMX2, NDSE, NDST )
 1058 !
 1059           ELSE
 1060 !
 1061             IF ( FLCX ) CALL W3QCK3                               &
 1062                         (NX, NY, NX, NY, VLCFLX, ATRNX, VQ,       &
 1063                          GLOBAL, NY, MAPAXY, NACT, MAPX2, NMX0,   &
 1064                          NMX1, NMX2, NDSE, NDST )
 1065             IF ( FLCY ) CALL W3QCK3                               &
 1066                         (NX, NY, NX, NY, VLCFLY, ATRNY, VQ,       &
 1067                          .FALSE., 1, MAPAXY, NACT, MAPY2, NMY0,   &
 1068                          NMY1, NMY2, NDSE, NDST )
 1069 !
 1070           END IF
 1071 !
 1072 !     Transform VQ back to normal space
 1073 !
 1074         DO ISEA=1, NSEA
 1075           IX     = MAPSF(ISEA,1)
 1076           IY     = MAPSF(ISEA,2)
 1077           IXY    = MAPSF(ISEA,3)
 1078           VQ(IXY)= VQ(IXY) / GSQRT(IY,IX)
 1079           END DO
 1080 !
 1081 ! 3.c Update boundaries
 1082 !
 1083         IF ( FLBPI ) THEN
 1084             RD1    = DSEC21 ( TBPI0, TIME ) - DTG *                   &
 1085                                       REAL(NTLOC-ITLOC)/REAL(NTLOC)
 1086             RD2    = DSEC21 ( TBPI0, TBPIN )
 1087             IF ( RD2 .GT. 0.001 ) THEN
 1088                  RD2    = MIN(1.,MAX(0.,RD1/RD2))
 1089                  RD1    = 1. - RD2
 1090               ELSE
 1091                  RD1    = 0.
 1092                  RD2    = 1.
 1093               END IF
 1094             DO IBI=1, NBI
 1095               IXY     = MAPSF(ISBPI(IBI),3)
 1096               VQ(IXY) = ( RD1*BBPI0(ISP,IBI) + RD2*BBPIN(ISP,IBI) )   &
 1097                           / CG(IK,ISBPI(IBI)) * CLATS(ISBPI(IBI))
 1098               END DO
 1099           END IF
 1100 !
 
 
                                                         w3pro3md.F90  page  21
 
 
 1101         YFIRST = .NOT. YFIRST
 1102         END DO
 1103 !
 1104 ! 4.  Store results in VQ in proper format --------------------------- *
 1105 !
 1106       DO ISEA=1, NSEA
 1107         IXY    = MAPSF(ISEA,3)
 1108         IF ( MAPSTA(IXY) .GT. 0 ) THEN
 1109             VQ(IXY) =  MAX ( 0. , CG(IK,ISEA)/CLATS(ISEA)*VQ(IXY) )
 1110           END IF
 1111         END DO
 1112 !
 1113       RETURN
 1114 !
 1115 ! Formats
 1116 !
 1117 !/
 1118 !/ End of W3XYP3 ----------------------------------------------------- /
 1119 !/
 1120       END SUBROUTINE W3XYP3
 1121 !/ ------------------------------------------------------------------- /
 1122       SUBROUTINE W3KTP3 ( ISEA, FACTH, FACK, CTHG0, CG, WN, DW,       &
 1123                           DDDX, DDDY, CX, CY, DCXDX, DCXDY,           &
 1124                           DCYDX, DCYDY, DCDX, DCDY, VA, CFLTHMAX, CFLKMAX>
 1125 !/
 1126 !/    *** THIS ROUTINE SHOULD BE IDENTICAL TO W3KTP2 ***
 1127 !/
 1128 !/                  +-----------------------------------+
 1129 !/                  | WAVEWATCH III           NOAA/NCEP |
 1130 !/                  |           H. L. Tolman            |
 1131 !/                  |                        FORTRAN 90 |
 1132 !/                  | Last update :         01-Jul-2013 |
 1133 !/                  +-----------------------------------+
 1134 !/
 1135 !/    14-Feb-2000 : Origination.                        ( version 2.08 )
 1136 !/    17-Dec-2004 : Multiple grid version.              ( version 3.06 )
 1137 !/    06-Mar-2011 : Output of maximum CFL  (F. Ardhuin) ( version 3.14 )
 1138 !/    24-Aug-2011 : Limiter on k advection (F. Ardhuin) ( version 4.04 )
 1139 !/    25-Aug-2011 : DEPTH  = MAX ( DMIN, DW(ISEA) )     ( version 4.04 )
 1140 !/    26-Dec-2012 : More initializations.               ( version 4.11 )
 1141 !/    01-Jul-2013 : Adding UQ and UNO switches to chose between third
 1142 !/                  and second order schemes.           ( version 4.12 )
 1143 !/
 1144 !  1. Purpose :
 1145 !
 1146 !     Propagation in spectral space.
 1147 !
 1148 !  2. Method :
 1149 !
 1150 !     Third order QUICKEST scheme with ULTIMATE limiter.
 1151 !
 1152 !     As with the spatial propagation, the two spaces are considered
 1153 !     independently, but the propagation is performed in a 2-D space.
 1154 !     Compared to the propagation in physical space, the directions
 1155 !     rerpesent a closed space and are therefore comparable to the
 
 
                                                         w3pro3md.F90  page  22
 
 
 1156 !     longitudinal or 'X' propagation. The wavenumber space has to be
 1157 !     extended to allow for boundary treatment. Using a simple first
 1158 !     order boundary treatment at both sided, two points need to
 1159 !     be added. This implies that the spectrum needs to be extended,
 1160 !     shifted and rotated, as is performed using MAPTH2 as set
 1161 !     in W3MAP3.
 1162 !
 1163 !  3. Parameters :
 1164 !
 1165 !     Parameter list
 1166 !     ----------------------------------------------------------------
 1167 !       ISEA    Int.   I   Number of sea point.
 1168 !       FACTH/K Real   I   Factor in propagation velocity.
 1169 !       CTHG0   Real   I   Factor in great circle refracftion term.
 1170 !       MAPxx2  I.A.   I   Propagation and storage maps.
 1171 !       CG      R.A.   I   Local group velocities.
 1172 !       WN      R.A.   I   Local wavenumbers.
 1173 !       DW      R.A.   I   Depth.
 1174 !       DDDx    Real   I   Depth gradients.
 1175 !       CX/Y    Real   I   Current components.
 1176 !       DCxDx   Real   I   Current gradients.
 1177 !       DCDX-Y  Real   I   Phase speed gradients.
 1178 !       VA      R.A.  I/O  Spectrum.
 1179 !     ----------------------------------------------------------------
 1180 !
 1181 !     Local variables.
 1182 !     ----------------------------------------------------------------
 1183 !       DSDD    R.A.  Partial derivative of sigma for depth.
 1184 !       FDD, FDU, FDG, FCD, FCU
 1185 !               R.A.  Directionally varying part of depth, current and
 1186 !                     great-circle refraction terms and of consit.
 1187 !                     of Ck term.
 1188 !       CFLT-K  R.A.  Propagation velocities of local fluxes.
 1189 !       DB      R.A.  Wavenumber band widths at cell centers.
 1190 !       DM      R.A.  Wavenumber band widths between cell centers and
 1191 !                     next cell center.
 1192 !       Q       R.A.  Extracted spectrum
 1193 !     ----------------------------------------------------------------
 1194 !
 1195 !  4. Subroutines used :
 1196 !
 1197 !       W3QCK1   Actual propagation routine.
 1198 !       W3QCK2   Actual propagation routine.
 1199 !       STRACE   Service routine.
 1200 !
 1201 !  5. Called by :
 1202 !
 1203 !       W3WAVE   Wave model routine.
 1204 !
 1205 !  6. Error messages :
 1206 !
 1207 !       None.
 1208 !
 1209 !  8. Structure :
 1210 !
 
 
                                                         w3pro3md.F90  page  23
 
 
 1211 !     -----------------------------------------------------------------
 1212 !       1.  Preparations
 1213 !         a Initialize arrays
 1214 !         b Set constants and counters
 1215 !       2.  Point  preparations
 1216 !         a Calculate DSDD
 1217 !         b Extract spectrum
 1218 !       3.  Refraction velocities
 1219 !         a Filter level depth reffraction.
 1220 !         b Depth refratcion velocity.
 1221 !         c Current refraction velocity.
 1222 !       4.  Wavenumber shift velocities
 1223 !         a Prepare directional arrays
 1224 !         b Calcuate velocity.
 1225 !       5.  Propagate.
 1226 !       6.  Store results.
 1227 !     -----------------------------------------------------------------
 1228 !
 1229 !  9. Switches :
 1230 !
 1231 !       !/S     Enable subroutine tracing.
 1232 !       !/T     Enable general test output.
 1233 !
 1234 ! 10. Source code :
 1235 !
 1236 !/ ------------------------------------------------------------------- /
 1237       USE CONSTANTS
 1238       USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
 1239                           EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
 1240                           CTMAX, DMIN
 1241       USE W3ADATMD, ONLY: MAPTH2, MAPWN2, ITIME
 1242       USE W3IDATMD, ONLY: FLCUR
 1243       USE W3ODATMD, ONLY: NDSE, NDST
 1244       USE W3UQCKMD
 1245 !/
 1246       IMPLICIT NONE
 1247 !/
 1248 !/ ------------------------------------------------------------------- /
 1249 !/ Parameter list
 1250 !/
 1251       INTEGER, INTENT(IN)     :: ISEA
 1252       REAL, INTENT(IN)        :: FACTH, FACK, CTHG0, CG(0:NK+1),      &
 1253                                  WN(0:NK+1), DW, DDDX, DDDY,       &
 1254                                  CX, CY, DCXDX, DCXDY, DCYDX, DCYDY
 1255       REAL, INTENT(IN)        :: DCDX(0:NK+1), DCDY(0:NK+1)
 1256       REAL, INTENT(INOUT)     :: VA(NSPEC)
 1257       REAL, INTENT(OUT)       :: CFLTHMAX, CFLKMAX
 1258 !/
 1259 !/ ------------------------------------------------------------------- /
 1260 !/ Local parameters
 1261 !/
 1262       INTEGER                 :: ITH, IK, ISP
 1263       REAL                    :: FDDMAX, FDG, FKD, FKD0, DCYX,        &
 1264                                  DCXXYY, DCXY, DCXX, DCXYYX, DCYY,    &
 1265                                  VELNOFILT, VELFAC, DEPTH
 
 
                                                         w3pro3md.F90  page  24
 
 
 1266       REAL                    :: DSDD(0:NK+1), FRK(NK), FRG(NK),      &
 1267                                  FKC(NTH), VQ(-NK-1:NK2*(NTH+2)),     &
 1268                                  DB(NK2,NTH+1), DM(NK2,0:NTH+1),      &
 1269                                  VCFLT(NK2*(NTH+1)), CFLK(NK2,NTH)
 1270 !/
 1271 !/ ------------------------------------------------------------------- /
 1272 !/
 1273 !
 1274 ! 1.  Preparations --------------------------------------------------- *
 1275 ! 1.a Initialize arrays
 1276 !
 1277      DEPTH  = MAX ( DMIN, DW )
 1278      VQ       = 0.
 1279      IF ( FLCTH ) VCFLT    = 0.
 1280      IF ( FLCK  ) CFLK     = 0.
 1281      CFLTHMAX = 0.
 1282      CFLKMAX  = 0.
 1283 !
 1284 ! 2.  Preparation for point ------------------------------------------ *
 1285 ! 2.a Array with partial derivative of sigma versus depth
 1286 !
 1287       DO IK=0, NK+1
 1288         IF ( DEPTH*WN(IK) .LT. 5. ) THEN
 1289             DSDD(IK) = MAX ( 0. ,                                     &
 1290                 CG(IK)*WN(IK)-0.5*SIG(IK) ) / DEPTH
 1291           ELSE
 1292             DSDD(IK) = 0.
 1293           END IF
 1294         END DO
 1295 !
 1296 ! 2.b Extract spectrum
 1297 !
 1298       DO ISP=1, NSPEC
 1299         VQ(MAPTH2(ISP)) = VA(ISP)
 1300         END DO
 1301 !
 1302 ! 3.  Refraction velocities ------------------------------------------ *
 1303 !
 1304       IF ( FLCTH ) THEN
 1305 !
 1306 ! 3.a Set slope filter for depth refraction
 1307 !
 1308 ! N.B.:  FACTH = DTG / DTH / REAL(NTLOC)  (value set in w3wavemd)
 1309 !        namely, FACTH*VC=1 corresponds to CFL=1
 1310 !
 1311           FDDMAX = 0.
 1312           FDG    = FACTH * CTHG0
 1313 !
 1314           DO ITH=1, NTH/2
 1315             FDDMAX = MAX(FDDMAX,ABS(ESIN(ITH)*DDDX-ECOS(ITH)*DDDY))
 1316             END DO
 1317 !
 1318           DO IK=1, NK
 1319             FRK(IK) = FACTH * DSDD(IK) / WN(IK)
 1320 !
 
 
                                                         w3pro3md.F90  page  25
 
 
 1321 ! Removes the filtering that was done at that stage (F. Ardhuin 2011/03/0>
 1322 !
 1323 !            FRK(IK) = FRK(IK) / MAX ( 1. , FRK(IK)*FDDMAX/CTMAX )
 1324             FRG(IK) = FDG * CG(IK)
 1325             END DO
 1326 !
 1327 ! 3.b Current refraction
 1328 !
 1329           IF ( FLCUR ) THEN
 1330 !
 1331               DCYX   = FACTH *   DCYDX
 1332               DCXXYY = FACTH * ( DCXDX - DCYDY )
 1333               DCXY   = FACTH *   DCXDY
 1334 !
 1335               DO ISP=1, NSPEC
 1336                 VCFLT(MAPTH2(ISP)) = ES2(ISP)*DCYX  +     &
 1337                                      ESC(ISP)*DCXXYY - EC2(ISP)*DCXY
 1338                 END DO
 1339 !
 1340             ELSE
 1341               VCFLT(:)=0.
 1342             END IF
 1343 !
 1344 ! 3.c Depth refraction and great-circle propagation
 1345 !
 1346           DO ISP=1, NSPEC
 1347             VELNOFILT = VCFLT(MAPTH2(ISP))       &
 1348                + FRG(MAPWN(ISP)) * ECOS(ISP)              &
 1349                + FRK(MAPWN(ISP)) * ( ESIN(ISP)*DDDX - ECOS(ISP)*DDDY )
 1350 !
 1351             CFLTHMAX = MAX(CFLTHMAX, ABS(VELNOFILT))
 1352 !
 1353 ! Puts filtering on total velocity (including currents and great circle e>
 1354 ! the filtering limits VCFLT to be less than CTMAX
 1355 ! this modification was proposed by F. Ardhuin 2011/03/06
 1356 !
 1357             VCFLT(MAPTH2(ISP))=SIGN(MIN(ABS(VELNOFILT),CTMAX),VELNOFILT)
 1358             END DO
 1359         END IF
 1360 !
 1361 ! 4.  Wavenumber shift velocities ------------------------------------ *
 1362 ! N.B.:  FACK = DTG / REAL(NTLOC)  (value set in w3wavemd)
 1363 !        namely, FACK*VC/DK=1 corresponds to CFL=1
 1364 !
 1365       IF ( FLCK ) THEN
 1366 !
 1367 ! 4.a Directionally dependent part
 1368 !
 1369           DCXX   =  -   DCXDX
 1370           DCXYYX =  - ( DCXDY + DCYDX )
 1371           DCYY   =  -   DCYDY
 1372           FKD    =    ( CX*DDDX + CY*DDDY )
 1373 !
 1374           DO ITH=1, NTH
 1375             FKC(ITH) = EC2(ITH)*DCXX +                                &
 
 
                                                         w3pro3md.F90  page  26
 
 
 1376                        ESC(ITH)*DCXYYX + ES2(ITH)*DCYY
 1377             END DO
 1378 !
 1379 ! 4.b Band widths
 1380 !
 1381           DO IK=0, NK
 1382             DB(IK+1,1) = DSIP(IK) / CG(IK)
 1383             DM(IK+1,1) = WN(IK+1) - WN(IK)
 1384             END DO
 1385           DB(NK+2,1) = DSIP(NK+1) / CG(NK+1)
 1386           DM(NK+2,1) = 0.
 1387 !
 1388           DO ITH=2, NTH
 1389             DO IK=1, NK+2
 1390               DB(IK,ITH) = DB(IK,1)
 1391               DM(IK,ITH) = DM(IK,1)
 1392               END DO
 1393             END DO
 1394 !
 1395 ! 4.c Velocities
 1396 !
 1397           DO IK=0, NK+1
 1398             FKD0   = FKD / CG(IK) * DSDD(IK)
 1399             VELFAC =  FACK/DB(IK+1,1)
 1400             DO ITH=1, NTH
 1401 !
 1402 ! Puts filtering on velocity (needs the band widths)
 1403 !
 1404               VELNOFILT = ( FKD0 + WN(IK)*FKC(ITH) ) * VELFAC   ! this is>
 1405               CFLKMAX = MAX(CFLKMAX, ABS(VELNOFILT))
 1406               CFLK(IK+1,ITH) = SIGN(MIN(ABS(VELNOFILT),CTMAX),VELNOFILT)/>
 1407               !CFLK(IK+1,ITH) = FKD0 + WN(IK)*FKC(ITH)          ! this wa>
 1408               END DO
 1409             END DO
 1410 !
 1411         END IF
 1412 !
 1413 ! 5.  Propagate ------------------------------------------------------ *
 1414 !
 1415       IF ( MOD(ITIME,2) .EQ. 0 ) THEN
 1416         IF ( FLCK ) THEN
 1417           DO ITH=1, NTH
 1418                 VQ(NK+2+(ITH-1)*NK2) = FACHFA * VQ(NK+1+(ITH-1)*NK2)
 1419                 END DO
 1420 !
 1421               CALL W3QCK2 ( NTH, NK2, NTH, NK2, CFLK, FACK, DB, DM,   &
 1422                             VQ, .FALSE., 1, MAPTH2, NSPEC,            &
 1423                             MAPWN2, NSPEC-NTH, NSPEC, NSPEC+NTH,      &
 1424                             NDSE, NDST )
 1425 !
 1426             END IF
 1427           IF ( FLCTH ) THEN
 1428 !
 1429               CALL W3QCK1 ( NTH, NK2, NTH, NK2, VCFLT, VQ, .TRUE.,    &
 1430                             NK2, MAPTH2, NSPEC, MAPTH2, NSPEC, NSPEC, &
 
 
                                                         w3pro3md.F90  page  27
 
 
 1431                             NSPEC, NDSE, NDST )
 1432 !
 1433             END IF
 1434         ELSE
 1435           IF ( FLCTH ) THEN
 1436 !
 1437               CALL W3QCK1 ( NTH, NK2, NTH, NK2, VCFLT, VQ, .TRUE.,    &
 1438                             NK2, MAPTH2, NSPEC, MAPTH2, NSPEC, NSPEC, &
 1439                             NSPEC, NDSE, NDST )
 1440 !
 1441             END IF
 1442           IF ( FLCK ) THEN
 1443             DO ITH=1, NTH
 1444                 VQ(NK+2+(ITH-1)*NK2) = FACHFA * VQ(NK+1+(ITH-1)*NK2)
 1445                 END DO
 1446 !
 1447               CALL W3QCK2 ( NTH, NK2, NTH, NK2, CFLK, FACK, DB, DM,   &
 1448                             VQ, .FALSE., 1, MAPTH2, NSPEC,            &
 1449                             MAPWN2, NSPEC-NTH, NSPEC, NSPEC+NTH,      &
 1450                             NDSE, NDST )
 1451 !
 1452           END IF
 1453         END IF
 1454 !
 1455 ! 6.  Store reults --------------------------------------------------- *
 1456 !
 1457       DO ISP=1, NSPEC
 1458         VA(ISP) = VQ(MAPTH2(ISP))
 1459         END DO
 1460 !
 1461       RETURN
 1462 !
 1463 ! Formats
 1464 !
 1465 !/
 1466 !/ End of W3KTP3 ----------------------------------------------------- /
 1467 !/
 1468       END SUBROUTINE W3KTP3
 1469 !/ ------------------------------------------------------------------- /
 1470       SUBROUTINE W3CFLXY ( ISEA, DTG, MAPSTA, MAPFS, CFLXYMAX, VGX, VGY )
 1471 !/
 1472 !/                  +-----------------------------------+
 1473 !/                  | WAVEWATCH III           NOAA/NCEP |
 1474 !/                  |           F. Ardhuin            |
 1475 !/                  |                        FORTRAN 90 |
 1476 !/                  | Last update :         31-Oct-2010 |
 1477 !/                  +-----------------------------------+
 1478 !/
 1479 !/    07-Mar-2011 : Origination.                        ( version 3.14 )
 1480 !/
 1481 !  1. Purpose :
 1482 !
 1483 !     Computes the maximum CFL number for spatial advection. Used for dia>
 1484 !     purposes. (Could be used to define a local time step ...)
 1485 !
 
 
                                                         w3pro3md.F90  page  28
 
 
 1486 !  2. Method :
 1487 !
 1488 !  3. Parameters :
 1489 !
 1490 !     Parameter list
 1491 !     ----------------------------------------------------------------
 1492 !       ISEA    Int.   I   Index of grid point.
 1493 !       DTG     Real   I   Total time step.
 1494 !       MAPSTA  I.A.   I   Grid point status map.
 1495 !       MAPFS   I.A.   I   Storage map.
 1496 !       CFLXYMAX Real  O   Maximum CFL number for XY propagation.
 1497 !       VGX/Y   Real   I   Speed of grid.
 1498 !     ----------------------------------------------------------------
 1499 !
 1500 !     Local variables.
 1501 !     ----------------------------------------------------------------
 1502 !       NTLOC   Int   Number of local time steps.
 1503 !       DTLOC   Real  Local propagation time step.
 1504 !       VCFL0X  R.A.  Local courant numbers for absolute group vel.
 1505 !                     using local X-grid step.
 1506 !       VCFL0Y  R.A.  Id. in Y.
 1507 !     ----------------------------------------------------------------
 1508 !
 1509 !  4. Subroutines used :
 1510 !
 1511 !       STRACE   Service routine.
 1512 !
 1513 !  5. Called by :
 1514 !
 1515 !       W3WAVE   Wave model routine.
 1516 !
 1517 !  6. Error messages :
 1518 !
 1519 !       None.
 1520 !
 1521 !  7. Remarks :
 1522 !
 1523 !     - Curvilinear grid implementation. Variables FACX, FACY, CCOS, CSIN,
 1524 !       CCURX, CCURY are not needed and have been removed.  FACX is accou>
 1525 !       for as approriate in this subroutine.  FACX is also accounted for>
 1526 !       the case of .NOT.FLCX.  Since FACX is removed, there is now a che>
 1527 !       .NOT.FLCX in this subroutine.  In CFL calcs dx and dy are omitted,
 1528 !       since dx=dy=1 in index space.  Curvilinear grid derivatives
 1529 !       (DPDY, DQDX, etc.) and metric (GSQRT) are brought in via W3GDATMD.
 1530 !
 1531 !  8. Structure :
 1532 !
 1533 !     ---------------------------------------------
 1534 !     ---------------------------------------------
 1535 !
 1536 !  9. Switches :
 1537 !
 1538 !       !/S     Enable subroutine tracing.
 1539 !
 1540 !       !/MGP   Moving grid corrections.
 
 
                                                         w3pro3md.F90  page  29
 
 
 1541 !       !/MGG   Moving grid corrections.
 1542 !
 1543 !       !/T     Enable general test output.
 1544 !
 1545 ! 10. Source code :
 1546 !
 1547 !/ ------------------------------------------------------------------- /
 1548       USE CONSTANTS
 1549 !
 1550       USE W3TIMEMD, ONLY: DSEC21
 1551 !
 1552       USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSF, DTCFL, CLATS,      &
 1553                           FLCX, FLCY, NK, NTH, DTH, XFR,          &
 1554                           ECOS, ESIN, SIG, WDCG, WDTH, PFMOVE,    &
 1555                           FLAGLL, DPDX, DPDY, DQDX, DQDY, GSQRT
 1556       USE W3WDATMD, ONLY: TIME
 1557       USE W3ADATMD, ONLY: NMX0, NMX1, NMX2, NMY0, NMY1, NMY2, NACT,   &
 1558                           NCENT, MAPX2, MAPY2, MAPAXY, MAPCXY,        &
 1559                           MAPTRN, CG, CX, CY, ATRNX, ATRNY, ITIME
 1560       USE W3IDATMD, ONLY: FLCUR
 1561       USE W3ODATMD, ONLY: NDSE, NDST, FLBPI, NBI, TBPI0, TBPIN,       &
 1562                           ISBPI, BBPI0, BBPIN
 1563 !/
 1564       IMPLICIT NONE
 1565 !/
 1566 !/ ------------------------------------------------------------------- /
 1567 !/ Parameter list
 1568 !/
 1569       INTEGER, INTENT(IN)     :: ISEA, MAPSTA(NY*NX), MAPFS(NY*NX)
 1570       REAL, INTENT(IN)        :: DTG, VGX, VGY
 1571       REAL, INTENT(INOUT)     :: CFLXYMAX
 1572 !/
 1573 !/ ------------------------------------------------------------------- /
 1574 !/ Local parameters
 1575 !/
 1576       INTEGER                 :: ITH, IK, IXY, IP
 1577       INTEGER                 :: IX, IY, IXC, IYC, IBI
 1578       REAL                    :: CG0, CGA, CGN, CGX, CGY, CXC, CYC,   &
 1579                                  CXMIN, CXMAX, CYMIN, CYMAX
 1580       REAL                    :: CGC, FGSE = 1.
 1581       REAL                    :: FTH, FTHX, FTHY, FCG, FCGX, FCGY
 1582       REAL                    :: CP, CQ
 1583 !/
 1584 !/ Automatic work arrays
 1585 !/
 1586       REAL                    :: VLCFLX, VLCFLY
 1587       REAL                    :: CXTOT, CYTOT
 1588 !/
 1589 !/ ------------------------------------------------------------------- /
 1590 !/
 1591 !
 1592 ! 1.  Preparations --------------------------------------------------- *
 1593 ! 1.a Set constants
 1594 !
 1595       CFLXYMAX=0.
 
 
                                                         w3pro3md.F90  page  30
 
 
 1596       IX  = MAPSF(ISEA,1)
 1597       IY  = MAPSF(ISEA,2)
 1598       IXY = MAPSF(ISEA,3)
 1599       DO IK=1,NK
 1600         DO ITH=1,NTH
 1601           CXTOT = ECOS(ITH) * CG(IK,ISEA) / CLATS(ISEA)
 1602           CYTOT = ESIN(ITH) * CG(IK,ISEA)
 1603                                          
 1604                                          
 1605           IF ( FLCUR ) THEN
 1606             CXTOT = CXTOT + CX(ISEA)/CLATS(ISEA)
 1607             CYTOT = CYTOT + CY(ISEA)
 1608             END IF
 1609                   
 1610           CP = CXTOT*DPDX(IY,IX) + CYTOT*DPDY(IY,IX)
 1611           CQ = CXTOT*DQDX(IY,IX) + CYTOT*DQDY(IY,IX)
 1612           VLCFLX = CP*DTG
 1613           VLCFLY = CQ*DTG
 1614           CFLXYMAX = MAX(VLCFLX,VLCFLY,CFLXYMAX)
 1615           END DO
 1616         END DO
 1617               
 1618       RETURN
 1619 !/
 1620 !/ End of W3XYCFL ----------------------------------------------------- /
 1621 !/
 1622       END SUBROUTINE W3CFLXY
 1623                             
 1624 !/
 1625 !/ End of module W3PRO3MD -------------------------------------------- /
 1626 !/
 1627       END MODULE W3PRO3MD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
