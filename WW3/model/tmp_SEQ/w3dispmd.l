                                                         w3dispmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3DISPMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         29-May-2009 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    30-Nov-1999 : Fortran 90 version.                 ( version 2.00 )
   13 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   14 !/    10-Mar-2016 : Added Liu & Mollo-Christensen
   15 !/                  dispersion with ice (E. Rogers)     ( version 5.10 )
   16 !/
   17 !/    Copyright 2009 National Weather Service (NWS),
   18 !/       National Oceanic and Atmospheric Administration.  All rights
   19 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   20 !/       No unauthorized use without permission.
   21 !/
   22 !  1. Purpose :
   23 !
   24 !     A set of routines for solving the dispersion relation.
   25 !
   26 !  2. Variables and types :
   27 !
   28 !     All variables are retated to the interpolation tables. See
   29 !     DISTAB for a more comprehensive description.
   30 !
   31 !      Name      Type  Scope    Description
   32 !     ----------------------------------------------------------------
   33 !      NAR1D     I.P.  Public   Nmmer of elements in interpolation
   34 !                               array.
   35 !      DFAC      R.P.  Public   Value of KH at deep boundary.
   36 !      EWN1      R.A.  Public   Wavenumber array.
   37 !      ECG1      R.A.  Public   Group velocity array.
   38 !      N1MAX     Int.  Public   Actual maximum position in array.
   39 !      DSIE      Real  Public   SI step.
   40 !     ----------------------------------------------------------------
   41 !
   42 !  3. Subroutines and functions :
   43 !
   44 !      Name      Type  Scope    Description
   45 !     ----------------------------------------------------------------
   46 !      WAVNU1    Subr. Public   Solve dispersion using lookup table.
   47 !      WAVNU2    Subr. Public   Solve dispersion relation itteratively.
   48 !      DISTAB    Subr. Public   Fill interpolation tables.
   49 !      LIU_FORWARD_DISPERSION Subr. Public  Dispersion with ice
   50 !      LIU_REVERSE_DISPERSION Subr. Public  Dispersion with ice
   51 !     ----------------------------------------------------------------
   52 !
   53 !  4. Subroutines and functions used :
   54 !
   55 !      Name      Type  Module   Description
 
 
                                                         w3dispmd.F90  page   2
 
 
   56 !     ----------------------------------------------------------------
   57 !      STRACE    Subr. W3SERVMD Subroutine tracing            ( !/S )
   58 !     ----------------------------------------------------------------
   59 !
   60 !  5. Remarks :
   61 !
   62 !  6. Switches :
   63 !
   64 !       !/S   Enable subroutine tracing.
   65 !
   66 !  7. Source code :
   67 !
   68 !/ ------------------------------------------------------------------- /
   69 !/
   70       PUBLIC
   71 !/
   72 !/ Set up of public interpolation table ------------------------------ /
   73 !/
   74       INTEGER, PARAMETER      :: NAR1D  =  121
   75       REAL, PARAMETER         :: DFAC   =    6.
   76 !/
   77       INTEGER                 :: N1MAX
   78       REAL                    :: ECG1(0:NAR1D), EWN1(0:NAR1D), DSIE
   79 !/
   80 !/ Set up of public subroutines -------------------------------------- /
   81 !/
   82       CONTAINS
   83 !/ ------------------------------------------------------------------- /
   84       SUBROUTINE WAVNU1 (SI,H,K,CG)
   85 !/
   86 !/                  +-----------------------------------+
   87 !/                  | WAVEWATCH III           NOAA/NCEP |
   88 !/                  |           H. L. Tolman            |
   89 !/                  |                        FORTRAN 90 |
   90 !/                  | Last update :         30-Nov-1999 |
   91 !/                  +-----------------------------------+
   92 !/
   93 !/    04-Nov-1990 : Final FORTRAN 77                    ( version 1.18 )
   94 !/    30-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
   95 !/
   96 !  1. Purpose :
   97 !
   98 !     Calculate wavenumber and group velocity from the interpolation
   99 !     array filled by DISTAB from a given intrinsic frequency and the
  100 !     waterdepth.
  101 !
  102 !  2. Method :
  103 !
  104 !     Linear interpolation from one-dimensional array.
  105 !
  106 !  3. Parameters used :
  107 !
  108 !     Parameter list
  109 !     ----------------------------------------------------------------
  110 !       SI      Real   I   Intrinsic frequency (moving frame)  (rad/s)
 
 
                                                         w3dispmd.F90  page   3
 
 
  111 !       H       Real   I   Waterdepth                            (m)
  112 !       K       Real   O   Wavenumber                          (rad/m)
  113 !       CG      Real   O   Group velocity                       (m/s)
  114 !     ----------------------------------------------------------------
  115 !
  116 !  4. Error messages :
  117 !
  118 !     - None.
  119 !
  120 !  5. Called by :
  121 !
  122 !     - Any main program
  123 !
  124 !  6. Subroutines used :
  125 !
  126 !     - None
  127 !
  128 !  7. Remarks :
  129 !
  130 !     - Calculated si* is always made positive without checks : check in
  131 !       main program assumed !
  132 !     - Depth is unlimited.
  133 !
  134 !  8. Structure :
  135 !
  136 !     +---------------------------------------------+
  137 !     | calculate non-dimensional frequency         |
  138 !     |---------------------------------------------|
  139 !     | T            si* in range ?               F |
  140 !     |----------------------|----------------------|
  141 !     | calculate k* and cg* | deep water approx.   |
  142 !     | calculate output     |                      |
  143 !     |      parameters      |                      |
  144 !     +---------------------------------------------+
  145 !
  146 !  9. Switches :
  147 !
  148 !     !/S  Enable subroutine tracing.
  149 !
  150 ! 10. Source code :
  151 !
  152 !/ ------------------------------------------------------------------- /
  153 !/
  154       USE CONSTANTS, ONLY : GRAV
  155 !
  156       IMPLICIT NONE
  157 !/
  158 !/ ------------------------------------------------------------------- /
  159 !/ Parameter list
  160 !/
  161       REAL, INTENT(IN)        :: SI, H
  162       REAL, INTENT(OUT)       :: K, CG
  163 !/
  164 !/ ------------------------------------------------------------------- /
  165 !/ Local parameters
 
 
                                                         w3dispmd.F90  page   4
 
 
  166 !/
  167       INTEGER                 :: I1, I2
  168       REAL                    :: SQRTH, SIX, R1, R2
  169 !/
  170 !/ ------------------------------------------------------------------- /
  171 !/
  172 !
  173       SQRTH  = SQRT(H)
  174       SIX    = SI * SQRTH
  175       I1     = INT(SIX/DSIE)
  176 !
  177       IF (I1.LE.N1MAX.AND.I1.GE.1) THEN
  178           I2 = I1 + 1
  179           R1 = SIX/DSIE - REAL(I1)
  180           R2 = 1. - R1
  181           K  = ( R2*EWN1(I1) + R1*EWN1(I2) ) / H
  182           CG = ( R2*ECG1(I1) + R1*ECG1(I2) ) * SQRTH
  183         ELSE
  184           K  = SI*SI/GRAV
  185           CG = 0.5 * GRAV / SI
  186         END IF
  187 !
  188       RETURN
  189 !/
  190 !/ End of WAVNU1 ----------------------------------------------------- /
  191 !/
  192       END SUBROUTINE WAVNU1
  193 !/ ------------------------------------------------------------------- /
  194       SUBROUTINE WAVNU2 (W,H,K,CG,EPS,NMAX,ICON)
  195 !/
  196 !/                  +-----------------------------------+
  197 !/                  | WAVEWATCH III           NOAA/NCEP |
  198 !/                  |           H. L. Tolman            |
  199 !/                  |                        FORTRAN 90 |
  200 !/                  | Last update :         30-Nov-1999 |
  201 !/                  +-----------------------------------+
  202 !/
  203 !/    17-Jul-1990 : Final FORTRAN 77                    ( version 1.18 )
  204 !/    30-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  205 !/
  206 !  1. Purpose :
  207 !
  208 !     Calculation of wavenumber K from a given angular
  209 !     frequency W and waterdepth H.
  210 !
  211 !  2. Method :
  212 !
  213 !     Used equation :
  214 !                        2
  215 !                       W  = G*K*TANH(K*H)
  216 !
  217 !     Because of the nature of the equation, K is calculated
  218 !     with an itterative procedure.
  219 !
  220 !  3. Parameters :
 
 
                                                         w3dispmd.F90  page   5
 
 
  221 !
  222 !     Parameter list
  223 !     ----------------------------------------------------------------
  224 !       W       Real   I   Angular frequency
  225 !       H       Real   I   Waterdepth
  226 !       K       Real   O   Wavenumber ( same sign as W )
  227 !       CG      Real   O   Group velocity (same sign as W)
  228 !       EPS     Real   I   Wanted max. difference between K and Kold
  229 !       NMAX    Int.   I   Max number of repetitions in calculation
  230 !       ICON    Int.   O   Contol counter ( See error messages )
  231 !     ----------------------------------------------------------------
  232 !
  233 !  9. Switches :
  234 !
  235 !     !/S  Enable subroutine tracing.
  236 !
  237 ! 10. Source code :
  238 !
  239 !/ ------------------------------------------------------------------- /
  240 !/
  241       USE CONSTANTS, ONLY : GRAV
  242 !
  243       IMPLICIT NONE
  244 !/
  245 !/ ------------------------------------------------------------------- /
  246 !/ Parameter list
  247 !/
  248       INTEGER, INTENT(IN)     :: NMAX
  249       INTEGER, INTENT(OUT)    :: ICON
  250       REAL, INTENT(IN)        :: W, H, EPS
  251       REAL, INTENT(OUT)       :: CG, K
  252 !/
  253 !/ ------------------------------------------------------------------- /
  254 !/ Local parameters
  255 !/
  256       INTEGER                 :: I
  257       REAL                    :: F, W0, FD, DIF, RDIF, KOLD
  258       !REAL                    :: KTEST1, CGTEST1, KTEST2, CGTEST2
  259 !/
  260 !/ ------------------------------------------------------------------- /
  261 !/
  262 !
  263 !     Initialisations :
  264 !
  265       !CALL WAVNU1(ABS(W),H,KTEST1,CGTEST1)
  266       !CALL WAVNU3(ABS(W),H,KTEST2,CGTEST2)
  267                                            
  268       CG   = 0
  269       KOLD = 0
  270       ICON = 0
  271       W0   = ABS(W)
  272                    
  273 !
  274 !     1st approach :
  275 !
 
 
                                                         w3dispmd.F90  page   6
 
 
  276       IF (W0.LT.SQRT(GRAV/H)) THEN
  277           K = W0/SQRT(GRAV*H)
  278         ELSE
  279           K = W0*W0/GRAV
  280         END IF
  281 !
  282 !     Refinement :
  283 !
  284       DO I=1, NMAX
  285         DIF = ABS(K-KOLD)
  286         IF (K.NE.0) THEN
  287             RDIF = DIF/K
  288           ELSE
  289             RDIF = 0
  290           END IF
  291         IF (DIF .LT. EPS .AND. RDIF .LT. EPS) THEN
  292             ICON = 1
  293             GOTO 100
  294           ELSE
  295             KOLD = K
  296             F    = GRAV*KOLD*TANH(KOLD*H)-W0**2
  297             IF (KOLD*H.GT.25) THEN
  298                 FD = GRAV*TANH(KOLD*H)
  299               ELSE
  300                 FD = GRAV*TANH(KOLD*H) + GRAV*KOLD*H/((COSH(KOLD*H))**2)
  301               END IF
  302             K    = KOLD - F/FD
  303           END IF
  304         END DO
  305 !
  306       DIF   = ABS(K-KOLD)
  307       RDIF  = DIF/K
  308       IF (DIF .LT. EPS .AND. RDIF .LT. EPS) ICON = 1
  309  100  CONTINUE
  310       IF (2*K*H.GT.25) THEN
  311           CG = W0/K * 0.5
  312         ELSE
  313           CG = W0/K * 0.5*(1+(2*K*H/SINH(2*K*H)))
  314         END IF
  315       IF (W.LT.0.0) THEN
  316           K  = (-1)*K
  317           CG = CG*(-1)
  318         END IF
  319               
  320       !WRITE(*,'(20F20.10)') W, H, (K-KTEST2)/K*100., (CG-CGTEST2)/CG*100.
  321 !
  322       RETURN
  323 !/
  324 !/ End of WAVNU2 ----------------------------------------------------- /
  325 !/
  326       END SUBROUTINE WAVNU2
  327 !/
  328       PURE SUBROUTINE WAVNU3 (SI,H,K,CG)
  329 !/
  330 !/                  +-----------------------------------+
 
 
                                                         w3dispmd.F90  page   7
 
 
  331 !/                  | WAVEWATCH III           NOAA/NCEP |
  332 !/                  |           Aron Roland             |
  333 !/                  |                        FORTRAN 90 |
  334 !/                  | Last update :         20-05-17    |
  335 !/                  +-----------------------------------+
  336 !/
  337 !/    20.05.17 : Initial Version, Aron Roland based on WAVNU1
  338 !/
  339 !  1. Purpose :
  340 !
  341 !     Calculate wavenumber and group velocity from the improved
  342 !     Eckard's formula by Beji (2003)
  343 !
  344 !  2. Method :
  345 !
  346 !     Linear interpolation from one-dimensional array.
  347 !
  348 !  3. Parameters used :
  349 !
  350 !     Parameter list
  351 !     ----------------------------------------------------------------
  352 !       SI      Real   I   Intrinsic frequency (moving frame)  (rad/s)
  353 !       H       Real   I   Waterdepth                            (m)
  354 !       K       Real   O   Wavenumber                          (rad/m)
  355 !       CG      Real   O   Group velocity                       (m/s)
  356 !     ----------------------------------------------------------------
  357 !
  358 !  4. Error messages :
  359 !
  360 !     - None.
  361 !
  362 !  5. Called by :
  363 !
  364 !     - Any main program
  365 !
  366 !  6. Subroutines used :
  367 !
  368 !     - None
  369 !
  370 !  7. Remarks :
  371 !
  372 !     - Calculated si* is always made positive without checks : check in
  373 !       main program assumed !
  374 !     - Depth is unlimited.
  375 !
  376 !  8. Structure :
  377 !
  378 !     +---------------------------------------------+
  379 !     | calculate non-dimensional frequency         |
  380 !     |---------------------------------------------|
  381 !     | T            si* in range ?               F |
  382 !     |----------------------|----------------------|
  383 !     | calculate k* and cg* | deep water approx.   |
  384 !     | calculate output     |                      |
  385 !     |      parameters      |                      |
 
 
                                                         w3dispmd.F90  page   8
 
 
  386 !     +---------------------------------------------+
  387 !
  388 !  9. Switches :
  389 !
  390 !     !/S  Enable subroutine tracing.
  391 !
  392 ! 10. Source code :
  393 !
  394 !/ ------------------------------------------------------------------- /
  395 !/
  396       USE CONSTANTS, ONLY : GRAV, PI
  397 !!/S      USE W3SERVMD, ONLY: STRACE
  398 !
  399       IMPLICIT NONE
  400 !/
  401 !/ ------------------------------------------------------------------- /
  402 !/ Parameter list
  403 !/
  404       REAL, INTENT(IN)        :: SI, H
  405       REAL, INTENT(OUT)       :: K, CG
  406 !/
  407 !/ ------------------------------------------------------------------- /
  408 !/ Local parameters
  409 !/
  410       INTEGER                 :: I1, I2
  411 !!/S      INTEGER, SAVE           :: IENT = 0
  412       REAL                    :: KH0, KH, TMP, TP, CP, L
  413       REAL, PARAMETER         :: BETA1 = 1.55
  414       REAL, PARAMETER         :: BETA2 = 1.3
  415       REAL, PARAMETER         :: BETA3 = 0.216
  416       REAL, PARAMETER         :: ZPI   = 2 * PI
  417       REAL, PARAMETER         :: KDMAX = 20.
  418 !/
  419 !/ ------------------------------------------------------------------- /
  420 !/
  421 ! IENT does not work with PURE subroutines
  422 !!/S      CALL STRACE (IENT, 'WAVNU1')
  423 !
  424       TP  = SI/ZPI
  425       KH0 = ZPI*ZPI*H/GRAV*TP*TP
  426       TMP = 1.55 + 1.3*KH0 + 0.216*KH0*KH0
  427       KH  = KH0 * (1 + KH0**1.09 * 1./EXP(MIN(KDMAX,TMP))) / SQRT(TANH(MI>
  428       K   = KH/H
  429       CG  = 0.5*(1+(2*KH/SINH(MIN(KDMAX,2*KH))))*SI/K
  430 !
  431       RETURN
  432 !/
  433 !/ End of WAVNU3 ----------------------------------------------------- /
  434 !/
  435       END SUBROUTINE WAVNU3
  436 !/ ------------------------------------------------------------------- /
  437       SUBROUTINE DISTAB
  438 !/
  439 !/                  +-----------------------------------+
  440 !/                  | WAVEWATCH III           NOAA/NCEP |
 
 
                                                         w3dispmd.F90  page   9
 
 
  441 !/                  |           H. L. Tolman            |
  442 !/                  |                        FORTRAN 90 |
  443 !/                  | Last update :         30-Nov-1990 |
  444 !/                  +-----------------------------------+
  445 !/
  446 !/    04-Nov-1990 : Final FORTRAN 77                    ( version 1.18 )
  447 !/    30-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  448 !/
  449 !  1. Purpose :
  450 !
  451 !     Fill interpolation arrays for the calculation of wave parameters
  452 !     according to the linear (Airy) wave theory given the intrinsic
  453 !     frequency.
  454 !
  455 !  2. Method :
  456 !
  457 !     For a given set of non-dimensional frequencies the interpolation
  458 !     arrays with non-dimensional depths and group velocity are filled.
  459 !     The following non-dimensional parameters are used :
  460 !
  461 !       frequency   f*SQRT(h/g) = f*
  462 !       depth       kh          = k*
  463 !       group vel.  c/SQRT(gh)  = c*
  464 !
  465 !     Where k is the wavenumber, h the depth f the intrinsic frequency,
  466 !     g the acceleration of gravity and c the group velocity.
  467 !
  468 !  3. Parameters :
  469 !
  470 !     See module documentation.
  471 !
  472 !  4. Error messages :
  473 !
  474 !     - None.
  475 !
  476 !  5. Called by :
  477 !
  478 !     - W3GRID
  479 !     - Any main program.
  480 !
  481 !  6. Subroutines used :
  482 !
  483 !     - WAVNU2 (solve dispersion relation)
  484 !
  485 !  7. Remarks :
  486 !
  487 !     - In the filling of the arrays H = 1. is assumed and the factor
  488 !       SQRT (g) is moved from the interpolation to the filling
  489 !       procedure thus :
  490 !
  491 !         k* = k
  492 !
  493 !         c* = cg/SQRT(g)
  494 !
  495 !  8. Structure
 
 
                                                         w3dispmd.F90  page  10
 
 
  496 !
  497 !     -----------------------------------
  498 !       include common block
  499 !       calculate parameters
  500 !       fill zero-th position of arrays
  501 !       fill middle positions of arrays
  502 !       fill last positions of arrays
  503 !     -----------------------------------
  504 !
  505 !  9. Switches :
  506 !
  507 !       !/S   Enable subroutine tracing.
  508 !
  509 ! 10. Source code :
  510 !
  511 !/ ------------------------------------------------------------------- /
  512 !/
  513       USE CONSTANTS, ONLY : GRAV
  514 !
  515       IMPLICIT NONE
  516 !/
  517 !/ ------------------------------------------------------------------- /
  518 !/ Local parameters
  519 !/
  520       INTEGER                 :: I, ICON
  521       REAL                    :: DEPTH, CG, SIMAX, SI, K
  522 !/
  523 !/ ------------------------------------------------------------------- /
  524 !/
  525 !
  526 ! Calculate parameters ----------------------------------------------- *
  527 !
  528       N1MAX  = NAR1D - 1
  529       DEPTH  = 1.
  530       SIMAX  = SQRT (GRAV * DFAC)
  531       DSIE   = SIMAX / REAL(N1MAX)
  532 !
  533 ! Fill zero-th position of arrays ------------------------------------ *
  534 !
  535       EWN1(0) = 0.
  536       ECG1(0) = SQRT(GRAV)
  537 !
  538 ! Fill middle positions of arrays ------------------------------------ *
  539 !
  540       DO I=1, N1MAX
  541         SI = REAL(I)*DSIE
  542         CALL WAVNU2 (SI,DEPTH,K,CG,1E-7,15,ICON)
  543         EWN1(I) = K
  544         ECG1(I) = CG
  545         END DO
  546 !
  547 ! Fill last positions of arrays -------------------------------------- *
  548 !
  549       I      = N1MAX+1
  550       SI     = REAL(I)*DSIE
 
 
                                                         w3dispmd.F90  page  11
 
 
  551       CALL WAVNU2 (SI,DEPTH,K,CG,1E-7,15,ICON)
  552       EWN1(I) = K
  553       ECG1(I) = CG
  554 !
  555       RETURN
  556 !/
  557 !/ End of DISTAB ----------------------------------------------------- /
  558 !/
  559       END SUBROUTINE DISTAB
  560                            
  561 !/ ------------------------------------------------------------------- /
  562 !/
  563       SUBROUTINE LIU_FORWARD_DISPERSION (H_ICE,VISC,H_WDEPTH,SIGMA &
  564                                          ,K_SOLUTION,CG,ALPHA)
  565 !/
  566 !/                  +-----------------------------------+
  567 !/                  | WAVEWATCH III           NOAA/NCEP |
  568 !/                  |        W. E. Rogers (NRL-SSC)     |
  569 !/                  |                        FORTRAN 90 |
  570 !/                  | Last update :         11-Oct-2013 |
  571 !/                  +-----------------------------------+
  572 !/
  573 !/    16-Oct-2012 : Origination.                        ( version 4.04 )
  574 !/                                                        (E. Rogers)
  575 !/
  576 !  1. Purpose :
  577 !
  578 !     Dispersion relation calculation: given frequency, find k
  579 !     This is for dispersion in ice, so it requires the ice thickness
  580 !     and viscosity also. (the latter is the "eddy viscosity in the
  581 !     turbulent boundary layer beneath the ice.").
  582 !     Please note that this is for a continuous ice cover (not broken in >
  583 !
  584 !     This subroutine also calculates Cg and alpha.
  585 !     alpha is the exponential decay rate of *energy* (not to be
  586 !     confused with k_i which is the exponential decay rate of
  587 !     amplitude)
  588 !
  589 !     Both alpha and k_i are for spatial decay rate, units (1/m)
  590 !     Neither is for temporal decay rate.
  591 !
  592 !     References:
  593 !     N/A here, but see subroutine "Liu_reverse_dispersion"
  594 !
  595 !  2. Method :
  596 !
  597 !     Newton-Raphson.
  598 !     For actual dispersion relation, see documentation of subroutine
  599 !     "Liu_reverse_dispersion"
  600 !
  601 !  3. Parameters :
  602 !
  603 !      Parameter list
  604 !     ----------------------------------------------------------------
  605 !      H_ICE      Real    I  Ice thickness
 
 
                                                         w3dispmd.F90  page  12
 
 
  606 !      VISC       Real    I  Eddy viscosity (m2/sec)
  607 !      H_WDEPTH   Real    I  Water depth
  608 !      SIGMA      R.A.    I  Radian Wave frequency
  609 !      K_SOLUTION R.A.    O  Wave number
  610 !      CG         R.A.    O  Group velocity
  611 !      ALPHA      R.A.    O  Exponential decay rate of energy
  612 !      NK         Int.    I  Number of frequencies
  613 !     ----------------------------------------------------------------
  614 !
  615 !  4. Subroutines used :
  616 !
  617 !      Name                   | Type |  Module | Description
  618 !     ----------------------------------------------------------------
  619 !      Liu_reverse_dispersion | Subr.| W3SIC2MD| As name implies.
  620 !      STRACE                 | Subr.| W3SERVMD| Subroutine tracing.
  621 !      WAVNU1                 | Subr.| W3DISPMD| Wavenumber for waves
  622 !                                                in open water.
  623 !     ----------------------------------------------------------------
  624 !
  625 !  5. Called by :
  626 !
  627 !      Name                   | Type |  Module | Description
  628 !     ----------------------------------------------------------------
  629 !      W3SIC2                 | Subr.| W3SIC2MD| S_ice source term
  630 !     ----------------------------------------------------------------
  631 !
  632 !  6. Error messages :
  633 !
  634 !     Fails if solution is not found in a given number of iterations
  635 !
  636 !  7. Remarks :
  637 !
  638 !     Eventually, k and Cg should be used for propagation. This is not
  639 !     implemented yet. For now, it is only used to calculate the source
  640 !     term.
  641 !
  642 !     For discussion of the eddy viscosity term, see documentation of
  643 !     subroutine "Liu_reverse_dispersion"
  644 !
  645 !     This subroutine expects eddy viscosity in units of m2/sec even
  646 !     though values are given in units of cm2/sec in the Liu paper.
  647 !
  648 !  8. Structure :
  649 !
  650 !     See source code.
  651 !
  652 !  9. Switches :
  653 !
  654 !     !/S   Enable subroutine tracing.
  655 !
  656 ! 10. Source code :
  657 !
  658 !/ ------------------------------------------------------------------- /
  659       USE CONSTANTS, ONLY: TPI
  660       USE W3ODATMD,  ONLY: NDSE
 
 
                                                         w3dispmd.F90  page  13
 
 
  661       USE W3SERVMD,  ONLY: EXTCDE
  662       USE W3GDATMD, ONLY: NK, IICEHDISP, IICEDDISP, IICEFDISP, IICEHMIN
  663       ! USE W3DISPMD,  ONLY: WAVNU1
  664 !/
  665       IMPLICIT NONE
  666 !/
  667 !/ ------------------------------------------------------------------- /
  668 !/ Parameter list
  669                  
  670       REAL   , INTENT(IN)  :: H_ICE, H_WDEPTH, SIGMA(NK)
  671       REAL   , INTENT(IN)  :: VISC    ! in m2/sec
  672       REAL   , INTENT(OUT) :: K_SOLUTION(NK) ,CG(NK) ,ALPHA(NK)
  673                                                                
  674 !/
  675 !/ ------------------------------------------------------------------- /
  676 !/ Local parameters
  677       INTEGER            :: IK
  678       REAL, PARAMETER    :: FERRORMAX=1.0E-5  ! maximum acceptable error
  679       INTEGER, PARAMETER :: N_ITER=20  ! number of iterations prior to
  680                                        ! failure
  681       LOGICAL            :: GET_CG     ! indicates whether to get Cg
  682                                        ! and alpha
  683                                        ! from "Liu_reverse_dispersion"
  684       REAL :: FREQ(20)    ! wave frequency at current
  685                                        ! iteration
  686       REAL  :: KWN(20)     ! wavenumber at current
  687                                        ! iteration
  688       INTEGER            :: ITER       ! iteration number
  689       REAL               :: DK,DF,DFDK ! as name implies
  690       REAL               :: FDUMMY     ! as name implies
  691       !REAL               :: SIGMA      ! 2*pi/T
  692       REAL               :: K_OPEN     ! open-water value of k
  693       REAL               :: CG_OPEN    ! open-water value of Cg
  694       REAL               :: FWANTED    ! Freq. corresponding to sigma
  695       REAL               :: FERROR     ! Max acceptable error after test >
  696                                                                           
  697 !/
  698 !/ ------------------------------------------------------------------- /
  699 !/
  700 !
  701 !/ 0) --- Initialize/allocate variables ------------------------------ /
  702                                                                         
  703                                                                         
  704                                                                         
  705                                                                         
  706       DO IK = 1, NK
  707                    
  708         GET_CG  = .FALSE.
  709         !/T38      WRITE(*,*)'FORWARD IN: H_ICE,VISC,H_WDEPTH,FWANTED = '>
  710         !/T38                          H_ICE,VISC,H_WDEPTH,FWANTED
  711              FWANTED=SIGMA(IK)/TPI
  712         ! First guess for k :
  713                              
  714               CALL WAVNU1(SIGMA(IK),H_WDEPTH,K_OPEN,CG_OPEN)
  715         !     KWN(1)  = 0.2 ! (old method)
 
 
                                                         w3dispmd.F90  page  14
 
 
  716               KWN(1)  =K_OPEN ! new method, Mar 10 2014
  717         !
  718         !/ 1) ----- Iteration loop to find k ---------------------------->
  719               ITER = 0
  720               DF   = 999.
  721                          
  722               IF ( (H_ICE.LT.IICEHDISP).OR.(H_WDEPTH.LT.IICEDDISP) ) THEN
  723                  FERROR=IICEFDISP*FERRORMAX
  724               ELSE
  725                  FERROR=FERRORMAX
  726               ENDIF
  727                    
  728               DO WHILE ( ABS(DF).GE.FERROR .AND. ITER.LE.N_ITER )
  729                  ITER = ITER + 1
  730                  ! compute freq for this iteration
  731                  CALL LIU_REVERSE_DISPERSION(H_ICE,VISC,H_WDEPTH,KWN(ITER>
  732                                              GET_CG,FREQ(ITER),CG(IK),ALP>
  733                                                                           
  734                  ! calculate dk
  735                  IF (ITER == 1)THEN
  736                     ! We do not have slope yet, so pick a number...
  737                     DK = 0.01
  738                  ELSEIF (ITER.EQ.N_ITER+1) THEN
  739                     WRITE(NDSE,800) N_ITER
  740                     CALL EXTCDE(2)
  741                  ELSE
  742                     ! use slope
  743                     DFDK = (FREQ(ITER)-FREQ(ITER-1)) / (KWN(ITER)-KWN(ITE>
  744                     DF   = FWANTED - FREQ(ITER)
  745         !/T38       WRITE(*,*)'ITER = ',ITER,' ;  K = ',KWN(ITER),' ; F =>
  746         !/T38                  FREQ(ITER),' ; DF = ',DF
  747                     DK   = DF / DFDK
  748                  ENDIF
  749                       
  750                  ! Decide on next k to try
  751                  KWN(ITER+1) = KWN(ITER) + DK
  752                  ! If we end up with a negative k for the next iteration,>
  753                  !   allow this.
  754                  IF(KWN(ITER+1) < 0.0)THEN
  755                     KWN(ITER+1) = TPI / 1000.0
  756                  ENDIF
  757                       
  758               END DO
  759                     
  760         !/ 2) -------- Finish up. --------------------------------------->
  761         !     Success, so return K_SOLUTION, and call LIU_REVERSE_DISPERS>
  762         !     last time, to get CG and ALPHA
  763                                             
  764               K_SOLUTION(IK) = KWN(ITER)
  765                                         
  766         GET_CG     = .TRUE.
  767       CALL LIU_REVERSE_DISPERSION(H_ICE,VISC,H_WDEPTH,K_SOLUTION(IK), &
  768            GET_CG,FDUMMY,CG(IK),ALPHA(IK))
  769       END DO
  770 !
 
 
                                                         w3dispmd.F90  page  15
 
 
  771   800 FORMAT (/' *** WAVEWATCH III ERROR IN '           &
  772                'W3SIC2_LIU_FORWARD_DISPERSION : ' /     &
  773                '     NO SOLUTION FOUND AFTER ',I4,' ITERATIONS.')
  774 !/
  775 !/ End of LIU_FORWARD_DISPERSION ------------------------------------- /
  776 !/
  777       END SUBROUTINE LIU_FORWARD_DISPERSION
  778 !/ ------------------------------------------------------------------- /
  779 !/
  780       SUBROUTINE LIU_REVERSE_DISPERSION (H_ICE,VISC,H_WDEPTH,KWN &
  781              ,GET_CG,FREQ,CG,ALPHA)
  782 !/
  783 !/                  +-----------------------------------+
  784 !/                  | WAVEWATCH III           NOAA/NCEP |
  785 !/                  |        W. E. Rogers (NRL-SSC)     |
  786 !/                  |                        FORTRAN 90 |
  787 !/                  | Last update :         11-Oct-2013 |
  788 !/                  +-----------------------------------+
  789 !/
  790 !/    12-Oct-2012 : Origination.                        ( version 4.04 )
  791 !/                                                        (E. Rogers)
  792 !/
  793 !  1. Purpose :
  794 !
  795 !     Dispersion relation calculation: given k, find frequency.
  796 !     This is for dispersion in ice, so it requires the ice thickness
  797 !     and viscosity also. (the latter is the "eddy viscosity in the
  798 !     turbulent boundary layer beneath the ice.").
  799 !
  800 !     This subroutine also (optionally) calculates Cg and alpha.
  801 !     alpha is the exponential decay rate of *energy* (not to be
  802 !     confused with k_i which is the exponential decay rate of
  803 !     amplitude)
  804 !
  805 !     Both alpha and k_i are for spatial decay rate, units (1/m)
  806 !     Neither is for temporal decay rate.
  807                                          
  808 !     This calculation is optional for reasons of computational
  809 !      efficiency (don't calculate if it will not be used). Note that
  810 !      if Cg and alpha are not calculated, the value of input viscosity
  811 !      is irrelevant.
  812 !
  813 !     References:
  814 !       Liu et al.    1991: JGR 96 (C3), 4605-4621
  815 !       Liu and Mollo 1988: JPO 18       1720-1712
  816 !
  817 !  2. Method :
  818 !
  819 !     In 1991 paper, see equations on page 4606. The key equations are:
  820 !     sigma2=(grav*k+B*k^5)/((coth(k*H_wdepth))+k*M);
  821 !     Cg=(grav+(5+4*k*M)*(B*k^4))/((2*sigma)*((1+k*M)^2));
  822 !     alpha=(sqrt(visc)*k*sqrt(sigma))/(Cg*sqrt(2)*(1+k*M));
  823 !
  824 !  3. Parameters :
  825 !
 
 
                                                         w3dispmd.F90  page  16
 
 
  826 !      Parameter list
  827 !     ----------------------------------------------------------------
  828 !      H_ICE     REAL    I  Ice thickness
  829 !      VISC      REAL    I  Eddy viscosity (if GET_CG) (m2/sec)
  830 !      H_WDEPTH  REAL    I  Water depth
  831 !      KWN       REAL    I  Wavenumber
  832 !      GET_CG    LOGICAL I  Indicates whether to calculate Cg and alpha
  833 !      FREQ      REAL    O  Frequency
  834 !      CG        REAL    O  Group velocity (if GET_CG)
  835 !      ALPHA     REAL    O  Exponential decay rate of energy (if GET_CG)
  836 !     ----------------------------------------------------------------
  837 !
  838 !  4. Subroutines used :
  839 !
  840 !      Name      Type  Module   Description
  841 !     ----------------------------------------------------------------
  842 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  843 !     ----------------------------------------------------------------
  844 !
  845 !  5. Called by :
  846 !
  847 !      Name                  | Type |  Module | Description
  848 !     ----------------------------------------------------------------
  849 !      Liu_forward_dispersion| Subr.| W3SIC2MD| As name implies.
  850 !     ----------------------------------------------------------------
  851 !
  852 !  6. Error messages :
  853 !
  854 !       None.
  855 !
  856 !  7. Remarks :
  857 !
  858 !     Eventually, k and Cg should be used for propagation. This is not
  859 !     implemented yet. For now, it is only used to calculate the source
  860 !     term.
  861 !
  862 !     The eddy viscosity term given by Liu is unfortunately highly
  863 !     variable, and "not a physical parameter", which suggests that it
  864 !     is difficult to specify in practice. In this paper, we see values
  865 !     of:
  866 !     nu= 160.0e-4 m2/sec (Brennecke (1921)
  867 !     nu=  24.0e-4 m2/sec (Hunkins 1966)
  868 !     nu=3450.0e-4 m2/sec (Fig 11)
  869 !     nu=   4.0e-4 m2/sec (Fig 12)
  870 !     nu= 150.0e-4 m2/sec (Fig 13)
  871 !     nu=  54.0e-4 m2/sec (Fig 14)
  872 !     nu= 384.0e-4 m2/sec (Fig 15)
  873 !     nu=1536.0e-4 m2/sec (Fig 16)
  874 !
  875 !     The paper states: "The only tuning parameter is the turbulent eddy
  876 !     viscosity, and it is a function of the flow conditions in the
  877 !     turbulent boundary layer which are determined by the ice
  878 !     thickness, floe sizes, ice concentration, and wavelength."
  879 !
  880 !     Another criticism of this source term is that it does not use the
 
 
                                                         w3dispmd.F90  page  17
 
 
  881 !     ice concentration in actual calculations. The method appears to
  882 !     simply rely on concentration being high, "When the ice is highly
  883 !     compact with high concentration, the flexural waves obey the
  884 !     dispersion relation (1) as similar waves in a continuous ice
  885 !     sheet." Later, "Five of these  cases with high ice conentration
  886 !     (larger than 60%) in the MIZ have been selected"
  887 !
  888 !     This subroutine expects eddy viscosity in units of m2/sec even
  889 !     though values are given in units of cm2/sec in the Liu paper.
  890 !
  891 !     Cg used here is correct only for deep water. It is taken from
  892 !     Liu et al. (1991) equation 2. If we want to calculate for finite
  893 !     depths accurately, we need to use d_sigma/d_k. However, be warned
  894 !     that this calculation is sensitive to numerical error and so the
  895 !     (potentially too coarse) computational grid for sigma and k should
  896 !     *not* be used.
  897 !
  898 !  8. Structure :
  899 !
  900 !     See source code.
  901 !
  902 !  9. Switches :
  903 !
  904 !     !/S   Enable subroutine tracing.
  905 !
  906 ! 10. Source code :
  907 !
  908 !/ ------------------------------------------------------------------- /
  909       USE CONSTANTS, ONLY: DWAT, TPI, GRAV
  910       USE W3GDATMD, ONLY: NK
  911 !/
  912       IMPLICIT NONE
  913 !/
  914 !/ ------------------------------------------------------------------- /
  915 !/ Parameter list
  916       REAL   , INTENT(IN)  :: H_ICE,H_WDEPTH,KWN
  917       REAL   , INTENT(IN)  :: VISC    ! in m2/sec
  918       LOGICAL, INTENT(IN)  :: GET_CG
  919       REAL   , INTENT(OUT) :: FREQ,CG,ALPHA
  920 !/
  921 !/ ------------------------------------------------------------------- /
  922 !/ Local parameters
  923       REAL, PARAMETER   :: E = 6.0E+9 ! Young's modulus of elasticity
  924       REAL, PARAMETER   :: S = 0.3    ! "s", Poisson's ratio
  925       REAL              :: DICE       ! "dice", density of ice
  926       REAL              :: B          ! quantifies effect of bending
  927                                       ! of ice
  928       REAL              :: M          ! quantifies effect of inertia
  929                                       ! of ice
  930       REAL              :: COTHTERM   ! temporary variable
  931       REAL              :: SIGMA      ! 2*pi/T
  932       REAL              :: KH         ! k*h
  933 !/
  934 !/ ------------------------------------------------------------------- /
  935 !/
 
 
                                                         w3dispmd.F90  page  18
 
 
  936 !
  937 !/ 0) --- Initialize essential parameters ---------------------------- /
  938       CG    = 0.
  939       ALPHA = 0.
  940       FREQ  = 0.
  941       DICE = DWAT * 0.9 ! from Liu 1991 pg 4606
  942                                                
  943                                                
  944 !
  945 !/ 1) --- Calculate frequency ---------------------------------------- /
  946                                                                         
  947 ! Note: Liu et al 1991 have "kwn*h_ice" in COTH(_) but I believe they
  948 ! meant to write "kwn*H_wdepth"
  949                                
  950       B  = (E * H_ICE**3) / (12. * (1. - S**2) * DWAT)
  951       M  = DICE * H_ICE / DWAT
  952       KH = KWN * H_WDEPTH
  953       IF ( KH>5.0 ) THEN
  954          COTHTERM = 1.0
  955       ELSEIF ( KH<1.0E-4 ) THEN
  956          COTHTERM = 1.0 / KH
  957       ELSE
  958          COTHTERM = COSH(KH) / SINH(KH)
  959       ENDIF
  960       SIGMA = SQRT((GRAV * KWN + B * KWN**5) / (COTHTERM + KWN * M))
  961       FREQ  = SIGMA/(TPI)
  962                          
  963 !/ 2) --- Calculate Cg and alpha if requested ------------------------ /
  964 !     Note: Cg is correct only for deep water
  965       IF (GET_CG) THEN
  966          CG    = (GRAV + (5.0+4.0 * KWN * M) * (B * KWN**4)) &
  967                / (2.0 * SIGMA * ((1.0 + KWN * M)**2))
  968          ALPHA = (SQRT(VISC) * KWN * SQRT(SIGMA)) &
  969                / (CG * SQRT(2.0) * (1 + KWN * M))
  970       ENDIF
  971            
  972            
  973 !/
  974 !/ End of LIU_REVERSE_DISPERSION ------------------------------------- /
  975 !/
  976       END SUBROUTINE LIU_REVERSE_DISPERSION
  977 !/ ------------------------------------------------------------------- /
  978 !/
  979 !/ End of module W3DISPMD -------------------------------------------- /
  980 !/
  981       END MODULE W3DISPMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
