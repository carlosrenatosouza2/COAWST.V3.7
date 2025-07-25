                                                         w3snl1md.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3SNL1MD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         03-Sep-2012 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    04-Feb-2000 : Origination.                        ( version 2.00 )
   13 !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
   14 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
   15 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   16 !/    03-Sep-2012 : Clean up of test output T0, T1      ( version 4.07 )
   17 !/
   18 !/    Copyright 2009 National Weather Service (NWS),
   19 !/       National Oceanic and Atmospheric Administration.  All rights
   20 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   21 !/       No unauthorized use without permission.
   22 !/
   23 !  1. Purpose :
   24 !
   25 !     Bundles routines calculate nonlinear wave-wave interactions
   26 !     according to the Discrete Interaction Approximation (DIA) of
   27 !     Hasselmann et al. (JPO, 1985).
   28 !
   29 !  2. Variables and types :
   30 !
   31 !      Name      Type  Scope    Description
   32 !     ----------------------------------------------------------------
   33 !     ----------------------------------------------------------------
   34 !
   35 !  3. Subroutines and functions :
   36 !
   37 !      Name      Type  Scope    Description
   38 !     ----------------------------------------------------------------
   39 !      W3SNL1    Subr. Public   Calculate interactions.
   40 !      INSNL1    Subr. Public   Initialization routine.
   41 !     ----------------------------------------------------------------
   42 !
   43 !  4. Subroutines and functions used :
   44 !
   45 !     See subroutine documentation.
   46 !
   47 !  5. Remarks :
   48 !
   49 !  6. Switches :
   50 !
   51 !       !/S      Enable subroutine tracing.
   52 !       !/T(n)   Test output, see subroutines.
   53 !
   54 !  7. Source code :
   55 !
 
 
                                                         w3snl1md.F90  page   2
 
 
   56 !/ ------------------------------------------------------------------- /
   57 !/
   58       PUBLIC
   59 !/
   60       CONTAINS
   61 !/ ------------------------------------------------------------------- /
   62       SUBROUTINE W3SNL1 (A, CG, KDMEAN, S, D)
   63 !/
   64 !/                  +-----------------------------------+
   65 !/                  | WAVEWATCH III           NOAA/NCEP |
   66 !/                  |           H. L. Tolman            |
   67 !/                  |                        FORTRAN 90 |
   68 !/                  | Last update :         06-Jun-2018 |
   69 !/                  +-----------------------------------+
   70 !/
   71 !/    12-Jun-1996 : Final FORTRAN 77                    ( version 1.18 )
   72 !/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
   73 !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
   74 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
   75 !/    03-Sep-2012 : Clean up of test output T0, T1      ( version 4.07 )
   76 !/    06-Jun-2018 : Add optional DEBUGSRC               ( version 6.04 )
   77 !/
   78 !  1. Purpose :
   79 !
   80 !     Calculate nonlinear interactions and the diagonal term of
   81 !     its derivative.
   82 !
   83 !  2. Method :
   84 !
   85 !     Discrete interaction approximation. (Hasselmann and Hasselmann
   86 !     1985; WAMDI group 1988)
   87 !
   88 !     The DIA is applied to the energy spectrum (instead of the action
   89 !     spectrum), for which is was originally developped. Because the
   90 !     frequency grid is invariant, the nonlinear interactions are
   91 !     calculated for the frequency spectrum, as in WAM. This requires
   92 !     only a single set of interpolation data which can be applied
   93 !     throughout the spatial domain. For deep water this is idenitical
   94 !     to a direct application to the wavenumber spectrum, for shallow
   95 !     water it is not. As the shallow water correction is nothing but
   96 !     a crude approximation, the choice between spectra is expected to
   97 !     be irrelevant.
   98 !
   99 !     The nonlinear interactions are calculated for two "mirror image"
  100 !     quadruplets as described in the manual. The central bin of these
  101 !     quadruples is placed on the discrete complonents of the spectrum,
  102 !     which requires interpolation to obtain other eneregy densities.
  103 !     The figure below defines the diferent basic counters and weights
  104 !     necessary for this interpolation.
  105 !
  106 !               IFRM1  IFRM
  107 !                5        7    T |
  108 !          ITHM1  +------+     H +
  109 !                 |      |     E |      IFRP      IFRP1
  110 !                 |   \  |     T |       3           1
 
 
                                                         w3snl1md.F90  page   3
 
 
  111 !           ITHM  +------+     A +        +---------+  ITHP1
  112 !                6       \8      |        |         |
  113 !                                |        |  /      |
  114 !                           \    +        +---------+  ITHP
  115 !                                |      /4           2
  116 !                              \ |  /
  117 !          -+-----+------+-------#--------+---------+----------+
  118 !                              / |  \        FREQ.
  119 !                                |      \4           2
  120 !                           /    +        +---------+  ITHP
  121 !                                |        |  \      |
  122 !                6       /8      |        |         |
  123 !           ITHM  +------+       +        +---------+  ITHP1
  124 !                 |   \  |       |       3           1
  125 !                 |      |       |      IFRP      IFRP1
  126 !          ITHM1  +------+       +
  127 !                5        7      |
  128 !
  129 !     To create long vector loops and to efficiently deal with the
  130 !     closed nature of the directional space, the relative counters
  131 !     above are replaced by complete addresses stored in 32 arrays
  132 !     (see section 3 and INSNL1). The interaction are furthermore
  133 !     calucated for an extended spectrum, making it unnecessary to
  134 !     introduce extra weight factors for low and high frequencies.
  135 !     Therefore low and high frequencies are added to the local
  136 !     (auxiliary) spectrum as illustraed below.
  137 !
  138 !              ^  +---+---------------------+---------+- NTH
  139 !              |  |   :                     :         |
  140 !                 |   :                     :         |
  141 !              d  | 2 :  original spectrum  :    1    |
  142 !              i  |   :                     :         |
  143 !              r  |   :                     :         |
  144 !                 +---+---------------------+---------+-  1
  145 !                            Frequencies -->     ^
  146 !         IFR =   0   1                    NFR   |  NFRHGH
  147 !                                                |
  148 !                                             NFRCHG
  149 !
  150 !     where : 1 : Extra tail added beyond NFR
  151 !             2 : Empty bins at low frequencies
  152 !
  153 !             NFRHGH = NFR + IFRP1 - IFRM1
  154 !             NFRCHG = NFR - IFRM1
  155 !
  156 !     All counters and arrays are set in INSNL1. See also section 3
  157 !     and section 8.
  158 !
  159 !  3. Parameters :
  160 !
  161 !     Parameter list
  162 !     ----------------------------------------------------------------
  163 !       A       R.A.  I   Action spectrum A(ISP) as a function of
  164 !                         direction (rad)  and wavenumber.
  165 !       CG      R.A.  I   Group velocities (dimension NK).
 
 
                                                         w3snl1md.F90  page   4
 
 
  166 !       KDMEAN  Real  I   Mean relative depth.
  167 !       S       R.A.  O   Source term.                           *)
  168 !       D       R.A.  O   Diagonal term of derivative.           *)
  169 !     ----------------------------------------------------------------
  170 !                             *) 1-D array with dimension NTH*NK
  171 !
  172 !  4. Subroutines used :
  173 !
  174 !      Name      Type  Module   Description
  175 !     ----------------------------------------------------------------
  176 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  177 !      PRT2DS    Subr. W3ARRYMD Print plot of spectra.
  178 !      OUTMAT    Subr. W3WRRYMD Print out 2D matrix.
  179 !     ----------------------------------------------------------------
  180 !
  181 !  5. Called by :
  182 !
  183 !      Name      Type  Module   Description
  184 !     ----------------------------------------------------------------
  185 !      W3SRCE    Subr. W3SRCEMD Source term integration.
  186 !      W3EXPO    Subr.   N/A    Point output post-processor.
  187 !      GXEXPO    Subr.   N/A    GrADS point output post-processor.
  188 !     ----------------------------------------------------------------
  189 !
  190 !  6. Error messages :
  191 !
  192 !       None.
  193 !
  194 !  7. Remarks :
  195 !
  196 !       None.
  197 !
  198 !  8. Structure :
  199 !
  200 !     -------------------------------------------
  201 !      1.  Calculate proportionality constant.
  202 !      2.  Prepare auxiliary spectrum
  203 !      3.  Calculate (unfolded) interactions
  204 !        a Energy at interacting bins
  205 !        b Contribution to interactions
  206 !        c Fold interactions to side angles
  207 !      4.  Put source and diagonal term together
  208 !     -------------------------------------------
  209 !
  210 !  9. Switches :
  211 !
  212 !     !/S   Enable subroutine tracing.
  213 !     !/T   Enable general test output.
  214 !     !/T0  2-D print plot of source term.
  215 !     !/T1  Print arrays.
  216 !
  217 ! 10. Source code :
  218 !
  219 !/ ------------------------------------------------------------------- /
  220 !/
 
 
                                                         w3snl1md.F90  page   5
 
 
  221       USE CONSTANTS
  222       USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, FACHFE,                &
  223                           KDCON, KDMN, SNLC1, SNLS1, SNLS2, SNLS3
  224       USE W3ADATMD, ONLY: NFR, NFRHGH, NFRCHG, NSPECX, NSPECY,        &
  225                     IP11, IP12, IP13, IP14, IM11, IM12, IM13, IM14,   &
  226                     IP21, IP22, IP23, IP24, IM21, IM22, IM23, IM24,   &
  227                     IC11, IC12, IC21, IC22, IC31, IC32, IC41, IC42,   &
  228                     IC51, IC52, IC61, IC62, IC71, IC72, IC81, IC82,   &
  229                     DAL1, DAL2, DAL3, AF11,                           &
  230                     AWG1, AWG2, AWG3, AWG4, AWG5, AWG6, AWG7, AWG8,   &
  231                     SWG1, SWG2, SWG3, SWG4, SWG5, SWG6, SWG7, SWG8
  232 !!/DEBUGSRC      USE W3ODATMD, only : IAPROC
  233 !
  234       IMPLICIT NONE
  235 !/
  236 !/ ------------------------------------------------------------------- /
  237 !/ Parameter list
  238 !/
  239       REAL, INTENT(IN)        :: A(NSPEC), CG(NK), KDMEAN
  240       REAL, INTENT(OUT)       :: S(NSPEC), D(NSPEC)
  241 !/
  242 !/ ------------------------------------------------------------------- /
  243 !/ Local parameters
  244 !/
  245       INTEGER                 :: ITH, IFR, ISP
  246       REAL                    :: X, X2, CONS, CONX, FACTOR,           &
  247                                  E00, EP1, EM1, EP2, EM2,             &
  248                                  SA1A, SA1B, SA2A, SA2B
  249       REAL               ::  UE  (1-NTH:NSPECY), SA1 (1-NTH:NSPECX),  &
  250                              SA2 (1-NTH:NSPECX), DA1C(1-NTH:NSPECX),  &
  251                              DA1P(1-NTH:NSPECX), DA1M(1-NTH:NSPECX),  &
  252                              DA2C(1-NTH:NSPECX), DA2P(1-NTH:NSPECX),  &
  253                              DA2M(1-NTH:NSPECX), CON (      NSPEC )
  254 !/
  255 !/ ------------------------------------------------------------------- /
  256 !/
  257 ! initialisations
  258 !
  259 ! 1.  Calculate prop. constant --------------------------------------- *
  260 !
  261       X      = MAX ( KDCON*KDMEAN , KDMN )
  262       X2     = MAX ( -1.E15, SNLS3*X)
  263       CONS   = SNLC1 * ( 1. + SNLS1/X * (1.-SNLS2*X) * EXP(X2) )
  264 !
  265 ! 2.  Prepare auxiliary spectrum and arrays -------------------------- *
  266 !
  267       DO IFR=1, NFR
  268         CONX = TPIINV / SIG(IFR) * CG(IFR)
  269         DO ITH=1, NTH
  270           ISP       = ITH + (IFR-1)*NTH
  271           UE (ISP) = A(ISP) / CONX
  272           CON(ISP) = CONX
  273           END DO
  274         END DO
  275 !
 
 
                                                         w3snl1md.F90  page   6
 
 
  276       DO IFR=NFR+1, NFRHGH
  277         DO ITH=1, NTH
  278           ISP      = ITH + (IFR-1)*NTH
  279           UE(ISP) = UE(ISP-NTH) * FACHFE
  280           END DO
  281         END DO
  282 !
  283       DO ISP=1-NTH, 0
  284         UE  (ISP) = 0.
  285         SA1 (ISP) = 0.
  286         SA2 (ISP) = 0.
  287         DA1C(ISP) = 0.
  288         DA1P(ISP) = 0.
  289         DA1M(ISP) = 0.
  290         DA2C(ISP) = 0.
  291         DA2P(ISP) = 0.
  292         DA2M(ISP) = 0.
  293         END DO
  294 !
  295 ! 3.  Calculate interactions for extended spectrum ------------------- *
  296 !
  297       DO ISP=1, NSPECX
  298 !
  299 ! 3.a Energy at interacting bins
  300 !
  301         E00    =        UE(ISP)
  302         EP1    = AWG1 * UE(IP11(ISP)) + AWG2 * UE(IP12(ISP))        &
  303                + AWG3 * UE(IP13(ISP)) + AWG4 * UE(IP14(ISP))
  304         EM1    = AWG5 * UE(IM11(ISP)) + AWG6 * UE(IM12(ISP))        &
  305                + AWG7 * UE(IM13(ISP)) + AWG8 * UE(IM14(ISP))
  306         EP2    = AWG1 * UE(IP21(ISP)) + AWG2 * UE(IP22(ISP))        &
  307                + AWG3 * UE(IP23(ISP)) + AWG4 * UE(IP24(ISP))
  308         EM2    = AWG5 * UE(IM21(ISP)) + AWG6 * UE(IM22(ISP))        &
  309                + AWG7 * UE(IM23(ISP)) + AWG8 * UE(IM24(ISP))
  310 !
  311 ! 3.b Contribution to interactions
  312 !
  313         FACTOR = CONS * AF11(ISP) * E00
  314 !
  315         SA1A   = E00 * ( EP1*DAL1 + EM1*DAL2 )
  316         SA1B   = SA1A - EP1*EM1*DAL3
  317         SA2A   = E00 * ( EP2*DAL1 + EM2*DAL2 )
  318         SA2B   = SA2A - EP2*EM2*DAL3
  319 !
  320         SA1 (ISP) = FACTOR * SA1B
  321         SA2 (ISP) = FACTOR * SA2B
  322 !
  323         DA1C(ISP) = CONS * AF11(ISP) * ( SA1A + SA1B )
  324         DA1P(ISP) = FACTOR * ( DAL1*E00 - DAL3*EM1 )
  325         DA1M(ISP) = FACTOR * ( DAL2*E00 - DAL3*EP1 )
  326 !
  327         DA2C(ISP) = CONS * AF11(ISP) * ( SA2A + SA2B )
  328         DA2P(ISP) = FACTOR * ( DAL1*E00 - DAL3*EM2 )
  329         DA2M(ISP) = FACTOR * ( DAL2*E00 - DAL3*EP2 )
  330 !
 
 
                                                         w3snl1md.F90  page   7
 
 
  331         END DO
  332 !
  333 ! 4.  Put source and diagonal term together -------------------------- *
  334 !
  335 !!/DEBUGSRC     WRITE(740+IAPROC,*)  'W3SNL1 : sum(SA1)=', sum(SA1)
  336 !!/DEBUGSRC     WRITE(740+IAPROC,*)  'W3SNL1 : sum(SA2)=', sum(SA2)
  337 !!/DEBUGSRC     FLUSH(740+IAPROC)
  338       DO ISP=1, NSPEC
  339 !
  340         S(ISP) = CON(ISP) * ( - 2. * ( SA1(ISP) + SA2(ISP) )       &
  341                    + AWG1 * ( SA1(IC11(ISP)) + SA2(IC12(ISP)) )    &
  342                    + AWG2 * ( SA1(IC21(ISP)) + SA2(IC22(ISP)) )    &
  343                    + AWG3 * ( SA1(IC31(ISP)) + SA2(IC32(ISP)) )    &
  344                    + AWG4 * ( SA1(IC41(ISP)) + SA2(IC42(ISP)) )    &
  345                    + AWG5 * ( SA1(IC51(ISP)) + SA2(IC52(ISP)) )    &
  346                    + AWG6 * ( SA1(IC61(ISP)) + SA2(IC62(ISP)) )    &
  347                    + AWG7 * ( SA1(IC71(ISP)) + SA2(IC72(ISP)) )    &
  348                    + AWG8 * ( SA1(IC81(ISP)) + SA2(IC82(ISP)) ) )
  349 !
  350         D(ISP) =  - 2. * ( DA1C(ISP) + DA2C(ISP) )                 &
  351                 + SWG1 * ( DA1P(IC11(ISP)) + DA2P(IC12(ISP)) )     &
  352                 + SWG2 * ( DA1P(IC21(ISP)) + DA2P(IC22(ISP)) )     &
  353                 + SWG3 * ( DA1P(IC31(ISP)) + DA2P(IC32(ISP)) )     &
  354                 + SWG4 * ( DA1P(IC41(ISP)) + DA2P(IC42(ISP)) )     &
  355                 + SWG5 * ( DA1M(IC51(ISP)) + DA2M(IC52(ISP)) )     &
  356                 + SWG6 * ( DA1M(IC61(ISP)) + DA2M(IC62(ISP)) )     &
  357                 + SWG7 * ( DA1M(IC71(ISP)) + DA2M(IC72(ISP)) )     &
  358                 + SWG8 * ( DA1M(IC81(ISP)) + DA2M(IC82(ISP)) )
  359 !
  360         END DO
  361 !!/DEBUGSRC     WRITE(740+IAPROC,*)  'W3SNL1 : sum(S)=', sum(S)
  362 !!/DEBUGSRC     WRITE(740+IAPROC,*)  'W3SNL1 : sum(D)=', sum(D)
  363 !!/DEBUGSRC     FLUSH(740+IAPROC)
  364 !
  365 ! ... Test output :
  366 !
  367       RETURN
  368 !
  369 ! Formats
  370 !
  371 !/
  372 !/ End of W3SNL1 ----------------------------------------------------- /
  373 !/
  374       END SUBROUTINE W3SNL1
  375 !/ ------------------------------------------------------------------- /
  376       SUBROUTINE INSNL1 ( IMOD )
  377 !/
  378 !/                  +-----------------------------------+
  379 !/                  | WAVEWATCH III           NOAA/NCEP |
  380 !/                  |           H. L. Tolman            |
  381 !/                  |                        FORTRAN 90 |
  382 !/                  | Last update :         24-Dec-2004 |
  383 !/                  +-----------------------------------+
  384 !/
  385 !/    19-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
 
 
                                                         w3snl1md.F90  page   8
 
 
  386 !/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
  387 !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
  388 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
  389 !/
  390 !  1. Purpose :
  391 !
  392 !     Preprocessing for nonlinear interactions (weights).
  393 !
  394 !  2. Method :
  395 !
  396 !     See W3SNL1.
  397 !
  398 !  3. Parameters :
  399 !
  400 !     Parameter list
  401 !     ----------------------------------------------------------------
  402 !       IMOD    Int.  I   Model number.
  403 !     ----------------------------------------------------------------
  404 !
  405 !     Local variables
  406 !     ----------------------------------------------------------------
  407 !       ITHxn   Real  Directional indices.                 (relative)
  408 !       IFRxn   Real  Frequency indices.                   (relative)
  409 !       IT1     R.A.  Directional indices.                      (1-D)
  410 !       IFn     R.A.  Frequency indices.                        (1-D)
  411 !     ----------------------------------------------------------------
  412 !
  413 !  4. Subroutines used :
  414 !
  415 !      Name      Type  Module   Description
  416 !     ----------------------------------------------------------------
  417 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  418 !     ----------------------------------------------------------------
  419 !
  420 !  5. Called by :
  421 !
  422 !      Name      Type  Module   Description
  423 !     ----------------------------------------------------------------
  424 !      W3IOGR    Subr. W3IOGRMD Model definition file processing.
  425 !     ----------------------------------------------------------------
  426 !
  427 !  6. Error messages :
  428 !
  429 !     - Check on array dimensions for local arrays in W3SNL.
  430 !
  431 !  7. Remarks :
  432 !
  433 !     - Test output is generated through W3IOGR.
  434 !     - No testing of IMOD ir resetting of pointers.
  435 !
  436 !  8. Structure :
  437 !
  438 !     - See source code.
  439 !
  440 !  9. Switches :
 
 
                                                         w3snl1md.F90  page   9
 
 
  441 !
  442 !       !/S      Enable subroutine tracing.
  443 !
  444 ! 10. Source code :
  445 !
  446 !/ ------------------------------------------------------------------- /
  447       USE CONSTANTS
  448       USE W3GDATMD, ONLY: NK, NTH, NSPEC, DTH, XFR, SIG, LAM
  449       USE W3ADATMD, ONLY: W3DMNL
  450       USE W3ADATMD, ONLY: NFR, NFRHGH, NFRCHG, NSPECX, NSPECY,        &
  451                     IP11, IP12, IP13, IP14, IM11, IM12, IM13, IM14,   &
  452                     IP21, IP22, IP23, IP24, IM21, IM22, IM23, IM24,   &
  453                     IC11, IC12, IC21, IC22, IC31, IC32, IC41, IC42,   &
  454                     IC51, IC52, IC61, IC62, IC71, IC72, IC81, IC82,   &
  455                     DAL1, DAL2, DAL3, AF11,                           &
  456                     AWG1, AWG2, AWG3, AWG4, AWG5, AWG6, AWG7, AWG8,   &
  457                     SWG1, SWG2, SWG3, SWG4, SWG5, SWG6, SWG7, SWG8
  458       USE W3ODATMD, ONLY: NDST, NDSE
  459 !/
  460       IMPLICIT NONE
  461 !/
  462 !/ ------------------------------------------------------------------- /
  463 !/ Parameter list
  464 !/
  465       INTEGER, INTENT(IN)     :: IMOD
  466 !/
  467 !/ Local parameters
  468 !/
  469       INTEGER                 :: IFR, ITH, ISP, ITHP, ITHP1, ITHM,    &
  470                                  ITHM1,IFRP, IFRP1, IFRM, IFRM1
  471       INTEGER, ALLOCATABLE    :: IF1(:), IF2(:), IF3(:), IF4(:),      &
  472                                  IF5(:), IF6(:), IF7(:), IF8(:),      &
  473                                  IT1(:), IT2(:), IT3(:), IT4(:),      &
  474                                  IT5(:), IT6(:), IT7(:), IT8(:)
  475       REAL                    :: DELTH3, DELTH4, LAMM2, LAMP2, CTHP,  &
  476                                  WTHP, WTHP1, CTHM, WTHM, WTHM1,      &
  477                                  XFRLN, WFRP, WFRP1, WFRM, WFRM1, FR, &
  478                                  AF11A
  479 !/
  480 !/ ------------------------------------------------------------------- /
  481 !/
  482 !
  483       NFR     = NK
  484 !
  485 ! 1.  Internal angles of quadruplet.
  486 !
  487       LAMM2  = (1.-LAM)**2
  488       LAMP2  = (1.+LAM)**2
  489       DELTH3 = ACOS( (LAMM2**2+4.-LAMP2**2) / (4.*LAMM2) )
  490       DELTH4 = ASIN(-SIN(DELTH3)*LAMM2/LAMP2)
  491 !
  492 ! 2.  Lambda dependend weight factors.
  493 !
  494       DAL1   = 1. / (1.+LAM)**4
  495       DAL2   = 1. / (1.-LAM)**4
 
 
                                                         w3snl1md.F90  page  10
 
 
  496       DAL3   = 2. * DAL1 * DAL2
  497 !
  498 ! 3.  Directional indices.
  499 !
  500       CTHP   = ABS(DELTH4/DTH)
  501       ITHP   = INT(CTHP)
  502       ITHP1  = ITHP + 1
  503       WTHP   = CTHP - REAL(ITHP)
  504       WTHP1  = 1.- WTHP
  505 !
  506       CTHM   = ABS(DELTH3/DTH)
  507       ITHM   = INT(CTHM)
  508       ITHM1  = ITHM + 1
  509       WTHM   = CTHM - REAL(ITHM)
  510       WTHM1  = 1.- WTHM
  511 !
  512 ! 4.  Frequency indices.
  513 !
  514       XFRLN  = LOG(XFR)
  515 !
  516       IFRP   = INT( LOG(1.+LAM) / XFRLN )
  517       IFRP1  = IFRP + 1
  518       WFRP   = (1.+LAM - XFR**IFRP) / (XFR**IFRP1 - XFR**IFRP)
  519       WFRP1  = 1. - WFRP
  520 !
  521       IFRM   = INT( LOG(1.-LAM) / XFRLN )
  522       IFRM1  = IFRM - 1
  523       WFRM   = (XFR**IFRM -(1.-LAM)) / (XFR**IFRM - XFR**IFRM1)
  524       WFRM1  = 1. - WFRM
  525 !
  526 ! 5.  Range of calculations
  527 !
  528       NFRHGH = NFR + IFRP1 - IFRM1
  529       NFRCHG = NFR - IFRM1
  530       NSPECY = NFRHGH * NTH
  531       NSPECX = NFRCHG * NTH
  532 !
  533 ! 6.  Allocate arrays or check array sizes
  534 !
  535       CALL W3DMNL ( IMOD, NDSE, NDST, NSPEC, NSPECX )
  536 !
  537       ALLOCATE ( IF1(NFRCHG), IF2(NFRCHG), IF3(NFRCHG), IF4(NFRCHG),  &
  538                  IF5(NFRCHG), IF6(NFRCHG), IF7(NFRCHG), IF8(NFRCHG),  &
  539                  IT1(NTH), IT2(NTH), IT3(NTH), IT4(NTH),              &
  540                  IT5(NTH), IT6(NTH), IT7(NTH), IT8(NTH) )
  541 !
  542 ! 7.  Spectral addresses
  543 !
  544       DO IFR=1, NFRCHG
  545         IF1(IFR) =           IFR+IFRP
  546         IF2(IFR) =           IFR+IFRP1
  547         IF3(IFR) = MAX ( 0 , IFR+IFRM  )
  548         IF4(IFR) = MAX ( 0 , IFR+IFRM1 )
  549         IF5(IFR) = MAX ( 0 , IFR-IFRP  )
  550         IF6(IFR) = MAX ( 0 , IFR-IFRP1 )
 
 
                                                         w3snl1md.F90  page  11
 
 
  551         IF7(IFR) =           IFR-IFRM
  552         IF8(IFR) =           IFR-IFRM1
  553         END DO
  554 !
  555       DO ITH=1, NTH
  556         IT1(ITH) = ITH + ITHP
  557         IT2(ITH) = ITH + ITHP1
  558         IT3(ITH) = ITH + ITHM
  559         IT4(ITH) = ITH + ITHM1
  560         IT5(ITH) = ITH - ITHP
  561         IT6(ITH) = ITH - ITHP1
  562         IT7(ITH) = ITH - ITHM
  563         IT8(ITH) = ITH - ITHM1
  564         IF ( IT1(ITH).GT.NTH) IT1(ITH) = IT1(ITH) - NTH
  565         IF ( IT2(ITH).GT.NTH) IT2(ITH) = IT2(ITH) - NTH
  566         IF ( IT3(ITH).GT.NTH) IT3(ITH) = IT3(ITH) - NTH
  567         IF ( IT4(ITH).GT.NTH) IT4(ITH) = IT4(ITH) - NTH
  568         IF ( IT5(ITH).LT. 1 ) IT5(ITH) = IT5(ITH) + NTH
  569         IF ( IT6(ITH).LT. 1 ) IT6(ITH) = IT6(ITH) + NTH
  570         IF ( IT7(ITH).LT. 1 ) IT7(ITH) = IT7(ITH) + NTH
  571         IF ( IT8(ITH).LT. 1 ) IT8(ITH) = IT8(ITH) + NTH
  572         END DO
  573 !
  574       DO ISP=1, NSPECX
  575         IFR       = 1 + (ISP-1)/NTH
  576         ITH       = 1 + MOD(ISP-1,NTH)
  577         IP11(ISP) = IT2(ITH) + (IF2(IFR)-1)*NTH
  578         IP12(ISP) = IT1(ITH) + (IF2(IFR)-1)*NTH
  579         IP13(ISP) = IT2(ITH) + (IF1(IFR)-1)*NTH
  580         IP14(ISP) = IT1(ITH) + (IF1(IFR)-1)*NTH
  581         IM11(ISP) = IT8(ITH) + (IF4(IFR)-1)*NTH
  582         IM12(ISP) = IT7(ITH) + (IF4(IFR)-1)*NTH
  583         IM13(ISP) = IT8(ITH) + (IF3(IFR)-1)*NTH
  584         IM14(ISP) = IT7(ITH) + (IF3(IFR)-1)*NTH
  585         IP21(ISP) = IT6(ITH) + (IF2(IFR)-1)*NTH
  586         IP22(ISP) = IT5(ITH) + (IF2(IFR)-1)*NTH
  587         IP23(ISP) = IT6(ITH) + (IF1(IFR)-1)*NTH
  588         IP24(ISP) = IT5(ITH) + (IF1(IFR)-1)*NTH
  589         IM21(ISP) = IT4(ITH) + (IF4(IFR)-1)*NTH
  590         IM22(ISP) = IT3(ITH) + (IF4(IFR)-1)*NTH
  591         IM23(ISP) = IT4(ITH) + (IF3(IFR)-1)*NTH
  592         IM24(ISP) = IT3(ITH) + (IF3(IFR)-1)*NTH
  593         END DO
  594 !
  595       DO ISP=1, NSPEC
  596         IFR       = 1 + (ISP-1)/NTH
  597         ITH       = 1 + MOD(ISP-1,NTH)
  598         IC11(ISP) = IT6(ITH) + (IF6(IFR)-1)*NTH
  599         IC21(ISP) = IT5(ITH) + (IF6(IFR)-1)*NTH
  600         IC31(ISP) = IT6(ITH) + (IF5(IFR)-1)*NTH
  601         IC41(ISP) = IT5(ITH) + (IF5(IFR)-1)*NTH
  602         IC51(ISP) = IT4(ITH) + (IF8(IFR)-1)*NTH
  603         IC61(ISP) = IT3(ITH) + (IF8(IFR)-1)*NTH
  604         IC71(ISP) = IT4(ITH) + (IF7(IFR)-1)*NTH
  605         IC81(ISP) = IT3(ITH) + (IF7(IFR)-1)*NTH
 
 
                                                         w3snl1md.F90  page  12
 
 
  606         IC12(ISP) = IT2(ITH) + (IF6(IFR)-1)*NTH
  607         IC22(ISP) = IT1(ITH) + (IF6(IFR)-1)*NTH
  608         IC32(ISP) = IT2(ITH) + (IF5(IFR)-1)*NTH
  609         IC42(ISP) = IT1(ITH) + (IF5(IFR)-1)*NTH
  610         IC52(ISP) = IT8(ITH) + (IF8(IFR)-1)*NTH
  611         IC62(ISP) = IT7(ITH) + (IF8(IFR)-1)*NTH
  612         IC72(ISP) = IT8(ITH) + (IF7(IFR)-1)*NTH
  613         IC82(ISP) = IT7(ITH) + (IF7(IFR)-1)*NTH
  614         END DO
  615 !
  616       DEALLOCATE ( IF1, IF2, IF3, IF4, IF5, IF6, IF7, IF8,  &
  617                    IT1, IT2, IT3, IT4, IT5, IT6, IT7, IT8 )
  618 !
  619 ! 8.  Fill scaling array (f**11)
  620 !
  621       DO IFR=1, NFR
  622         AF11A  = (SIG(IFR)*TPIINV)**11
  623         DO ITH=1, NTH
  624           AF11(ITH+(IFR-1)*NTH) = AF11A
  625           END DO
  626         END DO
  627 !
  628       FR     = SIG(NFR)*TPIINV
  629       DO IFR=NFR+1, NFRCHG
  630         FR     = FR * XFR
  631         AF11A  = FR**11
  632         DO ITH=1, NTH
  633           AF11(ITH+(IFR-1)*NTH) = AF11A
  634           END DO
  635         END DO
  636 !
  637 ! 9.  Interpolation weights
  638 !
  639       AWG1   = WTHP  * WFRP
  640       AWG2   = WTHP1 * WFRP
  641       AWG3   = WTHP  * WFRP1
  642       AWG4   = WTHP1 * WFRP1
  643       AWG5   = WTHM  * WFRM
  644       AWG6   = WTHM1 * WFRM
  645       AWG7   = WTHM  * WFRM1
  646       AWG8   = WTHM1 * WFRM1
  647 !
  648       SWG1   = AWG1**2
  649       SWG2   = AWG2**2
  650       SWG3   = AWG3**2
  651       SWG4   = AWG4**2
  652       SWG5   = AWG5**2
  653       SWG6   = AWG6**2
  654       SWG7   = AWG7**2
  655       SWG8   = AWG8**2
  656 !
  657       RETURN
  658 !
  659 ! Formats
  660 !
 
 
                                                         w3snl1md.F90  page  13
 
 
  661 !/
  662 !/ End of INSNL1 ----------------------------------------------------- /
  663 !/
  664       END SUBROUTINE INSNL1
  665 !/
  666 !/ End of module W3SNL1MD -------------------------------------------- /
  667 !/
  668       END MODULE W3SNL1MD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
