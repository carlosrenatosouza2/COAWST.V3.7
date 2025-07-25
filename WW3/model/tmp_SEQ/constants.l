                                                        constants.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE CONSTANTS
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         05-Jun-2018 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    11-Nov-1999 : Fortran 90 version.                 ( version 2.00 )
   13 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   14 !/    25-Jun-2011 : Adding Kelvin functions.            ( version 4.05 )
   15 !/    03-Sep-2012 : Adding TSTOUT flag.                 ( version 4.10 )
   16 !/    28-Feb-2013 : Adding cap at 0.5 in FWTABLE        ( version 4.08 )
   17 !/    20-Jan-2017 : Add parameters for ESMF             ( version 6.02 )
   18 !/    01-Mar-2018 : Add UNDEF parameter                 ( version 6.02 )
   19 !/    05-Jun-2018 : Add PDLIB parameters                ( version 6.04 )
   20 !/
   21 !/    Copyright 2009-2012 National Weather Service (NWS),
   22 !/       National Oceanic and Atmospheric Administration.  All rights
   23 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   24 !/       No unauthorized use without permission.
   25 !/
   26 !  1. Purpose :
   27 !
   28 !     Define some much-used constants for global use (all defined
   29 !     as PARAMETER).
   30 !
   31 !  2. Variables and types :
   32 !
   33 !      Name      Type  Scope    Description
   34 !     ----------------------------------------------------------------
   35 !      GRAV      Real  Global   Acc. of gravity                 (m/s2)
   36 !      DWAT      Real  Global   Density of water               (kg/m3)
   37 !      DAIR      Real  Global   Density of air                 (kg/m3)
   38 !      NU_AIR    Real  Global   Kinematic viscosity of air      (m2/s)
   39 !      NU_WATER  Real  Global   Kinematic viscosity of water    (m2/s)
   40 !      SED_SG    Real  Global   Specific gravity of sediments   (N.D.)
   41 !      KAPPA     Real  Global   von Karman's constant           (N.D.)
   42 !      PI        Real  Global   pi.
   43 !      TPI       Real  Global   2pi.
   44 !      HPI       Real  Global   0.5pi.
   45 !      TPIINV    Real  Global   1/2pi.
   46 !      HPIINV    Real  Global   2/pi.
   47 !      RADE      Real  Global   Conv. factor from radians to degrees.
   48 !      DERA      Real  Global   Conv. factor from degrees to radians.
   49 !      RADIUS    Real  Global   Radius of the earth.             (m)
   50 !      TSTOUT    Log.  Global   Flag for generation of test files.
   51 !      UNDEF     Real  Global   Value for undefined variable in output
   52 !     ----------------------------------------------------------------
   53 !
   54 !  5. Remarks
   55 !
 
 
                                                        constants.F90  page   2
 
 
   56 !      - The flag for generating test output files is included here as
   57 !        it is needed in both ww3_shel and ww3_multi at the same time.
   58 !        Make sure that this flag is true if you want to write to the
   59 !        test output file !
   60 !
   61 !/ ------------------------------------------------------------------- /
   62 !/
   63       LOGICAL, PARAMETER      :: TSTOUT = .FALSE.
   64 !
   65       REAL, PARAMETER         :: GRAV   =    9.806
   66       REAL, PARAMETER         :: DWAT   = 1000.
   67       REAL, PARAMETER         :: DAIR   =    1.225
   68       REAL, PARAMETER         :: nu_air  = 1.4E-5
   69 !mdo  *** Changing nu_water to be consistent with DWAT=1000 (assumes 10de>
   70       REAL, PARAMETER         :: nu_water  = 1.31E-6    !mdo   WAS: 3.E-6
   71       REAL, PARAMETER         :: sed_sg  = 2.65
   72       REAL, PARAMETER         :: KAPPA = 0.40       !Von Karman's constant
   73 !
   74       REAL, PARAMETER         :: PI     = 3.141592653589793
   75       REAL, PARAMETER         :: TPI    = 2.0 * PI
   76       REAL, PARAMETER         :: HPI    = 0.5 * PI
   77       REAL, PARAMETER         :: TPIINV = 1. / TPI
   78       REAL, PARAMETER         :: HPIINV = 1. / HPI
   79       REAL, PARAMETER         :: RADE   = 180. / PI
   80       REAL, PARAMETER         :: DERA   = PI / 180.
   81 !
   82       REAL, PARAMETER         :: RADIUS = 4.E7 * TPIINV
   83 !
   84       REAL, PARAMETER         :: G2PI3I = 1. / ( GRAV**2 * TPI**3 )
   85       REAL, PARAMETER         :: G1PI1I = 1. / ( GRAV * TPI )
   86 !
   87       REAL                    :: UNDEF = -999.9
   88 !
   89 ! Parameters for friction factor table
   90 !
   91       INTEGER, PARAMETER       :: SIZEFWTABLE=300
   92       REAL                     :: FWTABLE(0:SIZEFWTABLE)
   93       REAL                     :: DELAB
   94       REAL,    PARAMETER       :: ABMIN = -1.
   95       REAL, PRIVATE, PARAMETER :: ABMAX = 8.
   96       INTEGER, PARAMETER       :: srce_direct = 0
   97       INTEGER, PARAMETER       :: srce_imp_post = 1
   98       INTEGER, PARAMETER       :: srce_imp_pre = 2
   99       INTEGER, PARAMETER       :: DEBUG_NODE = 1014
  100       INTEGER, PARAMETER       :: DEBUG_ELEMENT = 50
  101       LOGICAL                  :: LPDLIB = .FALSE.
  102       LOGICAL                  :: LSETUP
  103 !
  104 ! Parameters in support of running as ESMF component
  105 !
  106 ! --- Flag indicating whether or not the model has been invoked as an
  107 !     ESMF Component.  This flag is set to true in the WMESMFMD ESMF
  108 !     module during initialization.
  109       LOGICAL :: IS_ESMF_COMPONENT = .FALSE.
  110 !
 
 
                                                        constants.F90  page   3
 
 
  111       CONTAINS
  112 ! ----------------------------------------------------------------------
  113       SUBROUTINE TABU_FW
  114 !/
  115 !/                  +-----------------------------------+
  116 !/                  | WAVEWATCH III           NOAA/NCEP |
  117 !/                  |            F. Ardhuin             |
  118 !/                  |                        FORTRAN 90 |
  119 !/                  | Last update :         28-Feb-2013 |
  120 !/                  +-----------------------------------+
  121 !/
  122 !/    19-Oct-2007 : Origination.                        ( version 3.13 )
  123 !/    28-Feb-2013 : Caps the friction factor to 0.5     ( version 4.08 )
  124 !/
  125 !  1. Purpose :
  126 !     TO estimate friction coefficients in oscillatory boundary layers
  127 !     METHOD.
  128 !      tabulation on Kelvin functions
  129 !
  130 !  2. Method :
  131 !
  132 !  3. Parameters :
  133 !
  134 !     Parameter list
  135 !     ----------------------------------------------------------------
  136 !     ----------------------------------------------------------------
  137 !
  138 !  4. Subroutines used :
  139 !
  140 !      Name      Type  Module   Description
  141 !     ----------------------------------------------------------------
  142 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  143 !     ----------------------------------------------------------------
  144 !
  145 !  5. Called by :
  146 !
  147 !      Name      Type  Module   Description
  148 !     ----------------------------------------------------------------
  149 !      WW3_GRID  Prog. WW3_GRID Model grid initialization
  150 !     ----------------------------------------------------------------
  151 !
  152 !  6. Error messages :
  153 !
  154 !       None.
  155 !
  156 !  7. Remarks :
  157 !
  158 !  8. Structure :
  159 !
  160 !     See source code.
  161 !
  162 !  9. Switches :
  163 !
  164 !     !/S  Enable subroutine tracing.
  165 !
 
 
                                                        constants.F90  page   4
 
 
  166 ! 10. Source code :
  167 !
  168 !/ ------------------------------------------------------------------- /
  169       IMPLICIT NONE
  170       INTEGER, PARAMETER      :: NITER=100
  171       REAL   , PARAMETER      :: XM=0.50, EPS1=0.00001
  172 !     VARIABLE.   TYPE.     PURPOSE.
  173 !      *XM*        REAL      POWER OF TAUW/TAU IN ROUGHNESS LENGTH.
  174 !      *XNU*       REAL      KINEMATIC VISCOSITY OF AIR.
  175 !      *NITER*     INTEGER   NUMBER OF ITERATIONS TO OBTAIN TOTAL STRESS
  176 !      *EPS1*      REAL      SMALL NUMBER TO MAKE SURE THAT A SOLUTION
  177 !                            IS OBTAINED IN ITERATION WITH TAU>TAUW.
  178 ! ----------------------------------------------------------------------
  179       INTEGER I,ITER
  180       REAL KER, KEI
  181       REAL ABR,ABRLOG,L10,FACT,FSUBW,FSUBWMEMO,dzeta0,dzeta0memo
  182 !
  183       DELAB   = (ABMAX-ABMIN)/REAL(SIZEFWTABLE)
  184       L10=ALOG(10.)
  185       DO I=0,SIZEFWTABLE
  186 !
  187 !  index I in this table corresponds to a normalized roughness z0/ABR = 1>
  188 !
  189          ABRLOG=ABMIN+REAL(I)*DELAB
  190          ABR=EXP(ABRLOG*L10)
  191          FACT=1/ABR/(21.2*KAPPA)
  192          FSUBW=0.05
  193          dzeta0=0.
  194          DO ITER=1,NITER
  195             fsubwmemo=fsubw
  196             dzeta0memo=dzeta0
  197             dzeta0=fact*fsubw**(-.5)
  198             CALL KERKEI(2.*SQRT(dzeta0),ker,kei)
  199             fsubw=.08/(ker**2+kei**2)
  200             fsubw=.5*(fsubwmemo+fsubw)
  201             dzeta0=.5*(dzeta0memo+dzeta0)
  202             END DO
  203 !
  204 ! Maximum value of 0.5 for fe is based on field
  205 ! and lab experiment by Lowe et al. JGR 2005, 2007
  206 !
  207             FWTABLE(I)  = MIN(fsubw,0.5)
  208 !           WRITE(994,*) 'Friction factor:',I,ABR,FWTABLE(I)
  209          END DO
  210       RETURN
  211       END SUBROUTINE TABU_FW
  212                             
  213 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  214 SUBROUTINE KZEONE(X, Y, RE0, IM0, RE1, IM1)
  215 !  June 1999 adaptation to CRESTb, all tests on range of (x,y) have been
  216 !  bypassed, we implicitly expect X to be positive or |x,y| non zero
  217 !
  218 ! This subroutine is copyright by ACM
  219 ! see http://www.acm.org/pubs/copyright_policy/softwareCRnotice.html
  220 ! ACM declines any responsibility of any kind
 
 
                                                        constants.F90  page   5
 
 
  221 !
  222 ! THE VARIABLES X AND Y ARE THE REAL AND IMAGINARY PARTS OF
  223 ! THE ARGUMENT OF THE FIRST TWO MODIFIED BESSEL FUNCTIONS
  224 ! OF THE SECOND KIND,K0 AND K1.  RE0,IM0,RE1 AND IM1 GIVE
  225 ! THE REAL AND IMAGINARY PARTS OF EXP(X)*K0 AND EXP(X)*K1,
  226 ! RESPECTIVELY.  ALTHOUGH THE REAL NOTATION USED IN THIS
  227 ! SUBROUTINE MAY SEEM INELEGANT WHEN COMPARED WITH THE
  228 ! COMPLEX NOTATION THAT FORTRAN ALLOWS, THIS VERSION RUNS
  229 ! ABOUT 30 PERCENT FASTER THAN ONE WRITTEN USING COMPLEX
  230 ! VARIABLES.
  231 ! ACM Libraries
  232 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  233    IMPLICIT NONE
  234    DOUBLE PRECISION X, Y, X2, Y2, RE0, IM0, RE1, IM1, &
  235       R1, R2, T1, T2, P1, P2, RTERM, ITERM, L
  236    DOUBLE PRECISION , PARAMETER, DIMENSION(8) :: EXSQ = &
  237          (/ 0.5641003087264D0,0.4120286874989D0,0.1584889157959D0, &
  238             0.3078003387255D-1,0.2778068842913D-2,0.1000044412325D-3, &
  239             0.1059115547711D-5,0.1522475804254D-8 /)
  240    DOUBLE PRECISION , PARAMETER, DIMENSION(8) :: TSQ = &
  241          (/ 0.0D0,3.19303633920635D-1,1.29075862295915D0, &
  242             2.95837445869665D0,5.40903159724444D0,8.80407957805676D0, &
  243             1.34685357432515D1,2.02499163658709D1 /)
  244    INTEGER N,M,K
  245 ! THE ARRAYS TSQ AND EXSQ CONTAIN THE SQUARE OF THE
  246 ! ABSCISSAS AND THE WEIGHT FACTORS USED IN THE GAUSS-
  247 ! HERMITE QUADRATURE.
  248       R2 = X*X + Y*Y
  249       IF (R2.GE.1.96D2) GO TO 50
  250       IF (R2.GE.1.849D1) GO TO 30
  251 ! THIS SECTION CALCULATES THE FUNCTIONS USING THE SERIES
  252 ! EXPANSIONS
  253       X2 = X/2.0D0
  254       Y2 = Y/2.0D0
  255       P1 = X2*X2
  256       P2 = Y2*Y2
  257       T1 = -(DLOG(P1+P2)/2.0D0+0.5772156649015329D0)
  258 ! THE CONSTANT IN THE PRECEDING STATEMENT IS EULER*S
  259 ! CONSTANT
  260       T2 = -DATAN2(Y,X)
  261       X2 = P1 - P2
  262       Y2 = X*Y2
  263       RTERM = 1.0D0
  264       ITERM = 0.0D0
  265       RE0 = T1
  266       IM0 = T2
  267       T1 = T1 + 0.5D0
  268       RE1 = T1
  269       IM1 = T2
  270       P2 = DSQRT(R2)
  271       L = 2.106D0*P2 + 4.4D0
  272       IF (P2.LT.8.0D-1) L = 2.129D0*P2 + 4.0D0
  273       DO 20 N=1,INT(L)
  274         P1 = N
  275         P2 = N*N
 
 
                                                        constants.F90  page   6
 
 
  276         R1 = RTERM
  277         RTERM = (R1*X2-ITERM*Y2)/P2
  278         ITERM = (R1*Y2+ITERM*X2)/P2
  279         T1 = T1 + 0.5D0/P1
  280         RE0 = RE0 + T1*RTERM - T2*ITERM
  281         IM0 = IM0 + T1*ITERM + T2*RTERM
  282         P1 = P1 + 1.0D0
  283         T1 = T1 + 0.5D0/P1
  284         RE1 = RE1 + (T1*RTERM-T2*ITERM)/P1
  285         IM1 = IM1 + (T1*ITERM+T2*RTERM)/P1
  286    20 CONTINUE
  287       R1 = X/R2 - 0.5D0*(X*RE1-Y*IM1)
  288       R2 = -Y/R2 - 0.5D0*(X*IM1+Y*RE1)
  289       P1 = DEXP(X)
  290       RE0 = P1*RE0
  291       IM0 = P1*IM0
  292       RE1 = P1*R1
  293       IM1 = P1*R2
  294       RETURN
  295 ! THIS SECTION CALCULATES THE FUNCTIONS USING THE INTEGRAL
  296 ! REPRESENTATION, EQN 3, EVALUATED WITH 15 POINT GAUSS-
  297 ! HERMITE QUADRATURE
  298    30 X2 = 2.0D0*X
  299       Y2 = 2.0D0*Y
  300       R1 = Y2*Y2
  301       P1 = DSQRT(X2*X2+R1)
  302       P2 = DSQRT(P1+X2)
  303       T1 = EXSQ(1)/(2.0D0*P1)
  304       RE0 = T1*P2
  305       IM0 = T1/P2
  306       RE1 = 0.0D0
  307       IM1 = 0.0D0
  308       DO 40 N=2,8
  309         T2 = X2 + TSQ(N)
  310         P1 = DSQRT(T2*T2+R1)
  311         P2 = DSQRT(P1+T2)
  312         T1 = EXSQ(N)/P1
  313         RE0 = RE0 + T1*P2
  314         IM0 = IM0 + T1/P2
  315         T1 = EXSQ(N)*TSQ(N)
  316         RE1 = RE1 + T1*P2
  317         IM1 = IM1 + T1/P2
  318    40 CONTINUE
  319       T2 = -Y2*IM0
  320       RE1 = RE1/R2
  321       R2 = Y2*IM1/R2
  322       RTERM = 1.41421356237309D0*DCOS(Y)
  323       ITERM = -1.41421356237309D0*DSIN(Y)
  324 ! THE CONSTANT IN THE PREVIOUS STATEMENTS IS,OF COURSE,
  325 ! SQRT(2.0).
  326       IM0 = RE0*ITERM + T2*RTERM
  327       RE0 = RE0*RTERM - T2*ITERM
  328       T1 = RE1*RTERM - R2*ITERM
  329       T2 = RE1*ITERM + R2*RTERM
  330       RE1 = T1*X + T2*Y
 
 
                                                        constants.F90  page   7
 
 
  331       IM1 = -T1*Y + T2*X
  332       RETURN
  333 ! THIS SECTION CALCULATES THE FUNCTIONS USING THE
  334 ! ASYMPTOTIC EXPANSIONS
  335    50 RTERM = 1.0D0
  336       ITERM = 0.0D0
  337       RE0 = 1.0D0
  338       IM0 = 0.0D0
  339       RE1 = 1.0D0
  340       IM1 = 0.0D0
  341       P1 = 8.0D0*R2
  342       P2 = DSQRT(R2)
  343       L = 3.91D0+8.12D1/P2
  344       R1 = 1.0D0
  345       R2 = 1.0D0
  346       M = -8
  347       K = 3
  348       DO 60 N=1,INT(L)
  349         M = M + 8
  350         K = K - M
  351         R1 = FLOAT(K-4)*R1
  352         R2 = FLOAT(K)*R2
  353         T1 = FLOAT(N)*P1
  354         T2 = RTERM
  355         RTERM = (T2*X+ITERM*Y)/T1
  356         ITERM = (-T2*Y+ITERM*X)/T1
  357         RE0 = RE0 + R1*RTERM
  358         IM0 = IM0 + R1*ITERM
  359         RE1 = RE1 + R2*RTERM
  360         IM1 = IM1 + R2*ITERM
  361    60 CONTINUE
  362       T1 = DSQRT(P2+X)
  363       T2 = -Y/T1
  364       P1 = 8.86226925452758D-1/P2
  365 ! THIS CONSTANT IS SQRT(PI)/2.0, WITH PI=3.14159...
  366       RTERM = P1*DCOS(Y)
  367       ITERM = -P1*DSIN(Y)
  368       R1 = RE0*RTERM - IM0*ITERM
  369       R2 = RE0*ITERM + IM0*RTERM
  370       RE0 = T1*R1 - T2*R2
  371       IM0 = T1*R2 + T2*R1
  372       R1 = RE1*RTERM - IM1*ITERM
  373       R2 = RE1*ITERM + IM1*RTERM
  374       RE1 = T1*R1 - T2*R2
  375       IM1 = T1*R2 + T2*R1
  376       RETURN
  377       END SUBROUTINE KZEONE
  378                            
  379 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  380 SUBROUTINE KERKEI(X,KER,KEI)
  381 !**********************************************************************
  382 ! Computes the values of the zeroth order Kelvin function Ker and Kei
  383 ! These functions are used to determine the friction factor fw as a
  384 ! function of the bottom roughness length assuming a linear profile
  385 ! of eddy viscosity (See Grant and Madsen, 1979)
 
 
                                                        constants.F90  page   8
 
 
  386 !**********************************************************************
  387    IMPLICIT NONE
  388                 
  389    DOUBLE PRECISION ZR,ZI,CYR,CYI,CYR1,CYI1
  390    REAL X,KER,KEI
  391                  
  392    ZR=X*.50D0*SQRT(2.0D0)
  393    ZI=ZR
  394    CALL KZEONE(ZR, ZI, CYR, CYI,CYR1,CYI1)
  395    KER=CYR/EXP(ZR)
  396    KEI=CYI/EXP(ZR)
  397 END SUBROUTINE KERKEI
  398 !/
  399 !/ End of module CONSTANTS ------------------------------------------- /
  400 !/
  401       END MODULE CONSTANTS
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
