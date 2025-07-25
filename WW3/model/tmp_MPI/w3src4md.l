                                                         w3src4md.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3SRC4MD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III                SHOM |
    7 !/                  !            F. Ardhuin             !
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         13-Nov-2013 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    30-Aug-2010 : Origination.                        ( version 3.14-If>
   13 !/    02-Nov-2010 : Addding fudge factor for low freq.  ( version 4.03 )
   14 !/    02-Sep-2011 : Clean up and time optimization      ( version 4.04 )
   15 !/    04-Sep-2011 : Estimation of whitecap stats.       ( version 4.04 )
   16 !/    13-Nov-2013 : Reduced frequency range with IG     ( version 4.13 )
   17 !/
   18 !  1. Purpose :
   19 !
   20 !     The 'SHOM/Ifremer' source terms based on P.A.E.M. Janssen's wind in>
   21 !     and dissipation functions by Ardhuin et al. (2009,2010)
   22 !     and Filipot & Ardhuin (2010)
   23 !     The wind input is converted from the original
   24 !     WAM codes, courtesy of P.A.E.M. Janssen and J. Bidlot
   25 !
   26 !  2. Variables and types :
   27 !
   28 !      Name      Type  Scope    Description
   29 !     ----------------------------------------------------------------
   30 !     ----------------------------------------------------------------
   31 !
   32 !  3. Subroutines and functions :
   33 !
   34 !      Name      Type  Scope    Description
   35 !     ----------------------------------------------------------------
   36 !      W3SPR4    Subr. Public   Mean parameters from spectrum.
   37 !      W3SIN4    Subr. Public   WAM4+ input source term.
   38 !      INSIN4    Subr. Public   Corresponding initialization routine.
   39 !      TABU_STRESS, TABU_TAUHF, TABU_TAUHF2
   40 !                Subr. Public   Populate various tables.
   41 !      CALC_USTAR
   42 !                Subr. Public   Compute stresses.
   43 !      W3SDS4    Subr. Public   Dissipation (Ardhuin & al. / Filipot & Ar>
   44 !     ----------------------------------------------------------------
   45 !
   46 !  4. Subroutines and functions used :
   47 !
   48 !      Name      Type  Module   Description
   49 !     ----------------------------------------------------------------
   50 !     ----------------------------------------------------------------
   51 !
   52 !  5. Remarks :
   53 !
   54 !  6. Switches :
   55 !
 
 
                                                         w3src4md.F90  page   2
 
 
   56 !  7. Source code :
   57 !/
   58 !/ ------------------------------------------------------------------- /
   59 !/
   60       PUBLIC
   61 !/
   62 !/ Public variables
   63 !/
   64       !air kinematic viscosity (used in WAM)
   65       INTEGER, PARAMETER      :: ITAUMAX=200,JUMAX=200
   66       INTEGER, PARAMETER      :: IUSTAR=100,IALPHA=200, ILEVTAIL=50
   67       REAL                    :: TAUT(0:ITAUMAX,0:JUMAX), DELTAUW, DELU
   68       ! Table for H.F. stress as a function of 2 variables
   69       REAL                    :: TAUHFT(0:IUSTAR,0:IALPHA), DELUST, DELALP
   70       ! Table for H.F. stress as a function of 3 variables
   71       REAL                    :: TAUHFT2(0:IUSTAR,0:IALPHA,0:ILEVTAIL)
   72       ! Table for swell damping
   73       REAL                    :: DELTAIL
   74       REAL,    PARAMETER      :: UMAX    = 50.
   75       REAL,    PARAMETER      :: TAUWMAX = 2.2361 !SQRT(5.)
   76       INTEGER                 :: DIKCUMUL
   77 !  Size of wave height table for integrating the PDF of wave heights
   78       INTEGER,    PARAMETER      :: NKHI=100, FAC_KD2=1000
   79       REAL,    PARAMETER      :: FAC_KD1=1.01, KHSMAX=2., KHMAX=2.
   80       REAL,    PARAMETER      ::KDMAX=200000.
   81 !/
   82       CONTAINS
   83 !/ ------------------------------------------------------------------- /
   84       SUBROUTINE W3SPR4 (A, CG, WN, EMEAN, FMEAN, FMEAN1, WNMEAN,     &
   85                     AMAX, U, UDIR, USTAR, USDIR, TAUWX, TAUWY, CD, Z0,&
   86                     CHARN, LLWS, FMEANWS, DLWMEAN)
   87 !/
   88 !/                  +-----------------------------------+
   89 !/                  | WAVEWATCH III                SHOM |
   90 !/                  !            F. Ardhuin             !
   91 !/                  |           H. L. Tolman            |
   92 !/                  |                        FORTRAN 90 |
   93 !/                  | Last update :         22-Feb-2020 |
   94 !/                  +-----------------------------------+
   95 !/
   96 !/    03-Oct-2007 : Origination.                        ( version 3.13 )
   97 !/    13-Jun-2011 : Adds f_m0,-1 as FMEAN in the outout ( version 4.04 )
   98 !/    08-Jun-2018 : use STRACE and FLUSH                ( version 6.04 )
   99 !/    22-Feb-2020 : Merge Romero (2019) and cleanup     ( version 7.06 )
  100 !/
  101 !  1. Purpose :
  102 !
  103 !     Calculate mean wave parameters for the use in the source term
  104 !     routines.
  105 !
  106 !  2. Method :
  107 !
  108 !     See source term routines.
  109 !
  110 !  3. Parameters :
 
 
                                                         w3src4md.F90  page   3
 
 
  111 !
  112 !     Parameter list
  113 !     ----------------------------------------------------------------
  114 !       A       R.A.  I   Action density spectrum.
  115 !       CG      R.A.  I   Group velocities.
  116 !       WN      R.A.  I   Wavenumbers.
  117 !       EMEAN   Real  O   Energy
  118 !       FMEAN1  Real  O   Mean  frequency (fm0,-1) used for reflection
  119 !       FMEAN   Real  O   Mean  frequency for determination of tail
  120 !       WNMEAN  Real  O   Mean wavenumber.
  121 !       AMAX    Real  O   Maximum of action spectrum.
  122 !       U       Real  I   Wind speed.
  123 !       UDIR    Real  I   Wind direction.
  124 !       USTAR   Real I/O  Friction velocity.
  125 !       USDIR   Real I/O  wind stress direction.
  126 !       TAUWX-Y Real  I   Components of wave-supported stress.
  127 !       CD      Real  O   Drag coefficient at wind level ZWND.
  128 !       Z0      Real  O   Corresponding z0.
  129 !       CHARN   Real  O   Corresponding Charnock coefficient
  130 !       LLWS    L.A.  I   Wind sea true/false array for each component
  131 !       FMEANWS Real  O   Mean frequency of wind sea, used for tail
  132 !       DLWMEAN Real  O   Mean Long wave direction  (L. Romero 2019)
  133 !     ----------------------------------------------------------------
  134 !
  135 !  4. Subroutines used :
  136 !
  137 !       STRACE   Service routine.
  138 !
  139 !  5. Called by :
  140 !
  141 !       W3SRCE   Source term integration routine.
  142 !       W3OUTP   Point output program.
  143 !       GXEXPO   GrADS point output program.
  144 !
  145 !  6. Error messages :
  146 !
  147 !  7. Remarks :
  148 !
  149 !  8. Structure :
  150 !
  151 !     See source code.
  152 !
  153 !  9. Switches :
  154 !
  155 !       !/S      Enable subroutine tracing.
  156 !       !/T      Enable test output.
  157 !
  158 ! 10. Source code :
  159 !
  160 !/ ------------------------------------------------------------------- /
  161       USE W3ODATMD, ONLY: IAPROC
  162       USE CONSTANTS, ONLY: TPIINV
  163       USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, DTH, DDEN, WWNMEANP, &
  164                           WWNMEANPTAIL, FTE, FTF, SSTXFTF, SSTXFTWN,&
  165                           SSTXFTFTAIL, SSWELLF, ESIN, ECOS, AAIRCMIN, &
 
 
                                                         w3src4md.F90  page   4
 
 
  166                           AAIRGB
  167 !
  168       IMPLICIT NONE
  169 !/
  170 !/ ------------------------------------------------------------------- /
  171 !/ Parameter list
  172 !/
  173       REAL, INTENT(IN)        :: A(NTH,NK), CG(NK), WN(NK), U, UDIR
  174       REAL, INTENT(IN)        :: TAUWX, TAUWY
  175       LOGICAL, INTENT(IN)     :: LLWS(NSPEC)
  176       REAL, INTENT(INOUT)     :: USTAR ,USDIR
  177       REAL, INTENT(OUT)       :: EMEAN, FMEAN, FMEAN1, WNMEAN, AMAX,  &
  178                                  CD, Z0, CHARN, FMEANWS, DLWMEAN
  179 !/
  180 !/ ------------------------------------------------------------------- /
  181 !/ Local parameters
  182 !/
  183       INTEGER                 :: IS, IK, ITH
  184                                             
  185       REAL                    :: TAUW, EBAND, EMEANWS,UNZ,            &
  186                                  EB(NK),EB2(NK),ELCS, ELSN
  187 !/
  188 !/ ------------------------------------------------------------------- /
  189 !/
  190 !
  191       UNZ    = MAX ( 0.01 , U )
  192       USTAR  = MAX ( 0.0001 , USTAR )
  193 !
  194       EMEAN  = 0.
  195       EMEANWS= 0.
  196       FMEANWS= 0.
  197       FMEAN  = 0.
  198       FMEAN1 = 0.
  199       WNMEAN = 0.
  200       AMAX   = 0.
  201       DLWMEAN =0.
  202       ELCS =0.
  203       ELSN =0.
  204 !
  205 ! 1.  Integral over directions and maximum --------------------------- *
  206 !
  207       DO IK=1, NK
  208         EB(IK)  = 0.
  209         EB2(IK) = 0.
  210         DO ITH=1, NTH
  211           IS=ITH+(IK-1)*NTH
  212           EB(IK) = EB(IK) + A(ITH,IK)
  213           ELCS = ELCS + A(ITH,IK)*ECOS(IS)*DDEN(IK) / CG(IK)
  214           ELSN = ELSN + A(ITH,IK)*ESIN(IS)*DDEN(IK) / CG(IK)
  215           IF (LLWS(IS)) EB2(IK) = EB2(IK) + A(ITH,IK)
  216           AMAX   = MAX ( AMAX , A(ITH,IK) )
  217           END DO
  218         END DO
  219               
  220         DLWMEAN=ATAN2(ELSN,ELCS);
 
 
                                                         w3src4md.F90  page   5
 
 
  221 !
  222 ! 2.  Integrate over directions -------------------------------------- *
  223 !
  224       DO IK=1, NK
  225         EB(IK)   = EB(IK) * DDEN(IK) / CG(IK)
  226         EB2(IK)   = EB2(IK) * DDEN(IK) / CG(IK)
  227         EMEAN    = EMEAN  + EB(IK)
  228         FMEAN    = FMEAN  + EB(IK) /SIG(IK)
  229         FMEAN1   = FMEAN1 + EB(IK) *(SIG(IK)**(2.*WWNMEANPTAIL))
  230         WNMEAN   = WNMEAN + EB(IK) *(WN(IK)**WWNMEANP)
  231         EMEANWS  = EMEANWS+ EB2(IK)
  232         FMEANWS  = FMEANWS+ EB2(IK)*(SIG(IK)**(2.*WWNMEANPTAIL))
  233         END DO
  234 !
  235 ! 3.  Add tail beyond discrete spectrum and get mean pars ------------ *
  236 !     ( DTH * SIG absorbed in FTxx )
  237 !
  238       EBAND  = EB(NK) / DDEN(NK)
  239       EMEAN  = EMEAN  + EBAND * FTE
  240       FMEAN  = FMEAN  + EBAND * FTF
  241       FMEAN1 = FMEAN1 + EBAND * SSTXFTFTAIL
  242       WNMEAN = WNMEAN + EBAND * SSTXFTWN
  243       EBAND  = EB2(NK) / DDEN(NK)
  244       EMEANWS = EMEANWS + EBAND * FTE
  245       FMEANWS = FMEANWS + EBAND * SSTXFTFTAIL
  246 !
  247 ! 4.  Final processing
  248 !
  249       FMEAN  = TPIINV * EMEAN / MAX ( 1.E-7 , FMEAN )
  250       IF (FMEAN1.LT.1.E-7) THEN
  251         FMEAN1=TPIINV * SIG(NK)
  252       ELSE
  253         FMEAN1  = TPIINV *( MAX ( 1.E-7 , FMEAN1 )                       &
  254                      / MAX ( 1.E-7 , EMEAN ))**(1/(2.*WWNMEANPTAIL))
  255         ENDIF
  256       WNMEAN = ( MAX ( 1.E-7 , WNMEAN )                              &
  257                 / MAX ( 1.E-7 , EMEAN ) )**(1/WWNMEANP)
  258       IF (FMEANWS.LT.1.E-7.OR.EMEANWS.LT.1.E-7) THEN
  259         FMEANWS=TPIINV * SIG(NK)
  260       ELSE
  261         FMEANWS  = TPIINV *( MAX ( 1.E-7 , FMEANWS )                     >
  262                      / MAX ( 1.E-7 , EMEANWS ))**(1/(2.*WWNMEANPTAIL))
  263         END IF
  264               
  265 !
  266 ! 5.  Cd and z0 ----------------------------------------------- *
  267 !
  268       TAUW = SQRT(TAUWX**2+TAUWY**2)
  269                                     
  270       Z0=0.
  271       CALL CALC_USTAR(U,TAUW,USTAR,Z0,CHARN)
  272       UNZ    = MAX ( 0.01 , U )
  273       CD     = (USTAR/UNZ)**2
  274       USDIR = UDIR
  275 !
 
 
                                                         w3src4md.F90  page   6
 
 
  276 ! 6.  Final test output ---------------------------------------------- *
  277 !
  278       RETURN
  279 !
  280 ! Formats
  281 !
  282 !/
  283 !/ End of W3SPR4 ----------------------------------------------------- /
  284 !/
  285       END SUBROUTINE W3SPR4
  286 !/ ------------------------------------------------------------------- /
  287       SUBROUTINE W3SIN4 (A, CG, K, U, USTAR, DRAT, AS, USDIR, Z0, CD,    &
  288                          TAUWX, TAUWY, TAUWNX, TAUWNY, S, D, LLWS,       &
  289                          IX, IY, BRLAMBDA)
  290 !/
  291 !/                  +-----------------------------------+
  292 !/                  | WAVEWATCH III                SHOM |
  293 !/                  !            F. Ardhuin             !
  294 !/                  |           H. L. Tolman            |
  295 !/                  |                        FORTRAN 90 |
  296 !/                  | Last update :         05-Dec-2013 |
  297 !/                  +-----------------------------------+
  298 !/
  299 !/    09-Oct-2007 : Origination.                        ( version 3.13 )
  300 !/    24-Jan-2013 : Adding breaking-related input       ( version 4.16 )
  301 !/    05-Dec-2013 : Cleaning up the ICE input           ( version 4.16 )
  302 !/
  303 !  1. Purpose :
  304 !
  305 !     Calculate diagonal and input source term for WAM4+ approach.
  306 !
  307 !  2. Method :
  308 !
  309 !       WAM-4     : Janssen et al.
  310 !       WAM-"4.5" : gustiness effect (Cavaleri et al. )
  311 !       SAT       : high-frequency input reduction for balance with
  312 !                   saturation dissipation (Ardhuin et al., 2008)
  313 !       SWELL     : negative wind input (Ardhuin et al. 2008)
  314 !
  315 !  3. Parameters :
  316 !
  317 !     Parameter list
  318 !     ----------------------------------------------------------------
  319 !       A       R.A.  I   Action density spectrum (1-D).
  320 !       CG      R.A.  I   Group speed                              *)
  321 !       K       R.A.  I   Wavenumber for entire spectrum.          *)
  322 !       U       Real  I   WIND SPEED
  323 !       USTAR   Real  I   Friction velocity.
  324 !       DRAT    Real  I   Air/water density ratio.
  325 !       AS      Real  I   Air-sea temperature difference
  326 !       USDIR   Real  I   wind stress direction
  327 !       Z0      Real  I   Air-side roughness lengh.
  328 !       CD      Real  I   Wind drag coefficient.
  329 !       USDIR   Real  I   Direction of friction velocity
  330 !       TAUWX-Y Real  I   Components of the wave-supported stress.
 
 
                                                         w3src4md.F90  page   7
 
 
  331 !       TAUWNX  Real  I   Component of the negative wave-supported stress.
  332 !       TAUWNY  Real  I   Component of the negative wave-supported stress.
  333 !       S       R.A.  O   Source term (1-D version).
  334 !       D       R.A.  O   Diagonal term of derivative.             *)
  335 !     ----------------------------------------------------------------
  336 !                         *) Stored as 1-D array with dimension NTH*NK
  337 !
  338 !  4. Subroutines used :
  339 !
  340 !       STRACE    Subroutine tracing.                 ( !/S switch )
  341 !       PRT2DS    Print plot of spectrum.             ( !/T0 switch )
  342 !       OUTMAT    Print out matrix.                   ( !/T1 switch )
  343 !
  344 !  5. Called by :
  345 !
  346 !       W3SRCE   Source term integration.
  347 !       W3EXPO   Point output program.
  348 !       GXEXPO   GrADS point output program.
  349 !
  350 !  6. Error messages :
  351 !
  352 !  7. Remarks :
  353 !
  354 !  8. Structure :
  355 !
  356 !     See source code.
  357 !
  358 !  9. Switches :
  359 !
  360 !     !/S   Enable subroutine tracing.
  361 !     !/T   Enable general test output.
  362 !     !/T0  2-D print plot of source term.
  363 !     !/T1  Print arrays.
  364 !
  365 ! 10. Source code :
  366 !
  367 !/ ------------------------------------------------------------------- /
  368       USE CONSTANTS, ONLY: GRAV,nu_air,KAPPA,TPI,FWTABLE,SIZEFWTABLE, &
  369                            DELAB,ABMIN
  370       USE W3GDATMD, ONLY: NK, NTH, NSPEC, DDEN, SIG, SIG2, TH,         &
  371                           ESIN, ECOS, EC2, ZZWND, AALPHA, BBETA, ZZALP,&
  372                           TTAUWSHELTER, SSWELLF, DDEN2, DTH, SSINTHP,  &
  373                           ZZ0RAT, SSINBR
  374       USE W3ODATMD, ONLY: IAPROC
  375 !
  376       IMPLICIT NONE
  377 !/
  378 !/ ------------------------------------------------------------------- /
  379 !/ Parameter list
  380 !/
  381       REAL, INTENT(IN)        :: A(NSPEC), BRLAMBDA(NSPEC)
  382       REAL, INTENT(IN)        :: CG(NK), K(NSPEC),Z0,U, CD
  383       REAL, INTENT(IN)        :: USTAR, USDIR, AS, DRAT
  384       REAL, INTENT(OUT)       :: S(NSPEC), D(NSPEC), TAUWX, TAUWY, TAUWNX>
  385       LOGICAL, INTENT(OUT)    :: LLWS(NSPEC)
 
 
                                                         w3src4md.F90  page   8
 
 
  386       INTEGER, INTENT(IN)     :: IX, IY
  387 !/
  388 !/ ------------------------------------------------------------------- /
  389 !/ Local parameters
  390 !/
  391       INTEGER                 :: IS,IK,ITH
  392       REAL                    :: FACLN1, FACLN2, LAMBDA
  393       REAL                    :: COSU, SINU, TAUX, TAUY, USDIRP, USTP
  394       REAL                    :: TAUPX, TAUPY, UST2, TAUW, TAUWB
  395       REAL   , PARAMETER      :: EPS1 = 0.00001, EPS2 = 0.000001
  396       REAL                    :: Usigma           !standard deviation of >
  397       REAL                    :: USTARsigma       !standard deviation of >
  398       REAL                    :: CM,UCN,ZCN, &
  399                                  Z0VISC, Z0NOZ, EB,  &
  400                                  EBX, EBY, AORB, AORB1, FW, UORB, TH2, &
  401                                  RE, FU, FUD, SWELLCOEFV, SWELLCOEFT
  402       REAL                   ::  PTURB, PVISC, SMOOTH
  403       REAL XI,DELI1,DELI2
  404       REAL XJ,DELJ1,DELJ2
  405       REAL XK,DELK1,DELK2
  406       REAL                    :: CONST, CONST0, CONST2, TAU1
  407       REAL X,ZARG,ZLOG,UST
  408       REAL                    :: COSWIND, XSTRESS, YSTRESS, TAUHF
  409       REAL TEMP, TEMP2
  410       INTEGER IND,J,I,ISTAB
  411       REAL DSTAB(3,NSPEC), DVISC, DTURB
  412       REAL STRESSSTAB(3,2),STRESSSTABN(3,2)
  413 !/
  414 !/ ------------------------------------------------------------------- /
  415 !/
  416 !
  417 ! 1.  Preparations
  418 !
  419       !JDM: Initializing values to zero, they shouldn't be used unless
  420       !set in another place, but seems to solve some bugs with certain
  421       !compilers.
  422       DSTAB =0.
  423       STRESSSTAB =0.
  424       STRESSSTABN =0.
  425 !
  426 ! 1.a  estimation of surface roughness parameters
  427 !
  428       Z0VISC = 0.1*nu_air/MAX(USTAR,0.0001)
  429       Z0NOZ = MAX(Z0VISC,ZZ0RAT*Z0)
  430       FACLN1 = U / LOG(ZZWND/Z0NOZ)
  431       FACLN2 = LOG(Z0NOZ)
  432 !
  433 ! 1.b  estimation of surface orbital velocity and displacement
  434 !
  435       UORB=0.
  436       AORB=0.
  437              
  438       DO IK=1, NK
  439         EB  = 0.
  440         EBX = 0.
 
 
                                                         w3src4md.F90  page   9
 
 
  441         EBY = 0.
  442         DO ITH=1, NTH
  443            IS=ITH+(IK-1)*NTH
  444            EB  = EB  + A(IS)
  445            END DO
  446 !
  447 !  At this point UORB and AORB are the variances of the orbital velocity >
  448 !
  449         UORB = UORB + EB *SIG(IK)**2 * DDEN(IK) / CG(IK)
  450         AORB = AORB + EB             * DDEN(IK) / CG(IK)  !deep water only
  451         END DO
  452               
  453       UORB  = 2*SQRT(UORB)                  ! significant orbital amplitu>
  454       AORB1 = 2*AORB**(1-0.5*SSWELLF(6))    ! half the significant wave h>
  455       RE = 4*UORB*AORB1 / NU_AIR           ! Reynolds number
  456 !
  457 ! Defines the swell dissipation based on the "Reynolds number"
  458 !
  459       IF (SSWELLF(4).GT.0) THEN
  460         IF (SSWELLF(7).GT.0.) THEN
  461           SMOOTH = 0.5*TANH((RE-SSWELLF(4))/SSWELLF(7))
  462           PTURB=(0.5+SMOOTH)
  463           PVISC=(0.5-SMOOTH)
  464         ELSE
  465           IF (RE.LE.SSWELLF(4)) THEN
  466             PTURB =  0.
  467             PVISC =  1.
  468           ELSE
  469             PTURB =  1.
  470             PVISC =  0.
  471             END IF
  472           END IF
  473       ELSE
  474         PTURB=1.
  475         PVISC=1.
  476         END IF
  477               
  478 !
  479       IF (SSWELLF(2).EQ.0) THEN
  480         FW=MAX(ABS(SSWELLF(3)),0.)
  481         FU=0.
  482         FUD=0.
  483       ELSE
  484         FU=ABS(SSWELLF(3))
  485         FUD=SSWELLF(2)
  486         AORB=2*SQRT(AORB)
  487         XI=(ALOG10(MAX(AORB/Z0NOZ,3.))-ABMIN)/DELAB
  488         IND  = MIN (SIZEFWTABLE-1, INT(XI))
  489         DELI1= MIN (1. ,XI-FLOAT(IND))
  490         DELI2= 1. - DELI1
  491         FW =FWTABLE(IND)*DELI2+FWTABLE(IND+1)*DELI1
  492         END IF
  493 !
  494 ! 2.  Diagonal
  495 !
 
 
                                                         w3src4md.F90  page  10
 
 
  496 ! Here AS is the air-sea temperature difference in degrees. Expression gi>
  497 ! Abdalla & Cavaleri, JGR 2002 for Usigma. For USTARsigma ... I do not se>
  498 ! I got it from, maybe just made up from drag law ...
  499 !
  500       UST=USTAR
  501       ISTAB=3
  502       TAUX = UST**2* COS(USDIR)
  503       TAUY = UST**2* SIN(USDIR)
  504 !
  505 ! Loop over the resolved part of the spectrum
  506 !
  507       STRESSSTAB(ISTAB,:)=0.
  508       STRESSSTABN(ISTAB,:)=0.
  509 !
  510 ! Coupling coefficient times density ratio DRAT
  511 !
  512       CONST0=BBETA*DRAT/(kappa**2)
  513 !
  514       DO IK=1, NK
  515         TAUPX=TAUX-ABS(TTAUWSHELTER)*STRESSSTAB(ISTAB,1)
  516         TAUPY=TAUY-ABS(TTAUWSHELTER)*STRESSSTAB(ISTAB,2)
  517 ! With MIN and MAX the bug should disappear.... but where did it come fro>
  518         USTP=MIN((TAUPX**2+TAUPY**2)**0.25,MAX(UST,0.3))
  519         USDIRP=ATAN2(TAUPY,TAUPX)
  520         COSU   = COS(USDIRP)
  521         SINU   = SIN(USDIRP)
  522         IS=1+(IK-1)*NTH
  523         CM=K(IS)/SIG2(IS) !inverse of phase speed
  524         UCN=USTP*CM+ZZALP  !this is the inverse wave age
  525            ! the stress is the real stress (N/m^2) divided by
  526            ! rho_a, and thus comparable to USTAR**2
  527            ! it is the integral of rho_w g Sin/C /rho_a
  528            ! (air-> waves momentum flux)
  529         CONST2=DDEN2(IS)/CG(IK) &        !Jacobian to get energy in band
  530               *GRAV/(SIG(IK)/K(IS)*DRAT) ! coefficient to get momentum
  531         CONST=SIG2(IS)*CONST0
  532            ! CM parameter is 1 / C_phi
  533            ! Z0 corresponds to Z0+Z1 of the Janssen eq. 14
  534         ZCN=ALOG(K(IS)*Z0)
  535 !
  536 ! precomputes swell factors
  537 !
  538         SWELLCOEFV=-SSWELLF(5)*DRAT*2*K(IS)*SQRT(2*NU_AIR*SIG2(IS))
  539         SWELLCOEFT=-DRAT*SSWELLF(1)*16*SIG2(IS)**2/GRAV
  540 !
  541         DO ITH=1,NTH
  542           IS=ITH+(IK-1)*NTH
  543           COSWIND=(ECOS(IS)*COSU+ESIN(IS)*SINU)
  544           IF (COSWIND.GT.0.01) THEN
  545             X=COSWIND*UCN
  546             ! this ZARG term is the argument of the exponential
  547             ! in Janssen 1991 eq. 16.
  548             ZARG=KAPPA/X
  549             ! ZLOG is ALOG(MU) where MU is defined by Janssen 1991 eq. 15
  550             ! MU=
 
 
                                                         w3src4md.F90  page  11
 
 
  551             ZLOG=ZCN+ZARG
  552                          
  553             IF (ZLOG.LT.0.) THEN
  554               ! The source term Sp is beta * omega * X**2
  555               ! as given by Janssen 1991 eq. 19
  556               ! Note that this is slightly diffent from ECWAM code CY45R2>
  557               DSTAB(ISTAB,IS) = CONST*EXP(ZLOG)*ZLOG**4*UCN*UCN*COSWIND**>
  558                                                                           
  559               ! Below is an example with breaking probability feeding bac>
  560               !DSTAB(ISTAB,IS) = CONST*EXP(ZLOG)*ZLOG**4  &
  561               !                  *UCN*UCN*COSWIND**SSINTHP *(1+BRLAMBDA(I>
  562               LLWS(IS)=.TRUE.
  563             ELSE
  564               DSTAB(ISTAB,IS) = 0.
  565               LLWS(IS)=.FALSE.
  566               END IF
  567 !
  568 !  Added for consistency with ECWAM implsch.F
  569 !
  570             IF (28.*CM*USTAR*COSWIND.GE.1) THEN
  571               LLWS(IS)=.TRUE.
  572               END IF
  573           ELSE  ! (COSWIND.LE.0.01)
  574             DSTAB(ISTAB,IS) = 0.
  575             LLWS(IS)=.FALSE.
  576             END IF
  577 !
  578           IF ((SSWELLF(1).NE.0.AND.DSTAB(ISTAB,IS).LT.1E-7*SIG2(IS)) &
  579               .OR.SSWELLF(3).GT.0) THEN
  580 !
  581               DVISC=SWELLCOEFV
  582               DTURB=SWELLCOEFT*(FW*UORB+(FU+FUD*COSWIND)*USTP)
  583 !
  584               DSTAB(ISTAB,IS) = DSTAB(ISTAB,IS) + PTURB*DTURB +  PVISC*DV>
  585             END IF
  586 !
  587 ! Sums up the wave-supported stress
  588 !
  589           ! Wave direction is "direction to"
  590           ! therefore there is a PLUS sign for the stress
  591           TEMP2=CONST2*DSTAB(ISTAB,IS)*A(IS)
  592           IF (DSTAB(ISTAB,IS).LT.0) THEN
  593             STRESSSTABN(ISTAB,1)=STRESSSTABN(ISTAB,1)+TEMP2*ECOS(IS)
  594             STRESSSTABN(ISTAB,2)=STRESSSTABN(ISTAB,2)+TEMP2*ESIN(IS)
  595           ELSE
  596             STRESSSTAB(ISTAB,1)=STRESSSTAB(ISTAB,1)+TEMP2*ECOS(IS)
  597             STRESSSTAB(ISTAB,2)=STRESSSTAB(ISTAB,2)+TEMP2*ESIN(IS)
  598             END IF
  599           END DO
  600         END DO
  601 !
  602         D(:)=DSTAB(3,:)
  603         XSTRESS=STRESSSTAB (3,1)
  604         YSTRESS=STRESSSTAB (3,2)
  605         TAUWNX =STRESSSTABN(3,1)
 
 
                                                         w3src4md.F90  page  12
 
 
  606         TAUWNY =STRESSSTABN(3,2)
  607       S = D * A
  608 !
  609 ! ... Test output of arrays
  610 !
  611       ! Computes the high-frequency contribution
  612       ! the difference in spectal density (kx,ky) to (f,theta)
  613       ! is integrated in this modified CONST0
  614       CONST0=DTH*SIG(NK)**5/((GRAV**2)*tpi) &
  615          *TPI*SIG(NK) / CG(NK)  !conversion WAM (E(f,theta) to WW3 A(k,th>
  616       TEMP=0.
  617       DO ITH=1,NTH
  618          IS=ITH+(NK-1)*NTH
  619          COSWIND=(ECOS(IS)*COSU+ESIN(IS)*SINU)
  620          TEMP=TEMP+A(IS)*(MAX(COSWIND,0.))**3
  621          END DO
  622                
  623       TAUPX=TAUX-ABS(TTAUWSHELTER)*XSTRESS
  624       TAUPY=TAUY-ABS(TTAUWSHELTER)*YSTRESS
  625       USTP=(TAUPX**2+TAUPY**2)**0.25
  626       USDIRP=ATAN2(TAUPY,TAUPX)
  627                                
  628       UST=USTP
  629       ! finds the values in the tabulated stress TAUHFT
  630       XI=UST/DELUST
  631       IND  = MAX(1,MIN (IUSTAR-1, INT(XI)))
  632       DELI1= MAX(MIN (1. ,XI-FLOAT(IND)),0.)
  633       DELI2= 1. - DELI1
  634       XJ=MAX(0.,(GRAV*Z0/MAX(UST,0.00001)**2-AALPHA) / DELALP)
  635       J    = MAX(1 ,MIN (IALPHA-1, INT(XJ)))
  636       DELJ1= MAX(0.,MIN (1.      , XJ-FLOAT(J)))
  637       DELJ2=1. - DELJ1
  638       IF (TTAUWSHELTER.GT.0) THEN
  639         XK = CONST0*TEMP / DELTAIL
  640          I = MIN (ILEVTAIL-1, INT(XK))
  641          DELK1= MIN (1. ,XK-FLOAT(I))
  642          DELK2=1. - DELK1
  643          TAU1 =((TAUHFT2(IND,J,I)*DELI2+TAUHFT2(IND+1,J,I)*DELI1 )*DELJ2 &
  644                +(TAUHFT2(IND,J+1,I)*DELI2+TAUHFT2(IND+1,J+1,I)*DELI1)*DEL>
  645               +((TAUHFT2(IND,J,I+1)*DELI2+TAUHFT2(IND+1,J,I+1)*DELI1 )*DE>
  646                +(TAUHFT2(IND,J+1,I+1)*DELI2+TAUHFT2(IND+1,J+1,I+1)*DELI1)>
  647       ELSE
  648         TAU1 =(TAUHFT(IND,J)*DELI2+TAUHFT(IND+1,J)*DELI1 )*DELJ2 &
  649          +(TAUHFT(IND,J+1)*DELI2+TAUHFT(IND+1,J+1)*DELI1)*DELJ1
  650         END IF
  651       TAUHF = CONST0*TEMP*UST**2*TAU1
  652       TAUWX = XSTRESS+TAUHF*COS(USDIRP)
  653       TAUWY = YSTRESS+TAUHF*SIN(USDIRP)
  654 !
  655 ! Reduces tail effect to make sure that wave-supported stress
  656 ! is less than total stress, this is borrowed from ECWAM Stresso.F
  657 !
  658       TAUW = SQRT(TAUWX**2+TAUWY**2)
  659       UST2   = MAX(USTAR,EPS2)**2
  660       TAUWB = MIN(TAUW,MAX(UST2-EPS1,EPS2**2))
 
 
                                                         w3src4md.F90  page  13
 
 
  661       IF (TAUWB.LT.TAUW) THEN
  662         TAUWX=TAUWX*TAUWB/TAUW
  663         TAUWY=TAUWY*TAUWB/TAUW
  664         END IF
  665 !
  666       RETURN
  667 !
  668 ! Formats
  669 !
  670 !/
  671 !/ End of W3SIN4 ----------------------------------------------------- /
  672 !/
  673       END SUBROUTINE W3SIN4
  674 !/ ------------------------------------------------------------------- /
  675       SUBROUTINE INSIN4(FLTABS)
  676 !/
  677 !/                  +-----------------------------------+
  678 !/                  | WAVEWATCH III           NOAA/NCEP |
  679 !/                  |                         SHOM      |
  680 !/                  |            F. Ardhuin             |
  681 !/                  |                        FORTRAN 90 |
  682 !/                  | Last update :         30-Aug-2010 |
  683 !/                  +-----------------------------------+
  684 !/
  685 !/    30-Aug-2010 : Origination.                        ( version 3.14-If>
  686 !
  687 !  1. Purpose :
  688 !
  689 !     Initialization for source term routine.
  690 !
  691 !  2. Method :
  692 !
  693 !  3. Parameters :
  694 !
  695 !     ----------------------------------------------------------------
  696 !      FLTABS    Logical
  697 !     ----------------------------------------------------------------
  698 !
  699 !  4. Subroutines used :
  700 !
  701 !      Name      Type  Module   Description
  702 !     ----------------------------------------------------------------
  703 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  704 !     ----------------------------------------------------------------
  705 !
  706 !  5. Called by :
  707 !
  708 !      Name      Type  Module   Description
  709 !     ----------------------------------------------------------------
  710 !      W3SIN4    Subr. W3SRC3MD Corresponding source term.
  711 !     ----------------------------------------------------------------
  712 !
  713 !  6. Error messages :
  714 !
  715 !       None.
 
 
                                                         w3src4md.F90  page  14
 
 
  716 !
  717 !  7. Remarks :
  718 !
  719 !  8. Structure :
  720 !
  721 !     See source code.
  722 !
  723 !  9. Switches :
  724 !
  725 !     !/S  Enable subroutine tracing.
  726 !
  727 ! 10. Source code :
  728 !
  729 !/ ------------------------------------------------------------------- /
  730       USE CONSTANTS, ONLY: TPIINV, RADE, GRAV
  731       USE W3ODATMD,  ONLY: NDSE
  732       USE W3SERVMD,  ONLY: EXTCDE
  733       USE W3DISPMD,  ONLY: WAVNU2
  734       USE W3GDATMD,  ONLY: SIG, DSIP, NK, NTH, TTAUWSHELTER,             &
  735                            SSDSDTH, SSDSCOS, TH, DTH, XFR, ECOS, ESIN,   &
  736                            SSDSC,  SSDSBRF1, SSDSBCK, SSDSBINT, SSDSPBK, &
  737                            SSDSABK, SSDSHCK, IKTAB, DCKI, SATINDICES,    &
  738                            SATWEIGHTS, CUMULW, NKHS, NKD, NDTAB, QBI
  739 !/
  740       IMPLICIT NONE
  741 !/
  742 !/ ------------------------------------------------------------------- /
  743 !/ Parameter list
  744 !/
  745       LOGICAL, INTENT(IN)     :: FLTABS
  746 !/
  747 !/ ------------------------------------------------------------------- /
  748 !/
  749       INTEGER  SDSNTH, ITH, I_INT, J_INT, IK, IK2, ITH2 , IS, IS2
  750       INTEGER  IKL, ID, ICON, IKD, IKHS, IKH, TOTO
  751       REAL     C, C2
  752       REAL     DIFF1, DIFF2, BINF, BSUP, CGG, PROF
  753       REAL     KIK, DHS, KD, KHS, KH, XT, GAM, DKH, PR, W, EPS
  754       REAL     DKD
  755       REAL, DIMENSION(:,:)   , ALLOCATABLE :: SIGTAB
  756       REAL, DIMENSION(:,:)   , ALLOCATABLE :: K1, K2
  757 !/
  758 !/ ------------------------------------------------------------------- /
  759 !/ Local parameters
  760 !/
  761 !/
  762 !/ ------------------------------------------------------------------- /
  763 !/
  764 !
  765 ! 1.  Initializations ------------------------------------------------ *
  766 !
  767 ! These precomputed tables are written in mod_def.ww3
  768 !
  769       IF (FLTABS) THEN
  770         CALL TABU_STRESS
 
 
                                                         w3src4md.F90  page  15
 
 
  771         CALL TABU_TAUHF(SIG(NK) )      !tabulate high-frequency stress: 2>
  772         IF (TTAUWSHELTER.GT.0) THEN
  773           CALL TABU_TAUHF2(SIG(NK) )   !tabulate high-frequency stress: 3>
  774           END IF
  775         END IF
  776 !
  777 ! 2.  SPONTANEOUS BREAKING
  778 ! 2.a Precomputes the indices for integrating the spectrum to get saturat>
  779 !
  780       IF (SSDSDTH.LT.180) THEN
  781         SDSNTH  = MIN(NINT(SSDSDTH/(DTH*RADE)),NTH/2-1)
  782         SATINDICES(:,:)=1
  783         SATWEIGHTS(:,:)=0.
  784         DO ITH=1,NTH
  785           DO I_INT=ITH-SDSNTH, ITH+SDSNTH
  786             J_INT=I_INT
  787             IF (I_INT.LT.1)  J_INT=I_INT+NTH
  788             IF (I_INT.GT.NTH) J_INT=I_INT-NTH
  789             SATINDICES(I_INT-(ITH-SDSNTH)+1,ITH)=J_INT
  790             SATWEIGHTS(I_INT-(ITH-SDSNTH)+1,ITH)=          &
  791                    COS(TH(ITH)-TH(J_INT))**SSDSCOS
  792             END DO
  793           END DO
  794       ELSE
  795         SATINDICES(:,:)=1
  796         SATWEIGHTS(:,:)=1.
  797         END IF
  798 !/ ------------------------------------------------------------------- /
  799 !
  800 ! Precomputes QBI and DCKI (TEST 500)
  801 !
  802       IF (SSDSBCK.GT.0) THEN
  803 !
  804 ! Precomputes the indices for integrating the spectrum over frequency ban>
  805 !
  806         BINF=(1-SSDSBINT) ! Banner et al 2002: Hp=4*sqrt(int_0.7^1.3fp E >
  807         BSUP=(1+SSDSBINT)
  808         KIK=0.
  809 !
  810 ! High frequency tail for convolution calculation
  811 !
  812         ALLOCATE(K1(NK,NDTAB))
  813         ALLOCATE(K2(NK,NDTAB))
  814         ALLOCATE(SIGTAB(NK,NDTAB))
  815                                   
  816         SIGTAB=0. !contains frequency for upper windows boundaries
  817         IKTAB=0  ! contains indices for upper windows boundaries
  818                                                                 
  819         DO ID=1,NDTAB
  820           TOTO=0
  821           PROF=REAL(ID)
  822           DO IKL=1,NK ! last window starts at IK=NK
  823             CALL WAVNU2(SIG(IKL), PROF, KIK, CGG, 1E-7, 15, ICON)
  824             K1(IKL,ID)=KIK  ! wavenumber lower boundary (is directly rela>
  825             K2(IKL,ID)=((BSUP/BINF)**2.)*K1(IKL,ID)! wavenumber upper bou>
 
 
                                                         w3src4md.F90  page  16
 
 
  826             SIGTAB(IKL,ID)=SQRT(GRAV*K2(IKL,ID)*TANH(K2(IKL,ID)*ID)) ! co>
  827             IF(SIGTAB(IKL,ID) .LE. SIG(1)) THEN
  828               IKTAB(IKL,ID)=1
  829               END IF
  830             IF(SIGTAB(IKL,ID) .GT. SIG(NK)) THEN
  831               IKTAB(IKL,ID)=NK+TOTO       ! in w3sds4 only windows with I>
  832               TOTO=1
  833               END IF
  834             DO IK=1,NK-1
  835               DIFF1=0.
  836               DIFF2=0.
  837               IF(SIG(IK)<SIGTAB(IKL,ID) .AND. SIG(IK+1)>=SIGTAB(IKL,ID)) >
  838                 DIFF1=SIGTAB(IKL,ID)-SIG(IK)   ! seeks the indices of the>
  839                 DIFF2=SIG(IK+1)-SIGTAB(IKL,ID)! the indices of lower boud>
  840                 IF (DIFF1<DIFF2) THEN
  841                   IKTAB(IKL,ID)=IK
  842                 ELSE
  843                   IKTAB(IKL,ID)=IK+1
  844                   END IF
  845                 END IF
  846               END DO
  847             END DO
  848           END DO
  849 !
  850 ! Tabulates DCKI and QBI
  851 !
  852         DHS=KHSMAX/NKHS ! max value of KHS=KHSMAX
  853         DKH=KHMAX/NKHI  ! max value of KH=KHMAX
  854         DKD=KDMAX/NKD
  855         ALLOCATE(DCKI(NKHS,NKD))
  856         ALLOCATE(QBI(NKHS,NKD))
  857         DCKI=0.
  858         QBI =0.
  859         DO IKD=1,NKD
  860           KHS=0.
  861           KD=(FAC_KD1**(IKD-FAC_KD2))
  862           XT=TANH(KD)
  863           GAM=1.0314*(XT**3)-1.9958*(XT**2)+1.5522*XT+0.1885
  864           GAM=GAM/2.15
  865           DO IKHS=1,NKHS  ! max value of KHS=1.
  866             KH=0.
  867             KHS=KHS+DHS
  868             DO IKH=1,NKHI
  869               KH=KH+DKH
  870               PR=(4.*KH/(KHS**2.))*exp(-(2*((KH/KHS)**2.)))
  871 !              W=1.5*(((KHS)/(SQRT(2.)*GAM*XT))**2.)*(1-exp(-(((KH)/(GAM*>
  872               W=SSDSABK*(((KHS)/(SQRT(2.)*GAM*XT))**2.)*(1-exp(-(((KH)/(G>
  873               EPS=-((((SSDSBCK/(XT**SSDSHCK))*KH)**3.)/4)*SQRT(GRAV/XT)
  874               DCKI(IKHS, IKD)= DCKI(IKHS, IKD)+PR*W*EPS*DKH
  875               QBI(IKHS, IKD) = QBI(IKHS, IKD) +PR*W*    DKH
  876               END DO
  877             END DO
  878           END DO
  879                 
  880         WHERE ( QBI .GT. 1. )
 
 
                                                         w3src4md.F90  page  17
 
 
  881           QBI = 1.
  882           END WHERE
  883                    
  884         DEALLOCATE(K1,K2)
  885         DEALLOCATE(SIGTAB)
  886       ELSE
  887         IKTAB(:,:)=1
  888         DCKI(:,:) =0.
  889         QBI(:,:)  =0.
  890         END IF
  891 !
  892 !/ ------------------------------------------------------------------- /
  893 !                        CUMULATIVE EFFECT
  894 !/ ------------------------------------------------------------------- /
  895 !
  896 ! Precomputes the weights for the cumulative effect (TEST 441 and 500)
  897 !
  898       DIKCUMUL = 0
  899       IF (SSDSC(3).NE.0) THEN
  900 !       DIKCUMUL is the integer difference in frequency bands
  901 !       between the "large breakers" and short "wiped-out waves"
  902         DIKCUMUL = NINT(SSDSBRF1/(XFR-1.))
  903 !        WRITE(6,*) 'INSIN4b:',DIKCUMUL
  904         CUMULW(:,:)=0.
  905         DO IK=1,NK
  906           C = GRAV/SIG(IK)   ! Valid in deep water only
  907           !C = SIG(IK)/K(IK) ! Valid in all water depth ???
  908           DO ITH=1,NTH
  909             IS=ITH+(IK-1)*NTH
  910             DO IK2=1,IK-DIKCUMUL
  911               C2 = GRAV/SIG(IK2) ! Valid in deep water only
  912               !C2 = SIG(IK2)/K(IK2) ! Valid in all water depth ???
  913               DO ITH2=1,NTH
  914                 IS2=ITH2+(IK2-1)*NTH
  915                 CUMULW(IS2,IS)=SQRT(C**2+C2**2-2*C*C2*ECOS(1+ABS(ITH2-ITH>
  916                                    *DSIP(IK2)/(0.5*C2) * DTH             >
  917                 END DO
  918               END DO
  919             END DO
  920           END DO
  921         ELSE
  922           CUMULW(:,:)=0.
  923           END IF
  924 !/
  925 !/ End of INSIN4 ----------------------------------------------------- /
  926 !/
  927       END SUBROUTINE INSIN4
  928 ! ----------------------------------------------------------------------
  929       SUBROUTINE TABU_STRESS
  930 !/
  931 !/                  +-----------------------------------+
  932 !/                  | WAVEWATCH III           NOAA/NCEP |
  933 !/                  |            F. Ardhuin             |
  934 !/                  |                        FORTRAN 90 |
  935 !/                  | Last update :         17-Oct-2007 |
 
 
                                                         w3src4md.F90  page  18
 
 
  936 !/                  +-----------------------------------+
  937 !/
  938 !/    23-Jun-2006 : Origination.                        ( version 3.13 )
  939 !/     adapted from WAM, original:P.A.E.M. JANSSEN    KNMI AUGUST 1990
  940 !/     adapted version (subr. STRESS): J. BIDLOT    ECMWF OCTOBER 2004
  941 !/     Table values were checkes against the original f90 result and foun>
  942 !/     be identical (at least at 0.001 m/s accuracy)
  943 !/
  944 !  1. Purpose :
  945 !     TO GENERATE friction velocity table TAUT(TAUW,U10)=SQRT(TAU).
  946 !     METHOD.
  947 !       A STEADY STATE WIND PROFILE IS ASSUMED.
  948 !       THE WIND STRESS IS COMPUTED USING THE ROUGHNESSLENGTH
  949 !                  Z1=Z0/SQRT(1-TAUW/TAU)
  950 !       WHERE Z0 IS THE CHARNOCK RELATION , TAUW IS THE WAVE-
  951 !       INDUCED STRESS AND TAU IS THE TOTAL STRESS.
  952 !       WE SEARCH FOR STEADY-STATE SOLUTIONS FOR WHICH TAUW/TAU < 1.
  953 !       FOR QUASILINEAR EFFECT SEE PETER A.E.M. JANSSEN,1990.
  954 !
  955 !     Initialization for source term routine.
  956 !
  957 !  2. Method :
  958 !
  959 !  3. Parameters :
  960 !
  961 !     Parameter list
  962 !     ----------------------------------------------------------------
  963 !     ----------------------------------------------------------------
  964 !
  965 !  4. Subroutines used :
  966 !
  967 !      Name      Type  Module   Description
  968 !     ----------------------------------------------------------------
  969 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  970 !     ----------------------------------------------------------------
  971 !
  972 !  5. Called by :
  973 !
  974 !      Name      Type  Module   Description
  975 !     ----------------------------------------------------------------
  976 !      W3SIN3    Subr. W3SRC3MD Corresponding source term.
  977 !     ----------------------------------------------------------------
  978 !
  979 !  6. Error messages :
  980 !
  981 !       None.
  982 !
  983 !  7. Remarks :
  984 !
  985 !  8. Structure :
  986 !
  987 !     See source code.
  988 !
  989 !  9. Switches :
  990 !
 
 
                                                         w3src4md.F90  page  19
 
 
  991 !     !/S  Enable subroutine tracing.
  992 !
  993 ! 10. Source code :
  994 !
  995 !/ ------------------------------------------------------------------- /
  996       USE CONSTANTS, ONLY: KAPPA, GRAV
  997       USE W3GDATMD, ONLY: ZZWND, AALPHA, ZZ0MAX
  998       IMPLICIT NONE
  999       INTEGER, PARAMETER      :: NITER=10
 1000       REAL   , PARAMETER      :: XM=0.50, EPS1=0.00001
 1001 !     VARIABLE.   TYPE.     PURPOSE.
 1002 !      *XM*        REAL      POWER OF TAUW/TAU IN ROUGHNESS LENGTH.
 1003 !      *XNU*       REAL      KINEMATIC VISCOSITY OF AIR.
 1004 !      *NITER*     INTEGER   NUMBER OF ITERATIONS TO OBTAIN TOTAL STRESS
 1005 !      *EPS1*      REAL      SMALL NUMBER TO MAKE SURE THAT A SOLUTION
 1006 !                            IS OBTAINED IN ITERATION WITH TAU>TAUW.
 1007 ! ----------------------------------------------------------------------
 1008       INTEGER I,J,ITER
 1009       REAL ZTAUW,UTOP,CDRAG,WCD,USTOLD,TAUOLD
 1010       REAL X,UST,ZZ0,ZNU,F,DELF,ZZ00
 1011 !
 1012       DELU    = UMAX/FLOAT(JUMAX)
 1013       DELTAUW = TAUWMAX/FLOAT(ITAUMAX)
 1014       DO I=0,ITAUMAX
 1015          ZTAUW   = (REAL(I)*DELTAUW)**2
 1016          DO J=0,JUMAX
 1017             UTOP    = FLOAT(J)*DELU
 1018             CDRAG   = 0.0012875
 1019             WCD     = SQRT(CDRAG)
 1020             USTOLD  = UTOP*WCD
 1021             TAUOLD  = MAX(USTOLD**2, ZTAUW+EPS1)
 1022             DO ITER=1,NITER
 1023                X   = ZTAUW/TAUOLD
 1024                UST = SQRT(TAUOLD)
 1025                ZZ00=AALPHA*TAUOLD/GRAV
 1026                IF (ZZ0MAX.NE.0) ZZ00=MIN(ZZ00,ZZ0MAX)
 1027                ! Corrects roughness ZZ00 for quasi-linear effect
 1028                ZZ0 = ZZ00/(1.-X)**XM
 1029                !ZNU = 0.1*nu_air/UST  ! This was removed by Bidlot in 1996
 1030                !ZZ0 = MAX(ZNU,ZZ0)
 1031                F   = UST-KAPPA*UTOP/(ALOG(ZZWND/ZZ0))
 1032                DELF= 1.-KAPPA*UTOP/(ALOG(ZZWND/ZZ0))**2*2./UST &
 1033                         *(1.-(XM+1)*X)/(1.-X)
 1034                UST = UST-F/DELF
 1035                TAUOLD= MAX(UST**2., ZTAUW+EPS1)
 1036                END DO
 1037             TAUT(I,J)  = SQRT(TAUOLD)
 1038             END DO
 1039          END DO
 1040        I=ITAUMAX
 1041        J=JUMAX
 1042 !
 1043 !  Force zero wind to have zero stress (Bidlot 1996)
 1044 !
 1045       DO I=0,ITAUMAX
 
 
                                                         w3src4md.F90  page  20
 
 
 1046         TAUT(I,0)=0.0
 1047         END DO
 1048       RETURN
 1049       END SUBROUTINE TABU_STRESS
 1050 !/ ------------------------------------------------------------------- /
 1051       SUBROUTINE TABU_TAUHF(SIGMAX)
 1052 !/
 1053 !/                  +-----------------------------------+
 1054 !/                  | WAVEWATCH III           NOAA/NCEP |
 1055 !/                  |            F. Ardhuin             |
 1056 !/                  |                        FORTRAN 90 |
 1057 !/                  | Last update 2006/08/14            |
 1058 !/                  +-----------------------------------+
 1059 !/
 1060 !/    27-Feb-2004 : Origination in WW3                  ( version 2.22.SH>
 1061 !/     the resulting table was checked to be identical to the original f7>
 1062 !/    14-Aug-2006 : Modified following Bidlot           ( version 2.22.SH>
 1063 !/    18-Aug-2006 : Ported to version 3.09
 1064 !
 1065 !  1. Purpose :
 1066 !
 1067 !     Tabulation of the high-frequency wave-supported stress
 1068 !
 1069 !  2. Method :
 1070 !
 1071 !       SEE REFERENCE FOR WAVE STRESS CALCULATION.
 1072 !       FOR QUASILINEAR EFFECT SEE PETER A.E.M. JANSSEN,1990.
 1073 !     See tech. Memo ECMWF 03 december 2003 by Bidlot & Janssen
 1074 !
 1075 !  3. Parameters :
 1076 !
 1077 !     Parameter list
 1078 !     ----------------------------------------------------------------
 1079 !       SIGMAX   Real  I   maximum frequency * TPI
 1080 !     ----------------------------------------------------------------
 1081 !
 1082 !  4. Subroutines used :
 1083 !
 1084 !       STRACE   Service routine.
 1085 !
 1086 !  5. Called by :
 1087 !
 1088 !       W3SIN3   Wind input Source term routine.
 1089 !
 1090 !  6. Error messages :
 1091 !
 1092 !  7. Remarks :
 1093 !
 1094 !  8. Structure :
 1095 !
 1096 !     See source code.
 1097 !
 1098 !  9. Switches :
 1099 !
 1100 !       !/S      Enable subroutine tracing.
 
 
                                                         w3src4md.F90  page  21
 
 
 1101 !       !/T      Enable test output.
 1102 !
 1103 ! 10. Source code :
 1104 !
 1105 !/ ------------------------------------------------------------------- /
 1106       USE CONSTANTS, ONLY: KAPPA, GRAV
 1107       USE W3GDATMD, ONLY: AALPHA, BBETA, ZZALP, FACHFE, ZZ0MAX
 1108 !
 1109       IMPLICIT NONE
 1110 !/
 1111 !/ ------------------------------------------------------------------- /
 1112 !/ Parameter list
 1113 !/
 1114       REAL, intent(in) :: SIGMAX  !  maximum frequency
 1115 !/
 1116 !/ ------------------------------------------------------------------- /
 1117 !/ Local parameters
 1118 !/
 1119 !       USTARM  R.A.  Maximum friction velocity
 1120 !       ALPHAM  R.A.  Maximum Charnock Coefficient
 1121 !       WLV     R.A.  Water levels.
 1122 !       UA      R.A.  Absolute wind speeds.
 1123 !       UD      R.A.  Absolute wind direction.
 1124 !       U10     R.A.  Wind speed used.
 1125 !       U10D    R.A.  Wind direction used.
 1126 ! 10. Source code :
 1127 !
 1128 !/ ------------------------------------------------------------------- /
 1129       REAL                    :: USTARM, ALPHAM
 1130       REAL                    :: CONST1, OMEGA, OMEGAC
 1131       REAL                    :: UST, ZZ0,OMEGACC, CM
 1132       INTEGER, PARAMETER      :: JTOT=250
 1133       REAL, ALLOCATABLE       :: W(:)
 1134       REAL                    :: ZX,ZARG,ZMU,ZLOG,ZZ00,ZBETA
 1135       REAL                    :: Y,YC,DELY
 1136       INTEGER                 :: J,K,L
 1137       REAL                    :: X0
 1138 !
 1139       USTARM = 5.
 1140       ALPHAM = 20.*AALPHA
 1141       DELUST = USTARM/REAL(IUSTAR)
 1142       DELALP = ALPHAM/REAL(IALPHA)
 1143       CONST1 = BBETA/KAPPA**2
 1144       OMEGAC = SIGMAX
 1145 !
 1146       TAUHFT(0:IUSTAR,0:IALPHA)=0. !table initialization
 1147 !
 1148       ALLOCATE(W(JTOT))
 1149       W(2:JTOT-1)=1.
 1150       W(1)=0.5
 1151       W(JTOT)=0.5
 1152       X0 = 0.05
 1153 !
 1154       DO L=0,IALPHA
 1155          DO K=0,IUSTAR
 
 
                                                         w3src4md.F90  page  22
 
 
 1156             UST      = MAX(REAL(K)*DELUST,0.000001)
 1157             ZZ00       = UST**2*AALPHA/GRAV
 1158             IF (ZZ0MAX.NE.0) ZZ00=MIN(ZZ00,ZZ0MAX)
 1159             ZZ0       = ZZ00*(1+FLOAT(L)*DELALP/AALPHA)
 1160             OMEGACC  = MAX(OMEGAC,X0*GRAV/UST)
 1161             YC       = OMEGACC*SQRT(ZZ0/GRAV)
 1162             DELY     = MAX((1.-YC)/REAL(JTOT),0.)
 1163             ! For a given value of UST and ALPHA,
 1164             ! the wave-supported stress is integrated all the way
 1165             ! to 0.05*g/UST
 1166             DO J=1,JTOT
 1167                Y        = YC+REAL(J-1)*DELY
 1168                OMEGA    = Y*SQRT(GRAV/ZZ0)
 1169                ! This is the deep water phase speed
 1170                CM       = GRAV/OMEGA
 1171                !this is the inverse wave age, shifted by ZZALP (tuning)
 1172                ZX       = UST/CM +ZZALP
 1173                ZARG     = MIN(KAPPA/ZX,20.)
 1174                ZMU      = MIN(GRAV*ZZ0/CM**2*EXP(ZARG),1.)
 1175                ZLOG     = MIN(ALOG(ZMU),0.)
 1176                ZBETA        = CONST1*ZMU*ZLOG**4
 1177                ! Power of Y in denominator should be FACHFE-4  tail appli>
 1178                TAUHFT(K,L)  = TAUHFT(K,L)+W(J)*ZBETA/Y*DELY
 1179                END DO
 1180          END DO
 1181       END DO
 1182       DEALLOCATE(W)
 1183       RETURN
 1184       END SUBROUTINE TABU_TAUHF
 1185                                
 1186 !/ ------------------------------------------------------------------- /
 1187       SUBROUTINE TABU_TAUHF2(SIGMAX)
 1188 !/
 1189 !/                  +-----------------------------------+
 1190 !/                  | WAVEWATCH III           NOAA/NCEP |
 1191 !/                  |            F. Ardhuin             |
 1192 !/                  |                        FORTRAN 90 |
 1193 !/                  | Last update 2006/08/14            |
 1194 !/                  | Last update 2013/01/24            |
 1195 !/                  +-----------------------------------+
 1196 !/
 1197 !/    15-May-2007 : Origination in WW3                  ( version 3.10.SH>
 1198 !/    24-Jan-2013 : Allows to read in table             ( version 4.08 )
 1199 !
 1200 !  1. Purpose :
 1201 !
 1202 !     Tabulation of the high-frequency wave-supported stress as a functio>
 1203 !     ustar, alpha (modified Charnock), and tail energy level
 1204 !
 1205 !  2. Method :
 1206 !
 1207 !       SEE REFERENCE FOR WAVE STRESS CALCULATION.
 1208 !       FOR QUASILINEAR EFFECT SEE PETER A.E.M. JANSSEN,1990.
 1209 !     See tech. Memo ECMWF 03 december 2003 by Bidlot & Janssen
 1210 !
 
 
                                                         w3src4md.F90  page  23
 
 
 1211 !  3. Parameters :
 1212 !
 1213 !     Parameter list
 1214 !     ----------------------------------------------------------------
 1215 !       SIGMAX   Real  I   maximum frequency*TPI
 1216 !     ----------------------------------------------------------------
 1217 !
 1218 !  4. Subroutines used :
 1219 !
 1220 !       STRACE   Service routine.
 1221 !
 1222 !  5. Called by :
 1223 !
 1224 !       W3SIN3   Wind input Source term routine.
 1225 !
 1226 !  6. Error messages :
 1227 !
 1228 !  7. Remarks :
 1229 !
 1230 !  8. Structure :
 1231 !
 1232 !     See source code.
 1233 !
 1234 !  9. Switches :
 1235 !
 1236 !       !/S      Enable subroutine tracing.
 1237 !       !/T      Enable test output.
 1238 !
 1239 ! 10. Source code :
 1240 !
 1241 !/ ------------------------------------------------------------------- /
 1242       USE CONSTANTS, ONLY: KAPPA, GRAV
 1243       USE W3GDATMD, ONLY: AALPHA, BBETA, ZZALP, FACHFE,  &
 1244                           TTAUWSHELTER, ZZ0MAX
 1245       USE W3ODATMD, ONLY: NDSE
 1246 !
 1247       IMPLICIT NONE
 1248 !/
 1249 !/ ------------------------------------------------------------------- /
 1250 !/ Parameter list
 1251 !/
 1252       REAL, intent(in) :: SIGMAX  !  maximum frequency * TPI
 1253 !/
 1254 !/ ------------------------------------------------------------------- /
 1255 !/ Local parameters
 1256 !/
 1257 !       USTARM  R.A.  Maximum friction velocity
 1258 !       ALPHAM  R.A.  Maximum Charnock Coefficient
 1259 !       WLV     R.A.  Water levels.
 1260 !       UA      R.A.  Absolute wind speeds.
 1261 !       UD      R.A.  Absolute wind direction.
 1262 !       U10     R.A.  Wind speed used.
 1263 !       U10D    R.A.  Wind direction used.
 1264 ! 10. Source code :
 1265 !
 
 
                                                         w3src4md.F90  page  24
 
 
 1266 !/ ------------------------------------------------------------------- /
 1267       REAL                    :: USTARM, ALPHAM, LEVTAILM
 1268       REAL                    :: CONST1, OMEGA, OMEGAC, LEVTAIL
 1269       REAL                    :: UST, UST0, ZZ0,OMEGACC, CM
 1270       REAL                    :: TAUW, TAUW0
 1271       INTEGER, PARAMETER      :: JTOT=250
 1272       REAL, ALLOCATABLE       :: W(:)
 1273       REAL                    :: ZX,ZARG,ZMU,ZLOG,ZBETA
 1274       REAL                    :: Y,YC,DELY
 1275       INTEGER                 :: I, J, K, L
 1276       REAL                    :: X0, INSIGMAX, INAALPHA, INBBETA, INZZALP>
 1277       INTEGER                 :: INIUSTAR, INIALPHA, INILEVTAIL, IERR
 1278       CHARACTER(160)          :: FNAMETAB
 1279       LOGICAL                 :: NOFILE
 1280       CHARACTER(LEN=10), PARAMETER :: VERGRD = '2018-06-08'
 1281       CHARACTER(LEN=35), PARAMETER :: IDSTR = 'WAVEWATCH III ST4 TABLE FO>
 1282       CHARACTER(LEN=10)       :: VERTST=' '
 1283       CHARACTER(LEN=35)       :: IDTST=' '
 1284 !
 1285       FNAMETAB='ST4TABUHF2.bin'
 1286       NOFILE=.TRUE.
 1287       OPEN (993,FILE=FNAMETAB,FORM='UNFORMATTED',IOSTAT=IERR,STATUS='OLD')
 1288       IF (IERR.EQ.0) THEN
 1289         READ(993,IOSTAT=IERR) IDTST, VERTST, INSIGMAX, INAALPHA, INBBETA,>
 1290                               INIALPHA, INILEVTAIL, INZZALP, INKAPPA, ING>
 1291         IF (VERTST.EQ.VERGRD.AND.IDTST.EQ.IDSTR.AND.IERR.EQ.0            >
 1292             .AND.INSIGMAX.EQ.SIGMAX.AND.INAALPHA.EQ.AALPHA.AND.INBBETA.EQ>
 1293           IF (INIUSTAR.EQ.IUSTAR.AND.INIALPHA.EQ.IALPHA.AND.INILEVTAIL.EQ>
 1294               INZZALP.EQ.ZZALP.AND.INGRAV.EQ.GRAV.AND.INKAPPA.EQ.KAPPA) T>
 1295             NOFILE=.FALSE.
 1296           ELSE
 1297             CLOSE(993)
 1298             END IF
 1299           END IF
 1300         END IF
 1301 !
 1302       USTARM = 5.
 1303       ALPHAM = 20.*AALPHA
 1304       LEVTAILM = 0.05
 1305       DELUST  = USTARM/REAL(IUSTAR)
 1306       DELALP  = ALPHAM/REAL(IALPHA)
 1307       DELTAIL = ALPHAM/REAL(ILEVTAIL)
 1308       CONST1  = BBETA/KAPPA**2
 1309       OMEGAC  = SIGMAX
 1310 800   CONTINUE
 1311       IF ( NOFILE ) THEN
 1312         WRITE(NDSE,*) 'Filling 3D look-up table for SIN4. please wait'
 1313         WRITE(NDSE,*)  IDSTR, VERGRD, SIGMAX, AALPHA, BBETA, IUSTAR, IALP>
 1314                        ILEVTAIL, ZZALP, KAPPA, GRAV
 1315 !
 1316         TAUHFT(0:IUSTAR,0:IALPHA)=0.  !table initialization
 1317 !
 1318         ALLOCATE(W(JTOT))
 1319         W(2:JTOT-1)=1.
 1320         W(1)=0.5
 
 
                                                         w3src4md.F90  page  25
 
 
 1321         W(JTOT)=0.5
 1322         X0 = 0.05
 1323 !
 1324         DO K=0,IUSTAR
 1325           UST0      = MAX(REAL(K)*DELUST,0.000001)
 1326           DO L=0,IALPHA
 1327             UST=UST0
 1328             ZZ0       = UST0**2*(AALPHA+FLOAT(L)*DELALP)/GRAV
 1329             OMEGACC  = MAX(OMEGAC,X0*GRAV/UST)
 1330             YC       = OMEGACC*SQRT(ZZ0/GRAV)
 1331             DELY     = MAX((1.-YC)/REAL(JTOT),0.)
 1332           ! For a given value of UST and ALPHA,
 1333           ! the wave-supported stress is integrated all the way
 1334           ! to 0.05*g/UST
 1335             DO I=0,ILEVTAIL
 1336               LEVTAIL=REAL(I)*DELTAIL
 1337               TAUHFT(K,L)=0.
 1338               TAUHFT2(K,L,I)=0.
 1339               TAUW0=UST0**2
 1340               TAUW=TAUW0
 1341               DO J=1,JTOT
 1342                 Y        = YC+REAL(J-1)*DELY
 1343                 OMEGA    = Y*SQRT(GRAV/ZZ0)
 1344                 ! This is the deep water phase speed
 1345                 CM       = GRAV/OMEGA
 1346                 !this is the inverse wave age, shifted by ZZALP (tuning)
 1347                 ZX       = UST0/CM +ZZALP
 1348                 ZARG     = MIN(KAPPA/ZX,20.)
 1349                 ZMU      = MIN(GRAV*ZZ0/CM**2*EXP(ZARG),1.)
 1350                 ZLOG     = MIN(ALOG(ZMU),0.)
 1351                 ZBETA        = CONST1*ZMU*ZLOG**4
 1352                 ! Power of Y in denominator should be FACHFE-4
 1353                 TAUHFT(K,L)  = TAUHFT(K,L)+W(J)*ZBETA/Y*DELY
 1354                 ZX       = UST/CM +ZZALP
 1355                 ZARG     = MIN(KAPPA/ZX,20.)
 1356                 ZMU      = MIN(GRAV*ZZ0/CM**2*EXP(ZARG),1.)
 1357                 ZLOG     = MIN(ALOG(ZMU),0.)
 1358                 ZBETA        = CONST1*ZMU*ZLOG**4
 1359                 ! Power of Y in denominator should be FACHFE-4
 1360                 TAUHFT2(K,L,I)  = TAUHFT2(K,L,I)+W(J)*ZBETA*(UST/UST0)**2>
 1361                 TAUW=TAUW-W(J)*UST**2*ZBETA*LEVTAIL/Y*DELY
 1362                 UST=SQRT(MAX(TAUW,0.))
 1363                 END DO
 1364               END DO
 1365             END DO
 1366           END DO
 1367         DEALLOCATE(W)
 1368         OPEN (993,FILE=FNAMETAB,FORM='UNFORMATTED',IOSTAT=IERR,STATUS='UN>
 1369         WRITE(993) IDSTR, VERGRD, SIGMAX, AALPHA, BBETA, IUSTAR, IALPHA, >
 1370         WRITE(993) TAUHFT(0:IUSTAR,0:IALPHA)
 1371         WRITE(993) TAUHFT2
 1372         CLOSE(993)
 1373         !DO K=0,IUSTAR
 1374         !  DO L=0,IALPHA
 1375         !    DO I=0,ILEVTAIL
 
 
                                                         w3src4md.F90  page  26
 
 
 1376         !      WRITE(995,*) K,L,I,MAX(REAL(K)*DELUST,0.000001),AALPHA+FLO>
 1377         !      END DO
 1378         !    END DO
 1379         !  END DO
 1380 !
 1381       ELSE
 1382         WRITE(NDSE,*) 'Reading 3D look-up table for SIN4 from file.'
 1383         READ(993,ERR=2000,IOSTAT=IERR ) TAUHFT(0:IUSTAR,0:IALPHA)
 1384         READ(993,ERR=2000,IOSTAT=IERR ) TAUHFT2
 1385         CLOSE(993)
 1386         END IF
 1387 !
 1388       GOTO 2001
 1389 2000  NOFILE=.TRUE.
 1390       GOTO 800
 1391 2001  CONTINUE
 1392       RETURN
 1393       END SUBROUTINE TABU_TAUHF2
 1394                                 
 1395 !/ ------------------------------------------------------------------- /
 1396       SUBROUTINE CALC_USTAR(WINDSPEED,TAUW,USTAR,Z0,CHARN)
 1397 !/
 1398 !/                  +-----------------------------------+
 1399 !/                  | WAVEWATCH III           NOAA/NCEP |
 1400 !/                  |            F. Ardhuin             |
 1401 !/                  |                        FORTRAN 90 |
 1402 !/                  | Last update 2006/08/14            |
 1403 !/                  +-----------------------------------+
 1404 !/
 1405 !/    27-Feb-2004 : Origination in WW3                  ( version 2.22-SH>
 1406 !/     the resulting table was checked to be identical to the original f7>
 1407 !/    14-Aug-2006 : Modified following Bidlot           ( version 2.22-SH>
 1408 !/    18-Aug-2006 : Ported to version 3.09
 1409 !/    03-Apr-2010 : Adding output of Charnock parameter ( version 3.14-IF>
 1410 !
 1411 !  1. Purpose :
 1412 !
 1413 !     Compute friction velocity based on wind speed U10
 1414 !
 1415 !  2. Method :
 1416 !
 1417 !     Computation of u* based on Quasi-linear theory
 1418 !
 1419 !  3. Parameters :
 1420 !
 1421 !     Parameter list
 1422 !     ----------------------------------------------------------------
 1423 !       U10,TAUW,USTAR,Z0
 1424 !     ----------------------------------------------------------------
 1425 !       WINDSPEED Real  I   10-m wind speed ... should be NEUTRAL
 1426 !       TAUW      Real  I   Wave-supported stress
 1427 !       USTAR     Real  O   Friction velocity.
 1428 !       Z0        Real  O   air-side roughness length
 1429 !     ----------------------------------------------------------------
 1430 !
 
 
                                                         w3src4md.F90  page  27
 
 
 1431 !  4. Subroutines used :
 1432 !
 1433 !       STRACE   Service routine.
 1434 !
 1435 !  5. Called by :
 1436 !
 1437 !       W3SIN3   Wind input Source term routine.
 1438 !
 1439 !  6. Error messages :
 1440 !
 1441 !  7. Remarks :
 1442 !
 1443 !  8. Structure :
 1444 !
 1445 !     See source code.
 1446 !
 1447 !  9. Switches :
 1448 !
 1449 !       !/S      Enable subroutine tracing.
 1450 !       !/T      Enable test output.
 1451 !
 1452 ! 10. Source code :
 1453 !------------------------------------------------------------------------>
 1454       USE CONSTANTS, ONLY: GRAV, KAPPA
 1455       USE W3GDATMD,  ONLY: ZZWND, AALPHA
 1456       IMPLICIT NONE
 1457       REAL, intent(in) :: WINDSPEED,TAUW
 1458       REAL, intent(out) :: USTAR, Z0, CHARN
 1459       ! local variables
 1460       REAL SQRTCDM1
 1461       REAL XI,DELI1,DELI2,XJ,delj1,delj2
 1462       REAL TAUW_LOCAL
 1463       INTEGER IND,J
 1464 !
 1465       TAUW_LOCAL=MAX(MIN(TAUW,TAUWMAX),0.)
 1466       XI      = SQRT(TAUW_LOCAL)/DELTAUW
 1467       IND     = MIN ( ITAUMAX-1, INT(XI)) ! index for stress table
 1468       DELI1   = MIN(1.,XI - REAL(IND))  !interpolation coefficient for st>
 1469       DELI2   = 1. - DELI1
 1470       XJ      = WINDSPEED/DELU
 1471       J       = MIN ( JUMAX-1, INT(XJ) )
 1472       DELJ1   = MIN(1.,XJ - REAL(J))
 1473       DELJ2   = 1. - DELJ1
 1474       USTAR=(TAUT(IND,J)*DELI2+TAUT(IND+1,J  )*DELI1)*DELJ2 &
 1475        + (TAUT(IND,J+1)*DELI2+TAUT(IND+1,J+1)*DELI1)*DELJ1
 1476 !
 1477 ! Determines roughness length
 1478 !
 1479       SQRTCDM1  = MIN(WINDSPEED/USTAR,100.0)
 1480       Z0  = ZZWND*EXP(-KAPPA*SQRTCDM1)
 1481       IF (USTAR.GT.0.001) THEN
 1482         CHARN = GRAV*Z0/USTAR**2
 1483       ELSE
 1484         CHARN = AALPHA
 1485         END IF
 
 
                                                         w3src4md.F90  page  28
 
 
 1486 !
 1487       RETURN
 1488       END SUBROUTINE CALC_USTAR
 1489 !/ ------------------------------------------------------------------- /
 1490       SUBROUTINE W3SDS4 (A, K, CG, USTAR, USDIR, DEPTH, SRHS,      &
 1491                          DDIAG, IX, IY, BRLAMBDA, WHITECAP, DLWMEAN )
 1492 !/
 1493 !/                  +-----------------------------------+
 1494 !/                  | WAVEWATCH III           NOAA/NCEP |
 1495 !/                  ! F. Ardhuin, F. Leckler, L. Romero !
 1496 !/                  |                        FORTRAN 90 |
 1497 !/                  | Last update :         22-Feb-2020 |
 1498 !/                  +-----------------------------------+
 1499 !/
 1500 !/    30-Aug-2010 : Clean up from common ST3-ST4 routine( version 3.14-If>
 1501 !/    23-Jan-2012 : Add output of lambdas to be used in SIN
 1502 !/    13-Nov-2013 : Reduced frequency range with IG1 switch
 1503 !/    06-Jun-2018 : Add optional DEBUGSRC              ( version 6.04 )
 1504 !/    22-Feb-2020 : Option to use Romero (GRL 2019)    ( version 7.06 )
 1505 !/
 1506 !  1. Purpose :
 1507 !
 1508 !     Calculate whitecapping source term and diagonal term of derivative.
 1509 !
 1510 !  2. Method :
 1511 !
 1512 !       This codes does either one or the other of
 1513 !       Ardhuin et al. (JPO 2010)
 1514 !       Filipot & Ardhuin (JGR 2012)
 1515 !       Romero (GRL 2009)
 1516 !       the choice depends on SDSBCHOICE
 1517 !
 1518 !  3. Parameters :
 1519 !
 1520 !     Parameter list
 1521 !     ----------------------------------------------------------------
 1522 !       IX, IY    Int   I   Grid Index
 1523 !       A         R.A.  I   Action density spectrum (1-D).
 1524 !       K         R.A.  I   Wavenumber for entire spectrum.          *)
 1525 !       USTAR     Real  I   Friction velocity.
 1526 !       USDIR     Real  I   wind stress direction.
 1527 !       DEPTH     Real  I   Water depth.
 1528 !       S         R.A.  O   Source term (1-D version).
 1529 !       D         R.A.  O   Diagonal term of derivative.             *)
 1530 !       BRLAMBDA  R.A.  O   Phillips' Lambdas
 1531 !     ----------------------------------------------------------------
 1532 !                         *) Stored in 1-D array with dimension NTH*NK
 1533 !
 1534 !  4. Subroutines used :
 1535 !
 1536 !       STRACE    Subroutine tracing.                 ( !/S switch )
 1537 !       PRT2DS    Print plot of spectrum.             ( !/T0 switch )
 1538 !       OUTMAT    Print out matrix.                   ( !/T1 switch )
 1539 !
 1540 !  5. Called by :
 
 
                                                         w3src4md.F90  page  29
 
 
 1541 !
 1542 !       W3SRCE   Source term integration.
 1543 !       W3EXPO   Point output program.
 1544 !       GXEXPO   GrADS point output program.
 1545 !
 1546 !  6. Error messages :
 1547 !
 1548 !  7. Remarks :
 1549 !
 1550 !  8. Structure :
 1551 !
 1552 !     See source code.
 1553 !
 1554 !  9. Switches :
 1555 !
 1556 !     !/S   Enable subroutine tracing.
 1557 !     !/T   Enable general test output.
 1558 !     !/T0  2-D print plot of source term.
 1559 !     !/T1  Print arrays.
 1560 !
 1561 ! 10. Source code :
 1562 !
 1563 !/ ------------------------------------------------------------------- /
 1564       USE CONSTANTS,ONLY: GRAV, DAIR, DWAT, PI, TPI, RADE, DEBUG_NODE
 1565       USE W3GDATMD, ONLY: NSPEC, NTH, NK, SSDSBR, SSDSBT, DDEN,      &
 1566                           SSDSC, EC2, ES2, ESC,                      &
 1567                           SIG, SSDSP, ECOS, ESIN, DTH, AAIRGB,       &
 1568                           SSDSISO, SSDSDTH, SSDSBM, AAIRCMIN,        &
 1569                           SSDSBRFDF, SSDSBCK, IKTAB, DCKI,           &
 1570                           SATINDICES, SATWEIGHTS, CUMULW, NKHS, NKD, &
 1571                           NDTAB, QBI
 1572       USE W3GDATMD, ONLY: IGPARS
 1573       USE W3ODATMD, ONLY: FLOGRD
 1574 !
 1575       IMPLICIT NONE
 1576 !/
 1577 !/ ------------------------------------------------------------------- /
 1578 !/ Parameter list
 1579 !/
 1580       INTEGER, OPTIONAL, INTENT(IN) :: IX, IY
 1581       REAL, INTENT(IN)        :: A(NSPEC), K(NK), CG(NK),            &
 1582                                  DEPTH, USTAR, USDIR, DLWMEAN
 1583       REAL, INTENT(OUT)       :: SRHS(NSPEC), DDIAG(NSPEC), BRLAMBDA(NSPE>
 1584       REAL, INTENT(OUT)       :: WHITECAP(1:4)
 1585 !/
 1586 !/ ------------------------------------------------------------------- /
 1587 !/ Local parameters
 1588 !/
 1589       INTEGER                 :: IS, IS2, IS0, IKL, IKC, ID, NKL
 1590       INTEGER                 :: IK, IK1, ITH, IK2, JTH, ITH2,           >
 1591                                  IKHS, IKD, SDSNTH, IT, IKM, NKM
 1592       INTEGER                 :: NSMOOTH(NK)
 1593       REAL                    :: C, COSWIND, ASUM, SDIAGISO
 1594       REAL                    :: COEF1, COEF2, COEF4(NK),      &
 1595                                  COEF5(NK)
 
 
                                                         w3src4md.F90  page  30
 
 
 1596                                           
 1597       REAL                    :: FACTURB, FACTURB2, DTURB, DVISC, DIAG2, >
 1598       REAL                    :: RENEWALFREQ, EPSR
 1599       REAL                    :: S1(NK), E1(NK)
 1600       INTEGER                 :: NTIMES(NK)
 1601       REAL                    :: GAM, XT
 1602       REAL                    :: DK(NK), HS(NK), KBAR(NK), DCK(NK)
 1603       REAL                    :: EFDF(NK)     ! Energy integrated over a >
 1604       INTEGER                 :: IKSUP(NK)
 1605       REAL                    :: FACSAT, DKHS, FACSTRAINB, FACSTRAINL
 1606       REAL                    :: BTH0(NK)     !saturation spectrum
 1607       REAL                    :: BTH(NSPEC)   !saturation spectrum
 1608       REAL                    :: BTH0S(NK)    !smoothed saturation spectr>
 1609       REAL                    :: BTHS(NSPEC)  !smoothed saturation spectr>
 1610       INTEGER                 :: IMSSMAX(NK), NTHSUM
 1611       REAL                    :: MSSSUM(NK,5), WTHSUM(NTH), FACHF
 1612       REAL                    :: MSSSUM2(NK,NTH)
 1613       REAL                    :: MSSLONG(NK,NTH)
 1614       REAL                    :: MSSPCS, MSSPC2, MSSPS2, MSSP, MSSD, MSSTH
 1615       REAL                    :: MICHE, X, KLOC
 1616       REAL                    :: QB(NK), S2(NK)
 1617       REAL                    :: TSTR, TMAX, DT, T, MFT
 1618       REAL                    :: PB(NSPEC), PB2(NSPEC), BRM12(NK), BTOVER
 1619       REAL                    :: KO, LMODULATION(NTH)
 1620 !/
 1621 !/ ------------------------------------------------------------------- /
 1622 !/
 1623 !
 1624 !----------------------------------------------------------------------
 1625 !
 1626 ! 0.  Pre-Initialization to zero out arrays. All arrays should be reset
 1627 !     within the computation, but these are helping with some bugs
 1628 !     found in certain compilers
 1629       NSMOOTH=0
 1630       S1=0.; E1=0.
 1631       NTIMES=0;IKSUP=0;IMSSMAX=0
 1632       DK=0.; HS=0.; KBAR=0.; DCK=0.; EFDF=0.
 1633       BTH0=0.; BTH=0.; BTH0S=0.; DDIAG=0.; SRHS=0.; PB=0.
 1634       BTHS=0.; MSSSUM(:,:)=0.
 1635       QB=0.; S2=0.;PB=0.; PB2=0.
 1636       BRM12(:)=0.
 1637 !
 1638 ! 1.  Initialization and numerical factors
 1639 !
 1640       FACTURB=SSDSC(5)*USTAR**2/GRAV*DAIR/DWAT
 1641       BREAKFRACTION=0.
 1642       RENEWALFREQ=0.
 1643       IK1=1
 1644       IK1=NINT(IGPARS(5))+1
 1645       NTHSUM=MIN(FLOOR(SSDSC(10)+0.5),NTH-1)  ! number of angular bins fo>
 1646       IF (NTHSUM.GT.0) THEN
 1647         WTHSUM(1:NTHSUM)=1
 1648         WTHSUM(NTHSUM+1)=SSDSC(10)+0.5-NTHSUM
 1649       ELSE
 1650         WTHSUM(1)=2*SSDSC(10)
 
 
                                                         w3src4md.F90  page  31
 
 
 1651         END IF
 1652 !
 1653 ! 1.b MSS parameters used for Modulation factors for B or lambda
 1654 !
 1655       IF (SSDSC(8).GT.0.OR.SSDSC(11).GT.0.OR.SSDSC(18).GT.0) THEN
 1656         MSSSUM2(:,:)=0.
 1657         DO IK=1,NK
 1658           IMSSMAX (IK) = 1
 1659           MSSP   = 0.
 1660           MSSPC2 = 0.
 1661           MSSPS2 = 0.
 1662           MSSPCS = 0.
 1663 !
 1664 ! Sums the contributions to the directional MSS for all ITH
 1665 !
 1666           DO ITH=1,NTH
 1667             IS=ITH+(IK-1)*NTH
 1668             MSSLONG(IK,ITH) = K(IK)**SSDSC(20) * A(IS) * DDEN(IK) / CG(IK>
 1669             END DO
 1670           DO ITH=1,NTH
 1671             DO JTH=-NTHSUM,NTHSUM
 1672               ITH2 = 1+MOD(ITH-1+JTH+NTH,NTH)
 1673               MSSSUM2(IK,ITH) = MSSSUM2(IK,ITH)+MSSLONG(IK,ITH2)*WTHSUM(A>
 1674               END DO
 1675             MSSPC2 = MSSPC2 +MSSLONG(IK,ITH)*EC2(ITH)
 1676             MSSPS2 = MSSPS2 +MSSLONG(IK,ITH)*ES2(ITH)
 1677             MSSPCS = MSSPCS +MSSLONG(IK,ITH)*ESC(ITH)
 1678             MSSP   = MSSP   +MSSLONG(IK,ITH)
 1679             END DO
 1680 !
 1681 ! Now sums over IK
 1682 !
 1683           MSSSUM  (IK:NK,1) = MSSSUM (IK:NK,1) +MSSP
 1684           MSSSUM  (IK:NK,3) = MSSSUM (IK:NK,3) +MSSPC2
 1685           MSSSUM  (IK:NK,4) = MSSSUM (IK:NK,4) +MSSPS2
 1686           MSSSUM  (IK:NK,5) = MSSSUM (IK:NK,5) +MSSPCS
 1687 !
 1688 ! Direction of long wave mss summed up to IK
 1689 !
 1690           MSSD=0.5*(ATAN2(2*MSSSUM(IK,5),MSSSUM(IK,3)-MSSSUM(IK,4)))
 1691           IF (MSSD.LT.0) MSSD = MSSD + PI
 1692             IMSSMAX (IK)=1+NINT(MSSD *NTH/TPI)
 1693 !
 1694 ! mss along perpendicular direction
 1695 !
 1696             MSSSUM  (IK,2)  = MAX(0.,MSSSUM(IK,4)*COS(MSSD)**2          &
 1697                                   -2*MSSSUM(IK,5)*SIN(MSSD)*COS(MSSD)+  &
 1698                                      MSSSUM(IK,3)*SIN(MSSD)**2 )
 1699           END DO
 1700         END IF ! SSDSC(8).GT.0) THEN
 1701 !
 1702 ! 2.   Estimation of spontaneous breaking from local saturation
 1703 !
 1704       SELECT CASE (NINT(SSDSC(1)))
 1705       CASE (1)
 
 
                                                         w3src4md.F90  page  32
 
 
 1706 !
 1707 ! 2.a  Case of a direction-dependent breaking term following Ardhuin et a>
 1708 !
 1709         EPSR = SQRT(SSDSBR)
 1710 !
 1711 ! 2.a.1 Computes saturation
 1712 !
 1713         SDSNTH = MIN(NINT(SSDSDTH/(DTH*RADE)),NTH/2-1)
 1714 !       SSDSDIK is the integer difference in frequency bands
 1715 !       between the "large breakers" and short "wiped-out waves"
 1716 !
 1717         BTH(:) = 0.
 1718                    
 1719         DO  IK=IK1, NK
 1720                       
 1721           FACSAT=SIG(IK)*K(IK)**3*DTH
 1722           IS0=(IK-1)*NTH
 1723           BTH(IS0+1)=0.
 1724           ASUM = SUM(A(IS0+1:IS0+NTH))
 1725           BTH0(IK)=ASUM*FACSAT
 1726           IKC = MAX(1,IK-DIKCUMUL)
 1727           KLOC=K(IK)**(2-SSDSC(20)) ! local wavenumber factor, if mss not>
 1728                                                                           
 1729           IF (SSDSDTH.GE.180) THEN  ! integrates around full circle
 1730             BTH(IS0+1:IS0+NTH)=BTH0(IK)
 1731           ELSE
 1732             DO ITH=1,NTH            ! partial integration
 1733               IS=ITH+(IK-1)*NTH
 1734                                
 1735 ! straining effect of long waves on short waves
 1736 ! extended from Longuet-Higgins and Stewart (JFM 1960, eq. 2.27) the ampl>
 1737 ! in deep water is equal to the long wave slope k*a cos(theta1-theta2)
 1738 ! Here we assume that the saturation is modulated as (1 + SSDSC(8) *  sqr>
 1739 ! where mss_theta is the mss in direction ITH.
 1740 !
 1741 ! Note: SSDSC(8) is sqrt(2)*times the mss MTF: equal to 4*sqrt(2) accordi>
 1742 !
 1743               IF (SSDSC(8).GT.0.OR.SSDSC(11).GT.0) THEN
 1744 !
 1745                 MSSTH=(MSSSUM(IKC,1)-MSSSUM(IKC,2))*EC2(1+ABS(ITH-IMSSMAX>
 1746                         +MSSSUM(IKC,2)*ES2(1+ABS(ITH-IMSSMAX (IKC)))*KLOC
 1747 !
 1748                 FACSTRAINB=1+SSDSC(8)*SQRT(MSSTH)+SSDSC(11)*SQRT(MSSSUM2(>
 1749               ELSE
 1750                 FACSTRAINB=1
 1751                 END IF
 1752 !
 1753               BTH(IS)=DOT_PRODUCT(SATWEIGHTS(:,ITH),  A(IS0+SATINDICES(:,>
 1754                     *FACSAT*FACSTRAINB
 1755               END DO
 1756                     
 1757             IF (SSDSISO.NE.1) THEN
 1758               BTH0(IK)=MAXVAL(BTH(IS0+1:IS0+NTH))
 1759               END IF
 1760             END IF
 
 
                                                         w3src4md.F90  page  33
 
 
 1761 !
 1762           END DO !NK END
 1763 !
 1764 !   Optional smoothing of B and B0 over frequencies
 1765 !
 1766         IF (SSDSBRFDF.GT.0.AND.SSDSBRFDF.LT.NK/2) THEN
 1767           BTH0S(:)=BTH0(:)
 1768           BTHS(:)=BTH(:)
 1769           NSMOOTH(:)=1
 1770           DO IK=1, SSDSBRFDF
 1771             BTH0S(1+SSDSBRFDF)=BTH0S(1+SSDSBRFDF)+BTH0(IK)
 1772             NSMOOTH(1+SSDSBRFDF)=NSMOOTH(1+SSDSBRFDF)+1
 1773             DO ITH=1,NTH
 1774               IS=ITH+(IK-1)*NTH
 1775               BTHS(ITH+SSDSBRFDF*NTH)=BTHS(ITH+SSDSBRFDF*NTH)+BTH(IS)
 1776               END DO
 1777             END DO
 1778           DO IK=IK1+1+SSDSBRFDF,1+2*SSDSBRFDF
 1779             BTH0S(1+SSDSBRFDF)=BTH0S(1+SSDSBRFDF)+BTH0(IK)
 1780             NSMOOTH(1+SSDSBRFDF)=NSMOOTH(1+SSDSBRFDF)+1
 1781             DO ITH=1,NTH
 1782               IS=ITH+(IK-1)*NTH
 1783               BTHS(ITH+SSDSBRFDF*NTH)=BTHS(ITH+SSDSBRFDF*NTH)+BTH(IS)
 1784               END DO
 1785             END DO
 1786           DO IK=SSDSBRFDF,IK1,-1
 1787             BTH0S(IK)=BTH0S(IK+1)-BTH0(IK+SSDSBRFDF+1)
 1788             NSMOOTH(IK)=NSMOOTH(IK+1)-1
 1789             DO ITH=1,NTH
 1790               IS=ITH+(IK-1)*NTH
 1791               BTHS(IS)=BTHS(IS+NTH)-BTH(IS+(SSDSBRFDF+1)*NTH)
 1792               END DO
 1793             END DO
 1794 !
 1795           DO IK=IK1+1+SSDSBRFDF,NK-SSDSBRFDF
 1796             BTH0S(IK)=BTH0S(IK-1)-BTH0(IK-SSDSBRFDF-1)+BTH0(IK+SSDSBRFDF)
 1797             NSMOOTH(IK)=NSMOOTH(IK-1)
 1798             DO ITH=1,NTH
 1799               IS=ITH+(IK-1)*NTH
 1800               BTHS(IS)=BTHS(IS-NTH)-BTH(IS-(SSDSBRFDF+1)*NTH)+BTH(IS+(SSD>
 1801               END DO
 1802             END DO
 1803 !
 1804           DO IK=NK-SSDSBRFDF+1,NK
 1805             BTH0S(IK)=BTH0S(IK-1)-BTH0(IK-SSDSBRFDF)
 1806             NSMOOTH(IK)=NSMOOTH(IK-1)-1
 1807             DO ITH=1,NTH
 1808               IS=ITH+(IK-1)*NTH
 1809               BTHS(IS)=BTHS(IS-NTH)-BTH(IS-(SSDSBRFDF+1)*NTH)
 1810               END DO
 1811             END DO
 1812 !    division by NSMOOTH
 1813           BTH0(:)=MAX(0.,BTH0S(:)/NSMOOTH(:))
 1814           DO IK=IK1,NK
 1815             IS0=(IK-1)*NTH
 
 
                                                         w3src4md.F90  page  34
 
 
 1816             BTH(IS0+1:IS0+NTH)=MAX(0.,BTHS(IS0+1:IS0+NTH)/NSMOOTH(IK))
 1817             END DO
 1818           END IF  ! end of optional smoothing
 1819 !
 1820 !  2.a.2  Computes spontaneous breaking dissipation rate
 1821 !
 1822         DO  IK=IK1, NK
 1823 !
 1824 !  Correction of saturation level for shallow-water kinematics
 1825 !
 1826           IF (SSDSBM(0).EQ.1) THEN
 1827             MICHE=1.
 1828           ELSE
 1829             X=TANH(MIN(K(IK)*DEPTH,10.))
 1830             MICHE=(X*(SSDSBM(1)+X*(SSDSBM(2)+X*(SSDSBM(3)+X*SSDSBM(4)))))>
 1831             END IF
 1832           COEF1=(SSDSBR*MICHE)
 1833 !
 1834 !  Computes isotropic part
 1835 !
 1836           SDIAGISO = SSDSC(2) * SIG(IK)*SSDSC(6)*(MAX(0.,BTH0(IK)/COEF1-1>
 1837 !
 1838 !  Computes anisotropic part and sums isotropic part
 1839 !
 1840           COEF2=SSDSC(2) * SIG(IK)*(1-SSDSC(6))/(COEF1*COEF1)
 1841           DDIAG((IK-1)*NTH+1:IK*NTH) = SDIAGISO + &
 1842                                    COEF2*((MAX(0.,BTH((IK-1)*NTH+1:IK*NTH>
 1843           END DO
 1844 !
 1845 ! Computes Breaking probability
 1846 !
 1847       PB = (MAX(SQRT(BTH)-EPSR,0.))**2
 1848 !
 1849 ! Multiplies by 28.16 = 22.0 * 1.6² * 1/2 with
 1850 !  22.0 (Banner & al. 2000, figure 6)
 1851 !  1.6  the coefficient that transforms  SQRT(B) to Banner et al. (2000)'>
 1852 !  1/2  factor to correct overestimation of Banner et al. (2000)'s breaki>
 1853 !
 1854       PB = PB * 28.16
 1855 ! Compute Lambda = PB* l(k,th)
 1856 ! with l(k,th)=1/(2*pi²)= the breaking crest density
 1857       BRLAMBDA = PB / (2.*PI**2.)
 1858       SRHS = DDIAG * A
 1859                       
 1860 !
 1861     CASE(2)
 1862 !
 1863 ! 2.b             Computes spontaneous breaking for T500 (Filipot et al. >
 1864 !
 1865       E1 = 0.
 1866       HS = 0.
 1867       SRHS  = 0.
 1868       DDIAG = 0.
 1869       PB2  = 0.
 1870 !
 
 
                                                         w3src4md.F90  page  35
 
 
 1871 ! Computes Wavenumber spectrum E1 integrated over direction and computes >
 1872 !
 1873       DO IK=IK1, NK
 1874         E1(IK)=0.
 1875         DO ITH=1,NTH
 1876           IS=ITH+(IK-1)*NTH
 1877           E1(IK)=E1(IK)+(A(IS)*SIG(IK))*DTH
 1878           END DO
 1879         DK(IK)=DDEN(IK)/(DTH*SIG(IK)*CG(IK))
 1880         END DO
 1881 !
 1882 ! Gets windows indices of IKTAB
 1883 !
 1884       ID=MIN(NINT(DEPTH),NDTAB)
 1885       IF (ID < 1) THEN
 1886         ID = 1
 1887       ELSE IF(ID > NDTAB) THEN
 1888         ID = NDTAB
 1889         END IF
 1890 !
 1891 ! loop over wave scales
 1892 !
 1893       HS=0.
 1894       EFDF=0.
 1895       KBAR=0.
 1896       NKL=0. !number of windows
 1897       DO IKL=1,NK
 1898         IKSUP(IKL)=IKTAB(IKL,ID)
 1899         IF (IKSUP(IKL) .LE. NK) THEN
 1900           EFDF(IKL) = DOT_PRODUCT(E1(IKL:IKSUP(IKL)-1),DK(IKL:IKSUP(IKL)->
 1901           IF (EFDF(IKL) .NE. 0) THEN
 1902             KBAR(IKL) = DOT_PRODUCT(K(IKL:IKSUP(IKL)-1)*E1(IKL:IKSUP(IKL)>
 1903                                     DK(IKL:IKSUP(IKL)-1)) / EFDF(IKL)
 1904           ELSE
 1905             KBAR(IKL)=0.
 1906             END IF
 1907 ! estimation of Significant wave height of a given scale
 1908           HS(IKL) = 4*SQRT(EFDF(IKL))
 1909           NKL = NKL+1
 1910           END IF
 1911         END DO
 1912 !
 1913 ! Computes Dissipation and breaking probability in each scale
 1914 !
 1915       DCK=0.
 1916       QB =0.
 1917       DKHS = KHSMAX/NKHS
 1918       DO IKL=1, NKL
 1919         IF (HS(IKL) .NE. 0. .AND. KBAR(IKL) .NE. 0.)  THEN
 1920 ! gets indices for tabulated dissipation DCKI and breaking probability QBI
 1921 !
 1922           IKD = FAC_KD2+ANINT(LOG(KBAR(IKL)*DEPTH)/LOG(FAC_KD1))
 1923           IKHS= 1+ANINT(KBAR(IKL)*HS(IKL)/DKHS)
 1924           IF (IKD > NKD) THEN    ! Deep water
 1925             IKD = NKD
 
 
                                                         w3src4md.F90  page  36
 
 
 1926           ELSE IF (IKD < 1) THEN ! Shallow water
 1927             IKD = 1
 1928             END IF
 1929           IF (IKHS > NKHS) THEN
 1930             IKHS = NKHS
 1931           ELSE IF (IKHS < 1) THEN
 1932             IKHS = 1
 1933             END IF
 1934           XT = TANH(KBAR(IKL)*DEPTH)
 1935 !
 1936 ! Gamma corrected for water depth
 1937 !
 1938           GAM=1.0314*(XT**3)-1.9958*(XT**2)+1.5522*XT+0.1885
 1939 !
 1940 ! Computes the energy dissipated for the scale IKL
 1941 ! using DCKI which is tabulated in INSIN4
 1942 !
 1943           DCK(IKL)=((KBAR(IKL)**(-2.5))*(KBAR(IKL)/(2*PI)))*DCKI(IKHS,IKD)
 1944 !
 1945 ! Get the breaking probability for the scale IKL
 1946 !
 1947           QB(IKL) = QBI(IKHS,IKD) ! QBI is tabulated in INSIN4
 1948         ELSE
 1949           DCK(IKL)=0.
 1950           QB(IKL) =0.
 1951           END IF
 1952         END DO
 1953 !
 1954 ! Distributes scale dissipation over the frequency spectrum
 1955 !
 1956       S1 = 0.
 1957       S2 = 0.
 1958       NTIMES = 0
 1959       DO IKL=1, NKL
 1960         IF (EFDF(IKL) .GT. 0.) THEN
 1961           S1(IKL:IKSUP(IKL))    = S1(IKL:IKSUP(IKL)) + &
 1962                                   DCK(IKL)*E1(IKL:IKSUP(IKL)) / EFDF(IKL)
 1963           S2(IKL:IKSUP(IKL))    = S2(IKL:IKSUP(IKL)) + &
 1964                                   QB(IKL) *E1(IKL:IKSUP(IKL)) / EFDF(IKL)
 1965           NTIMES(IKL:IKSUP(IKL)) = NTIMES(IKL:IKSUP(IKL)) + 1
 1966           END IF
 1967         END DO
 1968 !
 1969 ! Finish the average
 1970 !
 1971       WHERE (NTIMES .GT. 0)
 1972         S1 = S1 / NTIMES
 1973         S2 = S2 / NTIMES
 1974       ELSEWHERE
 1975         S1 = 0.
 1976         S2 = 0.
 1977         END WHERE
 1978 ! goes back to action for dissipation source term
 1979       S1(1:NK) = S1(1:NK) / SIG(1:NK)
 1980 !
 
 
                                                         w3src4md.F90  page  37
 
 
 1981 ! Makes Isotropic distribution
 1982 !
 1983       ASUM = 0.
 1984       DO IK = 1, NK
 1985         ASUM = (SUM(A(((IK-1)*NTH+1):(IK*NTH)))*DTH)
 1986         IF (ASUM.GT.1.E-8) THEN
 1987           FORALL (IS=1+(IK-1)*NTH:IK*NTH) DDIAG(IS)  = S1(IK)/ASUM
 1988           FORALL (IS=1+(IK-1)*NTH:IK*NTH) PB2(IS) = S2(IK)/ASUM
 1989         ELSE
 1990           FORALL (IS=1+(IK-1)*NTH:IK*NTH) DDIAG(IS)  = 0.
 1991           FORALL (IS=1+(IK-1)*NTH:IK*NTH) PB2(IS) = 0.
 1992           END IF
 1993         IF (PB2(1+(IK-1)*NTH).GT.0.001) THEN
 1994           BTH0(IK) = 2.*SSDSBR
 1995         ELSE
 1996           BTH0(IK) = 0.
 1997           END IF
 1998         END DO
 1999 !
 2000       PB = (1-SSDSC(1))*PB2*A + SSDSC(1)*PB
 2001 ! Compute Lambda = PB* l(k,th)
 2002 ! with l(k,th)=1/(2*pi²)= the breaking crest density
 2003       BRLAMBDA = PB / (2.*PI**2.)
 2004 !
 2005     CASE(3)
 2006 !
 2007 ! 2c Romero (GRL 2019)
 2008 !
 2009 ! directional saturation I
 2010 ! integrate in azimuth
 2011       KO=(GRAV/(1E-6+USTAR**2))/(28./SSDSC(16))**2
 2012       DO IK=1,NK
 2013         IS0=(IK-1)*NTH
 2014         KLOC=K(IK)**(2-SSDSC(20)) ! local wavenumber factor, if mss not u>
 2015         BTH(1:NTH)=MAX(A(IS0+1:IS0+NTH)*SIG(IK)*K(IK)**3,.00000000000001)
 2016 !
 2017         IF (SSDSC(8).GT.0) THEN ! Applies modulation factor on B
 2018           DO ITH=1,NTH
 2019             MSSTH=(MSSSUM(IK,1)-MSSSUM(IK,2))*EC2(1+ABS(ITH-IMSSMAX (IK))>
 2020                  +MSSSUM(IK,2)*ES2(1+ABS(ITH-IMSSMAX (IK)))*KLOC
 2021             FACSTRAINB=(1.+SSDSC(8)*SQRT(MSSTH)+SSDSC(11)*SQRT(MSSSUM2(IK>
 2022             BTH(ITH)=BTH(ITH)*FACSTRAINB
 2023             END DO
 2024           END IF
 2025 !
 2026         C=SIG(IK)/K(IK)
 2027         BTH0(IK)=sum(BTH(1:NTH)*DTH)
 2028         IF (SSDSC(18).GT.0) THEN ! Applies modulation factor on Lambda
 2029           DO ITH=1,NTH
 2030             IF (SSDSC(11).GT.0) THEN
 2031               MSSTH=(MSSSUM(IK,1)-MSSSUM(IK,2))*EC2(1+ABS(ITH-IMSSMAX (IK>
 2032                        +MSSSUM(IK,2)*ES2(1+ABS(ITH-IMSSMAX (IK)))*KLOC
 2033               FACSTRAINL=1.+SSDSC(18)*SQRT(MSSTH)+SSDSC(11)*SQRT(MSSSUM2(>
 2034             ELSE
 2035               FACSTRAINL=1.+SSDSC(18)*((MSSSUM(IK,1)*KLOC)**SSDSC(14) *  >
 
 
                                                         w3src4md.F90  page  38
 
 
 2036                 (ECOS(ITH)*COS(DLWMEAN)+ESIN(ITH)*SIN(DLWMEAN))**2)
 2037               ENDIF
 2038             LMODULATION(ITH)= FACSTRAINL**SSDSC(19)
 2039             END DO
 2040         ELSE
 2041           LMODULATION(:)= 1.
 2042           END IF
 2043                 
 2044 ! Lambda
 2045         BRLAMBDA(IS0+1:IS0+NTH)=SSDSC(9)*EXP(-SSDSBR/BTH(1:NTH))         >
 2046                                  *( 1.0+SSDSC(13)*MAX(1.,(K(IK)/KO))**SSD>
 2047                                  /(SSDSC(13)+1)*LMODULATION(1:NTH)
 2048 ! Breaking strength : generalisation of Duncan's b parameter
 2049         BTOVER = SQRT(BTH0(IK))-SQRT(SSDSBT)
 2050         BRM12(IK)=SSDSC(2)*(MAX(0.,BTOVER))**(2.5)/SIG(IK)  ! not functio>
 2051 !  For consistency set BRLAMBDA set to zero if b is zero
 2052         BRLAMBDA(IS0+1:IS0+NTH)= MAX(0.,SIGN(BRLAMBDA(IS0+1:IS0+NTH),BTOV>
 2053 !  Source term / sig2  (action dissipation)
 2054         SRHS(IS0+1:IS0+NTH)= BRM12(IK)/GRAV**2*BRLAMBDA(IS0+1:IS0+NTH)*C*>
 2055 ! diagonal
 2056         DDIAG(IS0+1:IS0+NTH) = SRHS(IS0+1:IS0+NTH)*SSDSBR/MAX(1.e-20,BTH(>
 2057        END DO
 2058 !   Breaking probability (Is actually the breaking rate)
 2059       PB = BRLAMBDA *C
 2060 !
 2061       END SELECT
 2062 !
 2063 !/ ------------------------------------------------------------------- /
 2064 !             WAVE-TURBULENCE INTERACTION AND CUMULATIVE EFFECT
 2065 !/ ------------------------------------------------------------------- /
 2066 !
 2067 ! loop over spectrum
 2068 !
 2069     IF ( (SSDSC(3).NE.0.) .OR. (SSDSC(5).NE.0.) .OR. (SSDSC(21).NE.0.) ) >
 2070       DO  IK=IK1, NK
 2071         FACTURB2=-2.*SIG(IK)*K(IK)*FACTURB
 2072         DVISC=-4.*SSDSC(21)*K(IK)*K(IK)
 2073 !
 2074         DO ITH=1,NTH
 2075           IS=ITH+(IK-1)*NTH
 2076 !
 2077 ! Computes cumulative effect from Breaking probability
 2078 !
 2079           RENEWALFREQ = 0.
 2080           IF (SSDSC(3).NE.0 .AND. IK.GT.DIKCUMUL) THEN
 2081             DO IK2=IK1,IK-DIKCUMUL
 2082               IF (BTH0(IK2).GT.SSDSBR) THEN
 2083                 IS2=(IK2-1)*NTH
 2084                 RENEWALFREQ=RENEWALFREQ+DOT_PRODUCT(CUMULW(IS2+1:IS2+NTH,>
 2085                 END IF
 2086               END DO
 2087             END IF
 2088 !
 2089 ! Computes wave turbulence interaction
 2090 !
 
 
                                                         w3src4md.F90  page  39
 
 
 2091           COSWIND=(ECOS(ITH)*COS(USDIR)+ESIN(ITH)*SIN(USDIR))
 2092           DTURB=FACTURB2*MAX(0.,COSWIND)  ! Theory -> stress direction
 2093 !
 2094 ! Add effects
 2095 !
 2096           DIAG2 = (SSDSC(3)*RENEWALFREQ+DTURB+DVISC)
 2097           DDIAG(IS) = DDIAG(IS) + DIAG2
 2098           SRHS(IS)  = SRHS(IS)  + A(IS)* DIAG2
 2099           END DO
 2100         END DO
 2101       END IF
 2102 !
 2103 !  COMPUTES WHITECAP PARAMETERS
 2104 !
 2105       IF ( .NOT. (FLOGRD(5,7).OR.FLOGRD(5,8) ) ) THEN
 2106         RETURN
 2107         END IF
 2108 !
 2109       WHITECAP(1:2) = 0.
 2110 !
 2111 ! precomputes integration of Lambda over direction
 2112 ! times wavelength times a (a=5 in Reul&Chapron JGR 2003) times dk
 2113 !
 2114       DO IK=1,MIN(FLOOR(AAIRCMIN),NK)
 2115         C=SIG(IK)/K(IK)
 2116         IS0=(IK-1)*NTH
 2117         COEF4(IK) = C*C*SUM(BRLAMBDA(IS0+1:IS0+NTH))                     >
 2118                          *2.*PI/GRAV*SSDSC(7) * DDEN(IK)/(SIG(IK)*CG(IK))
 2119         COEF5(IK) = C**3*SUM(BRLAMBDA(IS0+1:IS0+NTH)                     >
 2120                     *BRM12(IK))                       	       &
 2121                     *AAIRGB/GRAV * DDEN(IK)/(SIG(IK)*CG(IK))
 2122 !        COEF4(IK) = SUM(BRLAMBDA((IK-1)*NTH+1:IK*NTH) * DTH) *(2*PI/K(IK>
 2123 !                    SSDSC(7) * DDEN(IK)/(DTH*SIG(IK)*CG(IK))
 2124 !                   NB: SSDSC(7) is WHITECAPWIDTH
 2125         END DO
 2126 ! Need to extrapolate above NK if necessary ... to be added later.
 2127       DO IK=MIN(FLOOR(AAIRCMIN),NK),NK
 2128         COEF4(IK)=0.
 2129         COEF5(IK)=0.
 2130         END DO
 2131               
 2132 !/
 2133       IF ( FLOGRD(5,7) ) THEN
 2134 !
 2135 ! Computes the Total WhiteCap Coverage (a=5. ; Reul and Chapron, 2003)
 2136 !
 2137         DO IK=IK1,MIN(FLOOR(AAIRCMIN),NK)
 2138           WHITECAP(1) = WHITECAP(1) + COEF4(IK) * (1-WHITECAP(1))
 2139           WHITECAP(4) = WHITECAP(4) + COEF5(IK)
 2140           END DO
 2141         END IF
 2142 !/
 2143       IF ( FLOGRD(5,8) ) THEN
 2144 !
 2145 ! Calculates the Mean Foam Thickness for component K(IK) => Fig.3, Reul a>
 
 
                                                         w3src4md.F90  page  40
 
 
 2146 ! ( Copied from ST4 - not yet tested/validated with Romero 2019 (Lambda m>
 2147 !
 2148         DO IK=IK1,NK
 2149 !    Duration of active breaking (TAU*)
 2150           TSTR = 0.8 * 2*PI/SIG(IK)
 2151 !    Time persistence of foam (a=5.)
 2152           TMAX = 5.  * 2*PI/SIG(IK)
 2153           DT   = TMAX / 50
 2154           MFT  = 0.
 2155           DO IT = 1, 50
 2156 ! integration over time of foam persistance
 2157             T = FLOAT(IT) * DT
 2158 ! Eq. 5 and 6 of Reul and Chapron, 2003
 2159             IF ( T .LT. TSTR ) THEN
 2160               MFT = MFT + 0.4 / (K(IK)*TSTR) * T * DT
 2161             ELSE
 2162               MFT = MFT + 0.4 / K(IK) * EXP(-1*(T-TSTR)/3.8) * DT
 2163               END IF
 2164             END DO
 2165           MFT = MFT / TMAX
 2166 !
 2167 ! Computes foam-layer thickness (Reul and Chapron, 2003)
 2168 !
 2169           WHITECAP(2) = WHITECAP(2) + COEF4(IK) * MFT
 2170           END DO
 2171         END IF
 2172 !
 2173 ! End of output computing
 2174 !
 2175       RETURN
 2176 !
 2177 ! Formats
 2178 !
 2179 !/
 2180 !/ End of W3SDS4 ----------------------------------------------------- /
 2181 !/
 2182       END SUBROUTINE W3SDS4
 2183                            
 2184                            
 2185       END MODULE W3SRC4MD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
