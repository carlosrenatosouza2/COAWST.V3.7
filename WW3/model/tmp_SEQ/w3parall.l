                                                         w3parall.F90  page   1
 
 
    1       MODULE W3PARALL
    2 !/
    3 !/                  +-----------------------------------+
    4 !/                  | WAVEWATCH III           NOAA/NCEP |
    5 !/                  |                                   |
    6 !/                  | Aron Roland (BGS IT&E GmbH)       |
    7 !/                  | Mathieu Dutour-Sikiric (IRB)      |
    8 !/                  |                                   |
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :        01-June-2018 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/   01-June-2018 : Origination.                        ( version 6.04 )
   14 !/
   15 !  1. Purpose : Parallel routines for implicit solver
   16 !  2. Method :
   17 !  3. Parameters :
   18 !
   19 !     Parameter list
   20 !     ----------------------------------------------------------------
   21 !     ----------------------------------------------------------------
   22 !
   23 !  4. Subroutines used :
   24 !
   25 !      Name      Type  Module   Description
   26 !     ----------------------------------------------------------------
   27 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   28 !     ----------------------------------------------------------------
   29 !
   30 !  5. Called by :
   31 !
   32 !      Name      Type  Module   Description
   33 !     ----------------------------------------------------------------
   34 !     ----------------------------------------------------------------
   35 !
   36 !  6. Error messages :
   37 !  7. Remarks
   38 !  8. Structure :
   39 !  9. Switches :
   40 !
   41 !     !/S  Enable subroutine tracing.
   42 !
   43 ! 10. Source code :
   44 !
   45 !/ ------------------------------------------------------------------- /
   46 !/
   47 !/ ------------------------------------------------------------------- /
   48 !/ Parameter list
   49 !/
   50 !/ ------------------------------------------------------------------- /
   51 !/ Local parameters
   52 !/
   53 !
   54  
   55       REAL, ALLOCATABLE    :: AC_tot(:,:)
 
 
                                                         w3parall.F90  page   2
 
 
   56       INTEGER, ALLOCATABLE :: ListISPnextDir(:), ListISPprevDir(:)
   57       INTEGER, ALLOCATABLE :: ListISPnextFreq(:), ListISPprevFreq(:)
   58       INTEGER, PARAMETER   :: IMEM = 2
   59                                       
   60       REAL,  PARAMETER     :: ONESIXTH  = 1.0d0/6.0d0
   61       REAL,  PARAMETER     :: ONETHIRD  = 1.0d0/3.0d0
   62       REAL,  PARAMETER     :: ZERO      = 0.0d0
   63                                                
   64       REAL,  PARAMETER     :: THR8      = TINY(1.0)
   65       REAL,  PARAMETER     :: THR       = TINY(1.0)
   66 !!/S      CALL STRACE (IENT, 'W3XXXX')
   67       CONTAINS
   68 !/ ------------------------------------------------------------------- /
   69       SUBROUTINE WAV_MY_WTIME(eTime)
   70 !/ ------------------------------------------------------------------- /
   71 !/
   72 !/                  +-----------------------------------+
   73 !/                  | WAVEWATCH III           NOAA/NCEP |
   74 !/                  |                                   |
   75 !/                  | Aron Roland (BGS IT&E GmbH)       |
   76 !/                  | Mathieu Dutour-Sikiric (IRB)      |
   77 !/                  |                                   |
   78 !/                  |                        FORTRAN 90 |
   79 !/                  | Last update :        01-June-2018 |
   80 !/                  +-----------------------------------+
   81 !/
   82 !/   01-June-2018 : Origination.                        ( version 6.04 )
   83 !/
   84 !  1. Purpose :
   85 !  2. Method :
   86 !  3. Parameters :
   87 !
   88 !     Parameter list
   89 !     ----------------------------------------------------------------
   90 !     ----------------------------------------------------------------
   91 !
   92 !  4. Subroutines used :
   93 !
   94 !      Name      Type  Module   Description
   95 !     ----------------------------------------------------------------
   96 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   97 !     ----------------------------------------------------------------
   98 !
   99 !  5. Called by :
  100 !
  101 !      Name      Type  Module   Description
  102 !     ----------------------------------------------------------------
  103 !     ----------------------------------------------------------------
  104 !
  105 !  6. Error messages :
  106 !  7. Remarks
  107 !  8. Structure :
  108 !  9. Switches :
  109 !
  110 !     !/S  Enable subroutine tracing.
 
 
                                                         w3parall.F90  page   3
 
 
  111 !
  112 ! 10. Source code :
  113 !
  114 !/ ------------------------------------------------------------------- /
  115 !/
  116 !/ ------------------------------------------------------------------- /
  117 !/ Parameter list
  118 !/
  119 !/ ------------------------------------------------------------------- /
  120 !/ Local parameters
  121 !/
  122        IMPLICIT NONE
  123       INTEGER mpimode
  124       REAL(8), intent(out) :: eTime
  125       mpimode=0
  126       IF (mpimode .eq. 0) THEN
  127         CALL CPU_TIME(eTime)
  128       END IF
  129 !/
  130 !/ End of JACOBI_INIT ------------------------------------------------ /
  131 !/
  132       END SUBROUTINE
  133 !/ ------------------------------------------------------------------- /
  134       SUBROUTINE PRINT_MY_TIME(string)
  135 !/
  136 !/                  +-----------------------------------+
  137 !/                  | WAVEWATCH III           NOAA/NCEP |
  138 !/                  |                                   |
  139 !/                  | Aron Roland (BGS IT&E GmbH)       |
  140 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  141 !/                  |                                   |
  142 !/                  |                        FORTRAN 90 |
  143 !/                  | Last update :        01-June-2018 |
  144 !/                  +-----------------------------------+
  145 !/
  146 !/   01-June-2018 : Origination.                        ( version 6.04 )
  147 !/
  148 !  1. Purpose : Print timings
  149 !  2. Method :
  150 !  3. Parameters :
  151 !
  152 !     Parameter list
  153 !     ----------------------------------------------------------------
  154 !     ----------------------------------------------------------------
  155 !
  156 !  4. Subroutines used :
  157 !
  158 !      Name      Type  Module   Description
  159 !     ----------------------------------------------------------------
  160 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  161 !     ----------------------------------------------------------------
  162 !
  163 !  5. Called by :
  164 !
  165 !      Name      Type  Module   Description
 
 
                                                         w3parall.F90  page   4
 
 
  166 !     ----------------------------------------------------------------
  167 !     ----------------------------------------------------------------
  168 !
  169 !  6. Error messages :
  170 !  7. Remarks
  171 !  8. Structure :
  172 !  9. Switches :
  173 !
  174 !     !/S  Enable subroutine tracing.
  175 !
  176 ! 10. Source code :
  177 !
  178 !/ ------------------------------------------------------------------- /
  179       USE W3ODATMD, ONLY : IAPROC
  180       IMPLICIT NONE
  181 !/
  182 !/ ------------------------------------------------------------------- /
  183 !/ Parameter list
  184 !/
  185 !/ ------------------------------------------------------------------- /
  186 !/ Local parameters
  187 !/
  188 !/
  189 !/ ------------------------------------------------------------------- /
  190 !
  191       character(*), intent(in) :: string
  192       REAL(8) :: eTime
  193       CALL WAV_MY_WTIME(eTime)
  194       WRITE(740+IAPROC,*) 'TIMING time=', eTime, ' at step ', string
  195 !/
  196 !/ End of JACOBI_INIT ------------------------------------------------ /
  197 !/
  198       END SUBROUTINE
  199 !/ ------------------------------------------------------------------- /
  200       SUBROUTINE PROP_REFRACTION_PR1(ISEA,DTG, CAD)
  201 !/
  202 !/                  +-----------------------------------+
  203 !/                  | WAVEWATCH III           NOAA/NCEP |
  204 !/                  |                                   |
  205 !/                  | Aron Roland (BGS IT&E GmbH)       |
  206 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  207 !/                  |                                   |
  208 !/                  |                        FORTRAN 90 |
  209 !/                  | Last update :        01-June-2018 |
  210 !/                  +-----------------------------------+
  211 !/
  212 !/   01-June-2018 : Origination.                        ( version 6.04 )
  213 !/
  214 !  1. Purpose : Compute refraction part in matrix
  215 !  2. Method :
  216 !  3. Parameters :
  217 !
  218 !     Parameter list
  219 !     ----------------------------------------------------------------
  220 !     ----------------------------------------------------------------
 
 
                                                         w3parall.F90  page   5
 
 
  221 !
  222 !  4. Subroutines used :
  223 !
  224 !      Name      Type  Module   Description
  225 !     ----------------------------------------------------------------
  226 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  227 !     ----------------------------------------------------------------
  228 !
  229 !  5. Called by :
  230 !
  231 !      Name      Type  Module   Description
  232 !     ----------------------------------------------------------------
  233 !     ----------------------------------------------------------------
  234 !
  235 !  6. Error messages :
  236 !  7. Remarks
  237 !  8. Structure :
  238 !  9. Switches :
  239 !
  240 !     !/S  Enable subroutine tracing.
  241 !
  242 ! 10. Source code :
  243 !
  244 !/ ------------------------------------------------------------------- /
  245      USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
  246                           EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
  247                           CTMAX, DMIN, DTH, CTHG0S, MAPSF
  248       USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, DDDX,   &
  249                           DDDY, DW
  250       USE W3IDATMD, ONLY: FLCUR
  251       USE W3ODATMD, only : IAPROC
  252       IMPLICIT NONE
  253 !/
  254 !/ ------------------------------------------------------------------- /
  255 !/ Parameter list
  256 !/
  257 !/ ------------------------------------------------------------------- /
  258 !/ Local parameters
  259 !/
  260 !/
  261 !/ ------------------------------------------------------------------- /
  262 !/
  263 !/
  264       REAL, intent(out) :: CAD(NSPEC)
  265       INTEGER, intent(in) :: ISEA
  266       REAL, intent(in) :: DTG
  267       INTEGER :: ISP, IK, ITH, IX, IY
  268       REAL :: FRK(NK), FRG(NK), DSDD(0:NK+1)
  269       REAL :: FACTH, DCXY, DCYX, DCXXYY, DTTST
  270       REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eDDDX, eDDDY, eCTHG0
  271       REAL :: VCFLT(NSPEC), DEPTH, FDG
  272       REAL :: FDDMAX
  273       IX=MAPSF(ISEA,1)
  274       IY=MAPSF(ISEA,2)
  275       eDDDX=DDDX(IY,IX)
 
 
                                                         w3parall.F90  page   6
 
 
  276       eDDDY=DDDY(IY,IX)
  277       eCTHG0 = CTHG0S(ISEA)
  278       FACTH  = DTG / DTH
  279       !
  280       FDG    = FACTH * eCTHG0
  281       DEPTH  = MAX ( DMIN , DW(ISEA) )
  282       DO IK=0, NK+1
  283         IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
  284           DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / D>
  285         ELSE
  286           DSDD(IK) = 0.
  287         END IF
  288       END DO
  289       FDDMAX=0
  290       DO ITH=1, NTH
  291         FDDMAX = MAX ( FDDMAX , ABS(ESIN(ITH)*eDDDX - ECOS(ITH)*eDDDY ) )
  292       END DO
  293       DO IK=1, NK
  294         FRK(IK) = FACTH * DSDD(IK) / WN(IK,ISEA)
  295         !FRK(IK) = FRK(IK) / MAX ( 1. , FRK(IK)*FDDMAX/CTMAX )
  296         FRG(IK) = FDG * CG(IK,ISEA)
  297       END DO
  298       DO ISP=1, NSPEC
  299         VCFLT(ISP) = FRG(MAPWN(ISP)) * ECOS(ISP) +                     &
  300            FRK(MAPWN(ISP)) * ( ESIN(ISP)*eDDDX - ECOS(ISP)*eDDDY )
  301       END DO
  302 !
  303       IF ( FLCUR ) THEN
  304         eDCXDX=DCXDX(IY,IX)
  305         eDCXDY=DCXDY(IY,IX)
  306         eDCYDX=DCYDX(IY,IX)
  307         eDCYDY=DCYDY(IY,IX)
  308         DCYX   = FACTH *   eDCYDX
  309         DCXXYY = FACTH * ( eDCXDX - eDCYDY )
  310         DCXY   = FACTH *   eDCXDY
  311         DO ISP=1, NSPEC
  312           VCFLT(ISP) = VCFLT(ISP) + ES2(ISP)*DCYX  + ESC(ISP)*DCXXYY - EC>
  313         END DO
  314       END IF
  315       DO ISP=1,NSPEC
  316         CAD(ISP)=DBLE(VCFLT(ISP))
  317       END DO
  318 !/
  319 !/ End of JACOBI_INIT ------------------------------------------------ /
  320 !/
  321       END SUBROUTINE
  322 !/ ------------------------------------------------------------------- /
  323 !
  324       SUBROUTINE PROP_REFRACTION_PR3(IP, ISEA, DTG, CAD, DoLimiter)
  325 !/
  326 !/                  +-----------------------------------+
  327 !/                  | WAVEWATCH III           NOAA/NCEP |
  328 !/                  |                                   |
  329 !/                  | Aron Roland (BGS IT&E GmbH)       |
  330 !/                  | Mathieu Dutour-Sikiric (IRB)      |
 
 
                                                         w3parall.F90  page   7
 
 
  331 !/                  |                                   |
  332 !/                  |                        FORTRAN 90 |
  333 !/                  | Last update :        01-June-2018 |
  334 !/                  +-----------------------------------+
  335 !/
  336 !/   01-June-2018 : Origination.                        ( version 6.04 )
  337 !/
  338 !  1. Purpose : Compute refraction part in matrix alternative approach
  339 !  2. Method :
  340 !  3. Parameters :
  341 !
  342 !     Parameter list
  343 !     ----------------------------------------------------------------
  344 !     ----------------------------------------------------------------
  345 !
  346 !  4. Subroutines used :
  347 !
  348 !      Name      Type  Module   Description
  349 !     ----------------------------------------------------------------
  350 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  351 !     ----------------------------------------------------------------
  352 !
  353 !  5. Called by :
  354 !
  355 !      Name      Type  Module   Description
  356 !     ----------------------------------------------------------------
  357 !     ----------------------------------------------------------------
  358 !
  359 !  6. Error messages :
  360 !  7. Remarks
  361 !  8. Structure :
  362 !  9. Switches :
  363 !
  364 !     !/S  Enable subroutine tracing.
  365 !
  366 ! 10. Source code :
  367 !
  368 !/ ------------------------------------------------------------------- /
  369       USE CONSTANTS, ONLY : LPDLIB
  370       USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
  371                           EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
  372                           CTMAX, DMIN, DTH, CTHG0S, MAPSF
  373       USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, DDDX,   &
  374                           DDDY, DW
  375       USE W3IDATMD, ONLY: FLCUR
  376       USE W3ODATMD, only : IAPROC
  377       IMPLICIT NONE
  378 !/
  379 !/ ------------------------------------------------------------------- /
  380 !/ Parameter list
  381 !/
  382 !/ ------------------------------------------------------------------- /
  383 !/ Local parameters
  384 !/
  385       REAL, intent(out) :: CAD(NSPEC)
 
 
                                                         w3parall.F90  page   8
 
 
  386       INTEGER, intent(in) :: ISEA, IP
  387       REAL, intent(in) :: DTG
  388       logical, intent(in) :: DoLimiter
  389       INTEGER :: ISP, IK, ITH, IX, IY
  390       REAL :: FRK(NK), FRG(NK), DSDD(0:NK+1)
  391       REAL :: FACTH, DCXY, DCYX, DCXXYY, DTTST
  392       REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eDDDX, eDDDY, eCTHG0
  393       REAL :: VCFLT(NSPEC), DEPTH, FDG
  394       REAL :: FDDMAX, CFLTHMAX, VELNOFILT, CTMAX_eff
  395       IX=MAPSF(ISEA,1)
  396       IY=MAPSF(ISEA,2)
  397       IF (LPDLIB) THEN
  398         eDDDX=DDDX(1,IP)
  399         eDDDY=DDDY(1,IP)
  400       ELSE
  401         eDDDX=DDDX(IY,IX)
  402         eDDDY=DDDY(IY,IX)
  403       ENDIF
  404       eCTHG0 = CTHG0S(ISEA)
  405       FACTH  = 1.0 / DTH
  406       !
  407       FDG    = FACTH * eCTHG0
  408       DEPTH  = MAX ( DMIN , DW(ISEA) )
  409       DO IK=0, NK+1
  410         IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
  411           DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / D>
  412         ELSE
  413           DSDD(IK) = 0.
  414         END IF
  415       END DO
  416       DO IK=1, NK
  417         FRK(IK) = FACTH * DSDD(IK) / WN(IK,ISEA)
  418         FRG(IK) = FDG * CG(IK,ISEA)
  419       END DO
  420       IF (FLCUR) THEN
  421         IF (LPDLIB) THEN
  422           eDCXDX = DCXDX(1,IP)
  423           eDCXDY = DCXDY(1,IP)
  424           eDCYDX = DCYDX(1,IP)
  425           eDCYDY = DCYDY(1,IP)
  426         ELSE
  427           eDCXDX = DCXDX(IY,IX)
  428           eDCXDY = DCXDY(IY,IX)
  429           eDCYDX = DCYDX(IY,IX)
  430           eDCYDY = DCYDY(IY,IX)
  431         ENDIF
  432         DCYX   = FACTH *   eDCYDX
  433         DCXXYY = FACTH * ( eDCXDX - eDCYDY )
  434         DCXY   = FACTH *   eDCXDY
  435         DO ISP=1, NSPEC
  436           VCFLT(ISP) = ES2(ISP)*DCYX  + ESC(ISP)*DCXXYY - EC2(ISP)*DCXY
  437         END DO
  438       ELSE
  439         VCFLT=0
  440       END IF
 
 
                                                         w3parall.F90  page   9
 
 
  441 !
  442       CTMAX_eff=CTMAX/DTG
  443       DO ISP=1, NSPEC
  444         VELNOFILT = VCFLT(ISP)                                       &
  445            + FRG(MAPWN(ISP)) * ECOS(ISP)                             &
  446            + FRK(MAPWN(ISP)) * (ESIN(ISP)*eDDDX - ECOS(ISP)*eDDDY)
  447 !
  448 ! Puts filtering on total velocity (including currents and great circle e>
  449 ! the filtering limits VCFLT to be less than CTMAX
  450 ! this modification was proposed by F. Ardhuin 2011/03/06
  451 !
  452         IF (DoLimiter .eqv. .TRUE.) THEN
  453           VCFLT(ISP)=SIGN(MIN(ABS(VELNOFILT),CTMAX_eff),VELNOFILT)
  454         ELSE
  455           VCFLT(ISP)=VELNOFILT
  456         END IF
  457       END DO
  458       DO ISP=1,NSPEC
  459         CAD(ISP)=DBLE(VCFLT(ISP))
  460       END DO
  461 !/
  462 !/ End of JACOBI_INIT ------------------------------------------------ /
  463 !/
  464       END SUBROUTINE
  465 !/ ------------------------------------------------------------------- /
  466       SUBROUTINE PROP_FREQ_SHIFT(IP, ISEA, CAS, DMM, DTG)
  467 !/
  468 !/                  +-----------------------------------+
  469 !/                  | WAVEWATCH III           NOAA/NCEP |
  470 !/                  |                                   |
  471 !/                  | Aron Roland (BGS IT&E GmbH)       |
  472 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  473 !/                  |                                   |
  474 !/                  |                        FORTRAN 90 |
  475 !/                  | Last update :        01-June-2018 |
  476 !/                  +-----------------------------------+
  477 !/
  478 !/   01-June-2018 : Origination.                        ( version 6.04 )
  479 !/
  480 !  1. Purpose : Compute freq. shift in matrix
  481 !  2. Method :
  482 !  3. Parameters :
  483 !
  484 !     Parameter list
  485 !     ----------------------------------------------------------------
  486 !     ----------------------------------------------------------------
  487 !
  488 !  4. Subroutines used :
  489 !
  490 !      Name      Type  Module   Description
  491 !     ----------------------------------------------------------------
  492 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  493 !     ----------------------------------------------------------------
  494 !
  495 !  5. Called by :
 
 
                                                         w3parall.F90  page  10
 
 
  496 !
  497 !      Name      Type  Module   Description
  498 !     ----------------------------------------------------------------
  499 !     ----------------------------------------------------------------
  500 !
  501 !  6. Error messages :
  502 !  7. Remarks
  503 !  8. Structure :
  504 !  9. Switches :
  505 !
  506 !     !/S  Enable subroutine tracing.
  507 !
  508 ! 10. Source code :
  509 !
  510 !/ ------------------------------------------------------------------- /
  511       USE CONSTANTS, ONLY : LPDLIB
  512       USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
  513                           EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
  514                           CTMAX, DMIN, DTH, MAPSF
  515       USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, CX, CY, DDD>
  516       USE W3ODATMD, only : IAPROC
  517       IMPLICIT NONE
  518 !/ Parameter list
  519 !/
  520 !/ ------------------------------------------------------------------- /
  521 !/ Local parameters
  522 !/
  523       INTEGER, intent(in) :: ISEA, IP
  524       REAL, intent(out) :: DMM(0:NK2)
  525       REAL, intent(in) :: DTG
  526       REAL, intent(out) :: CAS(NSPEC)
  527       REAL :: DB(NK2), DSDD(0:NK+1)
  528       REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eCX, eCY, eDDDX, EDDDY
  529       REAL :: DCXX, DCXYYX, DCYY, FKD, FACK
  530       REAL :: VELNOFILT, VELFAC, DEPTH
  531       REAL :: CFLK(NK2,NTH), FKC(NTH), FKD0
  532       INTEGER :: IK, ITH, ISP, IY, IX
  533 !
  534       IF (LPDLIB) THEN
  535         eDCXDX = DCXDX(1,IP)
  536         eDCXDY = DCXDY(1,IP)
  537         eDCYDX = DCYDX(1,IP)
  538         eDCYDY = DCYDY(1,IP)
  539         eDDDX  = DDDX(1,IP)
  540         eDDDY  = DDDY(1,IP)
  541       ELSE
  542         IX=MAPSF(ISEA,1)
  543         IY=MAPSF(ISEA,2)
  544         eDCXDX=DCXDX(IY,IX)
  545         eDCXDY=DCXDY(IY,IX)
  546         eDCYDX=DCYDX(IY,IX)
  547         eDCYDY=DCYDY(IY,IX)
  548         eDDDX=DDDX(IY,IX)
  549         eDDDY=DDDY(IY,IX)
  550       ENDIF
 
 
                                                         w3parall.F90  page  11
 
 
  551       eCX=CX(ISEA)
  552       eCY=CY(ISEA)
  553       DCXX   =  -   eDCXDX
  554       DCXYYX =  - ( eDCXDY + eDCYDX )
  555       DCYY   =  -   eDCYDY
  556       FKD    =    ( eCX*eDDDX + eCY*eDDDY )
  557       FACK = DTG
  558       DO ITH=1, NTH
  559         FKC(ITH) = EC2(ITH)*DCXX + ESC(ITH)*DCXYYX + ES2(ITH)*DCYY
  560       END DO
  561       DO IK=0, NK
  562         DB(IK+1) = DSIP(IK) / CG(IK,ISEA)
  563         DMM(IK+1) = DBLE(WN(IK+1,ISEA) - WN(IK,ISEA))
  564       END DO
  565       DB(NK+2) = DSIP(NK+1) / CG(NK+1,ISEA)
  566       DMM(NK+2) = ZERO
  567       DMM(0)=DMM(1)
  568 !
  569       DEPTH  = MAX ( DMIN , DW(ISEA) )
  570       DO IK=0, NK+1
  571         IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
  572           DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / D>
  573         ELSE
  574           DSDD(IK) = 0.
  575         END IF
  576       END DO
  577       DO IK=0, NK+1
  578         FKD0   = FKD / CG(IK,ISEA) * DSDD(IK)
  579         VELFAC =  FACK/DB(IK+1)
  580         DO ITH=1, NTH
  581           VELNOFILT = ( FKD0 + WN(IK,ISEA)*FKC(ITH) ) * VELFAC
  582           CFLK(IK+1,ITH) = VELNOFILT/VELFAC
  583         END DO
  584       END DO
  585       DO IK=1,NK
  586         DO ITH=1,NTH
  587           ISP=ITH + (IK-1)*NTH
  588           CAS(ISP)=DBLE(CFLK(IK,ITH))
  589         END DO
  590       END DO
  591 !/
  592 !/ End of JACOBI_INIT ------------------------------------------------ /
  593 !/
  594       END SUBROUTINE
  595 !/ ------------------------------------------------------------------- /
  596       SUBROUTINE PROP_FREQ_SHIFT_M2(IP, ISEA, CWNB_M2, DWNI_M2, DTG)
  597 !/
  598 !/                  +-----------------------------------+
  599 !/                  | WAVEWATCH III           NOAA/NCEP |
  600 !/                  |                                   |
  601 !/                  | Aron Roland (BGS IT&E GmbH)       |
  602 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  603 !/                  |                                   |
  604 !/                  |                        FORTRAN 90 |
  605 !/                  | Last update :        01-June-2018 |
 
 
                                                         w3parall.F90  page  12
 
 
  606 !/                  +-----------------------------------+
  607 !/
  608 !/   01-June-2018 : Origination.                        ( version 6.04 )
  609 !/
  610 !  1. Purpose : Compute freq. shift alternative approach
  611 !  2. Method :
  612 !  3. Parameters :
  613 !
  614 !     Parameter list
  615 !     ----------------------------------------------------------------
  616 !     ----------------------------------------------------------------
  617 !
  618 !  4. Subroutines used :
  619 !
  620 !      Name      Type  Module   Description
  621 !     ----------------------------------------------------------------
  622 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  623 !     ----------------------------------------------------------------
  624 !
  625 !  5. Called by :
  626 !
  627 !      Name      Type  Module   Description
  628 !     ----------------------------------------------------------------
  629 !     ----------------------------------------------------------------
  630 !
  631 !  6. Error messages :
  632 !  7. Remarks
  633 !  8. Structure :
  634 !  9. Switches :
  635 !
  636 !     !/S  Enable subroutine tracing.
  637 !
  638 ! 10. Source code :
  639 !
  640 !/ ------------------------------------------------------------------- /
  641 !
  642       USE CONSTANTS, ONLY : LPDLIB
  643       USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
  644                           EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
  645                           CTMAX, DMIN, DTH, MAPSF
  646       USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, CX, CY, DDD>
  647       USE W3ODATMD, only : IAPROC
  648                                  
  649       IMPLICIT NONE
  650                    
  651 !/ ------------------------------------------------------------------- /
  652 !/ Local parameters
  653 !/
  654   
  655       INTEGER, intent(in) :: ISEA, IP
  656       REAL, intent(out) :: CWNB_M2(1-NTH:NSPEC)
  657       REAL, intent(out) :: DWNI_M2(NK)
  658       REAL, intent(in) :: DTG
  659       !
  660       REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eCX, eCY, eDDDX, EDDDY
 
 
                                                         w3parall.F90  page  13
 
 
  661       REAL :: DCXX, DCXYYX, DCYY, FKD, FACK
  662       REAL :: DEPTH
  663       REAL :: FKC(NTH), FKD0
  664       REAL :: VCWN(1-NTH:NSPEC+NTH)
  665       REAL :: DSDD(0:NK+1)
  666       REAL :: sumDiff, sumDiff1, sumDiff2, sumDiff3
  667       REAL :: sumDiff0, sumDiff4, sumDiff5
  668       INTEGER :: IK, ITH, ISP, IY, IX
  669                                      
  670 !/ ------------------------------------------------------------------- /
  671                                                                         
  672                                                                         
  673       IF (LPDLIB) THEN
  674         eDCXDX = DCXDX(1,IP)
  675         eDCXDY = DCXDY(1,IP)
  676         eDCYDX = DCYDX(1,IP)
  677         eDCYDY = DCYDY(1,IP)
  678         eDDDX  = DDDX(1,IP)
  679         eDDDY  = DDDY(1,IP)
  680       ELSE
  681         IX=MAPSF(ISEA,1)
  682         IY=MAPSF(ISEA,2)
  683         eDCXDX=DCXDX(IY,IX)
  684         eDCXDY=DCXDY(IY,IX)
  685         eDCYDX=DCYDX(IY,IX)
  686         eDCYDY=DCYDY(IY,IX)
  687         eDDDX=DDDX(IY,IX)
  688         eDDDY=DDDY(IY,IX)
  689       ENDIF
  690            
  691       eCX = CX(ISEA)
  692       eCY = CY(ISEA)
  693       FACK = DTG
  694       DCXX   =  - FACK *   eDCXDX
  695       DCXYYX =  - FACK * ( eDCXDY + eDCYDX )
  696       DCYY   =  - FACK *   eDCYDY
  697       FKD    =    FACK * ( eCX*eDDDX + eCY*eDDDY )
  698                                                   
  699       DO ITH=1, NTH
  700         FKC(ITH) = EC2(ITH)*DCXX + ESC(ITH)*DCXYYX + ES2(ITH)*DCYY
  701       END DO
  702 !
  703       DEPTH  = MAX ( DMIN , DW(ISEA) )
  704       DO IK=0, NK+1
  705         IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
  706           DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / D>
  707         ELSE
  708           DSDD(IK) = 0.
  709         END IF
  710       END DO
  711       ISP = -NTH
  712       DO IK=0, NK+1
  713         FKD0   = FKD / CG(IK,ISEA) * DSDD(IK)
  714         DO ITH=1, NTH
  715           ISP = ISP + 1
 
 
                                                         w3parall.F90  page  14
 
 
  716           VCWN(ISP) = FKD0 + WN(IK,ISEA)*FKC(ITH)
  717         END DO
  718       END DO
  719             
  720       sumDiff=0
  721       DO ISP=1-NTH,NSPEC
  722         CWNB_M2(ISP) = DBLE(0.5 * ( VCWN(ISP) + VCWN(ISP+NTH) ))
  723         sumDiff = sumDiff + MAX(CWNB_M2(ISP), ZERO)
  724       END DO
  725       DO IK=1,NK
  726         DWNI_M2(IK) = DBLE( CG(IK,ISEA) / DSIP(IK) )
  727       END DO
  728 !/
  729 !/ End of JACOBI_INIT ------------------------------------------------ /
  730 !/
  731       END SUBROUTINE
  732 !/ ------------------------------------------------------------------- /
  733       SUBROUTINE SYNCHRONIZE_IPGL_ETC_ARRAY(IMOD, IsMulti)
  734 !/
  735 !/                  +-----------------------------------+
  736 !/                  | WAVEWATCH III           NOAA/NCEP |
  737 !/                  |                                   |
  738 !/                  | Aron Roland (BGS IT&E GmbH)       |
  739 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  740 !/                  |                                   |
  741 !/                  |                        FORTRAN 90 |
  742 !/                  | Last update :        01-June-2018 |
  743 !/                  +-----------------------------------+
  744 !/
  745 !/   01-June-2018 : Origination.                        ( version 6.04 )
  746 !/
  747 !  1. Purpose : Sync global local arrays
  748 !  2. Method :
  749 ! 		All the process need to have IPGL_tot and IPGL_TO_PROC
  750 ! 		This is especially the case for the output process.
  751 ! 		So we need some painful exportation business
  752 !  3. Parameters :
  753 !
  754 !     Parameter list
  755 !     ----------------------------------------------------------------
  756 !     ----------------------------------------------------------------
  757 !
  758 !  4. Subroutines used :
  759 !
  760 !      Name      Type  Module   Description
  761 !     ----------------------------------------------------------------
  762 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  763 !     ----------------------------------------------------------------
  764 !
  765 !  5. Called by :
  766 !
  767 !      Name      Type  Module   Description
  768 !     ----------------------------------------------------------------
  769 !     ----------------------------------------------------------------
  770 !
 
 
                                                         w3parall.F90  page  15
 
 
  771 !  6. Error messages :
  772 !  7. Remarks
  773 !  8. Structure :
  774 !  9. Switches :
  775 !
  776 !     !/S  Enable subroutine tracing.
  777 !
  778 ! 10. Source code :
  779 !
  780 !/ ------------------------------------------------------------------- /
  781           IMPLICIT NONE
  782           INTEGER, intent(in) :: IMOD
  783           logical, intent(in) :: IsMulti
  784                                         
  785                                         
  786 !/
  787 !/ End of JACOBI_INIT ------------------------------------------------ /
  788 !/
  789       END SUBROUTINE
  790 !/ ....................----------------------------------------------- /
  791       SUBROUTINE SET_UP_NSEAL_NSEALM(NSEALout, NSEALMout)
  792 !/
  793 !/                  +-----------------------------------+
  794 !/                  | WAVEWATCH III           NOAA/NCEP |
  795 !/                  |                                   |
  796 !/                  | Aron Roland (BGS IT&E GmbH)       |
  797 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  798 !/                  |                                   |
  799 !/                  |                        FORTRAN 90 |
  800 !/                  | Last update :        01-June-2018 |
  801 !/                  +-----------------------------------+
  802 !/
  803 !/   01-June-2018 : Origination.                        ( version 6.04 )
  804 !/
  805 !  1. Purpose : Setup nseal, nsealm in contect of pdlib
  806 !  2. Method :
  807 !  3. Parameters :
  808 !
  809 !     Parameter list
  810 !     ----------------------------------------------------------------
  811 !     ----------------------------------------------------------------
  812 !
  813 !  4. Subroutines used :
  814 !
  815 !      Name      Type  Module   Description
  816 !     ----------------------------------------------------------------
  817 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  818 !     ----------------------------------------------------------------
  819 !
  820 !  5. Called by :
  821 !
  822 !      Name      Type  Module   Description
  823 !     ----------------------------------------------------------------
  824 !     ----------------------------------------------------------------
  825 !
 
 
                                                         w3parall.F90  page  16
 
 
  826 !  6. Error messages :
  827 !  7. Remarks
  828 !  8. Structure :
  829 !  9. Switches :
  830 !
  831 !     !/S  Enable subroutine tracing.
  832 !
  833 ! 10. Source code :
  834 !
  835 !/ ------------------------------------------------------------------- /
  836 !/
  837 !/ ------------------------------------------------------------------- /
  838       USE CONSTANTS, ONLY : LPDLIB
  839       USE W3GDATMD, ONLY: NSEA
  840       USE W3ODATMD, ONLY: NTPROC, NAPROC, IAPROC
  841       IMPLICIT NONE
  842       INTEGER, intent(out) :: NSEALout, NSEALMout
  843 !/ Local parameters
  844 !/
  845 !/
  846 !/ ------------------------------------------------------------------- /
  847 !/
  848 !!/PDLIB      WRITE(*,*) 'LPDLIB=', LPDLIB
  849 !!/PDLIB      WRITE(*,*) 'GTYPE=', GTYPE, ' UNGTYPE=', UNGTYPE
  850                                                               
  851       NSEALout  = NSEA
  852       NSEALMout = NSEA
  853 !
  854 !/
  855 !/ End of JACOBI_INIT ------------------------------------------------ /
  856 !/
  857       END SUBROUTINE
  858 !/ ------------------------------------------------------------------- /
  859       SUBROUTINE INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
  860 !/ ------------------------------------------------------------------- /
  861 !/
  862 !/                  +-----------------------------------+
  863 !/                  | WAVEWATCH III           NOAA/NCEP |
  864 !/                  |                                   |
  865 !/                  | Aron Roland (BGS IT&E GmbH)       |
  866 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  867 !/                  |                                   |
  868 !/                  |                        FORTRAN 90 |
  869 !/                  | Last update :        01-June-2018 |
  870 !/                  +-----------------------------------+
  871 !/
  872 !/   01-June-2018 : Origination.                        ( version 6.04 )
  873 !/
  874 !  1. Purpose : Set Jsea for all schemes
  875 !  2. Method :
  876 !  3. Parameters :
  877 !
  878 !     Parameter list
  879 !     ----------------------------------------------------------------
  880 !     ----------------------------------------------------------------
 
 
                                                         w3parall.F90  page  17
 
 
  881 !
  882 !  4. Subroutines used :
  883 !
  884 !      Name      Type  Module   Description
  885 !     ----------------------------------------------------------------
  886 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  887 !     ----------------------------------------------------------------
  888 !
  889 !  5. Called by :
  890 !
  891 !      Name      Type  Module   Description
  892 !     ----------------------------------------------------------------
  893 !     ----------------------------------------------------------------
  894 !
  895 !  6. Error messages :
  896 !  7. Remarks
  897 !  8. Structure :
  898 !  9. Switches :
  899 !
  900 !     !/S  Enable subroutine tracing.
  901 !
  902 ! 10. Source code :
  903 !
  904 !/ ------------------------------------------------------------------- /
  905 !/
  906       USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
  907       USE W3GDATMD, ONLY: GTYPE, UNGTYPE, MAPSF
  908       USE CONSTANTS, ONLY : LPDLIB
  909       IMPLICIT NONE
  910 !/ ------------------------------------------------------------------- /
  911 !/ Parameter list
  912 !/
  913 !/ ------------------------------------------------------------------- /
  914 !/ Local parameters
  915 !/
  916 !/
  917 !/ ------------------------------------------------------------------- /
  918       INTEGER, intent(in) :: ISEA
  919       INTEGER, intent(out) :: JSEA, ISPROC
  920       INTEGER IP_glob
  921 !!/DEBUG      WRITE(740+IAPROC,*) 'PDLIB=', PDLIB
  922 !!/DEBUG      WRITE(740+IAPROC,*) 'GTYPE=', GTYPE, ' UNGTYPE=', UNGTYPE
  923 !!/DEBUG      FLUSH(740+IAPROC)
  924       IF (.NOT. LPDLIB) THEN
  925         JSEA   = 1 + (ISEA-1)/NAPROC
  926         ISPROC = ISEA - (JSEA-1)*NAPROC
  927       ELSE
  928       ENDIF
  929 !/
  930 !/ End of JACOBI_INIT ------------------------------------------------ /
  931 !/
  932       END SUBROUTINE
  933 !/ ------------------------------------------------------------------- /
  934       SUBROUTINE GET_JSEA_IBELONG(ISEA, JSEA, IBELONG)
  935 !/ ------------------------------------------------------------------- /
 
 
                                                         w3parall.F90  page  18
 
 
  936 !/
  937 !/                  +-----------------------------------+
  938 !/                  | WAVEWATCH III           NOAA/NCEP |
  939 !/                  |                                   |
  940 !/                  | Aron Roland (BGS IT&E GmbH)       |
  941 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  942 !/                  |                                   |
  943 !/                  |                        FORTRAN 90 |
  944 !/                  | Last update :        01-June-2018 |
  945 !/                  +-----------------------------------+
  946 !/
  947 !/   01-June-2018 : Origination.                        ( version 6.04 )
  948 !/
  949 !  1. Purpose : Set belongings of jsea in context of pdlib
  950 !  2. Method :
  951 !  3. Parameters :
  952 !
  953 !     Parameter list
  954 !     ----------------------------------------------------------------
  955 !     ----------------------------------------------------------------
  956 !
  957 !  4. Subroutines used :
  958 !
  959 !      Name      Type  Module   Description
  960 !     ----------------------------------------------------------------
  961 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  962 !     ----------------------------------------------------------------
  963 !
  964 !  5. Called by :
  965 !
  966 !      Name      Type  Module   Description
  967 !     ----------------------------------------------------------------
  968 !     ----------------------------------------------------------------
  969 !
  970 !  6. Error messages :
  971 !  7. Remarks
  972 !  8. Structure :
  973 !  9. Switches :
  974 !
  975 !     !/S  Enable subroutine tracing.
  976 !
  977 ! 10. Source code :
  978 !
  979 !/ ------------------------------------------------------------------- /
  980 !/
  981       USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
  982       USE W3GDATMD, ONLY: GTYPE, UNGTYPE, MAPSF
  983       USE CONSTANTS, ONLY : LPDLIB
  984       IMPLICIT NONE
  985 !/ ------------------------------------------------------------------- /
  986 !/ Parameter list
  987 !/
  988 !/ ------------------------------------------------------------------- /
  989 !/ Local parameters
  990 !/
 
 
                                                         w3parall.F90  page  19
 
 
  991 !/
  992 !/ ------------------------------------------------------------------- /
  993 !/
  994       INTEGER, intent(in) :: ISEA
  995       INTEGER, intent(out) :: JSEA, IBELONG
  996       INTEGER ISPROC, IX, JX
  997       IF (.NOT. LPDLIB) THEN
  998         JSEA   = 1 + (ISEA-1)/NAPROC
  999         ISPROC = ISEA - (JSEA-1)*NAPROC
 1000         IF (ISPROC .eq. IAPROC) THEN
 1001           IBELONG=1
 1002         ELSE
 1003           IBELONG=0
 1004         END IF
 1005       ELSE
 1006       ENDIF
 1007 !/
 1008 !/ End of INIT_GET_ISEA ---------------------------------------------- /
 1009 !/
 1010       END SUBROUTINE
 1011 !/ ------------------------------------------------------------------- /
 1012       SUBROUTINE INIT_GET_ISEA(ISEA, JSEA)
 1013 !/ ------------------------------------------------------------------- /
 1014 !/
 1015 !/                  +-----------------------------------+
 1016 !/                  | WAVEWATCH III           NOAA/NCEP |
 1017 !/                  |                                   |
 1018 !/                  | Aron Roland (BGS IT&E GmbH)       |
 1019 !/                  | Mathieu Dutour-Sikiric (IRB)      |
 1020 !/                  |                                   |
 1021 !/                  |                        FORTRAN 90 |
 1022 !/                  | Last update :        01-June-2018 |
 1023 !/                  +-----------------------------------+
 1024 !/
 1025 !/   01-June-2018 : Origination.                        ( version 6.04 )
 1026 !/
 1027 !  1. Purpose : Set Isea for all schemes
 1028 !  2. Method :
 1029 !  3. Parameters :
 1030 !
 1031 !     Parameter list
 1032 !     ----------------------------------------------------------------
 1033 !     ----------------------------------------------------------------
 1034 !
 1035 !  4. Subroutines used :
 1036 !
 1037 !      Name      Type  Module   Description
 1038 !     ----------------------------------------------------------------
 1039 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1040 !     ----------------------------------------------------------------
 1041 !
 1042 !  5. Called by :
 1043 !
 1044 !      Name      Type  Module   Description
 1045 !     ----------------------------------------------------------------
 
 
                                                         w3parall.F90  page  20
 
 
 1046 !     ----------------------------------------------------------------
 1047 !
 1048 !  6. Error messages :
 1049 !  7. Remarks
 1050 !  8. Structure :
 1051 !  9. Switches :
 1052 !
 1053 !     !/S  Enable subroutine tracing.
 1054 !
 1055 ! 10. Source code :
 1056 !
 1057 !/ ------------------------------------------------------------------- /
 1058 !/
 1059       USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
 1060       USE W3GDATMD, ONLY: GTYPE, UNGTYPE
 1061       USE CONSTANTS, ONLY : LPDLIB
 1062 !/ ------------------------------------------------------------------- /
 1063 !/ Parameter list
 1064 !/
 1065 !/ ------------------------------------------------------------------- /
 1066 !/ Local parameters
 1067 !/
 1068 !/ ------------------------------------------------------------------- /
 1069 !/
 1070 !/
 1071 !/ ------------------------------------------------------------------- /
 1072 !/
 1073 !/ ------------------------------------------------------------------- /
 1074 !
 1075       USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
 1076       USE W3GDATMD, ONLY: GTYPE, UNGTYPE
 1077       USE CONSTANTS, ONLY : LPDLIB
 1078       IMPLICIT NONE
 1079       INTEGER, intent(in) :: JSEA
 1080       INTEGER, intent(out) :: ISEA
 1081           ISEA         = JSEA
 1082 !/
 1083 !/ End of INIT_GET_ISEA ------------------------------------------------ /
 1084 !/
 1085       END SUBROUTINE
 1086 !**********************************************************************
 1087 !*  An array of size (NSEA) is send but only the (1:NSEAL) values     *
 1088 !*  are correct. The program synchonizes everything on all nodes.     *
 1089 !**********************************************************************
 1090       SUBROUTINE SYNCHRONIZE_GLOBAL_ARRAY(TheVar)
 1091 !/ ------------------------------------------------------------------- /
 1092 !/
 1093 !/                  +-----------------------------------+
 1094 !/                  | WAVEWATCH III           NOAA/NCEP |
 1095 !/                  |                                   |
 1096 !/                  | Aron Roland (BGS IT&E GmbH)       |
 1097 !/                  | Mathieu Dutour-Sikiric (IRB)      |
 1098 !/                  |                                   |
 1099 !/                  |                        FORTRAN 90 |
 1100 !/                  | Last update :        01-June-2018 |
 
 
                                                         w3parall.F90  page  21
 
 
 1101 !/                  +-----------------------------------+
 1102 !/
 1103 !/   01-June-2018 : Origination.                        ( version 6.04 )
 1104 !/
 1105 !  1. Purpose : Sync global array in context of pdlib
 1106 !  2. Method :
 1107 !			An array of size (NSEA) is send but only the (1:NSEAL) values
 1108 ! 			are correct. The program synchonizes everything on all nodes.
 1109 !  3. Parameters :
 1110 !
 1111 !     Parameter list
 1112 !     ----------------------------------------------------------------
 1113 !     ----------------------------------------------------------------
 1114 !
 1115 !  4. Subroutines used :
 1116 !
 1117 !      Name      Type  Module   Description
 1118 !     ----------------------------------------------------------------
 1119 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1120 !     ----------------------------------------------------------------
 1121 !
 1122 !  5. Called by :
 1123 !
 1124 !      Name      Type  Module   Description
 1125 !     ----------------------------------------------------------------
 1126 !     ----------------------------------------------------------------
 1127 !
 1128 !  6. Error messages :
 1129 !  7. Remarks
 1130 !  8. Structure :
 1131 !  9. Switches :
 1132 !
 1133 !     !/S  Enable subroutine tracing.
 1134 !
 1135 ! 10. Source code :
 1136 !
 1137 !/ ------------------------------------------------------------------- /
 1138 !
 1139       USE W3GDATMD, ONLY: NSEAL, NSEA, NX
 1140       IMPLICIT NONE
 1141 !/ ------------------------------------------------------------------- /
 1142 !/ Parameter list
 1143 !/
 1144 !/ ------------------------------------------------------------------- /
 1145 !/ Local parameters
 1146 !/
 1147 !/
 1148 !/ ------------------------------------------------------------------- /
 1149 !/
 1150       INTEGER ISEA, JSEA, Status(NX), rStatus(NX)
 1151       INTEGER IPROC, I, ierr, IP, IX, IP_glob
 1152       REAL*8, intent(inout) :: TheVar(NX)
 1153       REAL*8  rVect(NX)
 1154       Status=0
 1155 !/
 
 
                                                         w3parall.F90  page  22
 
 
 1156 !/ End of JACOBI_INIT ------------------------------------------------ /
 1157 !/
 1158       END SUBROUTINE
 1159 !/ ------------------------------------------------------------------- /
 1160       END MODULE W3PARALL
 1161 !/ ------------------------------------------------------------------- /
 1162                                                                         
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
