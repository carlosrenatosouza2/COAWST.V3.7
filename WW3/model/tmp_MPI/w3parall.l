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
  125       REAL(8) mpi_wtime
  126       mpimode=0
  127       mpimode=1
  128       eTime=mpi_wtime()
  129       IF (mpimode .eq. 0) THEN
  130         CALL CPU_TIME(eTime)
  131       END IF
  132 !/
  133 !/ End of JACOBI_INIT ------------------------------------------------ /
  134 !/
  135       END SUBROUTINE
  136 !/ ------------------------------------------------------------------- /
  137       SUBROUTINE PRINT_MY_TIME(string)
  138 !/
  139 !/                  +-----------------------------------+
  140 !/                  | WAVEWATCH III           NOAA/NCEP |
  141 !/                  |                                   |
  142 !/                  | Aron Roland (BGS IT&E GmbH)       |
  143 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  144 !/                  |                                   |
  145 !/                  |                        FORTRAN 90 |
  146 !/                  | Last update :        01-June-2018 |
  147 !/                  +-----------------------------------+
  148 !/
  149 !/   01-June-2018 : Origination.                        ( version 6.04 )
  150 !/
  151 !  1. Purpose : Print timings
  152 !  2. Method :
  153 !  3. Parameters :
  154 !
  155 !     Parameter list
  156 !     ----------------------------------------------------------------
  157 !     ----------------------------------------------------------------
  158 !
  159 !  4. Subroutines used :
  160 !
  161 !      Name      Type  Module   Description
  162 !     ----------------------------------------------------------------
  163 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  164 !     ----------------------------------------------------------------
  165 !
 
 
                                                         w3parall.F90  page   4
 
 
  166 !  5. Called by :
  167 !
  168 !      Name      Type  Module   Description
  169 !     ----------------------------------------------------------------
  170 !     ----------------------------------------------------------------
  171 !
  172 !  6. Error messages :
  173 !  7. Remarks
  174 !  8. Structure :
  175 !  9. Switches :
  176 !
  177 !     !/S  Enable subroutine tracing.
  178 !
  179 ! 10. Source code :
  180 !
  181 !/ ------------------------------------------------------------------- /
  182       USE W3ODATMD, ONLY : IAPROC
  183       IMPLICIT NONE
  184 !/
  185 !/ ------------------------------------------------------------------- /
  186 !/ Parameter list
  187 !/
  188 !/ ------------------------------------------------------------------- /
  189 !/ Local parameters
  190 !/
  191 !/
  192 !/ ------------------------------------------------------------------- /
  193 !
  194       character(*), intent(in) :: string
  195       REAL(8) :: eTime
  196       CALL WAV_MY_WTIME(eTime)
  197       WRITE(740+IAPROC,*) 'TIMING time=', eTime, ' at step ', string
  198 !/
  199 !/ End of JACOBI_INIT ------------------------------------------------ /
  200 !/
  201       END SUBROUTINE
  202 !/ ------------------------------------------------------------------- /
  203       SUBROUTINE PROP_REFRACTION_PR1(ISEA,DTG, CAD)
  204 !/
  205 !/                  +-----------------------------------+
  206 !/                  | WAVEWATCH III           NOAA/NCEP |
  207 !/                  |                                   |
  208 !/                  | Aron Roland (BGS IT&E GmbH)       |
  209 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  210 !/                  |                                   |
  211 !/                  |                        FORTRAN 90 |
  212 !/                  | Last update :        01-June-2018 |
  213 !/                  +-----------------------------------+
  214 !/
  215 !/   01-June-2018 : Origination.                        ( version 6.04 )
  216 !/
  217 !  1. Purpose : Compute refraction part in matrix
  218 !  2. Method :
  219 !  3. Parameters :
  220 !
 
 
                                                         w3parall.F90  page   5
 
 
  221 !     Parameter list
  222 !     ----------------------------------------------------------------
  223 !     ----------------------------------------------------------------
  224 !
  225 !  4. Subroutines used :
  226 !
  227 !      Name      Type  Module   Description
  228 !     ----------------------------------------------------------------
  229 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  230 !     ----------------------------------------------------------------
  231 !
  232 !  5. Called by :
  233 !
  234 !      Name      Type  Module   Description
  235 !     ----------------------------------------------------------------
  236 !     ----------------------------------------------------------------
  237 !
  238 !  6. Error messages :
  239 !  7. Remarks
  240 !  8. Structure :
  241 !  9. Switches :
  242 !
  243 !     !/S  Enable subroutine tracing.
  244 !
  245 ! 10. Source code :
  246 !
  247 !/ ------------------------------------------------------------------- /
  248      USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
  249                           EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
  250                           CTMAX, DMIN, DTH, CTHG0S, MAPSF
  251       USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, DDDX,   &
  252                           DDDY, DW
  253       USE W3IDATMD, ONLY: FLCUR
  254       USE W3ODATMD, only : IAPROC
  255       IMPLICIT NONE
  256 !/
  257 !/ ------------------------------------------------------------------- /
  258 !/ Parameter list
  259 !/
  260 !/ ------------------------------------------------------------------- /
  261 !/ Local parameters
  262 !/
  263 !/
  264 !/ ------------------------------------------------------------------- /
  265 !/
  266 !/
  267       REAL, intent(out) :: CAD(NSPEC)
  268       INTEGER, intent(in) :: ISEA
  269       REAL, intent(in) :: DTG
  270       INTEGER :: ISP, IK, ITH, IX, IY
  271       REAL :: FRK(NK), FRG(NK), DSDD(0:NK+1)
  272       REAL :: FACTH, DCXY, DCYX, DCXXYY, DTTST
  273       REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eDDDX, eDDDY, eCTHG0
  274       REAL :: VCFLT(NSPEC), DEPTH, FDG
  275       REAL :: FDDMAX
 
 
                                                         w3parall.F90  page   6
 
 
  276       IX=MAPSF(ISEA,1)
  277       IY=MAPSF(ISEA,2)
  278       eDDDX=DDDX(IY,IX)
  279       eDDDY=DDDY(IY,IX)
  280       eCTHG0 = CTHG0S(ISEA)
  281       FACTH  = DTG / DTH
  282       !
  283       FDG    = FACTH * eCTHG0
  284       DEPTH  = MAX ( DMIN , DW(ISEA) )
  285       DO IK=0, NK+1
  286         IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
  287           DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / D>
  288         ELSE
  289           DSDD(IK) = 0.
  290         END IF
  291       END DO
  292       FDDMAX=0
  293       DO ITH=1, NTH
  294         FDDMAX = MAX ( FDDMAX , ABS(ESIN(ITH)*eDDDX - ECOS(ITH)*eDDDY ) )
  295       END DO
  296       DO IK=1, NK
  297         FRK(IK) = FACTH * DSDD(IK) / WN(IK,ISEA)
  298         !FRK(IK) = FRK(IK) / MAX ( 1. , FRK(IK)*FDDMAX/CTMAX )
  299         FRG(IK) = FDG * CG(IK,ISEA)
  300       END DO
  301       DO ISP=1, NSPEC
  302         VCFLT(ISP) = FRG(MAPWN(ISP)) * ECOS(ISP) +                     &
  303            FRK(MAPWN(ISP)) * ( ESIN(ISP)*eDDDX - ECOS(ISP)*eDDDY )
  304       END DO
  305 !
  306       IF ( FLCUR ) THEN
  307         eDCXDX=DCXDX(IY,IX)
  308         eDCXDY=DCXDY(IY,IX)
  309         eDCYDX=DCYDX(IY,IX)
  310         eDCYDY=DCYDY(IY,IX)
  311         DCYX   = FACTH *   eDCYDX
  312         DCXXYY = FACTH * ( eDCXDX - eDCYDY )
  313         DCXY   = FACTH *   eDCXDY
  314         DO ISP=1, NSPEC
  315           VCFLT(ISP) = VCFLT(ISP) + ES2(ISP)*DCYX  + ESC(ISP)*DCXXYY - EC>
  316         END DO
  317       END IF
  318       DO ISP=1,NSPEC
  319         CAD(ISP)=DBLE(VCFLT(ISP))
  320       END DO
  321 !/
  322 !/ End of JACOBI_INIT ------------------------------------------------ /
  323 !/
  324       END SUBROUTINE
  325 !/ ------------------------------------------------------------------- /
  326 !
  327       SUBROUTINE PROP_REFRACTION_PR3(IP, ISEA, DTG, CAD, DoLimiter)
  328 !/
  329 !/                  +-----------------------------------+
  330 !/                  | WAVEWATCH III           NOAA/NCEP |
 
 
                                                         w3parall.F90  page   7
 
 
  331 !/                  |                                   |
  332 !/                  | Aron Roland (BGS IT&E GmbH)       |
  333 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  334 !/                  |                                   |
  335 !/                  |                        FORTRAN 90 |
  336 !/                  | Last update :        01-June-2018 |
  337 !/                  +-----------------------------------+
  338 !/
  339 !/   01-June-2018 : Origination.                        ( version 6.04 )
  340 !/
  341 !  1. Purpose : Compute refraction part in matrix alternative approach
  342 !  2. Method :
  343 !  3. Parameters :
  344 !
  345 !     Parameter list
  346 !     ----------------------------------------------------------------
  347 !     ----------------------------------------------------------------
  348 !
  349 !  4. Subroutines used :
  350 !
  351 !      Name      Type  Module   Description
  352 !     ----------------------------------------------------------------
  353 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  354 !     ----------------------------------------------------------------
  355 !
  356 !  5. Called by :
  357 !
  358 !      Name      Type  Module   Description
  359 !     ----------------------------------------------------------------
  360 !     ----------------------------------------------------------------
  361 !
  362 !  6. Error messages :
  363 !  7. Remarks
  364 !  8. Structure :
  365 !  9. Switches :
  366 !
  367 !     !/S  Enable subroutine tracing.
  368 !
  369 ! 10. Source code :
  370 !
  371 !/ ------------------------------------------------------------------- /
  372       USE CONSTANTS, ONLY : LPDLIB
  373       USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
  374                           EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
  375                           CTMAX, DMIN, DTH, CTHG0S, MAPSF
  376       USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, DDDX,   &
  377                           DDDY, DW
  378       USE W3IDATMD, ONLY: FLCUR
  379       USE W3ODATMD, only : IAPROC
  380       IMPLICIT NONE
  381 !/
  382 !/ ------------------------------------------------------------------- /
  383 !/ Parameter list
  384 !/
  385 !/ ------------------------------------------------------------------- /
 
 
                                                         w3parall.F90  page   8
 
 
  386 !/ Local parameters
  387 !/
  388       REAL, intent(out) :: CAD(NSPEC)
  389       INTEGER, intent(in) :: ISEA, IP
  390       REAL, intent(in) :: DTG
  391       logical, intent(in) :: DoLimiter
  392       INTEGER :: ISP, IK, ITH, IX, IY
  393       REAL :: FRK(NK), FRG(NK), DSDD(0:NK+1)
  394       REAL :: FACTH, DCXY, DCYX, DCXXYY, DTTST
  395       REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eDDDX, eDDDY, eCTHG0
  396       REAL :: VCFLT(NSPEC), DEPTH, FDG
  397       REAL :: FDDMAX, CFLTHMAX, VELNOFILT, CTMAX_eff
  398       IX=MAPSF(ISEA,1)
  399       IY=MAPSF(ISEA,2)
  400       IF (LPDLIB) THEN
  401         eDDDX=DDDX(1,IP)
  402         eDDDY=DDDY(1,IP)
  403       ELSE
  404         eDDDX=DDDX(IY,IX)
  405         eDDDY=DDDY(IY,IX)
  406       ENDIF
  407       eCTHG0 = CTHG0S(ISEA)
  408       FACTH  = 1.0 / DTH
  409       !
  410       FDG    = FACTH * eCTHG0
  411       DEPTH  = MAX ( DMIN , DW(ISEA) )
  412       DO IK=0, NK+1
  413         IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
  414           DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / D>
  415         ELSE
  416           DSDD(IK) = 0.
  417         END IF
  418       END DO
  419       DO IK=1, NK
  420         FRK(IK) = FACTH * DSDD(IK) / WN(IK,ISEA)
  421         FRG(IK) = FDG * CG(IK,ISEA)
  422       END DO
  423       IF (FLCUR) THEN
  424         IF (LPDLIB) THEN
  425           eDCXDX = DCXDX(1,IP)
  426           eDCXDY = DCXDY(1,IP)
  427           eDCYDX = DCYDX(1,IP)
  428           eDCYDY = DCYDY(1,IP)
  429         ELSE
  430           eDCXDX = DCXDX(IY,IX)
  431           eDCXDY = DCXDY(IY,IX)
  432           eDCYDX = DCYDX(IY,IX)
  433           eDCYDY = DCYDY(IY,IX)
  434         ENDIF
  435         DCYX   = FACTH *   eDCYDX
  436         DCXXYY = FACTH * ( eDCXDX - eDCYDY )
  437         DCXY   = FACTH *   eDCXDY
  438         DO ISP=1, NSPEC
  439           VCFLT(ISP) = ES2(ISP)*DCYX  + ESC(ISP)*DCXXYY - EC2(ISP)*DCXY
  440         END DO
 
 
                                                         w3parall.F90  page   9
 
 
  441       ELSE
  442         VCFLT=0
  443       END IF
  444 !
  445       CTMAX_eff=CTMAX/DTG
  446       DO ISP=1, NSPEC
  447         VELNOFILT = VCFLT(ISP)                                       &
  448            + FRG(MAPWN(ISP)) * ECOS(ISP)                             &
  449            + FRK(MAPWN(ISP)) * (ESIN(ISP)*eDDDX - ECOS(ISP)*eDDDY)
  450 !
  451 ! Puts filtering on total velocity (including currents and great circle e>
  452 ! the filtering limits VCFLT to be less than CTMAX
  453 ! this modification was proposed by F. Ardhuin 2011/03/06
  454 !
  455         IF (DoLimiter .eqv. .TRUE.) THEN
  456           VCFLT(ISP)=SIGN(MIN(ABS(VELNOFILT),CTMAX_eff),VELNOFILT)
  457         ELSE
  458           VCFLT(ISP)=VELNOFILT
  459         END IF
  460       END DO
  461       DO ISP=1,NSPEC
  462         CAD(ISP)=DBLE(VCFLT(ISP))
  463       END DO
  464 !/
  465 !/ End of JACOBI_INIT ------------------------------------------------ /
  466 !/
  467       END SUBROUTINE
  468 !/ ------------------------------------------------------------------- /
  469       SUBROUTINE PROP_FREQ_SHIFT(IP, ISEA, CAS, DMM, DTG)
  470 !/
  471 !/                  +-----------------------------------+
  472 !/                  | WAVEWATCH III           NOAA/NCEP |
  473 !/                  |                                   |
  474 !/                  | Aron Roland (BGS IT&E GmbH)       |
  475 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  476 !/                  |                                   |
  477 !/                  |                        FORTRAN 90 |
  478 !/                  | Last update :        01-June-2018 |
  479 !/                  +-----------------------------------+
  480 !/
  481 !/   01-June-2018 : Origination.                        ( version 6.04 )
  482 !/
  483 !  1. Purpose : Compute freq. shift in matrix
  484 !  2. Method :
  485 !  3. Parameters :
  486 !
  487 !     Parameter list
  488 !     ----------------------------------------------------------------
  489 !     ----------------------------------------------------------------
  490 !
  491 !  4. Subroutines used :
  492 !
  493 !      Name      Type  Module   Description
  494 !     ----------------------------------------------------------------
  495 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 
 
                                                         w3parall.F90  page  10
 
 
  496 !     ----------------------------------------------------------------
  497 !
  498 !  5. Called by :
  499 !
  500 !      Name      Type  Module   Description
  501 !     ----------------------------------------------------------------
  502 !     ----------------------------------------------------------------
  503 !
  504 !  6. Error messages :
  505 !  7. Remarks
  506 !  8. Structure :
  507 !  9. Switches :
  508 !
  509 !     !/S  Enable subroutine tracing.
  510 !
  511 ! 10. Source code :
  512 !
  513 !/ ------------------------------------------------------------------- /
  514       USE CONSTANTS, ONLY : LPDLIB
  515       USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
  516                           EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
  517                           CTMAX, DMIN, DTH, MAPSF
  518       USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, CX, CY, DDD>
  519       USE W3ODATMD, only : IAPROC
  520       IMPLICIT NONE
  521 !/ Parameter list
  522 !/
  523 !/ ------------------------------------------------------------------- /
  524 !/ Local parameters
  525 !/
  526       INTEGER, intent(in) :: ISEA, IP
  527       REAL, intent(out) :: DMM(0:NK2)
  528       REAL, intent(in) :: DTG
  529       REAL, intent(out) :: CAS(NSPEC)
  530       REAL :: DB(NK2), DSDD(0:NK+1)
  531       REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eCX, eCY, eDDDX, EDDDY
  532       REAL :: DCXX, DCXYYX, DCYY, FKD, FACK
  533       REAL :: VELNOFILT, VELFAC, DEPTH
  534       REAL :: CFLK(NK2,NTH), FKC(NTH), FKD0
  535       INTEGER :: IK, ITH, ISP, IY, IX
  536 !
  537       IF (LPDLIB) THEN
  538         eDCXDX = DCXDX(1,IP)
  539         eDCXDY = DCXDY(1,IP)
  540         eDCYDX = DCYDX(1,IP)
  541         eDCYDY = DCYDY(1,IP)
  542         eDDDX  = DDDX(1,IP)
  543         eDDDY  = DDDY(1,IP)
  544       ELSE
  545         IX=MAPSF(ISEA,1)
  546         IY=MAPSF(ISEA,2)
  547         eDCXDX=DCXDX(IY,IX)
  548         eDCXDY=DCXDY(IY,IX)
  549         eDCYDX=DCYDX(IY,IX)
  550         eDCYDY=DCYDY(IY,IX)
 
 
                                                         w3parall.F90  page  11
 
 
  551         eDDDX=DDDX(IY,IX)
  552         eDDDY=DDDY(IY,IX)
  553       ENDIF
  554       eCX=CX(ISEA)
  555       eCY=CY(ISEA)
  556       DCXX   =  -   eDCXDX
  557       DCXYYX =  - ( eDCXDY + eDCYDX )
  558       DCYY   =  -   eDCYDY
  559       FKD    =    ( eCX*eDDDX + eCY*eDDDY )
  560       FACK = DTG
  561       DO ITH=1, NTH
  562         FKC(ITH) = EC2(ITH)*DCXX + ESC(ITH)*DCXYYX + ES2(ITH)*DCYY
  563       END DO
  564       DO IK=0, NK
  565         DB(IK+1) = DSIP(IK) / CG(IK,ISEA)
  566         DMM(IK+1) = DBLE(WN(IK+1,ISEA) - WN(IK,ISEA))
  567       END DO
  568       DB(NK+2) = DSIP(NK+1) / CG(NK+1,ISEA)
  569       DMM(NK+2) = ZERO
  570       DMM(0)=DMM(1)
  571 !
  572       DEPTH  = MAX ( DMIN , DW(ISEA) )
  573       DO IK=0, NK+1
  574         IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
  575           DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / D>
  576         ELSE
  577           DSDD(IK) = 0.
  578         END IF
  579       END DO
  580       DO IK=0, NK+1
  581         FKD0   = FKD / CG(IK,ISEA) * DSDD(IK)
  582         VELFAC =  FACK/DB(IK+1)
  583         DO ITH=1, NTH
  584           VELNOFILT = ( FKD0 + WN(IK,ISEA)*FKC(ITH) ) * VELFAC
  585           CFLK(IK+1,ITH) = VELNOFILT/VELFAC
  586         END DO
  587       END DO
  588       DO IK=1,NK
  589         DO ITH=1,NTH
  590           ISP=ITH + (IK-1)*NTH
  591           CAS(ISP)=DBLE(CFLK(IK,ITH))
  592         END DO
  593       END DO
  594 !/
  595 !/ End of JACOBI_INIT ------------------------------------------------ /
  596 !/
  597       END SUBROUTINE
  598 !/ ------------------------------------------------------------------- /
  599       SUBROUTINE PROP_FREQ_SHIFT_M2(IP, ISEA, CWNB_M2, DWNI_M2, DTG)
  600 !/
  601 !/                  +-----------------------------------+
  602 !/                  | WAVEWATCH III           NOAA/NCEP |
  603 !/                  |                                   |
  604 !/                  | Aron Roland (BGS IT&E GmbH)       |
  605 !/                  | Mathieu Dutour-Sikiric (IRB)      |
 
 
                                                         w3parall.F90  page  12
 
 
  606 !/                  |                                   |
  607 !/                  |                        FORTRAN 90 |
  608 !/                  | Last update :        01-June-2018 |
  609 !/                  +-----------------------------------+
  610 !/
  611 !/   01-June-2018 : Origination.                        ( version 6.04 )
  612 !/
  613 !  1. Purpose : Compute freq. shift alternative approach
  614 !  2. Method :
  615 !  3. Parameters :
  616 !
  617 !     Parameter list
  618 !     ----------------------------------------------------------------
  619 !     ----------------------------------------------------------------
  620 !
  621 !  4. Subroutines used :
  622 !
  623 !      Name      Type  Module   Description
  624 !     ----------------------------------------------------------------
  625 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  626 !     ----------------------------------------------------------------
  627 !
  628 !  5. Called by :
  629 !
  630 !      Name      Type  Module   Description
  631 !     ----------------------------------------------------------------
  632 !     ----------------------------------------------------------------
  633 !
  634 !  6. Error messages :
  635 !  7. Remarks
  636 !  8. Structure :
  637 !  9. Switches :
  638 !
  639 !     !/S  Enable subroutine tracing.
  640 !
  641 ! 10. Source code :
  642 !
  643 !/ ------------------------------------------------------------------- /
  644 !
  645       USE CONSTANTS, ONLY : LPDLIB
  646       USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
  647                           EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
  648                           CTMAX, DMIN, DTH, MAPSF
  649       USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, CX, CY, DDD>
  650       USE W3ODATMD, only : IAPROC
  651                                  
  652       IMPLICIT NONE
  653                    
  654 !/ ------------------------------------------------------------------- /
  655 !/ Local parameters
  656 !/
  657   
  658       INTEGER, intent(in) :: ISEA, IP
  659       REAL, intent(out) :: CWNB_M2(1-NTH:NSPEC)
  660       REAL, intent(out) :: DWNI_M2(NK)
 
 
                                                         w3parall.F90  page  13
 
 
  661       REAL, intent(in) :: DTG
  662       !
  663       REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eCX, eCY, eDDDX, EDDDY
  664       REAL :: DCXX, DCXYYX, DCYY, FKD, FACK
  665       REAL :: DEPTH
  666       REAL :: FKC(NTH), FKD0
  667       REAL :: VCWN(1-NTH:NSPEC+NTH)
  668       REAL :: DSDD(0:NK+1)
  669       REAL :: sumDiff, sumDiff1, sumDiff2, sumDiff3
  670       REAL :: sumDiff0, sumDiff4, sumDiff5
  671       INTEGER :: IK, ITH, ISP, IY, IX
  672                                      
  673 !/ ------------------------------------------------------------------- /
  674                                                                         
  675                                                                         
  676       IF (LPDLIB) THEN
  677         eDCXDX = DCXDX(1,IP)
  678         eDCXDY = DCXDY(1,IP)
  679         eDCYDX = DCYDX(1,IP)
  680         eDCYDY = DCYDY(1,IP)
  681         eDDDX  = DDDX(1,IP)
  682         eDDDY  = DDDY(1,IP)
  683       ELSE
  684         IX=MAPSF(ISEA,1)
  685         IY=MAPSF(ISEA,2)
  686         eDCXDX=DCXDX(IY,IX)
  687         eDCXDY=DCXDY(IY,IX)
  688         eDCYDX=DCYDX(IY,IX)
  689         eDCYDY=DCYDY(IY,IX)
  690         eDDDX=DDDX(IY,IX)
  691         eDDDY=DDDY(IY,IX)
  692       ENDIF
  693            
  694       eCX = CX(ISEA)
  695       eCY = CY(ISEA)
  696       FACK = DTG
  697       DCXX   =  - FACK *   eDCXDX
  698       DCXYYX =  - FACK * ( eDCXDY + eDCYDX )
  699       DCYY   =  - FACK *   eDCYDY
  700       FKD    =    FACK * ( eCX*eDDDX + eCY*eDDDY )
  701                                                   
  702       DO ITH=1, NTH
  703         FKC(ITH) = EC2(ITH)*DCXX + ESC(ITH)*DCXYYX + ES2(ITH)*DCYY
  704       END DO
  705 !
  706       DEPTH  = MAX ( DMIN , DW(ISEA) )
  707       DO IK=0, NK+1
  708         IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
  709           DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / D>
  710         ELSE
  711           DSDD(IK) = 0.
  712         END IF
  713       END DO
  714       ISP = -NTH
  715       DO IK=0, NK+1
 
 
                                                         w3parall.F90  page  14
 
 
  716         FKD0   = FKD / CG(IK,ISEA) * DSDD(IK)
  717         DO ITH=1, NTH
  718           ISP = ISP + 1
  719           VCWN(ISP) = FKD0 + WN(IK,ISEA)*FKC(ITH)
  720         END DO
  721       END DO
  722             
  723       sumDiff=0
  724       DO ISP=1-NTH,NSPEC
  725         CWNB_M2(ISP) = DBLE(0.5 * ( VCWN(ISP) + VCWN(ISP+NTH) ))
  726         sumDiff = sumDiff + MAX(CWNB_M2(ISP), ZERO)
  727       END DO
  728       DO IK=1,NK
  729         DWNI_M2(IK) = DBLE( CG(IK,ISEA) / DSIP(IK) )
  730       END DO
  731 !/
  732 !/ End of JACOBI_INIT ------------------------------------------------ /
  733 !/
  734       END SUBROUTINE
  735 !/ ------------------------------------------------------------------- /
  736       SUBROUTINE SYNCHRONIZE_IPGL_ETC_ARRAY(IMOD, IsMulti)
  737 !/
  738 !/                  +-----------------------------------+
  739 !/                  | WAVEWATCH III           NOAA/NCEP |
  740 !/                  |                                   |
  741 !/                  | Aron Roland (BGS IT&E GmbH)       |
  742 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  743 !/                  |                                   |
  744 !/                  |                        FORTRAN 90 |
  745 !/                  | Last update :        01-June-2018 |
  746 !/                  +-----------------------------------+
  747 !/
  748 !/   01-June-2018 : Origination.                        ( version 6.04 )
  749 !/
  750 !  1. Purpose : Sync global local arrays
  751 !  2. Method :
  752 ! 		All the process need to have IPGL_tot and IPGL_TO_PROC
  753 ! 		This is especially the case for the output process.
  754 ! 		So we need some painful exportation business
  755 !  3. Parameters :
  756 !
  757 !     Parameter list
  758 !     ----------------------------------------------------------------
  759 !     ----------------------------------------------------------------
  760 !
  761 !  4. Subroutines used :
  762 !
  763 !      Name      Type  Module   Description
  764 !     ----------------------------------------------------------------
  765 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  766 !     ----------------------------------------------------------------
  767 !
  768 !  5. Called by :
  769 !
  770 !      Name      Type  Module   Description
 
 
                                                         w3parall.F90  page  15
 
 
  771 !     ----------------------------------------------------------------
  772 !     ----------------------------------------------------------------
  773 !
  774 !  6. Error messages :
  775 !  7. Remarks
  776 !  8. Structure :
  777 !  9. Switches :
  778 !
  779 !     !/S  Enable subroutine tracing.
  780 !
  781 ! 10. Source code :
  782 !
  783 !/ ------------------------------------------------------------------- /
  784           IMPLICIT NONE
  785           INTEGER, intent(in) :: IMOD
  786           logical, intent(in) :: IsMulti
  787                                         
  788                                         
  789 !/
  790 !/ End of JACOBI_INIT ------------------------------------------------ /
  791 !/
  792       END SUBROUTINE
  793 !/ ....................----------------------------------------------- /
  794       SUBROUTINE SET_UP_NSEAL_NSEALM(NSEALout, NSEALMout)
  795 !/
  796 !/                  +-----------------------------------+
  797 !/                  | WAVEWATCH III           NOAA/NCEP |
  798 !/                  |                                   |
  799 !/                  | Aron Roland (BGS IT&E GmbH)       |
  800 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  801 !/                  |                                   |
  802 !/                  |                        FORTRAN 90 |
  803 !/                  | Last update :        01-June-2018 |
  804 !/                  +-----------------------------------+
  805 !/
  806 !/   01-June-2018 : Origination.                        ( version 6.04 )
  807 !/
  808 !  1. Purpose : Setup nseal, nsealm in contect of pdlib
  809 !  2. Method :
  810 !  3. Parameters :
  811 !
  812 !     Parameter list
  813 !     ----------------------------------------------------------------
  814 !     ----------------------------------------------------------------
  815 !
  816 !  4. Subroutines used :
  817 !
  818 !      Name      Type  Module   Description
  819 !     ----------------------------------------------------------------
  820 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  821 !     ----------------------------------------------------------------
  822 !
  823 !  5. Called by :
  824 !
  825 !      Name      Type  Module   Description
 
 
                                                         w3parall.F90  page  16
 
 
  826 !     ----------------------------------------------------------------
  827 !     ----------------------------------------------------------------
  828 !
  829 !  6. Error messages :
  830 !  7. Remarks
  831 !  8. Structure :
  832 !  9. Switches :
  833 !
  834 !     !/S  Enable subroutine tracing.
  835 !
  836 ! 10. Source code :
  837 !
  838 !/ ------------------------------------------------------------------- /
  839 !/
  840 !/ ------------------------------------------------------------------- /
  841       USE W3ADATMD, ONLY: MPI_COMM_WAVE, MPI_COMM_WCMP
  842       USE CONSTANTS, ONLY : LPDLIB
  843       USE W3GDATMD, ONLY: NSEA
  844       USE W3ODATMD, ONLY: NTPROC, NAPROC, IAPROC
  845       IMPLICIT NONE
  846       INTEGER, intent(out) :: NSEALout, NSEALMout
  847 !/ Local parameters
  848 !/
  849 !/
  850 !/ ------------------------------------------------------------------- /
  851 !/
  852 !!/PDLIB      WRITE(*,*) 'LPDLIB=', LPDLIB
  853 !!/PDLIB      WRITE(*,*) 'GTYPE=', GTYPE, ' UNGTYPE=', UNGTYPE
  854                                                               
  855 !
  856       IF (.NOT. LPDLIB ) THEN
  857         IF ( IAPROC .LE. NAPROC ) THEN
  858           NSEALout  = 1 + (NSEA-IAPROC)/NAPROC
  859         ELSE
  860           NSEALout  = 0
  861         END IF
  862         NSEALMout = 1 + (NSEA-1)/NAPROC
  863       ELSE
  864       ENDIF
  865 !/
  866 !/ End of JACOBI_INIT ------------------------------------------------ /
  867 !/
  868       END SUBROUTINE
  869 !/ ------------------------------------------------------------------- /
  870       SUBROUTINE INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
  871 !/ ------------------------------------------------------------------- /
  872 !/
  873 !/                  +-----------------------------------+
  874 !/                  | WAVEWATCH III           NOAA/NCEP |
  875 !/                  |                                   |
  876 !/                  | Aron Roland (BGS IT&E GmbH)       |
  877 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  878 !/                  |                                   |
  879 !/                  |                        FORTRAN 90 |
  880 !/                  | Last update :        01-June-2018 |
 
 
                                                         w3parall.F90  page  17
 
 
  881 !/                  +-----------------------------------+
  882 !/
  883 !/   01-June-2018 : Origination.                        ( version 6.04 )
  884 !/
  885 !  1. Purpose : Set Jsea for all schemes
  886 !  2. Method :
  887 !  3. Parameters :
  888 !
  889 !     Parameter list
  890 !     ----------------------------------------------------------------
  891 !     ----------------------------------------------------------------
  892 !
  893 !  4. Subroutines used :
  894 !
  895 !      Name      Type  Module   Description
  896 !     ----------------------------------------------------------------
  897 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  898 !     ----------------------------------------------------------------
  899 !
  900 !  5. Called by :
  901 !
  902 !      Name      Type  Module   Description
  903 !     ----------------------------------------------------------------
  904 !     ----------------------------------------------------------------
  905 !
  906 !  6. Error messages :
  907 !  7. Remarks
  908 !  8. Structure :
  909 !  9. Switches :
  910 !
  911 !     !/S  Enable subroutine tracing.
  912 !
  913 ! 10. Source code :
  914 !
  915 !/ ------------------------------------------------------------------- /
  916 !/
  917       USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
  918       USE W3GDATMD, ONLY: GTYPE, UNGTYPE, MAPSF
  919       USE CONSTANTS, ONLY : LPDLIB
  920       IMPLICIT NONE
  921 !/ ------------------------------------------------------------------- /
  922 !/ Parameter list
  923 !/
  924 !/ ------------------------------------------------------------------- /
  925 !/ Local parameters
  926 !/
  927 !/
  928 !/ ------------------------------------------------------------------- /
  929       INTEGER, intent(in) :: ISEA
  930       INTEGER, intent(out) :: JSEA, ISPROC
  931       INTEGER IP_glob
  932 !!/DEBUG      WRITE(740+IAPROC,*) 'PDLIB=', PDLIB
  933 !!/DEBUG      WRITE(740+IAPROC,*) 'GTYPE=', GTYPE, ' UNGTYPE=', UNGTYPE
  934 !!/DEBUG      FLUSH(740+IAPROC)
  935       IF (.NOT. LPDLIB) THEN
 
 
                                                         w3parall.F90  page  18
 
 
  936         JSEA   = 1 + (ISEA-1)/NAPROC
  937         ISPROC = ISEA - (JSEA-1)*NAPROC
  938       ELSE
  939       ENDIF
  940 !/
  941 !/ End of JACOBI_INIT ------------------------------------------------ /
  942 !/
  943       END SUBROUTINE
  944 !/ ------------------------------------------------------------------- /
  945       SUBROUTINE GET_JSEA_IBELONG(ISEA, JSEA, IBELONG)
  946 !/ ------------------------------------------------------------------- /
  947 !/
  948 !/                  +-----------------------------------+
  949 !/                  | WAVEWATCH III           NOAA/NCEP |
  950 !/                  |                                   |
  951 !/                  | Aron Roland (BGS IT&E GmbH)       |
  952 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  953 !/                  |                                   |
  954 !/                  |                        FORTRAN 90 |
  955 !/                  | Last update :        01-June-2018 |
  956 !/                  +-----------------------------------+
  957 !/
  958 !/   01-June-2018 : Origination.                        ( version 6.04 )
  959 !/
  960 !  1. Purpose : Set belongings of jsea in context of pdlib
  961 !  2. Method :
  962 !  3. Parameters :
  963 !
  964 !     Parameter list
  965 !     ----------------------------------------------------------------
  966 !     ----------------------------------------------------------------
  967 !
  968 !  4. Subroutines used :
  969 !
  970 !      Name      Type  Module   Description
  971 !     ----------------------------------------------------------------
  972 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  973 !     ----------------------------------------------------------------
  974 !
  975 !  5. Called by :
  976 !
  977 !      Name      Type  Module   Description
  978 !     ----------------------------------------------------------------
  979 !     ----------------------------------------------------------------
  980 !
  981 !  6. Error messages :
  982 !  7. Remarks
  983 !  8. Structure :
  984 !  9. Switches :
  985 !
  986 !     !/S  Enable subroutine tracing.
  987 !
  988 ! 10. Source code :
  989 !
  990 !/ ------------------------------------------------------------------- /
 
 
                                                         w3parall.F90  page  19
 
 
  991 !/
  992       USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
  993       USE W3GDATMD, ONLY: GTYPE, UNGTYPE, MAPSF
  994       USE CONSTANTS, ONLY : LPDLIB
  995       IMPLICIT NONE
  996 !/ ------------------------------------------------------------------- /
  997 !/ Parameter list
  998 !/
  999 !/ ------------------------------------------------------------------- /
 1000 !/ Local parameters
 1001 !/
 1002 !/
 1003 !/ ------------------------------------------------------------------- /
 1004 !/
 1005       INTEGER, intent(in) :: ISEA
 1006       INTEGER, intent(out) :: JSEA, IBELONG
 1007       INTEGER ISPROC, IX, JX
 1008       IF (.NOT. LPDLIB) THEN
 1009         JSEA   = 1 + (ISEA-1)/NAPROC
 1010         ISPROC = ISEA - (JSEA-1)*NAPROC
 1011         IF (ISPROC .eq. IAPROC) THEN
 1012           IBELONG=1
 1013         ELSE
 1014           IBELONG=0
 1015         END IF
 1016       ELSE
 1017       ENDIF
 1018 !/
 1019 !/ End of INIT_GET_ISEA ---------------------------------------------- /
 1020 !/
 1021       END SUBROUTINE
 1022 !/ ------------------------------------------------------------------- /
 1023       SUBROUTINE INIT_GET_ISEA(ISEA, JSEA)
 1024 !/ ------------------------------------------------------------------- /
 1025 !/
 1026 !/                  +-----------------------------------+
 1027 !/                  | WAVEWATCH III           NOAA/NCEP |
 1028 !/                  |                                   |
 1029 !/                  | Aron Roland (BGS IT&E GmbH)       |
 1030 !/                  | Mathieu Dutour-Sikiric (IRB)      |
 1031 !/                  |                                   |
 1032 !/                  |                        FORTRAN 90 |
 1033 !/                  | Last update :        01-June-2018 |
 1034 !/                  +-----------------------------------+
 1035 !/
 1036 !/   01-June-2018 : Origination.                        ( version 6.04 )
 1037 !/
 1038 !  1. Purpose : Set Isea for all schemes
 1039 !  2. Method :
 1040 !  3. Parameters :
 1041 !
 1042 !     Parameter list
 1043 !     ----------------------------------------------------------------
 1044 !     ----------------------------------------------------------------
 1045 !
 
 
                                                         w3parall.F90  page  20
 
 
 1046 !  4. Subroutines used :
 1047 !
 1048 !      Name      Type  Module   Description
 1049 !     ----------------------------------------------------------------
 1050 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1051 !     ----------------------------------------------------------------
 1052 !
 1053 !  5. Called by :
 1054 !
 1055 !      Name      Type  Module   Description
 1056 !     ----------------------------------------------------------------
 1057 !     ----------------------------------------------------------------
 1058 !
 1059 !  6. Error messages :
 1060 !  7. Remarks
 1061 !  8. Structure :
 1062 !  9. Switches :
 1063 !
 1064 !     !/S  Enable subroutine tracing.
 1065 !
 1066 ! 10. Source code :
 1067 !
 1068 !/ ------------------------------------------------------------------- /
 1069 !/
 1070       USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
 1071       USE W3GDATMD, ONLY: GTYPE, UNGTYPE
 1072       USE CONSTANTS, ONLY : LPDLIB
 1073 !/ ------------------------------------------------------------------- /
 1074 !/ Parameter list
 1075 !/
 1076 !/ ------------------------------------------------------------------- /
 1077 !/ Local parameters
 1078 !/
 1079 !/ ------------------------------------------------------------------- /
 1080 !/
 1081 !/
 1082 !/ ------------------------------------------------------------------- /
 1083 !/
 1084 !/ ------------------------------------------------------------------- /
 1085 !
 1086       USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
 1087       USE W3GDATMD, ONLY: GTYPE, UNGTYPE
 1088       USE CONSTANTS, ONLY : LPDLIB
 1089       IMPLICIT NONE
 1090       INTEGER, intent(in) :: JSEA
 1091       INTEGER, intent(out) :: ISEA
 1092           IF (.NOT. LPDLIB) THEN
 1093             ISEA         = IAPROC + (JSEA-1)*NAPROC
 1094           ELSE
 1095           ENDIF
 1096 !/
 1097 !/ End of INIT_GET_ISEA ------------------------------------------------ /
 1098 !/
 1099       END SUBROUTINE
 1100 !**********************************************************************
 
 
                                                         w3parall.F90  page  21
 
 
 1101 !*  An array of size (NSEA) is send but only the (1:NSEAL) values     *
 1102 !*  are correct. The program synchonizes everything on all nodes.     *
 1103 !**********************************************************************
 1104       SUBROUTINE SYNCHRONIZE_GLOBAL_ARRAY(TheVar)
 1105 !/ ------------------------------------------------------------------- /
 1106 !/
 1107 !/                  +-----------------------------------+
 1108 !/                  | WAVEWATCH III           NOAA/NCEP |
 1109 !/                  |                                   |
 1110 !/                  | Aron Roland (BGS IT&E GmbH)       |
 1111 !/                  | Mathieu Dutour-Sikiric (IRB)      |
 1112 !/                  |                                   |
 1113 !/                  |                        FORTRAN 90 |
 1114 !/                  | Last update :        01-June-2018 |
 1115 !/                  +-----------------------------------+
 1116 !/
 1117 !/   01-June-2018 : Origination.                        ( version 6.04 )
 1118 !/
 1119 !  1. Purpose : Sync global array in context of pdlib
 1120 !  2. Method :
 1121 !			An array of size (NSEA) is send but only the (1:NSEAL) values
 1122 ! 			are correct. The program synchonizes everything on all nodes.
 1123 !  3. Parameters :
 1124 !
 1125 !     Parameter list
 1126 !     ----------------------------------------------------------------
 1127 !     ----------------------------------------------------------------
 1128 !
 1129 !  4. Subroutines used :
 1130 !
 1131 !      Name      Type  Module   Description
 1132 !     ----------------------------------------------------------------
 1133 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1134 !     ----------------------------------------------------------------
 1135 !
 1136 !  5. Called by :
 1137 !
 1138 !      Name      Type  Module   Description
 1139 !     ----------------------------------------------------------------
 1140 !     ----------------------------------------------------------------
 1141 !
 1142 !  6. Error messages :
 1143 !  7. Remarks
 1144 !  8. Structure :
 1145 !  9. Switches :
 1146 !
 1147 !     !/S  Enable subroutine tracing.
 1148 !
 1149 ! 10. Source code :
 1150 !
 1151 !/ ------------------------------------------------------------------- /
 1152 !
 1153       USE W3GDATMD, ONLY: NSEAL, NSEA, NX
 1154       IMPLICIT NONE
 1155 !/ ------------------------------------------------------------------- /
 
 
                                                         w3parall.F90  page  22
 
 
 1156 !/ Parameter list
 1157 !/
 1158 !/ ------------------------------------------------------------------- /
 1159 !/ Local parameters
 1160 !/
 1161 !/
 1162 !/ ------------------------------------------------------------------- /
 1163 !/
 1164       INCLUDE "mpif.h"
 1165       INTEGER ISEA, JSEA, Status(NX), rStatus(NX)
 1166       INTEGER IPROC, I, ierr, IP, IX, IP_glob
 1167       REAL*8, intent(inout) :: TheVar(NX)
 1168       REAL*8  rVect(NX)
 1169       Status=0
 1170 !/
 1171 !/ End of JACOBI_INIT ------------------------------------------------ /
 1172 !/
 1173       END SUBROUTINE
 1174 !/ ------------------------------------------------------------------- /
 1175       END MODULE W3PARALL
 1176 !/ ------------------------------------------------------------------- /
 1177                                                                         
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
