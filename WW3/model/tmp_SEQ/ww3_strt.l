                                                         ww3_strt.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3STRT
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         06-Jun-2018 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    15-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
   13 !/    18-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
   14 !/    11-Jan-2001 : Flat grid version                   ( version 2.06 )
   15 !/    11-Jun-2001 : Clean up.                           ( version 2.11 )
   16 !/    30-Apr-2002 : Updated W3IORS.                     ( version 2.20 )
   17 !/    13-Nov-2002 : Updated W3IORS.                     ( version 3.00 )
   18 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
   19 !/    28-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   20 !/    08-May-2007 : Starting from calm as an option.    ( version 3.11 )
   21 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   22 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   23 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   24 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
   25 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   26 !/    31-Oct-2010 : Implement unstructured grid         ( version 3.14 )
   27 !/                  (A. Roland and F. Ardhuin)
   28 !/    05-Jul-2011 : Revert to X-Y gaussian shape        ( version 4.01 )
   29 !/    06-Mar-2012 : Hardening output.                   ( version 4.07 )
   30 !/    06-Jun-2018 : Add DEBUGINIT/EXPORTWWM             ( version 6.04 )
   31 !/
   32 !/
   33 !/    Copyright 2009-2012 National Weather Service (NWS),
   34 !/       National Oceanic and Atmospheric Administration.  All rights
   35 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   36 !/       No unauthorized use without permission.
   37 !/
   38 !  1. Purpose :
   39 !
   40 !     Generation of initial conditions for a "cold start" of
   41 !     WAVEWATCH III.
   42 !
   43 !  2. Method :
   44 !
   45 !     General model information os obtained from the model definition
   46 !     file using W3IOGR. The type of the initial field is read
   47 !     from the input file WW3_strt.inp (NDSI). Three types of initial
   48 !     conditions are available.
   49 !       1) Gaussian distribution in longitude, latitude and frequency,
   50 !          cos power in directions. Can default to single spectral
   51 !          bin.
   52 !       2) Predefined JONSWAP spectrum, Gaussian height distribution
   53 !          in space.
   54 !       3) Fetch-limited JONSWAP spectrum based on the actual wind
   55 !          speed. To avoid the need of reading a wind field, the
 
 
                                                         ww3_strt.F90  page   2
 
 
   56 !          restart file is a "dummy", and the actual initial field
   57 !          is constructed in the initialization routine W3INIT.
   58 !       4) User defined spectrum throughout the model.
   59 !       5) Starting from rest.
   60 !     The initial conditions are written to the restart.WW3 using the
   61 !     subroutine W3IORS. Note that the name of the restart file is set
   62 !     in W3IORS.
   63 !
   64 !  3. Parameters :
   65 !
   66 !     Local parameters.
   67 !     ----------------------------------------------------------------
   68 !       NDSI    Int.  Input unit number ("ww3_strt.inp").
   69 !       ITYPE   Int.  Type of field (see section 2).
   70 !       FP,SIP  Real  Peak frequency (Hz) and spread.  \
   71 !       XM,SIX  Real  Id. X (degr.).                   |
   72 !       YM,SIY  Real  Id. Y (degr.).                   |   ITYPE = 1
   73 !       HMAX    Real  Maximum wave height.             |
   74 !       NCOS    Real  Cosine power in dir. distr.      |
   75 !       THM     Real  Mean direction (cart. degr.)     / \
   76 !       ALFA    Real  Energy level of PM spectrum.       |
   77 !       FP      Real  Peak frequency (Hz).               | ITYPE = 2
   78 !       GAMMA   Real  Peak enhancement factor            |
   79 !       SIGA/B  Real  Spread with GAMA.                  /
   80 !     ----------------------------------------------------------------
   81 !
   82 !  4. Subroutines used :
   83 !
   84 !      Name      Type  Module   Description
   85 !     ----------------------------------------------------------------
   86 !      W3NMOD    Subr. W3GDATMD Set number of model.
   87 !      W3SETG    Subr.   Id.    Point to selected model.
   88 !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
   89 !      W3SETW    Subr.   Id.    Point to selected model for wave data.
   90 !      W3DIMW    Subr.   Id.    Set array dims for wave data.
   91 !      W3NAUX    Subr. W3ADATMD Set number of model for aux data.
   92 !      W3SETA    Subr.   Id.    Point to selected model for aux data.
   93 !      W3NOUT    Subr. W3ODATMD Set number of model for output.
   94 !      W3SETO    Subr.   Id.    Point to selected model for output.
   95 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
   96 !      STRACE    Subr.   Id.    Subroutine tracing.
   97 !      NEXTLN    Subr.   Id.    Get next line from input filw
   98 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
   99 !      EJ5P      Func.   Id.    Five parameter JONSWAP spectrum.
  100 !      PRT1DS    Subr. W3ARRYMD Print plot of 1-D spectrum.
  101 !      PRT2DS    Subr.   Id.    Print plot of 2-D spectrum.
  102 !      PRTBLK    Subr.   Id.    Print plot of array.
  103 !      WAVNU1    Subr. W3DISPMD Solve dispersion relation.
  104 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
  105 !      W3IORS    Subr. W3IORSMD Reading/writing restart files.
  106 !      W3DIST    Subr. W3GSRUMD Compute distance between two points.
  107 !      MPI_xxx   Subr. mpif.h   Standard MPI routines.
  108 !     ----------------------------------------------------------------
  109 !
  110 !  5. Called by :
 
 
                                                         ww3_strt.F90  page   3
 
 
  111 !
  112 !     None, stand-alone program.
  113 !
  114 !  6. Error messages :
  115 !
  116 !  7. Remarks :
  117 !
  118 !     - While reading the restart file W3IORS will recognize the
  119 !       need for checking the time, as the restart file contains
  120 !       information on the origine of the file ("cold" or "hot").
  121 !     - User input for x-wise gaussian spread control, SIX, is
  122 !       now available again (option for SIX.NE.SIY available.)
  123 !       If user desires a distribution that is circular in real
  124 !       distances, user should input a negative number for SIX.
  125 !
  126 !  8. Structure :
  127 !
  128 !     ----------------------------------------------------
  129 !        1.a  Set up data structures.
  130 !                            ( W3NMOD , W3NDAT , W3NOUT
  131 !                              W3SETG , W3SETW , W3SETO )
  132 !          b  I-O setup.
  133 !          b  Print heading(s).
  134 !        2.a  Read model defintion file with base model
  135 !             data.                            ( W3IOGR )
  136 !          b  MPP initializations.
  137 !        3.   Get field type from the input file.
  138 !        4.   ITYPE = 1, Gaussian, cosine.
  139 !          a  Read parameters.
  140 !          b  Set-up 1-D spectrum.
  141 !          c  Set-up directional distribution.
  142 !          d  Normalize spectrum with Hmax.
  143 !          e  Distribute over grid.
  144 !        5.   ITYPE = 2, pre-defined JONSWAP.
  145 !          a  Read parameters.
  146 !          b  Set-up 1-D spectrum.
  147 !          c  2-D energy spectrum.
  148 !          d  Distribute over grid.
  149 !        6.   ITYPE = 3, fetch limited JONSWAP.
  150 !        7.   ITYPE = 4, user-defined spectrum.
  151 !          a  Read scale factor.
  152 !          b  Read and rescale spectrum.
  153 !          c  Distribute over grid.
  154 !        8.   ITYPE = 5, start from calm conditions.
  155 !        9.   Convert energy to action
  156 !       10.   Write restart file.              ( W3IORS )
  157 !     ----------------------------------------------------
  158 !
  159 !  9. Switches :
  160 !
  161 !     !/SHRD  Switch for shared / distributed memory architecture.
  162 !     !/DIST  Id.
  163 !
  164 !     !/SHRD  Switch for message passing method.
  165 !     !/MPI   Id.
 
 
                                                         ww3_strt.F90  page   4
 
 
  166 !
  167 !     !/S     Enable subroutine tracing.
  168 !
  169 !     !/O4    Output normalized 1-D energy spectrum.
  170 !     !/O5    Output normalized 2-D energy spectrum.
  171 !     !/O6    Output normalized wave heights (not MPP adapted).
  172 !
  173 ! 10. Source code :
  174 !
  175 !/ ------------------------------------------------------------------- /
  176       USE CONSTANTS
  177 !/
  178 !     USE W3GDATMD, ONLY: W3NMOD, W3SETG
  179 !     USE W3WDATMD, ONLY: W3NDAT, W3SETW, W3DIMW
  180       USE W3ADATMD, ONLY: W3NAUX, W3SETA
  181       USE W3ODATMD, ONLY: W3NOUT, W3SETO
  182       USE W3SERVMD, ONLY: ITRACE, NEXTLN, EJ5P, EXTCDE
  183       USE W3ARRYMD, ONLY : PRT1DS
  184       USE W3ARRYMD, ONLY : PRT2DS
  185       USE W3ARRYMD, ONLY : PRTBLK
  186       USE W3DISPMD, ONLY : WAVNU1
  187       USE W3IOGRMD, ONLY: W3IOGR
  188       USE W3IORSMD, ONLY: W3IORS
  189       USE W3GSRUMD, ONLY: W3DIST
  190 !/
  191       USE W3GDATMD
  192       USE W3WDATMD
  193       USE W3ODATMD, ONLY: NDSE, NDST, NDSO, NAPROC, IAPROC,           &
  194                           NAPOUT, NAPERR, FNMPRE
  195 !/
  196       IMPLICIT NONE
  197 !
  198 !/
  199 !/ ------------------------------------------------------------------- /
  200 !/ Local parameters
  201 !/
  202       INTEGER                 :: NDSI, NDSM, NDSR, NDSTRC, NTRACE,    &
  203                                  NDSEN, IERR, ITYPE, NCOS, IKM, IK,   &
  204                                  ITHM, ITH, JSEA, ISEA, IX, IY, J
  205       INTEGER                 :: NSX, NSY
  206       INTEGER, ALLOCATABLE    :: MAPO(:,:)
  207       REAL                    :: FP, SIP, THM, XM, SIX, YM, SIY, HMAX,&
  208                                  CHSIP, FRREL, ETOT, E1I, FACTOR, X,  &
  209                                  Y, RDSQR, ALFA, GAMMA, SIGA, SIGB,   &
  210                                  YLN, FR, BETA, FRR, S, SUMD, ANG,    &
  211                                  ARG, FACS, DEPTH, WN, CG, HPQMAX
  212       REAL, ALLOCATABLE       :: E1(:), DD(:), E2(:,:), E21(:), FINP(:,:)
  213       REAL, ALLOCATABLE       :: E2OUT(:,:)
  214       REAL, ALLOCATABLE       :: HSIG(:,:)
  215       CHARACTER               :: COMSTR*1, INXOUT*4
  216       LOGICAL                 :: FLONE,NOSIX
  217 !/
  218 !/ ------------------------------------------------------------------- /
  219 !
  220 ! 1.a Initialize data structure
 
 
                                                         ww3_strt.F90  page   5
 
 
  221 !
  222       CALL W3NMOD ( 1, 6, 6 )
  223       CALL W3SETG ( 1, 6, 6 )
  224       CALL W3NDAT (    6, 6 )
  225       CALL W3SETW ( 1, 6, 6 )
  226       CALL W3NAUX (    6, 6 )
  227       CALL W3SETA ( 1, 6, 6 )
  228       CALL W3NOUT (    6, 6 )
  229       CALL W3SETO ( 1, 6, 6 )
  230 !
  231 ! 1.b IO set-up.
  232 !
  233       NDSI   = 10
  234       NDSM   = 20
  235       NDSR   = 20
  236 !
  237       NDSTRC =  6
  238       NTRACE = 10
  239       CALL ITRACE ( NDSTRC, NTRACE )
  240 !
  241 ! 1.c MPP initializations
  242 !
  243       NAPROC = 1
  244       IAPROC = 1
  245 !
  246       IF ( IAPROC .EQ. NAPERR ) THEN
  247           NDSEN  = NDSE
  248         ELSE
  249           NDSEN  = -1
  250         END IF
  251 !
  252       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,900)
  253 !
  254       J      = LEN_TRIM(FNMPRE)
  255       OPEN (NDSI,FILE=FNMPRE(:J)//'ww3_strt.inp',STATUS='OLD',        &
  256             ERR=800,IOSTAT=IERR)
  257       REWIND (NDSI)
  258       READ (NDSI,'(A)',END=801,ERR=802) COMSTR
  259       IF (COMSTR.EQ.' ') COMSTR = '$'
  260       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,901) COMSTR
  261 !
  262 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  263 ! 2.  Read model definition file and mpp initializations.
  264 ! 2.a Reading file
  265 !
  266       CALL W3IOGR ( 'READ', NDSM )
  267 !
  268       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,902) GNAME
  269 !
  270 ! 2.b MPP initializations
  271 !
  272       NSEAL  = NSEA
  273 !
  274       CALL W3DIMW ( 1, NDSE, NDST )
  275       ALLOCATE ( E1(NK), DD(NTH), E2(NTH,NK), E21(NSPEC),           &
 
 
                                                         ww3_strt.F90  page   6
 
 
  276                  FINP(NK,NTH) )
  277 !
  278 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  279 ! 3.  Read type from input file.
  280 !
  281       CALL NEXTLN ( COMSTR , NDSI , NDSEN )
  282       READ (NDSI,*,END=801,ERR=802) ITYPE
  283       IF ( ITYPE.LT.1 .OR. ITYPE.GT.5 ) THEN
  284           IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1010) ITYPE
  285           CALL EXTCDE ( 1 )
  286         END IF
  287       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,930) ITYPE
  288 !
  289 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  290 ! 4.  ITYPE = 1, Gaussian, cosine.
  291 !
  292       IF ( ITYPE .EQ. 1 ) THEN
  293           INXOUT = 'COLD'
  294 !
  295 ! 4.a Read parameters.
  296 !
  297           CALL NEXTLN ( COMSTR , NDSI , NDSEN )
  298           READ (NDSI,*,END=801,ERR=802)                            &
  299                 FP, SIP, THM, NCOS, XM, SIX, YM, SIY, HMAX
  300           FP     = MAX ( 0.5 * TPIINV * SIG(1) , FP )
  301           SIP    = MAX ( 0. , SIP )
  302           DO
  303             IF ( THM .LT. 0. ) THEN
  304                 THM    = THM + 360.
  305               ELSE
  306                 EXIT
  307               END IF
  308             END DO
  309           THM    = MOD ( THM , 360. )
  310           NCOS   = MAX ( 0 , 2*(NCOS/2) )
  311                                          
  312           NOSIX=.FALSE.
  313           IF(SIX.LT.0.0)THEN
  314              IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,903)
  315              NOSIX=.TRUE.
  316           END IF
  317                 
  318           HPQMAX=-999.0
  319           DO JSEA=1, NSEAL
  320             ISEA   = JSEA
  321             IX     = MAPSF(ISEA,1)
  322             IY     = MAPSF(ISEA,2)
  323             IF(HPFAC(IY,IX).GT.HPQMAX)THEN
  324                HPQMAX=HPFAC(IY,IX)
  325             ENDIF
  326           END DO
  327           SIX = MAX(0.01*HPQMAX,SIX)
  328                                     
  329           HPQMAX=-999.0
  330           DO JSEA=1, NSEAL
 
 
                                                         ww3_strt.F90  page   7
 
 
  331             ISEA   = JSEA
  332             IX     = MAPSF(ISEA,1)
  333             IY     = MAPSF(ISEA,2)
  334             IF(HQFAC(IY,IX).GT.HPQMAX)THEN
  335                HPQMAX=HQFAC(IY,IX)
  336             ENDIF
  337           END DO
  338           SIY = MAX(0.01*HPQMAX,SIY)
  339                                     
  340           HMAX   = MAX ( 0. , HMAX )
  341 !
  342           IF ( IAPROC .EQ. NAPOUT ) THEN
  343               IF ( FLAGLL ) THEN
  344                   FACTOR = 1.
  345                   WRITE (NDSO,940) FP, SIP, THM, NCOS, &
  346                     FACTOR*XM, MIN(9999.99,FACTOR*SIX), FACTOR*YM,  &
  347                     MIN(9999.99,FACTOR*SIY), HMAX
  348                 ELSE
  349                   FACTOR = 1.E-3
  350                   WRITE (NDSO,941) FP, SIP, THM, NCOS, &
  351                     FACTOR*XM, MIN(9999.99,FACTOR*SIX), FACTOR*YM,  &
  352                     MIN(9999.99,FACTOR*SIY), HMAX
  353                 END IF
  354             END IF
  355 !
  356           FP     = FP  * TPI
  357           SIP    = SIP * TPI
  358           THM    = MOD ( 630. - THM , 360. ) * DERA
  359 !
  360 ! 4.b Make 1-D spectrum.
  361 !
  362           CHSIP  = 0.1 * DSIP(1)
  363           FLONE  = SIP .LT. CHSIP
  364           IKM    = NINT ( 1. + (LOG(FP)-LOG(FR1*TPI))/LOG(XFR) )
  365           IKM    = MAX ( 1 , MIN ( NK , IKM ) )
  366 !
  367           DO IK=1, NK
  368             IF ( FLONE ) THEN
  369                 IF (IK.EQ.IKM) THEN
  370                     E1(IK) = 1.
  371                   ELSE
  372                     E1(IK) = 0.
  373                   END IF
  374               ELSE
  375                 FRREL  = (SIG(IK)-FP)/SIP
  376                 IF (ABS(FRREL).LT.10) THEN
  377                     E1(IK) = EXP ( -0.125 * FRREL**2 )
  378                   ELSE
  379                     E1(IK) = 0.
  380                   END IF
  381               END IF
  382             END DO
  383 !
  384            IF ( IAPROC .EQ. NAPOUT ) CALL PRT1DS                      &
  385                  (NDSO, NK, E1, SIG(1:), '  ', 10, 0.,                &
 
 
                                                         ww3_strt.F90  page   8
 
 
  386                   'Unscaled 1-D', ' ', 'TEST E(f)')
  387 !
  388 ! 4.c Make directional distribution.
  389 !
  390           FLONE  = NCOS .GT. 20
  391           ITHM   = 1 + NINT ( THM / DTH )
  392           DO ITH=1, NTH
  393             IF (FLONE) THEN
  394                 IF ( ITH .EQ. ITHM ) THEN
  395                     DD(ITH) = 1.
  396                   ELSE
  397                     DD(ITH) = 0.
  398                   END IF
  399               ELSE
  400                 DD(ITH) = MAX ( COS(TH(ITH)-THM) , 0. )**NCOS
  401               END IF
  402             END DO
  403 !
  404 ! 4.d 2-D energy spectrum.
  405 !
  406           ETOT   = 0.
  407           DO IK=1, NK
  408             E1I    = 0.
  409             DO ITH=1, NTH
  410               E2(ITH,IK) = E1(IK) * DD(ITH)
  411               E1I        = E1I + E2(ITH,IK)
  412               END DO
  413             ETOT   = ETOT + E1I * DSIP(IK)
  414             END DO
  415           ETOT   = ETOT * DTH
  416           FACTOR = HMAX**2 / ( 16. * ETOT )
  417 !
  418           E2     = FACTOR * E2
  419 !
  420           ALLOCATE ( E2OUT(NK,NTH) )
  421           DO ITH=1, NTH
  422             DO IK=1, NK
  423               E2OUT(IK,ITH) = TPI * E2(ITH,IK)
  424               END DO
  425             END DO
  426 !
  427           IF ( IAPROC .EQ. NAPOUT ) CALL PRT2DS                       &
  428                   ( NDSO, NK, NK, NTH, E2OUT, SIG(1:), ' ', DERA*TPI, &
  429                     0., 0.0001, 'Energy', 'm2s', 'TEST 2-D')
  430           DEALLOCATE ( E2OUT )
  431 !
  432 ! 4.e Distribute over grid.
  433 !
  434  
  435           DO IK=1, NK
  436             E21(1+(IK-1)*NTH:IK*NTH) = E2(:,IK)
  437             END DO
  438 !
  439           DO JSEA=1, NSEAL
  440 !
 
 
                                                         ww3_strt.F90  page   9
 
 
  441             ISEA   = JSEA
  442             IF (GTYPE .EQ. UNGTYPE) THEN
  443                IX     = MAPSF(ISEA,1)
  444                X      = XYB(IX,1)
  445                Y      = XYB(IX,2)
  446             ELSE
  447                IX     = MAPSF(ISEA,1)
  448                IY     = MAPSF(ISEA,2)
  449                X      = XGRD(IY,IX)
  450                Y      = YGRD(IY,IX)
  451             ENDIF
  452             IF(NOSIX)THEN
  453                RDSQR  =(W3DIST(FLAGLL,X,Y,XM,YM)/SIY)**2
  454             ELSE
  455                RDSQR  =((X-XM)/SIX)**2 + ((Y-YM)/SIY)**2
  456             ENDIF
  457             IF ( RDSQR .GT. 40. ) THEN
  458                 FACTOR = 0.
  459               ELSE
  460                 FACTOR = EXP ( -0.5 * RDSQR )
  461               END IF
  462 !
  463             VA(:,JSEA) = FACTOR * E21
  464 !
  465  
  466 !
  467             END DO
  468 !
  469 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  470 ! 5.  ITYPE = 2, pre-defined JONSWAP.
  471 !
  472         ELSE IF ( ITYPE .EQ. 2 ) THEN
  473           INXOUT = 'COLD'
  474 !
  475 ! 5.a Read parameters.
  476 !
  477           CALL NEXTLN ( COMSTR , NDSI , NDSEN )
  478           READ (NDSI,*,END=801,ERR=802)                               &
  479                 ALFA, FP, THM, GAMMA, SIGA, SIGB, XM, SIX, YM, SIY
  480 !
  481           IF (ALFA.LE.0.) ALFA = 0.0081
  482           IF (FP  .LE.0.) FP   = 0.10
  483           IF (SIGA.LE.0.) SIGA = 0.07
  484           IF (SIGB.LE.0.) SIGB = 0.09
  485           FP     = MAX ( 0.5 * TPIINV * SIG(1) , FP )
  486           FP     = MIN ( TPIINV * SIG(NK) , FP )
  487                                                 
  488           NOSIX=.FALSE.
  489           IF(SIX.LT.0.0)THEN
  490              IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,903)
  491              NOSIX=.TRUE.
  492           END IF
  493                 
  494           HPQMAX=-999.0
  495           DO JSEA=1, NSEAL
 
 
                                                         ww3_strt.F90  page  10
 
 
  496             ISEA   = JSEA
  497             IX     = MAPSF(ISEA,1)
  498             IY     = MAPSF(ISEA,2)
  499             IF(HPFAC(IY,IX).GT.HPQMAX)THEN
  500                HPQMAX=HPFAC(IY,IX)
  501             ENDIF
  502           END DO
  503           SIX = MAX(0.01*HPQMAX,SIX)
  504                                     
  505           HPQMAX=-999.0
  506           DO JSEA=1, NSEAL
  507             ISEA   = JSEA
  508             IX     = MAPSF(ISEA,1)
  509             IY     = MAPSF(ISEA,2)
  510             IF(HQFAC(IY,IX).GT.HPQMAX)THEN
  511                HPQMAX=HQFAC(IY,IX)
  512             ENDIF
  513           END DO
  514           SIY = MAX(0.01*HPQMAX,SIY)
  515                                     
  516           DO
  517             IF ( THM .LT. 0. ) THEN
  518                 THM    = THM + 360.
  519               ELSE
  520                 EXIT
  521               END IF
  522             END DO
  523           THM    = MOD ( THM , 360. )
  524           GAMMA  = MAX (GAMMA,1.)
  525           YLN    = LOG(GAMMA)
  526 !
  527           IF ( IAPROC .EQ. NAPOUT ) THEN
  528               IF ( FLAGLL ) THEN
  529                   FACTOR = 1.
  530                   WRITE (NDSO,950) ALFA, FP, THM, GAMMA, SIGA, SIGB,  &
  531                       FACTOR*XM, FACTOR*SIX, FACTOR*YM, FACTOR*SIY
  532                 ELSE
  533                   FACTOR = 1.E-3
  534                   WRITE (NDSO,951) ALFA, FP, THM, GAMMA, SIGA, SIGB,  &
  535                       FACTOR*XM, FACTOR*SIX, FACTOR*YM, FACTOR*SIY
  536                 END IF
  537             END IF
  538           THM    = MOD ( 630. - THM , 360. ) * DERA
  539 !
  540 ! 5.b Make 1-D spectrum.
  541 !
  542           DO IK=1, NK
  543             FR     = SIG(IK) * TPIINV
  544             E1(IK) = EJ5P (FR, ALFA, FP, YLN, SIGA, SIGB )
  545             END DO
  546 !
  547           IF ( IAPROC .EQ. NAPOUT ) CALL PRT1DS                   &
  548                        (NDSO, NK, E1, SIG(1:), '  ', 18, 0.,      &
  549                        'E(f)', ' ', 'TEST 1-D')
  550 !
 
 
                                                         ww3_strt.F90  page  11
 
 
  551 ! 5.c 2-D energy spectrum.
  552 !     Factor 2pi to go to E(sigma,theta)
  553 !
  554           DO IK = 1,NK
  555             FR     = SIG(IK) * TPIINV
  556             IF (FR.LT.FP) THEN
  557                 BETA =  4.06
  558               ELSE
  559                 BETA = -2.34
  560               END IF
  561             FRR    = MIN ( 2.5 , FR/FP )
  562             S      = 9.77 * FRR**BETA
  563             SUMD   = 0.
  564             DO ITH = 1,NTH
  565               ANG    = COS( 0.5 * ( THM - TH(ITH) ) )**2
  566               DD(ITH) = 0.
  567               IF(ANG.GT.1.E-20) THEN
  568                   ARG    = S * LOG(ANG)
  569                   IF(ARG.GT.-170) DD(ITH) = EXP(ARG)
  570                 END IF
  571               SUMD    = SUMD + DD(ITH)
  572               END DO
  573             FACTOR = 1. / (TPI*SUMD*DTH)
  574             DO ITH = 1,NTH
  575               E2(ITH,IK) = FACTOR * E1(IK) * DD(ITH)
  576               END DO
  577             END DO
  578 !
  579           ALLOCATE ( E2OUT(NK,NTH) )
  580           DO ITH=1, NTH
  581             DO IK=1, NK
  582               E2OUT(IK,ITH) = TPI * E2(ITH,IK)
  583               END DO
  584             END DO
  585 !
  586           IF ( IAPROC .EQ. NAPOUT ) CALL PRT2DS                   &
  587                    (NDSO, NK, NK, NTH, E2OUT, SIG(1:), ' ', 1.,   &
  588                     0., 0.0001, 'E(f,theta)', 'm2s', 'TEST 2-D')
  589           DEALLOCATE ( E2OUT )
  590 !
  591 ! 5.d Distribute over grid.
  592 !
  593  
  594           DO IK=1, NK
  595             E21(1+(IK-1)*NTH:IK*NTH) = E2(:,IK)
  596             END DO
  597 !
  598           DO JSEA=1, NSEAL
  599 !
  600             ISEA   = JSEA
  601             IF (GTYPE .EQ. UNGTYPE) THEN
  602                IX     = MAPSF(ISEA,1)
  603                X      = XYB(IX,1)
  604                Y      = XYB(IX,2)
  605             ELSE
 
 
                                                         ww3_strt.F90  page  12
 
 
  606                IX     = MAPSF(ISEA,1)
  607                IY     = MAPSF(ISEA,2)
  608                X      = XGRD(IY,IX)
  609                Y      = YGRD(IY,IX)
  610             ENDIF
  611             IF(NOSIX)THEN
  612                RDSQR  =(W3DIST(FLAGLL,X,Y,XM,YM)/SIY)**2
  613             ELSE
  614                RDSQR  =((X-XM)/SIX)**2 + ((Y-YM)/SIY)**2
  615             ENDIF
  616             IF ( RDSQR .GT. 40. ) THEN
  617                 FACTOR = 0.
  618               ELSE
  619                 FACTOR = EXP ( -0.5 * RDSQR )
  620               END IF
  621 !
  622             VA(:,JSEA) = FACTOR * E21
  623 !
  624             END DO
  625 !
  626 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  627 ! 6.  ITYPE = 3, fetch limited JONSWAP.
  628 !
  629         ELSE IF ( ITYPE .EQ. 3 ) THEN
  630           INXOUT = 'WIND'
  631           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,960)
  632 !
  633 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  634 ! 7.  ITYPE = 4, User defined.
  635 !
  636         ELSE IF ( ITYPE .EQ. 4 ) THEN
  637           INXOUT = 'COLD'
  638 !
  639 ! 7.a Read parameters.
  640 !
  641           CALL NEXTLN ( COMSTR , NDSI , NDSEN )
  642           READ (NDSI,*,END=801,ERR=802) FACS
  643           IF ( FACS .LE. 0. ) FACS = 1.
  644           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,970) FACS
  645 !
  646 ! 7.b Read and rescale spectrum.
  647 !
  648           CALL NEXTLN ( COMSTR , NDSI , NDSEN )
  649           READ (NDSI,*,END=801,ERR=802)                               &
  650                ((FINP(IK,ITH),IK=1,NK),ITH=1,NTH)
  651 !
  652           FINP = FINP * FACS / TPI
  653 !
  654           IF ( IAPROC .EQ. NAPOUT ) CALL PRT2DS                   &
  655                   (NDSO, NK, NK, NTH, FINP, SIG(1:), ' ', TPI,    &
  656                    0., 0.0001, 'Energy', 'm2s', 'TEST 2-D')
  657 !
  658 ! 7.c Distribute over grid.
  659 !
  660           DO JSEA=1, NSEAL
 
 
                                                         ww3_strt.F90  page  13
 
 
  661 !
  662             ISEA   = JSEA
  663             DO IK=1, NK
  664               DO ITH=1, NTH
  665                 VA(ITH+(IK-1)*NTH,JSEA) = FINP(IK,ITH)
  666                 END DO
  667               END DO
  668             END DO
  669 !
  670 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  671 ! 8.  ITYPE = 5, fetch limited JONSWAP.
  672 !
  673         ELSE
  674           INXOUT = 'CALM'
  675           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,980)
  676 !
  677         END IF
  678 !
  679 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  680 ! 9.  Convert E(sigma) to N(k)
  681 !
  682       IF ( ITYPE.NE.3 .AND. ITYPE.NE.5 ) THEN
  683           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,990)
  684 !
  685           ALLOCATE ( HSIG(NX,NY) )
  686           HSIG   = 0.
  687 !
  688           DO JSEA=1, NSEAL
  689             ISEA   = JSEA
  690             DEPTH  = MAX ( DMIN , -ZB(ISEA) )
  691             ETOT   = 0.
  692             DO IK=1, NK
  693               CALL WAVNU1 ( SIG(IK), DEPTH, WN, CG )
  694               E1I    = 0.
  695               DO ITH=1, NTH
  696                 E1I    = E1I + VA(ITH+(IK-1)*NTH,JSEA)
  697                 VA(ITH+(IK-1)*NTH,JSEA) = VA(ITH+(IK-1)*NTH,JSEA) *   &
  698                                                CG / SIG(IK)
  699                 END DO
  700               ETOT   = ETOT + E1I*DSIP(IK)
  701               END DO
  702             IX     = MAPSF(ISEA,1)
  703             IY     = MAPSF(ISEA,2)
  704             HSIG(IX,IY) = 4. * SQRT ( ETOT * DTH )
  705           END DO
  706 !
  707           ALLOCATE ( MAPO(NX,NY) )
  708           DO IX=1, NX
  709             DO IY=1, NY
  710               MAPO(IX,IY) = MAPSTA(IY,IX)
  711               END DO
  712             END DO
  713 !
  714           NSX    = 1 + NX/35
  715           NSY    = 1 + NY/35
 
 
                                                         ww3_strt.F90  page  14
 
 
  716           IF ( IAPROC .EQ. NAPOUT ) CALL PRTBLK                   &
  717                       (NDSO, NX, NY, NX, HSIG, MAPO, 0, 0.,       &
  718                        1, NX, NSX, 1, NY, NSY, 'Hs', 'm')
  719 !
  720         END IF
  721 !
  722 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  723 !10.  Write restart file.
  724 !
  725       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,995)
  726       CALL W3IORS ( INXOUT, NDSR, SIG(NK) )
  727 !
  728       GOTO 888
  729 !
  730 ! Escape locations read errors :
  731 !
  732   800 CONTINUE
  733       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) IERR
  734       CALL EXTCDE ( 10 )
  735 !
  736   801 CONTINUE
  737       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001)
  738       CALL EXTCDE ( 11 )
  739 !
  740   802 CONTINUE
  741       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002) IERR
  742       CALL EXTCDE ( 12 )
  743 !
  744   888 CONTINUE
  745       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,999)
  746 !
  747 ! Formats
  748 !
  749   900 FORMAT (/15X,'   *** WAVEWATCH III  Initial conditions ***   '/ &
  750                15X,'==============================================='/)
  751   901 FORMAT ( '  Comment character is ''',A,''''/)
  752   902 FORMAT ( '  Grid name : ',A/)
  753   903 FORMAT ( '  Negative SIX was provided by user.         '/       &
  754                '  WW3 will create a gaussian distribution    '/       &
  755                '  that is circular in real space. ')
  756 !
  757   930 FORMAT (/'  Initial field ITYPE =',I2/                          &
  758                ' --------------------------------------------------')
  759 !
  760   940 FORMAT ( '       Gaussian / cosine power spectrum '//           &
  761                '       Peak frequency and spread (Hz)    :',2X,2F8.4/ &
  762                '       Mean direction (Naut., degr.)     :',F7.1/     &
  763                '       Cosine power of dir. distribution :',I5/       &
  764                '       Mean longitude and spread (degr.) :',2F8.2/    &
  765                '       Mean latitude and spread (degr.)  :',2F8.2/    &
  766                '       Maximum wave height               :',F8.2/)
  767 !
  768   950 FORMAT ( '       JONSWAP spectrum'//                            &
  769                '       alfa                          (-) : ',F12.5/   &
  770                '       Peak frequecy                (Hz) : ',F11.4/   &
 
 
                                                         ww3_strt.F90  page  15
 
 
  771                '       Mean direction       (Naut.,deg.) : ',F 8.1/   &
  772                '       gamma                         (-) : ',F 9.2/   &
  773                '       sigma-A                       (-) : ',F11.4/   &
  774                '       sigma-B                       (-) : ',F11.4/   &
  775                '       Mean longitude and spread (degr.) : ',2F9.2/   &
  776                '       Mean latitude and spread  (degr.) : ',2F9.2)
  777   941 FORMAT ( '       Gaussian / cosine power spectrum '//           &
  778                '       Peak frequency and spread (Hz)    :',2X,2F8.4/ &
  779                '       Mean direction (Naut., degr.)     :',F7.1/     &
  780                '       Cosine power of dir. distribution :',I5/       &
  781                '       Mean X and spread (km)            :',2F8.2/    &
  782                '       Mean Y and spread (km)            :',2F8.2/    &
  783                '       Maximum wave height               :',F8.2/)
  784 !
  785   951 FORMAT ( '       JONSWAP spectrum'//                            &
  786                '       alfa                      (-) : ',F12.5/   &
  787                '       Peak frequecy            (Hz) : ',F11.4/   &
  788                '       Mean direction   (Naut.,deg.) : ',F 8.1/   &
  789                '       gamma                     (-) : ',F 9.2/   &
  790                '       sigma-A                   (-) : ',F11.4/   &
  791                '       sigma-B                   (-) : ',F11.4/   &
  792                '       Mean X and spread        (km) : ',2F9.2/   &
  793                '       Mean Y and spread        (km) : ',2F9.2)
  794 !
  795   960 FORMAT ( '       Fetch-limited JONSWAP spectra based on local '/ &
  796                '       wind speed (fetch related to grid increment).')
  797 !
  798   970 FORMAT ( '       User-defined energy spectrum F(f,theta).'//    &
  799                '       Scale factor             (-) : ',E12.4/)
  800 !
  801   980 FORMAT ( '       Starting from calm conditions (Hs = 0)')
  802 !
  803   990 FORMAT (/'       Converting energy to action ... ')
  804   995 FORMAT (/'       Writing restart file  ... '/)
  805 !
  806   999 FORMAT (/'  End of program '/                                   &
  807                ' ========================================='/          &
  808                '         WAVEWATCH III Initial conditions '/)
  809 !
  810  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3STRT : '/               &
  811                '     ERROR IN OPENING INPUT FILE'/                    &
  812                '     IOSTAT =',I5/)
  813 !
  814  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3STRT : '/               &
  815                '     PREMATURE END OF INPUT FILE'/)
  816 !
  817  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3STRT : '/               &
  818                '     ERROR IN READING FROM INPUT FILE'/               &
  819                '     IOSTAT =',I5/)
  820 !
  821  1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3STRT : '/               &
  822                '     ILLEGAL TYPE, ITYPE =',I4/)
  823 !
  824 !/
  825 !/ End of W3STRT ----------------------------------------------------- /
 
 
                                                         ww3_strt.F90  page  16
 
 
  826 !/
  827       END PROGRAM W3STRT
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
