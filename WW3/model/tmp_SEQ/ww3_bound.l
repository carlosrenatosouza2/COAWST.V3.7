                                                        ww3_bound.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3BOUND
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           F. Ardhuin              |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         21-Jul-2020 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    28-Aug-2012 : adaptation from SHOM/Ifremer code   ( version 4.08 )
   13 !/    01-Nov-2012 : Bug correction for NKI != NK        ( version 4.08 )
   14 !/    20-Oct-2016 : Error statement updates             ( version 5.15 )
   15 !/    21-Jul-2020 : Support rotated pole grid           ( version 7.11 )
   16 !/                                  Chris Bunney, UKMO.
   17 !/
   18 !/    Copyright 2012-2012 National Weather Service (NWS),
   19 !/       National Oceanic and Atmospheric Administration.  All rights
   20 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   21 !/       No unauthorized use without permission.
   22 !/
   23 !  1. Purpose :
   24 !
   25 !     Combines spectra files into a nest.ww3 file for boundary conditions
   26 !
   27 !  2. Method :
   28 !
   29 !     Finds nearest points and performs linear interpolation
   30 !
   31 !     The initial conditions are written to the restart.ww3 using the
   32 !     subroutine W3IORS. Note that the name of the restart file is set
   33 !     in W3IORS.
   34 !
   35 !  3. Parameters :
   36 !
   37 !     Local parameters.
   38 !     ----------------------------------------------------------------
   39 !       NDSI    Int.  Input unit number ("ww3_assm.inp").
   40 !       ITYPE   Int.  Type of data
   41 !     ----------------------------------------------------------------
   42 !
   43 !  4. Subroutines used :
   44 !
   45 !      Name      Type  Module   Description
   46 !     ----------------------------------------------------------------
   47 !      STRACE    Subr.   Id.    Subroutine tracing.
   48 !      NEXTLN    Subr.   Id.    Get next line from input filw
   49 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
   50 !      WAVNU1    Subr. W3DISPMD Solve dispersion relation.
   51 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
   52 !      W3EQTOLL  Subr  W3SERVMD Convert coordinates from rotated pole.
   53 !     ----------------------------------------------------------------
   54 !
   55 !  5. Called by :
 
 
                                                        ww3_bound.F90  page   2
 
 
   56 !
   57 !     None, stand-alone program.
   58 !
   59 !  6. Error messages :
   60 !
   61 !  7. Remarks :
   62 !
   63 !     - Can be used also to diagnose contents of nest.ww3 file
   64 !       in read mode
   65 !
   66 !     - Input spectra are assumed to be formulated on a standard
   67 !       pole. However, the model grid can be on a rotated pole.
   68 !
   69 !  8. Structure :
   70 !
   71 !     ----------------------------------------------------
   72 !        1.a  Set up data structures.
   73 !                            ( W3NMOD , W3NDAT , W3NOUT
   74 !                              W3SETG , W3SETW , W3SETO )
   75 !          b  I-O setup.
   76 !        ....
   77 !        9.   Convert energy to action
   78 !       10.   Write restart file.              ( W3IORS )
   79 !     ----------------------------------------------------
   80 !
   81 !  9. Switches :
   82 !
   83 !     !/SHRD  Switch for shared / distributed memory architecture.
   84 !     !/DIST  Id.
   85 !
   86 !     !/SHRD  Switch for message passing method.
   87 !     !/MPI   Id.
   88 !
   89 !     !/S     Enable subroutine tracing.
   90 !
   91 !     !/O4    Output normalized 1-D energy spectrum.
   92 !     !/O5    Output normalized 2-D energy spectrum.
   93 !     !/O6    Output normalized wave heights (not MPP adapted).
   94 !
   95 ! 10. Source code :
   96 !
   97 !/ ------------------------------------------------------------------- /
   98       USE CONSTANTS
   99       USE W3WDATMD, ONLY: W3NDAT, W3SETW
  100       USE W3ADATMD, ONLY: W3NAUX, W3SETA
  101       USE W3ODATMD, ONLY: W3NOUT, W3SETO, FLBPI
  102                                                
  103       USE W3GDATMD, ONLY: NK, NTH, XFR, FR1, GNAME, W3NMOD, W3SETG, &
  104                           NSEA, MAPSTA, XYB, GTYPE, XGRD, YGRD, X0, Y0, &
  105                           SX, SY, MAPSF, UNGTYPE, CLGTYPE, RLGTYPE
  106       USE W3ODATMD, ONLY: NDSO, NDSE
  107       USE W3IOBCMD, ONLY: VERBPTBC, IDSTRBC
  108       USE W3IOGRMD, ONLY: W3IOGR
  109       USE W3TIMEMD
  110       USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE
 
 
                                                        ww3_bound.F90  page   3
 
 
  111 !/
  112       IMPLICIT NONE
  113 !
  114 !/
  115 !/ ------------------------------------------------------------------- /
  116 !/ Local parameters
  117 !/
  118       INTEGER        :: TIME1(2),TIME2(2)
  119       CHARACTER               :: COMSTR*1, LINE*80
  120       CHARACTER*5   :: INXOUT
  121       CHARACTER*10  :: VERTEST  ! = '2018-03-01'
  122       CHARACTER*32  :: IDTST    != 'WAVEWATCH III BOUNDARY DATA FILE'
  123       CHARACTER*120,     ALLOCATABLE  :: SPECFILES(:)
  124       CHARACTER*120 :: FILENAME
  125                                
  126       INTEGER   IX, IY, ISEA, I,JJ,IP,IP1,J,K,ITIME,          &
  127                 NDSI,NDSM, NDSI2,NDSS,NDSB, NDSTRC, NTRACE, &
  128                 NK1,NTH1,NSPEC1, NBI, NBI2,           &
  129                 NKI, NTHI, NBO, NBO2, IERR, INTERP, ILOOP, &
  130                IFMIN, IFMIN2, IFMAX, VERBOSE
  131       INTEGER     IBO
  132       !REAL   , DIMENSION(:),     ALLOCATABLE :: SPEC
  133       REAL   , DIMENSION(:),     ALLOCATABLE :: LATS, LONS
  134       REAL   , DIMENSION(:,:),   ALLOCATABLE :: SPEC2D
  135       REAL   , DIMENSION(:),     ALLOCATABLE :: FREQ, THETA
  136       REAL   FR1I, TH1I
  137       REAL, ALLOCATABLE     :: XBPI(:), YBPI(:), RDBPI(:,:),        &
  138                                XBPO(:), YBPO(:), RDBPO(:,:),        &
  139                                ABPIN(:,:)
  140                                          
  141       INTEGER,  ALLOCATABLE :: IPBPI(:,:),   IPBPO(:,:)
  142                                                        
  143       CHARACTER(120) string1,buoyname
  144       CHARACTER space
  145       REAL depth,U10,Udir,Curr,Currdir,DMIN, DIST, DMIN2, COS1 !, COS2
  146 !/
  147 !/ ------------------------------------------------------------------- /
  148                                                                         
  149                                                                         
  150 !/
  151 ! 1.  IO set-up.
  152 !
  153       CALL W3NMOD ( 1, 6, 6 )
  154       CALL W3SETG ( 1, 6, 6 )
  155       CALL W3NDAT (    6, 6 )
  156       CALL W3SETW ( 1, 6, 6 )
  157       CALL W3NAUX (    6, 6 )
  158       CALL W3SETA ( 1, 6, 6 )
  159       CALL W3NOUT (    6, 6 )
  160       CALL W3SETO ( 1, 6, 6 )
  161 !
  162       NDSI   = 10
  163       NDSB   = 33
  164       NDSM   = 20
  165       NDSS   = 40
 
 
                                                        ww3_bound.F90  page   4
 
 
  166 !
  167       NDSTRC =  6
  168       NTRACE = 10
  169       CALL ITRACE ( NDSTRC, NTRACE )
  170 !
  171  
  172 !
  173 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  174 ! 2.  Read model definition file.
  175 !
  176       CALL W3IOGR ( 'READ', NDSM )
  177       WRITE (NDSO,920) GNAME
  178 !
  179 ! 3. Read input file
  180 !
  181       OPEN(NDSI,FILE='ww3_bound.inp',status='old')
  182       READ (NDSI,'(A)',END=2001,ERR=2002) COMSTR
  183       IF (COMSTR.EQ.' ') COMSTR = '$'
  184       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  185       READ (NDSI,*) INXOUT
  186       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  187       READ (NDSI,*) INTERP
  188       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  189       READ (NDSI,*) VERBOSE
  190       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  191 !
  192       NBO2 = 0
  193 !
  194 !     ILOOP = 1 to count NBO2
  195 !     ILOOP = 2 to read the file names
  196 !
  197       DO ILOOP = 1, 2
  198         OPEN (NDSS,FILE='ww3_bound.scratch',FORM='FORMATTED',          &
  199               status='UNKNOWN')
  200         IF ( ILOOP .EQ. 1 ) THEN
  201           NDSI2 = NDSI
  202         ELSE
  203           NDSI2 = NDSS
  204           ALLOCATE(SPECFILES(NBO2))
  205           NBO2=0
  206           ENDIF
  207                
  208         NBO2=0
  209 ! Read input file names
  210         DO
  211           CALL NEXTLN ( COMSTR , NDSI2 , NDSE )
  212           READ (NDSI2,'(A120)') FILENAME
  213           JJ     = LEN_TRIM(FILENAME)
  214             IF ( ILOOP .EQ. 1 ) THEN
  215               BACKSPACE (NDSI)
  216               READ (NDSI,'(A)') LINE
  217               WRITE (NDSS,'(A)') LINE
  218             END IF
  219           IF (FILENAME(:JJ).EQ."'STOPSTRING'") EXIT
  220           NBO2=NBO2+1
 
 
                                                        ww3_bound.F90  page   5
 
 
  221           IF (ILOOP.EQ.1) CYCLE
  222           SPECFILES(NBO2)=FILENAME
  223           END DO
  224 !
  225 ! ... End of ILOOP loop
  226 !
  227         IF ( ILOOP .EQ. 1 ) CLOSE ( NDSS)
  228                                          
  229         IF ( ILOOP .EQ. 2 ) CLOSE ( NDSS, STATUS='DELETE' )
  230        END DO
  231       CLOSE(NDSI)
  232                  
  233 !
  234 ! 3. Tests the reading of the file
  235 !
  236       IF ( INXOUT.EQ.'READ') THEN
  237         OPEN(NDSB,FILE='nest.ww3',FORM='UNFORMATTED',status='old')
  238         READ(NDSB) IDTST, VERTEST, NK1, NTH1, XFR, FR1I, TH1I, NBI
  239         NSPEC1  = NK1 * NTH1
  240         IF ( IDTST .NE. IDSTRBC ) THEN
  241           WRITE (NDSO,901) IDTST, IDSTRBC
  242           END IF
  243         WRITE(NDSO,*) "FORMAT VERSION: '",VERTEST,"'"
  244         WRITE(NDSO,*) "FILE TYPE: '",IDTST,"'"
  245         IF (VERBOSE.EQ.1) WRITE(NDSO,'(A,2I5,3F12.6,I5)') 'NK,NTH,XFR, FR>
  246                     NK1,NTH1,XFR, FR1I, TH1I, NBI
  247         ALLOCATE (XBPI(NBI),YBPI(NBI))
  248         ALLOCATE (IPBPI(NBI,4),RDBPI(NBI,4))
  249         READ(NDSB) (XBPI(I),I=1,NBI),           &
  250                (YBPI(I),I=1,NBI),             &
  251                ((IPBPI(I,J),I=1,NBI),J=1,4),  &
  252                ((RDBPI(I,J),I=1,NBI),J=1,4)
  253         IF (VERBOSE.EQ.1) WRITE(NDSO,*)      'XBPI:',XBPI
  254         IF (VERBOSE.EQ.1) WRITE(NDSO,*)      'YBPI:',YBPI
  255         IF (VERBOSE.EQ.1) WRITE(NDSO,*)      'IPBPI:'
  256         DO I=1,NBI
  257           IF (VERBOSE.EQ.1) WRITE(NDSO,*) I,' interpolated from:',IPBPI(I>
  258           IF (VERBOSE.EQ.1) WRITE(NDSO,*) I,' with coefficient :',RDBPI(I>
  259           END DO
  260 !
  261         READ (NDSB) TIME2, NBI2
  262         BACKSPACE (NDSB)
  263         ALLOCATE (ABPIN(NSPEC1,NBI2))
  264         IERR=0
  265         DO WHILE (IERR.EQ.0)
  266           READ (NDSB,IOSTAT=IERR) TIME2, NBI2
  267           IF (IERR.EQ.0) THEN
  268           IF (VERBOSE.EQ.1) WRITE(NDSO,*)      'TIME2,NBI2:',TIME2, NBI2,>
  269           DO IP=1, NBI2
  270             READ (NDSB,IOSTAT=IERR) ABPIN(:,IP)
  271             END DO
  272           END IF
  273         END DO
  274         CLOSE(NDSB)
  275         END IF
 
 
                                                        ww3_bound.F90  page   6
 
 
  276 !
  277       IF ( INXOUT.EQ.'WRITE') THEN
  278         IF ( FLBPI) THEN
  279 !
  280 !  Defines position of active boundary points
  281 !
  282         NBO = 0
  283         DO ISEA=1,NSEA
  284           IX     = MAPSF(ISEA,1)
  285           IY     = MAPSF(ISEA,2)
  286           IF (MAPSTA(IY,IX).EQ.2) THEN
  287             NBO=NBO+1
  288             END IF
  289           END DO
  290         ALLOCATE(XBPO(NBO),YBPO(NBO))
  291         ALLOCATE (IPBPO(NBO,4),RDBPO(NBO,4))
  292         IBO=0
  293         DO ISEA=1,NSEA
  294           IX     = MAPSF(ISEA,1)
  295           IY     = MAPSF(ISEA,2)
  296           IF (MAPSTA(IY,IX).EQ.2) THEN
  297             IBO=IBO+1
  298             SELECT CASE ( GTYPE )
  299             CASE ( RLGTYPE )
  300               XBPO(IBO)=X0+SX*(IX-1)
  301               YBPO(IBO)=Y0+SY*(IY-1)
  302             CASE ( CLGTYPE )
  303               XBPO(IBO)= XGRD(IY,IX)
  304               YBPO(IBO)= YGRD(IY,IX)
  305             CASE (UNGTYPE)
  306               XBPO(IBO)= XYB(IX,1)
  307               YBPO(IBO)= XYB(IX,2)
  308               END SELECT !GTYPE
  309             END IF
  310           END DO
  311         OPEN(NDSB,FILE='nest.ww3',FORM='UNFORMATTED',status='unknown')
  312         ALLOCATE(LATS(NBO2),LONS(NBO2))
  313         DO IP=1,NBO2
  314           OPEN(200+IP,FILE=SPECFILES(IP),status='old',iostat=IERR)
  315           IF (VERBOSE.EQ.1) WRITE(NDSO,'(A,I5,3A,I5)') 'IP, file, I/O err>
  316                                   TRIM(SPECFILES(IP)),', ',IERR
  317           IF (IERR.NE.0) WRITE(NDSE,*) 'File do not exist:',SPECFILES(IP)
  318           READ(200+IP,'(A1,A22,A1,X,2I6)',iostat=IERR)  &
  319              space,string1,space,NKI,NTHI
  320           IF (VERBOSE.EQ.1) WRITE(NDSO,'(A,3I5)') 'IP and spectral dimens>
  321           IF (IP.EQ.1) THEN
  322             NK1=NKI
  323             NTH1=NTHI
  324             NSPEC1  = NK1 * NTH1
  325             ALLOCATE (FREQ(NK1),THETA(NTH1))
  326             ALLOCATE (SPEC2D(NK1,NTH1))
  327             ALLOCATE (ABPIN(NK*NTH1,NBO2))
  328           ELSE
  329 !
  330 !  To be cleaned up later ...
 
 
                                                        ww3_bound.F90  page   7
 
 
  331 !
  332             IF (NK1.NE.NKI.OR.NTH1.NE.NTHI) THEN
  333               WRITE(NDSE,'(A,A,4I5)') 'ERROR, SPECTRAL GRID IN FILE:',   >
  334                          TRIM(SPECFILES(IP)),NK1,NKI,NTH1,NTHI
  335               STOP
  336               END IF
  337             END IF
  338 !
  339           READ(200+IP,*) FREQ(1:NK1)
  340           READ(200+IP,*) THETA(1:NTH1)
  341         END DO
  342               
  343 !
  344 ! Defines frequency range in spectra
  345 !
  346  
  347       ! Checks consistency of NK
  348       IF (NKI.GT.NK) GOTO 2008
  349 !
  350       ! HERE we define IFMIN IFMIN2 IFMAX and IFMAX2 frequency indices
  351       ! such that source spec SPEC (read in input) links with output spec
  352       ! APBIN with APBIN(IFMIN2:IFMAX2) = SPEC(IFMIN:IFMAX)
  353       ! Then APBIN(1:IFMIN2) = 0 and APBIN(IFMAX2:end) = 0
  354       IFMIN=1  ! index of first freq. in source spectrum
  355       IFMIN2=1 ! index of first freq. in output spectrum
  356       IFMAX=NK1 ! index of last freq. in source spectrum
  357 !        IFMAX2=NK ! index of last freq. in output spectrum
  358 !
  359       ! Checks consistency of XFR
  360       IF (ABS((FREQ(IFMIN+1)/FREQ(IFMIN))-XFR).GT.0.005) GOTO 2006
  361 !
  362       ! Checks consistency of NTH
  363       ! WARNING: check is only done on number of directions, no check
  364       ! is done on the relative offset of first direction in terms of
  365       ! the directional increment [-0.5,0.5] (last parameter of the
  366       ! spectral definition in ww3_grid.inp, on second active line)
  367       IF (NTHI.NE.NTH) GOTO 2007
  368                                 
  369       IF ((FR1-FREQ(1))/FR1.GT. 0.03) THEN
  370         DO J=1,MIN(NK1,NK)
  371           IF (ABS(FREQ(J)-FR1) .LT. ABS(FREQ(IFMIN)-FR1)) THEN
  372             IFMIN=J
  373             END IF
  374           END DO
  375         END IF
  376 !
  377       IF ((FREQ(1)-FR1)/FR1.GT. 0.03) THEN
  378         DO J=1,MIN(NK,NK1)
  379           IF (ABS(FREQ(J)-FR1*XFR**(J-1)) .LT. ABS(FREQ(IFMIN2)-FR1)) THEN
  380             IFMIN2=J
  381             END IF
  382           END DO
  383         END IF
  384 !
  385       IF ((FREQ(NK1)-FR1*XFR**(NK-1))/FREQ(NK1) .GT.0.03) THEN
 
 
                                                        ww3_bound.F90  page   8
 
 
  386         DO J=1,NK
  387             IF (ABS(FREQ(J)-FR1*XFR**(NK1-1)) .LT. ABS(FREQ(IFMAX)-FR1*XF>
  388             IFMAX=J
  389             END IF
  390           END DO
  391         END IF
  392 !
  393       IERR=0
  394       ITIME=0
  395 !
  396 ! Loop on times
  397 !
  398       DO WHILE (IERR.EQ.0)
  399         DO IP=1,NBO2
  400           READ(200+IP,*,IOSTAT=IERR) TIME2
  401           IF (IERR.EQ.0) THEN
  402 !
  403               IF (IP.EQ.1) THEN
  404               TIME1=TIME2
  405             ELSE
  406                 IF (TIME1(1).NE.TIME2(1).OR.TIME1(2).NE.TIME2(2)) THEN
  407                   WRITE(NDSE,*) 'AT POINT ',IP,', BAD TIMES:',TIME1, TIME2
  408                 END IF
  409               END IF
  410 !
  411             READ(200+IP,'(A1,A10,A1,2F7.2,F10.1,F7.2,F6.1,F7.2,F6.1)') &
  412               space,buoyname,space,LATS(IP),LONS(IP),depth,U10,Udir,Curr,>
  413             READ(200+IP,*,IOSTAT=IERR) SPEC2D
  414               IF (IFMIN2.GT.1) THEN
  415 !
  416 !  Fills in the low frequency end of the spectrum
  417 !
  418               ABPIN(1:(IFMIN2-1)*NTH,IP)=0.
  419               END IF
  420             DO I=IFMIN,IFMAX
  421               DO J=1,NTH
  422                 ABPIN((I-IFMIN+(IFMIN2-1))*NTH+J,IP)=SPEC2D(I,J)*tpiinv
  423                 END DO
  424               END DO
  425            IF (IFMAX-IFMIN+IFMIN2.LT.NK1) THEN
  426         !IF (VERBOSE.EQ.1) WRITE(NDSO,*) 'FILLING TAIL',IFMAX-IFMIN,NK1,I>
  427              ABPIN((IFMAX-IFMIN+IFMIN2)*NTH+1:NK1*NTH,IP)=0.
  428              END IF
  429            END IF  ! ned of test on IERR
  430          END DO
  431 !
  432 ! Writes header
  433 !
  434        IF (IERR.EQ.0) THEN
  435          IF (ITIME.EQ.0) THEN
  436 ! Correction for rounding error in ASCII files ...
  437            IF (ABS(THETA(1)-0.5*PI).LT.0.01) THETA(1)=0.5*PI
  438 ! Writes header in nest.ww3 file
  439            WRITE(NDSB) IDSTRBC, VERBPTBC, NK1, NTH, XFR, FREQ(1), &
  440                        MOD(2.5*PI-THETA(1),TPI), NBO
 
 
                                                        ww3_bound.F90  page   9
 
 
  441              IPBPO(:,:)=1
  442              RDBPO(:,1)=1.
  443              RDBPO(:,2:4)=0.
  444 !
  445            DO IP1=1,NBO
  446              DMIN=360.+180.
  447              DMIN2=360.+180.
  448              DO IP=1,NBO2
  449 !
  450 ! Searches for the nearest 2 points where spectra are available
  451 !
  452           DIST=SQRT((LONS(IP)-XBPO(IP1))**2+(LATS(IP)-YBPO(IP1))**2)
  453           IF (DMIN.EQ.(360.+180.)) THEN
  454             IF(DIST.LT.DMIN) THEN
  455               IPBPO(IP1,1)=IP
  456               DMIN=DIST
  457               END IF
  458           ELSE
  459             IF(DIST.LT.DMIN2) THEN
  460               IF(DIST.LT.DMIN) THEN
  461              IPBPO(IP1,2)=IPBPO(IP1,1)
  462              DMIN2=DMIN
  463                 IPBPO(IP1,1)=IP
  464              DMIN=DIST
  465            ELSE
  466                 IPBPO(IP1,2)=IP
  467              DMIN2=DIST
  468                 END IF
  469                  ENDIF
  470             END IF
  471           END DO
  472          !IF (VERBOSE.EQ.1)  WRITE(NDSO,*) 'DIST:',DMIN,DMIN2,IP1,IPBPO(I>
  473          !                    LONS(IPBPO(IP1,1)),LONS(IPBPO(IP1,2)),XBPO(>
  474          !                    LATS(IPBPO(IP1,1)),LATS(IPBPO(IP1,2)),YBPO(>
  475 !
  476 !  Computes linear interpolation coefficient between the nearest 2 points
  477 !
  478           IF (INTERP.GT.1.AND.NBO2.GT.1) THEN
  479                 DIST=SQRT((LONS(IPBPO(IP1,1))-LONS(IPBPO(IP1,2)))**2   &
  480                    +(LATS(IPBPO(IP1,1))-LATS(IPBPO(IP1,2)))**2)
  481           COS1=( (XBPO(IP1)-LONS(IPBPO(IP1,1)))  &
  482                 *(LONS(IPBPO(IP1,2))-LONS(IPBPO(IP1,1))) &
  483                 + (YBPO(IP1)-LATS(IPBPO(IP1,1)))  &
  484                 *(LATS(IPBPO(IP1,2))-LATS(IPBPO(IP1,1))))/(DIST**2)
  485           !COS2=( (XBPO(IP1)-LONS(IPBPO(IP1,2)))  &
  486           !      *(LONS(IPBPO(IP1,1))-LONS(IPBPO(IP1,2)))
  487           !      + (YBPO(IP1)-LATS(IPBPO(IP1,2)))  &
  488           !      *(LATS(IPBPO(IP1,1))-LATS(IPBPO(IP1,2))))/(DIST**2)
  489           RDBPO(IP1,1)=1-MIN(1.,MAX(0.,COS1))
  490              RDBPO(IP1,2)=MIN(1.,MAX(0.,COS1))
  491             END IF
  492 !
  493           IF (VERBOSE.EQ.1) WRITE(NDSO,*) 'IPBP:',IP1,(IPBPO(IP1,J),J=1,4)
  494           IF (VERBOSE.EQ.1) WRITE(NDSO,*) 'RDBP:',IP1,(RDBPO(IP1,J),J=1,4)
  495 !
 
 
                                                        ww3_bound.F90  page  10
 
 
  496         END DO
  497       WRITE(NDSB) (XBPO(I),I=1,NBO),           &
  498                   (YBPO(I),I=1,NBO),           &
  499                   ((IPBPO(I,J),I=1,NBO),J=1,4),&
  500                   ((RDBPO(I,J),I=1,NBO),J=1,4)
  501          END IF
  502                
  503        WRITE(NDSO,*) 'Writing boundary data for time:', TIME2, NBO2
  504        WRITE(NDSB,IOSTAT=IERR) TIME2, NBO2
  505        DO IP=1, NBO2
  506          WRITE (NDSB) ABPIN(:,IP)
  507          END DO
  508                
  509        ITIME=ITIME+1
  510        END IF
  511        END DO
  512        CLOSE(NDSB)
  513      END IF
  514      END IF
  515 STOP
  516 !
  517 ! Escape locations read errors :
  518 !
  519  
  520  2001 CONTINUE
  521       WRITE (NDSE,1001)
  522       CALL EXTCDE ( 61 )
  523 !
  524  2002 CONTINUE
  525       WRITE (NDSE,1002) IERR
  526       CALL EXTCDE ( 62 )
  527 !
  528  2006 CONTINUE
  529       WRITE (NDSE,1006) XFR
  530       CALL EXTCDE ( 66 )
  531 !
  532  2007 CONTINUE
  533       WRITE (NDSE,1007) NTH, NTHI
  534       CALL EXTCDE ( 67 )
  535 !
  536  2008 CONTINUE
  537       WRITE (NDSE,1008) NK, NKI
  538       CALL EXTCDE ( 68 )
  539 !
  540   901 FORMAT (/' *** WAVEWATCH-III ERROR IN W3IOBC :'/                &
  541                '     ILEGAL IDSTR, READ : ',A/                        &
  542                '                  CHECK : ',A/)
  543 !
  544   920 FORMAT ( '  Grid name : ',A/)
  545 !
  546  1001 FORMAT (/' *** WAVEWATCH-III ERROR IN W3BOUND : '/              &
  547                '     PREMATURE END OF INPUT FILE'/)
  548 !
  549  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUND: '/               &
  550                '     ERROR IN READING ',A,' FROM INPUT FILE'/         &
 
 
                                                        ww3_bound.F90  page  11
 
 
  551                '     IOSTAT =',I5/)
  552 !
  553  1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUND: '/               &
  554                '     ILLEGAL XFR, XFR =',F12.6/)
  555 !
  556  1007 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUND: '/               &
  557                '     ILLEGAL NTH, NTH =',I3,' DIFFERS FROM NTHI =',I3/)
  558 !
  559  1008 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUND: '/               &
  560                '     ILLEGAL NK, NK =',I3,' DIFFERS FROM NKI =',I3/   &
  561                '     IT WILL BE MANAGED SOON BY SPCONV')
  562 !
  563 !/
  564 !/ End of W3BOUND ---------------------------------------------------- /
  565 !/
  566       END PROGRAM W3BOUND
  567 !/ ------------------------------------------------------------------- /
  568                                                                         
  569                                                                         
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
