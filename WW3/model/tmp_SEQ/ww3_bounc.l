                                                        ww3_bounc.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3BOUNC
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           F. Ardhuin              |
    8 !/                  |           M. Accensi              |
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :         21-Jul-2020 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    24-May-2013 : Adaptation from ww3_bound.ftn       ( version 4.08 )
   14 !/     1-Apr-2015 : Add checks on lat lon xfr           ( version 5.05 )
   15 !/    11-May-2015 : Allow use of cartesian grids        ( version 5.08 )
   16 !/    17-Aug-2016 : Bug correction on RDBPO             ( version 5.10 )
   17 !/    20-Oct-2016 : Error statement updates             ( version 5.15 )
   18 !/    20-Mar-2018 : Improve netcdf file reading         ( version 6.02 )
   19 !/    15-May-2018 : Add namelist feature                ( version 6.05 )
   20 !/    04-May-2020 : Update spectral conversion          ( version 7.XX )
   21 !/    21-Jul-2020 : Support rotated pole grid           ( version 7.11 )
   22 !/
   23 !/
   24 !/    Copyright 2012-2013 National Weather Service (NWS),
   25 !/       National Oceanic and Atmospheric Administration.  All rights
   26 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   27 !/       No unauthorized use without permission.
   28 !/
   29 !  1. Purpose :
   30 !
   31 !     Combines spectra files into a nest.ww3 file for boundary conditions
   32 !
   33 !  2. Method :
   34 !
   35 !     Finds nearest points and performs linear interpolation
   36 !
   37 !     The initial conditions are written to the restart.ww3 using the
   38 !     subroutine W3IORS. Note that the name of the restart file is set
   39 !     in W3IORS.
   40 !
   41 !  3. Parameters :
   42 !
   43 !     Local parameters.
   44 !     ----------------------------------------------------------------
   45 !       NDSI    Int.  Input unit number ("ww3_assm.inp").
   46 !       ITYPE   Int.  Type of data
   47 !     ----------------------------------------------------------------
   48 !
   49 !  4. Subroutines used :
   50 !
   51 !      Name      Type  Module   Description
   52 !     ----------------------------------------------------------------
   53 !      STRACE    Subr.   Id.    Subroutine tracing.
   54 !      NEXTLN    Subr.   Id.    Get next line from input filw
   55 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
 
 
                                                        ww3_bounc.F90  page   2
 
 
   56 !      WAVNU1    Subr. W3DISPMD Solve dispersion relation.
   57 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
   58 !      W3EQTOLL  Subr  W3SERVMD Convert coordinates from rotated pole.
   59 !     ----------------------------------------------------------------
   60 !
   61 !  5. Called by :
   62 !
   63 !     None, stand-alone program.
   64 !
   65 !  6. Error messages :
   66 !
   67 !  7. Remarks :
   68 !
   69 !     - Can be used also to diagnose contents of nest.ww3 file
   70 !       in read mode
   71 !
   72 !     - Input spectra are assumed to be formulated on a standard
   73 !       pole. However, the model grid can be on a rotated pole.
   74 !
   75 !  8. Structure :
   76 !
   77 !     ----------------------------------------------------
   78 !        1.a  Set up data structures.
   79 !                            ( W3NMOD , W3NDAT , W3NOUT
   80 !                              W3SETG , W3SETW , W3SETO )
   81 !          b  I-O setup.
   82 !        ....
   83 !        9.   Convert energy to action
   84 !       10.   Write restart file.              ( W3IORS )
   85 !     ----------------------------------------------------
   86 !
   87 !  9. Switches :
   88 !
   89 !     !/SHRD  Switch for shared / distributed memory architecture.
   90 !     !/DIST  Id.
   91 !
   92 !     !/SHRD  Switch for message passing method.
   93 !     !/MPI   Id.
   94 !
   95 !     !/S     Enable subroutine tracing.
   96 !
   97 !     !/O4    Output normalized 1-D energy spectrum.
   98 !     !/O5    Output normalized 2-D energy spectrum.
   99 !     !/O6    Output normalized wave heights (not MPP adapted).
  100 !
  101 ! 10. Source code :
  102 !
  103 !/ ------------------------------------------------------------------- /
  104       USE CONSTANTS
  105       USE W3WDATMD, ONLY: W3NDAT, W3SETW
  106       USE W3ADATMD, ONLY: W3NAUX, W3SETA
  107       USE W3ODATMD, ONLY: W3NOUT, W3SETO, FNMPRE, NDST, NDSE
  108       USE W3CSPCMD, ONLY: W3CSPC
  109                                 
  110                                 
 
 
                                                        ww3_bounc.F90  page   3
 
 
  111       USE W3GDATMD, ONLY: NK, NTH, XFR, FR1, DTH, TH, FACHFE,           &
  112                           GNAME, W3NMOD, W3SETG,&
  113                           NSEA, MAPSTA, XYB, GTYPE, XGRD, YGRD, X0, Y0, &
  114                           SX, SY, MAPSF, UNGTYPE, CLGTYPE, RLGTYPE, FLAGLL
  115       USE W3ODATMD, ONLY: NDSO, NDSE
  116       USE W3IOBCMD, ONLY: VERBPTBC, IDSTRBC
  117       USE W3IOGRMD, ONLY: W3IOGR
  118       USE W3TIMEMD
  119       USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE, DIST_SPHERE
  120       USE W3NMLBOUNCMD
  121       USE NETCDF
  122                 
  123 !/
  124       IMPLICIT NONE
  125 !
  126 !/
  127 !/ ------------------------------------------------------------------- /
  128 !/ Local parameters
  129 !/
  130   
  131       TYPE(NML_BOUND_T)       :: NML_BOUND
  132 !
  133       INTEGER                 :: IX, IY, ISEA, I,JJ,IP,IP1,J,IT,       &
  134                                  NDSI,NDSM, NDSI2,NDSS,NDSB, NDSC,     &
  135                                  NDSTRC, NTRACE, NK1,NTH1,NT1, NSPEC1, &
  136                                  NBI, NBI2, NKI, NTHI, NTI, NBO, NBO2, &
  137                                  IERR, INTERP, ILOOP, IFMIN, IFMIN2,   &
  138                                  IFMAX, VERBOSE, IBO, IRET, ICODE, NDSL
  139       INTEGER                 :: TIME(2), TIME2(2), VARID(12),         &
  140                                  REFDATE(8), CURDATE(8)
  141 !
  142       INTEGER, ALLOCATABLE    :: IPBPI(:,:), IPBPO(:,:), NCID(:),      &
  143                                  DIMID(:,:), DIMLN(:,:)
  144 !
  145       REAL                    :: FR1I, XFRI, TH1I, FACTOR, OFFSET, DMIN,&
  146                                  DIST, DMIN2, COS1, DLON, DLAT, DLO
  147 !
  148       REAL, ALLOCATABLE       :: SPEC2D(:,:,:,:), LATS(:), LONS(:),    &
  149                                  FREQ(:), THETA(:),                    &
  150                                  XBPI(:), YBPI(:), RDBPI(:,:),         &
  151                                  XBPO(:), YBPO(:), RDBPO(:,:),         &
  152                                  ABPIN(:,:), ABPIN2(:,:,:)
  153 !
  154       REAL, ALLOCATABLE       :: TMPSPCI(:,:),TMPSPCO(:,:)
  155                                                           
  156 !
  157       DOUBLE PRECISION        :: REFJULDAY, CURJULDAY
  158       DOUBLE PRECISION, ALLOCATABLE       :: TIMES(:)
  159 !
  160       CHARACTER               :: COMSTR*1, LINE*512, FILENAME*512,     &
  161                                  INXOUT*5, FILE*128
  162       CHARACTER*50            :: TIMEUNITS, CALENDAR
  163       CHARACTER*10            :: VERTEST  ! = '2018-03-01'
  164       CHARACTER*32            :: IDTST    != 'WAVEWATCH III BOUNDARY DATA>
  165       CHARACTER*512, ALLOCATABLE          :: SPECFILES(:)
 
 
                                                        ww3_bounc.F90  page   4
 
 
  166       CHARACTER, ALLOCATABLE              :: STATION(:,:)
  167 !
  168       LOGICAL                 :: FLGNML, SPCONV
  169 !
  170 !/
  171 !/ ------------------------------------------------------------------- /
  172                                                                         
  173                                                                         
  174 !/
  175 ! 1.  IO set-up.
  176 !
  177       CALL W3NMOD ( 1, 6, 6 )
  178       CALL W3SETG ( 1, 6, 6 )
  179       CALL W3NDAT (    6, 6 )
  180       CALL W3SETW ( 1, 6, 6 )
  181       CALL W3NAUX (    6, 6 )
  182       CALL W3SETA ( 1, 6, 6 )
  183       CALL W3NOUT (    6, 6 )
  184       CALL W3SETO ( 1, 6, 6 )
  185 !
  186       NDSI   = 10
  187       NDSB   = 33
  188       NDSC   = 44
  189       NDSM   = 20
  190       NDSS   = 30
  191       NDSL   = 50
  192       NDSO   = 6
  193       NDSE   = 6
  194 !
  195       NDSTRC =  6
  196       NTRACE = 10
  197       CALL ITRACE ( NDSTRC, NTRACE )
  198 !
  199       WRITE (NDSO,900)
  200 !
  201  
  202 !
  203 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  204 ! 2.  Read model definition file.
  205 !
  206       CALL W3IOGR ( 'READ', NDSM )
  207       WRITE (NDSO,920) GNAME
  208 !
  209 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  210 ! 3.  Read requests from input file.
  211 !
  212  
  213 !
  214 ! process ww3_bounc namelist
  215 !
  216       INQUIRE(FILE=TRIM(FNMPRE)//"ww3_bounc.nml", EXIST=FLGNML)
  217       IF (FLGNML) THEN
  218         ! Read namelist
  219         CALL W3NMLBOUNC (NDSI, TRIM(FNMPRE)//'ww3_bounc.nml', NML_BOUND, >
  220                                                                           
 
 
                                                        ww3_bounc.F90  page   5
 
 
  221         INXOUT = NML_BOUND%MODE
  222         INTERP = NML_BOUND%INTERP
  223         VERBOSE = NML_BOUND%VERBOSE
  224         FILE = NML_BOUND%FILE
  225                              
  226         NBO2 = 0
  227         OPEN(NDSL,FILE=TRIM(FILE),STATUS='OLD',ERR=809,IOSTAT=IERR)
  228         REWIND (NDSL)
  229         DO
  230           READ (NDSL,*,END=400,ERR=802)
  231           NBO2 = NBO2 + 1
  232         END DO
  233         400 CONTINUE
  234         ALLOCATE(SPECFILES(NBO2))
  235         REWIND (NDSL)
  236         DO I=1,NBO2
  237           READ (NDSL,'(A512)',END=801,ERR=802) SPECFILES(I)
  238         END DO
  239         CLOSE(NDSL)
  240                    
  241       END IF ! FLGNML
  242                      
  243 !
  244 ! process old ww3_bounc.inp format
  245 !
  246       IF (.NOT. FLGNML) THEN
  247         OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_bounc.inp',STATUS='OLD',ERR=80>
  248         REWIND (NDSI)
  249                      
  250         READ (NDSI,'(A)',END=801,ERR=802,IOSTAT=IERR) COMSTR
  251         IF (COMSTR.EQ.' ') COMSTR = '$'
  252         WRITE (NDSO,901) COMSTR
  253                                
  254         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  255         READ (NDSI,*,END=801,ERR=802) INXOUT
  256         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  257         READ (NDSI,*,END=801,ERR=802) INTERP
  258         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  259         READ (NDSI,*,END=801,ERR=802) VERBOSE
  260         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  261 !
  262         NBO2 = 0
  263 !
  264 !       ILOOP = 1 to count NBO2
  265 !       ILOOP = 2 to read the file names
  266 !
  267         DO ILOOP = 1, 2
  268           OPEN (NDSS,FILE='ww3_bounc.scratch',FORM='FORMATTED',          &
  269                 status='UNKNOWN')
  270           IF ( ILOOP .EQ. 1 ) THEN
  271             NDSI2 = NDSI
  272           ELSE
  273             NDSI2 = NDSS
  274             ALLOCATE(SPECFILES(NBO2))
  275             NBO2=0
 
 
                                                        ww3_bounc.F90  page   6
 
 
  276           ENDIF
  277                
  278           NBO2=0
  279 !         Read input file names
  280           DO
  281             CALL NEXTLN ( COMSTR , NDSI2 , NDSE )
  282             READ (NDSI2,'(A512)') FILENAME
  283             JJ     = LEN_TRIM(FILENAME)
  284             IF ( ILOOP .EQ. 1 ) THEN
  285               BACKSPACE (NDSI)
  286               READ (NDSI,'(A)') LINE
  287               WRITE (NDSS,'(A)') LINE
  288             END IF
  289             IF (FILENAME(:JJ).EQ."'STOPSTRING'") EXIT
  290             NBO2=NBO2+1
  291             IF (ILOOP.EQ.1) CYCLE
  292             SPECFILES(NBO2)=FILENAME
  293           END DO
  294 !
  295           IF ( ILOOP .EQ. 1 ) CLOSE ( NDSS)
  296 !
  297           IF ( ILOOP .EQ. 2 ) CLOSE ( NDSS, STATUS='DELETE' )
  298         END DO ! ILOOP = 1, 2
  299         CLOSE(NDSI)
  300                    
  301       END IF ! .NOT. FLGNML
  302                            
  303                            
  304                            
  305 !
  306 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  307 ! 4. Tests the reading of the file
  308 !
  309       IF ( INXOUT.EQ.'READ') THEN
  310         OPEN(NDSB,FILE='nest.ww3',FORM='UNFORMATTED',status='old')
  311         READ(NDSB) IDTST, VERTEST, NK1, NTH1, XFR, FR1I, TH1I, NBI
  312         NSPEC1  = NK1 * NTH1
  313         IF ( IDTST .NE. IDSTRBC ) GOTO 803
  314         WRITE(NDSO,940) VERTEST
  315         WRITE(NDSO,941) IDTST
  316         IF (VERBOSE.EQ.1) WRITE(NDSO,'(A,2I5,3F12.6,I5)') 'NK,NTH,XFR, FR>
  317                     NK1,NTH1,XFR, FR1I, TH1I, NBI
  318         ALLOCATE (XBPI(NBI),YBPI(NBI))
  319         ALLOCATE (IPBPI(NBI,4),RDBPI(NBI,4))
  320         READ(NDSB) (XBPI(I),I=1,NBI),                                 &
  321                    (YBPI(I),I=1,NBI),                                 &
  322                    ((IPBPI(I,J),I=1,NBI),J=1,4),                      &
  323                    ((RDBPI(I,J),I=1,NBI),J=1,4)
  324         IF (VERBOSE.GE.1) WRITE(NDSO,*)      'XBPI:',XBPI
  325         IF (VERBOSE.GE.1) WRITE(NDSO,*)      'YBPI:',YBPI
  326         IF (VERBOSE.GE.1) WRITE(NDSO,*)      'IPBPI:'
  327         DO I=1,NBI
  328           IF (VERBOSE.GE.1) WRITE(NDSO,*) I,' interpolated from:',IPBPI(I>
  329           IF (VERBOSE.GE.1) WRITE(NDSO,*) I,' with coefficient :',RDBPI(I>
  330         END DO
 
 
                                                        ww3_bounc.F90  page   7
 
 
  331 !
  332         READ (NDSB) TIME2, NBI2
  333         BACKSPACE (NDSB)
  334         ALLOCATE (ABPIN(NSPEC1,NBI2))
  335         IERR=0
  336         DO WHILE (IERR.EQ.0)
  337           READ (NDSB,IOSTAT=IERR) TIME2, NBI2
  338           IF (IERR.EQ.0) THEN
  339             IF (VERBOSE.EQ.1) WRITE(NDSO,*)      'TIME2,NBI2:',TIME2, NBI>
  340             DO IP=1, NBI2
  341               READ (NDSB,END=803,ERR=804) ABPIN(:,IP)
  342             END DO
  343           END IF
  344         END DO
  345         CLOSE(NDSB)
  346       END IF ! INXOUT.EQ.'READ'
  347 !
  348       IF ( INXOUT.EQ.'WRITE') THEN
  349 !
  350 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  351 ! 5. Defines position of active boundary points
  352 !
  353         NBO = 0
  354         DO ISEA=1,NSEA
  355           IX     = MAPSF(ISEA,1)
  356           IY     = MAPSF(ISEA,2)
  357           IF (MAPSTA(IY,IX).EQ.2) THEN
  358             NBO=NBO+1
  359           END IF
  360         END DO
  361         ALLOCATE(XBPO(NBO),YBPO(NBO))
  362         ALLOCATE (IPBPO(NBO,4),RDBPO(NBO,4))
  363         IBO=0
  364         DO ISEA=1,NSEA
  365           IX     = MAPSF(ISEA,1)
  366           IY     = MAPSF(ISEA,2)
  367           IF (MAPSTA(IY,IX).EQ.2) THEN
  368             IBO=IBO+1
  369             SELECT CASE ( GTYPE )
  370             CASE ( RLGTYPE )
  371               XBPO(IBO)=X0+SX*(IX-1)
  372               YBPO(IBO)=Y0+SY*(IY-1)
  373             CASE ( CLGTYPE )
  374               XBPO(IBO)= XGRD(IY,IX)
  375               YBPO(IBO)= YGRD(IY,IX)
  376             CASE (UNGTYPE)
  377               XBPO(IBO)= XYB(IX,1)
  378               YBPO(IBO)= XYB(IX,2)
  379             END SELECT !GTYPE
  380           END IF
  381         END DO
  382 !
  383         OPEN(NDSB,FILE='nest.ww3',FORM='UNFORMATTED',status='unknown')
  384         ALLOCATE(DIMID(NBO2,3),DIMLN(NBO2,3),NCID(NBO2))
  385                                                         
 
 
                                                        ww3_bounc.F90  page   8
 
 
  386         ALLOCATE(LATS(NBO2),LONS(NBO2),STATION(16,NBO2))
  387                                                         
  388         DO IP=1,NBO2
  389           ! open file
  390           OPEN(NDSC,FILE=TRIM(SPECFILES(IP)),FORM='UNFORMATTED',      &
  391                status='old',iostat=ICODE)
  392           IF (ICODE.NE.0) THEN
  393             LONS(IP)=-999.
  394             LATS(IP)=-999.
  395             WRITE (NDSE,1010) TRIM(SPECFILES(IP))
  396             CALL EXTCDE ( 70 )
  397           END IF
  398                 
  399           IRET=NF90_OPEN(TRIM(SPECFILES(IP)),NF90_NOWRITE,NCID(IP))
  400           WRITE(6,*) 'Opening file:',TRIM(SPECFILES(IP))
  401           CALL CHECK_ERR(IRET)
  402                               
  403           ! dimensions
  404           IRET=NF90_INQ_DIMID(NCID(IP),'time',DIMID(IP,1))
  405           CALL CHECK_ERR(IRET)
  406           IRET=NF90_INQ_DIMID(NCID(IP),'frequency',DIMID(IP,2))
  407           CALL CHECK_ERR(IRET)
  408           IRET=NF90_INQ_DIMID(NCID(IP),'direction',DIMID(IP,3))
  409           CALL CHECK_ERR(IRET)
  410           IRET=NF90_INQUIRE_DIMENSION(NCID(IP),DIMID(IP,1),len=DIMLN(IP,1>
  411           CALL CHECK_ERR(IRET)
  412           IRET=NF90_INQUIRE_DIMENSION(NCID(IP),DIMID(IP,2),len=DIMLN(IP,2>
  413           CALL CHECK_ERR(IRET)
  414           IRET=NF90_INQUIRE_DIMENSION(NCID(IP),DIMID(IP,3),len=DIMLN(IP,3>
  415           CALL CHECK_ERR(IRET)
  416                               
  417           NTI=DIMLN(IP,1)
  418           NKI=DIMLN(IP,2)
  419           NTHI=DIMLN(IP,3)
  420                           
  421           IF (IP.EQ.1) THEN
  422             NT1=NTI
  423             NK1=NKI
  424             NTH1=NTHI
  425             NSPEC1  = NK1 * NTH1
  426             ALLOCATE(TIMES(NT1))
  427             ALLOCATE (FREQ(NK1),THETA(NTH1))
  428             ALLOCATE (SPEC2D(NTH1,NK1,NT1,NBO2))
  429             ALLOCATE (ABPIN2(NK*NTH,NT1,NBO2))
  430                                               
  431             ! instanciates time
  432             REFDATE(:)=0.
  433             IRET=NF90_INQ_VARID(NCID(IP),"time",VARID(1))
  434             CALL CHECK_ERR(IRET)
  435             IRET=NF90_GET_VAR(NCID(IP), VARID(1), TIMES(:))
  436             CALL CHECK_ERR(IRET)
  437             IRET=NF90_GET_ATT(NCID(IP),VARID(1),"calendar",CALENDAR)
  438             IF ( IRET/=NF90_NOERR ) THEN
  439               WRITE(NDSE,951)
  440             ELSE IF ((INDEX(CALENDAR, "standard").EQ.0) .AND. &
 
 
                                                        ww3_bounc.F90  page   9
 
 
  441                      (INDEX(CALENDAR, "gregorian").EQ.0)) THEN
  442               WRITE(NDSE,952)
  443             END IF
  444             IRET=NF90_GET_ATT(NCID(IP),VARID(1),"units",TIMEUNITS)
  445             CALL U2D(TIMEUNITS,REFDATE,IERR)
  446             CALL D2J(REFDATE,REFJULDAY,IERR)
  447                                             
  448           ELSE
  449             IF (NKI.NE.NK1.OR.NTHI.NE.NTH1.OR.NT1.NE.NTI &
  450                 ) GOTO 805
  451           END IF
  452                 
  453           ! position variables : lon/lat or x/y
  454           IF ( FLAGLL ) THEN
  455             IRET=NF90_INQ_VARID(NCID(IP), 'latitude', VARID(2))
  456             CALL CHECK_ERR(IRET)
  457             IRET=NF90_GET_VAR(NCID(IP), VARID(2), LATS(IP))
  458             CALL CHECK_ERR(IRET)
  459             IRET=NF90_INQ_VARID(NCID(IP), 'longitude', VARID(3))
  460             CALL CHECK_ERR(IRET)
  461             IRET=NF90_GET_VAR(NCID(IP), VARID(3), LONS(IP))
  462             CALL CHECK_ERR(IRET)
  463           ELSE
  464             IRET=NF90_INQ_VARID(NCID(IP), 'y', VARID(2))
  465             CALL CHECK_ERR(IRET)
  466             IRET=NF90_GET_VAR(NCID(IP), VARID(2), LATS(IP))
  467             CALL CHECK_ERR(IRET)
  468             IRET=NF90_INQ_VARID(NCID(IP), 'x', VARID(3))
  469             CALL CHECK_ERR(IRET)
  470             IRET=NF90_GET_VAR(NCID(IP), VARID(3), LONS(IP))
  471             CALL CHECK_ERR(IRET)
  472           END IF
  473                 
  474           ! freq and dir variables
  475           IRET=NF90_INQ_VARID(NCID(IP),"frequency",VARID(4))
  476           CALL CHECK_ERR(IRET)
  477           IRET=NF90_GET_VAR(NCID(IP),VARID(4),FREQ)
  478           CALL CHECK_ERR(IRET)
  479           IRET=NF90_INQ_VARID(NCID(IP),"direction",VARID(5))
  480           CALL CHECK_ERR(IRET)
  481           IRET=NF90_GET_VAR(NCID(IP),VARID(5),THETA)
  482           CALL CHECK_ERR(IRET)
  483           THETA=MOD(2.5*PI-(PI/180)*THETA,TPI)
  484                                               
  485           ! 2D spectra depending on station name or lat/lon
  486           IRET=NF90_INQ_VARID(NCID(IP),"efth",VARID(7))
  487           IF (IRET.NE.0) IRET=NF90_INQ_VARID(NCID(IP),"Efth",VARID(7))
  488           CALL CHECK_ERR(IRET)
  489           IRET=NF90_GET_ATT(NCID(IP),VARID(7),"scale_factor",FACTOR)
  490           IF (IRET.NE.0) FACTOR=1.
  491           IRET=NF90_GET_ATT(NCID(IP),VARID(7),"add_offset",OFFSET)
  492           IF (IRET.NE.0) OFFSET=0.
  493           IRET = NF90_INQ_VARID(NCID(IP), 'station_name', VARID(6))
  494           IF (IRET.NE.0) THEN
  495             ! efth(time, frequency, direction, latitude, longitude)
 
 
                                                        ww3_bounc.F90  page  10
 
 
  496             IRET=NF90_GET_VAR(NCID(IP),VARID(7),SPEC2D(:,:,:,IP),       &
  497                               start=(/1,1,1,1/),count=(/1,1,NTHI,NKI,NTI/>
  498             CALL CHECK_ERR(IRET)
  499           ELSE
  500             ! efth(time, station, frequency, direction)
  501             IRET=NF90_GET_VAR(NCID(IP),VARID(7),SPEC2D(:,:,:,IP),       &
  502                               start=(/1,1,1,1/),count=(/NTHI,NKI,1,NTI/))
  503             CALL CHECK_ERR(IRET)
  504           END IF
  505           ! apply scale_factor and add_offset
  506           SPEC2D(:,:,:,IP)=(SPEC2D(:,:,:,IP)*FACTOR)+OFFSET
  507                                                            
  508           ! close spectra file
  509           IRET=NF90_CLOSE(NCID(IP))
  510           CALL CHECK_ERR(IRET)
  511 !
  512           END DO ! IP=1,NBO2
  513                             
  514                             
  515                             
  516 !
  517 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  518 ! 6.  Checks on spectral discretization
  519 !     reminder: fr(NK)=fr1*XFR**(NK-1)
  520 !
  521         FR1I=FREQ(1)
  522         XFRI=EXP(ALOG(FREQ(NKI)/FREQ(1))/(NKI-1))
  523         TH1I=THETA(1)
  524                      
  525         SPCONV = NKI.NE.NK .OR. NTHI.NE.NTH .OR.                    &
  526                  ABS(XFRI/XFR-1.).GT.0.01 .OR.                      &
  527                  ABS(FR1I/FR1-1.).GT.0.01 .OR.                      &
  528                  ABS(TH1I-TH(1)).GT.0.01*DTH
  529                                             
  530          IF (VERBOSE.GE.1) WRITE(NDSO,*) 'SPCONV:', SPCONV, NKI, NK, NTHI>
  531 !
  532 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  533 ! 7. Loops on files and instanciate ABPIN2
  534 !
  535         IF ( .NOT. SPCONV ) THEN
  536                                 
  537           DO IP=1,NBO2
  538 ! Copies spectrum in frequency and direction ranges
  539             DO I=1,NK
  540               DO J=1,NTH
  541                 ABPIN2((I-1)*NTH+J,:,IP)=SPEC2D(J,I,:,IP)*tpiinv
  542                 END DO
  543               END DO
  544             END DO ! IP=1,NBO2
  545 !
  546         ELSE
  547           ALLOCATE(TMPSPCI(NKI*NTHI,NTI))
  548           ALLOCATE(TMPSPCO(NK*NTH,  NTI))
  549           DO IP=1,NBO2
  550             DO I=1,NKI
 
 
                                                        ww3_bounc.F90  page  11
 
 
  551               DO J=1,NTHI
  552                 TMPSPCI((I-1)*NTHI+J,:)=SPEC2D(J,I,:,IP)*tpiinv
  553                 END DO
  554               END DO
  555             CALL W3CSPC ( TMPSPCI, NKI, NTHI, XFRI, FR1I, TH1I, &
  556                           TMPSPCO, NK,  NTH,  XFR,  FR1,  TH(1),&
  557                           NTI, NDST, NDSE, FACHFE )
  558             ABPIN2(:,:,IP)=TMPSPCO(:,:)
  559             END DO
  560 !
  561           END IF
  562 !
  563 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  564 ! 8. Writes header
  565 !
  566 !       Writes header in nest.ww3 file
  567         WRITE(NDSB) IDSTRBC, VERBPTBC, NK, NTH, XFR, FR1,       &
  568         TH(1), NBO
  569         IPBPO(:,:)=1
  570         RDBPO(:,1)=1.
  571         RDBPO(:,2:4)=0.
  572                        
  573 !       Loops on points
  574         DO IP1=1,NBO
  575           DMIN=360.+180.
  576           DMIN2=360.+180.
  577 !         Loops on files
  578           DO IP=1,NBO2
  579 !           Searches for the nearest 2 points where spectra are available
  580             IF (FLAGLL)  THEN
  581               DIST=DIST_SPHERE ( LONS(IP),LATS(IP),XBPO(IP1),YBPO(IP1) )
  582             ELSE
  583               DIST=SQRT((LONS(IP)-XBPO(IP1))**2+(LATS(IP)-YBPO(IP1))**2)
  584               END IF
  585             IF (DMIN.EQ.(360.+180.)) THEN
  586               IF(DIST.LT.DMIN) THEN
  587                 IPBPO(IP1,1)=IP
  588                 DMIN=DIST
  589               END IF
  590             ELSE
  591               IF(DIST.LT.DMIN2) THEN
  592                 IF(DIST.LT.DMIN) THEN
  593                   IPBPO(IP1,2)=IPBPO(IP1,1)
  594                   DMIN2=DMIN
  595                   IPBPO(IP1,1)=IP
  596                   DMIN=DIST
  597                 ELSE
  598                   IPBPO(IP1,2)=IP
  599                   DMIN2=DIST
  600                 END IF
  601               END IF
  602             END IF
  603           END DO ! IP1=1,NBO2
  604           IF (VERBOSE.GE.1) WRITE(NDSO,*) 'DIST:',DMIN,DMIN2,IP1,IPBPO(IP>
  605                                           LONS(IPBPO(IP1,1)),LONS(IPBPO(I>
 
 
                                                        ww3_bounc.F90  page  12
 
 
  606                                           LATS(IPBPO(IP1,1)),LATS(IPBPO(I>
  607                                                                           
  608                                                                           
  609 !
  610 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  611 ! 9. Computes linear interpolation coefficient between the nearest 2 poin>
  612 !
  613           IF (INTERP.GT.1.AND.NBO2.GT.1) THEN
  614             IF (FLAGLL) THEN
  615               DLON=LONS(IPBPO(IP1,2))-LONS(IPBPO(IP1,1))
  616               DLAT=LATS(IPBPO(IP1,2))-LATS(IPBPO(IP1,1))
  617               DLO=XBPO(IP1)-LONS(IPBPO(IP1,1))
  618               IF (DLON.GT.180.) DLON=DLON-360
  619               IF (DLON.LT.-180.) DLON=DLON+360
  620               IF (DLO.GT.180.) DLO=DLO-360
  621               IF (DLO.LT.-180.) DLO=DLO+360
  622               DIST=SQRT(DLON**2+DLAT**2)
  623               COS1=( DLO*DLON &
  624                   + (YBPO(IP1)-LATS(IPBPO(IP1,1)))  &
  625                   *DLAT )/(DIST**2)
  626             ELSE
  627               DIST=SQRT((LONS(IPBPO(IP1,1))-LONS(IPBPO(IP1,2)))**2   &
  628                +(LATS(IPBPO(IP1,1))-LATS(IPBPO(IP1,2)))**2)
  629               COS1=( (XBPO(IP1)-LONS(IPBPO(IP1,1)))  &
  630                   *(LONS(IPBPO(IP1,2))-LONS(IPBPO(IP1,1))) &
  631                   + (YBPO(IP1)-LATS(IPBPO(IP1,1)))  &
  632                   *(LATS(IPBPO(IP1,2))-LATS(IPBPO(IP1,1))) )/(DIST**2)
  633             END IF
  634             !COS2=( (XBPO(IP1)-LONS(IPBPO(IP1,2)))  &
  635             !      *(LONS(IPBPO(IP1,1))-LONS(IPBPO(IP1,2)))
  636             !      + (YBPO(IP1)-LATS(IPBPO(IP1,2)))  &
  637             !      *(LATS(IPBPO(IP1,1))-LATS(IPBPO(IP1,2))))/(DIST**2)
  638             RDBPO(IP1,1)=1-MIN(1.,MAX(0.,COS1))
  639             RDBPO(IP1,2)=MIN(1.,MAX(0.,COS1))
  640           ELSE
  641             ! in this case: nearest point
  642             RDBPO(IP1,1)=1.
  643             RDBPO(IP1,2:4)=0.
  644           END IF
  645           IF (VERBOSE.GE.1) WRITE(NDSO,*) 'IPBP:',IP1,(IPBPO(IP1,J),J=1,4)
  646           IF (VERBOSE.GE.1) WRITE(NDSO,*) 'RDBP:',IP1,(RDBPO(IP1,J),J=1,4)
  647           !IF (VERBOSE.GE.1) WRITE(NDSO,*) 'RDBP:',COS1,DIST,DLON,DLO,DLA>
  648         END DO ! IP1=1,NBO
  649                           
  650         WRITE(NDSB)  (XBPO(I),I=1,NBO),            &
  651                      (YBPO(I),I=1,NBO),            &
  652                      ((IPBPO(I,J),I=1,NBO),J=1,4), &
  653                      ((RDBPO(I,J),I=1,NBO),J=1,4)
  654                                                  
  655                                                  
  656 !
  657 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  658 ! 10. Loops on times and files and write to nest.ww3
  659 !
  660         DO IT=1,NT1
 
 
                                                        ww3_bounc.F90  page  13
 
 
  661           CURJULDAY=TIMES(IT)
  662           IF (INDEX(TIMEUNITS, "seconds").NE.0)   CURJULDAY=CURJULDAY/864>
  663           IF (INDEX(TIMEUNITS, "minutes").NE.0)   CURJULDAY=CURJULDAY/144>
  664           IF (INDEX(TIMEUNITS, "hours").NE.0)     CURJULDAY=CURJULDAY/24.
  665           CURJULDAY=REFJULDAY+CURJULDAY
  666                                        
  667           ! convert julday to date and time
  668           CALL J2D(CURJULDAY,CURDATE,IERR)
  669           CALL D2T(CURDATE,TIME,IERR)
  670                                      
  671           ! write to output file nest.ww3
  672           WRITE(NDSO,'(A,2I9,A,I6,A,G16.5)') 'Writing boundary data for t>
  673             TIME, ' at ',NBO2,' points. Max.: ', MAXVAL(ABPIN2(:,IT,:))
  674           WRITE(NDSB,IOSTAT=IERR) TIME, NBO2
  675           DO IP=1, NBO2
  676             WRITE(NDSB) ABPIN2(:,IT,IP)
  677           END DO
  678         END DO ! IT=0,NT1
  679         CLOSE(NDSB)
  680                    
  681       END IF ! INXOUT.EQ.'WRITE'
  682                                 
  683       GOTO 888
  684               
  685 !
  686 ! Escape locations read errors :
  687 !
  688  
  689  801 CONTINUE
  690       WRITE (NDSE,1001)
  691       CALL EXTCDE ( 61 )
  692 !
  693  802 CONTINUE
  694       WRITE (NDSE,1002) IERR
  695       CALL EXTCDE ( 62 )
  696 !
  697  803 CONTINUE
  698       WRITE (NDSE,1003) IDTST, IDSTRBC
  699       CALL EXTCDE ( 63 )
  700 !
  701  804 CONTINUE
  702       WRITE (NDSE,1004)
  703       CALL EXTCDE ( 64 )
  704 !
  705  805 CONTINUE
  706       WRITE (NDSE,1005) TRIM(SPECFILES(IP)), NKI, NK1, NTHI, NTH1, NTI, N>
  707       CALL EXTCDE ( 65 )
  708 !
  709  809 CONTINUE
  710       WRITE (NDSE,1009) FILE, IERR
  711       CALL EXTCDE ( 69 )
  712 !
  713   888 CONTINUE
  714       WRITE (NDSO,999)
  715                       
 
 
                                                        ww3_bounc.F90  page  14
 
 
  716                       
  717 !
  718 ! Formats
  719 !
  720   900 FORMAT (/15X,'   *** WAVEWATCH III Bounday input prep. ***   '/ &
  721                15X,'==============================================='/)
  722 !
  723   901 FORMAT ( '  Comment character is ''',A,''''/)
  724 !
  725   920 FORMAT ( '  Grid name : ',A/)
  726 !
  727   940 FORMAT ( '  Format version : ',A/)
  728 !
  729   941 FORMAT ( '  File type : ',A/)
  730 !
  731   951 FORMAT (/' *** WAVEWATCH III WARNING IN W3BOUNC : '/            &
  732                '     CALENDAR ATTRIBUTE NOT DEFINED'/                 &
  733                '     IT MUST RESPECT STANDARD OR GREGORIAN CALENDAR')
  734 !
  735   952 FORMAT (/' *** WAVEWATCH III WARNING IN W3BOUNC : '/            &
  736                '     CALENDAR ATTRIBUTE NOT MATCH'/                   &
  737                '     IT MUST RESPECT STANDARD OR GREGORIAN CALENDAR')
  738 !
  739   999 FORMAT (/'  End of program '/                                   &
  740                ' ========================================='/          &
  741                '         WAVEWATCH III Boundary input '/)
  742 !
  743  1001 FORMAT (/' *** WAVEWATCH-III ERROR IN W3BOUNC : '/              &
  744                '     PREMATURE END OF INPUT FILE'/)
  745 !
  746  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUNC: '/               &
  747                '     ERROR IN READING ',A,' FROM INPUT FILE'/         &
  748                '     IOSTAT =',I5/)
  749 !
  750  1003 FORMAT (/' *** WAVEWATCH-III ERROR IN W3IOBC :'/                &
  751                '     ILLEGAL IDSTR, READ : ',A/                       &
  752                '                   CHECK : ',A/)
  753 !
  754  1004 FORMAT (/' *** WAVEWATCH-III ERROR IN W3BOUNC : '/              &
  755                '     PREMATURE END OF NEST FILE'/)
  756 !
  757  1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUNC: '/               &
  758                '     INCONSISTENT SPECTRAL DIMENSION FOR FILE ',A/    &
  759                '     NKI =',I3,' DIFFERS FROM NK1 =',I3/      &
  760                '     OR NTHI =',I3,' DIFFERS FROM NTH1 =',I3/ &
  761                '     OR NTI =',I5,' DIFFERS FROM NT1 =',I5 /)
  762 !
  763  1009 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUNC : '/              &
  764                '     ERROR IN OPENING SPEC FILE: ', A/                &
  765                '     IOSTAT =',I5/)
  766 !
  767  1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUNC : '/              &
  768                '     SPEC FILE DOES NOT EXIST : ',A/)
  769 !
  770 !/
 
 
                                                        ww3_bounc.F90  page  15
 
 
  771 !/ End of W3BOUNC ---------------------------------------------------- /
  772 !/
  773       END PROGRAM W3BOUNC
  774 !/ ------------------------------------------------------------------- /
  775                                                                         
  776                                                                         
  777 !========================================================================>
  778                                                                           
  779       SUBROUTINE CHECK_ERR(IRET)
  780                                 
  781       USE NETCDF
  782       USE W3ODATMD, ONLY: NDSE
  783       USE W3SERVMD, ONLY: EXTCDE
  784                                 
  785       IMPLICIT NONE
  786                    
  787       INTEGER IRET
  788                   
  789       IF (IRET .NE. NF90_NOERR) THEN
  790         WRITE(NDSE,*) ' *** WAVEWATCH III ERROR IN BOUNC :'
  791         WRITE(NDSE,*) ' NETCDF ERROR MESSAGE: '
  792         WRITE(NDSE,*) NF90_STRERROR(IRET)
  793         CALL EXTCDE ( 59 )
  794       END IF
  795       RETURN
  796             
  797       END SUBROUTINE CHECK_ERR
  798                               
  799 !========================================================================>
  800                                                                           
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/opt/cray/netcdf-hdf5parallel/4.2.0/pgi/119/include -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
