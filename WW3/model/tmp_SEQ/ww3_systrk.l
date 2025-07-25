                                                       ww3_systrk.F90  page   1
 
 
    1 !/ ------------------------------------------------------------------- /
    2       PROGRAM WW3_SYSTRK
    3 !/
    4 !/                  +-----------------------------------+
    5 !/                  | WAVEWATCH III           NOAA/NCEP |
    6 !/                  |     A. J. van der Westhuysen      |
    7 !/                  |            Jeff Hanson            |
    8 !/                  |        Eve-Marie Devaliere        |
    9 !/                  |                        FORTRAN 95 |
   10 !/                  | Last update :         16-Jan-2017 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
   14 !/                  by Jeff Hanson & Eve-Marie Devaliere
   15 !/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
   16 !/    29-Nov-2013 : Remove DOC control characters,
   17 !/                  update MPI! to MPI/! (H.L. Tolman). ( version 4.15 )
   18 !/    11-Feb-2014 : Add NetCDF output option. Both NetCDF-3 and
   19 !/                  NetCDF-4 are available. (B. Li).    ( version 4.18 )
   20 !/    26-Sep-2016 : Optimization updates (A. van der Westhuysen)
   21 !/                                                      ( version 5.15 )
   22 !/    20-Sep-2016 : Add support for unformatted partition file.
   23 !/                  (S.Zieger BoM, Australia)           ( version 5.16 )
   24 !/    20-Dec-2016 : Optimized search algorithms and
   25 !/                  set functions. (S.Zieger)           ( version 5.16 )
   26 !/
   27 !/    Copyright 2009-2013 National Weather Service (NWS),
   28 !/       National Oceanic and Atmospheric Administration.  All rights
   29 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   30 !/       No unauthorized use without permission.
   31 !/
   32       USE W3STRKMD
   33       USE W3TIMEMD, ONLY: TDIFF
   34       IMPLICIT NONE
   35 !
   36 !  1. Purpose :
   37 !
   38 !     Perform spatial and temporal tracking of wave systems, based
   39 !     on spectral partition (bulletin) output.
   40 !
   41 !  2. Method :
   42 !
   43 !     This is a controller program. It reads the input parameter file
   44 !     ww3_systrk.inp and calls subroutine waveTracking_NWS_V2 to
   45 !     perform the actual tracking procedure. Write output (fields and
   46 !     point output).
   47 !
   48 !  3. Parameters :
   49 !
   50       LOGICAL      :: testout
   51       PARAMETER (testout = .FALSE.)
   52       CHARACTER    :: filename*80, paramFile*32
   53       REAL         :: dirKnob, perKnob, hsKnob, wetPts, seedLat, &
   54                       seedLon, dirTimeKnob, tpTimeKnob, tint
   55       REAL         :: lonout(100), latout(100)                           >
 
 
                                                       ww3_systrk.F90  page   2
 
 
   56       INTEGER      :: maxGroup, ntint, noutp
   57       INTEGER      :: CLKDT0(8),CLKDT1(8)
   58       REAL         :: CLKFEL
   59       TYPE(dat2d), POINTER :: wsdat(:)
   60       TYPE(timsys), POINTER :: sysA(:)
   61       INTEGER, POINTER :: maxSys(:)
   62 !
   63 !     Local parameters.
   64 !     ----------------------------------------------------------------
   65 !     intype         Int       input  Type of input (0 = from memory; 1 =>
   66 !     tmax           Int       input  Value of maxTs to apply (1 or 2, us>
   67 !     tcur           Int       input  Index of current time step (1 or 2,>
   68 !     ulimGroup      Int       input  Upper limit of number of wave syste>
   69 !
   70       LOGICAL           :: file_exists
   71       CHARACTER         :: inpstr*72
   72       INTEGER           :: intype, tmax, tcur, maxI, maxJ
   73       INTEGER           :: it, igrp, sysmatch, ind, ip
   74       INTEGER           :: i, j, leng, ulimGroup
   75       REAL, ALLOCATABLE :: dum(:,:)
   76       REAL, ALLOCATABLE :: dum2nc(:,:,:,:)
   77       REAL, ALLOCATABLE ::  hsprt_nc(:,:,:)
   78       REAL, ALLOCATABLE ::  tpprt_nc(:,:,:)
   79       REAL, ALLOCATABLE ::  dirprt_nc(:,:,:)
   80       REAL, ALLOCATABLE ::  longitude_nc(:),latitude_nc(:)
   81       REAL, ALLOCATABLE ::  lonprt_nc(:),latprt_nc(:)
   82       INTEGER NTIME_NC
   83       INTEGER           :: outputType
   84       LOGICAL           :: outputCheck1,outputCheck2
   85       DOUBLE PRECISION  :: date1, date2, tstart, tend
   86       REAL              :: dlon, dlat, lonprt, latprt
   87       REAL              :: dt
   88       REAL              :: minlon, maxlon, minlat, maxlat
   89       INTEGER           :: mxcwt, mycwt
   90                                        
   91 !     For point output (bilinear interpolation)
   92       REAL :: hsprt(10),tpprt(10),dirprt(10)
   93       REAL :: BL_hsprt(10),BR_hsprt(10),TR_hsprt(10),TL_hsprt(10), &
   94               BL_tpprt(10),BR_tpprt(10),TR_tpprt(10),TL_tpprt(10), &
   95               BL_dirprt(10),BR_dirprt(10),TR_dirprt(10),TL_dirprt(10)
   96       REAL :: BL_dirx,BR_dirx,TR_dirx,TL_dirx, &
   97               BL_diry,BR_diry,TR_diry,TL_diry
   98       REAL :: BL_lonprt,BR_lonprt,TR_lonprt,TL_lonprt, &
   99               BL_latprt,BR_latprt,TR_latprt,TL_latprt
  100       REAL :: t, u, BL_W, BR_W, TR_W, TL_W
  101       REAL      :: PI
  102       PARAMETER  (PI = 3.1416)
  103 !
  104 !  4. Subroutines used :
  105 !
  106 !     waveTracking_NWS_V2
  107 !
  108 !  5. Called by :
  109 !
  110 !     None, stand-alone program.
 
 
                                                       ww3_systrk.F90  page   3
 
 
  111 !
  112 !  6. Error messages :
  113 !
  114 !  7. Remarks :
  115 !
  116 !  8. Structure :
  117 !
  118 !     Calls subroutine waveTracking_NWS_V2 in trackmd.95 - see that
  119 !     file for structure.
  120 !
  121 !  9. Switches :
  122 !
  123 !       !/SHRD  Switch for shared / distributed memory architecture.
  124 !       !/MPI   Id.
  125 !
  126 ! 10. Source code :
  127 !
  128 !/ ------------------------------------------------------------------- /
  129 !
  130 !     Open log file
  131       OPEN(unit=20,file='sys_log.ww3',status='unknown')
  132                                                        
  133 !     Print code version
  134          WRITE(6,900)
  135       WRITE(20,900)
  136   900 FORMAT (/15X,'    *** WAVEWATCH III Wave system tracking ***  '/ &
  137                15X,'==============================================='/)
  138                                                                       
  139 !     Since this program reads the raw partitioning input from file,
  140 !     we set intype=1 or 2, and tmax and tcur to dummy values (not used).
  141       intype = 2
  142 !      intype = 1
  143       IF (intype.EQ.1) WRITE(6,*) &
  144          '*** WARNING: partRes format input used!'
  145       tmax = 0
  146       tcur = 0
  147               
  148 !     Read input parameter file
  149       INQUIRE(FILE='ww3_systrk.inp', EXIST=file_exists)
  150       IF (.NOT.file_exists) THEN
  151          WRITE(20,2000)
  152          WRITE(6,2000)
  153          CALL ABORT
  154       END IF
  155       OPEN(unit=10,file='ww3_systrk.inp',status='old')
  156                                                       
  157       READ(10,'(A72)') inpstr
  158       DO WHILE (inpstr(1:1).EQ.'$')
  159          READ(10,'(A72)') inpstr
  160       END DO
  161       BACKSPACE(10)
  162       READ(10,*) filename
  163                          
  164       READ(10,'(A72)') inpstr
  165       DO WHILE (inpstr(1:1).EQ.'$')
 
 
                                                       ww3_systrk.F90  page   4
 
 
  166          READ(10,'(A72)') inpstr
  167       END DO
  168       BACKSPACE(10)
  169       READ(10,*) date1, date2, dt, ntint
  170       tstart = date1 + date2/1000000
  171                                     
  172       READ(10,'(A72)') inpstr
  173       DO WHILE (inpstr(1:1).EQ.'$')
  174          READ(10,'(A72)') inpstr
  175       END DO
  176       BACKSPACE(10)
  177       READ(10,*) outputType
  178                            
  179       !Check for correct outputType option:
  180       IF (outputType.EQ.1) THEN
  181          !ASCII output
  182       ELSEIF (outputType.EQ.3) THEN
  183          !NetCDF 3 - requrires !/TRKNC switch
  184          outputCheck1 = .TRUE.
  185      outputCheck1 = .FALSE.
  186          IF(outputCheck1)  THEN
  187             WRITE(6,993)
  188             STOP
  189          END IF
  190       ELSEIF (outputType.EQ.4) THEN
  191          !NetCDF 4 - requrires !/TRKNC and !/NC4 switch
  192          outputCheck1 = .TRUE.
  193          outputCheck2 = .TRUE.
  194      outputCheck1 = .FALSE.
  195        outputCheck2 = .FALSE.
  196          IF(outputCheck1.OR.outputCheck2) THEN
  197             WRITE(6,994)
  198             STOP
  199          END IF
  200       ELSE
  201          !Not a valid outputType
  202          WRITE(6,995) outputType
  203          STOP
  204       ENDIF
  205            
  206       READ(10,'(A72)') inpstr
  207       DO WHILE (inpstr(1:1).EQ.'$')
  208          READ(10,'(A72)') inpstr
  209       END DO
  210       BACKSPACE(10)
  211       READ(10,*) minlon, maxlon, mxcwt
  212                                       
  213       READ(10,'(A72)') inpstr
  214       DO WHILE (inpstr(1:1).EQ.'$')
  215          READ(10,'(A72)') inpstr
  216       END DO
  217       BACKSPACE(10)
  218       READ(10,*) minlat, maxlat, mycwt
  219                                       
  220       READ(10,'(A72)') inpstr
 
 
                                                       ww3_systrk.F90  page   5
 
 
  221       DO WHILE (inpstr(1:1).EQ.'$')
  222          READ(10,'(A72)') inpstr
  223       END DO
  224       BACKSPACE(10)
  225       READ(10,*) dirKnob, perKnob, hsKnob, wetPts, &
  226                 dirTimeKnob, tpTimeKnob
  227                                        
  228       READ(10,'(A72)') inpstr
  229       DO WHILE (inpstr(1:1).EQ.'$')
  230          READ(10,'(A72)') inpstr
  231       END DO
  232       BACKSPACE(10)
  233       READ(10,*) seedLat, seedLon
  234                                  
  235       READ(10,'(A72)') inpstr
  236       DO WHILE (inpstr(1:1).EQ.'$')
  237          READ(10,'(A72)') inpstr
  238       END DO
  239       BACKSPACE(10)
  240       noutp = 1
  241       lonout(:) = 9999.
  242       latout(:) = 9999.
  243       DO WHILE (.TRUE.)
  244          READ(10,*) lonout(noutp),latout(noutp)
  245          IF ((lonout(noutp).EQ.0.).AND.(latout(noutp).EQ.0.)) EXIT
  246          noutp = noutp + 1
  247       END DO
  248       noutp = noutp - 1
  249                        
  250       CLOSE(10)
  251                
  252       WRITE(20,*) 'Raw partition file = ',filename
  253       WRITE(20,'(A,F15.6)') 'Start time = ',tstart
  254       WRITE(20,*) 'dt = ',dt
  255       WRITE(20,*) 'No. time levels = ',ntint
  256       WRITE(20,'(A,2F7.2)') 'Domain limits: Longitude =',minlon, maxlon
  257       WRITE(20,'(A,2F7.2)') '               Latitude  =',minlat, maxlat
  258       WRITE(20,*) 'No. increments: Long, Lat  =',mxcwt, mycwt
  259       WRITE(20,*) 'dirKnob, perKnob, hsKnob, wetPts, &
  260                 dirTimeKnob, tpTimeKnob, seedLat, seedLon ='
  261       WRITE(20,'(8F6.2)') dirKnob, perKnob, hsKnob, wetPts, &
  262                 dirTimeKnob, tpTimeKnob, seedLat, seedLon
  263       WRITE(20,*) 'No. output points =',noutp
  264       DO i = 1,noutp
  265          WRITE(20,*) lonout(i), latout(i)
  266       END DO
  267             
  268       INQUIRE(FILE=filename, EXIST=file_exists)
  269       IF (.NOT.file_exists) THEN
  270          WRITE(20,2200) filename
  271          WRITE(6,2200) filename
  272          CALL EXIT(1)
  273       END IF
  274             
  275             
 
 
                                                       ww3_systrk.F90  page   6
 
 
  276          CALL DATE_AND_TIME ( VALUES=CLKDT0 )
  277                                              
  278                                              
  279                                              
  280                                              
  281       CALL waveTracking_NWS_V2 (intype     ,tmax       , &
  282                                 tcur       ,filename   , &
  283                                 tstart     ,tend       , &
  284                                 dt         ,ntint      , &
  285                                 minlon     ,maxlon     , &
  286                                 minlat     ,maxlat     , &
  287                                 mxcwt      ,mycwt      , &
  288                                 dirKnob    ,             &
  289                                 perKnob    ,hsKnob     , &
  290                                 wetPts     ,seedLat    , &
  291                                 seedLon    ,dirTimeKnob, &
  292                                 tpTimeKnob ,paramFile  , &
  293                                 sysA       ,wsdat      , &
  294                                 maxSys     ,maxGroup   )
  295                                                         
  296                                                         
  297          CALL DATE_AND_TIME ( VALUES=CLKDT1 )
  298          CLKFEL = TDIFF ( CLKDT0,CLKDT1 )
  299          WRITE (6,998) CLKFEL
  300          WRITE (6,*) 'Final system output...'
  301                                              
  302 !     Set upper limit for wave systems to output (limited by AWIPS displa>
  303       ulimGroup = 9
  304                    
  305 !-----Output systems as plain text----------------------------------------
  306                                                                           
  307       maxI = SIZE(wsdat(1)%lon,1)
  308       maxJ = SIZE(wsdat(1)%lon,2)
  309       dlon = wsdat(1)%lon(2,2)-wsdat(1)%lon(1,1)
  310       dlat = wsdat(1)%lat(2,2)-wsdat(1)%lat(1,1)
  311       WRITE(20,*) 'dlon, dlat =',dlon,dlat
  312                                           
  313 !-----Final SYSTEM output: Coordinates
  314       OPEN(unit=21,file='sys_coord.ww3', status='unknown')
  315                                                           
  316       WRITE(21,'(I6,69X,A)') maxJ,'Number of rows'
  317       WRITE(21,'(I6,69X,A)') maxI,'Number of cols'
  318       ALLOCATE( longitude_nc(maxI) )
  319       ALLOCATE( latitude_nc(maxJ) )
  320                                    
  321       WRITE(21,*) 'Longitude ='
  322       DO j = maxJ,1,-1
  323          DO i = 1,maxI
  324             WRITE(21,'(F7.2)',ADVANCE='NO') wsdat(1)%lon(i,j)
  325         longitude_nc(i)=wsdat(1)%lon(i,1)
  326          END DO
  327          WRITE(21,'(A)',ADVANCE='YES') ''
  328       END DO
  329             
  330       WRITE(21,*) 'Latitude = '
 
 
                                                       ww3_systrk.F90  page   7
 
 
  331       DO j = maxJ,1,-1
  332          DO i = 1,maxI
  333             WRITE(21,'(F7.2)',ADVANCE='NO') wsdat(1)%lat(i,j)
  334         latitude_nc(j)=wsdat(1)%lat(1,j)
  335          END DO
  336          WRITE(21,'(A)',ADVANCE='YES') ''
  337       END DO
  338             
  339       CLOSE(21)
  340                
  341 !-----Final SYSTEM output: hs
  342       IF(outputType == 1) THEN
  343       OPEN(unit=22,file='sys_hs.ww3', status='unknown')
  344                                                        
  345       WRITE(22,'(I6,69X,A)') maxJ,'Number of rows'
  346       WRITE(22,'(I6,69X,A)') maxI,'Number of cols'
  347       ENDIF
  348            
  349       NTIME_NC=SIZE(sysA)
  350       ALLOCATE( dum(maxI,maxJ) )
  351       IF(outputType == 3 .OR. outputType == 4) THEN
  352       ALLOCATE( dum2nc(maxI,maxJ,maxGroup,NTIME_NC) )
  353       ENDIF
  354            
  355       DO it = 1,SIZE(sysA)
  356 !        Loop through identified groups, limiting the output in file to u>
  357          IF(outputType == 1) THEN
  358             WRITE(22,'(F15.6,60x,A)') wsdat(it)%date,'Time'
  359             WRITE(22,'(I6,69x,A)') MIN(ulimGroup,maxGroup), &
  360                'Tot number of systems'
  361          ENDIF
  362          DO igrp = 1,MIN(ulimGroup,maxGroup)
  363             dum(1:maxI,1:maxJ) = 9999.00
  364 !           Find system with this group tag
  365             sysmatch = 1
  366             DO WHILE (sysmatch.LE.maxSys(it))
  367                IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
  368                sysmatch = sysmatch+1
  369             END DO
  370             IF (sysmatch.LE.maxSys(it)) THEN
  371 !              Match found: fill the output matrix with this data
  372                leng = sysA(it)%sys(sysmatch)%nPoints
  373                DO ind = 1, leng
  374                   dum(sysA(it)%sys(sysmatch)%i(ind), &
  375                       sysA(it)%sys(sysmatch)%j(ind)) = &
  376                       sysA(it)%sys(sysmatch)%hs(ind)
  377                END DO
  378             ELSE
  379                leng = 0
  380             END IF
  381                   
  382       IF(outputType == 1) THEN
  383             WRITE(22,'(I6,69x,A)') igrp,'System number'
  384             WRITE(22,'(I6,69x,A)') leng,'Number of points in system'
  385                                                                     
 
 
                                                       ww3_systrk.F90  page   8
 
 
  386             DO J = maxJ,1,-1
  387                DO i = 1,maxI
  388                   WRITE(22,'(F8.2)',ADVANCE='NO') dum(i,j)
  389                END DO
  390                WRITE(22,'(A)',ADVANCE='YES') ''
  391             END DO
  392       ELSE
  393             DO J = maxJ,1,-1
  394                DO i = 1,maxI
  395                dum2nc(i,j,igrp,it)=dum(i,j)
  396                END DO
  397             END DO
  398       ENDIF
  399            
  400          END DO
  401       END DO
  402             
  403       IF(outputType == 3 .OR. outputType == 4 ) THEN
  404       call t2netcdf(longitude_nc,latitude_nc,dum2nc,maxI,maxJ,&
  405          maxGroup,date1,date2,dt,NTIME_NC,1,outputType)
  406       ENDIF
  407            
  408       IF(outputType.EQ.1) CLOSE(22)
  409                                    
  410 !-----Final SYSTEM output: tp
  411       IF(outputType == 1) THEN
  412       OPEN(unit=23,file='sys_tp.ww3',status='unknown')
  413                                                       
  414       WRITE(23,'(I6,69X,A)') maxJ,'Number of rows'
  415       WRITE(23,'(I6,69X,A)') maxI,'Number of cols'
  416       ENDIF
  417            
  418       DO it = 1,SIZE(sysA)
  419 !        Loop through identified groups, limiting the output in file to u>
  420          IF(outputType == 1) THEN
  421             WRITE(23,'(F15.6,60x,A)') wsdat(it)%date,'Time'
  422             WRITE(23,'(I6,69X,A)') MIN(ulimGroup,maxGroup), &
  423                'Tot number of systems'
  424          ENDIF
  425          DO igrp = 1,MIN(ulimGroup,maxGroup)
  426             dum(1:maxI,1:maxJ) = 9999.00
  427 !           Find system with this group tag
  428             sysmatch = 1
  429             DO WHILE (sysmatch.LE.maxSys(it))
  430                IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
  431                sysmatch = sysmatch+1
  432             END DO
  433             IF (sysmatch.LE.maxSys(it)) THEN
  434 !              Match found: fill the output matrix with this data
  435                leng = sysA(it)%sys(sysmatch)%nPoints
  436                DO ind = 1, leng
  437                   dum(sysA(it)%sys(sysmatch)%i(ind), &
  438                       sysA(it)%sys(sysmatch)%j(ind)) = &
  439                       sysA(it)%sys(sysmatch)%tp(ind)
  440                END DO
 
 
                                                       ww3_systrk.F90  page   9
 
 
  441             ELSE
  442                leng = 0
  443             END IF
  444                   
  445       IF(outputType == 1) THEN
  446             WRITE(23,'(I6,69X,A)') igrp,'System number'
  447             WRITE(23,'(I6,69X,A)') leng,'Number of points in system'
  448             DO J = maxJ,1,-1
  449                DO i = 1,maxI
  450                   WRITE(23,'(F8.2)',ADVANCE='NO') dum(i,j)
  451                END DO
  452                WRITE(23,'(A)',ADVANCE='YES') ''
  453             END DO
  454       ELSE
  455           
  456             DO J = maxJ,1,-1
  457                DO i = 1,maxI
  458                dum2nc(i,j,igrp,it)=dum(i,j)
  459             END DO
  460             END DO
  461       ENDIF
  462            
  463          END DO
  464       END DO
  465             
  466       IF(outputType.EQ.3 .OR. outputType.EQ. 4 ) THEN
  467       call t2netcdf(longitude_nc,latitude_nc,dum2nc,maxI,maxJ,&
  468          maxGroup,date1,date2,dt,NTIME_NC,2,outputType)
  469       ENDIF
  470            
  471       IF(outputType.EQ.1) CLOSE(23)
  472                                    
  473 !-----Final SYSTEM output: dir
  474       IF(outputType == 1) THEN
  475       OPEN(unit=24,file='sys_dir.ww3',status='unknown')
  476                                                        
  477       WRITE(24,'(I6,69X,A)') maxJ,'Number of rows'
  478       WRITE(24,'(I6,69X,A)') maxI,'Number of cols'
  479       ENDIF
  480            
  481       DO it = 1,SIZE(sysA)
  482 !        Loop through identified groups, limiting the output in file to
  483 !        ulimGroup
  484          IF(outputType == 1) THEN
  485              WRITE(24,'(F15.6,60x,A)') wsdat(it)%date,'Time'
  486              WRITE(24,'(I6,69X,A)') MIN(ulimGroup,maxGroup), &
  487                'Tot number of systems'
  488          ENDIF
  489          DO igrp = 1,MIN(ulimGroup,maxGroup)
  490             dum(1:maxI,1:maxJ) = 9999.00
  491 !           Find system with this group tag
  492             sysmatch = 1
  493             DO WHILE (sysmatch.LE.maxSys(it))
  494                IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
  495                sysmatch = sysmatch+1
 
 
                                                       ww3_systrk.F90  page  10
 
 
  496             END DO
  497             IF (sysmatch.LE.maxSys(it)) THEN
  498 !              Match found: fill the output matrix with this data
  499                leng = sysA(it)%sys(sysmatch)%nPoints
  500                DO ind = 1, leng
  501                   dum(sysA(it)%sys(sysmatch)%i(ind), &
  502                       sysA(it)%sys(sysmatch)%j(ind)) = &
  503                       sysA(it)%sys(sysmatch)%dir(ind)
  504                END DO
  505             ELSE
  506                leng = 0
  507             END IF
  508                   
  509       IF(outputType == 1) THEN
  510             WRITE(24,'(I6,69X,A)') igrp,'System number'
  511             WRITE(24,'(I6,69X,A)') leng,'Number of points in system'
  512             DO J = maxJ,1,-1
  513                DO i = 1,maxI
  514                   WRITE(24,'(F8.2)',ADVANCE='NO') dum(i,j)
  515                END DO
  516                WRITE(24,'(A)',ADVANCE='YES') ''
  517             END DO
  518       ELSE
  519             DO J = maxJ,1,-1
  520                DO i = 1,maxI
  521                dum2nc(i,j,igrp,it)=dum(i,j)
  522                END DO
  523             END DO
  524       END IF
  525             
  526          END DO
  527       END DO
  528             
  529       IF(outputType.EQ.3 .OR. outputType.EQ.4 ) THEN
  530       call t2netcdf(longitude_nc,latitude_nc,dum2nc,maxI,maxJ,&
  531          maxGroup,date1,date2,dt,NTIME_NC,3,outputType)
  532       ENDIF
  533       IF(outputType.EQ.1) CLOSE(24)
  534                                    
  535 !-----Final SYSTEM output: dspr
  536       IF(outputType == 1) THEN
  537       OPEN(unit=25,file='sys_dspr.ww3',status='unknown')
  538                                                         
  539       WRITE(25,'(I6,69X,A)') maxJ,'Number of rows'
  540       WRITE(25,'(I6,69X,A)') maxI,'Number of cols'
  541       ENDIF
  542            
  543       DO it = 1,SIZE(sysA)
  544 !        Loop through identified groups, limiting the output in file to u>
  545          IF(outputType == 1) THEN
  546             WRITE(25,'(F15.6,60x,A)') wsdat(it)%date,'Time'
  547             WRITE(25,'(I6,69X,A)') MIN(ulimGroup,maxGroup), &
  548                'Tot number of systems'
  549          ENDIF
  550          DO igrp = 1,MIN(ulimGroup,maxGroup)
 
 
                                                       ww3_systrk.F90  page  11
 
 
  551             dum(1:maxI,1:maxJ) = 9999.00
  552 !           Find system with this group tag
  553             sysmatch = 1
  554             DO WHILE (sysmatch.LE.maxSys(it))
  555                IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
  556                sysmatch = sysmatch+1
  557             END DO
  558             IF (sysmatch.LE.maxSys(it)) THEN
  559 !              Match found: fill the output matrix with this data
  560                leng = sysA(it)%sys(sysmatch)%nPoints
  561                DO ind = 1, leng
  562                   dum(sysA(it)%sys(sysmatch)%i(ind), &
  563                       sysA(it)%sys(sysmatch)%j(ind)) = &
  564                       sysA(it)%sys(sysmatch)%dspr(ind)
  565                END DO
  566             ELSE
  567                leng = 0
  568             END IF
  569                   
  570       IF(outputType == 1) THEN
  571             WRITE(25,'(I6,69X,A)') igrp,'System number'
  572             WRITE(25,'(I6,69X,A)') leng,'Number of points in system'
  573             DO J = maxJ,1,-1
  574                DO i = 1,maxI
  575                   WRITE(25,'(F8.2)',ADVANCE='NO') dum(i,j)
  576                END DO
  577                WRITE(25,'(A)',ADVANCE='YES') ''
  578             END DO
  579        ELSE
  580             DO J = maxJ,1,-1
  581                DO i = 1,maxI
  582                dum2nc(i,j,igrp,it)=dum(i,j)
  583                END DO
  584             END DO
  585        ENDIF
  586             
  587          END DO
  588       END DO
  589             
  590       IF(outputType.EQ.3 .OR. outputType.EQ.4 ) THEN
  591       call t2netcdf(longitude_nc,latitude_nc,dum2nc,maxI,maxJ,&
  592          maxGroup,date1,date2,dt,NTIME_NC,4,outputType)
  593       ENDIF
  594       IF(outputType.EQ.1) CLOSE(25)
  595                                    
  596       IF (ALLOCATED(DUM)) DEALLOCATE(dum)
  597       IF (ALLOCATED(dum2nc)) DEALLOCATE(dum2nc)
  598                                                
  599       IF(outputType.EQ.3.OR.outputType.EQ.4) THEN
  600       ALLOCATE( hsprt_nc(10,noutp,NTIME_NC) )
  601       ALLOCATE( tpprt_nc(10,noutp,NTIME_NC) )
  602       ALLOCATE( dirprt_nc(10,noutp,NTIME_NC) )
  603       ALLOCATE( lonprt_nc(noutp) )
  604       ALLOCATE( latprt_nc(noutp) )
  605       ENDIF
 
 
                                                       ww3_systrk.F90  page  12
 
 
  606            
  607 !-----Final SYSTEM output: point output
  608       IF(outputType == 1) THEN
  609       OPEN(unit=26,file='sys_pnt.ww3',status='unknown')
  610       WRITE(26,'(A)') '%'
  611       WRITE(26,'(A)') '%'
  612       WRITE(26,'(A)') '% WW3 Wave tracking point output'
  613       WRITE(26,'(A)') '%'
  614       WRITE(26,'(10A)') '%       Xp            Yp            ', &
  615            'HsSY01        HsSY02        HsSY03        HsSY04        ', &
  616            'HsSY05        HsSY06        HsSY07        HsSY08        ', &
  617            'HsSY09        HsSY10        ', &
  618            'TpSY01        TpSY02        TpSY03        TpSY04        ', &
  619            'TpSY05        TpSY06        TpSY07        TpSY08        ', &
  620            'TpSY09        TpSY10        ', &
  621            'DrSY01        DrSY02        DrSY03        DrSY04        ', &
  622            'DrSY05        DrSY06        DrSY07        DrSY08        ', &
  623            'DrSY09        DrSY10'
  624       WRITE(26,'(10A)') '%       [degr]        [degr]        ', &
  625            '[m]           [m]           [m]           [m]           ', &
  626            '[m]           [m]           [m]           [m]           ', &
  627            '[m]           [m]           ', &
  628            '[sec]         [sec]         [sec]         [sec]         ', &
  629            '[sec]         [sec]         [sec]         [sec]         ', &
  630            '[sec]         [sec]         ', &
  631            '[degr]        [degr]        [degr]        [degr]        ', &
  632            '[degr]        [degr]        [degr]        [degr]        ', &
  633            '[degr]        [degr]'
  634       WRITE(26,'(A)') '%'
  635        ENDIF
  636             
  637       DO it = 1,SIZE(sysA)
  638       IF(outputType == 1) THEN
  639          WRITE(26,'(A,F15.6)') 'Time : ',wsdat(it)%date
  640        ENDIF
  641             
  642          DO ip = 1,noutp
  643             hsprt(1:10) = 999.9999
  644             tpprt(1:10) = 999.9999
  645             dirprt(1:10) = 999.9999
  646             lonprt = 999.9999
  647             latprt = 999.9999
  648             BL_hsprt(1:10) = 999.9999
  649             BL_tpprt(1:10) = 999.9999
  650             BL_dirprt(1:10) = 999.9999
  651             BR_hsprt(1:10) = 999.9999
  652             BR_tpprt(1:10) = 999.9999
  653             BR_dirprt(1:10) = 999.9999
  654             TL_hsprt(1:10) = 999.9999
  655             TL_tpprt(1:10) = 999.9999
  656             TL_dirprt(1:10) = 999.9999
  657             TR_hsprt(1:10) = 999.9999
  658             TR_tpprt(1:10) = 999.9999
  659             TR_dirprt(1:10) = 999.9999
  660             BL_lonprt = 999.9999
 
 
                                                       ww3_systrk.F90  page  13
 
 
  661             BL_latprt = 999.9999
  662             BR_lonprt = 999.9999
  663             BR_latprt = 999.9999
  664             TL_lonprt = 999.9999
  665             TL_latprt = 999.9999
  666             TR_lonprt = 999.9999
  667             TR_latprt = 999.9999
  668             BL_W = 999
  669             BR_W = 999
  670             TR_W = 999
  671             TL_W = 999
  672                       
  673             DO j = 1, (maxJ-1)
  674                DO i = 1, (maxI-1)
  675                   IF ( ( ((lonout(ip).GE. &
  676                            wsdat(1)%lon(i,j)).AND. &
  677                           (lonout(ip).LT. &
  678                            wsdat(1)%lon(i+1,j))).OR. &
  679                          ((lonout(ip).GT. &
  680                           wsdat(1)%lon(i,j)).AND. &
  681                          (lonout(ip).LE. &
  682                           wsdat(1)%lon(i+1,j))) ).AND. &
  683                        ( ((latout(ip).GE. &
  684                           wsdat(1)%lat(i,j)).AND. &
  685                           (latout(ip).LT. &
  686                           wsdat(1)%lat(i,j+1))).OR. &
  687                          ((latout(ip).GT. &
  688                           wsdat(1)%lat(i,j)).AND. &
  689                           (latout(ip).LE. &
  690                           wsdat(1)%lat(i,j+1))) ) ) &
  691                   THEN
  692                      BL_lonprt = wsdat(1)%lon(i,j)
  693                      BL_latprt = wsdat(1)%lat(i,j)
  694                      BR_lonprt = wsdat(1)%lon(i+1,j)
  695                      BR_latprt = wsdat(1)%lat(i+1,j)
  696                      TL_lonprt = wsdat(1)%lon(i,j+1)
  697                      TL_latprt = wsdat(1)%lat(i,j+1)
  698                      TR_lonprt = wsdat(1)%lon(i+1,j+1)
  699                      TR_latprt = wsdat(1)%lat(i+1,j+1)
  700 !                    Compute weights for this point
  701                      t = (lonout(ip)-BL_lonprt)/(BR_lonprt-BL_lonprt)
  702                      u = (latout(ip)-BL_latprt)/(TL_latprt-BL_latprt)
  703                      BL_W = (1-t)*(1-u)
  704                      BR_W = t*(1-u)
  705                      TR_W = t*u
  706                      TL_W = (1-t)*u
  707 !                    Compute output values using weights
  708                      lonprt = BL_W*BL_lonprt + BR_W*BR_lonprt + &
  709                               TL_W*TL_lonprt + TR_W*TR_lonprt
  710                      latprt = BL_W*BL_latprt + BR_W*BR_latprt + &
  711                               TL_W*TL_latprt + TR_W*TR_latprt
  712                   END IF
  713                END DO
  714             END DO
  715 !           Loop through identified groups, limiting the output in file t>
 
 
                                                       ww3_systrk.F90  page  14
 
 
  716             DO igrp = 1,MIN(10,maxGroup)
  717 !              Find system with this group tag
  718                sysmatch = 1
  719                DO WHILE (sysmatch.LE.maxSys(it))
  720                   IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
  721                   sysmatch = sysmatch+1
  722                END DO
  723                IF (sysmatch.LE.maxSys(it)) THEN
  724 !                Match found: fill the output matrix with this data
  725                  leng = sysA(it)%sys(sysmatch)%nPoints
  726                  DO ind = 1, leng
  727 !                  Write output point data with bilinear interpolation
  728                    IF ( (sysA(it)%sys(sysmatch)%lon(ind).EQ.&
  729                          BL_lonprt).AND.&
  730                         (sysA(it)%sys(sysmatch)%lat(ind).EQ.&
  731                          BL_latprt) ) THEN
  732                       BL_hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
  733                       BL_tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
  734                       BL_dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
  735                    ELSE IF ( (sysA(it)%sys(sysmatch)%lon(ind).EQ.&
  736                               BR_lonprt).AND.&
  737                              (sysA(it)%sys(sysmatch)%lat(ind).EQ.&
  738                               BR_latprt)) THEN
  739                       BR_hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
  740                       BR_tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
  741                       BR_dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
  742                    ELSE IF ( (sysA(it)%sys(sysmatch)%lon(ind).EQ.&
  743                               TL_lonprt).AND.&
  744                              (sysA(it)%sys(sysmatch)%lat(ind).EQ.&
  745                               TL_latprt)) THEN
  746                       TL_hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
  747                       TL_tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
  748                       TL_dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
  749                    ELSE IF ( (sysA(it)%sys(sysmatch)%lon(ind).EQ.&
  750                               TR_lonprt).AND.&
  751                              (sysA(it)%sys(sysmatch)%lat(ind).EQ.&
  752                               TR_latprt)) THEN
  753                       TR_hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
  754                       TR_tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
  755                       TR_dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
  756                    END IF
  757                  END DO
  758 !                  Compute output value using weights
  759 !                  (only if output point is surrounded by valid points)
  760                    IF ( (BL_hsprt(igrp).NE.999.9999).AND. &
  761                         (BR_hsprt(igrp).NE.999.9999).AND. &
  762                         (TL_hsprt(igrp).NE.999.9999).AND. &
  763                         (TR_hsprt(igrp).NE.999.9999) ) THEN
  764                       hsprt(igrp) = BL_W * BL_hsprt(igrp) + &
  765                                     BR_W * BR_hsprt(igrp) + &
  766                                     TL_W * TL_hsprt(igrp) + &
  767                                     TR_W * TR_hsprt(igrp)
  768                       tpprt(igrp) = BL_W * BL_tpprt(igrp) + &
  769                                     BR_W * BR_tpprt(igrp) + &
  770                                     TL_W * TL_tpprt(igrp) + &
 
 
                                                       ww3_systrk.F90  page  15
 
 
  771                                     TR_W * TR_tpprt(igrp)
  772                       BL_dirx = COS((270-BL_dirprt(igrp))*PI/180.)
  773                       BR_dirx = COS((270-BR_dirprt(igrp))*PI/180.)
  774                       TR_dirx = COS((270-TR_dirprt(igrp))*PI/180.)
  775                       TL_dirx = COS((270-TL_dirprt(igrp))*PI/180.)
  776                       BL_diry = SIN((270-BL_dirprt(igrp))*PI/180.)
  777                       BR_diry = SIN((270-BR_dirprt(igrp))*PI/180.)
  778                       TR_diry = SIN((270-TR_dirprt(igrp))*PI/180.)
  779                       TL_diry = SIN((270-TL_dirprt(igrp))*PI/180.)
  780                       dirprt(igrp)=270 - 180./PI* &
  781                          ATAN2(BL_W*BL_diry+BR_W*BR_diry+ &
  782                                TL_W*TL_diry+TR_W*TR_diry, &
  783                                BL_W*BL_dirx+BR_W*BR_dirx+ &
  784                                TL_W*TL_dirx+TR_W*TR_dirx)
  785                       IF (dirprt(igrp).GT.360.) THEN
  786                          dirprt(igrp) = dirprt(igrp) - 360.
  787                       END IF
  788                    ELSE
  789                       hsprt(igrp) = 999.9999
  790                       tpprt(igrp) = 999.9999
  791                       dirprt(igrp) = 999.9999
  792                    END IF
  793                END IF
  794             END DO
  795       IF(outputType == 1) THEN
  796             WRITE(26,'(32F14.4)') lonprt,latprt, &
  797                        hsprt(1:10),tpprt(1:10),dirprt(1:10)
  798         ENDIF
  799       IF(outputType.EQ.3.OR.outputType.EQ.4) THEN
  800         lonprt_nc(ip)=lonprt
  801         latprt_nc(ip)=latprt
  802         do igrp=1,10
  803         hsprt_nc(igrp,ip,it)=hsprt(igrp)
  804         tpprt_nc(igrp,ip,it)=tpprt(igrp)
  805         dirprt_nc(igrp,ip,it)=dirprt(igrp)
  806         enddo
  807       ENDIF
  808            
  809          END DO
  810       END DO
  811       IF(outputType.EQ.3.OR.outputType.EQ.4) THEN
  812       call pt2netcdf(lonprt_nc,latprt_nc,hsprt_nc,tpprt_nc, &
  813        dirprt_nc,noutp,date1,date2,dt,NTIME_NC,outputType)
  814       ENDIF
  815            
  816       IF(outputType.EQ.1) CLOSE(26)
  817                                    
  818 !-----Final SYSTEM output: point output (Nearest neighbor, as a double ch>
  819       IF (testout) THEN
  820       OPEN(unit=28,file='sys_pnt_nn.ww3',status='unknown')
  821       WRITE(28,'(A)') '%'
  822       WRITE(28,'(A)') '%'
  823       WRITE(28,'(A)') '% WW3 Wave tracking point output'
  824       WRITE(28,'(A)') '%'
  825       WRITE(28,'(10A)') '%       Xp            Yp            ', &
 
 
                                                       ww3_systrk.F90  page  16
 
 
  826            'HsSY01        HsSY02        HsSY03        HsSY04        ', &
  827            'HsSY05        HsSY06        HsSY07        HsSY08        ', &
  828            'HsSY09        HsSY10        ', &
  829            'TpSY01        TpSY02        TpSY03        TpSY04        ', &
  830            'TpSY05        TpSY06        TpSY07        TpSY08        ', &
  831            'TpSY09        TpSY10        ', &
  832            'DrSY01        DrSY02        DrSY03        DrSY04        ', &
  833            'DrSY05        DrSY06        DrSY07        DrSY08        ', &
  834            'DrSY09        DrSY10'
  835       WRITE(28,'(10A)') '%       [degr]        [degr]        ', &
  836            '[m]           [m]           [m]           [m]           ', &
  837            '[m]           [m]           [m]           [m]           ', &
  838            '[m]           [m]           ', &
  839            '[sec]         [sec]         [sec]         [sec]         ', &
  840            '[sec]         [sec]         [sec]         [sec]         ', &
  841            '[sec]         [sec]         ', &
  842            '[degr]        [degr]        [degr]        [degr]        ', &
  843            '[degr]        [degr]        [degr]        [degr]        ', &
  844            '[degr]        [degr]'
  845       WRITE(28,'(A)') '%'
  846                          
  847       DO it = 1,SIZE(sysA)
  848          WRITE(28,'(A,F15.6)') 'Time : ',wsdat(it)%date
  849                                                        
  850          DO ip = 1,noutp
  851             hsprt(1:10) = 999.9999
  852             tpprt(1:10) = 999.9999
  853             dirprt(1:10) = 999.9999
  854             lonprt = 999.9999
  855             latprt = 999.9999
  856                              
  857             DO j = 1, maxJ
  858                DO i = 1, maxI
  859 !                 Write nearest nearbor output (no bilinear interpolation)
  860                   IF ( (lonout(ip).GE. &
  861                         (wsdat(1)%lon(i,j)-dlon/2)).AND. &
  862                         (lonout(ip).LT. &
  863                         (wsdat(1)%lon(i,j)+dlon/2)).AND. &
  864                         (latout(ip).GE. &
  865                         (wsdat(1)%lat(i,j)-dlat/2)).AND. &
  866                         (latout(ip).LT. &
  867                         (wsdat(1)%lat(i,j)+dlat/2)) ) &
  868                   THEN
  869                          lonprt = wsdat(1)%lon(i,j)
  870                          latprt = wsdat(1)%lat(i,j)
  871                   END IF
  872                END DO
  873             END DO
  874 !           Loop through identified groups, limiting the output in file t>
  875             DO igrp = 1,MIN(10,maxGroup)
  876 !              Find system with this group tag
  877                sysmatch = 1
  878                DO WHILE (sysmatch.LE.maxSys(it))
  879                   IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
  880                   sysmatch = sysmatch+1
 
 
                                                       ww3_systrk.F90  page  17
 
 
  881                END DO
  882                IF (sysmatch.LE.maxSys(it)) THEN
  883 !                Match found: fill the output matrix with this data
  884                  leng = sysA(it)%sys(sysmatch)%nPoints
  885                  DO ind = 1, leng
  886 !                  Write nearest nearbor output (no bilinear interpolatio>
  887                    IF ( (lonout(ip).GE. &
  888                         (sysA(it)%sys(sysmatch)%lon(ind)-dlon/2)).AND. &
  889                         (lonout(ip).LT. &
  890                         (sysA(it)%sys(sysmatch)%lon(ind)+dlon/2)).AND. &
  891                         (latout(ip).GE. &
  892                         (sysA(it)%sys(sysmatch)%lat(ind)-dlat/2)).AND. &
  893                         (latout(ip).LT. &
  894                         (sysA(it)%sys(sysmatch)%lat(ind)+dlat/2)) ) &
  895                       THEN
  896                          hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
  897                          tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
  898                          dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
  899                    END IF
  900                  END DO
  901                END IF
  902             END DO
  903             WRITE(28,'(32F14.4)') lonprt,latprt, &
  904                        hsprt(1:10),tpprt(1:10),dirprt(1:10)
  905          END DO
  906       END DO
  907             
  908       CLOSE(28)
  909       END IF
  910             
  911 !-------------------------------------------------------------------------
  912                                                                           
  913       WRITE(20,*) 'In ww3_systrk: Deallocating wsdat ...'
  914       DO it=1,size(wsdat)
  915         IF (ASSOCIATED(wsdat(it)%lat)) DEALLOCATE(wsdat(it)%lat)
  916         IF (ASSOCIATED(wsdat(it)%lon)) DEALLOCATE(wsdat(it)%lon)
  917         IF (ASSOCIATED(wsdat(it)%par)) DEALLOCATE(wsdat(it)%par)
  918         IF (ASSOCIATED(wsdat(it)%wnd)) DEALLOCATE(wsdat(it)%wnd)
  919       END DO
  920       IF (ASSOCIATED(wsdat)) DEALLOCATE(wsdat)
  921       WRITE(20,*) '               Deallocating sysA ...'
  922       DO i=1,size(sysA)
  923        DO j=1,size(sysA(i)%sys)
  924          IF (ASSOCIATED(sysA(i)%sys(j)%i)) DEALLOCATE(sysA(i)%sys(j)%i)
  925          IF (ASSOCIATED(sysA(i)%sys(j)%j)) DEALLOCATE(sysA(i)%sys(j)%j)
  926          IF (ASSOCIATED(sysA(i)%sys(j)%lon)) &
  927                                          DEALLOCATE(sysA(i)%sys(j)%lon)
  928          IF (ASSOCIATED(sysA(i)%sys(j)%lat)) &
  929                                          DEALLOCATE(sysA(i)%sys(j)%lat)
  930          IF (ASSOCIATED(sysA(i)%sys(j)%hs))  &
  931                                           DEALLOCATE(sysA(i)%sys(j)%hs)
  932          IF (ASSOCIATED(sysA(i)%sys(j)%tp))  &
  933                                           DEALLOCATE(sysA(i)%sys(j)%tp)
  934          IF (ASSOCIATED(sysA(i)%sys(j)%dir)) &
  935                                          DEALLOCATE(sysA(i)%sys(j)%dir)
 
 
                                                       ww3_systrk.F90  page  18
 
 
  936          IF (ASSOCIATED(sysA(i)%sys(j)%dspr)) &
  937                                         DEALLOCATE(sysA(i)%sys(j)%dspr)
  938        END DO
  939       END DO
  940       IF (ASSOCIATED(sysA)) DEALLOCATE(sysA)
  941       WRITE(20,*) '               Deallocating maxSys ...'
  942       IF (ASSOCIATED(maxSys)) DEALLOCATE(maxSys)
  943       CLOSE(20)
  944                
  945       WRITE(6,*) '... ww3_systrk completed successfully.'
  946                                                          
  947       WRITE(6,999)
  948                   
  949                   
  950                   
  951   998 FORMAT ( ' ... finished. Elapsed time : ',F10.2,' s')
  952   993 FORMAT (/' *** WAVEWATCH III ERROR IN WW3_SYSTRK : '/           &
  953                '     OutputType=3 needs TRKNC switch ')
  954   994 FORMAT (/' *** WAVEWATCH III ERROR IN WW3_SYSTRK : '/           &
  955                '     OutputType=4 needs TRKNC and NC4 switch ')
  956   995 FORMAT (/' *** WAVEWATCH III ERROR IN WW3_SYSTRK : '/           &
  957                '     OutputType,',I3,'not valid. Options: 1,3,4')
  958                                                                  
  959   999 FORMAT (/15X,'End of program '/ &
  960                15X,'==============================================='/ &
  961                15X,'     *** WAVEWATCH III Wave system tracking ***  ')
  962                                                                        
  963  2000 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/    &
  964                '     ERROR IN OPENING INPUT FILE')
  965  2200 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/    &
  966                '     ERROR IN OPENING PARTITION FILE : ',A)
  967                                                            
  968       END PROGRAM WW3_SYSTRK
  969 !
  970       subroutine t2netcdf(lons,lats,data_in,nlons,nlats,nsys,date1,date2,&
  971          dt,ntime,ivar, outputType)
  972       USE W3TIMEMD
  973       use netcdf
  974       implicit none
  975       character (len = 15) :: file_name
  976       integer, parameter :: ndims = 4
  977       integer, parameter :: deflate = 1
  978       integer :: outputType, ncid, oldMode
  979       integer :: nlons,nlats,nsys,rec,ntime,ivar
  980       double precision :: date1,date2,timenc
  981       real :: data_in(nlons, nlats, nsys,ntime)
  982       real :: lats(nlats), lons(nlons),dt
  983       double precision :: times(ntime)
  984       integer :: iyc,imc,idc,ihc,iminc,isc,Jday,Jday0
  985       integer :: iret
  986 !
  987       integer :: lon_varid, lat_varid, rec_varid
  988       character (len = *), parameter :: lsys_name = "system_index"
  989       character (len = *), parameter :: lat_name = "latitude"
  990       character (len = *), parameter :: lon_name = "longitude"
 
 
                                                       ww3_systrk.F90  page  19
 
 
  991       character (len = *), parameter :: time_name = "time"
  992       integer :: sys_dimid, lon_dimid, lat_dimid, rec_dimid
  993       integer :: start(ndims), count(ndims)
  994 !
  995       character (len = *), parameter :: var1_name="hs"
  996       character (len = *), parameter :: var2_name="tp"
  997       character (len = *), parameter :: var3_name="dir"
  998       character (len = *), parameter :: var4_name="dspr"
  999       integer :: var1_varid, var2_varid, var3_varid,var4_varid
 1000       integer :: dimids(ndims)
 1001 !
 1002       character (len = *), parameter :: units = "units"
 1003       character (len = *), parameter :: var1_units = "m"
 1004       character (len = *), parameter :: var2_units = "s"
 1005       character (len = *), parameter :: var3_units = "degrees"
 1006       character (len = *), parameter :: var4_units = "degrees"
 1007       character (len = *), parameter :: lat_units = "degrees_north"
 1008       character (len = *), parameter :: lon_units = "degrees_east"
 1009       iyc=date1/10000
 1010       imc=(date1-iyc*10000)/100
 1011       idc=int(date1-DBLE(iyc*10000)-DBLE(imc*100))
 1012       ihc=date2/10000
 1013       iminc=(date2-ihc*10000)/100
 1014       isc=date2-ihc*10000-100*iminc
 1015       timenc=DBLE(julday(idc,imc,iyc))+(DBLE(ihc)+(DBLE(iminc)+ &
 1016          (DBLE(isc)/60.0D0))/60.0D0)/24.0D0
 1017       Jday0=julday(1,1,1990)
 1018       timenc=timenc-Jday0
 1019       do rec=1,ntime
 1020        times(rec)=timenc+DBLE( (rec-1)*dt)/3600.0D0/24.0D0
 1021       enddo
 1022       if( ivar == 1) then
 1023        file_name = "sys_hs.ww3.nc"
 1024       else if( ivar == 2) then
 1025        file_name = "sys_tp.ww3.nc"
 1026       else if( ivar == 3) then
 1027        file_name = "sys_dir.ww3.nc"
 1028       else
 1029       file_name = "sys_dspr.ww3.nc"
 1030       endif
 1031 !
 1032       if (outputType.EQ.3) then
 1033       call check( nf90_create(file_name, NF90_CLOBBER, ncid) )
 1034       endif
 1035         if(outputType.EQ.4) call check( nf90_create(file_name,NF90_NETCDF>
 1036       call check ( nf90_set_fill(ncid,nf90_nofill,oldMode) )
 1037       call check( nf90_def_dim(ncid, lsys_name, nsys, sys_dimid) )
 1038       call check( nf90_def_dim(ncid, lat_name, nlats, lat_dimid) )
 1039       call check( nf90_def_dim(ncid, lon_name, nlons, lon_dimid) )
 1040       call check( nf90_def_dim(ncid, time_name, ntime, rec_dimid) )
 1041       call check( nf90_def_var(ncid, lat_name, NF90_REAL, lat_dimid,lat_v>
 1042         call check( nf90_def_var_deflate(ncid,lat_varid,1,1,deflate) )
 1043       call check( nf90_def_var(ncid, lon_name, NF90_REAL, lon_dimid,lon_v>
 1044         call check( nf90_def_var_deflate(ncid,lon_varid,1,1,deflate) )
 1045       call check( nf90_def_var(ncid,time_name,NF90_DOUBLE,rec_dimid,rec_v>
 
 
                                                       ww3_systrk.F90  page  20
 
 
 1046         call check( nf90_def_var_deflate(ncid,rec_varid,1,1,deflate) )
 1047 !
 1048       call check( nf90_put_att(ncid, lat_varid, units, lat_units) )
 1049       call check( nf90_put_att(ncid, lat_varid, 'long_name', 'latitude') )
 1050       call check( nf90_put_att(ncid, lat_varid, 'standard_name', 'latitud>
 1051       call check( nf90_put_att(ncid, lat_varid, 'axis','Y'))
 1052       call check( nf90_put_att(ncid, lon_varid, units, lon_units) )
 1053       call check( nf90_put_att(ncid, lon_varid, 'long_name', 'longitude')>
 1054       call check( nf90_put_att(ncid, lon_varid, 'standard_name', 'longitu>
 1055       call check( nf90_put_att(ncid, lon_varid, 'axis','X'))
 1056       call check(nf90_put_att(ncid,rec_varid,units,&
 1057                   'days since 1990-01-01 00:00:00'))
 1058       call check(nf90_put_att(ncid,rec_varid,'long_name','julian day (UT)>
 1059       call check( nf90_put_att(ncid, rec_varid,'standard_name', 'time') )
 1060       call check( nf90_put_att(ncid, rec_varid, 'conventions',&
 1061       'relative julian day with decimal part (as part of the day)' ) )
 1062       call check( nf90_put_att(ncid, rec_varid, 'axis','T'))
 1063 !
 1064       dimids = (/ lon_dimid, lat_dimid, sys_dimid, rec_dimid /)
 1065       if( ivar == 1) then
 1066       call check( nf90_def_var(ncid, var1_name, NF90_REAL, dimids,var1_va>
 1067         call check( nf90_def_var_deflate(ncid,var1_varid,1,1,deflate) )
 1068       call check( nf90_put_att(ncid, var1_varid, units, var1_units) )
 1069       call check( nf90_put_att(ncid, var1_varid,'long_name','significant_>
 1070       call check( nf90_put_att(ncid, var1_varid,'missing_value','9999.00'>
 1071        else if( ivar == 2) then
 1072       call check( nf90_def_var(ncid, var2_name, NF90_REAL, dimids, var2_v>
 1073         call check( nf90_def_var_deflate(ncid,var2_varid,1,1,deflate) )
 1074       call check( nf90_put_att(ncid, var2_varid, units, var2_units) )
 1075       call check( nf90_put_att(ncid, var2_varid,'long_name','peak_period'>
 1076       call check( nf90_put_att(ncid, var2_varid,'missing_value','9999.00'>
 1077        else if ( ivar ==3 ) then
 1078       call check( nf90_def_var(ncid, var3_name, NF90_REAL, dimids, var3_v>
 1079         call check( nf90_def_var_deflate(ncid,var3_varid,1,1,deflate) )
 1080       call check( nf90_put_att(ncid, var3_varid, units, var3_units) )
 1081       call check( nf90_put_att(ncid, var3_varid,'long_name','peak_directi>
 1082       call check( nf90_put_att(ncid, var3_varid,'missing_value','9999.00'>
 1083        else
 1084       call check( nf90_def_var(ncid, var4_name, NF90_REAL, dimids, var4_v>
 1085         call check( nf90_def_var_deflate(ncid,var4_varid,1,1,deflate) )
 1086       call check( nf90_put_att(ncid, var4_varid, units, var4_units) )
 1087       call check( nf90_put_att(ncid,var4_varid,'long_name','directional_s>
 1088       call check( nf90_put_att(ncid, var4_varid,'missing_value','9999.00'>
 1089       endif
 1090       call check( nf90_enddef(ncid) )
 1091 !
 1092       call check( nf90_put_var(ncid, lat_varid, lats) )
 1093       call check( nf90_put_var(ncid, lon_varid, lons) )
 1094       call check( nf90_put_var(ncid, rec_varid, times) )
 1095 !
 1096       count = (/ nlons, nlats, nsys, ntime /)
 1097       start = (/ 1, 1, 1, 1 /)
 1098       if( ivar == 1) then
 1099       call check( nf90_put_var(ncid, var1_varid, data_in, start = start, &
 1100                               count = count) )
 
 
                                                       ww3_systrk.F90  page  21
 
 
 1101        else if( ivar == 2) then
 1102       call check( nf90_put_var(ncid, var2_varid, data_in, start = start, &
 1103                               count = count) )
 1104        else if( ivar == 3) then
 1105       call check( nf90_put_var(ncid, var3_varid, data_in, start = start, &
 1106                               count = count) )
 1107        else
 1108       call check( nf90_put_var(ncid, var4_varid, data_in, start = start, &
 1109                               count = count) )
 1110       endif
 1111       call check( nf90_close(ncid) )
 1112       end subroutine t2netcdf
 1113 !
 1114       subroutine check(status)
 1115       use netcdf
 1116       integer, intent ( in) :: status
 1117       if(status /= nf90_noerr) then
 1118         write(6,996)
 1119           996 FORMAT (/' *** WAVEWATCH III ERROR IN WW3_SYSTRK:'/        >
 1120                        'netCDF error:')
 1121         print *, trim(nf90_strerror(status))
 1122         stop "Stopped in netcdf output part"
 1123       endif
 1124       end subroutine check
 1125 !
 1126       subroutine pt2netcdf(longitude,latitude,hs,tp,&
 1127        dir,npoints,date1,date2,dt,ntime,outputType)
 1128       USE W3TIMEMD
 1129       use netcdf
 1130       implicit none
 1131       integer :: ntime,npoints,outputType
 1132       integer, parameter :: deflate = 1
 1133       integer :: iret, oldMode
 1134       integer :: ncid
 1135       integer :: system_index_dim
 1136       integer :: point_dim,rec_dim
 1137       integer :: nsys
 1138       integer :: start(3), count(3)
 1139       parameter (nsys = 10)
 1140       integer :: latitude_id
 1141       integer :: longitude_id
 1142       integer :: time_id
 1143       integer :: hs_id
 1144       integer :: tp_id
 1145       integer :: dir_id
 1146       integer :: time_rank
 1147       integer :: hs_rank
 1148       integer :: tp_rank
 1149       integer :: dir_rank
 1150       parameter (time_rank = 1)
 1151       parameter (hs_rank = 3)
 1152       parameter (tp_rank = 3)
 1153       parameter (dir_rank = 3)
 1154 !
 1155       integer :: hs_dims(hs_rank)
 
 
                                                       ww3_systrk.F90  page  22
 
 
 1156       integer :: tp_dims(tp_rank)
 1157       integer :: dir_dims(dir_rank)
 1158       real :: latitude(npoints),dt
 1159       real :: longitude(npoints)
 1160       real :: hs(nsys, npoints, ntime)
 1161       real :: tp(nsys, npoints, ntime)
 1162       real :: dir(nsys, npoints, ntime)
 1163       integer :: iyc,imc,idc,ihc,iminc,isc,Jday,Jday0,rec
 1164       double precision date1,date2,timenc
 1165       double precision times(ntime)
 1166 !
 1167       iyc=date1/10000
 1168       imc=(date1-iyc*10000)/100
 1169       idc=int(date1-DBLE(iyc*10000)-DBLE(imc*100))
 1170       ihc=date2/10000
 1171       iminc=(date2-ihc*10000)/100
 1172       isc=date2-ihc*10000-100*iminc
 1173       timenc=DBLE(julday(idc,imc,iyc))+(DBLE(ihc)+(DBLE(iminc)+ &
 1174        (DBLE(isc)/60.0D0))/60.0D0)/24.0D0
 1175       Jday0=julday(1,1,1990)
 1176       timenc=timenc-Jday0
 1177       do rec=1,ntime
 1178        times(rec)=timenc+DBLE( (rec-1)*dt)/3600.0D0/24.0D0
 1179       enddo
 1180 !
 1181       if(outputType.EQ.3) then
 1182       iret = nf90_create('sys_pnt.ww3.nc', NF90_CLOBBER, ncid)
 1183       endif
 1184       if (outputType.EQ.4) iret = nf90_create('sys_pnt.ww3.nc',NF90_NETCD>
 1185       call check(iret)
 1186       iret = nf90_set_fill(ncid,nf90_nofill,oldMode)
 1187       call check(iret)
 1188       iret = nf90_def_dim(ncid, 'system_index', nsys, system_index_dim)
 1189       call check(iret)
 1190       iret = nf90_def_dim(ncid, 'point', npoints, point_dim)
 1191       call check(iret)
 1192       iret = nf90_def_dim(ncid, 'time', ntime, rec_dim)
 1193       call check(iret)
 1194       iret = nf90_def_var(ncid, 'latitude', NF90_REAL, point_dim, &
 1195           latitude_id)
 1196       call check(iret)
 1197         call check( nf90_def_var_deflate(ncid,latitude_id,1,1,deflate))
 1198       iret = nf90_def_var(ncid, 'longitude', NF90_REAL, point_dim, &
 1199         longitude_id)
 1200       call check(iret)
 1201         call check( nf90_def_var_deflate(ncid,longitude_id,1,1,deflate))
 1202       iret = nf90_def_var(ncid, 'time', NF90_DOUBLE, rec_dim, &
 1203          time_id)
 1204       call check(iret)
 1205         call check( nf90_def_var_deflate(ncid,time_id,1,1,deflate) )
 1206       hs_dims(3) = rec_dim
 1207       hs_dims(2) = point_dim
 1208       hs_dims(1) = system_index_dim
 1209       iret = nf90_def_var(ncid, 'hs', NF90_REAL,  &
 1210         hs_dims, hs_id)
 
 
                                                       ww3_systrk.F90  page  23
 
 
 1211       call check(iret)
 1212         call check( nf90_def_var_deflate(ncid,hs_id,1,1,deflate))
 1213       tp_dims(3) = rec_dim
 1214       tp_dims(2) = point_dim
 1215       tp_dims(1) = system_index_dim
 1216       iret = nf90_def_var(ncid, 'tp', NF90_REAL, &
 1217         tp_dims, tp_id)
 1218       call check(iret)
 1219         call check( nf90_def_var_deflate(ncid,tp_id,1,1,deflate))
 1220       dir_dims(3) = rec_dim
 1221       dir_dims(2) = point_dim
 1222       dir_dims(1) = system_index_dim
 1223       iret = nf90_def_var(ncid, 'dir', NF90_REAL, &
 1224         dir_dims, dir_id)
 1225       call check(iret)
 1226         call check( nf90_def_var_deflate(ncid,dir_id,1,1,deflate))
 1227       iret = nf90_put_att(ncid, latitude_id, 'units', 'degrees_north')
 1228       call check(iret)
 1229       iret = nf90_put_att(ncid, latitude_id, 'long_name', 'latitude')
 1230       call check(iret)
 1231       iret = nf90_put_att(ncid, latitude_id, 'standard_name', 'latitude')
 1232       call check(iret)
 1233       iret = nf90_put_att(ncid, latitude_id, 'axis', 'Y')
 1234       call check(iret)
 1235       iret = nf90_put_att(ncid, longitude_id, 'units', 'degrees_east')
 1236       call check(iret)
 1237       iret = nf90_put_att(ncid, longitude_id,'long_name','longitude')
 1238       call check(iret)
 1239       iret = nf90_put_att(ncid, longitude_id,'standard_name','longitude')
 1240       call check(iret)
 1241       iret = nf90_put_att(ncid, longitude_id, 'axis', 'X')
 1242       call check(iret)
 1243       iret = nf90_put_att(ncid, time_id, 'units', &
 1244         'days since 1990-01-01 00:00:00')
 1245       call check(iret)
 1246       iret = nf90_put_att(ncid, time_id, 'long_name','julian day(UT)')
 1247       call check(iret)
 1248       iret = nf90_put_att(ncid, time_id, 'standard_name','time')
 1249       call check(iret)
 1250       iret = nf90_put_att(ncid, time_id, 'conventions', &
 1251        'relative julian day with decimal part (as part of the day)')
 1252       call check(iret)
 1253       iret = nf90_put_att(ncid, time_id, 'axis', 'T')
 1254       call check(iret)
 1255       iret = nf90_put_att(ncid, hs_id, 'units', 'm')
 1256       call check(iret)
 1257       iret = nf90_put_att(ncid, hs_id,'long_name','significant_wave_heigh>
 1258       call check(iret)
 1259       iret = nf90_put_att(ncid, hs_id, 'missing_value', &
 1260         '999.9999')
 1261       call check(iret)
 1262       iret = nf90_put_att(ncid, tp_id, 'units', 's')
 1263       call check(iret)
 1264       iret = nf90_put_att(ncid, tp_id,'long_name','peak_period')
 1265       call check(iret)
 
 
                                                       ww3_systrk.F90  page  24
 
 
 1266       iret = nf90_put_att(ncid, tp_id, 'missing_value', &
 1267        '999.9999')
 1268       call check(iret)
 1269       iret = nf90_put_att(ncid, dir_id, 'units', 'degrees')
 1270       call check(iret)
 1271       iret = nf90_put_att(ncid, dir_id,'long_name','peak_direction')
 1272       call check(iret)
 1273       iret = nf90_put_att(ncid, dir_id, 'missing_value',&
 1274          '999.9999')
 1275       call check(iret)
 1276       iret = nf90_enddef(ncid)
 1277       call check(iret)
 1278       iret = nf90_put_var(ncid, latitude_id, latitude)
 1279       call check(iret)
 1280 !
 1281       iret = nf90_put_var(ncid, longitude_id, longitude)
 1282       call check(iret)
 1283 !
 1284       iret = nf90_put_var(ncid, time_id, times)
 1285       call check(iret)
 1286 !
 1287       start = (/ 1, 1, 1 /)
 1288       count = (/ nsys,npoints,ntime /)
 1289 !
 1290       iret = nf90_put_var(ncid, hs_id, hs,&
 1291        start = start, count = count )
 1292       call check(iret)
 1293       iret = nf90_put_var(ncid, tp_id, tp, &
 1294        start = start, count = count )
 1295       call check(iret)
 1296       iret = nf90_put_var(ncid, dir_id, dir,&
 1297        start = start, count = count )
 1298       call check(iret)
 1299       iret = nf90_close(ncid)
 1300       call check(iret)
 1301       return
 1302       end subroutine pt2netcdf
 1303                               
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/opt/cray/netcdf-hdf5parallel/4.2.0/pgi/119/include -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
