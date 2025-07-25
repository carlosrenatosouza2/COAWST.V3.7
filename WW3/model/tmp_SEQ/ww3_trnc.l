                                                         ww3_trnc.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3TRNC
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           M. Accensi              |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         15-May-2018 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    17-Feb-2016 : Creation                            ( version 5.11 )
   13 !/    11-Apr-2016 : Adapted to use more options         ( version 5.11 )
   14 !/    15-May-2018 : Add namelist feature                ( version 6.05 )
   15 !/    18-Jun-2020 : Support for 360-day calendar.       ( version 7.08 )
   16 !/
   17 !/    Copyright 2014 National Weather Service (NWS),
   18 !/       National Oceanic and Atmospheric Administration.  All rights
   19 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   20 !/       No unauthorized use without permission.
   21 !/
   22 !  1. Purpose :
   23 !
   24 !     Convert direct access track output file to netCDF file.
   25 !
   26 !  2. Method :
   27 !
   28 !     Info read from track_o.ww3, written to track.nc
   29 !
   30 !  3. Parameters :
   31 !
   32 !  4. Subroutines used :
   33 !
   34 !      Name      Type  Module   Description
   35 !     ----------------------------------------------------------------
   36 !      W3NMOD    Subr. W3GDATMD Set number of model.
   37 !      W3NOUT    Subr. W3ODATMD Set number of model for output.
   38 !     ----------------------------------------------------------------
   39 !
   40 !  5. Called by :
   41 !
   42 !     None, stand-alone program.
   43 !
   44 !  6. Error messages :
   45 !
   46 !  7. Remarks :
   47 !
   48 !  8. Structure :
   49 !
   50 !     See source code.
   51 !
   52 !  9. Switches :
   53 !
   54 !       !/S    Enable subroutine tracing.
   55 !
 
 
                                                         ww3_trnc.F90  page   2
 
 
   56 ! 10. Source code :
   57 !
   58 !/ ------------------------------------------------------------------- /
   59       USE CONSTANTS
   60                    
   61       USE W3GDATMD, ONLY : W3NMOD, W3SETG, FLAGLL, XFR
   62       USE W3ODATMD, ONLY : W3NOUT, W3SETO, FNMPRE
   63       USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
   64       USE W3TIMEMD
   65 !
   66       USE W3ODATMD, ONLY: NDSO, NDSE
   67 !
   68       USE W3NMLTRNCMD
   69       USE NETCDF
   70 !
   71       IMPLICIT NONE
   72 !/
   73 !/ ------------------------------------------------------------------- /
   74 !/ Local parameters
   75 !/
   76       TYPE(NML_TRACK_T)       :: NML_TRACK
   77       TYPE(NML_FILE_T)        :: NML_FILE
   78 !
   79       INTEGER                 :: NDSI, NDSINP,                        &
   80                                  NDSOUT, NDSTRC, NTRACE,              &
   81                                  NSPEC, IERR, MK, MTH, IT,            &
   82                                  ILOC, ISPEC, S3, HASNC4, IOUT,       &
   83                                  IRET, NCTYPE,NCID, ITH
   84                                                        
   85       INTEGER                 :: TIME(2), TOUT(2), NOUT, TDUM(2),     &
   86                                  DIMID(4), VARID(18), DIMLN(4),       &
   87                                  STOPDATE(8)
   88 !
   89       REAL                    :: TH1, DTH, X, Y, DW, CX, CY, CAO, CDO,&
   90                                  WX, WY, WAO, WDO, UST, AS, DTEST,    &
   91                                  DTREQ, DTHD, RTH0, M2KM
   92 !
   93       REAL, ALLOCATABLE       :: FREQ(:), FREQ1(:), FREQ2(:), DSIP(:),&
   94                                  SPEC(:,:), E(:,:), THD(:), DIR(:)
   95 !
   96       CHARACTER*34, PARAMETER ::                                      &
   97                        IDTST  = 'WAVEWATCH III TRACK OUTPUT SPECTRA'
   98       CHARACTER*30            :: FILEPREFIX, STRSTOPDATE
   99       CHARACTER*20            :: FORMAT1
  100       CHARACTER               :: IDTIME*23, IDDDAY*11, TRCKID*32,     &
  101                                  COMSTR*1, IDSTR*34, TSTSTR*3, STIME*23
  102 !
  103       LOGICAL                 :: FLGNML
  104                                        
  105                                        
  106 !/
  107 !/ ------------------------------------------------------------------- /
  108 !/
  109 !
  110 ! 0.  Initialize data structure
 
 
                                                         ww3_trnc.F90  page   3
 
 
  111 !
  112       CALL W3NMOD ( 1, 6, 6 )
  113       CALL W3SETG ( 1, 6, 6 )
  114       CALL W3NOUT (    6, 6 )
  115       CALL W3SETO ( 1, 6, 6 )
  116 !
  117 ! 1.  IO set-up.
  118 !
  119       NDSI   = 10
  120       NDSINP = 11
  121       NDSOUT = 51
  122 !
  123       NDSTRC =  6
  124       NTRACE = 10
  125       CALL ITRACE ( NDSTRC, NTRACE )
  126 !
  127       WRITE (NDSO,900)
  128 !
  129  
  130  
  131 !
  132 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  133 ! 2.   Read requests from input file.
  134 !
  135  
  136 !
  137 ! process ww3_trnc namelist
  138 !
  139       INQUIRE(FILE=TRIM(FNMPRE)//"ww3_trnc.nml", EXIST=FLGNML)
  140       IF (FLGNML) THEN
  141         ! Read namelist
  142         CALL W3NMLTRNC (NDSI, TRIM(FNMPRE)//'ww3_trnc.nml', NML_TRACK, NM>
  143                                                                           
  144 ! 2.1 Time setup IDTIME, DTREQ, NOUT
  145         READ(NML_TRACK%TIMESTRIDE, *)  DTREQ
  146         READ(NML_TRACK%TIMECOUNT, *)   NOUT
  147         READ(NML_TRACK%TIMESTART, *)   TOUT(1), TOUT(2)
  148                                                        
  149                                                        
  150 ! 2.2 Output type
  151         NCTYPE = NML_FILE%NETCDF
  152         FILEPREFIX = NML_FILE%PREFIX
  153         S3 = NML_TRACK%TIMESPLIT
  154                                 
  155                                 
  156       END IF ! FLGNML
  157                      
  158 !
  159 ! process old ww3_trnc.inp format
  160 !
  161       IF (.NOT. FLGNML) THEN
  162         OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_trnc.inp',STATUS='OLD',ERR=805>
  163         REWIND (NDSI)
  164                      
  165         READ (NDSI,'(A)',END=806,ERR=807,IOSTAT=IERR) COMSTR
 
 
                                                         ww3_trnc.F90  page   4
 
 
  166         IF (COMSTR.EQ.' ') COMSTR = '$'
  167         WRITE (NDSO,901) COMSTR
  168                                
  169                                
  170 ! 2.1 Time setup IDTIME, DTREQ, NOUT
  171         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  172         READ (NDSI,*,END=806,ERR=807) TOUT, DTREQ, NOUT
  173                                                        
  174                                                        
  175 ! 2.2 Output type
  176         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  177         READ (NDSI,*,END=806,ERR=807) NCTYPE
  178         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  179         FILEPREFIX= 'ww3.'
  180         READ (NDSI,*,END=806,ERR=807) FILEPREFIX
  181         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  182         READ (NDSI,*,END=806,ERR=807) S3
  183                                         
  184       END IF ! .NOT. FLGNML
  185                            
  186                            
  187                            
  188                            
  189 ! 2.1 Time setup IDTIME, DTREQ, NOUT
  190       DTREQ  = MAX ( 0. , DTREQ )
  191       IF ( DTREQ.EQ.0. ) NOUT = 1
  192       NOUT   = MAX ( 1 , NOUT )
  193       CALL STME21 ( TOUT , IDTIME )
  194       WRITE (NDSO,940) IDTIME
  195       TDUM = 0
  196       CALL TICK21 ( TDUM , DTREQ )
  197       CALL STME21 ( TDUM , IDTIME )
  198       IF ( DTREQ .GE. 86400. ) THEN
  199           WRITE (IDDDAY,'(I10,1X)') INT(DTREQ/86400.)
  200         ELSE
  201           IDDDAY = '           '
  202         END IF
  203       IDTIME(1:11) = IDDDAY
  204       IDTIME(21:23) = '   '
  205       WRITE (NDSO,941) IDTIME, NOUT
  206                                    
  207                                    
  208 ! 2.2 Output type
  209       IF ( NCTYPE.LT.3 .OR. NCTYPE.GT.4 ) THEN
  210         WRITE (NDSE,1010) NCTYPE
  211         CALL EXTCDE ( 1 )
  212       END IF
  213       ! if NCTYPE = 4 checking that it is compiled with NC4
  214       HASNC4=0
  215       HASNC4=1
  216       IF ((HASNC4 .eq. 0).and.(NCTYPE.eq.4)) THEN
  217         WRITE (NDSE,1012)
  218         CALL EXTCDE ( 1 )
  219       END IF
  220       ! S3 defines the number of characters in the date for the filename
 
 
                                                         ww3_trnc.F90  page   5
 
 
  221       ! S3=4-> YYYY, S3=6 -> YYYYMM, S3=10 -> YYYYMMDDTHHZ ...
  222                                                               
  223                                                               
  224                                                               
  225 !
  226 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  227 ! 3.  Check consistency with input file and track_o.ww3
  228 !
  229       OPEN (NDSINP,FILE=TRIM(FNMPRE)//'track_o.ww3',FORM='UNFORMATTED', &
  230             STATUS='OLD',ERR=800,IOSTAT=IERR)
  231       READ (NDSINP,ERR=801,IOSTAT=IERR) IDSTR, FLAGLL, MK, MTH, XFR
  232 !
  233       IF ( FLAGLL ) THEN
  234           M2KM  = 1.
  235         ELSE
  236           M2KM  = 1.E-3
  237         END IF
  238 !
  239       IF ( IDSTR .NE. IDTST ) GOTO 810
  240                                       
  241       WRITE (NDSO,902) MK, MTH
  242       NSPEC  = MK * MTH
  243       ALLOCATE ( FREQ(MK), FREQ1(MK), FREQ2(MK), DSIP(MK), &
  244                  SPEC(MK,MTH), E(MK,MTH), THD(MTH), DIR(MTH) )
  245 !
  246       READ (NDSINP,ERR=801,IOSTAT=IERR) TH1, DTH, FREQ, DSIP
  247                                                             
  248 !
  249 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  250 ! 4.  Time management.
  251 !
  252       IOUT = 0
  253       NCID = 0
  254       WRITE (NDSO,970)
  255       READ (NDSINP,END=444, ERR=801,IOSTAT=IERR) TIME
  256       BACKSPACE (NDSINP)
  257                         
  258                         
  259 ! 4.1 Loops on track_o.ww3 to read the time and data
  260       DO
  261         DTEST  = DSEC21 ( TIME , TOUT )
  262                                        
  263         ! cycle to reach the start time of input file
  264         IF ( DTEST .LT. 0. ) THEN
  265           CALL TICK21 ( TOUT , DTREQ )
  266           CYCLE
  267         END IF
  268               
  269         IF ( DTEST .GE. 0. ) THEN
  270           TRCKID=''
  271           READ (NDSINP,END=444, ERR=801,IOSTAT=IERR) TIME, X, Y, TSTSTR, >
  272           IF ( TSTSTR .EQ. 'SEA' ) THEN
  273             READ (NDSINP,ERR=801,IOSTAT=IERR) DW, CX, CY, WX, WY, UST, &
  274                                               AS, SPEC
  275           END IF
 
 
                                                         ww3_trnc.F90  page   6
 
 
  276           IF ( IERR .EQ. -1 ) THEN
  277             WRITE (NDSO,944)
  278             EXIT
  279           END IF
  280                 
  281                 
  282           IF ( TIME(1).EQ.TOUT(1) .AND. TIME(2).EQ.TOUT(2) ) THEN
  283             ILOC = ILOC + 1
  284             IF ( TSTSTR .EQ. 'SEA' ) ISPEC = ISPEC + 1
  285           ENDIF
  286           IF ( TIME(1).GT.TOUT(1) .OR. TIME(2).GT.TOUT(2) ) THEN
  287             CALL STME21 ( TIME , STIME )
  288             WRITE (NDSO,945) STIME, ILOC, ISPEC
  289             ILOC    = 1
  290             ISPEC   = 0
  291             IF ( TSTSTR .EQ. 'SEA' ) ISPEC = ISPEC + 1
  292             TOUT(1) = TIME(1)
  293             TOUT(2) = TIME(2)
  294           ENDIF
  295         END IF
  296               
  297               
  298 ! 4.1.1 Increments the global time counter IOUT
  299         IOUT = IOUT + 1
  300         CALL STME21 ( TOUT , IDTIME )
  301         WRITE (NDSO,971) IDTIME
  302                                
  303                                
  304 ! 4.1.2  Processes the variable value for the time step IOUT
  305         CALL W3EXNC ( FILEPREFIX, NCTYPE, NCID, S3, STRSTOPDATE, MK, MTH )
  306                                                                           
  307                                                                           
  308 ! 4.1.3 Defines the stop date
  309         CALL T2D(TOUT,STOPDATE,IERR)
  310         WRITE(STRSTOPDATE,'(I4.4,A,4(I2.2,A),I2.2)') STOPDATE(1),'-',STOP>
  311               '-',STOPDATE(3),' ',STOPDATE(5),':',STOPDATE(6),':',STOPDAT>
  312                                                                           
  313         IF ( IOUT .GE. NOUT ) EXIT
  314       END DO
  315             
  316             
  317     444 CONTINUE
  318                 
  319 ! 4.2 Closes the netCDF file
  320       IF (NCID.NE.0) THEN
  321         IRET = NF90_REDEF(NCID)
  322         CALL CHECK_ERR(IRET)
  323         IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'stop_date',STRSTOPDATE)
  324         CALL CHECK_ERR(IRET)
  325         IRET=NF90_CLOSE(NCID)
  326         CALL CHECK_ERR(IRET)
  327       END IF
  328             
  329 !
  330       GOTO 888
 
 
                                                         ww3_trnc.F90  page   7
 
 
  331 !
  332 ! Escape locations read errors :
  333 !
  334   800 CONTINUE
  335       WRITE (NDSE,1000) IERR
  336       CALL EXTCDE ( 10 )
  337 !
  338   801 CONTINUE
  339       WRITE (NDSE,1001)
  340       CALL EXTCDE ( 11 )
  341 !
  342   805 CONTINUE
  343       WRITE (NDSE,1004) IERR
  344       CALL EXTCDE ( 14 )
  345 !
  346   806 CONTINUE
  347       WRITE (NDSE,1005) IERR
  348       CALL EXTCDE ( 15 )
  349 !
  350   807 CONTINUE
  351       WRITE (NDSE,1006) IERR
  352       CALL EXTCDE ( 16 )
  353 !
  354  
  355   810 CONTINUE
  356       WRITE (NDSE,1010) IDSTR, IDTST
  357       CALL EXTCDE ( 20 )
  358 !
  359   888 CONTINUE
  360       WRITE (NDSO,999)
  361 !
  362 ! Formats
  363 !
  364   900 FORMAT (/15X,'   *** WAVEWATCH III Track output postp. ***   '/ &
  365                15X,'==============================================='/)
  366   901 FORMAT ( '  Comment character is ''',A,''''/)
  367 !
  368   902 FORMAT ( '      Spectral grid size : ',I3,' by ',I3//              &
  369                    '  Opening file : '/                               &
  370                    ' -----------------------------------------------')
  371   940 FORMAT (/'  Output time data : '/                               &
  372                ' --------------------------------------------------'/ &
  373                '      First time         : ',A)
  374   941 FORMAT ( '      Interval           : ',A/                       &
  375                '      Number of requests : ',I10)
  376 !
  377   944 FORMAT (/'      End of file reached '/)
  378 !
  379   945 FORMAT ( '     ',A,' :',I6,' points and',I6,'  spectra.')
  380 !
  381   970 FORMAT (//'  Generating files '/                                &
  382                ' --------------------------------------------------')
  383   971 FORMAT ( '      Files for ',A)
  384 !
  385   999 FORMAT (/'  End of program '/                                   &
 
 
                                                         ww3_trnc.F90  page   8
 
 
  386                ' ========================================='/          &
  387                '         WAVEWATCH III Track output '/)
  388 !
  389  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRNC : '/               &
  390                '     ERROR IN OPENING INPUT FILE'/                    &
  391                '     IOSTAT =',I5/)
  392 !
  393  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRNC : '/               &
  394                '     PREMATURE END OF INPUT FILE'/)
  395 !
  396  1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRNC : '/               &
  397                '     ERROR IN OPENING INPUT FILE'/                    &
  398                '     IOSTAT =',I5/)
  399 !
  400  1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRNC : '/               &
  401                '     ERROR IN READING FROM INPUT FILE'/               &
  402                '     IOSTAT =',I5/)
  403 !
  404  1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRNC : '/               &
  405                '     ERROR IN OPENING OUTPUT FILE'/                   &
  406                '     IOSTAT =',I5/)
  407 !
  408  1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRNC : '/               &
  409                '     ILLEGAL TYPE, NCTYPE =',I4/)
  410 !
  411  1012 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRNC : '/               &
  412                '     NCTYPE = 4 BUT NOT COMPILED WITH NC4'/)
  413 !/
  414 !/ Internal subroutine W3EXNC ---------------------------------------- /
  415 !/
  416       CONTAINS
  417 !/ ------------------------------------------------------------------- /
  418       SUBROUTINE W3EXNC ( FILEPREFIX, NCTYPE, NCID, S3, STRSTOPDATE, MK, >
  419 !/
  420 !/                  +-----------------------------------+
  421 !/                  |           M. Accensi              |
  422 !/                  |                        FORTRAN 90 |
  423 !/                  | Last update :          8-Apr-2016 |
  424 !/                  +-----------------------------------+
  425 !/
  426 !/     8-apr-2016 : Creation                            ( version 5.11 )
  427 !/
  428 !  1. Purpose :
  429 !
  430 !     Perform actual track output in NetCDF file.
  431 !
  432 !  3. Parameters :
  433 !
  434 !     Parameter list
  435 !     ----------------------------------------------------------------
  436 !     ----------------------------------------------------------------
  437 !
  438 !     Internal parameters
  439 !     ----------------------------------------------------------------
  440 !     ----------------------------------------------------------------
 
 
                                                         ww3_trnc.F90  page   9
 
 
  441 !
  442 !  4. Subroutines used :
  443 !
  444 !      Name      Type  Module   Description
  445 !     ----------------------------------------------------------------
  446 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  447 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
  448 !     ----------------------------------------------------------------
  449 !
  450 !  5. Called by :
  451 !
  452 !     Main program in which it is contained.
  453 !
  454 !  6. Error messages :
  455 !
  456 !       None.
  457 !
  458 !  7. Remarks :
  459 !
  460 !       None.
  461 !
  462 !  8. Structure :
  463 !
  464 !     See source code.
  465 !
  466 !  9. Switches :
  467 !
  468 !       !/S  Enable subroutine tracing.
  469 !       !/T  Enable test output.
  470 !
  471 ! 10. Source code :
  472 !
  473 !/ ------------------------------------------------------------------- /
  474       USE NETCDF
  475       IMPLICIT NONE
  476                    
  477 !/
  478 !/ ------------------------------------------------------------------- /
  479 !/ Parameter list
  480 !/
  481       INTEGER, INTENT(IN)       :: NCTYPE, MK, MTH
  482       CHARACTER(30), INTENT(IN) :: FILEPREFIX, STRSTOPDATE
  483       INTEGER, INTENT(INOUT)    :: NCID, S3
  484 !/
  485 !/ ------------------------------------------------------------------- /
  486 !/ Local parameters
  487 !/
  488       INTEGER                 :: S1, S2, S4, S5, NDSDAT, IRET
  489       INTEGER                 :: STARTDATE(8), CURDATE(8), REFDATE(8)
  490     INTEGER                  :: DEFLATE=1
  491 !
  492       DOUBLE PRECISION        :: OUTJULDAY
  493 !
  494       CHARACTER*30            :: STRSTARTDATE
  495       CHARACTER               :: FNAMENC*50, ENAME*6
 
 
                                                         ww3_trnc.F90  page  10
 
 
  496       CHARACTER, SAVE         :: OLDTIMEID*16 = '0000000000000000'
  497       CHARACTER, SAVE         :: TIMEID*16 = '0000000000000000'
  498                                                                
  499 !/
  500 !/ ------------------------------------------------------------------- /
  501 !/
  502 !
  503       CALL U2D('days since 1990-01-01 00:00:00',REFDATE,IERR)
  504                                                              
  505 ! 1.1 Sets the date as ISO8601 convention
  506       ! S3 defines the number of characters in the date for the filename
  507       ! S3=4-> YYYY, S3=6 -> YYYYMM, S3=10 -> YYYYMMDDHH
  508       ! Setups min and max date format
  509       IF (S3.LT.4) S3=4
  510       IF (S3.GT.10) S3=10
  511 !
  512       ! Defines the format of FILETIME
  513       S5=S3-8
  514       S4=S3
  515       OLDTIMEID=TIMEID
  516       ! if S3=>YYYYMMDDHH then filetime='YYYYMMDDTHHMMSSZ'
  517       IF (S3.EQ.10) THEN
  518         S4=S4+2 ! add chars for ISO8601 : day T hours Z
  519         WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I8.8,A1,I',S5,'.',S5,',A1)'
  520         WRITE (TIMEID,FORMAT1) TIME(1), 'T', &
  521                FLOOR(REAL(TIME(2))/NINT(10.**(6-S5))), 'Z'
  522       ! if S3=>YYYYMMDD then filetime='YYYYMMDD'
  523       ELSE IF (S3.EQ.8) THEN
  524         WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
  525         WRITE (TIMEID,FORMAT1) TIME(1)
  526       ! if S3=>YYYYMM then filetime='YYYYMM'
  527       ! or S3=>YYYY then filetime='YYYY'
  528       ELSE
  529         WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
  530         WRITE (TIMEID,FORMAT1) FLOOR(REAL(TIME(1))/NINT(10.**(8-S3)))
  531       END IF
  532       ! redefines filename with updated date format
  533       S1=LEN_TRIM(FILEPREFIX)
  534       FNAMENC=''
  535       FNAMENC(1:S1)=FILEPREFIX(1:S1)
  536       FNAMENC(S1+1:S1+S4) = TIMEID(1:S4)
  537                                         
  538                                         
  539 ! 1.2 Setups the output type 4 ( NetCDF file )
  540                                               
  541       ENAME='.trck'
  542       S2=LEN_TRIM(ENAME)
  543       S1=LEN_TRIM(FILEPREFIX)+S4
  544       FNAMENC(S1+1:50)='       '
  545       FNAMENC(S1+1:S1+1) = '_'
  546                               
  547       ! add variable name in file name
  548       FNAMENC(S1+2:S1+S2) = ENAME(2:S2)
  549                                        
  550       ! Defines the netcdf extension
 
 
                                                         ww3_trnc.F90  page  11
 
 
  551       FNAMENC(S1+S2+1:S1+S2+3) = '.nc'
  552       FNAMENC(S1+S2+4:S1+S2+6) = '   '
  553                                       
  554       ! Defines the dimensions
  555       DIMLN(1)=NF90_UNLIMITED ! time
  556       DIMLN(2)=MK ! frequency
  557       DIMLN(3)=MTH ! direction
  558       DIMLN(4)=32  ! string track name length
  559                                              
  560                                              
  561 ! 1.3 Gets the netcdf id
  562                         
  563       NDSDAT=30
  564       OPEN (NDSDAT,FILE=FNAMENC,status='new',IOSTAT=IRET)
  565       IF (IRET.EQ.0) THEN
  566         ! CLOSE old file
  567         IF (INDEX('0000000000000000',OLDTIMEID).EQ.0 .AND. INDEX(TIMEID,O>
  568           IRET = NF90_REDEF(NCID)
  569           CALL CHECK_ERR(IRET)
  570           IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'stop_date',STRSTOPDATE)
  571           CALL CHECK_ERR(IRET)
  572           IRET=NF90_CLOSE(NCID)
  573           CALL CHECK_ERR(IRET)
  574         END IF
  575         NCID=0
  576       ELSE
  577         NCID=NCID
  578       END IF
  579             
  580             
  581 ! 1.4 Creates the netcdf file
  582                              
  583       IF (NCID.EQ.0) THEN
  584                          
  585         ! Initializes the time iteration counter n
  586         IT      = 0
  587         ILOC    = 0
  588         ISPEC   = 0
  589                    
  590 ! 1.4.1 Creates the NetCDF file
  591                                
  592         CALL W3CRNC(NCTYPE,FNAMENC,NCID,DIMID,DIMLN,VARID)
  593                                                           
  594         ! put start date in global attribute
  595         CALL T2D(TIME,STARTDATE,IERR)
  596         WRITE(STRSTARTDATE,'(I4.4,A,4(I2.2,A),I2.2)') STARTDATE(1),'-',ST>
  597               STARTDATE(3),' ',STARTDATE(5),':',STARTDATE(6),':',STARTDAT>
  598 !
  599         IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'start_date',STRSTARTDATE)
  600         CALL CHECK_ERR(IRET)
  601                             
  602         ! End of define mode of NetCDF file
  603         IRET = NF90_ENDDEF(NCID)
  604         CALL CHECK_ERR(IRET)
  605                             
 
 
                                                         ww3_trnc.F90  page  12
 
 
  606         ! Process lower band and higher band frequencies
  607         FREQ1(1:MK)=FREQ(1:MK)-0.5*(FREQ(1:MK)-(FREQ(1:MK)/XFR))
  608         FREQ2(1:MK)=FREQ(1:MK)+0.5*(-FREQ(1:MK)+(FREQ(1:MK)*XFR))
  609         FREQ1(1)=FREQ(1)
  610         FREQ2(MK)=FREQ(MK)
  611                           
  612         ! Converts direction unit in degree
  613         DTHD=360./MTH
  614         RTH0=TH1/DTH
  615         DO ITH=1, MTH
  616           THD(ITH)=DTHD*(RTH0+REAL(ITH-1))
  617         END DO
  618         DIR(1:MTH)=MOD(360-THD(1:MTH),360.)
  619                                            
  620                                            
  621 ! 1.4.2 Adds general variables to NetCDF file
  622         IRET=NF90_PUT_VAR(NCID,VARID(2),FREQ)
  623         CALL CHECK_ERR(IRET)
  624                             
  625         IRET=NF90_PUT_VAR(NCID,VARID(3),FREQ1)
  626         CALL CHECK_ERR(IRET)
  627                             
  628         IRET=NF90_PUT_VAR(NCID,VARID(4),FREQ2)
  629         CALL CHECK_ERR(IRET)
  630                             
  631         IRET=NF90_PUT_VAR(NCID,VARID(5),DSIP)
  632         CALL CHECK_ERR(IRET)
  633                             
  634         IRET=NF90_PUT_VAR(NCID,VARID(6),DIR)
  635         CALL CHECK_ERR(IRET)
  636                             
  637         WRITE (NDSO,973) FNAMENC
  638                                 
  639       END IF  ! IERR.EQ.0
  640                          
  641                          
  642 ! 1.5 Defines the current time step and index
  643                                              
  644       CALL T2D(TIME,CURDATE,IERR)
  645       OUTJULDAY=TSUB(REFDATE,CURDATE)
  646       WRITE(NDSO,'(3A,I6,A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2,2A)')   >
  647             'Writing new record ', ENAME(2:) ,'number ',IT,              >
  648             ' for ',CURDATE(1),':',CURDATE(2),':',CURDATE(3),'T',CURDATE(>
  649             ':',CURDATE(6),':',CURDATE(7),' in file ',TRIM(FNAMENC)
  650                                                                    
  651                                                                    
  652 !
  653 ! 1.6 Exit from W3EXNC if not sea point
  654 !
  655       IF ( TSTSTR .NE. 'SEA' ) GOTO 888
  656                                        
  657                                        
  658 !
  659 ! 1.6.1 Process speed and direction components
  660 !
 
 
                                                         ww3_trnc.F90  page  13
 
 
  661       WAO = SQRT ( WX**2 + WY**2 )
  662       IF ( WAO.GT.1.E-7 ) THEN
  663         WDO = MOD(270.-ATAN2(WY,WX)*RADE,360.)
  664       ELSE
  665         WDO = 0.
  666       END IF
  667             
  668       CAO = SQRT ( CX**2 + CY**2 )
  669       IF ( CAO.GT.1.E-7 ) THEN
  670         CDO = MOD(270.-ATAN2(CY,CX)*RADE,360.)
  671       ELSE
  672         CDO = 0.
  673       END IF
  674             
  675 !
  676 ! 1.7.1 Puts dimensions variables in NetCDF file
  677 !
  678       IT=IT+1
  679       IF ( UST .LT. 0. ) UST = -1.0
  680                                    
  681       ! time
  682       IRET=NF90_PUT_VAR(NCID,VARID(1),OUTJULDAY,start=(/IT/))
  683       CALL CHECK_ERR(IRET)
  684       ! longitude
  685       IRET=NF90_PUT_VAR(NCID,VARID(7),M2KM*X,start=(/IT/))
  686       CALL CHECK_ERR(IRET)
  687       ! latitude
  688       IRET=NF90_PUT_VAR(NCID,VARID(8),M2KM*Y,start=(/IT/))
  689       CALL CHECK_ERR(IRET)
  690                           
  691                           
  692                           
  693 ! 1.7.2 Puts fields in NetCDF file
  694                                   
  695                                   
  696 ! 1.7.2.a Write spectrum
  697                         
  698       IRET=NF90_PUT_VAR(NCID,VARID(9),                               &
  699            TRANSPOSE(SPEC),start=(/1,1,IT/), count=(/MTH,MK,1/))
  700        CALL CHECK_ERR(IRET)
  701                            
  702 ! 1.7.2.b Write the basic stuff
  703                                
  704       ! Write DW (depth)
  705       IRET=NF90_PUT_VAR(NCID, VARID(10),DW ,start=(/IT/))
  706       CALL CHECK_ERR(IRET)
  707       ! Write CAO (current - x direction)
  708       IRET=NF90_PUT_VAR(NCID, VARID(11),CAO ,start=(/IT/))
  709       CALL CHECK_ERR(IRET)
  710       ! Write CDO (current - y direction)
  711       IRET=NF90_PUT_VAR(NCID,VARID(12),CDO ,start=(/IT/))
  712       CALL CHECK_ERR(IRET)
  713       ! Write WAO (wind velocity - x direction)
  714       IRET=NF90_PUT_VAR(NCID,VARID(13),WAO ,start=(/IT/))
  715       CALL CHECK_ERR(IRET)
 
 
                                                         ww3_trnc.F90  page  14
 
 
  716       ! Write WDO (wind velocity - y direction)
  717       IRET=NF90_PUT_VAR(NCID,VARID(14),WDO ,start=(/IT/))
  718       CALL CHECK_ERR(IRET)
  719       ! Write UST (friction velocity)
  720       IRET=NF90_PUT_VAR(NCID,VARID(15),UST,start=(/IT/))
  721       CALL CHECK_ERR(IRET)
  722       ! Write AS (air sea temperature difference)
  723       IRET=NF90_PUT_VAR(NCID,VARID(16),AS ,start=(/IT/))
  724       CALL CHECK_ERR(IRET)
  725       ! Track name
  726       IRET=NF90_PUT_VAR(NCID,VARID(18),TRCKID,start=(/1,IT/),count=(/LEN_>
  727       CALL CHECK_ERR(IRET)
  728                           
  729                           
  730 !
  731   888 CONTINUE
  732 !
  733       RETURN
  734             
  735 !
  736 ! Formats
  737 !
  738   973 FORMAT ( 'NEW NetCDF file was created ',A)
  739                                                 
  740                                                 
  741 !/ End of W3EXNC ----------------------------------------------------- /
  742 !/
  743       END SUBROUTINE W3EXNC
  744                            
  745                            
  746                            
  747                            
  748 !------------------------------------------------------------------------>
  749       SUBROUTINE W3CRNC (NCTYPE,NCFILE,NCID,DIMID,DIMLN,VARID)
  750                                                               
  751       USE NETCDF
  752                 
  753       IMPLICIT NONE
  754                    
  755       INTEGER, INTENT(IN)               :: NCTYPE
  756       CHARACTER*(*), INTENT(IN)         :: NCFILE
  757       INTEGER, INTENT(IN)               :: DIMLN(:)
  758       INTEGER, INTENT(OUT)              :: DIMID(:), VARID(:), NCID
  759       INTEGER                           :: IRET
  760       INTEGER                           :: DEFLATE=1
  761                                                     
  762 !
  763 ! Creation in netCDF3 or netCDF4
  764 !
  765       IF(NCTYPE.EQ.3)  IRET = NF90_CREATE(TRIM(NCFILE), NF90_CLOBBER, NCI>
  766       IF(NCTYPE.EQ.4) IRET = NF90_CREATE(TRIM(NCFILE), NF90_NETCDF4, NCID)
  767       CALL CHECK_ERR(IRET)
  768                           
  769 !
  770 !     Define generals dimensions
 
 
                                                         ww3_trnc.F90  page  15
 
 
  771 !
  772       IRET = NF90_DEF_DIM(NCID, 'time', DIMLN(1), DIMID(1))
  773       CALL CHECK_ERR(IRET)
  774       IRET = NF90_DEF_DIM(NCID, 'frequency', DIMLN(2), DIMID(2))
  775       CALL CHECK_ERR(IRET)
  776       IRET = NF90_DEF_DIM(NCID, 'direction', DIMLN(3), DIMID(3))
  777       CALL CHECK_ERR(IRET)
  778       IRET = NF90_DEF_DIM(NCID, 'string32', DIMLN(4), DIMID(4))
  779       CALL CHECK_ERR(IRET)
  780                           
  781 !
  782 !     define generals variables
  783 !
  784  
  785 !  time
  786       IRET=NF90_DEF_VAR(NCID, 'time', NF90_DOUBLE, (/DIMID(1)/), VARID(1))
  787       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(1), 1, 1, DE>
  788       SELECT CASE (TRIM(CALTYPE))
  789         CASE ('360_day')
  790           IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','time in 360 day ca>
  791         CASE ('365_day')
  792           IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','time in 365 day ca>
  793         CASE ('standard')
  794           IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','julian day (UT)')
  795       END SELECT
  796       IRET=NF90_PUT_ATT(NCID,VARID(1),'standard_name','time')
  797       IRET=NF90_PUT_ATT(NCID,VARID(1),'units','days since 1990-01-01 00:0>
  798       IRET=NF90_PUT_ATT(NCID,VARID(1),'conventions',                   &
  799          'Relative julian days with decimal part (as parts of the day)')
  800       IRET=NF90_PUT_ATT(NCID,VARID(1),'axis','T')
  801       IRET=NF90_PUT_ATT(NCID,VARID(1),'calendar',TRIM(CALTYPE))
  802                                                                
  803 ! frequency
  804       IRET=NF90_DEF_VAR(NCID, 'frequency', NF90_FLOAT, (/DIMID(2)/),VARID>
  805       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(2), 1, 1, DE>
  806       IRET=NF90_PUT_ATT(NCID,VARID(2),'long_name','center frequencies for>
  807       IRET=NF90_PUT_ATT(NCID,VARID(2),'standard_name','frequency')
  808       IRET=NF90_PUT_ATT(NCID,VARID(2),'units','s-1')
  809       IRET=NF90_PUT_ATT(NCID,VARID(2),'scale_factor',1.)
  810       IRET=NF90_PUT_ATT(NCID,VARID(2),'add_offset',0.)
  811       IRET=NF90_PUT_ATT(NCID,VARID(2),'valid_min',0.)
  812       IRET=NF90_PUT_ATT(NCID,VARID(2),'valid_max',10.)
  813       IRET=NF90_PUT_ATT(NCID,VARID(2),'_FillValue',NF90_FILL_FLOAT)
  814       IRET=NF90_PUT_ATT(NCID,VARID(2),'axis','Y')
  815                                                  
  816 !frequency1
  817       IRET=NF90_DEF_VAR(NCID, 'frequency1', NF90_FLOAT, (/DIMID(2)/), VAR>
  818       CALL CHECK_ERR(IRET)
  819         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(3), 1, 1, >
  820       IRET=NF90_PUT_ATT(NCID,VARID(3),'long_name','frequency of lower ban>
  821       IRET=NF90_PUT_ATT(NCID,VARID(3),'standard_name','frequency_of_lower>
  822       IRET=NF90_PUT_ATT(NCID,VARID(3),'globwave_name','frequency_lower_ba>
  823       IRET=NF90_PUT_ATT(NCID,VARID(3),'units','s-1')
  824       IRET=NF90_PUT_ATT(NCID,VARID(3),'scale_factor',1.)
  825       IRET=NF90_PUT_ATT(NCID,VARID(3),'add_offset',0.)
 
 
                                                         ww3_trnc.F90  page  16
 
 
  826       IRET=NF90_PUT_ATT(NCID,VARID(3),'valid_min',0.)
  827       IRET=NF90_PUT_ATT(NCID,VARID(3),'valid_max',10.)
  828       IRET=NF90_PUT_ATT(NCID,VARID(3),'_FillValue',NF90_FILL_FLOAT)
  829       IRET=NF90_PUT_ATT(NCID,VARID(3),'content','Y')
  830       IRET=NF90_PUT_ATT(NCID,VARID(3),'associates','frequency')
  831                                                                
  832 !frequency2
  833       IRET=NF90_DEF_VAR(NCID, 'frequency2', NF90_FLOAT, (/DIMID(2)/), VAR>
  834       CALL CHECK_ERR(IRET)
  835         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(4), 1, 1, >
  836       IRET=NF90_PUT_ATT(NCID,VARID(4),'long_name','frequency of upper ban>
  837       IRET=NF90_PUT_ATT(NCID,VARID(4),'standard_name','frequency_of_upper>
  838       IRET=NF90_PUT_ATT(NCID,VARID(4),'globwave_name','frequency_upper_ba>
  839       IRET=NF90_PUT_ATT(NCID,VARID(4),'units','s-1')
  840       IRET=NF90_PUT_ATT(NCID,VARID(4),'scale_factor',1.)
  841       IRET=NF90_PUT_ATT(NCID,VARID(4),'add_offset',0.)
  842       IRET=NF90_PUT_ATT(NCID,VARID(4),'valid_min',0.)
  843       IRET=NF90_PUT_ATT(NCID,VARID(4),'valid_max',10.)
  844       IRET=NF90_PUT_ATT(NCID,VARID(4),'_FillValue',NF90_FILL_FLOAT)
  845       IRET=NF90_PUT_ATT(NCID,VARID(4),'content','Y')
  846       IRET=NF90_PUT_ATT(NCID,VARID(4),'associates','frequency')
  847                                                                
  848 ! frequency area
  849       IRET=NF90_DEF_VAR(NCID, 'frequency_area', NF90_FLOAT,(/DIMID(2)/),V>
  850       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(5), 1, 1, DE>
  851       IRET=NF90_PUT_ATT(NCID,VARID(5),'long_name','frequency spectral bin>
  852       IRET=NF90_PUT_ATT(NCID,VARID(5),'standard_name','frequency_area')
  853       IRET=NF90_PUT_ATT(NCID,VARID(5),'units','s-2')
  854       IRET=NF90_PUT_ATT(NCID,VARID(5),'scale_factor',1.)
  855       IRET=NF90_PUT_ATT(NCID,VARID(5),'add_offset',0.)
  856       IRET=NF90_PUT_ATT(NCID,VARID(5),'valid_min',0.)
  857       IRET=NF90_PUT_ATT(NCID,VARID(5),'valid_max',10.)
  858       IRET=NF90_PUT_ATT(NCID,VARID(5),'_FillValue',NF90_FILL_FLOAT)
  859       IRET=NF90_PUT_ATT(NCID,VARID(5),'content','Y')
  860       IRET=NF90_PUT_ATT(NCID,VARID(5),'associates','frequency')
  861                                                                
  862 ! direction
  863       IRET=NF90_DEF_VAR(NCID, 'direction', NF90_FLOAT, (/DIMID(3)/),VARID>
  864       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DE>
  865       IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','sea surface wave to di>
  866       IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave_t>
  867       IRET=NF90_PUT_ATT(NCID,VARID(6),'units','degree')
  868       IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
  869       IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
  870       IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
  871       IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',360.)
  872       IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
  873       IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Z')
  874                                                  
  875       IF (FLAGLL) THEN
  876 !  longitude
  877         IRET=NF90_DEF_VAR(NCID, 'longitude', NF90_FLOAT, (/DIMID(1)/),VAR>
  878         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
  879         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','longitude')
  880         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','longitude')
 
 
                                                         ww3_trnc.F90  page  17
 
 
  881         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','degree_east')
  882         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',-180.0)
  883         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',360.)
  884         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
  885         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','T')
  886         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time')
  887                                                             
  888                                                             
  889 !  latitude
  890         IRET=NF90_DEF_VAR(NCID, 'latitude', NF90_FLOAT, (/DIMID(1)/),VARI>
  891         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
  892         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','latitude')
  893         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','latitude')
  894         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','degree_north')
  895         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',-90.0)
  896         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',180.)
  897         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
  898         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','T')
  899         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time')
  900       ELSE
  901 !  longitude
  902         IRET=NF90_DEF_VAR(NCID, 'x', NF90_FLOAT, (/DIMID(1)/),VARID(7))
  903         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
  904         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','x')
  905         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','x')
  906         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m')
  907         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
  908         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','T')
  909         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time')
  910                                                             
  911 !  latitude
  912         IRET=NF90_DEF_VAR(NCID, 'y', NF90_FLOAT, (/DIMID(1)/),VARID(8))
  913         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
  914         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','y')
  915         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','y')
  916         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m')
  917         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
  918         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','T')
  919         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time')
  920                                                             
  921       END IF
  922             
  923             
  924 ! Efth
  925       IRET=NF90_DEF_VAR(NCID,'efth',NF90_FLOAT,(/DIMID(3),DIMID(2),DIMID(>
  926             IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1,>
  927       IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name',                     &
  928          'sea surface wave directional variance spectral density')
  929       IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name',                 &
  930          'sea_surface_wave_directional_variance_spectral_density')
  931       IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name',                 &
  932          'directional_variance_spectral_density')
  933       IRET=NF90_PUT_ATT(NCID,VARID(9),'units','m2 s rad-1')
  934       IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
  935       IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 
 
                                                         ww3_trnc.F90  page  18
 
 
  936       IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
  937       IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',10.)
  938       IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
  939       IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TYZ')
  940       IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time frequency direct>
  941                                                                           
  942 ! DW - depth
  943       IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/DIMID(1)/),VARID(10))
  944       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, D>
  945       IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','depth')
  946       IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','depth')
  947       IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','depth')
  948       IRET=NF90_PUT_ATT(NCID,VARID(10),'units','m')
  949       IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
  950       IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
  951       IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
  952       IRET=NF90_PUT_ATT(NCID,VARID(10),'content','T')
  953       IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time')
  954                                                            
  955 ! CAO - current speed (m/s)
  956       IRET=NF90_DEF_VAR(NCID, 'cur', NF90_FLOAT,(/DIMID(1)/), VARID(11))
  957       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1, D>
  958       IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','sea water speed')
  959       IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','sea_water_speed')
  960       IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','sea_water_speed')
  961       IRET=NF90_PUT_ATT(NCID,VARID(11),'units','m s-1')
  962       IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
  963       IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
  964       IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
  965       IRET=NF90_PUT_ATT(NCID,VARID(11),'content','T')
  966       IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time')
  967                                                            
  968 ! CDO - current direction (degree)
  969       IRET=NF90_DEF_VAR(NCID, 'curdir', NF90_FLOAT,(/DIMID(1)/), VARID(12>
  970       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1, D>
  971       IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','direction from of sea>
  972       IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','direction_of_sea_>
  973       IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','direction_of_sea_>
  974       IRET=NF90_PUT_ATT(NCID,VARID(12),'units','degree')
  975       IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
  976       IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
  977       IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
  978       IRET=NF90_PUT_ATT(NCID,VARID(12),'content','T')
  979       IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time')
  980                                                            
  981 ! WAO - wind speed (m/s)
  982       IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT,(/DIMID(1)/), VARID(13))
  983       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1, D>
  984       IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','wind speed at 10m')
  985       IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','wind_speed')
  986       IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','wind_speed')
  987       IRET=NF90_PUT_ATT(NCID,VARID(13),'units','m s-1')
  988       IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
  989       IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
  990       IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue',NF90_FILL_FLOAT)
 
 
                                                         ww3_trnc.F90  page  19
 
 
  991       IRET=NF90_PUT_ATT(NCID,VARID(13),'content','T')
  992       IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time')
  993                                                            
  994 ! WDO - wind direction (degree)
  995       IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT,(/DIMID(1)/), VARID(14>
  996       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1, D>
  997       IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','wind direction')
  998       IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','wind_from_directi>
  999       IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','wind_from_directi>
 1000       IRET=NF90_PUT_ATT(NCID,VARID(14),'units','m s-1')
 1001       IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
 1002       IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
 1003       IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue',NF90_FILL_FLOAT)
 1004       IRET=NF90_PUT_ATT(NCID,VARID(14),'content','T')
 1005       IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time')
 1006                                                            
 1007 ! UST - friction velocity  (m/s)
 1008       IRET=NF90_DEF_VAR(NCID, 'ust', NF90_FLOAT,(/DIMID(1)/), VARID(15))
 1009       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1, D>
 1010       IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','friction velocity')
 1011       IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','friction_velocity>
 1012       IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','friction_velocity>
 1013       IRET=NF90_PUT_ATT(NCID,VARID(15),'units','m s-1')
 1014       IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
 1015       IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
 1016       IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue',NF90_FILL_FLOAT)
 1017       IRET=NF90_PUT_ATT(NCID,VARID(15),'content','T')
 1018       IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time')
 1019                                                            
 1020 ! AS - air-sea temperature difference (deg C)
 1021       IRET=NF90_DEF_VAR(NCID, 'ast',NF90_FLOAT,(/DIMID(1)/), VARID(16))
 1022       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(16), 1, 1, D>
 1023       IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name','air sea temperature d>
 1024       IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name','air_sea_temperatu>
 1025       IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','air_sea_temperatu>
 1026       IRET=NF90_PUT_ATT(NCID,VARID(16),'units','degree')
 1027       IRET=NF90_PUT_ATT(NCID,VARID(16),'scale_factor',1.)
 1028       IRET=NF90_PUT_ATT(NCID,VARID(16),'add_offset',0.)
 1029       IRET=NF90_PUT_ATT(NCID,VARID(16),'_FillValue',NF90_FILL_FLOAT)
 1030       IRET=NF90_PUT_ATT(NCID,VARID(16),'content','T')
 1031       IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time')
 1032                                                            
 1033 !  string32
 1034       IRET=NF90_DEF_VAR(NCID, 'string32', NF90_INT, (/DIMID(4)/), VARID(1>
 1035       CALL CHECK_ERR(IRET)
 1036       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(17), 1, 1, D>
 1037       IRET=NF90_PUT_ATT(NCID,VARID(17),'long_name','track_name number of >
 1038       IRET=NF90_PUT_ATT(NCID,VARID(17),'_FillValue',NF90_FILL_INT)
 1039       IRET=NF90_PUT_ATT(NCID,VARID(17),'axis','W')
 1040                                                   
 1041 !  track_name
 1042       IRET=NF90_DEF_VAR(NCID, 'track_name', NF90_CHAR, (/DIMID(4),DIMID(1>
 1043       CALL CHECK_ERR(IRET)
 1044       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(18), 1, 1, D>
 1045       IRET=NF90_PUT_ATT(NCID,VARID(18),'long_name','track name')
 
 
                                                         ww3_trnc.F90  page  20
 
 
 1046       IRET=NF90_PUT_ATT(NCID,VARID(18),'content','TX')
 1047       IRET=NF90_PUT_ATT(NCID,VARID(18),'associates','time string16')
 1048                                                                     
 1049       RETURN
 1050             
 1051       END SUBROUTINE W3CRNC
 1052                            
 1053 !========================================================================>
 1054                                                                           
 1055       SUBROUTINE CHECK_ERR(IRET)
 1056                                 
 1057       USE NETCDF
 1058       USE W3ODATMD, ONLY: NDSE
 1059       USE W3SERVMD, ONLY: EXTCDE
 1060                                 
 1061       IMPLICIT NONE
 1062                    
 1063       INTEGER IRET
 1064                   
 1065       IF (IRET .NE. NF90_NOERR) THEN
 1066         WRITE(NDSE,*) ' *** WAVEWATCH III ERROR IN TRNC :'
 1067         WRITE(NDSE,*) ' NETCDF ERROR MESSAGE: '
 1068         WRITE(NDSE,*) NF90_STRERROR(IRET)
 1069         CALL EXTCDE ( 59 )
 1070       END IF
 1071       RETURN
 1072             
 1073       END SUBROUTINE CHECK_ERR
 1074                               
 1075 !========================================================================>
 1076                                                                           
 1077                                                                           
 1078 !/
 1079 !/ End of W3TRNC ----------------------------------------------------- /
 1080 !/
 1081       END PROGRAM W3TRNC
 1082                         
 1083                         
 1084                         
 1085                         
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/opt/cray/netcdf-hdf5parallel/4.2.0/pgi/119/include -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
