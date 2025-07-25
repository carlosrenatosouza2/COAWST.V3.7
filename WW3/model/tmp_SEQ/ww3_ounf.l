                                                         ww3_ounf.F90  page   1
 
 
    1 #include "w3macros.h"
    2 #define CHECK_ERR(I) CHECK_ERROR(I, __LINE__)
    3 !/ ------------------------------------------------------------------- /
    4       PROGRAM W3OUNF
    5 !/
    6 !/                  +-----------------------------------+
    7 !/                  | WAVEWATCH III           NOAA/NCEP |
    8 !/                  |           F. Ardhuin              |
    9 !/                  |           M. Accensi              |
   10 !/                  |                        FORTRAN 90 |
   11 !/                  | Last update :         26-Jan-2021 |
   12 !/                  +-----------------------------------+
   13 !/
   14 !/    17-Mar-2010 : Creation                            ( version 3.14_SH>
   15 !/    07-Nov-2011 : Debug for spectral output on UNST   ( version 4.04 )
   16 !/    13-Mar-2012 : Update of NC attributes             ( version 4.04 )
   17 !/    02-Apr-2013 : New structure of output fields.     ( version 4.10 )
   18 !/    02-Jul-2013 : Bug correction for lat in unst grid ( version 4.11 )
   19 !/    02-Nov-2013 : Removes unnecessary IDFM            ( version 4.12 )
   20 !/    30-Apr-2014 : Correct group3 freq dim.            ( version 5.00 )
   21 !/    23-May-2014 : Adding ice fluxes to W3SRCE         ( version 5.01 )
   22 !/    14-Oct-2014 : Keep the output files opened        ( version 5.01 )
   23 !/    27-Aug-2015 : ICEH and ICEF added as output       ( version 5.10 )
   24 !/    10-Jan-2017 : Changes for US3D and USSP output    ( version 6.01 )
   25 !/    01-May-2017 : Adds directional MSS parameters     ( version 6.04 )
   26 !/    01-Mar-2018 : RTD option add variable de-rotation,( version 6.02 )
   27 !/                  standard lat-lons and rotated grid
   28 !/                  metadata
   29 !/    15-May-2018 : Add namelist feature                ( version 6.05 )
   30 !/    06-Jun-2018 : Add DEBUG/SETUP                     ( version 6.04 )
   31 !/    27-Jun-2018 : Updated to handle SMC output.       ( version 6.05 )
   32 !/    26-Jul-2018 : Changed reading of TABIPART         ( version 6.05 )
   33 !/    12-Sep-2018 : Added extra partitioned fields      ( version 6.06 )
   34 !/    25-Sep-2018 : Add WBT parameter                   ( version 6.06 )
   35 !/    28-Mar-2019 : Bugfix to NBIPART check.            ( version 6.07 )
   36 !/    18-Jun-2020 : Support for 360-day calendar.       ( version 7.08 )
   37 !/    07-Oct-2019 : RTD option with standard lat-lon
   38 !/                  grid when nesting to rotated grid   ( version 7.11 )
   39 !/    03-Nov-2020 : Moved NetCDF metadata to separate   ( version 7.12 )
   40 !/                  module.
   41 !/    09-Dec-2020 : Set fixed values for VARID indices  ( version 7.12 )
   42 !/    26-Jan-2021 : Added TP output (derived from fp)   ( version 7.12 )
   43 !/                  and alternative dir/mag output.
   44 !/
   45 !/    Copyright 2009-2013 National Weather Service (NWS),
   46 !/       National Oceanic and Atmospheric Administration.  All rights
   47 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   48 !/       No unauthorized use without permission.
   49 !/
   50 !  1. Purpose :
   51 !
   52 !     Post-processing of grid output to NetCDF files
   53 !
   54 !  2. Method :
   55 !
 
 
                                                         ww3_ounf.F90  page   2
 
 
   56 !     Data is read from the grid output file out_grd.ww3 (raw data)
   57 !     and from the file ww3_ounf.nml or ww3_ounf.inp (NDSI)
   58 !     Model definition and raw data files are read using WAVEWATCH III
   59 !     subroutines. Extra global NetCDF attributes may be read from
   60 !     ASCII file NC_globatt.inp.
   61 !
   62 !     Output types :
   63 !      4 : NetCDF files
   64 !
   65 !  3. Parameters :
   66 !
   67 !  4. Subroutines used :
   68 !
   69 !      Name      Type  Module   Description
   70 !     ----------------------------------------------------------------
   71 !      W3NMOD    Subr. W3GDATMD Set number of model.
   72 !      W3SETG    Subr.   Id.    Point to selected model.
   73 !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
   74 !      W3SETW    Subr.   Id.    Point to selected model for wave data.
   75 !      W2NAUX    Subr. W3ADATMD Set number of model for aux data.
   76 !      W3SETA    Subr.   Id.    Point to selected model for aux data.
   77 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
   78 !      STRACE    Subr.   Id.    Subroutine tracing.
   79 !      NEXTLN    Subr.   Id.    Get next line from input filw
   80 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
   81 !      STME21    Subr. W3TIMEMD Convert time to string.
   82 !      TICK21    Subr.   Id.    Advance time.
   83 !      DSEC21    Func.   Id.    Difference between times.
   84 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
   85 !      W3IOGO    Subr. W3IOGOMD Reading/writing raw gridded data file.
   86 !      W3EXNC    Subr. Internal Execute grid netcdf output.
   87 !     ----------------------------------------------------------------
   88 !
   89 !  5. Called by :
   90 !
   91 !     None, stand-alone program.
   92 !
   93 !  6. Error messages :
   94 !
   95 !     Checks on input, checks in W3IOxx.
   96 !
   97 !  7. Remarks :
   98 !
   99 !     The VARID array stores netCDF variable IDs for all variables in
  100 !     file. The first 20 elements are reserved for dimension/auxiliary
  101 !     variables as defined below:
  102 !
  103 !       Index     Variable
  104 !       =====     ========
  105 !         1       Lon
  106 !         2       Lat
  107 !         3       Time
  108 !         4       Tri (UGRD)
  109 !         5       SMC CX (SMC)
  110 !         6       SMC CY (SMC)
 
 
                                                         ww3_ounf.F90  page   3
 
 
  111 !         7       Standard longitude (SMC/RTD)
  112 !         8       Standard latitude (SMC/RTD)
  113 !         9       Coordinate reference system (upcoming feature / RTD)
  114 !        10       Freq (extradim)
  115 !        11       Forecast period (upcoming feature)
  116 !        12       Forecast reference time (upcoming feature)
  117 !        13-19    [Reserved for future use]
  118 !        20       MAPSTA
  119 !
  120 !    Indices 21 - 300 are for storage of field output variable IDs.
  121 !
  122 !  8. Structure :
  123 !
  124 !     See source code.
  125 !
  126 !  9. Switches :
  127 !
  128 !     !/S     Enable subroutine tracing.
  129 !
  130 ! 10. Source code :
  131 !
  132 !/ ------------------------------------------------------------------- /
  133       USE CONSTANTS
  134                    
  135 !/
  136       USE W3WDATMD, ONLY: W3NDAT, W3SETW
  137       USE W3ADATMD, ONLY: W3NAUX, W3SETA
  138       USE W3ODATMD, ONLY: W3NOUT, W3SETO
  139       USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
  140       USE W3TIMEMD
  141       USE W3IOGRMD, ONLY: W3IOGR
  142       USE W3IOGOMD, ONLY: W3IOGO, W3READFLGRD, W3FLGRDFLAG
  143       USE W3INITMD, ONLY: WWVER, SWITCHES
  144       USE W3ODATMD, ONLY: NAPROC, NOSWLL, PTMETH, PTFCUT
  145 !/
  146       USE W3GDATMD
  147       USE W3WDATMD, ONLY: TIME, WLV, ICE, ICEH, ICEF, BERG, UST, USTDIR
  148       USE W3ADATMD, ONLY: DW, UA, UD, AS, CX, CY, HS, WLM, T0M1, THM,  &
  149                           THS, FP0, THP0, DTDYN, FCUT,                 &
  150                           ABA, ABD, UBA, UBD, SXX, SYY, SXY, USERO,    &
  151                           PHS, PTP, PLP, PDIR, PSI, PWS, PWST, PNR,    &
  152                           PTM1, PT1, PT2, PEP,                         &
  153                           PTHP0, PQP, PSW, PPE, PGW, QP,               &
  154                           TAUOX, TAUOY, TAUWIX,                        &
  155                           TAUWIY, PHIAW, PHIOC, TUSX, TUSY, PRMS, TPMS,&
  156                           USSX, USSY, MSSX, MSSY, MSSD, MSCX, MSCY,    &
  157                           MSCD, CHARN, TWS,                            &
  158                           TAUWNX, TAUWNY, BHD, T02, HSIG, CGE,         &
  159                           T01, BEDFORMS, WHITECAP, TAUBBL, PHIBBL,     &
  160                           CFLTHMAX, CFLXYMAX, CFLKMAX, TAUICE, PHICE,  &
  161                           STMAXE, STMAXD, HMAXE, HCMAXE, HMAXD, HCMAXD,&
  162                           P2SMS, EF, US3D, TH1M, STH1M, TH2M, STH2M,   &
  163                           WN, USSP, WBT
  164       USE W3ODATMD, ONLY: NDSO, NDSE, SCREEN, NOGRP, NGRPP, IDOUT,     &
  165                           UNDEF, FLOGRD, FNMPRE, NOSWLL, NOGE
 
 
                                                         ww3_ounf.F90  page   4
 
 
  166 !
  167       USE W3NMLOUNFMD
  168 !
  169       USE W3OUNFMETAMD, ONLY: INIT_META, TEARDOWN_META, GETMETA,       &
  170                               WRITE_META, WRITE_GLOBAL_META,           &
  171                               META_T, NCVARTYPE
  172 !
  173       USE NETCDF
  174                 
  175                 
  176       IMPLICIT NONE
  177                    
  178 !/
  179 !/ ------------------------------------------------------------------- /
  180 !/ Local parameters
  181 !/
  182       TYPE(NML_FIELD_T)       :: NML_FIELD
  183       TYPE(NML_FILE_T)        :: NML_FILE
  184       TYPE(NML_SMC_T)         :: NML_SMC
  185 !
  186       INTEGER                 :: NDSI, NDSM, NDSOG,                    &
  187                                  NDSTRC, NTRACE, IERR, I, I1F, I2F,    &
  188                                  IOTEST, NOUT,                         &
  189                                  IFI, IFJ, NCTYPE, IX1, IXN, IY1, IYN, &
  190                                  IOUT, S3, IRET, HASNC4,               &
  191                                  NBIPART, CNTIPART, NCVARTYPEI, IPART, &
  192                                  RTDNX, RTDNY
  193       INTEGER                 :: TOUT(2), TDUM(2), STOPDATE(8)
  194 !
  195       INTEGER, ALLOCATABLE    :: TABIPART(:), NCIDS(:,:,:)
  196 !
  197       REAL                    :: DTREQ, DTEST
  198 !
  199       CHARACTER*30            :: STRSTOPDATE, FILEPREFIX, STRINGIPART
  200       CHARACTER*1024          :: FLDOUT
  201       CHARACTER               :: COMSTR*1, IDTIME*23, IDDDAY*11
  202 !
  203       LOGICAL                 :: FLG2D(NOGRP,NGRPP), FLG1D(NOGRP),     &
  204                                  VECTOR, TOGETHER, FLGNML
  205                                                          
  206       LOGICAL                 :: SMCGRD = .FALSE.
  207 !/
  208 !/ ------------------------------------------------------------------- /
  209 !/
  210 ! 1.  IO set-up.
  211 !
  212       CALL W3NMOD ( 1, 6, 6 )
  213       CALL W3SETG ( 1, 6, 6 )
  214       CALL W3NDAT (    6, 6 )
  215       CALL W3SETW ( 1, 6, 6 )
  216       CALL W3NAUX (    6, 6 )
  217       CALL W3SETA ( 1, 6, 6 )
  218       CALL W3NOUT (    6, 6 )
  219       CALL W3SETO ( 1, 6, 6 )
  220 !
 
 
                                                         ww3_ounf.F90  page   5
 
 
  221       NDSI   = 10
  222       NDSM   = 20
  223       NDSOG  = 20
  224 !
  225       NDSTRC =  6
  226       NTRACE = 10
  227       CALL ITRACE ( NDSTRC, NTRACE )
  228 !
  229       WRITE (NDSO,900)
  230                       
  231 !
  232 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  233 ! 2.  Read model definition file.
  234 !
  235       CALL W3IOGR ( 'READ', NDSM )
  236       WRITE (NDSO,920) GNAME
  237 !
  238 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  239 ! 3.  Read general data and first fields from file
  240 !
  241       CALL W3IOGO ( 'READ', NDSOG, IOTEST )
  242 !
  243       WRITE (NDSO,930)
  244       DO IFI=1, NOGRP
  245         DO IFJ=1, NGRPP
  246           IF ( FLOGRD(IFI,IFJ) ) WRITE (NDSO,931) IDOUT(IFI,IFJ)
  247         END DO
  248       END DO
  249 !
  250 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  251 ! 4.  Read requests from input file.
  252 !
  253  
  254 !
  255 ! process ww3_ounf namelist
  256 !
  257       INQUIRE(FILE=TRIM(FNMPRE)//"ww3_ounf.nml", EXIST=FLGNML)
  258       IF (FLGNML) THEN
  259         ! Read namelist
  260         CALL W3NMLOUNF (NDSI, TRIM(FNMPRE)//'ww3_ounf.nml', NML_FIELD, &
  261                         NML_FILE, NML_SMC, IERR)
  262                                                 
  263 ! 4.1 Time setup
  264         READ(NML_FIELD%TIMESTRIDE, *)  DTREQ
  265         READ(NML_FIELD%TIMECOUNT, *)   NOUT
  266         READ(NML_FIELD%TIMESTART, *)   TOUT(1), TOUT(2)
  267                                                        
  268 ! 4.2 Output fields
  269         FLDOUT = NML_FIELD%LIST
  270         CALL W3FLGRDFLAG ( NDSO, SCREEN, NDSE, FLDOUT, FLG1D,       &
  271                            FLG2D, 1, 1, IERR )
  272         IF (IERR.NE.0) GOTO 800
  273                                
  274 ! 4.3 Output type
  275         NCTYPE = NML_FILE%NETCDF
 
 
                                                         ww3_ounf.F90  page   6
 
 
  276         NCVARTYPE = NML_FIELD%TYPE
  277         STRINGIPART = NML_FIELD%PARTITION
  278         TOGETHER = NML_FIELD%SAMEFILE
  279         VECTOR = NML_FIELD%VECTOR
  280         FILEPREFIX = NML_FILE%PREFIX
  281         S3 = NML_FIELD%TIMESPLIT
  282         IF(SMCGRD) THEN
  283         ELSE
  284           IX1 = NML_FILE%IX0
  285           IXN = NML_FILE%IXN
  286           IY1 = NML_FILE%IY0
  287           IYN = NML_FILE%IYN
  288         ENDIF ! SMCGRD
  289       END IF ! FLGNML
  290 !
  291 ! process old ww3_ounf.inp format
  292 !
  293       IF (.NOT. FLGNML) THEN
  294         OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_ounf.inp',STATUS='OLD',ERR=800>
  295         REWIND (NDSI)
  296                      
  297         READ (NDSI,'(A)',END=801,ERR=802,IOSTAT=IERR) COMSTR
  298         IF (COMSTR.EQ.' ') COMSTR = '$'
  299         WRITE (NDSO,901) COMSTR
  300         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  301                                             
  302 ! 4.1 Time setup
  303         READ (NDSI,*,END=801,ERR=802) TOUT, DTREQ, NOUT
  304                                                        
  305 ! 4.2 Output fields
  306         CALL W3READFLGRD ( NDSI, NDSO, SCREEN, NDSE, COMSTR, FLG1D,      &
  307                            FLG2D, 1, 1, IERR )
  308         IF (IERR.NE.0) GOTO 800
  309                                
  310 ! 4.3 Output type
  311         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  312         READ (NDSI,*,END=801,ERR=802) NCTYPE, NCVARTYPE
  313         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  314         READ (NDSI,'(A)',END=801,ERR=802) STRINGIPART
  315         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  316         READ (NDSI,*,END=801,ERR=802) TOGETHER
  317 !        CALL NEXTLN ( COMSTR , NDSI , NDSE )
  318 !        READ (NDSI,*,END=801,ERR=802) VECTOR
  319         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  320         FILEPREFIX= 'ww3.'
  321         READ (NDSI,*,END=801,ERR=802) FILEPREFIX
  322         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  323         READ (NDSI,*,END=801,ERR=802) S3
  324         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  325                                             
  326         IF( .NOT. SMCGRD) THEN
  327            READ (NDSI,*,END=801,ERR=802) IX1, IXN, IY1, IYN
  328         ENDIF
  329              
  330         CLOSE(NDSI,ERR=800,IOSTAT=IERR)
 
 
                                                         ww3_ounf.F90  page   7
 
 
  331                                        
  332         VECTOR = .TRUE.  ! Only vector output for .inp files.
  333                                                              
  334       END IF ! .NOT. FLGNML
  335                            
  336       ! Keep track of original NCVARTYPE, as it may change
  337       NCVARTYPEI = NCVARTYPE
  338 !
  339  
  340 ! 4.1 Time setup
  341       DTREQ  = MAX ( 0. , DTREQ )
  342       IF ( DTREQ.EQ.0. ) NOUT = 1
  343       NOUT   = MAX ( 1 , NOUT )
  344       CALL STME21 ( TOUT , IDTIME )
  345       WRITE (NDSO,940) IDTIME
  346       TDUM = 0
  347       CALL TICK21 ( TDUM , DTREQ )
  348       CALL STME21 ( TDUM , IDTIME )
  349       IF ( DTREQ .GE. 86400. ) THEN
  350         WRITE (IDDDAY,'(I10,1X)') INT(DTREQ/86400.)
  351       ELSE
  352         IDDDAY = '           '
  353       END IF
  354       IDTIME(1:11) = IDDDAY
  355       IDTIME(21:23) = '   '
  356       WRITE (NDSO,941) IDTIME, NOUT
  357                                    
  358                                    
  359 ! 4.2 Output fields
  360       DO IFI=1, NOGRP
  361         DO IFJ=1, NGRPP
  362           IF ( FLG2D(IFI,IFJ) ) THEN
  363             IF ( FLOGRD(IFI,IFJ) ) THEN
  364               WRITE (NDSO,946) IDOUT(IFI,IFJ), ' '
  365             ELSE
  366               WRITE (NDSO,946) IDOUT(IFI,IFJ), '*** NOT AVAILABLE ***'
  367               FLG2D(IFI,IFJ) = .FALSE.
  368             END IF
  369           END IF
  370         END DO
  371       END DO
  372             
  373             
  374 ! 4.3 Output type
  375 !!      NBIPART=0
  376 !!      DO I=1,29
  377 !!        IF ((STRINGIPART(I:I+1).EQ.'0').OR.(STRINGIPART(I:I+1).EQ.'1') >
  378 !!            .OR.(STRINGIPART(I:I+1).EQ.'2').OR.(STRINGIPART(I:I+1).EQ.'>
  379 !!            .OR.(STRINGIPART(I:I+1).EQ.'4').OR.(STRINGIPART(I:I+1).EQ.'>
  380 !!          NBIPART=NBIPART+1
  381 !!        END IF
  382 !!      END DO
  383 !!      ALLOCATE(TABIPART(NBIPART))
  384 !!      CNTIPART=0
  385 !!      DO I=1,29
 
 
                                                         ww3_ounf.F90  page   8
 
 
  386 !!        IF ((STRINGIPART(I:I+1).EQ.'0').OR.(STRINGIPART(I:I+1).EQ.'1') >
  387 !!            .OR.(STRINGIPART(I:I+1).EQ.'2').OR.(STRINGIPART(I:I+1).EQ.'>
  388 !!            .OR.(STRINGIPART(I:I+1).EQ.'4').OR.(STRINGIPART(I:I+1).EQ.'>
  389 !!          CNTIPART=CNTIPART+1
  390 !!          READ(STRINGIPART(I:I+1),'(I1)') TABIPART(CNTIPART)
  391 !!        END IF
  392 !!      END DO
  393               
  394       ! Alternative processing of TABIPART to capture requests
  395       ! greater than NOSWLL (C.Bunney):
  396       ALLOCATE(TABIPART(NOSWLL + 1))
  397       ALLOCATE(NCIDS(NOGRP,NGRPP,NOSWLL + 1))
  398       NBIPART=0
  399       DO I=1,30
  400         IF(STRINGIPART(I:I) .EQ. ' ') CYCLE
  401         READ(STRINGIPART(I:I),'(I1)') IPART
  402         IF(IPART .GT. NOSWLL) THEN
  403            WRITE(NDSO, 1500) IPART, NOSWLL
  404            CYCLE
  405         ENDIF
  406         NBIPART = NBIPART + 1
  407         IF(NBIPART .GT. NOSWLL + 1) THEN
  408            GOTO 803
  409         ENDIF
  410         TABIPART(NBIPART) = IPART
  411       ENDDO
  412 !
  413       IF ( NCTYPE.LT.3 .OR. NCTYPE.GT.4 ) THEN
  414         WRITE (NDSE,1010) NCTYPE
  415         CALL EXTCDE ( 1 )
  416       END IF
  417       ! if NCTYPE = 4 checking that it is compiled with NC4
  418       HASNC4=0
  419       HASNC4=1
  420       IF ((HASNC4 .EQ. 0).AND.(NCTYPE.EQ.4)) THEN
  421         WRITE (NDSE,1012)
  422         CALL EXTCDE ( 1 )
  423       END IF
  424             
  425             
  426       IF(.NOT. SMCGRD) THEN
  427         IX1    = MAX ( IX1 , 1 )
  428         IXN    = MIN ( IXN , NX )
  429         IY1    = MAX ( IY1 , 1 )
  430         IYN    = MIN ( IYN , NY )
  431         WRITE (NDSO,3940) IX1, IXN, IY1, IYN
  432       ENDIF ! SMCGRD
  433 !
  434 ! 4.4 Initialise meta-data
  435       CALL INIT_META(VECTOR)
  436 !
  437 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  438 ! 5.  Time management.
  439 !
  440       IOUT = 0
 
 
                                                         ww3_ounf.F90  page   9
 
 
  441       NCIDS(:,:,:) = 0
  442       WRITE (NDSO,970)
  443                       
  444                       
  445 ! 5.1 Loops on out_grd.ww3 to read the time and data
  446       DO
  447         DTEST  = DSEC21 ( TIME , TOUT )
  448         IF ( DTEST .GT. 0. ) THEN
  449           CALL W3IOGO ( 'READ', NDSOG, IOTEST )
  450             IF ( IOTEST .EQ. -1 ) THEN
  451               WRITE (NDSO,944)
  452               EXIT
  453             END IF
  454           CYCLE
  455         END IF
  456         IF ( DTEST .LT. 0. ) THEN
  457           CALL TICK21 ( TOUT , DTREQ )
  458           CYCLE
  459         END IF
  460               
  461               
  462 ! 5.1.1 Increments the time counter IOUT
  463         IOUT   = IOUT + 1
  464         CALL STME21 ( TOUT , IDTIME )
  465         WRITE (NDSO,971) IDTIME
  466                                
  467                                
  468 ! 5.1.2  Processes the variable value for the time step IOUT
  469         CALL W3EXNC ( NX, NY, IX1, IXN, IY1, IYN, NSEA, FILEPREFIX,   &
  470                       E3DF, P2MSF, US3DF, USSPF, NCTYPE, TOGETHER, NCVART>
  471                       FLG2D, NCIDS, S3, STRSTOPDATE )
  472                                                      
  473 ! 5.1.3 Defines the stop date
  474         CALL T2D(TOUT,STOPDATE,IERR)
  475         WRITE(STRSTOPDATE,'(I4.4,A,4(I2.2,A),I2.2)') STOPDATE(1),'-',STOP>
  476               '-',STOPDATE(3),' ',STOPDATE(5),':',STOPDATE(6),':',STOPDAT>
  477                                                                           
  478         CALL TICK21 ( TOUT , DTREQ )
  479         IF ( IOUT .GE. NOUT ) EXIT
  480       END DO
  481             
  482       CALL TEARDOWN_META()
  483                           
  484                           
  485 ! 5.2 Closes the netCDF file
  486       IF (TOGETHER .AND. NCIDS(1,1,1).NE.0) THEN
  487         IRET = NF90_REDEF(NCIDS(1,1,1))
  488         CALL CHECK_ERR(IRET)
  489         IRET=NF90_PUT_ATT(NCIDS(1,1,1),NF90_GLOBAL,'stop_date',STRSTOPDAT>
  490         CALL CHECK_ERR(IRET)
  491         IRET=NF90_CLOSE(NCIDS(1,1,1))
  492         CALL CHECK_ERR(IRET)
  493       END IF
  494 !
  495       DO IFI=1, NOGRP
 
 
                                                         ww3_ounf.F90  page  10
 
 
  496         DO IFJ=1, NGRPP
  497           IF ( FLG2D(IFI,IFJ) ) THEN
  498             IF ( FLOGRD(IFI,IFJ) ) THEN
  499               IF (.NOT. TOGETHER) THEN
  500                 IF (NCIDS(IFI,IFJ,1).NE.0) THEN
  501                   IRET = NF90_REDEF(NCIDS(IFI,IFJ,1))
  502                   CALL CHECK_ERR(IRET)
  503                   IRET=NF90_PUT_ATT(NCIDS(IFI,IFJ,1),NF90_GLOBAL,'stop_da>
  504                   CALL CHECK_ERR(IRET)
  505                   IRET=NF90_CLOSE(NCIDS(IFI,IFJ,1))
  506                   CALL CHECK_ERR(IRET)
  507                 END IF ! NCIDS
  508                 ! close partition files (except part 0 which is already c>
  509                 IF ((IFI.EQ.4).AND.(IFJ.LE.NOGE(IFI))) THEN
  510                   DO IPART=1,NOSWLL
  511                     IF (NCIDS(IFI,IFJ,IPART+1).NE.0) THEN
  512                       IRET = NF90_REDEF(NCIDS(IFI,IFJ,IPART+1))
  513                       CALL CHECK_ERR(IRET)
  514                       IRET=NF90_PUT_ATT(NCIDS(IFI,IFJ,IPART+1),NF90_GLOBA>
  515                       CALL CHECK_ERR(IRET)
  516                                           
  517                       IRET=NF90_CLOSE(NCIDS(IFI,IFJ,IPART+1))
  518                       CALL CHECK_ERR(IRET)
  519                     END IF ! NCIDS
  520                   END DO ! IPART
  521                 END IF ! partition
  522               ! else if together
  523               ELSE
  524                 ! close frequency file
  525                 IF ( ((IFI.EQ.6).AND.(IFJ.EQ.8)) .OR.                 &
  526                      ((IFI.EQ.6).AND.(IFJ.EQ.9)) .OR.                 &
  527                      (IFI.EQ.3) ) THEN
  528                   IF (NCIDS(IFI,IFJ,1).NE.0) THEN
  529                     IRET = NF90_REDEF(NCIDS(IFI,IFJ,1))
  530                     CALL CHECK_ERR(IRET)
  531                     IRET=NF90_PUT_ATT(NCIDS(IFI,IFJ,1),NF90_GLOBAL,'stop_>
  532                     CALL CHECK_ERR(IRET)
  533                     IRET=NF90_CLOSE(NCIDS(IFI,IFJ,1))
  534                     CALL CHECK_ERR(IRET)
  535                   END IF ! NCIDS
  536                 END IF ! IFI
  537               END IF ! TOGETHER
  538             END IF ! FLOGRD
  539           END IF ! FLG2D
  540         END DO ! IFJ
  541       END DO ! IFI
  542                   
  543 !
  544       GOTO 888
  545 !
  546 ! Escape locations read errors :
  547 !
  548   800 CONTINUE
  549       WRITE (NDSE,1000) IERR
  550       CALL EXTCDE ( 10 )
 
 
                                                         ww3_ounf.F90  page  11
 
 
  551 !
  552   801 CONTINUE
  553       WRITE (NDSE,1001)
  554       CALL EXTCDE ( 11 )
  555 !
  556   802 CONTINUE
  557       WRITE (NDSE,1002) IERR
  558       CALL EXTCDE ( 12 )
  559 !
  560   803 CONTINUE
  561       WRITE (NDSE,1003) NBIPART, NOSWLL
  562       CALL EXTCDE (13)
  563 !
  564   888 CONTINUE
  565       WRITE (NDSO,999)
  566 !
  567 ! Formats
  568 !
  569   900 FORMAT (/15X,'   *** WAVEWATCH III Field output postp. ***   '/ &
  570                15X,'==============================================='/)
  571   901 FORMAT ( '  Comment character is ''',A,''''/)
  572 !
  573   920 FORMAT ( '  Grid name : ',A/)
  574 !
  575   930 FORMAT ( '  Fields in file : '/                                 &
  576                ' --------------------------')
  577   931 FORMAT ( '      ',A)
  578 !
  579   940 FORMAT (/'  Output time data : '/                               &
  580                ' --------------------------------------------------'/ &
  581                '      First time         : ',A)
  582   941 FORMAT ( '      Interval           : ',A/                       &
  583                '      Number of requests : ',I10)
  584   944 FORMAT (/'      End of file reached '/)
  585   946 FORMAT ( '      ',A,2X,A)
  586 !
  587  3940 FORMAT ( '      X range : ',2I7/                                &
  588                '      Y range : ',2I7)
  589 !
  590   970 FORMAT (/'  Generating files '/                                 &
  591                ' --------------------------------------------------')
  592   971 FORMAT ( '      Files for ',A)
  593 !
  594   999 FORMAT (/'  End of program '/                                   &
  595                ' ========================================='/          &
  596                '         WAVEWATCH III Field output '/)
  597 !
  598  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNF : '/               &
  599                '     ERROR IN OPENING INPUT FILE'/                    &
  600                '     IOSTAT =',I5/)
  601 !
  602  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNF : '/               &
  603                '     PREMATURE END OF INPUT FILE'/)
  604 !
  605  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNF : '/               &
 
 
                                                         ww3_ounf.F90  page  12
 
 
  606                '     ERROR IN READING FROM INPUT FILE'/               &
  607                '     IOSTAT =',I5/)
  608 !
  609  1003 FORMAT (/' *** WAVEWATCH III WERROR IN W3EXNC : '/              &
  610                '     OUT OF RANGE REQUEST FOR NBIPART =',I2, /        &
  611                '     MAX SWELL PARTITIONS (NOSW) =',I2 /)
  612 !
  613  1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNF : '/               &
  614                '     ILLEGAL TYPE, NCTYPE =',I4/)
  615  1012 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNF : '/               &
  616                '     NCTYPE = 4 BUT NOT COMPILED WITH NC4'/)
  617 !
  618  1500 FORMAT (/' *** WAVEWATCH III WARNING IN W3EXNC : '/             &
  619                '     IGNORING REQUEST FOR IPART =',I2, /              &
  620                '     MAX SWELL PARTITIONS (NOSW) =',I2 /)
  621 !
  622 !/
  623 !/ Internal subroutine W3EXNC ---------------------------------------- /
  624 !/
  625       CONTAINS
  626 !/ ------------------------------------------------------------------- /
  627       SUBROUTINE W3EXNC ( NX, NY, IX1, IXN, IY1, IYN, NSEA,             &
  628                           FILEPREFIX, E3DF, P2MSF, US3DF, USSPF,NCTYPE, &
  629                           TOGETHER, NCVARTYPEI, FLG2D, NCIDS, S3, STRSTOP>
  630 !/
  631 !/                  +-----------------------------------+
  632 !/                  |           F. Ardhuin              |
  633 !/                  |           M. Accensi              |
  634 !/                  |                        FORTRAN 90 |
  635 !/                  | Last update :         26-Jan-2021 |
  636 !/                  +-----------------------------------+
  637 !/
  638 !/    17-Mar-2010 : Creation                            ( version 3.14_SH>
  639 !/    28-Feb-2013 : New option for float output         ( version 4.08 )
  640 !/    02-Apr-2013 : New structure of output fields.     ( version 4.09 )
  641 !/    12-Apr-2013 : Allows curvilinear grids            ( version 4.10 )
  642 !/    30-Apr-2014 : Correct group3 freq dim.            ( version 5.00 )
  643 !/    23-May-2014 : Adding ice fluxes to W3SRCE         ( version 5.01 )
  644 !/    14-Oct-2014 : Keep the output files opened        ( version 5.01 )
  645 !/    03-Nov-2020 : NetCDF metadata moved to separate   ( version 7.12 )
  646 !/                  module.
  647 !/    09-Dec-2020 : Set fixed values for VARID indices  ( version 7.12 )
  648 !/    26-Jan-2021 : Added TP output (derived from fp)   ( version 7.12 )
  649 !/                  and alternative dir/mag output.
  650 !/
  651 !  1. Purpose :
  652 !
  653 !     Perform actual grid output in NetCDF file.
  654 !
  655 !  3. Parameters :
  656 !
  657 !     Parameter list
  658 !     ----------------------------------------------------------------
  659 !       NX/Y    Int.  I  Grid dimensions.
  660 !       IX1/IXN Int.  I  Grid indexes along X
 
 
                                                         ww3_ounf.F90  page  13
 
 
  661 !       IY1/IYN Int.  I  Grid indexes along Y
  662 !       NSEA    Int.  I  Number of sea points.
  663 !     ----------------------------------------------------------------
  664 !
  665 !     Internal parameters
  666 !     ----------------------------------------------------------------
  667 !       FLTWO   Log.  Flags for two-dimensional field X Y.
  668 !       FLDIR   Log.  Flags for two-dimensional, directional field.
  669 !       FLFRQ   Log.  Flags for frequency array (3D field)
  670 !       X1, X2, XX, XY
  671 !               R.A.  Output fields
  672 !     ----------------------------------------------------------------
  673 !
  674 !  4. Subroutines used :
  675 !
  676 !      Name      Type  Module   Description
  677 !     ----------------------------------------------------------------
  678 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  679 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
  680 !      W3S2XY    Subr.   Id.    Convert from storage to spatial grid.
  681 !      PRTBLK    Subr. W3ARRYMD Print plot of array.
  682 !      OUTA2I    Subr.   Id.    Print array of INTEGERS.
  683 !     ----------------------------------------------------------------
  684 !
  685 !  5. Called by :
  686 !
  687 !     Main program in which it is contained.
  688 !
  689 !  6. Error messages :
  690 !
  691 !       None.
  692 !
  693 !  7. Remarks :
  694 !
  695 !     - Note that arrays CX and CY of the main program now contain
  696 !       the absolute current speed and direction respectively.
  697 !
  698 !  8. Structure :
  699 !
  700 !     See source code.
  701 !
  702 !  9. Switches :
  703 !
  704 !       !/S  Enable subroutine tracing.
  705 !       !/T  Enable test output.
  706 !
  707 ! 10. Source code :
  708 !
  709 !/ ------------------------------------------------------------------- /
  710       USE W3SERVMD, ONLY : W3S2XY, UV_TO_MAG_DIR
  711       USE W3ARRYMD, ONLY : OUTA2I, PRTBLK
  712       USE W3GDATMD, ONLY : SIG, GTYPE, FLAGLL, MAPSTA, MAPST2
  713       USE W3GDATMD, ONLY : NK, UNGTYPE, MAPSF, NTRI, CLGTYPE, RLGTYPE, &
  714                            XGRD, YGRD, SX, SY, X0, Y0, XYB, TRIGP, USSP_WN
  715       USE NETCDF
 
 
                                                         ww3_ounf.F90  page  14
 
 
  716       IMPLICIT NONE
  717                    
  718 !/
  719 !/ ------------------------------------------------------------------- /
  720 !/ Parameter list
  721 !/
  722       INTEGER, INTENT(IN)     :: NX, NY, IX1, IXN, IY1, IYN, NSEA,     &
  723                                  E3DF(3,5), P2MSF(3), US3DF(3),        &
  724                                  USSPF(2), NCTYPE, NCVARTYPEI
  725       CHARACTER(30)           :: FILEPREFIX
  726       LOGICAL, INTENT(IN)     :: TOGETHER
  727       LOGICAL, INTENT(IN)     :: FLG2D(NOGRP,NGRPP)
  728       INTEGER, INTENT(INOUT)  :: NCIDS(NOGRP,NGRPP,NOSWLL + 1), S3
  729       CHARACTER*30,INTENT(IN) :: STRSTOPDATE
  730 !/
  731 !/ ------------------------------------------------------------------- /
  732 !/ Local parameters
  733 !/
  734       INTEGER                 :: IFI, IFJ, MFILL, I, J, ISEA, IX, IY,  &
  735                                  I1, J1, IPART, INDEXIPART, COORDTYPE
  736       INTEGER                 :: S1, S2, S4, S5, NCID, OLDNCID,        &
  737                                  MAPSTAOUT, NDSDAT,                    &
  738                                  NFIELD, N, IRET, IK, EXTRADIM, IVAR,  &
  739                                  IVAR1
  740       INTEGER                 :: DIMID(6), VARID(300), START(4),       &
  741                                  COUNT(4), DIMLN(6),START1D(2),        &
  742                                  COUNT1D(2), DIMFIELD(3),              &
  743                                  STARTDATE(8), CURDATE(8), REFDATE(8), &
  744                                  MAP(NX+1,NY), MP2(NX+1,NY)
  745 !
  746     INTEGER                  :: DEFLATE=1
  747 !
  748       INTEGER, ALLOCATABLE    :: TRIGP2(:,:)
  749       ! Make the below allocatable to avoid stack overflow on some machin>
  750       INTEGER(KIND=2), ALLOCATABLE    :: MX1(:,:), MXX(:,:), MYY(:,:), &
  751                                          MXY(:,:), MAPOUT(:,:)
  752 !
  753       REAL                    :: CABS, UABS, MFILLR
  754 !
  755       REAL,DIMENSION(:),  ALLOCATABLE    :: LON, LAT, FREQ
  756       REAL,DIMENSION(:,:),  ALLOCATABLE  :: LON2D, LAT2D, ANGLD2D
  757       ! Make the below allocatable to avoid stack overflow on some machin>
  758       REAL, ALLOCATABLE       :: X1(:,:), X2(:,:), XX(:,:), XY(:,:),   &
  759                                  XK(:,:,:), XXK(:,:,:), XYK(:,:,:),    &
  760                                  MX1R(:,:), MXXR(:,:), MYYR(:,:),      &
  761                                  MXYR(:,:), AUX1(:)
  762 !
  763       DOUBLE PRECISION        :: OUTJULDAY
  764       DOUBLE PRECISION        :: SXD, SYD, X0D, Y0D
  765 !
  766       CHARACTER*120           :: STR2
  767       CHARACTER*512           :: PARTCOM
  768       !CHARACTER*30            :: UNITVAR(3),FORMAT1
  769       CHARACTER*30            :: FORMAT1
  770       CHARACTER*30            :: STRSTARTDATE
 
 
                                                         ww3_ounf.F90  page  15
 
 
  771       CHARACTER               :: FNAMENC*50,                           &
  772                                  FORMF*11
  773       CHARACTER, SAVE         :: OLDTIMEID*16 = '0000000000000000'
  774       CHARACTER, SAVE         :: TIMEID*16 = '0000000000000000'
  775 !
  776       LOGICAL                 :: FLFRQ, FLDIR, FEXIST, FREMOVE
  777       LOGICAL                 :: CUSTOMFRQ=.FALSE.
  778                                                   
  779       TYPE(META_T)            :: META(3)
  780       !TYPE(META_T)            :: META
  781 !/
  782 !/ ------------------------------------------------------------------- /
  783 !/
  784 !
  785 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  786 ! 1.  Preparations
  787 !
  788       ! Allocate output storage. This is required with the introduction
  789       ! of the SMC grid output as the regridded output grid dimensions co>
  790       ! conceivably be larger than the NX and NY values. Making these (la>
  791       ! arrays allocatable also moves them to the heap and avoids stack
  792       ! overflow issues that can occur on some architectures. (Chris Bunn>
  793       IF(SMCGRD) THEN
  794       ELSE
  795         ALLOCATE(X1(NX+1,NY),X2(NX+1,NY),XX(NX+1,NY),XY(NX+1,NY))
  796         ALLOCATE(XK(NX+1,NY,NK), XXK(NX+1,NY,NK), XYK(NX+1,NY,NK))
  797         ALLOCATE(MX1(NX,NY), MXX(NX,NY), MYY(NX,NY), MXY(NX,NY), MAPOUT(N>
  798         ALLOCATE(MX1R(NX,NY), MXXR(NX,NY), MYYR(NX,NY), MXYR(NX,NY))
  799       ENDIF ! SMCGRD
  800       ALLOCATE(AUX1(NSEA))
  801                           
  802       X1     = UNDEF
  803       X2     = UNDEF
  804       XX     = UNDEF
  805       XY     = UNDEF
  806       ! CB: Dont output MAPSTA for SMC grid - it does not make sense
  807       IF( SMCGRD ) THEN
  808         MAPSTAOUT = 0
  809       ELSE
  810         MAPSTAOUT = 1
  811       ENDIF
  812       NCVARTYPE  = NCVARTYPEI
  813       NDSDAT=30
  814       NCID = 0
  815 !
  816       CALL U2D('days since 1990-01-01 00:00:00',REFDATE,IERR)
  817                                                              
  818 ! 1.1 Set-up transfer files
  819       MFILL  = NF90_FILL_SHORT
  820       MFILLR  = NF90_FILL_FLOAT
  821       IF (GTYPE.NE.UNGTYPE) THEN
  822         COORDTYPE=1
  823       ELSE
  824         COORDTYPE=2
  825       ENDIF
 
 
                                                         ww3_ounf.F90  page  16
 
 
  826            
  827 ! 1.2 Sets the date as ISO8601 convention
  828       ! S3 defines the number of characters in the date for the filename
  829       ! S3=0 -> field, S3=4-> YYYY, S3=6 -> YYYYMM, S3=10 -> YYYYMMDDHH
  830       ! Setups min and max date format
  831       IF (S3.GT.0 .AND. S3.LT.4) S3=4
  832       IF (S3.GT.10) S3=10
  833 !
  834       ! Defines the format of FILETIME
  835       S5=S3-8
  836       S4=S3
  837       OLDTIMEID=TIMEID
  838       ! if S3=>nodate then filetime='field'
  839       IF (S3.EQ.0) THEN
  840         S4=5
  841         TIMEID="field"
  842       ! if S3=>YYYYMMDDHH then filetime='YYYYMMDDTHHZ'
  843       ELSE IF (S3.EQ.10) THEN
  844         S4=S4+2 ! add chars for ISO8601 : day T hours Z
  845         WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I8.8,A1,I',S5,'.',S5,',A1)'
  846         WRITE (TIMEID,FORMAT1) TIME(1), 'T', &
  847                FLOOR(REAL(TIME(2))/NINT(10.**(6-S5))), 'Z'
  848       ! if S3=>YYYYMMDD then filetime='YYYYMMDD'
  849       ELSE IF (S3.EQ.8) THEN
  850         WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
  851         WRITE (TIMEID,FORMAT1) TIME(1)
  852       ! if S3=>YYYYMM then filetime='YYYYMM'
  853       ! or S3=>YYYY then filetime='YYYY'
  854       ELSE
  855         WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
  856         WRITE (TIMEID,FORMAT1) FLOOR(REAL(TIME(1))/NINT(10.**(8-S3)))
  857       END IF
  858       ! redefines filename with updated date format
  859       S1=LEN_TRIM(FILEPREFIX)
  860       FNAMENC=''
  861       FNAMENC(1:S1)=FILEPREFIX(1:S1)
  862       FNAMENC(S1+1:S1+S4) = TIMEID(1:S4)
  863                                         
  864       !
  865        
  866 !
  867 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  868 ! 2.  Loop over output fields.
  869 !
  870  
  871       ! Instanciates the field and group indexes
  872       I1=0
  873       J1=0
  874 !
  875       DO IFI=1, NOGRP
  876         DO IFJ=1, NGRPP
  877           ! If the flag for the variable IFI of the group IFJ is .TRUE.
  878           IF ( FLG2D(IFI,IFJ) ) THEN
  879             ! Instanciates the partition array
  880             INDEXIPART=1
 
 
                                                         ww3_ounf.F90  page  17
 
 
  881             IPART=TABIPART(INDEXIPART)
  882             NFIELD=1 ! Default is one field
  883                                            
  884                                            
  885 !  Loop over IPART for partition variables
  886 555         CONTINUE
  887                     
  888             ! Initializes the index of field and group at the first flag >
  889             IF (I1.EQ.0) I1=IFI
  890             IF (J1.EQ.0) J1=IFJ
  891             FORMF  = '(1X,32I5)'
  892 !
  893 ! 2.1 Set output arrays and parameters
  894 !
  895             ! Initializes the flags for freq and direction dimensions
  896             FLFRQ = .FALSE.
  897             FLDIR = .FALSE.
  898             IF (NCVARTYPEI.EQ.3) NCVARTYPE=2
  899 !
  900             ! Depth
  901             IF ( IFI .EQ. 1 .AND. IFJ .EQ. 1 ) THEN
  902               IF( SMCGRD ) THEN
  903               ELSE ! IF(SMCGRD)
  904                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, DW(1:NSEA)     &
  905                                                             , MAPSF, X1 )
  906               ENDIF
  907                    
  908             ! Surface current
  909             ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 2 ) THEN
  910               !! Note - CX and CY read in from .ww3 file are X-Y vectors
  911 !
  912               IF( .NOT. VECTOR ) THEN
  913                 CALL UV_TO_MAG_DIR(CX(1:NSEA), CY(1:NSEA), NSEA,       &
  914                                    TOLERANCE=0.05, CONV='O')
  915               ENDIF
  916 !
  917               IF( SMCGRD ) THEN
  918               ELSE
  919                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CX(1:NSEA)        &
  920                                                         , MAPSF, XX )
  921                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CY(1:NSEA)        &
  922                                                         , MAPSF, XY )
  923               ENDIF
  924               NFIELD=2
  925 !
  926             ! Wind
  927             ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 3 ) THEN
  928               !! Note - UA and UD read in from .ww3 file are UX,UY
  929 !
  930               IF( .NOT. VECTOR ) THEN
  931                 CALL UV_TO_MAG_DIR(UA(1:NSEA), UD(1:NSEA), NSEA,       &
  932                                  TOLERANCE=1.0, CONV='N')
  933               ENDIF
  934 !
  935               IF( SMCGRD ) THEN
 
 
                                                         ww3_ounf.F90  page  18
 
 
  936               ELSE ! IF(SMCGRD)
  937                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, UA(1:NSEA)        &
  938                                                         , MAPSF, XX )
  939                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, UD(1:NSEA)        &
  940                                                         , MAPSF, XY )
  941               ENDIF
  942               NFIELD=2
  943 !
  944             ! Air-sea temperature difference
  945             ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 4 ) THEN
  946               IF( SMCGRD ) THEN
  947               ELSE ! IF(SMCGRD)
  948                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, AS(1:NSEA)        &
  949                                                         , MAPSF, X1 )
  950               ENDIF
  951 !
  952             ! Sea surface height above sea level
  953             ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 5 ) THEN
  954               IF( SMCGRD ) THEN
  955               ELSE ! IF(SMCGRD)
  956                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WLV   , MAPSF, X1 )
  957               ENDIF
  958 !
  959             ! Sea ice area fraction
  960             ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 6 ) THEN
  961               IF( SMCGRD ) THEN
  962               ELSE
  963                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, ICE(1:NSEA), MAPSF, X>
  964               ENDIF
  965                    
  966             ! Icebergs_damping
  967             ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 7 ) THEN
  968               IF( SMCGRD ) THEN
  969               ELSE
  970                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, BERG   , MAPSF, X1 )
  971               ENDIF
  972               WHERE ( X1.NE.UNDEF) X1 = X1*0.1
  973 !
  974  
  975             ! Significant wave height
  976             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 1 ) THEN
  977               IF (NCVARTYPEI.EQ.3) NCVARTYPE=2
  978               IF( SMCGRD ) THEN
  979               ELSE
  980                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, HS    , MAPSF, X1 )
  981               ENDIF
  982                    
  983             ! Mean wave length
  984             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 2 ) THEN
  985               IF( SMCGRD ) THEN
  986               ELSE
  987                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WLM, MAPSF, X1 )
  988               ENDIF
  989 !
  990             ! Mean period T02
 
 
                                                         ww3_ounf.F90  page  19
 
 
  991             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 3 ) THEN
  992               IF( SMCGRD ) THEN
  993               ELSE
  994                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, T02, MAPSF, X1 )
  995               ENDIF
  996 !
  997             ! Mean period T0m1
  998             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 4 ) THEN
  999               IF( SMCGRD ) THEN
 1000               ELSE
 1001                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, T0M1, MAPSF, X1 )
 1002               ENDIF
 1003 !
 1004             ! Mean period T01
 1005             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 5 ) THEN
 1006               IF( SMCGRD ) THEN
 1007               ELSE
 1008                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, T01   , MAPSF, X1 )
 1009               ENDIF
 1010 !
 1011             ! Wave peak frequency
 1012             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 6 ) THEN
 1013               IF( SMCGRD ) THEN
 1014               ELSE
 1015                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, FP0   , MAPSF, X1 )
 1016               ENDIF
 1017 !
 1018             ! Wave mean direction
 1019             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 7 ) THEN
 1020               IF( SMCGRD ) THEN
 1021               ELSE
 1022                  DO ISEA=1, NSEA
 1023                    IF ( THM(ISEA) .NE. UNDEF )  THEN
 1024                      THM(ISEA) = MOD ( 630. - RADE*THM(ISEA) , 360. )
 1025                      END IF
 1026                    END DO
 1027                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, THM, MAPSF, X1 )
 1028               ENDIF
 1029 !
 1030             ! Directional spread
 1031             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 8 ) THEN
 1032               IF( SMCGRD ) THEN
 1033               ELSE
 1034                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, THS   , MAPSF, X1 )
 1035               ENDIF
 1036 !
 1037             ! Peak direction
 1038             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 9 ) THEN
 1039               IF( SMCGRD ) THEN
 1040               ELSE
 1041                  DO ISEA=1, NSEA
 1042                    IF ( THP0(ISEA) .NE. UNDEF ) THEN
 1043                      THP0(ISEA) = MOD ( 630-RADE*THP0(ISEA) , 360. )
 1044                      END IF
 1045                    END DO
 
 
                                                         ww3_ounf.F90  page  20
 
 
 1046                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, THP0  , MAPSF, X1 )
 1047               ENDIF
 1048 !
 1049             ! Infragravity wave height
 1050             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 10 ) THEN
 1051               IF( SMCGRD ) THEN
 1052               ELSE
 1053                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, HSIG, MAPSF, X1 )
 1054               ENDIF
 1055 !
 1056             ! Expected maximum sea surface elevation
 1057             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 11 ) THEN
 1058               IF( SMCGRD ) THEN
 1059               ELSE
 1060                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, STMAXE, MAPSF, X1 )
 1061               ENDIF
 1062 !
 1063             ! Standard deviation of maximum sea surface elevation
 1064             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 12 ) THEN
 1065               IF( SMCGRD ) THEN
 1066               ELSE
 1067                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, STMAXD, MAPSF, X1 )
 1068               ENDIF
 1069 !
 1070             ! Expected maximum wave height
 1071             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 13 ) THEN
 1072               IF( SMCGRD ) THEN
 1073               ELSE
 1074                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, HMAXE, MAPSF, X1 )
 1075               ENDIF
 1076 !
 1077             ! Expected maximum wave height from crest
 1078             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 14 ) THEN
 1079               IF( SMCGRD ) THEN
 1080               ELSE
 1081                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, HCMAXE, MAPSF, X1 )
 1082               ENDIF
 1083 !
 1084             ! STD of maximum wave height
 1085             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 15 ) THEN
 1086               IF( SMCGRD ) THEN
 1087               ELSE
 1088                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, HMAXD, MAPSF, X1 )
 1089               ENDIF
 1090 !
 1091             ! STD of maximum wave height from crest
 1092             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 16 ) THEN
 1093               IF( SMCGRD ) THEN
 1094               ELSE
 1095                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, HCMAXD, MAPSF, X1 )
 1096               ENDIF
 1097 !
 1098             ! Dominant wave breaking probability
 1099             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 17 ) THEN
 1100               IF( SMCGRD ) THEN
 
 
                                                         ww3_ounf.F90  page  21
 
 
 1101               ELSE
 1102                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WBT, MAPSF, X1 )
 1103               END IF
 1104 !
 1105             ! Wave peak period (derived from peak freq field)
 1106             ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 18 ) THEN
 1107               DO I=1,NSEA
 1108                 IF(FP0(I) .NE. UNDEF) THEN
 1109                   AUX1(I) = 1.0 / FP0(I)
 1110                 ELSE
 1111                   AUX1(I) = UNDEF
 1112                 ENDIF
 1113               ENDDO
 1114               IF( SMCGRD ) THEN
 1115               ELSE
 1116                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, AUX1, MAPSF, X1 )
 1117               ENDIF
 1118 !
 1119             ! Wave elevation spectrum
 1120             ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 1 ) THEN
 1121               ! Information for spectral
 1122               FLFRQ  = .TRUE.
 1123               I1F=E3DF(2,1)
 1124               I2F=E3DF(3,1)
 1125               DO IK=I1F,I2F
 1126                 IF( SMCGRD ) THEN
 1127                 ELSE
 1128                    CALL W3S2XY ( NSEA, NSEA, NX+1,NY,EF(:,IK),MAPSF, XX )
 1129                 ENDIF
 1130                 IF (NCVARTYPE.EQ.2) WHERE ( XX.GE.0.) XX = ALOG10(XX+1E-1>
 1131                 XK(:,:,IK)=XX
 1132                 END DO
 1133 !
 1134             ! Mean wave direction frequency spectrum
 1135             ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 2 ) THEN
 1136               ! Information for spectral
 1137               FLFRQ  = .TRUE.
 1138               I1F=E3DF(2,2)
 1139               I2F=E3DF(3,2)
 1140               DO IK=I1F,I2F
 1141                 IF( SMCGRD ) THEN
 1142                 ELSE
 1143                    CALL W3S2XY ( NSEA, NSEA, NX+1,NY,TH1M(:,IK),MAPSF, XX>
 1144                 ENDIF
 1145                 XK(:,:,IK)=XX
 1146                 END DO
 1147 !
 1148             ! Spreading frequency spectrum
 1149             ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 3 ) THEN
 1150               ! Information for spectral
 1151               FLFRQ  = .TRUE.
 1152               I1F=E3DF(2,3)
 1153               I2F=E3DF(3,3)
 1154               DO IK=I1F,I2F
 1155                 IF( SMCGRD ) THEN
 
 
                                                         ww3_ounf.F90  page  22
 
 
 1156                 ELSE
 1157                    CALL W3S2XY ( NSEA, NSEA, NX+1,NY,STH1M(:,IK),MAPSF, X>
 1158                 ENDIF
 1159                 XK(:,:,IK)=XX
 1160                 END DO
 1161 !
 1162             ! Second mean wave direction frequency spectrum
 1163             ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 4 ) THEN
 1164               ! Information for spectral
 1165               FLFRQ  = .TRUE.
 1166               I1F=E3DF(2,4)
 1167               I2F=E3DF(3,4)
 1168               DO IK=I1F,I2F
 1169                 IF( SMCGRD ) THEN
 1170                 ELSE
 1171                    CALL W3S2XY ( NSEA, NSEA, NX+1,NY,TH2M(:,IK),MAPSF, XX>
 1172                 ENDIF
 1173                 XK(:,:,IK)=XX
 1174                 END DO
 1175 !
 1176             ! Second spreading frequency spectrum
 1177             ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 5 ) THEN
 1178               ! Information for spectral
 1179               FLFRQ  = .TRUE.
 1180               I1F=E3DF(2,5)
 1181               I2F=E3DF(3,5)
 1182               DO IK=I1F,I2F
 1183                 IF( SMCGRD ) THEN
 1184                 ELSE
 1185                    CALL W3S2XY ( NSEA, NSEA, NX+1,NY,STH2M(:,IK),MAPSF, X>
 1186                 ENDIF
 1187                 XK(:,:,IK)=XX
 1188                 END DO
 1189 !
 1190             ! Wave numbers
 1191             ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 6 ) THEN
 1192               ! Information for spectral
 1193               FLFRQ  = .TRUE.
 1194               I1F=1
 1195               I2F=NK
 1196               DO IK=1,NK
 1197                 IF( SMCGRD ) THEN
 1198                 ELSE
 1199                    CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WN(IK,:), MAPSF, X>
 1200                 ENDIF
 1201                 XK(:,:,IK)=XX
 1202               END DO
 1203 !
 1204             ! Partition wave significant height
 1205             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 1 ) THEN
 1206               IF( SMCGRD ) THEN
 1207               ELSE
 1208                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PHS(:,IPART), MAPSF, >
 1209               ENDIF
 1210 !
 
 
                                                         ww3_ounf.F90  page  23
 
 
 1211             ! Partition peak period
 1212             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 2 ) THEN
 1213               IF( SMCGRD ) THEN
 1214               ELSE
 1215                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PTP(:,IPART), MAPSF, >
 1216               ENDIF
 1217                    
 1218             ! Partition peak wave length
 1219             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 3 ) THEN
 1220               IF( SMCGRD ) THEN
 1221               ELSE
 1222                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PLP(:,IPART), MAPSF, >
 1223               ENDIF
 1224 !
 1225             ! Partition wave mean direction
 1226             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 4 ) THEN
 1227               IF( SMCGRD ) THEN
 1228               ELSE
 1229                 DO ISEA=1, NSEA
 1230                   IF ( PDIR(ISEA,IPART) .NE. UNDEF ) THEN
 1231                      PDIR(ISEA,IPART) = MOD ( 630-RADE*PDIR(ISEA,IPART) ,>
 1232                   END IF
 1233                 END DO
 1234                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PDIR(:,IPART), MAPSF,>
 1235               ENDIF
 1236 !
 1237             ! Partition directional spread
 1238             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 5 ) THEN
 1239               IF( SMCGRD ) THEN
 1240               ELSE
 1241                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PSI(:,IPART), MAPSF, >
 1242               ENDIF
 1243                    
 1244             ! Partition wind sea fraction
 1245             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 6 ) THEN
 1246               IF( SMCGRD ) THEN
 1247               ELSE
 1248                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PWS(:,IPART), MAPSF, >
 1249               ENDIF
 1250 !
 1251             ! Partition peak direction
 1252             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 7 ) THEN
 1253               IF( SMCGRD ) THEN
 1254               ELSE
 1255                 DO ISEA=1, NSEA
 1256                   IF ( PTHP0(ISEA,IPART) .NE. UNDEF ) THEN
 1257                     PTHP0(ISEA,IPART) = MOD ( 630-RADE*PTHP0(ISEA,IPART) >
 1258                     END IF
 1259                   END DO
 1260                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PTHP0(:,IPART), MAPSF>
 1261               END IF
 1262 !
 1263             ! Partition peakedness
 1264             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 8 ) THEN
 1265               IF( SMCGRD ) THEN
 
 
                                                         ww3_ounf.F90  page  24
 
 
 1266               ELSE
 1267                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PQP(:,IPART), MAPSF, >
 1268               ENDIF
 1269 !
 1270             ! Partition peak enhancement factor
 1271             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 9 ) THEN
 1272               IF( SMCGRD ) THEN
 1273               ELSE
 1274                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PPE(:,IPART), MAPSF, >
 1275               ENDIF
 1276 !
 1277             ! Partition frequency width
 1278             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 10 ) THEN
 1279               IF( SMCGRD ) THEN
 1280               ELSE
 1281                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PGW(:,IPART), MAPSF, >
 1282               ENDIF
 1283 !
 1284             ! Partition spectral width
 1285             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 11 ) THEN
 1286               IF( SMCGRD ) THEN
 1287               ELSE
 1288                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PSW(:,IPART), MAPSF, >
 1289               ENDIF
 1290 !
 1291             ! Partition mean period Tm10
 1292             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 12 ) THEN
 1293               IF( SMCGRD ) THEN
 1294               ELSE
 1295                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PTM1(:,IPART), MAPSF,>
 1296               ENDIF
 1297 !
 1298             ! Partition mean period T01
 1299             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 13 ) THEN
 1300               IF( SMCGRD ) THEN
 1301               ELSE
 1302                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PT1(:,IPART), MAPSF, >
 1303               ENDIF
 1304 !
 1305             ! Partition mean period T02
 1306             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 14 ) THEN
 1307               IF( SMCGRD ) THEN
 1308               ELSE
 1309                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PT2(:,IPART), MAPSF, >
 1310               ENDIF
 1311 !
 1312             ! Partition energy at peak frequency
 1313             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 15 ) THEN
 1314               IF( SMCGRD ) THEN
 1315               ELSE
 1316                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PEP(:,IPART), MAPSF, >
 1317               ENDIF
 1318               NFIELD=1
 1319 !
 1320             ! Partition wind sea fraction
 
 
                                                         ww3_ounf.F90  page  25
 
 
 1321             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 16 ) THEN
 1322               IF( SMCGRD ) THEN
 1323               ELSE
 1324                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PWST(:), MAPSF, X1 )
 1325               ENDIF
 1326 !
 1327             ! Number of wave partitions
 1328             ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 17 ) THEN
 1329               IF( SMCGRD ) THEN
 1330               ELSE
 1331                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PNR(:), MAPSF, X1 )
 1332               ENDIF
 1333 !
 1334             ! Friction velocity
 1335             ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 1 ) THEN
 1336               !! Note - UST and USTDIR read in from .ww3 file are X-Y vec>
 1337               DO ISEA=1, NSEA
 1338                 UABS   = SQRT(UST(ISEA)**2+USTDIR(ISEA)**2)
 1339                  IF (UABS.GE.10.) THEN
 1340                   UST(ISEA)=UNDEF
 1341                   USTDIR(ISEA)=UNDEF
 1342                   END IF
 1343                  END DO
 1344               IF( SMCGRD ) THEN
 1345               ELSE
 1346                 CALL W3S2XY (NSEA,NSEA,NX+1,NY, UST   (1:NSEA), MAPSF, XX>
 1347                 CALL W3S2XY (NSEA,NSEA,NX+1,NY, USTDIR(1:NSEA), MAPSF, XY>
 1348               ENDIF ! SMCGRD
 1349               !! Commented out unnecessary statements below for time being
 1350               !! UST,USTDIR are in north-east convention and X1,X2
 1351               !! are not actually written out below
 1352               !DO ISEA=1, NSEA
 1353               !  UABS   = SQRT(UST(ISEA)**2+USTDIR(ISEA)**2)
 1354               !  IF ( UST(ISEA) .EQ. UNDEF ) THEN
 1355               !      USTDIR(ISEA) = UNDEF
 1356               !      UABS         = UNDEF
 1357               !    ELSE IF ( UABS .GT. 0.05 ) THEN
 1358               !      USTDIR(ISEA) = MOD ( 630. -                     &
 1359               !        RADE*ATAN2(USTDIR(ISEA),UST(ISEA)) , 360. )
 1360               !    ELSE
 1361               !      USTDIR(ISEA) = UNDEF
 1362               !    END IF
 1363               !  UST(ISEA) = UABS
 1364               !  END DO
 1365               !CALL W3S2XY (NSEA,NSEA,NX+1,NY, UST   (1:NSEA) , MAPSF, X1>
 1366               !CALL W3S2XY (NSEA,NSEA,NX+1,NY, USTDIR(1:NSEA) , MAPSF, X2>
 1367               NFIELD=2
 1368 !
 1369             ! Charnock coefficient
 1370             ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 2 ) THEN
 1371               IF( SMCGRD ) THEN
 1372               ELSE
 1373                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CHARN(1:NSEA), MAPSF,>
 1374               ENDIF ! SMCGRD
 1375 !
 
 
                                                         ww3_ounf.F90  page  26
 
 
 1376             ! Wave energy flux
 1377             ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 3 ) THEN
 1378               CGE=CGE*0.001  ! from W / m to kW / m
 1379               IF( SMCGRD ) THEN
 1380               ELSE
 1381                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CGE(1:NSEA), MAPSF, X>
 1382               ENDIF ! SMCGRD
 1383 !
 1384             ! Wind to wave energy flux
 1385             ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 4 ) THEN
 1386               IF (NCVARTYPEI.EQ.3) NCVARTYPE=4
 1387               IF( SMCGRD ) THEN
 1388               ELSE
 1389                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PHIAW(1:NSEA) , MAPSF>
 1390               ENDIF ! SMCGRD
 1391 !
 1392             ! Wave supported wind stress
 1393             ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 5 ) THEN
 1394               IF( SMCGRD ) THEN
 1395               ELSE
 1396                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWIX(1:NSEA)      &
 1397                                                         , MAPSF, XX )
 1398                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWIY(1:NSEA)      &
 1399                                                         , MAPSF, XY )
 1400               ENDIF ! SMCGRD
 1401               !! Commented out unnecessary statements below for time being
 1402               !! TAUWIX, TAUWIY are in north-east convention and X1,X2
 1403               !! are not actually written out below
 1404               !DO ISEA=1, NSEA
 1405               !  CABS   = SQRT(TAUWIX(ISEA)**2+TAUWIY(ISEA)**2)
 1406               !  IF ( CABS .NE. UNDEF ) THEN
 1407               !      TAUWIY(ISEA) = MOD ( 630. -                         &
 1408               !            RADE*ATAN2(TAUWIY(ISEA),TAUWIX(ISEA)) , 360. )
 1409               !    ELSE
 1410               !      TAUWIY(ISEA) = UNDEF
 1411               !    END IF
 1412               !  TAUWIX(ISEA) = CABS
 1413               !  END DO
 1414               !CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWIX, MAPSF, X1 )
 1415               !CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWIY, MAPSF, X2 )
 1416               NFIELD=2
 1417 !
 1418             ! Wave to wind stress
 1419             ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 6 ) THEN
 1420               IF( SMCGRD ) THEN
 1421               ELSE
 1422                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWNX(1:NSEA)   &
 1423                                                         , MAPSF, XX )
 1424                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWNY(1:NSEA)   &
 1425                                                         , MAPSF, XY )
 1426               ENDIF ! SMCGRD
 1427               NFIELD=2
 1428 !
 1429             ! Whitecap coverage
 1430             ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 7 ) THEN
 
 
                                                         ww3_ounf.F90  page  27
 
 
 1431               IF( SMCGRD ) THEN
 1432               ELSE
 1433                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WHITECAP(1:NSEA,1) &
 1434                                                         , MAPSF, X1 )
 1435               ENDIF ! SMCGRD
 1436 !
 1437             ! Whitecap foam thickness
 1438             ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 8 ) THEN
 1439               IF( SMCGRD ) THEN
 1440               ELSE
 1441                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WHITECAP(1:NSEA,2) &
 1442                                                         , MAPSF, X1 )
 1443               ENDIF
 1444 !
 1445             ! Significant breaking wave height
 1446             ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 9 ) THEN
 1447               IF( SMCGRD ) THEN
 1448               ELSE
 1449                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WHITECAP(1:NSEA,3) &
 1450                                                         , MAPSF, X1 )
 1451               ENDIF ! SMCGRD
 1452 !
 1453             ! Whitecap moment
 1454             ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 10 ) THEN
 1455               IF( SMCGRD ) THEN
 1456               ELSE
 1457                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WHITECAP(1:NSEA,4) &
 1458                                                         , MAPSF, X1 )
 1459               ENDIF ! SMCGRD
 1460 !
 1461             ! Wind sea mean period T0M1
 1462             ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 11 ) THEN
 1463               IF( SMCGRD ) THEN
 1464               ELSE
 1465                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TWS(1:NSEA), MAPSF, X>
 1466               ENDIF ! SMCGRD
 1467 !
 1468             ! Radiation stress
 1469             ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 1 ) THEN
 1470                                                         
 1471               IF( SMCGRD ) THEN
 1472               ELSE
 1473               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, SXX(1:NSEA)       &
 1474                                                         , MAPSF, X1 )
 1475               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, SYY(1:NSEA)       &
 1476                                                         , MAPSF, X2 )
 1477               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, SXY(1:NSEA)       &
 1478                                                         , MAPSF, XY )
 1479               ENDIF ! SMCGRD
 1480               NFIELD=3
 1481 !
 1482             ! Wave to ocean stress
 1483             ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 2 ) THEN
 1484               IF( SMCGRD ) THEN
 1485               ELSE
 
 
                                                         ww3_ounf.F90  page  28
 
 
 1486                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUOX(1:NSEA)      &
 1487                                                         , MAPSF, XX )
 1488                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUOY(1:NSEA)      &
 1489                                                         , MAPSF, XY )
 1490               ENDIF ! SMCGRD
 1491               NFIELD=2
 1492 !
 1493             ! Radiation pressure (Bernouilli Head)
 1494             ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 3 ) THEN
 1495               IF( SMCGRD ) THEN
 1496               ELSE
 1497                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, BHD(1:NSEA)  &
 1498                                                         , MAPSF, X1 )
 1499               ENDIF ! SMCGRD
 1500 !
 1501             ! Wave to ocean energy flux
 1502             ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 4 ) THEN
 1503               IF (NCVARTYPEI.EQ.3) NCVARTYPE=4
 1504               DO ISEA=1, NSEA
 1505                  PHIOC(ISEA)=MIN(3000.,PHIOC(ISEA))
 1506                  END DO
 1507               IF( SMCGRD ) THEN
 1508               ELSE
 1509                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PHIOC(1:NSEA)  &
 1510                                                         , MAPSF, X1 )
 1511               ENDIF ! SMCGRD
 1512 !
 1513             ! Stokes transport
 1514             ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 5 ) THEN
 1515               IF( SMCGRD ) THEN
 1516               ELSE
 1517                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TUSX(1:NSEA)      &
 1518                                                         , MAPSF, XX )
 1519                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TUSY(1:NSEA)      &
 1520                                                         , MAPSF, XY )
 1521               ENDIF ! SMCGRD
 1522 ! X1, X2 will not be output when NFIELD == 2
 1523 ! ( Like for .cur, .wnd, .ust, .taw, and .uss ) (CHA at FCOO 2019-06-13):
 1524               !! Commented out unnecessary statements below for time being
 1525               !! (...) X1,X2 are not actually written out below
 1526               !DO ISEA=1, NSEA
 1527               !  CABS   = SQRT(TUSX(ISEA)**2+TUSY(ISEA)**2)
 1528               !  IF ( CABS .NE. UNDEF ) THEN
 1529               !      TUSY(ISEA) = MOD ( 630. -                         &
 1530               !            RADE*ATAN2(TUSY(ISEA),TUSX(ISEA)) , 360. )
 1531               !    ELSE
 1532               !      TUSY(ISEA) = UNDEF
 1533               !    END IF
 1534               !  TUSX(ISEA) = CABS
 1535               !  END DO
 1536               !IF( SMCGRD ) THEN
 1537               !ELSE
 1538               !  CALL W3S2XY ( NSEA, NSEA, NX+1, NY,TUSX,MAPSF, X1 )
 1539               !  CALL W3S2XY ( NSEA, NSEA, NX+1, NY,TUSY,MAPSF, X2 )
 1540               !ENDIF ! SMCGRD
 
 
                                                         ww3_ounf.F90  page  29
 
 
 1541               NFIELD=2
 1542 !
 1543             ! Surface stokes drift
 1544             ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 6 ) THEN
 1545               DO ISEA=1, NSEA
 1546                  USSX(ISEA)=MAX(-0.9998,MIN(0.9998,USSX(ISEA)))
 1547                  USSY(ISEA)=MAX(-0.9998,MIN(0.9998,USSY(ISEA)))
 1548                  END DO
 1549               IF( SMCGRD ) THEN
 1550               ELSE
 1551                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, USSX(1:NSEA)      &
 1552                                                         , MAPSF, XX )
 1553                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, USSY(1:NSEA)      &
 1554                                                         , MAPSF, XY )
 1555               ENDIF ! SMCGRD
 1556               !! Commented out unnecessary statements below for time being
 1557               !! TAUWIX, TAUWIY are in north-east convention and X1,X2
 1558               !! are not actually written out below
 1559               !DO ISEA=1, NSEA
 1560               !  CABS   = SQRT(USSX(ISEA)**2+USSY(ISEA)**2)
 1561               !  IF ( CABS .NE. UNDEF ) THEN
 1562               !      USSY(ISEA) = MOD ( 630. -                         &
 1563               !            RADE*ATAN2(USSY(ISEA),USSX(ISEA)) , 360. )
 1564               !    ELSE
 1565               !      USSY(ISEA) = UNDEF
 1566               !    END IF
 1567               !  USSX(ISEA) = CABS
 1568               !  END DO
 1569               !CALL W3S2XY ( NSEA, NSEA, NX+1, NY,USSX,MAPSF, X1 )
 1570               !CALL W3S2XY ( NSEA, NSEA, NX+1, NY,USSY,MAPSF, X2 )
 1571               NFIELD=2
 1572 !
 1573             ! Power spectral density of equivalent surface pressure
 1574             ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 7 ) THEN
 1575               NFIELD=2
 1576               IF( SMCGRD ) THEN
 1577               ELSE
 1578                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PRMS(1:NSEA)      &
 1579                                                         , MAPSF, XX )
 1580                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TPMS(1:NSEA)      &
 1581                                                         , MAPSF, XY )
 1582               ENDIF ! SMCGRD
 1583 !
 1584             ! Spectral variance of surface stokes drift
 1585             ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 8 ) THEN
 1586               ! Information for spectral distribution of surface Stokes d>
 1587               FLFRQ=.TRUE.
 1588               NFIELD=2
 1589               I1F=US3DF(2)
 1590               I2F=US3DF(3)
 1591               DO IK= I1F,I2F
 1592                 IF( SMCGRD ) THEN
 1593                 ELSE
 1594                   CALL W3S2XY ( NSEA, NSEA, NX+1,NY,US3D(:,IK), MAPSF, XX>
 1595                   CALL W3S2XY ( NSEA, NSEA, NX+1,NY,US3D(:,NK+IK), MAPSF,>
 
 
                                                         ww3_ounf.F90  page  30
 
 
 1596                 ENDIF ! SMCGRD
 1597                 XXK(:,:,IK)=XX
 1598                 XYK(:,:,IK)=XY
 1599               END DO
 1600 !
 1601             ! Base10 logarithm of power spectral density of equivalent su>
 1602             ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ.  9 ) THEN
 1603                 ! Information for spectral microseismic generation data (>
 1604                 FLFRQ=.TRUE.
 1605                 I1F=P2MSF(2)
 1606                 I2F=P2MSF(3)
 1607                 DO IK=I1F,I2F
 1608                   IF( SMCGRD ) THEN
 1609                   ELSE
 1610                     CALL W3S2XY ( NSEA, NSEA, NX+1,NY,P2SMS(:,IK),MAPSF, >
 1611                   ENDIF ! SMCGRD
 1612                   IF (NCVARTYPE.EQ.2) THEN
 1613                      WHERE ( XX.GE.0.) XX = ALOG10(XX*(DWAT*GRAV)**2+1E-1>
 1614                   ELSE
 1615                      WHERE ( XX.GE.0.) XX = XX*(DWAT*GRAV)**2
 1616                   END IF
 1617                         
 1618                   XK(:,:,IK)=XX
 1619                 END DO
 1620 !
 1621             ! Wave to sea ice stress
 1622             ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 10 ) THEN
 1623               IF( SMCGRD ) THEN
 1624               ELSE
 1625                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUICE(1:NSEA,1)     >
 1626                                                         , MAPSF, XX )
 1627                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUICE(1:NSEA,2)     >
 1628                                                         , MAPSF, XY )
 1629               ENDIF ! SMCGRD
 1630               NFIELD=2
 1631 !
 1632             ! Wave to sea ice energy flux
 1633             ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 11 ) THEN
 1634               IF (NCVARTYPEI.EQ.3) NCVARTYPE=4
 1635               DO ISEA=1, NSEA
 1636                  PHIOC(ISEA)=MIN(3000.,PHIOC(ISEA))
 1637                  END DO
 1638               IF( SMCGRD ) THEN
 1639               ELSE
 1640                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PHICE(1:NSEA)  &
 1641                                                         , MAPSF, X1 )
 1642               ENDIF ! SMCGRD
 1643                             
 1644            ! Partitioned surface stokes drift
 1645            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 12 ) THEN
 1646               ! Information for spectral distribution of surface Stokes d>
 1647               FLFRQ=.TRUE.
 1648               IF (USSPF(1)==1) THEN
 1649                  CUSTOMFRQ=.TRUE.
 1650               ENDIF
 
 
                                                         ww3_ounf.F90  page  31
 
 
 1651               NFIELD=2
 1652               I1F=1
 1653               I2F=USSPF(2)
 1654               DO IK= I1F,I2F
 1655                 IF( SMCGRD ) THEN
 1656                 ELSE
 1657                   CALL W3S2XY ( NSEA, NSEA, NX+1,NY,USSP(:,IK), MAPSF, XX>
 1658                   CALL W3S2XY ( NSEA, NSEA, NX+1,NY,USSP(:,NK+IK), MAPSF,>
 1659                 ENDIF ! SMCGRD
 1660                 XXK(:,:,IK)=XX
 1661                 XYK(:,:,IK)=XY
 1662               END DO
 1663 !
 1664             ! RMS of bottom displacement amplitude
 1665             ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 1 ) THEN
 1666               ! NB: ABA and ABD are the X and Y components of the bottom >
 1667               IF( SMCGRD ) THEN
 1668               ELSE
 1669                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, ABA(1:NSEA)     &
 1670                                                         , MAPSF, XX )
 1671                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, ABD(1:NSEA)     &
 1672                                                         , MAPSF, XY )
 1673               ENDIF ! SMCGRD
 1674               NFIELD=2
 1675 !
 1676             ! RMS of bottom velocity amplitude
 1677             ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 2 ) THEN
 1678               ! NB: UBA and UBD are the X and Y components of the bottom >
 1679               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, UBA(1:NSEA)       &
 1680                                                         , MAPSF, XX )
 1681               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, UBD(1:NSEA)       &
 1682                                                         , MAPSF, XY )
 1683               NFIELD=2
 1684 !
 1685             ! Bottom roughness
 1686             ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 3 ) THEN
 1687               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, BEDFORMS(1:NSEA,1)    &
 1688                                                         , MAPSF, X1 )
 1689               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, BEDFORMS(1:NSEA,2)    &
 1690                                                         , MAPSF, X2 )
 1691               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, BEDFORMS(1:NSEA,3)    &
 1692                                                         , MAPSF, XY )
 1693               NFIELD=3
 1694 !
 1695             ! Wave dissipation in bottom boundary layer
 1696             ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 4 ) THEN
 1697               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PHIBBL(1:NSEA)    &
 1698                                                         , MAPSF, X1 )
 1699 !
 1700             ! Wave to bottom boundary layer stress
 1701             ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 5 ) THEN
 1702               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUBBL(1:NSEA,1) &
 1703                                                         , MAPSF, XX )
 1704               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUBBL(1:NSEA,2) &
 1705                                                         , MAPSF, XY )
 
 
                                                         ww3_ounf.F90  page  32
 
 
 1706               NFIELD=2
 1707 !
 1708             ! Mean square slope
 1709             ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 1 ) THEN
 1710               CALL W3S2XY ( NSEA, NSEA, NX+1,NY,MSSX,MAPSF, XX )
 1711               CALL W3S2XY ( NSEA, NSEA, NX+1,NY,MSSY,MAPSF, XY )
 1712               NFIELD=2
 1713 !
 1714             ! Phillips constant
 1715             ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 2 ) THEN
 1716               CALL W3S2XY ( NSEA, NSEA, NX+1,NY,MSCX,MAPSF, XX )
 1717               CALL W3S2XY ( NSEA, NSEA, NX+1,NY,MSCY,MAPSF, XY )
 1718               NFIELD=2
 1719 !
 1720             ! u direction for mss
 1721             ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 3 ) THEN
 1722               DO ISEA=1, NSEA
 1723                 IF ( MSSD(ISEA) .NE. UNDEF )  THEN
 1724                   MSSD(ISEA) = MOD ( 630. - RADE*MSSD(ISEA) , 180. )
 1725                   END IF
 1726                 END DO
 1727               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, MSSD   , MAPSF, X1 )
 1728 !
 1729             ! x direction for msc
 1730             ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 4 ) THEN
 1731               DO ISEA=1, NSEA
 1732                 IF ( MSCD(ISEA) .NE. UNDEF )  THEN
 1733                   MSCD(ISEA) = MOD ( 630. - RADE*MSCD(ISEA) , 180. )
 1734                   END IF
 1735                 END DO
 1736               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, MSCD   , MAPSF, X1 )
 1737 !
 1738              ! Peakedness
 1739              ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 5 ) THEN
 1740               IF( SMCGRD ) THEN
 1741               ELSE
 1742                  CALL W3S2XY ( NSEA, NSEA, NX+1, NY, QP, MAPSF, X1 )
 1743               ENDIF
 1744 !
 1745             ! Dynamic time step
 1746             ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 1 ) THEN
 1747               DO ISEA=1, NSEA
 1748                 IF ( DTDYN(ISEA) .NE. UNDEF ) THEN
 1749                   DTDYN(ISEA) = DTDYN(ISEA) / 60.
 1750                   END IF
 1751                 END DO
 1752               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, DTDYN , MAPSF, X1 )
 1753 !
 1754             ! Cut off frequency
 1755             ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 2 ) THEN
 1756               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, FCUT  , MAPSF, X1 )
 1757 !
 1758             ! Maximum CFL for spatial advection
 1759             ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 3 ) THEN
 1760               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CFLXYMAX  , MAPSF, X1 )
 
 
                                                         ww3_ounf.F90  page  33
 
 
 1761 !
 1762             ! Maximum CFL for direction advection
 1763             ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 4 ) THEN
 1764               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CFLTHMAX  , MAPSF, X1 )
 1765 !
 1766             ! Maximum CFL for frequency advection
 1767             ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 5 ) THEN
 1768               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CFLKMAX  , MAPSF, X1 )
 1769 !
 1770             ! User defined...
 1771             ELSE IF ( IFI .EQ. 10 ) THEN
 1772               !CB WRITE (ENAME,'(A2,I2.2)') '.u', IFJ
 1773               CALL W3S2XY ( NSEA, NSEA, NX+1, NY, USERO(:,IFJ)        &
 1774                                                         , MAPSF, X1 )
 1775             ELSE
 1776               WRITE (NDSE,999) IFI, IFJ
 1777               CALL EXTCDE ( 1 )
 1778 !
 1779             END IF ! IFI AND IFJ
 1780                                 
 1781             ! CB Get netCDF metadata for IFI, IFJ combination (all compon>
 1782             DO I=1,NFIELD
 1783               META(I) = GETMETA(IFI, IFJ, ICOMP=I, IPART=IPART)
 1784             ENDDO
 1785                  
 1786 ! 2.2 Make map
 1787               
 1788             ! CB: TODO - need to handle MAPSTA differently for SMC grid o>
 1789             IF( .NOT. SMCGRD ) THEN
 1790             DO IX=1, NX
 1791               DO IY=1, NY
 1792                 MAPOUT(IX,IY)=INT2(MAPSTA(IY,IX) + 8*MAPST2(IY,IX))
 1793                 IF ( MAPSTA(IY,IX) .EQ. 0 ) THEN
 1794                   X1(IX,IY) = UNDEF
 1795                   X2(IX,IY) = UNDEF
 1796                   XX(IX,IY) = UNDEF
 1797                   XY(IX,IY) = UNDEF
 1798                 END IF
 1799                 IF ( X1(IX,IY) .EQ. UNDEF ) THEN
 1800                   MAP(IX,IY) = 0
 1801                 ELSE
 1802                   MAP(IX,IY) = 1
 1803                 END IF
 1804                 IF ( X2(IX,IY) .EQ. UNDEF ) THEN
 1805                   MP2(IX,IY) = 0
 1806                 ELSE
 1807                   MP2(IX,IY) = 1
 1808                 END IF
 1809               END DO
 1810             END DO
 1811             ENDIF ! CB
 1812                       
 1813                       
 1814 ! 2.3 Setups the output type 4 ( NetCDF file )
 1815                                               
 
 
                                                         ww3_ounf.F90  page  34
 
 
 1816             S2=LEN_TRIM(META(1)%ENAME)
 1817             S1=LEN_TRIM(FILEPREFIX)+S4
 1818             FNAMENC(S1+1:50)='       '
 1819             FNAMENC(S1+1:S1+1) = '_'
 1820                                     
 1821             ! If flag TOGETHER and not variable with freq dim &
 1822             ! (ef, p2l, ...), no variable name in file name
 1823             IF (TOGETHER.AND.(.NOT.FLFRQ)) THEN
 1824               S2=0
 1825             ! If NOT flag TOGETHER or variable with freq dim &
 1826             ! (ef, p2l, ...), add variable name in file name
 1827             ELSE
 1828               FNAMENC(S1+2:S1+S2) = META(1)%ENAME(2:S2)
 1829             ENDIF
 1830             ! Defines the netcdf extension
 1831             FNAMENC(S1+S2+1:S1+S2+3) = '.nc'
 1832             FNAMENC(S1+S2+4:S1+S2+6) = '   '
 1833             ! If the flag frequency is .TRUE., defines the fourth dimensi>
 1834             IF (FLFRQ) THEN
 1835               DIMLN(4)=I2F-I1F+1
 1836               EXTRADIM=1
 1837             ELSE
 1838               DIMLN(4)=0
 1839               EXTRADIM=0
 1840             END IF
 1841                   
 1842             ! If regular grid, initializes the lat/lon or x/y dimension l>
 1843             IF (GTYPE.NE.UNGTYPE) THEN
 1844               IF( SMCGRD ) THEN
 1845               ELSE ! SMCGRD
 1846                 DIMLN(2)=IXN-IX1+1
 1847                 DIMLN(3)=IYN-IY1+1
 1848               ENDIF ! SMCGRD
 1849             ! If unstructured mesh, initializes the nelem,tri dimension l>
 1850             ELSE
 1851               DIMLN(2)=IXN-IX1+1
 1852               DIMLN(3)=NTRI
 1853             ENDIF
 1854                  
 1855             ! Defines index of first field variable
 1856             IVAR1=21
 1857                     
 1858                     
 1859 ! 2.4.1 Save the id of the previous file
 1860                                         
 1861             IF (TOGETHER.AND.(.NOT.FLFRQ)) THEN
 1862               OLDNCID = NCIDS(1,1,1)
 1863             ELSE
 1864               OLDNCID = NCIDS(IFI,IFJ,IPART+1)
 1865             END IF
 1866                   
 1867                   
 1868 ! 2.4.2 Remove the new file (if not created by the run)
 1869                                                        
 1870             INQUIRE(FILE=FNAMENC, EXIST=FEXIST)
 
 
                                                         ww3_ounf.F90  page  35
 
 
 1871             IF (FEXIST) THEN
 1872               FREMOVE = .FALSE.
 1873               ! time splitted condition
 1874               IF (INDEX(TIMEID,OLDTIMEID).EQ.0) THEN
 1875                 ! all variables in the samefile
 1876                 IF (TOGETHER.AND.(.NOT.FLFRQ).AND.NCID.EQ.0) FREMOVE = .T>
 1877                 ! a file per variable
 1878                 IF (.NOT.TOGETHER.OR.FLFRQ) FREMOVE = .TRUE.
 1879               END IF
 1880                     
 1881               IF (FREMOVE) THEN
 1882                 OPEN(UNIT=1234, IOSTAT=IRET, FILE=FNAMENC, STATUS='old')
 1883                 IF (IRET == 0) CLOSE(1234, STATUS='delete')
 1884                 FEXIST=.FALSE.
 1885               ELSE
 1886                 NCID = OLDNCID
 1887               END IF
 1888             END IF
 1889                   
 1890 ! 2.4.3 Finalize the previous file (if a new one will be created)
 1891                                                                  
 1892             IF (.NOT.FEXIST) THEN
 1893               IF (INDEX('0000000000000000',OLDTIMEID).EQ.0 .AND. INDEX(TI>
 1894                 IRET = NF90_REDEF(OLDNCID)
 1895                 CALL CHECK_ERR(IRET)
 1896                 IRET=NF90_PUT_ATT(OLDNCID,NF90_GLOBAL,'stop_date',STRSTOP>
 1897                 CALL CHECK_ERR(IRET)
 1898                 IRET=NF90_CLOSE(OLDNCID)
 1899                 CALL CHECK_ERR(IRET)
 1900               END IF
 1901             END IF
 1902                   
 1903                   
 1904 ! 2.5 Creates the netcdf file
 1905                              
 1906             IF (.NOT.FEXIST) THEN
 1907                                  
 1908               ! Initializes the time dimension length
 1909               DIMLN(1)=1
 1910                         
 1911               ! If NOT unstructure mesh (i.e. regular grid)
 1912 !! CHRISB: VARNM for lat/lon not actually used below.
 1913 !              IF (GTYPE.NE.UNGTYPE) THEN
 1914 !                ! If spherical coordinate
 1915 !                IF (FLAGLL) THEN
 1916 !                  VARNM(NFIELD+1)='Longitude'
 1917 !                  VARNM(NFIELD+2)='Latitude'
 1918 !                ! If cartesian coordinate
 1919 !                ELSE
 1920 !                  VARNM(NFIELD+1)='x'
 1921 !                  VARNM(NFIELD+2)='y'
 1922 !                END IF
 1923 !              END IF
 1924                      
 1925               ! Initializes the time iteration counter n
 
 
                                                         ww3_ounf.F90  page  36
 
 
 1926               N=1
 1927                  
 1928 ! 2.5.1 Creates the NetCDF file
 1929               CALL W3CRNC(FNAMENC,NCID,DIMID,DIMLN,VARID, &
 1930                           EXTRADIM,NCTYPE,MAPSTAOUT)
 1931                                                     
 1932               ! Saves the NCID to keep the file opened to write all the v>
 1933               ! and open/close at each time step
 1934               IF (TOGETHER.AND.(.NOT.FLFRQ)) THEN
 1935                 NCIDS(1,1,1)=NCID
 1936               ELSE
 1937                 NCIDS(IFI,IFJ,IPART+1)=NCID
 1938               END IF
 1939                     
 1940               ! If curvilinear grid, instanciates lat / lon
 1941               IF (GTYPE.EQ.CLGTYPE) THEN
 1942                 IF (.NOT.ALLOCATED(LON2D)) ALLOCATE(LON2D(NX,NY),LAT2D(NX>
 1943                 LON2D=TRANSPOSE(XGRD)
 1944                 LAT2D=TRANSPOSE(YGRD)
 1945                 IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL, &
 1946                                      'latitude_resolution','n/a')
 1947                 CALL CHECK_ERR(IRET)
 1948                 IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL, &
 1949                                      'longitude_resolution','n/a')
 1950                 CALL CHECK_ERR(IRET)
 1951               ! If NOT curvilinear grid,
 1952               ELSE
 1953                 IF( SMCGRD ) THEN
 1954                 ELSE ! SMCGRD
 1955                   ! instanciates lon with x/lon for regular grid or nodes>
 1956                   IF (.NOT.ALLOCATED(LON)) ALLOCATE(LON(NX))
 1957                   IF (.NOT.ALLOCATED(LAT)) THEN
 1958                     ! If regular grid, instanciates lat with y/lat
 1959                     IF (GTYPE.EQ.RLGTYPE) THEN
 1960                       ALLOCATE(LAT(NY))
 1961                     ! If unstructured mesh, instanciates lat with nodes
 1962                     ELSE
 1963                       ALLOCATE(LAT(NX))
 1964                     END IF
 1965                   END IF
 1966                 END IF ! SMCGRD
 1967               END IF
 1968                     
 1969                     
 1970 ! 2.5.2 Generates Lat-Lon arrays
 1971                                 
 1972               ! If regular grid
 1973               IF (GTYPE.EQ.RLGTYPE) THEN
 1974                 IF( SMCGRD ) THEN
 1975                 ELSE ! SMCGRD
 1976                     SXD=DBLE(0.000001d0*DNINT(1d6*(DBLE(SX)) ))
 1977                     SYD=DBLE(0.000001d0*DNINT(1d6*(DBLE(SY)) ))
 1978                     X0D=DBLE(0.000001d0*DNINT(1d6*(DBLE(X0)) ))
 1979                     Y0D=DBLE(0.000001d0*DNINT(1d6*(DBLE(Y0)) ))
 1980                     DO I=1,NX
 
 
                                                         ww3_ounf.F90  page  37
 
 
 1981                       LON(I)=REAL(X0D+SXD*DBLE(I-1))
 1982                     END DO
 1983                     DO I=1,NY
 1984                       LAT(I)=REAL(Y0D+SYD*DBLE(I-1))
 1985                     END DO
 1986                   WRITE(STR2,'(F12.0)') SY
 1987                   STR2=ADJUSTL(STR2)
 1988                   IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,   &
 1989                            'latitude_resolution', TRIM(STR2))
 1990                   CALL CHECK_ERR(IRET)
 1991                   WRITE(STR2,'(F12.0)') SX
 1992                   STR2=ADJUSTL(STR2)
 1993                   IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,   &
 1994                            'longitude_resolution',TRIM(STR2))
 1995                   CALL CHECK_ERR(IRET)
 1996                 END IF ! SMCGRD
 1997               END IF
 1998                     
 1999               ! If unstructured mesh
 2000               IF (GTYPE.EQ.UNGTYPE) THEN
 2001                 LON(:)=XYB(:,1)
 2002                 LAT(:)=XYB(:,2)
 2003                 IF (.NOT.ALLOCATED(TRIGP2)) ALLOCATE(TRIGP2(3,NTRI))
 2004                 DIMLN(2)=NX
 2005                 DIMLN(3)=NTRI
 2006                 TRIGP2=TRANSPOSE(TRIGP)
 2007                 IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL, &
 2008                                      'latitude_resolution','n/a')
 2009                 CALL CHECK_ERR(IRET)
 2010                 IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL, &
 2011                                      'longitude_resolution','n/a')
 2012                 CALL CHECK_ERR(IRET)
 2013               END IF
 2014                     
 2015               ! Finishes declaration part in file by adding geographical >
 2016               IF(SMCGRD) THEN
 2017                 WRITE(STR2,'(F12.0)') MINVAL(LAT)
 2018               ELSE
 2019                 WRITE(STR2,'(F12.0)') MINVAL(YGRD)
 2020               ENDIF
 2021               STR2=ADJUSTL(STR2)
 2022               IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
 2023                    'southernmost_latitude',TRIM(STR2))
 2024               CALL CHECK_ERR(IRET)
 2025                                   
 2026               IF(SMCGRD) THEN
 2027                 WRITE(STR2,'(F12.0)') MAXVAL(LAT)
 2028               ELSE
 2029                 WRITE(STR2,'(F12.0)') MAXVAL(YGRD)
 2030               ENDIF
 2031               STR2=ADJUSTL(STR2)
 2032               IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
 2033                    'northernmost_latitude',TRIM(STR2))
 2034               CALL CHECK_ERR(IRET)
 2035                                   
 
 
                                                         ww3_ounf.F90  page  38
 
 
 2036               IF(SMCGRD) THEN
 2037                 WRITE(STR2,'(F12.0)') MINVAL(LON)
 2038               ELSE
 2039                 WRITE(STR2,'(F12.0)') MINVAL(XGRD)
 2040               ENDIF
 2041               STR2=ADJUSTL(STR2)
 2042               IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
 2043                   'westernmost_longitude',TRIM(STR2))
 2044               CALL CHECK_ERR(IRET)
 2045                                   
 2046                                   
 2047               IF(SMCGRD) THEN
 2048                 WRITE(STR2,'(F12.0)') MAXVAL(LON)
 2049               ELSE
 2050                 WRITE(STR2,'(F12.0)') MAXVAL(XGRD)
 2051               ENDIF
 2052               STR2=ADJUSTL(STR2)
 2053               IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
 2054                   'easternmost_longitude',TRIM(STR2))
 2055               CALL CHECK_ERR(IRET)
 2056               IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
 2057                   'minimum_altitude','-12000 m')
 2058               CALL CHECK_ERR(IRET)
 2059               IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
 2060                   'maximum_altitude','9000 m')
 2061               CALL CHECK_ERR(IRET)
 2062               IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
 2063                   'altitude_resolution','n/a')
 2064               CALL CHECK_ERR(IRET)
 2065                                   
 2066                                   
 2067               CALL T2D(TIME,STARTDATE,IERR)
 2068               WRITE(STRSTARTDATE,'(I4.4,A,4(I2.2,A),I2.2)') STARTDATE(1),>
 2069                     STARTDATE(3),' ',STARTDATE(5),':',STARTDATE(6),':',ST>
 2070                                                                           
 2071               ! End of define mode of NetCDF file
 2072               IRET = NF90_ENDDEF(NCID)
 2073               CALL CHECK_ERR(IRET)
 2074                                   
 2075 ! 2.5.3 Writes longitudes, latitudes, triangles, frequency and status map>
 2076                                                                           
 2077               ! If regular grid
 2078               IF (GTYPE.EQ.RLGTYPE) THEN
 2079                 IF(SMCGRD) THEN ! CB: shelter original code from SMC grid
 2080                 ELSE ! SMCGRD
 2081                   IRET=NF90_PUT_VAR(NCID,VARID(1),LON(IX1:IXN))
 2082                   CALL CHECK_ERR(IRET)
 2083                   IRET=NF90_PUT_VAR(NCID,VARID(2),LAT(IY1:IYN))
 2084                   CALL CHECK_ERR(IRET)
 2085                 ENDIF ! SMCGRD
 2086               END IF
 2087                     
 2088               ! If curvilinear grid
 2089               IF (GTYPE.EQ.CLGTYPE) THEN
 2090                 IRET=NF90_PUT_VAR(NCID,VARID(1),LON2D(IX1:IXN,IY1:IYN))
 
 
                                                         ww3_ounf.F90  page  39
 
 
 2091                 CALL CHECK_ERR(IRET)
 2092                 IRET=NF90_PUT_VAR(NCID,VARID(2),LAT2D(IX1:IXN,IY1:IYN))
 2093                 CALL CHECK_ERR(IRET)
 2094               END IF
 2095                     
 2096               ! If unstructured mesh
 2097               IF (GTYPE.EQ.UNGTYPE) THEN
 2098                 IRET=NF90_PUT_VAR(NCID,VARID(1),LON(IX1:IXN))
 2099                 CALL CHECK_ERR(IRET)
 2100                 IRET=NF90_PUT_VAR(NCID,VARID(2),LAT(IX1:IXN))
 2101                 CALL CHECK_ERR(IRET)
 2102               END IF
 2103                     
 2104               ! Writes frequencies to netcdf file
 2105               IF (EXTRADIM.EQ.1) THEN
 2106                 ALLOCATE(FREQ(I2F-I1F+1))
 2107                 !BGR Here is where we should tell it what frequencies are.
 2108                 IF (CUSTOMFRQ) THEN
 2109                    DO i=1,usspf(2)
 2110                       FREQ(i)=sqrt(GRAV*USSP_WN(i))*TPIINV
 2111                    ENDDO
 2112                 ELSE
 2113                    DO i=1,I2F-I1F+1
 2114                       FREQ(i)=SIG(I1F-1+i)*TPIINV
 2115                    END DO
 2116                 ENDIF
 2117                 IRET=NF90_PUT_VAR(NCID,VARID(10),FREQ)
 2118                 CALL CHECK_ERR(IRET)
 2119                 DEALLOCATE(FREQ)
 2120               END IF
 2121                     
 2122               ! Writes triangles to netcdf file
 2123               IF (GTYPE.EQ.UNGTYPE) THEN
 2124                 IRET=NF90_PUT_VAR(NCID,VARID(4),TRIGP2)
 2125                 CALL CHECK_ERR(IRET)
 2126               END IF
 2127                     
 2128               ! Writes status map array at variable index 2+1+coordtype+i>
 2129               IF (MAPSTAOUT.EQ.1) THEN
 2130                 START(1)=1
 2131                 START(2)=1
 2132                 COUNT(1)=IXN-IX1+1
 2133                 COUNT(2)=IYN-IY1+1
 2134                 IF (GTYPE.NE.UNGTYPE) THEN
 2135                   IRET=NF90_PUT_VAR(NCID,VARID(20),MAPOUT(IX1:IXN,IY1:IYN>
 2136                                      (/START(1:2)/),(/COUNT(1:2)/))
 2137                 ELSE
 2138                   IRET=NF90_PUT_VAR(NCID,VARID(20),MAPOUT(IX1:IXN,1),(/ST>
 2139                 ENDIF
 2140                 CALL CHECK_ERR(IRET)
 2141               END IF
 2142                     
 2143               WRITE (NDSO,973) FNAMENC
 2144                                       
 2145 ! 2.5.4  Defines the field(LON,LAT,time) of the variable (i.e. ucur,vcur >
 
 
                                                         ww3_ounf.F90  page  40
 
 
 2146                                                                           
 2147               IRET = NF90_REDEF(NCID)
 2148               CALL CHECK_ERR(IRET)
 2149               DO I=1,NFIELD
 2150                 IVAR=IVAR1+I
 2151                 IF (COORDTYPE.EQ.1) THEN
 2152                   IF (NCVARTYPE.EQ.2) THEN
 2153                     IF( SMCGRD ) THEN
 2154                     ELSE ! SMCGRD
 2155                       IRET=NF90_DEF_VAR(NCID,META(I)%VARNM, NF90_SHORT, D>
 2156                       CALL CHECK_ERR(IRET)
 2157                            IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(N>
 2158                            CALL CHECK_ERR(IRET)
 2159                     ENDIF ! SMCGRD
 2160                   ELSE
 2161                     IF( SMCGRD ) THEN
 2162                     ELSE ! SMCGRD
 2163                       IRET=NF90_DEF_VAR(NCID,META(I)%VARNM, NF90_FLOAT, D>
 2164                           IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NC>
 2165                           CALL CHECK_ERR(IRET)
 2166                     ENDIF ! SMCGRD
 2167                   END IF
 2168                 ELSE
 2169                   DIMFIELD(1)=DIMID(2)
 2170                   DIMFIELD(2)=DIMID(4)
 2171                   DIMFIELD(3)=DIMID(5)
 2172                   IF (NCVARTYPE.EQ.2) THEN
 2173                     IRET = NF90_DEF_VAR(NCID,META(I)%VARNM, NF90_SHORT, D>
 2174                     CALL CHECK_ERR(IRET)
 2175                         IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID>
 2176                         CALL CHECK_ERR(IRET)
 2177                   ELSE
 2178                     IRET = NF90_DEF_VAR(NCID,META(I)%VARNM, NF90_FLOAT, D>
 2179                     CALL CHECK_ERR(IRET)
 2180                         IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID>
 2181                         CALL CHECK_ERR(IRET)
 2182                   END IF
 2183                 END IF
 2184                       
 2185                 ! Set scale factor to 1.0 if using FLOAT variables for ou>
 2186                 IF(NCVARTYPE .GT. 2) META(I)%FSC = 1.0
 2187                                                       
 2188                 !! CB - USE NEW W3META MODULE
 2189                 CALL WRITE_META(NCID, VARID(IVAR), META(I), IRET) ! CB
 2190                 CALL CHECK_ERR(IRET) ! CB
 2191 !
 2192               END DO
 2193 !
 2194               ! put START date in global attribute
 2195               IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'start_date',STRSTARTDAT>
 2196               CALL CHECK_ERR(IRET)
 2197 !
 2198               IRET = NF90_ENDDEF(NCID)
 2199               CALL CHECK_ERR(IRET)
 2200                                   
 
 
                                                         ww3_ounf.F90  page  41
 
 
 2201                                   
 2202 ! 2.6 Append data to the existing file
 2203                                       
 2204             ELSE  ! FEXIST
 2205                           
 2206 ! 2.6.1 Get the dimensions from the netcdf header
 2207                                                  
 2208               ! If it is an unstructured mesh
 2209               IF (GTYPE.EQ.UNGTYPE) THEN
 2210                 IRET=NF90_INQ_VARID (NCID, 'tri', VARID(4))
 2211                 CALL CHECK_ERR(IRET)
 2212               ! If it is a regular grid
 2213               ELSE
 2214                 ! If it is spherical coordinate
 2215                 IF (FLAGLL) THEN
 2216                   IF(SMCGRD) THEN
 2217                   ELSE
 2218                     IRET=NF90_INQ_DIMID (NCID, 'longitude', DIMID(2))
 2219                     IRET=NF90_INQ_DIMID (NCID, 'latitude', DIMID(3))
 2220                   ENDIF ! SMCGRD
 2221                   IRET=NF90_INQ_VARID (NCID, 'longitude', VARID(1))
 2222                   IRET=NF90_INQ_VARID (NCID, 'latitude', VARID(2))
 2223                 ! If it is cartesian coordinate
 2224                 ELSE
 2225                   IRET=NF90_INQ_DIMID (NCID, 'x', DIMID(2))
 2226                   IRET=NF90_INQ_VARID (NCID, 'x', VARID(1))
 2227                   IRET=NF90_INQ_DIMID (NCID, 'y', DIMID(3))
 2228                   IRET=NF90_INQ_VARID (NCID, 'y', VARID(1))
 2229                 END IF
 2230                 CALL CHECK_ERR(IRET)
 2231               END IF
 2232               ! Get the dimension time
 2233               IRET=NF90_INQ_DIMID (NCID, 'time', DIMID(4+EXTRADIM))
 2234               IRET=NF90_INQUIRE_DIMENSION (NCID, DIMID(4+EXTRADIM),len=N)
 2235               CALL CHECK_ERR(IRET)
 2236               IRET=NF90_INQ_VARID (NCID, 'time', VARID(3))
 2237               ! Get the dimension f
 2238               IF (EXTRADIM.EQ.1) IRET=NF90_INQ_DIMID (NCID, 'f', DIMID(4))
 2239                                                                           
 2240 ! 2.6.2 Increments the time step for existing file
 2241                                                   
 2242               ! If it is the first field of the file in mode together
 2243               ! or NOT together or variable with freq dim (ef or p2l)
 2244               ! ChrisBunney: Also - check IPART=TABIPART in case first
 2245               ! requested output is a partitioned field.
 2246               IF((TOGETHER .AND. IFI.EQ.I1 .AND. IFJ.EQ.J1 .AND. IPART.EQ>
 2247                  .OR.(.NOT.TOGETHER).OR.FLFRQ) n=n+1
 2248                                                     
 2249 ! 2.6.3 Defines or gets the variables identifiers
 2250                                                  
 2251               ! If it is the first time step, define all the variables an>
 2252               IF (N.EQ.1) THEN
 2253                 IRET = NF90_REDEF(NCID)
 2254                 CALL CHECK_ERR(IRET)
 2255                                     
 
 
                                                         ww3_ounf.F90  page  42
 
 
 2256                 ! Loops on all the fields of the variable (i.e. ucur/vcur>
 2257                 DO I=1,NFIELD
 2258                   IVAR=IVAR1+I
 2259                   IF (COORDTYPE.EQ.1) THEN
 2260                     IF (NCVARTYPE.EQ.2) THEN
 2261                       IF( SMCGRD ) THEN
 2262                       ELSE
 2263                          IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_SHO>
 2264                          CALL CHECK_ERR(IRET)
 2265                       ENDIF ! SMCGRD
 2266                       IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, >
 2267                     ELSE
 2268                       IF( SMCGRD ) THEN
 2269                       ELSE
 2270                         IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_FLOA>
 2271                         CALL CHECK_ERR(IRET)
 2272                       ENDIF ! SMCGRD
 2273                       IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, >
 2274                       CALL CHECK_ERR(IRET)
 2275                     END IF
 2276                   ELSE
 2277                     DIMFIELD(1)=DIMID(2)
 2278                     DIMFIELD(2)=DIMID(4)
 2279                     DIMFIELD(3)=DIMID(5)
 2280                     IF (NCVARTYPE.EQ.2) THEN
 2281                       IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_SHORT,>
 2282                       CALL CHECK_ERR(IRET)
 2283                       IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, >
 2284                       CALL CHECK_ERR(IRET)
 2285                     ELSE
 2286                       IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_FLOAT,>
 2287                       CALL CHECK_ERR(IRET)
 2288                       IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, >
 2289                       CALL CHECK_ERR(IRET)
 2290                     END IF
 2291                   END IF
 2292 !
 2293                   ! Set scale factor to 1.0 if using FLOAT variables for >
 2294                   IF(NCVARTYPE .GT. 2) META(I)%FSC = 1.0
 2295                                                         
 2296                   !! CB - USE NEW W3META MODULE
 2297                   CALL WRITE_META(NCID, VARID(IVAR), META(I), IRET) ! CB
 2298                   CALL CHECK_ERR(IRET) ! CB
 2299 !
 2300                 END DO
 2301                 IRET = NF90_ENDDEF(NCID)
 2302                 CALL CHECK_ERR(IRET)
 2303                                     
 2304               ! If it is not the first time step, get all VARID from the >
 2305               ELSE
 2306                 IRET=NF90_REDEF(NCID)
 2307                 CALL CHECK_ERR(IRET)
 2308                 DO I=1,NFIELD
 2309                   ! Get meta-data for field
 2310                   !META = GETMETA(IFI, IFJ, ICOMP=I, IPART=IPART)
 
 
                                                         ww3_ounf.F90  page  43
 
 
 2311                   IVAR=IVAR1+I
 2312                   IRET=NF90_INQ_VARID (NCID, META(I)%VARNM, VARID(IVAR))
 2313                   CALL CHECK_ERR(IRET)
 2314                 END DO
 2315                 IRET=NF90_ENDDEF(NCID)
 2316                 CALL CHECK_ERR(IRET)
 2317               END IF !   N.EQ.1
 2318             END IF  ! IERR.EQ.0
 2319                                
 2320 ! 2.6.4 Defines the current time step and index
 2321                                                
 2322             CALL T2D(TIME,CURDATE,IERR)
 2323             OUTJULDAY=TSUB(REFDATE,CURDATE)
 2324             WRITE(NDSO,'(A,A9,A,I6,A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.>
 2325                     'Writing new record ', META(1)%ENAME(2:) ,'number ',N>
 2326                     ' for ',CURDATE(1),':',CURDATE(2),':',CURDATE(3),'T',>
 2327                     ':',CURDATE(6),':',CURDATE(7),' in file ',TRIM(FNAMEN>
 2328                                                                           
 2329                                                                           
 2330                                                                           
 2331             ! Defines starting point and size of arrays to be written
 2332             START(1)=1
 2333             START(2)=1
 2334             START(3)=1
 2335             START(4)=1
 2336                       
 2337             ! Sets time index
 2338             START(3+1-COORDTYPE+EXTRADIM)=N
 2339             COUNT(1)=IXN-IX1+1
 2340             COUNT(2)=IYN-IY1+1
 2341             COUNT(3)=1
 2342             COUNT(4)=1
 2343             START1D(1)=1
 2344             START1D(2)=N
 2345             COUNT1D(1)=IXN-IX1+1
 2346             COUNT1D(2)=1
 2347                         
 2348             ! Puts time in NetCDF file
 2349             IF((IFI.EQ.I1.AND.IFJ.EQ.J1.AND.TOGETHER)  &
 2350                .OR.(.NOT.TOGETHER).OR.FLFRQ) THEN
 2351               !IVAR1=3+EXTRADIM+(COORDTYPE-1)+MAPSTAOUT
 2352               IVAR1 = 21
 2353               IRET=NF90_PUT_VAR(NCID,VARID(3),OUTJULDAY,(/N/))
 2354               CALL CHECK_ERR(IRET)
 2355             END IF
 2356 !
 2357 ! 2.6.5 Puts field(s) in NetCDF file
 2358                                     
 2359 ! NFIELD=3
 2360             IF (NCVARTYPE.EQ.2) THEN
 2361               IF ( NFIELD.EQ.3 ) THEN
 2362                 IF (SMCGRD) THEN
 2363                 ELSE ! IF(SMCGRD)
 2364                   DO IX=IX1, IXN
 2365                     DO IY=IY1, IYN
 
 
                                                         ww3_ounf.F90  page  44
 
 
 2366                       IF ( MAPSTA(IY,IX) .LE. 0 .OR. X1(IX,IY) .EQ. UNDEF>
 2367                         MXX(IX,IY) = MFILL
 2368                         MYY(IX,IY) = MFILL
 2369                         MXY(IX,IY) = MFILL
 2370                       ELSE
 2371                         MXX(IX,IY) = NINT(X1(IX,IY)/META(1)%FSC)
 2372                         MYY(IX,IY) = NINT(X2(IX,IY)/META(2)%FSC)
 2373                         MXY(IX,IY) = NINT(XY(IX,IY)/META(3)%FSC)
 2374                       END IF
 2375                     END DO
 2376                   END DO
 2377                         
 2378                   IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),              &
 2379                           MXX(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3>
 2380                   CALL CHECK_ERR(IRET)
 2381                   IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),            &
 2382                           MYY(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3>
 2383                   CALL CHECK_ERR(IRET)
 2384                   IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+3),            &
 2385                           MXY(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3>
 2386                   CALL CHECK_ERR(IRET)
 2387                 ENDIF ! SMCGRD
 2388 ! NFIELD=2
 2389               ELSE IF (NFIELD.EQ.2 ) THEN
 2390 ! EXTRADIM=0
 2391                 IF (EXTRADIM.EQ.0) THEN
 2392                   IF (SMCGRD) THEN
 2393                   ELSE ! IF(SMCGRD)
 2394                     DO IX=IX1, IXN
 2395                       DO IY=IY1, IYN
 2396                         IF ( MAPSTA(IY,IX) .LE. 0 .OR. XX(IX,IY) .EQ. UND>
 2397                           MXX(IX,IY) = MFILL
 2398                           MYY(IX,IY) = MFILL
 2399                         ELSE
 2400                           MXX(IX,IY) = NINT(XX(IX,IY)/META(1)%FSC)
 2401                           MYY(IX,IY) = NINT(XY(IX,IY)/META(2)%FSC)
 2402                         END IF
 2403                       END DO
 2404                     END DO
 2405                     IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),             &
 2406                               MXX(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT>
 2407                     CALL CHECK_ERR(IRET)
 2408                     IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),           &
 2409                             MYY(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1>
 2410                     CALL CHECK_ERR(IRET)
 2411                   ENDIF ! SMCGRD
 2412 ! EXTRADIM=1
 2413                 ELSE
 2414                   START(3+1-COORDTYPE)=0
 2415                   DO IK=I1F,I2F
 2416                     START(3+1-COORDTYPE)=START(3+1-COORDTYPE)+1
 2417                                                                
 2418                     IF (SMCGRD) THEN
 2419                     ELSE ! IF(SMCGRD)
 2420                       DO IX=IX1, IXN
 
 
                                                         ww3_ounf.F90  page  45
 
 
 2421                         DO IY=IY1, IYN
 2422                           IF ( MAPSTA(IY,IX) .LE. 0 .OR.XXK(IX,IY,IK) .EQ>
 2423                             MXX(IX,IY) = MFILL
 2424                             MYY(IX,IY) = MFILL
 2425                           ELSE
 2426                             MXX(IX,IY) = NINT(XXK(IX,IY,IK)/META(1)%FSC)
 2427                             MYY(IX,IY) = NINT(XYK(IX,IY,IK)/META(2)%FSC)
 2428                           END IF
 2429                         END DO
 2430                       END DO
 2431                       IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),             >
 2432                               MXX(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUNT>
 2433                       IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),             &
 2434                               MYY(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUNT>
 2435                     ENDIF ! SMCGRD
 2436                   END DO
 2437                 END IF  ! EXTRADIM
 2438 ! NFIELD=1
 2439               ELSE
 2440 ! EXTRADIM=0
 2441                 IF (EXTRADIM.EQ.0) THEN
 2442                   IF (SMCGRD) THEN
 2443                   ELSE ! IF(SMCGRD)
 2444                     DO IX=IX1, IXN
 2445                       DO IY=IY1, IYN
 2446                         IF ( MAPSTA(IY,IX) .LE. 0 .OR.X1(IX,IY) .EQ. UNDE>
 2447                           MX1(IX,IY) = MFILL
 2448                         ELSE
 2449                           MX1(IX,IY) = NINT(X1(IX,IY)/META(1)%FSC)
 2450                         END IF
 2451                       END DO
 2452                     END DO
 2453                     IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
 2454                             MX1(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1>
 2455                     CALL CHECK_ERR(IRET)
 2456                   ENDIF ! SMCGRD
 2457 ! EXTRADIM=1
 2458                 ELSE
 2459                   START(3+1-COORDTYPE)=0
 2460                   DO IK=I1F,I2F
 2461                     START(3+1-COORDTYPE)=START(3+1-COORDTYPE)+1
 2462                                                                
 2463                     IF (SMCGRD) THEN
 2464                     ELSE ! IF(SMCGRD)
 2465                       DO IX=IX1, IXN
 2466                         DO IY=IY1, IYN
 2467                           IF ( MAPSTA(IY,IX) .LE. 0 .OR.XK(IX,IY,IK) .EQ.>
 2468                             MX1(IX,IY) = MFILL
 2469                           ELSE
 2470                             MX1(IX,IY) = NINT(XK(IX,IY,IK)/META(1)%FSC)
 2471                           END IF
 2472                         END DO
 2473                       END DO
 2474                       IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),             >
 2475                           MX1(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUNT(1:4>
 
 
                                                         ww3_ounf.F90  page  46
 
 
 2476                       CALL CHECK_ERR(IRET)
 2477                     ENDIF ! SMCGRD
 2478                   END DO
 2479                 END IF   ! EXTRADIM
 2480               END IF   ! NFIELD
 2481 !
 2482 ! Real output (NCVARTYPE.GE.3)
 2483 !
 2484             ELSE
 2485               IF ( NFIELD.EQ.3 ) THEN
 2486                 IF (SMCGRD) THEN
 2487                 ELSE ! IF(SMCGRD)
 2488                   DO IX=IX1, IXN
 2489                     DO IY=IY1, IYN
 2490                       IF ( MAPSTA(IY,IX) .LE. 0 .OR. X1(IX,IY) .EQ. UNDEF>
 2491                         MXXR(IX,IY) = MFILLR
 2492                         MYYR(IX,IY) = MFILLR
 2493                         MXYR(IX,IY) = MFILLR
 2494                       ELSE
 2495                         MXXR(IX,IY) = X1(IX,IY)
 2496                         MYYR(IX,IY) = X2(IX,IY)
 2497                         MXYR(IX,IY) = XY(IX,IY)
 2498                       END IF
 2499                     END DO
 2500                   END DO
 2501                         
 2502                   IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),              &
 2503                           MXXR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:>
 2504                   CALL CHECK_ERR(IRET)
 2505                   IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),            &
 2506                           MYYR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:>
 2507                   CALL CHECK_ERR(IRET)
 2508                   IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+3),            &
 2509                           MXYR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:>
 2510                   CALL CHECK_ERR(IRET)
 2511                 ENDIF ! SMCGRD
 2512 ! NFIELD=2
 2513               ELSE IF (NFIELD.EQ.2 ) THEN
 2514 ! EXTRADIM=0
 2515                 IF (EXTRADIM.EQ.0) THEN
 2516                   IF (SMCGRD) THEN
 2517                   ELSE ! IF SMCGRD
 2518                     DO IX=IX1, IXN
 2519                       DO IY=IY1, IYN
 2520                         IF ( MAPSTA(IY,IX) .LE. 0 .OR. XX(IX,IY) .EQ. UND>
 2521                           MXXR(IX,IY) = MFILLR
 2522                           MYYR(IX,IY) = MFILLR
 2523                         ELSE
 2524                           MXXR(IX,IY) = XX(IX,IY)
 2525                           MYYR(IX,IY) = XY(IX,IY)
 2526                         END IF
 2527                       END DO
 2528                     END DO
 2529                     IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),             &
 2530                               MXXR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUN>
 
 
                                                         ww3_ounf.F90  page  47
 
 
 2531                     CALL CHECK_ERR(IRET)
 2532                     IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),           &
 2533                             MYYR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(>
 2534                     CALL CHECK_ERR(IRET)
 2535                   ENDIF ! SMCGRD
 2536   ! EXTRADIM=1
 2537                 ELSE
 2538                   START(4-COORDTYPE)=0
 2539                   DO IK=I1F,I2F
 2540                     START(4-COORDTYPE)=START(4-COORDTYPE)+1
 2541                                                            
 2542                     IF (SMCGRD) THEN
 2543                     ELSE ! IF SMCGRD
 2544                       DO IX=IX1, IXN
 2545                         DO IY=IY1, IYN
 2546                           IF ( MAPSTA(IY,IX) .LE. 0 .OR.XXK(IX,IY,IK) .EQ>
 2547                             MXXR(IX,IY) = MFILLR
 2548                             MYYR(IX,IY) = MFILLR
 2549                           ELSE
 2550                             MXXR(IX,IY) = XXK(IX,IY,IK)
 2551                             MYYR(IX,IY) = XYK(IX,IY,IK)
 2552                           END IF
 2553                         END DO
 2554                       END DO
 2555                       IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),             >
 2556                               MXXR(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUN>
 2557                       IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),             &
 2558                               MYYR(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUN>
 2559                     ENDIF ! SMCGRD
 2560                   END DO
 2561                 END IF  ! EXTRADIM
 2562 ! NFIELD=1
 2563               ELSE
 2564 ! EXTRADIM=0
 2565                 IF (EXTRADIM.EQ.0) THEN
 2566                   IF (SMCGRD) THEN
 2567                   ELSE ! IF SMCGRD
 2568                     DO IX=IX1, IXN
 2569                       DO IY=IY1, IYN
 2570                         IF ( MAPSTA(IY,IX) .LE. 0 .OR.X1(IX,IY) .EQ. UNDE>
 2571                           MX1R(IX,IY) = MFILLR
 2572                         ELSE
 2573                           MX1R(IX,IY) = X1(IX,IY)
 2574                         END IF
 2575                       END DO
 2576                     END DO
 2577                     IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
 2578                             MX1R(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(>
 2579                     CALL CHECK_ERR(IRET)
 2580                   ENDIF ! SMCGRD
 2581 ! EXTRADIM=1
 2582                 ELSE
 2583                   START(4-COORDTYPE)=0
 2584                   DO IK=I1F,I2F
 2585                     START(4-COORDTYPE)=START(4-COORDTYPE)+1
 
 
                                                         ww3_ounf.F90  page  48
 
 
 2586                     IF (SMCGRD) THEN
 2587                     ELSE ! IF SMCGRD
 2588                       DO IX=IX1, IXN
 2589                         DO IY=IY1, IYN
 2590                           IF ( MAPSTA(IY,IX) .LE. 0 .OR.XK(IX,IY,IK) .EQ.>
 2591                             MX1R(IX,IY) = MFILLR
 2592                           ELSE
 2593                             MX1R(IX,IY) = XK(IX,IY,IK)
 2594                           END IF
 2595                         END DO
 2596                       END DO
 2597                       IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),             >
 2598                           MX1R(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUNT(1:>
 2599                       CALL CHECK_ERR(IRET)
 2600                     END IF ! SMCGRD
 2601                   END DO
 2602                 END IF   ! EXTRADIM
 2603               END IF   ! NFIELD
 2604             END IF   ! NCVARTYPE
 2605                                 
 2606             ! updates the variable index
 2607             IVAR1=IVAR1+NFIELD
 2608                               
 2609                               
 2610             ! Loops over IPART for partition variables
 2611             ! ChrisBunney: Don't loop IPART for last two entries in secti>
 2612             ! (16: total wind sea fraction, 17: number of parts) as these>
 2613             ! do not have partitions.
 2614             IF (IFI .EQ. 4 .AND. IFJ .LE. NOGE(IFI) - 2) THEN
 2615 560           CONTINUE
 2616               IF (INDEXIPART.LT.NBIPART) THEN
 2617                 INDEXIPART=INDEXIPART+1
 2618                 IF (TABIPART(INDEXIPART).EQ.-1) GOTO 560
 2619                 IPART=TABIPART(INDEXIPART)
 2620                 GOTO 555
 2621               END IF
 2622             ELSE
 2623               INDEXIPART=1
 2624             END IF
 2625 !
 2626           END IF  ! FLG2D(IFI,IFJ)
 2627         END DO  ! IFI=1, NOGRP
 2628       END DO  ! IFJ=1, NGRPP
 2629 !
 2630 ! Clean up
 2631       DEALLOCATE(X1, X2, XX, XY, XK, XXK, XYK)
 2632       DEALLOCATE(MX1, MXX, MYY, MXY, MAPOUT)
 2633       DEALLOCATE(MX1R, MXXR, MYYR, MXYR)
 2634       DEALLOCATE(AUX1)
 2635       IF (ALLOCATED(LON)) DEALLOCATE(LON, LAT)
 2636       IF (ALLOCATED(LON2D)) DEALLOCATE(LON2D, LAT2D)
 2637 !
 2638       RETURN
 2639 !
 2640 ! Error escape locations
 
 
                                                         ww3_ounf.F90  page  49
 
 
 2641 !
 2642  
 2643 !
 2644 ! Formats
 2645 !
 2646   973 FORMAT ( 'NEW NETCDF FILE WAS CREATED ',A)
 2647   999 FORMAT (/' *** WAVEWATCH III ERROR IN W3EXNC :'/                &
 2648                '     PLEASE UPDATE FIELDS !!! '/                      &
 2649                '     IFI = ',I2, '- IFJ = ',I2/)
 2650 !
 2651 !/
 2652   
 2653   
 2654   
 2655 !/ End of W3EXNC ----------------------------------------------------- /
 2656 !/
 2657       END SUBROUTINE W3EXNC
 2658                            
 2659                            
 2660                            
 2661                            
 2662 !------------------------------------------------------------------------>
 2663       SUBROUTINE W3CRNC (NCFILE, NCID, DIMID, DIMLN, VARID,  &
 2664                          EXTRADIM, NCTYPE, MAPSTAOUT )
 2665 !
 2666       USE W3GDATMD, ONLY : GTYPE, FLAGLL, UNGTYPE, CLGTYPE, RLGTYPE
 2667       USE NETCDF
 2668       USE W3TIMEMD
 2669                   
 2670       IMPLICIT NONE
 2671                    
 2672                    
 2673                    
 2674       INTEGER, INTENT(IN)               :: EXTRADIM
 2675       INTEGER, INTENT(IN)               :: NCTYPE
 2676       CHARACTER*(*), INTENT(IN)         :: NCFILE
 2677       INTEGER, INTENT(OUT)              :: NCID
 2678       INTEGER, INTENT(OUT)              :: DIMID(6)
 2679       INTEGER, INTENT(IN)               :: DIMLN(6)
 2680       INTEGER, INTENT(OUT)              :: VARID(300)
 2681       INTEGER, INTENT(IN)               :: MAPSTAOUT
 2682 !
 2683 !/ ------------------------------------------------------------------- /
 2684 !   Local parameters
 2685 !
 2686       INTEGER                           :: IVAR,IRET,ICODE,STRL,STRL2
 2687       INTEGER                           :: DIMTRI(2)
 2688     INTEGER                           :: DEFLATE=1
 2689 !
 2690       CHARACTER                         :: ATTNAME*120,ATTVAL*120
 2691                                                                  
 2692                                                                  
 2693 !
 2694 ! Creation in netCDF3 or netCDF4
 2695 !
 
 
                                                         ww3_ounf.F90  page  50
 
 
 2696       IF(NCTYPE.EQ.3)  IRET = NF90_CREATE(TRIM(NCFILE), NF90_CLOBBER, NCI>
 2697       IF(NCTYPE.EQ.4) IRET = NF90_CREATE(TRIM(NCFILE), NF90_NETCDF4, NCID)
 2698       CALL CHECK_ERR(IRET)
 2699 !
 2700 ! Define dimensions
 2701 !
 2702       IRET = NF90_DEF_DIM(NCID, 'level', DIMLN(1), DIMID(1))
 2703                                                             
 2704 !
 2705 ! Regular structured case
 2706 !
 2707       IF (GTYPE.NE.UNGTYPE) THEN
 2708         IF (FLAGLL) THEN
 2709           IF (SMCGRD) THEN
 2710           ELSE
 2711             IRET = NF90_DEF_DIM(NCID, 'longitude', DIMLN(2), DIMID(2))
 2712             IRET = NF90_DEF_DIM(NCID, 'latitude', DIMLN(3), DIMID(3))
 2713           ENDIF ! SMCGRD
 2714         ELSE
 2715           IRET = NF90_DEF_DIM(NCID, 'x', DIMLN(2), DIMID(2))
 2716           IRET = NF90_DEF_DIM(NCID, 'y', DIMLN(3), DIMID(3))
 2717           END IF
 2718         CALL CHECK_ERR(IRET)
 2719 !
 2720 ! Unstructured case
 2721 !
 2722       ELSE
 2723         IRET = NF90_DEF_DIM(NCID, 'node', DIMLN(2), DIMID(2))
 2724         IRET = NF90_DEF_DIM(NCID, 'element', DIMLN(3), DIMID(3))
 2725         CALL CHECK_ERR(IRET)
 2726       ENDIF
 2727 !
 2728  
 2729  
 2730       IF (EXTRADIM.EQ.1) THEN
 2731         IRET = NF90_DEF_DIM(NCID, 'f', DIMLN(4), DIMID(4))
 2732         CALL CHECK_ERR(IRET)
 2733       ENDIF
 2734            
 2735       IRET = NF90_DEF_DIM(NCID, 'time',NF90_UNLIMITED, DIMID(4+EXTRADIM))
 2736       CALL CHECK_ERR(IRET)
 2737                           
 2738       IF (GTYPE.EQ.UNGTYPE) THEN
 2739         IRET = NF90_DEF_DIM(NCID, 'noel',3, DIMID(5+EXTRADIM))
 2740         CALL CHECK_ERR(IRET)
 2741       ENDIF
 2742            
 2743            
 2744 !
 2745 !     define variables
 2746 !
 2747       IF (FLAGLL) THEN
 2748 !longitude
 2749         IF (GTYPE.EQ.RLGTYPE) THEN
 2750           IF (SMCGRD) THEN
 
 
                                                         ww3_ounf.F90  page  51
 
 
 2751           ELSE
 2752             IRET = NF90_DEF_VAR(NCID, 'longitude', NF90_FLOAT, DIMID(2), >
 2753             IRET = NF90_DEF_VAR(NCID, 'latitude', NF90_FLOAT, DIMID(3), V>
 2754           ENDIF ! SMCGRD
 2755         ELSE IF (GTYPE.EQ.CLGTYPE) THEN
 2756           IRET = NF90_DEF_VAR(NCID, 'longitude', NF90_FLOAT, (/ DIMID(2),>
 2757                                                                          >
 2758           IRET = NF90_DEF_VAR(NCID, 'latitude', NF90_FLOAT, (/ DIMID(2), >
 2759                                                                          >
 2760         ELSE
 2761           IRET = NF90_DEF_VAR(NCID, 'longitude', NF90_FLOAT, DIMID(2), VA>
 2762           IRET = NF90_DEF_VAR(NCID, 'latitude', NF90_FLOAT, DIMID(2), VAR>
 2763           END IF
 2764         IRET=NF90_PUT_ATT(NCID,VARID(1),'units','degree_east')
 2765         IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','longitude')
 2766         IRET=NF90_PUT_ATT(NCID,VARID(1),'standard_name','longitude')
 2767         IRET=NF90_PUT_ATT(NCID,VARID(1),'valid_min',-180.0)
 2768         IRET=NF90_PUT_ATT(NCID,VARID(1),'valid_max',360.)
 2769 !
 2770         IRET=NF90_PUT_ATT(NCID,VARID(2),'units','degree_north')
 2771         IRET=NF90_PUT_ATT(NCID,VARID(2),'long_name','latitude')
 2772         IRET=NF90_PUT_ATT(NCID,VARID(2),'standard_name','latitude')
 2773         IRET=NF90_PUT_ATT(NCID,VARID(2),'valid_min',-90.0)
 2774         IRET=NF90_PUT_ATT(NCID,VARID(2),'valid_max',180.)
 2775 !
 2776         IF(SMCGRD) THEN
 2777         ELSE
 2778         ENDIF ! SMCGRD
 2779 !
 2780       ELSE
 2781         IF (GTYPE.EQ.RLGTYPE) THEN
 2782           IRET = NF90_DEF_VAR(NCID, 'x', NF90_FLOAT, DIMID(2), VARID(1))
 2783           IRET = NF90_DEF_VAR(NCID, 'y', NF90_FLOAT, DIMID(3), VARID(2))
 2784         ELSE IF (GTYPE.EQ.CLGTYPE) THEN
 2785           IRET = NF90_DEF_VAR(NCID, 'x', NF90_FLOAT, (/ DIMID(2), DIMID(3>
 2786                                                                          >
 2787           IRET = NF90_DEF_VAR(NCID, 'y', NF90_FLOAT, (/ DIMID(2), DIMID(3>
 2788                                                                          >
 2789         ELSE
 2790           IRET = NF90_DEF_VAR(NCID, 'x', NF90_FLOAT, DIMID(2), VARID(1))
 2791           IRET = NF90_DEF_VAR(NCID, 'y', NF90_FLOAT, DIMID(2), VARID(2))
 2792           END IF
 2793 !
 2794         IRET=NF90_PUT_ATT(NCID,VARID(1),'units','m')
 2795         IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','x')
 2796         IRET=NF90_PUT_ATT(NCID,VARID(2),'units','m')
 2797         IRET=NF90_PUT_ATT(NCID,VARID(2),'long_name','y')
 2798 !
 2799       END IF  ! FLAGLL
 2800 !
 2801       IRET=NF90_PUT_ATT(NCID,VARID(1),'axis','X')
 2802       IRET=NF90_PUT_ATT(NCID,VARID(2),'axis','Y')
 2803         IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(1), 1, 1>
 2804         IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(2), 1, 1>
 2805                                                                           
 
 
                                                         ww3_ounf.F90  page  52
 
 
 2806 !
 2807 ! frequency
 2808 !
 2809       if (EXTRADIM.EQ.1) THEN
 2810         IRET = NF90_DEF_VAR(NCID, 'f', NF90_FLOAT, DIMID(4), VARID(10))
 2811         IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, >
 2812         CALL CHECK_ERR(IRET)
 2813         IRET=NF90_PUT_ATT(NCID,VARID(3),'long_name','wave_frequency')
 2814         CALL CHECK_ERR(IRET)
 2815         IRET=NF90_PUT_ATT(NCID,VARID(3),'standard_name','wave_frequency')
 2816         CALL CHECK_ERR(IRET)
 2817         IRET=NF90_PUT_ATT(NCID,VARID(3),'units','s-1')
 2818         CALL CHECK_ERR(IRET)
 2819         IRET=NF90_PUT_ATT(NCID,VARID(3),'axis','Hz')
 2820         CALL CHECK_ERR(IRET)
 2821       END IF
 2822             
 2823             
 2824 !
 2825 !  time
 2826 !
 2827       IRET = NF90_DEF_VAR(NCID, 'time', NF90_DOUBLE, DIMID(4+EXTRADIM), V>
 2828       IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(3), 1, 1, >
 2829       CALL CHECK_ERR(IRET)
 2830       SELECT CASE (TRIM(CALTYPE))
 2831         CASE ('360_day')
 2832           IRET=NF90_PUT_ATT(NCID,VARID(3),'long_name','time in 360 day ca>
 2833         CASE ('365_day')
 2834           IRET=NF90_PUT_ATT(NCID,VARID(3),'long_name','time in 365 day ca>
 2835         CASE ('standard')
 2836           IRET=NF90_PUT_ATT(NCID,VARID(3),'long_name','julian day (UT)')
 2837       END SELECT
 2838       CALL CHECK_ERR(IRET)
 2839       IRET=NF90_PUT_ATT(NCID,VARID(3),'standard_name','time')
 2840       CALL CHECK_ERR(IRET)
 2841       IRET=NF90_PUT_ATT(NCID,VARID(3),'units','days since 1990-01-01 00:0>
 2842       CALL CHECK_ERR(IRET)
 2843       IRET=NF90_PUT_ATT(NCID,VARID(3),'conventions', &
 2844         'relative julian days with decimal part (as parts of the day )')
 2845       IRET=NF90_PUT_ATT(NCID,VARID(3),'axis','T')
 2846       CALL CHECK_ERR(IRET)
 2847       IRET=NF90_PUT_ATT(NCID,VARID(3),'calendar',TRIM(CALTYPE))
 2848       CALL CHECK_ERR(IRET)
 2849 !
 2850 ! triangles for irregular grids
 2851 !
 2852       IF (GTYPE.EQ.UNGTYPE) THEN
 2853         DIMTRI(1)=DIMID(4+EXTRADIM+1)
 2854         DIMTRI(2)=DIMID(3)
 2855         IRET = NF90_DEF_VAR(NCID, 'tri', NF90_INT, DIMTRI, VARID(4))
 2856         IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(4), 1, 1>
 2857       END IF
 2858 !
 2859 !  Status map: useful for grid combination
 2860 !
 
 
                                                         ww3_ounf.F90  page  53
 
 
 2861       IF (MAPSTAOUT.EQ.1) THEN
 2862         IF (GTYPE.EQ.UNGTYPE) THEN
 2863           IRET = NF90_DEF_VAR(NCID,'MAPSTA', NF90_SHORT,(/ DIMID(2) /), V>
 2864         ELSE
 2865           IRET = NF90_DEF_VAR(NCID,'MAPSTA', NF90_SHORT,(/ DIMID(2) , DIM>
 2866                                                                        VA>
 2867           ENDIF
 2868           IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(20), 1>
 2869 !
 2870         IRET=NF90_PUT_ATT(NCID,VARID(20),'long_name','status map')
 2871         IRET=NF90_PUT_ATT(NCID,VARID(20),'standard_name','status map')
 2872         IRET=NF90_PUT_ATT(NCID,VARID(20),'units','1')
 2873         CALL CHECK_ERR(IRET)
 2874         IRET=NF90_PUT_ATT(NCID,VARID(20),'valid_min',-32)
 2875         CALL CHECK_ERR(IRET)
 2876         IRET=NF90_PUT_ATT(NCID,VARID(20),'valid_max',32)
 2877         CALL CHECK_ERR(IRET)
 2878       END IF
 2879             
 2880 !
 2881 ! Global attributes
 2882 !
 2883       IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'WAVEWATCH_III_version_number' ,>
 2884       CALL CHECK_ERR(IRET)
 2885       IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'WAVEWATCH_III_switches',TRIM(SW>
 2886       CALL CHECK_ERR(IRET)
 2887     IF (ZZWND.NE.10)      IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'SIN4 nameli>
 2888     IF (AALPHA.NE.0.0095) IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'SIN4 nameli>
 2889     IF (BBETA.NE.1.43)    IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'SIN4 nameli>
 2890     IF(SSDSC(7).NE.0.3)   IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'SDS4 nameli>
 2891 ! ... TO BE CONTINUED ...
 2892                          
 2893       IF(SMCGRD) THEN
 2894       ENDIF
 2895            
 2896       ! ChrisB: Write user global attributes:
 2897       CALL WRITE_GLOBAL_META(NCID, IRET)
 2898       CALL CHECK_ERR(IRET)
 2899                           
 2900       open(unit=994,file='NC_globatt.inp',status='old',iostat=ICODE)
 2901       IF (ICODE.EQ.0) THEN
 2902         DO WHILE (ICODE.EQ.0)
 2903           read(994,'(a)',iostat=ICODE) ATTNAME
 2904           read(994,'(a)',iostat=ICODE) ATTVAL
 2905           IF (ICODE.EQ.0) THEN
 2906             STRL=LEN_TRIM(ATTNAME)
 2907             STRL2=LEN_TRIM(ATTVAL)
 2908             IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,ATTNAME(1:STRL),ATTVAL(1:S>
 2909             CALL CHECK_ERR(IRET)
 2910           END IF
 2911         END DO
 2912       ENDIF
 2913       CLOSE(994)
 2914       IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'product_name' ,TRIM(NCFILE))
 2915       CALL CHECK_ERR(IRET)
 
 
                                                         ww3_ounf.F90  page  54
 
 
 2916       IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'area',TRIM(GNAME))
 2917       CALL CHECK_ERR(IRET)
 2918                           
 2919       RETURN
 2920             
 2921       END SUBROUTINE W3CRNC
 2922                            
 2923 !========================================================================>
 2924                                                                           
 2925       SUBROUTINE CHECK_ERROR(IRET, ILINE)
 2926                                          
 2927       USE NETCDF
 2928       USE W3ODATMD, ONLY: NDSE
 2929       USE W3SERVMD, ONLY: EXTCDE
 2930                                 
 2931       IMPLICIT NONE
 2932                    
 2933       INTEGER IRET, ILINE
 2934                          
 2935       IF (IRET .NE. NF90_NOERR) THEN
 2936         WRITE(NDSE,*) ' *** WAVEWATCH III ERROR IN OUNF :'
 2937         WRITE(NDSE,*) ' LINE NUMBER ', ILINE
 2938         WRITE(NDSE,*) ' NETCDF ERROR MESSAGE: '
 2939         WRITE(NDSE,*) NF90_STRERROR(IRET)
 2940         CALL EXTCDE ( 59 )
 2941       END IF
 2942       RETURN
 2943             
 2944       END SUBROUTINE CHECK_ERROR
 2945                                 
 2946 !========================================================================>
 2947                                                                           
 2948                                                                           
 2949 !/
 2950 !/ End of W3OUNF ----------------------------------------------------- /
 2951 !/
 2952       END PROGRAM W3OUNF
 2953                         
 2954                         
 2955                         
 2956                         
 2957                         
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/opt/cray/netcdf-hdf5parallel/4.2.0/pgi/119/include -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
