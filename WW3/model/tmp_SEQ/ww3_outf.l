                                                         ww3_outf.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3OUTF
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         26-Jan-2021 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    19-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
   13 !/    19-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
   14 !/    24-Jan-2001 : Flat grid version                   ( version 2.06 )
   15 !/    23-Apr-2002 : Clean-up                            ( version 2.19 )
   16 !/    29-Apr-2002 : Adding output fields 17-18.         ( version 2.20 )
   17 !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
   18 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
   19 !/    21-Jul-2005 : Adding output fields 19-21.         ( version 3.07 )
   20 !/    28-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   21 !/    05-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
   22 !/    28-Mar-2007 : Adding partitioned output.          ( version 3.11 )
   23 !/                  Adding user slots for outputs.
   24 !/    31-Jul-2007 : Fix file extension errors.          ( version 3.12 )
   25 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   26 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   27 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   28 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
   29 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   30 !/    12-Dec-2012 : SMC grid sea-point text output.JG_Li( version 4.08 )
   31 !/    25-Dec-2012 : New structure of output fields.     ( version 4.11 )
   32 !/                  Minor bug fixes and clean up.
   33 !/    11-Nov-2013 : SMC and rotated grid incorporated in the main
   34 !/                  trunk                               ( version 4.13 )
   35 !/    27-Aug-2015 : ICEH and ICEF added as output       ( version 5.10 )
   36 !/    12-Sep-2018 : Added new partitioned output fields ( version 6.06 )
   37 !/    26-Jan-2021 : Added TP field (derived from FP0)   ( version 7.12 )
   38 !/
   39 !/    Copyright 2009-2012 National Weather Service (NWS),
   40 !/       National Oceanic and Atmospheric Administration.  All rights
   41 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   42 !/       No unauthorized use without permission.
   43 !/
   44 !  1. Purpose :
   45 !
   46 !     Post-processing of grid output.
   47 !
   48 !  2. Method :
   49 !
   50 !     Data is read from the grid output file out_grd.ww3 (raw data)
   51 !     and from the file ww3_outf.inp ( NDSI, output requests ).
   52 !     Model definition and raw data files are read using WAVEWATCH III
   53 !     subroutines.
   54 !
   55 !     Output types :
 
 
                                                         ww3_outf.F90  page   2
 
 
   56 !      1 : print plots
   57 !      2 : field statistics
   58 !      3 : transfer file
   59 !      4 : text output at sea points (1:NSEA).
   60 !
   61 !  3. Parameters :
   62 !
   63 !  4. Subroutines used :
   64 !
   65 !      Name      Type  Module   Description
   66 !     ----------------------------------------------------------------
   67 !      W3NMOD    Subr. W3GDATMD Set number of model.
   68 !      W3SETG    Subr.   Id.    Point to selected model.
   69 !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
   70 !      W3SETW    Subr.   Id.    Point to selected model for wave data.
   71 !      W2NAUX    Subr. W3ADATMD Set number of model for aux data.
   72 !      W3SETA    Subr.   Id.    Point to selected model for aux data.
   73 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
   74 !      STRACE    Subr.   Id.    Subroutine tracing.
   75 !      NEXTLN    Subr.   Id.    Get next line from input file.
   76 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
   77 !      STME21    Subr. W3TIMEMD Convert time to string.
   78 !      TICK21    Subr.   Id.    Advance time.
   79 !      DSEC21    Func.   Id.    Difference between times.
   80 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
   81 !      W3IOGO    Subr. W3IOGOMD Reading/writing raw gridded data file.
   82 !      W3EXGO    Subr. Internal Execute grid output.
   83 !      W3TXTS    Subr. Internal Text output at sea points only.
   84 !     ----------------------------------------------------------------
   85 !
   86 !  5. Called by :
   87 !
   88 !     None, stand-alone program.
   89 !
   90 !  6. Error messages :
   91 !
   92 !     Checks on input, checks in W3IOxx.
   93 !
   94 !  7. Remarks :
   95 !
   96 !  8. Structure :
   97 !
   98 !     See source code.
   99 !
  100 !  9. Switches :
  101 !
  102 !     !/S     Enable subroutine tracing.
  103 !
  104 ! 10. Source code :
  105 !
  106 !/ ------------------------------------------------------------------- /
  107       USE CONSTANTS
  108 !/
  109 !     USE W3GDATMD, ONLY: W3NMOD, W3SETG
  110       USE W3WDATMD, ONLY: W3NDAT, W3SETW
 
 
                                                         ww3_outf.F90  page   3
 
 
  111       USE W3ADATMD, ONLY: W3NAUX, W3SETA
  112       USE W3ODATMD, ONLY: W3NOUT, W3SETO
  113       USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
  114       USE W3TIMEMD
  115       USE W3IOGRMD, ONLY: W3IOGR
  116       USE W3IOGOMD, ONLY: W3IOGO, W3READFLGRD
  117 !/
  118       USE W3GDATMD
  119       USE W3WDATMD, ONLY: TIME, WLV, ICE, ICEH, ICEF, BERG, UST, USTDIR
  120       USE W3ADATMD, ONLY: DW, UA, UD, AS, CX, CY, HS, WLM, T0M1, THM, &
  121                           THS, FP0, THP0, FP1, THP1, DTDYN, FCUT,     &
  122                           ABA, ABD, UBA, UBD, SXX, SYY, SXY, USERO,   &
  123                           PHS, PTP, PLP, PDIR, PSI, PWS, PWST, PNR,   &
  124                           PTM1, PT1, PT2, PEP,                        &
  125                           PTHP0, PQP, PSW, PPE, PGW, QP,              &
  126                           TAUOX, TAUOY, TAUWIX,BHD,                   &
  127                           TAUWIY, PHIAW, PHIOC, TUSX, TUSY, PRMS, TPMS,&
  128                           USSX, USSY, MSSX, MSSY, MSCX, MSCY, CHARN,  &
  129                           TAUWNX, TAUWNY, TAUBBL, PHIBBL, CFLXYMAX,   &
  130                           CFLTHMAX, CFLKMAX, BEDFORMS, WHITECAP, T02, &
  131                           CGE, T01, HSIG, STMAXE, STMAXD, HMAXE,      &
  132                           HCMAXE, HMAXD, HCMAXD, MSSD, MSCD, WBT
  133       USE W3ODATMD, ONLY: NDSO, NDSE, NDST, NOGRP, NGRPP, IDOUT,      &
  134                           UNDEF, FLOGRD, FNMPRE, NOSWLL, NOGE
  135 !
  136       IMPLICIT NONE
  137 !/
  138 !/ ------------------------------------------------------------------- /
  139 !/ Local parameters
  140 !/
  141       INTEGER                 :: NDSI, NDSM, NDSOG, NDSDAT, NDSDT,    &
  142                                  NDSTRC, NTRACE, IERR, I, J, IFI, IFJ,&
  143                                  TOUT(2), TDUM(2), IOTEST, NOUT,      &
  144                                  ITYPE, IX1, IXN, IXS, IY1, IYN, IYS, &
  145                                  IDLA, IDFM, IOUT, IPART
  146       REAL                    :: DTREQ, DTEST
  147       CHARACTER               :: COMSTR*1, IDTIME*23, IDDDAY*11,      &
  148                                  TABNME*9
  149       LOGICAL                 :: FLREQ(NOGRP,NGRPP), FLOG(NOGRP),     &
  150                                  SCALE, VECTOR, LTEMP(NGRPP)
  151 !/
  152 !/ ------------------------------------------------------------------- /
  153 !/
  154 ! 1.  IO set-up.
  155 !
  156       CALL W3NMOD ( 1, 6, 6 )
  157       CALL W3SETG ( 1, 6, 6 )
  158       CALL W3NDAT (    6, 6 )
  159       CALL W3SETW ( 1, 6, 6 )
  160       CALL W3NAUX (    6, 6 )
  161       CALL W3SETA ( 1, 6, 6 )
  162       CALL W3NOUT (    6, 6 )
  163       CALL W3SETO ( 1, 6, 6 )
  164 !
  165       NDSI   = 10
 
 
                                                         ww3_outf.F90  page   4
 
 
  166       NDSM   = 20
  167       NDSOG  = 20
  168       NDSDAT = 50
  169 !
  170       NDSTRC =  6
  171       NTRACE = 10
  172       CALL ITRACE ( NDSTRC, NTRACE )
  173 !
  174       WRITE (NDSO,900)
  175 !
  176       J      = LEN_TRIM(FNMPRE)
  177       OPEN (NDSI,FILE=FNMPRE(:J)//'ww3_outf.inp',STATUS='OLD',       &
  178             ERR=800,IOSTAT=IERR)
  179       READ (NDSI,'(A)',END=801,ERR=802) COMSTR
  180       IF (COMSTR.EQ.' ') COMSTR = '$'
  181       WRITE (NDSO,901) COMSTR
  182 !
  183 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  184 ! 2.  Read model definition file.
  185 !
  186       CALL W3IOGR ( 'READ', NDSM )
  187       WRITE (NDSO,920) GNAME
  188 !
  189 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  190 ! 3.  Read general data and first fields from file
  191 !
  192       CALL W3IOGO ( 'READ', NDSOG, IOTEST )
  193 !
  194       WRITE (NDSO,930)
  195       DO IFI=1, NOGRP
  196         DO IFJ=1, NGRPP
  197           IF ( FLOGRD(IFI,IFJ) ) WRITE (NDSO,931) IDOUT(IFI,IFJ)
  198           END DO
  199         END DO
  200 !
  201 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  202 ! 4.  Read requests from input file.
  203 !     Output times
  204 !
  205       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  206       READ (NDSI,*,END=801,ERR=802) TOUT, DTREQ, NOUT
  207       DTREQ  = MAX ( 0. , DTREQ )
  208       IF ( DTREQ.EQ.0. ) NOUT = 1
  209       NOUT   = MAX ( 1 , NOUT )
  210 !
  211       CALL STME21 ( TOUT , IDTIME )
  212       WRITE (NDSO,940) IDTIME
  213 !
  214       TDUM = 0
  215       CALL TICK21 ( TDUM , DTREQ )
  216       CALL STME21 ( TDUM , IDTIME )
  217       IF ( DTREQ .GE. 86400. ) THEN
  218           WRITE (IDDDAY,'(I10,1X)') INT(DTREQ/86400.)
  219         ELSE
  220           IDDDAY = '           '
 
 
                                                         ww3_outf.F90  page   5
 
 
  221         END IF
  222       IDTIME(1:11) = IDDDAY
  223       IDTIME(21:23) = '   '
  224       WRITE (NDSO,941) IDTIME, NOUT
  225 !
  226 ! ... Output fields
  227 !
  228       CALL W3READFLGRD ( NDSI, NDSO, 9, NDSE, COMSTR, FLOG,      &
  229                          FLREQ, 1, 1, IERR )
  230       IF (IERR.NE.0) GOTO 800
  231                              
  232 !
  233 ! ... Output type
  234 !
  235       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  236       READ (NDSI,*,END=801,ERR=802) ITYPE, IPART
  237 !Li   IF ( ITYPE.LT.0 .OR. ITYPE.GT.3 ) THEN
  238       IF ( ITYPE.LT.0 .OR. ITYPE.GT.4 ) THEN
  239 !Li   Type 4 for text output at sea points.  JGLi12Dec2012
  240           WRITE (NDSE,1010) ITYPE
  241           CALL EXTCDE ( 1 )
  242         END IF
  243       IPART  = MAX ( 0 , MIN ( NOSWLL , IPART ) )
  244 !
  245 ! ... ITYPE = 0
  246 !
  247       IF ( ITYPE .EQ. 0 ) THEN
  248           WRITE (NDSO,942) ITYPE, 'Checking contents of file'
  249           DO
  250             CALL STME21 ( TIME , IDTIME )
  251             WRITE (NDSO,943) IDTIME
  252             CALL W3IOGO ( 'READ', NDSOG, IOTEST )
  253             IF ( IOTEST .EQ. -1 ) THEN
  254                 WRITE (NDSO,944)
  255                 GOTO 888
  256               END IF
  257             END DO
  258 !
  259 ! ... ITYPE = 1
  260 !
  261         ELSE IF (ITYPE .EQ. 1) THEN
  262           WRITE (NDSO,942) ITYPE, 'Print plots'
  263           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  264           READ (NDSI,*,END=801,ERR=802)                               &
  265                 IX1, IXN, IXS, IY1, IYN, IYS, SCALE, VECTOR
  266           IX1    = MAX ( IX1 , 1 )
  267           IXN    = MIN ( IXN , NX )
  268           IXS    = MAX ( IXS , 1 )
  269           IY1    = MAX ( IY1 , 1 )
  270           IYN    = MIN ( IYN , NY )
  271           IYS    = MAX ( IYS , 1 )
  272           WRITE (NDSO,1940) IX1, IXN, IXS, IY1, IYN, IYS
  273           IF ( SCALE ) WRITE (NDSO,1941)
  274 !
  275 ! ... ITYPE = 2
 
 
                                                         ww3_outf.F90  page   6
 
 
  276 !
  277         ELSE IF (ITYPE .EQ. 2) THEN
  278           WRITE (NDSO,942) ITYPE, 'Field statistics'
  279           NDSDT  = NDSDAT - 1
  280           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  281           READ (NDSI,*,END=801,ERR=802) IX1, IXN, IY1, IYN
  282           IX1    = MAX ( IX1 , 1 )
  283           IXN    = MIN ( IXN , NX )
  284           IY1    = MAX ( IY1 , 1 )
  285           IYN    = MIN ( IYN , NY )
  286           WRITE (NDSO,2940) IX1, IXN, IY1, IYN
  287 !
  288 ! ... ITYPE = 3
  289 !
  290         ELSE IF (ITYPE .EQ. 3) THEN
  291           WRITE (NDSO,942) ITYPE, 'Transfer files'
  292           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  293           READ (NDSI,*,END=801,ERR=802)                               &
  294                 IX1, IXN, IY1, IYN, IDLA, IDFM
  295           IX1    = MAX ( IX1 , 1 )
  296           IXN    = MIN ( IXN , NX )
  297           IY1    = MAX ( IY1 , 1 )
  298           IYN    = MIN ( IYN , NY )
  299           IF (IDLA.LT.1 .OR. IDLA.GT.5) IDLA   = 1
  300           IF (IDFM.LT.1 .OR. IDFM.GT.3) IDFM   = 1
  301           VECTOR = .TRUE.
  302           WRITE (NDSO,3940) IX1, IXN, IY1, IYN, IDLA, IDFM
  303 !
  304 !Li   Added sea-point output type 4.  JGLi12Dec2012
  305 !
  306 ! ... ITYPE = 4
  307 !
  308         ELSE IF (ITYPE .EQ. 4) THEN
  309           WRITE (NDSO,942) ITYPE, 'Full sea-point output.'
  310           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  311           READ (NDSI,*,END=801,ERR=802)                               &
  312                 IX1, IXN, IY1, IYN, IDLA, IDFM
  313 !Li
  314 !
  315         END IF
  316 !
  317 ! ... Output of output fields
  318 !
  319       IF ( ITYPE.NE.2 ) THEN
  320           WRITE (NDSO,945)
  321         ELSE
  322           WRITE (NDSO,2945)
  323         END IF
  324 !
  325       DO IFI=1, NOGRP
  326       DO IFJ=1, NGRPP
  327         IF ( FLREQ(IFI,IFJ) ) THEN
  328             IF ( FLOGRD(IFI,IFJ) ) THEN
  329                 IF ( ITYPE.NE.2 ) THEN
  330                     WRITE (NDSO,946) IDOUT(IFI,IFJ), ' '
 
 
                                                         ww3_outf.F90  page   7
 
 
  331                   ELSE
  332                     J      = LEN_TRIM(FNMPRE)
  333                     NDSDT  = NDSDT + 1
  334                     WRITE (TABNME,'(A3,I2.2,A4)') 'tab', NDSDT, '.ww3'
  335                     WRITE (NDSO,2946) TABNME, IDOUT(IFI,IFJ)
  336                     OPEN (NDSDT,FILE=FNMPRE(:J)//TABNME)
  337                     WRITE (NDSDT,2947) IDOUT(IFI,IFJ)
  338                   END IF
  339               ELSE
  340                 WRITE (NDSO,946) IDOUT(IFI,IFJ), '*** NOT AVAILABLE ***'
  341                 FLREQ(IFI,IFJ) = .FALSE.
  342               END IF
  343           END IF
  344         END DO
  345         END DO
  346 !
  347       IF ( FLOG(4) ) THEN
  348           IF ( IPART .EQ. 0 ) THEN
  349               WRITE (NDSO,948)
  350             ELSE
  351               WRITE (NDSO,949) IPART
  352             END IF
  353         END IF
  354 !
  355 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  356 ! 5.  Time management.
  357 !
  358       IOUT   = 0
  359       IF (ITYPE.EQ.3) WRITE (NDSO,970)
  360 !
  361       DO
  362         DTEST  = DSEC21 ( TIME , TOUT )
  363         IF ( DTEST .GT. 0. ) THEN
  364             CALL W3IOGO ( 'READ', NDSOG, IOTEST )
  365               IF ( IOTEST .EQ. -1 ) THEN
  366                 WRITE (NDSO,944)
  367                 GOTO 888
  368               END IF
  369             CYCLE
  370           END IF
  371         IF ( DTEST .LT. 0. ) THEN
  372             CALL TICK21 ( TOUT , DTREQ )
  373             CYCLE
  374           END IF
  375 !
  376         IOUT   = IOUT + 1
  377         CALL STME21 ( TOUT , IDTIME )
  378         IF (ITYPE.EQ.1) THEN
  379             WRITE (NDSO,950) IDTIME
  380           ELSE IF (ITYPE.EQ.3) THEN
  381             WRITE (NDSO,971) IDTIME
  382           END IF
  383 !
  384         CALL W3EXGO ( NX, NY, NSEA )
  385 !
 
 
                                                         ww3_outf.F90  page   8
 
 
  386         CALL TICK21 ( TOUT , DTREQ )
  387         IF ( IOUT .GE. NOUT ) EXIT
  388         END DO
  389 !
  390       IF (ITYPE.EQ.3) WRITE (NDSO,972)
  391 !
  392       GOTO 888
  393 !
  394 ! Escape locations read errors :
  395 !
  396   800 CONTINUE
  397       WRITE (NDSE,1000) IERR
  398       CALL EXTCDE ( 10 )
  399 !
  400   801 CONTINUE
  401       WRITE (NDSE,1001)
  402       CALL EXTCDE ( 11 )
  403 !
  404   802 CONTINUE
  405       WRITE (NDSE,1002) IERR
  406       CALL EXTCDE ( 12 )
  407 !
  408   888 CONTINUE
  409       WRITE (NDSO,999)
  410 !
  411 ! Formats
  412 !
  413   900 FORMAT (/15X,'   *** WAVEWATCH III Field output postp. ***   '/ &
  414                15X,'==============================================='/)
  415   901 FORMAT ( '  Comment character is ''',A,''''/)
  416 !
  417   920 FORMAT ( '  Grid name : ',A/)
  418 !
  419   930 FORMAT ( '  Fields in file : '/                                 &
  420                ' --------------------------')
  421   931 FORMAT ( '      ',A)
  422 !
  423   940 FORMAT (/'  Output time data : '/                               &
  424                ' --------------------------------------------------'/ &
  425                '      First time         : ',A)
  426   941 FORMAT ( '      Interval           : ',A/                       &
  427                '      Number of requests : ',I6)
  428   942 FORMAT (/'  Output type ',I2,' :'/                              &
  429                ' --------------------------------------------------'/ &
  430                '      ',A/)
  431   943 FORMAT ( '      Data for ',A)
  432   944 FORMAT (/'      End of file reached '/)
  433 !
  434   945 FORMAT (/'  Requested output fields : '/                        &
  435                ' --------------------------------------------------')
  436  2945 FORMAT (/'  Output files and fields : '/                        &
  437                ' --------------------------------------------------')
  438   946 FORMAT ( '      ',A,2X,A)
  439  2946 FORMAT ( '      ',A,' : ',A)
  440  2947 FORMAT ( ' Statitics of ',A/                                    &
 
 
                                                         ww3_outf.F90  page   9
 
 
  441                '   (time, min, max, avg, std)'/)
  442   948 FORMAT (/'         Partitioned field data for wind seas')
  443   949 FORMAT (/'         Partitioned field data for swell field',I2)
  444 !
  445  1940 FORMAT ( '      X range and interval : ',3I5/                   &
  446                '      Y range and interval : ',3I5)
  447  1941 FORMAT ( '      Data is normalized ')
  448 !
  449  2940 FORMAT ( '      X range : ',2I5/                                &
  450                '      Y range : ',2I5)
  451 !
  452  3940 FORMAT ( '      X range          : ',2I5/                       &
  453                '      Y range          : ',2I5/                       &
  454                '      Layout indicator : ',I5/                        &
  455                '      Format indicator : ',I5)
  456 !
  457   950 FORMAT (//'  Output for ',A/                                    &
  458                ' --------------------------------------------------')
  459 !
  460   970 FORMAT (//'  Generating files '/                                &
  461                ' --------------------------------------------------')
  462   971 FORMAT ( '      Files for ',A)
  463   972 FORMAT ( ' ')
  464 !
  465   999 FORMAT (/'  End of program '/                                   &
  466                ' ========================================='/          &
  467                '         WAVEWATCH III Field output '/)
  468 !
  469  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUTF : '/               &
  470                '     ERROR IN OPENING INPUT FILE'/                    &
  471                '     IOSTAT =',I5/)
  472 !
  473  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUTF : '/               &
  474                '     PREMATURE END OF INPUT FILE'/)
  475 !
  476  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUTF : '/               &
  477                '     ERROR IN READING FROM INPUT FILE'/               &
  478                '     IOSTAT =',I5/)
  479 !
  480  1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUTF : '/               &
  481                '     ILLEGAL TYPE, ITYPE =',I4/)
  482 !/
  483 !/ Internal subroutine W3EXGO ---------------------------------------- /
  484 !/
  485       CONTAINS
  486 !/ ------------------------------------------------------------------- /
  487       SUBROUTINE W3EXGO ( NX, NY, NSEA )
  488 !/
  489 !/                  +-----------------------------------+
  490 !/                  | WAVEWATCH III           NOAA/NCEP |
  491 !/                  |           H. L. Tolman            |
  492 !/                  |                        FORTRAN 90 |
  493 !/                  | Last update :         26-Jan-2021 |
  494 !/                  +-----------------------------------+
  495 !/
 
 
                                                         ww3_outf.F90  page  10
 
 
  496 !/    26-Sep-1997 : Final FORTRAN 77                    ( version 1.18 )
  497 !/    19-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
  498 !/                  Massive changes to logistics
  499 !/    24-Jan-2001 : Flat grid version                   ( version 2.06 )
  500 !/    23-Apr-2002 : Clean-up                            ( version 2.19 )
  501 !/    29-Apr-2002 : Adding output fields 17-18.         ( version 2.20 )
  502 !/    16-Oct-2002 : Fix bound. error for stress output. ( version 3.00 )
  503 !/    16-Oct-2002 : Fix statistical output for UNDEF.   ( version 3.00 )
  504 !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
  505 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
  506 !/    21-Jul-2005 : Adding output fields 19-21.         ( version 3.07 )
  507 !/    28-Jun-2006 : Adding file name preamble.          ( version 3.09 )
  508 !/    05-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
  509 !/    28-Mar-2007 : Adding partitioned output.          ( version 3.11 )
  510 !/                  Adding user slots for outputs.
  511 !/    31-Jul-2007 : Fix file extension errors.          ( version 3.12 )
  512 !/    25-Dec-2012 : New structure of output fields.     ( version 4.11 )
  513 !/    25-Jun-2013 : Add type 4 sea point text output.   ( version 4.11 )
  514 !/    26-Jan-2021 : Added TP field (derived from FP0)   ( version 7.12 )
  515 !/
  516 !  1. Purpose :
  517 !
  518 !     Perform actual grid output.
  519 !
  520 !  3. Parameters :
  521 !
  522 !     Parameter list
  523 !     ----------------------------------------------------------------
  524 !       NX/Y    Int.  I  Grid dimensions.
  525 !       NSEA    Int.  I  Number of sea points.
  526 !     ----------------------------------------------------------------
  527 !
  528 !     Internal parameters
  529 !     ----------------------------------------------------------------
  530 !       FLONE   Log.  Flags for one-dimensional field.
  531 !       FLTWO   Log.  Flags for two-dimensional field X Y.
  532 !       FLDIR   Log.  Flags for two-dimensional, directional field.
  533 !       FLTRI   Log.  Flags for three dimensional field.
  534 !       X1, X2, XX, XY
  535 !               R.A.  Output fields
  536 !     ----------------------------------------------------------------
  537 !
  538 !  4. Subroutines used :
  539 !
  540 !      Name      Type  Module   Description
  541 !     ----------------------------------------------------------------
  542 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  543 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
  544 !      W3S2XY    Subr.   Id.    Convert from storage to spatial grid.
  545 !      PRTBLK    Subr. W3ARRYMD Print plot of array.
  546 !      OUTA2I    Subr.   Id.    Print array of INTEGERS.
  547 !     ----------------------------------------------------------------
  548 !
  549 !  5. Called by :
  550 !
 
 
                                                         ww3_outf.F90  page  11
 
 
  551 !     Main program in which it is contained.
  552 !
  553 !  6. Error messages :
  554 !
  555 !       None.
  556 !
  557 !  7. Remarks :
  558 !
  559 !     - Note that arrays CX and CY of the main program now contain
  560 !       the absolute current speed and direction respectively.
  561 !
  562 !  8. Structure :
  563 !
  564 !     See source code.
  565 !
  566 !  9. Switches :
  567 !
  568 !       !/S  Enable subroutine tracing.
  569 !       !/T  Enable test output.
  570 !
  571 ! 10. Source code :
  572 !
  573 !/ ------------------------------------------------------------------- /
  574       USE W3SERVMD, ONLY : W3S2XY
  575       USE W3ARRYMD, ONLY : OUTA2I, PRTBLK
  576 !/
  577 !/ ------------------------------------------------------------------- /
  578 !/ Parameter list
  579 !/
  580       INTEGER                 :: NX, NY, NSEA
  581 !/
  582 !/ ------------------------------------------------------------------- /
  583 !/ Local parameters
  584 !/
  585       INTEGER                 :: NXMAX, NXTOT, NBLOK, IH, IM, IS,     &
  586                                  MFILL, J, ISEA, IX, IY, IXB, IB,     &
  587                                  IXA, NINGRD, JJ, IFI, IFJ
  588       INTEGER                 :: MAP(NX+1,NY), MP2(NX+1,NY),          &
  589                                  MX1(NX,NY), MXX(NX,NY), MYY(NX,NY),  &
  590                                  MXY(NX,NY)
  591       INTEGER, SAVE           :: IPASS
  592 !     INTEGER, SAVE           :: NCOL   = 80
  593       INTEGER, SAVE           :: NCOL   = 132
  594       REAL                    :: FSC, CABS, UABS, FSCA, XMIN, XMAX,   &
  595                                  XAVG, XSTD, YGBX, XGBX, AABS
  596       REAL                    :: X1(NX+1,NY), X2(NX+1,NY),            &
  597                                  XX(NX+1,NY), XY(NX+1,NY), DPTMAX(1)
  598 !!Li  Type 4 sea point only text output variables.  JGLi25Jun2013
  599       REAL, Dimension(NSEA)   :: XS1, XS2, XS3, XS4, AUX
  600 !!Li
  601       DOUBLE PRECISION        :: XDS, XDSQ
  602       LOGICAL                 :: FLONE, FLTWO, FLDIR, FLTRI
  603       CHARACTER               :: OLDTID*8, FNAME*32, ENAME*7,         &
  604                                  FORMG*12, FORMF*11, UNITS*10, FSCS*7
  605       CHARACTER, SAVE         :: TIMEID*8 = '00000000'
 
 
                                                         ww3_outf.F90  page  12
 
 
  606       CHARACTER, SAVE         :: FILEID*13 = 'WAVEWATCH III'
  607 !/
  608 !/ ------------------------------------------------------------------- /
  609 !/
  610 !
  611 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  612 ! 1.  Preparations
  613 !
  614       X1     = UNDEF
  615       X2     = UNDEF
  616       XX     = UNDEF
  617       XY     = UNDEF
  618 !!Li  Type 4 sea point only variables
  619       XS1    = UNDEF
  620       XS2    = UNDEF
  621       XS3    = UNDEF
  622       XS4    = UNDEF
  623 !
  624 !     Number of print-plots
  625 !
  626       IF ( ITYPE .EQ. 1 ) THEN
  627           IF ( SCALE ) THEN
  628               NXMAX  = ( NCOL - 10 ) / 2
  629             ELSE
  630               NXMAX  = ( NCOL - 10 ) / 5
  631             END IF
  632           NXTOT  = 1 + (IXN-IX1)/IXS
  633           NBLOK  = 1 + (NXTOT-1)/NXMAX
  634         END IF
  635 !
  636 !     Output file unit number
  637 !
  638       IF ( ITYPE .EQ. 2 ) THEN
  639           NDSDT = NDSDAT - 1
  640           IH    = TIME(2) / 10000
  641           IM    = MOD ( TIME(2) , 10000 ) / 100
  642           IS    = MOD ( TIME(2) , 100 )
  643         END IF
  644 !
  645 !     Set-up transfer files
  646 !
  647 !!Li  Type 4 share filename with type 3   JGLi25Jun2013
  648 !!    IF ( ITYPE .EQ. 3 ) THEN
  649       IF ( ITYPE .EQ. 3 .OR. ITYPE .EQ. 4 ) THEN
  650           MFILL  = -999
  651           OLDTID = TIMEID
  652           WRITE (TIMEID,'(I6.6,I2.2)') MOD( TIME(1) , 1000000 ),      &
  653                                        TIME(2)/10000
  654           FNAME(05:12) = TIMEID
  655           FNAME(13:13) = '.'
  656           IF ( TIMEID .NE. OLDTID ) THEN
  657               FNAME(1:4) = 'ww3.'
  658               IPASS    = 1
  659             ELSE
  660               WRITE (ENAME,'(A1,I2.2,A1)') 'e', IPASS, '.'
 
 
                                                         ww3_outf.F90  page  13
 
 
  661               FNAME(1:4) = ENAME
  662               IPASS   = IPASS + 1
  663             END IF
  664           FORMG  = '((10G12.2))'
  665         END IF
  666 !
  667 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  668 ! 2.  Loop over output fields.
  669 !
  670       DO IFI=1, NOGRP
  671       DO IFJ=1, NGRPP
  672         IF ( FLREQ(IFI,IFJ) ) THEN
  673 !
  674             FORMF  = '(1X,32I4)'
  675 !
  676 ! 2.a Set output arrays and parameters
  677                                       
  678               FLONE = .FALSE.
  679               FLTWO = .FALSE.
  680               FLDIR = .FALSE.
  681               FLTRI = .FALSE.
  682 !
  683               IF ( IFI .EQ. 1 .AND. IFJ .EQ. 1 ) THEN
  684                 FLONE  = .TRUE.
  685                 DPTMAX = MAXVAL ( DW(1:NSEA) )
  686                 FSC    = 1.
  687                 IF ( DPTMAX(1) .GT. 999. ) THEN
  688                     FSC    = 0.1
  689                   ELSE IF ( DPTMAX(1) .GT. 99.9 ) THEN
  690                     FSC    = 0.1
  691                   ELSE IF ( DPTMAX(1) .GT. 9.99 ) THEN
  692                     FSC    = 0.01
  693                   END IF
  694                 IF ( ITYPE .EQ. 3 ) FSC = 0.01
  695                 UNITS  = 'm'
  696                 ENAME  = '.dpt'
  697                 FORMF  = '(1X,17I7)'
  698                 IF ( ITYPE .EQ. 4 ) THEN
  699                    XS1    = DW(1:NSEA)
  700                 ELSE
  701                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, DW(1:NSEA)        &
  702                                                           , MAPSF, X1 )
  703                 ENDIF
  704 !
  705               ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 2 ) THEN
  706                 IF ( VECTOR ) THEN
  707                     FLTWO  = .TRUE.
  708                   ELSE
  709                     FLDIR  = .TRUE.
  710                   END IF
  711                 FSC    = 0.01
  712                 ENAME  = '.cur'
  713                 UNITS  = 'm s-1'
  714                 FORMF  = '(1X,17I7)'
  715                 IF ( ITYPE .EQ. 4 ) THEN
 
 
                                                         ww3_outf.F90  page  14
 
 
  716                    XS1    = CX(1:NSEA)
  717                    XS2    = CY(1:NSEA)
  718                 ELSE
  719                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CX(1:NSEA)        &
  720                                                           , MAPSF, XX )
  721                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CY(1:NSEA)        &
  722                                                           , MAPSF, XY )
  723                 ENDIF
  724                 DO ISEA=1, NSEA
  725                   CABS   = SQRT(CX(ISEA)**2+CY(ISEA)**2)
  726                   IF ( CABS .GT. 0.05 ) THEN
  727                       CY(ISEA) = MOD ( 630. -                         &
  728                             RADE*ATAN2(CY(ISEA),CX(ISEA)) , 360. )
  729                     ELSE
  730                       CY(ISEA) = UNDEF
  731                     END IF
  732                   CX(ISEA) = CABS
  733                   END DO
  734                 IF ( ITYPE .EQ. 4 ) THEN
  735                    XS3    = CX(1:NSEA)
  736                    XS4    = CY(1:NSEA)
  737                 ELSE
  738                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CX(1:NSEA)        &
  739                                                           , MAPSF, X1 )
  740                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CY(1:NSEA)        &
  741                                                           , MAPSF, X2 )
  742                 ENDIF
  743 !
  744               ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 3 ) THEN
  745                 IF ( VECTOR ) THEN
  746                     FLTWO  = .TRUE.
  747                   ELSE
  748                     FLDIR  = .TRUE.
  749                   END IF
  750                 FSC    = 0.1
  751                 ENAME  = '.wnd'
  752                 UNITS  = 'm s-1'
  753                 IF ( ITYPE .EQ. 4 ) THEN
  754                    XS1    = UA(1:NSEA)
  755                    XS2    = UD(1:NSEA)
  756                 ELSE
  757                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, UA(1:NSEA)        &
  758                                                           , MAPSF, XX )
  759                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, UD(1:NSEA)        &
  760                                                           , MAPSF, XY )
  761                 ENDIF
  762                 DO ISEA=1, NSEA
  763                   UABS   = SQRT(UA(ISEA)**2+UD(ISEA)**2)
  764                   IF ( UABS .GT. 1.0 ) THEN
  765                       UD(ISEA) = MOD ( 630. -                         &
  766                             RADE*ATAN2(UD(ISEA),UA(ISEA)) , 360. )
  767                     ELSE
  768                       UD(ISEA) = UNDEF
  769                     END IF
  770                   UA(ISEA) = UABS
 
 
                                                         ww3_outf.F90  page  15
 
 
  771                   END DO
  772                 IF ( ITYPE .EQ. 4 ) THEN
  773                    XS3    = UA(1:NSEA)
  774                    XS4    = UD(1:NSEA)
  775                 ELSE
  776                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, UA(1:NSEA)        &
  777                                                           , MAPSF, X1 )
  778                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, UD(1:NSEA)        &
  779                                                           , MAPSF, X2 )
  780                 ENDIF
  781 !
  782               ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 4 ) THEN
  783                 FLONE  = .TRUE.
  784                 FSC    = 0.1
  785                 ENAME  = '.ast'
  786                 UNITS  = 'K'
  787                 IF ( ITYPE .EQ. 4 ) THEN
  788                    XS1    = AS(1:NSEA)
  789                 ELSE
  790                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, AS(1:NSEA)        &
  791                                                           , MAPSF, X1 )
  792                 ENDIF
  793 !
  794               ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 5 ) THEN
  795                 FLONE  = .TRUE.
  796                 FSC    = 0.01
  797                 UNITS  = 'm'
  798                 ENAME  = '.wlv'
  799                 IF ( ITYPE .EQ. 4 ) THEN
  800                    XS1    = WLV
  801                 ELSE
  802                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WLV   , MAPSF, X1 )
  803                 ENDIF
  804 !
  805               ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 6 ) THEN
  806                 FLONE  = .TRUE.
  807                 FSC    = 0.001
  808                 UNITS  = '1'
  809                 ENAME  = '.ice'
  810                 IF ( ITYPE .EQ. 4 ) THEN
  811                    XS1    = ICE
  812                 ELSE
  813                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, ICE   , MAPSF, X1 )
  814                 ENDIF
  815 !
  816               ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 7 ) THEN
  817                 FLONE  = .TRUE.
  818                 FSC    = 0.0002
  819                 UNITS  = 'km-1'
  820                 ENAME  = '.ibg'
  821                 WHERE ( BERG.NE.UNDEF) BERG = BERG*0.1
  822                 IF ( ITYPE .EQ. 4 ) THEN
  823                    XS1    = BERG
  824                 ELSE
  825                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, BERG  , MAPSF, X1 )
 
 
                                                         ww3_outf.F90  page  16
 
 
  826                 ENDIF
  827 !
  828  
  829               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 1 ) THEN
  830                 FLONE  = .TRUE.
  831                 FSC    = 0.01
  832                 UNITS  = 'm'
  833                 ENAME  = '.hs'
  834                 IF ( ITYPE .EQ. 4 ) THEN
  835                    XS1    = HS
  836                 ELSE
  837                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, HS    , MAPSF, X1 )
  838                 ENDIF
  839 !
  840               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 2 ) THEN
  841                 FLONE  = .TRUE.
  842                 FSC    = 1.
  843                 UNITS  = 'm'
  844                 ENAME  = '.lm'
  845                 IF ( ITYPE .EQ. 4 ) THEN
  846                    XS1    = WLM
  847                 ELSE
  848                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WLM   , MAPSF, X1 )
  849                 ENDIF
  850 !
  851               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 3 ) THEN
  852                 FLONE  = .TRUE.
  853                 FSC    = 0.01
  854                 UNITS  = 's'
  855                 ENAME  = '.t02'
  856                 IF ( ITYPE .EQ. 4 ) THEN
  857                    XS1    = T02
  858                 ELSE
  859                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, T02   , MAPSF, X1 )
  860                 ENDIF
  861 !
  862               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 4 ) THEN
  863                 FLONE  = .TRUE.
  864                 FSC    = 0.01
  865                 UNITS  = 's'
  866                 ENAME  = '.t0m1'
  867                 IF ( ITYPE .EQ. 4 ) THEN
  868                    XS1    = T0M1
  869                 ELSE
  870                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, T0M1  , MAPSF, X1 )
  871                 ENDIF
  872 !
  873               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 5 ) THEN
  874                 FLONE  = .TRUE.
  875                 FSC    = 0.01
  876                 UNITS  = 's'
  877                 ENAME  = '.t01'
  878                 IF ( ITYPE .EQ. 4 ) THEN
  879                    XS1    = T01
  880                 ELSE
 
 
                                                         ww3_outf.F90  page  17
 
 
  881                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, T01   , MAPSF, X1 )
  882                 ENDIF
  883 !
  884               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 6 ) THEN
  885                 FLONE  = .TRUE.
  886                 FSC    = 0.001
  887                 UNITS  = 's-1'
  888                 ENAME  = '.fp'
  889                 IF ( ITYPE .EQ. 4 ) THEN
  890                    XS1    = FP0
  891                 ELSE
  892                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, FP0   , MAPSF, X1 )
  893                 ENDIF
  894 !
  895               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 7 ) THEN
  896                 FLONE  = .TRUE.
  897                 FSC    = 1.
  898                 UNITS  = 'degree'
  899                 ENAME  = '.dir'
  900                 DO ISEA=1, NSEA
  901                   IF ( THM(ISEA) .NE. UNDEF )                         &
  902                        THM(ISEA) = MOD ( 630. - RADE*THM(ISEA) , 360. )
  903                   END DO
  904                 IF ( ITYPE .EQ. 4 ) THEN
  905                    XS1    = THM
  906                 ELSE
  907                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, THM   , MAPSF, X1 )
  908                 ENDIF
  909 !
  910               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 8 ) THEN
  911                 FLONE  = .TRUE.
  912                 FSC    = 0.1
  913                 UNITS  = 'degree'
  914                 ENAME  = '.spr'
  915                 IF ( ITYPE .EQ. 4 ) THEN
  916                    XS1    = THS
  917                 ELSE
  918                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, THS   , MAPSF, X1 )
  919                 ENDIF
  920 !
  921               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 9 ) THEN
  922                 FLONE  = .TRUE.
  923                 FSC    = 1.
  924                 UNITS  = 'degree'
  925                 ENAME  = '.dp'
  926                 DO ISEA=1, NSEA
  927                   IF ( THP0(ISEA) .NE. UNDEF ) THEN
  928                       THP0(ISEA) = MOD ( 630-RADE*THP0(ISEA) , 360. )
  929                     END IF
  930                   END DO
  931                 IF ( ITYPE .EQ. 4 ) THEN
  932                    XS1    = THP0
  933                 ELSE
  934                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, THP0  , MAPSF, X1 )
  935                 ENDIF
 
 
                                                         ww3_outf.F90  page  18
 
 
  936 !
  937               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 10 ) THEN
  938                 FLONE  = .TRUE.
  939                 FSC    = 0.001
  940                 UNITS  = 'm'
  941                 ENAME  = '.hig'
  942                 IF ( ITYPE .EQ. 4 ) THEN
  943                    XS1    = HSIG
  944                 ELSE
  945                   CALL W3S2XY ( NSEA, NSEA, NX+1, NY, HSIG  , MAPSF, X1 )
  946                   END IF
  947 !
  948               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 11 ) THEN
  949                 FLONE  = .TRUE.
  950                 FSC    = 0.002
  951                 UNITS  = 'm'
  952                 ENAME  = '.emc'
  953                 IF ( ITYPE .EQ. 4 ) THEN
  954                    XS1    = STMAXE
  955                 ELSE
  956                   CALL W3S2XY ( NSEA, NSEA, NX+1, NY, STMAXE, MAPSF, X1 )
  957                 END IF
  958 !
  959               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 12 ) THEN
  960                 FLONE  = .TRUE.
  961                 FSC    = 0.002
  962                 UNITS  = 'm'
  963                 ENAME  = '.smc'
  964                 IF ( ITYPE .EQ. 4 ) THEN
  965                    XS1    = STMAXD
  966                 ELSE
  967                   CALL W3S2XY ( NSEA, NSEA, NX+1, NY, STMAXD, MAPSF, X1 )
  968                 ENDIF
  969 !
  970               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 13 ) THEN
  971                 FLONE  = .TRUE.
  972                 FSC    = 0.002
  973                 UNITS  = 'm'
  974                 ENAME  = '.emh'
  975                 IF ( ITYPE .EQ. 4 ) THEN
  976                    XS1    = HMAXE
  977                 ELSE
  978                   CALL W3S2XY ( NSEA, NSEA, NX+1, NY, HMAXE, MAPSF, X1 )
  979                 END IF
  980 !
  981               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 14 ) THEN
  982                 FLONE  = .TRUE.
  983                 FSC    = 0.002
  984                 UNITS  = 'm'
  985                 ENAME  = '.eml'
  986                 IF ( ITYPE .EQ. 4 ) THEN
  987                    XS1    = HCMAXE
  988                 ELSE
  989                   CALL W3S2XY ( NSEA, NSEA, NX+1, NY, HCMAXE, MAPSF, X1 )
  990                 ENDIF
 
 
                                                         ww3_outf.F90  page  19
 
 
  991 !
  992               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 15 ) THEN
  993                 FLONE  = .TRUE.
  994                 FSC    = 0.002
  995                 UNITS  = 'm'
  996                 ENAME  = '.smh'
  997                 IF ( ITYPE .EQ. 4 ) THEN
  998                    XS1    = HMAXD
  999                 ELSE
 1000                   CALL W3S2XY ( NSEA, NSEA, NX+1, NY, HMAXD, MAPSF, X1 )
 1001                 END IF
 1002 !
 1003               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 16 ) THEN
 1004                 FLONE  = .TRUE.
 1005                 FSC    = 0.002
 1006                 UNITS  = 'm'
 1007                 ENAME  = '.sml'
 1008                 IF ( ITYPE .EQ. 4 ) THEN
 1009                    XS1    = HCMAXD
 1010                 ELSE
 1011                   CALL W3S2XY ( NSEA, NSEA, NX+1, NY, HCMAXD, MAPSF, X1)
 1012                 ENDIF
 1013 !
 1014               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 17 ) THEN
 1015                 FLONE  = .TRUE.
 1016                 FSC    = 0.001
 1017                 UNITS  = '1'
 1018                 ENAME  = '.wbt'
 1019                 IF ( ITYPE .EQ. 4 ) THEN
 1020                    XS1    = WBT
 1021                 ELSE
 1022                   CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WBT, MAPSF, X1)
 1023                 ENDIF
 1024 !
 1025               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 18 ) THEN
 1026                 FLONE  = .TRUE.
 1027                 FSC    = 0.01
 1028                 UNITS  = 's'
 1029                 ENAME  = '.tp'
 1030                 DO I=1,NSEA
 1031                   IF(FP0(I) .NE. UNDEF) THEN
 1032                     AUX(I) = 1.0 / FP0(I)
 1033                   ELSE
 1034                     AUX(I) = UNDEF
 1035                   ENDIF
 1036                 ENDDO
 1037                      
 1038                 IF ( ITYPE .EQ. 4 ) THEN
 1039                    XS1    = AUX
 1040                 ELSE
 1041                   CALL W3S2XY ( NSEA, NSEA, NX+1, NY, AUX, MAPSF, X1)
 1042                 ENDIF
 1043 !
 1044               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 1 ) THEN
 1045                 FLONE  = .TRUE.
 
 
                                                         ww3_outf.F90  page  20
 
 
 1046                 FSC    = 0.01
 1047                 UNITS  = 'm'
 1048                 ENAME  = '.phs'
 1049                 IF ( ITYPE .EQ. 4 ) THEN
 1050                    XS1    = PHS(:,IPART)
 1051                 ELSE
 1052                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PHS(:,IPART)      &
 1053                                                       , MAPSF, X1 )
 1054                 ENDIF
 1055 !
 1056               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 2 ) THEN
 1057                 FLONE  = .TRUE.
 1058                 FSC    = 0.01
 1059                 UNITS  = 's'
 1060                 ENAME  = '.ptp'
 1061                 IF ( ITYPE .EQ. 4 ) THEN
 1062                    XS1    = PTP(:,IPART)
 1063                 ELSE
 1064                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PTP(:,IPART)      &
 1065                                                       , MAPSF, X1 )
 1066                 ENDIF
 1067 !
 1068               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 3 ) THEN
 1069                 FLONE  = .TRUE.
 1070                 FSC    = 1.
 1071                 UNITS  = 'm'
 1072                 ENAME  = '.plp'
 1073                 IF ( ITYPE .EQ. 4 ) THEN
 1074                    XS1    = PLP(:,IPART)
 1075                 ELSE
 1076                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PLP(:,IPART)      &
 1077                                                       , MAPSF, X1 )
 1078                 ENDIF
 1079 !
 1080               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 4 ) THEN
 1081                 FLONE  = .TRUE.
 1082                 FSC    = 1.
 1083                 UNITS  = 'degree'
 1084                 ENAME  = '.pdir'
 1085                 DO ISEA=1, NSEA
 1086                   IF ( PDIR(ISEA,IPART) .NE. UNDEF ) THEN
 1087                       PDIR(ISEA,IPART) =                               &
 1088                             MOD ( 630-RADE*PDIR(ISEA,IPART) , 360. )
 1089                     END IF
 1090                   END DO
 1091                 IF ( ITYPE .EQ. 4 ) THEN
 1092                    XS1    = PDIR(:,IPART)
 1093                 ELSE
 1094                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PDIR(:,IPART)      &
 1095                                                       , MAPSF, X1 )
 1096                 ENDIF
 1097 !
 1098               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 5 ) THEN
 1099                 FLONE  = .TRUE.
 1100                 FSC    = 0.1
 
 
                                                         ww3_outf.F90  page  21
 
 
 1101                 UNITS  = 'degree'
 1102                 ENAME  = '.pspr'
 1103                 IF ( ITYPE .EQ. 4 ) THEN
 1104                    XS1    = PSI(:,IPART)
 1105                 ELSE
 1106                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PSI(:,IPART)      &
 1107                                                       , MAPSF, X1 )
 1108                 ENDIF
 1109 !
 1110               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 6 ) THEN
 1111                 FLONE  = .TRUE.
 1112                 FSC    = 0.001
 1113                 UNITS  = '1'
 1114                 ENAME  = '.pws'
 1115                 IF ( ITYPE .EQ. 4 ) THEN
 1116                    XS1    = PWS(:,IPART)
 1117                 ELSE
 1118                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PWS(:,IPART)      &
 1119                                                       , MAPSF, X1 )
 1120                 ENDIF
 1121 !
 1122               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 7 ) THEN
 1123                 FLONE  = .TRUE.
 1124                 FSC    = 1.0
 1125                 UNITS  = 'degree'
 1126                 ENAME  = '.pdp'
 1127                 DO ISEA=1, NSEA
 1128                   IF ( PTHP0(ISEA,IPART) .NE. UNDEF ) THEN
 1129                       PTHP0(ISEA,IPART) =                           &
 1130                             MOD ( 630-RADE*PTHP0(ISEA,IPART) , 360. )
 1131                     END IF
 1132                   END DO
 1133                 IF ( ITYPE .EQ. 4 ) THEN
 1134                    XS1    = PTHP0(:,IPART)
 1135                 ELSE
 1136                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PTHP0(:,IPART),   &
 1137                                                             MAPSF, X1 )
 1138                 ENDIF
 1139 !
 1140               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 8 ) THEN
 1141                 FLONE  = .TRUE.
 1142                 FSC    = 0.01
 1143                 UNITS  = '1'
 1144                 ENAME  = '.pqp'
 1145                 IF ( ITYPE .EQ. 4 ) THEN
 1146                    XS1    = PQP(:,IPART)
 1147                 ELSE
 1148                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PQP(:,IPART), MAPSF, >
 1149                 ENDIF
 1150 !
 1151               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 9 ) THEN
 1152                 FLONE  = .TRUE.
 1153                 FSC    = 0.01
 1154                 UNITS  = '1'
 1155                 ENAME  = '.ppe'
 
 
                                                         ww3_outf.F90  page  22
 
 
 1156                 IF ( ITYPE .EQ. 4 ) THEN
 1157                    XS1    = PPE(:,IPART)
 1158                 ELSE
 1159                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PPE(:,IPART), MAPSF, >
 1160                 ENDIF
 1161 !
 1162               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 10 ) THEN
 1163                 FLONE  = .TRUE.
 1164                 FSC    = 0.0001
 1165                 UNITS  = 's-1'
 1166                 ENAME  = '.pgw'
 1167                 IF ( ITYPE .EQ. 4 ) THEN
 1168                    XS1    = PGW(:,IPART)
 1169                 ELSE
 1170                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PGW(:,IPART), MAPSF, >
 1171                 ENDIF
 1172 !
 1173               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 11 ) THEN
 1174                 FLONE  = .TRUE.
 1175                 FSC    = 0.0001
 1176                 UNITS  = '1'
 1177                 ENAME  = '.psw'
 1178                 IF ( ITYPE .EQ. 4 ) THEN
 1179                    XS1    = PSW(:,IPART)
 1180                 ELSE
 1181                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PSW(:,IPART), MAPSF, >
 1182                 ENDIF
 1183 !
 1184               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 12 ) THEN
 1185                 FLONE  = .TRUE.
 1186                 FSC    = 0.01
 1187                 UNITS  = 's'
 1188                 ENAME  = '.ptm10'
 1189                 IF ( ITYPE .EQ. 4 ) THEN
 1190                    XS1    = PTM1(:,IPART)
 1191                 ELSE
 1192                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PTM1(:,IPART), MAPSF,>
 1193                 ENDIF
 1194 !
 1195               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 13 ) THEN
 1196                 FLONE  = .TRUE.
 1197                 FSC    = 0.01
 1198                 UNITS  = 's'
 1199                 ENAME  = '.pt01'
 1200                 IF ( ITYPE .EQ. 4 ) THEN
 1201                    XS1    = PT1(:,IPART)
 1202                 ELSE
 1203                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PT1(:,IPART), MAPSF, >
 1204                 ENDIF
 1205 !
 1206               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 14 ) THEN
 1207                 FLONE  = .TRUE.
 1208                 FSC    = 0.01
 1209                 UNITS  = 's'
 1210                 ENAME  = '.pt02'
 
 
                                                         ww3_outf.F90  page  23
 
 
 1211                 IF ( ITYPE .EQ. 4 ) THEN
 1212                    XS1    = PT2(:,IPART)
 1213                 ELSE
 1214                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PT2(:,IPART), MAPSF, >
 1215                 ENDIF
 1216 !
 1217               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 15 ) THEN
 1218                 FLONE  = .TRUE.
 1219                 FSC    = 0.02
 1220                 UNITS  = 'm2 s rad-1'
 1221                 ENAME  = '.pep'
 1222                 IF ( ITYPE .EQ. 4 ) THEN
 1223                    XS1    = PEP(:,IPART)
 1224                 ELSE
 1225                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PEP(:,IPART), MAPSF, >
 1226                 ENDIF
 1227 !
 1228               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 16 ) THEN
 1229                 FLONE  = .TRUE.
 1230                 FSC    = 0.001
 1231                 UNITS  = '1'
 1232                 ENAME  = '.tws'
 1233                 IF ( ITYPE .EQ. 4 ) THEN
 1234                    XS1    = PWST(:)
 1235                 ELSE
 1236                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PWST(:), MAPSF, X1 )
 1237                 ENDIF
 1238 !
 1239               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 17 ) THEN
 1240                 FLONE  = .TRUE.
 1241                 FSC    = 1.
 1242                 UNITS  = '1'
 1243                 ENAME  = '.pnr'
 1244                 IF ( ITYPE .EQ. 4 ) THEN
 1245                    XS1    = PNR(:)
 1246                 ELSE
 1247                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PNR(:), MAPSF, X1 )
 1248                 ENDIF
 1249 !
 1250               ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 1 ) THEN
 1251                 IF ( VECTOR ) THEN
 1252                     FLTWO  = .TRUE.
 1253                   ELSE
 1254                     FLDIR  = .TRUE.
 1255                   END IF
 1256                 FSC    = 0.001
 1257                 ENAME  = '.ust'
 1258                 FORMF  = '(1X,20I6)'
 1259                 UNITS  = 'm s-1'
 1260                 IF ( ITYPE .EQ. 4 ) THEN
 1261                    XS1    = UST   (1:NSEA)
 1262                    XS2    = USTDIR(1:NSEA)
 1263                 ELSE
 1264                 CALL W3S2XY (NSEA,NSEA,NX+1,NY, UST   (1:NSEA)        &
 1265                                                       , MAPSF, XX )
 
 
                                                         ww3_outf.F90  page  24
 
 
 1266                 CALL W3S2XY (NSEA,NSEA,NX+1,NY, USTDIR(1:NSEA)        &
 1267                                                       , MAPSF, XY )
 1268                 ENDIF
 1269                 DO ISEA=1, NSEA
 1270                   UABS   = SQRT(UST(ISEA)**2+USTDIR(ISEA)**2)
 1271                   IF ( UST(ISEA) .EQ. UNDEF ) THEN
 1272                       USTDIR(ISEA) = UNDEF
 1273                       UABS         = UNDEF
 1274                     ELSE IF ( UABS .GT. 0.05 ) THEN
 1275                       USTDIR(ISEA) = MOD ( 630. -                     &
 1276                         RADE*ATAN2(USTDIR(ISEA),UST(ISEA)) , 360. )
 1277                     ELSE
 1278                       USTDIR(ISEA) = UNDEF
 1279                     END IF
 1280                   UST(ISEA) = UABS
 1281                   END DO
 1282                 IF ( ITYPE .EQ. 4 ) THEN
 1283                    XS3    = UST   (1:NSEA)
 1284                    XS4    = USTDIR(1:NSEA)
 1285                 ELSE
 1286                 CALL W3S2XY (NSEA,NSEA,NX+1,NY, UST   (1:NSEA)        &
 1287                                                       , MAPSF, X1 )
 1288                 CALL W3S2XY (NSEA,NSEA,NX+1,NY, USTDIR(1:NSEA)        &
 1289                                                       , MAPSF, X2 )
 1290                 ENDIF
 1291 !
 1292               ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 2 ) THEN
 1293                 FLONE  = .TRUE.
 1294                 FSC    = 1.E-6
 1295                 UNITS  = '1'
 1296                 ENAME  = '.cha'
 1297                 IF ( ITYPE .EQ. 4 ) THEN
 1298                    XS1    = CHARN(1:NSEA)
 1299                 ELSE
 1300                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CHARN(1:NSEA)    &
 1301                                                       , MAPSF, X1 )
 1302                 ENDIF
 1303 !
 1304               ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 3 ) THEN
 1305                 FLONE  = .TRUE.
 1306                 FSC    = 0.1           !0.01
 1307                 UNITS  = 'kW m-1'
 1308                 ENAME  = '.cge'
 1309                 DO ISEA=1, NSEA
 1310                    IF ( CGE(ISEA) .NE. UNDEF )                       &
 1311                         CGE(ISEA) = 0.001 * CGE(ISEA)
 1312                    END DO
 1313                 IF ( ITYPE .EQ. 4 ) THEN
 1314                    XS1    = CGE(1:NSEA)
 1315                 ELSE
 1316                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CGE(1:NSEA)      &
 1317                                                       , MAPSF, X1 )
 1318                 ENDIF
 1319 !
 1320               ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 4 ) THEN
 
 
                                                         ww3_outf.F90  page  25
 
 
 1321                 FLONE  = .TRUE.
 1322                 FSC    = 0.01
 1323                 UNITS  = 'W m-2'
 1324                 ENAME  = '.faw'
 1325                 DO ISEA=1, NSEA
 1326                    PHIAW(ISEA)=MIN(99.98,PHIAW(ISEA))
 1327                    END DO
 1328                 IF ( ITYPE .EQ. 4 ) THEN
 1329                    XS1    = PHIAW(1:NSEA)
 1330                 ELSE
 1331                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PHIAW(1:NSEA)    &
 1332                                                           , MAPSF, X1 )
 1333                 ENDIF
 1334 !
 1335               ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 5 ) THEN
 1336                 IF ( VECTOR ) THEN
 1337                     FLTWO  = .TRUE.
 1338                   ELSE
 1339                     FLDIR  = .TRUE.
 1340                   END IF
 1341                 FSC    = 1.E-6
 1342                 UNITS  = 'm2 s-2'
 1343                 ENAME  = '.taw'
 1344                 IF ( ITYPE .EQ. 4 ) THEN
 1345                    XS1    = TAUWIX(1:NSEA)
 1346                    XS2    = TAUWIY(1:NSEA)
 1347                 ELSE
 1348                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWIX(1:NSEA)    &
 1349                                                           , MAPSF, XX )
 1350                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWIY(1:NSEA)    &
 1351                                                           , MAPSF, XY )
 1352                 ENDIF
 1353                 DO ISEA=1, NSEA
 1354                   CABS   = SQRT(TAUWIX(ISEA)**2+TAUWIY(ISEA)**2)
 1355                   IF ( TAUWIX(ISEA) .EQ. UNDEF ) THEN
 1356                       TAUWIY(ISEA) = UNDEF
 1357                       CABS         = UNDEF
 1358                     ELSE IF ( TAUWIX(ISEA) .EQ. 0. .AND.              &
 1359                               TAUWIY(ISEA) .EQ. 0. ) THEN
 1360                       TAUWIY(ISEA) = UNDEF
 1361                     ELSE
 1362                       TAUWIY(ISEA) = MOD ( 630. -                     &
 1363                           RADE*ATAN2(TAUWIY(ISEA),TAUWIX(ISEA)) , 360. )
 1364                     END IF
 1365                   TAUWIX(ISEA) = CABS
 1366                   END DO
 1367                 IF ( ITYPE .EQ. 4 ) THEN
 1368                    XS3    = TAUWIX(1:NSEA)
 1369                    XS4    = TAUWIY(1:NSEA)
 1370                 ELSE
 1371                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWIX(1:NSEA)   &
 1372                                                           , MAPSF, X1 )
 1373                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWIY(1:NSEA)   &
 1374                                                           , MAPSF, X2 )
 1375                 ENDIF
 
 
                                                         ww3_outf.F90  page  26
 
 
 1376 !
 1377               ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 6 ) THEN
 1378                 IF ( VECTOR ) THEN
 1379                     FLTWO  = .TRUE.
 1380                   ELSE
 1381                     FLDIR  = .TRUE.
 1382                   END IF
 1383                 FSC    = 1.E-6
 1384                 UNITS  = 'm2 s-2'
 1385                 ENAME  = '.twa'
 1386                 IF ( ITYPE .EQ. 4 ) THEN
 1387                    XS1    = TAUWNX(1:NSEA)
 1388                    XS2    = TAUWNY(1:NSEA)
 1389                 ELSE
 1390                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWNX(1:NSEA)    &
 1391                                                           , MAPSF, XX )
 1392                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWNY(1:NSEA)    &
 1393                                                           , MAPSF, XY )
 1394                 ENDIF
 1395                 DO ISEA=1, NSEA
 1396                   CABS   = SQRT(TAUWNX(ISEA)**2+TAUWNY(ISEA)**2)
 1397                   IF ( TAUWNX(ISEA) .EQ. UNDEF ) THEN
 1398                       TAUWNY(ISEA) = UNDEF
 1399                       CABS         = UNDEF
 1400                     ELSE IF ( TAUWNX(ISEA) .EQ. 0. .AND.              &
 1401                               TAUWNY(ISEA) .EQ. 0. ) THEN
 1402                       TAUWNY(ISEA) = UNDEF
 1403                     ELSE
 1404                       TAUWNY(ISEA) = MOD ( 630. -                     &
 1405                           RADE*ATAN2(TAUWNY(ISEA),TAUWNX(ISEA)) , 360. )
 1406                     END IF
 1407                   TAUWNX(ISEA) = CABS
 1408                   END DO
 1409                 IF ( ITYPE .EQ. 4 ) THEN
 1410                    XS3    = TAUWNX(1:NSEA)
 1411                    XS4    = TAUWNY(1:NSEA)
 1412                 ELSE
 1413                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWNX(1:NSEA)   &
 1414                                                           , MAPSF, X1 )
 1415                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWNY(1:NSEA)   &
 1416                                                           , MAPSF, X2 )
 1417                 ENDIF
 1418 !
 1419               ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 7 ) THEN
 1420                 FLONE  = .TRUE.
 1421                 FSC    = 0.001
 1422                 UNITS  = '1'
 1423                 ENAME  = '.wcc'
 1424                 IF ( ITYPE .EQ. 4 ) THEN
 1425                    XS1    = WHITECAP(1:NSEA,1)
 1426                 ELSE
 1427                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WHITECAP(1:NSEA,1) &
 1428                                                           , MAPSF, X1 )
 1429                 ENDIF
 1430 !
 
 
                                                         ww3_outf.F90  page  27
 
 
 1431               ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 8 ) THEN
 1432                 FLONE  = .TRUE.
 1433                 FSC    = 0.1
 1434                 UNITS  = 'm'
 1435                 ENAME  = '.wcf'
 1436                 IF ( ITYPE .EQ. 4 ) THEN
 1437                    XS1    = WHITECAP(1:NSEA,2)
 1438                 ELSE
 1439                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WHITECAP(1:NSEA,2) &
 1440                                                           , MAPSF, X1 )
 1441                 ENDIF
 1442 !
 1443               ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 9 ) THEN
 1444                 FLONE  = .TRUE.
 1445                 FSC    = 0.1
 1446                 UNITS  = 'm'
 1447                 ENAME  = '.wch'
 1448                 IF ( ITYPE .EQ. 4 ) THEN
 1449                    XS1    = WHITECAP(1:NSEA,3)
 1450                 ELSE
 1451                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WHITECAP(1:NSEA,3) &
 1452                                                           , MAPSF, X1 )
 1453                 ENDIF
 1454 !
 1455               ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 10 ) THEN
 1456                 FLONE  = .TRUE.
 1457                 FSC    = 0.1
 1458                 UNITS  = '1'
 1459                 ENAME  = '.wcm'
 1460                 IF ( ITYPE .EQ. 4 ) THEN
 1461                    XS1    = WHITECAP(1:NSEA,4)
 1462                 ELSE
 1463                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WHITECAP(1:NSEA,4) &
 1464                                                           , MAPSF, X1 )
 1465                 ENDIF
 1466 !
 1467               ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 1 ) THEN
 1468                 FLTRI  = .TRUE.
 1469                 FSC    = 10.
 1470                 UNITS  = 'N m-1'
 1471                 ENAME  = '.sxy'
 1472                 IF ( ITYPE .EQ. 4 ) THEN
 1473                    XS1    = SXX(1:NSEA)
 1474                    XS2    = SYY(1:NSEA)
 1475                    XS3    = SXY(1:NSEA)
 1476                 ELSE
 1477                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, SXX(1:NSEA)       &
 1478                                                           , MAPSF, X1 )
 1479                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, SYY(1:NSEA)       &
 1480                                                           , MAPSF, X2 )
 1481                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, SXY(1:NSEA)       &
 1482                                                           , MAPSF, XY )
 1483                 ENDIF
 1484 !
 1485               ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 2 ) THEN
 
 
                                                         ww3_outf.F90  page  28
 
 
 1486                 IF ( VECTOR ) THEN
 1487                     FLTWO  = .TRUE.
 1488                   ELSE
 1489                     FLDIR  = .TRUE.
 1490                   END IF
 1491                 FSC    = 1.E-6
 1492                 UNITS  = 'm2 s-2'
 1493                 ENAME  = '.two'
 1494                 IF ( ITYPE .EQ. 4 ) THEN
 1495                    XS1    = TAUOX(1:NSEA)
 1496                    XS2    = TAUOY(1:NSEA)
 1497                 ELSE
 1498                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUOX(1:NSEA)     &
 1499                                                           , MAPSF, XX )
 1500                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUOY(1:NSEA)     &
 1501                                                           , MAPSF, XY )
 1502                 ENDIF
 1503                 DO ISEA=1, NSEA
 1504                   UABS   = SQRT(TAUOX(ISEA)**2+TAUOY(ISEA)**2)
 1505                   IF ( TAUOX(ISEA) .EQ. UNDEF ) THEN
 1506                       TAUOY(ISEA) = UNDEF
 1507                       UABS        = UNDEF
 1508                     ELSE IF ( UABS .GT. 1.E-8 ) THEN
 1509                       TAUOY(ISEA) = MOD ( 630. -                     &
 1510                         RADE*ATAN2(TAUOY(ISEA),TAUOX(ISEA)) , 360. )
 1511                     ELSE
 1512                       TAUOY(ISEA) = UNDEF
 1513                     END IF
 1514                   TAUOX(ISEA) = UABS
 1515                   END DO
 1516                 IF ( ITYPE .EQ. 4 ) THEN
 1517                    XS3    = TAUOX(1:NSEA)
 1518                    XS4    = TAUOY(1:NSEA)
 1519                 ELSE
 1520                 CALL W3S2XY (NSEA,NSEA,NX+1,NY, TAUOX(1:NSEA)         &
 1521                                                       , MAPSF, X1 )
 1522                 CALL W3S2XY (NSEA,NSEA,NX+1,NY, TAUOY(1:NSEA)         &
 1523                                                       , MAPSF, X2 )
 1524                 ENDIF
 1525 !
 1526               ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ.3 ) THEN
 1527                 FLONE  = .TRUE.
 1528                 FSC    = 0.001
 1529                 UNITS  = 'N m-1'
 1530                 ENAME  = '.bhd'
 1531                 IF ( ITYPE .EQ. 4 ) THEN
 1532                    XS1    = BHD(1:NSEA)
 1533                 ELSE
 1534                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, BHD(1:NSEA)    &
 1535                                                           , MAPSF, X1 )
 1536                 ENDIF
 1537 !
 1538               ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 4 ) THEN
 1539                 FLONE  = .TRUE.
 1540                 FSC    = 0.1
 
 
                                                         ww3_outf.F90  page  29
 
 
 1541                 UNITS  = 'W m-2'
 1542                 ENAME  = '.foc'
 1543                 DO ISEA=1, NSEA
 1544                    PHIOC(ISEA)=MIN(99.98,PHIOC(ISEA))
 1545                    END DO
 1546                 IF ( ITYPE .EQ. 4 ) THEN
 1547                    XS1    = PHIOC(1:NSEA)
 1548                 ELSE
 1549                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PHIOC(1:NSEA)    &
 1550                                                           , MAPSF, X1 )
 1551                 ENDIF
 1552 !
 1553               ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 5 ) THEN
 1554                 IF ( VECTOR ) THEN
 1555                     FLTWO  = .TRUE.
 1556                   ELSE
 1557                     FLDIR  = .TRUE.
 1558                   END IF
 1559                 FSC    = 0.001
 1560                 UNITS  = 'm2 s-1'
 1561                 ENAME  = '.tus'
 1562                 IF ( ITYPE .EQ. 4 ) THEN
 1563                    XS1    = TUSX(1:NSEA)
 1564                    XS2    = TUSY(1:NSEA)
 1565                 ELSE
 1566                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TUSX(1:NSEA)      &
 1567                                                           , MAPSF, XX )
 1568                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TUSY(1:NSEA)      &
 1569                                                           , MAPSF, XY )
 1570                 ENDIF
 1571                 DO ISEA=1, NSEA
 1572                   CABS   = SQRT(TUSX(ISEA)**2+TUSY(ISEA)**2)
 1573                   IF ( TUSX(ISEA) .NE. UNDEF ) THEN
 1574                       TUSY(ISEA) = MOD ( 630. -                       &
 1575                             RADE*ATAN2(TUSY(ISEA),TUSX(ISEA)) , 360. )
 1576                     ELSE
 1577                       TUSY(ISEA) = UNDEF
 1578                       CABS       = UNDEF
 1579                     END IF
 1580                   TUSX(ISEA) = CABS
 1581                   END DO
 1582                 IF ( ITYPE .EQ. 4 ) THEN
 1583                    XS3    = TUSX(1:NSEA)
 1584                    XS4    = TUSY(1:NSEA)
 1585                 ELSE
 1586                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY,TUSX,MAPSF, X1 )
 1587                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY,TUSY,MAPSF, X2 )
 1588                 ENDIF
 1589 !
 1590               ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 6 ) THEN
 1591                 IF ( VECTOR ) THEN
 1592                     FLTWO  = .TRUE.
 1593                   ELSE
 1594                     FLDIR  = .TRUE.
 1595                   END IF
 
 
                                                         ww3_outf.F90  page  30
 
 
 1596                 FSC    = 0.001
 1597                 UNITS  = 'm s-1'
 1598                 ENAME  = '.uss'
 1599                 DO ISEA=1, NSEA
 1600                    IF (USSX(ISEA) .NE. UNDEF ) THEN
 1601                        USSX(ISEA)=MAX(-0.9998,MIN(0.9998,USSX(ISEA)))
 1602                        USSY(ISEA)=MAX(-0.9998,MIN(0.9998,USSY(ISEA)))
 1603                      END IF
 1604                    END DO
 1605                 IF ( ITYPE .EQ. 4 ) THEN
 1606                    XS1    = USSX(1:NSEA)
 1607                    XS2    = USSY(1:NSEA)
 1608                 ELSE
 1609                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, USSX(1:NSEA)     &
 1610                                                           , MAPSF, XX )
 1611                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, USSY(1:NSEA)     &
 1612                                                           , MAPSF, XY )
 1613                 ENDIF
 1614                 DO ISEA=1, NSEA
 1615                   CABS   = SQRT(USSX(ISEA)**2+USSY(ISEA)**2)
 1616                   IF ( USSX(ISEA)  .NE. UNDEF ) THEN
 1617                       USSY(ISEA) = MOD ( 630. -                      &
 1618                             RADE*ATAN2(USSY(ISEA),USSX(ISEA)) , 360. )
 1619                     ELSE
 1620                       USSY(ISEA) = UNDEF
 1621                       CABS       = UNDEF
 1622                     END IF
 1623                   USSX(ISEA) = CABS
 1624                   END DO
 1625                 IF ( ITYPE .EQ. 4 ) THEN
 1626                    XS3    = USSX(1:NSEA)
 1627                    XS4    = USSY(1:NSEA)
 1628                 ELSE
 1629                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, USSX(1:NSEA),     &
 1630                                                              MAPSF, X1 )
 1631                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, USSY(1:NSEA),     &
 1632                                                              MAPSF, X2 )
 1633                 ENDIF
 1634 !
 1635               ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 7 ) THEN
 1636                 FLTWO  = .TRUE.
 1637                 FSC    = 0.01
 1638                 ENAME  = '.p2s'
 1639                 UNITS  = 'm4'
 1640                 DO ISEA=1, NSEA
 1641                   PRMS(ISEA)=PRMS(ISEA)
 1642                   END DO
 1643                 IF ( ITYPE .EQ. 4 ) THEN
 1644                    XS1    = PRMS(1:NSEA)
 1645                    XS2    = TPMS(1:NSEA)
 1646                 ELSE
 1647                 CALL W3S2XY ( NSEA, NSEA, NX+1,NY,PRMS,MAPSF, X1 )
 1648                 CALL W3S2XY ( NSEA, NSEA, NX+1,NY,TPMS,MAPSF, X2 )
 1649                 ENDIF
 1650 !
 
 
                                                         ww3_outf.F90  page  31
 
 
 1651               ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 1 ) THEN
 1652                 IF ( VECTOR ) THEN
 1653                     FLTWO  = .TRUE.
 1654                   ELSE
 1655                     FLDIR  = .TRUE.
 1656                   END IF
 1657                 FSC    = 0.01
 1658                 ENAME  = '.abr'
 1659                 UNITS  = 'm'
 1660                 IF ( ITYPE .EQ. 4 ) THEN
 1661                    XS1    = ABA(1:NSEA)
 1662                    XS2    = ABD(1:NSEA)
 1663                 ELSE
 1664                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, ABA(1:NSEA)       &
 1665                                                           , MAPSF, XX )
 1666                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, ABD(1:NSEA)       &
 1667                                                           , MAPSF, XY )
 1668                 ENDIF
 1669                 DO ISEA=1, NSEA
 1670                   IF ( ABA(ISEA) .NE. UNDEF ) THEN
 1671                       AABS   = SQRT(ABA(ISEA)**2+ABD(ISEA)**2)
 1672                       IF ( AABS .GT. 0.005 ) THEN
 1673                           ABD(ISEA) = MOD ( 630. -                    &
 1674                                 RADE*ATAN2(ABD(ISEA),ABA(ISEA)) , 360. )
 1675                         ELSE
 1676                           ABD(ISEA) = UNDEF
 1677                         END IF
 1678                       ABA(ISEA) = AABS
 1679                     END IF
 1680                   END DO
 1681                 IF ( ITYPE .EQ. 4 ) THEN
 1682                    XS3    = ABA(1:NSEA)
 1683                    XS4    = ABD(1:NSEA)
 1684                 ELSE
 1685                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, ABA(1:NSEA)       &
 1686                                                           , MAPSF, X1 )
 1687                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, ABD(1:NSEA)       &
 1688                                                           , MAPSF, X2 )
 1689                 ENDIF
 1690 !
 1691               ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 2 ) THEN
 1692                 IF ( VECTOR ) THEN
 1693                     FLTWO  = .TRUE.
 1694                   ELSE
 1695                     FLDIR  = .TRUE.
 1696                   END IF
 1697                 FSC    = 0.01
 1698                 ENAME  = '.ubr'
 1699                 UNITS  = 'm s-1'
 1700                 IF ( ITYPE .EQ. 4 ) THEN
 1701                    XS1    = UBA(1:NSEA)
 1702                    XS2    = UBD(1:NSEA)
 1703                 ELSE
 1704                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, UBA(1:NSEA)       &
 1705                                                           , MAPSF, XX )
 
 
                                                         ww3_outf.F90  page  32
 
 
 1706                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, UBD(1:NSEA)       &
 1707                                                           , MAPSF, XY )
 1708                 ENDIF
 1709                 DO ISEA=1, NSEA
 1710                   IF ( UBA(ISEA) .NE. UNDEF ) THEN
 1711                       UABS   = SQRT(UBA(ISEA)**2+UBD(ISEA)**2)
 1712                       IF ( UABS .GT. 0.005 ) THEN
 1713                           UBD(ISEA) = MOD ( 630. -                    &
 1714                                 RADE*ATAN2(UBD(ISEA),UBA(ISEA)) , 360. )
 1715                         ELSE
 1716                           UBD(ISEA) = UNDEF
 1717                         END IF
 1718                       UBA(ISEA) = UABS
 1719                     END IF
 1720                   END DO
 1721                 IF ( ITYPE .EQ. 4 ) THEN
 1722                    XS3    = UBA(1:NSEA)
 1723                    XS4    = UBD(1:NSEA)
 1724                 ELSE
 1725                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, UBA(1:NSEA)       &
 1726                                                           , MAPSF, X1 )
 1727                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, UBD(1:NSEA)       &
 1728                                                           , MAPSF, X2 )
 1729                 ENDIF
 1730 !
 1731               ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 3 ) THEN
 1732                 FLTRI  = .TRUE.
 1733                 FSC    = 1.E-2
 1734                 UNITS  = 'm'
 1735                 ENAME  = '.bed'
 1736                 IF ( ITYPE .EQ. 4 ) THEN
 1737                    XS1    = BEDFORMS(1:NSEA,1)
 1738                    XS2    = BEDFORMS(1:NSEA,2)
 1739                    XS3    = BEDFORMS(1:NSEA,3)
 1740                 ELSE
 1741                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, BEDFORMS(1:NSEA,1) &
 1742                                                           , MAPSF, X1 )
 1743                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, BEDFORMS(1:NSEA,2) &
 1744                                                           , MAPSF, X2 )
 1745                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, BEDFORMS(1:NSEA,3) &
 1746                                                           , MAPSF, XY )
 1747                 ENDIF
 1748 !
 1749               ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 4 ) THEN
 1750                 FLONE  = .TRUE.
 1751                 FSC    = 0.1
 1752                 UNITS  = 'W m-2'
 1753                 ENAME  = '.fbb'
 1754                 IF ( ITYPE .EQ. 4 ) THEN
 1755                    XS1    = PHIBBL(1:NSEA)
 1756                 ELSE
 1757                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PHIBBL(1:NSEA)    &
 1758                                                           , MAPSF, X1 )
 1759                 ENDIF
 1760 !
 
 
                                                         ww3_outf.F90  page  33
 
 
 1761               ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 5 ) THEN
 1762                 FLTWO  = .TRUE.
 1763                 FSC    = 1.E-6
 1764                 UNITS  = 'm2 s-2'
 1765                 ENAME  = '.tbb'
 1766                 IF ( ITYPE .EQ. 4 ) THEN
 1767                    XS1    = TAUBBL(1:NSEA,1)
 1768                    XS2    = TAUBBL(1:NSEA,2)
 1769                 ELSE
 1770                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUBBL(1:NSEA,1)  &
 1771                                                           , MAPSF, XX )
 1772                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUBBL(1:NSEA,2)  &
 1773                                                           , MAPSF, XY )
 1774                 ENDIF
 1775 !
 1776               ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 1 ) THEN
 1777                 IF ( VECTOR ) THEN
 1778                     FLTWO  = .TRUE.
 1779                   ELSE
 1780                     FLDIR  = .TRUE.
 1781                   END IF
 1782                 FSC    = 1.E-6
 1783                 ENAME  = '.mss'
 1784                 FORMF  = '(1X,20I6)'
 1785                 UNITS  = '1'
 1786                 IF ( ITYPE .EQ. 4 ) THEN
 1787                    XS1    = MSSX(1:NSEA)
 1788                    XS2    = MSSY(1:NSEA)
 1789                 ELSE
 1790                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, MSSX(1:NSEA),     &
 1791                                                              MAPSF, XX )
 1792                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY ,MSSY(1:NSEA),     &
 1793                                                              MAPSF, XY )
 1794                 ENDIF
 1795 !
 1796               ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 2 ) THEN
 1797                 IF ( VECTOR ) THEN
 1798                     FLTWO  = .TRUE.
 1799                   ELSE
 1800                     FLDIR  = .TRUE.
 1801                   END IF
 1802                 FSC    = 0.00001
 1803                 ENAME  = '.msc'
 1804                 UNITS  = '1'
 1805                 IF ( ITYPE .EQ. 4 ) THEN
 1806                    XS1    = MSCX(1:NSEA)
 1807                    XS2    = MSCY(1:NSEA)
 1808                 ELSE
 1809                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, MSCX(1:NSEA),     &
 1810                                                              MAPSF, XX )
 1811                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, MSCY(1:NSEA),     &
 1812                                                              MAPSF, XY )
 1813                 ENDIF
 1814                 DO ISEA=1, NSEA
 1815                   CABS   = SQRT(MSCX(ISEA)**2+MSCY(ISEA)**2)
 
 
                                                         ww3_outf.F90  page  34
 
 
 1816                   IF ( MSCX(ISEA)  .EQ. UNDEF ) THEN
 1817                       MSCY(ISEA) = UNDEF
 1818                       CABS       = UNDEF
 1819                     ELSE IF ( MSCX(ISEA)  .EQ. 0. .AND.              &
 1820                               MSCY(ISEA)  .EQ. 0. ) THEN
 1821                       MSCY(ISEA) = UNDEF
 1822                     ELSE
 1823                       MSCY(ISEA) = MOD ( 630. -                      &
 1824                             RADE*ATAN2(MSCY(ISEA),MSCX(ISEA)) , 360. )
 1825                     END IF
 1826                   MSCX(ISEA) = CABS
 1827                   END DO
 1828                 IF ( ITYPE .EQ. 4 ) THEN
 1829                    XS3    = MSCX(1:NSEA)
 1830                    XS4    = MSCY(1:NSEA)
 1831                 ELSE
 1832                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, MSCX(1:NSEA),     &
 1833                                                              MAPSF, X1 )
 1834                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, MSCY(1:NSEA),     &
 1835                                                              MAPSF, X2 )
 1836                 ENDIF
 1837 !
 1838               ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 3 ) THEN
 1839                 FLONE  = .TRUE.
 1840                 FSC    = 0.1
 1841                 UNITS  = 'degree'
 1842                 ENAME  = '.msd'
 1843                 DO ISEA=1, NSEA
 1844                   IF ( MSSD(ISEA) .NE. UNDEF )  THEN
 1845                     MSSD(ISEA) = MOD ( 630. - RADE*MSSD(ISEA) , 180. )
 1846                     END IF
 1847                   END DO
 1848                 IF ( ITYPE .EQ. 4 ) THEN
 1849                    XS1    = MSSD(1:NSEA)
 1850                 ELSE
 1851                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, MSSD(1:NSEA), MAPSF, >
 1852                 ENDIF
 1853 !
 1854               ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 4 ) THEN
 1855                 FLONE  = .TRUE.
 1856                 FSC    = 0.1
 1857                 UNITS  = 'degree'
 1858                 ENAME  = '.mcd'
 1859                 DO ISEA=1, NSEA
 1860                   IF ( MSCD(ISEA) .NE. UNDEF )  THEN
 1861                     MSCD(ISEA) = MOD ( 630. - RADE*MSCD(ISEA) , 180. )
 1862                     END IF
 1863                   END DO
 1864                 IF ( ITYPE .EQ. 4 ) THEN
 1865                    XS1    = MSCD(1:NSEA)
 1866                 ELSE
 1867                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, MSCD(1:NSEA), MAPSF, >
 1868                 ENDIF
 1869 !
 1870               ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 5 ) THEN
 
 
                                                         ww3_outf.F90  page  35
 
 
 1871                 FLONE  = .TRUE.
 1872                 FSC    = 0.01
 1873                 UNITS  = '1'
 1874                 ENAME  = '.qp'
 1875                 IF ( ITYPE .EQ. 4 ) THEN
 1876                    XS1    = QP
 1877                 ELSE
 1878                   CALL W3S2XY ( NSEA, NSEA, NX+1, NY, QP, MAPSF, X1 )
 1879                 ENDIF
 1880 !
 1881               ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 1 ) THEN
 1882                 FLONE  = .TRUE.
 1883                 FSC    = 0.1
 1884                 UNITS  = 'min.'
 1885                 ENAME  = '.dtd'
 1886                 DO ISEA=1, NSEA
 1887                   IF ( DTDYN(ISEA) .NE. UNDEF )                       &
 1888                        DTDYN(ISEA) = DTDYN(ISEA) / 60.
 1889                   END DO
 1890                 IF ( ITYPE .EQ. 4 ) THEN
 1891                    XS1    = DTDYN(1:NSEA)
 1892                 ELSE
 1893                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, DTDYN , MAPSF, X1 )
 1894                 ENDIF
 1895 !
 1896               ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 2 ) THEN
 1897                 FLONE  = .TRUE.
 1898                 FSC    = 0.001
 1899                 UNITS  = 's-1'
 1900                 ENAME  = '.fc'
 1901                 IF ( ITYPE .EQ. 4 ) THEN
 1902                    XS1    = FCUT
 1903                 ELSE
 1904                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, FCUT  , MAPSF, X1 )
 1905                 ENDIF
 1906 !
 1907               ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 3 ) THEN
 1908                 FLONE  = .TRUE.
 1909                 FSC    = 0.001
 1910                 FSC    = 1.
 1911                 UNITS  = '1'
 1912                 ENAME  = '.cfx'
 1913                 IF ( ITYPE .EQ. 4 ) THEN
 1914                    XS1    = CFLXYMAX
 1915                 ELSE
 1916                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CFLXYMAX, MAPSF, X1 )
 1917                 ENDIF
 1918 !
 1919               ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 4 ) THEN
 1920                 FLONE  = .TRUE.
 1921                 FSC    = 0.001
 1922                 UNITS  = '1'
 1923                 ENAME  = '.cfd'
 1924                 IF ( ITYPE .EQ. 4 ) THEN
 1925                    XS1    = CFLTHMAX
 
 
                                                         ww3_outf.F90  page  36
 
 
 1926                 ELSE
 1927                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CFLTHMAX, MAPSF, X1 )
 1928                 ENDIF
 1929 !
 1930               ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 5 ) THEN
 1931                 FLONE  = .TRUE.
 1932                 FSC    = 0.001
 1933                 UNITS  = '1'
 1934                 ENAME  = '.cfk'
 1935                 IF ( ITYPE .EQ. 4 ) THEN
 1936                    XS1    = CFLKMAX
 1937                 ELSE
 1938                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, CFLKMAX, MAPSF, X1 )
 1939                 ENDIF
 1940 !
 1941              ELSE IF ( IFI .EQ. 10 ) THEN
 1942                 FLONE  = .TRUE.
 1943                 FSC    = 1.
 1944                 UNITS  = 'TBD'
 1945                 WRITE (ENAME,'(A2,I2.2)') '.u', IFJ
 1946                 IF ( ITYPE .EQ. 4 ) THEN
 1947                    XS1    = USERO(:,IFJ)
 1948                 ELSE
 1949                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, USERO(:,IFJ)      &
 1950                                                           , MAPSF, X1 )
 1951                 ENDIF
 1952 !
 1953               ELSE
 1954                 WRITE (NDSE,990) IFI,IFJ
 1955                 WRITE (NDSE,999)
 1956                 CALL EXTCDE ( 1 )
 1957 !
 1958               END IF
 1959 !
 1960 ! 2.b Make map
 1961 !
 1962             DO IX=1, NX
 1963               DO IY=1, NY
 1964                 IF ( MAPSTA(IY,IX) .EQ. 0 ) THEN
 1965                     X1(IX,IY) = UNDEF
 1966                     X2(IX,IY) = UNDEF
 1967                     XX(IX,IY) = UNDEF
 1968                     XY(IX,IY) = UNDEF
 1969                   END IF
 1970                 IF ( X1(IX,IY) .EQ. UNDEF ) THEN
 1971                     MAP(IX,IY) = 0
 1972                   ELSE
 1973                     MAP(IX,IY) = 1
 1974                   END IF
 1975                 IF ( X2(IX,IY) .EQ. UNDEF ) THEN
 1976                     MP2(IX,IY) = 0
 1977                   ELSE
 1978                     MP2(IX,IY) = 1
 1979                   END IF
 1980                 END DO
 
 
                                                         ww3_outf.F90  page  37
 
 
 1981               END DO
 1982 !
 1983 ! 2.c Perform output type 1 ( print plots )
 1984 !
 1985             IF ( ITYPE .EQ. 1 ) THEN
 1986 !
 1987                 IF ( SCALE ) THEN
 1988                     FSC    = 0.
 1989                     FSCA   = 0.
 1990                   ELSE
 1991                     FSCA   = 1.
 1992                   END IF
 1993                 IXB    = IX1 - IXS
 1994 !
 1995                 DO IB=1, NBLOK
 1996                   IXA    = IXB + IXS
 1997                   IXB    = IXA + (NXMAX-1)*IXS
 1998                   IXB    = MIN ( IXB , IXN )
 1999                   IF ( FLTRI ) THEN
 2000                       CALL PRTBLK (NDSO, NX, NY, NX+1, X1, MAP, 0,    &
 2001                                    FSC, IXA, IXB, IXS, IY1, IYN, IYS, &
 2002                                    IDOUT(IFI,IFJ), UNITS)
 2003                       CALL PRTBLK (NDSO, NX, NY, NX+1, X2, MAP, 0,    &
 2004                                    FSC, IXA, IXB, IXS, IY1, IYN, IYS, &
 2005                                    IDOUT(IFI,IFJ), UNITS)
 2006                       CALL PRTBLK (NDSO, NX, NY, NX+1, XY, MAP, 0,    &
 2007                                    FSC, IXA, IXB, IXS, IY1, IYN, IYS, &
 2008                                    IDOUT(IFI,IFJ), UNITS)
 2009                     ELSE IF ( FLONE ) THEN
 2010                       CALL PRTBLK (NDSO, NX, NY, NX+1, X1, MAP, 0,    &
 2011                                    FSC, IXA, IXB, IXS, IY1, IYN, IYS, &
 2012                                    IDOUT(IFI,IFJ), UNITS)
 2013                     ELSE IF ( FLTWO ) THEN
 2014                       CALL PRTBLK (NDSO, NX, NY, NX+1, XX, MAP, 0,    &
 2015                                    FSC, IXA, IXB, IXS, IY1, IYN, IYS, &
 2016                                    IDOUT(IFI,IFJ), UNITS)
 2017                       CALL PRTBLK (NDSO, NX, NY, NX+1, XY, MAP, 0,    &
 2018                                    FSC, IXA, IXB, IXS, IY1, IYN, IYS, &
 2019                                    IDOUT(IFI,IFJ), UNITS)
 2020                     ELSE IF ( FLDIR ) THEN
 2021                       CALL PRTBLK (NDSO, NX, NY, NX+1, X1, MAP, 0,    &
 2022                                    FSC, IXA, IXB, IXS, IY1, IYN, IYS, &
 2023                                    IDOUT(IFI,IFJ), UNITS)
 2024                       CALL PRTBLK (NDSO, NX, NY, NX+1, X2, MP2, 0,    &
 2025                                   FSCA, IXA, IXB, IXS, IY1, IYN, IYS, &
 2026                                    IDOUT(IFI,IFJ), 'Deg.')
 2027                     END IF
 2028                   END DO
 2029 !
 2030 ! 2.d Perform output type 2 ( statistics )
 2031 !
 2032               ELSE IF ( ITYPE .EQ. 2 ) THEN
 2033                 XMIN   =  1.E20
 2034                 XMAX   = -1.E20
 2035                 XDS    =  0.D0
 
 
                                                         ww3_outf.F90  page  38
 
 
 2036                 XDSQ   =  0.D0
 2037                 NINGRD =  0
 2038 !
 2039                 DO IX=IX1, IXN
 2040                   DO IY=IY1, IYN
 2041                     IF ( MAPSTA(IY,IX) .GT. 0 .AND.                   &
 2042                          X1(IX,IY) .NE. UNDEF ) THEN
 2043                         NINGRD = NINGRD + 1
 2044                         XMIN   = MIN ( XMIN , X1(IX,IY) )
 2045                         XMAX   = MAX ( XMAX , X1(IX,IY) )
 2046                         XDS    = XDS  + DBLE(X1(IX,IY))
 2047                         XDSQ   = XDSQ + DBLE(X1(IX,IY))**2
 2048                       END IF
 2049                     END DO
 2050                   END DO
 2051 !
 2052                 NDSDT  = NDSDT + 1
 2053 !
 2054                 IF ( NINGRD .EQ. 0 ) THEN
 2055                     WRITE (NDSDT,940) TIME(1), IH, IM, IS
 2056                   ELSE IF ( NINGRD .LE. 2 ) THEN
 2057                     XAVG   = REAL ( XDS / DBLE(NINGRD) )
 2058                     WRITE (NDSDT,940) TIME(1), IH, IM, IS,            &
 2059                                    XMIN, XMAX
 2060                   ELSE
 2061                     XAVG   = REAL ( XDS / DBLE(NINGRD) )
 2062                     XSTD   = REAL ( ( XDSQ - XDS**2/DBLE(NINGRD) )    &
 2063                                          / DBLE(NINGRD-1) )
 2064                     XSTD   = SQRT ( MAX ( XSTD , 0. ) )
 2065                     WRITE (NDSDT,940) TIME(1), IH, IM, IS,            &
 2066                                    XMIN, XMAX, XAVG, XSTD
 2067                   END IF
 2068 !
 2069 ! 2.e Perform output type 3 ( file )
 2070 !
 2071               ELSE IF ( ITYPE .EQ. 3 ) THEN
 2072 !
 2073                 FNAME(13:) = ENAME
 2074                 IF ( IDFM .EQ. 3 ) THEN
 2075                   IF(GTYPE .NE. UNGTYPE) THEN
 2076                     JJ     = LEN_TRIM(FNMPRE)
 2077                     OPEN (NDSDAT,FILE=FNMPRE(:JJ)//FNAME,             &
 2078                           FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
 2079                     WRITE (NDSDAT) FILEID, TIME,                      &
 2080                        MINVAL(XGRD(IY1:IYN,IX1:IXN)),                 &
 2081                        MAXVAL(XGRD(IY1:IYN,IX1:IXN)), IXN-IX1+1,      &
 2082                        MINVAL(YGRD(IY1:IYN,IX1:IXN)),                 &
 2083                        MAXVAL(YGRD(IY1:IYN,IX1:IXN)), IYN-IY1+1,      &
 2084                        ENAME, FSC, UNITS, IDLA, IDFM, FORMF, MFILL
 2085                   ELSE
 2086                     OPEN (NDSDAT,FILE=FNAME,             &
 2087                     FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
 2088                     WRITE (NDSDAT) FILEID, TIME,                      &
 2089                          X0,MAXX,NX, &
 2090                          Y0,MAXY,NY, &
 
 
                                                         ww3_outf.F90  page  39
 
 
 2091                          ENAME, FSC, UNITS, IDLA, IDFM, FORMF, MFILL
 2092                   ENDIF
 2093                 ELSE
 2094                    IF(GTYPE .NE. UNGTYPE) THEN
 2095                      JJ     = LEN_TRIM(FNMPRE)
 2096                      OPEN (NDSDAT,FILE=FNMPRE(:JJ)//FNAME,ERR=800,     &
 2097                           IOSTAT=IERR)
 2098                      IF (FSC.LT.1E-4) THEN
 2099                        WRITE(FSCS,'(G7.1)') FSC
 2100                      ELSE
 2101                        WRITE(FSCS,'(F7.4)') FSC
 2102                        END IF
 2103                     IF ( FLAGLL ) THEN
 2104                         WRITE (NDSDAT,950) FILEID, TIME,               &
 2105                         MINVAL(XGRD(IY1:IYN,IX1:IXN)),                 &
 2106                         MAXVAL(XGRD(IY1:IYN,IX1:IXN)), IXN-IX1+1,      &
 2107                         MINVAL(YGRD(IY1:IYN,IX1:IXN)),                 &
 2108                         MAXVAL(YGRD(IY1:IYN,IX1:IXN)), IYN-IY1+1,      &
 2109                         ENAME, FSCS, UNITS, IDLA, IDFM, FORMF, MFILL
 2110                     ELSE
 2111                         WRITE (NDSDAT,960) FILEID, TIME,               &
 2112                         MINVAL(XGRD(IY1:IYN,IX1:IXN)),                 &
 2113                         MAXVAL(XGRD(IY1:IYN,IX1:IXN)), IXN-IX1+1,      &
 2114                         MINVAL(YGRD(IY1:IYN,IX1:IXN)),                 &
 2115                         MAXVAL(YGRD(IY1:IYN,IX1:IXN)), IYN-IY1+1,      &
 2116                         ENAME, FSCS, UNITS, IDLA, IDFM, FORMF, MFILL
 2117                      END IF
 2118                    ELSE
 2119                      OPEN (NDSDAT,FILE=FNAME,                      &
 2120                         ERR=800,IOSTAT=IERR)
 2121                       WRITE (NDSDAT, 949) FILEID, TIME,              &
 2122                          X0,MAXX,NX, &
 2123                          Y0,MAXY,NY, &
 2124                          ENAME, FSC, UNITS, IDLA, IDFM, FORMF, MFILL
 2125                      ENDIF
 2126                 END IF
 2127 !
 2128                   IF ( FLTRI ) THEN
 2129                    DO IX=IX1, IXN
 2130                      DO IY=IY1, IYN
 2131                         IF ( MAPSTA(IY,IX) .LE. 0 .OR.                &
 2132                              XX(IX,IY) .EQ. UNDEF ) THEN
 2133                             MXX(IX,IY) = MFILL
 2134                             MYY(IX,IY) = MFILL
 2135                             MXY(IX,IY) = MFILL
 2136                           ELSE
 2137                             MXX(IX,IY) = NINT(X1(IX,IY)/FSC)
 2138                             MYY(IX,IY) = NINT(X2(IX,IY)/FSC)
 2139                             MXY(IX,IY) = NINT(XY(IX,IY)/FSC)
 2140                           END IF
 2141                         END DO
 2142                       END DO
 2143                     IF ( IDLA .NE. 5 ) THEN
 2144                         CALL OUTA2I ( MXX, NX, NY, IX1, IXN, IY1, IYN, &
 2145                            NDSDAT, NDST, NDSE, IDFM, FORMF, IDLA, 1, 0 )
 
 
                                                         ww3_outf.F90  page  40
 
 
 2146                         CALL OUTA2I ( MYY, NX, NY, IX1, IXN, IY1, IYN, &
 2147                            NDSDAT, NDST, NDSE, IDFM, FORMF, IDLA, 1, 0 )
 2148                         CALL OUTA2I ( MXY, NX, NY, IX1, IXN, IY1, IYN, &
 2149                            NDSDAT, NDST, NDSE, IDFM, FORMF, IDLA, 1, 0 )
 2150                        ELSE
 2151                         DO IY=IY1,IYN
 2152                           YGBX   = Y0 + REAL(IY-1)*SY
 2153                           DO IX=IX1, IXN
 2154                             XGBX   = X0 + REAL(IX-1)*SX
 2155                             IF ( MXX(IX,IY) .NE. MFILL ) THEN
 2156                                 IF ( IDFM .EQ. 3 ) THEN
 2157                                     WRITE (NDSDAT)                    &
 2158                                       XGBX, YGBX, MXX(IX,IY), MYY(IX,IY)
 2159                                   ELSE
 2160                                     WRITE (NDSDAT,951)                &
 2161                                       XGBX, YGBX, MXX(IX,IY), MYY(IX,IY)
 2162                                  END IF
 2163                               END IF
 2164                             END DO
 2165                           END DO
 2166                       END IF
 2167                 ELSE
 2168                   IF ( FLTWO .OR. FLDIR ) THEN
 2169                     DO IX=IX1, IXN
 2170                      DO IY=IY1, IYN
 2171                         IF ( MAPSTA(IY,IX) .LE. 0 .OR.                &
 2172                              XX(IX,IY) .EQ. UNDEF ) THEN
 2173                             MXX(IX,IY) = MFILL
 2174                             MYY(IX,IY) = MFILL
 2175                           ELSE
 2176                             MXX(IX,IY) = NINT(XX(IX,IY)/FSC)
 2177                             MYY(IX,IY) = NINT(XY(IX,IY)/FSC)
 2178                           END IF
 2179                         END DO
 2180                       END DO
 2181                     IF ( IDLA .NE. 5 ) THEN
 2182                         CALL OUTA2I ( MXX, NX, NY, IX1, IXN, IY1, IYN, &
 2183                              NDSDAT, NDST, NDSE, IDFM, FORMF, IDLA, 1,0)
 2184                         CALL OUTA2I ( MYY, NX, NY, IX1, IXN, IY1, IYN, &
 2185                              NDSDAT, NDST, NDSE, IDFM, FORMF, IDLA, 1,0)
 2186                       ELSE
 2187                         DO IY=IY1,IYN
 2188                           DO IX=IX1, IXN
 2189                             YGBX   = YGRD(IY,IX)
 2190                             XGBX   = XGRD(IY,IX)
 2191                             IF ( MXX(IX,IY) .NE. MFILL ) THEN
 2192                                 IF ( IDFM .EQ. 3 ) THEN
 2193                                     WRITE (NDSDAT)                    &
 2194                                       XGBX, YGBX, MXX(IX,IY), MYY(IX,IY)
 2195                                   ELSE
 2196                                     IF ( FLAGLL ) THEN
 2197                                         WRITE (NDSDAT,951) XGBX, YGBX, &
 2198                                            MXX(IX,IY), MYY(IX,IY)
 2199                                       ELSE
 2200                                         WRITE (NDSDAT,961) XGBX, YGBX, &
 
 
                                                         ww3_outf.F90  page  41
 
 
 2201                                            MXX(IX,IY), MYY(IX,IY)
 2202                                       END IF
 2203                                   END IF
 2204                               END IF
 2205                             END DO
 2206                           END DO
 2207                       END IF
 2208                   ELSE
 2209                     DO IX=IX1, IXN
 2210                       DO IY=IY1, IYN
 2211                         IF ( MAPSTA(IY,IX) .LE. 0 .OR.                &
 2212                              X1(IX,IY) .EQ. UNDEF ) THEN
 2213                             MX1(IX,IY) = MFILL
 2214                           ELSE
 2215                             MX1(IX,IY) = NINT(X1(IX,IY)/FSC)
 2216                           END IF
 2217                         END DO
 2218                       END DO
 2219                     IF ( IDLA .NE. 5 ) THEN
 2220                         CALL OUTA2I ( MX1, NX, NY, IX1, IXN, IY1, IYN, &
 2221                              NDSDAT, NDST, NDSE, IDFM, FORMF, IDLA, 1,0)
 2222                       ELSE
 2223                         DO IY=IY1,IYN
 2224                           DO IX=IX1, IXN
 2225                             YGBX   = YGRD(IY,IX)
 2226                             XGBX   = XGRD(IY,IX)
 2227                             IF ( MX1(IX,IY) .NE. MFILL ) THEN
 2228                                 IF ( IDFM .EQ. 3 ) THEN
 2229                                     WRITE (NDSDAT)                    &
 2230                                       XGBX, YGBX, MX1(IX,IY)
 2231                                   ELSE
 2232                                     IF ( FLAGLL ) THEN
 2233                                         WRITE (NDSDAT,951) XGBX, YGBX, &
 2234                                            MX1(IX,IY)
 2235                                       ELSE
 2236                                         WRITE (NDSDAT,961) XGBX, YGBX, &
 2237                                            MX1(IX,IY)
 2238                                       END IF
 2239                                   END IF
 2240                               END IF
 2241                             END DO
 2242                           END DO
 2243                       END IF
 2244                     END IF
 2245                   END IF
 2246 !
 2247                 CLOSE (NDSDAT)
 2248 !
 2249               ELSE IF ( ITYPE .EQ. 4 ) THEN
 2250 !
 2251                 FNAME(13:) = ENAME
 2252                 JJ     = LEN_TRIM(FNMPRE)
 2253                 OPEN (NDSDAT,FILE=FNMPRE(:JJ)//FNAME,ERR=800,     &
 2254                       IOSTAT=IERR)
 2255                 WRITE (6,*)  FNAME(1:16)
 
 
                                                         ww3_outf.F90  page  42
 
 
 2256 !
 2257                 IF ( FLTRI ) THEN
 2258                   WRITE (NDSDAT,980)  FILEID, TIME, NSEA, 3,       &
 2259                                  FSC, ENAME, UNITS, GNAME
 2260                   WRITE(NDSDAT, 113)  XS1
 2261                   WRITE(NDSDAT, 113)  XS2
 2262                   WRITE(NDSDAT, 113)  XS3
 2263                 ENDIF
 2264                 IF ( FLTWO .OR. FLDIR ) THEN
 2265                   WRITE (NDSDAT,980)  FILEID, TIME, NSEA, 2,       &
 2266                                  FSC, ENAME, UNITS, GNAME
 2267                   WRITE(NDSDAT, 113)  XS1
 2268                   WRITE(NDSDAT, 113)  XS2
 2269                 ENDIF
 2270                 IF ( FLONE ) THEN
 2271                   WRITE (NDSDAT,980)  FILEID, TIME, NSEA, 1,       &
 2272                                  FSC, ENAME, UNITS, GNAME
 2273                   WRITE(NDSDAT, 113)  XS1
 2274                 ENDIF
 2275 !
 2276                 CLOSE (NDSDAT)
 2277 !
 2278               END IF
 2279 !
 2280 ! ... End of fields loop
 2281 !
 2282           END IF
 2283         END DO
 2284         END DO
 2285 !
 2286       RETURN
 2287 !
 2288 ! Error escape locations
 2289 !
 2290   800 CONTINUE
 2291       WRITE (NDSE,1000) IERR
 2292       CALL EXTCDE (2)
 2293 !
 2294 ! Formats
 2295 !
 2296   113 FORMAT ((10ES11.3))
 2297   980 FORMAT (1X,A13,I9.8,I7.6,I9,I3,ES10.2,1X,A4,1X,A10,1X,A30)
 2298                                                                 
 2299   940 FORMAT (1X,I8,3I3.2,2X,4E12.4)
 2300   949 FORMAT (1X,A13,I9.8,I7.6,2(2F8.2,I8),                      &
 2301                     1X,A4,F8.4,1X,A10,2I2,1X,A11,I4)
 2302   950 FORMAT (1X,A13,1X,I9.8,1X,I7.6,2(1X,2F8.2,1X,I4),          &
 2303               1X,A4,1X,A7,1X,A10,1X,2I2,1X,A11,1X,I4)
 2304   951 FORMAT (1X,2F10.5,2I8)
 2305   960 FORMAT (1X,A13,I9.8,I7.6,2(2E11.3,I4),                     &
 2306               1X,A4,1X,A7,1X,A10,2I2,1X,A11,I4)
 2307   961 FORMAT (1X,2E12.4,2I8)
 2308 !
 2309   990 FORMAT (/' *** WAVEWATCH III ERROR IN W3EXGO :'/           &
 2310                '     GROUP',I2,' PARAMETER',I3,' NOT LISTED '    )
 
 
                                                         ww3_outf.F90  page  43
 
 
 2311   999 FORMAT (/' *** WAVEWATCH III ERROR IN W3EXGO :'/                &
 2312                '     PLEASE UPDATE FIELDS !!! '/)
 2313 !
 2314  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3EXGO : '/               &
 2315                '     ERROR IN OPENING OUTPUT FILE'/                   &
 2316                '     IOSTAT =',I5/)
 2317 !
 2318 !/
 2319 !/ End of W3EXGO ----------------------------------------------------- /
 2320 !/
 2321       END SUBROUTINE W3EXGO
 2322 !/
 2323 !
 2324 !/ End of W3OUTF ----------------------------------------------------- /
 2325 !/
 2326       END PROGRAM W3OUTF
 2327                         
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
