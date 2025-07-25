                                                          gx_outp.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM GXOUTP
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |            J.H. Alves             |
    9 !/                  |            F. Ardhuin             |
   10 !/                  |                        FORTRAN 90 |
   11 !/                  | Last update :         27-Aug-2015 |
   12 !/                  +-----------------------------------+
   13 !/
   14 !/    30-Jun-1999 : Final FORTRAN 77                    ( version 1.18 )
   15 !/    24-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
   16 !/    14-Feb-2000 : Exact nonlinear interactions        ( version 2.01 )
   17 !/    25-Jan-2001 : Cartesian grid version              ( version 2.06 )
   18 !/    02-Feb-2001 : Xnl version 3.0                     ( version 2.07 )
   19 !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
   20 !/    27-Nov-2002 : First version of VDIA and MDIA.     ( version 3.01 )
   21 !/    01-Aug-2003 : Fix format for SH output points.    ( version 3.03 )
   22 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
   23 !/    23-Jun-2006 : Linear input added.                 ( version 3.09 )
   24 !/    29-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   25 !/    03-Jul-2006 : Separate flux modules.              ( version 3.09 )
   26 !/    25-Jul-2006 : Grid ID for each point.             ( version 3.10 )
   27 !/    25-Apr-2007 : EMEAN in W3SPR2 par list.           ( version 3.11 )
   28 !/    09-Oct-2007 : WAM 4+ Sin and Sds added.           ( version 3.13 )
   29 !/                  (F. Ardhuin)
   30 !/    09-Oct-2007 : Experimental Sbs (BS1) added.       ( version 3.13 )
   31 !/                  (F. Ardhuin)
   32 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   33 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   34 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   35 !/    30-Aug-2010 : Adding ST4                          ( version 3.14 )
   36 !/    20-Apr-2010 : Fix initialization of USTAR.      ( version 3.14.1 )
   37 !/    23-Aug-2012 : Adding movable bed friction BT4     ( version 4.07 )
   38 !/    16-Jul-2012 : Move GMD (SNL3) and nonlinear filter (SNLS)
   39 !/                  from 3.15 (HLT).                    ( version 4.08 )
   40 !/    26-Dec-2012 : Modified obsolete declarations.     ( version 4.11 )
   41 !/    27-Aug-2015 : Sice add as additional output       ( version 5.10 )
   42 !/                  (in source terms)
   43 !/
   44 !/    Copyright 2009-2012 National Weather Service (NWS),
   45 !/       National Oceanic and Atmospheric Administration.  All rights
   46 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   47 !/       No unauthorized use without permission.
   48 !/
   49 !  1. Purpose :
   50 !
   51 !     Post-processing of point output for GrADS post-processing.
   52 !
   53 !  2. Method :
   54 !
   55 !     In order to be able to plot spectra and source terms as
 
 
                                                          gx_outp.F90  page   2
 
 
   56 !     fields, spectral data is written as if it is fields data.
   57 !     The spectral direction becomes the longitude, 90.-FREQ
   58 !     become the latitude. This way, polar plots can be made
   59 !     using the GrADS 'NPS' map option. The level or z coordinate
   60 !     is used to store spectra and source terms for separate
   61 !     output points. The name of the output point is stored in
   62 !     the control file as the 'description' of the field.
   63 !     Also written is a separate file with mean input and wave
   64 !     parameters. This file contains per level and per time a
   65 !     single line containing :
   66 !
   67 !        Station ID, Longitude, Latitude, Depth, , Wind speed.
   68 !           U and V components, Air-Sea Temperature difference,
   69 !           Current velocity, U and V components, Significant
   70 !           wave height.
   71 !
   72 !     The files generated are :
   73 !
   74 !       ww3.spec.ctl     GrADS control file.
   75 !       ww3.spec.grads   GrADS data file.
   76 !       ww3.mean.grads   File with additional input and wave
   77 !                        parameters.
   78 !
   79 !     The first direction set to 90 degr. Grads NPS plot should
   80 !     therefore have 'set lon -180 180' for oceanographic directional
   81 !     convention.
   82 !
   83 !     Examples of using the three files can be found in spec.gs and
   84 !     source.gs.
   85 !
   86 !  3. Parameters :
   87 !
   88 !  4. Subroutines used :
   89 !
   90 !      Name      Type  Module   Description
   91 !     ----------------------------------------------------------------
   92 !      W3NMOD    Subr. W3GDATMD Set number of model.
   93 !      W3SETG    Subr.   Id.    Point to selected model.
   94 !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
   95 !      W3SETW    Subr.   Id.    Point to selected model for wave data.
   96 !      W3NAUX    Subr. W3ADATMD Set number of model for aux data.
   97 !      W3SETA    Subr.   Id.    Point to selected model for aux data.
   98 !      W3NOUT    Subr. W3ODATMD Set number of model for output.
   99 !      W3SETO    Subr.   Id.    Point to selected model for output.
  100 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
  101 !      STRACE    Subr.   Id.    Subroutine tracing.
  102 !      NEXTLN    Subr.   Id.    Get next line from input filw
  103 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
  104 !      STME21    Subr. W3TIMEMD Convert time to string.
  105 !      TICK21    Subr.   Id.    Advance time.
  106 !      DSEC21    Func.   Id.    Difference between times.
  107 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
  108 !      W3IOPO    Subr. W3IOPOMD Reading/writing raw point output file.
  109 !      GXEXPO    Subr. Internal Execute point output.
  110 !     ----------------------------------------------------------------
 
 
                                                          gx_outp.F90  page   3
 
 
  111 !
  112 !  5. Called by :
  113 !
  114 !     None, stand-alone program.
  115 !
  116 !  6. Error messages :
  117 !
  118 !     Checks on input, checks in W3IOxx.
  119 !     Check on grid type.
  120 !
  121 !  7. Remarks :
  122 !
  123 !     - Curvilinear grids currently not supported.
  124 !
  125 !  8. Structure :
  126 !
  127 !     See source code.
  128 !
  129 !  9. Switches :
  130 !
  131 !       !/S    Enable subroutine tracing.
  132 !
  133 ! 10. Source code :
  134 !
  135 !/ ------------------------------------------------------------------- /
  136       USE CONSTANTS
  137 !/
  138 !     USE W3GDATMD, ONLY: W3NMOD, W3SETG
  139       USE W3WDATMD, ONLY: W3SETW, W3NDAT
  140       USE W3ADATMD, ONLY: W3SETA, W3NAUX
  141       USE W3ODATMD, ONLY: W3SETO, W3NOUT
  142       USE W3IOGRMD, ONLY: W3IOGR
  143       USE W3IOPOMD, ONLY: W3IOPO
  144       USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
  145       USE W3TIMEMD, ONLY: STME21, TICK21, DSEC21
  146 !/
  147       USE W3GDATMD
  148       USE W3WDATMD, ONLY: TIME
  149       USE W3ODATMD, ONLY: NDSE, NDST, NDSO, NOPTS, PTLOC, PTNME,      &
  150                           DPO, WAO, WDO, ASO, CAO, CDO, SPCO, FNMPRE, &
  151                           GRDID, ICEO, ICEHO, ICEFO
  152 !
  153       IMPLICIT NONE
  154 !/
  155 !/ ------------------------------------------------------------------- /
  156 !/ Local parameters
  157 !/
  158       INTEGER                 :: NDSI, NDSM, NDSOP, NDSGRD, NDSPNT,   &
  159                                  NDSCGR, NDSTRC, NTRACE, IERR,        &
  160                                  IOTEST, I, TOUT(2), NOUT, TDUM(2),   &
  161                                  NREQ, IPOINT, NLEV, IOUT, TIME0(2),  &
  162                                  IH0, IM0, ID0, IID, IJ0, IINC, IK,   &
  163                                  IREQ, TIMEN(2), J
  164       REAL                    :: DTREQ, DTEST
  165       REAL                    :: UNDEFP = -99.E20
 
 
                                                          gx_outp.F90  page   4
 
 
  166       REAL                    :: FACT
  167       LOGICAL                 :: FLSRCE(7)
  168       LOGICAL, ALLOCATABLE    :: FLREQ(:)
  169       CHARACTER               :: COMSTR*1, IDTIME*23, IDDDAY*11,      &
  170                                  CINC*2
  171       CHARACTER(LEN=3)        :: MNTH(12)
  172       CHARACTER(LEN=25)       :: IDSRCE(7)
  173 !/
  174 !/ ------------------------------------------------------------------- /
  175 !/
  176       DATA IDSRCE / 'Spectrum                 ' ,                     &
  177                     'Wind-wave interactions   ' ,                     &
  178                     'Nonlinear interactions   ' ,                     &
  179                     'Dissipation              ' ,                     &
  180                     'Wave-bottom interactions ' ,                     &
  181                     'Wave-ice interactions ' ,                     &
  182                     'Sum of selected sources  ' /
  183       DATA FLSRCE / .FALSE. , .FALSE. , .FALSE. ,                     &
  184                     .FALSE. , .FALSE. , .FALSE., .FALSE. /
  185       DATA TIME0  / -1, 0 /
  186       DATA MNTH   / 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',         &
  187                     'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /
  188 !
  189 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  190 ! 1.  IO set-up.
  191 !
  192       CALL W3NMOD ( 1, 6, 6 )
  193       CALL W3SETG ( 1, 6, 6 )
  194       CALL W3NDAT (    6, 6 )
  195       CALL W3SETW ( 1, 6, 6 )
  196       CALL W3NAUX (    6, 6 )
  197       CALL W3SETA ( 1, 6, 6 )
  198       CALL W3NOUT (    6, 6 )
  199       CALL W3SETO ( 1, 6, 6 )
  200 !
  201       NDSI   = 10
  202       NDSM   = 20
  203       NDSOP  = 20
  204       NDSGRD = 30
  205       NDSPNT = 31
  206       NDSCGR = 32
  207 !
  208       NDSTRC =  6
  209       NTRACE =  0
  210 !
  211       WRITE (NDSO,900)
  212 !
  213       CALL ITRACE ( NDSTRC, NTRACE )
  214 !
  215       J      = LEN_TRIM(FNMPRE)
  216       OPEN (NDSI,FILE=FNMPRE(:J)//'gx_outp.inp',STATUS='OLD',         &
  217             ERR=800,IOSTAT=IERR)
  218       READ (NDSI,'(A)',END=801,ERR=802) COMSTR
  219       IF (COMSTR.EQ.' ') COMSTR = '$'
  220       WRITE (NDSO,901) COMSTR
 
 
                                                          gx_outp.F90  page   5
 
 
  221 !
  222       OPEN (NDSGRD,FILE=FNMPRE(:J)//'ww3.spec.grads',                 &
  223             FORM='UNFORMATTED', CONVERT='big_endian')
  224       OPEN (NDSPNT,FILE=FNMPRE(:J)//'ww3.mean.grads',FORM='FORMATTED')
  225       OPEN (NDSCGR,FILE=FNMPRE(:J)//'ww3.spec.ctl',FORM='FORMATTED')
  226 !
  227 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  228 ! 2.  Read model definition file.
  229 !
  230       CALL W3IOGR ( 'READ', NDSM )
  231       WRITE (NDSO,920) GNAME
  232       IF ( FLAGLL ) THEN
  233           FACT   = 1.
  234         ELSE
  235           FACT   = 1.E-3
  236         END IF
  237 !
  238 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  239 ! 3.  Read general data and first fields from file
  240 !
  241       CALL W3IOPO ( 'READ', NDSOP, IOTEST )
  242       ALLOCATE ( FLREQ(NOPTS) )
  243 !
  244       WRITE (NDSO,930)
  245       DO I=1, NOPTS
  246         IF ( FLAGLL ) THEN
  247             WRITE (NDSO,931) PTNME(I), FACT*PTLOC(1,I), FACT*PTLOC(2,I)
  248           ELSE
  249             WRITE (NDSO,932) PTNME(I), FACT*PTLOC(1,I), FACT*PTLOC(2,I)
  250           END IF
  251         END DO
  252 !
  253 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  254 ! 4.  Read requests from input file.
  255 !     Output times
  256 !
  257       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  258       READ (NDSI,*,END=801,ERR=802) TOUT, DTREQ, NOUT
  259       DTREQ  = MAX ( 0. , DTREQ )
  260       IF ( DTREQ.EQ.0 ) NOUT = 1
  261       NOUT   = MAX ( 1 , NOUT )
  262 !
  263       CALL STME21 ( TOUT , IDTIME )
  264       WRITE (NDSO,940) IDTIME
  265 !
  266       TDUM = 0
  267       CALL TICK21 ( TDUM , DTREQ )
  268       CALL STME21 ( TDUM , IDTIME )
  269       IF ( DTREQ .GE. 86400. ) THEN
  270           WRITE (IDDDAY,'(I10,1X)') INT(DTREQ/86400.)
  271         ELSE
  272           IDDDAY = '           '
  273         END IF
  274       IDTIME(1:11) = IDDDAY
  275       IDTIME(21:23) = '   '
 
 
                                                          gx_outp.F90  page   6
 
 
  276       WRITE (NDSO,941) IDTIME, NOUT
  277 !
  278 ! ... Output points
  279 !
  280       FLREQ = .FALSE.
  281       NREQ   = 0
  282 !
  283       DO
  284         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  285         READ (NDSI,*,END=801,ERR=802) IPOINT
  286         IF ( IPOINT .GT. 0 ) THEN
  287             IF ( IPOINT .LE. NOPTS ) THEN
  288                 IF ( .NOT. FLREQ(IPOINT) ) NREQ   = NREQ + 1
  289                 FLREQ(IPOINT) = .TRUE.
  290               END IF
  291           ELSE
  292             EXIT
  293           END IF
  294         END DO
  295 !
  296 ! ... Output of output points
  297 !
  298       WRITE (NDSO,950) NREQ
  299       DO I=1, NOPTS
  300         IF (FLREQ(I)) THEN
  301             IF ( FLAGLL ) THEN
  302                 WRITE (NDSO,951) PTNME(I), FACT*PTLOC(1,I), &
  303                                            FACT*PTLOC(2,I)
  304               ELSE
  305                 WRITE (NDSO,956) PTNME(I), FACT*PTLOC(1,I), &
  306                                            FACT*PTLOC(2,I)
  307               END IF
  308           END IF
  309         END DO
  310 !
  311 ! ... Output of output points
  312 !
  313       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  314       READ (NDSI,*,END=801,ERR=802) FLSRCE
  315       WRITE (NDSO,952)
  316       NLEV   = 0
  317       DO I=1, 7
  318         IF ( FLSRCE(I) ) THEN
  319             WRITE (NDST,953) IDSRCE(I)
  320             NLEV   = NLEV + 1
  321           END IF
  322         END DO
  323 !
  324       WRITE (NDSO,955)
  325 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  326 ! 5.  Time management.
  327 !
  328       IOUT   = 0
  329 !
  330       DO
 
 
                                                          gx_outp.F90  page   7
 
 
  331         DTEST  = DSEC21 ( TIME , TOUT )
  332         IF ( DTEST .GT. 0. ) THEN
  333             CALL W3IOPO ( 'READ', NDSOP, IOTEST )
  334             IF ( IOTEST .EQ. -1 ) THEN
  335                 WRITE (NDSO,998)
  336                 EXIT
  337               END IF
  338             CYCLE
  339           END IF
  340         IF ( DTEST .LT. 0. ) THEN
  341             CALL TICK21 ( TOUT , DTREQ )
  342             CYCLE
  343           END IF
  344 !
  345         IOUT   = IOUT + 1
  346         CALL STME21 ( TOUT , IDTIME )
  347 !
  348         CALL GXEXPO
  349         TIMEN  = TOUT
  350 !
  351         IF ( TIME0(1) .EQ. -1 ) TIME0 = TIME
  352 !
  353         CALL TICK21 ( TOUT , DTREQ )
  354         IF ( IOUT .GE. NOUT ) EXIT
  355         END DO
  356 !
  357 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  358 ! 6.  Close data file and write control file
  359 ! 6.a Close data sets
  360 !
  361       WRITE (NDSO,960)
  362 !
  363       WRITE (NDSO,961)
  364       CLOSE (NDSGRD)
  365       CLOSE (NDSPNT)
  366 !
  367       WRITE (NDSO,962)
  368 !
  369 ! 6.b Set up timing info
  370 !
  371       IH0    = TIME0(2)/10000
  372       IM0    = MOD(TIME0(2)/100,100)
  373       ID0    = MOD(TIME0(1),100)
  374       IID    = MOD(TIME0(1)/100,100)
  375       IJ0    = TIME0(1)/10000
  376 !
  377       IF ( IOUT .GT. 1 ) DTREQ  = DSEC21 ( TIME0, TIMEN ) / REAL(IOUT-1)
  378       IF ( IOUT .EQ. 1 ) DTREQ  = 3600.
  379       IF ( DTREQ .GT. 3599. ) THEN
  380           CINC   = 'HR'
  381           IINC   = NINT(DTREQ/3600.)
  382           IF ( MOD(NINT(DTREQ),3600) .NE. 0 ) GOTO 820
  383         ELSE
  384           CINC   = 'MN'
  385           IINC   = NINT(DTREQ/60.)
 
 
                                                          gx_outp.F90  page   8
 
 
  386         END IF
  387 !
  388       WRITE (NDSO,963) IOUT, IH0, IM0, ID0, MNTH(IID), IJ0, IINC, CINC
  389 !
  390 ! 6.c Write control file for spectral data
  391 !
  392       WRITE (NDSO,964)
  393 !
  394       WRITE (NDSCGR,970) UNDEFP, NTH, 90.+TH(1)*RADE, DTH*RADE,       &
  395              NK, (90.-TPIINV*SIG(IK),IK=NK,MAX(1,NK-4),-1)
  396       WRITE (NDSCGR,971) (90.-TPIINV*SIG(IK),IK=NK-5,1,-1)
  397       WRITE (NDSCGR,972) NLEV, 1., 1.,                                &
  398                          IOUT, IH0, IM0, ID0, MNTH(IID), IJ0,         &
  399                          IINC, CINC, NREQ
  400 !
  401       IREQ    = 0
  402       DO I=1, NOPTS
  403         IF ( FLREQ(I) ) THEN
  404             IREQ   = IREQ + 1
  405             WRITE (NDSCGR,973) IREQ, NLEV, 99, PTNME(I)
  406           END IF
  407         END DO
  408 !
  409       WRITE (NDSCGR,974)
  410 !
  411       GOTO 888
  412 !
  413 ! Escape locations read errors :
  414 !
  415   800 CONTINUE
  416       WRITE (NDSE,1000) IERR
  417       CALL EXTCDE ( 10 )
  418 !
  419   801 CONTINUE
  420       WRITE (NDSE,1001)
  421       CALL EXTCDE ( 11 )
  422 !
  423   802 CONTINUE
  424       WRITE (NDSE,1002) IERR
  425       CALL EXTCDE ( 12 )
  426 !
  427   820 CONTINUE
  428       WRITE (NDSE,1020) DTREQ
  429       CALL EXTCDE ( 20 )
  430 !
  431   821 CONTINUE
  432       WRITE (NDSE,1021)
  433       CALL EXTCDE ( 21 )
  434 !
  435   888 CONTINUE
  436 !
  437       WRITE (NDSO,999)
  438 !
  439 ! Formats
  440 !
 
 
                                                          gx_outp.F90  page   9
 
 
  441   900 FORMAT (/12X,'    *** WAVEWATCH III GrADS point output post.***    >
  442                12X,'=====================================================>
  443   901 FORMAT ( '  Comment character is ''',A,''''/)
  444 !
  445   920 FORMAT ( '  Grid name : ',A/)
  446 !
  447   930 FORMAT ( '  Points in file : '/                                 &
  448                ' ------------------------------------')
  449                                                        
  450   931 FORMAT ( '      ',A,2F10.2)
  451                                  
  452   932 FORMAT ( '      ',A,2(F8.1,'E3'))
  453 !
  454   940 FORMAT (/'  Output time data : '/                               &
  455                ' --------------------------------------------------'/ &
  456                '      First time         : ',A)
  457   941 FORMAT ( '      Interval           : ',A/                       &
  458                '      Number of requests : ',I4)
  459 !
  460   950 FORMAT (/'  Requested output for',I3,' points : '/              &
  461                ' --------------------------------------------------')
  462                                                                      
  463   951 FORMAT ( '      ',A,2F10.2)
  464                                  
  465   956 FORMAT ( '      ',A,2(F8.1,'E3'))
  466                                        
  467   952 FORMAT (/'  Requested output fields :'/                         &
  468                ' --------------------------------------------------')
  469   953 FORMAT ( '      ',A)
  470   955 FORMAT (/'  Output times :'/                                    &
  471                ' --------------------------------------------------')
  472 !
  473   960 FORMAT (//'  Final file management '/                           &
  474                ' -----------------------------------------------------')
  475   961 FORMAT ( '      Closing file ww3.spec.grads'/                   &
  476                '      Closing file ww3.mean.grads')
  477   962 FORMAT ( '      Preparing control files :')
  478   963 FORMAT ( '         Number of times : ',I6/                      &
  479            '         Initial time ID : ',I2.2,':',I2.2,'Z',I2.2,A3,I4/ &
  480            '         Time step ID    : ',I2,A2)
  481   964 FORMAT ( '      Writing ww3.spec.ctl'/)
  482 !
  483   970 FORMAT ('DSET      ww3.spec.grads'/                             &
  484               'TITLE     WAVEWATCH III spectra and source terms'/     &
  485               'OPTIONS   sequential'/                                 &
  486               'OPTIONS   big_endian'/                                 &
  487               'UNDEF    ',E10.2/                                      &
  488               'XDEF     ',I4,'  LINEAR ',2F8.2/                       &
  489               'YDEF     ',I4,'  LEVELS ',5F8.4)
  490   971 FORMAT (22X,5F8.4)
  491   972 FORMAT ('ZDEF     ',I4,'  LINEAR ',2F8.2/                       &
  492               'TDEF     ',I4,'  LINEAR ',I6.2,':',I2.2,'Z',I2.2,A3,I4, &
  493                2x,I2,A2/                                              &
  494               'VARS     ',I4)
  495   973 FORMAT ('LOC',I3.3,2I4,2X,A)
 
 
                                                          gx_outp.F90  page  10
 
 
  496   974 FORMAT ('ENDVARS')
  497 !
  498   998 FORMAT (/'      End of file reached '/)
  499 !
  500   999 FORMAT (/'  End of program '/                                   &
  501                ' ========================================='/          &
  502                '         WAVEWATCH III GrADS point output '/)
  503 !
  504  1000 FORMAT (/' *** WAVEWATCH III ERROR IN GXOUTP : '/               &
  505                '     ERROR IN OPENING INPUT FILE'/                    &
  506                '     IOSTAT =',I5/)
  507 !
  508  1001 FORMAT (/' *** WAVEWATCH III ERROR IN GXOUTP : '/               &
  509                '     PREMATURE END OF INPUT FILE'/)
  510 !
  511  1002 FORMAT (/' *** WAVEWATCH III ERROR IN GXOUTP : '/               &
  512                '     ERROR IN READING FROM INPUT FILE'/               &
  513                '     IOSTAT =',I5/)
  514 !
  515  1020 FORMAT (/' *** WAVEWATCH III ERROR IN GXOUTF : '/               &
  516                '     FIELD INCREMENT > 1HR BUT NOT MULTIPLE',F10.0/)
  517 !
  518  1021 FORMAT (/' *** WAVEWATCH III ERROR IN GXOUTF : '/               &
  519                '     UPDATE PARS IN LOOP 610 !!!'/)
  520 !/
  521 !/ Internal subroutine GXEXPO ---------------------------------------- /
  522 !/
  523       CONTAINS
  524 !/ ------------------------------------------------------------------- /
  525       SUBROUTINE GXEXPO
  526 !/
  527 !/                  +-----------------------------------+
  528 !/                  | WAVEWATCH III           NOAA/NCEP |
  529 !/                  |           H. L. Tolman            |
  530 !/                  |                        FORTRAN 90 |
  531 !/                  | Last update :         16-Jul-2012 |
  532 !/                  +-----------------------------------+
  533 !/
  534 !/    30-Jun-1999 : Final FORTRAN 77                    ( version 1.18 )
  535 !/    24-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
  536 !/                  Massive changes to logistics
  537 !/    25-Jan-2001 : Cartesian grid version              ( version 2.06 )
  538 !/    02-Feb-2001 : Xnl version 5                       ( version 2.07 )
  539 !/    01-Aug-2003 : Fix format for SH output points.    ( version 3.03 )
  540 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
  541 !/    23-Jun-2006 : Linear input added.                 ( version 3.09 )
  542 !/    03-Jul-2006 : Separate flux modules.              ( version 3.09 )
  543 !/    25-Jul-2006 : Grid ID for each point.             ( version 3.10 )
  544 !/    25-Apr-2007 : EMEAN in W3SPR2 par list.           ( version 3.11 )
  545 !/    09-Oct-2007 : WAM 4+ Sin and Sds added.           ( version 3.13 )
  546 !/                  (F. Ardhuin)
  547 !/    09-Oct-2007 : Experimental Sbs (BS1) added.       ( version 3.13 )
  548 !/                  (F. Ardhuin)
  549 !/    16-Jul-2012 : Move GMD (SNL3) and nonlinear filter (SNLS)
  550 !/                  from 3.15 (HLT).                    ( version 4.08 )
 
 
                                                          gx_outp.F90  page  11
 
 
  551 !/    18-Aug-2018 : S_{ice} IC5 (Q. Liu)                ( version 6.06 )
  552 !/
  553 !  1. Purpose :
  554 !
  555 !     Perform actual point output.
  556 !
  557 !  3. Parameters :
  558 !
  559 !  4. Subroutines used :
  560 !
  561 !      Name      Type  Module   Description
  562 !     ----------------------------------------------------------------
  563 !      W3SPRn    Subr. W3SRCnMD Mean wave parameters for use in
  564 !                               source terms.
  565 !      W3FLXn    Subr. W3FLXnMD Flux/stress computation.
  566 !      W3SLNn    Subr. W3SLNnMD Linear input.
  567 !      W3SINn    Subr. W3SRCnMD Input source term.
  568 !      W3SDSn    Subr. W3SRCnMD Whitecapping source term
  569 !      W3SNLn    Subr. W3SNLnMD Nonlinear interactions.
  570 !      W3SBTn    Subr. W3SBTnMD Bottom friction source term.
  571 !      W3SDBn    Subr. W3SBTnMD Depth induced breaking source term.
  572 !      W3STRn    Subr. W3STRnMD Triad interaction source term.
  573 !      W3SBSn    Subr. W3SBSnMD Bottom scattering source term.
  574 !      W3SXXn    Subr. W3SXXnMD Unclassified source term.
  575 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  576 !      STME21    Subr. W3TIMEMD Convert time to string.
  577 !     ----------------------------------------------------------------
  578 !
  579 !  5. Called by :
  580 !
  581 !     Program in which it is contained.
  582 !
  583 !  6. Error messages :
  584 !
  585 !     None.
  586 !
  587 !  7. Remarks :
  588 !
  589 !     - Spectra are relative frequency energy spectra.
  590 !     - Note that arrays CX and CY of the main program now contain
  591 !       the absolute current speed and direction respectively.
  592 !
  593 !  8. Structure :
  594 !
  595 !     See source code.
  596 !
  597 !  9. Switches :
  598 !
  599 !       !/S      Enable subroutine tracing.
  600 !       !/T      Enable test output.
  601 !
  602 !       !/FLXx   Flux/stress computation.
  603 !       !/LNx    Linear input package
  604 !       !/STx    Source term package
  605 !       !/NLx    Nonlinear interaction package
 
 
                                                          gx_outp.F90  page  12
 
 
  606 !       !/BTx    Bottom friction package
  607 !       !/ICx    Ice source term package
  608 !       !/DBx    Depth-induced breaking package
  609 !       !/TRx    Triad interaction package
  610 !       !/BSx    Bottom scattering package
  611 !       !/XXx    Arbitrary adittional source term package
  612 !
  613 !       !/STAB2  Stability correction for !/ST2
  614 !
  615 ! 10. Source code :
  616 !
  617 !/ ------------------------------------------------------------------- /
  618       USE W3SLN1MD
  619       USE W3SRC4MD, ONLY : W3SPR4, W3SIN4, W3SDS4
  620       USE W3SNL1MD
  621 !/
  622       USE W3DISPMD, ONLY: LIU_FORWARD_DISPERSION, NAR1D, DFAC, N1MAX, &
  623                           ECG1, EWN1, DSIE
  624 !
  625       IMPLICIT NONE
  626 !/
  627 !/ ------------------------------------------------------------------- /
  628 !/ Local parameters
  629 !/
  630       INTEGER                 :: J, I1, I2, IK, ITH, ISPEC, IKM, IKL, &
  631                                  IKH, ITT, IX, IY, ISEA
  632       REAL                    :: XL, XH, XL2, XH2, DEPTH, SQRTH, UDIR,&
  633                                  UDIRR, UABS, CDIR, SIX, R1, R2, ET,  &
  634                                  EWN, ETR, ETX, ETY, EBND, EBX, EBY,  &
  635                                  HSIG, WLEN, TMEAN, THMEAN, THSPRD,   &
  636                                  EMAX, EL, EH, DENOM, FP, THP, SPP,   &
  637                                  FACTOR, CD, USTAR, FHIGH, ZWND, ICE, &
  638                                  USTD, Z0, CHARN, EMEAN, FMEAN, WNMEAN,&
  639                                  ICETHICK, ICECON, ICEF
  640       REAL                    :: FMEANWS, TAUWX, TAUWY, AMAX, &
  641                                  TAUWNX, TAUWNY,  FMEAN1, WHITECAP(1:4), >
  642       REAL                    :: HSMIN = 0.05
  643       REAL                    :: WN(NK), CG(NK), E(NK,NTH), E1(NK),   &
  644                                  APM(NK), THBND(NK), SPBND(NK),       &
  645                                  A(NTH,NK), WN2(NTH,NK),WN_R(NK),     &
  646                                  ALPHA_LIU(NK), CG_ICE(NK), R(NK)
  647       REAL                    :: DIA(NTH,NK), SWI(NK,NTH), SNL(NK,NTH),&
  648                                  SDS(NK,NTH), SBT(NK,NTH), SIS(NK,NTH),&
  649                                  STT(NK,NTH), DIA2(NK,NTH)
  650       REAL                    :: XLN(NTH,NK), XWI(NTH,NK), XNL(NTH,NK),&
  651                                  XTR(NTH,NK), XDS(NTH,NK), XDB(NTH,NK),&
  652                                  XBT(NTH,NK), XBS(NTH,NK), XXX(NTH,NK),&
  653                                  XWL(NTH,NK), XIS(NTH,NK)
  654       LOGICAL                 :: LBREAK
  655       LOGICAL                :: LLWS(NTH,NK)
  656       REAL                   :: LAMBDA(NSPEC)
  657       CHARACTER               :: DTME21*23
  658 !/
  659 !/ ------------------------------------------------------------------- /
  660 !/
 
 
                                                          gx_outp.F90  page  13
 
 
  661 !
  662       XL     = 1./XFR - 1.
  663       XH     =  XFR - 1.
  664       XL2    = XL**2
  665       XH2    = XH**2
  666       ICE = 0.
  667 !
  668       XLN = 0.
  669       XWI = 0.
  670       XNL = 0.
  671       XTR = 0.
  672       XDS = 0.
  673       XDB = 0.
  674       XBT = 0.
  675       XBS = 0.
  676       XWL = 0.
  677       XIS = 0.
  678       XXX = 0.
  679 !
  680 !     Output of time
  681 !
  682       CALL STME21 ( TIME , DTME21 )
  683       WRITE (NDSO,905) DTME21
  684 !
  685 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  686 !     Loop over output points.
  687 !
  688       DO J=1, NOPTS
  689         IF ( FLREQ(J) ) THEN
  690 !
  691 ! 2. Calculate grid parameters using and inlined version of WAVNU1.
  692 !
  693           DEPTH  = MAX ( DMIN, DPO(J) )
  694           SQRTH  = SQRT ( DEPTH )
  695           UDIR   = MOD ( 270. - WDO(J)*RADE , 360. )
  696           UDIRR  = WDO(J)
  697           UABS   = MAX ( 0.001 , WAO(J) )
  698           CDIR   = MOD ( 270. - CDO(J)*RADE , 360. )
  699                 ICETHICK = MAX (0., ICEHO(J))
  700                 ICECON = MAX (0., ICEO(J))
  701 !
  702             DO IK=1, NK
  703               SIX    = SIG(IK) * SQRTH
  704               I1     = INT(SIX/DSIE)
  705               IF (I1.LE.N1MAX) THEN
  706                   I2 = I1 + 1
  707                   R1 = SIX/DSIE - REAL(I1)
  708                   R2 = 1. - R1
  709                   WN(IK) = ( R2*EWN1(I1) + R1*EWN1(I2) ) / DEPTH
  710                   CG(IK) = ( R2*ECG1(I1) + R1*ECG1(I2) ) * SQRTH
  711                 ELSE
  712                   WN(IK) = SIG(IK)*SIG(IK)/GRAV
  713                   CG(IK) = 0.5 * GRAV / SIG(IK)
  714                 END IF
  715 !
 
 
                                                          gx_outp.F90  page  14
 
 
  716               END DO
  717                     
  718             IF (IICEDISP) THEN
  719                 CALL LIU_FORWARD_DISPERSION  (ICETHICK,0.,DEPTH, &
  720                                               SIG,WN_R,CG_ICE,ALPHA_LIU)
  721               ELSE
  722                 WN_R=WN
  723                 CG_ICE=CG
  724               END IF
  725             R(:)=1 ! In case IC2 is defined but not IS2
  726                                                        
  727 !
  728 ! 3.  Prepare spectra etc.
  729 ! 3.a Mean wave parameters.
  730 !
  731             ET     = 0.
  732             EWN    = 0.
  733             ETR    = 0.
  734             ETX    = 0.
  735             ETY    = 0.
  736             DO IK=1, NK
  737               EBND   = 0.
  738               EBX    = 0.
  739               EBY    = 0.
  740               DO ITH=1, NTH
  741                 ISPEC  = ITH + (IK-1)*NTH
  742                 E(IK,ITH) = SPCO(ISPEC,J)
  743                 EBND   = EBND + SPCO(ISPEC,J)
  744                 EBX    = EBX  + SPCO(ISPEC,J)*ECOS(ITH)
  745                 EBY    = EBY  + SPCO(ISPEC,J)*ESIN(ITH)
  746                 END DO
  747               E1(IK) = EBND * DTH
  748               APM(IK)= E1(IK) / ( TPI * GRAV**2 / SIG(IK)**5  )
  749               IF ( E1(IK) .GT. 1.E-5) THEN
  750                   THBND(IK) = MOD(630.- RADE*ATAN2(EBY,EBX),360.)
  751                   SPBND(IK) = RADE * SQRT ( MAX ( 0. , 2.*( 1. -      &
  752                     SQRT( MAX(0.,(EBX**2+EBY**2)/EBND**2) ) ) ) )
  753                 ELSE
  754                   THBND(IK) = -999.9
  755                   SPBND(IK) = -999.9
  756                 END IF
  757               EBND   = E1(IK) * DSII(IK) * TPIINV
  758               ET     = ET  + EBND
  759               EWN    = EWN + EBND / WN(IK)
  760               ETR    = ETR + EBND / SIG(IK)
  761               ETX    = ETX + EBX * DSII(IK)
  762               ETY    = ETY + EBY * DSII(IK)
  763               END DO
  764 !
  765 ! tail factors for radian action etc ...!
  766 !
  767             EBND   = E1(NK) * TPIINV / ( SIG(NK) * DTH )
  768             ET     = ET  + FTE *EBND
  769             EWN    = EWN + FTWL*EBND
  770             ETR    = ETR + FTTR*EBND
 
 
                                                          gx_outp.F90  page  15
 
 
  771             ETX    = DTH*ETX*TPIINV + FTE*EBX*TPIINV/SIG(NK)
  772             ETY    = DTH*ETY*TPIINV + FTE*EBY*TPIINV/SIG(NK)
  773 !
  774             HSIG   = 4. * SQRT ( ET )
  775             IF ( HSIG .GT. HSMIN ) THEN
  776                 WLEN   = EWN / ET * TPI
  777                 TMEAN  = ETR / ET * TPI
  778                 THMEAN = MOD ( 630. - RADE*ATAN2(ETY,ETX) , 360. )
  779                 THSPRD = RADE * SQRT ( MAX ( 0. , 2.*( 1. - SQRT(     &
  780                            MAX(0.,(ETX**2+ETY**2)/ET**2) ) ) ) )
  781               ELSE
  782                 WLEN   = 0.
  783                 TMEAN  = 0.
  784                 THMEAN = 0.
  785                 THSPRD = 0.
  786                 DO IK=1, NK
  787                   E1(IK) = 0.
  788                   DO ITH=1, NTH
  789                     E(IK,ITH) = 0.
  790                     END DO
  791                   END DO
  792               END IF
  793 !
  794 ! peak frequency
  795 !
  796             EMAX   = E1(NK)
  797             IKM    = NK
  798 !
  799             DO IK=NK-1, 1, -1
  800               IF ( E1(IK) .GT. EMAX ) THEN
  801                   EMAX   = E1(IK)
  802                   IKM    = IK
  803                 END IF
  804               END DO
  805 !
  806             IKL    = MAX (  1 , IKM-1 )
  807             IKH    = MIN ( NK , IKM+1 )
  808             EL     = E1(IKL) - E1(IKM)
  809             EH     = E1(IKH) - E1(IKM)
  810             DENOM  = XL*EH - XH*EL
  811 !
  812             IF ( HSIG .GE. HSMIN ) THEN
  813                 FP     = SIG(IKM) * ( 1. + 0.5 * ( XL2*EH - XH2*EL )  &
  814                             / SIGN ( MAX(ABS(DENOM),1.E-15) , DENOM ) )
  815                 THP    = THBND(IKM)
  816                 SPP    = SPBND(IKM)
  817               ELSE
  818                 FP     = 0.
  819                 THP    = 0.
  820                 SPP    = 0.
  821               END IF
  822 !
  823 ! 3.4 source terms
  824 !
  825             DO IK=1, NK
 
 
                                                          gx_outp.F90  page  16
 
 
  826               FACTOR = TPIINV * CG(IK) / SIG(IK)
  827               DO ITH=1, NTH
  828                 ISPEC  = ITH + (IK-1)*NTH
  829                 A(ITH,IK)   = FACTOR * SPCO(ISPEC,J)
  830                 WN2(ITH,IK) = WN(IK)
  831                 END DO
  832               END DO
  833 !
  834                  USTAR  = 1.
  835             ZWND   = ZZWND
  836             TAUWX  = 0.
  837             TAUWY  = 0.
  838 !
  839             CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN,   FMEAN1,        &
  840                          WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD, &
  841                          TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, DLWM>
  842 !
  843             DO ITT=1, 3
  844               CALL W3SIN4 (A, CG, WN2, UABS, USTAR, DAIR/DWAT,   &
  845                            ASO(J), UDIRR, Z0, CD, TAUWX, TAUWY,  &
  846                            TAUWNX, TAUWNY, XWI, DIA, LLWS, IX, IY, LAMBDA>
  847               CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN,  FMEAN1,         &
  848                          WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD, &
  849                          TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, DLWM>
  850               END DO
  851 !
  852             IF ( FLSRCE(2) ) THEN
  853                 CALL W3SLN1 ( WN, FHIGH, USTAR, UDIRR, XLN )
  854 !
  855                 CALL W3SIN4 (A, CG, WN2, UABS, USTAR, DAIR/DWAT, &
  856                              ASO(J), UDIRR, Z0, CD,              &
  857                              TAUWX, TAUWY, TAUWNX, TAUWNY,       &
  858                              XWI, DIA,  LLWS, IX, IY, LAMBDA )
  859               END IF
  860             IF ( FLSRCE(3) ) THEN
  861                 CALL W3SNL1 ( A, CG, WNMEAN*DEPTH,      XNL, DIA )
  862 !!/NLS                CALL W3SNLS ( A, CG, WN, DEPTH, UABS, 900.,      &
  863 !!/NLS                                                 SNL=XNL, AA=DIA )
  864 !
  865               END IF
  866             IF ( FLSRCE(4) ) THEN
  867                 CALL W3SDS4 ( A, WN, CG,          &
  868                                         USTAR, USTD, DEPTH, XDS, DIA, IX,>
  869 !
  870               END IF
  871             IF ( FLSRCE(5) ) THEN
  872                                  
  873                                  
  874                                  
  875                                  
  876 !
  877  
  878  
  879 !
  880               END IF
 
 
                                                          gx_outp.F90  page  17
 
 
  881                     
  882            IF ( FLSRCE(6) ) THEN
  883                                 
  884            END IF
  885 !
  886             DO IK=1, NK
  887               FACTOR = TPI / CG(IK) * SIG(IK)
  888               DO ITH=1, NTH
  889                 ISPEC       = ITH + (IK-1)*NTH
  890                 E  (IK,ITH) = SPCO(ISPEC,J)
  891                 SWI(IK,ITH) = ( XWI(ITH,IK) + XLN(ITH,IK) ) * FACTOR
  892                 SNL(IK,ITH) = ( XNL(ITH,IK) + XTR(ITH,IK) ) * FACTOR
  893                 SDS(IK,ITH) = ( XDS(ITH,IK) + XDB(ITH,IK) ) * FACTOR
  894                 SBT(IK,ITH) = ( XBT(ITH,IK) + XBS(ITH,IK) ) * FACTOR
  895                 SIS(IK,ITH) = XIS(ITH,IK) * FACTOR
  896                 STT(IK,ITH) = XXX(ITH,IK) * FACTOR
  897                 END DO
  898               END DO
  899             STT    = STT + SWI + SNL + SDS + SBT + SIS
  900                                                       
  901 !
  902 ! 4.a Perform output
  903 !
  904             IF ( FLSRCE(1) ) WRITE (NDSGRD)                           &
  905                ((E  (IK,ITH),ITH=1,NTH),IK=NK,1,-1)
  906             IF ( FLSRCE(2) ) WRITE (NDSGRD)                           &
  907                ((SWI(IK,ITH),ITH=1,NTH),IK=NK,1,-1)
  908             IF ( FLSRCE(3) ) WRITE (NDSGRD)                           &
  909                ((SNL(IK,ITH),ITH=1,NTH),IK=NK,1,-1)
  910             IF ( FLSRCE(4) ) WRITE (NDSGRD)                           &
  911                ((SDS(IK,ITH),ITH=1,NTH),IK=NK,1,-1)
  912             IF ( FLSRCE(5) ) WRITE (NDSGRD)                           &
  913                ((SBT(IK,ITH),ITH=1,NTH),IK=NK,1,-1)
  914             IF ( FLSRCE(6) ) WRITE (NDSGRD)                           &
  915                ((SIS(IK,ITH),ITH=1,NTH),IK=NK,1,-1)
  916             IF ( FLSRCE(7) ) WRITE (NDSGRD)                           &
  917                ((STT(IK,ITH),ITH=1,NTH),IK=NK,1,-1)
  918 !
  919             IF ( FLAGLL ) THEN
  920                 WRITE (NDSPNT,940) PTNME(J),                          &
  921                    FACT*PTLOC(1,J), FACT*PTLOC(2,J), DPO(J), WAO(J),  &
  922                    WAO(J)*COS(WDO(J)), WAO(J)*SIN(WDO(J)), ASO(J),    &
  923                    CAO(J), CAO(J)*COS(CDO(J)), CAO(J)*SIN(CDO(J)),    &
  924                    HSIG, GRDID(J)
  925               ELSE
  926                 WRITE (NDSPNT,941) PTNME(J),                          &
  927                    FACT*PTLOC(1,J), FACT*PTLOC(2,J), DPO(J), WAO(J),  &
  928                    WAO(J)*COS(WDO(J)), WAO(J)*SIN(WDO(J)), ASO(J),    &
  929                    CAO(J), CAO(J)*COS(CDO(J)), CAO(J)*SIN(CDO(J)),    &
  930                    HSIG, GRDID(J)
  931               END IF
  932 !
  933 ! ... End of points loop
  934 !
  935           END IF
 
 
                                                          gx_outp.F90  page  18
 
 
  936         END DO
  937 !
  938       RETURN
  939 !
  940 ! Formats
  941 !
  942   905 FORMAT (9X,A)
  943                    
  944   940 FORMAT (A10,1X,2F6.1,f7.1,3F7.1,F8.2,3F7.2,F6.2,2X,A)
  945                                                            
  946   941 FORMAT (A10,1X,2F8.1,f7.1,3F7.1,F8.2,3F7.2,F6.2,2X,A)
  947                                                            
  948 !
  949 !/
  950 !/ End of GXEXPO ----------------------------------------------------- /
  951 !/
  952       END SUBROUTINE GXEXPO
  953 !/
  954 !/ End of GXOUTP ----------------------------------------------------- /
  955 !/
  956       END PROGRAM GXOUTP
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
