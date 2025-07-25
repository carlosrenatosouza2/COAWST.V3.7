                                                          gx_outf.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM GXOUTF
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |           A. Chawla               |
    9 !/                  |           J.H.G.M. Alves          |
   10 !/                  |                        FORTRAN 90 |
   11 !/                  | Last update :         27-Aug-2015 |
   12 !/                  +-----------------------------------+
   13 !/
   14 !/    30-Jun-1999 : Final FORTRAN 77                    ( version 1.18 )
   15 !/    24-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
   16 !/    25-Jan-2001 : Cartesian grid version              ( version 2.06 )
   17 !/    29-Jan-2001 : Add output fields 17-18             ( version 2.20 )
   18 !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
   19 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
   20 !/    27-Jun-2005 : Adding MAPST2.                      ( version 3.07 )
   21 !/    21-Jul-2005 : Add output fields 19-21.            ( version 3.07 )
   22 !/    15-Dec-2005 : Updating MAPST2 for 2-way nest.     ( version 3.08 )
   23 !/    13-Mar-2006 : MSOUT and MBOUT added.              ( version 3.09 )
   24 !/    29-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   25 !/    05-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
   26 !/    18-Jan-2007 : Update MSOUT/MBOUT treatment.       ( version 3.10 )
   27 !/    28-Mar-2007 : Adding partitioned output.          ( version 3.11 )
   28 !/                  Adding user slots for outputs.
   29 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   30 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   31 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   32 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
   33 !/                  specify index closure for a grid.   ( version 3.14 )
   34 !/                  (T. J. Campbell, NRL)
   35 !/    05-Oct-2011 : Updating to the 53 output parameter ( version 4.05 )
   36 !/                  (J-H Alves)
   37 !/    25-Feb-2013 : Adding double-index output fields   ( version 4.11 )
   38 !/    27-Aug-2015 : ICEH and ICEF added as output       ( version 5.10 )
   39 !/    25-Aug-2018 : Add WBT parameter                   ( version 6.06 )
   40 !/
   41 !/
   42 !/    Copyright 2009 National Weather Service (NWS),
   43 !/       National Oceanic and Atmospheric Administration.  All rights
   44 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   45 !/       No unauthorized use without permission.
   46 !/
   47 !  1. Purpose :
   48 !
   49 !     Generate GrADS input files from raw WAVEWATCH data file.
   50 !
   51 !  2. Method :
   52 !
   53 !     Data is read from the grid output file out_grd.ww3 (raw data)
   54 !     and from the file gx_outf.inp ( NDSI, output requests ).
   55 !     Model definition and raw data files are read using WAVEWATCH III
 
 
                                                          gx_outf.F90  page   2
 
 
   56 !     subroutines.
   57 !
   58 !     Output files are ww3.ctl and ww3.grads. the output files
   59 !     contains a land-sea map, followed by requested fields. See the
   60 !     control file for the names of the fields.
   61 !
   62 !  3. Parameters :
   63 !
   64 !  4. Subroutines used :
   65 !
   66 !      Name      Type  Module   Description
   67 !     ----------------------------------------------------------------
   68 !      W3NMOD    Subr. W3GDATMD Set number of model.
   69 !      W3SETG    Subr.   Id.    Point to selected model.
   70 !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
   71 !      W3SETW    Subr.   Id.    Point to selected model for wave data.
   72 !      W3NAUX    Subr. W3ADATMD Set number of model for aux data.
   73 !      W3SETA    Subr.   Id.    Point to selected model for aux data.
   74 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
   75 !      STRACE    Subr.   Id.    Subroutine tracing.
   76 !      NEXTLN    Subr.   Id.    Get next line from input filw
   77 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
   78 !      STME21    Subr. W3TIMEMD Convert time to string.
   79 !      TICK21    Subr.   Id.    Advance time.
   80 !      DSEC21    Func.   Id.    Difference between times.
   81 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
   82 !      W3IOGO    Subr. W3IOGOMD Reading/writing raw gridded data file.
   83 !      W3READFLGRD Subr. W3IOGOMD Reading output fields flags.
   84 !      W3EXGO    Subr. Internal Execute grid output.
   85 !     ----------------------------------------------------------------
   86 !
   87 !  5. Called by :
   88 !
   89 !     None, stand-alone program.
   90 !
   91 !  6. Error messages :
   92 !
   93 !     Checks on input, checks in W3IOxx.
   94 !     Check on grid type.
   95 !
   96 !  7. Remarks :
   97 !
   98 !     - For the Cartesian grid version the X and Y increment are
   99 !       artificially converted to long-lat by assuming the 1 degree
  100 !       equals 100 km.
  101 !     - Curvilinear grids currently not supported.
  102 !
  103 !  8. Structure :
  104 !
  105 !     See source code.
  106 !
  107 !  9. Switches :
  108 !
  109 !        !/S     Enable subroutine tracing.
  110 !
 
 
                                                          gx_outf.F90  page   3
 
 
  111 ! 10. Source code :
  112 !
  113 !/ ------------------------------------------------------------------- /
  114       USE CONSTANTS
  115 !/
  116 !     USE W3GDATMD, ONLY: W3NMOD, W3SETG
  117       USE W3WDATMD, ONLY: W3NDAT, W3SETW
  118       USE W3ADATMD, ONLY: W3NAUX, W3SETA
  119       USE W3ODATMD, ONLY: W3NOUT, W3SETO
  120       USE W3IOGRMD, ONLY: W3IOGR
  121       USE W3IOGOMD, ONLY: W3READFLGRD, W3IOGO
  122       USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
  123       USE W3TIMEMD, ONLY: STME21, TICK21, DSEC21
  124 !/
  125       USE W3GDATMD
  126       USE W3WDATMD, ONLY: TIME, WLV, ICE, ICEH, ICEF, BERG, UST, USTDIR
  127       USE W3ADATMD, ONLY: CFLXYMAX, CFLTHMAX, AS, CX, CY, UA, UD, WN,  &
  128                           DW, HS, WLM, T01, T0M1, T02, THM, THS, FP0,  &
  129                           THP0, FP1, THP1, ABA, ABD, UBA, UBD, FCUT,   &
  130                           SXX, SYY, SXY, PHS, PTP, PLP, PDIR, PSI, PWS,&
  131                           PTM1, PT1, PT2, PEP,                         &
  132                           PTHP0, PQP, PSW, PPE, PGW, QP,               &
  133                           PWST, PNR, USERO, TAUOX, TAUOY, TAUWIX,      &
  134                           TAUWIY, PHIAW, PHIOC, TUSX, TUSY, PRMS, TPMS,&
  135                           USSX, USSY, MSSX, MSSY, MSCX, MSCY, CHARN,   &
  136                           TAUWNX,  TAUWNY, BHD, P2SMS, DTDYN,          &
  137                           CGE, T02, BEDFORMS, WHITECAP, TAUBBL, PHIBBL,&
  138                           CFLXYMAX, CFLTHMAX, CFLKMAX, US3D, STMAXE,   &
  139                           STMAXD, HMAXE, HCMAXE, HMAXD, HCMAXD, WBT
  140       USE W3ODATMD, ONLY: NDSE, NDST, NDSO, NOGRP, NGRPP, IDOUT, UNDEF,&
  141                           FLOGRD, FNMPRE, NOSWLL, NOGE
  142 !
  143       IMPLICIT NONE
  144 !/
  145 !/ ------------------------------------------------------------------- /
  146 !/ Local parameters
  147 !/
  148       INTEGER                 :: NDSI, NDSM, NDSOG, NDSDAT, NDSCTL,   &
  149                                  NDSTRC, NTRACE, IERR, I,J,K, IFI,IFJ,&
  150                                  TOUT(2), NOUT, TDUM(2), NVAR, IOUT,  &
  151                                  IX0, IXN, IY0, IYN, TIME0(2), IH0,   &
  152                                  IM0, ID0, IID, IJ0, IOTEST, IINC, IU,&
  153                                  TIMEN(2), JLEN
  154       REAL                    :: DTREQ, DTEST
  155       REAL                    :: FAC, XYMAX
  156       CHARACTER               :: COMSTR*1, IDTIME*23, IDDDAY*11,      &
  157                                  CINC*2
  158       CHARACTER*3             :: MNTH(12)
  159       CHARACTER*5             :: PARID
  160       LOGICAL                 :: FLONE, FLTWO, FLDIR, FLTRI, FLPRT
  161       LOGICAL                 :: FLREQ(NOGRP,NGRPP), FLOG(NOGRP),     &
  162                                  MSOUT, MBOUT, LTEMP(NGRPP)
  163 !/
  164 !/ ------------------------------------------------------------------- /
  165 !/
 
 
                                                          gx_outf.F90  page   4
 
 
  166       DATA TIME0  / -1, 0 /
  167       DATA MNTH   / 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',         &
  168                     'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /
  169 !
  170 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  171 ! 1.  IO set-up.
  172 !
  173       CALL W3NMOD ( 1, 6, 6 )
  174       CALL W3SETG ( 1, 6, 6 )
  175       CALL W3NDAT (    6, 6 )
  176       CALL W3SETW ( 1, 6, 6 )
  177       CALL W3NAUX (    6, 6 )
  178       CALL W3SETA ( 1, 6, 6 )
  179       CALL W3NOUT (    6, 6 )
  180       CALL W3SETO ( 1, 6, 6 )
  181 !
  182       NDSI   = 10
  183       NDSM   = 20
  184       NDSOG  = 20
  185       NDSDAT = 50
  186       NDSCTL = 51
  187 !
  188       NDSTRC =  6
  189       NTRACE = 10
  190 !
  191       WRITE (NDSO,900)
  192 !
  193       CALL ITRACE ( NDSTRC, NTRACE )
  194 !
  195       JLEN   = LEN_TRIM(FNMPRE)
  196       OPEN (NDSI,FILE=FNMPRE(:JLEN)//'gx_outf.inp',STATUS='OLD',       &
  197             ERR=800,IOSTAT=IERR)
  198       READ (NDSI,'(A)',END=801,ERR=802) COMSTR
  199       IF (COMSTR.EQ.' ') COMSTR = '$'
  200       WRITE (NDSO,901) COMSTR
  201 !
  202 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  203 ! 2.  Read model definition file.
  204 !
  205       CALL W3IOGR ( 'READ', NDSM )
  206       WRITE (NDSO,920) GNAME
  207       IF ( GTYPE .EQ. CLGTYPE ) THEN
  208          WRITE(NDSE,*)'PROGRAM GXOUTF: '// &
  209          'GRADS OUTPUT NOT IMPLEMENTED FOR CURVILINEAR GRIDS. '// &
  210          'NOW STOPPING.'
  211          CALL EXTCDE ( 1 )
  212       END IF
  213       IF ( ICLOSE .EQ. ICLOSE_TRPL ) THEN
  214          WRITE(NDSE,*)'PROGRAM GXOUTF: '// &
  215          'GRADS OUTPUT NOT IMPLEMENTED FOR TRIPOLE GRIDS. '// &
  216          'NOW STOPPING.'
  217          CALL EXTCDE ( 1 )
  218       END IF
  219 !
  220 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
                                                          gx_outf.F90  page   5
 
 
  221 ! 3.  Read general data and first fields from file
  222 !
  223       CALL W3IOGO ( 'READ', NDSOG, IOTEST )
  224 !
  225       WRITE (NDSO,930)
  226       DO I=1, NOGRP
  227        DO J=1, NGRPP
  228         IF ( FLOGRD(I,J) ) WRITE (NDSO,931) IDOUT(I,J)
  229         END DO
  230         END DO
  231 !
  232 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  233 ! 4.  Read requests from input file.
  234 !     Output times
  235 !
  236       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  237       READ (NDSI,*,END=801,ERR=802) TOUT, DTREQ, NOUT
  238       DTREQ  = MAX ( 0. , DTREQ )
  239       IF ( DTREQ.EQ.0 ) NOUT = 1
  240       NOUT   = MAX ( 1 , NOUT )
  241 !
  242       CALL STME21 ( TOUT , IDTIME )
  243       WRITE (NDSO,940) IDTIME
  244 !
  245       TDUM = 0
  246       CALL TICK21 ( TDUM , DTREQ )
  247       CALL STME21 ( TDUM , IDTIME )
  248       IF ( DTREQ .GE. 86400. ) THEN
  249           WRITE (IDDDAY,'(I10,1X)') INT(DTREQ/86400.)
  250         ELSE
  251           IDDDAY = '           '
  252         END IF
  253       IDTIME(1:11) = IDDDAY
  254       IDTIME(21:23) = '   '
  255       WRITE (NDSO,941) IDTIME, NOUT
  256 !
  257       IF ( MOD(NINT(DTREQ),60) .NE. 0 ) GOTO 810
  258 !
  259 ! ... Output fields
  260 !
  261       FLREQ(:,:)=.FALSE. ! Initialize FLGRD
  262       CALL W3READFLGRD ( NDSI, NDSO, 9, NDSE, COMSTR, FLOG,      &
  263                                  FLREQ, 1, 1, IERR )
  264 !
  265 ! Reset flags for variables not yet implemented in grads output interface
  266 !
  267 !    This needs to be reviewed whenever new parameters are added to the
  268 !     grads ctl and data files
  269 !
  270       IFI = 3
  271       DO IFJ = 1,NOGE(IFI)
  272         IF ( FLREQ(IFI,IFJ) ) THEN
  273           WRITE (NDSO,946) IDOUT(IFI,IFJ),                           &
  274                            '*** NOT YET CODED INTO GXOUTF ***'
  275           FLREQ(IFI,IFJ) = .FALSE.
 
 
                                                          gx_outf.F90  page   6
 
 
  276         END IF
  277       END DO
  278       IFI = 5
  279       DO IFJ = 7,10
  280         IF ( FLREQ(IFI,IFJ) ) THEN
  281           WRITE (NDSO,946) IDOUT(IFI,IFJ),                           &
  282                            '*** NOT YET CODED INTO GXOUTF ***'
  283           FLREQ(IFI,IFJ) = .FALSE.
  284         END IF
  285       END DO
  286       DO IFI = 6,8
  287         DO IFJ = 1,NOGE(IFI)
  288           IF ( FLREQ(IFI,IFJ) ) THEN
  289             WRITE (NDSO,946) IDOUT(IFI,IFJ),                           &
  290                              '*** NOT YET CODED INTO GXOUTF ***'
  291             FLREQ(IFI,IFJ) = .FALSE.
  292           END IF
  293         END DO
  294       END DO
  295       IFI = 10
  296       DO IFJ = 1,NOGE(IFI)
  297         IF ( FLREQ(IFI,IFJ) ) THEN
  298           WRITE (NDSO,946) IDOUT(IFI,IFJ),                           &
  299                            '*** NOT YET CODED INTO GXOUTF ***'
  300           FLREQ(IFI,IFJ) = .FALSE.
  301         END IF
  302       END DO
  303       IF ( FLREQ(9,5) ) THEN
  304         WRITE (NDSO,946) IDOUT(9,5),'*** NOT YET CODED INTO GXOUTF ***'
  305         FLREQ(9,5) = .FALSE.
  306       END IF
  307             
  308       WRITE (NDSO,945)
  309 !
  310       NVAR  = 1
  311       DO IFI=1, NOGRP
  312        DO IFJ=1, NGRPP
  313         IF ( FLREQ(IFI,IFJ) ) THEN
  314           IF ( .NOT. FLOGRD(IFI,IFJ) ) THEN
  315             WRITE (NDSO,946) IDOUT(IFI,IFJ),'*** DATA NOT AVAILABLE ***'
  316           ELSE
  317             WRITE (NDSO,946) IDOUT(IFI,IFJ), ' '
  318           END IF
  319         END IF
  320         FLREQ(IFI,IFJ) = FLREQ(IFI,IFJ) .AND. FLOGRD(IFI,IFJ)
  321 !
  322 ! Note: Whenever number of output parameters change, this needs updating
  323 !
  324 ! The grads output type code does NOT cover all the output parameter spac>
  325 !
  326         IF ( IFI.EQ.6 .AND. IFJ.EQ.1 ) THEN ! Radiation stresses needs 3D>
  327             IF ( FLREQ(IFI,IFJ) ) NVAR = NVAR + 3
  328           ELSE IF ( IFI.EQ.4 .AND. IFJ.LE.NOGE(4) ) THEN
  329             IF ( FLREQ(IFI,IFJ) ) NVAR = NVAR + NOSWLL + 1 ! Partitioned >
  330           ELSE IF ( IFI.EQ.1 .AND. IFJ .EQ. 2 ) THEN
 
 
                                                          gx_outf.F90  page   7
 
 
  331             IF ( FLREQ(IFI,IFJ) ) NVAR = NVAR + 2 ! Current vector
  332           ELSE IF ( IFI.EQ.1 .AND. IFJ.EQ.3 ) THEN
  333             IF ( FLREQ(IFI,IFJ) ) NVAR = NVAR + 2 ! Wind vector
  334           ELSE IF ( IFI.EQ.5 .AND. IFJ.EQ.1 ) THEN
  335             IF ( FLREQ(IFI,IFJ) ) NVAR = NVAR + 2 ! USTAR vector
  336           ELSE IF ( IFI.EQ.5 .AND. IFJ.EQ.5 ) THEN
  337             IF ( FLREQ(IFI,IFJ) ) NVAR = NVAR + 2 ! TAUWI vector
  338           ELSE IF ( IFI.EQ.5 .AND. IFJ.EQ.6 ) THEN
  339             IF ( FLREQ(IFI,IFJ) ) NVAR = NVAR + 2 ! TAUWN vector
  340 !          ELSE IF ( IFI.EQ.9 .AND. IFJ.EQ.5 ) THEN
  341 !            IF ( FLREQ(IFI,IFJ) ) NVAR = NVAR + 2 ! Max angular CFL vect>
  342           ELSE
  343             IF ( FLREQ(IFI,IFJ) ) NVAR = NVAR + 1 ! scalars
  344           END IF
  345         END DO
  346        END DO
  347 !
  348 ! ... Grid range
  349 !
  350       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  351       READ (NDSI,*,END=801,ERR=802) IX0, IXN, IY0, IYN, MSOUT, MBOUT
  352 !
  353       WRITE (NDSO,947)
  354 !
  355       IX0    = MAX ( 1, IX0 )
  356       IY0    = MAX ( 1, IY0 )
  357       IXN    = MIN ( NX, IXN )
  358       IYN    = MIN ( NY, IYN )
  359 !
  360       WRITE (NDSO,948) IX0, IXN, IY0, IYN
  361 !
  362       IF ( MSOUT ) THEN
  363           WRITE (NDSO,950) 'YES/--'
  364         ELSE
  365           WRITE (NDSO,950) '---/NO'
  366         END IF
  367 !
  368       IF ( .NOT. MSOUT ) MBOUT = .FALSE.
  369       IF ( MBOUT ) THEN
  370           WRITE (NDSO,951) 'YES/--'
  371         ELSE
  372           WRITE (NDSO,951) '---/NO'
  373         END IF
  374 !
  375       MSOUT  = .NOT. MSOUT
  376       MBOUT  = .NOT. MBOUT
  377 !
  378       OPEN (NDSDAT,FILE=FNMPRE(:JLEN)//'ww3.grads',FORM='UNFORMATTED', &
  379             ERR=811,IOSTAT=IERR, CONVERT='big_endian')
  380 !
  381 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  382 ! 5.  Time management.
  383 !
  384       IOUT   = 0
  385       WRITE (NDSO,970)
 
 
                                                          gx_outf.F90  page   8
 
 
  386 !
  387       DO
  388         DTEST  = DSEC21 ( TIME , TOUT )
  389         IF ( DTEST .GT. 0. ) THEN
  390             CALL W3IOGO ( 'READ', NDSOG, IOTEST )
  391             IF ( IOTEST .EQ. -1 ) THEN
  392                 WRITE (NDSO,942)
  393                 GOTO 600
  394               END IF
  395             CYCLE
  396           END IF
  397         IF ( DTEST .LT. 0. ) THEN
  398             CALL TICK21 ( TOUT , DTREQ )
  399             CYCLE
  400           END IF
  401 !
  402         IOUT   = IOUT + 1
  403         CALL STME21 ( TOUT , IDTIME )
  404         WRITE (NDSO,971) IDTIME
  405 !
  406         CALL GXEXGO ( NX, NY, NSEA )
  407         TIMEN  = TOUT
  408 !
  409         IF ( TIME0(1) .EQ. -1 ) TIME0 = TIME
  410 !
  411         CALL TICK21 ( TOUT , DTREQ )
  412         IF ( IOUT .GE. NOUT ) EXIT
  413         END DO
  414 !
  415 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  416 ! 6.  Close data file and write control file
  417 !
  418   600 CONTINUE
  419       WRITE (NDSO,980)
  420 !
  421       WRITE (NDSO,981)
  422       CLOSE (NDSDAT)
  423       WRITE (NDSO,982)
  424       OPEN (NDSCTL,FILE=FNMPRE(:JLEN)//'ww3.ctl',ERR=812,IOSTAT=IERR)
  425 !
  426       IH0    = TIME0(2)/10000
  427       IM0    = MOD(TIME0(2)/100,100)
  428       ID0    = MOD(TIME0(1),100)
  429       IID    = MOD(TIME0(1)/100,100)
  430       IJ0    = TIME0(1)/10000
  431 !
  432       IF ( IOUT .GT. 1 ) DTREQ  = DSEC21 ( TIME0, TIMEN ) / REAL(IOUT-1)
  433       IF ( IOUT .EQ. 1 ) DTREQ  = 3600.
  434       IF ( DTREQ .GT. 3599. ) THEN
  435           CINC   = 'HR'
  436           IINC   = NINT(DTREQ/3600.)
  437           IF ( MOD(NINT(DTREQ),3600) .NE. 0 ) GOTO 820
  438         ELSE
  439           CINC   = 'MN'
  440           IINC   = NINT(DTREQ/60.)
 
 
                                                          gx_outf.F90  page   9
 
 
  441         END IF
  442 !
  443       WRITE (NDSO,983) IOUT, IH0, IM0, ID0, MNTH(IID), IJ0, IINC, CINC
  444 !
  445       IF ( FLAGLL ) THEN
  446           FAC = 1.
  447         ELSE
  448           XYMAX  = MAX ( ABS(X0+REAL(IX0-1)*SX), &
  449                          ABS(X0+REAL(IXN-1)*SX), &
  450                          ABS(Y0+REAL(IY0-1)*SY), &
  451                          ABS(Y0+REAL(IYN-1)*SY) )
  452           IF ( XYMAX .LT. 1.E3 ) THEN
  453               FAC    = 1.E-1
  454             ELSE IF ( XYMAX .LT. 1.E4 ) THEN
  455               FAC    = 1.E-2
  456             ELSE IF ( XYMAX .LT. 1.E5 ) THEN
  457               FAC    = 1.E-3
  458             ELSE IF ( XYMAX .LT. 1.E6 ) THEN
  459               FAC    = 1.E-4
  460             ELSE
  461               FAC    = 1.E-5
  462           END IF
  463         END IF !FLAGLL
  464 !
  465       WRITE (NDSCTL,990) UNDEF,                                       &
  466                          (1+IXN-IX0), FAC*(X0+REAL(IX0-1)*SX), FAC*SX,&
  467                          (1+IYN-IY0), FAC*(Y0+REAL(IY0-1)*SY), FAC*SY,&
  468                          1, 1000., 1.,                                &
  469                          IOUT, IH0, IM0, ID0, MNTH(IID), IJ0,         &
  470                          IINC, CINC, NVAR
  471 !
  472       IU     = 99
  473       WRITE (NDSCTL,991) 'MAP  ', 0, IU, 'grid use map   '
  474 !
  475       IF ( FLREQ(01,01) )                                              &
  476            WRITE (NDSCTL,991) 'dpt  ', 0, IU, 'Water depth    '
  477       IF ( FLREQ(01,02) )                                              &
  478            WRITE (NDSCTL,991) 'ucur ', 0, IU, 'Current U (m/s)'
  479       IF ( FLREQ(01,02) )                                              &
  480            WRITE (NDSCTL,991) 'vcur ', 0, IU, 'Current V (m/s)'
  481       IF ( FLREQ(01,03) )                                              &
  482            WRITE (NDSCTL,991) 'uwnd ', 0, IU, 'Wind U (m/s)   '
  483       IF ( FLREQ(01,03) )                                              &
  484            WRITE (NDSCTL,991) 'vwnd ', 0, IU, 'Wind V (m/s)   '
  485       IF ( FLREQ(01,04) )                                              &
  486            WRITE (NDSCTL,991) 'ast  ', 0, IU, 'AT-SST (degr)  '
  487       IF ( FLREQ(01,05) )                                              &
  488            WRITE (NDSCTL,991) 'wlv  ', 0, IU, 'Water Level (m)'
  489       IF ( FLREQ(01,06) )                                              &
  490            WRITE (NDSCTL,991) 'ice  ', 0, IU, 'Ice Conc. (-)  '
  491       IF ( FLREQ(02,01) )                                              &
  492            WRITE (NDSCTL,991) 'hs   ', 0, IU, 'Wave height (m)'
  493       IF ( FLREQ(02,02) )                                              &
  494            WRITE (NDSCTL,991) 'lm   ', 0, IU, 'Mean L (m)     '
  495       IF ( FLREQ(02,03) )                                              &
 
 
                                                          gx_outf.F90  page  10
 
 
  496            WRITE (NDSCTL,991) 't02  ', 0, IU, 'Mean Per Tz (s)'
  497       IF ( FLREQ(02,04) )                                              &
  498            WRITE (NDSCTL,991) 't01  ', 0, IU, 'Mean Per Tm (s)  '
  499       IF ( FLREQ(02,05) )                                              &
  500            WRITE (NDSCTL,991) 't0m1', 0, IU, 'Mean Per Te (s)  '
  501       IF ( FLREQ(02,06) )                                              &
  502            WRITE (NDSCTL,991) 'fp   ', 0, IU, 'Peak Freq. (Hz)'
  503       IF ( FLREQ(02,07) )                                              &
  504            WRITE (NDSCTL,991) 'dir  ', 0, IU, 'Mean Dir. (rad)'
  505       IF ( FLREQ(02,08) )                                              &
  506            WRITE (NDSCTL,991) 'spr  ', 0, IU, 'Dir. spread    '
  507       IF ( FLREQ(02,09) )                                              &
  508            WRITE (NDSCTL,991) 'dp   ', 0, IU, 'Peak Dir. (rad)'
  509       IF ( FLREQ(04,01) ) THEN
  510           PARID  = 'phs  '
  511           DO I=0, NOSWLL
  512             WRITE (PARID(4:5),'(I2.2)') I
  513             WRITE (NDSCTL,991) PARID , 0, IU, 'Part. Hs (m)   '
  514             END DO
  515         END IF
  516       IF ( FLREQ(04,02) ) THEN
  517           PARID  = 'ptp  '
  518           DO I=0, NOSWLL
  519             WRITE (PARID(4:5),'(I2.2)') I
  520             WRITE (NDSCTL,991) PARID , 0, IU, 'Part. Tp (s)   '
  521             END DO
  522         END IF
  523       IF ( FLREQ(04,03) ) THEN
  524           PARID  = 'plp  '
  525           DO I=0, NOSWLL
  526             WRITE (PARID(4:5),'(I2.2)') I
  527             WRITE (NDSCTL,991) PARID , 0, IU, 'Part. L  (m)   '
  528             END DO
  529         END IF
  530       IF ( FLREQ(04,04) ) THEN
  531           PARID  = 'pdir  '
  532           DO I=0, NOSWLL
  533             WRITE (PARID(4:5),'(I2.2)') I
  534             WRITE (NDSCTL,991) PARID , 0, IU, 'Part. Th (deg.)'
  535             END DO
  536         END IF
  537       IF ( FLREQ(04,05) ) THEN
  538           PARID  = 'pspr '
  539           DO I=0, NOSWLL
  540             WRITE (PARID(4:5),'(I2.2)') I
  541             WRITE (NDSCTL,991) PARID , 0, IU, 'Part. si (deg.)'
  542             END DO
  543         END IF
  544       IF ( FLREQ(04,06) ) THEN
  545           PARID  = 'pws  '
  546           DO I=0, NOSWLL
  547             WRITE (PARID(4:5),'(I2.2)') I
  548             WRITE (NDSCTL,991) PARID , 0, IU, 'Part. ws frac. '
  549             END DO
  550         END IF
 
 
                                                          gx_outf.F90  page  11
 
 
  551       IF ( FLREQ(04,07) )                                              &
  552            WRITE (NDSCTL,991) 'tws  ', 0, IU, 'Total ws frac. '
  553       IF ( FLREQ(04,08) )                                              &
  554            WRITE (NDSCTL,991) 'pnr  ', 0, IU, 'Number of part.'
  555       IF ( FLREQ(05,01) )                                              &
  556            WRITE (NDSCTL,991) 'uust ', 0, IU, 'Fr.Vel. U(m/s) '
  557       IF ( FLREQ(05,01) )                                              &
  558            WRITE (NDSCTL,991) 'vust ', 0, IU, 'Fr.Vel. V(m/s) '
  559       IF ( FLREQ(05,02) )                                              &
  560            WRITE (NDSCTL,991) 'cha  ', 0, IU, 'Charnock parameter'
  561       IF ( FLREQ(05,03) )                                              &
  562            WRITE (NDSCTL,991) 'cge  ', 0, IU, 'Energy Flux (W/m)'
  563       IF ( FLREQ(05,04) )                                              &
  564            WRITE (NDSCTL,991) 'faw  ', 0, IU, 'Air-sea energy flux'
  565       IF ( FLREQ(05,05) )                                              &
  566            WRITE (NDSCTL,991) 'utaw  ', 0, IU, 'Net wave supp str'
  567       IF ( FLREQ(05,05) )                                              &
  568            WRITE (NDSCTL,991) 'vtaw  ', 0, IU, 'Net wave supp str'
  569       IF ( FLREQ(05,06) )                                              &
  570            WRITE (NDSCTL,991) 'utwa  ', 0, IU, 'Neg wave supp str'
  571       IF ( FLREQ(05,06) )                                              &
  572            WRITE (NDSCTL,991) 'vtwa  ', 0, IU, 'Neg wave supp str'
  573       IF ( FLREQ(09,01) )                                              &
  574            WRITE (NDSCTL,991) 'dtd  ', 0, IU, 'DTAVG ST (min) '
  575       IF ( FLREQ(09,02) )                                              &
  576            WRITE (NDSCTL,991) 'fc   ', 0, IU, 'fcut (Hz)      '
  577       IF ( FLREQ(09,03) )                                              &
  578            WRITE (NDSCTL,991) 'cfx  ', 0, IU, 'Max CFL XY Prop'
  579       IF ( FLREQ(09,04) )                                              &
  580            WRITE (NDSCTL,991) 'cfd  ', 0, IU, 'Max CFL TH Prop'
  581 !
  582       WRITE (NDSCTL,992)
  583 !
  584       GOTO 888
  585 !
  586 ! Escape locations read errors :
  587 !
  588   800 CONTINUE
  589       WRITE (NDSE,1000) IERR
  590       CALL EXTCDE ( 1 )
  591 !
  592   801 CONTINUE
  593       WRITE (NDSE,1001)
  594       CALL EXTCDE ( 2 )
  595 !
  596   802 CONTINUE
  597       WRITE (NDSE,1002) IERR
  598       CALL EXTCDE ( 3 )
  599 !
  600   810 CONTINUE
  601       WRITE (NDSE,1010)
  602       CALL EXTCDE ( 10 )
  603 !
  604   811 CONTINUE
  605       WRITE (NDSE,1011)
 
 
                                                          gx_outf.F90  page  12
 
 
  606       CALL EXTCDE ( 11 )
  607 !
  608   812 CONTINUE
  609       WRITE (NDSE,1012)
  610       CALL EXTCDE ( 12 )
  611 !
  612   820 CONTINUE
  613       WRITE (NDSE,1020) DTREQ
  614       CALL EXTCDE ( 20 )
  615 !
  616   821 CONTINUE
  617       WRITE (NDSE,1021)
  618       CALL EXTCDE ( 21 )
  619 !
  620   888 CONTINUE
  621       WRITE (NDSO,999)
  622 !
  623 ! Formats
  624 !
  625   900 FORMAT (/12X,'   *** WAVEWATCH III GrADS field output postp. ***   >
  626                12X,'=====================================================>
  627   901 FORMAT ( '  Comment character is ''',A,''''/)
  628 !
  629   920 FORMAT ( '  Grid name : ',A/)
  630 !
  631   930 FORMAT ( '  Fields in file : '/                                 &
  632                ' --------------------------')
  633   931 FORMAT ( '      ',A)
  634 !
  635   940 FORMAT (/'  Output time data : '/                               &
  636                ' -----------------------------------------------------'/ &
  637                '      First time         : ',A)
  638   941 FORMAT ( '      Interval           : ',A/                       &
  639                '      Number of requests : ',I4)
  640   942 FORMAT (/'      End of file reached '/)
  641 !
  642   945 FORMAT (/'  Requested output fields : '/                        &
  643                ' -----------------------------------------------------')
  644   946 FORMAT ( '      ',A,1X,A)
  645 !
  646   947 FORMAT (/'  Requested discrete grid ranges : '/                 &
  647                ' -----------------------------------------------------')
  648   948 FORMAT ( '      Longitudes         : ',2I6/                     &
  649                '      lattidutes         : ',2I6/                     &
  650                '      Opening file ww3.grads')
  651   949 FORMAT ( '      Alternative definition is used ')
  652   950 FORMAT ( '      Sea points in mask :      ',A)
  653   951 FORMAT ( '      Bound. pts. in mask:      ',A)
  654 !
  655   970 FORMAT (//'  Generating file '/                                 &
  656                ' -----------------------------------------------------')
  657   971 FORMAT ( '      Data for ',A)
  658 !
  659   980 FORMAT (//'  Final file management '/                           &
  660                ' -----------------------------------------------------')
 
 
                                                          gx_outf.F90  page  13
 
 
  661   981 FORMAT ( '      Closing file ww3.grads')
  662   982 FORMAT ( '      Opening file ww3.ctl')
  663   983 FORMAT ( '         Number of times : ',I6/                      &
  664            '         Initial time ID : ',I2.2,':',I2.2,'Z',I2.2,A3,I4/ &
  665            '         Time step ID    : ',I2,A2)
  666 !
  667   990 FORMAT ('DSET      ww3.grads'/                                  &
  668               'TITLE     WAVEWATCH III gridded data'/                 &
  669               'OPTIONS   sequential'/                                 &
  670               'OPTIONS   big_endian'/                                 &
  671               'UNDEF    ',F8.1/                                       &
  672               'XDEF     ',I4,'  LINEAR ',2F12.5/                      &
  673               'YDEF     ',I4,'  LINEAR ',2F12.5/                      &
  674               'ZDEF     ',I4,'  LINEAR ',2F12.5/                      &
  675               'TDEF     ',I4,'  LINEAR ',I6.2,':',I2.2,'Z',I2.2,A3,I4, &
  676                2x,I2,A2/                                              &
  677               'VARS     ',I4)
  678   991 FORMAT (A6,2I4,2X,A15)
  679   992 FORMAT ('ENDVARS')
  680 !
  681   999 FORMAT (/'  End of program '/                                   &
  682                ' ========================================='/          &
  683                '         WAVEWATCH III GrADS field output '/)
  684 !
  685  1000 FORMAT (/' *** WAVEWATCH III ERROR IN GXOUTF : '/               &
  686                '     ERROR IN OPENING INPUT FILE'/                    &
  687                '     IOSTAT =',I5/)
  688 !
  689  1001 FORMAT (/' *** WAVEWATCH III ERROR IN GXOUTF : '/               &
  690                '     PREMATURE END OF INPUT FILE'/)
  691 !
  692  1002 FORMAT (/' *** WAVEWATCH III ERROR IN GXOUTF : '/               &
  693                '     ERROR IN READING FROM INPUT FILE'/               &
  694                '     IOSTAT =',I5/)
  695 !
  696  1010 FORMAT (/' *** WAVEWATCH III ERROR IN GXOUTF : '/               &
  697                '     SMALLEST OUTPUT INCREMENT IS 60 SEC.'/)
  698 !
  699  1011 FORMAT (/' *** WAVEWATCH III ERROR IN GXOUTF : '/               &
  700                '     ERROR IN OPENING OUTPUT FILE ww3.grads'/         &
  701                '     IOSTAT =',I5/)
  702 !
  703  1012 FORMAT (/' *** WAVEWATCH III ERROR IN GXOUTF : '/               &
  704                '     ERROR IN OPENING OUTPUT FILE ww3.ctl'/           &
  705                '     IOSTAT =',I5/)
  706 !
  707  1020 FORMAT (/' *** WAVEWATCH III ERROR IN GXOUTF : '/               &
  708                '     FIELD INCREMENT > 1HR BUT NOT MULTIPLE',F10.0/)
  709 !
  710  1021 FORMAT (/' *** WAVEWATCH III ERROR IN GXOUTF : '/               &
  711                '     UPDATE PARS IN LOOP 610 !!!'/)
  712 !/
  713 !/ Internal subroutine GXEXGO ---------------------------------------- /
  714 !/
  715       CONTAINS
 
 
                                                          gx_outf.F90  page  14
 
 
  716 !/ ------------------------------------------------------------------- /
  717       SUBROUTINE GXEXGO ( NX, NY, NSEA )
  718 !/
  719 !/                  +-----------------------------------+
  720 !/                  | WAVEWATCH III           NOAA/NCEP |
  721 !/                  |           H. L. Tolman            |
  722 !/                  |                        FORTRAN 90 |
  723 !/                  | Last update :         28-Mar-2007 |
  724 !/                  +-----------------------------------+
  725 !/
  726 !/    30-Jun-1999 : Final FORTRAN 77                    ( version 1.18 )
  727 !/    24-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
  728 !/                  Massive changes to logistics.
  729 !/    29-Jan-2001 : Add output fields 17-18             ( version 2.20 )
  730 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
  731 !/    27-Jun-2005 : Adding MAPST2.                      ( version 3.07 )
  732 !/    21-Jul-2005 : Add output fields 19-21.            ( version 3.07 )
  733 !/    05-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
  734 !/    18-Jan-2007 : Update MSOUT/MBOUT treatment.       ( version 3.10 )
  735 !/    28-Mar-2007 : Adding partitioned output.          ( version 3.11 )
  736 !/                  Adding user slots for outputs.
  737 !/
  738 !  1. Purpose :
  739 !
  740 !     Perform actual output for GrADS postprocessing.
  741 !
  742 !  3. Parameters :
  743 !
  744 !     Parameter list
  745 !     ----------------------------------------------------------------
  746 !       NX/Y    Int.  I  Grid dimensions.
  747 !       NSEA    Int.  I  Number of sea points.
  748 !     ----------------------------------------------------------------
  749 !
  750 !     Internal parameters
  751 !     ----------------------------------------------------------------
  752 !       X1, X2, XX
  753 !               R.A.  Output fields
  754 !     ----------------------------------------------------------------
  755 !
  756 !  4. Subroutines used :
  757 !
  758 !      Name      Type  Module   Description
  759 !     ----------------------------------------------------------------
  760 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  761 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
  762 !      W3S2XY    Subr.   Id.    Convert from storage to spatial grid.
  763 !     ---------------------------------------------------------------
  764 !
  765 !  5. Called by :
  766 !
  767 !     Main program in which it is contained.
  768 !
  769 !  6. Error messages :
  770 !
 
 
                                                          gx_outf.F90  page  15
 
 
  771 !       None.
  772 !
  773 !  7. Remarks :
  774 !
  775 !     - Note that arrays CX and CY of the main program now contain
  776 !       the absolute current speed and direction respectively.
  777 !     - VALLND added to assure that map with only land plots in
  778 !       GrADS.
  779 !
  780 !  8. Structure :
  781 !
  782 !     See source code.
  783 !
  784 !  9. Switches :
  785 !
  786 !     !/S  Enable subroutine tracing.
  787 !     !/T  Enable test output.
  788 !
  789 ! 10. Source code :
  790 !
  791 !/ ------------------------------------------------------------------- /
  792       USE W3SERVMD, ONLY: W3S2XY
  793 !/
  794 !/ ------------------------------------------------------------------- /
  795 !/ Parameter list
  796 !/
  797       INTEGER, INTENT(IN)     :: NX, NY, NSEA
  798 !/
  799 !/ ------------------------------------------------------------------- /
  800 !/ Local parameters
  801 !/
  802       INTEGER                 :: IX, IY, J, ISEA, IXL, IXR
  803       INTEGER                 :: MAPXCL(NY,NX), MAPDRY(NY,NX),        &
  804                                  MAPICE(NY,NX), MAPLND(NY,NX),        &
  805                                  MAPMSK(NY,NX)
  806       REAL                    :: X1(NX,NY), XX(NX,NY), XY(NX,NY),     &
  807                                  XA(NX,NY,0:NOSWLL)
  808       REAL                    :: VALLND = 0.001
  809 !/
  810 !/ ------------------------------------------------------------------- /
  811 !/
  812 !
  813 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  814 ! 1.  Preparations
  815 ! 1.a Write map to file
  816 !
  817       MAPXCL = MOD(MAPST2,2)
  818       MAPICE = MOD(MAPST2,2)
  819       MAPDRY = MOD(MAPST2/2,2)
  820       MAPLND = MOD(MAPST2/4,2)
  821       MAPMSK = MOD(MAPST2/8,2)
  822 !
  823       DO IY=1, NY
  824         DO IX=1, NX
  825           IF ( MAPSTA(IY,IX).EQ.0 ) THEN
 
 
                                                          gx_outf.F90  page  16
 
 
  826               IF ( MAPXCL(IY,IX).EQ.1 ) THEN
  827                   X1(IX,IY) = UNDEF
  828                 ELSE
  829                   X1(IX,IY) = VALLND
  830                 END IF
  831             ELSE IF ( MAPSTA(IY,IX).LT.0 ) THEN
  832               IF ( MAPMSK(IY,IX).EQ.1 ) THEN
  833                   X1(IX,IY) = -4.
  834                 ELSE IF ( MAPLND(IY,IX).EQ.1 ) THEN
  835                   X1(IX,IY) = VALLND
  836                 ELSE IF ( MAPICE(IY,IX).EQ.1 .AND.                    &
  837                           MAPDRY(IY,IX).EQ.1 ) THEN
  838                   X1(IX,IY) = -3.
  839                 ELSE IF ( MAPDRY(IY,IX).EQ.1 ) THEN
  840                   X1(IX,IY) = -2.
  841                 ELSE IF ( MAPICE(IY,IX).EQ.1 ) THEN
  842                   X1(IX,IY) = -1.
  843                 ELSE
  844                   X1(IX,IY) = -5.
  845                 END IF
  846             ELSE
  847               X1(IX,IY) = REAL(MAPSTA(IY,IX))
  848               IF ( MSOUT ) THEN
  849                   IF ( MAPSTA(IY,IX) .GT. 0 ) X1(IX,IY) = UNDEF
  850                 ELSE IF ( MBOUT ) THEN
  851                   IF ( MAPSTA(IY,IX).EQ.2  .OR.                      &
  852                        IY.EQ.1 .OR. IY.EQ.NY .OR.                    &
  853                        ( ICLOSE.NE.ICLO_NONE .AND.                   &
  854                              (IX.EQ.1 .OR. IX.EQ.NX) ) ) THEN
  855                       X1(IX,IY) = UNDEF
  856                     ELSE
  857                       IXl    = 1 + MOD(IX+NX-2,NX)
  858                       IXR    = 1 + MOD(IX,NX)
  859                       IF ( MAPSTA(IY+1,IXL).EQ.0 .AND.               &
  860                            MAPXCL(IY+1,IXL).EQ.1 ) X1(IX,IY) = UNDEF
  861                       IF ( MAPSTA(IY+1,IX ).EQ.0 .AND.               &
  862                            MAPXCL(IY+1,IX ).EQ.1 ) X1(IX,IY) = UNDEF
  863                       IF ( MAPSTA(IY+1,IXR).EQ.0 .AND.               &
  864                            MAPXCL(IY+1,IXR).EQ.1 ) X1(IX,IY) = UNDEF
  865                       IF ( MAPSTA( IY ,IXR).EQ.0 .AND.               &
  866                            MAPXCL( IY ,IXR).EQ.1 ) X1(IX,IY) = UNDEF
  867                       IF ( MAPSTA(IY-1,IXR).EQ.0 .AND.               &
  868                            MAPXCL(IY-1,IXR).EQ.1 ) X1(IX,IY) = UNDEF
  869                       IF ( MAPSTA(IY-1,IX ).EQ.0 .AND.               &
  870                            MAPXCL(IY-1,IX ).EQ.1 ) X1(IX,IY) = UNDEF
  871                       IF ( MAPSTA(IY-1,IXL).EQ.0 .AND.               &
  872                            MAPXCL(IY-1,IXL).EQ.1 ) X1(IX,IY) = UNDEF
  873                       IF ( MAPSTA( IY ,IXL).EQ.0 .AND.               &
  874                            MAPXCL( IY ,IXL).EQ.1 ) X1(IX,IY) = UNDEF
  875                     END IF
  876                 END IF
  877               IF ( MSOUT .AND. MAPSTA(IY,IX).EQ.1 ) X1(IX,IY) = UNDEF
  878               IF ( MBOUT .AND. MAPSTA(IY,IX).EQ.2 ) X1(IX,IY) = UNDEF
  879             END IF
  880           VALLND = - VALLND
 
 
                                                          gx_outf.F90  page  17
 
 
  881           END DO
  882         END DO
  883 !
  884       WRITE (NDSDAT) ((X1(IX,IY),IX=IX0,IXN),IY=IY0,IYN)
  885 !
  886 ! 1.b Initialize arrays
  887 !
  888       X1 = UNDEF
  889       XX = UNDEF
  890       XY = UNDEF
  891       XA = UNDEF
  892 !
  893 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  894 ! 2.  Loop over output fields.
  895 !
  896       DO J=1, NOGRP
  897        DO K=1, NGRPP
  898 !        WRITE(*,*)J,K,FLREQ(J,K)
  899         IF ( FLREQ(J,K) ) THEN
  900 !
  901 ! Set array dimension flags
  902         FLONE = .FALSE.
  903         FLTWO = .FALSE.
  904         FLDIR = .FALSE.
  905         FLTRI = .FALSE.
  906         FLPRT = .FALSE.
  907 !
  908 ! 2.a Set output arrays and parameters
  909 !
  910 !     Water depth
  911 !
  912             IF ( J.EQ.1 .AND. K.EQ.1 ) THEN
  913                 FLONE = .TRUE.
  914                 CALL W3S2XY ( NSEA, NSEA, NX, NY, DW(1:NSEA)          &
  915                                                         , MAPSF, X1 )
  916 !
  917 !     Current
  918 !
  919               ELSE IF ( J.EQ.1 .AND. K.EQ.2 ) THEN
  920                 FLTWO = .TRUE.
  921                 CALL W3S2XY ( NSEA, NSEA, NX, NY, CX(1:NSEA)          &
  922                                                         , MAPSF, XX )
  923                 CALL W3S2XY ( NSEA, NSEA, NX, NY, CY(1:NSEA)          &
  924                                                         , MAPSF, XY )
  925 !
  926 !     Wind speed
  927 !
  928               ELSE IF ( J.EQ.1 .AND. K.EQ.3 ) THEN
  929                 FLTWO = .TRUE.
  930                 CALL W3S2XY ( NSEA, NSEA, NX, NY, UA(1:NSEA)          &
  931                                                         , MAPSF, XX )
  932                 CALL W3S2XY ( NSEA, NSEA, NX, NY, UD(1:NSEA)          &
  933                                                         , MAPSF, XY )
  934 !
  935 !     Air-sea temp. dif.
 
 
                                                          gx_outf.F90  page  18
 
 
  936 !
  937               ELSE IF ( J.EQ.1 .AND. K.EQ.4 ) THEN
  938                 FLONE = .TRUE.
  939                 CALL W3S2XY ( NSEA, NSEA, NX, NY, AS(1:NSEA)          &
  940                                                         , MAPSF, X1 )
  941 !
  942 !     Water level
  943 !
  944               ELSE IF ( J.EQ.1 .AND. K.EQ.5 ) THEN
  945                 FLONE = .TRUE.
  946                 CALL W3S2XY ( NSEA, NSEA, NX, NY, WLV   , MAPSF, X1 )
  947 !
  948 !     Ice concentration
  949 !
  950               ELSE IF ( J.EQ.1 .AND. K.EQ.6 ) THEN
  951                 FLONE = .TRUE.
  952                 CALL W3S2XY ( NSEA, NSEA, NX, NY, ICE   , MAPSF, X1 )
  953 !
  954 !     Ice thickness
  955 !
  956 !     Average sea ice floe diameter
  957 !
  958 !     Significant wave height
  959 !
  960               ELSE IF ( J.EQ.2 .AND. K.EQ.1 ) THEN
  961                 FLONE = .TRUE.
  962                 CALL W3S2XY ( NSEA, NSEA, NX, NY, HS    , MAPSF, X1 )
  963 !
  964 !     Mean wave length
  965 !
  966               ELSE IF ( J.EQ.2 .AND. K.EQ.2 ) THEN
  967                 FLONE = .TRUE.
  968                 CALL W3S2XY ( NSEA, NSEA, NX, NY, WLM   , MAPSF, X1 )
  969 !
  970 !     Mean zero-crossing wave period T02
  971 !
  972               ELSE IF ( J.EQ.2 .AND. K.EQ.3 ) THEN
  973                 FLONE = .TRUE.
  974                 CALL W3S2XY ( NSEA, NSEA, NX, NY, T02   , MAPSF, X1 )
  975 !
  976 !     Mean wave period Tm
  977 !
  978               ELSE IF ( J.EQ.2 .AND. K.EQ.4 ) THEN
  979                 FLONE = .TRUE.
  980                 CALL W3S2XY ( NSEA, NSEA, NX, NY, T0M1   , MAPSF, X1 )
  981 !
  982 !     Mean energy wave period Te
  983 !
  984               ELSE IF ( J.EQ.2 .AND. K.EQ.5 ) THEN
  985                 FLONE = .TRUE.
  986                 CALL W3S2XY ( NSEA, NSEA, NX, NY, T01   , MAPSF, X1 )
  987 !
  988 !     Peak period
  989 !
  990               ELSE IF ( J.EQ.2 .AND. K.EQ.6 ) THEN
 
 
                                                          gx_outf.F90  page  19
 
 
  991                 FLONE = .TRUE.
  992                 CALL W3S2XY ( NSEA, NSEA, NX, NY, FP0   , MAPSF, X1 )
  993 !
  994 !     Mean wave direction
  995 !
  996               ELSE IF ( J.EQ.2 .AND. K.EQ.7 ) THEN
  997                 FLONE = .TRUE.
  998                 CALL W3S2XY ( NSEA, NSEA, NX, NY, THM   , MAPSF, X1 )
  999 !
 1000 !     Directional spread
 1001 !
 1002               ELSE IF ( J.EQ.2 .AND. K.EQ.8 ) THEN
 1003                 FLONE = .TRUE.
 1004                 CALL W3S2XY ( NSEA, NSEA, NX, NY, THS   , MAPSF, X1 )
 1005 !
 1006 !     Peak direction
 1007 !
 1008               ELSE IF ( J.EQ.2 .AND. K.EQ.9 ) THEN
 1009                 FLONE = .TRUE.
 1010                 CALL W3S2XY ( NSEA, NSEA, NX, NY, THP0  , MAPSF, X1 )
 1011 !
 1012 !     Dominant wave breaking probability
 1013 !
 1014               ELSE IF ( J.EQ.2 .AND. K.EQ.17 ) THEN
 1015                 FLONE = .TRUE.
 1016                 CALL W3S2XY ( NSEA, NSEA, NX, NY, WBT  , MAPSF, X1 )
 1017 !
 1018 !     Partitioned wave heights
 1019 !
 1020               ELSE IF ( J.EQ.4 .AND. K.EQ.1 ) THEN
 1021                 FLPRT = .TRUE.
 1022                 DO I=0, NOSWLL
 1023                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PHS(:,I),         &
 1024                                                    MAPSF, XA(:,:,I) )
 1025                   END DO
 1026 !
 1027 !     Partitioned peak period
 1028 !
 1029               ELSE IF ( J.EQ.4 .AND. K.EQ.2 ) THEN
 1030                 FLPRT = .TRUE.
 1031                 DO I=0, NOSWLL
 1032                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PTP(:,I),         &
 1033                                                    MAPSF, XA(:,:,I) )
 1034                   END DO
 1035 !
 1036 !     Partitioned wave leangths (peak)
 1037 !
 1038               ELSE IF ( J.EQ.4 .AND. K.EQ.3 ) THEN
 1039                 FLPRT = .TRUE.
 1040                 DO I=0, NOSWLL
 1041                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PLP(:,I),         &
 1042                                                    MAPSF, XA(:,:,I) )
 1043                   END DO
 1044 !
 1045 !     Partitioned directions
 
 
                                                          gx_outf.F90  page  20
 
 
 1046 !
 1047               ELSE IF ( J.EQ.4 .AND. K.EQ.4 ) THEN
 1048                 FLPRT = .TRUE.
 1049                 DO I=0, NOSWLL
 1050                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PDIR(:,I),         &
 1051                                                    MAPSF, XA(:,:,I) )
 1052                   END DO
 1053 !
 1054 !     Partitioned direstional spread
 1055 !
 1056               ELSE IF ( J.EQ.4 .AND. K.EQ.5 ) THEN
 1057                 FLPRT = .TRUE.
 1058                 DO I=0, NOSWLL
 1059                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PSI(:,I),         &
 1060                                                    MAPSF, XA(:,:,I) )
 1061                   END DO
 1062 !
 1063 !     Partitioned wind sea fraction
 1064 !
 1065               ELSE IF ( J.EQ.4 .AND. K.EQ.6 ) THEN
 1066                 FLPRT = .TRUE.
 1067                 DO I=0, NOSWLL
 1068                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PWS(:,I),         &
 1069                                                    MAPSF, XA(:,:,I) )
 1070                   END DO
 1071 !
 1072 !     Partitioned peak direction
 1073 !
 1074               ELSE IF ( J.EQ.4 .AND. K.EQ.7 ) THEN
 1075                 FLPRT = .TRUE.
 1076                 DO I=0, NOSWLL
 1077                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PTHP0(:,I),       &
 1078                                                    MAPSF, XA(:,:,I) )
 1079                   END DO
 1080 !
 1081 !     Partitioned peakedness
 1082 !
 1083               ELSE IF ( J.EQ.4 .AND. K.EQ.8 ) THEN
 1084                 FLPRT = .TRUE.
 1085                 DO I=0, NOSWLL
 1086                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PQP(:,I),         &
 1087                                                    MAPSF, XA(:,:,I) )
 1088                   END DO
 1089 !
 1090 !     Partitioned peak enhancement factor
 1091 !
 1092               ELSE IF ( J.EQ.4 .AND. K.EQ.9 ) THEN
 1093                 FLPRT = .TRUE.
 1094                 DO I=0, NOSWLL
 1095                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PPE(:,I),         &
 1096                                                    MAPSF, XA(:,:,I) )
 1097                   END DO
 1098 !
 1099 !     Partitioned gaussian frequency spread
 1100 !
 
 
                                                          gx_outf.F90  page  21
 
 
 1101               ELSE IF ( J.EQ.4 .AND. K.EQ.10 ) THEN
 1102                 FLPRT = .TRUE.
 1103                 DO I=0, NOSWLL
 1104                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PGW(:,I),         &
 1105                                                    MAPSF, XA(:,:,I) )
 1106                   END DO
 1107 !
 1108 !     Partitioned spectral width
 1109 !
 1110               ELSE IF ( J.EQ.4 .AND. K.EQ.11 ) THEN
 1111                 FLPRT = .TRUE.
 1112                 DO I=0, NOSWLL
 1113                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PSW(:,I),         &
 1114                                                    MAPSF, XA(:,:,I) )
 1115                   END DO
 1116 !
 1117 !     Partitioned mean energy period (-1)
 1118 !
 1119               ELSE IF ( J.EQ.4 .AND. K.EQ.12 ) THEN
 1120                 FLPRT = .TRUE.
 1121                 DO I=0, NOSWLL
 1122                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PTM1(:,I),        &
 1123                                                    MAPSF, XA(:,:,I) )
 1124                   END DO
 1125 !
 1126 !     Partitioned mean wave period (+1)
 1127 !
 1128               ELSE IF ( J.EQ.4 .AND. K.EQ.13 ) THEN
 1129                 FLPRT = .TRUE.
 1130                 DO I=0, NOSWLL
 1131                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PT1(:,I),         &
 1132                                                    MAPSF, XA(:,:,I) )
 1133                   END DO
 1134 !
 1135 !     Partitioned mean wave period (+2)
 1136 !
 1137               ELSE IF ( J.EQ.4 .AND. K.EQ.14 ) THEN
 1138                 FLPRT = .TRUE.
 1139                 DO I=0, NOSWLL
 1140                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PT2(:,I),         &
 1141                                                    MAPSF, XA(:,:,I) )
 1142                   END DO
 1143 !
 1144 !     Partitioned peak density
 1145 !
 1146               ELSE IF ( J.EQ.4 .AND. K.EQ.15 ) THEN
 1147                 FLPRT = .TRUE.
 1148                 DO I=0, NOSWLL
 1149                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PEP(:,I),         &
 1150                                                    MAPSF, XA(:,:,I) )
 1151                   END DO
 1152 !
 1153 !     Total wind sea fraction
 1154 !
 1155               ELSE IF ( J.EQ.4 .AND. K.EQ.16 ) THEN
 
 
                                                          gx_outf.F90  page  22
 
 
 1156                 FLONE = .TRUE.
 1157                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PWST ,MAPSF, X1 )
 1158 !
 1159 !     Number of partitions
 1160 !
 1161               ELSE IF ( J.EQ.4 .AND. K.EQ.17 ) THEN
 1162                 FLONE = .TRUE.
 1163                   CALL W3S2XY ( NSEA, NSEA, NX, NY, PNR , MAPSF, X1 )
 1164 !
 1165 !     Friction velocity
 1166 !
 1167               ELSE IF ( J.EQ.5 .AND. K.EQ.1 ) THEN
 1168                 FLTWO = .TRUE.
 1169                 CALL W3S2XY ( NSEA, NSEA, NX, NY, UST   (1:NSEA)      &
 1170                                                         , MAPSF, XX )
 1171                 CALL W3S2XY ( NSEA, NSEA, NX, NY, USTDIR(1:NSEA)      &
 1172                                                         , MAPSF, XY )
 1173 !
 1174 !     Charnock parameter
 1175 !
 1176               ELSE IF ( J.EQ.5 .AND. K.EQ.2 ) THEN
 1177                 FLONE = .TRUE.
 1178                 CALL W3S2XY ( NSEA, NSEA, NX, NY, CHARN , MAPSF, X1 )
 1179 !
 1180 !     Mean energy flux
 1181 !
 1182               ELSE IF ( J.EQ.5 .AND. K.EQ.3 ) THEN
 1183                 FLONE = .TRUE.
 1184                 CALL W3S2XY ( NSEA, NSEA, NX, NY, CGE   , MAPSF, X1 )
 1185 !
 1186 !     Air-sea energy flux
 1187 !
 1188               ELSE IF ( J.EQ.5 .AND. K.EQ.4 ) THEN
 1189                 FLONE = .TRUE.
 1190                 CALL W3S2XY ( NSEA, NSEA, NX, NY, PHIAW , MAPSF, X1 )
 1191 !
 1192 !     Net supported wave stress
 1193 !
 1194               ELSE IF ( J.EQ.5 .AND. K.EQ.5 ) THEN
 1195                 FLTWO = .TRUE.
 1196                 CALL W3S2XY ( NSEA, NSEA, NX, NY, TAUWIX, MAPSF, XX )
 1197                 CALL W3S2XY ( NSEA, NSEA, NX, NY, TAUWIY, MAPSF, XY )
 1198 !
 1199 !     Net supported wave stress
 1200 !
 1201               ELSE IF ( J.EQ.5 .AND. K.EQ.6 ) THEN
 1202                 FLTWO = .TRUE.
 1203                 CALL W3S2XY ( NSEA, NSEA, NX, NY, TAUWNX, MAPSF, XX )
 1204                 CALL W3S2XY ( NSEA, NSEA, NX, NY, TAUWNY, MAPSF, XY )
 1205 !
 1206 !     Peakedness
 1207 !
 1208               ELSE IF ( J.EQ.8 .AND. K.EQ.5 ) THEN
 1209                 FLONE = .TRUE.
 1210                 CALL W3S2XY ( NSEA, NSEA, NX, NY, QP    , MAPSF, X1 )
 
 
                                                          gx_outf.F90  page  23
 
 
 1211 !
 1212 !     Average source term time step
 1213 !
 1214               ELSE IF ( J.EQ.9 .AND. K.EQ.1 ) THEN
 1215                 FLONE = .TRUE.
 1216                 DO ISEA=1, NSEA
 1217                   IF ( DTDYN(ISEA) .NE. UNDEF )                       &
 1218                        DTDYN(ISEA) = DTDYN(ISEA) / 60.
 1219                   END DO
 1220                 CALL W3S2XY ( NSEA, NSEA, NX, NY, DTDYN , MAPSF, X1 )
 1221 !
 1222 !     Cut-off frequency
 1223 !
 1224               ELSE IF ( J.EQ.9 .AND. K.EQ.2 ) THEN
 1225                 FLONE = .TRUE.
 1226                 CALL W3S2XY ( NSEA, NSEA, NX, NY, FCUT  , MAPSF, X1 )
 1227 !
 1228 !     Max CFL step for XY propagation
 1229 !
 1230               ELSE IF ( J.EQ.9 .AND. K.EQ.3 ) THEN
 1231                 FLONE = .TRUE.
 1232                 CALL W3S2XY ( NSEA, NSEA, NX, NY, CFLXYMAX  , MAPSF, X1 )
 1233 !
 1234 !     Max CFL step for directional propagation
 1235 !
 1236               ELSE IF ( J.EQ.9 .AND. K.EQ.4 ) THEN
 1237                 FLONE = .TRUE.
 1238                 CALL W3S2XY ( NSEA, NSEA, NX, NY, CFLTHMAX , MAPSF, X1 )
 1239 !!
 1240 !! Not yet coded onto control file list (section 6 above)
 1241 !
 1242 !    Near-bottom amplitude
 1243 !
 1244               ELSE IF ( J.EQ.7 .AND. K.EQ.1 ) THEN
 1245                 FLTWO = .TRUE.
 1246                 CALL W3S2XY ( NSEA, NSEA, NX, NY, ABA   , MAPSF, XX )
 1247                 CALL W3S2XY ( NSEA, NSEA, NX, NY, ABD   , MAPSF, XY )
 1248 !
 1249 !     Near-bottom velocity
 1250 !
 1251               ELSE IF ( J.EQ.7 .AND. K.EQ.2 ) THEN
 1252                 FLTWO = .TRUE.
 1253                 CALL W3S2XY ( NSEA, NSEA, NX, NY, UBA   , MAPSF, XX )
 1254                 CALL W3S2XY ( NSEA, NSEA, NX, NY, UBD   , MAPSF, XY )
 1255 !
 1256 !     Radiation stresses
 1257 !
 1258               ELSE IF ( J.EQ.6 .AND. K.EQ.1 ) THEN
 1259                 FLTWO = .TRUE.
 1260                 CALL W3S2XY ( NSEA, NSEA, NX, NY, SXX   , MAPSF, X1 )
 1261                 CALL W3S2XY ( NSEA, NSEA, NX, NY, SYY   , MAPSF, XX )
 1262                 CALL W3S2XY ( NSEA, NSEA, NX, NY, SXY   , MAPSF, XY )
 1263 !
 1264 !     User defined #1
 1265 !
 
 
                                                          gx_outf.F90  page  24
 
 
 1266               ELSE IF ( J.EQ.10 .AND. K.EQ.1 ) THEN
 1267                 FLONE = .TRUE.
 1268                 CALL W3S2XY ( NSEA, NSEA, NX, NY, USERO(:,1)          &
 1269                                                         , MAPSF, X1 )
 1270 !
 1271 !     User defined #2
 1272 !
 1273               ELSE IF ( J.EQ.10 .AND. K.EQ.2 ) THEN
 1274                 FLONE = .TRUE.
 1275                 CALL W3S2XY ( NSEA, NSEA, NX, NY, USERO(:,2)          &
 1276                                                         , MAPSF, X1 )
 1277 !
 1278               ELSE
 1279                 WRITE (NDSE,990) J, K
 1280                 WRITE (NDSE,999)
 1281                 CALL EXTCDE ( 1 )
 1282 !
 1283               END IF
 1284 !
 1285 ! 3   Perform output
 1286 !
 1287 !     3D array fields
 1288 !
 1289             IF ( FLTRI ) THEN
 1290                 WRITE (NDSDAT)                                        &
 1291                       ((X1(IX,IY),IX=IX0,IXN),IY=IY0,IYN)
 1292                 WRITE (NDSDAT)                                        &
 1293                       ((XX(IX,IY),IX=IX0,IXN),IY=IY0,IYN)
 1294                 WRITE (NDSDAT)                                        &
 1295                       ((XY(IX,IY),IX=IX0,IXN),IY=IY0,IYN)
 1296 !
 1297 !     Partitions parameters
 1298 !
 1299               ELSE IF ( FLPRT ) THEN
 1300                 DO I=0, NOSWLL
 1301                   WRITE (NDSDAT)                                      &
 1302                     ((XA(IX,IY,I),IX=IX0,IXN),IY=IY0,IYN)
 1303                   END DO
 1304 !
 1305 !     2D array fields
 1306 !
 1307               ELSE IF ( FLTWO ) THEN
 1308                 WRITE (NDSDAT)                                        &
 1309                       ((XX(IX,IY),IX=IX0,IXN),IY=IY0,IYN)
 1310                 WRITE (NDSDAT)                                        &
 1311                       ((XY(IX,IY),IX=IX0,IXN),IY=IY0,IYN)
 1312 !
 1313 !     Scalars
 1314 !
 1315               ELSE IF ( FLONE ) THEN
 1316                 WRITE (NDSDAT)                                        &
 1317                       ((X1(IX,IY),IX=IX0,IXN),IY=IY0,IYN)
 1318               END IF
 1319 !
 1320 ! ... End of fields loop
 
 
                                                          gx_outf.F90  page  25
 
 
 1321 !
 1322           END IF
 1323         END DO
 1324        END DO
 1325 !
 1326       RETURN
 1327 !
 1328 ! Error escape locations
 1329 !
 1330 ! Formats
 1331 !
 1332   940 FORMAT (1X,I8,3I3.2,2X,4E12.4)
 1333   950 FORMAT (1X,A13,I9.8,I7.6,2(2F8.2,I4),                           &
 1334               1X,A4,F8.4,1X,A10,2I2,1X,A11,I4)
 1335   951 FORMAT (1X,2F10.5,2I8)
 1336 !
 1337   990 FORMAT (/' *** WAVEWATCH III ERROR IN W3EXGO :'/           &
 1338                '     GROUP',I2,' PARAMETER',I3,' NOT LISTED '    )
 1339   999 FORMAT (/' *** WAVEWATCH III ERROR IN GXEXGO :'/                &
 1340                '     PLEASE UPDATE FIELDS !!! '/)
 1341 !
 1342 !/
 1343 !/ End of GXEXGO ----------------------------------------------------- /
 1344 !/
 1345       END SUBROUTINE GXEXGO
 1346 !/
 1347 !/ End of GXOUTF ----------------------------------------------------- /
 1348 !/
 1349       END PROGRAM GXOUTF
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
