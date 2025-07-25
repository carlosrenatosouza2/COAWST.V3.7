                                                         ww3_outp.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3OUTP
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |            J.H. Alves             |
    9 !/                  |             A. Chawla             |
   10 !/                  |            F. Ardhuin             |
   11 !/                  |             E. Rogers             |
   12 !/                  |            T. Campbell            |
   13 !/                  |                        FORTRAN 90 |
   14 !/                  | Last update :         27-Aug-2015 |
   15 !/                  +-----------------------------------+
   16 !/
   17 !/    14-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
   18 !/    21-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
   19 !/    14-Feb-2000 : Exact nonlinear interactions        ( version 2.01 )
   20 !/    09-Jan-2001 : U* bug fix in tabular output        ( version 2.05 )
   21 !/    25-Jan-2001 : Flat grid version                   ( version 2.06 )
   22 !/    02-Feb-2001 : Xnl version 3.0                     ( version 2.07 )
   23 !/    11-Jun-2001 : Clean up                            ( version 2.11 )
   24 !/    11-Oct-2001 : Clean up, X*, Y* in tables          ( version 2.14 )
   25 !/    13-Nov-2002 : Add stress vector                   ( version 3.00 )
   26 !/    27-Nov-2002 : First version of VDIA and MDIA      ( version 3.01 )
   27 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
   28 !/    17-Apr-2006 : Filter for directional spread.      ( version 3.09 )
   29 !/    23-Jun-2006 : Linear input added.                 ( version 3.09 )
   30 !/    28-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   31 !/    03-Jul-2006 : Separate flux modules.              ( version 3.09 )
   32 !/    28-Oct-2006 : Add partitioning option.            ( version 3.10 )
   33 !/    24-Mar-2007 : Add pars for entire spectrum.       ( version 3.11 )
   34 !/    25-Apr-2007 : Battjes-Janssen Sdb added.          ( version 3.11 )
   35 !/                  (J. H. Alves)
   36 !/    08-Aug-2007 : Creation of buoy log file added     ( version 3.12 )
   37 !/                  (switch O14 -- A. Chawla)
   38 !/    09-Oct-2007 : WAM 4+ Sin and Sds added.           ( version 3.13 )
   39 !/                  (F. Ardhuin)
   40 !/    09-Oct-2007 : Experimental Sbs (BS1) added.       ( version 3.13 )
   41 !/                  (F. Ardhuin)
   42 !/    09-Apr-2008 : Adding an additional output for     ( version 3.12 )
   43 !/                  WMO standard (A. Chawla)
   44 !/    29-Apr-2008 : Adjust format partition output.     ( version 3.14 )
   45 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   46 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   47 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   48 !/    04-Mar-2010 : Added partitions bulletin output.   ( version 3.14 )
   49 !/                  (J. H. Alves)
   50 !/    20-Apr-2010 : Fix initialization of USTAR.      ( version 3.14.1 )
   51 !/    16-Jul-2012 : Move GMD (SNL3) and nonlinear filter (SNLS)
   52 !/                  from 3.15 (HLT).                    ( version 4.08 )
   53 !/    23-Aug-2012 : Adding movable bed friction BT4     ( version 4.08 )
   54 !/    26-Dec-2012 : Modified obsolete declarations.     ( version 4.11 )
   55 !/    10-Sep-2013 : Implement second order correction   ( version 4.12 )
 
 
                                                         ww3_outp.F90  page   2
 
 
   56 !/                  (F. Ardhuin)
   57 !/    06-Feb-2014 : Fix header format in part. files.   ( version 4.18 )
   58 !/    27-Aug-2015 : Sice add as additional output       ( version 5.10 )
   59 !/                  (in source terms)
   60 !/    27-Jun-2017 : Expanding WMO table to 2 digits JHA ( version 6.02 )
   61 !/    18-Aug-2018 : S_{ice} IC5 (Q. Liu)                ( version 6.06 )
   62 !/
   63 !/    Copyright 2009-2014 National Weather Service (NWS),
   64 !/       National Oceanic and Atmospheric Administration.  All rights
   65 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   66 !/       No unauthorized use without permission.
   67 !/
   68 !  1. Purpose :
   69 !
   70 !     Post-processing of point output.
   71 !
   72 !  2. Method :
   73 !
   74 !     Data is read from the grid output file out_pnt.ww3 (raw data)
   75 !     and from the file ww3_outp.inp ( NDSI, output requests ).
   76 !     Model definition and raw data files are read using WAVEWATCH III
   77 !     subroutines.
   78 !
   79 !     Output types ITYPE :            Sub-type OTYPE :
   80 !     --------------------           -----------------
   81 !       0 : Check file.
   82 !       1 : Spectra.
   83 !                                      1 : Print plots.
   84 !                                      2 : Table of 1-D spectra
   85 !                                      3 : Transfer file
   86 !       2 : Table of mean wave parameters
   87 !                                      1 : Depth, current,  wind
   88 !                                      2 : Mean wave pars.
   89 !                                      3 : Nondimensional pars. (U*)
   90 !                                      4 : Nondimensional pars. (U10)
   91 !                                      5 : Validation table
   92 !                                      6 : WMO standard output
   93 !       3 : Source terms
   94 !                                      1 : Print plots.
   95 !                                      2 : Table of 1-D S(f).
   96 !                                      3 : Table of 1-D time scales.
   97 !                                      4 : Transfer file.
   98 !
   99 !       4 : Partitioning and bulletins
  100 !                                      1 : Spectral partitions table
  101 !                                      2 : Bulletins ASCII format
  102 !                                      3 : Bulletins CSV format
  103 !                                      4 : Bulletins CSV & ASCII format
  104 !  3. Parameters :
  105 !
  106 !  4. Subroutines used :
  107 !
  108 !      Name      Type  Module   Description
  109 !     ----------------------------------------------------------------
  110 !      W3NMOD    Subr. W3GDATMD Set number of model.
 
 
                                                         ww3_outp.F90  page   3
 
 
  111 !      W3SETG    Subr.   Id.    Point to selected model.
  112 !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
  113 !      W3SETW    Subr.   Id.    Point to selected model for wave data.
  114 !      W3NAUX    Subr. W3ADATMD Set number of model for aux data.
  115 !      W3SETA    Subr.   Id.    Point to selected model for aux data.
  116 !      W3NOUT    Subr. W3ODATMD Set number of model for output.
  117 !      W3SETO    Subr.   Id.    Point to selected model for output.
  118 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
  119 !      STRACE    Subr.   Id.    Subroutine tracing.
  120 !      NEXTLN    Subr.   Id.    Get next line from input filw
  121 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
  122 !      STME21    Subr. W3TIMEMD Convert time to string.
  123 !      TICK21    Subr.   Id.    Advance time.
  124 !      DSEC21    Func.   Id.    Difference between times.
  125 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
  126 !      W3IOPO    Subr. W3IOPOMD Reading/writing raw point output file.
  127 !      W3EXPO    Subr. Internal Execute point output.
  128 !      W3BULL    Subr. W3BULLMD Generate buletins from spectral part.
  129 !     ----------------------------------------------------------------
  130 !
  131 !  5. Called by :
  132 !
  133 !     None, stand-alone program.
  134 !
  135 !  6. Error messages :
  136 !
  137 !     Checks on input, checks in W3IOxx.
  138 !
  139 !  7. Remarks :
  140 !
  141 !     - Tables written to file 'tabNN.ww3', where NN is the
  142 !       unit umber (NDSTAB).
  143 !     - Transfder file written to ww3.yymmddhh.spc with multiple
  144 !       spectra and times in file. yymmddhh relates to first
  145 !       output (NDSTAB).
  146 !     - !/IC1 !/IC2 !/IC3 !/IC4 !/IC5 are not included in dissipation term
  147 !       FIXME: ICE is a dummy variable at the moment
  148 !              Include ice parameters in point output file out_pnt.ww3
  149 !              Ice coupling to SIN, SDS and SIC similar to w3srcemd.ftn
  150 !
  151 !  8. Structure :
  152 !
  153 !     See source code.
  154 !
  155 !  9. Switches :
  156 !
  157 !       !/S    Enable subroutine tracing.
  158 !
  159 !       !/NCO  NCEP NCO modifications for operational implementation.
  160 !
  161 !       !/O14  Buoy log file generation.
  162 !
  163 ! 10. Source code :
  164 !
  165 !/ ------------------------------------------------------------------- /
 
 
                                                         ww3_outp.F90  page   4
 
 
  166       USE CONSTANTS
  167 !/
  168 !     USE W3GDATMD, ONLY: W3NMOD, W3SETG
  169       USE W3WDATMD, ONLY: W3SETW, W3NDAT
  170       USE W3ADATMD, ONLY: W3SETA, W3NAUX
  171       USE W3ODATMD, ONLY: W3SETO, W3NOUT
  172       USE W3IOGRMD, ONLY: W3IOGR
  173       USE W3IOPOMD, ONLY: W3IOPO
  174       USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
  175       USE W3TIMEMD, ONLY: STME21, TICK21, DSEC21
  176 !/
  177       USE W3GDATMD
  178       USE W3WDATMD, ONLY: TIME
  179       USE W3ODATMD, ONLY: NDSE, NDST, NDSO, NOPTS, PTLOC, PTNME,     &
  180                           DPO, WAO, WDO, ASO, CAO, CDO, SPCO, FNMPRE,&
  181                           ICEO, ICEHO, ICEFO, DIMP
  182       USE W3BULLMD, ONLY: NPTAB, NFLD, NPMAX, BHSMIN, BHSDROP, IYY,  &
  183                           HST, TPT, DMT, ASCBLINE, CSVBLINE
  184        USE W3GIG1MD, ONLY: W3ADDIG
  185        USE W3CANOMD, ONLY: W3ADD2NDORDER
  186 !
  187       IMPLICIT NONE
  188 !/
  189 !/ ------------------------------------------------------------------- /
  190 !/ Local parameters
  191 !/
  192       INTEGER                 :: NDSI, NDSM, NDSOP,  NDSTRC, NTRACE,  &
  193                                  IERR, I, TOUT(2), NOUT, TDUM(2),     &
  194                                  NREQ, IPOINT, ITYPE, OTYPE, NDSTAB,  &
  195                                  IOTEST, IK, ITH, IOUT, J, DIMXP,     &
  196                                  NDSBUL, NDSCSV, ICSV, IJ
  197       INTEGER                 :: ISCALE = 0
  198       INTEGER                 :: TIMEV(2)
  199       REAL                    :: DTREQ, SCALE1, SCALE2, DTEST
  200       REAL                    :: M2KM
  201       REAL, ALLOCATABLE       :: XPART(:,:)
  202       LOGICAL                 :: FLFORM, FLSRCE(7)
  203       LOGICAL, ALLOCATABLE    :: FLREQ(:)
  204       CHARACTER               :: COMSTR*1, IDTIME*23, IDDDAY*11,      &
  205                                  TABNME*9, TFNAME*16
  206       CHARACTER(LEN=25)       :: IDSRCE(7)
  207       CHARACTER               :: HSTR*6, HTYPE*3
  208 !/
  209 !/ ------------------------------------------------------------------- /
  210 !/
  211       DATA IDSRCE / 'Spectrum                 ' ,                     &
  212                     'Wind-wave interactions   ' ,                     &
  213                     'Nonlinear interactions   ' ,                     &
  214                     'Dissipation              ' ,                     &
  215                     'Wave-bottom interactions ' ,                     &
  216                     'Wave-ice interactions    ' ,                     &
  217                     'Sum of selected sources  ' /
  218       FLSRCE = .FALSE.
  219 !
  220 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
                                                         ww3_outp.F90  page   5
 
 
  221 ! 1.  IO set-up.
  222 !
  223       CALL W3NMOD ( 1, 6, 6 )
  224       CALL W3SETG ( 1, 6, 6 )
  225       CALL W3NDAT (    6, 6 )
  226       CALL W3SETW ( 1, 6, 6 )
  227       CALL W3NAUX (    6, 6 )
  228       CALL W3SETA ( 1, 6, 6 )
  229       CALL W3NOUT (    6, 6 )
  230       CALL W3SETO ( 1, 6, 6 )
  231 !
  232       NDSI   = 10
  233       NDSM   = 20
  234       NDSOP  = 20
  235       NDSBUL = 0
  236 !
  237       NDSTRC =  6
  238       NTRACE = 10
  239       CALL ITRACE ( NDSTRC, NTRACE )
  240                                     
  241 !
  242       WRITE (NDSO,900)
  243 !
  244       J      = LEN_TRIM(FNMPRE)
  245       OPEN (NDSI,FILE=FNMPRE(:J)//'ww3_outp.inp',STATUS='OLD',        &
  246             ERR=800,IOSTAT=IERR)
  247       READ (NDSI,'(A)',END=801,ERR=802) COMSTR
  248       IF (COMSTR.EQ.' ') COMSTR = '$'
  249       WRITE (NDSO,901) COMSTR
  250 !
  251 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  252 ! 2.  Read model definition file.
  253 !
  254       CALL W3IOGR ( 'READ', NDSM )
  255       WRITE (NDSO,920) GNAME
  256 !
  257       IF ( FLAGLL ) THEN
  258           M2KM = 1.
  259         ELSE
  260           M2KM = 1.E-3
  261         END IF
  262 !
  263       DIMXP  = ((NK+1)/2) * ((NTH-1)/2)
  264       ALLOCATE ( XPART(DIMP,0:DIMXP) )
  265       XPART  = UNDEF
  266 !
  267 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  268 ! 3.  Read general data and first fields from file
  269 !
  270       CALL W3IOPO ( 'READ', NDSOP, IOTEST )
  271 !
  272       WRITE (NDSO,930)
  273       DO I=1, NOPTS
  274         IF ( FLAGLL ) THEN
  275             WRITE (NDSO,931) PTNME(I), M2KM*PTLOC(1,I), M2KM*PTLOC(2,I)
 
 
                                                         ww3_outp.F90  page   6
 
 
  276           ELSE
  277             WRITE (NDSO,932) PTNME(I), M2KM*PTLOC(1,I), M2KM*PTLOC(2,I)
  278           END IF
  279         END DO
  280 !
  281 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  282 ! 4.  Read requests from input file.
  283 !     Output times
  284 !
  285       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  286       READ (NDSI,*,END=801,ERR=802) TOUT, DTREQ, NOUT
  287       DTREQ  = MAX ( 0. , DTREQ )
  288       IF ( DTREQ.EQ.0 ) NOUT = 1
  289       NOUT   = MAX ( 1 , NOUT )
  290 !
  291       CALL STME21 ( TOUT , IDTIME )
  292       WRITE (NDSO,940) IDTIME
  293 !
  294       TDUM   = 0
  295       CALL TICK21 ( TDUM , DTREQ )
  296       CALL STME21 ( TDUM , IDTIME )
  297       IF ( DTREQ .GE. 86400. ) THEN
  298           WRITE (IDDDAY,'(I10,1X)') INT(DTREQ/86400.)
  299         ELSE
  300           IDDDAY = '           '
  301         END IF
  302       IDTIME(1:11) = IDDDAY
  303       IDTIME(21:23) = '   '
  304       WRITE (NDSO,941) IDTIME, NOUT
  305 !
  306 ! ... Output points
  307 !
  308       ALLOCATE ( FLREQ(NOPTS) )
  309       FLREQ = .FALSE.
  310       NREQ   = 0
  311 !
  312       DO I=1, NOPTS
  313         ! reads point index
  314         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  315         READ (NDSI,*,END=801,ERR=802) IPOINT
  316         ! last index
  317         IF (IPOINT .LT. 0) THEN
  318           IF (I.EQ.1) THEN
  319             FLREQ = .TRUE.
  320             NREQ = NOPTS
  321           END IF
  322           EXIT
  323         END IF
  324         ! existing index in out_pnt.ww3
  325         IF ( (IPOINT .GT. 0) .AND. (IPOINT .LE. NOPTS) ) THEN
  326           IF ( .NOT. FLREQ(IPOINT) ) THEN
  327             NREQ = NREQ + 1
  328           END IF
  329           FLREQ(IPOINT) = .TRUE.
  330         END IF
 
 
                                                         ww3_outp.F90  page   7
 
 
  331         ! read the 'end of list' if nopts reached before it
  332         IF ( (IPOINT .GT. 0) .AND. (NREQ .EQ. NOPTS) ) THEN
  333           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  334           READ (NDSI,*,END=801,ERR=802) IPOINT
  335         END IF
  336       END DO
  337       ! check if last point index is -1
  338       IF (IPOINT .NE. -1) THEN
  339         WRITE (NDSE,1007)
  340         CALL EXTCDE ( 47 )
  341       END IF
  342             
  343 !
  344 ! ... Output type
  345 !
  346       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  347       READ (NDSI,*,END=801,ERR=802) ITYPE
  348 !
  349 ! ... ITYPE = 0
  350 !
  351       IF ( ITYPE .EQ. 0 ) THEN
  352 !
  353           WRITE (NDSO,942) ITYPE, 'Checking contents of file'
  354           DO
  355             CALL STME21 ( TIME , IDTIME )
  356             WRITE (NDSO,948) IDTIME
  357             CALL W3IOPO ( 'READ', NDSOP, IOTEST )
  358             IF ( IOTEST .EQ. -1 ) THEN
  359                 WRITE (NDSO,949)
  360                 GOTO 888
  361               END IF
  362             END DO
  363 !
  364 ! ... ITYPE = 1
  365 !
  366         ELSE IF (ITYPE .EQ. 1) THEN
  367           WRITE (NDSO,942) ITYPE, '1-D and/or 2-D spectra'
  368           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  369           READ (NDSI,*,END=801,ERR=802) OTYPE, SCALE1, SCALE2,        &
  370                 NDSTAB, FLFORM
  371           IF (OTYPE .EQ. 1) THEN
  372               WRITE (NDSO,943) 'print plots'
  373               IF ( SCALE1 .LT. 0.  ) THEN
  374                   WRITE (NDSO,1940) '1-D'
  375                 ELSE IF ( SCALE1 .EQ. 0.  ) THEN
  376                   WRITE (NDSO,1941) '1-D'
  377                 ELSE
  378                   WRITE (NDSO,1942) '1-D', SCALE1
  379                 END IF
  380               IF ( SCALE2 .LT. 0.  ) THEN
  381                   WRITE (NDSO,1940) '2-D'
  382                 ELSE IF ( SCALE2 .EQ. 0.  ) THEN
  383                   WRITE (NDSO,1941) '2-D'
  384                 ELSE
  385                   WRITE (NDSO,1942) '2-D', SCALE2
 
 
                                                         ww3_outp.F90  page   8
 
 
  386                 END IF
  387             ELSE IF ( OTYPE .EQ. 2 ) THEN
  388               WRITE (NDSO,943) 'Table of 1-D spectral data'
  389               TABNME = 'tab--.ww3'
  390               IF ( NDSTAB.LE.0 .OR. NDSTAB.GT.99 ) NDSTAB = 51
  391               WRITE ( TABNME(4:5) , '(I2.2)' ) NDSTAB
  392               J      = LEN_TRIM(FNMPRE)
  393               OPEN (NDSTAB,FILE=FNMPRE(:J)//TABNME,ERR=803,IOSTAT=IERR)
  394               WRITE (NDSO,1947) TABNME
  395             ELSE IF ( OTYPE .EQ. 3 ) THEN
  396               TFNAME = 'ww3.--------.spc'
  397               WRITE (TFNAME(5:12),'(I6.6,I2.2)')                      &
  398                    MOD(TOUT(1),1000000), TOUT(2)/10000
  399               WRITE (NDSO,943) 'Transfer file'
  400               IF ( FLFORM ) THEN
  401                   WRITE (NDSO,1943) TFNAME, 'UNFORMATTED'
  402                   J      = LEN_TRIM(FNMPRE)
  403                   OPEN  (NDSTAB,FILE=FNMPRE(:J)//TFNAME,ERR=804,      &
  404                          IOSTAT=IERR,FORM='UNFORMATTED')
  405                   WRITE (NDSTAB) 'WAVEWATCH III SPECTRA',             &
  406                                   NK, NTH, NREQ, GNAME
  407                   WRITE (NDSTAB) (SIG(IK)*TPIINV,IK=1,NK)
  408 !
  409 ! conversion of directions from trignonmetric to nautical (still uses dir>
  410 !
  411                  WRITE (NDSTAB) (MOD(2.5*PI-TH(ITH),TPI),ITH=1,NTH)
  412                                                                    
  413                 ELSE
  414                   WRITE (NDSO,1943) TFNAME, 'FORMATTED'
  415                   J      = LEN_TRIM(FNMPRE)
  416                   OPEN  (NDSTAB,FILE=FNMPRE(:J)//TFNAME,ERR=804,      &
  417                          IOSTAT=IERR,FORM='FORMATTED')
  418                   WRITE (NDSTAB,1944) 'WAVEWATCH III SPECTRA',        &
  419                                        NK, NTH, NREQ, GNAME
  420                   WRITE (NDSTAB,1945) (SIG(IK)*TPIINV,IK=1,NK)
  421                   WRITE (NDSTAB,1946)                                 &
  422                                (MOD(2.5*PI-TH(ITH),TPI),ITH=1,NTH)
  423                 END IF
  424             ELSE
  425               WRITE (NDSE,1011) OTYPE
  426               CALL EXTCDE ( 10 )
  427             END IF
  428 !
  429 ! ... ITYPE = 2
  430 !
  431         ELSE IF (ITYPE .EQ. 2) THEN
  432           WRITE (NDSO,942) ITYPE, 'Table of mean wave parameters'
  433           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  434           READ (NDSI,*,END=801,ERR=802) OTYPE, NDSTAB
  435           TABNME = 'tab--.ww3'
  436           IF ( NDSTAB.LE.0 .OR. NDSTAB.GT.99 ) NDSTAB = 51
  437           WRITE ( TABNME(4:5) , '(I2.2)' ) NDSTAB
  438           J      = LEN_TRIM(FNMPRE)
  439           OPEN (NDSTAB,FILE=FNMPRE(:J)//TABNME,ERR=803,IOSTAT=IERR)
  440           IF ( OTYPE .EQ. 1 ) THEN
 
 
                                                         ww3_outp.F90  page   9
 
 
  441               WRITE (NDSO,2940) 'Depth, current and wind', TABNME
  442             ELSE IF ( OTYPE .EQ. 2 ) THEN
  443               WRITE (NDSO,2940) 'Mean wave parameters', TABNME
  444             ELSE IF ( OTYPE .EQ. 3 ) THEN
  445               WRITE (NDSO,2940) 'Nondimensional parameters (U*)',     &
  446                                  TABNME
  447             ELSE IF ( OTYPE .EQ. 4 ) THEN
  448               WRITE (NDSO,2940) 'Nondimensional parameters (U10)',    &
  449                                  TABNME
  450             ELSE IF ( OTYPE .EQ. 5 ) THEN
  451               WRITE (NDSO,2940) 'Validation parameters', TABNME
  452             ELSE IF ( OTYPE .EQ. 6 ) THEN
  453               WRITE (NDSO,2940) 'WMO standard mean parameters', TABNME
  454             ELSE
  455               WRITE (NDSE,1011) OTYPE
  456               CALL EXTCDE ( 20 )
  457             END IF
  458 !
  459 ! ... ITYPE = 3
  460 !
  461         ELSE IF (ITYPE .EQ. 3) THEN
  462           WRITE (NDSO,942) ITYPE, 'Source terms'
  463           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  464           READ (NDSI,*,END=801,ERR=802) OTYPE, SCALE1, SCALE2,        &
  465                                         NDSTAB, FLSRCE, ISCALE, FLFORM
  466           ISCALE = MAX ( 0 , MIN ( 5 , ISCALE ) )
  467           IF ( OTYPE .EQ. 1 ) THEN
  468               WRITE (NDSO,943) 'Print plots'
  469             ELSE IF ( OTYPE .EQ. 2 ) THEN
  470               IF ( ISCALE .LE. 2) THEN
  471                   WRITE (NDSO,943) 'Tables as a function of freq.'
  472                 ELSE
  473                   WRITE (NDSO,943) 'Tables as a function of f/fp.'
  474                 END IF
  475               IF ( MOD(ISCALE,3) .EQ. 1 ) THEN
  476                   WRITE (NDSO,944) '(nondimensional based on U10)'
  477                 ELSE IF ( MOD(ISCALE,3) .EQ. 2) THEN
  478                   WRITE (NDSO,944) '(nondimensional based on U*)'
  479                 END IF
  480             ELSE IF ( OTYPE .EQ. 3 ) THEN
  481               IF ( ISCALE .LE. 2) THEN
  482                   WRITE (NDSO,943) 'Time scales as a function of freq.'
  483                 ELSE
  484                   WRITE (NDSO,943) 'Time scales as a function of f/fp.'
  485                 END IF
  486               IF ( ISCALE .EQ. 1 ) THEN
  487                   WRITE (NDSO,944) '(nondimensional based on U10)'
  488                 ELSE IF ( ISCALE .EQ. 2) THEN
  489                   WRITE (NDSO,944) '(nondimensional based on U*)'
  490                 END IF
  491             ELSE IF ( OTYPE .EQ. 4 ) THEN
  492               TFNAME = 'ww3.--------.src'
  493               WRITE (TFNAME(5:12),'(I6.6,I2.2)')                      &
  494                    MOD(TOUT(1),1000000), TOUT(2)/10000
  495               WRITE (NDSO,943) 'Transfer file'
 
 
                                                         ww3_outp.F90  page  10
 
 
  496               IF ( FLFORM ) THEN
  497                   WRITE (NDSO,3943) TFNAME, 'UNFORMATTED'
  498                   J      = LEN_TRIM(FNMPRE)
  499                   OPEN  (NDSTAB,FILE=FNMPRE(:J)//TFNAME,ERR=804,      &
  500                          IOSTAT=IERR,FORM='UNFORMATTED')
  501                   WRITE (NDSTAB) 'WAVEWATCH III SOURCES',             &
  502                                   NK, NTH, NREQ, FLSRCE
  503                   WRITE (NDSTAB) (SIG(IK)*TPIINV,IK=1,NK)
  504                   WRITE (NDSTAB) (MOD(2.5*PI-TH(ITH),TPI),ITH=1,NTH)
  505                                                                     
  506                 ELSE
  507                   WRITE (NDSO,3943) TFNAME, 'FORMATTED'
  508                   J      = LEN_TRIM(FNMPRE)
  509                   OPEN  (NDSTAB,FILE=FNMPRE(:J)//TFNAME,ERR=804,      &
  510                          IOSTAT=IERR,FORM='FORMATTED')
  511                   WRITE (NDSTAB,3944) 'WAVEWATCH III SOURCES',        &
  512                                        NK, NTH, NREQ, FLSRCE
  513                   WRITE (NDSTAB,3945) (SIG(IK)*TPIINV,IK=1,NK)
  514                   WRITE (NDSTAB,3946)                                 &
  515                                (MOD(2.5*PI-TH(ITH),TPI),ITH=1,NTH)
  516                 END IF
  517             ELSE
  518               WRITE (NDSE,1011) OTYPE
  519               CALL EXTCDE ( 30 )
  520             END IF
  521 !
  522           DO I=1, 7
  523             IF ( FLSRCE(I) ) WRITE (NDSO,3940) IDSRCE(I)
  524             END DO
  525           WRITE (NDSO,*) ' '
  526 !
  527           IF ( OTYPE .EQ. 1 ) THEN
  528               IF ( SCALE1 .LT. 0.  ) THEN
  529                   WRITE (NDSO,1940) '1-D'
  530                 ELSE IF ( SCALE1 .EQ. 0.  ) THEN
  531                   WRITE (NDSO,1941) '1-D'
  532                 ELSE
  533                   WRITE (NDSO,1942) '1-D', SCALE1
  534                 END IF
  535               IF ( SCALE2 .LT. 0.  ) THEN
  536                   WRITE (NDSO,1940) '2-D'
  537                 ELSE IF ( SCALE2 .EQ. 0.  ) THEN
  538                   WRITE (NDSO,1941) '2-D'
  539                 ELSE
  540                   WRITE (NDSO,1942) '2-D', SCALE2
  541                 END IF
  542             END IF
  543 !
  544           IF ( OTYPE.EQ.2 .OR. OTYPE.EQ.3 ) THEN
  545               TABNME = 'tab--.ww3'
  546               IF ( NDSTAB.LE.0 .OR. NDSTAB.GT.99 ) NDSTAB = 51
  547               WRITE ( TABNME(4:5) , '(I2.2)' ) NDSTAB
  548               J      = LEN_TRIM(FNMPRE)
  549               OPEN (NDSTAB,FILE=FNMPRE(:J)//TABNME,ERR=803,IOSTAT=IERR)
  550               WRITE (NDSO,3941) TABNME
 
 
                                                         ww3_outp.F90  page  11
 
 
  551             END IF
  552 !
  553 ! ... ITYPE = 4
  554 !
  555         ELSE IF (ITYPE .EQ. 4) THEN
  556           WRITE (NDSO,942) ITYPE, 'Spectral partitions or bulletins'
  557           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  558           READ (NDSI,*,END=801,ERR=802) OTYPE, NDSTAB, TIMEV, HTYPE
  559             IF ( OTYPE .EQ. 1 ) THEN
  560               WRITE (NDSO,943) 'Partitioning of spectra'
  561               TABNME = 'tab--.ww3'
  562               IF ( NDSTAB.LE.0 .OR. NDSTAB.GT.99 ) NDSTAB = 51
  563               WRITE ( TABNME(4:5) , '(I2.2)' ) NDSTAB
  564               J      = LEN_TRIM(FNMPRE)
  565               OPEN (NDSTAB,FILE=FNMPRE(:J)//TABNME,ERR=803,IOSTAT=IERR)
  566               WRITE (NDSO,1947) TABNME
  567                                       
  568             ELSEIF ( OTYPE .GE. 2 ) THEN
  569               IF (OTYPE .EQ. 2 .OR. OTYPE .EQ. 4 ) THEN
  570               WRITE (NDSO,943) 'Bulletins, ASCII format'
  571               J      = LEN_TRIM(FNMPRE)
  572               DO IJ = 1,NOPTS
  573                 IF ( COUNT(FLREQ)  .GT. 1 ) THEN
  574 ! ... This version only allows single point output for bulletins
  575                   WRITE (NDSE,1012) OTYPE
  576                   CALL EXTCDE ( 45 )
  577                 ENDIF
  578                 IF (FLREQ(IJ)) THEN
  579                   NDSBUL = NDSTAB + (IJ - 1)
  580                   OPEN(NDSBUL,FILE=TRIM(PTNME(IJ))//'.bull',ERR=803,IOSTA>
  581                   WRITE (NDSO,1947) TRIM(PTNME(IJ))//'.bull'
  582                 ENDIF
  583               ENDDO
  584               ENDIF
  585               IF ( OTYPE .EQ. 3 .OR. OTYPE .EQ. 4 ) THEN
  586               WRITE (NDSO,943) 'Bulletins, CSV format'
  587               J      = LEN_TRIM(FNMPRE)
  588               DO IJ = 1,NOPTS
  589                 IF (FLREQ(IJ)) THEN
  590                   ICSV = 0
  591                   IF ( NDSBUL .GT. 0 ) ICSV = NDSBUL
  592                   NDSCSV = NDSTAB + (IJ - 1) + ICSV
  593                   OPEN(NDSCSV,FILE=TRIM(PTNME(IJ))//'.csv',ERR=803,IOSTAT>
  594                   WRITE (NDSO,1947) TRIM(PTNME(IJ))//'.csv'
  595                 ENDIF
  596               ENDDO
  597               ENDIF
  598             ELSE
  599               WRITE (NDSE,1011) OTYPE
  600               CALL EXTCDE ( 50 )
  601             END IF
  602 !
  603 ! ... ITYPE = ILLEGAL
  604 !
  605         ELSE
 
 
                                                         ww3_outp.F90  page  12
 
 
  606           WRITE (NDSE,1010) ITYPE
  607           CALL EXTCDE ( 1 )
  608 !
  609         END IF
  610 !
  611 ! ... Output of output points
  612 !
  613       WRITE (NDSO,950) NREQ
  614       DO I=1, NOPTS
  615         IF (FLREQ(I)) THEN
  616             IF ( FLAGLL ) THEN
  617                 WRITE (NDSO,951) PTNME(I), M2KM*PTLOC(1,I),   &
  618                                            M2KM*PTLOC(2,I)
  619               ELSE
  620                 WRITE (NDSO,953) PTNME(I), M2KM*PTLOC(1,I),   &
  621                                            M2KM*PTLOC(2,I)
  622               END IF
  623           END IF
  624         END DO
  625 !
  626       IF ( ITYPE.EQ.3 .AND. OTYPE.EQ.4 ) WRITE (NDSO,952)
  627 !
  628 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  629 ! 5.  Time management.
  630 !
  631       IOUT   = 0
  632 !
  633 ! remark: it would be better to write these warnings only if source term
  634 !         output is requested
  635                              
  636       DO
  637         DTEST  = DSEC21 ( TIME , TOUT )
  638         IF ( DTEST .GT. 0. ) THEN
  639             CALL W3IOPO ( 'READ', NDSOP, IOTEST )
  640             IF ( IOTEST .EQ. -1 ) THEN
  641                 WRITE (NDSO,949)
  642                 EXIT
  643               END IF
  644             CYCLE
  645           END IF
  646         IF ( DTEST .LT. 0. ) THEN
  647             CALL TICK21 ( TOUT , DTREQ )
  648             CYCLE
  649           END IF
  650 !
  651         IOUT   = IOUT + 1
  652         CALL STME21 ( TOUT , IDTIME )
  653         IF ( ( ITYPE.EQ.1 .AND. OTYPE.EQ.1 ) .OR.                     &
  654              ( ITYPE.EQ.3 .AND. OTYPE.EQ.1 )                          &
  655              ) WRITE (NDSO,960) IDTIME
  656         CALL W3EXPO
  657         CALL TICK21 ( TOUT , DTREQ )
  658         IF ( IOUT .GE. NOUT ) EXIT
  659         END DO
  660 !
 
 
                                                         ww3_outp.F90  page  13
 
 
  661 ! ... ITYPE=4 & OTYPES=[2,4] requires adding lines at bottom of
  662 !     bulletin output for compatibility with version 2.22
  663 !
  664       IF (ITYPE .EQ. 4 .AND. ( OTYPE .EQ. 2 .OR. OTYPE .EQ. 4 ) ) THEN
  665         DO IJ = 1,NOPTS
  666           IF (FLREQ(IJ)) THEN
  667             NDSBUL = NDSTAB + (IJ - 1)
  668             WRITE(NDSBUL,971)
  669             WRITE(NDSBUL,974) BHSDROP, BHSMIN
  670           ENDIF
  671         ENDDO
  672       ENDIF
  673 !
  674       GOTO 888
  675 !
  676 ! Escape locations read errors :
  677 !
  678   800 CONTINUE
  679       WRITE (NDSE,1000) IERR
  680       CALL EXTCDE ( 40 )
  681 !
  682   801 CONTINUE
  683       WRITE (NDSE,1001)
  684       CALL EXTCDE ( 41 )
  685 !
  686   802 CONTINUE
  687       WRITE (NDSE,1002) IERR
  688       CALL EXTCDE ( 42 )
  689 !
  690   803 CONTINUE
  691       WRITE (NDSE,1003) IERR
  692       CALL EXTCDE ( 43 )
  693 !
  694   804 CONTINUE
  695       WRITE (NDSE,1004) IERR
  696       CALL EXTCDE ( 44 )
  697 !
  698   888 CONTINUE
  699 !
  700       WRITE (NDSO,999)
  701 !
  702 ! Formats
  703 !
  704   900 FORMAT (/15X,'    *** WAVEWATCH III Point output post.***    '/ &
  705                15X,'==============================================='/)
  706   901 FORMAT ( '  Comment character is ''',A,''''/)
  707 !
  708   920 FORMAT ( '  Grid name : ',A/)
  709 !
  710   930 FORMAT ( '  Points in file : '/                                 &
  711                ' ------------------------------------')
  712   931 FORMAT ( '      ',A,2F10.2)
  713   932 FORMAT ( '      ',A,2(F8.1,'E3'))
  714 !
  715   940 FORMAT (/'  Output time data : '/                               &
 
 
                                                         ww3_outp.F90  page  14
 
 
  716                ' --------------------------------------------------'/ &
  717                '      First time         : ',A)
  718   941 FORMAT ( '      Interval           : ',A/                       &
  719                '      Number of requests : ',I6)
  720   942 FORMAT (/'  Output type ',I2,' :'/                              &
  721                ' --------------------------------------------------'/ &
  722                '      ',A/)
  723   943 FORMAT ( '      Subtype   : ',A)
  724   944 FORMAT ( '                  ',A)
  725   948 FORMAT ( '      Data for ',A)
  726   949 FORMAT (/'      End of file reached '/)
  727 !
  728   950 FORMAT (/'  Requested output for',I3,' points : '/              &
  729                ' --------------------------------------------------')
  730   951 FORMAT ( '      ',A,2F10.2)
  731   953 FORMAT ( '      ',A,2(F8.1,'E3'))
  732   952 FORMAT (/'  Output times :'/                                    &
  733                ' --------------------------------------------------')
  734   971 FORMAT (' +-------+-----------+-----------------+',             &
  735              '-----------------+-----------------+----',              &
  736              '-------------+-----------------+--------',              &
  737              '---------+')!
  738   974 FORMAT ( &
  739        75X,'Hst : Total sigificant wave height.'/                     &
  740        75X,'n   : Number of fields with Hs > ',f4.2,                  &
  741                 ' in 2-D spectrum.'/                                  &
  742        75X,'x   : Number of fields with Hs > ',f4.2,                  &
  743                 ' not in table.'/                                     &
  744        75X,'Hs  : Significant wave height of separate wave field.'/   &
  745        75X,'Tp  : Peak period of separate wave field.'/               &
  746        75X,'dir : Mean direction of separate wave field.'/            &
  747        75X,'*   : Wave generation due to local wind probable.')
  748                                                                
  749  1940 FORMAT ( '      ',A,' print plots not requested.')
  750  1941 FORMAT ( '      ',A,' print plots normalized.')
  751  1942 FORMAT ( '      Scale factor ',A,' spectrum : ',E10.3)
  752  1943 FORMAT ( '      File name : ',A,' (',A,')')
  753  1944 FORMAT ('''',A,'''',1X,3I6,1X,'''',A,'''')
  754  1945 FORMAT (8E10.3)
  755  1946 FORMAT (7E11.3)
  756  1947 FORMAT ( '      File name : ',A)
  757 !
  758  2940 FORMAT ( '      Table output : ',A/                             &
  759                '      File name    : ',A)
  760 !
  761  3940 FORMAT ( '                        ',A)
  762  3941 FORMAT ( '      File name : ',A)
  763  3943 FORMAT ( '      File name : ',A,' (',A,')')
  764  3944 FORMAT ('''',A,'''',1X,3I6,6L2)
  765  3945 FORMAT (8E10.3)
  766  3946 FORMAT (7E11.3)
  767 !
  768   960 FORMAT (//'  Output for ',A/                                    &
  769                ' --------------------------------------------------')
  770 !
 
 
                                                         ww3_outp.F90  page  15
 
 
  771   999 FORMAT (/'  End of program '/                                   &
  772                ' ========================================='/          &
  773                '         WAVEWATCH III Point output '/)
  774 !
  775  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUTP : '/               &
  776                '     ERROR IN OPENING INPUT FILE'/                    &
  777                '     IOSTAT =',I5/)
  778 !
  779  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUTP : '/               &
  780                '     PREMATURE END OF INPUT FILE'/)
  781 !
  782  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUTP : '/               &
  783                '     ERROR IN READING FROM INPUT FILE'/               &
  784                '     IOSTAT =',I5/)
  785 !
  786  1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUTP : '/               &
  787                '     ERROR IN OPENING TABLE FILE'/                    &
  788                '     IOSTAT =',I5/)
  789 !
  790  1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUTP : '/               &
  791                '     ERROR IN OPENING IDL FILE'/                      &
  792                '     IOSTAT =',I5/)
  793 !
  794  1007 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUTP : '/              &
  795                '     ERROR IN READING FROM INPUT FILE'/               &
  796                '     LAST POINT INDEX IS NOT -1'/                    &
  797                '     OR TOO MANY POINT INDEXES DEFINED'/)
  798 !
  799  1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUTP : '/               &
  800                '     ILLEGAL TYPE, ITYPE =',I4/)
  801 !
  802  1011 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUTP : '/               &
  803                '     ILLEGAL TYPE, OTYPE =',I4/)
  804 !
  805  1012 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUTP : '/               &
  806                '     MULTIPLE OUTPUT POINTS DEFINED, ITYPE =',I4,/    &
  807                '     ONLY SINGLE POINT ALLOWED IN THIS VERSION'/)
  808 !
  809 !/
  810 !/ Internal subroutine W3EXPO ---------------------------------------- /
  811 !/
  812       CONTAINS
  813 !/ ------------------------------------------------------------------- /
  814       SUBROUTINE W3EXPO
  815 !/
  816 !/                  +-----------------------------------+
  817 !/                  | WAVEWATCH III           NOAA/NCEP |
  818 !/                  |           H. L. Tolman            |
  819 !/                  |            J.H. Alves             |
  820 !/                  |            F. Ardhuin             |
  821 !/                  |             A. Chawla             |
  822 !/                  |                        FORTRAN 90 |
  823 !/                  | Last update :         06-Feb-2014 |
  824 !/                  +-----------------------------------+
  825 !/
 
 
                                                         ww3_outp.F90  page  16
 
 
  826 !/    08-Jun-1999 : Final FORTRAN 77                    ( version 1.18 )
  827 !/    21-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
  828 !/                  Massive changes to logistics
  829 !/    09-Jan-2001 : U* bug fix in tabular output        ( version 2.05 )
  830 !/    25-Jan-2001 : Flat grid version                   ( version 2.06 )
  831 !/    02-Feb-2001 : Xnl version 3.0                     ( version 2.07 )
  832 !/    11-Jun-2001 : Clean up                            ( version 2.11 )
  833 !/    11-Oct-2001 : Clean up, X*, Y* in tables          ( version 2.14 )
  834 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
  835 !/    17-Apr-2006 : Filter for directional spread.      ( version 3.09 )
  836 !/    23-Jun-2006 : Linear input added.                 ( version 3.09 )
  837 !/    03-Jul-2006 : Separate flux modules.              ( version 3.09 )
  838 !/    28-Oct-2006 : Add partitioning option.            ( version 3.10 )
  839 !/    24-Mar-2007 : Add pars for entire spectrum.       ( version 3.11 )
  840 !/    25-Apr-2007 : Battjes-Janssen Sdb added.          ( version 3.11 )
  841 !/                  (J. H. Alves)
  842 !/    09-Oct-2007 : WAM 4+ Sin and Sds added.           ( version 3.13 )
  843 !/                  (F. Ardhuin)
  844 !/    09-Oct-2007 : Experimental Sbs (BS1) added.       ( version 3.13 )
  845 !/                  (F. Ardhuin)
  846 !/    09-Apr-2008 : Adding an additional output for     ( version 3.12 )
  847 !/                  WMO standard (A. Chawla)
  848 !/    29-Apr-2008 : Adjust format partition output.     ( version 3.14 )
  849 !/    01-Jul-2011 : Adding BT4                          ( version 4.01 )
  850 !/    16-Jul-2012 : Move GMD (SNL3) and nonlinear filter (SNLS)
  851 !/                  from 3.15 (HLT).                    ( version 4.08 )
  852 !/    26-Dec-2012 : Modified obsolete declarations.     ( version 4.11 )
  853 !/    06-Feb-2014 : Fix header format in part. files.   ( version 4.18 )
  854 !/
  855 !  1. Purpose :
  856 !
  857 !     Perform actual point output.
  858 !
  859 !  3. Parameters :
  860 !
  861 !  4. Subroutines used :
  862 !
  863 !      Name      Type  Module   Description
  864 !     ----------------------------------------------------------------
  865 !      W3SPRn    Subr. W3SRCnMD Mean wave parameters for use in
  866 !                               source terms.
  867 !      W3FLXn    Subr. W3FLXnMD Flux/stress computation.
  868 !      W3SLNn    Subr. W3SLNnMD Linear input.
  869 !      W3SINn    Subr. W3SRCnMD Input source term.
  870 !      W3SDSn    Subr. W3SRCnMD Whitecapping source term
  871 !      W3SNLn    Subr. W3SNLnMD Nonlinear interactions.
  872 !      W3SBTn    Subr. W3SBTnMD Bottom friction source term.
  873 !      W3SDBn    Subr. W3SBTnMD Depth induced breaking source term.
  874 !      W3STRn    Subr. W3STRnMD Triad interaction source term.
  875 !      W3SBSn    Subr. W3SBSnMD Bottom scattering source term.
  876 !      W3SXXn    Subr. W3SXXnMD Unclassified source term.
  877 !      W3PART    Sunr. W3PARTMD Spectral partitioning routine.
  878 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  879 !      STME21    Subr. W3TIMEMD Convert time to string.
  880 !      PRT1DS    Subr. W3ARRYMD Print plot of 1-D spectrum.
 
 
                                                         ww3_outp.F90  page  17
 
 
  881 !      PRT1DM    Subr.   Id.    Print plot of several 1-D spectra.
  882 !      PRT2DS    Subr.   Id.    Print plot of 2-D spectrum.
  883 !      WAVNU1    Subr. W3DISPMD Solve dispersion relation.
  884 !     ----------------------------------------------------------------
  885 !
  886 !  5. Called by :
  887 !
  888 !     Main program in which it is contained,
  889 !
  890 !  6. Error messages :
  891 !
  892 !     None.
  893 !
  894 !  7. Remarks :
  895 !
  896 !     - Spectra are relative frequency energy spectra.
  897 !     - Note that arrays CX and CY of the main program now contain
  898 !       the absolute current speed and direction respectively.
  899 !
  900 !     - BT8&9 issues :
  901 !
  902 !       Q: What is the problem?
  903 !       A: Point output of Sbot with BT8 or BT9 is not presently
  904 !          supported.
  905 !
  906 !       Q: What can a user do now?
  907 !       A: When using BT8 or BT9 with ITYPE=3 , the
  908 !          user should set the 5th T/F value in ww3_outp.inp for
  909 !          ITYPE=3 to "F" like so :
  910 !          2  1. 1. 51   T T T T F T  0  F
  911 !        $               ^ ^ ^ ^ ^ ^ Sum of selected sources
  912 !        $               | | | | ^ Wave-bottom interactions
  913 !        $               | | | ^ Dissipation
  914 !        $               | | ^ Nonlinear interactions
  915 !        $               | ^ Wind-wave interactions
  916 !        $               ^ Spectrum
  917 !          If the user really need this source function, he/she
  918 !          needs to add test output to the mud subroutine
  919 !          directly
  920 !
  921 !       Q: Why doesn't this functionality exist?
  922 !       A: The Sbot source function in ww3_outp was originally written
  923 !          with the case of BT1 in mind. BT1 uses a uniform friction
  924 !          factor, so it does not need any special variable for the
  925 !          local friction factor. BT8 and BT9 allow non-uniform mud
  926 !          variables (thickness, density, viscosity) and the mud
  927 !          subroutines are written with ww3_shel in mind, where the
  928 !          source function is calculated on the computational grid
  929 !          point IX IY.
  930                        
  931 !       Q: How can we add this functionality?
  932 !       A: To fix it, we would need to :
  933 !          1) interpolate the mud variables from the computational
  934 !             grid point IX IY to the output points (this is already
  935 !             done now for wind, for example) (the same should probably
 
 
                                                         ww3_outp.F90  page  18
 
 
  936 !             be done for the ice properties also) This would be done
  937 !             in w3iopomd.ftn, analogous to what is done now for the
  938 !             wind variable WAO.
  939 !          2) manage the arrays for the new variables (mud and ice
  940 !             properties on the output points) This would be done in
  941 !             w3odatmd.ftn, again analogous to what is done now for the
  942 !             wind variable WAO.
  943 !          3) change the mud routines so that they take the local mud
  944 !             parameters through the subroutine arguments rather than
  945 !             taking IX IY as subroutine arguments. This would allow
  946 !             flexibility to call the mud routine from ww3_shel or
  947 !             ww3_outp (instead of just ww3_shel as is the case now).
  948 !
  949 !  8. Structure :
  950 !
  951 !     See source code.
  952 !
  953 !  9. Switches :
  954 !
  955 !       !/S      Enable subroutine tracing.
  956 !       !/T      Enable test output.
  957 !
  958 !       !/FLXx   Flux/stress computation.
  959 !       !/LNx    Linear input package
  960 !       !/STx    Source term package
  961 !       !/NLx    Nonlinear interaction package
  962 !       !/BTx    Bottom friction package
  963 !       !/ICx    S_ice source term package
  964 !       !/DBx    Depth-induced breaking package
  965 !       !/TRx    Triad interaction package
  966 !       !/BSx    Bottom scattering package
  967 !       !/XXx    Arbitrary adittional source term package
  968 !
  969 !       !/STAB2  Stability correction for !/ST2
  970 !
  971 ! 10. Source code :
  972 !
  973 !/ ------------------------------------------------------------------- /
  974       USE W3SLN1MD
  975       USE W3SRC4MD, ONLY : W3SPR4, W3SIN4, W3SDS4
  976       USE W3SNL1MD
  977       USE W3PARTMD, ONLY: W3PART
  978       USE W3DISPMD, ONLY: WAVNU1, LIU_FORWARD_DISPERSION
  979 !/
  980       USE W3ARRYMD, ONLY: PRT1DS, PRT2DS, PRT1DM
  981       USE W3DISPMD, ONLY: NAR1D, DFAC, N1MAX, ECG1, EWN1, DSIE
  982       USE W3BULLMD, ONLY: W3BULL
  983 !/
  984 !/ ------------------------------------------------------------------- /
  985 !/ Local parameters
  986 !/
  987       INTEGER                 :: J, I1, I2, ISP, IKM, IKL, IKH, ITH,  &
  988                                  IK, IH, IM, IS, IYR, IMTH, IDY, ITT, &
  989                                  I, NPART, IP, IX, IY, ISEA
  990       INTEGER, SAVE           :: IPASS  = 0
 
 
                                                         ww3_outp.F90  page  19
 
 
  991       REAL                    :: DEPTH, SQRTH, CDIR, SIX, R1, R2,     &
  992                                  UDIR, UDIRR, UABS, XL, XH, XL2, XH2, &
  993                                  ET, EWN, ETR, ETX, ETY, EBND, EBX,   &
  994                                  EBY, HSIG, WLEN, TMEAN, THMEAN,      &
  995                                  THSPRD, EMAX, EL, EH, DENOM, FP, THP,&
  996                                  SPP, CD, USTAR, FACTOR, UNORM, ESTAR,&
  997                                  FPSTAR, FACF, FACE, FACS, HMAT, WNA, &
  998                                  XYZ, AGE1, AFR, AGE2, FACT, XSTAR,   &
  999                                  YSTAR, FHIGH, ZWND, Z0, USTD, EMEAN, &
 1000                                  FMEAN, WNMEAN, UDIRCA, X, Y, CHARN,  &
 1001                                  M2KM, ICEF, ICEDMAX, ICETHICK,       &
 1002                                  ICECON
 1003       REAL                    :: AMAX, FMEANS, FMEANWS, TAUWX, TAUWY, &
 1004                                  TAUWNX, TAUWNY, FMEAN1, WHITECAP(1:4), D>
 1005            REAL                    :: ICE
 1006       REAL, SAVE              :: HSMIN  = 0.05
 1007       REAL                    :: WN(NK), CG(NK), R(NK)
 1008       REAL                    :: E(NK,NTH), E1(NK), APM(NK),           &
 1009                                  THBND(NK), SPBND(NK), A(NTH,NK),      &
 1010                                  WN2(NTH,NK)
 1011       REAL                    :: DIA(NTH,NK), SWN(NK,NTH), SNL(NK,NTH),&
 1012                                  SDS(NK,NTH), SBT(NK,NTH), SIS(NK,NTH),&
 1013                                  STT(NK,NTH), DIA2(NTH,NK)
 1014       REAL                    :: XLN(NTH,NK), XIN(NTH,NK), XNL(NTH,NK),&
 1015                                  XTR(NTH,NK), XDS(NTH,NK), XDB(NTH,NK),&
 1016                                  XBT(NTH,NK), XBS(NTH,NK), XXX(NTH,NK),&
 1017                                  XIS(NTH,NK), XWL(NTH,NK)
 1018       REAL                    :: SIN1(NK), SNL1(NK), SDS1(NK),         &
 1019                                  SBT1(NK), STT1(NK), SIS1(NK),         &
 1020                                  E1ALL(NK,6)
 1021       LOGICAL                 :: LBREAK
 1022       LOGICAL                 :: LLWS(NSPEC)
 1023       REAL                    :: LAMBDA(NSPEC)
 1024       CHARACTER               :: DTME21*23
 1025       CHARACTER(LEN=4)         VAR1(6)
 1026       CHARACTER(LEN=1)         IDLAT, IDLON
 1027       CHARACTER(LEN=100)       BT8MSG
 1028 !
 1029       DATA VAR1   / 'Sin ' , 'Snl ', 'Sds ' , 'Sbt ' , 'Sice', 'Stot' /
 1030 !/
 1031 !/ ------------------------------------------------------------------- /
 1032 !/
 1033 ! 1. Initialisations
 1034 !
 1035       IF ( FLAGLL ) THEN
 1036           M2KM   = 1.
 1037         ELSE
 1038           M2KM   = 1.E-3
 1039         END IF
 1040 !
 1041       XL     = 1./XFR - 1.
 1042       XH     =  XFR - 1.
 1043       XL2    = XL**2
 1044       XH2    = XH**2
 1045       IPASS  = IPASS + 1
 
 
                                                         ww3_outp.F90  page  20
 
 
 1046 !
 1047       IF ( ITYPE .EQ. 3 ) THEN
 1048           XLN = 0.
 1049           XIN = 0.
 1050           XNL = 0.
 1051           XTR = 0.
 1052           XDS = 0.
 1053           XDB = 0.
 1054           XBT = 0.
 1055           XBS = 0.
 1056           XWL = 0.
 1057           XXX = 0.
 1058           XIS = 0.
 1059         END IF
 1060 !
 1061 !     Output of time
 1062 !
 1063       IF (  ( ITYPE.EQ.1 .AND. OTYPE.EQ.3 ) .OR.                      &
 1064             ( ITYPE.EQ.3 .AND. OTYPE.EQ.4 ) ) THEN
 1065           IF ( FLFORM ) THEN
 1066               WRITE (NDSTAB) TIME
 1067             ELSE
 1068               WRITE (NDSTAB,900) TIME
 1069             END IF
 1070         END IF
 1071 !
 1072       IF (ITYPE.EQ.2) THEN
 1073           IF ( NREQ.EQ.1 .AND. IPASS.EQ.1 ) THEN
 1074               IF ( OTYPE.EQ.1 ) THEN
 1075                   WRITE (NDSTAB,1901)
 1076                 ELSE IF ( OTYPE.EQ.2 ) THEN
 1077                   WRITE (NDSTAB,1902)
 1078                 ELSE IF ( OTYPE.EQ.3 ) THEN
 1079                   WRITE (NDSTAB,1903)
 1080                 ELSE IF ( OTYPE.EQ.4 ) THEN
 1081                   WRITE (NDSTAB,1904)
 1082                 ELSE IF ( OTYPE.EQ.5 ) THEN
 1083                   WRITE (NDSTAB,1905)
 1084                 ELSE IF ( OTYPE.EQ.6 ) THEN
 1085                   WRITE (NDSTAB,1906)
 1086                 END IF
 1087             END IF
 1088           IF ( NREQ.NE.1 ) THEN
 1089               CALL STME21 ( TIME , DTME21 )
 1090               IF ( IPASS .NE. 1 ) WRITE (NDSTAB,1910)
 1091               IF ( OTYPE.EQ.1 ) THEN
 1092                   IF ( FLAGLL ) THEN
 1093                       WRITE (NDSTAB,1911) DTME21
 1094                     ELSE
 1095                       WRITE (NDSTAB,1711) DTME21
 1096                     END IF
 1097                 ELSE IF ( OTYPE.EQ.2 ) THEN
 1098                   IF ( FLAGLL ) THEN
 1099                       WRITE (NDSTAB,1912) DTME21
 1100                     ELSE
 
 
                                                         ww3_outp.F90  page  21
 
 
 1101                       WRITE (NDSTAB,1712) DTME21
 1102                     END IF
 1103                 ELSE IF ( OTYPE.EQ.3 ) THEN
 1104                   WRITE (NDSTAB,1913) DTME21
 1105                 ELSE IF ( OTYPE.EQ.4 ) THEN
 1106                   WRITE (NDSTAB,1914) DTME21
 1107                 ELSE IF ( OTYPE.EQ.5 ) THEN
 1108                   IF ( FLAGLL ) THEN
 1109                       WRITE (NDSTAB,1915) DTME21
 1110                     ELSE
 1111                       WRITE (NDSTAB,1715) DTME21
 1112                     END IF
 1113                 ELSE IF ( OTYPE.EQ.6 ) THEN
 1114                   IF ( FLAGLL ) THEN
 1115                       WRITE (NDSTAB,1916) DTME21
 1116                     ELSE
 1117                       WRITE (NDSTAB,1716) DTME21
 1118                     END IF
 1119                 END IF
 1120             END IF
 1121         END IF
 1122 !
 1123       IF (ITYPE.EQ.3) THEN
 1124             IF ( OTYPE .EQ. 4 ) THEN
 1125               CALL STME21 ( TIME , DTME21 )
 1126               WRITE (NDSO,905) DTME21
 1127             END IF
 1128         END IF
 1129 !
 1130 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 1131 !     Loop over output points.
 1132 !
 1133       DO J=1, NOPTS
 1134         IF ( FLREQ(J) ) THEN
 1135 !
 1136 ! 2. Calculate grid parameters using and inlined version of WAVNU1.
 1137 !
 1138             DEPTH    = MAX ( DMIN, DPO(J) )
 1139             SQRTH    = SQRT ( DEPTH )
 1140             UDIR     = MOD ( 270. - WDO(J)*RADE , 360. )
 1141             UDIRCA   = WDO(J)*RADE
 1142             UDIRR    = WDO(J)
 1143             UABS     = MAX ( 0.001 , WAO(J) )
 1144             CDIR     = MOD ( 270. - CDO(J)*RADE , 360. )
 1145             ICEDMAX  = MAX ( 0., ICEFO(J))
 1146             ICEF     = ICEDMAX
 1147             ICETHICK = MAX (0., ICEHO(J))
 1148             ICECON   = MAX (0., ICEO(J))
 1149 !
 1150             DO IK=1, NK
 1151               SIX    = SIG(IK) * SQRTH
 1152               I1     = INT(SIX/DSIE)
 1153               IF (I1.LE.N1MAX) THEN
 1154                   I2 = I1 + 1
 1155                   R1 = SIX/DSIE - REAL(I1)
 
 
                                                         ww3_outp.F90  page  22
 
 
 1156                   R2 = 1. - R1
 1157                   WN(IK) = ( R2*EWN1(I1) + R1*EWN1(I2) ) / DEPTH
 1158                   CG(IK) = ( R2*ECG1(I1) + R1*ECG1(I2) ) * SQRTH
 1159                 ELSE
 1160                   WN(IK) = SIG(IK)*SIG(IK)/GRAV
 1161                   CG(IK) = 0.5 * GRAV / SIG(IK)
 1162                 END IF
 1163 !
 1164               END DO
 1165                     
 1166 !
 1167 ! Computes 2nd order spectrum
 1168 !
 1169       IF (IGPARS(2).EQ.1) THEN
 1170         IF(IGPARS(1).EQ.1) THEN
 1171           CALL W3ADDIG(SPCO(:,J),DPO(J),WN,CG,0)
 1172         ELSE
 1173           CALL W3ADD2NDORDER(SPCO(:,J),DPO(J),WN,CG,0)
 1174           END IF
 1175         END IF
 1176 !
 1177 ! 3.  Prepare spectra etc.
 1178 ! 3.a Mean wave parameters.
 1179 !
 1180             ET     = 0.
 1181             EWN    = 0.
 1182             ETR    = 0.
 1183             ETX    = 0.
 1184             ETY    = 0.
 1185             DO IK=1, NK
 1186               EBND   = 0.
 1187               EBX    = 0.
 1188               EBY    = 0.
 1189               DO ITH=1, NTH
 1190                 ISP    = ITH + (IK-1)*NTH
 1191                 E(IK,ITH) = SPCO(ISP,J)
 1192                 EBND   = EBND + SPCO(ISP,J)
 1193                 EBX    = EBX  + SPCO(ISP,J)*ECOS(ITH)
 1194                 EBY    = EBY  + SPCO(ISP,J)*ESIN(ITH)
 1195                 END DO
 1196               E1(IK) = EBND * DTH
 1197               APM(IK)= E1(IK) / ( TPI * GRAV**2 / SIG(IK)**5  )
 1198               IF ( E1(IK) .GT. 1.E-5) THEN
 1199                   THBND(IK) = MOD(630.- RADE*ATAN2(EBY,EBX),360.)
 1200                   SPBND(IK) = RADE * SQRT ( MAX ( 0. , 2.*( 1. -      &
 1201                     SQRT( MAX(0.,(EBX**2+EBY**2)/EBND**2) ) ) ) )
 1202                 ELSE
 1203                   THBND(IK) = -999.9
 1204                   SPBND(IK) = -999.9
 1205                 END IF
 1206               EBND   = E1(IK) * DSII(IK) * TPIINV
 1207               ET     = ET  + EBND
 1208               EWN    = EWN + EBND / WN(IK)
 1209               ETR    = ETR + EBND / SIG(IK)
 1210               ETX    = ETX + EBX * DSII(IK)
 
 
                                                         ww3_outp.F90  page  23
 
 
 1211               ETY    = ETY + EBY * DSII(IK)
 1212               END DO
 1213 !
 1214 ! tail factors for radian action etc ...!
 1215 !
 1216             EBND   = E1(NK) * TPIINV / ( SIG(NK) * DTH )
 1217             ET     = ET  + FTE *EBND
 1218             EWN    = EWN + FTWL*EBND
 1219             ETR    = ETR + FTTR*EBND
 1220             ETX    = DTH*ETX*TPIINV + FTE*EBX*TPIINV/SIG(NK)
 1221             ETY    = DTH*ETY*TPIINV + FTE*EBY*TPIINV/SIG(NK)
 1222 !
 1223             HSIG   = 4. * SQRT ( MAX(0.,ET) )
 1224             IF ( HSIG .GT. HSMIN ) THEN
 1225                 WLEN   = EWN / ET * TPI
 1226                 TMEAN  = ETR / ET * TPI
 1227                 THMEAN = MOD ( 630. - RADE*ATAN2(ETY,ETX) , 360. )
 1228                 THSPRD = RADE * SQRT ( MAX ( 0. , 2.*( 1. - SQRT(     &
 1229                            MAX(0.,(ETX**2+ETY**2)/ET**2) ) ) ) )
 1230                 IF ( THSPRD .LT. 0.01*RADE*DTH ) THSPRD = 0.
 1231               ELSE
 1232                 WLEN   = 0.
 1233                 TMEAN  = 0.
 1234                 THMEAN = 0.
 1235                 THSPRD = 0.
 1236                 DO IK=1, NK
 1237                   E1(IK) = 0.
 1238                   DO ITH=1, NTH
 1239                     E(IK,ITH) = 0.
 1240                   END DO
 1241                 END DO
 1242               END IF
 1243 !
 1244 ! peak frequency
 1245 !
 1246             EMAX   = E1(NK)
 1247             IKM    = NK
 1248 !
 1249             DO IK=NK-1, 1, -1
 1250               IF ( E1(IK) .GT. EMAX ) THEN
 1251                   EMAX   = E1(IK)
 1252                   IKM    = IK
 1253                 END IF
 1254               END DO
 1255 !
 1256             IKL    = MAX (  1 , IKM-1 )
 1257             IKH    = MIN ( NK , IKM+1 )
 1258             EL     = E1(IKL) - E1(IKM)
 1259             EH     = E1(IKH) - E1(IKM)
 1260             DENOM  = XL*EH - XH*EL
 1261 !
 1262             IF ( HSIG .GE. HSMIN ) THEN
 1263                 FP     = SIG(IKM) * ( 1. + 0.5 * ( XL2*EH - XH2*EL )  &
 1264                             / SIGN ( MAX(ABS(DENOM),1.E-15) , DENOM ) )
 1265                 THP    = THBND(IKM)
 
 
                                                         ww3_outp.F90  page  24
 
 
 1266                 SPP    = SPBND(IKM)
 1267                 IF ( SPP .LT. 0.01*RADE*DTH ) SPP = 0.
 1268               ELSE
 1269                 FP     = 0.
 1270                 THP    = 0.
 1271                 SPP    = 0.
 1272               END IF
 1273 !
 1274 ! spectral partitioning
 1275 !
 1276             IF ( ITYPE.EQ.4 ) CALL W3PART                              &
 1277                ( E, UABS, UDIRCA, DEPTH, WN, NPART, XPART, DIMXP )
 1278 !
 1279 ! nondimensional parameters
 1280 !
 1281             IF ( ( ITYPE.EQ.2 .AND. (OTYPE.EQ.3.OR.OTYPE.EQ.4) ) .OR. &
 1282                  ( ITYPE.EQ.1 .AND. (OTYPE.EQ.2) ) ) THEN
 1283 !
 1284                 DO IK=1, NK
 1285                   FACTOR = TPIINV * CG(IK) / SIG(IK)
 1286                   DO ITH=1, NTH
 1287                     ISP    = ITH + (IK-1)*NTH
 1288                     A(ITH,IK)   = FACTOR * SPCO(ISP,J)
 1289                     WN2(ITH,IK) = WN(IK)
 1290                     END DO
 1291                   END DO
 1292 !
 1293                 LLWS(:)  = .TRUE.
 1294                 ZWND   = ZZWND
 1295                 TAUWX  = 0.
 1296                 TAUWY  = 0.
 1297                 USTAR  = 1.
 1298 !
 1299                 CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN,  FMEAN1,        &
 1300                              WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,&
 1301                              TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, >
 1302 !
 1303                 DO ITT=1, 3
 1304                   IX=1
 1305                   IY=1
 1306                   CALL W3SIN4 ( A, CG, WN2, UABS, USTAR, DAIR/DWAT,&
 1307                                ASO(J), UDIRR, Z0, CD, TAUWX, TAUWY,&
 1308                                TAUWNX, TAUWNY, XIN, DIA, LLWS, IX, IY, LA>
 1309                   CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN,  FMEAN1,      &
 1310                              WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,&
 1311                              TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, >
 1312                   END DO
 1313 !
 1314 ! Add alternative flux calculations here as part of !/ST2 option ....
 1315 ! Also add before actual source term calculation !!!
 1316 !
 1317               IF ( WAO(J) .LT. 0.01 ) THEN
 1318                   UNORM  = 0.
 1319                   ESTAR  = 0.
 1320                   FPSTAR = 0.
 
 
                                                         ww3_outp.F90  page  25
 
 
 1321                 ELSE
 1322                   IF ( OTYPE.EQ.3 ) THEN
 1323                       UNORM  = USTAR
 1324                     ELSE
 1325                       UNORM  = WAO(J)
 1326                     END IF
 1327                   ESTAR  = ET * GRAV**2 / UNORM**4
 1328                   FPSTAR = FP * TPIINV * UNORM / GRAV
 1329                   XSTAR  = PTLOC(1,J) * GRAV / UNORM**2
 1330                   YSTAR  = PTLOC(2,J) * GRAV / UNORM**2
 1331                   IF ( FLAGLL ) THEN
 1332                       XSTAR  = XSTAR * DERA * RADIUS &
 1333                              * COS(PTLOC(2,J)*DERA)
 1334                       YSTAR  = YSTAR * DERA * RADIUS
 1335                     END IF
 1336                 END IF
 1337 !
 1338               END IF
 1339 !
 1340 ! 3.4 source terms
 1341 !
 1342             IF ( ITYPE.EQ.3 ) THEN
 1343 !
 1344                 DO IK=1, NK
 1345                   FACTOR = TPIINV * CG(IK) / SIG(IK)
 1346                   DO ITH=1, NTH
 1347                     A(ITH,IK)   = FACTOR * SPCO(ITH+(IK-1)*NTH,J)
 1348                     WN2(ITH,IK) = WN(IK)
 1349                     END DO
 1350                   END DO
 1351 !
 1352                 ZWND   = ZZWND
 1353                 USTAR  = 0.
 1354                 USTD   = 0.
 1355                 TAUWX  = 0.
 1356                 TAUWY  = 0.
 1357 !
 1358                 CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN,  FMEAN1,        &
 1359                              WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,&
 1360                              TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, >
 1361 !
 1362                 DO ITT=1, 3
 1363                 CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN, FMEAN1,        &
 1364                              WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,&
 1365                              TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, >
 1366                   CALL W3SIN4 ( A, CG, WN2, UABS, USTAR, DAIR/DWAT,&
 1367                                 ASO(J), UDIRR, Z0, CD,TAUWX, TAUWY, &
 1368                                 TAUWNX, TAUWNY, XIN, DIA, LLWS, IX, IY, L>
 1369                   END DO
 1370 !
 1371                 IF ( FLSRCE(2) ) THEN
 1372                     CALL W3SLN1 (WN, FHIGH, USTAR, UDIRR, XLN )
 1373 !
 1374                     CALL W3SIN4 ( A, CG, WN2, UABS, USTAR,       &
 1375                                   DAIR/DWAT, ASO(J), UDIRR,      &
 
 
                                                         ww3_outp.F90  page  26
 
 
 1376                             Z0, CD, TAUWX, TAUWY,TAUWNX, TAUWNY, &
 1377                             XIN, DIA, LLWS, IX, IY, LAMBDA )
 1378                   END IF
 1379                 IF ( FLSRCE(3) ) THEN
 1380                     CALL W3SNL1 ( A, CG, WNMEAN*DEPTH,  XNL, DIA )
 1381 !
 1382                   END IF
 1383                 IF ( FLSRCE(4) ) THEN
 1384                     CALL W3SDS4 ( A, WN, CG,  USTAR, USTD, DEPTH, XDS, &
 1385                                   DIA, IX, IY, LAMBDA, WHITECAP, DLWMEAN )
 1386 !
 1387                   END IF
 1388                 IF ( FLSRCE(5) ) THEN
 1389                                      
 1390                                      
 1391                  BT8MSG='ww3_outp: ITYPE=3 with BT8 or BT9: Sbot out'//&
 1392                         'put is not yet supported. Use "F" for the 5'//&
 1393                         'th T/F flag.'
 1394                                       
 1395 ! For info on this issue, see : "BT8&9 issues" in "Remarks" section above.
 1396                                                                           
 1397 !...broken....!/BT8                    CALL W3SBT8 ( A, DEPTH, XBT, DIA, >
 1398 !...broken....!/BT9                    CALL W3SBT9 ( A, DEPTH, XBT, DIA, >
 1399                                                                           
 1400                                                                           
 1401 !
 1402                   END IF
 1403 !
 1404                 IF ( FLSRCE(6) ) THEN
 1405 !
 1406                   END IF
 1407 !
 1408                 IF ( ISCALE.EQ.0 .OR. ISCALE.EQ.3 ) THEN
 1409                     FACF   = TPIINV
 1410                     FACE   = 1.
 1411                     FACS   = 1.
 1412                   ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.4 ) THEN
 1413                     FACF   = TPIINV * UABS / GRAV
 1414                     FACE   = GRAV**3 / UABS**5
 1415                     FACS   = GRAV**2 / UABS**4
 1416                   ELSE IF ( ISCALE.EQ.2 .OR. ISCALE.EQ.5 ) THEN
 1417                     FACF   = TPIINV * USTAR / GRAV
 1418                     FACE   = GRAV**3 / USTAR**5
 1419                     FACS   = GRAV**2 / USTAR**4
 1420                   END IF
 1421 !
 1422                 DO IK=1, NK
 1423                   FACTOR = TPI / CG(IK) * SIG(IK)
 1424                   E1  (IK) = 0.
 1425                   SIN1(IK) = 0.
 1426                   SNL1(IK) = 0.
 1427                   SDS1(IK) = 0.
 1428                   SBT1(IK) = 0.
 1429                   STT1(IK) = 0.
 1430                   SIS1(IK) = 0.
 
 
                                                         ww3_outp.F90  page  27
 
 
 1431                   DO ITH=1, NTH
 1432                     ISP         = ITH + (IK-1)*NTH
 1433                     E  (IK,ITH) = SPCO(ISP,J)
 1434                     SWN(IK,ITH) = ( XLN(ITH,IK) + XIN(ITH,IK) ) * FACTOR
 1435                     SNL(IK,ITH) = ( XNL(ITH,IK) + XTR(ITH,IK) ) * FACTOR
 1436                     SDS(IK,ITH) = ( XDS(ITH,IK) + XDB(ITH,IK) ) * FACTOR
 1437                     SBT(IK,ITH) = ( XBT(ITH,IK) * XBS(ITH,IK) ) * FACTOR
 1438                     SIS(IK,ITH) = XIS(ITH,IK) * FACTOR
 1439                     STT(IK,ITH) = SWN(IK,ITH) + SNL(IK,ITH)+SDS(IK,ITH)&
 1440                                   + SBT(IK,ITH) + SIS(IK,ITH) &
 1441                                   + XXX(ITH,IK) * FACTOR
 1442                     E1  (IK) = E1  (IK) + E(IK,ITH)
 1443                     SIN1(IK) = SIN1(IK) + SWN(IK,ITH)
 1444                     SNL1(IK) = SNL1(IK) + SNL(IK,ITH)
 1445                     SDS1(IK) = SDS1(IK) + SDS(IK,ITH)
 1446                     SBT1(IK) = SBT1(IK) + SBT(IK,ITH)
 1447                     SIS1(IK) = SIS1(IK) + SIS(IK,ITH)
 1448                     END DO
 1449                   E1  (IK) = E1(IK)   * DTH * FACE
 1450                   SIN1(IK) = SIN1(IK) * DTH * FACS
 1451                   SNL1(IK) = SNL1(IK) * DTH * FACS
 1452                   SDS1(IK) = SDS1(IK) * DTH * FACS
 1453                   SBT1(IK) = SBT1(IK) * DTH * FACS
 1454                   SIS1(IK) = SIS1(IK) * DTH * FACS
 1455                   END DO
 1456 !
 1457                 STT1       = SIN1 + SNL1 + SDS1 + SBT1 + SIS1
 1458                 E1ALL(:,1) = SIN1
 1459                 E1ALL(:,2) = SNL1
 1460                 E1ALL(:,3) = SDS1
 1461                 E1ALL(:,4) = SBT1
 1462                 E1ALL(:,5) = SIS1
 1463                 E1ALL(:,6) = STT1
 1464 !
 1465               END IF
 1466 !
 1467 ! 4.a Perform output type 1 ( print plots / tables / file )
 1468 !
 1469             IF ( ITYPE .EQ. 1 ) THEN
 1470 !
 1471                 IF ( OTYPE .EQ. 1 ) THEN
 1472 !
 1473                     IF ( SCALE1 .GE. 0. )                             &
 1474                         CALL PRT1DS (NDSO, NK, E1, SIG(1:NK), 'RAD/S',&
 1475                              17, SCALE1, 'E(f)', 'm^2s', PTNME(J) )
 1476                     IF ( SCALE2 .GE. 0. )                             &
 1477                         CALL PRT2DS (NDSO, NK, NK, NTH, E, SIG(1:NK), &
 1478                              'RAD/S', 1., SCALE2, 0.0001, 'E(f,th)',  &
 1479                              'm^2s', PTNME(J) )
 1480                     WRITE (NDSO,910) DPO(J), UABS
 1481                     IF ( WAO(J) .GT. 0. ) WRITE (NDSO,911) UDIR
 1482                     WRITE (NDSO,912) ASO(J), CAO(J)
 1483                     IF ( CAO(J) .GT. 0. ) WRITE (NDSO,913) CDIR
 1484                     WRITE (NDSO,914) HSIG, WLEN, TMEAN, THMEAN, THSPRD
 1485 !
 
 
                                                         ww3_outp.F90  page  28
 
 
 1486                   ELSE IF ( OTYPE .EQ. 2 ) THEN
 1487 !
 1488                     CALL STME21 ( TIME , DTME21 )
 1489                     IF ( FLAGLL ) THEN
 1490                         WRITE (NDSTAB,920) DTME21, PTNME(J),          &
 1491                             M2KM*PTLOC(1,J), M2KM*PTLOC(2,J),         &
 1492                             DPO(J), USTAR, WAO(J), UDIR
 1493                       ELSE
 1494                         WRITE (NDSTAB,720) DTME21, PTNME(J),          &
 1495                             M2KM*PTLOC(1,J), M2KM*PTLOC(2,J),         &
 1496                             DPO(J), USTAR, WAO(J), UDIR
 1497                       END IF
 1498                     IF ( FP .EQ. 0. ) FP = SIG(NK)
 1499                     DO IK=1, NK
 1500                       WRITE (NDSTAB,921) TPIINV*SIG(IK), SIG(IK)/FP,  &
 1501                           E1(IK), THBND(IK), SPBND(IK), APM(IK)
 1502                       END DO
 1503                     IF ( FP .EQ. SIG(NK) ) FP = 0.
 1504                     WRITE (NDSTAB,922)
 1505 !
 1506                   ELSE IF ( OTYPE .EQ. 3 ) THEN
 1507 !
 1508                     IF ( FLFORM ) THEN
 1509                         WRITE (NDSTAB) PTNME(J), PTLOC(2,J),          &
 1510                                        PTLOC(1,J), DPO(J), WAO(J),    &
 1511                                        UDIR, CAO(J), CDIR
 1512                         WRITE (NDSTAB) ((E(IK,ITH),IK=1,NK),ITH=1,NTH)
 1513                       ELSE
 1514                         WRITE (NDSTAB,901) PTNME(J), M2KM*PTLOC(2,J), &
 1515                                            M2KM*PTLOC(1,J), DPO(J),   &
 1516                                            WAO(J), UDIR, CAO(J), CDIR
 1517                         WRITE (NDSTAB,902)                            &
 1518                               ((E(IK,ITH),IK=1,NK),ITH=1,NTH)
 1519                       END IF
 1520 !
 1521                   END IF
 1522 !
 1523 ! 4.b Perform output type 2 ( tables )
 1524 !
 1525               ELSE IF ( ITYPE .EQ. 2 ) THEN
 1526 !
 1527                 IF ( NREQ .EQ. 1 ) THEN
 1528 !
 1529                     IYR    = TIME(1) / 10000
 1530                     IMTH   = MOD ( TIME(1) , 10000 ) / 100
 1531                     IDY    = MOD ( TIME(1) , 100 )
 1532                     IH     = TIME(2) / 10000
 1533                     IM     = MOD ( TIME(2) , 10000 ) / 100
 1534                     IS     = MOD ( TIME(2) , 100 )
 1535                     IF ( OTYPE .EQ. 1 ) THEN
 1536                         WRITE (NDSTAB,1921) TIME(1), IH, IM, IS,      &
 1537                                DPO(J), CAO(J), CDIR, WAO(J), UDIR
 1538                       ELSE IF ( OTYPE .EQ. 2 ) THEN
 1539                         WRITE (NDSTAB,1922) TIME(1), IH, IM, IS,      &
 1540                                HSIG, WLEN, TMEAN, THMEAN, THSPRD,     &
 
 
                                                         ww3_outp.F90  page  29
 
 
 1541                                FP*TPIINV, THP, SPP
 1542                       ELSE IF ( OTYPE.EQ.3 ) THEN
 1543                         WRITE (NDSTAB,1923) TIME(1), IH, IM, IS,      &
 1544                            UNORM, ESTAR, FPSTAR, CD*1000., APM(NK)*100.
 1545                       ELSE IF ( OTYPE.EQ.4 ) THEN
 1546                         WRITE (NDSTAB,1924) TIME(1), IH, IM, IS,      &
 1547                            UNORM, ESTAR, FPSTAR, CD*1000., APM(NK)*100.
 1548                       ELSE IF ( OTYPE.EQ.5 ) THEN
 1549                         HMAT   = MIN ( 100. , 3.33*GRAV*HSIG/UABS**2 )
 1550                         IF ( HSIG .GE. HSMIN ) THEN
 1551                             CALL WAVNU1 ( FP, DPO(J), WNA, XYZ )
 1552                             AGE1   = MIN ( 100. , FP / WNA / UABS )
 1553                             AFR    = TPI / TMEAN
 1554                             CALL WAVNU1 ( AFR, DPO(J), WNA, XYZ )
 1555                             AGE2   = MIN ( 100. , AFR / WNA / UABS )
 1556                           ELSE
 1557                             AGE1   = -9.99
 1558                             AGE2   = -9.99
 1559                           END IF
 1560                         WRITE (NDSTAB,1925) TIME(1), IH, IM, IS,      &
 1561                                WAO(J), UDIR, HSIG, HMAT, AGE1, AGE2,  &
 1562                                ASO(J)
 1563                       ELSE IF ( OTYPE.EQ.6 ) THEN
 1564                         IF ( HSIG .GE. HSMIN ) THEN
 1565                            WRITE (NDSTAB,1926) IYR, IMTH, IDY, IH,    &
 1566                                  WAO(J), NINT(UDIR), HSIG, TPI / FP
 1567                         ELSE
 1568                            WRITE (NDSTAB,1926) IYR, IMTH, IDY, IH,    &
 1569                                  WAO(J), NINT(UDIR), HSIG, 0.0
 1570                         END IF
 1571                       END IF
 1572 !
 1573                   ELSE
 1574 !
 1575                     IF ( OTYPE .EQ. 1 ) THEN
 1576                         IF ( FLAGLL ) THEN
 1577                             WRITE (NDSTAB,1931) M2KM*PTLOC(1,J),      &
 1578                                    M2KM*PTLOC(2,J), DPO(J), CAO(J),   &
 1579                                    CDIR, WAO(J), UDIR
 1580                           ELSE
 1581                             WRITE (NDSTAB,1731) M2KM*PTLOC(1,J),      &
 1582                                    M2KM*PTLOC(2,J), DPO(J), CAO(J),   &
 1583                                    CDIR, WAO(J), UDIR
 1584                           END IF
 1585                       ELSE IF ( OTYPE .EQ. 2 ) THEN
 1586                         IF ( FLAGLL ) THEN
 1587                             WRITE (NDSTAB,1932) M2KM*PTLOC(1,J),      &
 1588                                    M2KM*PTLOC(2,J), HSIG, WLEN,       &
 1589                                    TMEAN, THMEAN, THSPRD, FP*TPIINV,  &
 1590                                    THP, SPP
 1591                           ELSE
 1592                             WRITE (NDSTAB,1732) M2KM*PTLOC(1,J),      &
 1593                                    M2KM*PTLOC(2,J), HSIG, WLEN,       &
 1594                                    TMEAN, THMEAN, THSPRD, FP*TPIINV,  &
 1595                                    THP, SPP
 
 
                                                         ww3_outp.F90  page  30
 
 
 1596                           END IF
 1597                       ELSE IF ( OTYPE .EQ. 3 ) THEN
 1598                         WRITE (NDSTAB,1933) 1.E-4*XSTAR,              &
 1599                                1.E-4*YSTAR, UNORM, ESTAR, FPSTAR,     &
 1600                                CD*1000., APM(NK)*100.
 1601                       ELSE IF ( OTYPE .EQ. 4 ) THEN
 1602                         WRITE (NDSTAB,1934) XSTAR, YSTAR, UNORM,      &
 1603                                ESTAR, FPSTAR, CD*1000., APM(NK)*100.
 1604                       ELSE IF ( OTYPE .EQ. 5 ) THEN
 1605                         HMAT   = MIN ( 100. , 3.33*GRAV*HSIG/UABS**2 )
 1606                         CALL WAVNU1 ( FP, DPO(J), WNA, XYZ )
 1607                         AGE1   = MIN ( 100. , FP / WNA / UABS )
 1608                         AFR    = TPI / TMEAN
 1609                         CALL WAVNU1 ( AFR, DPO(J), WNA, XYZ )
 1610                         AGE2   = MIN ( 100. , AFR / WNA / UABS )
 1611                         IF ( FLAGLL ) THEN
 1612                             WRITE (NDSTAB,1935) M2KM*PTLOC(1,J),      &
 1613                                    M2KM*PTLOC(2,J), WAO(J), UDIR,     &
 1614                                    HSIG, HMAT, AGE1, AGE2, ASO(J)
 1615                           ELSE
 1616                             WRITE (NDSTAB,1735) M2KM*PTLOC(1,J),      &
 1617                                    M2KM*PTLOC(2,J), WAO(J), UDIR,     &
 1618                                    HSIG, HMAT, AGE1, AGE2, ASO(J)
 1619                           END IF
 1620                       ELSE IF ( OTYPE .EQ. 6 ) THEN
 1621                         IF ( HSIG .GE. HSMIN ) THEN
 1622                            IF ( FLAGLL ) THEN
 1623                                WRITE (NDSTAB,1936) M2KM*PTLOC(1,J),      >
 1624                                       M2KM*PTLOC(2,J), WAO(J), NINT(UDIR)>
 1625                                       HSIG, TPI / FP
 1626                              ELSE
 1627                                WRITE (NDSTAB,1736) M2KM*PTLOC(1,J),      >
 1628                                       M2KM*PTLOC(2,J), WAO(J), NINT(UDIR)>
 1629                                       HSIG, TPI / FP
 1630                              END IF
 1631                         ELSE
 1632                            IF ( FLAGLL ) THEN
 1633                                WRITE (NDSTAB,1936) M2KM*PTLOC(1,J),      >
 1634                                       M2KM*PTLOC(2,J), WAO(J), NINT(UDIR)>
 1635                                       HSIG, 0.0
 1636                              ELSE
 1637                                WRITE (NDSTAB,1736) M2KM*PTLOC(1,J),      >
 1638                                       M2KM*PTLOC(2,J), WAO(J), NINT(UDIR)>
 1639                                       HSIG, 0.0
 1640                              END IF
 1641                         END IF
 1642                       END IF
 1643 !
 1644                   END IF
 1645 !
 1646 ! 4.c Perform output type 3 ( source terms )
 1647 !
 1648               ELSE IF ( ITYPE .EQ. 3 ) THEN
 1649 !
 1650                 IF ( OTYPE .EQ. 1 ) THEN
 
 
                                                         ww3_outp.F90  page  31
 
 
 1651 !
 1652                     IF ( SCALE1 .GE. 0. ) THEN
 1653                         IF ( FLSRCE(1) )                              &
 1654                             CALL PRT1DS (NDSO, NK, E1, SIG(1:NK),     &
 1655                                  'RAD/S', 17,  0., 'E(f)', 'm^2s',    &
 1656                                  PTNME(J) )
 1657                         IF (FLSRCE(2) .OR. FLSRCE(3) .OR.             &
 1658                             FLSRCE(4) .OR. FLSRCE(5) .OR.             &
 1659                             FLSRCE(6) .OR. FLSRCE(7) )                &
 1660                             CALL PRT1DM (NDSO, NK, 6, E1ALL, SIG(1:NK),&
 1661                                  'RAD/S', 17, SCALE1, VAR1, 'M2',     &
 1662                                  PTNME(J) )
 1663                       END IF
 1664                     IF ( SCALE2 .GE. 0. ) THEN
 1665                         IF ( FLSRCE(1) )                              &
 1666                             CALL PRT2DS (NDSO, NK, NK, NTH, E,        &
 1667                                  SIG(1:NK), 'RAD/S', 1., 0., 0.0001,  &
 1668                                  'E(f,th)', 'm^2s', PTNME(J) )
 1669                         IF ( FLSRCE(2) )                              &
 1670                             CALL PRT2DS (NDSO, NK, NK, NTH, SWN,      &
 1671                                 SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
 1672                                  'Sin(f,th)', 'm^2', PTNME(J) )
 1673                         IF ( FLSRCE(3) )                              &
 1674                             CALL PRT2DS (NDSO, NK, NK, NTH, SNL,      &
 1675                                 SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
 1676                                  'Snl(f,th)', 'm^2', PTNME(J) )
 1677                         IF ( FLSRCE(4) )                              &
 1678                             CALL PRT2DS (NDSO, NK, NK, NTH, SDS,      &
 1679                                 SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
 1680                                  'Sds(f,th)', 'm^2', PTNME(J) )
 1681                         IF ( FLSRCE(5) )                              &
 1682                             CALL PRT2DS (NDSO, NK, NK, NTH, SBT,      &
 1683                                 SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
 1684                                  'Sbt(f,th)', 'm^2', PTNME(J) )
 1685                         IF ( FLSRCE(6) )                              &
 1686                             CALL PRT2DS (NDSO, NK, NK, NTH, SIS,      &
 1687                                 SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
 1688                                  'Sice(f,th)', 'm^2', PTNME(J) )
 1689                         IF ( FLSRCE(7) )                              &
 1690                             CALL PRT2DS (NDSO, NK, NK, NTH, STT,      &
 1691                                 SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
 1692                                  'Stot(f,th)', 'm^2', PTNME(J) )
 1693                       END IF
 1694 !
 1695                   ELSE IF ( OTYPE .EQ. 2 ) THEN
 1696 !
 1697                     CALL STME21 ( TIME , DTME21 )
 1698                     IF ( FLAGLL ) THEN
 1699                         WRITE (NDSTAB,2920) DTME21, PTNME(J),         &
 1700                             M2KM*PTLOC(1,J), M2KM*PTLOC(2,J),         &
 1701                             DPO(J), USTAR, WAO(J)
 1702                       ELSE
 1703                         WRITE (NDSTAB,2720) DTME21, PTNME(J),         &
 1704                             M2KM*PTLOC(1,J), M2KM*PTLOC(2,J),         &
 1705                             DPO(J), USTAR, WAO(J)
 
 
                                                         ww3_outp.F90  page  32
 
 
 1706                       END IF
 1707                     IF ( ISCALE.EQ.0 ) THEN
 1708                         WRITE (NDSTAB,2921)
 1709                       ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 1710                         WRITE (NDSTAB,2922)
 1711                       ELSE IF ( ISCALE.EQ.3 ) THEN
 1712                         WRITE (NDSTAB,2923)
 1713                       ELSE IF ( ISCALE.EQ.4 .OR. ISCALE.EQ.5 ) THEN
 1714                         WRITE (NDSTAB,2924)
 1715                       END IF
 1716                     IF ( ISCALE.GE.3 ) FACF = 1. / FP
 1717                     DO IK=1, NK
 1718                       WRITE (NDSTAB,2930) FACF*SIG(IK), E1(IK),       &
 1719                         SIN1(IK), SNL1(IK), SDS1(IK), SBT1(IK),       &
 1720                         SIS1(IK), STT1(IK)
 1721                                           
 1722                       END DO
 1723                     WRITE (NDSTAB,2940)
 1724 !
 1725                   ELSE IF ( OTYPE .EQ. 3 ) THEN
 1726 !
 1727                     CALL STME21 ( TIME , DTME21 )
 1728                     IF ( FLAGLL ) THEN
 1729                         WRITE (NDSTAB,2920) DTME21, PTNME(J),         &
 1730                             M2KM*PTLOC(1,J), M2KM*PTLOC(2,J),         &
 1731                             DPO(J), USTAR, WAO(J)
 1732                       ELSE
 1733                         WRITE (NDSTAB,2720) DTME21, PTNME(J),         &
 1734                             M2KM*PTLOC(1,J), M2KM*PTLOC(2,J),         &
 1735                             DPO(J), USTAR, WAO(J)
 1736                       END IF
 1737                     IF ( ISCALE.EQ.0 ) THEN
 1738                         WRITE (NDSTAB,2925)
 1739                       ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 1740                         WRITE (NDSTAB,2926)
 1741                       ELSE IF ( ISCALE.EQ.3 ) THEN
 1742                         WRITE (NDSTAB,2927)
 1743                       ELSE IF ( ISCALE.EQ.4 .OR. ISCALE.EQ.5 ) THEN
 1744                         WRITE (NDSTAB,2928)
 1745                       END IF
 1746 !
 1747                     IF ( ISCALE.GE.3 ) FACF = 1. / FP
 1748                     DO IK=1, NK
 1749                       FACT   = 1. / MAX ( 1.E-10 , E1(IK) )
 1750                       IF ( E1(IK) .GT. 1.E-10 ) THEN
 1751                           WRITE (NDSTAB,2931) FACF*SIG(IK), E1(IK),   &
 1752                             FACT*SIN1(IK), FACT*SNL1(IK),             &
 1753                             FACT*SDS1(IK), FACT*SBT1(IK),             &
 1754                             FACT*SIS1(IK),FACT*STT1(IK)
 1755                         ELSE
 1756                           WRITE (NDSTAB,2931) FACF*SIG(IK), E1(IK)
 1757                         END IF
 1758                       END DO
 1759                     WRITE (NDSTAB,2940)
 1760 !
 
 
                                                         ww3_outp.F90  page  33
 
 
 1761                   ELSE IF ( OTYPE .EQ. 4 ) THEN
 1762 !
 1763                     IF ( FLFORM ) THEN
 1764                         WRITE (NDSTAB) PTNME(J), PTLOC(2,J),          &
 1765                                        PTLOC(1,J), DPO(J), WAO(J),    &
 1766                                        UDIR, CAO(J), CDIR
 1767                         IF ( FLSRCE(1) ) WRITE (NDSTAB)               &
 1768                               ((E(IK,ITH),IK=1,NK),ITH=1,NTH)
 1769                         IF ( FLSRCE(2) ) WRITE (NDSTAB)               &
 1770                               ((SWN(IK,ITH),IK=1,NK),ITH=1,NTH)
 1771                         IF ( FLSRCE(3) ) WRITE (NDSTAB)               &
 1772                               ((SNL(IK,ITH),IK=1,NK),ITH=1,NTH)
 1773                         IF ( FLSRCE(4) ) WRITE (NDSTAB)               &
 1774                               ((SDS(IK,ITH),IK=1,NK),ITH=1,NTH)
 1775                         IF ( FLSRCE(5) ) WRITE (NDSTAB)               &
 1776                               ((SBT(IK,ITH),IK=1,NK),ITH=1,NTH)
 1777                         IF ( FLSRCE(6) ) WRITE (NDSTAB)               &
 1778                               ((SIS(IK,ITH),IK=1,NK),ITH=1,NTH)
 1779                         IF ( FLSRCE(7) ) WRITE (NDSTAB)               &
 1780                               ((STT(IK,ITH),IK=1,NK),ITH=1,NTH)
 1781                       ELSE
 1782                         IF ( FLAGLL ) THEN
 1783                             WRITE (NDSTAB,901) PTNME(J),              &
 1784                                 M2KM*PTLOC(2,J), M2KM*PTLOC(1,J),     &
 1785                                 DPO(J), WAO(J), UDIR, CAO(J), CDIR
 1786                           ELSE
 1787                             WRITE (NDSTAB,701) PTNME(J),              &
 1788                                 M2KM*PTLOC(2,J), M2KM*PTLOC(1,J),     &
 1789                                 DPO(J), WAO(J), UDIR, CAO(J), CDIR
 1790                           END IF
 1791                         IF ( FLSRCE(1) ) WRITE (NDSTAB,902)           &
 1792                               ((E(IK,ITH),IK=1,NK),ITH=1,NTH)
 1793                         IF ( FLSRCE(2) ) WRITE (NDSTAB,902)           &
 1794                               ((SWN(IK,ITH),IK=1,NK),ITH=1,NTH)
 1795                         IF ( FLSRCE(3) ) WRITE (NDSTAB,902)           &
 1796                               ((SNL(IK,ITH),IK=1,NK),ITH=1,NTH)
 1797                         IF ( FLSRCE(4) ) WRITE (NDSTAB,902)           &
 1798                               ((SDS(IK,ITH),IK=1,NK),ITH=1,NTH)
 1799                         IF ( FLSRCE(5) ) WRITE (NDSTAB,902)           &
 1800                               ((SBT(IK,ITH),IK=1,NK),ITH=1,NTH)
 1801                         IF ( FLSRCE(6) ) WRITE (NDSTAB,902)           &
 1802                               ((SIS(IK,ITH),IK=1,NK),ITH=1, NTH)
 1803                         IF ( FLSRCE(7) ) WRITE (NDSTAB,902)           &
 1804                               ((STT(IK,ITH),IK=1,NK),ITH=1,NTH)
 1805                       END IF
 1806 !
 1807                   END IF
 1808 !
 1809 ! 4.d Perform output type 4 ( Spectral partitions and bulletins )
 1810 !
 1811               ELSE IF ( ITYPE .EQ. 4 ) THEN
 1812 !
 1813                   IF ( OTYPE .EQ. 1 ) THEN
 1814 !
 1815                     IF ( FLAGLL ) THEN
 
 
                                                         ww3_outp.F90  page  34
 
 
 1816                         IF ( PTLOC(1,J) .LT. 0. )                     &
 1817                             PTLOC(1,J) = PTLOC(1,J) + 360.
 1818                         WRITE (NDSTAB,940) TIME, M2KM*PTLOC(2,J),     &
 1819                             M2KM*PTLOC(1,J), PTNME(J), NPART, DEPTH,  &
 1820                             WAO(J), UDIR, CAO(J), CDIR
 1821                       ELSE
 1822                         WRITE (NDSTAB,943) TIME, M2KM*PTLOC(1,J),     &
 1823                             M2KM*PTLOC(2,J), PTNME(J), NPART, DEPTH,  &
 1824                             WAO(J), UDIR, CAO(J), CDIR
 1825                       END IF
 1826 !                   WRITE (NDSTAB,941)
 1827                     DO I=0, NPART
 1828                       WRITE (NDSTAB,942) I, XPART(:,I)
 1829                       END DO
 1830 !
 1831                   ELSEIF ( OTYPE .GE. 2 ) THEN
 1832                       CALL W3BULL (NPART, XPART, DIMXP, UABS,         &
 1833                                    UDIR, J, IOUT, TIMEV )
 1834 !
 1835                     IF ( FLAGLL ) THEN
 1836                       X = M2KM * PTLOC(1,J)
 1837                       Y = M2KM * PTLOC(2,J)
 1838                                            
 1839                       X      = MOD ( X+720. , 360. )
 1840                       IF ( X .LE. 180. ) THEN
 1841                         IDLON  = 'E'
 1842                       ELSE
 1843                         X      = 360. - X
 1844                         IDLON  = 'W'
 1845                       ENDIF
 1846                       !IF ( ABS(Y) .LE. 0.0049 ) THEN
 1847                        !IDLAT  = '-'
 1848                       IF ( Y .GE. 0. ) THEN
 1849                         IDLAT  = 'N'
 1850                       ELSE
 1851                         IDLAT  = 'S'
 1852                         Y      = -Y
 1853                       ENDIF
 1854                     ELSE
 1855                       IDLAT = ' '
 1856                       IDLON = ' '
 1857                     ENDIF
 1858                       IF ( OTYPE .EQ. 2 .OR. OTYPE .EQ. 4 ) THEN
 1859                         NDSBUL=NDSTAB + (J - 1)
 1860                         IF (IOUT .EQ. 1) THEN
 1861                           WRITE(HSTR,'(I2,1X,A)') TIMEV(2)/10000,   &
 1862                                    HTYPE
 1863                           WRITE (NDSBUL,970) PTNME(J), Y, IDLAT, X, &
 1864                                            IDLON, GNAME, TIMEV(1),  &
 1865                                            HSTR
 1866                           WRITE (NDSBUL,971)
 1867                           WRITE (NDSBUL,972)
 1868                           WRITE (NDSBUL,971)
 1869                         ENDIF
 1870                              
 
 
                                                         ww3_outp.F90  page  35
 
 
 1871                         WRITE (NDSBUL,973) ASCBLINE
 1872                      ENDIF
 1873                       IF ( OTYPE .EQ. 3 .OR. OTYPE .EQ. 4 ) THEN
 1874                         ICSV = 0
 1875                         IF ( NDSBUL .GT. 0 ) ICSV = NDSBUL
 1876                         NDSCSV = NDSTAB + (J - 1) + ICSV
 1877                         WRITE (NDSCSV,'(A664)') CSVBLINE
 1878                       ENDIF
 1879                   END IF
 1880 !
 1881               END IF
 1882 ! ... End of fields loop
 1883 !
 1884           END IF
 1885         END DO
 1886 !
 1887       RETURN
 1888 !
 1889 ! Formats
 1890 !
 1891   900 FORMAT (I8.8,I7.6)
 1892   901 FORMAT ('''',A10,'''',2F7.2,F10.1,2(F7.2,F6.1))
 1893   701 FORMAT ('''',A10,'''',2(F8.1,'E3'),F10.1,2(F7.2,F6.1))
 1894   902 FORMAT (7E11.3)
 1895   905 FORMAT (9X,A)
 1896   910 FORMAT (/15X,' Water depth       :',F7.1,'  (m)'/               &
 1897                15X,' Wind speed        :',F8.2,' (m/s)')
 1898   911 FORMAT ( 15X,' Wind direction    :',F7.1,'  (degr)')
 1899   912 FORMAT ( 15X,' Air-sea temp. dif.:',F7.1,'  (degr)'/            &
 1900                15X,' Current speed     :',F8.2,' (m/s)')
 1901   913 FORMAT ( 15X,' Current direction :',F7.1,'  (degr)')
 1902   914 FORMAT ( 15X,' Wave height       :',F8.2,' (m)'/                &
 1903                15X,' Mean wave length  :',F6.0,'   (m)'/              &
 1904                15X,' Mean wave period  :',F7.1,'  (s)'/               &
 1905                15X,' Mean wave direct. :',F7.1,'  (degr)'/            &
 1906                15X,' Direct. spread    :',F7.1,'  (degr)'/)
 1907   920 FORMAT (' Time     : ',A/                                  &
 1908               ' Location : ',A,'  (',2F8.2,' )'/                 &
 1909               ' depth    : ',F7.1,'   m'/                        &
 1910               ' U*       : ',F9.3,' m/s'/                        &
 1911               ' U10      : ',F7.1,'   m/s'/                      &
 1912               ' Dir U10  : ',F7.1,'   degr'//                    &
 1913        '      f     f/fp      F(f)    theta    spr    alpha  '/  &
 1914        '     (Hz)    (-)     (m2s)    (deg)   (deg)    (-)   '/  &
 1915            '  --------------------------------------------------')
 1916   720 FORMAT (' Time     : ',A/                                  &
 1917               ' Location : ',A,'  (',2(F8.1,'E3'),' )'/          &
 1918               ' depth    : ',F7.1,'   m'/                        &
 1919               ' U*       : ',F9.3,' m/s'/                        &
 1920               ' U10      : ',F7.1,'   m/s'/                      &
 1921               ' Dir U10  : ',F7.1,'   degr'//                    &
 1922        '      f     f/fp      F(f)    theta    spr    alpha  '/  &
 1923        '     (Hz)    (-)     (m2s)    (deg)   (deg)    (-)   '/  &
 1924        '  --------------------------------------------------')
 1925   921 FORMAT (1x,F8.5,F7.3,E11.3,2F8.1,F8.4)
 
 
                                                         ww3_outp.F90  page  36
 
 
 1926   922 FORMAT (' '/' ')
 1927 !
 1928   940 FORMAT (1X,I8.8,1X,I6.6,2F8.3,2X,'''',A10,'''',            &
 1929               1X,I3,F7.1,F5.1,f6.1,F5.2,F6.1)
 1930   943 FORMAT (1X,I8.8,1X,I6.6,2(F8.1,'E3'),2X,'''',A10,'''',     &
 1931               1X,I3,F7.1,F5.1,f6.1,F5.2,F6.1)
 1932   941 FORMAT ('        hs     tp     lp       theta     sp      wf')
 1933   942 FORMAT (I3,3F8.2,2F9.2,10F7.2)
 1934 !
 1935  970 FORMAT ( '  Location : ',A,' (',F5.2,A,1X,F6.2,A,')'/            &
 1936               '  Model    : ',A/                                      &
 1937               '  Cycle    : ',I8,1X,A)
 1938  971 FORMAT (' +-------+-----------+-----------------+',              &
 1939              '-----------------+-----------------+----',              &
 1940              '-------------+-----------------+--------',              &
 1941              '---------+')
 1942  972 FORMAT (' | day & |  Hst  n x |    Hs   Tp  dir |',              &
 1943                                    '    Hs   Tp  dir |',              &
 1944                                    '    Hs   Tp  dir |',              &
 1945                                    '    Hs   Tp  dir |',              &
 1946                                    '    Hs   Tp  dir |',              &
 1947                                    '    Hs   Tp  dir |'/              &
 1948              ' |  hour |  (m)  - - |    (m)  (s) (d) |',              &
 1949                                    '    (m)  (s) (d) |',              &
 1950                                    '    (m)  (s) (d) |',              &
 1951                                    '    (m)  (s) (d) |',              &
 1952                                    '    (m)  (s) (d) |',              &
 1953                                    '    (m)  (s) (d) |')
 1954  973 FORMAT (1X,A)
 1955 !
 1956  1901 FORMAT (                                                        &
 1957        '    Date     Time        d     Uc     Dir.  U10    Dir. '/    &
 1958        '            h  m  s     (m)   (m/s)  (d.N) (m/s)  (d.N) '/    &
 1959        ' ---------------------------------------------------------')
 1960  1902 FORMAT (                                                        &
 1961        '    Date     Time        Hs     L      Tr    Dir.  Spr. ',    &
 1962        '    fp    p_dir  p_spr'/                                      &
 1963        '            h  m  s     (m)    (m)    (s)   (d.N)  (deg)',    &
 1964        '   (Hz)   (d.N)  (deg)'/                                      &
 1965        ' -------------------------------------------------------',    &
 1966        '-----------------------')
 1967  1903 FORMAT (                                                        &
 1968     '    Date     Time       U*       E*        fp*       Cd   alpha'/&
 1969     '            h  m  s   (m/s)     (-)        (-)     *1000  *100'/ &
 1970     ' --------------------------------------------------------------')
 1971  1904 FORMAT (                                                        &
 1972      '    Date     Time     U10       E*        fp*       Cd   alpha'/&
 1973      '            h  m  s  (m/s)     (-)        (-)     *1000  *100'/ &
 1974      ' --------------------------------------------------------------')
 1975  1905 FORMAT (                                                        &
 1976        '   Date     Time     U10    Dir.    Hs      H*    cp/U  ',    &
 1977            '  cm/U     Dt'/                                           &
 1978        '                    (m/s)  (d.N)   (m)     (-)    (-)   ',    &
 1979            '   (-)   (deg)'/                                          &
 1980       ' --------------------------------------------------',          &
 
 
                                                         ww3_outp.F90  page  37
 
 
 1981        '---------------------')
 1982  1906 FORMAT (                                                         &
 1983      '     Time      U10    Dir. Hs  Tp  '/         &
 1984      '  yr mth dy h  (m/s) (d.N) (m) (s) '/         &
 1985      ' ----------------------------------')
 1986  1910 FORMAT ( ' '/' ' )
 1987  1911 FORMAT (' Time : ',A//                                     &
 1988    '    Long.    Lat.       d     Uc     Dir.  U10    Dir. '/    &
 1989    '                       (m)   (m/s)  (d.N) (m/s)  (d.N) '/    &
 1990    ' --------------------------------------------------------')
 1991  1912 FORMAT (' Time : ',A//                                     &
 1992    '    Long.    Lat.       Hs     L      Tr    Dir.  Spr. ',    &
 1993    '    fp    p_dir  p_spr'/                                     &
 1994    '                       (m)    (m)    (s)   (d.N)  (deg)',    &
 1995    '   (Hz)   (d.N)  (deg)'/                                     &
 1996    ' ------------------------------------------------------',    &
 1997    '-----------------------')
 1998  1711 FORMAT (' Time : ',A//                                     &
 1999    '       X        Y          d     Uc     Dir.  U10    Dir. '/ &
 2000    '      (m)      (m)        (m)   (m/s)  (d.N) (m/s)  (d.N) '/ &
 2001    ' ----------------------------------------------------------')
 2002  1712 FORMAT (' Time : ',A//                                     &
 2003    '       X        Y          Hs     L      Tr    Dir.  Spr. ', &
 2004    '    fp    p_dir  p_spr'/                                     &
 2005    '      (m)      (m))       (m)    (m)    (s)   (d.N)  (deg)', &
 2006    '   (Hz)   (d.N)  (deg)'/                                     &
 2007    ' ------------------------------------------------------',    &
 2008    '-------------------------')
 2009  1913 FORMAT (' Time : ',A//                                          &
 2010    '       X*       Y*       U*       E*        fp*       Cd   alpha'/&
 2011    '      (-)      (-)     (m/s)     (-)        (-)     *1000  *100'/ &
 2012    ' --------------------------------------------------------------')
 2013  1914 FORMAT (' Time : ',A//                                          &
 2014    '       X*       Y*     U10       E*        fp*       Cd   alpha'/ &
 2015    '      (-)      (-)    (m/s)     (-)        (-)     *1000  *100 '/ &
 2016    ' --------------------------------------------------------------')
 2017  1915 FORMAT (' Time : ',A//                                     &
 2018    '     Long.    Lat.   U10    Dir.    Hs      H*    cp/U  ',   &
 2019        '  cm/U     Dt'/                                          &
 2020    '                    (m/s)  (d.N)   (m)     (-)    (-)   ',   &
 2021        '   (-)   (deg)'/                                         &
 2022    ' -------------------------------------------------',         &
 2023    '---------------------')
 2024  1715 FORMAT (' Time : ',A//                                     &
 2025    '       X        Y      U10    Dir.    Hs      H*    cp/U  ', &
 2026        '  cm/U     Dt'/                                          &
 2027    '      (m)      (m)    (m/s)  (d.N)   (m)     (-)    (-)   ', &
 2028        '   (-)   (deg)'/                                         &
 2029    ' ---------------------------------------------------',       &
 2030    '---------------------')
 2031  1916 FORMAT (' Time : ',A//                                     &
 2032    '   Long.     Lat.   U10   Dir.     Hs      Tp  '/         &
 2033    '                 (m/s)   (d.N)    (m)     (s)  '/         &
 2034    '-----------------------------------------------')
 2035  1716 FORMAT (' Time : ',A//                                     &
 
 
                                                         ww3_outp.F90  page  38
 
 
 2036    '     X      Y      U10      Dir.      Hs       Tp  '/         &
 2037    '    (m)    (m)    (m/s)    (d.N)     (m)      (s)  '/         &
 2038    '---------------------------------------------------')
 2039  1921 FORMAT ( 2X,I8.8,I3,2(1X,I2.2),F10.1,F6.2,F7.1,F6.2,F7.1)
 2040  1922 FORMAT ( 2X,I8.8,I3,2(1X,I2.2),F9.3,F7.1,F7.2,F7.1,F7.2,        &
 2041                F8.4,F7.1,F7.2)
 2042  1923 FORMAT ( 2X,I8.8,I3,2(1X,I2.2),F8.4,2E11.3,2F7.3)
 2043  1924 FORMAT ( 2X,I8.8,I3,2(1X,I2.2),F7.1,2E11.3,2F7.3)
 2044  1925 FORMAT ( 2X,I8.8,I3,2(1X,I2.2),F7.2,F7.1,2F7.2,2F8.2,F7.1)
 2045  1926 FORMAT ( 2X,I4,3(1X,I2),F6.2,1X,I3,2F6.2)
 2046  1931 FORMAT ( 2X,2F8.3,F10.1,F6.2,F7.1,F6.2,F7.1)
 2047  1932 FORMAT ( 2X,2F8.3,F9.3,F7.1,F7.2,F7.1,F7.2,                &
 2048                F8.4,F7.1,F7.2)
 2049  1731 FORMAT ( 2X,2(F7.1,'E3'),F10.1,F6.2,F7.1,F6.2,F7.1)
 2050  1732 FORMAT ( 2X,2(F7.1,'E3'),F9.3,F7.1,F7.2,F7.1,F7.2,         &
 2051                F8.4,F7.1,F7.2)
 2052  1933 FORMAT ( 2X,2(F7.1,'E4'),F8.4,2E11.3,2F7.3)
 2053  1934 FORMAT ( 2X,2F9.1,F7.1,2E11.3,2F7.3)
 2054  1935 FORMAT ( 2X,2F8.3,F7.2,F7.1,2F7.2,2F8.2,F7.1)
 2055  1735 FORMAT ( 2X,2(F7.1,'E3'),F7.2,F7.1,2F7.2,2F8.2,F7.1)
 2056  1936 FORMAT ( 2X,2F8.3,F6.2,1X,I3,2F6.2)
 2057  1736 FORMAT ( 2X,2(F8.2,'E3'),F6.2,1X,I3,2F6.2)
 2058 !
 2059  2920 FORMAT (' Time     : ',A/                                  &
 2060               ' Location : ',A,'  (',2F8.2,' )'/                 &
 2061               ' depth    : ',F7.1,'   m'/                        &
 2062               ' U*       : ',F9.3,' m/s'/                        &
 2063               ' U10      : ',F7.1,'   m/s'/)
 2064  2720 FORMAT (' Time     : ',A/                                  &
 2065               ' Location : ',A,'  (',2(F8.1,'E3'),' )'/          &
 2066               ' depth    : ',F7.1,'   m'/                        &
 2067               ' U*       : ',F9.3,' m/s'/                        &
 2068               ' U10      : ',F7.1,'   m/s'/)
 2069  2921 FORMAT ('    f           E      ',                              &
 2070               '    Sin        Snl        Sds        Sbt       Sice       >
 2071               '   (Hz)       (m2s)    ',                              &
 2072               '   (m2)       (m2)       (m2)       (m2)       (m2)       >
 2073               ' ------------------------------------------',             >
 2074               '-------------------------------------------')
 2075  2922 FORMAT ('    f*          E*    ',                               &
 2076               '    Sin*       Snl*       Sds*       Sbt*      Sice*      >
 2077               '   (-)         (-)    ',                               &
 2078               '    (-)        (-)        (-)        (-)        (-)       >
 2079               ' ------------------------------------------',             >
 2080               '-------------------------------------------')
 2081  2923 FORMAT ('   f/fp         E      ',                              &
 2082               '   Sin        Snl        Sds        Sbt       Sice      St>
 2083               '   (-)        (m2s)    ',                              &
 2084               '  (m2)       (m2)       (m2)       (m2)       (m2)      (m>
 2085               ' ------------------------------------------',             >
 2086               '-------------------------------------------')
 2087  2924 FORMAT ('   f/fp         E*     ',                              &
 2088               '   Sin*       Snl*       Sds*       Sbt*      Sice*      S>
 2089               '     (-)       (-)     ',                              &
 2090               '   (-)        (-)        (-)        (-)        (-)        >
 
 
                                                         ww3_outp.F90  page  39
 
 
 2091               ' ------------------------------------------',             >
 2092               '-------------------------------------------')
 2093  2925 FORMAT ('    f         E      ',                                &
 2094               '  Tini       Tnli       Tdsi       Tbti      Ticei      Tt>
 2095               '   (Hz)     (m2s)    ',                                &
 2096               ' (1/s)      (1/s)      (1/s)      (1/s)      (1/s)      (1>
 2097               ' ----------------------------------------',               >
 2098               '-------------------------------------------')
 2099  2926 FORMAT ('    f*        E*     ',                                &
 2100               ' Tini*      Tnli*      Tdsi*      Tbti*     Ticei*      Tt>
 2101               '   (-)       (-)     ',                                &
 2102               '  (-)        (-)        (-)        (-)        (-)        (>
 2103               ' ----------------------------------------',               >
 2104               '-------------------------------------------')
 2105  2927 FORMAT ('   f/fp       E      ',                                &
 2106               '  Tini       Tnli       Tdsi       Tbti      Ticei      Tt>
 2107               '   (-)       (m2s)    ',                               &
 2108               ' (1/s)      (1/s)      (1/s)      (1/s)      (1/s)      (1>
 2109               ' ----------------------------------------',               >
 2110               '-------------------------------------------')
 2111  2928 FORMAT ('   f/fp       E*     ',                                &
 2112               ' Tini*      Tnli*      Tdsi*      Tbti*     Ticei*      Tt>
 2113               '   (-)       (-)     ',                                &
 2114               '  (-)        (-)        (-)        (-)        (-)        (>
 2115               ' ----------------------------------------',               >
 2116               '-------------------------------------------')
 2117  2930 FORMAT (1X,F6.4,2X,7E11.3)
 2118  2931 FORMAT (1X,F6.4,7E11.3)
 2119  2940 FORMAT ( ' '/' ' )
 2120 !
 2121 !/
 2122 !/ End of W3EXPO ----------------------------------------------------- /
 2123 !/
 2124       END SUBROUTINE W3EXPO
 2125 !/
 2126 !/ End of W3OUTP ----------------------------------------------------- /
 2127 !/
 2128       END PROGRAM W3OUTP
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
