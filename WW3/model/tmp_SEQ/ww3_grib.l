                                                         ww3_grib.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3GRIB
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |            A. Chawla              |
    9 !/                  |           J.-H. Alves             |
   10 !/                  |                        FORTRAN 90 |
   11 !/                  | Last update :         26-Jul-2018 |
   12 !/                  +-----------------------------------+
   13 !/
   14 !/    01-Nov-1999 : Final FORTRAN 77        ( version 1.18 + error fix )
   15 !/    24-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
   16 !/    25-Jan-2001 : Flat grid error exit added          ( version 2.06 )
   17 !/    29-Apr-2002 : Adding output fields 17-18.         ( version 2.20 )
   18 !/    08-May-2002 : Replace XLF switch with NCEP1.      ( version 2.21 )
   19 !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
   20 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
   21 !/    20-Jul-2005 : Additional output parameters.       ( version 3.07 )
   22 !/    11-Apr-2007 : Additional output parameters.       ( version 3.11 )
   23 !/    18-May-2007 : Update GRIB1 for partitioning.      ( version 3.11 )
   24 !/    16-Jul-2007 : Adding GRIB2 capability.            ( version 3.11 )
   25 !/                  (A. Chawla)
   26 !/    01-Aug-2007 : Update FLGRIB for GRIB2.            ( version 3.11 )
   27 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   28 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   29 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   30 !/    05-Oct-2011 : Updating to the 53 output parameter ( version 4.05 )
   31 !/                  (Arun Chawla)
   32 !/    01-Mar-2013 : Adding double-index output fields   ( version 4.11 )
   33 !/                  (J-Henrique Alves)
   34 !/    01-Dec-2016 : Adding lambert conformal grid       ( version 6.01 )
   35 !/                  (J.H. Alves)
   36 !/    26-Jul-2018 : Adding polar stereographic grid     ( version 6.05 )
   37 !/                  (J.H. Alves)
   38 !/
   39 !/    Copyright 2009 National Weather Service (NWS),
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
   51 !     and from the file ww3_grib.inp ( NDSI, output requests ).
   52 !     Model definition and raw data files are read using WAVEWATCH III
   53 !     subroutines.
   54 !     GRIB packing is performed using NCEP's W3 library (not supplied).
   55 !
 
 
                                                         ww3_grib.F90  page   2
 
 
   56 !     When adding new parameters to GRIB packing, keep in mind that
   57 !     packing is done differently for scalar and vector quantities
   58 !
   59 !  3. Parameters :
   60 !
   61 !  4. Subroutines used :
   62 !
   63 !      Name      Type  Module   Description
   64 !     ----------------------------------------------------------------
   65 !      W3NMOD    Subr. W3GDATMD Set number of model.
   66 !      W3SETG    Subr.   Id.    Point to selected model.
   67 !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
   68 !      W3SETW    Subr.   Id.    Point to selected model for wave data.
   69 !      W3NAUX    Subr. W3ADATMD Set number of model for aux data.
   70 !      W3SETA    Subr.   Id.    Point to selected model for aux data.
   71 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
   72 !      STRACE    Subr.   Id.    Subroutine tracing.
   73 !      NEXTLN    Subr.   Id.    Get next line from input filw
   74 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
   75 !      STME21    Subr. W3TIMEMD Convert time to string.
   76 !      TICK21    Subr.   Id.    Advance time.
   77 !      DSEC21    Func.   Id.    Difference between times.
   78 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
   79 !      W3IOGO    Subr. W3IOGOMD Reading/writing raw gridded data file.
   80 !      W3READFLGRD Subr. W3IOGOMD Reading output fields flags.
   81 !      W3EXGB    Subr. Internal Execute grib output.
   82 !      BAOPEN    Subr.          NCEP library routine.
   83 !      BAOPENW    Subr.          NCEP library routine.
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
  102 !       !/S     Enable subroutine tracing.
  103 !
  104 !       !/NCO   NCEP NCO modifications for operational implementation.
  105 !
  106 !       !/NOGRB No GRIB package included.
  107 !       !/NCEP1 NCEP IBM links to GRIB1 packing routines.
  108 !       !/NCEP2 NCEP IBM links to GRIB2 packing routines.
  109 !
  110 ! 10. Source code :
 
 
                                                         ww3_grib.F90  page   3
 
 
  111 !
  112 !/ ------------------------------------------------------------------- /
  113       USE CONSTANTS
  114 !
  115 !     USE W3GDATMD, ONLY: W3NMOD, W3SETG
  116       USE W3WDATMD, ONLY: W3NDAT, W3SETW
  117 !      USE W3ADATMD, ONLY: W3NAUX, W3SETA
  118       USE W3ODATMD, ONLY: W3NOUT, W3SETO
  119       USE W3IOGRMD, ONLY: W3IOGR
  120       USE W3IOGOMD, ONLY: W3READFLGRD, W3IOGO
  121       USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
  122       USE W3TIMEMD, ONLY: STME21, TICK21, DSEC21
  123 !
  124       USE W3GDATMD
  125       USE W3WDATMD, ONLY: TIME, WLV, ICE, UST, USTDIR
  126       USE W3ADATMD
  127       USE W3ODATMD, ONLY: NDSE, NDST, NDSO, NOGRP, NGRPP, IDOUT, UNDEF,&
  128                           FLOGRD, FNMPRE, NOSWLL, NOGE, FLOGD
  129 !
  130       IMPLICIT NONE
  131 !/
  132 !/ ------------------------------------------------------------------- /
  133 !/ Local variables
  134 !/
  135       INTEGER                 :: NDSI, NDSM, NDSOG, NDSDAT, NDSTRC,   &
  136                                  NTRACE, IERR, IOTEST, I,J,K, IFI,IFJ,&
  137                                  ISEA, IX, IY, TOUT(2), NOUT, TDUM(2),&
  138                                  FTIME(2), CID, PID, GID, GDS, IOUT,  &
  139                                  GDTN
  140       INTEGER, ALLOCATABLE    :: IFIA(:),IFJA(:)
  141 ! GRIB1 specific variables
  142       INTEGER                 :: KPDS(1), KGDS(1)
  143 ! GRIB2 specific variables
  144       REAL                    :: DTREQ, DTEST, RFTIME
  145       LOGICAL                 :: FLREQ(NOGRP,NGRPP), FLGRIB(NOGRP,NGRPP)
  146       CHARACTER               :: COMSTR*1, IDTIME*23, IDDDAY*11
  147       CHARACTER(LEN=80)       :: LINEIN
  148       CHARACTER(LEN=8)        :: WORDS(5)
  149       INTEGER                 :: GEN_PRO
  150                                         
  151 !/
  152 !/ ------------------------------------------------------------------- /
  153 !/
  154 !
  155 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  156 ! 1.  IO set-up.
  157 !
  158       CALL W3NMOD ( 1, 6, 6 )
  159       CALL W3SETG ( 1, 6, 6 )
  160       CALL W3NDAT (    6, 6 )
  161       CALL W3SETW ( 1, 6, 6 )
  162       CALL W3NAUX (    6, 6 )
  163       CALL W3SETA ( 1, 6, 6 )
  164       CALL W3NOUT (    6, 6 )
  165       CALL W3SETO ( 1, 6, 6 )
 
 
                                                         ww3_grib.F90  page   4
 
 
  166 !
  167       NDSI   = 10
  168       NDSM   = 20
  169       NDSOG  = 20
  170       NDSDAT = 50
  171 !
  172       NDSTRC =  6
  173       NTRACE = 10
  174 !
  175       WRITE (NDSO,900)
  176 !
  177       CALL ITRACE ( NDSTRC, NTRACE )
  178 !
  179       OPEN (NDSI,FILE='ww3_grib.inp',STATUS='OLD',ERR=800,IOSTAT=IERR)
  180       READ (NDSI,'(A)',END=801,ERR=802) COMSTR
  181       IF (COMSTR.EQ.' ') COMSTR = '$'
  182       WRITE (NDSO,901) COMSTR
  183 !
  184       WRITE (NDSE,902)
  185 !
  186 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  187 ! 2.  Read model definition file.
  188 !
  189       CALL W3IOGR ( 'READ', NDSM )
  190       WRITE (NDSO,920) GNAME
  191 !
  192       IF ( .NOT. FLAGLL ) GOTO 810
  193 !
  194 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  195 ! 3.  Read requests from input file.
  196 !     Output times
  197 !
  198       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  199       READ (NDSI,'(A)') LINEIN
  200       WRITE(NDSO,*)' LINEIN:  ',LINEIN
  201       READ(LINEIN,*,iostat=ierr) WORDS
  202       WRITE (NDSO,*) WORDS
  203           READ(WORDS( 1 ), * ) TOUT(1)
  204           READ(WORDS( 2 ), * ) TOUT(2)
  205           READ(WORDS( 3 ), * ) DTREQ
  206           READ(WORDS( 4 ), * ) NOUT
  207        IF (WORDS(5) .NE. '0' .AND. WORDS(5) .NE. '1') THEN
  208             GEN_PRO=-99999
  209           ELSE
  210             READ(WORDS( 5 ), * ) GEN_PRO
  211       ENDIF
  212       WRITE(NDSO,*) 'GEN_PRO  ',GEN_PRO
  213       DTREQ  = MAX ( 0. , DTREQ )
  214       IF ( DTREQ.EQ.0 ) NOUT = 1
  215       NOUT   = MAX ( 1 , NOUT )
  216 !
  217       CALL STME21 ( TOUT , IDTIME )
  218       WRITE (NDSO,940) IDTIME
  219 !
  220       TDUM(1) = 0
 
 
                                                         ww3_grib.F90  page   5
 
 
  221       TDUM(2) = 0
  222       CALL TICK21 ( TDUM , DTREQ )
  223       CALL STME21 ( TDUM , IDTIME )
  224       IF ( DTREQ .GE. 86400. ) THEN
  225           WRITE (IDDDAY,'(I10,1X)') INT(DTREQ/86400.)
  226         ELSE
  227           IDDDAY = '           '
  228         END IF
  229       IDTIME(1:11) = IDDDAY
  230       IDTIME(21:23) = '   '
  231       WRITE (NDSO,941) IDTIME, NOUT
  232 !
  233 ! ... Initialize FLGRD array
  234 !
  235       FLREQ(:,:)=.FALSE.
  236 !
  237 ! ... Call to interface for reading flags or namelists
  238 !
  239       CALL W3READFLGRD ( NDSI, NDSO, 9, NDSE, COMSTR, FLOGD, FLREQ,   &
  240                          1, 1, IERR )
  241 !
  242 ! Inform user of parameters that were requested but failed to make the
  243 ! grade, as they are not available for grib encoding, or are not
  244 ! included presently
  245 !
  246       WRITE (NDSO,944)
  247 ! Reset flags for variables not yet implemented in grib output
  248 ! interface
  249 !
  250       IFI = 3 ! Entire group Frequency-dependent parameters
  251       DO IFJ = 1,NOGE(IFI)
  252         IF ( FLREQ(IFI,IFJ) ) THEN
  253           WRITE (NDSO,946) IDOUT(IFI,IFJ),                           &
  254                            '*** NOT YET CODED INTO WW3_GRIB ***'
  255           FLREQ(IFI,IFJ) = .FALSE.
  256         END IF
  257       END DO
  258 !
  259       IFI = 5 ! Atm-waves layer, all except for friction velocity
  260       DO IFJ = 2,10
  261         IF ( FLREQ(IFI,IFJ) ) THEN
  262           WRITE (NDSO,946) IDOUT(IFI,IFJ),                           &
  263                            '*** NOT YET CODED INTO WW3_GRIB ***'
  264           FLREQ(IFI,IFJ) = .FALSE.
  265         END IF
  266       END DO
  267       DO IFI = 6,8 ! Entire groups wave-ocean interaction, wave-bottom
  268                    ! layer and spectrum parameters
  269         DO IFJ = 1,NOGE(IFI)
  270           IF ( FLREQ(IFI,IFJ) ) THEN
  271             WRITE (NDSO,946) IDOUT(IFI,IFJ),                           &
  272                              '*** NOT YET CODED INTO WW3_GRIB ***'
  273             FLREQ(IFI,IFJ) = .FALSE.
  274           END IF
  275         END DO
 
 
                                                         ww3_grib.F90  page   6
 
 
  276       END DO
  277       IF ( FLREQ(9,5) ) THEN ! CFL number for K advection
  278        WRITE (NDSO,946) IDOUT(9,5),'*** NOT YET CODED INTO WW3_GRIB ***'
  279         FLREQ(9,5) = .FALSE.
  280       END IF
  281       IFI = 10 ! User defined parameters
  282       DO IFJ = 1,NOGE(IFI)
  283         IF ( FLREQ(IFI,IFJ) ) THEN
  284           WRITE (NDSO,946) IDOUT(IFI,IFJ),                           &
  285                            '*** NOT YET CODED INTO WW3_GRIB ***'
  286           FLREQ(IFI,IFJ) = .FALSE.
  287         END IF
  288       END DO
  289 !
  290 ! Compatibility with NCEP operational codes, same effect as old FLGRIB
  291 !  lists variables that have no code for variable names (not 100%
  292 !  correct in old codes... )
  293 !
  294 ! Chage this as parameters become available in grib2 tables
  295 !
  296       ALLOCATE ( IFIA (13), IFJA(13) )
  297                                       
  298       IFIA = (/ 1, 2, 2, 4, 4, 4, 4, 4, 5, 9, 9, 9, 9 /)
  299       IFJA = (/ 4, 2, 8, 3, 5, 6, 7, 8, 1, 1, 2, 3, 4 /)
  300       DO I = 1, 13
  301         IF ( FLREQ(IFIA(I),IFJA(I)) ) THEN
  302           FLREQ(IFIA(I),IFJA(I)) = .FALSE.
  303           WRITE(NDSO,946) IDOUT(IFIA(I),IFJA(I)),                      &
  304                         '*** EXCLUDED FROM GRIB OUTPUT ***'
  305         END IF
  306       END DO
  307 !
  308 ! Write to stdout parameters that have successfully been requested
  309 !
  310       WRITE (NDSO,945)
  311        DO I=1, NOGRP
  312        DO J=1, NGRPP
  313         IF ( FLREQ(I,J) ) WRITE (NDSO,931) IDOUT(I,J)
  314         END DO
  315         END DO
  316 !
  317 ! ... GRIB specific parameters
  318 !
  319       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  320       READ (NDSI,*,END=801,ERR=802) FTIME, CID, PID, GID, GDS, GDTN
  321 !
  322 ! Check if grid type is curvilinear, and only go on if Lambert conformal
  323 ! or PolarStereo
  324 !
  325       IF ( GTYPE .EQ. CLGTYPE ) THEN
  326         WRITE(NDSE,*)'PROGRAM W3GRIB: CURVILINEAR GRID SUPPORT '// &
  327         'FOR GRIB OUTPUT IS NOT YET IMPLEMENTED. NOW STOPPING'
  328         CALL EXTCDE ( 1 )
  329       END IF
  330 !
 
 
                                                         ww3_grib.F90  page   7
 
 
  331 ! Coded up to now only for Lamber conformal grids (GDTN=30) or
  332 ! PolarStereo (GDTN=20). For regular grids use GDTN=0
  333 !
  334  
  335 !
  336       CALL STME21 ( FTIME , IDTIME )
  337       WRITE (NDSO,948) IDTIME, CID, PID, GID, GDS
  338 !
  339 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  340 ! 4.  Read general data and first fields from file
  341 ! 4.a Read file.
  342 !
  343       CALL W3IOGO ( 'READ', NDSOG, IOTEST )
  344 !
  345 ! 4.b Output fields in file
  346 !
  347       WRITE (NDSO,930)
  348       DO I=1, NOGRP
  349        DO J=1, NGRPP
  350         IF ( FLOGRD(I,J) ) WRITE (NDSO,931) IDOUT(I,J)
  351         END DO
  352         END DO
  353 !
  354 ! 4.c Flip MAPSF for REGULAR/RECTILINEAR grids
  355 !
  356       DO ISEA=1, NSEA
  357         IX            = MAPSF(ISEA,1)
  358         IY            = MAPSF(ISEA,2)
  359         MAPSF(ISEA,2) = NY + 1 - IY
  360         MAPSF(ISEA,3) = IY +( IX-1)*NY
  361         END DO
  362 !
  363 !--- - - - - - - - - - - - - - - - - - - - - - - - - -
  364 ! 5. Set grib encoding parameter Sections
  365 !
  366 ! ... Initialize KPDS and KGDS (for both NCEP1 and NCEP2)
  367 !
  368       KPDS     =   0
  369       KGDS     =   0
  370 !
  371 ! ... Set PDS GRIB1 elements
  372 !
  373 !     ( 1)  ID OF CENTER
  374 !     ( 2)  GENERATING PROCESS ID NUMBER
  375 !     ( 3)  GRID DEFINITION
  376 !     ( 4)  GDS/BMS FLAG (RIGHT ADJ COPY OF OCTET 8)
  377 !  ** ( 5)  INDICATOR OF PARAMETER
  378 !     ( 6)  TYPE OF LEVEL
  379 !     ( 7)  HEIGHT/PRESSURE , ETC OF LEVEL
  380 !   * ( 8)  YEAR of century
  381 !   * ( 9)  MONTH OF YEAR
  382 !   * (10)  DAY OF MONTH
  383 !   * (11)  HOUR OF DAY
  384 !     (12)  MINUTE OF HOUR
  385 !     (13)  INDICATOR OF FORECAST TIME UNIT
 
 
                                                         ww3_grib.F90  page   8
 
 
  386 !   * (14)  TIME RANGE 1
  387 !     (15)  TIME RANGE 2
  388 !     (16)  TIME RANGE FLAG
  389 !     (17)  NUMBER INCLUDED IN AVERAGE
  390 !     (18)  VERSION NR OF GRIB SPECIFICATION
  391 !     (19)  VERSION NR OF PARAMETER TABLE
  392 !     (20)  NR MISSING FROM AVERAGE/ACCUMULATION
  393 !   * (21)  CENTURY OF REFERENCE TIME OF DATA
  394 !     (22)  UNITS DECIMAL SCALE FACTOR
  395 !     (23)  SUBCENTER NUMBER
  396 !     (24)  PDS BYTE 29, FOR NMC ENSEMBLE PRODUCTS
  397 !     (25)  PDS BYTE 30, NOT USED
  398 !
  399 !  * : Changing on the fly in main program
  400 ! ** : Changing on the fly in W3EXGB
  401 !
  402 ! ... Set GDS GRIB1 elements
  403 !
  404 !     ( 1)  DATA REPRESENTATION TYPE
  405 !     ( 2)  N(I) NR POINTS ON LATITUDE CIRCLE
  406 !     ( 3)  N(J) NR POINTS ON LONGITUDE MERIDIAN
  407 !     ( 4)  LA(1) LATITUDE OF ORIGIN
  408 !     ( 5)  LO(1) LONGITUDE OF ORIGIN
  409 !     ( 6)  RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
  410 !     ( 7)  LA(2) LATITUDE OF EXTREME POINT
  411 !     ( 8)  LO(2) LONGITUDE OF EXTREME POINT
  412 !     ( 9)  DI LONGITUDINAL DIRECTION OF INCREMENT
  413 !     (10)  DJ LATITUDINAL DIRECTION INCREMENT
  414 !     (11)  SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
  415 !     (19)  NUMBER OF VERTICAL COORDINATE PARAMETERS
  416 !     (20)  OCTET NUMBER OF THE LIST OF VERTICAL COORDINATE
  417 !           PARAMETERS OR OCTET NUMBER OF THE LIST OF NUMBERS
  418 !           OF POINTS IN EACH ROW OR 255 IF NEITHER ARE PRESENT
  419 !     (21)  FOR GRIDS WITH PL, NUMBER OF POINTS IN GRID
  420 !     (22)  NUMBER OF WORDS IN EACH ROW
  421 !
  422 ! ... Set GRIB2 packing arrays
  423 !
  424 ! ... Set GRIB2 Indicator Section
  425 !      ( 1)  Discipline-GRIB Master Table Number (see Code Table 0.0)
  426 !            0 = Metereological; 10 = Oceanographic
  427 !      ( 2)  GRIB Edition Number
  428 !      ( 3)
  429 !
  430 ! ... Set GRIB2 Identification Section
  431 !       ( 1) ID OF CENTER
  432 !       ( 2) ID OF SUB-CENTER
  433 !       ( 3) GRIB Master Tables Version Number (Code Table 1.0)
  434 !       ( 4) GRIB Local Tables Version Number (Code Table 1.0)
  435 !       ( 5) Significance of Reference Time (Code Table 1.2)
  436 !     * ( 6) YEAR (4 digits)
  437 !     * ( 7) MONTH OF YEAR
  438 !     * ( 8) DAY OF MONTH
  439 !     * ( 9) HOUR OF DAY
  440 !       (10) MINUTE OF HOUR
 
 
                                                         ww3_grib.F90  page   9
 
 
  441 !       (11) SECOND OF MINUTE
  442 !       (12) Production status of data (Code Table 1.3)
  443 !       (13) Type of processed data (Code Table 1.4)
  444 !
  445 ! ... Set GRIB2 IGDS elements
  446 !       ( 1) Source of grid definition (Code Table 3.0)
  447 !       ( 2) Number of grid points
  448 !       ( 3) Number of octets needed for each additional grid points defi>
  449 !       ( 4) Interpretation of list for optional points definition (Code >
  450 !       ( 5) Grid definition template number (Code Table 3.1)
  451 !
  452 ! ... Set GRIB2 KGDS elements
  453 !
  454 ! General parameters for all grids
  455 !       ( 1) Coordinate system (6 = spherical coordinate system with radi>
  456 !       ( 2)
  457 !       ( 3)
  458 !       ( 4)
  459 !       ( 5)
  460 !       ( 6)
  461 !       ( 7)
  462 !       ( 8) Number of points along parallel
  463 !       ( 9) Number of points along meridian
  464 !
  465 ! Lambert Conformal grid
  466 !       (10) Latitude of first grid point
  467 !       (11) Longitude of first grid point
  468 !       (12) Resolution and component flags
  469 !       (13) Latitude where DX and DY are specified
  470 !       (14) Longitude of orientation
  471 !       (15) Increment of longitude
  472 !       (16) Increment of latitude
  473 !       (17) Projection center flag
  474 !       (18) Scanning mode
  475 !       (19) First latitude of secant cone
  476 !       (20) Second latitude of secant cone
  477 !       (21) Latitude of southern pole
  478 !       (22) Longitude of southern pole
  479 !
  480 ! PolarStereo grid
  481 !       (10) Latitude of first grid point
  482 !       (11) Longitude of first grid point
  483 !       (12) Res and component flags
  484 !       (13) Latitude where DX and DY are specified
  485 !       (14) Longitude of orientation
  486 !       (15) Increment of longitude
  487 !       (16) Increment of latitude
  488 !       (17) Projection center flag
  489 !       (18) Scanning mode
  490 !
  491 ! Lat Lon rectilinear grid
  492 !       (10)
  493 !       (11)
  494 !       (12) Latitude of first grid point
  495 !       (13) Longitude of first grid point
 
 
                                                         ww3_grib.F90  page  10
 
 
  496 !       (14) Res and component flags
  497 !       (15) Latitude of last grid point
  498 !       (16) Longitude of last grid point
  499 !       (17) Increment of longitude
  500 !       (18) Increment of latitude
  501 !       (19) Scanning mode
  502 !
  503 ! ... Set GRIB2 PDS elements
  504 !     KPDSNUM   (0 indicates forecast at a horizontal level)
  505 !     ( 1) Parameter category (Code Table 4.1)
  506 !            For oceanographic products -- 0 = waves; 1 = currents; 2 = i>
  507 !            For atmospheric products   -- 2 = momentum
  508 !     ( 2) Parameter number (Code Table 4.2)
  509 !     ( 3) Generating process (Code Table 4.3)
  510 !     ( 4) Background generating process identifier (center specific)
  511 !     ( 5) Process or model number
  512 !     ( 6) Hours of observational data cutoff after reference time
  513 !     ( 7) Minutes of observational data cutoff after reference time
  514 !     ( 8) Indicator of forecast time unit (Code Table 4.4)
  515 !     ( 9) Time range
  516 !     (10) Type of level (Code Table 4.5) 1st level
  517 !     (11) Scaled factor of (10)
  518 !     (12) Scaled value of (10)
  519 !     (13) Type of level (Code Table 4.5) 2nd level
  520 !     (14) Scaled factor of (13)
  521 !     (15) Scaled value of (13)
  522 !
  523 !      KPDS(3)=4 ensemble forecast:ww3_grib.inp has gen_pro set to 1
  524 !             =2 deterministic forecast: ww3_grib.inp gen_pro set to 0
  525 !             =2 legacy :with no gen_pro set in ww3_grib.inp
  526 !              (in the case of legacy the params revert back to old names)
  527 !
  528 ! ... Set GRIB2 vertical layer information
  529 !
  530 ! ... Set GRIB2 bitmap information
  531 !      0 Bitmap is provided
  532 !
  533 ! ... Set GRIB2 Data Representation Template Number (Code Table 5.0)
  534 !
  535 !                            clusters with Intel compiler ***
  536 !
  537 ! ... Set GRIB2 IDRS elements
  538 !     ( 1) Reference value (R) (IEEE 32-bit floating-point value)
  539 !     ( 2) Binary Scale Factor (E)
  540 !     ( 3) Decimal Scale Factor (D)
  541 !     ( 4) Number of bits used for each packed value
  542 !     ( 5) Type of original field values (Code Table 5.1)
  543 !
  544 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  545 ! 6.  Time management.
  546 !
  547       IOUT   = 0
  548       WRITE (NDSO,970)
  549 !
  550       DO
 
 
                                                         ww3_grib.F90  page  11
 
 
  551         DTEST  = DSEC21 ( TIME , TOUT )
  552         IF ( DTEST .GT. 0. ) THEN
  553             CALL W3IOGO ( 'READ', NDSOG, IOTEST )
  554             IF ( IOTEST .EQ. -1 ) THEN
  555                 WRITE (NDSO,942)
  556                 GOTO 888
  557               END IF
  558             CYCLE
  559           END IF
  560         IF ( DTEST .LT. 0. ) THEN
  561             CALL TICK21 ( TOUT , DTREQ )
  562             CYCLE
  563           END IF
  564 !
  565         IOUT   = IOUT + 1
  566         CALL STME21 ( TOUT , IDTIME )
  567 !
  568         RFTIME = DSEC21 ( FTIME , TIME ) / 3600.
  569         IF ( RFTIME .LT. 0. ) THEN
  570             WRITE (NDSO,972) IDTIME
  571           ELSE
  572             WRITE (NDSO,971) IDTIME, NINT(RFTIME)
  573           END IF
  574 !
  575         CALL W3EXGB ( NX, NY, NSEA )
  576         CALL TICK21 ( TOUT , DTREQ )
  577         IF ( IOUT .GE. NOUT ) EXIT
  578         END DO
  579 !
  580       GOTO 888
  581 !
  582 ! Escape locations read errors :
  583 !
  584   800 CONTINUE
  585       WRITE (NDSE,1000) IERR
  586       CALL EXTCDE ( 3 )
  587 !
  588   801 CONTINUE
  589       WRITE (NDSE,1001)
  590       CALL EXTCDE ( 4 )
  591 !
  592   802 CONTINUE
  593       WRITE (NDSE,1002) IERR
  594       CALL EXTCDE ( 5 )
  595 !
  596   810 CONTINUE
  597       IF ( .NOT. FLAGLL ) THEN
  598           WRITE (NDSE,1010)
  599           CALL EXTCDE ( 10 )
  600         END IF
  601 !
  602   888 CONTINUE
  603       WRITE (NDSO,999)
  604 !
  605 ! Formats
 
 
                                                         ww3_grib.F90  page  12
 
 
  606 !
  607   900 FORMAT (/15X,'   *** WAVEWATCH III GRIB output postp. ***   '/ &
  608                15X,'=============================================='/)
  609   901 FORMAT ( '  Comment character is ''',A,''''/)
  610   902 FORMAT (/'  *** WARNING : NO GRIB PACKAGE LINKED ***'/)
  611 !
  612   920 FORMAT ( '  Grid name : ',A/)
  613 !
  614   930 FORMAT ( '  Fields in file : '/                                 &
  615                ' --------------------------')
  616   931 FORMAT ( '      ',A)
  617 !
  618   940 FORMAT (/'  Output time data : '/                               &
  619                ' -----------------------------------------------------'/ &
  620                '      First time         : ',A)
  621   941 FORMAT ( '      Interval           : ',A/                       &
  622                '      Number of requests : ',I4)
  623   942 FORMAT (/'      End of file reached '/)
  624 !
  625   944 FORMAT (/'  Requested output fields not yet available: '/        &
  626                ' -----------------------------------------------------')
  627 !
  628   945 FORMAT (/'  Successfully requested output fields : '/            &
  629                ' -----------------------------------------------------')
  630   946 FORMAT ( '      ',A,1X,A)
  631 !
  632   948 FORMAT (/'  Additional GRIB parameters : '/                     &
  633                ' -----------------------------------------------------'/ &
  634                '      Run time           : ',A/                       &
  635                '      GRIB center ID     : ',I4/                      &
  636                '      GRIB gen. proc. ID : ',I4/                      &
  637                '      GRIB grid ID       : ',I4/                      &
  638                '      GRIB GDS parameter : ',I4)
  639 !
  640   970 FORMAT (//'  Generating file '/                                 &
  641                ' -----------------------------------------------------')
  642   971 FORMAT ( '      Data for ',A,'  ',I3,'H forecast.')
  643   972 FORMAT ( '      Data for ',A,'       hindcast.')
  644 !
  645   999 FORMAT (/'  End of program '/                                   &
  646                ' ========================================='/          &
  647                '         WAVEWATCH III GRIB output '/)
  648 !
  649  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRIB : '/               &
  650                '     ERROR IN OPENING INPUT FILE'/                    &
  651                '     IOSTAT =',I5/)
  652 !
  653  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRIB : '/               &
  654                '     PREMATURE END OF INPUT FILE'/)
  655 !
  656  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRIB : '/               &
  657                '     ERROR IN READING FROM INPUT FILE'/               &
  658                '     IOSTAT =',I5/)
  659 !
  660  1010 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRIB : '/          &
 
 
                                                         ww3_grib.F90  page  13
 
 
  661                '     GRIB REQUIRES SPHERICAL GRID'/)
  662 !/
  663 !/ Internal subroutine W3EXGB ---------------------------------------- /
  664 !/
  665       CONTAINS
  666 !/ ------------------------------------------------------------------- /
  667       SUBROUTINE W3EXGB ( NX, NY, NSEA )
  668 !/
  669 !/                  +-----------------------------------+
  670 !/                  | WAVEWATCH III           NOAA/NCEP |
  671 !/                  |           H. L. Tolman            |
  672 !/                  |             A. Chawla             |
  673 !/                  |                        FORTRAN 90 |
  674 !/                  | Last update :         16-Jul-2007 |
  675 !/                  +-----------------------------------+
  676 !/
  677 !/    10-Jun-1999 : Final FORTRAN 77                    ( version 1.18 )
  678 !/    24-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
  679 !/                  Massive changes to logistics.
  680 !/    29-Apr-2002 : Adding output fields 17-18.         ( version 2.20 )
  681 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
  682 !/    18-May-2007 : Update GRIB1 for partitioning.      ( version 3.11 )
  683 !/    16-Jul-2007 : Adding GRIB2 capability             ( version 3.11 )
  684 !/                  (A. Chawla)
  685 !/
  686 !  1. Purpose :
  687 !
  688 !     Perform actual GRIB output.
  689 !
  690 !  3. Parameters :
  691 !
  692 !     Parameter list
  693 !     ----------------------------------------------------------------
  694 !       NX, NY, NSEA
  695 !               Int.  I  Array dimensions.
  696 !     ----------------------------------------------------------------
  697 !
  698 !     Internal parameters
  699 !     ----------------------------------------------------------------
  700 !       X1, X2, XX, XY
  701 !               R.A.  Output fields
  702 !       BITMAP  L.A.  Data / no data bitmap
  703 !     ----------------------------------------------------------------
  704 !
  705 !  4. Subroutines used :
  706 !
  707 !      Name      Type  Module   Description
  708 !     ----------------------------------------------------------------
  709 !      STRACE     Subr. W3SERVMD Subroutine tracing.
  710 !      EXTCDE     Subr.   Id.    Abort program as graceful as possible.
  711 !      W3S2XY     Subr.   Id.    Convert from storage to spatial grid.
  712 !      PUTGB      Subr.          NCEP GRIB1 library routine.
  713 !      GRIBCREATE Subr.          NCEP GRIB2 library routine.
  714 !      ADDGRID    Subr.          NCEP GRIB2 library routine.
  715 !      ADDFIELD   Subr.          NCEP GRIB2 library routine.
 
 
                                                         ww3_grib.F90  page  14
 
 
  716 !      GRIBEND    Subr.          NCEP GRIB2 library routine.
  717 !      WRYTE      Subr.          NCEP GRIB2 library routine.
  718 !     ----------------------------------------------------------------
  719 !
  720 !  5. Called by :
  721 !
  722 !     Program in which it is contained.
  723 !
  724 !  6. Error messages :
  725 !
  726 !     None.
  727 !
  728 !  7. Remarks :
  729 !
  730 !     - Note that arrays CX and CY of the main program now contain
  731 !       the absolute current speed and direction respectively.
  732 !
  733 !  8. Structure :
  734 !
  735 !     See source code.
  736 !
  737 !  9. Switches :
  738 !
  739 !     !/S      Enable subroutine tracing.
  740 !     !/T      Enable test output.
  741 !     !/NCEP1  NCEP IBM calls to GRIB1 packer.
  742 !     !/NCEP2  NCEP IBM calls to GRIB2 packer (follows updated grib2
  743 !     tables under verification as of 02/10/2012).
  744 !
  745 ! 10. Source code :
  746 !
  747 !/ ------------------------------------------------------------------- /
  748       USE W3SERVMD, ONLY : W3S2XY
  749 !/
  750 !/ ------------------------------------------------------------------- /
  751 !/ Parameter list
  752 !/
  753       INTEGER, INTENT(IN)     :: NX, NY, NSEA
  754 !/
  755 !/ ------------------------------------------------------------------- /
  756 !/ Local parameters
  757 !/
  758       INTEGER                 :: J, IXY, NDATA
  759       INTEGER                 :: IO
  760       REAL                    ::  X1(NX*NY), X2(NX*NY), XX(NX*NY),    &
  761                                   XY(NX*NY), CABS, UABS,              &
  762                                   YY(NX*NY,0:NOSWLL), KPDS5A, KPDS5B, &
  763                                   KPDS5A1(3)
  764       LOGICAL*1               ::  BITMAP(NX*NY)
  765       LOGICAL                 :: FLONE, FLTWO, FLDIR, FLTRI, FLPRT
  766 !/
  767 !/ ------------------------------------------------------------------- /
  768 !/
  769 !
  770 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
                                                         ww3_grib.F90  page  15
 
 
  771 ! 1.  Preparations
  772 !
  773       X1     = UNDEF
  774       X2     = UNDEF
  775       XX     = UNDEF
  776       XY     = UNDEF
  777       YY     = UNDEF
  778 !
  779 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  780 ! 2.  Loop over output fields.
  781 !
  782       DO IFI=1, NOGRP
  783        DO IFJ=1, NGRPP
  784         IF ( FLREQ(IFI,IFJ) ) THEN
  785                                   
  786                                   
  787 !
  788 ! Initialize array dimension flags
  789 !
  790         FLONE = .FALSE.
  791         FLTWO = .FALSE.
  792         FLDIR = .FALSE.
  793         FLTRI = .FALSE.
  794         FLPRT = .FALSE.
  795 !
  796 ! 2.a Set output arrays and parameters
  797 !
  798 !     Water depth
  799 !
  800             IF ( IFI .EQ. 1 .AND. IFJ .EQ.  1 ) THEN
  801                 FLONE = .TRUE.
  802                 CALL W3S2XY ( NSEA, NSEA, NX, NY, DW(1:NSEA)          &
  803                                                         , MAPSF, X1 )
  804 !
  805 !     Current
  806 !
  807               ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ.  2 ) THEN
  808                 FLTWO = .TRUE.
  809                 CALL W3S2XY ( NSEA, NSEA, NX, NY, CX(1:NSEA)          &
  810                                                         , MAPSF, XX )
  811                 CALL W3S2XY ( NSEA, NSEA, NX, NY, CY(1:NSEA)          &
  812                                                         , MAPSF, XY )
  813                 DO ISEA=1, NSEA
  814                   IF (CX(ISEA) .NE. UNDEF) THEN
  815                      CABS   = SQRT(CX(ISEA)**2+CY(ISEA)**2)
  816                      IF ( CABS .GT. 0.001 ) THEN
  817                          CY(ISEA) = MOD ( 630. -                         &
  818                                RADE*ATAN2(CY(ISEA),CX(ISEA)) , 360. )
  819                        ELSE
  820                          CY(ISEA) = 0.
  821                        END IF
  822                     ELSE
  823                        CABS = UNDEF
  824                        CY(ISEA) = UNDEF
  825                      END IF
 
 
                                                         ww3_grib.F90  page  16
 
 
  826                   CX(ISEA) = CABS
  827                   END DO
  828                 CALL W3S2XY ( NSEA, NSEA, NX, NY, CX(1:NSEA)          &
  829                                                         , MAPSF, X1 )
  830                 CALL W3S2XY ( NSEA, NSEA, NX, NY, CY(1:NSEA)          &
  831                                                         , MAPSF, X2 )
  832 !
  833 !     Wind speed
  834 !
  835               ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ.  3 ) THEN
  836                 FLTWO = .TRUE.
  837                 CALL W3S2XY ( NSEA, NSEA, NX, NY, UA(1:NSEA)          &
  838                                                         , MAPSF, XX )
  839                 CALL W3S2XY ( NSEA, NSEA, NX, NY, UD(1:NSEA)          &
  840                                                         , MAPSF, XY )
  841                 DO ISEA=1, NSEA
  842                   IF (UA(ISEA) .NE. UNDEF) THEN
  843                      UABS   = SQRT(UA(ISEA)**2+UD(ISEA)**2)
  844                      IF ( UABS .GT. 0.001 ) THEN
  845                          UD(ISEA) = MOD ( 630. -                         &
  846                                RADE*ATAN2(UD(ISEA),UA(ISEA)) , 360. )
  847                        ELSE
  848                          UD(ISEA) = 0.
  849                        END IF
  850                     ELSE
  851                        UABS = UNDEF
  852                        UD(ISEA) = UNDEF
  853                      END IF
  854                   UA(ISEA) = UABS
  855                   END DO
  856                 CALL W3S2XY ( NSEA, NSEA, NX, NY, UA(1:NSEA)          &
  857                                                         , MAPSF, X1 )
  858                 CALL W3S2XY ( NSEA, NSEA, NX, NY, UD(1:NSEA)          &
  859                                                         , MAPSF, X2 )
  860 !
  861 !     Air-sea temp. dif.
  862 !
  863               ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ.  4 ) THEN
  864                 FLONE = .TRUE.
  865                 CALL W3S2XY ( NSEA, NSEA, NX, NY, AS(1:NSEA)          &
  866                                                         , MAPSF, X1 )
  867 !
  868 !     Water level
  869 !
  870               ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 5 ) THEN
  871                 FLONE = .TRUE.
  872                 CALL W3S2XY ( NSEA, NSEA, NX, NY, WLV   , MAPSF, X1 )
  873 !
  874 !     Ice concentration
  875 !
  876               ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 6 ) THEN
  877                 FLONE = .TRUE.
  878                 CALL W3S2XY ( NSEA, NSEA, NX, NY, ICE   , MAPSF, X1 )
  879 !
  880 !     Significant wave height
 
 
                                                         ww3_grib.F90  page  17
 
 
  881 !
  882               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ.  1 ) THEN
  883                 FLONE = .TRUE.
  884                 CALL W3S2XY ( NSEA, NSEA, NX, NY, HS    , MAPSF, X1 )
  885 !
  886 !     Mean wave length
  887 !
  888               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ.  2 ) THEN
  889                 FLONE = .TRUE.
  890                 CALL W3S2XY ( NSEA, NSEA, NX, NY, WLM   , MAPSF, X1 )
  891 !
  892 !     Mean wave period (based on second moment)
  893 !
  894               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ.  3 ) THEN
  895                 FLONE = .TRUE.
  896                               
  897                 CALL W3S2XY ( NSEA, NSEA, NX, NY, T02   , MAPSF, X1 )
  898 !
  899 !     Mean wave period (based on first moment)
  900 !
  901               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ.  4 ) THEN
  902                 FLONE = .TRUE.
  903                 CALL W3S2XY ( NSEA, NSEA, NX, NY, T0M1   , MAPSF, X1 )
  904 !
  905 !     Mean wave period (based on first inverse moment)
  906 !
  907               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ.  5 ) THEN
  908                 FLONE = .TRUE.
  909                 CALL W3S2XY ( NSEA, NSEA, NX, NY, T01   , MAPSF, X1 )
  910 !
  911 !     Peak frequency
  912 !
  913               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 6 ) THEN
  914                 FLONE = .TRUE.
  915                 DO ISEA=1, NSEA
  916                   IF ( FP0(ISEA) .NE. UNDEF .AND. FP0(ISEA) .NE. 0 ) THEN
  917                       FP0(ISEA) = 1. / MAX(FR1,FP0(ISEA)) ! Limit FP to l>
  918                     END IF
  919                   END DO
  920                 CALL W3S2XY ( NSEA, NSEA, NX, NY, FP0   , MAPSF, X1 )
  921 !
  922 !     Mean wave direction
  923 !
  924               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ.  7 ) THEN
  925                 FLONE = .TRUE.
  926                 DO ISEA=1, NSEA
  927                   IF ( THM(ISEA) .NE. UNDEF )                         &
  928                        THM(ISEA) = MOD ( 630. - RADE*THM(ISEA) , 360. )
  929                   END DO
  930                 CALL W3S2XY ( NSEA, NSEA, NX, NY, THM   , MAPSF, X1 )
  931 !
  932 !     Directional spread
  933 !
  934               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 8 ) THEN
  935                 FLONE = .TRUE.
 
 
                                                         ww3_grib.F90  page  18
 
 
  936                 CALL W3S2XY ( NSEA, NSEA, NX, NY, THS   , MAPSF, X1 )
  937 !
  938 !     Peak direction
  939 !
  940               ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 9 ) THEN
  941                 FLONE = .TRUE.
  942                 DO ISEA=1, NSEA
  943                   IF ( THP0(ISEA) .NE. UNDEF ) THEN
  944                       THP0(ISEA) = MOD ( 630-RADE*THP0(ISEA) , 360. )
  945                     END IF
  946                   END DO
  947                 CALL W3S2XY ( NSEA, NSEA, NX, NY, THP0  , MAPSF, X1 )
  948 !
  949 !     Partitioned wave height
  950 !
  951               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 1 ) THEN
  952                 FLPRT = .TRUE.
  953                 CALL W3S2XY                                           &
  954                         ( NSEA, NSEA, NX, NY, PHS(:,0), MAPSF, YY(:,0) )
  955                 DO I=1, NOSWLL
  956                   CALL W3S2XY                                         &
  957                         ( NSEA, NSEA, NX, NY, PHS(:,I), MAPSF, YY(:,I) )
  958                   END DO
  959 !
  960 !     Partitioned peak period
  961 !
  962               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 2 ) THEN
  963                 FLPRT = .TRUE.
  964                 CALL W3S2XY                                           &
  965                         ( NSEA, NSEA, NX, NY, PTP(:,0), MAPSF, YY(:,0) )
  966                 DO I=1, NOSWLL
  967                   CALL W3S2XY                                         &
  968                         ( NSEA, NSEA, NX, NY, PTP(:,I), MAPSF, YY(:,I) )
  969                   END DO
  970 !
  971 !     Partitioned peak wave length
  972 !
  973               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 3 ) THEN
  974                 FLPRT = .TRUE.
  975                 CALL W3S2XY                                           &
  976                         ( NSEA, NSEA, NX, NY, PLP(:,0), MAPSF, YY(:,0) )
  977                 DO I=1, NOSWLL
  978                   CALL W3S2XY                                         &
  979                         ( NSEA, NSEA, NX, NY, PLP(:,I), MAPSF, YY(:,I) )
  980                   END DO
  981 !
  982 !     Partitioned mean direction
  983 !
  984               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 4 ) THEN
  985                 FLPRT = .TRUE.
  986                 DO ISEA = 1,NSEA
  987                   DO I = 0,NOSWLL
  988                      IF ( PDIR(ISEA,I) .NE. UNDEF ) THEN
  989                         PDIR(ISEA,I) = MOD ( 630 - RADE*PDIR(ISEA,I) , 36>
  990                         END IF
 
 
                                                         ww3_grib.F90  page  19
 
 
  991                      END DO
  992                   END DO
  993                 CALL W3S2XY                                           &
  994                         ( NSEA, NSEA, NX, NY, PDIR(:,0), MAPSF, YY(:,0) )
  995                 DO I=1, NOSWLL
  996                   CALL W3S2XY                                         &
  997                         ( NSEA, NSEA, NX, NY, PDIR(:,I), MAPSF, YY(:,I) )
  998                   END DO
  999 !
 1000 !     Partitioned Directional spread
 1001 !
 1002               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 5 ) THEN
 1003                 FLPRT = .TRUE.
 1004                 CALL W3S2XY                                           &
 1005                         ( NSEA, NSEA, NX, NY, PSI(:,0), MAPSF, YY(:,0) )
 1006                 DO I=1, NOSWLL
 1007                   CALL W3S2XY                                         &
 1008                         ( NSEA, NSEA, NX, NY, PSI(:,I), MAPSF, YY(:,I) )
 1009                   END DO
 1010 !
 1011 !     Partitioned wind sea fraction
 1012 !
 1013               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 6 ) THEN
 1014                 FLPRT = .TRUE.
 1015                 CALL W3S2XY                                           &
 1016                         ( NSEA, NSEA, NX, NY, PWS(:,0), MAPSF, YY(:,0) )
 1017                 DO I=1, NOSWLL
 1018                   CALL W3S2XY                                         &
 1019                         ( NSEA, NSEA, NX, NY, PWS(:,I), MAPSF, YY(:,I) )
 1020                   END DO
 1021 !
 1022 !     Total wind sea fraction
 1023 !
 1024               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 16 ) THEN
 1025                 FLONE = .TRUE.
 1026                 CALL W3S2XY ( NSEA, NSEA, NX, NY, PWST  , MAPSF, X1 )
 1027 !
 1028 !     Number of fields in partition
 1029 !
 1030               ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 17 ) THEN
 1031                 FLONE = .TRUE.
 1032                 CALL W3S2XY ( NSEA, NSEA, NX, NY, PNR   , MAPSF, X1 )
 1033 !
 1034 !     Friction velocity
 1035 !
 1036               ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ.  1 ) THEN
 1037                 FLTWO = .TRUE.
 1038                 CALL W3S2XY ( NSEA, NSEA, NX, NY, UST(1:NSEA)         &
 1039                                                         , MAPSF, X1 )
 1040                 CALL W3S2XY ( NSEA, NSEA, NX, NY, USTDIR(1:NSEA)      &
 1041                                                         , MAPSF, X2 )
 1042 !
 1043 !     Average source term time step
 1044 !
 1045               ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 1 ) THEN
 
 
                                                         ww3_grib.F90  page  20
 
 
 1046                 FLONE = .TRUE.
 1047                 DO ISEA=1, NSEA
 1048                   IF ( DTDYN(ISEA) .NE. UNDEF )                       &
 1049                        DTDYN(ISEA) = DTDYN(ISEA) / 60.
 1050                   END DO
 1051                 CALL W3S2XY ( NSEA, NSEA, NX, NY, DTDYN , MAPSF, X1 )
 1052 !
 1053 !     Cut-off frequency
 1054 !
 1055               ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 2 ) THEN
 1056                 FLONE = .TRUE.
 1057                 CALL W3S2XY ( NSEA, NSEA, NX, NY, FCUT  , MAPSF, X1 )
 1058 !
 1059 !     CFL Maximum (in spatial space)
 1060 !
 1061               ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 3 ) THEN
 1062                 FLONE = .TRUE.
 1063                 CALL W3S2XY ( NSEA, NSEA, NX, NY, CFLXYMAX  , MAPSF, X1 )
 1064 !
 1065 !     CFL Maximum (in spectral space)
 1066 !
 1067               ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 4 ) THEN
 1068                 FLONE = .TRUE.
 1069                 CALL W3S2XY ( NSEA, NSEA, NX, NY, CFLTHMAX  , MAPSF, X1 )
 1070 !
 1071               ELSE
 1072                 WRITE (NDSE,999)
 1073                 CALL EXTCDE ( 1 )
 1074 !
 1075               END IF
 1076 !
 1077 ! 3   Perform output
 1078 !
 1079             NDATA  = NX*NY
 1080 !
 1081 ! 3.a Partitioned data
 1082 !
 1083             IF ( FLPRT ) THEN
 1084 !
 1085                 DO IXY=1, NX*NY
 1086                   BITMAP(IXY) = YY(IXY,0) .NE. UNDEF
 1087                   END DO
 1088 !
 1089                 DO I=1, NOSWLL
 1090                   DO IXY=1, NX*NY
 1091                     BITMAP(IXY) = YY(IXY,I) .NE. UNDEF
 1092                     END DO
 1093                   END DO
 1094                 DO I=1, NOSWLL
 1095                   DO IXY=1, NX*NY
 1096                     BITMAP(IXY) = YY(IXY,I) .NE. UNDEF
 1097                     END DO
 1098                   END DO
 1099 !
 1100 ! 3.b Other data
 
 
                                                         ww3_grib.F90  page  21
 
 
 1101 !
 1102               ELSE IF (FLONE) THEN
 1103 !
 1104                 DO IXY=1, NX*NY
 1105                   BITMAP(IXY) = X1(IXY) .NE. UNDEF
 1106                   END DO
 1107 !
 1108               ELSE IF ( FLTWO ) THEN
 1109 !
 1110                 DO IXY=1, NX*NY
 1111                   BITMAP(IXY) = X1(IXY) .NE. UNDEF
 1112                   END DO
 1113                         
 1114 !
 1115               END IF
 1116 !
 1117 ! ... End of fields loop
 1118 !
 1119           END IF
 1120         END DO
 1121        END DO
 1122 !
 1123       RETURN
 1124 !
 1125 ! Error escape locations
 1126 !
 1127 ! Formats
 1128 !
 1129   999 FORMAT (/' *** WAVEWATCH III ERROR IN W3EXGB :'/                &
 1130                '     PLEASE UPDATE FIELDS !!! '/)
 1131 !
 1132 !/
 1133 !/ End of W3EXGB ----------------------------------------------------- /
 1134 !/
 1135       END SUBROUTINE W3EXGB
 1136 !/
 1137 !/ End of W3GRIB ----------------------------------------------------- /
 1138 !/
 1139       END PROGRAM W3GRIB
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
