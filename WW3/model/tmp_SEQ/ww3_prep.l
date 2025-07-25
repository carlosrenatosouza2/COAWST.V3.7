                                                         ww3_prep.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3PREP
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |             A. Chawla             |
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :         11-Nov-2013 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    14-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
   14 !/    18-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
   15 !/    11-Jan-2001 : Flat grid option added              ( version 2.06 )
   16 !/    17-Jul-2001 : Clean-up                            ( version 2.11 )
   17 !/    24-Jan-2002 : Add data for data assimilation.     ( version 2.17 )
   18 !/    30-Apr-2002 : Fix 'AI' bug for 1-D fields.        ( version 2.20 )
   19 !/    24-Apr-2003 : Fix bug for NDAT = 0 in data.       ( version 3.03 )
   20 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
   21 !/    28-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   22 !/    25-Sep-2007 : Switch header of file on or off,    ( version 3.13 )
   23 !/                  Times to file (!/O15) (A. Chawla)
   24 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   25 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   26 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   27 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
   28 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   29 !/    15-May-2010 : Add ISI (icebergs and sea ice).     ( version 3.14.4 )
   30 !/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.4 )
   31 !/                  (A. Roland and F. Ardhuin)
   32 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
   33 !/                  specify index closure for a grid.   ( version 3.14 )
   34 !/                  (T. J. Campbell, NRL)
   35 !/     1-Apr-2011 : Fix bug GLOBX forcing with unst.    ( version 3.14.4 )
   36 !/    19-Sep-2011 : Fix bug prep forcing with unst.     ( version 4.04 )
   37 !/    26-Dec-2012 : Modified obsolete declarations.     ( version 4.OF )
   38 !/     3-Mar-2013 : Allows for longer input file name   ( version 4.09 )
   39 !/    11-Nov-2013 : Allows for input binary files to be of WAVEWATCH
   40 !/                  type (i.e. accounts for the header) ( version 4.13 )
   41 !/    20-Jan-2017 : Update to new W3GSRUMD APIs         ( version 6.02 )
   42 !/
   43 !/    Copyright 2009-2012 National Weather Service (NWS),
   44 !/       National Oceanic and Atmospheric Administration.  All rights
   45 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   46 !/       No unauthorized use without permission.
   47 !/
   48 !  1. Purpose :
   49 !
   50 !     Pre-processing of the input water level, current, wind and ice
   51 !     fields as well as assimilation data for the generic shell W3SHEL
   52 !     (ww3_shel.ftn).
   53 !
   54 !  2. Method :
   55 !
 
 
                                                         ww3_prep.F90  page   2
 
 
   56 !     See documented input file.
   57 !
   58 !  3. Parameters :
   59 !
   60 !     Local parameters.
   61 !     ----------------------------------------------------------------
   62 !       NDSI    Int.  Input unit number ("ww3_prep.inp").
   63 !       NDSLL   Int.  Unit number(s) of long-lat file(s)
   64 !       NDSF    I.A.  Unit number(s) of input file(s).
   65 !       NDSDAT  Int.  Unit number for output data file.
   66 !       IFLD    Int.  Integer input type.
   67 !       ITYPE   Int.  Integer input 'format' type.
   68 !       NFCOMP  Int.  Number of partial input to be processed.
   69 !       FLTIME  Log.  Time flag for input fields, if false, single
   70 !                     field, time read from NDSI.
   71 !       IDLALL  Int.  Layout indicator used by INA2R. +
   72 !       IDFMLL  Int.  Id. FORMAT indicator.           |
   73 !       FORMLL  C*16  Id. FORMAT.                     | Long-lat
   74 !       FROMLL  C*4   'UNIT' / 'NAME' indicator       |    file(s)
   75 !       NAMELL  C*65  Name of long-lat file(s)        +
   76 !       IDLAF   I.A.   +
   77 !       IDFMF   I.A.   |
   78 !       FORMF   C.A.   | Idem. fields file(s)
   79 !       FROMF   C*4    |
   80 !       NAMEF   C*65   +
   81 !       FORMT   C.A.  Format or time in field.
   82 !       XC      R.A.  Components of input vector field or first
   83 !                     input scalar field
   84 !       YC      R.A.  Components of input vector field or second
   85 !                     input scalar field
   86 !       FX,FY   R.A.  Output fields.
   87 !       ACC     Real  Required interpolation accuracy.
   88 !     ----------------------------------------------------------------
   89 !
   90 !  4. Subroutines used :
   91 !
   92 !      Name      Type  Module   Description
   93 !     ----------------------------------------------------------------
   94 !      W3NMOD    Subr. W3GDATMD Set number of model.
   95 !      W3SETG    Subr.   Id.    Point to selected model.
   96 !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
   97 !      W3SETW    Subr.   Id.    Point to selected model for wave data.
   98 !      W3NOUT    Subr. W3ODATMD Set number of model for output.
   99 !      W3SETO    Subr.   Id.    Point to selected model for output.
  100 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
  101 !      STRACE    Subr.   Id.    Subroutine tracing.
  102 !      NEXTLN    Subr.   Id.    Get next line from input filw
  103 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
  104 !      STME21    Subr. W3TIMEMD Convert time to string.
  105 !      INAR2R    Subr. W3ARRYMD Read in an REAL array.
  106 !      INAR2I    Subr.   Id.    Read in an INTEGER array.
  107 !      PRTBLK    Subr.   Id.    Print plot of array.
  108 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
  109 !      W3FLDO    Subr. W3FLDSMD Opening of WAVEWATCH III generic shell
  110 !                               data file.
 
 
                                                         ww3_prep.F90  page   3
 
 
  111 !      W3FLDP    Subr.   Id.    Prepare interp. from arbitrary grid.
  112 !      W3FLDG    Subr.   Id.    Reading/writing shell input data.
  113 !      W3FLDD    Subr.   Id.    Reading/writing shell assim. data.
  114 !      W3GSUC    Func. W3GSRUMD Create grid-search-utility object
  115 !      W3GSUD    Subr. W3GSRUMD Destroy grid-search-utility object
  116 !      W3GRMP    Func. W3GSRUMD Compute interpolation weights
  117 !     ----------------------------------------------------------------
  118 !
  119 !  5. Called by :
  120 !
  121 !     None, stand-alone program.
  122 !
  123 !  6. Error messages :
  124 !
  125 !     - Checks on files and reading from file.
  126 !     - Checks on validity of input parameters.
  127 !
  128 !  7. Remarks :
  129 !
  130 !     - Input fields need to be continuous in longitude and latitude.
  131 !     - Longitude - latitude grid (Section 4.a) : program attempts to
  132 !       detect closure type (ICLO) using longitudes of the grid. Thus,
  133 !       it does not allow the user to specify the closure type, and so
  134 !       tripole closure is not supported.
  135 !     - Grid(s) from file (Section 4.a) : program reads logical variable
  136 !       CLO(J) from .inp file. Thus, it does not allow the user to
  137 !       specify more than two closure type (SMPL or NONE), and so
  138 !       tripole closure is not supported.
  139                                          
  140 !  8. Structure :
  141 !
  142 !     ----------------------------------------------------
  143 !        1.a  Number of models.
  144 !                   ( W3NMOD , W3NOUT , W3SETG , W3SETO )
  145 !          b  I-O setup.
  146 !          c  Print heading(s).
  147 !        2.   Read model definition file.      ( W3IOGR )
  148 !        3.a  Read major types from input file.
  149 !          b  Check major types.
  150 !          c  Additional input format types and time.
  151 !        4.   Prepare interpolation.
  152 !          a  Longitude - latitude grid
  153 !          b  Grid(s) from file.               ( W3FLDP )
  154 !          c  Initialize fields.
  155 !          d  Input location and format.
  156 !        5    Prepare input and output files.
  157 !          a  Open input file
  158 !          b  Open and prepare output file     ( W3FLDO )
  159 !        6    Until end of file
  160 !          a  Read new time and fields
  161 !          b  Interpolate fields
  162 !          c  Write fields                     ( W3FLDG )
  163 !     ----------------------------------------------------
  164 !
  165 !  9. Switches :
 
 
                                                         ww3_prep.F90  page   4
 
 
  166 !
  167 !     !/WNT0  = !/WNT1
  168 !     !/WNT1  Correct wind speeds to (approximately) conserve the wind
  169 !             speed over the interpolation box.
  170 !     !/WNT2  Id. energy (USE ONLY ONE !)
  171 !     !/CRT1  Like !/WNT1 for currents.
  172 !     !/CRT2  Like !/WNT2 for currents.
  173 !
  174 !     !/O3    Additional output in fields processing loop.
  175 !     !/O15   Generate file with the times of the processed fields.
  176 !
  177 !     !/S     Enable subroutine tracing.
  178 !     !/T     Enable test output,
  179 !     !/T1    Full interpolation data.
  180 !     !/T1a   Echo of lat-long data in type Fn
  181 !     !/T2    Full input data.
  182 !     !/T3    Print-plot of output data.
  183 !
  184 !     !/NCO   NCEP NCO modifications for operational implementation.
  185 !
  186 ! 10. Source code :
  187 !
  188 !/ ------------------------------------------------------------------- /
  189       USE CONSTANTS
  190 !/
  191 !     USE W3GDATMD, ONLY: W3NMOD, W3SETG
  192       USE W3ADATMD,ONLY: W3NAUX, W3SETA
  193       USE W3ODATMD, ONLY: W3NOUT, W3SETO
  194       USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
  195       USE W3TIMEMD, ONLY : STME21
  196       USE W3ARRYMD, ONLY : INA2R, INA2I
  197       USE W3IOGRMD, ONLY: W3IOGR
  198       USE W3FLDSMD, ONLY: W3FLDO, W3FLDP, W3FLDG, W3FLDD
  199 !/
  200       USE W3GDATMD
  201       USE W3GSRUMD
  202       USE W3ODATMD, ONLY: NDSE, NDST, NDSO, FNMPRE
  203 !
  204       IMPLICIT NONE
  205 !/
  206 !/ ------------------------------------------------------------------- /
  207 !/ Local parameters
  208 !/
  209       INTEGER                 :: NDSI, NDSM, NDSDAT, NDSTRC, NTRACE,  &
  210                                  IERR, IFLD, ITYPE, J, IX, IY, NFCOMP,&
  211                                  TIME(2), NXI, NYI, NXJ(2), NYJ(2),   &
  212                                  NDSLL, IDLALL, IDFMLL, NDSF(2),      &
  213                                  IDLAF(2), IDFMF(2), TIME2(2),        &
  214                                  MXM, MYM, DATTYP, RECLDT, IDAT,      &
  215                                  NDAT, JJ, IS(4), JS(4)
  216       INTEGER                 :: NXT, NYT
  217       INTEGER                 :: ILAND = -999
  218       INTEGER, ALLOCATABLE    :: IX21(:,:), IX22(:,:),                &
  219                                  IY21(:,:), IY22(:,:),                &
  220                                  JX21(:,:), JX22(:,:),                &
 
 
                                                         ww3_prep.F90  page   5
 
 
  221                                  JY21(:,:), JY22(:,:), MAPOVR(:,:)
  222       INTEGER, ALLOCATABLE    :: MASK(:,:)
  223       TYPE(T_GSU)             :: GSI
  224       REAL                    :: X0I, XNI, Y0I, YNI, SXI, SYI,        &
  225                                  X, Y, FACTOR, EFAC, NODATA, RW(4)
  226       REAL                    :: ACC = 0.05
  227       REAL, ALLOCATABLE       :: RD11(:,:), RD21(:,:),                &
  228                                  RD12(:,:), RD22(:,:),                &
  229                                  XD11(:,:), XD21(:,:),                &
  230                                  XD12(:,:), XD22(:,:),                &
  231                                  FX(:,:), FY(:,:), FA(:,:),           &
  232                                  A1(:,:), A2(:,:), A3(:,:)
  233       REAL, POINTER           :: ALA(:,:), ALO(:,:)
  234       REAL, ALLOCATABLE       :: XC(:,:), YC(:,:), AC(:,:), DATA(:,:)
  235       LOGICAL                 :: INGRID
  236       LOGICAL                 :: FLSTAB, FLBERG, CLO(2), FLTIME, FLHDR
  237       INTEGER                 :: ICLO
  238       CHARACTER               :: COMSTR*1, IDFLD*3, IDTYPE*2,         &
  239                                  IDTIME*23, FROMLL*4, FORMLL*16,      &
  240                                  NAMELL*65, FROMF*4, NAMEF*65
  241       CHARACTER(LEN=12)       :: IDSTR1(-7:5)
  242       CHARACTER(LEN=15)       :: IDSTR3(3)
  243       CHARACTER(LEN=32)       :: FORMT(2), FORMF(2)
  244       CHARACTER(LEN=20)       :: IDSTR2(5)
  245       CHARACTER(LEN=13)       :: TSTR, IDSTR = 'WAVEWATCH III'
  246       CHARACTER(LEN=3)        :: TSFLD
  247       INTEGER                 :: GTYPEDUM = 0
  248 !
  249       EQUIVALENCE              ( NXI , NXJ(1) ) , ( NYI , NYJ(1) )
  250 !/
  251 !/ ------------------------------------------------------------------- /
  252 !/
  253 ! notes: Is it possible to combine ice parameters into one group,
  254 !        similar to the way 1D spectra are in one group?
  255       DATA IDSTR1 / 'ice param. 1' , 'ice param. 2' ,                 &
  256                     'ice param. 3' , 'ice param. 4' ,                 &
  257                     'ice param. 5' , 'mud density ' ,                 &
  258                     'mud thkness ' , 'mud viscos. ' ,                 &
  259                     'ice         ' , 'water levels' ,                 &
  260                     'winds       ' , 'currents    ' ,                 &
  261                     'data        ' /
  262       DATA IDSTR2 / 'pre-processed file  ' , 'long.-lat. grid     ' , &
  263                     'grid from file (1)  ' , 'grid from file (2)  ' , &
  264                     'data (assimilation) ' /
  265       DATA IDSTR3 / 'mean parameters', '1D spectra     ',             &
  266                     '2D spectra     ' /
  267       NULLIFY ( ALA, ALO )
  268 !
  269 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  270 ! 1.a  Set number of models
  271 !
  272       CALL W3NMOD ( 1, 6, 6 )
  273       CALL W3SETG ( 1, 6, 6 )
  274       CALL W3NAUX (    6, 6 )
  275       CALL W3SETA ( 1, 6, 6 )
 
 
                                                         ww3_prep.F90  page   6
 
 
  276       CALL W3NOUT (    6, 6 )
  277       CALL W3SETO ( 1, 6, 6 )
  278 !
  279 ! 1.b  IO set-up.
  280 !
  281       NDSI   = 10
  282       NDSO   =  6
  283       NDSE   =  6
  284       NDST   =  6
  285       NDSM   = 11
  286       NDSDAT = 12
  287 !
  288       NDSTRC =  6
  289       NTRACE = 10
  290       CALL ITRACE ( NDSTRC, NTRACE )
  291 !
  292 ! 1.c Print header
  293 !
  294       WRITE (NDSO,900)
  295 !
  296       J      = LEN_TRIM(FNMPRE)
  297       OPEN (NDSI,FILE=FNMPRE(:J)//'ww3_prep.inp',STATUS='OLD',        &
  298             ERR=800,IOSTAT=IERR)
  299       REWIND (NDSI)
  300       READ (NDSI,'(A)',END=801,ERR=802,IOSTAT=IERR) COMSTR
  301       IF (COMSTR.EQ.' ') COMSTR = '$'
  302       WRITE (NDSO,901) COMSTR
  303 !
  304 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  305 ! 2.  Read model definition file.
  306 !
  307       CALL W3IOGR ( 'READ', NDSM )
  308       WRITE (NDSO,902) GNAME
  309       ALLOCATE ( IX21(NX,NY), IX22(NX,NY), IY21(NX,NY), IY22(NX,NY),  &
  310                  JX21(NX,NY), JX22(NX,NY), JY21(NX,NY), JY22(NX,NY),  &
  311                  MAPOVR(NX,NY) )
  312       ALLOCATE ( RD11(NX,NY), RD21(NX,NY), RD12(NX,NY), RD22(NX,NY),  &
  313                  XD11(NX,NY), XD21(NX,NY), XD12(NX,NY), XD22(NX,NY),  &
  314                  FX(NX,NY), FY(NX,NY), FA(NX,NY),                     &
  315                  A1(NX,NY), A2(NX,NY), A3(NX,NY) )
  316 !
  317 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  318 ! 3.a Read types from input file.
  319 !
  320       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  321       READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) IDFLD, IDTYPE, FLTIME, &
  322                                                 FLHDR
  323 !
  324 ! 3.b Check types.
  325 !
  326       FLSTAB = IDFLD .EQ. 'WNS'
  327       FLBERG = IDFLD .EQ. 'ISI'
  328       IF        ( IDFLD.EQ.'IC1' ) THEN
  329           IFLD    = -7
  330         ELSE IF ( IDFLD.EQ.'IC2' ) THEN
 
 
                                                         ww3_prep.F90  page   7
 
 
  331           IFLD    = -6
  332         ELSE IF ( IDFLD.EQ.'IC3' ) THEN
  333           IFLD    = -5
  334         ELSE IF ( IDFLD.EQ.'IC4' ) THEN
  335           IFLD    = -4
  336         ELSE IF ( IDFLD.EQ.'IC5' ) THEN
  337           IFLD    = -3
  338         ELSE IF ( IDFLD.EQ.'MDN' ) THEN
  339           IFLD    = -2
  340         ELSE IF ( IDFLD.EQ.'MTH' ) THEN
  341           IFLD    = -1
  342         ELSE IF ( IDFLD.EQ.'MVS' ) THEN
  343           IFLD    = 0
  344         ELSE IF ( IDFLD.EQ.'ICE' .OR. FLBERG ) THEN
  345           IFLD    = 1
  346         ELSE IF ( IDFLD.EQ.'LEV' ) THEN
  347           IFLD    = 2
  348         ELSE IF ( IDFLD.EQ.'WND' .OR. FLSTAB ) THEN
  349           IFLD    = 3
  350         ELSE IF ( IDFLD.EQ.'CUR' ) THEN
  351           IFLD    = 4
  352         ELSE IF ( IDFLD.EQ.'DAT' ) THEN
  353           IFLD    = 5
  354         ELSE
  355           WRITE (NDSE,1030) IDFLD
  356           CALL EXTCDE ( 1 )
  357         END IF
  358 !
  359       NFCOMP = 1
  360       IF (IDFLD.EQ.'DAT') THEN
  361           ITYPE   = 5
  362         ELSE IF (IDTYPE.EQ.'AI') THEN
  363           ITYPE   = 1
  364         ELSE IF (IDTYPE.EQ.'LL') THEN
  365           ITYPE   = 2
  366         ELSE IF (IDTYPE.EQ.'F1') THEN
  367           ITYPE   = 3
  368         ELSE IF (IDTYPE.EQ.'F2') THEN
  369           ITYPE   = 4
  370           NFCOMP = 2
  371         ELSE
  372           WRITE (NDSE,1031) IDTYPE
  373           CALL EXTCDE ( 2 )
  374         END IF
  375 !
  376       WRITE (NDSO,930) IDSTR1(IFLD), IDSTR2(ITYPE)
  377       IF ( ITYPE.NE.1 ) THEN
  378           IF (IFLD.EQ.3) WRITE (NDSO,2930)
  379           IF (IFLD.EQ.4) WRITE (NDSO,1930)
  380         END IF
  381       IF ( FLBERG ) WRITE (NDSO,938)
  382       IF ( FLSTAB ) WRITE (NDSO,939)
  383       IF (ITYPE.EQ.4 .AND. IFLD.GT.2) THEN
  384           WRITE (NDSE,1032)
  385           CALL EXTCDE ( 3 )
 
 
                                                         ww3_prep.F90  page   8
 
 
  386         END IF
  387 !
  388 ! 3.c Additional input for format types and time
  389 ! ... time
  390 !
  391       IF (.NOT. FLTIME) THEN
  392           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  393           READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) TIME
  394           IF (TIME(1).LT.10000000) THEN
  395               WRITE (NDSE,1035) TIME
  396               CALL EXTCDE ( 4 )
  397             END IF
  398           CALL STME21 ( TIME , IDTIME )
  399           WRITE (NDSO,931) IDTIME
  400         END IF
  401 !
  402       J      = 1
  403       IF ( FLAGLL ) THEN
  404           FACTOR = 1.
  405         ELSE
  406           FACTOR = 1.E-3
  407         END IF
  408 !
  409 ! ... type 1
  410 !
  411       IF (ITYPE.EQ.1) THEN
  412 !
  413           NXI    = NX
  414           NYI    = NY
  415           ALLOCATE ( MASK(NXI,NYI) )
  416           MASK   = 1
  417           IF(GTYPE .EQ. UNGTYPE) THEN
  418 !
  419 !  X0, Y0 are the coordinates of the lower-left point in mesh
  420 !
  421               RW(1) = FACTOR*X0        ; RW(2) = FACTOR*MAXX
  422               RW(3) = FACTOR*Y0        ; RW(4) = FACTOR*MAXY
  423             ELSE
  424               RW(1) = FACTOR*XGRD(1,1) ; RW(2) = FACTOR*XGRD(NY,NX)
  425               RW(3) = FACTOR*YGRD(1,1) ; RW(4) = FACTOR*YGRD(NY,NX)
  426             END IF
  427           WRITE (NDSO,932) NXI, NYI
  428           IF ( FLAGLL ) THEN
  429               WRITE (NDSO,933) RW(1),RW(2),RW(3),RW(4)
  430             ELSE
  431               WRITE (NDSO,733) RW(1),RW(2),RW(3),RW(4)
  432             END IF
  433 !
  434 ! ... type 2
  435 !
  436         ELSE IF (ITYPE.EQ.2) THEN
  437 !
  438           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  439           READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR)                   &
  440                 X0I, XNI, NXI, Y0I, YNI, NYI
 
 
                                                         ww3_prep.F90  page   9
 
 
  441           IF (NXI.LT.2 .OR. NYI.LT.2) THEN
  442               WRITE (NDSE,1036) NXI, NYI
  443               CALL EXTCDE ( 5 )
  444             END IF
  445           ALLOCATE ( MASK(NXI,NYI) )
  446           MASK   = 1
  447           WRITE (NDSO,932) NXI, NYI
  448                                    
  449           IF ( FLAGLL ) THEN
  450               WRITE (NDSO,933) FACTOR*X0I, FACTOR*XNI,                &
  451                                FACTOR*Y0I, FACTOR*YNI
  452             ELSE
  453               WRITE (NDSO,733) FACTOR*X0I, FACTOR*XNI,                &
  454                                FACTOR*Y0I, FACTOR*YNI
  455             END IF
  456 !
  457 ! ... type 5
  458 !
  459         ELSE IF (ITYPE.EQ.5) THEN
  460           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  461           READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR)                   &
  462                 DATTYP, RECLDT, NODATA
  463           IF (DATTYP.LT.0 .OR. DATTYP.GT.2) THEN
  464               WRITE (NDSE,1033) DATTYP
  465               CALL EXTCDE ( 6 )
  466             END IF
  467           IF (RECLDT.LE.0) THEN
  468               WRITE (NDSE,1034) RECLDT
  469               CALL EXTCDE ( 7 )
  470             END IF
  471           WRITE (NDSO,934) IDSTR3(DATTYP+1), RECLDT, NODATA
  472           WRITE (IDFLD,935) DATTYP
  473           DEALLOCATE ( IX21, IX22, IY21, IY22, JX21, JX22, JY21, JY22, &
  474                        MAPOVR )
  475           DEALLOCATE ( RD11, RD21, RD12, RD22, XD11, XD21, XD12, XD22, &
  476                        FX, FY, FA, A1, A2, A3 )
  477 !
  478 ! ... types 3 and 4 ... in preprocessing loop ....
  479 !
  480         END IF
  481 !
  482 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  483 ! 4   Prepare interpolation.
  484 !
  485       WRITE (NDSO,940)
  486 !
  487       IF (ITYPE.NE.1 .AND. ITYPE.NE.5) THEN
  488 !
  489 ! 4.a Longitude - latitude grid
  490 !
  491           IF (ITYPE.EQ.2) THEN
  492               WRITE (NDSO,941)
  493 !
  494 ! ... setup coordinates
  495 !
 
 
                                                         ww3_prep.F90  page  10
 
 
  496               SXI    = (XNI-X0I)/REAL(NXI-1)
  497               SYI    = (YNI-Y0I)/REAL(NYI-1)
  498               ICLO   = ICLOSE_NONE
  499               IF ( FLAGLL ) THEN
  500                   IF ( ABS(ABS(REAL(NXI)*SXI)-360.) .LT. 0.1*ABS(SXI) ) &
  501                       ICLO = ICLOSE_SMPL
  502                 END IF
  503               IF ( ASSOCIATED(ALA) ) THEN
  504                   DEALLOCATE ( ALA, ALO )
  505                   NULLIFY ( ALA, ALO )
  506                 END IF
  507               ALLOCATE ( ALA(NXI,NYI), ALO(NXI,NYI) )
  508               DO IY=1, NYI
  509                 DO IX=1, NXI
  510                     ALO(IX,IY) = X0I + REAL(IX-1)*SXI
  511                     ALA(IX,IY) = Y0I + REAL(IY-1)*SYI
  512                   END DO
  513                 END DO
  514 !
  515 ! ... create grid search utility
  516 !
  517               GSI = W3GSUC( .TRUE., FLAGLL, ICLO, ALO, ALA )
  518 !
  519 ! ... construct interpolation data
  520 !
  521             IF (GTYPE .NE. UNGTYPE) THEN
  522               DO IY=1,NY
  523                 DO IX=1,NX
  524                   INGRID = W3GRMP( GSI, XGRD(IY,IX), YGRD(IY,IX),      &
  525                                    IS, JS, RW )
  526                                                
  527                   IF ( .NOT.INGRID ) THEN
  528                                          
  529 ! Notes: It would make sense to give this warning for only cases where
  530 !          the grid point is *not* masked. Obviously we don't care if
  531 !          a masked grid point is not given winds, etc.
  532                                                        
  533                      WRITE(NDSO,1042) IX, IY, XGRD(IY,IX), YGRD(IY,IX)
  534                                                                       
  535 ! Notes: We need to set these variables, even if we never intend to use t>
  536 !...........Especially in the case of IX?? IY??, we cannot leave them uns>
  537 !...........since they will be used as array indices later.
  538                                                            
  539                      IX21(IX,IY) = 1
  540                      IX22(IX,IY) = 1
  541                      IY21(IX,IY) = 1
  542                      IY22(IX,IY) = 1
  543                      RD11(IX,IY) = 0.0
  544                      RD21(IX,IY) = 0.0
  545                      RD12(IX,IY) = 0.0
  546                      RD22(IX,IY) = 0.0
  547                                       
  548                      CYCLE
  549                   END IF
  550                         
 
 
                                                         ww3_prep.F90  page  11
 
 
  551                   IX21(IX,IY) = IS(1)
  552                   IX22(IX,IY) = IS(2)
  553                   IY21(IX,IY) = JS(1)
  554                   IY22(IX,IY) = JS(4)
  555                   RD11(IX,IY) = RW(1)
  556                   RD21(IX,IY) = RW(2)
  557                   RD12(IX,IY) = RW(4)
  558                   RD22(IX,IY) = RW(3)
  559                   END DO
  560                 END DO
  561             ELSE
  562               DO IX=1, NX
  563                 X = XYB(IX,1)
  564                 Y = XYB(IX,2)
  565                              
  566                 IX21(IX,1) =   1 + INT(MOD(360.+(X-X0I),360.)/SXI)
  567 !
  568 ! Manages the simple closure of the grid
  569 !
  570                 IF (ICLO.EQ.ICLOSE_NONE) THEN
  571                   IX21(IX,1) =   MAX ( 1 , MIN(IX21(IX,1),NXI-1) )
  572                   IX22(IX,1) =   IX21(IX,1) + 1
  573                 ELSE
  574                   IX21(IX,1) =   MAX ( 1 , MIN(IX21(IX,1),NXI) )
  575                   IX22(IX,1) =   MOD(IX21(IX,1),NXI)+1
  576                   END IF
  577                 IY21(IX,1) =   1 + INT((Y-Y0I)/SYI)
  578                 IY21(IX,1) =   MAX ( 1 , MIN(IY21(IX,1),NYI-1) )
  579                 IY22(IX,1) =   IY21(IX,1) + 1
  580 !
  581                 RW(1) =  MOD(360.+(X-X0I),360.)/SXI - REAL(IX21(IX,1)-1)
  582                 RW(2) =  (Y-Y0I)/SYI - REAL(IY21(IX,1)-1)
  583 !
  584                 IF (IY21(IX,1).EQ.1 .AND. RW(2).LT.ACC) THEN
  585                   IF (RW(2).LT.-ACC) THEN
  586                     WRITE (NDSO,1044) Y
  587                   ELSE IF (RW(2).LT.0.) THEN
  588                     RW(2) = 0.
  589                   END IF
  590                 END IF
  591 !
  592               IF (IY21(IX,1).EQ.NYI .AND. RW(2).GT.1.-ACC) THEN
  593                 IF (RW(2).GT.1.+ACC) THEN
  594                   WRITE (NDSO,1044) Y
  595                 ELSE IF (RW(2).GT.1.) THEN
  596                   RW(2) = 1.
  597                   END IF
  598                 END IF
  599 !
  600                 EFAC       = SQRT ( MAX(0.,ABS(RW(1)-0.5)-0.5)**2 +  &
  601                                     MAX(0.,ABS(RW(2)-0.5)-0.5)**2 )
  602                 EFAC        = 1. / ( 1. + 0.25*EFAC**2 )
  603                                                         
  604                                                         
  605                 RD11(IX,1) = EFAC * (1.-RW(1)) * (1.-RW(2))
 
 
                                                         ww3_prep.F90  page  12
 
 
  606                 RD21(IX,1) = EFAC *    RW(1)   * (1.-RW(2))
  607                 RD12(IX,1) = EFAC * (1.-RW(1)) *    RW(2)
  608                 RD22(IX,1) = EFAC *    RW(1)   *    RW(2)
  609                 END DO
  610               END IF ! GTYPE .NE. UNGTYPE
  611 !
  612               CALL W3GSUD( GSI )
  613               DEALLOCATE ( ALA, ALO )
  614               NULLIFY ( ALA, ALO )
  615 !
  616 ! 4.b Grid(s) from file
  617 !
  618             ELSE
  619               WRITE (NDSO,942)
  620 !
  621 ! ... prepare overlay map
  622 !
  623               DO IY=1, NY
  624                 DO IX=1, NX
  625                   IF ( MAPSTA(IY,IX) .EQ. 0 ) THEN
  626                       MAPOVR(IX,IY) = ILAND
  627                     ELSE
  628                       MAPOVR(IX,IY) = 0
  629                     END IF
  630                   END DO
  631                 END DO
  632 !
  633 ! ... loop over fields
  634 !
  635               DO J=1, NFCOMP
  636 !
  637                 WRITE (NDSO,943) J
  638 !
  639 ! ... file info lat-long file
  640 !
  641                 CALL NEXTLN ( COMSTR , NDSI , NDSE )
  642                 READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR)             &
  643                       NXJ(J), NYJ(J), CLO(J)
  644                 IF (NXJ(J).LT.2 .OR. NYJ(J).LT.2) THEN
  645                     WRITE (NDSE,1036) NXJ(J), NYJ(J)
  646                     CALL EXTCDE ( 10 )
  647                   END IF
  648                 IF ( ALLOCATED(MASK) ) DEALLOCATE (MASK)
  649                 ALLOCATE ( MASK(NXJ(J),NYJ(J)) )
  650                 MASK   = 1
  651                 WRITE (NDSO,944) NXJ(J), NYJ(J), CLO(J)
  652 !
  653                 CALL NEXTLN ( COMSTR , NDSI , NDSE )
  654                 READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR)             &
  655                       FROMLL, IDLALL, IDFMLL, FORMLL
  656                 IF (IDLALL.LT.1 .OR. IDLALL.GT.4) IDLALL   = 1
  657                 IF (IDFMLL.LT.1 .OR. IDFMLL.GT.3) IDFMLL   = 1
  658                 WRITE (NDSO,945) IDLALL, IDFMLL
  659                 IF (IDFMLL.EQ.2) WRITE (NDSO,946) FORMLL
  660 !
 
 
                                                         ww3_prep.F90  page  13
 
 
  661                 CALL NEXTLN ( COMSTR , NDSI , NDSE )
  662                 READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) NDSLL, NAMELL
  663                 WRITE (NDSO,947) NDSLL
  664                 IF (FROMLL.EQ.'NAME') WRITE (NDSO,948) NAMELL
  665                 IF (NDSLL.EQ.NDSI) THEN
  666                     WRITE (NDSE,10381)
  667                     CALL NEXTLN ( COMSTR , NDSI , NDSE )
  668                   ELSE
  669 !
  670 ! ... open lat-long file
  671 !
  672                     IF ( IDFMLL .EQ. 3 ) THEN
  673                         IF (FROMLL.EQ.'NAME') THEN
  674                             JJ     = LEN_TRIM(FNMPRE)
  675                             OPEN (NDSLL,FILE=FNMPRE(:JJ)//NAMELL,     &
  676                                   FORM='UNFORMATTED',STATUS='OLD',    &
  677                                   ERR=845,IOSTAT=IERR)
  678                           ELSE
  679                             OPEN (NDSLL, FORM='UNFORMATTED',          &
  680                                   STATUS='OLD',ERR=845,IOSTAT=IERR)
  681                           END IF
  682                       ELSE
  683                         IF (FROMLL.EQ.'NAME') THEN
  684                             JJ     = LEN_TRIM(FNMPRE)
  685                             OPEN (NDSLL,FILE=FNMPRE(:JJ)//NAMELL,     &
  686                                   STATUS='OLD',ERR=845,IOSTAT=IERR)
  687                           ELSE
  688                             OPEN (NDSLL,                              &
  689                                   STATUS='OLD',ERR=845,IOSTAT=IERR)
  690                           END IF
  691                       END IF
  692 !
  693                   END IF
  694 !
  695 ! ... read lat-lon data
  696 !
  697                 IF ( ASSOCIATED(ALA) ) THEN
  698                     DEALLOCATE ( ALA, ALO )
  699                     NULLIFY ( ALA, ALO )
  700                   END IF
  701                 ALLOCATE ( ALA(NXJ(J),NYJ(J)), ALO(NXJ(J),NYJ(J)) )
  702                 CALL INA2R (ALA, NXJ(J), NYJ(J), 1, NXJ(J), 1, NYJ(J),&
  703                      NDSLL, NDST, NDSE, IDFMLL, FORMLL, IDLALL, 1., 0.)
  704                 CALL INA2R (ALO, NXJ(J), NYJ(J), 1, NXJ(J), 1, NYJ(J),&
  705                      NDSLL, NDST, NDSE, IDFMLL, FORMLL, IDLALL, 1., 0.)
  706                 IF ( NDSLL .NE. NDSI ) CLOSE (NDSLL)
  707 !
  708 ! ... file info mask file
  709 !
  710                 WRITE (NDSO,949)
  711 !
  712                 CALL NEXTLN ( COMSTR , NDSI , NDSE )
  713                 READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR)             &
  714                       FROMLL, IDLALL, IDFMLL, FORMLL
  715                 IF (IDLALL.LT.1 .OR. IDLALL.GT.4) IDLALL   = 1
 
 
                                                         ww3_prep.F90  page  14
 
 
  716                 IF (IDFMLL.LT.1 .OR. IDFMLL.GT.3) IDFMLL   = 1
  717                 WRITE (NDSO,945) IDLALL, IDFMLL
  718                 IF (IDFMLL.EQ.2) WRITE (NDSO,946) FORMLL
  719 !
  720                 CALL NEXTLN ( COMSTR , NDSI , NDSE )
  721                 READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) NDSLL, NAMELL
  722                 WRITE (NDSO,947) NDSLL
  723                 IF (FROMLL.EQ.'NAME') WRITE (NDSO,948) NAMELL
  724                 WRITE (NDSO,*) ' '
  725                 IF (NDSLL.EQ.NDSI) THEN
  726                     WRITE (NDSE,10382)
  727                     CALL NEXTLN ( COMSTR , NDSI , NDSE )
  728                   ELSE
  729 !
  730 ! ... open mask file
  731 !
  732                     IF ( IDFMLL .EQ. 3 ) THEN
  733                         IF (FROMLL.EQ.'NAME') THEN
  734                             JJ     = LEN_TRIM(FNMPRE)
  735                             OPEN (NDSLL,FILE=FNMPRE(:JJ)//NAMELL,     &
  736                                   FORM='UNFORMATTED',STATUS='OLD',    &
  737                                   ERR=846,IOSTAT=IERR)
  738                           ELSE
  739                             OPEN (NDSLL,FORM='UNFORMATTED',           &
  740                                   STATUS='OLD',ERR=846,IOSTAT=IERR)
  741                           END IF
  742                       ELSE
  743                         IF (FROMLL.EQ.'NAME') THEN
  744                             JJ     = LEN_TRIM(FNMPRE)
  745                             OPEN (NDSLL,FILE=FNMPRE(:JJ)//NAMELL,     &
  746                                   STATUS='OLD',ERR=846,IOSTAT=IERR)
  747                           ELSE
  748                             OPEN (NDSLL,                              &
  749                                   STATUS='OLD',ERR=846,IOSTAT=IERR)
  750                           END IF
  751                       END IF
  752 !
  753                   END IF
  754 !
  755 ! ... read mask data
  756 !
  757                 CALL INA2I (MASK, NXJ(J), NYJ(J), 1,NXJ(J), 1,NYJ(J), &
  758                      NDSLL, NDST, NDSE, IDFMLL, FORMLL, IDLALL, 1, 0)
  759                 IF ( NDSLL .NE. NDSI ) CLOSE (NDSLL)
  760 !
  761 ! ... generate interpolation data
  762 !
  763               IF ( J .EQ. 1 ) THEN
  764                   CALL W3FLDP ( NDSO, NDST, NDSE, IERR, FLAGLL,       &
  765                     NX, NY, NX, NY, YGRD, XGRD, MAPOVR, ILAND,        &
  766                     NXJ(J), NYJ(J), NXJ(J), NYJ(J), CLO(J), ALA, ALO, &
  767                     MASK, RD11, RD21, RD12, RD22, IX21, IX22, IY21,   &
  768                     IY22 )
  769                 ELSE
  770                   CALL W3FLDP ( NDSO, NDST, NDSE, IERR, FLAGLL,       &
 
 
                                                         ww3_prep.F90  page  15
 
 
  771                     NX, NY, NX, NY, YGRD, XGRD, MAPOVR, ILAND,        &
  772                     NXJ(J), NYJ(J), NXJ(J), NYJ(J), CLO(J), ALA, ALO, &
  773                     MASK, XD11, XD21, XD12, XD22, JX21, JX22, JY21,   &
  774                     JY22 )
  775                 END IF
  776 !
  777               END DO
  778 !
  779 ! ... average two fields !
  780 !
  781               IF ( NFCOMP .EQ. 2) THEN
  782                   DO IX=1, NX
  783                     DO IY=1, NY
  784                       IF ( MAPOVR(IX,IY) .GE. 2) THEN
  785                           FACTOR = 1. / REAL(MAPOVR(IX,IY))
  786                           RD11(IX,IY) = FACTOR * RD11(IX,IY)
  787                           RD12(IX,IY) = FACTOR * RD12(IX,IY)
  788                           RD21(IX,IY) = FACTOR * RD21(IX,IY)
  789                           RD22(IX,IY) = FACTOR * RD22(IX,IY)
  790                           XD11(IX,IY) = FACTOR * XD11(IX,IY)
  791                           XD12(IX,IY) = FACTOR * XD12(IX,IY)
  792                           XD21(IX,IY) = FACTOR * XD21(IX,IY)
  793                           XD22(IX,IY) = FACTOR * XD22(IX,IY)
  794                         END IF
  795                       END DO
  796                     END DO
  797                 END IF
  798 !
  799             END IF
  800         END IF
  801 !
  802 ! 4.c Input location and format
  803 !
  804       DO J=1, NFCOMP
  805 !
  806         IF ( ITYPE .GE. 5 ) THEN
  807             WRITE (NDSO,960)
  808           ELSE
  809             IF (ITYPE.LE.3) THEN
  810                 WRITE (NDSO,961) NXJ(J), NYJ(J)
  811               ELSE
  812                 WRITE (NDSO,962) J, NXJ(J), NYJ(J)
  813               END IF
  814           END IF
  815 !
  816         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  817         READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR)                     &
  818               FROMF, IDLAF(J), IDFMF(J), FORMT(J), FORMF(J)
  819         IF (IDLAF(J).LT.1 .OR. IDLAF(J).GT.4) IDLAF(J) = 1
  820         IF (IDFMF(J).LT.1 .OR. IDFMF(J).GT.3) IDFMF(J) = 1
  821         IF ( ITYPE .NE. 5 ) WRITE (NDSO,963) IDLAF(J)
  822         WRITE (NDSO,964) IDFMF(J)
  823         IF (IDFMF(J).EQ.2) WRITE (NDSO,965) FORMT(J), FORMF(J)
  824 !
  825         CALL NEXTLN ( COMSTR , NDSI , NDSE )
 
 
                                                         ww3_prep.F90  page  16
 
 
  826         READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) NDSF(J), NAMEF
  827         WRITE (NDSO,966) NDSF(J)
  828         IF (FROMF.EQ.'NAME') WRITE (NDSO,967) NAMEF
  829 !
  830 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  831 ! 5   Prepare files
  832 ! 5.a Open input file
  833 !
  834         WRITE (NDSO,970)
  835 !
  836         IF ( IDFMF(J) .EQ. 3 ) THEN
  837             IF (NDSF(J).EQ.NDSI) THEN
  838                 WRITE (NDSE,1051) NDSI
  839                 CALL EXTCDE ( 20 )
  840               ELSE
  841                 IF (FROMF.EQ.'NAME') THEN
  842                     JJ     = LEN_TRIM(FNMPRE)
  843                     OPEN (NDSF(J),FILE=FNMPRE(:JJ)//NAMEF,            &
  844                           FORM='UNFORMATTED',STATUS='OLD',ERR=850,    &
  845                           IOSTAT=IERR)
  846                   ELSE
  847                     OPEN (NDSF(J),FORM='UNFORMATTED',                 &
  848                           STATUS='OLD',ERR=850,IOSTAT=IERR)
  849                   END IF
  850 !
  851 ! Adding a check to see if input file is a WAVEWATCH III file
  852 ! (This check has only been added for binary wind files)
  853 !
  854                   READ (NDSF(J),END=888,IOSTAT=IERR) TSTR,            &
  855                         TSFLD, NXT, NYT
  856                   IF (IERR .EQ. 0 .AND. TSTR .EQ. IDSTR) THEN
  857                       IF (TSFLD .NE. IDFLD .OR. NXT .NE. NXI          &
  858                           .OR. NYT .NE. NYI ) THEN
  859                           WRITE (NDSE,1052) TSFLD, NXT, NYT, IDFLD,   &
  860                                  NXI, NYI
  861                           CALL EXTCDE ( 21 )
  862                       END IF
  863                   ELSE
  864                       REWIND(NDSF(J))
  865                   END IF
  866               END IF
  867           ELSE
  868             IF (NDSF(J).EQ.NDSI) THEN
  869                 CALL NEXTLN ( COMSTR , NDSI , NDSE )
  870               ELSE
  871                 IF (FROMF.EQ.'NAME') THEN
  872                     JJ     = LEN_TRIM(FNMPRE)
  873                     OPEN (NDSF(J),FILE=FNMPRE(:JJ)//NAMEF,            &
  874                           STATUS='OLD',ERR=850,IOSTAT=IERR)
  875                   ELSE
  876                     OPEN (NDSF(J),STATUS='OLD',ERR=850,IOSTAT=IERR)
  877                   END IF
  878               END IF
  879           END IF
  880 !
 
 
                                                         ww3_prep.F90  page  17
 
 
  881         END DO
  882 !
  883       IF ( NFCOMP .EQ. 1 ) THEN
  884           NXJ  (2) = NXJ  (1)
  885           NYJ  (2) = NYJ  (1)
  886           NDSF (2) = NDSF (1)
  887           IDLAF(2) = IDLAF(1)
  888           IDFMF(2) = IDFMF(1)
  889           FORMT(2) = FORMT(1)
  890           FORMF(2) = FORMF(1)
  891         END IF
  892 !
  893 ! 5.b Open and prepare output file
  894 !
  895       WRITE (NDSO,971)
  896       J      = LEN_TRIM(FNMPRE)
  897       IF ( ITYPE .LE. 4 ) THEN
  898           CALL W3FLDO ( 'WRITE', IDFLD, NDSDAT, NDST, NDSE,           &
  899                NX, NY, GTYPE, IERR, FPRE=FNMPRE(:J),                  &
  900                FHDR=FLHDR )
  901         ELSE
  902           CALL W3FLDO ( 'WRITE', IDFLD, NDSDAT, NDST, NDSE,           &
  903                RECLDT, 0, GTYPEDUM, IERR, FPRE=FNMPRE(:J) )
  904         END IF
  905 !
  906 ! 5.c Initialize fields
  907 !
  908       IF ( ITYPE .NE. 5 ) THEN
  909           FX = 0.
  910           FY = 0.
  911           FA = 0.
  912           MXM = MAX ( NXJ(1), NXJ(2) )
  913           MYM = MAX ( NYJ(1), NYJ(2) )
  914           ALLOCATE ( XC(MXM,MYM), YC(MXM,MYM), AC(MXM,MYM) )
  915           XC = 0.
  916           YC = 0.
  917           AC = 0.
  918         END IF
  919 !
  920 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  921 ! 6   Begin loop over input fields
  922 !
  923       WRITE (NDSO,972)
  924       DO
  925 !
  926 ! 6.a Read new time and fields
  927 !
  928         IF ( FLTIME ) THEN
  929 !
  930             J      = 1
  931             IF (IDFMF(J).EQ.1) THEN
  932                 READ (NDSF(J),    *   ,END=888,ERR=860,IOSTAT=IERR) TIME
  933               ELSE IF (IDFMF(J).EQ.2) THEN
  934                 READ (NDSF(J),FORMT(J),END=888,ERR=860,IOSTAT=IERR) TIME
  935               ELSE
 
 
                                                         ww3_prep.F90  page  18
 
 
  936                 READ (NDSF(J),         END=888,ERR=860,IOSTAT=IERR) TIME
  937               END IF
  938 ! <---
  939       IF (NFCOMP.EQ.2) THEN
  940           J      = 2
  941           IF (IDFMF(J).EQ.1) THEN
  942               READ (NDSF(J),    *   ,END=888,ERR=860,IOSTAT=IERR) TIME2
  943             ELSE IF (IDFMF(J).EQ.2) THEN
  944               READ (NDSF(J),FORMT(J),END=888,ERR=860,IOSTAT=IERR) TIME2
  945             ELSE
  946               READ (NDSF(J),         END=888,ERR=860,IOSTAT=IERR) TIME2
  947             END IF
  948           IF (TIME2(1).NE.TIME(1) .OR. TIME2(2).NE.TIME(2)) GOTO 861
  949         END IF
  950 ! <---
  951           END IF
  952 !
  953         CALL STME21 ( TIME , IDTIME )
  954         WRITE (NDSO,973) IDTIME
  955         WRITE (NDSO,974)
  956 !
  957 ! ... Input
  958 !
  959 !  read in array from ww3_prep.inp
  960         IF ( ITYPE .LE. 4 ) THEN
  961             CALL INA2R (XC, MXM, MYM, 1, NXJ(1), 1, NYJ(1),           &
  962                  NDSF(1), NDST, NDSE, IDFMF(1), FORMF(1), IDLAF(1), 1., 0>
  963 !
  964             IF (NFCOMP.EQ.2 .OR. IFLD.GE.3 .OR. FLBERG) THEN
  965                 CALL INA2R (YC, MXM, MYM, 1, NXJ(2), 1, NYJ(2),       &
  966                             NDSF(2), NDST, NDSE, IDFMF(2), FORMF(2),  &
  967                             IDLAF(2), 1., 0.)
  968 !
  969                 IF ( FLSTAB ) THEN
  970                     CALL INA2R (AC, MXM, MYM, 1, NXJ(2), 1, NYJ(2),   &
  971                        NDSF(2), NDST, NDSE, IDFMF(2), FORMF(2),       &
  972                        IDLAF(2), 1., 0. )
  973 !
  974                   END IF
  975 !
  976               END IF
  977 !
  978           ELSE
  979 !
  980             IF (IDFMF(1).EQ.3) THEN
  981                 READ (NDSF(1),  END=862,ERR=862,IOSTAT=IERR) NDAT
  982               ELSE
  983                 READ (NDSF(1),*,END=862,ERR=862,IOSTAT=IERR) NDAT
  984               END IF
  985             WRITE (NDSO,975) NDAT
  986             IF ( NDAT.GT.0 ) THEN
  987                 ALLOCATE ( DATA(RECLDT,NDAT) )
  988                 DO IDAT=1, NDAT
  989                   IF (IDFMF(1).EQ.1) THEN
  990                       READ (NDSF(1),    *   ,END=863,ERR=863,         &
 
 
                                                         ww3_prep.F90  page  19
 
 
  991                             IOSTAT=IERR) DATA(:,IDAT)
  992                     ELSE IF (IDFMF(1).EQ.2) THEN
  993                       READ (NDSF(1),FORMT(1),END=863,ERR=863,         &
  994                             IOSTAT=IERR) DATA(:,IDAT)
  995                     ELSE
  996                       READ (NDSF(1),         END=863,ERR=863,         &
  997                             IOSTAT=IERR) DATA(:,IDAT)
  998                     END IF
  999                   END DO
 1000               END IF
 1001 !
 1002           END IF
 1003 !
 1004 ! 6.b Interpolate fields
 1005 ! ... No interpolation, type AI (should not use array syntax !!!)
 1006 !
 1007         IF (ITYPE.EQ.1) THEN
 1008 !
 1009             IF (( IFLD.LE.2 ).AND.( .NOT. FLBERG )) THEN
 1010                 DO IY=1, NY
 1011                   DO IX=1, NX
 1012                     FA(IX,IY) = XC(IX,IY)
 1013                     END DO
 1014                   END DO
 1015               ELSE
 1016                 DO IY=1, NY
 1017                   DO IX=1, NX
 1018                     FX(IX,IY) = XC(IX,IY)
 1019                     FY(IX,IY) = YC(IX,IY)
 1020                     FA(IX,IY) = AC(IX,IY)
 1021                     END DO
 1022                   END DO
 1023               END IF
 1024 !
 1025           ELSE IF (ITYPE.NE.5) THEN
 1026 !
 1027 ! ... One-component fields
 1028 !
 1029             WRITE (NDSO,976) ' '
 1030             IF (( IFLD.LE.2 ).AND.( .NOT. FLBERG )) THEN
 1031 !
 1032                 DO IY=1,NY
 1033                   DO IX=1,NX
 1034                     FA(IX,IY)                                         &
 1035                           = RD11(IX,IY) * XC(IX21(IX,IY),IY21(IX,IY)) &
 1036                           + RD21(IX,IY) * XC(IX22(IX,IY),IY21(IX,IY)) &
 1037                           + RD12(IX,IY) * XC(IX21(IX,IY),IY22(IX,IY)) &
 1038                           + RD22(IX,IY) * XC(IX22(IX,IY),IY22(IX,IY))
 1039                     END DO
 1040                   END DO
 1041 !
 1042                 IF (NFCOMP.EQ.2) THEN
 1043                 WRITE (NDSO,976) ' (2) '
 1044                     DO IY=1,NY
 1045                       DO IX=1,NX
 
 
                                                         ww3_prep.F90  page  20
 
 
 1046                         FA(IX,IY) = FA(IX,IY)                         &
 1047                           + XD11(IX,IY) * YC(JX21(IX,IY),JY21(IX,IY)) &
 1048                           + XD21(IX,IY) * YC(JX22(IX,IY),JY21(IX,IY)) &
 1049                           + XD12(IX,IY) * YC(JX21(IX,IY),JY22(IX,IY)) &
 1050                           + XD22(IX,IY) * YC(JX22(IX,IY),JY22(IX,IY))
 1051                         END DO
 1052                       END DO
 1053                   END IF
 1054 !
 1055 ! ... Two-component fields
 1056 !
 1057               ELSE
 1058 !
 1059                 DO IY=1,NY
 1060                   DO IX=1,NX
 1061                  IF (IY21(IX,IY).LT.1) THEN
 1062                     IY21(IX,IY)=1
 1063                     IX21(IX,IY)=1
 1064                     IX22(IX,IY)=1
 1065                   ENDIF
 1066                   IF (IY22(IX,IY).LT.1) IY22(IX,IY)=1
 1067                   IF (IY21(IX,IY).GT.MYM) IY21(IX,IY)=MYM
 1068                   IF (IY22(IX,IY).GT.MYM) THEN
 1069                      IY22(IX,IY)=MYM
 1070                      IX21(IX,IY)=1
 1071                      IX22(IX,IY)=1
 1072                   END IF
 1073                     FX(IX,IY)                                         &
 1074                           = RD11(IX,IY) * XC(IX21(IX,IY),IY21(IX,IY)) &
 1075                           + RD21(IX,IY) * XC(IX22(IX,IY),IY21(IX,IY)) &
 1076                           + RD12(IX,IY) * XC(IX21(IX,IY),IY22(IX,IY)) &
 1077                           + RD22(IX,IY) * XC(IX22(IX,IY),IY22(IX,IY))
 1078                     FY(IX,IY)                                         &
 1079                           = RD11(IX,IY) * YC(IX21(IX,IY),IY21(IX,IY)) &
 1080                           + RD21(IX,IY) * YC(IX22(IX,IY),IY21(IX,IY)) &
 1081                           + RD12(IX,IY) * YC(IX21(IX,IY),IY22(IX,IY)) &
 1082                           + RD22(IX,IY) * YC(IX22(IX,IY),IY22(IX,IY))
 1083                     FA(IX,IY)                                         &
 1084                           = RD11(IX,IY) * AC(IX21(IX,IY),IY21(IX,IY)) &
 1085                           + RD21(IX,IY) * AC(IX22(IX,IY),IY21(IX,IY)) &
 1086                           + RD12(IX,IY) * AC(IX21(IX,IY),IY22(IX,IY)) &
 1087                           + RD22(IX,IY) * AC(IX22(IX,IY),IY22(IX,IY))
 1088                     A1(IX,IY) = MAX ( 1.E-10 ,                        &
 1089                                   SQRT( FX(IX,IY)**2 + FY(IX,IY)**2 ) )
 1090                     A2(IX,IY)                                         &
 1091                  = RD11(IX,IY) * SQRT(XC(IX21(IX,IY),IY21(IX,IY))**2  &
 1092                                      +YC(IX21(IX,IY),IY21(IX,IY))**2) &
 1093                  + RD21(IX,IY) * SQRT(XC(IX22(IX,IY),IY21(IX,IY))**2  &
 1094                                      +YC(IX22(IX,IY),IY21(IX,IY))**2) &
 1095                  + RD12(IX,IY) * SQRT(XC(IX21(IX,IY),IY22(IX,IY))**2  &
 1096                                      +YC(IX21(IX,IY),IY22(IX,IY))**2) &
 1097                  + RD22(IX,IY) * SQRT(XC(IX22(IX,IY),IY22(IX,IY))**2  &
 1098                                      +YC(IX22(IX,IY),IY22(IX,IY))**2)
 1099                     A3(IX,IY) = SQRT (                                &
 1100                      RD11(IX,IY) * ( XC(IX21(IX,IY),IY21(IX,IY))**2   &
 
 
                                                         ww3_prep.F90  page  21
 
 
 1101                                    + YC(IX21(IX,IY),IY21(IX,IY))**2 ) &
 1102                    + RD21(IX,IY) * ( XC(IX22(IX,IY),IY21(IX,IY))**2   &
 1103                                    + YC(IX22(IX,IY),IY21(IX,IY))**2 ) &
 1104                    + RD12(IX,IY) * ( XC(IX21(IX,IY),IY22(IX,IY))**2   &
 1105                                    + YC(IX21(IX,IY),IY22(IX,IY))**2 ) &
 1106                    + RD22(IX,IY) * ( XC(IX22(IX,IY),IY22(IX,IY))**2   &
 1107                                    + YC(IX22(IX,IY),IY22(IX,IY))**2 ) )
 1108                     END DO
 1109                   END DO
 1110 !
 1111 ! ... Winds, correct for velocity or energy conservation
 1112 !
 1113                 IF (IFLD.EQ.3) THEN
 1114                     DO IY=1,NY
 1115                       DO IX=1,NX
 1116                         FACTOR = MIN ( 1.5 , A3(IX,IY)/A1(IX,IY) )
 1117                         FX(IX,IY) = FACTOR * FX(IX,IY)
 1118                         FY(IX,IY) = FACTOR * FY(IX,IY)
 1119                         END DO
 1120                       END DO
 1121                   END IF
 1122 !
 1123 ! ... Currents, correct for velocity or energy conservation
 1124 !
 1125                 IF (IFLD.EQ.4) THEN
 1126                     DO IY=1,NY
 1127                       DO IX=1,NX
 1128                         FACTOR = MIN ( 1.5 , A2(IX,IY)/A1(IX,IY) )
 1129                         FX(IX,IY) = FACTOR * FX(IX,IY)
 1130                         FY(IX,IY) = FACTOR * FY(IX,IY)
 1131                         END DO
 1132                       END DO
 1133                   END IF
 1134 !
 1135               END IF
 1136 !
 1137           END IF
 1138 !
 1139 ! ... Test output
 1140 !
 1141 ! 6.c Write fields
 1142 !
 1143         IF ( ITYPE .LE. 4 ) THEN
 1144             WRITE (NDSO,977)
 1145             CALL W3FLDG ('WRITE', IDFLD, NDSDAT, NDST, NDSE, NX, NY,  &
 1146                          NX, NY, TIME, TIME, TIME, FX, FY, FA, TIME,  &
 1147                          FX, FY, FA, IERR)
 1148           ELSE IF ( ITYPE .EQ. 5 ) THEN
 1149             IF ( NDAT .EQ. 0 ) THEN
 1150                 WRITE (NDSO,978)
 1151               ELSE
 1152                 WRITE (NDSO,977)
 1153                 CALL W3FLDD ('WRITE', IDFLD, NDSDAT, NDST, NDSE, TIME,&
 1154                              TIME, RECLDT, NDAT, IDAT, DATA, IERR )
 1155                 DEALLOCATE ( DATA )
 
 
                                                         ww3_prep.F90  page  22
 
 
 1156               END IF
 1157           END IF
 1158         IF (IERR.NE.0) CALL EXTCDE ( 30 )
 1159 !
 1160         IF ( .NOT. FLTIME ) EXIT
 1161         END DO
 1162 !
 1163 !     End loop over input fields
 1164 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 1165 !
 1166       GOTO 888
 1167 !
 1168 ! Error escape locations
 1169 !
 1170   800 CONTINUE
 1171       WRITE (NDSE,1000) IERR
 1172       CALL EXTCDE ( 40 )
 1173 !
 1174   801 CONTINUE
 1175       WRITE (NDSE,1001)
 1176       CALL EXTCDE ( 41 )
 1177 !
 1178   802 CONTINUE
 1179       WRITE (NDSE,1002) IERR
 1180       CALL EXTCDE ( 42 )
 1181 !
 1182   845 CONTINUE
 1183       WRITE (NDSE,1045) IERR
 1184       CALL EXTCDE ( 47 )
 1185 !
 1186   846 CONTINUE
 1187       WRITE (NDSE,1046) IERR
 1188       CALL EXTCDE ( 48 )
 1189 !
 1190   850 CONTINUE
 1191       WRITE (NDSE,1050) IERR, NDSF(J), NAMEF
 1192       CALL EXTCDE ( 49 )
 1193 !
 1194   860 CONTINUE
 1195       WRITE (NDSE,1060) J, IERR
 1196       CALL EXTCDE ( 50 )
 1197 !
 1198   861 CONTINUE
 1199       WRITE (NDSE,1061) TIME, TIME2
 1200       CALL EXTCDE ( 51 )
 1201 !
 1202   862 CONTINUE
 1203       WRITE (NDSE,1062) IERR
 1204       CALL EXTCDE ( 52 )
 1205 !
 1206   863 CONTINUE
 1207       WRITE (NDSE,1063) IDAT, IERR
 1208       CALL EXTCDE ( 53 )
 1209 !
 1210   888 CONTINUE
 
 
                                                         ww3_prep.F90  page  23
 
 
 1211       WRITE (NDSO,999)
 1212 !
 1213 ! Formats
 1214 !
 1215   900 FORMAT (/15X,'  *** WAVEWATCH III  Input pre-processing ***  '/ &
 1216                15X,'==============================================='/)
 1217   901 FORMAT ( '  Comment character is ''',A,''''/)
 1218   902 FORMAT ( '  Grid name : ',A/)
 1219 !
 1220   930 FORMAT (/'  Description of inputs'/                             &
 1221                ' --------------------------------------------------'/ &
 1222                '       Input type        : ',A/                       &
 1223                '       Format type       : ',A)
 1224  1930 FORMAT ( '          Field conserves velocity.')
 1225  2930 FORMAT ( '          Field corrected for energy conservation.')
 1226   931 FORMAT (/'       Single field, time: ',A)
 1227   932 FORMAT (/'       Input grid dim.   :',I5,3X,I5)
 1228   933 FORMAT ( '       Longitude range   :',2F8.2,' (deg)'/           &
 1229                '       Latitude range    :',2F8.2,' (deg)')
 1230   733 FORMAT ( '       X range           :',2F8.2,' (km)'/           &
 1231                '       Y range           :',2F8.2,' (km)')
 1232   934 FORMAT (/'       Data type         : ',A/                       &
 1233                '       Data record length:',I5/                       &
 1234                '       Missing values    :',F8.2)
 1235   935 FORMAT ( 'DT',I1 )
 1236   938 FORMAT ( '          Icebergs included.')
 1237   939 FORMAT ( '          Air-sea temperature differences included.')
 1238 !
 1239   940 FORMAT (//'  Preprocessing data'/                               &
 1240                ' --------------------------------------------------')
 1241   941 FORMAT ( '       Interpolation factors ..... '/                 &
 1242                '          (longitude-latitude grid)')
 1243   942 FORMAT ( '       Interpolation factors ..... '/                 &
 1244                '          (grid from file)')
 1245   943 FORMAT (/'           Longitude-latitude file ',I1,' :'/         &
 1246                '          ---------------------------------------')
 1247   944 FORMAT ( '           Input grid dim.   :',I5,3X,I5/             &
 1248                '           Closed longitudes :',L5)
 1249   945 FORMAT ( '           Layout indicator  :',I5/                   &
 1250                '           Format indicator  :',I5)
 1251   946 FORMAT ( '           Format            : ',A)
 1252   947 FORMAT ( '           Unit number       :',I5)
 1253   948 FORMAT ( '           File name         : ',A)
 1254   949 FORMAT (/'           Corresponding map file '/                  &
 1255                '          ---------------------------------------')
 1256 !
 1257   960 FORMAT (/'           Data file :'/                              &
 1258                '          ---------------------------------------')
 1259   961 FORMAT (/'           Data file :'/                              &
 1260                '          ---------------------------------------'/   &
 1261                '           Input grid dim.   :',I5,3X,I5)
 1262   962 FORMAT (/'           Data file (',I1,') :'/                     &
 1263                '          ---------------------------------------'/   &
 1264                '           Input grid dim.   :',I5,3X,I5)
 1265   963 FORMAT ( '           Layout indicator  :',I5)
 
 
                                                         ww3_prep.F90  page  24
 
 
 1266   964 FORMAT ( '           Format indicator  :',I5)
 1267   965 FORMAT ( '           Format for time   : ',A/                   &
 1268                '           Format for data   : ',A)
 1269   966 FORMAT ( '           Unit number       :',I5)
 1270   967 FORMAT ( '           File name         : ',A)
 1271 !
 1272   970 FORMAT (/'       Opening input data file .....')
 1273   971 FORMAT (/'       Opening output data file .....')
 1274   972 FORMAT (//'  Processing data'/                                  &
 1275                ' --------------------------------------------------')
 1276   973 FORMAT ( '           Time : ',A)
 1277   974 FORMAT ( '                  reading ....')
 1278   975 FORMAT ( '                     number of data records :',I6)
 1279   976 FORMAT ( '                  interpolating',A,'....')
 1280   977 FORMAT ( '                  writing ....')
 1281   978 FORMAT ( '                  skipping ....')
 1282 !
 1283   999 FORMAT(//'  End of program '/                                   &
 1284                ' ========================================='/          &
 1285                '         WAVEWATCH III Input preprocessing '/)
 1286 !
 1287  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1288                '     ERROR IN OPENING INPUT FILE'/                    &
 1289                '     IOSTAT =',I5/)
 1290 !
 1291  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1292                '     PREMATURE END OF INPUT FILE'/)
 1293 !
 1294  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1295                '     ERROR IN READING FROM INPUT FILE'/               &
 1296                '     IOSTAT =',I5/)
 1297 !
 1298  1030 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1299                '     ILLEGAL FIELD ID -->',A,'<--'/)
 1300  1031 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1301                '     ILLEGAL FORMAT ID -->',A,'<--'/)
 1302  1032 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1303                '     THIS FORMAT TYPE IS ALLOWED FOR ICE AND LEV ONLY'/)
 1304 !
 1305  1033 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1306                '     ILLEGAL DATA RECORD LENGTH : ',I6/)
 1307  1034 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1308                '     ILLEGAL DATA TYPE : ',I2/)
 1309 !
 1310  1035 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1311                '     ILLEGAL TIME : ',I8.8,I7.6/)
 1312  1036 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1313                '     ILLEGAL SIZE OF INPUT GRID : ',I5,1X,I5/)
 1314  10381 FORMAT (/' *** WAVEWATCH III WARNING IN W3PREP : '/             &
 1315                '     LAT/LON DATA READ FROM INPUT FILE')
 1316  10382 FORMAT (/' *** WAVEWATCH III WARNING IN W3PREP : '/             &
 1317                '     MASK DATA READ FROM INPUT FILE')
 1318 !
 1319  1042 FORMAT (/' *** WAVEWATCH-III WARNING W3PREP : '/                &
 1320                '     GRID POINT ',2I6,2F7.2,/                         &
 
 
                                                         ww3_prep.F90  page  25
 
 
 1321                ' NOT COVERED BY INPUT GRID.'/)
 1322  1044 FORMAT (/' *** WAVEWATCH III WARNING W3PREP : '/                &
 1323                '     Y = ',F10.1,' NOT COVERED BY INPUT GRID.'/)
 1324 !
 1325  
 1326 !
 1327  1045 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1328                '     ERROR IN OPENING LAT-LONG DATA FILE'/            &
 1329                '     IOSTAT =',I5/)
 1330 !
 1331  1046 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1332                '     ERROR IN OPENING MASK FILE'/                     &
 1333                '     IOSTAT =',I5/)
 1334 !
 1335  1050 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1336                '     ERROR IN OPENING INPUT DATA FILE'/               &
 1337                '     IOSTAT =',I5/                                    &
 1338                '     NDSF   =',I5/                                    &
 1339                '     NAMEF  = ',A/)
 1340  1051 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1341                '     CANNOT READ UNFORMATTED FROM UNIT',I3/)
 1342 !
 1343  1052 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1344                '     ERROR IN READING FROM INPUT DATA FILE'/          &
 1345                '     IN FILE ,    VARIABLE ID     = ',A/              &
 1346                '                  ARRAY DIMENSION = ',2I5/            &
 1347                '     EXPECTING ,  VARIABLE ID     = ',A/              &
 1348                '                  ARRAY DIMENSION = ',2I5/)
 1349 !
 1350  1060 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1351                '     ERROR IN READING TIME FROM FILE (',I1,')'/       &
 1352                '     IOSTAT =',I5/)
 1353  1061 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1354                '     INCOMPATIBLE FIELD TIMES '/                      &
 1355                '            FIELD #1 : ',I8.8,I7.6/                   &
 1356                '            FIELD #2 : ',I8.8,I7.6/)
 1357  1062 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1358                '     ERROR IN READING NDAT FROM FILE'/                &
 1359                '     IOSTAT =',I5/)
 1360  1063 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1361                '     ERROR IN READING DATA RECORD',I6,' FROM FILE'/   &
 1362                '     IOSTAT =',I5/)
 1363 !
 1364 !/
 1365 !/ End of W3PREP ----------------------------------------------------- /
 1366 !/
 1367       END PROGRAM W3PREP
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
