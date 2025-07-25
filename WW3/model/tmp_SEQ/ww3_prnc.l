                                                         ww3_prnc.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3PRNC
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           M. Accensi              |
    8 !/                  |           F. Ardhuin              |
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :         21-Apr-2020 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    01-Jan-2011 : Creation                            ( version 4.01 )
   14 !/    17-Nov-2011 : Fix bug on latitudes                ( version 4.04 )
   15 !/    30-Sep-2012 : Implement tidal analysis            ( version 4.08 )
   16 !/    29-Oct-2012 : Parallelization of tidal analysis   ( version 4.08 )
   17 !/     4-Mar-2012 : allows any NetCDF dimensions names  ( version 4.09 )
   18 !/    13-Mar-2012 : Makes compatible with NC3           ( version 4.10 )
   19 !/    18-Oct-2013 : Debug compile issue with TIDE switch( version 4.12 )
   20 !/    18-Oct-2013 : Initialize interpolation weights    ( version 4.12 )
   21 !/    20-Dec-2013 : Allow scale factor and offset in    ( version 4.16 )
   22 !/                  NetCDF variables (S. Zieger)
   23 !/    24-Oct-2014 : Allows "As Is" curvilinear grids    ( version 5.02 )
   24 !/    14-Oct-2015 : Add a check for latitude reversed   ( version 5.11 )
   25 !/    20-Jan-2017 : Update to new W3GSRUMD APIs         ( version 6.02 )
   26 !/    04-Jan-2018 : Add namelist feature                ( version 6.04 )
   27 !/    21-Apr-2020 : Correction in MPI for tide          ( version 7.XX )
   28 !/    21-Apr-2020 : Correction in scale factor          ( version 7.XX )
   29 !/
   30 !/    Copyright 2009 National Weather Service (NWS),
   31 !/       National Oceanic and Atmospheric Administration.  All rights
   32 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   33 !/       No unauthorized use without permission.
   34 !/
   35 !  1. Purpose :
   36 !
   37 !     Pre-processing of the input water level, current, wind and ice
   38 !     fields as well as assimilation data ... from NetCDF input
   39 !
   40 !  2. Method :
   41 !
   42 !     See documented input file.
   43 !
   44 !  3. Parameters :
   45 !
   46 !     Local parameters.
   47 !     ----------------------------------------------------------------
   48 !       NDSI    Int.  Input unit number ("ww3_prnc.inp").
   49 !       NDSLL   Int.  Unit number(s) of long-lat file(s)
   50 !       NDSF    I.A.  Unit number(s) of input file(s).
   51 !       NDSDAT  Int.  Unit number for output data file.
   52 !       IFLD    Int.  Integer input type.
   53 !       ITYPE   Int.  Integer input 'format' type.
   54 !       NFCOMP  Int.  Number of partial input to be processed.
   55 !       FLTIME  Log.  Time flag for input fields, if false, single
 
 
                                                         ww3_prnc.F90  page   2
 
 
   56 !                     field, time read from NDSI.
   57 !       IDLALL  Int.  Layout indicator used by INA2R. +
   58 !       IDFMLL  Int.  Id. FORMAT indicator.           |
   59 !       FORMLL  C*16  Id. FORMAT.                     | Long-lat
   60 !       FROMLL  C*4   'UNIT' / 'NAME' indicator       |    file(s)
   61 !       NAMELL  C*20  Name of long-lat file(s)        +
   62 !       IDLAF   I.A.   +
   63 !       IDFMF   I.A.   |
   64 !       FORMF   C.A.   | Idem. fields file(s)
   65 !       NAMEF   C*20   +
   66 !       FORMT   C.A.  Format or time in field.
   67 !       XC      R.A.  Components of input vector field or first
   68 !                     input scalar field
   69 !       XCFAC   Real  Scale factor for input scalar field
   70 !       XCOFF   Real  Offset for input scalar field
   71 !       YC      R.A.  Components of input vector field or second
   72 !                     input scalar field
   73 !       YCFAC   Real  Scale factor for input scalar field
   74 !       YCOFF   Real  Offset for input scalar field
   75 !       FX,FY   R.A.  Output fields.
   76 !       ACC     Real  Required interpolation accuracy.
   77 !     ----------------------------------------------------------------
   78 !
   79 !  4. Subroutines used :
   80 !
   81 !      Name      Type  Module   Description
   82 !     ----------------------------------------------------------------
   83 !      W3NMOD    Subr. W3GDATMD Set number of model.
   84 !      W3SETG    Subr.   Id.    Point to selected model.
   85 !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
   86 !      W3SETW    Subr.   Id.    Point to selected model for wave data.
   87 !      W3NOUT    Subr. W3ODATMD Set number of model for output.
   88 !      W3SETO    Subr.   Id.    Point to selected model for output.
   89 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
   90 !      STRACE    Subr.   Id.    Subroutine tracing.
   91 !      NEXTLN    Subr.   Id.    Get next line from input filw
   92 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
   93 !      STME21    Subr. W3TIMEMD Convert time to string.
   94 !      INAR2R    Subr. W3ARRYMD Read in an REAL array.
   95 !      INAR2I    Subr.   Id.    Read in an INTEGER array.
   96 !      PRTBLK    Subr.   Id.    Print plot of array.
   97 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
   98 !      W3FLDO    Subr. W3FLDSMD Opening of WAVEWATCH III generic shell
   99 !                               data file.
  100 !      W3FLDP    Subr.   Id.    Prepare interp. from arbitrary grid.
  101 !      W3FLDG    Subr.   Id.    Reading/writing shell input data.
  102 !      W3FLDD    Subr.   Id.    Reading/writing shell assim. data.
  103 !      W3GSUC    Func. W3GSRUMD Create grid-search-utility object
  104 !      W3GSUD    Subr. W3GSRUMD Destroy grid-search-utility object
  105 !      W3GRMP    Func. W3GSRUMD Compute interpolation weights
  106 !     ----------------------------------------------------------------
  107 !
  108 !  5. Called by :
  109 !
  110 !     None, stand-alone program.
 
 
                                                         ww3_prnc.F90  page   3
 
 
  111 !
  112 !  6. Error messages :
  113 !
  114 !     - Checks on files and reading from file.
  115 !     - Checks on validity of input parameters.
  116 !
  117 !  7. Remarks :
  118 !
  119 !     - Input fields need to be continuous in longitude and latitude.
  120 !     - Program attempts to detect closure type using longitudes of the
  121 !       grid. Thus, it does not allow the user to specify the closure
  122 !       type, and so tripole closure is not supported.
  123 !
  124 !  8. Structure :
  125 !
  126 !     ----------------------------------------------------
  127 !        1.a  Number of models.
  128 !                   ( W3NMOD , W3NOUT , W3SETG , W3SETO )
  129 !          b  I-O setup.
  130 !          c  Print heading(s).
  131 !        2.   Read model definition file.      ( W3IOGR )
  132 !        3.a  Read major types from input file.
  133 !          b  Check major types.
  134 !          c  Additional input format types and time.
  135 !        4.   Prepare interpolation.
  136 !          a  Longitude - latitude grid
  137 !          b  Grid(s) from file.               ( W3FLDP )
  138 !          c  Initialize fields.
  139 !          d  Input location and format.
  140 !        5    Prepare input and output files.
  141 !          a  Open input file
  142 !          b  Open and prepare output file     ( W3FLDO )
  143 !        6    Until end of file
  144 !          a  Read new time and fields
  145 !          b  Interpolate fields
  146 !          c  Write fields                     ( W3FLDG )
  147 !     ----------------------------------------------------
  148 !
  149 !  9. Switches :
  150 !
  151 !     !/WNT0  = !/WNT1
  152 !     !/WNT1  Correct wind speeds to (approximately) conserve the wind
  153 !             speed over the interpolation box.
  154 !     !/WNT2  Id. energy (USE ONLY ONE !)
  155 !     !/CRT1  Like !/WNT1 for currents.
  156 !     !/CRT2  Like !/WNT2 for currents.
  157 !     !/MPI   Parallel processing is used for tidal analysis.
  158 !
  159 !     !/O3    Additional output in fields processing loop.
  160 !     !/O15   Generate file with the times of the processed fields.
  161 !
  162 !     !/S     Enable subroutine tracing.
  163 !     !/T     Enable test output,
  164 !     !/T1    Full interpolation data.
  165 !     !/T1a   Echo of lat-long data in type Fn
 
 
                                                         ww3_prnc.F90  page   4
 
 
  166 !     !/T2    Full input data.
  167 !     !/T3    Print-plot of output data.
  168 !
  169 !     !/NCO   NCEP NCO modifications for operational implementation.
  170 !
  171 ! 10. Source code :
  172 !
  173 !/ ------------------------------------------------------------------- /
  174       USE CONSTANTS
  175 !/
  176 !     USE W3GDATMD, ONLY: W3NMOD, W3SETG
  177       USE W3ADATMD,ONLY: W3NAUX, W3SETA
  178       USE W3ODATMD, ONLY: W3NOUT, W3SETO
  179       USE W3ODATMD, ONLY: IAPROC, NAPROC, NAPERR, NAPOUT
  180       USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE, STRSPLIT
  181       USE W3ARRYMD, ONLY : INA2R, INA2I
  182       USE W3IOGRMD, ONLY: W3IOGR
  183       USE W3FLDSMD, ONLY: W3FLDO, W3FLDP, W3FLDG, W3FLDD,  &
  184                           W3FLDTIDE1, W3FLDTIDE2
  185 !/
  186       USE W3GDATMD
  187       USE W3GSRUMD
  188       USE W3ODATMD, ONLY: NDSE, NDST, NDSO, FNMPRE
  189                                                   
  190       USE W3TIDEMD
  191       USE W3TIMEMD
  192       USE W3NMLPRNCMD
  193       USE NETCDF
  194 !
  195       IMPLICIT NONE
  196 !
  197 !/
  198 !/ ------------------------------------------------------------------- /
  199 !/ Local parameters
  200 !/
  201       TYPE(NML_FORCING_T)     :: NML_FORCING
  202       TYPE(NML_FILE_T)        :: NML_FILE
  203       TYPE(T_GSU)             :: GSI
  204 !
  205       INTEGER                 :: NTI, NDSEN, NIDIMS, NFIELDS, ICLO,   &
  206                                  NDSI, NDSM, NDSDAT, NDSTRC, NTRACE,  &
  207                                  IERR, IFLD, ITYPE, J, NFCOMP,        &
  208                                  IX, IY, JX, NXI, NYI, NDAT, JJ,      &
  209                                  NDSLL, IDLALL, IDFMLL, NCID, IRET,   &
  210                                  MXM, MYM, DATTYP, RECLDT, IDAT,      &
  211                                  NDIMSGRID, NDIMSVAR, VARIDTMP,       &
  212                                  NUMDIMS, I, ITIME
  213       INTEGER                 :: ILAND = -999
  214       INTEGER                 :: GTYPEDUM = 0
  215                                              
  216 !
  217       INTEGER                 :: TIME(2), TIMESTART(2), TIMESTOP(2),  &
  218                                  TIMESHIFT(2), NXJ(2), NYJ(2),        &
  219                                  NDSF(2), IDLAF(2), IDFMF(2),         &
  220                                  IS(4), JS(4), VARIDF(50), DIMSVAR(4),&
 
 
                                                         ww3_prnc.F90  page   5
 
 
  221                                  DIMLN(5), REFDATE(8),CURDATE(8),     &
  222                                  STARTDATE(8),STPDATE(8)
  223 !
  224       INTEGER, ALLOCATABLE    :: IX21(:,:), IX22(:,:),                &
  225                                  IY21(:,:), IY22(:,:),                &
  226                                  JX21(:,:), JX22(:,:),                &
  227                                  JY21(:,:), JY22(:,:),                &
  228                                  MAPOVR(:,:), MASK(:,:),              &
  229                                  NELEM(:), CUMUL(:)
  230 !
  231       REAL                    :: X0I, XNI, Y0I, YNI, SXI, SYI,        &
  232                                  X, Y, FACTOR, EFAC, NODATA,          &
  233                                  XCFAC, XCOFF, YCFAC, YCOFF,          &
  234                                  FILLVALUE, TIMEDELAY
  235       REAL                    :: ACC = 0.05
  236 !
  237       REAL                    :: SCFAC(2), ADDOFF(2), RW(4)
  238 !
  239       REAL, ALLOCATABLE       :: RD11(:,:), RD21(:,:),                &
  240                                  RD12(:,:), RD22(:,:),                &
  241                                  XD11(:,:), XD21(:,:),                &
  242                                  XD12(:,:), XD22(:,:),                &
  243                                  FX(:,:), FY(:,:), FA(:,:),           &
  244                                  A1(:,:), A2(:,:), A3(:,:)
  245       REAL, ALLOCATABLE       :: XC(:,:), YC(:,:), AC(:,:), DATA(:,:)
  246 !
  247       REAL, POINTER           :: ALA(:,:), ALO(:,:)
  248 !
  249       DOUBLE PRECISION        :: REFJULDAY, CURJULDAY, STARTJULDAY, STPJU>
  250 !
  251       CHARACTER*1024          :: STRFIELDSNAME
  252       CHARACTER*100           :: FIELDSNAME(4)
  253       CHARACTER*1024          :: STRDIMSNAME
  254       CHARACTER*100           :: DIMSNAME(2)
  255       CHARACTER               :: COMSTR*1, IDFLD*3, IDTYPE*2,         &
  256                                  IDTIME*23, FROMLL*4, FORMLL*16,      &
  257                                  NAMELL*80, NAMEF*80, IDTIME2*23
  258       CHARACTER*14            :: IDSTR1(-7:5)
  259       CHARACTER*15            :: IDSTR3(3)
  260       CHARACTER*32            :: FORMT(2), FORMF(2)
  261       CHARACTER*20            :: IDSTR2(6)
  262       CHARACTER*20            :: DIMNAME(5)
  263       CHARACTER*50            :: TIMEUNITS, CALENDAR
  264 !
  265       LOGICAL                 :: INGRID, FLGNML
  266       LOGICAL                 :: FLSTAB, FLBERG, CLO(2), FLTIME, FLHDR
  267                                                                       
  268                                                                       
  269                                                                       
  270 !
  271 ! Variables used in tidal analysis
  272 !
  273       INTEGER                 :: K, L, TIDEFLAG,                      &
  274                                  TIDE_NDEF, TIDE_ITREND
  275 !
 
 
                                                         ww3_prnc.F90  page   6
 
 
  276       INTEGER, ALLOCATABLE          :: IMAX(:)
  277 !
  278       REAL                          :: TIDE_LAT
  279 !
  280       REAL, ALLOCATABLE       :: TIDE_DATA_ALL(:,:,:),                &
  281                                  SSQ(:), RES(:)
  282 !
  283       DOUBLE PRECISION, ALLOCATABLE :: ALLTIMES(:),                   &
  284                                        SDEV0(:), SDEV(:), RMSR(:),    &
  285                                        RMSR0(:), RMSRP(:), RESMAX(:)
  286 !
  287       CHARACTER*256           :: TIDECONSTNAMES
  288       CHARACTER*100           :: LIST(70)
  289 !
  290       EQUIVALENCE              ( NXI , NXJ(1) ) , ( NYI , NYJ(1) )
  291 !/
  292 !/ ------------------------------------------------------------------- /
  293 !/
  294       DATA IDSTR1 / 'ice thickness ' , 'ice viscosity' ,              &
  295                     'ice density   ' , 'ice modulus  ' ,              &
  296                     'ice flow diam.' , 'mud density  ' ,              &
  297                     'mud thickness ', 'mud viscosity ' ,              &
  298                     'ice conc.     ' , 'water levels ' ,              &
  299                     'winds         ' , 'currents     ' ,              &
  300                     'data          ' /
  301       DATA IDSTR2 / 'pre-processed file  ' , 'long.-lat. grid     ' , &
  302                     'grid from file (1)  ' , 'grid from file (2)  ' , &
  303                     'data (assimilation) ' , 'pre-pro. file + tide'  /
  304       DATA IDSTR3 / 'mean parameters', '1D spectra     ',             &
  305                     '2D spectra     ' /
  306 !
  307 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  308 ! 1.a  Set number of models
  309 !
  310       CALL W3NMOD ( 1, 6, 6 )
  311       CALL W3SETG ( 1, 6, 6 )
  312       CALL W3NAUX (    6, 6 )
  313       CALL W3SETA ( 1, 6, 6 )
  314       CALL W3NOUT (    6, 6 )
  315       CALL W3SETO ( 1, 6, 6 )
  316 !
  317 ! 1.b  IO set-up.
  318 !
  319       NDSI   = 10
  320       NDSO   =  6
  321       NDSE   =  6
  322       NDST   =  6
  323       NDSM   = 11
  324       NDSDAT = 12
  325 !
  326       NDSTRC =  6
  327       NTRACE = 10
  328       CALL ITRACE ( NDSTRC, NTRACE )
  329 !
  330 ! 1.c MPP initializations
 
 
                                                         ww3_prnc.F90  page   7
 
 
  331 !
  332       NAPROC = 1
  333       IAPROC = 1
  334 !
  335       IF ( IAPROC .EQ. NAPERR ) THEN
  336           NDSEN  = NDSE
  337         ELSE
  338           NDSEN  = -1
  339         END IF
  340 !
  341       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,900)
  342 !
  343  
  344 !
  345 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  346 ! 2.  Read model definition file.
  347 !
  348       CALL W3IOGR ( 'READ', NDSM )
  349       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,902) GNAME
  350       ALLOCATE ( IX21(NX,NY), IX22(NX,NY), IY21(NX,NY), IY22(NX,NY),  &
  351                  JX21(NX,NY), JX22(NX,NY), JY21(NX,NY), JY22(NX,NY),  &
  352                  MAPOVR(NX,NY) )
  353       ALLOCATE ( RD11(NX,NY), RD21(NX,NY), RD12(NX,NY), RD22(NX,NY),  &
  354                  XD11(NX,NY), XD21(NX,NY), XD12(NX,NY), XD22(NX,NY),  &
  355                  FX(NX,NY), FY(NX,NY), FA(NX,NY),                     &
  356                  A1(NX,NY), A2(NX,NY), A3(NX,NY) )
  357 !
  358 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  359 ! 3. Read types and variables from input file.
  360 !
  361  
  362       FLBERG = .FALSE.
  363       FLSTAB = .FALSE.
  364 !
  365 ! process ww3_prnc namelist
  366 !
  367       INQUIRE(FILE=TRIM(FNMPRE)//"ww3_prnc.nml", EXIST=FLGNML)
  368       IF (FLGNML) THEN
  369          ! Read namelist
  370          CALL W3NMLPRNC (NDSI, TRIM(FNMPRE)//'ww3_prnc.nml', NML_FORCING,>
  371          ! Check field
  372          IF      (NML_FORCING%FIELD%ICE_PARAM1) THEN
  373            IDFLD = 'IC1'
  374            IFLD = -7
  375            NFIELDS = 1
  376          ELSE IF (NML_FORCING%FIELD%ICE_PARAM2) THEN
  377            IDFLD = 'IC2'
  378            IFLD = -6
  379            NFIELDS = 1
  380          ELSE IF (NML_FORCING%FIELD%ICE_PARAM3) THEN
  381            IDFLD = 'IC3'
  382            IFLD = -5
  383            NFIELDS = 1
  384          ELSE IF (NML_FORCING%FIELD%ICE_PARAM4) THEN
  385            IDFLD = 'IC4'
 
 
                                                         ww3_prnc.F90  page   8
 
 
  386            IFLD = -4
  387            NFIELDS = 1
  388          ELSE IF (NML_FORCING%FIELD%ICE_PARAM5) THEN
  389            IDFLD = 'IC5'
  390            IFLD = -3
  391            NFIELDS = 1
  392          ELSE IF (NML_FORCING%FIELD%MUD_DENSITY) THEN
  393            IDFLD = 'MDN'
  394            IFLD = -2
  395            NFIELDS = 1
  396          ELSE IF (NML_FORCING%FIELD%MUD_THICKNESS) THEN
  397            IDFLD = 'MTH'
  398            IFLD = -1
  399            NFIELDS = 1
  400          ELSE IF (NML_FORCING%FIELD%MUD_VISCOSITY) THEN
  401            IDFLD = 'MVS'
  402            IFLD = 0
  403            NFIELDS = 1
  404          ELSE IF (NML_FORCING%FIELD%ICE_CONC) THEN
  405            IDFLD = 'ICE'
  406            IFLD = 1
  407            NFIELDS = 1
  408          ELSE IF (NML_FORCING%FIELD%ICE_BERG) THEN
  409            IDFLD = 'ISI'
  410            IFLD = 1
  411            FLBERG = .TRUE.
  412            NFIELDS = 2
  413          ELSE IF (NML_FORCING%FIELD%WATER_LEVELS) THEN
  414            IDFLD = 'LEV'
  415            IFLD = 2
  416            NFIELDS = 1
  417          ELSE IF (NML_FORCING%FIELD%WINDS) THEN
  418            IDFLD = 'WND'
  419            IFLD = 3
  420            NFIELDS = 2
  421          ELSE IF (NML_FORCING%FIELD%WINDS_AST) THEN
  422            IDFLD = 'WNS'
  423            IFLD = 3
  424            FLSTAB = .TRUE.
  425            NFIELDS = 3
  426          ELSE IF (NML_FORCING%FIELD%CURRENTS) THEN
  427            IDFLD = 'CUR'
  428            IFLD = 4
  429            NFIELDS = 2
  430          ELSE IF (NML_FORCING%FIELD%DATA_ASSIM) THEN
  431            IDFLD = 'DAT'
  432            IFLD = 5
  433            ITYPE = 5
  434            NFIELDS = 1
  435          ELSE
  436             GOTO 810
  437          END IF ! NML_FORCING
  438                              
  439         ! Check grid asis/latlon
  440         IF (NML_FORCING%GRID%ASIS) THEN
 
 
                                                         ww3_prnc.F90  page   9
 
 
  441           ITYPE = 1
  442         ELSE IF (NML_FORCING%GRID%LATLON) THEN
  443           ITYPE = 2
  444          ELSE
  445            GOTO 811
  446         END IF
  447               
  448         ! Check tidal component
  449         TIDEFLAG = 0
  450         IF (TRIM(NML_FORCING%TIDAL).NE.'unset' .AND. &
  451             TRIM(NML_FORCING%TIDAL).NE.'UNSET') THEN
  452           TIDEFLAG = 1
  453           ITYPE = 6
  454           LIST(:)=''
  455           CALL STRSPLIT(TRIM(NML_FORCING%TIDAL),LIST)
  456         END IF
  457               
  458         ! Check file name, dimensions, variables
  459         NFCOMP = 1 ! not anymore used 'F1' 'F2' ?
  460         NAMEF=TRIM(NML_FILE%FILENAME)
  461         DIMSNAME(1)=NML_FILE%LONGITUDE
  462         DIMSNAME(2)=NML_FILE%LATITUDE
  463         DO I=1,NFIELDS
  464           FIELDSNAME(I)=NML_FILE%VAR(I)
  465         END DO
  466         ! Counts the number of dimensions
  467         NIDIMS=0
  468         DO I=1,2
  469           IF (LEN_TRIM(DIMSNAME(I)).NE.0) NIDIMS=NIDIMS+1
  470         END DO
  471               
  472               
  473         ! Check time start and stop
  474         READ(NML_FORCING%TIMESTART,*) TIMESTART
  475         CALL T2D(TIMESTART,STARTDATE,IERR)
  476         CALL D2J(STARTDATE,STARTJULDAY,IERR)
  477         READ(NML_FORCING%TIMESTOP,*) TIMESTOP
  478         CALL T2D(TIMESTOP,STPDATE,IERR)
  479         CALL D2J(STPDATE,STPJULDAY,IERR)
  480                                         
  481         ! Check time shift
  482         FLHDR = .TRUE.
  483         FLTIME = .TRUE.
  484         READ(NML_FILE%TIMESHIFT,*) TIMESHIFT
  485         IF(TIMESHIFT(1).NE.0 .OR. TIMESHIFT(2).NE.0) FLTIME = .FALSE.
  486                                                                      
  487       END IF ! FLGNML
  488                      
  489 !
  490 ! process old ww3_prnc.inp format
  491 !
  492       IF (.NOT. FLGNML) THEN
  493         OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_prnc.inp',STATUS='OLD',ERR=800>
  494         REWIND (NDSI)
  495                      
 
 
                                                         ww3_prnc.F90  page  10
 
 
  496         READ (NDSI,'(A)',END=801,ERR=802,IOSTAT=IERR) COMSTR
  497         IF (COMSTR.EQ.' ') COMSTR = '$'
  498         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,901) COMSTR
  499         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  500         READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) IDFLD, IDTYPE, FLTIME, >
  501                                                                           
  502         ! Check field
  503         FLSTAB = IDFLD .EQ. 'WNS'
  504         FLBERG = IDFLD .EQ. 'ISI'
  505         IF        ( IDFLD.EQ.'IC1' ) THEN
  506             IFLD    = -7
  507           ELSE IF ( IDFLD.EQ.'IC2' ) THEN
  508             IFLD    = -6
  509           ELSE IF ( IDFLD.EQ.'IC3' ) THEN
  510             IFLD    = -5
  511           ELSE IF ( IDFLD.EQ.'IC4' ) THEN
  512             IFLD    = -4
  513           ELSE IF ( IDFLD.EQ.'IC5' ) THEN
  514             IFLD    = -3
  515           ELSE IF ( IDFLD.EQ.'MDN' ) THEN
  516             IFLD    = -2
  517           ELSE IF ( IDFLD.EQ.'MTH' ) THEN
  518             IFLD    = -1
  519           ELSE IF ( IDFLD.EQ.'MVS' ) THEN
  520             IFLD    = 0
  521           ELSE IF ( IDFLD.EQ.'ICE' .OR. FLBERG ) THEN
  522             IFLD    = 1
  523           ELSE IF ( IDFLD.EQ.'LEV' ) THEN
  524             IFLD    = 2
  525           ELSE IF ( IDFLD.EQ.'WND' .OR. FLSTAB ) THEN
  526             IFLD    = 3
  527           ELSE IF ( IDFLD.EQ.'CUR' ) THEN
  528             IFLD    = 4
  529           ELSE IF ( IDFLD.EQ.'DAT' ) THEN
  530             IFLD    = 5
  531           ELSE
  532             WRITE (NDSE,1030) IDFLD
  533             CALL EXTCDE ( 30 )
  534           END IF
  535                 
  536         ! Check grid and tidal component
  537         NFCOMP = 1
  538         TIDEFLAG = 0
  539         IF (IDFLD.EQ.'DAT') THEN
  540           ITYPE   = 5
  541         ELSE IF (IDTYPE.EQ.'AI') THEN
  542           ITYPE   = 1
  543         ELSE IF (IDTYPE.EQ.'AT') THEN
  544           ITYPE   = 6
  545           TIDEFLAG= 1
  546           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  547           READ (NDSI,'(A)',END=801,ERR=803,IOSTAT=IERR) TIDECONSTNAMES
  548           LIST(:)=''
  549           CALL STRSPLIT(TIDECONSTNAMES,LIST)
  550         ELSE IF (IDTYPE.EQ.'LL') THEN
 
 
                                                         ww3_prnc.F90  page  11
 
 
  551           ITYPE   = 2
  552         ELSE IF (IDTYPE.EQ.'F1') THEN
  553           ITYPE   = 3
  554         ELSE IF (IDTYPE.EQ.'F2') THEN
  555           ITYPE   = 4
  556           NFCOMP = 2
  557         ELSE
  558           WRITE (NDSE,1031) IDTYPE
  559           CALL EXTCDE ( 31 )
  560         END IF
  561 !
  562         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  563         READ (NDSI,'(A)',END=801,ERR=802,IOSTAT=IERR) STRDIMSNAME
  564 !
  565         FIELDSNAME(:)=''
  566         DIMSNAME(:)=''
  567         CALL STRSPLIT(STRDIMSNAME,DIMSNAME)
  568         ! Counts the number of dimensions
  569         NIDIMS=0
  570         DO I=1,2
  571           IF (LEN_TRIM(DIMSNAME(I)).NE.0) NIDIMS=NIDIMS+1
  572         END DO
  573 !
  574         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  575         READ (NDSI,'(A)',END=801,ERR=802,IOSTAT=IERR) STRFIELDSNAME
  576 !
  577         FIELDSNAME(:)=''
  578         CALL STRSPLIT(STRFIELDSNAME,FIELDSNAME)
  579         ! Counts the number of variables
  580         NFIELDS=0
  581         DO WHILE (LEN_TRIM(FIELDSNAME(NFIELDS+1)).NE.0)
  582           NFIELDS=NFIELDS+1
  583         END DO
  584         ! time flag and start date
  585         IF (.NOT. FLTIME) THEN
  586           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  587           READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) TIMESHIFT
  588           IF (TIMESHIFT(1).LT.10000000) THEN
  589             WRITE (NDSE,1035) TIME
  590             CALL EXTCDE ( 35 )
  591           END IF
  592         END IF
  593         ! Read netcdf filename
  594         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  595         READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) NAMEF
  596                                                        
  597         ! initialize timestart and timestop
  598         STARTJULDAY=0
  599         STPJULDAY=100000000
  600                            
  601       END IF ! .NOT. FLGNML
  602                            
  603                            
  604 !
  605 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
                                                         ww3_prnc.F90  page  12
 
 
  606 ! 4. Print logs
  607 !
  608       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,930) IDSTR1(IFLD), IDSTR2(ITY>
  609       IF ( ITYPE.NE.1 .AND. ITYPE.NE.6 ) THEN
  610           IF ( IAPROC .EQ. NAPOUT .AND.IFLD.EQ.3) WRITE (NDSO,2930)
  611           IF ( IAPROC .EQ. NAPOUT .AND.IFLD.EQ.4) WRITE (NDSO,1930)
  612       END IF
  613       IF (FLGNML) THEN
  614         IF(TIMESTART(1).NE.19000101 .OR. TIMESTART(2).NE.0) THEN
  615           CALL STME21 ( TIMESTART , IDTIME )
  616           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1931) IDTIME
  617         END IF
  618         IF(TIMESTOP(1).NE.29001231 .OR. TIMESTOP(2).NE.0) THEN
  619           CALL STME21 ( TIMESTOP , IDTIME )
  620           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2931) IDTIME
  621         END IF
  622       END IF
  623       IF (.NOT. FLTIME) THEN
  624         CALL STME21 ( TIMESHIFT , IDTIME )
  625         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,3931) IDTIME
  626       END IF
  627       IF ( IAPROC .EQ. NAPOUT .AND.FLBERG ) WRITE (NDSO,938)
  628       IF ( IAPROC .EQ. NAPOUT .AND.FLSTAB ) WRITE (NDSO,939)
  629                                                             
  630       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,967) NAMEF
  631       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,968) TRIM(DIMSNAME(1)), TRIM(>
  632       DO I=1,NFIELDS
  633         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,969) I, TRIM(FIELDSNAME(I))
  634       END DO
  635             
  636             
  637 !
  638 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  639 ! 5. Read Input netcdf file
  640 !
  641  
  642       ! open input file
  643       IRET=NF90_OPEN(PATH=TRIM(FNMPRE)//NAMEF,MODE=NF90_NOWRITE,NCID=NCID)
  644       CALL CHECK_ERR(IRET)
  645                           
  646       ! instanciates time
  647       REFDATE(:)=0.
  648       IRET=NF90_INQ_VARID(NCID,"time",VARIDTMP)
  649       IF ( IRET/=NF90_NOERR ) IRET=NF90_INQ_VARID(NCID,"MT",VARIDTMP)
  650       CALL CHECK_ERR(IRET)
  651       IRET=NF90_GET_ATT(NCID,VARIDTMP,"calendar",CALENDAR)
  652       IF ( IRET/=NF90_NOERR ) THEN
  653         WRITE(NDSE,1028)
  654       ELSE IF ((INDEX(CALENDAR, "standard").EQ.0) .AND. &
  655                (INDEX(CALENDAR, "gregorian").EQ.0)) THEN
  656         WRITE(NDSE,1029)
  657       END IF
  658       IRET=NF90_GET_ATT(NCID,VARIDTMP,"units",TIMEUNITS)
  659       CALL U2D(TIMEUNITS,REFDATE,IERR)
  660       CALL D2J(REFDATE,REFJULDAY,IERR)
 
 
                                                         ww3_prnc.F90  page  13
 
 
  661                                       
  662       ! gets variables ids, dimensions and fillvalue
  663       DO I=1,NFIELDS
  664         IRET = NF90_INQ_VARID(NCID,TRIM(FIELDSNAME(I)),VARIDF(I))
  665         CALL CHECK_ERR(IRET)
  666         IRET = NF90_INQUIRE_VARIABLE(NCID, VARIDF(I), ndims=NDIMSVAR)
  667         CALL CHECK_ERR(IRET)
  668         IRET = NF90_INQUIRE_VARIABLE(NCID, VARIDF(I), dimids=DIMSVAR(:NDI>
  669         CALL CHECK_ERR(IRET)
  670         DO J=1,NDIMSVAR
  671           IRET=NF90_INQUIRE_DIMENSION(NCID,DIMSVAR(J),name=DIMNAME(J), le>
  672           CALL CHECK_ERR(IRET)
  673         END DO
  674         IRET=NF90_GET_ATT(NCID,VARIDF(I),"_FillValue", FILLVALUE)
  675         IF ( IRET/=NF90_NOERR ) THEN
  676           WRITE(NDSE,1027) TRIM(FIELDSNAME(I))
  677           CALL EXTCDE ( 27 )
  678         END IF
  679       END DO
  680             
  681       ! instanciates generic variables dimensions
  682       NXI=0
  683       NYI=0
  684       NDIMSGRID=2
  685       DO i=1,NDIMSVAR
  686         IF (DIMNAME(i) .EQ. "time".OR.DIMNAME(i) .EQ."MT") NTI = DIMLN(i)
  687         IF (DIMNAME(i) .EQ. DIMSNAME(1)) NXI = DIMLN(i)
  688         IF (DIMNAME(i) .EQ. DIMSNAME(1).AND.NIDIMS.EQ.1) THEN
  689           NDIMSGRID=1
  690           NYI = 1
  691         END IF
  692         IF (NIDIMS.GE.2) THEN
  693           IF (DIMNAME(i) .EQ. DIMSNAME(2)) NYI = DIMLN(i)
  694         END IF
  695       END DO
  696       IF (NXI*NYI.EQ.0) GOTO 864
  697                                 
  698       ! Set factor for deg/km
  699       IF ( FLAGLL ) THEN
  700         FACTOR = 1.
  701       ELSE
  702         FACTOR = 1.E-3
  703       END IF
  704             
  705       ! Get longitude and latitude
  706       IF (ITYPE.NE.1.AND.ITYPE.NE.6) THEN
  707         ALLOCATE (ALA(NXI,NYI))
  708         ALLOCATE (ALO(NXI,NYI))
  709         ! get longitude
  710         IRET=NF90_INQ_VARID(NCID,"longitude",VARIDTMP)
  711         IF ( IRET/=NF90_NOERR ) IRET=NF90_INQ_VARID(NCID,"lon",VARIDTMP)
  712         IF ( IRET/=NF90_NOERR ) IRET=NF90_INQ_VARID(NCID,"Longitude",VARI>
  713         IF ( IRET/=NF90_NOERR ) IRET=NF90_INQ_VARID(NCID,"x",VARIDTMP)
  714         IF ( IRET/=NF90_NOERR ) IRET=NF90_INQ_VARID(NCID,"X",VARIDTMP)
  715         IRET = NF90_INQUIRE_VARIABLE(NCID, VARIDTMP, ndims = NUMDIMS)
 
 
                                                         ww3_prnc.F90  page  14
 
 
  716         call CHECK_ERR(IRET)
  717         IF (NUMDIMS.EQ.1) THEN
  718           IRET=NF90_GET_VAR(NCID,VARIDTMP,X0I,start=(/1/))
  719           call CHECK_ERR(IRET)
  720           IRET=NF90_GET_VAR(NCID,VARIDTMP,XNI,start=(/NXI/))
  721           call CHECK_ERR(IRET)
  722           IRET=NF90_GET_VAR(NCID,VARIDTMP,ALO(:,1))
  723           call CHECK_ERR(IRET)
  724           DO i=1,NYI
  725             ALO(:,i)=ALO(:,1)
  726           END DO
  727         ELSE
  728           IRET=NF90_GET_VAR(NCID,VARIDTMP,X0I,start=(/1,1/))
  729           call CHECK_ERR(IRET)
  730           IRET=NF90_GET_VAR(NCID,VARIDTMP,XNI,start=(/NXI,1/))
  731           call CHECK_ERR(IRET)
  732           IRET=NF90_GET_VAR(NCID,VARIDTMP,ALO(:,:))
  733           call CHECK_ERR(IRET)
  734         END IF
  735         ! get latitude
  736         IRET=NF90_INQ_VARID(NCID,"latitude",VARIDTMP)
  737         IF ( IRET/=NF90_NOERR ) IRET=NF90_INQ_VARID(NCID,"lat",VARIDTMP)
  738         IF ( IRET/=NF90_NOERR ) IRET=NF90_INQ_VARID(NCID,"Latitude",VARID>
  739         IF ( IRET/=NF90_NOERR ) IRET=NF90_INQ_VARID(NCID,"y",VARIDTMP)
  740         IF ( IRET/=NF90_NOERR ) IRET=NF90_INQ_VARID(NCID,"Y",VARIDTMP)
  741         IRET = NF90_INQUIRE_VARIABLE(NCID, VARIDTMP, ndims = NUMDIMS)
  742         CALL CHECK_ERR(IRET)
  743         IRET=NF90_GET_VAR(NCID,VARIDTMP,Y0I, start=(/1/))
  744         CALL CHECK_ERR(IRET)
  745         IF (NUMDIMS.EQ.1) THEN
  746           IRET=NF90_GET_VAR(NCID,VARIDTMP,ALA(1,:))
  747           CALL CHECK_ERR(IRET)
  748           YNI=ALA(1,NYI)
  749           DO i=1,NXI
  750             ALA(i,:)=ALA(1,:)
  751           END DO
  752         ELSE
  753           IRET=NF90_GET_VAR(NCID,VARIDTMP,ALA(:,:))
  754           CALL CHECK_ERR(IRET)
  755           YNI=ALA(1,NYI)
  756         END IF
  757       END IF
  758             
  759             
  760             
  761 !
  762 ! ... type 1 or 6 : "As Is" (AI) or "As Is with tide" (AT)
  763 !
  764       IF (ITYPE.EQ.1.OR.ITYPE.EQ.6) THEN
  765 !
  766         NXI    = NX
  767         NYI    = NY
  768         ALLOCATE ( MASK(NXI,NYI) )
  769         MASK   = 1
  770         IF(GTYPE .EQ. UNGTYPE) THEN
 
 
                                                         ww3_prnc.F90  page  15
 
 
  771 !
  772 !  X0, Y0 are the coordinates of the lower-left point in mesh
  773 !
  774           RW(1) = FACTOR*X0        ; RW(2) = FACTOR*MAXX
  775           RW(3) = FACTOR*Y0        ; RW(4) = FACTOR*MAXY
  776         ELSE
  777           RW(1) = FACTOR*XGRD(1,1) ; RW(2) = FACTOR*XGRD(NY,NX)
  778           RW(3) = FACTOR*YGRD(1,1) ; RW(4) = FACTOR*YGRD(NY,NX)
  779         END IF
  780         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,932) NXI, NYI
  781         IF ( FLAGLL ) THEN
  782           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1933) RW(1),RW(2),RW(3),R>
  783         ELSE
  784           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2933) RW(1),RW(2),RW(3),R>
  785         END IF
  786 !
  787 ! ... type 2 : "Lat/Lon" (LL)
  788 !
  789       ELSE IF (ITYPE.EQ.2) THEN
  790 !
  791         ! check latitude values order
  792         IF ((GTYPE .EQ. RLGTYPE) .AND. (Y0I.GT.YNI)) THEN
  793           WRITE (NDSE,1032)
  794           CALL EXTCDE ( 32 )
  795         END IF
  796               
  797         IF (NXI.LT.2 .OR. NYI.LT.2) THEN
  798           WRITE (NDSE,1036) NXI, NYI
  799           CALL EXTCDE ( 36 )
  800         END IF
  801         ALLOCATE ( MASK(NXI,NYI) )
  802         MASK   = 1
  803         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,932) NXI, NYI
  804                                                            
  805         IF ( FLAGLL ) THEN
  806           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1933) FACTOR*X0I, FACTOR*>
  807                            FACTOR*Y0I, FACTOR*YNI
  808         ELSE
  809           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2933) FACTOR*X0I, FACTOR*>
  810                            FACTOR*Y0I, FACTOR*YNI
  811         END IF
  812 !
  813 ! ... type 5 : "Data" (DAT)
  814 !
  815       ELSE IF (ITYPE.EQ.5) THEN
  816         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  817         READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR)                   &
  818               DATTYP, RECLDT, NODATA
  819         IF (DATTYP.LT.0 .OR. DATTYP.GT.2) THEN
  820           WRITE (NDSE,1033) DATTYP
  821           CALL EXTCDE ( 33 )
  822         END IF
  823         IF (RECLDT.LE.0) THEN
  824           WRITE (NDSE,1034) RECLDT
  825           CALL EXTCDE ( 34 )
 
 
                                                         ww3_prnc.F90  page  16
 
 
  826         END IF
  827         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,934) IDSTR3(DATTYP+1), RECL>
  828         WRITE (IDFLD,935) DATTYP
  829         DEALLOCATE ( IX21, IX22, IY21, IY22, JX21, JX22, JY21, JY22, &
  830                      MAPOVR )
  831         DEALLOCATE ( RD11, RD21, RD12, RD22, XD11, XD21, XD12, XD22, &
  832                      FX, FY, FA, A1, A2, A3 )
  833 !
  834 ! ... types 3 and 4 ... in preprocessing loop ....
  835 !
  836       END IF
  837 !
  838 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  839 ! 6   Prepare interpolation.
  840 !
  841       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,940)
  842 !
  843       IF (ITYPE.NE.1 .AND. ITYPE.NE.5 .AND. ITYPE.NE.6 ) THEN
  844 !
  845 ! 6.a Longitude - latitude grid
  846 !
  847         IF (ITYPE.EQ.2) THEN
  848           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,941)
  849 !
  850 ! ... setup coordinates
  851 !
  852           SXI    = (XNI-X0I)/REAL(NXI-1)
  853           SYI    = (YNI-Y0I)/REAL(NYI-1)
  854           ICLO   = ICLOSE_NONE
  855           IF ( FLAGLL ) THEN
  856             IF ( ABS(ABS(REAL(NXI)*SXI)-360.) .LT. 0.1*ABS(SXI) ) THEN
  857               ICLO = ICLOSE_SMPL
  858             END IF
  859           END IF
  860 !
  861 ! ... create grid search utility
  862 !
  863           GSI = W3GSUC( .TRUE., FLAGLL, ICLO, ALO, ALA )
  864 !
  865 ! ... construct Interpolation data
  866 !
  867           IF (GTYPE .NE. UNGTYPE) THEN
  868             DO IY=1,NY
  869               DO IX=1,NX
  870                 INGRID = W3GRMP( GSI, XGRD(IY,IX), YGRD(IY,IX),      &
  871                                  IS, JS, RW )
  872                 IF ( .NOT.INGRID ) THEN
  873                     IF ( IAPROC .EQ. NAPOUT ) WRITE(NDSO,1042) IX, IY, XG>
  874                   IX21(IX,IY) = 1
  875                   IX22(IX,IY) = 1
  876                   IY21(IX,IY) = 1
  877                   IY22(IX,IY) = 1
  878                   RD11(IX,IY) = 0.
  879                   RD21(IX,IY) = 0.
  880                   RD12(IX,IY) = 0.
 
 
                                                         ww3_prnc.F90  page  17
 
 
  881                   RD22(IX,IY) = 0.
  882                                   
  883                   CYCLE
  884                   END IF
  885                 IX21(IX,IY) = IS(1)
  886                 IX22(IX,IY) = IS(2)
  887                 IY21(IX,IY) = JS(1)
  888                 IY22(IX,IY) = JS(4)
  889                 RD11(IX,IY) = RW(1)
  890                 RD21(IX,IY) = RW(2)
  891                 RD12(IX,IY) = RW(4)
  892                 RD22(IX,IY) = RW(3)
  893                 END DO
  894               END DO
  895           ELSE ! GTYPE .NE. UNGTYPE
  896             DO IX=1, NX
  897               X = XYB(IX,1)
  898               Y = XYB(IX,2)
  899               IX21(IX,1) =   1 + INT(MOD(360.+(X-X0I),360.)/SXI)
  900 !
  901 ! Manages the simple closure of the grid
  902 !
  903               IF (ICLO.EQ.ICLOSE_NONE) THEN
  904                 IF (IX21(IX,1).LT.1.OR.IX21(IX,1).GT.NXI-1) WRITE(NDSO,10>
  905                 IX21(IX,1) =   MAX ( 1 , MIN(IX21(IX,1),NXI-1) )
  906                 IX22(IX,1) =   IX21(IX,1) + 1
  907               ELSE
  908                 IX21(IX,1) =   MAX ( 1 , MIN(IX21(IX,1),NXI) )
  909                 IX22(IX,1) =   MOD(IX21(IX,1),NXI)+1
  910                 END IF
  911               IY21(IX,1) =   1 + INT((Y-Y0I)/SYI)
  912               IF (IY21(IX,1).LT.1.OR.IY21(IX,1).GT.NYI-1) WRITE(NDSO,1042>
  913               IY21(IX,1) =   MAX ( 1 , MIN(IY21(IX,1),NYI-1) )
  914               IY22(IX,1) =   IY21(IX,1) + 1
  915 !
  916               RW(1) =  MOD(360.+(X-X0I),360.)/SXI - REAL(IX21(IX,1)-1)
  917               RW(2) =  (Y-Y0I)/SYI - REAL(IY21(IX,1)-1)
  918 !
  919               IF (IX21(IX,1).LE.1 .AND. RW(1).LT.ACC) THEN
  920                 IF (RW(1).LT.0.) THEN
  921                   RW(1) = 0.
  922                   IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1043) X
  923                   END IF
  924                 END IF
  925 !
  926               IF (IX21(IX,1).GE.(NXI-1) .AND. RW(1).GT.1.-ACC) THEN
  927                 IF (RW(1).GT.1.) THEN
  928                   IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1043) X
  929                   RW(1) = 1.
  930                   END IF
  931                 END IF
  932 !
  933               IF (IY21(IX,1).LE.1 .AND. RW(2).LT.ACC) THEN
  934                 IF (RW(2).LT.0.) THEN
  935                   IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1044) Y
 
 
                                                         ww3_prnc.F90  page  18
 
 
  936                   RW(2) = 0.
  937                 END IF
  938               END IF
  939 !
  940               IF (IY21(IX,1).GE.NYI .AND. RW(2).GT.1.-ACC) THEN
  941                 IF (RW(2).GT.1) THEN
  942                   IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1044) Y
  943                   RW(2) = 1.
  944                 END IF
  945               END IF
  946 !
  947               EFAC = SQRT ( MAX(0.,ABS(RW(1)-0.5)-0.5)**2 +  &
  948                             MAX(0.,ABS(RW(2)-0.5)-0.5)**2 )
  949               EFAC        = 1. / ( 1. + 0.25*EFAC**2 )
  950                                                       
  951               RD11(IX,1) = EFAC * (1.-RW(1)) * (1.-RW(2))
  952               RD21(IX,1) = EFAC *    RW(1)   * (1.-RW(2))
  953               RD12(IX,1) = EFAC * (1.-RW(1)) *    RW(2)
  954               RD22(IX,1) = EFAC *    RW(1)   *    RW(2)
  955                                                        
  956             END DO ! IX=1, NX
  957           END IF ! GTYPE .NE. UNGTYPE
  958 !
  959           CALL W3GSUD( GSI )
  960                             
  961 !
  962 ! 6.b Grid(s) from file
  963 !
  964         ELSE ! ITYPE.EQ.2
  965           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,942)
  966 !
  967 ! ... prepare overlay map
  968 !
  969           DO IY=1, NY
  970             DO IX=1, NX
  971               IF ( MAPSTA(IY,IX) .EQ. 0 ) THEN
  972                   MAPOVR(IX,IY) = ILAND
  973                 ELSE
  974                   MAPOVR(IX,IY) = 0
  975               END IF
  976             END DO
  977           END DO
  978 !
  979 ! ... loop over fields
  980 !
  981           DO J=1, NFCOMP
  982 !
  983             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) J
  984 !
  985 ! ... file info lat-long file
  986 !
  987             CALL NEXTLN ( COMSTR , NDSI , NDSE )
  988             READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR)             &
  989                   NXJ(J), NYJ(J), CLO(J)
  990             IF (NXJ(J).LT.2 .OR. NYJ(J).LT.2) THEN
 
 
                                                         ww3_prnc.F90  page  19
 
 
  991                 WRITE (NDSE,1036) NXJ(J), NYJ(J)
  992                 CALL EXTCDE ( 36 )
  993               END IF
  994             IF ( ALLOCATED(MASK) ) DEALLOCATE (MASK)
  995             ALLOCATE ( MASK(NXJ(J),NYJ(J)) )
  996             MASK   = 1
  997             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,944) NXJ(J), NYJ(J), CL>
  998 !
  999             CALL NEXTLN ( COMSTR , NDSI , NDSE )
 1000             READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR)             &
 1001                   FROMLL, IDLALL, IDFMLL, FORMLL
 1002             IF (IDLALL.LT.1 .OR. IDLALL.GT.4) IDLALL   = 1
 1003             IF (IDFMLL.LT.1 .OR. IDFMLL.GT.3) IDFMLL   = 1
 1004             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,945) IDLALL, IDFMLL
 1005             IF (IDFMLL.EQ.2) WRITE (NDSO,946) FORMLL
 1006 !
 1007             CALL NEXTLN ( COMSTR , NDSI , NDSE )
 1008             READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) NDSLL, NAMELL
 1009             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,947) NDSLL
 1010             IF ( IAPROC .EQ. NAPOUT.AND.FROMLL.EQ.'NAME') WRITE (NDSO,948>
 1011             IF (NDSLL.EQ.NDSI) THEN
 1012                 WRITE (NDSE,1038)
 1013                 CALL NEXTLN ( COMSTR , NDSI , NDSE )
 1014             ELSE
 1015 !
 1016 ! ... open lat-long file
 1017 !
 1018               IF ( IDFMLL .EQ. 3 ) THEN
 1019                 IF (FROMLL.EQ.'NAME') THEN
 1020                   JJ = LEN_TRIM(FNMPRE)
 1021                   OPEN (NDSLL,FILE=FNMPRE(:JJ)//NAMELL,     &
 1022                               FORM='UNFORMATTED',STATUS='OLD',    &
 1023                               ERR=845,IOSTAT=IERR)
 1024                 ELSE
 1025                   OPEN (NDSLL, FORM='UNFORMATTED',          &
 1026                                STATUS='OLD',ERR=845,IOSTAT=IERR)
 1027                 END IF
 1028               ELSE
 1029                 IF (FROMLL.EQ.'NAME') THEN
 1030                   JJ = LEN_TRIM(FNMPRE)
 1031                   OPEN (NDSLL,FILE=FNMPRE(:JJ)//NAMELL,     &
 1032                               STATUS='OLD',ERR=845,IOSTAT=IERR)
 1033                 ELSE
 1034                   OPEN (NDSLL,                              &
 1035                               STATUS='OLD',ERR=845,IOSTAT=IERR)
 1036                 END IF
 1037               END IF
 1038 !
 1039             END IF
 1040 !
 1041 ! ... read lat-lon data
 1042 !
 1043             IF ( ASSOCIATED(ALA) ) THEN
 1044                 DEALLOCATE ( ALA, ALO )
 1045                 NULLIFY ( ALA, ALO )
 
 
                                                         ww3_prnc.F90  page  20
 
 
 1046             END IF
 1047             ALLOCATE ( ALA(NXJ(J),NYJ(J)), ALO(NXJ(J),NYJ(J)) )
 1048             CALL INA2R (ALA, NXJ(J), NYJ(J), 1, NXJ(J), 1, NYJ(J),&
 1049                  NDSLL, NDST, NDSE, IDFMLL, FORMLL, IDLALL, 1., 0.)
 1050             CALL INA2R (ALO, NXJ(J), NYJ(J), 1, NXJ(J), 1, NYJ(J),&
 1051                  NDSLL, NDST, NDSE, IDFMLL, FORMLL, IDLALL, 1., 0.)
 1052 !
 1053             IF ( NDSLL .NE. NDSI ) CLOSE (NDSLL)
 1054 !
 1055 ! ... file info mask file
 1056 !
 1057             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,949)
 1058 !
 1059             CALL NEXTLN ( COMSTR , NDSI , NDSE )
 1060             READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR)             &
 1061                   FROMLL, IDLALL, IDFMLL, FORMLL
 1062             IF (IDLALL.LT.1 .OR. IDLALL.GT.4) IDLALL   = 1
 1063             IF (IDFMLL.LT.1 .OR. IDFMLL.GT.3) IDFMLL   = 1
 1064             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,945) IDLALL, IDFMLL
 1065             IF (IDFMLL.EQ.2) WRITE (NDSO,946) FORMLL
 1066 !
 1067             CALL NEXTLN ( COMSTR , NDSI , NDSE )
 1068             READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) NDSLL, NAMELL
 1069             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,947) NDSLL
 1070             IF (FROMLL.EQ.'NAME') WRITE (NDSO,948) NAMELL
 1071             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,*) ' '
 1072             IF (NDSLL.EQ.NDSI) THEN
 1073                 WRITE (NDSE,1038)
 1074                 CALL NEXTLN ( COMSTR , NDSI , NDSE )
 1075             ELSE
 1076 !
 1077 ! ... open mask file
 1078 !
 1079               IF ( IDFMLL .EQ. 3 ) THEN
 1080                 IF (FROMLL.EQ.'NAME') THEN
 1081                   JJ = LEN_TRIM(FNMPRE)
 1082                   OPEN (NDSLL,FILE=FNMPRE(:JJ)//NAMELL,     &
 1083                             FORM='UNFORMATTED',STATUS='OLD',    &
 1084                             ERR=846,IOSTAT=IERR)
 1085                 ELSE
 1086                   OPEN (NDSLL,FORM='UNFORMATTED',           &
 1087                               STATUS='OLD',ERR=846,IOSTAT=IERR)
 1088                 END IF
 1089               ELSE
 1090                 IF (FROMLL.EQ.'NAME') THEN
 1091                   JJ = LEN_TRIM(FNMPRE)
 1092                   OPEN (NDSLL,FILE=FNMPRE(:JJ)//NAMELL,     &
 1093                               STATUS='OLD',ERR=846,IOSTAT=IERR)
 1094                 ELSE
 1095                   OPEN (NDSLL,                              &
 1096                               STATUS='OLD',ERR=846,IOSTAT=IERR)
 1097                 END IF
 1098               END IF
 1099 !
 1100             END IF
 
 
                                                         ww3_prnc.F90  page  21
 
 
 1101 !
 1102 ! ... read mask data
 1103 !
 1104             CALL INA2I (MASK, NXJ(J), NYJ(J), 1,NXJ(J), 1,NYJ(J), &
 1105                  NDSLL, NDST, NDSE, IDFMLL, FORMLL, IDLALL, 1, 0)
 1106             IF ( NDSLL .NE. NDSI ) CLOSE (NDSLL)
 1107 !
 1108 ! ... generate interpolation data
 1109 !
 1110             IF ( J .EQ. 1 ) THEN
 1111               CALL W3FLDP ( NDSO, NDST, NDSE, IERR, FLAGLL,       &
 1112                 NX, NY, NX, NY, YGRD, XGRD, MAPOVR, ILAND,        &
 1113                 NXJ(J), NYJ(J), NXJ(J), NYJ(J), CLO(J), ALA, ALO, &
 1114                 MASK, RD11, RD21, RD12, RD22, IX21, IX22, IY21,   &
 1115                 IY22 )
 1116             ELSE
 1117               CALL W3FLDP ( NDSO, NDST, NDSE, IERR, FLAGLL,       &
 1118                 NX, NY, NX, NY, YGRD, XGRD, MAPOVR, ILAND,        &
 1119                 NXJ(J), NYJ(J), NXJ(J), NYJ(J), CLO(J), ALA, ALO, &
 1120                 MASK, XD11, XD21, XD12, XD22, JX21, JX22, JY21,   &
 1121                 JY22 )
 1122             END IF ! J .EQ. 1
 1123 !
 1124           END DO ! J=1, NFCOMP
 1125 !
 1126 ! ... average two fields !
 1127 !
 1128           IF ( NFCOMP .EQ. 2) THEN
 1129             DO IX=1, NX
 1130               DO IY=1, NY
 1131                 IF ( MAPOVR(IX,IY) .GE. 2) THEN
 1132                   FACTOR = 1. / REAL(MAPOVR(IX,IY))
 1133                   RD11(IX,IY) = FACTOR * RD11(IX,IY)
 1134                   RD12(IX,IY) = FACTOR * RD12(IX,IY)
 1135                   RD21(IX,IY) = FACTOR * RD21(IX,IY)
 1136                   RD22(IX,IY) = FACTOR * RD22(IX,IY)
 1137                   XD11(IX,IY) = FACTOR * XD11(IX,IY)
 1138                   XD12(IX,IY) = FACTOR * XD12(IX,IY)
 1139                   XD21(IX,IY) = FACTOR * XD21(IX,IY)
 1140                   XD22(IX,IY) = FACTOR * XD22(IX,IY)
 1141                 END IF
 1142               END DO
 1143             END DO
 1144           END IF ! NFCOMP .EQ. 2
 1145 !
 1146         END IF ! ITYPE.EQ.2
 1147       END IF ! ITYPE.NE.1 .AND. ITYPE.NE.5
 1148 !
 1149 ! 6.c Input location and format
 1150 !
 1151       DO J=1, NFCOMP
 1152 !
 1153         IF ( ITYPE .EQ. 5 ) THEN
 1154           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,960)
 1155         ELSE
 
 
                                                         ww3_prnc.F90  page  22
 
 
 1156           IF (ITYPE.LE.3) THEN
 1157             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,961) NXJ(J), NYJ(J)
 1158           ELSE
 1159             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,962) J, NXJ(J), NYJ(J)
 1160           END IF
 1161         END IF ! ITYPE .EQ. 5
 1162 !
 1163       END DO
 1164 !
 1165 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 1166 ! 7   Prepare files
 1167 !
 1168       IF ( NFCOMP .EQ. 1 ) THEN
 1169           NXJ  (2) = NXJ  (1)
 1170           NYJ  (2) = NYJ  (1)
 1171           NDSF (2) = NDSF (1)
 1172           IDLAF(2) = IDLAF(1)
 1173           IDFMF(2) = IDFMF(1)
 1174           FORMT(2) = FORMT(1)
 1175           FORMF(2) = FORMF(1)
 1176         END IF
 1177               
 1178 ! 7.b Open and prepare output file
 1179 !
 1180       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,971)
 1181       J      = LEN_TRIM(FNMPRE)
 1182                                
 1183       ! define tidal constituents for analysis
 1184       IF (ITYPE.EQ.6) THEN
 1185         CALL VUF_SET_PARAMETERS
 1186         TIDE_NDEF = NFIELDS
 1187         IF (TRIM(LIST(1)).EQ.'ALL') THEN
 1188           WRITE(NDSE,'(A)') 'Tidal constituent ALL not available anymore'
 1189           CALL EXTCDE(29)
 1190         END IF
 1191         CALL TIDE_FIND_INDICES_ANALYSIS(LIST)
 1192       END IF
 1193             
 1194       ! Create output binary file
 1195       IF ( ITYPE .LE. 4 .OR. ITYPE.EQ.6 ) THEN
 1196         IF ( IAPROC .EQ. NAPOUT )  &
 1197              CALL W3FLDO ( 'WRITE', IDFLD, NDSDAT, NDST, NDSE,      &
 1198              NX, NY, GTYPE, IERR, FPRE=FNMPRE(:J),                  &
 1199              FHDR=FLHDR, TIDEFLAGIN=TIDEFLAG)
 1200         ELSE
 1201           IF ( IAPROC .EQ. NAPOUT )  &
 1202           CALL W3FLDO ( 'WRITE', IDFLD, NDSDAT, NDST, NDSE,           &
 1203                RECLDT, 0, GTYPEDUM, IERR, FPRE=FNMPRE(:J) )
 1204         END IF
 1205               
 1206               
 1207 !
 1208 ! 7.c Initialize fields
 1209 !
 1210       IF ( ITYPE .NE. 5 ) THEN
 
 
                                                         ww3_prnc.F90  page  23
 
 
 1211           FX = 0.
 1212           FY = 0.
 1213           FA = 0.
 1214           MXM = MAX ( NXJ(1), NXJ(2) )
 1215           MYM = MAX ( NYJ(1), NYJ(2) )
 1216           IF (ITYPE.EQ.1.AND.GTYPE.EQ.UNGTYPE) THEN
 1217             ALLOCATE ( XC(MXM,1), YC(MXM,1), AC(MXM,1) )
 1218           ELSE
 1219             ALLOCATE ( XC(MXM,MYM), YC(MXM,MYM), AC(MXM,MYM) )
 1220           END IF
 1221           XC = 0.
 1222           YC = 0.
 1223           AC = 0.
 1224         END IF
 1225 !
 1226 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 1227 !
 1228 ! Dedicated section to ITYPE.EQ.6
 1229 !
 1230 ! points are read one by one for tidal analysis
 1231 ! For other ITYPE, time steps are read one by one.
 1232 !
 1233  
 1234       IF (ITYPE.GE.6.AND.TIDEFLAG.GT.0) THEN
 1235 !
 1236 ! Reads in the full time vector
 1237 !
 1238         IF (NX*NY.GT.4000) THEN
 1239           WRITE(NDSE,*) 'Starting tidal analysis for ',NX*NY, ' points.'
 1240           IF (NAPROC.EQ.1) WRITE(NDSE,*) 'This can take hours ...Consider>
 1241           END IF
 1242         IRET=NF90_INQ_VARID(NCID,"time",VARIDTMP)
 1243         IF ( IRET/=NF90_NOERR ) IRET=NF90_INQ_VARID(NCID,"MT",VARIDTMP)
 1244         CALL CHECK_ERR(IRET)
 1245         ALLOCATE(ALLTIMES(NTI))
 1246         IRET=NF90_GET_VAR(NCID,VARIDTMP,ALLTIMES,start=(/1/))
 1247         CALL CHECK_ERR(IRET)
 1248         IF (INDEX(TIMEUNITS, "seconds").NE.0)   ALLTIMES=ALLTIMES/86400.
 1249         IF (INDEX(TIMEUNITS, "minutes").NE.0)   ALLTIMES=ALLTIMES/1440.
 1250         IF (INDEX(TIMEUNITS, "hours").NE.0)   ALLTIMES=ALLTIMES/24.
 1251         ALLTIMES=REFJULDAY+ALLTIMES
 1252                                    
 1253 !
 1254 ! Performs tidal analysis
 1255 !
 1256         TIDE_NTI = NTI
 1257         TIDE_NDEF = NFIELDS
 1258         ALLOCATE(SDEV0(TIDE_NDEF),SDEV(TIDE_NDEF), RMSR(TIDE_NDEF), &
 1259                    RES(TIDE_NDEF), SSQ(TIDE_NDEF),RMSR0(TIDE_NDEF), &
 1260                  RMSRP(TIDE_NDEF), IMAX(TIDE_NDEF), RESMAX(TIDE_NDEF))
 1261                                                                       
 1262         ALLOCATE( TIDE_DATA(TIDE_NTI,TIDE_NDEF) )
 1263         ALLOCATE( TIDE_DAYS(TIDE_NTI), TIDE_SECS(TIDE_NTI), TIDE_HOURS(TI>
 1264         ALLOCATE(V_ARG(170,TIDE_NTI),F_ARG(170,TIDE_NTI),U_ARG(170,TIDE_N>
 1265         TIDE_NX=NX
 
 
                                                         ww3_prnc.F90  page  24
 
 
 1266         TIDE_NY=NY
 1267         ALLOCATE(TIDAL_CONST(NX,NY,TIDE_MF,2,2))
 1268         TIDAL_CONST(:,:,:,:,:)=0.
 1269         DO I=1,NFIELDS
 1270           IRET=NF90_INQ_VARID(NCID,FIELDSNAME(I),VARIDF(I))
 1271         END DO
 1272         IRET=NF90_GET_ATT(NCID,VARIDTMP,"_FillValue", FILLVALUE)
 1273                                                                 
 1274         IRET = NF90_GET_ATT(NCID,VARIDF(1),'scale_factor',SCFAC(1))
 1275         IF (IRET .NE. 0) SCFAC(1) = 1.0
 1276         IRET = NF90_GET_ATT(NCID,VARIDF(1),'add_offset',ADDOFF(1))
 1277         IF (IRET .NE. 0) ADDOFF(1) = 0.0
 1278         IF ( NFCOMP.EQ.2 .OR. IFLD.GE.3 .OR. FLBERG ) THEN
 1279           IRET = NF90_GET_ATT(NCID,VARIDF(2),'scale_factor',SCFAC(2))
 1280           IF (IRET .NE. 0) SCFAC(2) = 1.0
 1281           IRET = NF90_GET_ATT(NCID,VARIDF(2),'add_offset',ADDOFF(1))
 1282           IF (IRET .NE. 0) ADDOFF(1) = 0.0
 1283         END IF
 1284               
 1285               
 1286 !
 1287 ! Set arrays for MPI exchanges
 1288 !
 1289         IF (NX .LT. NAPROC) THEN
 1290           WRITE(NDSE,*) 'NUMBER OF NX POINTS LESS THAN NUMBER OF PROC'
 1291           CALL EXTCDE (30)
 1292         END IF
 1293               
 1294               
 1295               
 1296               
 1297         ! set arrays for number of elements per MPI proc
 1298         ALLOCATE(CUMUL(NAPROC))
 1299         ALLOCATE(NELEM(NAPROC))
 1300         CUMUL(1) = 0
 1301         NELEM(1) = NX / NAPROC
 1302                               
 1303                               
 1304         ALLOCATE(TIDE_DATA_ALL(NELEM(IAPROC),NTI,NFIELDS))
 1305                                                           
 1306                                                           
 1307                                                           
 1308                                                           
 1309                                                           
 1310 !
 1311 ! Loops on Y dimension
 1312 !
 1313         DO IY=1,NY
 1314 !
 1315           IF (NDIMSGRID.EQ.1) THEN
 1316             DO I=1,NFIELDS
 1317               IRET=NF90_GET_VAR(NCID,VARIDF(I),TIDE_DATA_ALL(:,:,I), &
 1318                                 start=(/CUMUL(IAPROC)+1,1/),count=(/NELEM>
 1319               CALL CHECK_ERR(IRET)
 1320               WHERE (TIDE_DATA_ALL(:,:,I).NE.FILLVALUE) TIDE_DATA_ALL(:,:>
 
 
                                                         ww3_prnc.F90  page  25
 
 
 1321             END DO
 1322           ELSE IF (NDIMSGRID.EQ.2) THEN
 1323             IF (NDIMSVAR.EQ.3) THEN
 1324               DO I=1,NFIELDS
 1325                 IRET=NF90_GET_VAR(NCID,VARIDF(I),TIDE_DATA_ALL(:,:,I), &
 1326                                   start=(/CUMUL(IAPROC)+1,IY,1/),count=(/>
 1327                 CALL CHECK_ERR(IRET)
 1328                 WHERE (TIDE_DATA_ALL(:,:,I).NE.FILLVALUE) TIDE_DATA_ALL(:>
 1329               END DO
 1330             ELSE IF (NDIMSVAR.EQ.4) THEN
 1331               DO I=1,NFIELDS
 1332                 IRET=NF90_GET_VAR(NCID,VARIDF(I),TIDE_DATA_ALL(:,:,I), &
 1333                                   start=(/CUMUL(IAPROC)+1,IY,1,1/),count=>
 1334                 CALL CHECK_ERR(IRET)
 1335                 WHERE (TIDE_DATA_ALL(:,:,I).NE.FILLVALUE) TIDE_DATA_ALL(:>
 1336               END DO
 1337             END IF ! NDIMSVAR
 1338           END IF ! NDIMSGRID
 1339                             
 1340                             
 1341 !
 1342           DO JX=1,NELEM(IAPROC)
 1343            IX=JX
 1344                 
 1345 !
 1346             TIDE_NTI=0
 1347             DO I=1,NTI
 1348 !
 1349 ! Defines usable timesteps ... criteria could be improved
 1350 ! remove the times when the point IX,IY is dry ...
 1351 ! and redefine TIDE_NTI based on wet times only
 1352 !
 1353               IF (TIDE_DATA_ALL(JX,I,1).NE.FILLVALUE     &
 1354                   .AND.TIDE_DATA_ALL(JX,I,NFIELDS).NE.FILLVALUE
 1355                   .AND.TIDE_DATA_ALL(JX,I,1).NE.0.0) THEN
 1356                 TIDE_NTI=TIDE_NTI+1
 1357                 TIDE_DATA(TIDE_NTI,:)=TIDE_DATA_ALL(JX,I,:)
 1358                 TIDE_DAYS(TIDE_NTI)=INT(ALLTIMES(I))
 1359                 TIDE_SECS(TIDE_NTI)=(ALLTIMES(I)-TIDE_DAYS(TIDE_NTI))*864>
 1360               END IF
 1361             END DO ! NTI
 1362 !
 1363             TIDE_HOURS(1:TIDE_NTI)=24.d0*dfloat(TIDE_DAYS(1:TIDE_NTI)) &
 1364                                    +dfloat(TIDE_SECS(1:TIDE_NTI))/3600.d0
 1365                                                                          
 1366 !
 1367 ! Compute amplitude and phase
 1368 !
 1369             IF (TIDE_NTI.GT.(TIDE_MF*3)) THEN
 1370               TIDE_LAT= YGRD(IY,IX)
 1371               IF (ABS(TIDE_LAT).LT.5.) TIDE_LAT=SIGN(5.,TIDE_LAT)
 1372               DO I=1,TIDE_NTI
 1373                 CALL SETVUF(TIDE_HOURS(I),TIDE_LAT,I)
 1374               END DO
 1375               TIDE_ITREND=0
 
 
                                                         ww3_prnc.F90  page  26
 
 
 1376               CALL flex_tidana_webpage(IX,IY,XGRD(IY,IX),TIDE_LAT,TIDE_DA>
 1377                                        TIDE_NDEF, TIDE_ITREND, RES, SSQ, >
 1378                                        SDEV0, RMSR, RESMAX, IMAX, 0)
 1379                                                                     
 1380                                                                     
 1381             ELSE ! TIDE_NTI.GT.(TIDE_MF*3)
 1382               WRITE(NDSE,*) 'WARNING NOT ENOUGH DATA AT POINT:',IX,IY, NT>
 1383               TIDE_AMPC(1:TIDE_MF,1:NFIELDS)=0.
 1384               TIDE_PHG(1:TIDE_MF,1:NFIELDS)=0.
 1385             END IF ! end of test on TIDE_NTI
 1386                                             
 1387                                             
 1388 !
 1389 ! Save tidal amplitude and phase
 1390 !
 1391  
 1392  
 1393             TIDAL_CONST(IX,IY,1:TIDE_MF,1:NFIELDS,1)=TIDE_AMPC(1:TIDE_MF,>
 1394             TIDAL_CONST(IX,IY,1:TIDE_MF,1:NFIELDS,2)=TIDE_PHG(1:TIDE_MF,1>
 1395                                                                           
 1396           END DO  ! JX=1,NELEM(IAPROC)
 1397                                       
 1398 !
 1399 ! Gather from other MPI tasks
 1400 !
 1401  
 1402  
 1403 !
 1404 ! Convert from 1D to 2D array
 1405 !
 1406  
 1407  
 1408  
 1409         END DO ! IY=1,NY
 1410                         
 1411                         
 1412                         
 1413 !
 1414 ! After loop on points, write tidal constituents to file.
 1415 !
 1416         IF ( IAPROC .EQ. NAPOUT.AND.TIDEFLAG.GE.1)  &
 1417           CALL W3FLDTIDE1 ( 'WRITE',  NDSDAT, NDST, NDSE, NX, NY, IDFLD, >
 1418         CALL W3FLDTIDE2 ( 'WRITE',  NDSDAT, NDST, NDSE, NX, NY, IDFLD, 0,>
 1419 !
 1420    GOTO 880
 1421            
 1422       END IF ! end of test   IF (ITYPE.GE.6.AND.TIDEFLAG.GT.0)
 1423                                                               
 1424 !
 1425 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 1426 ! 8   Begin loop over input fields
 1427 !
 1428 !     Read scale factor and offset for input fields
 1429         XCFAC = 1.0
 1430         YCFAC = 1.0
 
 
                                                         ww3_prnc.F90  page  27
 
 
 1431         XCOFF = 0.0
 1432         YCOFF = 0.0
 1433 !
 1434         IF ( ITYPE .LE. 4 .OR. ITYPE.EQ.6 ) THEN
 1435           IRET = NF90_GET_ATT(NCID,VARIDF(1),'scale_factor',XCFAC)
 1436           IF (IRET.NE.0 ) XCFAC = 1.0
 1437           IRET = NF90_GET_ATT(NCID,VARIDF(1),'add_offset',XCOFF)
 1438           IF (IRET.NE.0 ) XCOFF = 0.0
 1439           IF ( NFCOMP.EQ.2 .OR. IFLD.GE.3 .OR. FLBERG ) THEN
 1440             IRET = NF90_GET_ATT(NCID,VARIDF(2),'scale_factor',YCFAC)
 1441             IF (IRET.NE.0 ) YCFAC = 1.0
 1442             IRET = NF90_GET_ATT(NCID,VARIDF(2),'add_offset',YCOFF)
 1443           IF (IRET.NE.0 ) YCOFF = 0.0
 1444           END IF
 1445         END IF
 1446 !
 1447         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,972)
 1448         TIMEDELAY = 0
 1449         DO ITIME=1,NTI
 1450 !
 1451 ! 8.a Read new time and fields
 1452 !
 1453           IRET=NF90_INQ_VARID(NCID,"time",VARIDTMP)
 1454           IF ( IRET/=NF90_NOERR ) IRET=NF90_INQ_VARID(NCID,"MT",VARIDTMP)
 1455           CALL CHECK_ERR(IRET)
 1456           IRET=NF90_GET_VAR(NCID,VARIDTMP,CURJULDAY,start=(/ITIME/))
 1457           call CHECK_ERR(IRET)
 1458           IF (INDEX(TIMEUNITS, "seconds").NE.0)   CURJULDAY=CURJULDAY/864>
 1459           IF (INDEX(TIMEUNITS, "minutes").NE.0)   CURJULDAY=CURJULDAY/144>
 1460           IF (INDEX(TIMEUNITS, "hours").NE.0)     CURJULDAY=CURJULDAY/24.
 1461           CURJULDAY=REFJULDAY+CURJULDAY
 1462                                        
 1463           ! cycle until reaching the start time
 1464           IF (STARTJULDAY.GT.CURJULDAY) CYCLE
 1465                                              
 1466           ! exit when reaching the stop time
 1467           IF (STPJULDAY.LT.CURJULDAY) EXIT
 1468                                           
 1469           ! convert julday to date and time
 1470           CALL J2D(CURJULDAY,CURDATE,IERR)
 1471           CALL D2T(CURDATE,TIME,IERR)
 1472           CALL STME21 (TIME,IDTIME)
 1473                                    
 1474           ! define time delay
 1475           IF (.NOT.FLTIME.AND.TIMEDELAY.EQ.0) THEN
 1476             TIMEDELAY = DSEC21 (TIME,TIMESHIFT)
 1477           END IF
 1478                 
 1479           ! shift time
 1480           IF (TIMEDELAY.NE.0) THEN
 1481             CALL TICK21 (TIME,TIMEDELAY)
 1482             CALL STME21 (TIME,IDTIME2)
 1483             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1973) IDTIME2, IDTIME
 1484           ELSE
 1485             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2973) IDTIME
 
 
                                                         ww3_prnc.F90  page  28
 
 
 1486           END IF
 1487         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,974)
 1488 !
 1489 ! ... Input
 1490 !
 1491           IF ( ITYPE .LE. 4 .OR. ITYPE.EQ.6) THEN
 1492             IF (NDIMSGRID.EQ.1) THEN
 1493               IRET=NF90_GET_VAR(NCID,VARIDF(1),XC(:,1),start=(/1,ITIME/),>
 1494             ELSE
 1495               IF (NDIMSVAR.EQ.3) THEN
 1496                 IRET=NF90_GET_VAR(NCID,VARIDF(1),XC,start=(/1,1,ITIME/),c>
 1497               ELSE
 1498                 IRET=NF90_GET_VAR(NCID,VARIDF(1),XC,start=(/1,1,1,ITIME/)>
 1499               END IF
 1500             END IF
 1501             CALL CHECK_ERR(IRET)
 1502             ! forces to 0 values that are undefined
 1503             WHERE(XC.NE.XC) XC = FILLVALUE
 1504             WHERE (XC.NE.FILLVALUE) XC=XC*XCFAC+XCOFF
 1505                                                      
 1506 !
 1507             IF (NFCOMP.EQ.2 .OR. IFLD.GE.3 .OR. FLBERG) THEN
 1508                                                             
 1509 ! This is a quick fix that works if the lon,lat,level,time dimensions are>
 1510 ! otherwise, one should check the length of each dimension ...
 1511                     IF (NDIMSGRID.EQ.1) THEN
 1512                       IRET=NF90_GET_VAR(NCID,VARIDF(2),YC(:,1),start=(/1,>
 1513                     ELSE
 1514                       IF (NDIMSVAR.EQ.3) THEN
 1515                         IRET=NF90_GET_VAR(NCID,VARIDF(2),YC,start=(/1,1,I>
 1516                       ELSE
 1517                         IRET=NF90_GET_VAR(NCID,VARIDF(2),YC,start=(/1,1,1>
 1518                         END IF
 1519                       END IF
 1520 ! The following line forces to 0 values that are undefine
 1521                     CALL CHECK_ERR(IRET)
 1522                     WHERE(YC.NE.YC) YC = FILLVALUE
 1523                     WHERE (YC.NE.FILLVALUE) YC=YC*YCFAC+YCOFF
 1524 !
 1525                 IF (FLSTAB) THEN
 1526 ! This is a quick fix that works if the lon,lat,level,time dimensions are>
 1527 ! otherwise, one should check the length of each dimension ...
 1528                     IF (NDIMSGRID.EQ.1) THEN
 1529                       IRET=NF90_GET_VAR(NCID,VARIDF(3),AC(:,1),start=(/1,>
 1530                     ELSE
 1531                       IF (NDIMSVAR.EQ.3) THEN
 1532                         IRET=NF90_GET_VAR(NCID,VARIDF(3),AC,start=(/1,1,I>
 1533                       ELSE
 1534                         IRET=NF90_GET_VAR(NCID,VARIDF(3),AC,start=(/1,1,1>
 1535                         END IF
 1536                       END IF
 1537                     CALL CHECK_ERR(IRET)
 1538                     !AC(:,:)=AC(:,MYM:1:-1)
 1539 !
 1540                   END IF
 
 
                                                         ww3_prnc.F90  page  29
 
 
 1541 !
 1542               END IF
 1543                     
 1544           ELSE  ! ITYPE .NE. 5
 1545 !
 1546             IF ( IAPROC .EQ. NAPOUT ) WRITE(NDSO,*) "ITYPE5 TO DO"
 1547             IF (IDFMF(1).EQ.3) THEN
 1548                 READ (NDSF(1),  END=862,ERR=862,IOSTAT=IERR) NDAT
 1549               ELSE
 1550                 READ (NDSF(1),*,END=862,ERR=862,IOSTAT=IERR) NDAT
 1551               END IF
 1552            IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,975) NDAT
 1553             IF ( NDAT.GT.0 ) THEN
 1554                 ALLOCATE ( DATA(RECLDT,NDAT) )
 1555                 DO IDAT=1, NDAT
 1556                   IF (IDFMF(1).EQ.1) THEN
 1557                       READ (NDSF(1),    *   ,END=863,ERR=863,         &
 1558                             IOSTAT=IERR) DATA(:,IDAT)
 1559                     ELSE IF (IDFMF(1).EQ.2) THEN
 1560                       READ (NDSF(1),FORMT(1),END=863,ERR=863,         &
 1561                             IOSTAT=IERR) DATA(:,IDAT)
 1562                     ELSE
 1563                       READ (NDSF(1),         END=863,ERR=863,         &
 1564                             IOSTAT=IERR) DATA(:,IDAT)
 1565                     END IF
 1566                   END DO
 1567               END IF
 1568 !
 1569           END IF
 1570 !
 1571 ! 8.b Interpolate fields
 1572 ! ... No Interpolation, type AI (should not use array syntax !!!)
 1573 !
 1574         IF (ITYPE.EQ.1.OR.ITYPE.EQ.6) THEN
 1575 !
 1576             ! change fillvalue
 1577             DO IY=1,NY
 1578               DO IX=1,NX
 1579                 IF (XC(IX,IY) .EQ. FILLVALUE) XC(IX,IY)=0
 1580                 IF (YC(IX,IY) .EQ. FILLVALUE) YC(IX,IY)=0
 1581               END DO
 1582             END DO
 1583                   
 1584             IF (( IFLD.LE.2 ).AND.( .NOT. FLBERG )) THEN
 1585                 DO IY=1, NY
 1586                   DO IX=1, NX
 1587                     FA(IX,IY) = XC(IX,IY)
 1588                     END DO
 1589                   END DO
 1590               ELSE
 1591                 DO IY=1, NY
 1592                   DO IX=1, NX
 1593                     FX(IX,IY) = XC(IX,IY)
 1594                     FY(IX,IY) = YC(IX,IY)
 1595                     FA(IX,IY) = AC(IX,IY)
 
 
                                                         ww3_prnc.F90  page  30
 
 
 1596                     END DO
 1597                   END DO
 1598               END IF
 1599 !
 1600           ELSE IF (ITYPE.NE.5) THEN
 1601 !
 1602 ! ... One-component fields
 1603 !
 1604            IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,976) ' '
 1605             IF (( IFLD.LE.2 ).AND.( .NOT. FLBERG )) THEN
 1606 !
 1607                 CALL INTERP(MXM, MYM, XC, IX21, IX22, IY21, IY22,      &
 1608                                RD11, RD12, RD21, RD22, FILLVALUE, FA)
 1609 !
 1610                 IF (NFCOMP.EQ.2) THEN
 1611              IF ( IAPROC .EQ. NAPOUT )    WRITE (NDSO,976) ' (2) '
 1612                     CALL INTERP(YC, JX21, JX22, JY21, JY22, XD11, XD12,&
 1613                                              XD21, XD22, FILLVALUE, FA)
 1614                 END IF
 1615 !
 1616 ! ... Two-component fields
 1617 !
 1618               ELSE  !so if IFLD.GT.2
 1619 !
 1620                 CALL INTERP(MXM, MYM, XC, IX21, IX22, IY21, IY22,      &
 1621                                RD11, RD12, RD21, RD22, FILLVALUE, FX)
 1622                                                                      
 1623                 CALL INTERP(MXM, MYM, YC, IX21, IX22, IY21, IY22,      &
 1624                                RD11, RD12, RD21, RD22, FILLVALUE, FY)
 1625                                                                      
 1626                 CALL INTERP(MXM, MYM, AC, IX21, IX22, IY21, IY22,      &
 1627                                RD11, RD12, RD21, RD22, FILLVALUE, FA)
 1628                                                                      
 1629                 CALL INTERP(MXM, MYM, SQRT(XC**2+YC**2), IX21, IX22,   &
 1630                                IY21, IY22, RD11, RD12, RD21, RD22,     &
 1631                                SQRT(2.0)*FILLVALUE, A2)
 1632                                                        
 1633                 CALL INTERP(MXM, MYM, XC**2+YC**2, IX21, IX22,         &
 1634                                IY21, IY22, RD11, RD12, RD21, RD22,     &
 1635                                2.0*FILLVALUE*FILLVALUE, A3)
 1636                                                            
 1637                 DO IY=1,NY
 1638                   DO IX=1,NX
 1639                     A1(IX,IY) = MAX ( 1.E-10 ,                        &
 1640                                   SQRT( FX(IX,IY)**2 + FY(IX,IY)**2 ) )
 1641                                                                        
 1642                     A3(IX,IY) = SQRT( A3(IX,IY) )
 1643                   END DO
 1644                 END DO
 1645 !
 1646 ! ... Winds, correct for velocity or energy conservation
 1647 !
 1648                 IF (IFLD.EQ.3) THEN
 1649                     DO IY=1,NY
 1650                       DO IX=1,NX
 
 
                                                         ww3_prnc.F90  page  31
 
 
 1651                         FACTOR = MIN ( 1.5 , A3(IX,IY)/A1(IX,IY) )
 1652                         FX(IX,IY) = FACTOR * FX(IX,IY)
 1653                         FY(IX,IY) = FACTOR * FY(IX,IY)
 1654                         END DO
 1655                       END DO
 1656                   END IF
 1657 !
 1658 ! ... Currents, correct for velocity or energy conservation
 1659 !
 1660                 IF (IFLD.EQ.4) THEN
 1661                     DO IY=1,NY
 1662                       DO IX=1,NX
 1663                         FACTOR = MIN ( 1.5 , A2(IX,IY)/A1(IX,IY) )
 1664                         FX(IX,IY) = FACTOR * FX(IX,IY)
 1665                         FY(IX,IY) = FACTOR * FY(IX,IY)
 1666                         END DO
 1667                       END DO
 1668                   END IF
 1669 !
 1670               END IF
 1671 !
 1672           END IF
 1673 !
 1674 ! ... Test output
 1675 !
 1676 ! 8.c Write fields
 1677 !
 1678         IF ( ITYPE .LE. 4 .OR. ITYPE.EQ.6 ) THEN
 1679       IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,977)
 1680           IF ( IAPROC .EQ. NAPOUT ) CALL W3FLDG ('WRITE', IDFLD, NDSDAT, >
 1681                        NX, NY, TIME, TIME, TIME, FX, FY, FA, TIME,  &
 1682                        FX, FY, FA, IERR)
 1683                                         
 1684         ELSE IF ( ITYPE .EQ. 5 ) THEN
 1685           IF ( NDAT .EQ. 0 ) THEN
 1686         IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,978)
 1687           ELSE
 1688         IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,977)
 1689             IF ( IAPROC .EQ. NAPOUT ) CALL W3FLDD ('WRITE', IDFLD, NDSDAT>
 1690                          TIME, RECLDT, NDAT, IDAT, DATA, IERR )
 1691             DEALLOCATE ( DATA )
 1692           END IF
 1693         END IF
 1694         IF (IERR.NE.0) CALL EXTCDE ( 30 )
 1695 !
 1696       END DO ! NTI
 1697 !
 1698       DEALLOCATE(XC,YC,AC)
 1699       IF (ASSOCIATED(ALA)) DEALLOCATE(ALA,ALO)
 1700 !
 1701 !     End loop over input fields
 1702 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 1703 !
 1704   880 CONTINUE
 1705       GOTO 888
 
 
                                                         ww3_prnc.F90  page  32
 
 
 1706 !
 1707 ! Error escape locations
 1708 !
 1709   800 CONTINUE
 1710       WRITE (NDSE,1000) IERR
 1711       CALL EXTCDE ( 40 )
 1712 !
 1713   801 CONTINUE
 1714       WRITE (NDSE,1001)
 1715       CALL EXTCDE ( 41 )
 1716 !
 1717   802 CONTINUE
 1718       WRITE (NDSE,1002) IERR
 1719       CALL EXTCDE ( 42 )
 1720 !
 1721   803 CONTINUE
 1722       WRITE (NDSE,1003) IERR
 1723       CALL EXTCDE ( 43 )
 1724 !
 1725   810 CONTINUE
 1726       WRITE (NDSE,1010)
 1727       CALL EXTCDE ( 1010 )
 1728 !
 1729   811 CONTINUE
 1730       WRITE (NDSE,1011)
 1731       CALL EXTCDE ( 1011 )
 1732 !
 1733   845 CONTINUE
 1734       WRITE (NDSE,1045) IERR
 1735       CALL EXTCDE ( 49 )
 1736 !
 1737   846 CONTINUE
 1738       WRITE (NDSE,1046) IERR
 1739       CALL EXTCDE ( 50 )
 1740 !
 1741   862 CONTINUE
 1742       WRITE (NDSE,1062) IERR
 1743       CALL EXTCDE ( 54 )
 1744 !
 1745   863 CONTINUE
 1746       WRITE (NDSE,1063) IDAT, IERR
 1747       CALL EXTCDE ( 55 )
 1748   864 CONTINUE
 1749       WRITE (NDSE,1064)  TRIM(STRDIMSNAME)
 1750       CALL EXTCDE ( 56 )
 1751 !
 1752   888 CONTINUE
 1753       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,999)
 1754                                                 
 1755 !
 1756  
 1757  
 1758 !
 1759 ! Formats
 1760 !
 
 
                                                         ww3_prnc.F90  page  33
 
 
 1761   900 FORMAT (/15X,'  *** WAVEWATCH III  Input pre-processing ***  '/ &
 1762                15X,'==============================================='/)
 1763   901 FORMAT ( '  Comment character is ''',A,''''/)
 1764   902 FORMAT ( '  Grid name : ',A/)
 1765 !
 1766   930 FORMAT (/'  Description of inputs'/                             &
 1767                ' --------------------------------------------------'/ &
 1768                '       Input type        : ',A/                       &
 1769                '       Format type       : ',A)
 1770  1930 FORMAT ( '          Field conserves velocity.')
 1771  2930 FORMAT ( '          Field corrected for energy conservation.')
 1772  1931 FORMAT ( '       Start time        : ',A)
 1773  2931 FORMAT ( '       Stop time         : ',A)
 1774  3931 FORMAT ( '       Shifted time      : ',A)
 1775   932 FORMAT (/'       Input grid dim.   :',I9,3X,I5)
 1776  1933 FORMAT ( '       Longitude range   :',2F8.2,' (deg)'/           &
 1777                '       Latitude range    :',2F8.2,' (deg)')
 1778  2933 FORMAT ( '       X range           :',2F8.2,' (km)'/           &
 1779                '       Y range           :',2F8.2,' (km)')
 1780   934 FORMAT (/'       Data type         : ',A/                       &
 1781                '       Data record length:',I5/                       &
 1782                '       Missing values    :',F8.2)
 1783   935 FORMAT ( 'DT',I1 )
 1784   938 FORMAT ( '          Icebergs included.')
 1785   939 FORMAT ( '          Air-sea temperature differences included.')
 1786 !
 1787   940 FORMAT (//'  Preprocessing data'/                               &
 1788                ' --------------------------------------------------')
 1789   941 FORMAT ( '       Interpolation factors ..... '/                 &
 1790                '          (longitude-latitude grid)')
 1791   942 FORMAT ( '       Interpolation factors ..... '/                 &
 1792                '          (grid from file)')
 1793   943 FORMAT (/'           Longitude-latitude file ',I1,' :'/         &
 1794                '          ---------------------------------------')
 1795   944 FORMAT ( '           Input grid dim.   :',I9,3X,I5/             &
 1796                '           Closed longitudes :',L5)
 1797   945 FORMAT ( '           Layout indicator  :',I5/                   &
 1798                '           Format indicator  :',I5)
 1799   946 FORMAT ( '           Format            : ',A)
 1800   947 FORMAT ( '           Unit number       :',I5)
 1801   948 FORMAT ( '           File name         : ',A)
 1802   949 FORMAT (/'           Corresponding map file '/                  &
 1803                '          ---------------------------------------')
 1804 !
 1805   960 FORMAT (/'           Data file :'/                              &
 1806                '          ---------------------------------------')
 1807   961 FORMAT (/'           Data file :'/                              &
 1808                '          ---------------------------------------'/   &
 1809                '           Input grid dim.   :',I9,3X,I5)
 1810   962 FORMAT (/'           Data file (',I1,') :'/                     &
 1811                '          ---------------------------------------'/   &
 1812                '           Input grid dim.   :',I9,3X,I5)
 1813   967 FORMAT (/'           File name         : ',A)
 1814   968 FORMAT ( '           Dimension along x : ',A/                   &
 1815                '           Dimension along y : ',A)
 
 
                                                         ww3_prnc.F90  page  34
 
 
 1816   969 FORMAT ( '           Field component ',I1,' : ',A)
 1817 !
 1818   971 FORMAT (/'       Opening output data file .....')
 1819   972 FORMAT (//'  Processing data'/                                  &
 1820                ' --------------------------------------------------')
 1821  1973 FORMAT ( '   Shifted Time : ',A,'    (File time : ',A,')')
 1822  2973 FORMAT ( '           Time : ',A)
 1823                                       
 1824   974 FORMAT ( '                  reading ....')
 1825   975 FORMAT ( '                     number of data records :',I6)
 1826   976 FORMAT ( '                  interpolating',A,'....')
 1827   977 FORMAT ( '                  writing ....')
 1828   978 FORMAT ( '                  skipping ....')
 1829 !
 1830   999 FORMAT(//'  End of program '/                                   &
 1831                ' ========================================='/          &
 1832                '         WAVEWATCH III Input preprocessing '/)
 1833 !
 1834  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1835                '     ERROR IN OPENING INPUT FILE'/                    &
 1836                '     IOSTAT =',I5/)
 1837 !
 1838  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1839                '     PREMATURE END OF INPUT FILE'/)
 1840 !
 1841  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1842                '     ERROR IN READING FROM INPUT FILE'/               &
 1843                '     IOSTAT =',I5/)
 1844 !
 1845  1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1846                '     ERROR IN READING FROM INPUT FILE'/               &
 1847                '     EXPECTING LIST OF TIDAL CONST. OR FAST OR VFAST'/&
 1848                '     IOSTAT =',I5/)
 1849 !
 1850  1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1851                '     NO FIELD SELECTED'/)
 1852  1011 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1853                '     NO GRID SELECTED'/)
 1854 !
 1855  1027 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1856                '     _FillValue ATTRIBUTE NOT DEFINED FOR : ',A/)
 1857 !
 1858  1028 FORMAT (/' *** WAVEWATCH III WARNING IN W3PRNC : '/             &
 1859                '     calendar ATTRIBUTE NOT DEFINED'/                 &
 1860                '     IT MUST RESPECT STANDARD OR GREGORIAN CALENDAR')
 1861  1029 FORMAT (/' *** WAVEWATCH III WARNING IN W3PRNC : '/             &
 1862                '     CALENDAR ATTRIBUTE NOT MATCH'/                   &
 1863                '     IT MUST RESPECT STANDARD OR GREGORIAN CALENDAR')
 1864  1030 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1865                '     ILLEGAL FIELD ID -->',A,'<--'/)
 1866  1031 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1867                '     ILLEGAL FORMAT ID -->',A,'<--'/)
 1868  1032 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1869                '     LATITUDE VALUES MUST BE REVERSED'/               &
 1870                '     EXAMPLE: ncpdq -h -O -a -lat file.nc'/ )
 
 
                                                         ww3_prnc.F90  page  35
 
 
 1871 !
 1872  1033 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1873                '     ILLEGAL DATA RECORD LENGTH : ',I6/)
 1874  1034 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1875                '     ILLEGAL DATA TYPE : ',I2/)
 1876 !
 1877  1035 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1878                '     ILLEGAL TIME : ',I8.8,I7.6/)
 1879  1036 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1880                '     ILLEGAL SIZE OF INPUT GRID : ',I5,1X,I5/)
 1881  1038 FORMAT (/' *** WAVEWATCH III WARNING IN W3PRNC : '/             &
 1882                '     DATA READ FROM INPUT FILE')
 1883  1039 FORMAT (/' *** WAVEWATCH III WARNING IN W3PRNC : '/       &
 1884                '     NAN VALUES IN HARMONICS '/                 &
 1885                '     REMOVE NON-LINEAR TIDAL COMPONENTS '/      &
 1886                '     2MS2 2MN2 2NK2 MNS2 MSN2 2SM2 3MSN2 '      &
 1887                '     M4 MS4 MN4 M6 2MS6 2MN6'/)
 1888 !
 1889  1042 FORMAT (/' *** WAVEWATCH-III WARNING W3PRNC : '/                &
 1890                '     GRID POINT ',2I6,2F7.2,/                         &
 1891                ' NOT COVERED BY INPUT GRID.'/)
 1892  1043 FORMAT (/' *** WAVEWATCH III WARNING W3PRNC : '/                &
 1893                '     X = ',F10.1,' NOT COVERED BY INPUT GRID.'/)
 1894  1044 FORMAT (/' *** WAVEWATCH III WARNING W3PRNC : '/                &
 1895                '     Y = ',F10.1,' NOT COVERED BY INPUT GRID.'/)
 1896 !
 1897  
 1898 !
 1899  1045 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1900                '     ERROR IN OPENING LAT-LONG DATA FILE'/            &
 1901                '     IOSTAT =',I5/)
 1902 !
 1903  1046 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1904                '     ERROR IN OPENING MASK FILE'/                     &
 1905                '     IOSTAT =',I5/)
 1906 !
 1907  1062 FORMAT (/' *** WAVEWATCH III ERROR IN W3PREP : '/               &
 1908                '     ERROR IN READING NDAT FROM FILE'/                &
 1909                '     IOSTAT =',I5/)
 1910  1063 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1911                '     ERROR IN READING DATA RECORD',I6,' FROM FILE'/   &
 1912                '     IOSTAT =',I5/)
 1913  1064 FORMAT (/' *** WAVEWATCH III ERROR IN W3PRNC : '/               &
 1914                '     GRID DIMENSIONS ', A,' NOT FOUND... CHECK DIMENSION >
 1915 !
 1916 !/
 1917 !/ End of W3PRNC ----------------------------------------------------- /
 1918 !/
 1919   
 1920       END PROGRAM W3PRNC
 1921                         
 1922 !========================================================================>
 1923                                                                           
 1924       SUBROUTINE INTERP(MXM, MYM, XC, IX21, IX22, IY21, IY22,       &
 1925                                RD11, RD12, RD21, RD22, FILLVALUE, FA)
 
 
                                                         ww3_prnc.F90  page  36
 
 
 1926 !/
 1927 !/                  +-----------------------------------+
 1928 !/                  | WAVEWATCH III           NOAA/NCEP |
 1929 !/                  |           J. M. Castillo          |
 1930 !/                  |                        FORTRAN 90 |
 1931 !/                  | Last update :         23-Feb-2021 |
 1932 !/                  +-----------------------------------+
 1933 !/
 1934 !/    23-Feb-2021 : First version                       ( version 7.xx )
 1935 !/
 1936 !  1. Purpose :
 1937 !
 1938 !     Interpolate from a field read from file to the wave grid
 1939 !
 1940 !  2. Method :
 1941 !
 1942 !     Invalid points are identified by the fill value read from the
 1943 !     netcdf input, and interpolation does not take into account
 1944 !     these points. The valid interpolation coefficients are scaled
 1945 !     so that the sum is one, otherwise unphysical values can be
 1946 !     generated.
 1947 !
 1948 !     When one point is on the boundary but is not an ocean grid point,
 1949 !     the interpolation coefficients are zero, and in this case we
 1950 !     provide a sensible value - the value as read, not interpolated
 1951 !
 1952 !  3. Parameters :
 1953 !
 1954 !     Parameter list
 1955 !     ----------------------------------------------------------------
 1956 !       MxM     I      I   Dimensions of the XC variable
 1957 !       XC      R.A.   I   Field to be interpolated, as read from the
 1958 !                          input netcdf
 1959 !       IXxx    I.A.   I   List of x-index to convert from the original
 1960 !                          field to the model grid
 1961 !       IYxx    I.A.   I   List of y-index to convert from the original
 1962 !                          field to the model grid
 1963 !       RDxx    R.A.   I   Interpolation factors
 1964 !       FILLVALUE   R  I   Fill value identifying non valid input
 1965 !       FA      F      O   Result of the interpolation
 1966 !     ----------------------------------------------------------------
 1967 !
 1968 !  4. Subroutines used :
 1969 !
 1970 !     None
 1971 !
 1972 !  5. Called by :
 1973 !
 1974 !      Name      Type  Module   Description
 1975 !     ----------------------------------------------------------------
 1976 !      WW3_PRNC  Prog.   N/A    Input data preprocessor.
 1977 !     ----------------------------------------------------------------
 1978 !
 1979 !  6. Error messages :
 1980 !
 
 
                                                         ww3_prnc.F90  page  37
 
 
 1981 !     None
 1982 !
 1983 !  7. Remarks :
 1984 !
 1985 !  8. Structure :
 1986 !
 1987 !     See source code.
 1988 !
 1989 !  9. Switches :
 1990 !
 1991 ! 10. Source code :
 1992 !
 1993 !/ ------------------------------------------------------------------- /
 1994       USE W3GDATMD, ONLY: NX, NY
 1995                                 
 1996       IMPLICIT NONE
 1997 !/
 1998 !/ ------------------------------------------------------------------- /
 1999 !/ Parameter list
 2000 !/
 2001       INTEGER,                     INTENT(IN)  :: MXM, MYM
 2002       REAL,    DIMENSION(MXM,MYM), INTENT(IN)  :: XC
 2003       INTEGER, DIMENSION(NX,NY),   INTENT(IN)  :: IX21, IX22, IY21, IY22
 2004       REAL,    DIMENSION(NX,NY),   INTENT(IN)  :: RD11, RD12, RD21, RD22
 2005       REAL,                        INTENT(IN)  :: FILLVALUE
 2006       REAL,    DIMENSION(NX,NY),   INTENT(OUT) :: FA
 2007 !/
 2008 !/ ------------------------------------------------------------------- /
 2009 !/    Local variables
 2010 !/
 2011       INTEGER      :: IX, IY
 2012       REAL         :: FACTOR
 2013 !/ ------------------------------------------------------------------- /
 2014                                                                         
 2015       DO IY=1,NY
 2016         DO IX=1,NX
 2017           FACTOR = 0.0
 2018           FA(IX,IY) = 0.0
 2019                          
 2020           IF(XC(IX21(IX,IY),IY21(IX,IY)).NE.FILLVALUE) THEN
 2021             FACTOR = FACTOR + RD11(IX,IY)
 2022             FA(IX,IY) = RD11(IX,IY) * XC(IX21(IX,IY),IY21(IX,IY))
 2023           ENDIF
 2024           IF(XC(IX22(IX,IY),IY21(IX,IY)).NE.FILLVALUE) THEN
 2025             FACTOR = FACTOR + RD21(IX,IY)
 2026             FA(IX,IY) = FA(IX,IY) + RD21(IX,IY) * XC(IX22(IX,IY),IY21(IX,>
 2027           ENDIF
 2028           IF(XC(IX21(IX,IY),IY22(IX,IY)).NE.FILLVALUE) THEN
 2029             FACTOR = FACTOR + RD12(IX,IY)
 2030             FA(IX,IY) = FA(IX,IY) + RD12(IX,IY) * XC(IX21(IX,IY),IY22(IX,>
 2031           ENDIF
 2032           IF(XC(IX22(IX,IY),IY22(IX,IY)).NE.FILLVALUE) THEN
 2033             FACTOR = FACTOR + RD22(IX,IY)
 2034             FA(IX,IY) = FA(IX,IY) + RD22(IX,IY) * XC(IX22(IX,IY),IY22(IX,>
 2035           ENDIF
 
 
                                                         ww3_prnc.F90  page  38
 
 
 2036                
 2037           IF(FACTOR.GT.0.0) THEN
 2038             FA(IX,IY) = FA(IX,IY) / FACTOR
 2039           ELSE
 2040             IF(XC(IX,IY).EQ.FILLVALUE) THEN
 2041               FA(IX,IY) = 0.0
 2042             ELSE
 2043               FA(IX,IY) = XC(IX,IY)
 2044             ENDIF
 2045           END IF
 2046         END DO
 2047       END DO
 2048             
 2049       END SUBROUTINE INTERP
 2050                            
 2051 !========================================================================>
 2052                                                                           
 2053       SUBROUTINE CHECK_ERR(IRET)
 2054                                 
 2055       USE NETCDF
 2056       USE W3ODATMD, ONLY: NDSE
 2057       USE W3SERVMD, ONLY: EXTCDE
 2058                                 
 2059       IMPLICIT NONE
 2060                    
 2061       INTEGER IRET
 2062                   
 2063       IF (IRET .NE. NF90_NOERR) THEN
 2064         WRITE(NDSE,*) ' *** WAVEWATCH III ERROR IN PRNC :'
 2065         WRITE(NDSE,*) ' NETCDF ERROR MESSAGE: '
 2066         WRITE(NDSE,*) NF90_STRERROR(IRET)
 2067         CALL EXTCDE ( 59 )
 2068       END IF
 2069       RETURN
 2070             
 2071       END SUBROUTINE CHECK_ERR
 2072                               
 2073 !========================================================================>
 2074                                                                           
 2075                                                                           
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/opt/cray/netcdf-hdf5parallel/4.2.0/pgi/119/include -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
