                                                         w3iorsmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3IORSMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                      FORTRAN 2003 |
    9 !/                  | Last update :         09-Aug-2017 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    See subroutine for update log.
   13 !/
   14 !  1. Purpose :
   15 !
   16 !     Read/write restart files.
   17 !
   18 !  2. Variables and types :
   19 !
   20 !      Name      Type  Scope    Description
   21 !     ----------------------------------------------------------------
   22 !      VERINI    C*10  Private  Restart file version number.
   23 !      IDSTR     C*26  Private  Restart file UD string.
   24 !     ----------------------------------------------------------------
   25 !
   26 !  3. Subroutines and functions :
   27 !
   28 !      Name      Type  Scope    Description
   29 !     ----------------------------------------------------------------
   30 !      W3IORS    Subr. Public   Read/write restart files.
   31 !     ----------------------------------------------------------------
   32 !
   33 !  4. Subroutines and functions used :
   34 !
   35 !      Name      Type  Module   Description
   36 !     ----------------------------------------------------------------
   37 !      W3SETO, W3SETG, W3SETW, W3DIMW
   38 !                Subr. W3xDATMD Manage data structures.
   39 !      STRACE    Subr. W3SERVMD Subroutine tracing.            (!/S)
   40 !      EXTCDE    Subr. W3SERVMD Abort program with exit code.
   41 !      MPI_STARTALL, MPI_WAITALL                              (!/MPI)
   42 !                Subr.          MPI persistent communication routines
   43 !     ----------------------------------------------------------------
   44 !
   45 !  5. Remarks :
   46 !
   47 !  6. Switches :
   48 !
   49 !     See also routine.
   50 !
   51 !  7. Source code :
   52 !
   53 !/ ------------------------------------------------------------------- /
   54       PUBLIC
   55 !/
 
 
                                                         w3iorsmd.F90  page   2
 
 
   56       ! Add fields needed for OASIS coupling in restart
   57       LOGICAL :: OARST
   58 !/
   59 !/ Private parameter statements (ID strings)
   60 !/
   61       CHARACTER(LEN=10), PARAMETER, PRIVATE :: VERINI = '2020-09-25'
   62       CHARACTER(LEN=26), PARAMETER, PRIVATE ::                        &
   63                                IDSTR = 'WAVEWATCH III RESTART FILE'
   64 !/
   65       CONTAINS
   66 !/ ------------------------------------------------------------------- /
   67       SUBROUTINE W3IORS ( INXOUT, NDSR, DUMFPI, IMOD, FLRSTRT )
   68 !/
   69 !/                  +-----------------------------------+
   70 !/                  | WAVEWATCH III           NOAA/NCEP |
   71 !/                  |           H. L. Tolman            |
   72 !/                  |                        FORTRAN 90 |
   73 !/                  | Last update :         25-Sep-2020 |
   74 !/                  +-----------------------------------+
   75 !/
   76 !/    12-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
   77 !/    27-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
   78 !/    30-Apr-2002 : Add ice for transparencies.         ( version 2.20 )
   79 !/    13-Nov-2002 : Add stress as vector.               ( version 3.00 )
   80 !/    19-Aug-2003 : Output server options added.        ( version 3.04 )
   81 !/    09-Dec-2004 : Multiple grid version.              ( version 3.06 )
   82 !/    24-Jun-2005 : Adding MAPST2.                      ( version 3.07 )
   83 !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   84 !/    05-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
   85 !/    08-May-2007 : Starting from calm as an option.    ( version 3.11 )
   86 !/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
   87 !/    22-Jun-2007 : Dedicated output processes.         ( version 3.11 )
   88 !/    15-Apr-2008 : Clean up for distribution.          ( version 3.14 )
   89 !/    21-Apr-2008 : Remove PGI bug internal files.      ( version 3.14 )
   90 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   91 !/    30-Oct-2009 : Output file name with 3 digit id.   ( version 3.14 )
   92 !/                  (W. E. Rogers, NRL)
   93 !/    14-Nov-2013 : Remove cold start init. UST(DIR).   ( version 4.13 )
   94 !/    31-May-2016 : Optimize restart file size for un-  ( version 5.10 )
   95 !/                  structured grid and restart read.
   96 !/                  (M. Ward, NCI, S. Zieger, BOM)
   97 !/    10-Mar-2017 : File access mode changed to 'STREAM'( version 6.02 )
   98 !/                  (S. Zieger, BOM)
   99 !/    09-Aug-2017 : Bug fix for MPI restart read issue  ( version 6.02 )
  100 !/                  (T. Campbell, NRL)
  101 !/    05-Jun-2018 : Add PDLIB/TIMINGS/DEBUGIO           ( version 6.04 )
  102 !/                  DEBUGINIT/MPI
  103 !/    19-Dec-2019 : Optional second stream of           ( version 7.00 )
  104 !/                  restart files
  105 !/                  (Roberto Padilla-Hernandez & J.H. Alves)
  106 !/    25-Sep-2020 : Extra fields for coupled restart    ( version 7.10 )
  107                                                                         
  108 !/
  109 !/    Copyright 2009-2013 National Weather Service (NWS),
  110 !/       National Oceanic and Atmospheric Administration.  All rights
 
 
                                                         w3iorsmd.F90  page   3
 
 
  111 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  112 !/       No unauthorized use without permission.
  113 !/
  114 !  1. Purpose :
  115 !
  116 !     Reads/writes restart files.
  117 !
  118 !  2. Method :
  119 !
  120 !     The file is opened within the routine, the name is pre-defined
  121 !     and the unit number is given in the parameter list. The restart
  122 !     file is written using UNFORMATTED write statements. The routine
  123 !     generates new names when called more than once. File names are :
  124 !
  125 !                                 restart000.FILEXT
  126 !                                 restart001.FILEXT
  127 !                                 restart002.FILEXT etc.
  128 !
  129 !     Optionally, a second stream of restart files is generated given
  130 !     a secondary stride definad by an additional start/end time line
  131 !     triggered by an optional argument added to the end of the stan-
  132 !     dard restart request line (a sixth argument flag set to T). File
  133 !     names include a time-tag prefix:
  134 !
  135 !                                 YYYYMMDD.HHMMSS.restart.FILEXT
  136 !
  137 !     The file to be read thus always is unnumbered, whereas all
  138 !     written files are automatically numbered.
  139 !
  140 !  3. Parameters :
  141 !
  142 !     Parameter list
  143 !     ----------------------------------------------------------------
  144 !       INXOUT  C*(*)  I   Test string for read/write, valid are:
  145 !                          'READ' Reading of a restart file.
  146 !                          'HOT'  Writing a full restart from the model.
  147 !                          'COLD' Writing a cold start file.
  148 !                          'WIND' Initialize fields using first wind
  149 !                                 field.
  150 !                          'CALM' Starting from calm conditions.
  151 !       NDSR    Int.  I/O  File unit number.
  152 !       DUMFPI  Real   I   Dummy values for FPIS for cold start.
  153 !       RSTYPE  Int.   O   Type of input field,
  154 !                           0 : cold start,
  155 !                           1 : cold start with fetch-limited spectra,
  156 !                           2 : full restart,
  157 !                           3 : for writing file.
  158 !                           4 : starting from calm.
  159 !       IMOD    Int.   I   Optional grid number, defaults to 1.
  160 !       FLRSTRT LOGIC  I    OTIONAL TRUE: A second request for restart fi>
  161 !     ----------------------------------------------------------------
  162 !
  163 !  4. Subroutines used :
  164 !
  165 !     See module documentation.
 
 
                                                         w3iorsmd.F90  page   4
 
 
  166 !
  167 !  5. Called by :
  168 !
  169 !      Name      Type  Module   Description
  170 !     ----------------------------------------------------------------
  171 !      W3INIT    Subr. W3INITMD Wave model initialization routine.
  172 !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
  173 !      WW3_STRT  Prog.   N/A    Initial conditions program.
  174 !     ----------------------------------------------------------------
  175 !
  176 !  6. Error messages :
  177 !
  178 !       Tests on INXOUT, file status and on array dimensions.
  179 !
  180 !  7. Remarks :
  181 !
  182 !     - MAPSTA is dumped as it contains information on inactive points.
  183 !       Note that the original MAPSTA is dumped in the model def. file
  184 !       for use in the initial conditions (and output) programs.
  185 !     - Note that MAPSTA and MAPST2 data is combinded in the file.
  186 !     - The depth is recalculated in a write to avoid floating point
  187 !       errors in W3STRT.
  188 !     - Fields and field info read by all, written by las processor
  189 !       only.
  190 !     - The MPP version of the model will perform a gather here to
  191 !       maximize hiding of communication with IO.
  192 !
  193 !  8. Structure :
  194 !
  195 !     +---------------------------------------------------------------+
  196 !     | initialisations                                               |
  197 !     | test INXOUT                                                   |
  198 !     | open file                                                     |
  199 !     +---------------------------------------------------------------|
  200 !     |                             WRITE ?                           |
  201 !     | Y                                                           N |
  202 !     |-------------------------------|-------------------------------|
  203 !     | Write identifiers and         | Write identifiers and         |
  204 !     |   dimensions.                 |   dimensions.                 |
  205 !     |                               | Check ident. and dimensions.  |
  206 !     +-------------------------------+-------------------------------|
  207 !     |                       Full restart ?                          |
  208 !     | Y                                                           N |
  209 !     |-------------------------------|-------------------------------|
  210 !     | read/write/test time          |                               |
  211 !     +-------------------------------+-------------------------------|
  212 !     |                             WRITE ?                           |
  213 !     | Y                                                           N |
  214 !     |-------------------------------|-------------------------------|
  215 !     |          TYPE = 'WIND' ?      |          TYPE = 'WIND' ?      |
  216 !     | Y                           N | Y                           N |
  217 !     |---------------|---------------|---------------|---------------|
  218 !     | close file    | write spectra | gen. fetch-l. | read spectra  |
  219 !     | RETURN        |               |   spectra.    |               |
  220 !     |---------------+---------------+---------------+---------------|
 
 
                                                         w3iorsmd.F90  page   5
 
 
  221 !     |                             WRITE ?                           |
  222 !     | Y                                                           N |
  223 !     |-------------------------------|-------------------------------|
  224 !     |          TYPE = 'FULL' ?      |          TYPE = 'FULL' ?      |
  225 !     | Y                           N | Y                           N |
  226 !     |---------------|---------------|---------------|---------------|
  227 !     | write level & | ( prep. level | read level &  | initalize l.& |
  228 !     |   (ice) map & |   for test    |   (ice) map.& |   times       |
  229 !     |   times       |   output )    |   times       | ( no ice )    |
  230 !     +---------------+---------------+---------------+-------------- +
  231 !
  232 !  9. Switches :
  233 !
  234 !     !/SEED  Linear input / seeding option.
  235 !     !/LNx
  236 !
  237 !     !/SHRD  Switch for shared / distributed memory architecture.
  238 !     !/DIST  Id.
  239 !     !/MPI   Id.
  240 !
  241 !     !/S     Enable subroutine tracing.
  242 !     !/T     Enable test output
  243 !
  244 ! 10. Source code :
  245 !
  246 !/ ------------------------------------------------------------------- /
  247       USE W3GDATMD, ONLY: W3SETG, W3SETREF, RSTYPE
  248       USE W3ODATMD, ONLY: W3SETO
  249       USE W3ADATMD, ONLY: W3SETA, W3XETA, NSEALM
  250       USE W3ADATMD, ONLY: CX, CY, HS, WLM, T0M1, FP0, THM, CHARN,     &
  251                           TAUWIX, TAUWIY, TWS, TAUOX, TAUOY, BHD,     &
  252                           PHIOC, TUSX, TUSY, USSX, USSY, TAUICE,      &
  253                           UBA, UBD, PHIBBL, TAUBBL
  254 !/
  255       USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, NSPEC, MAPSTA, MAPST2, &
  256                           GNAME, FILEXT, GTYPE, UNGTYPE
  257       USE W3TRIAMD, ONLY: SETUGIOBP
  258       USE W3WDATMD
  259       USE W3ODATMD, ONLY: NDSE, NDST, IAPROC, NAPROC, NAPERR, NAPRST, &
  260                           IFILE => IFILE4, FNMPRE, NTPROC, IOSTYP,    &
  261                           FLOGRR
  262       USE W3ODATMD, ONLY: NRQRS, NBLKRS, RSBLKS, IRQRS, IRQRSS,  &
  263                                VAAUX
  264       USE W3ADATMD, ONLY: MPI_COMM_WCMP
  265 !/
  266       USE W3SERVMD, ONLY: EXTCDE
  267       USE CONSTANTS, only: LPDLIB
  268       USE W3PARALL, ONLY: INIT_GET_ISEA, INIT_GET_JSEA_ISPROC
  269       USE W3GDATMD, ONLY: NK, NTH
  270 !!!!!/PDLIB    USE PDLIB_FIELD_VEC!, only : UNST_PDLIB_READ_FROM_FILE, UN>
  271 !
  272       IMPLICIT NONE
  273 !
  274       INCLUDE "mpif.h"
  275 !/
 
 
                                                         w3iorsmd.F90  page   6
 
 
  276 !/ ------------------------------------------------------------------- /
  277 !/ Parameter list
  278 !/
  279       INTEGER                       :: NDSR
  280 !      INTEGER, INTENT(IN)           :: NDSR
  281       INTEGER, INTENT(IN), OPTIONAL :: IMOD
  282       REAL, INTENT(INOUT)           :: DUMFPI
  283       CHARACTER, INTENT(IN)         :: INXOUT*(*)
  284       LOGICAL, INTENT(IN),OPTIONAL  :: FLRSTRT
  285 !/
  286 !/ ------------------------------------------------------------------- /
  287 !/ Local parameters
  288 !/
  289       INTEGER, PARAMETER      :: LRB = 4
  290 !
  291       INTEGER                 :: IGRD, I, J, LRECL, NSIZE, IERR,      &
  292                                  NSEAT, MSPEC, TTIME(2), ISEA, JSEA,  &
  293                                  NREC, NPART, IPART, IX, IY, IXL, IP, &
  294                                  NPRTX2, NPRTY2, IYL
  295       INTEGER, ALLOCATABLE    :: MAPTMP(:,:)
  296       INTEGER                 :: IERR_MPI, IH, IB, ISEA0, ISEAN, &
  297                                  NRQ, NSEAL_MIN
  298       INTEGER(KIND=8)         :: RPOS
  299       INTEGER, ALLOCATABLE    :: STAT1(:,:), STAT2(:,:)
  300       REAL, ALLOCATABLE       :: VGBUFF(:), VLBUFF(:)
  301       REAL(KIND=LRB), ALLOCATABLE :: WRITEBUFF(:), TMP(:), TMP2(:)
  302                                                                   
  303       LOGICAL                 :: WRITE, IOSFLG
  304       CHARACTER(LEN=4)        :: TYPE
  305       CHARACTER(LEN=10)       :: VERTST
  306 !      CHARACTER(LEN=21)       :: FNAME
  307       CHARACTER(LEN=40)       :: FNAME
  308       CHARACTER(LEN=26)       :: IDTST
  309       CHARACTER(LEN=30)       :: TNAME
  310       CHARACTER(LEN=15)       :: TIMETAG
  311 !/
  312 !/ ------------------------------------------------------------------- /
  313 !/
  314 !
  315 ! Constant NDSR for using mpiifort in ZEUS ... paralell runs crashing
  316 !  because compiler doesn't accept reciclyng of UNIT for FORMATTED or
  317 !  UNFORMATTED files in OPEN
  318 !
  319 !     NDSR = 525
  320                 
  321       IOSFLG = IOSTYP .GT. 0
  322 !
  323 ! test parameter list input ------------------------------------------ *
  324 !
  325       IF ( PRESENT(IMOD) ) THEN
  326           IGRD   = IMOD
  327         ELSE
  328           IGRD   = 1
  329         END IF
  330 !
 
 
                                                         w3iorsmd.F90  page   7
 
 
  331       CALL W3SETO ( IGRD, NDSE, NDST )
  332       CALL W3SETG ( IGRD, NDSE, NDST )
  333       CALL W3SETW ( IGRD, NDSE, NDST )
  334 !
  335       IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'HOT'  .AND.               &
  336           INXOUT.NE.'COLD' .AND. INXOUT.NE.'WIND' .AND.               &
  337           INXOUT.NE.'CALM' ) THEN
  338           IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,900) INXOUT
  339           CALL EXTCDE ( 1 )
  340         END IF
  341 !
  342       WRITE = INXOUT .NE. 'READ'
  343       IF ( INXOUT .EQ. 'HOT' ) THEN
  344           TYPE   = 'FULL'
  345         ELSE
  346           TYPE   = INXOUT
  347         END IF
  348 !
  349 ! initializations ---------------------------------------------------- *
  350 !
  351       IF ( .NOT.DINIT ) THEN
  352           IF ( IAPROC .LE. NAPROC ) THEN
  353               CALL W3DIMW ( IMOD, NDSE, NDST )
  354             ELSE
  355               CALL W3DIMW ( IMOD, NDSE, NDST, .FALSE. )
  356             END IF
  357         END IF
  358 !
  359       IF ( IAPROC .LE. NAPROC ) VA(:,0) = 0.
  360 !
  361       LRECL  = MAX ( LRB*NSPEC ,                                      &
  362                      LRB*(6+(25/LRB)+(9/LRB)+(29/LRB)+(3/LRB)) )
  363       NSIZE  = LRECL / LRB
  364 !     --- Allocate buffer array with zeros (used to
  365 !         fill bytes up to size LRECL). ---
  366       ALLOCATE(WRITEBUFF(NSIZE))
  367       WRITEBUFF(:) = 0.
  368 !
  369 !     Allocate memory to receive fields needed for coupling
  370       IF (OARST) THEN
  371         ALLOCATE(TMP(NSEA))
  372         ALLOCATE(TMP2(NSEA))
  373       ENDIF
  374 !
  375 ! open file ---------------------------------------------------------- *
  376 !
  377       I      = LEN_TRIM(FILEXT)
  378       J      = LEN_TRIM(FNMPRE)
  379 !
  380 !CHECKPOINT
  381       IF ( PRESENT(FLRSTRT) .AND. FLRSTRT) THEN
  382           WRITE(TIMETAG,"(i8.8,'.'i6.6)")TIME(1),TIME(2)
  383           FNAME=TIMETAG//'.restart.'//FILEXT(:I)
  384       ELSE
  385          IF ( IFILE.EQ.0 ) THEN
 
 
                                                         w3iorsmd.F90  page   8
 
 
  386             FNAME  = 'restart.'//FILEXT(:I)
  387          ELSE
  388             FNAME  = 'restartNNN.'//FILEXT(:I)
  389             IF ( WRITE .AND. IAPROC.EQ.NAPRST )                         &
  390                WRITE (FNAME(8:10),'(I3.3)') IFILE
  391         END IF
  392       END IF
  393             
  394       IFILE  = IFILE + 1
  395 !
  396  
  397       IF(NDST.EQ.NDSR)THEN
  398          IF ( IAPROC .EQ. NAPERR )                                    &
  399             WRITE(NDSE,'(A,I8)')'UNIT NUMBERS OF RESTART FILE AND '&
  400             //'TEST OUTPUT ARE THE SAME : ',NDST
  401          CALL EXTCDE ( 15 )
  402       ENDIF
  403            
  404       IF ( WRITE ) THEN
  405           IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST )                    &
  406           OPEN (NDSR,FILE=FNMPRE(:J)//FNAME,FORM='UNFORMATTED',       &
  407                 ACCESS='STREAM',ERR=800,IOSTAT=IERR)
  408         ELSE
  409           OPEN (NDSR,FILE=FNMPRE(:J)//FNAME,FORM='UNFORMATTED',       &
  410                 ACCESS='STREAM',ERR=800,IOSTAT=IERR,                  &
  411                 STATUS='OLD',ACTION='READ')
  412         END IF
  413 !
  414 ! test info ---------------------------------------------------------- *
  415 !
  416       IF ( WRITE ) THEN
  417 !
  418           IF ( IAPROC .EQ. NAPRST ) THEN
  419 !           Because data has mixed data types we do not know how many
  420 !           bytes remain to fill up to LRECL. ---
  421 !           --- Make the entire record zero ---
  422             WRITEBUFF(:) = 0.
  423             WRITE (NDSR,POS=1) WRITEBUFF
  424 !           --- Replace zeros with data ---
  425             WRITE (NDSR,POS=1) IDSTR, VERINI, GNAME, TYPE, NSEA, NSPEC
  426           END IF
  427           RSTYPE = 3
  428 !
  429         ELSE
  430           READ (NDSR,POS=1,ERR=802,IOSTAT=IERR)                       &
  431             IDTST, VERTST, TNAME, TYPE, NSEAT, MSPEC
  432 !
  433           IF ( IDTST .NE. IDSTR ) THEN
  434               IF ( IAPROC .EQ. NAPERR )                               &
  435                   WRITE (NDSE,901) IDTST, IDSTR
  436               CALL EXTCDE ( 10 )
  437             END IF
  438           IF ( VERTST .NE. VERINI ) THEN
  439               IF ( IAPROC .EQ. NAPERR )                               &
  440                   WRITE (NDSE,902) VERTST, VERINI
 
 
                                                         w3iorsmd.F90  page   9
 
 
  441               CALL EXTCDE ( 11 )
  442             END IF
  443           IF ( TNAME .NE. GNAME ) THEN
  444               IF ( IAPROC .EQ. NAPERR )                               &
  445                   WRITE (NDSE,903) TNAME, GNAME
  446             END IF
  447           IF (TYPE.NE.'FULL' .AND. TYPE.NE.'COLD' .AND.               &
  448               TYPE.NE.'WIND' .AND. TYPE.NE.'CALM' ) THEN
  449               IF ( IAPROC .EQ. NAPERR )                               &
  450                   WRITE (NDSE,904) TYPE
  451               CALL EXTCDE ( 12 )
  452             END IF
  453           IF (NSEAT.NE.NSEA .OR. NSPEC.NE.MSPEC) THEN
  454               IF ( IAPROC .EQ. NAPERR )                               &
  455                   WRITE (NDSE,905) MSPEC, NSEAT, NSPEC, NSEA
  456               CALL EXTCDE ( 13 )
  457             END IF
  458           IF (TYPE.EQ.'FULL') THEN
  459               RSTYPE = 2
  460             ELSE IF (TYPE.EQ.'WIND') THEN
  461               RSTYPE = 1
  462             ELSE IF (TYPE.EQ.'CALM') THEN
  463               RSTYPE = 4
  464             ELSE
  465               RSTYPE = 0
  466             END IF
  467 !
  468         END IF
  469 !
  470   100 CONTINUE
  471 !
  472 ! TIME if required --------------------------------------------------- *
  473 !
  474       IF (TYPE.EQ.'FULL') THEN
  475           RPOS  = 1_8 + LRECL*(2-1_8)
  476           IF ( WRITE ) THEN
  477               IF ( IAPROC .EQ. NAPRST ) THEN
  478                 WRITEBUFF(:) = 0.
  479                 WRITE (NDSR,POS=RPOS) WRITEBUFF
  480                 WRITE (NDSR,POS=RPOS) TIME
  481               END IF
  482             ELSE
  483               READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR) TTIME
  484               IF (TIME(1).NE.TTIME(1) .OR. TIME(2).NE.TTIME(2)) THEN
  485                   IF ( IAPROC .EQ. NAPERR )                           &
  486                       WRITE (NDSE,906) TTIME, TIME
  487                   CALL EXTCDE ( 20 )
  488                 END IF
  489             END IF
  490 !
  491         END IF
  492 !
  493 ! Spectra ------------------------------------------------------------ *
  494 !          ( Bail out if write for TYPE.EQ.'WIND' )
  495 !
 
 
                                                         w3iorsmd.F90  page  10
 
 
  496       IF ( WRITE ) THEN
  497           IF ( TYPE.EQ.'WIND' .OR. TYPE.EQ.'CALM' ) THEN
  498               IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST ) THEN
  499                 CLOSE ( NDSR )
  500               END IF
  501               RETURN
  502             ELSE IF ( IAPROC.LE.NAPROC .OR. IAPROC.EQ. NAPRST ) THEN
  503 !
  504 ! Original non-server version writing of spectra
  505 !
  506               IF ( .NOT.IOSFLG .OR. (NAPROC.EQ.1.AND.NAPRST.EQ.1) ) THEN
  507                   DO JSEA=1, NSEAL
  508                     CALL INIT_GET_ISEA(ISEA, JSEA)
  509                     NREC   = ISEA + 2
  510                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  511                     WRITEBUFF(:) = 0.
  512                     WRITEBUFF(1:NSPEC) = VA(1:NSPEC,JSEA)
  513                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  514                     END DO
  515 !
  516 ! I/O server version writing of spectra ( !/MPI )
  517 !
  518                 ELSE
  519 !
  520                 IF (LPDLIB .and. (GTYPE.eq.UNGTYPE)) THEN
  521                 ELSE
  522                     
  523                   IF ( IAPROC .NE. NAPRST ) THEN
  524                       NRQ    = 1
  525                     ELSE IF ( NAPRST .LE. NAPROC ) THEN
  526                       NRQ    = NAPROC - 1
  527                     ELSE
  528                       NRQ    = NAPROC
  529                     END IF
  530 !
  531                   ALLOCATE ( STAT1(MPI_STATUS_SIZE,NRQ) )
  532                   IF ( IAPROC .EQ. NAPRST ) CALL MPI_STARTALL    &
  533                                       ( NRQ, IRQRSS, IERR_MPI )
  534 !
  535                   DO IB=1, NBLKRS
  536                     ISEA0  = 1 + (IB-1)*RSBLKS*NAPROC
  537                     ISEAN  = MIN ( NSEA , IB*RSBLKS*NAPROC )
  538 !
  539                     IF ( IAPROC .EQ. NAPRST ) THEN
  540 !
  541                         IH     = 1 + NRQ * (IB-1)
  542                         CALL MPI_WAITALL                         &
  543                            ( NRQ, IRQRSS(IH), STAT1, IERR_MPI )
  544                         IF ( IB .LT. NBLKRS ) THEN
  545                             IH     = 1 + NRQ * IB
  546                             CALL MPI_STARTALL                    &
  547                                ( NRQ, IRQRSS(IH), IERR_MPI )
  548                           END IF
  549 !
  550                         DO ISEA=ISEA0, ISEAN
 
 
                                                         w3iorsmd.F90  page  11
 
 
  551                           NREC   = ISEA + 2
  552                           CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, IP)
  553                           RPOS   = 1_8 + LRECL*(NREC-1_8)
  554                           WRITEBUFF(:) = 0.
  555                           IF ( IP .EQ. NAPRST ) THEN
  556                               WRITEBUFF(1:NSPEC) = VA(1:NSPEC,JSEA)
  557                             ELSE
  558                               JSEA   = JSEA - 2*((IB-1)/2)*RSBLKS
  559                               WRITEBUFF(1:NSPEC) = VAAUX(1:NSPEC,JSEA,IP)
  560                             END IF
  561                             WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) &
  562                                   WRITEBUFF
  563                           END DO
  564 !
  565                       ELSE
  566 !
  567                         CALL MPI_STARTALL                        &
  568                            ( 1, IRQRSS(IB), IERR_MPI )
  569                         CALL MPI_WAITALL                         &
  570                            ( 1, IRQRSS(IB), STAT1, IERR_MPI )
  571 !
  572                       END IF
  573                     END DO
  574 !
  575                   DEALLOCATE ( STAT1 )
  576                 END IF
  577 !
  578                 END IF
  579 !
  580             END IF
  581         ELSE
  582 !
  583 ! Reading spectra
  584 !
  585           IF ( TYPE.EQ.'WIND' .OR. TYPE.EQ.'CALM' ) THEN
  586           ELSE
  587             IF (LPDLIB .and. (GTYPE.eq.UNGTYPE)) THEN
  588             ELSE
  589             NSEAL_MIN = 1 + (NSEA-NAPROC)/NAPROC
  590             IF ( NAPROC.GT.1 ) THEN
  591                IF ( IAPROC.LE.NAPROC ) THEN
  592                ALLOCATE( VGBUFF( NSIZE * NAPROC ) )
  593                ALLOCATE( VLBUFF( NSIZE ) )
  594                DO JSEA = 1, NSEAL_MIN
  595                  IF ( IAPROC .EQ. NAPROC ) THEN
  596                      RPOS = 1_8 + (2 + (JSEA - 1_8) * NAPROC) * LRECL
  597                      READ(NDSR, POS=RPOS,ERR=802,IOSTAT=IERR) VGBUFF(:)
  598                    ELSE
  599                      VGBUFF(:) = 0.
  600                    END IF
  601                  CALL MPI_SCATTER(VGBUFF, NSIZE, MPI_REAL,             &
  602                                   VLBUFF, NSIZE, MPI_REAL,             &
  603                                   NAPROC-1, MPI_COMM_WCMP, IERR        )
  604                  VA(1:NSPEC,JSEA) = VLBUFF(1:NSPEC)
  605                  END DO
 
 
                                                         w3iorsmd.F90  page  12
 
 
  606                JSEA = NSEAL_MIN + 1
  607                IF ( JSEA.EQ.NSEAL ) THEN
  608                   ISEA = IAPROC + (JSEA - 1) * NAPROC
  609                   NREC = ISEA + 2
  610                   RPOS = 1_8 + LRECL*(NREC-1_8)
  611                   READ (NDSR, POS=RPOS, ERR=802, IOSTAT=IERR)          &
  612                             (VA(I,JSEA), I=1,NSPEC)
  613                  END IF
  614                DEALLOCATE( VGBUFF )
  615                DEALLOCATE( VLBUFF )
  616                END IF
  617             ELSE
  618               VA = 0.
  619               DO JSEA=1, NSEAL
  620                 CALL INIT_GET_ISEA(ISEA, JSEA)
  621                 NREC   = ISEA + 2
  622                 RPOS   = 1_8 + LRECL*(NREC-1_8)
  623                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  624                          (VA(I,JSEA),I=1,NSPEC)
  625                 ENDDO
  626             END IF
  627             END IF
  628           END IF
  629         END IF
  630               
  631 !AR: Must be checked better ... will do that when cleaning debugging swit>
  632         VA = MAX(0.,VA)
  633 !
  634 ! Water level etc. if required --------------------------------------- *
  635 !     ( For cold start write test output and cold start initialize
  636 !       water levels. Note that MAPSTA overwrites the one read from the
  637 !       model definition file, so that it need not be initialized. )
  638 !
  639       NREC   = NSEA + 3
  640       NPART  = 1 + (NSEA-1)/NSIZE
  641       NPRTX2 = 1 + (NX-1)/NSIZE
  642       NPRTY2 = 1 + (NY-1)/NSIZE
  643 !
  644       IF ( WRITE ) THEN
  645 !
  646           IF (TYPE.EQ.'FULL') THEN
  647 !
  648               IF ( IAPROC .EQ. NAPRST ) THEN
  649 !
  650                   ALLOCATE ( STAT2(MPI_STATUS_SIZE,NRQRS) )
  651                   CALL MPI_WAITALL                               &
  652                      ( NRQRS, IRQRS , STAT2, IERR_MPI )
  653                   DEALLOCATE ( STAT2 )
  654 !
  655                   RPOS  = 1_8 + LRECL*(NREC-1_8)
  656                   WRITEBUFF(:) = 0.
  657                   WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  658                   WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)           &
  659                           TLEV, TICE
  660                   DO IPART=1,NPART
 
 
                                                         w3iorsmd.F90  page  13
 
 
  661                     NREC  = NREC + 1
  662                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  663                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  664                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
  665                           (WLV(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
  666                                           MIN(NSEA,IPART*NSIZE))
  667                     END DO
  668                   DO IPART=1,NPART
  669                     NREC  = NREC + 1
  670                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  671                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  672                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
  673                           (ICE(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
  674                                           MIN(NSEA,IPART*NSIZE))
  675                   END DO
  676                         
  677                         
  678                   ALLOCATE ( MAPTMP(NY,NX) )
  679                   MAPTMP = MAPSTA + 8*MAPST2
  680                   DO IY=1, NY
  681                     DO IPART=1,NPRTX2
  682                       NREC  = NREC + 1
  683                       RPOS  = 1_8 + LRECL*(NREC-1_8)
  684                       WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)       &
  685                              WRITEBUFF
  686                       WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)       &
  687                             (MAPTMP(IY,IXL),IXL=1+(IPART-1)*NSIZE,    &
  688                                                 MIN(NX,IPART*NSIZE))
  689                       END DO
  690                     END DO
  691                   DEALLOCATE ( MAPTMP )
  692                   DO IPART=1,NPART
  693                     NREC  = NREC + 1
  694                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  695                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  696                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
  697                           (UST(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
  698                                           MIN(NSEA,IPART*NSIZE))
  699                     END DO
  700                   DO IPART=1,NPART
  701                     NREC  = NREC + 1
  702                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  703                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  704                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
  705                           (USTDIR(ISEA),ISEA=1+(IPART-1)*NSIZE,       &
  706                                           MIN(NSEA,IPART*NSIZE))
  707                     END DO
  708                   DO IPART=1,NPART
  709                     NREC  = NREC + 1
  710                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  711                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  712                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
  713                           (ASF(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
  714                                           MIN(NSEA,IPART*NSIZE))
  715                     END DO
 
 
                                                         w3iorsmd.F90  page  14
 
 
  716                   DO IPART=1,NPART
  717                     NREC  = NREC + 1
  718                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  719                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  720                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
  721                           (FPIS(ISEA),ISEA=1+(IPART-1)*NSIZE,         &
  722                                           MIN(NSEA,IPART*NSIZE))
  723                     END DO
  724                 IF (OARST) THEN
  725                   CALL W3XETA ( IGRD, NDSE, NDST )
  726 !
  727                   IF ( FLOGRR(1,2) ) THEN
  728                     WRITE(NDSR,ERR=803,IOSTAT=IERR) CX(1:NSEA)
  729                     WRITE(NDSR,ERR=803,IOSTAT=IERR) CY(1:NSEA)
  730                   ENDIF
  731                   IF ( FLOGRR(1,9) )                                  &
  732                     WRITE(NDSR,ERR=803,IOSTAT=IERR) ICEF(1:NSEA)
  733                   IF ( FLOGRR(2,1) )                                  &
  734                     WRITE(NDSR,ERR=803,IOSTAT=IERR) HS(1:NSEA)
  735                   IF ( FLOGRR(2,2) )                                  &
  736                     WRITE(NDSR,ERR=803,IOSTAT=IERR) WLM(1:NSEA)
  737                   IF ( FLOGRR(2,4) )                                  &
  738                     WRITE(NDSR,ERR=803,IOSTAT=IERR) T0M1(1:NSEA)
  739                   IF ( FLOGRR(2,6) )                                  &
  740                     WRITE(NDSR,ERR=803,IOSTAT=IERR) FP0(1:NSEA)
  741                   IF ( FLOGRR(2,7) )                                  &
  742                     WRITE(NDSR,ERR=803,IOSTAT=IERR) THM(1:NSEA)
  743                   IF ( FLOGRR(5,2) )                                  &
  744                     WRITE(NDSR,ERR=803,IOSTAT=IERR) CHARN(1:NSEA)
  745                   IF ( FLOGRR(5,5) ) THEN
  746                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUWIX(1:NSEA)
  747                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUWIY(1:NSEA)
  748                   ENDIF
  749                   IF ( FLOGRR(5,11) )                                 &
  750                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TWS(1:NSEA)
  751                   IF ( FLOGRR(6,2) ) THEN
  752                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUOX(1:NSEA)
  753                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUOY(1:NSEA)
  754                   ENDIF
  755                   IF ( FLOGRR(6,3) )                                  &
  756                     WRITE(NDSR,ERR=803,IOSTAT=IERR) BHD(1:NSEA)
  757                   IF ( FLOGRR(6,4) )                                  &
  758                     WRITE(NDSR,ERR=803,IOSTAT=IERR) PHIOC(1:NSEA)
  759                   IF ( FLOGRR(6,5) ) THEN
  760                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TUSX(1:NSEA)
  761                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TUSY(1:NSEA)
  762                   ENDIF
  763                   IF ( FLOGRR(6,6) ) THEN
  764                     WRITE(NDSR,ERR=803,IOSTAT=IERR) USSX(1:NSEA)
  765                     WRITE(NDSR,ERR=803,IOSTAT=IERR) USSY(1:NSEA)
  766                   ENDIF
  767                   IF ( FLOGRR(6,10) ) THEN
  768                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUICE(1:NSEA,1)
  769                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUICE(1:NSEA,2)
  770                   ENDIF
 
 
                                                         w3iorsmd.F90  page  15
 
 
  771                   IF ( FLOGRR(7,2) ) THEN
  772                     WRITE(NDSR,ERR=803,IOSTAT=IERR) UBA(1:NSEA)
  773                     WRITE(NDSR,ERR=803,IOSTAT=IERR) UBD(1:NSEA)
  774                   ENDIF
  775                   IF ( FLOGRR(7,4) )                                  &
  776                     WRITE(NDSR,ERR=803,IOSTAT=IERR) PHIBBL(1:NSEA)
  777                   IF ( FLOGRR(7,5) ) THEN
  778                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUBBL(1:NSEA,1)
  779                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUBBL(1:NSEA,2)
  780                   ENDIF
  781 !
  782                   CALL W3SETA ( IGRD, NDSE, NDST )
  783                 ENDIF
  784               END IF
  785           END IF
  786       ELSE
  787           IF (TYPE.EQ.'FULL') THEN
  788               RPOS = 1_8 + LRECL*(NREC-1_8)
  789               READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)                &
  790                       TLEV, TICE
  791               DO IPART=1,NPART
  792                 NREC  = NREC + 1
  793                 RPOS = 1_8 + LRECL*(NREC-1_8)
  794                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  795                       (WLV(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
  796                                       MIN(NSEA,IPART*NSIZE))
  797                 END DO
  798               DO IPART=1,NPART
  799                 NREC  = NREC + 1
  800                 RPOS = 1_8 + LRECL*(NREC-1_8)
  801                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  802                       (ICE(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
  803                                       MIN(NSEA,IPART*NSIZE))
  804               END DO
  805               ALLOCATE ( MAPTMP(NY,NX) )
  806               DO IY=1, NY
  807                 DO IPART=1,NPRTX2
  808                   NREC  = NREC + 1
  809                   RPOS  = 1_8 + LRECL*(NREC-1_8)
  810                   READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)            &
  811                         (MAPTMP(IY,IXL),IXL=1+(IPART-1)*NSIZE,        &
  812                                             MIN(NX,IPART*NSIZE))
  813                   END DO
  814                 END DO
  815               MAPSTA = MOD(MAPTMP+2,8) - 2
  816               MAPST2 = (MAPTMP-MAPSTA) / 8
  817               DEALLOCATE ( MAPTMP )
  818 !
  819 ! Updates reflections maps:
  820 !
  821               IF (GTYPE.EQ.UNGTYPE) THEN
  822                 CALL SETUGIOBP
  823               ELSE
  824                 CALL W3SETREF
  825                 ENDIF
 
 
                                                         w3iorsmd.F90  page  16
 
 
  826 !
  827               DO IPART=1,NPART
  828                 NREC  = NREC + 1
  829                 RPOS  = 1_8 + LRECL*(NREC-1_8)
  830                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  831                       (UST(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
  832                                       MIN(NSEA,IPART*NSIZE))
  833                 END DO
  834               DO IPART=1,NPART
  835                 NREC  = NREC + 1
  836                 RPOS  = 1_8 + LRECL*(NREC-1_8)
  837                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  838                       (USTDIR(ISEA),ISEA=1+(IPART-1)*NSIZE,           &
  839                                       MIN(NSEA,IPART*NSIZE))
  840                 END DO
  841               DO IPART=1,NPART
  842                 NREC  = NREC + 1
  843                 RPOS  = 1_8 + LRECL*(NREC-1_8)
  844                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  845                       (ASF(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
  846                                       MIN(NSEA,IPART*NSIZE))
  847                 END DO
  848               DO IPART=1,NPART
  849                 NREC  = NREC + 1
  850                 RPOS  = 1_8 + LRECL*(NREC-1_8)
  851                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  852                       (FPIS(ISEA),ISEA=1+(IPART-1)*NSIZE,             &
  853                                       MIN(NSEA,IPART*NSIZE))
  854                 END DO
  855             IF (OARST) THEN
  856               IF ( FLOGRR(1,2) ) THEN
  857                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  858                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  859                 DO I=1, NSEALM
  860                   J = IAPROC + (I-1)*NAPROC
  861                   IF (J .LE. NSEA) THEN
  862                     CX(I) = TMP(J)
  863                     CY(I) = TMP2(J)
  864                   ENDIF
  865                 ENDDO
  866               ENDIF
  867               IF ( FLOGRR(1,9) ) THEN
  868                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  869                 DO I=1, NSEALM
  870                   J = IAPROC + (I-1)*NAPROC
  871                   IF (J .LE. NSEA) ICEF(I) = TMP(J)
  872                 ENDDO
  873               ENDIF
  874               IF ( FLOGRR(2,1) ) THEN
  875                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  876                 DO I=1, NSEALM
  877                   J = IAPROC + (I-1)*NAPROC
  878                   IF (J .LE. NSEA) HS(I) = TMP(J)
  879                 ENDDO
  880               ENDIF
 
 
                                                         w3iorsmd.F90  page  17
 
 
  881               IF ( FLOGRR(2,2) ) THEN
  882                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  883                 DO I=1, NSEALM
  884                   J = IAPROC + (I-1)*NAPROC
  885                   IF (J .LE. NSEA) WLM(I) = TMP(J)
  886                 ENDDO
  887               ENDIF
  888               IF ( FLOGRR(2,4) ) THEN
  889                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  890                 DO I=1, NSEALM
  891                   J = IAPROC + (I-1)*NAPROC
  892                   IF (J .LE. NSEA) THEN
  893                     T0M1(I) = TMP(J)
  894                   ENDIF
  895                 ENDDO
  896               ENDIF
  897               IF ( FLOGRR(2,6) ) THEN
  898                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  899                 DO I=1, NSEALM
  900                   J = IAPROC + (I-1)*NAPROC
  901                   IF (J .LE. NSEA) FP0(I) = TMP(J)
  902                 ENDDO
  903               ENDIF
  904               IF ( FLOGRR(2,7) ) THEN
  905                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  906                 DO I=1, NSEALM
  907                   J = IAPROC + (I-1)*NAPROC
  908                   IF (J .LE. NSEA) THM(I) = TMP(J)
  909                 ENDDO
  910               ENDIF
  911               IF ( FLOGRR(5,2) ) THEN
  912                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  913                 DO I=1, NSEALM
  914                   J = IAPROC + (I-1)*NAPROC
  915                   IF (J .LE. NSEA) CHARN(I) = TMP(J)
  916                 ENDDO
  917               ENDIF
  918               IF ( FLOGRR(5,5) ) THEN
  919                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  920                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  921                 DO I=1, NSEALM
  922                   J = IAPROC + (I-1)*NAPROC
  923                   IF (J .LE. NSEA) THEN
  924                     TAUWIX(I) = TMP(J)
  925                     TAUWIY(I) = TMP2(J)
  926                   ENDIF
  927                 ENDDO
  928               ENDIF
  929               IF ( FLOGRR(5,11) ) THEN
  930                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  931                 DO I=1, NSEALM
  932                   J = IAPROC + (I-1)*NAPROC
  933                   IF (J .LE. NSEA) TWS(I) = TMP(J)
  934                 ENDDO
  935               ENDIF
 
 
                                                         w3iorsmd.F90  page  18
 
 
  936               IF ( FLOGRR(6,2) ) THEN
  937                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  938                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  939                 DO I=1, NSEALM
  940                   J = IAPROC + (I-1)*NAPROC
  941                   IF (J .LE. NSEA) THEN
  942                     TAUOX(I) = TMP(J)
  943                     TAUOY(I) = TMP2(J)
  944                   ENDIF
  945                 ENDDO
  946               ENDIF
  947               IF ( FLOGRR(6,3) ) THEN
  948                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  949                 DO I=1, NSEALM
  950                   J = IAPROC + (I-1)*NAPROC
  951                   IF (J .LE. NSEA) BHD(I) = TMP(J)
  952                 ENDDO
  953               ENDIF
  954               IF ( FLOGRR(6,4) ) THEN
  955                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  956                 DO I=1, NSEALM
  957                   J = IAPROC + (I-1)*NAPROC
  958                   IF (J .LE. NSEA) PHIOC(I) = TMP(J)
  959                 ENDDO
  960               ENDIF
  961               IF ( FLOGRR(6,5) ) THEN
  962                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  963                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  964                 DO I=1, NSEALM
  965                   J = IAPROC + (I-1)*NAPROC
  966                   IF (J .LE. NSEA) THEN
  967                     TUSX(I) = TMP(J)
  968                     TUSY(I) = TMP2(J)
  969                   ENDIF
  970                 ENDDO
  971               ENDIF
  972               IF ( FLOGRR(6,6) ) THEN
  973                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  974                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  975                 DO I=1, NSEALM
  976                   J = IAPROC + (I-1)*NAPROC
  977                   IF (J .LE. NSEA) THEN
  978                     USSX(I) = TMP(J)
  979                     USSY(I) = TMP2(J)
  980                   ENDIF
  981                 ENDDO
  982               ENDIF
  983               IF ( FLOGRR(6,10) ) THEN
  984                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  985                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  986                 DO I=1, NSEALM
  987                   J = IAPROC + (I-1)*NAPROC
  988                   IF (J .LE. NSEA) THEN
  989                     TAUICE(I,1) = TMP(J)
  990                     TAUICE(I,2) = TMP2(J)
 
 
                                                         w3iorsmd.F90  page  19
 
 
  991                   ENDIF
  992                 ENDDO
  993               ENDIF
  994               IF ( FLOGRR(7,2) ) THEN
  995                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  996                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  997                 DO I=1, NSEALM
  998                   J = IAPROC + (I-1)*NAPROC
  999                   IF (J .LE. NSEA) THEN
 1000                     UBA(I) = TMP(J)
 1001                     UBD(I) = TMP2(J)
 1002                   ENDIF
 1003                 ENDDO
 1004               ENDIF
 1005               IF ( FLOGRR(7,4) ) THEN
 1006                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
 1007                 DO I=1, NSEALM
 1008                   J = IAPROC + (I-1)*NAPROC
 1009                   IF (J .LE. NSEA) PHIBBL(I) = TMP(J)
 1010                 ENDDO
 1011               ENDIF
 1012               IF ( FLOGRR(7,5) ) THEN
 1013                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
 1014                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
 1015                 DO I=1, NSEALM
 1016                   J = IAPROC + (I-1)*NAPROC
 1017                   IF (J .LE. NSEA) THEN
 1018                     TAUBBL(I,1) = TMP(J)
 1019                     TAUBBL(I,2) = TMP2(J)
 1020                   ENDIF
 1021                 ENDDO
 1022               ENDIF
 1023             ENDIF
 1024           ELSE
 1025               TLEV(1) = -1
 1026               TLEV(2) =  0
 1027               TICE(1) = -1
 1028               TICE(2) =  0
 1029               TIC1(1) = -1
 1030               TIC1(2) =  0
 1031               TIC5(1) = -1
 1032               TIC5(2) =  0
 1033               WLV     =  0.
 1034               ICE     =  0.
 1035               ASF     =  1.
 1036               FPIS    =  DUMFPI
 1037                                
 1038             ! Initialize coupled fields if no restart is present
 1039             IF (OARST) THEN
 1040               CX      = 0.
 1041               CY      = 0.
 1042               ICEF    = 0.
 1043               HS      = 0.
 1044               WLM     = 0.
 1045               T0M1    = 0.
 
 
                                                         w3iorsmd.F90  page  20
 
 
 1046               FP0     = 1.
 1047               THM     = 0.
 1048               CHARN   = 0.0185
 1049               TAUWIX  = 0.
 1050               TAUWIY  = 0.
 1051               TWS     = 0.
 1052               TAUOX   = 0.
 1053               TAUOY   = 0.
 1054               BHD     = 0.
 1055               PHIOC   = 0.
 1056               TUSX    = 0.
 1057               TUSY    = 0.
 1058               USSX    = 0.
 1059               USSY    = 0.
 1060               TAUICE  = 0.
 1061               UBA     = 0.
 1062               UBD     = 0.
 1063               PHIBBL  = 0.
 1064               TAUBBL  = 0.
 1065             ENDIF
 1066           END IF
 1067         END IF
 1068 !
 1069 ! Close file --------------------------------------------------------- *
 1070 !
 1071       IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST ) THEN
 1072         CLOSE ( NDSR )
 1073       END IF
 1074 !
 1075       IF (ALLOCATED(WRITEBUFF)) DEALLOCATE(WRITEBUFF)
 1076       IF (ALLOCATED(TMP))  DEALLOCATE(TMP)
 1077       IF (ALLOCATED(TMP2)) DEALLOCATE(TMP2)
 1078 !
 1079       RETURN
 1080 !
 1081 ! Escape locations read errors :
 1082 !
 1083   800 CONTINUE
 1084       TYPE   = 'CALM'
 1085       RSTYPE = 4
 1086       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,990) TYPE, IERR
 1087       GOTO 100
 1088 !
 1089   801 CONTINUE
 1090       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,991)
 1091       CALL EXTCDE ( 30 )
 1092 !
 1093   802 CONTINUE
 1094       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,992) IERR
 1095       CALL EXTCDE ( 31 )
 1096 !
 1097   803 CONTINUE
 1098       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,993) IERR, RPOS
 1099       CALL EXTCDE ( 31 )
 1100 !
 
 
                                                         w3iorsmd.F90  page  21
 
 
 1101 ! Formats
 1102 !
 1103   900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
 1104                '     ILLEGAL INXOUT VALUE: ',A/)
 1105   901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
 1106                '     ILLEGAL IDSTR, READ : ',A/                       &
 1107                '                   CHECK : ',A/)
 1108   902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
 1109                '     ILLEGAL VERINI, READ : ',A/                      &
 1110                '                    CHECK : ',A/)
 1111   903 FORMAT (/' *** WAVEWATCH III WARNING IN W3IORS :'/              &
 1112                '     ILLEGAL GNAME, READ : ',A/                       &
 1113                '                   CHECK : ',A/)
 1114   904 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
 1115                '     ILLEGAL TYPE : ',A/)
 1116   905 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
 1117                '     CONFLICTING NSPEC, NSEA GRID : ',2I8/            &
 1118                '                         EXPECTED : ',2I8/)
 1119   906 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
 1120                '     CONFLICTING TIMES: FILE : ',I10.8,I8.6/          &
 1121                '                       MODEL : ',I10.8,I8.6/)
 1122 !
 1123   990 FORMAT (/' *** WAVEWATCH III WARNING IN W3IORS : '/             &
 1124                '     NO READABLE RESTART FILE, ',                     &
 1125                     'INITIALIZE WITH ''',A,''' INSTEAD'/              &
 1126                '     IOSTAT =',I5/)
 1127   991 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS : '/               &
 1128                '     PREMATURE END OF FILE'/)
 1129   992 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS : '/               &
 1130                '     ERROR IN READING FROM FILE'/                     &
 1131                '     IOSTAT =',I5/)
 1132   993 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS : '/               &
 1133                '     ERROR IN WRITING TO FILE'/                       &
 1134                '     IOSTAT =',I5,', POS =',I11 /)
 1135 !
 1136 !/
 1137 !/ End of W3IORS ----------------------------------------------------- /
 1138 !/
 1139       END SUBROUTINE W3IORS
 1140 !/
 1141 !/ End of module W3IORSMD -------------------------------------------- /
 1142 !/
 1143       END MODULE W3IORSMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
