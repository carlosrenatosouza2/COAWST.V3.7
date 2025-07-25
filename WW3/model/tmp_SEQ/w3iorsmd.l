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
  262 !/
  263       USE W3SERVMD, ONLY: EXTCDE
  264       USE CONSTANTS, only: LPDLIB
  265       USE W3PARALL, ONLY: INIT_GET_ISEA, INIT_GET_JSEA_ISPROC
  266       USE W3GDATMD, ONLY: NK, NTH
  267 !!!!!/PDLIB    USE PDLIB_FIELD_VEC!, only : UNST_PDLIB_READ_FROM_FILE, UN>
  268 !
  269       IMPLICIT NONE
  270 !
  271 !/
  272 !/ ------------------------------------------------------------------- /
  273 !/ Parameter list
  274 !/
  275       INTEGER                       :: NDSR
 
 
                                                         w3iorsmd.F90  page   6
 
 
  276 !      INTEGER, INTENT(IN)           :: NDSR
  277       INTEGER, INTENT(IN), OPTIONAL :: IMOD
  278       REAL, INTENT(INOUT)           :: DUMFPI
  279       CHARACTER, INTENT(IN)         :: INXOUT*(*)
  280       LOGICAL, INTENT(IN),OPTIONAL  :: FLRSTRT
  281 !/
  282 !/ ------------------------------------------------------------------- /
  283 !/ Local parameters
  284 !/
  285       INTEGER, PARAMETER      :: LRB = 4
  286 !
  287       INTEGER                 :: IGRD, I, J, LRECL, NSIZE, IERR,      &
  288                                  NSEAT, MSPEC, TTIME(2), ISEA, JSEA,  &
  289                                  NREC, NPART, IPART, IX, IY, IXL, IP, &
  290                                  NPRTX2, NPRTY2, IYL
  291       INTEGER, ALLOCATABLE    :: MAPTMP(:,:)
  292       INTEGER(KIND=8)         :: RPOS
  293       REAL(KIND=LRB), ALLOCATABLE :: WRITEBUFF(:), TMP(:), TMP2(:)
  294                                                                   
  295       LOGICAL                 :: WRITE, IOSFLG
  296       CHARACTER(LEN=4)        :: TYPE
  297       CHARACTER(LEN=10)       :: VERTST
  298 !      CHARACTER(LEN=21)       :: FNAME
  299       CHARACTER(LEN=40)       :: FNAME
  300       CHARACTER(LEN=26)       :: IDTST
  301       CHARACTER(LEN=30)       :: TNAME
  302       CHARACTER(LEN=15)       :: TIMETAG
  303 !/
  304 !/ ------------------------------------------------------------------- /
  305 !/
  306 !
  307 ! Constant NDSR for using mpiifort in ZEUS ... paralell runs crashing
  308 !  because compiler doesn't accept reciclyng of UNIT for FORMATTED or
  309 !  UNFORMATTED files in OPEN
  310 !
  311 !     NDSR = 525
  312                 
  313       IOSFLG = IOSTYP .GT. 0
  314 !
  315 ! test parameter list input ------------------------------------------ *
  316 !
  317       IF ( PRESENT(IMOD) ) THEN
  318           IGRD   = IMOD
  319         ELSE
  320           IGRD   = 1
  321         END IF
  322 !
  323       CALL W3SETO ( IGRD, NDSE, NDST )
  324       CALL W3SETG ( IGRD, NDSE, NDST )
  325       CALL W3SETW ( IGRD, NDSE, NDST )
  326 !
  327       IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'HOT'  .AND.               &
  328           INXOUT.NE.'COLD' .AND. INXOUT.NE.'WIND' .AND.               &
  329           INXOUT.NE.'CALM' ) THEN
  330           IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,900) INXOUT
 
 
                                                         w3iorsmd.F90  page   7
 
 
  331           CALL EXTCDE ( 1 )
  332         END IF
  333 !
  334       WRITE = INXOUT .NE. 'READ'
  335       IF ( INXOUT .EQ. 'HOT' ) THEN
  336           TYPE   = 'FULL'
  337         ELSE
  338           TYPE   = INXOUT
  339         END IF
  340 !
  341 ! initializations ---------------------------------------------------- *
  342 !
  343       IF ( .NOT.DINIT ) THEN
  344           IF ( IAPROC .LE. NAPROC ) THEN
  345               CALL W3DIMW ( IMOD, NDSE, NDST )
  346             ELSE
  347               CALL W3DIMW ( IMOD, NDSE, NDST, .FALSE. )
  348             END IF
  349         END IF
  350 !
  351       IF ( IAPROC .LE. NAPROC ) VA(:,0) = 0.
  352 !
  353       LRECL  = MAX ( LRB*NSPEC ,                                      &
  354                      LRB*(6+(25/LRB)+(9/LRB)+(29/LRB)+(3/LRB)) )
  355       NSIZE  = LRECL / LRB
  356 !     --- Allocate buffer array with zeros (used to
  357 !         fill bytes up to size LRECL). ---
  358       ALLOCATE(WRITEBUFF(NSIZE))
  359       WRITEBUFF(:) = 0.
  360 !
  361 !     Allocate memory to receive fields needed for coupling
  362       IF (OARST) THEN
  363         ALLOCATE(TMP(NSEA))
  364         ALLOCATE(TMP2(NSEA))
  365       ENDIF
  366 !
  367 ! open file ---------------------------------------------------------- *
  368 !
  369       I      = LEN_TRIM(FILEXT)
  370       J      = LEN_TRIM(FNMPRE)
  371 !
  372 !CHECKPOINT
  373       IF ( PRESENT(FLRSTRT) .AND. FLRSTRT) THEN
  374           WRITE(TIMETAG,"(i8.8,'.'i6.6)")TIME(1),TIME(2)
  375           FNAME=TIMETAG//'.restart.'//FILEXT(:I)
  376       ELSE
  377          IF ( IFILE.EQ.0 ) THEN
  378             FNAME  = 'restart.'//FILEXT(:I)
  379          ELSE
  380             FNAME  = 'restartNNN.'//FILEXT(:I)
  381             IF ( WRITE .AND. IAPROC.EQ.NAPRST )                         &
  382                WRITE (FNAME(8:10),'(I3.3)') IFILE
  383         END IF
  384       END IF
  385             
 
 
                                                         w3iorsmd.F90  page   8
 
 
  386       IFILE  = IFILE + 1
  387 !
  388  
  389       IF(NDST.EQ.NDSR)THEN
  390          IF ( IAPROC .EQ. NAPERR )                                    &
  391             WRITE(NDSE,'(A,I8)')'UNIT NUMBERS OF RESTART FILE AND '&
  392             //'TEST OUTPUT ARE THE SAME : ',NDST
  393          CALL EXTCDE ( 15 )
  394       ENDIF
  395            
  396       IF ( WRITE ) THEN
  397           IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST )                    &
  398           OPEN (NDSR,FILE=FNMPRE(:J)//FNAME,FORM='UNFORMATTED',       &
  399                 ACCESS='STREAM',ERR=800,IOSTAT=IERR)
  400         ELSE
  401           OPEN (NDSR,FILE=FNMPRE(:J)//FNAME,FORM='UNFORMATTED',       &
  402                 ACCESS='STREAM',ERR=800,IOSTAT=IERR,                  &
  403                 STATUS='OLD',ACTION='READ')
  404         END IF
  405 !
  406 ! test info ---------------------------------------------------------- *
  407 !
  408       IF ( WRITE ) THEN
  409 !
  410           IF ( IAPROC .EQ. NAPRST ) THEN
  411 !           Because data has mixed data types we do not know how many
  412 !           bytes remain to fill up to LRECL. ---
  413 !           --- Make the entire record zero ---
  414             WRITEBUFF(:) = 0.
  415             WRITE (NDSR,POS=1) WRITEBUFF
  416 !           --- Replace zeros with data ---
  417             WRITE (NDSR,POS=1) IDSTR, VERINI, GNAME, TYPE, NSEA, NSPEC
  418           END IF
  419           RSTYPE = 3
  420 !
  421         ELSE
  422           READ (NDSR,POS=1,ERR=802,IOSTAT=IERR)                       &
  423             IDTST, VERTST, TNAME, TYPE, NSEAT, MSPEC
  424 !
  425           IF ( IDTST .NE. IDSTR ) THEN
  426               IF ( IAPROC .EQ. NAPERR )                               &
  427                   WRITE (NDSE,901) IDTST, IDSTR
  428               CALL EXTCDE ( 10 )
  429             END IF
  430           IF ( VERTST .NE. VERINI ) THEN
  431               IF ( IAPROC .EQ. NAPERR )                               &
  432                   WRITE (NDSE,902) VERTST, VERINI
  433               CALL EXTCDE ( 11 )
  434             END IF
  435           IF ( TNAME .NE. GNAME ) THEN
  436               IF ( IAPROC .EQ. NAPERR )                               &
  437                   WRITE (NDSE,903) TNAME, GNAME
  438             END IF
  439           IF (TYPE.NE.'FULL' .AND. TYPE.NE.'COLD' .AND.               &
  440               TYPE.NE.'WIND' .AND. TYPE.NE.'CALM' ) THEN
 
 
                                                         w3iorsmd.F90  page   9
 
 
  441               IF ( IAPROC .EQ. NAPERR )                               &
  442                   WRITE (NDSE,904) TYPE
  443               CALL EXTCDE ( 12 )
  444             END IF
  445           IF (NSEAT.NE.NSEA .OR. NSPEC.NE.MSPEC) THEN
  446               IF ( IAPROC .EQ. NAPERR )                               &
  447                   WRITE (NDSE,905) MSPEC, NSEAT, NSPEC, NSEA
  448               CALL EXTCDE ( 13 )
  449             END IF
  450           IF (TYPE.EQ.'FULL') THEN
  451               RSTYPE = 2
  452             ELSE IF (TYPE.EQ.'WIND') THEN
  453               RSTYPE = 1
  454             ELSE IF (TYPE.EQ.'CALM') THEN
  455               RSTYPE = 4
  456             ELSE
  457               RSTYPE = 0
  458             END IF
  459 !
  460         END IF
  461 !
  462   100 CONTINUE
  463 !
  464 ! TIME if required --------------------------------------------------- *
  465 !
  466       IF (TYPE.EQ.'FULL') THEN
  467           RPOS  = 1_8 + LRECL*(2-1_8)
  468           IF ( WRITE ) THEN
  469               IF ( IAPROC .EQ. NAPRST ) THEN
  470                 WRITEBUFF(:) = 0.
  471                 WRITE (NDSR,POS=RPOS) WRITEBUFF
  472                 WRITE (NDSR,POS=RPOS) TIME
  473               END IF
  474             ELSE
  475               READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR) TTIME
  476               IF (TIME(1).NE.TTIME(1) .OR. TIME(2).NE.TTIME(2)) THEN
  477                   IF ( IAPROC .EQ. NAPERR )                           &
  478                       WRITE (NDSE,906) TTIME, TIME
  479                   CALL EXTCDE ( 20 )
  480                 END IF
  481             END IF
  482 !
  483         END IF
  484 !
  485 ! Spectra ------------------------------------------------------------ *
  486 !          ( Bail out if write for TYPE.EQ.'WIND' )
  487 !
  488       IF ( WRITE ) THEN
  489           IF ( TYPE.EQ.'WIND' .OR. TYPE.EQ.'CALM' ) THEN
  490               IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST ) THEN
  491                 CLOSE ( NDSR )
  492               END IF
  493               RETURN
  494             ELSE IF ( IAPROC.LE.NAPROC .OR. IAPROC.EQ. NAPRST ) THEN
  495 !
 
 
                                                         w3iorsmd.F90  page  10
 
 
  496 ! Original non-server version writing of spectra
  497 !
  498               IF ( .NOT.IOSFLG .OR. (NAPROC.EQ.1.AND.NAPRST.EQ.1) ) THEN
  499                   DO JSEA=1, NSEAL
  500                     CALL INIT_GET_ISEA(ISEA, JSEA)
  501                     NREC   = ISEA + 2
  502                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  503                     WRITEBUFF(:) = 0.
  504                     WRITEBUFF(1:NSPEC) = VA(1:NSPEC,JSEA)
  505                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  506                     END DO
  507 !
  508 ! I/O server version writing of spectra ( !/MPI )
  509 !
  510  
  511 !
  512                 END IF
  513 !
  514             END IF
  515         ELSE
  516 !
  517 ! Reading spectra
  518 !
  519           IF ( TYPE.EQ.'WIND' .OR. TYPE.EQ.'CALM' ) THEN
  520           ELSE
  521             IF (LPDLIB .and. (GTYPE.eq.UNGTYPE)) THEN
  522             ELSE
  523               VA = 0.
  524               DO JSEA=1, NSEAL
  525                 CALL INIT_GET_ISEA(ISEA, JSEA)
  526                 NREC   = ISEA + 2
  527                 RPOS   = 1_8 + LRECL*(NREC-1_8)
  528                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  529                          (VA(I,JSEA),I=1,NSPEC)
  530                 ENDDO
  531             END IF
  532           END IF
  533         END IF
  534               
  535 !AR: Must be checked better ... will do that when cleaning debugging swit>
  536         VA = MAX(0.,VA)
  537 !
  538 ! Water level etc. if required --------------------------------------- *
  539 !     ( For cold start write test output and cold start initialize
  540 !       water levels. Note that MAPSTA overwrites the one read from the
  541 !       model definition file, so that it need not be initialized. )
  542 !
  543       NREC   = NSEA + 3
  544       NPART  = 1 + (NSEA-1)/NSIZE
  545       NPRTX2 = 1 + (NX-1)/NSIZE
  546       NPRTY2 = 1 + (NY-1)/NSIZE
  547 !
  548       IF ( WRITE ) THEN
  549 !
  550           IF (TYPE.EQ.'FULL') THEN
 
 
                                                         w3iorsmd.F90  page  11
 
 
  551 !
  552               IF ( IAPROC .EQ. NAPRST ) THEN
  553 !
  554                   RPOS  = 1_8 + LRECL*(NREC-1_8)
  555                   WRITEBUFF(:) = 0.
  556                   WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  557                   WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)           &
  558                           TLEV, TICE
  559                   DO IPART=1,NPART
  560                     NREC  = NREC + 1
  561                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  562                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  563                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
  564                           (WLV(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
  565                                           MIN(NSEA,IPART*NSIZE))
  566                     END DO
  567                   DO IPART=1,NPART
  568                     NREC  = NREC + 1
  569                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  570                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  571                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
  572                           (ICE(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
  573                                           MIN(NSEA,IPART*NSIZE))
  574                   END DO
  575                         
  576                         
  577                   ALLOCATE ( MAPTMP(NY,NX) )
  578                   MAPTMP = MAPSTA + 8*MAPST2
  579                   DO IY=1, NY
  580                     DO IPART=1,NPRTX2
  581                       NREC  = NREC + 1
  582                       RPOS  = 1_8 + LRECL*(NREC-1_8)
  583                       WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)       &
  584                              WRITEBUFF
  585                       WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)       &
  586                             (MAPTMP(IY,IXL),IXL=1+(IPART-1)*NSIZE,    &
  587                                                 MIN(NX,IPART*NSIZE))
  588                       END DO
  589                     END DO
  590                   DEALLOCATE ( MAPTMP )
  591                   DO IPART=1,NPART
  592                     NREC  = NREC + 1
  593                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  594                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  595                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
  596                           (UST(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
  597                                           MIN(NSEA,IPART*NSIZE))
  598                     END DO
  599                   DO IPART=1,NPART
  600                     NREC  = NREC + 1
  601                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  602                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  603                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
  604                           (USTDIR(ISEA),ISEA=1+(IPART-1)*NSIZE,       &
  605                                           MIN(NSEA,IPART*NSIZE))
 
 
                                                         w3iorsmd.F90  page  12
 
 
  606                     END DO
  607                   DO IPART=1,NPART
  608                     NREC  = NREC + 1
  609                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  610                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  611                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
  612                           (ASF(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
  613                                           MIN(NSEA,IPART*NSIZE))
  614                     END DO
  615                   DO IPART=1,NPART
  616                     NREC  = NREC + 1
  617                     RPOS  = 1_8 + LRECL*(NREC-1_8)
  618                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
  619                     WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
  620                           (FPIS(ISEA),ISEA=1+(IPART-1)*NSIZE,         &
  621                                           MIN(NSEA,IPART*NSIZE))
  622                     END DO
  623                 IF (OARST) THEN
  624 !
  625                   IF ( FLOGRR(1,2) ) THEN
  626                     WRITE(NDSR,ERR=803,IOSTAT=IERR) CX(1:NSEA)
  627                     WRITE(NDSR,ERR=803,IOSTAT=IERR) CY(1:NSEA)
  628                   ENDIF
  629                   IF ( FLOGRR(1,9) )                                  &
  630                     WRITE(NDSR,ERR=803,IOSTAT=IERR) ICEF(1:NSEA)
  631                   IF ( FLOGRR(2,1) )                                  &
  632                     WRITE(NDSR,ERR=803,IOSTAT=IERR) HS(1:NSEA)
  633                   IF ( FLOGRR(2,2) )                                  &
  634                     WRITE(NDSR,ERR=803,IOSTAT=IERR) WLM(1:NSEA)
  635                   IF ( FLOGRR(2,4) )                                  &
  636                     WRITE(NDSR,ERR=803,IOSTAT=IERR) T0M1(1:NSEA)
  637                   IF ( FLOGRR(2,6) )                                  &
  638                     WRITE(NDSR,ERR=803,IOSTAT=IERR) FP0(1:NSEA)
  639                   IF ( FLOGRR(2,7) )                                  &
  640                     WRITE(NDSR,ERR=803,IOSTAT=IERR) THM(1:NSEA)
  641                   IF ( FLOGRR(5,2) )                                  &
  642                     WRITE(NDSR,ERR=803,IOSTAT=IERR) CHARN(1:NSEA)
  643                   IF ( FLOGRR(5,5) ) THEN
  644                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUWIX(1:NSEA)
  645                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUWIY(1:NSEA)
  646                   ENDIF
  647                   IF ( FLOGRR(5,11) )                                 &
  648                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TWS(1:NSEA)
  649                   IF ( FLOGRR(6,2) ) THEN
  650                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUOX(1:NSEA)
  651                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUOY(1:NSEA)
  652                   ENDIF
  653                   IF ( FLOGRR(6,3) )                                  &
  654                     WRITE(NDSR,ERR=803,IOSTAT=IERR) BHD(1:NSEA)
  655                   IF ( FLOGRR(6,4) )                                  &
  656                     WRITE(NDSR,ERR=803,IOSTAT=IERR) PHIOC(1:NSEA)
  657                   IF ( FLOGRR(6,5) ) THEN
  658                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TUSX(1:NSEA)
  659                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TUSY(1:NSEA)
  660                   ENDIF
 
 
                                                         w3iorsmd.F90  page  13
 
 
  661                   IF ( FLOGRR(6,6) ) THEN
  662                     WRITE(NDSR,ERR=803,IOSTAT=IERR) USSX(1:NSEA)
  663                     WRITE(NDSR,ERR=803,IOSTAT=IERR) USSY(1:NSEA)
  664                   ENDIF
  665                   IF ( FLOGRR(6,10) ) THEN
  666                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUICE(1:NSEA,1)
  667                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUICE(1:NSEA,2)
  668                   ENDIF
  669                   IF ( FLOGRR(7,2) ) THEN
  670                     WRITE(NDSR,ERR=803,IOSTAT=IERR) UBA(1:NSEA)
  671                     WRITE(NDSR,ERR=803,IOSTAT=IERR) UBD(1:NSEA)
  672                   ENDIF
  673                   IF ( FLOGRR(7,4) )                                  &
  674                     WRITE(NDSR,ERR=803,IOSTAT=IERR) PHIBBL(1:NSEA)
  675                   IF ( FLOGRR(7,5) ) THEN
  676                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUBBL(1:NSEA,1)
  677                     WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUBBL(1:NSEA,2)
  678                   ENDIF
  679 !
  680                 ENDIF
  681               END IF
  682           END IF
  683       ELSE
  684           IF (TYPE.EQ.'FULL') THEN
  685               RPOS = 1_8 + LRECL*(NREC-1_8)
  686               READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)                &
  687                       TLEV, TICE
  688               DO IPART=1,NPART
  689                 NREC  = NREC + 1
  690                 RPOS = 1_8 + LRECL*(NREC-1_8)
  691                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  692                       (WLV(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
  693                                       MIN(NSEA,IPART*NSIZE))
  694                 END DO
  695               DO IPART=1,NPART
  696                 NREC  = NREC + 1
  697                 RPOS = 1_8 + LRECL*(NREC-1_8)
  698                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  699                       (ICE(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
  700                                       MIN(NSEA,IPART*NSIZE))
  701               END DO
  702               ALLOCATE ( MAPTMP(NY,NX) )
  703               DO IY=1, NY
  704                 DO IPART=1,NPRTX2
  705                   NREC  = NREC + 1
  706                   RPOS  = 1_8 + LRECL*(NREC-1_8)
  707                   READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)            &
  708                         (MAPTMP(IY,IXL),IXL=1+(IPART-1)*NSIZE,        &
  709                                             MIN(NX,IPART*NSIZE))
  710                   END DO
  711                 END DO
  712               MAPSTA = MOD(MAPTMP+2,8) - 2
  713               MAPST2 = (MAPTMP-MAPSTA) / 8
  714               DEALLOCATE ( MAPTMP )
  715 !
 
 
                                                         w3iorsmd.F90  page  14
 
 
  716 ! Updates reflections maps:
  717 !
  718               IF (GTYPE.EQ.UNGTYPE) THEN
  719                 CALL SETUGIOBP
  720               ELSE
  721                 CALL W3SETREF
  722                 ENDIF
  723 !
  724               DO IPART=1,NPART
  725                 NREC  = NREC + 1
  726                 RPOS  = 1_8 + LRECL*(NREC-1_8)
  727                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  728                       (UST(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
  729                                       MIN(NSEA,IPART*NSIZE))
  730                 END DO
  731               DO IPART=1,NPART
  732                 NREC  = NREC + 1
  733                 RPOS  = 1_8 + LRECL*(NREC-1_8)
  734                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  735                       (USTDIR(ISEA),ISEA=1+(IPART-1)*NSIZE,           &
  736                                       MIN(NSEA,IPART*NSIZE))
  737                 END DO
  738               DO IPART=1,NPART
  739                 NREC  = NREC + 1
  740                 RPOS  = 1_8 + LRECL*(NREC-1_8)
  741                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  742                       (ASF(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
  743                                       MIN(NSEA,IPART*NSIZE))
  744                 END DO
  745               DO IPART=1,NPART
  746                 NREC  = NREC + 1
  747                 RPOS  = 1_8 + LRECL*(NREC-1_8)
  748                 READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
  749                       (FPIS(ISEA),ISEA=1+(IPART-1)*NSIZE,             &
  750                                       MIN(NSEA,IPART*NSIZE))
  751                 END DO
  752             IF (OARST) THEN
  753               IF ( FLOGRR(1,2) ) THEN
  754                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  755                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  756                 DO I=1, NSEALM
  757                   J = IAPROC + (I-1)*NAPROC
  758                   IF (J .LE. NSEA) THEN
  759                     CX(I) = TMP(J)
  760                     CY(I) = TMP2(J)
  761                   ENDIF
  762                 ENDDO
  763               ENDIF
  764               IF ( FLOGRR(1,9) ) THEN
  765                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  766                 DO I=1, NSEALM
  767                   J = IAPROC + (I-1)*NAPROC
  768                   IF (J .LE. NSEA) ICEF(I) = TMP(J)
  769                 ENDDO
  770               ENDIF
 
 
                                                         w3iorsmd.F90  page  15
 
 
  771               IF ( FLOGRR(2,1) ) THEN
  772                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  773                 DO I=1, NSEALM
  774                   J = IAPROC + (I-1)*NAPROC
  775                   IF (J .LE. NSEA) HS(I) = TMP(J)
  776                 ENDDO
  777               ENDIF
  778               IF ( FLOGRR(2,2) ) THEN
  779                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  780                 DO I=1, NSEALM
  781                   J = IAPROC + (I-1)*NAPROC
  782                   IF (J .LE. NSEA) WLM(I) = TMP(J)
  783                 ENDDO
  784               ENDIF
  785               IF ( FLOGRR(2,4) ) THEN
  786                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  787                 DO I=1, NSEALM
  788                   J = IAPROC + (I-1)*NAPROC
  789                   IF (J .LE. NSEA) THEN
  790                     T0M1(I) = TMP(J)
  791                   ENDIF
  792                 ENDDO
  793               ENDIF
  794               IF ( FLOGRR(2,6) ) THEN
  795                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  796                 DO I=1, NSEALM
  797                   J = IAPROC + (I-1)*NAPROC
  798                   IF (J .LE. NSEA) FP0(I) = TMP(J)
  799                 ENDDO
  800               ENDIF
  801               IF ( FLOGRR(2,7) ) THEN
  802                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  803                 DO I=1, NSEALM
  804                   J = IAPROC + (I-1)*NAPROC
  805                   IF (J .LE. NSEA) THM(I) = TMP(J)
  806                 ENDDO
  807               ENDIF
  808               IF ( FLOGRR(5,2) ) THEN
  809                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  810                 DO I=1, NSEALM
  811                   J = IAPROC + (I-1)*NAPROC
  812                   IF (J .LE. NSEA) CHARN(I) = TMP(J)
  813                 ENDDO
  814               ENDIF
  815               IF ( FLOGRR(5,5) ) THEN
  816                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  817                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  818                 DO I=1, NSEALM
  819                   J = IAPROC + (I-1)*NAPROC
  820                   IF (J .LE. NSEA) THEN
  821                     TAUWIX(I) = TMP(J)
  822                     TAUWIY(I) = TMP2(J)
  823                   ENDIF
  824                 ENDDO
  825               ENDIF
 
 
                                                         w3iorsmd.F90  page  16
 
 
  826               IF ( FLOGRR(5,11) ) THEN
  827                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  828                 DO I=1, NSEALM
  829                   J = IAPROC + (I-1)*NAPROC
  830                   IF (J .LE. NSEA) TWS(I) = TMP(J)
  831                 ENDDO
  832               ENDIF
  833               IF ( FLOGRR(6,2) ) THEN
  834                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  835                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  836                 DO I=1, NSEALM
  837                   J = IAPROC + (I-1)*NAPROC
  838                   IF (J .LE. NSEA) THEN
  839                     TAUOX(I) = TMP(J)
  840                     TAUOY(I) = TMP2(J)
  841                   ENDIF
  842                 ENDDO
  843               ENDIF
  844               IF ( FLOGRR(6,3) ) THEN
  845                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  846                 DO I=1, NSEALM
  847                   J = IAPROC + (I-1)*NAPROC
  848                   IF (J .LE. NSEA) BHD(I) = TMP(J)
  849                 ENDDO
  850               ENDIF
  851               IF ( FLOGRR(6,4) ) THEN
  852                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  853                 DO I=1, NSEALM
  854                   J = IAPROC + (I-1)*NAPROC
  855                   IF (J .LE. NSEA) PHIOC(I) = TMP(J)
  856                 ENDDO
  857               ENDIF
  858               IF ( FLOGRR(6,5) ) THEN
  859                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  860                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  861                 DO I=1, NSEALM
  862                   J = IAPROC + (I-1)*NAPROC
  863                   IF (J .LE. NSEA) THEN
  864                     TUSX(I) = TMP(J)
  865                     TUSY(I) = TMP2(J)
  866                   ENDIF
  867                 ENDDO
  868               ENDIF
  869               IF ( FLOGRR(6,6) ) THEN
  870                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  871                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  872                 DO I=1, NSEALM
  873                   J = IAPROC + (I-1)*NAPROC
  874                   IF (J .LE. NSEA) THEN
  875                     USSX(I) = TMP(J)
  876                     USSY(I) = TMP2(J)
  877                   ENDIF
  878                 ENDDO
  879               ENDIF
  880               IF ( FLOGRR(6,10) ) THEN
 
 
                                                         w3iorsmd.F90  page  17
 
 
  881                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  882                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  883                 DO I=1, NSEALM
  884                   J = IAPROC + (I-1)*NAPROC
  885                   IF (J .LE. NSEA) THEN
  886                     TAUICE(I,1) = TMP(J)
  887                     TAUICE(I,2) = TMP2(J)
  888                   ENDIF
  889                 ENDDO
  890               ENDIF
  891               IF ( FLOGRR(7,2) ) THEN
  892                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  893                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  894                 DO I=1, NSEALM
  895                   J = IAPROC + (I-1)*NAPROC
  896                   IF (J .LE. NSEA) THEN
  897                     UBA(I) = TMP(J)
  898                     UBD(I) = TMP2(J)
  899                   ENDIF
  900                 ENDDO
  901               ENDIF
  902               IF ( FLOGRR(7,4) ) THEN
  903                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  904                 DO I=1, NSEALM
  905                   J = IAPROC + (I-1)*NAPROC
  906                   IF (J .LE. NSEA) PHIBBL(I) = TMP(J)
  907                 ENDDO
  908               ENDIF
  909               IF ( FLOGRR(7,5) ) THEN
  910                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
  911                 READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
  912                 DO I=1, NSEALM
  913                   J = IAPROC + (I-1)*NAPROC
  914                   IF (J .LE. NSEA) THEN
  915                     TAUBBL(I,1) = TMP(J)
  916                     TAUBBL(I,2) = TMP2(J)
  917                   ENDIF
  918                 ENDDO
  919               ENDIF
  920             ENDIF
  921           ELSE
  922               TLEV(1) = -1
  923               TLEV(2) =  0
  924               TICE(1) = -1
  925               TICE(2) =  0
  926               TIC1(1) = -1
  927               TIC1(2) =  0
  928               TIC5(1) = -1
  929               TIC5(2) =  0
  930               WLV     =  0.
  931               ICE     =  0.
  932               ASF     =  1.
  933               FPIS    =  DUMFPI
  934                                
  935             ! Initialize coupled fields if no restart is present
 
 
                                                         w3iorsmd.F90  page  18
 
 
  936             IF (OARST) THEN
  937               CX      = 0.
  938               CY      = 0.
  939               ICEF    = 0.
  940               HS      = 0.
  941               WLM     = 0.
  942               T0M1    = 0.
  943               FP0     = 1.
  944               THM     = 0.
  945               CHARN   = 0.0185
  946               TAUWIX  = 0.
  947               TAUWIY  = 0.
  948               TWS     = 0.
  949               TAUOX   = 0.
  950               TAUOY   = 0.
  951               BHD     = 0.
  952               PHIOC   = 0.
  953               TUSX    = 0.
  954               TUSY    = 0.
  955               USSX    = 0.
  956               USSY    = 0.
  957               TAUICE  = 0.
  958               UBA     = 0.
  959               UBD     = 0.
  960               PHIBBL  = 0.
  961               TAUBBL  = 0.
  962             ENDIF
  963           END IF
  964         END IF
  965 !
  966 ! Close file --------------------------------------------------------- *
  967 !
  968       IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST ) THEN
  969         CLOSE ( NDSR )
  970       END IF
  971 !
  972       IF (ALLOCATED(WRITEBUFF)) DEALLOCATE(WRITEBUFF)
  973       IF (ALLOCATED(TMP))  DEALLOCATE(TMP)
  974       IF (ALLOCATED(TMP2)) DEALLOCATE(TMP2)
  975 !
  976       RETURN
  977 !
  978 ! Escape locations read errors :
  979 !
  980   800 CONTINUE
  981       TYPE   = 'CALM'
  982       RSTYPE = 4
  983       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,990) TYPE, IERR
  984       GOTO 100
  985 !
  986   801 CONTINUE
  987       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,991)
  988       CALL EXTCDE ( 30 )
  989 !
  990   802 CONTINUE
 
 
                                                         w3iorsmd.F90  page  19
 
 
  991       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,992) IERR
  992       CALL EXTCDE ( 31 )
  993 !
  994   803 CONTINUE
  995       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,993) IERR, RPOS
  996       CALL EXTCDE ( 31 )
  997 !
  998 ! Formats
  999 !
 1000   900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
 1001                '     ILLEGAL INXOUT VALUE: ',A/)
 1002   901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
 1003                '     ILLEGAL IDSTR, READ : ',A/                       &
 1004                '                   CHECK : ',A/)
 1005   902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
 1006                '     ILLEGAL VERINI, READ : ',A/                      &
 1007                '                    CHECK : ',A/)
 1008   903 FORMAT (/' *** WAVEWATCH III WARNING IN W3IORS :'/              &
 1009                '     ILLEGAL GNAME, READ : ',A/                       &
 1010                '                   CHECK : ',A/)
 1011   904 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
 1012                '     ILLEGAL TYPE : ',A/)
 1013   905 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
 1014                '     CONFLICTING NSPEC, NSEA GRID : ',2I8/            &
 1015                '                         EXPECTED : ',2I8/)
 1016   906 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
 1017                '     CONFLICTING TIMES: FILE : ',I10.8,I8.6/          &
 1018                '                       MODEL : ',I10.8,I8.6/)
 1019 !
 1020   990 FORMAT (/' *** WAVEWATCH III WARNING IN W3IORS : '/             &
 1021                '     NO READABLE RESTART FILE, ',                     &
 1022                     'INITIALIZE WITH ''',A,''' INSTEAD'/              &
 1023                '     IOSTAT =',I5/)
 1024   991 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS : '/               &
 1025                '     PREMATURE END OF FILE'/)
 1026   992 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS : '/               &
 1027                '     ERROR IN READING FROM FILE'/                     &
 1028                '     IOSTAT =',I5/)
 1029   993 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS : '/               &
 1030                '     ERROR IN WRITING TO FILE'/                       &
 1031                '     IOSTAT =',I5,', POS =',I11 /)
 1032 !
 1033 !/
 1034 !/ End of W3IORS ----------------------------------------------------- /
 1035 !/
 1036       END SUBROUTINE W3IORS
 1037 !/
 1038 !/ End of module W3IORSMD -------------------------------------------- /
 1039 !/
 1040       END MODULE W3IORSMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
