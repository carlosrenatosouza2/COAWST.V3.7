                                                         w3iogrmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3IOGRMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  !            F. Ardhuin             !
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :         15-Apr-2020 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    For updates see W3IOGR documentation.
   14 !/
   15 !  1. Purpose :
   16 !
   17 !     Reading/writing of model definition file .
   18 !
   19 !  2. Variables and types :
   20 !
   21 !      Name      Type  Scope    Description
   22 !     ----------------------------------------------------------------
   23 !      VERGRD    C*10  Private  Model definition file version number.
   24 !      IDSTR     C*35  Private  Model definition file ID string.
   25 !     ----------------------------------------------------------------
   26 !
   27 !  3. Subroutines and functions :
   28 !
   29 !      Name      Type  Scope    Description
   30 !     ----------------------------------------------------------------
   31 !      W3IOGR    Subr. Public   Read/write model definition file.
   32 !     ----------------------------------------------------------------
   33 !
   34 !  4. Subroutines and functions used :
   35 !
   36 !      Name      Type  Module   Description
   37 !     ----------------------------------------------------------------
   38 !      W3SETG    Subr. W3GDATMD Point to data structure for spatial gr.
   39 !      W3DIMX    Subr.    Id.   Set up arrays for spatial grid.
   40 !      W3DIMS    Subr.    Id.   Set array dimensions for a spec. grid.
   41 !      W3SETO    Subr. W3ODATMD Point to data structure for spatial gr.
   42 !      W3DMO5    Subr.    Id.   Set array dimensions.
   43 !      INPTAB    Subr. W3SRC2MD Fill interpolation tables for
   44 !                               dispersion relation.
   45 !      DISTAB    Subr. W3DISPMD Input coefficient lookup table.
   46 !      INSNL1    Subr. W3SNL1MD Initialization of the DIA.
   47 !      INSNL2    Subr. W3SNL2MD Initialization of WRT.
   48 !      INSNL3    Subr. W3SNL3MD Initialization of GMD.
   49 !      INSNLX    Subr. W3SNLXMD Initialization of exp. DIA.
   50 !      INSNLS    Subr. W3SNLSMD Initialization of nonlinear `smoother'.
   51 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   52 !      EXTCDE    Subr. W3SERVMD Abort program with exit code.
   53 !     ----------------------------------------------------------------
   54 !
   55 !  5. Remarks :
 
 
                                                         w3iogrmd.F90  page   2
 
 
   56 !
   57 !     - Arrays allocated here on read or ing ww3_grid on write.
   58 !
   59 !  6. Switches :
   60 !
   61 !     See subroutine.
   62 !
   63 !  7. Source code :
   64 !
   65 !/ ------------------------------------------------------------------- /
   66       PUBLIC
   67 !/
   68 !/ Private parameter statements (ID strings)
   69 !/
   70       CHARACTER(LEN=10), PARAMETER, PRIVATE :: VERGRD = '2020-10-19'
   71       CHARACTER(LEN=35), PARAMETER, PRIVATE ::                        &
   72                          IDSTR = 'WAVEWATCH III MODEL DEFINITION FILE'
   73 !/
   74 !/ Public variables
   75 !/
   76 !/
   77       CONTAINS
   78 !/ ------------------------------------------------------------------- /
   79       SUBROUTINE W3IOGR ( INXOUT, NDSM, IMOD, FEXT )
   80 !/
   81 !/                  +-----------------------------------+
   82 !/                  | WAVEWATCH III           NOAA/NCEP |
   83 !/                  |           H. L. Tolman            |
   84 !/                  !            F. Ardhuin             !
   85 !/                  |                        FORTRAN 90 |
   86 !/                  | Last update :         19-Oct-2020 |
   87 !/                  +-----------------------------------+
   88 !/
   89 !/    14-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
   90 !/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
   91 !/                  Major changes to logistics.
   92 !/    14-Feb-2000 : Exact-NL added.                     ( version 2.01 )
   93 !/    09-Jan-2001 : Flat grid option.                   ( version 2.06 )
   94 !/    02-Feb-2001 : Exact-NL version 3.0                ( version 2.07 )
   95 !/    27-Feb-2001 : Third propagation scheme added.     ( version 2.08 )
   96 !/    16-Mar-2001 : Fourth propagation scheme added.    ( version 2.09 )
   97 !/    29-Mar-2001 : Sub-grid islands added.             ( version 2.10 )
   98 !/    11-Jan-2002 : Sub-grid ice added.                 ( version 2.15 )
   99 !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
  100 !/    27-Aug-2002 : Exact-NL version 4.0                ( version 2.22 )
  101 !/    26-Nov-2002 : Adding first VDIA and MDIA.         ( version 3.01 )
  102 !/    01-Aug-2003 : Adding moving grid GSE correction.  ( version 3.03 )
  103 !/    08-Mar-2004 : Multiple grid version.              ( version 3.06 )
  104 !/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
  105 !/    24-Jun-2005 : Add MAPST2 processing.              ( version 3.07 )
  106 !/    09-Nov-2005 : Remove soft boundary options.       ( version 3.08 )
  107 !/    23-Jun-2006 : Add W3SLN1 parameters.              ( version 3.09 )
  108 !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
  109 !/    25-Jul-2006 : Reorder for 'GRID' option to read   ( version 3.10 )
  110 !/                  spectral data also.
 
 
                                                         w3iogrmd.F90  page   3
 
 
  111 !/    28-Oct-2006 : Add partitioning pars.              ( version 3.10 )
  112 !/    26-Mar-2007 : Add partitioning pars.              ( version 3.11 )
  113 !/    16-Apr-2006 : Add Miche limiter pars.             ( version 3.11 )
  114 !/    25-Apr-2007 : Adding Battjes-Janssen Sdb.         ( version 3.11 )
  115 !/    09-Oct-2007 : Adding WAM cycle 4+ Sin and Sds.    ( version 3.13 )
  116 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  117 !/    30-Oct-2009 : Fix ndst arg in call to w3dmo5.     ( version 3.14 )
  118 !/                  (T. J. Campbell, NRL)
  119 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
  120 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  121 !/    23-Dec-2009 : Addition of COU namelists           ( version 3.14 )
  122 !/    31-Oct-2010 : Implement unstructured grids        ( version 3.14 )
  123 !/                  (A. Roland and F. Ardhuin)
  124 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
  125 !/                  specify index closure for a grid.   ( version 3.14 )
  126 !/                  (T. J. Campbell, NRL)
  127 !/    12-Jun-2012 : Add /RTD option or rotated grid option.
  128 !/                  (Jian-Guo Li)                       ( version 4.06 )
  129 !/    13-Jul-2012 : Move GMD (SNL3) and nonlinear filter (SNLS)
  130 !/                  from 3.15 (HLT).                    ( version 4.08 )
  131 !/    12-Dec-2012 : Adding SMC grid.  JG_Li             ( version 4.08 )
  132 !/    19-Dec-2012 : Add NOSWLL to file.                 ( version 4.11 )
  133 !/    01-Jul-2013 : Document UQ / UNO switches in file  ( version 4.12 )
  134 !/    10-Sep-2013 : Add IG1 parameters                  ( version 4.12 )
  135 !/    16-Sep-2013 : Add Arctic part in SMC grid.        ( version 4.12 )
  136 !/    11-Nov-2013 : Make SMC and RTD grids compatible.  ( version 4.13 )
  137 !/    06-Mar-2014 : Writes out a help message on error  ( version 4.18 )
  138 !/    10-Mar-2014 : Add IC2 parameters                  ( version 5.01 )
  139 !/    29-May-2014 : Add IC3 parameters                  ( version 5.01 )
  140 !/    20-Aug-2016 : Add IOBPA                           ( version 5.12 )
  141 !/    08-Mar-2018 : Add FSWND for SMC grid.             ( version 6.02 )
  142 !/    05-Jun-2018 : Add PDLIB/DEBUGINIT and implcit scheme parameters
  143 !/                  for unstructured grids              ( version 6.04 )
  144 !/    27-Jul-2018 : Added PTMETH and PTFCUT parameters  ( version 6.05 )
  145 !/                  (C. Bunney, UKMO)
  146 !/    18-Aug-2018 : S_{ice} IC5 (Q. Liu)                ( version 6.06 )
  147 !/    26-Aug-2018 : UOST (Mentaschi et al. 2015, 2018)  ( version 6.06 )
  148 !/    15-Apr-2020 : Adds optional opt-out for CFL on BC ( version 7.08 )
  149 !/    18-Jun-2020 : Adds 360-day calendar option        ( version 7.08 )
  150 !/    19-Oct-2020 : Add AIRCMIN, AIRGB parameters       ( version 7.08 )
  151 !/
  152 !/    Copyright 2009-2013 National Weather Service (NWS),
  153 !/       National Oceanic and Atmospheric Administration.  All rights
  154 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  155 !/       No unauthorized use without permission.
  156 !/
  157 !  1. Purpose :
  158 !
  159 !     Reading and writing of the model definition file.
  160 !
  161 !  2. Method :
  162 !
  163 !     The file is opened within the routine, the name is pre-defined
  164 !     and the unit number is given in the parameter list. The model
  165 !     definition file is written using UNFORMATTED write statements.
 
 
                                                         w3iogrmd.F90  page   4
 
 
  166 !
  167 !  3. Parameters :
  168 !
  169 !     Parameter list
  170 !     ----------------------------------------------------------------
  171 !       INXOUT  C*(*)  I   Test string for read/write, valid are:
  172 !                         'READ',  'WRITE' and 'GRID'.
  173 !       NDSM    Int.   I   File unit number.
  174 !       IMOD    Int.   I   Model number for W3GDAT etc.
  175 !       FEXT    C*(*)  I   File extension to be used.
  176 !     ----------------------------------------------------------------
  177 !
  178 !  4. Subroutines used :
  179 !
  180 !     See above.
  181 !
  182 !  5. Called by :
  183 !
  184 !      Name      Type  Module   Description
  185 !     ----------------------------------------------------------------
  186 !      W3INIT    Subr. W3INITMD Wave model initialization routine.
  187 !      ......    Prog.   N/A    All WAVEWATCH III aux programs and
  188 !                               drivers.
  189 !     ----------------------------------------------------------------
  190 !
  191 !  6. Error messages :
  192 !
  193 !       Tests on INXOUT, file status and on array dimensions.
  194 !
  195 !  7. Remarks :
  196 !
  197 !     - The model definition file has the pre-defined name
  198 !       'mod_def.FILEXT'.
  199 !
  200 !  8. Structure :
  201 !
  202 !     See source code.
  203 !
  204 !  9. Switches :
  205 !
  206 !     !/MPI  MPI calls
  207 !
  208 !     !/LNn  Select source terms
  209 !     !/STn
  210 !     !/NLn
  211 !     !/BTn
  212 !     !/DBn
  213 !     !/TRn
  214 !     !/BSn
  215 !     !/XXn
  216 !
  217 !     !/S    Enable subroutine tracing.
  218 !     !/T    Enable test output
  219 !
  220 ! 10. Source code :
 
 
                                                         w3iogrmd.F90  page   5
 
 
  221 !
  222 !/ ------------------------------------------------------------------- /
  223       USE CONSTANTS
  224       USE W3GDATMD
  225       USE W3ADATMD, ONLY: MPI_COMM_WAVE
  226       USE W3ODATMD
  227       USE W3SRC4MD, ONLY: INSIN4, TAUT, TAUHFT, TAUHFT2, &
  228                           DELU, DELTAUW, DELUST, &
  229                           DELALP, DELTAIL, &
  230                           DIKCUMUL
  231       USE W3SNL1MD, ONLY: INSNL1
  232       USE W3TIMEMD, ONLY: CALTYPE
  233       USE W3SERVMD, ONLY: EXTCDE
  234       USE W3DISPMD
  235 !
  236       IMPLICIT NONE
  237 !
  238       INCLUDE "mpif.h"
  239 !/
  240 !/ ------------------------------------------------------------------- /
  241 !/ Parameter list
  242 !/
  243       INTEGER, INTENT(IN)             :: NDSM
  244       INTEGER, INTENT(IN), OPTIONAL   :: IMOD
  245       CHARACTER, INTENT(IN)           :: INXOUT*(*)
  246       CHARACTER, INTENT(IN), OPTIONAL :: FEXT*(*)
  247 !/
  248 !/ ------------------------------------------------------------------- /
  249 !/ Local parameters
  250 !/
  251       INTEGER                 :: IGRD, IERR, I, J, MTH, MK, ISEA, IX, IY
  252  INTEGER                 :: IK, ITH, IK2, ITH2
  253       INTEGER, ALLOCATABLE    :: MAPTMP(:,:)
  254       INTEGER                 :: IERR_MPI, IP
  255       LOGICAL                 :: WRITE, FLTEST = .FALSE., TESTLL,     &
  256                                  FLSNL2 = .FALSE.
  257       LOGICAL, SAVE           :: FLINP = .FALSE. , FLDISP = .FALSE.,  &
  258                                  FLIS  = .FALSE.
  259       CHARACTER(LEN=10)       :: VERTST
  260       CHARACTER(LEN=13)       :: TEMPXT
  261       CHARACTER(LEN=30)       :: TNAME0, TNAME1, TNAME2, TNAME3,      &
  262                                  TNAME4, TNAME5, TNAME6, TNAME7,      &
  263                                  TNAMEP, TNAMEG, TNAMEF, TNAMEI
  264       CHARACTER(LEN=30)       :: FNAME0, FNAME1, FNAME2, FNAME3,      &
  265                                  FNAME4, FNAME5, FNAME6, FNAME7,      &
  266                                  FNAMEP, FNAMEG, FNAMEF, FNAMEI
  267       CHARACTER(LEN=35)       :: IDTST
  268       CHARACTER(LEN=60)      :: MESSAGE(5)
  269       LOGICAL                 :: GLOBAL
  270 !/
  271 !/ ------------------------------------------------------------------- /
  272 !/
  273 !
  274  
  275  
 
 
                                                         w3iogrmd.F90  page   6
 
 
  276       MESSAGE =(/ '     MOD DEF FILE WAS GENERATED WITH A DIFFERENT    ',>
  277                   '     WW3 VERSION OR USING A DIFFERENT SWITCH FILE.  ',>
  278                   '     MAKE SURE WW3_GRID IS COMPILED WITH SAME SWITCH',>
  279                   '     AS WW3_SHEL OR WW3_MULTI, RUN WW3_GRID AGAIN   ',>
  280                   '     AND THEN TRY AGAIN THE PROGRAM YOU JUST USED.  '/)
  281 !
  282       TNAMEF = '------------------------------'
  283       TNAME0 = '------------------------------'
  284       TNAME1 = '------------------------------'
  285       TNAME2 = '------------------------------'
  286       TNAME3 = '------------------------------'
  287       TNAME4 = '------------------------------'
  288       TNAME5 = '------------------------------'
  289       TNAME6 = '------------------------------'
  290       TNAME7 = '------------------------------'
  291       TNAMEP = '------------------------------'
  292       TNAMEG = '------------------------------'
  293       TNAMEI = '------------------------------'
  294 !
  295       TNAME0 = 'Cavaleri and M.-R. (1982)     '
  296       TNAME1 = 'Ardhuin et al. (2009+)        '
  297       TNAME2 = 'Discrete Interaction Approx.  '
  298       TNAME3 = 'Not defined                   '
  299       TNAME4 = 'Not defined                   '
  300       TNAME5 = 'Not defined                   '
  301       TNAME6 = 'Not defined                   '
  302       TNAME7 = 'Not defined                   '
  303       TNAMEP = '3rd order UQ scheme           '
  304       TNAMEG = 'Averaging operator            '
  305 !
  306       FNAMEF = TNAMEF
  307       FNAME0 = TNAME0
  308       FNAME1 = TNAME1
  309       FNAME2 = TNAME2
  310       FNAME3 = TNAME3
  311       FNAME4 = TNAME4
  312       FNAME5 = TNAME5
  313       FNAME6 = TNAME6
  314       FNAME7 = TNAME7
  315       FNAMEP = TNAMEP
  316       FNAMEG = TNAMEG
  317       FNAMEI = TNAMEI
  318 !
  319 ! test input parameters ---------------------------------------------- *
  320 !
  321       IF ( PRESENT(IMOD) ) THEN
  322           IGRD   = IMOD
  323         ELSE
  324           IGRD   = 1
  325         END IF
  326 !
  327       IF ( PRESENT(FEXT) ) THEN
  328           TEMPXT = FEXT
  329         ELSE
  330           TEMPXT = 'ww3'
 
 
                                                         w3iogrmd.F90  page   7
 
 
  331         END IF
  332 !
  333       IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE'                    &
  334                            .AND. INXOUT.NE.'GRID') THEN
  335           IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,900) INXOUT
  336           CALL EXTCDE ( 1 )
  337         END IF
  338 !
  339       WRITE  = INXOUT .EQ. 'WRITE'
  340 !
  341       CALL W3SETO ( IGRD, NDSE, NDST )
  342       CALL W3SETG ( IGRD, NDSE, NDST )
  343       FILEXT = TEMPXT
  344 !
  345 ! open file ---------------------------------------------------------- *
  346 !
  347       I      = LEN_TRIM(FILEXT)
  348       J      = LEN_TRIM(FNMPRE)
  349 !
  350 !AR: ADD DEBUGFLAG      WRITE(*,*) 'FILE=', FNMPRE(:J)//'mod_def.'//FILEX>
  351       IF ( WRITE ) THEN
  352           OPEN (NDSM,FILE=FNMPRE(:J)//'mod_def.'//FILEXT(:I),         &
  353                 FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
  354         ELSE
  355           OPEN (NDSM,FILE=FNMPRE(:J)//'mod_def.'//FILEXT(:I),         &
  356                 FORM='UNFORMATTED',STATUS='OLD',ERR=800,IOSTAT=IERR)
  357         ENDIF
  358 !
  359       REWIND ( NDSM )
  360 !
  361 ! Dimensions and test information --------------------------------------
  362 !
  363       IF ( WRITE ) THEN
  364           WRITE (NDSM)                                                &
  365                 IDSTR, VERGRD, NX, NY, NSEA, NTH, NK,                 &
  366                 NBI, NFBPO, GNAME, FNAME0, FNAME1, FNAME2, FNAME3,    &
  367                 FNAME4, FNAME5, FNAME6, FNAME7, FNAMEP, FNAMEG,       &
  368                 FNAMEF, FNAMEI
  369 !
  370           WRITE (NDSM)                                                &
  371                 (NBO(I),I=0,NFBPO), (NBO2(I),I=0,NFBPO)
  372         ELSE
  373           READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
  374                 IDTST, VERTST, NX, NY, NSEA, MTH, MK,                 &
  375                 NBI, NFBPO, GNAME, FNAME0, FNAME1, FNAME2, FNAME3,    &
  376                 FNAME4, FNAME5, FNAME6, FNAME7, FNAMEP, FNAMEG,       &
  377                 FNAMEF, FNAMEI
  378 !
  379           NK     = MK
  380           NTH    = MTH
  381           NK2    = NK + 2
  382           NSPEC  = NK * NTH
  383 !
  384           IF ( IDTST .NE. IDSTR ) THEN
  385               IF ( IAPROC .EQ. NAPERR )                               &
 
 
                                                         w3iogrmd.F90  page   8
 
 
  386                   WRITE (NDSE,901) IDTST, IDSTR
  387               CALL EXTCDE ( 10 )
  388             END IF
  389           IF ( VERTST .NE. VERGRD ) THEN
  390               IF ( IAPROC .EQ. NAPERR )                               &
  391                   WRITE (NDSE,902) VERTST, VERGRD
  392               CALL EXTCDE ( 11 )
  393             END IF
  394           IF ( NFBPO .GT. 9 ) THEN
  395               IF ( IAPROC .EQ. NAPERR )                               &
  396                   WRITE (NDSE,904) NFBPO, 9
  397               CALL EXTCDE ( 13 )
  398             END IF
  399           IF ( FNAME0 .NE. TNAME0 ) THEN
  400                IF ( IAPROC .EQ. NAPERR )                              &
  401                   WRITE (NDSE,905) 0, FILEXT(:I), FNAME0, TNAME0,     &
  402                                    MESSAGE
  403                CALL EXTCDE ( 14 )
  404             END IF
  405           IF ( FNAME1 .NE. TNAME1 ) THEN
  406                IF ( IAPROC .EQ. NAPERR )                              &
  407                   WRITE (NDSE,905) 1, FILEXT(:I), FNAME1, TNAME1,     &
  408                                    MESSAGE
  409                CALL EXTCDE ( 15 )
  410             END IF
  411           IF ( FNAME2 .NE. TNAME2 ) THEN
  412                IF ( IAPROC .EQ. NAPERR )                              &
  413                   WRITE (NDSE,905) 2, FILEXT(:I), FNAME2, TNAME2,     &
  414                                    MESSAGE
  415                CALL EXTCDE ( 16 )
  416             END IF
  417           IF ( FNAME3 .NE. TNAME3 ) THEN
  418                IF ( IAPROC .EQ. NAPERR )                              &
  419                   WRITE (NDSE,905) 3, FILEXT(:I), FNAME3, TNAME3,     &
  420                                    MESSAGE
  421                CALL EXTCDE ( 17 )
  422             END IF
  423           IF ( FNAMEI .NE. TNAMEI ) THEN
  424                IF ( IAPROC .EQ. NAPERR )                              &
  425                   WRITE (NDSE,905) 3, FILEXT(:I), FNAMEI, TNAMEI,     &
  426                                    MESSAGE
  427                CALL EXTCDE ( 17 )
  428             END IF
  429           IF ( FNAME4 .NE. TNAME4 ) THEN
  430                IF ( IAPROC .EQ. NAPERR )                              &
  431                   WRITE (NDSE,905) 4, FILEXT(:I), FNAME4, TNAME4,     &
  432                                    MESSAGE
  433                CALL EXTCDE ( 18 )
  434             END IF
  435           IF ( FNAME5 .NE. TNAME5 ) THEN
  436                IF ( IAPROC .EQ. NAPERR )                              &
  437                   WRITE (NDSE,905) 5, FILEXT(:I), FNAME5, TNAME5,     &
  438                                    MESSAGE
  439                CALL EXTCDE ( 19 )
  440             END IF
 
 
                                                         w3iogrmd.F90  page   9
 
 
  441           IF ( FNAME6 .NE. TNAME6 ) THEN
  442                IF ( IAPROC .EQ. NAPERR )                              &
  443                   WRITE (NDSE,905) 6, FILEXT(:I), FNAME6, TNAME6,     &
  444                                    MESSAGE
  445                CALL EXTCDE ( 20 )
  446             END IF
  447           IF ( FNAME7 .NE. TNAME7 ) THEN
  448                IF ( IAPROC .EQ. NAPERR )                              &
  449                   WRITE (NDSE,905) 7, FILEXT(:I), FNAME7, TNAME7,     &
  450                                    MESSAGE
  451                CALL EXTCDE ( 21 )
  452             END IF
  453           IF ( FNAMEP .NE. TNAMEP ) THEN
  454                IF ( IAPROC .EQ. NAPERR )                              &
  455                   WRITE (NDSE,906) FNAMEP, TNAMEP
  456                CALL EXTCDE ( 22 )
  457             END IF
  458           IF ( FNAMEG .NE. TNAMEG ) THEN
  459                IF ( IAPROC .EQ. NAPERR )                              &
  460                   WRITE (NDSE,907) FNAMEG, TNAMEG, MESSAGE
  461                CALL EXTCDE ( 22 )
  462             END IF
  463           IF ( FNAMEF .NE. TNAMEF ) THEN
  464                IF ( IAPROC .EQ. NAPERR )                              &
  465                   WRITE (NDSE,908) FILEXT(:I), FNAMEF, TNAMEF, MESSAGE
  466                CALL EXTCDE ( 24 )
  467             END IF
  468 !
  469           READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
  470                 (NBO(I),I=0,NFBPO), (NBO2(I),I=0,NFBPO)
  471 !
  472         ENDIF
  473              
  474 !
  475 ! Parameters in modules  --------------------------------------------- *
  476 !                                                   Module W3GDAT GRID
  477 !
  478       ALLOCATE ( MAPTMP(NY,NX) )
  479 !
  480       IF ( WRITE ) THEN
  481           MAPTMP = MAPSTA + 8*MAPST2
  482           WRITE (NDSM)                                                &
  483                GTYPE, FLAGLL, ICLOSE
  484 !
  485 ! Writes different kind of information depending on grid type
  486 !
  487           SELECT CASE ( GTYPE )
  488             CASE ( RLGTYPE )
  489               WRITE (NDSM)                                            &
  490                    SX, SY, X0, Y0
  491             CASE ( CLGTYPE )
  492               WRITE (NDSM)                                            &
  493                    XGRD, YGRD
  494             CASE (UNGTYPE)
  495               WRITE (NDSM)                                            &
 
 
                                                         w3iogrmd.F90  page  10
 
 
  496                 FSN, FSPSI,FSFCT,FSNIMP,FSTOTALIMP,FSTOTALEXP,        &
  497                 FSBCCFL, FSREFRACTION, FSFREQSHIFT, FSSOURCE,         &
  498                 DO_CHANGE_WLV, SOLVERTHR_STP, CRIT_DEP_STP,           &
  499                 NTRI,COUNTOT, COUNTRI, NNZ,                           &
  500                 B_JGS_TERMINATE_MAXITER,                              &
  501                 B_JGS_TERMINATE_DIFFERENCE,                           &
  502                 B_JGS_TERMINATE_NORM,                                 &
  503                 B_JGS_LIMITER,                                        &
  504                 B_JGS_BLOCK_GAUSS_SEIDEL,                             &
  505                 B_JGS_USE_JACOBI,                                     &
  506                 B_JGS_MAXITER,                                        &
  507                 B_JGS_PMIN,                                           &
  508                 B_JGS_DIFF_THR,                                       &
  509                 B_JGS_NORM_THR,                                       &
  510                 B_JGS_NLEVEL,                                         &
  511                 B_JGS_SOURCE_NONLINEAR
  512               !Init COUNTCON to zero, it needs to be set somewhere or
  513               !removed
  514               COUNTCON=0
  515               WRITE (NDSM)                                            &
  516                 X0, Y0, SX, SY, DXYMAX, XYB, TRIGP, TRIA,             &
  517                 LEN, IEN, ANGLE0, ANGLE, SI, MAXX, MAXY,   &
  518                 DXYMAX, INDEX_CELL, CCON, COUNTCON, IE_CELL,  &
  519                 POS_CELL, IOBP, IOBPA, IOBDP, IOBPD, IAA, JAA, POSI
  520             END SELECT !GTYPE
  521 !
  522           WRITE (NDSM)                                                &
  523                ZB, MAPTMP, MAPFS, MAPSF, TRFLAG
  524 !
  525           IF ( TRFLAG .NE. 0 ) WRITE (NDSM) TRNX, TRNY
  526           WRITE (NDSM)                     &
  527                DTCFL, DTCFLI, DTMAX, DTMIN, DMIN, CTMAX,              &
  528                FICE0, FICEN, FICEL, PFMOVE, FLDRY, FLCX, FLCY, FLCTH, &
  529                FLCK, FLSOU, FLBPI, FLBPO, CLATS, CLATIS, CTHG0S,      &
  530                STEXU, STEYU, STEDU, IICEHMIN, IICEHINIT, IICEDISP,    &
  531                ICESCALES(1:4), CALTYPE, CMPRTRCK, IICEHFAC, IICEHDISP,&
  532                IICEDDISP, IICEFDISP, BTBETA,                          &
  533                AAIRCMIN, AAIRGB
  534                                
  535           WRITE(NDSM)GRIDSHIFT
  536 !!        WRITE(NDSM)                                                 &
  537 !!             COUG_2D, COUG_RAD3D, COUG_US3D
  538         ELSE
  539             
  540           READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
  541                GTYPE, FLAGLL, ICLOSE
  542 !!Li      IF (.NOT.GINIT) CALL W3DIMX ( IGRD, NX, NY, NSEA, NDSE, NDST )
  543           IF (.NOT.GINIT) CALL W3DIMX ( IGRD, NX, NY, NSEA, NDSE, NDST &
  544                                       )
  545 !
  546 ! Reads different kind of information depending on grid type
  547 !
  548           SELECT CASE ( GTYPE )
  549             CASE ( RLGTYPE )
  550               READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                 &
 
 
                                                         w3iogrmd.F90  page  11
 
 
  551                    SX, SY, X0, Y0
  552               DO IX=1,NX
  553                 XGRD(:,IX) = X0 + REAL(IX-1)*SX
  554                 END DO
  555               DO IY=1,NY
  556                 YGRD(IY,:) = Y0 + REAL(IY-1)*SY
  557                 END DO
  558             CASE ( CLGTYPE )
  559               READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                 &
  560                    XGRD, YGRD
  561               !Set SX, SY, X0, Y0 to large values if curvilinear grid
  562               X0 = HUGE(X0); Y0 = HUGE(Y0)
  563               SX = HUGE(SX); SY = HUGE(SY)
  564             CASE (UNGTYPE)
  565               READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                 &
  566                 FSN, FSPSI,FSFCT,FSNIMP,FSTOTALIMP,FSTOTALEXP,        &
  567                 FSBCCFL, FSREFRACTION, FSFREQSHIFT, FSSOURCE,         &
  568                 DO_CHANGE_WLV, SOLVERTHR_STP, CRIT_DEP_STP,           &
  569                 NTRI,COUNTOT, COUNTRI, NNZ,                           &
  570                 B_JGS_TERMINATE_MAXITER,                              &
  571                 B_JGS_TERMINATE_DIFFERENCE,                           &
  572                 B_JGS_TERMINATE_NORM,                                 &
  573                 B_JGS_LIMITER,                                        &
  574                 B_JGS_BLOCK_GAUSS_SEIDEL,                             &
  575                 B_JGS_USE_JACOBI,                                     &
  576                 B_JGS_MAXITER,                                        &
  577                 B_JGS_PMIN,                                           &
  578                 B_JGS_DIFF_THR,                                       &
  579                 B_JGS_NORM_THR,                                       &
  580                 B_JGS_NLEVEL,                                         &
  581                 B_JGS_SOURCE_NONLINEAR
  582               IF (.NOT. GUGINIT) THEN
  583                 CALL W3DIMUG ( IGRD, NTRI, NX, COUNTOT, NNZ, NDSE, NDST )
  584               END IF
  585               READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                 &
  586                 X0, Y0, SX, SY, DXYMAX, XYB, TRIGP, TRIA,             &
  587                 LEN, IEN, ANGLE0, ANGLE, SI, MAXX, MAXY,   &
  588                 DXYMAX, INDEX_CELL, CCON, COUNTCON, IE_CELL,  &
  589                 POS_CELL, IOBP, IOBPA, IOBDP, IOBPD, IAA, JAA, POSI
  590                                                                    
  591                                                                    
  592                 XGRD(1,:)=XYB(:,1)
  593                 YGRD(1,:)=XYB(:,2)
  594             END SELECT !GTYPE
  595 !
  596           IF (GTYPE.NE.UNGTYPE) CALL W3GNTX ( IGRD, NDSE, NDST )
  597           READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
  598                ZB, MAPTMP, MAPFS, MAPSF, TRFLAG
  599 !
  600           MAPSTA = MOD(MAPTMP+2,8) - 2
  601           MAPST2 = (MAPTMP-MAPSTA) / 8
  602           MAPSF(:,3) = MAPSF(:,2) + (MAPSF(:,1)-1)*NY
  603           IF ( TRFLAG .NE. 0 ) THEN
  604               READ (NDSM,END=801,ERR=802,IOSTAT=IERR) TRNX, TRNY
  605             END IF
 
 
                                                         w3iogrmd.F90  page  12
 
 
  606                   
  607           READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
  608                DTCFL, DTCFLI, DTMAX, DTMIN, DMIN, CTMAX,              &
  609                FICE0, FICEN, FICEL, PFMOVE, FLDRY, FLCX, FLCY,        &
  610                FLCTH, FLCK, FLSOU, FLBPI, FLBPO, CLATS, CLATIS,       &
  611                CTHG0S, STEXU, STEYU, STEDU, IICEHMIN, IICEHINIT,      &
  612                IICEDISP, ICESCALES(1:4), CALTYPE, CMPRTRCK, IICEHFAC, &
  613                IICEDDISP, IICEHDISP, IICEFDISP, BTBETA,               &
  614                AAIRCMIN, AAIRGB
  615                                
  616               READ(NDSM,END=801,ERR=802,IOSTAT=IERR)GRIDSHIFT
  617 !
  618         END IF
  619               
  620               
  621               
  622 !
  623       DEALLOCATE ( MAPTMP )
  624 !
  625 ! Spectral parameters ------------------------------------------------ *
  626 !                                                 Module W3GDATMD SGRD
  627 !
  628       IF ( WRITE ) THEN
  629           WRITE (NDSM)                                                &
  630                MAPWN, MAPTH, DTH, TH, ESIN, ECOS, ES2, ESC, EC2,      &
  631                XFR, FR1, SIG, SIG2, DSIP, DSII, DDEN, DDEN2, FTE,     &
  632                FTF, FTWN, FTTR, FTWL, FACTI1, FACTI2, FACHFA, FACHFE
  633         ELSE
  634           IF (.NOT.SINIT) CALL W3DIMS ( IGRD, NK, NTH, NDSE, NDST )
  635           READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
  636                MAPWN, MAPTH, DTH, TH, ESIN, ECOS, ES2, ESC, EC2,      &
  637                XFR, FR1, SIG, SIG2, DSIP, DSII, DDEN, DDEN2, FTE,     &
  638                FTF, FTWN, FTTR, FTWL, FACTI1, FACTI2, FACHFA, FACHFE
  639         END IF
  640               
  641 !
  642 ! Output flags for 3D parameters ------------------------------------- *
  643 !                                                 Module W3GDATMD
  644       IF ( WRITE ) THEN
  645           WRITE (NDSM)                                                &
  646                 E3DF, P2MSF, US3DF,USSPF, USSP_WN
  647         ELSE
  648           READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
  649                 E3DF, P2MSF, US3DF,USSPF, USSP_WN
  650         END IF
  651               
  652       IF ( INXOUT .EQ. 'GRID' ) THEN
  653           CLOSE (NDSM)
  654           RETURN
  655         END IF
  656 !
  657 ! Parameters for output boundary points ------------------------------ *
  658 !                                                 Module W3ODATMD OUT5
  659 !
  660       IF ( WRITE ) THEN
 
 
                                                         w3iogrmd.F90  page  13
 
 
  661           WRITE (NDSM)                                                &
  662                XBPO, YBPO, RDBPO, IPBPO, ISBPO
  663         ELSE
  664           CALL W3DMO5 ( IGRD, NDSE, NDST, 2 )
  665           READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
  666                XBPO, YBPO, RDBPO, IPBPO, ISBPO
  667         END IF
  668 !
  669 ! Parameters for spectral partitioning  ------------------------------ *
  670 !                                                 Module W3ODATMD OUT6
  671 !
  672       IF ( WRITE ) THEN
  673           WRITE (NDSM)                                                &
  674                 IHMAX, HSPMIN, WSMULT, WSCUT, FLCOMB, NOSWLL,         &
  675                 PTMETH, PTFCUT
  676         ELSE
  677           READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
  678                 IHMAX, HSPMIN, WSMULT, WSCUT, FLCOMB, NOSWLL,         &
  679                 PTMETH, PTFCUT
  680         END IF
  681 !
  682 ! Numerical parameters ----------------------------------------------- *
  683 !                                                 Module W3GDATMD NPAR
  684 !
  685       IF ( WRITE ) THEN
  686           WRITE (NDSM)                                                &
  687                 FACP, XREL, XFLT, FXFM, FXPM, XFT, XFC, FACSD, FHMAX, &
  688                 FFACBERG, DELAB, FWTABLE
  689           WRITE (NDSM)                                                &
  690                 RWINDC
  691           WRITE (NDSM)                                                &
  692                 RREF, REFPARS, REFLC, REFLD
  693           WRITE   (NDSM)                                              &
  694                 IGPARS(1:12)
  695         ELSE
  696           READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
  697                 FACP, XREL, XFLT, FXFM, FXPM, XFT, XFC, FACSD, FHMAX, &
  698                 FFACBERG, DELAB, FWTABLE
  699           READ  (NDSM)                                                &
  700                 RWINDC
  701           READ  (NDSM)                                                &
  702                 RREF, REFPARS, REFLC, REFLD
  703          READ   (NDSM)                                                &
  704                 IGPARS(1:12)
  705         END IF
  706 !
  707 ! Source term parameters --------------------------------------------- *
  708 !                                                 Module W3GDATMD SFLP
  709 !                                                 Module W3GDATMD SLNP
  710 !                                                 Module W3GDATMD SRCP
  711 !                                                 Module W3GDATMD SNLP
  712 !                                                 Module W3GDATMD SBTP
  713 !
  714       IF ( WRITE ) THEN
  715           WRITE (NDSM)                            SLNC1, FSPM, FSHF
 
 
                                                         w3iogrmd.F90  page  14
 
 
  716         ELSE
  717           READ (NDSM,END=801,ERR=802,IOSTAT=IERR) SLNC1, FSPM, FSHF
  718         END IF
  719 !
  720       IF ( FLTEST ) WRITE (NDST,9049) SLNC1, FSPM, FSHF
  721 !
  722       IF ( WRITE ) THEN
  723           CALL INSIN4(.TRUE.)
  724           WRITE (NDSM)                                           &
  725                 ZZWND, AALPHA, ZZ0MAX, BBETA, SSINTHP, ZZALP,    &
  726                 TTAUWSHELTER, SSWELLFPAR, SSWELLF, SSINBR,       &
  727                 ZZ0RAT, SSDSC,                                   &
  728                 SSDSISO, SSDSBR, SSDSBT, SSDSBM, SSDSP,         &
  729                 SSDSCOS, SSDSDTH, WWNMEANP, WWNMEANPTAIL,SSTXFTF,&
  730                 SSTXFTFTAIL, SSTXFTWN, SSTXFTF, SSTXFTWN,        &
  731                 SSDSBRF1, SSDSBRF2, SSDSBRFDF,SSDSBCK, SSDSABK,  &
  732                 SSDSPBK, SSDSBINT, FFXPM, FFXFM, FFXFA,   &
  733                 SSDSHCK, DELUST, DELTAIL, DELTAUW,        &
  734                 DELU, DELALP, TAUT, TAUHFT, TAUHFT2,             &
  735                 IKTAB, DCKI, QBI, SATINDICES, SATWEIGHTS,        &
  736                 DIKCUMUL, CUMULW
  737         ELSE
  738           READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                &
  739                 ZZWND, AALPHA, ZZ0MAX, BBETA, SSINTHP, ZZALP,    &
  740                 TTAUWSHELTER, SSWELLFPAR, SSWELLF, SSINBR,       &
  741                 ZZ0RAT, SSDSC,                                   &
  742                 SSDSISO, SSDSBR, SSDSBT, SSDSBM, SSDSP,         &
  743                 SSDSCOS, SSDSDTH, WWNMEANP, WWNMEANPTAIL,SSTXFTF,&
  744                 SSTXFTFTAIL, SSTXFTWN, SSTXFTF, SSTXFTWN,        &
  745                 SSDSBRF1, SSDSBRF2, SSDSBRFDF,SSDSBCK, SSDSABK,  &
  746                 SSDSPBK, SSDSBINT, FFXPM, FFXFM, FFXFA,   &
  747                  SSDSHCK, DELUST, DELTAIL, DELTAUW,        &
  748                 DELU, DELALP, TAUT, TAUHFT, TAUHFT2,             &
  749                 IKTAB, DCKI, QBI, SATINDICES, SATWEIGHTS,        &
  750                 DIKCUMUL, CUMULW
  751         END IF
  752 !
  753 ! ... Nonlinear interactions
  754 !
  755       IF ( WRITE ) THEN
  756           WRITE (NDSM)                                           &
  757                 SNLC1, LAM, KDCON, KDMN, SNLS1, SNLS2, SNLS3
  758         ELSE
  759           READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                &
  760                 SNLC1, LAM, KDCON, KDMN, SNLS1, SNLS2, SNLS3
  761         END IF
  762 !
  763       IF ( FLTEST ) WRITE (NDST,9051) SNLC1, LAM,                &
  764                            KDCON, KDMN, SNLS1, SNLS2, SNLS3
  765 !
  766       IF ( .NOT. WRITE ) CALL INSNL1 ( IGRD )
  767 !
  768 ! Layered barriers needed for file management in xnl_init
  769 !
  770       IF ( FLSNL2 .AND. .NOT.WRITE ) THEN
 
 
                                                         w3iogrmd.F90  page  15
 
 
  771           DO IP=1, IAPROC-1
  772             CALL MPI_BARRIER (  MPI_COMM_WAVE, IERR_MPI )
  773             END DO
  774         END IF
  775       IF ( FLSNL2 .AND. .NOT.WRITE ) THEN
  776           DO IP=IAPROC, NAPROC-1
  777             CALL MPI_BARRIER (  MPI_COMM_WAVE, IERR_MPI )
  778             END DO
  779         END IF
  780 !
  781 ! ... Bottom friction ...
  782 !
  783 ! ... Depth induced breaking ...
  784 !
  785  
  786 !
  787  
  788  
  789  
  790 !
  791 ! Propagation scheme ------------------------------------------------- *
  792 !                                                 Module W3GDATMD PROP
  793 !
  794       IF ( WRITE ) THEN
  795           WRITE (NDSM) WDCG, WDTH
  796         ELSE
  797           READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                &
  798                        WDCG, WDTH
  799         END IF
  800 !
  801       IF ( FLTEST ) WRITE (NDST,9060) WDCG, WDTH
  802 !
  803 ! Interpolation tables ( fill locally ) ----------------------------- *
  804 !                                                      Module W3DISPMD
  805 !
  806       IF ( .NOT.WRITE .AND. .NOT.FLDISP ) THEN
  807           CALL DISTAB
  808           FLDISP = .TRUE.
  809         END IF
  810 !
  811       CLOSE ( NDSM )
  812                     
  813 !
  814       RETURN
  815 !
  816 ! Escape locations read errors --------------------------------------- *
  817 !
  818   800 CONTINUE
  819       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) FILEXT(:I), IERR
  820       CALL EXTCDE ( 50 )
  821 !
  822   801 CONTINUE
  823       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001) FILEXT(:I)
  824       CALL EXTCDE ( 51 )
  825 !
 
 
                                                         w3iogrmd.F90  page  16
 
 
  826   802 CONTINUE
  827       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002) FILEXT(:I), IERR,   &
  828                                                   MESSAGE
  829       CALL EXTCDE ( 52 )
  830 !
  831 ! Formats
  832 !
  833   900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
  834                '     ILEGAL INXOUT VALUE: ',A/)
  835   901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
  836                '     ILEGAL IDSTR, READ : ',A/                        &
  837                '                  CHECK : ',A/)
  838   902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
  839                '     ILEGAL VERGRD, READ : ',A/                       &
  840                '                   CHECK : ',A/)
  841   904 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
  842                '     ILEGAL NFBPO READ : ',I8/                        &
  843                '                 CHECK : ',I8/)
  844   905 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
  845                '     UNEXPECTED SOURCE TERM IDENTIFIER',I2/           &
  846                '          IN mod_def.',A,' FILE : ',A/                &
  847                '    EXPECTED FROM switch FILE : ',A,/                 &
  848                5(A,/) /)
  849 !               '     CHECK CONSISTENCY OF SWITCHES IN PROGRAMS'/)
  850   906 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
  851                '     UNEXPECTED PROPAGATION SCHEME IDENTIFIER'/       &
  852                '                IN FILE :',A/                         &
  853                '               EXPECTED :',A/                         &
  854                '     CHECK CONSISTENCY OF SWITCHES IN PROGRAMS'/)
  855   907 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
  856                '     UNEXPECTED GSE ALEVIATION IDENTIFIER'/           &
  857                '                IN FILE :',A/                         &
  858                '               EXPECTED :',A/                         &
  859                , 5(A,/) /)
  860 !               '     CHECK CONSISTENCY OF SWITCHES IN PROGRAMS'/)
  861   908 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
  862                '     UNEXPECTED FLUX PARAMETERIZATION IDENTIFIER'/    &
  863                '         IN mod_def.',A,' :',A/                       &
  864                '               EXPECTED :',A/                         &
  865                , 5(A,/) /)
  866 !               '     CHECK CONSISTENCY OF SWITCHES IN PROGRAMS'/)
  867 !
  868  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR : '/               &
  869                '     ERROR IN OPENING mod_def.',A,' FILE'/            &
  870                '     IOSTAT =',I5/)
  871  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR : '/               &
  872                '     PREMATURE END OF mod_def.',A,' FILE'/)
  873  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR : '/,              &
  874                '     ERROR IN READING FROM mod_def.',A,' FILE'/       &
  875                '     IOSTAT =',I5,                                    &
  876                5(A,/) /)
  877 !
  878  9049 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SLNP'/             &
  879               '      INPUT  : ',3E10.3)
  880 !
 
 
                                                         w3iogrmd.F90  page  17
 
 
  881  9051 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SNLP'/             &
  882               '      DATA   : ',2E10.3/                          &
  883               '               ',5E10.3)
  884 !
  885  9060 FORMAT (' TEST W3IOGR : MODULE W3GDATMD PROP'/             &
  886               '      DATA   : ',2F6.2)
  887 !
  888 !/
  889 !/ End of W3IOGR ----------------------------------------------------- /
  890 !/
  891       END SUBROUTINE W3IOGR
  892 !/
  893 !/ End of module W3IOGRMD -------------------------------------------- /
  894 !/
  895       END MODULE W3IOGRMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
