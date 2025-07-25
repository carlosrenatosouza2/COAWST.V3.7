                                                         w3iogomd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3IOGOMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         03-Nov-2020 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    04-Jan-2001 : Origination.                        ( version 2.00 )
   13 !/    23-Apr-2002 : Clean up.                           ( version 2.19 )
   14 !/    29-Apr-2002 : Add output parameters 17-18.        ( version 2.20 )
   15 !/    30-May-2002 : Switch clean up.                    ( version 2.21 )
   16 !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
   17 !/    25-Oct-2004 : Multiple grid version.              ( version 3.06 )
   18 !/    27-Jun-2005 : Adding MAPST2.                      ( version 3.07 )
   19 !/    21-Jul-2005 : Adding output fields 19-21.         ( version 3.07 )
   20 !/    23-Apr-2006 : Filter for directional spread.      ( version 3.09 )
   21 !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   22 !/    05-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
   23 !/    02-Apr-2007 : Adding partitioned output.          ( version 3.11 )
   24 !/                  Adding user slots for outputs.
   25 !/    08-Oct-2007 : Adding ST3 source term option.      ( version 3.13 )
   26 !/                  ( F. Ardhuin )
   27 !/    05-Mar-2008 : Added NEC sxf90 compiler directives
   28 !/                  (Chris Bunney, UK Met Office)       ( version 3.13 )
   29 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   30 !/    13-Sep-2009 : Add coupling option                 ( version 3.14 )
   31 !/    10-Mar-2009 : Add second order pressure           ( version 3.14 )
   32 !/    15-Sep-2010 : Adding ST4 source term option.      ( version 3.14 )
   33 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
   34 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   35 !/    05-Feb-2011 : Implement unstructured grid        ( version 3.14.3 )
   36 !/                  (A. Roland and F. Ardhuin)
   37 !/    12-Jun-2012 : Add /RTD option or rotated grid option.
   38 !/                  (Jian-Guo Li)                       ( version 4.06 )
   39 !/    25-Dec-2012 : New output structure and smaller    ( version 4.11 )
   40 !/                  memory footprint.
   41 !/    15-Apr-2013 : New subroutine to read param. names ( version 4.11 )
   42 !/    21-Aug-2013 : Bug correction in W3IOGO: UBR, ABR  ( version 4.11 )
   43 !/    11-Nov-2013 : SMC and rotated grid incorporated in the main
   44 !/                  trunk                               ( version 4.13 )
   45 !/    31-Jan-2014 : Bug fix warning output (Tolman).    ( version 4.18 )
   46 !/    10-Feb-2014 : Bug correction for US3D: div. by df ( version 4.18 )
   47 !/    30-Apr-2014 : Add th2m and sth2m calculation      ( version 5.01 )
   48 !/    27-May-2014 : Switch to OMPG switch.              ( version 5.02 )
   49 !/    27-Aug-2015 : Add ICEF,ICEH as output fields      ( version 5.10 )
   50 !/    01-Mar-2018 : Removed RTD code (now used in post  ( version 6.02 )
   51 !/                  processing code)
   52 !/    05-Jun-2018 : Add DEBUGSTP/SETUP                  ( version 6.04 )
   53 !/    22-Aug-2018 : Add WBT output parameter            ( version 6.06 )
   54 !/    25-Sep-2019 : Corrected th2m and sth2m            ( version 6.07 )
   55 !/                  calculations. (J Dykes, NRL)
 
 
                                                         w3iogomd.F90  page   2
 
 
   56 !/    04-Oct-2019 : Optional one file per output stride ( version 7.00 )
   57 !/                  (Roberto Padilla-Hernandez & J.H. Alves)
   58 !/    03-Nov-2020 : Factored out NAME matching into     ( version 7.12 )
   59 !/                  seperate subroutine. (C. Bunney)
   60 !/    15-Jan-2020 : Added TP output based on exsiting   ( version 7.12 )
   61 !/                  FP internal field. (C. Bunney)
   62 !/
   63 !/    Copyright 2009-2014 National Weather Service (NWS),
   64 !/       National Oceanic and Atmospheric Administration.  All rights
   65 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   66 !/       No unauthorized use without permission.
   67 !/
   68 !  1. Purpose :
   69 !
   70 !     Gridded output of mean wave parameters.
   71 !
   72 !  2. Variables and types :
   73 !
   74 !      Name      Type  Scope    Description
   75 !     ----------------------------------------------------------------
   76 !      VEROGR    C*10  Private  Gridded output file version number.
   77 !      IDSTR     C*30  Private  Gridded output file ID string.
   78 !     ----------------------------------------------------------------
   79 !
   80 !  3. Subroutines and functions :
   81 !
   82 !      Name      Type  Scope    Description
   83 !     ----------------------------------------------------------------
   84 !      W3OUTG    Subr. Public   Calculate mean parameters.
   85 !      W3IOGO    Subr. Public   IO to raw gridded fields file.
   86 !     ----------------------------------------------------------------
   87 !
   88 !  4. Subroutines and functions used :
   89 !
   90 !      Name      Type  Module   Description
   91 !     ----------------------------------------------------------------
   92 !      W3SETO    Subr. W3ODATMD Point to data structure.
   93 !      W3SETG    Subr. W3GDATMD Point to data structure.
   94 !      W3SETW    Subr. W3WDATMD Point to data structure.
   95 !      W3SETA    Subr. W3ADATMD Point to data structure.
   96 !      W3XETA    Subr. W3ADATMD Point to data structure.
   97 !      W3DIMW    Subr. W3WDATMD Allocate data structure.
   98 !      W3DIMA    Subr. W3ADATMD Allocate data structure.
   99 !      STRACE    Subr. W3SERVMD Subroutine tracing.           ( !/S )
  100 !      EXTCDE    Subr. W3SERVMD Program abort with exit code.
  101 !     ----------------------------------------------------------------
  102 !
  103 !  5. Remarks :
  104 !
  105 !     - The different output fields are not folded in with this module
  106 !       due to the different requirements for a element '0' in some of
  107 !       the fields.
  108 !
  109 !  6. Switches :
  110 !
 
 
                                                         w3iogomd.F90  page   3
 
 
  111 !       !/SHRD  Switch for shared / distributed memory architecture.
  112 !       !/DIST  Id.
  113 !
  114 !       !/OMPG  OpenMP compiler directive for loop splitting.
  115 !       !/C90   Cray FORTRAN 90 compiler directive.
  116 !       !/NEC   NEC SXF90 compiler directives.
  117 !
  118 !       !/O8    Filter for low wave heights ( HSMIN )
  119 !       !/O9    Negative wave height alowed, other mean parameters will
  120 !             not be correct.
  121 !
  122 !       !/ST0   No source terms.
  123 !       !/ST1   Source term set 1 (WAM equiv.)
  124 !       !/ST2   Source term set 2 (Tolman and Chalikov)
  125 !       !/ST3   Source term set 3 (WAM 4+)
  126 !       !/ST4   Source term set 4 (Ardhuin et al. 2009, 2010)
  127 !       !/ST6   Source term set 6 (BYDRZ)
  128 !       !/STX   Open source term slot (implemented as ST0).
  129 !
  130 !       !/S     Enable subroutine tracing.
  131 !       !/T     Test output.
  132 !
  133 !  7. Source code :
  134 !
  135 !/ ------------------------------------------------------------------- /
  136 !/
  137       PUBLIC
  138       CHARACTER(LEN=1024)                   :: FLDOUT
  139 !/
  140 !/ Private parameter statements (ID strings)
  141 !/
  142       CHARACTER(LEN=10), PARAMETER, PRIVATE :: VEROGR = '2019-10-04'
  143       CHARACTER(LEN=30), PARAMETER, PRIVATE ::                        &
  144                             IDSTR = 'WAVEWATCH III GRID OUTPUT FILE'
  145 !/
  146       CONTAINS
  147 !/ ------------------------------------------------------------------- /
  148       SUBROUTINE W3FLGRDUPDT ( NDSO, NDSEN, FLGRD, FLGR2, FLGD, FLG2 )
  149 !/
  150 !/                  +-----------------------------------+
  151 !/                  | WAVEWATCH III           NOAA/NCEP |
  152 !/                  |           F. Ardhuin              |
  153 !/                  |                        FORTRAN 90 |
  154 !/                  | Last update :         15-Apr-2013 |
  155 !/                  +-----------------------------------+
  156 !/
  157 !/    15-Apr-2013 : Origination.                        ( version 4.10 )
  158 !/
  159 !  1. Purpose :
  160 !
  161 !     Updates the flags for output parameters based on the mod_def file
  162 !     this is to prevent the allocation of big 3D arrays when not request>
  163 !
  164 !  3. Parameters :
  165 !
 
 
                                                         w3iogomd.F90  page   4
 
 
  166 !     Parameter list
  167 !     ----------------------------------------------------------------
  168 !       NDSO      Int.   I   Output file logical unit number
  169 !       NDSEN     R.A.   I   Error output file logical unit number
  170 !       FLGD,FLG2 L.A.   O   1D array of flags for groups
  171 !       FLGRD     L.A.   O   2D array of flags
  172 !       FLGR2     L.A.   O   2D array of flags
  173 !     ----------------------------------------------------------------
  174 !
  175 !  4. Subroutines used :
  176 !
  177 !     None
  178 !
  179 !  5. Called by :
  180 !
  181 !      Name      Type  Module   Description
  182 !     ----------------------------------------------------------------
  183 !      W3INIT    Subr.   N/A
  184 !     ----------------------------------------------------------------
  185 !
  186 !  6. Error messages :
  187 !
  188 !     None.
  189 !
  190 !  8. Structure :
  191 !
  192 !     See source code.
  193 !
  194 !  9. Switches :
  195 !
  196 !     !/S     Enable subroutine tracing.
  197 !     !/T     Test output.
  198 !
  199 ! 10. Source code :
  200 !
  201 !/ ------------------------------------------------------------------- /
  202       USE CONSTANTS
  203       USE W3GDATMD, ONLY: E3DF, P2MSF, US3DF, USSPF
  204       USE W3ODATMD, ONLY: NOGRP, NGRPP
  205 !
  206       IMPLICIT NONE
  207 !/
  208 !/ ------------------------------------------------------------------- /
  209 !/ Parameter list
  210 !/
  211       INTEGER, INTENT(IN)     :: NDSO, NDSEN
  212       LOGICAL, INTENT(INOUT)  :: FLGRD(NOGRP,NGRPP), FLGD(NOGRP),     &
  213                                  FLGR2(NOGRP,NGRPP), FLG2(NOGRP)
  214 !/
  215 !/ ------------------------------------------------------------------- /
  216 !/ Local parameters
  217 !/
  218       INTEGER             :: I
  219       CHARACTER(LEN=10)  :: VARNAME1(5),VARNAME2(5)
  220 !/
 
 
                                                         w3iogomd.F90  page   5
 
 
  221 !/ ------------------------------------------------------------------- /
  222 !/
  223 !
  224       VARNAME1(1) = 'EF';    VARNAME2(1) = 'E3D'
  225       VARNAME1(2) = 'TH1M';  VARNAME2(2) = 'TH1MF'
  226       VARNAME1(3) = 'STH1M'; VARNAME2(3) = 'STH1MF'
  227       VARNAME1(4) = 'TH2M';  VARNAME2(4) = 'TH2MF'
  228       VARNAME1(5) = 'STH2M'; VARNAME2(5) = 'STH2MF'
  229                                                    
  230       DO I=1,5
  231         IF (E3DF(1,I).LE.0.OR.E3DF(3,I).LT.E3DF(2,I)) THEN
  232           IF (FLGRD(3,I).OR.FLGR2(3,I)) THEN
  233              WRITE(NDSEN,1008) VARNAME1(I),VARNAME2(I)
  234           END IF
  235           FLGRD(3,I)=.FALSE.
  236           FLGR2(3,I)=.FALSE.
  237         END IF
  238       END DO
  239       IF (US3DF(1).LE.0.OR.US3DF(3).LT.US3DF(2)) THEN
  240         IF (FLGRD(6,8).OR.FLGR2(6,8)) THEN
  241           WRITE(NDSEN,1008) 'USF','US3D'
  242         END IF
  243         FLGRD(6,8)=.FALSE.
  244         FLGR2(6,8)=.FALSE.
  245       END IF
  246       IF (USSPF(1).LE.0.OR.USSPF(2).LE.0) THEN
  247         IF (FLGRD(6,12).OR.FLGR2(6,12)) THEN
  248           WRITE(NDSEN,1008) 'USP','USSP'
  249         END IF
  250         FLGRD(6,12)=.FALSE.
  251         FLGR2(6,12)=.FALSE.
  252       END IF
  253       IF (P2MSF(1).LE.0.OR.P2MSF(3).LT.P2MSF(2)) THEN
  254         IF (FLGRD(6,9).OR.FLGR2(6,9)) THEN
  255           WRITE(NDSEN,1008) 'P2L','P2SF'
  256         END IF
  257         FLGRD(6,9)=.FALSE.
  258         FLGR2(6,9)=.FALSE.
  259       END IF
  260 !
  261       FLGD(3) = .FALSE.
  262       FLG2(3) = .FALSE.
  263       IF(ANY(FLGRD(3,:))) FLGD(3)=.TRUE.
  264       IF(ANY(FLGR2(3,:))) FLG2(3)=.TRUE.
  265       FLGD(6) = .FALSE.
  266       FLG2(6) = .FALSE.
  267       IF(ANY(FLGRD(6,:))) FLGD(6)=.TRUE.
  268       IF(ANY(FLGR2(6,:))) FLG2(6)=.TRUE.
  269 !
  270       RETURN
  271 !
  272  1008 FORMAT (/' *** WAVEWATCH III WARNING  : '/                       &
  273                '     PARAMETER ',A,' not allowed: need to set',        &
  274                ' parameter ',A,' in OUTS namelist (in ww3_grid.inp)'   &
  275                ' with proper bounds' )
 
 
                                                         w3iogomd.F90  page   6
 
 
  276 !
  277       END SUBROUTINE W3FLGRDUPDT
  278 !/ ------------------------------------------------------------------- /
  279       SUBROUTINE W3READFLGRD ( NDSI , NDSO, NDSS, NDSEN, COMSTR,      &
  280                                FLG1D, FLG2D, IAPROC, NAPOUT, IERR)
  281 !/
  282 !/                  +-----------------------------------+
  283 !/                  | WAVEWATCH III           NOAA/NCEP |
  284 !/                  |           F. Ardhuin              |
  285 !/                  |                        FORTRAN 90 |
  286 !/                  | Last update :         25-Sep-2020 |
  287 !/                  +-----------------------------------+
  288 !/
  289 !/    15-Apr-2013 : Origination.                        ( version 4.10 )
  290 !/    31-Jan-2014 : Bug fix warning output (Tolman).    ( version 4.18 )
  291 !/    30-Apr-2014 : Add th2m and sth2m calculation      ( version 5.01 )
  292 !/    25-Sep-2020 : Calculate FLG1D for any processor   ( version 7.10 )
  293 !/    03-Nov-2020 : Factored out NAME matching into     ( version 7.12 )
  294 !/                  seperate subroutine (C. Bunney)
  295 !/
  296 !  1. Purpose :
  297 !
  298 !     Fills in FLG1D and FLG2D arrays from ASCII input file
  299 !
  300 !  3. Parameters :
  301 !
  302 !     Parameter list
  303 !     ----------------------------------------------------------------
  304 !       NDSI    Int.   I   Input file logical unit number
  305 !       NDSO    Int.   I   Output file logical unit number
  306 !       NDSS    Int.   I   Screen file logical unit number
  307 !       NDSEN   R.A.   I   Error output file logical unit number
  308 !       COMSTR  Char   I   Comment string, usually '$'
  309 !       FLG1D   L.A.   O   1D array of flags for groups
  310 !       FLG2D   L.A.   O   2D array of flags
  311 !       IAPROC  Int.   I   index of current processor
  312 !       NAPOUT  Int.   I   index of processor for output (screen)
  313 !       IERR    Int.   O   Error message number
  314 !     ----------------------------------------------------------------
  315 !
  316 !  4. Subroutines used :
  317 !
  318 !     None
  319 !
  320 !  5. Called by :
  321 !
  322 !      Name      Type  Module   Description
  323 !     ----------------------------------------------------------------
  324 !      WW3_SHEL  Prog.   N/A    Actual wave model program
  325 !      WW3_OUTF  Prog.   N/A    Output postprocessor.
  326 !      WW3_OUNF  Prog.   N/A    NetCDF output postprocessor.
  327 !     ----------------------------------------------------------------
  328 !
  329 !  6. Error messages :
  330 !
 
 
                                                         w3iogomd.F90  page   7
 
 
  331 !     None.
  332 !
  333 !  8. Structure :
  334 !
  335 !     See source code.
  336 !
  337 !  9. Switches :
  338 !
  339 !     !/S     Enable subroutine tracing.
  340 !     !/T     Test output.
  341 !
  342 ! 10. Source code :
  343 !
  344 !/ ------------------------------------------------------------------- /
  345       USE CONSTANTS
  346       USE W3GDATMD, ONLY: US3DF, USSPF
  347       USE W3ODATMD, ONLY: NOGRP, NGRPP, NOGE, IDOUT
  348       USE W3SERVMD, ONLY: NEXTLN, STRSPLIT, STR_TO_UPPER
  349 !
  350       IMPLICIT NONE
  351 !/
  352 !/ ------------------------------------------------------------------- /
  353 !/ Parameter list
  354 !/
  355       INTEGER, INTENT(IN)     :: NDSI, NDSO, NDSS, NDSEN, IAPROC, NAPOUT
  356       INTEGER, INTENT(OUT)    :: IERR
  357       CHARACTER(LEN=1)        :: COMSTR
  358       LOGICAL, INTENT(OUT)    :: FLG2D(NOGRP,NGRPP), FLG1D(NOGRP)
  359       CHARACTER(LEN=100)      :: OUT_NAMES(100), TESTSTR
  360 !/
  361 !/ ------------------------------------------------------------------- /
  362 !/ Local parameters
  363 !/
  364       INTEGER             :: IFI, IFJ, IOUT
  365       CHARACTER(LEN=1)    :: AFLG
  366       LOGICAL             :: FLT, NAMES
  367 !/
  368 !/ ------------------------------------------------------------------- /
  369 !/
  370 !
  371 ! 1.  Initialize flags -------------------------------------- *
  372 !
  373       IERR=0
  374       FLG2D(:,:)=.FALSE. ! Initialize FLG2D
  375       FLG1D(:)=.FALSE. ! Initialize FLOG
  376       NAMES =.FALSE.
  377 !
  378       DO IFI=1,NOGRP ! Loop over field output groups
  379 !
  380         CALL NEXTLN ( COMSTR , NDSI , NDSEN )
  381         READ (NDSI,*,END=2001,ERR=2002) AFLG
  382         IF (AFLG.EQ.'T') THEN
  383           FLG1D(IFI)=.TRUE.
  384         ELSE IF (AFLG.EQ.'F') THEN
  385           FLG1D(IFI)=.FALSE.
 
 
                                                         w3iogomd.F90  page   8
 
 
  386         ELSE IF (AFLG.EQ.'N') THEN
  387           NAMES=.TRUE.
  388           EXIT
  389         ELSE
  390           IERR=1
  391           GOTO 2005
  392           END IF
  393         IF ( FLG1D (IFI) ) THEN ! Skip if group not requested
  394           CALL NEXTLN ( COMSTR , NDSI , NDSEN )
  395           READ (NDSI,'(A)',END=2001,ERR=2006,IOSTAT=IERR)              &
  396                                                        FLDOUT
  397           OUT_NAMES(:)=''
  398           CALL STRSPLIT(FLDOUT,OUT_NAMES)
  399           IFJ=0
  400           DO WHILE (len_trim(OUT_NAMES(IFJ+1)).NE.0)
  401              IFJ=IFJ+1
  402              IF ( OUT_NAMES(IFJ) .EQ. 'T' )                            &
  403                                FLG2D(IFI,IFJ)=.TRUE.
  404           ENDDO
  405           IF ( IAPROC .EQ. NAPOUT .AND. IFJ .LT. NOGE(IFI) ) WRITE(NDSEN,>
  406          ENDIF
  407         END DO
  408 !
  409       IF (NAMES) THEN
  410 !
  411 ! 2. Reads and splits list of output field names
  412 !
  413         CALL NEXTLN ( COMSTR , NDSI , NDSEN )
  414         READ (NDSI,'(A)',END=2001,ERR=2003,IOSTAT=IERR) FLDOUT
  415         OUT_NAMES(:)=''
  416         CALL STRSPLIT(FLDOUT,OUT_NAMES)
  417         IOUT=0
  418         DO WHILE (len_trim(OUT_NAMES(IOUT+1)).NE.0)
  419           CALL STR_TO_UPPER(OUT_NAMES(IOUT+1))
  420 !
  421 ! 2. Matches names with expected ...
  422 !
  423           TESTSTR=OUT_NAMES(IOUT+1)
  424           CALL W3FLDTOIJ(TESTSTR, IFI, IFJ, IAPROC, NAPOUT, NDSEN)
  425                                                                   
  426           IF(IFI .NE. -1) THEN
  427             FLG2D(IFI, IFJ) = .TRUE.
  428           ENDIF
  429 !
  430           IOUT=IOUT+1
  431 !
  432         END DO
  433 !
  434       END IF
  435 !
  436       FLT    = .TRUE.
  437       DO IFI=1, NOGRP
  438         IF ( IAPROC .EQ. NAPOUT ) THEN
  439           DO IFJ=1, NGRPP
  440             IF ( FLG2D(IFI,IFJ) ) THEN
 
 
                                                         w3iogomd.F90  page   9
 
 
  441               IF ( FLT ) THEN
  442                 WRITE (NDSO,1945) IDOUT(IFI,IFJ)
  443                 FLT    = .FALSE.
  444               ELSE
  445                 WRITE (NDSO,1946) IDOUT(IFI,IFJ)
  446               END IF
  447             END IF
  448           END DO
  449         END IF
  450         IF(ANY(FLG2D(IFI,:))) FLG1D(IFI)=.TRUE. !Update FLG1D
  451       END DO
  452       IF ( IAPROC .EQ. NAPOUT ) THEN
  453         IF ( FLT ) WRITE (NDSO,1945) 'no fields defined'
  454       END IF
  455 !
  456       RETURN
  457 !
  458  2001 CONTINUE
  459       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSEN,1001)
  460       RETURN
  461  2002 CONTINUE
  462       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSEN, 1002) IFI, IERR
  463       RETURN
  464  2003 CONTINUE
  465       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSEN, 1003) IERR
  466       RETURN
  467 !2004 CONTINUE ! replaced by warning in code ....
  468  2005 CONTINUE
  469       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSEN, 1005) AFLG
  470       RETURN
  471  2006 CONTINUE
  472       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSEN, 1006) IFI,IERR
  473       RETURN
  474 !
  475  1945 FORMAT ( '            Fields   : ',A)
  476  1946 FORMAT ( '                       ',A)
  477 !
  478  1001 FORMAT (/' *** WAVEWATCH III ERROR  : '/                         &
  479                '     PREMATURE END OF INPUT FILE'/)
  480 !
  481  1002 FORMAT (/' *** WAVEWATCH III ERROR  : '/                         &
  482                '     ERROR IN READING OUTPUT FIELDS GROUP FLAGS ',     &
  483                I2, /, '     IOSTAT =',I5/)
  484 !
  485  1003 FORMAT (/' *** WAVEWATCH III ERROR  : '/                         &
  486                '     ERROR READING OUTPUT FIELD NAMES FROM INPUT FILE'/&
  487                '     IOSTAT =',I5/)
  488 !
  489  1005 FORMAT (/' *** WAVEWATCH III ERROR    : '/                       &
  490                '     WAS EXPECTING "T" "F" or "N", but found "',A,'".'/)
  491 !
  492  1006 FORMAT (/' *** WAVEWATCH III ERROR  : '/                         &
  493                '     ERROR IN READING OUTPUT FIELDS FLAGS FOR GROUP ', &
  494                I2, /, '     IOSTAT =',I5/)
  495 !
 
 
                                                         w3iogomd.F90  page  10
 
 
  496  1007 FORMAT (/' *** WAVEWATCH III WARNING  : '/                       &
  497                '     NUMBER OF REQUESTED OUTPUT FIELD FLAGS IN GROUP ',&
  498               I2, /,' LESS THAN AVAILABLE, CHECK DOCS FOR MORE OPTIONS')
  499 !
  500       END SUBROUTINE W3READFLGRD
  501                                 
  502 !/ ------------------------------------------------------------------- /
  503       SUBROUTINE W3FLGRDFLAG ( NDSO, NDSS, NDSEN, FLDOUT,      &
  504                                FLG1D, FLG2D, IAPROC, NAPOUT, IERR)
  505 !/
  506 !/                  +-----------------------------------+
  507 !/                  | WAVEWATCH III           NOAA/NCEP |
  508 !/                  |           F. Ardhuin              |
  509 !/                  |                        FORTRAN 90 |
  510 !/                  | Last update :         25-Sep-2020 |
  511 !/                  +-----------------------------------+
  512 !/
  513 !/    15-Apr-2013 : Origination.                        ( version 4.10 )
  514 !/    31-Jan-2014 : Bug fix warning output (Tolman).    ( version 4.18 )
  515 !/    30-Apr-2014 : Add th2m and sth2m calculation      ( version 5.01 )
  516 !/    17-Feb-2016 : New version for namelist use        ( version 5.11 )
  517 !/    25-Sep-2020 : Calculate FLG1D for any processor   ( version 7.10 )
  518 !/    03-Nov-2020 : Factored out NAME matching into     ( version 7.12 )
  519 !/                  seperate subroutine (C. Bunney)
  520 !/
  521 !  1. Purpose :
  522 !
  523 !     Fills in FLG1D and FLG2D arrays from ASCII input file
  524 !
  525 !  3. Parameters :
  526 !
  527 !     Parameter list
  528 !     ----------------------------------------------------------------
  529 !       NDSO    Int.   I   Output file logical unit number
  530 !       NDSS    Int.   I   Screen file logical unit number
  531 !       NDSEN   R.A.   I   Error output file logical unit number
  532 !       FLDOUT  Cha.   I   List of field names
  533 !       FLG1D   L.A.   O   1D array of flags for groups
  534 !       FLG2D   L.A.   O   2D array of flags
  535 !       IAPROC  Int.   I   index of current processor
  536 !       NAPOUT  Int.   I   index of processor for output (screen)
  537 !       IERR    Int.   O   Error message number
  538 !     ----------------------------------------------------------------
  539 !
  540 !  4. Subroutines used :
  541 !
  542 !     None
  543 !
  544 !  5. Called by :
  545 !
  546 !      Name      Type  Module   Description
  547 !     ----------------------------------------------------------------
  548 !      WW3_SHEL  Prog.   N/A    Actual wave model program
  549 !      WW3_OUTF  Prog.   N/A    Output postprocessor.
  550 !      WW3_OUNF  Prog.   N/A    NetCDF output postprocessor.
 
 
                                                         w3iogomd.F90  page  11
 
 
  551 !     ----------------------------------------------------------------
  552 !
  553 !  6. Error messages :
  554 !
  555 !     None.
  556 !
  557 !  8. Structure :
  558 !
  559 !     See source code.
  560 !
  561 !  9. Switches :
  562 !
  563 !     !/S     Enable subroutine tracing.
  564 !     !/T     Test output.
  565 !
  566 ! 10. Source code :
  567 !
  568 !/ ------------------------------------------------------------------- /
  569       USE CONSTANTS
  570       USE W3ODATMD, ONLY: NOGRP, NGRPP, IDOUT
  571       USE W3SERVMD, ONLY: STRSPLIT, STR_TO_UPPER
  572       USE W3GDATMD, ONLY: US3DF, USSPF
  573 !
  574       IMPLICIT NONE
  575 !/
  576 !/ ------------------------------------------------------------------- /
  577 !/ Parameter list
  578 !/
  579       INTEGER, INTENT(IN)     :: NDSO, NDSS, NDSEN, IAPROC, NAPOUT
  580       CHARACTER(1024), INTENT(IN)   :: FLDOUT
  581       INTEGER, INTENT(OUT)    :: IERR
  582       LOGICAL, INTENT(OUT)    :: FLG2D(NOGRP,NGRPP), FLG1D(NOGRP)
  583       CHARACTER(LEN=100)      :: OUT_NAMES(100), TESTSTR
  584 !/
  585 !/ ------------------------------------------------------------------- /
  586 !/ Local parameters
  587 !/
  588       INTEGER             :: I, IFI, IFJ, IOUT
  589       LOGICAL             :: FLT
  590 !/
  591 !/ ------------------------------------------------------------------- /
  592 !/
  593 !
  594 ! 1.  Initialize flags -------------------------------------- *
  595 !
  596       IERR=0
  597       FLG2D(:,:)=.FALSE. ! Initialize FLG2D
  598       FLG1D(:)=.FALSE. ! Initialize FLOG
  599 !
  600 ! 2. Splits list of output field names
  601 !
  602       OUT_NAMES(:)=''
  603       CALL STRSPLIT(FLDOUT,OUT_NAMES)
  604       IOUT=0
  605       DO WHILE (len_trim(OUT_NAMES(IOUT+1)).NE.0)
 
 
                                                         w3iogomd.F90  page  12
 
 
  606         CALL STR_TO_UPPER(OUT_NAMES(IOUT+1))
  607 !
  608 ! 2. Matches names with expected ...
  609 !
  610         TESTSTR=OUT_NAMES(IOUT+1)
  611         CALL W3FLDTOIJ(TESTSTR, IFI, IFJ, IAPROC, NAPOUT, NDSEN)
  612                                                                 
  613         IF(IFI .NE. -1) THEN
  614           FLG2D(IFI, IFJ) = .TRUE.
  615         ENDIF
  616 !
  617         IOUT=IOUT+1
  618 !
  619       END DO
  620 !
  621       FLT    = .TRUE.
  622       DO IFI=1, NOGRP
  623         IF ( IAPROC .EQ. NAPOUT ) THEN
  624           DO IFJ=1, NGRPP
  625             IF ( FLG2D(IFI,IFJ) ) THEN
  626               IF ( FLT ) THEN
  627                 WRITE (NDSO,1945) IDOUT(IFI,IFJ)
  628                 FLT    = .FALSE.
  629               ELSE
  630                 WRITE (NDSO,1946) IDOUT(IFI,IFJ)
  631               END IF
  632             END IF
  633           END DO
  634         ENDIF
  635         IF(ANY(FLG2D(IFI,:))) FLG1D(IFI)=.TRUE. !Update FLG1D
  636       END DO
  637       IF ( IAPROC .EQ. NAPOUT ) THEN
  638         IF ( FLT ) WRITE (NDSO,1945) 'no fields defined'
  639       ENDIF
  640 !
  641       RETURN
  642 !
  643  1945 FORMAT ( '            Fields   : ',A)
  644  1946 FORMAT ( '                       ',A)
  645 !
  646 ! 1004 FORMAT (/' *** WAVEWATCH III WARNING  : '/                       &
  647 !               '     REQUESTED OUTPUT FIELD ',A,' WAS NOT RECOGNIZED.'/)
  648 !!
  649 ! 1008 FORMAT (/' *** WAVEWATCH III WARNING  : '/                       &
  650 !               '     PARAMETER ',A,' not allowed: need to set',        &
  651 !               ' parameter ',A,' in OUTS namelist (in ww3_grid.inp)')
  652 !
  653       END SUBROUTINE W3FLGRDFLAG
  654                                 
  655 !/ ------------------------------------------------------------------- /
  656       SUBROUTINE W3FLDTOIJ(FLD, I, J, IAPROC, NAPOUT, NDSEN)
  657 !/
  658 !/                  +-----------------------------------+
  659 !/                  | WAVEWATCH III           NOAA/NCEP |
  660 !/                  |            C. Bunney              |
 
 
                                                         w3iogomd.F90  page  13
 
 
  661 !/                  |                        FORTRAN 90 |
  662 !/                  | Last update :         03-Nov-2020 |
  663 !/                  +-----------------------------------+
  664 !/
  665 !/    03-Nov-2020 : Origination.                        ( version 7.12 )
  666 !
  667 !  1. Purpose :
  668 !
  669 !     Returns the group/field (I/J) indices for a named output field.
  670 !
  671 !  3. Parameters :
  672 !
  673 !     Parameter list
  674 !     ----------------------------------------------------------------
  675 !       FLD     Cha.   I   Field names
  676 !       I       Int.   O   Output group number (IFI)
  677 !       J       Int.   O   Output field number (IFJ)
  678 !       IAPROC  Int.   I   index of current processor
  679 !       NAPOUT  Int.   I   index of processor for output (screen)
  680 !       NDSEN   R.A.   I   Error output file logical unit number
  681 !     ----------------------------------------------------------------
  682 !
  683 !/ ------------------------------------------------------------------- /
  684       USE W3GDATMD, ONLY: US3DF, USSPF
  685       IMPLICIT NONE
  686 !/
  687 !/ ------------------------------------------------------------------- /
  688 !/ Local parameters
  689 !/
  690       CHARACTER(LEN=*), INTENT(IN) :: FLD
  691       INTEGER, INTENT(IN) :: IAPROC, NAPOUT, NDSEN
  692       INTEGER, INTENT(OUT) :: I, J
  693                                   
  694       I = -1
  695       J = -1
  696             
  697       SELECT CASE(TRIM(FLD(1:6)))
  698 !
  699 ! Group 1
  700 !
  701       CASE('DPT')
  702         I = 1
  703         J = 1
  704       CASE('CUR')
  705         I = 1
  706         J = 2
  707       CASE('WND')
  708         I = 1
  709         J = 3
  710       CASE('AST')
  711         I = 1
  712         J = 4
  713       CASE('WLV')
  714         I = 1
  715         J = 5
 
 
                                                         w3iogomd.F90  page  14
 
 
  716       CASE('ICE')
  717         I = 1
  718         J = 6
  719       CASE('IBG')
  720         I = 1
  721         J = 7
  722 ! Group 2
  723 !
  724       CASE('HS')
  725         I = 2
  726         J = 1
  727       CASE('LM')
  728         I = 2
  729         J = 2
  730       CASE('T02')
  731         I = 2
  732         J = 3
  733       CASE('T0M1')
  734         I = 2
  735         J = 4
  736       CASE('T01')
  737         I = 2
  738         J = 5
  739       CASE('FP')
  740         I = 2
  741         J = 6
  742       CASE('DIR')
  743         I = 2
  744         J = 7
  745       CASE('SPR')
  746         I = 2
  747         J = 8
  748       CASE('DP')
  749         I = 2
  750         J = 9
  751       CASE('HIG')
  752         I = 2
  753         J = 10
  754       CASE('MXE')
  755         I = 2
  756         J = 11
  757       CASE('MXES')
  758         I = 2
  759         J = 12
  760       CASE('MXH')
  761         I = 2
  762         J = 13
  763       CASE('MXHC')
  764         I = 2
  765         J = 14
  766       CASE('SDMH')
  767         I = 2
  768         J = 15
  769       CASE('SDMHC')
  770         I = 2
 
 
                                                         w3iogomd.F90  page  15
 
 
  771         J = 16
  772       CASE('WBT')
  773         I = 2
  774         J = 17
  775       CASE('TP') ! Uses FP0 internally, as per FP
  776         I = 2
  777         J = 18
  778 !
  779 ! Group 3
  780 !
  781       CASE('EF')
  782         I = 3
  783         J = 1
  784       CASE('TH1M')
  785         I = 3
  786         J = 2
  787       CASE('STH1M')
  788         I = 3
  789         J = 3
  790       CASE('TH2M')
  791         I = 3
  792         J = 4
  793       CASE('STH2M')
  794         I = 3
  795         J = 5
  796       CASE('WN')
  797         I = 3
  798         J = 6
  799 !
  800 ! Group 4
  801 !
  802       CASE('PHS')
  803         I = 4
  804         J = 1
  805       CASE('PTP')
  806         I = 4
  807         J = 2
  808       CASE('PLP')
  809         I = 4
  810         J = 3
  811       CASE('PDIR')
  812         I = 4
  813         J = 4
  814       CASE('PSPR')
  815         I = 4
  816         J = 5
  817       CASE('PWS')
  818         I = 4
  819         J = 6
  820       CASE('PDP')
  821         I = 4
  822         J = 7
  823       CASE('PQP')
  824         I = 4
  825         J = 8
 
 
                                                         w3iogomd.F90  page  16
 
 
  826       CASE('PPE')
  827         I = 4
  828         J = 9
  829       CASE('PGW')
  830         I = 4
  831         J = 10
  832       CASE('PSW')
  833         I = 4
  834         J = 11
  835       CASE('PTM10')
  836         I = 4
  837         J = 12
  838       CASE('PT01')
  839         I = 4
  840         J = 13
  841       CASE('PT02')
  842         I = 4
  843         J = 14
  844       CASE('PEP')
  845         I = 4
  846         J = 15
  847       CASE('TWS')
  848         I = 4
  849         J = 16
  850       CASE('PNR')
  851         I = 4
  852         J = 17
  853 !
  854 ! Group 5
  855 !
  856       CASE('UST')
  857         I = 5
  858         J = 1
  859       CASE('CHA')
  860         I = 5
  861         J = 2
  862       CASE('CGE')
  863         I = 5
  864         J = 3
  865       CASE('FAW')
  866         I = 5
  867         J = 4
  868       CASE('TAW')
  869         I = 5
  870         J = 5
  871       CASE('TWA')
  872         I = 5
  873         J = 6
  874       CASE('WCC')
  875         I = 5
  876         J = 7
  877       CASE('WCF')
  878         I = 5
  879         J = 8
  880       CASE('WCH')
 
 
                                                         w3iogomd.F90  page  17
 
 
  881         I = 5
  882         J = 9
  883       CASE('WCM')
  884         I = 5
  885         J = 10
  886       CASE('FWS')
  887         I = 5
  888         J = 11
  889 !
  890 ! Group 6
  891 !
  892       CASE('SXY')
  893         I = 6
  894         J = 1
  895       CASE('TWO')
  896         I = 6
  897         J = 2
  898       CASE('BHD')
  899         I = 6
  900         J = 3
  901       CASE('FOC')
  902         I = 6
  903         J = 4
  904       CASE('TUS')
  905         I = 6
  906         J = 5
  907       CASE('USS')
  908         I = 6
  909         J = 6
  910       CASE('P2S')
  911         I = 6
  912         J = 7
  913       CASE('USF')
  914         IF (US3DF(1).GE.1) THEN
  915           I = 6
  916           J = 8
  917         ELSE
  918           IF ( IAPROC .EQ. NAPOUT ) WRITE(NDSEN,1008) 'USF','US3D'
  919         END IF
  920       CASE('P2L')
  921         I = 6
  922         J = 9
  923       CASE('TWI')
  924         I = 6
  925         J = 10
  926       CASE('FIC')
  927         I = 6
  928         J = 11
  929       CASE('USP')
  930         IF (USSPF(1).GE.1) THEN
  931           I = 6
  932           J = 12
  933         ELSE
  934           IF ( IAPROC .EQ. NAPOUT ) WRITE(NDSEN,1008) 'USP','USSP'
  935         END IF
 
 
                                                         w3iogomd.F90  page  18
 
 
  936               
  937 !
  938 ! Group 7
  939 !
  940       CASE('ABR')
  941         I = 7
  942         J = 1
  943       CASE('UBR')
  944         I = 7
  945         J = 2
  946       CASE('BED')
  947         I = 7
  948         J = 3
  949       CASE('FBB')
  950         I = 7
  951         J = 4
  952       CASE('TBB')
  953         I = 7
  954         J = 5
  955 !
  956 ! Group 8
  957 !
  958       CASE('MSS')
  959         I = 8
  960         J = 1
  961       CASE('MSC')
  962         I = 8
  963         J = 2
  964       CASE('MSD')
  965         I = 8
  966         J = 3
  967       CASE('MCD')
  968         I = 8
  969         J = 4
  970       CASE('QP')
  971         I = 8
  972         J = 5
  973 !
  974 ! Group 9
  975 !
  976       CASE('DTD')
  977         I = 9
  978         J = 1
  979       CASE('FC')
  980         I = 9
  981         J = 2
  982       CASE('CFX')
  983         I = 9
  984         J = 3
  985       CASE('CFD')
  986         I = 9
  987         J = 4
  988       CASE('CFK')
  989         I = 9
  990         J = 5
 
 
                                                         w3iogomd.F90  page  19
 
 
  991 !
  992 ! Group 10
  993 !
  994       CASE('U1')
  995         I = 10
  996         J = 1
  997       CASE('U2')
  998         I = 10
  999         J = 1
 1000 ! Not found:
 1001       CASE('UNSET')
 1002       CASE DEFAULT
 1003         I = -1
 1004         J = -1
 1005         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSEN,1004) TRIM(FLD)
 1006       END SELECT
 1007                 
 1008  1004 FORMAT (/' *** WAVEWATCH III WARNING  : '/                       &
 1009                '     REQUESTED OUTPUT FIELD ',A,' WAS NOT RECOGNIZED.'/)
 1010 !
 1011  1008 FORMAT (/' *** WAVEWATCH III WARNING  : '/                       &
 1012                '     PARAMETER ',A,' not allowed: need to set',        &
 1013                ' parameter ',A,' in OUTS namelist (in ww3_grid.inp)')
 1014 !
 1015       END SUBROUTINE W3FLDTOIJ
 1016                               
 1017 !/ ------------------------------------------------------------------- /
 1018       SUBROUTINE W3OUTG ( A, FLPART, FLOUTG, FLOUTG2 )
 1019 !/
 1020 !/                  +-----------------------------------+
 1021 !/                  | WAVEWATCH III           NOAA/NCEP |
 1022 !/                  |           H. L. Tolman            |
 1023 !/                  |                        FORTRAN 90 |
 1024 !/                  | Last update :         10-Apr-2015 |
 1025 !/                  +-----------------------------------+
 1026 !/
 1027 !/    10-Dec-1998 : Distributed FORTRAN 77 version.     ( version 1.18 )
 1028 !/    04-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
 1029 !/                  Major changes to logistics.
 1030 !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
 1031 !/    19-Oct-2004 : Multiple grid version.              ( version 3.06 )
 1032 !/    21-Jul-2005 : Adding output fields 19-21.         ( version 3.07 )
 1033 !/    23-Apr-2006 : Filter for directional spread.      ( version 3.09 )
 1034 !/    02-Apr-2007 : Adding partitioned output.          ( version 3.11 )
 1035 !/                  Adding user slots for outputs.
 1036 !/    08-Oct-2007 : Adding ST3 source term option.      ( version 3.13 )
 1037 !/                  ( F. Ardhuin )
 1038 !/    05-Mar-2008 : Added NEC sxf90 compiler directives
 1039 !/                  (Chris Bunney, UK Met Office)       ( version 3.13 )
 1040 !/    25-Dec-2012 : New output structure and smaller    ( version 4.11 )
 1041 !/                  memory footprint.
 1042 !/    10-Feb-2014 : Bug correction for US3D: div. by df ( version 4.18 )
 1043 !/    30-Apr-2014 : Add th2m and sth2m calculation      ( version 5.01 )
 1044 !/    27-May-2014 : Switch to OMPG switch.              ( version 5.02 )
 1045 !/    10-Apr-2015 : Remove unused variables             ( version 5.08 )
 
 
                                                         w3iogomd.F90  page  20
 
 
 1046 !/    10-Jan-2017 : Separate Stokes drift calculation   ( version 6.01 )
 1047 !/    01-Mar-2018 : Removed RTD code (now used in post  ( version 6.02 )
 1048 !/                  processing code)
 1049 !/    22-Aug-2018 : Add WBT parameter                   ( version 6.06 )
 1050 !/    25-Sep-2019 : Corrected th2m and sth2m            ( version 6.07 )
 1051 !/                  calculations. (J Dykes, NRL)
 1052 !/
 1053 !  1. Purpose :
 1054 !
 1055 !     Fill necessary arrays with gridded data for output.
 1056 !
 1057 !  3. Parameters :
 1058 !
 1059 !     Parameter list
 1060 !     ----------------------------------------------------------------
 1061 !       A       R.A.   I   Input spectra. Left in par list to change
 1062 !                          shape.
 1063 !       FLPART  Log.   I   Flag for filling fields with part. data.
 1064 !       FLOUTG  Log.   I   Flag for file field output
 1065 !       FLOUTG2 Log.   I   Flag for coupling field output
 1066 !     ----------------------------------------------------------------
 1067 !
 1068 !     Locally saved parameters
 1069 !     ----------------------------------------------------------------
 1070 !       HSMIN   Real  Filter level in Hs for calculation of mean
 1071 !                     wave parameters.
 1072 !     ----------------------------------------------------------------
 1073 !
 1074 !  4. Subroutines used :
 1075 !
 1076 !     See module documentation.
 1077 !
 1078 !  5. Called by :
 1079 !
 1080 !      Name      Type  Module   Description
 1081 !     ----------------------------------------------------------------
 1082 !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
 1083 !     ----------------------------------------------------------------
 1084 !
 1085 !  6. Error messages :
 1086 !
 1087 !     None.
 1088 !
 1089 !  8. Structure :
 1090 !
 1091 !     See source code.
 1092 !
 1093 !  9. Switches :
 1094 !
 1095 !     !/SHRD  Switch for shared / distributed memory architecture.
 1096 !     !/DIST  Id.
 1097 !
 1098 !     !/C90   Cray FORTRAN 90 compiler directives.
 1099 !     !/NEC   NEC SXF90 compiler directives.
 1100 !     !/OMPG  OpenMP compiler directive for loop splitting.
 
 
                                                         w3iogomd.F90  page  21
 
 
 1101 !
 1102 !     !/O8    Filter for low wave heights ( HSMIN )
 1103 !     !/O9    Negative wave height alowed, other mean parameters will
 1104 !             not be correct.
 1105 !
 1106 !     !/ST0   No source terms.
 1107 !     !/ST1   Source term set 1 (WAM equiv.)
 1108 !     !/ST2   Source term set 2 (Tolman and Chalikov)
 1109 !     !/ST3   Source term set 3 (WAM 4+)
 1110 !     !/ST6   Source term set 6 (BYDRZ)
 1111 !     !/STX   Open source term slot (implemented as ST0).
 1112 !
 1113 !     !/S     Enable subroutine tracing.
 1114 !     !/T     Test output.
 1115 !
 1116 ! 10. Source code :
 1117 !
 1118 !/ ------------------------------------------------------------------- /
 1119       USE CONSTANTS
 1120       USE W3GDATMD
 1121       USE W3WDATMD, ONLY: UST, FPIS
 1122       USE W3ADATMD, ONLY: CG, WN, DW
 1123       USE W3ADATMD, ONLY: HS, WLM, T02, T0M1, T01, FP0,               &
 1124                           THM, THS, THP0
 1125       USE W3ADATMD, ONLY: FP1, THP1, ABA, ABD, UBA, UBD, FCUT, SXX,   &
 1126                           SYY, SXY, PHS, PTP, PLP, PDIR, PSI, PWS,    &
 1127                           PWST, PNR, USERO, TUSX, TUSY, PRMS, TPMS,   &
 1128                           USSX, USSY, MSSX, MSSY, MSSD, MSCX, MSCY,   &
 1129                           MSCD, CHARN,                                &
 1130                           BHD, CGE, P2SMS, US3D, EF, TH1M, STH1M,     &
 1131                           TH2M, STH2M, HSIG, STMAXE, STMAXD,          &
 1132                           HCMAXE, HMAXE, HCMAXD, HMAXD, USSP, QP, PQP,&
 1133                           PTHP0, PPE, PGW, PSW, PTM1, PT1, PT2, PEP,  &
 1134                           WBT
 1135       USE W3ODATMD, ONLY: NDST, UNDEF, IAPROC, NAPROC, NAPFLD,        &
 1136                           ICPRT, DTPRT, WSCUT, NOSWLL, FLOGRD, FLOGR2,&
 1137                           NOGRP, NGRPP
 1138       USE W3ADATMD, ONLY: NSEALM
 1139 !
 1140       USE W3PARALL, ONLY : INIT_GET_ISEA
 1141       IMPLICIT NONE
 1142 !/
 1143 !/ ------------------------------------------------------------------- /
 1144 !/ Parameter list
 1145 !/
 1146       REAL, INTENT(IN)        :: A(NTH,NK,0:NSEAL)
 1147       LOGICAL, INTENT(IN)     :: FLPART, FLOUTG, FLOUTG2
 1148 !/
 1149 !/ ------------------------------------------------------------------- /
 1150 !/ Local parameters
 1151 !/
 1152       INTEGER                 :: IK, ITH, JSEA, ISEA, IX, IY,         &
 1153                                  IKP0(NSEAL), IKP1(NSEAL), NKH(NSEAL),&
 1154                                  ILOW, ICEN, IHGH, I, J, LKMS, HKMS,  &
 1155                                  ITL
 
 
                                                         w3iogomd.F90  page  22
 
 
 1156       REAL                    :: FXPMC, FACTOR, FACTOR2, EBAND, FKD,  &
 1157                                  FP1STR, FP1TST, FPISTR, AABS, UABS,  &
 1158                                  XL, XH, XL2, XH2, EL, EH, DENOM, KD, &
 1159                                  M1, M2, MA, MB, MC, STEX, STEY, STED
 1160       REAL                    :: ET(NSEAL), EWN(NSEAL), ETR(NSEAL),   &
 1161                                  ETX(NSEAL), ETY(NSEAL), AB(NSEAL),   &
 1162                                  ETXX(NSEAL), ETYY(NSEAL), ETXY(NSEAL),&
 1163                                  ABX(NSEAL), ABY(NSEAL),ET02(NSEAL),  &
 1164                                  EBD(NK,NSEAL), EC(NSEAL),            &
 1165                                  ABR(NSEAL), UBR(NSEAL), UBS(NSEAL),  &
 1166                                  ABX2(NSEAL), ABY2(NSEAL),            &
 1167                                  AB2X(NSEAL), AB2Y(NSEAL),            &
 1168                                  ABST(NSEAL), ABXX(NSEAL),            &
 1169                                  ABYY(NSEAL), ABXY(NSEAL),            &
 1170                                  ABYX(NSEAL), EET1(NSEAL),            &
 1171                                  ETUSCX(NSEAL), ETUSCY(NSEAL),        &
 1172                                  ETMSSL(NSEAL), ETMSSCL(NSEAL),       &
 1173                                  ETTPMM(NSEAL), ETF(NSEAL),           &
 1174                                  ET1(NSEAL), ABX2M(NSEAL),            &
 1175                                  ABY2M(NSEAL), ABXM(NSEAL),           &
 1176                                  ABYM(NSEAL), ABXYM(NSEAL),           &
 1177                                  MSSXM(NSEAL), MSSYM(NSEAL),          &
 1178                                  MSSXTM(NSEAL), MSSYTM(NSEAL),        &
 1179                                  MSSXYM(NSEAL), THMP(NSEAL),          &
 1180                                  T02P(NSEAL), NV(NSEAL), NS(NSEAL),   &
 1181                                  NB(NSEAL), MODE(NSEAL),              &
 1182                                  MU(NSEAL), NI(NSEAL), STMAXEL(NSEAL),&
 1183                                  PHI(21,NSEAL),PHIST(NSEAL),         &
 1184                                  EBC(NK,NSEAL), ABP(NSEAL),           &
 1185                                  STMAXDL(NSEAL), TLPHI(NSEAL),        &
 1186                                  WL02X(NSEAL), WL02Y(NSEAL),          &
 1187                                  ALPXT(NSEAL), ALPYT(NSEAL),          &
 1188                                  ALPXY(NSEAL), SCREST(NSEAL)
 1189       REAL                       USSCO, FT1
 1190       REAL, SAVE              :: HSMIN = 0.01
 1191       LOGICAL                 :: FLOLOC(NOGRP,NGRPP)
 1192 !/
 1193 !/ ------------------------------------------------------------------- /
 1194 !/
 1195 !
 1196       DO I=1,NOGRP
 1197         DO J=1,NGRPP
 1198           FLOLOC(I,J) =   &
 1199              ((FLOUTG.AND.FLOGRD(I,J)).OR.(FLOUTG2.AND.FLOGR2(I,J)))
 1200           END DO
 1201         END DO
 1202 !
 1203       FXPMC  = 0.66 * GRAV / 28.
 1204       HSMIN  = HSMIN
 1205       FT1    =  0.3333 * SIG(NK)**2 * DTH * SIG(NK)
 1206 !
 1207 ! 1.  Initialize storage arrays -------------------------------------- *
 1208 !
 1209       ET     = 0.
 1210       ET02   = 0.
 
 
                                                         w3iogomd.F90  page  23
 
 
 1211       EWN    = 0.
 1212       ETR    = 0.
 1213       ET1    = 0.
 1214       EET1   = 0.
 1215       ETX    = 0.
 1216       ETY    = 0.
 1217       ETXX   = 0.
 1218       ETYY   = 0.
 1219       ETXY   = 0.
 1220       ABR    = 0.
 1221       ABA    = 0.
 1222       ABD    = 0.
 1223       UBR    = 0.
 1224       UBA    = 0.
 1225       UBD    = 0.
 1226       UBS    = 0.
 1227       SXX    = 0.
 1228       SYY    = 0.
 1229       SXY    = 0.
 1230       USSX   = 0.
 1231       USSY   = 0.
 1232       TUSX   = 0.
 1233       TUSY   = 0.
 1234       MSSX   = 0.
 1235       MSSY   = 0.
 1236       MSSD   = 0.
 1237       MSCX   = 0.
 1238       MSCY   = 0.
 1239       MSCD   = 0.
 1240       PRMS   = 0.
 1241       TPMS   = 0.
 1242       ETUSCY = 0.
 1243       ETUSCY = 0.
 1244       ETMSSL = 0.
 1245       ETMSSCL= 0.
 1246       ETTPMM = 0.
 1247       EBD    = 0.
 1248       EC     = 0.
 1249       ETF    = 0.
 1250       EBC    = 0.
 1251       BHD = 0.
 1252       MSSXM = 0.
 1253       MSSYM = 0.
 1254       MSSXTM = 0.
 1255       MSSYTM = 0.
 1256       MSSXYM = 0.
 1257       PHI    = 0.
 1258       PHIST  = 0.
 1259       TLPHI  = 0.
 1260       STMAXEL = 0.
 1261       STMAXDL = 0.
 1262 !
 1263       HS     = UNDEF
 1264       WLM    = UNDEF
 1265       T0M1   = UNDEF
 
 
                                                         w3iogomd.F90  page  24
 
 
 1266       T01    = UNDEF
 1267       T02    = UNDEF
 1268       FP0    = UNDEF
 1269       THM    = UNDEF
 1270       THS    = UNDEF
 1271       THP0   = UNDEF
 1272       HSIG   = UNDEF
 1273       WL02X  = UNDEF
 1274       WL02Y  = UNDEF
 1275       ALPXY  = UNDEF
 1276       ALPXT  = UNDEF
 1277       ALPYT  = UNDEF
 1278       THMP = UNDEF
 1279       T02P = UNDEF
 1280       SCREST = UNDEF
 1281       NV = UNDEF
 1282       NS = UNDEF
 1283       NB = UNDEF
 1284       MU = UNDEF
 1285       NI = UNDEF
 1286       MODE = UNDEF
 1287       STMAXE = UNDEF
 1288       STMAXD = UNDEF
 1289       HCMAXE = UNDEF
 1290       HMAXE = UNDEF
 1291       HCMAXD = UNDEF
 1292       HMAXD = UNDEF
 1293       QP    = UNDEF
 1294       WBT    = UNDEF
 1295 !
 1296       FP1    = UNDEF
 1297       THP1   = UNDEF
 1298 !
 1299 ! 2.  Integral over discrete part of spectrum ------------------------ *
 1300 !
 1301       DO IK=1, NK
 1302 !
 1303 ! 2.a Initialize energy in band
 1304 !
 1305         AB     = 0.
 1306         ABX    = 0.
 1307         ABY    = 0.
 1308         ABX2   = 0.
 1309         ABY2   = 0.
 1310         AB2X   = 0.
 1311         AB2Y   = 0.
 1312         ABXX   = 0.
 1313         ABYY   = 0.
 1314         ABXY   = 0.
 1315         ABYX   = 0.
 1316         ABST   = 0.
 1317 !
 1318 ! 2.b Integrate energy in band
 1319 !
 1320         DO ITH=1, NTH
 
 
                                                         w3iogomd.F90  page  25
 
 
 1321 !
 1322           DO JSEA=1, NSEAL
 1323             NKH(JSEA)  = MIN ( NK ,   &
 1324                     INT(FACTI2+FACTI1*LOG(MAX(1.E-7,FCUT(JSEA)))) )
 1325             AB (JSEA)  = AB (JSEA) + A(ITH,IK,JSEA)
 1326             ABX(JSEA)  = ABX(JSEA) + A(ITH,IK,JSEA)*ECOS(ITH)
 1327             ABY(JSEA)  = ABY(JSEA) + A(ITH,IK,JSEA)*ESIN(ITH)
 1328 ! These are the integrals with cos^2 and sin^2
 1329             ABX2(JSEA) = ABX2(JSEA) + A(ITH,IK,JSEA)*EC2(ITH)
 1330             ABY2(JSEA) = ABY2(JSEA) + A(ITH,IK,JSEA)*ES2(ITH)
 1331 ! Using trig identities to represent cos2theta and sin2theta.
 1332             AB2X(JSEA) = AB2X(JSEA) + A(ITH,IK,JSEA)*(2*EC2(ITH) - 1)
 1333             AB2Y(JSEA) = AB2Y(JSEA) + A(ITH,IK,JSEA)*(2*ESC(ITH))
 1334             ABYX(JSEA) = ABYX(JSEA) + A(ITH,IK,JSEA)*ESC(ITH)
 1335             IF (ITH.LE.NTH/2) THEN
 1336               ABST(JSEA) = ABST(JSEA) +                               &
 1337                               A(ITH,IK,JSEA)*A(ITH+NTH/2,IK,JSEA)
 1338               END IF
 1339             CALL INIT_GET_ISEA(ISEA, JSEA)
 1340             FACTOR     = MAX ( 0.5 , CG(IK,ISEA)/SIG(IK)*WN(IK,ISEA) )
 1341             ABXX(JSEA) = ABXX(JSEA) + ((1.+EC2(ITH))*FACTOR-0.5) *    &
 1342                                      A(ITH,IK,JSEA)
 1343             ABYY(JSEA) = ABYY(JSEA) + ((1.+ES2(ITH))*FACTOR-0.5) *    &
 1344                                      A(ITH,IK,JSEA)
 1345             ABXY(JSEA) = ABXY(JSEA) + ESC(ITH)*FACTOR * A(ITH,IK,JSEA)
 1346             END DO
 1347 !
 1348           END DO
 1349 !
 1350 ! 2.c Finalize integration over band and update mean arrays
 1351 !
 1352         DO JSEA=1, NSEAL
 1353           CALL INIT_GET_ISEA(ISEA, JSEA)
 1354           FACTOR       = DDEN(IK) / CG(IK,ISEA)
 1355           EBD(IK,JSEA) = AB(JSEA) * FACTOR
 1356           ET (JSEA)    = ET (JSEA) + EBD(IK,JSEA)
 1357      IF (IK.EQ.NINT(IGPARS(5))) HSIG(JSEA) = 4*SQRT(ET(JSEA))
 1358           ETF(JSEA)  = ETF(JSEA) + EBD(IK,JSEA) * CG(IK,ISEA)
 1359           EWN(JSEA)  = EWN(JSEA) + EBD(IK,JSEA) / WN(IK,ISEA)
 1360           ETR(JSEA)  = ETR(JSEA) + EBD(IK,JSEA) / SIG(IK)
 1361           ET1(JSEA)  = ET1(JSEA) + EBD(IK,JSEA) * SIG(IK)
 1362           EET1(JSEA) = EET1(JSEA)+ EBD(IK,JSEA)**2 * SIG(IK)
 1363           ET02(JSEA) = ET02(JSEA)+ EBD(IK,JSEA) * SIG(IK)**2
 1364           ETX(JSEA)  = ETX(JSEA) + ABX(JSEA) * FACTOR
 1365           ETY(JSEA)  = ETY(JSEA) + ABY(JSEA) * FACTOR
 1366           TUSX(JSEA) = TUSX(JSEA) + ABX(JSEA)*FACTOR               &
 1367                      *GRAV*WN(IK,ISEA)/SIG(IK)
 1368           TUSY(JSEA)  = TUSY(JSEA) + ABY(JSEA)*FACTOR               &
 1369                      *GRAV*WN(IK,ISEA)/SIG(IK)
 1370           ETXX(JSEA) = ETXX(JSEA) + ABX2(JSEA) * FACTOR* WN(IK,ISEA)**2
 1371           ETYY(JSEA) = ETYY(JSEA) + ABY2(JSEA) * FACTOR* WN(IK,ISEA)**2
 1372           ETXY(JSEA) = ETXY(JSEA) + ABYX(JSEA) * FACTOR* WN(IK,ISEA)**2
 1373           IF (SIG(IK)*0.5*(1+XFR).LT.0.4*TPI) THEN
 1374             ETMSSL(JSEA)  = ETMSSL(JSEA) + AB(JSEA)*FACTOR           &
 1375                      *WN(IK,ISEA)**2
 
 
                                                         w3iogomd.F90  page  26
 
 
 1376           ELSE
 1377             IF (SIG(MAX(IK-1,1))*0.5*(1+XFR).LT.0.4*TPI) THEN
 1378               ETMSSL(JSEA)  = ETMSSL(JSEA) + AB(JSEA)*FACTOR         &
 1379                      *(SIG(IK)*0.5*(1+1/XFR)-(0.4*TPI))/DSII(IK)     &
 1380                      *WN(IK,ISEA)**2
 1381               FACTOR2       = SIG(IK)**5/(GRAV**2)/DSII(IK)
 1382               ETMSSCL(JSEA) = AB(JSEA)*FACTOR*FACTOR2
 1383               END IF
 1384             END IF
 1385 !
 1386           UBS(JSEA) = UBS(JSEA) + AB(JSEA) * SIG(IK)**2
 1387 !
 1388 !   2nd order equivalent surface pressure spectral density at K=0
 1389 !   this is used for microseismic or microbarom sources
 1390 !   Finite water depth corrections (Ardhuin & Herbers 2013) are not
 1391 !   included here.
 1392 !
 1393           FACTOR2 = DTH*2/(TPI**2)                        &
 1394                     * SIG(IK)                             &
 1395                     * (TPI*SIG(IK)/CG(IK,ISEA))**2        &  ! Jacobian^2>
 1396                     * ABST(JSEA)
 1397 !
 1398 !   Integration over seismic radian frequency : *2*dsigma
 1399 !
 1400           PRMS(JSEA)  = PRMS(JSEA) + FACTOR2 * 2 * DSII(IK)
 1401           IF ( FLOLOC (6, 9).AND.(IK.GE.P2MSF(2).AND.IK.LE.P2MSF(3)))   &
 1402             P2SMS(JSEA,IK) = FACTOR2 * 2 * TPI
 1403           IF (FACTOR2 .GT. ETTPMM(JSEA)) THEN
 1404             ETTPMM(JSEA) = FACTOR2
 1405             TPMS(JSEA) = TPI/SIG(IK)
 1406             END IF
 1407                   
 1408 !
 1409 ! Directional moments in the last freq. band
 1410 !
 1411           IF (IK.EQ.NK) THEN
 1412             FACTOR2       = SIG(IK)**5/(GRAV**2)/DSII(IK)
 1413             ETUSCX(JSEA)  = ABX(JSEA)*FACTOR*FACTOR2
 1414             ETUSCY(JSEA)  = ABY(JSEA)*FACTOR*FACTOR2
 1415 !
 1416 !     NB: the slope PDF is proportional to ell1=ETYY*EC2-2*ETXY*ECS+ETYY*>
 1417 !     This is an ellipse equation with axis direction given by dir=0.5*AT>
 1418 !
 1419             MA  = ABX2(JSEA) * FACTOR * FACTOR2
 1420             MC  = ABY2(JSEA) * FACTOR * FACTOR2
 1421             MB  = ABYX(JSEA) * FACTOR * FACTOR2
 1422 !
 1423 ! Old definitions:  MSCX(JSEA)  = ABX2(JSEA) * FACTOR * FACTOR2
 1424 !                   MSCY(JSEA)  = ABY2(JSEA) * FACTOR * FACTOR2
 1425             MSCD(JSEA)=0.5*ATAN2(2*MB,MA-MC)
 1426                                             
 1427             MSCX(JSEA)= MA*COS(MSCD(JSEA))**2   &
 1428                        +2*MB*SIN(MSCD(JSEA))*COS(MSCD(JSEA))+MA*SIN(MSCD(>
 1429             MSCY(JSEA)= MC*COS(MSCD(JSEA))**2   &
 1430                        -2*MB*SIN(MSCD(JSEA))*COS(MSCD(JSEA))+MA*SIN(MSCD(>
 
 
                                                         w3iogomd.F90  page  27
 
 
 1431             END IF
 1432 !
 1433 ! Deep water limits
 1434 !
 1435           KD    = MAX ( 0.001 , WN(IK,ISEA) * DW(ISEA) )
 1436           IF ( KD .LT. 6. ) THEN
 1437             FKD       = FACTOR / SINH(KD)**2
 1438             ABR(JSEA) = ABR(JSEA) + AB(JSEA) * FKD
 1439             ABA(JSEA) = ABA(JSEA) + ABX(JSEA) * FKD
 1440             ABD(JSEA) = ABD(JSEA) + ABY(JSEA) * FKD
 1441             UBR(JSEA) = UBR(JSEA) + AB(JSEA) * SIG(IK)**2 * FKD
 1442             UBA(JSEA) = UBA(JSEA) + ABX(JSEA) * SIG(IK)**2 * FKD
 1443             UBD(JSEA) = UBD(JSEA) + ABY(JSEA) * SIG(IK)**2 * FKD
 1444             USSCO=FKD*SIG(IK)*WN(IK,ISEA)*COSH(2.*KD)
 1445             BHD(JSEA) = BHD(JSEA) +                             &
 1446                 GRAV*WN(IK,ISEA) * EBD(IK,JSEA) / (SINH(2.*KD))
 1447           ELSE
 1448             USSCO=FACTOR*SIG(IK)*2.*WN(IK,ISEA)
 1449             END IF
 1450 !
 1451           ABXX(JSEA)   = MAX ( 0. , ABXX(JSEA) ) * FACTOR
 1452           ABYY(JSEA)   = MAX ( 0. , ABYY(JSEA) ) * FACTOR
 1453           ABXY(JSEA)   = ABXY(JSEA) * FACTOR
 1454           SXX(JSEA)    = SXX(JSEA)  + ABXX(JSEA)
 1455           SYY(JSEA)    = SYY(JSEA)  + ABYY(JSEA)
 1456           SXY(JSEA)    = SXY(JSEA)  + ABXY(JSEA)
 1457           EBD(IK,JSEA) = EBD(IK,JSEA) / DSII(IK)
 1458 !
 1459           IF ( FLOLOC( 3, 1).AND.(IK.GE.E3DF(2,1).AND.IK.LE.E3DF(3,1)))  >
 1460             EF(JSEA,IK)  = EBD(IK,JSEA) * TPI
 1461 !
 1462           USSX(JSEA)  = USSX(JSEA) + ABX(JSEA)*USSCO
 1463           USSY(JSEA)  = USSY(JSEA) + ABY(JSEA)*USSCO
 1464 !
 1465 ! Fills the 3D Stokes drift spectrum array
 1466 !  ! The US3D Stokes drift specrum array is now calculated in a
 1467 !  subroutine and called at the end of this subroutine
 1468 !          IF ( FLOLOC( 6, 8).AND.(IK.GE.US3DF(2).AND.IK.LE.US3DF(3) ))  >
 1469 !            US3D(JSEA,IK)    =  ABX(JSEA)*USSCO/(DSII(IK)*TPIINV)
 1470 !            US3D(JSEA,NK+IK) =  ABY(JSEA)*USSCO/(DSII(IK)*TPIINV)
 1471 !          END IF
 1472           IF ( FLOLOC( 3, 2).AND.(IK.GE.E3DF(2,2).AND.IK.LE.E3DF(3,2)))  &
 1473             TH1M(JSEA,IK)= MOD ( 630. - RADE*ATAN2(ABY(JSEA),ABX(JSEA)) ,>
 1474           M1 = SQRT(ABX(JSEA)**2+ABY(JSEA)**2)/MAX(1E-20,AB(JSEA))
 1475           IF ( FLOLOC( 3, 3).AND.(IK.GE.E3DF(2,3).AND.IK.LE.E3DF(3,3)))  &
 1476              STH1M(JSEA,IK)= SQRT(ABS(2.*(1-M1)))*RADE
 1477           IF ( FLOLOC( 3, 4).AND.(IK.GE.E3DF(2,4).AND.IK.LE.E3DF(3,4)))  &
 1478             TH2M(JSEA,IK)= MOD ( 270. - RADE*0.5*ATAN2(ABY2(JSEA),AB2X(JS>
 1479           M2 = SQRT(AB2X(JSEA)**2+AB2Y(JSEA)**2)/MAX(1E-20,AB(JSEA))
 1480           IF ( FLOLOC( 3, 5).AND.(IK.GE.E3DF(2,5).AND.IK.LE.E3DF(3,5)))  &
 1481              STH2M(JSEA,IK)= SQRT(ABS(0.5*(1-M2)))*RADE
 1482           END DO
 1483 !
 1484         END DO
 1485 !
 
 
                                                         w3iogomd.F90  page  28
 
 
 1486 ! Start of Space-Time Extremes Section
 1487      IF ( ( STEXU .GT. 0. .AND. STEYU .GT. 0. ) &
 1488                                       .OR. ( STEDU .GT. 0. ) ) THEN
 1489 !  Space-Time extremes
 1490 !    (for references:
 1491 !     - Krogstad et al, OMAE 2004
 1492 !     - Baxevani and Rychlik, OE 2006
 1493 !     - Adler and Taylor, 2007
 1494 !     - Fedele, JPO 2012
 1495 !     - Fedele et al, OM 2013
 1496 !     - Benetazzo et al, JPO 2015)
 1497 !
 1498 !  Compute spectral parameters wrt the mean wave direction
 1499 !  (no tail contribution - Prognostic)
 1500       DO JSEA=1, NSEAL
 1501         ISEA   = JSEA
 1502         IX     = MAPSF(ISEA,1)
 1503         IY     = MAPSF(ISEA,2)
 1504         IF ( MAPSTA(IY,IX) .GT. 0 ) THEN
 1505           IF ( ABS(ETX(JSEA))+ABS(ETY(JSEA)) .GT. 1.E-7 ) THEN
 1506             THMP(JSEA) = ATAN2(ETY(JSEA),ETX(JSEA))
 1507             END IF
 1508           END IF
 1509         END DO
 1510 !
 1511       DO IK=1, NK
 1512 !
 1513         ABX2M = 0.
 1514         ABY2M = 0.
 1515         ABXM = 0.
 1516         ABYM = 0.
 1517         ABXYM = 0.
 1518 !
 1519         DO ITH=1, NTH
 1520 !
 1521           DO JSEA=1, NSEAL
 1522         ISEA      = JSEA
 1523             ABX2M(JSEA) = ABX2M(JSEA) + A(ITH,IK,JSEA)*                &
 1524               (ECOS(ITH)*COS(THMP(JSEA))+ESIN(ITH)*SIN(THMP(JSEA)))**2
 1525             ABY2M(JSEA) = ABY2M(JSEA) + A(ITH,IK,JSEA)*                &
 1526               (ESIN(ITH)*COS(THMP(JSEA))-ECOS(ITH)*SIN(THMP(JSEA)))**2
 1527             ABXM(JSEA)  = ABXM(JSEA) + A(ITH,IK,JSEA)*                 &
 1528               (ECOS(ITH)*COS(THMP(JSEA))+ESIN(ITH)*SIN(THMP(JSEA)))
 1529             ABYM(JSEA)  = ABYM(JSEA) + A(ITH,IK,JSEA)*                 &
 1530               (ESIN(ITH)*COS(THMP(JSEA))-ECOS(ITH)*SIN(THMP(JSEA)))
 1531             ABXYM(JSEA) = ABXYM(JSEA) + A(ITH,IK,JSEA)*                &
 1532               (ECOS(ITH)*COS(THMP(JSEA))+ESIN(ITH)*SIN(THMP(JSEA)))*   &
 1533               (ESIN(ITH)*COS(THMP(JSEA))-ECOS(ITH)*SIN(THMP(JSEA)))
 1534             END DO
 1535 !
 1536           END DO
 1537 !
 1538         DO JSEA=1, NSEAL
 1539           ISEA         = JSEA
 1540           FACTOR       = DDEN(IK) / CG(IK,ISEA)
 
 
                                                         w3iogomd.F90  page  29
 
 
 1541           MSSXM(JSEA)  = MSSXM(JSEA) + ABX2M(JSEA)*FACTOR*             &
 1542             WN(IK,ISEA)**2
 1543           MSSYM(JSEA)  = MSSYM(JSEA) + ABY2M(JSEA)*FACTOR*             &
 1544             WN(IK,ISEA)**2
 1545           MSSXTM(JSEA)  = MSSXTM(JSEA) + ABXM(JSEA)*FACTOR*WN(IK,ISEA)* &
 1546             SIG(IK)
 1547           MSSYTM(JSEA)  = MSSYTM(JSEA) + ABYM(JSEA)*FACTOR*WN(IK,ISEA)* &
 1548             SIG(IK)
 1549           MSSXYM(JSEA)  = MSSXYM(JSEA) + ABXYM(JSEA)*FACTOR*           &
 1550             WN(IK,ISEA)**2
 1551           END DO
 1552 !
 1553         END DO
 1554               
 1555 !
 1556         DO JSEA=1, NSEAL
 1557         ISEA      = JSEA
 1558         IX     = MAPSF(ISEA,1)
 1559         IY     = MAPSF(ISEA,2)
 1560 !
 1561 !  Mean wave period (no tail contribution - Prognostic)
 1562         IF ( ET02(JSEA) .GT. 1.E-7 ) THEN
 1563           T02P(JSEA) = TPI * SQRT(ET(JSEA) / ET02(JSEA) )
 1564           END IF
 1565 !
 1566 !  Mean wavelength and mean crest length (02) for space-time extremes
 1567         IF ( MSSXM(JSEA) .GT. 1.E-7 ) THEN
 1568           WL02X(JSEA) = TPI * SQRT(ET(JSEA) / MSSXM(JSEA))
 1569           END IF
 1570         IF ( MSSYM(JSEA) .GT. 1.E-7 ) THEN
 1571           WL02Y(JSEA) = TPI * SQRT(ET(JSEA) / MSSYM(JSEA))
 1572           END IF
 1573 !
 1574 !  Irregularity parameters for space-time extremes
 1575         IF ((MSSXM(JSEA) .GT. 1.E-7) .AND. (ET02(JSEA) .GT. 1.E-7)) THEN
 1576           ALPXT(JSEA) = MSSXTM(JSEA) / (SQRT(MSSXM(JSEA) * ET02(JSEA)))
 1577           ENDIF
 1578         IF ((MSSYM(JSEA) .GT. 1.E-7) .AND. (ET02(JSEA) .GT. 1.E-7)) THEN
 1579           ALPYT(JSEA) = MSSYTM(JSEA) / (SQRT(MSSYM(JSEA) * ET02(JSEA)))
 1580           ENDIF
 1581         IF ((MSSXM(JSEA) .GT. 1.E-7) .AND. (MSSYM(JSEA) .GT. 1.E-7)) THEN
 1582           ALPXY(JSEA) = MSSXYM(JSEA) / (SQRT(MSSXM(JSEA) * MSSYM(JSEA)))
 1583           ENDIF
 1584 !
 1585 !  Short-crestedness parameter
 1586         IF (MSSXM(JSEA) .GT. 1.E-7)  THEN
 1587           SCREST(JSEA) = SQRT(MSSYM(JSEA)/MSSXM(JSEA))
 1588           END IF
 1589 !
 1590 !  Space domain size (user-defined or default)
 1591         IF ( STEXU .GT. 0 .AND. STEYU .GT. 0 ) THEN
 1592           STEX = STEXU
 1593           STEY = STEYU
 1594         ELSE
 1595           STEX = 0.
 
 
                                                         w3iogomd.F90  page  30
 
 
 1596           STEY = 0.
 1597         END IF
 1598 !
 1599 !  Time domain size (user-defined or default)
 1600         IF ( STEDU .GT. 0 ) THEN
 1601           STED = STEDU
 1602           ELSE
 1603           STED = 0.
 1604           END IF
 1605 !
 1606 !  Average numbers of waves in the space-time domain (Volume+Sides+Border>
 1607         IF ((WL02X(JSEA) .GT. 1.E-7) .AND. (WL02Y(JSEA) .GT. 1.E-7)    &
 1608           .AND. (T02P(JSEA) .GT. 1.E-7)) THEN
 1609           NV(JSEA) = TPI*(STEX*STEY*STED)/                             &
 1610             (WL02X(JSEA)*WL02Y(JSEA)*T02P(JSEA))  *                    &
 1611             SQRT(1-ALPXT(JSEA)**2-ALPYT(JSEA)**2  -                    &
 1612             ALPXY(JSEA)**2+2*ALPXT(JSEA)*ALPYT(JSEA)*ALPXY(JSEA))
 1613           NS(JSEA) = SQRT(TPI)*((STEX*STED)/(WL02X(JSEA)*T02P(JSEA)) * &
 1614             SQRT(1-ALPXT(JSEA)**2) +                                   &
 1615             (STEY*STED)/(WL02Y(JSEA)*T02P(JSEA)) *                     &
 1616             SQRT(1-ALPYT(JSEA)**2) +                                   &
 1617             (STEX*STEY)/(WL02X(JSEA)*WL02Y(JSEA)) *                    &
 1618             SQRT(1-ALPXY(JSEA)**2))
 1619           NB(JSEA) = STEX/WL02X(JSEA) + STEY/WL02Y(JSEA) +             &
 1620                         STED/T02P(JSEA)
 1621           END IF
 1622 !
 1623 ! Integral measure of wave steepness (Fedele & Tayfun, 2009) MU, as a
 1624 ! function of the spectral width parameter NI (Longuet-Higgins, 1985)
 1625         IF (ET1(JSEA) .GT. 1.E-7) THEN
 1626           NI(JSEA) = SQRT(ET(JSEA)*ET02(JSEA)/ET1(JSEA)**2 - 1)
 1627           ENDIF
 1628         IF (ET(JSEA) .GT. 1.E-7) THEN
 1629           MU(JSEA) = ET1(JSEA)**2/GRAV * (ET(JSEA))**(-1.5) *          &
 1630             (1-NI(JSEA)+NI(JSEA)**2)
 1631           ENDIF
 1632 !
 1633 ! Mode of the Adler&Taylor distribution
 1634 ! (normalized on the standard deviation = Hs/4)
 1635 ! Time extremes
 1636       IF ((STEX .EQ. 0) .AND. (STEY .EQ. 0)) THEN
 1637         MODE(JSEA) = SQRT(2.*LOG(NB(JSEA)))
 1638 ! Space extremes (strictly for STEX*STEY >> WL02X*WL02Y)
 1639       ELSEIF (STED .EQ. 0) THEN
 1640         MODE(JSEA) = SQRT(2.*LOG(NS(JSEA))+LOG(2.*LOG(NS(JSEA))+     &
 1641                      LOG(2.*LOG(NS(JSEA)))))
 1642 ! Space-time extremes (strictly for STEX*STEY >> WL02X*WL02Y)
 1643       ELSEIF ((WL02X(JSEA) .GT. 1.E-7) .AND. (WL02Y(JSEA) .GT. 1.E-7) &
 1644           .AND. (T02P(JSEA) .GT. 1.E-7)) THEN
 1645         MODE(JSEA) = SQRT(2.*LOG(NV(JSEA))+2.*LOG(2.*LOG(NV(JSEA))+  &
 1646          2.*LOG(2.*LOG(NV(JSEA)))))
 1647       ENDIF
 1648 !
 1649 ! Expected maximum sea surface elevation in the ST domain - nonlinear
 1650 ! (in meters, Hs/4=SQRT(ET(JSEA)))
 
 
                                                         w3iogomd.F90  page  31
 
 
 1651       STMAXE(JSEA) = SQRT(ET(JSEA)) *                                &
 1652           ( MODE(JSEA)+0.5*MU(JSEA)*MODE(JSEA)**2 +                  &
 1653           0.5772*(1+MU(JSEA)*MODE(JSEA)) /                           &
 1654           (MODE(JSEA)-(2*NV(JSEA)*MODE(JSEA)+NS(JSEA)) /             &
 1655           (NV(JSEA)*MODE(JSEA)**2+NS(JSEA)*MODE(JSEA)+NB(JSEA))) )
 1656 !
 1657 ! Standard deviation of the maximum sea surface elevation in ST domain
 1658 !  - nonlinear (in meters, Hs/4=SQRT(ET(JSEA)))
 1659       STMAXD(JSEA) =  SQRT(ET(JSEA)) *                               &
 1660           ( PI*(1+MU(JSEA)*MODE(JSEA))/SQRT(6.) /                    &
 1661           (MODE(JSEA)-(2*NV(JSEA)*MODE(JSEA)+NS(JSEA)) /             &
 1662           (NV(JSEA)*MODE(JSEA)**2+NS(JSEA)*MODE(JSEA)+NB(JSEA))) )
 1663 !
 1664 ! Autocovariance (time) function (normalized on the maximum, i.e. total
 1665 ! variance)
 1666       IF (T02P(JSEA) .GT. 1.E-7) THEN
 1667         TLPHI(JSEA) = 0.3*T02P(JSEA)
 1668         DO ITL = 1, 21
 1669           DO IK = 1, NK-3, 4
 1670              PHI(ITL,JSEA) = PHI(ITL,JSEA) +                          &
 1671                  (XFR**3*EBD(IK+3,JSEA)*COS(XFR**3*SIG(IK)*TLPHI(JSEA))+ >
 1672                  XFR**2*EBD(IK+2,JSEA)*COS(XFR**2*SIG(IK)*TLPHI(JSEA))+  >
 1673                  XFR*EBD(IK+1,JSEA)*COS(XFR*SIG(IK)*TLPHI(JSEA)) +       >
 1674                  EBD(IK,JSEA)*COS(SIG(IK)*TLPHI(JSEA)))*DSII(IK)
 1675              ENDDO
 1676           TLPHI(JSEA) = TLPHI(JSEA) + T02P(JSEA)/20.
 1677           ENDDO
 1678           PHI(:,JSEA) = PHI(:,JSEA)/ET(JSEA)
 1679 !
 1680 ! First minimum of the autocovariance function (absolute value)
 1681         PHIST(JSEA) = ABS(MINVAL(PHI(:,JSEA),1))
 1682       ENDIF
 1683 !
 1684 ! Wave height of the wave with the maximum expected crest height
 1685 ! and corresponding standard deviation
 1686 ! (according to Boccotti Quasi-Determinism theory - linear)
 1687         STMAXEL(JSEA) = SQRT(ET(JSEA)) * ( MODE(JSEA)+0.5772 /         &
 1688             (MODE(JSEA)-(2*NV(JSEA)*MODE(JSEA)+NS(JSEA)) /             &
 1689             (NV(JSEA)*MODE(JSEA)**2+NS(JSEA)*MODE(JSEA)+NB(JSEA))) )
 1690         STMAXDL(JSEA) = SQRT(ET(JSEA)) *                               &
 1691             ( PI/SQRT(6.) /                                            &
 1692             (MODE(JSEA)-(2*NV(JSEA)*MODE(JSEA)+NS(JSEA)) /             &
 1693             (NV(JSEA)*MODE(JSEA)**2+NS(JSEA)*MODE(JSEA)+NB(JSEA))) )
 1694         HCMAXE(JSEA) = STMAXEL(JSEA)*(1+PHIST(JSEA))
 1695         HCMAXD(JSEA) = STMAXDL(JSEA)*(1+PHIST(JSEA))
 1696 ! Maximum expected wave height and corresponding standard deviation
 1697 ! (according to Boccotti Quasi-Determinism theory - linear)
 1698         HMAXE(JSEA) = STMAXEL(JSEA)*SQRT(2*(1+PHIST(JSEA)))
 1699         HMAXD(JSEA) = STMAXDL(JSEA)*SQRT(2*(1+PHIST(JSEA)))
 1700         ENDDO
 1701 !
 1702  
 1703 ! End of Space-Time Extremes Section
 1704      ENDIF
 1705 !
 
 
                                                         w3iogomd.F90  page  32
 
 
 1706 ! 3.  Finalize computation of mean parameters ------------------------ *
 1707 !
 1708       DO JSEA=1, NSEAL
 1709         CALL INIT_GET_ISEA(ISEA, JSEA)
 1710 !
 1711 ! 3.a Directional mss parameters
 1712 !     NB: the slope PDF is proportional to ell1=ETYY*EC2-2*ETXY*ECS+ETXX*>
 1713 !     This is an ellipse equation with axis direction given by dir=0.5*AT>
 1714 !     From matlab script: t0=0.5*(atan2(2.*B,A-C));
 1715 !     From matlab script: A2=A.*cos(t0).^2+2.*B.*sin(t0).*cos(t0)+A.*cos(>
 1716 !     From matlab script: C2=C.*cos(t0)^2-2.*B.*sin(t0).*cos(t0)+A.*sin(t>
 1717          MSSD(JSEA)=0.5*(ATAN2(2*ETXY(JSEA),ETXX(JSEA)-ETYY(JSEA)))
 1718          MSSX(JSEA)  = ETXX(JSEA)*COS(MSSD(JSEA))**2   &
 1719                        +2*ETXY(JSEA)*SIN(MSSD(JSEA))*COS(MSSD(JSEA))+ETYY>
 1720          MSSY(JSEA)  = ETYY(JSEA)*COS(MSSD(JSEA))**2   &
 1721                        -2*ETXY(JSEA)*SIN(MSSD(JSEA))*COS(MSSD(JSEA))+ETXX>
 1722 !
 1723 ! 3.b Add tail
 1724 !     ( DTH * SIG absorbed in FTxx )
 1725                                     
 1726         EBAND     = AB(JSEA) / CG(NK,ISEA)
 1727         ET (JSEA) = ET (JSEA) + FTE  * EBAND
 1728         EWN(JSEA) = EWN(JSEA) + FTWL * EBAND
 1729         ETF(JSEA) = ETF(JSEA) + GRAV * FTTR * EBAND  ! this is the integr>
 1730         ETR(JSEA) = ETR(JSEA) + FTTR * EBAND
 1731         ET1(JSEA) = ET1(JSEA) + FT1  * EBAND
 1732         EET1(JSEA)= ET1(JSEA) + FT1  * EBAND**2
 1733         ET02(JSEA)= ET02(JSEA)+ EBAND* 0.5 * SIG(NK)**4 * DTH
 1734         ETX(JSEA) = ETX(JSEA) + FTE * ABX(JSEA) / CG(NK,ISEA)
 1735         ETY(JSEA) = ETY(JSEA) + FTE * ABY(JSEA) / CG(NK,ISEA)
 1736         SXX(JSEA) = SXX(JSEA) + FTE * ABXX(JSEA) / CG(NK,ISEA)
 1737         SYY(JSEA) = SYY(JSEA) + FTE * ABYY(JSEA) / CG(NK,ISEA)
 1738         SXY(JSEA) = SXY(JSEA) + FTE * ABXY(JSEA) / CG(NK,ISEA)
 1739 !
 1740 ! Tail for surface stokes drift is commented out: very sensitive to tail >
 1741 !
 1742 !       USSX(JSEA)  = USSX(JSEA) + 2*GRAV*ETUSCX(JSEA)/SIG(NK)
 1743 !       USSY(JSEA)  = USSY(JSEA) + 2*GRAV*ETUSCY(JSEA)/SIG(NK)
 1744         UBS(JSEA) = UBS(JSEA) + FTWL * EBAND/GRAV
 1745         END DO
 1746 !
 1747       SXX    = SXX * DWAT * GRAV
 1748       SYY    = SYY * DWAT * GRAV
 1749       SXY    = SXY * DWAT * GRAV
 1750 !
 1751       DO JSEA=1, NSEAL
 1752         CALL INIT_GET_ISEA(ISEA, JSEA)
 1753         IX     = MAPSF(ISEA,1)
 1754         IY     = MAPSF(ISEA,2)
 1755         IF ( MAPSTA(IY,IX) .GT. 0 ) THEN
 1756             HS (JSEA) = 4. * SQRT ( ET(JSEA) )
 1757             IF ( ET(JSEA) .GT. 1.E-7 ) THEN
 1758                 QP(JSEA) = ( 2. / ET(JSEA)**2 ) * EET1(JSEA) * TPIINV**2
 1759                 WLM(JSEA) = EWN(JSEA) / ET(JSEA) * TPI
 1760                 T0M1(JSEA) = ETR(JSEA) / ET(JSEA) * TPI
 
 
                                                         w3iogomd.F90  page  33
 
 
 1761                 THS(JSEA) = RADE * SQRT ( MAX ( 0. , 2. * ( 1. - SQRT ( &
 1762                 MAX(0.,(ETX(JSEA)**2+ETY(JSEA)**2)/ET(JSEA)**2) ) ) ) )
 1763                 IF ( THS(JSEA) .LT. 0.01*RADE*DTH ) THS(JSEA) = 0.
 1764               ELSE
 1765                 WLM(JSEA) = 0.
 1766                 T0M1(JSEA) = TPI / SIG(NK)
 1767                 THS(JSEA) = 0.
 1768               END IF
 1769             IF ( ABS(ETX(JSEA))+ABS(ETY(JSEA)) .GT. 1.E-7 ) THEN
 1770                 THM(JSEA) = ATAN2(ETY(JSEA),ETX(JSEA))
 1771               ELSE
 1772                 THM(JSEA) = 0.
 1773               END IF
 1774             ABR(JSEA) = SQRT ( 2. * MAX ( 0. , ABR(JSEA) ) )
 1775             IF ( ABR(JSEA) .GE. 1.E-7 ) THEN
 1776                 ABD(JSEA) = ATAN2(ABD(JSEA),ABA(JSEA))
 1777               ELSE
 1778                 ABD(JSEA) = 0.
 1779               ENDIF
 1780             ABA(JSEA) = ABR(JSEA)
 1781             UBR(JSEA) = SQRT ( 2. * MAX ( 0. , UBR(JSEA) ) )
 1782             IF ( UBR(JSEA) .GE. 1.E-7 ) THEN
 1783                 UBD(JSEA) = ATAN2(UBD(JSEA),UBA(JSEA))
 1784               ELSE
 1785                 UBD(JSEA) = 0.
 1786               ENDIF
 1787             UBA(JSEA) = UBR(JSEA)
 1788             CGE(JSEA) = DWAT*GRAV*ETF(JSEA)
 1789             IF ( ET02(JSEA) .GT. 1.E-7  .AND.  ET(JSEA) .GT. 0 ) THEN
 1790               T02(JSEA) = TPI * SQRT(ET(JSEA) / ET02(JSEA) )
 1791               T01(JSEA) = TPI * ET(JSEA) / ET1(JSEA)
 1792             ELSE
 1793               T02(JSEA) = TPI / SIG(NK)
 1794               T01(JSEA)= T02(JSEA)
 1795               ENDIF
 1796 !
 1797 !  Add here USERO(JSEA,1) ...
 1798 !
 1799           END IF
 1800         END DO
 1801 !
 1802 ! 3.b Clean-up small values if !/O8 switch selected
 1803 !
 1804 ! 4.  Peak frequencies and directions -------------------------------- *
 1805 ! 4.a Initialize
 1806 !
 1807       DO JSEA=1, NSEAL
 1808         CALL INIT_GET_ISEA(ISEA, JSEA)
 1809         EC  (JSEA) = EBD(NK,JSEA)
 1810         FP0 (JSEA) = UNDEF
 1811         IKP0(JSEA) = 0
 1812         THP0(JSEA) = UNDEF
 1813         FP1 (JSEA) = UNDEF
 1814         IKP1(JSEA) = 0
 1815         THP1(JSEA) = UNDEF
 
 
                                                         w3iogomd.F90  page  34
 
 
 1816         END DO
 1817 !
 1818 ! 4.b Discrete peak frequencies
 1819 !
 1820       DO IK=NK-1, 2, -1
 1821 !
 1822         DO JSEA=1, NSEAL
 1823           CALL INIT_GET_ISEA(ISEA, JSEA)
 1824           IF ( EC(JSEA) .LT. EBD(IK,JSEA) ) THEN
 1825               EC  (JSEA) = EBD(IK,JSEA)
 1826               IKP0(JSEA) = IK
 1827             END IF
 1828           IF ( IKP1(JSEA).EQ.0                             &
 1829                  .AND. EBD(IK-1,JSEA).LT.EBD(IK,JSEA)      &
 1830                  .AND. EBD(IK-1,JSEA).LT.EBD(IK+1,JSEA)    &
 1831                  .AND. SIG(IK).GT.FXPMC/MAX(1.E-4,UST(ISEA)) &
 1832                  .AND. SIG(IK).LT.0.75*SIG(NK) )           &
 1833               IKP1(JSEA) = IK
 1834           END DO
 1835 !
 1836         END DO
 1837 !
 1838       DO JSEA=1, NSEAL
 1839         CALL INIT_GET_ISEA(ISEA, JSEA)
 1840         IF ( IKP0(JSEA) .NE. 0 ) FP0(JSEA) = SIG(IKP0(JSEA)) * TPIINV
 1841         IF ( IKP1(JSEA) .NE. 0 ) FP1(JSEA) = SIG(IKP1(JSEA)) * TPIINV
 1842         END DO
 1843 !
 1844 ! 4.c Continuous peak frequencies
 1845 !
 1846       XL     = 1./XFR - 1.
 1847       XH     =  XFR - 1.
 1848       XL2    = XL**2
 1849       XH2    = XH**2
 1850 !
 1851       DO JSEA=1, NSEAL
 1852         CALL INIT_GET_ISEA(ISEA, JSEA)
 1853         ILOW   = MAX (  1 , IKP0(JSEA)-1 )
 1854         ICEN   = MAX (  1 , IKP0(JSEA)   )
 1855         IHGH   = MIN ( NK , IKP0(JSEA)+1 )
 1856         EL     = EBD(ILOW,JSEA) - EBD(ICEN,JSEA)
 1857         EH     = EBD(IHGH,JSEA) - EBD(ICEN,JSEA)
 1858         DENOM  = XL*EH - XH*EL
 1859         FP0(JSEA) = FP0 (JSEA) * ( 1. + 0.5 * ( XL2*EH - XH2*EL )     &
 1860                        / SIGN ( MAX(ABS(DENOM),1.E-15) , DENOM ) )
 1861         ILOW   = MAX (  1 , IKP1(JSEA)-1 )
 1862         ICEN   = MAX (  1 , IKP1(JSEA)   )
 1863         IHGH   = MIN ( NK , IKP1(JSEA)+1 )
 1864         EL     = EBD(ILOW,JSEA) - EBD(ICEN,JSEA)
 1865         EH     = EBD(IHGH,JSEA) - EBD(ICEN,JSEA)
 1866         DENOM  = XL*EH - XH*EL
 1867         FP1(JSEA) = FP1(JSEA) * ( 1. + 0.5 * (XL2*EH - XH2*EL )  &
 1868                        / SIGN ( MAX(ABS(DENOM),1.E-15) , DENOM ) )
 1869         END DO
 1870 !
 
 
                                                         w3iogomd.F90  page  35
 
 
 1871 ! 4.d Peak directions
 1872 !
 1873       DO JSEA=1, NSEAL
 1874         ETX(JSEA) = 0.
 1875         ETY(JSEA) = 0.
 1876         END DO
 1877 !
 1878       DO ITH=1, NTH
 1879 !
 1880         DO JSEA=1, NSEAL
 1881           CALL INIT_GET_ISEA(ISEA, JSEA)
 1882           IF (IKP0(JSEA).NE.0) THEN
 1883               ETX(JSEA) = ETX(JSEA) + A(ITH,IKP0(JSEA),JSEA)*ECOS(ITH)
 1884               ETY(JSEA) = ETY(JSEA) + A(ITH,IKP0(JSEA),JSEA)*ESIN(ITH)
 1885             END IF
 1886           END DO
 1887 !
 1888         END DO
 1889 !
 1890       DO JSEA=1, NSEAL
 1891         CALL INIT_GET_ISEA(ISEA, JSEA)
 1892         IF ( ABS(ETX(JSEA))+ABS(ETY(JSEA)) .GT. 1.E-7 .AND.           &
 1893              FP0(JSEA).NE.UNDEF )                                     &
 1894             THP0(JSEA) = ATAN2(ETY(JSEA),ETX(JSEA))
 1895         ETX(JSEA) = 0.
 1896         ETY(JSEA) = 0.
 1897         IKP1(JSEA) = MAX ( 1 , IKP1(JSEA) )
 1898         END DO
 1899 !
 1900       DO ITH=1, NTH
 1901 !
 1902         DO JSEA=1, NSEAL
 1903           CALL INIT_GET_ISEA(ISEA, JSEA)
 1904           IF ( FP1(JSEA).NE.UNDEF) THEN
 1905               ETX(JSEA) = ETX(JSEA) + A(ITH,IKP1(JSEA),JSEA)*ECOS(ITH)
 1906               ETY(JSEA) = ETY(JSEA) + A(ITH,IKP1(JSEA),JSEA)*ESIN(ITH)
 1907             END IF
 1908           END DO
 1909 !
 1910         END DO
 1911 !
 1912       DO JSEA =1, NSEAL
 1913         CALL INIT_GET_ISEA(ISEA, JSEA)
 1914         IX          = MAPSF(ISEA,1)
 1915         IY          = MAPSF(ISEA,2)
 1916         IF ( MAPSTA(IY,IX) .LE. 0 ) THEN
 1917             FP0 (JSEA) = UNDEF
 1918             THP0(JSEA) = UNDEF
 1919             FP1 (JSEA) = UNDEF
 1920           END IF
 1921         END DO
 1922 !
 1923       DO JSEA=1, NSEAL
 1924         CALL INIT_GET_ISEA(ISEA, JSEA)
 1925         IF ( ABS(ETX(JSEA))+ABS(ETY(JSEA)) .GT. 1.E-7 .AND.           &
 
 
                                                         w3iogomd.F90  page  36
 
 
 1926              FP1(JSEA) .NE. UNDEF )                                   &
 1927             THP1(JSEA) = ATAN2(ETY(JSEA),ETX(JSEA))
 1928         END DO
 1929 !
 1930 ! 5.  Test output (local to MPP only)
 1931 !
 1932 ! 6.  Fill arrays wth partitioned data
 1933 !
 1934       IF ( FLPART ) THEN
 1935 !
 1936 ! 6.a Initializations
 1937 !
 1938           PHS    = UNDEF
 1939           PTP    = UNDEF
 1940           PLP    = UNDEF
 1941           PDIR   = UNDEF
 1942           PSI    = UNDEF
 1943           PWS    = UNDEF
 1944           PWST   = UNDEF
 1945           PNR    = UNDEF
 1946           PTHP0  = UNDEF
 1947           PQP    = UNDEF
 1948           PPE    = UNDEF
 1949           PGW    = UNDEF
 1950           PSW    = UNDEF
 1951           PTM1   = UNDEF
 1952           PT1    = UNDEF
 1953           PT2    = UNDEF
 1954           PEP    = UNDEF
 1955 !
 1956 ! 6.b Loop over local sea points
 1957 !
 1958           DO JSEA=1, NSEAL
 1959             CALL INIT_GET_ISEA(ISEA, JSEA)
 1960             IX          = MAPSF(ISEA,1)
 1961             IY          = MAPSF(ISEA,2)
 1962 !
 1963             IF ( MAPSTA(IY,IX).GT.0 ) THEN
 1964                 I         = ICPRT(JSEA,2)
 1965                 PNR(JSEA) = MAX ( 0. , REAL(ICPRT(JSEA,1)-1) )
 1966                 IF ( ICPRT(JSEA,1).GE.1 ) PWST(JSEA) = DTPRT(6,I)
 1967               END IF
 1968 !
 1969             IF ( MAPSTA(IY,IX).GT.0 .AND. ICPRT(JSEA,1).GT.1 ) THEN
 1970                 I      = ICPRT(JSEA,2) + 1
 1971                 IF ( DTPRT(6,I) .GE. WSCUT ) THEN
 1972                     PHS(JSEA,0) = DTPRT(1,I)
 1973                     PTP(JSEA,0) = DTPRT(2,I)
 1974                     PLP(JSEA,0) = DTPRT(3,I)
 1975                     ! (PDIR is already in degrees nautical - convert back>
 1976                     !  Cartesian in radians to maintain internal conventi>
 1977                     IF(DTPRT(4,I) .NE. UNDEF) THEN
 1978                       PDIR(JSEA,0) = (270. - DTPRT(4,I)) * DERA
 1979                     ENDIF
 1980                     PSI(JSEA,0) = DTPRT(5,I)
 
 
                                                         w3iogomd.F90  page  37
 
 
 1981                     PWS(JSEA,0) = DTPRT(6,I)
 1982                     ! (PTHP0 is already in degrees nautical - convert bac>
 1983                     !  Cartesian in radians to maintain internal conventi>
 1984                     IF(DTPRT(7,I) .NE. UNDEF) THEN
 1985                       PTHP0(JSEA,0) = (270. - DTPRT(7,I)) * DERA
 1986                     ENDIF
 1987                     PSW(JSEA,0) = DTPRT(8,I)
 1988                     PPE(JSEA,0) = DTPRT(9,I)
 1989                     PQP(JSEA,0) = DTPRT(10,I)
 1990                     PGW(JSEA,0) = DTPRT(11,I)
 1991                     PTM1(JSEA,0) = DTPRT(12,I)
 1992                     PT1(JSEA,0) = DTPRT(13,I)
 1993                     PT2(JSEA,0) = DTPRT(14,I)
 1994                     PEP(JSEA,0) = DTPRT(15,I)
 1995                     I      = I + 1
 1996                   END IF
 1997                 DO J=1, NOSWLL
 1998                   IF ( I .GT.  ICPRT(JSEA,2)+ICPRT(JSEA,1)-1 ) EXIT
 1999                   PHS(JSEA,J) = DTPRT(1,I)
 2000                   PTP(JSEA,J) = DTPRT(2,I)
 2001                   PLP(JSEA,J) = DTPRT(3,I)
 2002                   ! (PDIR is already in degrees nautical - convert back to
 2003                   !  Cartesian in radians to maintain internal convention)
 2004                   IF(DTPRT(4,I) .NE. UNDEF) THEN
 2005                     PDIR(JSEA,J) = (270. - DTPRT(4,I)) * DERA
 2006                   ENDIF
 2007                   PSI(JSEA,J) = DTPRT(5,I)
 2008                   PWS(JSEA,J) = DTPRT(6,I)
 2009                   ! (PTHP0 is already in degrees nautical - convert back >
 2010                   !  Cartesian in radians to maintain internal convention)
 2011                   IF(DTPRT(7,I) .NE. UNDEF) THEN
 2012                     PTHP0(JSEA,J) = (270. - DTPRT(7,I)) * DERA
 2013                   ENDIF
 2014                   PSW(JSEA,J) = DTPRT(8,I)
 2015                   PPE(JSEA,J) = DTPRT(9,I)
 2016                   PQP(JSEA,J) = DTPRT(10,I)
 2017                   PGW(JSEA,J) = DTPRT(11,I)
 2018                   PTM1(JSEA,J) = DTPRT(12,I)
 2019                   PT1(JSEA,J) = DTPRT(13,I)
 2020                   PT2(JSEA,J) = DTPRT(14,I)
 2021                   PEP(JSEA,J) = DTPRT(15,I)
 2022                   I      = I + 1
 2023                   END DO
 2024               END IF
 2025 !
 2026             END DO
 2027 !
 2028  
 2029         END IF
 2030               
 2031       IF (FLOLOC( 6, 8)) THEN
 2032         CALL CALC_U3STOKES(A,1)
 2033       END IF
 2034 !
 2035       IF (FLOLOC( 6, 12)) THEN
 
 
                                                         w3iogomd.F90  page  38
 
 
 2036          CALL CALC_U3STOKES(A,2)
 2037       ENDIF
 2038 !
 2039 ! Dominant wave breaking probability
 2040 !
 2041       IF (FLOLOC(2, 17)) CALL CALC_WBT(A)
 2042 !
 2043       RETURN
 2044 !
 2045 ! Formats
 2046 !
 2047  
 2048 !/
 2049 !/ End of W3OUTG ----------------------------------------------------- /
 2050 !/
 2051       END SUBROUTINE W3OUTG
 2052 !/ ------------------------------------------------------------------- /
 2053 !/
 2054       SUBROUTINE W3IOGO ( INXOUT, NDSOG, IOTST, IMOD )
 2055 !/
 2056 !/                  +-----------------------------------+
 2057 !/                  | WAVEWATCH III           NOAA/NCEP |
 2058 !/                  |           H. L. Tolman            |
 2059 !/                  |                        FORTRAN 90 |
 2060 !/                  | Last update :         27-Nov-2013 |
 2061 !/                  +-----------------------------------+
 2062 !/
 2063 !/    17-Mar-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
 2064 !/    04-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
 2065 !/                  Major changes to logistics.
 2066 !/    24-Jan-2001 : Flat grid version (formats only)    ( version 2.06 )
 2067 !/    23-Apr-2002 : Clean up                            ( version 2.19 )
 2068 !/    29-Apr-2002 : Add output types 17-18.             ( version 2.20 )
 2069 !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
 2070 !/    25-Oct-2004 : Multiple grid version.              ( version 3.06 )
 2071 !/    27-Jun-2005 : Adding MAPST2.                      ( version 3.07 )
 2072 !/    21-Jul-2005 : Adding output fields 19-21.         ( version 3.07 )
 2073 !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
 2074 !/    05-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
 2075 !/    02-Apr-2007 : Adding partitioned output.          ( version 3.11 )
 2076 !/                  Adding user slots for outputs.
 2077 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
 2078 !/                  (W. E. Rogers & T. J. Campbell, NRL)
 2079 !/    31-Oct-2010 : Implement unstructured grids        ( version 3.14 )
 2080 !/                  (A. Roland and F. Ardhuin)
 2081 !/    05-Feb-2011 : Renumbering of output fields        ( version 3.14 )
 2082 !/                  (F. Ardhuin)
 2083 !/    25-Dec-2012 : New output structure and smaller    ( version 4.11 )
 2084 !/                  memory footprint.
 2085 !/    21-Aug-2013 : Added missing cos,sin for UBA, ABA  ( version 4.11 )
 2086 !/    27-Nov-2013 : Management of coupling output       ( version 4.18 )
 2087 !/    01-Mar-2018 : Removed RTD code (now used in post  ( version 6.02 )
 2088 !/                  processing code)
 2089 !/    25-Aug-2018 : Add WBT parameter                   ( version 6.06 )
 2090 !/
 
 
                                                         w3iogomd.F90  page  39
 
 
 2091 !  1. Purpose :
 2092 !
 2093 !     Read/write gridded output.
 2094 !
 2095 !  2. Method :
 2096 !
 2097 !     Fields in file are determined by flags in FLOGRD in W3ODATMD.
 2098 !
 2099 !  3. Parameters :
 2100 !
 2101 !     Parameter list
 2102 !     ----------------------------------------------------------------
 2103 !       INXOUT  C*(*)  I   Test string for read/write, valid are:
 2104 !                          'READ' and 'WRITE'.
 2105 !       NDSOG   Int.   I   File unit number.
 2106 !       IOTST   Int.   O   Test indictor for reading.
 2107 !                           0 : Fields read.
 2108 !                          -1 : Past end of file.
 2109 !       IMOD    Int.   I   Model number for W3GDAT etc.
 2110 !     ----------------------------------------------------------------
 2111 !
 2112 !  4. Subroutines used :
 2113 !
 2114 !       See module documentation above.
 2115 !
 2116 !  5. Called by :
 2117 !
 2118 !      Name      Type  Module   Description
 2119 !     ----------------------------------------------------------------
 2120 !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
 2121 !      WW3_OUTF  Prog.   N/A    Ouput postprocessor.
 2122 !      WW3_GRIB  Prog.   N/A    Ouput postprocessor.
 2123 !      GX_OUTF   Prog.   N/A    Ouput postprocessor.
 2124 !     ----------------------------------------------------------------
 2125 !
 2126 !  6. Error messages :
 2127 !
 2128 !       Tests on INXOUT, file status and on array dimensions.
 2129 !
 2130 !  7. Remarks :
 2131 !
 2132 !     - MAPSTA is dumped as it contains information on the ice edge.
 2133 !       Dynamic ice edges require MAPSTA to be dumped every time step.
 2134 !     - The output file has the pre-defined name 'out_grd.FILEXT'.
 2135 !     - The current components CX and CY are written to out_grd as
 2136 !       components, but converted to magnitude and direction in most
 2137 !       gridded and point output post-processors (except gx_outf).
 2138 !     - All written direction are in degrees, nautical convention,
 2139 !       but in reading, all is convered back to radians and cartesian
 2140 !       conventions.
 2141 !     - Before writing, wind and current directions are converted,
 2142 !       wave directions are already in correct convention (see W3OUTG).
 2143 !     - In MPP version of model data is supposed to be gatherd at the
 2144 !       correct processor before the routine is called.
 2145 !     - In MPP version routine is called by only one process, therefore
 
 
                                                         w3iogomd.F90  page  40
 
 
 2146 !       no test on process for error messages is needed.
 2147 !
 2148 !  8. Structure :
 2149 !
 2150 !     See source code.
 2151 !
 2152 !  9. Switches :
 2153 !
 2154 !     !/ST1   First source term package (WAM3).
 2155 !     !/ST2   Second source term package (TC96).
 2156 !     !/S     Enable subroutine tracing.
 2157 !     !/T     Test output.
 2158 !
 2159 ! 10. Source code :
 2160 !
 2161 !/ ------------------------------------------------------------------- /
 2162      USE CONSTANTS
 2163      USE W3GDATMD
 2164 !/
 2165       USE W3WDATMD, ONLY: W3SETW, W3DIMW
 2166       USE W3ADATMD, ONLY: W3SETA, W3DIMA, W3XETA
 2167       USE W3ODATMD, ONLY: W3SETO
 2168 !/
 2169       USE W3WDATMD, ONLY: TIME, DINIT, WLV, ICE, ICEF, ICEH, BERG,    &
 2170                           UST,  USTDIR, ASF
 2171       USE W3ADATMD, ONLY: AINIT, DW, UA, UD, AS, CX, CY, WN
 2172       USE W3ADATMD, ONLY: HS, WLM, T02, T0M1, T01, FP0, THM, THS, THP0,&
 2173                           WBT
 2174       USE W3ADATMD, ONLY: FP1, THP1, DTDYN, &
 2175                           FCUT, ABA, ABD, UBA, UBD, SXX, SYY, SXY,     &
 2176                           PHS, PTP, PLP, PDIR, PSI, PWS, PWST, PNR,    &
 2177                           PTHP0, PQP, PPE, PGW, PSW, PTM1, PT1, PT2,  &
 2178                           PEP, USERO, TAUOX, TAUOY, TAUWIX, TAUWIY,    &
 2179                           PHIAW, PHIOC, TUSX, TUSY, PRMS, TPMS,        &
 2180                           USSX, USSY, MSSX, MSSY, MSSD, MSCX, MSCY,    &
 2181                           MSCD, QP, TAUWNX, TAUWNY, CHARN, TWS, BHD,   &
 2182                           PHIBBL, TAUBBL, WHITECAP, BEDFORMS, CGE, EF, &
 2183                           CFLXYMAX, CFLTHMAX, CFLKMAX, P2SMS, US3D,    &
 2184                           TH1M, STH1M, TH2M, STH2M, HSIG, PHICE, TAUICE,&
 2185                           STMAXE, STMAXD, HMAXE, HCMAXE, HMAXD, HCMAXD,&
 2186                           USSP
 2187 !/
 2188       USE W3ODATMD, ONLY: NOGRP, NGRPP, IDOUT, UNDEF, NDST, NDSE,     &
 2189                           FLOGRD, IPASS => IPASS1, WRITE => WRITE1,   &
 2190                           FNMPRE, NOSWLL, NOEXTR
 2191 !/
 2192       USE W3SERVMD, ONLY: EXTCDE
 2193       USE W3ODATMD, only : IAPROC
 2194       USE W3ODATMD, ONLY :  OFILES
 2195 !
 2196       IMPLICIT NONE
 2197 !/
 2198 !/ ------------------------------------------------------------------- /
 2199 !/ Parameter list
 2200 !/
 
 
                                                         w3iogomd.F90  page  41
 
 
 2201       INTEGER, INTENT(INOUT)        :: IOTST
 2202       INTEGER, INTENT(IN)           :: NDSOG
 2203       INTEGER, INTENT(IN), OPTIONAL :: IMOD
 2204       CHARACTER, INTENT(IN)         :: INXOUT*(*)
 2205       CHARACTER(LEN=15) :: TIMETAG
 2206 !/
 2207 !/ ------------------------------------------------------------------- /
 2208 !/ Local parameters
 2209 !/
 2210       INTEGER                 :: IGRD, IERR, I, J, IX, IY, MOGRP,     &
 2211                                  MGRPP, ISEA, MOSWLL, IK, IFI, IFJ    &
 2212                                  ,IFILOUT
 2213       INTEGER, ALLOCATABLE    :: MAPTMP(:,:)
 2214       REAL                    :: AUX1(NSEA), AUX2(NSEA),              &
 2215                                  AUX3(NSEA), AUX4(NSEA)
 2216       CHARACTER(LEN=30)       :: IDTST, TNAME
 2217       CHARACTER(LEN=10)       :: VERTST
 2218 !/
 2219 !/ ------------------------------------------------------------------- /
 2220 !/
 2221 !
 2222 ! test input parameters ---------------------------------------------- *
 2223 !
 2224       IF ( PRESENT(IMOD) ) THEN
 2225           IGRD   = IMOD
 2226         ELSE
 2227           IGRD   = 1
 2228         END IF
 2229 !
 2230       CALL W3SETO ( IGRD, NDSE, NDST )
 2231       CALL W3SETG ( IGRD, NDSE, NDST )
 2232       CALL W3SETA ( IGRD, NDSE, NDST )
 2233       CALL W3SETW ( IGRD, NDSE, NDST )
 2234 !
 2235       IPASS  = IPASS + 1
 2236       IOTST  = 0
 2237 !
 2238       IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE' ) THEN
 2239           WRITE (NDSE,900) INXOUT
 2240           CALL EXTCDE ( 1 )
 2241         END IF
 2242 !
 2243       IF ( IPASS.EQ.1 .AND. OFILES(1) .EQ. 0) THEN
 2244           WRITE  = INXOUT.EQ.'WRITE'
 2245         ELSE
 2246           IF ( WRITE .AND. INXOUT.EQ.'READ' ) THEN
 2247               WRITE (NDSE,901) INXOUT
 2248               CALL EXTCDE ( 2 )
 2249             END IF
 2250         END IF
 2251 !
 2252 ! open file ---------------------------------------------------------- *
 2253 ! ( IPASS = 1 )
 2254 !
 2255       IF ( IPASS.EQ.1 .AND. OFILES(1) .EQ. 0) THEN
 
 
                                                         w3iogomd.F90  page  42
 
 
 2256           I      = LEN_TRIM(FILEXT)
 2257           J      = LEN_TRIM(FNMPRE)
 2258 !
 2259           IF ( WRITE ) THEN
 2260               OPEN (NDSOG,FILE=FNMPRE(:J)//'out_grd.'//FILEXT(:I),    &
 2261                     FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
 2262             ELSE
 2263               OPEN (NDSOG,FILE=FNMPRE(:J)//'out_grd.'//FILEXT(:I),    &
 2264                     FORM='UNFORMATTED',ERR=800,IOSTAT=IERR,STATUS='OLD')
 2265             END IF
 2266 !
 2267           REWIND ( NDSOG )
 2268 !
 2269 ! test info --------------------------------------------------------- *
 2270 ! ( IPASS = 1 )
 2271 !
 2272           IF ( WRITE ) THEN
 2273               WRITE (NDSOG)                                           &
 2274                 IDSTR, VEROGR, GNAME, NOGRP, NGRPP, NSEA, NX, NY,     &
 2275                 UNDEF, NOSWLL
 2276             ELSE
 2277               READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)                &
 2278                 IDTST, VERTST, TNAME, MOGRP, MGRPP, NSEA, NX, NY,     &
 2279                 UNDEF, MOSWLL
 2280 !
 2281               IF ( IDTST .NE. IDSTR ) THEN
 2282                   WRITE (NDSE,902) IDTST, IDSTR
 2283                   CALL EXTCDE ( 20 )
 2284                 END IF
 2285               IF ( VERTST .NE. VEROGR ) THEN
 2286                   WRITE (NDSE,903) VERTST, VEROGR
 2287                   CALL EXTCDE ( 21 )
 2288                 END IF
 2289               IF ( NOGRP .NE. MOGRP .OR. NGRPP .NE. MGRPP ) THEN
 2290                   WRITE (NDSE,904) MOGRP, MGRPP, NOGRP, NGRPP
 2291                   CALL EXTCDE ( 22 )
 2292                 END IF
 2293               IF ( TNAME .NE. GNAME ) THEN
 2294                   WRITE (NDSE,905) TNAME, GNAME
 2295                 END IF
 2296               IF ( NOSWLL .NE. MOSWLL ) THEN
 2297                   WRITE (NDSE,906) MOSWLL, NOSWLL
 2298                   CALL EXTCDE ( 24 )
 2299                 END IF
 2300 !
 2301             END IF
 2302 !
 2303         END IF
 2304 !
 2305 !  IN CASE OF GENERATION OF A NEW FILE OUTPUT EVERY DELTA OUTPUT
 2306 ! open file ---------------------------------------------------------- *
 2307 ! ( IPASS = 1 )
 2308 !
 2309       IF ( IPASS.GE.1 .AND. OFILES(1) .EQ. 1) THEN
 2310           WRITE  = INXOUT.EQ.'WRITE'
 
 
                                                         w3iogomd.F90  page  43
 
 
 2311         ELSE
 2312           IF ( WRITE .AND. INXOUT.EQ.'READ' ) THEN
 2313               WRITE (NDSE,901) INXOUT
 2314               CALL EXTCDE ( 2 )
 2315             END IF
 2316         END IF
 2317               
 2318 !
 2319       IF ( IPASS.GE.1 .AND. OFILES(1) .EQ. 1) THEN
 2320           I      = LEN_TRIM(FILEXT)
 2321           J      = LEN_TRIM(FNMPRE)
 2322 !
 2323 ! Create TIMETAG for file name using YYYYMMDD.HHMMS prefix
 2324           WRITE(TIMETAG,"(i8.8,'.'i6.6)")TIME(1),TIME(2)
 2325           IF ( WRITE ) THEN
 2326               OPEN (NDSOG,FILE=FNMPRE(:J)//TIMETAG//'.out_grd.'  &
 2327                     //FILEXT(:I),FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
 2328           ELSE
 2329               OPEN (NDSOG,FILE=FNMPRE(:J)//'out_grd.'//FILEXT(:I),    &
 2330                     FORM='UNFORMATTED',ERR=800,IOSTAT=IERR,STATUS='OLD')
 2331             END IF
 2332 !
 2333           REWIND ( NDSOG )
 2334 !
 2335 ! test info --------------------------------------------------------- *
 2336 ! ( IPASS >= 1 & OFILES(1) = 1)
 2337 !
 2338           IF ( WRITE ) THEN
 2339               WRITE (NDSOG)                                           &
 2340                 IDSTR, VEROGR, GNAME, NOGRP, NGRPP, NSEA, NX, NY,     &
 2341                 UNDEF, NOSWLL
 2342             ELSE
 2343               READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)                &
 2344                 IDTST, VERTST, TNAME, MOGRP, MGRPP, NSEA, NX, NY,     &
 2345                 UNDEF, MOSWLL
 2346 !
 2347               IF ( IDTST .NE. IDSTR ) THEN
 2348                   WRITE (NDSE,902) IDTST, IDSTR
 2349                   CALL EXTCDE ( 20 )
 2350                 END IF
 2351               IF ( VERTST .NE. VEROGR ) THEN
 2352                   WRITE (NDSE,903) VERTST, VEROGR
 2353                   CALL EXTCDE ( 21 )
 2354                 END IF
 2355               IF ( NOGRP .NE. MOGRP .OR. NGRPP .NE. MGRPP ) THEN
 2356                   WRITE (NDSE,904) MOGRP, MGRPP, NOGRP, NGRPP
 2357                   CALL EXTCDE ( 22 )
 2358                 END IF
 2359               IF ( TNAME .NE. GNAME ) THEN
 2360                   WRITE (NDSE,905) TNAME, GNAME
 2361                 END IF
 2362               IF ( NOSWLL .NE. MOSWLL ) THEN
 2363                   WRITE (NDSE,906) MOSWLL, NOSWLL
 2364                   CALL EXTCDE ( 24 )
 2365                 END IF
 
 
                                                         w3iogomd.F90  page  44
 
 
 2366 !
 2367             END IF
 2368 !
 2369         END IF
 2370 !
 2371 ! TIME and flags ----------------------------------------------------- *
 2372 !
 2373       IF ( WRITE ) THEN
 2374           WRITE (NDSOG)                            TIME, FLOGRD
 2375         ELSE
 2376           READ (NDSOG,END=803,ERR=802,IOSTAT=IERR) TIME, FLOGRD
 2377         END IF
 2378 !
 2379 ! MAPSTA ------------------------------------------------------------- *
 2380 !
 2381       ALLOCATE ( MAPTMP(NY,NX) )
 2382       IF ( WRITE ) THEN
 2383           MAPTMP = MAPSTA + 8*MAPST2
 2384           WRITE (NDSOG)                                               &
 2385                ((MAPTMP(IY,IX),IX=1,NX),IY=1,NY)
 2386         ELSE
 2387           READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)                    &
 2388                ((MAPTMP(IY,IX),IX=1,NX),IY=1,NY)
 2389           MAPSTA = MOD(MAPTMP+2,8) - 2
 2390           MAPST2 = (MAPTMP-MAPSTA) / 8
 2391         END IF
 2392       DEALLOCATE ( MAPTMP )
 2393 !
 2394 ! Fields ---------------------------------------------- *
 2395 !
 2396 ! Initialization ---------------------------------------------- *
 2397 !
 2398       IF ( WRITE ) THEN
 2399           DO ISEA=1, NSEA
 2400             IF ( MAPSTA(MAPSF(ISEA,2),MAPSF(ISEA,1)) .LT. 0 ) THEN
 2401 !
 2402                 IF ( FLOGRD( 2, 2) ) WLM   (ISEA) = UNDEF
 2403                 IF ( FLOGRD( 2, 3) ) T02   (ISEA) = UNDEF
 2404                 IF ( FLOGRD( 2, 4) ) T0M1  (ISEA) = UNDEF
 2405                 IF ( FLOGRD( 2, 5) ) T01   (ISEA) = UNDEF
 2406                 IF ( FLOGRD( 2, 6) .OR. FLOGRD( 2,18) )                 &
 2407                                      FP0   (ISEA) = UNDEF  ! FP or TP
 2408                 IF ( FLOGRD( 2, 7) ) THM   (ISEA) = UNDEF
 2409                 IF ( FLOGRD( 2, 8) ) THS   (ISEA) = UNDEF
 2410                 IF ( FLOGRD( 2, 9) ) THP0  (ISEA) = UNDEF
 2411                                      UST   (ISEA) = UNDEF
 2412                                      USTDIR(ISEA) = UNDEF
 2413                 IF ( FLOGRD( 2,10) ) HSIG  (ISEA) = UNDEF
 2414                 IF ( FLOGRD( 2,11) ) STMAXE(ISEA) = UNDEF
 2415                 IF ( FLOGRD( 2,12) ) STMAXD(ISEA) = UNDEF
 2416                 IF ( FLOGRD( 2,13) ) HMAXE (ISEA) = UNDEF
 2417                 IF ( FLOGRD( 2,14) ) HCMAXE(ISEA) = UNDEF
 2418                 IF ( FLOGRD( 2,15) ) HMAXD (ISEA) = UNDEF
 2419                 IF ( FLOGRD( 2,16) ) HCMAXD(ISEA) = UNDEF
 2420                 IF ( FLOGRD( 2,17) ) WBT   (ISEA) = UNDEF
 
 
                                                         w3iogomd.F90  page  45
 
 
 2421 !
 2422                 IF ( FLOGRD( 3, 1) ) EF   (ISEA,:) = UNDEF
 2423                 IF ( FLOGRD( 3, 2) ) TH1M (ISEA,:) = UNDEF
 2424                 IF ( FLOGRD( 3, 3) ) STH1M(ISEA,:) = UNDEF
 2425                 IF ( FLOGRD( 3, 4) ) TH2M (ISEA,:) = UNDEF
 2426                 IF ( FLOGRD( 3, 5) ) STH2M(ISEA,:) = UNDEF
 2427 !
 2428                 IF ( FLOGRD( 4, 1) ) PHS (ISEA,:) = UNDEF
 2429                 IF ( FLOGRD( 4, 2) ) PTP (ISEA,:) = UNDEF
 2430                 IF ( FLOGRD( 4, 3) ) PLP (ISEA,:) = UNDEF
 2431                 IF ( FLOGRD( 4, 4) ) PDIR (ISEA,:) = UNDEF
 2432                 IF ( FLOGRD( 4, 5) ) PSI (ISEA,:) = UNDEF
 2433                 IF ( FLOGRD( 4, 6) ) PWS (ISEA,:) = UNDEF
 2434                 IF ( FLOGRD( 4, 7) ) PTHP0(ISEA,:) = UNDEF
 2435                 IF ( FLOGRD( 4, 8) ) PQP (ISEA,:) = UNDEF
 2436                 IF ( FLOGRD( 4, 9) ) PPE(ISEA,:)  = UNDEF
 2437                 IF ( FLOGRD( 4,10) ) PGW(ISEA,:)  = UNDEF
 2438                 IF ( FLOGRD( 4,11) ) PSW (ISEA,:) = UNDEF
 2439                 IF ( FLOGRD( 4,12) ) PTM1(ISEA,:) = UNDEF
 2440                 IF ( FLOGRD( 4,13) ) PT1 (ISEA,:) = UNDEF
 2441                 IF ( FLOGRD( 4,14) ) PT2 (ISEA,:) = UNDEF
 2442                 IF ( FLOGRD( 4,15) ) PEP (ISEA,:) = UNDEF
 2443                 IF ( FLOGRD( 4,16) ) PWST(ISEA  ) = UNDEF
 2444                 IF ( FLOGRD( 4,17) ) PNR (ISEA  ) = UNDEF
 2445 !
 2446                 IF ( FLOGRD( 5, 2) ) CHARN (ISEA) = UNDEF
 2447                 IF ( FLOGRD( 5, 3) ) CGE   (ISEA) = UNDEF
 2448                 IF ( FLOGRD( 5, 4) ) PHIAW (ISEA) = UNDEF
 2449                 IF ( FLOGRD( 5, 5) ) THEN
 2450                                      TAUWIX(ISEA) = UNDEF
 2451                                      TAUWIY(ISEA) = UNDEF
 2452                   END IF
 2453                 IF ( FLOGRD( 5, 6) ) THEN
 2454                                      TAUWNX(ISEA) = UNDEF
 2455                                      TAUWNY(ISEA) = UNDEF
 2456                   END IF
 2457                 IF ( FLOGRD( 5, 7) ) WHITECAP(ISEA,1) = UNDEF
 2458                 IF ( FLOGRD( 5, 8) ) WHITECAP(ISEA,2) = UNDEF
 2459                 IF ( FLOGRD( 5, 9) ) WHITECAP(ISEA,3) = UNDEF
 2460                 IF ( FLOGRD( 5,10) ) WHITECAP(ISEA,4) = UNDEF
 2461 !
 2462                 IF ( FLOGRD( 6, 1) ) THEN
 2463                                      SXX   (ISEA) = UNDEF
 2464                                      SYY   (ISEA) = UNDEF
 2465                                      SXY   (ISEA) = UNDEF
 2466                   END IF
 2467                 IF ( FLOGRD( 6, 2) ) THEN
 2468                                      TAUOX (ISEA) = UNDEF
 2469                                      TAUOY (ISEA) = UNDEF
 2470                   END IF
 2471                 IF ( FLOGRD( 6, 3) ) BHD(ISEA) = UNDEF
 2472                 IF ( FLOGRD( 6, 4) ) PHIOC (ISEA) = UNDEF
 2473                 IF ( FLOGRD( 6, 5) ) THEN
 2474                                      TUSX  (ISEA) = UNDEF
 2475                                      TUSY  (ISEA) = UNDEF
 
 
                                                         w3iogomd.F90  page  46
 
 
 2476                   END IF
 2477                 IF ( FLOGRD( 6, 6) ) THEN
 2478                                      USSX  (ISEA) = UNDEF
 2479                                      USSY  (ISEA) = UNDEF
 2480                   END IF
 2481                 IF ( FLOGRD( 6, 7) ) THEN
 2482                                      PRMS  (ISEA) = UNDEF
 2483                                      TPMS  (ISEA) = UNDEF
 2484                   END IF
 2485                 IF ( FLOGRD( 6, 8) ) US3D(ISEA,:) = UNDEF
 2486                 IF ( FLOGRD( 6, 9) ) P2SMS(ISEA,:) = UNDEF
 2487                 IF ( FLOGRD( 6, 10) ) TAUICE(ISEA,:) = UNDEF
 2488                 IF ( FLOGRD( 6, 11) ) PHICE(ISEA) = UNDEF
 2489                 IF ( FLOGRD( 6, 12) ) USSP(ISEA,:) = UNDEF
 2490 !
 2491                 IF ( FLOGRD( 7, 1) ) THEN
 2492                                      ABA   (ISEA) = UNDEF
 2493                                      ABD   (ISEA) = UNDEF
 2494                   END IF
 2495                 IF ( FLOGRD( 7, 2) ) THEN
 2496                                      UBA   (ISEA) = UNDEF
 2497                                      UBD   (ISEA) = UNDEF
 2498                   END IF
 2499                 IF ( FLOGRD( 7, 3) ) BEDFORMS(ISEA,:) = UNDEF
 2500                 IF ( FLOGRD( 7, 4) ) PHIBBL(ISEA) = UNDEF
 2501                 IF ( FLOGRD( 7, 5) ) TAUBBL(ISEA,:) = UNDEF
 2502 !
 2503                 IF ( FLOGRD( 8, 1) ) THEN
 2504                                      MSSX  (ISEA) = UNDEF
 2505                                      MSSY  (ISEA) = UNDEF
 2506                   END IF
 2507                 IF ( FLOGRD( 8, 2) ) THEN
 2508                                      MSCX  (ISEA) = UNDEF
 2509                                      MSCY  (ISEA) = UNDEF
 2510                   END IF
 2511                 IF ( FLOGRD( 8, 3) ) MSSD (ISEA) = UNDEF
 2512                 IF ( FLOGRD( 8, 4) ) MSCD (ISEA) = UNDEF
 2513                 IF ( FLOGRD( 8, 5) ) QP   (ISEA) = UNDEF
 2514 !
 2515                 IF ( FLOGRD( 9, 1) ) DTDYN (ISEA) = UNDEF
 2516                 IF ( FLOGRD( 9, 2) ) FCUT  (ISEA) = UNDEF
 2517                 IF ( FLOGRD( 9, 3) ) CFLXYMAX(ISEA) = UNDEF
 2518                 IF ( FLOGRD( 9, 4) ) CFLTHMAX(ISEA) = UNDEF
 2519                 IF ( FLOGRD( 9, 5) ) CFLKMAX(ISEA) = UNDEF
 2520 !
 2521               END IF
 2522 !
 2523             IF ( MAPSTA(MAPSF(ISEA,2),MAPSF(ISEA,1)) .EQ. 2 ) THEN
 2524 !
 2525                 IF ( FLOGRD( 5, 4) ) PHIAW (ISEA) = UNDEF
 2526                 IF ( FLOGRD( 5, 5) ) THEN
 2527                                      TAUWIX(ISEA) = UNDEF
 2528                                      TAUWIY(ISEA) = UNDEF
 2529                   END IF
 2530                 IF ( FLOGRD( 5, 6) ) THEN
 
 
                                                         w3iogomd.F90  page  47
 
 
 2531                                      TAUWNX(ISEA) = UNDEF
 2532                                      TAUWNY(ISEA) = UNDEF
 2533                   END IF
 2534                 IF ( FLOGRD( 5, 7) ) WHITECAP(ISEA,1) = UNDEF
 2535                 IF ( FLOGRD( 5, 8) ) WHITECAP(ISEA,2) = UNDEF
 2536                 IF ( FLOGRD( 5, 9) ) WHITECAP(ISEA,3) = UNDEF
 2537                 IF ( FLOGRD( 5,10) ) WHITECAP(ISEA,4) = UNDEF
 2538 !
 2539                 IF ( FLOGRD( 6, 2) )THEN
 2540                                       TAUOX (ISEA) = UNDEF
 2541                                       TAUOY (ISEA) = UNDEF
 2542                   END IF
 2543                 IF ( FLOGRD( 6, 4) ) PHIOC (ISEA) = UNDEF
 2544 !
 2545                 IF ( FLOGRD( 7, 3) ) BEDFORMS(ISEA,:) = UNDEF
 2546                 IF ( FLOGRD( 7, 4) ) PHIBBL(ISEA) = UNDEF
 2547                 IF ( FLOGRD( 7, 5) ) TAUBBL(ISEA,:) = UNDEF
 2548 !
 2549               END IF
 2550 !
 2551             END DO
 2552 !
 2553         ELSE
 2554           IF (.NOT.DINIT) CALL W3DIMW ( IGRD, NDSE, NDST, .TRUE. )
 2555           IF (.NOT.AINIT) CALL W3DIMA ( IGRD, NDSE, NDST, .TRUE. )
 2556         END IF
 2557 !
 2558 ! Actual output  ---------------------------------------------- *
 2559       DO IFI=1, NOGRP
 2560        DO IFJ=1, NGRPP
 2561                       
 2562         IF ( FLOGRD(IFI,IFJ) ) THEN
 2563 !
 2564             IF ( WRITE ) THEN
 2565 !
 2566 !     Section 1)
 2567 !
 2568                 IF ( IFI .EQ. 1 .AND. IFJ .EQ. 1 ) THEN
 2569                     WRITE ( NDSOG ) DW(1:NSEA)
 2570                   ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 2 ) THEN
 2571                     WRITE ( NDSOG ) CX(1:NSEA)
 2572                     WRITE ( NDSOG ) CY(1:NSEA)
 2573                   ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 3 ) THEN
 2574                     DO ISEA=1, NSEA
 2575                       IF (UA(ISEA) .NE.UNDEF) THEN
 2576                           AUX1(ISEA) = UA(ISEA)*COS(UD(ISEA))
 2577                           AUX2(ISEA) = UA(ISEA)*SIN(UD(ISEA))
 2578                        ELSE
 2579                         AUX1(ISEA) = UNDEF
 2580                         AUX2(ISEA) = UNDEF
 2581                        END IF
 2582                       END DO
 2583                     WRITE ( NDSOG ) AUX1
 2584                     WRITE ( NDSOG ) AUX2
 2585                   ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 4 ) THEN
 
 
                                                         w3iogomd.F90  page  48
 
 
 2586                     WRITE ( NDSOG ) AS(1:NSEA)
 2587                   ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 5 ) THEN
 2588                     WRITE ( NDSOG ) WLV(1:NSEA)
 2589                   ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 6 ) THEN
 2590                     WRITE ( NDSOG ) ICE(1:NSEA)
 2591                   ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 7 ) THEN
 2592                     WRITE ( NDSOG ) BERG(1:NSEA)
 2593                                                 
 2594 !
 2595 !     Section 2)
 2596 !
 2597                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 1 ) THEN
 2598                     WRITE ( NDSOG ) HS(1:NSEA)
 2599                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 2 ) THEN
 2600                     WRITE ( NDSOG ) WLM(1:NSEA)
 2601                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 3 ) THEN
 2602                     WRITE ( NDSOG ) T02(1:NSEA)
 2603                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 4 ) THEN
 2604                     WRITE ( NDSOG ) T0M1(1:NSEA)
 2605                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 5 ) THEN
 2606                     WRITE ( NDSOG ) T01(1:NSEA)
 2607                   ELSE IF ( (IFI .EQ. 2 .AND. IFJ .EQ. 6) .OR.         &
 2608                             (IFI .EQ. 2 .AND. IFJ .EQ. 18) ) THEN
 2609                     ! Note: TP output is derived from FP field.
 2610                     WRITE ( NDSOG ) FP0(1:NSEA)
 2611                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 7 ) THEN
 2612                     WRITE ( NDSOG ) THM(1:NSEA)
 2613                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 8 ) THEN
 2614                     WRITE ( NDSOG ) THS(1:NSEA)
 2615                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 9 ) THEN
 2616                     WRITE ( NDSOG ) THP0(1:NSEA)
 2617                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 10 ) THEN
 2618                     WRITE ( NDSOG ) HSIG(1:NSEA)
 2619                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 11 ) THEN
 2620                     WRITE ( NDSOG ) STMAXE(1:NSEA)
 2621                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 12 ) THEN
 2622                     WRITE ( NDSOG ) STMAXD(1:NSEA)
 2623                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 13 ) THEN
 2624                     WRITE ( NDSOG ) HMAXE(1:NSEA)
 2625                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 14 ) THEN
 2626                     WRITE ( NDSOG ) HCMAXE(1:NSEA)
 2627                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 15 ) THEN
 2628                     WRITE ( NDSOG ) HMAXD(1:NSEA)
 2629                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 16 ) THEN
 2630                     WRITE ( NDSOG ) HCMAXD(1:NSEA)
 2631                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 17 ) THEN
 2632                     WRITE ( NDSOG ) WBT(1:NSEA)
 2633 !
 2634 !     Section 3)
 2635 !
 2636                   ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 1 ) THEN
 2637                     WRITE ( NDSOG ) EF(1:NSEA,E3DF(2,1):E3DF(3,1))
 2638                   ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 2 ) THEN
 2639                     WRITE ( NDSOG ) TH1M(1:NSEA,E3DF(2,2):E3DF(3,2))
 2640                   ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 3 ) THEN
 
 
                                                         w3iogomd.F90  page  49
 
 
 2641                     WRITE ( NDSOG ) STH1M(1:NSEA,E3DF(2,3):E3DF(3,3))
 2642                   ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 4 ) THEN
 2643                     WRITE ( NDSOG ) TH2M(1:NSEA,E3DF(2,4):E3DF(3,4))
 2644                   ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 5 ) THEN
 2645                     WRITE ( NDSOG ) STH2M(1:NSEA,E3DF(2,5):E3DF(3,5))
 2646                   ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 6) THEN
 2647                     WRITE ( NDSOG ) WN(1:NK,1:NSEA)
 2648 !
 2649 !     Section 4)
 2650 !
 2651                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 1 ) THEN
 2652                     WRITE ( NDSOG ) PHS(1:NSEA,0:NOSWLL)
 2653                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 2 ) THEN
 2654                     WRITE ( NDSOG ) PTP(1:NSEA,0:NOSWLL)
 2655                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 3 ) THEN
 2656                     WRITE ( NDSOG ) PLP(1:NSEA,0:NOSWLL)
 2657                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 4 ) THEN
 2658                     WRITE ( NDSOG ) PDIR(1:NSEA,0:NOSWLL)
 2659                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 5 ) THEN
 2660                     WRITE ( NDSOG ) PSI(1:NSEA,0:NOSWLL)
 2661                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 6 ) THEN
 2662                     WRITE ( NDSOG ) PWS(1:NSEA,0:NOSWLL)
 2663                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 7 ) THEN
 2664                     WRITE ( NDSOG ) PTHP0(1:NSEA,0:NOSWLL)
 2665                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 8  ) THEN
 2666                     WRITE ( NDSOG ) PQP(1:NSEA,0:NOSWLL)
 2667                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 9  ) THEN
 2668                     WRITE ( NDSOG ) PPE(1:NSEA,0:NOSWLL)
 2669                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 10 ) THEN
 2670                     WRITE ( NDSOG ) PGW(1:NSEA,0:NOSWLL)
 2671                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 11 ) THEN
 2672                     WRITE ( NDSOG ) PSW(1:NSEA,0:NOSWLL)
 2673                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 12 ) THEN
 2674                     WRITE ( NDSOG ) PTM1(1:NSEA,0:NOSWLL)
 2675                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 13 ) THEN
 2676                     WRITE ( NDSOG ) PT1(1:NSEA,0:NOSWLL)
 2677                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 14 ) THEN
 2678                     WRITE ( NDSOG ) PT2(1:NSEA,0:NOSWLL)
 2679                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 15 ) THEN
 2680                     WRITE ( NDSOG ) PEP(1:NSEA,0:NOSWLL)
 2681                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 16 ) THEN
 2682                     WRITE ( NDSOG ) PWST(1:NSEA)
 2683                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 17 ) THEN
 2684                     WRITE ( NDSOG ) PNR(1:NSEA)
 2685 !
 2686 !     Section 5)
 2687 !
 2688                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 1 ) THEN
 2689                     DO ISEA=1, NSEA
 2690                       IX     = MAPSF(ISEA,1)
 2691                       IY     = MAPSF(ISEA,2)
 2692                       IF ( MAPSTA(IY,IX) .EQ. 1 ) THEN
 2693                           AUX1(ISEA) = UST(ISEA) * ASF(ISEA) *        &
 2694                                                       COS(USTDIR(ISEA))
 2695                           AUX2(ISEA) = UST(ISEA) * ASF(ISEA) *        &
 
 
                                                         w3iogomd.F90  page  50
 
 
 2696                                                       SIN(USTDIR(ISEA))
 2697                         ELSE
 2698                           AUX1(ISEA) = UNDEF
 2699                           AUX2(ISEA) = UNDEF
 2700                         END IF
 2701                       END DO
 2702                     WRITE ( NDSOG ) AUX1
 2703                     WRITE ( NDSOG ) AUX2
 2704                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 2 ) THEN
 2705                     WRITE ( NDSOG ) CHARN(1:NSEA)
 2706                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 3 ) THEN
 2707                     WRITE ( NDSOG ) CGE(1:NSEA)
 2708                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 4 ) THEN
 2709                     WRITE ( NDSOG ) PHIAW(1:NSEA)
 2710                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 5 ) THEN
 2711                     WRITE ( NDSOG ) TAUWIX(1:NSEA)
 2712                     WRITE ( NDSOG ) TAUWIY(1:NSEA)
 2713                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 6 ) THEN
 2714                     WRITE ( NDSOG ) TAUWNX(1:NSEA)
 2715                     WRITE ( NDSOG ) TAUWNY(1:NSEA)
 2716                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 7 ) THEN
 2717                     WRITE ( NDSOG ) WHITECAP(1:NSEA,1)
 2718                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 8 ) THEN
 2719                     WRITE ( NDSOG ) WHITECAP(1:NSEA,2)
 2720                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 9 ) THEN
 2721                     WRITE ( NDSOG ) WHITECAP(1:NSEA,3)
 2722                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 10 ) THEN
 2723                     WRITE ( NDSOG ) WHITECAP(1:NSEA,4)
 2724                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 11 ) THEN
 2725                     WRITE ( NDSOG ) TWS(1:NSEA)
 2726 !
 2727 !     Section 6)
 2728 !
 2729                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 1 ) THEN
 2730                     WRITE ( NDSOG ) SXX(1:NSEA)
 2731                     WRITE ( NDSOG ) SYY(1:NSEA)
 2732                     WRITE ( NDSOG ) SXY(1:NSEA)
 2733                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 2 ) THEN
 2734                     WRITE ( NDSOG ) TAUOX(1:NSEA)
 2735                     WRITE ( NDSOG ) TAUOY(1:NSEA)
 2736                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 3  ) THEN
 2737                     WRITE ( NDSOG ) BHD(1:NSEA)
 2738                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 4 ) THEN
 2739                     WRITE ( NDSOG ) PHIOC(1:NSEA)
 2740                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 5 ) THEN
 2741                     WRITE ( NDSOG ) TUSX(1:NSEA)
 2742                     WRITE ( NDSOG ) TUSY(1:NSEA)
 2743                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 6 ) THEN
 2744                     WRITE ( NDSOG ) USSX(1:NSEA)
 2745                     WRITE ( NDSOG ) USSY(1:NSEA)
 2746                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 7 ) THEN
 2747                     WRITE ( NDSOG ) PRMS(1:NSEA)
 2748                     WRITE ( NDSOG ) TPMS(1:NSEA)
 2749                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 8 ) THEN
 2750                     WRITE ( NDSOG ) US3D(1:NSEA,   US3DF(2):US3DF(3))
 
 
                                                         w3iogomd.F90  page  51
 
 
 2751                     WRITE ( NDSOG ) US3D(1:NSEA,NK+US3DF(2):NK+US3DF(3))
 2752                  ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ.  9 ) THEN
 2753                     WRITE ( NDSOG ) P2SMS(1:NSEA,P2MSF(2):P2MSF(3))
 2754                  ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 10 ) THEN
 2755                     WRITE ( NDSOG ) TAUICE(1:NSEA,1)
 2756                     WRITE ( NDSOG ) TAUICE(1:NSEA,2)
 2757                  ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 11 ) THEN
 2758                     WRITE ( NDSOG ) PHICE(1:NSEA)
 2759                  ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 12 ) THEN
 2760                     WRITE ( NDSOG ) USSP(1:NSEA,   1:USSPF(2))
 2761                     WRITE ( NDSOG ) USSP(1:NSEA,NK+1:NK+USSPF(2))
 2762 !
 2763 !     Section 7)
 2764 !
 2765                   ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 1 ) THEN
 2766                     DO ISEA=1, NSEA
 2767                       IF ( ABA(ISEA) .NE. UNDEF ) THEN
 2768                           AUX1(ISEA) = ABA(ISEA)*COS(ABD(ISEA))
 2769                           AUX2(ISEA) = ABA(ISEA)*SIN(ABD(ISEA))
 2770                         ELSE
 2771                           AUX1(ISEA) = UNDEF
 2772                           AUX2(ISEA) = UNDEF
 2773                         END IF
 2774                       END DO
 2775                     WRITE ( NDSOG ) AUX1
 2776                     WRITE ( NDSOG ) AUX2
 2777                     !WRITE ( NDSOG ) ABA(1:NSEA)
 2778                     !WRITE ( NDSOG ) ABD(1:NSEA)
 2779                   ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 2 ) THEN
 2780                     DO ISEA=1, NSEA
 2781                       IF ( UBA(ISEA) .NE. UNDEF ) THEN
 2782                           AUX1(ISEA) = UBA(ISEA)*COS(UBD(ISEA))
 2783                           AUX2(ISEA) = UBA(ISEA)*SIN(UBD(ISEA))
 2784                         ELSE
 2785                           AUX1(ISEA) = UNDEF
 2786                           AUX2(ISEA) = UNDEF
 2787                         END IF
 2788                       END DO
 2789                     WRITE ( NDSOG ) AUX1
 2790                     WRITE ( NDSOG ) AUX2
 2791 !                    WRITE ( NDSOG ) UBA(1:NSEA)
 2792 !                    WRITE ( NDSOG ) UBD(1:NSEA)
 2793                   ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 3 ) THEN
 2794                     WRITE ( NDSOG ) BEDFORMS(1:NSEA,1)
 2795                     WRITE ( NDSOG ) BEDFORMS(1:NSEA,2)
 2796                     WRITE ( NDSOG ) BEDFORMS(1:NSEA,3)
 2797                   ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 4 ) THEN
 2798                     WRITE ( NDSOG ) PHIBBL(1:NSEA)
 2799                   ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 5 ) THEN
 2800                     WRITE ( NDSOG ) TAUBBL(1:NSEA,1)
 2801                     WRITE ( NDSOG ) TAUBBL(1:NSEA,2)
 2802 !
 2803 !     Section 8)
 2804 !
 2805                   ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 1 ) THEN
 
 
                                                         w3iogomd.F90  page  52
 
 
 2806                     WRITE ( NDSOG ) MSSX(1:NSEA)
 2807                     WRITE ( NDSOG ) MSSY(1:NSEA)
 2808                   ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 2 ) THEN
 2809                     WRITE ( NDSOG ) MSCX(1:NSEA)
 2810                     WRITE ( NDSOG ) MSCY(1:NSEA)
 2811                   ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 3 ) THEN
 2812                     WRITE ( NDSOG ) MSSD(1:NSEA)
 2813                   ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 4 ) THEN
 2814                     WRITE ( NDSOG ) MSCD(1:NSEA)
 2815                   ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 5 ) THEN
 2816                     WRITE ( NDSOG ) QP(1:NSEA)
 2817 !
 2818 !     Section 9)
 2819 !
 2820                   ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 1 ) THEN
 2821                     WRITE ( NDSOG ) DTDYN(1:NSEA)
 2822                   ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 2 ) THEN
 2823                     WRITE ( NDSOG ) FCUT(1:NSEA)
 2824                   ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 3 ) THEN
 2825                     WRITE ( NDSOG ) CFLXYMAX(1:NSEA)
 2826                   ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 4 ) THEN
 2827                     WRITE ( NDSOG ) CFLTHMAX(1:NSEA)
 2828                   ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 5 ) THEN
 2829                     WRITE ( NDSOG ) CFLKMAX(1:NSEA)
 2830 !
 2831 !     Section 10)
 2832 !
 2833                   ELSE IF ( IFI .EQ. 10 ) THEN
 2834                     WRITE ( NDSOG ) USERO(1:NSEA,IFJ)
 2835 !
 2836                   END IF
 2837 !
 2838               ELSE
 2839 !
 2840 !     Start of reading ......
 2841 !
 2842 !     Section 1)
 2843 !
 2844                 IF ( IFI .EQ. 1 .AND. IFJ .EQ. 1 ) THEN
 2845                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) DW(1:NSEA)
 2846                   ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 2 ) THEN
 2847                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) CX(1:NSEA)
 2848                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) CY(1:NSEA)
 2849                   ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 3 ) THEN
 2850                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) UA(1:NSEA)
 2851                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) UD(1:NSEA)
 2852                   ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 4 ) THEN
 2853                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) AS(1:NSEA)
 2854                   ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 5 ) THEN
 2855                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) WLV(1:NSEA)
 2856                   ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 6 ) THEN
 2857                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ICE(1:NSEA)
 2858                   ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 7 ) THEN
 2859                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) BERG(1:NSEA)
 2860 !
 
 
                                                         w3iogomd.F90  page  53
 
 
 2861 !     Section 2)
 2862 !
 2863                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 1 ) THEN
 2864                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) HS(1:NSEA)
 2865                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 2 ) THEN
 2866                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) WLM(1:NSEA)
 2867                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 3 ) THEN
 2868                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) T02(1:NSEA)
 2869                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 4 ) THEN
 2870                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) T0M1(1:NSEA)
 2871                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 5 ) THEN
 2872                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) T01(1:NSEA)
 2873                   ELSE IF ( (IFI .EQ. 2 .AND. IFJ .EQ. 6) .OR.       &
 2874                             (IFI .EQ. 2 .AND. IFJ .EQ. 18) ) THEN
 2875                     ! Note: TP output is derived from FP field.
 2876                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) FP0(1:NSEA)
 2877                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 7 ) THEN
 2878                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) THM(1:NSEA)
 2879                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 8 ) THEN
 2880                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) THS(1:NSEA)
 2881                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 9 ) THEN
 2882                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2883                                                          THP0(1:NSEA)
 2884                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 10 ) THEN
 2885                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2886                                                          HSIG(1:NSEA)
 2887                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 11 ) THEN
 2888                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2889                                                          STMAXE(1:NSEA)
 2890                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 12 ) THEN
 2891                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2892                                                          STMAXD(1:NSEA)
 2893                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 13 ) THEN
 2894                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2895                                                          HMAXE(1:NSEA)
 2896                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 14 ) THEN
 2897                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2898                                                          HCMAXE(1:NSEA)
 2899                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 15 ) THEN
 2900                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2901                                                          HMAXD(1:NSEA)
 2902                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 16 ) THEN
 2903                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2904                                                          HCMAXD(1:NSEA)
 2905                   ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 17 ) THEN
 2906                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) WBT(1:NSEA)
 2907 !
 2908 !     Section 3)
 2909 !
 2910                  ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 1 ) THEN
 2911                    READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2912                                       EF(1:NSEA,E3DF(2,1):E3DF(3,1))
 2913                   ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 2 ) THEN
 2914                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2915                                     TH1M(1:NSEA,E3DF(2,2):E3DF(3,2))
 
 
                                                         w3iogomd.F90  page  54
 
 
 2916                   ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 3 ) THEN
 2917                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2918                                     STH1M(1:NSEA,E3DF(2,3):E3DF(3,3))
 2919                   ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 4 ) THEN
 2920                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2921                                     TH2M(1:NSEA,E3DF(2,4):E3DF(3,4))
 2922                   ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 5 ) THEN
 2923                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2924                                     STH2M(1:NSEA,E3DF(2,5):E3DF(3,5))
 2925                   ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 6) THEN
 2926                      READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
 2927                                                       WN(1:NK,1:NSEA)
 2928 !
 2929 !     Section 4)
 2930 !
 2931                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 1 ) THEN
 2932                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2933                                                   PHS(1:NSEA,0:NOSWLL)
 2934                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 2 ) THEN
 2935                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2936                                                   PTP(1:NSEA,0:NOSWLL)
 2937                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 3 ) THEN
 2938                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2939                                                   PLP(1:NSEA,0:NOSWLL)
 2940                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 4 ) THEN
 2941                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2942                                                   PDIR(1:NSEA,0:NOSWLL)
 2943                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 5 ) THEN
 2944                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2945                                                   PSI(1:NSEA,0:NOSWLL)
 2946                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 6 ) THEN
 2947                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2948                                                   PWS(1:NSEA,0:NOSWLL)
 2949                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 7 ) THEN
 2950                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2951                                                 PTHP0(1:NSEA,0:NOSWLL)
 2952                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 8  ) THEN
 2953                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2954                                                   PQP(1:NSEA,0:NOSWLL)
 2955                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 9  ) THEN
 2956                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2957                                                   PPE(1:NSEA,0:NOSWLL)
 2958                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 10 ) THEN
 2959                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2960                                                   PGW(1:NSEA,0:NOSWLL)
 2961                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 11 ) THEN
 2962                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2963                                                   PSW(1:NSEA,0:NOSWLL)
 2964                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 12 ) THEN
 2965                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2966                                                  PTM1(1:NSEA,0:NOSWLL)
 2967                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 13 ) THEN
 2968                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2969                                                   PT1(1:NSEA,0:NOSWLL)
 2970                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 14 ) THEN
 
 
                                                         w3iogomd.F90  page  55
 
 
 2971                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2972                                                   PT2(1:NSEA,0:NOSWLL)
 2973                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 15 ) THEN
 2974                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2975                                                   PEP(1:NSEA,0:NOSWLL)
 2976                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 16) THEN
 2977                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2978                                                           PWST(1:NSEA)
 2979                   ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 17) THEN
 2980                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) PNR(1:NSEA)
 2981 !
 2982 !     Section 5)
 2983 !
 2984                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 1 ) THEN
 2985                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)          &
 2986                                                          UST(1:NSEA)
 2987                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)          &
 2988                                                          USTDIR(1:NSEA)
 2989                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 2 ) THEN
 2990                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2991                                                          CHARN(1:NSEA)
 2992                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 3 ) THEN
 2993                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) CGE(1:NSEA)
 2994                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 4 ) THEN
 2995                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2996                                                        PHIAW(1:NSEA)
 2997                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 5 ) THEN
 2998                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 2999                                                        TAUWIX(1:NSEA)
 3000                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3001                                                        TAUWIY(1:NSEA)
 3002                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 6 ) THEN
 3003                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3004                                                        TAUWNX(1:NSEA)
 3005                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3006                                                        TAUWNY(1:NSEA)
 3007                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 7 ) THEN
 3008                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3009                                                    WHITECAP(1:NSEA,1)
 3010                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 8 ) THEN
 3011                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3012                                                    WHITECAP(1:NSEA,2)
 3013                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 9 ) THEN
 3014                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3015                                                    WHITECAP(1:NSEA,3)
 3016                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 10 ) THEN
 3017                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3018                                                    WHITECAP(1:NSEA,4)
 3019                   ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 11 ) THEN
 3020                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3021                                                    TWS(1:NSEA)
 3022 !
 3023 !     Section 6)
 3024 !
 3025                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 1 ) THEN
 
 
                                                         w3iogomd.F90  page  56
 
 
 3026                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) SXX(1:NSEA)
 3027                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) SYY(1:NSEA)
 3028                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) SXY(1:NSEA)
 3029                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 2 ) THEN
 3030                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3031                                                        TAUOX(1:NSEA)
 3032                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3033                                                        TAUOY(1:NSEA)
 3034                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 3 ) THEN
 3035                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3036                                                        BHD(1:NSEA)
 3037                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 4 ) THEN
 3038                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3039                                                        PHIOC(1:NSEA)
 3040                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 5 ) THEN
 3041                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3042                                                        TUSX(1:NSEA)
 3043                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3044                                                        TUSY(1:NSEA)
 3045                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 6 ) THEN
 3046                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3047                                                        USSX(1:NSEA)
 3048                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3049                                                        USSY(1:NSEA)
 3050                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 7 ) THEN
 3051                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3052                                                        PRMS(1:NSEA)
 3053                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3054                                                        TPMS(1:NSEA)
 3055                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 8 ) THEN
 3056                      READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
 3057                                    US3D(1:NSEA,US3DF(2):US3DF(3))
 3058                      READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
 3059                                    US3D(1:NSEA,NK+US3DF(2):NK+US3DF(3))
 3060                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ.  9 ) THEN
 3061                      READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)     &
 3062                                      P2SMS(1:NSEA,P2MSF(2):P2MSF(3))
 3063                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 10 ) THEN
 3064                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3065                                                        TAUICE(1:NSEA,1)
 3066                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3067                                                        TAUICE(1:NSEA,2)
 3068                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 11 ) THEN
 3069                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3070                                                        PHICE(1:NSEA)
 3071                   ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 12 ) THEN
 3072                      READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
 3073                                    USSP(1:NSEA,1:USSPF(2))
 3074                      READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
 3075                                    USSP(1:NSEA,NK+1:NK+USSPF(2))
 3076                                                                 
 3077 !
 3078 !     Section 7)
 3079 !
 3080                   ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 1 ) THEN
 
 
                                                         w3iogomd.F90  page  57
 
 
 3081                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ABA(1:NSEA)
 3082                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ABD(1:NSEA)
 3083                   ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 2 ) THEN
 3084                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) UBA(1:NSEA)
 3085                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) UBD(1:NSEA)
 3086                   ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 3 ) THEN
 3087                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3088                                                     BEDFORMS(1:NSEA,1)
 3089                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3090                                                     BEDFORMS(1:NSEA,2)
 3091                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3092                                                     BEDFORMS(1:NSEA,3)
 3093                   ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 4 ) THEN
 3094                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3095                                                        PHIBBL(1:NSEA)
 3096                   ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 5 ) THEN
 3097                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3098                                                        TAUBBL(1:NSEA,1)
 3099                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3100                                                        TAUBBL(1:NSEA,2)
 3101 !
 3102 !     Section 8)
 3103 !
 3104                   ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 1 ) THEN
 3105                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3106                                                        MSSX(1:NSEA)
 3107                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3108                                                        MSSY(1:NSEA)
 3109                   ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 2 ) THEN
 3110                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3111                                                        MSCX(1:NSEA)
 3112                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3113                                                        MSCY(1:NSEA)
 3114                   ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 3 ) THEN
 3115                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3116                                                        MSSD(1:NSEA)
 3117                   ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 4 ) THEN
 3118                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3119                                                        MSCD(1:NSEA)
 3120                   ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 5 ) THEN
 3121                      READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) QP(1:NSEA)
 3122 !
 3123 !     Section 9)
 3124 !
 3125                   ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 1 ) THEN
 3126                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3127                                                          DTDYN(1:NSEA)
 3128                   ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 2 ) THEN
 3129                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3130                                                          FCUT(1:NSEA)
 3131                   ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 3 ) THEN
 3132                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3133                                                         CFLXYMAX(1:NSEA)
 3134                   ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 4 ) THEN
 3135                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 
 
                                                         w3iogomd.F90  page  58
 
 
 3136                                                         CFLTHMAX(1:NSEA)
 3137                   ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 5 ) THEN
 3138                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3139                                                          CFLKMAX(1:NSEA)
 3140 !
 3141 !     Section 10)
 3142 !
 3143                   ELSE IF ( IFI .EQ. 10 ) THEN
 3144                     READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
 3145                                                       USERO(1:NSEA,IFJ)
 3146                   END IF
 3147 !
 3148 ! End of test on WRITE/READ:
 3149 !
 3150               END IF
 3151 !
 3152 ! End of test on  FLOGRD(IFI,IFJ):
 3153 !
 3154           END IF
 3155 !
 3156 ! End of IFI and IFJ loops
 3157 !
 3158          END DO
 3159         END DO
 3160 !
 3161 ! Flush the buffers for write
 3162 !
 3163       IF ( WRITE ) CALL FLUSH ( NDSOG )
 3164 !
 3165       IF(OFILES(1) .EQ. 1) CLOSE(NDSOG)
 3166 !
 3167       RETURN
 3168 !
 3169 ! Escape locations read errors
 3170 !
 3171   800 CONTINUE
 3172       WRITE (NDSE,1000) IERR
 3173       CALL EXTCDE ( 41 )
 3174 !
 3175   801 CONTINUE
 3176       WRITE (NDSE,1001)
 3177       CALL EXTCDE ( 42 )
 3178 !
 3179   802 CONTINUE
 3180       WRITE (NDSE,1002) IERR
 3181       CALL EXTCDE ( 43 )
 3182 !
 3183   803 CONTINUE
 3184       IOTST  = -1
 3185       RETURN
 3186 !
 3187 ! Formats
 3188 !
 3189   900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
 3190                '     ILEGAL INXOUT VALUE: ',A/)
 
 
                                                         w3iogomd.F90  page  59
 
 
 3191   901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
 3192                '     MIXED READ/WRITE, LAST REQUEST: ',A/)
 3193   902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
 3194                '     ILEGAL IDSTR, READ : ',A/                        &
 3195                '                  CHECK : ',A/)
 3196   903 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
 3197                '     ILEGAL VEROGR, READ : ',A/                       &
 3198                '                   CHECK : ',A/)
 3199   904 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
 3200                '     DIFFERENT NUMBER OF FIELDS, FILE :',I8,I8/       &
 3201                '                              PROGRAM :',I8,I8/)
 3202   905 FORMAT (/' *** WAVEWATCH III WARNING IN W3IOGO :'/              &
 3203                '     ILEGAL GNAME, READ : ',A/                        &
 3204                '                  CHECK : ',A/)
 3205   906 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
 3206                '     ILEGAL NOSWLL, READ : ',I4/                      &
 3207                '                   CHECK : ',I4/)
 3208 !
 3209 !  999 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
 3210 !               '     PLEASE UPDATE FIELDS !!! '/)
 3211 !
 3212  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO : '/               &
 3213                '     ERROR IN OPENING FILE'/                          &
 3214                '     IOSTAT =',I5/)
 3215  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO : '/               &
 3216                '     PREMATURE END OF FILE'/)
 3217  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO : '/               &
 3218                '     ERROR IN READING FROM FILE'/                     &
 3219                '     IOSTAT =',I5/)
 3220 !
 3221 !/
 3222 !/ End of W3IOGO ----------------------------------------------------- /
 3223 !/
 3224       END SUBROUTINE W3IOGO
 3225 !/
 3226 !/ ------------------------------------------------------------------- /
 3227       SUBROUTINE CALC_U3STOKES ( A , USS_SWITCH )
 3228 !/
 3229 !/                  +-----------------------------------+
 3230 !/                  | WAVEWATCH III           NOAA/NCEP |
 3231 !/                  |           H. L. Tolman            |
 3232 !/                  |                        FORTRAN 90 |
 3233 !/                  | Last update :         10-Jan-2017 |
 3234 !/                  +-----------------------------------+
 3235 !/
 3236 !/    10-Jan-2017 : Separate Stokes drift calculation  ( version 6.01 )
 3237 !/
 3238 !  1. Purpose :
 3239 !
 3240 !     This code is built for the purpose of outputting Stokes drift
 3241 !       related parameters that can be utilized to obtain full
 3242 !       Stokes drift profiles external to the wave model.
 3243 !
 3244 !     Option 1: USS_SWITCH == 1
 3245 !               This method is for outputing the Stokes drift frequency
 
 
                                                         w3iogomd.F90  page  60
 
 
 3246 !               spectrum for spectral frequency bands as defined by the
 3247 !               WW3 computation spectral frequency grid.
 3248 !               Output Quantity: Stokes drift frequency spectrum [m/s/Hz]
 3249 !                                X and Y componenets.
 3250 !
 3251 !     Option 2: USS_SWITCH == 2
 3252 !               This method is for outputing the surface Stokes drift
 3253 !               for a specified frequency partition/band of the
 3254 !               wave spectrum.  These partitions do not need to be
 3255 !               matched to WW3's computation spectral frequency grid,
 3256 !               and will rather sum the contributions of the WW3 bands
 3257 !               into the output partition.  The partitions are defined
 3258 !               in the ww3_grid.inp namelist section.
 3259 !               Output Quantity: Stokes drift surface velocity [m/s]
 3260 !                                X and Y components
 3261 !                                For each partition (up to 25).
 3262 !
 3263 !  3. Parameters :
 3264 !
 3265 !     Parameter list
 3266 !     ----------------------------------------------------------------
 3267 !       A       R.A.   I   Input spectra. Left in par list to change
 3268 !                          shape.
 3269 !      USS_SWITCH  I   I   Switch if computing US3D (spectral) or USSP
 3270 !                           (partitions)
 3271 !     ----------------------------------------------------------------
 3272 !
 3273 !  4. Subroutines used :
 3274 !
 3275 !     See module documentation.
 3276 !
 3277 !  5. Called by :
 3278 !
 3279 !      Name      Type  Module   Description
 3280 !     ----------------------------------------------------------------
 3281 !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
 3282 !     ----------------------------------------------------------------
 3283 !
 3284 !  6. Error messages :
 3285 !
 3286 !     None.
 3287 !
 3288 !  8. Structure :
 3289 !
 3290 !     See source code.
 3291 !
 3292 !  9. Switches :
 3293 !
 3294 !     !/SHRD  Switch for shared / distributed memory architecture.
 3295 !     !/DIST  Id.
 3296 !
 3297 !     !/C90   Cray FORTRAN 90 compiler directives.
 3298 !     !/NEC   NEC SXF90 compiler directives.
 3299 !     !/OMPG  OpenMP compiler directive for loop splitting.
 3300 !
 
 
                                                         w3iogomd.F90  page  61
 
 
 3301 !     !/S     Enable subroutine tracing.
 3302 !     !/T     Test output.
 3303 !
 3304 ! 10. Source code :
 3305 !
 3306 !/ ------------------------------------------------------------------- /
 3307       USE CONSTANTS, ONLY: TPIINV, GRAV, TPI
 3308       USE W3GDATMD,  ONLY: DDEN, DSII, XFR, SIG, NK, NTH, NSEAl,    &
 3309                            ECOS, ESIN, US3DF, USSPF, USSP_WN
 3310       USE W3ADATMD,  ONLY: CG, WN, DW
 3311       USE W3ADATMD,  ONLY: USSX, USSY,  US3D, USSP
 3312       USE W3ODATMD, ONLY: IAPROC, NAPROC
 3313 !
 3314       IMPLICIT NONE
 3315 !/
 3316 !/ ------------------------------------------------------------------- /
 3317 !/ Parameter list
 3318 !/
 3319       REAL, INTENT(IN)        :: A(NTH,NK,0:NSEAL)
 3320       INTEGER, INTENT(IN)     :: USS_SWITCH
 3321 !/
 3322 !/ ------------------------------------------------------------------- /
 3323 !/ Local parameters
 3324 !/
 3325       INTEGER                 :: IK, ITH, ISEA, JSEA
 3326       INTEGER                 :: IKST, IKFI, IB
 3327       REAL                    :: FACTOR, FKD,KD
 3328       REAL                    :: ABX(NSEAL), ABY(NSEAL), USSCO
 3329       REAL                    :: MINDIFF
 3330       INTEGER                 :: Spc2Bnd(NK)
 3331 !/
 3332 !/ ------------------------------------------------------------------- /
 3333 !/
 3334 !
 3335 ! 1.  Initialize storage arrays -------------------------------------- *
 3336 !
 3337 ! 2.  Integral over discrete part of spectrum ------------------------ *
 3338 !
 3339       !Two options ----------------------------------------------------|
 3340       ! USS_SWITCH == 1 -> Old option, Stokes drift integrated in same |
 3341       !                    wavenumber bands as model integrates.       |
 3342       ! USS_SWITCH == 2 -> New option, Stokes drift integrated in a    |
 3343       !                    defined number (NP) of user specified       |
 3344       !                    partitions, where NP and the frequency      |
 3345       !                    ranges for each partition can be user       |
 3346       !                    defined at run-time.                        |
 3347       !----------------------------------------------------------------|
 3348                                                                         
 3349       if (USS_SWITCH==1) then
 3350          IKST=US3DF(2)!Start at US3DF(2)
 3351          IKFI=US3DF(3)!End at US3DF(3)
 3352       ELSEif (USS_SWITCH==2) then
 3353          IKST=1  ! Start at 1
 3354          IKFI=NK ! End at NK
 3355       ENDIF
 
 
                                                         w3iogomd.F90  page  62
 
 
 3356            
 3357 ! Initialize US3D/USSP
 3358       IF (USS_SWITCH.eq.1) then
 3359          US3D(:,:)=0.0
 3360       ELSEIF (USS_SWITCH.eq.2) then
 3361          USSP(:,:)=0.0
 3362        ENDIF
 3363       DO IK=IKST,IKFI   !1, NK
 3364 !
 3365 ! 2.a Initialize energy in band
 3366 !
 3367          ABX    = 0.
 3368          ABY    = 0.
 3369          !
 3370 ! 2.b Integrate energy in band
 3371 !
 3372          DO ITH=1, NTH
 3373 !
 3374             DO JSEA=1, NSEAL
 3375                 ISEA         = JSEA
 3376                ABX(JSEA)  = ABX(JSEA) + A(ITH,IK,JSEA)*ECOS(ITH)
 3377                ABY(JSEA)  = ABY(JSEA) + A(ITH,IK,JSEA)*ESIN(ITH)
 3378             END DO
 3379 !
 3380          END DO
 3381 !
 3382 ! 2.c Finalize integration over band and update mean arrays
 3383 !
 3384          DO JSEA=1, NSEAL
 3385               ISEA         = JSEA
 3386             FACTOR       = DDEN(IK) / CG(IK,ISEA)
 3387 !
 3388 ! Deep water limits
 3389 !
 3390             KD    = MAX ( 0.001 , WN(IK,ISEA) * DW(ISEA) )
 3391             IF ( KD .LT. 6. ) THEN
 3392                FKD       = FACTOR / SINH(KD)**2
 3393                USSCO=FKD*SIG(IK)*WN(IK,ISEA)*COSH(2.*KD)
 3394             ELSE
 3395                USSCO=FACTOR*SIG(IK)*2.*WN(IK,ISEA)
 3396             END IF
 3397 !
 3398           !USSX(JSEA)  = USSX(JSEA) + ABX(JSEA)*USSCO
 3399           !USSY(JSEA)  = USSY(JSEA) + ABY(JSEA)*USSCO
 3400 !
 3401 ! Fills the 3D Stokes drift spectrum array or surface Stokes partitions
 3402 !
 3403             IF (USS_SWITCH==1) THEN
 3404                !Old method fills into WW3 bands
 3405                IF (IK.GE.US3DF(2).and.IK.LE.US3DF(3)) then
 3406                   US3D(JSEA,IK)    =  ABX(JSEA)*USSCO/(DSII(IK)*TPIINV)
 3407                   US3D(JSEA,NK+IK) =  ABY(JSEA)*USSCO/(DSII(IK)*TPIINV)
 3408                ENDIF
 3409             ELSEIF (USS_SWITCH==2) THEN
 3410                ! Match each spectral component to the nearest partition
 
 
                                                         w3iogomd.F90  page  63
 
 
 3411                MINDIFF=1.e8
 3412                Spc2BND(IK) = 1
 3413                MINDIFF=abs(USSP_WN(1)-WN(IK,ISEA))
 3414                DO IB=2,USSPF(2)
 3415                   IF (MinDiff .gt. abs(USSP_WN(IB)-WN(IK,ISEA))) then
 3416                      Spc2BND(IK) = IB
 3417                      MinDiff = abs(USSP_WN(IB)-WN(IK,ISEA))
 3418                   ENDIF
 3419                ENDDO
 3420                !Put spectral energey into whichever band central wavenumb>
 3421                USSP(JSEA,Spc2Bnd(IK))    =  USSP(JSEA,Spc2Bnd(IK)) + ABX(>
 3422                USSP(JSEA,NK+Spc2BND(IK)) =  USSP(JSEA,NK+Spc2Bnd(IK)) + A>
 3423             ENDIF
 3424          END DO
 3425       END DO
 3426 !
 3427       RETURN
 3428 !
 3429 !/ End of CALC_U3STOKES
 3430 !----------------------------------------------------- /
 3431 !/
 3432       END SUBROUTINE CALC_U3STOKES
 3433 !/
 3434 !/ ------------------------------------------------------------------- /
 3435       SUBROUTINE CALC_WBT (A)
 3436 !/
 3437 !/                  +-----------------------------------+
 3438 !/                  | WAVEWATCH III           NOAA/NCEP |
 3439 !/                  |           Q. Liu                  |
 3440 !/                  |                        FORTRAN 90 |
 3441 !/                  | Last update :         24-Aug-2018 |
 3442 !/                  +-----------------------------------+
 3443 !/
 3444 !/    24-Aug-2018 : Origination.                       ( version 6.06 )
 3445 !/
 3446 !  1. Purpose :
 3447 !
 3448 !     Estimate the dominant wave breaking probability b_T based on
 3449 !     the empirical parameterization proposed by Babanin et al. (2001).
 3450 !     From their Fig. 12, we have
 3451 !
 3452 !         b_T = 85.1 * [(εp - 0.055) * (1 + H_s/d)]^2.33,
 3453 !
 3454 !     where ε is the significant steepness of the spectral peak, H_s is
 3455 !     the significant wave height, d is the water depth.
 3456 !
 3457 !     For more details, please see
 3458 !         Banner et al.  2000: JPO,      30,  3145 -  3160.
 3459 !         Babanin et al. 2001: JGR, 106(C6), 11569 - 11676.
 3460 !
 3461 !  2. Method :
 3462 !
 3463 !  3. Parameters :
 3464 !
 3465 !     Parameter list
 
 
                                                         w3iogomd.F90  page  64
 
 
 3466 !     ----------------------------------------------------------------
 3467 !       A       R.A.   I   Input wave action spectra N(j, θ, k)
 3468 !     ----------------------------------------------------------------
 3469 !
 3470 !  4. Subroutines used :
 3471 !
 3472 !  5. Called by :
 3473 !
 3474 !      Name      Type  Module   Description
 3475 !     ----------------------------------------------------------------
 3476 !      W3OUTG    Subr. Public   Calculate mean parameters.
 3477 !     ----------------------------------------------------------------
 3478 !
 3479 !  6. Error messages :
 3480 !
 3481 !     None.
 3482 !
 3483 !  8. Structure :
 3484 !
 3485 !     See source code.
 3486 !
 3487 !  9. Switches :
 3488 !
 3489 !     !/S     Enable subroutine tracing.
 3490 !     !/T     Test output.
 3491 !
 3492 ! 10. Source code :
 3493 !
 3494 !/ ------------------------------------------------------------------- /
 3495       USE W3DISPMD, ONLY: WAVNU1
 3496       USE W3ADATMD, ONLY: U10, U10D, WBT
 3497       USE W3ADATMD, ONLY: CG, WN, DW
 3498       USE W3GDATMD, ONLY: NK, NTH, NSEAL, SIG, ESIN, ECOS, DTH, DSII, &
 3499                           FTE, XFR, MAPSF, MAPSTA, DMIN
 3500       USE W3GDATMD, ONLY: BTBETA
 3501       USE W3PARALL, ONLY: INIT_GET_ISEA
 3502 !
 3503       IMPLICIT NONE
 3504 !
 3505 !/ ------------------------------------------------------------------- /
 3506 !/ Parameter list
 3507 !/
 3508       REAL, INTENT(IN)     :: A  (NTH, NK, 0:NSEAL)
 3509 !/
 3510 !/ ------------------------------------------------------------------- /
 3511 !/ Local parameters
 3512 !/
 3513 !
 3514       INTEGER              :: FPOPT = 0
 3515 !
 3516       INTEGER              :: IK, ITH, ISEA, JSEA, IKM, IKL, IKH, IX, IY
 3517       REAL                 :: TDPT, TU10, TUDIR, SINU, COSU, TC, TFORCE
 3518       REAL                 :: ESIG(NK) ! E(σ)
 3519       REAL                 :: FACTOR, ET, HS, ETP, HSP, SIGP, KP, &
 3520                               CGP, WSTP
 
 
                                                         w3iogomd.F90  page  65
 
 
 3521       REAL                 :: XL, XH, XL2, XH2, EL, EH, DENOM
 3522       REAL                 :: TWBT
 3523 !/
 3524 !/ ------------------------------------------------------------------- /
 3525 !/
 3526 !
 3527       DO JSEA = 1, NSEAL
 3528 ! JSEA 2 ISEA
 3529           CALL INIT_GET_ISEA(ISEA, JSEA)
 3530 !
 3531 ! check the status of this grid point [escape if this point is excluded]
 3532 !
 3533           IX = MAPSF(ISEA,1)
 3534           IY = MAPSF(ISEA,2)
 3535           IF ( MAPSTA(IY,IX) .LE. 0 ) CYCLE
 3536 !
 3537 ! Wind info. is required to select wind sea partition from the wave
 3538 ! spectrum. Two wind velocities are availabe:
 3539 ! - U10 & U10D   (w3adatmd)
 3540 ! - UST & USTDIR (w3wdatmd)
 3541 !     * U10D & USTDIR are not really the same when swell are present.
 3542 !
 3543 ! Following Janssen et al. (1989) and Bidlot (2001), spectral components
 3544 ! are considered to be subject to local wind forcing when
 3545 !
 3546 !          c / [U cos(θ - φ)] < β,
 3547 !
 3548 ! where c is the phase velocity c = σ/k, φ is the wind direction, U is
 3549 ! the wind speed U10, (sometimes approximated by U10≅ 28 * ust), β is
 3550 ! the constant forcing parameter with β∈ [1.0, 2.0]. By default, we use
 3551 ! β = 1.2(Bidlot 2001).
 3552 !
 3553           TDPT  = MAX(DW(ISEA), DMIN)          ! water depth d
 3554           TU10  = U10(ISEA)                    ! wind velocity U10
 3555           TUDIR = U10D(ISEA)                   ! wind direction φ (rad)
 3556           SINU  = SIN(TUDIR)                   ! sinφ
 3557           COSU  = COS(TUDIR)                   ! cosφ
 3558 !
 3559           ESIG  = 0.                           ! E(σ)
 3560           ET    = 0.                           ! ΣE(σ)δσ
 3561           ETP   = 0.                           ! ΣE(σ)δσ at peak only
 3562 !
 3563           DO IK = 1, NK
 3564               TC     = SIG(IK) / WN(IK, ISEA)  ! phase velocity c=σ/k
 3565               FACTOR = SIG(IK) / CG(IK, ISEA)  ! σ / cg
 3566               FACTOR = FACTOR * DTH            ! σ / cg * δθ
 3567 !
 3568               DO ITH = 1, NTH
 3569                   TFORCE = TC - TU10 * (COSU*ECOS(ITH)+SINU*ESIN(ITH)) &
 3570                                * BTBETA
 3571                                        
 3572                   IF (TFORCE .LT. 0.) THEN ! wind sea component
 3573                       ESIG(IK) = ESIG(IK) + A(ITH, IK, JSEA) * FACTOR
 3574                   ENDIF
 3575               ENDDO ! ITH
 
 
                                                         w3iogomd.F90  page  66
 
 
 3576 !
 3577           ENDDO ! IK
 3578 !
 3579 ! ESIG is E(σ) of the wind sea after filtration of any background swell.
 3580 ! Now we need to get Hs & σp for the wind sea spectrum.
 3581 ! FTE    = 0.25 * SIG(NK) * DTH * SIG(NK) [ww3_grid.ftn]
 3582 !
 3583           ET = SUM(ESIG * DSII)
 3584           ET = ET + ESIG(NK) * FTE / (DTH * SIG(NK))  ! FTE: add tail
 3585           HS = 4. * SQRT(MAX(0., ET))                 ! Hs
 3586 !
 3587 ! Get σp from E(σ)
 3588 !
 3589 ! Here we have tried three different ways to calculate FP:
 3590 !
 3591 ! FPOPT = 0: fp defined by Young (1999, p. 239)
 3592 ! FPOPT = 1: parabolic fit around the discrete peak frequency, as used
 3593 !            by ww3_outp
 3594 ! FPOPT = 2: discrete peak frequency
 3595 !
 3596 ! When the discrete peak frequency is used:
 3597 ! * For XFR = 1.1, the **discrete** peak region [0.7σp, 1.3σp] will be
 3598 !     {0.75, 0.83, 0.91, 1., 1.1, 1.21, 1.33}σp,
 3599 ! * and for XFR = 1.07, the **discrete** peak region becomes
 3600 !     {0.71, 0.76, 0.82, 0.87, 0.93, 1., 1.07, 1.14, 1.23, 1.31}σp.
 3601 !
 3602 ! Thus, a good approximation to the range [0.7σp, 1.3σp] is guranteed
 3603 ! by each XFR. I however found using the discrete peak frequency yielded
 3604 ! step-wise results. According to my test, the smoothest results were
 3605 ! obtained with FPOPT = 0. For simplicity, the δσ values (DSII) are
 3606 ! not modified.
 3607 !
 3608           IKM   = MAXLOC(ESIG, 1)                     ! index for σp
 3609 !
 3610           IF (FPOPT .EQ. 0) THEN
 3611 !             FP defined in Ian's book
 3612               SIGP = SUM(ESIG**4. * SIG(1:NK) * DSII) /  &
 3613                          MAX(1E-10, SUM(ESIG**4. * DSII))
 3614 !
 3615           ELSE IF (FPOPT .EQ. 1) THEN
 3616 !             Parabolic fit around the discrete peak (ww3_outp.ftn)
 3617               XL    = 1./XFR - 1.
 3618               XH    = XFR - 1.
 3619               XL2   = XL**2.
 3620               XH2   = XH**2.
 3621               IKL   = MAX (  1 , IKM-1 )
 3622               IKH   = MIN ( NK , IKM+1 )
 3623               EL    = ESIG(IKL) - ESIG(IKM)
 3624               EH    = ESIG(IKH) - ESIG(IKM)
 3625               DENOM = XL*EH - XH*EL
 3626               SIGP  = SIG(IKM) * (1. + 0.5 * ( XL2*EH - XH2*EL) &
 3627                           / SIGN (MAX(ABS(DENOM), 1.E-15), DENOM)) ! σp
 3628 !
 3629           ELSE IF (FPOPT .EQ. 2) THEN
 3630 !             Discrete peak (Give stepwise results, not used by default)
 
 
                                                         w3iogomd.F90  page  67
 
 
 3631               SIGP  = SIG(IKM)
 3632           ENDIF
 3633 !
 3634 ! kp from σp (linear dispersion)
 3635 !
 3636 ! N(k, θ) at first step is zero →  σp=0 → floating divided by zero >
 3637           IF (SIGP < 1E-6) SIGP = SIG(NK)     ! Hsp & b_T will be still 0.
 3638 !
 3639           CALL WAVNU1 (SIGP, TDPT, KP, CGP)
 3640 !
 3641 !                         { /1.3σp         }1/2
 3642 ! peak wave height Hp = 4 { |      E(σ) dσ }
 3643 !                         { /0.7σp         }
 3644 !
 3645           DO IK = 1, NK
 3646               IF ( (SIG(IK) >= 0.7 * SIGP) .AND. &
 3647                    (SIG(IK) <= 1.3 * SIGP) ) THEN
 3648                   ETP = ETP + ESIG(IK) * DSII(IK)
 3649               ENDIF
 3650           ENDDO ! IK
 3651           HSP = 4. * SQRT(MAX(0., ETP))
 3652 !
 3653 ! significant steepness of the peak region εp
 3654 !
 3655           WSTP = 0.5 * KP * HSP
 3656 !
 3657 ! Dominant wave breaking b_T
 3658 !
 3659           TWBT = 85.1 * (MAX(0.0, WSTP - 0.055) * (1 + HS/TDPT))**2.33
 3660           WBT(JSEA) = MIN(1.0, TWBT)
 3661 !
 3662       ENDDO ! JSEA
 3663 !/
 3664 !/ End of  CALC_WBT -------------------------------------------------- /
 3665 !/
 3666       END SUBROUTINE CALC_WBT
 3667 !/ ------------------------------------------------------------------- /
 3668       END MODULE W3IOGOMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
