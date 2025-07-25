                                                         wmunitmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE WMUNITMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         29-May-2009 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    29-Mar-2005 : Origination.                        ( version 3.07 )
   13 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   14 !/
   15 !/    Copyright 2009 National Weather Service (NWS),
   16 !/       National Oceanic and Atmospheric Administration.  All rights
   17 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   18 !/       No unauthorized use without permission.
   19 !/
   20 !  1. Purpose :
   21 !
   22 !     Dynamic assignement of unit numbers for the multi-grid wave
   23 !     model.
   24 !
   25 !     Allowed range of unit numbers is set in parameter statements.
   26 !
   27 !  2. Variables and types :
   28 !
   29 !      Name      Type  Scope    Description
   30 !     ----------------------------------------------------------------
   31 !      UNITLW    I.P.  Private  Lowest unit number.
   32 !      UNITHG    I.P.  Private  Highest unit number.
   33 !      INPLOW, INPHGH, OUTLOW, OUTHGH, SCRLOW, SCRHGH
   34 !                I.P.  Private  Low and high for input, output and
   35 !                               scratch files.
   36 !      FLINIT    Log.  Private  Flag for intialization.
   37 !
   38 !      U_USED    L.A.  Private  Flag for use/assignement.
   39 !      U_TYPE    C.A.  Private  Type of unit.
   40 !                                'RES' : Reserved.
   41 !                                'INP' : Input file.
   42 !                                'OUT' : Output file.
   43 !                                'SCR' : Scratch file.
   44 !      U_NAME    C.A.  Private  File name of unit.
   45 !      U_DESC    C.A.  Private  Decription of file.
   46 !     ----------------------------------------------------------------
   47 !
   48 !  3. Subroutines and functions :
   49 !
   50 !      Name      Type  Scope    Description
   51 !     ----------------------------------------------------------------
   52 !      WMUINI    Subr. Public   Initialize data structures.
   53 !      WMUDMP    Subr. Public   Dump contents of data structures.
   54 !      WMUSET    Subr. Public   Put data directly in structure.
   55 !      WMUGET    Subr. Public   Get a unit number.
 
 
                                                         wmunitmd.F90  page   2
 
 
   56 !      WMUINQ    Subr. Public   Update ansilary info automatically.
   57 !     ----------------------------------------------------------------
   58 !
   59 !  4. Subroutines and functions used :
   60 !
   61 !      Name      Type  Module   Description
   62 !     ----------------------------------------------------------------
   63 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   64 !      EXTCDE    Subr.   Id.    Program abort.
   65 !     ----------------------------------------------------------------
   66 !
   67 !  5. Remarks :
   68 !
   69 !     - All parameters are private. Dump data using WMUDMP routine.
   70 !
   71 !  6. Switches :
   72 !
   73 !     !/S    Enable subroutine tracing.
   74 !     !/T    Enable test output
   75 !
   76 !  7. Source code :
   77 !
   78 !/ ------------------------------------------------------------------- /
   79       PUBLIC
   80 !/
   81 !/ Define acceptable ranges of unit numbers
   82 !/
   83       INTEGER, PARAMETER, PRIVATE   :: UNITLW =  1, UNITHG = 120
   84       INTEGER, PARAMETER, PRIVATE   :: INPLOW = 10, INPHGH =  49
   85       INTEGER, PARAMETER, PRIVATE   :: OUTLOW = 50, OUTHGH =  98
   86       INTEGER, PARAMETER, PRIVATE   :: SCRLOW = 99, SCRHGH = 100
   87 !
   88       LOGICAL, PRIVATE              :: FLINIT = .FALSE.
   89       LOGICAL, PRIVATE, ALLOCATABLE :: U_USED(:)
   90       CHARACTER(LEN= 3), PRIVATE, ALLOCATABLE :: U_TYPE(:)
   91       CHARACTER(LEN=30), PRIVATE, ALLOCATABLE :: U_NAME(:)
   92       CHARACTER(LEN=30), PRIVATE, ALLOCATABLE :: U_DESC(:)
   93 !/
   94       CONTAINS
   95 !/ ------------------------------------------------------------------- /
   96       SUBROUTINE WMUINI ( NDSE, NDST )
   97 !/
   98 !/                  +-----------------------------------+
   99 !/                  | WAVEWATCH III           NOAA/NCEP |
  100 !/                  |           H. L. Tolman            |
  101 !/                  |                        FORTRAN 90 |
  102 !/                  | Last update :         25-Mar-2005 !
  103 !/                  +-----------------------------------+
  104 !/
  105 !/    25-Mar-2005 : Origination.                        ( version 3.07 )
  106 !/
  107 !  1. Purpose :
  108 !
  109 !     Allocate and initialize arrays of module.
  110 !
 
 
                                                         wmunitmd.F90  page   3
 
 
  111 !  2. Method :
  112 !
  113 !     Allocate and test parameter setting.
  114 !
  115 !  3. Parameters :
  116 !
  117 !     Parameter list
  118 !     ----------------------------------------------------------------
  119 !       NDSE    Int.   I   Unit number for error output.
  120 !       NDST    Int.   I   Unit number for test output.
  121 !     ----------------------------------------------------------------
  122 !
  123 !  4. Subroutines used :
  124 !
  125 !      Name      Type  Module   Description
  126 !     ----------------------------------------------------------------
  127 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  128 !      EXTCDE    Subr.   Id.    Program abort.
  129 !     ----------------------------------------------------------------
  130 !
  131 !  5. Called by :
  132 !
  133 !  6. Error messages :
  134 !
  135 !     See source code.
  136 !
  137 !  7. Remarks :
  138 !
  139 !  8. Structure :
  140 !
  141 !     See source code.
  142 !
  143 !  9. Switches :
  144 !
  145 !     !/S    Enable subroutine tracing.
  146 !     !/T    Enable test output
  147 !
  148 ! 10. Source code :
  149 !
  150 !/ ------------------------------------------------------------------- /
  151       USE W3SERVMD, ONLY: EXTCDE
  152 !
  153       IMPLICIT NONE
  154 !/
  155 !/ ------------------------------------------------------------------- /
  156 !/ Parameter list
  157 !/
  158       INTEGER, INTENT(IN)     :: NDSE, NDST
  159 !/
  160 !/ ------------------------------------------------------------------- /
  161 !/ Local parameters
  162 !/
  163       INTEGER                 :: J, I1, IN, I
  164       CHARACTER(LEN=3)        :: STRING
  165 !/
 
 
                                                         wmunitmd.F90  page   4
 
 
  166 !
  167 ! -------------------------------------------------------------------- /
  168 ! 1.  Test parameter settings
  169 !
  170       IF ( UNITLW .GE. UNITHG ) THEN
  171           WRITE (NDSE,1000) UNITLW, UNITHG
  172           CALL EXTCDE ( 1000 )
  173         END IF
  174 !
  175       IF ( UNITLW .GT. INPLOW .OR.                                    &
  176            UNITLW .GT. OUTLOW .OR.                                    &
  177            UNITLW .GT. SCRLOW ) THEN
  178           WRITE (NDSE,1001) UNITLW, INPLOW, OUTLOW, SCRLOW
  179           CALL EXTCDE ( 1001 )
  180         END IF
  181 !
  182       IF ( UNITHG .LT. INPHGH .OR.                                    &
  183            UNITHG .LT. OUTHGH .OR.                                    &
  184            UNITHG .LT. SCRHGH ) THEN
  185           WRITE (NDSE,1002) UNITHG, INPHGH, OUTHGH, SCRHGH
  186           CALL EXTCDE ( 1002 )
  187         END IF
  188 !
  189       IF ( FLINIT ) THEN
  190           WRITE (NDSE,1003)
  191           CALL EXTCDE ( 1003 )
  192         END IF
  193 !
  194 ! -------------------------------------------------------------------- /
  195 ! 1.  Allocate and initialize arrays
  196 !
  197       ALLOCATE ( U_USED(UNITLW:UNITHG) , U_TYPE(UNITLW:UNITHG) ,      &
  198                  U_NAME(UNITLW:UNITHG) , U_DESC(UNITLW:UNITHG) )
  199 !
  200       U_USED = .FALSE.
  201       U_TYPE = 'RES'
  202       U_NAME = 'unknown'
  203       U_DESC = 'unknown'
  204 !
  205 ! -------------------------------------------------------------------- /
  206 ! 2.  Designate file types
  207 !
  208       DO J=1, 3
  209 !
  210         SELECT CASE(J)
  211           CASE(1)
  212             STRING = 'INP'
  213             I1     = INPLOW
  214             IN     = INPHGH
  215           CASE(2)
  216             STRING = 'OUT'
  217             I1     = OUTLOW
  218             IN     = OUTHGH
  219           CASE DEFAULT
  220             STRING = 'SCR'
 
 
                                                         wmunitmd.F90  page   5
 
 
  221             I1     = SCRLOW
  222             IN     = SCRHGH
  223         END SELECT
  224 !
  225         DO I=I1, IN
  226           IF ( U_TYPE(I) .NE. 'RES' ) THEN
  227               WRITE (NDSE,1020) I, U_TYPE(I)
  228             END IF
  229           U_TYPE(I) = STRING
  230           END DO
  231         END DO
  232 !
  233 ! -------------------------------------------------------------------- /
  234 ! 3.  Set flags
  235 !
  236       FLINIT = .TRUE.
  237 !
  238 ! -------------------------------------------------------------------- /
  239 ! 4.  Test output
  240 !
  241       RETURN
  242 !
  243 ! Formats
  244 !
  245  1000 FORMAT (/' *** ERROR WMUINI: ILLEGAL UNIT RANGE ***'/           &
  246                '                   LOW - HIGH : ',2I10/)
  247  1001 FORMAT (/' *** ERROR WMUINI: ILLEGAL LOWER LIMITS ***'/         &
  248                '                   ',4I10/)
  249  1002 FORMAT (/' *** ERROR WMUINI: ILLEGAL HIGHER LIMITS ***'/        &
  250                '                   ',4I10/)
  251  1003 FORMAT (/' *** ERROR WMUINI: DATA ALREADY INITIALIZED ***'/)
  252  1020 FORMAT (/' *** WARNING WMUINI: UNIT',I4,' ALREADY ASSIGNED [',  &
  253                A,'] ***')
  254 !
  255 !/
  256 !/ End of WMUINI ----------------------------------------------------- /
  257 !/
  258       END SUBROUTINE WMUINI
  259 !/ ------------------------------------------------------------------- /
  260       SUBROUTINE WMUDMP ( NDS, IREQ )
  261 !/
  262 !/                  +-----------------------------------+
  263 !/                  | WAVEWATCH III           NOAA/NCEP |
  264 !/                  |           H. L. Tolman            |
  265 !/                  |                        FORTRAN 90 |
  266 !/                  | Last update :         25-Mar-2005 !
  267 !/                  +-----------------------------------+
  268 !/
  269 !/    25-Mar-2005 : Origination.                        ( version 3.07 )
  270 !/
  271 !  1. Purpose :
  272 !
  273 !     Display assigned unit number information from private data base.
  274 !
  275 !  2. Method :
 
 
                                                         wmunitmd.F90  page   6
 
 
  276 !
  277 !  3. Parameters :
  278 !
  279 !     Parameter list
  280 !     ----------------------------------------------------------------
  281 !       NDS     Int.   I   Unit number for output.
  282 !       IREQ    Int.   I   Request identifier.
  283 !                           < 0 : Dump all data.
  284 !                            0  : Dump assigned units only.
  285 !                           > 0 : Dump this unit only.
  286 !     ----------------------------------------------------------------
  287 !
  288 !  4. Subroutines used :
  289 !
  290 !      Name      Type  Module   Description
  291 !     ----------------------------------------------------------------
  292 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  293 !      EXTCDE    Subr.   Id.    Program abort.
  294 !     ----------------------------------------------------------------
  295 !
  296 !  5. Called by :
  297 !
  298 !  6. Error messages :
  299 !
  300 !  7. Remarks :
  301 !
  302 !  8. Structure :
  303 !
  304 !  9. Switches :
  305 !
  306 !     !/S    Enable subroutine tracing.
  307 !     !/T    Enable test output
  308 !
  309 ! 10. Source code :
  310 !
  311 !/ ------------------------------------------------------------------- /
  312       USE W3SERVMD, ONLY: EXTCDE
  313 !
  314       IMPLICIT NONE
  315 !/
  316 !/ ------------------------------------------------------------------- /
  317 !/ Parameter list
  318 !/
  319       INTEGER, INTENT(IN)     :: NDS, IREQ
  320 !/
  321 !/ ------------------------------------------------------------------- /
  322 !/ Local parameters
  323 !/
  324       INTEGER                 :: I
  325 !/
  326 !
  327 ! -------------------------------------------------------------------- /
  328 ! 1.  Test request and intialization
  329 !
  330       IF ( .NOT. FLINIT ) THEN
 
 
                                                         wmunitmd.F90  page   7
 
 
  331           WRITE (NDS,1000)
  332           CALL EXTCDE ( 1000 )
  333         END IF
  334 !
  335       IF ( IREQ.GT.0 .AND. ( IREQ.LT.UNITLW .OR. IREQ.GT.UNITHG) ) THEN
  336           WRITE (NDS,1001) IREQ, UNITLW, UNITHG
  337           CALL EXTCDE ( 1001 )
  338         END IF
  339 !
  340 ! -------------------------------------------------------------------- /
  341 ! 2.  Single unit request
  342 !
  343       IF ( IREQ .GT. 0 ) THEN
  344           WRITE (NDS,920) IREQ, U_USED(IREQ), U_TYPE(IREQ),           &
  345                                 U_NAME(IREQ), U_DESC(IREQ)
  346 !
  347 ! -------------------------------------------------------------------- /
  348 ! 3.  Multiple unit request
  349 !
  350         ELSE
  351 !
  352           IF ( IREQ .LT. 0 ) THEN
  353               WRITE (NDS,930)
  354             ELSE
  355               WRITE (NDS,931)
  356             END IF
  357 !
  358           DO I=UNITLW, UNITHG
  359             IF ( IREQ.LT.0 .OR. U_USED(I) )                           &
  360                WRITE (NDS,932) I, U_USED(I), U_TYPE(I),         &
  361                                   U_NAME(I), U_DESC(I)
  362             END DO
  363             WRITE (NDS,*)
  364 !
  365         END IF
  366 !
  367       RETURN
  368 !
  369 ! Formats
  370 !
  371   920 FORMAT (/' WMUDMP: Unit number : ',I6/                          &
  372                '         Assigned    : ',L6/                          &
  373                '         Type        : ',A/                           &
  374                '         Name        : ',A/                           &
  375                '         Description : ',A/)
  376 !
  377   930 FORMAT (/' WMUDMP: Unit information '//                         &
  378                '    Nr Flg Type  Name                  Description '/ &
  379                '  -------------------------------------------------', &
  380                '---------------------')
  381   931 FORMAT (/' WMUDMP: Unit information (assigned only)'//          &
  382                '    Nr Flg Type  Name                  Description '/ &
  383                '  -------------------------------------------------', &
  384                '---------------------')
  385   932 FORMAT ( 2X,I4,L4,2X,A3,2X,A20,2X,A)
 
 
                                                         wmunitmd.F90  page   8
 
 
  386 !
  387  1000 FORMAT (/' *** ERROR WMUDMP: DATA STRUCTURE READY ***'/         &
  388               /'                   RUN WMUINI FIRST '/)
  389  1001 FORMAT (/' *** ERROR WMUDMP: UNIT NUMBER OUT OF RANGE ***'      &
  390               /'                   REQ/RANG :',3I6/)
  391 !/
  392 !/ End of WMUDMP ----------------------------------------------------- /
  393 !/
  394       END SUBROUTINE WMUDMP
  395 !/ ------------------------------------------------------------------- /
  396       SUBROUTINE WMUSET ( NDSE, NDST, NDS, FLAG, TYPE, NAME, DESC )
  397 !/
  398 !/                  +-----------------------------------+
  399 !/                  | WAVEWATCH III           NOAA/NCEP |
  400 !/                  |           H. L. Tolman            |
  401 !/                  |                        FORTRAN 90 |
  402 !/                  | Last update :         25-Mar-2005 !
  403 !/                  +-----------------------------------+
  404 !/
  405 !/    25-Mar-2005 : Origination.                        ( version 3.07 )
  406 !/
  407 !  1. Purpose :
  408 !
  409 !     Directly set information for a unit number in the data structure.
  410 !
  411 !  2. Method :
  412 !
  413 !  3. Parameters :
  414 !
  415 !     Parameter list
  416 !     ----------------------------------------------------------------
  417 !       NDSE    Int.   I   Unit number for error output.
  418 !       NDST    Int.   I   Unit number for test output.
  419 !       NDS     Int.   I   Unit number to be assigned.
  420 !       FLAG    Log.   I   Flag for assigning unit.
  421 !       TYPE    C*3    I   Type identifier to be used.
  422 !       NAME    C*     I   Name of file.
  423 !       DESC    C*     I   Description of file.
  424 !     ----------------------------------------------------------------
  425 !
  426 !  4. Subroutines used :
  427 !
  428 !      Name      Type  Module   Description
  429 !     ----------------------------------------------------------------
  430 !      STRACE    Sur.  W3SERVMD Subroutine tracing.
  431 !      EXCTDE    Sur.    Id.    Program abort.
  432 !     ----------------------------------------------------------------
  433 !
  434 !  5. Called by :
  435 !
  436 !  6. Error messages :
  437 !
  438 !  7. Remarks :
  439 !
  440 !  8. Structure :
 
 
                                                         wmunitmd.F90  page   9
 
 
  441 !
  442 !  9. Switches :
  443 !
  444 !     !/S    Enable subroutine tracing.
  445 !     !/T    Enable test output
  446 !
  447 ! 10. Source code :
  448 !
  449 !/ ------------------------------------------------------------------- /
  450       USE W3SERVMD, ONLY: EXTCDE
  451 !
  452       IMPLICIT NONE
  453 !/
  454 !/ ------------------------------------------------------------------- /
  455 !/ Parameter list
  456 !/
  457       INTEGER, INTENT(IN)     :: NDSE, NDST, NDS
  458       LOGICAL, INTENT(IN)     :: FLAG
  459       CHARACTER(LEN=3), INTENT(IN), OPTIONAL ::                       &
  460                                  TYPE
  461       CHARACTER*(*), INTENT(IN), OPTIONAL ::                          &
  462                                  NAME, DESC
  463 !/
  464 !/ ------------------------------------------------------------------- /
  465 !/ Local parameters
  466 !/
  467 !/
  468 !
  469 ! -------------------------------------------------------------------- /
  470 ! 1.  Test input
  471 !
  472       IF ( .NOT. FLINIT ) THEN
  473           WRITE (NDSE,1000)
  474           CALL EXTCDE ( 1000 )
  475         END IF
  476 !
  477       IF ( NDS.LT.UNITLW .OR. NDS.GT.UNITHG ) THEN
  478           WRITE (NDSE,1001) NDS, UNITLW, UNITHG
  479           CALL EXTCDE ( 1001 )
  480         END IF
  481 !
  482 ! -------------------------------------------------------------------- /
  483 ! 2.  Set data
  484 ! 2.a Flag
  485 !
  486       U_USED(NDS) = FLAG
  487 !
  488 ! 2.b Type
  489 !
  490       IF ( PRESENT(TYPE) ) U_TYPE(NDS) = TYPE
  491 !
  492 ! 2.c Name
  493 !
  494       IF ( PRESENT(NAME) ) THEN
  495           U_NAME(NDS) = NAME
 
 
                                                         wmunitmd.F90  page  10
 
 
  496         ELSE IF ( .NOT. FLAG ) THEN
  497           U_NAME(NDS) = 'unknown'
  498         END IF
  499 !
  500 ! 2.d Description
  501 !
  502       IF ( PRESENT(DESC) ) THEN
  503           U_DESC(NDS) = DESC
  504         ELSE IF ( .NOT. FLAG ) THEN
  505           U_DESC(NDS) = 'unknown'
  506         END IF
  507 !
  508       RETURN
  509 !
  510 ! Formats
  511 !
  512  1000 FORMAT (/' *** ERROR WMUSET: INITIALIZE FIRST !!! ***')
  513  1001 FORMAT (/' *** ERROR WMUSET: UNIT NUMBER OUT OF RANGE ***'      &
  514               /'                   REQ/RANG :',3I6/)
  515 !
  516 !/
  517 !/ End of WMUSET ----------------------------------------------------- /
  518 !/
  519       END SUBROUTINE WMUSET
  520 !/ ------------------------------------------------------------------- /
  521       SUBROUTINE WMUGET ( NDSE, NDST, NDS, TYPE, NR )
  522 !/
  523 !/                  +-----------------------------------+
  524 !/                  | WAVEWATCH III           NOAA/NCEP |
  525 !/                  |           H. L. Tolman            |
  526 !/                  |                        FORTRAN 90 |
  527 !/                  | Last update :         20-Jan-2017 !
  528 !/                  +-----------------------------------+
  529 !/
  530 !/    28-Mar-2005 : Origination.                        ( version 3.07 )
  531 !/    20-Jan-2017 : Add INQUIRE OPENED check.           ( version 6.02 )
  532 !/                  (T. J. Campbell, NRL)
  533 !/
  534 !  1. Purpose :
  535 !
  536 !     Find a free unit number for a given file type.
  537 !
  538 !  2. Method :
  539 !
  540 !     Search the data base.
  541 !
  542 !  3. Parameters :
  543 !
  544 !     Parameter list
  545 !     ----------------------------------------------------------------
  546 !       NDSE    Int.   I   Unit number for error output.
  547 !       NDST    Int.   I   Unit number for test output.
  548 !       NDS     Int.   O   Unit number to be assigned.
  549 !       TYPE    C*3    I   Type identifier to be used.
  550 !       NR      Int.   I   Number of consecutive units needed.
 
 
                                                         wmunitmd.F90  page  11
 
 
  551 !                          Needed for output bounday data files.
  552 !     ----------------------------------------------------------------
  553 !
  554 !  4. Subroutines used :
  555 !
  556 !      Name      Type  Module   Description
  557 !     ----------------------------------------------------------------
  558 !      STRACE    Sur.  W3SERVMD Subroutine tracing.
  559 !      EXCTDE    Sur.    Id.    Program abort.
  560 !     ----------------------------------------------------------------
  561 !
  562 !  5. Called by :
  563 !
  564 !  6. Error messages :
  565 !
  566 !  7. Remarks :
  567 !
  568 !  8. Structure :
  569 !
  570 !  9. Switches :
  571 !
  572 !     !/S    Enable subroutine tracing.
  573 !     !/T    Enable test output
  574 !
  575 ! 10. Source code :
  576 !
  577 !/ ------------------------------------------------------------------- /
  578       USE W3SERVMD, ONLY: EXTCDE
  579 !
  580       IMPLICIT NONE
  581 !/
  582 !/ ------------------------------------------------------------------- /
  583 !/ Parameter list
  584 !/
  585       INTEGER, INTENT(IN)           :: NDSE, NDST
  586       INTEGER, INTENT(OUT)          :: NDS
  587       CHARACTER(LEN=3), INTENT(IN)  :: TYPE
  588       INTEGER, INTENT(IN), OPTIONAL :: NR
  589 !/
  590 !/ ------------------------------------------------------------------- /
  591 !/ Local parameters
  592 !/
  593       INTEGER                 :: NRC, I, J
  594       LOGICAL                 :: OK
  595       LOGICAL                 :: OPND
  596 !/
  597 !
  598 ! -------------------------------------------------------------------- /
  599 ! 1.  Test input / output
  600 !
  601       IF ( .NOT. FLINIT ) THEN
  602           WRITE (NDSE,1010)
  603           CALL EXTCDE ( 1010 )
  604         END IF
  605 !
 
 
                                                         wmunitmd.F90  page  12
 
 
  606       IF ( PRESENT(NR) ) THEN
  607           NRC    = MAX ( 1 , NR )
  608         ELSE
  609           NRC    = 1
  610         END IF
  611 !
  612 ! -------------------------------------------------------------------- /
  613 ! 2.  Find first free unit number and reset flag
  614 !
  615       NDS    = -1
  616 !
  617       DO I=UNITLW, UNITHG - NRC + 1
  618 ! new: We do not allow I=NDST (unit number for test output).
  619 !      NDST (aka MDST or IDST) is set to 10 in call to WMINIT
  620 !      (4th argument)
  621         OK     = .NOT.U_USED(I) .AND. U_TYPE(I).EQ.TYPE  &
  622                  .AND. I.NE.NDST
  623         INQUIRE ( I, OPENED=OPND )
  624         OK     = OK .AND. .NOT.OPND
  625         IF ( OK ) THEN
  626             DO J=1, NRC-1
  627               OK     = OK .AND. (.NOT.U_USED(I+J) .AND.               &
  628                                       U_TYPE(I+J).EQ.TYPE )
  629               INQUIRE ( I+J, OPENED=OPND )
  630               OK     = OK .AND. .NOT.OPND
  631               END DO
  632           END IF
  633         IF ( OK ) THEN
  634             NDS       = I
  635             DO J=0, NRC-1
  636               U_USED(I+J) = .TRUE.
  637               END DO
  638             EXIT
  639           END IF
  640         END DO
  641 !
  642       IF ( NDS .EQ. -1 ) THEN
  643           WRITE (NDSE,1020) TYPE
  644           CALL EXTCDE ( 1020 )
  645         END IF
  646 !
  647       RETURN
  648 !
  649 ! Formats
  650 !
  651  1010 FORMAT (/' *** ERROR WMUGET: INITIALIZE FIRST !!! ***')
  652  1020 FORMAT (/' *** ERROR WMUGET: CANNOT FIND FREE UNIT FOR TYPE ',  &
  653                A,' ***'/)
  654 !
  655 !/
  656 !/ End of WMUGET ----------------------------------------------------- /
  657 !/
  658       END SUBROUTINE WMUGET
  659 !/ ------------------------------------------------------------------- /
  660       SUBROUTINE WMUINQ ( NDSE, NDST, NDS )
 
 
                                                         wmunitmd.F90  page  13
 
 
  661 !/
  662 !/                  +-----------------------------------+
  663 !/                  | WAVEWATCH III           NOAA/NCEP |
  664 !/                  |           H. L. Tolman            |
  665 !/                  |                        FORTRAN 90 |
  666 !/                  | Last update :         29-Mar-2005 !
  667 !/                  +-----------------------------------+
  668 !/
  669 !/    29-Mar-2005 : Origination.                        ( version 3.07 )
  670 !/
  671 !  1. Purpose :
  672 !
  673 !     Update data base information for a given unit number.
  674 !
  675 !  2. Method :
  676 !
  677 !     FORTRAN INQUIRE statement.
  678 !
  679 !  3. Parameters :
  680 !
  681 !     Parameter list
  682 !     ----------------------------------------------------------------
  683 !       NDSE    Int.   I   Unit number for error output.
  684 !       NDST    Int.   I   Unit number for test output.
  685 !       NDS     Int.   I   Unit number to be assigned.
  686 !     ----------------------------------------------------------------
  687 !
  688 !  4. Subroutines used :
  689 !
  690 !      Name      Type  Module   Description
  691 !     ----------------------------------------------------------------
  692 !      STRACE    Sur.  W3SERVMD Subroutine tracing.
  693 !      EXCTDE    Sur.    Id.    Program abort.
  694 !     ----------------------------------------------------------------
  695 !
  696 !  5. Called by :
  697 !
  698 !  6. Error messages :
  699 !
  700 !  7. Remarks :
  701 !
  702 !  8. Structure :
  703 !
  704 !  9. Switches :
  705 !
  706 !     !/S    Enable subroutine tracing.
  707 !     !/T    Enable test output
  708 !
  709 ! 10. Source code :
  710 !
  711 !/ ------------------------------------------------------------------- /
  712       USE W3SERVMD, ONLY: EXTCDE
  713 !
  714       IMPLICIT NONE
  715 !/
 
 
                                                         wmunitmd.F90  page  14
 
 
  716 !/ ------------------------------------------------------------------- /
  717 !/ Parameter list
  718 !/
  719       INTEGER, INTENT(IN)           :: NDSE, NDST, NDS
  720 !/
  721 !/ ------------------------------------------------------------------- /
  722 !/ Local parameters
  723 !/
  724       LOGICAL                 :: CHECK
  725 !/
  726 !
  727 ! -------------------------------------------------------------------- /
  728 ! 1.  Test input / output
  729 !
  730       IF ( .NOT. FLINIT ) THEN
  731           WRITE (NDSE,1010)
  732           CALL EXTCDE ( 1010 )
  733         END IF
  734 !
  735       IF ( NDS.LT.UNITLW .OR. NDS.GT.UNITHG ) THEN
  736           WRITE (NDSE,1011) NDS, UNITLW, UNITHG
  737           CALL EXTCDE ( 1011 )
  738         END IF
  739 !
  740 ! -------------------------------------------------------------------- /
  741 ! 2.  Check out file
  742 ! 2.a Check if opened :
  743 !
  744       INQUIRE (NDS,OPENED=CHECK)
  745 !
  746 ! 2.b File not opened, release to pool
  747 !
  748          IF ( .NOT. CHECK ) THEN
  749              CALL WMUSET ( NDSE, NDST, NDS, .FALSE. )
  750            ELSE
  751 !
  752 ! 2.c File is opened, get the name
  753 !
  754              INQUIRE (NDS,NAME=U_NAME(NDS))
  755 !
  756            END IF
  757 !
  758       RETURN
  759 !
  760 ! Escape locations read errors --------------------------------------- *
  761 !
  762 ! Formats
  763 !
  764  1010 FORMAT (/' *** ERROR WMUINQ: INITIALIZE FIRST !!! ***')
  765  1011 FORMAT (/' *** ERROR WMUINQ: UNIT NUMBER OUT OF RANGE ***'      &
  766               /'                   REQ/RANG :',3I6/)
  767 !
  768 !/
  769 !/ End of WMUINQ ----------------------------------------------------- /
  770 !/
 
 
                                                         wmunitmd.F90  page  15
 
 
  771       END SUBROUTINE WMUINQ
  772 !/
  773 !/ End of module WMUNITMD -------------------------------------------- /
  774 !/
  775       END MODULE WMUNITMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
