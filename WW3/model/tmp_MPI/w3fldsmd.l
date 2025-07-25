                                                         w3fldsmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3FLDSMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |            A. Chawla              |
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :         05-Jun-2018 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    30-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
   14 !/    25-Jan-2002 : Data assimilation set up.           ( version 2.17 )
   15 !/    26-Dec-2002 : Continuously moving grid.           ( version 3.02 )
   16 !/    04-Sep-2003 : Bug fix W3FLHD.                     ( version 3.04 )
   17 !/    27-Dec-2004 : Multiple grid version.              ( version 3.06 )
   18 !/    05-Jul-2005 : Correct first level/ice.            ( version 3.07 )
   19 !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   20 !/    09-Oct-2007 : Make file header optional.          ( version 3.13 )
   21 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   22 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
   23 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   24 !/    04-Apr-2010 : Adding icebergs with ISI.           ( version 3.14 )
   25 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLO (integer) to
   26 !/                  specify index closure for a grid.   ( version 3.14 )
   27 !/                  (T. J. Campbell, NRL)
   28 !/    30-Oct-2012 : Implement tidal analysis            ( version 4.08 )
   29 !/                  (F. Ardhuin)
   30 !/    26-Dec-2012 : Modified obsolete declarations.     ( version 4.OF )
   31 !/     5-Mar-2012 : Cleanup of tidal analysis           ( version 4.09 )
   32 !/    24-Apr-2015 : Adding OASIS coupling calls         ( version 5.07 )
   33 !/                  (M. Accensi & F. Ardhuin, IFREMER)
   34 !/    20-Jan-2017 : Update to new W3GSRUMD APIs         ( version 6.02 )
   35 !/    05-Jun-2018 : adds DEBUGFLS                       ( version 6.04 )
   36 !/
   37 !/    Copyright 2009-2012 National Weather Service (NWS),
   38 !/       National Oceanic and Atmospheric Administration.  All rights
   39 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   40 !/       No unauthorized use without permission.
   41 !/
   42 !  1. Purpose :
   43 !
   44 !     Gathers a set of routines to manage input fields of depth,
   45 !     current, wind and ice concentration.
   46 !
   47 !  2. Variables and types :
   48 !
   49 !  3. Subroutines and functions :
   50 !
   51 !      Name      Type  Scope    Description
   52 !     ----------------------------------------------------------------
   53 !      W3FLDO    Subr. Public   Open data file.
   54 !      W3FLDG    Subr. Public.  Read/write data file (fields).
   55 !      W3FLDD    Subr. Public.  Read/write data file (data).
 
 
                                                         w3fldsmd.F90  page   2
 
 
   56 !      W3FLDP    Subr. Public.  Generic field interpolation.
   57 !      W3FLDH    Subr. Public.  Process homogeneous fields.
   58 !      W3FLDM    Subr. Public.  Process moving grid data.
   59 !      W3FLDTIDE Subr. Public.  Read/write tidal constituents
   60 !     ----------------------------------------------------------------
   61 !
   62 !  4. Subroutines and functions used :
   63 !
   64 !      Name      Type  Module   Description
   65 !     ----------------------------------------------------------------
   66 !      STRACE    Subr. W3SERVMD Subroutine tracing.           ( !/S )
   67 !      TICK21    Subr. W3TIMEMD Increment the clock.
   68 !      DSEC21    R.F.  W3TIMEMD Calculate time differnces.
   69 !     ----------------------------------------------------------------
   70 !
   71 !  5. Remarks :
   72 !
   73 !     - By design, these routines do not use the WAVEWATCH III data
   74 !       structure. With this approach, they can be used in a straight-
   75 !       forward way in other programs to generate WAVEWATCH III input
   76 !       data sets directly from such programs.
   77 !
   78 !  6. Switches :
   79 !
   80 !  7. Source code :
   81 !
   82 !/ ------------------------------------------------------------------- /
   83       PUBLIC
   84       CONTAINS
   85 !/ ------------------------------------------------------------------- /
   86       SUBROUTINE W3FLDO ( INXOUT, IDFLD, NDS, NDST, NDSE, NX, NY,     &
   87                           GTYPE, IERR, FEXT, FPRE, FHDR, TIDEFLAGIN )
   88 !/
   89 !/                  +-----------------------------------+
   90 !/                  | WAVEWATCH III           NOAA/NCEP |
   91 !/                  |           H. L. Tolman            |
   92 !/                  |            A. Chawla              |
   93 !/                  |                        FORTRAN 90 |
   94 !/                  | Last update :         26-Dec-2012 |
   95 !/                  +-----------------------------------+
   96 !/
   97 !/    15-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
   98 !/    30-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
   99 !/    24-Jan-2001 : Flat grid version (formats only)    ( version 2.06 )
  100 !/    24-Jan-2002 : Assimilation data added.            ( version 2.17 )
  101 !/    27-Dec-2004 : Multiple grid version.              ( version 3.06 )
  102 !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
  103 !/    09-Oct-2007 : Make file header optional.          ( version 3.13 )
  104 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
  105 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  106 !/    04-Apr-2010 : Adding iceberg field.               ( version 3.14 )
  107 !/    09-Sep-2012 : Implement tidal cons. (F. Ardhuin ) ( version 4.09 )
  108 !/    26-Dec-2012 : Modified obsolete declarations.     ( version 4.11 )
  109 !/
  110 !  1. Purpose :
 
 
                                                         w3fldsmd.F90  page   3
 
 
  111 !
  112 !     Open and prepare WAVEWATCH III field files as used by the
  113 !     generic shell and the field preprocessor.
  114 !
  115 !  2. Method :
  116 !
  117 !     The file header contains a general WAVEWATCH III ID string,
  118 !     a field ID string and the dimensions of the grid. If a file
  119 !     is opened to be read, these parameters are all checked.
  120 !
  121 !  3. Parameters :
  122 !
  123 !     Parameter list
  124 !     ----------------------------------------------------------------
  125 !       INXOUT  C*(*) I  Test string for read/write, valid are:
  126 !                        'READ' and 'WRITE'.
  127 !       IDFLD   C*3  I/O ID string for field type, valid are: 'IC1',
  128 !                        'IC2', 'IC3', 'IC4', 'IC5', 'MDN', 'MTH',
  129 !                        'MVS', 'LEV', 'CUR', 'WND', 'WNS', 'ICE',
  130 !                        'ISI', and 'DTn'.
  131 !       NDS     Int.  I  Dataset number for fields file.
  132 !       NDST    Int.  I  Dataset number for test output.
  133 !       NDSE    Int.  I  Dataset number for error output.
  134 !                        (No output if NDSE < 0).
  135 !       NX, NY  Int.  I  Discrete grid dimensions.                 \
  136 !       GTYPE   Int.  I  Integer flag indicating type of grid.     /a
  137 !       NX      Int. I/O Record length.                            \
  138 !       GTYPE   Int.  I  Undefined value.                          /b
  139 !       IERR    Int.  O  Error indicator.
  140 !                         0 : No errors.
  141 !                         1 : Illegal INXOUT.
  142 !                         2 : Illegal ID.
  143 !                         3 : Error in opening file.
  144 !                         4 : Write error in file.
  145 !                         5 : Read error in file.
  146 !                         6 : Premature EOF in read.
  147 !                         7 : Unexpected file identifier read.
  148 !                         8 : Unexpected field identifier read.
  149 !                         9 : Unexpected grid dimensions read.
  150 !                        10 : Unexpected data info.
  151 !     ----------------------------------------------------------------
  152 !      a) for output fields.
  153 !      b) for input data.
  154 !
  155 !  4. Subroutines used :
  156 !
  157 !      Name      Type  Module   Description
  158 !     ----------------------------------------------------------------
  159 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  160 !     ----------------------------------------------------------------
  161 !
  162 !  5. Called by :
  163 !
  164 !      Name      Type  Module   Description
  165 !     ----------------------------------------------------------------
 
 
                                                         w3fldsmd.F90  page   4
 
 
  166 !      WW3_PREP  Prog.   N/A    Input data preprocessor.
  167 !      WW3_SHEL  Prog.   N/A    Basic wave model driver.
  168 !      ......    Prog.   N/A    Any other program that reads or
  169 !                               writes WAVEWATCH III data files.
  170 !     ----------------------------------------------------------------
  171 !
  172 !  6. Error messages :
  173 !
  174 !     See end of subroutine.
  175 !
  176 !  7. Remarks :
  177 !
  178 !     - On read, the ID 'WND' may be changed to 'WNS' (including
  179 !       stability data).
  180 !     - On read, the ID 'ICE' may be changed to 'ISI' (including
  181 !       iceberg data).
  182 !
  183 !  8. Structure :
  184 !
  185 !     See source code.
  186 !
  187 !  9. Switches :
  188 !
  189 !     !/S  Enable subroutine tracing.
  190 !     !/T  Enable test output.
  191 !
  192 ! 10. Source code :
  193 !
  194 !/ ------------------------------------------------------------------- /
  195 !/
  196 !
  197       IMPLICIT NONE
  198 !/
  199 !/ ------------------------------------------------------------------- /
  200 !/ Parameter list
  201 !/
  202       INTEGER, INTENT(IN)                 :: NDS, NDST, NDSE, NY
  203       INTEGER, INTENT(INOUT)              :: NX
  204       INTEGER, INTENT(OUT)                :: IERR
  205       INTEGER, INTENT(INOUT)              :: GTYPE
  206       CHARACTER(LEN=3), INTENT(INOUT)     :: IDFLD
  207       CHARACTER, INTENT(IN)               :: INXOUT*(*)
  208       CHARACTER, INTENT(IN), OPTIONAL     :: FEXT*(*), FPRE*(*)
  209       LOGICAL, INTENT(IN), OPTIONAL       :: FHDR
  210       INTEGER, INTENT(INOUT), OPTIONAL    :: TIDEFLAGIN
  211 !/
  212 !/ ------------------------------------------------------------------- /
  213 !/ Local parameters
  214 !/
  215       INTEGER                 :: NXT, NYT, GTYPET, I
  216       INTEGER                 :: FILLER(3)
  217       LOGICAL                 :: WRITE
  218       CHARACTER(LEN=3)        :: TSFLD
  219       CHARACTER(LEN=11)       :: FORM = 'UNFORMATTED'
  220       CHARACTER(LEN=13)       :: TSSTR, IDSTR = 'WAVEWATCH III'
 
 
                                                         w3fldsmd.F90  page   5
 
 
  221       CHARACTER(LEN=20)       :: TEMPXT
  222       CHARACTER(LEN=30)       :: FNAME
  223       LOGICAL                 :: FDHDR = .TRUE.
  224       INTEGER                 :: TIDEFLAG = 0
  225       LOGICAL                 ::  TIDEOK = .FALSE.
  226 !
  227 ! 'FORM' is used for initial testing of new files only.
  228 !/
  229 !/ ------------------------------------------------------------------- /
  230 !/
  231 !
  232 ! test input parameters ---------------------------------------------- *
  233 !
  234       FILLER(:)=0
  235       IF ( PRESENT(TIDEFLAGIN) ) THEN
  236         TIDEFLAG = TIDEFLAGIN
  237       ELSE
  238         TIDEFLAG = 0
  239         END IF
  240               
  241       IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE') GOTO 801
  242       IF ( IDFLD.NE.'IC1' .AND. IDFLD.NE.'IC2' .AND.                  &
  243            IDFLD.NE.'IC3' .AND. IDFLD.NE.'IC4' .AND.                  &
  244            IDFLD.NE.'IC5' .AND. IDFLD.NE.'MDN' .AND.                  &
  245            IDFLD.NE.'MTH' .AND. IDFLD.NE.'MVS' .AND.                  &
  246            IDFLD.NE.'LEV' .AND. IDFLD.NE.'CUR' .AND.                  &
  247            IDFLD.NE.'WND' .AND. IDFLD.NE.'WNS' .AND.                  &
  248            IDFLD.NE.'ICE' .AND. IDFLD.NE.'DT0' .AND.                  &
  249            IDFLD.NE.'DT1' .AND. IDFLD.NE.'DT2' .AND.                  &
  250            IDFLD.NE.'ISI' )    GOTO 802
  251 !
  252       IF ( PRESENT(FEXT) ) THEN
  253           TEMPXT = FEXT
  254           I      = LEN_TRIM(FEXT)
  255         ELSE
  256           TEMPXT = 'ww3'
  257           I      = 3
  258         END IF
  259 !
  260       IF ( PRESENT(FHDR) ) THEN
  261           FDHDR = FHDR
  262         END IF
  263 !
  264 ! Set internal variables --------------------------------------------- *
  265 !
  266       IF ( IDFLD.EQ.'LEV' ) THEN
  267           FNAME = 'level.' // TEMPXT(:I)
  268           I     = I + 6
  269         ELSE IF ( IDFLD.EQ.'CUR' ) THEN
  270           FNAME = 'current.' // TEMPXT(:I)
  271           I     = I + 8
  272         ELSE IF ( IDFLD.EQ.'WND' .OR. IDFLD.EQ.'WNS' ) THEN
  273           FNAME = 'wind.' // TEMPXT(:I)
  274           I     = I + 5
  275         ELSE IF ( IDFLD.EQ.'ICE' .OR. IDFLD.EQ.'ISI' ) THEN
 
 
                                                         w3fldsmd.F90  page   6
 
 
  276           FNAME = 'ice.' // TEMPXT(:I)
  277           I     = I + 4
  278         ELSE IF ( IDFLD.EQ.'DT0' ) THEN
  279           FNAME = 'data0.' // TEMPXT(:I)
  280           I     = I + 6
  281         ELSE IF ( IDFLD.EQ.'DT1' ) THEN
  282           FNAME = 'data1.' // TEMPXT(:I)
  283           I     = I + 6
  284         ELSE IF ( IDFLD.EQ.'DT2' ) THEN
  285           FNAME = 'data2.' // TEMPXT(:I)
  286           I     = I + 6
  287         ELSE IF ( IDFLD.EQ.'MDN' ) THEN
  288           FNAME = 'muddens.' // TEMPXT(:I)
  289           I     = I + 8
  290         ELSE IF ( IDFLD.EQ.'MTH' ) THEN
  291           FNAME = 'mudthk.' // TEMPXT(:I)
  292           I     = I + 7
  293         ELSE IF ( IDFLD.EQ.'MVS' ) THEN
  294           FNAME = 'mudvisc.' // TEMPXT(:I)
  295           I     = I + 8
  296         ELSE IF ( IDFLD.EQ.'IC1' ) THEN
  297           FNAME = 'ice1.' // TEMPXT(:I)
  298           I     = I + 5
  299         ELSE IF ( IDFLD.EQ.'IC2' ) THEN
  300           FNAME = 'ice2.' // TEMPXT(:I)
  301           I     = I + 5
  302         ELSE IF ( IDFLD.EQ.'IC3' ) THEN
  303           FNAME = 'ice3.' // TEMPXT(:I)
  304           I     = I + 5
  305         ELSE IF ( IDFLD.EQ.'IC4' ) THEN
  306           FNAME = 'ice4.' // TEMPXT(:I)
  307           I     = I + 5
  308         ELSE IF ( IDFLD.EQ.'IC5' ) THEN
  309           FNAME = 'ice5.' // TEMPXT(:I)
  310           I     = I + 5
  311         END IF
  312 !
  313       WRITE  = INXOUT .EQ. 'WRITE'
  314 !
  315 ! Open file ---------------------------------------------------------- *
  316 !
  317       IF ( WRITE ) THEN
  318           IF ( PRESENT(FPRE) ) THEN
  319               OPEN (NDS,FILE=FPRE//FNAME(:I),FORM=FORM,ERR=803,       &
  320                     IOSTAT=IERR)
  321           ELSE
  322               OPEN (NDS,FILE=FNAME(:I),FORM=FORM,ERR=803,IOSTAT=IERR)
  323             END IF
  324         ELSE
  325           IF ( PRESENT(FPRE) ) THEN
  326               OPEN (NDS,FILE=FPRE//FNAME(:I),FORM=FORM,               &
  327                     STATUS='OLD',ERR=803,IOSTAT=IERR)
  328           ELSE
  329               OPEN (NDS,FILE=FNAME(:I),FORM=FORM,                     &
  330                     STATUS='OLD',ERR=803,IOSTAT=IERR)
 
 
                                                         w3fldsmd.F90  page   7
 
 
  331             END IF
  332         END IF
  333 !
  334 ! Process test data -------------------------------------------------- *
  335 !
  336       IF ( WRITE ) THEN
  337           IF ( FDHDR ) THEN
  338              IF ( FORM .EQ. 'UNFORMATTED' ) THEN
  339 !
  340 ! The "filler" was added for compatibility with old binary forcing files
  341 ! It is now also used for tidal info ...
  342 !
  343                  WRITE (NDS,ERR=804,IOSTAT=IERR)                      &
  344                        IDSTR, IDFLD, NX, NY, GTYPE, FILLER(1:2), TIDEFLAG
  345                ELSE
  346                  WRITE (NDS,900,ERR=804,IOSTAT=IERR)                  &
  347                        IDSTR, IDFLD, NX, NY, GTYPE, FILLER(1:2), TIDEFLAG
  348                END IF
  349             END IF
  350         ELSE
  351           IF ( FORM .EQ. 'UNFORMATTED' ) THEN
  352               READ (NDS,END=806,ERR=805,IOSTAT=IERR)                  &
  353                     TSSTR, TSFLD, NXT, NYT, GTYPET, FILLER(1:2), TIDEFLAG
  354           ELSE
  355               READ (NDS,900,END=806,ERR=805,IOSTAT=IERR)              &
  356                     TSSTR, TSFLD, NXT, NYT, GTYPET, FILLER(1:2), TIDEFLAG
  357           END IF
  358           IF ((FILLER(1).NE.0.OR.FILLER(2).NE.0).AND.TIDEFLAG.GE.0) TIDEF>
  359           IF (TIDEFLAG.NE.0.AND.(.NOT.TIDEOK)) THEN
  360             GOTO 810
  361             END IF
  362 !
  363           IF ( IDSTR .NE. TSSTR ) GOTO 807
  364           IF (( IDFLD.EQ.'WND' .AND. TSFLD.EQ.'WNS') .OR.             &
  365               ( IDFLD.EQ.'ICE' .AND. TSFLD.EQ.'ISI')  ) THEN
  366               IDFLD  = TSFLD
  367             END IF
  368           IF ( IDFLD .NE. TSFLD ) GOTO 808
  369           IF ( IDFLD(1:2) .NE. 'DT' ) THEN
  370             IF ( NX.NE.NXT .OR. NY.NE.NYT ) THEN
  371               GOTO 809
  372             ELSE
  373               NX     = NXT
  374               IF (GTYPE.LE.3) GTYPE  = GTYPET
  375               END IF
  376             END IF
  377         END IF
  378 !
  379 ! File OK ------------------------------------------------------------ *
  380 !
  381       IERR   = 0
  382       IF ( PRESENT(TIDEFLAGIN) ) THEN
  383           TIDEFLAGIN = TIDEFLAG
  384         END IF
  385               
 
 
                                                         w3fldsmd.F90  page   8
 
 
  386       RETURN
  387 !
  388 ! Error escape locations
  389 !
  390   801 CONTINUE
  391       IF ( NDSE .GE. 0 ) WRITE (NDSE,1001) INXOUT
  392       IERR   = 1
  393       RETURN
  394 !
  395   802 CONTINUE
  396       IF ( NDSE .GE. 0 ) WRITE (NDSE,1002) IDFLD
  397       IERR   = 2
  398       RETURN
  399 !
  400   803 CONTINUE
  401       IF ( NDSE .GE. 0 ) WRITE (NDSE,1003) IDFLD, IERR
  402       IERR   = 3
  403       RETURN
  404 !
  405   804 CONTINUE
  406       IF ( NDSE .GE. 0 ) WRITE (NDSE,1004) IDFLD, IERR
  407       IERR   = 4
  408       RETURN
  409 !
  410   805 CONTINUE
  411       IF ( NDSE .GE. 0 ) WRITE (NDSE,1005) IDFLD, IERR
  412       IERR   = 5
  413       RETURN
  414 !
  415   806 CONTINUE
  416       IF ( NDSE .GE. 0 ) WRITE (NDSE,1006) IDFLD
  417       IERR   = 6
  418       RETURN
  419 !
  420   807 CONTINUE
  421       IF ( NDSE .GE. 0 ) WRITE (NDSE,1007) TSSTR, IDSTR
  422       IERR   = 7
  423       RETURN
  424 !
  425   808 CONTINUE
  426       IF ( NDSE .GE. 0 ) WRITE (NDSE,1008) TSFLD, IDFLD
  427       IERR   = 8
  428       RETURN
  429 !
  430   809 CONTINUE
  431       IF ( NDSE .GE. 0 ) WRITE (NDSE,1009)                      &
  432                                 NXT, NYT, GTYPET,               &
  433                                 NX , NY , GTYPE
  434       IERR   = 9
  435       RETURN
  436 !
  437   810 CONTINUE
  438       IF ( NDSE .GE. 0 ) WRITE (NDSE,1010)                      &
  439                                 FILLER(1:2),TIDEFLAG
  440       IERR   = 10
 
 
                                                         w3fldsmd.F90  page   9
 
 
  441       RETURN
  442 !
  443 ! Formats
  444 !
  445   900 FORMAT (1X,A13,1X,A3,6I12)
  446 !
  447  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDO : '/         &
  448                '     ILLEGAL INXOUT STRING : ',A/)
  449  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDO : '/         &
  450                '     ILLEGAL FIELD ID STRING : ',A/)
  451  1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDO : '/         &
  452                '     ERROR IN OPENING ',A,' FILE, IOSTAT =',I6/)
  453  1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDO : '/         &
  454                '     ERROR IN WRITING TO ',A,' FILE, IOSTAT =',I6/)
  455  1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDO : '/         &
  456                '     ERROR IN READING ',A,' FILE, IOSTAT =',I6/)
  457                                                                 
  458  1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDO : '/         &
  459                '     PREMATURE END OF ',A,' FILE'/)
  460  1007 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDO : '/         &
  461                '     ILLEGAL FILE ID STRING >',A,'<'/           &
  462                '                  SHOULD BE >',A,'<'/)
  463  1008 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDO : '/         &
  464                '     ILLEGAL FIELD ID STRING >',A,'<'/          &
  465                '                   SHOULD BE >',A,'<'/)
  466  1009 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDO : '/         &
  467          '     INCOMPATIBLE GRID DATA : ',3(1X,I10)/             &
  468          '                  SHOULD BE : ',3(1X,I10)/)
  469  1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDO : '/          &
  470          '     FILLER indicates use of tidal constituents',3I4, /&
  471          '     For this the code should be compiled with TIDE switch'/)
  472 !
  473 !/
  474 !/ End of W3FLDO  ---------------------------------------------------- /
  475 !/
  476       END SUBROUTINE W3FLDO
  477                            
  478 !/ ------------------------------------------------------------------- /
  479       SUBROUTINE W3FLDTIDE1 ( INXOUT, NDS, NDST, NDSE, NX, NY, IDFLD, IER>
  480 !/
  481 !/                  +-----------------------------------+
  482 !/                  | WAVEWATCH III           NOAA/NCEP |
  483 !/                  |           F. Ardhuin              |
  484 !/                  |                                   |
  485 !/                  |                        FORTRAN 90 |
  486 !/                  | Last update :         30-Jun-2013 |
  487 !/                  +-----------------------------------+
  488 !/
  489 !/    24-Sep-2012 : Creation                            ( version 4.09 )
  490 !/    30-Jun-2013 : Split in 2 subroutines              ( version 4.11 )
  491 !/
  492 !  1. Purpose :
  493 !
  494 !     Reads and writes tidal consituents
  495 !
 
 
                                                         w3fldsmd.F90  page  10
 
 
  496 !  2. Method :
  497 !
  498 !  3. Parameters :
  499 !
  500 !     Parameter list
  501 !     ----------------------------------------------------------------
  502 !       INXOUT  C*(*) I  Test string for read/write, valid are:
  503 !                        'READ' and 'WRITE'.
  504 !       IDFLD   C*3  I/O ID string for field type, valid are:
  505 !                        'LEV', 'CUR', 'WND', 'WNS', 'ICE', 'ISI', and 'D>
  506 !       NDS     Int.  I  Dataset number for fields file.
  507 !       NDST    Int.  I  Dataset number for test output.
  508 !       NDSE    Int.  I  Dataset number for error output.
  509 !                        (No output if NDSE < 0).
  510 !       NX, NY  Int.  I  Discrete grid dimensions.                 \
  511 !       IERR    Int.  O  Error indicator.
  512 !                         0 : No errors.
  513 !                         1 : Illegal INXOUT.
  514 !     ----------------------------------------------------------------
  515 !      a) for output fields.
  516 !      b) for input data.
  517 !
  518 !  4. Subroutines used :
  519 !
  520 !      Name      Type  Module   Description
  521 !     ----------------------------------------------------------------
  522 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  523 !     ----------------------------------------------------------------
  524 !
  525 !  5. Called by :
  526 !
  527 !      Name      Type  Module   Description
  528 !     ----------------------------------------------------------------
  529 !      WW3_PREP  Prog.   N/A    Input data preprocessor.
  530 !      WW3_PRNC  Prog.   N/A    NetCDF input data preprocessor.
  531 !      WW3_SHEL  Prog.   N/A    Basic wave model driver.
  532 !     ----------------------------------------------------------------
  533 !
  534 !  6. Error messages :
  535 !
  536 !     See end of subroutine.
  537 !
  538 !  7. Remarks :
  539 !
  540 !     - On read, the ID 'WND' may be changed to 'WNS' (including
  541 !       stability data).
  542 !     - On read, the ID 'ICE' may be changed to 'ISI' (including
  543 !       iceberg data).
  544 !
  545 !  8. Structure :
  546 !
  547 !     See source code.
  548 !
  549 !  9. Switches :
  550 !
 
 
                                                         w3fldsmd.F90  page  11
 
 
  551 !     !/S  Enable subroutine tracing.
  552 !     !/T  Enable test output.
  553 !
  554 ! 10. Source code :
  555 !
  556 !/ ------------------------------------------------------------------- /
  557 !/
  558 !
  559       USE W3IDATMD
  560       IMPLICIT NONE
  561 !/
  562 !/ ------------------------------------------------------------------- /
  563 !/ Parameter list
  564 !/
  565       INTEGER, INTENT(IN)                 :: NDS, NDST, NDSE, NX, NY
  566       CHARACTER(LEN=3), INTENT(INOUT)     :: IDFLD
  567       CHARACTER*(*), INTENT(IN)           :: INXOUT
  568       INTEGER, INTENT(OUT)                :: IERR
  569 !/
  570 !/ ------------------------------------------------------------------- /
  571 !/ Local parameters
  572 !/
  573       LOGICAL                 :: WRITE
  574       INTEGER                 :: I, IX
  575 !
  576 !/
  577 !/ ------------------------------------------------------------------- /
  578 !/
  579 !
  580 ! test input parameters ---------------------------------------------- *
  581 !
  582       IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE') GOTO 801
  583       IF ( IDFLD.NE.'LEV' .AND. IDFLD.NE.'CUR' .AND.                  &
  584            IDFLD.NE.'WND' .AND. IDFLD.NE.'WNS' .AND.                  &
  585            IDFLD.NE.'ICE' .AND. IDFLD.NE.'DT0' .AND.                  &
  586            IDFLD.NE.'DT1' .AND. IDFLD.NE.'DT2' .AND.                  &
  587            IDFLD.NE.'ISI' )    GOTO 802
  588       WRITE  = INXOUT .EQ. 'WRITE'
  589                                   
  590 !
  591 ! File OK ------------------------------------------------------------ *
  592 !
  593       IERR   = 0
  594       RETURN
  595 !
  596 ! Error escape locations
  597 !
  598   801 CONTINUE
  599       IF ( NDSE .GE. 0 ) WRITE (NDSE,1001) INXOUT
  600       IERR   = 1
  601       RETURN
  602 !
  603   802 CONTINUE
  604       IF ( NDSE .GE. 0 ) WRITE (NDSE,1002) IDFLD
  605       IERR   = 2
 
 
                                                         w3fldsmd.F90  page  12
 
 
  606       RETURN
  607 !
  608   804 CONTINUE
  609       IF ( NDSE .GE. 0 ) WRITE (NDSE,1004) IDFLD, IERR
  610       IERR   = 4
  611       RETURN
  612 !
  613   805 CONTINUE
  614       IF ( NDSE .GE. 0 ) WRITE (NDSE,1005) IDFLD, IERR
  615       IERR   = 5
  616       RETURN
  617 !
  618   806 CONTINUE
  619       IF ( NDSE .GE. 0 ) WRITE (NDSE,1006) IDFLD
  620       IERR   = 6
  621       RETURN
  622 !
  623 ! Formats
  624 !
  625  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDTIDE1 : '/        &
  626                '     ILLEGAL INXOUT STRING : ',A/)
  627  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDTIDE1 : '/        &
  628                '     ILLEGAL FIELD ID STRING : ',A/)
  629  1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDTIDE1 : '/        &
  630                '     ERROR IN WRITING TO ',A,' FILE, IOSTAT =',I6/)
  631  1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDTIDE1 : '/        &
  632                '     ERROR IN READING ',A,' FILE, IOSTAT =',I6/)
  633                                                                 
  634  1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDTIDE1 : '/        &
  635                '     PREMATURE END OF ',A,' FILE'/)
  636 !/
  637 !/ End of W3FLDO  ---------------------------------------------------- /
  638 !/
  639       END SUBROUTINE W3FLDTIDE1
  640                                
  641                                
  642 !/ ------------------------------------------------------------------- /
  643       SUBROUTINE W3FLDTIDE2 ( INXOUT, NDS, NDST, NDSE, NX, NY, IDFLD, IDA>
  644 !/
  645 !/                  +-----------------------------------+
  646 !/                  | WAVEWATCH III           NOAA/NCEP |
  647 !/                  |           F. Ardhuin              |
  648 !/                  |                                   |
  649 !/                  |                        FORTRAN 90 |
  650 !/                  | Last update :         30-Jun-2013 |
  651 !/                  +-----------------------------------+
  652 !/
  653 !/    24-Sep-2012 : Creation                            ( version 4.09 )
  654 !/    30-Jun-2013 : Split in 2 subroutines              ( version 4.11 )
  655 !/
  656 !  1. Purpose :
  657 !
  658 !     Reads and writes tidal constituents
  659 !
  660 !  2. Method :
 
 
                                                         w3fldsmd.F90  page  13
 
 
  661 !
  662 !  3. Parameters :
  663 !
  664 !     Parameter list
  665 !     ----------------------------------------------------------------
  666 !       INXOUT  C*(*) I  Test string for read/write, valid are:
  667 !                        'READ' and 'WRITE'.
  668 !       IDFLD   C*3  I/O ID string for field type, valid are:
  669 !                        'LEV', 'CUR', 'WND', 'WNS', 'ICE', 'ISI', and 'D>
  670 !       NDS     Int.  I  Dataset number for fields file.
  671 !       NDST    Int.  I  Dataset number for test output.
  672 !       NDSE    Int.  I  Dataset number for error output.
  673 !                        (No output if NDSE < 0).
  674 !       NX, NY  Int.  I  Discrete grid dimensions.                 \
  675 !       IDAT    Int.  I  Equal to 1 if W3IDATMD arrays are to be filled
  676 !       IERR    Int.  O  Error indicator.
  677 !                         0 : No errors.
  678 !                         1 : Illegal INXOUT.
  679 !     ----------------------------------------------------------------
  680 !      a) for output fields.
  681 !      b) for input data.
  682 !
  683 !  4. Subroutines used :
  684 !
  685 !      Name      Type  Module   Description
  686 !     ----------------------------------------------------------------
  687 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  688 !     ----------------------------------------------------------------
  689 !
  690 !  5. Called by :
  691 !
  692 !      Name      Type  Module   Description
  693 !     ----------------------------------------------------------------
  694 !      WW3_PREP  Prog.   N/A    Input data preprocessor.
  695 !      WW3_PRNC  Prog.   N/A    NetCDF input data preprocessor.
  696 !      WW3_SHEL  Prog.   N/A    Basic wave model driver.
  697 !     ----------------------------------------------------------------
  698 !
  699 !  6. Error messages :
  700 !
  701 !     See end of subroutine.
  702 !
  703 !  7. Remarks :
  704 !
  705 !     - On read, the ID 'WND' may be changed to 'WNS' (including
  706 !       stability data).
  707 !     - On read, the ID 'ICE' may be changed to 'ISI' (including
  708 !       iceberg data).
  709 !
  710 !  8. Structure :
  711 !
  712 !     See source code.
  713 !
  714 !  9. Switches :
  715 !
 
 
                                                         w3fldsmd.F90  page  14
 
 
  716 !     !/S  Enable subroutine tracing.
  717 !     !/T  Enable test output.
  718 !
  719 ! 10. Source code :
  720 !
  721 !/ ------------------------------------------------------------------- /
  722 !/
  723 !
  724       USE W3IDATMD
  725       IMPLICIT NONE
  726 !/
  727 !/ ------------------------------------------------------------------- /
  728 !/ Parameter list
  729 !/
  730       INTEGER, INTENT(IN)                 :: NDS, NDST, NDSE, NX, NY, IDAT
  731       CHARACTER(LEN=3), INTENT(INOUT)     :: IDFLD
  732       CHARACTER*(*), INTENT(IN)           :: INXOUT
  733       INTEGER, INTENT(OUT)                :: IERR
  734 !/
  735 !/ ------------------------------------------------------------------- /
  736 !/ Local parameters
  737 !/
  738       LOGICAL                 :: WRITE
  739       INTEGER                 :: I, IX, TIDE_MF1
  740       CHARACTER(LEN=100)      :: LIST(70)
  741 !/
  742 !/ ------------------------------------------------------------------- /
  743 !/
  744 !
  745 ! test input parameters ---------------------------------------------- *
  746 !
  747       IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE') GOTO 801
  748       IF ( IDFLD.NE.'LEV' .AND. IDFLD.NE.'CUR' .AND.                  &
  749            IDFLD.NE.'WND' .AND. IDFLD.NE.'WNS' .AND.                  &
  750            IDFLD.NE.'ICE' .AND. IDFLD.NE.'DT0' .AND.                  &
  751            IDFLD.NE.'DT1' .AND. IDFLD.NE.'DT2' .AND.                  &
  752            IDFLD.NE.'ISI' )    GOTO 802
  753       WRITE  = INXOUT .EQ. 'WRITE'
  754                                   
  755 !
  756 ! File OK ------------------------------------------------------------ *
  757 !
  758       IERR   = 0
  759       RETURN
  760 !
  761 ! Error escape locations
  762 !
  763   801 CONTINUE
  764       IF ( NDSE .GE. 0 ) WRITE (NDSE,1001) INXOUT
  765       IERR   = 1
  766       RETURN
  767 !
  768   802 CONTINUE
  769       IF ( NDSE .GE. 0 ) WRITE (NDSE,1002) IDFLD
  770       IERR   = 2
 
 
                                                         w3fldsmd.F90  page  15
 
 
  771       RETURN
  772 !
  773   804 CONTINUE
  774       IF ( NDSE .GE. 0 ) WRITE (NDSE,1004) IDFLD, IERR
  775       IERR   = 4
  776       RETURN
  777 !
  778   805 CONTINUE
  779       IF ( NDSE .GE. 0 ) WRITE (NDSE,1005) IDFLD, IERR
  780       IERR   = 5
  781       RETURN
  782 !
  783   806 CONTINUE
  784       IF ( NDSE .GE. 0 ) WRITE (NDSE,1006) IDFLD
  785       IERR   = 6
  786       RETURN
  787 !
  788   807 CONTINUE
  789       IERR   = 7
  790       RETURN
  791 !
  792 ! Formats
  793 !
  794  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDTIDE2 : '/         &
  795                '     ILLEGAL INXOUT STRING : ',A/)
  796  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDTIDE2 : '/         &
  797                '     ILLEGAL FIELD ID STRING : ',A/)
  798  1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDTIDE2 : '/         &
  799                '     ERROR IN WRITING TO ',A,' FILE, IOSTAT =',I6/)
  800  1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDTIDE2 : '/         &
  801                '     ERROR IN READING ',A,' FILE, IOSTAT =',I6/)
  802  1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDTIDE2 : '/         &
  803                '     PREMATURE END OF ',A,' FILE'/)
  804 !/
  805 !/ End of W3FLDO  ---------------------------------------------------- /
  806 !/
  807       END SUBROUTINE W3FLDTIDE2
  808                                
  809 !/ ------------------------------------------------------------------- /
  810       SUBROUTINE W3FLDG (INXOUT, IDFLD, NDS, NDST, NDSE, MX, MY,      &
  811                          NX, NY, T0, TN, TF0, FX0, FY0, FA0,          &
  812                          TFN, FXN, FYN, FAN, IERR, FLAGSC             &
  813                          )
  814 !/
  815 !/                  +-----------------------------------+
  816 !/                  | WAVEWATCH III           NOAA/NCEP |
  817 !/                  |           H. L. Tolman            |
  818 !/                  |                        FORTRAN 90 |
  819 !/                  | Last update :         25-Sep-2020 |
  820 !/                  +-----------------------------------+
  821 !/
  822 !/    15-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
  823 !/    30-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  824 !/    05-Jul-2005 : Correct first level/ice.            ( version 3.07 )
  825 !/    04-Apr-2010 : Adding icebergs in ISI              ( version 3.14 )
 
 
                                                         w3fldsmd.F90  page  16
 
 
  826 !/    26-Dec-2012 : Modified obsolete declarations.     ( version 4.11 )
  827 !/    24-Apr-2015 : Adding OASIS coupling calls         ( version 5.07 )
  828 !/                  (M. Accensi & F. Ardhuin, IFREMER)
  829 !/    25-Sep-2020 : Receive coupled fields at T+0       ( version 7.10 )
  830 !/
  831 !  1. Purpose :
  832 !
  833 !     Update input fields in the WAVEWATCH III generic shell from a
  834 !     WAVEWATCH III shell data file or write from preprocessor.
  835 !
  836 !  2. Method :
  837 !
  838 !     Read from file opened by W3FLDO.
  839 !
  840 !  3. Parameters :
  841 !
  842 !     Parameter list
  843 !     ----------------------------------------------------------------
  844 !       INXOUT  C*(*)  I   Test string for read/write, valid are:
  845 !                          'READ' and 'WRITE'.
  846 !       IDFLD   C*3    I   ID string for field type, valid are: 'IC1',
  847 !                          'IC2', 'IC3', 'IC4', 'IC5', 'MDN', 'MTH', 'MVS>
  848 !                          'LEV', 'CUR', 'WND', 'WNS', 'ICE' and 'ISI'.
  849 !       NDS     Int.   I   Dataset number for fields file.
  850 !       NDST    Int.   I   Dataset number for test output.
  851 !       NDSE    Int.   I   Dataset number for error output.
  852 !                          (No error output if NDSE < 0 ).
  853 !       MX,MY   Int.   I   Array dimensions output fields.
  854 !       NX,NY   Int.   I   Discrete grid dimensions.
  855 !       T0-N    I.A.   I   Time interval considered (dummy for write).
  856 !       TF0-N   I.A.  I/O  Field times (TFN dummy for write).
  857 !       Fxx     R.A.  I/O  Input fields (FxN dummy for write).
  858 !            subtypes: FX0, FY0, FA0, FXN, FYN, FAN
  859 !                    (meaning is inferred from context as follows)
  860 !                      "0" denotes "prior time level"
  861 !                      "N" denotes "next time level"
  862 !                      "X" denotes x in a vector
  863 !                      "Y" denotes y in a vector
  864 !                      "A" denotes scalar
  865 !       IERR    Int.   O   Error indicator,
  866 !                          -1 Past last data
  867 !                           0 OK,
  868 !                           1 : Illegal INXOUT.
  869 !                           2 : Illegal IDFLD.
  870 !                           3 : Error in writing time.
  871 !                           4 : Error in writing field.
  872 !                           5 : Error in reading time.
  873 !                           6 : Premature EOF reading field.
  874 !                           7 : Error reading field.
  875 !       FLAGSC  Log.  I/O  Flag for coupling field
  876 !       COUPL_COMM Int. I  MPI communicator for coupling
  877 !     ----------------------------------------------------------------
  878 !
  879 !  4. Subroutines used :
  880 !
 
 
                                                         w3fldsmd.F90  page  17
 
 
  881 !      Name      Type  Module   Description
  882 !     ----------------------------------------------------------------
  883 !      STRACE    Subr.   Id.    Subroutine tracing.
  884 !      TICK21    Subr. W3TIMEMD Advance time.
  885 !      DSEC21    Func.   Id.    Difference between times.
  886 !     ----------------------------------------------------------------
  887 !
  888 !  5. Called by :
  889 !
  890 !      Name      Type  Module   Description
  891 !     ----------------------------------------------------------------
  892 !      WW3_PREP  Prog.   N/A    Input data preprocessor.
  893 !      WW3_SHEL  Prog.   N/A    Basic wave model driver.
  894 !      ......    Prog.   N/A    Any other program that reads or
  895 !                               writes WAVEWATCH III data files.
  896 !     ----------------------------------------------------------------
  897 !
  898 !  6. Error messages :
  899 !
  900 !     See end of subroutine.
  901 !
  902 !  7. Remarks :
  903 !
  904 !     - Saving of previous fields needed only for reading of 2-D fields.
  905 !
  906 !  8. Structure :
  907 !
  908 !     See source code.
  909 !
  910 !  9. Switches :
  911 !
  912 !     !/S  Enable subroutine tracing.
  913 !     !/T  Enable test output.
  914 !
  915 ! 10. Source code :
  916 !
  917 !/ ------------------------------------------------------------------- /
  918 !/
  919       USE W3TIMEMD
  920       IMPLICIT NONE
  921 !/
  922 !/ ------------------------------------------------------------------- /
  923 !/ Parameter list
  924 !/
  925       INTEGER, INTENT(IN)          :: NDS, NDST, NDSE, MX, MY,        &
  926                                       NX, NY, T0(2), TN(2)
  927       INTEGER, INTENT(INOUT)       :: TF0(2), TFN(2)
  928       INTEGER, INTENT(OUT)         :: IERR
  929       REAL, INTENT(INOUT)          :: FX0(MX,MY), FY0(MX,MY),         &
  930                                       FXN(MX,MY), FYN(MX,MY),         &
  931                                       FA0(MX,MY), FAN(MX,MY)
  932       CHARACTER, INTENT(IN)        :: INXOUT*(*)
  933       CHARACTER(LEN=3), INTENT(IN) :: IDFLD
  934       LOGICAL, INTENT(INOUT), OPTIONAL        :: FLAGSC
  935                                                        
 
 
                                                         w3fldsmd.F90  page  18
 
 
  936 !/
  937 !/ ------------------------------------------------------------------- /
  938 !/ Local parameters
  939 !/
  940       INTEGER                 :: IX, IY, J, ISTAT
  941       REAL                    :: DTTST
  942       LOGICAL                 :: WRITE, FL2D, FLFRST, FLBE, FLST,    &
  943                                  FLINTERP, FLCOUPL
  944       LOGICAL, PARAMETER      :: FLAGSC_DEFAULT = .FALSE.
  945 !/
  946 !/ ------------------------------------------------------------------- /
  947 !/
  948 !/
  949       IERR   = 0
  950 !
  951 ! test input parameters ---------------------------------------------- *
  952 !
  953       IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE') GOTO 801
  954       IF ( IDFLD.NE.'IC1' .AND. IDFLD.NE.'IC2' .AND.                  &
  955            IDFLD.NE.'IC3' .AND. IDFLD.NE.'IC4' .AND.                  &
  956            IDFLD.NE.'IC5' .AND. IDFLD.NE.'MDN' .AND.                  &
  957            IDFLD.NE.'MTH' .AND. IDFLD.NE.'MVS' .AND.                  &
  958            IDFLD.NE.'LEV' .AND. IDFLD.NE.'CUR' .AND.                  &
  959            IDFLD.NE.'WND' .AND. IDFLD.NE.'WNS' .AND.                  &
  960            IDFLD.NE.'ICE' .AND. IDFLD.NE.'ISI'  )    GOTO 802
  961 !
  962 ! Set internal variables --------------------------------------------- *
  963 !
  964       WRITE  = INXOUT .EQ. 'WRITE'
  965       FL2D   = IDFLD.EQ.'CUR' .OR. IDFLD.EQ.'WND' .OR. IDFLD.EQ.'WNS' &
  966                               .OR. IDFLD.EQ.'ISI'
  967       FLBE   = IDFLD.EQ.'ISI'
  968       FLST   = IDFLD.EQ.'WNS'
  969                              
  970       IF ( .NOT. PRESENT(FLAGSC) ) THEN
  971         FLCOUPL=FLAGSC_DEFAULT
  972       ELSE
  973         FLCOUPL=FLAGSC
  974       END IF
  975             
  976 ! this flag is necessary to define the field at the start and end time
  977 ! of integration for the first time step which is integrated on 0
  978 ! to be able to output integrated variables like cha, ust, taw
  979                                                               
  980       FLINTERP = IDFLD.EQ.'CUR' .OR. IDFLD.EQ.'WND' .OR. IDFLD.EQ.'WNS'
  981                                                                        
  982 ! if the model is coupled, no interpolation in time must be done
  983                                                                 
  984       IF (FLCOUPL) FLINTERP = .FALSE.
  985                                      
  986       FLFRST = TFN(1) .EQ. -1
  987 !
  988 ! Loop over times / fields ========================================== *
  989 !
  990       DO
 
 
                                                         w3fldsmd.F90  page  19
 
 
  991 !
  992 ! Shift fields (interpolated fields only)
  993 !
  994         IF ( (.NOT.WRITE) .AND. FLINTERP ) THEN
  995 !
  996             TF0(1) = TFN(1)
  997             TF0(2) = TFN(2)
  998 ! unless TFN has been changed in the do loop, the following line is essen>
  999 !       "if not.flfrst"
 1000             IF ( TFN(1) .NE. -1 ) THEN
 1001                 DO IX=1, NX
 1002                   DO IY=1, NY
 1003                     FX0(IX,IY) = FXN(IX,IY)
 1004                     FY0(IX,IY) = FYN(IX,IY)
 1005                     END DO
 1006                   IF( FLST ) THEN
 1007                       DO IY=1, NY
 1008                         FA0(IX,IY) = FAN(IX,IY)
 1009                         END DO
 1010                     END IF
 1011                   END DO
 1012               END IF
 1013 !
 1014           END IF
 1015                 
 1016 !
 1017 ! Process fields, write --------------------------------------------- *
 1018 !
 1019         IF ( WRITE ) THEN
 1020 !
 1021             WRITE (NDS,ERR=803,IOSTAT=ISTAT) TF0
 1022             IF ( .NOT. FL2D ) THEN
 1023                 J      = 1
 1024                 WRITE (NDS,ERR=804,IOSTAT=ISTAT)                      &
 1025                            ((FA0(IX,IY),IX=1,NX),IY=1,NY)
 1026               ELSE
 1027                 J      = 1
 1028                 WRITE (NDS,ERR=804,IOSTAT=ISTAT)                      &
 1029                            ((FX0(IX,IY),IX=1,NX),IY=1,NY)
 1030                 J      = 2
 1031                 WRITE (NDS,ERR=804,IOSTAT=ISTAT)                      &
 1032                            ((FY0(IX,IY),IX=1,NX),IY=1,NY)
 1033                 J      = 3
 1034                 IF ( FLST ) WRITE (NDS,ERR=804,IOSTAT=ISTAT)          &
 1035                            ((FA0(IX,IY),IX=1,NX),IY=1,NY)
 1036               END IF
 1037 !
 1038             EXIT
 1039 !
 1040 ! Process fields, read ---------------------------------------------- *
 1041 !
 1042           ELSE
 1043 !
 1044  
 1045               READ (NDS,END=800,ERR=805,IOSTAT=ISTAT) TFN
 
 
                                                         w3fldsmd.F90  page  20
 
 
 1046               IF ( .NOT. FL2D ) THEN
 1047 ! note: "J" here does *not* refer to data type, wlev etc.
 1048 !       It refers to the dimension.
 1049                 J      = 1
 1050                 READ (NDS,END=806,ERR=807,IOSTAT=ISTAT)               &
 1051                            ((FAN(IX,IY),IX=1,NX),IY=1,NY)
 1052               ELSE
 1053                 J      = 1
 1054                 READ (NDS,END=806,ERR=807,IOSTAT=ISTAT)               &
 1055                            ((FXN(IX,IY),IX=1,NX),IY=1,NY)
 1056                 J      = 2
 1057                 READ (NDS,END=806,ERR=807,IOSTAT=ISTAT)               &
 1058                            ((FYN(IX,IY),IX=1,NX),IY=1,NY)
 1059                                                          
 1060 ! this was added for ISI files to store ICE in FAN and BERG in FYN
 1061                                                                   
 1062                 IF (FLBE) FAN(:,:) = FXN(:,:)
 1063                                              
 1064 ! this was added for WNS files to store WND in FXN & FYN and AST in FAN
 1065                                                                        
 1066                 J      = 3
 1067                 IF ( FLST ) READ (NDS,END=806,ERR=807,IOSTAT=ISTAT)   &
 1068                            ((FAN(IX,IY),IX=1,NX),IY=1,NY)
 1069               END IF
 1070 !
 1071 ! Check time, branch back if necessary
 1072 !
 1073             DTTST  = DSEC21 ( T0 , TFN )
 1074                                         
 1075 ! Exit if the time is the first time and the field is not interpolated in>
 1076                                                                           
 1077             IF ( .NOT.FLINTERP .AND. FLFRST .AND. DTTST .EQ. 0. ) EXIT
 1078                                                                       
 1079 ! Exit if the time of the input field is larger than the current time
 1080                                                                      
 1081             IF ( DTTST .GT. 0. ) EXIT
 1082 !
 1083           END IF
 1084 !
 1085         END DO
 1086 !
 1087 ! Branch point for EOF and interpolated fields (forcing current, wind or >
 1088 !
 1089   300 CONTINUE
 1090               
 1091 ! If the field is interpolated in time and the start time of interpolatio>
 1092 ! save the time and field values at the start time and field of interpola>
 1093                                                                           
 1094       IF ( .NOT.WRITE .AND. FLINTERP .AND. TF0(1) .EQ. -1 ) THEN
 1095 !
 1096           TF0(1) = T0(1)
 1097           TF0(2) = T0(2)
 1098 !
 1099           DO IX=1, NX
 1100             DO IY=1, NY
 
 
                                                         w3fldsmd.F90  page  21
 
 
 1101               FX0(IX,IY) = FXN(IX,IY)
 1102               FY0(IX,IY) = FYN(IX,IY)
 1103               END DO
 1104             IF( FLST ) THEN
 1105                 DO IY=1, NY
 1106                   FA0(IX,IY) = FAN(IX,IY)
 1107                   END DO
 1108               END IF
 1109             END DO
 1110 !
 1111         END IF
 1112 !
 1113 ! Branch point for EOF and not interpolated fields (coupled fields, ice, >
 1114 !
 1115   500 CONTINUE
 1116 !
 1117 ! Process fields, end ----------------------------------------------- *
 1118 !
 1119       RETURN
 1120 !
 1121 ! EOF escape location (have read to end of file)
 1122 !
 1123   800 CONTINUE
 1124       IERR   = -1
 1125 !
 1126       IF ( FLINTERP ) THEN
 1127           TFN(1) = TN(1)
 1128           TFN(2) = TN(2)
 1129           CALL TICK21 ( TFN , 1. )
 1130         END IF
 1131 !
 1132       IF ( FLINTERP ) THEN
 1133           GOTO 300
 1134         ELSE
 1135           GOTO 500
 1136         END IF
 1137 !
 1138 ! Error escape locations
 1139 !
 1140   801 CONTINUE
 1141       IF ( NDSE .GE. 0 ) WRITE (NDSE,1001) INXOUT
 1142       IERR   = 1
 1143       RETURN
 1144 !
 1145   802 CONTINUE
 1146       IF ( NDSE .GE. 0 ) WRITE (NDSE,1002) IDFLD
 1147       IERR   = 2
 1148       RETURN
 1149 !
 1150   803 CONTINUE
 1151       IF ( NDSE .GE. 0 ) WRITE (NDSE,1003) ISTAT
 1152       IERR   = 3
 1153       RETURN
 1154 !
 1155   804 CONTINUE
 
 
                                                         w3fldsmd.F90  page  22
 
 
 1156       IF ( NDSE .GE. 0 ) WRITE (NDSE,1004) J, ISTAT
 1157       IERR   = 4
 1158       RETURN
 1159 !
 1160   805 CONTINUE
 1161       IF ( NDSE .GE. 0 ) WRITE (NDSE,1005) ISTAT
 1162       IERR   = 5
 1163       RETURN
 1164 !
 1165   806 CONTINUE
 1166       IF ( NDSE .GE. 0 ) WRITE (NDSE,1006) J, ISTAT
 1167       IERR   = 6
 1168       RETURN
 1169 !
 1170   807 CONTINUE
 1171       IF ( NDSE .GE. 0 ) WRITE (NDSE,1007) J, ISTAT
 1172       IERR   = 7
 1173       RETURN
 1174 !
 1175 ! Formats
 1176 !
 1177  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDG : '/               &
 1178                '     ILLEGAL INXOUT STRING : ',A/)
 1179  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDG : '/               &
 1180                '     ILLEGAL FIELD ID STRING : ',A/)
 1181  1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDG : '/               &
 1182                '     ERROR IN WRITING TIME, IOSTAT =',I6/)
 1183  1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDG : '/               &
 1184                '     ERROR IN WRITING FIELD ',I1,', IOSTAT =',I6/)
 1185  1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDG : '/               &
 1186                '     ERROR IN READING TIME, IOSTAT =',I6/)
 1187  1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDG : '/               &
 1188                '     PRMATURE EOF READING FIELD ',I1,', IOSTAT =',I6/)
 1189  1007 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDG : '/               &
 1190                '     ERROR IN READING FIELD ',I1,', IOSTAT =',I6/)
 1191 !
 1192 !/
 1193 !/ End of W3FLDG ----------------------------------------------------- /
 1194 !/
 1195       END SUBROUTINE W3FLDG
 1196 !/ ------------------------------------------------------------------- /
 1197       SUBROUTINE W3FLDD (INXOUT, IDFLD, NDS, NDST, NDSE, TIME, TD,    &
 1198                          NR, ND, NDOUT, DATA, IERR )
 1199 !/
 1200 !/                  +-----------------------------------+
 1201 !/                  | WAVEWATCH III           NOAA/NCEP |
 1202 !/                  |           H. L. Tolman            |
 1203 !/                  |                        FORTRAN 90 |
 1204 !/                  | Last update :         26-Dec-2012 |
 1205 !/                  +-----------------------------------+
 1206 !/
 1207 !/    24-Jan-2002 : Origination.                        ( version 2.17 )
 1208 !/    26-Dec-2012 : Modified obsolete declarations.     ( version 4.11 )
 1209 !/
 1210 !  1. Purpose :
 
 
                                                         w3fldsmd.F90  page  23
 
 
 1211 !
 1212 !     Update assimilation data in the WAVEWATCH III generic shell from
 1213 !     a WAVEWATCH III shell data file or write from preprocessor.
 1214 !
 1215 !  2. Method :
 1216 !
 1217 !     Read from file opened by W3FLDO.
 1218 !
 1219 !  3. Parameters :
 1220 !
 1221 !     Parameter list
 1222 !     ----------------------------------------------------------------
 1223 !       INXOUT  C*(*)  I   Test string for read/write, valid are:
 1224 !                           'WRITE'  Write a data field to file.
 1225 !                           'SIZE'   Get the number of records of
 1226 !                                    next data set.
 1227 !                           'READ'   Read the data set found by
 1228 !                                    'SIZE' after allocating proper
 1229 !                                    data array.
 1230 !       IDFLD   C*3    I   ID string for field type, valid are:
 1231 !                          'DT0', 'DT1', and 'DT2'.
 1232 !       NDS     Int.   I   Dataset number for fields file.
 1233 !       NDST    Int.   I   Dataset number for test output.
 1234 !       NDSE    Int.   I   Dataset number for error output.
 1235 !                          (No error output if NDSE < 0 ).
 1236 !       TIME    I.A.   I   Minimum time for data.
 1237 !       TD      I.A.  I/O  Data time.
 1238 !       NR,ND   Int.   I   Array dimensions.
 1239 !       NDOUT   Int.   O   Number of data to be read next.
 1240 !       DATA    R.A.  I/O  Data array.
 1241 !       IERR    Int.   O   Error indicator,
 1242 !                          -1 Past last data
 1243 !                           0 OK,
 1244 !                           1 : Illegal INXOUT.
 1245 !                           2 : Illegal IDFLD.
 1246 !                           3 : Error in writing time.
 1247 !                           4 : Error in writing data.
 1248 !                           5 : Error in reading time.
 1249 !                           6 : Premature EOF reading data.
 1250 !                           7 : Error reading data.
 1251 !     ----------------------------------------------------------------
 1252 !
 1253 !  4. Subroutines used :
 1254 !
 1255 !      Name      Type  Module   Description
 1256 !     ----------------------------------------------------------------
 1257 !      STRACE    Subr.   Id.    Subroutine tracing.
 1258 !      TICK21    Subr. W3TIMEMD Advance time.
 1259 !      DSEC21    Func.   Id.    Difference between times.
 1260 !     ----------------------------------------------------------------
 1261 !
 1262 !  5. Called by :
 1263 !
 1264 !      Name      Type  Module   Description
 1265 !     ----------------------------------------------------------------
 
 
                                                         w3fldsmd.F90  page  24
 
 
 1266 !      WW3_PREP  Prog.   N/A    Input data preprocessor.
 1267 !      WW3_SHEL  Prog.   N/A    Basic wave model driver.
 1268 !      ......    Prog.   N/A    Any other program that reads or
 1269 !                               writes WAVEWATCH III data files.
 1270 !     ----------------------------------------------------------------
 1271 !
 1272 !  6. Error messages :
 1273 !
 1274 !     See end of subroutine.
 1275 !
 1276 !  7. Remarks :
 1277 !
 1278 !  8. Structure :
 1279 !
 1280 !     See source code.
 1281 !
 1282 !  9. Switches :
 1283 !
 1284 !     !/S  Enable subroutine tracing.
 1285 !     !/T  Enable test output.
 1286 !
 1287 ! 10. Source code :
 1288 !
 1289 !/ ------------------------------------------------------------------- /
 1290 !/
 1291       USE W3TIMEMD
 1292 !
 1293       IMPLICIT NONE
 1294 !/
 1295 !/ ------------------------------------------------------------------- /
 1296 !/ Parameter list
 1297 !/
 1298       INTEGER, INTENT(IN)          :: NDS, NDST, NDSE, TIME(2), NR, ND
 1299       INTEGER, INTENT(INOUT)       :: TD(2), NDOUT
 1300       INTEGER, INTENT(OUT)         :: IERR
 1301       REAL, INTENT(INOUT)          :: DATA(NR,ND)
 1302       CHARACTER, INTENT(IN)        :: INXOUT*(*)
 1303       CHARACTER(LEN=3), INTENT(IN) :: IDFLD
 1304 !/
 1305 !/ ------------------------------------------------------------------- /
 1306 !/ Local parameters
 1307 !/
 1308       INTEGER                 :: ISTAT, NRT
 1309       REAL                    :: DTTST
 1310       LOGICAL                 :: WRITE, SIZE
 1311 !/
 1312 !/ ------------------------------------------------------------------- /
 1313 !/
 1314 !/
 1315       IERR   = 0
 1316 !
 1317 ! test input parameters ---------------------------------------------- *
 1318 !
 1319       IF ( INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE' .AND.             &
 1320            INXOUT.NE.'SIZE' ) GOTO 801
 
 
                                                         w3fldsmd.F90  page  25
 
 
 1321       IF ( IDFLD.NE.'DT0' .AND. IDFLD.NE.'DT1' .AND.                  &
 1322            IDFLD.NE.'DT2' )    GOTO 802
 1323 !
 1324 ! Set internal variables --------------------------------------------- *
 1325 !
 1326       WRITE  = INXOUT .EQ. 'WRITE'
 1327       SIZE   = INXOUT .EQ. 'SIZE'
 1328 !
 1329 ! Process fields, write --------------------------------------------- *
 1330 !
 1331       IF ( WRITE ) THEN
 1332 !
 1333           WRITE (NDS,ERR=803,IOSTAT=ISTAT) TD, ND
 1334           WRITE (NDS,ERR=804,IOSTAT=ISTAT) DATA
 1335 !
 1336 ! Process fields, read size ----------------------------------------- *
 1337 !
 1338         ELSE IF ( SIZE ) THEN
 1339 !
 1340   100     CONTINUE
 1341           READ (NDS,END=800,ERR=805,IOSTAT=ISTAT) TD, NDOUT
 1342 !
 1343 ! Check time, read and branch back if necessary
 1344 !
 1345           DTTST  = DSEC21 ( TIME , TD )
 1346           IF ( DTTST.LT.0. .OR. NDOUT.EQ.0 ) THEN
 1347               IF (NDOUT.GT.0) READ (NDS,END=806,ERR=807,IOSTAT=ISTAT)
 1348               GOTO 100
 1349            END IF
 1350 !
 1351 ! Process fields, read data ----------------------------------------- *
 1352 !
 1353         ELSE
 1354 !
 1355           READ (NDS,END=806,ERR=807,IOSTAT=ISTAT) DATA
 1356         END IF
 1357 !
 1358 ! Process fields, end ----------------------------------------------- *
 1359 !
 1360       RETURN
 1361 !
 1362 ! EOF escape location
 1363 !
 1364   800 CONTINUE
 1365       IERR   = -1
 1366       RETURN
 1367 !
 1368 ! Error escape locations
 1369 !
 1370   801 CONTINUE
 1371       IF ( NDSE .GE. 0 ) WRITE (NDSE,1001) INXOUT
 1372       IERR   = 1
 1373       RETURN
 1374 !
 1375   802 CONTINUE
 
 
                                                         w3fldsmd.F90  page  26
 
 
 1376       IF ( NDSE .GE. 0 ) WRITE (NDSE,1002) IDFLD
 1377       IERR   = 2
 1378       RETURN
 1379 !
 1380   803 CONTINUE
 1381       IF ( NDSE .GE. 0 ) WRITE (NDSE,1003) ISTAT
 1382       IERR   = 3
 1383       RETURN
 1384 !
 1385   804 CONTINUE
 1386       IF ( NDSE .GE. 0 ) WRITE (NDSE,1004) ISTAT
 1387       IERR   = 4
 1388       RETURN
 1389 !
 1390   805 CONTINUE
 1391       IF ( NDSE .GE. 0 ) WRITE (NDSE,1005) ISTAT
 1392       IERR   = 5
 1393       RETURN
 1394 !
 1395   806 CONTINUE
 1396       IF ( NDSE .GE. 0 ) WRITE (NDSE,1006) ISTAT
 1397       IERR   = 6
 1398       RETURN
 1399 !
 1400   807 CONTINUE
 1401       IF ( NDSE .GE. 0 ) WRITE (NDSE,1007) ISTAT
 1402       IERR   = 7
 1403       RETURN
 1404 !
 1405 ! Formats
 1406 !
 1407  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDD : '/               &
 1408                '     ILLEGAL INXOUT STRING : ',A/)
 1409  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDD : '/               &
 1410                '     ILLEGAL FIELD ID STRING : ',A/)
 1411  1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDD : '/               &
 1412                '     ERROR IN WRITING TIME, IOSTAT =',I6/)
 1413  1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDD : '/               &
 1414                '     ERROR IN WRITING DATA, IOSTAT =',I6/)
 1415  1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDD : '/               &
 1416                '     ERROR IN READING TIME, IOSTAT =',I6/)
 1417  1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDD : '/               &
 1418                '     PRMATURE EOF READING DATA, IOSTAT =',I6/)
 1419  1007 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDD : '/               &
 1420                '     ERROR IN READING DATA, IOSTAT =',I6/)
 1421 !
 1422 !/
 1423 !/ End of W3FLDD ----------------------------------------------------- /
 1424 !/
 1425       END SUBROUTINE W3FLDD
 1426 !/ ------------------------------------------------------------------- /
 1427       SUBROUTINE W3FLDP ( NDSM, NDST, NDSE, IERR, FLAGLL,             &
 1428                           MX, MY, NX, NY,                             &
 1429                           TLAT, TLON,     MAPOVR, ILAND, MXI, MYI,    &
 1430                           NXI, NYI, CLOSED, ALAT, ALON, MASK,         &
 
 
                                                         w3fldsmd.F90  page  27
 
 
 1431                           RD11, RD21, RD12, RD22, IX1, IX2, IY1, IY2 )
 1432 !/
 1433 !/                  +-----------------------------------+
 1434 !/                  | WAVEWATCH III           NOAA/NCEP |
 1435 !/                  |           H. L. Tolman            |
 1436 !/                  |                        FORTRAN 90 |
 1437 !/                  | Last update :         30-Oct-2009 |
 1438 !/                  +-----------------------------------+
 1439 !/
 1440 !/    08-Feb-1999 : Final FORTRAN 77                    ( version 1.18 )
 1441 !/    30-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
 1442 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
 1443 !/                  (W. E. Rogers & T. J. Campbell, NRL)
 1444 !/    20-Jan-2017 : Update to new W3GSRUMD APIs         ( version 6.02 )
 1445 !/
 1446 !  1. Purpose :
 1447 !
 1448 !     General purpose routine for interpolating data of an irregular
 1449 !     grid given by ALAT and ALON to a target grid given by TLAT and TLON.
 1450 !
 1451 !  2. Method :
 1452 !
 1453 !     Use the grid search and remapping utilities (W3GSRUMD).
 1454 !     Bi-linear interpolation.
 1455 !
 1456 !  3. Parameters :
 1457 !
 1458 !     Parameter list
 1459 !     ----------------------------------------------------------------
 1460 !       NDSM    Int.  I  Unit number message  output (disabled if 0).
 1461 !       NDST    Int.  I  Unit number test output.
 1462 !       NDSE    Int.  I  Unit number error output.
 1463 !       IERR    Int.  O  Error indicator (number of lost points due
 1464 !                        to ap conflicts).
 1465 !       FLAGLL  Log.  I  Coordinate system flag (T=Lat/Lon, F=Cartesian)
 1466 !       MX,MY   Int.  I  Array dimensions for output type arrays.
 1467 !       NX,NY   Int.  I  Id. actual field syze.
 1468 !       TLAT    R.A.  I  Y-coordinates of output grid.
 1469 !       TLON    R.A.  I  X-coordinates of output grid.
 1470 !       MAPOVR  I.A. I/O Overlay map, the value of a grid point is
 1471 !                        incremeted by 1 of the corresponding grid
 1472 !                        point of the output grid is covered by the
 1473 !                        input grid. Land points are masked out by
 1474 !                        setting them to ILAND.
 1475 !       ILAND   Int.  I  Value for land points in MAPOVR (typically<0)
 1476 !       MXI,MYI Int.  I  Array dimensions for input fields.
 1477 !       NXI,NYI Int.  I  Id. actual field sizes.
 1478 !       CLOSED  Log.  I  Flag for closed longitude range in input.
 1479 !       ALAT    R.A.  I  Y-coordinates of input grid.
 1480 !       ALON    R.A. I/O X-coordinates of input grid.
 1481 !                        (will be modified if CLOSED)
 1482 !       MASK    I.A.  I  Land-sea mask for input field (0=land).
 1483 !       RDnn    R.A.  O  Interpolation factors (see below).
 1484 !       IXn,IYn I.A.  O  Interpolation addresses (see below).
 1485 !     ----------------------------------------------------------------
 
 
                                                         w3fldsmd.F90  page  28
 
 
 1486 !
 1487 !                             RD12|          |RD22
 1488 !                     IY2       --+----------+--
 1489 !                                 |          |
 1490 !                                 |          |
 1491 !                                 |          |
 1492 !                                 |          |
 1493 !                     IY1       --+----------+--
 1494 !                             RD11|          |RD21
 1495 !
 1496 !                                IX1        IX2
 1497 !
 1498 !     Internal parameters
 1499 !     ----------------------------------------------------------------
 1500 !     ----------------------------------------------------------------
 1501 !
 1502 !  4. Subroutines used :
 1503 !
 1504 !      Name      Type  Module   Description
 1505 !     ----------------------------------------------------------------
 1506 !      STRACE    Subr.   Id.    Subroutine tracing.
 1507 !      TICK21    Subr. W3TIMEMD Advance time.
 1508 !      DSEC21    Func.   Id.    Difference between times.
 1509 !      W3GSUC    Func. W3GSRUMD Create grid-search-utility object
 1510 !      W3GSUD    Subr. W3GSRUMD Destroy grid-search-utility object
 1511 !      W3GRMP    Func. W3GSRUMD Compute interpolation weights
 1512 !     ----------------------------------------------------------------
 1513 !
 1514 !  5. Called by :
 1515 !
 1516 !      Name      Type  Module   Description
 1517 !     ----------------------------------------------------------------
 1518 !      WW3_PREP  Prog.   N/A    Input data preprocessor.
 1519 !      ......    Prog.   N/A    Any other program that reads or
 1520 !                               writes WAVEWATCH III data files.
 1521 !     ----------------------------------------------------------------
 1522 !
 1523 !  6. Error messages :
 1524 !
 1525 !  7. Remarks :
 1526 !
 1527 !     - Land points in the input grid are taken out of the interp.
 1528 !       algorithm. If this results in zero weight factors through the
 1529 !       interpolation box in the input grid, the closest 2 sea point
 1530 !       for an extended 4x4 grid are used for interpolation, weighted
 1531 !       by the inverse distance.
 1532 !     - The "CLOSED" variable comes from ww3_prep.inp and is associated
 1533 !       with the input grid (e.g. grid that winds are provided on).
 1534 !       It is a logical, not an integer, so it only allows two cases:
 1535 !       no closure, or simple closure. "ww3_prep" only supports these
 1536 !       two (not tripole).
 1537 !
 1538 !  8. Structure :
 1539 !
 1540 !     -----------------------------------------------------------------
 
 
                                                         w3fldsmd.F90  page  29
 
 
 1541 !      1.  Initializations.
 1542 !        a Initialize counters and factors.
 1543 !        b Setup logical mask
 1544 !        c Create grid-search-utility object
 1545 !      2.  Loop over output grid
 1546 !        a Check if sea point
 1547 !        b Find enclosing cell and compute interpolation weights using
 1548 !          W3GRMP
 1549 !        c Non-masked or partially masked cell
 1550 !        d Fully masked cell
 1551 !        e Update overlay map
 1552 !      2.  Finalizations.
 1553 !        a Final output
 1554 !        b Destroy grid-search-utility object
 1555 !     -----------------------------------------------------------------
 1556 !
 1557 !  9. Switches :
 1558 !
 1559 !     !/S   Enable subroutine tracing.
 1560 !
 1561 !     !/T   Enable limited test output.
 1562 !     !/T1  Enable full debugging in W3GRMP
 1563 !
 1564 ! 10. Source code :
 1565 !
 1566 !/ ------------------------------------------------------------------- /
 1567 !/
 1568       USE W3GSRUMD
 1569 !
 1570       IMPLICIT NONE
 1571 !/
 1572 !/ ------------------------------------------------------------------- /
 1573 !/ Parameter list
 1574 !/
 1575       INTEGER, INTENT(IN)     :: NDSM, NDST, NDSE, MX, MY, NX, NY,    &
 1576                                  MXI, MYI, NXI, NYI, MASK(MXI,MYI)
 1577       INTEGER, INTENT(INOUT)  :: MAPOVR(MX,MY), ILAND
 1578       INTEGER, INTENT(OUT)    :: IERR, IX1(MX,MY), IX2(MX,MY),        &
 1579                                        IY1(MX,MY), IY2(MX,MY)
 1580       REAL, INTENT(IN)        :: TLAT(MY,MX), TLON(MY,MX)
 1581       REAL, INTENT(IN)   ,TARGET :: ALAT(MXI,MYI)
 1582       REAL, INTENT(INOUT),TARGET :: ALON(MXI,MYI)
 1583       REAL, INTENT(OUT)       :: RD11(MX,MY), RD12(MX,MY),            &
 1584                                  RD21(MX,MY), RD22(MX,MY)
 1585       LOGICAL, INTENT(IN)     :: FLAGLL, CLOSED
 1586 !/
 1587 !/ ------------------------------------------------------------------- /
 1588 !/ Local parameters
 1589 !/
 1590       TYPE(T_GSU)             :: GSU
 1591       INTEGER                 :: IX, IY, I, J, NNBR, II(4), JJ(4),    &
 1592                                  MSKC, IFOUND, IMASK, ICOR1
 1593       REAL                    :: RR(4), X, Y
 1594       REAL, POINTER           :: PLAT(:,:), PLON(:,:)
 1595       LOGICAL                 :: INGRID, LMSK(MXI,MYI)
 
 
                                                         w3fldsmd.F90  page  30
 
 
 1596       LOGICAL                 :: LDBG = .FALSE.
 1597       INTEGER, PARAMETER      :: NNBR_MAX = 2
 1598       INTEGER                 :: ICLO
 1599 !/
 1600 !/ ------------------------------------------------------------------- /
 1601 !/
 1602 !
 1603 ! 1.  Initializations ------------------------------------------------ *
 1604 ! 1.a Initialize counters and factors
 1605 !
 1606       IERR   = 0
 1607       IFOUND = 0
 1608       IMASK  = 0
 1609       ICOR1  = 0
 1610       ICLO   = ICLO_NONE
 1611       IF ( FLAGLL .AND. CLOSED ) ICLO = ICLO_SMPL
 1612 !
 1613       DO 110, IX=1, NX
 1614         DO 100, IY=1, NY
 1615           RD11(IX,IY) = 0.
 1616           RD12(IX,IY) = 0.
 1617           RD21(IX,IY) = 0.
 1618           RD22(IX,IY) = 0.
 1619           IX1(IX,IY)  = 1
 1620           IX2(IX,IY)  = 1
 1621           IY1(IX,IY)  = 1
 1622           IY2(IX,IY)  = 1
 1623   100     CONTINUE
 1624   110   CONTINUE
 1625 !
 1626 ! 1.b Setup logical mask
 1627 !
 1628       LMSK = MASK .EQ. 0
 1629 !
 1630 ! 1.c Create grid-search-utility object for input grid
 1631 !
 1632       PLAT => ALAT
 1633       PLON => ALON
 1634       GSU = W3GSUC( .TRUE., FLAGLL, ICLO, PLON, PLAT )
 1635 !
 1636 ! 2.  Loop over output grid ------------------------------------------ *
 1637 !
 1638       DO 500, IY=1, NY
 1639         DO 400, IX=1, NX
 1640 !
 1641           X = TLON(IY,IX)
 1642           Y = TLAT(IY,IX)
 1643 !
 1644 ! 2.a Check if sea point
 1645 !
 1646           IF ( MAPOVR(IX,IY) .NE. ILAND ) THEN
 1647 !
 1648 ! 2.b Find enclosing cell and compute interpolation weights
 1649 !
 1650               NNBR = NNBR_MAX
 
 
                                                         w3fldsmd.F90  page  31
 
 
 1651               INGRID = W3GRMP( GSU, X, Y, II, JJ, RR, &
 1652                        MASK=LMSK, MSKC=MSKC, NNBR=NNBR, DEBUG=LDBG )
 1653 !
 1654               IF ( INGRID ) THEN
 1655 !
 1656 ! 2.c Non-masked or partially masked cell: simply store the weights
 1657 !
 1658                   IF ( MSKC.EQ.MSKC_NONE .OR. MSKC.EQ.MSKC_PART ) THEN
 1659 !
 1660                       IF ( MSKC.EQ.MSKC_PART ) IMASK  = IMASK + 1
 1661 !
 1662 ! ..... Here we switch from counter-clockwise order to column-major
 1663                       IX1 (IX,IY) = II(1)
 1664                       IX2 (IX,IY) = II(2)
 1665                       IY1 (IX,IY) = JJ(1)
 1666                       IY2 (IX,IY) = JJ(4)
 1667                       RD11(IX,IY) = RR(1)
 1668                       RD21(IX,IY) = RR(2)
 1669                       RD12(IX,IY) = RR(4)
 1670                       RD22(IX,IY) = RR(3)
 1671 !
 1672 ! 2.d Fully masked cell
 1673 !
 1674                     ELSE !MSKC.EQ.MSKC_FULL
 1675 !
 1676                       IMASK  = IMASK + 1
 1677 !
 1678                       IF ( NNBR .GT. 0 ) THEN
 1679                           ICOR1  = ICOR1 + 1
 1680                           IX1 (IX,IY) = II(1)
 1681                           IY1 (IX,IY) = JJ(1)
 1682                           RD11(IX,IY) = RR(1)
 1683                           IF ( NNBR .GT. 1 ) THEN
 1684                               IX1 (IX,IY) = II(2)
 1685                               IY1 (IX,IY) = JJ(2)
 1686                               RD22(IX,IY) = RR(2)
 1687                             END IF
 1688                         ELSE
 1689                           IERR   = IERR + 1
 1690                           WRITE (NDSE,910) IX, IY, X, Y,    &
 1691                                            II(1), II(2), JJ(1), JJ(2)
 1692                         END IF ! NNBR
 1693 !
 1694                     END IF ! MSKC
 1695 !
 1696 ! 2.e Update overlay map
 1697 !
 1698                   MAPOVR(IX,IY) = MAPOVR(IX,IY) + 1
 1699                   IFOUND  = IFOUND + 1
 1700 !
 1701                 END IF ! INGRID
 1702             ENDIF ! sea-point
 1703 !
 1704 ! ... End loop over output grid -------------------------------------- *
 1705 !
 
 
                                                         w3fldsmd.F90  page  32
 
 
 1706   400     CONTINUE
 1707   500   CONTINUE
 1708 !
 1709 ! 3.  Finalizations -------------------------------------------------- *
 1710 ! 3.a Final output
 1711 !
 1712       IF (NDSM.NE.0) WRITE (NDSM,900) IFOUND, IMASK, ICOR1, IERR
 1713 !
 1714 ! 3.b Destroy grid-search-utility object
 1715 !
 1716       CALL W3GSUD(GSU)
 1717 !
 1718       RETURN
 1719 !
 1720 ! Formats
 1721 !
 1722   900 FORMAT (/' *** MESSAGE W3FLDP: FINAL SEA POINT COUNT     :',I8/ &
 1723                '                     INTERPOLATION ACROSS SHORE:',I8/ &
 1724                '                     CORRECTED COASTAL POINTS  :',I8/ &
 1725                '                     UNCORRECTABLE C. POINTS   :',I8/)
 1726 !
 1727   910 FORMAT ( ' *** WARNING W3FLDP : SEA POINT ON LAND MASK ', &
 1728                     '(COULD NOT BE CORRECTED)'/                 &
 1729                '     COORDINATES IN OUTPUT GRID :',2I4,2F8.2/   &
 1730                '     X-COUNTERS IN INPUT GRID   :',2I4/         &
 1731                '     Y-COUNTERS IN INPUT GRID   :',2I4)
 1732 !
 1733 !/
 1734 !/ End of W3FLDP ----------------------------------------------------- /
 1735 !/
 1736       END SUBROUTINE W3FLDP
 1737 !/ ------------------------------------------------------------------- /
 1738       SUBROUTINE W3FLDH (J, NDST, NDSE, MX, MY, NX, NY, T0, TN,       &
 1739                          NH, NHM, THO, HA, HD, HS, TF0, FX0, FY0, FS0,&
 1740                          TFN, FXN, FYN, FSN, IERR)
 1741 !/
 1742 !/                  +-----------------------------------+
 1743 !/                  | WAVEWATCH III           NOAA/NCEP |
 1744 !/                  |           H. L. Tolman            |
 1745 !/                  |                        FORTRAN 90 |
 1746 !/                  | Last update :         05-Jul-2005 |
 1747 !/                  +-----------------------------------+
 1748 !/
 1749 !/    15-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
 1750 !/    30-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
 1751 !/    04-Sep-2003 : Bug fix par. list declaration.      ( version 3.04 )
 1752 !/    05-Jul-2005 : Correct first level/ice.            ( version 3.07 )
 1753 !/    15-May-2018 : Allow homog ice.                    ( version 6.05 )
 1754 !/
 1755 !  1. Purpose :
 1756 !
 1757 !     Update homogeneous input fields for the WAVEWATCH III generic
 1758 !     shell.
 1759 !
 1760 !  2. Method :
 
 
                                                         w3fldsmd.F90  page  33
 
 
 1761 !
 1762 !     Variables defining the homogeneous fields are transfered through
 1763 !     the parameter list (see section 3).
 1764 !
 1765 !  3. Parameters :
 1766 !
 1767 !     Parameter list
 1768 !     ----------------------------------------------------------------
 1769 !       J       Int    I   Field number of input field as in shell.
 1770 !                          -7 : ice parameter 1
 1771 !                          -6 : ice parameter 2
 1772 !                          -5 : ice parameter 3
 1773 !                          -4 : ice parameter 4
 1774 !                          -3 : ice parameter 5
 1775 !                          -2 : mud parameter 1
 1776 !                          -1 : mud parameter 2
 1777 !                           0 : mud parameter 3
 1778 !                           1 : water levels
 1779 !                           2 : currents
 1780 !                           3 : winds
 1781 !                           4 : ice
 1782 !                           8 : moving grid
 1783 !       NDST    Int.   I   Unit number test output.
 1784 !       NDSE    Int.   I   Unit number error messages.
 1785 !                          (No output if NDSE < 0).
 1786 !       MX,MY   Int.   I   Array dimensions output fields.
 1787 !       NX,NY   Int.   I   Field dimensions output fields.
 1788 !       T0-N    I.A.   I   Time interval considered.
 1789 !       NH      Int.  I/O  Number of homogeneous fields J.
 1790 !       NHM     Int.   I   Array dimension corresponding to NH.
 1791 !       THO     I.A.  I/O  Times for all homogeneous fields left.
 1792 !       HA      R.A.  I/O  Id. amplitude.
 1793 !       HD      R.A.  I/O  Id. direction (degr., Naut.).
 1794 !       HS      R.A.  I/O  Id. air-sea temperature difference (degr.).
 1795 !       TF0-N   I.A.  I/O  Times of input fields
 1796 !       Fxx     R.A.  I/O  Input fields (X, Y, Scalar)
 1797 !       IERR    Int.   O   Error indicator,
 1798 !                           0 OK,
 1799 !                           1 Illegal field number
 1800 !                          -1 Past last data
 1801 !     ----------------------------------------------------------------
 1802 !
 1803 !  4. Subroutines used :
 1804 !
 1805 !      Name      Type  Module   Description
 1806 !     ----------------------------------------------------------------
 1807 !      STRACE    Subr.   Id.    Subroutine tracing.
 1808 !      TICK21    Subr. W3TIMEMD Advance time.
 1809 !      DSEC21    Func.   Id.    Difference between times.
 1810 !     ----------------------------------------------------------------
 1811 !
 1812 !  5. Called by :
 1813 !
 1814 !      Name      Type  Module   Description
 1815 !     ----------------------------------------------------------------
 
 
                                                         w3fldsmd.F90  page  34
 
 
 1816 !      WW3_SHEL  Prog.   N/A    Basic wave model driver.
 1817 !     ----------------------------------------------------------------
 1818 !
 1819 !  6. Error messages :
 1820 !
 1821 !     - See end of subroutine.
 1822 !     - Array dimensions not checked.
 1823 !
 1824 !  7. Remarks :
 1825 !
 1826 !     - No homogeneous ice fields available.
 1827 !     - Previous fields needed only for 2-D fields.
 1828 !
 1829 !  8. Structure :
 1830 !
 1831 !       See source code.
 1832 !
 1833 !  9. Switches :
 1834 !
 1835 !     !/S  Enable subroutine tracing.
 1836 !     !/T  Enable test output.
 1837 !
 1838 ! 10. Source code :
 1839 !
 1840 !/ ------------------------------------------------------------------- /
 1841 !/
 1842       USE W3TIMEMD
 1843 !
 1844       IMPLICIT NONE
 1845 !/
 1846 !/ ------------------------------------------------------------------- /
 1847 !/ Parameter list
 1848 !/
 1849       INTEGER, INTENT(IN)     :: J, NDST, NDSE, MX, MY, NX, NY,       &
 1850                                  T0(2), TN(2), NHM
 1851       INTEGER, INTENT(INOUT)  :: NH, THO(2,-7:8,NHM), TF0(2), TFN(2)
 1852       INTEGER, INTENT(OUT)    :: IERR
 1853       REAL, INTENT(INOUT)     :: HA(NHM,-7:8), HD(NHM,-7:8), HS(NHM,-7:8)>
 1854                                  FX0(MX,MY), FY0(MX,MY), FS0(MX,MY),  &
 1855                                  FXN(MX,MY), FYN(MX,MY), FSN(MX,MY)
 1856 !/
 1857 !/ ------------------------------------------------------------------- /
 1858 !/ Local parameters
 1859 !/
 1860       INTEGER                 :: IX, IY, I
 1861       REAL                    :: X, Y, DIR, DTTST, DERA
 1862       LOGICAL                 :: FLFRST
 1863 !/
 1864 !/ ------------------------------------------------------------------- /
 1865 !/
 1866 !
 1867       IERR   = 0
 1868       DERA   = ATAN(1.)/45.
 1869                            
 1870                            
 
 
                                                         w3fldsmd.F90  page  35
 
 
 1871 !
 1872 ! Test field ID number for validity
 1873 !
 1874       IF ( J.LT.-7 .OR. J .GT.8 ) GOTO 801
 1875       FLFRST = TFN(1) .EQ. -1
 1876 !
 1877 ! Loop over times / fields ========================================== *
 1878 !
 1879       DO
 1880 !
 1881 ! Shift fields
 1882 !
 1883         TF0(1) = TFN(1)
 1884         TF0(2) = TFN(2)
 1885         IF ( TFN(1) .NE. -1 ) THEN
 1886             IF ( J .EQ. 2 ) THEN
 1887                 DO IX=1, NX
 1888                   DO IY=1, NY
 1889                     FX0(IX,IY) = FXN(IX,IY)
 1890                     FY0(IX,IY) = FYN(IX,IY)
 1891                     END DO
 1892                   END DO
 1893                ELSE IF ( J .EQ. 3 ) THEN
 1894                 DO IX=1, NX
 1895                   DO IY=1, NY
 1896                     FX0(IX,IY) = FXN(IX,IY)
 1897                     FY0(IX,IY) = FYN(IX,IY)
 1898                     FS0(IX,IY) = FSN(IX,IY)
 1899                     END DO
 1900                   END DO
 1901                END IF
 1902           END IF
 1903 !
 1904 ! New field
 1905 !
 1906         IF ( NH .NE. 0. ) THEN
 1907             TFN(1) = THO(1,J,1)
 1908             TFN(2) = THO(2,J,1)
 1909 ! ic* md* lev ice
 1910             IF ( ( J.LE.1 ) .OR. ( J.EQ.4) ) THEN
 1911                 DO IX=1, NX
 1912                   DO IY=1, NY
 1913                     FSN(IX,IY) = HA(1,J)
 1914                     END DO
 1915                   END DO
 1916               END IF
 1917 ! cur
 1918             IF ( J .EQ. 2 ) THEN
 1919                 DIR    = ( 270. - HD(1,J) ) * DERA
 1920                 X      = HA(1,J) * COS(DIR)
 1921                 Y      = HA(1,J) * SIN(DIR)
 1922                 DO IX=1, NX
 1923                   DO IY=1, NY
 1924                     FXN(IX,IY) = X
 1925                     FYN(IX,IY) = Y
 
 
                                                         w3fldsmd.F90  page  36
 
 
 1926                     END DO
 1927                   END DO
 1928               END IF
 1929 ! wnd
 1930             IF ( J .EQ. 3 ) THEN
 1931                 DIR    = ( 270. - HD(1,J) ) * DERA
 1932                 X      = HA(1,J) * COS(DIR)
 1933                 Y      = HA(1,J) * SIN(DIR)
 1934                 DO IX=1, NX
 1935                   DO IY=1, NY
 1936                     FXN(IX,IY) = X
 1937                     FYN(IX,IY) = Y
 1938                     FSN(IX,IY) = HS(1,J)
 1939                     END DO
 1940                   END DO
 1941               END IF
 1942 !
 1943 ! Shift data arrays
 1944 !
 1945             DO I=1, NH-1
 1946               THO(1,J,I) = THO(1,J,I+1)
 1947               THO(2,J,I) = THO(2,J,I+1)
 1948               HA(I,J)    = HA(I+1,J)
 1949               HD(I,J)    = HD(I+1,J)
 1950               HS(I,J)    = HS(I+1,J)
 1951               END DO
 1952             NH      = NH - 1
 1953 !
 1954           ELSE
 1955 !
 1956             TFN(1) = TN(1)
 1957             TFN(2) = TN(2)
 1958             CALL TICK21 ( TFN , 1. )
 1959             IERR   = -1
 1960 !
 1961           END IF
 1962 !
 1963 ! Check time
 1964 !
 1965  
 1966           DTTST  = DSEC21 ( T0 , TFN )
 1967                                       
 1968           ! exit if field time is later than run time
 1969           IF ( DTTST .GT. 0. ) EXIT
 1970           ! exit if field is ic* or md* or lev or ice
 1971           ! and first forcing field has been stored
 1972           ! at start run time
 1973           IF ( J.LE.(1).OR.(J.EQ.4) ) THEN
 1974             IF (FLFRST .AND. DTTST.EQ.0. ) EXIT
 1975           END IF
 1976         END DO
 1977 !
 1978 ! Check if first field
 1979 !
 1980       IF ( J.NE.1 .AND. TFN(1) .EQ. -1 ) THEN
 
 
                                                         w3fldsmd.F90  page  37
 
 
 1981           TF0(1) = T0(1)
 1982           TF0(2) = T0(2)
 1983 !
 1984           DO IX=1, NX
 1985             DO IY=1, NY
 1986               FX0(IX,IY) = FXN(IX,IY)
 1987               FY0(IX,IY) = FYN(IX,IY)
 1988               FS0(IX,IY) = FSN(IX,IY)
 1989               END DO
 1990             END DO
 1991         END IF
 1992 !
 1993       RETURN
 1994 !
 1995 ! Error escape locations
 1996 !
 1997   801 CONTINUE
 1998       IF ( NDSE .GE. 0 ) WRITE (NDSE,1001) J
 1999       IERR   = 1
 2000       RETURN
 2001 !
 2002 ! Formats
 2003 !
 2004  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDH : '/               &
 2005                '     ILLEGAL FIELD ID NR : ',I4/)
 2006 !
 2007 !/
 2008 !/ End of W3FLDH ----------------------------------------------------- /
 2009 !/
 2010       END SUBROUTINE W3FLDH
 2011 !/ ------------------------------------------------------------------- /
 2012       SUBROUTINE W3FLDM (J, NDST, NDSE, T0, TN, NH, NHM, THO, HA, HD, &
 2013                          TF0, A0, D0, TFN, AN, DN, IERR)
 2014 !/
 2015 !/                  +-----------------------------------+
 2016 !/                  | WAVEWATCH III           NOAA/NCEP |
 2017 !/                  |           H. L. Tolman            |
 2018 !/                  |                        FORTRAN 90 |
 2019 !/                  | Last update :         26-Dec-2002 |
 2020 !/                  +-----------------------------------+
 2021 !/
 2022 !/    26-Dec-2002 : Origination.                        ( version 3.02 )
 2023 !/
 2024 !  1. Purpose :
 2025 !
 2026 !     Update moving grid info for the WAVEWATCH III generic
 2027 !     shell.
 2028 !
 2029 !  2. Method :
 2030 !
 2031 !     Variables defining the homogeneous fields are transfered through
 2032 !     the parameter list (see section 3).
 2033 !
 2034 !  3. Parameters :
 2035 !
 
 
                                                         w3fldsmd.F90  page  38
 
 
 2036 !     Parameter list
 2037 !     ----------------------------------------------------------------
 2038 !       J       Int    I   Field number, should be 4.
 2039 !       NDST    Int.   I   Unit number test output.
 2040 !       NDSE    Int.   I   Unit number error messages.
 2041 !                          (No output if NDSE < 0).
 2042 !       T0-N    I.A.   I   Time interval considered.
 2043 !       NH      Int.  I/O  Number of homogeneous fields J.
 2044 !       NHM     Int.   I   Array dimension corresponding to NH.
 2045 !       THO     I.A.  I/O  Times for all homogeneous fields left.
 2046 !       HA      R.A.  I/O  Id. amplitude.
 2047 !       HD      R.A.  I/O  Id. direction (degr., Naut.).
 2048 !       TF0-N   I.A.  I/O  Times of input fields
 2049 !       A/D0/N  R.A.  I/O  Input data.
 2050 !       IERR    Int.   O   Error indicator,
 2051 !                           0 OK,
 2052 !                           1 Illegal field number
 2053 !                          -1 Past last data
 2054 !     ----------------------------------------------------------------
 2055 !
 2056 !  4. Subroutines used :
 2057 !
 2058 !      Name      Type  Module   Description
 2059 !     ----------------------------------------------------------------
 2060 !      STRACE    Subr.   Id.    Subroutine tracing.
 2061 !      TICK21    Subr. W3TIMEMD Advance time.
 2062 !      DSEC21    Func.   Id.    Difference between times.
 2063 !     ----------------------------------------------------------------
 2064 !
 2065 !  5. Called by :
 2066 !
 2067 !      Name      Type  Module   Description
 2068 !     ----------------------------------------------------------------
 2069 !      WW3_SHEL  Prog.   N/A    Basic wave model driver.
 2070 !     ----------------------------------------------------------------
 2071 !
 2072 !  6. Error messages :
 2073 !
 2074 !     - See end of subroutine.
 2075 !     - Array dimensions not checked.
 2076 !
 2077 !  7. Remarks :
 2078 !
 2079 !  8. Structure :
 2080 !
 2081 !       See source code.
 2082 !
 2083 !  9. Switches :
 2084 !
 2085 !     !/S  Enable subroutine tracing.
 2086 !     !/T  Enable test output.
 2087 !
 2088 ! 10. Source code :
 2089 !
 2090 !/ ------------------------------------------------------------------- /
 
 
                                                         w3fldsmd.F90  page  39
 
 
 2091 !/
 2092       USE W3TIMEMD
 2093 !
 2094       IMPLICIT NONE
 2095 !/
 2096 !/ ------------------------------------------------------------------- /
 2097 !/ Parameter list
 2098 !/
 2099       INTEGER, INTENT(IN)     :: J, NDST, NDSE, T0(2), TN(2), NHM
 2100       INTEGER, INTENT(INOUT)  :: NH, THO(2,-7:8,NHM), TF0(2), TFN(2)
 2101       INTEGER, INTENT(OUT)    :: IERR
 2102       REAL, INTENT(INOUT)     :: HA(NHM,-7:8), HD(NHM,-7:8), A0, AN, D0, >
 2103 !/
 2104 !/ ------------------------------------------------------------------- /
 2105 !/ Local parameters
 2106 !/
 2107       INTEGER                 :: I
 2108       REAL                    :: DTTST, DERA
 2109       LOGICAL                 :: FLFRST
 2110 !/
 2111 !/ ------------------------------------------------------------------- /
 2112 !/
 2113 !
 2114       IERR   = 0
 2115       DERA   = ATAN(1.)/45.
 2116 !
 2117 ! Test field ID number for validity
 2118 !
 2119       IF ( J .NE. 4 ) GOTO 801
 2120       FLFRST = TFN(1) .EQ. -1
 2121 !
 2122 ! Backward branch point ============================================= *
 2123 !
 2124   100 CONTINUE
 2125 !
 2126 ! Shift data
 2127 !
 2128       TF0(1) = TFN(1)
 2129       TF0(2) = TFN(2)
 2130       IF ( TFN(1) .NE. -1 ) THEN
 2131           A0     = AN
 2132           D0     = DN
 2133         END IF
 2134 !
 2135 ! New field
 2136 !
 2137       IF ( NH .NE. 0. ) THEN
 2138           TFN(1) = THO(1,J,1)
 2139           TFN(2) = THO(2,J,1)
 2140           AN     = HA(1,J)
 2141           DN     = ( 90. - HD(1,J) ) * DERA
 2142 !
 2143 ! Shift data arrays
 2144 !
 2145           DO I=1, NH-1
 
 
                                                         w3fldsmd.F90  page  40
 
 
 2146             THO(1,J,I) = THO(1,J,I+1)
 2147             THO(2,J,I) = THO(2,J,I+1)
 2148             HA(I,J)    = HA(I+1,J)
 2149             HD(I,J)    = HD(I+1,J)
 2150             END DO
 2151           NH      = NH - 1
 2152 !
 2153         ELSE
 2154 !
 2155           TFN(1) = TN(1)
 2156           TFN(2) = TN(2)
 2157           CALL TICK21 ( TFN , 1. )
 2158           IERR   = -1
 2159 !
 2160         END IF
 2161 !
 2162 ! Check time
 2163 !
 2164       DTTST  = DSEC21 ( T0 , TFN )
 2165       IF ( DTTST .LE. 0. ) GOTO 100
 2166 !
 2167 ! Check if first field
 2168 !
 2169       IF ( TF0(1).EQ.-1 ) THEN
 2170           TF0(1) = T0(1)
 2171           TF0(2) = T0(2)
 2172           A0     = AN
 2173           D0     = DN
 2174         END IF
 2175 !
 2176       RETURN
 2177 !
 2178 ! Error escape locations
 2179 !
 2180   801 CONTINUE
 2181       IF ( NDSE .GE. 0 ) WRITE (NDSE,1001) J
 2182       IERR   = 1
 2183       RETURN
 2184 !
 2185 ! Formats
 2186 !
 2187  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLDM : '/               &
 2188                '     ILLEGAL FIELD ID NR : ',I4/)
 2189 !
 2190 !/
 2191 !/ End of W3FLDM ----------------------------------------------------- /
 2192 !/
 2193       END SUBROUTINE W3FLDM
 2194 !/
 2195 !/ End of module W3FLDSMD -------------------------------------------- /
 2196 !/
 2197       END MODULE W3FLDSMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
