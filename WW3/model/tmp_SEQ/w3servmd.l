                                                         w3servmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3SERVMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         15-Jan-2021 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    For update log see individual subroutines.
   13 !/    12-Jun-2012 : Add /RTD option or rotated grid option.
   14 !/                  (Jian-Guo Li)                       ( version 4.06 )
   15 !/    11-Nov-2013 : SMC and rotated grid incorporated in the main
   16 !/                  trunk                               ( version 4.13 )
   17 !/    18-Aug-2016 : Add dist_sphere: angular distance   ( version 5.11 )
   18 !/    01-Mar-2016 : Added W3THRTN and W3XYRTN for post  ( version 6.02 )
   19 !/                  processing rotated grid data
   20 !/    15-Jan-2021 : Added UV_TO_MAG_DIR routine         ( version 7.12 )
   21 !/
   22 !/    Copyright 2009-2012 National Weather Service (NWS),
   23 !/       National Oceanic and Atmospheric Administration.  All rights
   24 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   25 !/       No unauthorized use without permission.
   26 !/
   27 !  1. Purpose :
   28 !
   29 !     In this module all WAVEWATCH specific service routines have
   30 !     been gathered.
   31 !
   32 !  2. Variables and types :
   33 !
   34 !      Name      Type  Scope    Description
   35 !     ----------------------------------------------------------------
   36 !      NDSTRC    Int.  Private  Data set number for output of STRACE
   37 !                               (set in ITRACE).
   38 !      NTRACE    Int.  Private  Maximum number of trace prints in
   39 !                               strace (set in ITRACE).
   40 !     ----------------------------------------------------------------
   41 !
   42 !  3. Subroutines and functions :
   43 !
   44 !      Name      Type  Scope    Description
   45 !     ----------------------------------------------------------------
   46 !      ITRACE    Subr. Public   (Re-) Initialization for STRACE.
   47 !      STRACE    Subr. Public   Enable subroutine tracing, usually
   48 !                               activated with the !/S switch.
   49 !      NEXTLN    Subr. Public   Get to next line in input command file.
   50 !      W3S2XY    Subr. Public   Grid conversion routine.
   51 !      EJ5P      R.F.  Public   Five parameter JONSWAP spectrum.
   52 !      WWDATE    Subr. Public   Get system date.
   53 !      WWTIME    Subr. Public   Get system time.
   54 !      EXTCDE    Subr. Public   Abort program with exit code.
   55 !     Four subs for rotated grid are appended to this module.  As they
 
 
                                                         w3servmd.F90  page   2
 
 
   56 !     are shared with SMC grid, they are not quoted by option /RTD but
   57 !     are available for general use.     JGLi12Jun2012
   58 !     W3SPECTN       turns wave spectrum anti-clockwise by AnglD
   59 !     W3ACTURN       turns wave action(k,nth) anti-clockwise by AnglD.
   60 !     W3LLTOEQ       convert standard into rotated lat/lon, plus AnglD
   61 !     W3EQTOLL       revers of the LLTOEQ, but AnglD unchanged.
   62 !     W3THTRN        turns direction value anti-clockwise by AnglD
   63 !     W3XYTRN        turns 2D vectors anti-clockwise by AnglD
   64 !
   65 !     ----------------------------------------------------------------
   66 !
   67 !  4. Subroutines and functions used :
   68 !
   69 !     None.
   70 !
   71 !  5. Remarks :
   72 !
   73 !  6. Switches
   74 !
   75 !       !/S    Enable subroutine tracing using STRACE in this module.
   76 !
   77 !       !/F90  FORTRAN 90 specific switches.
   78 !
   79 !  7. Source code :
   80 !
   81 !/ ------------------------------------------------------------------- /
   82       PUBLIC
   83 !
   84       INTEGER, PRIVATE        :: NDSTRC = 6, NTRACE = 0
   85 !
   86       CONTAINS
   87 !/ ------------------------------------------------------------------- /
   88       SUBROUTINE ITRACE (NDS, NMAX)
   89 !/
   90 !/                  +-----------------------------------+
   91 !/                  | WAVEWATCH III           NOAA/NCEP |
   92 !/                  |           H. L. Tolman            |
   93 !/                  |                        FORTRAN 90 |
   94 !/                  | Last update :         23-Nov-1999 |
   95 !/                  +-----------------------------------+
   96 !/
   97 !/    23-Nov-1999 : First version of routine.           ( version 2.00 )
   98 !/
   99 !  1. Purpose :
  100 !
  101 !     (Re-) initialization for module version of STRACE.
  102 !
  103 !  3. Parameter list
  104 !     ----------------------------------------------------------------
  105 !       NDS     Int.   I   Data set number ofr trace file.
  106 !       NMAX    Int.   I   Maximum number of traces per routine.
  107 !     ----------------------------------------------------------------
  108 !
  109 !     Private to module :
  110 !     ----------------------------------------------------------------
 
 
                                                         w3servmd.F90  page   3
 
 
  111 !       NDSTRC  Int.  Output unit number for trace.     ( from NDS  )
  112 !       NTRACE  Int.  Maximum number of trace prints.   ( from NMAX )
  113 !     ----------------------------------------------------------------
  114 !
  115 !  4. Subroutines used :
  116 !
  117 !     None.
  118 !
  119 !  5. Called by :
  120 !
  121 !     Any program, multiple calls allowed.
  122 !
  123 !  9. Switches :
  124 !
  125 ! 10. Source code :
  126 !
  127 !/ ------------------------------------------------------------------- /
  128       IMPLICIT NONE
  129 !/
  130 !/ ------------------------------------------------------------------- /
  131 !/ Parameter list
  132 !/
  133       INTEGER, INTENT(IN)     :: NDS, NMAX
  134 !/
  135 !/ ------------------------------------------------------------------- /
  136 !/
  137       NTRACE = MAX ( 0 , NMAX )
  138       NDSTRC = NDS
  139 !
  140       RETURN
  141 !/
  142 !/ End of ITRACE ----------------------------------------------------- /
  143 !/
  144       END SUBROUTINE ITRACE
  145 !/ ------------------------------------------------------------------- /
  146       SUBROUTINE STRACE (IENT, SNAME)
  147 !/
  148 !/                  +-----------------------------------+
  149 !/                  | WAVEWATCH III           NOAA/NCEP |
  150 !/                  |           H. L. Tolman            |
  151 !/                  |                        FORTRAN 90 |
  152 !/                  | Last update :         25-Jan-2000 |
  153 !/                  +-----------------------------------+
  154 !/                                   Original version by N. Booij, DUT
  155 !/
  156 !/    30-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
  157 !/    23-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  158 !/    25-Jan-2000 : Force flushing of uniit.            ( version 2.00 )
  159 !/                  This was taken out around version 3.01.
  160 !/
  161 !  1. Purpose :
  162 !
  163 !     Keep track of entered subroutines.
  164 !
  165 !  3. Parameter list
 
 
                                                         w3servmd.F90  page   4
 
 
  166 !     ----------------------------------------------------------------
  167 !       IENT    Int.  I/O  Number of times that STRACE has been
  168 !                          called by the routine.
  169 !       SNAME   Char.  I   Name of the subroutine (max. 6 characters)
  170 !     ----------------------------------------------------------------
  171 !
  172 !     Private to module :
  173 !     ----------------------------------------------------------------
  174 !       NDSTRC  Int.  Output unit number for trace.
  175 !       NTRACE  Int.  Maximum number of trace prints.
  176 !     ----------------------------------------------------------------
  177 !
  178 !  4. Subroutines used :
  179 !
  180 !     None.
  181 !
  182 !  5. Called by :
  183 !
  184 !     Any program, after private variables have been set by NTRACE.
  185 !
  186 !  9. Switches :
  187 !
  188 ! 10. Source code :
  189 !
  190 !/ ------------------------------------------------------------------- /
  191       IMPLICIT NONE
  192 !/
  193 !/ ------------------------------------------------------------------- /
  194 !/ Parameter list
  195 !/
  196       INTEGER, INTENT(INOUT)  :: IENT
  197       CHARACTER, INTENT(IN)   :: SNAME*(*)
  198 !/
  199 !/ ------------------------------------------------------------------- /
  200 !/
  201       IF (NTRACE.EQ.0 .OR. IENT.GE.NTRACE) RETURN
  202 !
  203       IENT = IENT + 1
  204       IF (IENT.EQ.1) THEN
  205           WRITE (NDSTRC,10) SNAME
  206         ELSE
  207           WRITE (NDSTRC,11) SNAME, IENT
  208         END IF
  209 !
  210       RETURN
  211 !
  212 ! Formats
  213 !
  214   10  FORMAT (' ---> TRACE SUBR : ',A6)
  215   11  FORMAT (' ---> TRACE SUBR : ',A6,'  ENTRY: ',I6)
  216 !/
  217 !/ End of STRACE ----------------------------------------------------- /
  218 !/
  219       END SUBROUTINE STRACE
  220 !/ ------------------------------------------------------------------- /
 
 
                                                         w3servmd.F90  page   5
 
 
  221       SUBROUTINE NEXTLN ( CHCKC , NDSI , NDSE )
  222 !/
  223 !/                  +-----------------------------------+
  224 !/                  | WAVEWATCH III           NOAA/NCEP |
  225 !/                  |           H. L. Tolman            |
  226 !/                  |                        FORTRAN 90 |
  227 !/                  | Last update :         10-Dec-2014 |
  228 !/                  +-----------------------------------+
  229 !/
  230 !/    15-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
  231 !/    18-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  232 !/    10-Dec-2014 : Skip blank lines and leading blanks ( version 5.04 )
  233 !/
  234 !  1. Purpose :
  235 !
  236 !     Sets file pointer to next active line of input file, by skipping
  237 !     blank lines and lines starting with the character CHCKC. Leading
  238 !     white space is allowed before the character CHCKC.
  239 !
  240 !  3. Parameters :
  241 !
  242 !     Parameter list
  243 !     ----------------------------------------------------------------
  244 !       CHCKC   C*1   I  Check character for defining comment line.
  245 !       NDSI    Int.  I  Input dataset number.
  246 !       NDSE    Int.  I  Error output dataset number.
  247 !                        (No output if NDSE < 0).
  248 !     ----------------------------------------------------------------
  249 !
  250 !  4. Subroutines used :
  251 !
  252 !       STRACE ( !/S switch )
  253 !
  254 !  5. Called by :
  255 !
  256 !       Any routine.
  257 !
  258 !  6. Error messages :
  259 !
  260 !     - On EOF or error in input file.
  261 !
  262 !  9. Switches :
  263 !
  264 !     !/S  Enable subroutine tracing.
  265 !
  266 ! 10. Source code :
  267 !
  268 !/ ------------------------------------------------------------------- /
  269       IMPLICIT NONE
  270 !/
  271 !/ ------------------------------------------------------------------- /
  272 !/ Parameter list
  273 !/
  274       INTEGER, INTENT(IN)     :: NDSI, NDSE
  275       CHARACTER, INTENT(IN)   :: CHCKC*1
 
 
                                                         w3servmd.F90  page   6
 
 
  276 !/
  277 !/ ------------------------------------------------------------------- /
  278 !/ Local parameters
  279 !/
  280       INTEGER                 :: IERR
  281       CHARACTER(128)          :: MSG
  282       CHARACTER(256)          :: LINE, TEST
  283 !/
  284 !/ ------------------------------------------------------------------- /
  285 !/
  286 !
  287   100 CONTINUE
  288       ! read line
  289       READ ( NDSI, 900, END=800, ERR=801, IOSTAT=IERR, IOMSG=MSG ) LINE
  290       ! leading blanks removed and placed on the right
  291       TEST = ADJUSTL ( LINE )
  292       IF ( TEST(1:1).EQ.CHCKC .OR. LEN_TRIM(TEST).EQ.0 ) THEN
  293         ! if comment or blank line, then skip
  294           GOTO 100
  295         ELSE
  296         ! otherwise, backup to beginning of line
  297           BACKSPACE ( NDSI, ERR=802, IOSTAT=IERR, IOMSG=MSG )
  298         ENDIF
  299       RETURN
  300 !
  301   800 CONTINUE
  302       IF ( NDSE .GE. 0 ) WRITE (NDSE,910)
  303       CALL EXTCDE ( 1 )
  304 !
  305   801 CONTINUE
  306       IF ( NDSE .GE. 0 ) WRITE (NDSE,911) IERR, TRIM(MSG)
  307       CALL EXTCDE ( 2 )
  308 !
  309   802 CONTINUE
  310       IF ( NDSE .GE. 0 ) WRITE (NDSE,912) IERR, TRIM(MSG)
  311       CALL EXTCDE ( 3 )
  312 !
  313 ! Formats
  314 !
  315   900 FORMAT (A)
  316   910 FORMAT (/' *** WAVEWATCH III ERROR IN NEXTLN : '/         &
  317                '     PREMATURE END OF INPUT FILE'/)
  318   911 FORMAT (/' *** WAVEWATCH III ERROR IN NEXTLN : '/         &
  319                '     ERROR IN READING FROM FILE'/               &
  320                '     IOSTAT =',I5,/                             &
  321                '     IOMSG = ',A/)
  322   912 FORMAT (/' *** WAVEWATCH III ERROR IN NEXTLN : '/         &
  323                '     ERROR ON BACKSPACE'/                       &
  324                '     IOSTAT =',I5,/                             &
  325                '     IOMSG = ',A/)
  326 !/
  327 !/ End of NEXTLN ----------------------------------------------------- /
  328 !/
  329       END SUBROUTINE NEXTLN
  330 !/ ------------------------------------------------------------------- /
 
 
                                                         w3servmd.F90  page   7
 
 
  331       SUBROUTINE W3S2XY ( NSEA, MSEA, MX, MY, S, MAPSF, XY )
  332 !/
  333 !/                  +-----------------------------------+
  334 !/                  | WAVEWATCH III            NOAA/NMC |
  335 !/                  |           H. L. Tolman            |
  336 !/                  |                        FORTRAN 90 |
  337 !/                  | Last update :         23-Nov-1999 |
  338 !/                  +-----------------------------------+
  339 !/
  340 !/    11-Dec-1996 : Final FORTRAN 77                    ( version 1.18 )
  341 !/    23-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  342 !/
  343 !  1. Purpose :
  344 !
  345 !     Convert a data array on the storage grid to a data array on the
  346 !     full spatial grid. Land and ice points in the full grid are
  347 !     not touched. Output array of conventional type XY(IX,IY).
  348 !
  349 !  3. Parameters :
  350 !
  351 !     Parameter list
  352 !     ----------------------------------------------------------------
  353 !       NSEA    Int.   I    Number of sea points.
  354 !       MSEA, MX, MY
  355 !               Int.   I    Array dimensions.
  356 !       S       R.A.   I    Data on storage grid.
  357 !       MAPSF   I.A.   I    Storage map for IX and IY, resp.
  358 !       XY      R.A.   O    Data on XY grid.
  359 !     ----------------------------------------------------------------
  360 !
  361 !  4. Subroutines used :
  362 !
  363 !     None.
  364 !
  365 !  5. Called by :
  366 !
  367 !     Any WAVEWATCH III routine.
  368 !
  369 !  9. Switches :
  370 !
  371 !     None.
  372 !
  373 ! 10. Source code :
  374 !
  375 !/ ------------------------------------------------------------------- /
  376       IMPLICIT NONE
  377 !/
  378 !/ ------------------------------------------------------------------- /
  379 !/ Parameter list
  380 !/
  381       INTEGER, INTENT(IN)     :: MSEA, NSEA, MX, MY, MAPSF(MSEA,2)
  382       REAL, INTENT(IN)        :: S(MSEA)
  383       REAL, INTENT(OUT)       :: XY(MX,MY)
  384 !/
  385 !/ ------------------------------------------------------------------- /
 
 
                                                         w3servmd.F90  page   8
 
 
  386 !/ Local parameters
  387 !/
  388       INTEGER                 :: ISEA, IX, IY
  389 !/
  390 !/ ------------------------------------------------------------------- /
  391 !/
  392       DO 100, ISEA=1, NSEA
  393         IX     = MAPSF(ISEA,1)
  394         IY     = MAPSF(ISEA,2)
  395         XY(IX,IY) = S(ISEA)
  396   100   CONTINUE
  397 !/
  398 !/ End of W3S2XY ----------------------------------------------------- /
  399 !/
  400       END SUBROUTINE W3S2XY
  401 !/ ------------------------------------------------------------------- /
  402       REAL FUNCTION EJ5P ( F, ALFA, FP, YLN, SIGA, SIGB )
  403 !/
  404 !/                  +-----------------------------------+
  405 !/                  | WAVEWATCH III           NOAA/NCEP |
  406 !/                  |           H. L. Tolman            |
  407 !/                  |                        FORTRAN 90 |
  408 !/                  | Last update :         23-Nov-1999 |
  409 !/                  +-----------------------------------+
  410 !/
  411 !/    23-AMy-1985 : Original by G. Ph. van Vledder.
  412 !/    23-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  413 !/
  414 !  1. Purpose :
  415 !
  416 !     Computation of spectral density using a 5-parameter
  417 !     JONSWAP-spectrum
  418 !
  419 !  2. Method
  420 !
  421 !     EJ5P(F) = A.EXP(B + LN(Y).EXP(C))
  422 !
  423 !     where: A = ALFA * 0.06175 * F**(-5)
  424 !            B = -1.25*(FP/F)**4
  425 !            C = -0.5 * ((F - FP)/(SIG * FP))**2
  426 !     and
  427 !            GRAV**2/(2.PI)**4 = 0.06175
  428 !
  429 !  3. Parameters :
  430 !
  431 !     Parameter list
  432 !
  433 !     ----------------------------------------------------------------
  434 !       F       Real   I    Frequency in Hz
  435 !       ALFA    Real   I    Energy scaling factor
  436 !       FP      Real   I    Peak frequency in Hz
  437 !       YLN     Real   I    Peak overshoot factor, given by LN-value
  438 !       SIGA    Real   I    Spectral width, for F < FP
  439 !       SIGB    Real   I    Spectral width, FOR F > FP
  440 !     ----------------------------------------------------------------
 
 
                                                         w3servmd.F90  page   9
 
 
  441 !
  442 !  4. Subroutines used :
  443 !
  444 !     None.
  445 !
  446 !  5. Called by :
  447 !
  448 !     Any.
  449 !
  450 !  6. Error messages :
  451 !
  452 !  7. Remarks :
  453 !
  454 !     EXPMIN is a machine dependant constant such that
  455 !     EXP(EXPMIN) can be successfully evaluated without
  456 !     underflow by the compiler supllied EXP routine.
  457 !
  458 !  8. Structure :
  459 !
  460 !     See source code.
  461 !
  462 !  9. Switches :
  463 !
  464 !     None.
  465 !
  466 ! 10. Source code :
  467 !
  468 !/ ------------------------------------------------------------------- /
  469       IMPLICIT NONE
  470 !/
  471 !/ ------------------------------------------------------------------- /
  472 !/ Parameter list
  473 !/
  474       REAL, INTENT(IN)        :: F, ALFA, FP, YLN, SIGA, SIGB
  475 !/
  476 !/ ------------------------------------------------------------------- /
  477 !/ Local parameters
  478 !/
  479       REAL                    :: SIG, A, B, C
  480       REAL, SAVE              :: EPS=1.E-4, EXPMIN=-180.
  481 !/
  482 !/ ------------------------------------------------------------------- /
  483 !/
  484       IF(F.LT.EPS) THEN
  485         EJ5P = 0.0
  486         RETURN
  487       END IF
  488 !
  489       A = ALFA * 0.06175 / F**5
  490       B = -1.25 * (FP/F)**4
  491       B = MAX(B,EXPMIN)
  492 !
  493       IF (YLN.LT.EPS) THEN
  494         EJ5P = A * EXP(B)
  495       ELSE
 
 
                                                         w3servmd.F90  page  10
 
 
  496         IF( F.LE.FP) THEN
  497           SIG = SIGA
  498         ELSE
  499           SIG = SIGB
  500         END IF
  501         C = -0.5 * ((F - FP)/(SIG * FP))**2
  502         C = MAX(C,EXPMIN)
  503         EJ5P = A * EXP(B + EXP(C) * YLN)
  504       END IF
  505 !
  506       RETURN
  507 !/
  508 !/ End of NEXTLN ----------------------------------------------------- /
  509 !/
  510       END FUNCTION
  511 !/ ------------------------------------------------------------------- /
  512       REAL FUNCTION DIST_SPHERE ( lo1,la1,lo2,la2 )
  513 !/
  514 !/                  +-----------------------------------+
  515 !/                  | WAVEWATCH III           NOAA/NCEP |
  516 !/                  |           F. Ardhuin              |
  517 !/                  |                        FORTRAN 90 |
  518 !/                  | Last update :         18-Aug-2016 |
  519 !/                  +-----------------------------------+
  520 !/
  521 !/    18-Aug-2016 :  Creation                           ( version 5.11 )
  522 !/
  523 !  1. Purpose :
  524 !
  525 !     Computes distance between two points on a sphere
  526 !
  527 !  2. Method
  528 !
  529 !  3. Parameters :
  530 !
  531 !     Parameter list
  532 !
  533 !     ----------------------------------------------------------------
  534 !       LO1     Real   I    Longitude of 1st point
  535 !       LA1     Real   I    Latitude of 1st point
  536 !       LO2     Real   I    Longitude of 2nd point
  537 !       LA2     Real   I    Latitude of 2nd point
  538 !     ----------------------------------------------------------------
  539 !
  540 !  4. Subroutines used :
  541 !
  542 !     None.
  543 !
  544 !  5. Called by :
  545 !
  546 !     WW3_BOUNC
  547 !
  548 !  6. Error messages :
  549 !
  550 !  7. Remarks :
 
 
                                                         w3servmd.F90  page  11
 
 
  551 !
  552 !     None.
  553 !
  554 !  8. Structure :
  555 !
  556 !     See source code.
  557 !
  558 !  9. Switches :
  559 !
  560 !     None.
  561 !
  562 ! 10. Source code :
  563 !
  564 !/ ------------------------------------------------------------------- /
  565       USE CONSTANTS
  566       IMPLICIT NONE
  567 !/
  568 !/ ------------------------------------------------------------------- /
  569 !/ Parameter list
  570 !/
  571       REAL, INTENT(IN)        :: LO1, LA1, LO2, LA2
  572 !/
  573 !/ ------------------------------------------------------------------- /
  574 !/ Local parameters
  575 !/
  576 !  None
  577 !/
  578 !/ ------------------------------------------------------------------- /
  579 !/
  580      DIST_SPHERE=acos(sin(la2*DERA)*sin(la1*DERA)+ &
  581                       cos(la2*DERA)*cos(la1*DERA)*cos((lo2-lo1)*DERA))*RA>
  582 !
  583       RETURN
  584 !/
  585 !/ End of NEXTLN ----------------------------------------------------- /
  586 !/
  587       END FUNCTION
  588 !/ ------------------------------------------------------------------- /
  589                                                                         
  590 !/ ------------------------------------------------------------------- /
  591       SUBROUTINE WWDATE (STRNG)
  592 !/
  593 !/                  +-----------------------------------+
  594 !/                  | WAVEWATCH III           NOAA/NCEP |
  595 !/                  |           H. L. Tolman            |
  596 !/                  |                        FORTRAN 90 |
  597 !/                  | Last update :         26-Dec-2012 |
  598 !/                  +-----------------------------------+
  599 !/
  600 !/    23-Dec-1998 : Final FORTRAN 77                    ( version 1.18 )
  601 !/    23-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  602 !/    18-Sep-2000 : PGI switch added                    ( version 2.04 )
  603 !/    13-Mar-2001 : LF95 switch added                   ( version 2.09 )
  604 !/    08-May-2002 : Replace obsolete switches with F90  ( version 2.21 )
  605 !/    26-Dec-2012 : Modified obsolete declarations.     ( version 4.11 )
 
 
                                                         w3servmd.F90  page  12
 
 
  606 !/
  607 !  1. Purpose :
  608 !
  609 !     Get date from machine dependent routine.
  610 !
  611 !  3. Parameters :
  612 !
  613 !     Parameter list
  614 !     ----------------------------------------------------------------
  615 !       STRNG   C*10   O   String with date in format YYYY/MM/DD
  616 !     ----------------------------------------------------------------
  617 !
  618 !  4. Subroutines used :
  619 !
  620 !     Machine dependent.
  621 !
  622 !  5. Called by :
  623 !
  624 !     Any routine.
  625 !
  626 !  9. Switches :
  627 !
  628 !     !/DUM  Dummy.
  629 !     !/F90  FORTRAN 90 standard.
  630 !
  631 ! 10. Source code :
  632 !
  633 !/ ------------------------------------------------------------------- /
  634       IMPLICIT NONE
  635 !/
  636 !/ ------------------------------------------------------------------- /
  637 !/ Parameter list
  638 !/
  639       CHARACTER, INTENT(OUT)  :: STRNG*10
  640 !/
  641 !/ ------------------------------------------------------------------- /
  642 !/ Local parameters
  643 !/
  644       CHARACTER(LEN=8)        :: DATE
  645       CHARACTER(LEN=10)       :: TIME
  646       CHARACTER(LEN=5)        :: ZONE
  647       INTEGER                 :: VALUES(8)
  648 !/
  649 !/ ------------------------------------------------------------------- /
  650 !/
  651 ! This is supposed to be standard F90
  652 !
  653       STRNG = '----/--/--'
  654       CALL DATE_AND_TIME ( DATE, TIME, ZONE, VALUES )
  655       STRNG(1:4) = DATE(1:4)
  656       STRNG(6:7) = DATE(5:6)
  657       STRNG(9:10) = DATE(7:8)
  658 !
  659 ! Dummy alternative
  660 !
 
 
                                                         w3servmd.F90  page  13
 
 
  661       RETURN
  662 !/
  663 !/ End of WWDATE ----------------------------------------------------- /
  664 !/
  665       END SUBROUTINE WWDATE
  666 !/ ------------------------------------------------------------------- /
  667       SUBROUTINE WWTIME (STRNG)
  668 !/
  669 !/                  +-----------------------------------+
  670 !/                  | WAVEWATCH III           NOAA/NCEP |
  671 !/                  |           H. L. Tolman            |
  672 !/                  |                        FORTRAN 90 |
  673 !/                  | Last update :         26-Dec-2012 |
  674 !/                  +-----------------------------------+
  675 !/
  676 !/    23-Dec-1998 : Final FORTRAN 77                    ( version 1.18 )
  677 !/    23-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  678 !/    18-Sep-2000 : PGI switch added                    ( version 2.04 )
  679 !/    13-Mar-2001 : LF95 switch added                   ( version 2.09 )
  680 !/    08-May-2002 : Replace obsolete switches with F90  ( version 2.21 )
  681 !/    26-Dec-2012 : Modified obsolete declarations.     ( version 4.11 )
  682 !/
  683 !  1. Purpose :
  684 !
  685 !     Get time from machine dependent routine.
  686 !
  687 !  2. Method :
  688 !
  689 !  3. Parameters :
  690 !
  691 !     Parameter list
  692 !     ----------------------------------------------------------------
  693 !       STRNG   C*8    O   String with time in format hh:mm:ss
  694 !     ----------------------------------------------------------------
  695 !
  696 !  4. Subroutines used :
  697 !
  698 !     Machine dependent.
  699 !
  700 !  5. Called by :
  701 !
  702 !     Any routine.
  703 !
  704 !  9. Switches :
  705 !
  706 !     !/DUM  Dummy.
  707 !     !/F90  FORTRAN 90 standard.
  708 !
  709 ! 10. Source code :
  710 !
  711 !/ ------------------------------------------------------------------- /
  712       IMPLICIT NONE
  713 !/
  714 !/ ------------------------------------------------------------------- /
  715 !/ Parameter list
 
 
                                                         w3servmd.F90  page  14
 
 
  716 !/
  717       CHARACTER, INTENT(OUT)  :: STRNG*8
  718 !/
  719 !/ ------------------------------------------------------------------- /
  720 !/ Local parameters
  721 !/
  722       CHARACTER(LEN=8)        :: DATE
  723       CHARACTER(LEN=10)       :: TIME
  724       CHARACTER(LEN=5)        :: ZONE
  725       INTEGER                 :: VALUES(8)
  726 !/
  727 !/ ------------------------------------------------------------------- /
  728 !/
  729 ! This is supposed to be standard F90
  730 !
  731       STRNG = '--:--:--'
  732       CALL DATE_AND_TIME ( DATE, TIME, ZONE, VALUES )
  733       STRNG(1:2) = TIME(1:2)
  734       STRNG(4:5) = TIME(3:4)
  735       STRNG(7:8) = TIME(5:6)
  736 !
  737 ! Dummy alternative
  738 !
  739       RETURN
  740 !/
  741 !/ End of WWTIME ----------------------------------------------------- /
  742 !/
  743       END SUBROUTINE WWTIME
  744 !/ ------------------------------------------------------------------- /
  745       SUBROUTINE EXTCDE ( IEXIT, UNIT, MSG, FILE, LINE, COMM )
  746 !/
  747 !/                  +-----------------------------------+
  748 !/                  | WAVEWATCH III           NOAA/NCEP |
  749 !/                  |           H. L. Tolman            |
  750 !/                  |                        FORTRAN 90 |
  751 !/                  | Last update :         06-Jun-2018 |
  752 !/                  +-----------------------------------+
  753 !/
  754 !/    06-Jan-1998 : Final FORTRAN 77                    ( version 1.18 )
  755 !/    23-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  756 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
  757 !/    11-Mar-2015 : Allow non-error exit (iexit=0)      ( version 5.04 )
  758 !/    20-Jan-2017 : Add optional MPI communicator arg   ( version 6.02 )
  759 !/    06-Jun-2018 : Add optional MPI                    ( version 6.04 )
  760 !/
  761 !  1. Purpose :
  762 !
  763 !     Perform a program stop with an exit code.
  764 !
  765 !     If exit code IEXIT=0, then it is not an error, but
  766 !     a stop has been requested by the calling routine:
  767 !     wait for other processes in communicator to catch up.
  768 !
  769 !     If exit code IEXIT.ne.0, then abort program w/out
  770 !     waiting for other processes to catch up (important for example
 
 
                                                         w3servmd.F90  page  15
 
 
  771 !     when not all processes are used by WW3).
  772 !
  773 !  2. Method :
  774 !
  775 !     Machine dependent.
  776 !
  777 !  3. Parameters :
  778 !
  779 !     Parameter list
  780 !     ----------------------------------------------------------------
  781 !       IEXIT   Int.   I   Exit code to be used.
  782 !       UNIT    Int.   I   (optional) file unit to write error message
  783 !       MSG     Str.   I   (optional) error message
  784 !       FILE    Str.   I   (optional) name of source code file
  785 !       LINE    Int.   I   (optional) line number in source code file
  786 !       COMM    Int.   I   (optional) MPI communicator
  787 !     ----------------------------------------------------------------
  788 !
  789 !  4. Subroutines used :
  790 !
  791 !  5. Called by :
  792 !
  793 !     Any.
  794 !
  795 !  9. Switches :
  796 !
  797 !     !/MPI  MPI finalize interface if active
  798 !
  799 ! 10. Source code :
  800 !
  801 !/ ------------------------------------------------------------------- /
  802       IMPLICIT NONE
  803 !
  804 !/
  805 !/ ------------------------------------------------------------------- /
  806 !/ Parameter list
  807 !/
  808       INTEGER, INTENT(IN) :: IEXIT
  809       INTEGER,      INTENT(IN), OPTIONAL :: UNIT
  810       CHARACTER(*), INTENT(IN), OPTIONAL :: MSG
  811       CHARACTER(*), INTENT(IN), OPTIONAL :: FILE
  812       INTEGER,      INTENT(IN), OPTIONAL :: LINE
  813       INTEGER,      INTENT(IN), OPTIONAL :: COMM
  814 !/
  815 !/ ------------------------------------------------------------------- /
  816 !/
  817       INTEGER                 :: IUN
  818       CHARACTER(256)          :: LMSG = ""
  819       CHARACTER(6)            :: LSTR
  820       CHARACTER(10)           :: PREFIX = "WW3 ERROR:"
  821 !/
  822 !/ Set file unit for error output
  823 !/
  824       IUN = 0
  825       IF (PRESENT(UNIT)) IUN = UNIT
 
 
                                                         w3servmd.F90  page  16
 
 
  826 !/
  827 !/ Report error message
  828 !/
  829       IF (PRESENT(MSG)) THEN
  830           WRITE (IUN,"(A)") PREFIX//" "//TRIM(MSG)
  831         END IF
  832 !/
  833 !/ Report context
  834 !/
  835       IF ( PRESENT(FILE) ) THEN
  836           LMSG = TRIM(LMSG)//" FILE="//TRIM(FILE)
  837         END IF
  838       IF ( PRESENT(LINE) ) THEN
  839           WRITE (LSTR,'(I0)') LINE
  840           LMSG = TRIM(LMSG)//" LINE="//TRIM(LSTR)
  841         END IF
  842       IF ( LEN_TRIM(LMSG).GT.0 ) THEN
  843           WRITE (IUN,"(A)") PREFIX//TRIM(LMSG)
  844         END IF
  845 !/
  846 !/ Handle MPI exit
  847 !/
  848 !!/MPI          ELSE
  849 !!/MPI            WRITE(*,'(A)') 'EXTCDE UNIT missing'
  850 !!/MPI          ELSE
  851 !!/MPI            WRITE(*,'(A)') 'EXTCDE MSG missing'
  852 !!/MPI          ELSE
  853 !!/MPI            WRITE(*,'(A)') 'EXTCDE FILE missing'
  854 !!/MPI          ELSE
  855 !!/MPI            WRITE(*,'(A)') 'EXTCDE LINE missing'
  856 !!/MPI          ELSE
  857 !!/MPI            WRITE(*,'(A)') 'EXTCDE COMM missing'
  858 !/
  859 !/ Handle non-MPI exit
  860 !/
  861       CALL EXIT ( IEXIT )
  862 !/
  863 !/ End of EXTCDE ----------------------------------------------------- /
  864 !/
  865       END SUBROUTINE EXTCDE
  866 !/ ------------------------------------------------------------------- /
  867 !  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  868 !  This subroutine turn the wave spectrum by an fixed angle anti-clockwise
  869 !  so that it may be used in the rotated or stanadard system.
  870 !  First created:   26 Aug 2005   Jian-Guo Li
  871 !  Last modified:   21 Feb 2008   Jian-Guo Li
  872 !
  873 ! Subroutine Interface:
  874                        
  875        Subroutine W3SPECTN( NFreq, NDirc, Alpha, Spectr )
  876                                                          
  877 ! Description:
  878 !   Rotates wave spectrum anticlockwise by angle alpha in degree
  879 !   This routine is distinct from W3ACTURN since orders spectrum as freq,>
  880 !
 
 
                                                         w3servmd.F90  page  17
 
 
  881 ! Subroutine arguments
  882         IMPLICIT NONE
  883         INTEGER, INTENT(IN) :: NFreq, NDirc         ! No. freq and dirn b>
  884         REAL,    INTENT(IN) :: Alpha                ! Turning angle (degr>
  885         REAL, INTENT(INOUT) :: Spectr(NFreq,NDirc)  ! Wave spectrum in/out
  886                                                                           
  887 ! Local variables
  888         INTEGER :: ii, jj, kk, nsft
  889         REAL    :: Ddirc, frac, CNST
  890         REAL, Dimension(NFreq)      ::  Wrkfrq, Tmpfrq
  891         REAL, Dimension(NFreq,NDirc)::  Wrkspc
  892                                               
  893 ! Check input bin numbers
  894         IF( (NFreq .LT. 0) .OR. (NDirc .LT. 0) )  THEN
  895            PRINT*, " Invalid bin number NF or ND", NFreq, NDirc
  896            RETURN
  897         ELSE
  898            Ddirc=360.0/FLOAT(NDirc)
  899         ENDIF
  900              
  901 ! Work out shift bin number and fraction
  902                                         
  903            CNST=Alpha/Ddirc
  904            nsft=INT( CNST )
  905            frac= CNST - FLOAT( nsft )
  906 !     PRINT*, ' nsft and frac =', nsft, frac
  907                                             
  908 ! Shift nsft bins if >=1
  909          IF( ABS(nsft) .GE. 1 )  THEN
  910            DO ii=1, NDirc
  911                          
  912 ! Wave spectral direction bin number is assumed to increase Anti-clockwis>
  913 ! So shift nsft bins anticlockwise results in local bin number decreasing>
  914               jj=ii - nsft
  915                           
  916 ! As nsft may be either positive or negative depends on alpha, wrapping m>
  917 ! happen in either ends of the bin number train
  918               IF( jj > NDirc )  jj=jj - NDirc
  919               IF( jj < 1     )  jj=jj + NDirc
  920                                              
  921 ! Copy the selected bin to the loop bin number
  922               Wrkspc(:,ii)=Spectr(:,jj)
  923                                        
  924            ENDDO
  925                 
  926 ! If nsft=0, no need to shift, simply copy
  927          ELSE
  928              Wrkspc = Spectr
  929          ENDIF
  930               
  931 ! Pass fraction of wave energy in frac direction
  932 ! Wave spectral direction bin number is assumed to increase Anti-clockwis>
  933 ! So Positive frac or anticlock case, smaller bin upstream
  934          IF( frac > 0.0 ) THEN
  935            Tmpfrq=Wrkspc(:,NDirc)*frac
 
 
                                                         w3servmd.F90  page  18
 
 
  936            DO kk=1, NDirc
  937               Wrkfrq=Wrkspc(:,kk)*frac
  938               Spectr(:,kk)=Wrkspc(:,kk) - Wrkfrq + Tmpfrq
  939               Tmpfrq=Wrkfrq
  940            ENDDO
  941          ELSE
  942 ! Negative or clockwise case, larger bin upstream
  943            Tmpfrq=Wrkspc(:,1)*frac
  944            DO kk=NDirc, 1, -1
  945               Wrkfrq=Wrkspc(:,kk)*frac
  946               Spectr(:,kk)=Wrkspc(:,kk) + Wrkfrq - Tmpfrq
  947               Tmpfrq=Wrkfrq
  948            ENDDO
  949          ENDIF
  950               
  951 ! Spectral turning completed
  952                             
  953         RETURN
  954         END SUBROUTINE W3SPECTN
  955 !
  956 !  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  957 !  This subroutine turn the wave action by an angle (deg) anti-clockwise
  958 !  so that it may be used in the rotated or stanadard system.
  959 !  First created:   26 Aug 2005   Jian-Guo Li
  960 !  Last modified:    9 Oct 2008   Jian-Guo Li
  961 !
  962 ! Subroutine Interface:
  963                        
  964        Subroutine W3ACTURN( NDirc, NFreq, Alpha, Spectr )
  965                                                          
  966 ! Description:
  967 !   Rotates wave spectrum anticlockwise by angle alpha
  968 !   Routine is distinct from W3SPECTN since orders spectrum as dirn, freq
  969 !
  970 ! Subroutine arguments
  971         IMPLICIT NONE
  972         INTEGER, INTENT(IN) :: NFreq, NDirc          ! No. freq and dirn >
  973         REAL,    INTENT(IN) :: Alpha                 ! Turning angle (deg>
  974         REAL, INTENT(INOUT) :: Spectr(NDirc, NFreq)  ! Wave action in/out
  975                                                                          
  976 ! Local variables
  977         INTEGER :: ii, jj, kk, nsft
  978         REAL    :: Ddirc, frac, CNST
  979         REAL, Dimension(NFreq)      ::  Wrkfrq, Tmpfrq
  980         REAL, Dimension(NDirc,NFreq)::  Wrkspc
  981                                               
  982 ! Check input bin numbers
  983         IF( (NFreq .LT. 0) .OR. (NDirc .LT. 0) )  THEN
  984            PRINT*, " Invalid bin number NF or ND", NFreq, NDirc
  985            RETURN
  986         ELSE
  987            Ddirc=360.0/FLOAT(NDirc)
  988         ENDIF
  989              
  990 ! Work out shift bin number and fraction
 
 
                                                         w3servmd.F90  page  19
 
 
  991                                         
  992         CNST=Alpha/Ddirc
  993         nsft=INT( CNST )
  994         frac= CNST - FLOAT( nsft )
  995 !     PRINT*, ' nsft and frac =', nsft, frac
  996                                             
  997 ! Shift nsft bins if >=1
  998         IF( ABS(nsft) .GE. 1 )  THEN
  999           DO ii=1, NDirc
 1000                         
 1001 ! Wave spectral direction bin number is assumed to increase Anti-clockwis>
 1002 ! So shift nsft bins anticlockwise results in local bin number decreasing>
 1003              jj=ii - nsft
 1004                          
 1005 ! As nsft may be either positive or negative depends on alpha, wrapping m>
 1006 ! happen in either ends of the bin number train
 1007              IF( jj > NDirc )  jj=jj - NDirc
 1008              IF( jj < 1     )  jj=jj + NDirc
 1009                                             
 1010 ! Copy the selected bin to the loop bin number
 1011              Wrkspc(ii,:)=Spectr(jj,:)
 1012                                       
 1013           ENDDO
 1014                
 1015 ! If nsft=0, no need to shift, simply copy
 1016         ELSE
 1017             Wrkspc = Spectr
 1018         ENDIF
 1019              
 1020 ! Pass fraction of wave energy in frac direction
 1021 ! Wave spectral direction bin number is assumed to increase anti-clockwis>
 1022 ! So positive frac or anticlock case, smaller bin upstream
 1023         IF( frac > 0.0 ) THEN
 1024           Tmpfrq=Wrkspc(NDirc,:)*frac
 1025           DO kk=1, NDirc
 1026              Wrkfrq=Wrkspc(kk,:)*frac
 1027              Spectr(kk,:)=Wrkspc(kk,:) - Wrkfrq + Tmpfrq
 1028              Tmpfrq=Wrkfrq
 1029           ENDDO
 1030         ELSE
 1031 ! Negative or clockwise case, larger bin upstream
 1032           Tmpfrq=Wrkspc(1,:)*frac
 1033           DO kk=NDirc, 1, -1
 1034              Wrkfrq=Wrkspc(kk,:)*frac
 1035              Spectr(kk,:)=Wrkspc(kk,:) + Wrkfrq - Tmpfrq
 1036              Tmpfrq=Wrkfrq
 1037           ENDDO
 1038         ENDIF
 1039              
 1040 ! Spectral turning completed
 1041                             
 1042        RETURN
 1043        END SUBROUTINE W3ACTURN
 1044 !
 1045 !Li  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
 
                                                         w3servmd.F90  page  20
 
 
 1046 !Li
 1047 !Li  Merged UM source code for rotated grid, consisting the following
 1048 !Li  original subroutines in UM 6.1
 1049 !Li    LLTOEQ1A  WCOEFF1A  and  LBCROTWINDS1
 1050 !Li  The last subroutine is modified to process only one level winds
 1051 !Li  cpp directives are removed and required header C_Pi.h inserted.
 1052 !Li         Jian-Guo Li     26 May 2005
 1053 !Li
 1054 !Li  The WCOEFF1A subroutine is merged into LLTOEQ to reduce repetition
 1055 !Li  of the same calculations. Subroutine interface changed to
 1056 !Li  LLTOEQANGLE
 1057 !Li         Jian-GUo Li     23 Aug 2005
 1058 !Li
 1059 !Li  Subroutine W3LLTOEQ   --------------------------------------------
 1060 !Li
 1061 !Li  Purpose:  Calculates latitude and longitude on equatorial
 1062 !Li            latitude-longitude (eq) grid used in regional
 1063 !Li            models from input arrays of latitude and
 1064 !Li            longitude on standard grid. Both input and output
 1065 !Li            latitudes and longitudes are in degrees.
 1066 !Li            Also calculate rotation angle in degree to tranform
 1067 !Li            standard wind velocity into equatorial wind.
 1068 !Li            Valid for 0<PHI_POLE<90 or new pole in N. hemisphere.
 1069 !Li
 1070 !* Arguments:--------------------------------------------------------
 1071       SUBROUTINE W3LLTOEQ ( PHI, LAMBDA, PHI_EQ, LAMBDA_EQ,     &
 1072      &                 ANGLED, PHI_POLE, LAMBDA_POLE, POINTS )
 1073                                                               
 1074       IMPLICIT NONE
 1075                    
 1076       INTEGER:: POINTS    !IN  Number of points to be processed
 1077                                                                
 1078       REAL :: PHI_POLE,  & !IN  Latitude of equatorial lat-lon pole
 1079      &        LAMBDA_POLE  !INOUT  Longitude of equatorial lat-lon pole
 1080                                                                        
 1081       REAL, DIMENSION(POINTS) ::         &
 1082      &        PHI,       & !IN  Latitude
 1083      &        LAMBDA,    & !IN  Longitude
 1084      &        ANGLED,    & !OUT turning angle in deg for standard wind
 1085      &        LAMBDA_EQ, & !OUT Longitude in equatorial lat-lon coords
 1086      &        PHI_EQ       !OUT Latitude in equatorial lat-lon coords
 1087                                                                      
 1088 ! Define local varables:-----------------------------------------------
 1089       REAL(KIND=8) :: A_LAMBDA, A_PHI, E_LAMBDA, E_PHI,                 &
 1090                       SIN_PHI_POLE, COS_PHI_POLE,                       &
 1091                       TERM1, TERM2, ARG, LAMBDA_ZERO, LAMBDA_POLE_KEEP
 1092       INTEGER      :: I
 1093                        
 1094       REAL(KIND=8), PARAMETER :: SMALL=1.0E-6
 1095                                              
 1096       ! Double precision versions of values in constants.ftn:
 1097       REAL(KIND=8), PARAMETER         :: PI = 3.141592653589793
 1098       REAL(KIND=8), PARAMETER         :: RECIP_PI_OVER_180 = 180. / PI
 1099       REAL(KIND=8), PARAMETER         :: PI_OVER_180   = PI / 180.
 1100                                                                   
 
 
                                                         w3servmd.F90  page  21
 
 
 1101 !*----------------------------------------------------------------------
 1102                                                                         
 1103 ! 1. Initialise local constants
 1104 ! Scale lambda pole to range -180 to 180 degs
 1105       LAMBDA_POLE_KEEP=LAMBDA_POLE
 1106       IF (LAMBDA_POLE.LE.-180.0) LAMBDA_POLE=LAMBDA_POLE+360.D0
 1107       IF (LAMBDA_POLE.GT. 180.0) LAMBDA_POLE=LAMBDA_POLE-360.D0
 1108                                                                
 1109 ! Latitude of zeroth meridian
 1110       LAMBDA_ZERO=LAMBDA_POLE+180.D0
 1111 ! Sine and cosine of latitude of eq pole
 1112       IF (PHI_POLE >= 0.0) THEN
 1113         SIN_PHI_POLE =  SIN(PI_OVER_180*PHI_POLE)
 1114         COS_PHI_POLE =  COS(PI_OVER_180*PHI_POLE)
 1115       ELSE
 1116         SIN_PHI_POLE = -SIN(PI_OVER_180*PHI_POLE)
 1117         COS_PHI_POLE = -COS(PI_OVER_180*PHI_POLE)
 1118       ENDIF
 1119            
 1120 ! 2. Transform from standard to equatorial latitude-longitude
 1121                                                              
 1122       DO I= 1, POINTS
 1123                      
 1124 ! Scale longitude to range -180 to +180 degs
 1125                                             
 1126         A_LAMBDA=LAMBDA(I)-LAMBDA_ZERO
 1127         IF(A_LAMBDA.GT. 180.0) A_LAMBDA=A_LAMBDA-360.D0
 1128         IF(A_LAMBDA.LE.-180.0) A_LAMBDA=A_LAMBDA+360.D0
 1129                                                        
 1130 ! Convert latitude & longitude to radians
 1131                                          
 1132         A_LAMBDA=PI_OVER_180*A_LAMBDA
 1133         A_PHI=PI_OVER_180*PHI(I)
 1134                                 
 1135 ! Compute eq latitude using equation (4.4)
 1136                                           
 1137         ARG=-COS_PHI_POLE*COS(A_PHI)*COS(A_LAMBDA)   &
 1138        &    +SIN_PHI_POLE*SIN(A_PHI)
 1139         ARG=MIN(ARG, 1.D0)
 1140         ARG=MAX(ARG,-1.D0)
 1141         E_PHI=ASIN(ARG)
 1142         PHI_EQ(I)=RECIP_PI_OVER_180*E_PHI
 1143                                          
 1144 ! Compute eq longitude using equation (4.6)
 1145                                            
 1146         TERM1 = SIN_PHI_POLE*COS(A_PHI)*COS(A_LAMBDA)   &
 1147        &       +COS_PHI_POLE*SIN(A_PHI)
 1148         TERM2 = COS(E_PHI)
 1149         IF(TERM2 .LT. SMALL) THEN
 1150           E_LAMBDA=0.D0
 1151         ELSE
 1152           ARG=TERM1/TERM2
 1153           ARG=MIN(ARG, 1.D0)
 1154           ARG=MAX(ARG,-1.D0)
 1155           E_LAMBDA=RECIP_PI_OVER_180*ACOS(ARG)
 
 
                                                         w3servmd.F90  page  22
 
 
 1156           E_LAMBDA=SIGN(E_LAMBDA,A_LAMBDA)
 1157         ENDIF
 1158              
 1159 ! Scale longitude to range 0 to 360 degs
 1160                                         
 1161         IF(E_LAMBDA.GE.360.0) E_LAMBDA=E_LAMBDA-360.D0
 1162         IF(E_LAMBDA.LT.  0.0) E_LAMBDA=E_LAMBDA+360.D0
 1163         LAMBDA_EQ(I)=E_LAMBDA
 1164                              
 1165 !Li  Calculate turning angle for standard wind velocity
 1166                                                        
 1167         E_LAMBDA=PI_OVER_180*LAMBDA_EQ(I)
 1168                                          
 1169 ! Formulae used are from eqs (4.19) and (4.21)
 1170                                               
 1171         TERM2=SIN(E_LAMBDA)
 1172         ARG= SIN(A_LAMBDA)*TERM2*SIN_PHI_POLE      &
 1173        &    +COS(A_LAMBDA)*COS(E_LAMBDA)
 1174         ARG=MIN(ARG, 1.D0)
 1175         ARG=MAX(ARG,-1.D0)
 1176         TERM1=RECIP_PI_OVER_180*ACOS(ARG)
 1177         ANGLED(I)=SIGN(TERM1,TERM2)
 1178 !Li
 1179    
 1180       ENDDO
 1181            
 1182 ! Reset Lambda pole to the setting on entry to subroutine
 1183       LAMBDA_POLE=LAMBDA_POLE_KEEP
 1184                                   
 1185       RETURN
 1186       END SUBROUTINE W3LLTOEQ
 1187 !
 1188 !Li  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 1189 !Li
 1190 !Li  Merged UM source code for rotated grid, consiting the following
 1191 !Li  original subroutines in UM 6.1
 1192 !Li    EQTOLL1A  WCOEFF1A  and  LBCROTWINDS1
 1193 !Li  The last subroutine is modified to process only one level winds
 1194 !Li  cpp directives are removed and required header C_Pi.h inserted.
 1195 !Li         Jian-Guo Li     26 May 2005
 1196 !Li
 1197 !Li  The WCOEFF1A subroutine is merged into EQTOLL to reduce repetition
 1198 !Li  of the same calculations. Subroutine interface changed to
 1199 !Li  EQTOLLANGLE
 1200 !Li  First created:   Jian-GUo Li     23 Aug 2005
 1201 !Li  Last modified:   Jian-GUo Li     25 Feb 2008
 1202 !Li
 1203 !Li  Subroutine W3EQTOLL  --------------------------------------------
 1204 !Li
 1205 !Li  Purpose:  Calculates latitude and longitude on standard grid
 1206 !Li            from input arrays of latitude and longitude on
 1207 !Li            equatorial latitude-longitude (eq) grid used
 1208 !Li            in regional models. Both input and output latitudes
 1209 !Li            and longitudes are in degrees.
 1210 !Li            Also calculate rotation angle in degree to tranform
 
 
                                                         w3servmd.F90  page  23
 
 
 1211 !Li            standard wind velocity into equatorial wind.
 1212 !Li            Valid for 0<PHI_POLE<90 or new pole in N. hemisphere.
 1213 !Li
 1214 !Li  Arguments:--------------------------------------------------------
 1215                                                                        
 1216       SUBROUTINE W3EQTOLL( PHI_EQ, LAMBDA_EQ, PHI, LAMBDA,   &
 1217      &                 ANGLED, PHI_POLE, LAMBDA_POLE, POINTS )
 1218                                                               
 1219       IMPLICIT NONE
 1220                    
 1221       INTEGER:: POINTS      !IN  Number of points to be processed
 1222                                                                  
 1223       REAL :: PHI_POLE,   & !IN  Latitude of equatorial lat-lon pole
 1224      &        LAMBDA_POLE   !IN  Longitude of equatorial lat-lon pole
 1225                                                                      
 1226       REAL, DIMENSION(POINTS) ::         &
 1227      &        PHI,       & !OUT Latitude
 1228      &        LAMBDA,    & !OUT Longitude (0 =< LON < 360)
 1229      &        ANGLED,    & !OUT turning angle in deg for standard wind
 1230      &        LAMBDA_EQ, & !IN  Longitude in equatorial lat-lon coords
 1231      &        PHI_EQ       !IN  Latitude in equatorial lat-lon coords
 1232                                                                      
 1233 ! Local varables:------------------------------------------------------
 1234       REAL(KIND=8) :: E_LAMBDA, E_PHI, A_LAMBDA, A_PHI,                 &
 1235                       SIN_PHI_POLE, COS_PHI_POLE,                       &
 1236                       TERM1, TERM2, ARG, LAMBDA_ZERO
 1237       INTEGER :: I
 1238                   
 1239       REAL(KIND=8), PARAMETER :: SMALL=1.0E-6
 1240                                              
 1241       ! Double precision versions of values in constants.ftn:
 1242       REAL(KIND=8), PARAMETER         :: PI = 3.141592653589793
 1243       REAL(KIND=8), PARAMETER         :: RECIP_PI_OVER_180 = 180. / PI
 1244       REAL(KIND=8), PARAMETER         :: PI_OVER_180   = PI / 180.
 1245                                                                   
 1246 ! ----------------------------------------------------------------------
 1247                                                                         
 1248 ! 1. Initialise local constants
 1249                                
 1250 ! Latitude of zeroth meridian
 1251       LAMBDA_ZERO=LAMBDA_POLE+180.D0
 1252 ! Sine and cosine of latitude of eq pole
 1253       IF (PHI_POLE >= 0.0) THEN
 1254         SIN_PHI_POLE =  SIN(PI_OVER_180*PHI_POLE)
 1255         COS_PHI_POLE =  COS(PI_OVER_180*PHI_POLE)
 1256       ELSE
 1257         SIN_PHI_POLE = -SIN(PI_OVER_180*PHI_POLE)
 1258         COS_PHI_POLE = -COS(PI_OVER_180*PHI_POLE)
 1259       ENDIF
 1260            
 1261 ! 2. Transform from equatorial to standard latitude-longitude
 1262                                                              
 1263       DO I= 1, POINTS
 1264                      
 1265 ! Scale eq longitude to range -180 to +180 degs
 
 
                                                         w3servmd.F90  page  24
 
 
 1266                                                
 1267         E_LAMBDA=LAMBDA_EQ(I)
 1268         IF(E_LAMBDA.GT. 180.0) E_LAMBDA=E_LAMBDA-360.D0
 1269         IF(E_LAMBDA.LT.-180.0) E_LAMBDA=E_LAMBDA+360.D0
 1270                                                        
 1271 ! Convert eq latitude & longitude to radians
 1272                                             
 1273         E_LAMBDA=PI_OVER_180*E_LAMBDA
 1274         E_PHI=PI_OVER_180*PHI_EQ(I)
 1275                                    
 1276 ! Compute latitude using equation (4.7)
 1277                                        
 1278         ARG=COS_PHI_POLE*COS(E_PHI)*COS(E_LAMBDA)    &
 1279        &   +SIN_PHI_POLE*SIN(E_PHI)
 1280         ARG=MIN(ARG, 1.D0)
 1281         ARG=MAX(ARG,-1.D0)
 1282         A_PHI=ASIN(ARG)
 1283         PHI(I)=RECIP_PI_OVER_180*A_PHI
 1284                                       
 1285 ! Compute longitude using equation (4.8)
 1286                                         
 1287         TERM1 = COS(E_PHI)*SIN_PHI_POLE*COS(E_LAMBDA)   &
 1288        &       -SIN(E_PHI)*COS_PHI_POLE
 1289         TERM2 = COS(A_PHI)
 1290         IF(TERM2.LT.SMALL) THEN
 1291           A_LAMBDA=0.D0
 1292         ELSE
 1293           ARG=TERM1/TERM2
 1294           ARG=MIN(ARG, 1.D0)
 1295           ARG=MAX(ARG,-1.D0)
 1296           A_LAMBDA=RECIP_PI_OVER_180*ACOS(ARG)
 1297           A_LAMBDA=SIGN(A_LAMBDA,E_LAMBDA)
 1298           A_LAMBDA=A_LAMBDA+LAMBDA_ZERO
 1299         END IF
 1300               
 1301 ! Scale longitude to range 0 to 360 degs
 1302                                         
 1303         IF(A_LAMBDA.GE.360.0) A_LAMBDA=A_LAMBDA-360.D0
 1304         IF(A_LAMBDA.LT.  0.0) A_LAMBDA=A_LAMBDA+360.D0
 1305         LAMBDA(I)=A_LAMBDA
 1306                           
 1307 !Li  Calculate turning angle for standard wind velocity
 1308                                                        
 1309         A_LAMBDA=PI_OVER_180*(LAMBDA(I)-LAMBDA_ZERO)
 1310                                                     
 1311 ! Formulae used are from eqs (4.19) and (4.21)
 1312                                               
 1313         TERM2=SIN(E_LAMBDA)
 1314         ARG=SIN(A_LAMBDA)*TERM2*SIN_PHI_POLE     &
 1315        &           +COS(A_LAMBDA)*COS(E_LAMBDA)
 1316         ARG=MIN(ARG, 1.D0)
 1317         ARG=MAX(ARG,-1.D0)
 1318         TERM1=RECIP_PI_OVER_180*ACOS(ARG)
 1319         ANGLED(I)=SIGN(TERM1,TERM2)
 1320 !Li
 
 
                                                         w3servmd.F90  page  25
 
 
 1321    
 1322       ENDDO
 1323            
 1324       RETURN
 1325       END SUBROUTINE W3EQTOLL
 1326                              
 1327 !Li
 1328 !/ ------------------------------------------------------------------- /
 1329 !/ ------------------------------------------------------------------- /
 1330       SUBROUTINE W3THRTN ( NSEA, THETA, AnglD, Degrees )
 1331 !/
 1332 !/                  +-----------------------------------+
 1333 !/                  | WAVEWATCH III            NOAA/NMC |
 1334 !/                  |             A. Saulter            |
 1335 !/                  |                        FORTRAN 90 |
 1336 !/                  | Last update :         01-Mar-2018 |
 1337 !/                  +-----------------------------------+
 1338 !/
 1339 !/    01-Mar-2018 : Added subroutine                   ( version 6.02 )
 1340 !
 1341 !  1. Purpose :
 1342 !     Subroutine to de-rotate directions from rotated to standard pole
 1343 !     reference system
 1344 !
 1345 !  2. Method:
 1346 !   Rotates x,y vectors anticlockwise by angle alpha in radians
 1347 !
 1348 !/ ------------------------------------------------------------------- /
 1349       USE CONSTANTS, ONLY : DERA, TPI, UNDEF
 1350       IMPLICIT NONE
 1351 !
 1352 !/ ------------------------------------------------------------------- /
 1353 !/ Parameter list
 1354 !/
 1355       INTEGER, INTENT(IN) :: NSEA        ! Number of sea points
 1356       REAL,    INTENT(IN) :: AnglD(NSEA) ! Turning angle (degrees)
 1357       LOGICAL, INTENT(IN) :: Degrees     ! Use degrees or radians
 1358       REAL, INTENT(INOUT) :: THETA(NSEA) ! Direction seapoint array
 1359 !
 1360 !/ ------------------------------------------------------------------- /
 1361 !/ Local parameters
 1362 !/
 1363       INTEGER :: ISEA
 1364 !
 1365 !/ ------------------------------------------------------------------- /
 1366 ! Apply the rotation
 1367 !
 1368       DO ISEA=1, NSEA
 1369         IF ( THETA(ISEA) .NE. UNDEF ) THEN
 1370           IF ( Degrees ) THEN
 1371             THETA(ISEA) = THETA(ISEA) - AnglD(ISEA)
 1372             IF ( THETA(ISEA) .LT. 0 ) THETA(ISEA) = THETA(ISEA) + 360.0
 1373           ELSE
 1374             THETA(ISEA) = THETA(ISEA) - AnglD(ISEA)*DERA
 1375             IF ( THETA(ISEA) .LT. 0 ) THETA(ISEA) = THETA(ISEA) + TPI
 
 
                                                         w3servmd.F90  page  26
 
 
 1376           END IF
 1377         ENDIF
 1378       END DO
 1379             
 1380       RETURN
 1381       END SUBROUTINE W3THRTN
 1382 !
 1383 !/ ------------------------------------------------------------------- /
 1384 !/ ------------------------------------------------------------------- /
 1385       SUBROUTINE W3XYRTN ( NSEA, XVEC, YVEC, AnglD )
 1386 !/
 1387 !/                  +-----------------------------------+
 1388 !/                  | WAVEWATCH III            NOAA/NMC |
 1389 !/                  |             A. Saulter            |
 1390 !/                  |                        FORTRAN 90 |
 1391 !/                  | Last update :         01-Mar-2018 |
 1392 !/                  +-----------------------------------+
 1393 !/
 1394 !/    01-Mar-2018 : Added subroutine                   ( version 6.02 )
 1395 !
 1396 !  1. Purpose :
 1397 !     Subroutine to de-rotate x,y vectors from rotated to standard pole
 1398 !     reference system
 1399 !
 1400 !  2. Method:
 1401 !   Rotates x,y vectors anticlockwise by angle alpha in radians
 1402 !
 1403 !/ ------------------------------------------------------------------- /
 1404       USE CONSTANTS, ONLY : DERA, TPI, UNDEF
 1405       IMPLICIT NONE
 1406 !
 1407 !/ ------------------------------------------------------------------- /
 1408 !/ Parameter list
 1409 !/
 1410       INTEGER, INTENT(IN) :: NSEA        ! Number of sea points
 1411       REAL,    INTENT(IN) :: AnglD(NSEA) ! Turning angle (degrees)
 1412       REAL, INTENT(INOUT) :: XVEC(NSEA), YVEC(NSEA)
 1413 !
 1414 !/ ------------------------------------------------------------------- /
 1415 !/ Local parameters
 1416 !/
 1417       INTEGER :: ISEA
 1418       REAL    :: XVTMP, YVTMP
 1419 !
 1420 !/ ------------------------------------------------------------------- /
 1421 ! Apply the rotation
 1422 !
 1423       DO ISEA=1, NSEA
 1424         IF (( XVEC(ISEA) .NE. UNDEF ) .AND. &
 1425             ( YVEC(ISEA) .NE. UNDEF )) THEN
 1426           XVTMP = XVEC(ISEA)*COS(AnglD(ISEA)*DERA) + &
 1427                    YVEC(ISEA)*SIN(AnglD(ISEA)*DERA)
 1428           YVTMP = YVEC(ISEA)*COS(AnglD(ISEA)*DERA) - &
 1429                    XVEC(ISEA)*SIN(AnglD(ISEA)*DERA)
 1430           XVEC(ISEA) = XVTMP
 
 
                                                         w3servmd.F90  page  27
 
 
 1431           YVEC(ISEA) = YVTMP
 1432         END IF
 1433       END DO
 1434             
 1435       RETURN
 1436       END SUBROUTINE W3XYRTN
 1437 !
 1438 !/ ------------------------------------------------------------------- /
 1439 !/ ------------------------------------------------------------------- /
 1440 !/
 1441       SUBROUTINE STRSPLIT(STRING,TAB)
 1442 !/
 1443 !/                  +-----------------------------------+
 1444 !/                  | WAVEWATCH III           NOAA/NCEP |
 1445 !/                  |          M. Accensi               |
 1446 !/                  |                        FORTRAN 90 |
 1447 !/                  | Last update :         29-Apr-2013 !
 1448 !/                  +-----------------------------------+
 1449 !/
 1450 !/    29-Mar-2013 : Origination.                        ( version 4.10 )
 1451 !/
 1452 !  1. Purpose :
 1453 !
 1454 !     Splits string into words
 1455 !
 1456 !  2. Method :
 1457 !
 1458 !     finds spaces and loops
 1459 !
 1460 !  3. Parameters :
 1461 !
 1462 !     Parameter list
 1463 !     ----------------------------------------------------------------
 1464 !       STRING   Str   O   String to be splitted
 1465 !       TAB      Str   O   Array of strings
 1466 !     ----------------------------------------------------------------
 1467 !
 1468  
 1469       IMPLICIT NONE
 1470                    
 1471                    
 1472                    
 1473       CHARACTER(LEN=*), intent(IN)         :: STRING
 1474       CHARACTER(LEN=100), intent(INOUT)    :: TAB(*)
 1475       INTEGER                              :: cnt, I
 1476       CHARACTER(LEN=1024)                  :: tmp_str, ori_str
 1477                                                               
 1478 ! initializes arrays
 1479       ori_str=ADJUSTL(TRIM(STRING))
 1480       tmp_str=ori_str
 1481       cnt=0
 1482            
 1483 ! counts the number of substrings
 1484       DO WHILE ((INDEX(tmp_str,' ').NE.0) .AND. (len_trim(tmp_str).NE.0))
 1485         tmp_str=ADJUSTL(tmp_str(INDEX(tmp_str,' ')+1:))
 
 
                                                         w3servmd.F90  page  28
 
 
 1486         cnt=cnt+1
 1487         ENDDO
 1488 !
 1489 ! reinitializes arrays
 1490 !
 1491       tmp_str=ori_str
 1492 ! loops on each substring
 1493       DO I=1,cnt
 1494         TAB(I)=tmp_str(:INDEX(tmp_str,' '))
 1495         tmp_str=ADJUSTL(tmp_str(INDEX(tmp_str,' ')+1:))
 1496         END DO
 1497               
 1498       RETURN
 1499 !/
 1500 !/ End of STRSPLIT ----------------------------------------------------- /
 1501 !/
 1502       END SUBROUTINE STRSPLIT
 1503 !/
 1504   
 1505 !/ ------------------------------------------------------------------- /
 1506    SUBROUTINE STR_TO_UPPER(STR)
 1507      character(*), intent(inout) :: str
 1508      integer :: i
 1509                  
 1510      DO i = 1, len(str)
 1511        select case(str(i:i))
 1512          case("a":"z")
 1513            str(i:i) = achar(iachar(str(i:i))-32)
 1514        end select
 1515        END DO
 1516 !/ End of STR_TO_UPPER
 1517 !/ ------------------------------------------------------------------- /
 1518 END SUBROUTINE STR_TO_UPPER
 1519                            
 1520 !**********************************************************************
 1521 !*                                                                    *
 1522                                                                        
 1523 !*********************************************************************
 1524    SUBROUTINE DIAGONALIZE(a1,d,v,nrot)
 1525 !*********************************************************************
 1526    IMPLICIT NONE
 1527    INTEGER,                          INTENT(out)   :: nrot
 1528    DOUBLE PRECISION, DIMENSION(:)  , INTENT(OUT)   ::d
 1529    DOUBLE PRECISION, DIMENSION(:,:), INTENT(IN)    ::a1  ! Modified from >
 1530    DOUBLE PRECISION, DIMENSION(:,:), INTENT(OUT)   ::v
 1531                                                       
 1532    INTEGER    i,j,ip,iq,n
 1533    DOUBLE PRECISION       c,g,h,s,sm,t,tau,theta,tresh
 1534    DOUBLE PRECISION    , DIMENSION(size(d)) ::b,z
 1535    DOUBLE PRECISION, DIMENSION(size(d),size(d)) :: a
 1536    LOGICAL, DIMENSION(size(d),size(d)) :: upper_triangle
 1537                                                         
 1538    a=a1
 1539    n=size(d)
 1540    v(:,:)=0.
 
 
                                                         w3servmd.F90  page  29
 
 
 1541    upper_triangle(:,:)=.FALSE.
 1542    DO I=1,n
 1543       v(I,I)=1.
 1544       b(I)=a(I,I)
 1545       DO J=I+1,n
 1546          upper_triangle(I,J)=.TRUE.
 1547       ENDDO
 1548    ENDDO
 1549    d(:)=b(:)
 1550    z(:)=0.0
 1551    nrot=0
 1552       DO I=1,50
 1553       sm=SUM(ABS(a),mask=upper_triangle)
 1554       IF (sm.EQ.0.0) RETURN
 1555       tresh=merge(0.2*sm/n**2,0.0D0,i<4)
 1556       DO ip=1,n-1
 1557          do iq=ip+1,n
 1558             g=100.0*abs(a(ip,iq))
 1559             IF((i > 4).AND.(ABS(d(ip))+g.EQ.abs(d(ip))) &
 1560                .AND.(ABS(d(iq))+g.EQ.abs(d(iq)))) THEN
 1561                a(ip,iq)=0.0
 1562             ELSE IF (abs(a(ip,iq)) > tresh) THEN
 1563                h=d(iq)-d(ip)
 1564                if (abs(h)+g == abs(h)) THEN
 1565                   t=a(ip,iq)/h
 1566                ELSE
 1567                   theta=0.5*h/a(ip,iq)
 1568                   t=1.0/(abs(theta)+sqrt(1.0+theta**2))
 1569                   IF ( theta < 0.0) t=-t
 1570                ENDIF
 1571                c=1.0/sqrt(1+t**2)
 1572                s=t*c
 1573                tau=s/(1.0+c)
 1574                h=t*a(ip,iq)
 1575                z(ip)=z(ip)-h
 1576                z(iq)=z(iq)+h
 1577                d(ip)=d(ip)-h
 1578                d(iq)=d(iq)+h
 1579                a(ip,iq)=0.0
 1580                IF (ip.GE.1) CALL ROTATE(a(1:ip-1,ip),a(1:ip-1,iq))
 1581 !The IF test was added by F.A. (2005/04/04) because of the following erro>
 1582 !Subscript out of range. Location: line 593 column 36 of 'cb_botsc.f90'
 1583 !Subscript number 1 has value 0 in array 'A'
 1584                CALL ROTATE(a(ip,ip+1:iq-1),a(ip+1:iq-1,iq))
 1585                CALL ROTATE(a(ip,iq+1:n),a(iq,iq+1:n))
 1586                CALL ROTATE(v(:,ip),v(:,iq))
 1587                nrot=nrot+1
 1588             ENDIF
 1589          ENDDO
 1590       ENDDO
 1591       b(:)=b(:)+z(:)
 1592       d(:)=b(:)
 1593       z(:)=0.0
 1594    ENDDO
 1595    WRITE(6,*) 'Too many iterations in DIAGONALIZE'
 
 
                                                         w3servmd.F90  page  30
 
 
 1596    CONTAINS
 1597    SUBROUTINE ROTATE(X1,X2)
 1598    DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: X1,X2
 1599    DOUBLE PRECISION, DIMENSION(size(X1)) :: MEM
 1600    MEM(:)=X1(:)
 1601    X1(:)=X1(:)-s*(X2(:)+X1(:)*tau)
 1602    X2(:)=X2(:)+s*(MEM(:)-X2(:)*tau)
 1603    END SUBROUTINE ROTATE
 1604    END SUBROUTINE DIAGONALIZE
 1605                              
 1606 !/ ------------------------------------------------------------------- /
 1607       SUBROUTINE UV_TO_MAG_DIR(U, V, NSEA, MAG, DIR, TOLERANCE, CONV)
 1608 !/
 1609 !/                  +-----------------------------------+
 1610 !/                  | WAVEWATCH III           NOAA/NCEP |
 1611 !/                  |           C. Bunney               |
 1612 !/                  |                        FORTRAN 90 |
 1613 !/                  | Last update :         15-Jan-2021 |
 1614 !/                  +-----------------------------------+
 1615 !/
 1616 !/    15-Jan-2021 : Creation                            ( version 7.12 )
 1617 !/
 1618 !  1. Purpose :
 1619 !
 1620 !     Converts seapoint arrays formulated as U/V vectors into magnitude
 1621 !     and direction arrays.
 1622 !
 1623 !     If MAG and DIR input parameters are not specificed then the
 1624 !     conversion is performed in-place (U => MAG, v => DIR).
 1625 !
 1626 !  2. Parameters
 1627 !
 1628 !     Parameter list
 1629 !     ----------------------------------------------------------------
 1630 !       U/V       R.Arr  I  Array of U/V components
 1631 !       NSEA      Int    I  Number of sea points
 1632 !       MAG       R.Arr  O  Magnitude array            (Optional)
 1633 !       DIR       R.Arr  O  Direction array (degrees)  (Optional)
 1634 !       TOLERANCE Real   I  Minimum allowed magnitude  (Optional)
 1635 !       CONV      Char   I  Ouput direciton convention (Optional)
 1636 !     ----------------------------------------------------------------
 1637 !
 1638 !  3. Remarks
 1639 !
 1640 !     Optional CONV specifies direction convention. Must be one of:
 1641 !       'N'=Nautical     : North=0, clockwise, direction-from (default)
 1642 !       'O'=Oceangraphic : North=0, clockwise, direction-to
 1643 !       'C'=Cartesian    : North=90, counter-clockwise, direction-to
 1644 !
 1645 !/ ------------------------------------------------------------------- /
 1646       USE CONSTANTS, ONLY: RADE, UNDEF
 1647       IMPLICIT NONE
 1648                    
 1649       REAL, INTENT(INOUT)             :: U(NSEA), V(NSEA)
 1650       INTEGER, INTENT(IN)             :: NSEA
 
 
                                                         w3servmd.F90  page  31
 
 
 1651       REAL, INTENT(OUT), OPTIONAL     :: MAG(NSEA), DIR(NSEA)
 1652       REAL, INTENT(IN), OPTIONAL      :: TOLERANCE
 1653       CHARACTER, INTENT(IN), OPTIONAL :: CONV
 1654 !/ ------------------------------------------------------------------- /
 1655 !/ Local parameters
 1656 !
 1657       REAL :: TOL, SGN, OFFSET, TMP
 1658       CHARACTER :: DIRCONV
 1659       INTEGER :: ISEA
 1660       LOGICAL :: INPLACE
 1661                         
 1662       DIRCONV = 'N'
 1663       TOL = 1.0
 1664       INPLACE = .TRUE.
 1665       IF(PRESENT(TOLERANCE)) TOL = TOLERANCE
 1666       IF(PRESENT(CONV)) DIRCONV = CONV
 1667       IF(PRESENT(MAG) .AND. PRESENT(DIR)) INPLACE = .FALSE.
 1668                                                            
 1669       SELECT CASE (CONV)
 1670         CASE('N')
 1671           OFFSET = 630.
 1672           SGN = -1.
 1673         CASE('O')
 1674           OFFSET = 450.
 1675           SGN = -1.
 1676         CASE('C')
 1677           OFFSET = 360.
 1678           SGN = 1.
 1679         CASE DEFAULT
 1680           WRITE(*,*) "UV_TO_MAG_DIR: UNKNOWN DIR CONVENTION: ", DIRCONV
 1681           CALL EXTCDE(1)
 1682       END SELECT
 1683                 
 1684       IF(INPLACE) THEN
 1685         DO ISEA=1, NSEA
 1686           TMP = SQRT(U(ISEA)**2 + V(ISEA)**2)
 1687           IF(TMP .GE. TOL) THEN
 1688             V(ISEA) = MOD(OFFSET + (SGN * RADE * ATAN2(V(ISEA), U(ISEA)))>
 1689             U(ISEA) = TMP
 1690           ELSE
 1691             U(ISEA) = UNDEF
 1692             V(ISEA) = UNDEF
 1693           END IF
 1694         END DO
 1695       ELSE
 1696         DO ISEA=1, NSEA
 1697           MAG(ISEA) = SQRT(U(ISEA)**2 + V(ISEA)**2)
 1698           IF(MAG(ISEA) .GE. TOL) THEN
 1699             DIR(ISEA) = MOD(OFFSET + (SGN * RADE * ATAN2(V(ISEA), U(ISEA)>
 1700           ELSE
 1701             MAG(ISEA) = UNDEF
 1702             DIR(ISEA) = UNDEF
 1703           END IF
 1704         END DO
 1705       ENDIF
 
 
                                                         w3servmd.F90  page  32
 
 
 1706            
 1707      END SUBROUTINE UV_TO_MAG_DIR
 1708 !/
 1709 !/ End of module W3SERVMD -------------------------------------------- /
 1710 !/
 1711       END MODULE W3SERVMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
