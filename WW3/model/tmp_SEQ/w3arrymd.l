                                                         w3arrymd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3ARRYMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         30-Oct-2009 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    Copyright 2009 National Weather Service (NWS),
   13 !/       National Oceanic and Atmospheric Administration.  All rights
   14 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   15 !/       No unauthorized use without permission.
   16 !/
   17 !  1. Purpose :
   18 !
   19 !     In this module all service routines for in and output (binary
   20 !     and test) of arrays are gathered.
   21 !
   22 !  2. Variables and types :
   23 !
   24 !      Name      Type  Scope    Description
   25 !     ----------------------------------------------------------------
   26 !      ICOL      Int.  Private  Number of collums four array output
   27 !                               (if not 80, 132 assumed).
   28 !      NFRMAX    Int.  Private  Max number of frequencies in 1D
   29 !                               print plots of spectra.
   30 !     ----------------------------------------------------------------
   31 !
   32 !  3. Subroutines and functions :
   33 !
   34 !      Name      Type  Scope    Description
   35 !     ----------------------------------------------------------------
   36 !      INA2R     Subr. Public   Read 2D real array.
   37 !      INA2I     Subr. Public   Read 2D integer array.
   38 !      OUTA2R    Subr. Public   Write 2D real array.
   39 !      OUTA2I    Subr. Public   Write 2D integer array.
   40 !      OUTREA    Subr. Public   Print out 1D real array.
   41 !      OUTINT    Subr. Public   Print out 1D integer array.
   42 !      OUTMAT    Subr. Public   Print out 2D real array.
   43 !      PRTBLK    Subr. Public   Print a block-type table of a 2D
   44 !                               real array.
   45 !      PRT1DS    Subr. Public   Print plot of 1D spectrum.
   46 !      PRT1DM    Subr. Public   Print plot of 1D spectra.
   47 !      PRT2DS    Subr. Public   Print plot of 2D spectrum.
   48 !      ANGSTR    Subr. PRT2DS   Convert direction to string.
   49 !     ----------------------------------------------------------------
   50 !
   51 !  4. Subroutines and functions used :
   52 !
   53 !      Name      Type  Module   Description
   54 !     ----------------------------------------------------------------
   55 !      STRACE    Subr. W3SERVMD Subroutine tracing.            ( !/S )
 
 
                                                         w3arrymd.F90  page   2
 
 
   56 !     ----------------------------------------------------------------
   57 !
   58 !  5. Remarks :
   59 !
   60 !  6. Switches :
   61 !
   62 !       !/S    Enable subroutine tracing troughout module.
   63 !       !/T    Switch on test output for INA2R/I and OUTA2R/I.
   64 !
   65 !  7. Source code :
   66 !
   67 !/ ------------------------------------------------------------------- /
   68       PUBLIC
   69 !
   70       INTEGER, PARAMETER, PRIVATE :: ICOL   = 80
   71       INTEGER, PARAMETER, PRIVATE :: NFRMAX = 50
   72       INTEGER, PARAMETER, PRIVATE :: NFM2   = NFRMAX+1
   73 !
   74       CONTAINS
   75 !/ ------------------------------------------------------------------- /
   76       SUBROUTINE INA2R  (ARRAY, MX, MY, LX, HX, LY, HY,              &
   77                          NDS, NDST, NDSE, IDFM, RFORM, IDLA, VSC, VOF)
   78 !/
   79 !/                  +-----------------------------------+
   80 !/                  | WAVEWATCH III           NOAA/NCEP |
   81 !/                  |           H. L. Tolman            |
   82 !/                  |                        FORTRAN 90 |
   83 !/                  | Last update :         30-Oct-2009 |
   84 !/                  +-----------------------------------+
   85 !/                                  Based on INAR2D by N.Booij, DUT.
   86 !/
   87 !/    31-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
   88 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
   89 !/    30-Oct-2009 : Implement add offset argument.      ( version 3.14 )
   90 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   91 !/    20-Jan-2017 : Add error exit using EXTCDE.        ( version 6.02 )
   92 !/
   93 !  1. Purpose :
   94 !
   95 !     Reads 2-D array of pre-described layout and format.
   96 !
   97 !  3. Parameter list
   98 !     ----------------------------------------------------------------
   99 !       ARRAY   R.A.   O   Array to be read.
  100 !       MX,MY   Int.   I   Declared size of array.
  101 !       LX,HX   Int.   I   Range of x-counters to be read.
  102 !       LY,HY   Int.   I   Range of y-counters to be read.
  103 !       NDS     Int.   I   Unit number for dataset with array.
  104 !       NDST    Int.   I   Unit number for test output.
  105 !       NDSE    Int.   I   Unit number for error messages.
  106 !       IDFM    Int.   I   Format indicator.
  107 !                           IDFM = 1 : Free format.
  108 !                           IDFM = 2 : Fixed format RFORM.
  109 !                           IDFM = 3 : Unformatted.
  110 !       RFORM   C*(*)  I   Format, if IDFM = 2
 
 
                                                         w3arrymd.F90  page   3
 
 
  111 !       IDLA    Int.   I   Lay out indicator.
  112 !                           IDLA = 1 : Read for IY=LY-HY, IX=LX-HX,
  113 !                                      IX line by IX line.
  114 !                           IDLA = 2 : Idem, one read statement.
  115 !                           IDLA = 3 : Read for IY=HY-LY, IX=LX,HX,
  116 !                                      IX line by IX line.
  117 !                           IDLA = 4 : Idem, one read statement.
  118 !       VSC     Real   I   Scaling factor (multiplication).
  119 !       VOF     Real   I   Add offset.
  120 !     ----------------------------------------------------------------
  121 !
  122 !  4. Subroutines used :
  123 !
  124 !     See mudule documentation.
  125 !
  126 !  5. Called by :
  127 !
  128 !     Any.
  129 !
  130 !  6. Error messages :
  131 !
  132 !     See error escape locations at end of routine.
  133 !
  134 !  8. Structure :
  135 !
  136 !     See comments in code.
  137 !
  138 !  9. Switches :
  139 !
  140 !     !/S   Enable subroutine tracing.
  141 !     !/T   Dump of input parameters in parameter list.
  142 !
  143 ! 10. Source code :
  144 !
  145 !/ ------------------------------------------------------------------- /
  146 !/
  147       USE W3SERVMD, ONLY: EXTCDE
  148 !
  149       IMPLICIT NONE
  150 !/
  151 !/ ------------------------------------------------------------------- /
  152 !/ Parameter list
  153 !/
  154       INTEGER, INTENT(IN)     :: MX, MY, LX, HX, LY, HY, NDS, NDST,  &
  155                                  NDSE, IDFM, IDLA
  156       REAL, INTENT(IN)        :: VSC, VOF
  157       CHARACTER, INTENT(IN)   :: RFORM*(*)
  158       REAL, INTENT(OUT)       :: ARRAY(MX,MY)
  159 !/
  160 !/ ------------------------------------------------------------------- /
  161 !/ Local parameters
  162 !/
  163       INTEGER                 :: IIDFM, IIDLA, IX, IY, ISTAT
  164 !/
  165 !/ ------------------------------------------------------------------- /
 
 
                                                         w3arrymd.F90  page   4
 
 
  166 !/
  167 !
  168       IF (IDFM.LT.1 .OR. IDFM.GT.3) THEN
  169           IIDFM = 1
  170         ELSE
  171           IIDFM = IDFM
  172         END IF
  173       IF (IDLA.LT.1 .OR. IDLA.GT.4) THEN
  174           IIDLA = 1
  175         ELSE
  176           IIDLA = IDLA
  177         END IF
  178 !
  179 ! Free format read :
  180 !
  181       IF (IIDFM.EQ.1) THEN
  182           IF (IIDLA.EQ.1) THEN
  183                 DO IY=LY, HY
  184                   READ (NDS,*,END=800,ERR=801,IOSTAT=ISTAT)     &
  185                        (ARRAY(IX,IY),IX=LX,HX)
  186                   END DO
  187             ELSE IF (IIDLA.EQ.2) THEN
  188                   READ (NDS,*,END=800,ERR=801,IOSTAT=ISTAT)     &
  189                        ((ARRAY(IX,IY),IX=LX,HX),IY=LY,HY)
  190             ELSE IF (IIDLA.EQ.3) THEN
  191                 DO IY=HY, LY, -1
  192                   READ (NDS,*,END=800,ERR=801,IOSTAT=ISTAT)     &
  193                        (ARRAY(IX,IY),IX=LX,HX)
  194                   END DO
  195             ELSE
  196                   READ (NDS,*,END=800,ERR=801,IOSTAT=ISTAT)     &
  197                        ((ARRAY(IX,IY),IX=LX,HX),IY=HY,LY,-1)
  198             END IF
  199 !
  200 ! Fixed format read :
  201 !
  202         ELSE IF (IIDFM.EQ.2) THEN
  203           IF (IIDLA.EQ.1) THEN
  204                 DO IY=LY, HY
  205                   READ (NDS,RFORM,END=800,ERR=801,IOSTAT=ISTAT) &
  206                        (ARRAY(IX,IY),IX=LX,HX)
  207                   END DO
  208             ELSE IF (IIDLA.EQ.2) THEN
  209                   READ (NDS,RFORM,END=800,ERR=801,IOSTAT=ISTAT) &
  210                        ((ARRAY(IX,IY),IX=LX,HX),IY=LY,HY)
  211             ELSE IF (IIDLA.EQ.3) THEN
  212                 DO IY=HY, LY, -1
  213                   READ (NDS,RFORM,END=800,ERR=801,IOSTAT=ISTAT) &
  214                        (ARRAY(IX,IY),IX=LX,HX)
  215                   END DO
  216             ELSE
  217                   READ (NDS,RFORM,END=800,ERR=801,IOSTAT=ISTAT) &
  218                        ((ARRAY(IX,IY),IX=LX,HX),IY=HY,LY,-1)
  219             END IF
  220 !
 
 
                                                         w3arrymd.F90  page   5
 
 
  221 ! Unformat read :
  222 !
  223         ELSE
  224           IF (IIDLA.EQ.1) THEN
  225                 DO IY=LY, HY
  226                   READ (NDS,END=800,ERR=801,IOSTAT=ISTAT)       &
  227                        (ARRAY(IX,IY),IX=LX,HX)
  228                   END DO
  229             ELSE IF (IIDLA.EQ.2) THEN
  230                   READ (NDS,END=800,ERR=801,IOSTAT=ISTAT)       &
  231                        ((ARRAY(IX,IY),IX=LX,HX),IY=LY,HY)
  232             ELSE IF (IIDLA.EQ.3) THEN
  233                 DO IY=HY, LY, -1
  234                   READ (NDS,END=800,ERR=801,IOSTAT=ISTAT)       &
  235                        (ARRAY(IX,IY),IX=LX,HX)
  236                   END DO
  237             ELSE
  238                   READ (NDS,END=800,ERR=801,IOSTAT=ISTAT)       &
  239                        ((ARRAY(IX,IY),IX=LX,HX),IY=HY,LY,-1)
  240             END IF
  241         END IF
  242 !
  243 ! Scaling :
  244 !
  245       DO IX=LX, HX
  246         DO IY=LY, HY
  247           ARRAY(IX,IY) = VSC * ARRAY(IX,IY) + VOF
  248           END DO
  249         END DO
  250 !
  251       RETURN
  252 !
  253 ! Escape locations read errors :
  254 !
  255   800 CONTINUE
  256       WRITE (NDSE,900)
  257       CALL EXTCDE ( ISTAT )
  258 !
  259   801 CONTINUE
  260       WRITE (NDSE,901) ISTAT
  261       CALL EXTCDE ( ISTAT )
  262 !
  263 ! Formats
  264 !
  265   900 FORMAT (/' *** ERROR INA2R : '/                           &
  266                '     PREMATURE END OF FILE'/)
  267   901 FORMAT (/' *** ERROR INA2R : '/                           &
  268                '     ERROR IN READING FROM FILE'/               &
  269                '     IOSTAT =',I5/)
  270 !
  271 !/
  272 !/ End of INA2R  ----------------------------------------------------- /
  273 !/
  274       END SUBROUTINE INA2R
  275 !/ ------------------------------------------------------------------- /
 
 
                                                         w3arrymd.F90  page   6
 
 
  276       SUBROUTINE INA2I  (ARRAY, MX, MY, LX, HX, LY, HY,              &
  277                          NDS, NDST, NDSE, IDFM, RFORM, IDLA, VSC, VOF)
  278 !/
  279 !/                  +-----------------------------------+
  280 !/                  | WAVEWATCH III           NOAA/NCEP |
  281 !/                  |           H. L. Tolman            |
  282 !/                  |                        FORTRAN 90 |
  283 !/                  | Last update :         30-Oct-2009 |
  284 !/                  +-----------------------------------+
  285 !/                                  Based on INAR2D by N.Booij, DUT.
  286 !/
  287 !/    31-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
  288 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  289 !/    30-Oct-2009 : Implement add offset argument.      ( version 3.14 )
  290 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  291 !/    20-Jan-2017 : Add error exit using EXTCDE.        ( version 6.02 )
  292 !/
  293 !  1. Purpose :
  294 !
  295 !     Like INA2R , integer ARRAY, VSC and VOF, see INA2R .
  296 !
  297 ! 10. Source code :
  298 !
  299 !/ ------------------------------------------------------------------- /
  300 !/
  301       USE W3SERVMD, ONLY: EXTCDE
  302 !
  303       IMPLICIT NONE
  304 !/
  305 !/ ------------------------------------------------------------------- /
  306 !/ Parameter list
  307 !/
  308       INTEGER, INTENT(IN)     :: MX, MY, LX, HX, LY, HY, NDS, NDST,  &
  309                                  NDSE, IDFM, IDLA, VSC, VOF
  310       INTEGER, INTENT(OUT)    :: ARRAY(MX,MY)
  311       CHARACTER, INTENT(IN)   :: RFORM*(*)
  312 !/
  313 !/ ------------------------------------------------------------------- /
  314 !/ Local parameters
  315 !/
  316       INTEGER                 :: IIDFM, IIDLA, IX, IY, ISTAT
  317 !/
  318 !/ ------------------------------------------------------------------- /
  319 !/
  320 !
  321       IF (IDFM.LT.1 .OR. IDFM.GT.3) THEN
  322           IIDFM = 1
  323         ELSE
  324           IIDFM = IDFM
  325         END IF
  326       IF (IDLA.LT.1 .OR. IDLA.GT.4)THEN
  327           IIDLA = 1
  328         ELSE
  329           IIDLA = IDLA
  330         END IF
 
 
                                                         w3arrymd.F90  page   7
 
 
  331 !
  332 ! Free format read :
  333 !
  334       IF (IIDFM.EQ.1) THEN
  335           IF (IIDLA.EQ.1) THEN
  336                 DO IY=LY, HY
  337                   READ (NDS,*,END=800,ERR=801,IOSTAT=ISTAT)     &
  338                        (ARRAY(IX,IY),IX=LX,HX)
  339                   END DO
  340             ELSE IF (IIDLA.EQ.2) THEN
  341                   READ (NDS,*,END=800,ERR=801,IOSTAT=ISTAT)     &
  342                        ((ARRAY(IX,IY),IX=LX,HX),IY=LY,HY)
  343             ELSE IF (IIDLA.EQ.3) THEN
  344                 DO IY=HY, LY, -1
  345                   READ (NDS,*,END=800,ERR=801,IOSTAT=ISTAT)     &
  346                        (ARRAY(IX,IY),IX=LX,HX)
  347                   END DO
  348             ELSE
  349                   READ (NDS,*,END=800,ERR=801,IOSTAT=ISTAT)     &
  350                        ((ARRAY(IX,IY),IX=LX,HX),IY=HY,LY,-1)
  351             END IF
  352 !
  353 ! Fixed format read :
  354 !
  355         ELSE IF (IIDFM.EQ.2) THEN
  356           IF (IIDLA.EQ.1) THEN
  357                 DO IY=LY, HY
  358                   READ (NDS,RFORM,END=800,ERR=801,IOSTAT=ISTAT) &
  359                        (ARRAY(IX,IY),IX=LX,HX)
  360                   END DO
  361             ELSE IF (IIDLA.EQ.2) THEN
  362                   READ (NDS,RFORM,END=800,ERR=801,IOSTAT=ISTAT) &
  363                        ((ARRAY(IX,IY),IX=LX,HX),IY=LY,HY)
  364             ELSE IF (IIDLA.EQ.3) THEN
  365                 DO IY=HY, LY, -1
  366                   READ (NDS,RFORM,END=800,ERR=801,IOSTAT=ISTAT) &
  367                        (ARRAY(IX,IY),IX=LX,HX)
  368                   END DO
  369             ELSE
  370                   READ (NDS,RFORM,END=800,ERR=801,IOSTAT=ISTAT) &
  371                        ((ARRAY(IX,IY),IX=LX,HX),IY=HY,LY,-1)
  372             END IF
  373 !
  374 ! Unformat read :
  375 !
  376         ELSE
  377           IF (IIDLA.EQ.1) THEN
  378                 DO IY=LY, HY
  379                   READ (NDS,END=800,ERR=801,IOSTAT=ISTAT)       &
  380                        (ARRAY(IX,IY),IX=LX,HX)
  381                   END DO
  382             ELSE IF (IIDLA.EQ.2) THEN
  383                   READ (NDS,END=800,ERR=801,IOSTAT=ISTAT)       &
  384                        ((ARRAY(IX,IY),IX=LX,HX),IY=LY,HY)
  385             ELSE IF (IIDLA.EQ.3) THEN
 
 
                                                         w3arrymd.F90  page   8
 
 
  386                 DO IY=HY, LY, -1
  387                   READ (NDS,END=800,ERR=801,IOSTAT=ISTAT)       &
  388                        (ARRAY(IX,IY),IX=LX,HX)
  389                   END DO
  390             ELSE
  391                   READ (NDS,END=800,ERR=801,IOSTAT=ISTAT)       &
  392                        ((ARRAY(IX,IY),IX=LX,HX),IY=HY,LY,-1)
  393             END IF
  394         END IF
  395 !
  396 ! Scaling :
  397 !
  398       DO IX=LX, HX
  399         DO IY=LY, HY
  400           ARRAY(IX,IY) = VSC * ARRAY(IX,IY) + VOF
  401           END DO
  402         END DO
  403 !
  404       RETURN
  405 !
  406 ! Escape locations read errors :
  407 !
  408   800 CONTINUE
  409       WRITE (NDSE,900)
  410       CALL EXTCDE ( ISTAT )
  411 !
  412   801 CONTINUE
  413       WRITE (NDSE,901) ISTAT
  414       CALL EXTCDE ( ISTAT )
  415 !
  416 ! Formats
  417 !
  418   900 FORMAT (/' *** ERROR INA2I : '/                           &
  419                '     PREMATURE END OF FILE'/)
  420   901 FORMAT (/' *** ERROR INA2I : '/                           &
  421                '     ERROR IN READING FROM FILE'/               &
  422                '     IOSTAT =',I5/)
  423 !
  424 !/
  425 !/ End of INA2I  ----------------------------------------------------- /
  426 !/
  427       END SUBROUTINE INA2I
  428 !/ ------------------------------------------------------------------- /
  429       SUBROUTINE OUTA2R (ARRAY, MX, MY, LX, HX, LY, HY,               &
  430                          NDS, NDST, NDSE, IDFM, RFORM, IDLA, VSC, VOF)
  431 !/
  432 !/                  +-----------------------------------+
  433 !/                  | WAVEWATCH III           NOAA/NCEP |
  434 !/                  |           H. L. Tolman            |
  435 !/                  |                        FORTRAN 90 |
  436 !/                  | Last update :         30-Oct-2009 |
  437 !/                  +-----------------------------------+
  438 !/
  439 !/    31-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
  440 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
 
 
                                                         w3arrymd.F90  page   9
 
 
  441 !/    21-Feb-2008 ; Bug fix IDFM=1, IDLA=2 writing      ( version 3.13 )
  442 !/    30-Oct-2009 ; Fix non-integer loop bound.         ( version 3.14 )
  443 !/                  (T. J. Campbell, NRL)
  444 !/    30-Oct-2009 : Implement add offset argument.      ( version 3.14 )
  445 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  446 !/    20-Jan-2017 : Add error exit using EXTCDE.        ( version 6.02 )
  447 !/
  448 !  1. Purpose :
  449 !
  450 !     Writes 2-D array of pre-described layout and format. "Inverse"
  451 !     version of INA2R . For documentation see INA2R .
  452 !
  453 !     N.B. - ARRAY_OUT <= ( ARRAY_IN - VOF ) / VSC
  454 !          - No error trapping on write.
  455 !
  456 ! 10. Source code :
  457 !
  458 !/ ------------------------------------------------------------------- /
  459 !/
  460       USE W3SERVMD, ONLY: EXTCDE
  461 !
  462       IMPLICIT NONE
  463 !/
  464 !/ ------------------------------------------------------------------- /
  465 !/ Parameter list
  466 !/
  467       INTEGER, INTENT(IN)     :: MX, MY, LX, HX, LY, HY, NDS, NDST,  &
  468                                  NDSE, IDFM, IDLA
  469       REAL, INTENT(IN)        :: VSC, VOF, ARRAY(MX,MY)
  470       CHARACTER, INTENT(IN)   :: RFORM*(*)
  471 !/
  472 !/ ------------------------------------------------------------------- /
  473 !/ Local parameters
  474 !/
  475       INTEGER                 :: IIDFM, IIDLA, IX, IY, ISTAT
  476 !/
  477 !/ ------------------------------------------------------------------- /
  478 !/
  479 !
  480       IF (IDFM.LT.1 .OR. IDFM.GT.3) THEN
  481           IIDFM = 1
  482         ELSE
  483           IIDFM = IDFM
  484         END IF
  485       IF (IDLA.LT.1 .OR. IDLA.GT.4) THEN
  486           IIDLA = 1
  487         ELSE
  488           IIDLA = IDLA
  489         END IF
  490 !
  491 ! Free format write :
  492 !
  493       IF (IIDFM.EQ.1) THEN
  494           IF (IIDLA.EQ.1) THEN
  495               DO IY=LY, HY
 
 
                                                         w3arrymd.F90  page  10
 
 
  496                 WRITE (NDS,*,ERR=800,IOSTAT=ISTAT)              &
  497                       ((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX)
  498                 END DO
  499             ELSE IF (IIDLA.EQ.2) THEN
  500               WRITE (NDS,*,ERR=800,IOSTAT=ISTAT)                &
  501                    (((ARRAY(IX,IY)-VOF)/VSC,IX=LX,INT(HX/VSC)),IY=LY,HY)
  502             ELSE IF (IIDLA.EQ.3) THEN
  503               DO IY=HY, LY, -1
  504                 WRITE (NDS,*,ERR=800,IOSTAT=ISTAT)              &
  505                       ((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX)
  506                 END DO
  507             ELSE
  508               WRITE (NDS,*,ERR=800,IOSTAT=ISTAT)                &
  509                    (((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX),IY=HY,LY,-1)
  510             END IF
  511 !
  512 ! Fixed format write :
  513 !
  514         ELSE IF (IIDFM.EQ.2) THEN
  515           IF (IIDLA.EQ.1) THEN
  516               DO IY=LY, HY
  517                 WRITE (NDS,RFORM,ERR=800,IOSTAT=ISTAT)          &
  518                       ((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX)
  519                 END DO
  520             ELSE IF (IIDLA.EQ.2) THEN
  521               WRITE (NDS,RFORM,ERR=800,IOSTAT=ISTAT)            &
  522                    (((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX),IY=LY,HY)
  523             ELSE IF (IIDLA.EQ.3) THEN
  524               DO IY=HY, LY, -1
  525                 WRITE (NDS,RFORM,ERR=800,IOSTAT=ISTAT)          &
  526                       ((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX)
  527                 END DO
  528             ELSE
  529               WRITE (NDS,RFORM,ERR=800,IOSTAT=ISTAT)            &
  530                    (((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX),IY=HY,LY,-1)
  531             END IF
  532 !
  533 ! Unformat write :
  534 !
  535         ELSE
  536           IF (IIDLA.EQ.1) THEN
  537               DO IY=LY, HY
  538                 WRITE (NDS,ERR=800,IOSTAT=ISTAT)                &
  539                       ((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX)
  540                 END DO
  541             ELSE IF (IIDLA.EQ.2) THEN
  542               WRITE (NDS,ERR=800,IOSTAT=ISTAT)                  &
  543                    (((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX),IY=LY,HY)
  544             ELSE IF (IIDLA.EQ.3) THEN
  545               DO IY=HY, LY, -1
  546                 WRITE (NDS,ERR=800,IOSTAT=ISTAT)                &
  547                       ((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX)
  548                 END DO
  549             ELSE
  550               WRITE (NDS,ERR=800,IOSTAT=ISTAT)                  &
 
 
                                                         w3arrymd.F90  page  11
 
 
  551                     (((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX),IY=HY,LY,-1)
  552             END IF
  553         END IF
  554 !
  555       RETURN
  556 !
  557 ! Escape locations write errors :
  558 !
  559   800 CONTINUE
  560       WRITE (NDSE,900) ISTAT
  561       CALL EXTCDE ( ISTAT )
  562 !
  563 ! Formats
  564 !
  565   900 FORMAT (/' *** ERROR OUTA2R : '/                          &
  566                '     ERROR IN WRITING TO FILE'/                 &
  567                '     IOSTAT =',I5/)
  568 !
  569 !/
  570 !/ End of OUTA2R ----------------------------------------------------- /
  571 !/
  572       END SUBROUTINE OUTA2R
  573 !/ ------------------------------------------------------------------- /
  574       SUBROUTINE OUTA2I (ARRAY, MX, MY, LX, HX, LY, HY,              &
  575                          NDS, NDST, NDSE, IDFM, RFORM, IDLA, VSC, VOF)
  576 !/
  577 !/                  +-----------------------------------+
  578 !/                  | WAVEWATCH III           NOAA/NCEP |
  579 !/                  |           H. L. Tolman            |
  580 !/                  |                        FORTRAN 90 |
  581 !/                  | Last update :         30-Oct-2009 |
  582 !/                  +-----------------------------------+
  583 !/
  584 !/    31-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
  585 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  586 !/    30-Oct-2009 : Implement add offset argument.      ( version 3.14 )
  587 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  588 !/    20-Jan-2017 : Add error exit using EXTCDE.        ( version 6.02 )
  589 !/
  590 !  1. Purpose :
  591 !
  592 !     Like OUTA2R, integer ARRAY, VSC and VOF, see OUTA2R.
  593 !
  594 ! 10. Source code :
  595 !
  596 !/ ------------------------------------------------------------------- /
  597 !/
  598       USE W3SERVMD, ONLY: EXTCDE
  599 !
  600       IMPLICIT NONE
  601 !/
  602 !/ ------------------------------------------------------------------- /
  603 !/ Parameter list
  604 !/
  605       INTEGER, INTENT(IN)     :: MX, MY, LX, HX, LY, HY, NDS, NDST,  &
 
 
                                                         w3arrymd.F90  page  12
 
 
  606                                  NDSE, IDFM, IDLA, ARRAY(MX,MY)
  607       INTEGER, INTENT(IN)     :: VSC, VOF
  608       CHARACTER, INTENT(IN)   :: RFORM*(*)
  609 !/
  610 !/ ------------------------------------------------------------------- /
  611 !/ Local parameters
  612 !/
  613       INTEGER                 :: IIDFM, IIDLA, IX, IY, ISTAT
  614 !/
  615 !/ ------------------------------------------------------------------- /
  616 !/
  617 !
  618       IF (IDFM.LT.1 .OR. IDFM.GT.3) THEN
  619           IIDFM = 1
  620         ELSE
  621           IIDFM = IDFM
  622         END IF
  623       IF (IDLA.LT.1 .OR. IDLA.GT.4) THEN
  624           IIDLA = 1
  625         ELSE
  626           IIDLA = IDLA
  627         END IF
  628 !
  629 ! Free format write :
  630 !
  631       IF (IIDFM.EQ.1) THEN
  632           IF (IIDLA.EQ.1) THEN
  633               DO IY=LY, HY
  634                 WRITE (NDS,*,ERR=800,IOSTAT=ISTAT)              &
  635                       ((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX)
  636                 END DO
  637             ELSE IF (IIDLA.EQ.2) THEN
  638               WRITE (NDS,*,ERR=800,IOSTAT=ISTAT)                &
  639                    (((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX),IY=LY,HY)
  640             ELSE IF (IIDLA.EQ.3) THEN
  641               DO IY=HY, LY, -1
  642                 WRITE (NDS,*,ERR=800,IOSTAT=ISTAT)              &
  643                       ((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX)
  644                 END DO
  645             ELSE
  646               WRITE (NDS,*,ERR=800,IOSTAT=ISTAT)                &
  647                    (((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX),IY=HY,LY,-1)
  648             END IF
  649 !
  650 ! Fixed format write :
  651 !
  652         ELSE IF (IIDFM.EQ.2) THEN
  653           IF (IIDLA.EQ.1) THEN
  654               DO IY=LY, HY
  655                 WRITE (NDS,RFORM,ERR=800,IOSTAT=ISTAT)          &
  656                       ((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX)
  657                 END DO
  658             ELSE IF (IIDLA.EQ.2) THEN
  659               WRITE (NDS,RFORM,ERR=800,IOSTAT=ISTAT)            &
  660                    (((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX),IY=LY,HY)
 
 
                                                         w3arrymd.F90  page  13
 
 
  661             ELSE IF (IIDLA.EQ.3) THEN
  662               DO IY=HY, LY, -1
  663                 WRITE (NDS,RFORM,ERR=800,IOSTAT=ISTAT)          &
  664                       ((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX)
  665                 END DO
  666             ELSE
  667               WRITE (NDS,RFORM,ERR=800,IOSTAT=ISTAT)            &
  668                    (((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX),IY=HY,LY,-1)
  669             END IF
  670 !
  671 ! Unformat write :
  672 !
  673         ELSE
  674           IF (IIDLA.EQ.1) THEN
  675               DO IY=LY, HY
  676                 WRITE (NDS,ERR=800,IOSTAT=ISTAT)                &
  677                       ((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX)
  678                 END DO
  679             ELSE IF (IIDLA.EQ.2) THEN
  680               WRITE (NDS,ERR=800,IOSTAT=ISTAT)                  &
  681                    (((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX),IY=LY,HY)
  682             ELSE IF (IIDLA.EQ.3) THEN
  683               DO IY=HY, LY, -1
  684                 WRITE (NDS,ERR=800,IOSTAT=ISTAT)                &
  685                       ((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX)
  686                 END DO
  687             ELSE
  688               WRITE (NDS,ERR=800,IOSTAT=ISTAT)                  &
  689                    (((ARRAY(IX,IY)-VOF)/VSC,IX=LX,HX),IY=HY,LY,-1)
  690             END IF
  691         END IF
  692 !
  693       RETURN
  694 !
  695 ! Escape locations write errors :
  696 !
  697   800 CONTINUE
  698       WRITE (NDSE,900) ISTAT
  699       CALL EXTCDE ( ISTAT )
  700 !
  701 ! Formats
  702 !
  703   900 FORMAT (/' *** ERROR OUTA2I : '/                          &
  704                '     ERROR IN WRITING TO FILE'/                 &
  705                '     IOSTAT =',I5/)
  706 !
  707 !/
  708 !/ End of OUTA2I ----------------------------------------------------- /
  709 !/
  710       END SUBROUTINE OUTA2I
  711 !/ ------------------------------------------------------------------- /
  712       SUBROUTINE OUTREA (NDS,ARRAY,DIM,ANAME)
  713 !/
  714 !/                  +-----------------------------------+
  715 !/                  | WAVEWATCH III           NOAA/NCEP |
 
 
                                                         w3arrymd.F90  page  14
 
 
  716 !/                  |           H. L. Tolman            |
  717 !/                  |                        FORTRAN 90 |
  718 !/                  | Last update :         29-Nov-1999 |
  719 !/                  +-----------------------------------+
  720 !/                        Original versions G. Ph. van Vledder
  721 !/                                          P. H. Willems
  722 !/
  723 !/    29-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
  724 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  725 !/
  726 !  1. Purpose :
  727 !
  728 !     Print contents of a 1-D real array, see OUTINT.
  729 !
  730 !/ ------------------------------------------------------------------- /
  731 !/
  732 !
  733       IMPLICIT NONE
  734 !/
  735 !/ ------------------------------------------------------------------- /
  736 !/ Parameter list
  737 !/
  738       INTEGER, INTENT(IN)     :: NDS, DIM
  739       REAL, INTENT(IN)        :: ARRAY(DIM)
  740       CHARACTER, INTENT(IN)   :: ANAME*(*)
  741 !/
  742 !/ ------------------------------------------------------------------- /
  743 !/ Local parameters
  744 !/
  745       INTEGER                 :: I, K
  746 !/
  747 !/ ------------------------------------------------------------------- /
  748 !/
  749 !
  750       WRITE (NDS,8000) ANAME
  751 !
  752       IF (ICOL.EQ.80) THEN
  753 !
  754           WRITE (NDS,8005) (I, I=1, 5)
  755           WRITE (NDS,8010)
  756           DO K=0, DIM, 5
  757           IF (DIM-K.GE.5) THEN
  758               WRITE (NDS,'(1X,I4,A,5E12.4,A)')                  &
  759                 K,'  |',(ARRAY(I),I= K+1, K+5),'  |'
  760             ELSE
  761               WRITE (NDS,'(1X,T71,''|'',T2,I4,A,5E12.4)')       &
  762                 K,'  |',(ARRAY(I),I= K+1, DIM)
  763              END IF
  764           END DO
  765           WRITE (NDS,8010)
  766 !
  767         ELSE
  768 !
  769           WRITE (NDS,9005) (I, I=1, 10)
  770           WRITE (NDS,9010)
 
 
                                                         w3arrymd.F90  page  15
 
 
  771           DO K=0, DIM, 10
  772           IF (DIM-K.GE.10) THEN
  773               WRITE (NDS,'(1X,I4,A,10E12.4,A)')                 &
  774                 K,'  |',(ARRAY(I),I= K+1, K+10),'  |'
  775             ELSE
  776               WRITE (NDS,'(1X,T131,''|'',T2,I4,A,10E12.4)')     &
  777                 K,'  |',(ARRAY(I),I= K+1, DIM)
  778              END IF
  779           END DO
  780           WRITE (NDS,9010)
  781          END IF
  782 !
  783       RETURN
  784 !
  785  8000 FORMAT (/,1X,'A R R A Y   D U M P  (REAL) / NAME: ',A)
  786  8005 FORMAT (8X,5I12)
  787  8010 FORMAT (7X,'+',62('-'),'+')
  788  9005 FORMAT (8X,10I12)
  789  9010 FORMAT (7X,'+',122('-'),'+')
  790 !/
  791 !/ End of OUTREA ----------------------------------------------------- /
  792 !/
  793       END SUBROUTINE OUTREA
  794 !/ ------------------------------------------------------------------- /
  795       SUBROUTINE OUTINT ( NDS, IARRAY, DIM, ANAME )
  796 !/
  797 !/                  +-----------------------------------+
  798 !/                  | WAVEWATCH III           NOAA/NCEP |
  799 !/                  |           H. L. Tolman            |
  800 !/                  |                        FORTRAN 90 |
  801 !/                  | Last update :         29-Mar-1993 |
  802 !/                  +-----------------------------------+
  803 !/                        Original versions G. Ph. van Vledder
  804 !/                                          P. H. Willems
  805 !/
  806 !/    29-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
  807 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  808 !/
  809 !  1. Purpose :
  810 !
  811 !     Print contents of a 1-D integer array.
  812 !
  813 !  2. Method :
  814 !
  815 !  3. Parameters :
  816 !
  817 !     Parameter list
  818 !     ----------------------------------------------------------------
  819 !       NDS     Int.   I   Output unit number.
  820 !       IARRAY  I.A.   I   Array to be printed.
  821 !       DIM     Int.   I   Number of elements to be printed.
  822 !       ANAME   C*(*)  I   Name of array.
  823 !     ----------------------------------------------------------------
  824 !
  825 !  4. Subroutines used :
 
 
                                                         w3arrymd.F90  page  16
 
 
  826 !
  827 !     See mudule documentation.
  828 !
  829 !  5. Called by :
  830 !
  831 !       Anny routine or program.
  832 !
  833 ! 10. Source code :
  834 !
  835 !/ ------------------------------------------------------------------- /
  836 !/
  837 !
  838       IMPLICIT NONE
  839 !/
  840 !/ ------------------------------------------------------------------- /
  841 !/ Parameter list
  842 !/
  843       INTEGER, INTENT(IN)     :: NDS, DIM, IARRAY(DIM)
  844       CHARACTER, INTENT(IN)   :: ANAME*(*)
  845 !/
  846 !/ ------------------------------------------------------------------- /
  847 !/ Local parameters
  848 !/
  849       INTEGER                 :: I, K
  850 !/
  851 !/ ------------------------------------------------------------------- /
  852 !/
  853 !
  854       WRITE (NDS,8000) ANAME
  855 !
  856 !  ------- 80 COLUMNS -----
  857 !
  858       IF (ICOL.EQ.80) THEN
  859           WRITE (NDS,8005) (I, I=1, 5)
  860           WRITE (NDS,8010)
  861           DO K=0, DIM, 5
  862           IF (DIM-K.GE.5) THEN
  863               WRITE (NDS,'(1X,I4,A,5I12,A)')                    &
  864                 K,'  |',(IARRAY(I),I= K+1, K+5),'  |'
  865             ELSE
  866               WRITE (NDS,'(1X,T71,''|'',T2,I4,A,5I12)')         &
  867                 K,'  |',(IARRAY(I),I= K+1, DIM)
  868             END IF
  869           END DO
  870           WRITE (NDS,8010)
  871       ELSE
  872 !
  873 !    ---- 132 COLUMNS ----
  874 !
  875           WRITE (NDS,9005) (I, I=1, 10)
  876           WRITE (NDS,9010)
  877           DO K=0, DIM, 10
  878           IF (DIM-K.GE.10) THEN
  879               WRITE (NDS,'(1X,I4,A,10I12,A)')                   &
  880                 K,'  |',(IARRAY(I),I= K+1, K+10),'  |'
 
 
                                                         w3arrymd.F90  page  17
 
 
  881             ELSE
  882               WRITE (NDS,'(1X,T131,''|'',T2,I4,A,10I12)')       &
  883                 K,'  |',(IARRAY(I),I= K+1, DIM)
  884              END IF
  885           END DO
  886           WRITE (NDS,9010)
  887          END IF
  888 !
  889       RETURN
  890 !
  891  8000 FORMAT (/,1X,'A R R A Y   D U M P  (INTEGER) / NAME: ',A)
  892  8005 FORMAT (8X,5I12)
  893  8010 FORMAT (7X,'+',62('-'),'+')
  894  9005 FORMAT (8X,10I12)
  895  9010 FORMAT (7X,'+',122('-'),'+')
  896 !/
  897 !/ End of OUTINT ----------------------------------------------------- /
  898 !/
  899       END SUBROUTINE OUTINT
  900 !/ ------------------------------------------------------------------- /
  901       SUBROUTINE OUTMAT (NDS,A,MX,NX,NY,MNAME)
  902 !/
  903 !/                  +-----------------------------------+
  904 !/                  | WAVEWATCH III           NOAA/NCEP |
  905 !/                  |           H. L. Tolman            |
  906 !/                  |                        FORTRAN 90 |
  907 !/                  | Last update :         29-Nov-1999 |
  908 !/                  +-----------------------------------+
  909 !/                        Original versions G. Ph. van Vledder
  910 !/
  911 !/    29-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
  912 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  913 !/
  914 !  1. Purpose :
  915 !
  916 !     Print contents of a 2-D real array.
  917 !
  918 !  2. Method :
  919 !
  920 !  3. Parameters :
  921 !
  922 !     Parameter list
  923 !     ----------------------------------------------------------------
  924 !       NDS     Int.   I   Output unit number.
  925 !       A       R.A.   I   Matrix to be printed.
  926 !       MX      Int.   I   Dimension of first index.
  927 !       NX      Int.   I   Number of points for first index.
  928 !       NY      Int.   I   Number of points for scond index.
  929 !       MNAME   C*(*)  I   Name of matrix.
  930 !     ----------------------------------------------------------------
  931 !
  932 !  4. Subroutines used :
  933 !
  934 !     See mudule documentation.
  935 !
 
 
                                                         w3arrymd.F90  page  18
 
 
  936 !  5. Called by :
  937 !
  938 !       Anny routine or program.
  939 !
  940 ! 10. Source code :
  941 !
  942 !/ ------------------------------------------------------------------- /
  943 !/
  944 !
  945       IMPLICIT NONE
  946 !/
  947 !/ ------------------------------------------------------------------- /
  948 !/ Parameter list
  949 !/
  950       INTEGER, INTENT(IN)     :: NDS, MX, NX, NY
  951       REAL, INTENT(IN)        :: A(MX,NY)
  952       CHARACTER, INTENT(IN)   :: MNAME*(*)
  953 !/
  954 !/ ------------------------------------------------------------------- /
  955 !/ Local parameters
  956 !/
  957       INTEGER                 :: LBLOK, NBLOK, IBLOK, IX, IX1, IX2, IY
  958 !/
  959 !/ ------------------------------------------------------------------- /
  960 !/
  961 !
  962       WRITE(NDS,8000) MNAME
  963 !
  964 !  ------ 80 COLUMNS -----
  965 !
  966       IF(ICOL.EQ.80) THEN
  967           LBLOK = 6
  968           NBLOK = (NX-1)/LBLOK + 1
  969           DO IBLOK = 1,NBLOK
  970             IX1 = (IBLOK-1)*LBLOK + 1
  971             IX2 = IX1 + LBLOK - 1
  972             IF(IX2.GT.NX) IX2 = NX
  973             WRITE(NDS,8001) (IX,IX = IX1,IX2)
  974             WRITE(NDS,8002)
  975             DO IY = 1,NY
  976               WRITE(NDS,8003) IY,(A(IX,IY),IX = IX1,IX2)
  977               END DO
  978             WRITE(NDS,8002)
  979             END DO
  980         ELSE
  981 !
  982 !   ---- 132 COLUMNS ----
  983 !
  984           LBLOK = 12
  985           NBLOK = (NX-1)/LBLOK + 1
  986           DO IBLOK = 1,NBLOK
  987             IX1 = (IBLOK-1)*LBLOK + 1
  988             IX2 = IX1 + LBLOK - 1
  989             IF(IX2.GT.NX) IX2 = NX
  990             WRITE(NDS,9001) (IX,IX = IX1,IX2)
 
 
                                                         w3arrymd.F90  page  19
 
 
  991             WRITE(NDS,9002)
  992             DO IY = 1,NY
  993               WRITE(NDS,9003) IY,(A(IX,IY),IX = IX1,IX2)
  994               END DO
  995             WRITE(NDS,9002)
  996             END DO
  997          END IF
  998 !
  999       RETURN
 1000 !
 1001 ! Formats
 1002 !
 1003  8000 FORMAT(/,1X,' M A T R I X   D U M P  (REAL) / NAME: ',A)
 1004  8001 FORMAT(9X,6I10)
 1005  8002 FORMAT(1X,6X,'+',62('-'),'+')
 1006  8003 FORMAT(1X,T71,'|',T2,I5,' | ',12E10.3)
 1007  9001 FORMAT(9X,12I10)
 1008  9002 FORMAT(1X,6X,'+',122('-'),'+')
 1009  9003 FORMAT(1X,T131,'|',T2,I5,' | ',12E10.3)
 1010 !/
 1011 !/ End of OUTMAT ----------------------------------------------------- /
 1012 !/
 1013       END SUBROUTINE OUTMAT
 1014 !/ ------------------------------------------------------------------- /
 1015       SUBROUTINE PRTBLK (NDS, NX, NY, MX, F, MAP, MAP0, FSC,    &
 1016                          IX1, IX2, IX3, IY1, IY2, IY3, PRVAR, PRUNIT)
 1017 !/
 1018 !/                  +-----------------------------------+
 1019 !/                  | WAVEWATCH III           NOAA/NCEP |
 1020 !/                  |           H. L. Tolman            |
 1021 !/                  |                        FORTRAN 90 |
 1022 !/                  | Last update :         29-Nov-1999 |
 1023 !/                  +-----------------------------------+
 1024 !/
 1025 !/    04-Jun-1996 : Final FORTRAN 77                    ( version 1.18 )
 1026 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
 1027 !/
 1028 !  1. Purpose :
 1029 !
 1030 !     Print a block-type table of a two-dimensional field using a
 1031 !     land-sea array.
 1032 !
 1033 !  3. Parameters :
 1034 !
 1035 !     Parameter list
 1036 !     ----------------------------------------------------------------
 1037 !       NDS     Int.   I   File unit number.
 1038 !       NX, NY  Int.   I   X and Y range of arrays.
 1039 !       MY      Int.   I   Actual X size of arrays.
 1040 !       F       R.A.   I   Array to pr presented.
 1041 !       MAP     I.A.   I   Map array for land points.
 1042 !       MAP0    Int.   I   Map value for land points in MAP.
 1043 !       FSC     Real   I   Scaling factor.
 1044 !       IX1-3   Int.   I   Firts, last, increment grid points in X
 1045 !                          direction.
 
 
                                                         w3arrymd.F90  page  20
 
 
 1046 !       IY1-3   Int.   I   Id. Y direction.
 1047 !       PRVAR   C*(*)  I   Name of variable.
 1048 !       PRUNIT  C*(*)  I   Units of spectrum.
 1049 !     ----------------------------------------------------------------
 1050 !
 1051 !  4. Subroutines used :
 1052 !
 1053 !     See mudule documentation.
 1054 !
 1055 !  5. Called by :
 1056 !
 1057 !     Any program.
 1058 !
 1059 !  6. Error messages :
 1060 !
 1061 !     None.
 1062 !
 1063 !  7. Remarks :
 1064 !
 1065 !  8. Structure :
 1066 !
 1067 !     ------------------------------------------------
 1068 !       Check if automatic scaling
 1069 !       If automatic scaling : get extermata
 1070 !       Print heading
 1071 !       Print table
 1072 !       Print ending
 1073 !     ------------------------------------------------
 1074 !
 1075 !  9. Switches :
 1076 !
 1077 !     !/S  Enable subroutine tracing using STRACE.
 1078 !
 1079 ! 10. Source code :
 1080 !
 1081 !/ ------------------------------------------------------------------- /
 1082 !/
 1083 !
 1084       IMPLICIT NONE
 1085 !/
 1086 !/ ------------------------------------------------------------------- /
 1087 !/ Parameter list
 1088 !/
 1089       INTEGER, INTENT(IN)     :: NDS, NX, NY, MX, MAP(MX,NY), MAP0,  &
 1090                                  IX1, IX2, IX3, IY1, IY2, IY3
 1091       REAL, INTENT(IN)        :: F(MX,NY), FSC
 1092       CHARACTER, INTENT(IN)   :: PRVAR*(*), PRUNIT*(*)
 1093 !/
 1094 !/ ------------------------------------------------------------------- /
 1095 !/ Local parameters
 1096 !/
 1097       INTEGER                 :: IX, IY, JJ, JM, K1, LX, I
 1098       REAL                    :: FMAX, RR
 1099       LOGICAL                 :: FLSCLE
 1100       CHARACTER               :: PNUM*5, STRA*5, PNUM2*2, STRA3*3
 
 
                                                         w3arrymd.F90  page  21
 
 
 1101       DIMENSION               :: PNUM(25), PNUM2(61)
 1102 !/
 1103 !/ ------------------------------------------------------------------- /
 1104 !/
 1105 !
 1106 ! Check scaling
 1107 !
 1108       FLSCLE = (FSC.LE.0.)
 1109 !
 1110 ! Extremata
 1111 !
 1112       IF (FLSCLE) THEN
 1113           FMAX   = 1.E-15
 1114           DO IX=1, NX
 1115             DO IY=1, NY
 1116               IF ( MAP(IX,IY) .NE. MAP0 )                       &
 1117                    FMAX   = MAX ( FMAX , ABS(F(IX,IY)) )
 1118               END DO
 1119             END DO
 1120         END IF
 1121 !
 1122 ! Normalized print plot -----------------------------------------------
 1123 !
 1124       IF (FLSCLE) THEN
 1125 !
 1126 ! Heading
 1127 !
 1128           WRITE (NDS,901) PRVAR, FMAX, PRUNIT
 1129 !
 1130           STRA   = '     '
 1131           JJ     = 0
 1132           DO IX = IX1, IX2, IX3
 1133             JJ = JJ + 1
 1134             END DO
 1135           LX = JJ
 1136           WRITE (NDS,911)
 1137           WRITE (NDS,912) (IX,IX=IX1,IX2,2*IX3)
 1138           PNUM2(1) = '--'
 1139           WRITE (NDS,910) STRA, ' +', (PNUM2(1), I=1, LX), '-+'
 1140 !
 1141 ! Write table
 1142 !
 1143           JM = 0
 1144           DO IY = IY2, IY1, IY3*(-1)
 1145 !
 1146             JJ = 0
 1147             DO IX = IX1, IX2, IX3
 1148               JJ = JJ + 1
 1149               IF (MAP(IX,IY).EQ.MAP0) THEN
 1150                   PNUM2(JJ) = '  '
 1151                 ELSE
 1152                   RR = 10.*F(IX,IY)/FMAX
 1153                   WRITE (STRA, FMT='(I2,3X)') INT(RR*1.000001)
 1154                   PNUM2(JJ) = STRA(1:2)
 1155                   IF (PNUM2(JJ).EQ.'10' .OR. PNUM2(JJ).EQ.'**' .OR. &
 
 
                                                         w3arrymd.F90  page  22
 
 
 1156                       F(IX,IY).EQ.FMAX) THEN
 1157                       IF ( RR .LT. 0. ) THEN
 1158                           PNUM2(JJ) = '-*'
 1159                         ELSE
 1160                           PNUM2(JJ) = ' *'
 1161                         END IF
 1162                     END IF
 1163                 END IF
 1164               END DO
 1165 !
 1166             IF (JM.EQ.0) THEN
 1167                 WRITE (STRA, FMT='(I5)') IY
 1168                 JM   = 2
 1169               ELSE
 1170                 STRA = '     '
 1171                 JM   = JM-1
 1172               END IF
 1173 !
 1174             LX = JJ
 1175             WRITE (NDS,910) STRA, ' |', (PNUM2(I), I=1, LX), ' |'
 1176             END DO
 1177 !
 1178           STRA     = '     '
 1179           PNUM2(1) = '--'
 1180           WRITE (NDS,910) STRA, ' +', (PNUM2(1), I=1, LX), '-+'
 1181           WRITE (NDS,912) (IX,IX=IX1,IX2,2*IX3)
 1182           WRITE (NDS,911)
 1183 !
 1184 ! Non-normalized print plot -------------------------------------------
 1185 !
 1186         ELSE
 1187 !
 1188 ! Heading
 1189 !
 1190           WRITE (NDS,900) PRVAR, FSC, PRUNIT
 1191 !
 1192           JJ = 0
 1193           PNUM(1) = '     '
 1194           DO IX = IX1, IX2, IX3
 1195             JJ = JJ + 1
 1196             END DO
 1197           LX = JJ
 1198           WRITE (NDS,921)
 1199           WRITE (NDS,922) (IX,IX=IX1,IX2,IX3)
 1200           STRA3   = '   '
 1201           PNUM(1) = '-----'
 1202           WRITE (NDS,920) STRA3, ' +', (PNUM(1), I=1, LX), '-+   '
 1203 !
 1204 ! Write table
 1205 !
 1206           JM = 0
 1207           DO IY = IY2, IY1, IY3*(-1)
 1208             IF (JM.EQ.0) THEN
 1209                 WRITE (STRA3, FMT='(I3)') IY
 1210                 JM = 2
 
 
                                                         w3arrymd.F90  page  23
 
 
 1211               ELSE
 1212                 STRA3  = '   '
 1213                 JM = JM-1
 1214               END IF
 1215 !
 1216             JJ = 0
 1217             DO IX = IX1, IX2, IX3
 1218               JJ = JJ + 1
 1219               IF (MAP(IX,IY).EQ.MAP0) THEN
 1220                   PNUM(JJ) = '     '
 1221                 ELSE
 1222                   RR     = F(IX,IY)
 1223                   K1 = NINT (RR / FSC)
 1224                   WRITE (STRA, FMT='(I5)') K1
 1225                   PNUM(JJ) = STRA
 1226                 END IF
 1227               END DO
 1228 !
 1229             LX = JJ
 1230             WRITE (NDS,920) STRA3, ' |', (PNUM(I), I=1, LX), ' |   '
 1231             END DO
 1232 !
 1233           STRA3   = '   '
 1234           PNUM(1) = '-----'
 1235           WRITE (NDS,920) STRA3, ' +', (PNUM(1), I=1, LX), '-+   '
 1236           WRITE (NDS,922) (IX,IX=IX1,IX2,IX3)
 1237           WRITE (NDS,921)
 1238 !
 1239         END IF
 1240 !
 1241       RETURN
 1242 !
 1243 ! Formats
 1244 !
 1245   900 FORMAT (/, ' Variable: ',A,' Units: ',E10.3,1X,A)
 1246   901 FORMAT (/, ' Variable: ',A,' Max.: ',E10.3,1X,A)
 1247 !
 1248   910 FORMAT (1X,A5,63A2)
 1249   911 FORMAT (' ')
 1250   912 FORMAT (6X,32I8)
 1251 !
 1252   920 FORMAT (1X,A3,A2,25A5)
 1253   921 FORMAT (' ')
 1254   922 FORMAT (6X,25I5)
 1255 !/
 1256 !/ End of PRTBLK ----------------------------------------------------- /
 1257 !/
 1258       END SUBROUTINE PRTBLK
 1259 !/ ------------------------------------------------------------------- /
 1260       SUBROUTINE PRT1DS (NDS, NFR, E, FR, UFR, NLINES, FTOPI,    &
 1261                          PRVAR, PRUNIT, PNTNME)
 1262 !/
 1263 !/                  +-----------------------------------+
 1264 !/                  | WAVEWATCH III           NOAA/NCEP |
 1265 !/                  |           H. L. Tolman            |
 
 
                                                         w3arrymd.F90  page  24
 
 
 1266 !/                  |                        FORTRAN 90 |
 1267 !/                  | Last update :         29-Nov-1999 |
 1268 !/                  +-----------------------------------+
 1269 !/
 1270 !/    10-Mar-1992 : Final FORTRAN 77                    ( version 1.18 )
 1271 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
 1272 !/
 1273 !  1. Purpose :
 1274 !
 1275 !     Produces a print plot of a 1-D spectrum.
 1276 !
 1277 !  3. Parameters :
 1278 !
 1279 !     Parameter list
 1280 !     ----------------------------------------------------------------
 1281 !       NDS     Int.   I   File unit number.
 1282 !       NFR     Int.   I   Number of frequencies.
 1283 !       E       R.A.   I   Spectral densities.
 1284 !       FR      R.A.   I   Frequencies.
 1285 !       UFR     C*(*)  I   If 'HZ', frequencies in Hz, otherwise in
 1286 !                          rad/s (N.B., does not re-scale spectrum).
 1287 !       NLINES  Int.   I   Hight of plot in lines.
 1288 !       FTOPI   Real   I   Highest value of density in plot,
 1289 !                          if FTOPI.LE.0., automatic scaling.
 1290 !       PRVAR   C*(*)  I   Name of variable.
 1291 !       PRUNIT  C*(*)  I   Units of spectrum.
 1292 !       PNTNME  C*(*)  I   Name of location.
 1293 !     ----------------------------------------------------------------
 1294 !
 1295 !  4. Subroutines used :
 1296 !
 1297 !     See mudule documentation.
 1298 !
 1299 !  5. Called by :
 1300 !
 1301 !       Any routine.
 1302 !
 1303 !  6. Error messages :
 1304 !
 1305 !       None.
 1306 !
 1307 !  7. Remarks :
 1308 !
 1309 !     - Paperwidth is "set" by NFRMAX.
 1310 !
 1311 !  8. Structure :
 1312 !
 1313 !     ------------------------------------------------
 1314 !       Initializations and preparations.
 1315 !       Determine maximum of spectra.
 1316 !       Scaling / normalization.
 1317 !       Printing of spectrum
 1318 !       ----------------------------------------------
 1319 !         Print ID
 1320 !         Print heading
 
 
                                                         w3arrymd.F90  page  25
 
 
 1321 !         Print table
 1322 !         Print ending
 1323 !     ------------------------------------------------
 1324 !
 1325 !  9. Switches :
 1326 !
 1327 !     !/S  Enable subroutine tracing using STRACE.
 1328 !
 1329 ! 10. Source code :
 1330 !
 1331 !/ ------------------------------------------------------------------- /
 1332 !/
 1333 !
 1334       IMPLICIT NONE
 1335 !/
 1336 !/ ------------------------------------------------------------------- /
 1337 !/ Parameter list
 1338 !/
 1339       INTEGER, INTENT(IN)     :: NDS, NFR, NLINES
 1340       REAL, INTENT(IN)        :: FTOPI, E(NFR), FR(NFR)
 1341       CHARACTER, INTENT(IN)   :: PRVAR*(*), PRUNIT*(*), PNTNME*(*),  &
 1342                                  UFR*(*)
 1343 !/
 1344 !/ ------------------------------------------------------------------- /
 1345 !/ Local parameters
 1346 !/
 1347       INTEGER                 :: NFRB, IFR, IL, IL0
 1348       REAL, SAVE              :: TOPFAC = 1.1
 1349       REAL                    :: FTOP, RLINES, FACFR, FSC, FLINE,    &
 1350                                  EMAX, EMIN, EXTR, FLOC
 1351       LOGICAL                 :: FLSCLE
 1352       CHARACTER               :: STRA*10, STRA2*2, PNUM2*2
 1353       DIMENSION               :: PNUM2(NFM2)
 1354 !/
 1355 !/ ------------------------------------------------------------------- /
 1356 !/
 1357 !
 1358       FTOP   = FTOPI
 1359 !
 1360       NFRB   = MIN (NFR,50)
 1361       RLINES = REAL(NLINES)
 1362       FLSCLE = FTOP.LE.0.
 1363 !
 1364       IF (UFR.EQ.'HZ') THEN
 1365          FACFR  = 1.
 1366        ELSE
 1367          FACFR  = 0.159155
 1368        END IF
 1369 !
 1370 ! Maximum of 1-D spectrum
 1371 !
 1372       EMAX   = 0.
 1373       EMIN   = 0.
 1374 !
 1375       DO IFR=1, NFR
 
 
                                                         w3arrymd.F90  page  26
 
 
 1376         EMAX = MAX ( EMAX , E(IFR) )
 1377         EMIN = MIN ( EMIN , E(IFR) )
 1378         END DO
 1379 !
 1380       IF (EMAX.EQ.0. .AND. EMIN.EQ.0.) THEN
 1381           EMAX   =  1.E-20
 1382           EMIN   = -1.E-20
 1383         END IF
 1384 !
 1385       IF (EMAX.GT.ABS(EMIN)) THEN
 1386           EXTR   = EMAX
 1387         ELSE
 1388           EXTR   = EMIN
 1389         END IF
 1390 !
 1391 ! Scaling / Normalization
 1392 !
 1393       IF (FLSCLE) THEN
 1394           IF (EMAX.GT.ABS(EMIN)) THEN
 1395               FLOC   = EMAX * TOPFAC
 1396               FSC    = FLOC / REAL(NINT(EMAX/(EMAX-EMIN)*RLINES))
 1397             ELSE
 1398               FLOC   = EMIN * TOPFAC
 1399               FSC    = FLOC / REAL(NINT(EMIN/(EMAX-EMIN)*RLINES))
 1400               FLOC   = FTOP + RLINES*FSC
 1401               IF (EMAX.LT.0.01*FSC) FTOP = 0.
 1402             END IF
 1403         ELSE
 1404           FLOC   = FTOP
 1405           FSC    = FLOC  / RLINES
 1406           IF (EMAX*EMIN.LT.0) FSC = 2.*FSC
 1407           IF (EMAX.LT.0.01*FSC) FLOC = 0.
 1408         END IF
 1409 !
 1410       IL0   = MOD ( NINT(FLOC/FSC) , 2 ) + 1
 1411 !
 1412 ! Print ID
 1413 !
 1414       WRITE (NDS,900) PNTNME, PRVAR, EXTR, PRUNIT
 1415 !
 1416 ! Print heading
 1417 !
 1418       FLINE  = FLOC
 1419       IF (MOD(NLINES+IL0,2).EQ.0) THEN
 1420           WRITE (STRA, FMT='(E10.3)') FLINE
 1421         ELSE
 1422           STRA=  '          '
 1423         END IF
 1424 !
 1425       DO IFR=1, NFRB
 1426         IF ( NINT( (E(IFR)-FLINE)/FSC ) .EQ.0) THEN
 1427             PNUM2(IFR) = '-*'
 1428           ELSE
 1429             PNUM2(IFR) = '--'
 1430           END IF
 
 
                                                         w3arrymd.F90  page  27
 
 
 1431         END DO
 1432 !
 1433       PNUM2(NFRB+1) = '-+'
 1434       STRA2 = ' +'
 1435       WRITE (NDS,910) STRA, STRA2, (PNUM2(IFR),IFR=1, NFRB+1)
 1436 !
 1437 ! Print table
 1438 !
 1439       DO IL = 1, NLINES-1
 1440         FLINE  = FLOC - FSC * REAL(IL)
 1441         IF (ABS(FLINE).LT.0.01*FSC) FLINE = 0.
 1442         IF (MOD(NLINES+IL0-IL,2).EQ.0) THEN
 1443             WRITE (STRA, FMT='(E10.3)') FLINE
 1444             STRA2 =  ' +'
 1445           ELSE
 1446             STRA  =  '          '
 1447             STRA2 =  ' |'
 1448           END IF
 1449         DO IFR=1, NFRB
 1450           IF (ABS(FLINE).LT.0.1*FSC) THEN
 1451               PNUM2(NFRB+1) = '-|'
 1452               IF ( NINT( (E(IFR)-FLINE)/FSC ) .EQ.0) THEN
 1453                   PNUM2(IFR) = '-*'
 1454                 ELSE
 1455                   PNUM2(IFR) = '--'
 1456                 END IF
 1457             ELSE
 1458               PNUM2(NFRB+1) = ' |'
 1459               IF ( NINT( (E(IFR)-FLINE)/FSC ) .EQ.0) THEN
 1460                   PNUM2(IFR) = ' *'
 1461                 ELSE
 1462                   PNUM2(IFR) = '  '
 1463                 END IF
 1464             END IF
 1465           END DO
 1466         WRITE (NDS,910) STRA, STRA2, (PNUM2(IFR),IFR=1, NFRB+1)
 1467         END DO
 1468 !
 1469 ! write ending
 1470 !
 1471       FLINE  = FLOC - FSC * REAL(IL)
 1472       IF (ABS(FLINE).LT.0.01*FSC) FLINE = 0.
 1473       WRITE (STRA, FMT='(E10.3)') FLINE
 1474         IF (MOD(IL0,2).EQ.0) THEN
 1475             WRITE (STRA, FMT='(E10.3)') FLINE
 1476           ELSE
 1477             STRA  =  '          '
 1478           END IF
 1479       STRA2         = ' +'
 1480       PNUM2(NFRB+1) = '-+'
 1481 !
 1482       DO IFR=1, NFRB
 1483         IF ( NINT( (E(IFR)-FLINE)/FSC ) .EQ.0) THEN
 1484             PNUM2(IFR) = '-*'
 1485           ELSE IF ( MOD (IFR-2,4) .EQ. 0 ) THEN
 
 
                                                         w3arrymd.F90  page  28
 
 
 1486             PNUM2(IFR) = '-|'
 1487           ELSE
 1488             PNUM2(IFR) = '--'
 1489           END IF
 1490         END DO
 1491 !
 1492       WRITE (NDS,910) STRA, STRA2, (PNUM2(IFR),IFR=1, NFRB+1)
 1493       WRITE (NDS,911) (FR(IFR)*FACFR,IFR=2,NFRB,4)
 1494       WRITE (NDS,920)
 1495 !
 1496       RETURN
 1497 !
 1498 ! Formats
 1499 !
 1500   900 FORMAT (/'  Location : ',A                                &
 1501               /'  Spectrum : ',A,'  Extreme value : ',E10.3,1X,A/)
 1502 !
 1503   910 FORMAT (A10,A2,60A2)
 1504   911 FORMAT (10X,15F8.3)
 1505 !
 1506   920 FORMAT (' ')
 1507 !/
 1508 !/ End of PRT1DS ----------------------------------------------------- /
 1509 !/
 1510       END SUBROUTINE PRT1DS
 1511 !/ ------------------------------------------------------------------- /
 1512       SUBROUTINE PRT1DM (NDS, NFR, NE, E, FR, UFR, NLINES, FTOPI, &
 1513                          PRVAR, PRUNIT, PNTNME)
 1514 !/
 1515 !/                  +-----------------------------------+
 1516 !/                  | WAVEWATCH III           NOAA/NCEP |
 1517 !/                  |           H. L. Tolman            |
 1518 !/                  |                        FORTRAN 90 |
 1519 !/                  | Last update :         17-Apr-1992 |
 1520 !/                  +-----------------------------------+
 1521 !/
 1522 !/    17-Apr-1992 : Final FORTRAN 77                    ( version 1.18 )
 1523 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
 1524 !/
 1525 !  1. Purpose :
 1526 !
 1527 !     Produces a print plot of several 1-D spectra.
 1528 !
 1529 !  3. Parameters :
 1530 !
 1531 !     Parameter list
 1532 !     ----------------------------------------------------------------
 1533 !       NDS     Int.   I   File unit number.
 1534 !       NFR     Int.   I   Number of frequencies.
 1535 !       NE      Int.   I   Number of spectra.
 1536 !       E       R.A.   I   Spectral densities.
 1537 !       FR      R.A.   I   Frequencies.
 1538 !       UFR     C*     I   If 'HZ', frequencies in Hz, otherwise in
 1539 !                          rad/s
 1540 !       NLINES  Int.   I   Hight of plot in lines.
 
 
                                                         w3arrymd.F90  page  29
 
 
 1541 !       FTOPI   Real   I   Highest value of density in plot,
 1542 !                          if FTOP.LE.0., automatic scaling.
 1543 !       PRVAR   C*(*)  I   Name of variable.
 1544 !       PRUNIT  C*(*)  I   Units of spectrum.
 1545 !       PNTNME  C*(*)  I   Name of location.
 1546 !     ----------------------------------------------------------------
 1547 !
 1548 !  4. Subroutines used :
 1549 !
 1550 !     See mudule documentation.
 1551 !
 1552 !  5. Called by :
 1553 !
 1554 !       Any routine.
 1555 !
 1556 !  6. Error messages :
 1557 !
 1558 !       None.
 1559 !
 1560 !  7. Remarks :
 1561 !
 1562 !     - Paperwidth is "set" by NFRMAX.
 1563 !
 1564 !  8. Structure :
 1565 !
 1566 !     ------------------------------------------------
 1567 !       Initializations and preparations.
 1568 !       Determine maximum of spectrum.
 1569 !       Scaling / normalization.
 1570 !       Printing of spectrum
 1571 !       ----------------------------------------------
 1572 !         Print ID
 1573 !         Print heading
 1574 !         Print table
 1575 !         Print ending
 1576 !     ------------------------------------------------
 1577 !
 1578 !  9. Switches :
 1579 !
 1580 !     !/S  Enable subroutine tracing using STRACE.
 1581 !
 1582 ! 10. Source code :
 1583 !
 1584 !/ ------------------------------------------------------------------- /
 1585 !/
 1586 !
 1587       IMPLICIT NONE
 1588 !/
 1589 !/ ------------------------------------------------------------------- /
 1590 !/ Parameter list
 1591 !/
 1592       INTEGER, INTENT(IN)     :: NDS, NFR, NE, NLINES
 1593       REAL, INTENT(IN)        :: FTOPI, E(NFR,NE), FR(NFR)
 1594       CHARACTER, INTENT(IN)   :: PRVAR*(*), PRUNIT*(*), PNTNME*(*),  &
 1595                                  UFR*(*)
 
 
                                                         w3arrymd.F90  page  30
 
 
 1596       DIMENSION               :: PRVAR(NE)
 1597 !/
 1598 !/ ------------------------------------------------------------------- /
 1599 !/ Local parameters
 1600 !/
 1601       INTEGER, PARAMETER      :: NFRMAX = 100
 1602       INTEGER, PARAMETER      :: NFM2   = NFRMAX+1
 1603       INTEGER                 :: NFRB, IFR, IE, IL
 1604       REAL, SAVE              :: TOPFAC = 1.1
 1605       REAL                    :: FTOP, RLINES, FACFR, FSC, FLINE,    &
 1606                                  EMAX, EMIN, EXTR, FLOC
 1607       LOGICAL                 :: FLSCLE
 1608       CHARACTER               :: STRA*10, STRA2*2, STRAX*2, PNUM2*2
 1609       DIMENSION               :: PNUM2(NFM2)
 1610 !/
 1611 !/ ------------------------------------------------------------------- /
 1612 !/
 1613 !
 1614 ! Test output, echo input
 1615 !
 1616       FTOP   = FTOPI
 1617       NFRB   = MIN (NFR,50)
 1618       RLINES = REAL(NLINES)
 1619       FLSCLE = FTOP.LE.0.
 1620 !
 1621       IF (UFR.EQ.'HZ') THEN
 1622          FACFR  = 1.
 1623        ELSE
 1624          FACFR  = 0.159155
 1625        END IF
 1626 !
 1627 ! Maximum of 1-D spectrum
 1628 !
 1629       EMAX   = 0.
 1630       EMIN   = 0.
 1631 !
 1632       DO IE=1, NE
 1633         DO IFR=1, NFR
 1634           EMAX = MAX ( EMAX , E(IFR,IE) )
 1635           EMIN = MIN ( EMIN , E(IFR,IE) )
 1636           END DO
 1637         END DO
 1638 !
 1639       IF (EMAX.EQ.0. .AND. EMIN.EQ.0.) THEN
 1640           EMAX   =  1.E-20
 1641           EMIN   = -1.E-20
 1642         END IF
 1643 !
 1644       IF (EMAX.GT.ABS(EMIN)) THEN
 1645           EXTR   = EMAX
 1646         ELSE
 1647           EXTR   = EMIN
 1648         END IF
 1649 !
 1650 ! Scaling / Normalization
 
 
                                                         w3arrymd.F90  page  31
 
 
 1651 !
 1652       IF (FLSCLE) THEN
 1653           IF (EMAX.GT.ABS(EMIN)) THEN
 1654               FTOP   = EMAX * TOPFAC
 1655               FSC    = FTOP / REAL(NINT(EMAX/(EMAX-EMIN)*RLINES))
 1656             ELSE
 1657               FTOP   = EMIN * TOPFAC
 1658               FSC    = FTOP / REAL(NINT(EMIN/(EMAX-EMIN)*RLINES))
 1659               FTOP   = FTOP + RLINES*FSC
 1660               IF (ABS(FTOP).LT.0.01*FSC) FTOP = 0.
 1661             END IF
 1662         ELSE
 1663           FSC    = FTOP  / RLINES
 1664           IF (EMAX*EMIN.LT.0) FSC = 2.*FSC
 1665           IF (EMAX.EQ.0.) FTOP = 0.
 1666         END IF
 1667 !
 1668 ! Print ID
 1669 !
 1670       WRITE (NDS,900) PNTNME, EXTR, PRUNIT
 1671 !
 1672 ! Print heading
 1673 !
 1674       FLINE  = FTOP
 1675       IF (MOD(NLINES,2).EQ.0) THEN
 1676           WRITE (STRA, FMT='(E10.3)') FLINE
 1677         ELSE
 1678           STRA=  '          '
 1679         END IF
 1680 !
 1681       DO IFR=1, NFRB
 1682         PNUM2(IFR) = '--'
 1683         DO IE=1, NE
 1684           IF ( NINT( (E(IFR,IE)-FLINE)/FSC ) .EQ.0) THEN
 1685               IF (IE.LT.10) THEN
 1686                   WRITE (STRAX,'(A1,I1)') '-', IE
 1687                 ELSE
 1688                   WRITE (STRAX,'(I2)') IE
 1689                 END IF
 1690               PNUM2(IFR) = STRAX
 1691             END IF
 1692           END DO
 1693         END DO
 1694 !
 1695       PNUM2(NFRB+1) = '-+'
 1696       STRA2 = ' +'
 1697       WRITE (NDS,910) STRA, STRA2, (PNUM2(IFR),IFR=1, NFRB+1)
 1698 !
 1699 ! Print table
 1700 !
 1701       PNUM2(NFRB+1) = ' |'
 1702 !
 1703       DO IL = 1, NLINES-1
 1704         FLINE  = FTOP - FSC * REAL(IL)
 1705         IF (ABS(FLINE).LT.0.01*FSC) FLINE = 0.
 
 
                                                         w3arrymd.F90  page  32
 
 
 1706         IF (MOD(NLINES-IL,2).EQ.0) THEN
 1707             WRITE (STRA, FMT='(E10.3)') FLINE
 1708             STRA2 =  ' +'
 1709           ELSE
 1710             STRA  =  '          '
 1711             STRA2 =  ' |'
 1712           END IF
 1713         DO IFR=1, NFRB
 1714           PNUM2(NFRB+1) = ' |'
 1715           IF (ABS(FLINE).LT.0.1*FSC) THEN
 1716               PNUM2(IFR) = '--'
 1717               PNUM2(NFRB+1) = '-+'
 1718               DO IE=1, NE
 1719                 IF ( NINT( (E(IFR,IE)-FLINE)/FSC ) .EQ.0) THEN
 1720                     IF (IE.LT.10) THEN
 1721                         WRITE (STRAX,'(A1,I1)') '-', IE
 1722                       ELSE
 1723                         WRITE (STRAX,'(I2)') IE
 1724                       END IF
 1725                     PNUM2(IFR) = STRAX
 1726                   END IF
 1727                 END DO
 1728             ELSE
 1729               PNUM2(IFR) = '  '
 1730               DO IE=1, NE
 1731                 IF ( NINT( (E(IFR,IE)-FLINE)/FSC ) .EQ.0) THEN
 1732                     WRITE (STRAX,'(I2)') IE
 1733                     PNUM2(IFR) = STRAX
 1734                   END IF
 1735                 END DO
 1736             END IF
 1737           END DO
 1738         WRITE (NDS,910) STRA, STRA2, (PNUM2(IFR),IFR=1, NFRB+1)
 1739         END DO
 1740 !
 1741 ! write ending
 1742 !
 1743       FLINE  = FTOP - FSC * REAL(IL)
 1744       IF (ABS(FLINE).LT.0.01*FSC) FLINE = 0.
 1745       WRITE (STRA, FMT='(E10.3)') FLINE
 1746       STRA2         = ' +'
 1747       PNUM2(NFRB+1) = '-+'
 1748 !
 1749       DO IFR=1, NFRB
 1750         IF ( MOD (IFR-2,4) .EQ. 0 ) THEN
 1751             PNUM2(IFR) = '-|'
 1752           ELSE
 1753             PNUM2(IFR) = '--'
 1754           END IF
 1755         DO IE=1, NE
 1756           IF ( NINT( (E(IFR,IE)-FLINE)/FSC ) .EQ.0) THEN
 1757               IF (IE.LT.10) THEN
 1758                   WRITE (STRAX,'(A1,I1)') '-', IE
 1759                 ELSE
 1760                   WRITE (STRAX,'(I2)') IE
 
 
                                                         w3arrymd.F90  page  33
 
 
 1761                 END IF
 1762               PNUM2(IFR) = STRAX
 1763             END IF
 1764           END DO
 1765         END DO
 1766 !
 1767       WRITE (NDS,910) STRA, STRA2, (PNUM2(IFR),IFR=1, NFRB+1)
 1768       WRITE (NDS,911) (FR(IFR)*FACFR,IFR=2,NFRB,4)
 1769       WRITE (NDS,920)
 1770       WRITE (NDS,921) (PRVAR(IE),IE=1,NE)
 1771       WRITE (NDS,920)
 1772       IF (FLSCLE) FTOP = 0.
 1773 !
 1774       RETURN
 1775 !
 1776 ! Formats
 1777 !
 1778   900 FORMAT (/'  Location : ',A                                &
 1779               /'  Extreme value : ',E10.3,1X,A/)
 1780 !
 1781   910 FORMAT (A10,A2,60A2)
 1782   911 FORMAT (10X,15F8.3)
 1783 !
 1784   920 FORMAT (' ')
 1785   921 FORMAT (10X,'spectra : ',10(A,'  ')/)
 1786 !/
 1787 !/ End of PRT1DM ----------------------------------------------------- /
 1788 !/
 1789       END SUBROUTINE PRT1DM
 1790 !/ ------------------------------------------------------------------- /
 1791       SUBROUTINE PRT2DS (NDS, NFR0, NFR, NTH, E, FR, UFR, FACSP, FSC, &
 1792                          RRCUT, PRVAR, PRUNIT, PNTNME)
 1793 !/
 1794 !/                  +-----------------------------------+
 1795 !/                  | WAVEWATCH III           NOAA/NCEP |
 1796 !/                  |           H. L. Tolman            |
 1797 !/                  |                        FORTRAN 90 |
 1798 !/                  | Last update :         29-Nov-1999 |
 1799 !/                  +-----------------------------------+
 1800 !/
 1801 !/    07-Jun-1996 : Final FORTRAN 77                    ( version 1.18 )
 1802 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
 1803 !/
 1804 !  1. Purpose :
 1805 !
 1806 !     Prints a block type table of a 2-D spectrum. Input considers
 1807 !     cartesian directions, output according to meteorological
 1808 !     conventions (compass direction where waves come from).
 1809 !
 1810 !  3. Parameters :
 1811 !
 1812 !     Parameter list
 1813 !     ----------------------------------------------------------------
 1814 !       NDS     Int.   I   File unit number.
 1815 !       NFR0    Int.   I   Array size for freq.
 
 
                                                         w3arrymd.F90  page  34
 
 
 1816 !       NFR     Int.   I   Number of frequencies.
 1817 !       NTH     Int.   I   Number of frequencies.
 1818 !       E       R.A.   I   Spectral densities.
 1819 !       FR      R.A.   I   Frequencies.
 1820 !       UFR     C*(*)  I   If 'HZ', frequencies in Hz, otherwise in
 1821 !                          rad/s
 1822 !       FACSP   Real   I   Conversion factor to obtain (Hz,degr)
 1823 !                          spectrum from E
 1824 !       FSC     Real   I   Scale factor, if FSC.eq.0. automatic
 1825 !                          scaling for "compressed" block.
 1826 !       RRCUT   Real   I   Relative cut-off for printing.
 1827 !       PRVAR   C*(*)  I   Name of variable.
 1828 !       PRUNIT  C*(*)  I   Units of spectrum.
 1829 !       PNTNME  C*(*)  I   Name of location.
 1830 !     ----------------------------------------------------------------
 1831 !
 1832 !  4. Subroutines used :
 1833 !
 1834 !       ANGSTR (Internal)
 1835 !
 1836 !  5. Called by :
 1837 !
 1838 !       Any program.
 1839 !
 1840 !  6. Error messages :
 1841 !
 1842 !       None.
 1843 !
 1844 !  7. Remarks :
 1845 !
 1846 !       PNUM2: dimensioning changed from 51 to 71 due to "subscript out
 1847 !           of range" fault (Sep 28 2012)
 1848 !
 1849 !  8. Structure :
 1850 !
 1851 !     ------------------------------------------------
 1852 !       Initializations and preparations.
 1853 !       Determine maximum of spectrum.
 1854 !       Scaling / normalization.
 1855 !       Do for normalized or non-norm. spectrum
 1856 !       ----------------------------------------------
 1857 !         Print ID
 1858 !         Print heading
 1859 !         Print table
 1860 !         Print ending
 1861 !     ------------------------------------------------
 1862 !
 1863 !  9. Switches :
 1864 !
 1865 !     !/S  Enable subroutine tracing using STRACE.
 1866 !     !/T  Diagnostic test output.
 1867 !
 1868 ! 10. Source code :
 1869 !
 1870 !/ ------------------------------------------------------------------- /
 
 
                                                         w3arrymd.F90  page  35
 
 
 1871 !/
 1872 !
 1873       IMPLICIT NONE
 1874 !/
 1875 !/ ------------------------------------------------------------------- /
 1876 !/ Parameter list
 1877 !/
 1878       INTEGER, INTENT(IN)     :: NDS, NFR0, NFR, NTH
 1879       REAL, INTENT(IN)        :: E(NFR0,*), FR(*), FACSP, FSC, RRCUT
 1880       CHARACTER, INTENT(IN)   :: PRVAR*(*), PRUNIT*(*), PNTNME*(*),  &
 1881                                  UFR*(*)
 1882 !/
 1883 !/ ------------------------------------------------------------------- /
 1884 !/ Local parameters
 1885 !/
 1886       INTEGER                 :: IFR, ITH, NFRB, INTANG, ITHSEC
 1887       LOGICAL                 :: FLSCLE
 1888       REAL                    :: FACFR, EMAX, EMIN, DTHDEG, RR, RRC
 1889       CHARACTER               :: PNUM*5, STRA*5, STRANG*5, PNUM2*2,  &
 1890                                  STRA2*2
 1891       DIMENSION               :: PNUM(25), PNUM2(101)
 1892 !/
 1893 !/ ------------------------------------------------------------------- /
 1894 !/
 1895 !
 1896 ! initialisations
 1897 !
 1898       FLSCLE = .FALSE.
 1899       IF (FSC.EQ.0.) THEN
 1900           FLSCLE = .TRUE.
 1901           RRC    = RRCUT * 10.
 1902         END IF
 1903 !
 1904       IF (UFR.EQ.'HZ') THEN
 1905            FACFR  = 1.
 1906          ELSE
 1907            FACFR  = 0.159155
 1908          END IF
 1909 !
 1910 ! Maximum of spectrum
 1911 !
 1912       EMAX   = 1.E-20
 1913       EMIN   = 0.
 1914 !
 1915       DO IFR=1, NFR
 1916         DO ITH=1, NTH
 1917           EMAX = MAX ( EMAX , E(IFR,ITH) )
 1918           EMIN = MIN ( EMIN , E(IFR,ITH) )
 1919           END DO
 1920         END DO
 1921 !
 1922       EMAX = MAX (EMAX, ABS(EMIN) )
 1923 !
 1924       DTHDEG = 360. / REAL(NTH)
 1925 !
 
 
                                                         w3arrymd.F90  page  36
 
 
 1926 ! Normalized spectra :  = = = = = = = = = = = = = = = = = = = = = =
 1927 !
 1928       IF (FLSCLE) THEN
 1929 !
 1930 ! Write ID
 1931 !
 1932           WRITE (NDS,900) PNTNME, PRVAR, EMAX*FACSP, PRUNIT
 1933 !
 1934 ! Write Head
 1935 !
 1936           NFRB  = MIN (NFR,50)
 1937           WRITE (NDS,910) (FR(IFR)*FACFR,IFR=2,NFRB,4)
 1938 !
 1939           DO IFR=1, NFR
 1940             IF ( MOD((IFR-2),4) .EQ. 0) THEN
 1941                 PNUM2(IFR) = '-|'
 1942               ELSE
 1943                 PNUM2(IFR) = '--'
 1944               END IF
 1945             END DO
 1946 !
 1947           PNUM2(NFRB+1) = '-+'
 1948           WRITE (NDS,920) (PNUM2(IFR),IFR=1, NFRB+1)
 1949 !
 1950 ! Write table
 1951 !
 1952           ITHSEC = NTH + 1
 1953 !
 1954           DO ITH= NTH, 1, -1
 1955             INTANG = 270 - NINT (DTHDEG*REAL(ITH-1))
 1956             IF (INTANG.LT.0) THEN
 1957                 ITHSEC = ITH
 1958                 CYCLE
 1959               END IF
 1960             CALL ANGSTR (INTANG, STRANG, 4, 2)
 1961             DO IFR=1, NFRB
 1962               RR     = E(IFR,ITH)/EMAX
 1963               IF (E(IFR,ITH).EQ.EMAX .OR. RR.GE.1.) THEN
 1964                   PNUM2(IFR) = ' *'
 1965                 ELSE IF (-E(IFR,ITH).EQ.EMAX .OR. RR.LE.-1.) THEN
 1966                   PNUM2(IFR) = ' #'
 1967                 ELSE IF (ABS(RR).LT.RRC) THEN
 1968                   PNUM2(IFR) = '  '
 1969                 ELSE IF ((RR*10.).LT.0. .AND. (RR*10.).GT.-1.) THEN
 1970                   PNUM2(IFR) = '-0'
 1971                 ELSE
 1972                   WRITE (STRA2, FMT='(I2)') INT (RR*10.)
 1973                   PNUM2(IFR) = STRA2
 1974                 END IF
 1975               END DO
 1976             PNUM2(NFRB+1) = ' |'
 1977             WRITE (NDS,930) STRANG, (PNUM2(IFR),IFR=1, NFRB+1)
 1978             END DO
 1979 !
 1980           DO ITH= NTH, ITHSEC, -1
 
 
                                                         w3arrymd.F90  page  37
 
 
 1981             INTANG = 630 - NINT (DTHDEG*REAL(ITH-1))
 1982             CALL ANGSTR (INTANG, STRANG, 4, 2)
 1983             DO IFR=1, NFRB
 1984               RR     = E(IFR,ITH)/EMAX
 1985               IF (E(IFR,ITH).EQ.EMAX .OR. RR.GE.1.) THEN
 1986                   PNUM2(IFR) = ' *'
 1987                 ELSE IF (-E(IFR,ITH).EQ.EMAX .OR. RR.LE.-1.) THEN
 1988                   PNUM2(IFR) = ' #'
 1989                 ELSE IF (ABS(RR).LT.RRC) THEN
 1990                   PNUM2(IFR) = '  '
 1991                 ELSE IF ((RR*10.).LT.0. .AND. (RR*10.).GT.-1.) THEN
 1992                   PNUM2(IFR) = '-0'
 1993                 ELSE
 1994                   WRITE (STRA2, FMT='(I2)') INT (RR*10.)
 1995                   PNUM2(IFR) = STRA2
 1996                 END IF
 1997               END DO
 1998             PNUM2(NFRB+1) = ' |'
 1999             WRITE (NDS,930) STRANG, (PNUM2(IFR),IFR=1, NFRB+1)
 2000             END DO
 2001 !
 2002 ! Write ending:
 2003 !
 2004           PNUM2(1) = '--'
 2005           PNUM2(2) = '-+'
 2006           WRITE (NDS,920) (PNUM2(1),IFR=1, NFRB), PNUM2(2)
 2007           WRITE (NDS,950)
 2008 !
 2009 ! Scaled spectra :  = = = = = = = = = = = = = = = = = = = = = = = =
 2010 !
 2011         ELSE
 2012 !
 2013 ! Write ID
 2014 !
 2015           WRITE (NDS,901) PNTNME, PRVAR, FSC, PRUNIT,           &
 2016                           EMAX*FACSP, PRUNIT
 2017 !
 2018 ! Write heading
 2019 !
 2020           NFRB  = MIN (NFR,25)
 2021 !
 2022           WRITE (NDS,911) (FR(IFR)*FACFR,IFR=2,NFRB,2)
 2023           PNUM(1) = '-----'
 2024           PNUM(2) = '--   '
 2025 !
 2026           IF (NFRB.LT.25) THEN
 2027               WRITE (NDS,921) (PNUM(1),IFR=1, NFRB), PNUM(2)
 2028             ELSE
 2029               WRITE (NDS,921) (PNUM(1),IFR=1, NFRB)
 2030             END IF
 2031 !
 2032 !     write table :
 2033 !
 2034           ITHSEC = NTH + 1
 2035 !
 
 
                                                         w3arrymd.F90  page  38
 
 
 2036           DO ITH= NTH, 1, -1
 2037             INTANG = 270 - NINT (DTHDEG*REAL(ITH-1))
 2038             IF (INTANG.LT.0) THEN
 2039                 ITHSEC = ITH
 2040                 CYCLE
 2041               END IF
 2042             CALL ANGSTR (INTANG, STRANG, 4, 2)
 2043             DO IFR=1, NFRB
 2044               RR = E(IFR,ITH)
 2045               IF (ABS(RR/EMAX).LT.RRCUT) THEN
 2046                   PNUM(IFR) = '     '
 2047                 ELSE
 2048                   WRITE (STRA, FMT='(I5)') NINT (RR*FACSP/FSC)
 2049                   PNUM(IFR) = STRA
 2050                 END IF
 2051               END DO
 2052             WRITE (NDS,931) STRANG, (PNUM(IFR),IFR=1, NFRB)
 2053             END DO
 2054 !
 2055           DO ITH= NTH, ITHSEC, -1
 2056             INTANG = 630 - NINT (DTHDEG*REAL(ITH-1))
 2057             CALL ANGSTR (INTANG, STRANG, 4, 2)
 2058             DO IFR=1, NFRB
 2059               RR = E(IFR,ITH)
 2060               IF (ABS(RR/EMAX).LT.RRCUT) THEN
 2061                   PNUM(IFR) = '     '
 2062                 ELSE
 2063                   WRITE (STRA, FMT='(I5)') NINT (RR*FACSP/FSC)
 2064                   PNUM(IFR) = STRA
 2065                 END IF
 2066               END DO
 2067             WRITE (NDS,931) STRANG, (PNUM(IFR),IFR=1, NFRB)
 2068             END DO
 2069 !
 2070 !     write ending :
 2071 !
 2072           PNUM(1) = '-----'
 2073           PNUM(2) = '--   '
 2074           IF (NFRB.LT.25) THEN
 2075               WRITE (NDS,921) (PNUM(1),IFR=1, NFRB), PNUM(2)
 2076             ELSE
 2077               WRITE (NDS,921) (PNUM(1),IFR=1, NFRB)
 2078             END IF
 2079           WRITE (NDS,950)
 2080 !
 2081         END IF
 2082 !
 2083       RETURN
 2084 !
 2085 ! Formats
 2086 !
 2087   900 FORMAT (/'  Location : ',A/                               &
 2088                '  Spectrum : ',A,' (Normalized) ',              &
 2089                '  Maximum value : ',E8.3,1X,A/)
 2090   901 FORMAT (/'  Location : ',A/                               &
 
 
                                                         w3arrymd.F90  page  39
 
 
 2091                '  Spectrum : ',A,'  Units : ',E8.3,1X,A,        &
 2092                '  Maximum value : ',E8.3,1X,A/)
 2093 !
 2094   910 FORMAT (5X,'  ang.|  frequencies (Hz) '/                  &
 2095               5X,'  deg.|',F6.3,15F8.3)
 2096   920 FORMAT (5X,'  ----+',60A2)
 2097   930 FORMAT (5X,' ',A4,' |',60A2)
 2098 !
 2099   911 FORMAT ('  ang.|  frequencies (Hz) '/                     &
 2100               '  deg.|',12F10.3)
 2101   921 FORMAT ('  ----|',25A5)
 2102   931 FORMAT (' ',A4,' |',25A5)
 2103 !
 2104   950 FORMAT (' ')
 2105 !
 2106 !/
 2107 !/ Internal subroutine ANGSTR ---------------------------------------- /
 2108 !/
 2109       CONTAINS
 2110 !/
 2111 !/ ------------------------------------------------------------------- /
 2112       SUBROUTINE ANGSTR (IANG, SANG, ILEN, INUM)
 2113 !/
 2114 !/                  +-----------------------------------+
 2115 !/                  | WAVEWATCH III           NOAA/NCEP |
 2116 !/                  |           H. L. Tolman            |
 2117 !/                  |                        FORTRAN 90 |
 2118 !/                  | Last update :         29-Nov-1999 |
 2119 !/                  +-----------------------------------+
 2120 !/
 2121 !/    10-Mar-1992 : Final FORTRAN 77                    ( version 1.18 )
 2122 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
 2123 !
 2124 !     INPUT  : IANG --> INTEGER ANGLE (DEGREES)
 2125 !              ILEN --> STRING LENGTH
 2126 !              INUM --> <1 : ONLY FOUR MAIN DIRECTIONS
 2127 !                        1 : N,E,S,W AND NUMERICAL OUTPUT
 2128 !                        2 : EIGHT MAIN DIRECTIONS
 2129 !                       >2 : EIGHT DIRECTIONS + NUMERICAL OUTPUT
 2130 !     OUTPUT : SANG --> STRING
 2131 !
 2132 !/ ------------------------------------------------------------------- /
 2133 !/
 2134 !
 2135       IMPLICIT NONE
 2136 !/
 2137 !/ ------------------------------------------------------------------- /
 2138 !/ Parameter list
 2139 !/
 2140       INTEGER, INTENT(IN)     :: IANG, ILEN, INUM
 2141       CHARACTER, INTENT(OUT)  :: SANG*(*)
 2142 !/
 2143 !/ ------------------------------------------------------------------- /
 2144 !/ Local parameters
 2145 !/
 
 
                                                         w3arrymd.F90  page  40
 
 
 2146       INTEGER                 :: I, J
 2147       CHARACTER               :: SAUX*4
 2148 !/
 2149 !/ ------------------------------------------------------------------- /
 2150 !/
 2151 !     numerical :
 2152 !
 2153       IF (INUM.EQ.1 .OR. INUM.GE.3) THEN
 2154           WRITE (SAUX, FMT='(I4)') IANG
 2155         ELSE
 2156           SAUX = '    '
 2157         END IF
 2158 !
 2159 !     string :
 2160 !
 2161       IF (IANG.EQ.0) THEN
 2162           SAUX = '   N'
 2163         ELSE IF (IANG.EQ.90) THEN
 2164           SAUX = '   E'
 2165         ELSE IF (IANG.EQ.180) THEN
 2166           SAUX = '   S'
 2167         ELSE IF (IANG.EQ.270) THEN
 2168           SAUX = '   W'
 2169         ELSE IF (INUM.GE.2) THEN
 2170           IF (IANG.EQ.45) THEN
 2171               SAUX = '  NE'
 2172             ELSE IF (IANG.EQ.135) THEN
 2173               SAUX = '  SE'
 2174             ELSE IF (IANG.EQ.225) THEN
 2175               SAUX = '  SW'
 2176             ELSE IF (IANG.EQ.315) THEN
 2177               SAUX = '  NW'
 2178             END IF
 2179         END IF
 2180 !
 2181 !     Auxilary string to output :
 2182 !
 2183       DO I=1, ILEN-4
 2184         SANG = ' '
 2185         END DO
 2186       J = 0
 2187       DO I=ILEN-3, ILEN
 2188         J = J + 1
 2189         SANG(I:I) = SAUX(J:J)
 2190         END DO
 2191       RETURN
 2192 !/
 2193 !/ End of ANGSTR ----------------------------------------------------- /
 2194 !/
 2195       END SUBROUTINE ANGSTR
 2196 !/
 2197 !/ End of PRT2DS ----------------------------------------------------- /
 2198 !/
 2199       END SUBROUTINE PRT2DS
 2200 !/
 
 
                                                         w3arrymd.F90  page  41
 
 
 2201 !/ End of module W3ARRYMD -------------------------------------------- /
 2202 !/
 2203       END MODULE W3ARRYMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
