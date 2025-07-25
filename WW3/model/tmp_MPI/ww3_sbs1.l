                                                         ww3_sbs1.F90  page   1
 
 
    1 #include "w3macros.h"
    2       PROGRAM W3SBS1
    3 !/
    4 !/                  +-----------------------------------+
    5 !/                  | WAVEWATCH-III           NOAA/NCEP |
    6 !/                  |           H. L. Tolman            |
    7 !/                  |            A. Chawla              |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         05-Dec-2012 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    04-May-2005 : Origination.                        ( version 3.07 )
   13 !/    11-Aug-2010 : Upgrade for operations and inclusion in svn.
   14 !/                                                    ( version 3.14.4 )
   15 !/    05-Dec-2012 : Making sleep a system call.       ( version 4.11   )
   16 !/
   17 !/    Copyright 2010 National Weather Service (NWS),
   18 !/       National Oceanic and Atmospheric Administration.  All rights
   19 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   20 !/       No unauthorized use without permission.
   21 !/
   22 !  1. Purpose :
   23 !
   24 !     Program shell or driver to run the multi-grid wave model in
   25 !     'half-coupled' mode, that is running side-by-side with a weather
   26 !     model while waiting for wind field to become available.
   27 !
   28 !     This version is set up for running at NCEP with a single input
   29 !     wind file, and requires an additional input file.
   30 !       times.inp  Input file with time stamps. Add to this input file
   31 !                  a time stamp after the field has been properly
   32 !                  added to the wind.ww3 or equavalent file.
   33 !     This file should have the time stamps of fields available in
   34 !     the first auxiliary wind input file (grid).
   35 !
   36 !     Apart from management of the time stepping, this code is
   37 !     identical to ww3_multi.ftn, and reads the corresponding input
   38 !     file ww3_multi.inp
   39 !
   40 !     Note hardwired options and system dependent parts as identified
   41 !     in Section 7.
   42 !
   43 !  2. Method :
   44 !
   45 !     Calling WMWAVE in a loop as wind data become available, with
   46 !     test on wind file.
   47 !
   48 !     In order for this to work properly, the user needs to increment
   49 !     then main wind input file (wind.XXXX) as data become avalable
   50 !     while this program is running. After a new field is added to
   51 !     wind file, the corresponding time stamp in YYYYMMDD YYMMSS
   52 !     format is concatenated to the times.inp file. As the code
   53 !     reads the new time stamp (1X,I8,1Z,I6 format), a test read of
   54 !     the wind file is performed until the file is readable, after
   55 !     which the wave model is run until the new time stamp and the
 
 
                                                         ww3_sbs1.F90  page   2
 
 
   56 !     process is repeated. The test reading of the wind file proved
   57 !     essential on the NCEP IBM systems to deal with file system
   58 !     latencies and buffer flushing.
   59 !
   60 !  3. Parameters :
   61 !
   62 !     Local parameters.
   63 !     ----------------------------------------------------------------
   64 !       SLEEP1  I.P.   Sleep time for testing times file.
   65 !       SLEEP2  I.P.   Sleep time for testing winds file.
   66 !     ----------------------------------------------------------------
   67 !
   68 !  4. Subroutines used :
   69 !
   70 !      Name      Type  Module   Description
   71 !     ----------------------------------------------------------------
   72 !      WMINIT    Subr. WMINITMD Multi-grid model initialization.
   73 !      WMWAVE    Subr. WMWAVEMD Multi-grid model subroutine.
   74 !      WMFINL    Subr. WMFINLMD Multi-grid model finalization.
   75 !      EXTCDE    Subr. W3SERVMD Abort program as graceful as possible.
   76 !      W3SETG    Subr. W3GDATMD Point to Grid data data structure.
   77 !      W3SETI    Subr. W3IDATMD Point to input fields data structure.
   78 !      WMUGET    Subr. WMUNITMD Automatic unit number assignement.
   79 !      WMUSET    Subr.          Automatic unit number assignement.
   80 !
   81 !      MPI_INIT, MPI_COMM_SIZE, MPI_COMM_RANK, MPI_BARRIER,
   82 !         MPI_FINALIZE
   83 !                Subr.          Standard MPI routines.
   84 !
   85 !      RDTIME    Subr. W3MLT    Get next wind time.
   86 !      RDWIND    Subr. W3MLT    Test read next wind field.
   87 !     ----------------------------------------------------------------
   88 !
   89 !  5. Called by :
   90 !
   91 !     None, stand-alone program.
   92 !
   93 !  6. Error messages :
   94 !
   95 !  7. Remarks :
   96 !
   97 !     - Assumptions in this program:
   98 !        1) WNS or WND option with 3 or two data fields is set in the
   99 !           decaration and initialization of the C*3 TYPE.
  100 !        2) Single wind file to be tested, this is the first aux grid
  101 !           with wind defined.
  102 !        3) Needs system SLEEP command, now behind SBS switch.
  103 !/    05-Dec-2012 : Making sleep a system call.       ( version 4.11   )
  104                                                                         
  105 !
  106 !  8. Structure :
  107 !
  108 !     ----------------------------------------------------------------
  109 !      0.  Initialization necessary for driver
  110 !        a General I/O: (implicit in wmmdatmd)
 
 
                                                         ww3_sbs1.F90  page   3
 
 
  111 !        b MPI environment
  112 !        c Identifying output to "screen" unit
  113 !      1.  Initialization of all wave models / grids       ( WMINIT )
  114 !      2.  Open and prepare test files.
  115 !      3.  Run the multi-grid models
  116 !        a Preparations
  117 !        b Catch up with starting time of model            ( RDTIME )
  118 !        c Catch up with test reading of file              ( DWINDE )
  119 !        d Run wave model                                  ( WMWAVE )
  120 !      4.  Finalization of wave model                      ( WMFINL )
  121 !      5.  Finalization of driver
  122 !     ----------------------------------------------------------------
  123 !
  124 !  9. Switches :
  125 !
  126 !       !/F90   F90 extensions.
  127 !
  128 !       !/MPI   Including MPI routines / environment.
  129 !
  130 ! 10. Source code :
  131 !
  132 !/ ------------------------------------------------------------------- /
  133       USE WMINITMD, ONLY: WMINIT
  134       USE WMWAVEMD, ONLY: WMWAVE
  135       USE WMFINLMD, ONLY: WMFINL
  136       USE W3SERVMD, ONLY: EXTCDE
  137       USE W3GDATMD, ONLY: W3SETG
  138       USE W3GDATMD, ONLY: NGRIDS, NAUXGR, NX, NY, GNAME, FILEXT
  139       USE W3IDATMD, ONLY: W3SETI
  140       USE W3IDATMD, ONLY: FLWIND
  141       USE WMMDATMD, ONLY: MDSF
  142       USE WMUNITMD, ONLY: WMUGET, WMUSET
  143       USE W3TIMEMD
  144 !/
  145       USE WMMDATMD, ONLY: MDSE, MDST, MDSS, NMPROC, IMPROC, NMPSCR,   &
  146                           NRGRD, STIME, ETIME
  147 !/
  148       IMPLICIT NONE
  149 !
  150       INCLUDE "mpif.h"
  151 !/
  152 !/ ------------------------------------------------------------------- /
  153 !/ Local parameters
  154 !/
  155       INTEGER              :: MPI_COMM = -99, IERR, NDST1, NDST2 = -1,&
  156                               NXW = -1, NYW = -1, TNEXT(2), TOLD(2),  &
  157                               I
  158       INTEGER              :: IERR_MPI
  159       INTEGER, PARAMETER   :: SLEEP1 = 10 , SLEEP2 = 10
  160       INTEGER, ALLOCATABLE :: TEND(:,:)
  161       REAL                 :: DTTST
  162 !      CHARACTER(LEN=3)     :: TSFLD, TYPE = 'WNS'
  163       CHARACTER(LEN=3)     :: TSFLD, TYPE = 'WND'
  164       CHARACTER(LEN=13)    :: TSSTR
  165 !/
 
 
                                                         ww3_sbs1.F90  page   4
 
 
  166 !/ ------------------------------------------------------------------- /
  167 ! 0.  Initialization necessary for driver
  168 ! 0.a General I/O: all can start with initialization in wmmdatmd
  169 !
  170 ! 0.b MPI environment: Here, we use MPI_COMM_WORLD
  171 !
  172       CALL MPI_INIT      ( IERR_MPI )
  173       MPI_COMM = MPI_COMM_WORLD
  174       CALL MPI_COMM_SIZE ( MPI_COMM, NMPROC, IERR_MPI )
  175       CALL MPI_COMM_RANK ( MPI_COMM, IMPROC, IERR_MPI )
  176       IMPROC = IMPROC + 1
  177 !
  178 ! 0.c Identifying output to "screen" unit
  179 !
  180       IF ( IMPROC .EQ. NMPSCR ) WRITE (*,900)
  181 !
  182 !/ ------------------------------------------------------------------- /
  183 ! 1.  Initialization of all wave models / grids
  184 !     Use only one of the calls ....
  185 !
  186 ! ... Log and screen output, no separate test output file
  187 !
  188 !     CALL WMINIT ( 8, 9, 6, 6, 6, 'ww3_multi.inp', MPI_COMM )
  189 !
  190 ! ... Screen output disabled
  191 !
  192 !     CALL WMINIT ( 8, 9, 9, 6, 6, 'ww3_multi.inp', MPI_COMM )
  193 !
  194 ! ... Separate test output file and file preamble defined
  195 !
  196 !     CALL WMINIT ( 8, 9, 6, 10, 6, 'ww3_multi.inp', MPI_COMM,        &
  197 !                   './data/' )
  198 !
  199 ! ... Separate test output file
  200 !
  201       CALL WMINIT ( 8, 9, 6, 10, 6, 'ww3_multi.inp', MPI_COMM )
  202 !
  203 !/ ------------------------------------------------------------------- /
  204 ! 2.  Setting up test files
  205 !
  206       CALL WMUGET ( MDSE, MDST, NDST1, 'INP' )
  207       CALL WMUSET ( MDSE, MDST, NDST1, .TRUE., 'I/O',                 &
  208                     NAME='times.inp',                                 &
  209                     DESC='times file for sbs driver' )
  210       OPEN (NDST1,FILE='times.inp',STATUS='OLD',ERR=820,IOSTAT=IERR)
  211 !
  212       DO I=-1, -NAUXGR, -1
  213         CALL W3SETG ( I, MDSE, MDST )
  214         CALL W3SETI ( I, MDSE, MDST )
  215         IF ( FLWIND ) THEN
  216             IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,920) FILEXT
  217             NXW    = NX
  218             NYW    = NY
  219             NDST2  = MDSF(I,3)
  220             EXIT
 
 
                                                         ww3_sbs1.F90  page   5
 
 
  221           END IF
  222         END DO
  223 !
  224       IF ( NXW .EQ. -1 ) GOTO 825
  225       IF ( NDST2 .EQ. -1 ) GOTO 825
  226 !
  227 !/ ------------------------------------------------------------------- /
  228 ! 3.  Run the wave model
  229 ! 3.a Prepping, initial time stamp
  230 !
  231       ALLOCATE ( TEND(2,NRGRD) )
  232 !
  233       CALL RDTIME ( NDST1, TNEXT )
  234       IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,930) TNEXT
  235 !
  236 ! 3.b Catch up with starting time as needed
  237 !
  238       DO
  239         TOLD   = TNEXT
  240         CALL RDTIME ( NDST1, TNEXT )
  241         DTTST  = DSEC21 ( TNEXT , STIME )
  242         IF ( DTTST .GT. 0. ) THEN
  243             IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,930) TNEXT
  244           ELSE IF ( DTTST .EQ. 0. ) THEN
  245             IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,930) TNEXT
  246             EXIT
  247           ELSE
  248             BACKSPACE NDST1
  249             TNEXT  = TOLD
  250             EXIT
  251           END IF
  252         END DO
  253 !
  254 ! 3.c Test readig of initial fields
  255 !
  256       CALL RDWIND ( NDST2, TNEXT, NXW, NYW, .FALSE. )
  257 !
  258 ! 3.d Loop to run the model
  259 !
  260       DO
  261 !
  262         CALL RDTIME ( NDST1, TNEXT )
  263         IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,930) TNEXT
  264         CALL RDWIND ( NDST2, TNEXT, NXW, NYW, .TRUE. )
  265         IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,931)
  266 !
  267         DTTST  = DSEC21 ( TNEXT , ETIME )
  268         IF ( DTTST .LT. 0. ) THEN
  269             TNEXT  = ETIME
  270             DTTST  = 0.
  271           END IF
  272 !
  273         DO I=1, NRGRD
  274           TEND(:,I) = TNEXT(:)
  275           END DO
 
 
                                                         ww3_sbs1.F90  page   6
 
 
  276 !
  277         CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
  278         CALL WMWAVE ( TEND )
  279 !
  280         DTTST  = DSEC21 ( TNEXT , ETIME )
  281         IF ( DTTST .LE. 0 ) EXIT
  282 !
  283         END DO
  284 !
  285       DEALLOCATE ( TEND )
  286 !
  287 !/ ------------------------------------------------------------------- /
  288 ! 4.  Finalize the wave model
  289 !
  290       CALL WMFINL
  291 !
  292 !/ ------------------------------------------------------------------- /
  293 ! 5   Finalize the driver
  294 !
  295       IF ( IMPROC .EQ. NMPSCR ) WRITE (*,999)
  296 !
  297       CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
  298       CALL MPI_FINALIZE  ( IERR_MPI )
  299 !
  300       GO TO 888
  301 !
  302   820 CONTINUE
  303       WRITE (MDSS,1020) IERR
  304       CALL EXTCDE ( 20 )
  305 !
  306   825 CONTINUE
  307       WRITE (MDSS,1025) NDST2
  308       CALL EXTCDE ( 25 )
  309 !
  310   888 CONTINUE
  311 !
  312 ! Formats
  313 !
  314   900 FORMAT (/15X,'     *** WAVEWATCH III Multi-grid shell ***    '/ &
  315                15X,'================================================='/&
  316                15X,'                           side-by-side version'/)
  317 !
  318   920 FORMAT ( '     WIND DATA FILE USED IS wind.',A)
  319   930 FORMAT (/'     WIND DATA FOUND AT TIME : ',I8.8,1X,I6.6)
  320   931 FORMAT (' ')
  321 !
  322   999 FORMAT (//'  End of program '/                                  &
  323                ' ========================================'/           &
  324                '          WAVEWATCH III Multi-grid shell '/)
  325 !
  326  1020 FORMAT (/' *** WAVEWATCH-III ERROR IN W3SBS1 : '/               &
  327                '     ERROR IN OPENING TIMES FILE'/                    &
  328                '     IOSTAT =',I5/)
  329 !
  330  1025 FORMAT (/' *** WAVEWATCH-III ERROR IN W3SBS1 : '/               &
 
 
                                                         ww3_sbs1.F90  page   7
 
 
  331                '     WIND FILE NOT FOUND, NDST2 =  ',I8/)
  332 !
  333 !/
  334 !/ Internal subroutines RDTIME and RDWIND ---------------------------- /
  335 !/
  336       CONTAINS
  337 !/ ------------------------------------------------------------------- /
  338       SUBROUTINE RDTIME ( NDS, TIME )
  339 !/
  340 !/                  +-----------------------------------+
  341 !/                  | WAVEWATCH-III           NOAA/NCEP |
  342 !/                  |           H. L. Tolman            |
  343 !/                  |                        FORTRAN 90 |
  344 !/                  | Last update :         05-Dec-2012 !
  345 !/                  +-----------------------------------+
  346 !/
  347 !/    10-Aug-2010 : Origination.                      ( version 3.14.4 )
  348 !/    05-Dec-2012 : Making sleep a system call.       ( version 4.11   )
  349 !/
  350 !  1. Purpose :
  351 !
  352 !     Internal subroutine to get next time in time file, including
  353 !     waiting until the file is there.
  354 !
  355 !  3. Parameters :
  356 !
  357 !     Parameter list
  358 !     ----------------------------------------------------------------
  359 !       NDS     Int.   I   Unit number for times file.
  360 !       TIME    I.A.   O   Next time in times file.
  361 !     ----------------------------------------------------------------
  362 !
  363 ! 10. Source code :
  364 !
  365 !/ ------------------------------------------------------------------- /
  366 !/ Parameter list
  367 !/
  368       INTEGER, INTENT(IN)     :: NDS
  369       INTEGER, INTENT(OUT)    :: TIME(2)
  370 !/
  371 !/ ------------------------------------------------------------------- /
  372 !/ Local parameters
  373 !/
  374 !/
  375 ! -------------------------------------------------------------------- /
  376 ! 1.  Reading loop
  377 !
  378       DO
  379 !
  380         READ (NDS,910,END=110,ERR=810,IOSTAT=IERR) TIME
  381         EXIT
  382 !
  383   110   CONTINUE
  384         IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS, 911 )
  385         BACKSPACE NDS
 
 
                                                         ww3_sbs1.F90  page   8
 
 
  386 !
  387         END DO
  388 !
  389       RETURN
  390 !
  391 ! Escape locations read errors --------------------------------------- *
  392 !
  393   810 CONTINUE
  394       WRITE (MDSS,1010) IERR
  395       CALL EXTCDE ( 10 )
  396 !
  397 ! Formats
  398 !
  399   910 FORMAT (1X,I8,1X,I6)
  400 !
  401   911 FORMAT (/'    END OF TIMES FILE REACHED FOR WIND DATA '/        &
  402                '       WAITING BEFORE CHECKING AGAIN')
  403 !
  404  1010 FORMAT (/' *** WAVEWATCH-III ERROR IN W3SBS1/RDTIME : '/        &
  405                 '     ERROR IN OPENING TIMES FILE'/                   &
  406                 '     IOSTAT =',I5/)
  407 !/
  408 !/ End of RDTIME ----------------------------------------------------- /
  409 !/
  410       END SUBROUTINE RDTIME
  411 !/ ------------------------------------------------------------------- /
  412       SUBROUTINE RDWIND ( NDS, TIME, NX, NY, REWIND )
  413 !/
  414 !/                  +-----------------------------------+
  415 !/                  | WAVEWATCH-III           NOAA/NCEP |
  416 !/                  |           H. L. Tolman            |
  417 !/                  |                        FORTRAN 90 |
  418 !/                  | Last update :         05-Dec-2012 !
  419 !/                  +-----------------------------------+
  420 !/
  421 !/    10-Aug-2010 : Origination.                      ( version 3.14.4 )
  422 !/    05-Dec-2012 : Making sleep a system call.       ( version 4.11   )
  423 !/
  424 !  1. Purpose :
  425 !
  426 !     Internal subroutine to test readnext wind fields from the data
  427 !     file, including testing to see if file was read properly.
  428 !
  429 !  3. Parameters :
  430 !
  431 !     Parameter list
  432 !     ----------------------------------------------------------------
  433 !       NDS     Int.   I   Unit number for times file.
  434 !       TIME    I.A.   I   Next time in times file.
  435 !       NX,NY   Int.   I   Grid size.
  436 !       REWIND  Log.   I   Flag for wind file rewind.
  437 !     ----------------------------------------------------------------
  438 !
  439 ! 10. Source code :
  440 !
 
 
                                                         ww3_sbs1.F90  page   9
 
 
  441 !/ ------------------------------------------------------------------- /
  442 !/ Parameter list
  443 !/
  444       INTEGER, INTENT(IN)     :: NDS, TIME(2), NX, NY
  445       LOGICAL, INTENT(IN)     :: REWIND
  446 !/
  447 !/ ------------------------------------------------------------------- /
  448 !/ Local parameters
  449 !/
  450       INTEGER                 :: TTIME(2), IX, IY
  451       INTEGER, SAVE           :: NREW  = 0
  452       REAL                    :: DTTST, XXX(NX,NY)
  453 !
  454 ! -------------------------------------------------------------------- /
  455 ! 1.  Loops
  456 !
  457       DO
  458 !
  459 ! ... Inner loop reading
  460 !
  461         DO
  462 !
  463           NREW   = NREW + 1
  464           READ (NDS,END=140,ERR=140) TTIME
  465 !
  466           NREW   = NREW + 1
  467           READ (NDS,END=130,ERR=130) ((XXX(IX,IY),IX=1,NX),IY=1,NY)
  468 !
  469           NREW   = NREW + 1
  470           READ (NDS,END=120,ERR=120) ((XXX(IX,IY),IX=1,NX),IY=1,NY)
  471 !
  472           IF ( TYPE .EQ. 'WNS' ) THEN
  473               NREW   = NREW + 1
  474               READ (NDS,END=110,ERR=110) ((XXX(IX,IY),IX=1,NX),IY=1,NY)
  475             END IF
  476 !
  477           EXIT
  478 !
  479   110     CONTINUE
  480           BACKSPACE NDS
  481           NREW   = NREW - 1
  482   120     CONTINUE
  483           BACKSPACE NDS
  484           NREW   = NREW - 1
  485   130     CONTINUE
  486           BACKSPACE NDS
  487           NREW   = NREW - 1
  488   140     CONTINUE
  489           BACKSPACE NDS
  490           NREW   = NREW - 1
  491 !
  492           IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,900)
  493 !
  494           END DO
  495 !
 
 
                                                         ww3_sbs1.F90  page  10
 
 
  496 ! ... Outer loop catching up
  497 !
  498         DTTST  = DSEC21 ( TIME , TTIME )
  499 !
  500         IF ( DTTST .LT. 0. ) THEN
  501             IF ( IMPROC .EQ. NMPSCR ) WRITE (MDSS,901) TTIME
  502           ELSE IF ( DTTST .EQ. 0. ) THEN
  503             EXIT
  504           ELSE
  505             GOTO 800
  506           END IF
  507 !
  508         END DO
  509 !
  510 ! ... Rewind all
  511 !
  512       IF ( REWIND ) THEN
  513 !
  514          IF ( IMPROC.EQ.NMPSCR .AND. NREW.GT.4 ) WRITE (MDSS,902) NREW
  515 !
  516           DO I=1, NREW
  517             BACKSPACE NDS
  518             END DO
  519 !
  520           NREW   = 0
  521 !
  522         END IF
  523 !
  524       RETURN
  525 !
  526 ! Escape locations read errors --------------------------------------- *
  527 !
  528   800 CONTINUE
  529       WRITE (MDSS,1010)
  530       CALL EXTCDE ( 10 )
  531 !
  532 ! Formats
  533 !
  534   900 FORMAT ('       FILE NOT YET COMPLETE ... ')
  535   901 FORMAT ('         SKIPPING FILE FOR ',I8.8,I7.6)
  536   902 FORMAT ('         REWINDING FILE BY ',I4,' RECORDS')
  537 !
  538  1010 FORMAT (/' *** WAVEWATCH-III ERROR IN W3SBS1/RDWIND : '/        &
  539                '     FILE READ PAST EXPECTED TIME '/)
  540 !
  541 !/
  542 !/ End of RDWIND ----------------------------------------------------- /
  543 !/
  544       END SUBROUTINE RDWIND
  545 !/
  546 !/ End of W3SBS1 ----------------------------------------------------- /
  547 !/
  548       END PROGRAM W3SBS1
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
