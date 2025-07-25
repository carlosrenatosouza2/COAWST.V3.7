                                                         ww3_shel.F90  page   1
 
 
    1                                                                          >
    2                                                                           
    3 !#include "w3macros.h"
    4                       
    5                       
    6                       
    7                       
    8                       
    9                       
   10                       
   11                       
   12                       
   13                       
   14                       
   15                       
   16                       
   17                       
   18                       
   19                       
   20                       
   21                       
   22                       
   23                       
   24                       
   25                       
   26                       
   27                       
   28                       
   29                       
   30                       
   31                       
   32                       
   33                       
   34                       
   35                       
   36                       
   37                       
   38                       
   39                       
   40                       
   41                       
   42                       
   43                       
   44                       
   45                       
   46                       
   47                       
   48                       
   49                       
   50                       
   51                       
   52                       
   53                       
   54                       
   55                       
 
 
                                                         ww3_shel.F90  page   2
 
 
   56                       
   57                       
   58                       
   59                       
   60                       
   61                       
   62                       
   63                       
   64                       
   65                       
   66                       
   67                       
   68                       
   69                       
   70                       
   71                       
   72                       
   73                       
   74                       
   75                       
   76                       
   77                       
   78                       
   79                       
   80                       
   81                       
   82                       
   83                       
   84                       
   85                       
   86                       
   87                       
   88                       
   89                       
   90                       
   91                       
   92                       
   93                       
   94                       
   95                       
   96                       
   97                       
   98                       
   99                       
  100                       
  101                       
  102                       
  103                       
  104                       
  105                       
  106                       
  107                       
  108                       
  109                       
  110                       
 
 
                                                         ww3_shel.F90  page   3
 
 
  111                       
  112                       
  113                       
  114                       
  115                       
  116                       
  117                       
  118                       
  119                       
  120                       
  121                       
  122                       
  123                       
  124                       
  125                       
  126                       
  127                       
  128                       
  129                       
  130                       
  131                       
  132                       
  133                       
  134                       
  135                       
  136                       
  137                       
  138                       
  139                       
  140                       
  141                       
  142                       
  143                       
  144                       
  145                       
  146                       
  147                       
  148                       
  149                       
  150                       
  151                       
  152                       
  153                       
  154                       
  155                       
  156                       
  157                       
  158                       
  159                       
  160                       
  161                       
  162                       
  163                       
  164                       
  165                       
 
 
                                                         ww3_shel.F90  page   4
 
 
  166                       
  167                       
  168                       
  169                       
  170                       
  171                       
  172                       
  173                       
  174                       
  175                       
  176                       
  177                       
  178                       
  179                       
  180                       
  181                       
  182                       
  183                       
  184                       
  185                       
  186                       
  187                       
  188                       
  189                       
  190                       
  191                       
  192                       
  193                       
  194                       
  195                       
  196                       
  197                       
  198                       
  199                       
  200                       
  201                       
  202                       
  203                       
  204                       
  205                       
  206                       
  207                       
  208                       
  209                       
  210                       
  211                       
  212                       
  213                       
  214                       
  215                       
  216                       
  217                       
  218                       
  219                       
  220                       
 
 
                                                         ww3_shel.F90  page   5
 
 
  221                       
  222                       
  223                       
  224                       
  225                       
  226                       
  227                       
  228                       
  229                       
  230                       
  231                       
  232                       
  233                       
  234                       
  235                       
  236                       
  237                       
  238                       
  239                       
  240                       
  241                       
  242                       
  243                       
  244                       
  245                       
  246                       
  247                       
  248                       
  249                       
  250                       
  251                       
  252                       
  253                       
  254                       
  255                       
  256                       
  257                       
  258                       
  259                       
  260                       
  261                       
  262                       
  263                       
  264                       
  265                       
  266                       
  267                       
  268                       
  269                       
  270                       
  271                       
  272                       
  273                       
  274                       
  275                       
 
 
                                                         ww3_shel.F90  page   6
 
 
  276                       
  277                       
  278                       
  279                       
  280                       
  281                       
  282                       
  283                       
  284 !/ ------------------------------------------------------------------- /
  285       SUBROUTINE WW3_init (MyCOMM)
  286 !/
  287 !/                  +-----------------------------------+
  288 !/                  | WAVEWATCH III           NOAA/NCEP |
  289 !/                  |           H. L. Tolman            |
  290 !/                  |                        FORTRAN 90 |
  291 !/                  | Last update :         25-Sep-2020 |
  292 !/                  +-----------------------------------+
  293 !/
  294 !/    19-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
  295 !/    19-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
  296 !/    08-Mar-2000 : Fix time managament bug.            ( version 2.04 )
  297 !/    09-Jan-2001 : Fix FOUT allocation bug.            ( version 2.05 )
  298 !/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
  299 !/    25-Jan-2002 : Data assimilation set up.           ( version 2.17 )
  300 !/    08-May-2002 : Clean up for timers.                ( version 2.21 )
  301 !/    26-Aug-2002 : Generalizing timer.                 ( version 2.22 )
  302 !/    26-Dec-2002 : Continuously moving grid.           ( version 3.02 )
  303 !/    01-Aug-2003 : Continuously moving grid, input.    ( version 3.03 )
  304 !/    07-Oct-2003 : Fixed NHMAX test.                   ( version 3.05 )
  305 !/    05-Jan-2005 : Multiple grid version.              ( version 3.06 )
  306 !/    04-May-2005 : Change to MPI_COMM[_WAVE.           ( version 3.07 )
  307 !/    26-Jun-2006 : Add wiring for output type 6.       ( version 3.07 )
  308 !/    28-Jun-2006 : Adding file name preamble.          ( version 3.09 )
  309 !/    28-Oct-2006 : Adding partitioning options.        ( version 3.10 )
  310 !/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
  311 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  312 !/    30-Oct-2009 : Fix format statement 2945.          ( version 3.14 )
  313 !/                  (T. J. Campbell, NRL)
  314 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
  315 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  316 !/    13-Sep-2009 : Add coupling option                 ( version 3.14_SH>
  317 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
  318 !/                  (W. E. Rogers & T. J. Campbell, NRL)
  319 !/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.4 )
  320 !/                  (A. Roland and F. Ardhuin)
  321 !/    23-Nov-2011 : Comments clean up                   ( version 4.04 )
  322 !/    06-Mar-2012 : Repairing test output.              ( version 4.07 )
  323 !/    03-Sep-2012 : Output initialization time.         ( version 4.10 )
  324 !/    27-Sep-2012 : Implement use of tidal constituents ( version 4.08 )
  325 !/    04-Feb-2014 : Switched clock to DATE_AND_TIME     ( version 4.18 )
  326 !/                  (A. Chawla and Mark Szyszka)
  327 !/    23-Apr-2015 : Adding NCEP Coupler                 ( version 5.06 )
  328 !/                  (A. Chawla and Dmitry Sheinin)
  329 !/    24-Apr-2015 : Adding OASIS coupling calls         ( version 5.07 )
  330 !/                  (M. Accensi & F. Ardhuin, IFREMER)
 
 
                                                         ww3_shel.F90  page   7
 
 
  331 !/    11-May-2015 : Checks dates for output types       ( version 5.08 )
  332 !/    26-Mar-2018 : Sea-point only Wnd/Cur input. JGLi  ( version 6.02 )
  333 !/    15-May-2018 : Update namelist                     ( version 6.05 )
  334 !/    06-Jun-2018 : Add PDLIB/MEMCHECK/NETCDF_QAD/DEBUGINIT ( version 6.0>
  335 !/    14-Sep-2018 : Remove PALM implementation          ( version 6.06 )
  336 !/    04-Oct-2019 : Inline Output implementation        ( version 6.07 )
  337 !/                  (Roberto Padilla-Hernandez)
  338 !/    16-Jul-2020 : Variable coupling time step         ( version 7.08 )
  339 !/    25-Sep-2020 : Oasis coupling at T+0               ( version 7.10 )
  340 !/
  341 !/    Copyright 2009-2012 National Weather Service (NWS),
  342 !/       National Oceanic and Atmospheric Administration.  All rights
  343 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  344 !/       No unauthorized use without permission.
  345 !/
  346 !  1. Purpose :
  347 !
  348 !     A generic shell for WAVEWATCH III, using preformatted
  349 !     input fields.
  350 !
  351 !  2. Method :
  352 !
  353 !     Driver for the actual wave model (W3WAVE).
  354 !
  355 !     Files : ww3_shel.inp  Input commands for shell.
  356 !             level.ww3     Water level fields (optional).
  357 !             current.ww3   Current fields (optional).
  358 !             wind.ww3      Wind fields (optional).
  359 !             muddens.ww3   Mud parameter (optional)
  360 !             mudthk.ww3    Mud parameter (optional)
  361 !             mudvisc.ww3   Mud parameter (optional)
  362 !             ice(n).ww3    Ice parameters (n=1 to 5) (optional)
  363 !             ice.ww3       ice concentration fields (optional).
  364 !             data0.ww3     Files with assimilation data (optional).
  365 !             data1.ww3
  366 !             data2.ww3
  367 !
  368 !     The file names of the input files are set in W3FLDO
  369 !
  370 !  3. Parameters :
  371 !
  372 !     Local parameters.
  373 !     ----------------------------------------------------------------
  374 !       NHMAX   I.P.  Maximum number of homogeneous fields.
  375 !
  376 !       NDSI    Int.  General input unit number (shell only).
  377 !       NDSS    Int.  Scratch file.
  378 !       NDSO    Int.  General output unit number (shell only).
  379 !       NDSE    Int.  Error output unit number (shell only).
  380 !       NDST    Int.  Test output unit number (shell only).
  381 !       NDSF    I.A.  Field files unit numbers (shell only).
  382 !       FLH     L.A.  Flags for homogeneous fields.
  383 !       FLAGSC  L.A.  Flags for coupling fields
  384 !       FLAGSCI Log.  Flags for ice ic1 ic5 coupling
  385 !       NH      I.A.  Number of times for homogeneous fields.
 
 
                                                         ww3_shel.F90  page   8
 
 
  386 !       THO     I.A.  Times of homogeneous fields.
  387 !       TIME0   I.A.  Starting time.
  388 !       TIMEN   I.A.  Ending time.
  389 !     ----------------------------------------------------------------
  390 !
  391 !       NDS, NTRACE, ..., see W3WAVE
  392 !
  393 !  4. Subroutines used :
  394 !
  395 !      Name      Type  Module   Description
  396 !     ----------------------------------------------------------------
  397 !      W3NMOD    Subr. W3GDATMD Set nummber of data structures
  398 !      W3SETG    Subr.   Id.    Point to data structure.
  399 !      W3NDAT    Subr. W3WDATMD Set nummber of data structures
  400 !      W3SETW    Subr.   Id.    Point to data structure.
  401 !      W3NMOD    Subr. W3ADATMD Set nummber of data structures
  402 !      W3NAUX    Subr.   Id.    Point to data structure.
  403 !      W3NOUT    Subr. W3ODATMD Set nummber of data structures
  404 !      W3SETO    Subr.   Id.    Point to data structure.
  405 !      W3NINP    Subr. W3IDATMD Set nummber of data structures
  406 !      W3SETI    Subr.   Id.    Point to data structure.
  407 !
  408 !      NEXTLN    Subr. W3SERVMD Skip to next input line.
  409 !      STME21    Subr. W3TIMEMD Print date and time readable.
  410 !      DSEC21    Func.   Id.    Difference between times.
  411 !      TICK21    Subr.   Id.    Increment time.
  412 !
  413 !      W3FLDO    Subr. W3FLDSMD Opens and checks input files.
  414 !      W3FLDG    Subr.   Id.    Reads from input files.
  415 !      W3FLDD    Subr.   Id.    Reads from data files.
  416 !      W3FLDH    Subr.   Id.    Udates homogeneous fields.
  417 !
  418 !      W3INIT    Subr. W3INITMD Wave model initialization.
  419 !      W3READFLGRD Subr. W3IOGOMD Reading output fields flags.
  420 !      W3WAVE    Subr. W3WAVEMD Wave model.
  421 !      W3WDAS    Subr. W3WDASMD Data assimilation interface.
  422 !
  423 !      MPI_INIT, MPI_COMM_SIZE, MPI_COMM_RANK, MPI_BARRIER,
  424 !         MPI_FINALIZE
  425 !                Subr.          Standard MPI routines.
  426 !     ----------------------------------------------------------------
  427 !
  428 !  5. Called by :
  429 !
  430 !     None, stand-alone program.
  431 !
  432 !  6. Error messages :
  433 !
  434 !     - Checks on I-O.
  435 !     - Check on time interval.
  436 !
  437 !  7. Remarks :
  438 !
  439 !     - A rigourous input check is made in W3INIT.
  440 !     - See W3WDAS for documentation on the set-up of the data
 
 
                                                         ww3_shel.F90  page   9
 
 
  441 !       assimilation.
  442 !     - in "7.a.2 Check if update is needed"
  443 !       Field is updated when compute time is past old input time, and
  444 !       (in case of homogeneous input field),  grabs field value at next
  445 !       input time, which may in fact be far in the future from current
  446 !       compute time. Example: user says
  447 !       field=1   on 19680101 000000 and
  448 !       field=100 on 20160101 000000
  449 !       then on if 7.a.2 is reached on 19680101 010000, WW3 will set
  450 !       field to 100.
  451 !
  452 !  8. Structure :
  453 !
  454 !     ----------------------------------------------------------------
  455 !        0.   Set up data structures.                ( W3NMOD, etc. )
  456 !        1.   I-O setup.
  457 !          a  For shell.
  458 !          b  For WAVEWATCH III.
  459 !          c  Local parameters.
  460 !        2.   Define input fields
  461 !        3.   Set time frame.
  462 !        4.   Define output
  463 !          a  Loop over types, do
  464 !        +--------------------------------------------------------+
  465 !        | b    Process standard line                             |
  466 !        | c    If type 1: fields of mean wave parameters         |
  467 !        | d    If type 2: point output                           |
  468 !        | e    If type 3: track output                           |
  469 !        | f    If type 4: restart files                          |
  470 !        | g    If type 5: boundary output                        |
  471 !        | h    If type 6: separated wave fields                  |
  472 !        | i    If type 7: coupling fields                        |
  473 !        +--------------------------------------------------------+
  474 !        5.   Initialzations
  475 !          a  Wave model.                              ( W3INIT )
  476 !          b  Read homogeneous field data.
  477 !          c  Prepare input files.                     ( W3FLDO )
  478 !          d  Set field times.
  479 !        6.   If no input fields required, run model in a single
  480 !             sweep and exit.                          ( W3WAVE )
  481 !        7.   Run model with input
  482 !             Do until end time is reached
  483 !        +--------------------------------------------------------+
  484 !        | a  Determine next time interval and input fields.      |
  485 !        |   1  Preparation                                       |
  486 !        |      Loop over input fields                            |
  487 !        | +------------------------------------------------------|
  488 !        | | 2  Check if update is needed                         |
  489 !        | | 3  Update time and fields                 ( W3FLDG ) |
  490 !        | |                                           ( W3FLDH ) |
  491 !        | | 4  Update next ending time                           |
  492 !        | +------------------------------------------------------|
  493 !        | b  Run wave model.                          ( W3WAVE ) |
  494 !        | c  If requested, data assimilation.         ( W3WDAS ) |
  495 !        | d  Final output if needed.                  ( W3WAVE ) |
 
 
                                                         ww3_shel.F90  page  10
 
 
  496 !        | e  Check time                                          |
  497 !        +--------------------------------------------------------+
  498 !     ----------------------------------------------------------------
  499 !
  500 !  9. Switches :
  501 !
  502 !       !/SHRD  Switch for shared / distributed memory architecture.
  503 !       !/DIST  Id.
  504 !       !/MPI   Id.
  505 !
  506 !       !/MGW   Moving grid wind correction.
  507 !       !/MGP   Moving grid propagation correction.
  508 !
  509 !       !/T     Enable test output.
  510 !       !/O7    Echo input homogeneous fields.
  511 !
  512 !       !/NCO   NCEP NCO modifications for operational implementation.
  513 !
  514 !       !/F90   Timer function included for F90.
  515 !
  516 !       !/NCC   Ncep Coupler
  517 !
  518 ! 10. Source code :
  519 !
  520 !/ ------------------------------------------------------------------- /
  521       USE W3GDATMD
  522       USE W3WDATMD, ONLY: TIME, VA, W3NDAT, W3DIMW, W3SETW
  523       USE W3WDATMD, ONLY: TIMEEND
  524       USE W3ADATMD, ONLY: W3NAUX, W3DIMA, W3SETA
  525       USE W3IDATMD
  526       USE W3ODATMD, ONLY: W3NOUT, W3SETO
  527       USE W3ODATMD, ONLY: NAPROC, IAPROC, NAPOUT, NAPERR, NOGRP,      &
  528                           NGRPP, IDOUT, FNMPRE, IOSTYP, NOTYPE
  529       USE W3ODATMD, ONLY: FLOGRR, FLOGR, OFILES
  530 !/
  531       USE W3FLDSMD
  532       USE W3INITMD
  533       USE W3WAVEMD
  534       USE W3WDASMD
  535 !/
  536       USE W3IOGRMD, ONLY: W3IOGR
  537       USE W3IOGOMD, ONLY: W3READFLGRD, FLDOUT, W3FLGRDFLAG
  538       USE W3IORSMD, ONLY: OARST
  539       USE W3IOPOMD
  540       USE W3SERVMD, ONLY : NEXTLN, EXTCDE
  541       USE W3TIMEMD
  542                   
  543                   
  544       USE CWSTWVCP
  545       USE MCT_COUPLER_PARAMS
  546 !
  547       USE W3NMLSHELMD
  548       IMPLICIT NONE
  549 !
  550       INCLUDE "mpif.h"
 
 
                                                         ww3_shel.F90  page  11
 
 
  551 !/
  552 !/ ------------------------------------------------------------------- /
  553 !/ Local PARAMETER statements
  554 !/
  555       INTEGER, PARAMETER  :: NHMAX =    200
  556 !/
  557 !/ ------------------------------------------------------------------- /
  558 !/ Local parameters
  559 !/
  560       TYPE(NML_DOMAIN_T)       :: NML_DOMAIN
  561       TYPE(NML_INPUT_T)        :: NML_INPUT
  562       TYPE(NML_OUTPUT_TYPE_T)  :: NML_OUTPUT_TYPE
  563       TYPE(NML_OUTPUT_DATE_T)  :: NML_OUTPUT_DATE
  564       TYPE(NML_HOMOG_COUNT_T)  :: NML_HOMOG_COUNT
  565       TYPE(NML_HOMOG_INPUT_T), ALLOCATABLE  :: NML_HOMOG_INPUT(:)
  566 !
  567       INTEGER             :: NDSI, NDSI2, NDSS, NDSO, NDSE, NDST, NDSL,&
  568                              NDSEN, IERR, J, I, ILOOP, IPTS, NPTS,     &
  569                              NDTNEW, MPI_COMM = -99,                   &
  570                              FLAGTIDE, COUPL_COMM, IH, N_TOT
  571       INTEGER             :: NDSF(-7:7), NDS(13), NTRACE(2), NDT(5:7), &
  572                              TIME0(2), TIMEN(2), TTIME(2), TTT(2),     &
  573                              NH(-7:8), THO(2,-7:8,NHMAX), RCLD(5:7),   &
  574                              NODATA(5:7), ODAT(40), IPRT(6) = 0,       &
  575                              STARTDATE(8), STOPDATE(8), IHH(-7:8)
  576 !
  577       INTEGER :: COAWSTED, ccount
  578       INTEGER             :: CLKDT1(8), CLKDT2(8), CLKDT3(8)
  579       INTEGER             :: IERR_MPI
  580       INTEGER            :: MyCOMM
  581 !
  582       REAL                :: FACTOR, DTTST, XX, YY,                    &
  583                              HA(NHMAX,-7:8), HD(NHMAX,-7:8),           &
  584                              HS(NHMAX,-7:8)
  585       REAL                :: CLKFIN, CLKFEL
  586       REAL, ALLOCATABLE   :: X(:), Y(:), XXX(:,:), DATA0(:,:),         &
  587                              DATA1(:,:), DATA2(:,:)
  588 !
  589       DOUBLE PRECISION    :: STARTJULDAY, STOPJULDAY
  590 !
  591       CHARACTER(LEN=1)    :: COMSTR, FLAGTFC(-7:8)
  592       CHARACTER(LEN=3)    :: IDSTR(-7:8), IDTST
  593       CHARACTER(LEN=6)    :: YESXNO
  594       CHARACTER(LEN=40)   :: PN
  595       CHARACTER(LEN=40),                                               &
  596               ALLOCATABLE :: PNAMES(:)
  597       CHARACTER(LEN=13)   :: IDFLDS(-7:8)
  598       CHARACTER(LEN=20)   :: STRNG
  599       CHARACTER(LEN=23)   :: DTME21
  600       CHARACTER(LEN=30)   :: IDOTYP(8)
  601       CHARACTER(LEN=80)   :: LINE
  602       CHARACTER(LEN=256)  :: TMPLINE, TEST
  603       CHARACTER(LEN=1024) :: FLDIN
  604       CHARACTER(LEN=1024) :: FLDRST=''
  605       CHARACTER(LEN=80)   :: LINEIN
 
 
                                                         ww3_shel.F90  page  12
 
 
  606       CHARACTER(LEN=8)    :: WORDS(7)=''
  607                                         
  608 !
  609       LOGICAL             :: FLLSTL, FLLSTI, FLFLG, FLHOM, TFLAGI,     &
  610                              PRTFRM, FLAGSCI, FLGNML
  611       LOGICAL             :: FLGRD(NOGRP,NGRPP), FLGD(NOGRP),          &
  612                              FLGR2(NOGRP,NGRPP), FLG2(NOGRP),          &
  613                              FLAGSTIDE(4), FLH(-7:8), FLGDAS(3),       &
  614                              FLLST_ALL(-7:8)
  615       LOGICAL             :: DEBUG_NCC = .FALSE.
  616       LOGICAL             :: FLHYBR = .FALSE.
  617 !
  618 !/
  619 !/ ------------------------------------------------------------------- /
  620 !/
  621       DATA IDFLDS / 'ice param. 1 ' , 'ice param. 2 ' ,               &
  622                     'ice param. 3 ' , 'ice param. 4 ' ,               &
  623                     'ice param. 5 ' ,                                 &
  624                     'mud density  ' , 'mud thkness  ' ,               &
  625                     'mud viscos.  ' ,                                 &
  626                     'water levels ' , 'currents     ' ,               &
  627                     'winds        ' , 'ice fields   ' ,               &
  628                     'mean param.  ' , '1D spectra   ' ,               &
  629                     '2D spectra   ' , 'moving grid  ' /
  630       DATA IDOTYP / 'Fields of mean wave parameters' ,                &
  631                     'Point output                  ' ,                &
  632                     'Track point output            ' ,                &
  633                     'Restart files                 ' ,                &
  634                     'Nesting data                  ' ,                &
  635                     'Partitioned wave field data   ' ,                &
  636                     'Fields for coupling           ' ,                &
  637                     'Restart files second request  '/
  638       DATA IDSTR  / 'IC1', 'IC2', 'IC3', 'IC4', 'IC5', 'MDN', 'MTH',  &
  639                     'MVS', 'LEV', 'CUR', 'WND', 'ICE', 'DT0', 'DT1',  &
  640                     'DT2', 'MOV' /
  641 !
  642       FLGR2 = .FALSE.
  643       FLAGSTIDE(:) = .FALSE.
  644       FLH(:)       = .FALSE.
  645 !
  646       CALL DATE_AND_TIME ( VALUES=CLKDT1 )
  647 !
  648 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  649 ! 0.  Set up data structures
  650 !
  651       COAWSTED=1
  652 !
  653       CALL W3NMOD ( 1, 6, 6 )
  654       CALL W3NDAT (    6, 6 )
  655       CALL W3NAUX (    6, 6 )
  656       CALL W3NOUT (    6, 6 )
  657       CALL W3NINP (    6, 6 )
  658 !
  659       CALL W3SETG ( 1, 6, 6 )
  660       CALL W3SETW ( 1, 6, 6 )
 
 
                                                         ww3_shel.F90  page  13
 
 
  661       CALL W3SETA ( 1, 6, 6 )
  662       CALL W3SETO ( 1, 6, 6 )
  663       CALL W3SETI ( 1, 6, 6 )
  664                              
  665 !
  666  
  667       IF (COAWSTED.EQ.0) THEN
  668       CALL MPI_INIT      ( IERR_MPI )
  669       MPI_COMM = MPI_COMM_WORLD
  670       END IF
  671       MPI_COMM = MyCOMM
  672       WAV_COMM_WORLD = MyCOMM
  673 !
  674       CALL MPI_COMM_SIZE ( MPI_COMM, NAPROC, IERR_MPI )
  675       CALL MPI_COMM_RANK ( MPI_COMM, IAPROC, IERR_MPI )
  676       IAPROC = IAPROC + 1
  677 !
  678  
  679 !
  680 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  681 ! 1.  IO set-up
  682 ! 1.a For shell
  683 !
  684       NDSI   = 10
  685       NDSS   = 90
  686       NDSO   =  6
  687       NDSE   =  6
  688       NDST   =  6
  689       NDSL   = 50
  690                  
  691                  
  692       NDSF(-7)  = 1008
  693       NDSF(-6)  = 1009
  694       NDSF(-5)  = 1010
  695       NDSF(-4)  = 1011
  696       NDSF(-3)  = 1012
  697       NDSF(-2)  = 1013
  698       NDSF(-1)  = 1014
  699       NDSF(0)   = 1015
  700                       
  701       NDSF(1)  = 11
  702       NDSF(2)  = 12
  703       NDSF(3)  = 13
  704       NDSF(4)  = 14
  705       NDSF(5)  = 15
  706       NDSF(6)  = 16
  707       NDSF(7)  = 17
  708 !
  709       NAPOUT = 1
  710       NAPERR = 1
  711 !
  712  
  713       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,900)
  714 !
  715       IF ( IAPROC .EQ. NAPERR ) THEN
 
 
                                                         ww3_shel.F90  page  14
 
 
  716         NDSEN  = NDSE
  717       ELSE
  718         NDSEN  = -1
  719       END IF
  720 !
  721  
  722 !
  723 ! 1.b For WAVEWATCH III (See W3INIT)
  724 !
  725       NDS( 1) = 20
  726       NDS( 2) =  6
  727       NDS( 3) = 21
  728       NDS( 4) =  6
  729       NDS( 5) = 30
  730       NDS( 6) = 30
  731       NDS( 7) = 31
  732       NDS( 8) = 32
  733       NDS( 9) = 33
  734       NDS(10) = 35
  735       NDS(11) = 22
  736       NDS(12) = 23
  737       NDS(13) = 34
  738 !
  739       NTRACE(1) =  NDS(3)
  740       NTRACE(2) =  10
  741 !
  742 ! 1.c Local parameters
  743 !
  744 ! Default COMSTR to "$" (for when using nml input files)
  745       COMSTR = "$"
  746 !
  747 ! inferred from context: these flags (FL) are to indicate that the last (>
  748 !   field has been read from a file.
  749       FLLSTL = .FALSE. ! This is associated with J.EQ.1 (wlev)
  750       FLLSTI = .FALSE. ! This is associated with J.EQ.4 (ice)
  751       FLLST_ALL = .FALSE. ! For all
  752                                    
  753 ! If using experimental mud or ice physics, additional lines will
  754 !  be read in from ww3_shel.inp and applied, so JFIRST is changed from
  755 !  its initialization setting "JFIRST=1" to some lower value.
  756                                                              
  757                                                              
  758                                                              
  759 !
  760 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  761 ! 2.  Define input fields
  762 !
  763  
  764 !
  765 ! process ww3_prnc namelist
  766 !
  767       INQUIRE(FILE=TRIM(FNMPRE)//"ww3_shel.nml", EXIST=FLGNML)
  768       IF (FLGNML) THEN
  769         ! Read namelist
  770         CALL W3NMLSHEL (MPI_COMM, NDSI, TRIM(FNMPRE)//'ww3_shel.nml',  &
 
 
                                                         ww3_shel.F90  page  15
 
 
  771                         NML_DOMAIN, NML_INPUT, NML_OUTPUT_TYPE,        &
  772                         NML_OUTPUT_DATE, NML_HOMOG_COUNT,             &
  773                         NML_HOMOG_INPUT, IERR)
  774                                               
  775 ! 2.1 forcing flags
  776                    
  777         FLH(-7:8)=.FALSE.
  778         FLAGTFC(-7)=TRIM(NML_INPUT%FORCING%ICE_PARAM1)
  779         FLAGTFC(-6)=TRIM(NML_INPUT%FORCING%ICE_PARAM2)
  780         FLAGTFC(-5)=TRIM(NML_INPUT%FORCING%ICE_PARAM3)
  781         FLAGTFC(-4)=TRIM(NML_INPUT%FORCING%ICE_PARAM4)
  782         FLAGTFC(-3)=TRIM(NML_INPUT%FORCING%ICE_PARAM5)
  783         FLAGTFC(-2)=TRIM(NML_INPUT%FORCING%MUD_DENSITY)
  784         FLAGTFC(-1)=TRIM(NML_INPUT%FORCING%MUD_THICKNESS)
  785         FLAGTFC(0)=TRIM(NML_INPUT%FORCING%MUD_VISCOSITY)
  786         FLAGTFC(1)=TRIM(NML_INPUT%FORCING%WATER_LEVELS)
  787         FLAGTFC(2)=TRIM(NML_INPUT%FORCING%CURRENTS)
  788         FLAGTFC(3)=TRIM(NML_INPUT%FORCING%WINDS)
  789         FLAGTFC(4)=TRIM(NML_INPUT%FORCING%ICE_CONC)
  790         FLAGTFC(5)=TRIM(NML_INPUT%ASSIM%MEAN)
  791         FLAGTFC(6)=TRIM(NML_INPUT%ASSIM%SPEC1D)
  792         FLAGTFC(7)=TRIM(NML_INPUT%ASSIM%SPEC2D)
  793                                                
  794         IF (TRIM(NML_INPUT%FORCING%ICE_PARAM1) .EQ. 'H') THEN
  795           FLAGTFC(-7)='T'
  796           FLH(-7)=.TRUE.
  797         END IF
  798         IF (TRIM(NML_INPUT%FORCING%ICE_PARAM2) .EQ. 'H') THEN
  799           FLAGTFC(-6)='T'
  800           FLH(-6)=.TRUE.
  801         END IF
  802         IF (TRIM(NML_INPUT%FORCING%ICE_PARAM3) .EQ. 'H') THEN
  803           FLAGTFC(-5)='T'
  804           FLH(-5)=.TRUE.
  805         END IF
  806         IF (TRIM(NML_INPUT%FORCING%ICE_PARAM4) .EQ. 'H') THEN
  807           FLAGTFC(-4)='T'
  808           FLH(-4)=.TRUE.
  809         END IF
  810         IF (TRIM(NML_INPUT%FORCING%ICE_PARAM5) .EQ. 'H') THEN
  811           FLAGTFC(-3)='T'
  812           FLH(-3)=.TRUE.
  813         END IF
  814         IF (TRIM(NML_INPUT%FORCING%MUD_DENSITY) .EQ. 'H') THEN
  815           FLAGTFC(-2)='T'
  816           FLH(-2)=.TRUE.
  817         END IF
  818         IF (TRIM(NML_INPUT%FORCING%MUD_THICKNESS) .EQ. 'H') THEN
  819           FLAGTFC(-1)='T'
  820           FLH(-1)=.TRUE.
  821         END IF
  822         IF (TRIM(NML_INPUT%FORCING%MUD_VISCOSITY) .EQ. 'H') THEN
  823           FLAGTFC(0)='T'
  824           FLH(0)=.TRUE.
  825         END IF
 
 
                                                         ww3_shel.F90  page  16
 
 
  826         IF (TRIM(NML_INPUT%FORCING%WATER_LEVELS) .EQ. 'H') THEN
  827           FLAGTFC(1)='T'
  828           FLH(1)=.TRUE.
  829         END IF
  830         IF (TRIM(NML_INPUT%FORCING%CURRENTS) .EQ. 'H') THEN
  831           FLAGTFC(2)='T'
  832           FLH(2)=.TRUE.
  833         END IF
  834         IF (TRIM(NML_INPUT%FORCING%WINDS) .EQ. 'H') THEN
  835           FLAGTFC(3)='T'
  836           FLH(3)=.TRUE.
  837         END IF
  838         IF (TRIM(NML_INPUT%FORCING%ICE_CONC) .EQ. 'H') THEN
  839           FLAGTFC(4)='T'
  840           FLH(4)=.TRUE.
  841         END IF
  842               
  843         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,920)
  844         DO J=JFIRST, 7
  845           IF ( J .LE. 4 ) THEN
  846             IF (FLAGTFC(J).EQ.'T') THEN
  847               INFLAGS1(J)=.TRUE.
  848               FLAGSC(J)=.FALSE.
  849             END IF
  850             IF (FLAGTFC(J).EQ.'F') THEN
  851               INFLAGS1(J)=.FALSE.
  852               FLAGSC(J)=.FALSE.
  853             END IF
  854             IF (FLAGTFC(J).EQ.'C') THEN
  855               INFLAGS1(J)=.TRUE.
  856               FLAGSC(J)=.TRUE.
  857             END IF
  858             FLH(J) = FLH(J) .AND. INFLAGS1(J)
  859           ELSE
  860             FLH(J) = .FALSE.
  861             IF (FLAGTFC(J).EQ.'T') THEN
  862               INFLAGS1(J)=.TRUE.
  863               FLAGSC(J)=.FALSE.
  864             END IF
  865             IF (FLAGTFC(J).EQ.'F') THEN
  866               INFLAGS1(J)=.FALSE.
  867               FLAGSC(J)=.FALSE.
  868             END IF
  869             IF (FLAGTFC(J).EQ.'C') THEN
  870               INFLAGS1(J)=.TRUE.
  871               FLAGSC(J)=.TRUE.
  872             END IF
  873           END IF
  874           IF ( INFLAGS1(J) ) THEN
  875             YESXNO = 'YES/--'
  876           ELSE
  877             YESXNO = '---/NO'
  878           END IF
  879           IF ( FLH(J) ) THEN
  880             STRNG  = '(homogeneous field) '
 
 
                                                         ww3_shel.F90  page  17
 
 
  881           ELSE IF ( FLAGSC(J) ) THEN
  882             STRNG  = '(coupling field) '
  883           ELSE
  884             STRNG  = '                    '
  885           END IF
  886           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,921) IDFLDS(J), YESXNO, S>
  887         END DO
  888               
  889         INFLAGS1(8) = .FALSE.
  890         FLH(8)   = .FALSE.
  891         IF ( INFLAGS1(8) .AND. IAPROC.EQ.NAPOUT )                        >
  892              WRITE (NDSO,921) IDFLDS(8), 'YES/--', ' '
  893 !
  894         FLFLG  = INFLAGS1(-7) .OR. INFLAGS1(-6) .OR. INFLAGS1(-5) .OR. IN>
  895                  .OR. INFLAGS1(-3) .OR. INFLAGS1(-2) .OR. INFLAGS1(-1)
  896                  .OR. INFLAGS1(0)  .OR. INFLAGS1(1)  .OR. INFLAGS1(2)
  897                  .OR. INFLAGS1(3)  .OR. INFLAGS1(4)  .OR. INFLAGS1(5)
  898                  .OR. INFLAGS1(6)  .OR. INFLAGS1(7)
  899         FLHOM  = FLH(-7) .OR. FLH(-6) .OR. FLH(-5) .OR. FLH(-4)       &
  900                  .OR. FLH(-3) .OR. FLH(-2) .OR. FLH(-1) .OR. FLH(0)   &
  901                  .OR. FLH(1) .OR. FLH(2) .OR. FLH(3) .OR. FLH(4) .OR. FLH>
  902 !
  903         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,922)
  904 !
  905 !       INFLAGS2 is just "initial value of INFLAGS1", i.e. does *not* get
  906 !          changed when model reads last record of ice.ww3
  907         INFLAGS2=INFLAGS1
  908                          
  909                          
  910                          
  911                          
  912 ! 2.2 Time setup
  913                 
  914         READ(NML_DOMAIN%START,*) TIME0
  915         CALL T2D(TIME0,STARTDATE,IERR)
  916         CALL D2J(STARTDATE,STARTJULDAY,IERR)
  917         READ(NML_DOMAIN%STOP,*) TIMEN
  918         CALL T2D(TIMEN,STOPDATE,IERR)
  919         CALL D2J(STOPDATE,STOPJULDAY,IERR)
  920                                           
  921 ! 2.3 Domain setup
  922                   
  923         IOSTYP = NML_DOMAIN%IOSTYP
  924         CALL W3IOGR ( 'GRID', NDSF(5) )
  925         IF ( FLAGLL ) THEN
  926           FACTOR = 1.
  927         ELSE
  928           FACTOR = 1.E-3
  929         END IF
  930               
  931 ! 2.4 Output dates
  932                   
  933         READ(NML_OUTPUT_DATE%FIELD%START, *)   ODAT(1), ODAT(2)
  934         READ(NML_OUTPUT_DATE%FIELD%STRIDE, *)  ODAT(3)
  935         READ(NML_OUTPUT_DATE%FIELD%STOP, *)    ODAT(4), ODAT(5)
 
 
                                                         ww3_shel.F90  page  18
 
 
  936                                                                
  937         READ(NML_OUTPUT_DATE%FIELD%OUTFFILE, *)  OFILES(1)
  938 !        OUTPTS(I)%OUTSTRIDE(1)=ODAT(3,I)
  939                                          
  940         READ(NML_OUTPUT_DATE%POINT%START, *)   ODAT(6), ODAT(7)
  941         READ(NML_OUTPUT_DATE%POINT%STRIDE, *)  ODAT(8)
  942         READ(NML_OUTPUT_DATE%POINT%STOP, *)    ODAT(9), ODAT(10)
  943                                                                 
  944         READ(NML_OUTPUT_DATE%POINT%OUTFFILE, *)  OFILES(2)
  945 !        OUTPTS(I)%OUTSTRIDE(2)=ODAT(8,I)
  946                                          
  947         READ(NML_OUTPUT_DATE%TRACK%START, *)   ODAT(11), ODAT(12)
  948         READ(NML_OUTPUT_DATE%TRACK%STRIDE, *)  ODAT(13)
  949         READ(NML_OUTPUT_DATE%TRACK%STOP, *)    ODAT(14), ODAT(15)
  950         READ(NML_OUTPUT_DATE%RESTART%START, *)   ODAT(16), ODAT(17)
  951         READ(NML_OUTPUT_DATE%RESTART%STRIDE, *)  ODAT(18)
  952         READ(NML_OUTPUT_DATE%RESTART%STOP, *)    ODAT(19), ODAT(20)
  953         READ(NML_OUTPUT_DATE%RESTART2%START, *)   ODAT(36), ODAT(37)
  954         READ(NML_OUTPUT_DATE%RESTART2%STRIDE, *)  ODAT(38)
  955         READ(NML_OUTPUT_DATE%RESTART2%STOP, *)    ODAT(39), ODAT(40)
  956         READ(NML_OUTPUT_DATE%BOUNDARY%START, *)   ODAT(21), ODAT(22)
  957         READ(NML_OUTPUT_DATE%BOUNDARY%STRIDE, *)  ODAT(23)
  958         READ(NML_OUTPUT_DATE%BOUNDARY%STOP, *)    ODAT(24), ODAT(25)
  959         READ(NML_OUTPUT_DATE%PARTITION%START, *)   ODAT(26), ODAT(27)
  960         READ(NML_OUTPUT_DATE%PARTITION%STRIDE, *)  ODAT(28)
  961         READ(NML_OUTPUT_DATE%PARTITION%STOP, *)    ODAT(29), ODAT(30)
  962         READ(NML_OUTPUT_DATE%COUPLING%START, *)   ODAT(31), ODAT(32)
  963         READ(NML_OUTPUT_DATE%COUPLING%STRIDE, *)  ODAT(33)
  964         READ(NML_OUTPUT_DATE%COUPLING%STOP, *)    ODAT(34), ODAT(35)
  965                                                                     
  966         ! set the time stride at 0 or more
  967         ODAT(3) = MAX ( 0 , ODAT(3) )
  968         ODAT(8) = MAX ( 0 , ODAT(8) )
  969         ODAT(13) = MAX ( 0 , ODAT(13) )
  970         ODAT(18) = MAX ( 0 , ODAT(18) )
  971         ODAT(23) = MAX ( 0 , ODAT(23) )
  972         ODAT(28) = MAX ( 0 , ODAT(28) )
  973         ODAT(33) = MAX ( 0 , ODAT(33) )
  974         ODAT(38) = MAX ( 0 , ODAT(38) )
  975 !
  976 ! 2.5 Output types
  977                   
  978         NPTS   = 0
  979         NOTYPE = 6
  980         DO J = 1, NOTYPE
  981 !          OUTPTS(I)%OFILES(J)=OFILES(J)
  982           IF ( ODAT(5*(J-1)+3) .NE. 0 ) THEN
  983                                             
  984 ! Type 1: fields of mean wave parameters
  985             IF ( J .EQ. 1 ) THEN
  986               FLDOUT = NML_OUTPUT_TYPE%FIELD%LIST
  987               CALL W3FLGRDFLAG ( NDSO, NDSO, NDSE, FLDOUT, FLGD,     &
  988                                  FLGRD, IAPROC, NAPOUT, IERR )
  989               IF ( IERR .NE. 0 ) GOTO 2222
  990                                           
 
 
                                                         ww3_shel.F90  page  19
 
 
  991                                           
  992 ! Type 2: point output
  993             ELSE IF ( J .EQ. 2 ) THEN
  994               OPEN (NDSL, FILE=TRIM(FNMPRE)//TRIM(NML_OUTPUT_TYPE%POINT%F>
  995                     FORM='FORMATTED', STATUS='OLD', ERR=2104, IOSTAT=IERR)
  996                                                                           
  997               ! first loop to count the number of points
  998               ! second loop to allocate the array and store the points
  999               IPTS = 0
 1000               DO ILOOP=1,2
 1001                 REWIND (NDSL)
 1002 !
 1003                 IF ( ILOOP.EQ.2) THEN
 1004                   NPTS = IPTS
 1005                   IF ( NPTS.GT.0 ) THEN
 1006                     ALLOCATE ( X(NPTS), Y(NPTS), PNAMES(NPTS) )
 1007                     IPTS = 0 ! reset counter to be reused for next do loop
 1008                   ELSE
 1009                     ALLOCATE ( X(1), Y(1), PNAMES(1) )
 1010                     GOTO 2054
 1011                   END IF
 1012                 END IF
 1013 !
 1014                 DO
 1015                   READ (NDSL,*,ERR=2004,IOSTAT=IERR) TMPLINE
 1016                   ! if end of file or stopstring, then exit
 1017                   IF ( IERR.NE.0 .OR. INDEX(TMPLINE,"STOPSTRING").NE.0 ) >
 1018                   ! leading blanks removed and placed on the right
 1019                   TEST = ADJUSTL ( TMPLINE )
 1020                   IF ( TEST(1:1).EQ.COMSTR .OR. LEN_TRIM(TEST).EQ.0 ) THEN
 1021                     ! if comment or blank line, then skip
 1022                     CYCLE
 1023                   ELSE
 1024                     ! otherwise, backup to beginning of line
 1025                     BACKSPACE ( NDSL, ERR=2004, IOSTAT=IERR)
 1026                     READ (NDSL,*,ERR=2004,IOSTAT=IERR) XX, YY, PN
 1027                   END IF
 1028                   IPTS = IPTS + 1
 1029                   IF ( ILOOP .EQ. 1 ) CYCLE
 1030                   IF ( ILOOP .EQ. 2 ) THEN
 1031                     X(IPTS)      = XX
 1032                     Y(IPTS)      = YY
 1033                     PNAMES(IPTS) = PN
 1034                     IF ( IAPROC .EQ. NAPOUT ) THEN
 1035                       IF ( FLAGLL ) THEN
 1036                         IF ( IPTS .EQ. 1 ) THEN
 1037                           WRITE (NDSO,2945)                     &
 1038                                  FACTOR*XX, FACTOR*YY, PN
 1039                         ELSE
 1040                           WRITE (NDSO,2946) IPTS,               &
 1041                                  FACTOR*XX, FACTOR*YY, PN
 1042                         END IF
 1043                       ELSE
 1044                         IF ( IPTS .EQ. 1 ) THEN
 1045                           WRITE (NDSO,2955)                     &
 
 
                                                         ww3_shel.F90  page  20
 
 
 1046                                  FACTOR*XX, FACTOR*YY, PN
 1047                         ELSE
 1048                           WRITE (NDSO,2956) IPTS,               &
 1049                                  FACTOR*XX, FACTOR*YY, PN
 1050                         END IF
 1051                       END IF
 1052                     END IF
 1053                   END IF ! ILOOP.EQ.2
 1054                 END DO ! end of file
 1055               END DO ! ILOOP
 1056               CLOSE(NDSL)
 1057                          
 1058 ! Type 3: track output
 1059             ELSE IF ( J .EQ. 3 ) THEN
 1060               TFLAGI = NML_OUTPUT_TYPE%TRACK%FORMAT
 1061               IF ( .NOT. TFLAGI ) NDS(11) = -NDS(11)
 1062               IF ( IAPROC .EQ. NAPOUT ) THEN
 1063                 IF ( .NOT. TFLAGI ) THEN
 1064                   WRITE (NDSO,3945) 'input', 'UNFORMATTED'
 1065                 ELSE
 1066                   WRITE (NDSO,3945) 'input', 'FORMATTED'
 1067                 END IF
 1068               END IF
 1069                     
 1070 ! Type 6: partitioning
 1071             ELSE IF ( J .EQ. 6 ) THEN
 1072               IPRT(1) = NML_OUTPUT_TYPE%PARTITION%X0
 1073               IPRT(2) = NML_OUTPUT_TYPE%PARTITION%XN
 1074               IPRT(3) = NML_OUTPUT_TYPE%PARTITION%NX
 1075               IPRT(4) = NML_OUTPUT_TYPE%PARTITION%Y0
 1076               IPRT(5) = NML_OUTPUT_TYPE%PARTITION%YN
 1077               IPRT(6) = NML_OUTPUT_TYPE%PARTITION%NY
 1078               PRTFRM = NML_OUTPUT_TYPE%PARTITION%FORMAT
 1079 !
 1080               IF ( IAPROC .EQ. NAPOUT ) THEN
 1081                 IF ( PRTFRM ) THEN
 1082                   YESXNO = 'YES/--'
 1083                 ELSE
 1084                   YESXNO = '---/NO'
 1085                 END IF
 1086                 WRITE (NDSO,6945) IPRT, YESXNO
 1087               END IF
 1088                     
 1089                     
 1090             END IF ! J
 1091           END IF ! ODAT
 1092         END DO ! J
 1093                   
 1094         ! Extra fields to be written in the restart
 1095         FLDRST = NML_OUTPUT_TYPE%RESTART%EXTRA
 1096         CALL W3FLGRDFLAG ( NDSO, NDSO, NDSE, FLDRST, FLOGR,  &
 1097                            FLOGRR, IAPROC, NAPOUT, IERR )
 1098         IF ( IERR .NE. 0 ) GOTO 2222
 1099                                     
 1100         ! force minimal allocation to avoid memory seg fault
 
 
                                                         ww3_shel.F90  page  21
 
 
 1101         IF ( .NOT.ALLOCATED(X) .AND. NPTS.EQ.0 ) ALLOCATE ( X(1), Y(1), P>
 1102                                                                           
 1103 ! 2.6 Homogeneous field data
 1104                             
 1105         IF ( FLHOM ) THEN
 1106           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951)                   &
 1107                           'Homogeneous field data (and moving grid) ...'
 1108                                                                         
 1109           NH(-7) = NML_HOMOG_COUNT%N_IC1
 1110           NH(-6) = NML_HOMOG_COUNT%N_IC2
 1111           NH(-5) = NML_HOMOG_COUNT%N_IC3
 1112           NH(-4) = NML_HOMOG_COUNT%N_IC4
 1113           NH(-3) = NML_HOMOG_COUNT%N_IC5
 1114           NH(-2) = NML_HOMOG_COUNT%N_MDN
 1115           NH(-1) = NML_HOMOG_COUNT%N_MTH
 1116           NH(0)  = NML_HOMOG_COUNT%N_MVS
 1117           NH(1)  = NML_HOMOG_COUNT%N_LEV
 1118           NH(2)  = NML_HOMOG_COUNT%N_CUR
 1119           NH(3)  = NML_HOMOG_COUNT%N_WND
 1120           NH(4)  = NML_HOMOG_COUNT%N_ICE
 1121           NH(8)  = NML_HOMOG_COUNT%N_MOV
 1122 !
 1123           N_TOT = NML_HOMOG_COUNT%N_TOT
 1124 !
 1125           DO J=JFIRST,8
 1126             IF ( NH(J) .GT. NHMAX ) GOTO 2006
 1127           END DO
 1128                 
 1129                 
 1130           ! Store homogeneous fields
 1131           IF ( N_TOT .GT. 0 ) THEN
 1132             IHH(:)=0
 1133             DO IH=1,N_TOT
 1134               READ(NML_HOMOG_INPUT(IH)%NAME,*) IDTST
 1135               SELECT CASE (IDTST)
 1136               CASE ('IC1')
 1137                 J=-7
 1138               CASE ('IC2')
 1139                 J=-6
 1140               CASE ('IC3')
 1141                 J=-5
 1142               CASE ('IC4')
 1143                 J=-4
 1144               CASE ('IC5')
 1145                 J=-3
 1146               CASE ('MDN')
 1147                 J=-2
 1148               CASE ('MTH')
 1149                 J=-1
 1150               CASE ('MVS')
 1151                 J=0
 1152               CASE ('LEV')
 1153                 J=1
 1154               CASE ('CUR')
 1155                 J=2
 
 
                                                         ww3_shel.F90  page  22
 
 
 1156               CASE ('WND')
 1157                 J=3
 1158               CASE ('ICE')
 1159                 J=4
 1160               CASE ('MOV')
 1161                 J=8
 1162               CASE DEFAULT
 1163                 GOTO 2062
 1164               END SELECT
 1165               IHH(J)=IHH(J)+1
 1166               READ(NML_HOMOG_INPUT(IH)%DATE,*) THO(:,J,IHH(J))
 1167               HA(IHH(J),J) = NML_HOMOG_INPUT(IH)%VALUE1
 1168               HD(IHH(J),J) = NML_HOMOG_INPUT(IH)%VALUE2
 1169               HS(IHH(J),J) = NML_HOMOG_INPUT(IH)%VALUE3
 1170             END DO
 1171           END IF
 1172                 
 1173           DO J=JFIRST, 8
 1174             IF ( FLH(J) .AND. IAPROC.EQ.NAPOUT ) THEN
 1175               WRITE (NDSO,952) NH(J), IDFLDS(J)
 1176               DO I=1, NH(J)
 1177                 IF ( ( J .LE. 1 ) .OR. ( J .EQ. 4 ) ) THEN
 1178                   WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
 1179                                    HA(I,J)
 1180                 ELSE IF ( ( J .EQ. 2 ) .OR. (J .EQ. 8) ) THEN
 1181                   WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
 1182                                    HA(I,J), HD(I,J)
 1183                 ELSE IF ( J .EQ. 3 ) THEN
 1184                   WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
 1185                                    HA(I,J), HD(I,J), HS(I,J)
 1186                 END IF
 1187               END DO
 1188             END IF
 1189           END DO
 1190 !
 1191           IF ( ( FLH(-7) .AND. (NH(-7).EQ.0) ) .OR.                     &
 1192                ( FLH(-6) .AND. (NH(-6).EQ.0) ) .OR.                     &
 1193                ( FLH(-5) .AND. (NH(-5).EQ.0) ) .OR.                     &
 1194                ( FLH(-4) .AND. (NH(-4).EQ.0) ) .OR.                     &
 1195                ( FLH(-3) .AND. (NH(-3).EQ.0) ) .OR.                     &
 1196                ( FLH(-2) .AND. (NH(-2).EQ.0) ) .OR.                     &
 1197                ( FLH(-1) .AND. (NH(-1).EQ.0) ) .OR.                     &
 1198                ( FLH(0)  .AND. (NH(0).EQ.0)  ) .OR.                     &
 1199                ( FLH(1)  .AND. (NH(1).EQ.0)  ) .OR.                     &
 1200                ( FLH(2)  .AND. (NH(2).EQ.0)  ) .OR.                     &
 1201                ( FLH(3)  .AND. (NH(3).EQ.0)  ) .OR.                     &
 1202                ( FLH(4)  .AND. (NH(4).EQ.0)  ) .OR.                     &
 1203                ( FLH(8) .AND. (NH(8).EQ.0) ) ) GOTO 2007
 1204 !
 1205         END IF ! FLHOM
 1206                       
 1207                       
 1208       END IF ! FLGNML
 1209                      
 1210                      
 
 
                                                         ww3_shel.F90  page  23
 
 
 1211                      
 1212 !
 1213 ! process old ww3_shel.inp format
 1214 !
 1215       IF (.NOT. FLGNML) THEN
 1216                             
 1217         OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_shel.inp',STATUS='OLD',IOSTAT=>
 1218         REWIND (NDSI)
 1219 !AR: I changed the error handling for err=2002, see commit message ...
 1220         READ (NDSI,'(A)') COMSTR
 1221         IF (COMSTR.EQ.' ') COMSTR = '$'
 1222         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,901) COMSTR
 1223                                                          
 1224 ! 2.1 forcing flags
 1225                    
 1226         DO J=JFIRST, 7
 1227           CALL NEXTLN ( COMSTR , NDSI , NDSEN )
 1228           IF ( J .LT. 4 ) THEN
 1229             READ (NDSI,*) FLAGTFC(J), FLH(J)
 1230           ELSE
 1231             READ (NDSI,*) FLAGTFC(J)
 1232             FLH(J) = .FALSE.
 1233           END IF
 1234         END DO
 1235               
 1236         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,920)
 1237         DO J=JFIRST, 7
 1238           IF ( J .LT. 4 ) THEN
 1239             IF (FLAGTFC(J).EQ.'T') THEN
 1240               INFLAGS1(J)=.TRUE.
 1241               FLAGSC(J)=.FALSE.
 1242             END IF
 1243             IF (FLAGTFC(J).EQ.'F') THEN
 1244               INFLAGS1(J)=.FALSE.
 1245               FLAGSC(J)=.FALSE.
 1246             END IF
 1247             IF (FLAGTFC(J).EQ.'C') THEN
 1248               INFLAGS1(J)=.TRUE.
 1249               FLAGSC(J)=.TRUE.
 1250             END IF
 1251             FLH(J) = FLH(J) .AND. INFLAGS1(J)
 1252           ELSE
 1253             FLH(J) = .FALSE.
 1254             IF (FLAGTFC(J).EQ.'T') THEN
 1255               INFLAGS1(J)=.TRUE.
 1256               FLAGSC(J)=.FALSE.
 1257             END IF
 1258             IF (FLAGTFC(J).EQ.'F') THEN
 1259               INFLAGS1(J)=.FALSE.
 1260               FLAGSC(J)=.FALSE.
 1261             END IF
 1262             IF (FLAGTFC(J).EQ.'C') THEN
 1263               INFLAGS1(J)=.TRUE.
 1264               FLAGSC(J)=.TRUE.
 1265             END IF
 
 
                                                         ww3_shel.F90  page  24
 
 
 1266           END IF
 1267           IF ( INFLAGS1(J) ) THEN
 1268             YESXNO = 'YES/--'
 1269           ELSE
 1270             YESXNO = '---/NO'
 1271           END IF
 1272           IF ( FLH(J) ) THEN
 1273             STRNG  = '(homogeneous field) '
 1274           ELSE IF ( FLAGSC(J) ) THEN
 1275             STRNG  = '(coupling field) '
 1276           ELSE
 1277             STRNG  = '                    '
 1278           END IF
 1279           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,921) IDFLDS(J), YESXNO, S>
 1280         END DO
 1281               
 1282               
 1283 !
 1284         INFLAGS1(8) = .FALSE.
 1285         FLH(8)   = .FALSE.
 1286         IF ( INFLAGS1(8) .AND. IAPROC.EQ.NAPOUT )                        >
 1287              WRITE (NDSO,921) IDFLDS(8), 'YES/--', ' '
 1288 !
 1289         FLFLG  = INFLAGS1(-7) .OR. INFLAGS1(-6) .OR. INFLAGS1(-5) .OR. IN>
 1290                  .OR. INFLAGS1(-3) .OR. INFLAGS1(-2) .OR. INFLAGS1(-1)
 1291                  .OR. INFLAGS1(0)  .OR. INFLAGS1(1)  .OR. INFLAGS1(2)
 1292                  .OR. INFLAGS1(3)  .OR. INFLAGS1(4)  .OR. INFLAGS1(5)
 1293                  .OR. INFLAGS1(6)  .OR. INFLAGS1(7)
 1294         FLHOM  = FLH(-7) .OR. FLH(-6) .OR. FLH(-5) .OR. FLH(-4)       &
 1295                  .OR. FLH(-3) .OR. FLH(-2) .OR. FLH(-1) .OR. FLH(0)   &
 1296                  .OR. FLH(1) .OR. FLH(2) .OR. FLH(3) .OR. FLH(4) .OR. FLH>
 1297 !
 1298         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,922)
 1299 !
 1300 !       INFLAGS2 is just "initial value of INFLAGS1", i.e. does *not* get
 1301 !          changed when model reads last record of ice.ww3
 1302         INFLAGS2=INFLAGS1
 1303                          
 1304                          
 1305                          
 1306 ! 2.2 Time setup
 1307                 
 1308         CALL NEXTLN ( COMSTR , NDSI , NDSEN )
 1309         READ (NDSI,*) TIME0
 1310                            
 1311                            
 1312                            
 1313         CALL NEXTLN ( COMSTR , NDSI , NDSEN )
 1314         READ (NDSI,*) TIMEN
 1315                            
 1316 !
 1317  
 1318 ! 2.3 Domain setup
 1319                   
 1320         CALL NEXTLN ( COMSTR , NDSI , NDSEN )
 
 
                                                         ww3_shel.F90  page  25
 
 
 1321         READ (NDSI,*) IOSTYP
 1322         CALL W3IOGR ( 'GRID', NDSF(5) )
 1323         IF ( FLAGLL ) THEN
 1324           FACTOR = 1.
 1325         ELSE
 1326           FACTOR = 1.E-3
 1327         END IF
 1328               
 1329               
 1330 ! 2.4 Output dates
 1331                   
 1332         NPTS   = 0
 1333         NOTYPE = 6
 1334         DO J = 1, NOTYPE
 1335           CALL NEXTLN ( COMSTR , NDSI , NDSEN )
 1336 !
 1337 ! CHECKPOINT
 1338         IF(J .EQ. 4) THEN
 1339           ODAT(38)=0
 1340           WORDS(1:7)=''
 1341           READ (NDSI,'(A)') LINEIN
 1342           READ(LINEIN,*,iostat=ierr) WORDS
 1343           READ(WORDS( 1 ), * ) ODAT(16)
 1344           READ(WORDS( 2 ), * ) ODAT(17)
 1345           READ(WORDS( 3 ), * ) ODAT(18)
 1346           READ(WORDS( 4 ), * ) ODAT(19)
 1347           READ(WORDS( 5 ), * ) ODAT(20)
 1348           IF (WORDS(6) .EQ. 'T') THEN
 1349             CALL NEXTLN ( COMSTR , NDSI , NDSEN )
 1350             READ (NDSI,*,END=2001,ERR=2002)(ODAT(I),I=5*(8-1)+1,5*8)
 1351             WRITE(*,*)(ODAT(I),I=5*(8-1)+1,5*8)
 1352           END IF
 1353           IF (WORDS(7) .EQ. 'T') THEN
 1354             CALL NEXTLN ( COMSTR , NDSI , NDSEN )
 1355             READ (NDSI,*,END=2001,ERR=2002) FLDRST
 1356           END IF
 1357           CALL W3FLGRDFLAG ( NDSO, NDSO, NDSE, FLDRST, FLOGR,  &
 1358                              FLOGRR, IAPROC, NAPOUT, IERR )
 1359           IF ( IERR .NE. 0 ) GOTO 2222
 1360         ELSE
 1361 !
 1362 !INLINE NEW VARIABLE TO READ IF PRESENT OFILES(J), IF NOT ==0
 1363 !          READ (NDSI,*) (ODAT(I),I=5*(J-1)+1,5*J)
 1364 !          READ (NDSI,*,IOSTAT=IERR) (ODAT(I),I=5*(J-1)+1,5*J),OFILES(J)
 1365         IF(J .LE. 2) THEN
 1366           WORDS(1:6)=''
 1367 !          READ (NDSI,*,END=2001,ERR=2002)(ODAT(I),I=5*(J-1)+1,5*J),OFILE>
 1368           READ (NDSI,'(A)') LINEIN
 1369           READ(LINEIN,*,iostat=ierr) WORDS
 1370 !
 1371           IF(J .EQ. 1) THEN
 1372             READ(WORDS( 1 ), * ) ODAT(1)
 1373             READ(WORDS( 2 ), * ) ODAT(2)
 1374             READ(WORDS( 3 ), * ) ODAT(3)
 1375             READ(WORDS( 4 ), * ) ODAT(4)
 
 
                                                         ww3_shel.F90  page  26
 
 
 1376             READ(WORDS( 5 ), * ) ODAT(5)
 1377           ELSE
 1378             READ(WORDS( 1 ), * ) ODAT(6)
 1379             READ(WORDS( 2 ), * ) ODAT(7)
 1380             READ(WORDS( 3 ), * ) ODAT(8)
 1381             READ(WORDS( 4 ), * ) ODAT(9)
 1382             READ(WORDS( 5 ), * ) ODAT(10)
 1383           END IF
 1384                 
 1385           IF (WORDS(6) .NE. '0' .AND. WORDS(6) .NE. '1') THEN
 1386             OFILES(J)=0
 1387           ELSE
 1388             READ(WORDS( 6 ), * ) OFILES(J)
 1389           END IF
 1390                 
 1391                 
 1392         ELSE
 1393           OFILES(J)=0
 1394           READ (NDSI,*,END=2001,ERR=2002)(ODAT(I),I=5*(J-1)+1,5*J)
 1395         END IF
 1396 !          WRITE(*,*) 'OFILES(J)= ', OFILES(J),J
 1397 !
 1398           ODAT(5*(J-1)+3) = MAX ( 0 , ODAT(5*(J-1)+3) )
 1399 !
 1400  
 1401 ! 2.5 Output types
 1402                   
 1403           IF ( ODAT(5*(J-1)+3) .NE. 0 ) THEN
 1404                                             
 1405 ! Type 1: fields of mean wave parameters
 1406             IF ( J .EQ. 1 ) THEN
 1407               CALL W3READFLGRD ( NDSI, NDSO, 9, NDSEN, COMSTR, FLGD,   &
 1408                                  FLGRD, IAPROC, NAPOUT, IERR )
 1409               IF ( IERR .NE. 0 ) GOTO 2222
 1410                                           
 1411                                           
 1412                                           
 1413 ! Type 2: point output
 1414             ELSE IF ( J .EQ. 2 ) THEN
 1415               DO ILOOP=1,2
 1416                 IF ( ILOOP .EQ. 1 ) THEN
 1417                   NDSI2  = NDSI
 1418                   IF ( IAPROC .EQ. 1 ) OPEN                       &
 1419                        (NDSS,FILE=TRIM(FNMPRE)//'ww3_shel.scratch')
 1420                 ELSE
 1421                   NDSI2  = NDSS
 1422                   CALL MPI_BARRIER (MPI_COMM,IERR_MPI)
 1423                   OPEN (NDSS,FILE=TRIM(FNMPRE)//'ww3_shel.scratch')
 1424                   REWIND (NDSS)
 1425 !
 1426                   IF ( .NOT.ALLOCATED(X) ) THEN
 1427                     IF ( NPTS.GT.0 ) THEN
 1428                       ALLOCATE ( X(NPTS), Y(NPTS), PNAMES(NPTS) )
 1429                     ELSE
 1430                       ALLOCATE ( X(1), Y(1), PNAMES(1) )
 
 
                                                         ww3_shel.F90  page  27
 
 
 1431                       GOTO 2054
 1432                     END IF
 1433                   END IF
 1434                 END IF
 1435 !
 1436                 NPTS   = 0
 1437                 DO
 1438                   CALL NEXTLN ( COMSTR , NDSI , NDSEN )
 1439                   READ (NDSI2,*) XX, YY, PN
 1440                   IF ( ILOOP.EQ.1 .AND. IAPROC.EQ.1 ) THEN
 1441                     BACKSPACE (NDSI)
 1442                     READ (NDSI,'(A)') LINE
 1443                     WRITE (NDSS,'(A)') LINE
 1444                   END IF
 1445                   IF ( INDEX(PN,"STOPSTRING").NE.0 ) EXIT
 1446                   NPTS   = NPTS + 1
 1447                   IF ( ILOOP .EQ. 1 ) CYCLE
 1448                   X(NPTS)      = XX
 1449                   Y(NPTS)      = YY
 1450                   PNAMES(NPTS) = PN
 1451                   IF ( IAPROC .EQ. NAPOUT ) THEN
 1452                     IF ( FLAGLL ) THEN
 1453                       IF ( NPTS .EQ. 1 ) THEN
 1454                         WRITE (NDSO,2945)                     &
 1455                                FACTOR*XX, FACTOR*YY, PN
 1456                       ELSE
 1457                         WRITE (NDSO,2946) NPTS,               &
 1458                                FACTOR*XX, FACTOR*YY, PN
 1459                       END IF
 1460                     ELSE
 1461                       IF ( NPTS .EQ. 1 ) THEN
 1462                         WRITE (NDSO,2955)                     &
 1463                                FACTOR*XX, FACTOR*YY, PN
 1464                       ELSE
 1465                         WRITE (NDSO,2956) NPTS,               &
 1466                                FACTOR*XX, FACTOR*YY, PN
 1467                       END IF
 1468                     END IF
 1469                   END IF
 1470                 END DO
 1471 !
 1472                 IF ( IAPROC.EQ.1 .AND. ILOOP.EQ.1 ) CLOSE (NDSS)
 1473               END DO
 1474 !
 1475               IF ( NPTS.EQ.0 .AND. IAPROC.EQ.NAPOUT )               &
 1476                    WRITE (NDSO,2947)
 1477               IF ( IAPROC .EQ. 1 ) THEN
 1478                 CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
 1479                 CLOSE (NDSS,STATUS='DELETE')
 1480               ELSE
 1481                 CLOSE (NDSS)
 1482                 CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
 1483               END IF
 1484 !
 1485  
 
 
                                                         ww3_shel.F90  page  28
 
 
 1486  
 1487 ! Type 3: track output
 1488             ELSE IF ( J .EQ. 3 ) THEN
 1489               CALL NEXTLN ( COMSTR , NDSI , NDSEN )
 1490               READ (NDSI,*) TFLAGI
 1491 !
 1492               IF ( .NOT. TFLAGI ) NDS(11) = -NDS(11)
 1493               IF ( IAPROC .EQ. NAPOUT ) THEN
 1494                 IF ( .NOT. TFLAGI ) THEN
 1495                   WRITE (NDSO,3945) 'input', 'UNFORMATTED'
 1496                 ELSE
 1497                   WRITE (NDSO,3945) 'input', 'FORMATTED'
 1498                 END IF
 1499               END IF
 1500                     
 1501                     
 1502 ! Type 6: partitioning
 1503             ELSE IF ( J .EQ. 6 ) THEN
 1504 !             IPRT: IX0, IXN, IXS, IY0, IYN, IYS
 1505               CALL NEXTLN ( COMSTR , NDSI , NDSEN )
 1506               READ (NDSI,*) IPRT, PRTFRM
 1507 !
 1508               IF ( IAPROC .EQ. NAPOUT ) THEN
 1509                 IF ( PRTFRM ) THEN
 1510                   YESXNO = 'YES/--'
 1511                 ELSE
 1512                   YESXNO = '---/NO'
 1513                 END IF
 1514                 WRITE (NDSO,6945) IPRT, YESXNO
 1515               END IF
 1516                     
 1517                     
 1518                     
 1519             END IF ! J
 1520           END IF ! ODAT
 1521         END IF ! IF J=4
 1522         END DO ! J
 1523                   
 1524         ! force minimal allocation to avoid memory seg fault
 1525         IF ( .NOT.ALLOCATED(X) .AND. NPTS.EQ.0 ) ALLOCATE ( X(1), Y(1), P>
 1526                                                                           
 1527 ! 2.6 Homogeneous field data
 1528                             
 1529         IF ( FLHOM ) THEN
 1530           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951)                  &
 1531                           'Homogeneous field data (and moving grid) ...'
 1532           NH     = 0
 1533 !
 1534           ! Start of loop
 1535           DO
 1536             CALL NEXTLN ( COMSTR , NDSI , NDSEN )
 1537             READ (NDSI,*) IDTST
 1538                                
 1539                                
 1540             ! Exit if illegal id
 
 
                                                         ww3_shel.F90  page  29
 
 
 1541             IF ( IDTST.NE.IDSTR(-7) .AND. IDTST.NE.IDSTR(-6) .AND.   &
 1542                  IDTST.NE.IDSTR(-5) .AND. IDTST.NE.IDSTR(-4) .AND.   &
 1543                  IDTST.NE.IDSTR(-3) .AND. IDTST.NE.IDSTR(-2) .AND.   &
 1544                  IDTST.NE.IDSTR(-1) .AND. IDTST.NE.IDSTR(0)  .AND.   &
 1545                  IDTST.NE.IDSTR(1)  .AND. IDTST.NE.IDSTR(2)  .AND.   &
 1546                  IDTST.NE.IDSTR(3)  .AND. IDTST.NE.IDSTR(4)  .AND.   &
 1547                  IDTST.NE.IDSTR(8)  .AND. IDTST.NE.'STP' ) GOTO 2005
 1548                                                                     
 1549             ! Stop conditions
 1550             IF ( IDTST .EQ. 'STP' ) THEN
 1551               EXIT
 1552             ELSE
 1553               BACKSPACE ( NDSI )
 1554             END IF
 1555                   
 1556             ! Store data
 1557             DO J=LBOUND(IDSTR,1), 8
 1558               IF ( IDTST .EQ. IDSTR(J) ) THEN
 1559                 NH(J)    = NH(J) + 1
 1560                 IF ( NH(J) .GT. NHMAX ) GOTO 2006
 1561                 IF ( J .LE. 1  ) THEN ! water levels, etc. : get HA
 1562                   READ (NDSI,*) IDTST,           &
 1563                         THO(1,J,NH(J)), THO(2,J,NH(J)),            &
 1564                         HA(NH(J),J)
 1565                 ELSE IF ( J .EQ. 2 ) THEN ! currents: get HA and HD
 1566                   READ (NDSI,*) IDTST,           &
 1567                         THO(1,J,NH(J)), THO(2,J,NH(J)),            &
 1568                         HA(NH(J),J), HD(NH(J),J)
 1569                 ELSE IF ( J .EQ. 3 ) THEN ! wind: get HA HD and HS
 1570                   READ (NDSI,*) IDTST,           &
 1571                         THO(1,J,NH(J)), THO(2,J,NH(J)),            &
 1572                         HA(NH(J),J), HD(NH(J),J), HS(NH(J),J)
 1573                 ELSE IF ( J .EQ. 4 ) THEN ! ice
 1574                   READ (NDSI,*) IDTST,           &
 1575                         THO(1,J,NH(J)), THO(2,J,NH(J)),            &
 1576                         HA(NH(J),J)
 1577                 ELSE IF ( J .EQ. 8 ) THEN ! mov: HA and HD
 1578                   READ (NDSI,*) IDTST,           &
 1579                         THO(1,J,NH(J)), THO(2,J,NH(J)),            &
 1580                         HA(NH(J),J), HD(NH(J),J)
 1581                 END IF
 1582               END IF
 1583             END DO
 1584           END DO
 1585                 
 1586 !
 1587  
 1588           DO J=JFIRST, 8
 1589             IF ( FLH(J) .AND. IAPROC.EQ.NAPOUT ) THEN
 1590               WRITE (NDSO,952) NH(J), IDFLDS(J)
 1591               DO I=1, NH(J)
 1592                 IF ( ( J .LE. 1 ) .OR. ( J .EQ. 4 ) ) THEN
 1593                   WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
 1594                                    HA(I,J)
 1595                 ELSE IF ( ( J .EQ. 2 ) .OR. (J .EQ. 8) ) THEN
 
 
                                                         ww3_shel.F90  page  30
 
 
 1596                   WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
 1597                                    HA(I,J), HD(I,J)
 1598                 ELSE IF ( J .EQ. 3 ) THEN
 1599                   WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
 1600                                    HA(I,J), HD(I,J), HS(I,J)
 1601                 END IF
 1602               END DO
 1603             END IF
 1604           END DO
 1605 !
 1606           IF ( ( FLH(-7) .AND. (NH(-7).EQ.0) ) .OR.                     &
 1607                ( FLH(-6) .AND. (NH(-6).EQ.0) ) .OR.                     &
 1608                ( FLH(-5) .AND. (NH(-5).EQ.0) ) .OR.                     &
 1609                ( FLH(-4) .AND. (NH(-4).EQ.0) ) .OR.                     &
 1610                ( FLH(-3) .AND. (NH(-3).EQ.0) ) .OR.                     &
 1611                ( FLH(-2) .AND. (NH(-2).EQ.0) ) .OR.                     &
 1612                ( FLH(-1) .AND. (NH(-1).EQ.0) ) .OR.                     &
 1613                ( FLH(0)  .AND. (NH(0).EQ.0)  ) .OR.                     &
 1614                ( FLH(1)  .AND. (NH(1).EQ.0)  ) .OR.                     &
 1615                ( FLH(2)  .AND. (NH(2).EQ.0)  ) .OR.                     &
 1616                ( FLH(3)  .AND. (NH(3).EQ.0)  ) .OR.                     &
 1617                ( FLH(4)  .AND. (NH(4).EQ.0)  ) .OR.                     &
 1618                ( FLH(8) .AND. (NH(8).EQ.0) ) ) GOTO 2007
 1619 !
 1620         END IF ! FLHOM
 1621                       
 1622       END IF
 1623             
 1624             
 1625             
 1626             
 1627             
 1628 !
 1629 ! ----------------
 1630 !
 1631  
 1632 ! 2.1 input fields
 1633                   
 1634 ! 2.1.a Opening field and data files
 1635                                     
 1636       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,950)
 1637       IF ( FLFLG ) THEN
 1638         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951)                  &
 1639                                          'Preparing input files ...'
 1640 !
 1641  
 1642         DO J=JFIRST, 4
 1643           IF ( INFLAGS1(J) .AND. .NOT. FLAGSC(J)) THEN
 1644             IF ( FLH(J) ) THEN
 1645               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,954) IDFLDS(J)
 1646             ELSE
 1647               FLAGTIDE = 0
 1648               CALL W3FLDO ('READ', IDSTR(J), NDSF(J), NDST,     &
 1649                             NDSEN, NX, NY, GTYPE,               &
 1650                             IERR, FPRE=TRIM(FNMPRE), TIDEFLAGIN=FLAGTIDE )
 
 
                                                         ww3_shel.F90  page  31
 
 
 1651               IF ( IERR .NE. 0 ) GOTO 2222
 1652               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,955) IDFLDS(J)
 1653             END IF
 1654           ELSE
 1655             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,954) IDFLDS(J)
 1656           END IF
 1657         END DO
 1658 !
 1659         DO J=5, 7
 1660           IF ( INFLAGS1(J) .AND. .NOT. FLAGSC(J)) THEN
 1661             CALL W3FLDO ('READ', IDSTR(J), NDSF(J), NDST, NDSEN, &
 1662                          RCLD(J), NY, NODATA(J),                 &
 1663                          IERR, FPRE=TRIM(FNMPRE) )
 1664             IF ( IERR .NE. 0 ) GOTO 2222
 1665             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,956) IDFLDS(J),&
 1666                                              RCLD(J), NODATA(J)
 1667           ELSE
 1668             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,954) IDFLDS(J)
 1669           END IF
 1670         END DO
 1671 !
 1672       END IF ! FLFLG
 1673                     
 1674                     
 1675                     
 1676                     
 1677 ! 2.2 Time setup
 1678                 
 1679       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,930)
 1680       CALL STME21 ( TIME0 , DTME21 )
 1681       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,931) DTME21
 1682       TIME = TIME0
 1683       CALL STME21 ( TIMEN , DTME21 )
 1684       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,932) DTME21
 1685       TIMEEND = TIMEN
 1686 !
 1687       DTTST  = DSEC21 ( TIME0 , TIMEN )
 1688       IF ( DTTST .LE. 0. ) GOTO 2003
 1689                                     
 1690                                     
 1691 ! 2.3 Domain setup
 1692                   
 1693       IOSTYP = MAX ( 0 , MIN ( 3 , IOSTYP ) )
 1694       IF ( IAPROC .EQ. NAPOUT ) THEN
 1695         IF ( IOSTYP .EQ. 0 ) THEN
 1696           WRITE (NDSO,940) 'No dedicated output process, ' //   &
 1697                            'parallel file system required.'
 1698         ELSE IF ( IOSTYP .EQ. 1 ) THEN
 1699           WRITE (NDSO,940) 'No dedicated output process, ' //   &
 1700                            'any file system.'
 1701         ELSE IF ( IOSTYP .EQ. 2 ) THEN
 1702           WRITE (NDSO,940) 'Single dedicated output process.'
 1703         ELSE IF ( IOSTYP .EQ. 3 ) THEN
 1704           WRITE (NDSO,940) 'Multiple dedicated output processes.'
 1705         ELSE
 
 
                                                         ww3_shel.F90  page  32
 
 
 1706           WRITE (NDSO,940) 'IOSTYP NOT RECOGNIZED'
 1707         END IF
 1708       END IF
 1709             
 1710             
 1711 ! 2.4 Output dates
 1712                   
 1713       DO J = 1, NOTYPE
 1714 !
 1715         IF ( ODAT(5*(J-1)+3) .NE. 0 ) THEN
 1716           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,941) J, IDOTYP(J)
 1717           TTIME(1) = ODAT(5*(J-1)+1)
 1718           TTIME(2) = ODAT(5*(J-1)+2)
 1719           CALL STME21 ( TTIME , DTME21 )
 1720           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,942) DTME21
 1721           TTIME(1) = ODAT(5*(J-1)+4)
 1722           TTIME(2) = ODAT(5*(J-1)+5)
 1723           CALL STME21 ( TTIME , DTME21 )
 1724           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) DTME21
 1725           TTIME(1) = 0
 1726           TTIME(2) = 0
 1727           DTTST    = REAL ( ODAT(5*(J-1)+3) )
 1728 !jcw if J=1, then DTTST above is the output time step for mean wave field>
 1729              IF (J.EQ.1) THEN
 1730                DTTST=MIN(DTTST,REAL(TI_OCN2WAV))
 1731              END IF
 1732              IF (J.EQ.1) THEN
 1733                DTTST=MIN(DTTST,REAL(TI_ATM2WAV))
 1734              END IF
 1735 !
 1736           CALL TICK21 ( TTIME , DTTST  )
 1737           CALL STME21 ( TTIME , DTME21 )
 1738           IF ( ( ODAT(5*(J-1)+1) .NE. ODAT(5*(J-1)+4) .OR.          &
 1739                  ODAT(5*(J-1)+2) .NE. ODAT(5*(J-1)+5) ) .AND.       &
 1740                  IAPROC .EQ. NAPOUT ) THEN
 1741             IF ( DTME21(9:9) .NE. '0' ) THEN
 1742               WRITE (NDSO,1944) DTME21( 9:19)
 1743             ELSE IF ( DTME21(10:10) .NE. '0' ) THEN
 1744               WRITE (NDSO,2944) DTME21(10:19)
 1745             ELSE
 1746               WRITE (NDSO,3944) DTME21(12:19)
 1747             END IF
 1748           END IF
 1749         END IF
 1750       END DO
 1751 !
 1752 ! CHECKPOINT
 1753       J=8
 1754       IF (ODAT(38) .NE. 0) THEN
 1755         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,941) J, IDOTYP(J)
 1756         TTIME(1) = ODAT(5*(J-1)+1)
 1757         TTIME(2) = ODAT(5*(J-1)+2)
 1758         CALL STME21 ( TTIME , DTME21 )
 1759         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,942) DTME21
 1760         TTIME(1) = ODAT(5*(J-1)+4)
 
 
                                                         ww3_shel.F90  page  33
 
 
 1761         TTIME(2) = ODAT(5*(J-1)+5)
 1762         CALL STME21 ( TTIME , DTME21 )
 1763         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) DTME21
 1764         TTIME(1) = 0
 1765         TTIME(2) = 0
 1766         DTTST    = REAL ( ODAT(5*(J-1)+3) )
 1767         CALL TICK21 ( TTIME , DTTST  )
 1768         CALL STME21 ( TTIME , DTME21 )
 1769         IF ( ( ODAT(5*(J-1)+1) .NE. ODAT(5*(J-1)+4) .OR.          &
 1770                ODAT(5*(J-1)+2) .NE. ODAT(5*(J-1)+5) ) .AND.       &
 1771                IAPROC .EQ. NAPOUT ) THEN
 1772           IF ( DTME21(9:9) .NE. '0' ) THEN
 1773             WRITE (NDSO,1944) DTME21( 9:19)
 1774           ELSE IF ( DTME21(10:10) .NE. '0' ) THEN
 1775             WRITE (NDSO,2944) DTME21(10:19)
 1776           ELSE
 1777             WRITE (NDSO,3944) DTME21(12:19)
 1778           END IF
 1779         END IF
 1780       END IF
 1781 !
 1782 ! 2.5 Output types
 1783                   
 1784                   
 1785 !
 1786 ! For outputs with non-zero time step, check dates :
 1787 ! If output ends before run start OR output starts after run end,
 1788 ! deactivate output cleanly with output time step = 0
 1789 ! This is usefull for IOSTYP=3 (Multiple dedicated output processes)
 1790 ! to avoid the definition of dedicated proc. for unused output.
 1791 !
 1792       DO J = 1, NOTYPE
 1793         DTTST  = DSEC21 ( TIME0 , ODAT(5*(J-1)+4:5*(J-1)+5) )
 1794         IF ( DTTST .LT. 0 ) THEN
 1795           ODAT(5*(J-1)+3) = 0
 1796           IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,8945) TRIM(IDOTYP(J))
 1797           CONTINUE
 1798         END IF
 1799         DTTST  = DSEC21 ( ODAT(5*(J-1)+1:5*(J-1)+2), TIMEN )
 1800         IF ( DTTST .LT. 0 ) THEN
 1801           ODAT(5*(J-1)+3) = 0
 1802           IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,8945) TRIM(IDOTYP(J))
 1803           CONTINUE
 1804         END IF
 1805       END DO
 1806 !
 1807 ! CHECKPOINT
 1808       J = 8
 1809       DTTST  = DSEC21 ( TIME0 , ODAT(5*(J-1)+4:5*(J-1)+5) )
 1810       IF ( DTTST .LT. 0 ) THEN
 1811         ODAT(5*(J-1)+3) = 0
 1812         IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,8945) TRIM(IDOTYP(J))
 1813         CONTINUE
 1814       END IF
 1815       DTTST  = DSEC21 ( ODAT(5*(J-1)+1:5*(J-1)+2), TIMEN )
 
 
                                                         ww3_shel.F90  page  34
 
 
 1816       IF ( DTTST .LT. 0 ) THEN
 1817         ODAT(5*(J-1)+3) = 0
 1818         IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,8945) TRIM(IDOTYP(J))
 1819         CONTINUE
 1820       END IF
 1821 !
 1822 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 1823 ! 5.  Initializations
 1824 !
 1825  
 1826       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951) 'Wave model ...'
 1827 !
 1828       OARST = ANY(FLOGR)
 1829 !
 1830      CALL W3INIT ( 1, .FALSE., 'ww3', NDS, NTRACE, ODAT, FLGRD, FLGR2, FL>
 1831                    FLG2, NPTS, X, Y, PNAMES, IPRT, PRTFRM, MPI_COMM,   &
 1832                    FLAGSTIDEIN=FLAGSTIDE )
 1833 !
 1834 !      IF (MINVAL(VA) .LT. 0.) THEN
 1835 !        WRITE(740+IAPROC,*) 'NEGATIVE ACTION SHELL 5', MINVAL(VA)
 1836 !        CALL FLUSH(740+IAPROC)
 1837 !        CALL EXTCDE(665)
 1838 !      ENDIF
 1839 !      IF (SUM(VA) .NE. SUM(VA)) THEN
 1840 !        WRITE(740+IAPROC,*) 'NAN in ACTION SHEL1', SUM(VA)
 1841 !        CALL FLUSH(740+IAPROC)
 1842 !        CALL EXTCDE(666)
 1843 !      ENDIF
 1844             
 1845             
 1846 !
 1847       ALLOCATE ( XXX(NX,NY) )
 1848 !
 1849  
 1850 !
 1851       CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
 1852 !
 1853       IF ( IAPROC .EQ. NAPOUT ) THEN
 1854         CALL DATE_AND_TIME ( VALUES=CLKDT2 )
 1855       END IF
 1856 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 1857 !
 1858  
 1859 !jcw
 1860       CALL INIT_WVCP (1)
 1861       CALL INITIALIZE_WAV_ROUTERS
 1862       ccount=0
 1863       CALL COAWST_CPL (ccount)
 1864                               
 1865 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 1866 ! 6.  Model without input
 1867 !
 1868 !      IF (MINVAL(VA) .LT. 0.) THEN
 1869 !        WRITE(740+IAPROC,*) 'NEGATIVE ACTION SHELL 6', MINVAL(VA)
 1870 !        CALL FLUSH(740+IAPROC)
 
 
                                                         ww3_shel.F90  page  35
 
 
 1871 !        CALL EXTCDE(665)
 1872 !      ENDIF
 1873 !      IF (SUM(VA) .NE. SUM(VA)) THEN
 1874 !        WRITE(740+IAPROC,*) 'NAN in ACTION SHEL2', SUM(VA)
 1875 !        CALL FLUSH(740+IAPROC)
 1876 !        CALL EXTCDE(666)
 1877 !      ENDIF
 1878             
 1879             
 1880       IF ( .NOT. FLFLG ) THEN
 1881 !
 1882         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,960)
 1883         CALL W3WAVE ( 1, ODAT, TIMEN                      &
 1884                     )
 1885 !
 1886         GOTO 2222
 1887 !
 1888       END IF
 1889 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 1890 ! 7.  Model with input
 1891 !
 1892       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,970)
 1893 !
 1894  
 1895  
 1896   700 CONTINUE
 1897 !
 1898 ! 7.a Determine next time interval and input fields
 1899 ! 7.a.1 Preparation
 1900 !
 1901       TTIME  = TIMEN
 1902 !
 1903       CALL STME21 ( TIME0 , DTME21 )
 1904       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,971) DTME21
 1905 !
 1906  
 1907  
 1908       DO J=JFIRST,8
 1909 !
 1910  
 1911         IF ( INFLAGS1(J) ) THEN
 1912 !
 1913 ! 7.a.2 Check if update is needed
 1914 !
 1915           IF (.NOT.FLAGSC(J)) THEN
 1916             TTT(1) = TFN(1,J)
 1917             TTT(2) = TFN(2,J)
 1918             IF ( TTT(1) .EQ. -1 ) THEN
 1919               DTTST  = 0.
 1920             ELSE
 1921               DTTST  = DSEC21 ( TIME0 , TTT )
 1922             END IF
 1923          ELSE
 1924          IF (DSEC21 (TIME, TIMEEND) .GT. 0.0) DTTST=0.
 1925           END IF
 
 
                                                         ww3_shel.F90  page  36
 
 
 1926 !
 1927 ! 7.a.3 Update time and fields / data
 1928 !
 1929           IF ( DTTST .LE. 0. ) THEN
 1930                                    
 1931             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,972) IDFLDS(J)
 1932 !
 1933 ! IC1 : (in context of IC3 & IC2, this is ice thickness)
 1934             IF ( J .EQ. -7 ) THEN
 1935               IF ( FLH(J) ) THEN
 1936                 CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
 1937                              TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
 1938                              TTT, XXX, XXX, XXX, TI1, XXX, XXX, ICEP1, IE>
 1939               ELSE
 1940                 CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
 1941                              NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
 1942                              TTT, XXX, XXX, XXX, TI1, XXX, XXX, ICEP1,  &
 1943                              IERR, FLAGSC(J)                            &
 1944                              )
 1945               END IF
 1946               IF ( IERR .LT. 0 ) FLLST_ALL(J) = .TRUE.
 1947                                                       
 1948 ! IC2 : (in context of IC3, this is ice viscosity)
 1949             ELSE IF ( J .EQ. -6 ) THEN
 1950               IF ( FLH(J) ) THEN
 1951                 CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
 1952                              TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
 1953                              TTT, XXX, XXX, XXX, TI2, XXX, XXX, ICEP2, IE>
 1954               ELSE
 1955                 CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
 1956                              NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
 1957                              TTT, XXX, XXX, XXX, TI2, XXX, XXX, ICEP2,  &
 1958                              IERR, FLAGSC(J))
 1959               END IF
 1960               IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.
 1961                                                      
 1962 ! IC3 : (in context of IC3, this is ice density)
 1963             ELSE IF ( J .EQ. -5 ) THEN
 1964               IF ( FLH(J) ) THEN
 1965                 CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
 1966                              TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
 1967                              TTT, XXX, XXX, XXX, TI3, XXX, XXX, ICEP3, IE>
 1968               ELSE
 1969                 CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
 1970                              NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
 1971                              TTT, XXX, XXX, XXX, TI3, XXX, XXX, ICEP3,  &
 1972                              IERR, FLAGSC(J))
 1973               END IF
 1974               IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.
 1975                                                      
 1976 ! IC4 : (in context of IC3, this is ice modulus)
 1977             ELSE IF ( J .EQ. -4 ) THEN
 1978               IF ( FLH(J) ) THEN
 1979                 CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
 1980                              TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
 
 
                                                         ww3_shel.F90  page  37
 
 
 1981                              TTT, XXX, XXX, XXX, TI4, XXX, XXX, ICEP4, IE>
 1982               ELSE
 1983                 CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
 1984                              NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
 1985                              TTT, XXX, XXX, XXX, TI4, XXX, XXX, ICEP4,  &
 1986                              IERR, FLAGSC(J))
 1987               END IF
 1988               IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.
 1989                                                      
 1990 ! IC5 : ice flow diam.
 1991             ELSE IF ( J .EQ. -3 ) THEN
 1992               IF ( FLH(J) ) THEN
 1993                 CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
 1994                              TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
 1995                              TTT, XXX, XXX, XXX, TI5, XXX, XXX, ICEP5, IE>
 1996               ELSE
 1997                 CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
 1998                              NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
 1999                              TTT, XXX, XXX, XXX, TI5, XXX, XXX, ICEP5,  &
 2000                              IERR, FLAGSC(J)                            &
 2001                              )
 2002               END IF
 2003               IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.
 2004                                                      
 2005 ! MUD1 : mud density
 2006             ELSE IF ( J .EQ. -2 ) THEN
 2007               IF ( FLH(J) ) THEN
 2008                 CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
 2009                              TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
 2010                              TTT, XXX, XXX, XXX, TZN, XXX, XXX, MUDD, IER>
 2011               ELSE
 2012                 CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
 2013                              NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
 2014                              TTT, XXX, XXX, XXX, TZN, XXX, XXX, MUDD,   &
 2015                              IERR, FLAGSC(J))
 2016               END IF
 2017               IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.
 2018                                                      
 2019 ! MUD2 : mud thickness
 2020             ELSE IF ( J .EQ. -1 ) THEN
 2021               IF ( FLH(J) ) THEN
 2022                 CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
 2023                              TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
 2024                              TTT, XXX, XXX, XXX, TTN, XXX, XXX, MUDT, IER>
 2025               ELSE
 2026                 CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
 2027                              NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
 2028                              TTT, XXX, XXX, XXX, TTN, XXX, XXX, MUDT,   &
 2029                              IERR, FLAGSC(J))
 2030               END IF
 2031               IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.
 2032                                                      
 2033 ! MUD3 : mud viscosity
 2034             ELSE IF ( J .EQ. 0 ) THEN
 2035               IF ( FLH(J) ) THEN
 
 
                                                         ww3_shel.F90  page  38
 
 
 2036                 CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
 2037                              TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
 2038                              TTT, XXX, XXX, XXX, TVN, XXX, XXX, MUDV, IER>
 2039               ELSE
 2040                 CALL W3FLDG ('READ', IDSTR(J), NDSF(J),         &
 2041                              NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN, &
 2042                              TTT, XXX, XXX, XXX, TVN, XXX, XXX, MUDV,   &
 2043                              IERR, FLAGSC(J))
 2044               END IF
 2045               IF ( IERR .LT. 0 )FLLST_ALL(J) = .TRUE.
 2046                                                      
 2047 ! LEV : water levels
 2048             ELSE IF ( J .EQ. 1 ) THEN
 2049               IF ( FLH(J) ) THEN
 2050                 CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
 2051                              TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
 2052                              TTT, XXX, XXX, XXX, TLN, XXX, XXX, WLEV, IER>
 2053               ELSE
 2054               TFN(1,J)=TIME0(1)
 2055               TFN(2,J)=TIME0(2)
 2056               CALL TICK21(TFN(:,J),REAL(TI_OCN2WAV))
 2057               END IF
 2058               IF ( IERR .LT. 0 ) FLLSTL = .TRUE.
 2059 !could be:    IF ( IERR .LT. 0 ) FLLST_ALL(J) = .TRUE.
 2060                                                       
 2061 ! CUR : currents
 2062             ELSE IF ( J .EQ. 2 ) THEN
 2063               IF ( FLH(J) ) THEN
 2064                 CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
 2065                              TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
 2066                              TC0, CX0, CY0, XXX, TCN, CXN, CYN, XXX, IERR)
 2067 !
 2068               ELSE
 2069               TFN(1,J)=TIME0(1)
 2070               TFN(2,J)=TIME0(2)
 2071               CALL TICK21(TFN(:,J),REAL(TI_OCN2WAV))
 2072               END IF
 2073                     
 2074 ! WND : winds
 2075             ELSE IF ( J .EQ. 3 ) THEN
 2076               IF ( FLH(J) ) THEN
 2077                 CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
 2078                              TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
 2079                              TW0, WX0, WY0, DT0, TWN, WXN, WYN, DTN, IERR)
 2080 !
 2081               ELSE
 2082               TFN(1,J)=TIME0(1)
 2083               TFN(2,J)=TIME0(2)
 2084               CALL TICK21(TFN(:,J),REAL(TI_ATM2WAV))
 2085               END IF
 2086                     
 2087 ! ICE : ice conc.
 2088             ELSE IF ( J .EQ. 4 ) THEN
 2089               IF ( FLH(J) ) THEN
 2090                 CALL W3FLDH (J, NDST, NDSEN, NX, NY, NX, NY,    &
 
 
                                                         ww3_shel.F90  page  39
 
 
 2091                              TIME0, TIMEN, NH(J), NHMAX, THO, HA, HD, HS,&
 2092                              TW0, WX0, WY0, DT0, TWN, WXN, WYN, DTN, IERR)
 2093               ELSE
 2094                 CALL W3FLDG ('READ', IDSTR(J), NDSF(J),            &
 2095                              NDST, NDSEN, NX, NY, NX, NY, TIME0, TIMEN,  >
 2096                              TTT, XXX, XXX, XXX, TIN, XXX, BERGI, ICEI,  >
 2097                              IERR, FLAGSC(J)                             >
 2098                             )
 2099                 IF ( IERR .LT. 0 ) FLLSTI = .TRUE.
 2100 !could be:      IF ( IERR .LT. 0 ) FLLST_ALL(J) = .TRUE.
 2101               END IF
 2102                     
 2103 ! Assim data
 2104             ELSE IF ( J .EQ. 5 ) THEN
 2105               CALL W3FLDD ('SIZE', IDSTR(J), NDSF(J), NDST,      &
 2106                            NDSEN, TIME0, T0N, RCLD(J), NDT(J),           &
 2107                            NDTNEW, DATA0, IERR )
 2108               IF ( IERR .LT. 0 ) THEN
 2109                 INFLAGS1(J) = .FALSE.
 2110                 IF ( ALLOCATED(DATA0) ) DEALLOCATE(DATA0)
 2111               ELSE
 2112                 NDT(J) = NDTNEW
 2113                 IF ( ALLOCATED(DATA0) ) DEALLOCATE(DATA0)
 2114                 ALLOCATE ( DATA0(RCLD(J),NDT(J)) )
 2115                 CALL W3FLDD ('READ', IDSTR(J), NDSF(J), NDST, &
 2116                              NDSEN, TIME0, T0N, RCLD(J), NDT(J),      &
 2117                              NDTNEW, DATA0, IERR )
 2118               END IF
 2119                     
 2120 ! Assim data
 2121             ELSE IF ( J .EQ. 6 ) THEN
 2122               CALL W3FLDD ('SIZE', IDSTR(J), NDSF(J), NDST,      &
 2123                            NDSEN, TIME0, T1N, RCLD(J), NDT(J),           &
 2124                            NDTNEW, DATA1, IERR )
 2125               IF ( IERR .LT. 0 ) THEN
 2126                 INFLAGS1(J) = .FALSE.
 2127                 IF ( ALLOCATED(DATA1) ) DEALLOCATE(DATA1)
 2128               ELSE
 2129                 NDT(J) = NDTNEW
 2130                 IF ( ALLOCATED(DATA1) ) DEALLOCATE(DATA1)
 2131                 ALLOCATE ( DATA1(RCLD(J),NDT(J)) )
 2132                 CALL W3FLDD ('READ', IDSTR(J), NDSF(J), NDST, &
 2133                              NDSEN, TIME0, T1N, RCLD(J), NDT(J),      &
 2134                              NDTNEW, DATA1, IERR )
 2135               END IF
 2136                     
 2137 ! Assim data
 2138             ELSE IF ( J .EQ. 7 ) THEN
 2139               CALL W3FLDD ('SIZE', IDSTR(J), NDSF(J), NDST,      &
 2140                            NDSEN, TIME0, T2N, RCLD(J), NDT(J),           &
 2141                            NDTNEW, DATA2, IERR )
 2142               IF ( IERR .LT. 0 ) THEN
 2143                 INFLAGS1(J) = .FALSE.
 2144                 IF ( ALLOCATED(DATA2) ) DEALLOCATE(DATA2)
 2145               ELSE
 
 
                                                         ww3_shel.F90  page  40
 
 
 2146                 NDT(J) = NDTNEW
 2147                 IF ( ALLOCATED(DATA2) ) DEALLOCATE(DATA2)
 2148                 ALLOCATE ( DATA2(RCLD(J),NDT(J)) )
 2149                 CALL W3FLDD ('READ', IDSTR(J), NDSF(J), NDST, &
 2150                              NDSEN, TIME0, T2N, RCLD(J), NDT(J),      &
 2151                              NDTNEW, DATA2, IERR )
 2152               END IF
 2153                     
 2154 ! Track
 2155             ELSE IF ( J .EQ. 8 ) THEN
 2156               CALL W3FLDM (4, NDST, NDSEN, TIME0, TIMEN, NH(4),  &
 2157                            NHMAX, THO, HA, HD, TG0, GA0, GD0,         &
 2158                            TGN, GAN, GDN, IERR)
 2159             END IF
 2160 !
 2161             IF ( IERR.GT.0 ) GOTO 2222
 2162             IF ( IERR.LT.0 .AND. IAPROC.EQ.NAPOUT ) WRITE (NDSO,973) IDFL>
 2163                                                                           
 2164                                                                           
 2165           END IF ! DTTST .LE. 0.
 2166 !
 2167 ! 7.a.4 Update next ending time
 2168 !
 2169           IF ( INFLAGS1(J) ) THEN
 2170             TTT    = TFN(:,J)
 2171             DTTST  = DSEC21 ( TTT , TTIME )
 2172             IF ( DTTST.GT.0. .AND. .NOT.                          &
 2173                    ( (FLLSTL .AND. J.EQ.1) .OR.                   &
 2174                      (FLLST_ALL(J) .AND. J.EQ.-7) .OR.            &
 2175                      (FLLST_ALL(J) .AND. J.EQ.-6) .OR.            &
 2176                      (FLLST_ALL(J) .AND. J.EQ.-5) .OR.            &
 2177                      (FLLST_ALL(J) .AND. J.EQ.-4) .OR.            &
 2178                      (FLLST_ALL(J) .AND. J.EQ.-3) .OR.            &
 2179                      (FLLST_ALL(J) .AND. J.EQ.-2) .OR.            &
 2180                      (FLLST_ALL(J) .AND. J.EQ.-1) .OR.            &
 2181                      (FLLST_ALL(J) .AND. J.EQ.0 ) .OR.            &
 2182                      (FLLSTI .AND. J.EQ.4) ) ) THEN
 2183               TTIME  = TTT
 2184 ! notes: if model has run out beyond field input, then this line should n>
 2185 !    be reached.
 2186             END IF
 2187           END IF
 2188 !
 2189         END IF ! INFLAGSC1(J)
 2190 !
 2191       END DO ! J=JFIRST,8
 2192 !
 2193 ! update the next assimilation data time
 2194 !
 2195  
 2196  
 2197       TDN = TTIME
 2198       CALL TICK21 ( TDN, 1. )
 2199       DO J=5, 7
 2200         IF ( INFLAGS1(J) ) THEN
 
 
                                                         ww3_shel.F90  page  41
 
 
 2201           TTT    = TFN(:,J)
 2202           DTTST  = DSEC21 ( TTT , TDN )
 2203           IF ( DTTST.GT.0. ) TDN = TTT
 2204         END IF
 2205       END DO
 2206 !
 2207       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,*) ' '
 2208 !
 2209 ! 7.b Run the wave model for the given interval
 2210 !
 2211       TIME0  = TTIME
 2212 !
 2213       CALL W3WAVE ( 1, ODAT, TIME0                                    &
 2214                   )
 2215                    
 2216 !
 2217       ! The following lines prevents us from trying to read past the end
 2218       ! of the files. This feature existed in v3.14.
 2219       ! "1" is for water levels
 2220       ! "4" is for ice concentration:
 2221       IF ( FLLSTL ) INFLAGS1(1) = .FALSE.
 2222       IF ( FLLSTI ) INFLAGS1(4) = .FALSE.
 2223                                          
 2224       ! We include something like this for mud and ice parameters also:
 2225       DO J=-7,0
 2226         IF (FLLST_ALL(J))THEN
 2227           INFLAGS1(J)=.FALSE.
 2228         END IF
 2229       END DO
 2230             
 2231 !
 2232 ! 7.c Run data assimilation at ending time
 2233 !
 2234       DTTST  = DSEC21 ( TIME , TDN )
 2235       IF ( DTTST .EQ. 0 ) THEN
 2236         CALL STME21 ( TIME0 , DTME21 )
 2237         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,975) DTME21
 2238 !
 2239         FLGDAS(1) = DSEC21(TIME,T0N) .EQ. 0.
 2240         FLGDAS(2) = DSEC21(TIME,T1N) .EQ. 0.
 2241         FLGDAS(3) = DSEC21(TIME,T2N) .EQ. 0.
 2242 !
 2243         CALL W3WDAS ( FLGDAS, RCLD, NDT, DATA0, DATA1, DATA2 )
 2244 !
 2245 ! 7.d Call wave model again after data assimilation for output only
 2246 !
 2247         DTTST  = DSEC21 ( TIME , TIMEN )
 2248                                         
 2249         IF ( DTTST .EQ. 0. ) THEN
 2250           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,*) ' '
 2251           CALL W3WAVE ( 1, ODAT, TIME0                                 &
 2252                       )
 2253         END IF
 2254       END IF
 2255 !
 
 
                                                         ww3_shel.F90  page  42
 
 
 2256 ! 7.e Check times
 2257 !
 2258  
 2259  
 2260       DTTST  = DSEC21 ( TIME0 , TIMEN )
 2261       IF ( DTTST .GT. 0. ) GOTO 700
 2262 !
 2263 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 2264 !     End of shel
 2265 !
 2266       GOTO 2222
 2267 !
 2268 ! Error escape locations
 2269 !
 2270  2000 CONTINUE
 2271       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) IERR
 2272       CALL EXTCDE ( 1000 )
 2273 !
 2274  2001 CONTINUE
 2275       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001)
 2276       CALL EXTCDE ( 1001 )
 2277 !
 2278  2002 CONTINUE
 2279       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002) IERR
 2280       CALL EXTCDE ( 1002 )
 2281 !
 2282  2102 CONTINUE
 2283       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1102)
 2284       CALL EXTCDE ( 1102 )
 2285 !
 2286  2003 CONTINUE
 2287       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1003)
 2288       CALL EXTCDE ( 1003 )
 2289 !
 2290  2104 CONTINUE
 2291       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1104) IERR
 2292       CALL EXTCDE ( 1104 )
 2293 !
 2294  2004 CONTINUE
 2295       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1004) IERR
 2296       CALL EXTCDE ( 1004 )
 2297 !
 2298  2005 CONTINUE
 2299       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1005) IDTST
 2300       CALL EXTCDE ( 1005 )
 2301 !
 2302  2006 CONTINUE
 2303       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1006) IDTST, NH(J)
 2304       CALL EXTCDE ( 1006 )
 2305 !
 2306  2062 CONTINUE
 2307       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1062) IDTST
 2308       CALL EXTCDE ( 1062 )
 2309 !
 2310  2007 CONTINUE
 
 
                                                         ww3_shel.F90  page  43
 
 
 2311       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1007)
 2312       CALL EXTCDE ( 1007 )
 2313 !
 2314  2008 CONTINUE
 2315       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1008) IERR
 2316       CALL EXTCDE ( 1008 )
 2317 !
 2318  2054 CONTINUE
 2319       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1054)
 2320       CALL EXTCDE ( 1054 )
 2321  2222 CONTINUE
 2322 !
 2323       CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
 2324 !
 2325       IF ( IAPROC .EQ. NAPOUT ) THEN
 2326           CALL DATE_AND_TIME ( VALUES=CLKDT3 )
 2327           CLKFIN = MAX(TDIFF ( CLKDT1,CLKDT2 ), 0.)
 2328           CLKFEL = MAX(TDIFF ( CLKDT1,CLKDT3 ), 0.)
 2329           WRITE (NDSO,997) CLKFIN
 2330           WRITE (NDSO,998) CLKFEL
 2331           IF ( NDSO .NE. NDS(1) ) THEN
 2332             WRITE (NDS(1),997) CLKFIN
 2333             WRITE (NDS(1),998) CLKFEL
 2334           END IF
 2335         WRITE (NDSO,999)
 2336       END IF
 2337 !
 2338       IF (COAWSTED.EQ.0) THEN
 2339         CALL MPI_FINALIZE  ( IERR_MPI )
 2340       END IF
 2341       CALL FINALIZE_WAV_COUPLING(1)
 2342 !
 2343 ! Formats
 2344 !
 2345   900 FORMAT (/15X,'      *** WAVEWATCH III Program shell ***      '/ &
 2346                15X,'==============================================='/)
 2347   901 FORMAT ( '  Comment character is ''',A,''''/)
 2348 !
 2349   920 FORMAT (/'  Input fields : '/                                   &
 2350                ' --------------------------------------------------')
 2351   921 FORMAT ( '       ',A,2X,A,2X,A)
 2352   922 FORMAT ( ' ' )
 2353 !
 2354   930 FORMAT (/'  Time interval : '/                                  &
 2355                ' --------------------------------------------------')
 2356   931 FORMAT ( '       Starting time : ',A)
 2357   932 FORMAT ( '       Ending time   : ',A/)
 2358 !
 2359   940 FORMAT (/'  Output requests : '/                                &
 2360                ' --------------------------------------------------'/ &
 2361                '       ',A)
 2362   941 FORMAT (/'       Type',I2,' : ',A/                              &
 2363                '      -----------------------------------------')
 2364   942 FORMAT ( '            From     : ',A)
 2365   943 FORMAT ( '            To       : ',A)
 
 
                                                         ww3_shel.F90  page  44
 
 
 2366  1944 FORMAT ( '            Interval : ', 8X,A11/)
 2367  2944 FORMAT ( '            Interval : ', 9X,A10/)
 2368  3944 FORMAT ( '            Interval : ',11X,A8/)
 2369  2945 FORMAT ( '            Point  1 : ',2F8.2,2X,A)
 2370  2955 FORMAT ( '            Point  1 : ',2(F8.1,'E3'),2X,A)
 2371  2946 FORMAT ( '              ',I6,' : ',2F8.2,2X,A)
 2372  2956 FORMAT ( '              ',I6,' : ',2(F8.1,'E3'),2X,A)
 2373  2947 FORMAT ( '            No points defined')
 2374  3945 FORMAT ( '            The file with ',A,' data is ',A,'.')
 2375  6945 FORMAT ( '            IX first,last,inc :',3I5/                 &
 2376                '            IY first,last,inc :',3I5/                 &
 2377                '            Formatted file    :    ',A)
 2378  8945 FORMAT ( '            output dates out of run dates : ', A,     &
 2379                ' deactivated')
 2380 !
 2381   950 FORMAT (/'  Initializations :'/                                 &
 2382                ' --------------------------------------------------')
 2383   951 FORMAT ( '       ',A)
 2384   952 FORMAT ( '       ',I6,2X,A)
 2385   953 FORMAT ( '          ',I6,I11.8,I7.6,3E12.4)
 2386   954 FORMAT ( '            ',A,': file not needed')
 2387   955 FORMAT ( '            ',A,': file OK')
 2388   956 FORMAT ( '            ',A,': file OK, recl =',I3,               &
 2389                '  undef = ',E10.3)
 2390 !
 2391   960 FORMAT (/'  Running model without input fields'/                &
 2392                ' --------------------------------------------------'/)
 2393 !
 2394   970 FORMAT (/'  Running model with input fields'/                   &
 2395                ' --------------------------------------------------')
 2396   971 FORMAT (/'  Updating input at ',A)
 2397   972 FORMAT ( '     Updating ',A)
 2398   973 FORMAT ( '        Past last ',A)
 2399   975 FORMAT (/'  Data assimmilation at ',A)
 2400 !
 2401   997 FORMAT (/'  Initialization time :',F10.2,' s')
 2402   998 FORMAT ( '  Elapsed time        :',F10.2,' s')
 2403 !
 2404   999 FORMAT(/'  End of program '/                                    &
 2405                ' ===================================='/               &
 2406                '         WAVEWATCH III Program shell '/)
 2407 !
 2408  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
 2409                '     ERROR IN OPENING INPUT FILE'/                    &
 2410                '     IOSTAT =',I5/)
 2411 !
 2412  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
 2413                '     PREMATURE END OF INPUT FILE'/)
 2414 !
 2415  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
 2416                '     ERROR IN READING FROM INPUT FILE'/               &
 2417                '     IOSTAT =',I5/)
 2418 !
 2419  1102 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
 2420                '     LEVEL AND CURRENT ARE MIXING COUPLED AND FORCED'/&
 
 
                                                         ww3_shel.F90  page  45
 
 
 2421                '     IT MUST BE FULLY COUPLED OR DISABLED '/)
 2422 !
 2423  1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
 2424                '     ILLEGAL TIME INTERVAL'/)
 2425 !
 2426  1104 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
 2427                '     ERROR IN OPENING POINT FILE'/                    &
 2428                '     IOSTAT =',I5/)
 2429 !
 2430  1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
 2431                '     ERROR IN READING FROM POINT FILE'/               &
 2432                '     IOSTAT =',I5/)
 2433 !
 2434  1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
 2435                '     ILLEGAL ID STRING HOMOGENEOUS FIELD : ',A/)
 2436 !
 2437  1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
 2438                '     TOO MANY HOMOGENEOUS FIELDS : ',A,1X,I4/)
 2439 !
 2440  1062 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : ***'/             &
 2441                '     HOMOGENEOUS NAME NOT RECOGNIZED : ', A/)
 2442 !
 2443  1007 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
 2444                '     INSUFFICIENT DATA FOR HOMOGENEOUS FIELDS'/)
 2445 !
 2446  1008 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
 2447                '     ERROR IN OPENING OUTPUT FILE'/                   &
 2448                '     IOSTAT =',I5/)
 2449 !
 2450  1054 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
 2451                '     POINT OUTPUT ACTIVATED BUT NO POINTS DEFINED'/)
 2452 !
 2453 !/
 2454 !/ End of W3SHEL ----------------------------------------------------- /
 2455 !/
 2456       END SUBROUTINE WW3_init
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
