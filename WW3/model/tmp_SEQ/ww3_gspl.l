                                                         ww3_gspl.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3GSPL
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         18-Nov-2013 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    24-Sep-2012 : Origination.                        ( version 4.10 )
   13 !/    16-Jan-2013 : Add output of mask file (no halo).  ( version 4.10 )
   14 !/    19-Jan-2013 : Tweaking the template file.         ( version 4.10 )
   15 !/    24-Jan-2013 : Set up for minimum of 2 grids.      ( version 4.10 )
   16 !/                  Add XOFF to grid origin in X.
   17 !/                  Fix IDCLSE for partial grids.
   18 !/                  Add FRFLAG option to disable side-by-side
   19 !/                      running of grids in ww3_multi.
   20 !/    29-Jan-2013 : Add error code on stop.             ( version 4.10 )
   21 !/    31-Jan-2013 : Add routine GRLOST.                 ( version 4.10 )
   22 !/    01-Feb-2013 : Speed up GRSEPA.                    ( version 4.10 )
   23 !/                  Add dynamic trim range in GRTRIM.
   24 !/                  Speed up GRFILL.
   25 !/                  Add small grid merge (GRFSML) early in loop.
   26 !/    04-Feb-2013 : Testing on zero grid size added.    ( version 4.10 )
   27 !/                  Corner point in halo for GR1GRD.
   28 !/    04-Mar-2013 : Adding GrADS output.                ( version 4.10 )
   29 !/    05-Aug-2013 : Add UQ/UNO for distances.           ( version 4.12 )
   30 !/    18-Nov-2013 : Add user-defined halo extension.    ( version 4.14 )
   31 !/
   32 !/    Copyright 2012-2013 National Weather Service (NWS),
   33 !/       National Oceanic and Atmospheric Administration.  All rights
   34 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   35 !/       No unauthorized use without permission.
   36 !/
   37 !  1. Purpose :
   38 !
   39 !     Take an existing grid and create from this the grid data for a set
   40 !     of overlapping grids to be used in the ww3_multi code for hybid
   41 !     paralellization.
   42 !
   43 !  2. Method :
   44 !
   45 !     See Section 8.
   46 !
   47 !  3. Parameters :
   48 !
   49 !     Local parameters.
   50 !     ----------------------------------------------------------------
   51 !       NDSI    Int.  Input unit number ("ww3_prep.inp").
   52 !       NDSO    Int.  Output unit number.
   53 !       NDSE    Int.  Error unit number.
   54 !       NDST    Int.  Test output unit number.
   55 !       NDSM    Int.  Unit number for mod_def file.
 
 
                                                         ww3_gspl.F90  page   2
 
 
   56 !       NG      Int.  Number of grids to be generated.
   57 !       NITMAX  Int.  Maximum number of iterations on grid ref.
   58 !       STARG   Real  std target in percent.
   59 !       GLOBAL  Log.  Closure flag.
   60 !       SEA     L.A.  Sea point map.
   61 !     ----------------------------------------------------------------
   62 !
   63 !  4. Subroutines used :
   64 !
   65 !      Name      Type  Module   Description
   66 !     ----------------------------------------------------------------
   67 !      W3NMOD    Subr. W3GDATMD Set number of model.
   68 !      W3SETG    Subr.   Id.    Point to selected model.
   69 !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
   70 !      W3SETW    Subr.   Id.    Point to selected model for wave data.
   71 !      W3NOUT    Subr. W3ODATMD Set number of model for output.
   72 !      W3SETO    Subr.   Id.    Point to selected model for output.
   73 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
   74 !      STRACE    Subr.   Id.    Subroutine tracing.
   75 !      NEXTLN    Subr.   Id.    Get next line from input filw
   76 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
   77 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
   78 !
   79 !      GRINFO    Subr. Internal Compile info on all grids.
   80 !      GRTRIM    Subr. Internal Trim edges of grids.
   81 !      GRFILL    Subr. Internal Fill unassigned space in grid.
   82 !      GRLOST    Subr. Internal Assign "lost points".
   83 !      GRSQRG    Subr. Internal Attempt to square-up grid.
   84 !      GRSNGL    Subr. Internal Remove grid points that stick out.
   85 !      GRSEPA    Subr. Internal Remove separated grid pieces.
   86 !      GRFSML    Subr. Internal Deal with fixed minimum size.
   87 !      GRFLRG    Subr. Internal Deal with fixed maximum size.
   88 !      GR1GRD    Subr. Internal Extract single grid from map.
   89 !     ----------------------------------------------------------------
   90 !
   91 !  5. Called by :
   92 !
   93 !     None, stand-alone program.
   94 !
   95 !  6. Error messages :
   96 !
   97 !  7. Remarks :
   98 !
   99 !  8. Structure :
  100 !
  101 !     ----------------------------------------------------
  102 !        1.a  Number of models.
  103 !                   ( W3NMOD , W3NOUT , W3SETG , W3SETO )
  104 !          b  I-O setup.
  105 !          c  Print heading(s).
  106 !        2.   Read model definition file.      ( W3IOGR )
  107 !        3.   Read options from file.
  108 !        4.  Generate first-guess map of sub-grids
  109 !          a Set up array
  110 !          b First cut with regular grid set up
 
 
                                                         ww3_gspl.F90  page   3
 
 
  111 !            1 Set up 'checkerboard'
  112 !            2 Fill checkerboard
  113 !            3 Remove smallest grids as necessary
  114 !            4 Store first guess in MSPLIT
  115 !        5.  Refine map of sub-grids (no halo).
  116 !          a Set up loop.                      ( GRINFO )
  117 !          b Remove small grids.               ( GRFSML )
  118 !          c Trim edges of grids               ( GRTRIM )
  119 !                                      ( GRFILL, GRLOST )
  120 !          d Attempt to square-up grid         ( GRINFO )
  121 !                                              ( GRSQRG )
  122 !                                      ( GRFILL, GRLOST )
  123 !          e Remove mid-sea points sticking out of grid
  124 !                                              ( GRSNGL )
  125 !          f Remove detached grid parts.       ( GRSEPA )
  126 !          g Recompute stats                   ( GRINFO )
  127 !          h Optional GrADS output.
  128 !          i Test convergence
  129 !            Check if stuck on min or max.     ( GRFSML )
  130 !                                              ( GRFLRG )
  131 !          j Test output
  132 !        6.  Output info for all sub grids.
  133 !          a Set up loop.                      ( GRINFO )
  134 !          b Extract grid including halo.      ( GR1GRD )
  135 !        7.  End of program.
  136 !     ----------------------------------------------------
  137 !
  138 !  9. Switches :
  139 !
  140 !     !/PRn   Select propgation scheme.
  141 !
  142 !     !/O16   Generate GrADS output of grid partitioning.
  143 !
  144 !     !/S     Enable subroutine tracing.
  145 !     !/T     Enable test output (main).
  146 !     !/T1    Enable test output (GRINFO).
  147 !     !/T2    Enable test output (GRFILL).
  148 !     !/T3    Enable test output (GRSNGL).
  149 !     !/T4    Enable test output (GRSEPA).
  150 !     !/T5    Enable test output (GRFSML).
  151 !     !/T6    Enable test output (GRFLRG).
  152 !     !/T7    Enable test output (GR1GRD).
  153 !
  154 ! 10. Source code :
  155 !
  156 !/ ------------------------------------------------------------------- /
  157       USE CONSTANTS
  158 !/
  159 !     USE W3GDATMD, ONLY: W3NMOD, W3SETG
  160       USE W3ADATMD, ONLY: W3NAUX, W3SETA
  161       USE W3ODATMD, ONLY: W3NOUT, W3SETO
  162       USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
  163       USE W3ARRYMD, ONLY : OUTA2I, OUTA2R
  164       USE W3IOGRMD, ONLY: W3IOGR
  165 !/
 
 
                                                         ww3_gspl.F90  page   4
 
 
  166       USE W3GDATMD
  167       USE W3ODATMD, ONLY: NDSE, NDST, NDSO, FNMPRE
  168 !
  169       IMPLICIT NONE
  170 !/
  171 !/ ------------------------------------------------------------------- /
  172 !/ Local parameters
  173 !/
  174       INTEGER                 :: NDSI, NDSM, NDSTRC, NTRACE, J, IERR, &
  175                                  NG, IX, IY, NGB, NGX, NGY, IG, IGG,  &
  176                                  IGX, IGY, IGY0, IGYN, IGX0, IGXN,    &
  177                                  MINGRD, MINNR, MINNXT, MINNNR,       &
  178                                  NITMAX, IIT, INGMIN, INGMAX,         &
  179                                  INGMNC, INGMXC, INGLAG, JJ,          &
  180                                  NSTDLG, MSTDLG = 5, NSEAT, J1, J2,   &
  181                                  J3, J4, J5, IDFM1, IDFM2, IDFM3,     &
  182                                  IDLA1, IDLA2, IDLA3, VSC3, NHEXT
  183       INTEGER, ALLOCATABLE    :: MSPLIT(:,:), MTEMP(:,:), INGRD(:)
  184       REAL                    :: RATIO1, XMEAN, STARG, STDMIN,        &
  185                                  ZBDUM, ZBMIN, VSC1, VSC2, FRACL, FRACH
  186       LOGICAL                 :: GLOBAL, OK, DONE, FRFLAG
  187       LOGICAL, ALLOCATABLE    :: ISNEXT(:), SEA(:,:)
  188       CHARACTER(LEN=1)        :: COMSTR
  189       CHARACTER(LEN=3)        :: G0ID
  190       CHARACTER(LEN=4)        :: IDGRID, IDCLSE, PTCLSE
  191       CHARACTER(LEN=6)        :: NRFMT
  192       CHARACTER(LEN=11)       :: FEXT, AEXT
  193       CHARACTER(LEN=16)       :: RFORM1, RFORM2, RFORM3
  194       CHARACTER(LEN=20)       :: FNAME, INAME
  195 !
  196       TYPE STATS_GRID
  197         LOGICAL               :: STRADLE, INSTAT
  198         INTEGER               :: NPTS, NYL, NYH, NXL, NXH
  199       END TYPE STATS_GRID
  200 !
  201       TYPE STATS_MEAN
  202         INTEGER               :: NMIN, NMAX
  203         REAL                  :: RSTD
  204       END TYPE STATS_MEAN
  205 !
  206       TYPE PART_GRID
  207         INTEGER               :: NX, NY, NSEA
  208         INTEGER, POINTER      :: MASK(:,:)
  209         REAL                  :: X0, Y0, SX, SY
  210         REAL, POINTER         :: ZBIN(:,:), OBSX(:,:), OBSY(:,:)
  211         LOGICAL               :: GLOBAL
  212       END TYPE PART_GRID
  213 !
  214       TYPE(STATS_GRID), POINTER :: GSTATS(:), GSTOLD(:)
  215       TYPE(STATS_MEAN)          :: MSTATS   , MSTOLD
  216       TYPE(PART_GRID), POINTER  :: PGRID(:)
  217 !/
  218 !/ ------------------------------------------------------------------- /
  219 !/
  220 ! 1.a  Set number of models
 
 
                                                         ww3_gspl.F90  page   5
 
 
  221 !
  222       CALL W3NMOD ( 1, 6, 6 )
  223       CALL W3SETG ( 1, 6, 6 )
  224       CALL W3NAUX (    6, 6 )
  225       CALL W3SETA ( 1, 6, 6 )
  226       CALL W3NOUT (    6, 6 )
  227       CALL W3SETO ( 1, 6, 6 )
  228 !
  229 ! 1.b  IO set-up.
  230 !
  231       NDSI   = 10
  232       NDSO   =  6
  233       NDSE   =  6
  234       NDST   =  6
  235       NDSM   = 11
  236 !
  237       NDSTRC =  6
  238       NTRACE = 100
  239       CALL ITRACE ( NDSTRC, NTRACE )
  240 !
  241 ! 1.c Print header
  242 !
  243       WRITE (NDSO,900)
  244 !
  245       J      = LEN_TRIM(FNMPRE)
  246       OPEN (NDSI,FILE=FNMPRE(:J)//'ww3_gspl.inp',STATUS='OLD',        &
  247             ERR=800,IOSTAT=IERR)
  248       REWIND (NDSI)
  249       READ (NDSI,'(A)',END=801,ERR=802,IOSTAT=IERR) COMSTR
  250       IF (COMSTR.EQ.' ') COMSTR = '$'
  251       WRITE (NDSO,901) COMSTR
  252 !
  253 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  254 ! 2.  Read model definition file.
  255 !
  256       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  257       READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) FEXT
  258 !
  259       CALL W3IOGR ( 'READ', NDSM, 1, FEXT )
  260       CLOSE (NDSM)
  261 !
  262       WRITE (NDSO,902) FEXT, GNAME
  263 !
  264       SELECT CASE (GTYPE)
  265         CASE (RLGTYPE)
  266           WRITE ( NDSO,903) 'rectilinear'
  267           IDGRID = 'RECT'
  268         CASE (CLGTYPE)
  269           WRITE ( NDSO,903) 'curvictilinear'
  270           IDGRID = 'CURV'
  271         CASE (UNGTYPE)
  272           WRITE ( NDSO,903) 'unstructured'
  273           IDGRID = 'UNST'
  274           GOTO 820
  275         CASE DEFAULT
 
 
                                                         ww3_gspl.F90  page   6
 
 
  276           WRITE ( NDSO,903) 'not recognized'
  277           GOTO 821
  278       END SELECT
  279 !
  280       SELECT CASE (ICLOSE)
  281         CASE (ICLOSE_NONE)
  282           WRITE ( NDSO,904) 'none'
  283           IDCLSE = 'NONE'
  284           GLOBAL = .FALSE.
  285         CASE (ICLOSE_SMPL)
  286           WRITE ( NDSO,904) 'global (simple)'
  287           IDCLSE = 'SMPL'
  288           GLOBAL = .TRUE.
  289         CASE (ICLOSE_TRPL)
  290           WRITE ( NDSO,904) 'global (tripolar)'
  291           IDCLSE = 'TRPL'
  292           GLOBAL = .TRUE.
  293           GOTO 822
  294         CASE DEFAULT
  295           WRITE ( NDSO,904) 'not recognized'
  296           GOTO 823
  297       END SELECT
  298 !
  299       WRITE (NDSO,905) NX, NY, NSEA
  300       IF ( NSEA .EQ. 0 ) GOTO 824
  301 !
  302 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  303 ! 3.  Read options from input file.
  304 !
  305       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  306       READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) NG, NITMAX, STARG, NHEXT
  307       NG     = MAX ( 2, NG )
  308       NITMAX = MAX ( 1, NITMAX )
  309       STARG  = MAX ( 0. , STARG )
  310       NHEXT  = MAX ( 0, NHEXT )
  311       WRITE (NDSO,930) NG, NITMAX, STARG, NHEXT
  312 !
  313       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  314       READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) IDLA1, IDFM1,         &
  315                                                 VSC1, RFORM1
  316       IF (IDLA1.LT.1 .OR. IDLA1.GT.4) IDLA1  = 1
  317       IF (IDFM1.LT.1 .OR. IDFM1.GT.3) IDFM1  = 1
  318       IF ( ABS(VSC1) .LT. 1.E-15 )    VSC1   = 1.
  319                                                  
  320       WRITE (NDSO,931) IDLA1, IDFM1, VSC1, RFORM1
  321 !
  322       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  323       READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) IDLA2, IDFM2,         &
  324                                                 VSC2, RFORM2
  325       IF (IDLA2.LT.1 .OR. IDLA2.GT.4) IDLA2  = 1
  326       IF (IDFM2.LT.1 .OR. IDFM2.GT.3) IDFM2  = 1
  327       IF ( ABS(VSC2) .LT. 1.E-15 )    VSC2   = 1.
  328       IF ( TRFLAG .EQ. 0 ) THEN
  329           WRITE (NDSO,932)
  330         ELSE
 
 
                                                         ww3_gspl.F90  page   7
 
 
  331           WRITE (NDSO,933) IDLA2, IDFM2, VSC2, RFORM2
  332         END IF
  333 !
  334       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  335       READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) IDLA3, IDFM3,         &
  336                                                 VSC3, RFORM3
  337       IF (IDLA3.LT.1 .OR. IDLA3.GT.4) IDLA3  = 1
  338       IF (IDFM3.LT.1 .OR. IDFM3.GT.3) IDFM3  = 1
  339       IF (     VSC3  .EQ. 0      )    VSC3   = 1
  340       WRITE (NDSO,934) IDLA3, IDFM3, VSC3, RFORM3
  341 !
  342       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  343       READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) FRACL, FRACH, FRFLAG
  344       FRACL = MAX ( 0. , FRACL )
  345       FRACH = MIN ( 1. , FRACH )
  346       WRITE (NDSO,935) FRACL, FRACH
  347       IF ( FRACL .GT. FRACH ) GOTO 830
  348       IF ( .NOT. FRFLAG ) WRITE (NDSO,936)
  349 !
  350 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  351 ! 4.  Generate map of sub-grids (no halo)
  352 ! 4.a Set up array
  353 !
  354       ALLOCATE ( MSPLIT(NY,NX) , MTEMP(NY,NX), SEA(NY,NX) )
  355 !
  356       DO IY=1, NY
  357         DO IX=1, NX
  358           IF ( MAPSTA(IY,IX) .EQ. 0 ) THEN
  359               MSPLIT(IY,IX) = 0
  360               SEA   (IY,IX) = .FALSE.
  361             ELSE
  362               MSPLIT(IY,IX) = -1
  363               SEA   (IY,IX) = .TRUE.
  364             END IF
  365           END DO
  366         END DO
  367               
  368 !
  369 ! 4.b First cut with regular grid set up
  370 ! 4.b.1 Set up 'checkerboard'
  371 !
  372       RATIO1 = REAL(NX) / REAL(NY)
  373 !
  374       NGX    = 1
  375       NGY    = 1
  376 !
  377       DO
  378         IF ( NGX*NGY .GE. NG ) EXIT
  379         IF ( REAL(NGX)/REAL(NGY) .GT. RATIO1 ) THEN
  380             NGY    = NGY + 1
  381           ELSE
  382             NGX    = NGX + 1
  383           END IF
  384         END DO
  385 !
 
 
                                                         ww3_gspl.F90  page   8
 
 
  386       IF ( NGX .GT. NGY ) THEN
  387           IF ( (NGY-1)*NGX .GE. NG ) NGY = NGY - 1
  388           IF ( (NGX-1)*NGY .GE. NG ) NGX = NGX - 1
  389         ELSE
  390           IF ( (NGY-1)*NGX .GE. NG ) NGY = NGY - 1
  391           IF ( (NGX-1)*NGY .GE. NG ) NGX = NGX - 1
  392         END IF
  393 !
  394 ! 4.b.2 Fill checkerboard
  395 !
  396       J      = 0
  397       DO
  398 !
  399         MTEMP  = MSPLIT
  400         IG     = 1
  401         IGYN   = 0
  402         J      = J + 1
  403         ALLOCATE ( INGRD(NGX*NGY) )
  404         INGRD  = 0
  405 !
  406         DO IGY=1, NGY
  407 !
  408           IGY0   = IGYN + 1
  409           IF ( IGY .EQ. NGY ) THEN
  410               IGYN   = NY
  411             ELSE
  412               IGYN   = NINT ( REAL(NY*IGY) / REAL(NGY)  )
  413             END IF
  414           IGXN   = 0
  415 !
  416           DO IGX=1, NGX
  417 !
  418             IGX0   = IGXN + 1
  419             IF ( IGX .EQ. NGX ) THEN
  420                 IGXN   = NX
  421               ELSE
  422                 IGXN   = NINT ( REAL(NX*IGX) / REAL(NGX)  )
  423               END IF
  424 !
  425             DO IX=IGX0, IGXN
  426               DO IY=IGY0, IGYN
  427                 IF ( MTEMP(IY,IX) .EQ. -1 ) THEN
  428                     MTEMP(IY,IX) = IG
  429                     INGRD(IG) = INGRD(IG) + 1
  430                   END IF
  431                 END DO
  432               END DO
  433 !
  434             IF ( INGRD(IG) .GT. 0 ) THEN
  435                 IG     = IG + 1
  436               END IF
  437 !
  438             END DO
  439 !
  440           END DO
 
 
                                                         ww3_gspl.F90  page   9
 
 
  441 !
  442         IG     = IG - 1
  443         IF ( IG .LT. NG ) THEN
  444             IF ( NGX .LT. NGY ) THEN
  445                 NGY     = NGY + 1
  446               ELSE
  447                 NGX     = NGX + 1
  448               END IF
  449             DEALLOCATE ( INGRD )
  450           ELSE
  451             EXIT
  452           END IF
  453 !
  454         END DO
  455 !
  456       MINGRD = 0
  457       DO J=1, IG
  458         MINGRD = MINGRD + INGRD(J)
  459         END DO
  460       IF ( MINGRD .NE. NSEA ) GOTO 825
  461 !
  462 ! 4.b.3 Merge smallest grids as necessary
  463 !
  464       IGG    = IG
  465 !
  466       DO
  467 !
  468         IF ( IGG .EQ. NG ) EXIT
  469 !
  470         MINGRD = NSEA
  471         MINNR  = 0
  472         DO J=1, IG
  473           IF ( INGRD(J) .LT. MINGRD ) THEN
  474               MINGRD = INGRD(J)
  475               MINNR  = J
  476             END IF
  477           END DO
  478         INGRD(MINNR) = NSEA + 1
  479 !
  480         ALLOCATE ( ISNEXT(0:IG) )
  481         ISNEXT = .FALSE.
  482 !
  483         DO IY=1, NY-1
  484           DO IX=1, NX-1
  485             IF ( ( MTEMP(IY  ,IX  ) - MINNR ) *                       &
  486                  ( MTEMP(IY+1,IX  ) - MINNR ) *                       &
  487                  ( MTEMP(IY  ,IX+1) - MINNR ) *                       &
  488                  ( MTEMP(IY+1,IX+1) - MINNR ) .EQ. 0 ) THEN
  489                 ISNEXT(MTEMP(IY  ,IX  )) = .TRUE.
  490                 ISNEXT(MTEMP(IY+1,IX  )) = .TRUE.
  491                 ISNEXT(MTEMP(IY  ,IX+1)) = .TRUE.
  492                 ISNEXT(MTEMP(IY+1,IX+1)) = .TRUE.
  493               END IF
  494             END DO
  495           END DO
 
 
                                                         ww3_gspl.F90  page  10
 
 
  496 !
  497         IF ( GLOBAL ) THEN
  498             DO IY=1, NY-1
  499               IF ( ( MTEMP(IY  ,NX) - MINNR ) *                       &
  500                    ( MTEMP(IY+1,NX) - MINNR ) *                       &
  501                    ( MTEMP(IY  , 1) - MINNR ) *                       &
  502                    ( MTEMP(IY+1, 1) - MINNR ) .EQ. 0 ) THEN
  503                   ISNEXT(MTEMP(IY  ,NX)) = .TRUE.
  504                   ISNEXT(MTEMP(IY+1,NX)) = .TRUE.
  505                   ISNEXT(MTEMP(IY  , 1)) = .TRUE.
  506                   ISNEXT(MTEMP(IY+1, 1)) = .TRUE.
  507                 END IF
  508               END DO
  509           END IF
  510 !
  511         MINNXT = NSEA
  512         MINNNR = 0
  513         DO J=1, IG
  514           IF ( ISNEXT(J) .AND. ( INGRD(J) .LT. MINNXT ) ) THEN
  515               MINNXT = INGRD(J)
  516               MINNNR = J
  517             END IF
  518           END DO
  519 !
  520         IF ( MINNNR .GT. 0 ) THEN
  521             DO IY=1, NY
  522               DO IX=1, NX
  523                 IF ( MTEMP(IY,IX) .EQ. MINNR ) THEN
  524                     MTEMP(IY,IX) = MINNNR
  525                     INGRD(MINNNR) = INGRD(MINNNR) + 1
  526                   END IF
  527                 END DO
  528               END DO
  529             IGG    = IGG - 1
  530           END IF
  531 !
  532         DEALLOCATE ( ISNEXT)
  533 !
  534         END DO
  535 !
  536       DO J=1, IG
  537         IF ( INGRD(J) .GT. NSEA ) INGRD(J) = 0
  538         END DO
  539 !
  540 ! 4.b.4 Store first guess in MSPLT
  541 !
  542       IGG    = 0
  543       DO J=1, IG
  544         IF ( INGRD(J) .NE. 0 ) THEN
  545             IGG    = IGG + 1
  546             DO IY=1, NY
  547               DO IX=1, NX
  548                 IF ( MTEMP(IY,IX) .EQ. J ) MSPLIT(IY,IX) = IGG
  549                 END DO
  550               END DO
 
 
                                                         ww3_gspl.F90  page  11
 
 
  551           END IF
  552         END DO
  553 !
  554 ! 5.b.5 Optional GrADS output
  555 !
  556 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  557 ! 5.  Refine grids
  558 ! 5.a Set up loop
  559 !
  560       ALLOCATE ( GSTATS(NG), GSTOLD(NG), PGRID(NG) )
  561       GSTATS(:)%INSTAT = .TRUE.
  562       WRITE (NDSO,950)
  563       DONE   = .FALSE.
  564 !
  565       CALL GRINFO
  566       WRITE (NDSO,951) 0, MSTATS%NMIN, MSTATS%NMAX,                   &
  567                           100.*MSTATS%RSTD/XMEAN
  568       G0ID   = '5.a'
  569       IF ( MSTATS%NMIN .EQ. 0 ) GOTO 850
  570       INGMIN = MSTATS%NMIN
  571       INGMAX = MSTATS%NMAX
  572       INGMNC = 0
  573       INGMXC = 0
  574       INGLAG = 3
  575       STDMIN = 100.*MSTATS%RSTD/XMEAN
  576       NSTDLG = 0
  577 !
  578       DO IIT=1, NITMAX
  579 !
  580         IF ( NG .EQ. 1 ) EXIT
  581 !
  582         MSTOLD = MSTATS
  583         GSTOLD = GSTATS
  584 !
  585 ! 5.b Small grid attempt to merge
  586 !
  587         IF ( MSTATS%NMIN .LT. NINT(0.45*XMEAN) ) THEN
  588 !
  589             CALL GRFSML
  590             CALL GRINFO
  591 !
  592             G0ID   = '5.b'
  593             IF ( MSTOLD%NMIN .NE. MSTATS%NMIN ) THEN
  594                 WRITE (NDSO,951) IIT, MSTATS%NMIN, MSTATS%NMAX,       &
  595                               100.*MSTATS%RSTD/XMEAN
  596                 IF ( MSTATS%NMIN .EQ. 0 ) GOTO 850
  597                                                   
  598                 CYCLE
  599               ELSE
  600                 WRITE (NDSO,952)      MSTATS%NMIN, MSTATS%NMAX,       &
  601                               100.*MSTATS%RSTD/XMEAN
  602                 IF ( MSTATS%NMIN .EQ. 0 ) GOTO 850
  603               END IF
  604 !
  605          END IF
 
 
                                                         ww3_gspl.F90  page  12
 
 
  606 !
  607 ! 5.c Trim edges of grids and reassign
  608 !
  609         CALL GRTRIM
  610         CALL GRFILL ( 2 )
  611 !
  612 ! 5.d Attempt to quare-up grid
  613 !
  614         CALL GRINFO   ! call needed as GRSQRG uses grid ranges
  615         CALL GRSQRG
  616         CALL GRFILL ( 1 )
  617 !
  618 ! 5.e Remove mid-sea points sticking out of grid
  619 !     Call more than once to remove most .....
  620 !
  621         OK     = .TRUE.
  622 !
  623         DO JJ=1, 4
  624           CALL GRSNGL ( OK )
  625           END DO
  626 !
  627 ! 5.f Remove parts of grid separated from main body, and attachable to
  628 !     other grids.
  629 !
  630         CALL GRSEPA ( OK , 0.10 )
  631         IF ( .NOT. OK ) THEN
  632             CALL GRFILL ( 1 )
  633             OK     = .TRUE.
  634           END IF
  635 !
  636 ! 5.g Re-compute grid stats
  637 !
  638         CALL GRINFO
  639         WRITE (NDSO,951) IIT, MSTATS%NMIN, MSTATS%NMAX,               &
  640                               100.*MSTATS%RSTD/XMEAN
  641 !
  642         G0ID   = '5.g'
  643         IF ( MSTATS%NMIN .EQ. 0 ) GOTO 850
  644 !
  645 ! 5.h Optional GrADS output
  646 !
  647 ! 5.i Convergence tests
  648 ! ... The quick one
  649 !
  650         IF ( 100.*MSTATS%RSTD/XMEAN .LE. STARG ) THEN
  651             WRITE (NDSO,959)
  652             EXIT
  653           END IF
  654 !
  655 ! ... Monitoring convergence ....
  656 !
  657         IF ( 100.*MSTATS%RSTD/XMEAN .LT. 1.0001*STDMIN ) THEN
  658             IF ( NSTDLG .LT. MSTDLG ) THEN
  659                 NSTDLG = 0
  660               ELSE
 
 
                                                         ww3_gspl.F90  page  13
 
 
  661                 WRITE (NDSO,959)
  662                 EXIT
  663               END IF
  664             STDMIN = 100.*MSTATS%RSTD/XMEAN
  665           ELSE
  666             NSTDLG = NSTDLG + 1
  667             IF ( NSTDLG .GT. MSTDLG ) STDMIN = 1.01*STDMIN
  668           END IF
  669 !
  670 ! ... Check if stuck on min or max
  671 !
  672         IF ( MSTATS%NMAX .LT. INGMAX ) THEN
  673             INGMAX = MSTATS%NMAX
  674             INGMXC = 0
  675           ELSE
  676             INGMXC = INGMXC + 1
  677           END IF
  678 !
  679         IF ( MSTATS%NMIN .GT. INGMIN ) THEN
  680             INGMIN = MSTATS%NMIN
  681             INGMNC = 0
  682           ELSE
  683             INGMNC = INGMNC + 1
  684           END IF
  685 !
  686 ! ... Stuck in min ...
  687 !
  688         IF ( INGMNC .GE. INGLAG ) THEN
  689 !
  690             IF ( REAL(INGMIN) .LT. 0.85*XMEAN ) THEN
  691 !
  692                 CALL GRFSML
  693                 CALL GRINFO
  694                 WRITE (NDSO,952) MSTATS%NMIN, MSTATS%NMAX,            &
  695                                  100.*MSTATS%RSTD/XMEAN
  696                 INGMIN = MSTATS%NMIN
  697                 INGMAX = MSTATS%NMAX
  698                 INGMNC = 0
  699                 INGMXC = 0
  700                 IF ( DONE ) EXIT
  701 !
  702               END IF
  703 !
  704           END IF
  705 !
  706 ! ... Stuck in max ...
  707 !
  708         IF ( INGMXC .GE. INGLAG ) THEN
  709 !
  710             IF ( REAL(INGMAX) .GT. 1.075*XMEAN ) THEN
  711 !
  712                 CALL GRINFO
  713                 WRITE (NDSO,952) MSTATS%NMIN, MSTATS%NMAX,            &
  714                                  100.*MSTATS%RSTD/XMEAN
  715                 INGMIN = MSTATS%NMIN
 
 
                                                         ww3_gspl.F90  page  14
 
 
  716                 INGMAX = MSTATS%NMAX
  717                 INGMNC = 0
  718                 INGMXC = 0
  719                 IF ( DONE ) EXIT
  720 !
  721               END IF
  722 !
  723           END IF
  724 !
  725         END DO
  726 !
  727 ! 5.j Test output
  728 !
  729       WRITE (NDSO,955)
  730       ALLOCATE ( ISNEXT(NG) )
  731       ISNEXT = .TRUE.
  732       CALL GRINFO
  733 !
  734       DO JJ=1, NG
  735         MINNR  = NSEA + 1
  736         DO J=1, NG
  737           IF ( ISNEXT(J) .AND. GSTATS(J)%NPTS.LT.MINNR ) THEN
  738               MINNR  = GSTATS(J)%NPTS
  739               IG     = J
  740             END IF
  741           END DO
  742         ISNEXT(IG) = .FALSE.
  743         WRITE (NDST,956) IG, GSTATS(IG)%STRADLE, GSTATS(IG)%NPTS,     &
  744                              GSTATS(IG)%NXL, GSTATS(IG)%NXH,          &
  745                              GSTATS(IG)%NYL, GSTATS(IG)%NYH
  746         END DO
  747 !
  748       DEALLOCATE ( ISNEXT )
  749 !
  750 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  751 ! 6.  Generate output to make separate grids
  752 ! 6.a Set up loop
  753 !
  754       WRITE (NDSO,960)
  755 !
  756       ZBDUM  = 999.
  757       IF ( MAXVAL(ZB) .LT. -0.11 ) THEN
  758           ZBMIN  = -0.1
  759         ELSE
  760           ZBMIN  = MAXVAL(ZB) + 1.
  761           ZBDUM  = MAX ( ZBDUM , ZBMIN+1 )
  762         END IF
  763 !
  764       J1     = LEN_TRIM(FEXT)
  765       J2     = 1 + INT(LOG10(REAL(NG)+0.5))
  766       WRITE (NRFMT,'(A2,I1,A1,I1,A1)') '(I', J2, '.', J2, ')'
  767 !
  768       IF ( J1 + J2 + 2 .LE. 10 ) THEN
  769           FNAME = FEXT(:J1) // '_p'
  770           J3     = J1 + 3
 
 
                                                         ww3_gspl.F90  page  15
 
 
  771         ELSE
  772           FNAME = 'part_'
  773           J3     = 6
  774         END IF
  775       J4     = J3 + J2 - 1
  776 !
  777       NSEAT  = 0
  778 !
  779       DO IG=1, NG
  780 !
  781 ! 6.b Extract grid including halo
  782 !
  783         WRITE (NDSO,961) IG
  784         CALL GR1GRD
  785         NSEAT  = NSEAT + PGRID(IG)%NSEA
  786 !
  787         WRITE (AEXT,NRFMT) IG
  788         FNAME(J3:J4) = AEXT(:J2)
  789         J      = LEN_TRIM(FNMPRE)
  790 !
  791 ! 6.c Writing bottom file
  792 !
  793         J5    = J4 + 4
  794         FNAME(J4+1:J5) = '.bot'
  795         WRITE (NDSO,962) FNAME(:J5)
  796 !
  797         IF ( IDFM1 .EQ. 3 ) THEN
  798             OPEN (NDSM,FILE=FNMPRE(:J)//FNAME(:J5),                   &
  799                   FORM='UNFORMATTED',ERR=860,IOSTAT=IERR)
  800           ELSE
  801             OPEN (NDSM,FILE=FNMPRE(:J)//FNAME(:J5), ERR=860,IOSTAT=IERR)
  802           END IF
  803         REWIND (NDSM)
  804         CALL OUTA2R ( PGRID(IG)%ZBIN, PGRID(IG)%NX, PGRID(IG)%NY,     &
  805                       1, PGRID(IG)%NX, 1, PGRID(IG)%NY, NDSM, NDST,   &
  806                       NDSE, IDFM1, RFORM1, IDLA1, VSC1, 0.0 )
  807         CLOSE (NDSM)
  808 !
  809 ! 6.d Writing obstruction file
  810 !
  811         J5    = J4 + 5
  812         FNAME(J4+1:J5) = '.obst'
  813 !
  814         IF ( TRFLAG .EQ. 0 ) THEN
  815             WRITE (NDSO,963) FNAME(:J5)
  816           ELSE
  817             WRITE (NDSO,962) FNAME(:J5)
  818 !
  819             IF ( IDFM2 .EQ. 3 ) THEN
  820                 OPEN (NDSM,FILE=FNMPRE(:J)//FNAME(:J5),               &
  821                       FORM='UNFORMATTED',ERR=860,IOSTAT=IERR)
  822               ELSE
  823                 OPEN (NDSM,FILE=FNMPRE(:J)//FNAME(:J5),               &
  824                       ERR=860,IOSTAT=IERR)
  825               END IF
 
 
                                                         ww3_gspl.F90  page  16
 
 
  826             REWIND (NDSM)
  827             CALL OUTA2R ( PGRID(IG)%OBSX, PGRID(IG)%NX, PGRID(IG)%NY, &
  828                           1, PGRID(IG)%NX, 1, PGRID(IG)%NY, NDSM,     &
  829                           NDST, NDSE, IDFM2, RFORM2, IDLA2, VSC2, 0.0 )
  830             CALL OUTA2R ( PGRID(IG)%OBSY, PGRID(IG)%NX, PGRID(IG)%NY, &
  831                           1, PGRID(IG)%NX, 1, PGRID(IG)%NY, NDSM,     &
  832                           NDST, NDSE, IDFM2, RFORM2, IDLA2, VSC2, 0.0 )
  833             CLOSE (NDSM)
  834 !
  835           END IF
  836 !
  837 ! 6.e Writing mask file
  838 !
  839         J5    = J4 + 5
  840         FNAME(J4+1:J5) = '.mask'
  841         WRITE (NDSO,962) FNAME(:J5)
  842 !
  843         IF ( IDFM3 .EQ. 3 ) THEN
  844             OPEN (NDSM,FILE=FNMPRE(:J)//FNAME(:J5),                   &
  845                   FORM='UNFORMATTED',ERR=860,IOSTAT=IERR)
  846           ELSE
  847             OPEN (NDSM,FILE=FNMPRE(:J)//FNAME(:J5), ERR=860,IOSTAT=IERR)
  848           END IF
  849         REWIND (NDSM)
  850         CALL OUTA2I ( PGRID(IG)%MASK, PGRID(IG)%NX, PGRID(IG)%NY,     &
  851                       1, PGRID(IG)%NX, 1, PGRID(IG)%NY, NDSM, NDST,   &
  852                       NDSE, IDFM3, RFORM3, IDLA3, VSC3, 0 )
  853         CLOSE (NDSM)
  854 !
  855 ! 6.f Writing input file
  856 !
  857         J5    = J4 + 5
  858         FNAME(J4+1:J5) = '.tmpl'
  859         WRITE (NDSO,962) FNAME(:J5)
  860 !
  861         OPEN (NDSM,FILE=FNMPRE(:J)//FNAME(:J5), ERR=860,IOSTAT=IERR)
  862 !
  863         GNAME(31-J2:30) = AEXT
  864         GNAME(30-J2:30-J2) = 'p'
  865         WRITE (NDSM,965) GNAME, SIG(2)/SIG(1), TPIINV*SIG(1), NK,     &
  866                          NTH, TH(1)/DTH, FLDRY, FLCX, FLCY, FLCTH,    &
  867                          FLCK, FLSOU, DTMAX, DTCFL, DTCFLI, DTMIN
  868         J5     = LEN_TRIM(RFORM1)
  869         IF ( REAL(PGRID(IG)%NX) * PGRID(IG)%SX .LT. 359.9 ) THEN
  870             PTCLSE = 'NONE'
  871           ELSE
  872             PTCLSE = IDCLSE
  873           END IF
  874         WRITE (NDSM,966) IDGRID, FLAGLL, PTCLSE,                      &
  875                          PGRID(IG)%NX, PGRID(IG)%NY,                  &
  876                          PGRID(IG)%SX, PGRID(IG)%SY,                  &
  877                          PGRID(IG)%X0, PGRID(IG)%Y0,                  &
  878                          ZBMIN, DMIN, VSC1, IDLA1, IDFM1,             &
  879                          RFORM1(:J5), FNAME(:J4)//'.bot'
  880         IF ( TRFLAG .NE. 0 ) THEN
 
 
                                                         ww3_gspl.F90  page  17
 
 
  881         J5     = LEN_TRIM(RFORM2)
  882             WRITE (NDSM,967) VSC2,IDLA2, IDFM2, RFORM2(:J5),          &
  883                              FNAME(:J4)//'.obst'
  884           END IF
  885         J5     = LEN_TRIM(RFORM3)
  886         WRITE (NDSM,968) IDLA3, IDFM3, RFORM3(:J5), FNAME(:J4)//'.mask'
  887         CLOSE (NDSM)
  888 !
  889         END DO
  890 !
  891       WRITE (NDSO,969) 100. * (REAL(NSEAT)/REAL(NSEA)-1.)
  892 !
  893 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  894 ! 7.  Write part of ww3_multi.inp
  895 !
  896       J5     = 11+J1+J2
  897       INAME(:J5) = 'ww3_multi.'//FEXT(:J1)//'.'//AEXT(:J2)
  898       OPEN (NDSM,FILE=FNMPRE(:J)//INAME(:J5), ERR=870,IOSTAT=IERR)
  899 !
  900       DO IG=1, NG
  901         WRITE (AEXT,NRFMT) IG
  902         FNAME(J3:J4) = AEXT(:J2)
  903         IF ( FRFLAG ) THEN
  904             WRITE (NDSM,970) FNAME(:J4),                              &
  905                          FRACL + REAL(IG-1)*(FRACH-FRACL)/REAL(NG),   &
  906                          FRACL + REAL( IG )*(FRACH-FRACL)/REAL(NG)
  907           ELSE
  908             WRITE (NDSM,970) FNAME(:J4), FRACL, FRACH
  909           END IF
  910         END DO
  911 !
  912       CLOSE (NDSM)
  913 !
  914 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  915 ! 8.  Write mask file (no halo)
  916 !
  917       J5     = 10+J1+J2
  918       INAME(:J5) = 'ww3_mask.'//FEXT(:J1)//'.'//AEXT(:J2)
  919       OPEN (NDSM,FILE=FNMPRE(:J)//INAME(:J5), ERR=870,IOSTAT=IERR)
  920 !
  921       DO IY=1, NY
  922         WRITE (NDSM,980) MSPLIT(IY,:)
  923         END DO
  924 !
  925 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  926 ! 9.  End of program
  927 !
  928       GOTO 888
  929 !
  930 ! Error escape locations
  931 !
  932   800 CONTINUE
  933       WRITE (NDSE,1000) IERR
  934       CALL EXTCDE ( 40 )
  935 !
 
 
                                                         ww3_gspl.F90  page  18
 
 
  936   801 CONTINUE
  937       WRITE (NDSE,1001)
  938       CALL EXTCDE ( 41 )
  939 !
  940   802 CONTINUE
  941       WRITE (NDSE,1002) IERR
  942       CALL EXTCDE ( 42 )
  943 !
  944   820 CONTINUE
  945       WRITE (NDSE,1020) GTYPE
  946       CALL EXTCDE ( 20 )
  947 !
  948   821 CONTINUE
  949       WRITE (NDSE,1021) GTYPE
  950       CALL EXTCDE ( 21 )
  951 !
  952   822 CONTINUE
  953       WRITE (NDSE,1022) ICLOSE
  954       CALL EXTCDE ( 22 )
  955 !
  956   823 CONTINUE
  957       WRITE (NDSE,1023) ICLOSE
  958       CALL EXTCDE ( 23 )
  959 !
  960   824 CONTINUE
  961       WRITE (NDSE,1024)
  962       CALL EXTCDE ( 24 )
  963 !
  964   825 CONTINUE
  965       WRITE (NDSE,1025) MINGRD, NSEA
  966       CALL EXTCDE ( 25 )
  967 !
  968   830 CONTINUE
  969       WRITE (NDSE,1030)
  970       CALL EXTCDE ( 30 )
  971 !
  972   850 CONTINUE
  973       WRITE (NDSE,1050) G0ID
  974       CALL EXTCDE ( 50 )
  975 !
  976   860 CONTINUE
  977       WRITE (NDSE,1060) FNMPRE(:J)//FNAME(:J5), IERR
  978       CALL EXTCDE ( 60 )
  979 !
  980   870 CONTINUE
  981       WRITE (NDSE,1070) FNMPRE(:J)//INAME(:J5), IERR
  982       CALL EXTCDE ( 70 )
  983 !
  984   888 CONTINUE
  985       WRITE (NDSO,999)
  986 !
  987 ! Formats
  988 !
  989   900 FORMAT (/15X,'  *** WAVEWATCH III  Grid splitting ***  '/ &
  990                15X,'=========================================='/)
 
 
                                                         ww3_gspl.F90  page  19
 
 
  991   901 FORMAT ( '  Comment character is ''',A,''''/)
  992   902 FORMAT ( '  Grid ID   : ',A/                                    &
  993                '  Grid name : ',A)
  994   903 FORMAT ( '  Grid type : ',A)
  995   904 FORMAT ( '  Closure   : ',A)
  996   905 FORMAT ( '  Grid size : ',I4,' x',I4,'   (',I8,')'/)
  997 !
  998   930 FORMAT ( '  Generating ',I3,' grids'/                           &
  999                '  No more than',I4,' refinement iterations'/          &
 1000                '  Grid point count std target (%) :',F6.2/            &
 1001                '  Halo per sub grid extended by',I3,' grid point.')
 1002   931 FORMAT ( '  Format info for bottom file      :',2I2,F12.4,2X,A)
 1003   932 FORMAT ( '  Format info for obstruction file not used')
 1004   933 FORMAT ( '  Format info for obstruction file :',2I2,F12.4,2X,A)
 1005   934 FORMAT ( '  Format info for mask file        :',2I2,I7,7X,A)
 1006   935 FORMAT ( '  Part of cummunicator to be used  :',2F7.4)
 1007   936 FORMAT ( '  Not running grids side-by-side'/                    &
 1008                '     *** NON CONVENTIONAL OPERATION ***'/)
 1009 !
 1010   950 FORMAT (/'  Iterations:'/                                       &
 1011                '     nr     min     max    std (%) '/                 &
 1012                '  ---------------------------------')
 1013   951 FORMAT (2X,I5,2I8,2F10.2)
 1014   952 FORMAT (2X,5x,2I8,2F10.2)
 1015   955 FORMAT (/'  Resulting grids:'/                                  &
 1016                '     grid  stradle points  range X   range Y '/       &
 1017                '  ---------------------------------------------')
 1018   956 FORMAT ( '     ',I4,5X,L1,2X,I7,4I5)
 1019   959 FORMAT ( '  Convergence reached')
 1020 !
 1021   960 FORMAT (/'  Generating grid data:'/                             &
 1022                '  ---------------------------------------------')
 1023   961 FORMAT ( '     Extracting data for grid',I4)
 1024   962 FORMAT ( '        Writing file ',A)
 1025   963 FORMAT ( '        File ',A,' not requested')
 1026 !
 1027   970 FORMAT ( ' ''',A,''' ''LEV'' ''CUR'' ''WND'' ''ICE''',          &
 1028                ' ''D1'' ''D2'' ''D3'' RANK GROUP',2F10.7,' BFLAG')
 1029 !
 1030   980 FORMAT (1X,360I2)
 1031 !
 1032   965 FORMAT ( '$ -------------------------------------',             &
 1033                '------------------------------- $'/                   &
 1034                '$ WAVEWATCH III Grid preprocessor input',             &
 1035                ' file                           $'/                   &
 1036                '$ -------------------------------------',             &
 1037                '------------------------------- $'/                   &
 1038                '   ''',A,''''/'$'/                                    &
 1039                '   ',F8.4,F10.6,2I6,F8.4/'   ',6L2/'   ',4F12.4/      &
 1040                '$ NAMELISTS'/'$')
 1041   966 FORMAT ( '   ''',A4,'''  ',L1,'  ''',A4,''''/1X,I8,I12/         &
 1042                4X,2F12.6,'    1.0'/4X,2F12.6,'    1.0'/2F8.2,'  20',  &
 1043                F12.6,2I2,' ''',A,''' ''NAME'' ''',A,'''')
 1044   967 FORMAT ( 18X,'30',F12.6,2I2,' ''',A,''' ''NAME'' ''',A,'''' )
 1045   968 FORMAT ( 18X,'40',12X,2I2,' ''',A,''' ''NAME'' ''',A,''''/'$'/  &
 
 
                                                         ww3_gspl.F90  page  20
 
 
 1046                '$ Note: cannot make output boundary points here'/'$'/ &
 1047                ' 0. 0. 0. 0. 0'/                                      &
 1048                '$ -------------------------------------',             &
 1049                '------------------------------- $'/                   &
 1050                '$ End of input file                    ',             &
 1051                '                                $'/                   &
 1052                '$ -------------------------------------',             &
 1053                '------------------------------- $')
 1054 !
 1055   969 FORMAT (/'  Grid point inflation',F7.2,'%')
 1056 !
 1057   999 FORMAT(//'  End of program '/                                   &
 1058                ' ========================================='/          &
 1059                '             WAVEWATCH III Grid splitting '/)
 1060 !
 1061  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3GSPL : '/               &
 1062                '     ERROR IN OPENING INPUT FILE'/                    &
 1063                '     IOSTAT =',I5/)
 1064 !
 1065  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3GSPL : '/               &
 1066                '     PREMATURE END OF INPUT FILE'/)
 1067 !
 1068  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3GSPL : '/               &
 1069                '     ERROR IN READING FROM INPUT FILE'/               &
 1070                '     IOSTAT =',I5/)
 1071 !
 1072  1020 FORMAT (/' *** WAVEWATCH III ERROR IN W3GSPL : '/               &
 1073                '     SPLITTING NOT AVAILABLE FOR GRID TYPE'/          &
 1074                '     GTYPE =',I5/)
 1075 !
 1076  1021 FORMAT (/' *** WAVEWATCH III ERROR IN W3GSPL : '/               &
 1077                '     GRID TYPE NOT RECOGNIZED'/                       &
 1078                '     GTYPE =',I5/)
 1079 !
 1080  1022 FORMAT (/' *** WAVEWATCH III ERROR IN W3GSPL : '/               &
 1081                '     SPLITTING NOT AVAILABLE FOR CLOSURE TYPE'/       &
 1082                '     ICLOSE =',I5/)
 1083 !
 1084  1023 FORMAT (/' *** WAVEWATCH III ERROR IN W3GSPL : '/               &
 1085                '     CLOSURE TYPE NOT RECOGNIZED'/                    &
 1086                '     ICLOSE =',I5/)
 1087 !
 1088  1024 FORMAT (/' *** WAVEWATCH III ERROR IN W3GSPL : '/               &
 1089                '     NO ACTIVE SEA POINT IN GRID'/)
 1090 !
 1091  1025 FORMAT (/' *** WAVEWATCH III ERROR IN W3GSPL : '/               &
 1092                '     WRONG NUMBER OF SEA POINTS'/                     &
 1093                '     MINGRD, NSEA =',2I7/)
 1094 !
 1095  1030 FORMAT (/' *** WAVEWATCH III ERROR IN W3GSPL : '/               &
 1096                '     ILLEGAL PART OF COMMUNICATOR REQUESTED'/)
 1097 !
 1098  1050 FORMAT (/' *** WAVEWATCH III ERROR IN W3GSPL : '/               &
 1099                '     SHOULD NOT HAVE ZERO GRID SIZE (',A,') ...'/)
 1100 !
 
 
                                                         ww3_gspl.F90  page  21
 
 
 1101  1060 FORMAT (/' *** WAVEWATCH III ERROR IN W3GSPL : '/               &
 1102                '     ERROR IN OPENING FILE ',A/                       &
 1103                '     IOSTAT =',I5/)
 1104 !
 1105  1070 FORMAT (/' *** WAVEWATCH III ERROR IN W3GSPL : '/               &
 1106                '     ERROR IN OPENING FILE ',A/                       &
 1107                '     IOSTAT =',I5/)
 1108 !
 1109  9052 FORMAT ( 'TEST W3GSPL: STUCK ON ',A,' GRID SIZE')
 1110  9053 FORMAT ( '             OUT OF RANGE, PROCESSING (',F6.3,')')
 1111  9054 FORMAT ( '             IN RANGE, NO ACTION')
 1112 !/
 1113 !/ Embedded subroutines ---------------------------------------------- /
 1114 !/
 1115       CONTAINS
 1116 !/ ------------------------------------------------------------------- /
 1117       SUBROUTINE GRINFO
 1118 !/
 1119 !/                  +-----------------------------------+
 1120 !/                  | WAVEWATCH III           NOAA/NCEP |
 1121 !/                  |           H. L. Tolman            |
 1122 !/                  |                        FORTRAN 90 |
 1123 !/                  | Last update :         13-Sep-2012 |
 1124 !/                  +-----------------------------------+
 1125 !/
 1126 !/    06-Sep-2012 : Origination.                        ( version 4.10 )
 1127 !/    13-Sep-2012 : Option to exclude grids from stats. ( version 4.10 )
 1128 !/
 1129 !  1. Purpose :
 1130 !
 1131 !     Compile statistical info on all sub grids (no halo).
 1132 !
 1133 ! 10. Source code :
 1134 !
 1135 !/ ------------------------------------------------------------------- /
 1136 !/
 1137       IMPLICIT NONE
 1138 !/
 1139 !/ ------------------------------------------------------------------- /
 1140 !/ Parameter list
 1141 !/
 1142 !/ ------------------------------------------------------------------- /
 1143 !/ Local parameters
 1144 !/
 1145       INTEGER                 :: NOCNT, NOCNTM, NOCNTL, NGC, NSEAC
 1146       REAL                    :: SUMSQR
 1147       LOGICAL                 :: LEFT, RIGHT, THERE
 1148 !/
 1149 !/ ------------------------------------------------------------------- /
 1150 !/
 1151 !
 1152 ! 1.  Initialization ------------------------------------------------- *
 1153 !
 1154       GSTATS(:)%STRADLE = .FALSE.
 1155       GSTATS(:)%NPTS    = 0
 
 
                                                         ww3_gspl.F90  page  22
 
 
 1156       GSTATS(:)%NXL     = NX
 1157       GSTATS(:)%NXH     = 1
 1158       GSTATS(:)%NYL     = NY
 1159       GSTATS(:)%NYH     = 1
 1160 !
 1161 ! 2.  Get STRADLE, NGC ----------------------------------------------- *
 1162 !
 1163       NGC    = 0
 1164 !
 1165       DO IG=1, NG
 1166         LEFT   = .FALSE.
 1167         RIGHT  = .FALSE.
 1168         IF ( GSTATS(IG)%INSTAT ) NGC = NGC + 1
 1169         DO IY=1, NY
 1170           IF ( MSPLIT(IY, 1) .EQ. IG ) LEFT   = .TRUE.
 1171           IF ( MSPLIT(IY,NX) .EQ. IG ) RIGHT  = .TRUE.
 1172           END DO
 1173         GSTATS(IG)%STRADLE = LEFT .AND. RIGHT
 1174         END DO
 1175 !
 1176       IF ( NGC .EQ. 0 ) THEN
 1177           NGC    = 1
 1178           DONE   = .TRUE.
 1179         END IF
 1180 !
 1181 ! 3.  Run grid stats ------------------------------------------------- *
 1182 ! 3.a General
 1183 !
 1184       DO IY=1, NY
 1185         DO IX=1, NX
 1186           IG     = MSPLIT(IY,IX)
 1187           IF ( MSPLIT(IY,IX) .GT. 0 ) THEN
 1188               GSTATS(IG)%NPTS = GSTATS(IG)%NPTS + 1
 1189               GSTATS(IG)%NXL  = MIN ( GSTATS(IG)%NXL , IX )
 1190               GSTATS(IG)%NXH  = MAX ( GSTATS(IG)%NXH , IX )
 1191               GSTATS(IG)%NYL  = MIN ( GSTATS(IG)%NYL , IY )
 1192               GSTATS(IG)%NYH  = MAX ( GSTATS(IG)%NYH , IY )
 1193             END IF
 1194           END DO
 1195         END DO
 1196 !
 1197 ! 3.b Stradled grids
 1198 !
 1199       IF ( NG .GT. 1) THEN
 1200         DO IG=1, NG
 1201           IF ( GSTATS(IG)%STRADLE ) THEN
 1202               NOCNT  = 0
 1203               NOCNTM = 0
 1204               NOCNTL = 0
 1205               DO IX=1, NX
 1206                 THERE  = .FALSE.
 1207                 DO IY=1, NY
 1208                   IF ( MSPLIT(IY,IX) .EQ. IG ) THEN
 1209                       THERE  = .TRUE.
 1210                       EXIT
 
 
                                                         ww3_gspl.F90  page  23
 
 
 1211                     END IF
 1212                   END DO
 1213                 IF ( THERE ) THEN
 1214                     NOCNT = 0
 1215                   ELSE
 1216                     NOCNT = NOCNT + 1
 1217                     IF ( NOCNT .GT. NOCNTM ) THEN
 1218                         NOCNTM = NOCNT
 1219                         NOCNTL = IX
 1220                       END IF
 1221                   END IF
 1222                 END DO
 1223               GSTATS(IG)%NXL = NOCNTL + 1
 1224               GSTATS(IG)%NXH = NOCNTL - NOCNTM
 1225             END IF
 1226           END DO
 1227         ELSE
 1228           GSTATS(1)%STRADLE = .FALSE.
 1229         END IF
 1230 !
 1231 ! 3.c Corrected NSEA
 1232 !
 1233       NSEAC   = 0
 1234 !
 1235       DO IG=1, NG
 1236         IF ( GSTATS(IG)%INSTAT ) NSEAC = NSEAC + GSTATS(IG)%NPTS
 1237         END DO
 1238 !
 1239 ! 4.  Run overall stats ---------------------------------------------- *
 1240 !
 1241       MSTATS%NMIN = NSEA + 1
 1242       MSTATS%NMAX = 0
 1243       XMEAN  = REAL(NSEAC) / REAL(NGC)
 1244       SUMSQR = 0.
 1245 !
 1246       DO IG=1, NG
 1247         IF ( .NOT. GSTATS(IG)%INSTAT ) CYCLE
 1248         MSTATS%NMIN = MIN ( MSTATS%NMIN , GSTATS(IG)%NPTS )
 1249         MSTATS%NMAX = MAX ( MSTATS%NMAX , GSTATS(IG)%NPTS )
 1250         SUMSQR      = SUMSQR + ( REAL(GSTATS(IG)%NPTS) - XMEAN )**2
 1251         END DO
 1252 !
 1253       MSTATS%RSTD = SQRT ( SUMSQR / REAL(NGC) )
 1254 !
 1255 ! 5.  Test output ---------------------------------------------------- *
 1256 !
 1257       RETURN
 1258 !
 1259 ! Formats
 1260 !
 1261 !/ End of GRINFO ----------------------------------------------------- /
 1262 !/
 1263       END SUBROUTINE GRINFO
 1264 !/ ------------------------------------------------------------------- /
 1265       SUBROUTINE GRTRIM
 
 
                                                         ww3_gspl.F90  page  24
 
 
 1266 !/
 1267 !/                  +-----------------------------------+
 1268 !/                  | WAVEWATCH III           NOAA/NCEP |
 1269 !/                  |           H. L. Tolman            |
 1270 !/                  |                        FORTRAN 90 |
 1271 !/                  | Last update :         01-Feb-2013 |
 1272 !/                  +-----------------------------------+
 1273 !/
 1274 !/    07-Sep-2012 : Origination.                        ( version 4.10 )
 1275 !/    18-Sep-2012 : Include edge points of grid.        ( version 4.10 )
 1276 !/    01-Feb-2013 : Add dynamic trim range.             ( version 4.10 )
 1277 !/
 1278 !  1. Purpose :
 1279 !
 1280 !    Trim edges of all grids where they are next to another grid or next
 1281 !    to unassigned grid points. This is done in preparation for
 1282 !    reassigning edges of grids to smaller adjacent grids.
 1283 !
 1284 ! 10. Source code :
 1285 !
 1286 !/ ------------------------------------------------------------------- /
 1287 !/
 1288       IMPLICIT NONE
 1289 !/
 1290 !/ ------------------------------------------------------------------- /
 1291 !/ Parameter list
 1292 !/
 1293 !/ ------------------------------------------------------------------- /
 1294 !/ Local parameters
 1295 !/
 1296       INTEGER                 :: ITARG, ITL, IPTS, MX, MY, ICIRC, NWDTH
 1297       LOGICAL                 :: MASK(NY,NX)
 1298 !/
 1299 !/ ------------------------------------------------------------------- /
 1300 !/
 1301 !
 1302       ITARG  = NSEA / NG
 1303 !
 1304 ! 1.  Loop over grids ------------------------------------------------ *
 1305 !
 1306       DO IG=1, NG
 1307 !
 1308         IPTS   = GSTATS(IG)%NPTS
 1309         MY     = 1 + GSTATS(IG)%NYH - GSTATS(IG)%NYL
 1310         MX     = 1 + GSTATS(IG)%NXH - GSTATS(IG)%NXL
 1311         IF ( GSTATS(IG)%STRADLE ) MX     = MX + NX
 1312         ICIRC  = 2 * ( MX + MY )
 1313 !
 1314         NWDTH  = 1
 1315 !
 1316         ITL    = MIN ( ITARG , MAX ( ITARG-2*ICIRC , 3*ICIRC ) )
 1317         IF ( IPTS .LT. ITL ) NWDTH  = 0
 1318 !
 1319         IF ( IPTS.GT.ITARG ) THEN
 1320             NWDTH = 1 +                                              &
 
 
                                                         ww3_gspl.F90  page  25
 
 
 1321                    MAX(0,+NINT((REAL((IPTS-ITARG))/REAL(ICIRC)-1.)/3.))
 1322           ENDIF
 1323 !
 1324         DO J=1, NWDTH
 1325 !
 1326           MASK   = .FALSE.
 1327 !
 1328 ! 2.  Mark points to be removed -------------------------------------- *
 1329 !
 1330           DO IX=2, NX-1
 1331             IF ( MSPLIT( 1,IX) .EQ. IG ) MASK( 1,IX) =                &
 1332                      (SEA( 2,IX  ).AND.(MSPLIT( 2,IX  ).NE.IG))       &
 1333                 .OR. (SEA( 1,IX+1).AND.(MSPLIT( 1,IX+1).NE.IG))       &
 1334                 .OR. (SEA( 1,IX-1).AND.(MSPLIT( 1,IX-1).NE.IG))
 1335             DO IY=2, NY-1
 1336               IF ( MSPLIT(IY,IX) .EQ. IG ) MASK(IY,IX) =              &
 1337                        (SEA(IY+1,IX  ).AND.(MSPLIT(IY+1,IX  ).NE.IG)) &
 1338                   .OR. (SEA(IY-1,IX  ).AND.(MSPLIT(IY-1,IX  ).NE.IG)) &
 1339                   .OR. (SEA(IY  ,IX+1).AND.(MSPLIT(IY  ,IX+1).NE.IG)) &
 1340                   .OR. (SEA(IY  ,IX-1).AND.(MSPLIT(IY  ,IX-1).NE.IG))
 1341               END DO
 1342             IF ( MSPLIT(NY,IX) .EQ. IG ) MASK(NY,IX) =                &
 1343                      (SEA(NY-1,IX  ).AND.(MSPLIT(NY-1,IX  ).NE.IG))   &
 1344                 .OR. (SEA(NY  ,IX+1).AND.(MSPLIT(NY  ,IX+1).NE.IG))   &
 1345                 .OR. (SEA(NY  ,IX-1).AND.(MSPLIT(NY  ,IX-1).NE.IG))
 1346             END DO
 1347 !
 1348           IF ( GLOBAL ) THEN
 1349               IF ( MSPLIT( 1, 1) .EQ. IG ) MASK( 1, 1) =              &
 1350                        (SEA( 2, 1).AND.(MSPLIT( 2, 1).NE.IG))         &
 1351                   .OR. (SEA( 1, 2).AND.(MSPLIT( 1, 2).NE.IG))         &
 1352                   .OR. (SEA( 1,NX).AND.(MSPLIT( 1,NX).NE.IG))
 1353               IF ( MSPLIT( 1,NX) .EQ. IG ) MASK( 1,NX) =              &
 1354                        (SEA( 2,NX  ).AND.(MSPLIT( 2,NX  ).NE.IG))     &
 1355                   .OR. (SEA( 1, 1  ).AND.(MSPLIT( 1, 1  ).NE.IG))     &
 1356                   .OR. (SEA( 1,NX-1).AND.(MSPLIT( 1,NX-1).NE.IG))
 1357               DO IY=2, NY-1
 1358                 IF ( MSPLIT(IY, 1) .EQ. IG ) MASK(IY, 1) =            &
 1359                          (SEA(IY+1, 1).AND.(MSPLIT(IY+1, 1).NE.IG))   &
 1360                     .OR. (SEA(IY-1, 1).AND.(MSPLIT(IY-1, 1).NE.IG))   &
 1361                     .OR. (SEA(IY  , 2).AND.(MSPLIT(IY  , 2).NE.IG))   &
 1362                     .OR. (SEA(IY  ,NX).AND.(MSPLIT(IY  ,NX).NE.IG))
 1363                 IF ( MSPLIT(IY,NX) .EQ. IG ) MASK(IY,NX) =            &
 1364                          (SEA(IY+1,NX).AND.(MSPLIT(IY+1,NX).NE.IG))   &
 1365                     .OR. (SEA(IY-1,NX).AND.(MSPLIT(IY-1,NX).NE.IG))   &
 1366                     .OR. (SEA(IY  , 1).AND.(MSPLIT(IY  , 1).NE.IG))   &
 1367                     .OR. (SEA(IY,NX-1).AND.(MSPLIT(IY,NX-1).NE.IG))
 1368                 END DO
 1369               IF ( MSPLIT(NY, 1) .EQ. IG ) MASK(NY, 1) =              &
 1370                        (SEA(NY-1, 1).AND.(MSPLIT(NY-1, 1).NE.IG))     &
 1371                   .OR. (SEA(NY  , 2).AND.(MSPLIT(NY  , 2).NE.IG))     &
 1372                   .OR. (SEA(NY  ,NX).AND.(MSPLIT(NY  ,NX).NE.IG))
 1373               IF ( MSPLIT(NY,NX) .EQ. IG ) MASK(NY,NX) =              &
 1374                        (SEA(NY-1,NX).AND.(MSPLIT(NY-1,NX).NE.IG))     &
 1375                   .OR. (SEA(NY  , 1).AND.(MSPLIT(NY  , 1).NE.IG))     &
 
 
                                                         ww3_gspl.F90  page  26
 
 
 1376                   .OR. (SEA(NY,NX-1).AND.(MSPLIT(NY,NX-1).NE.IG))
 1377           ELSE
 1378               IF ( MSPLIT( 1, 1) .EQ. IG ) MASK( 1, 1) =              &
 1379                        (SEA( 2, 1).AND.(MSPLIT( 2, 1).NE.IG))         &
 1380                   .OR. (SEA( 1, 2).AND.(MSPLIT( 1, 2).NE.IG))
 1381               IF ( MSPLIT( 1,NX) .EQ. IG ) MASK( 1,NX) =              &
 1382                        (SEA( 2,NX  ).AND.(MSPLIT( 2,NX  ).NE.IG))     &
 1383                   .OR. (SEA( 1,NX-1).AND.(MSPLIT( 1,NX-1).NE.IG))
 1384               DO IY=2, NY-1
 1385                 IF ( MSPLIT(IY, 1) .EQ. IG ) MASK(IY, 1) =            &
 1386                          (SEA(IY+1, 1).AND.(MSPLIT(IY+1, 1).NE.IG))   &
 1387                     .OR. (SEA(IY-1, 1).AND.(MSPLIT(IY-1, 1).NE.IG))   &
 1388                     .OR. (SEA(IY  , 2).AND.(MSPLIT(IY  , 2).NE.IG))
 1389                 IF ( MSPLIT(IY,NX) .EQ. IG ) MASK(IY,NX) =            &
 1390                          (SEA(IY+1,NX).AND.(MSPLIT(IY+1,NX).NE.IG))   &
 1391                     .OR. (SEA(IY-1,NX).AND.(MSPLIT(IY-1,NX).NE.IG))   &
 1392                     .OR. (SEA(IY,NX-1).AND.(MSPLIT(IY,NX-1).NE.IG))
 1393                 END DO
 1394               IF ( MSPLIT(NY, 1) .EQ. IG ) MASK(NY, 1) =              &
 1395                        (SEA(NY-1, 1).AND.(MSPLIT(NY-1, 1).NE.IG))     &
 1396                   .OR. (SEA(NY  , 2).AND.(MSPLIT(NY  , 2).NE.IG))
 1397               IF ( MSPLIT(NY,NX) .EQ. IG ) MASK(NY,NX) =              &
 1398                        (SEA(NY-1,NX).AND.(MSPLIT(NY-1,NX).NE.IG))     &
 1399                   .OR. (SEA(NY,NX-1).AND.(MSPLIT(NY,NX-1).NE.IG))
 1400             END IF
 1401 !
 1402 ! 3.  Remove marked points ------------------------------------------- *
 1403 !
 1404           DO IX=1, NX
 1405             DO IY=1, NY
 1406               IF ( MASK(IY,IX) ) THEN
 1407                   MSPLIT(IY,IX) = -1
 1408                 END IF
 1409               END DO
 1410             END DO
 1411 !
 1412 ! ... End loops started in 1.
 1413 !
 1414           END DO
 1415         END DO
 1416 !
 1417       RETURN
 1418 !
 1419 ! Formats
 1420 !
 1421 !/ End of GRTRIM ----------------------------------------------------- /
 1422 !/
 1423       END SUBROUTINE GRTRIM
 1424 !/ ------------------------------------------------------------------- /
 1425       SUBROUTINE GRFILL ( ND )
 1426 !/
 1427 !/                  +-----------------------------------+
 1428 !/                  | WAVEWATCH III           NOAA/NCEP |
 1429 !/                  |           H. L. Tolman            |
 1430 !/                  |                        FORTRAN 90 |
 
 
                                                         ww3_gspl.F90  page  27
 
 
 1431 !/                  | Last update :         01-Feb-2013 |
 1432 !/                  +-----------------------------------+
 1433 !/
 1434 !/    07-Sep-2012 : Origination.                        ( version 4.10 )
 1435 !/    18-Sep-2012 : Include edge points of grid.        ( version 4.10 )
 1436 !/                  Add convergence check.
 1437 !/    29-Jan-2013 : Add error code on stop.             ( version 4.10 )
 1438 !/    29-Jan-2013 : Add error test output.              ( version 4.10 )
 1439 !/    01-Feb-2013 : Loop over selected sea points only. ( version 4.10 )
 1440 !/
 1441 !  1. Purpose :
 1442 !
 1443 !     Reassign unassigned grid points to grids, starting with the
 1444 !     smallest grids.
 1445 !
 1446 !  3. Parameters :
 1447 !
 1448 !     Parameter list
 1449 !     ----------------------------------------------------------------
 1450 !       ND      Int.   I   Depth of halo for first sweep.
 1451 !     ----------------------------------------------------------------
 1452 !
 1453 ! 10. Source code :
 1454 !
 1455 !/ ------------------------------------------------------------------- /
 1456 !/
 1457       IMPLICIT NONE
 1458 !/
 1459 !/ ------------------------------------------------------------------- /
 1460 !/ Parameter list
 1461 !/
 1462       INTEGER, INTENT(IN)     :: ND
 1463 !/
 1464 !/ ------------------------------------------------------------------- /
 1465 !/ Local parameters
 1466 !/
 1467       INTEGER                 :: NMIN, I, NDEPTH, NITT, NADD, IXL, IXR,&
 1468                                  NLEFT, NRIGHT, NXL, NXH, NYL, NYH
 1469       INTEGER                 :: NXYOFF = 3
 1470       INTEGER                 :: IIX(NSEA), IIY(NSEA), ISEA, NSEAL
 1471       LOGICAL                 :: DONE(NG), MASK(NY,NX), FLOST(NG),     &
 1472                                  XFL(NX), YFL(NY)
 1473 !/
 1474 !/ ------------------------------------------------------------------- /
 1475 !/
 1476 !
 1477 ! 1.  Loop to assure all reassigned ---------------------------------- *
 1478 !
 1479       NDEPTH = ND
 1480       NITT   = 0
 1481       NLEFT  = -1
 1482       FLOST  = .FALSE.
 1483 !
 1484       NSEAL  = 0
 1485       DO IX=1, NX
 
 
                                                         ww3_gspl.F90  page  28
 
 
 1486         DO IY=1, NY
 1487           IF ( MSPLIT(IY,IX) .EQ. -1 ) THEN
 1488               NSEAL  = NSEAL + 1
 1489               IIX(NSEAL) = IX
 1490               IIY(NSEAL) = IY
 1491             END IF
 1492           END DO
 1493         END DO
 1494 !
 1495       DO
 1496         NITT   = NITT + 1
 1497 !
 1498 ! 2.  Loop over all grids -------------------------------------------- *
 1499 !
 1500         DONE   = .FALSE.
 1501 !
 1502         DO J=1, NG
 1503 !
 1504 ! 3.  Find smallest unprocessed grid --------------------------------- *
 1505 !
 1506           NMIN   = NSEA + 1
 1507           IG     = 0
 1508 !
 1509           DO I=1, NG
 1510             IF ( .NOT.DONE(I) .AND. GSTATS(I)%NPTS.LT.NMIN ) THEN
 1511                 IG     = I
 1512                 NMIN   = GSTATS(I)%NPTS
 1513               END IF
 1514             END DO
 1515 !
 1516           DONE(IG) = .TRUE.
 1517 !
 1518 ! 4.  Loop for halos per grid ---------------------------------------- *
 1519 !
 1520           DO, I=1, NDEPTH
 1521 !
 1522             MASK   = .FALSE.
 1523 !
 1524 ! 5.  Mark grid point for adding ------------------------------------- *
 1525 !
 1526             DO ISEA=1, NSEAL
 1527               IX     = IIX(ISEA)
 1528               IY     = IIY(ISEA)
 1529               IXL    = 1 + MOD(IX-2+NX,NX)
 1530               IXR    = 1 + MOD(IX,NX)
 1531               IF ( MSPLIT(IY,IX) .EQ. -1 ) MASK(IY,IX) =              &
 1532                                       ( MSPLIT(IY+1,IX ) .EQ. IG )    &
 1533                                 .OR.  ( MSPLIT(IY-1,IX ) .EQ. IG )    &
 1534                                 .OR.  ( MSPLIT(IY  ,IXR) .EQ. IG )    &
 1535                                 .OR.  ( MSPLIT(IY  ,IXL) .EQ. IG )
 1536               END DO
 1537 !
 1538 ! 6.  Add marked grid point ------------------------------------------ *
 1539 !
 1540             NADD   = 0
 
 
                                                         ww3_gspl.F90  page  29
 
 
 1541 !
 1542             DO ISEA=1, NSEAL
 1543               IX     = IIX(ISEA)
 1544               IY     = IIY(ISEA)
 1545               IF ( MASK(IY,IX) ) THEN
 1546                   MSPLIT(IY,IX) = IG
 1547                   NADD          = NADD + 1
 1548                 END IF
 1549               END DO
 1550 !
 1551             IF ( NADD .EQ. 0 ) EXIT
 1552 !
 1553 ! ... End loop started in 4.
 1554 !
 1555             END DO
 1556 !
 1557 ! ... End loop started in 2.
 1558 !
 1559           END DO
 1560 !
 1561         NDEPTH = 1
 1562 !
 1563 ! 7.  Check convergence ---------------------------------------------- *
 1564 ! 7.a Find number of points left
 1565 !
 1566         NRIGHT = NLEFT
 1567         NLEFT  = 0
 1568 !
 1569         DO ISEA=1, NSEAL
 1570           IX     = IIX(ISEA)
 1571           IY     = IIY(ISEA)
 1572           IF ( MSPLIT(IY,IX) .EQ. -1 ) NLEFT = NLEFT + 1
 1573           END DO
 1574 !
 1575 ! 7.b No point left, exit loop
 1576 !
 1577         IF ( NLEFT .EQ. 0 ) EXIT
 1578 !
 1579 ! 7.c Stuck with points left
 1580 !
 1581         IF ( NRIGHT .GT. 0 ) THEN
 1582             IF ( NLEFT .EQ. NRIGHT ) THEN
 1583 !
 1584 ! 7.d Do lost point correction once
 1585 !
 1586                 IF ( .NOT. FLOST(IG) ) THEN
 1587                     CALL GRLOST
 1588                     FLOST(IG) = .TRUE.
 1589                   ELSE
 1590 !
 1591 ! 7.e Got stuck for good, error message and ouput
 1592 !
 1593                     WRITE (NDSE,1000) IG, NITT, NLEFT
 1594 !
 1595                     XFL    = .FALSE.
 
 
                                                         ww3_gspl.F90  page  30
 
 
 1596                     YFL    = .FALSE.
 1597 !
 1598                     DO ISEA=1, NSEAL
 1599                       IX     = IIX(ISEA)
 1600                       IY     = IIY(ISEA)
 1601                       IF ( MSPLIT(IY,IX) .EQ. -1 ) THEN
 1602                           XFL(MAX(1,IX-NXYOFF):MIN(NX,IX+NXYOFF)) = .TRUE.
 1603                           YFL(MAX(1,IY-NXYOFF):MIN(NY,IY+NXYOFF)) = .TRUE.
 1604                           END IF
 1605                       END DO
 1606 !
 1607                     NXL   = 0
 1608                     NXH   = 0
 1609                     DO IX=1, NX
 1610                       IF ( XFL(IX) .AND. NXL.EQ. 0 ) NXL = IX
 1611                       IF ( XFL(IX) .AND. IX.EQ. NX ) NXH = IX
 1612                       IF ( .NOT. XFL(IX) .AND. NXL.NE. 0 ) NXH = IX-1
 1613                       IF ( NXH .NE. 0 ) THEN
 1614                           NYL   = 0
 1615                           NYH   = 0
 1616                           DO IY=1, NY
 1617                             IF ( YFL(IY) .AND. NYL.EQ. 0 ) NYL = IY
 1618                             IF ( YFL(IY) .AND. IY.EQ. NY ) NYH = IY
 1619                             IF ( .NOT. YFL(IY) .AND. NYL.NE. 0 )      &
 1620                                                             NYH = IY-1
 1621                             IF ( NYH .NE. 0 ) THEN
 1622                                 WRITE (NDST,1001) NXL, NXH, NYH, NYL
 1623                                 DO I=NYH, NYL, -1
 1624                               WRITE (NDST,1002) MSPLIT(I,NXL:NXH)
 1625                                   END DO
 1626                                 NYL    = 0
 1627                                 NYH    = 0
 1628                               END IF
 1629                             END DO
 1630                           NXL   = 0
 1631                           NXH   = 0
 1632                         END IF
 1633                       END DO
 1634 !
 1635 ! ... Stop program with error output ...
 1636 !
 1637                     STOP 01
 1638                   ENDIF
 1639 !
 1640               END IF
 1641           END IF
 1642 !
 1643 ! ... End loop started in 1.
 1644 !
 1645         END DO
 1646 !
 1647       RETURN
 1648 !
 1649 ! Formats
 1650 !
 
 
                                                         ww3_gspl.F90  page  31
 
 
 1651  1000 FORMAT (/' *** ERROR GRFILL : NO MORE CONVERGENCE, ',           &
 1652                'NITT, NLEFT:',2I8,' ***'/)
 1653  1001 FORMAT ( ' MAP OUTPUT FOR GRID',I3,' AND X AND Y RANGE :',4I6/)
 1654  1002 FORMAT ( ' ',60I2)
 1655 !
 1656 !/ End of GRFILL ----------------------------------------------------- /
 1657 !/
 1658       END SUBROUTINE GRFILL
 1659 !/ ------------------------------------------------------------------- /
 1660       SUBROUTINE GRLOST
 1661 !/
 1662 !/                  +-----------------------------------+
 1663 !/                  | WAVEWATCH III           NOAA/NCEP |
 1664 !/                  |           H. L. Tolman            |
 1665 !/                  |                        FORTRAN 90 |
 1666 !/                  | Last update :         .9-Jan-2013 |
 1667 !/                  +-----------------------------------+
 1668 !/
 1669 !/    31-Jan-2013 : Origination.                        ( version 4.10 )
 1670 !/
 1671 !  1. Purpose :
 1672 !
 1673 !     Reassign unassigned grid points to gridsR. Dealing with lost
 1674 !     point by finding clostst grids.
 1675 !
 1676 ! 10. Source code :
 1677 !
 1678 !/ ------------------------------------------------------------------- /
 1679 !/
 1680       IMPLICIT NONE
 1681 !/
 1682 !/ ------------------------------------------------------------------- /
 1683 !/ Local parameters
 1684 !/
 1685       INTEGER                 :: IX, IY, IOFF, JJX, JX, JY, IG, I
 1686       INTEGER                 :: IFOUND(-1:NG)
 1687 !/
 1688 !/ ------------------------------------------------------------------- /
 1689 !/
 1690 !
 1691 ! 1.  Loop over all grid points -------------------------------------- *
 1692 !
 1693       DO IX=1, NX
 1694         DO IY=1, NY
 1695 !
 1696           IF ( MSPLIT(IY,IX) .EQ. -1 ) THEN
 1697 !
 1698 ! 2.  Find nearest grid(s) ------------------------------------------- *
 1699 !
 1700               IOFF   = 1
 1701 !
 1702               DO
 1703 !
 1704                 IFOUND = 0
 1705                 DO JJX=IX-IOFF, IX+IOFF
 
 
                                                         ww3_gspl.F90  page  32
 
 
 1706                   IF ( GLOBAL ) THEN
 1707                       JX     = 1 + MOD(JJX-1+2*NX,NX)
 1708                     ELSE
 1709                       JX     = JJX
 1710                     END IF
 1711                    IF ( JX.LT.1 .OR. JX.GT.NX ) CYCLE
 1712                    DO JY=IY-IOFF, IY+IOFF
 1713                      IF ( JY.LT.1 .OR. JY.GT.NY ) CYCLE
 1714                      IFOUND(MSPLIT(JY,JX)) = IFOUND(MSPLIT(JY,JX)) + 1
 1715                     END DO
 1716                   END DO
 1717 !
 1718                 IG     = 0
 1719                 DO I=1, NG
 1720                   IF ( IFOUND(I) .GT. 0 ) THEN
 1721                       IG     = I
 1722                       EXIT
 1723                     END IF
 1724                   END DO
 1725 !
 1726                 IF ( IG .NE. 0 ) THEN
 1727                     MSPLIT(IY,IX) = IG
 1728                     EXIT
 1729                   END IF
 1730 !
 1731                 IOFF   = IOFF + 1
 1732                 IF ( IOFF .GT. NX .AND. IOFF.GT.NY ) EXIT
 1733                 END DO
 1734 !
 1735 ! ... End of loops and logic started in 1.
 1736 !
 1737             END IF
 1738 !
 1739           END DO
 1740         END DO
 1741 !
 1742       RETURN
 1743 !
 1744 ! Formats
 1745 !
 1746 !/ End of GRLOST ----------------------------------------------------- /
 1747 !/
 1748       END SUBROUTINE GRLOST
 1749 !/ ------------------------------------------------------------------- /
 1750       SUBROUTINE GRSQRG
 1751 !/
 1752 !/                  +-----------------------------------+
 1753 !/                  | WAVEWATCH III           NOAA/NCEP |
 1754 !/                  |           H. L. Tolman            |
 1755 !/                  |                        FORTRAN 90 |
 1756 !/                  | Last update :         07-Sep-2012 |
 1757 !/                  +-----------------------------------+
 1758 !/
 1759 !/    07-Sep-2012 : Origination.                        ( version 4.10 )
 1760 !/
 
 
                                                         ww3_gspl.F90  page  33
 
 
 1761 !  1. Purpose :
 1762 !
 1763 !     Attemp to square-up grid by taking off grid point in outermost
 1764 !     grid point in X and Y only, after which GRFILL is to be run to
 1765 !     re-assign grid points,
 1766 !
 1767 ! 10. Source code :
 1768 !
 1769 !/ ------------------------------------------------------------------- /
 1770 !/
 1771       IMPLICIT NONE
 1772 !/
 1773 !/ ------------------------------------------------------------------- /
 1774 !/ Parameter list
 1775 !/
 1776 !/ ------------------------------------------------------------------- /
 1777 !/ Local parameters
 1778 !/
 1779       INTEGER                 :: MX, MY
 1780 !/
 1781 !/ ------------------------------------------------------------------- /
 1782 !/
 1783 !
 1784 ! 1.  Loop over grids ------------------------------------------------ *
 1785 !
 1786       DO IG=1, NG
 1787 !
 1788         MY     = 1 + GSTATS(IG)%NYH - GSTATS(IG)%NYL
 1789         MX     = 1 + GSTATS(IG)%NXH - GSTATS(IG)%NXL
 1790         IF ( GSTATS(IG)%STRADLE ) MX     = MX + NX
 1791 !
 1792 ! 2.  Top ------------------------------------------------------------ *
 1793 !
 1794         IF ( MY .GE. 5 ) THEN
 1795 !
 1796             DO IX=1, NX
 1797               IF (MSPLIT(GSTATS(IG)%NYH,IX) .EQ. IG )                 &
 1798                   MSPLIT(GSTATS(IG)%NYH,IX) = -1
 1799               END DO
 1800 !
 1801 ! 3.  Bottom --------------------------------------------------------- *
 1802 !
 1803             DO IX=1, NX
 1804               IF (MSPLIT(GSTATS(IG)%NYL,IX) .EQ. IG )                 &
 1805                   MSPLIT(GSTATS(IG)%NYL,IX) = -1
 1806               END DO
 1807 !
 1808           END IF
 1809 !
 1810 ! 4.  Left ----------------------------------------------------------- *
 1811 !
 1812         IF ( MX .GE. 5 ) THEN
 1813 !
 1814             DO IY=GSTATS(IG)%NYL, GSTATS(IG)%NYH
 1815               IF (MSPLIT(IY,GSTATS(IG)%NXL) .EQ. IG )                 &
 
 
                                                         ww3_gspl.F90  page  34
 
 
 1816                   MSPLIT(IY,GSTATS(IG)%NXL) = -1
 1817               END DO
 1818 !
 1819 ! 5.  Right ---------------------------------------------------------- *
 1820 !
 1821             DO IY=GSTATS(IG)%NYH, GSTATS(IG)%NYH
 1822               IF (MSPLIT(IY,GSTATS(IG)%NXH) .EQ. IG )                 &
 1823                   MSPLIT(IY,GSTATS(IG)%NXH) = -1
 1824               END DO
 1825 !
 1826           END IF
 1827 !
 1828 ! ... End loop started in 1.
 1829 !
 1830         END DO
 1831 !
 1832       RETURN
 1833 !
 1834 ! Formats
 1835 !
 1836 !/ End of GRSQRG ----------------------------------------------------- /
 1837 !/
 1838       END SUBROUTINE GRSQRG
 1839 !/ ------------------------------------------------------------------- /
 1840       SUBROUTINE GRSNGL ( OK )
 1841 !/
 1842 !/                  +-----------------------------------+
 1843 !/                  | WAVEWATCH III           NOAA/NCEP |
 1844 !/                  |           H. L. Tolman            |
 1845 !/                  |                        FORTRAN 90 |
 1846 !/                  | Last update :         09-Sep-2012 |
 1847 !/                  +-----------------------------------+
 1848 !/
 1849 !/    09-Sep-2012 : Origination.                        ( version 4.10 )
 1850 !/
 1851 !  1. Purpose :
 1852 !
 1853 !     Remove points from a grid that are in the middle of the sea, but
 1854 !     that have omly one adjacent point in the same grid. Directly
 1855 !     select a new grid for this point rather than deactivate and use
 1856 !     GRFILL.
 1857 !
 1858 !  3. Parameters :
 1859 !
 1860 !     Parameter list
 1861 !     ----------------------------------------------------------------
 1862 !       OK      Log.  I/O  Flag for grid status, .F. if values of
 1863 !                          -1 are left in MSPLIT.
 1864 !     ----------------------------------------------------------------
 1865 !
 1866 ! 10. Source code :
 1867 !
 1868 !/ ------------------------------------------------------------------- /
 1869 !/
 1870       IMPLICIT NONE
 
 
                                                         ww3_gspl.F90  page  35
 
 
 1871 !/
 1872 !/ ------------------------------------------------------------------- /
 1873 !/ Parameter list
 1874 !/
 1875       LOGICAL, INTENT(INOUT)  :: OK
 1876 !/
 1877 !/ ------------------------------------------------------------------- /
 1878 !/ Local parameters
 1879 !/
 1880       INTEGER                 :: NX0, NXN, IXL, IXH, COUNT(-1:NG),    &
 1881                                  INEW1, INEW2, INEW
 1882 !/
 1883 !/ ------------------------------------------------------------------- /
 1884 !/
 1885 !
 1886 ! 1.  Set up looping ------------------------------------------------- *
 1887 !
 1888       IF ( GLOBAL ) THEN
 1889           NX0    =  1
 1890           NXN    = NX
 1891         ELSE
 1892           NX0    =  2
 1893           NXN    = NX-1
 1894         END IF
 1895 !
 1896 ! 2.  Loops over 2D grid --------------------------------------------- *
 1897 !
 1898       DO IX=NX0, NXN
 1899 !
 1900         IXL    = IX - 1
 1901         IXH    = IX + 1
 1902         IF ( IX .EQ.  1 ) IXL = NX
 1903         IF ( IX .EQ. NX ) IXH =  1
 1904 !
 1905         DO IY=2, NY-1
 1906 !
 1907 ! 3.  Central sea points only ---------------------------------------- *
 1908 !
 1909           IF ( SEA(IY,IX) .AND. SEA(IY-1,IX ) .AND. SEA(IY+1,IX )      &
 1910                           .AND. SEA(IY  ,IXL) .AND. SEA(IY  ,IXH) ) THEN
 1911 !
 1912 ! 4.  Check for 'lost points' ---------------------------------------- *
 1913 !
 1914               COUNT  = 0
 1915               IG     = MSPLIT(IY,IX)
 1916 !
 1917               COUNT(IG) = 1
 1918               COUNT(MSPLIT(IY-1,IX )) = COUNT(MSPLIT(IY-1,IX )) + 1
 1919               COUNT(MSPLIT(IY+1,IX )) = COUNT(MSPLIT(IY+1,IX )) + 1
 1920               COUNT(MSPLIT(IY  ,IXL)) = COUNT(MSPLIT(IY  ,IXL)) + 1
 1921               COUNT(MSPLIT(IY  ,IXH)) = COUNT(MSPLIT(IY  ,IXH)) + 1
 1922 !
 1923               IF ( COUNT(IG) .LE. 2 ) THEN
 1924 !
 1925                   INEW1  = -1
 
 
                                                         ww3_gspl.F90  page  36
 
 
 1926                   INEW2  = -1
 1927 !
 1928                   DO J=1, NG
 1929                     IF ( COUNT(J) .GE. 2 ) THEN
 1930                         IF ( INEW1 .EQ. -1 ) THEN
 1931                             INEW1  = J
 1932                           ELSE
 1933                             INEW2  = J
 1934                             EXIT
 1935                           END IF
 1936                       END IF
 1937                     END DO
 1938 !
 1939                  IF ( INEW1 .EQ. -1 ) THEN
 1940                      INEW   = -1
 1941                      OK     = .FALSE.
 1942                    ELSE IF ( INEW2 .EQ. -1 ) THEN
 1943                      INEW   = INEW1
 1944                    ELSE
 1945                      IF ( GSTATS(INEW1)%NPTS .GT.                     &
 1946                           GSTATS(INEW2)%NPTS ) THEN
 1947                          INEW   = INEW2
 1948                        ELSE
 1949                          INEW   = INEW1
 1950                        END IF
 1951                    END IF
 1952 !
 1953                 MSPLIT(IY,IX) = INEW
 1954 !
 1955                 END IF
 1956 !
 1957             END IF
 1958 !
 1959 ! ... End loops started in 2.
 1960 !
 1961           END DO
 1962 !
 1963         END DO
 1964 !
 1965       RETURN
 1966 !
 1967 ! Formats
 1968 !
 1969 !/ End of GRSNGL ----------------------------------------------------- /
 1970 !/
 1971       END SUBROUTINE GRSNGL
 1972 !/ ------------------------------------------------------------------- /
 1973       SUBROUTINE GRSEPA ( OK, FRAC )
 1974 !/
 1975 !/                  +-----------------------------------+
 1976 !/                  | WAVEWATCH III           NOAA/NCEP |
 1977 !/                  |           H. L. Tolman            |
 1978 !/                  |                        FORTRAN 90 |
 1979 !/                  | Last update :         01-Feb-2013 |
 1980 !/                  +-----------------------------------+
 
 
                                                         ww3_gspl.F90  page  37
 
 
 1981 !/
 1982 !/    10-Sep-2012 : Origination.                        ( version 4.10 )
 1983 !/    18-Sep-2012 : Include edge points of grid.        ( version 4.10 )
 1984 !/    01-Feb-2013 : Much faster algorithms.             ( version 4.10 )
 1985 !/
 1986 !  1. Purpose :
 1987 !
 1988 !     Remove smller parts of a grid that are separated from the main
 1989 !     body, and that can be attached to other grids.
 1990 !
 1991 !  3. Parameters :
 1992 !
 1993 !     Parameter list
 1994 !     ----------------------------------------------------------------
 1995 !       OK      Log.  I/O  Flag for grid status, .F. if values of
 1996 !                          -1 are left in MSPLIT.
 1997 !       FRAC    Real   I   Fraction of average size used to remove grid
 1998 !                          part.
 1999 !     ----------------------------------------------------------------
 2000 !
 2001 ! 10. Source code :
 2002 !
 2003 !/ ------------------------------------------------------------------- /
 2004 !/
 2005       IMPLICIT NONE
 2006 !/
 2007 !/ ------------------------------------------------------------------- /
 2008 !/ Parameter list
 2009 !/
 2010       REAL, INTENT(IN)        :: FRAC
 2011       LOGICAL, INTENT(INOUT)  :: OK
 2012 !/
 2013 !/ ------------------------------------------------------------------- /
 2014 !/ Local parameters
 2015 !/
 2016       INTEGER                 :: IPAVG, IPCHCK, ID, IPTOT, IX, IY,    &
 2017                                  IXL, IYL, IDL, JX, JY, KY, IPT,      &
 2018                                  IXH, IYH, I, J, K, L, IMIN, LMIN
 2019       INTEGER                 :: GMASK(NY,NX), IIX(NSEA), IIY(NSEA)
 2020       INTEGER, ALLOCATABLE    :: PMAP(:), INGRD(:)
 2021       LOGICAL                 :: PREV
 2022       LOGICAL,ALLOCATABLE     :: FLNEXT(:), NEXTTO(:,:)
 2023 !/
 2024 !/ ------------------------------------------------------------------- /
 2025 !/
 2026 !
 2027       IPAVG  = NINT ( REAL(NSEA) / REAL(NG) )
 2028       IPCHCK = NINT ( FRAC * REAL(NSEA) / REAL(NG) )
 2029 !
 2030 ! 1.  Loop over grids ------------------------------------------------ *
 2031 !
 2032       DO IG=1, NG
 2033 !
 2034         GMASK  = 0
 2035         ID     = 0
 
 
                                                         ww3_gspl.F90  page  38
 
 
 2036 !
 2037 ! 2.  Find all parts ------------------------------------------------- *
 2038 ! 2.a First loop, partial parts
 2039 !
 2040         IPTOT  = 0
 2041 !
 2042         DO IX=1, NX
 2043 !
 2044           IXL    = 1 + MOD(IX-2+NX,NX)
 2045           PREV   = .FALSE.
 2046 !
 2047           DO IY=1, NY
 2048             IF (MSPLIT(IY,IX) .EQ. IG ) THEN
 2049                 IPTOT  = IPTOT + 1
 2050                 IIX(IPTOT) = IX
 2051                 IIY(IPTOT) = IY
 2052                 IF ( .NOT. PREV) THEN
 2053                     ID     = ID + 1
 2054                     PREV   = .TRUE.
 2055                   END IF
 2056                 GMASK(IY,IX) = ID
 2057               ELSE IF ( PREV ) THEN
 2058                 PREV   = .FALSE.
 2059                 IDL    = 0
 2060                 DO JY=IY-1, 1, -1
 2061                   IF ( GMASK(JY,IX) .EQ. 0 ) EXIT
 2062                   IF ( GMASK(JY,IXL).NE.0 .AND. IDL.EQ.0 )            &
 2063                                                  IDL = GMASK(JY,IXL)
 2064                   END DO
 2065                 IF ( IDL .NE. 0 ) THEN
 2066                     DO KY=JY+1, IY-1
 2067                       IF ( GMASK(KY,IX).EQ.ID ) GMASK(KY,IX) = IDL
 2068                       END DO
 2069                     ID     = ID - 1
 2070                   END IF
 2071 !
 2072               END  IF
 2073             END DO
 2074           END DO
 2075 !
 2076 ! 2.b Grid too small, do not cut
 2077 !
 2078         IF ( IPTOT .LE. IPAVG ) THEN
 2079             CYCLE
 2080           END IF
 2081 !
 2082 ! 2.c Neighbouring grid parts
 2083 !     Raw data
 2084 !
 2085         ALLOCATE ( NEXTTO(0:ID,0:ID), PMAP(0:ID) )
 2086         NEXTTO = .FALSE.
 2087 !
 2088         DO IPT=1, IPTOT
 2089           IX     = IIX(IPT)
 2090           IY     = IIY(IPT)
 
 
                                                         ww3_gspl.F90  page  39
 
 
 2091           IXL    = 1 + MOD(IX-2+NX,NX)
 2092           IYL    = IY - 1
 2093           IXH    = 1 + MOD(IX,NX)
 2094           IYH    = IY + 1
 2095           NEXTTO( GMASK(IY,IX) , GMASK(IY ,IXL) ) = .TRUE.
 2096           NEXTTO( GMASK(IY,IX) , GMASK(IY ,IXH) ) = .TRUE.
 2097           NEXTTO( GMASK(IY,IX) , GMASK(IYL,IX ) ) = .TRUE.
 2098           NEXTTO( GMASK(IY,IX) , GMASK(IYH,IX ) ) = .TRUE.
 2099           END DO
 2100 !
 2101 !     Make symmetric
 2102 !
 2103         DO I=1, ID
 2104           DO J=1, ID
 2105             NEXTTO(I,J) = NEXTTO(I,J) .OR. NEXTTO(J,I)
 2106             END DO
 2107           END DO
 2108 !
 2109 !     Connect accross neighbours
 2110 !
 2111         DO I=1, ID
 2112           DO J=1, ID
 2113             IF ( NEXTTO(I,J) ) THEN
 2114                 DO K=1, ID
 2115                   IF ( NEXTTO(K,J) ) THEN
 2116                       NEXTTO(K,I) = .TRUE.
 2117                       NEXTTO(I,K) = .TRUE.
 2118                     END IF
 2119                   END DO
 2120               END IF
 2121             END DO
 2122           END DO
 2123 !
 2124 !     Map the parts
 2125 !
 2126         IDL    = ID
 2127         PMAP   = 0
 2128         ID     = 0
 2129 !
 2130         DO I=1, IDL
 2131           IF ( PMAP(I) .EQ. 0 ) THEN
 2132               ID     = ID + 1
 2133               DO J=1, IDL
 2134                 IF ( NEXTTO(J,I) ) EXIT
 2135                 END DO
 2136               IF ( J .GT. IDL ) THEN
 2137                   PMAP(I) = ID
 2138                 ELSE
 2139                   DO K=I, IDL
 2140                     IF ( PMAP(K).EQ.0 .AND. NEXTTO(J,K) ) PMAP(K) = ID
 2141                     END DO
 2142                 END IF
 2143             END IF
 2144           END DO
 2145 !
 
 
                                                         ww3_gspl.F90  page  40
 
 
 2146         DEALLOCATE ( NEXTTO )
 2147 !
 2148 ! 3.  Grid is contiguous --------------------------------------------- *
 2149 !
 2150         IF ( ID .EQ. 1 ) THEN
 2151             DEALLOCATE ( PMAP )
 2152             CYCLE
 2153           END IF
 2154 !
 2155 ! 4.  Grid is split, get stats --------------------------------------- *
 2156 !
 2157 ! 4.a Construct final map for grid
 2158 !
 2159         DO IPT=1, IPTOT
 2160           IX     = IIX(IPT)
 2161           IY     = IIY(IPT)
 2162           GMASK(IY,IX) = PMAP(GMASK(IY,IX))
 2163           END DO
 2164 !
 2165         DEALLOCATE ( PMAP )
 2166 !
 2167 ! 4.b Run stats
 2168 !
 2169         ALLOCATE ( INGRD(ID), FLNEXT(ID) )
 2170         INGRD  = 0
 2171         FLNEXT = .FALSE.
 2172         IPTOT  = 0
 2173 !
 2174         DO JX=1, NX
 2175           DO JY=1, NY
 2176             IF ( GMASK(JY,JX) .GT. 0 ) THEN
 2177                 INGRD(GMASK(JY,JX))  = INGRD(GMASK(JY,JX)) + 1
 2178                 IPTOT                = IPTOT + 1
 2179               END IF
 2180             END DO
 2181           END DO
 2182 !
 2183         DO JX=1, NX
 2184           DO JY=1, NY-1
 2185             IF ( ( GMASK(JY  ,JX) .GT. 0 ) .AND.                      &
 2186                  ( SEA(JY+1,JX) .AND. MSPLIT(JY+1,JX).NE.IG ) )       &
 2187                 FLNEXT(GMASK(JY  ,JX)) = .TRUE.
 2188             IF ( ( GMASK(JY+1,JX) .GT. 0 ) .AND.                      &
 2189                  ( SEA(JY  ,JX) .AND. MSPLIT(JY  ,JX).NE.IG ) )       &
 2190                 FLNEXT(GMASK(JY+1,JX)) = .TRUE.
 2191             END DO
 2192           END DO
 2193 !
 2194         DO JY=1, NY
 2195           DO JX=1, NX-1
 2196             IF ( ( GMASK(JY,JX  ) .GT. 0 ) .AND.                      &
 2197                  ( SEA(JY,JX+1) .AND. MSPLIT(JY,JX+1).NE.IG ) )       &
 2198                 FLNEXT(GMASK(JY,JX  )) = .TRUE.
 2199             IF ( ( GMASK(JY,JX+1) .GT. 0 ) .AND.                      &
 2200                  ( SEA(JY,JX  ) .AND. MSPLIT(JY,JX  ).NE.IG ) )       &
 
 
                                                         ww3_gspl.F90  page  41
 
 
 2201                 FLNEXT(GMASK(JY,JX+1)) = .TRUE.
 2202             END DO
 2203           IF ( GLOBAL ) THEN
 2204               IF ( ( GMASK(JY,NX) .GT. 0 ) .AND.                      &
 2205                    ( SEA(JY, 1) .AND. MSPLIT(JY, 1).NE.IG ) )         &
 2206                   FLNEXT(GMASK(JY,NX)) = .TRUE.
 2207               IF ( ( GMASK(JY, 1) .GT. 0 ) .AND.                      &
 2208                    ( SEA(JY,NX) .AND. MSPLIT(JY,NX).NE.IG ) )         &
 2209                   FLNEXT(GMASK(JY, 1)) = .TRUE.
 2210             END IF
 2211           END DO
 2212 !
 2213 ! 5.  Grid large enough, find smallest part -------------------------- *
 2214 !
 2215         IMIN   = NSEA
 2216         LMIN   = 0
 2217 !
 2218         DO J=1, ID
 2219           IF ( FLNEXT(J) .AND. INGRD(J).LT.IMIN ) THEN
 2220               IMIN   = INGRD(J)
 2221               LMIN   = J
 2222             END IF
 2223           END DO
 2224 !
 2225         IF ( LMIN .EQ. 0 ) THEN
 2226             DEALLOCATE ( INGRD, FLNEXT )
 2227             CYCLE
 2228           END IF
 2229 !
 2230         IF ( IMIN .GT. IPCHCK ) THEN
 2231             DEALLOCATE ( INGRD, FLNEXT )
 2232             CYCLE
 2233           END IF
 2234 !
 2235 ! 6.  Part to cut has been identified -------------------------------- *
 2236 !
 2237         DO JX=1, NX
 2238           DO JY=1, NY
 2239               IF ( GMASK(JY,JX) .EQ. LMIN ) MSPLIT(JY,JX) = -1
 2240             END DO
 2241           END DO
 2242 !
 2243         DEALLOCATE ( INGRD, FLNEXT )
 2244         OK     = .FALSE.
 2245 !
 2246 ! ... End loops started in 1.
 2247 !
 2248         END DO
 2249 !
 2250       RETURN
 2251 !
 2252 ! Formats
 2253 !
 2254 !/ End of GRSEPA ----------------------------------------------------- /
 2255 !/
 
 
                                                         ww3_gspl.F90  page  42
 
 
 2256       END SUBROUTINE GRSEPA
 2257 !/ ------------------------------------------------------------------- /
 2258       SUBROUTINE GRFSML
 2259 !/
 2260 !/                  +-----------------------------------+
 2261 !/                  | WAVEWATCH III           NOAA/NCEP |
 2262 !/                  |           H. L. Tolman            |
 2263 !/                  |                        FORTRAN 90 |
 2264 !/                  | Last update :         04-Feb-2013 |
 2265 !/                  +-----------------------------------+
 2266 !/
 2267 !/    13-Sep-2012 : Origination.                        ( version 4.10 )
 2268 !/    04-Feb-2013 : Bug fix grid splitting.             ( version 4.10 )
 2269 !/
 2270 !  1. Purpose :
 2271 !
 2272 !     Subroutine called when lowest grid size is stuck. Attempting to
 2273 !     joint to neighbor grid, otherwise mark as accepted small grid.
 2274 !     note that small grid does not influence parallel scaling like a
 2275 !     big grid does .....
 2276 !
 2277 !     1-Feb-2013: Also used for early small-grid merging.
 2278 !
 2279 ! 10. Source code :
 2280 !
 2281 !/ ------------------------------------------------------------------- /
 2282 !/
 2283       IMPLICIT NONE
 2284 !/
 2285 !/ ------------------------------------------------------------------- /
 2286 !/ Parameter list
 2287 !/
 2288 !/ ------------------------------------------------------------------- /
 2289 !/ Local parameters
 2290 !/
 2291       INTEGER                 :: NSMALL, IGMIN(NG), NNEXT, JG, IGADD, &
 2292                                  IGTEST, FREE(NG), NFREE, NBIG, IGB,  &
 2293                                  MX, MY, NX0, NXN, NY0, NYN, JX
 2294       CHARACTER(LEN=1)        :: NEXTTO(0:NG,0:NG), TEMP(NG)
 2295 !/
 2296 !/ ------------------------------------------------------------------- /
 2297 !/
 2298 !
 2299 ! 1.  Find small(s) -------------------------------------------------- *
 2300 !
 2301       NSMALL = 0
 2302       IGMIN  = 0
 2303 !
 2304       DO IG=1,NG
 2305         IF ( GSTATS(IG)%INSTAT .AND.                                  &
 2306              GSTATS(IG)%NPTS .EQ. MSTATS%NMIN ) THEN
 2307             NSMALL = NSMALL + 1
 2308             IGMIN(NSMALL)  = IG
 2309           END IF
 2310         END DO
 
 
                                                         ww3_gspl.F90  page  43
 
 
 2311 !
 2312 ! 2.  Find neighbours ------------------------------------------------ *
 2313 !
 2314       NEXTTO = '.'
 2315 !
 2316       DO IX=1, NX-1
 2317         DO IY=1, NY-1
 2318           NEXTTO(MSPLIT(IY  ,IX  ),MSPLIT(IY+1,IX  )) = 'X'
 2319           NEXTTO(MSPLIT(IY+1,IX  ),MSPLIT(IY  ,IX  )) = 'X'
 2320           NEXTTO(MSPLIT(IY  ,IX+1),MSPLIT(IY  ,IX  )) = 'X'
 2321           NEXTTO(MSPLIT(IY  ,IX  ),MSPLIT(IY  ,IX+1)) = 'X'
 2322           END DO
 2323         END DO
 2324 !
 2325       IF ( GLOBAL ) THEN
 2326           DO IY=1, NY-1
 2327             NEXTTO(MSPLIT(IY  ,NX),MSPLIT(IY+1,NX)) = 'X'
 2328             NEXTTO(MSPLIT(IY+1,NX),MSPLIT(IY  ,NX)) = 'X'
 2329             NEXTTO(MSPLIT(IY  , 1),MSPLIT(IY  ,NX)) = 'X'
 2330             NEXTTO(MSPLIT(IY  ,NX),MSPLIT(IY  , 1)) = 'X'
 2331             END DO
 2332         END IF
 2333 !
 2334       DO IG=0,NG
 2335         NEXTTO(IG,IG) = '-'
 2336         END DO
 2337 !
 2338 ! 3.  Loop over small grids ------------------------------------------ *
 2339 !
 2340       FREE   = 0
 2341       NFREE  = 0
 2342 !
 2343       DO J=1, NSMALL
 2344 !
 2345 ! 3.a Find neighbours
 2346 !
 2347         IG     = IGMIN(J)
 2348         IGADD   = 0
 2349         IGTEST  = NSEA + 1
 2350         NNEXT  = 0
 2351         DO JG=1, NG
 2352           IF ( NEXTTO(IG,JG) .EQ. 'X' ) THEN
 2353               NNEXT  = NNEXT + 1
 2354               IF ( GSTATS(JG)%NPTS .LT. IGTEST ) THEN
 2355                   IGTEST = GSTATS(JG)%NPTS
 2356                   IGADD  = JG
 2357                 END IF
 2358             END IF
 2359           END DO
 2360 !
 2361 ! 3.b No neighbours found, mark as 'not to be processed further'
 2362 !
 2363         IF ( NNEXT .EQ. 0 ) THEN
 2364             GSTATS(IG)%INSTAT = .FALSE.
 2365           ELSE
 
 
                                                         ww3_gspl.F90  page  44
 
 
 2366 !
 2367 ! 3.c Check smallest neighbor
 2368 !
 2369             IF ( IGTEST + INGMIN .LT. NINT(XMEAN) ) THEN
 2370 !
 2371 ! ... Merge grids
 2372 !
 2373                 DO IX=1, NX
 2374                  DO IY=1, NY
 2375                    IF ( MSPLIT(IY,IX) .EQ. IG ) MSPLIT(IY,IX) = IGADD
 2376                    END DO
 2377                  END DO
 2378 !
 2379                 NFREE  = NFREE + 1
 2380                 FREE(NFREE) = IG
 2381 !
 2382               ELSE
 2383 !
 2384 ! ... Remove grid(s) from stats
 2385 !
 2386                 GSTATS(IG)%INSTAT = .FALSE.
 2387                 NNEXT  = 0
 2388                 DO JG=1, NG
 2389                   IF ( NEXTTO(IGADD,JG) .EQ. 'X' ) NNEXT  = NNEXT + 1
 2390                   END DO
 2391                 IF ( NNEXT .EQ. 1 ) THEN
 2392                     GSTATS(IGADD)%INSTAT = .FALSE.
 2393                   END IF
 2394 !
 2395               END IF
 2396 !
 2397           END IF
 2398 !
 2399         END DO
 2400 !
 2401 ! 4.  Make new grids as needed --------------------------------------- *
 2402 !
 2403       DO J=1, NFREE
 2404 !
 2405 ! 4.a Find biggest grid
 2406 !
 2407         NBIG   = 0
 2408         IGB    = 0
 2409 !
 2410         DO IG=1, NG
 2411           IF ( GSTATS(IG)%NPTS .GT. NBIG ) THEN
 2412               NBIG   = GSTATS(IG)%NPTS
 2413               IGB    = IG
 2414             END IF
 2415           END DO
 2416 !
 2417 ! 4.a Split biggest grid
 2418 !
 2419         NX0    = GSTATS(IGB)%NXL
 2420         NXN    = GSTATS(IGB)%NXH
 
 
                                                         ww3_gspl.F90  page  45
 
 
 2421         NY0    = GSTATS(IGB)%NYL
 2422         NYN    = GSTATS(IGB)%NYH
 2423 !
 2424         MY     = 1 + GSTATS(IGB)%NYH - GSTATS(IGB)%NYL
 2425         MX     = 1 + GSTATS(IGB)%NXH - GSTATS(IGB)%NXL
 2426         IF ( GSTATS(IGB)%STRADLE ) MX     = MX + NX
 2427 !
 2428         IF ( MY .GE. MX ) THEN
 2429             NYN    = NY0 + MY/2
 2430           ELSE
 2431             NXN    = NX0 + MX/2
 2432           END IF
 2433 !
 2434         DO IX=NX0, NXN
 2435           JX     = 1 + MOD(IX-1,NX)
 2436           DO IY=NY0, NYN
 2437             IF ( MSPLIT(IY,JX) .EQ. IGB ) MSPLIT(IY,JX) = FREE(J)
 2438             END DO
 2439           END DO
 2440 !
 2441         GSTATS(IGB)%NPTS = 0
 2442         GSTATS(FREE(J))%NPTS = 0
 2443 !
 2444         END DO
 2445 !
 2446       RETURN
 2447 !
 2448 ! Formats
 2449 !
 2450 !/ End of GRFSML ----------------------------------------------------- /
 2451 !/
 2452       END SUBROUTINE GRFSML
 2453 !/ ------------------------------------------------------------------- /
 2454       SUBROUTINE GRFLRG
 2455 !/
 2456 !/                  +-----------------------------------+
 2457 !/                  | WAVEWATCH III           NOAA/NCEP |
 2458 !/                  |           H. L. Tolman            |
 2459 !/                  |                        FORTRAN 90 |
 2460 !/                  | Last update :         29-Jan-2013 |
 2461 !/                  +-----------------------------------+
 2462 !/
 2463 !/    19-Sep-2012 : Origination.                        ( version 4.10 )
 2464 !/    29-Jan-2013 : Add error code on stop.             ( version 4.10 )
 2465 !/
 2466 !  1. Purpose :
 2467 !
 2468 !     Like GRFSML for largest grid ...
 2469 !
 2470 ! 10. Source code :
 2471 !
 2472 !/ ------------------------------------------------------------------- /
 2473 !/
 2474       IMPLICIT NONE
 2475 !/
 
 
                                                         ww3_gspl.F90  page  46
 
 
 2476 !/ ------------------------------------------------------------------- /
 2477 !/ Parameter list
 2478 !/
 2479 !/ ------------------------------------------------------------------- /
 2480 !/ Local parameters
 2481 !/
 2482       INTEGER                 :: NBIG, IGMAX(NG), NNEXT, JG
 2483 !!!      INTEGER                 :: NSMALL, IGMIN(NG), NNEXT, JG, IGADD, &
 2484 !!!                                 IGTEST, FREE(NG), NFREE, NBIG, IGB,  &
 2485 !!!                                 MX, MY, NX0, NXN, NY0, NYN
 2486 !!!!/S      INTEGER, SAVE           :: IENT = 0
 2487       CHARACTER(LEN=1)        :: NEXTTO(0:NG,0:NG), TEMP(NG)
 2488 !/
 2489 !/ ------------------------------------------------------------------- /
 2490 !/
 2491 !
 2492 ! 1.  Find big(s) ---------------------------------------------------- *
 2493 !
 2494       NBIG   = 0
 2495       IGMAX  = 0
 2496 !
 2497       DO IG=1,NG
 2498         IF ( GSTATS(IG)%INSTAT .AND.                                  &
 2499              GSTATS(IG)%NPTS .EQ. MSTATS%NMAX ) THEN
 2500             NBIG   = NBIG + 1
 2501             IGMAX(NBIG)  = IG
 2502           END IF
 2503         END DO
 2504 !
 2505 ! 2.  Find neighbours ------------------------------------------------ *
 2506 !
 2507       NEXTTO = '.'
 2508 !
 2509       DO IX=1, NX-1
 2510         DO IY=1, NY-1
 2511           NEXTTO(MSPLIT(IY  ,IX  ),MSPLIT(IY+1,IX  )) = 'X'
 2512           NEXTTO(MSPLIT(IY+1,IX  ),MSPLIT(IY  ,IX  )) = 'X'
 2513           NEXTTO(MSPLIT(IY  ,IX+1),MSPLIT(IY  ,IX  )) = 'X'
 2514           NEXTTO(MSPLIT(IY  ,IX  ),MSPLIT(IY  ,IX+1)) = 'X'
 2515           END DO
 2516         END DO
 2517 !
 2518       IF ( GLOBAL ) THEN
 2519           DO IY=1, NY-1
 2520             NEXTTO(MSPLIT(IY  ,NX),MSPLIT(IY+1,NX)) = 'X'
 2521             NEXTTO(MSPLIT(IY+1,NX),MSPLIT(IY  ,NX)) = 'X'
 2522             NEXTTO(MSPLIT(IY  , 1),MSPLIT(IY  ,NX)) = 'X'
 2523             NEXTTO(MSPLIT(IY  ,NX),MSPLIT(IY  , 1)) = 'X'
 2524             END DO
 2525         END IF
 2526 !
 2527       DO IG=0,NG
 2528         NEXTTO(IG,IG) = '-'
 2529         END DO
 2530 !
 
 
                                                         ww3_gspl.F90  page  47
 
 
 2531 ! 3.  Loop over big grids -------------------------------------------- *
 2532 !
 2533       DO J=1, NBIG
 2534 !
 2535 ! 3.a Find neighbours
 2536 !
 2537         IG     = IGMAX(J)
 2538         NNEXT  = 0
 2539         DO JG=1, NG
 2540           IF ( NEXTTO(IG,JG) .EQ. 'X' ) NNEXT  = NNEXT + 1
 2541           END DO
 2542 !
 2543 ! 3.b Enough neighbours found, mark as 'not to be processed further'
 2544 !
 2545         IF ( NNEXT .GE. 1 ) THEN
 2546             GSTATS(IG)%INSTAT = .FALSE.
 2547           ELSE
 2548 !
 2549 ! 3.c Biggest grid is isolated, should split
 2550 !
 2551             WRITE (NDSE,930)
 2552             STOP 11
 2553 !
 2554           END IF
 2555 !
 2556         END DO
 2557 !
 2558       RETURN
 2559 !
 2560 ! Formats
 2561 !
 2562   930 FORMAT ( ' *** ERROR GRFLRG: LARGEST GRID IS ISOLATED ***'      &
 2563                '     SPLITTING NOT YET IMPLEMENTED '/)
 2564 !
 2565 !/ End of GRFLRG ----------------------------------------------------- /
 2566 !/
 2567       END SUBROUTINE GRFLRG
 2568 !/ ------------------------------------------------------------------- /
 2569       SUBROUTINE GR1GRD
 2570 !/
 2571 !/                  +-----------------------------------+
 2572 !/                  | WAVEWATCH III           NOAA/NCEP |
 2573 !/                  |           H. L. Tolman            |
 2574 !/                  |                        FORTRAN 90 |
 2575 !/                  | Last update :         18-Nov-2012 |
 2576 !/                  +-----------------------------------+
 2577 !/
 2578 !/    23-Sep-2012 : Origination.                        ( version 4.10 )
 2579 !/    24-Jan-2013 : Correct X0 to be in range.          ( version 4.10 )
 2580 !/    04-Feb-2013 : Add corner point to halo.           ( version 4.10 )
 2581 !/    18-Nov-2012 : Add user-defined halo extension.    ( version 4.14 )
 2582 !/
 2583 !  1. Purpose :
 2584 !
 2585 !     Extract single grid from master map, including halo needed for
 
 
                                                         ww3_gspl.F90  page  48
 
 
 2586 !     grid overlap in ww3_multi.
 2587 !
 2588 ! 10. Source code :
 2589 !
 2590 !/ ------------------------------------------------------------------- /
 2591 !/
 2592       IMPLICIT NONE
 2593 !/
 2594 !/ ------------------------------------------------------------------- /
 2595 !/ Parameter list
 2596 !/
 2597 !/ ------------------------------------------------------------------- /
 2598 !/ Local parameters
 2599 !/
 2600       INTEGER                 :: NIT, IIT,  IXL, IXH, IYL, IYH, NOCNT,&
 2601                                  NOCNTM, NOCNTL, JX, JY, ISEA, MX, MY
 2602       INTEGER                 :: MTMP2(NY,NX)
 2603       REAL                    :: XOFF
 2604       LOGICAL                 :: MASK(NY,NX), LEFT, RIGHT, THERE
 2605 !/
 2606 !/ ------------------------------------------------------------------- /
 2607 !/
 2608 !
 2609 ! 1.  Set up MTEMP with MAPSTA 0,1,3 for grid ------------------------ *
 2610 !
 2611       DO IX=1, NX
 2612         DO IY=1, NY
 2613           IF ( MSPLIT(IY,IX) .EQ. IG ) THEN
 2614               MTEMP(IY,IX) = 1
 2615             ELSE IF ( MSPLIT(IY,IX) .GT. 0 ) THEN
 2616               MTEMP(IY,IX) = 3
 2617             ELSE
 2618               MTEMP(IY,IX) = 0
 2619             END IF
 2620           END DO
 2621         END DO
 2622 !
 2623 ! 2.  Add ALL MAPSTA = 2 points to grid ------------------------------ *
 2624 !
 2625       DO IX=1, NX
 2626         DO IY=1, NY
 2627           IF ( MAPSTA(IY,IX) .EQ. 2 ) THEN
 2628               MTEMP(IY,IX) = 2
 2629             END IF
 2630           END DO
 2631         END DO
 2632 !
 2633 ! 3.  Add halo ------------------------------------------------------- *
 2634 ! 3.a Set up halo width depending on scheme and time steps
 2635 !     NEEDED TO SET UP A LITTLE WIDER. NOT SURE WHY. NEED TO CHECK WITH
 2636 !     WMEQL SUBROUTINE.
 2637 !
 2638       NIT    = 1 + NHEXT + ( 1 + INT(DTMAX/DTCFL-0.001) ) * 3
 2639 !
 2640 ! 3.b Exand halo
 
 
                                                         ww3_gspl.F90  page  49
 
 
 2641 !
 2642       DO IIT=1, NIT
 2643 !
 2644         MASK   = .FALSE.
 2645 !
 2646         DO IX=1, NX
 2647           IXL    = 1 + MOD(IX-2+NX,NX)
 2648           IXH    = 1 + MOD(IX,NX)
 2649           DO IY=2, NY-1
 2650             IF ( MTEMP(IY,IX) .EQ. 3 ) MASK(IY,IX) =                  &
 2651                               ( ( MTEMP(IY+1,IX ) .EQ. 1 ) .OR.       &
 2652                                 ( MTEMP(IY-1,IX ) .EQ. 1 ) .OR.       &
 2653                                 ( MTEMP(IY  ,IXH) .EQ. 1 ) .OR.       &
 2654                                 ( MTEMP(IY  ,IXL) .EQ. 1 ) )          &
 2655                     .OR.    (   ( MTEMP(IY+1,IXL) .EQ. 1 ) .AND.      &
 2656                               ( ( MTEMP(IY  ,IXL) .EQ. 1 ) .OR.       &
 2657                                 ( MTEMP(IY+1,IX ) .EQ. 1 ) ) )        &
 2658                     .OR.    (   ( MTEMP(IY+1,IXH) .EQ. 1 ) .AND.      &
 2659                               ( ( MTEMP(IY  ,IXH) .EQ. 1 ) .OR.       &
 2660                                 ( MTEMP(IY+1,IX ) .EQ. 1 ) ) )        &
 2661                     .OR.    (   ( MTEMP(IY-1,IXH) .EQ. 1 ) .AND.      &
 2662                               ( ( MTEMP(IY  ,IXH) .EQ. 1 ) .OR.       &
 2663                                 ( MTEMP(IY-1,IX ) .EQ. 1 ) ) )        &
 2664                     .OR.    (   ( MTEMP(IY-1,IXL) .EQ. 1 ) .AND.      &
 2665                               ( ( MTEMP(IY  ,IXL) .EQ. 1 ) .OR.       &
 2666                                 ( MTEMP(IY-1,IX ) .EQ. 1 ) ) )
 2667             END DO
 2668           END DO
 2669 !
 2670         DO IX=1, NX
 2671           DO IY=1, NY
 2672             IF ( MASK(IY,IX) ) MTEMP(IY,IX) = 1
 2673             END DO
 2674           END DO
 2675 !
 2676         END DO
 2677 !
 2678 ! 3.c Contract halo
 2679 !
 2680 !     MTMP2 = MTEMP
 2681 !
 2682 !     DO IIT=1, NIT
 2683 !
 2684 !       MASK   = .FALSE.
 2685 !
 2686 !       DO IX=1, NX
 2687 !         IXL    = 1 + MOD(IX-2+NX,NX)
 2688 !         IXH    = 1 + MOD(IX,NX)
 2689 !         DO IY=2, NY-1
 2690 !           IF ( MTMP2(IY,IX) .EQ. 1 ) MASK(IY,IX) =                  &
 2691 !                             ( ( MTMP2(IY+1,IX ) .EQ. 3 ) .OR.       &
 2692 !                               ( MTMP2(IY-1,IX ) .EQ. 3 ) .OR.       &
 2693 !                               ( MTMP2(IY  ,IXH) .EQ. 3 ) .OR.       &
 2694 !                               ( MTMP2(IY  ,IXL) .EQ. 3 ) )          &
 2695 !                   .OR.    (   ( MTMP2(IY+1,IXL) .EQ. 3 ) .AND.      &
 
 
                                                         ww3_gspl.F90  page  50
 
 
 2696 !                             ( ( MTMP2(IY  ,IXL) .EQ. 3 ) .OR.       &
 2697 !                               ( MTMP2(IY+1,IX ) .EQ. 3 ) ) )        &
 2698 !                   .OR.    (   ( MTMP2(IY+1,IXH) .EQ. 3 ) .AND.      &
 2699 !                             ( ( MTMP2(IY  ,IXH) .EQ. 3 ) .OR.       &
 2700 !                               ( MTMP2(IY+1,IX ) .EQ. 3 ) ) )        &
 2701 !                   .OR.    (   ( MTMP2(IY-1,IXH) .EQ. 3 ) .AND.      &
 2702 !                             ( ( MTMP2(IY  ,IXH) .EQ. 3 ) .OR.       &
 2703 !                               ( MTMP2(IY-1,IX ) .EQ. 3 ) ) )        &
 2704 !                   .OR.    (   ( MTMP2(IY-1,IXL) .EQ. 3 ) .AND.      &
 2705 !                             ( ( MTMP2(IY  ,IXL) .EQ. 3 ) .OR.       &
 2706 !                               ( MTMP2(IY-1,IX ) .EQ. 3 ) ) )
 2707 !           END DO
 2708 !         END DO
 2709 !
 2710 !       DO IX=1, NX
 2711 !         DO IY=1, NY
 2712 !           IF ( MASK(IY,IX) ) MTMP2(IY,IX) = 3
 2713 !           END DO
 2714 !         END DO
 2715 !
 2716 !       END DO
 2717 !
 2718 ! 3.d Check if consistent .....
 2719 !
 2720 !     DO IX=1, NX
 2721 !       DO IY=1, NY
 2722 !         IF ( MSPLIT(IY,IX).EQ.IG .OR. MTMP2(IY,IX).EQ.1 ) THEN
 2723 !             IF ( MSPLIT(IY,IX).EQ.IG .AND. MTMP2(IY,IX).NE.1 ) THEN
 2724 !                 write (ndst,*) ix, iy, '  in grid, not in e-c grid'
 2725 !               END IF
 2726 !             IF ( MSPLIT(IY,IX).NE.IG .AND. MTMP2(IY,IX).EQ.1 ) THEN
 2727 !                 write (ndst,*) ix, iy, '  in e-c grid, not in grid'
 2728 !               END IF
 2729 !           END IF
 2730 !         END DO
 2731 !       END DO
 2732 !
 2733 ! 4.  Remove extraeneous MAPSTA = 2 ---------------------------------- *
 2734 !
 2735       DO IX=1, NX
 2736 !
 2737         IF ( GLOBAL ) THEN
 2738             IXL    = 1 + MOD(IX-2+NX,NX)
 2739             IXH    = 1 + MOD(IX,NX)
 2740           ELSE
 2741             IXL    = MAX ( 1 , IX-1 )
 2742             IXH    = MIN ( NX , IX+1 )
 2743           END IF
 2744 !
 2745         DO IY=1, NY
 2746           IF ( MTEMP(IY,IX) .EQ. 2 ) THEN
 2747               IYL    = MAX ( 1 , IY-1 )
 2748               IYH    = MIN ( NY , IY+1 )
 2749               IF ( .NOT. ( ( MTEMP(IYL,IX ) .EQ. 1 ) .OR.             &
 2750                            ( MTEMP(IYH,IX ) .EQ. 1 ) .OR.             &
 
 
                                                         ww3_gspl.F90  page  51
 
 
 2751                            ( MTEMP(IY ,IXL) .EQ. 1 ) .OR.             &
 2752                            ( MTEMP(IY ,IXH) .EQ. 1 ) ) )              &
 2753                    MTEMP(IY,IX) = 3
 2754             END IF
 2755           END DO
 2756 !
 2757         END DO
 2758 !
 2759 ! 5.  Recompute grid range ------------------------------------------- *
 2760 !     Using GSTOLD to store info for modified grid
 2761 !
 2762       GSTOLD(IG)%STRADLE = .FALSE.
 2763       GSTOLD(IG)%NPTS    = 0
 2764       GSTOLD(IG)%NXL     = NX
 2765       GSTOLD(IG)%NXH     = 1
 2766       GSTOLD(IG)%NYL     = NY
 2767       GSTOLD(IG)%NYH     = 1
 2768 !
 2769       IF ( GLOBAL ) THEN
 2770 !
 2771           LEFT   = .FALSE.
 2772           RIGHT  = .FALSE.
 2773 !
 2774           DO IY=1, NY
 2775             IF ( MTEMP(IY, 1).EQ.1 .OR.  MTEMP(IY, 1).EQ.2 ) LEFT   = .TR>
 2776             IF ( MTEMP(IY,NX).EQ.1 .OR.  MTEMP(IY,NX).EQ.2 ) RIGHT  = .TR>
 2777             END DO
 2778           GSTOLD(IG)%STRADLE = LEFT .AND. RIGHT
 2779 !
 2780         END IF
 2781 !
 2782       DO IY=1, NY
 2783         DO IX=1, NX
 2784           IF ( MTEMP(IY,IX).EQ.1 .OR.  MTEMP(IY,IX).EQ.2 ) THEN
 2785               GSTOLD(IG)%NPTS = GSTOLD(IG)%NPTS + 1
 2786               GSTOLD(IG)%NXL  = MIN ( GSTOLD(IG)%NXL , IX )
 2787               GSTOLD(IG)%NXH  = MAX ( GSTOLD(IG)%NXH , IX )
 2788               GSTOLD(IG)%NYL  = MIN ( GSTOLD(IG)%NYL , IY )
 2789               GSTOLD(IG)%NYH  = MAX ( GSTOLD(IG)%NYH , IY )
 2790             END IF
 2791           END DO
 2792         END DO
 2793 !
 2794       IF ( GSTOLD(IG)%STRADLE ) THEN
 2795           NOCNT  = 0
 2796           NOCNTM = 0
 2797           NOCNTL = 0
 2798           DO IX=1, NX
 2799             THERE  = .FALSE.
 2800             DO IY=1, NY
 2801               IF ( MTEMP(IY,IX).EQ.1 .OR.  MTEMP(IY,IX).EQ.2 ) THEN
 2802                   THERE  = .TRUE.
 2803                   EXIT
 2804                 END IF
 2805               END DO
 
 
                                                         ww3_gspl.F90  page  52
 
 
 2806             IF ( THERE ) THEN
 2807                 NOCNT = 0
 2808               ELSE
 2809                 NOCNT = NOCNT + 1
 2810                 IF ( NOCNT .GT. NOCNTM ) THEN
 2811                     NOCNTM = NOCNT
 2812                     NOCNTL = IX
 2813                   END IF
 2814               END IF
 2815             END DO
 2816           GSTOLD(IG)%NXL = NOCNTL + 1
 2817           GSTOLD(IG)%NXH = NOCNTL - NOCNTM
 2818         END IF
 2819 !
 2820 ! ... Make sure outside of grid is 2 or 3
 2821 !
 2822       LEFT   = .FALSE.
 2823       RIGHT  = .FALSE.
 2824 !
 2825       DO IX=1, NX
 2826         LEFT   = LEFT  .OR. ( MTEMP(GSTOLD(IG)%NYL,IX) .EQ. 1 )
 2827         RIGHT  = RIGHT .OR. ( MTEMP(GSTOLD(IG)%NYH,IX) .EQ. 1 )
 2828         END DO
 2829 !
 2830       IF ( LEFT  ) GSTOLD(IG)%NYL = GSTOLD(IG)%NYL - 1
 2831       IF ( RIGHT ) GSTOLD(IG)%NYH = GSTOLD(IG)%NYH + 1
 2832 !
 2833       DO IY=1, NY
 2834         LEFT   = LEFT  .OR. ( MTEMP(IY,GSTOLD(IG)%NXL) .EQ. 1 )
 2835         RIGHT  = RIGHT .OR. ( MTEMP(IY,GSTOLD(IG)%NXH) .EQ. 1 )
 2836         END DO
 2837 !
 2838       IF ( LEFT  ) GSTOLD(IG)%NXL = GSTOLD(IG)%NXL - 1
 2839       IF ( RIGHT ) GSTOLD(IG)%NXH = GSTOLD(IG)%NXH + 1
 2840 !
 2841       IF ( GLOBAL .AND. GSTOLD(IG)%NXL.EQ.0 ) THEN
 2842           GSTOLD(IG)%NXL     = NX
 2843           GSTOLD(IG)%STRADLE = .TRUE.
 2844         END IF
 2845 !
 2846       IF ( GLOBAL .AND. GSTOLD(IG)%NXH.EQ.NX+1 ) THEN
 2847           GSTOLD(IG)%NXH     = 1
 2848           GSTOLD(IG)%STRADLE = .TRUE.
 2849         END IF
 2850 !
 2851 ! 6.  Extract reduced grid data -------------------------------------- *
 2852 !
 2853       MY             = 1 + GSTOLD(IG)%NYH - GSTOLD(IG)%NYL
 2854       MX             = 1 + GSTOLD(IG)%NXH - GSTOLD(IG)%NXL
 2855       IF ( GSTOLD(IG)%STRADLE ) MX = MX + NX
 2856       PGRID(IG)%NY   = MY
 2857       PGRID(IG)%NX   = MX
 2858       PGRID(IG)%NSEA = GSTOLD(IG)%NPTS
 2859       PGRID(IG)%X0   = X0 + REAL(GSTOLD(IG)%NXL-1)*SX
 2860       PGRID(IG)%Y0   = Y0 + REAL(GSTOLD(IG)%NYL-1)*SY
 
 
                                                         ww3_gspl.F90  page  53
 
 
 2861       PGRID(IG)%SX   = SX
 2862       PGRID(IG)%SY   = SY
 2863 !
 2864       XOFF   = 360. * REAL ( NINT((PGRID(IG)%X0+0.5*REAL(MX-1)*SX)/360.) )
 2865       PGRID(IG)%X0   = PGRID(IG)%X0 - XOFF
 2866 !
 2867       ALLOCATE ( PGRID(IG)%ZBIN(MX,MY) ,          &
 2868                  PGRID(IG)%OBSX(MX,MY) ,          &
 2869                  PGRID(IG)%OBSY(MX,MY) ,          &
 2870                  PGRID(IG)%MASK(MX,MY) )
 2871 !
 2872       PGRID(IG)%ZBIN   = ZBDUM
 2873       PGRID(IG)%OBSX   = 0.
 2874       PGRID(IG)%OBSY   = 0.
 2875       PGRID(IG)%MASK   = 99
 2876 !
 2877       DO IX=1, PGRID(IG)%NX
 2878         JX     = 1 + MOD ( IX+GSTOLD(IG)%NXL-2 , NX )
 2879         DO IY=1, PGRID(IG)%NY
 2880           JY     = IY + GSTOLD(IG)%NYL - 1
 2881           ISEA   = MAPFS(JY,JX)
 2882           IF ( MTEMP(JY,JX) .NE. 0 ) THEN
 2883               PGRID(IG)%ZBIN(IX,IY) = ZB(ISEA)
 2884             END IF
 2885           IF ( TRFLAG .NE. 0 ) THEN
 2886               PGRID(IG)%OBSX(IX,IY) = 1. - TRNX(JY,JX)
 2887               PGRID(IG)%OBSY(IX,IY) = 1. - TRNY(JY,JX)
 2888             END IF
 2889           PGRID(IG)%MASK(IX,IY) = MTEMP(JY,JX)
 2890           END DO
 2891         END DO
 2892 !
 2893       RETURN
 2894 !
 2895 ! Formats
 2896 !
 2897 !/ End of GR1GRD ----------------------------------------------------- /
 2898 !/
 2899       END SUBROUTINE GR1GRD
 2900 !/
 2901 !/ End of W3GSPL ----------------------------------------------------- /
 2902 !/
 2903       END PROGRAM W3GSPL
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
