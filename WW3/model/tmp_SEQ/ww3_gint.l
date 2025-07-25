                                                         ww3_gint.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3GRID_INTERP
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH-III           NOAA/NCEP |
    7 !/                  |             A. Chawla             |SX
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         31-Aug-2018 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    15-Mar-2007 : Origination.                        ( version 3.13 )
   13 !/    24-Sep-2007 : Original code                       ( version 3.14 )
   14 !/    01-Aug-2011 : Modified to match grid output       ( version 4.01 )
   15 !/    20-Feb-2013 : Modified for new output fields      ( version 4.11 )
   16 !/    11-Nov-2013 : Update for curvilinear grids        ( version 4.13 )
   17 !/    22-Jan-2014 : Update for UNST grids (F. Ardhuin)  ( version 4.18 )
   18 !/    30-Apr-2014 : Add group 3 (M. Accensi)            ( version 5.00 )
   19 !/    26-Jul-2018 : Write weights file WHTGRIDINT.bin
   20 !/                  (F.Ardhuin, M.Accensi, J.H.Alves)   ( version 6.05 )
   21 !/    31-Aug-2018 : Update groups 2,4,6,8 (S. Zieger)   ( version 6.05 )
   22 !/    26-Jan-2021 : Added TP field (derived from FP)    ( version 7.12 )
   23 !/
   24 !   1. Purpose :
   25 !
   26 !   Re-gridding binary output (out_grd.* files) to another grid
   27 !
   28 !   2. Method :
   29 !
   30 !   Data is interpolated from a combination of base grids to the target
   31 !   grid. For each grid, if resolution is coarser or similar to target
   32 !   grid then a linear interpolation approach is used. On the other hand
   33 !   if resolution is much higher then an averaging technique based on
   34 !   cell areas.
   35 !   Total number of base grids to be used for interpolation together with
   36 !   their (and target grid) file extns are read from 'ww3_gint.inp'.
   37 !   Base grids can be arranged in any order but the target grid should
   38 !       always be the last grid.
   39 !
   40 !   3. Parameters :
   41 !
   42 !   4. Subroutines used :
   43 !
   44 !      Name      Type  Module   Description
   45 !     ----------------------------------------------------------------
   46 !      W3NMOD    Subr. W3GDATMD Set number of model.
   47 !      W3SETG    Subr.   Id.    Point to selected model.
   48 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
   49 !      NEXTLN    Subr. W3SERVMD Get next line from input file
   50 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
   51 !      ITRACE    Subr.   Id.    Subroutine tracing initialization.
   52 !      STRACE    Subr.   Id.    Subroutine tracing.
   53 !      W3NOUT    Subr. W3ODATMD Set number of model for output.
   54 !      W3SETO    Subr.   Id.    Point to selected model for output.
   55 !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
 
 
                                                         ww3_gint.F90  page   2
 
 
   56 !      W3SETW    Subr. W3WDATMD Point to selected model for wave data.
   57 !      W3NAUX    Subr. W3ADATMD Set number of model for aux data.
   58 !      W3SETA    Subr.   Id.    Point to selected model for aux data.
   59 !      W3DIMA      Subr.   Id.    Assign memory for aux data.
   60 !      W3GRMP    Func. W3GSRUMD Compute interpolation coeff. from grid.
   61 !      W3CKCL    Func.   Id.    Check if point lies within grid cell.
   62 !      W3IOGO    Subr. W3IOGOMD Reading/writing raw gridded data file.
   63 !     ----------------------------------------------------------------
   64 !
   65 !  5. Called by :
   66 !
   67 !     None, stand-alone program.
   68 !
   69 !  6. Error messages :
   70 !
   71 !     Checks on input, checks on determining the interpolation weights
   72 !
   73 !  7. Remarks :
   74 !
   75 !  8. Structure :
   76 !
   77 !     See source code.
   78 !
   79 !  9. Switches :
   80 !
   81 !       !/S     Enable subroutine tracing.
   82 !
   83 ! 10. Source code :
   84 !
   85 !/ ------------------------------------------------------------------- /
   86         USE CONSTANTS
   87 !/
   88         USE W3IOGRMD
   89         USE W3TIMEMD
   90         USE W3IOGOMD, ONLY : W3IOGO
   91         USE W3ADATMD, ONLY : W3DIMA, W3NAUX, W3SETA
   92         USE W3GDATMD
   93         USE W3ODATMD, ONLY : FNMPRE, NOGRP, NGRPP, OUTPTS, UNDEF, FLOGRD,>
   94                              NAPROC, NOSWLL, IDOUT
   95         USE W3ODATMD, ONLY : W3NOUT, W3SETO
   96         USE W3IDATMD
   97         USE W3WDATMD, ONLY : W3NDAT, W3DIMW, W3SETW
   98         USE W3WDATMD, ONLY : WDATAS, TIME, WLV, ICE, ICEH, ICEF, UST, UST>
   99         USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
  100         USE W3ARRYMD, ONLY : PRTBLK
  101         USE W3GSRUMD
  102         USE W3TRIAMD
  103 !/
  104         IMPLICIT NONE
  105 !/
  106 !/ ------------------------------------------------------------------- /
  107 !/ Local data structure
  108 !/
  109         TYPE GR_WT
  110            INTEGER              :: NP
 
 
                                                         ww3_gint.F90  page   3
 
 
  111            INTEGER, ALLOCATABLE :: IP(:), JP(:)
  112            REAL,    ALLOCATABLE :: WT(:)
  113            REAL                 :: AR
  114         END TYPE GR_WT
  115 !
  116         TYPE GR_INT
  117            INTEGER              :: NGRDS
  118            INTEGER, ALLOCATABLE :: GDID(:)
  119            TYPE(GR_WT), ALLOCATABLE :: IND_WTS(:)
  120         END TYPE GR_INT
  121 !/
  122 !/ Local variables
  123 !/
  124   
  125         TYPE(GR_INT), TARGET, ALLOCATABLE :: GR_INTS(:)
  126         INTEGER                 :: I, J, IERR, NG, IG, JG, ISEA, IX, IY, >
  127         INTEGER                 :: IYT, NS, COUNTF, COUNTG, NOSWLL_MIN, I>
  128         INTEGER                 :: NDSM, NDSI, NDSE, NDSO, NDSTRC, NTRACE>
  129         INTEGER                 :: EXTRA, NSEA_FILE
  130         INTEGER, ALLOCATABLE    :: FIDOUT(:), MAP(:,:), TMP_INDX(:)
  131         REAL                    :: SXT, SYT, XT, YT, XTT
  132         DOUBLE PRECISION        :: DAREA, SAREA
  133         REAL                    :: XCRNR(5),YCRNR(5),DT(4),DX,DY,XSUB,YSUB
  134         INTEGER                 :: TOUT(2), NOUT, IOUT
  135         REAL                    :: DTREQ, DTEST
  136         INTEGER                 :: IS(4), JS(4)
  137         INTEGER                 :: MAPINT
  138         REAL                    :: RW(4), SUMWT
  139         REAL, ALLOCATABLE       :: INT_MAP(:,:)
  140         LOGICAL                 :: L360=.FALSE., LPLC, INGRD, BRNCHCL, BR>
  141         CHARACTER               :: COMSTR*1, IDTIME*23, FNAMEWHT*32
  142 !
  143 !------------------------------------------------------------------------>
  144 ! 1. Initialization
  145 !
  146         NDSM   = 20
  147         NDSI   = 10
  148         NDSE   = 6
  149         NDSO   = 6
  150 !
  151         NDSTRC = 6
  152         NTRACE = 10
  153                    
  154 !
  155 !------------------------------------------------------------------------>
  156 ! 2.  I/O Setup
  157 !
  158         J      = LEN_TRIM(FNMPRE)
  159         OPEN(NDSI,FILE=FNMPRE(:J)//'ww3_gint.inp',STATUS='OLD', ERR=2000,>
  160              IOSTAT=IERR)
  161         WRITE (NDSO,900)
  162 !
  163         CALL ITRACE ( NDSTRC, NTRACE )
  164 !------------------------------------------------------------------------>
  165 ! 3.  Read and process input file upto number of grids
 
 
                                                         ww3_gint.F90  page   4
 
 
  166 ! 3.a Get comment character
  167 !
  168         REWIND (NDSI)
  169         READ (NDSI,'(A)',END=2001,ERR=2002) COMSTR
  170         IF ( COMSTR .EQ. ' ' ) COMSTR = '$'
  171         WRITE (NDSO,901) COMSTR
  172 !
  173 ! 3.b Read starting time, time step and number of outputs
  174 !
  175         CALL NEXTLN ( COMSTR, NDSI, NDSE )
  176         READ (NDSI,*,END=2001,ERR=2002) TOUT, DTREQ, NOUT
  177         DTREQ  = MAX ( 0. , DTREQ )
  178         IF ( DTREQ.EQ.0 ) NOUT = 1
  179         NOUT   = MAX ( 1 , NOUT )
  180 !
  181         CALL STME21 ( TOUT , IDTIME )
  182         WRITE (NDSO,902) IDTIME, DTREQ, NOUT
  183 !
  184 ! 3.c Read number of grids and allocate memory
  185 !
  186         CALL NEXTLN ( COMSTR, NDSI, NDSE )
  187         READ (NDSI,*,END=2001,ERR=2002) NG
  188         WRITE (NDSO,903) NG
  189 !
  190         CALL W3NMOD (NG, 6, 6)
  191         CALL W3NDAT (    6, 6)
  192         CALL W3NAUX (    6, 6)
  193         CALL W3NINP (    6, 6)
  194         CALL W3NOUT(     6, 6)
  195 !
  196 ! 3.d Read file extensions for each of the grids and
  197 !     the grid information from the corresponding mod_def files
  198 !
  199         NOSWLL_MIN = 9999999
  200         CALL NEXTLN ( COMSTR, NDSI, NDSE )
  201 !
  202         DO IG = 1,NG
  203           READ (NDSI,*,END=2001,ERR=2002) GRIDS(IG)%FILEXT
  204           WRITE (NDSO,904) IG,GRIDS(IG)%FILEXT
  205 !
  206           CALL W3SETO( IG, 6, 6)
  207           CALL W3SETA( IG, 6, 6)
  208           CALL W3SETW( IG, 6, 6)
  209           CALL W3SETG( IG, 6, 6)
  210           CALL W3IOGR ('READ', NDSM, IG, GRIDS(IG)%FILEXT)
  211           WRITE (NDSO,905) NX, NY, GTYPE, ICLOSE
  212                                                 
  213           IF ( ICLOSE .EQ. ICLOSE_TRPL ) THEN
  214              WRITE(NDSE,*)'PROGRAM W3GRID_INTERP HAS NOT BEEN '//  &
  215                   'TESTED WITH TRIPOLE GRIDS. STOPPING NOW.'
  216              CALL EXTCDE ( 1 )
  217           END IF
  218                 
  219           IF ( IG .NE. NG .AND. NOSWLL_MIN .GE. OUTPTS(IG)%NOSWLL ) THEN
  220              NOSWLL_MIN = OUTPTS(IG)%NOSWLL
 
 
                                                         ww3_gint.F90  page   5
 
 
  221           END IF
  222 !
  223         END DO
  224 !
  225         IF ( NOSWLL_MIN .NE. OUTPTS(NG)%NOSWLL ) THEN
  226            WRITE (NDSO,907) NOSWLL_MIN, OUTPTS(NG)%NOSWLL
  227            NOSWLL_MIN = MIN (NOSWLL_MIN,OUTPTS(NG)%NOSWLL)
  228         END IF
  229               
  230         CALL NEXTLN ( COMSTR, NDSI, NDSE )
  231         READ (NDSI,'(I1)',END=2001,ERR=2002) EXTRA
  232                                                   
  233 !
  234 ! 3.e Allocate memory for integration map and initialize with grid status>
  235 !
  236         ALLOCATE(INT_MAP(NX,NY),MAP(NX,NY))
  237         INT_MAP = 0.0
  238 !        MAP = TRANSPOSE(MAPSTA)
  239         DO IX = 1,NX
  240           DO IY = 1,NY
  241             IF ( MAPSTA(IY,IX) .EQ. 0 ) THEN
  242               MAP(IX,IY) = -1
  243             END IF
  244           END DO
  245         END DO
  246 !
  247 !------------------------------------------------------------------------>
  248 ! 4.  Determine interpolation weights for output grids
  249 !
  250 ! 4.a Point to output grid and allocate space for interpolation weights
  251 !
  252         CALL W3SETG( NG, 6, 6)
  253         WRITE (NDSO,908) NSEA
  254 !
  255         ALLOCATE ( GR_INTS(NSEA) )
  256 !
  257         IF ( FLAGLL ) THEN
  258           IF ( MINVAL ( XGRD ) .LT. 0 .OR.                               >
  259                MAXVAL ( XGRD ) .GT. 180.0 ) L360 = .TRUE.
  260         END IF
  261 !
  262 ! 4.b Check if weight files exist or create it
  263 !
  264         FNAMEWHT='WHTGRIDINT.bin'
  265         OPEN (994,FILE=FNMPRE(:J)//TRIM(FNAMEWHT),FORM='UNFORMATTED',IOST>
  266         NSEA_FILE = 0
  267         IF (IERR.EQ.0) READ(994) NSEA_FILE ! basic security check ...
  268         IF (NSEA_FILE.EQ.NSEA) THEN
  269           DO ISEA = 1, NSEA
  270             READ(994) COUNTG
  271             ALLOCATE ( GR_INTS(ISEA)%IND_WTS(COUNTG),GR_INTS(ISEA)%GDID(C>
  272             DO IG = 1,COUNTG
  273               READ(994) GR_INTS(ISEA)%IND_WTS(IG)%AR
  274               READ(994) GR_INTS(ISEA)%GDID(IG)
  275               READ(994) COUNTF
 
 
                                                         ww3_gint.F90  page   6
 
 
  276               ALLOCATE ( GR_INTS(ISEA)%IND_WTS(IG)%IP(COUNTF),         &
  277                          GR_INTS(ISEA)%IND_WTS(IG)%JP(COUNTF),         &
  278                          GR_INTS(ISEA)%IND_WTS(IG)%WT(COUNTF) )
  279               DO I = 1,COUNTF
  280                 READ(994) GR_INTS(ISEA)%IND_WTS(IG)%IP(I)
  281                 READ(994) GR_INTS(ISEA)%IND_WTS(IG)%JP(I)
  282                 READ(994) GR_INTS(ISEA)%IND_WTS(IG)%WT(I)
  283               END DO
  284               READ(994) GR_INTS(ISEA)%IND_WTS(IG)%NP
  285             END DO ! IG
  286             READ(994) GR_INTS(ISEA)%NGRDS
  287           END DO ! ISEA
  288                        
  289         ELSE
  290           OPEN (994,FILE=FNMPRE(:J)//TRIM(FNAMEWHT),FORM='UNFORMATTED',IO>
  291                                                                           
  292 !
  293 ! 4.b Loop through the wet points
  294 !
  295         DO ISEA = 1, NSEA
  296 !
  297 !          IF (MOD(ISEA,NINT(REAL(NSEA)/100)).EQ.1)  &
  298 !            WRITE(6,*) 'Treating point ',ISEA,' out of ', NSEA
  299           IX = MAPSF(ISEA,1)
  300           IY = MAPSF(ISEA,2)
  301           DAREA = ABS(GSQRT(IY,IX))
  302 !
  303           ALLOCATE ( GR_INTS(ISEA)%IND_WTS(NG-1),GR_INTS(ISEA)%GDID(NG-1)>
  304 !
  305 ! 4.b.i Loop through the input grids for each wet point
  306 !
  307           COUNTG = 0
  308           DO IG = 1,NG-1
  309 !
  310 ! 4.b.ii Check if point is enclosed in grid domain
  311 !
  312             INGRID=.FALSE.
  313             IF (GRIDS(IG)%GTYPE .EQ. UNGTYPE) THEN
  314               ! Look for a triangle at the coarse cell center
  315               CALL IS_IN_UNGRID(IG, XGRD(IY,IX), YGRD(IY,IX),  &
  316                                                  ITOUT, IS, JS, RW)
  317               IF (ITOUT.GT.0) INGRID=.TRUE.
  318 !              ! If extrapolation activated, force to find if a triangles>
  319 !              ! the coarse grid cell even if there is no triangle in the>
  320               IF (EXTRA.GT.0) THEN
  321                 WRITE(991,'(2I6,2F9.4,I8,3I8,3F5.3)') IX,IY,XGRD(IY,IX), >
  322                 IF (ITOUT.EQ.0)  WRITE(992,*) IX,IY,ISEA,XGRD(IY,IX), YGR>
  323                 IF (ITOUT.EQ.0) THEN
  324                    CALL IS_IN_UNGRID2(IG, XGRD(IY,IX), YGRD(IY,IX), EXTRA>
  325                                                  ITOUT, IS, JS, RW)
  326                    WRITE(993,'(2I6,2F9.4,I8,3I8,3F6.3)') IX,IY,XGRD(IY,IX>
  327                 ENDIF
  328                 !IF (ITOUT.EQ.0) CALL IS_IN_UNGRID(IG, XGRD(IY,IX)+DX, YG>
  329                 !IF (ITOUT.GT.0) INGRID=.TRUE.
  330                 !IF (ITOUT.EQ.0) CALL IS_IN_UNGRID(IG, XGRD(IY,IX)-DX, YG>
 
 
                                                         ww3_gint.F90  page   7
 
 
  331                 !IF (ITOUT.GT.0) INGRID=.TRUE.
  332                 !IF (ITOUT.EQ.0) CALL IS_IN_UNGRID(IG, XGRD(IY,IX), YGRD(>
  333                 !IF (ITOUT.GT.0) INGRID=.TRUE.
  334                 !IF (ITOUT.EQ.0) CALL IS_IN_UNGRID(IG, XGRD(IY,IX), YGRD(>
  335                 !IF (ITOUT.GT.0) INGRID=.TRUE.
  336               END IF
  337             ELSE
  338               IF ( W3GRMP ( GRIDS(IG)%GSU, XGRD(IY,IX), YGRD(IY,IX), IS,
  339                    JS, RW ) ) INGRID=.TRUE.
  340               END IF
  341            IF (INGRID) THEN
  342 !
  343 ! 4.b.iii Check source grid resolution vs target grid resolution
  344 !         (averaging used for finer resolution source grids)
  345 !
  346              IF (GRIDS(IG)%GTYPE .EQ. UNGTYPE) THEN
  347                 SAREA = GRIDS(IG)%TRIA(ITOUT)
  348              ELSE
  349                DO I = 1,4
  350                  XCRNR(I) = GRIDS(IG)%XGRD(JS(I),IS(I))
  351                  YCRNR(I) = GRIDS(IG)%YGRD(JS(I),IS(I))
  352                END DO
  353                XCRNR(5) = XCRNR(1)
  354                YCRNR(5) = YCRNR(1)
  355                DO I = 1,4
  356                  IF ( ABS (XCRNR(I+1)-XCRNR(I)) .GT. 180. .AND.
  357                    GRIDS(IG)%ICLOSE .EQ.  ICLOSE_SMPL ) THEN
  358                    DT(I) = SQRT ( (ABS(XCRNR(I+1)-XCRNR(I))-360.)**2 +
  359                                       (YCRNR(I+1)-YCRNR(I))**2 )
  360                  ELSE
  361                    DT(I) = SQRT ( (XCRNR(I+1)-XCRNR(I))**2 +
  362                                   (YCRNR(I+1)-YCRNR(I))**2 )
  363                  END IF
  364                END DO
  365                SXT = 0.5*(DT(1)+DT(3))
  366                SYT = 0.5*(DT(2)+DT(4))
  367                SAREA = (SXT*SYT)
  368              END IF
  369              NS = NINT(DAREA/SAREA)
  370 !
  371              IF ( NS .LE. 2 .OR. GRIDS(IG)%GTYPE .EQ. UNGTYPE ) THEN
  372 ! FA: Quick fix for UNST type grids: always perform interpolation
  373 !     To be updated later ...
  374 !
  375 ! 4.b.iv Counting the contributing nodes to re-normalize the weights RW
  376 !
  377                 ALLOCATE ( TMP_INDX(4) )
  378                 COUNTF = 0
  379                 SUMWT = 0.0
  380                 DO I = 1,4
  381 ! The following two IF tests are separated because for triangles, JS(4)=I>
  382                   IF ( RW(I) .GT. 0.0 ) THEN
  383                     IF ( GRIDS(IG)%MAPSTA(JS(I),IS(I)) .NE. 0) THEN
  384                       COUNTF = COUNTF+1
  385                       TMP_INDX(COUNTF) = I
 
 
                                                         ww3_gint.F90  page   8
 
 
  386                       SUMWT = SUMWT + RW(I)
  387                     END IF
  388                   END IF
  389                 END DO
  390 !
  391 ! 4.b.v  Interpolating to target grid
  392 !
  393                 IF ( COUNTF .GT. 0 ) THEN
  394 ! Should use SAREA info to prevent the increment of COUNTG ...
  395 ! what about islands / land in triangle meshes? they are not part of the >
  396                   COUNTG = COUNTG + 1
  397                   IF (COUNTG.GT.1) THEN
  398                     IF (SAREA.LT.0.5*GR_INTS(ISEA)%IND_WTS(COUNTG-1)%AR) >
  399                        DO JG=1,COUNTG-1
  400                          DEALLOCATE (GR_INTS(ISEA)%IND_WTS(JG)%IP)
  401                          DEALLOCATE (GR_INTS(ISEA)%IND_WTS(JG)%JP)
  402                          DEALLOCATE (GR_INTS(ISEA)%IND_WTS(JG)%WT)
  403                          END DO
  404                        COUNTG=1
  405                        END IF
  406                     END IF
  407                           
  408                   GR_INTS(ISEA)%IND_WTS(COUNTG)%AR = SAREA
  409                   GR_INTS(ISEA)%GDID(COUNTG) = IG
  410                   INT_MAP(IX,IY) = REAL( IG )
  411                                              
  412                   ALLOCATE ( GR_INTS(ISEA)%IND_WTS(COUNTG)%IP(COUNTF),   >
  413                              GR_INTS(ISEA)%IND_WTS(COUNTG)%JP(COUNTF),   >
  414                              GR_INTS(ISEA)%IND_WTS(COUNTG)%WT(COUNTF) )
  415                   DO I = 1,COUNTF
  416                     GR_INTS(ISEA)%IND_WTS(COUNTG)%IP(I) = IS(TMP_INDX(I))
  417                     GR_INTS(ISEA)%IND_WTS(COUNTG)%JP(I) = JS(TMP_INDX(I))
  418                     GR_INTS(ISEA)%IND_WTS(COUNTG)%WT(I) = RW(TMP_INDX(I))>
  419                   END DO
  420                   GR_INTS(ISEA)%IND_WTS(COUNTG)%NP = COUNTF
  421                 END IF
  422                 DEALLOCATE ( TMP_INDX )
  423 !
  424               ELSE
  425 !
  426 ! 4.b.vi Find the averaging points for higher resolution grid
  427 !        Step 1 : Compute the corners of the cell
  428 !
  429                 X0 = XGRD(IY,IX)
  430                 Y0 = YGRD(IY,IX)
  431                 IF ( IX .GT. 1 .AND. IX .LT. NX .AND. IY .GT. 1          >
  432                      .AND. IY .LT. NY ) THEN
  433                   XT = XGRD(IY-1,IX+1)
  434                   YT = YGRD(IY-1,IX+1)
  435                   IF ( ABS(XT-X0) .GT. 270 ) THEN
  436                     XT = XT - SIGN(360.,XT-X0)
  437                     END IF
  438                   XCRNR(1) = 0.5*(XT+X0)
  439                   YCRNR(1) = 0.5*(YT+Y0)
  440                   XT = XGRD(IY+1,IX+1)
 
 
                                                         ww3_gint.F90  page   9
 
 
  441                   YT = YGRD(IY+1,IX+1)
  442                   IF ( ABS(XT-X0) .GT. 270 ) THEN
  443                     XT = XT - SIGN(360.,XT-X0)
  444                     END IF
  445                   XCRNR(2) = 0.5*(XT+X0)
  446                   YCRNR(2) = 0.5*(YT+Y0)
  447                   XT = XGRD(IY+1,IX-1)
  448                   YT = YGRD(IY+1,IX-1)
  449                   IF ( ABS(XT-X0) .GT. 270 ) THEN
  450                     XT = XT - SIGN(360.,XT-X0)
  451                     END IF
  452                   XCRNR(3) = 0.5*(XT+X0)
  453                   YCRNR(3) = 0.5*(YT+Y0)
  454                   XT = XGRD(IY-1,IX-1)
  455                   YT = YGRD(IY-1,IX-1)
  456                   IF ( ABS(XT-X0) .GT. 270 ) THEN
  457                     XT = XT - SIGN(360.,XT-X0)
  458                     END IF
  459                   XCRNR(4) = 0.5*(XT+X0)
  460                   YCRNR(4) = 0.5*(YT+Y0)
  461                 ELSEIF ( IX .EQ. 1 ) THEN
  462                   IF ( IY .EQ. 1 ) THEN
  463                     XT = XGRD(IY+1,IX+1)
  464                     YT = YGRD(IY+1,IX+1)
  465                     IF ( ABS(XT-X0) .GT. 270 ) THEN
  466                       XT = XT - SIGN(360.,XT-X0)
  467                       END IF
  468                     XCRNR(2) = 0.5*(XT+X0)
  469                     YCRNR(2) = 0.5*(YT+Y0)
  470                     XCRNR(4) = 2*X0 - XCRNR(2)
  471                     YCRNR(4) = 2*Y0 - YCRNR(2)
  472                     XCRNR(3) = X0 - (YCRNR(2)-Y0)
  473                     YCRNR(3) = Y0 + (XCRNR(2)-X0)
  474                     XCRNR(1) = 2*X0 - XCRNR(3)
  475                     YCRNR(1) = 2*Y0 - YCRNR(3)
  476                   ELSEIF ( IY .EQ. NY ) THEN
  477                     XT = XGRD(IY-1,IX+1)
  478                     YT = YGRD(IY-1,IX+1)
  479                     IF ( ABS(XT-X0) .GT. 270 ) THEN
  480                       XT = XT - SIGN(360.,XT-X0)
  481                     END IF
  482                     XCRNR(1) = 0.5*(XT+X0)
  483                     YCRNR(1) = 0.5*(YT+Y0)
  484                     XCRNR(3) = 2*X0 - XCRNR(1)
  485                     YCRNR(3) = 2*Y0 - YCRNR(1)
  486                     XCRNR(2) = X0 - (Y0-YCRNR(1))
  487                     YCRNR(2) = Y0 + (X0-XCRNR(1))
  488                     XCRNR(4) = 2*X0 - XCRNR(2)
  489                     YCRNR(4) = 2*Y0 - YCRNR(2)
  490                   ELSE
  491                     XT = XGRD(IY-1,IX+1)
  492                     YT = YGRD(IY-1,IX+1)
  493                     IF ( ABS(XT-X0) .GT. 270 ) THEN
  494                       XT = XT - SIGN(360.,XT-X0)
  495                     END IF
 
 
                                                         ww3_gint.F90  page  10
 
 
  496                     XCRNR(1) = 0.5*(XT+X0)
  497                     YCRNR(1) = 0.5*(YT+Y0)
  498                     XT = XGRD(IY+1,IX+1)
  499                     YT = YGRD(IY+1,IX+1)
  500                     IF ( ABS(XT-X0) .GT. 270 ) THEN
  501                       XT = XT - SIGN(360.,XT-X0)
  502                     END IF
  503                     XCRNR(2) = 0.5*(XT+X0)
  504                     YCRNR(2) = 0.5*(YT+Y0)
  505                     XCRNR(3) = 2*X0 - XCRNR(1)
  506                     YCRNR(3) = 2*Y0 - YCRNR(1)
  507                     XCRNR(4) = 2*X0 - XCRNR(2)
  508                     YCRNR(4) = 2*Y0 - YCRNR(2)
  509                   ENDIF
  510                 ELSEIF ( IX .EQ. NX ) THEN
  511                   IF ( IY .EQ. 1 ) THEN
  512                     XT = XGRD(IY+1,IX-1)
  513                     YT = YGRD(IY+1,IX-1)
  514                     IF ( ABS(XT-X0) .GT. 270 ) THEN
  515                       XT = XT - SIGN(360.,XT-X0)
  516                     END IF
  517                     XCRNR(3) = 0.5*(XT+X0)
  518                     YCRNR(3) = 0.5*(YT+Y0)
  519                     XCRNR(2) = X0 - (YCRNR(3)-Y0)
  520                     YCRNR(2) = Y0 + (XCRNR(3)-X0)
  521                     XCRNR(1) = 2*X0 - XCRNR(3)
  522                     YCRNR(1) = 2*Y0 - YCRNR(3)
  523                     XCRNR(4) = 2*X0 - XCRNR(2)
  524                     YCRNR(4) = 2*Y0 - YCRNR(2)
  525                   ELSEIF ( IY .EQ. NY ) THEN
  526                     XT = XGRD(IY-1,IX-1)
  527                     YT = YGRD(IY-1,IX-1)
  528                     IF ( ABS(XT-X0) .GT. 270 ) THEN
  529                       XT = XT - SIGN(360.,XT-X0)
  530                     END IF
  531                     XCRNR(4) = 0.5*(XT+X0)
  532                     YCRNR(4) = 0.5*(YT+Y0)
  533                     XCRNR(3) = X0 - (YCRNR(4)-Y0)
  534                     YCRNR(3) = Y0 + (XCRNR(4)-X0)
  535                     XCRNR(1) = 2*X0 - XCRNR(3)
  536                     YCRNR(1) = 2*Y0 - YCRNR(3)
  537                     XCRNR(2) = 2*X0 - XCRNR(4)
  538                     YCRNR(2) = 2*Y0 - YCRNR(4)
  539                   ELSE
  540                     XT = XGRD(IY+1,IX-1)
  541                     YT = YGRD(IY+1,IX-1)
  542                     IF ( ABS(XT-X0) .GT. 270 ) THEN
  543                       XT = XT - SIGN(360.,XT-X0)
  544                     END IF
  545                     XCRNR(3) = 0.5*(XT+X0)
  546                     YCRNR(3) = 0.5*(YT+Y0)
  547                     XT = XGRD(IY-1,IX-1)
  548                     YT = YGRD(IY-1,IX-1)
  549                     IF ( ABS(XT-X0) .GT. 270 ) THEN
  550                       XT = XT - SIGN(360.,XT-X0)
 
 
                                                         ww3_gint.F90  page  11
 
 
  551                     END IF
  552                     XCRNR(4) = 0.5*(XT+X0)
  553                     YCRNR(4) = 0.5*(YT+Y0)
  554                     XCRNR(1) = 2*X0 - XCRNR(3)
  555                     YCRNR(1) = 2*Y0 - YCRNR(3)
  556                     XCRNR(2) = 2*X0 - XCRNR(4)
  557                     YCRNR(2) = 2*Y0 - YCRNR(4)
  558                   ENDIF
  559                 ELSE
  560                   IF ( IY .EQ. 1 ) THEN
  561                     XT = XGRD(IY+1,IX+1)
  562                     YT = YGRD(IY+1,IX+1)
  563                     IF ( ABS(XT-X0) .GT. 270 ) THEN
  564                       XT = XT - SIGN(360.,XT-X0)
  565                     END IF
  566                     XCRNR(2) = 0.5*(XT+X0)
  567                     YCRNR(2) = 0.5*(YT+Y0)
  568                     XT = XGRD(IY+1,IX-1)
  569                     YT = YGRD(IY+1,IX-1)
  570                     IF ( ABS(XT-X0) .GT. 270 ) THEN
  571                       XT = XT - SIGN(360.,XT-X0)
  572                     END IF
  573                     XCRNR(3) = 0.5*(XT+X0)
  574                     YCRNR(3) = 0.5*(YT+Y0)
  575                     XCRNR(4) = 2*X0 - XCRNR(2)
  576                     YCRNR(4) = 2*Y0 - YCRNR(2)
  577                     XCRNR(1) = 2*X0 - XCRNR(3)
  578                     YCRNR(1) = 2*Y0 - YCRNR(3)
  579                   ELSE
  580                     XT = XGRD(IY-1,IX-1)
  581                     YT = YGRD(IY-1,IX-1)
  582                     IF ( ABS(XT-X0) .GT. 270 ) THEN
  583                       XT = XT - SIGN(360.,XT-X0)
  584                     END IF
  585                     XCRNR(4) = 0.5*(XT+X0)
  586                     YCRNR(4) = 0.5*(YT+Y0)
  587                     XT = XGRD(IY-1,IX+1)
  588                     YT = YGRD(IY-1,IX+1)
  589                     IF ( ABS(XT-X0) .GT. 270 ) THEN
  590                       XT = XT - SIGN(360.,XT-X0)
  591                     END IF
  592                     XCRNR(1) = 0.5*(XT+X0)
  593                     YCRNR(1) = 0.5*(YT+Y0)
  594                     XCRNR(2) = 2*X0 - XCRNR(4)
  595                     YCRNR(2) = 2*Y0 - YCRNR(4)
  596                     XCRNR(3) = 2*X0 - XCRNR(1)
  597                     YCRNR(3) = 2*Y0 - YCRNR(1)
  598                   END IF
  599                 END IF
  600                 BRNCHCL = .FALSE.
  601                 BRNCHCR = .FALSE.
  602                 IF ( FLAGLL .AND. ICLOSE .EQ. ICLOSE_SMPL ) THEN
  603                   IF ( L360 ) THEN
  604                     IF ( MINVAL ( XCRNR(1:4) ) .LT.   0.0 ) BRNCHCL = .TR>
  605                     IF ( MAXVAL ( XCRNR(1:4) ) .GT. 360.0 ) BRNCHCR = .TR>
 
 
                                                         ww3_gint.F90  page  12
 
 
  606                   ELSE
  607                     IF ( MINVAL ( XCRNR(1:4) ) .LT. -180.0 ) BRNCHCL = .T>
  608                     IF ( MAXVAL ( XCRNR(1:4) ) .GT.  180.0 ) BRNCHCR = .T>
  609                   END IF
  610                 END IF
  611 !
  612 !        Step 2 : Loop through source grid to find all active points in c>
  613 !
  614 !FA : why only *5 ???...
  615 !
  616                 ALLOCATE ( TMP_INDX(NS*5) )
  617                 COUNTF = 0
  618                 DO I = 1, GRIDS(IG)%NSEA
  619                   IXT = GRIDS(IG)%MAPSF(I,1)
  620                   IYT = GRIDS(IG)%MAPSF(I,2)
  621                   XT = GRIDS(IG)%XGRD(IYT,IXT)
  622                   YT = GRIDS(IG)%YGRD(IYT,IXT)
  623 !
  624                   IF ( FLAGLL ) THEN
  625                     IF (  L360 ) THEN
  626                       IF ( XT .LT. 0 ) XT = XT + 360.
  627                     ELSE
  628                       IF ( XT .GT. 180. ) XT = XT - 360.
  629                     END IF
  630                   END IF
  631                   INGRD = W3CKCL (FLAGLL,XT,YT,4,XCRNR,YCRNR,LPLC)
  632                   IF ( INGRD ) THEN
  633                     COUNTF = COUNTF+1
  634                     TMP_INDX(COUNTF) = I
  635                   ELSEIF ( BRNCHCL .AND. GRIDS(IG)%ICLOSE                >
  636                            .EQ. ICLOSE_SMPL ) THEN
  637                     XTT = XT - 360.0
  638                     INGRD = W3CKCL (FLAGLL,XTT,YT,4,XCRNR,YCRNR,LPLC)
  639                     IF ( INGRD ) THEN
  640                       COUNTF = COUNTF+1
  641                       TMP_INDX(COUNTF) = I
  642                     END IF
  643                   ELSEIF ( BRNCHCR .AND. GRIDS(IG)%ICLOSE                >
  644                           .EQ. ICLOSE_SMPL ) THEN
  645                     XTT = XT + 360.0
  646                     INGRD = W3CKCL (FLAGLL,XTT,YT,4,XCRNR,YCRNR,LPLC)
  647                     IF ( INGRD ) THEN
  648                       COUNTF = COUNTF+1
  649                       TMP_INDX(COUNTF) = I
  650                     END IF
  651                   END IF
  652                 END DO
  653 !
  654 !        Step 3 : Save interior points for equal wt. interpolation (avera>
  655 !
  656                 IF ( COUNTF .NE. 0 ) THEN
  657                   COUNTG = COUNTG + 1
  658                   GR_INTS(ISEA)%GDID(COUNTG) = IG
  659                   INT_MAP(IX,IY) = REAL( IG )
  660                   ALLOCATE ( GR_INTS(ISEA)%IND_WTS(COUNTG)%IP(COUNTF),   >
 
 
                                                         ww3_gint.F90  page  13
 
 
  661                              GR_INTS(ISEA)%IND_WTS(COUNTG)%JP(COUNTF),   >
  662                              GR_INTS(ISEA)%IND_WTS(COUNTG)%WT(COUNTF) )
  663                   DO I = 1,COUNTF
  664                     IXT = GRIDS(IG)%MAPSF(TMP_INDX(I),1)
  665                     IYT = GRIDS(IG)%MAPSF(TMP_INDX(I),2)
  666                     GR_INTS(ISEA)%IND_WTS(COUNTG)%IP(I) = IXT
  667                     GR_INTS(ISEA)%IND_WTS(COUNTG)%JP(I) = IYT
  668                     GR_INTS(ISEA)%IND_WTS(COUNTG)%WT(I) = 1./( REAL(COUNT>
  669                   END DO
  670                   GR_INTS(ISEA)%IND_WTS(COUNTG)%NP = COUNTF
  671                 END IF
  672                 DEALLOCATE ( TMP_INDX )
  673 !
  674               END IF  ! End of check for grid resolution
  675 !
  676             END IF    ! End of check for point inside grid
  677 !
  678           END DO      ! End of loop through all input grids
  679 !
  680           GR_INTS(ISEA)%NGRDS = COUNTG
  681 !
  682 ! 4.b.vii Check to see if interpolation weights found.
  683 !        Status of output points with / without weights set in MAPST2
  684 !        using the next available bit
  685 !
  686           IF ( GR_INTS(ISEA)%NGRDS .EQ. 0 ) THEN
  687             MAPINT = 1
  688             MAPST2(IY,IX) = MAPST2(IY,IX) + MAPINT*16
  689             MAPSTA(IY,IX) = -ABS ( MAPSTA(IY,IX) )
  690           END IF
  691 !
  692         END DO       ! End of loop through all wet points
  693 !
  694 ! Now dumps the coefficients to file ...
  695         WRITE(994) NSEA
  696         DO ISEA = 1, NSEA
  697           COUNTG = GR_INTS(ISEA)%NGRDS
  698           WRITE(994) COUNTG
  699           DO IG = 1,COUNTG
  700             WRITE(994) GR_INTS(ISEA)%IND_WTS(IG)%AR
  701             WRITE(994) GR_INTS(ISEA)%GDID(IG)
  702             COUNTF = GR_INTS(ISEA)%IND_WTS(IG)%NP
  703             WRITE(994) COUNTF
  704             DO I = 1,COUNTF
  705               WRITE(994) GR_INTS(ISEA)%IND_WTS(IG)%IP(I)
  706               WRITE(994) GR_INTS(ISEA)%IND_WTS(IG)%JP(I)
  707               WRITE(994) GR_INTS(ISEA)%IND_WTS(IG)%WT(I)
  708             END DO
  709             WRITE(994) GR_INTS(ISEA)%IND_WTS(IG)%NP
  710           END DO ! IG
  711           WRITE(994) GR_INTS(ISEA)%NGRDS
  712         END DO ! ISEA
  713         END IF ! NSEA.EQ.NSEA_FILE	
  714         CLOSE(994)
  715 !
 
 
                                                         ww3_gint.F90  page  14
 
 
  716 ! 4.c Print Interpolation grids map
  717 !
  718         IX = 1+NX/24
  719         IY = 1+NY/24
  720         CALL PRTBLK ( NDSO, NX, NY, NX, INT_MAP, MAP, -1, 1., 1, NX, IX, >
  721                       NY, IY, 'Grid Interpolation Map', ' ' )
  722 !
  723 !------------------------------------------------------------------------>
  724 ! 5   Output interpolations
  725 !
  726 ! 5.a Set-up dimensions for target grid outputs and allocate file pointers
  727 !
  728         CALL W3SETA(NG, 6, 6)
  729         CALL W3DIMA(NG, 6, 6, .TRUE. )
  730         CALL W3DIMW(NG, 6, 6, .TRUE. )
  731         ALLOCATE(FIDOUT(NG))
  732         DO IG = 1,NG
  733           FIDOUT(IG) = 30 + (IG-1)*10
  734         END DO
  735 !
  736 ! 5.b Initialize and read the first set of fields for base grids
  737 !
  738         DO IG = 1,NG-1
  739           CALL W3SETO( IG, 6, 6)
  740           CALL W3IOGO('READ',FIDOUT(IG),IOTST,IG)
  741           IF ( IOTST .NE. 0 ) THEN
  742             GO TO 2111
  743           ENDIF
  744         END DO
  745 !
  746 ! 5.c Setup the output flag options for the target grid
  747 !
  748         WRITE (NDSO,910)
  749         DO I = 1, NOGRP
  750           OUTPTS(NG)%OUT1%FLOGRD(I,:) = OUTPTS(1)%OUT1%FLOGRD(I,:)
  751           WRITE (NDSO,911) I
  752           IF (I.LT.9) THEN
  753             WRITE (NDSO, 912) (OUTPTS(NG)%OUT1%FLOGRD(I,J),J=1,NGRPP)
  754            ELSE
  755             WRITE (NDSO, 913)
  756           END IF
  757         END DO
  758         WRITE (NDSO, 915)
  759 !
  760 !     Print output flags in human readable from. Mark
  761 !     groups that do not make sense to interpolate to
  762 !     target grid (e.g. Groups 9, 10).
  763 !
  764         DO I=1, NOGRP
  765          DO J=1, NGRPP
  766            IF ( OUTPTS(NG)%OUT1%FLOGRD(I,J) ) THEN
  767              IF ( I .EQ. 4 .AND. J .EQ. 8 ) THEN
  768                WRITE (NDSO, 916) I,IDOUT(I,J), '*** NOT IMPLEMENTED ***'
  769                OUTPTS(NG)%OUT1%FLOGRD(I,J) = .FALSE.
  770              ELSE IF ( I .LE. 8 ) THEN
 
 
                                                         ww3_gint.F90  page  15
 
 
  771                WRITE (NDSO, 916) I,IDOUT(I,J), ' '
  772              ELSE
  773                WRITE (NDSO, 916) I,IDOUT(I,J), '*** NOT IMPLEMENTED ***'
  774                OUTPTS(NG)%OUT1%FLOGRD(I,J) = .FALSE.
  775              END IF
  776            END IF
  777          END DO
  778         END DO
  779         WRITE (NDSO, 915)
  780 !
  781 ! 5.d Carry out interpolation in an infinite loop till appropriate
  782 !     time steps are interpolated
  783 !
  784         IOUT = 0
  785 !
  786         DO
  787           DTEST = DSEC21 ( WDATAS(1)%TIME, TOUT )
  788           IF ( DTEST .GT. 0. ) THEN
  789             DO IG = 1,NG-1
  790               CALL W3IOGO('READ',FIDOUT(IG),IOTST,IG)
  791               IF ( IOTST .NE. 0 ) THEN
  792                 GO TO 2111
  793               ENDIF
  794             END DO
  795             CYCLE
  796           ENDIF
  797           IF ( DTEST .LT. 0. ) THEN
  798             CALL TICK21 ( TOUT , DTREQ )
  799             CYCLE
  800           END IF
  801 !
  802           IOUT = IOUT + 1
  803           CALL STME21 ( TOUT, IDTIME)
  804           WRITE (NDSO,914) IDTIME
  805 !
  806           WDATAS(NG)%TIME = WDATAS(1)%TIME
  807           CALL W3SETO(NG, 6, 6)
  808           CALL W3SETG(NG, 6, 6)
  809           CALL W3SETA(NG, 6, 6)
  810           CALL W3SETW(NG, 6, 6)
  811 !
  812           CALL W3EXGI ( NG-1, NSEA, NOSWLL_MIN )
  813 !
  814           CALL TICK21 ( TOUT , DTREQ )
  815           IF ( IOUT .GE. NOUT ) EXIT
  816         END DO
  817         GOTO 2222
  818 !
  819 !------------------------------------------------------------------------>
  820 ! Escape locations read errors :
  821 !
  822  2000   CONTINUE
  823         WRITE (NDSE,1000) IERR
  824         CALL EXTCDE ( 1 )
  825 !
 
 
                                                         ww3_gint.F90  page  16
 
 
  826  2001   CONTINUE
  827         WRITE(NDSE,1001)
  828         CALL EXTCDE ( 2 )
  829 !
  830  2002   CONTINUE
  831         WRITE(NDSE,1002) IERR
  832         CALL EXTCDE ( 3 )
  833 !
  834  2111   CONTINUE
  835         WRITE(NDSO,950)
  836  2222   CONTINUE
  837         WRITE(NDSO,999)
  838 !
  839 !------------------------------------------------------------------------>
  840 ! Formats
  841 !
  842  900    FORMAT (/15X,'    *** WAVEWATCH III Grid interpolation ***    '/ >
  843                15X,'==============================================='/)
  844  901    FORMAT ( '  Comment character is ''',A,''''/)
  845  902    FORMAT ( '  Time Information : '/                                >
  846                  '---------------------------------------------'/        >
  847                  '    Starting Time      : ',A/                          >
  848                  '    Interval (in sec)  : ',F10.2/                      >
  849                  '    Number of requests : ',I4/                         >
  850                  '---------------------------------------------')
  851  903    FORMAT ( '  Number of grids (including output grid) =',I3/)
  852  904    FORMAT ( /'  Extension for grid ',I3,' is --> ',A10/)
  853  905    FORMAT ( '    Grid Particulars are : '/                          >
  854                  '      Dimensions =  ',2(I9,2X)/                        >
  855                  '       Grid Type = ',I3,'  ==> 1 Rect, 2 Curv, 3 Unstr'>
  856                  '    Grid Closure = ',I3,'  ==> 0 None, 1 Simple, 2 Trip>
  857  907    FORMAT ( /' NOTE :  The no. of swell partitions from input and', >
  858                     ' target grids do not match',/                       >
  859                   '  The Min. no. of partitions from input grids =',I5/  >
  860                   '        The no. of partitions for target grid =',I5/  >
  861                   '  Interpolation will be limited to the smaller',      >
  862                   ' number of the partitions,',/                         >
  863                   '  rest will be marked undefined.'                     >
  864  908    FORMAT (/'  Preparing interpolation weights for output grid ' /  >
  865                  '  Total number of wet points for interpolation ',I6/)
  866  909    FORMAT (/'  *** WARNING !! No interpolation points at ',2(I5)/)
  867  910    FORMAT (/'  Interpolating fields .... '/)
  868  911    FORMAT ('     Output group   ', I5)
  869  912    FORMAT ('     Output variable flags are -> ',7(5L2,1X))
  870  913    FORMAT ('     Output variables skipped')
  871  914    FORMAT ( '        OUTPUT TIME : ',A)
  872  915    FORMAT ( '   ------------------------------------------------')
  873  916    FORMAT ( I5,A,2X,A)
  874  950    FORMAT (/'  End of file reached'/)
  875  999    FORMAT (/15X,'    *** End of Grid interpolation Routine ***    '/>
  876                  15X,'==============================================='/)
  877 !
  878  1000   FORMAT (/' *** ERROR IN WAVEGRID_INTERP : '/                     >
  879                  '     ERROR IN OPENING INPUT FILE'/                     >
  880                  '     IOSTAT =',I5/)
 
 
                                                         ww3_gint.F90  page  17
 
 
  881  1001   FORMAT (/' *** ERROR IN WAVEGRID_INTERP : '/                     >
  882                  '     PREMATURE END IN INPUT FILE'/)
  883  1002   FORMAT (/' *** ERROR IN WAVEGRID_INTERP : '/                     >
  884                  '     ERROR IN READING FROM INPUT FILE'/                >
  885                  '     IOSTAT =',I5/)
  886 !
  887 !/
  888 !/ Internal Subroutine
  889 !/
  890 !/ Internal Subroutine W3EXGI ------------------------------------------->
  891 !/
  892         CONTAINS
  893 !/ ---------------------------------------------------------------------->
  894         SUBROUTINE W3EXGI ( NGRD, NSEA, NOSWLL_MIN )
  895 !/                  +-----------------------------------+
  896 !/                  | WAVEWATCH-III           NOAA/NCEP |
  897 !/                  |             A. Chawla             |
  898 !/                  |                        FORTRAN 90 |
  899 !/                  | Last update :         27-Aug-2015 |
  900 !/                  +-----------------------------------+
  901 !/
  902 !/    09-Jul-2009 : Original code                       ( version 3.14 )
  903 !/    21-Feb-2013 : Modified to new output structure    ( version 4.11 )
  904 !/    30-Apr-2014 : Add group 3                         ( version 5.00 )
  905 !/    27-Aug-2015 : ice thick. and floe added as output ( version 5.10 )
  906 !/
  907 !   1. Purpose :
  908 !
  909 !      Perform actual output of interpolated data.
  910 !
  911 !   3. Parameters :
  912 !
  913 !   4. Subroutines used :
  914 !
  915 !      Name      Type  Module   Description
  916 !     ----------------------------------------------------------------
  917 !        W3IOGO    Subr. W3IOGOMD Reading/writing raw gridded data file.
  918 !     ----------------------------------------------------------------
  919 !
  920 !   5. Called by :
  921 !
  922 !      Subroutine it resides in
  923 !
  924 !   6. Error messages :
  925 !
  926 !      None.
  927 !
  928 !   7. Remarks :
  929 !
  930 !   8. Structure :
  931 !
  932 !      See source code.
  933 !
  934 !   9. Switches :
  935 !
 
 
                                                         ww3_gint.F90  page  18
 
 
  936 !      10. Source code :
  937 !
  938 !/ ---------------------------------------------------------------------->
  939         USE W3ADATMD
  940         USE W3WDATMD
  941         USE W3ODATMD, ONLY: NOGE
  942         USE W3IOGOMD, ONLY: W3IOGO
  943         USE W3GDATMD, ONLY: E3DF, NK
  944 !/ ---------------------------------------------------------------------->
  945 !/ Parameter List
  946 !/
  947         INTEGER, INTENT(IN) :: NGRD, NSEA, NOSWLL_MIN
  948 !/
  949 !/ Local Parameters
  950 !/
  951         INTEGER       :: ISEA, GSEA, IG, IGRID, IPTS, IGX, IGY, IX,    &
  952                          IY, ISWLL, ICAP, IBED, IFREQ, IK
  953         INTEGER       :: MAPINT, MAPICE, MAPDRY, MAPMSK, MAPLND,       &
  954                          NMAPICE, NMAPDRY, NMAPMSK, NMAPLND,           &
  955                          LMAPICE, LMAPDRY, LMAPMSK, LMAPLND,           &
  956                          MAPICET, MAPDRYT, MAPMSKT, MAPLNDT
  957         INTEGER       :: SUMGRD
  958         REAL          :: VAR1, VAR2, WT
  959 ! Local group 1 variables
  960         REAL          :: DWAUX, CXAUX, CYAUX, UAAUX, UDAUX, ASAUX,     &
  961                          WLVAUX, ICEAUX, ICEHAUX, ICEFAUX, BERGAUX,    &
  962                          SED_D50AUX, SUMWT1(NOGE(1))
  963 ! Local group 2 variables
  964         REAL          :: HSAUX, WLMAUX, T02AUX, T0M1AUX, T01AUX,       &
  965                          FP0AUX, THMAUX1, THMAUX2, THSAUX, THP0AUX1,   &
  966                          THP0AUX2, HSIGAUX, STMAXEAUX,STMAXDAUX,       &
  967                          HMAXEAUX, HCMAXEAUX, HMAXDAUX, HCMAXDAUX,     &
  968                          WBTAUX, SUMWT2(NOGE(2))
  969 ! Local group 3 variables
  970         REAL          :: EFAUX(E3DF(2,1):E3DF(3,1)),                   &
  971                          TH1MAUX(E3DF(2,2):E3DF(3,2)),                 &
  972                          STH1MAUX(E3DF(2,3):E3DF(3,3)),                &
  973                          TH2MAUX(E3DF(2,4):E3DF(3,4)),                 &
  974                          STH2MAUX(E3DF(2,5):E3DF(3,5)), WNAUX(1:NK),   &
  975                          SUMWT3A(E3DF(2,1):E3DF(3,1)),                 &
  976                          SUMWT3B(E3DF(2,2):E3DF(3,2)),                 &
  977                          SUMWT3C(E3DF(2,3):E3DF(3,3)),                 &
  978                          SUMWT3D(E3DF(2,4):E3DF(3,4)),                 &
  979                          SUMWT3E(E3DF(2,5):E3DF(3,5)),                 &
  980                          SUMWT3F(1:NK)
  981 ! Local group 4 variables
  982         REAL          :: PHSAUX(0:NOSWLL_MIN), PTPAUX(0:NOSWLL_MIN),   &
  983                          PLPAUX(0:NOSWLL_MIN), PSIAUX(0:NOSWLL_MIN),   &
  984                          PWSAUX(0:NOSWLL_MIN), PDIRAUX1(0:NOSWLL_MIN), &
  985                          PWSTAUX, PDIRAUX2(0:NOSWLL_MIN),              &
  986                          PTHP0AUX1(0:NOSWLL_MIN),                      &
  987                          PTHP0AUX2(0:NOSWLL_MIN),                      &
  988                          PQPAUX(0:NOSWLL_MIN), PPEAUX(0:NOSWLL_MIN),   &
  989                          PGWAUX(0:NOSWLL_MIN), PSWAUX(0:NOSWLL_MIN),   &
  990                          PTM1AUX(0:NOSWLL_MIN), PT1AUX(0:NOSWLL_MIN), &
 
 
                                                         ww3_gint.F90  page  19
 
 
  991                          PT2AUX(0:NOSWLL_MIN), PEPAUX(0:NOSWLL_MIN),   &
  992                          SUMWT4(NOGE(4),0:NOSWLL_MIN)
  993 ! Local group 5 variables
  994         REAL          :: USTAUX1, USTAUX2, CHARNAUX, CGEAUX,           &
  995                          PHIAWAUX, TAUWIXAUX, TAUWIYAUX, TAUWNXAUX,    &
  996                          TAUWNYAUX, WHITECAPAUX(4), SUMWT5(NOGE(5)),   &
  997                          SUMWTC(4)
  998 ! Local group 6 variables
  999         REAL          :: SXXAUX, SYYAUX, SXYAUX, TAUOXAUX, TAUOYAUX,   &
 1000                          BHDAUX, PHIOCAUX, TUSXAUX, TUSYAUX, USSXAUX,  &
 1001                          USSYAUX, PRMSAUX, TPMSAUX, SUMWT6(NOGE(6)),   &
 1002                          TAUICEAUX(2), PHICEAUX,                       &
 1003                          US3DAUX(2*NK), SUMWT68(2*NK),                 &
 1004                          P2SMSAUX(P2MSF(2):P2MSF(3)),                  &
 1005                          SUMWT69(P2MSF(2):P2MSF(3)),                   &
 1006                          USSPAUX(2*NK), SUMWT612(2*NK)
 1007 ! Local Group 7 variables
 1008         REAL          :: ABAAUX, ABDAUX, UBAAUX, UBDAUX, PHIBBLAUX,    &
 1009                          BEDFORMSAUX(3), TAUBBLAUX(2),                 &
 1010                          SUMWT7(NOGE(7)), SUMWTB(3)
 1011 ! Local group 8 variables
 1012         REAL          :: MSSXAUX, MSSYAUX, MSCXAUX, MSCYAUX, MSSDAUX1, &
 1013                          MSSDAUX2, MSCDAUX1, MSCDAUX2, QPAUX,          &
 1014                          SUMWT8(NOGE(8))
 1015 !/
 1016         LOGICAL       :: ACTIVE
 1017         LOGICAL       :: USEGRID(NGRD)
 1018 !/
 1019 !
 1020 !-------------------------------------------------------------------
 1021 ! 1.  Preparations
 1022 !
 1023 ! Group 1 Variables
 1024 !
 1025         DW       = UNDEF
 1026         CX       = UNDEF
 1027         CY       = UNDEF
 1028         UA       = UNDEF
 1029         UD       = UNDEF
 1030         AS       = UNDEF
 1031         WLV      = UNDEF
 1032         ICE      = UNDEF
 1033         BERG     = UNDEF
 1034 !
 1035 ! Group 2 variables
 1036 !
 1037         HS       = UNDEF
 1038         WLM      = UNDEF
 1039         T02      = UNDEF
 1040         T0M1     = UNDEF
 1041         T01      = UNDEF
 1042         FP0      = UNDEF
 1043         THM      = UNDEF
 1044         THS      = UNDEF
 1045         THP0     = UNDEF
 
 
                                                         ww3_gint.F90  page  20
 
 
 1046         HSIG     = UNDEF
 1047         STMAXE   = UNDEF
 1048         STMAXD   = UNDEF
 1049         HMAXE    = UNDEF
 1050         HCMAXE   = UNDEF
 1051         HMAXD    = UNDEF
 1052         HCMAXD   = UNDEF
 1053         WBT      = UNDEF
 1054 !
 1055 ! Group 3 variables
 1056 !
 1057       IF (  E3DF(1,1).GT.0 ) EF      = UNDEF
 1058       IF (  E3DF(1,2).GT.0 ) TH1M    = UNDEF
 1059       IF (  E3DF(1,3).GT.0 ) STH1M   = UNDEF
 1060       IF (  E3DF(1,4).GT.0 ) TH2M    = UNDEF
 1061       IF (  E3DF(1,5).GT.0 ) STH2M   = UNDEF
 1062         WN      = UNDEF
 1063 !
 1064 ! Group 4 variables
 1065 !
 1066         PHS      = UNDEF
 1067         PTP      = UNDEF
 1068         PLP      = UNDEF
 1069         PDIR     = UNDEF
 1070         PSI      = UNDEF
 1071         PWS      = UNDEF
 1072         PWST     = UNDEF
 1073         PNR      = UNDEF
 1074         PTHP0    = UNDEF
 1075         PQP      = UNDEF
 1076         PPE      = UNDEF
 1077         PGW      = UNDEF
 1078         PSW      = UNDEF
 1079         PTM1     = UNDEF
 1080         PT1      = UNDEF
 1081         PT2      = UNDEF
 1082         PEP      = UNDEF
 1083 !
 1084 ! Group 5 variables
 1085 !
 1086         UST      = UNDEF
 1087         USTDIR   = UNDEF
 1088         CHARN    = UNDEF
 1089         CGE      = UNDEF
 1090         PHIAW    = UNDEF
 1091         TAUWIX   = UNDEF
 1092         TAUWIY   = UNDEF
 1093         TAUWNX   = UNDEF
 1094         TAUWNY   = UNDEF
 1095         WHITECAP = UNDEF
 1096 !
 1097 ! Group 6 variables
 1098 !
 1099         SXX      = UNDEF
 1100         SXY      = UNDEF
 
 
                                                         ww3_gint.F90  page  21
 
 
 1101         SYY      = UNDEF
 1102         TAUOX    = UNDEF
 1103         TAUOY    = UNDEF
 1104         BHD      = UNDEF
 1105         PHIOC    = UNDEF
 1106         TUSX     = UNDEF
 1107         TUSY     = UNDEF
 1108         USSX     = UNDEF
 1109         USSY     = UNDEF
 1110         PRMS     = UNDEF
 1111         TPMS     = UNDEF
 1112         IF ( US3DF(1).GT.0 ) THEN
 1113              US3D     = UNDEF
 1114         ENDIF
 1115         IF ( P2MSF(1).GT.0) THEN
 1116              P2SMS    = UNDEF
 1117         ENDIF
 1118         TAUICE   = UNDEF
 1119         PHICE    = UNDEF
 1120         IF ( USSPF(1).GT.0 ) THEN
 1121              USSP     = UNDEF
 1122         ENDIF
 1123 !
 1124 ! Group 7 variables
 1125 !
 1126         ABA      = UNDEF
 1127         ABD      = UNDEF
 1128         UBA      = UNDEF
 1129         UBD      = UNDEF
 1130         BEDFORMS = UNDEF
 1131         PHIBBL   = UNDEF
 1132         TAUBBL   = UNDEF
 1133 !
 1134 ! Group 8 variables
 1135 !
 1136         MSSX     = UNDEF
 1137         MSSY     = UNDEF
 1138         MSCX     = UNDEF
 1139         MSCY     = UNDEF
 1140         MSSD     = UNDEF
 1141         MSCD     = UNDEF
 1142         QP       = UNDEF
 1143 !
 1144 !-------------------------------------------------------------------
 1145 ! 2.  Loop through output points
 1146 !
 1147         DO ISEA = 1, NSEA
 1148 !
 1149           IX = MAPSF(ISEA,1)
 1150           IY = MAPSF(ISEA,2)
 1151           MAPICE = MOD(MAPST2(IY,IX),2)
 1152           MAPDRY = MOD(MAPST2(IY,IX)/2,2)
 1153           MAPLND = MOD(MAPST2(IY,IX)/4,2)
 1154           MAPMSK = MOD(MAPST2(IY,IX)/8,2)
 1155           MAPINT = MOD(MAPST2(IY,IX)/16,2)
 
 
                                                         ww3_gint.F90  page  22
 
 
 1156           MAPST2(IY,IX) = MAPST2(IY,IX) - MAPICE - 2*MAPDRY - 4*MAPLND   >
 1157                           - 8*MAPMSK
 1158           ACTIVE =  (MAPICE .NE. 1 .AND. MAPDRY .NE. 1)
 1159 !
 1160           IF ( MAPINT .EQ. 0 ) THEN
 1161 !
 1162 ! Initial loop to determine status map
 1163 ! Initialize by setting it to be ice free and wet
 1164 !
 1165             MAPICE = 0
 1166             MAPDRY = 0
 1167             MAPMSK = 0
 1168             MAPLND = 0
 1169             ACTIVE = .TRUE.
 1170             MAPSTA(IY,IX) = ABS ( MAPSTA(IY,IX) )
 1171             SUMGRD = 0
 1172             DO IG = 1,GR_INTS(ISEA)%NGRDS
 1173               IGRID = GR_INTS(ISEA)%GDID(IG)
 1174               NMAPICE = 0
 1175               NMAPDRY = 0
 1176               NMAPLND = 0
 1177               NMAPMSK = 0
 1178               MAPICET = 0
 1179               MAPDRYT = 0
 1180               MAPLNDT = 0
 1181               MAPMSKT = 0
 1182               DO IPTS = 1,GR_INTS(ISEA)%IND_WTS(IG)%NP
 1183                 IGX = GR_INTS(ISEA)%IND_WTS(IG)%IP(IPTS)
 1184                 IGY = GR_INTS(ISEA)%IND_WTS(IG)%JP(IPTS)
 1185                 LMAPICE = MOD ( GRIDS(IGRID)%MAPST2(IGY,IGX),2 )
 1186                 LMAPDRY = MOD ( GRIDS(IGRID)%MAPST2(IGY,IGX)/2,2 )
 1187                 LMAPLND = MOD ( GRIDS(IGRID)%MAPST2(IGY,IGX)/4,2 )
 1188                 LMAPMSK = MOD ( GRIDS(IGRID)%MAPST2(IGY,IGX)/8,2 )
 1189                 IF ( LMAPICE .EQ. 1 ) NMAPICE = NMAPICE + 1
 1190                 IF ( LMAPDRY .EQ. 1 ) NMAPDRY = NMAPDRY + 1
 1191                 IF ( LMAPLND .EQ. 1 ) NMAPLND = NMAPLND + 1
 1192                 IF ( LMAPMSK .EQ. 1 ) NMAPMSK = NMAPMSK + 1
 1193               END DO
 1194               NMAPICE = NMAPICE*100/GR_INTS(ISEA)%IND_WTS(IG)%NP
 1195               NMAPDRY = NMAPDRY*100/GR_INTS(ISEA)%IND_WTS(IG)%NP
 1196               NMAPLND = NMAPLND*100/GR_INTS(ISEA)%IND_WTS(IG)%NP
 1197               NMAPMSK = NMAPMSK*100/GR_INTS(ISEA)%IND_WTS(IG)%NP
 1198               IF ( NMAPICE .GT. 50 ) MAPICET = 1
 1199               IF ( NMAPDRY .GT. 50 ) MAPDRYT = 1
 1200               IF ( NMAPLND .GT. 50 ) MAPLNDT = 1
 1201               IF ( NMAPMSK .GT. 50 ) MAPMSKT = 1
 1202               ACTIVE =  (MAPICET .NE. 1 .AND. MAPDRYT .NE. 1 .AND.       >
 1203                          MAPLNDT .NE. 1 .AND. MAPMSKT .NE. 1)
 1204               IF ( ACTIVE ) THEN
 1205                 USEGRID(IG) = .TRUE.
 1206                 SUMGRD = SUMGRD+1
 1207                 MAPICE = MAPICET
 1208                 MAPDRY = MAPDRYT
 1209                 MAPLND = MAPLNDT
 1210                 MAPMSK = MAPMSKT
 
 
                                                         ww3_gint.F90  page  23
 
 
 1211               ELSE
 1212                 USEGRID(IG) = .FALSE.
 1213               END IF
 1214             END DO
 1215             IF ( SUMGRD .EQ. 0 ) THEN
 1216               MAPICE = MAPICET
 1217               MAPDRY = MAPDRYT
 1218               MAPLND = MAPLNDT
 1219               MAPMSK = MAPMSKT
 1220             END IF
 1221 !
 1222 ! Reset the status map
 1223 !
 1224             MAPST2(IY,IX) = MAPST2(IY,IX) + MAPICE + 2*MAPDRY + 4*MAPLND >
 1225                             8*MAPMSK
 1226             ACTIVE =  (MAPICE .NE. 1 .AND. MAPDRY .NE. 1 .AND. MAPLND .NE>
 1227                        .AND. MAPMSK .NE. 1)
 1228             IF ( .NOT. ACTIVE ) MAPSTA(IY,IX) = -ABS ( MAPSTA(IY,IX) )
 1229 !
 1230 ! Second loop to do the actual interpolation
 1231 !
 1232             DO IG = 1,GR_INTS(ISEA)%NGRDS
 1233 !
 1234               IF ( USEGRID(IG) ) THEN
 1235 !
 1236                 IGRID = GR_INTS(ISEA)%GDID(IG)
 1237 !
 1238 ! Initialize temporary variables used
 1239 !
 1240                 SUMWT = 0.0
 1241 !
 1242 ! Group 1 variables
 1243 !
 1244                 DWAUX       = UNDEF
 1245                 CXAUX       = UNDEF
 1246                 CYAUX       = UNDEF
 1247                 UAAUX       = UNDEF
 1248                 UDAUX       = UNDEF
 1249                 ASAUX       = UNDEF
 1250                 WLVAUX      = UNDEF
 1251                 ICEAUX      = UNDEF
 1252                 BERGAUX     = UNDEF
 1253                 SED_D50AUX  = UNDEF
 1254                 ICEHAUX     = UNDEF
 1255                 ICEFAUX     = UNDEF
 1256                 SUMWT1      = 0
 1257 !
 1258 ! Group 2 variables
 1259 !
 1260                 HSAUX       = UNDEF
 1261                 WLMAUX      = UNDEF
 1262                 T02AUX      = UNDEF
 1263                 T0M1AUX     = UNDEF
 1264                 T01AUX      = UNDEF
 1265                 FP0AUX      = UNDEF
 
 
                                                         ww3_gint.F90  page  24
 
 
 1266                 THMAUX1     = UNDEF
 1267                 THMAUX2     = UNDEF
 1268                 THSAUX      = UNDEF
 1269                 THP0AUX1    = UNDEF
 1270                 THP0AUX2    = UNDEF
 1271                 HSIGAUX     = UNDEF
 1272                 STMAXEAUX   = UNDEF
 1273                 STMAXDAUX   = UNDEF
 1274                 HMAXEAUX    = UNDEF
 1275                 HCMAXEAUX   = UNDEF
 1276                 HMAXDAUX    = UNDEF
 1277                 HCMAXDAUX   = UNDEF
 1278                 WBTAUX      = UNDEF
 1279                 SUMWT2      = 0
 1280 !
 1281 ! Group 3 variables
 1282 !
 1283                 EFAUX      = UNDEF
 1284                 TH1MAUX    = UNDEF
 1285                 STH1MAUX   = UNDEF
 1286                 TH2MAUX    = UNDEF
 1287                 STH2MAUX   = UNDEF
 1288                 WNAUX      = UNDEF
 1289                 SUMWT3A    = 0
 1290                 SUMWT3B    = 0
 1291                 SUMWT3C    = 0
 1292                 SUMWT3D    = 0
 1293                 SUMWT3E    = 0
 1294                 SUMWT3F    = 0
 1295 !
 1296 ! Group 4 variables
 1297 !
 1298                 PHSAUX      = UNDEF
 1299                 PTPAUX      = UNDEF
 1300                 PLPAUX      = UNDEF
 1301                 PDIRAUX1    = UNDEF
 1302                 PDIRAUX2    = UNDEF
 1303                 PSIAUX      = UNDEF
 1304                 PWSAUX      = UNDEF
 1305                 PWSTAUX     = UNDEF
 1306                 PTHP0AUX1   = UNDEF
 1307                 PTHP0AUX2   = UNDEF
 1308                 PQPAUX      = UNDEF
 1309                 PPEAUX      = UNDEF
 1310                 PGWAUX      = UNDEF
 1311                 PSWAUX      = UNDEF
 1312                 PTM1AUX     = UNDEF
 1313                 PT1AUX      = UNDEF
 1314                 PT2AUX      = UNDEF
 1315                 PEPAUX      = UNDEF
 1316                 SUMWT4      = 0
 1317 !
 1318 ! Group 5 variables
 1319 !
 1320                 USTAUX1     = UNDEF
 
 
                                                         ww3_gint.F90  page  25
 
 
 1321                 USTAUX2     = UNDEF
 1322                 CHARNAUX    = UNDEF
 1323                 CGEAUX      = UNDEF
 1324                 PHIAWAUX    = UNDEF
 1325                 TAUWIXAUX   = UNDEF
 1326                 TAUWIYAUX   = UNDEF
 1327                 TAUWNXAUX   = UNDEF
 1328                 TAUWNYAUX   = UNDEF
 1329                 WHITECAPAUX = UNDEF
 1330                 SUMWT5      = 0
 1331                 SUMWTC      = 0
 1332 !
 1333 ! Group 6 variables
 1334 !
 1335                 SXXAUX      = UNDEF
 1336                 SXYAUX      = UNDEF
 1337                 SYYAUX      = UNDEF
 1338                 TAUOXAUX    = UNDEF
 1339                 TAUOYAUX    = UNDEF
 1340                 BHDAUX      = UNDEF
 1341                 PHIOCAUX    = UNDEF
 1342                 TUSXAUX     = UNDEF
 1343                 TUSYAUX     = UNDEF
 1344                 USSXAUX     = UNDEF
 1345                 USSYAUX     = UNDEF
 1346                 PRMSAUX     = UNDEF
 1347                 TPMSAUX     = UNDEF
 1348                 P2SMSAUX    = UNDEF
 1349                 US3DAUX     = UNDEF
 1350                 PHICEAUX    = UNDEF
 1351                 TAUICEAUX   = UNDEF
 1352                 USSPAUX     = UNDEF
 1353                 SUMWT69     = 0
 1354                 SUMWT68     = 0
 1355                 SUMWT612    = 0
 1356                 SUMWT6      = 0
 1357 !
 1358 ! Group 7 variables
 1359 !
 1360                 ABAAUX      = UNDEF
 1361                 ABDAUX      = UNDEF
 1362                 UBAAUX      = UNDEF
 1363                 UBDAUX      = UNDEF
 1364                 BEDFORMSAUX = UNDEF
 1365                 PHIBBLAUX   = UNDEF
 1366                 TAUBBLAUX   = UNDEF
 1367                 SUMWT7      = 0
 1368                 SUMWTB      = 0
 1369 !
 1370 ! Group 8 variables
 1371 !
 1372                 MSSXAUX     = UNDEF
 1373                 MSSYAUX     = UNDEF
 1374                 MSCXAUX     = UNDEF
 1375                 MSCYAUX     = UNDEF
 
 
                                                         ww3_gint.F90  page  26
 
 
 1376                 MSSDAUX1    = UNDEF
 1377                 MSSDAUX2    = UNDEF
 1378                 MSCDAUX1    = UNDEF
 1379                 MSCDAUX2    = UNDEF
 1380                 QPAUX       = UNDEF
 1381                 SUMWT8      = 0
 1382 !
 1383 ! Loop through the points per grid to obtain interpolated values
 1384 !
 1385                 DO IPTS = 1,GR_INTS(ISEA)%IND_WTS(IG)%NP
 1386                   IGX = GR_INTS(ISEA)%IND_WTS(IG)%IP(IPTS)
 1387                   IGY = GR_INTS(ISEA)%IND_WTS(IG)%JP(IPTS)
 1388                   WT = GR_INTS(ISEA)%IND_WTS(IG)%WT(IPTS)
 1389                   GSEA = GRIDS(IGRID)%MAPFS(IGY,IGX)
 1390 !
 1391 ! Group 1 variables
 1392 !
 1393                   IF ( FLOGRD(1,1) .AND. ACTIVE ) THEN
 1394                     IF ( WADATS(IGRID)%DW(GSEA) .NE. UNDEF ) THEN
 1395                       SUMWT1(1) = SUMWT1(1) + WT
 1396                       IF ( DWAUX .EQ. UNDEF ) THEN
 1397                         DWAUX = WADATS(IGRID)%DW(GSEA)*WT
 1398                       ELSE
 1399                         DWAUX = DWAUX + WADATS(IGRID)%DW(GSEA)*WT
 1400                       END IF
 1401                     END IF
 1402                   END IF
 1403 !
 1404                   IF ( FLOGRD(1,2) .AND. ACTIVE ) THEN
 1405                     IF ( WADATS(IGRID)%CX(GSEA) .NE. UNDEF ) THEN
 1406                       SUMWT1(2) = SUMWT1(2) + WT
 1407                       IF ( CXAUX .EQ. UNDEF ) THEN
 1408                         CXAUX = WADATS(IGRID)%CX(GSEA)*WT
 1409                         CYAUX = WADATS(IGRID)%CY(GSEA)*WT
 1410                       ELSE
 1411                         CXAUX = CXAUX + WADATS(IGRID)%CX(GSEA)*WT
 1412                         CYAUX = CYAUX + WADATS(IGRID)%CY(GSEA)*WT
 1413                        END IF
 1414                     END IF
 1415                   END IF
 1416 !
 1417                   IF ( FLOGRD(1,3) ) THEN
 1418                     IF ( WADATS(IGRID)%UA(GSEA) .NE. UNDEF ) THEN
 1419                       SUMWT1(3) = SUMWT1(3) + WT
 1420                       IF ( UAAUX .EQ. UNDEF ) THEN
 1421                         UAAUX = WADATS(IGRID)%UA(GSEA)*WT
 1422                         UDAUX = WADATS(IGRID)%UD(GSEA)*WT
 1423                       ELSE
 1424                         UAAUX = UAAUX + WADATS(IGRID)%UA(GSEA)*WT
 1425                         UDAUX = UDAUX + WADATS(IGRID)%UD(GSEA)*WT
 1426                       END IF
 1427                     END IF
 1428                   END IF
 1429 !
 1430                   IF ( FLOGRD(1,4) .AND. ACTIVE ) THEN
 
 
                                                         ww3_gint.F90  page  27
 
 
 1431                     IF ( WADATS(IGRID)%AS(GSEA) .NE. UNDEF ) THEN
 1432                       SUMWT1(4) = SUMWT1(4) + WT
 1433                       IF ( ASAUX .EQ. UNDEF ) THEN
 1434                         ASAUX = WADATS(IGRID)%AS(GSEA)*WT
 1435                       ELSE
 1436                         ASAUX = ASAUX + WADATS(IGRID)%AS(GSEA)*WT
 1437                       END IF
 1438                     END IF
 1439                   END IF
 1440 !
 1441                   IF ( FLOGRD(1,5) .AND. ACTIVE ) THEN
 1442                     IF ( WDATAS(IGRID)%WLV(GSEA) .NE. UNDEF ) THEN
 1443                       SUMWT1(5) = SUMWT1(5) + WT
 1444                       IF ( WLVAUX .EQ. UNDEF ) THEN
 1445                         WLVAUX = WDATAS(IGRID)%WLV(GSEA)*WT
 1446                       ELSE
 1447                         WLVAUX = WLVAUX + WDATAS(IGRID)%WLV(GSEA)*WT
 1448                       END IF
 1449                     END IF
 1450                   END IF
 1451 !
 1452                   IF ( FLOGRD(1,6) ) THEN
 1453                     IF ( WDATAS(IGRID)%ICE(GSEA) .NE. UNDEF ) THEN
 1454                       SUMWT1(6) = SUMWT1(6) + WT
 1455                       IF ( ICEAUX .EQ. UNDEF ) THEN
 1456                         ICEAUX = WDATAS(IGRID)%ICE(GSEA)*WT
 1457                       ELSE
 1458                         ICEAUX = ICEAUX + WDATAS(IGRID)%ICE(GSEA)*WT
 1459                       END IF
 1460                     END IF
 1461                   END IF
 1462 !
 1463                   IF ( FLOGRD(1,7) .AND. ACTIVE ) THEN
 1464                     IF ( WDATAS(IGRID)%BERG(GSEA) .NE. UNDEF ) THEN
 1465                       SUMWT1(7) = SUMWT1(7) + WT
 1466                       IF ( BERGAUX .EQ. UNDEF ) THEN
 1467                         BERGAUX = WDATAS(IGRID)%BERG(GSEA)*WT
 1468                       ELSE
 1469                         BERGAUX = BERGAUX + WDATAS(IGRID)%BERG(GSEA)*WT
 1470                       END IF
 1471                     END IF
 1472                   END IF
 1473 !
 1474 ! Group 2 variables
 1475 !
 1476                   IF ( FLOGRD(2,1) .AND. ACTIVE ) THEN
 1477                     IF ( WADATS(IGRID)%HS(GSEA) .NE. UNDEF ) THEN
 1478                       SUMWT2(1) = SUMWT2(1) + WT
 1479                       IF ( HSAUX .EQ. UNDEF ) THEN
 1480                         HSAUX = WADATS(IGRID)%HS(GSEA)*WT
 1481                       ELSE
 1482                         HSAUX = HSAUX + WADATS(IGRID)%HS(GSEA)*WT
 1483                       END IF
 1484                     END IF
 1485                   END IF
 
 
                                                         ww3_gint.F90  page  28
 
 
 1486 !
 1487                   IF ( FLOGRD(2,2) .AND. ACTIVE ) THEN
 1488                     IF ( WADATS(IGRID)%WLM(GSEA) .NE. UNDEF ) THEN
 1489                       SUMWT2(2) = SUMWT2(2) + WT
 1490                       IF ( WLMAUX .EQ. UNDEF ) THEN
 1491                         WLMAUX = WADATS(IGRID)%WLM(GSEA)*WT
 1492                       ELSE
 1493                         WLMAUX = WLMAUX + WADATS(IGRID)%WLM(GSEA)*WT
 1494                       END IF
 1495                     END IF
 1496                   END IF
 1497 !
 1498                   IF ( FLOGRD(2,3) .AND. ACTIVE ) THEN
 1499                     IF ( WADATS(IGRID)%T02(GSEA) .NE. UNDEF ) THEN
 1500                       SUMWT2(3) = SUMWT2(3) + WT
 1501                       IF ( T02AUX .EQ. UNDEF ) THEN
 1502                         T02AUX = WADATS(IGRID)%T02(GSEA)*WT
 1503                       ELSE
 1504                         T02AUX = T02AUX + WADATS(IGRID)%T02(GSEA)*WT
 1505                       END IF
 1506                     END IF
 1507                   END IF
 1508 !
 1509                   IF ( FLOGRD(2,4) .AND. ACTIVE ) THEN
 1510                     IF ( WADATS(IGRID)%T0M1(GSEA) .NE. UNDEF ) THEN
 1511                       SUMWT2(4) = SUMWT2(4) + WT
 1512                       IF ( T0M1AUX .EQ. UNDEF ) THEN
 1513                         T0M1AUX = WADATS(IGRID)%T0M1(GSEA)*WT
 1514                       ELSE
 1515                         T0M1AUX = T0M1AUX + WADATS(IGRID)%T0M1(GSEA)*WT
 1516                       END IF
 1517                     END IF
 1518                   END IF
 1519 !
 1520                   IF ( FLOGRD(2,5) .AND. ACTIVE ) THEN
 1521                     IF ( WADATS(IGRID)%T01(GSEA) .NE. UNDEF ) THEN
 1522                       SUMWT2(5) = SUMWT2(5) + WT
 1523                       IF ( T01AUX .EQ. UNDEF ) THEN
 1524                         T01AUX = WADATS(IGRID)%T01(GSEA)*WT
 1525                       ELSE
 1526                         T01AUX = T01AUX + WADATS(IGRID)%T01(GSEA)*WT
 1527                       END IF
 1528                     END IF
 1529                   END IF
 1530 !
 1531                   IF ( (FLOGRD(2,6) .OR. FLOGRD(2,18)) .AND. ACTIVE ) THEN
 1532                     ! Note: Output TP [FLOGRD(2,18)] is derived from FP0
 1533                     IF ( WADATS(IGRID)%FP0(GSEA) .NE. UNDEF ) THEN
 1534                       SUMWT2(6) = SUMWT2(6) + WT
 1535                       IF ( FP0AUX .EQ. UNDEF ) THEN
 1536                         FP0AUX = WADATS(IGRID)%FP0(GSEA)*WT
 1537                       ELSE
 1538                         FP0AUX = FP0AUX + WADATS(IGRID)%FP0(GSEA)*WT
 1539                       END IF
 1540                     END IF
 
 
                                                         ww3_gint.F90  page  29
 
 
 1541                   END IF
 1542 !
 1543                   IF ( FLOGRD(2,7) .AND. ACTIVE ) THEN
 1544                     IF ( WADATS(IGRID)%THM(GSEA) .NE. UNDEF ) THEN
 1545                       SUMWT2(7) = SUMWT2(7) + WT
 1546                       IF ( THMAUX1 .EQ. UNDEF ) THEN
 1547                         THMAUX1 = COS ( WADATS(IGRID)%THM(GSEA) )*WT
 1548                         THMAUX2 = SIN ( WADATS(IGRID)%THM(GSEA) )*WT
 1549                       ELSE
 1550                         THMAUX1 = THMAUX1 + COS ( WADATS(IGRID)%THM(GSEA)>
 1551                         THMAUX2 = THMAUX2 + SIN ( WADATS(IGRID)%THM(GSEA)>
 1552                       END IF
 1553                     END IF
 1554                   END IF
 1555 !
 1556                   IF ( FLOGRD(2,8) .AND. ACTIVE ) THEN
 1557                     IF ( WADATS(IGRID)%THS(GSEA) .NE. UNDEF ) THEN
 1558                       SUMWT2(8) = SUMWT2(8) + WT
 1559                       IF ( THSAUX .EQ. UNDEF ) THEN
 1560                         THSAUX = WADATS(IGRID)%THS(GSEA)*WT
 1561                       ELSE
 1562                         THSAUX = THSAUX + WADATS(IGRID)%THS(GSEA)*WT
 1563                       END IF
 1564                     END IF
 1565                   END IF
 1566 !
 1567                   IF ( FLOGRD(2,9) .AND. ACTIVE ) THEN
 1568                     IF ( WADATS(IGRID)%THP0(GSEA) .NE. UNDEF ) THEN
 1569                       SUMWT2(9) = SUMWT2(9) + WT
 1570                       IF ( THP0AUX1 .EQ. UNDEF ) THEN
 1571                         THP0AUX1 = COS ( WADATS(IGRID)%THP0(GSEA) )*WT
 1572                         THP0AUX2 = SIN ( WADATS(IGRID)%THP0(GSEA) )*WT
 1573                       ELSE
 1574                         THP0AUX1 = THP0AUX1 +                            >
 1575                                    COS ( WADATS(IGRID)%THP0(GSEA) )*WT
 1576                         THP0AUX2 = THP0AUX2 +                            >
 1577                                    SIN ( WADATS(IGRID)%THP0(GSEA) )*WT
 1578                       END IF
 1579                     END IF
 1580                   END IF
 1581 !
 1582                   IF ( FLOGRD(2,10) .AND. ACTIVE ) THEN
 1583                     IF ( WADATS(IGRID)%HSIG(GSEA) .NE. UNDEF ) THEN
 1584                       SUMWT2(10) = SUMWT2(10) + WT
 1585                       IF ( HSIGAUX .EQ. UNDEF )   HSIGAUX = 0.
 1586                       HSIGAUX = HSIGAUX + WADATS(IGRID)%HSIG(GSEA)*WT
 1587                     END IF
 1588                   END IF
 1589 !
 1590                   IF ( FLOGRD(2,11) .AND. ACTIVE ) THEN
 1591                     IF ( WADATS(IGRID)%STMAXE(GSEA) .NE. UNDEF ) THEN
 1592                       SUMWT2(11) = SUMWT2(11) + WT
 1593                       IF ( STMAXEAUX .EQ. UNDEF )   STMAXEAUX = 0.
 1594                       STMAXEAUX = STMAXEAUX + WADATS(IGRID)%STMAXE(GSEA)*>
 1595                     END IF
 
 
                                                         ww3_gint.F90  page  30
 
 
 1596                   END IF
 1597 !
 1598                   IF ( FLOGRD(2,12) .AND. ACTIVE ) THEN
 1599                     IF ( WADATS(IGRID)%STMAXD(GSEA) .NE. UNDEF ) THEN
 1600                       SUMWT2(12) = SUMWT2(12) + WT
 1601                       IF ( STMAXDAUX .EQ. UNDEF )   STMAXDAUX = 0.
 1602                       STMAXDAUX = STMAXDAUX + WADATS(IGRID)%STMAXD(GSEA)*>
 1603                     END IF
 1604                   END IF
 1605 !
 1606                   IF ( FLOGRD(2,13) .AND. ACTIVE ) THEN
 1607                     IF ( WADATS(IGRID)%HMAXE(GSEA) .NE. UNDEF ) THEN
 1608                       SUMWT2(13) = SUMWT2(13) + WT
 1609                       IF ( HMAXEAUX .EQ. UNDEF )   HMAXEAUX = 0.
 1610                       HMAXEAUX = HMAXEAUX + WADATS(IGRID)%HMAXE(GSEA)*WT
 1611                     END IF
 1612                   END IF
 1613 !
 1614                   IF ( FLOGRD(2,14) .AND. ACTIVE ) THEN
 1615                     IF ( WADATS(IGRID)%HCMAXE(GSEA) .NE. UNDEF ) THEN
 1616                       SUMWT2(14) = SUMWT2(14) + WT
 1617                       IF ( HCMAXEAUX .EQ. UNDEF ) HCMAXEAUX = 0.
 1618                       HCMAXEAUX = HCMAXEAUX + WADATS(IGRID)%HCMAXE(GSEA)*>
 1619                     END IF
 1620                   END IF
 1621 !
 1622                   IF ( FLOGRD(2,15) .AND. ACTIVE ) THEN
 1623                     IF ( WADATS(IGRID)%HMAXD(GSEA) .NE. UNDEF ) THEN
 1624                       SUMWT2(15) = SUMWT2(15) + WT
 1625                       IF ( HMAXDAUX .EQ. UNDEF )   HMAXDAUX = 0.
 1626                       HMAXDAUX = HMAXDAUX + WADATS(IGRID)%HMAXD(GSEA)*WT
 1627                     END IF
 1628                   END IF
 1629 !
 1630                   IF ( FLOGRD(2,16) .AND. ACTIVE ) THEN
 1631                     IF ( WADATS(IGRID)%HCMAXD(GSEA) .NE. UNDEF ) THEN
 1632                       SUMWT2(16) = SUMWT2(16) + WT
 1633                       IF ( HCMAXDAUX .EQ. UNDEF )   HCMAXDAUX = 0.
 1634                       HCMAXDAUX = HCMAXDAUX + WADATS(IGRID)%HCMAXD(GSEA)*>
 1635                     END IF
 1636                   END IF
 1637 !
 1638                   IF ( FLOGRD(2,17) .AND. ACTIVE ) THEN
 1639                     IF ( WADATS(IGRID)%WBT(GSEA) .NE. UNDEF ) THEN
 1640                       SUMWT2(17) = SUMWT2(17) + WT
 1641                       IF ( WBTAUX .EQ. UNDEF )   WBTAUX = 0.
 1642                       WBTAUX = WBTAUX + WADATS(IGRID)%WBT(GSEA)*WT
 1643                     END IF
 1644                   END IF
 1645 !
 1646 ! Group 3 variables
 1647 !
 1648                   IF ( FLOGRD(3,1) .AND. ACTIVE ) THEN
 1649                     DO IFREQ = E3DF(2,1),E3DF(3,1)
 1650                       IF ( WADATS(IGRID)%EF(GSEA,IFREQ) .NE. UNDEF ) THEN
 
 
                                                         ww3_gint.F90  page  31
 
 
 1651                         SUMWT3A(IFREQ) = SUMWT3A(IFREQ) + WT
 1652                         IF ( EFAUX(IFREQ) .EQ. UNDEF ) THEN
 1653                           EFAUX(IFREQ) = WADATS(IGRID)%EF(GSEA,IFREQ)*WT
 1654                         ELSE
 1655                           EFAUX(IFREQ) = EFAUX(IFREQ) + WADATS(IGRID)%EF(>
 1656                         END IF
 1657                       END IF
 1658                     END DO
 1659                   END IF
 1660 !
 1661                   IF ( FLOGRD(3,2) .AND. ACTIVE ) THEN
 1662                     DO IFREQ = E3DF(2,2),E3DF(3,2)
 1663                       IF ( WADATS(IGRID)%TH1M(GSEA,IFREQ) .NE. UNDEF ) TH>
 1664                         SUMWT3B(IFREQ) = SUMWT3B(IFREQ) + WT
 1665                         IF ( TH1MAUX(IFREQ) .EQ. UNDEF ) THEN
 1666                           TH1MAUX(IFREQ) = WADATS(IGRID)%TH1M(GSEA,IFREQ)>
 1667                         ELSE
 1668                           TH1MAUX(IFREQ) = TH1MAUX(IFREQ) + WADATS(IGRID)>
 1669                         END IF
 1670                       END IF
 1671                     END DO
 1672                   END IF
 1673 !
 1674                   IF ( FLOGRD(3,3) .AND. ACTIVE ) THEN
 1675                     DO IFREQ = E3DF(2,3),E3DF(3,3)
 1676                       IF ( WADATS(IGRID)%STH1M(GSEA,IFREQ) .NE. UNDEF ) T>
 1677                         SUMWT3C(IFREQ) = SUMWT3C(IFREQ) + WT
 1678                         IF ( STH1MAUX(IFREQ) .EQ. UNDEF ) THEN
 1679                           STH1MAUX(IFREQ) = WADATS(IGRID)%STH1M(GSEA,IFRE>
 1680                         ELSE
 1681                           STH1MAUX(IFREQ) = STH1MAUX(IFREQ) + WADATS(IGRI>
 1682                         END IF
 1683                       END IF
 1684                     END DO
 1685                   END IF
 1686 !
 1687                   IF ( FLOGRD(3,4) .AND. ACTIVE ) THEN
 1688                     DO IFREQ = E3DF(2,4),E3DF(3,4)
 1689                       IF ( WADATS(IGRID)%TH2M(GSEA,IFREQ) .NE. UNDEF ) TH>
 1690                         SUMWT3D(IFREQ) = SUMWT3D(IFREQ) + WT
 1691                         IF ( TH2MAUX(IFREQ) .EQ. UNDEF ) THEN
 1692                           TH2MAUX(IFREQ) = WADATS(IGRID)%TH2M(GSEA,IFREQ)>
 1693                         ELSE
 1694                           TH2MAUX(IFREQ) = TH2MAUX(IFREQ) + WADATS(IGRID)>
 1695                         END IF
 1696                       END IF
 1697                     END DO
 1698                   END IF
 1699 !
 1700                   IF ( FLOGRD(3,5) .AND. ACTIVE ) THEN
 1701                     DO IFREQ = E3DF(2,5),E3DF(3,5)
 1702                       IF ( WADATS(IGRID)%STH2M(GSEA,IFREQ) .NE. UNDEF ) T>
 1703                         SUMWT3E(IFREQ) = SUMWT3E(IFREQ) + WT
 1704                         IF ( STH2MAUX(IFREQ) .EQ. UNDEF ) THEN
 1705                           STH2MAUX(IFREQ) = WADATS(IGRID)%STH2M(GSEA,IFRE>
 
 
                                                         ww3_gint.F90  page  32
 
 
 1706                         ELSE
 1707                           STH2MAUX(IFREQ) = STH2MAUX(IFREQ) + WADATS(IGRI>
 1708                         END IF
 1709                       END IF
 1710                     END DO
 1711                   END IF
 1712                         
 1713 !
 1714                   IF ( FLOGRD(3,6) .AND. ACTIVE ) THEN
 1715                     DO IK = 1,NK
 1716                       IF ( WADATS(IGRID)%WN(IK,GSEA) .NE. UNDEF ) THEN
 1717                         SUMWT3F(IK) = SUMWT3F(IK) + WT
 1718                         IF ( WNAUX(IK) .EQ. UNDEF ) THEN
 1719                           WNAUX(IK) = WADATS(IGRID)%WN(IK,GSEA)*WT
 1720                         ELSE
 1721                           WNAUX(IK) = WNAUX(IK) + WADATS(IGRID)%WN(IK,GSE>
 1722                         END IF
 1723                       END IF
 1724                     END DO
 1725                   END IF
 1726 !
 1727 ! Group 4 variables
 1728 !
 1729                   DO ISWLL = 0, NOSWLL_MIN
 1730 !
 1731                     IF ( FLOGRD(4,1) .AND. ACTIVE ) THEN
 1732                       IF ( WADATS(IGRID)%PHS(GSEA,ISWLL) .NE. UNDEF ) THEN
 1733                         SUMWT4(1,ISWLL) = SUMWT4(1,ISWLL) + WT
 1734                         IF ( PHSAUX(ISWLL) .EQ. UNDEF ) THEN
 1735                           PHSAUX(ISWLL) = WADATS(IGRID)%PHS(GSEA,ISWLL)*WT
 1736                         ELSE
 1737                           PHSAUX(ISWLL) = PHSAUX(ISWLL) +                >
 1738                                           WADATS(IGRID)%PHS(GSEA,ISWLL)*WT
 1739                         END IF
 1740                       END IF
 1741                     END IF
 1742 !
 1743                     IF ( FLOGRD(4,2) .AND. ACTIVE ) THEN
 1744                       IF ( WADATS(IGRID)%PTP(GSEA,ISWLL) .NE. UNDEF ) THEN
 1745                         SUMWT4(2,ISWLL) = SUMWT4(2,ISWLL) + WT
 1746                         IF ( PTPAUX(ISWLL) .EQ. UNDEF ) THEN
 1747                           PTPAUX(ISWLL) = WADATS(IGRID)%PTP(GSEA,ISWLL)*WT
 1748                         ELSE
 1749                           PTPAUX(ISWLL) = PTPAUX(ISWLL) +                >
 1750                                           WADATS(IGRID)%PTP(GSEA,ISWLL)*WT
 1751                         END IF
 1752                       END IF
 1753                     END IF
 1754 !
 1755                     IF ( FLOGRD(4,3) .AND. ACTIVE ) THEN
 1756                       IF ( WADATS(IGRID)%PLP(GSEA,ISWLL) .NE. UNDEF ) THEN
 1757                         SUMWT4(3,ISWLL) = SUMWT4(3,ISWLL) + WT
 1758                         IF ( PLPAUX(ISWLL) .EQ. UNDEF ) THEN
 1759                           PLPAUX(ISWLL) = WADATS(IGRID)%PLP(GSEA,ISWLL)*WT
 1760                         ELSE
 
 
                                                         ww3_gint.F90  page  33
 
 
 1761                           PLPAUX(ISWLL) = PLPAUX(ISWLL) +                >
 1762                                           WADATS(IGRID)%PLP(GSEA,ISWLL)*WT
 1763                         END IF
 1764                       END IF
 1765                     END IF
 1766 !
 1767                     IF ( FLOGRD(4,4) .AND. ACTIVE ) THEN
 1768                       IF ( WADATS(IGRID)%PDIR(GSEA,ISWLL) .NE. UNDEF ) TH>
 1769                         SUMWT4(4,ISWLL) = SUMWT4(4,ISWLL) + WT
 1770                         IF ( PDIRAUX1(ISWLL) .EQ. UNDEF ) THEN
 1771                           PDIRAUX1(ISWLL) =
 1772                                COS ( WADATS(IGRID)%PDIR(GSEA,ISWLL) )*WT
 1773                           PDIRAUX2(ISWLL) =
 1774                                SIN ( WADATS(IGRID)%PDIR(GSEA,ISWLL) )*WT
 1775                         ELSE
 1776                           PDIRAUX1(ISWLL) = PDIRAUX1(ISWLL) +
 1777                                COS ( WADATS(IGRID)%PDIR(GSEA,ISWLL) )*WT
 1778                           PDIRAUX2(ISWLL) = PDIRAUX2(ISWLL) +
 1779                                SIN ( WADATS(IGRID)%PDIR(GSEA,ISWLL) )*WT
 1780                         END IF
 1781                       END IF
 1782                     END IF
 1783 !
 1784                     IF ( FLOGRD(4,5) .AND. ACTIVE ) THEN
 1785                       IF ( WADATS(IGRID)%PSI(GSEA,ISWLL) .NE. UNDEF ) THEN
 1786                         SUMWT4(5,ISWLL) = SUMWT4(5,ISWLL) + WT
 1787                         IF ( PSIAUX(ISWLL) .EQ. UNDEF ) THEN
 1788                           PSIAUX(ISWLL) = WADATS(IGRID)%PSI(GSEA,ISWLL)*WT
 1789                         ELSE
 1790                           PSIAUX(ISWLL) = PSIAUX(ISWLL) +                >
 1791                                           WADATS(IGRID)%PSI(GSEA,ISWLL)*WT
 1792                         END IF
 1793                       END IF
 1794                     END IF
 1795 !
 1796                     IF ( FLOGRD(4,6) .AND. ACTIVE ) THEN
 1797                       IF ( WADATS(IGRID)%PWS(GSEA,ISWLL) .NE. UNDEF ) THEN
 1798                         SUMWT4(6,ISWLL) = SUMWT4(6,ISWLL) + WT
 1799                         IF ( PWSAUX(ISWLL) .EQ. UNDEF ) THEN
 1800                           PWSAUX(ISWLL) = WADATS(IGRID)%PWS(GSEA,ISWLL)*WT
 1801                         ELSE
 1802                           PWSAUX(ISWLL) = PWSAUX(ISWLL) +                >
 1803                                           WADATS(IGRID)%PWS(GSEA,ISWLL)*WT
 1804                         END IF
 1805                       END IF
 1806                     END IF
 1807 !
 1808                     IF ( FLOGRD(4,7) .AND. ACTIVE ) THEN
 1809                       IF ( WADATS(IGRID)%PTHP0(GSEA,ISWLL) .NE. UNDEF ) T>
 1810                         SUMWT4(7,ISWLL) = SUMWT4(7,ISWLL) + WT
 1811                         IF (PTHP0AUX1(ISWLL).EQ.UNDEF)                 &
 1812                                                    PTHP0AUX1(ISWLL) = 0.
 1813                         IF (PTHP0AUX2(ISWLL).EQ.UNDEF)                 &
 1814                                                    PTHP0AUX2(ISWLL) = 0.
 1815                         PTHP0AUX1(ISWLL) = PTHP0AUX1(ISWLL) +          &
 
 
                                                         ww3_gint.F90  page  34
 
 
 1816                               COS ( WADATS(IGRID)%PTHP0(GSEA,ISWLL) )*WT
 1817                         PTHP0AUX2(ISWLL) = PTHP0AUX2(ISWLL) +          &
 1818                               SIN ( WADATS(IGRID)%PTHP0(GSEA,ISWLL) )*WT
 1819                       END IF
 1820                     END IF
 1821 !
 1822                     IF ( FLOGRD(4,8) .AND. ACTIVE ) THEN
 1823                       IF ( WADATS(IGRID)%PQP(GSEA,ISWLL) .NE. UNDEF ) THEN
 1824                         SUMWT4(8,ISWLL) = SUMWT4(8,ISWLL) + WT
 1825                         IF ( PQPAUX(ISWLL).EQ.UNDEF ) PQPAUX(ISWLL) = 0.
 1826                         PQPAUX(ISWLL) = PQPAUX(ISWLL) +                &
 1827                                         WADATS(IGRID)%PQP(GSEA,ISWLL)*WT
 1828                       END IF
 1829                     END IF
 1830 !
 1831                     IF ( FLOGRD(4,9) .AND. ACTIVE ) THEN
 1832                       IF ( WADATS(IGRID)%PPE(GSEA,ISWLL) .NE. UNDEF ) THEN
 1833                         SUMWT4(9,ISWLL) = SUMWT4(9,ISWLL) + WT
 1834                         IF ( PPEAUX(ISWLL).EQ.UNDEF ) PPEAUX(ISWLL) = 0.
 1835                         PPEAUX(ISWLL) = PPEAUX(ISWLL) +                &
 1836                                         WADATS(IGRID)%PPE(GSEA,ISWLL)*WT
 1837                       END IF
 1838                     END IF
 1839 !
 1840                     IF ( FLOGRD(4,10) .AND. ACTIVE ) THEN
 1841                       IF ( WADATS(IGRID)%PGW(GSEA,ISWLL) .NE. UNDEF ) THEN
 1842                         SUMWT4(10,ISWLL) = SUMWT4(10,ISWLL) + WT
 1843                         IF ( PGWAUX(ISWLL).EQ.UNDEF ) PGWAUX(ISWLL) = 0.
 1844                         PGWAUX(ISWLL) = PGWAUX(ISWLL) +                &
 1845                                         WADATS(IGRID)%PGW(GSEA,ISWLL)*WT
 1846                       END IF
 1847                     END IF
 1848 !
 1849                     IF ( FLOGRD(4,11) .AND. ACTIVE ) THEN
 1850                       IF ( WADATS(IGRID)%PSW(GSEA,ISWLL) .NE. UNDEF ) THEN
 1851                         SUMWT4(11,ISWLL) = SUMWT4(11,ISWLL) + WT
 1852                         IF ( PSWAUX(ISWLL).EQ.UNDEF ) PSWAUX(ISWLL) = 0.
 1853                         PSWAUX(ISWLL) = PSWAUX(ISWLL) +                &
 1854                                         WADATS(IGRID)%PSW(GSEA,ISWLL)*WT
 1855                       END IF
 1856                     END IF
 1857 !
 1858                     IF ( FLOGRD(4,12) .AND. ACTIVE ) THEN
 1859                       IF ( WADATS(IGRID)%PTM1(GSEA,ISWLL) .NE. UNDEF ) TH>
 1860                         SUMWT4(12,ISWLL) = SUMWT4(12,ISWLL) + WT
 1861                         IF ( PTM1AUX(ISWLL).EQ.UNDEF )                &
 1862                                                     PTM1AUX(ISWLL) = 0.
 1863                         PTM1AUX(ISWLL) = PTM1AUX(ISWLL) +            &
 1864                                       WADATS(IGRID)%PTM1(GSEA,ISWLL)*WT
 1865                       END IF
 1866                     END IF
 1867 !
 1868                     IF ( FLOGRD(4,13) .AND. ACTIVE ) THEN
 1869                       IF ( WADATS(IGRID)%PT1(GSEA,ISWLL) .NE. UNDEF ) THEN
 1870                         SUMWT4(13,ISWLL) = SUMWT4(13,ISWLL) + WT
 
 
                                                         ww3_gint.F90  page  35
 
 
 1871                         IF ( PT1AUX(ISWLL).EQ.UNDEF ) PT1AUX(ISWLL) = 0.
 1872                         PT1AUX(ISWLL) = PT1AUX(ISWLL) +                &
 1873                                         WADATS(IGRID)%PT1(GSEA,ISWLL)*WT
 1874                       END IF
 1875                     END IF
 1876 !
 1877                     IF ( FLOGRD(4,14) .AND. ACTIVE ) THEN
 1878                       IF ( WADATS(IGRID)%PT2(GSEA,ISWLL) .NE. UNDEF ) THEN
 1879                         SUMWT4(14,ISWLL) = SUMWT4(14,ISWLL) + WT
 1880                         IF ( PT2AUX(ISWLL).EQ.UNDEF ) PT2AUX(ISWLL) = 0.
 1881                         PT2AUX(ISWLL) = PT2AUX(ISWLL) +                &
 1882                                         WADATS(IGRID)%PT2(GSEA,ISWLL)*WT
 1883                       END IF
 1884                     END IF
 1885 !
 1886                     IF ( FLOGRD(4,15) .AND. ACTIVE ) THEN
 1887                       IF ( WADATS(IGRID)%PEP(GSEA,ISWLL) .NE. UNDEF ) THEN
 1888                         SUMWT4(15,ISWLL) = SUMWT4(15,ISWLL) + WT
 1889                         IF ( PEPAUX(ISWLL).EQ.UNDEF ) PEPAUX(ISWLL) = 0.
 1890                         PEPAUX(ISWLL) = PEPAUX(ISWLL) +                &
 1891                                         WADATS(IGRID)%PEP(GSEA,ISWLL)*WT
 1892                       END IF
 1893                     END IF
 1894 !
 1895                   END DO !/ ISWLL = 0, NOSWLL_MIN
 1896 !
 1897                   IF ( FLOGRD(4,16) .AND. ACTIVE ) THEN
 1898                     IF ( WADATS(IGRID)%PWST(GSEA) .NE. UNDEF ) THEN
 1899                         SUMWT4(16,0) = SUMWT4(16,0) + WT
 1900                       IF ( PWSTAUX .EQ. UNDEF ) THEN
 1901                         PWSTAUX = WADATS(IGRID)%PWST(GSEA)*WT
 1902                       ELSE
 1903                         PWSTAUX = PWSTAUX + WADATS(IGRID)%PWST(GSEA)*WT
 1904                       END IF
 1905                     END IF
 1906                   END IF
 1907 !
 1908 ! Group 5 variables
 1909 !
 1910                   IF ( FLOGRD(5,1) ) THEN
 1911                     IF ( WDATAS(IGRID)%UST(GSEA) .NE. UNDEF ) THEN
 1912                       SUMWT5(1) = SUMWT5(1) + WT
 1913                       IF ( USTAUX1 .EQ. UNDEF ) THEN
 1914                         USTAUX1 = WDATAS(IGRID)%UST(GSEA)*WT
 1915                         USTAUX2 = WDATAS(IGRID)%USTDIR(GSEA)*WT
 1916                       ELSE
 1917                         USTAUX1 = USTAUX1 + WDATAS(IGRID)%UST(GSEA)*WT
 1918                         USTAUX2 = USTAUX2 + WDATAS(IGRID)%USTDIR(GSEA)*WT
 1919                       END IF
 1920                     END IF
 1921                   END IF
 1922 !
 1923                   IF ( FLOGRD(5,2) .AND. ACTIVE ) THEN
 1924                     IF ( WADATS(IGRID)%CHARN(GSEA) .NE. UNDEF ) THEN
 1925                       SUMWT5(2) = SUMWT5(2) + WT
 
 
                                                         ww3_gint.F90  page  36
 
 
 1926                       IF ( CHARNAUX .EQ. UNDEF ) THEN
 1927                         CHARNAUX = WADATS(IGRID)%CHARN(GSEA)*WT
 1928                       ELSE
 1929                         CHARNAUX = CHARNAUX + WADATS(IGRID)%CHARN(GSEA)*WT
 1930                       END IF
 1931                     END IF
 1932                   END IF
 1933 !
 1934                   IF ( FLOGRD(5,3) .AND. ACTIVE ) THEN
 1935                     IF ( WADATS(IGRID)%CGE(GSEA) .NE. UNDEF ) THEN
 1936                       SUMWT5(3) = SUMWT5(3) + WT
 1937                       IF ( CGEAUX .EQ. UNDEF ) THEN
 1938                         CGEAUX = WADATS(IGRID)%CGE(GSEA)*WT
 1939                       ELSE
 1940                         CGEAUX = CGEAUX + WADATS(IGRID)%CGE(GSEA)*WT
 1941                       END IF
 1942                     END IF
 1943                   END IF
 1944 !
 1945                   IF ( FLOGRD(5,4) .AND. ACTIVE ) THEN
 1946                     IF ( WADATS(IGRID)%PHIAW(GSEA) .NE. UNDEF ) THEN
 1947                       SUMWT5(4) = SUMWT5(4) + WT
 1948                       IF ( PHIAWAUX .EQ. UNDEF ) THEN
 1949                         PHIAWAUX = WADATS(IGRID)%PHIAW(GSEA)*WT
 1950                       ELSE
 1951                         PHIAWAUX = PHIAWAUX + WADATS(IGRID)%PHIAW(GSEA)*WT
 1952                       END IF
 1953                     END IF
 1954                   END IF
 1955 !
 1956                   IF ( FLOGRD(5,5) .AND. ACTIVE ) THEN
 1957                     IF ( WADATS(IGRID)%TAUWIX(GSEA) .NE. UNDEF ) THEN
 1958                       SUMWT5(5) = SUMWT5(5) + WT
 1959                       IF ( TAUWIXAUX .EQ. UNDEF ) THEN
 1960                         TAUWIXAUX = WADATS(IGRID)%TAUWIX(GSEA)*WT
 1961                         TAUWIYAUX = WADATS(IGRID)%TAUWIY(GSEA)*WT
 1962                       ELSE
 1963                         TAUWIXAUX = TAUWIXAUX + WADATS(IGRID)%TAUWIX(GSEA>
 1964                         TAUWIYAUX = TAUWIYAUX + WADATS(IGRID)%TAUWIY(GSEA>
 1965                       END IF
 1966                     END IF
 1967                   END IF
 1968 !
 1969                   IF ( FLOGRD(5,6) .AND. ACTIVE ) THEN
 1970                     IF ( WADATS(IGRID)%TAUWNX(GSEA) .NE. UNDEF ) THEN
 1971                       SUMWT5(6) = SUMWT5(6) + WT
 1972                       IF ( TAUWNXAUX .EQ. UNDEF ) THEN
 1973                         TAUWNXAUX = WADATS(IGRID)%TAUWNX(GSEA)*WT
 1974                         TAUWNYAUX = WADATS(IGRID)%TAUWNY(GSEA)*WT
 1975                       ELSE
 1976                         TAUWNXAUX = TAUWNXAUX + WADATS(IGRID)%TAUWNX(GSEA>
 1977                         TAUWNYAUX = TAUWNYAUX + WADATS(IGRID)%TAUWNY(GSEA>
 1978                       END IF
 1979                     END IF
 1980                   END IF
 
 
                                                         ww3_gint.F90  page  37
 
 
 1981 !
 1982                   DO ICAP = 1,4
 1983 !
 1984                     IF ( FLOGRD(5,ICAP+6) .AND. ACTIVE ) THEN
 1985                       IF ( WADATS(IGRID)%WHITECAP(GSEA,ICAP) .NE. UNDEF )>
 1986                       SUMWTC(ICAP) = SUMWTC(ICAP) + WT
 1987                         IF ( WHITECAPAUX(ICAP) .EQ. UNDEF ) THEN
 1988                           WHITECAPAUX(ICAP) = WADATS(IGRID)%WHITECAP(GSEA>
 1989                                               *WT
 1990                         ELSE
 1991                           WHITECAPAUX(ICAP) = WHITECAPAUX(ICAP) +        >
 1992                                      WADATS(IGRID)%WHITECAP(GSEA,ICAP)*WT
 1993                         END IF
 1994                       END IF
 1995                     END IF
 1996 !
 1997                   END DO
 1998 !
 1999 ! Group 6 variables
 2000 !
 2001                   IF ( FLOGRD(6,1) .AND. ACTIVE ) THEN
 2002                     IF ( WADATS(IGRID)%SXX(GSEA) .NE. UNDEF ) THEN
 2003                       SUMWT6(1) = SUMWT6(1) + WT
 2004                       IF ( SXXAUX .EQ. UNDEF ) THEN
 2005                         SXXAUX = WADATS(IGRID)%SXX(GSEA)*WT
 2006                         SXYAUX = WADATS(IGRID)%SXY(GSEA)*WT
 2007                         SYYAUX = WADATS(IGRID)%SYY(GSEA)*WT
 2008                       ELSE
 2009                         SXXAUX = SXXAUX + WADATS(IGRID)%SXX(GSEA)*WT
 2010                         SXYAUX = SXYAUX + WADATS(IGRID)%SXY(GSEA)*WT
 2011                         SYYAUX = SYYAUX + WADATS(IGRID)%SYY(GSEA)*WT
 2012                       END IF
 2013                     END IF
 2014                   END IF
 2015 !
 2016                   IF ( FLOGRD(6,2) .AND. ACTIVE ) THEN
 2017                     IF ( WADATS(IGRID)%TAUOX(GSEA) .NE. UNDEF ) THEN
 2018                       SUMWT6(2) = SUMWT6(2) + WT
 2019                       IF ( TAUOXAUX .EQ. UNDEF ) THEN
 2020                         TAUOXAUX = WADATS(IGRID)%TAUOX(GSEA)*WT
 2021                         TAUOYAUX = WADATS(IGRID)%TAUOY(GSEA)*WT
 2022                       ELSE
 2023                         TAUOXAUX = TAUOXAUX + WADATS(IGRID)%TAUOX(GSEA)*WT
 2024                         TAUOYAUX = TAUOYAUX + WADATS(IGRID)%TAUOY(GSEA)*WT
 2025                       END IF
 2026                     END IF
 2027                   END IF
 2028 !
 2029                   IF ( FLOGRD(6,3) .AND. ACTIVE ) THEN
 2030                     IF ( WADATS(IGRID)%BHD(GSEA) .NE. UNDEF ) THEN
 2031                       SUMWT6(3) = SUMWT6(3) + WT
 2032                       IF ( BHDAUX .EQ. UNDEF ) THEN
 2033                         BHDAUX = WADATS(IGRID)%BHD(GSEA)*WT
 2034                       ELSE
 2035                         BHDAUX = BHDAUX + WADATS(IGRID)%BHD(GSEA)*WT
 
 
                                                         ww3_gint.F90  page  38
 
 
 2036                       END IF
 2037                     END IF
 2038                   END IF
 2039 !
 2040                   IF ( FLOGRD(6,4) .AND. ACTIVE ) THEN
 2041                     IF ( WADATS(IGRID)%PHIOC(GSEA) .NE. UNDEF ) THEN
 2042                       SUMWT6(4) = SUMWT6(4) + WT
 2043                       IF ( PHIOCAUX .EQ. UNDEF ) THEN
 2044                         PHIOCAUX = WADATS(IGRID)%PHIOC(GSEA)*WT
 2045                       ELSE
 2046                         PHIOCAUX = PHIOCAUX + WADATS(IGRID)%PHIOC(GSEA)*WT
 2047                       END IF
 2048                     END IF
 2049                   END IF
 2050 !
 2051                   IF ( FLOGRD(6,5) .AND. ACTIVE ) THEN
 2052                     IF ( WADATS(IGRID)%TUSX(GSEA) .NE. UNDEF ) THEN
 2053                       SUMWT6(5) = SUMWT6(5) + WT
 2054                       IF ( TUSXAUX .EQ. UNDEF ) THEN
 2055                         TUSXAUX = WADATS(IGRID)%TUSX(GSEA)*WT
 2056                         TUSYAUX = WADATS(IGRID)%TUSY(GSEA)*WT
 2057                       ELSE
 2058                         TUSXAUX = TUSXAUX + WADATS(IGRID)%TUSX(GSEA)*WT
 2059                         TUSYAUX = TUSYAUX + WADATS(IGRID)%TUSY(GSEA)*WT
 2060                       END IF
 2061                     END IF
 2062                   END IF
 2063 !
 2064                   IF ( FLOGRD(6,6) .AND. ACTIVE ) THEN
 2065                     IF ( WADATS(IGRID)%USSX(GSEA) .NE. UNDEF ) THEN
 2066                       SUMWT6(6) = SUMWT6(6) + WT
 2067                       IF ( USSXAUX .EQ. UNDEF ) THEN
 2068                         USSXAUX = WADATS(IGRID)%USSX(GSEA)*WT
 2069                         USSYAUX = WADATS(IGRID)%USSY(GSEA)*WT
 2070                       ELSE
 2071                         USSXAUX = USSXAUX + WADATS(IGRID)%USSX(GSEA)*WT
 2072                         USSYAUX = USSYAUX + WADATS(IGRID)%USSY(GSEA)*WT
 2073                       END IF
 2074                     END IF
 2075                   END IF
 2076 !
 2077                   IF ( FLOGRD(6,7) .AND. ACTIVE ) THEN
 2078                     IF ( WADATS(IGRID)%PRMS(GSEA) .NE. UNDEF ) THEN
 2079                       SUMWT6(7) = SUMWT6(7) + WT
 2080                       IF ( PRMSAUX .EQ. UNDEF ) THEN
 2081                         PRMSAUX = WADATS(IGRID)%PRMS(GSEA)*WT
 2082                         TPMSAUX = WADATS(IGRID)%TPMS(GSEA)*WT
 2083                       ELSE
 2084                         PRMSAUX = PRMSAUX + WADATS(IGRID)%PRMS(GSEA)*WT
 2085                         TPMSAUX = TPMSAUX + WADATS(IGRID)%TPMS(GSEA)*WT
 2086                       END IF
 2087                     END IF
 2088                   END IF
 2089 !
 2090                   IF ( FLOGRD(6,8) .AND. ACTIVE .AND. US3DF(1).GT.0 ) THEN
 
 
                                                         ww3_gint.F90  page  39
 
 
 2091                     DO IK = US3DF(2),US3DF(3)
 2092                       IF ( WADATS(IGRID)%US3D(GSEA,IK) .NE. UNDEF ) THEN
 2093                         SUMWT68(IK) = SUMWT68(IK) + WT
 2094                         IF ( US3DAUX(IK) .EQ. UNDEF )   US3DAUX(IK) = 0.
 2095                         US3DAUX(IK) = US3DAUX(IK) +                    &
 2096                                           WADATS(IGRID)%US3D(GSEA,IK)*WT
 2097                       END IF
 2098                       IF ( WADATS(IGRID)%US3D(GSEA,NK+IK) .NE. UNDEF ) TH>
 2099                         SUMWT68(NK+IK) = SUMWT68(NK+IK) + WT
 2100                         IF ( US3DAUX(NK+IK) .EQ. UNDEF )               &
 2101                                                      US3DAUX(NK+IK) = 0.
 2102                         US3DAUX(NK+IK) = US3DAUX(NK+IK) +            &
 2103                                      WADATS(IGRID)%US3D(GSEA,NK+IK)*WT
 2104                       END IF
 2105                     END DO
 2106                   END IF
 2107 !
 2108                   IF ( FLOGRD(6,9) .AND. ACTIVE .AND. P2MSF(1).GT.0) THEN
 2109                     DO IK = P2MSF(2),P2MSF(3)
 2110                       IF ( WADATS(IGRID)%P2SMS(GSEA,IK) .NE. UNDEF ) THEN
 2111                         SUMWT69(IK) = SUMWT69(IK) + WT
 2112                         IF ( P2SMSAUX(IK) .EQ. UNDEF ) P2SMSAUX(IK) = 0.
 2113                         P2SMSAUX(IK) = P2SMSAUX(IK) +                  &
 2114                                          WADATS(IGRID)%P2SMS(GSEA,IK)*WT
 2115                       END IF
 2116                     END DO
 2117                   END IF
 2118 !
 2119                   IF ( FLOGRD(6,10) .AND. ACTIVE ) THEN
 2120                     IF ( WADATS(IGRID)%TAUICE(GSEA,1) .NE. UNDEF ) THEN
 2121                       SUMWT6(10) = SUMWT6(10) + WT
 2122                       IF ( TAUICEAUX(1) .EQ. UNDEF ) TAUICEAUX(1) = 0.
 2123                       IF ( TAUICEAUX(2) .EQ. UNDEF ) TAUICEAUX(2) = 0.
 2124                       TAUICEAUX(1) = TAUICEAUX(1) +                    &
 2125                                          WADATS(IGRID)%TAUICE(GSEA,1)*WT
 2126                       TAUICEAUX(2) = TAUICEAUX(2) +                    &
 2127                                          WADATS(IGRID)%TAUICE(GSEA,2)*WT
 2128                     END IF
 2129                   END IF
 2130 !
 2131                   IF ( FLOGRD(6,11) .AND. ACTIVE ) THEN
 2132                     IF ( WADATS(IGRID)%PHICE(GSEA) .NE. UNDEF ) THEN
 2133                       SUMWT6(11) = SUMWT6(11) + WT
 2134                       IF ( PHICEAUX.EQ.UNDEF ) PHICEAUX = 0.
 2135                       PHICEAUX = PHICEAUX + WADATS(IGRID)%PHICE(GSEA)*WT
 2136                     END IF
 2137                   END IF
 2138 !
 2139                   IF ( FLOGRD(6,12) .AND. ACTIVE .AND. USSPF(1).GT.0 ) TH>
 2140                     DO IK = 1,USSPF(2)
 2141                       IF ( WADATS(IGRID)%USSP(GSEA,IK) .NE. UNDEF ) THEN
 2142                         SUMWT612(IK) = SUMWT612(IK) + WT
 2143                         IF ( USSPAUX(IK) .EQ. UNDEF ) USSPAUX(IK) = 0.
 2144                         USSPAUX(IK) = USSPAUX(IK) +                  &
 2145                                         WADATS(IGRID)%USSP(GSEA,IK)*WT
 
 
                                                         ww3_gint.F90  page  40
 
 
 2146                       END IF
 2147                       IF ( WADATS(IGRID)%USSP(GSEA,NK+IK) .NE. UNDEF ) TH>
 2148                         SUMWT612(NK+IK) = SUMWT612(NK+IK) + WT
 2149                         IF ( USSPAUX(NK+IK) .EQ. UNDEF )             &
 2150                                                    USSPAUX(NK+IK) = 0.
 2151                         USSPAUX(NK+IK) = USSPAUX(NK+IK) +            &
 2152                                      WADATS(IGRID)%USSP(GSEA,NK+IK)*WT
 2153                       END IF
 2154                     END DO
 2155                   END IF
 2156 !
 2157 ! Group 7 variables
 2158 !
 2159                   IF ( FLOGRD(7,1) .AND. ACTIVE ) THEN
 2160                     IF ( WADATS(IGRID)%ABA(GSEA) .NE. UNDEF ) THEN
 2161                       SUMWT7(1) = SUMWT7(1) + WT
 2162                       IF ( ABAAUX .EQ. UNDEF ) THEN
 2163                         ABAAUX = WADATS(IGRID)%ABA(GSEA)*WT
 2164                         ABDAUX = WADATS(IGRID)%ABD(GSEA)*WT
 2165                       ELSE
 2166                         ABAAUX = ABAAUX + WADATS(IGRID)%ABA(GSEA)*WT
 2167                         ABDAUX = ABDAUX + WADATS(IGRID)%ABD(GSEA)*WT
 2168                       END IF
 2169                     END IF
 2170                   END IF
 2171 !
 2172                   IF ( FLOGRD(7,2) .AND. ACTIVE ) THEN
 2173                     IF ( WADATS(IGRID)%ABA(GSEA) .NE. UNDEF ) THEN
 2174                       SUMWT7(2) = SUMWT7(2) + WT
 2175                       IF ( UBAAUX .EQ. UNDEF ) THEN
 2176                         UBAAUX = WADATS(IGRID)%UBA(GSEA)*WT
 2177                         UBDAUX = WADATS(IGRID)%UBD(GSEA)*WT
 2178                       ELSE
 2179                         UBAAUX = UBAAUX + WADATS(IGRID)%UBA(GSEA)*WT
 2180                         UBDAUX = UBDAUX + WADATS(IGRID)%UBD(GSEA)*WT
 2181                       END IF
 2182                     END IF
 2183                   END IF
 2184 !
 2185                   IF ( FLOGRD(7,3) .AND. ACTIVE ) THEN
 2186                     DO IBED = 1, 3
 2187                       IF ( WADATS(IGRID)%BEDFORMS(GSEA,IBED) .NE. UNDEF )>
 2188                       SUMWTB(IBED) = SUMWTB(IBED) + WT
 2189                         IF ( BEDFORMSAUX(IBED) .EQ. UNDEF ) THEN
 2190                           BEDFORMSAUX(IBED) = WADATS(IGRID)%BEDFORMS(GSEA>
 2191                                               *WT
 2192                         ELSE
 2193                           BEDFORMSAUX(IBED) = BEDFORMSAUX(IBED) +        >
 2194                                    WADATS(IGRID)%BEDFORMS(GSEA,IBED)*WT
 2195                         END IF
 2196                       END IF
 2197                     END DO
 2198                   END IF
 2199 !
 2200                   IF ( FLOGRD(7,4) .AND. ACTIVE ) THEN
 
 
                                                         ww3_gint.F90  page  41
 
 
 2201                     IF ( WADATS(IGRID)%PHIBBL(GSEA) .NE. UNDEF ) THEN
 2202                       SUMWT7(4) = SUMWT7(4) + WT
 2203                       IF ( PHIBBLAUX .EQ. UNDEF ) THEN
 2204                          PHIBBLAUX = WADATS(IGRID)%PHIBBL(GSEA)*WT
 2205                       ELSE
 2206                         PHIBBLAUX = PHIBBLAUX + WADATS(IGRID)%PHIBBL(GSEA>
 2207                       END IF
 2208                     END IF
 2209                   END IF
 2210 !
 2211                   IF ( FLOGRD(7,5) .AND. ACTIVE ) THEN
 2212                     IF ( WADATS(IGRID)%TAUBBL(GSEA,1) .NE. UNDEF ) THEN
 2213                       SUMWT7(5) = SUMWT7(5) + WT
 2214                       IF ( TAUBBLAUX(1) .EQ. UNDEF ) THEN
 2215                         TAUBBLAUX(1) = WADATS(IGRID)%TAUBBL(GSEA,1)*WT
 2216                         TAUBBLAUX(2) = WADATS(IGRID)%TAUBBL(GSEA,2)*WT
 2217                       ELSE
 2218                         TAUBBLAUX(1) = TAUBBLAUX(1) +                    >
 2219                                 WADATS(IGRID)%TAUBBL(GSEA,1)*WT
 2220                         TAUBBLAUX(2) = TAUBBLAUX(2) +                    >
 2221                                 WADATS(IGRID)%TAUBBL(GSEA,2)*WT
 2222                       END IF
 2223                     END IF
 2224                   END IF
 2225 !
 2226 ! Group 8 variables
 2227 !
 2228                   IF ( FLOGRD(8,1) .AND. ACTIVE ) THEN
 2229                     IF ( WADATS(IGRID)%MSSX(GSEA) .NE. UNDEF ) THEN
 2230                       SUMWT8(1) = SUMWT8(1) + WT
 2231                       IF ( MSSXAUX .EQ. UNDEF )   MSSXAUX = 0.
 2232                       IF ( MSSYAUX .EQ. UNDEF )   MSSYAUX = 0.
 2233                       MSSXAUX = MSSXAUX + WADATS(IGRID)%MSSX(GSEA)*WT
 2234                       MSSYAUX = MSSYAUX + WADATS(IGRID)%MSSY(GSEA)*WT
 2235                     END IF
 2236                   END IF
 2237 !
 2238                   IF ( FLOGRD(8,2) .AND. ACTIVE ) THEN
 2239                     IF ( WADATS(IGRID)%MSCX(GSEA) .NE. UNDEF ) THEN
 2240                       SUMWT8(2) = SUMWT8(2) + WT
 2241                       IF ( MSCXAUX .EQ. UNDEF )   MSCXAUX = 0.
 2242                       IF ( MSCYAUX .EQ. UNDEF )   MSCYAUX = 0.
 2243                       MSCXAUX = MSCXAUX + WADATS(IGRID)%MSSX(GSEA)*WT
 2244                       MSCYAUX = MSCYAUX + WADATS(IGRID)%MSSY(GSEA)*WT
 2245                     END IF
 2246                   END IF
 2247 !
 2248                   IF ( FLOGRD(8,3) .AND. ACTIVE ) THEN
 2249                     IF ( WADATS(IGRID)%MSSD(GSEA) .NE. UNDEF ) THEN
 2250                       SUMWT8(3) = SUMWT8(3) + WT
 2251                       IF ( MSSDAUX1 .EQ. UNDEF )   MSSDAUX1 = 0.
 2252                       IF ( MSSDAUX2 .EQ. UNDEF )   MSSDAUX2 = 0.
 2253                       MSSDAUX1 = MSSDAUX1 +                           &
 2254                                  COS ( WADATS(IGRID)%MSSD(GSEA) )*WT
 2255                       MSSDAUX2 = MSSDAUX2 +                           &
 
 
                                                         ww3_gint.F90  page  42
 
 
 2256                                  SIN ( WADATS(IGRID)%MSSD(GSEA) )*WT
 2257                     END IF
 2258                   END IF
 2259 !
 2260                   IF ( FLOGRD(8,4) .AND. ACTIVE ) THEN
 2261                     IF ( WADATS(IGRID)%MSCD(GSEA) .NE. UNDEF ) THEN
 2262                       SUMWT8(4) = SUMWT8(4) + WT
 2263                       IF ( MSCDAUX1 .EQ. UNDEF )   MSCDAUX1 = 0.
 2264                       IF ( MSCDAUX2 .EQ. UNDEF )   MSCDAUX2 = 0.
 2265                       MSCDAUX1 = MSCDAUX1 +                           &
 2266                                  COS ( WADATS(IGRID)%MSCD(GSEA) )*WT
 2267                       MSCDAUX2 = MSCDAUX2 +                           &
 2268                                  SIN ( WADATS(IGRID)%MSCD(GSEA) )*WT
 2269                     END IF
 2270                   END IF
 2271 !
 2272                   IF ( FLOGRD(8,5) .AND. ACTIVE ) THEN
 2273                     IF ( WADATS(IGRID)%QP(GSEA) .NE. UNDEF ) THEN
 2274                       SUMWT8(5) = SUMWT8(5) + WT
 2275                       IF ( QPAUX .EQ. UNDEF )   QPAUX = 0.
 2276                       QPAUX = QPAUX + WADATS(IGRID)%QP(GSEA)*WT
 2277                     END IF
 2278                   END IF
 2279 !
 2280 ! End of loop through the points per grid to obtain interpolated values
 2281                 END DO   !/ IPTS = 1, ...
 2282 !
 2283 ! Save temp. interpolated variables in proper variables
 2284 ! (weighted by the number of grids)
 2285 !
 2286 ! Group 1 variables
 2287 !
 2288                 IF ( DWAUX .NE. UNDEF ) THEN
 2289                   DWAUX = DWAUX / SUMWT1(1)
 2290                   IF ( DW(ISEA) .EQ. UNDEF )  THEN
 2291                     DW(ISEA) = DWAUX / REAL( SUMGRD )
 2292                   ELSE
 2293                     DW(ISEA) = DW(ISEA) + DWAUX / REAL( SUMGRD )
 2294                   END IF
 2295                 END IF
 2296 !
 2297                 IF ( CXAUX .NE. UNDEF ) THEN
 2298                   CXAUX = CXAUX / SUMWT1(2)
 2299                   CYAUX = CYAUX / SUMWT1(2)
 2300                   IF ( CX(ISEA) .EQ. UNDEF )  THEN
 2301                     CX(ISEA) = CXAUX / REAL( SUMGRD )
 2302                     CY(ISEA) = CYAUX / REAL( SUMGRD )
 2303                   ELSE
 2304                     CX(ISEA) = CX(ISEA) + CXAUX / REAL( SUMGRD )
 2305                     CY(ISEA) = CY(ISEA) + CYAUX / REAL( SUMGRD )
 2306                   END IF
 2307                 END IF
 2308 !
 2309                 IF ( UAAUX .NE. UNDEF ) THEN
 2310                   UAAUX = UAAUX / SUMWT1(3)
 
 
                                                         ww3_gint.F90  page  43
 
 
 2311                   UDAUX = UDAUX / SUMWT1(3)
 2312                   IF ( UA(ISEA) .EQ. UNDEF )  THEN
 2313                     UA(ISEA) = UAAUX / REAL( SUMGRD )
 2314                     UD(ISEA) = UDAUX / REAL( SUMGRD )
 2315                   ELSE
 2316                     UA(ISEA) = UA(ISEA) + UAAUX / REAL( SUMGRD )
 2317                     UD(ISEA) = UD(ISEA) + UDAUX / REAL( SUMGRD )
 2318                   END IF
 2319                 END IF
 2320 !
 2321                 IF ( ASAUX .NE. UNDEF ) THEN
 2322                   ASAUX = ASAUX / SUMWT1(4)
 2323                   IF ( AS(ISEA) .EQ. UNDEF )  THEN
 2324                     AS(ISEA) = ASAUX / REAL( SUMGRD )
 2325                   ELSE
 2326                     AS(ISEA) = AS(ISEA) + ASAUX / REAL( SUMGRD )
 2327                   END IF
 2328                 END IF
 2329 !
 2330                 IF ( WLVAUX .NE. UNDEF ) THEN
 2331                   WLVAUX = WLVAUX / SUMWT1(5)
 2332                   IF ( WLV(ISEA) .EQ. UNDEF )  THEN
 2333                     WLV(ISEA) = WLVAUX / REAL( SUMGRD )
 2334                   ELSE
 2335                     WLV(ISEA) = WLV(ISEA) + WLVAUX / REAL( SUMGRD )
 2336                   END IF
 2337                 END IF
 2338 !
 2339                 IF ( ICEAUX .NE. UNDEF ) THEN
 2340                   ICEAUX = ICEAUX / SUMWT1(6)
 2341                   IF ( ICE(ISEA) .EQ. UNDEF )  THEN
 2342                     ICE(ISEA) = ICEAUX / REAL( SUMGRD )
 2343                   ELSE
 2344                     ICE(ISEA) = ICE(ISEA) + ICEAUX / REAL( SUMGRD )
 2345                   END IF
 2346                 END IF
 2347 !
 2348                 IF ( BERGAUX .NE. UNDEF ) THEN
 2349                   BERGAUX = BERGAUX / SUMWT1(7)
 2350                   IF ( BERG(ISEA) .EQ. UNDEF )  THEN
 2351                     BERG(ISEA) = BERGAUX / REAL( SUMGRD )
 2352                   ELSE
 2353                     BERG(ISEA) = BERG(ISEA) + BERGAUX / REAL( SUMGRD )
 2354                   END IF
 2355                 END IF
 2356 !
 2357 ! Group 2 variables
 2358 !
 2359                 IF ( HSAUX .NE. UNDEF ) THEN
 2360                   HSAUX = HSAUX / SUMWT2(1)
 2361                   IF ( HS(ISEA) .EQ. UNDEF )  THEN
 2362                     HS(ISEA) = HSAUX / REAL( SUMGRD )
 2363                   ELSE
 2364                     HS(ISEA) = HS(ISEA) + HSAUX / REAL( SUMGRD )
 2365                   END IF
 
 
                                                         ww3_gint.F90  page  44
 
 
 2366                 END IF
 2367 !
 2368                 IF ( WLMAUX .NE. UNDEF ) THEN
 2369                   WLMAUX = WLMAUX / SUMWT2(2)
 2370                   IF ( WLM(ISEA) .EQ. UNDEF )  THEN
 2371                     WLM(ISEA) = WLMAUX / REAL( SUMGRD )
 2372                   ELSE
 2373                     WLM(ISEA) = WLM(ISEA) + WLMAUX / REAL( SUMGRD )
 2374                   END IF
 2375                 END IF
 2376 !
 2377                 IF ( T02AUX .NE. UNDEF ) THEN
 2378                   T02AUX = T02AUX / SUMWT2(3)
 2379                   IF ( T02(ISEA) .EQ. UNDEF )  THEN
 2380                     T02(ISEA) = T02AUX / REAL( SUMGRD )
 2381                   ELSE
 2382                     T02(ISEA) = T02(ISEA) + T02AUX / REAL( SUMGRD )
 2383                   END IF
 2384                 END IF
 2385 !
 2386                 IF ( T0M1AUX .NE. UNDEF ) THEN
 2387                   T0M1AUX = T0M1AUX / SUMWT2(4)
 2388                   IF ( T0M1(ISEA) .EQ. UNDEF )  THEN
 2389                     T0M1(ISEA) = T0M1AUX / REAL( SUMGRD )
 2390                   ELSE
 2391                     T0M1(ISEA) = T0M1(ISEA) + T0M1AUX / REAL( SUMGRD )
 2392                   END IF
 2393                 END IF
 2394 !
 2395                 IF ( T01AUX .NE. UNDEF ) THEN
 2396                   T01AUX = T01AUX / SUMWT2(5)
 2397                   IF ( T01(ISEA) .EQ. UNDEF )  THEN
 2398                     T01(ISEA) = T01AUX / REAL( SUMGRD )
 2399                   ELSE
 2400                     T01(ISEA) = T01(ISEA) + T01AUX / REAL( SUMGRD )
 2401                   END IF
 2402                 END IF
 2403 !
 2404                 IF ( FP0AUX .NE. UNDEF ) THEN
 2405                   FP0AUX = FP0AUX / SUMWT2(6)
 2406                   IF ( FP0(ISEA) .EQ. UNDEF )  THEN
 2407                     FP0(ISEA) = FP0AUX / REAL( SUMGRD )
 2408                   ELSE
 2409                     FP0(ISEA) = FP0(ISEA) + FP0AUX / REAL( SUMGRD )
 2410                   END IF
 2411                 END IF
 2412 !
 2413                 IF ( THMAUX1 .NE. UNDEF ) THEN
 2414                   THMAUX1 = THMAUX1 / SUMWT2(7)
 2415                   THMAUX2 = THMAUX2 / SUMWT2(7)
 2416                   IF ( THM(ISEA) .EQ. UNDEF )  THEN
 2417                     THMAUX1 = THMAUX1 / REAL( SUMGRD )
 2418                     THMAUX2 = THMAUX2 / REAL( SUMGRD )
 2419                     THM(ISEA) = ATAN2 ( THMAUX2, THMAUX1 )
 2420                   ELSE
 
 
                                                         ww3_gint.F90  page  45
 
 
 2421                     THMAUX1 = THMAUX1 / REAL( SUMGRD ) +  COS ( THM(ISEA)>
 2422                     THMAUX2 = THMAUX2 / REAL( SUMGRD ) +  SIN ( THM(ISEA)>
 2423                     THM(ISEA) = ATAN2 ( THMAUX2, THMAUX1 )
 2424                   END IF
 2425                 END IF
 2426 !
 2427                 IF ( THSAUX .NE. UNDEF ) THEN
 2428                   THSAUX = THSAUX / SUMWT2(8)
 2429                   IF ( THS(ISEA) .EQ. UNDEF )  THEN
 2430                     THS(ISEA) = THSAUX / REAL( SUMGRD )
 2431                   ELSE
 2432                     THS(ISEA) = THS(ISEA) + THSAUX / REAL( SUMGRD )
 2433                   END IF
 2434                 END IF
 2435 !
 2436                 IF ( THP0AUX1 .NE. UNDEF ) THEN
 2437                   THP0AUX1 = THP0AUX1 / SUMWT2(9)
 2438                   THP0AUX2 = THP0AUX2 / SUMWT2(9)
 2439                   IF ( THP0(ISEA) .EQ. UNDEF )  THEN
 2440                     THP0AUX1 = THP0AUX1 / REAL( SUMGRD )
 2441                     THP0AUX2 = THP0AUX2 / REAL( SUMGRD )
 2442                     THP0(ISEA) = ATAN2 ( THP0AUX2, THP0AUX1 )
 2443                   ELSE
 2444                     THP0AUX1 = THP0AUX1 / REAL( SUMGRD ) + COS ( THP0(ISE>
 2445                     THP0AUX2 = THP0AUX2 / REAL( SUMGRD ) + SIN ( THP0(ISE>
 2446                     THP0(ISEA) = ATAN2 ( THP0AUX2, THP0AUX1 )
 2447                   END IF
 2448                 END IF
 2449 !
 2450                 IF ( HSIGAUX .NE. UNDEF ) THEN
 2451                   IF ( HSIG(ISEA) .EQ. UNDEF )   HSIG(ISEA) = 0.
 2452                   HSIG(ISEA) = HSIG(ISEA) +                            &
 2453                                HSIGAUX / REAL( SUMWT2(10)+SUMGRD )
 2454                 END IF
 2455 !
 2456                 IF ( STMAXEAUX .NE. UNDEF ) THEN
 2457                   IF ( STMAXE(ISEA) .EQ. UNDEF )   STMAXE(ISEA) = 0.
 2458                   STMAXE(ISEA) = STMAXE(ISEA) +                        &
 2459                                STMAXEAUX / REAL( SUMWT2(11) + SUMGRD )
 2460                 END IF
 2461 !
 2462                 IF ( STMAXDAUX .NE. UNDEF ) THEN
 2463                   IF ( STMAXD(ISEA) .EQ. UNDEF )   STMAXD(ISEA) = 0.
 2464                   STMAXD(ISEA) = STMAXD(ISEA) +                        &
 2465                                  STMAXDAUX / REAL( SUMWT2(12) + SUMGRD )
 2466                 END IF
 2467 !
 2468                 IF ( HMAXEAUX .NE. UNDEF ) THEN
 2469                   IF ( HMAXE(ISEA) .EQ. UNDEF )   HMAXE(ISEA) = 0.
 2470                   HMAXE(ISEA) = HMAXE(ISEA) +                          &
 2471                                 HMAXEAUX / REAL( SUMWT2(13) + SUMGRD )
 2472                 END IF
 2473 !
 2474                 IF ( HCMAXEAUX .NE. UNDEF ) THEN
 2475                   IF ( HCMAXE(ISEA) .EQ. UNDEF )   HCMAXE(ISEA) = 0.
 
 
                                                         ww3_gint.F90  page  46
 
 
 2476                   HCMAXE(ISEA) = HCMAXE(ISEA) +                        &
 2477                                  HCMAXEAUX / REAL( SUMWT2(14) + SUMGRD )
 2478                 END IF
 2479 !
 2480                 IF ( HMAXDAUX .NE. UNDEF ) THEN
 2481                   IF ( HMAXD(ISEA) .EQ. UNDEF )   HMAXD(ISEA) = 0.
 2482                   HMAXD(ISEA) = HMAXD(ISEA)  +                         &
 2483                                 HMAXDAUX / REAL( SUMWT2(15) + SUMGRD )
 2484                 END IF
 2485 !
 2486                 IF ( HCMAXDAUX .NE. UNDEF ) THEN
 2487                   IF ( HCMAXD(ISEA) .EQ. UNDEF )   HCMAXD(ISEA) = 0.
 2488                   HCMAXD(ISEA) = HCMAXD(ISEA) +                        &
 2489                                  HCMAXDAUX / REAL( SUMWT2(16) + SUMGRD )
 2490                 END IF
 2491 !
 2492                 IF ( WBTAUX .NE. UNDEF ) THEN
 2493                   IF ( WBT(ISEA) .EQ. UNDEF )   WBT(ISEA) = 0.
 2494                   WBT(ISEA) = WBT(ISEA) +                            &
 2495                                WBTAUX / REAL( SUMWT2(17)+SUMGRD )
 2496                 END IF
 2497 !
 2498 ! Group 3 variables
 2499 !
 2500                 IF (  E3DF(1,1).GT.0 ) THEN
 2501                   DO IFREQ = E3DF(2,1),E3DF(3,1)
 2502                     IF ( EFAUX(IFREQ) .NE. UNDEF ) THEN
 2503                       EFAUX(IFREQ) = EFAUX(IFREQ) / SUMWT3A(IFREQ)
 2504                       IF ( EF(ISEA,IFREQ) .EQ. UNDEF )  THEN
 2505                         EF(ISEA,IFREQ) = EFAUX(IFREQ) / REAL( SUMGRD )
 2506                       ELSE
 2507                         EF(ISEA,IFREQ) = EF(ISEA,IFREQ) +             &
 2508                             EFAUX(IFREQ) / REAL( SUMGRD )
 2509                       END IF
 2510                     END IF
 2511                   END DO
 2512                 END IF
 2513 !
 2514                 IF (  E3DF(1,2).GT.0 ) THEN
 2515                   DO IFREQ = E3DF(2,2),E3DF(3,2)
 2516                     IF ( TH1MAUX(IFREQ) .NE. UNDEF ) THEN
 2517                       TH1MAUX(IFREQ) = TH1MAUX(IFREQ) / SUMWT3B(IFREQ)
 2518                       IF ( TH1M(ISEA,IFREQ) .EQ. UNDEF )  THEN
 2519                         TH1M(ISEA,IFREQ) = TH1MAUX(IFREQ) / REAL( SUMGRD )
 2520                       ELSE
 2521                         TH1M(ISEA,IFREQ) = TH1M(ISEA,IFREQ) +         &
 2522                             TH1MAUX(IFREQ) / REAL( SUMGRD )
 2523                       END IF
 2524                     END IF
 2525                   END DO
 2526                 END IF
 2527 !
 2528                 IF (  E3DF(1,3).GT.0 ) THEN
 2529                   DO IFREQ = E3DF(2,3),E3DF(3,3)
 2530                     IF ( STH1MAUX(IFREQ) .NE. UNDEF ) THEN
 
 
                                                         ww3_gint.F90  page  47
 
 
 2531                       STH1MAUX(IFREQ) = STH1MAUX(IFREQ) / SUMWT3C(IFREQ)
 2532                       IF ( STH1M(ISEA,IFREQ) .EQ. UNDEF )  THEN
 2533                         STH1M(ISEA,IFREQ) = STH1MAUX(IFREQ) / REAL( SUMGR>
 2534                       ELSE
 2535                         STH1M(ISEA,IFREQ) = STH1M(ISEA,IFREQ) +       &
 2536                             STH1MAUX(IFREQ) / REAL( SUMGRD )
 2537                       END IF
 2538                     END IF
 2539                   END DO
 2540                 END IF
 2541 !
 2542                 IF (  E3DF(1,4).GT.0 ) THEN
 2543                   DO IFREQ = E3DF(2,4),E3DF(3,4)
 2544                     IF ( TH2MAUX(IFREQ) .NE. UNDEF ) THEN
 2545                       TH2MAUX(IFREQ) = TH2MAUX(IFREQ) / SUMWT3D(IFREQ)
 2546                       IF ( TH2M(ISEA,IFREQ) .EQ. UNDEF )  THEN
 2547                         TH2M(ISEA,IFREQ) = TH2MAUX(IFREQ) / REAL( SUMGRD )
 2548                       ELSE
 2549                         TH2M(ISEA,IFREQ) = TH2M(ISEA,IFREQ) +         &
 2550                             TH2MAUX(IFREQ) / REAL( SUMGRD )
 2551                       END IF
 2552                     END IF
 2553                   END DO
 2554                 END IF
 2555 !
 2556                 IF (  E3DF(1,5).GT.0 ) THEN
 2557                   DO IFREQ = E3DF(2,5),E3DF(3,5)
 2558                     IF ( STH2MAUX(IFREQ) .NE. UNDEF ) THEN
 2559                       STH2MAUX(IFREQ) = STH2MAUX(IFREQ) / SUMWT3E(IFREQ)
 2560                       IF ( STH2M(ISEA,IFREQ) .EQ. UNDEF )  THEN
 2561                         STH2M(ISEA,IFREQ) = STH2MAUX(IFREQ) / REAL( SUMGR>
 2562                       ELSE
 2563                         STH2M(ISEA,IFREQ) = STH2M(ISEA,IFREQ) +       &
 2564                             STH2MAUX(IFREQ) / REAL( SUMGRD )
 2565                       END IF
 2566                     END IF
 2567                   END DO
 2568                 END IF
 2569 !
 2570                 DO IK = 1,NK
 2571                   IF ( WNAUX(IK) .NE. UNDEF ) THEN
 2572                     WNAUX(IK) = WNAUX(IK) / SUMWT3F(IK)
 2573                     IF ( WN(IK,ISEA) .EQ. UNDEF )  THEN
 2574                       WN(IK,ISEA) = WNAUX(IK) / REAL( SUMGRD )
 2575                     ELSE
 2576                       WN(IK,ISEA) = WN(IK,ISEA) +                     &
 2577                           WNAUX(IK) / REAL( SUMGRD )
 2578                     END IF
 2579                   END IF
 2580                 END DO
 2581 !
 2582 ! Group 4 variables
 2583 !
 2584                 DO ISWLL = 0, NOSWLL_MIN
 2585 !
 
 
                                                         ww3_gint.F90  page  48
 
 
 2586                   IF ( PHSAUX(ISWLL) .NE. UNDEF ) THEN
 2587                     PHSAUX(ISWLL) = PHSAUX(ISWLL) / SUMWT4(1,ISWLL)
 2588                     IF ( PHS(ISEA,ISWLL) .EQ. UNDEF )  THEN
 2589                       PHS(ISEA,ISWLL) = PHSAUX(ISWLL) / REAL( SUMGRD )
 2590                     ELSE
 2591                       PHS(ISEA,ISWLL) = PHS(ISEA,ISWLL) +                >
 2592                           PHSAUX(ISWLL) / REAL( SUMGRD )
 2593                     END IF
 2594                   END IF
 2595 !
 2596                   IF ( PTPAUX(ISWLL) .NE. UNDEF ) THEN
 2597                     PTPAUX(ISWLL) = PTPAUX(ISWLL) / SUMWT4(2,ISWLL)
 2598                     IF ( PTP(ISEA,ISWLL) .EQ. UNDEF )  THEN
 2599                       PTP(ISEA,ISWLL) = PTPAUX(ISWLL) / REAL( SUMGRD )
 2600                     ELSE
 2601                       PTP(ISEA,ISWLL) = PTP(ISEA,ISWLL) +                >
 2602                           PTPAUX(ISWLL) / REAL( SUMGRD )
 2603                     END IF
 2604                   END IF
 2605 !
 2606                   IF ( PLPAUX(ISWLL) .NE. UNDEF ) THEN
 2607                     PLPAUX(ISWLL) = PLPAUX(ISWLL) / SUMWT4(3,ISWLL)
 2608                     IF ( PLP(ISEA,ISWLL) .EQ. UNDEF )  THEN
 2609                       PLP(ISEA,ISWLL) = PLPAUX(ISWLL) / REAL( SUMGRD )
 2610                     ELSE
 2611                       PLP(ISEA,ISWLL) = PLP(ISEA,ISWLL) +                >
 2612                           PLPAUX(ISWLL) / REAL( SUMGRD )
 2613                     END IF
 2614                   END IF
 2615 !
 2616                   IF ( PDIRAUX1(ISWLL) .NE. UNDEF ) THEN
 2617                     PDIRAUX1(ISWLL) = PDIRAUX1(ISWLL) / SUMWT4(4,ISWLL)
 2618                     PDIRAUX2(ISWLL) = PDIRAUX2(ISWLL) / SUMWT4(4,ISWLL)
 2619                     IF ( PDIR(ISEA,ISWLL) .EQ. UNDEF )  THEN
 2620                       PDIRAUX1(ISWLL) = PDIRAUX1(ISWLL) / REAL( SUMGRD )
 2621                       PDIRAUX2(ISWLL) = PDIRAUX2(ISWLL) / REAL( SUMGRD )
 2622                       PDIR(ISEA,ISWLL) = ATAN2 ( PDIRAUX2(ISWLL), PDIRAUX>
 2623                     ELSE
 2624                       PDIRAUX1(ISWLL) = PDIRAUX1(ISWLL) / REAL( SUMGRD ) +
 2625                                COS ( PDIR(ISEA,ISWLL) )
 2626                       PDIRAUX2(ISWLL) = PDIRAUX2(ISWLL) / REAL( SUMGRD ) +
 2627                                SIN ( PDIR(ISEA,ISWLL) )
 2628                       PDIR(ISEA,ISWLL) = ATAN2 ( PDIRAUX2(ISWLL), PDIRAUX>
 2629                     END IF
 2630                   END IF
 2631 !
 2632                   IF ( PSIAUX(ISWLL) .NE. UNDEF ) THEN
 2633                     PSIAUX(ISWLL) = PSIAUX(ISWLL) / SUMWT4(5,ISWLL)
 2634                     IF ( PSI(ISEA,ISWLL) .EQ. UNDEF )  THEN
 2635                       PSI(ISEA,ISWLL) = PSIAUX(ISWLL) / REAL( SUMGRD )
 2636                     ELSE
 2637                       PSI(ISEA,ISWLL) = PSI(ISEA,ISWLL) +                >
 2638                           PSIAUX(ISWLL) / REAL( SUMGRD )
 2639                     END IF
 2640                   END IF
 
 
                                                         ww3_gint.F90  page  49
 
 
 2641 !
 2642                   IF ( PWSAUX(ISWLL) .NE. UNDEF ) THEN
 2643                     PWSAUX(ISWLL) = PWSAUX(ISWLL) / SUMWT4(6,ISWLL)
 2644                     IF ( PWS(ISEA,ISWLL) .EQ. UNDEF )  THEN
 2645                       PWS(ISEA,ISWLL) = PWSAUX(ISWLL) / REAL( SUMGRD )
 2646                     ELSE
 2647                       PWS(ISEA,ISWLL) = PWS(ISEA,ISWLL) +                >
 2648                           PWSAUX(ISWLL) / REAL( SUMGRD )
 2649                     END IF
 2650                   END IF
 2651 !
 2652                   IF ( PTHP0AUX1(ISWLL) .NE. UNDEF ) THEN
 2653                     PTHP0AUX1(ISWLL) = PTHP0AUX1(ISWLL)                &
 2654                                         / REAL( SUMWT4(7,ISWLL)+SUMGRD )
 2655                     PTHP0AUX2(ISWLL) = PTHP0AUX2(ISWLL)                &
 2656                                         / REAL( SUMWT4(7,ISWLL)+SUMGRD )
 2657                     IF ( PTHP0(ISEA,ISWLL) .NE. UNDEF ) THEN
 2658                       PTHP0AUX1(ISWLL) = PTHP0AUX1(ISWLL) +            &
 2659                                                COS ( PTHP0(ISEA,ISWLL) )
 2660                       PTHP0AUX2(ISWLL) = PTHP0AUX2(ISWLL) +            &
 2661                                                SIN ( PTHP0(ISEA,ISWLL) )
 2662                     END IF
 2663                     PTHP0(ISEA,ISWLL) =                                &
 2664                             ATAN2 ( PTHP0AUX2(ISWLL), PTHP0AUX1(ISWLL) )
 2665                   END IF
 2666 !
 2667                   IF ( PQPAUX(ISWLL) .NE. UNDEF ) THEN
 2668                     IF ( PQP(ISEA,ISWLL) .EQ. UNDEF )                  &
 2669                                                     PQP(ISEA,ISWLL) = 0.
 2670                     PQP(ISEA,ISWLL) = PQP(ISEA,ISWLL) +                &
 2671                          PQPAUX(ISWLL) / REAL( SUMWT4(8,ISWLL)+SUMGRD )
 2672                   END IF
 2673 !
 2674                   IF ( PPEAUX(ISWLL) .NE. UNDEF ) THEN
 2675                     IF ( PPE(ISEA,ISWLL) .EQ. UNDEF )                  &
 2676                                                     PPE(ISEA,ISWLL) = 0.
 2677                     PPE(ISEA,ISWLL) = PPE(ISEA,ISWLL) +                &
 2678                          PPEAUX(ISWLL) / REAL( SUMWT4(9,ISWLL)+SUMGRD )
 2679                                                                        
 2680                   END IF
 2681 !
 2682                   IF ( PGWAUX(ISWLL) .NE. UNDEF ) THEN
 2683                     IF ( PGW(ISEA,ISWLL) .EQ. UNDEF )                  &
 2684                                                     PGW(ISEA,ISWLL) = 0.
 2685                     PGW(ISEA,ISWLL) = PGW(ISEA,ISWLL) +                &
 2686                          PGWAUX(ISWLL) / REAL( SUMWT4(10,ISWLL)+SUMGRD )
 2687                   END IF
 2688 !
 2689                   IF ( PSWAUX(ISWLL) .NE. UNDEF ) THEN
 2690                     IF ( PSW(ISEA,ISWLL) .EQ. UNDEF )                  &
 2691                                                     PSW(ISEA,ISWLL) = 0.
 2692                     PSW(ISEA,ISWLL) = PSW(ISEA,ISWLL) +                &
 2693                          PSWAUX(ISWLL) / REAL( SUMWT4(11,ISWLL)+SUMGRD )
 2694                   END IF
 2695 !
 
 
                                                         ww3_gint.F90  page  50
 
 
 2696                   IF ( PTM1AUX(ISWLL) .NE. UNDEF ) THEN
 2697                     IF ( PTM1(ISEA,ISWLL) .EQ. UNDEF )                &
 2698                                                   PTM1(ISEA,ISWLL) = 0.
 2699                     PTM1(ISEA,ISWLL) = PTM1(ISEA,ISWLL) +            &
 2700                        PTM1AUX(ISWLL) / REAL( SUMWT4(12,ISWLL)+SUMGRD )
 2701                   END IF
 2702 !
 2703                   IF ( PT1AUX(ISWLL) .NE. UNDEF ) THEN
 2704                     IF ( PT1(ISEA,ISWLL) .EQ. UNDEF )                  &
 2705                                                     PT1(ISEA,ISWLL) = 0.
 2706                     PT1(ISEA,ISWLL) = PT1(ISEA,ISWLL) +                &
 2707                          PT1AUX(ISWLL) / REAL( SUMWT4(13,ISWLL)+SUMGRD )
 2708                   END IF
 2709 !
 2710                   IF ( PT2AUX(ISWLL) .NE. UNDEF ) THEN
 2711                     IF ( PT2(ISEA,ISWLL) .EQ. UNDEF )                  &
 2712                                                     PT2(ISEA,ISWLL) = 0.
 2713                     PT2(ISEA,ISWLL) = PT2(ISEA,ISWLL) +                &
 2714                          PT2AUX(ISWLL) / REAL( SUMWT4(14,ISWLL)+SUMGRD )
 2715                   END IF
 2716 !
 2717                   IF ( PEPAUX(ISWLL) .NE. UNDEF ) THEN
 2718                     IF ( PEP(ISEA,ISWLL) .EQ. UNDEF )                  &
 2719                                                     PEP(ISEA,ISWLL) = 0.
 2720                     PEP(ISEA,ISWLL) = PEP(ISEA,ISWLL) +                &
 2721                          PEPAUX(ISWLL) / REAL( SUMWT4(15,ISWLL)+SUMGRD )
 2722                   END IF
 2723 !
 2724                 END DO  !/ ISWLL = 0, NOSWLL_MIN
 2725 !
 2726                 IF ( PWSTAUX .NE. UNDEF ) THEN
 2727                   PWSTAUX = PWSTAUX / SUMWT4(16,0)
 2728                   IF ( PWST(ISEA) .EQ. UNDEF )  THEN
 2729                     PWST(ISEA) = PWSTAUX / REAL( SUMGRD )
 2730                   ELSE
 2731                     PWST(ISEA) = PWST(ISEA) + PWSTAUX / REAL( SUMGRD )
 2732                   END IF
 2733                 END IF
 2734 !
 2735 ! Group 5 variables
 2736 !
 2737                 IF ( USTAUX1 .NE. UNDEF ) THEN
 2738                   USTAUX1 = USTAUX1 / SUMWT5(1)
 2739                   USTAUX2 = USTAUX2 / SUMWT5(1)
 2740                   IF ( UST(ISEA) .EQ. UNDEF )  THEN
 2741                     UST(ISEA) = USTAUX1 / REAL( SUMGRD )
 2742                     USTDIR(ISEA) = USTAUX2 / REAL( SUMGRD )
 2743                   ELSE
 2744                     UST(ISEA) = UST(ISEA) + USTAUX1 / REAL( SUMGRD )
 2745                     USTDIR(ISEA) = USTDIR(ISEA) + USTAUX2 / REAL( SUMGRD )
 2746                   END IF
 2747                 END IF
 2748 !
 2749                 IF ( CHARNAUX .NE. UNDEF ) THEN
 2750                   CHARNAUX = CHARNAUX / SUMWT5(2)
 
 
                                                         ww3_gint.F90  page  51
 
 
 2751                   IF ( CHARN(ISEA) .EQ. UNDEF )  THEN
 2752                     CHARN(ISEA) = CHARNAUX / REAL( SUMGRD )
 2753                   ELSE
 2754                     CHARN(ISEA) = CHARN(ISEA) + CHARNAUX / REAL( SUMGRD )
 2755                   END IF
 2756                 END IF
 2757 !
 2758                 IF ( CGEAUX .NE. UNDEF ) THEN
 2759                   CGEAUX = CGEAUX / SUMWT5(3)
 2760                   IF ( CGE(ISEA) .EQ. UNDEF )  THEN
 2761                     CGE(ISEA) = CGEAUX / REAL( SUMGRD )
 2762                   ELSE
 2763                     CGE(ISEA) = CGE(ISEA) + CGEAUX / REAL( SUMGRD )
 2764                   END IF
 2765                 END IF
 2766 !
 2767                 IF ( PHIAWAUX .NE. UNDEF ) THEN
 2768                   PHIAWAUX = PHIAWAUX / SUMWT5(4)
 2769                   IF ( PHIAW(ISEA) .EQ. UNDEF )  THEN
 2770                     PHIAW(ISEA) = PHIAWAUX / REAL( SUMGRD )
 2771                   ELSE
 2772                     PHIAW(ISEA) = PHIAW(ISEA) + PHIAWAUX / REAL( SUMGRD )
 2773                   END IF
 2774                 END IF
 2775 !
 2776                 IF ( TAUWIXAUX .NE. UNDEF ) THEN
 2777                   TAUWIXAUX = TAUWIXAUX / SUMWT5(5)
 2778                   TAUWIYAUX = TAUWIYAUX / SUMWT5(5)
 2779                   IF ( TAUWIX(ISEA) .EQ. UNDEF )  THEN
 2780                     TAUWIX(ISEA) = TAUWIXAUX / REAL( SUMGRD )
 2781                     TAUWIY(ISEA) = TAUWIYAUX / REAL( SUMGRD )
 2782                   ELSE
 2783                     TAUWIX(ISEA) = TAUWIX(ISEA) + TAUWIXAUX / REAL( SUMGR>
 2784                     TAUWIY(ISEA) = TAUWIY(ISEA) + TAUWIYAUX / REAL( SUMGR>
 2785                   END IF
 2786                 END IF
 2787 !
 2788                 IF ( TAUWNXAUX .NE. UNDEF ) THEN
 2789                   TAUWNXAUX = TAUWNXAUX / SUMWT5(6)
 2790                   TAUWNYAUX = TAUWNYAUX / SUMWT5(6)
 2791                   IF ( TAUWNX(ISEA) .EQ. UNDEF )  THEN
 2792                     TAUWNX(ISEA) = TAUWNXAUX / REAL( SUMGRD )
 2793                     TAUWNY(ISEA) = TAUWNYAUX / REAL( SUMGRD )
 2794                   ELSE
 2795                     TAUWNX(ISEA) = TAUWNX(ISEA) + TAUWNXAUX / REAL( SUMGR>
 2796                     TAUWNY(ISEA) = TAUWNY(ISEA) + TAUWNYAUX / REAL( SUMGR>
 2797                   END IF
 2798                 END IF
 2799 !
 2800                 DO ICAP = 1,4
 2801                   IF ( WHITECAPAUX(ICAP) .NE. UNDEF ) THEN
 2802                     WHITECAPAUX(ICAP) = WHITECAPAUX(ICAP) / SUMWTC(ICAP)
 2803                     IF ( WHITECAP(ISEA,ICAP) .EQ. UNDEF )  THEN
 2804                       WHITECAP(ISEA,ICAP) = WHITECAPAUX(ICAP) / REAL( SUM>
 2805                     ELSE
 
 
                                                         ww3_gint.F90  page  52
 
 
 2806                       WHITECAP(ISEA,ICAP) = WHITECAP(ISEA,ICAP) +        >
 2807                                WHITECAPAUX(ICAP) / REAL( SUMGRD )
 2808                     END IF
 2809                   END IF
 2810                 END DO
 2811 !
 2812 ! Group 6 variables
 2813 !
 2814                 IF ( SXXAUX .NE. UNDEF ) THEN
 2815                   SXXAUX = SXXAUX / SUMWT6(1)
 2816                   SXYAUX = SXYAUX / SUMWT6(1)
 2817                   SYYAUX = SYYAUX / SUMWT6(1)
 2818                   IF ( SXX(ISEA) .EQ. UNDEF )  THEN
 2819                     SXX(ISEA) = SXXAUX / REAL( SUMGRD )
 2820                     SXY(ISEA) = SXYAUX / REAL( SUMGRD )
 2821                     SYY(ISEA) = SYYAUX / REAL( SUMGRD )
 2822                   ELSE
 2823                     SXX(ISEA) = SXX(ISEA) + SXXAUX / REAL( SUMGRD )
 2824                     SXY(ISEA) = SXY(ISEA) + SXYAUX / REAL( SUMGRD )
 2825                     SYY(ISEA) = SYY(ISEA) + SYYAUX / REAL( SUMGRD )
 2826                   END IF
 2827                 END IF
 2828 !
 2829                 IF ( TAUOXAUX .NE. UNDEF ) THEN
 2830                   TAUOXAUX = TAUOXAUX / SUMWT6(2)
 2831                   TAUOYAUX = TAUOYAUX / SUMWT6(2)
 2832                   IF ( TAUOX(ISEA) .EQ. UNDEF )  THEN
 2833                     TAUOX(ISEA) = TAUOXAUX / REAL( SUMGRD )
 2834                     TAUOY(ISEA) = TAUOYAUX / REAL( SUMGRD )
 2835                   ELSE
 2836                     TAUOX(ISEA) = TAUOX(ISEA) + TAUOXAUX / REAL( SUMGRD )
 2837                     TAUOY(ISEA) = TAUOY(ISEA) + TAUOYAUX / REAL( SUMGRD )
 2838                   END IF
 2839                 END IF
 2840 !
 2841                 IF ( BHDAUX .NE. UNDEF ) THEN
 2842                   BHDAUX = BHDAUX / SUMWT6(3)
 2843                   IF ( BHD(ISEA) .EQ. UNDEF )  THEN
 2844                     BHD(ISEA) = BHDAUX / REAL( SUMGRD )
 2845                   ELSE
 2846                     BHD(ISEA) = BHD(ISEA) + BHDAUX / REAL( SUMGRD )
 2847                   END IF
 2848                 END IF
 2849 !
 2850                 IF ( PHIOCAUX .NE. UNDEF ) THEN
 2851                   PHIOCAUX = PHIOCAUX / SUMWT6(4)
 2852                   IF ( PHIOC(ISEA) .EQ. UNDEF )  THEN
 2853                     PHIOC(ISEA) = PHIOCAUX / REAL( SUMGRD )
 2854                   ELSE
 2855                     PHIOC(ISEA) = PHIOC(ISEA) + PHIOCAUX / REAL( SUMGRD )
 2856                   END IF
 2857                 END IF
 2858 !
 2859                 IF ( TUSXAUX .NE. UNDEF ) THEN
 2860                   TUSXAUX = TUSXAUX / SUMWT6(5)
 
 
                                                         ww3_gint.F90  page  53
 
 
 2861                   TUSYAUX = TUSYAUX / SUMWT6(5)
 2862                   IF ( TUSX(ISEA) .EQ. UNDEF )  THEN
 2863                     TUSX(ISEA) = TUSXAUX / REAL( SUMGRD )
 2864                     TUSY(ISEA) = TUSYAUX / REAL( SUMGRD )
 2865                   ELSE
 2866                     TUSX(ISEA) = TUSX(ISEA) + TUSXAUX / REAL( SUMGRD )
 2867                     TUSY(ISEA) = TUSY(ISEA) + TUSYAUX / REAL( SUMGRD )
 2868                   END IF
 2869                 END IF
 2870 !
 2871                 IF ( USSXAUX .NE. UNDEF ) THEN
 2872                   USSXAUX = USSXAUX / SUMWT6(6)
 2873                   USSYAUX = USSYAUX / SUMWT6(6)
 2874                   IF ( USSX(ISEA) .EQ. UNDEF )  THEN
 2875                     USSX(ISEA) = USSXAUX / REAL( SUMGRD )
 2876                     USSY(ISEA) = USSYAUX / REAL( SUMGRD )
 2877                   ELSE
 2878                     USSX(ISEA) = USSX(ISEA) + USSXAUX / REAL( SUMGRD )
 2879                     USSY(ISEA) = USSY(ISEA) + USSYAUX / REAL( SUMGRD )
 2880                   END IF
 2881                 END IF
 2882 !
 2883                 IF ( PRMSAUX .NE. UNDEF ) THEN
 2884                   PRMSAUX = PRMSAUX / SUMWT6(7)
 2885                   TPMSAUX = TPMSAUX / SUMWT6(7)
 2886                   IF ( PRMS(ISEA) .EQ. UNDEF )  THEN
 2887                     PRMS(ISEA) = PRMSAUX / REAL( SUMGRD )
 2888                     TPMS(ISEA) = TPMSAUX / REAL( SUMGRD )
 2889                   ELSE
 2890                     PRMS(ISEA) = PRMS(ISEA) + PRMSAUX / REAL( SUMGRD )
 2891                     TPMS(ISEA) = TPMS(ISEA) + TPMSAUX / REAL( SUMGRD )
 2892                   END IF
 2893                 END IF
 2894 !
 2895                 IF ( US3DF(1).GT.0 ) THEN
 2896                   DO IK = US3DF(2),US3DF(3)
 2897                     IF ( US3DAUX(IK) .NE. UNDEF ) THEN
 2898                       IF ( US3D(ISEA,IK) .EQ. UNDEF ) US3D(ISEA,IK) = 0.
 2899                       US3D(ISEA,IK) = US3D(ISEA,IK) +                  &
 2900                               US3DAUX(IK) / REAL( SUMWT68(IK) + SUMGRD )
 2901                     END IF
 2902                     IF ( US3DAUX(NK+IK) .NE. UNDEF ) THEN
 2903                       IF ( US3D(ISEA,NK+IK) .EQ. UNDEF )               &
 2904                                                    US3D(ISEA,NK+IK) = 0.
 2905                       US3D(ISEA,NK+IK) = US3D(ISEA,NK+IK) +            &
 2906                         US3DAUX(NK+IK) / REAL( SUMWT68(NK+IK) + SUMGRD )
 2907                     END IF
 2908                   END DO
 2909                 END IF
 2910 !
 2911                 IF ( P2MSF(1).GT.0 ) THEN
 2912                   DO IK = P2MSF(2),P2MSF(3)
 2913                     IF ( P2SMSAUX(IK) .NE. UNDEF ) THEN
 2914                       IF ( P2SMS(ISEA,IK).EQ.UNDEF ) P2SMS(ISEA,IK) = 0.
 2915                       P2SMS(ISEA,IK) = P2SMS(ISEA,IK) +                &
 
 
                                                         ww3_gint.F90  page  54
 
 
 2916                              P2SMSAUX(IK) / REAL( SUMWT69(IK) + SUMGRD )
 2917                     END IF
 2918                   END DO
 2919                 END IF
 2920 !
 2921                 IF ( TAUICEAUX(1) .NE. UNDEF ) THEN
 2922                   IF ( TAUICE(ISEA,1) .EQ. UNDEF )   TAUICE(ISEA,1) = 0.
 2923                   IF ( TAUICE(ISEA,2) .EQ. UNDEF )   TAUICE(ISEA,2) = 0.
 2924                   TAUICE(ISEA,1) = TAUICE(ISEA,1) +                    &
 2925                               TAUICEAUX(1) / REAL( SUMWT6(10) + SUMGRD )
 2926                   TAUICE(ISEA,2) = TAUICE(ISEA,2) +                    &
 2927                               TAUICEAUX(2) / REAL( SUMWT6(10) + SUMGRD )
 2928                 END IF
 2929 !
 2930                 IF ( PHICEAUX .NE. UNDEF ) THEN
 2931                   IF ( PHICE(ISEA) .EQ. UNDEF )   PHICE(ISEA) = 0.
 2932                   PHICE(ISEA) = PHICE(ISEA) +                          &
 2933                                   PHICEAUX / REAL( SUMWT6(11) + SUMGRD )
 2934                 END IF
 2935 !
 2936                 IF ( USSPF(1).GT.0 ) THEN
 2937                   DO IK = 1,USSPF(2)
 2938                     IF ( USSPAUX(IK) .NE. UNDEF ) THEN
 2939                       IF ( USSP(ISEA,IK) .EQ. UNDEF ) USSP(ISEA,IK) = 0.
 2940                       USSP(ISEA,IK) = USSP(ISEA,IK) +                  &
 2941                              USSPAUX(IK) / REAL( SUMWT612(IK) + SUMGRD )
 2942                     END IF
 2943                     IF ( USSPAUX(NK+IK) .NE. UNDEF ) THEN
 2944                       IF ( USSP(ISEA,NK+IK) .EQ. UNDEF )               &
 2945                                                    USSP(ISEA,NK+IK) = 0.
 2946                       USSP(ISEA,NK+IK) = USSP(ISEA,NK+IK) +            &
 2947                        USSPAUX(NK+IK) / REAL( SUMWT612(NK+IK) + SUMGRD )
 2948                     END IF
 2949                   END DO
 2950                 END IF
 2951 !
 2952 ! Group 7 variables
 2953 !
 2954                 IF ( ABAAUX .NE. UNDEF ) THEN
 2955                   ABAAUX = ABAAUX / SUMWT7(1)
 2956                   ABDAUX = ABDAUX / SUMWT7(1)
 2957                   IF ( ABA(ISEA) .EQ. UNDEF )  THEN
 2958                     ABA(ISEA) = ABAAUX / REAL( SUMGRD )
 2959                     ABD(ISEA) = ABDAUX / REAL( SUMGRD )
 2960                   ELSE
 2961                     ABA(ISEA) = ABA(ISEA) + ABAAUX / REAL( SUMGRD )
 2962                     ABD(ISEA) = ABD(ISEA) + ABDAUX / REAL( SUMGRD )
 2963                   END IF
 2964                 END IF
 2965 !
 2966                 IF ( UBAAUX .NE. UNDEF ) THEN
 2967                   UBAAUX = UBAAUX / SUMWT7(2)
 2968                   UBDAUX = UBDAUX / SUMWT7(2)
 2969                   IF ( UBA(ISEA) .EQ. UNDEF )  THEN
 2970                     UBA(ISEA) = UBAAUX / REAL( SUMGRD )
 
 
                                                         ww3_gint.F90  page  55
 
 
 2971                     UBD(ISEA) = UBDAUX / REAL( SUMGRD )
 2972                   ELSE
 2973                     UBA(ISEA) = UBA(ISEA) + UBAAUX / REAL( SUMGRD )
 2974                     UBD(ISEA) = UBD(ISEA) + UBDAUX / REAL( SUMGRD )
 2975                   END IF
 2976                 END IF
 2977 !
 2978                 DO IBED = 1,3
 2979                   IF ( BEDFORMSAUX(IBED) .NE. UNDEF ) THEN
 2980                     BEDFORMSAUX(IBED) = BEDFORMSAUX(IBED) / SUMWTB(IBED)
 2981                     IF ( BEDFORMS(ISEA,IBED) .EQ. UNDEF )  THEN
 2982                       BEDFORMS(ISEA,IBED) = BEDFORMSAUX(IBED) / REAL( SUM>
 2983                     ELSE
 2984                       BEDFORMS(ISEA,IBED) = BEDFORMS(ISEA,IBED) +        >
 2985                                BEDFORMSAUX(IBED) / REAL( SUMGRD )
 2986                     END IF
 2987                   END IF
 2988                 END DO
 2989 !
 2990                 IF ( PHIBBLAUX .NE. UNDEF ) THEN
 2991                   PHIBBLAUX = PHIBBLAUX / SUMWT7(4)
 2992                   IF ( PHIBBL(ISEA) .EQ. UNDEF )  THEN
 2993                     PHIBBL(ISEA) = PHIBBLAUX / REAL( SUMGRD )
 2994                   ELSE
 2995                     PHIBBL(ISEA) = PHIBBL(ISEA) + PHIBBLAUX / REAL( SUMGR>
 2996                   END IF
 2997                 END IF
 2998 !
 2999                 IF ( TAUBBLAUX(1) .NE. UNDEF ) THEN
 3000                   TAUBBLAUX(1) = TAUBBLAUX(1) / SUMWT7(5)
 3001                   TAUBBLAUX(2) = TAUBBLAUX(2) / SUMWT7(5)
 3002                   IF ( TAUBBL(ISEA,1) .EQ. UNDEF )  THEN
 3003                     TAUBBL(ISEA,1) = TAUBBLAUX(1) / REAL( SUMGRD )
 3004                     TAUBBL(ISEA,2) = TAUBBLAUX(2) / REAL( SUMGRD )
 3005                   ELSE
 3006                     TAUBBL(ISEA,1) = TAUBBL(ISEA,1) +                  &
 3007                            TAUBBLAUX(1) / REAL( SUMGRD )
 3008                     TAUBBL(ISEA,2) = TAUBBL(ISEA,2) +                  &
 3009                            TAUBBLAUX(2) / REAL( SUMGRD )
 3010                   END IF
 3011                 END IF
 3012 !
 3013 ! Group 8 variables
 3014 !
 3015                 IF ( MSSXAUX .NE. UNDEF ) THEN
 3016                   IF ( MSSX(ISEA) .EQ. UNDEF )   MSSX(ISEA) = 0.
 3017                   MSSX(ISEA) = MSSX(ISEA) +                            &
 3018                                MSSXAUX / REAL( SUMWT8(1)+SUMGRD )
 3019                 END IF
 3020 !
 3021                 IF ( MSSYAUX .NE. UNDEF ) THEN
 3022                   IF ( MSSY(ISEA) .EQ. UNDEF )   MSSY(ISEA) = 0.
 3023                   MSSY(ISEA) = MSSY(ISEA) +                            &
 3024                                MSSYAUX / REAL( SUMWT8(1)+SUMGRD )
 3025                 END IF
 
 
                                                         ww3_gint.F90  page  56
 
 
 3026 !
 3027                 IF ( MSCXAUX .NE. UNDEF ) THEN
 3028                   IF ( MSCX(ISEA) .EQ. UNDEF )   MSCX(ISEA) = 0.
 3029                   MSCX(ISEA) = MSCX(ISEA) +                            &
 3030                                MSCXAUX / REAL( SUMWT8(2)+SUMGRD )
 3031                 END IF
 3032 !
 3033                 IF ( MSCYAUX .NE. UNDEF ) THEN
 3034                   IF ( MSCY(ISEA) .EQ. UNDEF )   MSCY(ISEA) = 0.
 3035                   MSCY(ISEA) = MSCY(ISEA) +                            &
 3036                                MSCYAUX / REAL( SUMWT8(2)+SUMGRD )
 3037                 END IF
 3038 !
 3039                 IF ( MSSDAUX1 .NE. UNDEF .AND. MSSDAUX2 .NE. UNDEF ) THEN
 3040                   MSSDAUX1 = MSSDAUX1 / REAL( SUMWT8(3)+SUMGRD )
 3041                   MSSDAUX2 = MSSDAUX2 / REAL( SUMWT8(3)+SUMGRD )
 3042                   IF ( MSSD(ISEA) .NE. UNDEF ) THEN
 3043                     MSSDAUX1 = MSSDAUX1 + COS ( MSSD(ISEA) )
 3044                     MSSDAUX2 = MSSDAUX2 + SIN ( MSSD(ISEA) )
 3045                   END IF
 3046                   MSSD(ISEA) = ATAN2 ( MSSDAUX2, MSSDAUX1 )
 3047                 END IF
 3048 !
 3049                 IF ( MSCDAUX1 .NE. UNDEF .AND. MSCDAUX2 .NE. UNDEF ) THEN
 3050                   MSCDAUX1 = MSCDAUX1 / REAL( SUMWT8(4)+SUMGRD )
 3051                   MSCDAUX2 = MSCDAUX2 / REAL( SUMWT8(4)+SUMGRD )
 3052                   IF ( MSCD(ISEA) .NE. UNDEF ) THEN
 3053                     MSCDAUX1 = MSCDAUX1 + COS ( MSCD(ISEA) )
 3054                     MSCDAUX2 = MSCDAUX2 + SIN ( MSCD(ISEA) )
 3055                   END IF
 3056                   MSCD(ISEA) = ATAN2 ( MSCDAUX2, MSCDAUX1 )
 3057                 END IF
 3058 !
 3059                 IF ( QPAUX .NE. UNDEF ) THEN
 3060                   IF ( QP(ISEA) .EQ. UNDEF )   QP(ISEA) = 0.
 3061                   QP(ISEA) = QP(ISEA) + QPAUX / REAL( SUMWT8(5)+SUMGRD )
 3062                 END IF
 3063 !
 3064               END IF !/ ( USEGRID(IG) )
 3065 !
 3066 ! End of Second loop
 3067             END DO !/ IG = 1, GR_INTS
 3068 !
 3069 ! Convert select variables back to polar notation. This is done because j>
 3070 ! prior to writing to file the w3iogo routine converts these variables
 3071 ! from polar to cartesian coordinates
 3072 !
 3073             IF ( UA(ISEA) .NE. UNDEF ) THEN
 3074               VAR1 = UA(ISEA)
 3075               VAR2 = UD(ISEA)
 3076               UA(ISEA) = SQRT ( VAR1**2 + VAR2**2 )
 3077               UD(ISEA) = ATAN2 ( VAR2, VAR1 )
 3078             END IF
 3079 !
 3080             IF ( UST(ISEA) .NE. UNDEF ) THEN
 
 
                                                         ww3_gint.F90  page  57
 
 
 3081               VAR1 = UST(ISEA)
 3082               VAR2 = USTDIR(ISEA)
 3083               UST(ISEA) = SQRT ( VAR1**2 + VAR2**2 )
 3084               USTDIR(ISEA) = ATAN2 ( VAR2, VAR1 )
 3085             END IF
 3086 !
 3087             IF ( ABA(ISEA) .NE. UNDEF ) THEN
 3088               VAR1 = ABA(ISEA)
 3089               VAR2 = ABD(ISEA)
 3090               ABA(ISEA) = SQRT ( VAR1**2 + VAR2**2 )
 3091               ABD(ISEA) = ATAN2 ( VAR2, VAR1 )
 3092             END IF
 3093 !
 3094             IF ( UBA(ISEA) .NE. UNDEF ) THEN
 3095               VAR1 = UBA(ISEA)
 3096               VAR2 = UBD(ISEA)
 3097               UBA(ISEA) = SQRT ( VAR1**2 + VAR2**2 )
 3098               UBD(ISEA) = ATAN2 ( VAR2, VAR1 )
 3099             END IF
 3100 !
 3101           END IF
 3102 !
 3103 !/ End of main loop through output points
 3104         END DO   !/ ISEA = 1, NSEA
 3105 !
 3106 !------------------------------------------------------------------------>
 3107 ! 3. Write out interpolated data to target output file
 3108 !
 3109         CALL W3IOGO('WRITE',FIDOUT(NG),IOTST,NG)
 3110 !
 3111         RETURN
 3112 !
 3113 ! Error escape locations
 3114 !
 3115 !/
 3116 !/ End of W3EXGI -------------------------------------------------------->
 3117 !/
 3118         END SUBROUTINE W3EXGI
 3119 !/
 3120 !/ End of W3GRID_INTERP ------------------------------------------------->
 3121 !/
 3122         END PROGRAM W3GRID_INTERP
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
