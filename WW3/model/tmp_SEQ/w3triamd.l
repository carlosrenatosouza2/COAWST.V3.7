                                                         w3triamd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3TRIAMD
    4 !/ -------------------------------------------------------------------
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |       F. Ardhuin and A. Roland    |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :          26-Jan-2014|
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    15-Mar-2007 : Origination.                        ( version 3.13 )
   13 !/    25-Aug-2011 : Modification of boundary treatment  ( version 4.04 )
   14 !/    30-Aug-2012 : Automatic detection of open BC      ( version 4.08 )
   15 !/    02-Sep-2012 : Clean up of open BC for UG grids    ( version 4.08 )
   16 !/    14-Oct-2013 : Correction  of latitude factor      ( version 4.12 )
   17 !/    26-Jan-2014 : Correction  interpolation weights   ( version 4.18 )
   18 !/    21-Apr-2016 : New algorithm to detect boundary    ( version 5.12 )
   19 !/
   20 !
   21 !  1. Purpose :
   22 !
   23 !      Reads triangle and unstructured grid information
   24 !
   25 !  2. Method :
   26 !
   27 !     Look for namelist with name NAME in unit NDS and read if found.
   28 !
   29 !  3. Parameters :
   30 !
   31 !  4. Subroutines used :
   32 !
   33 !      Name               Type  Module   Description
   34 !     ------------------------------------------------------------------->
   35 !      READTRI            Subr. Internal Read unstructured grid data from>
   36 !      READMSH            Subr.   Id.    Read unstructured grid data from>
   37 !      COUNT              Subr. Internal Count connection.
   38 !      SPATIAL_GRID       Subr.   Id.    Calculate surfaces.
   39 !      NVECTRI            Subr.   Id.    Define cell normals and angles a>
   40 !      COORDMAX           Subr.   Id.    Calculate  useful grid elements
   41 !      AREA_SI            Subr.   Id.    Define Connections
   42 !     ------------------------------------------------------------------->
   43 !
   44 !  5. Called by :
   45 !
   46 !     Program in which it is contained.
   47 !
   48 !  6. Error messages :
   49 !
   50 !  7. Remarks :
   51 !     The only point index which is needed is IX and NX stands for the to>
   52 !     IY and NY are not needed anymore, they are set to 1 in the unstruct>
   53 !     Some noticeable arrays are:
   54 !                     XYB    : give the 2D coordinates of all grid points
   55 !                     TRIGP  : give the vertices of each triangle
 
 
                                                         w3triamd.F90  page   2
 
 
   56 !  8. Structure :
   57 !
   58 !  9. Switches :
   59 !       !/PR3   : Enables unstructured meshes (temporary, will be replace>
   60 ! 10. Source code :
   61 !
   62 !/ ------------------------------------------------------------------- /
   63  PUBLIC
   64 !      USE CONSTANTS
   65 !      USE W3GDATMD, ONLY: W3NMOD, W3SETG
   66 !      USE W3ODATMD, ONLY: W3NOUT, W3SETO, W3DMO5
   67 !      USE W3IOGRMD, ONLY: W3IOGR
   68 !     USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE
   69 !!/S      USE W3SERVMD, ONLY: STRACE
   70 !      USE W3ARRYMD, ONLY: INA2R, INA2I
   71 !!/T      USE W3ARRYMD, ONLY: PRTBLK
   72 !      USE W3DISPMD, ONLY: DISTAB
   73 !      USE W3GDATMD
   74 !      USE W3ODATMD, ONLY: NDSE, NDST, NDSO
   75 !      USE W3ODATMD, ONLY: NBI, NBI2, NFBPO, NBO, NBO2, FLBPI, FLBPO,  &
   76 !                         IPBPO, ISBPO, XBPO, YBPO, RDBPO, FNMPRE
   77 !---------------------------------------------------------------------
   78 !
   79 !C
   80         integer :: node_num
   81         integer :: dim_num
   82         integer :: triangle_order
   83         integer :: triangle_num
   84         integer :: bound_edge_num
   85         integer :: bound_num
   86 !C
   87         logical,save, allocatable :: edge_boundary(:)
   88         logical,save, allocatable :: node_boundary(:)
   89         integer,save, allocatable :: edge_nums(:)
   90         integer,save, allocatable :: boundary_node_index(:)
   91 !C
   92         integer,save, allocatable :: triangle_node(:,:)
   93         integer,save, allocatable :: edge(:,:)
   94         integer,save, allocatable :: edge_index(:,:)
   95         integer,save, allocatable :: iobp_aux(:)
   96                                                 
   97         INTEGER, SAVE                      :: N_OUTSIDE_BOUNDARY
   98         INTEGER, SAVE, ALLOCATABLE         :: OUTSIDE_BOUNDARY(:)
   99         real (kind = 8), save, allocatable :: node_xy(:,:)
  100         real (kind = 8), save, allocatable :: edge_angle(:,:)
  101                                                              
  102 CONTAINS
  103 !/ -------------------------------------------------------------------/
  104       SUBROUTINE READMSH(NDS,FNAME)
  105 !/ -------------------------------------------------------------------
  106 !/                  +-----------------------------------+
  107 !/                  | WAVEWATCH III           NOAA/NCEP |
  108 !/                  |           F. Ardhuin              |
  109 !/                  |                        FORTRAN 90 |
  110 !/                  | Last update :          06-Jun-2018|
 
 
                                                         w3triamd.F90  page   3
 
 
  111 !/                  +-----------------------------------+
  112 !/
  113 !/    15-Feb-2008 : Origination.                        ( version 3.13 )
  114 !/    25-Aug-2011 : Change of method for IOBPD          ( version 4.04 )
  115 !/    06-Jun-2018 : Add DEBUGINIT/PDLIB/DEBUGSTP/DEBUGSETIOBP
  116 !/                                                      ( version 6.04 )
  117 !/
  118 !
  119 !  1. Purpose :
  120 !
  121 !      Reads triangle and unstructured grid information from GMSH files
  122 !      Calls the subroutines needed to compute grid connectivity
  123 !
  124 !  2. Method :
  125 !
  126 !     Look for namelist with name NAME in unit NDS and read if found.
  127 !
  128 !  3. Parameters :
  129 !
  130 !     Parameter list
  131 !     ----------------------------------------------------------------
  132 !       NDS     Int.   I   Data set number used for search.
  133 !       NAME    C*4    I   Name of namelist.
  134 !       STATUS  C*20   O   Status at end of routine,
  135 !                            '(default values)  ' if no namelist found.
  136 !                            '(user def. values)' if namelist read.
  137 !     ----------------------------------------------------------------
  138 !
  139 !  4. Subroutines used :
  140 !
  141 !      Name               Type  Module   Description
  142 !     ------------------------------------------------------------------->
  143 !      NEXTLN             Subr.
  144 !      COUNT              Subr. Internal Count connection.
  145 !      SPATIAL_GRID       Subr.   Id.    Calculate surfaces.
  146 !      NVECTRI            Subr.   Id.    Define cell normals and angles a>
  147 !      COORDMAX           Subr.   Id.    Calculate  useful grid elements
  148 !      AREA_SI            Subr.   Id.    Define Connections
  149 !     ----------------------------------------------------------------
  150 !
  151 !  5. Called by :
  152 !      Name      Type  Module   Description
  153 !     ----------------------------------------------------------------
  154 !      W3GRID    Prog.          Model configuration program
  155 !     ----------------------------------------------------------------
  156 !
  157 !  6. Error messages :
  158 !
  159 !  7. Remarks :
  160 !     The only point index which is needed is IX and NX stands for the to>
  161 !     IY and NY are not needed anymore, they are set to 1 in the unstruct>
  162 !     Some noticeable arrays are:
  163 !                     XYB    : give the 2D coordinates of all grid points
  164 !                     TRIGP  : give the vertices of each triangle
  165 !     GMSH file gives too much information that is not necessarily requir>
 
 
                                                         w3triamd.F90  page   4
 
 
  166 !  8. Structure :
  167 !
  168 !  9. Switches :
  169 !
  170 ! 10. Source code :
  171 !
  172 !/ ------------------------------------------------------------------- /
  173       USE W3ODATMD, ONLY: NDSE, NDST, NDSO
  174       USE W3GDATMD
  175       USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE
  176       USE CONSTANTS, only: LPDLIB
  177       USE W3ODATMD, ONLY: IAPROC
  178 !
  179       IMPLICIT NONE
  180 !/
  181 !/ Parameter list
  182 !/
  183       INTEGER, INTENT(IN)                :: NDS
  184       CHARACTER(60), INTENT(IN)          :: FNAME
  185 !/
  186 !/ local parameters
  187 !/
  188       INTEGER                            :: i,j,k, NODES, NELTS, ID, KID
  189       INTEGER                            :: ID1, ID2, KID1, ITMP(3)
  190       INTEGER                            :: I1, I2, I3
  191       INTEGER(KIND=4)                    :: Ind,eltype,ntag, INode
  192       CHARACTER                          :: COMSTR*1, SPACE*1 = ' ', CELS>
  193       REAL, ALLOCATABLE                  :: TAGS(:)
  194       CHARACTER(LEN=64), ALLOCATABLE     :: ELS(:)
  195       CHARACTER(LEN=120)                 :: LINE
  196       CHARACTER(LEN=50)                  :: CHTMP
  197       CHARACTER(LEN=10)                  :: A, B, C
  198       INTEGER,ALLOCATABLE                :: NELS(:), TRIGPTMP1(:,:), TRIG>
  199       INTEGER(KIND=4),ALLOCATABLE        :: IFOUND(:), VERTEX(:), BOUNDTM>
  200       DOUBLE PRECISION, ALLOCATABLE      :: XYBTMP1(:,:),XYBTMP2(:,:)
  201       REAL                               :: z
  202                                              
  203       OPEN(NDS,FILE = FNAME,STATUS='old')
  204       READ (NDS,'(A)') COMSTR
  205       IF (COMSTR.EQ.' ') COMSTR = '$'
  206       CALL NEXTLN(COMSTR, NDS, NDSE)
  207       READ(NDS,*) i,j,k
  208       CALL NEXTLN(COMSTR, NDS, NDSE)
  209       LPDLIB = .FALSE.
  210 !
  211 ! read number of nodes and nodes from Gmsh files
  212 !
  213       READ(NDS,*) NODES
  214       ALLOCATE(XYBTMP1(NODES,3))
  215       DO I= 1, NODES
  216         READ(NDS,*) j, XYBTMP1(I,1), XYBTMP1(I,2), XYBTMP1(I,3)
  217         END DO
  218 !
  219 ! read number of elements and elements from Gmsh files
  220 !
 
 
                                                         w3triamd.F90  page   5
 
 
  221       ALLOCATE(BOUNDTMP(NODES))
  222       N_OUTSIDE_BOUNDARY = 0
  223       CALL NEXTLN(COMSTR, NDS, NDSE)
  224       READ(NDS,*) NELTS
  225       ALLOCATE(TRIGPTMP1(NELTS, 3))
  226       J = 0
  227       DO I= 1, NELTS
  228         READ(NDS,'(A100)') LINE
  229         READ(LINE,*) Ind,eltype,ntag
  230         ALLOCATE(TAGS(ntag))
  231         SELECT CASE (eltype)
  232 !
  233 ! eltype = 15 : boundary points  (this is used to make the difference
  234 !                                between the outside polygon and islands)
  235 !
  236         CASE(15)
  237           READ(LINE,*) Ind,eltype,ntag,TAGS,INODE
  238           N_OUTSIDE_BOUNDARY = N_OUTSIDE_BOUNDARY +1
  239           BOUNDTMP(N_OUTSIDE_BOUNDARY)=INODE
  240 !
  241 ! eltype = 2 : triangles
  242 !
  243         CASE (2)
  244           J = J + 1
  245           READ(LINE,*)  Ind,eltype,ntag,tags,ITMP
  246           TRIGPTMP1(J,1:3) = ITMP
  247           END SELECT
  248                     
  249         DEALLOCATE(TAGS)
  250         END DO
  251 !
  252 ! organizes the grid data structure
  253 !
  254       ALLOCATE(OUTSIDE_BOUNDARY(N_OUTSIDE_BOUNDARY))
  255       OUTSIDE_BOUNDARY(:)=BOUNDTMP(1:N_OUTSIDE_BOUNDARY)
  256       NTRI = J
  257               
  258       ALLOCATE(IFOUND(NODES))
  259                              
  260       IFOUND = 0
  261 !
  262 ! Verifies that the nodes are used in at least one triangle
  263 !
  264       DO K = 1, NTRI
  265         I1 = TRIGPTMP1(K,1)
  266         I2 = TRIGPTMP1(K,2)
  267         I3 = TRIGPTMP1(K,3)
  268                            
  269         IFOUND(I1)= IFOUND(I1) + 1
  270         IFOUND(I2)= IFOUND(I2) + 1
  271         IFOUND(I3)= IFOUND(I3) + 1
  272         END DO
  273               
  274       J = 0
  275            
 
 
                                                         w3triamd.F90  page   6
 
 
  276       ALLOCATE(TRIGPTMP2(NTRI,3),VERTEX(NODES),XYBTMP2(NODES,3))
  277       VERTEX(:)=0
  278       XYBTMP2 = 0
  279                  
  280       DO I = 1, NODES
  281         IF( IFOUND(I) .GT. 0) THEN
  282           J = J+1
  283           XYBTMP2(J,:) = XYBTMP1(I,:)
  284           VERTEX(I) = J
  285           END IF
  286         END DO
  287 !
  288 ! Number of nodes after clean up
  289 !
  290       NX = J
  291 !
  292       DO I = 1, NTRI
  293         I1 = TRIGPTMP1(I,1)
  294         I2 = TRIGPTMP1(I,2)
  295         I3 = TRIGPTMP1(I,3)
  296         TRIGPTMP2(I,1) = VERTEX(I1)
  297         TRIGPTMP2(I,2) = VERTEX(I2)
  298         TRIGPTMP2(I,3) = VERTEX(I3)
  299         END DO
  300 !
  301       DEALLOCATE(XYBTMP1, IFOUND,TRIGPTMP1)
  302       DEALLOCATE(VERTEX)
  303 !
  304 !count points connections to allocate array in W3DIMUG
  305 !
  306       CALL COUNT(TRIGPTMP2)
  307       CALL W3DIMUG ( 1, NTRI, NX, COUNTOT, NNZ, NDSE, NDST )
  308 !
  309 ! fills arrays
  310 !
  311       DO I = 1, NX
  312         XYB(I,1) = XYBTMP2(I,1)
  313         XYB(I,2) = XYBTMP2(I,2)
  314         XYB(I,3) = XYBTMP2(I,3)
  315         END DO
  316 !
  317       DO I=1, NTRI
  318         ITMP = TRIGPTMP2(I,:)
  319         TRIGP(I,:) = ITMP
  320         END DO
  321 !
  322       DEALLOCATE(TRIGPTMP2,XYBTMP2)
  323 !
  324 ! call the various routines which define the point spotting strategy
  325 !
  326       CALL SPATIAL_GRID
  327       CALL NVECTRI
  328       CALL COORDMAX
  329 !
  330 !READMSH is only called by ww3_grid, thus the grid index is always 1.
 
 
                                                         w3triamd.F90  page   7
 
 
  331 !
  332       CALL AREA_SI(1)
  333 !
  334       CLOSE(NDS)
  335       END SUBROUTINE READMSH
  336       SUBROUTINE GET_BOUNDARY_STATUS(STATUS)
  337 !/
  338 !/                  +-----------------------------------+
  339 !/                  | WAVEWATCH III           NOAA/NCEP |
  340 !/                  |                                   |
  341 !/                  | Mathieu Dutour-Sikiric (IRB)      |
  342 !/                  | Aron Roland (BGS IT&E GmbH)       |
  343 !/                  |                                   |
  344 !/                  |                        FORTRAN 90 |
  345 !/                  | Last update :         01-Mai-2018 |
  346 !/                  +-----------------------------------+
  347 !/
  348 !/    01-Mai-2018 : Origination.                        ( version 6.04 )
  349 !/
  350 !  1. Purpose : boundary status (code duplication)
  351 !  2. Method :
  352 !  3. Parameters :
  353 !
  354 !     Parameter list
  355 !     ----------------------------------------------------------------
  356 !     ----------------------------------------------------------------
  357 !
  358 !  4. Subroutines used :
  359 !
  360 !      Name      Type  Module   Description
  361 !     ----------------------------------------------------------------
  362 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  363 !     ----------------------------------------------------------------
  364 !
  365 !  5. Called by :
  366 !
  367 !      Name      Type  Module   Description
  368 !     ----------------------------------------------------------------
  369 !     ----------------------------------------------------------------
  370 !
  371 !  6. Error messages :
  372 !  7. Remarks
  373 !  8. Structure :
  374 !  9. Switches :
  375 !
  376 !     !/S  Enable subroutine tracing.
  377 !
  378 ! 10. Source code :
  379 !
  380 !/ ------------------------------------------------------------------- /
  381 !
  382  
  383       USE W3GDATMD, ONLY : TRIGP, NTRI, NX
  384       IMPLICIT NONE
  385 !/
 
 
                                                         w3triamd.F90  page   8
 
 
  386 !/ ------------------------------------------------------------------- /
  387 !/ Parameter list
  388 !/
  389 !/ ------------------------------------------------------------------- /
  390 !/ Local PARAMETERs
  391 !/
  392 !/
  393 !/ ------------------------------------------------------------------- /
  394 !/
  395 !
  396       integer, intent(out) :: STATUS(NX)
  397       INTEGER :: COLLECTED(NX), NEXTVERT(NX), PREVVERT(NX)
  398       INTEGER :: ISFINISHED, INEXT, IPREV
  399       INTEGER :: IPNEXT, IPPREV, ZNEXT, IP, I, IE
  400       STATUS(:) = 0
  401       DO IE=1,NTRI
  402         DO I=1,3
  403           IF (I.EQ.1) THEN
  404             IPREV=3
  405           ELSE
  406             IPREV=I-1
  407           END IF
  408           IF (I.EQ.3) THEN
  409             INEXT=1
  410           ELSE
  411             INEXT=I+1
  412           END IF
  413           IP=TRIGP(IE,I)
  414           IPNEXT=TRIGP(IE,INEXT)
  415           IPPREV=TRIGP(IE,IPREV)
  416           IF (STATUS(IP).EQ.0) THEN
  417             STATUS(IP)=1
  418             PREVVERT(IP)=IPPREV
  419             NEXTVERT(IP)=IPNEXT
  420           END IF
  421         END DO
  422       END DO
  423       STATUS(:)=0
  424       DO
  425         COLLECTED(:)=0
  426         DO IE=1,NTRI
  427           DO I=1,3
  428             IF (I.EQ.1) THEN
  429               IPREV=3
  430             ELSE
  431               IPREV=I-1
  432             END IF
  433             IF (I.EQ.3) THEN
  434               INEXT=1
  435             ELSE
  436               INEXT=I+1
  437             END IF
  438             IP=TRIGP(IE,I)
  439             IPNEXT=TRIGP(IE,INEXT)
  440             IPPREV=TRIGP(IE,IPREV)
 
 
                                                         w3triamd.F90  page   9
 
 
  441             IF (STATUS(IP).eq.0) THEN
  442               ZNEXT=NEXTVERT(IP)
  443               IF (ZNEXT.eq.IPPREV) THEN
  444                 COLLECTED(IP)=1
  445                 NEXTVERT(IP)=IPNEXT
  446                 IF (NEXTVERT(IP).eq.PREVVERT(IP)) THEN
  447                   STATUS(IP)=1
  448                 END IF
  449               END IF
  450             END IF
  451           END DO
  452         END DO
  453         ISFINISHED=1
  454         DO IP=1,NX
  455           IF ((COLLECTED(IP).eq.0).and.(STATUS(IP).eq.0)) THEN
  456             STATUS(IP)=-1
  457           END IF
  458           IF (STATUS(IP).eq.0) THEN
  459             ISFINISHED=0
  460           END IF
  461         END DO
  462         IF (ISFINISHED.eq.1) THEN
  463           EXIT
  464         END IF
  465       END DO
  466       END SUBROUTINE
  467                     
  468 !/ -------------------------------------------------------------------/
  469       SUBROUTINE READMSHOBC(NDS, FNAME, TMPSTA, UGOBCOK)
  470 !/ -------------------------------------------------------------------
  471 !/                  +-----------------------------------+
  472 !/                  | WAVEWATCH III           NOAA/NCEP |
  473 !/                  |           F. Ardhuin              |
  474 !/                  |                        FORTRAN 90 |
  475 !/                  | Last update :          14-Mar-2018|
  476 !/                  +-----------------------------------+
  477 !/
  478 !/    14-Mar-2018 : Origination.                        ( version 6.02 )
  479 !/
  480 !
  481 !  1. Purpose :
  482 !
  483 !      Reads open boundary information for UNST grids following GMESH typ>
  484 !
  485 !  2. Method :
  486 !
  487 !     Reads an ASCII file
  488 !
  489 !  3. Parameters :
  490 !
  491 !     Parameter list
  492 !     ----------------------------------------------------------------
  493 !       NDS     Int.   I   Data set number used for search.
  494 !       FNAME   Char*60 I  File name
  495 !       TMPSTA  Char*60 I/O status map to be updated (for OBC, TMPSTA = 2)
 
 
                                                         w3triamd.F90  page  10
 
 
  496 !       UGOBCOK Logical O   flag for proper reading of OBC file
  497 !     ----------------------------------------------------------------
  498 !
  499 !  4. Subroutines used : NONE
  500 !
  501 !  5. Called by :
  502 !      Name      Type  Module   Description
  503 !     ----------------------------------------------------------------
  504 !      W3GRID    Prog.          Model configuration program
  505 !     ----------------------------------------------------------------
  506 !
  507 !  6. Error messages :
  508 !
  509 !  7. Remarks :
  510 !
  511 !  8. Structure :
  512 !
  513 !  9. Switches :
  514 !
  515 ! 10. Source code :
  516 !
  517 !/ ------------------------------------------------------------------- /
  518       USE W3GDATMD, ONLY: NX, NY, CCON , COUNTCON
  519       USE W3ODATMD, ONLY: NDSE, NDST, NDSO
  520       USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE
  521                                                 
  522       IMPLICIT NONE
  523 !/ ------------------------------------------------------------------- /
  524 !/ Parameter list
  525 !/
  526       INTEGER, INTENT(IN)                :: NDS
  527       CHARACTER(60), INTENT(IN) :: FNAME
  528       INTEGER, INTENT(INOUT)    :: TMPSTA(NY,NX)
  529       LOGICAL, INTENT(OUT)      :: UGOBCOK
  530 !/
  531 !/ local parameters
  532 !/
  533       INTEGER                            :: I, IERR
  534       INTEGER(KIND=4)                    :: Ind,ntag, INode
  535       CHARACTER                          :: COMSTR*1, SPACE*1 = ' ', CELS>
  536       REAL, ALLOCATABLE                  :: TAGS(:)
  537       CHARACTER(LEN=120)                 :: LINE
  538                                                 
  539       UGOBCOK=.FALSE.
  540                      
  541       OPEN(NDS,FILE = FNAME,STATUS='old')
  542       READ (NDS,'(A)') COMSTR
  543      IF (COMSTR.EQ.' ') COMSTR = '$'
  544       CALL NEXTLN(COMSTR, NDS, NDSE)
  545       IERR = 0
  546       DO WHILE (IERR.EQ.0)
  547         READ (NDS,'(A100)',END=2001,ERR=2002,IOSTAT=IERR) LINE
  548         READ(LINE,*,IOSTAT=IERR) Ind,ntag
  549         IF (IERR.EQ.0) THEN
  550           ALLOCATE(TAGS(ntag))
 
 
                                                         w3triamd.F90  page  11
 
 
  551           READ(LINE,*,IOSTAT=IERR) Ind,ntag,TAGS,INODE
  552           IF (IERR.EQ.0) THEN
  553             TMPSTA(1,INODE)=2
  554             DEALLOCATE(TAGS)
  555           ELSE
  556             GOTO 2001
  557             END IF
  558           END IF
  559         END DO
  560         CLOSE(NDS)
  561       UGOBCOK=.TRUE.
  562       RETURN
  563 !
  564  2001 CONTINUE
  565       WRITE (NDSE,1001)
  566       CALL EXTCDE ( 61 )
  567 !
  568  2002 CONTINUE
  569       WRITE (NDSE,1002) IERR
  570       CALL EXTCDE ( 62 )
  571  1001 FORMAT (/' *** WAVEWATCH III ERROR IN READMSHOBC : '/          &
  572                '     PREMATURE END OF FILE IN READING ',A/)
  573  1002 FORMAT (/' *** WAVEWATCH III ERROR IN READMSHOBC : '/          &
  574                '     ERROR IN READING ',A,'  IOSTAT =',I8/)
  575                                                            
  576       END SUBROUTINE READMSHOBC
  577 !/ ------------------------------------------------------------------- /
  578                                                                         
  579                                                                         
  580 !/ ------------------------------------------------------------------- /
  581       SUBROUTINE UG_GETOPENBOUNDARY(TMPSTA,ZBIN,ZLIM)
  582 !/                  +-----------------------------------+
  583 !/                  | WAVEWATCH III           NOAA/NCEP |
  584 !/                  |           F. Ardhuin              |
  585 !/                  |                        FORTRAN 90 |
  586 !/                  | Last update :          30-Aug-2012|
  587 !/                  +-----------------------------------+
  588 !/
  589 !/    30-Aug-2012 : Adpatation from SHOM-Ifremer program( version 4.07 )
  590 !/
  591 !
  592 !  1. purpose: defines open boundary points based on depth
  593 !  2. Method : a boundary node has more node around it than triangles
  594 !
  595 !  3. Parameters :
  596 !     TMPSTA: status map to be updated (for OBC, TMPSTA = 2)
  597 !
  598 !  4. Subroutines used :
  599 !
  600 !  5. Called by :
  601 !
  602 !       Name      Type  Module   Description
  603 !     ----------------------------------------------------------------
  604 !      w3GRID    Prog.          Model configuration program
  605 !     ----------------------------------------------------------------
 
 
                                                         w3triamd.F90  page  12
 
 
  606 !
  607 !  6. Error messages :
  608 !
  609 !  7. Remarks :
  610 !
  611  
  612 !
  613 !  8. Structure :
  614 !
  615 !  9. Switches :
  616 !
  617 ! 10. Source code :
  618       USE W3GDATMD, ONLY: NX, NY, CCON, COUNTCON, IOBP
  619                                                       
  620                                                       
  621       IMPLICIT NONE
  622 !/ ------------------------------------------------------------------- /
  623 !/ Parameter list
  624 !/
  625       INTEGER, INTENT(INOUT)  :: TMPSTA(NY,NX)
  626       REAL   , INTENT(IN)     :: ZBIN(NY,NX)
  627       REAL   , INTENT(IN)     :: ZLIM
  628 !/
  629 !/ ------------------------------------------------------------------- /
  630 !/ Local parameters
  631 !/
  632       INTEGER                 ::  IBC, IX
  633       INTEGER                 ::  MASK(NX), STATUS(NX)
  634 !
  635       MASK(:)=1
  636       CALL SET_IOBP (MASK, STATUS)
  637 !
  638         DO IBC = 1, N_OUTSIDE_BOUNDARY
  639            IX = OUTSIDE_BOUNDARY(IBC)
  640            !write(*,*) 'TEST1', IX, TMPSTA(1,IX), CCON(IX), COUNTCON(IX),>
  641                                                                           
  642            ! OUTSIDE_BOUNDARY(IBC) is defined over the full nodes NODES i>
  643            ! whereas TMPSTA and ZBIN are defined over the clean up list o>
  644            IF ((IX.NE.0).AND.(IX.LE.NX)) THEN
  645              IF ((TMPSTA(1,IX).EQ.1).AND.(STATUS(IX).EQ.0)  &
  646                .AND.(ZBIN(1,IX).LT.ZLIM))  TMPSTA(1,IX)=2
  647             END IF
  648           END DO
  649 !
  650     END SUBROUTINE UG_GETOPENBOUNDARY
  651 !/ ------------------------------------------------------------------- /
  652                                                                         
  653                                                                         
  654       SUBROUTINE SPATIAL_GRID
  655 !/ -------------------------------------------------------------------
  656 !/                  +-----------------------------------+
  657 !/                  | WAVEWATCH III           NOAA/NCEP |
  658 !/                  |      A. Roland  (BGS IT&E GbmH)   |
  659 !/                  |      F. Ardhuin (IFREMER)         |
  660 !/                  |                        FORTRAN 90 |
 
 
                                                         w3triamd.F90  page  13
 
 
  661 !/                  | Last update :          31-Aug-2011|
  662 !/                  +-----------------------------------+
  663 !/
  664 !/    15-May-2007 : Origination: adjustment from the WWM code       ( ver>
  665 !/    31-Aug-2011 : Simplfies the cross products                    ( ver>
  666 !/
  667 !
  668 !  1. Purpose :
  669 !
  670 !      Calculates triangle areas and reorders the triangles to have them
  671 !      oriented counterclockwise
  672 !
  673 !  2. Method :
  674 !
  675 !     The triangle surface calculation is based on cross product.
  676 !
  677 !  3. Parameters :
  678 !
  679 !  4. Subroutines used :
  680 !
  681 !  5. Called by :
  682 !
  683 !       Name      Type  Module   Description
  684 !     ----------------------------------------------------------------
  685 !      READTRI    Subr. Internal  Unstructured mesh definition.
  686 !     ----------------------------------------------------------------
  687 !
  688 !  6. Error messages :
  689 !
  690 !  7. Remarks :
  691 !
  692 !     This part of code is adapted from the WWM wave model develop at the>
  693 !     (Aaron Roland)
  694 !
  695 !  8. Structure :
  696 !
  697 !  9. Switches :
  698 !
  699 ! 10. Source code :
  700 !
  701 !/ ------------------------------------------------------------------- /
  702          USE W3GDATMD
  703          USE W3ODATMD, ONLY: NDSE
  704                                  
  705          IMPLICIT NONE
  706 !	
  707 !local parameters	
  708 !
  709          REAL              :: TL1, TL2, TL3, TMPTRIGP
  710          INTEGER           :: I1, I2, I3
  711          INTEGER           :: K
  712          REAL*8            :: PT(3,2)
  713                                      
  714 !/ ------------------------------------------------------------------- /
  715                                                                         
 
 
                                                         w3triamd.F90  page  14
 
 
  716                 DO K = 1, NTRI
  717                               
  718                    I1 = TRIGP(K,1)
  719                    I2 = TRIGP(K,2)
  720                    I3 = TRIGP(K,3)
  721                                   
  722                    CALL FIX_PERIODCITY(I1,I2,I3,XYB,PT)
  723 !
  724 ! cross product of edge-vector  (orientated anticlockwise)
  725 !
  726  
  727                    TRIA(K) = REAL( (PT(2,2)-PT(1,2))      &     !  (Y2-Y1)
  728                                   *(PT(1,1)-PT(3,1))      &     ! *(X1-X3)
  729                                   +(PT(3,2)-PT(1,2))      &     !  (Y3-Y1>
  730                                   *(PT(2,1)-PT(1,1))      )*0.5
  731 !
  732 ! test on negative triangle area, which means that the orientiation is no>
  733 ! therefore we swap the nodes !!!
  734 !
  735                   IF (TRIA(K) .lt. TINY(1.)) THEN
  736          TMPTRIGP = TRIGP(K,2)
  737          TRIGP(K,2) = TRIGP(K,3)
  738          TRIGP(K,3) = TMPTRIGP
  739          I2 = TRIGP(K,2)
  740          I3 = TRIGP(K,3)
  741          TRIA(K) = -1.d0*TRIA(K)
  742          WRITE(NDSE,*) 'WRONG TRIANGLE',TRIA(K),K,I1,I2,I3, XYB(I2,2)-XYB>
  743                           XYB(I1,1)-XYB(I3,1),XYB(I3,2)-XYB(I1,2), XYB(I2>
  744          STOP
  745          END IF
  746        END DO
  747      END SUBROUTINE
  748 !
  749     SUBROUTINE NVECTRI
  750 !/ -------------------------------------------------------------------
  751 !/                  +-----------------------------------+
  752 !/                  | WAVEWATCH III           NOAA/NCEP |
  753 !/                  |           A. Roland               |
  754 !/                  |                        FORTRAN 90 |
  755 !/                  | Last update :          15-May-2008|
  756 !/                  +-----------------------------------+
  757 !/
  758 !/    15-May-2007 : Origination: adjustment from the WWM code       ( ver>
  759 !/
  760 !
  761 !  1. Purpose :
  762 !
  763 !      Calculate cell tools: inward normal, angles and length of edges.
  764 !
  765 !  2. Method :
  766 !      To get inward pointing normals, triangle are glanced through anti->
  767 !
  768 !  3. Parameters :
  769 !
  770 !  4. Subroutines used :
 
 
                                                         w3triamd.F90  page  15
 
 
  771 !
  772 !  5. Called by :
  773 !
  774 !       Name      Type  Module   Description
  775 !     ----------------------------------------------------------------
  776 !      READTRI    Subr. Internal  Unstructured mesh definition.
  777 !     ----------------------------------------------------------------
  778 !
  779 !  6. Error messages :
  780 !
  781 !  7. Remarks :
  782 !
  783 !  8. Structure :
  784 !
  785 !  9. Switches :
  786 !
  787 ! 10. Source code :
  788 !
  789 !/ ------------------------------------------------------------------- /
  790     USE W3GDATMD
  791     USE CONSTANTS
  792                  
  793          IMPLICIT NONE
  794 !
  795 !local parameter
  796 !	
  797          INTEGER :: IP, IE
  798          INTEGER :: I1, I2, I3, I11, I22, I33
  799          REAL*8    :: P1(2), P2(2), P3(2)
  800          REAL*8    :: R1(2), R2(2), R3(2)
  801          REAL*8    :: N1(2), N2(2), N3(2)
  802 	 REAL*8    :: TMP(3)
  803 	 REAL*8    :: TMPINV(3)
  804          REAL*8    :: PT(3,2)
  805 !/ ------------------------------------------------------------------- /
  806                                                                         
  807                                                                         
  808          DO IE = 1, NTRI
  809 !
  810 ! vertices
  811 !
  812             I1 = TRIGP(IE,1)
  813             I2 = TRIGP(IE,2)
  814             I3 = TRIGP(IE,3)
  815                             
  816             CALL FIX_PERIODCITY(I1,I2,I3,XYB,PT)
  817                                                 
  818             P1(1) = PT(1,1)
  819             P1(2) = PT(1,2)
  820             P2(1) = PT(2,1)
  821             P2(2) = PT(2,2)
  822             P3(1) = PT(3,1)
  823             P3(2) = PT(3,2)
  824 !
  825 ! I1 -> I2, I2 -> I3, I3 -> I1 (anticlockwise orientation is preserved)
 
 
                                                         w3triamd.F90  page  16
 
 
  826 !
  827             R1 = P3-P2
  828             R2 = P1-P3
  829             R3 = P2-P1
  830                       
  831             N1(1) = (-R1(2))
  832             N1(2) = ( R1(1))
  833             N2(1) = (-R2(2))
  834             N2(2) = ( R2(1))
  835             N3(1) = (-R3(2))
  836             N3(2) = ( R3(1))
  837 !
  838 ! edges length
  839 !
  840             LEN(IE,1) = DSQRT(R1(1)**2+R1(2)**2)
  841             LEN(IE,2) = DSQRT(R2(1)**2+R2(2)**2)
  842             LEN(IE,3) = DSQRT(R3(1)**2+R3(2)**2)
  843 !
  844 ! inward normal used for propagation (not normalized)
  845 !
  846             IEN(IE,1) = N1(1)
  847             IEN(IE,2) = N1(2)
  848             IEN(IE,3) = N2(1)
  849             IEN(IE,4) = N2(2)
  850             IEN(IE,5) = N3(1)
  851             IEN(IE,6) = N3(2)
  852                              
  853          END DO
  854                
  855      END SUBROUTINE
  856                    
  857                    
  858       SUBROUTINE COUNT(TRIGPTEMP)
  859                                  
  860 !/ -------------------------------------------------------------------
  861 !/                  +-----------------------------------+
  862 !/                  | WAVEWATCH III           NOAA/NCEP |
  863 !/                  |           A. Roland               |
  864 !/                  |           F. Ardhuin              |
  865 !/                  |                        FORTRAN 90 |
  866 !/                  | Last update :          15-May-2008|
  867 !/                  +-----------------------------------+
  868 !/
  869 !/    15-May-2007 : Origination.                        ( version 3.13 )
  870 !/
  871 !
  872 !  1. Purpose :
  873 !
  874 !      Calculate global and maximum number of connection for array alloca>
  875 !
  876 !  2. Method :
  877 !
  878 !  3. Parameters :
  879 !     Parameter list
  880 !     ----------------------------------------------------------------
 
 
                                                         w3triamd.F90  page  17
 
 
  881 !       NTRI         Int.   I   Total number of triangle.
  882 !       TRIGPTEMP    Int    I   Temporary array of triangle vertices
  883 !       COUNTRI      Int    O   Maximum number of connected triangle
  884 !                               for a given points
  885 !       COUNTOT      Int    O   Global number of triangle connection
  886 !                               for the whole grid.   	
  887 !     ----------------------------------------------------------------
  888 !  4. Subroutines used :
  889 !
  890 !  5. Called by :
  891 !
  892 !       Name      Type  Module   Description
  893 !     ----------------------------------------------------------------
  894 !      READTRI    Subr. Internal  Unstructured mesh definition.
  895 !     ----------------------------------------------------------------
  896 !
  897 !  6. Error messages :
  898 !
  899 !  7. Remarks :
  900 !
  901 !  8. Structure :
  902 !
  903 !  9. Switches :
  904 !
  905 ! 10. Source code :
  906 !
  907 !/ ------------------------------------------------------------------- /
  908         USE W3GDATMD
  909         IMPLICIT NONE
  910                      
  911                      
  912 !/ parameter list
  913                  
  914    INTEGER,INTENT(IN) :: TRIGPTEMP(:,:)
  915 !/ ------------------------------------------------------------------- /
  916 !/ local parameter
  917                   
  918    INTEGER               :: CONN(NX)
  919    INTEGER               :: COUNTER, IP, IE, I, J, N(3)
  920                                                        
  921                                                        
  922 COUNTRI=0
  923 COUNTOT=0
  924 CONN(:)= 0
  925           
  926 !
  927 !calculate the number of connected triangles for a given point.
  928 !
  929  
  930 DO IE = 1,NTRI
  931    N(:) = 0.
  932    N(1) = TRIGPTEMP(IE,1)
  933    N(2) = TRIGPTEMP(IE,2)
  934    N(3) = TRIGPTEMP(IE,3)
  935    CONN(N(1)) = CONN(N(1)) + 1
 
 
                                                         w3triamd.F90  page  18
 
 
  936    CONN(N(2)) = CONN(N(2)) + 1
  937    CONN(N(3)) = CONN(N(3)) + 1
  938 ENDDO
  939      
  940  COUNTRI = MAXVAL(CONN)
  941 !
  942 ! calculate the global number of connections available through the mesh
  943 !
  944 J=0
  945  DO  IP=1,NX
  946    DO I=1,CONN(IP)
  947       J=J+1
  948    ENDDO
  949  ENDDO
  950  COUNTOT=J
  951           
  952 END SUBROUTINE
  953               
  954       SUBROUTINE COORDMAX
  955 !/ -------------------------------------------------------------------
  956 !/                  +-----------------------------------+
  957 !/                  | WAVEWATCH III           NOAA/NCEP |
  958 !/                  |           F. Ardhuin              |
  959 !/                  |                        FORTRAN 90 |
  960 !/                  | Last update :          15-May-2008|
  961 !/                  +-----------------------------------+
  962 !/
  963 !/    15-May-2007 : Origination.                        ( version 3.13 )
  964 !/
  965 !  1. Purpose :
  966 !
  967 !      Calculate first point and last point coordinates, and minimum and >
  968 !
  969 !  2. Method :
  970 !
  971 !  3. Parameters :
  972 !
  973 !  4. Subroutines used :
  974 !
  975 !  5. Called by :
  976 !
  977 !       Name      Type  Module   Description
  978 !     ----------------------------------------------------------------
  979 !      READTRI    Subr. Internal  Unstructured mesh definition.
  980 !     ----------------------------------------------------------------
  981 !
  982 !  6. Error messages :
  983 !
  984 !  7. Remarks :
  985 !
  986 !  8. Structure :
  987 !
  988 !  9. Switches :
  989 !
  990 ! 10. Source code :
 
 
                                                         w3triamd.F90  page  19
 
 
  991 !
  992 !/ ------------------------------------------------------------------- /
  993           USE W3GDATMD
  994           IMPLICIT NONE
  995                        
  996                        
  997 !
  998 ! maximum of coordinates s
  999 !
 1000     MAXX = MAXVAL(XYB(:,1))
 1001     MAXY = MAXVAL(XYB(:,2))
 1002 !
 1003 ! minimum of coordinates
 1004 !
 1005     X0 = MINVAL(XYB(:,1))
 1006     Y0 = MINVAL(XYB(:,2))
 1007 !
 1008 !maximum and minimum length of edges
 1009 !
 1010     DXYMAX = MAXVAL(LEN(:,:))
 1011     SX = MINVAL(LEN(:,:))
 1012     SY = SX
 1013 !
 1014  END SUBROUTINE
 1015 !-------------------------------------------------------------------------
 1016                                                                           
 1017   SUBROUTINE AREA_SI(IMOD)
 1018 !/ -------------------------------------------------------------------
 1019 !/                  +-----------------------------------+
 1020 !/                  | WAVEWATCH III           NOAA/NCEP |
 1021 !/                  |           A. Roland               |
 1022 !/                  |                        FORTRAN 90 |
 1023 !/                  | Last update :          23-Aug-2011|
 1024 !/                  +-----------------------------------+
 1025 !/
 1026 !/    15-May-2007 : Origination: adjustment from the WWM code       ( ver>
 1027 !/    23-Aug-2011 : Removes double entries in VNEIGH                ( ver>
 1028 !/
 1029 !
 1030 !  1. Purpose :
 1031 !
 1032 !      Define optimized connection arrays (points and triangles) for spat>
 1033 !
 1034 !  2. Method :
 1035 !
 1036 !  3. Parameters :
 1037 !
 1038 !  4. Subroutines used :
 1039 !
 1040 !  5. Called by :
 1041 !
 1042 !       Name      Type  Module   Description
 1043 !     ----------------------------------------------------------------
 1044 !      READTRI    Subr. Internal  Unstructured mesh definition.
 1045 !     ----------------------------------------------------------------
 
 
                                                         w3triamd.F90  page  20
 
 
 1046 !
 1047 !  6. Error messages :
 1048 !
 1049 !  7. Remarks :
 1050 !
 1051 !     The storage is optimize especially considering the iterative solver>
 1052 !     The schemes used are vertex-centered, a point has to be considered >
 1053 !     median dual cell. For a given point, the surface of the dual cell i>
 1054 !     of the sum of the surface of connected triangles.
 1055 !     This routine is from WWM developped in Darmstadt(Aaron Roland)
 1056 !
 1057 !  8. Structure :
 1058 !
 1059 !  9. Switches :
 1060 !
 1061 ! 10. Source code :
 1062 !
 1063 !/ ------------------------------------------------------------------- /
 1064                                                                         
 1065         USE W3GDATMD
 1066          IMPLICIT NONE
 1067 !/ input
 1068         
 1069          INTEGER, INTENT(IN) :: IMOD
 1070                                     
 1071 !/ local parameters
 1072                    
 1073          INTEGER :: COUNTER,ifound,alreadyfound
 1074          INTEGER :: I, J, K, II
 1075          INTEGER :: IP, IE, POS, POS_I, POS_J, POS_K, IP_I, IP_J, IP_K
 1076          INTEGER :: I1, I2, I3, IP2, CHILF(NX)
 1077          INTEGER :: TMP(NX), CELLVERTEX(NX,COUNTRI,2)
 1078          INTEGER :: COUNT_MAX
 1079          DOUBLE PRECISION   :: TRIA03
 1080          INTEGER, ALLOCATABLE :: PTABLE(:,:)
 1081                                             
 1082 !/ ------------------------------------------------------------------- /
 1083                                                                         
 1084                                                                         
 1085          WRITE(*,'("+TRACE......",A)') 'COMPUTE SI, TRIA und CCON'
 1086                                                                   
 1087          SI(:) = 0.D0
 1088 !
 1089          CCON(:) = 0     ! Number of connected Elements
 1090          DO IE = 1 , NTRI
 1091            I1 = TRIGP(IE,1)
 1092            I2 = TRIGP(IE,2)
 1093            I3 = TRIGP(IE,3)
 1094            CCON(I1) = CCON(I1) + 1
 1095            CCON(I2) = CCON(I2) + 1
 1096            CCON(I3) = CCON(I3) + 1
 1097            TRIA03 = 1./3. * TRIA(IE)
 1098            SI(I1) = SI(I1) + TRIA03
 1099            SI(I2) = SI(I2) + TRIA03
 1100            SI(I3) = SI(I3) + TRIA03
 
 
                                                         w3triamd.F90  page  21
 
 
 1101            ENDDO
 1102                 
 1103          CELLVERTEX(:,:,:) = 0 ! Stores for each node the Elementnumbers >
 1104                                ! and the Position of the position of the >
 1105                                                                           
 1106          WRITE(*,'("+TRACE......",A)') 'COMPUTE CELLVERTEX'
 1107                                                            
 1108          CHILF             = 0
 1109                               
 1110          DO IE = 1, NTRI
 1111            DO J=1,3
 1112              I = TRIGP(IE,J)!INE(J,IE)
 1113              CHILF(I) = CHILF(I)+1
 1114              CELLVERTEX(I,CHILF(I),1) = IE
 1115              CELLVERTEX(I,CHILF(I),2) = J
 1116            END DO
 1117          ENDDO
 1118               
 1119          WRITE(*,'("+TRACE......",A)') 'COMPUTE IE_CELL and POS_CELL'
 1120 !
 1121 ! Second step in storage, the initial 3D array CELLVERTEX, is transformed>
 1122 ! the global index is J . From now, all the computation step based on the>
 1123 ! abide by the conservation of the 2 loop algorithm (points + connected t>
 1124 ! AR: I will change this now to pointers in order to omit fix loop struct>
 1125 !	
 1126          INDEX_CELL(1)=1
 1127          J = 0
 1128          DO IP = 1, NX
 1129            DO I = 1, CCON(IP)
 1130              J = J + 1
 1131              IE_CELL(J)  = CELLVERTEX(IP,I,1)
 1132              POS_CELL(J) = CELLVERTEX(IP,I,2)
 1133            END DO
 1134            INDEX_CELL(IP+1)=J+1
 1135          END DO
 1136                
 1137          IF (.NOT. FSNIMP) RETURN
 1138                                  
 1139          J = 0
 1140          DO IP = 1, NX
 1141            DO I = 1, CCON(IP)
 1142              J = J + 1
 1143            END DO
 1144          END DO
 1145                
 1146          COUNT_MAX = J
 1147                       
 1148            ALLOCATE(PTABLE(COUNT_MAX,7))
 1149                                         
 1150            J = 0
 1151            PTABLE(:,:) = 0.
 1152            DO IP = 1, NX
 1153              DO I = 1, CCON(IP)
 1154                J = J + 1
 1155                IE    = IE_CELL(J)
 
 
                                                         w3triamd.F90  page  22
 
 
 1156                POS   = POS_CELL(J)
 1157                I1 = TRIGP(IE,1)
 1158                I2 = TRIGP(IE,2)
 1159                I3 = TRIGP(IE,3)
 1160                IF (POS == 1) THEN
 1161                  POS_J = 2
 1162                  POS_K = 3
 1163                ELSE IF (POS == 2) THEN
 1164                  POS_J = 3
 1165                  POS_K = 1
 1166                ELSE
 1167                  POS_J = 1
 1168                  POS_K = 2
 1169                END IF
 1170                IP_I = IP
 1171                IP_J = TRIGP(IE,POS_J)
 1172                IP_K = TRIGP(IE,POS_K)
 1173                PTABLE(J,1) = IP_I
 1174                PTABLE(J,2) = IP_J
 1175                PTABLE(J,3) = IP_K
 1176                PTABLE(J,4) = POS
 1177                PTABLE(J,5) = POS_J
 1178                PTABLE(J,6) = POS_K
 1179                PTABLE(J,7) = IE
 1180              END DO
 1181            END DO
 1182                  
 1183 !           WRITE(*,'("+TRACE......",A)') 'SET UP SPARSE MATRIX POINTER .>
 1184                                                                           
 1185            J = 0
 1186            K = 0
 1187            DO IP = 1, NX
 1188              TMP(:) = 0
 1189              DO I = 1, CCON(IP)
 1190                J = J + 1
 1191                IP_J  = PTABLE(J,2)
 1192                IP_K  = PTABLE(J,3)
 1193                POS   = PTABLE(J,4)
 1194                TMP(IP)   = 1
 1195                TMP(IP_J) = 1
 1196                TMP(IP_K) = 1
 1197             END DO
 1198             K = K + SUM(TMP)
 1199           END DO
 1200                 
 1201           NNZ => GRIDS(IMOD)%NNZ
 1202                                 
 1203           NNZ = K
 1204                  
 1205 !          WRITE(*,'("+TRACE......",A)') 'SET UP SPARSE MATRIX POINTER ..>
 1206                                                                           
 1207           ALLOCATE (GRIDS(IMOD)%JAA(NNZ))
 1208           ALLOCATE (GRIDS(IMOD)%IAA(NX+1))
 1209           ALLOCATE (GRIDS(IMOD)%POSI(3,COUNT_MAX))
 1210           JAA   => GRIDS(IMOD)%JAA
 
 
                                                         w3triamd.F90  page  23
 
 
 1211           IAA   => GRIDS(IMOD)%IAA
 1212           POSI  => GRIDS(IMOD)%POSI
 1213                                    
 1214            J = 0
 1215            K = 0
 1216            IAA(1) = 1
 1217            JAA    = 0
 1218            DO IP = 1, NX ! Run through all rows
 1219              TMP(:)=0
 1220              DO I = 1, CCON(IP)         ! Check how many entries there ar>
 1221                J = J + 1                ! this is the same J index as in >
 1222                IP_J  = PTABLE(J,2)
 1223                IP_K  = PTABLE(J,3)
 1224                TMP(IP)   = 1
 1225                TMP(IP_J) = 1
 1226                TMP(IP_K) = 1
 1227              END DO
 1228              DO I = 1, NX               ! Run through all columns
 1229                IF (TMP(I) .GT. 0) THEN  ! this is true only for the conne>
 1230                  K = K + 1
 1231                  JAA(K) = I
 1232                END IF
 1233              END DO
 1234              IAA(IP + 1) = K + 1
 1235            END DO
 1236                  
 1237            POSI = 0
 1238            J = 0
 1239            DO IP = 1, NX
 1240              DO I = 1, CCON(IP)
 1241                J = J + 1
 1242                IP_J  = PTABLE(J,2)
 1243                IP_K  = PTABLE(J,3)
 1244                DO K = IAA(IP), IAA(IP+1) - 1
 1245                  IF (IP   == JAA(K)) POSI(1,J)  = K
 1246                  IF (IP_J == JAA(K)) POSI(2,J)  = K
 1247                  IF (IP_K == JAA(K)) POSI(3,J)  = K
 1248                  IF (K == 0) THEN
 1249                   WRITE(*,*) 'ERROR IN AREA_SI K .EQ. 0'
 1250                   STOP
 1251                  END IF
 1252                END DO
 1253             END DO
 1254           END DO
 1255                 
 1256           DEALLOCATE(PTABLE)
 1257                             
 1258        END SUBROUTINE
 1259                      
 1260      SUBROUTINE IS_IN_UNGRID(IMOD, XTIN, YTIN, ITOUT, IS, JS, RW)
 1261 !/ -------------------------------------------------------------------
 1262 !/                  +-----------------------------------+
 1263 !/                  | WAVEWATCH III           NOAA/NCEP |
 1264 !/                  |      Mathieu Dutour Sikiric, IRB  |
 1265 !/                  |                 Aron Roland, Z&P  |
 
 
                                                         w3triamd.F90  page  24
 
 
 1266 !/                  |             Fabrice Ardhuin       |
 1267 !/                  |                        FORTRAN 90 |
 1268 !/                  | Last update :          26-Jan-2014|
 1269 !/                  +-----------------------------------+
 1270 !/
 1271 !/ Adapted from other subroutine
 1272 !/    15-Oct-2007 : Origination.                        ( version 3.13 )
 1273 !/    21-Sep-2012 : Uses same interpolation as regular  ( version 4.08 )
 1274 !/    26-Jan-2014 : Correcting bug in RW                ( version 4.18 )
 1275 !/
 1276 !  1. Purpose :
 1277 !
 1278 !      Determine whether a point is inside or outside an unstructured gri>
 1279 !      and returns index of triangle and interpolation weights
 1280 !      This is the analogue for triangles of the FUNCTION W3GRMP
 1281 !
 1282 !  2. Method :
 1283 !
 1284 !     Using barycentric coordinates defined as the ratio of triangle alge>
 1285 !     which are positive or negative.
 1286 !     Computes the 3 interpolation weights for each triangle until they a>
 1287 !
 1288 !  3. Parameters :
 1289 !
 1290 !     Parameter list
 1291 !     ----------------------------------------------------------------
 1292 !       IMOD    Int.   I   Model number to point to.
 1293 !       XTIN    Real   I   X-coordinate of target point.
 1294 !       YTIN    Real   I   Y-coordinate of target point.
 1295 !       ITOUT    Int.   I   Model number to point to.
 1296 !       IS,JS   I.A.   O   (I,J) indices of vertices of enclosing grid ce>
 1297 !       RW      R.A.   O   Array of interpolation weights.
 1298 !     ----------------------------------------------------------------
 1299 !
 1300 !  4. Subroutines used :
 1301 !
 1302 !     None
 1303 !
 1304 !  5. Called by :
 1305 !
 1306 !     WMGLOW, W3IOPP, WMIOPP, WW3_GINT
 1307 !
 1308 !  6. Error messages :
 1309 !
 1310 !     - Error checks on previous setting of variable.
 1311 !
 1312 !  7. Remarks :
 1313 !
 1314 !  8. Structure :
 1315 !
 1316 !  9. Switches :
 1317 !
 1318 !     !/S    Enable subroutine tracing.
 1319 !     !/T    Enable test output
 1320 !
 
 
                                                         w3triamd.F90  page  25
 
 
 1321 ! 10. Source code :
 1322 !
 1323  
 1324  
 1325 !  2. Method :
 1326 !
 1327 !     Using barycentric coordinates. Each coefficient depends on the mass>
 1328 !
 1329 !  3. Parameters :
 1330 !
 1331 !  4. Subroutines used :
 1332 !
 1333 !  5. Called by :
 1334 !
 1335 !       Name      Type  Module   Description
 1336 !     ----------------------------------------------------------------
 1337 !      W3IOPP    Subr. Internal  Preprocessing of point output.
 1338 !     ----------------------------------------------------------------
 1339 !
 1340 !  6. Error messages :
 1341 !
 1342 !  7. Remarks :
 1343 !
 1344 !      This subroutine is adjusted from CREST code (Fabrice Ardhuin)
 1345 !      For a given output point, the algorithm enable to glance through a>
 1346 !      to find the one the point belong to, and then make interpolation.
 1347 !
 1348 !  8. Structure :
 1349 !
 1350 !  9. Switches :
 1351 !
 1352 !       !/LLG   Spherical grid.
 1353 !       !/XYG   Carthesian grid.
 1354 !
 1355 ! 10. Source code :
 1356 !
 1357 !/ ------------------------------------------------------------------- /
 1358       USE W3GDATMD
 1359       USE W3SERVMD, ONLY: EXTCDE
 1360       USE W3ODATMD, ONLY: NDSE
 1361       IMPLICIT NONE
 1362                    
 1363 !/ ------------------------------------------------------------------- /
 1364 ! Parameter list
 1365                 
 1366      INTEGER, INTENT(IN)            :: IMOD
 1367      REAL   , INTENT(IN)            :: XTIN, YTIN
 1368      INTEGER, INTENT(OUT)           :: itout
 1369      INTEGER, INTENT(OUT)           :: IS(4), JS(4)
 1370      REAL, INTENT(OUT)              :: RW(4)
 1371 !/ ------------------------------------------------------------------- /
 1372 !local parameters
 1373                  
 1374      DOUBLE PRECISION             :: x1, x2, x3
 1375      DOUBLE PRECISION             :: y1, y2, y3
 
 
                                                         w3triamd.F90  page  26
 
 
 1376      DOUBLE PRECISION             :: s1, s2, s3, sg1, sg2, sg3
 1377      REAL*8                       :: PT(3,2)
 1378      INTEGER                      :: ITRI
 1379      INTEGER                      :: I1, I2, I3
 1380      INTEGER                      :: nbFound
 1381                                             
 1382 !
 1383      itout = 0
 1384      nbFound=0
 1385      ITRI = 0
 1386      DO WHILE (nbFound.EQ.0.AND.ITRI.LT.GRIDS(IMOD)%NTRI)
 1387        ITRI = ITRI +1
 1388        I1=GRIDS(IMOD)%TRIGP(ITRI,1)
 1389        I2=GRIDS(IMOD)%TRIGP(ITRI,2)
 1390        I3=GRIDS(IMOD)%TRIGP(ITRI,3)
 1391                                    
 1392        CALL FIX_PERIODCITY(I1,I2,I3,GRIDS(IMOD)%XYB,PT)
 1393 ! coordinates of the first vertex A
 1394        x1 = PT(1,1)
 1395        y1 = PT(1,2)
 1396 ! coordinates of the 2nd vertex B
 1397        x2 = PT(2,1)
 1398        y2 = PT(2,2)
 1399 !coordinates of the 3rd vertex C
 1400        x3 = PT(3,1)
 1401        y3 = PT(3,2)
 1402 !with M = (XTIN,YTIN) the target point ...
 1403 !vector product of AB and AC
 1404        sg3=(y3-y1)*(x2-x1)-(x3-x1)*(y2-y1)
 1405 !vector product of AB and AM
 1406        s3=(YTIN-y1)*(x2-x1)-(XTIN-x1)*(y2-y1)
 1407 !vector product of BC and BA
 1408        sg1=(y1-y2)*(x3-x2)-(x1-x2)*(y3-y2)
 1409 !vector product of BC and BM
 1410        s1=(YTIN-y2)*(x3-x2)-(XTIN-x2)*(y3-y2)
 1411 !vector product of CA and CB
 1412        sg2=(y2-y3)*(x1-x3)-(x2-x3)*(y1-y3)
 1413 !vector product of CA and CM
 1414        s2=(YTIN-y3)*(x1-x3)-(XTIN-x3)*(y1-y3)
 1415        IF ((s1*sg1.GE.0).AND.(s2*sg2.GE.0).AND.(s3*sg3.GE.0)) THEN
 1416          itout=ITRI
 1417          nbFound=nbFound+1
 1418          IS(1)=I1
 1419          IS(2)=I2
 1420          IS(3)=I3
 1421          IS(4)=1
 1422          JS(:)=1
 1423          RW(1)=s1/sg1
 1424          RW(2)=s2/sg2
 1425          RW(3)=1.-RW(1)-RW(2)  !s3/sg3
 1426          RW(4)=0.
 1427        END IF
 1428      ENDDO
 1429      END SUBROUTINE IS_IN_UNGRID
 1430 !/ -------------------------------------------------------------------
 
 
                                                         w3triamd.F90  page  27
 
 
 1431      SUBROUTINE IS_IN_UNGRID2(IMOD, XTIN, YTIN, FORCE, ITOUT, IS, JS, RW)
 1432 !/ -------------------------------------------------------------------
 1433 !/                  +-----------------------------------+
 1434 !/                  | WAVEWATCH III           NOAA/NCEP |
 1435 !/                  |      Mathieu Dutour Sikiric, IRB  |
 1436 !/                  |                 Aron Roland, Z&P  |
 1437 !/                  |             Fabrice Ardhuin       |
 1438 !/                  |                        FORTRAN 90 |
 1439 !/                  | Last update :          26-Jan-2014|
 1440 !/                  +-----------------------------------+
 1441 !/
 1442 !/ Adapted from other subroutine
 1443 !/    15-Oct-2007 : Origination.                        ( version 3.13 )
 1444 !/    21-Sep-2012 : Uses same interpolation as regular  ( version 4.08 )
 1445 !/    26-Jan-2014 : Correcting bug in RW                ( version 4.18 )
 1446 !/
 1447 !  1. Purpose :
 1448 !
 1449 !      Determine whether a point is inside or outside an unstructured gri>
 1450 !      and returns index of triangle and interpolation weights
 1451 !      This is the analogue for triangles of the FUNCTION W3GRMP
 1452 !
 1453 !  2. Method :
 1454 !
 1455 !     Using barycentric coordinates defined as the ratio of triangle alge>
 1456 !     which are positive or negative.
 1457 !     Computes the 3 interpolation weights for each triangle until they a>
 1458 !
 1459 !  3. Parameters :
 1460 !
 1461 !     Parameter list
 1462 !     ----------------------------------------------------------------
 1463 !       IMOD    Int.   I   Model number to point to.
 1464 !       XTIN    Real   I   X-coordinate of target point.
 1465 !       YTIN    Real   I   Y-coordinate of target point.
 1466 !       ITOUT    Int.   I   Model number to point to.
 1467 !       IS,JS   I.A.   O   (I,J) indices of vertices of enclosing grid ce>
 1468 !       RW      R.A.   O   Array of interpolation weights.
 1469 !     ----------------------------------------------------------------
 1470 !
 1471 !  4. Subroutines used :
 1472 !
 1473 !     None
 1474 !
 1475 !  5. Called by :
 1476 !
 1477 !     WMGLOW, W3IOPP, WMIOPP, WW3_GINT
 1478 !
 1479 !  6. Error messages :
 1480 !
 1481 !     - Error checks on previous setting of variable.
 1482 !
 1483 !  7. Remarks :
 1484 !
 1485 !  8. Structure :
 
 
                                                         w3triamd.F90  page  28
 
 
 1486 !
 1487 !  9. Switches :
 1488 !
 1489 !     !/S    Enable subroutine tracing.
 1490 !     !/T    Enable test output
 1491 !
 1492 ! 10. Source code :
 1493 !
 1494  
 1495  
 1496 !  2. Method :
 1497 !
 1498 !     Using barycentric coordinates. Each coefficient depends on the mass>
 1499 !
 1500 !  3. Parameters :
 1501 !
 1502 !  4. Subroutines used :
 1503 !
 1504 !  5. Called by :
 1505 !
 1506 !       Name      Type  Module   Description
 1507 !     ----------------------------------------------------------------
 1508 !      W3IOPP    Subr. Internal  Preprocessing of point output.
 1509 !     ----------------------------------------------------------------
 1510 !
 1511 !  6. Error messages :
 1512 !
 1513 !  7. Remarks :
 1514 !
 1515 !      This subroutine is adjusted from CREST code (Fabrice Ardhuin)
 1516 !      For a given output point, the algorithm enable to glance through a>
 1517 !      to find the one the point belong to, and then make interpolation.
 1518 !
 1519 !  8. Structure :
 1520 !
 1521 !  9. Switches :
 1522 !
 1523 !       !/LLG   Spherical grid.
 1524 !       !/XYG   Carthesian grid.
 1525 !
 1526 ! 10. Source code :
 1527 !
 1528 !/ ------------------------------------------------------------------- /
 1529       USE W3GDATMD
 1530       USE W3SERVMD, ONLY: EXTCDE
 1531       USE W3ODATMD, ONLY: NDSE
 1532       IMPLICIT NONE
 1533                    
 1534 !/ ------------------------------------------------------------------- /
 1535 ! Parameter list
 1536                 
 1537      INTEGER, INTENT(IN)            :: IMOD, FORCE
 1538      REAL   , INTENT(IN)            :: XTIN, YTIN
 1539      INTEGER, INTENT(OUT)           :: itout
 1540      INTEGER, INTENT(OUT)           :: IS(4), JS(4)
 
 
                                                         w3triamd.F90  page  29
 
 
 1541      REAL, INTENT(OUT)              :: RW(4)
 1542 !/ ------------------------------------------------------------------- /
 1543 !local parameters
 1544                  
 1545      DOUBLE PRECISION             :: x1, x2, x3, D1, D2, D3, DISTMIN, DDM>
 1546      DOUBLE PRECISION             :: s1, s2, s3, sg1, sg2, sg3, smin, ssum
 1547      DOUBLE PRECISION             :: y1, y2, y3
 1548      INTEGER                      :: ITRI, ITRIS
 1549      INTEGER                      :: I1, I2, I3
 1550      INTEGER                      :: nbFound
 1551      LOGICAL                      :: MAPSTAOK
 1552                                              
 1553 !
 1554      itout = 0
 1555      nbFound=0
 1556      ITRI = 0
 1557      ITRIS = 1
 1558      ssum = 0
 1559      smin = 0
 1560      DO WHILE (nbFound.EQ.0.AND.ITRI.LT.GRIDS(IMOD)%NTRI)
 1561        ITRI = ITRI +1
 1562        I1=GRIDS(IMOD)%TRIGP(ITRI,1)
 1563        I2=GRIDS(IMOD)%TRIGP(ITRI,2)
 1564        I3=GRIDS(IMOD)%TRIGP(ITRI,3)
 1565 ! coordinates of the first vertex A
 1566        x1=GRIDS(IMOD)%XYB(I1,1)
 1567        y1=GRIDS(IMOD)%XYB(I1,2)
 1568 ! coordinates of the 2nd vertex B
 1569        x2=GRIDS(IMOD)%XYB(I2,1)
 1570        y2=GRIDS(IMOD)%XYB(I2,2)
 1571 !coordinates of the 3rd vertex C
 1572        x3=GRIDS(IMOD)%XYB(I3,1)
 1573        y3=GRIDS(IMOD)%XYB(I3,2)
 1574 !with M = (XTIN,YTIN) the target point ...
 1575 !vector product of AB and AC
 1576        sg3=(y3-y1)*(x2-x1)-(x3-x1)*(y2-y1)
 1577 !vector product of AB and AM
 1578        s3=(YTIN-y1)*(x2-x1)-(XTIN-x1)*(y2-y1)
 1579 !vector product of BC and BA
 1580        sg1=(y1-y2)*(x3-x2)-(x1-x2)*(y3-y2)
 1581 !vector product of BC and BM
 1582        s1=(YTIN-y2)*(x3-x2)-(XTIN-x2)*(y3-y2)
 1583 !vector product of CA and CB
 1584        sg2=(y2-y3)*(x1-x3)-(x2-x3)*(y1-y3)
 1585 !vector product of CA and CM
 1586        s2=(YTIN-y3)*(x1-x3)-(XTIN-x3)*(y1-y3)
 1587 !     ssum = ABS(s1*sg1)+ABS(s2*sg2)+ABS(s3*sg3)
 1588        MAPSTAOK = ((GRIDS(IMOD)%MAPSTA(1,I1).GE.1).AND. &
 1589                    (GRIDS(IMOD)%MAPSTA(1,I2).GE.1).AND.(GRIDS(IMOD)%MAPST>
 1590        IF (FORCE.LT.2) MAPSTAOK =.TRUE.
 1591        ssum = (XTIN-(x1+x2+x3)/3.)**2+(YTIN-(y1+y2+y2)/3.)**2
 1592        IF (smin.EQ.0.AND. MAPSTAOK ) smin=ssum
 1593        !WRITE(6,*) 'ssum',ITRI,MAPSTAOK,ssum,smin
 1594        IF (ssum.LT.smin .AND.  MAPSTAOK  ) THEN
 1595           smin=ssum
 
 
                                                         w3triamd.F90  page  30
 
 
 1596           ITRIS=ITRI
 1597        ENDIF
 1598        IF ((s1*sg1.GE.0).AND.(s2*sg2.GE.0).AND.(s3*sg3.GE.0)) THEN
 1599          itout=ITRI
 1600          nbFound=nbFound+1
 1601          IS(1)=I1
 1602          IS(2)=I2
 1603          IS(3)=I3
 1604          IS(4)=1
 1605          JS(:)=1
 1606          RW(1)=s1/sg1
 1607          RW(2)=s2/sg2
 1608          RW(3)=1.-RW(1)-RW(2)  !s3/sg3
 1609          RW(4)=0.
 1610        END IF
 1611      ENDDO
 1612      IF (itout.EQ.0.AND.FORCE.GT.0) THEN
 1613        ITRI=ITRIS
 1614        I1=GRIDS(IMOD)%TRIGP(ITRI,1)
 1615        I2=GRIDS(IMOD)%TRIGP(ITRI,2)
 1616        I3=GRIDS(IMOD)%TRIGP(ITRI,3)
 1617 ! coordinates of the first vertex A
 1618        x1=GRIDS(IMOD)%XYB(I1,1)
 1619        y1=GRIDS(IMOD)%XYB(I1,2)
 1620 ! coordinates of the 2nd vertex B
 1621        x2=GRIDS(IMOD)%XYB(I2,1)
 1622        y2=GRIDS(IMOD)%XYB(I2,2)
 1623 !coordinates of the 3rd vertex C
 1624        x3=GRIDS(IMOD)%XYB(I3,1)
 1625        y3=GRIDS(IMOD)%XYB(I3,2)
 1626        D1=(XTIN-X1)**2+(YTIN-Y1)**2
 1627        D2=(XTIN-X2)**2+(YTIN-Y2)**2
 1628        D3=(XTIN-X3)**2+(YTIN-Y3)**2
 1629          IF (D1.LE.D2.AND.D1.LE.D3) IS(1)=I1
 1630          IF (D2.LE.D1.AND.D2.LE.D3) IS(1)=I2
 1631          IF (D3.LE.D2.AND.D3.LE.D1) IS(1)=I3
 1632          IS(2:4)=1
 1633          JS(:)=1
 1634          RW(1)=1
 1635          RW(2:4)=0.
 1636          ITOUT=ITRI
 1637                    
 1638      ENDIF
 1639      END SUBROUTINE IS_IN_UNGRID2
 1640 !/ ------------------------------------------------------------------- /
 1641       SUBROUTINE UG_GRADIENTS (PARAM, DIFFX, DIFFY)
 1642 !/                  +-----------------------------------+
 1643 !/                  | WAVEWATCH III           NOAA/NCEP |
 1644 !/                  |           F. Ardhuin              |
 1645 !/                  |           A. Roland               |
 1646 !/                  |                        FORTRAN 90 |
 1647 !/                  | Last update :          14-Oct-2013|
 1648 !/                  +-----------------------------------+
 1649 !/
 1650 !/    15-Nov-2007 : Origination.                        ( version 3.13 )
 
 
                                                         w3triamd.F90  page  31
 
 
 1651 !/    31-Oct-2010 : Merging of 4.03 with 3.14-Ifremer   ( version 4.04 )
 1652 !/    08-Nov-2011 : Correction for zero grad. on contour( version 4.04 )
 1653 !/    14-Oct-2013 : Correction  of latitude factor      ( version 4.12 )
 1654 !/    01-Mai-2018 : Using linear shape function for gradients [ version 6>
 1655 !/
 1656 !
 1657 !  1. purpose: calculate gradients at a point via its connection.
 1658 !  2. Method : using linear shape function this is a basis on which
 1659 !              all advection schemes in Roland (2008) are checked.
 1660 !
 1661 !  3. Parameters :
 1662 !     PARAM : depth or current field (indices 0 to NSEA)
 1663 !     DIFFX :  x gradient            (indices 1 to NX)
 1664 !     DIFFY :  y gradient            (indices 1 to NX)
 1665 !
 1666 !  4. Subroutines used :
 1667 !
 1668 !  5. Called by :
 1669 !
 1670 !       Name      Type  Module   Description
 1671 !     ----------------------------------------------------------------
 1672 !      W3WAVE    Subr.          Actual wind wave routine
 1673 !     ----------------------------------------------------------------
 1674 !
 1675 !  6. Error messages :
 1676 !
 1677 !  7. Remarks :
 1678 !
 1679 !      This subroutine is adjusted from WWM code (Aaron Roland)
 1680 !
 1681 !  8. Structure :
 1682 !
 1683 !  9. Switches :
 1684 !
 1685 ! 10. Source code :
 1686       USE CONSTANTS
 1687       USE W3GDATMD, ONLY : TRIGP, NTRI, NX, NSEA, MAPFS, CLATIS, &
 1688                            MAPSTA, ANGLE, FLAGLL,  IOBP, IEN, TRIA, NSEAL>
 1689       USE W3ADATMD, ONLY : NSEALM
 1690                                  
 1691       IMPLICIT NONE
 1692                    
 1693                    
 1694       REAL, INTENT(IN)     :: PARAM(0:NSEA)
 1695       REAL, INTENT(OUT)    :: DIFFX(:,:), DIFFY(:,:)
 1696                                                     
 1697 ! local parameters
 1698                   
 1699       INTEGER              :: VERTICES(3), NI(3), NI_GL(3)
 1700       REAL                 :: TMP1(3), TMP2(3)
 1701       INTEGER              :: I, IX, IE, IE_GL
 1702       REAL                 :: VAR(3), FACT, LATMEAN
 1703       REAL                 :: DIFFXTMP, DIFFYTMP
 1704       REAL                 :: DEDX(3), DEDY(3)
 1705       REAL                 :: DVDXIE, DVDYIE
 
 
                                                         w3triamd.F90  page  32
 
 
 1706       REAL                 :: WEI(NX), WEI_LOCAL(NSEAL)
 1707                                                        
 1708      DIFFX = 0.
 1709      DIFFY = 0.
 1710 !
 1711      IF (FLAGLL) THEN
 1712        FACT=1./(DERA*RADIUS)
 1713      ELSE
 1714        FACT=1.
 1715      END IF
 1716            
 1717        WEI = 0.
 1718        DO IE = 1, NTRI
 1719          NI = TRIGP(IE,:)
 1720          LATMEAN = 1./3. * SUM(CLATIS(MAPFS(1,NI)))
 1721          WEI(NI) = WEI(NI) + 2.*TRIA(IE)
 1722          IF (TRIA(IE) .LT. TINY(1.)) THEN
 1723            WRITE(*,*) 'AREA SMALLER ZERO', IE, NTRI, TRIA(IE)
 1724            STOP
 1725          ENDIF
 1726          DEDX(1)     = IEN(IE,1)
 1727          DEDX(2)     = IEN(IE,3)
 1728          DEDX(3)     = IEN(IE,5)
 1729          DEDY(1)     = IEN(IE,2)
 1730          DEDY(2)     = IEN(IE,4)
 1731          DEDY(3)     = IEN(IE,6)
 1732          VAR         = PARAM(MAPFS(1,NI)) * FACT
 1733          DVDXIE      = DOT_PRODUCT( VAR,DEDX)
 1734          DVDYIE      = DOT_PRODUCT( VAR,DEDY)
 1735          DIFFX(1,NI) = DIFFX(1,NI) + DVDXIE * LATMEAN
 1736          DIFFY(1,NI) = DIFFY(1,NI) + DVDYIE
 1737        END DO
 1738        DIFFX(1,:) = DIFFX(1,:)/WEI
 1739        DIFFY(1,:) = DIFFY(1,:)/WEI
 1740                                   
 1741 !
 1742     END SUBROUTINE UG_GRADIENTS
 1743 !/ ------------------------------------------------------------------- /
 1744     SUBROUTINE W3NESTUG(DISTMIN,FLOK)
 1745 !/
 1746 !/                  +-----------------------------------+
 1747 !/                  | WAVEWATCH III           NOAA/NCEP |
 1748 !/                  |                                   |
 1749 !/                  | Aron Roland (BGS IT&E GmbH)       |
 1750 !/                  | Mathieu Dutour-Sikiric (IRB)      |
 1751 !/                  |                                   |
 1752 !/                  |                        FORTRAN 90 |
 1753 !/                  | Last update :        01-June-2018 |
 1754 !/                  +-----------------------------------+
 1755 !/
 1756 !/    01-June-2018 : Origination.                        ( version 6.04 )
 1757 !/
 1758 !  1. Purpose : UGTYPE nesting initialization
 1759 !  2. Method :
 1760 !  3. Parameters :
 
 
                                                         w3triamd.F90  page  33
 
 
 1761 !
 1762 !     Parameter list
 1763 !     ----------------------------------------------------------------
 1764 !     ----------------------------------------------------------------
 1765 !
 1766 !  4. Subroutines used :
 1767 !
 1768 !      Name      Type  Module   Description
 1769 !     ----------------------------------------------------------------
 1770 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1771 !     ----------------------------------------------------------------
 1772 !
 1773 !  5. Called by :
 1774 !
 1775 !      Name      Type  Module   Description
 1776 !     ----------------------------------------------------------------
 1777 !     ----------------------------------------------------------------
 1778 !
 1779 !  6. Error messages :
 1780 !  7. Remarks
 1781 !  8. Structure :
 1782 !  9. Switches :
 1783 !
 1784 !     !/S  Enable subroutine tracing.
 1785 !
 1786 ! 10. Source code :
 1787 !
 1788 !/ ------------------------------------------------------------------- /
 1789 !
 1790     USE W3ODATMD, ONLY: NBI, NDSE, ISBPI, XBPI, YBPI
 1791     USE W3GDATMD, ONLY: NX, XYB, XGRD, YGRD, MAPSTA, MAPFS, MAPSF
 1792                                                                  
 1793                                                                  
 1794     REAL, INTENT(IN)         :: DISTMIN
 1795     LOGICAL, INTENT(INOUT)         :: FLOK
 1796                                           
 1797     INTEGER                   :: I, J, JMEMO, IS, IX,  N, IX1(NBI)
 1798     REAL                      :: DIST, DIST0
 1799 !
 1800     N = 0
 1801 !
 1802 !1. look for input boundary point index
 1803 ! warning: if land points are included as boundary points to abide by the>
 1804 ! file, their status should be -2.
 1805 !
 1806     IX1 = 0
 1807     ISBPI = 1
 1808     DO IX = 1, NX
 1809       IF (ABS(MAPSTA (1,IX)) .EQ. 2) THEN
 1810         N = N + 1
 1811         IF (N.GT.NBI) THEN
 1812           WRITE(NDSE,*) 'Error: boundary node index > NBI ... nest.ww3 fi>
 1813           STOP
 1814         ENDIF
 1815         IX1(N) = IX
 
 
                                                         w3triamd.F90  page  34
 
 
 1816         END IF
 1817       END DO
 1818 !
 1819 !2. Matches the model grid points (where MAPSTA = 2) with the points in n>
 1820 !   For this, we use the nearest point in the nest file.
 1821 !
 1822     DO I = 1, NBI
 1823 !FA: This will not work with FLAGLL=.F.  (XY grid)
 1824       DIST0 = 360**2
 1825       IS=1
 1826       DO J = 1, N
 1827         DIST=(XBPI(I)-XYB(IX1(J),1))**2+(YBPI(I)-XYB(IX1(J),2))**2
 1828         IF (DIST.LT.DIST0) THEN
 1829           IS = MAPFS(1,IX1(J))
 1830           DIST0=DIST
 1831           JMEMO=J
 1832           END IF
 1833         END DO
 1834       DIST0=SQRT(DIST0)
 1835       IF (DIST0.LE.DISTMIN) THEN
 1836         ISBPI(I)=IS
 1837      ELSE
 1838         FLOK=.TRUE.
 1839         END IF
 1840       END DO
 1841     IF ( N .NE. NBI) THEN
 1842       WRITE(NDSE ,900) N, NBI
 1843       DO J=1,N
 1844         WRITE(6,*) 'THIS POINT HAS MAPSTA=2:',ISBPI(J)
 1845         END DO
 1846       ISBPI(N+1:NBI)=ISBPI(1)
 1847       END IF
 1848             
 1849 900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOBC : '/                &
 1850              '     NUMBER OF MAPSTA=2 DIFFERS FROM NUMBER IN nest.ww3    >
 1851              '     CHECK nest.ww3 AND ww3_grid.inp ',2I8/)
 1852 END SUBROUTINE
 1853               
 1854               
 1855 !/ ------------------------------------------------------------------- /
 1856    SUBROUTINE SET_IOBP (MASK, STATUS)
 1857 !/
 1858 !/                  +-----------------------------------+
 1859 !/                  | WAVEWATCH III           NOAA/NCEP |
 1860 !/                  |                                   |
 1861 !/                  | Aron Roland (BGS IT&E GmbH)       |
 1862 !/                  | Mathieu Dutour-Sikiric (IRB)      |
 1863 !/                  |                                   |
 1864 !/                  |                        FORTRAN 90 |
 1865 !/                  | Last update :        01-June-2018 |
 1866 !/                  +-----------------------------------+
 1867 !/
 1868 !/    01-June-2018 : Origination.                        ( version 6.04 )
 1869 !/
 1870 !  1. Purpose : setup boundary pointer
 
 
                                                         w3triamd.F90  page  35
 
 
 1871 !  2. Method :
 1872 !  3. Parameters :
 1873 !
 1874 !     Parameter list
 1875 !     ----------------------------------------------------------------
 1876 !     ----------------------------------------------------------------
 1877 !
 1878 !  4. Subroutines used :
 1879 !
 1880 !      Name      Type  Module   Description
 1881 !     ----------------------------------------------------------------
 1882 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1883 !     ----------------------------------------------------------------
 1884 !
 1885 !  5. Called by :
 1886 !
 1887 !      Name      Type  Module   Description
 1888 !     ----------------------------------------------------------------
 1889 !     ----------------------------------------------------------------
 1890 !
 1891 !  6. Error messages :
 1892 !  7. Remarks
 1893 !  8. Structure :
 1894 !  9. Switches :
 1895 !
 1896 !     !/S  Enable subroutine tracing.
 1897 !
 1898 ! 10. Source code :
 1899 !
 1900 !/ ------------------------------------------------------------------- /
 1901 !
 1902 !/
 1903 !
 1904       USE CONSTANTS
 1905 !
 1906       USE W3GDATMD, ONLY: NX, NTRI, TRIGP
 1907       USE W3ODATMD, ONLY: IAPROC
 1908                                 
 1909                                 
 1910       IMPLICIT NONE
 1911                    
 1912 !/
 1913 !/ ------------------------------------------------------------------- /
 1914 !/ Parameter list
 1915 !/
 1916       INTEGER, INTENT(IN)   :: MASK(NX)
 1917       INTEGER, INTENT(OUT)  :: STATUS(NX)
 1918 !
 1919       INTEGER :: COLLECTED(NX), NEXTVERT(NX), PREVVERT(NX)
 1920       INTEGER          :: ISFINISHED !, INEXT, IPREV
 1921       INTEGER :: INEXT(3), IPREV(3)
 1922       INTEGER          :: ZNEXT, IP, I, IE, IPNEXT, IPPREV, COUNT
 1923       integer nb0, nb1, nbM1
 1924       STATUS = -1
 1925       INEXT=(/ 2, 3, 1 /) !IPREV=1+MOD(I+1,3)
 
 
                                                         w3triamd.F90  page  36
 
 
 1926       IPREV=(/ 3, 1, 2 /) !INEXT=1+MOD(I,3)
 1927       DO IE=1,NTRI
 1928 ! If one of the points of the triangle is masked out (land) then do as if>
 1929 !        IF ((MASK(TRIGP(IE,1)).GT.0).AND.(MASK(TRIGP(IE,2)).GT.0).AND.(M>
 1930         DO I=1,3
 1931           IP=TRIGP(IE,I)
 1932           CALL TRIANG_INDEXES(I, IPNEXT, IPPREV)
 1933           !IPNEXT=TRIGP(IE,INEXT(I))
 1934           !IPPREV=TRIGP(IE,IPREV(I))
 1935           IF (STATUS(IP).EQ.-1) THEN
 1936             STATUS(IP)=1
 1937             PREVVERT(IP)=IPPREV
 1938             NEXTVERT(IP)=IPNEXT
 1939             END IF
 1940           END DO
 1941 !        ENDIF
 1942         END DO
 1943       STATUS(:)=-1
 1944  !
 1945       COUNT = 0
 1946       DO
 1947         COUNT = COUNT + 1
 1948         COLLECTED(:)=0
 1949         DO IE=1,NTRI
 1950 !        IF ((MASK(TRIGP(IE,1)).GT.0).AND.(MASK(TRIGP(IE,2)).GT.0).AND.(M>
 1951           DO I=1,3
 1952             IP=TRIGP(IE,I)
 1953             CALL TRIANG_INDEXES(I, IPNEXT, IPPREV)
 1954             !IPNEXT=TRIGP(IE,INEXT(I))
 1955             !IPPREV=TRIGP(IE,IPREV(I))
 1956             IF (STATUS(IP).EQ.-1) THEN
 1957               ZNEXT=NEXTVERT(IP)
 1958               IF (ZNEXT.EQ.IPPREV) THEN
 1959                 COLLECTED(IP)=1
 1960                 NEXTVERT(IP)=IPNEXT
 1961                 IF (NEXTVERT(IP).EQ.PREVVERT(IP)) THEN
 1962                   STATUS(IP)=1
 1963                   END IF
 1964                 END IF
 1965               END IF
 1966             END DO
 1967 !            END IF ! end of test on MASK
 1968           END DO
 1969 !
 1970 ! Checks that all nodes have been treated ...
 1971 !
 1972        ISFINISHED=1
 1973        DO IP=1,NX
 1974           IF (MASK(IP).LE.0) THEN
 1975              STATUS(IP)=0
 1976           ELSE
 1977             IF ((COLLECTED(IP).EQ.0).AND.(STATUS(IP).EQ.-1)) THEN
 1978               STATUS(IP)=0
 1979               END IF
 1980             IF (STATUS(IP).eq.-1) THEN
 
 
                                                         w3triamd.F90  page  37
 
 
 1981               ISFINISHED=0
 1982               END IF
 1983             ENDIF
 1984         END DO
 1985         IF (ISFINISHED.EQ.1) THEN
 1986           EXIT
 1987         END IF
 1988       END DO
 1989             
 1990       STATUS = 1
 1991       CALL GET_BOUNDARY(NX, NTRI, TRIGP, STATUS, PREVVERT, NEXTVERT)
 1992 !      DO IP= 1, NX
 1993 !        WRITE(12000,*) IP, STATUS(IP)
 1994 !      ENDDO
 1995             
 1996 !#ifdef MPI_PARALL_GRID
 1997 !      CALL exchange_p2di(STATUS)
 1998 !#endif
 1999       END SUBROUTINE SET_IOBP
 2000 !/ ------------------------------------------------------------------- /
 2001                                                                         
 2002       SUBROUTINE GET_BOUNDARY(MNP, MNE, TRIGP, IOBP, NEIGHBOR_PREV,      >
 2003      &   NEIGHBOR_NEXT)
 2004 !/
 2005 !/                  +-----------------------------------+
 2006 !/                  | WAVEWATCH III           NOAA/NCEP |
 2007 !/                  |                                   |
 2008 !/                  | Aron Roland (BGS IT&E GmbH)       |
 2009 !/                  | Mathieu Dutour-Sikiric (IRB)      |
 2010 !/                  |                                   |
 2011 !/                  |                        FORTRAN 90 |
 2012 !/                  | Last update :        01-June-2018 |
 2013 !/                  +-----------------------------------+
 2014 !/
 2015 !/    01-June-2018 : Origination.                        ( version 6.04 )
 2016 !/
 2017 !  1. Purpose : find boundary points
 2018 !  2. Method :
 2019 !  3. Parameters :
 2020 !
 2021 !     Parameter list
 2022 !     ----------------------------------------------------------------
 2023 !     ----------------------------------------------------------------
 2024 !
 2025 !  4. Subroutines used :
 2026 !
 2027 !      Name      Type  Module   Description
 2028 !     ----------------------------------------------------------------
 2029 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 2030 !     ----------------------------------------------------------------
 2031 !
 2032 !  5. Called by :
 2033 !
 2034 !      Name      Type  Module   Description
 2035 !     ----------------------------------------------------------------
 
 
                                                         w3triamd.F90  page  38
 
 
 2036 !     ----------------------------------------------------------------
 2037 !
 2038 !  6. Error messages :
 2039 !  7. Remarks
 2040 !  8. Structure :
 2041 !  9. Switches :
 2042 !
 2043 !     !/S  Enable subroutine tracing.
 2044 !
 2045 ! 10. Source code :
 2046 !
 2047 !/ ------------------------------------------------------------------- /
 2048 !
 2049         USE W3SERVMD, ONLY: EXTCDE
 2050         IMPLICIT NONE
 2051 !/
 2052 !/ ------------------------------------------------------------------- /
 2053 !/ Parameter list
 2054 !/
 2055 !/ ------------------------------------------------------------------- /
 2056 !/ Local PARAMETERs
 2057 !/
 2058   
 2059         INTEGER, INTENT(IN)             :: MNP, MNE, TRIGP(MNE,3)
 2060         INTEGER, INTENT(INOUT)          :: IOBP(MNP)
 2061         INTEGER, INTENT(INOUT)          :: NEIGHBOR_PREV(MNP)
 2062         INTEGER, INTENT(INOUT)          :: NEIGHBOR_NEXT(MNP)
 2063                                                              
 2064         INTEGER, POINTER :: STATUS(:)
 2065         INTEGER, POINTER :: COLLECTED(:)
 2066         INTEGER, POINTER :: NEXTVERT(:)
 2067         INTEGER, POINTER :: PREVVERT(:)
 2068                                        
 2069         INTEGER :: IE, I, IP, IP2, IP3
 2070         INTEGER :: ISFINISHED, INEXT, IPREV, ISTAT
 2071         INTEGER :: IPNEXT, IPPREV, ZNEXT, ZPREV
 2072         ALLOCATE(STATUS(MNP), STAT=ISTAT)
 2073         CHECK_ALLOC_STATUS ( ISTAT )
 2074         ALLOCATE(COLLECTED(MNP), STAT=ISTAT)
 2075         CHECK_ALLOC_STATUS ( ISTAT )
 2076         ALLOCATE(PREVVERT(MNP), STAT=ISTAT)
 2077         CHECK_ALLOC_STATUS ( ISTAT )
 2078         ALLOCATE(NEXTVERT(MNP), STAT=ISTAT)
 2079         CHECK_ALLOC_STATUS ( ISTAT )
 2080         NEIGHBOR_NEXT = 0
 2081         NEIGHBOR_PREV = 0
 2082 !  Now computing the next items
 2083         STATUS = 0
 2084         NEXTVERT = 0
 2085         PREVVERT = 0
 2086                     
 2087         DO IE=1,MNE
 2088           DO I=1,3
 2089             CALL TRIANG_INDEXES(I, INEXT, IPREV)
 2090             IP=TRIGP(IE,I)
 
 
                                                         w3triamd.F90  page  39
 
 
 2091             IPNEXT=TRIGP(IE,INEXT)
 2092             IPPREV=TRIGP(IE,IPREV)
 2093             IF (STATUS(IP).EQ.0) THEN
 2094               STATUS(IP)=1
 2095               PREVVERT(IP)=IPPREV
 2096               NEXTVERT(IP)=IPNEXT
 2097             END IF
 2098           END DO
 2099         END DO
 2100         STATUS(:)=0
 2101         DO
 2102           COLLECTED(:)=0
 2103           DO IE=1,MNE
 2104             DO I=1,3
 2105               CALL TRIANG_INDEXES(I, INEXT, IPREV)
 2106               IP=TRIGP(IE,I)
 2107               IPNEXT=TRIGP(IE,INEXT)
 2108               IPPREV=TRIGP(IE,IPREV)
 2109               IF (STATUS(IP).EQ.0) THEN
 2110                 ZNEXT=NEXTVERT(IP)
 2111                 IF (ZNEXT.EQ.IPPREV) THEN
 2112                   COLLECTED(IP)=1
 2113                   NEXTVERT(IP)=IPNEXT
 2114                   IF (NEXTVERT(IP).EQ.PREVVERT(IP)) THEN
 2115                     STATUS(IP)=1
 2116                   END IF
 2117                 END IF
 2118               END IF
 2119             END DO
 2120           END DO
 2121                 
 2122           ISFINISHED=1
 2123           DO IP=1,MNP
 2124             IF ((COLLECTED(IP).EQ.0).AND.(STATUS(IP).EQ.0)) THEN
 2125               STATUS(IP)=-1
 2126               NEIGHBOR_NEXT(IP)=NEXTVERT(IP)
 2127             END IF
 2128             IF (STATUS(IP).EQ.0) THEN
 2129               ISFINISHED=0
 2130             END IF
 2131           END DO
 2132           IF (ISFINISHED.EQ.1) THEN
 2133             EXIT
 2134           END IF
 2135         END DO
 2136               
 2137 !  Now computing the prev items
 2138         STATUS = 0
 2139         NEXTVERT = 0
 2140         PREVVERT = 0
 2141         DO IE=1,MNE
 2142           DO I=1,3
 2143             CALL TRIANG_INDEXES(I, INEXT, IPREV)
 2144             IP=TRIGP(IE,I)
 2145             IPNEXT=TRIGP(IE,INEXT)
 
 
                                                         w3triamd.F90  page  40
 
 
 2146             IPPREV=TRIGP(IE,IPREV)
 2147             IF (STATUS(IP).EQ.0) THEN
 2148               STATUS(IP)=1
 2149               PREVVERT(IP)=IPPREV
 2150               NEXTVERT(IP)=IPNEXT
 2151             END IF
 2152           END DO
 2153         END DO
 2154         STATUS(:)=0
 2155         DO
 2156           COLLECTED(:)=0
 2157           DO IE=1,MNE
 2158             DO I=1,3
 2159               CALL TRIANG_INDEXES(I, INEXT, IPREV)
 2160               IP=TRIGP(IE,I)
 2161               IPNEXT=TRIGP(IE,INEXT)
 2162               IPPREV=TRIGP(IE,IPREV)
 2163               IF (STATUS(IP).EQ.0) THEN
 2164                 ZPREV=PREVVERT(IP)
 2165                 IF (ZPREV.EQ.IPNEXT) THEN
 2166                   COLLECTED(IP)=1
 2167                   PREVVERT(IP)=IPPREV
 2168                   IF (PREVVERT(IP).EQ.NEXTVERT(IP)) THEN
 2169                     STATUS(IP)=1
 2170                   END IF
 2171                 END IF
 2172               END IF
 2173             END DO
 2174           END DO
 2175                 
 2176           ISFINISHED=1
 2177           DO IP=1,MNP
 2178             IF ((COLLECTED(IP).EQ.0).AND.(STATUS(IP).EQ.0)) THEN
 2179               STATUS(IP)=-1
 2180               NEIGHBOR_PREV(IP)=PREVVERT(IP)     ! new code
 2181             END IF
 2182             IF (STATUS(IP).EQ.0) THEN
 2183               ISFINISHED=0
 2184             END IF
 2185           END DO
 2186           IF (ISFINISHED.EQ.1) THEN
 2187             EXIT
 2188           END IF
 2189         END DO
 2190 !  Now making checks
 2191         DO IP=1,MNP
 2192           IP2=NEIGHBOR_NEXT(IP)
 2193           IF (IP2.GT.0) THEN
 2194             IP3=NEIGHBOR_PREV(IP2)
 2195             IF (ABS(IP3 - IP).GT.0) THEN
 2196               WRITE(*,*) 'IP=', IP, ' IP2=', IP2, ' IP3=', IP3
 2197               WRITE(*,*) 'We have a dramatic inconsistency'
 2198               STOP
 2199             END IF
 2200           END IF
 
 
                                                         w3triamd.F90  page  41
 
 
 2201         END DO
 2202 !   Now assigning the boundary IOBP array
 2203         DO IP=1,MNP
 2204           IF (STATUS(IP).EQ.-1 .AND. IOBP(IP) .EQ. 1) THEN
 2205             IOBP(IP)=0
 2206           END IF
 2207         END DO
 2208               
 2209         DEALLOCATE(STATUS, STAT=ISTAT)
 2210         CHECK_DEALLOC_STATUS ( ISTAT )
 2211         DEALLOCATE(COLLECTED, STAT=ISTAT)
 2212         CHECK_DEALLOC_STATUS ( ISTAT )
 2213         DEALLOCATE(NEXTVERT, STAT=ISTAT)
 2214         CHECK_DEALLOC_STATUS ( ISTAT )
 2215         DEALLOCATE(PREVVERT, STAT=ISTAT)
 2216         CHECK_DEALLOC_STATUS ( ISTAT )
 2217                                       
 2218       END SUBROUTINE
 2219                     
 2220 !/ ------------------------------------------------------------------- /
 2221                                                                         
 2222       SUBROUTINE TRIANG_INDEXES(I, INEXT, IPREV)
 2223 !/
 2224 !/                  +-----------------------------------+
 2225 !/                  | WAVEWATCH III           NOAA/NCEP |
 2226 !/                  |                                   |
 2227 !/                  | Aron Roland (BGS IT&E GmbH)       |
 2228 !/                  | Mathieu Dutour-Sikiric (IRB)      |
 2229 !/                  |                                   |
 2230 !/                  |                        FORTRAN 90 |
 2231 !/                  | Last update :        01-June-2018 |
 2232 !/                  +-----------------------------------+
 2233 !/
 2234 !/    01-June-2018 : Origination.                        ( version 6.04 )
 2235 !/
 2236 !  1. Purpose : set indices of the triangle
 2237 !  2. Method :
 2238 !  3. Parameters :
 2239 !
 2240 !     Parameter list
 2241 !     ----------------------------------------------------------------
 2242 !     ----------------------------------------------------------------
 2243 !
 2244 !  4. Subroutines used :
 2245 !
 2246 !      Name      Type  Module   Description
 2247 !     ----------------------------------------------------------------
 2248 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 2249 !     ----------------------------------------------------------------
 2250 !
 2251 !  5. Called by :
 2252 !
 2253 !      Name      Type  Module   Description
 2254 !     ----------------------------------------------------------------
 2255 !     ----------------------------------------------------------------
 
 
                                                         w3triamd.F90  page  42
 
 
 2256 !
 2257 !  6. Error messages :
 2258 !  7. Remarks
 2259 !  8. Structure :
 2260 !  9. Switches :
 2261 !
 2262 !     !/S  Enable subroutine tracing.
 2263 !
 2264 ! 10. Source code :
 2265 !
 2266 !/ ------------------------------------------------------------------- /
 2267          IMPLICIT NONE
 2268 !
 2269 !/ ------------------------------------------------------------------- /
 2270 !/ Parameter list
 2271 !/
 2272 !/ ------------------------------------------------------------------- /
 2273 !/ Local PARAMETERs
 2274 !/
 2275 !/
 2276 !/ ------------------------------------------------------------------- /
 2277 !/
 2278         INTEGER, INTENT(IN)  :: I
 2279         INTEGER, INTENT(OUT) :: INEXT, IPREV
 2280         IF (I.EQ.1) THEN
 2281           INEXT=3
 2282         ELSE
 2283           INEXT=I-1
 2284         END IF
 2285         IF (I.EQ.3) THEN
 2286           IPREV=1
 2287         ELSE
 2288           IPREV=I+1
 2289         END IF
 2290       END SUBROUTINE
 2291                     
 2292 !/ ------------------------------------------------------------------- /
 2293                                                                         
 2294       SUBROUTINE GET_INTERFACE()
 2295 !/
 2296 !/                  +-----------------------------------+
 2297 !/                  | WAVEWATCH III           NOAA/NCEP |
 2298 !/                  |                                   |
 2299 !/                  | Aron Roland (BGS IT&E GmbH)       |
 2300 !/                  | Mathieu Dutour-Sikiric (IRB)      |
 2301 !/                  |                                   |
 2302 !/                  |                        FORTRAN 90 |
 2303 !/                  | Last update :        01-June-2018 |
 2304 !/                  +-----------------------------------+
 2305 !/
 2306 !/    01-June-2018 : Origination.                        ( version 6.04 )
 2307 !/
 2308 !  1. Purpose : get interface nodes of the wetting and drying part
 2309 !  2. Method :
 2310 !  3. Parameters :
 
 
                                                         w3triamd.F90  page  43
 
 
 2311 !
 2312 !     Parameter list
 2313 !     ----------------------------------------------------------------
 2314 !     ----------------------------------------------------------------
 2315 !
 2316 !  4. Subroutines used :
 2317 !
 2318 !      Name      Type  Module   Description
 2319 !     ----------------------------------------------------------------
 2320 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 2321 !     ----------------------------------------------------------------
 2322 !
 2323 !  5. Called by :
 2324 !
 2325 !      Name      Type  Module   Description
 2326 !     ----------------------------------------------------------------
 2327 !     ----------------------------------------------------------------
 2328 !
 2329 !  6. Error messages :
 2330 !  7. Remarks
 2331 !  8. Structure :
 2332 !  9. Switches :
 2333 !
 2334 !     !/S  Enable subroutine tracing.
 2335 !
 2336 ! 10. Source code :
 2337 !
 2338 !/ ------------------------------------------------------------------- /
 2339 !
 2340         USE CONSTANTS, ONLY : LPDLIB
 2341         USE W3GDATMD, ONLY : NX, IOBP, CCON, NSEAL, IOBDP, IE_CELL, IOBDP>
 2342         IMPLICIT NONE
 2343 !/
 2344 !/ ------------------------------------------------------------------- /
 2345 !/ Parameter list
 2346 !/
 2347 !/ ------------------------------------------------------------------- /
 2348 !/ Local PARAMETERs
 2349 !/
 2350 !/
 2351 !/ ------------------------------------------------------------------- /
 2352 !/
 2353         INTEGER :: I, J, IP, IE
 2354       J = 0
 2355       DO IP = 1, NSEAL
 2356         IF (IOBP(IP) .NE. 1 .OR. IOBDP(IP) .EQ. 0) CYCLE
 2357         DO I = 1, CCON(IP)
 2358           J = J + 1
 2359           IE = IE_CELL(J)
 2360           IF (ANY(IOBDP(TRIGP(IE,:)) .EQ. 0)) THEN
 2361             IOBDP(IP) = -1 ! Set this node as a wet node adjacent to a dr>
 2362             EXIT
 2363           ENDIF
 2364         ENDDO
 2365       ENDDO
 
 
                                                         w3triamd.F90  page  44
 
 
 2366            
 2367    END SUBROUTINE
 2368 !/ ------------------------------------------------------------------- /
 2369    SUBROUTINE SETUGIOBP ( )
 2370 !/
 2371 !/                  +-----------------------------------+
 2372 !/                  | WAVEWATCH III           NOAA/NCEP |
 2373 !/                  |        Fabrice Ardhuin            |
 2374 !/                  |        Aron Roland                |
 2375 !/                  |                        FORTRAN 90 |
 2376 !/                  | Last update :         17-Apr-2016 |
 2377 !/                  +-----------------------------------+
 2378 !/
 2379 !/    23-Aug-2011 : Origination.                        ( version 4.04 )
 2380 !/    17-Apr-2016 : Uses optimized boundary detection   ( version 5.10 )
 2381 !/
 2382 !  1. Purpose :
 2383 !
 2384 !     Redefines the values of the boundary points and angle pointers
 2385 !     based on the MAPSTA array
 2386 !
 2387 !  2. Method :
 2388 !
 2389 !     Adapted boundary detection from A. Roland and M. Dutour (WWM code)
 2390 !
 2391 !  3. Parameters :
 2392 !
 2393 !     Parameter list
 2394 !     ----------------------------------------------------------------
 2395 !     ----------------------------------------------------------------
 2396 !
 2397 !     Local variables.
 2398 !     ----------------------------------------------------------------
 2399 !     ----------------------------------------------------------------
 2400 !
 2401 !  4. Subroutines used :
 2402 !
 2403  
 2404 !  5. Called by :
 2405 !
 2406 !      Name      Type  Module   Description
 2407 !     ----------------------------------------------------------------
 2408 !      WW3_GRID  Prog. WW3_GRID Grid preprocessor
 2409 !      W3ULEV    Subr. W3UPDTMD Water level update
 2410 !     ----------------------------------------------------------------
 2411 !
 2412 !  6. Error messages :
 2413 !
 2414 !       None.
 2415 !
 2416 !  7. Remarks :
 2417 !
 2418 !  8. Structure :
 2419 !
 2420 !  9. Switches :
 
 
                                                         w3triamd.F90  page  45
 
 
 2421 !
 2422 !       !/S     Enable subroutine tracing.
 2423 !
 2424 ! 10. Source code :
 2425 !/ ------------------------------------------------------------------- /
 2426 !/
 2427 !
 2428       USE CONSTANTS
 2429 !
 2430       USE W3GDATMD, ONLY: NX, NY, NSEA, MAPFS,                        &
 2431                           NK, NTH, DTH, XFR, MAPSTA, COUNTRI,         &
 2432                           ECOS, ESIN, IEN, NTRI, TRIGP,               &
 2433                           IOBP,IOBPD, IOBPA,                          &
 2434                     REFPARS, REFLC, REFLD,                      &
 2435                           ANGLE0, ANGLE
 2436                                        
 2437       USE W3ODATMD, ONLY: TBPI0, TBPIN, FLBPI
 2438       USE W3ADATMD, ONLY: CG, CX, CY, ATRNX, ATRNY, ITIME, CFLXYMAX
 2439       USE W3IDATMD, ONLY: FLCUR
 2440       USE W3ODATMD, only : IAPROC
 2441                                  
 2442       IMPLICIT NONE
 2443 !/ ------------------------------------------------------------------- /
 2444 !/ Parameter list
 2445 !/
 2446 !/
 2447 !/ ------------------------------------------------------------------- /
 2448 !/ Local parameters
 2449 !/
 2450       INTEGER                 :: ITH, IX, I, J, IP, IE, NDIRSUM
 2451       REAL (KIND = 8)         :: COSSUM, SINSUM
 2452       REAL (KIND = 8)         :: DIRMIN, DIRMAX, SHIFT, TEMPO, DIRCOAST
 2453       REAL (KIND = 8)         :: X1, X2, Y1, Y2, DXP1, DXP2, DXP3
 2454       REAL (KIND = 8)         :: DYP1, DYP2, DYP3, eDet1, eDet2, EVX, EVY
 2455       REAL(KIND=8), PARAMETER :: THR    = TINY(1.)
 2456       INTEGER                 :: I1, I2, I3
 2457       INTEGER                 :: ITMP(NX), NEXTVERT(NX), PREVVERT(NX)
 2458 !/ ------------------------------------------------------------------- /
 2459 !
 2460 ! 1.  Preparations --------------------------------------------------- *
 2461 ! 1.a Set constants
 2462 !
 2463  
 2464 !
 2465 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 2466 ! 2.  Searches for boundary points
 2467 !
 2468       ITMP = MAPSTA(1,:)
 2469       CALL SET_IOBP(ITMP, IOBP)
 2470 !
 2471 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 2472 ! 3. Defines directions pointing into land or sea
 2473 !
 2474       IOBPD(:,:) = 0
 2475       IOBPA(:) = 0
 
 
                                                         w3triamd.F90  page  46
 
 
 2476 !
 2477       DO IP=1,NX
 2478         IF ((MAPSTA(1,IP).EQ.2).AND.(IOBP(IP).EQ.0)) IOBPA(IP)=1
 2479 !WRITE(600,*) IP,ITMP(IP),IOBP(IP),IOBPA(IP)
 2480       END DO
 2481             
 2482       DO IE=1,NTRI
 2483         I1   =   TRIGP(IE,1)
 2484         I2   =   TRIGP(IE,2)
 2485         I3   =   TRIGP(IE,3)
 2486         DXP1 =   IEN(IE,6)
 2487         DYP1 = - IEN(IE,5)
 2488         DXP2 =   IEN(IE,2)
 2489         DYP2 = - IEN(IE,1)
 2490         DXP3 =   IEN(IE,4)
 2491         DYP3 = - IEN(IE,3)
 2492         DO ITH=1,NTH
 2493           EVX=ECOS(ITH)
 2494           EVY=ESIN(ITH)
 2495           DO I=1,3
 2496             IF (I.eq.1) THEN
 2497               x1=   DXP1
 2498               y1=   DYP1
 2499               x2= - DXP3
 2500               y2= - DYP3
 2501               IP=   I1
 2502             END IF
 2503             IF (I.eq.2) THEN
 2504               x1 =   DXP2
 2505               y1 =   DYP2
 2506               x2 = - DXP1
 2507               y2 = - DYP1
 2508               IP =   I2
 2509             END IF
 2510             IF (I.eq.3) THEN
 2511               x1 =   DXP3
 2512               y1 =   DYP3
 2513               x2 = - DXP2
 2514               y2 = - DYP2
 2515               IP =   I3
 2516             END IF
 2517             IF (IOBP(IP) .eq. 0) THEN ! physical boundary
 2518               eDet1 = THR-x1*EVY+y1*EVX
 2519               eDet2 = THR+x2*EVY-y2*EVX
 2520               IF ((eDet1.gt.0.).and.(eDet2.gt.0.)) THEN
 2521 ! this is the case of waves going towards the boundary ...
 2522                 IOBPD(ITH,IP)=1
 2523               ENDIF
 2524             ELSE ! water ...
 2525               IOBPD(ITH,IP)=1
 2526             END IF
 2527           END DO
 2528         END DO
 2529       END DO
 2530       DO IP = 1, NX
 
 
                                                         w3triamd.F90  page  47
 
 
 2531         IF ( IOBPA(IP) .eq. 1 .OR. IOBP(IP) .eq. 3 .OR. IOBP(IP) .eq. 4) >
 2532       END DO
 2533 !2do: recode for mpi
 2534 !        IF (LBCWA .OR. LBCSP) THEN
 2535 !          IF (.NOT. ANY(IOBP .EQ. 2)) THEN
 2536 !            CALL WWM_ABORT('YOU IMPOSED BOUNDARY CONDITIONS BUT IN THE B>
 2537 !          ENDIF
 2538 !        ENDIF
 2539 !#ifdef MPI_PARALL_GRID
 2540 !      CALL exchange_p2di(IOBWB)
 2541 !      DO ID = 1, MDC
 2542 !        iwild = IOBPD(ID,:)
 2543 !        CALL exchange_p2di(iwild)
 2544 !        IOBPD(ID,:) = iwild
 2545 !      ENDDO
 2546 !#endif
 2547 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 2548 ! 3. Updates the reflection direction and sharp / flat shoreline angle
 2549                                                                       
 2550  !
 2551  ! Finds the shoreline direction from IOBPD
 2552  !
 2553           REFLC(1,:)= 0.
 2554           REFLD(:,:)= 1
 2555           DO IP=1,NX
 2556             IF (IOBP(IP).EQ.0.AND.MAPSTA(1,IP).EQ.1) THEN
 2557               COSSUM=0.
 2558               SINSUM=0.
 2559               NDIRSUM=0.
 2560               DO ITH=1,NTH
 2561                 COSSUM=COSSUM+IOBPD(ITH,IP)*ECOS(ITH)
 2562                 SINSUM=SINSUM+IOBPD(ITH,IP)*ESIN(ITH)
 2563                 NDIRSUM=NDIRSUM+IOBPD(ITH,IP)
 2564                 END DO
 2565               DIRCOAST=ATAN2(SINSUM, COSSUM)
 2566               REFLD(1,MAPFS(1,IP)) = 1+MOD(NTH+NINT(DIRCOAST/DTH),NTH)
 2567               REFLD(2,MAPFS(1,IP)) = 4-MAX(2,NINT(4.*REAL(NDIRSUM)/REAL(N>
 2568               REFLC(1,MAPFS(1,IP))= REFPARS(1)
 2569               END IF
 2570             END DO
 2571                   
 2572                   
 2573 !DO IX=1,NX
 2574 !DO ITH=1,NTH
 2575 !  WRITE(500+IAPROC,*) IX,ITH,IOBP(IX),IOBPA(IX),IOBPD(ITH,IX) !,REFLD(1:>
 2576 !ENDDO
 2577 !ENDDO
 2578       
 2579 !
 2580 ! Recomputes the angles used in the gradients estimation
 2581 !
 2582       RETURN
 2583       END SUBROUTINE SETUGIOBP
 2584 !/ ------------------------------------------------------------------- /
 2585                                                                         
 
 
                                                         w3triamd.F90  page  48
 
 
 2586       SUBROUTINE FIX_PERIODCITY(I1,I2,I3,XYB,PT)
 2587 !/
 2588 !/                  +-----------------------------------+
 2589 !/                  | WAVEWATCH III           NOAA/NCEP |
 2590 !/                  |        Steven Brus                |
 2591 !/                  |        Ali Abdolali               |
 2592 !/                  |                        FORTRAN 90 |
 2593 !/                  | Last update :         21-May-2020 |
 2594 !/                  +-----------------------------------+
 2595 !/
 2596 !/    21-May-2020 : Origination.                        ( version 6.07 )
 2597 !/
 2598 !/
 2599 !  1. Purpose :
 2600 !
 2601 !     Adjust element longitude coordinates for elements straddling the
 2602 !     dateline with distance of ~360 degrees
 2603 !
 2604 !  2. Method :
 2605 !
 2606 !     Detect if element has nodes on both sides of dateline and adjust
 2607 !     coordinates so that all nodes have the same sign
 2608 !
 2609 !  3. Parameters :
 2610 !
 2611 !     Parameter list
 2612 !     ----------------------------------------------------------------
 2613       IMPLICIT NONE
 2614       INTEGER, INTENT(IN) :: I1, I2, I3
 2615       DOUBLE PRECISION, INTENT(IN) :: XYB(:,:)
 2616       REAL*8, INTENT(OUT) :: PT(3,2)
 2617 !     ----------------------------------------------------------------
 2618 !
 2619 !     Local variables.
 2620 !     ----------------------------------------------------------------
 2621       INTEGER :: I
 2622       INTEGER :: R1GT180, R2GT180, R3GT180
 2623 !     ----------------------------------------------------------------
 2624 !
 2625 !  4. Subroutines used :
 2626 !
 2627  
 2628 !  5. Called by :
 2629 !
 2630 !      Name         Type  Module   Description
 2631 !     ----------------------------------------------------------------
 2632 !      SPATIAL_GRID Subr. W3TRIAM  Triangle area calculation
 2633 !      NVECTRI      Subr. W3TRIAM  Edge length, angle, normal calcuation
 2634 !      IS_IN_UNGRID Subr. W3TRIAM  Point in element calculation
 2635 !     ----------------------------------------------------------------
 2636 !
 2637 !  6. Error messages :
 2638 !
 2639 !       None.
 2640 !
 
 
                                                         w3triamd.F90  page  49
 
 
 2641 !  7. Remarks :
 2642 !
 2643 !  8. Structure :
 2644 !
 2645 !  9. Switches :
 2646 !
 2647 ! 10. Source code :
 2648 !/ ------------------------------------------------------------------- /
 2649                                                                         
 2650       PT(1,1) = XYB(I1,1)
 2651       PT(1,2) = XYB(I1,2)
 2652       PT(2,1) = XYB(I2,1)
 2653       PT(2,2) = XYB(I2,2)
 2654       PT(3,1) = XYB(I3,1)
 2655       PT(3,2) = XYB(I3,2)
 2656                          
 2657                          
 2658        R1GT180 = MERGE(1, 0, ABS(PT(3,1)-PT(2,1)).GT.180)
 2659        R2GT180 = MERGE(1, 0, ABS(PT(1,1)-PT(3,1)).GT.180)
 2660        R3GT180 = MERGE(1, 0, ABS(PT(2,1)-PT(1,1)).GT.180)
 2661  ! if R1GT180+R2GT180+R3GT180 .eq. 0 the element does not cross the datel>
 2662  ! if R1GT180+R2GT180+R3GT180 .eq. 1 the element contains the pole
 2663  ! if R1GT180+R2GT180+R3GT180 .eq. 2 the element crosses the dateline
 2664                                                                      
 2665                                                                      
 2666       IF ( R1GT180 + R2GT180 == 2 ) THEN
 2667              PT(3,1)=PT(3,1)-SIGN(360.0d0,(PT(3,1)-PT(2,1)))
 2668       ELSE IF ( R2GT180 + R3GT180 == 2 ) THEN
 2669              PT(1,1)=PT(1,1)-SIGN(360.0d0,(PT(1,1)-PT(2,1)))
 2670       ELSE IF ( R1GT180 + R3GT180 == 2 ) THEN
 2671              PT(2,1)=PT(2,1)-SIGN(360.0d0,(PT(2,1)-PT(3,1)))
 2672       ENDIF
 2673            
 2674       RETURN
 2675       END SUBROUTINE FIX_PERIODCITY
 2676 END MODULE W3TRIAMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
