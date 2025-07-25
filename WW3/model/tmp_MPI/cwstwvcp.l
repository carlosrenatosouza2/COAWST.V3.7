                                                         cwstwvcp.F90  page   1
 
 
    1                                                                          >
    2                                                                           
    3                                                                           
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
 
 
                                                         cwstwvcp.F90  page   2
 
 
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
 
 
                                                         cwstwvcp.F90  page   3
 
 
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
 
 
                                                         cwstwvcp.F90  page   4
 
 
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
 
 
                                                         cwstwvcp.F90  page   5
 
 
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
 
 
                                                         cwstwvcp.F90  page   6
 
 
  276                                                                           
  277                                                                           
  278                                                                           
  279                                                                           
  280                                                                           
  281                                                                           
  282                                                                           
  283                                                                           
  284                                                                           
  285       MODULE CWSTWVCP
  286                      
  287 !
  288 !svn $Id: waves_coupler.F 756 2008-09-14 20:18:28Z jcwarner $
  289 !==================================================== John C. Warner ===
  290 !  Copyright (c) 2002-2008 The ROMS/TOMS Group      Hernan G. Arango   !
  291 !   Licensed under a MIT/X style license                               !
  292 !   See License_ROMS.txt                                               !
  293 !=======================================================================
  294 !                                                                      !
  295 !  This module is used to communicate and exchange data between SWAN   !
  296 !  other coupled model(s) using the Model Coupling Toolkit (MCT).      !
  297 !                                                                      !
  298 !=======================================================================
  299 !
  300 !  Componenet model registry.
  301 !
  302       USE m_MCTWorld, ONLY : MCTWorld_init => init
  303       USE m_MCTWorld, ONLY : MCTWorld_clean => clean
  304 !
  305 !  Domain decompositin descriptor datatype and assocoiated methods.
  306 !
  307       USE m_GlobalSegMap, ONLY : GlobalSegMap
  308       USE m_GlobalSegMap, ONLY : GlobalSegMap_init => init
  309       USE m_GlobalSegMap, ONLY : GlobalSegMap_lsize => lsize
  310       USE m_GlobalSegMap, ONLY : GlobalSegMap_clean => clean
  311       USE m_GlobalSegMap, ONLY : GlobalSegMap_Ordpnts => OrderedPoints
  312 !
  313 !  Field storage data types and associated methods.
  314 !
  315       USE m_AttrVect, ONLY : AttrVect
  316       USE m_AttrVect, ONLY : AttrVect_init => init
  317       USE m_AttrVect, ONLY : AttrVect_zero => zero
  318       USE m_AttrVect, ONLY : AttrVect_clean => clean
  319       USE m_AttrVect, ONLY : AttrVect_indxR => indexRA
  320       USE m_AttrVect, ONLY : AttrVect_importRAttr => importRAttr
  321       USE m_AttrVect, ONLY : AttrVect_exportRAttr => exportRAttr
  322 !
  323 !  Intercomponent communitcations scheduler.
  324 !
  325       USE m_Router, ONLY : Router
  326       USE m_Router, ONLY : Router_init => init
  327       USE m_Router, ONLY : Router_clean => clean
  328 !
  329 !  Intercomponent transfer.
  330 !
 
 
                                                         cwstwvcp.F90  page   7
 
 
  331       USE m_Transfer, ONLY : MCT_isend => isend
  332       USE m_Transfer, ONLY : MCT_irecv => irecv
  333       USE m_Transfer, ONLY : MCT_waitr => waitrecv
  334       USE m_Transfer, ONLY : MCT_waits => waitsend
  335 !
  336 !  Sparse Matrix DataType and associated methods.
  337 !
  338       USE m_SparseMatrix, ONLY : SparseMatrix
  339       USE m_SparseMatrix, ONLY : SparseMatrix_init => init
  340       USE m_SparseMatrix, ONLY : SparseMatrix_importGRowInd =>          &
  341      &                           importGlobalRowIndices
  342       USE m_SparseMatrix, ONLY : SparseMatrix_importGColInd =>          &
  343      &                           importGlobalColumnIndices
  344       USE m_SparseMatrix, ONLY : SparseMatrix_importMatrixElts =>       &
  345      &                           importMatrixElements
  346       USE m_SparseMatrix, only : SparseMatrix_lsize => lsize
  347       USE m_SparseMatrix, only : SparseMatrix_clean => clean
  348       USE m_SparseMatrixPlus, ONLY : SparseMatrixPlus
  349       USE m_SparseMatrixPlus, ONLY : SparseMatrixPlus_init => init
  350       USE m_SparseMatrixPlus, ONLY : SparseMatrixPlus_clean => clean
  351 !
  352 !  Decompose matrix by row.
  353 !
  354       USE m_SparseMatrixPlus, ONLY : Xonly
  355 !
  356 !  Matrix-Vector multiply methods.
  357 !
  358       USE m_MatAttrVectMul, ONLY : MCT_MatVecMul => sMatAvMult
  359                                                               
  360       USE W3ODATMD, ONLY: SCREEN
  361                                 
  362       implicit none
  363 !
  364 !     PRIVATE
  365              
  366       PUBLIC :: INIT_WVCP
  367       PUBLIC :: initialize_wav_routers
  368       PUBLIC :: COAWST_CPL
  369       PUBLIC :: wav2ocn_coupling
  370       PUBLIC :: wavfocn_coupling
  371       PUBLIC :: wav2atm_coupling
  372       PUBLIC :: wavfatm_coupling
  373       PUBLIC :: finalize_wav_coupling
  374 !
  375 !  Declarations.
  376 !
  377       TYPE T_GlobalSegMap_G
  378         TYPE(GlobalSegMap) :: GSMapSWAN         ! GloabalSegMap variables
  379       END TYPE T_GlobalSegMap_G
  380       TYPE (T_GlobalSegMap_G), ALLOCATABLE :: GlobalSegMap_G(:)
  381                                                                
  382       TYPE T_AttrVect_G
  383         TYPE(AttrVect) :: wav2ocn_AV            ! AttrVect variables
  384         TYPE(AttrVect) :: ocn2wav_AV
  385         TYPE(AttrVect) :: atm2wav_AV            ! AttrVec variables
 
 
                                                         cwstwvcp.F90  page   8
 
 
  386         TYPE(AttrVect) :: wav2atm_AV            ! AttrVec variables
  387       END TYPE T_AttrVect_G
  388       TYPE (T_AttrVect_G), ALLOCATABLE :: AttrVect_G(:)
  389                                                        
  390       TYPE T_Router_O
  391         type(Router)   :: SWANtoROMS            ! Router variables
  392       END TYPE T_Router_O
  393       TYPE (T_Router_O), ALLOCATABLE :: Router_O(:,:)
  394                                                      
  395       TYPE T_GSMapInterp_A
  396         TYPE(GlobalSegMap) :: GSMapWRF        ! GloabalSegMap variables
  397       END TYPE T_GSMapInterp_A
  398       TYPE (T_GSMapInterp_A), ALLOCATABLE :: GSMapInterp_A(:,:)
  399                                                                
  400       TYPE T_Router_A
  401         type(Router)   :: SWANtoWRF           ! Router variables
  402       END TYPE T_Router_A
  403       TYPE (T_Router_A), ALLOCATABLE :: Router_A(:,:)
  404                                                      
  405       TYPE T_AV2_A
  406         TYPE(AttrVect) :: wav2atm_AV2         ! AttrVect variables
  407         TYPE(AttrVect) :: atm2wav_AV2
  408       END TYPE T_AV2_A
  409       TYPE (T_AV2_A), ALLOCATABLE :: AV2_A(:,:)
  410                                                
  411       TYPE(SparseMatrix) :: sMatW             ! Sparse matrix elements
  412       TYPE(SparseMatrix) :: sMatA             ! Sparse matrix elements
  413       TYPE T_SMPlus_G
  414         TYPE(SparseMatrixPlus) :: A2WMatPlus    ! Sparse matrix plus elem>
  415         TYPE(SparseMatrixPlus) :: W2AMatPlus    ! Sparse matrix plus elem>
  416       END TYPE T_SMPlus_G
  417       TYPE (T_SMPlus_G), ALLOCATABLE :: SMPlus_G(:,:)
  418                                                      
  419       CONTAINS
  420               
  421 !***********************************************************************
  422 !                                                                      *
  423       SUBROUTINE COAWST_CPL (first)
  424 !                                                                      *
  425 !***********************************************************************
  426 !
  427       USE MCT_COUPLER_PARAMS
  428 !
  429       IMPLICIT NONE
  430 !
  431       INTEGER,  intent(in) :: first
  432       INTEGER   io, iw, ia, offset, run_couple
  433                                               
  434 !
  435 !     Send data to ocn model.
  436       DO iw=1,Nwav_grids
  437         DO io=1,Nocn_grids
  438           run_couple=1
  439 !         IF ((first.eq.1).and.(iics(iw).eq.0)) run_couple=0
  440           IF (MOD(first, nWAV2OCN(1,1)).ne.0) run_couple=0
 
 
                                                         cwstwvcp.F90  page   9
 
 
  441           IF (run_couple.eq.1) THEN
  442             CALL WAV2OCN_COUPLING (iw, io)
  443           ELSE
  444             GOTO 40
  445           END IF
  446         END DO
  447       END DO
  448   40  CONTINUE
  449 !
  450 !     Call to get data from ocn model.
  451       DO iw=1,Nwav_grids
  452         DO io=1,Nocn_grids
  453           run_couple=1
  454 !         IF ((first.eq.1).and.(iics(iw).eq.0)) run_couple=0
  455           IF (MOD(first, nWAVFOCN(1,1)).ne.0) run_couple=0
  456           IF (run_couple.eq.1) THEN
  457              CALL WAVFOCN_COUPLING (iw, io)
  458           ELSE
  459             GOTO 50
  460           END IF
  461         END DO
  462       END DO
  463   50  CONTINUE
  464 !
  465 !     Call to get data from atm model.
  466       DO iw=1,Nwav_grids
  467 !        CALL INIT_POINTERS(iw)
  468 !        CALL INIT_COUPLING_POINTERS(iw)
  469         DO ia=1,Natm_grids
  470           run_couple=1
  471 !          IF ((first.eq.1).and.(iics(iw).eq.0)) run_couple=0
  472           IF (MOD(first, nWAVFATM(1,1)).ne.0) run_couple=0
  473           IF (run_couple.eq.1) THEN
  474             CALL WAVFATM_COUPLING (iw, ia)
  475           ELSE
  476             GOTO 55
  477           END IF
  478         END DO
  479       END DO
  480   55  CONTINUE
  481 !
  482 !     Send data to atm model.
  483       DO iw=1,Nwav_grids
  484 !        CALL INIT_POINTERS(iw)
  485 !        CALL INIT_COUPLING_POINTERS(iw)
  486         DO ia=1,Natm_grids
  487           run_couple=1
  488 !          IF ((first.eq.1).and.(iics(iw).eq.0)) run_couple=0
  489           IF (MOD(first, nWAV2ATM(1,1)).ne.0) run_couple=0
  490           IF (run_couple.eq.1) THEN
  491             CALL WAV2ATM_COUPLING (iw, ia)
  492           ELSE
  493             GOTO 45
  494           END IF
  495         END DO
 
 
                                                         cwstwvcp.F90  page  10
 
 
  496       END DO
  497   45  CONTINUE
  498               
  499       RETURN
  500       END SUBROUTINE COAWST_CPL
  501                                
  502       SUBROUTINE INIT_WVCP (ng)
  503 !
  504 !=======================================================================
  505 !                                                                      !
  506 !  Initialize waves and ocean models coupling stream.  This is the     !
  507 !  training phase use to constuct  MCT  parallel interpolators and     !
  508 !  stablish communication patterns.                                    !
  509 !                                                                      !
  510 !=======================================================================
  511 !
  512       USE MCT_COUPLER_PARAMS
  513       USE W3GDATMD, ONLY: NX, NY, NSEA
  514 !     USE mod_coupler_iounits
  515 !
  516       include 'mpif.h'
  517       integer, intent(in) :: ng
  518 !
  519 !  Local variable declarations.
  520 !
  521       integer :: MyError, MyRank
  522       integer :: gsmsize, Nprocs
  523       integer :: i, j, io, ia, Isize, Jsize, Asize
  524       integer :: nRows, nCols, num_sparse_elems
  525       integer :: cid, cad
  526       character (len=70)  :: nc_name
  527       character (len=20)  :: to_add
  528       character (len=120) :: wostring
  529       character (len=120) :: owstring
  530                                      
  531       real :: cff
  532                  
  533 !     integer, dimension(2) :: src_grid_dims, dst_grid_dims
  534       integer, allocatable :: start(:), length(:)
  535 !
  536 !-----------------------------------------------------------------------
  537 !  Begin initialization phase.
  538 !-----------------------------------------------------------------------
  539 !
  540 !  Get communicator local rank and size.
  541 !
  542       CALL mpi_comm_rank (WAV_COMM_WORLD, MyRank, MyError)
  543       CALL mpi_comm_size (WAV_COMM_WORLD, Nprocs, MyError)
  544 !
  545 !  Initialize MCT coupled model registry.
  546 !
  547       IF (ng.eq.1) THEN
  548         ALLOCATE(GlobalSegMap_G(Nwav_grids))
  549         ALLOCATE(AttrVect_G(Nwav_grids))
  550         ALLOCATE(SMPlus_G(Nwav_grids,Natm_grids))
 
 
                                                         cwstwvcp.F90  page  11
 
 
  551         ALLOCATE(AV2_A(Nwav_grids,Natm_grids))
  552         ALLOCATE(GSMapInterp_A(Nwav_grids,Natm_grids))
  553       END IF
  554 !
  555       WAVid=wavids(ng)
  556       IF (Nwav_grids.gt.1) THEN
  557         CALL MCTWorld_init (N_mctmodels, MPI_COMM_WORLD,                &
  558      &                      WAV_COMM_WORLD,myids=wavids)
  559       ELSE
  560         CALL MCTWorld_init (N_mctmodels, MPI_COMM_WORLD,                &
  561      &                      WAV_COMM_WORLD,WAVid)
  562       END IF
  563 !
  564 !  Initialize a Global Segment Map for non-haloed transfer of data for
  565 !  SWAN. Determine non-haloed start and length arrays for this
  566 !  processor. For now, this will set up a tiled exchange that is
  567 !  not identical to thw ww3 tiling.
  568 !
  569       Jsize=INT(NY/Nprocs)
  570       IF (MyRank.eq.Nprocs-1) THEN
  571         Jsize=NY-Jsize*(Nprocs-1)
  572       ENDIF
  573       IF (.not.allocated(start)) THEN
  574         allocate ( start(1) )
  575       END IF
  576       IF (.not.allocated(length)) THEN
  577         allocate ( length(1) )
  578       END IF
  579       start=(MyRank*INT(NY/Nprocs))*NX+1
  580       length=Jsize*NX
  581                      
  582       gsmsize=length(1)
  583 !
  584       CALL GlobalSegMap_init (GlobalSegMap_G(ng)%GSMapSWAN, start,      &
  585      &                        length, 0, WAV_COMM_WORLD, WAVid)
  586       deallocate (start)
  587       deallocate (length)
  588                          
  589 !
  590 !  If wave grid and atm grids are different sizes, then
  591 !  develop sparse matrices for interpolation.
  592 !
  593   35  FORMAT(a3,i1,a7,i1,a11)
  594       DO ia=1,Natm_grids
  595 !!!!!!!!!!!!!!!!!!!!!!
  596 ! First work on atm to wave.
  597 !!!!!!!!!!!!!!!!!!!!!!
  598 !
  599         IF (MyRank.eq.0) THEN
  600 !          IF (scrip_opt.eq.1) THEN
  601             write(nc_name,35) 'atm',ia,'_to_wav',ng,'_weights.nc'
  602 !          ELSE
  603 !            nc_name=A2Wname(ia,ng)
  604 !          END IF
  605           call get_sparse_matrix (ng, nc_name, num_sparse_elems,        &
 
 
                                                         cwstwvcp.F90  page  12
 
 
  606      &                            src_grid_dims, dst_grid_dims)
  607 !
  608 ! Init the sparse matrix.
  609 !
  610           nRows=dst_grid_dims(1)*dst_grid_dims(2)
  611           nCols=src_grid_dims(1)*src_grid_dims(2)
  612 !
  613 ! Create sparse matrix.
  614 !
  615 !         Sparse rows is the dst address. Multiply the interp weights
  616 !         by the dst masking.
  617 !
  618           DO i=1,num_sparse_elems
  619             j=sparse_rows(i)
  620             cff=REAL(dst_grid_imask(j),m8)
  621             sparse_weights(i)=sparse_weights(i)*cff
  622           END DO
  623                 
  624           call SparseMatrix_init(sMatA,nRows,nCols,num_sparse_elems)
  625           call SparseMatrix_importGRowInd(sMatA, sparse_rows,           &
  626      &                                    size(sparse_rows))
  627           call SparseMatrix_importGColInd(sMatA, sparse_cols,           &
  628      &                                    size(sparse_cols))
  629           call SparseMatrix_importMatrixElts(sMatA, sparse_weights,     &
  630      &                                       size(sparse_weights))
  631 !
  632 ! Deallocate arrays.
  633 !
  634           deallocate ( sparse_rows )
  635           deallocate ( sparse_cols )
  636           deallocate ( sparse_weights )
  637           deallocate ( dst_grid_imask )
  638                                        
  639 !!!!!!!!!!!!!!!!!!!!!!
  640 ! Second work on waves to atm.
  641 !!!!!!!!!!!!!!!!!!!!!!
  642 !
  643 !          IF (scrip_opt.eq.1) THEN
  644             write(nc_name,35) 'wav',ng,'_to_atm',ia,'_weights.nc'
  645 !          ELSE
  646 !            nc_name=W2Aname(ng,ia)
  647 !          END IF
  648           call get_sparse_matrix (ng, nc_name, num_sparse_elems,        &
  649      &                            src_grid_dims, dst_grid_dims)
  650 !
  651 ! Init the sparse matrix.
  652 !
  653           nRows=dst_grid_dims(1)*dst_grid_dims(2)
  654           nCols=src_grid_dims(1)*src_grid_dims(2)
  655 !
  656 ! Create sparse matrix.
  657 !
  658           DO i=1,num_sparse_elems
  659             j=sparse_rows(i)
  660             cff=REAL(dst_grid_imask(j),m8)
 
 
                                                         cwstwvcp.F90  page  13
 
 
  661             sparse_weights(i)=sparse_weights(i)*cff
  662           END DO
  663 !
  664 ! Load the dst grid as a coupling mask.
  665 !
  666           allocate(W2A_CPLMASK(ng,ia)%dst_mask(nRows))
  667           DO i=1,nRows
  668             W2A_CPLMASK(ng,ia)%dst_mask(i)=dst_grid_imask(i)
  669           END DO
  670 !
  671           call SparseMatrix_init(sMatW,nRows,nCols,num_sparse_elems)
  672           call SparseMatrix_importGRowInd(sMatW, sparse_rows,           &
  673      &                                    size(sparse_rows))
  674           call SparseMatrix_importGColInd(sMatW, sparse_cols,           &
  675      &                                    size(sparse_cols))
  676           call SparseMatrix_importMatrixElts(sMatW, sparse_weights,     &
  677      &                                       size(sparse_weights))
  678 !
  679 ! Deallocate arrays.
  680 !
  681           deallocate ( sparse_rows )
  682           deallocate ( sparse_cols )
  683           deallocate ( sparse_weights )
  684           deallocate ( dst_grid_imask )
  685         END IF
  686         CALL mpi_bcast(dst_grid_dims, 2, MPI_INTEGER, 0,                &
  687      &                 WAV_COMM_WORLD, MyError)
  688 !
  689 ! scatter dst_grid_imask to be used as cpl_mask
  690 !
  691         IF (MyRank.ne.0) THEN
  692           nRows=dst_grid_dims(1)*dst_grid_dims(2)
  693           allocate(W2A_CPLMASK(ng,ia)%dst_mask(nRows))
  694         END IF
  695         CALL mpi_bcast(W2A_CPLMASK(ng,ia)%dst_mask, nRows,              &
  696      &                 MPI_INTEGER, 0,                                  &
  697      &                 WAV_COMM_WORLD, MyError)
  698 !
  699 !  Initialize a Global Segment Map for non-haloed transfer of data
  700 !  for the atmosphere model.
  701 !
  702         Isize=INT(dst_grid_dims(1)/Nprocs)
  703         IF (MyRank.eq.Nprocs-1) THEN
  704           Isize=dst_grid_dims(1)-Isize*(Nprocs-1)
  705         ENDIF
  706         IF (.not.allocated(start)) THEN
  707           allocate ( start(1) )
  708         END IF
  709         IF (.not.allocated(length)) THEN
  710           allocate ( length(1) )
  711         END IF
  712         start=(MyRank*INT(dst_grid_dims(1)/Nprocs))*dst_grid_dims(2)+1
  713         length=Isize*dst_grid_dims(2)
  714 !
  715         CALL GlobalSegMap_init (GSMapInterp_A(ng,ia)%GSMapWRF,          &
 
 
                                                         cwstwvcp.F90  page  14
 
 
  716      &                          start, length, 0, WAV_COMM_WORLD, WAVid)
  717         deallocate (start)
  718         deallocate (length)
  719         call mpi_barrier(WAV_COMM_WORLD, MyError)
  720 !
  721 ! Create ATM sparse matrix plus for interpolation.
  722 ! Specify matrix decomposition to be by row.
  723 !
  724         call SparseMatrixPlus_init(SMPlus_G(ng,ia)%A2WMatPlus, sMatA,   &
  725      &                             GSMapInterp_A(ng,ia)%GSMapWRF,       &
  726      &                             GlobalSegMap_G(ng)%GSMapSWAN,        &
  727      &                             Xonly,0,WAV_COMM_WORLD, WAVid)
  728         call SparseMatrix_clean(sMatA)
  729 !
  730 ! Create Wave sparse matrix plus for interpolation.
  731 ! Specify matrix decomposition to be by row.
  732 !
  733          call SparseMatrixPlus_init(SMPlus_G(ng,ia)%W2AMatPlus, sMatW,  &
  734      &                              GlobalSegMap_G(ng)%GSMapSWAN,       &
  735      &                              GSMapInterp_A(ng,ia)%GSMapWRF,      &
  736      &                              Xonly,0,WAV_COMM_WORLD, WAVid)
  737         call SparseMatrix_clean(sMatW)
  738       END DO
  739 !
  740 !  Initialize attribute vector holding the export data code strings of
  741 !  the wave model.
  742 !
  743       cad=LEN(wostring)
  744       DO i=1,cad
  745         wostring(i:i)=''
  746       END DO
  747       cid=1
  748 !
  749       to_add='DISBOT'
  750       cad=LEN_TRIM(to_add)
  751       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  752       cid=cid+cad
  753 !
  754       to_add=':DISSURF'
  755       cad=LEN_TRIM(to_add)
  756       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  757       cid=cid+cad
  758 !
  759       to_add=':DISWCAP'
  760       cad=LEN_TRIM(to_add)
  761       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  762       cid=cid+cad
  763 !
  764       to_add=':HSIGN'
  765       cad=LEN_TRIM(to_add)
  766       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  767       cid=cid+cad
  768 !
  769       to_add=':RTP'
  770       cad=LEN_TRIM(to_add)
 
 
                                                         cwstwvcp.F90  page  15
 
 
  771       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  772       cid=cid+cad
  773 !
  774       to_add=':TMBOT'
  775       cad=LEN_TRIM(to_add)
  776       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  777       cid=cid+cad
  778 !
  779       to_add=':UBOT'
  780       cad=LEN_TRIM(to_add)
  781       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  782       cid=cid+cad
  783 !
  784       to_add=':DIRE'
  785       cad=LEN_TRIM(to_add)
  786       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  787       cid=cid+cad
  788                  
  789       to_add=':DIRN'
  790       cad=LEN_TRIM(to_add)
  791       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  792       cid=cid+cad
  793 !
  794       to_add=':WLEN'
  795       cad=LEN_TRIM(to_add)
  796       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  797       cid=cid+cad
  798 !
  799       to_add=':WLENP'
  800       cad=LEN_TRIM(to_add)
  801       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  802       cid=cid+cad
  803 !
  804       to_add=':QB'
  805       cad=LEN_TRIM(to_add)
  806       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  807       cid=cid+cad
  808 !
  809       to_add=':WDSPR'
  810       cad=LEN_TRIM(to_add)
  811       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  812       cid=cid+cad
  813 !
  814       to_add=':WQP'
  815       cad=LEN_TRIM(to_add)
  816       write(wostring(cid:cid+cad-1),'(a)') to_add(1:cad)
  817       cid=cid+cad
  818 !
  819 !  Finalize and remove trailing spaces from the wostring
  820 !  for the rlist.
  821 !
  822       cad=LEN_TRIM(wostring)
  823       wostring=wostring(1:cad)
  824 !
  825       CALL AttrVect_init(AttrVect_G(ng)%wav2ocn_AV,                     &
 
 
                                                         cwstwvcp.F90  page  16
 
 
  826      &                   rList=TRIM(wostring),lsize=gsmsize)
  827       CALL AttrVect_zero(AttrVect_G(ng)%wav2ocn_AV)
  828 !
  829 !  Initialize attribute vector holding the export data code string of
  830 !  the ocean model.
  831 !
  832       cad=LEN(owstring)
  833       DO i=1,cad
  834         owstring(i:i)=''
  835       END DO
  836       cid=1
  837 !
  838       to_add='DEPTH'
  839       cad=LEN_TRIM(to_add)
  840       write(owstring(cid:cid+cad-1),'(a)') to_add(1:cad)
  841       cid=cid+cad
  842 !
  843       to_add=':WLEV'
  844       cad=LEN_TRIM(to_add)
  845       write(owstring(cid:cid+cad-1),'(a)') to_add(1:cad)
  846       cid=cid+cad
  847 !
  848       to_add=':VELX'
  849       cad=LEN_TRIM(to_add)
  850       write(owstring(cid:cid+cad-1),'(a)') to_add(1:cad)
  851       cid=cid+cad
  852 !
  853       to_add=':VELY'
  854       cad=LEN_TRIM(to_add)
  855       write(owstring(cid:cid+cad-1),'(a)') to_add(1:cad)
  856       cid=cid+cad
  857 !
  858       to_add=':ZO'
  859       cad=LEN_TRIM(to_add)
  860       write(owstring(cid:cid+cad-1),'(a)') to_add(1:cad)
  861       cid=cid+cad
  862 !
  863 !  Finalize and remove trailing spaces from the owstring
  864 !  for the rlist.
  865 !
  866       cad=LEN_TRIM(owstring)
  867       owstring=owstring(1:cad)
  868 !
  869       CALL AttrVect_init (AttrVect_G(ng)%ocn2wav_AV,                    &
  870      &                    rList=TRIM(owstring),lsize=gsmsize)
  871       CALL AttrVect_zero (AttrVect_G(ng)%ocn2wav_AV)
  872 !
  873 !  Initialize attribute vector holding the export data code string of
  874 !  the atmosphere model.
  875 !
  876       Asize=GlobalSegMap_lsize(GlobalSegMap_G(ng)%GSMapSWAN,            &
  877      &                         WAV_COMM_WORLD)
  878       CALL AttrVect_init (AttrVect_G(ng)%atm2wav_AV, rlist="U10:V10",   &
  879      &                    lsize=Asize)
  880       CALL AttrVect_zero (AttrVect_G(ng)%atm2wav_AV)
 
 
                                                         cwstwvcp.F90  page  17
 
 
  881 !
  882 ! Initialize atribute vector holding wave data to atm.
  883 !
  884       CALL AttrVect_init(AttrVect_G(ng)%wav2atm_AV,                     &
  885      &                   rList="HSIGN:WLENP:RTP",                       &
  886      &                   lsize=Asize)
  887       CALL AttrVect_zero(AttrVect_G(ng)%wav2atm_AV)
  888 !
  889       DO ia=1,Natm_grids
  890 !  Initialize attribute vector holding the export data code strings of
  891 !  the atm model. The Asize is the number of grid point on this
  892 !  processor.
  893 !
  894         Asize=GlobalSegMap_lsize(GSMapInterp_A(ng,ia)%GSMapWRF,         &
  895      &                           WAV_COMM_WORLD)
  896         CALL AttrVect_init (AV2_A(ng,ia)%atm2wav_AV2, rlist="U10:V10",  &
  897      &                      lsize=Asize)
  898         CALL AttrVect_zero (AV2_A(ng,ia)%atm2wav_AV2)
  899 !
  900 !  Initialize attribute vector holding the export data code string of
  901 !  the wave model.
  902 !
  903         CALL AttrVect_init(AV2_A(ng,ia)%wav2atm_AV2,                    &
  904      &                     rList="HSIGN:WLENP:RTP:CPL_MASK",            &
  905      &                     lsize=Asize)
  906         CALL AttrVect_zero(AV2_A(ng,ia)%wav2atm_AV2)
  907       END DO
  908             
  909       RETURN
  910       END SUBROUTINE INIT_WVCP
  911                               
  912       SUBROUTINE INITIALIZE_WAV_ROUTERS
  913 !
  914 !=======================================================================
  915 !                                                                      !
  916 !  Initialize waves routers for wave model.                            !
  917 !                                                                      !
  918 !=======================================================================
  919 !
  920       USE MCT_COUPLER_PARAMS
  921 !      USE ww3_iounits
  922 !     USE M_PARALL
  923 !
  924 !      include 'mpif.h'
  925 !
  926 !  Local variable declarations.
  927 !
  928       integer :: MyError, MyRank
  929       integer :: ng, iw, ia
  930 !
  931 !  Initialize MCT Routers.
  932 !
  933       ALLOCATE(Router_O(Nwav_grids,Nocn_grids))
  934 !
  935 !  Initialize a router to the ocean model component.
 
 
                                                         cwstwvcp.F90  page  18
 
 
  936 !
  937       DO ng=1,Nocn_grids
  938         DO iw=1,Nwav_grids
  939           OCNid=ocnids(ng)
  940           CALL Router_init (OCNid, GlobalSegMap_G(iw)%GSMapSWAN,        &
  941      &                      WAV_COMM_WORLD, Router_O(iw,ng)%SWANtoROMS)
  942         END DO
  943       END DO
  944       ALLOCATE(Router_A(Nwav_grids,Natm_grids))
  945 !
  946 !  Initialize a router to the atmosphere model component.
  947 !
  948       DO ia=1,Natm_grids
  949         DO iw=1,Nwav_grids
  950           ATMid=atmids(ia)
  951           CALL Router_init (ATMid, GSMapInterp_A(iw,ia)%GSMapWRF,       &
  952      &                      WAV_COMM_WORLD, Router_A(iw,ia)%SWANtoWRF)
  953         END DO
  954       END DO
  955             
  956       RETURN
  957       END SUBROUTINE INITIALIZE_WAV_ROUTERS
  958                                            
  959       SUBROUTINE WAV2OCN_COUPLING (iw, io)
  960 !
  961 !=======================================================================
  962 !                                                                      !
  963 !  This subroutine reads and writes the coupling data streams between  !
  964 !  ocean and wave models. Currently, the following data streams are    !
  965 !  processed:                                                          !
  966 !                                                                      !
  967 !  Fields exported to the OCEAN model:                                 !
  968 !                                                                      !
  969 !     * Wave direction (degrees)                                       !
  970 !     * Significant wave height (m)                                    !
  971 !     * Average wave length (m)                                        !
  972 !     * Surface wave relative peak period (s)                          !
  973 !     * Bottom wave period (s)                                         !
  974 !     * Percent of breakig waves (nondimensional)                      !
  975 !     * Wave energy dissipation (W/m2)                                 !
  976 !     * Wave bottom orbital velocity (m/s)                             !
  977 !                                                                      !
  978 !  Fields imported from the OCEAN Model:                               !
  979 !                                                                      !
  980 !     * Bathymetry, bottom elevation (m)                               !
  981 !     * Free-surface, water surface elevation (m)                      !
  982 !     * Depth integrated u-momentum (m/s)                              !
  983 !     * Depth integrated v-momentum (m/s)                              !
  984 !                                                                      !
  985 !=======================================================================
  986 !
  987       USE CONSTANTS, ONLY: PI
  988       USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, MAPSF
  989       USE MCT_COUPLER_PARAMS
  990       USE W3ADATMD, ONLY: HS, PHIBBL, PHIOC, FP0, T0M1, UBA
 
 
                                                         cwstwvcp.F90  page  19
 
 
  991       USE W3ADATMD, ONLY: THM, WLM
  992 !     USE W3ODATMD, ONLY: QB
  993       USE W3WDATMD, ONLY: VA
  994       USE W3IOGOMD
  995 !
  996       implicit none
  997       INCLUDE "mpif.h"
  998 !
  999 !  Imported variable declarations.
 1000 !
 1001       integer :: Numcouple, iw, io
 1002       integer :: IP, IX, IY
 1003 !
 1004 !  Local variable declarations.
 1005 !
 1006       integer :: MyStatus, MyError, MySize, MyRank
 1007       integer :: i, id, j, gsmsize, ierr, indx, Tag, JSEA
 1008       integer :: Istr, Iend, Jstr, Jend
 1009       integer :: start, length, grdsize
 1010       integer :: Isize, Jsize, Nprocs, ipp
 1011       integer, pointer :: points(:)
 1012                                    
 1013       real, pointer :: SND_BUF(:), RCV_BUF(:)
 1014       real(m8) :: fac, cff
 1015       real(m8), pointer :: avdata(:)
 1016       real(m8), pointer :: DIRE(:)
 1017       real(m8), pointer :: DIRN(:)
 1018 !
 1019 !-----------------------------------------------------------------------
 1020 !  Send wave fields to ROMS.
 1021 !-----------------------------------------------------------------------
 1022 !
 1023       CALL MPI_COMM_RANK (WAV_COMM_WORLD, MyRank, MyError)
 1024       CALL MPI_COMM_SIZE (WAV_COMM_WORLD, Nprocs, MyError)
 1025 !
 1026 !  Get the number of grid point on this processor.
 1027 !
 1028       gsmsize=GlobalSegMap_lsize(GlobalSegMap_G(iw)%GSMapSWAN,          &
 1029      &                           WAV_COMM_WORLD)
 1030 !
 1031 !  Allocate attribute vector array used to export/import data.
 1032 !
 1033       allocate ( avdata(gsmsize),stat=ierr )
 1034       allocate ( DIRE(gsmsize),stat=ierr )
 1035       allocate ( DIRN(gsmsize),stat=ierr )
 1036       avdata=0.0_m8
 1037       DIRE=0.0_m8
 1038       DIRN=0.0_m8
 1039 !
 1040       grdsize=NX*NY
 1041       allocate ( SND_BUF(grdsize),stat=ierr )
 1042       SND_BUF=0.0
 1043       allocate ( RCV_BUF(grdsize),stat=ierr )
 1044       RCV_BUF=0.0
 1045 !
 
 
                                                         cwstwvcp.F90  page  20
 
 
 1046 !  Ask for points in this tile.
 1047 !
 1048       CALL GlobalSegMap_Ordpnts (GlobalSegMap_G(iw)%GSMapSWAN,          &
 1049      &                           MyRank, points)
 1050 !
 1051 !  Determine grid tiling for exchanges.
 1052 !
 1053       Jsize=INT(NY/Nprocs)
 1054       IF (MyRank.eq.Nprocs-1) THEN
 1055         Jsize=NY-Jsize*(Nprocs-1)
 1056       ENDIF
 1057       start=(MyRank*INT(NY/Nprocs))*NX+1
 1058       length=Jsize*NX
 1059                      
 1060 !
 1061 !  Call the output routine to compute the mean properties.
 1062 !
 1063        CALL W3OUTG ( VA, .FALSE., .FALSE., .FALSE. )
 1064 !
 1065 !  Load WW3 data into MCT storage buffers.
 1066 !  The data is exported using ROMS definition for real kind m8=r8.
 1067 !
 1068 !-------------------------------------------------------------------
 1069 !  DISBOT: Dissipation Bottom
 1070 !
 1071 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1072 !  The local variable is only 1:NSEAL(M) long.
 1073 !
 1074       SND_BUF=0.0
 1075       DO i=1,NSEAL
 1076         IP=(MyRank+1)+(i-1)*Nprocs
 1077         IX     = MAPSF(IP,1)
 1078         IY     = MAPSF(IP,2)
 1079         IP=(IY-1)*NX+IX
 1080         SND_BUF(IP)=PHIBBL(i)
 1081       END DO
 1082 !
 1083 !  Gather up all the data.
 1084 !
 1085       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1086      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1087 !
 1088 !  Now extract the section of data from this tile
 1089 !  and fill the mct array.
 1090 !
 1091       IP=0
 1092       DO i=start,start+length-1
 1093         IP=IP+1
 1094         avdata(IP)=REAL(RCV_BUF(i),m8)
 1095       END DO
 1096       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1097      &                             "DISBOT",avdata)
 1098 !-------------------------------------------------------------------
 1099 !  DISSURF: Dissipation surface
 1100 !
 
 
                                                         cwstwvcp.F90  page  21
 
 
 1101 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1102 !  The local variable is only 1:NSEAL(M) long.
 1103 !
 1104       SND_BUF=0.0
 1105       DO i=1,NSEAL
 1106         IP=(MyRank+1)+(i-1)*Nprocs
 1107         IX     = MAPSF(IP,1)
 1108         IY     = MAPSF(IP,2)
 1109         IP=(IY-1)*NX+IX
 1110         SND_BUF(IP)=PHIOC(i)
 1111       END DO
 1112 !
 1113 !  Gather up all the data.
 1114 !
 1115       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1116      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1117 !
 1118 !  Now extract the section of data from this tile
 1119 !  and fill the mct array.
 1120 !
 1121       IP=0
 1122       DO i=start,start+length-1
 1123         IP=IP+1
 1124         avdata(IP)=REAL(RCV_BUF(i),m8)
 1125       END DO
 1126       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1127      &                             "DISSURF",avdata)
 1128 !-------------------------------------------------------------------
 1129 !  DISWCAP: Dissipation white capping
 1130 !
 1131 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1132 !  The local variable is only 1:NSEAL(M) long.
 1133 !
 1134       SND_BUF=0.0
 1135       DO i=1,NSEAL
 1136         IP=(MyRank+1)+(i-1)*Nprocs
 1137         IX     = MAPSF(IP,1)
 1138         IY     = MAPSF(IP,2)
 1139         IP=(IY-1)*NX+IX
 1140         SND_BUF(IP)=PHIOC(i)*0.        ! jcw need this
 1141       END DO
 1142 !
 1143 !  Gather up all the data.
 1144 !
 1145       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1146      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1147 !
 1148 !  Now extract the section of data from this tile
 1149 !  and fill the mct array.
 1150 !
 1151       IP=0
 1152       DO i=start,start+length-1
 1153         IP=IP+1
 1154         avdata(IP)=REAL(RCV_BUF(i),m8)
 1155       END DO
 
 
                                                         cwstwvcp.F90  page  22
 
 
 1156       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1157      &                             "DISWCAP",avdata)
 1158 !-------------------------------------------------------------------
 1159 !  HS: Signfiicant wave height
 1160 !
 1161 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1162 !  The local variable is only 1:NSEAL(M) long.
 1163 !
 1164       SND_BUF=0.0
 1165       DO i=1,NSEAL
 1166         IP=(MyRank+1)+(i-1)*Nprocs
 1167         IX     = MAPSF(IP,1)
 1168         IY     = MAPSF(IP,2)
 1169         IP=(IY-1)*NX+IX
 1170         SND_BUF(IP)=HS(i)
 1171       END DO
 1172 !
 1173 !  Gather up all the data.
 1174 !
 1175       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1176      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1177 !
 1178 !  Now extract the section of data from this tile
 1179 !  and fill the mct array.
 1180 !
 1181       IP=0
 1182       DO i=start,start+length-1
 1183         IP=IP+1
 1184         avdata(IP)=REAL(RCV_BUF(i),m8)
 1185       END DO
 1186       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1187      &                             "HSIGN",avdata)
 1188 !-------------------------------------------------------------------
 1189 !  RTP: Peak surface period
 1190 !
 1191 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1192 !  The local variable is only 1:NSEAL(M) long.
 1193 !
 1194       SND_BUF=0.0
 1195       DO i=1,NSEAL
 1196         IP=(MyRank+1)+(i-1)*Nprocs
 1197         IX     = MAPSF(IP,1)
 1198         IY     = MAPSF(IP,2)
 1199         IP=(IY-1)*NX+IX
 1200         SND_BUF(IP)=FP0(i)
 1201       END DO
 1202 !
 1203 !  Gather up all the data.
 1204 !
 1205       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1206      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1207 !
 1208 !  Now extract the section of data from this tile
 1209 !  and fill the mct array.
 1210 !
 
 
                                                         cwstwvcp.F90  page  23
 
 
 1211 !     fac=2.0_m8*PI
 1212       fac=1.0_m8
 1213       IP=0
 1214       DO i=start,start+length-1
 1215         IP=IP+1
 1216         avdata(IP)=fac/MAX(REAL(RCV_BUF(i),m8),0.001_m8)
 1217       END DO
 1218       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1219      &                             "RTP",avdata)
 1220 !-------------------------------------------------------------------
 1221 !  TMBOT: Mean bottom period
 1222 !
 1223 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1224 !  The local variable is only 1:NSEAL(M) long.
 1225 !
 1226       SND_BUF=0.0
 1227       DO i=1,NSEAL
 1228         IP=(MyRank+1)+(i-1)*Nprocs
 1229         IX     = MAPSF(IP,1)
 1230         IY     = MAPSF(IP,2)
 1231         IP=(IY-1)*NX+IX
 1232         SND_BUF(IP)=T0M1(i)
 1233       END DO
 1234 !
 1235 !  Gather up all the data.
 1236 !
 1237       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1238      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1239 !
 1240 !  Now extract the section of data from this tile
 1241 !  and fill the mct array.
 1242 !
 1243       IP=0
 1244       DO i=start,start+length-1
 1245         IP=IP+1
 1246         avdata(IP)=REAL(RCV_BUF(i),m8)
 1247       END DO
 1248       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1249      &                             "TMBOT",avdata)
 1250 !-------------------------------------------------------------------
 1251 !  UBOT: bottom orbitral velocity
 1252 !
 1253 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1254 !  The local variable is only 1:NSEAL(M) long.
 1255 !
 1256       SND_BUF=0.0
 1257       DO i=1,NSEAL
 1258         IP=(MyRank+1)+(i-1)*Nprocs
 1259         IX     = MAPSF(IP,1)
 1260         IY     = MAPSF(IP,2)
 1261         IP=(IY-1)*NX+IX
 1262         SND_BUF(IP)=UBA(i)
 1263       END DO
 1264 !
 1265 !  Gather up all the data.
 
 
                                                         cwstwvcp.F90  page  24
 
 
 1266 !
 1267       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1268      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1269 !
 1270 !  Now extract the section of data from this tile
 1271 !  and fill the mct array.
 1272 !
 1273       IP=0
 1274       DO i=start,start+length-1
 1275         IP=IP+1
 1276         avdata(IP)=REAL(RCV_BUF(i),m8)
 1277       END DO
 1278       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1279      &                             "UBOT",avdata)
 1280 !-------------------------------------------------------------------
 1281 !  DIRE and DIRN: mean direction, break into 2 components
 1282 !
 1283 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1284 !  The local variable is only 1:NSEAL(M) long.
 1285 !
 1286       SND_BUF=0.0_m8
 1287       RCV_BUF=0.0_m8
 1288       DO i=1,NSEAL
 1289         IP=(MyRank+1)+(i-1)*Nprocs
 1290         IX     = MAPSF(IP,1)
 1291         IY     = MAPSF(IP,2)
 1292         IP=(IY-1)*NX+IX
 1293         SND_BUF(IP)=THM(i)
 1294       END DO
 1295 !
 1296 !  Gather up all the data.
 1297 !
 1298       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1299      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1300 !
 1301 !  Now extract the section of data from this tile
 1302 !  and fill the mct array.
 1303 !
 1304       IP=0
 1305       DO i=start,start+length-1
 1306         IP=IP+1
 1307         avdata(IP)=REAL(RCV_BUF(i),m8)
 1308       END DO
 1309             
 1310 !     fac=PI/180.0
 1311       fac=1.0_m8
 1312       DO IP=1,gsmsize
 1313         cff=450.0_m8*PI/180.0_m8-(avdata(IP)+PI)
 1314         DIRE(IP)=1.0_m8*SIN(cff*fac)
 1315         DIRN(IP)=1.0_m8*COS(cff*fac)
 1316       END DO
 1317       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1318      &                           "DIRE",DIRE)
 1319       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1320      &                           "DIRN",DIRN)
 
 
                                                         cwstwvcp.F90  page  25
 
 
 1321 !-------------------------------------------------------------------
 1322 !  WLEN: mean wave length
 1323 !
 1324 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1325 !  The local variable is only 1:NSEAL(M) long.
 1326 !
 1327       SND_BUF=0.0
 1328       DO i=1,NSEAL
 1329         IP=(MyRank+1)+(i-1)*Nprocs
 1330         IX     = MAPSF(IP,1)
 1331         IY     = MAPSF(IP,2)
 1332         IP=(IY-1)*NX+IX
 1333         SND_BUF(IP)=WLM(i)
 1334       END DO
 1335 !
 1336 !  Gather up all the data.
 1337 !
 1338       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1339      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1340 !
 1341 !  Now extract the section of data from this tile
 1342 !  and fill the mct array.
 1343 !
 1344       IP=0
 1345       DO i=start,start+length-1
 1346         IP=IP+1
 1347         avdata(IP)=REAL(RCV_BUF(i),m8)
 1348       END DO
 1349       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1350      &                             "WLEN",avdata)
 1351 !-------------------------------------------------------------------
 1352 !  WLENP: peak wave length
 1353 !
 1354 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1355 !  The local variable is only 1:NSEAL(M) long.
 1356 !
 1357       SND_BUF=0.0
 1358       DO i=1,NSEAL
 1359         IP=(MyRank+1)+(i-1)*Nprocs
 1360         IX     = MAPSF(IP,1)
 1361         IY     = MAPSF(IP,2)
 1362         IP=(IY-1)*NX+IX
 1363         SND_BUF(IP)=WLM(i)     !  jcw need to make this peak
 1364       END DO
 1365 !
 1366 !  Gather up all the data.
 1367 !
 1368       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1369      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1370 !
 1371 !  Now extract the section of data from this tile
 1372 !  and fill the mct array.
 1373 !
 1374       IP=0
 1375       DO i=start,start+length-1
 
 
                                                         cwstwvcp.F90  page  26
 
 
 1376         IP=IP+1
 1377         avdata(IP)=REAL(RCV_BUF(i),m8)
 1378       END DO
 1379       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1380      &                             "WLENP",avdata)
 1381 !-------------------------------------------------------------------
 1382 !  QB: percent wave break
 1383 !
 1384 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1385 !  The local variable is only 1:NSEAL(M) long.
 1386 !
 1387       SND_BUF=0.0
 1388       DO i=1,NSEAL
 1389         IP=(MyRank+1)+(i-1)*Nprocs
 1390         IX     = MAPSF(IP,1)
 1391         IY     = MAPSF(IP,2)
 1392         IP=(IY-1)*NX+IX
 1393 !       SND_BUF(IP)=QB(i)
 1394         SND_BUF(IP)=0.
 1395       END DO
 1396 !
 1397 !  Gather up all the data.
 1398 !
 1399       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1400      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1401 !
 1402 !  Now extract the section of data from this tile
 1403 !  and fill the mct array.
 1404 !
 1405       IP=0
 1406       DO i=start,start+length-1
 1407         IP=IP+1
 1408         avdata(IP)=REAL(RCV_BUF(i),m8)
 1409       END DO
 1410       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1411      &                             "QB",avdata)
 1412 !-------------------------------------------------------------------
 1413 !  WDSPR: wave dir spread
 1414 !
 1415 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1416 !  The local variable is only 1:NSEAL(M) long.
 1417 !
 1418       SND_BUF=0.0
 1419       DO i=1,NSEAL
 1420         IP=(MyRank+1)+(i-1)*Nprocs
 1421         IX     = MAPSF(IP,1)
 1422         IY     = MAPSF(IP,2)
 1423         IP=(IY-1)*NX+IX
 1424 !       SND_BUF(IP)=QB(i)*0.    ! jcw need this
 1425         SND_BUF(IP)=0.    ! jcw need this
 1426       END DO
 1427 !
 1428 !  Gather up all the data.
 1429 !
 1430       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 
 
                                                         cwstwvcp.F90  page  27
 
 
 1431      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1432 !
 1433 !  Now extract the section of data from this tile
 1434 !  and fill the mct array.
 1435 !
 1436       IP=0
 1437       DO i=start,start+length-1
 1438         IP=IP+1
 1439         avdata(IP)=REAL(RCV_BUF(i),m8)
 1440       END DO
 1441       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1442      &                             "WDSPR",avdata)
 1443 !-------------------------------------------------------------------
 1444 !  WQP:
 1445 !
 1446 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1447 !  The local variable is only 1:NSEAL(M) long.
 1448 !
 1449       SND_BUF=0.0
 1450       DO i=1,NSEAL
 1451         IP=(MyRank+1)+(i-1)*Nprocs
 1452         IX     = MAPSF(IP,1)
 1453         IY     = MAPSF(IP,2)
 1454         IP=(IY-1)*NX+IX
 1455 !       SND_BUF(IP)=QB(i)*0.    ! jcw need this
 1456         SND_BUF(IP)=0.    ! jcw need this
 1457       END DO
 1458 !
 1459 !  Gather up all the data.
 1460 !
 1461       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1462      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1463 !
 1464 !  Now extract the section of data from this tile
 1465 !  and fill the mct array.
 1466 !
 1467       IP=0
 1468       DO i=start,start+length-1
 1469         IP=IP+1
 1470         avdata(IP)=REAL(RCV_BUF(i),m8)
 1471       END DO
 1472       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2ocn_AV,             &
 1473      &                             "WQP",avdata)
 1474 !
 1475 !-----------------------------------------------------------------------
 1476 !  Send wave fields bundle to ocean model, ROMS.
 1477 !-----------------------------------------------------------------------
 1478 !
 1479           Tag=io*100+0*10+iw
 1480           CALL MCT_isend (AttrVect_G(iw)%wav2ocn_AV,                    &
 1481      &                   Router_O(iw,io)%SWANtoROMS, Tag)
 1482           CALL MCT_waits (Router_O(iw,io)%SWANtoROMS)
 1483           IF (MyRank.EQ.0) THEN
 1484             WRITE (SCREEN,36)' == WW3 grid ',iw,                        &
 1485      &                       ' sent wave data to ROMS grid ', io
 
 
                                                         cwstwvcp.F90  page  28
 
 
 1486  36         FORMAT (a14,i2,a29,i2)
 1487           END IF
 1488           IF (MyError.ne.0) THEN
 1489             WRITE (*,*)'coupling send fail swancplr, Error= ', MyError
 1490             CALL FINALIZE_WAV_COUPLING(iw)
 1491           END IF
 1492       deallocate (avdata, points, DIRE, DIRN)
 1493       deallocate (SND_BUF, RCV_BUF)
 1494 !
 1495       RETURN
 1496       END SUBROUTINE WAV2OCN_COUPLING
 1497       SUBROUTINE WAV2ATM_COUPLING (iw, ia)
 1498 !
 1499 !=======================================================================
 1500 !                                                                      !
 1501 !  This subroutine reads and writes the coupling data streams between  !
 1502 !  atm and wave models. Currently, the following data streams are      !
 1503 !  processed:                                                          !
 1504 !                                                                      !
 1505 !  Fields exported to the ATM model:                                   !
 1506 !                                                                      !
 1507 !     * Significant wave height (m)                                    !
 1508 !     * Surface wave relative peak period (s)                          !
 1509 !     * Surface wave length (m)                                        !
 1510 !                                                                      !
 1511 !  Fields imported from the ATM Model:                                 !
 1512 !                                                                      !
 1513 !     * Wind Speed (m/s)                                               !
 1514 !                                                                      !
 1515 !=======================================================================
 1516 !
 1517       USE CONSTANTS, ONLY: PI
 1518       USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, MAPSF
 1519       USE MCT_COUPLER_PARAMS
 1520       USE W3ADATMD, ONLY: HS, PHIBBL, PHIOC, FP0, T0M1, UBA
 1521       USE W3ADATMD, ONLY: WLM
 1522 !
 1523       implicit none
 1524       INCLUDE "mpif.h"
 1525 !
 1526 !  Imported variable declarations.
 1527 !
 1528       integer :: Numcouple, iw, ia
 1529       integer :: IP, IX, IY
 1530 !
 1531 !  Local variable declarations.
 1532 !
 1533       integer :: MyStatus, MyError, MySize, MyRank
 1534       integer :: i, id, j, gsmsize, ierr, indx, Tag
 1535       integer :: Istr, Iend, Jstr, Jend, Asize
 1536       integer :: Isize, Jsize, INDXG, Nprocs, OFFSET
 1537       integer :: start, length, grdsize
 1538       integer, pointer :: points(:)
 1539                                    
 1540       real, pointer :: SND_BUF(:), RCV_BUF(:)
 
 
                                                         cwstwvcp.F90  page  29
 
 
 1541       real(m8) :: fac
 1542       real(m8), pointer :: avdata(:)
 1543       integer, pointer :: indices(:)
 1544       real(m8), pointer :: Amask(:)
 1545 !
 1546 !-----------------------------------------------------------------------
 1547 !  Send wave fields to WRF.
 1548 !-----------------------------------------------------------------------
 1549 !
 1550       CALL MPI_COMM_RANK (WAV_COMM_WORLD, MyRank, MyError)
 1551       CALL MPI_COMM_SIZE (WAV_COMM_WORLD, Nprocs, MyError)
 1552 !
 1553 !  Get the number of grid point on this processor.
 1554 !
 1555       gsmsize=GlobalSegMap_lsize(GlobalSegMap_G(iw)%GSMapSWAN,          &
 1556      &                           WAV_COMM_WORLD)
 1557 !
 1558 !  Allocate attribute vector array used to export/import data.
 1559 !
 1560       allocate ( avdata(gsmsize),stat=ierr )
 1561       avdata=0.0_m8
 1562 !
 1563       grdsize=NX*NY
 1564       allocate ( SND_BUF(grdsize),stat=ierr )
 1565       SND_BUF=0.0
 1566       allocate ( RCV_BUF(grdsize),stat=ierr )
 1567       RCV_BUF=0.0
 1568 !
 1569 !  Ask for points in this tile.
 1570 !
 1571       CALL GlobalSegMap_Ordpnts (GlobalSegMap_G(iw)%GSMapSWAN,          &
 1572      &                           MyRank, points)
 1573 !
 1574 !  Determine grid tiling for exchanges.
 1575 !
 1576       Jsize=INT(NY/Nprocs)
 1577       IF (MyRank.eq.Nprocs-1) THEN
 1578         Jsize=NY-Jsize*(Nprocs-1)
 1579       ENDIF
 1580       start=(MyRank*INT(NY/Nprocs))*NX+1
 1581       length=Jsize*NX
 1582 !
 1583 !  Load WW3 data into MCT storage buffers.
 1584 !  The data is exported using WRF definition for real kind m8=r8.
 1585 !-------------------------------------------------------------------
 1586 !  HS: Signfiicant wave height
 1587 !
 1588 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1589 !  The local variable is only 1:NSEAL(M) long.
 1590 !
 1591       SND_BUF=0.0
 1592       DO i=1,NSEAL
 1593         IP=(MyRank+1)+(i-1)*Nprocs
 1594         IX     = MAPSF(IP,1)
 1595         IY     = MAPSF(IP,2)
 
 
                                                         cwstwvcp.F90  page  30
 
 
 1596         IP=(IY-1)*NX+IX
 1597         SND_BUF(IP)=HS(i)
 1598       END DO
 1599 !
 1600 !  Gather up all the data.
 1601 !
 1602       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1603      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1604 !
 1605 !  Now extract the section of data from this tile
 1606 !  and fill the mct array.
 1607 !
 1608       IP=0
 1609       DO i=start,start+length-1
 1610         IP=IP+1
 1611         avdata(IP)=REAL(RCV_BUF(i),m8)
 1612       END DO
 1613       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2atm_AV,             &
 1614      &                             "HSIGN",avdata)
 1615 !-------------------------------------------------------------------
 1616 !  RTP: Peak surface period
 1617 !
 1618 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1619 !  The local variable is only 1:NSEAL(M) long.
 1620 !
 1621       SND_BUF=0.0
 1622       DO i=1,NSEAL
 1623         IP=(MyRank+1)+(i-1)*Nprocs
 1624         IX     = MAPSF(IP,1)
 1625         IY     = MAPSF(IP,2)
 1626         IP=(IY-1)*NX+IX
 1627         SND_BUF(IP)=FP0(i)
 1628       END DO
 1629 !
 1630 !  Gather up all the data.
 1631 !
 1632       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1633      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1634 !
 1635 !  Now extract the section of data from this tile
 1636 !  and fill the mct array.
 1637 !
 1638       fac=2.0_m8*PI
 1639       IP=0
 1640       DO i=start,start+length-1
 1641         IP=IP+1
 1642         avdata(IP)=fac/MAX(REAL(RCV_BUF(i),m8),0.001_m8)
 1643       END DO
 1644       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2atm_AV,             &
 1645      &                             "RTP",avdata)
 1646 !-------------------------------------------------------------------
 1647 !  WLEN: mean wave length
 1648 !
 1649 !  Fill wet parts of array SND_BUF that is NXxNY length.
 1650 !  The local variable is only 1:NSEAL(M) long.
 
 
                                                         cwstwvcp.F90  page  31
 
 
 1651 !
 1652       SND_BUF=0.0
 1653       DO i=1,NSEAL
 1654         IP=(MyRank+1)+(i-1)*Nprocs
 1655         IX     = MAPSF(IP,1)
 1656         IY     = MAPSF(IP,2)
 1657         IP=(IY-1)*NX+IX
 1658         SND_BUF(IP)=WLM(i)
 1659       END DO
 1660 !
 1661 !  Gather up all the data.
 1662 !
 1663       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 1664      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1665 !
 1666 !  Now extract the section of data from this tile
 1667 !  and fill the mct array.
 1668 !
 1669       IP=0
 1670       DO i=start,start+length-1
 1671         IP=IP+1
 1672         avdata(IP)=REAL(RCV_BUF(i),m8)
 1673       END DO
 1674       CALL AttrVect_importRAttr (AttrVect_G(iw)%wav2atm_AV,             &
 1675      &                             "WLENP",avdata)
 1676 !-------------------------------------------------------------------
 1677 !  Send wave fields bundle to atm model, WRF.
 1678 !-----------------------------------------------------------------------
 1679 !
 1680  35     FORMAT (a14,i2,a23,i5)
 1681 !
 1682 !  Send fields to atmosphere model.
 1683 !
 1684         Tag=ia*10+iw
 1685         CALL MCT_MatVecMul(AttrVect_G(iw)%wav2atm_AV,                   &
 1686      &                     SMPlus_G(iw,ia)%W2AMatPlus,                  &
 1687      &                     AV2_A(iw,ia)%wav2atm_AV2)
 1688 !
 1689 !  Now add in the CPL_MASK before we send it over to wrf.
 1690 !  Get the number of grid points on this processor.
 1691 !
 1692         Asize=GlobalSegMap_lsize (GSMapInterp_A(iw,ia)%GSMapWRF,        &
 1693      &                            WAV_COMM_WORLD)
 1694         allocate (Amask(Asize))
 1695         Amask=0.0
 1696 !
 1697 !  Ask for points in this tile.
 1698 !
 1699         CALL GlobalSegMap_Ordpnts (GSMapInterp_A(iw,ia)%GSMapWRF,       &
 1700      &                             MyRank, points)
 1701 !
 1702 !  Load the dst grid cpl mask into the attr vect.
 1703 !
 1704         DO i=1,Asize
 1705           Amask(i)=REAL(W2A_CPLMASK(iw,ia)%dst_mask(points(i)),m8)
 
 
                                                         cwstwvcp.F90  page  32
 
 
 1706         END DO
 1707         deallocate (points)
 1708         CALL AttrVect_importRAttr (AV2_A(iw,ia)%wav2atm_AV2, "CPL_MASK",&
 1709      &                             Amask, Asize)
 1710         CALL MCT_isend (AV2_A(iw,ia)%wav2atm_AV2,                       &
 1711      &                  Router_A(iw,ia)%SWANtoWRF, Tag)
 1712         CALL MCT_waits (Router_A(iw,ia)%SWANtoWRF)
 1713         IF (MyRank.EQ.0) THEN
 1714           WRITE (SCREEN,35) '== WW3 grid ',iw,' sent data to WRF grid ' &
 1715      &                      ,ia
 1716         END IF
 1717         IF (MyError.ne.0) THEN
 1718           WRITE (*,*) 'coupl fail swancplr, MyStatus= ', MyError
 1719           CALL FINALIZE_WAV_COUPLING(iw)
 1720         END IF
 1721         deallocate (Amask)
 1722         if (associated (indices)) then
 1723           deallocate (indices)
 1724         endif
 1725       deallocate (avdata)
 1726       deallocate (SND_BUF, RCV_BUF)
 1727 !
 1728       RETURN
 1729       END SUBROUTINE WAV2ATM_COUPLING
 1730       SUBROUTINE WAVFOCN_COUPLING (ng, io)
 1731 !
 1732 !=======================================================================
 1733 !                                                                      !
 1734 !  This subroutine reads and writes the coupling data streams between  !
 1735 !  ocean and wave models. Currently, the following data streams are    !
 1736 !  processed:                                                          !
 1737 !                                                                      !
 1738 !  Fields exported to the OCEAN model:                                 !
 1739 !                                                                      !
 1740 !     * Wave direction (degrees)                                       !
 1741 !     * Significant wave height (m)                                    !
 1742 !     * Average wave length (m)                                        !
 1743 !     * Surface wave relative peak period (s)                          !
 1744 !     * Bottom wave period (s)                                         !
 1745 !     * Percent of breakig waves (nondimensional)                      !
 1746 !     * Wave energy dissipation (W/m2)                                 !
 1747 !     * Wave bottom orbital velocity (m/s)                             !
 1748 !                                                                      !
 1749 !  Fields imported from the OCEAN Model:                               !
 1750 !                                                                      !
 1751 !     * Bathymetry, bottom elevation (m)                               !
 1752 !     * Free-surface, water surface elevation (m)                      !
 1753 !     * Depth integrated u-momentum (m/s)                              !
 1754 !     * Depth integrated v-momentum (m/s)                              !
 1755 !                                                                      !
 1756 !=======================================================================
 1757 !
 1758       USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, MAPSF
 1759       USE W3SERVMD
 1760       USE W3IDATMD
 
 
                                                         cwstwvcp.F90  page  33
 
 
 1761       USE MCT_COUPLER_PARAMS
 1762       USE W3WDATMD, ONLY: WLV
 1763       USE W3ADATMD, ONLY: CX, CY
 1764 !
 1765       implicit none
 1766 !
 1767       INCLUDE "mpif.h"
 1768 !
 1769 !  Imported variable declarations.
 1770 !
 1771       integer :: ng, io
 1772 !
 1773 !  Local variable declarations.
 1774 !
 1775       integer :: IP, IX, IY, grdsize
 1776       integer :: MyStatus, MyError, MySize, MyRank, Nprocs
 1777       integer :: i, id, j, gsmsize, ierr, indx, Tag
 1778       integer :: Istr, Iend, Jstr, Jend, start, length
 1779       integer :: Isize, Jsize, INDXG, OFFSET, IAPROC
 1780                                                     
 1781       real :: cff, cffmin, cffmax
 1782       real :: cff2
 1783       real, parameter              :: Large = 1.0E+20
 1784       real, dimension(2)           :: range
 1785       real, pointer                :: SND_BUF(:), RCV_BUF(:)
 1786       real(m8), pointer            :: avdata(:)
 1787 !
 1788 !-----------------------------------------------------------------------
 1789 !  Send wave fields to ROMS.
 1790 !-----------------------------------------------------------------------
 1791 !
 1792       CALL MPI_COMM_RANK (WAV_COMM_WORLD, MyRank, MyError)
 1793       CALL MPI_COMM_SIZE (WAV_COMM_WORLD, Nprocs, MyError)
 1794       IAPROC=MyRank+1
 1795       grdsize=NX*NY
 1796 !
 1797 !  Get the number of grid point on this processor.
 1798 !
 1799       gsmsize=GlobalSegMap_lsize(GlobalSegMap_G(ng)%GSMapSWAN,          &
 1800      &                           WAV_COMM_WORLD)
 1801 !
 1802 !  Allocate attribute vector array used to export/import data.
 1803 !
 1804       allocate ( avdata(gsmsize),stat=ierr )
 1805       avdata=0.0_m8
 1806       allocate ( SND_BUF(grdsize),stat=ierr )
 1807       SND_BUF=0.
 1808       allocate ( RCV_BUF(grdsize),stat=ierr )
 1809       RCV_BUF=0.
 1810 !
 1811 !-----------------------------------------------------------------------
 1812 !  Receive from ROMS: Depth, Water Level, VELX, and VELY.
 1813 !-----------------------------------------------------------------------
 1814 !
 1815 !  Schedule receiving field from ocean model.
 
 
                                                         cwstwvcp.F90  page  34
 
 
 1816 !
 1817       Tag=io*100+0*10+ng
 1818       CALL MCT_irecv (AttrVect_G(ng)%ocn2wav_AV,                        &
 1819      &                Router_O(ng,io)%SWANtoROMS, Tag)
 1820 !
 1821 !     Wait to make sure the OCN data has arrived.
 1822 !
 1823       CALL MCT_waitr (AttrVect_G(ng)%ocn2wav_AV,                        &
 1824      &                 Router_O(ng,io)%SWANtoROMS)
 1825 !
 1826       IF (MyRank.EQ.0) THEN
 1827         WRITE (SCREEN,35) ' == WW3 grid ',ng,                           &
 1828      &                    ' recv data from ROMS grid ', io
 1829       END IF
 1830       IF (MyError.ne.0) THEN
 1831         WRITE (*,*) 'coupling fail ww3cplr, MyStatus= ', MyError
 1832         CALL FINALIZE_WAV_COUPLING(ng)
 1833       END IF
 1834  35   FORMAT (a14,i2,a26,i2)
 1835 !
 1836 ! Compute local non-halo data size.
 1837 !
 1838       Jsize=INT(NY/Nprocs)
 1839       IF (MyRank.eq.Nprocs-1) THEN
 1840         Jsize=NY-Jsize*(Nprocs-1)
 1841       ENDIF
 1842       start=(MyRank*INT(NY/Nprocs))*NX+1
 1843       length=Jsize*NX
 1844 !
 1845  40   FORMAT (a36,1x,2(1pe14.6))
 1846 !
 1847 !  Bottom elevation....................................................
 1848 !
 1849         CALL AttrVect_exportRAttr (AttrVect_G(ng)%ocn2wav_AV,           &
 1850      &                             "DEPTH",avdata,gsmsize)
 1851         range(1)= Large
 1852         range(2)=-Large
 1853         SND_BUF=0.0
 1854         IP=0
 1855         DO i=start,start+length-1
 1856           IP=IP+1
 1857           range(1)=MIN(range(1),REAL(avdata(IP)))
 1858           range(2)=MAX(range(2),REAL(avdata(IP)))
 1859           SND_BUF(i)=REAL(avdata(IP))
 1860         END DO
 1861         CALL MPI_ALLREDUCE(range(1), cffmin, 1, MPI_REAL,               &
 1862                            MPI_SUM, WAV_COMM_WORLD, MyError)
 1863         CALL MPI_ALLREDUCE(range(2), cffmax, 1, MPI_REAL,               &
 1864                            MPI_SUM, WAV_COMM_WORLD, MyError)
 1865         IF (MyRank.eq.0) THEN
 1866           write(SCREEN,40) 'ROMStoWW3 Min/Max DEPTH   (m):     ',       &
 1867      &                      cffmin, cffmax
 1868         END IF
 1869 !
 1870 !  now scatter data to all nodes.
 
 
                                                         cwstwvcp.F90  page  35
 
 
 1871 !
 1872         CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                   &
 1873      &                     MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1874 !
 1875 !  Scatter to array Depth.
 1876 !
 1877         IF (MyRank.eq.0) THEN
 1878           write(SCREEN,*) 'Depth from ROMS to WW3 not coded yet.'
 1879         END IF
 1880 !
 1881 !  Water surface elevation............................................
 1882 !
 1883         CALL AttrVect_exportRAttr (AttrVect_G(ng)%ocn2wav_AV,           &
 1884      &                             "WLEV",avdata,gsmsize)
 1885         range(1)= Large
 1886         range(2)=-Large
 1887         SND_BUF=0.0
 1888         IP=0
 1889         DO i=start,start+length-1
 1890           IP=IP+1
 1891           range(1)=MIN(range(1),REAL(avdata(IP)))
 1892           range(2)=MAX(range(2),REAL(avdata(IP)))
 1893           SND_BUF(i)=REAL(avdata(IP))
 1894         END DO
 1895         CALL MPI_ALLREDUCE(range(1), cffmin, 1, MPI_REAL,               &
 1896                            MPI_SUM, WAV_COMM_WORLD, MyError)
 1897         CALL MPI_ALLREDUCE(range(2), cffmax, 1, MPI_REAL,               &
 1898                            MPI_SUM, WAV_COMM_WORLD, MyError)
 1899         IF (MyRank.eq.0) THEN
 1900           write(SCREEN,40) 'ROMStoWW3 Min/Max WLEV    (m):     ',       &
 1901      &                      cffmin, cffmax
 1902         END IF
 1903 !
 1904 !  now scatter data to all nodes.
 1905 !
 1906         CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                   &
 1907      &                     MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1908 !
 1909 !  Scatter to array WLV.
 1910 !
 1911        DO i=1,NSEA
 1912          IX     = MAPSF(i,1)
 1913          IY     = MAPSF(i,2)
 1914          IP=(IY-1)*NX+IX
 1915          IF (io.eq.1) THEN
 1916            WLV(i)=RCV_BUF(IP)
 1917          ELSE
 1918            WLV(i)=WLV(i)+RCV_BUF(IP)
 1919          END IF
 1920        END DO
 1921 !
 1922 !  Depth-integrated u-velocity........................................
 1923 !
 1924         CALL AttrVect_exportRAttr (AttrVect_G(ng)%ocn2wav_AV,           &
 1925      &                             "VELX",avdata,gsmsize)
 
 
                                                         cwstwvcp.F90  page  36
 
 
 1926         range(1)= Large
 1927         range(2)=-Large
 1928         SND_BUF=0.0
 1929         IP=0
 1930         DO i=start,start+length-1
 1931           IP=IP+1
 1932           range(1)=MIN(range(1),REAL(avdata(IP)))
 1933           range(2)=MAX(range(2),REAL(avdata(IP)))
 1934           SND_BUF(i)=REAL(avdata(IP))
 1935         END DO
 1936         CALL MPI_ALLREDUCE(range(1), cffmin, 1, MPI_REAL,               &
 1937                            MPI_SUM, WAV_COMM_WORLD, MyError)
 1938         CALL MPI_ALLREDUCE(range(2), cffmax, 1, MPI_REAL,               &
 1939                            MPI_SUM, WAV_COMM_WORLD, MyError)
 1940         IF (MyRank.eq.0) THEN
 1941           write(SCREEN,40) 'ROMStoWW3 Min/Max VELX    (ms-1):     ',    &
 1942      &                      cffmin, cffmax
 1943         END IF
 1944 !
 1945 !  now scatter data to all nodes.
 1946 !
 1947         CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                   &
 1948      &                     MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1949 !
 1950 !  Scatter to array VELX.
 1951 !
 1952        DO i=1,NSEA
 1953          IX     = MAPSF(i,1)
 1954          IY     = MAPSF(i,2)
 1955          IP=(IY-1)*NX+IX
 1956          IF (io.eq.1) THEN
 1957            CX(i)=RCV_BUF(IP)
 1958          ELSE
 1959            CX(i)=CX(i)+RCV_BUF(IP)
 1960          END IF
 1961        END DO
 1962 !
 1963 !  Depth-integrated v-velocity........................................
 1964 !
 1965         CALL AttrVect_exportRAttr (AttrVect_G(ng)%ocn2wav_AV,           &
 1966      &                             "VELY",avdata,gsmsize)
 1967         range(1)= Large
 1968         range(2)=-Large
 1969         SND_BUF=0.0
 1970         IP=0
 1971         DO i=start,start+length-1
 1972           IP=IP+1
 1973           range(1)=MIN(range(1),REAL(avdata(IP)))
 1974           range(2)=MAX(range(2),REAL(avdata(IP)))
 1975           SND_BUF(i)=REAL(avdata(IP))
 1976         END DO
 1977         CALL MPI_ALLREDUCE(range(1), cffmin, 1, MPI_REAL,               &
 1978                            MPI_SUM, WAV_COMM_WORLD, MyError)
 1979         CALL MPI_ALLREDUCE(range(2), cffmax, 1, MPI_REAL,               &
 1980                            MPI_SUM, WAV_COMM_WORLD, MyError)
 
 
                                                         cwstwvcp.F90  page  37
 
 
 1981         IF (MyRank.eq.0) THEN
 1982           write(SCREEN,40) 'ROMStoWW3 Min/Max VELY    (ms-1):     ',    &
 1983      &                      cffmin, cffmax
 1984         END IF
 1985 !
 1986 !  now scatter data to all nodes.
 1987 !
 1988         CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                   &
 1989      &                     MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 1990 !
 1991 !  Scatter to array VELY.
 1992 !
 1993        DO i=1,NSEA
 1994          IX     = MAPSF(i,1)
 1995          IY     = MAPSF(i,2)
 1996          IP=(IY-1)*NX+IX
 1997          IF (io.eq.1) THEN
 1998            CY(i)=RCV_BUF(IP)
 1999          ELSE
 2000            CY(i)=CY(i)+RCV_BUF(IP)
 2001          END IF
 2002        END DO
 2003 !
 2004 !  Bottom roughness...................................................
 2005 !
 2006         CALL AttrVect_exportRAttr(AttrVect_G(ng)%ocn2wav_AV,           &
 2007      &                            "ZO",avdata,gsmsize)
 2008         range(1)= Large
 2009         range(2)=-Large
 2010         SND_BUF=0.0
 2011         IP=0
 2012         DO i=start,start+length-1
 2013           IP=IP+1
 2014           range(1)=MIN(range(1),REAL(avdata(IP)))
 2015           range(2)=MAX(range(2),REAL(avdata(IP)))
 2016           SND_BUF(i)=REAL(avdata(IP))
 2017         END DO
 2018         CALL MPI_ALLREDUCE(range(1), cffmin, 1, MPI_REAL,               &
 2019                            MPI_SUM, WAV_COMM_WORLD, MyError)
 2020         CALL MPI_ALLREDUCE(range(2), cffmax, 1, MPI_REAL,               &
 2021                            MPI_SUM, WAV_COMM_WORLD, MyError)
 2022         IF (MyRank.eq.0) THEN
 2023           write(SCREEN,40) 'ROMStoWW3 Min/Max BottZ0  (m):     ',       &
 2024      &                      cffmin, cffmax
 2025         END IF
 2026 !
 2027 !  now scatter data to all nodes.
 2028 !
 2029         CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                   &
 2030      &                     MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 2031 !
 2032 !  Scatter to array Z0.
 2033 !
 2034         IF (MyRank.eq.0) THEN
 2035           write(SCREEN,*) 'Z0 from ROMS to WW3 not coded yet.'
 
 
                                                         cwstwvcp.F90  page  38
 
 
 2036         END IF
 2037 !
 2038       deallocate (avdata, SND_BUF, RCV_BUF)
 2039 !
 2040       RETURN
 2041       END SUBROUTINE WAVFOCN_COUPLING
 2042       SUBROUTINE WAVFATM_COUPLING (iw, ia)
 2043 !
 2044 !=======================================================================
 2045 !                                                                      !
 2046 !  This subroutine reads and writes the coupling data streams between  !
 2047 !  ocean and wave models. Currently, the following data streams are    !
 2048 !  processed:                                                          !
 2049 !                                                                      !
 2050 !                                                                      !
 2051 !  Fields imported from the ATM Model:                                 !
 2052 !                                                                      !
 2053 !     * Wind E and N (m/s)                                             !
 2054 !                                                                      !
 2055 !=======================================================================
 2056 !
 2057       USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, MAPSF
 2058       USE W3SERVMD
 2059       USE W3IDATMD
 2060       USE MCT_COUPLER_PARAMS
 2061       USE W3ADATMD, ONLY: UA, UD
 2062 !
 2063       implicit none
 2064       INCLUDE "mpif.h"
 2065 !
 2066 !  Imported variable declarations.
 2067 !
 2068       integer :: iw, ia
 2069 !
 2070 !  Local variable declarations.
 2071 !
 2072       integer :: IP, IX, IY, grdsize
 2073       integer :: MyStatus, MyError, MySize, MyRank, Nprocs
 2074       integer :: i, id, j, gsmsize, ierr, indx, Tag
 2075       integer :: Istr, Iend, Jstr, Jend, start, length
 2076       integer :: Isize, Jsize, INDXG, OFFSET, IAPROC
 2077       integer, pointer :: indices(:)
 2078       real :: cff, cffmin, cffmax
 2079       real, parameter    ::  Large = 1.0E+20
 2080       real, dimension(2) :: range
 2081       real, pointer      :: SND_BUF(:), RCV_BUF(:)
 2082       real, pointer      :: WND_U10(:), WND_V10(:)
 2083       real(m8), pointer  :: avdata(:)
 2084 !
 2085 !-----------------------------------------------------------------------
 2086 !  Get wind data from atm.
 2087 !-----------------------------------------------------------------------
 2088 !
 2089       CALL MPI_COMM_RANK (WAV_COMM_WORLD, MyRank, MyError)
 2090       CALL MPI_COMM_SIZE (WAV_COMM_WORLD, Nprocs, MyError)
 
 
                                                         cwstwvcp.F90  page  39
 
 
 2091       IAPROC=MyRank+1
 2092       grdsize=NX*NY
 2093 !
 2094 !  Get the number of grid point on this processor.
 2095 !
 2096       gsmsize=GlobalSegMap_lsize(GlobalSegMap_G(iw)%GSMapSWAN,          &
 2097      &                           WAV_COMM_WORLD)
 2098 !
 2099 !  Allocate attribute vector array used to export/import data.
 2100 !
 2101       allocate ( avdata(gsmsize),stat=ierr )
 2102       avdata=0.0_m8
 2103       MyError=0
 2104       allocate ( SND_BUF(grdsize),stat=ierr )
 2105       SND_BUF=0.
 2106       allocate ( RCV_BUF(grdsize),stat=ierr )
 2107       RCV_BUF=0.
 2108       allocate ( WND_U10(grdsize),stat=ierr )
 2109       WND_U10=0.
 2110       allocate ( WND_V10(grdsize),stat=ierr )
 2111       WND_V10=0.
 2112 !
 2113 !-----------------------------------------------------------------------
 2114 !  RCV the data from WRF.
 2115 !-----------------------------------------------------------------------
 2116 !
 2117  35   FORMAT (a14,i2,a24,i2)
 2118 !
 2119 !  Receive fields from atmosphere model.
 2120 !
 2121       Tag=0*100+ia*10+iw
 2122       CALL MCT_irecv (AV2_A(iw,ia)%atm2wav_AV2,                         &
 2123      &                Router_A(iw,ia)%SWANtoWRF, Tag)
 2124 !     Wait to make sure the WRF data has arrived.
 2125       CALL MCT_waitr (AV2_A(iw,ia)%atm2wav_AV2,                         &
 2126      &                Router_A(iw,ia)%SWANtoWRF)
 2127       CALL MCT_MatVecMul(AV2_A(iw,ia)%atm2wav_AV2,                      &
 2128      &                   SMPlus_G(iw,ia)%A2WMatPlus,                    &
 2129      &                   AttrVect_G(iw)%atm2wav_AV)
 2130       IF (MyRank.EQ.0) THEN
 2131         WRITE (SCREEN,35)'== WW3 grid ',iw,' recv data from WRF grid'   &
 2132      &                     ,ia
 2133       END IF
 2134       IF (MyError.ne.0) THEN
 2135         WRITE (*,*) 'coupling fail swancplr, MyStatus= ', MyError
 2136         CALL FINALIZE_WAV_COUPLING(iw)
 2137       END IF
 2138 !
 2139 ! Compute local non-halo data size.
 2140 !
 2141       Jsize=INT(NY/Nprocs)
 2142       IF (MyRank.eq.Nprocs-1) THEN
 2143         Jsize=NY-Jsize*(Nprocs-1)
 2144       ENDIF
 2145       start=(MyRank*INT(NY/Nprocs))*NX+1
 
 
                                                         cwstwvcp.F90  page  40
 
 
 2146       length=Jsize*NX
 2147 !
 2148 !  U-wind.
 2149 !
 2150       CALL AttrVect_exportRAttr(AttrVect_G(iw)%atm2wav_AV,"U10",       &
 2151      &                          avdata,gsmsize)
 2152       range(1)= Large
 2153       range(2)=-Large
 2154       SND_BUF=0.0
 2155       IP=0
 2156       DO i=start,start+length-1
 2157         IP=IP+1
 2158         range(1)=MIN(range(1),REAL(avdata(IP)))
 2159         range(2)=MAX(range(2),REAL(avdata(IP)))
 2160         SND_BUF(i)=REAL(avdata(IP))
 2161       END DO
 2162       CALL MPI_ALLREDUCE(range(1), cffmin, 1, MPI_REAL,                 &
 2163                          MPI_SUM, WAV_COMM_WORLD, MyError)
 2164       CALL MPI_ALLREDUCE(range(2), cffmax, 1, MPI_REAL,                 &
 2165                          MPI_SUM, WAV_COMM_WORLD, MyError)
 2166       IF (MyRank.eq.0) THEN
 2167         write(SCREEN,40) 'WRFtoWW3 Min/Max U10     (ms-1):     ',       &
 2168      &                      cffmin, cffmax
 2169       END IF
 2170 !
 2171 !  now scatter data to all nodes.
 2172 !
 2173       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 2174      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 2175 !
 2176 !  Scatter to array WND_U10 as temporary for now.
 2177 !
 2178       DO i=1,grdsize
 2179         IF (ia.eq.1) THEN
 2180           WND_U10(i)=RCV_BUF(i)
 2181         ELSE
 2182           WND_U10(i)=WND_U10(i)+RCV_BUF(i)
 2183         END IF
 2184       END DO
 2185 !     DO i=1,NSEA
 2186 !       IX     = MAPSF(i,1)
 2187 !       IY     = MAPSF(i,2)
 2188 !       IP=(IY-1)*NX+IX
 2189 !       WND_SPD(i)=RCV_BUF(IP)
 2190 !     END DO
 2191 !
 2192 !  V-wind.
 2193 !
 2194       CALL AttrVect_exportRAttr(AttrVect_G(iw)%atm2wav_AV,"V10",        &
 2195      &                          avdata,gsmsize)
 2196       range(1)= Large
 2197       range(2)=-Large
 2198       SND_BUF=0.0
 2199       IP=0
 2200       DO i=start,start+length-1
 
 
                                                         cwstwvcp.F90  page  41
 
 
 2201         IP=IP+1
 2202         range(1)=MIN(range(1),REAL(avdata(IP)))
 2203         range(2)=MAX(range(2),REAL(avdata(IP)))
 2204        SND_BUF(i)=REAL(avdata(IP))
 2205       END DO
 2206       CALL MPI_ALLREDUCE(range(1), cffmin, 1, MPI_REAL,                 &
 2207                          MPI_SUM, WAV_COMM_WORLD, MyError)
 2208       CALL MPI_ALLREDUCE(range(2), cffmax, 1, MPI_REAL,                 &
 2209                          MPI_SUM, WAV_COMM_WORLD, MyError)
 2210       IF (MyRank.eq.0) THEN
 2211         write(SCREEN,40) 'WRFtoWW3 Min/Max V10     (ms-1):     ',       &
 2212      &                    cffmin, cffmax
 2213       END IF
 2214 !
 2215 !  now scatter data to all nodes.
 2216 !
 2217       CALL MPI_ALLREDUCE(SND_BUF, RCV_BUF, grdsize,                     &
 2218      &                   MPI_REAL, MPI_SUM, WAV_COMM_WORLD, MyError)
 2219 !
 2220 !  Scatter to array WND_V10 as temporary for now.
 2221 !
 2222       DO i=1,grdsize
 2223         IF (ia.eq.1) THEN
 2224           WND_V10(i)=RCV_BUF(i)
 2225         ELSE
 2226           WND_V10(i)=WND_V10(i)+RCV_BUF(i)
 2227         END IF
 2228       END DO
 2229 !
 2230 !  Now we need to combine wnd speed and dir and scatter.
 2231 !
 2232       DO i=1,NSEA
 2233         IX     = MAPSF(i,1)
 2234         IY     = MAPSF(i,2)
 2235         IP=(IY-1)*NX+IX
 2236         IF (ia.eq.1) THEN
 2237           UA(i)=SQRT(WND_U10(IP)**2+WND_V10(IP)**2+0.000001)
 2238           UD(i)=ATAN2(WND_V10(IP),WND_U10(IP))
 2239         ELSE
 2240           UA(i)=UA(i)+SQRT(WND_U10(IP)**2+WND_V10(IP)**2+0.000001)
 2241           UD(i)=UD(i)+ATAN2(WND_V10(IP),WND_U10(IP))
 2242         END IF
 2243       END DO
 2244  40     FORMAT (a36,1x,2(1pe14.6))
 2245 !
 2246       deallocate (avdata)
 2247       deallocate (SND_BUF, RCV_BUF)
 2248       deallocate (WND_U10, WND_V10)
 2249       if (associated (indices)) then
 2250         deallocate (indices)
 2251       endif
 2252 !
 2253       RETURN
 2254       END SUBROUTINE WAVFATM_COUPLING
 2255                                      
 
 
                                                         cwstwvcp.F90  page  42
 
 
 2256       SUBROUTINE FINALIZE_WAV_COUPLING(iw)
 2257 !
 2258 !=======================================================================
 2259 !                                                                    ===
 2260 !  This routines terminates execution during coupling error.         ===
 2261 !                                                                    ===
 2262 !=======================================================================
 2263       USE MCT_COUPLER_PARAMS
 2264 !
 2265 !  Local variable declarations.
 2266 !
 2267       integer :: iw, io, ia, MyError
 2268 !
 2269 !-----------------------------------------------------------------------
 2270 !  Deallocate MCT environment.
 2271 !-----------------------------------------------------------------------
 2272 !
 2273       DO io=1,Nocn_grids
 2274         CALL Router_clean (Router_O(iw,io)%SWANtoROMS, MyError)
 2275       END DO
 2276       CALL AttrVect_clean (AttrVect_G(iw)%wav2ocn_AV, MyError)
 2277       DO ia=1,Natm_grids
 2278         CALL Router_clean (Router_A(iw,ia)%SWANtoWRF, MyError)
 2279       END DO
 2280       CALL AttrVect_clean (AttrVect_G(iw)%atm2wav_AV, MyError)
 2281       CALL GlobalSegMap_clean (GlobalSegMap_G(iw)%GSMapSWAN, MyError)
 2282                                                                      
 2283       END SUBROUTINE FINALIZE_WAV_COUPLING
 2284       END MODULE CWSTWVCP
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
