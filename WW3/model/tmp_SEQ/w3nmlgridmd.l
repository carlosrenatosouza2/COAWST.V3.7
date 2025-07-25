                                                      w3nmlgridmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3NMLGRIDMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           M. Accensi              |
    8 !/                  |                                   |
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :         15-May-2018 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    For updates see subroutines.
   14 !/
   15 !  1. Purpose :
   16 !
   17 !     Manages namelists from configuration file ww3_grid.nml for ww3_grid>
   18 !
   19 !/ ------------------------------------------------------------------- /
   20                                                                         
   21   ! module defaults
   22   IMPLICIT NONE
   23                
   24   PUBLIC
   25         
   26   ! coord file structure
   27   TYPE NML_COORD_FILE_T
   28     REAL                        :: SF
   29     REAL                        :: OFF
   30     CHARACTER(256)              :: FILENAME
   31     INTEGER                     :: IDF
   32     INTEGER                     :: IDLA
   33     INTEGER                     :: IDFM
   34     CHARACTER(256)              :: FORMAT
   35     CHARACTER(256)              :: FROM
   36   END TYPE NML_COORD_FILE_T
   37                            
   38   ! file structure
   39   TYPE NML_FILE_T
   40     REAL                        :: SF
   41     CHARACTER(256)              :: FILENAME
   42     INTEGER                     :: IDF
   43     INTEGER                     :: IDLA
   44     INTEGER                     :: IDFM
   45     CHARACTER(256)              :: FORMAT
   46     CHARACTER(256)              :: FROM
   47   END TYPE NML_FILE_T
   48                      
   49   ! smc file structure
   50   TYPE NML_SMC_FILE_T
   51     CHARACTER(256)              :: FILENAME
   52     INTEGER                     :: IDF
   53     INTEGER                     :: IDLA
   54     INTEGER                     :: IDFM
   55     CHARACTER(256)              :: FORMAT
 
 
                                                      w3nmlgridmd.F90  page   2
 
 
   56   END TYPE NML_SMC_FILE_T
   57                          
   58                          
   59   ! spectrum structure
   60   TYPE NML_SPECTRUM_T
   61     REAL                        :: XFR
   62     REAL                        :: FREQ1
   63     INTEGER                     :: NK
   64     INTEGER                     :: NTH
   65     REAL                        :: THOFF
   66   END TYPE NML_SPECTRUM_T
   67                          
   68   ! run structure
   69   TYPE NML_RUN_T
   70     LOGICAL                     :: FLDRY
   71     LOGICAL                     :: FLCX
   72     LOGICAL                     :: FLCY
   73     LOGICAL                     :: FLCTH
   74     LOGICAL                     :: FLCK
   75     LOGICAL                     :: FLSOU
   76   END TYPE NML_RUN_T
   77                     
   78   ! timesteps structure
   79   TYPE NML_TIMESTEPS_T
   80     REAL                        :: DTMAX
   81     REAL                        :: DTXY
   82     REAL                        :: DTKTH
   83     REAL                        :: DTMIN
   84   END TYPE NML_TIMESTEPS_T
   85                           
   86   ! grid structure
   87   TYPE NML_GRID_T
   88     CHARACTER(256)              :: NAME
   89     CHARACTER(256)              :: NML
   90     CHARACTER(256)              :: TYPE
   91     CHARACTER(256)              :: COORD
   92     CHARACTER(256)              :: CLOS
   93     REAL                        :: ZLIM
   94     REAL                        :: DMIN
   95   END TYPE NML_GRID_T
   96                      
   97   ! rect structure
   98   TYPE NML_RECT_T
   99     INTEGER                     :: NX
  100     INTEGER                     :: NY
  101     REAL                        :: SX
  102     REAL                        :: SY
  103     REAL                        :: SF
  104     REAL                        :: X0
  105     REAL                        :: Y0
  106     REAL                        :: SF0
  107   END TYPE NML_RECT_T
  108                      
  109   ! curv structure
  110   TYPE NML_CURV_T
 
 
                                                      w3nmlgridmd.F90  page   3
 
 
  111     INTEGER                     :: NX
  112     INTEGER                     :: NY
  113     TYPE(NML_COORD_FILE_T)      :: XCOORD
  114     TYPE(NML_COORD_FILE_T)      :: YCOORD
  115   END TYPE NML_CURV_T
  116                      
  117   ! unst structure
  118   TYPE NML_UNST_T
  119     REAL                        :: SF
  120     CHARACTER(256)              :: FILENAME
  121     INTEGER                     :: IDF
  122     INTEGER                     :: IDLA
  123     INTEGER                     :: IDFM
  124     CHARACTER(256)              :: FORMAT
  125     CHARACTER(256)              :: UGOBCFILE
  126   END TYPE NML_UNST_T
  127                      
  128   ! smc structure
  129   TYPE NML_SMC_T
  130     TYPE(NML_SMC_FILE_T)        :: MCELS
  131     TYPE(NML_SMC_FILE_T)        :: ISIDE
  132     TYPE(NML_SMC_FILE_T)        :: JSIDE
  133     TYPE(NML_SMC_FILE_T)        :: SUBTR
  134     TYPE(NML_SMC_FILE_T)        :: BUNDY
  135   END TYPE NML_SMC_T
  136                     
  137   ! depth structure
  138   TYPE NML_DEPTH_T
  139     REAL                        :: SF
  140     CHARACTER(256)              :: FILENAME
  141     INTEGER                     :: IDF
  142     INTEGER                     :: IDLA
  143     INTEGER                     :: IDFM
  144     CHARACTER(256)              :: FORMAT
  145     CHARACTER(256)              :: FROM
  146   END TYPE NML_DEPTH_T
  147                       
  148   ! mask structure
  149   TYPE NML_MASK_T
  150     REAL                        :: SF
  151     CHARACTER(256)              :: FILENAME
  152     INTEGER                     :: IDF
  153     INTEGER                     :: IDLA
  154     INTEGER                     :: IDFM
  155     CHARACTER(256)              :: FORMAT
  156     CHARACTER(256)              :: FROM
  157   END TYPE NML_MASK_T
  158                      
  159   ! obst structure
  160   TYPE NML_OBST_T
  161     REAL                        :: SF
  162     CHARACTER(256)              :: FILENAME
  163     INTEGER                     :: IDF
  164     INTEGER                     :: IDLA
  165     INTEGER                     :: IDFM
 
 
                                                      w3nmlgridmd.F90  page   4
 
 
  166     CHARACTER(256)              :: FORMAT
  167     CHARACTER(256)              :: FROM
  168   END TYPE NML_OBST_T
  169                      
  170   ! slope structure
  171   TYPE NML_SLOPE_T
  172     REAL                        :: SF
  173     CHARACTER(256)              :: FILENAME
  174     INTEGER                     :: IDF
  175     INTEGER                     :: IDLA
  176     INTEGER                     :: IDFM
  177     CHARACTER(256)              :: FORMAT
  178     CHARACTER(256)              :: FROM
  179   END TYPE NML_SLOPE_T
  180                       
  181   ! sed structure
  182   TYPE NML_SED_T
  183     REAL                        :: SF
  184     CHARACTER(256)              :: FILENAME
  185     INTEGER                     :: IDF
  186     INTEGER                     :: IDLA
  187     INTEGER                     :: IDFM
  188     CHARACTER(256)              :: FORMAT
  189     CHARACTER(256)              :: FROM
  190   END TYPE NML_SED_T
  191                     
  192   ! inbound structure
  193   TYPE NML_INBND_COUNT_T
  194     INTEGER                     :: N_POINT
  195   END TYPE NML_INBND_COUNT_T
  196                             
  197   TYPE NML_INBND_POINT_T
  198     INTEGER                     :: X_INDEX
  199     INTEGER                     :: Y_INDEX
  200     LOGICAL                     :: CONNECT
  201   END TYPE NML_INBND_POINT_T
  202                             
  203                             
  204   ! excluded structure
  205   TYPE NML_EXCL_COUNT_T
  206     INTEGER                     :: N_POINT
  207     INTEGER                     :: N_BODY
  208   END TYPE NML_EXCL_COUNT_T
  209                            
  210   TYPE NML_EXCL_POINT_T
  211     INTEGER                     :: X_INDEX
  212     INTEGER                     :: Y_INDEX
  213     LOGICAL                     :: CONNECT
  214   END TYPE NML_EXCL_POINT_T
  215                            
  216   TYPE NML_EXCL_BODY_T
  217     INTEGER                     :: X_INDEX
  218     INTEGER                     :: Y_INDEX
  219   END TYPE NML_EXCL_BODY_T
  220                           
 
 
                                                      w3nmlgridmd.F90  page   5
 
 
  221                           
  222   ! outbound structure
  223   TYPE NML_OUTBND_COUNT_T
  224     INTEGER                     :: N_LINE
  225   END TYPE NML_OUTBND_COUNT_T
  226                              
  227   TYPE NML_OUTBND_LINE_T
  228     REAL                        :: X0
  229     REAL                        :: Y0
  230     REAL                        :: DX
  231     REAL                        :: DY
  232     INTEGER                     :: NP
  233   END TYPE NML_OUTBND_LINE_T
  234                             
  235                             
  236                             
  237   ! miscellaneous
  238   CHARACTER(256)                :: MSG
  239   INTEGER                       :: NDSN
  240                                        
  241                                        
  242                                        
  243   CONTAINS
  244 !/ ------------------------------------------------------------------- /
  245   SUBROUTINE W3NMLGRID (NDSI, INFILE, NML_SPECTRUM, NML_RUN,           &
  246                         NML_TIMESTEPS, NML_GRID, NML_RECT, NML_CURV,   &
  247                         NML_UNST, NML_SMC, NML_DEPTH, NML_MASK,        &
  248                         NML_OBST, NML_SLOPE, NML_SED, NML_INBND_COUNT, &
  249                         NML_INBND_POINT, NML_EXCL_COUNT,               &
  250                         NML_EXCL_POINT, NML_EXCL_BODY,                 &
  251                         NML_OUTBND_COUNT, NML_OUTBND_LINE, IERR)
  252 !/
  253 !/                  +-----------------------------------+
  254 !/                  | WAVEWATCH III           NOAA/NCEP |
  255 !/                  |           M. Accensi              |
  256 !/                  |                                   |
  257 !/                  |                        FORTRAN 90 |
  258 !/                  | Last update :         15-May-2018 |
  259 !/                  +-----------------------------------+
  260 !/
  261 !
  262 !  1. Purpose :
  263 !
  264 !     Reads all the namelist to define the model grid
  265 !
  266 !  2. Method :
  267 !
  268 !     See source term routines.
  269 !
  270 !  3. Parameters :
  271 !
  272 !     Parameter list
  273 !     ----------------------------------------------------------------
  274 !      NDSI              Int.
  275 !      INFILE            Char.
 
 
                                                      w3nmlgridmd.F90  page   6
 
 
  276 !      NML_SPECTRUM      Type
  277 !      NML_RUN           Type
  278 !      NML_TIMESTEPS     Type
  279 !      NML_GRID          Type
  280 !      NML_RECT          Type
  281 !      NML_CURV          Type
  282 !      NML_UNST          Type
  283 !      NML_SMC           Type
  284 !      NML_DEPTH         Type
  285 !      NML_MASK          Type
  286 !      NML_OBST          Type
  287 !      NML_SLOPE         Type
  288 !      NML_SED           Type
  289 !      NML_INBND_COUNT   Type
  290 !      NML_INBND_POINT   Type
  291 !      NML_EXCL_COUNT    Type
  292 !      NML_EXCL_POINT    Type
  293 !      NML_EXCL_BODY     Type
  294 !      NML_OUTBND_COUNT  Type
  295 !      NML_OUTBND_LINE   Type
  296 !      IERR              Int.
  297 !     ----------------------------------------------------------------
  298 !
  299 !  4. Subroutines used :
  300 !
  301 !      Name      TYPE  Module   Description
  302 !     ----------------------------------------------------------------
  303 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  304 !      READ_SPECTRUM_NML
  305 !      REPORT_SPECTRUM_NML
  306 !      READ_RUN_NML
  307 !      REPORT_RUN_NML
  308 !      READ_TIMESTEPS_NML
  309 !      REPORT_TIMESTEPS_NML
  310 !      READ_GRID_NML
  311 !      REPORT_GRID_NML
  312 !      READ_RECT_NML
  313 !      REPORT_RECT_NML
  314 !      READ_CURV_NML
  315 !      REPORT_CURV_NML
  316 !      READ_UNST_NML
  317 !      REPORT_UNST_NML
  318 !      READ_SMC_NML
  319 !      REPORT_SMC_NML
  320 !      READ_DEPTH_NML
  321 !      REPORT_DEPTH_NML
  322 !      READ_MASK_NML
  323 !      REPORT_MASK_NML
  324 !      READ_OBST_NML
  325 !      REPORT_OBST_NML
  326 !      READ_SLOPE_NML
  327 !      REPORT_SLOPE_NML
  328 !      READ_SED_NML
  329 !      REPORT_SED_NML
  330 !      READ_INBOUND_NML
 
 
                                                      w3nmlgridmd.F90  page   7
 
 
  331 !      REPORT_INBOUND_NML
  332 !      READ_EXCLUDED_NML
  333 !      REPORT_EXCLUDED_NML
  334 !      READ_OUTBOUND_NML
  335 !      REPORT_OUTBOUND_NML
  336 !     ----------------------------------------------------------------
  337 !
  338 !  5. Called by :
  339 !
  340 !      Name      TYPE  Module   Description
  341 !     ----------------------------------------------------------------
  342 !      WW3_GRID  Prog.   N/A    Preprocess model grid.
  343 !     ----------------------------------------------------------------
  344 !
  345 !  6. Error messages :
  346 !
  347 !     None.
  348 !
  349 !  7. Remarks :
  350 !
  351 !  8. Structure :
  352 !
  353 !     See source code.
  354 !
  355 !  9. Switches :
  356 !
  357 ! 10. Source code :
  358 !
  359 !/ ------------------------------------------------------------------- /
  360                                                                         
  361     USE W3ODATMD, ONLY: NDSE
  362                             
  363     IMPLICIT NONE
  364                  
  365     INTEGER, INTENT(IN)                         :: NDSI
  366     CHARACTER*(*), INTENT(IN)                   :: INFILE
  367     TYPE(NML_SPECTRUM_T), INTENT(INOUT)         :: NML_SPECTRUM
  368     TYPE(NML_RUN_T), INTENT(INOUT)              :: NML_RUN
  369     TYPE(NML_TIMESTEPS_T), INTENT(INOUT)        :: NML_TIMESTEPS
  370     TYPE(NML_GRID_T), INTENT(INOUT)             :: NML_GRID
  371     TYPE(NML_RECT_T), INTENT(INOUT)             :: NML_RECT
  372     TYPE(NML_CURV_T), INTENT(INOUT)             :: NML_CURV
  373     TYPE(NML_UNST_T), INTENT(INOUT)             :: NML_UNST
  374     TYPE(NML_SMC_T), INTENT(INOUT)              :: NML_SMC
  375     TYPE(NML_DEPTH_T), INTENT(INOUT)            :: NML_DEPTH
  376     TYPE(NML_MASK_T), INTENT(INOUT)             :: NML_MASK
  377     TYPE(NML_OBST_T), INTENT(INOUT)             :: NML_OBST
  378     TYPE(NML_SLOPE_T), INTENT(INOUT)            :: NML_SLOPE
  379     TYPE(NML_SED_T), INTENT(INOUT)              :: NML_SED
  380     TYPE(NML_INBND_COUNT_T), INTENT(INOUT)      :: NML_INBND_COUNT
  381     TYPE(NML_INBND_POINT_T), ALLOCATABLE, INTENT(INOUT)      :: NML_INBND>
  382     TYPE(NML_EXCL_COUNT_T), INTENT(INOUT)       :: NML_EXCL_COUNT
  383     TYPE(NML_EXCL_POINT_T), ALLOCATABLE, INTENT(INOUT)       :: NML_EXCL_>
  384     TYPE(NML_EXCL_BODY_T), ALLOCATABLE, INTENT(INOUT)        :: NML_EXCL_>
  385     TYPE(NML_OUTBND_COUNT_T), INTENT(INOUT)     :: NML_OUTBND_COUNT
 
 
                                                      w3nmlgridmd.F90  page   8
 
 
  386     TYPE(NML_OUTBND_LINE_T), ALLOCATABLE, INTENT(INOUT)      :: NML_OUTBN>
  387     INTEGER, INTENT(OUT)                        :: IERR
  388                                                        
  389     IERR = 0
  390             
  391     ! open namelist log file
  392     NDSN = 3
  393     OPEN (NDSN, file=TRIM(INFILE)//'.log', form='formatted', iostat=IERR)
  394        IF (IERR.NE.0) THEN
  395       WRITE (NDSE,'(A)') 'ERROR: open full nml file '//TRIM(INFILE)//'.lo>
  396       RETURN
  397     END IF
  398           
  399     ! open input file
  400     OPEN (NDSI, file=TRIM(INFILE), form='formatted', status='old', iostat>
  401     IF (IERR.NE.0) THEN
  402       WRITE (NDSE,'(A)') 'ERROR: open input file '//TRIM(INFILE)//' faile>
  403       RETURN
  404     END IF
  405           
  406     ! read spectrum namelist
  407     CALL READ_SPECTRUM_NML (NDSI, NML_SPECTRUM)
  408     CALL REPORT_SPECTRUM_NML (NML_SPECTRUM)
  409                                            
  410     ! read run namelist
  411     CALL READ_RUN_NML (NDSI, NML_RUN)
  412     CALL REPORT_RUN_NML (NML_RUN)
  413                                  
  414     ! read timesteps namelist
  415     CALL READ_TIMESTEPS_NML (NDSI, NML_TIMESTEPS)
  416     CALL REPORT_TIMESTEPS_NML (NML_TIMESTEPS)
  417                                              
  418     ! read grid namelist
  419     CALL READ_GRID_NML (NDSI, NML_GRID)
  420     CALL REPORT_GRID_NML (NML_GRID)
  421                                    
  422     ! read rect namelist
  423     CALL READ_RECT_NML (NDSI, NML_RECT)
  424     CALL REPORT_RECT_NML (NML_RECT)
  425                                    
  426     ! read curv namelist
  427     CALL READ_CURV_NML (NDSI, NML_CURV)
  428     CALL REPORT_CURV_NML (NML_CURV)
  429                                    
  430     ! read unst namelist
  431     CALL READ_UNST_NML (NDSI, NML_UNST)
  432     CALL REPORT_UNST_NML (NML_UNST)
  433                                    
  434     ! read smc namelist
  435     CALL READ_SMC_NML (NDSI, NML_SMC)
  436     CALL REPORT_SMC_NML (NML_SMC)
  437                                  
  438     ! read depth namelist
  439     CALL READ_DEPTH_NML (NDSI, NML_DEPTH)
  440     CALL REPORT_DEPTH_NML (NML_DEPTH)
 
 
                                                      w3nmlgridmd.F90  page   9
 
 
  441                                      
  442     ! read mask namelist
  443     CALL READ_MASK_NML (NDSI, NML_MASK)
  444     CALL REPORT_MASK_NML (NML_MASK)
  445                                    
  446     ! read obst namelist
  447     CALL READ_OBST_NML (NDSI, NML_OBST)
  448     CALL REPORT_OBST_NML (NML_OBST)
  449                                    
  450     ! read slope namelist
  451     CALL READ_SLOPE_NML (NDSI, NML_SLOPE)
  452     CALL REPORT_SLOPE_NML (NML_SLOPE)
  453                                      
  454     ! read sed namelist
  455     CALL READ_SED_NML (NDSI, NML_SED)
  456     CALL REPORT_SED_NML (NML_SED)
  457                                  
  458     ! read inbound namelist
  459     CALL READ_INBOUND_NML (NDSI, NML_INBND_COUNT, NML_INBND_POINT)
  460     CALL REPORT_INBOUND_NML (NML_INBND_COUNT, NML_INBND_POINT)
  461                                                               
  462     ! read excluded namelist
  463     CALL READ_EXCLUDED_NML (NDSI, NML_EXCL_COUNT, NML_EXCL_POINT, NML_EXC>
  464     CALL REPORT_EXCLUDED_NML (NML_EXCL_COUNT, NML_EXCL_POINT, NML_EXCL_BO>
  465                                                                           
  466     ! read outbound namelist
  467     CALL READ_OUTBOUND_NML (NDSI, NML_OUTBND_COUNT, NML_OUTBND_LINE)
  468     CALL REPORT_OUTBOUND_NML (NML_OUTBND_COUNT, NML_OUTBND_LINE)
  469                                                                 
  470     ! close namelist files
  471     CLOSE (NDSI)
  472     CLOSE (NDSN)
  473                 
  474   END SUBROUTINE W3NMLGRID
  475                           
  476                           
  477 !/ ------------------------------------------------------------------- /
  478                                                                         
  479                                                                         
  480                                                                         
  481                                                                         
  482                                                                         
  483                                                                         
  484 !/ ------------------------------------------------------------------- /
  485                                                                         
  486   SUBROUTINE READ_SPECTRUM_NML (NDSI, NML_SPECTRUM)
  487 !/
  488 !/                  +-----------------------------------+
  489 !/                  | WAVEWATCH III           NOAA/NCEP |
  490 !/                  |           M. Accensi              |
  491 !/                  |                                   |
  492 !/                  |                        FORTRAN 90 |
  493 !/                  | Last update :         15-May-2018 |
  494 !/                  +-----------------------------------+
  495 !/
 
 
                                                      w3nmlgridmd.F90  page  10
 
 
  496 !  1. Purpose :
  497 !
  498 !  2. Method :
  499 !
  500 !     See source term routines.
  501 !
  502 !  3. Parameters :
  503 !
  504 !     Parameter list
  505 !     ----------------------------------------------------------------
  506 !      NDSI            Int.
  507 !      NML_SPECTRUM    Type.
  508 !     ----------------------------------------------------------------
  509 !
  510 !  4. Subroutines used :
  511 !
  512 !      Name      TYPE  Module   Description
  513 !     ----------------------------------------------------------------
  514 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  515 !     ----------------------------------------------------------------
  516 !
  517 !  5. Called by :
  518 !
  519 !      Name      TYPE  Module   Description
  520 !     ----------------------------------------------------------------
  521 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
  522 !     ----------------------------------------------------------------
  523 !
  524 !  6. Error messages :
  525 !
  526 !     None.
  527 !
  528 !  7. Remarks :
  529 !
  530 !  8. Structure :
  531 !
  532 !     See source code.
  533 !
  534 !  9. Switches :
  535 !
  536 ! 10. Source code :
  537 !
  538 !/ ------------------------------------------------------------------- /
  539                                                                         
  540     USE W3ODATMD, ONLY: NDSE
  541     USE W3SERVMD, ONLY: EXTCDE
  542                               
  543     IMPLICIT NONE
  544                  
  545     INTEGER, INTENT(IN)                 :: NDSI
  546     TYPE(NML_SPECTRUM_T), INTENT(INOUT) :: NML_SPECTRUM
  547                                                        
  548     ! locals
  549     INTEGER                   :: IERR
  550     TYPE(NML_SPECTRUM_T) :: SPECTRUM
 
 
                                                      w3nmlgridmd.F90  page  11
 
 
  551     NAMELIST /SPECTRUM_NML/ SPECTRUM
  552                                     
  553     IERR = 0
  554             
  555     ! set default values for spectrum structure
  556     SPECTRUM%XFR        = 0.
  557     SPECTRUM%FREQ1      = 0.
  558     SPECTRUM%NK         = 0
  559     SPECTRUM%NTH        = 0
  560     SPECTRUM%THOFF      = 0.
  561                             
  562     ! read spectrum namelist
  563     REWIND (NDSI)
  564     READ (NDSI, nml=SPECTRUM_NML, iostat=IERR, iomsg=MSG)
  565     IF (IERR.NE.0) THEN
  566       WRITE (NDSE,'(A,/A)') &
  567         'ERROR: READ_SPECTRUM_NML: namelist read error', &
  568         'ERROR: '//TRIM(MSG)
  569       CALL EXTCDE (1)
  570     END IF
  571           
  572     ! save namelist
  573     NML_SPECTRUM = SPECTRUM
  574                            
  575   END SUBROUTINE READ_SPECTRUM_NML
  576                                   
  577 !/ ------------------------------------------------------------------- /
  578                                                                         
  579                                                                         
  580                                                                         
  581 !/ ------------------------------------------------------------------- /
  582                                                                         
  583   SUBROUTINE READ_RUN_NML (NDSI, NML_RUN)
  584 !/
  585 !/                  +-----------------------------------+
  586 !/                  | WAVEWATCH III           NOAA/NCEP |
  587 !/                  |           M. Accensi              |
  588 !/                  |                                   |
  589 !/                  |                        FORTRAN 90 |
  590 !/                  | Last update :         15-May-2018 |
  591 !/                  +-----------------------------------+
  592 !/
  593 !  1. Purpose :
  594 !
  595 !  2. Method :
  596 !
  597 !     See source term routines.
  598 !
  599 !  3. Parameters :
  600 !
  601 !     Parameter list
  602 !     ----------------------------------------------------------------
  603 !      NDSI         Int.
  604 !      NML_RUN      Type.
  605 !     ----------------------------------------------------------------
 
 
                                                      w3nmlgridmd.F90  page  12
 
 
  606 !
  607 !  4. Subroutines used :
  608 !
  609 !      Name      TYPE  Module   Description
  610 !     ----------------------------------------------------------------
  611 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  612 !     ----------------------------------------------------------------
  613 !
  614 !  5. Called by :
  615 !
  616 !      Name      TYPE  Module   Description
  617 !     ----------------------------------------------------------------
  618 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
  619 !     ----------------------------------------------------------------
  620 !
  621 !  6. Error messages :
  622 !
  623 !     None.
  624 !
  625 !  7. Remarks :
  626 !
  627 !  8. Structure :
  628 !
  629 !     See source code.
  630 !
  631 !  9. Switches :
  632 !
  633 ! 10. Source code :
  634 !
  635 !/ ------------------------------------------------------------------- /
  636                                                                         
  637     USE W3ODATMD, ONLY: NDSE
  638     USE W3SERVMD, ONLY: EXTCDE
  639                               
  640     IMPLICIT NONE
  641                  
  642     INTEGER, INTENT(IN)            :: NDSI
  643     TYPE(NML_RUN_T), INTENT(INOUT) :: NML_RUN
  644                                              
  645     ! locals
  646     INTEGER                   :: IERR
  647     TYPE(NML_RUN_T) :: RUN
  648     NAMELIST /RUN_NML/ RUN
  649                           
  650     IERR = 0
  651             
  652     ! set default values for run structure
  653     RUN%FLDRY      = .FALSE.
  654     RUN%FLCX       = .FALSE.
  655     RUN%FLCY       = .FALSE.
  656     RUN%FLCTH      = .FALSE.
  657     RUN%FLCK       = .FALSE.
  658     RUN%FLSOU      = .FALSE.
  659                             
  660     ! read run namelist
 
 
                                                      w3nmlgridmd.F90  page  13
 
 
  661     REWIND (NDSI)
  662     READ (NDSI, nml=RUN_NML, iostat=IERR, iomsg=MSG)
  663     IF (IERR.GT.0) THEN
  664       WRITE (NDSE,'(A,/A)') &
  665         'ERROR: READ_RUN_NML: namelist read error', &
  666         'ERROR: '//TRIM(MSG)
  667       CALL EXTCDE (2)
  668     END IF
  669           
  670     ! save namelist
  671     NML_RUN = RUN
  672                  
  673   END SUBROUTINE READ_RUN_NML
  674                              
  675 !/ ------------------------------------------------------------------- /
  676                                                                         
  677                                                                         
  678                                                                         
  679 !/ ------------------------------------------------------------------- /
  680                                                                         
  681   SUBROUTINE READ_TIMESTEPS_NML (NDSI, NML_TIMESTEPS)
  682 !/
  683 !/                  +-----------------------------------+
  684 !/                  | WAVEWATCH III           NOAA/NCEP |
  685 !/                  |           M. Accensi              |
  686 !/                  |                                   |
  687 !/                  |                        FORTRAN 90 |
  688 !/                  | Last update :         15-May-2018 |
  689 !/                  +-----------------------------------+
  690 !/
  691 !  1. Purpose :
  692 !
  693 !  2. Method :
  694 !
  695 !     See source term routines.
  696 !
  697 !  3. Parameters :
  698 !
  699 !     Parameter list
  700 !     ----------------------------------------------------------------
  701 !      NDSI             Int.
  702 !      NML_TIMESTEPS    Type.
  703 !     ----------------------------------------------------------------
  704 !
  705 !  4. Subroutines used :
  706 !
  707 !      Name      TYPE  Module   Description
  708 !     ----------------------------------------------------------------
  709 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  710 !     ----------------------------------------------------------------
  711 !
  712 !  5. Called by :
  713 !
  714 !      Name      TYPE  Module   Description
  715 !     ----------------------------------------------------------------
 
 
                                                      w3nmlgridmd.F90  page  14
 
 
  716 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
  717 !     ----------------------------------------------------------------
  718 !
  719 !  6. Error messages :
  720 !
  721 !     None.
  722 !
  723 !  7. Remarks :
  724 !
  725 !  8. Structure :
  726 !
  727 !     See source code.
  728 !
  729 !  9. Switches :
  730 !
  731 ! 10. Source code :
  732 !
  733 !/ ------------------------------------------------------------------- /
  734                                                                         
  735     USE W3ODATMD, ONLY: NDSE
  736     USE W3SERVMD, ONLY: EXTCDE
  737                               
  738     IMPLICIT NONE
  739                  
  740     INTEGER, INTENT(IN)                  :: NDSI
  741     TYPE(NML_TIMESTEPS_T), INTENT(INOUT) :: NML_TIMESTEPS
  742                                                          
  743     ! locals
  744     INTEGER                   :: IERR
  745     TYPE(NML_TIMESTEPS_T) :: TIMESTEPS
  746     NAMELIST /TIMESTEPS_NML/ TIMESTEPS
  747                                       
  748     IERR = 0
  749             
  750     ! set default values for timesteps structure
  751     TIMESTEPS%DTMAX      = 0.
  752     TIMESTEPS%DTXY       = 0.
  753     TIMESTEPS%DTKTH      = 0.
  754     TIMESTEPS%DTMIN      = 0.
  755                              
  756     ! read timesteps namelist
  757     REWIND (NDSI)
  758     READ (NDSI, nml=TIMESTEPS_NML, iostat=IERR, iomsg=MSG)
  759     IF (IERR.NE.0) THEN
  760       WRITE (NDSE,'(A,/A)') &
  761         'ERROR: READ_TIMESTEPS_NML: namelist read error', &
  762         'ERROR: '//TRIM(MSG)
  763       CALL EXTCDE (3)
  764     END IF
  765           
  766     ! save namelist
  767     NML_TIMESTEPS = TIMESTEPS
  768                              
  769   END SUBROUTINE READ_TIMESTEPS_NML
  770                                    
 
 
                                                      w3nmlgridmd.F90  page  15
 
 
  771 !/ ------------------------------------------------------------------- /
  772                                                                         
  773                                                                         
  774                                                                         
  775                                                                         
  776 !/ ------------------------------------------------------------------- /
  777                                                                         
  778   SUBROUTINE READ_GRID_NML (NDSI, NML_GRID)
  779 !/
  780 !/                  +-----------------------------------+
  781 !/                  | WAVEWATCH III           NOAA/NCEP |
  782 !/                  |           M. Accensi              |
  783 !/                  |                                   |
  784 !/                  |                        FORTRAN 90 |
  785 !/                  | Last update :         15-May-2018 |
  786 !/                  +-----------------------------------+
  787 !/
  788 !  1. Purpose :
  789 !
  790 !  2. Method :
  791 !
  792 !     See source term routines.
  793 !
  794 !  3. Parameters :
  795 !
  796 !     Parameter list
  797 !     ----------------------------------------------------------------
  798 !      NDSI             Int.
  799 !      NML_GRID         Type.
  800 !     ----------------------------------------------------------------
  801 !
  802 !  4. Subroutines used :
  803 !
  804 !      Name      TYPE  Module   Description
  805 !     ----------------------------------------------------------------
  806 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  807 !     ----------------------------------------------------------------
  808 !
  809 !  5. Called by :
  810 !
  811 !      Name      TYPE  Module   Description
  812 !     ----------------------------------------------------------------
  813 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
  814 !     ----------------------------------------------------------------
  815 !
  816 !  6. Error messages :
  817 !
  818 !     None.
  819 !
  820 !  7. Remarks :
  821 !
  822 !  8. Structure :
  823 !
  824 !     See source code.
  825 !
 
 
                                                      w3nmlgridmd.F90  page  16
 
 
  826 !  9. Switches :
  827 !
  828 ! 10. Source code :
  829 !
  830 !/ ------------------------------------------------------------------- /
  831                                                                         
  832     USE W3ODATMD, ONLY: NDSE
  833     USE W3SERVMD, ONLY: EXTCDE
  834                               
  835     IMPLICIT NONE
  836                  
  837     INTEGER, INTENT(IN)                  :: NDSI
  838     TYPE(NML_GRID_T), INTENT(INOUT)      :: NML_GRID
  839                                                     
  840     ! locals
  841     INTEGER                   :: IERR
  842     TYPE(NML_GRID_T) :: GRID
  843     NAMELIST /GRID_NML/ GRID
  844                             
  845     IERR = 0
  846             
  847     ! set default values for grid structure
  848     GRID%NAME       = 'unset'
  849     GRID%NML        = 'namelists.nml'
  850     GRID%TYPE       = 'unset'
  851     GRID%COORD      = 'unset'
  852     GRID%CLOS       = 'unset'
  853     GRID%ZLIM       = 0.
  854     GRID%DMIN       = 0.
  855                         
  856     ! read grid namelist
  857     REWIND (NDSI)
  858     READ (NDSI, nml=GRID_NML, iostat=IERR, iomsg=MSG)
  859     IF (IERR.NE.0) THEN
  860       WRITE (NDSE,'(A,/A)') &
  861         'ERROR: READ_GRID_NML: namelist read error', &
  862         'ERROR: '//TRIM(MSG)
  863       CALL EXTCDE (4)
  864     END IF
  865           
  866     ! save namelist
  867     NML_GRID = GRID
  868                    
  869   END SUBROUTINE READ_GRID_NML
  870                               
  871 !/ ------------------------------------------------------------------- /
  872                                                                         
  873                                                                         
  874                                                                         
  875 !/ ------------------------------------------------------------------- /
  876                                                                         
  877   SUBROUTINE READ_RECT_NML (NDSI, NML_RECT)
  878 !/
  879 !/                  +-----------------------------------+
  880 !/                  | WAVEWATCH III           NOAA/NCEP |
 
 
                                                      w3nmlgridmd.F90  page  17
 
 
  881 !/                  |           M. Accensi              |
  882 !/                  |                                   |
  883 !/                  |                        FORTRAN 90 |
  884 !/                  | Last update :         15-May-2018 |
  885 !/                  +-----------------------------------+
  886 !/
  887 !  1. Purpose :
  888 !
  889 !  2. Method :
  890 !
  891 !     See source term routines.
  892 !
  893 !  3. Parameters :
  894 !
  895 !     Parameter list
  896 !     ----------------------------------------------------------------
  897 !      NDSI             Int.
  898 !      NML_RECT         Type.
  899 !     ----------------------------------------------------------------
  900 !
  901 !  4. Subroutines used :
  902 !
  903 !      Name      TYPE  Module   Description
  904 !     ----------------------------------------------------------------
  905 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  906 !     ----------------------------------------------------------------
  907 !
  908 !  5. Called by :
  909 !
  910 !      Name      TYPE  Module   Description
  911 !     ----------------------------------------------------------------
  912 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
  913 !     ----------------------------------------------------------------
  914 !
  915 !  6. Error messages :
  916 !
  917 !     None.
  918 !
  919 !  7. Remarks :
  920 !
  921 !  8. Structure :
  922 !
  923 !     See source code.
  924 !
  925 !  9. Switches :
  926 !
  927 ! 10. Source code :
  928 !
  929 !/ ------------------------------------------------------------------- /
  930                                                                         
  931     USE W3ODATMD, ONLY: NDSE
  932     USE W3SERVMD, ONLY: EXTCDE
  933                               
  934     IMPLICIT NONE
  935                  
 
 
                                                      w3nmlgridmd.F90  page  18
 
 
  936     INTEGER, INTENT(IN)                  :: NDSI
  937     TYPE(NML_RECT_T), INTENT(INOUT)      :: NML_RECT
  938                                                     
  939     ! locals
  940     INTEGER                   :: IERR
  941     TYPE(NML_RECT_T) :: RECT
  942     NAMELIST /RECT_NML/ RECT
  943                             
  944     IERR = 0
  945             
  946     ! set default values for rect structure
  947     RECT%NX         = 0
  948     RECT%NY         = 0
  949     RECT%SX         = 0.
  950     RECT%SY         = 0.
  951     RECT%SF         = 1.
  952     RECT%X0         = 0.
  953     RECT%Y0         = 0.
  954     RECT%SF0        = 1.
  955                         
  956     ! read rect namelist
  957     REWIND (NDSI)
  958     READ (NDSI, nml=RECT_NML, iostat=IERR, iomsg=MSG)
  959     IF (IERR.GT.0) THEN
  960       WRITE (NDSE,'(A,/A)') &
  961         'ERROR: READ_RECT_NML: namelist read error', &
  962         'ERROR: '//TRIM(MSG)
  963       CALL EXTCDE (5)
  964     END IF
  965           
  966     ! save namelist
  967     NML_RECT = RECT
  968                    
  969   END SUBROUTINE READ_RECT_NML
  970                               
  971 !/ ------------------------------------------------------------------- /
  972                                                                         
  973                                                                         
  974                                                                         
  975 !/ ------------------------------------------------------------------- /
  976                                                                         
  977   SUBROUTINE READ_CURV_NML (NDSI, NML_CURV)
  978 !/
  979 !/                  +-----------------------------------+
  980 !/                  | WAVEWATCH III           NOAA/NCEP |
  981 !/                  |           M. Accensi              |
  982 !/                  |                                   |
  983 !/                  |                        FORTRAN 90 |
  984 !/                  | Last update :         15-May-2018 |
  985 !/                  +-----------------------------------+
  986 !/
  987 !  1. Purpose :
  988 !
  989 !  2. Method :
  990 !
 
 
                                                      w3nmlgridmd.F90  page  19
 
 
  991 !     See source term routines.
  992 !
  993 !  3. Parameters :
  994 !
  995 !     Parameter list
  996 !     ----------------------------------------------------------------
  997 !      NDSI             Int.
  998 !      NML_CURV         Type.
  999 !     ----------------------------------------------------------------
 1000 !
 1001 !  4. Subroutines used :
 1002 !
 1003 !      Name      TYPE  Module   Description
 1004 !     ----------------------------------------------------------------
 1005 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1006 !     ----------------------------------------------------------------
 1007 !
 1008 !  5. Called by :
 1009 !
 1010 !      Name      TYPE  Module   Description
 1011 !     ----------------------------------------------------------------
 1012 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 1013 !     ----------------------------------------------------------------
 1014 !
 1015 !  6. Error messages :
 1016 !
 1017 !     None.
 1018 !
 1019 !  7. Remarks :
 1020 !
 1021 !  8. Structure :
 1022 !
 1023 !     See source code.
 1024 !
 1025 !  9. Switches :
 1026 !
 1027 ! 10. Source code :
 1028 !
 1029 !/ ------------------------------------------------------------------- /
 1030                                                                         
 1031     USE W3ODATMD, ONLY: NDSE
 1032     USE W3SERVMD, ONLY: EXTCDE
 1033                               
 1034     IMPLICIT NONE
 1035                  
 1036     INTEGER, INTENT(IN)                  :: NDSI
 1037     TYPE(NML_CURV_T), INTENT(INOUT)      :: NML_CURV
 1038                                                     
 1039     ! locals
 1040     INTEGER                   :: IERR
 1041     TYPE(NML_CURV_T) :: CURV
 1042     NAMELIST /CURV_NML/ CURV
 1043                             
 1044     IERR = 0
 1045             
 
 
                                                      w3nmlgridmd.F90  page  20
 
 
 1046     ! set default values for curv structure
 1047     CURV%NX              = 0
 1048     CURV%NY              = 0
 1049 !
 1050     CURV%XCOORD%SF        = 1.
 1051     CURV%XCOORD%OFF       = 0.
 1052     CURV%XCOORD%FILENAME  = 'unset'
 1053     CURV%XCOORD%IDF       = 21
 1054     CURV%XCOORD%IDLA      = 1
 1055     CURV%XCOORD%IDFM      = 1
 1056     CURV%XCOORD%FORMAT    = '(....)'
 1057     CURV%XCOORD%FROM      = 'NAME'
 1058 !
 1059     CURV%YCOORD%SF        = 1.
 1060     CURV%YCOORD%OFF       = 0.
 1061     CURV%YCOORD%FILENAME  = 'unset'
 1062     CURV%YCOORD%IDF       = 22
 1063     CURV%YCOORD%IDLA      = 1
 1064     CURV%YCOORD%IDFM      = 1
 1065     CURV%YCOORD%FORMAT    = '(....)'
 1066     CURV%YCOORD%FROM      = 'NAME'
 1067                                   
 1068     ! read curv namelist
 1069     REWIND (NDSI)
 1070     READ (NDSI, nml=CURV_NML, iostat=IERR, iomsg=MSG)
 1071     IF (IERR.GT.0) THEN
 1072       WRITE (NDSE,'(A,/A)') &
 1073         'ERROR: READ_CURV_NML: namelist read error', &
 1074         'ERROR: '//TRIM(MSG)
 1075       CALL EXTCDE (6)
 1076     END IF
 1077           
 1078     ! save namelist
 1079     NML_CURV = CURV
 1080                    
 1081   END SUBROUTINE READ_CURV_NML
 1082                               
 1083 !/ ------------------------------------------------------------------- /
 1084                                                                         
 1085                                                                         
 1086                                                                         
 1087 !/ ------------------------------------------------------------------- /
 1088                                                                         
 1089   SUBROUTINE READ_UNST_NML (NDSI, NML_UNST)
 1090 !/
 1091 !/                  +-----------------------------------+
 1092 !/                  | WAVEWATCH III           NOAA/NCEP |
 1093 !/                  |           M. Accensi              |
 1094 !/                  |                                   |
 1095 !/                  |                        FORTRAN 90 |
 1096 !/                  | Last update :         15-May-2018 |
 1097 !/                  +-----------------------------------+
 1098 !/
 1099 !  1. Purpose :
 1100 !
 
 
                                                      w3nmlgridmd.F90  page  21
 
 
 1101 !  2. Method :
 1102 !
 1103 !     See source term routines.
 1104 !
 1105 !  3. Parameters :
 1106 !
 1107 !     Parameter list
 1108 !     ----------------------------------------------------------------
 1109 !      NDSI             Int.
 1110 !      NML_UNST         Type.
 1111 !     ----------------------------------------------------------------
 1112 !
 1113 !  4. Subroutines used :
 1114 !
 1115 !      Name      TYPE  Module   Description
 1116 !     ----------------------------------------------------------------
 1117 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1118 !     ----------------------------------------------------------------
 1119 !
 1120 !  5. Called by :
 1121 !
 1122 !      Name      TYPE  Module   Description
 1123 !     ----------------------------------------------------------------
 1124 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 1125 !     ----------------------------------------------------------------
 1126 !
 1127 !  6. Error messages :
 1128 !
 1129 !     None.
 1130 !
 1131 !  7. Remarks :
 1132 !
 1133 !  8. Structure :
 1134 !
 1135 !     See source code.
 1136 !
 1137 !  9. Switches :
 1138 !
 1139 ! 10. Source code :
 1140 !
 1141 !/ ------------------------------------------------------------------- /
 1142                                                                         
 1143     USE W3ODATMD, ONLY: NDSE
 1144     USE W3SERVMD, ONLY: EXTCDE
 1145                               
 1146     IMPLICIT NONE
 1147                  
 1148     INTEGER, INTENT(IN)                  :: NDSI
 1149     TYPE(NML_UNST_T), INTENT(INOUT)      :: NML_UNST
 1150                                                     
 1151     ! locals
 1152     INTEGER                   :: IERR
 1153     TYPE(NML_UNST_T) :: UNST
 1154     NAMELIST /UNST_NML/ UNST
 1155                             
 
 
                                                      w3nmlgridmd.F90  page  22
 
 
 1156     IERR = 0
 1157             
 1158     ! set default values for unst structure
 1159     UNST%SF              = 0
 1160     UNST%FILENAME        = 'unset'
 1161     UNST%IDF             = 20
 1162     UNST%IDLA            = 1
 1163     UNST%IDFM            = 1
 1164     UNST%FORMAT          = '(....)'
 1165     UNST%UGOBCFILE       = 'unset'
 1166                                   
 1167     ! read unst namelist
 1168     REWIND (NDSI)
 1169     READ (NDSI, nml=UNST_NML, iostat=IERR, iomsg=MSG)
 1170     IF (IERR.GT.0) THEN
 1171       WRITE (NDSE,'(A,/A)') &
 1172         'ERROR: READ_UNST_NML: namelist read error', &
 1173         'ERROR: '//TRIM(MSG)
 1174       CALL EXTCDE (7)
 1175     END IF
 1176           
 1177     ! save namelist
 1178     NML_UNST = UNST
 1179                    
 1180   END SUBROUTINE READ_UNST_NML
 1181                               
 1182 !/ ------------------------------------------------------------------- /
 1183                                                                         
 1184                                                                         
 1185                                                                         
 1186                                                                         
 1187 !/ ------------------------------------------------------------------- /
 1188                                                                         
 1189   SUBROUTINE READ_SMC_NML (NDSI, NML_SMC)
 1190 !/
 1191 !/                  +-----------------------------------+
 1192 !/                  | WAVEWATCH III           NOAA/NCEP |
 1193 !/                  |           M. Accensi              |
 1194 !/                  |                                   |
 1195 !/                  |                        FORTRAN 90 |
 1196 !/                  | Last update :         15-May-2018 |
 1197 !/                  +-----------------------------------+
 1198 !/
 1199 !  1. Purpose :
 1200 !
 1201 !  2. Method :
 1202 !
 1203 !     See source term routines.
 1204 !
 1205 !  3. Parameters :
 1206 !
 1207 !     Parameter list
 1208 !     ----------------------------------------------------------------
 1209 !      NDSI             Int.
 1210 !      NML_SMC          Type.
 
 
                                                      w3nmlgridmd.F90  page  23
 
 
 1211 !     ----------------------------------------------------------------
 1212 !
 1213 !  4. Subroutines used :
 1214 !
 1215 !      Name      TYPE  Module   Description
 1216 !     ----------------------------------------------------------------
 1217 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1218 !     ----------------------------------------------------------------
 1219 !
 1220 !  5. Called by :
 1221 !
 1222 !      Name      TYPE  Module   Description
 1223 !     ----------------------------------------------------------------
 1224 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 1225 !     ----------------------------------------------------------------
 1226 !
 1227 !  6. Error messages :
 1228 !
 1229 !     None.
 1230 !
 1231 !  7. Remarks :
 1232 !
 1233 !  8. Structure :
 1234 !
 1235 !     See source code.
 1236 !
 1237 !  9. Switches :
 1238 !
 1239 ! 10. Source code :
 1240 !
 1241 !/ ------------------------------------------------------------------- /
 1242                                                                         
 1243     USE W3ODATMD, ONLY: NDSE
 1244     USE W3SERVMD, ONLY: EXTCDE
 1245                               
 1246     IMPLICIT NONE
 1247                  
 1248     INTEGER, INTENT(IN)                  :: NDSI
 1249     TYPE(NML_SMC_T), INTENT(INOUT)       :: NML_SMC
 1250                                                    
 1251     ! locals
 1252     INTEGER                   :: IERR
 1253     TYPE(NML_SMC_T) :: SMC
 1254     NAMELIST /SMC_NML/ SMC
 1255                           
 1256     IERR = 0
 1257             
 1258     ! set default values for smc structure
 1259     SMC%MCELS%FILENAME   = 'unset'
 1260     SMC%MCELS%IDF        = 31
 1261     SMC%MCELS%IDLA       = 1
 1262     SMC%MCELS%IDFM       = 1
 1263     SMC%MCELS%FORMAT     = '(....)'
 1264 !
 1265     SMC%ISIDE%FILENAME   = 'unset'
 
 
                                                      w3nmlgridmd.F90  page  24
 
 
 1266     SMC%ISIDE%IDF        = 32
 1267     SMC%ISIDE%IDLA       = 1
 1268     SMC%ISIDE%IDFM       = 1
 1269     SMC%ISIDE%FORMAT     = '(....)'
 1270 !
 1271     SMC%JSIDE%FILENAME   = 'unset'
 1272     SMC%JSIDE%IDF        = 33
 1273     SMC%JSIDE%IDLA       = 1
 1274     SMC%JSIDE%IDFM       = 1
 1275     SMC%JSIDE%FORMAT     = '(....)'
 1276 !
 1277     SMC%SUBTR%FILENAME   = 'unset'
 1278     SMC%SUBTR%IDF        = 34
 1279     SMC%SUBTR%IDLA       = 1
 1280     SMC%SUBTR%IDFM       = 1
 1281     SMC%SUBTR%FORMAT     = '(....)'
 1282 !
 1283     SMC%BUNDY%FILENAME   = 'unset'
 1284     SMC%BUNDY%IDF        = 35
 1285     SMC%BUNDY%IDLA       = 1
 1286     SMC%BUNDY%IDFM       = 1
 1287     SMC%BUNDY%FORMAT     = '(....)'
 1288 !
 1289  
 1290  
 1291     ! read smc namelist
 1292     REWIND (NDSI)
 1293     READ (NDSI, nml=SMC_NML, iostat=IERR, iomsg=MSG)
 1294     IF (IERR.GT.0) THEN
 1295       WRITE (NDSE,'(A,/A)') &
 1296         'ERROR: READ_SMC_NML: namelist read error', &
 1297         'ERROR: '//TRIM(MSG)
 1298       CALL EXTCDE (8)
 1299     END IF
 1300           
 1301     ! save namelist
 1302     NML_SMC = SMC
 1303                  
 1304   END SUBROUTINE READ_SMC_NML
 1305                              
 1306 !/ ------------------------------------------------------------------- /
 1307                                                                         
 1308                                                                         
 1309                                                                         
 1310 !/ ------------------------------------------------------------------- /
 1311                                                                         
 1312   SUBROUTINE READ_DEPTH_NML (NDSI, NML_DEPTH)
 1313 !/
 1314 !/                  +-----------------------------------+
 1315 !/                  | WAVEWATCH III           NOAA/NCEP |
 1316 !/                  |           M. Accensi              |
 1317 !/                  |                                   |
 1318 !/                  |                        FORTRAN 90 |
 1319 !/                  | Last update :         15-May-2018 |
 1320 !/                  +-----------------------------------+
 
 
                                                      w3nmlgridmd.F90  page  25
 
 
 1321 !/
 1322 !  1. Purpose :
 1323 !
 1324 !  2. Method :
 1325 !
 1326 !     See source term routines.
 1327 !
 1328 !  3. Parameters :
 1329 !
 1330 !     Parameter list
 1331 !     ----------------------------------------------------------------
 1332 !      NDSI             Int.
 1333 !      NML_DEPTH        Type.
 1334 !     ----------------------------------------------------------------
 1335 !
 1336 !  4. Subroutines used :
 1337 !
 1338 !      Name      TYPE  Module   Description
 1339 !     ----------------------------------------------------------------
 1340 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1341 !     ----------------------------------------------------------------
 1342 !
 1343 !  5. Called by :
 1344 !
 1345 !      Name      TYPE  Module   Description
 1346 !     ----------------------------------------------------------------
 1347 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 1348 !     ----------------------------------------------------------------
 1349 !
 1350 !  6. Error messages :
 1351 !
 1352 !     None.
 1353 !
 1354 !  7. Remarks :
 1355 !
 1356 !  8. Structure :
 1357 !
 1358 !     See source code.
 1359 !
 1360 !  9. Switches :
 1361 !
 1362 ! 10. Source code :
 1363 !
 1364 !/ ------------------------------------------------------------------- /
 1365                                                                         
 1366     USE W3ODATMD, ONLY: NDSE
 1367     USE W3SERVMD, ONLY: EXTCDE
 1368                               
 1369     IMPLICIT NONE
 1370                  
 1371     INTEGER, INTENT(IN)                  :: NDSI
 1372     TYPE(NML_DEPTH_T), INTENT(INOUT)     :: NML_DEPTH
 1373                                                      
 1374     ! locals
 1375     INTEGER                   :: IERR
 
 
                                                      w3nmlgridmd.F90  page  26
 
 
 1376     TYPE(NML_DEPTH_T) :: DEPTH
 1377     NAMELIST /DEPTH_NML/ DEPTH
 1378                               
 1379     IERR = 0
 1380             
 1381     ! set default values for depth structure
 1382     DEPTH%SF         = 1.
 1383     DEPTH%FILENAME   = 'unset'
 1384     DEPTH%IDF        = 50
 1385     DEPTH%IDLA       = 1
 1386     DEPTH%IDFM       = 1
 1387     DEPTH%FORMAT     = '(....)'
 1388     DEPTH%FROM       = 'NAME'
 1389                              
 1390     ! read depth namelist
 1391     REWIND (NDSI)
 1392     READ (NDSI, nml=DEPTH_NML, iostat=IERR, iomsg=MSG)
 1393     IF (IERR.GT.0) THEN
 1394       WRITE (NDSE,'(A,/A)') &
 1395         'ERROR: READ_DEPTH_NML: namelist read error', &
 1396         'ERROR: '//TRIM(MSG)
 1397       CALL EXTCDE (9)
 1398     END IF
 1399           
 1400     ! save namelist
 1401     NML_DEPTH = DEPTH
 1402                      
 1403   END SUBROUTINE READ_DEPTH_NML
 1404                                
 1405 !/ ------------------------------------------------------------------- /
 1406                                                                         
 1407                                                                         
 1408                                                                         
 1409 !/ ------------------------------------------------------------------- /
 1410                                                                         
 1411   SUBROUTINE READ_MASK_NML (NDSI, NML_MASK)
 1412 !/
 1413 !/                  +-----------------------------------+
 1414 !/                  | WAVEWATCH III           NOAA/NCEP |
 1415 !/                  |           M. Accensi              |
 1416 !/                  |                                   |
 1417 !/                  |                        FORTRAN 90 |
 1418 !/                  | Last update :         15-May-2018 |
 1419 !/                  +-----------------------------------+
 1420 !/
 1421 !  1. Purpose :
 1422 !
 1423 !  2. Method :
 1424 !
 1425 !     See source term routines.
 1426 !
 1427 !  3. Parameters :
 1428 !
 1429 !     Parameter list
 1430 !     ----------------------------------------------------------------
 
 
                                                      w3nmlgridmd.F90  page  27
 
 
 1431 !      NDSI             Int.
 1432 !      NML_MASK         Type.
 1433 !     ----------------------------------------------------------------
 1434 !
 1435 !  4. Subroutines used :
 1436 !
 1437 !      Name      TYPE  Module   Description
 1438 !     ----------------------------------------------------------------
 1439 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1440 !     ----------------------------------------------------------------
 1441 !
 1442 !  5. Called by :
 1443 !
 1444 !      Name      TYPE  Module   Description
 1445 !     ----------------------------------------------------------------
 1446 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 1447 !     ----------------------------------------------------------------
 1448 !
 1449 !  6. Error messages :
 1450 !
 1451 !     None.
 1452 !
 1453 !  7. Remarks :
 1454 !
 1455 !  8. Structure :
 1456 !
 1457 !     See source code.
 1458 !
 1459 !  9. Switches :
 1460 !
 1461 ! 10. Source code :
 1462 !
 1463 !/ ------------------------------------------------------------------- /
 1464                                                                         
 1465     USE W3ODATMD, ONLY: NDSE
 1466     USE W3SERVMD, ONLY: EXTCDE
 1467                               
 1468     IMPLICIT NONE
 1469                  
 1470     INTEGER, INTENT(IN)                  :: NDSI
 1471     TYPE(NML_MASK_T), INTENT(INOUT)      :: NML_MASK
 1472                                                     
 1473     ! locals
 1474     INTEGER                   :: IERR
 1475     TYPE(NML_MASK_T) :: MASK
 1476     NAMELIST /MASK_NML/ MASK
 1477                             
 1478     IERR = 0
 1479             
 1480     ! set default values for mask structure
 1481     MASK%SF         = 1.
 1482     MASK%FILENAME   = 'unset'
 1483     MASK%IDF        = 60
 1484     MASK%IDLA       = 1
 1485     MASK%IDFM       = 1
 
 
                                                      w3nmlgridmd.F90  page  28
 
 
 1486     MASK%FORMAT     = '(....)'
 1487     MASK%FROM       = 'NAME'
 1488                             
 1489     ! read mask namelist
 1490     REWIND (NDSI)
 1491     READ (NDSI, nml=MASK_NML, iostat=IERR, iomsg=MSG)
 1492     IF (IERR.GT.0) THEN
 1493       WRITE (NDSE,'(A,/A)') &
 1494         'ERROR: READ_MASK_NML: namelist read error', &
 1495         'ERROR: '//TRIM(MSG)
 1496       CALL EXTCDE (10)
 1497     END IF
 1498           
 1499     ! save namelist
 1500     NML_MASK = MASK
 1501                    
 1502   END SUBROUTINE READ_MASK_NML
 1503                               
 1504 !/ ------------------------------------------------------------------- /
 1505                                                                         
 1506                                                                         
 1507                                                                         
 1508                                                                         
 1509 !/ ------------------------------------------------------------------- /
 1510                                                                         
 1511   SUBROUTINE READ_OBST_NML (NDSI, NML_OBST)
 1512 !/
 1513 !/                  +-----------------------------------+
 1514 !/                  | WAVEWATCH III           NOAA/NCEP |
 1515 !/                  |           M. Accensi              |
 1516 !/                  |                                   |
 1517 !/                  |                        FORTRAN 90 |
 1518 !/                  | Last update :         15-May-2018 |
 1519 !/                  +-----------------------------------+
 1520 !/
 1521 !  1. Purpose :
 1522 !
 1523 !  2. Method :
 1524 !
 1525 !     See source term routines.
 1526 !
 1527 !  3. Parameters :
 1528 !
 1529 !     Parameter list
 1530 !     ----------------------------------------------------------------
 1531 !      NDSI             Int.
 1532 !      NML_OBST         Type.
 1533 !     ----------------------------------------------------------------
 1534 !
 1535 !  4. Subroutines used :
 1536 !
 1537 !      Name      TYPE  Module   Description
 1538 !     ----------------------------------------------------------------
 1539 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1540 !     ----------------------------------------------------------------
 
 
                                                      w3nmlgridmd.F90  page  29
 
 
 1541 !
 1542 !  5. Called by :
 1543 !
 1544 !      Name      TYPE  Module   Description
 1545 !     ----------------------------------------------------------------
 1546 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 1547 !     ----------------------------------------------------------------
 1548 !
 1549 !  6. Error messages :
 1550 !
 1551 !     None.
 1552 !
 1553 !  7. Remarks :
 1554 !
 1555 !  8. Structure :
 1556 !
 1557 !     See source code.
 1558 !
 1559 !  9. Switches :
 1560 !
 1561 ! 10. Source code :
 1562 !
 1563 !/ ------------------------------------------------------------------- /
 1564                                                                         
 1565     USE W3ODATMD, ONLY: NDSE
 1566     USE W3SERVMD, ONLY: EXTCDE
 1567                               
 1568     IMPLICIT NONE
 1569                  
 1570     INTEGER, INTENT(IN)                  :: NDSI
 1571     TYPE(NML_OBST_T), INTENT(INOUT)      :: NML_OBST
 1572                                                     
 1573     ! locals
 1574     INTEGER                   :: IERR
 1575     TYPE(NML_OBST_T) :: OBST
 1576     NAMELIST /OBST_NML/ OBST
 1577                             
 1578     IERR = 0
 1579             
 1580     ! set default values for obst structure
 1581     OBST%SF         = 1.
 1582     OBST%FILENAME   = 'unset'
 1583     OBST%IDF        = 70
 1584     OBST%IDLA       = 1
 1585     OBST%IDFM       = 1
 1586     OBST%FORMAT     = '(....)'
 1587     OBST%FROM       = 'NAME'
 1588                             
 1589     ! read obst namelist
 1590     REWIND (NDSI)
 1591     READ (NDSI, nml=OBST_NML, iostat=IERR, iomsg=MSG)
 1592     IF (IERR.GT.0) THEN
 1593       WRITE (NDSE,'(A,/A)') &
 1594         'ERROR: READ_OBST_NML: namelist read error', &
 1595         'ERROR: '//TRIM(MSG)
 
 
                                                      w3nmlgridmd.F90  page  30
 
 
 1596       CALL EXTCDE (11)
 1597     END IF
 1598           
 1599     ! save namelist
 1600     NML_OBST = OBST
 1601                    
 1602   END SUBROUTINE READ_OBST_NML
 1603                               
 1604 !/ ------------------------------------------------------------------- /
 1605                                                                         
 1606                                                                         
 1607                                                                         
 1608                                                                         
 1609 !/ ------------------------------------------------------------------- /
 1610                                                                         
 1611   SUBROUTINE READ_SLOPE_NML (NDSI, NML_SLOPE)
 1612 !/
 1613 !/                  +-----------------------------------+
 1614 !/                  | WAVEWATCH III           NOAA/NCEP |
 1615 !/                  |           M. Accensi              |
 1616 !/                  |                                   |
 1617 !/                  |                        FORTRAN 90 |
 1618 !/                  | Last update :         15-May-2018 |
 1619 !/                  +-----------------------------------+
 1620 !/
 1621 !  1. Purpose :
 1622 !
 1623 !  2. Method :
 1624 !
 1625 !     See source term routines.
 1626 !
 1627 !  3. Parameters :
 1628 !
 1629 !     Parameter list
 1630 !     ----------------------------------------------------------------
 1631 !      NDSI             Int.
 1632 !      NML_SLOPE        Type.
 1633 !     ----------------------------------------------------------------
 1634 !
 1635 !  4. Subroutines used :
 1636 !
 1637 !      Name      TYPE  Module   Description
 1638 !     ----------------------------------------------------------------
 1639 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1640 !     ----------------------------------------------------------------
 1641 !
 1642 !  5. Called by :
 1643 !
 1644 !      Name      TYPE  Module   Description
 1645 !     ----------------------------------------------------------------
 1646 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 1647 !     ----------------------------------------------------------------
 1648 !
 1649 !  6. Error messages :
 1650 !
 
 
                                                      w3nmlgridmd.F90  page  31
 
 
 1651 !     None.
 1652 !
 1653 !  7. Remarks :
 1654 !
 1655 !  8. Structure :
 1656 !
 1657 !     See source code.
 1658 !
 1659 !  9. Switches :
 1660 !
 1661 ! 10. Source code :
 1662 !
 1663 !/ ------------------------------------------------------------------- /
 1664                                                                         
 1665     USE W3ODATMD, ONLY: NDSE
 1666     USE W3SERVMD, ONLY: EXTCDE
 1667                               
 1668     IMPLICIT NONE
 1669                  
 1670     INTEGER, INTENT(IN)                  :: NDSI
 1671     TYPE(NML_SLOPE_T), INTENT(INOUT)     :: NML_SLOPE
 1672                                                      
 1673     ! locals
 1674     INTEGER                   :: IERR
 1675     TYPE(NML_SLOPE_T) :: SLOPE
 1676     NAMELIST /SLOPE_NML/ SLOPE
 1677                               
 1678     IERR = 0
 1679             
 1680     ! set default values for slope structure
 1681     SLOPE%SF         = 1.
 1682     SLOPE%FILENAME   = 'unset'
 1683     SLOPE%IDF        = 80
 1684     SLOPE%IDLA       = 1
 1685     SLOPE%IDFM       = 1
 1686     SLOPE%FORMAT     = '(....)'
 1687     SLOPE%FROM       = 'NAME'
 1688                              
 1689     ! read slope namelist
 1690     REWIND (NDSI)
 1691     READ (NDSI, nml=SLOPE_NML, iostat=IERR, iomsg=MSG)
 1692     IF (IERR.GT.0) THEN
 1693       WRITE (NDSE,'(A,/A)') &
 1694         'ERROR: READ_SLOPE_NML: namelist read error', &
 1695         'ERROR: '//TRIM(MSG)
 1696       CALL EXTCDE (12)
 1697     END IF
 1698           
 1699     ! save namelist
 1700     NML_SLOPE = SLOPE
 1701                      
 1702   END SUBROUTINE READ_SLOPE_NML
 1703                                
 1704 !/ ------------------------------------------------------------------- /
 1705                                                                         
 
 
                                                      w3nmlgridmd.F90  page  32
 
 
 1706                                                                         
 1707                                                                         
 1708                                                                         
 1709                                                                         
 1710 !/ ------------------------------------------------------------------- /
 1711                                                                         
 1712   SUBROUTINE READ_SED_NML (NDSI, NML_SED)
 1713 !/
 1714 !/                  +-----------------------------------+
 1715 !/                  | WAVEWATCH III           NOAA/NCEP |
 1716 !/                  |           M. Accensi              |
 1717 !/                  |                                   |
 1718 !/                  |                        FORTRAN 90 |
 1719 !/                  | Last update :         15-May-2018 |
 1720 !/                  +-----------------------------------+
 1721 !/
 1722 !  1. Purpose :
 1723 !
 1724 !  2. Method :
 1725 !
 1726 !     See source term routines.
 1727 !
 1728 !  3. Parameters :
 1729 !
 1730 !     Parameter list
 1731 !     ----------------------------------------------------------------
 1732 !      NDSI             Int.
 1733 !      NML_SED          Type.
 1734 !     ----------------------------------------------------------------
 1735 !
 1736 !  4. Subroutines used :
 1737 !
 1738 !      Name      TYPE  Module   Description
 1739 !     ----------------------------------------------------------------
 1740 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1741 !     ----------------------------------------------------------------
 1742 !
 1743 !  5. Called by :
 1744 !
 1745 !      Name      TYPE  Module   Description
 1746 !     ----------------------------------------------------------------
 1747 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 1748 !     ----------------------------------------------------------------
 1749 !
 1750 !  6. Error messages :
 1751 !
 1752 !     None.
 1753 !
 1754 !  7. Remarks :
 1755 !
 1756 !  8. Structure :
 1757 !
 1758 !     See source code.
 1759 !
 1760 !  9. Switches :
 
 
                                                      w3nmlgridmd.F90  page  33
 
 
 1761 !
 1762 ! 10. Source code :
 1763 !
 1764 !/ ------------------------------------------------------------------- /
 1765                                                                         
 1766     USE W3ODATMD, ONLY: NDSE
 1767     USE W3SERVMD, ONLY: EXTCDE
 1768                               
 1769     IMPLICIT NONE
 1770                  
 1771     INTEGER, INTENT(IN)                  :: NDSI
 1772     TYPE(NML_SED_T), INTENT(INOUT)       :: NML_SED
 1773                                                    
 1774     ! locals
 1775     INTEGER                   :: IERR
 1776     TYPE(NML_SED_T) :: SED
 1777     NAMELIST /SED_NML/ SED
 1778                           
 1779     IERR = 0
 1780             
 1781     ! set default values for sed structure
 1782     SED%SF         = 1.
 1783     SED%FILENAME   = 'unset'
 1784     SED%IDF        = 90
 1785     SED%IDLA       = 1
 1786     SED%IDFM       = 1
 1787     SED%FORMAT     = '(....)'
 1788     SED%FROM       = 'NAME'
 1789                            
 1790     ! read sed namelist
 1791     REWIND (NDSI)
 1792     READ (NDSI, nml=SED_NML, iostat=IERR, iomsg=MSG)
 1793     IF (IERR.GT.0) THEN
 1794       WRITE (NDSE,'(A,/A)') &
 1795         'ERROR: READ_SED_NML: namelist read error', &
 1796         'ERROR: '//TRIM(MSG)
 1797       CALL EXTCDE (13)
 1798     END IF
 1799           
 1800     ! save namelist
 1801     NML_SED = SED
 1802                  
 1803   END SUBROUTINE READ_SED_NML
 1804                              
 1805 !/ ------------------------------------------------------------------- /
 1806                                                                         
 1807                                                                         
 1808                                                                         
 1809                                                                         
 1810                                                                         
 1811 !/ ------------------------------------------------------------------- /
 1812                                                                         
 1813   SUBROUTINE READ_INBOUND_NML (NDSI, NML_INBND_COUNT, NML_INBND_POINT)
 1814 !/
 1815 !/                  +-----------------------------------+
 
 
                                                      w3nmlgridmd.F90  page  34
 
 
 1816 !/                  | WAVEWATCH III           NOAA/NCEP |
 1817 !/                  |           M. Accensi              |
 1818 !/                  |                                   |
 1819 !/                  |                        FORTRAN 90 |
 1820 !/                  | Last update :         15-May-2018 |
 1821 !/                  +-----------------------------------+
 1822 !/
 1823 !  1. Purpose :
 1824 !
 1825 !  2. Method :
 1826 !
 1827 !     See source term routines.
 1828 !
 1829 !  3. Parameters :
 1830 !
 1831 !     Parameter list
 1832 !     ----------------------------------------------------------------
 1833 !      NDSI             Int.
 1834 !      NML_INBND_COUNT  Type.
 1835 !      NML_INBND_POINT  Type.
 1836 !     ----------------------------------------------------------------
 1837 !
 1838 !  4. Subroutines used :
 1839 !
 1840 !      Name      TYPE  Module   Description
 1841 !     ----------------------------------------------------------------
 1842 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1843 !     ----------------------------------------------------------------
 1844 !
 1845 !  5. Called by :
 1846 !
 1847 !      Name      TYPE  Module   Description
 1848 !     ----------------------------------------------------------------
 1849 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 1850 !     ----------------------------------------------------------------
 1851 !
 1852 !  6. Error messages :
 1853 !
 1854 !     None.
 1855 !
 1856 !  7. Remarks :
 1857 !
 1858 !  8. Structure :
 1859 !
 1860 !     See source code.
 1861 !
 1862 !  9. Switches :
 1863 !
 1864 ! 10. Source code :
 1865 !
 1866 !/ ------------------------------------------------------------------- /
 1867                                                                         
 1868     USE W3ODATMD, ONLY: NDSE
 1869     USE W3SERVMD, ONLY: EXTCDE
 1870                               
 
 
                                                      w3nmlgridmd.F90  page  35
 
 
 1871     IMPLICIT NONE
 1872                  
 1873     INTEGER, INTENT(IN)                      :: NDSI
 1874     TYPE(NML_INBND_COUNT_T), INTENT(INOUT)   :: NML_INBND_COUNT
 1875     TYPE(NML_INBND_POINT_T), ALLOCATABLE, INTENT(INOUT)   :: NML_INBND_PO>
 1876                                                                           
 1877     ! locals
 1878     INTEGER                   :: IERR, I
 1879     TYPE(NML_INBND_COUNT_T) :: INBND_COUNT
 1880     NAMELIST /INBND_COUNT_NML/ INBND_COUNT
 1881     TYPE(NML_INBND_POINT_T), ALLOCATABLE :: INBND_POINT(:)
 1882     NAMELIST /INBND_POINT_NML/ INBND_POINT
 1883                                           
 1884     IERR = 0
 1885             
 1886     ! set default values for inbnd count structure
 1887     INBND_COUNT%N_POINT    = 0
 1888                               
 1889     ! read inbnd count namelist
 1890     REWIND (NDSI)
 1891     READ (NDSI, nml=INBND_COUNT_NML, iostat=IERR, iomsg=MSG)
 1892     IF (IERR.GT.0) THEN
 1893       WRITE (NDSE,'(A,/A)') &
 1894         'ERROR: READ_INBND_COUNT_NML: namelist read error', &
 1895         'ERROR: '//TRIM(MSG)
 1896       CALL EXTCDE (14)
 1897     END IF
 1898           
 1899     ! allocate the total count of inbound points
 1900     ALLOCATE(INBND_POINT(INBND_COUNT%N_POINT))
 1901     ALLOCATE(NML_INBND_POINT(INBND_COUNT%N_POINT))
 1902                                                   
 1903     ! set default values for inbnd point structure
 1904     IF (INBND_COUNT%N_POINT .NE. 0 ) THEN
 1905       DO I=1,INBND_COUNT%N_POINT
 1906         INBND_POINT(I)%X_INDEX  = 0
 1907         INBND_POINT(I)%Y_INDEX  = 0
 1908         INBND_POINT(I)%CONNECT  = .FALSE.
 1909       END DO
 1910     END IF
 1911           
 1912     ! read inbnd point namelist
 1913     REWIND (NDSI)
 1914     READ (NDSI, nml=INBND_POINT_NML, iostat=IERR, iomsg=MSG)
 1915     IF (IERR.GT.0) THEN
 1916       WRITE (NDSE,'(A,/A)') &
 1917         'ERROR: READ_INBND_POINT_NML: namelist read error', &
 1918         'ERROR: '//TRIM(MSG)
 1919       CALL EXTCDE (15)
 1920     END IF
 1921           
 1922     ! save namelist
 1923     NML_INBND_COUNT = INBND_COUNT
 1924     NML_INBND_POINT = INBND_POINT
 1925                                  
 
 
                                                      w3nmlgridmd.F90  page  36
 
 
 1926   END SUBROUTINE READ_INBOUND_NML
 1927                                  
 1928 !/ ------------------------------------------------------------------- /
 1929                                                                         
 1930                                                                         
 1931                                                                         
 1932                                                                         
 1933 !/ ------------------------------------------------------------------- /
 1934                                                                         
 1935   SUBROUTINE READ_EXCLUDED_NML (NDSI, NML_EXCL_COUNT, NML_EXCL_POINT, &
 1936                                 NML_EXCL_BODY)
 1937 !/
 1938 !/                  +-----------------------------------+
 1939 !/                  | WAVEWATCH III           NOAA/NCEP |
 1940 !/                  |           M. Accensi              |
 1941 !/                  |                                   |
 1942 !/                  |                        FORTRAN 90 |
 1943 !/                  | Last update :         15-May-2018 |
 1944 !/                  +-----------------------------------+
 1945 !/
 1946 !  1. Purpose :
 1947 !
 1948 !  2. Method :
 1949 !
 1950 !     See source term routines.
 1951 !
 1952 !  3. Parameters :
 1953 !
 1954 !     Parameter list
 1955 !     ----------------------------------------------------------------
 1956 !      NDSI             Int.
 1957 !      NML_EXCL_COUNT   Type.
 1958 !      NML_EXCL_POINT   Type.
 1959 !      NML_EXCL_BODY    Type.
 1960 !     ----------------------------------------------------------------
 1961 !
 1962 !  4. Subroutines used :
 1963 !
 1964 !      Name      TYPE  Module   Description
 1965 !     ----------------------------------------------------------------
 1966 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1967 !     ----------------------------------------------------------------
 1968 !
 1969 !  5. Called by :
 1970 !
 1971 !      Name      TYPE  Module   Description
 1972 !     ----------------------------------------------------------------
 1973 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 1974 !     ----------------------------------------------------------------
 1975 !
 1976 !  6. Error messages :
 1977 !
 1978 !     None.
 1979 !
 1980 !  7. Remarks :
 
 
                                                      w3nmlgridmd.F90  page  37
 
 
 1981 !
 1982 !  8. Structure :
 1983 !
 1984 !     See source code.
 1985 !
 1986 !  9. Switches :
 1987 !
 1988 ! 10. Source code :
 1989 !
 1990 !/ ------------------------------------------------------------------- /
 1991                                                                         
 1992     USE W3ODATMD, ONLY: NDSE
 1993     USE W3SERVMD, ONLY: EXTCDE
 1994                               
 1995     IMPLICIT NONE
 1996                  
 1997     INTEGER, INTENT(IN)                      :: NDSI
 1998     TYPE(NML_EXCL_COUNT_T), INTENT(INOUT)    :: NML_EXCL_COUNT
 1999     TYPE(NML_EXCL_POINT_T), ALLOCATABLE, INTENT(INOUT)   :: NML_EXCL_POIN>
 2000     TYPE(NML_EXCL_BODY_T), ALLOCATABLE, INTENT(INOUT)    :: NML_EXCL_BODY>
 2001                                                                           
 2002     ! locals
 2003     INTEGER                   :: IERR, J, K
 2004     TYPE(NML_EXCL_COUNT_T) :: EXCL_COUNT
 2005     NAMELIST /EXCL_COUNT_NML/ EXCL_COUNT
 2006     TYPE(NML_EXCL_POINT_T), ALLOCATABLE :: EXCL_POINT(:)
 2007     NAMELIST /EXCL_POINT_NML/ EXCL_POINT
 2008     TYPE(NML_EXCL_BODY_T), ALLOCATABLE :: EXCL_BODY(:)
 2009     NAMELIST /EXCL_BODY_NML/ EXCL_BODY
 2010                                       
 2011     IERR = 0
 2012             
 2013     ! set default values for excl count structure
 2014     EXCL_COUNT%N_POINT    = 0
 2015     EXCL_COUNT%N_BODY     = 0
 2016                              
 2017     ! read excl count namelist
 2018     REWIND (NDSI)
 2019     READ (NDSI, nml=EXCL_COUNT_NML, iostat=IERR, iomsg=MSG)
 2020     IF (IERR.GT.0) THEN
 2021       WRITE (NDSE,'(A,/A)') &
 2022         'ERROR: READ_EXCL_COUNT_NML: namelist read error', &
 2023         'ERROR: '//TRIM(MSG)
 2024       CALL EXTCDE (16)
 2025     END IF
 2026           
 2027     ! allocate the total count of excluded points
 2028     ALLOCATE(EXCL_POINT(EXCL_COUNT%N_POINT))
 2029     ALLOCATE(NML_EXCL_POINT(EXCL_COUNT%N_POINT))
 2030                                                 
 2031     ! set default values for excl point structure
 2032     IF (EXCL_COUNT%N_POINT .NE. 0 ) THEN
 2033       DO J=1,EXCL_COUNT%N_POINT
 2034         EXCL_POINT(J)%X_INDEX  = 0
 2035         EXCL_POINT(J)%Y_INDEX  = 0
 
 
                                                      w3nmlgridmd.F90  page  38
 
 
 2036         EXCL_POINT(J)%CONNECT  = .FALSE.
 2037       END DO
 2038     END IF
 2039           
 2040     ! read excl point namelist
 2041     REWIND (NDSI)
 2042     READ (NDSI, nml=EXCL_POINT_NML, iostat=IERR, iomsg=MSG)
 2043     IF (IERR.GT.0) THEN
 2044       WRITE (NDSE,'(A,/A)') &
 2045         'ERROR: READ_EXCL_POINT_NML: namelist read error', &
 2046         'ERROR: '//TRIM(MSG)
 2047       CALL EXTCDE (17)
 2048     END IF
 2049           
 2050     ! allocate the total count of excluded bodies
 2051     ALLOCATE(EXCL_BODY(EXCL_COUNT%N_BODY))
 2052     ALLOCATE(NML_EXCL_BODY(EXCL_COUNT%N_BODY))
 2053                                               
 2054     ! set default values for excl body structure
 2055     IF (EXCL_COUNT%N_BODY .NE. 0 ) THEN
 2056       DO K=1,EXCL_COUNT%N_BODY
 2057         EXCL_BODY(K)%X_INDEX  = 0
 2058         EXCL_BODY(K)%Y_INDEX  = 0
 2059       END DO
 2060     END IF
 2061           
 2062     ! read excl body namelist
 2063     REWIND (NDSI)
 2064     READ (NDSI, nml=EXCL_POINT_NML, iostat=IERR, iomsg=MSG)
 2065     IF (IERR.GT.0) THEN
 2066       WRITE (NDSE,'(A,/A)') &
 2067         'ERROR: READ_EXCL_POINT_NML: namelist read error', &
 2068         'ERROR: '//TRIM(MSG)
 2069       CALL EXTCDE (18)
 2070     END IF
 2071           
 2072     ! save namelist
 2073     NML_EXCL_COUNT = EXCL_COUNT
 2074     NML_EXCL_POINT = EXCL_POINT
 2075     NML_EXCL_BODY  = EXCL_BODY
 2076                               
 2077   END SUBROUTINE READ_EXCLUDED_NML
 2078                                   
 2079 !/ ------------------------------------------------------------------- /
 2080                                                                         
 2081                                                                         
 2082                                                                         
 2083                                                                         
 2084 !/ ------------------------------------------------------------------- /
 2085                                                                         
 2086   SUBROUTINE READ_OUTBOUND_NML (NDSI, NML_OUTBND_COUNT, NML_OUTBND_LINE)
 2087 !/
 2088 !/                  +-----------------------------------+
 2089 !/                  | WAVEWATCH III           NOAA/NCEP |
 2090 !/                  |           M. Accensi              |
 
 
                                                      w3nmlgridmd.F90  page  39
 
 
 2091 !/                  |                                   |
 2092 !/                  |                        FORTRAN 90 |
 2093 !/                  | Last update :         15-May-2018 |
 2094 !/                  +-----------------------------------+
 2095 !/
 2096 !  1. Purpose :
 2097 !
 2098 !  2. Method :
 2099 !
 2100 !     See source term routines.
 2101 !
 2102 !  3. Parameters :
 2103 !
 2104 !     Parameter list
 2105 !     ----------------------------------------------------------------
 2106 !      NDSI             Int.
 2107 !      NML_OUTBND_COUNT Type.
 2108 !      NML_OUTBND_LINE  Type.
 2109 !     ----------------------------------------------------------------
 2110 !
 2111 !  4. Subroutines used :
 2112 !
 2113 !      Name      TYPE  Module   Description
 2114 !     ----------------------------------------------------------------
 2115 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 2116 !     ----------------------------------------------------------------
 2117 !
 2118 !  5. Called by :
 2119 !
 2120 !      Name      TYPE  Module   Description
 2121 !     ----------------------------------------------------------------
 2122 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 2123 !     ----------------------------------------------------------------
 2124 !
 2125 !  6. Error messages :
 2126 !
 2127 !     None.
 2128 !
 2129 !  7. Remarks :
 2130 !
 2131 !  8. Structure :
 2132 !
 2133 !     See source code.
 2134 !
 2135 !  9. Switches :
 2136 !
 2137 ! 10. Source code :
 2138 !
 2139 !/ ------------------------------------------------------------------- /
 2140                                                                         
 2141     USE W3ODATMD, ONLY: NDSE
 2142     USE W3SERVMD, ONLY: EXTCDE
 2143                               
 2144     IMPLICIT NONE
 2145                  
 
 
                                                      w3nmlgridmd.F90  page  40
 
 
 2146     INTEGER, INTENT(IN)                      :: NDSI
 2147     TYPE(NML_OUTBND_COUNT_T), INTENT(INOUT)    :: NML_OUTBND_COUNT
 2148     TYPE(NML_OUTBND_LINE_T), ALLOCATABLE, INTENT(INOUT)   :: NML_OUTBND_L>
 2149                                                                           
 2150     ! locals
 2151     INTEGER                   :: IERR, I
 2152     TYPE(NML_OUTBND_COUNT_T) :: OUTBND_COUNT
 2153     NAMELIST /OUTBND_COUNT_NML/ OUTBND_COUNT
 2154     TYPE(NML_OUTBND_LINE_T), ALLOCATABLE :: OUTBND_LINE(:)
 2155     NAMELIST /OUTBND_LINE_NML/ OUTBND_LINE
 2156                                           
 2157                                           
 2158     IERR = 0
 2159             
 2160     ! set default values for outbnd count structure
 2161     OUTBND_COUNT%N_LINE    = 0
 2162                               
 2163     ! read outbnd count namelist
 2164     REWIND (NDSI)
 2165     READ (NDSI, nml=OUTBND_COUNT_NML, iostat=IERR, iomsg=MSG)
 2166     IF (IERR.GT.0) THEN
 2167       WRITE (NDSE,'(A,/A)') &
 2168         'ERROR: READ_OUTBND_COUNT_NML: namelist read error', &
 2169         'ERROR: '//TRIM(MSG)
 2170       CALL EXTCDE (19)
 2171     END IF
 2172           
 2173     ! allocate the total count of outbound lines
 2174     ALLOCATE(OUTBND_LINE(OUTBND_COUNT%N_LINE))
 2175     ALLOCATE(NML_OUTBND_LINE(OUTBND_COUNT%N_LINE))
 2176                                                   
 2177     ! set default values for outbnd line structure
 2178     IF (OUTBND_COUNT%N_LINE .NE. 0 ) THEN
 2179       DO I=1,OUTBND_COUNT%N_LINE
 2180         OUTBND_LINE(I)%X0   = 0.
 2181         OUTBND_LINE(I)%Y0   = 0.
 2182         OUTBND_LINE(I)%DX   = 0.
 2183         OUTBND_LINE(I)%DY   = 0.
 2184         OUTBND_LINE(I)%NP   = 0
 2185       END DO
 2186     END IF
 2187           
 2188     ! read outbnd line namelist
 2189     REWIND (NDSI)
 2190     READ (NDSI, nml=OUTBND_LINE_NML, iostat=IERR, iomsg=MSG)
 2191     IF (IERR.GT.0) THEN
 2192       WRITE (NDSE,'(A,/A)') &
 2193         'ERROR: READ_OUTBND_LINE_NML: namelist read error', &
 2194         'ERROR: '//TRIM(MSG)
 2195       CALL EXTCDE (20)
 2196     END IF
 2197           
 2198     ! save namelist
 2199     NML_OUTBND_COUNT = OUTBND_COUNT
 2200     NML_OUTBND_LINE  = OUTBND_LINE
 
 
                                                      w3nmlgridmd.F90  page  41
 
 
 2201                                   
 2202   END SUBROUTINE READ_OUTBOUND_NML
 2203                                   
 2204 !/ ------------------------------------------------------------------- /
 2205                                                                         
 2206                                                                         
 2207                                                                         
 2208                                                                         
 2209                                                                         
 2210 !/ ------------------------------------------------------------------- /
 2211                                                                         
 2212   SUBROUTINE REPORT_SPECTRUM_NML (NML_SPECTRUM)
 2213 !/
 2214 !/                  +-----------------------------------+
 2215 !/                  | WAVEWATCH III           NOAA/NCEP |
 2216 !/                  |           M. Accensi              |
 2217 !/                  |                        FORTRAN 90 |
 2218 !/                  | Last update :         15-May-2018 |
 2219 !/                  +-----------------------------------+
 2220 !/
 2221 !/
 2222 !  1. Purpose :
 2223 !
 2224 !  2. Method :
 2225 !
 2226 !     See source term routines.
 2227 !
 2228 !  3. Parameters :
 2229 !
 2230 !     Parameter list
 2231 !     ----------------------------------------------------------------
 2232 !      NML_SPECTRUM  Type.
 2233 !     ----------------------------------------------------------------
 2234 !
 2235 !  4. Subroutines used :
 2236 !
 2237 !      Name      TYPE  Module   Description
 2238 !     ----------------------------------------------------------------
 2239 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 2240 !     ----------------------------------------------------------------
 2241 !
 2242 !  5. Called by :
 2243 !
 2244 !      Name      TYPE  Module   Description
 2245 !     ----------------------------------------------------------------
 2246 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 2247 !     ----------------------------------------------------------------
 2248 !
 2249 !  6. Error messages :
 2250 !
 2251 !     None.
 2252 !
 2253 !  7. Remarks :
 2254 !
 2255 !  8. Structure :
 
 
                                                      w3nmlgridmd.F90  page  42
 
 
 2256 !
 2257 !     See source code.
 2258 !
 2259 !  9. Switches :
 2260 !
 2261 ! 10. Source code :
 2262 !
 2263 !/ ------------------------------------------------------------------- /
 2264                                                                         
 2265                                                                         
 2266     IMPLICIT NONE
 2267                  
 2268     TYPE(NML_SPECTRUM_T), INTENT(IN) :: NML_SPECTRUM
 2269                                                     
 2270                                                     
 2271       WRITE (MSG,'(A)') 'SPECTRUM % '
 2272       WRITE (NDSN,'(A)')
 2273       WRITE (NDSN,14) TRIM(MSG),'XFR        = ', NML_SPECTRUM%XFR
 2274       WRITE (NDSN,14) TRIM(MSG),'FREQ1      = ', NML_SPECTRUM%FREQ1
 2275       WRITE (NDSN,11) TRIM(MSG),'NK         = ', NML_SPECTRUM%NK
 2276       WRITE (NDSN,11) TRIM(MSG),'NTH        = ', NML_SPECTRUM%NTH
 2277       WRITE (NDSN,14) TRIM(MSG),'THOFF      = ', NML_SPECTRUM%THOFF
 2278                                                                    
 2279 10  FORMAT (A,2X,A,A)
 2280 11  FORMAT (A,2X,A,I8)
 2281 13  FORMAT (A,2X,A,L1)
 2282 14  FORMAT (A,2X,A,F8.2)
 2283                         
 2284   END SUBROUTINE REPORT_SPECTRUM_NML
 2285                                     
 2286 !/ ------------------------------------------------------------------- /
 2287                                                                         
 2288                                                                         
 2289                                                                         
 2290 !/ ------------------------------------------------------------------- /
 2291                                                                         
 2292   SUBROUTINE REPORT_RUN_NML (NML_RUN)
 2293 !/
 2294 !/                  +-----------------------------------+
 2295 !/                  | WAVEWATCH III           NOAA/NCEP |
 2296 !/                  |           M. Accensi              |
 2297 !/                  |                        FORTRAN 90 |
 2298 !/                  | Last update :         15-May-2018 |
 2299 !/                  +-----------------------------------+
 2300 !/
 2301 !/
 2302 !  1. Purpose :
 2303 !
 2304 !  2. Method :
 2305 !
 2306 !     See source term routines.
 2307 !
 2308 !  3. Parameters :
 2309 !
 2310 !     Parameter list
 
 
                                                      w3nmlgridmd.F90  page  43
 
 
 2311 !     ----------------------------------------------------------------
 2312 !      NML_RUN  Type.
 2313 !     ----------------------------------------------------------------
 2314 !
 2315 !  4. Subroutines used :
 2316 !
 2317 !      Name      TYPE  Module   Description
 2318 !     ----------------------------------------------------------------
 2319 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 2320 !     ----------------------------------------------------------------
 2321 !
 2322 !  5. Called by :
 2323 !
 2324 !      Name      TYPE  Module   Description
 2325 !     ----------------------------------------------------------------
 2326 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 2327 !     ----------------------------------------------------------------
 2328 !
 2329 !  6. Error messages :
 2330 !
 2331 !     None.
 2332 !
 2333 !  7. Remarks :
 2334 !
 2335 !  8. Structure :
 2336 !
 2337 !     See source code.
 2338 !
 2339 !  9. Switches :
 2340 !
 2341 ! 10. Source code :
 2342 !
 2343 !/ ------------------------------------------------------------------- /
 2344                                                                         
 2345                                                                         
 2346     IMPLICIT NONE
 2347                  
 2348     TYPE(NML_RUN_T), INTENT(IN) :: NML_RUN
 2349                                           
 2350                                           
 2351       WRITE (MSG,'(A)') 'RUN % '
 2352       WRITE (NDSN,'(A)')
 2353       WRITE (NDSN,13) TRIM(MSG),'FLDRY      = ', NML_RUN%FLDRY
 2354       WRITE (NDSN,13) TRIM(MSG),'FLCX       = ', NML_RUN%FLCX
 2355       WRITE (NDSN,13) TRIM(MSG),'FLCY       = ', NML_RUN%FLCY
 2356       WRITE (NDSN,13) TRIM(MSG),'FLCTH      = ', NML_RUN%FLCTH
 2357       WRITE (NDSN,13) TRIM(MSG),'FLCK       = ', NML_RUN%FLCK
 2358       WRITE (NDSN,13) TRIM(MSG),'FLSOU      = ', NML_RUN%FLSOU
 2359                                                               
 2360                                                               
 2361 10  FORMAT (A,2X,A,A)
 2362 11  FORMAT (A,2X,A,I8)
 2363 13  FORMAT (A,2X,A,L1)
 2364 14  FORMAT (A,2X,A,F8.2)
 2365                         
 
 
                                                      w3nmlgridmd.F90  page  44
 
 
 2366   END SUBROUTINE REPORT_RUN_NML
 2367                                
 2368 !/ ------------------------------------------------------------------- /
 2369                                                                         
 2370                                                                         
 2371 !/ ------------------------------------------------------------------- /
 2372                                                                         
 2373   SUBROUTINE REPORT_TIMESTEPS_NML (NML_TIMESTEPS)
 2374 !/
 2375 !/                  +-----------------------------------+
 2376 !/                  | WAVEWATCH III           NOAA/NCEP |
 2377 !/                  |           M. Accensi              |
 2378 !/                  |                        FORTRAN 90 |
 2379 !/                  | Last update :         15-May-2018 |
 2380 !/                  +-----------------------------------+
 2381 !/
 2382 !/
 2383 !  1. Purpose :
 2384 !
 2385 !  2. Method :
 2386 !
 2387 !     See source term routines.
 2388 !
 2389 !  3. Parameters :
 2390 !
 2391 !     Parameter list
 2392 !     ----------------------------------------------------------------
 2393 !      NML_TIMESTEPS  Type.
 2394 !     ----------------------------------------------------------------
 2395 !
 2396 !  4. Subroutines used :
 2397 !
 2398 !      Name      TYPE  Module   Description
 2399 !     ----------------------------------------------------------------
 2400 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 2401 !     ----------------------------------------------------------------
 2402 !
 2403 !  5. Called by :
 2404 !
 2405 !      Name      TYPE  Module   Description
 2406 !     ----------------------------------------------------------------
 2407 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 2408 !     ----------------------------------------------------------------
 2409 !
 2410 !  6. Error messages :
 2411 !
 2412 !     None.
 2413 !
 2414 !  7. Remarks :
 2415 !
 2416 !  8. Structure :
 2417 !
 2418 !     See source code.
 2419 !
 2420 !  9. Switches :
 
 
                                                      w3nmlgridmd.F90  page  45
 
 
 2421 !
 2422 ! 10. Source code :
 2423 !
 2424 !/ ------------------------------------------------------------------- /
 2425                                                                         
 2426                                                                         
 2427     IMPLICIT NONE
 2428                  
 2429     TYPE(NML_TIMESTEPS_T), INTENT(IN) :: NML_TIMESTEPS
 2430                                                       
 2431                                                       
 2432       WRITE (MSG,'(A)') 'TIMESTEPS % '
 2433       WRITE (NDSN,'(A)')
 2434       WRITE (NDSN,14) TRIM(MSG),'DTMAX      = ', NML_TIMESTEPS%DTMAX
 2435       WRITE (NDSN,14) TRIM(MSG),'DTXY       = ', NML_TIMESTEPS%DTXY
 2436       WRITE (NDSN,14) TRIM(MSG),'DTKTH      = ', NML_TIMESTEPS%DTKTH
 2437       WRITE (NDSN,14) TRIM(MSG),'DTMIN      = ', NML_TIMESTEPS%DTMIN
 2438                                                                     
 2439 10  FORMAT (A,2X,A,A)
 2440 11  FORMAT (A,2X,A,I8)
 2441 13  FORMAT (A,2X,A,L1)
 2442 14  FORMAT (A,2X,A,F8.2)
 2443                         
 2444   END SUBROUTINE REPORT_TIMESTEPS_NML
 2445                                      
 2446 !/ ------------------------------------------------------------------- /
 2447                                                                         
 2448                                                                         
 2449                                                                         
 2450 !/ ------------------------------------------------------------------- /
 2451                                                                         
 2452   SUBROUTINE REPORT_GRID_NML (NML_GRID)
 2453 !/
 2454 !/                  +-----------------------------------+
 2455 !/                  | WAVEWATCH III           NOAA/NCEP |
 2456 !/                  |           M. Accensi              |
 2457 !/                  |                        FORTRAN 90 |
 2458 !/                  | Last update :         15-May-2018 |
 2459 !/                  +-----------------------------------+
 2460 !/
 2461 !/
 2462 !  1. Purpose :
 2463 !
 2464 !  2. Method :
 2465 !
 2466 !     See source term routines.
 2467 !
 2468 !  3. Parameters :
 2469 !
 2470 !     Parameter list
 2471 !     ----------------------------------------------------------------
 2472 !      NML_GRID  Type.
 2473 !     ----------------------------------------------------------------
 2474 !
 2475 !  4. Subroutines used :
 
 
                                                      w3nmlgridmd.F90  page  46
 
 
 2476 !
 2477 !      Name      TYPE  Module   Description
 2478 !     ----------------------------------------------------------------
 2479 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 2480 !     ----------------------------------------------------------------
 2481 !
 2482 !  5. Called by :
 2483 !
 2484 !      Name      TYPE  Module   Description
 2485 !     ----------------------------------------------------------------
 2486 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 2487 !     ----------------------------------------------------------------
 2488 !
 2489 !  6. Error messages :
 2490 !
 2491 !     None.
 2492 !
 2493 !  7. Remarks :
 2494 !
 2495 !  8. Structure :
 2496 !
 2497 !     See source code.
 2498 !
 2499 !  9. Switches :
 2500 !
 2501 ! 10. Source code :
 2502 !
 2503 !/ ------------------------------------------------------------------- /
 2504                                                                         
 2505                                                                         
 2506     IMPLICIT NONE
 2507                  
 2508     TYPE(NML_GRID_T), INTENT(IN) :: NML_GRID
 2509                                             
 2510                                             
 2511       WRITE (MSG,'(A)') 'GRID % '
 2512       WRITE (NDSN,'(A)')
 2513       WRITE (NDSN,10) TRIM(MSG),'NAME       = ', TRIM(NML_GRID%NAME)
 2514       WRITE (NDSN,10) TRIM(MSG),'NML        = ', TRIM(NML_GRID%NML)
 2515       WRITE (NDSN,10) TRIM(MSG),'TYPE       = ', TRIM(NML_GRID%TYPE)
 2516       WRITE (NDSN,10) TRIM(MSG),'COORD      = ', TRIM(NML_GRID%COORD)
 2517       WRITE (NDSN,10) TRIM(MSG),'CLOS       = ', TRIM(NML_GRID%CLOS)
 2518       WRITE (NDSN,14) TRIM(MSG),'ZLIM       = ', NML_GRID%ZLIM
 2519       WRITE (NDSN,14) TRIM(MSG),'DMIN       = ', NML_GRID%DMIN
 2520                                                               
 2521 10  FORMAT (A,2X,A,A)
 2522 11  FORMAT (A,2X,A,I8)
 2523 13  FORMAT (A,2X,A,L1)
 2524 14  FORMAT (A,2X,A,F8.2)
 2525                         
 2526   END SUBROUTINE REPORT_GRID_NML
 2527                                 
 2528 !/ ------------------------------------------------------------------- /
 2529                                                                         
 2530                                                                         
 
 
                                                      w3nmlgridmd.F90  page  47
 
 
 2531 !/ ------------------------------------------------------------------- /
 2532                                                                         
 2533   SUBROUTINE REPORT_RECT_NML (NML_RECT)
 2534 !/
 2535 !/                  +-----------------------------------+
 2536 !/                  | WAVEWATCH III           NOAA/NCEP |
 2537 !/                  |           M. Accensi              |
 2538 !/                  |                        FORTRAN 90 |
 2539 !/                  | Last update :         15-May-2018 |
 2540 !/                  +-----------------------------------+
 2541 !/
 2542 !/
 2543 !  1. Purpose :
 2544 !
 2545 !  2. Method :
 2546 !
 2547 !     See source term routines.
 2548 !
 2549 !  3. Parameters :
 2550 !
 2551 !     Parameter list
 2552 !     ----------------------------------------------------------------
 2553 !      NML_RECT  Type.
 2554 !     ----------------------------------------------------------------
 2555 !
 2556 !  4. Subroutines used :
 2557 !
 2558 !      Name      TYPE  Module   Description
 2559 !     ----------------------------------------------------------------
 2560 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 2561 !     ----------------------------------------------------------------
 2562 !
 2563 !  5. Called by :
 2564 !
 2565 !      Name      TYPE  Module   Description
 2566 !     ----------------------------------------------------------------
 2567 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 2568 !     ----------------------------------------------------------------
 2569 !
 2570 !  6. Error messages :
 2571 !
 2572 !     None.
 2573 !
 2574 !  7. Remarks :
 2575 !
 2576 !  8. Structure :
 2577 !
 2578 !     See source code.
 2579 !
 2580 !  9. Switches :
 2581 !
 2582 ! 10. Source code :
 2583 !
 2584 !/ ------------------------------------------------------------------- /
 2585                                                                         
 
 
                                                      w3nmlgridmd.F90  page  48
 
 
 2586                                                                         
 2587     IMPLICIT NONE
 2588                  
 2589     TYPE(NML_RECT_T), INTENT(IN) :: NML_RECT
 2590                                             
 2591                                             
 2592       WRITE (MSG,'(A)') 'RECT % '
 2593       WRITE (NDSN,'(A)')
 2594       WRITE (NDSN,11) TRIM(MSG),'NX         = ', NML_RECT%NX
 2595       WRITE (NDSN,11) TRIM(MSG),'NY         = ', NML_RECT%NY
 2596       WRITE (NDSN,14) TRIM(MSG),'SX         = ', NML_RECT%SX
 2597       WRITE (NDSN,14) TRIM(MSG),'SY         = ', NML_RECT%SY
 2598       WRITE (NDSN,14) TRIM(MSG),'SF         = ', NML_RECT%SF
 2599       WRITE (NDSN,14) TRIM(MSG),'X0         = ', NML_RECT%X0
 2600       WRITE (NDSN,14) TRIM(MSG),'Y0         = ', NML_RECT%Y0
 2601       WRITE (NDSN,14) TRIM(MSG),'SF0        = ', NML_RECT%SF0
 2602                                                              
 2603 10  FORMAT (A,2X,A,A)
 2604 11  FORMAT (A,2X,A,I8)
 2605 13  FORMAT (A,2X,A,L1)
 2606 14  FORMAT (A,2X,A,F12.2)
 2607                          
 2608   END SUBROUTINE REPORT_RECT_NML
 2609                                 
 2610 !/ ------------------------------------------------------------------- /
 2611                                                                         
 2612                                                                         
 2613                                                                         
 2614 !/ ------------------------------------------------------------------- /
 2615                                                                         
 2616   SUBROUTINE REPORT_CURV_NML (NML_CURV)
 2617 !/
 2618 !/                  +-----------------------------------+
 2619 !/                  | WAVEWATCH III           NOAA/NCEP |
 2620 !/                  |           M. Accensi              |
 2621 !/                  |                        FORTRAN 90 |
 2622 !/                  | Last update :         15-May-2018 |
 2623 !/                  +-----------------------------------+
 2624 !/
 2625 !/
 2626 !  1. Purpose :
 2627 !
 2628 !  2. Method :
 2629 !
 2630 !     See source term routines.
 2631 !
 2632 !  3. Parameters :
 2633 !
 2634 !     Parameter list
 2635 !     ----------------------------------------------------------------
 2636 !      NML_CURV  Type.
 2637 !     ----------------------------------------------------------------
 2638 !
 2639 !  4. Subroutines used :
 2640 !
 
 
                                                      w3nmlgridmd.F90  page  49
 
 
 2641 !      Name      TYPE  Module   Description
 2642 !     ----------------------------------------------------------------
 2643 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 2644 !     ----------------------------------------------------------------
 2645 !
 2646 !  5. Called by :
 2647 !
 2648 !      Name      TYPE  Module   Description
 2649 !     ----------------------------------------------------------------
 2650 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 2651 !     ----------------------------------------------------------------
 2652 !
 2653 !  6. Error messages :
 2654 !
 2655 !     None.
 2656 !
 2657 !  7. Remarks :
 2658 !
 2659 !  8. Structure :
 2660 !
 2661 !     See source code.
 2662 !
 2663 !  9. Switches :
 2664 !
 2665 ! 10. Source code :
 2666 !
 2667 !/ ------------------------------------------------------------------- /
 2668                                                                         
 2669                                                                         
 2670     IMPLICIT NONE
 2671                  
 2672     TYPE(NML_CURV_T), INTENT(IN) :: NML_CURV
 2673                                             
 2674                                             
 2675       WRITE (MSG,'(A)') 'CURV % '
 2676       WRITE (NDSN,'(A)')
 2677       WRITE (NDSN,11) TRIM(MSG),'NX                 = ', NML_CURV%NX
 2678       WRITE (NDSN,11) TRIM(MSG),'NY                 = ', NML_CURV%NY
 2679 !
 2680       WRITE (NDSN,14) TRIM(MSG),'XCOORD % SF        = ', NML_CURV%XCOORD%>
 2681       WRITE (NDSN,14) TRIM(MSG),'XCOORD % OFF       = ', NML_CURV%XCOORD%>
 2682       WRITE (NDSN,10) TRIM(MSG),'XCOORD % FILENAME  = ', TRIM(NML_CURV%XC>
 2683       WRITE (NDSN,11) TRIM(MSG),'XCOORD % IDF       = ', NML_CURV%XCOORD%>
 2684       WRITE (NDSN,11) TRIM(MSG),'XCOORD % IDLA      = ', NML_CURV%XCOORD%>
 2685       WRITE (NDSN,11) TRIM(MSG),'XCOORD % IDFM      = ', NML_CURV%XCOORD%>
 2686       WRITE (NDSN,10) TRIM(MSG),'XCOORD % FORMAT    = ', TRIM(NML_CURV%XC>
 2687       WRITE (NDSN,10) TRIM(MSG),'XCOORD % FROM      = ', TRIM(NML_CURV%XC>
 2688 !
 2689       WRITE (NDSN,14) TRIM(MSG),'YCOORD % SF        = ', NML_CURV%YCOORD%>
 2690       WRITE (NDSN,14) TRIM(MSG),'YCOORD % OFF       = ', NML_CURV%YCOORD%>
 2691       WRITE (NDSN,10) TRIM(MSG),'YCOORD % FILENAME  = ', TRIM(NML_CURV%YC>
 2692       WRITE (NDSN,11) TRIM(MSG),'YCOORD % IDF       = ', NML_CURV%YCOORD%>
 2693       WRITE (NDSN,11) TRIM(MSG),'YCOORD % IDLA      = ', NML_CURV%YCOORD%>
 2694       WRITE (NDSN,11) TRIM(MSG),'YCOORD % IDFM      = ', NML_CURV%YCOORD%>
 2695       WRITE (NDSN,10) TRIM(MSG),'YCOORD % FORMAT    = ', TRIM(NML_CURV%YC>
 
 
                                                      w3nmlgridmd.F90  page  50
 
 
 2696       WRITE (NDSN,10) TRIM(MSG),'YCOORD % FROM      = ', TRIM(NML_CURV%YC>
 2697                                                                           
 2698                                                                           
 2699 10  FORMAT (A,2X,A,A)
 2700 11  FORMAT (A,2X,A,I8)
 2701 13  FORMAT (A,2X,A,L1)
 2702 14  FORMAT (A,2X,A,F8.2)
 2703                         
 2704   END SUBROUTINE REPORT_CURV_NML
 2705                                 
 2706 !/ ------------------------------------------------------------------- /
 2707                                                                         
 2708                                                                         
 2709                                                                         
 2710 !/ ------------------------------------------------------------------- /
 2711                                                                         
 2712   SUBROUTINE REPORT_UNST_NML (NML_UNST)
 2713 !/
 2714 !/                  +-----------------------------------+
 2715 !/                  | WAVEWATCH III           NOAA/NCEP |
 2716 !/                  |           M. Accensi              |
 2717 !/                  |                        FORTRAN 90 |
 2718 !/                  | Last update :         15-May-2018 |
 2719 !/                  +-----------------------------------+
 2720 !/
 2721 !/
 2722 !  1. Purpose :
 2723 !
 2724 !  2. Method :
 2725 !
 2726 !     See source term routines.
 2727 !
 2728 !  3. Parameters :
 2729 !
 2730 !     Parameter list
 2731 !     ----------------------------------------------------------------
 2732 !      NML_UNST  Type.
 2733 !     ----------------------------------------------------------------
 2734 !
 2735 !  4. Subroutines used :
 2736 !
 2737 !      Name      TYPE  Module   Description
 2738 !     ----------------------------------------------------------------
 2739 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 2740 !     ----------------------------------------------------------------
 2741 !
 2742 !  5. Called by :
 2743 !
 2744 !      Name      TYPE  Module   Description
 2745 !     ----------------------------------------------------------------
 2746 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 2747 !     ----------------------------------------------------------------
 2748 !
 2749 !  6. Error messages :
 2750 !
 
 
                                                      w3nmlgridmd.F90  page  51
 
 
 2751 !     None.
 2752 !
 2753 !  7. Remarks :
 2754 !
 2755 !  8. Structure :
 2756 !
 2757 !     See source code.
 2758 !
 2759 !  9. Switches :
 2760 !
 2761 ! 10. Source code :
 2762 !
 2763 !/ ------------------------------------------------------------------- /
 2764                                                                         
 2765                                                                         
 2766     IMPLICIT NONE
 2767                  
 2768     TYPE(NML_UNST_T), INTENT(IN) :: NML_UNST
 2769                                             
 2770                                             
 2771       WRITE (MSG,'(A)') 'UNST % '
 2772       WRITE (NDSN,'(A)')
 2773       WRITE (NDSN,14) TRIM(MSG),'SF        = ', NML_UNST%SF
 2774       WRITE (NDSN,10) TRIM(MSG),'FILENAME  = ', TRIM(NML_UNST%FILENAME)
 2775       WRITE (NDSN,11) TRIM(MSG),'IDF       = ', NML_UNST%IDF
 2776       WRITE (NDSN,11) TRIM(MSG),'IDLA      = ', NML_UNST%IDLA
 2777       WRITE (NDSN,11) TRIM(MSG),'IDFM      = ', NML_UNST%IDFM
 2778       WRITE (NDSN,10) TRIM(MSG),'FORMAT    = ', TRIM(NML_UNST%FORMAT)
 2779 !
 2780       WRITE (NDSN,10) TRIM(MSG),'UGOBCFILE = ', TRIM(NML_UNST%UGOBCFILE)
 2781                                                                         
 2782                                                                         
 2783 10  FORMAT (A,2X,A,A)
 2784 11  FORMAT (A,2X,A,I8)
 2785 13  FORMAT (A,2X,A,L1)
 2786 14  FORMAT (A,2X,A,F8.2)
 2787                         
 2788   END SUBROUTINE REPORT_UNST_NML
 2789                                 
 2790 !/ ------------------------------------------------------------------- /
 2791                                                                         
 2792                                                                         
 2793                                                                         
 2794 !/ ------------------------------------------------------------------- /
 2795                                                                         
 2796   SUBROUTINE REPORT_SMC_NML (NML_SMC)
 2797 !/
 2798 !/                  +-----------------------------------+
 2799 !/                  | WAVEWATCH III           NOAA/NCEP |
 2800 !/                  |           M. Accensi              |
 2801 !/                  |                        FORTRAN 90 |
 2802 !/                  | Last update :         15-May-2018 |
 2803 !/                  +-----------------------------------+
 2804 !/
 2805 !/
 
 
                                                      w3nmlgridmd.F90  page  52
 
 
 2806 !  1. Purpose :
 2807 !
 2808 !  2. Method :
 2809 !
 2810 !     See source term routines.
 2811 !
 2812 !  3. Parameters :
 2813 !
 2814 !     Parameter list
 2815 !     ----------------------------------------------------------------
 2816 !      NML_SMC   Type.
 2817 !     ----------------------------------------------------------------
 2818 !
 2819 !  4. Subroutines used :
 2820 !
 2821 !      Name      TYPE  Module   Description
 2822 !     ----------------------------------------------------------------
 2823 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 2824 !     ----------------------------------------------------------------
 2825 !
 2826 !  5. Called by :
 2827 !
 2828 !      Name      TYPE  Module   Description
 2829 !     ----------------------------------------------------------------
 2830 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 2831 !     ----------------------------------------------------------------
 2832 !
 2833 !  6. Error messages :
 2834 !
 2835 !     None.
 2836 !
 2837 !  7. Remarks :
 2838 !
 2839 !  8. Structure :
 2840 !
 2841 !     See source code.
 2842 !
 2843 !  9. Switches :
 2844 !
 2845 ! 10. Source code :
 2846 !
 2847 !/ ------------------------------------------------------------------- /
 2848                                                                         
 2849                                                                         
 2850     IMPLICIT NONE
 2851                  
 2852     TYPE(NML_SMC_T), INTENT(IN) :: NML_SMC
 2853                                           
 2854                                           
 2855       WRITE (MSG,'(A)') 'SMC % '
 2856       WRITE (NDSN,'(A)')
 2857 !
 2858       WRITE (NDSN,10) TRIM(MSG),'MCELS % FILENAME  = ', TRIM(NML_SMC%MCEL>
 2859       WRITE (NDSN,11) TRIM(MSG),'MCELS % IDF       = ', NML_SMC%MCELS%IDF
 2860       WRITE (NDSN,11) TRIM(MSG),'MCELS % IDLA      = ', NML_SMC%MCELS%IDLA
 
 
                                                      w3nmlgridmd.F90  page  53
 
 
 2861       WRITE (NDSN,11) TRIM(MSG),'MCELS % IDFM      = ', NML_SMC%MCELS%IDFM
 2862       WRITE (NDSN,10) TRIM(MSG),'MCELS % FORMAT    = ', TRIM(NML_SMC%MCEL>
 2863 !
 2864       WRITE (NDSN,10) TRIM(MSG),'ISIDE % FILENAME  = ', TRIM(NML_SMC%ISID>
 2865       WRITE (NDSN,11) TRIM(MSG),'ISIDE % IDF       = ', NML_SMC%ISIDE%IDF
 2866       WRITE (NDSN,11) TRIM(MSG),'ISIDE % IDLA      = ', NML_SMC%ISIDE%IDLA
 2867       WRITE (NDSN,11) TRIM(MSG),'ISIDE % IDFM      = ', NML_SMC%ISIDE%IDFM
 2868       WRITE (NDSN,10) TRIM(MSG),'ISIDE % FORMAT    = ', TRIM(NML_SMC%ISID>
 2869 !
 2870       WRITE (NDSN,10) TRIM(MSG),'JSIDE % FILENAME  = ', TRIM(NML_SMC%JSID>
 2871       WRITE (NDSN,11) TRIM(MSG),'JSIDE % IDF       = ', NML_SMC%JSIDE%IDF
 2872       WRITE (NDSN,11) TRIM(MSG),'JSIDE % IDLA      = ', NML_SMC%JSIDE%IDLA
 2873       WRITE (NDSN,11) TRIM(MSG),'JSIDE % IDFM      = ', NML_SMC%JSIDE%IDFM
 2874       WRITE (NDSN,10) TRIM(MSG),'JSIDE % FORMAT    = ', TRIM(NML_SMC%JSID>
 2875 !
 2876       WRITE (NDSN,10) TRIM(MSG),'SUBTR % FILENAME  = ', TRIM(NML_SMC%SUBT>
 2877       WRITE (NDSN,11) TRIM(MSG),'SUBTR % IDF       = ', NML_SMC%SUBTR%IDF
 2878       WRITE (NDSN,11) TRIM(MSG),'SUBTR % IDLA      = ', NML_SMC%SUBTR%IDLA
 2879       WRITE (NDSN,11) TRIM(MSG),'SUBTR % IDFM      = ', NML_SMC%SUBTR%IDFM
 2880       WRITE (NDSN,10) TRIM(MSG),'SUBTR % FORMAT    = ', TRIM(NML_SMC%SUBT>
 2881 !
 2882       WRITE (NDSN,10) TRIM(MSG),'BUNDY % FILENAME  = ', TRIM(NML_SMC%BUND>
 2883       WRITE (NDSN,11) TRIM(MSG),'BUNDY % IDF       = ', NML_SMC%BUNDY%IDF
 2884       WRITE (NDSN,11) TRIM(MSG),'BUNDY % IDLA      = ', NML_SMC%BUNDY%IDLA
 2885       WRITE (NDSN,11) TRIM(MSG),'BUNDY % IDFM      = ', NML_SMC%BUNDY%IDFM
 2886       WRITE (NDSN,10) TRIM(MSG),'BUNDY % FORMAT    = ', TRIM(NML_SMC%BUND>
 2887 !
 2888  
 2889  
 2890 10  FORMAT (A,2X,A,A)
 2891 11  FORMAT (A,2X,A,I8)
 2892 13  FORMAT (A,2X,A,L1)
 2893 14  FORMAT (A,2X,A,F8.2)
 2894                         
 2895                         
 2896   END SUBROUTINE REPORT_SMC_NML
 2897                                
 2898 !/ ------------------------------------------------------------------- /
 2899                                                                         
 2900                                                                         
 2901                                                                         
 2902                                                                         
 2903                                                                         
 2904 !/ ------------------------------------------------------------------- /
 2905                                                                         
 2906   SUBROUTINE REPORT_DEPTH_NML (NML_DEPTH)
 2907 !/
 2908 !/                  +-----------------------------------+
 2909 !/                  | WAVEWATCH III           NOAA/NCEP |
 2910 !/                  |           M. Accensi              |
 2911 !/                  |                        FORTRAN 90 |
 2912 !/                  | Last update :         15-May-2018 |
 2913 !/                  +-----------------------------------+
 2914 !/
 2915 !/
 
 
                                                      w3nmlgridmd.F90  page  54
 
 
 2916 !  1. Purpose :
 2917 !
 2918 !  2. Method :
 2919 !
 2920 !     See source term routines.
 2921 !
 2922 !  3. Parameters :
 2923 !
 2924 !     Parameter list
 2925 !     ----------------------------------------------------------------
 2926 !      NML_DEPTH  Type.
 2927 !     ----------------------------------------------------------------
 2928 !
 2929 !  4. Subroutines used :
 2930 !
 2931 !      Name      TYPE  Module   Description
 2932 !     ----------------------------------------------------------------
 2933 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 2934 !     ----------------------------------------------------------------
 2935 !
 2936 !  5. Called by :
 2937 !
 2938 !      Name      TYPE  Module   Description
 2939 !     ----------------------------------------------------------------
 2940 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 2941 !     ----------------------------------------------------------------
 2942 !
 2943 !  6. Error messages :
 2944 !
 2945 !     None.
 2946 !
 2947 !  7. Remarks :
 2948 !
 2949 !  8. Structure :
 2950 !
 2951 !     See source code.
 2952 !
 2953 !  9. Switches :
 2954 !
 2955 ! 10. Source code :
 2956 !
 2957 !/ ------------------------------------------------------------------- /
 2958                                                                         
 2959                                                                         
 2960     IMPLICIT NONE
 2961                  
 2962     TYPE(NML_DEPTH_T), INTENT(IN) :: NML_DEPTH
 2963                                               
 2964                                               
 2965       WRITE (MSG,'(A)') 'DEPTH % '
 2966       WRITE (NDSN,'(A)')
 2967       WRITE (NDSN,14) TRIM(MSG),'SF        = ', NML_DEPTH%SF
 2968       WRITE (NDSN,10) TRIM(MSG),'FILENAME  = ', TRIM(NML_DEPTH%FILENAME)
 2969       WRITE (NDSN,11) TRIM(MSG),'IDF       = ', NML_DEPTH%IDF
 2970       WRITE (NDSN,11) TRIM(MSG),'IDLA      = ', NML_DEPTH%IDLA
 
 
                                                      w3nmlgridmd.F90  page  55
 
 
 2971       WRITE (NDSN,11) TRIM(MSG),'IDFM      = ', NML_DEPTH%IDFM
 2972       WRITE (NDSN,10) TRIM(MSG),'FORMAT    = ', TRIM(NML_DEPTH%FORMAT)
 2973       WRITE (NDSN,10) TRIM(MSG),'FROM      = ', TRIM(NML_DEPTH%FROM)
 2974                                                                     
 2975                                                                     
 2976 10  FORMAT (A,2X,A,A)
 2977 11  FORMAT (A,2X,A,I8)
 2978 13  FORMAT (A,2X,A,L1)
 2979 14  FORMAT (A,2X,A,F8.2)
 2980                         
 2981   END SUBROUTINE REPORT_DEPTH_NML
 2982                                  
 2983 !/ ------------------------------------------------------------------- /
 2984                                                                         
 2985                                                                         
 2986                                                                         
 2987                                                                         
 2988 !/ ------------------------------------------------------------------- /
 2989                                                                         
 2990   SUBROUTINE REPORT_MASK_NML (NML_MASK)
 2991 !/
 2992 !/                  +-----------------------------------+
 2993 !/                  | WAVEWATCH III           NOAA/NCEP |
 2994 !/                  |           M. Accensi              |
 2995 !/                  |                        FORTRAN 90 |
 2996 !/                  | Last update :         15-May-2018 |
 2997 !/                  +-----------------------------------+
 2998 !/
 2999 !/
 3000 !  1. Purpose :
 3001 !
 3002 !  2. Method :
 3003 !
 3004 !     See source term routines.
 3005 !
 3006 !  3. Parameters :
 3007 !
 3008 !     Parameter list
 3009 !     ----------------------------------------------------------------
 3010 !      NML_MASK  Type.
 3011 !     ----------------------------------------------------------------
 3012 !
 3013 !  4. Subroutines used :
 3014 !
 3015 !      Name      TYPE  Module   Description
 3016 !     ----------------------------------------------------------------
 3017 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 3018 !     ----------------------------------------------------------------
 3019 !
 3020 !  5. Called by :
 3021 !
 3022 !      Name      TYPE  Module   Description
 3023 !     ----------------------------------------------------------------
 3024 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 3025 !     ----------------------------------------------------------------
 
 
                                                      w3nmlgridmd.F90  page  56
 
 
 3026 !
 3027 !  6. Error messages :
 3028 !
 3029 !     None.
 3030 !
 3031 !  7. Remarks :
 3032 !
 3033 !  8. Structure :
 3034 !
 3035 !     See source code.
 3036 !
 3037 !  9. Switches :
 3038 !
 3039 ! 10. Source code :
 3040 !
 3041 !/ ------------------------------------------------------------------- /
 3042                                                                         
 3043                                                                         
 3044     IMPLICIT NONE
 3045                  
 3046     TYPE(NML_MASK_T), INTENT(IN) :: NML_MASK
 3047                                             
 3048                                             
 3049       WRITE (MSG,'(A)') 'MASK % '
 3050       WRITE (NDSN,'(A)')
 3051       WRITE (NDSN,10) TRIM(MSG),'FILENAME  = ', TRIM(NML_MASK%FILENAME)
 3052       WRITE (NDSN,11) TRIM(MSG),'IDF       = ', NML_MASK%IDF
 3053       WRITE (NDSN,11) TRIM(MSG),'IDLA      = ', NML_MASK%IDLA
 3054       WRITE (NDSN,11) TRIM(MSG),'IDFM      = ', NML_MASK%IDFM
 3055       WRITE (NDSN,10) TRIM(MSG),'FORMAT    = ', TRIM(NML_MASK%FORMAT)
 3056       WRITE (NDSN,10) TRIM(MSG),'FROM      = ', TRIM(NML_MASK%FROM)
 3057                                                                    
 3058                                                                    
 3059 10  FORMAT (A,2X,A,A)
 3060 11  FORMAT (A,2X,A,I8)
 3061 13  FORMAT (A,2X,A,L1)
 3062 14  FORMAT (A,2X,A,F8.2)
 3063                         
 3064   END SUBROUTINE REPORT_MASK_NML
 3065                                 
 3066 !/ ------------------------------------------------------------------- /
 3067                                                                         
 3068                                                                         
 3069                                                                         
 3070                                                                         
 3071                                                                         
 3072                                                                         
 3073 !/ ------------------------------------------------------------------- /
 3074                                                                         
 3075   SUBROUTINE REPORT_OBST_NML (NML_OBST)
 3076 !/
 3077 !/                  +-----------------------------------+
 3078 !/                  | WAVEWATCH III           NOAA/NCEP |
 3079 !/                  |           M. Accensi              |
 3080 !/                  |                        FORTRAN 90 |
 
 
                                                      w3nmlgridmd.F90  page  57
 
 
 3081 !/                  | Last update :         15-May-2018 |
 3082 !/                  +-----------------------------------+
 3083 !/
 3084 !/
 3085 !  1. Purpose :
 3086 !
 3087 !  2. Method :
 3088 !
 3089 !     See source term routines.
 3090 !
 3091 !  3. Parameters :
 3092 !
 3093 !     Parameter list
 3094 !     ----------------------------------------------------------------
 3095 !      NML_OBST  Type.
 3096 !     ----------------------------------------------------------------
 3097 !
 3098 !  4. Subroutines used :
 3099 !
 3100 !      Name      TYPE  Module   Description
 3101 !     ----------------------------------------------------------------
 3102 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 3103 !     ----------------------------------------------------------------
 3104 !
 3105 !  5. Called by :
 3106 !
 3107 !      Name      TYPE  Module   Description
 3108 !     ----------------------------------------------------------------
 3109 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 3110 !     ----------------------------------------------------------------
 3111 !
 3112 !  6. Error messages :
 3113 !
 3114 !     None.
 3115 !
 3116 !  7. Remarks :
 3117 !
 3118 !  8. Structure :
 3119 !
 3120 !     See source code.
 3121 !
 3122 !  9. Switches :
 3123 !
 3124 ! 10. Source code :
 3125 !
 3126 !/ ------------------------------------------------------------------- /
 3127                                                                         
 3128                                                                         
 3129     IMPLICIT NONE
 3130                  
 3131     TYPE(NML_OBST_T), INTENT(IN) :: NML_OBST
 3132                                             
 3133                                             
 3134       WRITE (MSG,'(A)') 'OBST % '
 3135       WRITE (NDSN,'(A)')
 
 
                                                      w3nmlgridmd.F90  page  58
 
 
 3136       WRITE (NDSN,14) TRIM(MSG),'SF        = ', NML_OBST%SF
 3137       WRITE (NDSN,10) TRIM(MSG),'FILENAME  = ', TRIM(NML_OBST%FILENAME)
 3138       WRITE (NDSN,11) TRIM(MSG),'IDF       = ', NML_OBST%IDF
 3139       WRITE (NDSN,11) TRIM(MSG),'IDLA      = ', NML_OBST%IDLA
 3140       WRITE (NDSN,11) TRIM(MSG),'IDFM      = ', NML_OBST%IDFM
 3141       WRITE (NDSN,10) TRIM(MSG),'FORMAT    = ', TRIM(NML_OBST%FORMAT)
 3142       WRITE (NDSN,10) TRIM(MSG),'FROM      = ', TRIM(NML_OBST%FROM)
 3143                                                                    
 3144                                                                    
 3145 10  FORMAT (A,2X,A,A)
 3146 11  FORMAT (A,2X,A,I8)
 3147 13  FORMAT (A,2X,A,L1)
 3148 14  FORMAT (A,2X,A,F8.2)
 3149                         
 3150   END SUBROUTINE REPORT_OBST_NML
 3151                                 
 3152 !/ ------------------------------------------------------------------- /
 3153                                                                         
 3154                                                                         
 3155                                                                         
 3156                                                                         
 3157                                                                         
 3158                                                                         
 3159                                                                         
 3160 !/ ------------------------------------------------------------------- /
 3161                                                                         
 3162   SUBROUTINE REPORT_SLOPE_NML (NML_SLOPE)
 3163 !/
 3164 !/                  +-----------------------------------+
 3165 !/                  | WAVEWATCH III           NOAA/NCEP |
 3166 !/                  |           M. Accensi              |
 3167 !/                  |                        FORTRAN 90 |
 3168 !/                  | Last update :         15-May-2018 |
 3169 !/                  +-----------------------------------+
 3170 !/
 3171 !/
 3172 !  1. Purpose :
 3173 !
 3174 !  2. Method :
 3175 !
 3176 !     See source term routines.
 3177 !
 3178 !  3. Parameters :
 3179 !
 3180 !     Parameter list
 3181 !     ----------------------------------------------------------------
 3182 !      NML_SLOPE  Type.
 3183 !     ----------------------------------------------------------------
 3184 !
 3185 !  4. Subroutines used :
 3186 !
 3187 !      Name      TYPE  Module   Description
 3188 !     ----------------------------------------------------------------
 3189 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 3190 !     ----------------------------------------------------------------
 
 
                                                      w3nmlgridmd.F90  page  59
 
 
 3191 !
 3192 !  5. Called by :
 3193 !
 3194 !      Name      TYPE  Module   Description
 3195 !     ----------------------------------------------------------------
 3196 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 3197 !     ----------------------------------------------------------------
 3198 !
 3199 !  6. Error messages :
 3200 !
 3201 !     None.
 3202 !
 3203 !  7. Remarks :
 3204 !
 3205 !  8. Structure :
 3206 !
 3207 !     See source code.
 3208 !
 3209 !  9. Switches :
 3210 !
 3211 ! 10. Source code :
 3212 !
 3213 !/ ------------------------------------------------------------------- /
 3214                                                                         
 3215                                                                         
 3216     IMPLICIT NONE
 3217                  
 3218     TYPE(NML_SLOPE_T), INTENT(IN) :: NML_SLOPE
 3219                                               
 3220                                               
 3221       WRITE (MSG,'(A)') 'SLOPE % '
 3222       WRITE (NDSN,'(A)')
 3223       WRITE (NDSN,14) TRIM(MSG),'SF        = ', NML_SLOPE%SF
 3224       WRITE (NDSN,10) TRIM(MSG),'FILENAME  = ', TRIM(NML_SLOPE%FILENAME)
 3225       WRITE (NDSN,11) TRIM(MSG),'IDF       = ', NML_SLOPE%IDF
 3226       WRITE (NDSN,11) TRIM(MSG),'IDLA      = ', NML_SLOPE%IDLA
 3227       WRITE (NDSN,11) TRIM(MSG),'IDFM      = ', NML_SLOPE%IDFM
 3228       WRITE (NDSN,10) TRIM(MSG),'FORMAT    = ', TRIM(NML_SLOPE%FORMAT)
 3229       WRITE (NDSN,10) TRIM(MSG),'FROM      = ', TRIM(NML_SLOPE%FROM)
 3230                                                                     
 3231                                                                     
 3232 10  FORMAT (A,2X,A,A)
 3233 11  FORMAT (A,2X,A,I8)
 3234 13  FORMAT (A,2X,A,L1)
 3235 14  FORMAT (A,2X,A,F8.2)
 3236                         
 3237   END SUBROUTINE REPORT_SLOPE_NML
 3238                                  
 3239 !/ ------------------------------------------------------------------- /
 3240                                                                         
 3241                                                                         
 3242                                                                         
 3243                                                                         
 3244                                                                         
 3245                                                                         
 
 
                                                      w3nmlgridmd.F90  page  60
 
 
 3246                                                                         
 3247                                                                         
 3248 !/ ------------------------------------------------------------------- /
 3249                                                                         
 3250   SUBROUTINE REPORT_SED_NML (NML_SED)
 3251 !/
 3252 !/                  +-----------------------------------+
 3253 !/                  | WAVEWATCH III           NOAA/NCEP |
 3254 !/                  |           M. Accensi              |
 3255 !/                  |                        FORTRAN 90 |
 3256 !/                  | Last update :         15-May-2018 |
 3257 !/                  +-----------------------------------+
 3258 !/
 3259 !/
 3260 !  1. Purpose :
 3261 !
 3262 !  2. Method :
 3263 !
 3264 !     See source term routines.
 3265 !
 3266 !  3. Parameters :
 3267 !
 3268 !     Parameter list
 3269 !     ----------------------------------------------------------------
 3270 !      NML_SED  Type.
 3271 !     ----------------------------------------------------------------
 3272 !
 3273 !  4. Subroutines used :
 3274 !
 3275 !      Name      TYPE  Module   Description
 3276 !     ----------------------------------------------------------------
 3277 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 3278 !     ----------------------------------------------------------------
 3279 !
 3280 !  5. Called by :
 3281 !
 3282 !      Name      TYPE  Module   Description
 3283 !     ----------------------------------------------------------------
 3284 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 3285 !     ----------------------------------------------------------------
 3286 !
 3287 !  6. Error messages :
 3288 !
 3289 !     None.
 3290 !
 3291 !  7. Remarks :
 3292 !
 3293 !  8. Structure :
 3294 !
 3295 !     See source code.
 3296 !
 3297 !  9. Switches :
 3298 !
 3299 ! 10. Source code :
 3300 !
 
 
                                                      w3nmlgridmd.F90  page  61
 
 
 3301 !/ ------------------------------------------------------------------- /
 3302                                                                         
 3303                                                                         
 3304     IMPLICIT NONE
 3305                  
 3306     TYPE(NML_SED_T), INTENT(IN) :: NML_SED
 3307                                           
 3308                                           
 3309       WRITE (MSG,'(A)') 'SED % '
 3310       WRITE (NDSN,'(A)')
 3311       WRITE (NDSN,14) TRIM(MSG),'SF        = ', NML_SED%SF
 3312       WRITE (NDSN,10) TRIM(MSG),'FILENAME  = ', TRIM(NML_SED%FILENAME)
 3313       WRITE (NDSN,11) TRIM(MSG),'IDF       = ', NML_SED%IDF
 3314       WRITE (NDSN,11) TRIM(MSG),'IDLA      = ', NML_SED%IDLA
 3315       WRITE (NDSN,11) TRIM(MSG),'IDFM      = ', NML_SED%IDFM
 3316       WRITE (NDSN,10) TRIM(MSG),'FORMAT    = ', TRIM(NML_SED%FORMAT)
 3317       WRITE (NDSN,10) TRIM(MSG),'FROM      = ', TRIM(NML_SED%FROM)
 3318                                                                   
 3319                                                                   
 3320 10  FORMAT (A,2X,A,A)
 3321 11  FORMAT (A,2X,A,I8)
 3322 13  FORMAT (A,2X,A,L1)
 3323 14  FORMAT (A,2X,A,F8.2)
 3324                         
 3325   END SUBROUTINE REPORT_SED_NML
 3326                                
 3327 !/ ------------------------------------------------------------------- /
 3328                                                                         
 3329                                                                         
 3330                                                                         
 3331                                                                         
 3332                                                                         
 3333                                                                         
 3334 !/ ------------------------------------------------------------------- /
 3335                                                                         
 3336   SUBROUTINE REPORT_INBOUND_NML (NML_INBND_COUNT, NML_INBND_POINT)
 3337 !/
 3338 !/                  +-----------------------------------+
 3339 !/                  | WAVEWATCH III           NOAA/NCEP |
 3340 !/                  |           M. Accensi              |
 3341 !/                  |                        FORTRAN 90 |
 3342 !/                  | Last update :         15-May-2018 |
 3343 !/                  +-----------------------------------+
 3344 !/
 3345 !/
 3346 !  1. Purpose :
 3347 !
 3348 !  2. Method :
 3349 !
 3350 !     See source term routines.
 3351 !
 3352 !  3. Parameters :
 3353 !
 3354 !     Parameter list
 3355 !     ----------------------------------------------------------------
 
 
                                                      w3nmlgridmd.F90  page  62
 
 
 3356 !      NML_INBND_COUNT  Type
 3357 !      NML_INBND_POINT  Type
 3358 !     ----------------------------------------------------------------
 3359 !
 3360 !  4. Subroutines used :
 3361 !
 3362 !      Name      TYPE  Module   Description
 3363 !     ----------------------------------------------------------------
 3364 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 3365 !     ----------------------------------------------------------------
 3366 !
 3367 !  5. Called by :
 3368 !
 3369 !      Name      TYPE  Module   Description
 3370 !     ----------------------------------------------------------------
 3371 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 3372 !     ----------------------------------------------------------------
 3373 !
 3374 !  6. Error messages :
 3375 !
 3376 !     None.
 3377 !
 3378 !  7. Remarks :
 3379 !
 3380 !  8. Structure :
 3381 !
 3382 !     See source code.
 3383 !
 3384 !  9. Switches :
 3385 !
 3386 ! 10. Source code :
 3387 !
 3388 !/ ------------------------------------------------------------------- /
 3389                                                                         
 3390                                                                         
 3391     IMPLICIT NONE
 3392                  
 3393     TYPE(NML_INBND_COUNT_T), INTENT(IN) :: NML_INBND_COUNT
 3394     TYPE(NML_INBND_POINT_T), INTENT(IN) :: NML_INBND_POINT(NML_INBND_COUN>
 3395                                                                           
 3396     ! locals
 3397     INTEGER              :: I
 3398                              
 3399                              
 3400     WRITE (MSG,'(A)') 'INBND_COUNT % '
 3401     WRITE (NDSN,'(A)')
 3402     WRITE (NDSN,11) TRIM(MSG),'N_POINT       = ', NML_INBND_COUNT%N_POINT
 3403                                                                          
 3404     IF (NML_INBND_COUNT%N_POINT .NE. 0) THEN
 3405       DO I=1,NML_INBND_COUNT%N_POINT
 3406         WRITE (MSG,'(A,I8,A)') 'INBND_POINT(',I,') % '
 3407         WRITE (NDSN,'(A)')
 3408         WRITE (NDSN,11) TRIM(MSG),'X_INDEX   = ', NML_INBND_POINT(I)%X_IN>
 3409         WRITE (NDSN,11) TRIM(MSG),'Y_INDEX   = ', NML_INBND_POINT(I)%Y_IN>
 3410         WRITE (NDSN,13) TRIM(MSG),'CONNECT   = ', NML_INBND_POINT(I)%CONN>
 
 
                                                      w3nmlgridmd.F90  page  63
 
 
 3411         WRITE (NDSN,'(A)')
 3412       END DO
 3413     END IF
 3414           
 3415 10  FORMAT (A,2X,A,A)
 3416 11  FORMAT (A,2X,A,I8)
 3417 13  FORMAT (A,2X,A,L1)
 3418 14  FORMAT (A,2X,A,F8.2)
 3419                         
 3420   END SUBROUTINE REPORT_INBOUND_NML
 3421                                    
 3422 !/ ------------------------------------------------------------------- /
 3423                                                                         
 3424                                                                         
 3425                                                                         
 3426                                                                         
 3427 !/ ------------------------------------------------------------------- /
 3428                                                                         
 3429   SUBROUTINE REPORT_EXCLUDED_NML (NML_EXCL_COUNT, NML_EXCL_POINT, NML_EXC>
 3430 !/
 3431 !/                  +-----------------------------------+
 3432 !/                  | WAVEWATCH III           NOAA/NCEP |
 3433 !/                  |           M. Accensi              |
 3434 !/                  |                        FORTRAN 90 |
 3435 !/                  | Last update :         15-May-2018 |
 3436 !/                  +-----------------------------------+
 3437 !/
 3438 !/
 3439 !  1. Purpose :
 3440 !
 3441 !  2. Method :
 3442 !
 3443 !     See source term routines.
 3444 !
 3445 !  3. Parameters :
 3446 !
 3447 !     Parameter list
 3448 !     ----------------------------------------------------------------
 3449 !      NML_EXCL_COUNT  Type
 3450 !      NML_EXCL_POINT  Type
 3451 !      NML_EXCL_BODY   Type
 3452 !     ----------------------------------------------------------------
 3453 !
 3454 !  4. Subroutines used :
 3455 !
 3456 !      Name      TYPE  Module   Description
 3457 !     ----------------------------------------------------------------
 3458 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 3459 !     ----------------------------------------------------------------
 3460 !
 3461 !  5. Called by :
 3462 !
 3463 !      Name      TYPE  Module   Description
 3464 !     ----------------------------------------------------------------
 3465 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 
 
                                                      w3nmlgridmd.F90  page  64
 
 
 3466 !     ----------------------------------------------------------------
 3467 !
 3468 !  6. Error messages :
 3469 !
 3470 !     None.
 3471 !
 3472 !  7. Remarks :
 3473 !
 3474 !  8. Structure :
 3475 !
 3476 !     See source code.
 3477 !
 3478 !  9. Switches :
 3479 !
 3480 ! 10. Source code :
 3481 !
 3482 !/ ------------------------------------------------------------------- /
 3483                                                                         
 3484                                                                         
 3485     IMPLICIT NONE
 3486                  
 3487     TYPE(NML_EXCL_COUNT_T), INTENT(IN) :: NML_EXCL_COUNT
 3488     TYPE(NML_EXCL_POINT_T), INTENT(IN) :: NML_EXCL_POINT(NML_EXCL_COUNT%N>
 3489     TYPE(NML_EXCL_BODY_T), INTENT(IN)  :: NML_EXCL_BODY(NML_EXCL_COUNT%N_>
 3490                                                                           
 3491     ! locals
 3492     INTEGER              :: I
 3493                              
 3494                              
 3495     WRITE (MSG,'(A)') 'EXCL_COUNT % '
 3496     WRITE (NDSN,'(A)')
 3497     WRITE (NDSN,11) TRIM(MSG),'N_POINT       = ', NML_EXCL_COUNT%N_POINT
 3498     WRITE (NDSN,11) TRIM(MSG),'N_BODY        = ', NML_EXCL_COUNT%N_BODY
 3499                                                                        
 3500     IF (NML_EXCL_COUNT%N_POINT .NE. 0) THEN
 3501       DO I=1,NML_EXCL_COUNT%N_POINT
 3502         WRITE (MSG,'(A,I8,A)') 'EXCL_POINT(',I,') % '
 3503         WRITE (NDSN,'(A)')
 3504         WRITE (NDSN,11) TRIM(MSG),'X_INDEX   = ', NML_EXCL_POINT(I)%X_IND>
 3505         WRITE (NDSN,11) TRIM(MSG),'Y_INDEX   = ', NML_EXCL_POINT(I)%Y_IND>
 3506         WRITE (NDSN,13) TRIM(MSG),'CONNECT   = ', NML_EXCL_POINT(I)%CONNE>
 3507         WRITE (NDSN,'(A)')
 3508       END DO
 3509     END IF
 3510           
 3511     IF (NML_EXCL_COUNT%N_BODY .NE. 0) THEN
 3512       DO I=1,NML_EXCL_COUNT%N_BODY
 3513         WRITE (MSG,'(A,I8,A)') 'EXCL_BODY(',I,') % '
 3514         WRITE (NDSN,'(A)')
 3515         WRITE (NDSN,11) TRIM(MSG),'X_INDEX   = ', NML_EXCL_BODY(I)%X_INDEX
 3516         WRITE (NDSN,11) TRIM(MSG),'Y_INDEX   = ', NML_EXCL_BODY(I)%Y_INDEX
 3517         WRITE (NDSN,'(A)')
 3518       END DO
 3519     END IF
 3520           
 
 
                                                      w3nmlgridmd.F90  page  65
 
 
 3521 10  FORMAT (A,2X,A,A)
 3522 11  FORMAT (A,2X,A,I8)
 3523 13  FORMAT (A,2X,A,L1)
 3524 14  FORMAT (A,2X,A,F12.2)
 3525                          
 3526   END SUBROUTINE REPORT_EXCLUDED_NML
 3527                                     
 3528 !/ ------------------------------------------------------------------- /
 3529                                                                         
 3530                                                                         
 3531                                                                         
 3532                                                                         
 3533 !/ ------------------------------------------------------------------- /
 3534                                                                         
 3535   SUBROUTINE REPORT_OUTBOUND_NML (NML_OUTBND_COUNT, NML_OUTBND_LINE)
 3536 !/
 3537 !/                  +-----------------------------------+
 3538 !/                  | WAVEWATCH III           NOAA/NCEP |
 3539 !/                  |           M. Accensi              |
 3540 !/                  |                        FORTRAN 90 |
 3541 !/                  | Last update :         15-May-2018 |
 3542 !/                  +-----------------------------------+
 3543 !/
 3544 !/
 3545 !  1. Purpose :
 3546 !
 3547 !  2. Method :
 3548 !
 3549 !     See source term routines.
 3550 !
 3551 !  3. Parameters :
 3552 !
 3553 !     Parameter list
 3554 !     ----------------------------------------------------------------
 3555 !      NML_OUTBND_COUNT  Type
 3556 !      NML_OUTBND_LINE   Type
 3557 !     ----------------------------------------------------------------
 3558 !
 3559 !  4. Subroutines used :
 3560 !
 3561 !      Name      TYPE  Module   Description
 3562 !     ----------------------------------------------------------------
 3563 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 3564 !     ----------------------------------------------------------------
 3565 !
 3566 !  5. Called by :
 3567 !
 3568 !      Name      TYPE  Module   Description
 3569 !     ----------------------------------------------------------------
 3570 !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
 3571 !     ----------------------------------------------------------------
 3572 !
 3573 !  6. Error messages :
 3574 !
 3575 !     None.
 
 
                                                      w3nmlgridmd.F90  page  66
 
 
 3576 !
 3577 !  7. Remarks :
 3578 !
 3579 !  8. Structure :
 3580 !
 3581 !     See source code.
 3582 !
 3583 !  9. Switches :
 3584 !
 3585 ! 10. Source code :
 3586 !
 3587 !/ ------------------------------------------------------------------- /
 3588                                                                         
 3589                                                                         
 3590     IMPLICIT NONE
 3591                  
 3592     TYPE(NML_OUTBND_COUNT_T), INTENT(IN) :: NML_OUTBND_COUNT
 3593     TYPE(NML_OUTBND_LINE_T), INTENT(IN)  :: NML_OUTBND_LINE(NML_OUTBND_CO>
 3594                                                                           
 3595     ! locals
 3596     INTEGER              :: I
 3597                              
 3598                              
 3599     WRITE (MSG,'(A)') 'OUTBND_COUNT % '
 3600     WRITE (NDSN,'(A)')
 3601     WRITE (NDSN,11) TRIM(MSG),'N_LINE        = ', NML_OUTBND_COUNT%N_LINE
 3602                                                                          
 3603     IF (NML_OUTBND_COUNT%N_LINE .NE. 0) THEN
 3604       DO I=1,NML_OUTBND_COUNT%N_LINE
 3605         WRITE (MSG,'(A,I8,A)') 'OUTBND_LINE(',I,') % '
 3606         WRITE (NDSN,'(A)')
 3607         WRITE (NDSN,14) TRIM(MSG),'X0        = ', NML_OUTBND_LINE(I)%X0
 3608         WRITE (NDSN,14) TRIM(MSG),'Y0        = ', NML_OUTBND_LINE(I)%Y0
 3609         WRITE (NDSN,14) TRIM(MSG),'DX        = ', NML_OUTBND_LINE(I)%DX
 3610         WRITE (NDSN,14) TRIM(MSG),'DY        = ', NML_OUTBND_LINE(I)%DY
 3611         WRITE (NDSN,11) TRIM(MSG),'NP        = ', NML_OUTBND_LINE(I)%NP
 3612         WRITE (NDSN,'(A)')
 3613       END DO
 3614     END IF
 3615           
 3616 10  FORMAT (A,2X,A,A)
 3617 11  FORMAT (A,2X,A,I8)
 3618 13  FORMAT (A,2X,A,L1)
 3619 14  FORMAT (A,2X,A,F12.2)
 3620                          
 3621   END SUBROUTINE REPORT_OUTBOUND_NML
 3622                                     
 3623 !/ ------------------------------------------------------------------- /
 3624                                                                         
 3625                                                                         
 3626                                                                         
 3627                                                                         
 3628                                                                         
 3629                                                                         
 3630                                                                         
 
 
                                                      w3nmlgridmd.F90  page  67
 
 
 3631                                                                         
 3632 END MODULE W3NMLGRIDMD
 3633                       
 3634 !/ ------------------------------------------------------------------- /
 3635                                                                         
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
