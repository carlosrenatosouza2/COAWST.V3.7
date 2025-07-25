                                                     w3nmlmultimd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3NMLMULTIMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           M. Accensi              |
    8 !/                  |                                   |
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :         25-Sep-2020 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    For updates see subroutines.
   14 !/
   15 !  1. Purpose :
   16 !
   17 !     Manages namelists from configuration file ww3_multi.nml for ww3_mul>
   18 !
   19 !/ ------------------------------------------------------------------- /
   20                                                                         
   21   ! module defaults
   22   IMPLICIT NONE
   23                
   24   PUBLIC
   25         
   26   ! domain definition structure
   27   TYPE NML_DOMAIN_T
   28     INTEGER                     :: NRINP
   29     INTEGER                     :: NRGRD
   30     LOGICAL                     :: UNIPTS
   31     INTEGER                     :: IOSTYP
   32     LOGICAL                     :: UPPROC
   33     LOGICAL                     :: PSHARE
   34     LOGICAL                     :: FLGHG1
   35     LOGICAL                     :: FLGHG2
   36     CHARACTER(15)               :: START
   37     CHARACTER(15)               :: STOP
   38   END TYPE NML_DOMAIN_T
   39                        
   40                        
   41                        
   42   ! model grid data structure
   43   TYPE NML_MODEL_FORCING_T
   44     CHARACTER(13)               :: WATER_LEVELS
   45     CHARACTER(13)               :: CURRENTS
   46     CHARACTER(13)               :: WINDS
   47     CHARACTER(13)               :: ICE_CONC
   48     CHARACTER(13)               :: ICE_PARAM1
   49     CHARACTER(13)               :: ICE_PARAM2
   50     CHARACTER(13)               :: ICE_PARAM3
   51     CHARACTER(13)               :: ICE_PARAM4
   52     CHARACTER(13)               :: ICE_PARAM5
   53     CHARACTER(13)               :: MUD_DENSITY
   54     CHARACTER(13)               :: MUD_THICKNESS
   55     CHARACTER(13)               :: MUD_VISCOSITY
 
 
                                                     w3nmlmultimd.F90  page   2
 
 
   56   END TYPE NML_MODEL_FORCING_T
   57 !
   58   TYPE NML_MODEL_ASSIM_T
   59     CHARACTER(13)               :: MEAN
   60     CHARACTER(13)               :: SPEC1D
   61     CHARACTER(13)               :: SPEC2D
   62   END TYPE NML_MODEL_ASSIM_T
   63 !
   64   TYPE NML_MODEL_RESOURCE_T
   65     INTEGER                     :: RANK_ID
   66     INTEGER                     :: GROUP_ID
   67     REAL(4)                     :: COMM_FRAC(2)
   68     LOGICAL                     :: BOUND_FLAG
   69   END TYPE NML_MODEL_RESOURCE_T
   70 !
   71   TYPE NML_MODEL_GRID_T
   72     CHARACTER(13)               :: NAME
   73     TYPE(NML_MODEL_FORCING_T)   :: FORCING
   74     TYPE(NML_MODEL_ASSIM_T)     :: ASSIM
   75     TYPE(NML_MODEL_RESOURCE_T)  :: RESOURCE
   76   END TYPE NML_MODEL_GRID_T
   77                            
   78                            
   79                            
   80   ! input grid data structure
   81   TYPE NML_INPUT_FORCING_T
   82     LOGICAL                     :: WATER_LEVELS
   83     LOGICAL                     :: CURRENTS
   84     LOGICAL                     :: WINDS
   85     LOGICAL                     :: ICE_CONC
   86     LOGICAL                     :: ICE_PARAM1
   87     LOGICAL                     :: ICE_PARAM2
   88     LOGICAL                     :: ICE_PARAM3
   89     LOGICAL                     :: ICE_PARAM4
   90     LOGICAL                     :: ICE_PARAM5
   91     LOGICAL                     :: MUD_DENSITY
   92     LOGICAL                     :: MUD_THICKNESS
   93     LOGICAL                     :: MUD_VISCOSITY
   94   END TYPE NML_INPUT_FORCING_T
   95 !
   96   TYPE NML_INPUT_ASSIM_T
   97     LOGICAL                     :: MEAN
   98     LOGICAL                     :: SPEC1D
   99     LOGICAL                     :: SPEC2D
  100   END TYPE NML_INPUT_ASSIM_T
  101 !
  102   TYPE NML_INPUT_GRID_T
  103     CHARACTER(13)               :: NAME
  104     TYPE(NML_INPUT_FORCING_T)   :: FORCING
  105     TYPE(NML_INPUT_ASSIM_T)     :: ASSIM
  106   END TYPE NML_INPUT_GRID_T
  107                            
  108                            
  109                            
  110   ! output type structure
 
 
                                                     w3nmlmultimd.F90  page   3
 
 
  111   TYPE NML_FIELD_T
  112     CHARACTER(1024)             :: LIST
  113   END TYPE NML_FIELD_T
  114 !
  115   TYPE NML_POINT_T
  116     CHARACTER(13)               :: NAME
  117     CHARACTER(64)               :: FILE
  118   END TYPE NML_POINT_T
  119 !
  120   TYPE NML_TRACK_T
  121     LOGICAL                     :: FORMAT
  122   END TYPE NML_TRACK_T
  123 !
  124   TYPE NML_PARTITION_T
  125     INTEGER                     :: X0
  126     INTEGER                     :: XN
  127     INTEGER                     :: NX
  128     INTEGER                     :: Y0
  129     INTEGER                     :: YN
  130     INTEGER                     :: NY
  131     LOGICAL                     :: FORMAT
  132   END TYPE NML_PARTITION_T
  133 !
  134   TYPE NML_RESTART_T
  135     CHARACTER(1024)             :: EXTRA
  136   END TYPE NML_RESTART_T
  137 !
  138   TYPE NML_OUTPUT_TYPE_T
  139     TYPE(NML_POINT_T)           :: POINT
  140     TYPE(NML_FIELD_T)           :: FIELD
  141     TYPE(NML_TRACK_T)           :: TRACK
  142     TYPE(NML_PARTITION_T)       :: PARTITION
  143     TYPE(NML_RESTART_T)         :: RESTART
  144   END TYPE NML_OUTPUT_TYPE_T
  145                             
  146                             
  147                             
  148   ! output date structure
  149   TYPE NML_OUTPUT_TIME_T
  150     CHARACTER(15)               :: START
  151     CHARACTER(15)               :: STRIDE
  152     CHARACTER(15)               :: STOP
  153     CHARACTER(1)                :: OUTFFILE
  154 !
  155   END TYPE NML_OUTPUT_TIME_T
  156 !
  157   TYPE NML_OUTPUT_DATE_T
  158     TYPE(NML_OUTPUT_TIME_T)     :: FIELD
  159     TYPE(NML_OUTPUT_TIME_T)     :: POINT
  160     TYPE(NML_OUTPUT_TIME_T)     :: TRACK
  161     TYPE(NML_OUTPUT_TIME_T)     :: RESTART
  162     TYPE(NML_OUTPUT_TIME_T)     :: RESTART2
  163     TYPE(NML_OUTPUT_TIME_T)     :: BOUNDARY
  164     TYPE(NML_OUTPUT_TIME_T)     :: PARTITION
  165   END TYPE NML_OUTPUT_DATE_T
 
 
                                                     w3nmlmultimd.F90  page   4
 
 
  166                             
  167                             
  168                             
  169   ! homogeneous input structure
  170   TYPE NML_HOMOG_COUNT_T
  171     INTEGER                     :: N_MOV
  172     INTEGER                     :: N_TOT
  173   END TYPE NML_HOMOG_COUNT_T
  174 !
  175   TYPE NML_HOMOG_INPUT_T
  176     CHARACTER(15)               :: NAME
  177     CHARACTER(15)               :: DATE
  178     REAL                        :: VALUE1
  179     REAL                        :: VALUE2
  180     REAL                        :: VALUE3
  181   END TYPE NML_HOMOG_INPUT_T
  182                             
  183                             
  184   ! miscellaneous
  185   CHARACTER(256)                :: MSG
  186   INTEGER                       :: NDSN
  187                                        
  188                                        
  189                                        
  190   CONTAINS
  191 !/ ------------------------------------------------------------------- /
  192   SUBROUTINE W3NMLMULTIDEF (MPI_COMM, NDSI, INFILE, NML_DOMAIN, IERR)
  193 !/
  194 !/                  +-----------------------------------+
  195 !/                  | WAVEWATCH III           NOAA/NCEP |
  196 !/                  |           M. Accensi              |
  197 !/                  |           F. Ardhuin              |
  198 !/                  |                        FORTRAN 90 |
  199 !/                  | Last update :         15-May-2018 |
  200 !/                  +-----------------------------------+
  201 !/
  202 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
  203 !/    15-May-2018 : Update namelist                     ( version 6.05 )
  204 !
  205 !  1. Purpose :
  206 !
  207 !     Reads the domain definition namelist to define the number of
  208 !     model and forcing grids
  209 !
  210 !  2. Method :
  211 !
  212 !     See source term routines.
  213 !
  214 !  3. Parameters :
  215 !
  216 !     Parameter list
  217 !     ----------------------------------------------------------------
  218 !      MPI_COMM   Int.  Public   Communicator used in the wave MODEL.
  219 !      NDSI       Int.
  220 !      INFILE     Char.
 
 
                                                     w3nmlmultimd.F90  page   5
 
 
  221 !      NML_DOMAIN type.
  222 !      IERR       Int.
  223 !     ----------------------------------------------------------------
  224 !
  225 !  4. Subroutines used :
  226 !
  227 !      Name      Type  Module   Description
  228 !     ----------------------------------------------------------------
  229 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  230 !      READ_DOMAIN_NML
  231 !     ----------------------------------------------------------------
  232 !
  233 !  5. Called by :
  234 !
  235 !      Name      Type  Module   Description
  236 !     ----------------------------------------------------------------
  237 !      WMINITNML Subr.   N/A    Model configuration routine.
  238 !     ----------------------------------------------------------------
  239 !
  240 !  6. Error messages :
  241 !
  242 !     None.
  243 !
  244 !  7. Remarks :
  245 !
  246 !  8. Structure :
  247 !
  248 !     See source code.
  249 !
  250 !  9. Switches :
  251 !
  252 !     !/MPI  Uses MPI communications
  253 !
  254 ! 10. Source code :
  255 !
  256 !/ ------------------------------------------------------------------- /
  257     USE WMMDATMD, ONLY: MDSE, IMPROC, NMPLOG
  258       USE WMMDATMD, ONLY: MPI_COMM_MWAVE
  259                                         
  260     IMPLICIT NONE
  261                  
  262     INTEGER, INTENT(IN)                       :: MPI_COMM, NDSI
  263     CHARACTER*(*), INTENT(IN)                 :: INFILE
  264     TYPE(NML_DOMAIN_T), INTENT(OUT)           :: NML_DOMAIN
  265     INTEGER, INTENT(OUT)                      :: IERR
  266                                                      
  267     ! locals
  268       INTEGER                                 :: IERR_MPI
  269                                                          
  270     IERR = 0
  271             
  272       MPI_COMM_MWAVE = MPI_COMM
  273       CALL MPI_COMM_RANK ( MPI_COMM_MWAVE, IMPROC, IERR_MPI )
  274       IMPROC = IMPROC + 1
  275                          
 
 
                                                     w3nmlmultimd.F90  page   6
 
 
  276     ! open namelist log file
  277     IF ( NMPLOG .EQ. IMPROC ) THEN
  278       NDSN = 3
  279       OPEN (NDSN, file=TRIM(INFILE)//'.log', form='formatted', iostat=IER>
  280       IF (IERR.NE.0) THEN
  281         WRITE (MDSE,'(A)') 'ERROR: open full nml file '//TRIM(INFILE)//'.>
  282         RETURN
  283       END IF
  284     END IF
  285           
  286     ! open input file
  287     OPEN (NDSI, FILE=TRIM(INFILE), form='formatted', status='old', iostat>
  288     IF (IERR.NE.0) THEN
  289       WRITE (MDSE,'(A)') 'ERROR: open input file '//TRIM(INFILE)//' faile>
  290       RETURN
  291     END IF
  292           
  293     ! read domain def namelist
  294     CALL READ_DOMAIN_NML (NDSI, NML_DOMAIN)
  295     IF ( NMPLOG .EQ. IMPROC ) CALL REPORT_DOMAIN_NML (NML_DOMAIN)
  296                                                                  
  297     ! close namelist files
  298     CLOSE (NDSI)
  299     IF ( NMPLOG .EQ. IMPROC ) CLOSE (NDSN)
  300                                           
  301   END SUBROUTINE W3NMLMULTIDEF
  302                               
  303 !/ ------------------------------------------------------------------- /
  304                                                                         
  305                                                                         
  306                                                                         
  307                                                                         
  308                                                                         
  309                                                                         
  310 !/ ------------------------------------------------------------------- /
  311                                                                         
  312   SUBROUTINE W3NMLMULTICONF (MPI_COMM, NDSI, INFILE, NML_DOMAIN,       &
  313                              NML_INPUT_GRID, NML_MODEL_GRID,           &
  314                              NML_OUTPUT_TYPE, NML_OUTPUT_DATE,         &
  315                              NML_HOMOG_COUNT, NML_HOMOG_INPUT, IERR)
  316 !/
  317 !/                  +-----------------------------------+
  318 !/                  | WAVEWATCH III           NOAA/NCEP |
  319 !/                  |           M. Accensi              |
  320 !/                  |           F. Ardhuin              |
  321 !/                  |                        FORTRAN 90 |
  322 !/                  | Last update :         15-May-2018 |
  323 !/                  +-----------------------------------+
  324 !/
  325 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
  326 !/    15-May-2018 : Update namelist                     ( version 6.05 )
  327                                                                         
  328 !  1. Purpose :
  329 !
  330 !     Reads all the namelist to define the multi grid
 
 
                                                     w3nmlmultimd.F90  page   7
 
 
  331 !
  332 !  2. Method :
  333 !
  334 !     See source term routines.
  335 !
  336 !  3. Parameters :
  337 !
  338 !     Parameter list
  339 !     ----------------------------------------------------------------
  340 !      MPI_COMM  Int.  Public   Communicator used in the wave MODEL.
  341 !      NDSI
  342 !      INFILE
  343 !      NML_DOMAIN
  344 !      NML_INPUT_GRID
  345 !      NML_MODEL_GRID
  346 !      NML_OUTPUT_TYPE
  347 !      NML_OUTPUT_DATE
  348 !      NML_HOMOG_COUNT
  349 !      NML_HOMOG_INPUT
  350 !      IERR
  351 !     ----------------------------------------------------------------
  352 !
  353 !  4. Subroutines used :
  354 !
  355 !      Name      Type  Module   Description
  356 !     ----------------------------------------------------------------
  357 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  358 !      READ_INPUT_GRID_NML
  359 !      REPORT_INPUT_GRID_NML
  360 !      READ_MODEL_GRID_NML
  361 !      REPORT_MODEL_GRID_NML
  362 !      READ_OUTPUT_TYPE_NML
  363 !      REPORT_OUTPUT_TYPE_NML
  364 !      READ_OUTPUT_DATE_NML
  365 !      REPORT_OUTPUT_DATE_NML
  366 !      READ_HOMOGENEOUS_NML
  367 !      REPORT_HOMOGENEOUS_NML
  368 !     ----------------------------------------------------------------
  369 !
  370 !  5. Called by :
  371 !
  372 !      Name      Type  Module   Description
  373 !     ----------------------------------------------------------------
  374 !      WMINITNML Subr.   N/A    Model configuration routine.
  375 !     ----------------------------------------------------------------
  376 !
  377 !  6. Error messages :
  378 !
  379 !     None.
  380 !
  381 !  7. Remarks :
  382 !
  383 !  8. Structure :
  384 !
  385 !     See source code.
 
 
                                                     w3nmlmultimd.F90  page   8
 
 
  386 !
  387 !  9. Switches :
  388 !
  389 !     !/MPI  Uses MPI communications
  390 !
  391 ! 10. Source code :
  392 !
  393 !/ ------------------------------------------------------------------- /
  394                                                                         
  395     USE WMMDATMD, ONLY: MDSE, IMPROC, NMPLOG
  396       USE WMMDATMD, ONLY: MPI_COMM_MWAVE
  397                                         
  398     IMPLICIT NONE
  399                  
  400     INTEGER, INTENT(IN)                       :: MPI_COMM, NDSI
  401     CHARACTER*(*), INTENT(IN)                 :: INFILE
  402     TYPE(NML_DOMAIN_T), INTENT(INOUT)         :: NML_DOMAIN
  403     TYPE(NML_INPUT_GRID_T), INTENT(INOUT)     :: NML_INPUT_GRID(:)
  404     TYPE(NML_MODEL_GRID_T), INTENT(INOUT)     :: NML_MODEL_GRID(:)
  405     TYPE(NML_OUTPUT_TYPE_T), INTENT(INOUT)    :: NML_OUTPUT_TYPE(:)
  406     TYPE(NML_OUTPUT_DATE_T), INTENT(INOUT)    :: NML_OUTPUT_DATE(:)
  407     TYPE(NML_HOMOG_COUNT_T), INTENT(INOUT)    :: NML_HOMOG_COUNT
  408     TYPE(NML_HOMOG_INPUT_T), ALLOCATABLE, INTENT(INOUT)    :: NML_HOMOG_I>
  409     INTEGER, INTENT(OUT)                      :: IERR
  410                                                      
  411     ! locals
  412       INTEGER                                 :: IERR_MPI
  413                                                          
  414     IERR = 0
  415             
  416       MPI_COMM_MWAVE = MPI_COMM
  417       CALL MPI_COMM_RANK ( MPI_COMM_MWAVE, IMPROC, IERR_MPI )
  418       IMPROC = IMPROC + 1
  419                          
  420     ! open namelist log file
  421     IF ( NMPLOG .EQ. IMPROC ) THEN
  422       NDSN = 3
  423       OPEN (NDSN, file=TRIM(INFILE)//'.log', form='formatted', status='ol>
  424       IF (IERR.NE.0) THEN
  425         WRITE (MDSE,'(A)') 'ERROR: open full nml file '//TRIM(INFILE)//'.>
  426         RETURN
  427       END IF
  428     END IF
  429           
  430     ! open input file
  431     OPEN (NDSI, FILE=TRIM(INFILE), form='formatted', status='old', iostat>
  432     IF (IERR.NE.0) THEN
  433       WRITE (MDSE,'(A)') 'ERROR: open INPUT file '//TRIM(INFILE)//' faile>
  434       RETURN
  435     END IF
  436           
  437     ! read input grid namelist
  438     CALL READ_INPUT_GRID_NML (NDSI, NML_DOMAIN%NRINP, NML_INPUT_GRID)
  439     IF ( NMPLOG .EQ. IMPROC ) CALL REPORT_INPUT_GRID_NML (NML_DOMAIN%NRIN>
  440                                                                           
 
 
                                                     w3nmlmultimd.F90  page   9
 
 
  441     ! read model grid namelist
  442     CALL READ_MODEL_GRID_NML (NDSI, NML_DOMAIN%NRGRD, NML_MODEL_GRID)
  443     IF ( NMPLOG .EQ. IMPROC ) CALL REPORT_MODEL_GRID_NML (NML_DOMAIN%NRGR>
  444                                                                           
  445     ! read output type namelist
  446     CALL READ_OUTPUT_TYPE_NML (NDSI, NML_DOMAIN%NRGRD, NML_OUTPUT_TYPE)
  447     IF ( NMPLOG .EQ. IMPROC ) CALL REPORT_OUTPUT_TYPE_NML (NML_DOMAIN%NRG>
  448                                                                           
  449     ! read output date namelist
  450     CALL READ_OUTPUT_DATE_NML (NDSI, NML_DOMAIN%NRGRD, NML_OUTPUT_DATE)
  451     IF ( NMPLOG .EQ. IMPROC ) CALL REPORT_OUTPUT_DATE_NML (NML_DOMAIN%NRG>
  452                                                                           
  453     ! read homogeneous namelist
  454     CALL READ_HOMOGENEOUS_NML (NDSI, NML_HOMOG_COUNT, NML_HOMOG_INPUT)
  455     IF ( NMPLOG .EQ. IMPROC ) CALL REPORT_HOMOGENEOUS_NML (NML_HOMOG_COUN>
  456                                                                           
  457                                                                           
  458     ! close namelist files
  459     CLOSE (NDSI)
  460     IF ( NMPLOG .EQ. IMPROC ) CLOSE (NDSN)
  461                                           
  462   END SUBROUTINE W3NMLMULTICONF
  463                                
  464 !/ ------------------------------------------------------------------- /
  465                                                                         
  466                                                                         
  467                                                                         
  468                                                                         
  469                                                                         
  470                                                                         
  471 !/ ------------------------------------------------------------------- /
  472                                                                         
  473   SUBROUTINE READ_DOMAIN_NML (NDSI, NML_DOMAIN)
  474 !/
  475 !/                  +-----------------------------------+
  476 !/                  | WAVEWATCH III           NOAA/NCEP |
  477 !/                  |           M. Accensi              |
  478 !/                  |           F. Ardhuin              |
  479 !/                  |                        FORTRAN 90 |
  480 !/                  | Last update :         15-May-2018 |
  481 !/                  +-----------------------------------+
  482 !/
  483 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
  484 !/    15-May-2018 : Update namelist                     ( version 6.05 )
  485                                                                         
  486 !  1. Purpose :
  487 !
  488 !  2. Method :
  489 !
  490 !     See source term routines.
  491 !
  492 !  3. Parameters :
  493 !
  494 !     Parameter list
  495 !     ----------------------------------------------------------------
 
 
                                                     w3nmlmultimd.F90  page  10
 
 
  496 !      NDSI            Int.
  497 !      NML_DOMAIN      Type.
  498 !     ----------------------------------------------------------------
  499 !
  500 !  4. Subroutines used :
  501 !
  502 !      Name      Type  Module   Description
  503 !     ----------------------------------------------------------------
  504 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  505 !     ----------------------------------------------------------------
  506 !
  507 !  5. Called by :
  508 !
  509 !      Name      Type  Module   Description
  510 !     ----------------------------------------------------------------
  511 !      W3NMLMULTIDEF Subr.   N/A    Namelist configuration routine.
  512 !     ----------------------------------------------------------------
  513 !
  514 !  6. Error messages :
  515 !
  516 !     None.
  517 !
  518 !  7. Remarks :
  519 !
  520 !  8. Structure :
  521 !
  522 !     See source code.
  523 !
  524 !  9. Switches :
  525 !
  526 !     !/MPI  Uses MPI communications
  527 !
  528 ! 10. Source code :
  529 !
  530 !/ ------------------------------------------------------------------- /
  531                                                                         
  532     USE WMMDATMD, ONLY: MDSE
  533     USE W3SERVMD, ONLY: EXTCDE
  534                               
  535     IMPLICIT NONE
  536                  
  537     INTEGER, INTENT(IN)               :: NDSI
  538     TYPE(NML_DOMAIN_T), INTENT(OUT)   :: NML_DOMAIN
  539                                                    
  540     ! locals
  541     INTEGER                   :: IERR
  542     TYPE(NML_DOMAIN_T) :: DOMAIN
  543     NAMELIST /DOMAIN_NML/ DOMAIN
  544                                 
  545     IERR = 0
  546             
  547     ! set default values for model definition data
  548     DOMAIN%NRINP  = 0
  549     DOMAIN%NRGRD  = 1
  550     DOMAIN%UNIPTS = .FALSE.
 
 
                                                     w3nmlmultimd.F90  page  11
 
 
  551     DOMAIN%IOSTYP = 1
  552     DOMAIN%UPPROC = .FALSE.
  553     DOMAIN%PSHARE = .FALSE.
  554     DOMAIN%FLGHG1 = .FALSE.
  555     DOMAIN%FLGHG2 = .FALSE.
  556     DOMAIN%START  = '19680606 000000'
  557     DOMAIN%STOP   = '19680607 000000'
  558                                      
  559     ! read model def namelist
  560     REWIND (NDSI)
  561     READ (NDSI, nml=DOMAIN_NML, iostat=IERR, iomsg=MSG)
  562     IF (IERR.GT.0) THEN
  563       WRITE (MDSE,'(A,/A)') &
  564         'ERROR: READ_DOMAIN_NML: namelist read error', &
  565         'ERROR: '//TRIM(MSG)
  566       CALL EXTCDE (1)
  567     END IF
  568           
  569     ! set/check return values
  570     IF (DOMAIN%NRINP.LT.0) THEN
  571       WRITE (MDSE,'(A,I3)') 'ERROR: BAD NRINP INPUT: ',DOMAIN%NRINP
  572       CALL EXTCDE (2)
  573     END IF
  574     IF (DOMAIN%NRGRD.LE.0) THEN
  575       WRITE (MDSE,'(A,I3)') 'ERROR: BAD NRGRD INPUT: ',DOMAIN%NRGRD
  576       CALL EXTCDE (3)
  577     END IF
  578     IF (DOMAIN%IOSTYP.LT.0.or.DOMAIN%IOSTYP.GT.3) THEN
  579       WRITE (MDSE,'(A,I3)') 'ERROR: BAD IOSTYP INPUT: ',DOMAIN%IOSTYP
  580       CALL EXTCDE (4)
  581     END IF
  582           
  583     ! save namelist
  584     NML_DOMAIN = DOMAIN
  585                        
  586   END SUBROUTINE READ_DOMAIN_NML
  587                                 
  588 !/ ------------------------------------------------------------------- /
  589                                                                         
  590                                                                         
  591                                                                         
  592                                                                         
  593                                                                         
  594                                                                         
  595 !/ ------------------------------------------------------------------- /
  596                                                                         
  597   SUBROUTINE READ_INPUT_GRID_NML (NDSI, NRINP, NML_INPUT_GRID)
  598 !/
  599 !/                  +-----------------------------------+
  600 !/                  | WAVEWATCH III           NOAA/NCEP |
  601 !/                  |           M. Accensi              |
  602 !/                  |                        FORTRAN 90 |
  603 !/                  | Last update :         15-May-2018 |
  604 !/                  +-----------------------------------+
  605 !/
 
 
                                                     w3nmlmultimd.F90  page  12
 
 
  606 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
  607 !/    15-May-2018 : Update namelist                     ( version 6.05 )
  608 !/
  609 !  1. Purpose :
  610 !
  611 !  2. Method :
  612 !
  613 !     See source term routines.
  614 !
  615 !  3. Parameters :
  616 !
  617 !     Parameter list
  618 !     ----------------------------------------------------------------
  619 !      NDSI              Int.
  620 !      NRINP             Int.
  621 !      NML_INPUT_GRID    Type.
  622 !     ----------------------------------------------------------------
  623 !
  624 !  4. Subroutines used :
  625 !
  626 !      Name      Type  Module   Description
  627 !     ----------------------------------------------------------------
  628 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  629 !     ----------------------------------------------------------------
  630 !
  631 !  5. Called by :
  632 !
  633 !      Name      Type  Module   Description
  634 !     ----------------------------------------------------------------
  635 !      W3NMLMULTICONF Subr.   N/A    Namelist configuration routine.
  636 !     ----------------------------------------------------------------
  637 !
  638 !  6. Error messages :
  639 !
  640 !     None.
  641 !
  642 !  7. Remarks :
  643 !
  644 !  8. Structure :
  645 !
  646 !     See source code.
  647 !
  648 !  9. Switches :
  649 !
  650 !     !/MPI  Uses MPI communications
  651 !
  652 ! 10. Source code :
  653 !
  654 !/ ------------------------------------------------------------------- /
  655                                                                         
  656     USE WMMDATMD, ONLY: MDSE
  657     USE W3SERVMD, ONLY: EXTCDE
  658                               
  659     IMPLICIT NONE
  660                  
 
 
                                                     w3nmlmultimd.F90  page  13
 
 
  661     INTEGER, INTENT(IN)               :: NDSI, NRINP
  662     TYPE(NML_INPUT_GRID_T), INTENT(INOUT) :: NML_INPUT_GRID(NRINP)
  663                                                                   
  664     ! locals
  665     INTEGER                   :: IERR, I
  666     INTEGER, PARAMETER :: MAX_NRINP = 99
  667     TYPE(NML_INPUT_GRID_T) :: INPUT(MAX_NRINP)
  668     NAMELIST /INPUT_GRID_NML/ INPUT
  669                                    
  670     IERR = 0
  671             
  672     ! test NRINP
  673     IF (NRINP.GT.MAX_NRINP) THEN
  674       WRITE (MDSE,'(A,/A,I6,/A,I6)') &
  675         'ERROR: READ_INPUT_GRID_NML: NRINP > MAX_NRINP',      &
  676         'ERROR: READ_INPUT_GRID_NML:     NRINP = ',    NRINP, &
  677         'ERROR: READ_INPUT_GRID_NML: MAX_NRINP = ',MAX_NRINP
  678       CALL EXTCDE (6)
  679     END IF
  680           
  681     ! if no input grids, then exit
  682     IF (NRINP.EQ.0) RETURN
  683                           
  684     ! set default values for grid INPUT data
  685     DO I = 1,NRINP
  686       INPUT(I)%NAME                  = 'unset'
  687       INPUT(I)%FORCING%WATER_LEVELS  = .FALSE.
  688       INPUT(I)%FORCING%CURRENTS      = .FALSE.
  689       INPUT(I)%FORCING%WINDS         = .FALSE.
  690       INPUT(I)%FORCING%ICE_CONC      = .FALSE.
  691       INPUT(I)%FORCING%ICE_PARAM1    = .FALSE.
  692       INPUT(I)%FORCING%ICE_PARAM2    = .FALSE.
  693       INPUT(I)%FORCING%ICE_PARAM3    = .FALSE.
  694       INPUT(I)%FORCING%ICE_PARAM4    = .FALSE.
  695       INPUT(I)%FORCING%ICE_PARAM5    = .FALSE.
  696       INPUT(I)%FORCING%MUD_DENSITY   = .FALSE.
  697       INPUT(I)%FORCING%MUD_THICKNESS = .FALSE.
  698       INPUT(I)%FORCING%MUD_VISCOSITY = .FALSE.
  699       INPUT(I)%ASSIM%MEAN            = .FALSE.
  700       INPUT(I)%ASSIM%SPEC1D          = .FALSE.
  701       INPUT(I)%ASSIM%SPEC2D          = .FALSE.
  702     END DO
  703           
  704     ! read input grid namelist
  705     REWIND (NDSI)
  706     READ (NDSI, nml=INPUT_GRID_NML, iostat=IERR, iomsg=MSG)
  707     IF (IERR.GT.0) THEN
  708       WRITE (MDSE,'(A,/A)') &
  709         'ERROR: READ_INPUT_GRID_NML: namelist read error', &
  710         'ERROR: '//TRIM(MSG)
  711       CALL EXTCDE (7)
  712     END IF
  713           
  714     ! set/check RETURN values
  715     DO I = 1,NRINP
 
 
                                                     w3nmlmultimd.F90  page  14
 
 
  716       IF (TRIM(INPUT(I)%NAME).EQ.'unset') THEN
  717         WRITE (MDSE,10) 'ERROR: READ_INPUT_GRID_NML: REQUIRED INPUT(',I,'>
  718         CALL EXTCDE (8)
  719       END IF
  720     END DO
  721           
  722     ! save namelist
  723     NML_INPUT_GRID = INPUT(1:NRINP)
  724                                    
  725 10  FORMAT (A,I0,A)
  726                    
  727   END SUBROUTINE READ_INPUT_GRID_NML
  728                                     
  729 !/ ------------------------------------------------------------------- /
  730                                                                         
  731                                                                         
  732                                                                         
  733                                                                         
  734                                                                         
  735                                                                         
  736 !/ ------------------------------------------------------------------- /
  737                                                                         
  738   SUBROUTINE READ_MODEL_GRID_NML (NDSI, NRGRD, NML_MODEL_GRID)
  739 !/
  740 !/                  +-----------------------------------+
  741 !/                  | WAVEWATCH III           NOAA/NCEP |
  742 !/                  |           M. Accensi              |
  743 !/                  |                        FORTRAN 90 |
  744 !/                  | Last update :         15-May-2018 |
  745 !/                  +-----------------------------------+
  746 !/
  747 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
  748 !/    15-May-2018 : Update namelist                     ( version 6.05 )
  749 !/
  750 !  1. Purpose :
  751 !
  752 !  2. Method :
  753 !
  754 !     See source term routines.
  755 !
  756 !  3. Parameters :
  757 !
  758 !     Parameter list
  759 !     ----------------------------------------------------------------
  760 !      NDSI              Int.
  761 !      NRGRD             Int.
  762 !      NML_MODEL_GRID    Type.
  763 !     ----------------------------------------------------------------
  764 !
  765 !  4. Subroutines used :
  766 !
  767 !      Name      Type  Module   Description
  768 !     ----------------------------------------------------------------
  769 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  770 !     ----------------------------------------------------------------
 
 
                                                     w3nmlmultimd.F90  page  15
 
 
  771 !
  772 !  5. Called by :
  773 !
  774 !      Name      Type  Module   Description
  775 !     ----------------------------------------------------------------
  776 !      W3NMLMULTICONF Subr.   N/A    Namelist configuration routine.
  777 !     ----------------------------------------------------------------
  778 !
  779 !  6. Error messages :
  780 !
  781 !     None.
  782 !
  783 !  7. Remarks :
  784 !
  785 !  8. Structure :
  786 !
  787 !     See source code.
  788 !
  789 !  9. Switches :
  790 !
  791 !     !/MPI  Uses MPI communications
  792 !
  793 ! 10. Source code :
  794 !
  795 !/ ------------------------------------------------------------------- /
  796                                                                         
  797     USE WMMDATMD, ONLY: MDSE
  798     USE W3SERVMD, ONLY: EXTCDE
  799                               
  800     IMPLICIT NONE
  801                  
  802     INTEGER, INTENT(IN)               :: NDSI, NRGRD
  803     TYPE(NML_MODEL_GRID_T), INTENT(INOUT) :: NML_MODEL_GRID(NRGRD)
  804                                                                   
  805     ! locals
  806     INTEGER                   :: IERR, I
  807     INTEGER, PARAMETER :: MAX_NRGRD = 99
  808     TYPE(NML_MODEL_GRID_T) :: MODEL(MAX_NRGRD)
  809     NAMELIST /MODEL_GRID_NML/ MODEL
  810                                    
  811     IERR = 0
  812             
  813     ! test NRGRD
  814     IF (NRGRD.GT.MAX_NRGRD) THEN
  815       WRITE (MDSE,'(A,/A,I6,/A,I6)') &
  816         'ERROR: READ_MODEL_GRID_NML: NRGRD > MAX_NRGRD',      &
  817         'ERROR: READ_MODEL_GRID_NML:     NRGRD = ',    NRGRD, &
  818         'ERROR: READ_MODEL_GRID_NML: MAX_NRGRD = ',MAX_NRGRD
  819       CALL EXTCDE (9)
  820     END IF
  821           
  822     ! if no model grids, then exit
  823     IF (NRGRD.EQ.0) RETURN
  824                           
  825     ! set default values for model input data
 
 
                                                     w3nmlmultimd.F90  page  16
 
 
  826     DO I = 1,NRGRD
  827       MODEL(I)%NAME                  = 'unset'
  828       MODEL(I)%FORCING%WATER_LEVELS  = 'no'
  829       MODEL(I)%FORCING%CURRENTS      = 'no'
  830       MODEL(I)%FORCING%WINDS         = 'no'
  831       MODEL(I)%FORCING%ICE_CONC      = 'no'
  832       MODEL(I)%FORCING%ICE_PARAM1    = 'no'
  833       MODEL(I)%FORCING%ICE_PARAM2    = 'no'
  834       MODEL(I)%FORCING%ICE_PARAM3    = 'no'
  835       MODEL(I)%FORCING%ICE_PARAM4    = 'no'
  836       MODEL(I)%FORCING%ICE_PARAM5    = 'no'
  837       MODEL(I)%FORCING%MUD_DENSITY   = 'no'
  838       MODEL(I)%FORCING%MUD_THICKNESS = 'no'
  839       MODEL(I)%FORCING%MUD_VISCOSITY = 'no'
  840       MODEL(I)%ASSIM%MEAN            = 'no'
  841       MODEL(I)%ASSIM%SPEC1D          = 'no'
  842       MODEL(I)%ASSIM%SPEC2D          = 'no'
  843       MODEL(I)%RESOURCE%RANK_ID      = I
  844       MODEL(I)%RESOURCE%GROUP_ID     = 1
  845       MODEL(I)%RESOURCE%COMM_FRAC    = (/0.00,1.00/)
  846       MODEL(I)%RESOURCE%BOUND_FLAG   = .FALSE.
  847     END DO
  848           
  849     ! read model namelist
  850     REWIND (NDSI)
  851     READ (NDSI, nml=MODEL_GRID_NML, iostat=IERR, iomsg=MSG)
  852     IF (IERR.NE.0) THEN
  853       WRITE (MDSE,'(A,/A)') &
  854         'ERROR: READ_MODEL_GRID_NML: namelist read error', &
  855         'ERROR: '//TRIM(MSG)
  856       CALL EXTCDE (10)
  857     END IF
  858           
  859     ! set/check return values
  860     DO I = 1,NRGRD
  861       IF (TRIM(MODEL(I)%NAME).EQ.'unset') THEN
  862         WRITE (MDSE,10) 'ERROR: READ_MODEL_GRID_NML: required MODEL(',I,'>
  863         CALL EXTCDE (11)
  864       END IF
  865     END DO
  866           
  867     ! save namelist
  868     NML_MODEL_GRID = MODEL(1:NRGRD)
  869                                    
  870 10  FORMAT (A,I0,A)
  871                    
  872   END SUBROUTINE READ_MODEL_GRID_NML
  873                                     
  874 !/ ------------------------------------------------------------------- /
  875                                                                         
  876                                                                         
  877                                                                         
  878                                                                         
  879                                                                         
  880                                                                         
 
 
                                                     w3nmlmultimd.F90  page  17
 
 
  881 !/ ------------------------------------------------------------------- /
  882                                                                         
  883   SUBROUTINE READ_OUTPUT_TYPE_NML (NDSI, NRGRD, NML_OUTPUT_TYPE)
  884 !/
  885 !/                  +-----------------------------------+
  886 !/                  | WAVEWATCH III           NOAA/NCEP |
  887 !/                  |           M. Accensi              |
  888 !/                  |                        FORTRAN 90 |
  889 !/                  | Last update :         25-Sep-2020 |
  890 !/                  +-----------------------------------+
  891 !/
  892 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
  893 !/    15-May-2018 : Update namelist                     ( version 6.05 )
  894 !/    25-Sep-2020 : Update namelist                     ( version 7.10 )
  895 !/
  896 !  1. Purpose :
  897 !
  898 !  2. Method :
  899 !
  900 !     See source term routines.
  901 !
  902 !  3. Parameters :
  903 !
  904 !     Parameter list
  905 !     ----------------------------------------------------------------
  906 !      NDSI              Int.
  907 !      NRGRD             Int.
  908 !      NML_OUTPUT_TYPE   Type.
  909 !     ----------------------------------------------------------------
  910 !
  911 !  4. Subroutines used :
  912 !
  913 !      Name      Type  Module   Description
  914 !     ----------------------------------------------------------------
  915 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  916 !     ----------------------------------------------------------------
  917 !
  918 !  5. Called by :
  919 !
  920 !      Name      Type  Module   Description
  921 !     ----------------------------------------------------------------
  922 !      W3NMLMULTICONF Subr.   N/A    Namelist configuration routine.
  923 !     ----------------------------------------------------------------
  924 !
  925 !  6. Error messages :
  926 !
  927 !     None.
  928 !
  929 !  7. Remarks :
  930 !
  931 !  8. Structure :
  932 !
  933 !     See source code.
  934 !
  935 !  9. Switches :
 
 
                                                     w3nmlmultimd.F90  page  18
 
 
  936 !
  937 !     !/MPI  Uses MPI communications
  938 !
  939 ! 10. Source code :
  940 !
  941 !/ ------------------------------------------------------------------- /
  942                                                                         
  943     USE WMMDATMD, ONLY: MDSE
  944     USE W3SERVMD, ONLY: EXTCDE
  945                               
  946     IMPLICIT NONE
  947                  
  948     INTEGER, INTENT(IN)                :: NDSI, NRGRD
  949     TYPE(NML_OUTPUT_TYPE_T), INTENT(INOUT) :: NML_OUTPUT_TYPE(NRGRD)
  950                                                                     
  951     ! locals
  952     INTEGER                   :: IERR, I
  953     INTEGER, PARAMETER        :: MAX_NRGRD = 99
  954     TYPE(NML_OUTPUT_TYPE_T)   :: ALLTYPE
  955     TYPE(NML_OUTPUT_TYPE_T)   :: ITYPE(MAX_NRGRD)
  956     NAMELIST /OUTPUT_TYPE_NML/ ALLTYPE, ITYPE
  957                                              
  958     IERR = 0
  959             
  960     ! if no model grids, then exit
  961     IF (NRGRD.EQ.0) RETURN
  962                           
  963     ! set default values for output type data
  964     DO I=1,NRGRD
  965       ITYPE(I)%FIELD%LIST   = 'unset'
  966       ITYPE(I)%POINT%NAME   = 'unset'
  967       ITYPE(I)%POINT%FILE   = 'points.list'
  968       ITYPE(I)%TRACK%FORMAT = .TRUE.
  969       ITYPE(I)%PARTITION%X0 = 0
  970       ITYPE(I)%PARTITION%XN = 0
  971       ITYPE(I)%PARTITION%NX = 0
  972       ITYPE(I)%PARTITION%Y0 = 0
  973       ITYPE(I)%PARTITION%YN = 0
  974       ITYPE(I)%PARTITION%NY = 0
  975       ITYPE(I)%PARTITION%FORMAT = .TRUE.
  976       ITYPE(I)%RESTART%EXTRA = 'unset'
  977     END DO
  978     ALLTYPE%FIELD%LIST   = 'unset'
  979     ALLTYPE%POINT%NAME   = 'unset'
  980     ALLTYPE%POINT%FILE   = 'points.list'
  981     ALLTYPE%TRACK%FORMAT = .TRUE.
  982     ALLTYPE%PARTITION%X0 = 0
  983     ALLTYPE%PARTITION%XN = 0
  984     ALLTYPE%PARTITION%NX = 0
  985     ALLTYPE%PARTITION%Y0 = 0
  986     ALLTYPE%PARTITION%YN = 0
  987     ALLTYPE%PARTITION%NY = 0
  988     ALLTYPE%PARTITION%FORMAT = .TRUE.
  989     ALLTYPE%RESTART%EXTRA = 'unset'
  990                                    
 
 
                                                     w3nmlmultimd.F90  page  19
 
 
  991                                    
  992     ! read OUTPUT_TYPE namelist
  993     REWIND (NDSI)
  994     READ (NDSI, nml=OUTPUT_TYPE_NML, iostat=IERR, iomsg=MSG)
  995     IF (IERR.GT.0) THEN
  996       WRITE (MDSE,'(A,/A)') &
  997         'ERROR: READ_OUTPUT_TYPE_NML: namelist read error', &
  998         'ERROR: '//TRIM(MSG)
  999       CALL EXTCDE (12)
 1000     END IF
 1001           
 1002     ! propagate general type on all grids
 1003     DO I=1,NRGRD
 1004       ITYPE(I) = ALLTYPE
 1005     END DO
 1006           
 1007     ! read OUTPUT_TYPE namelist
 1008     REWIND (NDSI)
 1009     READ (NDSI, nml=OUTPUT_TYPE_NML, iostat=IERR, iomsg=MSG)
 1010     IF (IERR.GT.0) THEN
 1011       WRITE (MDSE,'(A,/A)') &
 1012         'ERROR: READ_OUTPUT_TYPE_NML: namelist read error', &
 1013         'ERROR: '//TRIM(MSG)
 1014       CALL EXTCDE (13)
 1015     END IF
 1016           
 1017     ! save namelist
 1018     NML_OUTPUT_TYPE = ITYPE(1:NRGRD)
 1019                                     
 1020                                     
 1021   END SUBROUTINE READ_OUTPUT_TYPE_NML
 1022                                      
 1023 !/ ------------------------------------------------------------------- /
 1024                                                                         
 1025                                                                         
 1026                                                                         
 1027                                                                         
 1028                                                                         
 1029                                                                         
 1030 !/ ------------------------------------------------------------------- /
 1031                                                                         
 1032   SUBROUTINE READ_OUTPUT_DATE_NML (NDSI, NRGRD, NML_OUTPUT_DATE)
 1033 !/
 1034 !/                  +-----------------------------------+
 1035 !/                  | WAVEWATCH III           NOAA/NCEP |
 1036 !/                  |           M. Accensi              |
 1037 !/                  |                        FORTRAN 90 |
 1038 !/                  | Last update :         15-May-2018 |
 1039 !/                  +-----------------------------------+
 1040 !/
 1041 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
 1042 !/    15-May-2018 : Update namelist                     ( version 6.05 )
 1043 !/
 1044 !  1. Purpose :
 1045 !
 
 
                                                     w3nmlmultimd.F90  page  20
 
 
 1046 !  2. Method :
 1047 !
 1048 !     See source term routines.
 1049 !
 1050 !  3. Parameters :
 1051 !
 1052 !     Parameter list
 1053 !     ----------------------------------------------------------------
 1054 !      NDSI              Int.
 1055 !      NRGRD             Int.
 1056 !      NML_OUTPUT_DATE   Type.
 1057 !     ----------------------------------------------------------------
 1058 !
 1059 !  4. Subroutines used :
 1060 !
 1061 !      Name      Type  Module   Description
 1062 !     ----------------------------------------------------------------
 1063 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1064 !     ----------------------------------------------------------------
 1065 !
 1066 !  5. Called by :
 1067 !
 1068 !      Name      Type  Module   Description
 1069 !     ----------------------------------------------------------------
 1070 !      W3NMLMULTICONF Subr.   N/A    Namelist configuration routine.
 1071 !     ----------------------------------------------------------------
 1072 !
 1073 !  6. Error messages :
 1074 !
 1075 !     None.
 1076 !
 1077 !  7. Remarks :
 1078 !
 1079 !  8. Structure :
 1080 !
 1081 !     See source code.
 1082 !
 1083 !  9. Switches :
 1084 !
 1085 !     !/MPI  Uses MPI communications
 1086 !
 1087 ! 10. Source code :
 1088 !
 1089 !/ ------------------------------------------------------------------- /
 1090                                                                         
 1091     USE WMMDATMD, ONLY: MDSE
 1092     USE W3SERVMD, ONLY: EXTCDE
 1093                               
 1094     IMPLICIT NONE
 1095                  
 1096     INTEGER, INTENT(IN)                :: NDSI, NRGRD
 1097     TYPE(NML_OUTPUT_DATE_T), INTENT(INOUT) :: NML_OUTPUT_DATE(NRGRD)
 1098                                                                     
 1099     ! locals
 1100     INTEGER                   :: IERR, I
 
 
                                                     w3nmlmultimd.F90  page  21
 
 
 1101     INTEGER, PARAMETER        :: MAX_NRGRD = 99
 1102     TYPE(NML_OUTPUT_DATE_T)   :: ALLDATE
 1103     TYPE(NML_OUTPUT_DATE_T)   :: IDATE(MAX_NRGRD)
 1104     NAMELIST /OUTPUT_DATE_NML/ ALLDATE, IDATE
 1105                                              
 1106     IERR = 0
 1107             
 1108     ! if no model grids, then exit
 1109     IF (NRGRD.EQ.0) RETURN
 1110                           
 1111     ! set default values for OUTPUT_DATE input data
 1112     DO I=1,NRGRD
 1113       IDATE(I)%FIELD%START = '19680606 000000'
 1114       IDATE(I)%FIELD%STRIDE = '0'
 1115       IDATE(I)%FIELD%STOP = '19680607 000000'
 1116       IDATE(I)%FIELD%OUTFFILE = '0'
 1117       IDATE(I)%POINT%OUTFFILE = '0'
 1118       IDATE(I)%POINT%START = '19680606 000000'
 1119       IDATE(I)%POINT%STRIDE = '0'
 1120       IDATE(I)%POINT%STOP = '19680607 000000'
 1121       IDATE(I)%TRACK%START = '19680606 000000'
 1122       IDATE(I)%TRACK%STRIDE = '0'
 1123       IDATE(I)%TRACK%STOP = '19680607 000000'
 1124       IDATE(I)%RESTART%START = '19680606 000000'
 1125       IDATE(I)%RESTART%STRIDE = '0'
 1126       IDATE(I)%RESTART%STOP = '19680607 000000'
 1127       IDATE(I)%RESTART2%START = '19680606 000000'
 1128       IDATE(I)%RESTART2%STRIDE = '0'
 1129       IDATE(I)%RESTART2%STOP = '19680607 000000'
 1130       IDATE(I)%BOUNDARY%START = '19680606 000000'
 1131       IDATE(I)%BOUNDARY%STRIDE = '0'
 1132       IDATE(I)%BOUNDARY%STOP = '19680607 000000'
 1133       IDATE(I)%PARTITION%START = '19680606 000000'
 1134       IDATE(I)%PARTITION%STRIDE = '0'
 1135       IDATE(I)%PARTITION%STOP = '19680607 000000'
 1136     END DO
 1137     ALLDATE%FIELD%START = '19680606 000000'
 1138     ALLDATE%FIELD%STRIDE = '0'
 1139     ALLDATE%FIELD%STOP = '19680607 000000'
 1140     ALLDATE%FIELD%OUTFFILE = '0'
 1141     ALLDATE%POINT%START = '19680606 000000'
 1142     ALLDATE%POINT%STRIDE = '0'
 1143     ALLDATE%POINT%STOP = '19680607 000000'
 1144     ALLDATE%POINT%OUTFFILE = '0'
 1145     ALLDATE%TRACK%START = '19680606 000000'
 1146     ALLDATE%TRACK%STRIDE = '0'
 1147     ALLDATE%TRACK%STOP = '19680607 000000'
 1148     ALLDATE%RESTART%START = '19680606 000000'
 1149     ALLDATE%RESTART%STRIDE = '0'
 1150     ALLDATE%RESTART%STOP   = '19680607 000000'
 1151     ALLDATE%RESTART2%START  = '19680606 000000'
 1152     ALLDATE%RESTART2%STRIDE = '0'
 1153     ALLDATE%RESTART2%STOP   = '19680607 000000'
 1154     ALLDATE%BOUNDARY%START = '19680606 000000'
 1155     ALLDATE%BOUNDARY%STRIDE = '0'
 
 
                                                     w3nmlmultimd.F90  page  22
 
 
 1156     ALLDATE%BOUNDARY%STOP = '19680607 000000'
 1157     ALLDATE%PARTITION%START = '19680606 000000'
 1158     ALLDATE%PARTITION%STRIDE = '0'
 1159     ALLDATE%PARTITION%STOP = '19680607 000000'
 1160                                               
 1161                                               
 1162     ! read OUTPUT_DATE namelist
 1163     REWIND (NDSI)
 1164     READ (NDSI, nml=OUTPUT_DATE_NML, iostat=IERR, iomsg=MSG)
 1165     IF (IERR.GT.0) THEN
 1166       WRITE (MDSE,'(A,/A)') &
 1167         'ERROR: READ_OUTPUT_DATE_NML: namelist read error', &
 1168         'ERROR: '//TRIM(MSG)
 1169       CALL EXTCDE (14)
 1170     END IF
 1171           
 1172     ! propagate general date to all grids
 1173     DO I=1,NRGRD
 1174       IDATE(I) = ALLDATE
 1175     END DO
 1176           
 1177     ! read OUTPUT_DATE namelist
 1178     REWIND (NDSI)
 1179     READ (NDSI, nml=OUTPUT_DATE_NML, iostat=IERR, iomsg=MSG)
 1180     IF (IERR.GT.0) THEN
 1181       WRITE (MDSE,'(A,/A)') &
 1182         'ERROR: READ_OUTPUT_DATE_NML: namelist read error', &
 1183         'ERROR: '//TRIM(MSG)
 1184       CALL EXTCDE (15)
 1185     END IF
 1186           
 1187     ! save namelist
 1188     NML_OUTPUT_DATE = IDATE(1:NRGRD)
 1189                                     
 1190                                     
 1191   END SUBROUTINE READ_OUTPUT_DATE_NML
 1192                                      
 1193 !/ ------------------------------------------------------------------- /
 1194                                                                         
 1195                                                                         
 1196                                                                         
 1197                                                                         
 1198                                                                         
 1199                                                                         
 1200 !/ ------------------------------------------------------------------- /
 1201                                                                         
 1202   SUBROUTINE READ_HOMOGENEOUS_NML (NDSI, NML_HOMOG_COUNT, NML_HOMOG_INPUT)
 1203 !/
 1204 !/                  +-----------------------------------+
 1205 !/                  | WAVEWATCH III           NOAA/NCEP |
 1206 !/                  |           M. Accensi              |
 1207 !/                  |                        FORTRAN 90 |
 1208 !/                  | Last update :         15-May-2018 |
 1209 !/                  +-----------------------------------+
 1210 !/
 
 
                                                     w3nmlmultimd.F90  page  23
 
 
 1211 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
 1212 !/    15-May-2018 : Update namelist                     ( version 6.05 )
 1213 !/
 1214 !  1. Purpose :
 1215 !
 1216 !  2. Method :
 1217 !
 1218 !     See source term routines.
 1219 !
 1220 !  3. Parameters :
 1221 !
 1222 !     Parameter list
 1223 !     ----------------------------------------------------------------
 1224 !      NDSI               Int.
 1225 !      NML_HOMOG_COUNT    Type.
 1226 !      NML_HOMOG_INPUT    Type.
 1227 !     ----------------------------------------------------------------
 1228 !
 1229 !  4. Subroutines used :
 1230 !
 1231 !      Name      Type  Module   Description
 1232 !     ----------------------------------------------------------------
 1233 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1234 !     ----------------------------------------------------------------
 1235 !
 1236 !  5. Called by :
 1237 !
 1238 !      Name      Type  Module   Description
 1239 !     ----------------------------------------------------------------
 1240 !      W3NMLMULTICONF Subr.   N/A    Namelist configuration routine.
 1241 !     ----------------------------------------------------------------
 1242 !
 1243 !  6. Error messages :
 1244 !
 1245 !     None.
 1246 !
 1247 !  7. Remarks :
 1248 !
 1249 !  8. Structure :
 1250 !
 1251 !     See source code.
 1252 !
 1253 !  9. Switches :
 1254 !
 1255 !     !/MPI  Uses MPI communications
 1256 !
 1257 ! 10. Source code :
 1258 !
 1259 !/ ------------------------------------------------------------------- /
 1260                                                                         
 1261     USE WMMDATMD, ONLY: MDSE
 1262     USE W3SERVMD, ONLY: EXTCDE
 1263                               
 1264     IMPLICIT NONE
 1265                  
 
 
                                                     w3nmlmultimd.F90  page  24
 
 
 1266     INTEGER, INTENT(IN)                      :: NDSI
 1267     TYPE(NML_HOMOG_COUNT_T), INTENT(OUT)  :: NML_HOMOG_COUNT
 1268     TYPE(NML_HOMOG_INPUT_T), ALLOCATABLE, INTENT(OUT)   :: NML_HOMOG_INPU>
 1269                                                                           
 1270     ! locals
 1271     INTEGER                   :: IERR, I
 1272     TYPE(NML_HOMOG_COUNT_T)  :: HOMOG_COUNT
 1273     NAMELIST /HOMOG_COUNT_NML/  HOMOG_COUNT
 1274     TYPE(NML_HOMOG_INPUT_T), ALLOCATABLE   :: HOMOG_INPUT(:)
 1275     NAMELIST /HOMOG_INPUT_NML/   HOMOG_INPUT
 1276                                             
 1277     IERR = 0
 1278             
 1279             
 1280     ! set default values for homogeneous number structure
 1281     HOMOG_COUNT%N_MOV   = 0
 1282     HOMOG_COUNT%N_TOT   = 0
 1283                            
 1284                            
 1285     ! read homogeneous count namelist
 1286     REWIND (NDSI)
 1287     READ (NDSI, nml=HOMOG_COUNT_NML, iostat=IERR, iomsg=MSG)
 1288     IF (IERR.GT.0) THEN
 1289       WRITE (MDSE,'(A,/A)') &
 1290         'ERROR: READ_HOMOGENEOUS_NML: namelist HOMOG_COUNT read error', &
 1291         'ERROR: '//TRIM(MSG)
 1292       CALL EXTCDE (16)
 1293     END IF
 1294           
 1295     ! allocate the total count of homogeneous input
 1296     HOMOG_COUNT%N_TOT = HOMOG_COUNT%N_MOV
 1297     ALLOCATE(HOMOG_INPUT(HOMOG_COUNT%N_TOT))
 1298     ALLOCATE(NML_HOMOG_INPUT(HOMOG_COUNT%N_TOT))
 1299                                                 
 1300     ! set default values for homogeneous structure
 1301     IF (HOMOG_COUNT%N_TOT .NE. 0 ) THEN
 1302       DO I=1,HOMOG_COUNT%N_TOT
 1303         HOMOG_INPUT(I)%NAME      = 'unset'
 1304         HOMOG_INPUT(I)%DATE      = '19680606 000000'
 1305         HOMOG_INPUT(I)%VALUE1    = 0.
 1306         HOMOG_INPUT(I)%VALUE2    = 0.
 1307         HOMOG_INPUT(I)%VALUE3    = 0.
 1308       END DO
 1309     END IF
 1310           
 1311     ! read homogeneous input namelist
 1312     REWIND (NDSI)
 1313     READ (NDSI, nml=HOMOG_INPUT_NML, iostat=IERR, iomsg=MSG)
 1314     IF (IERR.GT.0) THEN
 1315       WRITE (MDSE,'(A,/A)') &
 1316         'ERROR: READ_HOMOGENEOUS_NML: namelist HOMOG_INPUT_NML read error>
 1317         'ERROR: '//TRIM(MSG)
 1318       CALL EXTCDE (17)
 1319     END IF
 1320           
 
 
                                                     w3nmlmultimd.F90  page  25
 
 
 1321     ! save namelist
 1322     NML_HOMOG_COUNT = HOMOG_COUNT
 1323     NML_HOMOG_INPUT = HOMOG_INPUT
 1324                                  
 1325                                  
 1326   END SUBROUTINE READ_HOMOGENEOUS_NML
 1327                                      
 1328 !/ ------------------------------------------------------------------- /
 1329                                                                         
 1330                                                                         
 1331                                                                         
 1332                                                                         
 1333                                                                         
 1334                                                                         
 1335                                                                         
 1336                                                                         
 1337                                                                         
 1338 !/ ------------------------------------------------------------------- /
 1339                                                                         
 1340   SUBROUTINE REPORT_DOMAIN_NML (NML_DOMAIN)
 1341 !/
 1342 !/                  +-----------------------------------+
 1343 !/                  | WAVEWATCH III           NOAA/NCEP |
 1344 !/                  |           M. Accensi              |
 1345 !/                  |                        FORTRAN 90 |
 1346 !/                  | Last update :         15-May-2018 |
 1347 !/                  +-----------------------------------+
 1348 !/
 1349 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
 1350 !/    15-May-2018 : Update namelist                     ( version 6.05 )
 1351 !/
 1352 !  1. Purpose :
 1353 !
 1354 !  2. Method :
 1355 !
 1356 !     See source term routines.
 1357 !
 1358 !  3. Parameters :
 1359 !
 1360 !     Parameter list
 1361 !     ----------------------------------------------------------------
 1362 !      NML_DOMAIN       Type.
 1363 !     ----------------------------------------------------------------
 1364 !
 1365 !  4. Subroutines used :
 1366 !
 1367 !      Name      Type  Module   Description
 1368 !     ----------------------------------------------------------------
 1369 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1370 !     ----------------------------------------------------------------
 1371 !
 1372 !  5. Called by :
 1373 !
 1374 !      Name      Type  Module   Description
 1375 !     ----------------------------------------------------------------
 
 
                                                     w3nmlmultimd.F90  page  26
 
 
 1376 !      W3NMLMULTICONF Subr.   N/A    Namelist configuration routine.
 1377 !     ----------------------------------------------------------------
 1378 !
 1379 !  6. Error messages :
 1380 !
 1381 !     None.
 1382 !
 1383 !  7. Remarks :
 1384 !
 1385 !  8. Structure :
 1386 !
 1387 !     See source code.
 1388 !
 1389 !  9. Switches :
 1390 !
 1391 !     !/MPI  Uses MPI communications
 1392 !
 1393 ! 10. Source code :
 1394 !
 1395 !/ ------------------------------------------------------------------- /
 1396                                                                         
 1397                                                                         
 1398     IMPLICIT NONE
 1399                  
 1400     TYPE(NML_DOMAIN_T), INTENT(IN) :: NML_DOMAIN
 1401                                                 
 1402                                                 
 1403       WRITE (MSG,'(A)') 'DOMAIN % '
 1404       WRITE (NDSN,'(A)')
 1405       WRITE (NDSN,11) TRIM(MSG),'NRINP  = ', NML_DOMAIN%NRINP
 1406       WRITE (NDSN,11) TRIM(MSG),'NRGRD  = ', NML_DOMAIN%NRGRD
 1407       WRITE (NDSN,13) TRIM(MSG),'UNIPTS = ', NML_DOMAIN%UNIPTS
 1408       WRITE (NDSN,11) TRIM(MSG),'IOSTYP = ', NML_DOMAIN%IOSTYP
 1409       WRITE (NDSN,13) TRIM(MSG),'UPPROC = ', NML_DOMAIN%UPPROC
 1410       WRITE (NDSN,13) TRIM(MSG),'PSHARE = ', NML_DOMAIN%PSHARE
 1411       WRITE (NDSN,13) TRIM(MSG),'FLGHG1 = ', NML_DOMAIN%FLGHG1
 1412       WRITE (NDSN,13) TRIM(MSG),'FLGHG2 = ', NML_DOMAIN%FLGHG2
 1413       WRITE (NDSN,10) TRIM(MSG),'START  = ', TRIM(NML_DOMAIN%START)
 1414       WRITE (NDSN,10) TRIM(MSG),'STOP   = ', TRIM(NML_DOMAIN%STOP)
 1415                                                                   
 1416 10  FORMAT (A,2X,A,A)
 1417 11  FORMAT (A,2X,A,I8)
 1418 13  FORMAT (A,2X,A,L1)
 1419                       
 1420   END SUBROUTINE REPORT_DOMAIN_NML
 1421                                   
 1422 !/ ------------------------------------------------------------------- /
 1423                                                                         
 1424                                                                         
 1425                                                                         
 1426                                                                         
 1427                                                                         
 1428                                                                         
 1429 !/ ------------------------------------------------------------------- /
 1430                                                                         
 
 
                                                     w3nmlmultimd.F90  page  27
 
 
 1431   SUBROUTINE REPORT_INPUT_GRID_NML (NRINP, NML_INPUT_GRID)
 1432 !/
 1433 !/                  +-----------------------------------+
 1434 !/                  | WAVEWATCH III           NOAA/NCEP |
 1435 !/                  |           M. Accensi              |
 1436 !/                  |                        FORTRAN 90 |
 1437 !/                  | Last update :         15-May-2018 |
 1438 !/                  +-----------------------------------+
 1439 !/
 1440 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
 1441 !/    15-May-2018 : Update namelist                     ( version 6.05 )
 1442 !/
 1443 !  1. Purpose :
 1444 !
 1445 !  2. Method :
 1446 !
 1447 !     See source term routines.
 1448 !
 1449 !  3. Parameters :
 1450 !
 1451 !     Parameter list
 1452 !     ----------------------------------------------------------------
 1453 !      NRINP             Int.
 1454 !      NML_INPUT_GRID    Type.
 1455 !     ----------------------------------------------------------------
 1456 !
 1457 !  4. Subroutines used :
 1458 !
 1459 !      Name      Type  Module   Description
 1460 !     ----------------------------------------------------------------
 1461 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1462 !     ----------------------------------------------------------------
 1463 !
 1464 !  5. Called by :
 1465 !
 1466 !      Name      Type  Module   Description
 1467 !     ----------------------------------------------------------------
 1468 !      W3NMLMULTICONF Subr.   N/A    Namelist configuration routine.
 1469 !     ----------------------------------------------------------------
 1470 !
 1471 !  6. Error messages :
 1472 !
 1473 !     None.
 1474 !
 1475 !  7. Remarks :
 1476 !
 1477 !  8. Structure :
 1478 !
 1479 !     See source code.
 1480 !
 1481 !  9. Switches :
 1482 !
 1483 !     !/MPI  Uses MPI communications
 1484 !
 1485 ! 10. Source code :
 
 
                                                     w3nmlmultimd.F90  page  28
 
 
 1486 !
 1487 !/ ------------------------------------------------------------------- /
 1488                                                                         
 1489                                                                         
 1490     IMPLICIT NONE
 1491                  
 1492     INTEGER, INTENT(IN)            :: NRINP
 1493     TYPE(NML_INPUT_GRID_T), INTENT(IN) :: NML_INPUT_GRID(NRINP)
 1494                                                                
 1495     ! locals
 1496     INTEGER              :: I
 1497                              
 1498                              
 1499     DO I = 1,NRINP
 1500       WRITE (MSG,'(A,I0.2,A)') 'INPUT GRID ',I,' % '
 1501       WRITE (NDSN,'(A)')
 1502       WRITE (NDSN,10) TRIM(MSG),'NAME                     = ', TRIM(NML_I>
 1503       WRITE (NDSN,13) TRIM(MSG),'FORCING % WATER_LEVELS   = ', NML_INPUT_>
 1504       WRITE (NDSN,13) TRIM(MSG),'FORCING % CURRENTS       = ', NML_INPUT_>
 1505       WRITE (NDSN,13) TRIM(MSG),'FORCING % WINDS          = ', NML_INPUT_>
 1506       WRITE (NDSN,13) TRIM(MSG),'FORCING % ICE_CONC       = ', NML_INPUT_>
 1507       WRITE (NDSN,13) TRIM(MSG),'FORCING % ICE_PARAM1     = ', NML_INPUT_>
 1508       WRITE (NDSN,13) TRIM(MSG),'FORCING % ICE_PARAM2     = ', NML_INPUT_>
 1509       WRITE (NDSN,13) TRIM(MSG),'FORCING % ICE_PARAM3     = ', NML_INPUT_>
 1510       WRITE (NDSN,13) TRIM(MSG),'FORCING % ICE_PARAM4     = ', NML_INPUT_>
 1511       WRITE (NDSN,13) TRIM(MSG),'FORCING % ICE_PARAM5     = ', NML_INPUT_>
 1512       WRITE (NDSN,13) TRIM(MSG),'FORCING % MUD_DENSITY    = ', NML_INPUT_>
 1513       WRITE (NDSN,13) TRIM(MSG),'FORCING % MUD_THICKNESS  = ', NML_INPUT_>
 1514       WRITE (NDSN,13) TRIM(MSG),'FORCING % MUD_VISCOSITY  = ', NML_INPUT_>
 1515       WRITE (NDSN,13) TRIM(MSG),'ASSIM % MEAN             = ', NML_INPUT_>
 1516       WRITE (NDSN,13) TRIM(MSG),'ASSIM % SPEC1D           = ', NML_INPUT_>
 1517       WRITE (NDSN,13) TRIM(MSG),'ASSIM % SPEC2D           = ', NML_INPUT_>
 1518     END DO
 1519     WRITE (*,*)
 1520                
 1521 10  FORMAT (A,2X,A,A)
 1522 13  FORMAT (A,2X,A,L1)
 1523                       
 1524   END SUBROUTINE REPORT_INPUT_GRID_NML
 1525                                       
 1526 !/ ------------------------------------------------------------------- /
 1527                                                                         
 1528                                                                         
 1529                                                                         
 1530                                                                         
 1531                                                                         
 1532                                                                         
 1533 !/ ------------------------------------------------------------------- /
 1534                                                                         
 1535   SUBROUTINE REPORT_MODEL_GRID_NML (NRGRD, NML_MODEL_GRID)
 1536 !/
 1537 !/                  +-----------------------------------+
 1538 !/                  | WAVEWATCH III           NOAA/NCEP |
 1539 !/                  |           M. Accensi              |
 1540 !/                  |                        FORTRAN 90 |
 
 
                                                     w3nmlmultimd.F90  page  29
 
 
 1541 !/                  | Last update :         15-May-2018 |
 1542 !/                  +-----------------------------------+
 1543 !/
 1544 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
 1545 !/    15-May-2018 : Update namelist                     ( version 6.05 )
 1546 !/
 1547 !  1. Purpose :
 1548 !
 1549 !  2. Method :
 1550 !
 1551 !     See source term routines.
 1552 !
 1553 !  3. Parameters :
 1554 !
 1555 !     Parameter list
 1556 !     ----------------------------------------------------------------
 1557 !      NRGRD             Int.
 1558 !      NML_MODEL_GRID    Type.
 1559 !     ----------------------------------------------------------------
 1560 !
 1561 !  4. Subroutines used :
 1562 !
 1563 !      Name      Type  Module   Description
 1564 !     ----------------------------------------------------------------
 1565 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1566 !     ----------------------------------------------------------------
 1567 !
 1568 !  5. Called by :
 1569 !
 1570 !      Name      Type  Module   Description
 1571 !     ----------------------------------------------------------------
 1572 !      W3NMLMULTICONF Subr.   N/A    Namelist configuration routine.
 1573 !     ----------------------------------------------------------------
 1574 !
 1575 !  6. Error messages :
 1576 !
 1577 !     None.
 1578 !
 1579 !  7. Remarks :
 1580 !
 1581 !  8. Structure :
 1582 !
 1583 !     See source code.
 1584 !
 1585 !  9. Switches :
 1586 !
 1587 !     !/MPI  Uses MPI communications
 1588 !
 1589 ! 10. Source code :
 1590 !
 1591 !/ ------------------------------------------------------------------- /
 1592                                                                         
 1593                                                                         
 1594     IMPLICIT NONE
 1595                  
 
 
                                                     w3nmlmultimd.F90  page  30
 
 
 1596     INTEGER, INTENT(IN)            :: NRGRD
 1597     TYPE(NML_MODEL_GRID_T), INTENT(IN) :: NML_MODEL_GRID(NRGRD)
 1598                                                                
 1599     ! locals
 1600     INTEGER              :: I
 1601                              
 1602                              
 1603     DO I = 1,NRGRD
 1604       WRITE (MSG,'(A,I0.4,A)') 'MODEL GRID ',I,' % '
 1605       WRITE (NDSN,'(A)')
 1606       WRITE (NDSN,10) TRIM(MSG),'NAME                     = ', TRIM(NML_M>
 1607       WRITE (NDSN,10) TRIM(MSG),'FORCING % WATER_LEVELS   = ', TRIM(NML_M>
 1608       WRITE (NDSN,10) TRIM(MSG),'FORCING % CURRENTS       = ', TRIM(NML_M>
 1609       WRITE (NDSN,10) TRIM(MSG),'FORCING % WINDS          = ', TRIM(NML_M>
 1610       WRITE (NDSN,10) TRIM(MSG),'FORCING % ICE_CONC       = ', TRIM(NML_M>
 1611       WRITE (NDSN,10) TRIM(MSG),'FORCING % ICE_PARAM1     = ', TRIM(NML_M>
 1612       WRITE (NDSN,10) TRIM(MSG),'FORCING % ICE_PARAM2     = ', TRIM(NML_M>
 1613       WRITE (NDSN,10) TRIM(MSG),'FORCING % ICE_PARAM3     = ', TRIM(NML_M>
 1614       WRITE (NDSN,10) TRIM(MSG),'FORCING % ICE_PARAM4     = ', TRIM(NML_M>
 1615       WRITE (NDSN,10) TRIM(MSG),'FORCING % ICE_PARAM5     = ', TRIM(NML_M>
 1616       WRITE (NDSN,10) TRIM(MSG),'FORCING % MUD_DENSITY    = ', TRIM(NML_M>
 1617       WRITE (NDSN,10) TRIM(MSG),'FORCING % MUD_THICKNESS  = ', TRIM(NML_M>
 1618       WRITE (NDSN,10) TRIM(MSG),'FORCING % MUD_VISCOSITY  = ', TRIM(NML_M>
 1619       WRITE (NDSN,10) TRIM(MSG),'ASSIM % MEAN             = ', TRIM(NML_M>
 1620       WRITE (NDSN,10) TRIM(MSG),'ASSIM % SPEC1D           = ', TRIM(NML_M>
 1621       WRITE (NDSN,10) TRIM(MSG),'ASSIM % SPEC2D           = ', TRIM(NML_M>
 1622       WRITE (NDSN,11) TRIM(MSG),'RESOURCE % RANK_ID       = ', NML_MODEL_>
 1623       WRITE (NDSN,11) TRIM(MSG),'RESOURCE % GROUP_ID      = ', NML_MODEL_>
 1624       WRITE (NDSN,12) TRIM(MSG),'RESOURCE % COMM_FRAC     = ', NML_MODEL_>
 1625                                                                NML_MODEL_>
 1626       WRITE (NDSN,13) TRIM(MSG),'RESOURCE % BOUND_FLAG    = ', NML_MODEL_>
 1627     END DO
 1628     WRITE (NDSN,'(A)')
 1629                       
 1630 10  FORMAT (A,2X,A,A)
 1631 11  FORMAT (A,2X,A,I8)
 1632 12  FORMAT (A,2X,A,'(',F5.2,',',F5.2,' )')
 1633 13  FORMAT (A,2X,A,L1)
 1634                       
 1635   END SUBROUTINE REPORT_MODEL_GRID_NML
 1636                                       
 1637 !/ ------------------------------------------------------------------- /
 1638                                                                         
 1639                                                                         
 1640                                                                         
 1641                                                                         
 1642                                                                         
 1643                                                                         
 1644 !/ ------------------------------------------------------------------- /
 1645                                                                         
 1646   SUBROUTINE REPORT_OUTPUT_TYPE_NML (NRGRD, NML_OUTPUT_TYPE)
 1647 !/
 1648 !/                  +-----------------------------------+
 1649 !/                  | WAVEWATCH III           NOAA/NCEP |
 1650 !/                  |           M. Accensi              |
 
 
                                                     w3nmlmultimd.F90  page  31
 
 
 1651 !/                  |                        FORTRAN 90 |
 1652 !/                  | Last update :         25-Sep-2020 |
 1653 !/                  +-----------------------------------+
 1654 !/
 1655 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
 1656 !/    15-May-2018 : Update namelist                     ( version 6.05 )
 1657 !/    25-Sep-2020 : Update namelist                     ( version 7.10 )
 1658 !/
 1659 !  1. Purpose :
 1660 !
 1661 !  2. Method :
 1662 !
 1663 !     See source term routines.
 1664 !
 1665 !  3. Parameters :
 1666 !
 1667 !     Parameter list
 1668 !     ----------------------------------------------------------------
 1669 !      NRGRD             Int.
 1670 !      NML_OUTPUT_TYPE   Type.
 1671 !     ----------------------------------------------------------------
 1672 !
 1673 !  4. Subroutines used :
 1674 !
 1675 !      Name      Type  Module   Description
 1676 !     ----------------------------------------------------------------
 1677 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1678 !     ----------------------------------------------------------------
 1679 !
 1680 !  5. Called by :
 1681 !
 1682 !      Name      Type  Module   Description
 1683 !     ----------------------------------------------------------------
 1684 !      W3NMLMULTICONF Subr.   N/A    Namelist configuration routine.
 1685 !     ----------------------------------------------------------------
 1686 !
 1687 !  6. Error messages :
 1688 !
 1689 !     None.
 1690 !
 1691 !  7. Remarks :
 1692 !
 1693 !  8. Structure :
 1694 !
 1695 !     See source code.
 1696 !
 1697 !  9. Switches :
 1698 !
 1699 !     !/MPI  Uses MPI communications
 1700 !
 1701 ! 10. Source code :
 1702 !
 1703 !/ ------------------------------------------------------------------- /
 1704                                                                         
 1705                                                                         
 
 
                                                     w3nmlmultimd.F90  page  32
 
 
 1706     IMPLICIT NONE
 1707                  
 1708     INTEGER, INTENT(IN)             :: NRGRD
 1709     TYPE(NML_OUTPUT_TYPE_T), INTENT(IN) :: NML_OUTPUT_TYPE(NRGRD)
 1710                                                                  
 1711     ! locals
 1712     INTEGER              :: I
 1713                              
 1714                              
 1715     DO I=1,NRGRD
 1716       WRITE (MSG,'(A,I1,A)') 'OUTPUT TYPE MODEL GRID',I, ' % '
 1717       WRITE (NDSN,'(A)')
 1718       WRITE (NDSN,10) TRIM(MSG),'FIELD % LIST         = ', TRIM(NML_OUTPU>
 1719       WRITE (NDSN,10) TRIM(MSG),'POINT % NAME         = ', TRIM(NML_OUTPU>
 1720       WRITE (NDSN,10) TRIM(MSG),'POINT % FILE         = ', TRIM(NML_OUTPU>
 1721       WRITE (NDSN,13) TRIM(MSG),'TRACK % FORMAT       = ', NML_OUTPUT_TYP>
 1722       WRITE (NDSN,11) TRIM(MSG),'PARTITION % X0       = ', NML_OUTPUT_TYP>
 1723       WRITE (NDSN,11) TRIM(MSG),'PARTITION % XN       = ', NML_OUTPUT_TYP>
 1724       WRITE (NDSN,11) TRIM(MSG),'PARTITION % NX       = ', NML_OUTPUT_TYP>
 1725       WRITE (NDSN,11) TRIM(MSG),'PARTITION % Y0       = ', NML_OUTPUT_TYP>
 1726       WRITE (NDSN,11) TRIM(MSG),'PARTITION % YN       = ', NML_OUTPUT_TYP>
 1727       WRITE (NDSN,11) TRIM(MSG),'PARTITION % NY       = ', NML_OUTPUT_TYP>
 1728       WRITE (NDSN,13) TRIM(MSG),'PARTITION % FORMAT   = ', NML_OUTPUT_TYP>
 1729       WRITE (NDSN,10) TRIM(MSG),'RESTART % EXTRA      = ', TRIM(NML_OUTPU>
 1730     END DO
 1731     WRITE (NDSN,'(A)')
 1732                       
 1733 10  FORMAT (A,2X,A,A)
 1734 11  FORMAT (A,2X,A,I8)
 1735 13  FORMAT (A,2X,A,L1)
 1736                       
 1737   END SUBROUTINE REPORT_OUTPUT_TYPE_NML
 1738                                        
 1739 !/ ------------------------------------------------------------------- /
 1740                                                                         
 1741                                                                         
 1742                                                                         
 1743                                                                         
 1744                                                                         
 1745                                                                         
 1746 !/ ------------------------------------------------------------------- /
 1747                                                                         
 1748   SUBROUTINE REPORT_OUTPUT_DATE_NML (NRGRD, NML_OUTPUT_DATE)
 1749 !/
 1750 !/                  +-----------------------------------+
 1751 !/                  | WAVEWATCH III           NOAA/NCEP |
 1752 !/                  |           M. Accensi              |
 1753 !/                  |                        FORTRAN 90 |
 1754 !/                  | Last update :         15-May-2018 |
 1755 !/                  +-----------------------------------+
 1756 !/
 1757 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
 1758 !/    15-May-2018 : Update namelist                     ( version 6.05 )
 1759 !/
 1760 !  1. Purpose :
 
 
                                                     w3nmlmultimd.F90  page  33
 
 
 1761 !
 1762 !  2. Method :
 1763 !
 1764 !     See source term routines.
 1765 !
 1766 !  3. Parameters :
 1767 !
 1768 !     Parameter list
 1769 !     ----------------------------------------------------------------
 1770 !      NRGRD             Int.
 1771 !      NML_OUTPUT_DATE   Type.
 1772 !     ----------------------------------------------------------------
 1773 !
 1774 !  4. Subroutines used :
 1775 !
 1776 !      Name      Type  Module   Description
 1777 !     ----------------------------------------------------------------
 1778 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1779 !     ----------------------------------------------------------------
 1780 !
 1781 !  5. Called by :
 1782 !
 1783 !      Name      Type  Module   Description
 1784 !     ----------------------------------------------------------------
 1785 !      W3NMLMULTICONF Subr.   N/A    Namelist configuration routine.
 1786 !     ----------------------------------------------------------------
 1787 !
 1788 !  6. Error messages :
 1789 !
 1790 !     None.
 1791 !
 1792 !  7. Remarks :
 1793 !
 1794 !  8. Structure :
 1795 !
 1796 !     See source code.
 1797 !
 1798 !  9. Switches :
 1799 !
 1800 !     !/MPI  Uses MPI communications
 1801 !
 1802 ! 10. Source code :
 1803 !
 1804 !/ ------------------------------------------------------------------- /
 1805                                                                         
 1806                                                                         
 1807     IMPLICIT NONE
 1808                  
 1809     INTEGER, INTENT(IN)             :: NRGRD
 1810     TYPE(NML_OUTPUT_DATE_T), INTENT(IN) :: NML_OUTPUT_DATE(NRGRD)
 1811                                                                  
 1812     ! locals
 1813     INTEGER              :: I
 1814                              
 1815                              
 
 
                                                     w3nmlmultimd.F90  page  34
 
 
 1816     DO I=1,NRGRD
 1817       WRITE (MSG,'(A,I1,A)') 'OUTPUT DATE MODEL GRID',I, ' % '
 1818       WRITE (NDSN,'(A)')
 1819       WRITE (NDSN,10) TRIM(MSG),'FIELD % START        = ', TRIM(NML_OUTPU>
 1820       WRITE (NDSN,10) TRIM(MSG),'FIELD % STRIDE       = ', TRIM(NML_OUTPU>
 1821       WRITE (NDSN,10) TRIM(MSG),'FIELD % STOP         = ', TRIM(NML_OUTPU>
 1822       WRITE (NDSN,10) TRIM(MSG),'POINT % START        = ', TRIM(NML_OUTPU>
 1823       WRITE (NDSN,10) TRIM(MSG),'POINT % STRIDE       = ', TRIM(NML_OUTPU>
 1824       WRITE (NDSN,10) TRIM(MSG),'POINT % STOP         = ', TRIM(NML_OUTPU>
 1825       WRITE (NDSN,10) TRIM(MSG),'TRACK % START        = ', TRIM(NML_OUTPU>
 1826       WRITE (NDSN,10) TRIM(MSG),'TRACK % STRIDE       = ', TRIM(NML_OUTPU>
 1827       WRITE (NDSN,10) TRIM(MSG),'TRACK % STOP         = ', TRIM(NML_OUTPU>
 1828       WRITE (NDSN,10) TRIM(MSG),'RESTART % START      = ', TRIM(NML_OUTPU>
 1829       WRITE (NDSN,10) TRIM(MSG),'RESTART % STRIDE     = ', TRIM(NML_OUTPU>
 1830       WRITE (NDSN,10) TRIM(MSG),'RESTART % STOP       = ', TRIM(NML_OUTPU>
 1831       WRITE (NDSN,10) TRIM(MSG),'RESTART2 % START     = ', TRIM(NML_OUTPU>
 1832       WRITE (NDSN,10) TRIM(MSG),'RESTART2 % STRIDE    = ', TRIM(NML_OUTPU>
 1833       WRITE (NDSN,10) TRIM(MSG),'RESTART2 % STOP      = ', TRIM(NML_OUTPU>
 1834       WRITE (NDSN,10) TRIM(MSG),'BOUNDARY % START     = ', TRIM(NML_OUTPU>
 1835       WRITE (NDSN,10) TRIM(MSG),'BOUNDARY % STRIDE    = ', TRIM(NML_OUTPU>
 1836       WRITE (NDSN,10) TRIM(MSG),'BOUNDARY % STOP      = ', TRIM(NML_OUTPU>
 1837       WRITE (NDSN,10) TRIM(MSG),'PARTITION % START    = ', TRIM(NML_OUTPU>
 1838       WRITE (NDSN,10) TRIM(MSG),'PARTITION % STRIDE   = ', TRIM(NML_OUTPU>
 1839       WRITE (NDSN,10) TRIM(MSG),'PARTITION % STOP     = ', TRIM(NML_OUTPU>
 1840     END DO
 1841     WRITE (NDSN,'(A)')
 1842                       
 1843 10  FORMAT (A,2X,A,A)
 1844                      
 1845   END SUBROUTINE REPORT_OUTPUT_DATE_NML
 1846                                        
 1847 !/ ------------------------------------------------------------------- /
 1848                                                                         
 1849                                                                         
 1850                                                                         
 1851                                                                         
 1852                                                                         
 1853                                                                         
 1854 !/ ------------------------------------------------------------------- /
 1855                                                                         
 1856   SUBROUTINE REPORT_HOMOGENEOUS_NML (NML_HOMOG_COUNT, NML_HOMOG_INPUT)
 1857 !/
 1858 !/                  +-----------------------------------+
 1859 !/                  | WAVEWATCH III           NOAA/NCEP |
 1860 !/                  |           M. Accensi              |
 1861 !/                  |                        FORTRAN 90 |
 1862 !/                  | Last update :         15-May-2018 |
 1863 !/                  +-----------------------------------+
 1864 !/
 1865 !/    09-Aug-2016 : Adding comments                     ( version 5.12 )
 1866 !/    15-May-2018 : Update namelist                     ( version 6.05 )
 1867 !/
 1868 !  1. Purpose :
 1869 !
 1870 !  2. Method :
 
 
                                                     w3nmlmultimd.F90  page  35
 
 
 1871 !
 1872 !     See source term routines.
 1873 !
 1874 !  3. Parameters :
 1875 !
 1876 !     Parameter list
 1877 !     ----------------------------------------------------------------
 1878 !      NML_HOMOG_COUNT    Type.
 1879 !      NML_HOMOG_INPUT    Type.
 1880 !     ----------------------------------------------------------------
 1881 !
 1882 !  4. Subroutines used :
 1883 !
 1884 !      Name      Type  Module   Description
 1885 !     ----------------------------------------------------------------
 1886 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1887 !     ----------------------------------------------------------------
 1888 !
 1889 !  5. Called by :
 1890 !
 1891 !      Name      Type  Module   Description
 1892 !     ----------------------------------------------------------------
 1893 !      W3NMLMULTICONF Subr.   N/A    Namelist configuration routine.
 1894 !     ----------------------------------------------------------------
 1895 !
 1896 !  6. Error messages :
 1897 !
 1898 !     None.
 1899 !
 1900 !  7. Remarks :
 1901 !
 1902 !  8. Structure :
 1903 !
 1904 !     See source code.
 1905 !
 1906 !  9. Switches :
 1907 !
 1908 !     !/MPI  Uses MPI communications
 1909 !
 1910 ! 10. Source code :
 1911 !
 1912 !/ ------------------------------------------------------------------- /
 1913                                                                         
 1914                                                                         
 1915     IMPLICIT NONE
 1916                  
 1917     TYPE(NML_HOMOG_COUNT_T), INTENT(IN)  :: NML_HOMOG_COUNT
 1918     TYPE(NML_HOMOG_INPUT_T), INTENT(IN) :: NML_HOMOG_INPUT(NML_HOMOG_COUN>
 1919                                                                           
 1920     ! locals
 1921     INTEGER              :: I
 1922                              
 1923                              
 1924     WRITE (MSG,'(A)') 'HOMOG_COUNT % '
 1925     WRITE (NDSN,'(A)')
 
 
                                                     w3nmlmultimd.F90  page  36
 
 
 1926     WRITE (NDSN,11) TRIM(MSG),'N_MOV       = ', NML_HOMOG_COUNT%N_MOV
 1927                                                                      
 1928     IF (NML_HOMOG_COUNT%N_TOT .NE. 0) THEN
 1929       DO I=1,NML_HOMOG_COUNT%N_TOT
 1930         WRITE (MSG,'(A,I5,A)') 'HOMOG_INPUT(',I,') % '
 1931         WRITE (NDSN,'(A)')
 1932         WRITE (NDSN,10) TRIM(MSG),'NAME      = ', TRIM(NML_HOMOG_INPUT(I)>
 1933         WRITE (NDSN,10) TRIM(MSG),'DATE      = ', TRIM(NML_HOMOG_INPUT(I)>
 1934         WRITE (NDSN,14) TRIM(MSG),'VALUE1    = ', NML_HOMOG_INPUT(I)%VALU>
 1935         WRITE (NDSN,14) TRIM(MSG),'VALUE2    = ', NML_HOMOG_INPUT(I)%VALU>
 1936         WRITE (NDSN,14) TRIM(MSG),'VALUE3    = ', NML_HOMOG_INPUT(I)%VALU>
 1937         WRITE (NDSN,'(A)')
 1938       END DO
 1939     END IF
 1940           
 1941           
 1942 10  FORMAT (A,2X,A,A)
 1943 11  FORMAT (A,2X,A,I8)
 1944 14  FORMAT (A,2X,A,F8.2)
 1945                         
 1946                         
 1947   END SUBROUTINE REPORT_HOMOGENEOUS_NML
 1948                                        
 1949 !/ ------------------------------------------------------------------- /
 1950                                                                         
 1951                                                                         
 1952                                                                         
 1953                                                                         
 1954                                                                         
 1955 END MODULE W3NMLMULTIMD
 1956                        
 1957 !/ ------------------------------------------------------------------- /
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
