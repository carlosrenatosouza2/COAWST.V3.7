                                                      w3nmlounpmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3NMLOUNPMD
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
   17 !     Manages namelists from configuration file ww3_ounp.nml for ww3_ounp>
   18 !
   19 !/ ------------------------------------------------------------------- /
   20                                                                         
   21   ! module defaults
   22   IMPLICIT NONE
   23                
   24   PUBLIC
   25         
   26   ! point structure
   27   TYPE NML_POINT_T
   28     CHARACTER(15)               :: TIMESTART
   29     CHARACTER(15)               :: TIMESTRIDE
   30     CHARACTER(15)               :: TIMECOUNT
   31     INTEGER                     :: TIMESPLIT
   32     CHARACTER(1024)             :: LIST
   33     LOGICAL                     :: SAMEFILE
   34     INTEGER                     :: BUFFER
   35     INTEGER                     :: TYPE
   36     LOGICAL                     :: DIMORDER
   37   END TYPE NML_POINT_T
   38                       
   39   ! file structure
   40   TYPE NML_FILE_T
   41     CHARACTER(30)               :: PREFIX
   42     INTEGER                     :: NETCDF
   43   END TYPE NML_FILE_T
   44                      
   45   ! spectra structure
   46   TYPE NML_SPECTRA_T
   47     INTEGER                     :: OUTPUT
   48     REAL                        :: SCALE_FAC
   49     REAL                        :: OUTPUT_FAC
   50   END TYPE NML_SPECTRA_T
   51                         
   52   ! param structure
   53   TYPE NML_PARAM_T
   54     INTEGER                     :: OUTPUT
   55   END TYPE NML_PARAM_T
 
 
                                                      w3nmlounpmd.F90  page   2
 
 
   56                       
   57   ! source structure
   58   TYPE NML_SOURCE_T
   59     INTEGER                     :: OUTPUT
   60     REAL                        :: SCALE_FAC
   61     REAL                        :: OUTPUT_FAC
   62     INTEGER                     :: TABLE_FAC
   63     LOGICAL                     :: SPECTRUM
   64     LOGICAL                     :: INPUT
   65     LOGICAL                     :: INTERACTIONS
   66     LOGICAL                     :: DISSIPATION
   67     LOGICAL                     :: BOTTOM
   68     LOGICAL                     :: ICE
   69     LOGICAL                     :: TOTAL
   70   END TYPE NML_SOURCE_T
   71                        
   72                        
   73   ! miscellaneous
   74   CHARACTER(256)                :: MSG
   75   INTEGER                       :: NDSN
   76                                        
   77                                        
   78                                        
   79                                        
   80   CONTAINS
   81 !/ ------------------------------------------------------------------- /
   82   SUBROUTINE W3NMLOUNP (NDSI, INFILE, NML_POINT, NML_FILE,             &
   83                         NML_SPECTRA, NML_PARAM, NML_SOURCE, IERR)
   84 !/
   85 !/                  +-----------------------------------+
   86 !/                  | WAVEWATCH III           NOAA/NCEP |
   87 !/                  |           M. Accensi              |
   88 !/                  |                                   |
   89 !/                  |                        FORTRAN 90 |
   90 !/                  | Last update :         15-May-2018 |
   91 !/                  +-----------------------------------+
   92 !/
   93 !
   94 !  1. Purpose :
   95 !
   96 !     Reads all the namelist to define the output point
   97 !
   98 !  2. Method :
   99 !
  100 !     See source term routines.
  101 !
  102 !  3. Parameters :
  103 !
  104 !     Parameter list
  105 !     ----------------------------------------------------------------
  106 !      NDSI          Int.
  107 !      INFILE        Char.
  108 !      NML_POINT     type.
  109 !      NML_FILE      type.
  110 !      NML_SPECTRA   type.
 
 
                                                      w3nmlounpmd.F90  page   3
 
 
  111 !      NML_PARAM     type.
  112 !      NML_SOURCE    type.
  113 !      IERR          Int.
  114 !     ----------------------------------------------------------------
  115 !
  116 !  4. Subroutines used :
  117 !
  118 !      Name      TYPE  Module   Description
  119 !     ----------------------------------------------------------------
  120 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  121 !      READ_POINT_NML
  122 !     ----------------------------------------------------------------
  123 !
  124 !  5. Called by :
  125 !
  126 !      Name      TYPE  Module   Description
  127 !     ----------------------------------------------------------------
  128 !      WW3_OUNP  Prog.   N/A    Postprocess output points.
  129 !     ----------------------------------------------------------------
  130 !
  131 !  6. Error messages :
  132 !
  133 !     None.
  134 !
  135 !  7. Remarks :
  136 !
  137 !  8. Structure :
  138 !
  139 !     See source code.
  140 !
  141 !  9. Switches :
  142 !
  143 ! 10. Source code :
  144 !
  145 !/ ------------------------------------------------------------------- /
  146                                                                         
  147     USE W3ODATMD, ONLY: NDSE
  148                             
  149     IMPLICIT NONE
  150                  
  151     INTEGER, INTENT(IN)                         :: NDSI
  152     CHARACTER*(*), INTENT(IN)                   :: INFILE
  153     TYPE(NML_POINT_T), INTENT(INOUT)            :: NML_POINT
  154     TYPE(NML_FILE_T), INTENT(INOUT)             :: NML_FILE
  155     TYPE(NML_SPECTRA_T), INTENT(INOUT)          :: NML_SPECTRA
  156     TYPE(NML_PARAM_T), INTENT(INOUT)            :: NML_PARAM
  157     TYPE(NML_SOURCE_T), INTENT(INOUT)           :: NML_SOURCE
  158     INTEGER, INTENT(OUT)                        :: IERR
  159                                                        
  160     IERR = 0
  161             
  162     ! open namelist log file
  163     NDSN = 3
  164     OPEN (NDSN, file=TRIM(INFILE)//'.log', form='formatted', iostat=IERR)
  165        IF (IERR.NE.0) THEN
 
 
                                                      w3nmlounpmd.F90  page   4
 
 
  166       WRITE (NDSE,'(A)') 'ERROR: open full nml file '//TRIM(INFILE)//'.lo>
  167       RETURN
  168     END IF
  169           
  170     ! open input file
  171     OPEN (NDSI, file=TRIM(INFILE), form='formatted', status='old', iostat>
  172     IF (IERR.NE.0) THEN
  173       WRITE (NDSE,'(A)') 'ERROR: open input file '//TRIM(INFILE)//' faile>
  174       RETURN
  175     END IF
  176           
  177     ! read point namelist
  178     CALL READ_POINT_NML (NDSI, NML_POINT)
  179     CALL REPORT_POINT_NML (NML_POINT)
  180                                      
  181     ! read file namelist
  182     CALL READ_FILE_NML (NDSI, NML_FILE)
  183     CALL REPORT_FILE_NML (NML_FILE)
  184                                    
  185     ! read spectra namelist
  186     CALL READ_SPECTRA_NML (NDSI, NML_SPECTRA)
  187     CALL REPORT_SPECTRA_NML (NML_SPECTRA)
  188                                          
  189     ! read param namelist
  190     CALL READ_PARAM_NML (NDSI, NML_PARAM)
  191     CALL REPORT_PARAM_NML (NML_PARAM)
  192                                      
  193     ! read source namelist
  194     CALL READ_SOURCE_NML (NDSI, NML_SOURCE)
  195     CALL REPORT_SOURCE_NML (NML_SOURCE)
  196                                        
  197     ! close namelist files
  198     CLOSE (NDSI)
  199     CLOSE (NDSN)
  200                 
  201   END SUBROUTINE W3NMLOUNP
  202                           
  203                           
  204 !/ ------------------------------------------------------------------- /
  205                                                                         
  206                                                                         
  207                                                                         
  208                                                                         
  209                                                                         
  210                                                                         
  211 !/ ------------------------------------------------------------------- /
  212                                                                         
  213   SUBROUTINE READ_POINT_NML (NDSI, NML_POINT)
  214 !/
  215 !/                  +-----------------------------------+
  216 !/                  | WAVEWATCH III           NOAA/NCEP |
  217 !/                  |           M. Accensi              |
  218 !/                  |                                   |
  219 !/                  |                        FORTRAN 90 |
  220 !/                  | Last update :         15-May-2018 |
 
 
                                                      w3nmlounpmd.F90  page   5
 
 
  221 !/                  +-----------------------------------+
  222 !/
  223 !  1. Purpose :
  224 !
  225 !  2. Method :
  226 !
  227 !     See source term routines.
  228 !
  229 !  3. Parameters :
  230 !
  231 !     Parameter list
  232 !     ----------------------------------------------------------------
  233 !      NDSI         Int.
  234 !      NML_POINT    Type.
  235 !     ----------------------------------------------------------------
  236 !
  237 !  4. Subroutines used :
  238 !
  239 !      Name      TYPE  Module   Description
  240 !     ----------------------------------------------------------------
  241 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  242 !     ----------------------------------------------------------------
  243 !
  244 !  5. Called by :
  245 !
  246 !      Name      TYPE  Module   Description
  247 !     ----------------------------------------------------------------
  248 !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
  249 !     ----------------------------------------------------------------
  250 !
  251 !  6. Error messages :
  252 !
  253 !     None.
  254 !
  255 !  7. Remarks :
  256 !
  257 !  8. Structure :
  258 !
  259 !     See source code.
  260 !
  261 !  9. Switches :
  262 !
  263 ! 10. Source code :
  264 !
  265 !/ ------------------------------------------------------------------- /
  266                                                                         
  267     USE W3ODATMD, ONLY: NDSE
  268     USE W3SERVMD, ONLY: EXTCDE
  269                               
  270     IMPLICIT NONE
  271                  
  272     INTEGER, INTENT(IN)                 :: NDSI
  273     TYPE(NML_POINT_T), INTENT(INOUT)    :: NML_POINT
  274                                                     
  275     ! locals
 
 
                                                      w3nmlounpmd.F90  page   6
 
 
  276     INTEGER                                :: IERR
  277     TYPE(NML_POINT_T) :: POINT
  278     NAMELIST /POINT_NML/ POINT
  279                               
  280     IERR = 0
  281             
  282     ! set default values for point structure
  283     POINT%TIMESTART  = '19000101 000000'
  284     POINT%TIMESTRIDE = '0'
  285     POINT%TIMECOUNT  = '1000000000'
  286     POINT%TIMESPLIT  = 6
  287     POINT%LIST       = 'all'
  288     POINT%SAMEFILE   = .TRUE.
  289     POINT%BUFFER     = 150
  290     POINT%TYPE       = 1
  291     POINT%DIMORDER   = .TRUE.
  292                              
  293     ! read point namelist
  294     REWIND (NDSI)
  295     READ (NDSI, nml=POINT_NML, iostat=IERR, iomsg=MSG)
  296     IF (IERR.NE.0) THEN
  297       WRITE (NDSE,'(A,/A)') &
  298         'ERROR: READ_POINT_NML: namelist read error', &
  299         'ERROR: '//TRIM(MSG)
  300       CALL EXTCDE (1)
  301     END IF
  302           
  303     ! save namelist
  304     NML_POINT = POINT
  305                      
  306   END SUBROUTINE READ_POINT_NML
  307                                
  308 !/ ------------------------------------------------------------------- /
  309                                                                         
  310                                                                         
  311                                                                         
  312 !/ ------------------------------------------------------------------- /
  313                                                                         
  314   SUBROUTINE READ_FILE_NML (NDSI, NML_FILE)
  315 !/
  316 !/                  +-----------------------------------+
  317 !/                  | WAVEWATCH III           NOAA/NCEP |
  318 !/                  |           M. Accensi              |
  319 !/                  |                                   |
  320 !/                  |                        FORTRAN 90 |
  321 !/                  | Last update :         15-May-2018 |
  322 !/                  +-----------------------------------+
  323 !/
  324 !  1. Purpose :
  325 !
  326 !  2. Method :
  327 !
  328 !     See source term routines.
  329 !
  330 !  3. Parameters :
 
 
                                                      w3nmlounpmd.F90  page   7
 
 
  331 !
  332 !     Parameter list
  333 !     ----------------------------------------------------------------
  334 !      NDSI         Int.
  335 !      NML_FILE     Type.
  336 !     ----------------------------------------------------------------
  337 !
  338 !  4. Subroutines used :
  339 !
  340 !      Name      TYPE  Module   Description
  341 !     ----------------------------------------------------------------
  342 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  343 !     ----------------------------------------------------------------
  344 !
  345 !  5. Called by :
  346 !
  347 !      Name      TYPE  Module   Description
  348 !     ----------------------------------------------------------------
  349 !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
  350 !     ----------------------------------------------------------------
  351 !
  352 !  6. Error messages :
  353 !
  354 !     None.
  355 !
  356 !  7. Remarks :
  357 !
  358 !  8. Structure :
  359 !
  360 !     See source code.
  361 !
  362 !  9. Switches :
  363 !
  364 ! 10. Source code :
  365 !
  366 !/ ------------------------------------------------------------------- /
  367                                                                         
  368     USE W3ODATMD, ONLY: NDSE
  369     USE W3SERVMD, ONLY: EXTCDE
  370                               
  371     IMPLICIT NONE
  372                  
  373     INTEGER, INTENT(IN)                 :: NDSI
  374     TYPE(NML_FILE_T), INTENT(INOUT)     :: NML_FILE
  375                                                    
  376     ! locals
  377     INTEGER                                :: IERR
  378     TYPE(NML_FILE_T) :: FILE
  379     NAMELIST /FILE_NML/ FILE
  380                             
  381     IERR = 0
  382             
  383     ! set default values for file structure
  384     FILE%PREFIX    = 'ww3.'
  385     FILE%NETCDF    = 3
 
 
                                                      w3nmlounpmd.F90  page   8
 
 
  386                       
  387                       
  388     ! read file namelist
  389     REWIND (NDSI)
  390     READ (NDSI, nml=FILE_NML, iostat=IERR, iomsg=MSG)
  391     IF (IERR.GT.0) THEN
  392       WRITE (NDSE,'(A,/A)') &
  393         'ERROR: READ_FILE_NML: namelist read error', &
  394         'ERROR: '//TRIM(MSG)
  395       CALL EXTCDE (2)
  396     END IF
  397           
  398     ! save namelist
  399     NML_FILE = FILE
  400                    
  401   END SUBROUTINE READ_FILE_NML
  402                               
  403 !/ ------------------------------------------------------------------- /
  404                                                                         
  405                                                                         
  406                                                                         
  407 !/ ------------------------------------------------------------------- /
  408                                                                         
  409   SUBROUTINE READ_SPECTRA_NML (NDSI, NML_SPECTRA)
  410 !/
  411 !/                  +-----------------------------------+
  412 !/                  | WAVEWATCH III           NOAA/NCEP |
  413 !/                  |           M. Accensi              |
  414 !/                  |                                   |
  415 !/                  |                        FORTRAN 90 |
  416 !/                  | Last update :         15-May-2018 |
  417 !/                  +-----------------------------------+
  418 !/
  419 !  1. Purpose :
  420 !
  421 !  2. Method :
  422 !
  423 !     See source term routines.
  424 !
  425 !  3. Parameters :
  426 !
  427 !     Parameter list
  428 !     ----------------------------------------------------------------
  429 !      NDSI         Int.
  430 !      NML_SPECTRA  Type.
  431 !     ----------------------------------------------------------------
  432 !
  433 !  4. Subroutines used :
  434 !
  435 !      Name      TYPE  Module   Description
  436 !     ----------------------------------------------------------------
  437 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  438 !     ----------------------------------------------------------------
  439 !
  440 !  5. Called by :
 
 
                                                      w3nmlounpmd.F90  page   9
 
 
  441 !
  442 !      Name      TYPE  Module   Description
  443 !     ----------------------------------------------------------------
  444 !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
  445 !     ----------------------------------------------------------------
  446 !
  447 !  6. Error messages :
  448 !
  449 !     None.
  450 !
  451 !  7. Remarks :
  452 !
  453 !  8. Structure :
  454 !
  455 !     See source code.
  456 !
  457 !  9. Switches :
  458 !
  459 ! 10. Source code :
  460 !
  461 !/ ------------------------------------------------------------------- /
  462                                                                         
  463     USE W3ODATMD, ONLY: NDSE
  464     USE W3SERVMD, ONLY: EXTCDE
  465                               
  466     IMPLICIT NONE
  467                  
  468     INTEGER, INTENT(IN)                 :: NDSI
  469     TYPE(NML_SPECTRA_T), INTENT(INOUT)  :: NML_SPECTRA
  470                                                       
  471     ! locals
  472     INTEGER                                :: IERR
  473     TYPE(NML_SPECTRA_T) :: SPECTRA
  474     NAMELIST /SPECTRA_NML/ SPECTRA
  475                                   
  476     IERR = 0
  477             
  478     ! set default values for spectra structure
  479     SPECTRA%OUTPUT      = 3
  480     SPECTRA%SCALE_FAC   = 1
  481     SPECTRA%OUTPUT_FAC  = 0
  482                            
  483                            
  484     ! read spectra namelist
  485     REWIND (NDSI)
  486     READ (NDSI, nml=SPECTRA_NML, iostat=IERR, iomsg=MSG)
  487     IF (IERR.GT.0) THEN
  488       WRITE (NDSE,'(A,/A)') &
  489         'ERROR: READ_SPECTRA_NML: namelist read error', &
  490         'ERROR: '//TRIM(MSG)
  491       CALL EXTCDE (3)
  492     END IF
  493           
  494     ! save namelist
  495     NML_SPECTRA = SPECTRA
 
 
                                                      w3nmlounpmd.F90  page  10
 
 
  496                          
  497   END SUBROUTINE READ_SPECTRA_NML
  498                                  
  499 !/ ------------------------------------------------------------------- /
  500                                                                         
  501                                                                         
  502 !/ ------------------------------------------------------------------- /
  503                                                                         
  504   SUBROUTINE READ_PARAM_NML (NDSI, NML_PARAM)
  505 !/
  506 !/                  +-----------------------------------+
  507 !/                  | WAVEWATCH III           NOAA/NCEP |
  508 !/                  |           M. Accensi              |
  509 !/                  |                                   |
  510 !/                  |                        FORTRAN 90 |
  511 !/                  | Last update :         15-May-2018 |
  512 !/                  +-----------------------------------+
  513 !/
  514 !  1. Purpose :
  515 !
  516 !  2. Method :
  517 !
  518 !     See source term routines.
  519 !
  520 !  3. Parameters :
  521 !
  522 !     Parameter list
  523 !     ----------------------------------------------------------------
  524 !      NDSI         Int.
  525 !      NML_PARAM    Type.
  526 !     ----------------------------------------------------------------
  527 !
  528 !  4. Subroutines used :
  529 !
  530 !      Name      TYPE  Module   Description
  531 !     ----------------------------------------------------------------
  532 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  533 !     ----------------------------------------------------------------
  534 !
  535 !  5. Called by :
  536 !
  537 !      Name      TYPE  Module   Description
  538 !     ----------------------------------------------------------------
  539 !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
  540 !     ----------------------------------------------------------------
  541 !
  542 !  6. Error messages :
  543 !
  544 !     None.
  545 !
  546 !  7. Remarks :
  547 !
  548 !  8. Structure :
  549 !
  550 !     See source code.
 
 
                                                      w3nmlounpmd.F90  page  11
 
 
  551 !
  552 !  9. Switches :
  553 !
  554 ! 10. Source code :
  555 !
  556 !/ ------------------------------------------------------------------- /
  557                                                                         
  558     USE W3ODATMD, ONLY: NDSE
  559     USE W3SERVMD, ONLY: EXTCDE
  560                               
  561     IMPLICIT NONE
  562                  
  563     INTEGER, INTENT(IN)                 :: NDSI
  564     TYPE(NML_PARAM_T), INTENT(INOUT)    :: NML_PARAM
  565                                                     
  566     ! locals
  567     INTEGER                                :: IERR
  568     TYPE(NML_PARAM_T) :: PARAM
  569     NAMELIST /PARAM_NML/ PARAM
  570                               
  571     IERR = 0
  572             
  573     ! set default values for param structure
  574     PARAM%OUTPUT      = 3
  575                          
  576     ! read param namelist
  577     REWIND (NDSI)
  578     READ (NDSI, nml=PARAM_NML, iostat=IERR, iomsg=MSG)
  579     IF (IERR.GT.0) THEN
  580       WRITE (NDSE,'(A,/A)') &
  581         'ERROR: READ_PARAM_NML: namelist read error', &
  582         'ERROR: '//TRIM(MSG)
  583       CALL EXTCDE (4)
  584     END IF
  585           
  586     ! save namelist
  587     NML_PARAM = PARAM
  588                      
  589   END SUBROUTINE READ_PARAM_NML
  590                                
  591 !/ ------------------------------------------------------------------- /
  592                                                                         
  593                                                                         
  594                                                                         
  595   SUBROUTINE READ_SOURCE_NML (NDSI, NML_SOURCE)
  596 !/
  597 !/                  +-----------------------------------+
  598 !/                  | WAVEWATCH III           NOAA/NCEP |
  599 !/                  |           M. Accensi              |
  600 !/                  |                                   |
  601 !/                  |                        FORTRAN 90 |
  602 !/                  | Last update :         15-May-2018 |
  603 !/                  +-----------------------------------+
  604 !/
  605 !  1. Purpose :
 
 
                                                      w3nmlounpmd.F90  page  12
 
 
  606 !
  607 !  2. Method :
  608 !
  609 !     See source term routines.
  610 !
  611 !  3. Parameters :
  612 !
  613 !     Parameter list
  614 !     ----------------------------------------------------------------
  615 !      NDSI         Int.
  616 !      NML_SOURCE   Type.
  617 !     ----------------------------------------------------------------
  618 !
  619 !  4. Subroutines used :
  620 !
  621 !      Name      TYPE  Module   Description
  622 !     ----------------------------------------------------------------
  623 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  624 !     ----------------------------------------------------------------
  625 !
  626 !  5. Called by :
  627 !
  628 !      Name      TYPE  Module   Description
  629 !     ----------------------------------------------------------------
  630 !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
  631 !     ----------------------------------------------------------------
  632 !
  633 !  6. Error messages :
  634 !
  635 !     None.
  636 !
  637 !  7. Remarks :
  638 !
  639 !  8. Structure :
  640 !
  641 !     See source code.
  642 !
  643 !  9. Switches :
  644 !
  645 ! 10. Source code :
  646 !
  647 !/ ------------------------------------------------------------------- /
  648                                                                         
  649     USE W3ODATMD, ONLY: NDSE
  650     USE W3SERVMD, ONLY: EXTCDE
  651                               
  652     IMPLICIT NONE
  653                  
  654     INTEGER, INTENT(IN)                 :: NDSI
  655     TYPE(NML_SOURCE_T), INTENT(INOUT)   :: NML_SOURCE
  656                                                      
  657     ! locals
  658     INTEGER                                :: IERR
  659     TYPE(NML_SOURCE_T) :: SOURCE
  660     NAMELIST /SOURCE_NML/ SOURCE
 
 
                                                      w3nmlounpmd.F90  page  13
 
 
  661                                 
  662     IERR = 0
  663             
  664     ! set default values for source structure
  665     SOURCE%OUTPUT      = 4
  666     SOURCE%SCALE_FAC   = 0
  667     SOURCE%OUTPUT_FAC  = 0
  668     SOURCE%TABLE_FAC   = 0
  669     SOURCE%SPECTRUM    = .TRUE.
  670     SOURCE%INPUT       = .TRUE.
  671     SOURCE%INTERACTIONS= .TRUE.
  672     SOURCE%DISSIPATION = .TRUE.
  673     SOURCE%BOTTOM      = .TRUE.
  674     SOURCE%ICE         = .TRUE.
  675     SOURCE%TOTAL       = .TRUE.
  676                                
  677     ! read source namelist
  678     REWIND (NDSI)
  679     READ (NDSI, nml=SOURCE_NML, iostat=IERR, iomsg=MSG)
  680     IF (IERR.GT.0) THEN
  681       WRITE (NDSE,'(A,/A)') &
  682         'ERROR: READ_SOURCE_NML: namelist read error', &
  683         'ERROR: '//TRIM(MSG)
  684       CALL EXTCDE (5)
  685     END IF
  686           
  687     ! save namelist
  688     NML_SOURCE = SOURCE
  689                        
  690   END SUBROUTINE READ_SOURCE_NML
  691                                 
  692 !/ ------------------------------------------------------------------- /
  693                                                                         
  694                                                                         
  695                                                                         
  696                                                                         
  697                                                                         
  698                                                                         
  699                                                                         
  700 !/ ------------------------------------------------------------------- /
  701                                                                         
  702   SUBROUTINE REPORT_POINT_NML (NML_POINT)
  703 !/
  704 !/                  +-----------------------------------+
  705 !/                  | WAVEWATCH III           NOAA/NCEP |
  706 !/                  |           M. Accensi              |
  707 !/                  |                        FORTRAN 90 |
  708 !/                  | Last update :         15-May-2018 |
  709 !/                  +-----------------------------------+
  710 !/
  711 !/
  712 !  1. Purpose :
  713 !
  714 !  2. Method :
  715 !
 
 
                                                      w3nmlounpmd.F90  page  14
 
 
  716 !     See source term routines.
  717 !
  718 !  3. Parameters :
  719 !
  720 !     Parameter list
  721 !     ----------------------------------------------------------------
  722 !      NML_POINT  Type.
  723 !     ----------------------------------------------------------------
  724 !
  725 !  4. Subroutines used :
  726 !
  727 !      Name      TYPE  Module   Description
  728 !     ----------------------------------------------------------------
  729 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  730 !     ----------------------------------------------------------------
  731 !
  732 !  5. Called by :
  733 !
  734 !      Name      TYPE  Module   Description
  735 !     ----------------------------------------------------------------
  736 !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
  737 !     ----------------------------------------------------------------
  738 !
  739 !  6. Error messages :
  740 !
  741 !     None.
  742 !
  743 !  7. Remarks :
  744 !
  745 !  8. Structure :
  746 !
  747 !     See source code.
  748 !
  749 !  9. Switches :
  750 !
  751 ! 10. Source code :
  752 !
  753 !/ ------------------------------------------------------------------- /
  754                                                                         
  755                                                                         
  756     IMPLICIT NONE
  757                  
  758     TYPE(NML_POINT_T), INTENT(IN) :: NML_POINT
  759                                               
  760                                               
  761       WRITE (MSG,'(A)') 'POINT % '
  762       WRITE (NDSN,'(A)')
  763       WRITE (NDSN,10) TRIM(MSG),'TIMESTART  = ', TRIM(NML_POINT%TIMESTART)
  764       WRITE (NDSN,10) TRIM(MSG),'TIMESTRIDE = ', TRIM(NML_POINT%TIMESTRID>
  765       WRITE (NDSN,10) TRIM(MSG),'TIMECOUNT  = ', TRIM(NML_POINT%TIMECOUNT)
  766                                                                           
  767       WRITE (NDSN,11) TRIM(MSG),'TIMESPLIT  = ', NML_POINT%TIMESPLIT
  768       WRITE (NDSN,10) TRIM(MSG),'LIST       = ', TRIM(NML_POINT%LIST)
  769       WRITE (NDSN,13) TRIM(MSG),'SAMEFILE   = ', NML_POINT%SAMEFILE
  770       WRITE (NDSN,11) TRIM(MSG),'BUFFER     = ', NML_POINT%BUFFER
 
 
                                                      w3nmlounpmd.F90  page  15
 
 
  771       WRITE (NDSN,11) TRIM(MSG),'TYPE       = ', NML_POINT%TYPE
  772       WRITE (NDSN,13) TRIM(MSG),'DIMORDER   = ', NML_POINT%DIMORDER
  773                                                                    
  774 10  FORMAT (A,2X,A,A)
  775 11  FORMAT (A,2X,A,I8)
  776 13  FORMAT (A,2X,A,L1)
  777                       
  778   END SUBROUTINE REPORT_POINT_NML
  779                                  
  780 !/ ------------------------------------------------------------------- /
  781                                                                         
  782                                                                         
  783                                                                         
  784                                                                         
  785                                                                         
  786                                                                         
  787 !/ ------------------------------------------------------------------- /
  788                                                                         
  789   SUBROUTINE REPORT_FILE_NML (NML_FILE)
  790 !/
  791 !/                  +-----------------------------------+
  792 !/                  | WAVEWATCH III           NOAA/NCEP |
  793 !/                  |           M. Accensi              |
  794 !/                  |                        FORTRAN 90 |
  795 !/                  | Last update :         15-May-2018 |
  796 !/                  +-----------------------------------+
  797 !/
  798 !/
  799 !  1. Purpose :
  800 !
  801 !  2. Method :
  802 !
  803 !     See source term routines.
  804 !
  805 !  3. Parameters :
  806 !
  807 !     Parameter list
  808 !     ----------------------------------------------------------------
  809 !      NML_FILE  Type.
  810 !     ----------------------------------------------------------------
  811 !
  812 !  4. Subroutines used :
  813 !
  814 !      Name      TYPE  Module   Description
  815 !     ----------------------------------------------------------------
  816 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  817 !     ----------------------------------------------------------------
  818 !
  819 !  5. Called by :
  820 !
  821 !      Name      TYPE  Module   Description
  822 !     ----------------------------------------------------------------
  823 !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
  824 !     ----------------------------------------------------------------
  825 !
 
 
                                                      w3nmlounpmd.F90  page  16
 
 
  826 !  6. Error messages :
  827 !
  828 !     None.
  829 !
  830 !  7. Remarks :
  831 !
  832 !  8. Structure :
  833 !
  834 !     See source code.
  835 !
  836 !  9. Switches :
  837 !
  838 ! 10. Source code :
  839 !
  840 !/ ------------------------------------------------------------------- /
  841                                                                         
  842                                                                         
  843     IMPLICIT NONE
  844                  
  845     TYPE(NML_FILE_T), INTENT(IN) :: NML_FILE
  846                                             
  847                                             
  848       WRITE (MSG,'(A)') 'FILE % '
  849       WRITE (NDSN,'(A)')
  850       WRITE (NDSN,10) TRIM(MSG),'PREFIX    = ', TRIM(NML_FILE%PREFIX)
  851       WRITE (NDSN,11) TRIM(MSG),'NETCDF    = ', NML_FILE%NETCDF
  852                                                                
  853                                                                
  854 10  FORMAT (A,2X,A,A)
  855 11  FORMAT (A,2X,A,I8)
  856                       
  857   END SUBROUTINE REPORT_FILE_NML
  858                                 
  859 !/ ------------------------------------------------------------------- /
  860                                                                         
  861                                                                         
  862 !/ ------------------------------------------------------------------- /
  863                                                                         
  864   SUBROUTINE REPORT_SPECTRA_NML (NML_SPECTRA)
  865 !/
  866 !/                  +-----------------------------------+
  867 !/                  | WAVEWATCH III           NOAA/NCEP |
  868 !/                  |           M. Accensi              |
  869 !/                  |                        FORTRAN 90 |
  870 !/                  | Last update :         15-May-2018 |
  871 !/                  +-----------------------------------+
  872 !/
  873 !/
  874 !  1. Purpose :
  875 !
  876 !  2. Method :
  877 !
  878 !     See source term routines.
  879 !
  880 !  3. Parameters :
 
 
                                                      w3nmlounpmd.F90  page  17
 
 
  881 !
  882 !     Parameter list
  883 !     ----------------------------------------------------------------
  884 !      NML_SPECTRA  Type.
  885 !     ----------------------------------------------------------------
  886 !
  887 !  4. Subroutines used :
  888 !
  889 !      Name      TYPE  Module   Description
  890 !     ----------------------------------------------------------------
  891 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  892 !     ----------------------------------------------------------------
  893 !
  894 !  5. Called by :
  895 !
  896 !      Name      TYPE  Module   Description
  897 !     ----------------------------------------------------------------
  898 !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
  899 !     ----------------------------------------------------------------
  900 !
  901 !  6. Error messages :
  902 !
  903 !     None.
  904 !
  905 !  7. Remarks :
  906 !
  907 !  8. Structure :
  908 !
  909 !     See source code.
  910 !
  911 !  9. Switches :
  912 !
  913 ! 10. Source code :
  914 !
  915 !/ ------------------------------------------------------------------- /
  916                                                                         
  917                                                                         
  918     IMPLICIT NONE
  919                  
  920     TYPE(NML_SPECTRA_T), INTENT(IN) :: NML_SPECTRA
  921                                                   
  922                                                   
  923       WRITE (MSG,'(A)') 'SPECTRA % '
  924       WRITE (NDSN,'(A)')
  925       WRITE (NDSN,11) TRIM(MSG),'OUTPUT     = ', NML_SPECTRA%OUTPUT
  926       WRITE (NDSN,14) TRIM(MSG),'SCALE_FAC  = ', NML_SPECTRA%SCALE_FAC
  927       WRITE (NDSN,14) TRIM(MSG),'OUTPUT_FAC = ', NML_SPECTRA%OUTPUT_FAC
  928                                                                        
  929                                                                        
  930 11  FORMAT (A,2X,A,I8)
  931 14  FORMAT (A,2X,A,F8.2)
  932                         
  933   END SUBROUTINE REPORT_SPECTRA_NML
  934                                    
  935 !/ ------------------------------------------------------------------- /
 
 
                                                      w3nmlounpmd.F90  page  18
 
 
  936                                                                         
  937                                                                         
  938 !/ ------------------------------------------------------------------- /
  939                                                                         
  940   SUBROUTINE REPORT_PARAM_NML (NML_PARAM)
  941 !/
  942 !/                  +-----------------------------------+
  943 !/                  | WAVEWATCH III           NOAA/NCEP |
  944 !/                  |           M. Accensi              |
  945 !/                  |                        FORTRAN 90 |
  946 !/                  | Last update :         15-May-2018 |
  947 !/                  +-----------------------------------+
  948 !/
  949 !/
  950 !  1. Purpose :
  951 !
  952 !  2. Method :
  953 !
  954 !     See source term routines.
  955 !
  956 !  3. Parameters :
  957 !
  958 !     Parameter list
  959 !     ----------------------------------------------------------------
  960 !      NML_PARAM  Type.
  961 !     ----------------------------------------------------------------
  962 !
  963 !  4. Subroutines used :
  964 !
  965 !      Name      TYPE  Module   Description
  966 !     ----------------------------------------------------------------
  967 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  968 !     ----------------------------------------------------------------
  969 !
  970 !  5. Called by :
  971 !
  972 !      Name      TYPE  Module   Description
  973 !     ----------------------------------------------------------------
  974 !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
  975 !     ----------------------------------------------------------------
  976 !
  977 !  6. Error messages :
  978 !
  979 !     None.
  980 !
  981 !  7. Remarks :
  982 !
  983 !  8. Structure :
  984 !
  985 !     See source code.
  986 !
  987 !  9. Switches :
  988 !
  989 ! 10. Source code :
  990 !
 
 
                                                      w3nmlounpmd.F90  page  19
 
 
  991 !/ ------------------------------------------------------------------- /
  992                                                                         
  993                                                                         
  994     IMPLICIT NONE
  995                  
  996     TYPE(NML_PARAM_T), INTENT(IN) :: NML_PARAM
  997                                               
  998                                               
  999       WRITE (MSG,'(A)') 'PARAM % '
 1000       WRITE (NDSN,'(A)')
 1001       WRITE (NDSN,11) TRIM(MSG),'OUTPUT     = ', NML_PARAM%OUTPUT
 1002                                                                  
 1003 11  FORMAT (A,2X,A,I8)
 1004                       
 1005   END SUBROUTINE REPORT_PARAM_NML
 1006                                  
 1007 !/ ------------------------------------------------------------------- /
 1008                                                                         
 1009 !/ ------------------------------------------------------------------- /
 1010                                                                         
 1011   SUBROUTINE REPORT_SOURCE_NML (NML_SOURCE)
 1012 !/
 1013 !/                  +-----------------------------------+
 1014 !/                  | WAVEWATCH III           NOAA/NCEP |
 1015 !/                  |           M. Accensi              |
 1016 !/                  |                        FORTRAN 90 |
 1017 !/                  | Last update :         15-May-2018 |
 1018 !/                  +-----------------------------------+
 1019 !/
 1020 !/
 1021 !  1. Purpose :
 1022 !
 1023 !  2. Method :
 1024 !
 1025 !     See source term routines.
 1026 !
 1027 !  3. Parameters :
 1028 !
 1029 !     Parameter list
 1030 !     ----------------------------------------------------------------
 1031 !      NML_SOURCE  Type.
 1032 !     ----------------------------------------------------------------
 1033 !
 1034 !  4. Subroutines used :
 1035 !
 1036 !      Name      TYPE  Module   Description
 1037 !     ----------------------------------------------------------------
 1038 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
 1039 !     ----------------------------------------------------------------
 1040 !
 1041 !  5. Called by :
 1042 !
 1043 !      Name      TYPE  Module   Description
 1044 !     ----------------------------------------------------------------
 1045 !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
 
 
                                                      w3nmlounpmd.F90  page  20
 
 
 1046 !     ----------------------------------------------------------------
 1047 !
 1048 !  6. Error messages :
 1049 !
 1050 !     None.
 1051 !
 1052 !  7. Remarks :
 1053 !
 1054 !  8. Structure :
 1055 !
 1056 !     See source code.
 1057 !
 1058 !  9. Switches :
 1059 !
 1060 ! 10. Source code :
 1061 !
 1062 !/ ------------------------------------------------------------------- /
 1063                                                                         
 1064                                                                         
 1065     IMPLICIT NONE
 1066                  
 1067     TYPE(NML_SOURCE_T), INTENT(IN) :: NML_SOURCE
 1068                                                 
 1069                                                 
 1070       WRITE (MSG,'(A)') 'SOURCE % '
 1071       WRITE (NDSN,'(A)')
 1072       WRITE (NDSN,11) TRIM(MSG),'OUTPUT       = ', NML_SOURCE%OUTPUT
 1073       WRITE (NDSN,14) TRIM(MSG),'SCALE_FAC    = ', NML_SOURCE%SCALE_FAC
 1074       WRITE (NDSN,14) TRIM(MSG),'OUTPUT_FAC   = ', NML_SOURCE%OUTPUT_FAC
 1075       WRITE (NDSN,11) TRIM(MSG),'TABLE_FAC    = ', NML_SOURCE%TABLE_FAC
 1076       WRITE (NDSN,13) TRIM(MSG),'SPECTRUM     = ', NML_SOURCE%SPECTRUM
 1077       WRITE (NDSN,13) TRIM(MSG),'INPUT        = ', NML_SOURCE%INPUT
 1078       WRITE (NDSN,13) TRIM(MSG),'INTERACTIONS = ', NML_SOURCE%INTERACTIONS
 1079       WRITE (NDSN,13) TRIM(MSG),'DISSIPATION  = ', NML_SOURCE%DISSIPATION
 1080       WRITE (NDSN,13) TRIM(MSG),'ICE          = ', NML_SOURCE%ICE
 1081       WRITE (NDSN,13) TRIM(MSG),'TOTAL        = ', NML_SOURCE%TOTAL
 1082                                                                    
 1083                                                                    
 1084                                                                    
 1085 11  FORMAT (A,2X,A,I8)
 1086 13  FORMAT (A,2X,A,L1)
 1087 14  FORMAT (A,2X,A,F8.2)
 1088                         
 1089   END SUBROUTINE REPORT_SOURCE_NML
 1090                                   
 1091 !/ ------------------------------------------------------------------- /
 1092                                                                         
 1093                                                                         
 1094                                                                         
 1095 END MODULE W3NMLOUNPMD
 1096                       
 1097 !/ ------------------------------------------------------------------- /
 1098                                                                         
 1099                                                                         
 1100                                                                         
 
 
                                                      w3nmlounpmd.F90  page  21
 
 
 1101                                                                         
 1102                                                                         
 1103                                                                         
 1104                                                                         
 1105                                                                         
 1106                                                                         
 1107                                                                         
 1108                                                                         
 1109                                                                         
 1110                                                                         
 1111                                                                         
 1112                                                                         
 1113                                                                         
 1114                                                                         
 1115                                                                         
 1116                                                                         
 1117                                                                         
 1118                                                                         
 1119                                                                         
 1120                                                                         
 1121                                                                         
 1122                                                                         
 1123                                                                         
 1124                                                                         
 1125                                                                         
 1126                                                                         
 1127                                                                         
 1128                                                                         
 1129                                                                         
 1130                                                                         
 1131                                                                         
 1132                                                                         
 1133                                                                         
 1134                                                                         
 1135                                                                         
 1136                                                                         
 1137                                                                         
 1138                                                                         
 1139                                                                         
 1140                                                                         
 1141                                                                         
 1142                                                                         
 1143                                                                         
 1144                                                                         
 1145                                                                         
 1146                                                                         
 1147                                                                         
 1148                                                                         
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/opt/cray/netcdf-hdf5parallel/4.2.0/pgi/119/include -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
