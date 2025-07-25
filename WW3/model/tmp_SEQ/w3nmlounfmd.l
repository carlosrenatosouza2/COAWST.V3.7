                                                      w3nmlounfmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3NMLOUNFMD
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
   17 !     Manages namelists from configuration file ww3_ounf.nml for ww3_ounf>
   18 !
   19 !/ ------------------------------------------------------------------- /
   20                                                                         
   21   ! module defaults
   22   IMPLICIT NONE
   23                
   24   PUBLIC
   25         
   26   ! field structure
   27   TYPE NML_FIELD_T
   28     CHARACTER(15)               :: TIMESTART
   29     CHARACTER(15)               :: TIMESTRIDE
   30     CHARACTER(15)               :: TIMECOUNT
   31     INTEGER                     :: TIMESPLIT
   32     CHARACTER(1024)             :: LIST
   33     CHARACTER(15)               :: PARTITION
   34     LOGICAL                     :: SAMEFILE
   35     LOGICAL                     :: VECTOR
   36     INTEGER                     :: TYPE
   37   END TYPE NML_FIELD_T
   38                       
   39   ! file structure
   40   TYPE NML_FILE_T
   41     CHARACTER(30)               :: PREFIX
   42     INTEGER                     :: NETCDF
   43     INTEGER                     :: IX0
   44     INTEGER                     :: IXN
   45     INTEGER                     :: IY0
   46     INTEGER                     :: IYN
   47   END TYPE NML_FILE_T
   48                      
   49   ! smc grid structure
   50   TYPE NML_SMC_T
   51     INTEGER                     :: TYPE
   52     REAL                        :: SXO
   53     REAL                        :: SYO
   54     REAL                        :: EXO
   55     REAL                        :: EYO
 
 
                                                      w3nmlounfmd.F90  page   2
 
 
   56     INTEGER                     :: CELFAC
   57     REAL                        :: NOVAL
   58   END TYPE NML_SMC_T
   59                     
   60   ! miscellaneous
   61   CHARACTER(256)                :: MSG
   62   INTEGER                       :: NDSN
   63                                        
   64                                        
   65                                        
   66                                        
   67   CONTAINS
   68 !/ ------------------------------------------------------------------- /
   69   SUBROUTINE W3NMLOUNF (NDSI, INFILE, NML_FIELD, NML_FILE, NML_SMC, IERR)
   70 !/
   71 !/                  +-----------------------------------+
   72 !/                  | WAVEWATCH III           NOAA/NCEP |
   73 !/                  |           M. Accensi              |
   74 !/                  |                                   |
   75 !/                  |                        FORTRAN 90 |
   76 !/                  | Last update :         15-May-2018 |
   77 !/                  +-----------------------------------+
   78 !/
   79 !
   80 !  1. Purpose :
   81 !
   82 !     Reads all the namelist to define the output field
   83 !
   84 !  2. Method :
   85 !
   86 !     See source term routines.
   87 !
   88 !  3. Parameters :
   89 !
   90 !     Parameter list
   91 !     ----------------------------------------------------------------
   92 !      NDSI        Int.
   93 !      INFILE      Char.
   94 !      NML_FIELD   type.
   95 !      NML_FILE    type.
   96 !      NML_SMC     type.
   97 !      IERR        Int.
   98 !     ----------------------------------------------------------------
   99 !
  100 !  4. Subroutines used :
  101 !
  102 !      Name      TYPE  Module   Description
  103 !     ----------------------------------------------------------------
  104 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  105 !      READ_FIELD_NML
  106 !     ----------------------------------------------------------------
  107 !
  108 !  5. Called by :
  109 !
  110 !      Name      TYPE  Module   Description
 
 
                                                      w3nmlounfmd.F90  page   3
 
 
  111 !     ----------------------------------------------------------------
  112 !      WW3_OUNF  Prog.   N/A    Postprocess output fields.
  113 !     ----------------------------------------------------------------
  114 !
  115 !  6. Error messages :
  116 !
  117 !     None.
  118 !
  119 !  7. Remarks :
  120 !
  121 !  8. Structure :
  122 !
  123 !     See source code.
  124 !
  125 !  9. Switches :
  126 !
  127 ! 10. Source code :
  128 !
  129 !/ ------------------------------------------------------------------- /
  130                                                                         
  131     USE W3ODATMD, ONLY: NDSE
  132                             
  133     IMPLICIT NONE
  134                  
  135     INTEGER, INTENT(IN)                         :: NDSI
  136     CHARACTER*(*), INTENT(IN)                   :: INFILE
  137     TYPE(NML_FIELD_T), INTENT(INOUT)            :: NML_FIELD
  138     TYPE(NML_FILE_T), INTENT(INOUT)             :: NML_FILE
  139     TYPE(NML_SMC_T), INTENT(INOUT)              :: NML_SMC
  140     INTEGER, INTENT(OUT)                        :: IERR
  141                                                        
  142     IERR = 0
  143             
  144     ! open namelist log file
  145     NDSN = 3
  146     OPEN (NDSN, file=TRIM(INFILE)//'.log', form='formatted', iostat=IERR)
  147        IF (IERR.NE.0) THEN
  148       WRITE (NDSE,'(A)') 'ERROR: open full nml file '//TRIM(INFILE)//'.lo>
  149       RETURN
  150     END IF
  151           
  152     ! open input file
  153     OPEN (NDSI, file=TRIM(INFILE), form='formatted', status='old', iostat>
  154     IF (IERR.NE.0) THEN
  155       WRITE (NDSE,'(A)') 'ERROR: open input file '//TRIM(INFILE)//' faile>
  156       RETURN
  157     END IF
  158           
  159     ! read field namelist
  160     CALL READ_FIELD_NML (NDSI, NML_FIELD)
  161     CALL REPORT_FIELD_NML (NML_FIELD)
  162                                      
  163     ! read file namelist
  164     CALL READ_FILE_NML (NDSI, NML_FILE)
  165     CALL REPORT_FILE_NML (NML_FILE)
 
 
                                                      w3nmlounfmd.F90  page   4
 
 
  166                                    
  167     ! read smc namelist
  168     CALL READ_SMC_NML (NDSI, NML_SMC)
  169     CALL REPORT_SMC_NML (NML_SMC)
  170                                  
  171     ! close namelist files
  172     CLOSE (NDSI)
  173     CLOSE (NDSN)
  174                 
  175   END SUBROUTINE W3NMLOUNF
  176                           
  177                           
  178 !/ ------------------------------------------------------------------- /
  179                                                                         
  180                                                                         
  181                                                                         
  182                                                                         
  183                                                                         
  184                                                                         
  185 !/ ------------------------------------------------------------------- /
  186                                                                         
  187   SUBROUTINE READ_FIELD_NML (NDSI, NML_FIELD)
  188 !/
  189 !/                  +-----------------------------------+
  190 !/                  | WAVEWATCH III           NOAA/NCEP |
  191 !/                  |           M. Accensi              |
  192 !/                  |                                   |
  193 !/                  |                        FORTRAN 90 |
  194 !/                  | Last update :         15-May-2018 |
  195 !/                  +-----------------------------------+
  196 !/
  197 !  1. Purpose :
  198 !
  199 !  2. Method :
  200 !
  201 !     See source term routines.
  202 !
  203 !  3. Parameters :
  204 !
  205 !     Parameter list
  206 !     ----------------------------------------------------------------
  207 !      NDSI         Int.
  208 !      NML_FIELD    Type.
  209 !     ----------------------------------------------------------------
  210 !
  211 !  4. Subroutines used :
  212 !
  213 !      Name      TYPE  Module   Description
  214 !     ----------------------------------------------------------------
  215 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  216 !     ----------------------------------------------------------------
  217 !
  218 !  5. Called by :
  219 !
  220 !      Name      TYPE  Module   Description
 
 
                                                      w3nmlounfmd.F90  page   5
 
 
  221 !     ----------------------------------------------------------------
  222 !      W3NMLOUNF Subr.   N/A    Namelist configuration routine.
  223 !     ----------------------------------------------------------------
  224 !
  225 !  6. Error messages :
  226 !
  227 !     None.
  228 !
  229 !  7. Remarks :
  230 !
  231 !  8. Structure :
  232 !
  233 !     See source code.
  234 !
  235 !  9. Switches :
  236 !
  237 ! 10. Source code :
  238 !
  239 !/ ------------------------------------------------------------------- /
  240                                                                         
  241     USE W3ODATMD, ONLY: NDSE
  242     USE W3SERVMD, ONLY: EXTCDE
  243                               
  244     IMPLICIT NONE
  245                  
  246     INTEGER, INTENT(IN)                 :: NDSI
  247     TYPE(NML_FIELD_T), INTENT(INOUT)    :: NML_FIELD
  248                                                     
  249     ! locals
  250     INTEGER                                :: IERR
  251     TYPE(NML_FIELD_T) :: FIELD
  252     NAMELIST /FIELD_NML/ FIELD
  253                               
  254     IERR = 0
  255             
  256     ! set default values for field structure
  257     FIELD%TIMESTART  = '19000101 000000'
  258     FIELD%TIMESTRIDE = '0'
  259     FIELD%TIMECOUNT  = '1000000000'
  260     FIELD%TIMESPLIT  = 6
  261     FIELD%LIST       = 'unset'
  262     FIELD%PARTITION  = '0 1 2 3'
  263     FIELD%SAMEFILE   = .TRUE.
  264     FIELD%VECTOR     = .TRUE.
  265     FIELD%TYPE       = 3
  266                         
  267     ! read field namelist
  268     REWIND (NDSI)
  269     READ (NDSI, nml=FIELD_NML, iostat=IERR, iomsg=MSG)
  270     IF (IERR.NE.0) THEN
  271       WRITE (NDSE,'(A,/A)') &
  272         'ERROR: READ_FIELD_NML: namelist read error', &
  273         'ERROR: '//TRIM(MSG)
  274       CALL EXTCDE (1)
  275     END IF
 
 
                                                      w3nmlounfmd.F90  page   6
 
 
  276           
  277     ! save namelist
  278     NML_FIELD = FIELD
  279                      
  280   END SUBROUTINE READ_FIELD_NML
  281                                
  282 !/ ------------------------------------------------------------------- /
  283                                                                         
  284                                                                         
  285                                                                         
  286 !/ ------------------------------------------------------------------- /
  287                                                                         
  288   SUBROUTINE READ_FILE_NML (NDSI, NML_FILE)
  289 !/
  290 !/                  +-----------------------------------+
  291 !/                  | WAVEWATCH III           NOAA/NCEP |
  292 !/                  |           M. Accensi              |
  293 !/                  |                                   |
  294 !/                  |                        FORTRAN 90 |
  295 !/                  | Last update :         15-May-2018 |
  296 !/                  +-----------------------------------+
  297 !/
  298 !  1. Purpose :
  299 !
  300 !  2. Method :
  301 !
  302 !     See source term routines.
  303 !
  304 !  3. Parameters :
  305 !
  306 !     Parameter list
  307 !     ----------------------------------------------------------------
  308 !      NDSI         Int.
  309 !      NML_FILE     Type.
  310 !     ----------------------------------------------------------------
  311 !
  312 !  4. Subroutines used :
  313 !
  314 !      Name      TYPE  Module   Description
  315 !     ----------------------------------------------------------------
  316 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  317 !     ----------------------------------------------------------------
  318 !
  319 !  5. Called by :
  320 !
  321 !      Name      TYPE  Module   Description
  322 !     ----------------------------------------------------------------
  323 !      W3NMLOUNF Subr.   N/A    Namelist configuration routine.
  324 !     ----------------------------------------------------------------
  325 !
  326 !  6. Error messages :
  327 !
  328 !     None.
  329 !
  330 !  7. Remarks :
 
 
                                                      w3nmlounfmd.F90  page   7
 
 
  331 !
  332 !  8. Structure :
  333 !
  334 !     See source code.
  335 !
  336 !  9. Switches :
  337 !
  338 ! 10. Source code :
  339 !
  340 !/ ------------------------------------------------------------------- /
  341                                                                         
  342     USE W3ODATMD, ONLY: NDSE
  343     USE W3SERVMD, ONLY: EXTCDE
  344                               
  345     IMPLICIT NONE
  346                  
  347     INTEGER, INTENT(IN)                 :: NDSI
  348     TYPE(NML_FILE_T), INTENT(INOUT)     :: NML_FILE
  349                                                    
  350     ! locals
  351     INTEGER                                :: IERR
  352     TYPE(NML_FILE_T) :: FILE
  353     NAMELIST /FILE_NML/ FILE
  354                             
  355     IERR = 0
  356             
  357     ! set default values for file structure
  358     FILE%PREFIX    = 'ww3.'
  359     FILE%NETCDF    = 3
  360     FILE%IX0       = 1
  361     FILE%IXN       = 1000000000
  362     FILE%IY0       = 1
  363     FILE%IYN       = 1000000000
  364                                
  365     ! read file namelist
  366     REWIND (NDSI)
  367     READ (NDSI, nml=FILE_NML, iostat=IERR, iomsg=MSG)
  368     IF (IERR.GT.0) THEN
  369       WRITE (NDSE,'(A,/A)') &
  370         'ERROR: READ_FILE_NML: namelist read error', &
  371         'ERROR: '//TRIM(MSG)
  372       CALL EXTCDE (2)
  373     END IF
  374           
  375     ! save namelist
  376     NML_FILE = FILE
  377                    
  378   END SUBROUTINE READ_FILE_NML
  379                               
  380 !/ ------------------------------------------------------------------- /
  381                                                                         
  382                                                                         
  383                                                                         
  384 !/ ------------------------------------------------------------------- /
  385                                                                         
 
 
                                                      w3nmlounfmd.F90  page   8
 
 
  386   SUBROUTINE READ_SMC_NML (NDSI, NML_SMC)
  387 !/
  388 !/                  +-----------------------------------+
  389 !/                  | WAVEWATCH III           NOAA/NCEP |
  390 !/                  |           M. Accensi              |
  391 !/                  |                                   |
  392 !/                  |                        FORTRAN 90 |
  393 !/                  | Last update :         19-Sep-2018 |
  394 !/                  +-----------------------------------+
  395 !/
  396 !  1. Purpose :
  397 !
  398 !  2. Method :
  399 !
  400 !     See source term routines.
  401 !
  402 !  3. Parameters :
  403 !
  404 !     Parameter list
  405 !     ----------------------------------------------------------------
  406 !      NDSI         Int.
  407 !      NML_SMC      Type.
  408 !     ----------------------------------------------------------------
  409 !
  410 !  4. Subroutines used :
  411 !
  412 !      Name      TYPE  Module   Description
  413 !     ----------------------------------------------------------------
  414 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  415 !     ----------------------------------------------------------------
  416 !
  417 !  5. Called by :
  418 !
  419 !      Name      TYPE  Module   Description
  420 !     ----------------------------------------------------------------
  421 !      W3NMLOUNF Subr.   N/A    Namelist configuration routine.
  422 !     ----------------------------------------------------------------
  423 !
  424 !  6. Error messages :
  425 !
  426 !     None.
  427 !
  428 !  7. Remarks :
  429 !
  430 !  8. Structure :
  431 !
  432 !     See source code.
  433 !
  434 !  9. Switches :
  435 !
  436 ! 10. Source code :
  437 !
  438 !/ ------------------------------------------------------------------- /
  439                                                                         
  440     USE W3ODATMD, ONLY: NDSE
 
 
                                                      w3nmlounfmd.F90  page   9
 
 
  441     USE W3SERVMD, ONLY: EXTCDE
  442          USE CONSTANTS, ONLY: UNDEF
  443                                    
  444     IMPLICIT NONE
  445                  
  446     INTEGER, INTENT(IN)                 :: NDSI
  447     TYPE(NML_SMC_T), INTENT(INOUT)      :: NML_SMC
  448                                                   
  449     ! locals
  450     INTEGER                                :: IERR
  451     TYPE(NML_SMC_T) :: SMC
  452     NAMELIST /SMC_NML/ SMC
  453                           
  454     IERR = 0
  455             
  456     ! set default values for smc structure
  457     SMC%SXO       = -999.9
  458     SMC%SYO       = -999.9
  459     SMC%EXO       = -999.9
  460     SMC%EYO       = -999.9
  461     SMC%CELFAC    = 1
  462     SMC%TYPE      = 1
  463     SMC%NOVAL     = UNDEF
  464                          
  465     ! read smc namelist
  466     REWIND (NDSI)
  467     READ (NDSI, nml=SMC_NML, iostat=IERR, iomsg=MSG)
  468     IF (IERR.GT.0) THEN
  469       WRITE (NDSE,'(A,/A)') &
  470         'ERROR: READ_SMC_NML: namelist read error', &
  471         'ERROR: '//TRIM(MSG)
  472       CALL EXTCDE (3)
  473     END IF
  474           
  475     ! save namelist
  476     NML_SMC = SMC
  477                  
  478   END SUBROUTINE READ_SMC_NML
  479                              
  480 !/ ------------------------------------------------------------------- /
  481                                                                         
  482                                                                         
  483                                                                         
  484                                                                         
  485                                                                         
  486 !/ ------------------------------------------------------------------- /
  487                                                                         
  488   SUBROUTINE REPORT_FIELD_NML (NML_FIELD)
  489 !/
  490 !/                  +-----------------------------------+
  491 !/                  | WAVEWATCH III           NOAA/NCEP |
  492 !/                  |           M. Accensi              |
  493 !/                  |                        FORTRAN 90 |
  494 !/                  | Last update :         15-May-2018 |
  495 !/                  +-----------------------------------+
 
 
                                                      w3nmlounfmd.F90  page  10
 
 
  496 !/
  497 !/
  498 !  1. Purpose :
  499 !
  500 !  2. Method :
  501 !
  502 !     See source term routines.
  503 !
  504 !  3. Parameters :
  505 !
  506 !     Parameter list
  507 !     ----------------------------------------------------------------
  508 !      NML_FIELD  Type.
  509 !     ----------------------------------------------------------------
  510 !
  511 !  4. Subroutines used :
  512 !
  513 !      Name      TYPE  Module   Description
  514 !     ----------------------------------------------------------------
  515 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  516 !     ----------------------------------------------------------------
  517 !
  518 !  5. Called by :
  519 !
  520 !      Name      TYPE  Module   Description
  521 !     ----------------------------------------------------------------
  522 !      W3NMLOUNF Subr.   N/A    Namelist configuration routine.
  523 !     ----------------------------------------------------------------
  524 !
  525 !  6. Error messages :
  526 !
  527 !     None.
  528 !
  529 !  7. Remarks :
  530 !
  531 !  8. Structure :
  532 !
  533 !     See source code.
  534 !
  535 !  9. Switches :
  536 !
  537 ! 10. Source code :
  538 !
  539 !/ ------------------------------------------------------------------- /
  540                                                                         
  541                                                                         
  542     IMPLICIT NONE
  543                  
  544     TYPE(NML_FIELD_T), INTENT(IN) :: NML_FIELD
  545                                               
  546                                               
  547       WRITE (MSG,'(A)') 'FIELD % '
  548       WRITE (NDSN,'(A)')
  549       WRITE (NDSN,10) TRIM(MSG),'TIMESTART  = ', TRIM(NML_FIELD%TIMESTART)
  550       WRITE (NDSN,10) TRIM(MSG),'TIMESTRIDE = ', TRIM(NML_FIELD%TIMESTRID>
 
 
                                                      w3nmlounfmd.F90  page  11
 
 
  551       WRITE (NDSN,10) TRIM(MSG),'TIMECOUNT  = ', TRIM(NML_FIELD%TIMECOUNT)
  552                                                                           
  553       WRITE (NDSN,11) TRIM(MSG),'TIMESPLIT  = ', NML_FIELD%TIMESPLIT
  554       WRITE (NDSN,10) TRIM(MSG),'LIST       = ', TRIM(NML_FIELD%LIST)
  555       WRITE (NDSN,10) TRIM(MSG),'PARTITION  = ', TRIM(NML_FIELD%PARTITION)
  556       WRITE (NDSN,13) TRIM(MSG),'SAMEFILE   = ', NML_FIELD%SAMEFILE
  557       WRITE (NDSN,11) TRIM(MSG),'TYPE       = ', NML_FIELD%TYPE
  558                                                                
  559                                                                
  560 10  FORMAT (A,2X,A,A)
  561 11  FORMAT (A,2X,A,I8)
  562 13  FORMAT (A,2X,A,L1)
  563                       
  564   END SUBROUTINE REPORT_FIELD_NML
  565                                  
  566 !/ ------------------------------------------------------------------- /
  567                                                                         
  568                                                                         
  569                                                                         
  570                                                                         
  571                                                                         
  572                                                                         
  573 !/ ------------------------------------------------------------------- /
  574                                                                         
  575   SUBROUTINE REPORT_FILE_NML (NML_FILE)
  576 !/
  577 !/                  +-----------------------------------+
  578 !/                  | WAVEWATCH III           NOAA/NCEP |
  579 !/                  |           M. Accensi              |
  580 !/                  |                        FORTRAN 90 |
  581 !/                  | Last update :         15-May-2018 |
  582 !/                  +-----------------------------------+
  583 !/
  584 !/
  585 !  1. Purpose :
  586 !
  587 !  2. Method :
  588 !
  589 !     See source term routines.
  590 !
  591 !  3. Parameters :
  592 !
  593 !     Parameter list
  594 !     ----------------------------------------------------------------
  595 !      NML_FILE  Type.
  596 !     ----------------------------------------------------------------
  597 !
  598 !  4. Subroutines used :
  599 !
  600 !      Name      TYPE  Module   Description
  601 !     ----------------------------------------------------------------
  602 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  603 !     ----------------------------------------------------------------
  604 !
  605 !  5. Called by :
 
 
                                                      w3nmlounfmd.F90  page  12
 
 
  606 !
  607 !      Name      TYPE  Module   Description
  608 !     ----------------------------------------------------------------
  609 !      W3NMLOUNF Subr.   N/A    Namelist configuration routine.
  610 !     ----------------------------------------------------------------
  611 !
  612 !  6. Error messages :
  613 !
  614 !     None.
  615 !
  616 !  7. Remarks :
  617 !
  618 !  8. Structure :
  619 !
  620 !     See source code.
  621 !
  622 !  9. Switches :
  623 !
  624 ! 10. Source code :
  625 !
  626 !/ ------------------------------------------------------------------- /
  627                                                                         
  628                                                                         
  629     IMPLICIT NONE
  630                  
  631     TYPE(NML_FILE_T), INTENT(IN) :: NML_FILE
  632                                             
  633                                             
  634       WRITE (MSG,'(A)') 'FILE % '
  635       WRITE (NDSN,'(A)')
  636       WRITE (NDSN,10) TRIM(MSG),'PREFIX    = ', TRIM(NML_FILE%PREFIX)
  637       WRITE (NDSN,11) TRIM(MSG),'NETCDF    = ', NML_FILE%NETCDF
  638       WRITE (NDSN,11) TRIM(MSG),'IX0       = ', NML_FILE%IX0
  639       WRITE (NDSN,11) TRIM(MSG),'IXN       = ', NML_FILE%IXN
  640       WRITE (NDSN,11) TRIM(MSG),'IY0       = ', NML_FILE%IY0
  641       WRITE (NDSN,11) TRIM(MSG),'IYN       = ', NML_FILE%IYN
  642                                                             
  643 10  FORMAT (A,2X,A,A)
  644 11  FORMAT (A,2X,A,I12)
  645                        
  646   END SUBROUTINE REPORT_FILE_NML
  647                                 
  648 !/ ------------------------------------------------------------------- /
  649                                                                         
  650                                                                         
  651 !/ ------------------------------------------------------------------- /
  652                                                                         
  653   SUBROUTINE REPORT_SMC_NML (NML_SMC)
  654 !/
  655 !/                  +-----------------------------------+
  656 !/                  | WAVEWATCH III           NOAA/NCEP |
  657 !/                  |           M. Accensi              |
  658 !/                  |                        FORTRAN 90 |
  659 !/                  | Last update :         19-Sep-2018 |
  660 !/                  +-----------------------------------+
 
 
                                                      w3nmlounfmd.F90  page  13
 
 
  661 !/
  662 !/
  663 !  1. Purpose :
  664 !
  665 !  2. Method :
  666 !
  667 !     See source term routines.
  668 !
  669 !  3. Parameters :
  670 !
  671 !     Parameter list
  672 !     ----------------------------------------------------------------
  673 !      NML_SMC   Type.
  674 !     ----------------------------------------------------------------
  675 !
  676 !  4. Subroutines used :
  677 !
  678 !      Name      TYPE  Module   Description
  679 !     ----------------------------------------------------------------
  680 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  681 !     ----------------------------------------------------------------
  682 !
  683 !  5. Called by :
  684 !
  685 !      Name      TYPE  Module   Description
  686 !     ----------------------------------------------------------------
  687 !      W3NMLOUNF Subr.   N/A    Namelist configuration routine.
  688 !     ----------------------------------------------------------------
  689 !
  690 !  6. Error messages :
  691 !
  692 !     None.
  693 !
  694 !  7. Remarks :
  695 !
  696 !  8. Structure :
  697 !
  698 !     See source code.
  699 !
  700 !  9. Switches :
  701 !
  702 ! 10. Source code :
  703 !
  704 !/ ------------------------------------------------------------------- /
  705                                                                         
  706                                                                         
  707     IMPLICIT NONE
  708                  
  709     TYPE(NML_SMC_T), INTENT(IN) :: NML_SMC
  710                                           
  711                                           
  712       WRITE (MSG,'(A)') 'SMC % '
  713       WRITE (NDSN,'(A)')
  714       WRITE (NDSN,11) TRIM(MSG),'TYPE      = ', NML_SMC%TYPE
  715       WRITE (NDSN,14) TRIM(MSG),'SXO       = ', NML_SMC%SXO
 
 
                                                      w3nmlounfmd.F90  page  14
 
 
  716       WRITE (NDSN,14) TRIM(MSG),'SYO       = ', NML_SMC%SYO
  717       WRITE (NDSN,14) TRIM(MSG),'EXO       = ', NML_SMC%EXO
  718       WRITE (NDSN,14) TRIM(MSG),'EYO       = ', NML_SMC%EYO
  719       WRITE (NDSN,11) TRIM(MSG),'CELFAC    = ', NML_SMC%CELFAC
  720       WRITE (NDSN,14) TRIM(MSG),'NOVAL     = ', NML_SMC%NOVAL
  721                                                              
  722 11  FORMAT (A,2X,A,I12)
  723 14  FORMAT (A,2X,A,F8.2)
  724                         
  725   END SUBROUTINE REPORT_SMC_NML
  726                                
  727 !/ ------------------------------------------------------------------- /
  728                                                                         
  729                                                                         
  730                                                                         
  731                                                                         
  732 END MODULE W3NMLOUNFMD
  733                       
  734 !/ ------------------------------------------------------------------- /
  735                                                                         
  736                                                                         
  737                                                                         
  738                                                                         
  739                                                                         
  740                                                                         
  741                                                                         
  742                                                                         
  743                                                                         
  744                                                                         
  745                                                                         
  746                                                                         
  747                                                                         
  748                                                                         
  749                                                                         
  750                                                                         
  751                                                                         
  752                                                                         
  753                                                                         
  754                                                                         
  755                                                                         
  756                                                                         
  757                                                                         
  758                                                                         
  759                                                                         
  760                                                                         
  761                                                                         
  762                                                                         
  763                                                                         
  764                                                                         
  765                                                                         
  766                                                                         
  767                                                                         
  768                                                                         
  769                                                                         
  770                                                                         
 
 
                                                      w3nmlounfmd.F90  page  15
 
 
  771                                                                         
  772                                                                         
  773                                                                         
  774                                                                         
  775                                                                         
  776                                                                         
  777                                                                         
  778                                                                         
  779                                                                         
  780                                                                         
  781                                                                         
  782                                                                         
  783                                                                         
  784                                                                         
  785                                                                         
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/opt/cray/netcdf-hdf5parallel/4.2.0/pgi/119/include -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
