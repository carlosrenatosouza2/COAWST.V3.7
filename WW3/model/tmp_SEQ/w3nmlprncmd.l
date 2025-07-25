                                                      w3nmlprncmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3NMLPRNCMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           M. Accensi              |
    8 !/                  |                                   |
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :         04-Jan-2018 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    For updates see subroutines.
   14 !/
   15 !  1. Purpose :
   16 !
   17 !     Manages namelists from configuration file ww3_prnc.nml for ww3_prnc>
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
   28     LOGICAL                     :: ICE_PARAM1
   29     LOGICAL                     :: ICE_PARAM2
   30     LOGICAL                     :: ICE_PARAM3
   31     LOGICAL                     :: ICE_PARAM4
   32     LOGICAL                     :: ICE_PARAM5
   33     LOGICAL                     :: MUD_DENSITY
   34     LOGICAL                     :: MUD_THICKNESS
   35     LOGICAL                     :: MUD_VISCOSITY
   36     LOGICAL                     :: WATER_LEVELS
   37     LOGICAL                     :: CURRENTS
   38     LOGICAL                     :: WINDS
   39     LOGICAL                     :: WINDS_AST
   40     LOGICAL                     :: ICE_CONC
   41     LOGICAL                     :: ICE_BERG
   42     LOGICAL                     :: DATA_ASSIM
   43   END TYPE NML_FIELD_T
   44                       
   45   ! grid structure
   46   TYPE NML_GRID_T
   47     LOGICAL                     :: ASIS
   48     LOGICAL                     :: LATLON
   49   END TYPE NML_GRID_T
   50                      
   51   ! forcing structure
   52   TYPE NML_FORCING_T
   53     CHARACTER(15)               :: TIMESTART
   54     CHARACTER(15)               :: TIMESTOP
   55     TYPE(NML_FIELD_T)           :: FIELD
 
 
                                                      w3nmlprncmd.F90  page   2
 
 
   56     TYPE(NML_GRID_T)            :: GRID
   57     CHARACTER(256)              :: TIDAL
   58   END TYPE NML_FORCING_T
   59                         
   60   ! file structure
   61   TYPE NML_FILE_T
   62     CHARACTER(256)              :: FILENAME
   63     CHARACTER(100)              :: LONGITUDE
   64     CHARACTER(100)              :: LATITUDE
   65     CHARACTER(100)              :: VAR(3)
   66     CHARACTER(15)               :: TIMESHIFT
   67   END TYPE NML_FILE_T
   68                      
   69                      
   70   ! miscellaneous
   71   CHARACTER(256)                :: MSG
   72   INTEGER                       :: NDSN
   73                                        
   74                                        
   75                                        
   76                                        
   77   CONTAINS
   78 !/ ------------------------------------------------------------------- /
   79   SUBROUTINE W3NMLPRNC (NDSI, INFILE, NML_FORCING, NML_FILE, IERR)
   80 !/
   81 !/                  +-----------------------------------+
   82 !/                  | WAVEWATCH III           NOAA/NCEP |
   83 !/                  |           M. Accensi              |
   84 !/                  |                                   |
   85 !/                  |                        FORTRAN 90 |
   86 !/                  | Last update :         04-Jan-2018 |
   87 !/                  +-----------------------------------+
   88 !/
   89 !
   90 !  1. Purpose :
   91 !
   92 !     Reads all the namelist to define the forcing field
   93 !
   94 !  2. Method :
   95 !
   96 !     See source term routines.
   97 !
   98 !  3. Parameters :
   99 !
  100 !     Parameter list
  101 !     ----------------------------------------------------------------
  102 !      NDSI        Int.
  103 !      INFILE      Char.
  104 !      NML_FORCING type.
  105 !      NML_FILE    type.
  106 !      IERR        Int.
  107 !     ----------------------------------------------------------------
  108 !
  109 !  4. Subroutines used :
  110 !
 
 
                                                      w3nmlprncmd.F90  page   3
 
 
  111 !      Name      TYPE  Module   Description
  112 !     ----------------------------------------------------------------
  113 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  114 !      READ_FORCING_NML
  115 !     ----------------------------------------------------------------
  116 !
  117 !  5. Called by :
  118 !
  119 !      Name      TYPE  Module   Description
  120 !     ----------------------------------------------------------------
  121 !      WW3_PRNC  Prog.   N/A    Preprocess forcing fields.
  122 !     ----------------------------------------------------------------
  123 !
  124 !  6. Error messages :
  125 !
  126 !     None.
  127 !
  128 !  7. Remarks :
  129 !
  130 !  8. Structure :
  131 !
  132 !     See source code.
  133 !
  134 !  9. Switches :
  135 !
  136 ! 10. Source code :
  137 !
  138 !/ ------------------------------------------------------------------- /
  139                                                                         
  140     USE W3ODATMD, ONLY: NDSE
  141                             
  142     IMPLICIT NONE
  143                  
  144     INTEGER, INTENT(IN)                         :: NDSI
  145     CHARACTER*(*), INTENT(IN)                   :: INFILE
  146     TYPE(NML_FORCING_T), INTENT(INOUT)          :: NML_FORCING
  147     TYPE(NML_FILE_T), INTENT(INOUT)             :: NML_FILE
  148     INTEGER, INTENT(OUT)                        :: IERR
  149                                                        
  150     IERR = 0
  151             
  152     ! open namelist log file
  153     NDSN = 3
  154     OPEN (NDSN, file=TRIM(INFILE)//'.log', form='formatted', iostat=IERR)
  155        IF (IERR.NE.0) THEN
  156       WRITE (NDSE,'(A)') 'ERROR: open full nml file '//TRIM(INFILE)//'.lo>
  157       RETURN
  158     END IF
  159           
  160     ! open input file
  161     OPEN (NDSI, file=TRIM(INFILE), form='formatted', status='old', iostat>
  162     IF (IERR.NE.0) THEN
  163       WRITE (NDSE,'(A)') 'ERROR: open input file '//TRIM(INFILE)//' faile>
  164       RETURN
  165     END IF
 
 
                                                      w3nmlprncmd.F90  page   4
 
 
  166           
  167     ! read forcing namelist
  168     CALL READ_FORCING_NML (NDSI, NML_FORCING)
  169     CALL REPORT_FORCING_NML (NML_FORCING)
  170                                          
  171     ! read file namelist
  172     CALL READ_FILE_NML (NDSI, NML_FILE)
  173     CALL REPORT_FILE_NML (NML_FILE)
  174                                    
  175     ! close namelist files
  176     CLOSE (NDSI)
  177     CLOSE (NDSN)
  178                 
  179   END SUBROUTINE W3NMLPRNC
  180                           
  181                           
  182 !/ ------------------------------------------------------------------- /
  183                                                                         
  184                                                                         
  185                                                                         
  186                                                                         
  187                                                                         
  188                                                                         
  189 !/ ------------------------------------------------------------------- /
  190                                                                         
  191   SUBROUTINE READ_FORCING_NML (NDSI, NML_FORCING)
  192 !/
  193 !/                  +-----------------------------------+
  194 !/                  | WAVEWATCH III           NOAA/NCEP |
  195 !/                  |           M. Accensi              |
  196 !/                  |                                   |
  197 !/                  |                        FORTRAN 90 |
  198 !/                  | Last update :         04-Jan-2018 |
  199 !/                  +-----------------------------------+
  200 !/
  201 !  1. Purpose :
  202 !
  203 !  2. Method :
  204 !
  205 !     See source term routines.
  206 !
  207 !  3. Parameters :
  208 !
  209 !     Parameter list
  210 !     ----------------------------------------------------------------
  211 !      NDSI         Int.
  212 !      NML_FORCING  Type.
  213 !     ----------------------------------------------------------------
  214 !
  215 !  4. Subroutines used :
  216 !
  217 !      Name      TYPE  Module   Description
  218 !     ----------------------------------------------------------------
  219 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  220 !     ----------------------------------------------------------------
 
 
                                                      w3nmlprncmd.F90  page   5
 
 
  221 !
  222 !  5. Called by :
  223 !
  224 !      Name      TYPE  Module   Description
  225 !     ----------------------------------------------------------------
  226 !      W3NMLPRNC Subr.   N/A    Namelist configuration routine.
  227 !     ----------------------------------------------------------------
  228 !
  229 !  6. Error messages :
  230 !
  231 !     None.
  232 !
  233 !  7. Remarks :
  234 !
  235 !  8. Structure :
  236 !
  237 !     See source code.
  238 !
  239 !  9. Switches :
  240 !
  241 ! 10. Source code :
  242 !
  243 !/ ------------------------------------------------------------------- /
  244                                                                         
  245     USE W3ODATMD, ONLY: NDSE
  246     USE W3SERVMD, ONLY: EXTCDE
  247                               
  248     IMPLICIT NONE
  249                  
  250     INTEGER, INTENT(IN)                 :: NDSI
  251     TYPE(NML_FORCING_T), INTENT(INOUT)  :: NML_FORCING
  252                                                       
  253     ! locals
  254     INTEGER                                :: IERR
  255     TYPE(NML_FORCING_T) :: FORCING
  256     NAMELIST /FORCING_NML/ FORCING
  257                                   
  258     IERR = 0
  259             
  260     ! set default values for forcing structure
  261     FORCING%TIMESTART  = '19000101 000000'
  262     FORCING%TIMESTOP   = '29001231 000000'
  263 !
  264     FORCING%FIELD%ICE_PARAM1     = .FALSE.
  265     FORCING%FIELD%ICE_PARAM2     = .FALSE.
  266     FORCING%FIELD%ICE_PARAM3     = .FALSE.
  267     FORCING%FIELD%ICE_PARAM4     = .FALSE.
  268     FORCING%FIELD%ICE_PARAM5     = .FALSE.
  269     FORCING%FIELD%MUD_DENSITY    = .FALSE.
  270     FORCING%FIELD%MUD_THICKNESS  = .FALSE.
  271     FORCING%FIELD%MUD_VISCOSITY  = .FALSE.
  272     FORCING%FIELD%WATER_LEVELS   = .FALSE.
  273     FORCING%FIELD%CURRENTS       = .FALSE.
  274     FORCING%FIELD%WINDS          = .FALSE.
  275     FORCING%FIELD%WINDS_AST      = .FALSE.
 
 
                                                      w3nmlprncmd.F90  page   6
 
 
  276     FORCING%FIELD%ICE_CONC       = .FALSE.
  277     FORCING%FIELD%ICE_BERG       = .FALSE.
  278     FORCING%FIELD%DATA_ASSIM     = .FALSE.
  279 !
  280     FORCING%GRID%LATLON  = .FALSE.
  281     FORCING%GRID%ASIS    = .FALSE.
  282 !
  283     FORCING%TIDAL  = 'unset'
  284                             
  285                             
  286     ! read forcing namelist
  287     REWIND (NDSI)
  288     READ (NDSI, nml=FORCING_NML, iostat=IERR, iomsg=MSG)
  289     IF (IERR.NE.0) THEN
  290       WRITE (NDSE,'(A,/A)') &
  291         'ERROR: READ_FORCING_NML: namelist read error', &
  292         'ERROR: '//TRIM(MSG)
  293       CALL EXTCDE (1)
  294     END IF
  295           
  296     ! set/check RETURN values
  297     IF (FORCING%TIDAL.NE.'unset') THEN
  298       IF (.NOT. FORCING%FIELD%WATER_LEVELS .AND. .NOT. FORCING%FIELD%CURR>
  299         WRITE (NDSE,'(A,I3)') 'ERROR: TIDAL must only be used on FIELD%WA>
  300         CALL EXTCDE (2)
  301       ELSE IF (.NOT. FORCING%GRID%ASIS) THEN
  302         WRITE (NDSE,'(A,I3)') 'ERROR: TIDAL must only be used on GRID%ASI>
  303         CALL EXTCDE (3)
  304       END IF
  305     END IF
  306           
  307     ! save namelist
  308     NML_FORCING = FORCING
  309                          
  310   END SUBROUTINE READ_FORCING_NML
  311                                  
  312 !/ ------------------------------------------------------------------- /
  313                                                                         
  314                                                                         
  315                                                                         
  316 !/ ------------------------------------------------------------------- /
  317                                                                         
  318   SUBROUTINE READ_FILE_NML (NDSI, NML_FILE)
  319 !/
  320 !/                  +-----------------------------------+
  321 !/                  | WAVEWATCH III           NOAA/NCEP |
  322 !/                  |           M. Accensi              |
  323 !/                  |                                   |
  324 !/                  |                        FORTRAN 90 |
  325 !/                  | Last update :         04-Jan-2018 |
  326 !/                  +-----------------------------------+
  327 !/
  328 !  1. Purpose :
  329 !
  330 !  2. Method :
 
 
                                                      w3nmlprncmd.F90  page   7
 
 
  331 !
  332 !     See source term routines.
  333 !
  334 !  3. Parameters :
  335 !
  336 !     Parameter list
  337 !     ----------------------------------------------------------------
  338 !      NDSI         Int.
  339 !      NML_FILE     Type.
  340 !     ----------------------------------------------------------------
  341 !
  342 !  4. Subroutines used :
  343 !
  344 !      Name      TYPE  Module   Description
  345 !     ----------------------------------------------------------------
  346 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  347 !     ----------------------------------------------------------------
  348 !
  349 !  5. Called by :
  350 !
  351 !      Name      TYPE  Module   Description
  352 !     ----------------------------------------------------------------
  353 !      W3NMLPRNC Subr.   N/A    Namelist configuration routine.
  354 !     ----------------------------------------------------------------
  355 !
  356 !  6. Error messages :
  357 !
  358 !     None.
  359 !
  360 !  7. Remarks :
  361 !
  362 !  8. Structure :
  363 !
  364 !     See source code.
  365 !
  366 !  9. Switches :
  367 !
  368 ! 10. Source code :
  369 !
  370 !/ ------------------------------------------------------------------- /
  371                                                                         
  372     USE W3ODATMD, ONLY: NDSE
  373     USE W3SERVMD, ONLY: EXTCDE
  374                               
  375     IMPLICIT NONE
  376                  
  377     INTEGER, INTENT(IN)                 :: NDSI
  378     TYPE(NML_FILE_T), INTENT(INOUT)     :: NML_FILE
  379                                                    
  380     ! locals
  381     INTEGER                                :: IERR
  382     TYPE(NML_FILE_T) :: FILE
  383     NAMELIST /FILE_NML/ FILE
  384                             
  385     IERR = 0
 
 
                                                      w3nmlprncmd.F90  page   8
 
 
  386             
  387     ! set default values for file structure
  388     FILE%FILENAME  = 'unset'
  389     FILE%LONGITUDE = 'unset'
  390     FILE%LATITUDE  = 'unset'
  391     FILE%VAR(1)    = 'unset'
  392     FILE%VAR(2)    = 'unset'
  393     FILE%VAR(3)    = 'unset'
  394     FILE%TIMESHIFT = '00000000 000000'
  395                                       
  396     ! read file namelist
  397     REWIND (NDSI)
  398     READ (NDSI, nml=FILE_NML, iostat=IERR, iomsg=MSG)
  399     IF (IERR.NE.0) THEN
  400       WRITE (NDSE,'(A,/A)') &
  401         'ERROR: READ_FILE_NML: namelist read error', &
  402         'ERROR: '//TRIM(MSG)
  403       CALL EXTCDE (4)
  404     END IF
  405           
  406     ! save namelist
  407     NML_FILE = FILE
  408                    
  409   END SUBROUTINE READ_FILE_NML
  410                               
  411 !/ ------------------------------------------------------------------- /
  412                                                                         
  413                                                                         
  414                                                                         
  415                                                                         
  416                                                                         
  417                                                                         
  418                                                                         
  419 !/ ------------------------------------------------------------------- /
  420                                                                         
  421   SUBROUTINE REPORT_FORCING_NML (NML_FORCING)
  422 !/
  423 !/                  +-----------------------------------+
  424 !/                  | WAVEWATCH III           NOAA/NCEP |
  425 !/                  |           M. Accensi              |
  426 !/                  |                        FORTRAN 90 |
  427 !/                  | Last update :         04-Jan-2018 |
  428 !/                  +-----------------------------------+
  429 !/
  430 !/
  431 !  1. Purpose :
  432 !
  433 !  2. Method :
  434 !
  435 !     See source term routines.
  436 !
  437 !  3. Parameters :
  438 !
  439 !     Parameter list
  440 !     ----------------------------------------------------------------
 
 
                                                      w3nmlprncmd.F90  page   9
 
 
  441 !      NML_FORCING  Type.
  442 !     ----------------------------------------------------------------
  443 !
  444 !  4. Subroutines used :
  445 !
  446 !      Name      TYPE  Module   Description
  447 !     ----------------------------------------------------------------
  448 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  449 !     ----------------------------------------------------------------
  450 !
  451 !  5. Called by :
  452 !
  453 !      Name      TYPE  Module   Description
  454 !     ----------------------------------------------------------------
  455 !      W3NMLPRNC Subr.   N/A    Namelist configuration routine.
  456 !     ----------------------------------------------------------------
  457 !
  458 !  6. Error messages :
  459 !
  460 !     None.
  461 !
  462 !  7. Remarks :
  463 !
  464 !  8. Structure :
  465 !
  466 !     See source code.
  467 !
  468 !  9. Switches :
  469 !
  470 ! 10. Source code :
  471 !
  472 !/ ------------------------------------------------------------------- /
  473                                                                         
  474                                                                         
  475     IMPLICIT NONE
  476                  
  477     TYPE(NML_FORCING_T), INTENT(IN) :: NML_FORCING
  478                                                   
  479                                                   
  480       WRITE (MSG,'(A)') 'FORCING % '
  481       WRITE (NDSN,'(A)')
  482       WRITE (NDSN,10) TRIM(MSG),'TIMESTART  = ', TRIM(NML_FORCING%TIMESTA>
  483       WRITE (NDSN,10) TRIM(MSG),'TIMESTOP   = ', TRIM(NML_FORCING%TIMESTO>
  484                                                                           
  485       WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_PARAM1     = ', NML_FORCING%>
  486       WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_PARAM2     = ', NML_FORCING%>
  487       WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_PARAM3     = ', NML_FORCING%>
  488       WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_PARAM4     = ', NML_FORCING%>
  489       WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_PARAM5     = ', NML_FORCING%>
  490       WRITE (NDSN,13) TRIM(MSG),'FIELD % MUD_DENSITY    = ', NML_FORCING%>
  491       WRITE (NDSN,13) TRIM(MSG),'FIELD % MUD_THICKNESS  = ', NML_FORCING%>
  492       WRITE (NDSN,13) TRIM(MSG),'FIELD % MUD_VISCOSITY  = ', NML_FORCING%>
  493       WRITE (NDSN,13) TRIM(MSG),'FIELD % WATER_LEVELS   = ', NML_FORCING%>
  494       WRITE (NDSN,13) TRIM(MSG),'FIELD % CURRENTS       = ', NML_FORCING%>
  495       WRITE (NDSN,13) TRIM(MSG),'FIELD % WINDS          = ', NML_FORCING%>
 
 
                                                      w3nmlprncmd.F90  page  10
 
 
  496       WRITE (NDSN,13) TRIM(MSG),'FIELD % WINDS_AST      = ', NML_FORCING%>
  497       WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_CONC       = ', NML_FORCING%>
  498       WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_BERG       = ', NML_FORCING%>
  499       WRITE (NDSN,13) TRIM(MSG),'FIELD % DATA_ASSIM     = ', NML_FORCING%>
  500                                                                           
  501       WRITE (NDSN,13) TRIM(MSG),'GRID % ASIS   = ', NML_FORCING%GRID%ASIS
  502       WRITE (NDSN,13) TRIM(MSG),'GRID % LATLON = ', NML_FORCING%GRID%LATL>
  503                                                                           
  504       WRITE (NDSN,10) TRIM(MSG),'TIDAL = ', TRIM(NML_FORCING%TIDAL)
  505                                                                    
  506                                                                    
  507 10  FORMAT (A,2X,A,A)
  508 13  FORMAT (A,2X,A,L1)
  509                       
  510   END SUBROUTINE REPORT_FORCING_NML
  511                                    
  512 !/ ------------------------------------------------------------------- /
  513                                                                         
  514                                                                         
  515                                                                         
  516                                                                         
  517                                                                         
  518                                                                         
  519 !/ ------------------------------------------------------------------- /
  520                                                                         
  521   SUBROUTINE REPORT_FILE_NML (NML_FILE)
  522 !/
  523 !/                  +-----------------------------------+
  524 !/                  | WAVEWATCH III           NOAA/NCEP |
  525 !/                  |           M. Accensi              |
  526 !/                  |                        FORTRAN 90 |
  527 !/                  | Last update :         04-Jan-2018 |
  528 !/                  +-----------------------------------+
  529 !/
  530 !/
  531 !  1. Purpose :
  532 !
  533 !  2. Method :
  534 !
  535 !     See source term routines.
  536 !
  537 !  3. Parameters :
  538 !
  539 !     Parameter list
  540 !     ----------------------------------------------------------------
  541 !      NML_FILE  Type.
  542 !     ----------------------------------------------------------------
  543 !
  544 !  4. Subroutines used :
  545 !
  546 !      Name      TYPE  Module   Description
  547 !     ----------------------------------------------------------------
  548 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  549 !     ----------------------------------------------------------------
  550 !
 
 
                                                      w3nmlprncmd.F90  page  11
 
 
  551 !  5. Called by :
  552 !
  553 !      Name      TYPE  Module   Description
  554 !     ----------------------------------------------------------------
  555 !      W3NMLPRNC Subr.   N/A    Namelist configuration routine.
  556 !     ----------------------------------------------------------------
  557 !
  558 !  6. Error messages :
  559 !
  560 !     None.
  561 !
  562 !  7. Remarks :
  563 !
  564 !  8. Structure :
  565 !
  566 !     See source code.
  567 !
  568 !  9. Switches :
  569 !
  570 ! 10. Source code :
  571 !
  572 !/ ------------------------------------------------------------------- /
  573                                                                         
  574                                                                         
  575     IMPLICIT NONE
  576                  
  577     TYPE(NML_FILE_T), INTENT(IN) :: NML_FILE
  578                                             
  579                                             
  580       WRITE (MSG,'(A)') 'FILE % '
  581       WRITE (NDSN,'(A)')
  582       WRITE (NDSN,10) TRIM(MSG),'FILENAME    = ', TRIM(NML_FILE%FILENAME)
  583       WRITE (NDSN,10) TRIM(MSG),'LONGITUDE   = ', TRIM(NML_FILE%LONGITUDE)
  584       WRITE (NDSN,10) TRIM(MSG),'LATITUDE    = ', TRIM(NML_FILE%LATITUDE)
  585       WRITE (NDSN,10) TRIM(MSG),'VAR(1)      = ', TRIM(NML_FILE%VAR(1))
  586       WRITE (NDSN,10) TRIM(MSG),'VAR(2)      = ', TRIM(NML_FILE%VAR(2))
  587       WRITE (NDSN,10) TRIM(MSG),'VAR(3)      = ', TRIM(NML_FILE%VAR(3))
  588       WRITE (NDSN,10) TRIM(MSG),'TIMESHIFT   = ', TRIM(NML_FILE%TIMESHIFT)
  589                                                                           
  590 10  FORMAT (A,2X,A,A)
  591                      
  592                      
  593   END SUBROUTINE REPORT_FILE_NML
  594                                 
  595 !/ ------------------------------------------------------------------- /
  596                                                                         
  597                                                                         
  598                                                                         
  599                                                                         
  600                                                                         
  601 END MODULE W3NMLPRNCMD
  602                       
  603 !/ ------------------------------------------------------------------- /
  604                                                                         
  605                                                                         
 
 
                                                      w3nmlprncmd.F90  page  12
 
 
  606                                                                         
  607                                                                         
  608                                                                         
  609                                                                         
  610                                                                         
  611                                                                         
  612                                                                         
  613                                                                         
  614                                                                         
  615                                                                         
  616                                                                         
  617                                                                         
  618                                                                         
  619                                                                         
  620                                                                         
  621                                                                         
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/opt/cray/netcdf-hdf5parallel/4.2.0/pgi/119/include -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
