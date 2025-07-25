                                                      w3nmltrncmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3NMLTRNCMD
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
   17 !     Manages namelists from configuration file ww3_trnc.nml for ww3_trnc>
   18 !
   19 !/ ------------------------------------------------------------------- /
   20                                                                         
   21   ! module defaults
   22   IMPLICIT NONE
   23                
   24   PUBLIC
   25         
   26   ! track structure
   27   TYPE NML_TRACK_T
   28     CHARACTER(15)               :: TIMESTART
   29     CHARACTER(15)               :: TIMESTRIDE
   30     CHARACTER(15)               :: TIMECOUNT
   31     INTEGER                     :: TIMESPLIT
   32   END TYPE NML_TRACK_T
   33                       
   34   ! file structure
   35   TYPE NML_FILE_T
   36     CHARACTER(30)               :: PREFIX
   37     INTEGER                     :: NETCDF
   38   END TYPE NML_FILE_T
   39                      
   40   ! miscellaneous
   41   CHARACTER(256)                :: MSG
   42   INTEGER                       :: NDSN
   43                                        
   44                                        
   45                                        
   46                                        
   47   CONTAINS
   48 !/ ------------------------------------------------------------------- /
   49   SUBROUTINE W3NMLTRNC (NDSI, INFILE, NML_TRACK, NML_FILE, IERR)
   50 !/
   51 !/                  +-----------------------------------+
   52 !/                  | WAVEWATCH III           NOAA/NCEP |
   53 !/                  |           M. Accensi              |
   54 !/                  |                                   |
   55 !/                  |                        FORTRAN 90 |
 
 
                                                      w3nmltrncmd.F90  page   2
 
 
   56 !/                  | Last update :         15-May-2018 |
   57 !/                  +-----------------------------------+
   58 !/
   59 !
   60 !  1. Purpose :
   61 !
   62 !     Reads all the namelist to define the output track
   63 !
   64 !  2. Method :
   65 !
   66 !     See source term routines.
   67 !
   68 !  3. Parameters :
   69 !
   70 !     Parameter list
   71 !     ----------------------------------------------------------------
   72 !      NDSI        Int.
   73 !      INFILE      Char.
   74 !      NML_TRACK   type.
   75 !      NML_FILE    type.
   76 !      IERR        Int.
   77 !     ----------------------------------------------------------------
   78 !
   79 !  4. Subroutines used :
   80 !
   81 !      Name      TYPE  Module   Description
   82 !     ----------------------------------------------------------------
   83 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
   84 !      READ_TRACK_NML
   85 !     ----------------------------------------------------------------
   86 !
   87 !  5. Called by :
   88 !
   89 !      Name      TYPE  Module   Description
   90 !     ----------------------------------------------------------------
   91 !      WW3_TRNC  Prog.   N/A    Postprocess output tracks.
   92 !     ----------------------------------------------------------------
   93 !
   94 !  6. Error messages :
   95 !
   96 !     None.
   97 !
   98 !  7. Remarks :
   99 !
  100 !  8. Structure :
  101 !
  102 !     See source code.
  103 !
  104 !  9. Switches :
  105 !
  106 ! 10. Source code :
  107 !
  108 !/ ------------------------------------------------------------------- /
  109                                                                         
  110     USE W3ODATMD, ONLY: NDSE
 
 
                                                      w3nmltrncmd.F90  page   3
 
 
  111                             
  112     IMPLICIT NONE
  113                  
  114     INTEGER, INTENT(IN)                         :: NDSI
  115     CHARACTER*(*), INTENT(IN)                   :: INFILE
  116     TYPE(NML_TRACK_T), INTENT(INOUT)            :: NML_TRACK
  117     TYPE(NML_FILE_T), INTENT(INOUT)             :: NML_FILE
  118     INTEGER, INTENT(OUT)                        :: IERR
  119                                                        
  120     IERR = 0
  121             
  122     ! open namelist log file
  123     NDSN = 3
  124     OPEN (NDSN, file=TRIM(INFILE)//'.log', form='formatted', iostat=IERR)
  125        IF (IERR.NE.0) THEN
  126       WRITE (NDSE,'(A)') 'ERROR: open full nml file '//TRIM(INFILE)//'.lo>
  127       RETURN
  128     END IF
  129           
  130     ! open input file
  131     OPEN (NDSI, file=TRIM(INFILE), form='formatted', status='old', iostat>
  132     IF (IERR.NE.0) THEN
  133       WRITE (NDSE,'(A)') 'ERROR: open input file '//TRIM(INFILE)//' faile>
  134       RETURN
  135     END IF
  136           
  137     ! read track namelist
  138     CALL READ_TRACK_NML (NDSI, NML_TRACK)
  139     CALL REPORT_TRACK_NML (NML_TRACK)
  140                                      
  141     ! read file namelist
  142     CALL READ_FILE_NML (NDSI, NML_FILE)
  143     CALL REPORT_FILE_NML (NML_FILE)
  144                                    
  145     ! close namelist files
  146     CLOSE (NDSI)
  147     CLOSE (NDSN)
  148                 
  149   END SUBROUTINE W3NMLTRNC
  150                           
  151                           
  152 !/ ------------------------------------------------------------------- /
  153                                                                         
  154                                                                         
  155                                                                         
  156                                                                         
  157                                                                         
  158                                                                         
  159 !/ ------------------------------------------------------------------- /
  160                                                                         
  161   SUBROUTINE READ_TRACK_NML (NDSI, NML_TRACK)
  162 !/
  163 !/                  +-----------------------------------+
  164 !/                  | WAVEWATCH III           NOAA/NCEP |
  165 !/                  |           M. Accensi              |
 
 
                                                      w3nmltrncmd.F90  page   4
 
 
  166 !/                  |                                   |
  167 !/                  |                        FORTRAN 90 |
  168 !/                  | Last update :         15-May-2018 |
  169 !/                  +-----------------------------------+
  170 !/
  171 !  1. Purpose :
  172 !
  173 !  2. Method :
  174 !
  175 !     See source term routines.
  176 !
  177 !  3. Parameters :
  178 !
  179 !     Parameter list
  180 !     ----------------------------------------------------------------
  181 !      NDSI         Int.
  182 !      NML_TRACK    Type.
  183 !     ----------------------------------------------------------------
  184 !
  185 !  4. Subroutines used :
  186 !
  187 !      Name      TYPE  Module   Description
  188 !     ----------------------------------------------------------------
  189 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  190 !     ----------------------------------------------------------------
  191 !
  192 !  5. Called by :
  193 !
  194 !      Name      TYPE  Module   Description
  195 !     ----------------------------------------------------------------
  196 !      W3NMLTRNC Subr.   N/A    Namelist configuration routine.
  197 !     ----------------------------------------------------------------
  198 !
  199 !  6. Error messages :
  200 !
  201 !     None.
  202 !
  203 !  7. Remarks :
  204 !
  205 !  8. Structure :
  206 !
  207 !     See source code.
  208 !
  209 !  9. Switches :
  210 !
  211 ! 10. Source code :
  212 !
  213 !/ ------------------------------------------------------------------- /
  214                                                                         
  215     USE W3ODATMD, ONLY: NDSE
  216     USE W3SERVMD, ONLY: EXTCDE
  217                               
  218     IMPLICIT NONE
  219                  
  220     INTEGER, INTENT(IN)                 :: NDSI
 
 
                                                      w3nmltrncmd.F90  page   5
 
 
  221     TYPE(NML_TRACK_T), INTENT(INOUT)    :: NML_TRACK
  222     INTEGER                             :: IERR
  223                                                
  224     ! locals
  225     TYPE(NML_TRACK_T) :: TRACK
  226     NAMELIST /TRACK_NML/ TRACK
  227                               
  228     IERR = 0
  229             
  230     ! set default values for track structure
  231     TRACK%TIMESTART  = '19000101 000000'
  232     TRACK%TIMESTRIDE = '0'
  233     TRACK%TIMECOUNT  = '1000000000'
  234     TRACK%TIMESPLIT  = 6
  235                         
  236     ! read track namelist
  237     REWIND (NDSI)
  238     READ (NDSI, nml=TRACK_NML, iostat=IERR, iomsg=MSG)
  239     IF (IERR.NE.0) THEN
  240       WRITE (NDSE,'(A,/A)') &
  241         'ERROR: READ_TRACK_NML: namelist read error', &
  242         'ERROR: '//TRIM(MSG)
  243       CALL EXTCDE (1)
  244     END IF
  245           
  246     ! save namelist
  247     NML_TRACK = TRACK
  248                      
  249   END SUBROUTINE READ_TRACK_NML
  250                                
  251 !/ ------------------------------------------------------------------- /
  252                                                                         
  253                                                                         
  254                                                                         
  255 !/ ------------------------------------------------------------------- /
  256                                                                         
  257   SUBROUTINE READ_FILE_NML (NDSI, NML_FILE)
  258 !/
  259 !/                  +-----------------------------------+
  260 !/                  | WAVEWATCH III           NOAA/NCEP |
  261 !/                  |           M. Accensi              |
  262 !/                  |                                   |
  263 !/                  |                        FORTRAN 90 |
  264 !/                  | Last update :         15-May-2018 |
  265 !/                  +-----------------------------------+
  266 !/
  267 !  1. Purpose :
  268 !
  269 !  2. Method :
  270 !
  271 !     See source term routines.
  272 !
  273 !  3. Parameters :
  274 !
  275 !     Parameter list
 
 
                                                      w3nmltrncmd.F90  page   6
 
 
  276 !     ----------------------------------------------------------------
  277 !      NDSI         Int.
  278 !      NML_FILE     Type.
  279 !     ----------------------------------------------------------------
  280 !
  281 !  4. Subroutines used :
  282 !
  283 !      Name      TYPE  Module   Description
  284 !     ----------------------------------------------------------------
  285 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  286 !     ----------------------------------------------------------------
  287 !
  288 !  5. Called by :
  289 !
  290 !      Name      TYPE  Module   Description
  291 !     ----------------------------------------------------------------
  292 !      W3NMLTRNC Subr.   N/A    Namelist configuration routine.
  293 !     ----------------------------------------------------------------
  294 !
  295 !  6. Error messages :
  296 !
  297 !     None.
  298 !
  299 !  7. Remarks :
  300 !
  301 !  8. Structure :
  302 !
  303 !     See source code.
  304 !
  305 !  9. Switches :
  306 !
  307 ! 10. Source code :
  308 !
  309 !/ ------------------------------------------------------------------- /
  310                                                                         
  311     USE W3ODATMD, ONLY: NDSE
  312     USE W3SERVMD, ONLY: EXTCDE
  313                               
  314     IMPLICIT NONE
  315                  
  316     INTEGER, INTENT(IN)                 :: NDSI
  317     TYPE(NML_FILE_T), INTENT(INOUT)     :: NML_FILE
  318     INTEGER                             :: IERR
  319                                                
  320     ! locals
  321     TYPE(NML_FILE_T) :: FILE
  322     NAMELIST /FILE_NML/ FILE
  323                             
  324     IERR = 0
  325             
  326     ! set default values for file structure
  327     FILE%PREFIX    = 'ww3.'
  328     FILE%NETCDF    = 3
  329                       
  330     ! read file namelist
 
 
                                                      w3nmltrncmd.F90  page   7
 
 
  331     REWIND (NDSI)
  332     READ (NDSI, nml=FILE_NML, iostat=IERR, iomsg=MSG)
  333     IF (IERR.GT.0) THEN
  334       WRITE (NDSE,'(A,/A)') &
  335         'ERROR: READ_FILE_NML: namelist read error', &
  336         'ERROR: '//TRIM(MSG)
  337       CALL EXTCDE (2)
  338     END IF
  339           
  340     ! save namelist
  341     NML_FILE = FILE
  342                    
  343   END SUBROUTINE READ_FILE_NML
  344                               
  345 !/ ------------------------------------------------------------------- /
  346                                                                         
  347                                                                         
  348                                                                         
  349                                                                         
  350                                                                         
  351                                                                         
  352                                                                         
  353 !/ ------------------------------------------------------------------- /
  354                                                                         
  355   SUBROUTINE REPORT_TRACK_NML (NML_TRACK)
  356 !/
  357 !/                  +-----------------------------------+
  358 !/                  | WAVEWATCH III           NOAA/NCEP |
  359 !/                  |           M. Accensi              |
  360 !/                  |                        FORTRAN 90 |
  361 !/                  | Last update :         15-May-2018 |
  362 !/                  +-----------------------------------+
  363 !/
  364 !/
  365 !  1. Purpose :
  366 !
  367 !  2. Method :
  368 !
  369 !     See source term routines.
  370 !
  371 !  3. Parameters :
  372 !
  373 !     Parameter list
  374 !     ----------------------------------------------------------------
  375 !      NML_TRACK  Type.
  376 !     ----------------------------------------------------------------
  377 !
  378 !  4. Subroutines used :
  379 !
  380 !      Name      TYPE  Module   Description
  381 !     ----------------------------------------------------------------
  382 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  383 !     ----------------------------------------------------------------
  384 !
  385 !  5. Called by :
 
 
                                                      w3nmltrncmd.F90  page   8
 
 
  386 !
  387 !      Name      TYPE  Module   Description
  388 !     ----------------------------------------------------------------
  389 !      W3NMLTRNC Subr.   N/A    Namelist configuration routine.
  390 !     ----------------------------------------------------------------
  391 !
  392 !  6. Error messages :
  393 !
  394 !     None.
  395 !
  396 !  7. Remarks :
  397 !
  398 !  8. Structure :
  399 !
  400 !     See source code.
  401 !
  402 !  9. Switches :
  403 !
  404 ! 10. Source code :
  405 !
  406 !/ ------------------------------------------------------------------- /
  407                                                                         
  408                                                                         
  409     IMPLICIT NONE
  410                  
  411     TYPE(NML_TRACK_T), INTENT(IN) :: NML_TRACK
  412                                               
  413                                               
  414       WRITE (MSG,'(A)') 'TRACK % '
  415       WRITE (NDSN,'(A)')
  416       WRITE (NDSN,10) TRIM(MSG),'TIMESTART  = ', TRIM(NML_TRACK%TIMESTART)
  417       WRITE (NDSN,10) TRIM(MSG),'TIMESTRIDE = ', TRIM(NML_TRACK%TIMESTRID>
  418       WRITE (NDSN,10) TRIM(MSG),'TIMECOUNT  = ', TRIM(NML_TRACK%TIMECOUNT)
  419                                                                           
  420       WRITE (NDSN,11) TRIM(MSG),'TIMESPLIT  = ', NML_TRACK%TIMESPLIT
  421                                                                     
  422                                                                     
  423 10  FORMAT (A,2X,A,A)
  424 11  FORMAT (A,2X,A,I8)
  425                       
  426   END SUBROUTINE REPORT_TRACK_NML
  427                                  
  428 !/ ------------------------------------------------------------------- /
  429                                                                         
  430                                                                         
  431                                                                         
  432                                                                         
  433                                                                         
  434                                                                         
  435 !/ ------------------------------------------------------------------- /
  436                                                                         
  437   SUBROUTINE REPORT_FILE_NML (NML_FILE)
  438 !/
  439 !/                  +-----------------------------------+
  440 !/                  | WAVEWATCH III           NOAA/NCEP |
 
 
                                                      w3nmltrncmd.F90  page   9
 
 
  441 !/                  |           M. Accensi              |
  442 !/                  |                        FORTRAN 90 |
  443 !/                  | Last update :         15-May-2018 |
  444 !/                  +-----------------------------------+
  445 !/
  446 !/
  447 !  1. Purpose :
  448 !
  449 !  2. Method :
  450 !
  451 !     See source term routines.
  452 !
  453 !  3. Parameters :
  454 !
  455 !     Parameter list
  456 !     ----------------------------------------------------------------
  457 !      NML_FILE  Type.
  458 !     ----------------------------------------------------------------
  459 !
  460 !  4. Subroutines used :
  461 !
  462 !      Name      TYPE  Module   Description
  463 !     ----------------------------------------------------------------
  464 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  465 !     ----------------------------------------------------------------
  466 !
  467 !  5. Called by :
  468 !
  469 !      Name      TYPE  Module   Description
  470 !     ----------------------------------------------------------------
  471 !      W3NMLTRNC Subr.   N/A    Namelist configuration routine.
  472 !     ----------------------------------------------------------------
  473 !
  474 !  6. Error messages :
  475 !
  476 !     None.
  477 !
  478 !  7. Remarks :
  479 !
  480 !  8. Structure :
  481 !
  482 !     See source code.
  483 !
  484 !  9. Switches :
  485 !
  486 ! 10. Source code :
  487 !
  488 !/ ------------------------------------------------------------------- /
  489                                                                         
  490                                                                         
  491     IMPLICIT NONE
  492                  
  493     TYPE(NML_FILE_T), INTENT(IN) :: NML_FILE
  494                                             
  495                                             
 
 
                                                      w3nmltrncmd.F90  page  10
 
 
  496       WRITE (MSG,'(A)') 'FILE % '
  497       WRITE (NDSN,'(A)')
  498       WRITE (NDSN,10) TRIM(MSG),'PREFIX    = ', TRIM(NML_FILE%PREFIX)
  499       WRITE (NDSN,11) TRIM(MSG),'NETCDF    = ', NML_FILE%NETCDF
  500                                                                
  501                                                                
  502 10  FORMAT (A,2X,A,A)
  503 11  FORMAT (A,2X,A,I8)
  504                       
  505   END SUBROUTINE REPORT_FILE_NML
  506                                 
  507 !/ ------------------------------------------------------------------- /
  508                                                                         
  509                                                                         
  510                                                                         
  511                                                                         
  512                                                                         
  513 END MODULE W3NMLTRNCMD
  514                       
  515 !/ ------------------------------------------------------------------- /
  516                                                                         
  517                                                                         
  518                                                                         
  519                                                                         
  520                                                                         
  521                                                                         
  522                                                                         
  523                                                                         
  524                                                                         
  525                                                                         
  526                                                                         
  527                                                                         
  528                                                                         
  529                                                                         
  530                                                                         
  531                                                                         
  532                                                                         
  533                                                                         
  534                                                                         
  535                                                                         
  536                                                                         
  537                                                                         
  538                                                                         
  539                                                                         
  540                                                                         
  541                                                                         
  542                                                                         
  543                                                                         
  544                                                                         
  545                                                                         
  546                                                                         
  547                                                                         
  548                                                                         
  549                                                                         
  550                                                                         
 
 
                                                      w3nmltrncmd.F90  page  11
 
 
  551                                                                         
  552                                                                         
  553                                                                         
  554                                                                         
  555                                                                         
  556                                                                         
  557                                                                         
  558                                                                         
  559                                                                         
  560                                                                         
  561                                                                         
  562                                                                         
  563                                                                         
  564                                                                         
  565                                                                         
  566                                                                         
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/opt/cray/netcdf-hdf5parallel/4.2.0/pgi/119/include -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
