                                                    w3nmluprstrmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3NMLUPRSTRMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           M. Accensi              |
    8 !/                  |                                   |
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :         06-Oct-2020 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    For updates see subroutines.
   14 !/
   15 !  1. Purpose :
   16 !
   17 !     Manages namelists from configuration file ww3_uprstr.nml for ww3_up>
   18 !
   19 !/ ------------------------------------------------------------------- /
   20                                                                         
   21   ! module defaults
   22   IMPLICIT NONE
   23                
   24   PUBLIC
   25         
   26   ! restart time
   27   TYPE NML_RESTART_T
   28     CHARACTER(15)               :: RESTARTTIME
   29   END TYPE NML_RESTART_T
   30                         
   31   ! update approach
   32   TYPE NML_UPDATE_T
   33     CHARACTER(5)                :: UPDPROC
   34     REAL                        :: PRCNTG
   35     REAL                        :: PRCNTGCAP
   36     REAL                        :: THRWSEA
   37     CHARACTER(30)               :: FILE
   38   END TYPE NML_UPDATE_T
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
   49   SUBROUTINE W3NMLUPRSTR (NDSI, INFILE, NML_RESTART, NML_UPDATE, IERR)
   50 !/
   51 !/                  +-----------------------------------+
   52 !/                  | WAVEWATCH III           NOAA/NCEP |
   53 !/                  |           M. Accensi              |
   54 !/                  |                                   |
   55 !/                  |                        FORTRAN 90 |
 
 
                                                    w3nmluprstrmd.F90  page   2
 
 
   56 !/                  | Last update :         06-Oct-2020 |
   57 !/                  +-----------------------------------+
   58 !/
   59 !
   60 !  1. Purpose :
   61 !
   62 !     Reads all the namelist to define the output field
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
   74 !      NML_RESTART type.
   75 !      NML_UPDATE  type.
   76 !      IERR        Int.
   77 !     ----------------------------------------------------------------
   78 !
   79 !  4. Subroutines used :
   80 !
   81 !      Name      TYPE  Module   Description
   82 !     ----------------------------------------------------------------
   83 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
   84 !      READ_RESTART_NML
   85 !      READ_UPDATE_NML
   86 !     ----------------------------------------------------------------
   87 !
   88 !  5. Called by :
   89 !
   90 !      Name       TYPE   Module   Description
   91 !     ----------------------------------------------------------------
   92 !      WW3_UPRSTR Prog.  N/A      Update restart file
   93 !     ----------------------------------------------------------------
   94 !
   95 !  6. Error messages :
   96 !
   97 !     None.
   98 !
   99 !  7. Remarks :
  100 !
  101 !  8. Structure :
  102 !
  103 !     See source code.
  104 !
  105 !  9. Switches :
  106 !
  107 ! 10. Source code :
  108 !
  109 !/ ------------------------------------------------------------------- /
  110                                                                         
 
 
                                                    w3nmluprstrmd.F90  page   3
 
 
  111     USE W3ODATMD, ONLY: NDSE
  112                             
  113     IMPLICIT NONE
  114                  
  115     INTEGER, INTENT(IN)                         :: NDSI
  116     CHARACTER*(*), INTENT(IN)                   :: INFILE
  117     TYPE(NML_RESTART_T), INTENT(INOUT)          :: NML_RESTART
  118     TYPE(NML_UPDATE_T), INTENT(INOUT)           :: NML_UPDATE
  119     INTEGER, INTENT(OUT)                        :: IERR
  120                                                        
  121     IERR = 0
  122             
  123     ! open namelist log file
  124     NDSN = 3
  125     OPEN (NDSN, file=TRIM(INFILE)//'.log', form='formatted', iostat=IERR)
  126     IF (IERR.NE.0) THEN
  127       WRITE (NDSE,'(A)') 'ERROR: open full nml file '//TRIM(INFILE)//'.lo>
  128       RETURN
  129     END IF
  130           
  131     ! open input file
  132     OPEN (NDSI, file=TRIM(INFILE), form='formatted', status='old', iostat>
  133     IF (IERR.NE.0) THEN
  134       WRITE (NDSE,'(A)') 'ERROR: open input file '//TRIM(INFILE)//' faile>
  135       RETURN
  136     END IF
  137           
  138     ! read restart time namelist
  139     CALL READ_RESTART_NML (NDSI, NML_RESTART)
  140     CALL REPORT_RESTART_NML (NML_RESTART)
  141                                          
  142     ! read update approach namelist
  143     CALL READ_UPDATE_NML (NDSI, NML_UPDATE)
  144     CALL REPORT_UPDATE_NML (NML_UPDATE)
  145                                        
  146     ! close namelist files
  147     CLOSE (NDSI)
  148     CLOSE (NDSN)
  149                 
  150   END SUBROUTINE W3NMLUPRSTR
  151                             
  152                             
  153 !/ ------------------------------------------------------------------- /
  154                                                                         
  155                                                                         
  156                                                                         
  157 !/ ------------------------------------------------------------------- /
  158                                                                         
  159   SUBROUTINE READ_RESTART_NML (NDSI, NML_RESTART)
  160 !/
  161 !/                  +-----------------------------------+
  162 !/                  | WAVEWATCH III           NOAA/NCEP |
  163 !/                  |           M. Accensi              |
  164 !/                  |                                   |
  165 !/                  |                        FORTRAN 90 |
 
 
                                                    w3nmluprstrmd.F90  page   4
 
 
  166 !/                  | Last update :         06-Oct-2020 |
  167 !/                  +-----------------------------------+
  168 !/
  169 !  1. Purpose :
  170 !
  171 !  2. Method :
  172 !
  173 !     See source term routines.
  174 !
  175 !  3. Parameters :
  176 !
  177 !     Parameter list
  178 !     ----------------------------------------------------------------
  179 !      NDSI         Int.
  180 !      NML_RESTART  Type.
  181 !     ----------------------------------------------------------------
  182 !
  183 !  4. Subroutines used :
  184 !
  185 !      Name      TYPE  Module   Description
  186 !     ----------------------------------------------------------------
  187 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  188 !     ----------------------------------------------------------------
  189 !
  190 !  5. Called by :
  191 !
  192 !      Name        TYPE   Module  Description
  193 !     ----------------------------------------------------------------
  194 !      W3NMLUPRSTR Subr.   N/A    Namelist configuration routine.
  195 !     ----------------------------------------------------------------
  196 !
  197 !  6. Error messages :
  198 !
  199 !     None.
  200 !
  201 !  7. Remarks :
  202 !
  203 !  8. Structure :
  204 !
  205 !     See source code.
  206 !
  207 !  9. Switches :
  208 !
  209 ! 10. Source code :
  210 !
  211 !/ ------------------------------------------------------------------- /
  212                                                                         
  213     USE W3ODATMD, ONLY: NDSE
  214     USE W3SERVMD, ONLY: EXTCDE
  215                               
  216     IMPLICIT NONE
  217                  
  218     INTEGER, INTENT(IN)                 :: NDSI
  219     TYPE(NML_RESTART_T), INTENT(INOUT)    :: NML_RESTART
  220                                                         
 
 
                                                    w3nmluprstrmd.F90  page   5
 
 
  221     ! locals
  222     INTEGER                                :: IERR
  223     TYPE(NML_RESTART_T) :: RESTART
  224     NAMELIST /RESTART_NML/ RESTART
  225                                   
  226     IERR = 0
  227             
  228     ! set default values
  229     RESTART%RESTARTTIME  = '19680607 120000'
  230                                             
  231     ! read restart namelist
  232     REWIND (NDSI)
  233     READ (NDSI, nml=RESTART_NML, iostat=IERR, iomsg=MSG)
  234     IF (IERR.NE.0) THEN
  235       WRITE (NDSE,'(A,/A)') &
  236         'ERROR: READ_RESTART_NML: namelist read error', &
  237         'ERROR: '//TRIM(MSG)
  238       CALL EXTCDE (1)
  239     END IF
  240           
  241     ! save namelist
  242     NML_RESTART = RESTART
  243                          
  244   END SUBROUTINE READ_RESTART_NML
  245                                  
  246 !/ ------------------------------------------------------------------- /
  247                                                                         
  248                                                                         
  249                                                                         
  250 !/ ------------------------------------------------------------------- /
  251                                                                         
  252   SUBROUTINE READ_UPDATE_NML (NDSI, NML_UPDATE)
  253 !/
  254 !/                  +-----------------------------------+
  255 !/                  | WAVEWATCH III           NOAA/NCEP |
  256 !/                  |           M. Accensi              |
  257 !/                  |                                   |
  258 !/                  |                        FORTRAN 90 |
  259 !/                  | Last update :         06-Oct-2020 |
  260 !/                  +-----------------------------------+
  261 !/
  262 !  1. Purpose :
  263 !
  264 !  2. Method :
  265 !
  266 !     See source term routines.
  267 !
  268 !  3. Parameters :
  269 !
  270 !     Parameter list
  271 !     ----------------------------------------------------------------
  272 !      NDSI         Int.
  273 !      NML_UPDATE   Type.
  274 !     ----------------------------------------------------------------
  275 !
 
 
                                                    w3nmluprstrmd.F90  page   6
 
 
  276 !  4. Subroutines used :
  277 !
  278 !      Name      TYPE  Module   Description
  279 !     ----------------------------------------------------------------
  280 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  281 !     ----------------------------------------------------------------
  282 !
  283 !  5. Called by :
  284 !
  285 !      Name        TYPE   Module  Description
  286 !     ----------------------------------------------------------------
  287 !      W3NMLUPRSTR Subr.   N/A    Namelist configuration routine.
  288 !     ----------------------------------------------------------------
  289 !
  290 !  6. Error messages :
  291 !
  292 !     None.
  293 !
  294 !  7. Remarks :
  295 !
  296 !  8. Structure :
  297 !
  298 !     See source code.
  299 !
  300 !  9. Switches :
  301 !
  302 ! 10. Source code :
  303 !
  304 !/ ------------------------------------------------------------------- /
  305                                                                         
  306     USE W3ODATMD, ONLY: NDSE
  307     USE W3SERVMD, ONLY: EXTCDE
  308                               
  309     IMPLICIT NONE
  310                  
  311     INTEGER, INTENT(IN)                 :: NDSI
  312     TYPE(NML_UPDATE_T), INTENT(INOUT)   :: NML_UPDATE
  313                                                      
  314     ! locals
  315     INTEGER                                :: IERR
  316     TYPE(NML_UPDATE_T) :: UPDATE
  317     NAMELIST /UPDATE_NML/ UPDATE
  318                                 
  319     IERR = 0
  320             
  321     ! set default values for update approach
  322     ! as set, these would run the update but not correct
  323     ! any spectra (scalar correction by factor of 1.0)
  324     UPDATE%UPDPROC    = 'UPD0F'      ! Update type
  325     UPDATE%PRCNTG     = 1.0          ! Scalar correction factor (1.0=no c>
  326     UPDATE%PRCNTGCAP  = 10.0         ! Cap on correction factor
  327     UPDATE%THRWSEA    = 0.7          ! Energy threshold for wind-sea domi>
  328     UPDATE%FILE       = 'anl.grbtxt' ! Corrected analysed SWH field file
  329                                                                         
  330     ! read file namelist
 
 
                                                    w3nmluprstrmd.F90  page   7
 
 
  331     REWIND (NDSI)
  332     READ (NDSI, nml=UPDATE_NML, iostat=IERR, iomsg=MSG)
  333     IF (IERR.GT.0) THEN
  334       WRITE (NDSE,'(A,/A)') &
  335         'ERROR: READ_UPDATE_NML: namelist read error', &
  336         'ERROR: '//TRIM(MSG)
  337       CALL EXTCDE (2)
  338     END IF
  339           
  340     ! save namelist
  341     NML_UPDATE = UPDATE
  342                        
  343   END SUBROUTINE READ_UPDATE_NML
  344                                 
  345 !/ ------------------------------------------------------------------- /
  346                                                                         
  347                                                                         
  348                                                                         
  349 !/ ------------------------------------------------------------------- /
  350                                                                         
  351   SUBROUTINE REPORT_RESTART_NML (NML_RESTART)
  352 !/
  353 !/                  +-----------------------------------+
  354 !/                  | WAVEWATCH III           NOAA/NCEP |
  355 !/                  |           M. Accensi              |
  356 !/                  |                        FORTRAN 90 |
  357 !/                  | Last update :         06-Oct-2020 |
  358 !/                  +-----------------------------------+
  359 !/
  360 !/
  361 !  1. Purpose :
  362 !
  363 !  2. Method :
  364 !
  365 !     See source term routines.
  366 !
  367 !  3. Parameters :
  368 !
  369 !     Parameter list
  370 !     ----------------------------------------------------------------
  371 !      NML_RESTART  Type.
  372 !     ----------------------------------------------------------------
  373 !
  374 !  4. Subroutines used :
  375 !
  376 !      Name      TYPE  Module   Description
  377 !     ----------------------------------------------------------------
  378 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  379 !     ----------------------------------------------------------------
  380 !
  381 !  5. Called by :
  382 !
  383 !      Name        TYPE   Module  Description
  384 !     ----------------------------------------------------------------
  385 !      W3NMLUPRSTR Subr.   N/A    Namelist configuration routine.
 
 
                                                    w3nmluprstrmd.F90  page   8
 
 
  386 !     ----------------------------------------------------------------
  387 !
  388 !  6. Error messages :
  389 !
  390 !     None.
  391 !
  392 !  7. Remarks :
  393 !
  394 !  8. Structure :
  395 !
  396 !     See source code.
  397 !
  398 !  9. Switches :
  399 !
  400 ! 10. Source code :
  401 !
  402 !/ ------------------------------------------------------------------- /
  403                                                                         
  404                                                                         
  405     IMPLICIT NONE
  406                  
  407     TYPE(NML_RESTART_T), INTENT(IN) :: NML_RESTART
  408                                                   
  409                                                   
  410       WRITE (MSG,'(A)') 'RESTART % '
  411       WRITE (NDSN,'(A)')
  412       WRITE (NDSN,10) TRIM(MSG),'RESTARTTIME = ', TRIM(NML_RESTART%RESTAR>
  413                                                                           
  414 10  FORMAT (A,2X,A,A)
  415                      
  416   END SUBROUTINE REPORT_RESTART_NML
  417                                    
  418 !/ ------------------------------------------------------------------- /
  419                                                                         
  420                                                                         
  421                                                                         
  422 !/ ------------------------------------------------------------------- /
  423                                                                         
  424   SUBROUTINE REPORT_UPDATE_NML (NML_UPDATE)
  425 !/
  426 !/                  +-----------------------------------+
  427 !/                  | WAVEWATCH III           NOAA/NCEP |
  428 !/                  |           M. Accensi              |
  429 !/                  |                        FORTRAN 90 |
  430 !/                  | Last update :         06-Oct-2020 |
  431 !/                  +-----------------------------------+
  432 !/
  433 !/
  434 !  1. Purpose :
  435 !
  436 !  2. Method :
  437 !
  438 !     See source term routines.
  439 !
  440 !  3. Parameters :
 
 
                                                    w3nmluprstrmd.F90  page   9
 
 
  441 !
  442 !     Parameter list
  443 !     ----------------------------------------------------------------
  444 !      NML_UPDATE  Type.
  445 !     ----------------------------------------------------------------
  446 !
  447 !  4. Subroutines used :
  448 !
  449 !      Name      TYPE  Module   Description
  450 !     ----------------------------------------------------------------
  451 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  452 !     ----------------------------------------------------------------
  453 !
  454 !  5. Called by :
  455 !
  456 !      Name        TYPE   Module  Description
  457 !     ----------------------------------------------------------------
  458 !      W3NMLUPRSTR Subr.   N/A    Namelist configuration routine.
  459 !     ----------------------------------------------------------------
  460 !
  461 !  6. Error messages :
  462 !
  463 !     None.
  464 !
  465 !  7. Remarks :
  466 !
  467 !  8. Structure :
  468 !
  469 !     See source code.
  470 !
  471 !  9. Switches :
  472 !
  473 ! 10. Source code :
  474 !
  475 !/ ------------------------------------------------------------------- /
  476                                                                         
  477                                                                         
  478     IMPLICIT NONE
  479                  
  480     TYPE(NML_UPDATE_T), INTENT(IN) :: NML_UPDATE
  481                                                 
  482                                                 
  483       WRITE (MSG,'(A)') 'UPDATE % '
  484       WRITE (NDSN,'(A)')
  485       WRITE (NDSN,10) TRIM(MSG),'UPDPROC   = ', TRIM(NML_UPDATE%UPDPROC)
  486       ! PRCNTG only used by UPD0F
  487       IF (TRIM(NML_UPDATE%UPDPROC) .EQ. 'UPD0F') THEN
  488          WRITE (NDSN,11) TRIM(MSG),'PRCNTG    = ', NML_UPDATE%PRCNTG
  489       ELSE
  490          WRITE (NDSN,11) TRIM(MSG),'PRCNTGCAP = ', NML_UPDATE%PRCNTGCAP
  491          ! THRWSEA only used by UPD5/6
  492          IF ((TRIM(NML_UPDATE%UPDPROC) .EQ. 'UPD5') .OR. &
  493              (TRIM(NML_UPDATE%UPDPROC) .EQ. 'UPD6')) THEN
  494             WRITE (NDSN,11) TRIM(MSG),'THRWSEA   = ', NML_UPDATE%THRWSEA
  495          ENDIF
 
 
                                                    w3nmluprstrmd.F90  page  10
 
 
  496          WRITE (NDSN,10) TRIM(MSG),'FILE      = ', TRIM(NML_UPDATE%FILE)
  497       ENDIF
  498            
  499 10  FORMAT (A,2X,A,A)
  500 11  FORMAT (A,2X,A,F5.3)
  501                         
  502   END SUBROUTINE REPORT_UPDATE_NML
  503                                   
  504 !/ ------------------------------------------------------------------- /
  505                                                                         
  506                                                                         
  507                                                                         
  508 END MODULE W3NMLUPRSTRMD
  509                         
  510 !/ ------------------------------------------------------------------- /
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
