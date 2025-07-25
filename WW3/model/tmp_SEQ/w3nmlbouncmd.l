                                                     w3nmlbouncmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3NMLBOUNCMD
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
   17 !     Manages namelists from configuration file ww3_bounc.nml for ww3_bou>
   18 !
   19 !/ ------------------------------------------------------------------- /
   20                                                                         
   21   ! module defaults
   22   IMPLICIT NONE
   23                
   24   PUBLIC
   25         
   26   ! bound structure
   27   TYPE NML_BOUND_T
   28     CHARACTER(5)                :: MODE
   29     INTEGER                     :: INTERP
   30     INTEGER                     :: VERBOSE
   31     CHARACTER(128)              :: FILE
   32   END TYPE NML_BOUND_T
   33                       
   34                       
   35   ! miscellaneous
   36   CHARACTER(256)                :: MSG
   37   INTEGER                       :: NDSN
   38                                        
   39                                        
   40                                        
   41                                        
   42   CONTAINS
   43 !/ ------------------------------------------------------------------- /
   44   SUBROUTINE W3NMLBOUNC (NDSI, INFILE, NML_BOUND, IERR)
   45 !/
   46 !/                  +-----------------------------------+
   47 !/                  | WAVEWATCH III           NOAA/NCEP |
   48 !/                  |           M. Accensi              |
   49 !/                  |                                   |
   50 !/                  |                        FORTRAN 90 |
   51 !/                  | Last update :         15-May-2018 |
   52 !/                  +-----------------------------------+
   53 !/
   54 !
   55 !  1. Purpose :
 
 
                                                     w3nmlbouncmd.F90  page   2
 
 
   56 !
   57 !     Reads all the namelist to define the input boundary
   58 !
   59 !  2. Method :
   60 !
   61 !     See source term routines.
   62 !
   63 !  3. Parameters :
   64 !
   65 !     Parameter list
   66 !     ----------------------------------------------------------------
   67 !      NDSI        Int.
   68 !      INFILE      Char.
   69 !      NML_BOUND   type.
   70 !      IERR        Int.
   71 !     ----------------------------------------------------------------
   72 !
   73 !  4. Subroutines used :
   74 !
   75 !      Name      TYPE  Module   Description
   76 !     ----------------------------------------------------------------
   77 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
   78 !      READ_BOUND_NML
   79 !     ----------------------------------------------------------------
   80 !
   81 !  5. Called by :
   82 !
   83 !      Name      TYPE  Module   Description
   84 !     ----------------------------------------------------------------
   85 !      WW3_BOUNC  Prog.   N/A    Preprocess input boundaries.
   86 !     ----------------------------------------------------------------
   87 !
   88 !  6. Error messages :
   89 !
   90 !     None.
   91 !
   92 !  7. Remarks :
   93 !
   94 !  8. Structure :
   95 !
   96 !     See source code.
   97 !
   98 !  9. Switches :
   99 !
  100 ! 10. Source code :
  101 !
  102 !/ ------------------------------------------------------------------- /
  103                                                                         
  104     USE W3ODATMD, ONLY: NDSE
  105                             
  106     IMPLICIT NONE
  107                  
  108     INTEGER, INTENT(IN)                         :: NDSI
  109     CHARACTER*(*), INTENT(IN)                   :: INFILE
  110     TYPE(NML_BOUND_T), INTENT(INOUT)            :: NML_BOUND
 
 
                                                     w3nmlbouncmd.F90  page   3
 
 
  111     INTEGER, INTENT(OUT)                        :: IERR
  112                                                        
  113     IERR = 0
  114             
  115     ! open namelist log file
  116     NDSN = 3
  117     OPEN (NDSN, file=TRIM(INFILE)//'.log', form='formatted', iostat=IERR)
  118        IF (IERR.NE.0) THEN
  119       WRITE (NDSE,'(A)') 'ERROR: open full nml file '//TRIM(INFILE)//'.lo>
  120       RETURN
  121     END IF
  122           
  123     ! open input file
  124     OPEN (NDSI, file=TRIM(INFILE), form='formatted', status='old', iostat>
  125     IF (IERR.NE.0) THEN
  126       WRITE (NDSE,'(A)') 'ERROR: open input file '//TRIM(INFILE)//' faile>
  127       RETURN
  128     END IF
  129           
  130     ! read bound namelist
  131     CALL READ_BOUND_NML (NDSI, NML_BOUND)
  132     CALL REPORT_BOUND_NML (NML_BOUND)
  133                                      
  134     ! close namelist files
  135     CLOSE (NDSI)
  136     CLOSE (NDSN)
  137                 
  138   END SUBROUTINE W3NMLBOUNC
  139                            
  140                            
  141 !/ ------------------------------------------------------------------- /
  142                                                                         
  143                                                                         
  144                                                                         
  145                                                                         
  146                                                                         
  147                                                                         
  148 !/ ------------------------------------------------------------------- /
  149                                                                         
  150   SUBROUTINE READ_BOUND_NML (NDSI, NML_BOUND)
  151 !/
  152 !/                  +-----------------------------------+
  153 !/                  | WAVEWATCH III           NOAA/NCEP |
  154 !/                  |           M. Accensi              |
  155 !/                  |                                   |
  156 !/                  |                        FORTRAN 90 |
  157 !/                  | Last update :         15-May-2018 |
  158 !/                  +-----------------------------------+
  159 !/
  160 !  1. Purpose :
  161 !
  162 !  2. Method :
  163 !
  164 !     See source term routines.
  165 !
 
 
                                                     w3nmlbouncmd.F90  page   4
 
 
  166 !  3. Parameters :
  167 !
  168 !     Parameter list
  169 !     ----------------------------------------------------------------
  170 !      NDSI         Int.
  171 !      NML_BOUND    Type.
  172 !     ----------------------------------------------------------------
  173 !
  174 !  4. Subroutines used :
  175 !
  176 !      Name      TYPE  Module   Description
  177 !     ----------------------------------------------------------------
  178 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  179 !     ----------------------------------------------------------------
  180 !
  181 !  5. Called by :
  182 !
  183 !      Name      TYPE  Module   Description
  184 !     ----------------------------------------------------------------
  185 !      W3NMLBOUNC Subr.   N/A    Namelist configuration routine.
  186 !     ----------------------------------------------------------------
  187 !
  188 !  6. Error messages :
  189 !
  190 !     None.
  191 !
  192 !  7. Remarks :
  193 !
  194 !  8. Structure :
  195 !
  196 !     See source code.
  197 !
  198 !  9. Switches :
  199 !
  200 ! 10. Source code :
  201 !
  202 !/ ------------------------------------------------------------------- /
  203                                                                         
  204     USE W3ODATMD, ONLY: NDSE
  205     USE W3SERVMD, ONLY: EXTCDE
  206                               
  207     IMPLICIT NONE
  208                  
  209     INTEGER, INTENT(IN)                 :: NDSI
  210     TYPE(NML_BOUND_T), INTENT(INOUT)    :: NML_BOUND
  211                                                     
  212     ! locals
  213     INTEGER                   :: IERR
  214     TYPE(NML_BOUND_T) :: BOUND
  215     NAMELIST /BOUND_NML/ BOUND
  216                               
  217     IERR = 0
  218             
  219     ! set default values for track structure
  220     BOUND%MODE       = 'WRITE'
 
 
                                                     w3nmlbouncmd.F90  page   5
 
 
  221     BOUND%INTERP     = 2
  222     BOUND%VERBOSE    = 1
  223     BOUND%FILE       = 'spec.list'
  224                                   
  225     ! read bound namelist
  226     REWIND (NDSI)
  227     READ (NDSI, nml=BOUND_NML, iostat=IERR, iomsg=MSG)
  228     IF (IERR.GT.0) THEN
  229       WRITE (NDSE,'(A,/A)') &
  230         'ERROR: READ_BOUND_NML: namelist read error', &
  231         'ERROR: '//TRIM(MSG)
  232       CALL EXTCDE (1)
  233     END IF
  234           
  235     ! save namelist
  236     NML_BOUND = BOUND
  237                      
  238   END SUBROUTINE READ_BOUND_NML
  239                                
  240 !/ ------------------------------------------------------------------- /
  241                                                                         
  242                                                                         
  243                                                                         
  244                                                                         
  245                                                                         
  246                                                                         
  247                                                                         
  248                                                                         
  249 !/ ------------------------------------------------------------------- /
  250                                                                         
  251   SUBROUTINE REPORT_BOUND_NML (NML_BOUND)
  252 !/
  253 !/                  +-----------------------------------+
  254 !/                  | WAVEWATCH III           NOAA/NCEP |
  255 !/                  |           M. Accensi              |
  256 !/                  |                        FORTRAN 90 |
  257 !/                  | Last update :         15-May-2018 |
  258 !/                  +-----------------------------------+
  259 !/
  260 !/
  261 !  1. Purpose :
  262 !
  263 !  2. Method :
  264 !
  265 !     See source term routines.
  266 !
  267 !  3. Parameters :
  268 !
  269 !     Parameter list
  270 !     ----------------------------------------------------------------
  271 !      NML_BOUND  Type.
  272 !     ----------------------------------------------------------------
  273 !
  274 !  4. Subroutines used :
  275 !
 
 
                                                     w3nmlbouncmd.F90  page   6
 
 
  276 !      Name      TYPE  Module   Description
  277 !     ----------------------------------------------------------------
  278 !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
  279 !     ----------------------------------------------------------------
  280 !
  281 !  5. Called by :
  282 !
  283 !      Name      TYPE  Module   Description
  284 !     ----------------------------------------------------------------
  285 !      W3NMLBOUNC Subr.   N/A    Namelist configuration routine.
  286 !     ----------------------------------------------------------------
  287 !
  288 !  6. Error messages :
  289 !
  290 !     None.
  291 !
  292 !  7. Remarks :
  293 !
  294 !  8. Structure :
  295 !
  296 !     See source code.
  297 !
  298 !  9. Switches :
  299 !
  300 ! 10. Source code :
  301 !
  302 !/ ------------------------------------------------------------------- /
  303                                                                         
  304                                                                         
  305     IMPLICIT NONE
  306                  
  307     TYPE(NML_BOUND_T), INTENT(IN) :: NML_BOUND
  308                                               
  309                                               
  310       WRITE (MSG,'(A)') 'BOUND % '
  311       WRITE (NDSN,'(A)')
  312       WRITE (NDSN,10) TRIM(MSG),'MODE       = ', TRIM(NML_BOUND%MODE)
  313       WRITE (NDSN,11) TRIM(MSG),'INTERP     = ', NML_BOUND%INTERP
  314       WRITE (NDSN,11) TRIM(MSG),'VERBOSE    = ', NML_BOUND%VERBOSE
  315       WRITE (NDSN,10) TRIM(MSG),'FILE       = ', TRIM(NML_BOUND%FILE)
  316                                                                      
  317                                                                      
  318 10  FORMAT (A,2X,A,A)
  319 11  FORMAT (A,2X,A,I8)
  320                       
  321   END SUBROUTINE REPORT_BOUND_NML
  322                                  
  323 !/ ------------------------------------------------------------------- /
  324                                                                         
  325                                                                         
  326                                                                         
  327                                                                         
  328 END MODULE W3NMLBOUNCMD
  329                        
  330 !/ ------------------------------------------------------------------- /
 
 
                                                     w3nmlbouncmd.F90  page   7
 
 
  331                                                                         
  332                                                                         
  333                                                                         
  334                                                                         
  335                                                                         
  336                                                                         
  337                                                                         
  338                                                                         
  339                                                                         
  340                                                                         
  341                                                                         
  342                                                                         
  343                                                                         
  344                                                                         
  345                                                                         
  346                                                                         
  347                                                                         
  348                                                                         
  349                                                                         
  350                                                                         
  351                                                                         
  352                                                                         
  353                                                                         
  354                                                                         
  355                                                                         
  356                                                                         
  357                                                                         
  358                                                                         
  359                                                                         
  360                                                                         
  361                                                                         
  362                                                                         
  363                                                                         
  364                                                                         
  365                                                                         
  366                                                                         
  367                                                                         
  368                                                                         
  369                                                                         
  370                                                                         
  371                                                                         
  372                                                                         
  373                                                                         
  374                                                                         
  375                                                                         
  376                                                                         
  377                                                                         
  378                                                                         
  379                                                                         
  380                                                                         
  381                                                                         
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/opt/cray/netcdf-hdf5parallel/4.2.0/pgi/119/include -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
