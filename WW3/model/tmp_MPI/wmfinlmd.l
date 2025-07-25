                                                         wmfinlmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE WMFINLMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         04-Feb-2014 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    06-May-2005 : Origination.                        ( version 3.07 )
   13 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   14 !/    03-Sep-2012 : Output of initilization time.       ( version 4.10 )
   15 !/    28-Jan-2014 : Add memory hwm to profiling.        ( version 5.00 )
   16 !/    04-Feb-2014 : Switched clock to DATE_AND_TIME     ( version 4.18 )
   17 !/                  (A. Chawla and Mark Szyszka)
   18 !/
   19 !/    Copyright 2009-2014 National Weather Service (NWS),
   20 !/       National Oceanic and Atmospheric Administration.  All rights
   21 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   22 !/       No unauthorized use without permission.
   23 !/
   24 !  1. Purpose :
   25 !
   26 !     Finalization of the multi-grid wave model.
   27 !
   28 !  2. Variables and types :
   29 !
   30 !      Name      Type  Scope    Description
   31 !     ----------------------------------------------------------------
   32 !     ----------------------------------------------------------------
   33 !
   34 !  3. Subroutines and functions :
   35 !
   36 !      Name      Type  Scope    Description
   37 !     ----------------------------------------------------------------
   38 !      WMFINL    Subr. Public   Wave model initialization.
   39 !     ----------------------------------------------------------------
   40 !
   41 !  4. Subroutines and functions used :
   42 !
   43 !     See subroutine documentation.
   44 !
   45 !  5. Remarks :
   46 !
   47 !  6. Switches :
   48 !
   49 !     See subroutine documentation.
   50 !
   51 !  7. Source code :
   52 !
   53 !/ ------------------------------------------------------------------- /
   54       PUBLIC
   55 !/
 
 
                                                         wmfinlmd.F90  page   2
 
 
   56       CONTAINS
   57 !/ ------------------------------------------------------------------- /
   58       SUBROUTINE WMFINL
   59 !/
   60 !/                  +-----------------------------------+
   61 !/                  | WAVEWATCH III           NOAA/NCEP |
   62 !/                  |           H. L. Tolman            |
   63 !/                  |                        FORTRAN 90 |
   64 !/                  | Last update :         28-Jan-2014 |
   65 !/                  +-----------------------------------+
   66 !/
   67 !/    06-May-2005 : Origination.                        ( version 3.07 )
   68 !/    03-Sep-2012 : Output of initilization time.       ( version 4.10 )
   69 !/    28-Jan-2014 : Add memory hwm to profiling.        ( version 5.00 )
   70 !/
   71 !  1. Purpose :
   72 !
   73 !     Initialize multi-grid version of WAVEWATCH III.
   74 !
   75 !  2. Method :
   76 !
   77 !  3. Parameters :
   78 !
   79 !     Parameter list
   80 !     ----------------------------------------------------------------
   81 !     ----------------------------------------------------------------
   82 !
   83 !  4. Subroutines used :
   84 !
   85 !      Name      Type  Module   Description
   86 !     ----------------------------------------------------------------
   87 !      PRTIME    Subr. W3SERVMD Profiling routine ( !/MPRF )
   88 !      MPI_BARRIER
   89 !                Subr.          Standard MPI routines.
   90 !     ----------------------------------------------------------------
   91 !
   92 !  5. Called by :
   93 !
   94 !      Name      Type  Module   Description
   95 !     ----------------------------------------------------------------
   96 !      WW3_MULTI Prog.   N/A    Multi-grid model driver.
   97 !      ....                     Any coupled model.
   98 !     ----------------------------------------------------------------
   99 !
  100 !  6. Error messages :
  101 !
  102 !  7. Remarks :
  103 !
  104 !  8. Structure :
  105 !
  106 !     See source code.
  107 !
  108 !  9. Switches :
  109 !
  110 !       !/MPI   MPI routines.
 
 
                                                         wmfinlmd.F90  page   3
 
 
  111 !
  112 !       !/O10   Enable output identifying start and end of routine
  113 !
  114 !       !/F90   FORTRAN 90 specific extensions.
  115 !
  116 !       !/S     Enable subroutine tracing.
  117 !       !/T     Enable test output
  118 !       !/MPRF  Profiling.
  119 !
  120 ! 10. Source code :
  121 !
  122 !/ ------------------------------------------------------------------- /
  123       USE W3TIMEMD, ONLY: TDIFF
  124       USE WMMDATMD, ONLY: MDSS, MDSO, NMPSCR, NMPLOG, IMPROC
  125       USE WMMDATMD, ONLY: CLKDT1, CLKDT2, CLKDT3, CLKFIN
  126       USE WMMDATMD, ONLY: MPI_COMM_MWAVE
  127 !/
  128 !/
  129       IMPLICIT NONE
  130 !
  131       INCLUDE "mpif.h"
  132 !/
  133 !/ ------------------------------------------------------------------- /
  134 !/ Parameter list
  135 !/
  136 !/ ------------------------------------------------------------------- /
  137 !/ Local parameters
  138 !/
  139       INTEGER                 :: IERR_MPI
  140 !/
  141 !/ ------------------------------------------------------------------- /
  142 ! 1.  Identification at start
  143 !
  144 !/ ------------------------------------------------------------------- /
  145 ! 2.  Finalization
  146 !
  147       CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
  148 !
  149       IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )             &
  150            WRITE (MDSS,920) CLKFIN
  151       IF ( NMPLOG.EQ.IMPROC ) WRITE (MDSO,920) CLKFIN
  152 !
  153       CALL DATE_AND_TIME ( VALUES=CLKDT3 )
  154 !
  155       CLKFIN = TDIFF ( CLKDT1,CLKDT3 )
  156       IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )             &
  157            WRITE (MDSS,921) CLKFIN
  158       IF ( NMPLOG.EQ.IMPROC ) WRITE (MDSO,921) CLKFIN
  159 !
  160 !/ ------------------------------------------------------------------- /
  161 ! 3.  Identification at end
  162 !
  163       RETURN
  164 !
  165 ! Formats
 
 
                                                         wmfinlmd.F90  page   4
 
 
  166 !
  167   900 FORMAT ( ' ========== STARTING MWW3 FINALIZATION (WMFINL) ===', &
  168                '============================' )
  169   920 FORMAT (/'  Initialization time :',F10.2,' s')
  170   921 FORMAT ( '  Elapsed time        :',F10.2,' s')
  171                                                     
  172 !
  173   999 FORMAT (/' ========== END OF MWW3 INITIALIZATION (WMFINL) ===', &
  174                '============================'/)
  175 !/
  176 !/ End of WMFINL ----------------------------------------------------- /
  177 !/
  178       END SUBROUTINE WMFINL
  179 !/
  180 !/ End of module WMFINLMD -------------------------------------------- /
  181 !/
  182       END MODULE WMFINLMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
