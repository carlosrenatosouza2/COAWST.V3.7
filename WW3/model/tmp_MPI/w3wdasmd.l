                                                         w3wdasmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3WDASMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         06-Dec-2010 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    25-Jan-2002 : Origination.                        ( version 2.17 )
   13 !/    27-Dec-2004 : Multiple grid version.              ( version 3.06 )
   14 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   15 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
   16 !/                  specify index closure for a grid.   ( version 3.14 )
   17 !/                  (T. J. Campbell, NRL)
   18 !/
   19 !/    Copyright 2009 National Weather Service (NWS),
   20 !/       National Oceanic and Atmospheric Administration.  All rights
   21 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   22 !/       No unauthorized use without permission.
   23 !/
   24 !  1. Purpose :
   25 !
   26 !     This module is intended as the interface for externally supplied
   27 !     data assimlation software to be used with WAVEWATCH III. The
   28 !     main subroutine W3WDAS is incorporated in the generic WAVEWATCH
   29 !     III shell ww3_shel, and thus provides integrated time management
   30 !     and running of the wave model and data assimilation side by side.
   31 !
   32 !     Present wave conditions (including dynamically changing wave
   33 !     grids), as well as wave data are passed to the routine through
   34 !     the dynamic data structrure, as introduced in model version 3.06
   35 !
   36 !     A three tier data structure is used with three separate data
   37 !     sets. Tentatively, they are intended for mean wave parameters,
   38 !     1-D and 2-D spectral data. This separation is made only for
   39 !     economy in file and menory usage. All three data sets are defined
   40 !     here onlt by a record length and a number of records. All data are
   41 !     treated as real numbers, but the meaing of all record components
   42 !     is completely at the discretion of the author of the data
   43 !     assimilation scheme.
   44 !
   45 !     To promote portability, it is suggested to use this module only
   46 !     as an interface to your own assimilation routine(s).
   47 !
   48 !  2. Variables and types :
   49 !
   50 !      Name      Type  Scope    Description
   51 !     ----------------------------------------------------------------
   52 !     ----------------------------------------------------------------
   53 !
   54 !  3. Subroutines and functions :
   55 !
 
 
                                                         w3wdasmd.F90  page   2
 
 
   56 !      Name      Type  Scope    Description
   57 !     ----------------------------------------------------------------
   58 !      W3WDAS    Subr. Public   Actual wave model.
   59 !     ----------------------------------------------------------------
   60 !
   61 !  4. Subroutines and functions used :
   62 !
   63 !      Name      Type  Module   Description
   64 !     ----------------------------------------------------------------
   65 !      ....      Subr. W3SERVMD Service routines.
   66 !     ----------------------------------------------------------------
   67 !
   68 !  5. Remarks :
   69 !
   70 !     - This module still requires an OpenMP or  MPI setup to be made
   71 !       compatible with WAVEWATCH III inside the user supplied
   72 !       routines.
   73 !
   74 !  6. Switches :
   75 !
   76 !       !/S     Enable subroutine tracing.
   77 !       !/T     Test output.
   78 !
   79 !  7. Source code :
   80 !
   81 !/ ------------------------------------------------------------------- /
   82       PUBLIC
   83 !/
   84       CONTAINS
   85 !/ ------------------------------------------------------------------- /
   86       SUBROUTINE W3WDAS ( DASFLAG, RECL, NDAT, DATA0, DATA1, DATA2 )
   87 !/
   88 !/                  +-----------------------------------+
   89 !/                  | WAVEWATCH III           NOAA/NCEP |
   90 !/                  |           H. L. Tolman            |
   91 !/                  |                        FORTRAN 90 |
   92 !/                  | Last update :         06-Dec-2010 |
   93 !/                  +-----------------------------------+
   94 !/
   95 !/    25-Jan-2002 : Origination.                        ( version 2.17 )
   96 !/    27-Dec-2004 : Multiple grid version.              ( version 3.06 )
   97 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
   98 !/                  specify index closure for a grid.   ( version 3.14 )
   99 !/                  (T. J. Campbell, NRL)
  100 !/
  101 !  1. Purpose :
  102 !
  103 !     WAVEWATCH III data assimilation interface routine.
  104 !
  105 !  3. Parameters :
  106 !
  107 !     Parameter list
  108 !     ----------------------------------------------------------------
  109 !       DASFLAG   L.A.   I   FLags for three data sets.
  110 !       RECLD   I.A.   I   Record lengths for three data sets.
 
 
                                                         w3wdasmd.F90  page   3
 
 
  111 !       ND      I.A.   I   Number of data for three data sets.
  112 !       DATAn   R.A.   I   Observations.
  113 !     ----------------------------------------------------------------
  114 !
  115 !     Local parameters :
  116 !     ----------------------------------------------------------------
  117 !     ----------------------------------------------------------------
  118 !
  119 !  4. Subroutines used :
  120 !
  121 !      Name      Type  Module   Description
  122 !     ----------------------------------------------------------------
  123 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  124 !      EXTCDE    Subr. W3SERVMD Program abort.
  125 !     ----------------------------------------------------------------
  126 !
  127 !  5. Called by :
  128 !
  129 !     Any program shell or integrated model after initialization of
  130 !     WAVEWATCH III (to assure availability of data in used modules).
  131 !
  132 !  6. Error messages :
  133 !
  134 !  7. Remarks :
  135 !
  136 !  8. Structure :
  137 !
  138 !     See source code.
  139 !
  140 !  9. Switches :
  141 !
  142 !       !/S     Enable subroutine tracing.
  143 !       !/T     Enable test output.
  144 !
  145 ! 10. Source code :
  146 !
  147 !/ ------------------------------------------------------------------- /
  148       USE W3GDATMD
  149       USE W3WDATMD
  150       USE W3ADATMD
  151       USE W3ODATMD, ONLY: NDSO, NDSE, NDST, SCREEN, NAPROC, IAPROC,   &
  152                           NAPLOG, NAPOUT, NAPERR
  153 !
  154       IMPLICIT NONE
  155 !
  156       INCLUDE "mpif.h"
  157 !/
  158 !/ ------------------------------------------------------------------- /
  159 !/ Parameter list
  160 !/
  161       INTEGER, INTENT(IN)     :: RECL(3), NDAT(3)
  162       REAL, INTENT(IN)        :: DATA0(RECL(1),NDAT(1))
  163       REAL, INTENT(IN)        :: DATA1(RECL(2),NDAT(2))
  164       REAL, INTENT(IN)        :: DATA2(RECL(3),NDAT(3))
  165       LOGICAL, INTENT(IN)     :: DASFLAG(3)
 
 
                                                         w3wdasmd.F90  page   4
 
 
  166 !/
  167 !/ ------------------------------------------------------------------- /
  168 !/ Local parameters :
  169 !/
  170       INTEGER                 :: J
  171 !/
  172 !/ ------------------------------------------------------------------- /
  173 ! 1.  Initializations and test output
  174 ! 1.a Subroutine tracing
  175 !
  176 ! 1.b Echo part of parameter list (test output only).
  177 !
  178 ! 1.c Test grid info from W3GDATMD
  179 !
  180 ! 2.  Actual data assimilation routine ------------------------------- /
  181 !
  182 !     User-defined data assimilation routines to be plugged in here.
  183 !     All that could be needed is avainalble in this subroutine,
  184 !     including the grid definition from W3GDATMD. All
  185 !     can thus be included in the parameter list, and no explcit links
  186 !     to other WAVEWATCH III routines will be needed within the
  187 !     data assimilation routines ( with the possible exception of the
  188 !     CONSTANTS module ), If there is a reason to terminate the code,
  189 !     pass an error code out of the routine and use EXTCDE to stop
  190 !     the WAVEWATCH III run altogether. Check the system documentation
  191 !     on how to ad your routines to the compile and link system.
  192 !
  193 !     CALL .....
  194 !
  195 !     IF ( ..... ) CALL EXTCDE ( 99 )
  196 !
  197       RETURN
  198 !
  199 ! Formats
  200 !
  201 !1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3WDAS :'/                &
  202 !              '     ILLIGAL GRID SIZES INPUT : ',4I8/                &
  203 !              '                         GRID : ',4I8/)
  204 !
  205 !/
  206 !/ End of W3WDAS ----------------------------------------------------- /
  207 !/
  208       END SUBROUTINE W3WDAS
  209 !/
  210 !/ End of module W3WDASMD -------------------------------------------- /
  211 !/
  212       END MODULE W3WDASMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
