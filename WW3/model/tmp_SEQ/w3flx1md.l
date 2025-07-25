                                                         w3flx1md.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3FLX1MD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         29-May-2009 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    03-Jul-2006 : Origination.                        ( version 3.09 )
   13 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   14 !/
   15 !/    Copyright 2009 National Weather Service (NWS),
   16 !/       National Oceanic and Atmospheric Administration.  All rights
   17 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   18 !/       No unauthorized use without permission.
   19 !/
   20 !  1. Purpose :
   21 !
   22 !     Flux/stress computations according to Wu (1980)
   23 !
   24 !  2. Variables and types :
   25 !
   26 !  3. Subroutines and functions :
   27 !
   28 !      Name      Type  Scope    Description
   29 !     ----------------------------------------------------------------
   30 !      W3FLX1    Subr. Public   Stresses according to Wu (1980).
   31 !     ----------------------------------------------------------------
   32 !
   33 !  4. Subroutines and functions used :
   34 !
   35 !      Name      Type  Module   Description
   36 !     ----------------------------------------------------------------
   37 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   38 !     ----------------------------------------------------------------
   39 !
   40 !  5. Remarks :
   41 !
   42 !     - Originally used with source term !/ST1.
   43 !
   44 !  6. Switches :
   45 !
   46 !     !/S  Enable subroutine tracing.
   47 !
   48 !  7. Source code :
   49 !/
   50 !/ ------------------------------------------------------------------- /
   51 !/
   52       PUBLIC
   53 !/
   54       CONTAINS
   55 !/ ------------------------------------------------------------------- /
 
 
                                                         w3flx1md.F90  page   2
 
 
   56       SUBROUTINE W3FLX1 ( ZWND, U10, U10D, UST, USTD, Z0, CD )
   57 !/
   58 !/                  +-----------------------------------+
   59 !/                  | WAVEWATCH III           NOAA/NCEP |
   60 !/                  |           H. L. Tolman            |
   61 !/                  |                        FORTRAN 90 |
   62 !/                  | Last update :         03-Jul-2006 |
   63 !/                  +-----------------------------------+
   64 !/
   65 !/    03-Jul-2006 : Origination.                        ( version 3.09 )
   66 !/
   67 !  1. Purpose :
   68 !
   69 !     FLux/stress computations according to Wu (1980)
   70 !
   71 !  2. Method :
   72 !
   73 !  3. Parameters :
   74 !
   75 !     Parameter list
   76 !     ----------------------------------------------------------------
   77 !       ZWND    Real   I   Wind height.
   78 !       U10     Real   I   Wind speed.
   79 !       U10D    Real   I   Wind direction.
   80 !       UST     Real   O   Friction velocity.
   81 !       USTD    Real   0   Direction of friction velocity.
   82 !       Z0      Real   O   z0 in profile law.
   83 !       CD      Real   O   Drag coefficient.
   84 !     ----------------------------------------------------------------
   85 !
   86 !  4. Subroutines used :
   87 !
   88 !      Name      Type  Module   Description
   89 !     ----------------------------------------------------------------
   90 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   91 !     ----------------------------------------------------------------
   92 !
   93 !  5. Called by :
   94 !
   95 !      Name      Type  Module   Description
   96 !     ----------------------------------------------------------------
   97 !      W3SRCE    Subr. W3SRCEMD Source term integration.
   98 !     ----------------------------------------------------------------
   99 !
  100 !  6. Error messages :
  101 !
  102 !       None.
  103 !
  104 !  7. Remarks :
  105 !
  106 !  8. Structure :
  107 !
  108 !     See source code.
  109 !
  110 !  9. Switches :
 
 
                                                         w3flx1md.F90  page   3
 
 
  111 !
  112 !     !/S  Enable subroutine tracing.
  113 !
  114 ! 10. Source code :
  115 !
  116 !/ ------------------------------------------------------------------- /
  117       USE W3ODATMD, ONLY: NDSE, IAPROC, NAPERR
  118       USE W3SERVMD, ONLY: EXTCDE
  119 !/
  120       IMPLICIT NONE
  121 !/
  122 !/ ------------------------------------------------------------------- /
  123 !/ Parameter list
  124 !/
  125       REAL, INTENT(IN)        :: ZWND, U10, U10D
  126       REAL, INTENT(OUT)       :: UST, USTD, Z0, CD
  127 !/
  128 !/ ------------------------------------------------------------------- /
  129 !/ Local parameters
  130 !/
  131 !/
  132 !/ ------------------------------------------------------------------- /
  133 !/
  134 !
  135 ! 1.  Tests ---------------------------------------------------------- *
  136 !
  137       IF ( ABS(ZWND-10.) .GT. 0.01 ) THEN
  138           IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) ZWND
  139           CALL EXTCDE (1)
  140         END IF
  141 !
  142 ! 2.  Computation ---------------------------------------------------- *
  143 !
  144       CD     = 0.001 * (0.8+0.065*U10)
  145       Z0     = ZWND * EXP ( -0.4 / SQRT(CD) )
  146       UST    = U10 * SQRT(CD)
  147       USTD   = U10D
  148 !
  149       RETURN
  150 !
  151 ! Formats
  152 !
  153  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3STR1 : '/               &
  154                '     HIGHT OF WIND SHOULD BE 10m IN THIS APPRACH '/   &
  155                '     ZWND =',F8.2,'m'/)
  156 !/
  157 !/ End of W3FLX1 ----------------------------------------------------- /
  158 !/
  159       END SUBROUTINE W3FLX1
  160 !/
  161 !/ End of module INFLX1MD -------------------------------------------- /
  162 !/
  163       END MODULE W3FLX1MD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
