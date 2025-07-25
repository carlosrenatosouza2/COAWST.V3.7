                                                         w3sln1md.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3SLN1MD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         29-May-2009 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    23-Jun-2006 : Origination.                        ( version 3.09 )
   13 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   14 !/
   15 !/    Copyright 2009 National Weather Service (NWS),
   16 !/       National Oceanic and Atmospheric Administration.  All rights
   17 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   18 !/       No unauthorized use without permission.
   19 !/
   20 !  1. Purpose :
   21 !
   22 !     Linear wind input according to Cavaleri and Melanotte-Rizzoli
   23 !     (1982) filtered for low frequencies according to Tolman (1992).
   24 !
   25 !  2. Variables and types :
   26 !
   27 !  3. Subroutines and functions :
   28 !
   29 !      Name      Type  Scope    Description
   30 !     ----------------------------------------------------------------
   31 !      W3SLN1    Subr. Public   User supplied linear input.
   32 !     ----------------------------------------------------------------
   33 !
   34 !  4. Subroutines and functions used :
   35 !
   36 !      Name      Type  Module   Description
   37 !     ----------------------------------------------------------------
   38 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   39 !     ----------------------------------------------------------------
   40 !
   41 !  5. Remarks :
   42 !
   43 !  6. Switches :
   44 !
   45 !     !/S  Enable subroutine tracing.
   46 !     !/T  Test output.
   47 !
   48 !  7. Source code :
   49 !/
   50 !/ ------------------------------------------------------------------- /
   51 !/
   52       PUBLIC
   53 !/
   54       CONTAINS
   55 !/ ------------------------------------------------------------------- /
 
 
                                                         w3sln1md.F90  page   2
 
 
   56       SUBROUTINE W3SLN1 (K, FHIGH, USTAR, USDIR, S)
   57 !/
   58 !/                  +-----------------------------------+
   59 !/                  | WAVEWATCH III           NOAA/NCEP |
   60 !/                  |           H. L. Tolman            |
   61 !/                  |                        FORTRAN 90 |
   62 !/                  | Last update :         23-Jun-2006 |
   63 !/                  +-----------------------------------+
   64 !/
   65 !/    23-Jun-2006 : Origination.                        ( version 3.09 )
   66 !/
   67 !  1. Purpose :
   68 !
   69 !     Linear wind input according to Cavaleri and Melanotte-Rizzoli
   70 !     (1982) filtered for low frequencies according to Tolman (1992).
   71 !
   72 !  2. Method :
   73 !
   74 !     The expression of Cavaleri and Melanotte-Rizzoli, converted to
   75 !     action spectra defined in terms of wavenumber and direction
   76 !     becomes
   77 !
   78 !                       -1       /     /                \ \ 4
   79 !       Sln  = SLNC1 * k   * max | 0., | U* cos(Dtheta) | |        (1)
   80 !                                \     \                / /
   81 !
   82 !                             2     -2
   83 !              SLNC1 = 80 RHOr  GRAV   FILT                        (2)
   84 !
   85 !     Where :
   86 !
   87 !        RHOr     Density of air dev. by density of water.
   88 !        U*       Wind friction velocity.
   89 !        Dtheta   Difference in wind and wave direction.
   90 !        FILT     Filter based on PM and cut-off frequencies.
   91 !
   92 !  3. Parameters :
   93 !
   94 !     Parameter list
   95 !     ----------------------------------------------------------------
   96 !       K       R.A.  I   Wavenumber for entire spectrum.
   97 !       FHIGH   R.A.  I   Cut-off frequency in integration (rad/s)
   98 !       USTAR   Real  I   Friction velocity.
   99 !       USDIR   Real  I   Direction of USTAR.
  100 !       S       R.A.  O   Source term.
  101 !     ----------------------------------------------------------------
  102 !                         *) Stored as 1-D array with dimension NTH*NK
  103 !
  104 !  4. Subroutines used :
  105 !
  106 !      Name      Type  Module   Description
  107 !     ----------------------------------------------------------------
  108 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  109 !     ----------------------------------------------------------------
  110 !
 
 
                                                         w3sln1md.F90  page   3
 
 
  111 !  5. Called by :
  112 !
  113 !      Name      Type  Module   Description
  114 !     ----------------------------------------------------------------
  115 !      W3SRCE    Subr. W3SRCEMD Source term integration.
  116 !      W3EXPO    Subr.   N/A    Point output post-processor.
  117 !      GXEXPO    Subr.   N/A    GrADS point output post-processor.
  118 !     ----------------------------------------------------------------
  119 !
  120 !  6. Error messages :
  121 !
  122 !       None.
  123 !
  124 !  7. Remarks :
  125 !
  126 !  8. Structure :
  127 !
  128 !     See source code.
  129 !
  130 !  9. Switches :
  131 !
  132 !     !/S  Enable subroutine tracing.
  133 !     !/T  Test output.
  134 !
  135 ! 10. Source code :
  136 !
  137 !/ ------------------------------------------------------------------- /
  138       USE CONSTANTS
  139       USE W3GDATMD, ONLY: NTH, NK, ECOS, ESIN, SIG, SLNC1, FSPM, FSHF
  140       USE W3ODATMD, ONLY: NDSE, NDST
  141       USE W3SERVMD, ONLY: EXTCDE
  142 !/
  143       IMPLICIT NONE
  144 !/
  145 !/ ------------------------------------------------------------------- /
  146 !/ Parameter list
  147 !/
  148       REAL, INTENT(IN)        :: K(NK), FHIGH, USTAR, USDIR
  149       REAL, INTENT(OUT)       :: S(NTH,NK)
  150 !/
  151 !/ ------------------------------------------------------------------- /
  152 !/ Local parameters
  153 !/
  154       INTEGER                 :: ITH, IK
  155       REAL                    :: COSU, SINU, DIRF(NTH), FAC, FF1, FF2, &
  156                                  FFILT, RFR, WNF(NK)
  157 !/
  158 !/ ------------------------------------------------------------------- /
  159 !/
  160 !
  161 ! 1.  Set up factors ------------------------------------------------- *
  162 !
  163       COSU   = COS(USDIR)
  164       SINU   = SIN(USDIR)
  165 !
 
 
                                                         w3sln1md.F90  page   4
 
 
  166       DO ITH=1, NTH
  167         DIRF(ITH) = MAX ( 0. , (ECOS(ITH)*COSU+ESIN(ITH)*SINU) )**4
  168         END DO
  169 !
  170       FAC    = SLNC1 * USTAR**4
  171       FF1    = FSPM * GRAV/(28.*USTAR)
  172       FF2    = FSHF * MIN(SIG(NK),FHIGH)
  173       FFILT  = MIN ( MAX(FF1,FF2) , 2.*SIG(NK) )
  174       DO IK=1, NK
  175         RFR    = SIG(IK) / FFILT
  176         IF ( RFR .LT. 0.5 ) THEN
  177             WNF(IK) = 0.
  178           ELSE
  179             WNF(IK) = FAC / K(IK) * EXP(-RFR**(-4))
  180           END IF
  181         END DO
  182 !
  183 ! 2.  Compose source term -------------------------------------------- *
  184 !
  185       DO IK=1, NK
  186         S(:,IK) = WNF(IK) * DIRF(:)
  187         END DO
  188 !
  189       RETURN
  190 !
  191 ! Formats
  192 !
  193 !/
  194 !/ End of W3SLN1 ----------------------------------------------------- /
  195 !/
  196       END SUBROUTINE W3SLN1
  197 !/
  198 !/ End of module INSLN1MD -------------------------------------------- /
  199 !/
  200       END MODULE W3SLN1MD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
