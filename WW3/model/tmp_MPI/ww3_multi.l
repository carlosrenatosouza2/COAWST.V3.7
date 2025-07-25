                                                        ww3_multi.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3MLTI
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         29-May-2009 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    04-May-2005 : Origination.                        ( version 3.07 )
   13 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   14 !/    17-Feb-2016 : New version from namelist use       ( version 5.11 )
   15 !/
   16 !/    Copyright 2009 National Weather Service (NWS),
   17 !/       National Oceanic and Atmospheric Administration.  All rights
   18 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   19 !/       No unauthorized use without permission.
   20 !/
   21 !  1. Purpose :
   22 !
   23 !     Program shell or driver to run the multi-grid wave model
   24 !     (uncoupled).
   25 !
   26 !  2. Method :
   27 !
   28 !  3. Parameters :
   29 !
   30 !     Local parameters.
   31 !     ----------------------------------------------------------------
   32 !     ----------------------------------------------------------------
   33 !
   34 !  4. Subroutines used :
   35 !
   36 !      Name      Type  Module   Description
   37 !     ----------------------------------------------------------------
   38 !      WMINIT    Subr. WMINITMD Multi-grid model initialization.
   39 !      WMFINL    Subr. WMFINLMD Multi-grid model finalization.
   40 !
   41 !      MPI_INIT, MPI_COMM_SIZE, MPI_COMM_RANK, MPI_BARRIER,
   42 !         MPI_FINALIZE
   43 !                Subr.          Standard MPI routines.
   44 !     ----------------------------------------------------------------
   45 !
   46 !  5. Called by :
   47 !
   48 !     None, stand-alone program.
   49 !
   50 !  6. Error messages :
   51 !
   52 !  7. Remarks :
   53 !
   54 !     - This is he third version, version 1 and 2 were use for proof
   55 !       of concept only, and were not retained.
 
 
                                                        ww3_multi.F90  page   2
 
 
   56 !
   57 !  8. Structure :
   58 !
   59 !     ----------------------------------------------------------------
   60 !      0.  Initialization necessary for driver
   61 !        a General I/O: (implicit in wmmdatmd)
   62 !        b MPI environment
   63 !        c Identifying output to "screen" unit
   64 !      1.  Initialization of all wave models / grids       ( WMINIT )
   65 !      2.  Run the multi-grid models                       ( WMWAVE )
   66 !      3.  Finalization of wave model                      ( WMFINL )
   67 !      4.  Finalization of driver
   68 !     ----------------------------------------------------------------
   69 !
   70 !  9. Switches :
   71 !
   72 !       !/MPI   Including MPI routines / environment.
   73 !
   74 ! 10. Source code :
   75 !
   76 !/ ------------------------------------------------------------------- /
   77       USE WMINITMD, ONLY: WMINIT, WMINITNML
   78       USE WMWAVEMD, ONLY: WMWAVE
   79       USE WMFINLMD, ONLY: WMFINL
   80 !/
   81       USE WMMDATMD, ONLY: MDSI, MDSO, MDSS, MDST, MDSE, &
   82                           NMPROC, IMPROC, NMPSCR, NRGRD, ETIME
   83 !/
   84       IMPLICIT NONE
   85 !
   86       INCLUDE "mpif.h"
   87 !/
   88 !/ ------------------------------------------------------------------- /
   89 !/ Local parameters
   90 !/
   91       INTEGER              :: I, MPI_COMM = -99
   92       INTEGER, ALLOCATABLE :: TEND(:,:)
   93       LOGICAL              :: FLGNML
   94       INTEGER              :: IERR_MPI
   95       LOGICAL              :: FLHYBR = .FALSE.
   96 !/
   97 !/ ------------------------------------------------------------------- /
   98 ! 0.  Initialization necessary for driver
   99 ! 0.a General I/O: all can start with initialization in wmmdatmd
  100 !
  101 ! 0.b MPI environment: Here, we use MPI_COMM_WORLD
  102 !
  103       CALL MPI_INIT      ( IERR_MPI )
  104       MPI_COMM = MPI_COMM_WORLD
  105       CALL MPI_COMM_SIZE ( MPI_COMM, NMPROC, IERR_MPI )
  106       CALL MPI_COMM_RANK ( MPI_COMM, IMPROC, IERR_MPI )
  107       IMPROC = IMPROC + 1
  108 !
  109 ! 0.c Identifying output to "screen" unit
  110 !
 
 
                                                        ww3_multi.F90  page   3
 
 
  111       IF ( IMPROC .EQ. NMPSCR ) WRITE (*,900)
  112 !
  113 !/ ------------------------------------------------------------------- /
  114 ! 1.  Initialization of all wave models / grids
  115 !     Use only one of the calls ....
  116 !
  117 ! ... Log and screen output, no separate test output file
  118 !
  119 !     CALL WMINIT ( MDSI, MDSO, MDSS, MDST, MDSE, 'ww3_multi.inp', MPI_CO>
  120 !
  121 ! ... Screen output disabled
  122 !
  123 !     CALL WMINIT ( MDSI, MDSO, MDSO, MDST, MDSE, 'ww3_multi.inp', MPI_CO>
  124 !
  125 ! ... Separate test output file and file preamble defined
  126 !
  127 !     CALL WMINIT ( MDSI, MDSO, MDSS, 10, MDSE, 'ww3_multi.inp', MPI_COMM,
  128 !                   './data/' )
  129 !
  130 ! ... Separate test output file
  131 !
  132       INQUIRE(FILE="ww3_multi.nml", EXIST=FLGNML)
  133       IF (FLGNML) THEN
  134         CALL WMINITNML ( MDSI, MDSO, MDSS, 10, MDSE, 'ww3_multi.nml', MPI>
  135       ELSE
  136         CALL WMINIT ( MDSI, MDSO, MDSS, 10, MDSE, 'ww3_multi.inp', MPI_CO>
  137       END IF
  138 !
  139  
  140 !
  141 !/ ------------------------------------------------------------------- /
  142 ! 2.  Run the wave model
  143 !
  144       ALLOCATE ( TEND(2,NRGRD) )
  145 !
  146       DO I=1, NRGRD
  147         TEND(:,I) = ETIME(:)
  148         END DO
  149 !
  150       CALL WMWAVE ( TEND )
  151 !
  152       DEALLOCATE ( TEND )
  153 !
  154 !/ ------------------------------------------------------------------- /
  155 ! 3.  Finalize the wave model
  156 !
  157       CALL WMFINL
  158 !
  159 !/ ------------------------------------------------------------------- /
  160 ! 4   Finalize the driver
  161 !
  162       IF ( IMPROC .EQ. NMPSCR ) WRITE (*,999)
  163 !
  164       CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
  165       CALL MPI_FINALIZE  ( IERR_MPI )
 
 
                                                        ww3_multi.F90  page   4
 
 
  166 !
  167 ! Formats
  168 !
  169   900 FORMAT (/15X,'     *** WAVEWATCH III Multi-grid shell ***    '/ &
  170                15X,'================================================='/)
  171 !
  172   999 FORMAT(//'  End of program '/                                   &
  173                ' ========================================'/           &
  174                '          WAVEWATCH III Multi-grid shell '/)
  175 !/
  176 !/ End of W3MLTI ----------------------------------------------------- /
  177 !/
  178       END PROGRAM W3MLTI
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
