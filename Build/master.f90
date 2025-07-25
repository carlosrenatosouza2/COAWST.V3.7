      PROGRAM mct_driver
!
!svn $Id: mct_driver.h 995 2020-01-10 04:01:28Z arango $
!=======================================================================
!  Copyright (c) 2002-2020 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                           Hernan G. Arango   !
!==================================================== John C. Warner ===
!                                                                      !
!  Master program to couple ROMS/TOMS to other models using the Model  !
!  Coupling Toolkit (MCT) library.                                     !
!                                                                      !
!  The following models are coupled to ROMS/TOMS:                      !
!                                                                      !
!  WRF, Weather Research and Forecasting model:                        !
!       http://www.wrf-model.org                                       !
!                                                                      !
!=======================================================================
!
      USE mod_iounits
      USE mod_scalars
      USE module_wrf_top, ONLY : wrf_init
      USE module_wrf_top, ONLY : wrf_run
      USE module_wrf_top, ONLY : wrf_finalize
      USE mct_coupler_params
      USE mod_coupler_iounits
!
      USE m_MCTWorld, ONLY : MCTWorld_clean => clean
      USE ocean_control_mod, ONLY : ROMS_initialize
      USE ocean_control_mod, ONLY : ROMS_run
      USE ocean_control_mod, ONLY : ROMS_finalize
      USE ocean_coupler_mod, ONLY : finalize_ocn2atm_coupling
!
      implicit none
      include 'mpif.h'
!
!  Local variable declarations.
!
      logical, save :: first
      integer :: MyColor, MyCOMM, MyError, MyKey, Nnodes
      integer :: MyRank, pelast
      integer :: Ocncolor, Wavcolor, Atmcolor, Hydcolor, IOMcolor
	integer :: ng, iw, io, ia, ih, icc
      real(m8) :: lcm, gcdlcm
      real(m4) :: CouplingTime             ! single precision
!
!-----------------------------------------------------------------------
!  Initialize distributed-memory (1) configuration
!-----------------------------------------------------------------------
!
!  Initialize 1 execution environment.
!
      CALL mpi_init (MyError)
!      print*, "RENATO"
!
!  Get rank of the local process in the group associated with the
!  comminicator.
!
      CALL mpi_comm_size (MPI_COMM_WORLD, Nnodes, MyError)
      CALL mpi_comm_rank (MPI_COMM_WORLD, MyRank, MyError)
!
!  Read in coupled model parameters from standard input.
!
      CALL read_coawst_par(1)
!
!  Now that we know the input file names and locations for each model,
!  for each model read in the number of grids and the grid time steps.
!
      CALL read_model_inputs
!
      CALL allocate_coupler_params
!
!
!  Read coupled model sparse matrix file names from standard input.
!
        CALL allocate_coupler_iounits
        CALL read_coawst_par(2)
!
!  To find out if we have any moving wrf grids, get wrf dst sizes.
!  This will be compared to wrf actual grid size in mc_wrf_coupler_params.
!
        CALL get_wrf_moving_grids
!
!  Compute the mct send and recv instances.
!
!  For each model grid, determine the number of steps it should
!  compute before it sends data out.
!  For example, nWAV2OCN(1,2) is the number of steps the wave model
!  grid 1 should take before it sends data to the ocn grid 2.
!
      DO ia=1,Natm_grids
        DO io=1,Nocn_grids
          lcm=gcdlcm(dtatm(ia),dtocn(io))
          IF (MOD(TI_ATM2OCN,lcm).eq.0) THEN
            nATM2OCN(ia,io)=INT(TI_ATM2OCN/dtatm(ia))
          ELSE
            lcm=gcdlcm(TI_ATM2OCN,lcm)
            nATM2OCN(ia,io)=INT(lcm/dtatm(ia))
          END IF
        END DO
      END DO
!
      DO io=1,Nocn_grids
        DO ia=1,Natm_grids
          lcm=gcdlcm(dtatm(ia),dtocn(io))
          IF (MOD(TI_OCN2ATM,lcm).eq.0) THEN
            nOCN2ATM(io,ia)=INT(TI_OCN2ATM/dtocn(io))
          ELSE
            lcm=gcdlcm(TI_OCN2ATM,lcm)
            nOCN2ATM(io,ia)=INT(lcm/dtocn(io))
          END IF
        END DO
      END DO
!
!  Similarly, for each model grid, determine the number of steps
!  it should compute before it recvs data from somewhere.
!  For example, nWAVFOCN(1,2) is the number of steps the wave model
!  grid 1 should take before it gets data from ocn grid 2.
!
      DO ia=1,Natm_grids
        DO io=1,Nocn_grids
          lcm=gcdlcm(dtatm(ia),dtocn(io))
          IF (MOD(TI_OCN2ATM,lcm).eq.0) THEN
            nATMFOCN(ia,io)=INT(TI_OCN2ATM/dtatm(ia))
          ELSE
            lcm=gcdlcm(TI_OCN2ATM,lcm)
            nATMFOCN(ia,io)=INT(lcm/dtatm(ia))
          END IF
        END DO
      END DO
!
      DO io=1,Nocn_grids
        DO ia=1,Natm_grids
          lcm=gcdlcm(dtatm(ia),dtocn(io))
          IF (MOD(TI_ATM2OCN,lcm).eq.0) THEN
            nOCNFATM(io,ia)=INT(TI_ATM2OCN/dtocn(io))
          ELSE
            lcm=gcdlcm(TI_ATM2OCN,lcm)
            nOCNFATM(io,ia)=INT(lcm/dtocn(io))
          END IF
        END DO
      END DO
!
!  Allocate several coupling variables.
!
      allocate(ocnids(Nocn_grids))
      allocate(atmids(Natm_grids))
!
      N_mctmodels=0
      DO ng=1,Nocn_grids
        N_mctmodels=N_mctmodels+1
        ocnids(ng)=N_mctmodels
      END DO
      DO ng=1,Natm_grids
        N_mctmodels=N_mctmodels+1
        atmids(ng)=N_mctmodels
     END DO
!
!  Assign processors to the models.
!
      pelast=-1
      peOCN_frst=pelast+1
      peOCN_last=peOCN_frst+NnodesOCN-1
      pelast=peOCN_last
      peATM_frst=pelast+1
      peATM_last=peATM_frst+NnodesATM-1
      pelast=peATM_last
      IF (pelast.ne.Nnodes-1) THEN
        IF (MyRank.eq.0) THEN
          WRITE (stdout,10) pelast+1, Nnodes
 10       FORMAT (/,' mct_coupler - Number assigned processors: '       &
     &            ,i3.3,/,15x,'not equal to spawned MPI nodes: ',i3.3)
        END IF
        STOP
      ELSE
        IF (MyRank.eq.0) THEN
          WRITE (stdout,19)
 19       FORMAT (/,' Model Coupling: ',/)
          WRITE (stdout,20) peOCN_frst, peOCN_last
 20       FORMAT (/,7x,'Ocean Model MPI nodes: ',i3.3,' - ', i3.3)
          WRITE (stdout,22) peATM_frst, peATM_last
 22       FORMAT (/,7x,'Atmos Model MPI nodes: ',i3.3,' - ', i3.3)
!
!  Write out some coupled model info.
!
        DO ia=1,Natm_grids
          DO io=1,Nocn_grids
            WRITE (stdout,27) ia, dtatm(ia),io, dtocn(io),                &
     &                        TI_ATM2OCN, nATM2OCN(ia,io)
 27         FORMAT (/,7x,'ATMgrid ',i2.2,' dt= ',f5.1,' -to- OCNgrid ',   &
     &              i2.2,' dt= ',f5.1,', CplInt: ',f7.1,' Steps: ',i3.3)
            WRITE (stdout,28) io, dtocn(io),ia, dtatm(ia),                &
     &                        TI_OCN2ATM, nOCN2ATM(io,ia)
 28         FORMAT (/,7x,'OCNgrid ',i2.2,' dt= ',f5.1,' -to- ATMgrid ',   &
     &              i2.2,' dt= ',f5.1,', CplInt: ',f7.1,' Steps: ',i3.3)
          END DO
        END DO
        END IF
      END IF
!     CALL flush_coawst (stdout)
!
!  Split the communicator into SWAN or WW3, WRF, and ROMS subgroups based
!  on color and key.
!
      Atmcolor=1
      Ocncolor=2
      Wavcolor=3
      Hydcolor=4
      IOMcolor=5  !CR
      MyKey=0
      IF ((peOCN_frst.le.MyRank).and.(MyRank.le.peOCN_last)) THEN
        MyColor=OCNcolor
      END IF
      IF ((peATM_frst.le.MyRank).and.(MyRank.le.peATM_last)) THEN
        MyColor=ATMcolor
      END IF
      CALL mpi_comm_split (MPI_COMM_WORLD, MyColor, MyKey, MyCOMM,      &
     &                     MyError)
!
!-----------------------------------------------------------------------
!  Run coupled models according to the processor rank.
!-----------------------------------------------------------------------
!
      IF (MyColor.eq.ATMcolor) THEN
        CALL wrf_init (MyCOMM)
        CALL wrf_run
        CALL wrf_finalize(.TRUE.)
      END IF
      IF (MyColor.eq.OCNcolor) THEN
        first=.TRUE.
        Nrun=1
        IF (exit_flag.eq.NoError) THEN
          CALL ROMS_initialize (first, MyCOMM)
        END IF
        IF (exit_flag.eq.NoError) THEN
          run_time=0.0_m8
          DO ng=1,Ngrids
            run_time=MAX(run_time, dt(ng)*ntimes(ng))
          END DO
          CALL ROMS_run (run_time)
        END IF
        CALL ROMS_finalize
        CALL finalize_ocn2atm_coupling
      END IF
!
!-----------------------------------------------------------------------
!  Terminates all the mpi-processing and coupling.
!-----------------------------------------------------------------------
!
      CALL mpi_barrier (MPI_COMM_WORLD, MyError)
      CALL MCTWorld_clean ()
      CALL mpi_finalize (MyError)
      STOP
      END PROGRAM mct_driver
      FUNCTION gcdlcm (dtAin, dtBin)
!
!=======================================================================
!                                                                      !
!  This function computes the greatest common denominator              !
!  and lowest common multiple.                                         !
!                                                                      !
!  On Input:                                                           !
!     dtA        time step of model A                                  !
!     dtB        time step of model B                                  !
!                                                                      !
!  On Output:                                                          !
!     lcm        least common multiple                                 !
!                                                                      !
!=======================================================================
!
      USE mod_coupler_kinds
!
      implicit none
!
!  Imported variable declarations.
!
      real(m8), intent(in) :: dtAin, dtBin
      real(m8) :: gcdlcm
!
!  Local variable declarations.
!
      logical :: stayin
      real(m8) :: r, m, n, p, gcd, dtA, dtB, scale
      scale=1000.0_m8
!
!-----------------------------------------------------------------------
!  Compute greatest common denominator and least common multiplier.
!-----------------------------------------------------------------------
      dtA=dtAin*scale
      dtB=dtBin*scale
      m=dtA
      n=dtB
      IF (dtA.gt.dtB) THEN
        p=dtA
        dtA=dtB
        dtB=p
      END IF
      stayin=.true.
      DO WHILE (stayin)
        r=mod(dtB,dtA)
        IF (r.eq.0) THEN
          gcd=dtA
          stayin=.false.
        ELSE
          dtB=dtA
          dtA=r
        END IF
      END DO
      gcdlcm=m*n/dtA/scale
      RETURN
      END FUNCTION gcdlcm
