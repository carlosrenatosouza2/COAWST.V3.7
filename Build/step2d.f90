      MODULE step2d_mod
!
!svn $Id: step2d.F 1054 2021-03-06 19:47:12Z arango $
!=======================================================================
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license           Hernan G. Arango   !
!    See License_ROMS.txt                   Alexander F. Shchepetkin   !
!==================================================== John C. Warner ===
!                                                                      !
!  This subroutine performs a fast (predictor or corrector) time-step  !
!  for the free-surface  and 2D momentum nonlinear equations.
!  It also calculates the time filtering variables over all fast-time  !
!  steps  to damp high frequency signals in 3D applications.           !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: step2d
!
      CONTAINS
!
      SUBROUTINE step2d (ng, tile)
!
!svn $Id: step2d_LF_AM3.h 1054 2021-03-06 19:47:12Z arango $
!=======================================================================
!                                                                      !
!  Nonlinear shallow-water primitive equations predictor (Leap-frog)   !
!  and corrector (Adams-Moulton) time-stepping engine.                 !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_coupling
      USE mod_diags
      USE mod_forces
      USE mod_grid
      USE mod_mixing
      USE mod_ocean
      USE mod_stepping
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Nonlinear/step2d_LF_AM3.h"
!
      integer :: IminS, ImaxS, JminS, JmaxS
      integer :: LBi, UBi, LBj, UBj, LBij, UBij
!
!  Set horizontal starting and ending indices for automatic private
!  storage arrays.
!
      IminS=BOUNDS(ng)%Istr(tile)-3
      ImaxS=BOUNDS(ng)%Iend(tile)+3
      JminS=BOUNDS(ng)%Jstr(tile)-3
      JmaxS=BOUNDS(ng)%Jend(tile)+3
!
!  Determine array lower and upper bounds in the I- and J-directions.
!
      LBi=BOUNDS(ng)%LBi(tile)
      UBi=BOUNDS(ng)%UBi(tile)
      LBj=BOUNDS(ng)%LBj(tile)
      UBj=BOUNDS(ng)%UBj(tile)
!
!  Set array lower and upper bounds for MIN(I,J) directions and
!  MAX(I,J) directions.
!
      LBij=BOUNDS(ng)%LBij
      UBij=BOUNDS(ng)%UBij
!
      CALL wclock_on (ng, iNLM, 9, 50, MyFile)
      CALL step2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj, N(ng),                      &
     &                  IminS, ImaxS, JminS, JmaxS,                     &
     &                  krhs(ng), kstp(ng), knew(ng),                   &
     &                  nstp(ng), nnew(ng),                             &
     &                  GRID(ng) % pmask,       GRID(ng) % rmask,       &
     &                  GRID(ng) % umask,       GRID(ng) % vmask,       &
     &                  GRID(ng) % fomn,        GRID(ng) % h,           &
     &                  GRID(ng) % om_u,        GRID(ng) % om_v,        &
     &                  GRID(ng) % on_u,        GRID(ng) % on_v,        &
     &                  GRID(ng) % omn,                                 &
     &                  GRID(ng) % pm,          GRID(ng) % pn,          &
     &                  GRID(ng) % dndx,        GRID(ng) % dmde,        &
     &                  GRID(ng) % pmon_r,      GRID(ng) % pnom_r,      &
     &                  GRID(ng) % pmon_p,      GRID(ng) % pnom_p,      &
     &                  GRID(ng) % om_r,        GRID(ng) % on_r,        &
     &                  GRID(ng) % om_p,        GRID(ng) % on_p,        &
     &                  MIXING(ng) % visc2_p,   MIXING(ng) % visc2_r,   &
     &                  COUPLING(ng) % rhoA,    COUPLING(ng) % rhoS,    &
     &                  COUPLING(ng) % DU_avg1, COUPLING(ng) % DU_avg2, &
     &                  COUPLING(ng) % DV_avg1, COUPLING(ng) % DV_avg2, &
     &                  COUPLING(ng) % Zt_avg1,                         &
     &                  COUPLING(ng) % rufrc,   COUPLING(ng) % rvfrc,   &
     &                  OCEAN(ng) % ru,         OCEAN(ng) % rv,         &
     &                  DIAGS(ng) % DiaU2wrk,   DIAGS(ng) % DiaV2wrk,   &
     &                  DIAGS(ng) % DiaRUbar,   DIAGS(ng) % DiaRVbar,   &
     &                  DIAGS(ng) % DiaU2int,   DIAGS(ng) % DiaV2int,   &
     &                  DIAGS(ng) % DiaRUfrc,   DIAGS(ng) % DiaRVfrc,   &
     &                  OCEAN(ng) % rubar,      OCEAN(ng) % rvbar,      &
     &                  OCEAN(ng) % rzeta,                              &
     &                  OCEAN(ng) % ubar,       OCEAN(ng) % vbar,       &
     &                  OCEAN(ng) % zeta)
      CALL wclock_off (ng, iNLM, 9, 158, MyFile)
!
      RETURN
      END SUBROUTINE step2d
!
!***********************************************************************
      SUBROUTINE step2d_tile (ng, tile,                                 &
     &                        LBi, UBi, LBj, UBj, UBk,                  &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        krhs, kstp, knew,                         &
     &                        nstp, nnew,                               &
     &                        pmask, rmask, umask, vmask,               &
     &                        fomn, h,                                  &
     &                        om_u, om_v, on_u, on_v, omn, pm, pn,      &
     &                        dndx, dmde,                               &
     &                        pmon_r, pnom_r, pmon_p, pnom_p,           &
     &                        om_r, on_r, om_p, on_p,                   &
     &                        visc2_p, visc2_r,                         &
     &                        rhoA, rhoS,                               &
     &                        DU_avg1, DU_avg2,                         &
     &                        DV_avg1, DV_avg2,                         &
     &                        Zt_avg1,                                  &
     &                        rufrc, rvfrc, ru, rv,                     &
     &                        DiaU2wrk, DiaV2wrk,                       &
     &                        DiaRUbar, DiaRVbar,                       &
     &                        DiaU2int, DiaV2int,                       &
     &                        DiaRUfrc, DiaRVfrc,                       &
     &                        rubar, rvbar, rzeta,                      &
     &                        ubar,  vbar, zeta)
!***********************************************************************
!
      USE mod_param
      USE mod_clima
      USE mod_ncparam
      USE mod_scalars
      USE mod_sources
!
      USE exchange_2d_mod
      USE mp_exchange_mod, ONLY : mp_exchange2d
      USE obc_volcons_mod, ONLY : obc_flux_tile, set_DUV_bc_tile
      USE u2dbc_mod, ONLY : u2dbc_tile
      USE v2dbc_mod, ONLY : v2dbc_tile
      USE zetabc_mod, ONLY : zetabc_tile
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, UBk
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: krhs, kstp, knew
      integer, intent(in) :: nstp, nnew
!
      real(r8), intent(in) :: pmask(LBi:,LBj:)
      real(r8), intent(in) :: rmask(LBi:,LBj:)
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
      real(r8), intent(in) :: fomn(LBi:,LBj:)
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(in) :: om_u(LBi:,LBj:)
      real(r8), intent(in) :: om_v(LBi:,LBj:)
      real(r8), intent(in) :: on_u(LBi:,LBj:)
      real(r8), intent(in) :: on_v(LBi:,LBj:)
      real(r8), intent(in) :: omn(LBi:,LBj:)
      real(r8), intent(in) :: pm(LBi:,LBj:)
      real(r8), intent(in) :: pn(LBi:,LBj:)
      real(r8), intent(in) :: dndx(LBi:,LBj:)
      real(r8), intent(in) :: dmde(LBi:,LBj:)
      real(r8), intent(in) :: pmon_r(LBi:,LBj:)
      real(r8), intent(in) :: pnom_r(LBi:,LBj:)
      real(r8), intent(in) :: pmon_p(LBi:,LBj:)
      real(r8), intent(in) :: pnom_p(LBi:,LBj:)
      real(r8), intent(in) :: om_r(LBi:,LBj:)
      real(r8), intent(in) :: on_r(LBi:,LBj:)
      real(r8), intent(in) :: om_p(LBi:,LBj:)
      real(r8), intent(in) :: on_p(LBi:,LBj:)
      real(r8), intent(in) :: visc2_p(LBi:,LBj:)
      real(r8), intent(in) :: visc2_r(LBi:,LBj:)
      real(r8), intent(in) :: rhoA(LBi:,LBj:)
      real(r8), intent(in) :: rhoS(LBi:,LBj:)
      real(r8), intent(inout) :: DU_avg1(LBi:,LBj:)
      real(r8), intent(inout) :: DU_avg2(LBi:,LBj:)
      real(r8), intent(inout) :: DV_avg1(LBi:,LBj:)
      real(r8), intent(inout) :: DV_avg2(LBi:,LBj:)
      real(r8), intent(inout) :: Zt_avg1(LBi:,LBj:)
      real(r8), intent(inout) :: rufrc(LBi:,LBj:)
      real(r8), intent(inout) :: rvfrc(LBi:,LBj:)
      real(r8), intent(inout) :: ru(LBi:,LBj:,0:,:)
      real(r8), intent(inout) :: rv(LBi:,LBj:,0:,:)
      real(r8), intent(inout) :: DiaU2wrk(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaV2wrk(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaRUbar(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaRVbar(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaU2int(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaV2int(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaRUfrc(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaRVfrc(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: rubar(LBi:,LBj:,:)
      real(r8), intent(inout) :: rvbar(LBi:,LBj:,:)
      real(r8), intent(inout) :: rzeta(LBi:,LBj:,:)
      real(r8), intent(inout) :: ubar(LBi:,LBj:,:)
      real(r8), intent(inout) :: vbar(LBi:,LBj:,:)
      real(r8), intent(inout) :: zeta(LBi:,LBj:,:)
!
!  Local variable declarations.
!
      logical :: CORRECTOR_2D_STEP
      integer :: i, is, j, ptsk, k
      integer :: idiag
      real(r8) :: cff, cff1, cff2, cff3, cff4
      real(r8) :: cff5, cff6, cff7, cff8
      real(r8) :: fac, fac1, fac2, fac3
!
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Dgrad
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Dnew
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Drhs
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Drhs_p
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Dstp
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DUon
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DVom
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: UFe
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: UFx
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: VFe
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: VFx
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: grad
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: gzeta
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: gzeta2
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: gzetaSA
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: rhs_ubar
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: rhs_vbar
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: rhs_zeta
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: zeta_new
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: zwrk
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Uwrk
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Vwrk
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,NDM2d-1) :: DiaU2rhs
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,NDM2d-1) :: DiaV2rhs
!
!-----------------------------------------------------------------------
!  Set lower and upper tile bounds and staggered variables bounds for
!  this horizontal domain partition.  Notice that if tile=-1, it will
!  set the values for the global grid.
!-----------------------------------------------------------------------
!
      integer :: Istr, IstrB, IstrP, IstrR, IstrT, IstrM, IstrU
      integer :: Iend, IendB, IendP, IendR, IendT
      integer :: Jstr, JstrB, JstrP, JstrR, JstrT, JstrM, JstrV
      integer :: Jend, JendB, JendP, JendR, JendT
      integer :: Istrm3, Istrm2, Istrm1, IstrUm2, IstrUm1
      integer :: Iendp1, Iendp2, Iendp2i, Iendp3
      integer :: Jstrm3, Jstrm2, Jstrm1, JstrVm2, JstrVm1
      integer :: Jendp1, Jendp2, Jendp2i, Jendp3
!
      Istr   =BOUNDS(ng) % Istr   (tile)
      IstrB  =BOUNDS(ng) % IstrB  (tile)
      IstrM  =BOUNDS(ng) % IstrM  (tile)
      IstrP  =BOUNDS(ng) % IstrP  (tile)
      IstrR  =BOUNDS(ng) % IstrR  (tile)
      IstrT  =BOUNDS(ng) % IstrT  (tile)
      IstrU  =BOUNDS(ng) % IstrU  (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      IendB  =BOUNDS(ng) % IendB  (tile)
      IendP  =BOUNDS(ng) % IendP  (tile)
      IendR  =BOUNDS(ng) % IendR  (tile)
      IendT  =BOUNDS(ng) % IendT  (tile)
      Jstr   =BOUNDS(ng) % Jstr   (tile)
      JstrB  =BOUNDS(ng) % JstrB  (tile)
      JstrM  =BOUNDS(ng) % JstrM  (tile)
      JstrP  =BOUNDS(ng) % JstrP  (tile)
      JstrR  =BOUNDS(ng) % JstrR  (tile)
      JstrT  =BOUNDS(ng) % JstrT  (tile)
      JstrV  =BOUNDS(ng) % JstrV  (tile)
      Jend   =BOUNDS(ng) % Jend   (tile)
      JendB  =BOUNDS(ng) % JendB  (tile)
      JendP  =BOUNDS(ng) % JendP  (tile)
      JendR  =BOUNDS(ng) % JendR  (tile)
      JendT  =BOUNDS(ng) % JendT  (tile)
!
      Istrm3 =BOUNDS(ng) % Istrm3 (tile)            ! Istr-3
      Istrm2 =BOUNDS(ng) % Istrm2 (tile)            ! Istr-2
      Istrm1 =BOUNDS(ng) % Istrm1 (tile)            ! Istr-1
      IstrUm2=BOUNDS(ng) % IstrUm2(tile)            ! IstrU-2
      IstrUm1=BOUNDS(ng) % IstrUm1(tile)            ! IstrU-1
      Iendp1 =BOUNDS(ng) % Iendp1 (tile)            ! Iend+1
      Iendp2 =BOUNDS(ng) % Iendp2 (tile)            ! Iend+2
      Iendp2i=BOUNDS(ng) % Iendp2i(tile)            ! Iend+2 interior
      Iendp3 =BOUNDS(ng) % Iendp3 (tile)            ! Iend+3
      Jstrm3 =BOUNDS(ng) % Jstrm3 (tile)            ! Jstr-3
      Jstrm2 =BOUNDS(ng) % Jstrm2 (tile)            ! Jstr-2
      Jstrm1 =BOUNDS(ng) % Jstrm1 (tile)            ! Jstr-1
      JstrVm2=BOUNDS(ng) % JstrVm2(tile)            ! JstrV-2
      JstrVm1=BOUNDS(ng) % JstrVm1(tile)            ! JstrV-1
      Jendp1 =BOUNDS(ng) % Jendp1 (tile)            ! Jend+1
      Jendp2 =BOUNDS(ng) % Jendp2 (tile)            ! Jend+2
      Jendp2i=BOUNDS(ng) % Jendp2i(tile)            ! Jend+2 interior
      Jendp3 =BOUNDS(ng) % Jendp3 (tile)            ! Jend+3
!
      ptsk=3-kstp
      CORRECTOR_2D_STEP=.not.PREDICTOR_2D_STEP(ng)
!
!-----------------------------------------------------------------------
!  Compute total depth (m) and vertically integrated mass fluxes.
!-----------------------------------------------------------------------
!
!  In distributed-memory, the I- and J-ranges are different and a
!  special exchange is done to avoid having three ghost points for
!  high order numerical stencils. Notice that a private array is
!  passed below to the exchange routine. It also applies periodic
!  boundary conditions, if appropriate and no partitions in I- or
!  J-directions.
!
      DO j=JstrV-2,Jendp2
        DO i=IstrU-2,Iendp2
          Drhs(i,j)=zeta(i,j,krhs)+h(i,j)
        END DO
      END DO
      DO j=JstrV-2,Jendp2
        DO i=IstrU-1,Iendp2
          cff=0.5_r8*on_u(i,j)
          cff1=cff*(Drhs(i,j)+Drhs(i-1,j))
          DUon(i,j)=ubar(i,j,krhs)*cff1
        END DO
      END DO
      DO j=JstrV-1,Jendp2
        DO i=IstrU-2,Iendp2
          cff=0.5_r8*om_v(i,j)
          cff1=cff*(Drhs(i,j)+Drhs(i,j-1))
          DVom(i,j)=vbar(i,j,krhs)*cff1
        END DO
      END DO
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          DUon)
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          DVom)
      END IF
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    IminS, ImaxS, JminS, JmaxS,                   &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    DUon, DVom)
!
!  Set vertically integrated mass fluxes DUon and DVom along the open
!  boundaries in such a way that the integral volume is conserved.
!
      IF (ANY(VolCons(:,ng))) THEN
        CALL set_DUV_bc_tile (ng, tile,                                 &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        krhs,                                     &
     &                        umask, vmask,                             &
     &                        om_v, on_u,                               &
     &                        ubar, vbar,                               &
     &                        Drhs, DUon, DVom)
      END IF
!
!-----------------------------------------------------------------------
!  Compute time averaged fields over all short time-steps.
!-----------------------------------------------------------------------
!
      IF (PREDICTOR_2D_STEP(ng)) THEN
        IF (iif(ng).eq.1) THEN
!
!  Reset arrays for 2D fields averaged within the short time-steps.
!
          cff2=(-1.0_r8/12.0_r8)*weight(2,iif(ng)+1,ng)
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              Zt_avg1(i,j)=0.0_r8
            END DO
            DO i=Istr,IendR
              DU_avg1(i,j)=0.0_r8
              DU_avg2(i,j)=cff2*DUon(i,j)
            END DO
          END DO
          DO j=Jstr,JendR
            DO i=IstrR,IendR
              DV_avg1(i,j)=0.0_r8
              DV_avg2(i,j)=cff2*DVom(i,j)
            END DO
          END DO
        ELSE
!
!  Accumulate field averages of previous time-step after they are
!  computed in the previous corrector step, updated their boundaries,
!  and synchronized.
!
          cff1=weight(1,iif(ng)-1,ng)
          cff2=(8.0_r8/12.0_r8)*weight(2,iif(ng)  ,ng)-                 &
     &         (1.0_r8/12.0_r8)*weight(2,iif(ng)+1,ng)
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              Zt_avg1(i,j)=Zt_avg1(i,j)+cff1*zeta(i,j,krhs)
            END DO
            DO i=Istr,IendR
              DU_avg1(i,j)=DU_avg1(i,j)+cff1*DUon(i,j)
              DU_avg2(i,j)=DU_avg2(i,j)+cff2*DUon(i,j)
            END DO
          END DO
          DO j=Jstr,JendR
            DO i=IstrR,IendR
              DV_avg1(i,j)=DV_avg1(i,j)+cff1*DVom(i,j)
              DV_avg2(i,j)=DV_avg2(i,j)+cff2*DVom(i,j)
            END DO
          END DO
        END IF
      ELSE
        IF (iif(ng).eq.1) THEN
          cff2=weight(2,iif(ng),ng)
        ELSE
          cff2=(5.0_r8/12.0_r8)*weight(2,iif(ng),ng)
        END IF
        DO j=JstrR,JendR
          DO i=Istr,IendR
            DU_avg2(i,j)=DU_avg2(i,j)+cff2*DUon(i,j)
          END DO
        END DO
        DO j=Jstr,JendR
          DO i=IstrR,IendR
            DV_avg2(i,j)=DV_avg2(i,j)+cff2*DVom(i,j)
          END DO
        END DO
      END IF
!
!  After all fast time steps are completed, apply boundary conditions
!  to time averaged fields.
!
      IF ((iif(ng).eq.(nfast(ng)+1)).and.PREDICTOR_2D_STEP(ng)) THEN
        IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
          CALL exchange_r2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            Zt_avg1)
          CALL exchange_u2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            DU_avg1)
          CALL exchange_v2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            DV_avg1)
        END IF
        CALL mp_exchange2d (ng, tile, iNLM, 3,                          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      NghostPoints,                               &
     &                      EWperiodic(ng), NSperiodic(ng),             &
     &                      Zt_avg1, DU_avg1, DV_avg1)
      END IF
!
!  Do not perform the actual time stepping during the auxiliary
!  (nfast(ng)+1) time step.
!
      IF (iif(ng).gt.nfast(ng)) RETURN
!
!=======================================================================
!  Time step free-surface equation.
!=======================================================================
!
!  During the first time-step, the predictor step is Forward-Euler
!  and the corrector step is Backward-Euler. Otherwise, the predictor
!  step is Leap-frog and the corrector step is Adams-Moulton.
!
      fac=1000.0_r8/rho0
      IF (iif(ng).eq.1) THEN
        cff1=dtfast(ng)
        DO j=JstrV-1,Jend
          DO i=IstrU-1,Iend
            rhs_zeta(i,j)=(DUon(i,j)-DUon(i+1,j))+                      &
     &                    (DVom(i,j)-DVom(i,j+1))
            zeta_new(i,j)=zeta(i,j,kstp)+                               &
     &                    pm(i,j)*pn(i,j)*cff1*rhs_zeta(i,j)
            zeta_new(i,j)=zeta_new(i,j)*rmask(i,j)
            Dnew(i,j)=zeta_new(i,j)+h(i,j)
!
            zwrk(i,j)=0.5_r8*(zeta(i,j,kstp)+zeta_new(i,j))
            gzeta(i,j)=(fac+rhoS(i,j))*zwrk(i,j)
            gzeta2(i,j)=gzeta(i,j)*zwrk(i,j)
            gzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
          END DO
        END DO
      ELSE IF (PREDICTOR_2D_STEP(ng)) THEN
        cff1=2.0_r8*dtfast(ng)
        cff4=4.0_r8/25.0_r8
        cff5=1.0_r8-2.0_r8*cff4
        DO j=JstrV-1,Jend
          DO i=IstrU-1,Iend
            rhs_zeta(i,j)=(DUon(i,j)-DUon(i+1,j))+                      &
     &                    (DVom(i,j)-DVom(i,j+1))
            zeta_new(i,j)=zeta(i,j,kstp)+                               &
     &                    pm(i,j)*pn(i,j)*cff1*rhs_zeta(i,j)
            zeta_new(i,j)=zeta_new(i,j)*rmask(i,j)
            Dnew(i,j)=zeta_new(i,j)+h(i,j)
!
            zwrk(i,j)=cff5*zeta(i,j,krhs)+                              &
     &                cff4*(zeta(i,j,kstp)+zeta_new(i,j))
            gzeta(i,j)=(fac+rhoS(i,j))*zwrk(i,j)
            gzeta2(i,j)=gzeta(i,j)*zwrk(i,j)
            gzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
          END DO
        END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=dtfast(ng)*5.0_r8/12.0_r8
        cff2=dtfast(ng)*8.0_r8/12.0_r8
        cff3=dtfast(ng)*1.0_r8/12.0_r8
        cff4=2.0_r8/5.0_r8
        cff5=1.0_r8-cff4
        DO j=JstrV-1,Jend
          DO i=IstrU-1,Iend
            cff=cff1*((DUon(i,j)-DUon(i+1,j))+                          &
     &                (DVom(i,j)-DVom(i,j+1)))
            zeta_new(i,j)=zeta(i,j,kstp)+                               &
     &                    pm(i,j)*pn(i,j)*(cff+                         &
     &                                     cff2*rzeta(i,j,kstp)-        &
     &                                     cff3*rzeta(i,j,ptsk))
            zeta_new(i,j)=zeta_new(i,j)*rmask(i,j)
            Dnew(i,j)=zeta_new(i,j)+h(i,j)
!
            zwrk(i,j)=cff5*zeta_new(i,j)+cff4*zeta(i,j,krhs)
            gzeta(i,j)=(fac+rhoS(i,j))*zwrk(i,j)
            gzeta2(i,j)=gzeta(i,j)*zwrk(i,j)
            gzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
          END DO
        END DO
      END IF
!
!  Load new free-surface values into shared array at both predictor
!  and corrector steps.
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
          zeta(i,j,knew)=zeta_new(i,j)
        END DO
      END DO
!
!  If predictor step, load right-side-term into shared array.
!
      IF (PREDICTOR_2D_STEP(ng)) THEN
        DO j=Jstr,Jend
          DO i=Istr,Iend
            rzeta(i,j,krhs)=rhs_zeta(i,j)
          END DO
        END DO
        IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
          CALL exchange_r2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            rzeta(:,:,krhs))
        END IF
        CALL mp_exchange2d (ng, tile, iNLM, 1,                          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      NghostPoints,                               &
     &                      EWperiodic(ng), NSperiodic(ng),             &
     &                      rzeta(:,:,krhs))
      END IF
!
!  Apply mass point sources (volume vertical influx), if any.
!
      IF (LwSrc(ng)) THEN
        DO is=1,Nsrc(ng)
          i=SOURCES(ng)%Isrc(is)
          j=SOURCES(ng)%Jsrc(is)
          IF (((IstrR.le.i).and.(i.le.IendR)).and.                      &
     &        ((JstrR.le.j).and.(j.le.JendR))) THEN
            zeta(i,j,knew)=zeta(i,j,knew)+                              &
     &                     SOURCES(ng)%Qbar(is)*                        &
     &                     pm(i,j)*pn(i,j)*dtfast(ng)
          END IF
        END DO
      END IF
!
!  Set free-surface lateral boundary conditions.
!
      CALL zetabc_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  IminS, ImaxS, JminS, JmaxS,                     &
     &                  krhs, kstp, knew,                               &
     &                  zeta)
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          zeta(:,:,knew))
      END IF
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    zeta(:,:,knew))
!
!=======================================================================
!  Compute right-hand-side for the 2D momentum equations.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Compute pressure gradient terms.
!-----------------------------------------------------------------------
!
      cff1=0.5_r8*g
      cff2=1.0_r8/3.0_r8
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          rhs_ubar(i,j)=cff1*on_u(i,j)*                                 &
     &                  ((h(i-1,j)+                                     &
     &                    h(i ,j))*                                     &
     &                   (gzeta(i-1,j)-                                 &
     &                    gzeta(i  ,j))+                                &
     &                   (h(i-1,j)-                                     &
     &                    h(i  ,j))*                                    &
     &                   (gzetaSA(i-1,j)+                               &
     &                    gzetaSA(i  ,j)+                               &
     &                    cff2*(rhoA(i-1,j)-                            &
     &                          rhoA(i  ,j))*                           &
     &                         (zwrk(i-1,j)-                            &
     &                          zwrk(i  ,j)))+                          &
     &                   (gzeta2(i-1,j)-                                &
     &                    gzeta2(i  ,j)))
          DiaU2rhs(i,j,M2pgrd)=rhs_ubar(i,j)
        END DO
        IF (j.ge.JstrV) THEN
          DO i=Istr,Iend
            rhs_vbar(i,j)=cff1*om_v(i,j)*                               &
     &                    ((h(i,j-1)+                                   &
     &                      h(i,j  ))*                                  &
     &                     (gzeta(i,j-1)-                               &
     &                      gzeta(i,j  ))+                              &
     &                     (h(i,j-1)-                                   &
     &                      h(i,j  ))*                                  &
     &                     (gzetaSA(i,j-1)+                             &
     &                      gzetaSA(i,j  )+                             &
     &                      cff2*(rhoA(i,j-1)-                          &
     &                            rhoA(i,j  ))*                         &
     &                           (zwrk(i,j-1)-                          &
     &                            zwrk(i,j  )))+                        &
     &                     (gzeta2(i,j-1)-                              &
     &                      gzeta2(i,j  )))
            DiaV2rhs(i,j,M2pgrd)=rhs_vbar(i,j)
          END DO
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Add in horizontal advection of momentum.
!-----------------------------------------------------------------------
!
!  Fourth-order, centered differences advection.
!
      DO j=Jstr,Jend
        DO i=IstrUm1,Iendp1
          grad (i,j)=ubar(i-1,j,krhs)-2.0_r8*ubar(i,j,krhs)+            &
     &               ubar(i+1,j,krhs)
          Dgrad(i,j)=DUon(i-1,j)-2.0_r8*DUon(i,j)+DUon(i+1,j)
        END DO
      END DO
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=Jstr,Jend
            grad (Istr,j)=grad (Istr+1,j)
            Dgrad(Istr,j)=Dgrad(Istr+1,j)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=Jstr,Jend
            grad (Iend+1,j)=grad (Iend,j)
            Dgrad(Iend+1,j)=Dgrad(Iend,j)
          END DO
        END IF
      END IF
      cff=1.0_r8/6.0_r8
      DO j=Jstr,Jend
        DO i=IstrU-1,Iend
          UFx(i,j)=0.25_r8*(ubar(i  ,j,krhs)+                           &
     &                      ubar(i+1,j,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i+1,j)))*             &
     &                     (DUon(i,j)+DUon(i+1,j)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i+1,j)))
        END DO
      END DO
!
      DO j=Jstrm1,Jendp1
        DO i=IstrU,Iend
          grad(i,j)=ubar(i,j-1,krhs)-2.0_r8*ubar(i,j,krhs)+             &
     &              ubar(i,j+1,krhs)
        END DO
      END DO
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=IstrU,Iend
            grad(i,Jstr-1)=grad(i,Jstr)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=IstrU,Iend
            grad(i,Jend+1)=grad(i,Jend)
          END DO
        END IF
      END IF
      DO j=Jstr,Jend+1
        DO i=IstrU-1,Iend
          Dgrad(i,j)=DVom(i-1,j)-2.0_r8*DVom(i,j)+DVom(i+1,j)
        END DO
      END DO
      cff=1.0_r8/6.0_r8
      DO j=Jstr,Jend+1
        DO i=IstrU,Iend
          UFe(i,j)=0.25_r8*(ubar(i,j  ,krhs)+                           &
     &                      ubar(i,j-1,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i,j-1)))*             &
     &                     (DVom(i,j)+DVom(i-1,j)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i-1,j)))
        END DO
      END DO
!
      DO j=JstrV,Jend
        DO i=Istrm1,Iendp1
          grad(i,j)=vbar(i-1,j,krhs)-2.0_r8*vbar(i,j,krhs)+             &
     &              vbar(i+1,j,krhs)
        END DO
      END DO
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=JstrV,Jend
            grad(Istr-1,j)=grad(Istr,j)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=JstrV,Jend
            grad(Iend+1,j)=grad(Iend,j)
          END DO
        END IF
      END IF
      DO j=JstrV-1,Jend
        DO i=Istr,Iend+1
          Dgrad(i,j)=DUon(i,j-1)-2.0_r8*DUon(i,j)+DUon(i,j+1)
        END DO
      END DO
      cff=1.0_r8/6.0_r8
      DO j=JstrV,Jend
        DO i=Istr,Iend+1
          VFx(i,j)=0.25_r8*(vbar(i  ,j,krhs)+                           &
     &                      vbar(i-1,j,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i-1,j)))*             &
     &                     (DUon(i,j)+DUon(i,j-1)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i,j-1)))
        END DO
      END DO
!
      DO j=JstrVm1,Jendp1
        DO i=Istr,Iend
          grad(i,j)=vbar(i,j-1,krhs)-2.0_r8*vbar(i,j,krhs)+             &
     &              vbar(i,j+1,krhs)
          Dgrad(i,j)=DVom(i,j-1)-2.0_r8*DVom(i,j)+DVom(i,j+1)
        END DO
      END DO
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=Istr,Iend
            grad (i,Jstr)=grad (i,Jstr+1)
            Dgrad(i,Jstr)=Dgrad(i,Jstr+1)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=Istr,Iend
            grad (i,Jend+1)=grad (i,Jend)
            Dgrad(i,Jend+1)=Dgrad(i,Jend)
          END DO
        END IF
      END IF
      cff=1.0_r8/6.0_r8
      DO j=JstrV-1,Jend
        DO i=Istr,Iend
          VFe(i,j)=0.25_r8*(vbar(i,j  ,krhs)+                           &
     &                      vbar(i,j+1,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i,j+1)))*             &
     &                     (DVom(i,j)+DVom(i,j+1)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i,j+1)))
        END DO
      END DO
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff1=UFx(i,j)-UFx(i-1,j)
          cff2=UFe(i,j+1)-UFe(i,j)
          fac=cff1+cff2
          rhs_ubar(i,j)=rhs_ubar(i,j)-fac
          DiaU2rhs(i,j,M2xadv)=-cff1
          DiaU2rhs(i,j,M2yadv)=-cff2
          DiaU2rhs(i,j,M2hadv)=-fac
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          cff1=VFx(i+1,j)-VFx(i,j)
          cff2=VFe(i,j)-VFe(i,j-1)
          fac=cff1+cff2
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac
          DiaV2rhs(i,j,M2xadv)=-cff1
          DiaV2rhs(i,j,M2yadv)=-cff2
          DiaV2rhs(i,j,M2hadv)=-fac
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Add in Coriolis term.
!-----------------------------------------------------------------------
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff=0.5_r8*Drhs(i,j)*fomn(i,j)
          UFx(i,j)=cff*(vbar(i,j  ,krhs)+                               &
     &                  vbar(i,j+1,krhs))
          VFe(i,j)=cff*(ubar(i  ,j,krhs)+                               &
     &                  ubar(i+1,j,krhs))
        END DO
      END DO
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac1=0.5_r8*(UFx(i,j)+UFx(i-1,j))
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac1
          DiaU2rhs(i,j,M2fcor)=fac1
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac1=0.5_r8*(VFe(i,j)+VFe(i,j-1))
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac1
          DiaV2rhs(i,j,M2fcor)=-fac1
        END DO
      END DO
!
!
!-----------------------------------------------------------------------
!  Add in curvilinear transformation terms.
!-----------------------------------------------------------------------
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff1=0.5_r8*(vbar(i,j  ,krhs)+                                &
     &                 vbar(i,j+1,krhs))
          cff2=0.5_r8*(ubar(i  ,j,krhs)+                                &
     &                 ubar(i+1,j,krhs))
          cff3=cff1*dndx(i,j)
          cff4=cff2*dmde(i,j)
          cff=Drhs(i,j)*(cff3-cff4)
          UFx(i,j)=cff*cff1
          VFe(i,j)=cff*cff2
          cff=Drhs(i,j)*cff4
          Uwrk(i,j)=-cff*cff1                  ! ubar equation, ETA-term
          Vwrk(i,j)=-cff*cff2                  ! vbar equation, ETA-term
        END DO
      END DO
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac1=0.5_r8*(UFx(i,j)+UFx(i-1,j))
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac1
          fac2=0.5_r8*(Uwrk(i,j)+Uwrk(i-1,j))
          DiaU2rhs(i,j,M2xadv)=DiaU2rhs(i,j,M2xadv)+fac1-fac2
          DiaU2rhs(i,j,M2yadv)=DiaU2rhs(i,j,M2yadv)+fac2
          DiaU2rhs(i,j,M2hadv)=DiaU2rhs(i,j,M2hadv)+fac1
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac1=0.5_r8*(VFe(i,j)+VFe(i,j-1))
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac1
          fac2=0.5_r8*(Vwrk(i,j)+Vwrk(i,j-1))
          DiaV2rhs(i,j,M2xadv)=DiaV2rhs(i,j,M2xadv)-fac1+fac2
          DiaV2rhs(i,j,M2yadv)=DiaV2rhs(i,j,M2yadv)-fac2
          DiaV2rhs(i,j,M2hadv)=DiaV2rhs(i,j,M2hadv)-fac1
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  If horizontal mixing, compute total depth at PSI-points.
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend+1
        DO i=Istr,Iend+1
          Drhs_p(i,j)=0.25_r8*(Drhs(i,j  )+Drhs(i-1,j  )+               &
     &                         Drhs(i,j-1)+Drhs(i-1,j-1))
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Add in horizontal harmonic viscosity.
!-----------------------------------------------------------------------
!
!  Compute flux-components of the horizontal divergence of the stress
!  tensor (m5/s2) in XI- and ETA-directions.
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff=visc2_r(i,j)*Drhs(i,j)*0.5_r8*                            &
     &        (pmon_r(i,j)*                                             &
     &         ((pn(i  ,j)+pn(i+1,j))*ubar(i+1,j,krhs)-                 &
     &          (pn(i-1,j)+pn(i  ,j))*ubar(i  ,j,krhs))-                &
     &         pnom_r(i,j)*                                             &
     &         ((pm(i,j  )+pm(i,j+1))*vbar(i,j+1,krhs)-                 &
     &          (pm(i,j-1)+pm(i,j  ))*vbar(i,j  ,krhs)))
          UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
          VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
        END DO
      END DO
      DO j=Jstr,Jend+1
        DO i=Istr,Iend+1
          cff=visc2_p(i,j)*Drhs_p(i,j)*0.5_r8*                          &
     &        (pmon_p(i,j)*                                             &
     &         ((pn(i  ,j-1)+pn(i  ,j))*vbar(i  ,j,krhs)-               &
     &          (pn(i-1,j-1)+pn(i-1,j))*vbar(i-1,j,krhs))+              &
     &         pnom_p(i,j)*                                             &
     &         ((pm(i-1,j  )+pm(i,j  ))*ubar(i,j  ,krhs)-               &
     &          (pm(i-1,j-1)+pm(i,j-1))*ubar(i,j-1,krhs)))
          cff=cff*pmask(i,j)
          UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
          VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
        END DO
      END DO
!
!  Add in harmonic viscosity.
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff1=0.5_r8*(pn(i-1,j)+pn(i,j))*(UFx(i,j  )-UFx(i-1,j))
          cff2=0.5_r8*(pm(i-1,j)+pm(i,j))*(UFe(i,j+1)-UFe(i  ,j))
          fac=cff1+cff2
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac
          DiaU2rhs(i,j,M2hvis)=fac
          DiaU2rhs(i,j,M2xvis)=cff1
          DiaU2rhs(i,j,M2yvis)=cff2
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          cff1=0.5_r8*(pn(i,j-1)+pn(i,j))*(VFx(i+1,j)-VFx(i,j  ))
          cff2=0.5_r8*(pm(i,j-1)+pm(i,j))*(VFe(i  ,j)-VFe(i,j-1))
          fac=cff1-cff2
          rhs_vbar(i,j)=rhs_vbar(i,j)+fac
          DiaV2rhs(i,j,M2hvis)=fac
          DiaV2rhs(i,j,M2xvis)= cff1
          DiaV2rhs(i,j,M2yvis)=-cff2
        END DO
      END DO
!
!  Initialize the stress term if no bottom friction is defined.
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          DiaU2rhs(i,j,M2bstr)=0.0_r8
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          DiaV2rhs(i,j,M2bstr)=0.0_r8
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Add in nudging of 2D momentum climatology.
!-----------------------------------------------------------------------
!
      IF (LnudgeM2CLM(ng)) THEN
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=0.25_r8*(CLIMA(ng)%M2nudgcof(i-1,j)+                    &
     &                   CLIMA(ng)%M2nudgcof(i  ,j))*                   &
     &          om_u(i,j)*on_u(i,j)
            rhs_ubar(i,j)=rhs_ubar(i,j)+                                &
     &                    cff*(Drhs(i-1,j)+Drhs(i,j))*                  &
     &                        (CLIMA(ng)%ubarclm(i,j)-                  &
     &                         ubar(i,j,krhs))
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=0.25_r8*(CLIMA(ng)%M2nudgcof(i,j-1)+                    &
     &                   CLIMA(ng)%M2nudgcof(i,j  ))*                   &
     &          om_v(i,j)*on_v(i,j)
            rhs_vbar(i,j)=rhs_vbar(i,j)+                                &
     &                    cff*(Drhs(i,j-1)+Drhs(i,j))*                  &
     &                        (CLIMA(ng)%vbarclm(i,j)-                  &
     &                         vbar(i,j,krhs))
          END DO
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Coupling between 2D and 3D equations.
!-----------------------------------------------------------------------
!
!  Before the predictor step of the first barotropic time-step,
!  arrays "rufrc" and "rvfrc" contain the vertical integrals of
!  the 3D right-hand-side terms for momentum equations (including
!  surface and bottom stresses, if so prescribed).
!
!  Convert them into forcing terms by subtracting the fast time
!  "rhs_ubar" and "rhs_vbar" from them; Also, immediately apply
!  these forcing terms "rhs_ubar" and "rhs_vbar".
!
!  From now on, these newly computed forcing terms will remain
!  constant during the fast time stepping and will added to
!  "rhs_ubar" and "rhs_vbar" during all subsequent time steps.
!
      IF (iif(ng).eq.1.and.PREDICTOR_2D_STEP(ng)) THEN
        IF (iic(ng).eq.ntfirst(ng)) THEN
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              rufrc(i,j)=rufrc(i,j)-rhs_ubar(i,j)
              rhs_ubar(i,j)=rhs_ubar(i,j)+rufrc(i,j)
              ru(i,j,0,nstp)=rufrc(i,j)
              DO idiag=1,M2pgrd
                DiaRUfrc(i,j,3,idiag)=DiaRUfrc(i,j,3,idiag)-            &
     &                                DiaU2rhs(i,j,idiag)
                DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                &
     &                              DiaRUfrc(i,j,3,idiag)
                DiaRUfrc(i,j,nstp,idiag)=DiaRUfrc(i,j,3,idiag)
              END DO
              DiaU2rhs(i,j,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaRUfrc(i,j,nstp,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaU2rhs(i,j,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
              DiaRUfrc(i,j,nstp,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)-rhs_vbar(i,j)
              rhs_vbar(i,j)=rhs_vbar(i,j)+rvfrc(i,j)
              rv(i,j,0,nstp)=rvfrc(i,j)
              DO idiag=1,M2pgrd
                DiaRVfrc(i,j,3,idiag)=DiaRVfrc(i,j,3,idiag)-            &
     &                                DiaV2rhs(i,j,idiag)
                DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                &
     &                              DiaRVfrc(i,j,3,idiag)
                DiaRVfrc(i,j,nstp,idiag)=DiaRVfrc(i,j,3,idiag)
              END DO
              DiaV2rhs(i,j,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaRVfrc(i,j,nstp,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaV2rhs(i,j,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
              DiaRVfrc(i,j,nstp,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
            END DO
          END DO
        ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              rufrc(i,j)=rufrc(i,j)-rhs_ubar(i,j)
              rhs_ubar(i,j)=rhs_ubar(i,j)+                              &
     &                      1.5_r8*rufrc(i,j)-0.5_r8*ru(i,j,0,nnew)
              ru(i,j,0,nstp)=rufrc(i,j)
              DO idiag=1,M2pgrd
                DiaRUfrc(i,j,3,idiag)=DiaRUfrc(i,j,3,idiag)-            &
     &                                DiaU2rhs(i,j,idiag)
                DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                &
     &                              1.5_r8*DiaRUfrc(i,j,3,idiag)-       &
     &                              0.5_r8*DiaRUfrc(i,j,nnew,idiag)
                DiaRUfrc(i,j,nstp,idiag)=DiaRUfrc(i,j,3,idiag)
              END DO
              DiaU2rhs(i,j,M2sstr)=1.5_r8*DiaRUfrc(i,j,3,M2sstr)-       &
     &                             0.5_r8*DiaRUfrc(i,j,nnew,M2sstr)
              DiaRUfrc(i,j,nstp,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaU2rhs(i,j,M2bstr)=1.5_r8*DiaRUfrc(i,j,3,M2bstr)-       &
     &                             0.5_r8*DiaRUfrc(i,j,nnew,M2bstr)
              DiaRUfrc(i,j,nstp,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)-rhs_vbar(i,j)
              rhs_vbar(i,j)=rhs_vbar(i,j)+                              &
     &                      1.5_r8*rvfrc(i,j)-0.5_r8*rv(i,j,0,nnew)
              rv(i,j,0,nstp)=rvfrc(i,j)
              DO idiag=1,M2pgrd
                DiaRVfrc(i,j,3,idiag)=DiaRVfrc(i,j,3,idiag)-            &
     &                                DiaV2rhs(i,j,idiag)
                DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                &
     &                              1.5_r8*DiaRVfrc(i,j,3,idiag)-       &
     &                              0.5_r8*DiaRVfrc(i,j,nnew,idiag)
                DiaRVfrc(i,j,nstp,idiag)=DiaRVfrc(i,j,3,idiag)
              END DO
              DiaV2rhs(i,j,M2sstr)=1.5_r8*DiaRVfrc(i,j,3,M2sstr)-       &
     &                             0.5_r8*DiaRVfrc(i,j,nnew,M2sstr)
              DiaRVfrc(i,j,nstp,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaV2rhs(i,j,M2bstr)=1.5_r8*DiaRVfrc(i,j,3,M2bstr)-       &
     &                             0.5_r8*DiaRVfrc(i,j,nnew,M2bstr)
              DiaRVfrc(i,j,nstp,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
            END DO
          END DO
        ELSE
          cff1=23.0_r8/12.0_r8
          cff2=16.0_r8/12.0_r8
          cff3= 5.0_r8/12.0_r8
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              rufrc(i,j)=rufrc(i,j)-rhs_ubar(i,j)
              rhs_ubar(i,j)=rhs_ubar(i,j)+                              &
     &                      cff1*rufrc(i,j)-                            &
     &                      cff2*ru(i,j,0,nnew)+                        &
     &                      cff3*ru(i,j,0,nstp)
              ru(i,j,0,nstp)=rufrc(i,j)
              DO idiag=1,M2pgrd
                DiaRUfrc(i,j,3,idiag)=DiaRUfrc(i,j,3,idiag)-            &
     &                                DiaU2rhs(i,j,idiag)
                DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                &
     &                              cff1*DiaRUfrc(i,j,3,idiag)-         &
     &                              cff2*DiaRUfrc(i,j,nnew,idiag)+      &
     &                              cff3*DiaRUfrc(i,j,nstp,idiag)
                DiaRUfrc(i,j,nstp,idiag)=DiaRUfrc(i,j,3,idiag)
              END DO
              DiaU2rhs(i,j,M2sstr)=cff1*DiaRUfrc(i,j,3,M2sstr)-         &
     &                             cff2*DiaRUfrc(i,j,nnew,M2sstr)+      &
     &                             cff3*DiaRUfrc(i,j,nstp,M2sstr)
              DiaRUfrc(i,j,nstp,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaU2rhs(i,j,M2bstr)=cff1*DiaRUfrc(i,j,3,M2bstr)-         &
     &                             cff2*DiaRUfrc(i,j,nnew,M2bstr)+      &
     &                             cff3*DiaRUfrc(i,j,nstp,M2bstr)
              DiaRUfrc(i,j,nstp,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)-rhs_vbar(i,j)
              rhs_vbar(i,j)=rhs_vbar(i,j)+                              &
     &                      cff1*rvfrc(i,j)-                            &
     &                      cff2*rv(i,j,0,nnew)+                        &
     &                      cff3*rv(i,j,0,nstp)
              rv(i,j,0,nstp)=rvfrc(i,j)
              DO idiag=1,M2pgrd
                DiaRVfrc(i,j,3,idiag)=DiaRVfrc(i,j,3,idiag)-            &
     &                                DiaV2rhs(i,j,idiag)
                DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                &
     &                              cff1*DiaRVfrc(i,j,3,idiag)-         &
     &                              cff2*DiaRVfrc(i,j,nnew,idiag)+      &
     &                              cff3*DiaRVfrc(i,j,nstp,idiag)
                DiaRVfrc(i,j,nstp,idiag)=DiaRVfrc(i,j,3,idiag)
              END DO
              DiaV2rhs(i,j,M2sstr)=cff1*DiaRVfrc(i,j,3,M2sstr)-         &
     &                             cff2*DiaRVfrc(i,j,nnew,M2sstr)+      &
     &                             cff3*DiaRVfrc(i,j,nstp,M2sstr)
              DiaRVfrc(i,j,nstp,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaV2rhs(i,j,M2bstr)=cff1*DiaRVfrc(i,j,3,M2bstr)-         &
     &                             cff2*DiaRVfrc(i,j,nnew,M2bstr)+      &
     &                             cff3*DiaRVfrc(i,j,nstp,M2bstr)
              DiaRVfrc(i,j,nstp,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
            END DO
          END DO
        END IF
      ELSE
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            rhs_ubar(i,j)=rhs_ubar(i,j)+rufrc(i,j)
            DO idiag=1,M2pgrd
              DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                  &
     &                            DiaRUfrc(i,j,3,idiag)
            END DO
            DiaU2rhs(i,j,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
            DiaU2rhs(i,j,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            rhs_vbar(i,j)=rhs_vbar(i,j)+rvfrc(i,j)
            DO idiag=1,M2pgrd
              DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                  &
     &                            DiaRVfrc(i,j,3,idiag)
            END DO
            DiaV2rhs(i,j,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
            DiaV2rhs(i,j,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
          END DO
        END DO
      END IF
!
!=======================================================================
!  Time step 2D momentum equations.
!=======================================================================
!
!  Compute total water column depth.
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          Dstp(i,j)=zeta(i,j,kstp)+h(i,j)
        END DO
      END DO
!
!  During the first time-step, the predictor step is Forward-Euler
!  and the corrector step is Backward-Euler. Otherwise, the predictor
!  step is Leap-frog and the corrector step is Adams-Moulton.
!
      IF (iif(ng).eq.1) THEN
        cff1=0.5_r8*dtfast(ng)
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            ubar(i,j,knew)=(ubar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i-1,j))+                    &
     &                      cff*cff1*rhs_ubar(i,j))*fac
            ubar(i,j,knew)=ubar(i,j,knew)*umask(i,j)
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            vbar(i,j,knew)=(vbar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i,j-1))+                    &
     &                      cff*cff1*rhs_vbar(i,j))*fac
            vbar(i,j,knew)=vbar(i,j,knew)*vmask(i,j)
          END DO
        END DO
      ELSE IF (PREDICTOR_2D_STEP(ng)) THEN
        cff1=dtfast(ng)
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            ubar(i,j,knew)=(ubar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i-1,j))+                    &
     &                      cff*cff1*rhs_ubar(i,j))*fac
            ubar(i,j,knew)=ubar(i,j,knew)*umask(i,j)
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            vbar(i,j,knew)=(vbar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i,j-1))+                    &
     &                      cff*cff1*rhs_vbar(i,j))*fac
            vbar(i,j,knew)=vbar(i,j,knew)*vmask(i,j)
          END DO
        END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)*5.0_r8/12.0_r8
        cff2=0.5_r8*dtfast(ng)*8.0_r8/12.0_r8
        cff3=0.5_r8*dtfast(ng)*1.0_r8/12.0_r8
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            ubar(i,j,knew)=(ubar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i-1,j))+                    &
     &                      cff*(cff1*rhs_ubar(i,j)+                    &
     &                           cff2*rubar(i,j,kstp)-                  &
     &                           cff3*rubar(i,j,ptsk)))*fac
            ubar(i,j,knew)=ubar(i,j,knew)*umask(i,j)
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            vbar(i,j,knew)=(vbar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i,j-1))+                    &
     &                      cff*(cff1*rhs_vbar(i,j)+                    &
     &                           cff2*rvbar(i,j,kstp)-                  &
     &                           cff3*rvbar(i,j,ptsk)))*fac
            vbar(i,j,knew)=vbar(i,j,knew)*vmask(i,j)
          END DO
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Time step 2D momentum diagnostic terms.
!-----------------------------------------------------------------------
!
!  Apply land/sea mask.
!
      DO idiag=1,NDM2d-1
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)*umask(i,j)
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)*vmask(i,j)
          END DO
        END DO
      END DO
!
!  The arrays "DiaU2rhs" and "DiaV2rhs" contain the contributions of
!  each of the 2D right-hand-side terms for the momentum equations.
!
!  These values are integrated, time-stepped and converted to mass flux
!  units (m3 s-1) for coupling with the 3D diagnostic terms.
!
      fac=weight(1,iif(ng),ng)
      IF (iif(ng).eq.1.and.CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              DiaU2int(i,j,idiag)=cff1*DiaU2rhs(i,j,idiag)
              DiaU2wrk(i,j,idiag)=DiaU2int(i,j,idiag)*                  &
     &                            (pm(i-1,j)+pm(i,j))*fac
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              DiaV2int(i,j,idiag)=cff1*DiaV2rhs(i,j,idiag)
              DiaV2wrk(i,j,idiag)=DiaV2int(i,j,idiag)*                  &
     &                            (pn(i,j)+pn(i,j-1))*fac
            END DO
          END DO
        END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)*5.0_r8/12.0_r8
        cff2=0.5_r8*dtfast(ng)*8.0_r8/12.0_r8
        cff3=0.5_r8*dtfast(ng)*1.0_r8/12.0_r8
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              DiaU2int(i,j,idiag)=DiaU2int(i,j,idiag)+                  &
     &                            (cff1*DiaU2rhs(i,j,idiag)+            &
     &                             cff2*DiaRUbar(i,j,kstp,idiag)-       &
     &                             cff3*DiaRUbar(i,j,ptsk,idiag))
              DiaU2wrk(i,j,idiag)=DiaU2wrk(i,j,idiag)+                  &
     &                            DiaU2int(i,j,idiag)*                  &
     &                            (pm(i-1,j)+pm(i,j))*fac
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              DiaV2int(i,j,idiag)=DiaV2int(i,j,idiag)+                  &
     &                            (cff1*DiaV2rhs(i,j,idiag)+            &
     &                             cff2*DiaRVbar(i,j,kstp,idiag)-       &
     &                             cff3*DiaRVbar(i,j,ptsk,idiag))
              DiaV2wrk(i,j,idiag)=DiaV2wrk(i,j,idiag)+                  &
     &                            DiaV2int(i,j,idiag)*                  &
     &                            (pn(i,j)+pn(i,j-1))*fac
            END DO
          END DO
        END DO
      END IF
!
!  If predictor step, load right-side-term into shared arrays for
!  future use during the subsequent corrector step.
!
      IF (PREDICTOR_2D_STEP(ng)) THEN
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            rubar(i,j,krhs)=rhs_ubar(i,j)
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            rvbar(i,j,krhs)=rhs_vbar(i,j)
          END DO
        END DO
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              DiaRUbar(i,j,krhs,idiag)=DiaU2rhs(i,j,idiag)
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              DiaRVbar(i,j,krhs,idiag)=DiaV2rhs(i,j,idiag)
            END DO
          END DO
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Apply lateral boundary conditions.
!-----------------------------------------------------------------------
!
      CALL u2dbc_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 krhs, kstp, knew,                                &
     &                 ubar, vbar, zeta)
      CALL v2dbc_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 krhs, kstp, knew,                                &
     &                 ubar, vbar, zeta)
!
!  Compute integral mass flux across open boundaries and adjust
!  for volume conservation.
!
      IF (ANY(VolCons(:,ng))) THEN
        CALL obc_flux_tile (ng, tile,                                   &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      IminS, ImaxS, JminS, JmaxS,                 &
     &                      knew,                                       &
     &                      umask, vmask,                               &
     &                      h, om_v, on_u,                              &
     &                      ubar, vbar, zeta)
      END IF
!
!-----------------------------------------------------------------------
!  Apply momentum transport point sources (like river runoff), if any.
!-----------------------------------------------------------------------
!
      IF (LuvSrc(ng)) THEN
        DO is=1,Nsrc(ng)
          i=SOURCES(ng)%Isrc(is)
          j=SOURCES(ng)%Jsrc(is)
          IF (((IstrR.le.i).and.(i.le.IendR)).and.                      &
     &        ((JstrR.le.j).and.(j.le.JendR))) THEN
            IF (INT(SOURCES(ng)%Dsrc(is)).eq.0) THEN
              cff=1.0_r8/(on_u(i,j)*                                    &
     &                    0.5_r8*(zeta(i-1,j,knew)+h(i-1,j)+            &
     &                            zeta(i  ,j,knew)+h(i  ,j)))
              ubar(i,j,knew)=SOURCES(ng)%Qbar(is)*cff
            ELSE
              cff=1.0_r8/(om_v(i,j)*                                    &
     &                    0.5_r8*(zeta(i,j-1,knew)+h(i,j-1)+            &
     &                            zeta(i,j  ,knew)+h(i,j  )))
              vbar(i,j,knew)=SOURCES(ng)%Qbar(is)*cff
            END IF
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Exchange boundary information.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          ubar(:,:,knew))
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          vbar(:,:,knew))
      END IF
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    ubar(:,:,knew),                               &
     &                    vbar(:,:,knew))
!
      RETURN
      END SUBROUTINE step2d_tile
      END MODULE step2d_mod
