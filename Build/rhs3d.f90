      MODULE rhs3d_mod
!
!svn $Id: rhs3d.F 1054 2021-03-06 19:47:12Z arango $
!=======================================================================
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                           Hernan G. Arango   !
!========================================== Alexander F. Shchepetkin ===
!                                                                      !
!  This subroutine evaluates right-hand-side terms for 3D momentum     !
!  and tracers equations.                                              !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: rhs3d
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE rhs3d (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_coupling
      USE mod_diags
      USE mod_forces
      USE mod_grid
      USE mod_ocean
      USE mod_stepping
!
      USE pre_step3d_mod, ONLY : pre_step3d
      USE prsgrd_mod, ONLY : prsgrd
      USE t3dmix_mod, ONLY : t3dmix2
      USE uv3dmix_mod, ONLY : uv3dmix2
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Nonlinear/rhs3d.F"
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
!-----------------------------------------------------------------------
!  Initialize computations for new time step of the 3D primitive
!  variables.
!-----------------------------------------------------------------------
!
      CALL pre_step3d (ng, tile)
!
!-----------------------------------------------------------------------
!  Compute baroclinic pressure gradient.
!-----------------------------------------------------------------------
!
      CALL prsgrd (ng, tile)
!
!
!
!-----------------------------------------------------------------------
!  Compute horizontal harmonic mixing of tracer type variables.
!-----------------------------------------------------------------------
!
      CALL t3dmix2 (ng, tile)
!
!-----------------------------------------------------------------------
!  Compute right-hand-side terms for the 3D momentum equations.
!-----------------------------------------------------------------------
!
      CALL wclock_on (ng, iNLM, 21, 140, MyFile)
      CALL rhs3d_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 nrhs(ng),                                        &
     &                 GRID(ng) % Hz,                                   &
     &                 GRID(ng) % Huon,                                 &
     &                 GRID(ng) % Hvom,                                 &
     &                 GRID(ng) % dmde,                                 &
     &                 GRID(ng) % dndx,                                 &
     &                 GRID(ng) % fomn,                                 &
     &                 GRID(ng) % om_u,                                 &
     &                 GRID(ng) % om_v,                                 &
     &                 GRID(ng) % on_u,                                 &
     &                 GRID(ng) % on_v,                                 &
     &                 GRID(ng) % pm,                                   &
     &                 GRID(ng) % pn,                                   &
     &                 FORCES(ng) % bustr,                              &
     &                 FORCES(ng) % bvstr,                              &
     &                 FORCES(ng) % sustr,                              &
     &                 FORCES(ng) % svstr,                              &
     &                 OCEAN(ng) % u,                                   &
     &                 OCEAN(ng) % v,                                   &
     &                 OCEAN(ng) % W,                                   &
     &                 COUPLING(ng) % rufrc,                            &
     &                 COUPLING(ng) % rvfrc,                            &
     &                 DIAGS(ng) % DiaRUfrc,                            &
     &                 DIAGS(ng) % DiaRVfrc,                            &
     &                 DIAGS(ng) % DiaRU,                               &
     &                 DIAGS(ng) % DiaRV,                               &
     &                 OCEAN(ng) % ru,                                  &
     &                 OCEAN(ng) % rv)
      CALL wclock_off (ng, iNLM, 21, 197, MyFile)
!
!-----------------------------------------------------------------------
!  Compute horizontal, harmonic mixing of momentum.
!-----------------------------------------------------------------------
!
      CALL uv3dmix2 (ng, tile)
!
      RETURN
      END SUBROUTINE rhs3d
!
!***********************************************************************
      SUBROUTINE rhs3d_tile (ng, tile,                                  &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
     &                       nrhs,                                      &
     &                       Hz, Huon, Hvom,                            &
     &                       dmde, dndx,                                &
     &                       fomn,                                      &
     &                       om_u, om_v, on_u, on_v, pm, pn,            &
     &                       bustr, bvstr,                              &
     &                       sustr, svstr,                              &
     &                       u, v, W,                                   &
     &                       rufrc, rvfrc,                              &
     &                       DiaRUfrc, DiaRVfrc,                        &
     &                       DiaRU, DiaRV,                              &
     &                       ru, rv)
!***********************************************************************
!
      USE mod_param
      USE mod_clima
      USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nrhs
!
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: Huon(LBi:,LBj:,:)
      real(r8), intent(in) :: Hvom(LBi:,LBj:,:)
      real(r8), intent(in) :: dmde(LBi:,LBj:)
      real(r8), intent(in) :: dndx(LBi:,LBj:)
      real(r8), intent(in) :: fomn(LBi:,LBj:)
      real(r8), intent(in) :: om_u(LBi:,LBj:)
      real(r8), intent(in) :: om_v(LBi:,LBj:)
      real(r8), intent(in) :: on_u(LBi:,LBj:)
      real(r8), intent(in) :: on_v(LBi:,LBj:)
      real(r8), intent(in) :: pm(LBi:,LBj:)
      real(r8), intent(in) :: pn(LBi:,LBj:)
      real(r8), intent(in) :: bustr(LBi:,LBj:)
      real(r8), intent(in) :: bvstr(LBi:,LBj:)
      real(r8), intent(in) :: sustr(LBi:,LBj:)
      real(r8), intent(in) :: svstr(LBi:,LBj:)
      real(r8), intent(in) :: u(LBi:,LBj:,:,:)
      real(r8), intent(in) :: v(LBi:,LBj:,:,:)
      real(r8), intent(in) :: W(LBi:,LBj:,0:)
      real(r8), intent(inout) :: ru(LBi:,LBj:,0:,:)
      real(r8), intent(inout) :: rv(LBi:,LBj:,0:,:)
      real(r8), intent(inout) :: DiaRUfrc(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaRVfrc(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaRU(LBi:,LBj:,:,:,:)
      real(r8), intent(inout) :: DiaRV(LBi:,LBj:,:,:,:)
      real(r8), intent(out) :: rufrc(LBi:,LBj:)
      real(r8), intent(out) :: rvfrc(LBi:,LBj:)
!
!  Local variable declarations.
!
      integer :: i, j, k
      real(r8), parameter :: Gadv = -0.25_r8
      real(r8) :: cff, cff1, cff2, cff3, cff4
      real(r8) :: cff5, cff6, cff7, cff8
      real(r8) :: fac, fac1, fac2
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: CF
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: DC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FC
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Huee
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Huxx
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Hvee
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Hvxx
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: UFx
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: UFe
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Uwrk
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: VFx
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: VFe
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Vwrk
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: uee
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: uxx
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: vee
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: vxx
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: wrk
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
!
      K_LOOP : DO k=1,N(ng)
!
!-----------------------------------------------------------------------
!  Add in Coriolis terms.
!-----------------------------------------------------------------------
!
        DO j=JstrV-1,Jend
          DO i=IstrU-1,Iend
            cff=0.5_r8*Hz(i,j,k)*fomn(i,j)
            UFx(i,j)=cff*(v(i,j  ,k,nrhs)+                              &
     &                    v(i,j+1,k,nrhs))
            VFe(i,j)=cff*(u(i  ,j,k,nrhs)+                              &
     &                    u(i+1,j,k,nrhs))
          END DO
        END DO
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff1=0.5_r8*(UFx(i,j)+UFx(i-1,j))
            ru(i,j,k,nrhs)=ru(i,j,k,nrhs)+cff1
            DiaRU(i,j,k,nrhs,M3fcor)=cff1
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff1=0.5_r8*(VFe(i,j)+VFe(i,j-1))
            rv(i,j,k,nrhs)=rv(i,j,k,nrhs)-cff1
            DiaRV(i,j,k,nrhs,M3fcor)=-cff1
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
            cff1=0.5_r8*(v(i,j  ,k,nrhs)+                               &
     &                   v(i,j+1,k,nrhs))
            cff2=0.5_r8*(u(i  ,j,k,nrhs)+                               &
     &                   u(i+1,j,k,nrhs))
            cff3=cff1*dndx(i,j)
            cff4=cff2*dmde(i,j)
            cff=Hz(i,j,k)*(cff3-cff4)
            UFx(i,j)=cff*cff1
            VFe(i,j)=cff*cff2
            cff=Hz(i,j,k)*cff4
            Uwrk(i,j)=-cff*cff1                ! u equation, ETA-term
            Vwrk(i,j)=-cff*cff2                ! v equation, ETA-term
          END DO
        END DO
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff1=0.5_r8*(UFx(i,j)+UFx(i-1,j))
            ru(i,j,k,nrhs)=ru(i,j,k,nrhs)+cff1
            cff2=0.5_r8*(Uwrk(i,j)+Uwrk(i-1,j))
            DiaRU(i,j,k,nrhs,M3xadv)=cff1-cff2
            DiaRU(i,j,k,nrhs,M3yadv)=cff2
            DiaRU(i,j,k,nrhs,M3hadv)=cff1
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff1=0.5_r8*(VFe(i,j)+VFe(i,j-1))
            rv(i,j,k,nrhs)=rv(i,j,k,nrhs)-cff1
            cff2=0.5_r8*(Vwrk(i,j)+Vwrk(i,j-1))
            DiaRV(i,j,k,nrhs,M3xadv)=-cff1+cff2
            DiaRV(i,j,k,nrhs,M3yadv)=-cff2
            DiaRV(i,j,k,nrhs,M3hadv)=-cff1
          END DO
        END DO
!
!-----------------------------------------------------------------------
!  Add in nudging of 3D momentum climatology.
!-----------------------------------------------------------------------
!
        IF (LnudgeM3CLM(ng)) THEN
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              cff=0.25_r8*(CLIMA(ng)%M3nudgcof(i-1,j,k)+                &
     &                     CLIMA(ng)%M3nudgcof(i  ,j,k))*               &
     &            om_u(i,j)*on_u(i,j)
              ru(i,j,k,nrhs)=ru(i,j,k,nrhs)+                            &
     &                       cff*(Hz(i-1,j,k)+Hz(i,j,k))*               &
     &                       (CLIMA(ng)%uclm(i,j,k)-                    &
     &                        u(i,j,k,nrhs))
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              cff=0.25_r8*(CLIMA(ng)%M3nudgcof(i,j-1,k)+                &
     &                     CLIMA(ng)%M3nudgcof(i,j  ,k))*               &
     &            om_v(i,j)*on_v(i,j)
              rv(i,j,k,nrhs)=rv(i,j,k,nrhs)+                            &
     &                       cff*(Hz(i,j-1,k)+Hz(i,j,k))*               &
     &                       (CLIMA(ng)%vclm(i,j,k)-                    &
     &                        v(i,j,k,nrhs))
            END DO
          END DO
        END IF
!
!-----------------------------------------------------------------------
!  Add in horizontal advection of momentum.
!-----------------------------------------------------------------------
!
!  Compute diagonal [UFx,VFe] and off-diagonal [UFe,VFx] components
!  of tensor of momentum flux due to horizontal advection.
!
        DO j=Jstr,Jend
          DO i=IstrUm1,Iendp1
            uxx(i,j)=u(i-1,j,k,nrhs)-2.0_r8*u(i,j,k,nrhs)+              &
     &               u(i+1,j,k,nrhs)
            Huxx(i,j)=Huon(i-1,j,k)-2.0_r8*Huon(i,j,k)+Huon(i+1,j,k)
          END DO
        END DO
        IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
          IF (DOMAIN(ng)%Western_Edge(tile)) THEN
            DO j=Jstr,Jend
              uxx (Istr,j)=uxx (Istr+1,j)
              Huxx(Istr,j)=Huxx(Istr+1,j)
            END DO
          END IF
        END IF
        IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
          IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
            DO j=Jstr,Jend
              uxx (Iend+1,j)=uxx (Iend,j)
              Huxx(Iend+1,j)=Huxx(Iend,j)
            END DO
          END IF
        END IF
!
!  Third-order, upstream bias u-momentum advection with velocity
!  dependent hyperdiffusion.
!
        DO j=Jstr,Jend
          DO i=IstrU-1,Iend
            cff1=u(i  ,j,k,nrhs)+                                       &
     &           u(i+1,j,k,nrhs)
            IF (cff1.gt.0.0_r8) THEN
              cff=uxx(i,j)
            ELSE
              cff=uxx(i+1,j)
            END IF
            UFx(i,j)=0.25_r8*(cff1+Gadv*cff)*                           &
     &               (Huon(i  ,j,k)+                                    &
     &                Huon(i+1,j,k)+                                    &
     &                Gadv*0.5_r8*(Huxx(i  ,j)+                         &
     &                             Huxx(i+1,j)))
          END DO
        END DO
        DO j=Jstrm1,Jendp1
          DO i=IstrU,Iend
            uee(i,j)=u(i,j-1,k,nrhs)-2.0_r8*u(i,j,k,nrhs)+              &
     &               u(i,j+1,k,nrhs)
          END DO
        END DO
        IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
          IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
            DO i=IstrU,Iend
              uee(i,Jstr-1)=uee(i,Jstr)
            END DO
          END IF
        END IF
        IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
          IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
            DO i=IstrU,Iend
              uee(i,Jend+1)=uee(i,Jend)
            END DO
          END IF
        END IF
        DO j=Jstr,Jend+1
          DO i=IstrU-1,Iend
           Hvxx(i,j)=Hvom(i-1,j,k)-2.0_r8*Hvom(i,j,k)+Hvom(i+1,j,k)
          END DO
        END DO
        DO j=Jstr,Jend+1
          DO i=IstrU,Iend
            cff1=u(i,j  ,k,nrhs)+                                       &
     &           u(i,j-1,k,nrhs)
            cff2=Hvom(i,j,k)+Hvom(i-1,j,k)
            IF (cff2.gt.0.0_r8) THEN
              cff=uee(i,j-1)
            ELSE
              cff=uee(i,j)
            END IF
            UFe(i,j)=0.25_r8*(cff1+Gadv*cff)*                           &
     &               (cff2+Gadv*0.5_r8*(Hvxx(i  ,j)+                    &
     &                                  Hvxx(i-1,j)))
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istrm1,Iendp1
            vxx(i,j)=v(i-1,j,k,nrhs)-2.0_r8*v(i,j,k,nrhs)+              &
     &               v(i+1,j,k,nrhs)
          END DO
        END DO
        IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
          IF (DOMAIN(ng)%Western_Edge(tile)) THEN
            DO j=JstrV,Jend
              vxx(Istr-1,j)=vxx(Istr,j)
            END DO
          END IF
        END IF
        IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
          IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
            DO j=JstrV,Jend
              vxx(Iend+1,j)=vxx(Iend,j)
            END DO
          END IF
        END IF
        DO j=JstrV-1,Jend
          DO i=Istr,Iend+1
           Huee(i,j)=Huon(i,j-1,k)-2.0_r8*Huon(i,j,k)+Huon(i,j+1,k)
          END DO
        END DO
!
!  Third-order, upstream bias v-momentum advection with velocity
!  dependent hyperdiffusion.
!
        DO j=JstrV,Jend
          DO i=Istr,Iend+1
            cff1=v(i  ,j,k,nrhs)+                                       &
     &           v(i-1,j,k,nrhs)
            cff2=Huon(i,j,k)+Huon(i,j-1,k)
            IF (cff2.gt.0.0_r8) THEN
              cff=vxx(i-1,j)
            ELSE
              cff=vxx(i,j)
            END IF
            VFx(i,j)=0.25_r8*(cff1+Gadv*cff)*                           &
     &               (cff2+Gadv*0.5_r8*(Huee(i,j  )+                    &
     &                                  Huee(i,j-1)))
          END DO
        END DO
        DO j=JstrVm1,Jendp1
          DO i=Istr,Iend
            vee(i,j)=v(i,j-1,k,nrhs)-2.0_r8*v(i,j,k,nrhs)+              &
     &               v(i,j+1,k,nrhs)
            Hvee(i,j)=Hvom(i,j-1,k)-2.0_r8*Hvom(i,j,k)+Hvom(i,j+1,k)
          END DO
        END DO
        IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
          IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
            DO i=Istr,Iend
              vee (i,Jstr)=vee (i,Jstr+1)
              Hvee(i,Jstr)=Hvee(i,Jstr+1)
            END DO
          END IF
        END IF
        IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
          IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
            DO i=Istr,Iend
              vee (i,Jend+1)=vee (i,Jend)
              Hvee(i,Jend+1)=Hvee(i,Jend)
            END DO
          END IF
        END IF
        DO j=JstrV-1,Jend
          DO i=Istr,Iend
            cff1=v(i,j  ,k,nrhs)+                                       &
     &           v(i,j+1,k,nrhs)
            IF (cff1.gt.0.0_r8) THEN
              cff=vee(i,j)
            ELSE
              cff=vee(i,j+1)
            END IF
            VFe(i,j)=0.25_r8*(cff1+Gadv*cff)*                           &
     &               (Hvom(i,j  ,k)+                                    &
     &                Hvom(i,j+1,k)+                                    &
     &                Gadv*0.5_r8*(Hvee(i,j  )+                         &
     &                             Hvee(i,j+1)))
          END DO
        END DO
!
!  Add in horizontal advection.
!
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff1=UFx(i,j)-UFx(i-1,j)
            cff2=UFe(i,j+1)-UFe(i,j)
            cff=cff1+cff2
            ru(i,j,k,nrhs)=ru(i,j,k,nrhs)-cff
            DiaRU(i,j,k,nrhs,M3xadv)=DiaRU(i,j,k,nrhs,M3xadv)-cff1
            DiaRU(i,j,k,nrhs,M3yadv)=DiaRU(i,j,k,nrhs,M3yadv)-cff2
            DiaRU(i,j,k,nrhs,M3hadv)=DiaRU(i,j,k,nrhs,M3hadv)-cff
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff1=VFx(i+1,j)-VFx(i,j)
            cff2=VFe(i,j)-VFe(i,j-1)
            cff=cff1+cff2
            rv(i,j,k,nrhs)=rv(i,j,k,nrhs)-cff
            DiaRV(i,j,k,nrhs,M3xadv)=DiaRV(i,j,k,nrhs,M3xadv)-cff1
            DiaRV(i,j,k,nrhs,M3yadv)=DiaRV(i,j,k,nrhs,M3yadv)-cff2
            DiaRV(i,j,k,nrhs,M3hadv)=DiaRV(i,j,k,nrhs,M3hadv)-cff
          END DO
        END DO
      END DO K_LOOP
!
      J_LOOP : DO j=Jstr,Jend
!
!-----------------------------------------------------------------------
!  Add in vertical advection.
!-----------------------------------------------------------------------
!
        cff1=9.0_r8/16.0_r8
        cff2=1.0_r8/16.0_r8
        DO k=2,N(ng)-2
          DO i=IstrU,Iend
            FC(i,k)=(cff1*(u(i,j,k  ,nrhs)+                             &
     &                     u(i,j,k+1,nrhs))-                            &
     &               cff2*(u(i,j,k-1,nrhs)+                             &
     &                     u(i,j,k+2,nrhs)))*                           &
     &              (cff1*(W(i  ,j,k)+                                  &
     &                     W(i-1,j,k))-                                 &
     &               cff2*(W(i+1,j,k)+                                  &
     &                     W(i-2,j,k)))
          END DO
        END DO
        DO i=IstrU,Iend
          FC(i,N(ng))=0.0_r8
          FC(i,N(ng)-1)=(cff1*(u(i,j,N(ng)-1,nrhs)+                     &
     &                         u(i,j,N(ng)  ,nrhs))-                    &
     &                   cff2*(u(i,j,N(ng)-2,nrhs)+                     &
     &                         u(i,j,N(ng)  ,nrhs)))*                   &
     &                  (cff1*(W(i  ,j,N(ng)-1)+                        &
     &                         W(i-1,j,N(ng)-1))-                       &
     &                   cff2*(W(i+1,j,N(ng)-1)+                        &
     &                         W(i-2,j,N(ng)-1)))
          FC(i,1)=(cff1*(u(i,j,1,nrhs)+                                 &
     &                   u(i,j,2,nrhs))-                                &
     &             cff2*(u(i,j,1,nrhs)+                                 &
     &                   u(i,j,3,nrhs)))*                               &
     &            (cff1*(W(i  ,j,1)+                                    &
     &                   W(i-1,j,1))-                                   &
     &             cff2*(W(i+1,j,1)+                                    &
     &                   W(i-2,j,1)))
          FC(i,0)=0.0_r8
        END DO
        DO k=1,N(ng)
          DO i=IstrU,Iend
            cff=FC(i,k)-FC(i,k-1)
            ru(i,j,k,nrhs)=ru(i,j,k,nrhs)-cff
            DiaRU(i,j,k,nrhs,M3vadv)=-cff
          END DO
        END DO
        IF (j.ge.JstrV) THEN
          cff1=9.0_r8/16.0_r8
          cff2=1.0_r8/16.0_r8
          DO k=2,N(ng)-2
            DO i=Istr,Iend
              FC(i,k)=(cff1*(v(i,j,k  ,nrhs)+                           &
     &                       v(i,j,k+1,nrhs))-                          &
     &                 cff2*(v(i,j,k-1,nrhs)+                           &
     &                       v(i,j,k+2,nrhs)))*                         &
     &                (cff1*(W(i,j  ,k)+                                &
     &                       W(i,j-1,k))-                               &
     &                 cff2*(W(i,j+1,k)+                                &
     &                       W(i,j-2,k)))
            END DO
          END DO
          DO i=Istr,Iend
            FC(i,N(ng))=0.0_r8
            FC(i,N(ng)-1)=(cff1*(v(i,j,N(ng)-1,nrhs)+                   &
     &                           v(i,j,N(ng)  ,nrhs))-                  &
     &                     cff2*(v(i,j,N(ng)-2,nrhs)+                   &
     &                           v(i,j,N(ng)  ,nrhs)))*                 &
     &                    (cff1*(W(i,j  ,N(ng)-1)+                      &
     &                           W(i,j-1,N(ng)-1))-                     &
     &                     cff2*(W(i,j+1,N(ng)-1)+                      &
     &                           W(i,j-2,N(ng)-1)))
            FC(i,1)=(cff1*(v(i,j,1,nrhs)+                               &
     &                     v(i,j,2,nrhs))-                              &
     &               cff2*(v(i,j,1,nrhs)+                               &
     &                     v(i,j,3,nrhs)))*                             &
     &              (cff1*(W(i,j  ,1)+                                  &
     &                     W(i,j-1,1))-                                 &
     &               cff2*(W(i,j+1,1)+                                  &
     &                     W(i,j-2,1)))
            FC(i,0)=0.0_r8
          END DO
          DO k=1,N(ng)
            DO i=Istr,Iend
              cff=FC(i,k)-FC(i,k-1)
              rv(i,j,k,nrhs)=rv(i,j,k,nrhs)-cff
              DiaRV(i,j,k,nrhs,M3vadv)=-cff
            END DO
          END DO
        END IF
!
!-----------------------------------------------------------------------
!  Compute forcing term for the 2D momentum equations.
!-----------------------------------------------------------------------
!
!  Vertically integrate baroclinic right-hand-side terms. If not
!  body force stresses, add in the difference between surface and
!  bottom stresses.
!
        DO i=IstrU,Iend
          rufrc(i,j)=ru(i,j,1,nrhs)
          DiaRUfrc(i,j,3,M2pgrd)=DiaRU(i,j,1,nrhs,M3pgrd)
          DiaRUfrc(i,j,3,M2fcor)=DiaRU(i,j,1,nrhs,M3fcor)
          DiaRUfrc(i,j,3,M2xadv)=DiaRU(i,j,1,nrhs,M3xadv)
          DiaRUfrc(i,j,3,M2yadv)=DiaRU(i,j,1,nrhs,M3yadv)
          DiaRUfrc(i,j,3,M2hadv)=DiaRU(i,j,1,nrhs,M3hadv)
          DiaRUfrc(i,j,3,M2xvis)=0.0_r8
          DiaRUfrc(i,j,3,M2yvis)=0.0_r8
          DiaRUfrc(i,j,3,M2hvis)=0.0_r8
        END DO
        DO k=2,N(ng)
          DO i=IstrU,Iend
            rufrc(i,j)=rufrc(i,j)+ru(i,j,k,nrhs)
            DiaRUfrc(i,j,3,M2pgrd)=DiaRUfrc(i,j,3,M2pgrd)+              &
     &                             DiaRU(i,j,k,nrhs,M3pgrd)
            DiaRUfrc(i,j,3,M2fcor)=DiaRUfrc(i,j,3,M2fcor)+              &
     &                             DiaRU(i,j,k,nrhs,M3fcor)
            DiaRUfrc(i,j,3,M2xadv)=DiaRUfrc(i,j,3,M2xadv)+              &
     &                             DiaRU(i,j,k,nrhs,M3xadv)
            DiaRUfrc(i,j,3,M2yadv)=DiaRUfrc(i,j,3,M2yadv)+              &
     &                             DiaRU(i,j,k,nrhs,M3yadv)
            DiaRUfrc(i,j,3,M2hadv)=DiaRUfrc(i,j,3,M2hadv)+              &
     &                             DiaRU(i,j,k,nrhs,M3hadv)
          END DO
        END DO
        DO i=IstrU,Iend
          cff=om_u(i,j)*on_u(i,j)
          cff1= sustr(i,j)*cff
          cff2=-bustr(i,j)*cff
          rufrc(i,j)=rufrc(i,j)+cff1+cff2
          DiaRUfrc(i,j,3,M2sstr)=cff1
          DiaRUfrc(i,j,3,M2bstr)=cff2
        END DO
        IF (j.ge.JstrV) THEN
          DO i=Istr,Iend
            rvfrc(i,j)=rv(i,j,1,nrhs)
            DiaRVfrc(i,j,3,M2pgrd)=DiaRV(i,j,1,nrhs,M3pgrd)
            DiaRVfrc(i,j,3,M2fcor)=DiaRV(i,j,1,nrhs,M3fcor)
            DiaRVfrc(i,j,3,M2xadv)=DiaRV(i,j,1,nrhs,M3xadv)
            DiaRVfrc(i,j,3,M2yadv)=DiaRV(i,j,1,nrhs,M3yadv)
            DiaRVfrc(i,j,3,M2hadv)=DiaRV(i,j,1,nrhs,M3hadv)
            DiaRVfrc(i,j,3,M2hvis)=0.0_r8
            DiaRVfrc(i,j,3,M2xvis)=0.0_r8
            DiaRVfrc(i,j,3,M2yvis)=0.0_r8
          END DO
          DO k=2,N(ng)
            DO i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)+rv(i,j,k,nrhs)
              DiaRVfrc(i,j,3,M2pgrd)=DiaRVfrc(i,j,3,M2pgrd)+            &
     &                               DiaRV(i,j,k,nrhs,M3pgrd)
              DiaRVfrc(i,j,3,M2fcor)=DiaRVfrc(i,j,3,M2fcor)+            &
     &                               DiaRV(i,j,k,nrhs,M3fcor)
              DiaRVfrc(i,j,3,M2xadv)=DiaRVfrc(i,j,3,M2xadv)+            &
     &                               DiaRV(i,j,k,nrhs,M3xadv)
              DiaRVfrc(i,j,3,M2yadv)=DiaRVfrc(i,j,3,M2yadv)+            &
     &                               DiaRV(i,j,k,nrhs,M3yadv)
              DiaRVfrc(i,j,3,M2hadv)=DiaRVfrc(i,j,3,M2hadv)+            &
     &                               DiaRV(i,j,k,nrhs,M3hadv)
            END DO
          END DO
          DO i=Istr,Iend
            cff=om_v(i,j)*on_v(i,j)
            cff1= svstr(i,j)*cff
            cff2=-bvstr(i,j)*cff
            rvfrc(i,j)=rvfrc(i,j)+cff1+cff2
            DiaRVfrc(i,j,3,M2sstr)=cff1
            DiaRVfrc(i,j,3,M2bstr)=cff2
          END DO
        END IF
      END DO J_LOOP
!
      RETURN
      END SUBROUTINE rhs3d_tile
      END MODULE rhs3d_mod
