      MODULE step3d_uv_mod
!
!svn $Id: step3d_uv.F 1054 2021-03-06 19:47:12Z arango $
!=======================================================================
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license           Hernan G. Arango   !
!    See License_ROMS.txt                   Alexander F. Shchepetkin   !
!==================================================== John C. Warner ===
!                                                                      !
!  This subroutine time-steps the nonlinear  horizontal  momentum      !
!  equations. The vertical viscosity terms are time-stepped using      !
!  an implicit algorithm.                                              !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: step3d_uv
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE step3d_uv (ng, tile)
!***********************************************************************
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
     &  "ROMS/Nonlinear/step3d_uv.F"
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
      CALL wclock_on (ng, iNLM, 34, 53, MyFile)
      CALL step3d_uv_tile (ng, tile,                                    &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     IminS, ImaxS, JminS, JmaxS,                  &
     &                     nrhs(ng), nstp(ng), nnew(ng),                &
     &                     GRID(ng) % umask,                            &
     &                     GRID(ng) % vmask,                            &
     &                     GRID(ng) % om_v,                             &
     &                     GRID(ng) % on_u,                             &
     &                     GRID(ng) % pm,                               &
     &                     GRID(ng) % pn,                               &
     &                     GRID(ng) % Hz,                               &
     &                     GRID(ng) % z_r,                              &
     &                     GRID(ng) % z_w,                              &
     &                     MIXING(ng) % Akv,                            &
     &                     COUPLING(ng) % DU_avg1,                      &
     &                     COUPLING(ng) % DV_avg1,                      &
     &                     COUPLING(ng) % DU_avg2,                      &
     &                     COUPLING(ng) % DV_avg2,                      &
     &                     OCEAN(ng) % ru,                              &
     &                     OCEAN(ng) % rv,                              &
     &                     DIAGS(ng) % DiaU2wrk,                        &
     &                     DIAGS(ng) % DiaV2wrk,                        &
     &                     DIAGS(ng) % DiaU2int,                        &
     &                     DIAGS(ng) % DiaV2int,                        &
     &                     DIAGS(ng) % DiaU3wrk,                        &
     &                     DIAGS(ng) % DiaV3wrk,                        &
     &                     DIAGS(ng) % DiaRU,                           &
     &                     DIAGS(ng) % DiaRV,                           &
     &                     OCEAN(ng) % u,                               &
     &                     OCEAN(ng) % v,                               &
     &                     OCEAN(ng) % ubar,                            &
     &                     OCEAN(ng) % vbar,                            &
     &                     GRID(ng) % Huon,                             &
     &                     GRID(ng) % Hvom)
      CALL wclock_off (ng, iNLM, 34, 107, MyFile)
!
      RETURN
      END SUBROUTINE step3d_uv
!
!***********************************************************************
      SUBROUTINE step3d_uv_tile (ng, tile,                              &
     &                           LBi, UBi, LBj, UBj,                    &
     &                           IminS, ImaxS, JminS, JmaxS,            &
     &                           nrhs, nstp, nnew,                      &
     &                           umask, vmask,                          &
     &                           om_v, on_u, pm, pn,                    &
     &                           Hz, z_r, z_w,                          &
     &                           Akv,                                   &
     &                           DU_avg1, DV_avg1,                      &
     &                           DU_avg2, DV_avg2,                      &
     &                           ru, rv,                                &
     &                           DiaU2wrk, DiaV2wrk,                    &
     &                           DiaU2int, DiaV2int,                    &
     &                           DiaU3wrk, DiaV3wrk,                    &
     &                           DiaRU,    DiaRV,                       &
     &                           u, v,                                  &
     &                           ubar, vbar,                            &
     &                           Huon, Hvom)
!***********************************************************************
!
      USE mod_param
      USE mod_ncparam
      USE mod_scalars
      USE mod_sources
!
      USE exchange_2d_mod
      USE exchange_3d_mod
      USE mp_exchange_mod, ONLY : mp_exchange2d, mp_exchange3d
      USE u3dbc_mod, ONLY : u3dbc_tile
      USE v3dbc_mod, ONLY : v3dbc_tile
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nrhs, nstp, nnew
!
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
      real(r8), intent(in) :: om_v(LBi:,LBj:)
      real(r8), intent(in) :: on_u(LBi:,LBj:)
      real(r8), intent(in) :: pm(LBi:,LBj:)
      real(r8), intent(in) :: pn(LBi:,LBj:)
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in) :: Akv(LBi:,LBj:,0:)
      real(r8), intent(in) :: DU_avg1(LBi:,LBj:)
      real(r8), intent(in) :: DV_avg1(LBi:,LBj:)
      real(r8), intent(in) :: DU_avg2(LBi:,LBj:)
      real(r8), intent(in) :: DV_avg2(LBi:,LBj:)
      real(r8), intent(inout) :: ru(LBi:,LBj:,0:,:)
      real(r8), intent(inout) :: rv(LBi:,LBj:,0:,:)
      real(r8), intent(inout) :: DiaU2wrk(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaV2wrk(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaU2int(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaV2int(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaU3wrk(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaV3wrk(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaRU(LBi:,LBj:,:,:,:)
      real(r8), intent(inout) :: DiaRV(LBi:,LBj:,:,:,:)
      real(r8), intent(inout) :: u(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: v(LBi:,LBj:,:,:)
      real(r8), intent(out) :: ubar(LBi:,LBj:,:)
      real(r8), intent(out) :: vbar(LBi:,LBj:,:)
      real(r8), intent(out) :: Huon(LBi:,LBj:,:)
      real(r8), intent(out) :: Hvom(LBi:,LBj:,:)
!
!  Local variable declarations.
!
      integer :: i, idiag, is, j, k
!
      real(r8) :: cff, cff1, cff2
!
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: AK
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: BC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: CF
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: DC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FC
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hzk
      real(r8), dimension(IminS:ImaxS,N(ng)) :: oHz
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: wrk
      real(r8), dimension(IminS:ImaxS,1:NDM2d-1) :: Dwrk
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
!-----------------------------------------------------------------------
!  Time step momentum equation in the XI-direction.
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          AK(i,0)=0.5_r8*(Akv(i-1,j,0)+                                 &
     &                    Akv(i  ,j,0))
          DO k=1,N(ng)
            AK(i,k)=0.5_r8*(Akv(i-1,j,k)+                               &
     &                      Akv(i  ,j,k))
            Hzk(i,k)=0.5_r8*(Hz(i-1,j,k)+                               &
     &                       Hz(i  ,j,k))
            oHz(i,k)=1.0_r8/Hzk(i,k)
          END DO
        END DO
!
!  Time step right-hand-side terms.
!
        IF (iic(ng).eq.ntfirst(ng)) THEN
          cff=0.25_r8*dt(ng)
        ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
          cff=0.25_r8*dt(ng)*3.0_r8/2.0_r8
        ELSE
          cff=0.25_r8*dt(ng)*23.0_r8/12.0_r8
        END IF
        DO i=IstrU,Iend
          DC(i,0)=cff*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
        END DO
        DO k=1,N(ng)
          DO i=IstrU,Iend
            u(i,j,k,nnew)=u(i,j,k,nnew)+                                &
     &                    DC(i,0)*ru(i,j,k,nrhs)
            u(i,j,k,nnew)=u(i,j,k,nnew)*oHz(i,k)
            DO idiag=1,M3pgrd
              DiaU3wrk(i,j,k,idiag)=(DiaU3wrk(i,j,k,idiag)+             &
     &                               DC(i,0)*DiaRU(i,j,k,nrhs,idiag))*  &
     &                              oHz(i,k)
            END DO
            DiaU3wrk(i,j,k,M3xvis)=DiaU3wrk(i,j,k,M3xvis)*oHz(i,k)
            DiaU3wrk(i,j,k,M3yvis)=DiaU3wrk(i,j,k,M3yvis)*oHz(i,k)
            DiaU3wrk(i,j,k,M3hvis)=DiaU3wrk(i,j,k,M3hvis)*oHz(i,k)
            DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)*oHz(i,k)
            DiaU3wrk(i,j,k,M3rate)=DiaU3wrk(i,j,k,M3rate)*oHz(i,k)
          END DO
        END DO
!
!  Use conservative, parabolic spline reconstruction of vertical
!  viscosity derivatives.  Then, time step vertical viscosity term
!  implicitly by solving a tridiagonal system.
!
        cff1=1.0_r8/6.0_r8
        DO k=1,N(ng)-1
          DO i=IstrU,Iend
            FC(i,k)=cff1*Hzk(i,k  )-dt(ng)*AK(i,k-1)*oHz(i,k  )
            CF(i,k)=cff1*Hzk(i,k+1)-dt(ng)*AK(i,k+1)*oHz(i,k+1)
          END DO
        END DO
        DO i=IstrU,Iend
          CF(i,0)=0.0_r8
          DC(i,0)=0.0_r8
        END DO
!
!  LU decomposition and forward substitution.
!
        cff1=1.0_r8/3.0_r8
        DO k=1,N(ng)-1
          DO i=IstrU,Iend
            BC(i,k)=cff1*(Hzk(i,k)+Hzk(i,k+1))+                         &
     &              dt(ng)*AK(i,k)*(oHz(i,k)+oHz(i,k+1))
            cff=1.0_r8/(BC(i,k)-FC(i,k)*CF(i,k-1))
            CF(i,k)=cff*CF(i,k)
            DC(i,k)=cff*(u(i,j,k+1,nnew)-u(i,j,k,nnew)-                 &
     &                   FC(i,k)*DC(i,k-1))
          END DO
        END DO
!
!  Backward substitution.
!
        DO i=IstrU,Iend
          DC(i,N(ng))=0.0_r8
        END DO
        DO k=N(ng)-1,1,-1
          DO i=IstrU,Iend
            DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
          END DO
        END DO
!
        DO k=1,N(ng)
          DO i=IstrU,Iend
            DC(i,k)=DC(i,k)*AK(i,k)
            cff=dt(ng)*oHz(i,k)*(DC(i,k)-DC(i,k-1))
            u(i,j,k,nnew)=u(i,j,k,nnew)+cff
            DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)+cff
          END DO
        END DO
!
!  Replace INTERIOR POINTS incorrect vertical mean with more accurate
!  barotropic component, ubar=DU_avg1/(D*on_u). Recall that, D=CF(:,0).
!
        DO i=IstrU,Iend
          CF(i,0)=Hzk(i,1)
          DC(i,0)=u(i,j,1,nnew)*Hzk(i,1)
          Dwrk(i,M2pgrd)=DiaU3wrk(i,j,1,M3pgrd)*Hzk(i,1)
          Dwrk(i,M2bstr)=DiaU3wrk(i,j,1,M3vvis)*Hzk(i,1)
          Dwrk(i,M2fcor)=DiaU3wrk(i,j,1,M3fcor)*Hzk(i,1)
          Dwrk(i,M2xvis)=DiaU3wrk(i,j,1,M3xvis)*Hzk(i,1)
          Dwrk(i,M2yvis)=DiaU3wrk(i,j,1,M3yvis)*Hzk(i,1)
          Dwrk(i,M2hvis)=DiaU3wrk(i,j,1,M3hvis)*Hzk(i,1)
          Dwrk(i,M2xadv)=DiaU3wrk(i,j,1,M3xadv)*Hzk(i,1)
          Dwrk(i,M2yadv)=DiaU3wrk(i,j,1,M3yadv)*Hzk(i,1)
          Dwrk(i,M2hadv)=DiaU3wrk(i,j,1,M3hadv)*Hzk(i,1)
        END DO
        DO k=2,N(ng)
          DO i=IstrU,Iend
            CF(i,0)=CF(i,0)+Hzk(i,k)
            DC(i,0)=DC(i,0)+u(i,j,k,nnew)*Hzk(i,k)
            Dwrk(i,M2pgrd)=Dwrk(i,M2pgrd)+                              &
     &                     DiaU3wrk(i,j,k,M3pgrd)*Hzk(i,k)
            Dwrk(i,M2bstr)=Dwrk(i,M2bstr)+                              &
     &                     DiaU3wrk(i,j,k,M3vvis)*Hzk(i,k)
            Dwrk(i,M2fcor)=Dwrk(i,M2fcor)+                              &
     &                     DiaU3wrk(i,j,k,M3fcor)*Hzk(i,k)
            Dwrk(i,M2xvis)=Dwrk(i,M2xvis)+                              &
     &                     DiaU3wrk(i,j,k,M3xvis)*Hzk(i,k)
            Dwrk(i,M2yvis)=Dwrk(i,M2yvis)+                              &
     &                     DiaU3wrk(i,j,k,M3yvis)*Hzk(i,k)
            Dwrk(i,M2hvis)=Dwrk(i,M2hvis)+                              &
     &                     DiaU3wrk(i,j,k,M3hvis)*Hzk(i,k)
            Dwrk(i,M2xadv)=Dwrk(i,M2xadv)+                              &
     &                     DiaU3wrk(i,j,k,M3xadv)*Hzk(i,k)
            Dwrk(i,M2yadv)=Dwrk(i,M2yadv)+                              &
     &                     DiaU3wrk(i,j,k,M3yadv)*Hzk(i,k)
            Dwrk(i,M2hadv)=Dwrk(i,M2hadv)+                              &
     &                     DiaU3wrk(i,j,k,M3hadv)*Hzk(i,k)
          END DO
        END DO
        DO i=IstrU,Iend
          cff1=1.0_r8/(CF(i,0)*on_u(i,j))
          DC(i,0)=(DC(i,0)*on_u(i,j)-DU_avg1(i,j))*cff1      ! recursive
          DO idiag=1,M2pgrd
            Dwrk(i,idiag)=(Dwrk(i,idiag)*on_u(i,j)-                     &
     &                     DiaU2wrk(i,j,idiag))*cff1
          END DO
          Dwrk(i,M2bstr)=(Dwrk(i,M2bstr)*on_u(i,j)-                     &
     &                    DiaU2wrk(i,j,M2bstr)-DiaU2wrk(i,j,M2sstr))*   &
     &                   cff1
        END DO
!
!  Couple and update new solution.
!
        DO k=1,N(ng)
          DO i=IstrU,Iend
            u(i,j,k,nnew)=u(i,j,k,nnew)-DC(i,0)
            u(i,j,k,nnew)=u(i,j,k,nnew)*umask(i,j)
            DiaU3wrk(i,j,k,M3pgrd)=DiaU3wrk(i,j,k,M3pgrd)-              &
     &                             Dwrk(i,M2pgrd)
            DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)-              &
     &                             Dwrk(i,M2bstr)
            DiaU3wrk(i,j,k,M3fcor)=DiaU3wrk(i,j,k,M3fcor)-              &
     &                             Dwrk(i,M2fcor)
            DiaU3wrk(i,j,k,M3xvis)=DiaU3wrk(i,j,k,M3xvis)-              &
     &                             Dwrk(i,M2xvis)
            DiaU3wrk(i,j,k,M3yvis)=DiaU3wrk(i,j,k,M3yvis)-              &
     &                             Dwrk(i,M2yvis)
            DiaU3wrk(i,j,k,M3hvis)=DiaU3wrk(i,j,k,M3hvis)-              &
     &                             Dwrk(i,M2hvis)
            DiaU3wrk(i,j,k,M3xadv)=DiaU3wrk(i,j,k,M3xadv)-              &
     &                             Dwrk(i,M2xadv)
            DiaU3wrk(i,j,k,M3yadv)=DiaU3wrk(i,j,k,M3yadv)-              &
     &                             Dwrk(i,M2yadv)
            DiaU3wrk(i,j,k,M3hadv)=DiaU3wrk(i,j,k,M3hadv)-              &
     &                             Dwrk(i,M2hadv)
          END DO
        END DO
        DO k=1,N(ng)
          DO i=IstrU,Iend
            DO idiag=1,NDM3d
              DiaU3wrk(i,j,k,idiag)=DiaU3wrk(i,j,k,idiag)*umask(i,j)
            END DO
          END DO
        END DO
!
!-----------------------------------------------------------------------
!  Time step momentum equation in the ETA-direction.
!-----------------------------------------------------------------------
!
        IF (j.ge.JstrV) THEN
          DO i=Istr,Iend
            AK(i,0)=0.5_r8*(Akv(i,j-1,0)+                               &
     &                      Akv(i,j  ,0))
            DO k=1,N(ng)
              AK(i,k)=0.5_r8*(Akv(i,j-1,k)+                             &
     &                        Akv(i,j  ,k))
              Hzk(i,k)=0.5_r8*(Hz(i,j-1,k)+                             &
     &                         Hz(i,j  ,k))
              oHz(i,k)=1.0_r8/Hzk(i,k)
            END DO
          END DO
!
!  Time step right-hand-side terms.
!
          IF (iic(ng).eq.ntfirst(ng)) THEN
            cff=0.25_r8*dt(ng)
          ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
            cff=0.25_r8*dt(ng)*3.0_r8/2.0_r8
          ELSE
            cff=0.25_r8*dt(ng)*23.0_r8/12.0_r8
          END IF
          DO i=Istr,Iend
            DC(i,0)=cff*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
          END DO
          DO k=1,N(ng)
            DO i=Istr,Iend
              v(i,j,k,nnew)=v(i,j,k,nnew)+DC(i,0)*rv(i,j,k,nrhs)
              v(i,j,k,nnew)=v(i,j,k,nnew)*oHz(i,k)
              DO idiag=1,M3pgrd
                DiaV3wrk(i,j,k,idiag)=(DiaV3wrk(i,j,k,idiag)+           &
     &                                 DC(i,0)*                         &
     &                                 DiaRV(i,j,k,nrhs,idiag))*        &
     &                                oHz(i,k)
              END DO
              DiaV3wrk(i,j,k,M3xvis)=DiaV3wrk(i,j,k,M3xvis)*oHz(i,k)
              DiaV3wrk(i,j,k,M3yvis)=DiaV3wrk(i,j,k,M3yvis)*oHz(i,k)
              DiaV3wrk(i,j,k,M3hvis)=DiaV3wrk(i,j,k,M3hvis)*oHz(i,k)
              DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)*oHz(i,k)
              DiaV3wrk(i,j,k,M3rate)=DiaV3wrk(i,j,k,M3rate)*oHz(i,k)
            END DO
          END DO
!
!  Use conservative, parabolic spline reconstruction of vertical
!  viscosity derivatives.  Then, time step vertical viscosity term
!  implicitly by solving a tridiagonal system.
!
          cff1=1.0_r8/6.0_r8
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              FC(i,k)=cff1*Hzk(i,k  )-dt(ng)*AK(i,k-1)*oHz(i,k  )
              CF(i,k)=cff1*Hzk(i,k+1)-dt(ng)*AK(i,k+1)*oHz(i,k+1)
            END DO
          END DO
          DO i=Istr,Iend
            CF(i,0)=0.0_r8
            DC(i,0)=0.0_r8
          END DO
!
!  LU decomposition and forward substitution.
!
          cff1=1.0_r8/3.0_r8
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              BC(i,k)=cff1*(Hzk(i,k)+Hzk(i,k+1))+                       &
     &                dt(ng)*AK(i,k)*(oHz(i,k)+oHz(i,k+1))
              cff=1.0_r8/(BC(i,k)-FC(i,k)*CF(i,k-1))
              CF(i,k)=cff*CF(i,k)
              DC(i,k)=cff*(v(i,j,k+1,nnew)-v(i,j,k,nnew)-               &
     &                     FC(i,k)*DC(i,k-1))
            END DO
          END DO
!
!  Backward substitution.
!
          DO i=Istr,Iend
            DC(i,N(ng))=0.0_r8
          END DO
          DO k=N(ng)-1,1,-1
            DO i=Istr,Iend
              DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
            END DO
          END DO
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              DC(i,k)=DC(i,k)*AK(i,k)
              cff=dt(ng)*oHz(i,k)*(DC(i,k)-DC(i,k-1))
              v(i,j,k,nnew)=v(i,j,k,nnew)+cff
              DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)+cff
            END DO
          END DO
!
!  Replace INTERIOR POINTS incorrect vertical mean with more accurate
!  barotropic component, vbar=DV_avg1/(D*om_v). Recall that, D=CF(:,0).
!
          DO i=Istr,Iend
            CF(i,0)=Hzk(i,1)
            DC(i,0)=v(i,j,1,nnew)*Hzk(i,1)
            Dwrk(i,M2pgrd)=DiaV3wrk(i,j,1,M3pgrd)*Hzk(i,1)
            Dwrk(i,M2bstr)=DiaV3wrk(i,j,1,M3vvis)*Hzk(i,1)
            Dwrk(i,M2fcor)=DiaV3wrk(i,j,1,M3fcor)*Hzk(i,1)
            Dwrk(i,M2xvis)=DiaV3wrk(i,j,1,M3xvis)*Hzk(i,1)
            Dwrk(i,M2yvis)=DiaV3wrk(i,j,1,M3yvis)*Hzk(i,1)
            Dwrk(i,M2hvis)=DiaV3wrk(i,j,1,M3hvis)*Hzk(i,1)
            Dwrk(i,M2xadv)=DiaV3wrk(i,j,1,M3xadv)*Hzk(i,1)
            Dwrk(i,M2yadv)=DiaV3wrk(i,j,1,M3yadv)*Hzk(i,1)
            Dwrk(i,M2hadv)=DiaV3wrk(i,j,1,M3hadv)*Hzk(i,1)
          END DO
          DO k=2,N(ng)
            DO i=Istr,Iend
              CF(i,0)=CF(i,0)+Hzk(i,k)
              DC(i,0)=DC(i,0)+v(i,j,k,nnew)*Hzk(i,k)
              Dwrk(i,M2pgrd)=Dwrk(i,M2pgrd)+                            &
     &                       DiaV3wrk(i,j,k,M3pgrd)*Hzk(i,k)
              Dwrk(i,M2bstr)=Dwrk(i,M2bstr)+                            &
     &                       DiaV3wrk(i,j,k,M3vvis)*Hzk(i,k)
              Dwrk(i,M2fcor)=Dwrk(i,M2fcor)+                            &
     &                       DiaV3wrk(i,j,k,M3fcor)*Hzk(i,k)
              Dwrk(i,M2xvis)=Dwrk(i,M2xvis)+                            &
     &                       DiaV3wrk(i,j,k,M3xvis)*Hzk(i,k)
              Dwrk(i,M2yvis)=Dwrk(i,M2yvis)+                            &
     &                       DiaV3wrk(i,j,k,M3yvis)*Hzk(i,k)
              Dwrk(i,M2hvis)=Dwrk(i,M2hvis)+                            &
     &                       DiaV3wrk(i,j,k,M3hvis)*Hzk(i,k)
              Dwrk(i,M2xadv)=Dwrk(i,M2xadv)+                            &
     &                       DiaV3wrk(i,j,k,M3xadv)*Hzk(i,k)
              Dwrk(i,M2yadv)=Dwrk(i,M2yadv)+                            &
     &                       DiaV3wrk(i,j,k,M3yadv)*Hzk(i,k)
              Dwrk(i,M2hadv)=Dwrk(i,M2hadv)+                            &
     &                       DiaV3wrk(i,j,k,M3hadv)*Hzk(i,k)
            END DO
          END DO
          DO i=Istr,Iend
            cff1=1.0_r8/(CF(i,0)*om_v(i,j))
            DC(i,0)=(DC(i,0)*om_v(i,j)-DV_avg1(i,j))*cff1    ! recursive
            DO idiag=1,M2pgrd
              Dwrk(i,idiag)=(Dwrk(i,idiag)*om_v(i,j)-                   &
     &                       DiaV2wrk(i,j,idiag))*cff1
            END DO
            Dwrk(i,M2bstr)=(Dwrk(i,M2bstr)*om_v(i,j)-                   &
     &                      DiaV2wrk(i,j,M2bstr)-DiaV2wrk(i,j,M2sstr))* &
     &                     cff1
          END DO
!
!  Couple and update new solution.
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              v(i,j,k,nnew)=v(i,j,k,nnew)-DC(i,0)
              v(i,j,k,nnew)=v(i,j,k,nnew)*vmask(i,j)
              DiaV3wrk(i,j,k,M3pgrd)=DiaV3wrk(i,j,k,M3pgrd)-            &
     &                               Dwrk(i,M2pgrd)
              DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)-            &
     &                               Dwrk(i,M2bstr)
              DiaV3wrk(i,j,k,M3fcor)=DiaV3wrk(i,j,k,M3fcor)-            &
     &                               Dwrk(i,M2fcor)
              DiaV3wrk(i,j,k,M3xvis)=DiaV3wrk(i,j,k,M3xvis)-            &
     &                               Dwrk(i,M2xvis)
              DiaV3wrk(i,j,k,M3yvis)=DiaV3wrk(i,j,k,M3yvis)-            &
     &                               Dwrk(i,M2yvis)
              DiaV3wrk(i,j,k,M3hvis)=DiaV3wrk(i,j,k,M3hvis)-            &
     &                               Dwrk(i,M2hvis)
              DiaV3wrk(i,j,k,M3xadv)=DiaV3wrk(i,j,k,M3xadv)-            &
     &                               Dwrk(i,M2xadv)
              DiaV3wrk(i,j,k,M3yadv)=DiaV3wrk(i,j,k,M3yadv)-            &
     &                               Dwrk(i,M2yadv)
              DiaV3wrk(i,j,k,M3hadv)=DiaV3wrk(i,j,k,M3hadv)-            &
     &                               Dwrk(i,M2hadv)
            END DO
          END DO
          DO k=1,N(ng)
            DO i=Istr,Iend
              DO idiag=1,NDM3d
                DiaV3wrk(i,j,k,idiag)=DiaV3wrk(i,j,k,idiag)*vmask(i,j)
              END DO
            END DO
          END DO
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Set lateral boundary conditions.
!-----------------------------------------------------------------------
!
      CALL u3dbc_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj, N(ng),                       &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 nstp, nnew,                                      &
     &                 u)
      CALL v3dbc_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj, N(ng),                       &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 nstp, nnew,                                      &
     &                 v)
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
              DO k=1,N(ng)
                cff1=1.0_r8/(on_u(i,j)*                                 &
     &                       0.5_r8*(z_w(i-1,j,k)-z_w(i-1,j,k-1)+       &
     &                               z_w(i  ,j,k)-z_w(i  ,j,k-1)))
                u(i,j,k,nnew)=SOURCES(ng)%Qsrc(is,k)*cff1
              END DO
            ELSE
              DO k=1,N(ng)
                cff1=1.0_r8/(om_v(i,j)*                                 &
     &                       0.5_r8*(z_w(i,j-1,k)-z_w(i,j-1,k-1)+       &
     &                               z_w(i,j  ,k)-z_w(i,j  ,k-1)))
                v(i,j,k,nnew)=SOURCES(ng)%Qsrc(is,k)*cff1
              END DO
            END IF
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Couple 2D and 3D momentum equations.
!-----------------------------------------------------------------------
!
!  Couple velocity component in the XI-direction.
!
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          DC(i,0)=0.0_r8
          CF(i,0)=0.0_r8
          FC(i,0)=0.0_r8
        END DO
!
!  Compute thicknesses of U-boxes DC(i,1:N), total depth of the water
!  column DC(i,0), and incorrect vertical mean CF(i,0).  Notice that
!  barotropic component is replaced with its fast-time averaged
!  values.
!
        DO k=1,N(ng)
          DO i=IstrP,IendT
            cff=0.5_r8*on_u(i,j)
            DC(i,k)=cff*(Hz(i,j,k)+Hz(i-1,j,k))
            DC(i,0)=DC(i,0)+DC(i,k)
            CF(i,0)=CF(i,0)+                                            &
     &              DC(i,k)*u(i,j,k,nnew)
          END DO
        END DO
        DO i=IstrP,IendT
          cff1=DC(i,0)                                  ! intermediate
          DC(i,0)=1.0_r8/DC(i,0)                        ! recursive
          CF(i,0)=DC(i,0)*(CF(i,0)-DU_avg1(i,j))        ! recursive
          ubar(i,j,1)=DC(i,0)*DU_avg1(i,j)
          ubar(i,j,2)=ubar(i,j,1)
          DiaU2wrk(i,j,M2rate)=ubar(i,j,1)-DiaU2int(i,j,M2rate)*DC(i,0)
          DiaU2int(i,j,M2rate)=ubar(i,j,1)*cff1
        END DO
!
!  Convert the units of the fast-time integrated change in mass flux
!  diagnostic values to change in velocity (m s-1).
!
        DO idiag=1,NDM2d-1
          DO i=IstrP,IendT
            DiaU2wrk(i,j,idiag)=DC(i,0)*DiaU2wrk(i,j,idiag)
            DiaU2wrk(i,j,idiag)=DiaU2wrk(i,j,idiag)*umask(i,j)
          END DO
        END DO
!
!  Replace only BOUNDARY POINTS incorrect vertical mean with more
!  accurate barotropic component, ubar=DU_avg1/(D*on_u). Recall that,
!  D=CF(:,0).
!
!  NOTE:  Only the BOUNDARY POINTS need to be replaced. Avoid redundant
!         update in the interior again for computational purposes which
!         will not affect the nonlinear code.  However, the adjoint
!         code is wrong because the interior solution is corrected
!         twice. The replacement is avoided if the boundary edge is
!         periodic. The J-loop is pipelined, so we need to do a special
!         test for the southern and northern domain edges.
!
        IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
          IF (DOMAIN(ng)%Western_Edge(tile)) THEN
            DO k=1,N(ng)
              u(Istr,j,k,nnew)=u(Istr,j,k,nnew)-CF(Istr,0)
              u(Istr,j,k,nnew)=u(Istr,j,k,nnew)*                        &
     &                         umask(Istr,j)
            END DO
          END IF
        END IF
!
        IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
          IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
            DO k=1,N(ng)
              u(Iend+1,j,k,nnew)=u(Iend+1,j,k,nnew)-CF(Iend+1,0)
              u(Iend+1,j,k,nnew)=u(Iend+1,j,k,nnew)*                    &
     &                           umask(Iend+1,j)
            END DO
          END IF
        END IF
!
        IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
          IF (j.eq.0) THEN                           ! southern boundary
            DO k=1,N(ng)                             ! J-loop pipelined
              DO i=IstrU,Iend
                u(i,j,k,nnew)=u(i,j,k,nnew)-CF(i,0)
                u(i,j,k,nnew)=u(i,j,k,nnew)*                            &
     &                        umask(i,j)
              END DO
            END DO
          END IF
        END IF
!
        IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
          IF (j.eq.Mm(ng)+1) THEN                    ! northern boundary
            DO k=1,N(ng)                             ! J-loop pipelined
              DO i=IstrU,Iend
                u(i,j,k,nnew)=u(i,j,k,nnew)-CF(i,0)
                u(i,j,k,nnew)=u(i,j,k,nnew)*                            &
     &                        umask(i,j)
              END DO
            END DO
          END IF
        END IF
!
!  Compute correct mass flux, Hz*u/n.
!
        DO k=N(ng),1,-1
          DO i=IstrP,IendT
            Huon(i,j,k)=0.5_r8*(Huon(i,j,k)+u(i,j,k,nnew)*DC(i,k))
            FC(i,0)=FC(i,0)+Huon(i,j,k)
            DiaU3wrk(i,j,k,M3rate)=u(i,j,k,nnew)-DiaU3wrk(i,j,k,M3rate)
          END DO
        END DO
        DO i=IstrP,IendT
          FC(i,0)=DC(i,0)*(FC(i,0)-DU_avg2(i,j))        ! recursive
        END DO
        DO k=1,N(ng)
          DO i=IstrP,IendT
            Huon(i,j,k)=Huon(i,j,k)-DC(i,k)*FC(i,0)
          END DO
        END DO
!
!  Couple velocity component in the ETA-direction.
!
        IF (j.ge.Jstr) THEN
          DO i=IstrT,IendT
            DC(i,0)=0.0_r8
            CF(i,0)=0.0_r8
            FC(i,0)=0.0_r8
          END DO
!
!  Compute thicknesses of V-boxes DC(i,1:N), total depth of the water
!  column DC(i,0), and incorrect vertical mean CF(i,0).  Notice that
!  barotropic component is replaced with its fast-time averaged
!  values.
!
          DO k=1,N(ng)
            DO i=IstrT,IendT
              cff=0.5_r8*om_v(i,j)
              DC(i,k)=cff*(Hz(i,j,k)+Hz(i,j-1,k))
              DC(i,0)=DC(i,0)+DC(i,k)
              CF(i,0)=CF(i,0)+                                          &
     &                DC(i,k)*v(i,j,k,nnew)
            END DO
          END DO
          DO i=IstrT,IendT
            cff1=DC(i,0)                                 ! Intermediate
            DC(i,0)=1.0_r8/DC(i,0)                       ! recursive
            CF(i,0)=DC(i,0)*(CF(i,0)-DV_avg1(i,j))       ! recursive
            vbar(i,j,1)=DC(i,0)*DV_avg1(i,j)
            vbar(i,j,2)=vbar(i,j,1)
            DiaV2wrk(i,j,M2rate)=vbar(i,j,1)-                           &
     &                           DiaV2int(i,j,M2rate)*DC(i,0)
            DiaV2int(i,j,M2rate)=vbar(i,j,1)*cff1
          END DO
!
!  Convert the units of the fast-time integrated change in mass flux
!  diagnostic values to change in velocity (m s-1).
!
          DO idiag=1,NDM2d-1
            DO i=IstrT,IendT
              DiaV2wrk(i,j,idiag)=DC(i,0)*DiaV2wrk(i,j,idiag)
              DiaV2wrk(i,j,idiag)=DiaV2wrk(i,j,idiag)*vmask(i,j)
            END DO
          END DO
!
!  Replace only BOUNDARY POINTS incorrect vertical mean with more
!  accurate barotropic component, vbar=DV_avg1/(D*om_v).  Recall that,
!  D=CF(:,0).
!
!  NOTE:  Only the BOUNDARY POINTS need to be replaced. Avoid redundant
!         update in the interior again for computational purposes which
!         will not affect the nonlinear code.  However, the adjoint
!         code is wrong because the interior solution is corrected
!         twice. The replacement is avoided if the boundary edge is
!         periodic. The J-loop is pipelined, so we need to do a special
!         test for the southern and northern domain edges.
!
          IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
            IF (DOMAIN(ng)%Western_Edge(tile)) THEN
              DO k=1,N(ng)
                v(Istr-1,j,k,nnew)=v(Istr-1,j,k,nnew)-CF(Istr-1,0)
                v(Istr-1,j,k,nnew)=v(Istr-1,j,k,nnew)*                  &
     &                             vmask(Istr-1,j)
              END DO
            END IF
          END IF
!
          IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
            IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
              DO k=1,N(ng)
                v(Iend+1,j,k,nnew)=v(Iend+1,j,k,nnew)-CF(Iend+1,0)
                v(Iend+1,j,k,nnew)=v(Iend+1,j,k,nnew)*                  &
     &                             vmask(Iend+1,j)
              END DO
            END IF
          END IF
!
          IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
            IF (j.eq.1) THEN                         ! southern boundary
              DO k=1,N(ng)                           ! J-loop pipelined
                DO i=Istr,Iend
                  v(i,j,k,nnew)=v(i,j,k,nnew)-CF(i,0)
                  v(i,j,k,nnew)=v(i,j,k,nnew)*                          &
     &                          vmask(i,j)
                END DO
              END DO
            END IF
          END IF
!
          IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
            IF (j.eq.Mm(ng)+1) THEN                  ! northern boundary
              DO k=1,N(ng)                           ! J-loop pipelined
                DO i=Istr,Iend
                  v(i,j,k,nnew)=v(i,j,k,nnew)-CF(i,0)
                  v(i,j,k,nnew)=v(i,j,k,nnew)*                          &
     &                          vmask(i,j)
                END DO
              END DO
            END IF
          END IF
!
!  Compute correct mass flux, Hz*v/m.
!
          DO k=N(ng),1,-1
            DO i=IstrT,IendT
              Hvom(i,j,k)=0.5_r8*(Hvom(i,j,k)+v(i,j,k,nnew)*DC(i,k))
              FC(i,0)=FC(i,0)+Hvom(i,j,k)
              DiaV3wrk(i,j,k,M3rate)=v(i,j,k,nnew)-                     &
     &                               DiaV3wrk(i,j,k,M3rate)
            END DO
          END DO
          DO i=IstrT,IendT
            FC(i,0)=DC(i,0)*(FC(i,0)-DV_avg2(i,j))      ! recursive
          END DO
          DO k=1,N(ng)
            DO i=IstrT,IendT
              Hvom(i,j,k)=Hvom(i,j,k)-DC(i,k)*FC(i,0)
            END DO
          END DO
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Exchange boundary data.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_u3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          u(:,:,:,nnew))
        CALL exchange_v3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          v(:,:,:,nnew))
        CALL exchange_u3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          Huon)
        CALL exchange_v3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          Hvom)
!
        DO k=1,2
          CALL exchange_u2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            ubar(:,:,k))
          CALL exchange_v2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            vbar(:,:,k))
        END DO
      END IF
      CALL mp_exchange3d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj, 1, N(ng),                 &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    u(:,:,:,nnew), v(:,:,:,nnew),                 &
     &                    Huon, Hvom)
!
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    ubar(:,:,1), vbar(:,:,1),                     &
     &                    ubar(:,:,2), vbar(:,:,2))
!
      RETURN
      END SUBROUTINE step3d_uv_tile
      END MODULE step3d_uv_mod
