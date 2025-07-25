      MODULE ROMS_export_mod
!
!svn $Id: roms_export.F 1054 2021-03-06 19:47:12Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This module contains several routines to prepare ROMS fields to     !
!  export to other models.  It assumed that outside models  fields     !
!  (like observations) are located at RHO-points.                      !
!                                                                      !
!=======================================================================
!
      USE mod_kinds
      implicit none
      PUBLIC :: ROMS_export2d
      CONTAINS
!
!***********************************************************************
      SUBROUTINE ROMS_export2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          InpField,                               &
     &                          OutFmin, OutFmax,                       &
     &                          Npts, OutField,                         &
     &                          status)
!***********************************************************************
!
      USE mod_param
      USE mod_ncparam
!
      USE distribute_mod,  ONLY : mp_reduce
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, id, gtype
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: Npts
      integer, intent(out) :: status
      real(r8), intent(in) :: scale, add_offset
      real(r8), intent(out) :: OutFmin, OutFmax
      real(r8), intent(in)  :: InpField(LBi:,LBj:)
      real(r8), intent(out) :: OutField(:)
!
!  Local variable declarations.
!
      integer :: i, ij, j
      real(r8), parameter :: Large = 1.0E+20_r8
      real(r8), dimension(2) :: range
      real(r8), dimension(LBi:UBi,LBj:UBj) :: Awork
      character (len=3), dimension(2) :: op_handle
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
!  Compute export fields to RHO-points.
!-----------------------------------------------------------------------
!
      status=0
      range(1)= Large
      range(2)=-Large
!
!  RHO-type variables.
!
      IF (gtype.eq.r2dvar) THEN
        ij=0
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            ij=ij+1
            OutField(ij)=InpField(i,j)
            range(1)=MIN(range(1),OutField(ij))
            range(2)=MAX(range(2),OutField(ij))
          END DO
        END DO
!
!  U-type variables.
!
      ELSE IF (gtype.eq.u2dvar) THEN
        DO j=JstrR,JendR
          DO i=Istr,Iend
            Awork(i,j)=0.5_r8*(InpField(i  ,j)+                         &
     &                         InpField(i+1,j))
          END DO
        END DO
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=Jstr,Jend
            Awork(Istr-1,j)=Awork(Istr,j)
          END DO
        END IF
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=Jstr,Jend
            Awork(Iend+1,j)=Awork(Iend,j)
          END DO
        END IF
        IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
          Awork(Istr-1,Jstr-1)=0.5_r8*(Awork(Istr  ,Jstr-1)+            &
     &                                 Awork(Istr-1,Jstr  ))
        END IF
        IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
          Awork(Iend+1,Jstr-1)=0.5_r8*(Awork(Iend  ,Jstr-1)+            &
     &                                 Awork(Iend+1,Jstr  ))
        END IF
        IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
          Awork(Istr-1,Jend+1)=0.5_r8*(Awork(Istr-1,Jend  )+            &
     &                                 Awork(Istr  ,Jend+1))
        END IF
        IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          Awork(Iend+1,Jend+1)=0.5_r8*(Awork(Iend+1,Jend  )+            &
     &                                 Awork(Iend  ,Jend+1))
        END IF
!
!  Pack into export vector.
!
        ij=0
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            ij=ij+1
            OutField(ij)=Awork(i,j)
            range(1)=MIN(range(1),OutField(ij))
            range(2)=MAX(range(2),OutField(ij))
          END DO
        END DO
!
!  V-type variables.
!
      ELSE IF (gtype.eq.v2dvar) THEN
        DO j=Jstr,Jend
          DO i=IstrR,IendR
            Awork(i,j)=0.5_r8*(InpField(i,j  )+                         &
     &                         InpField(i,j+1))
          END DO
        END DO
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=Istr,Iend
            Awork(i,Jend+1)=Awork(i,Jend)
          END DO
        END IF
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=Istr,Iend
            Awork(i,Jstr-1)=Awork(i,Jstr)
          END DO
        END IF
        IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
          Awork(Istr-1,Jstr-1)=0.5_r8*(Awork(Istr  ,Jstr-1)+            &
     &                                 Awork(Istr-1,Jstr  ))
        END IF
        IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
          Awork(Iend+1,Jstr-1)=0.5_r8*(Awork(Iend  ,Jstr-1)+            &
     &                                 Awork(Iend+1,Jstr  ))
        END IF
        IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
          Awork(Istr-1,Jend+1)=0.5_r8*(Awork(Istr-1,Jend  )+            &
     &                                 Awork(Istr  ,Jend+1))
        END IF
        IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          Awork(Iend+1,Jend+1)=0.5_r8*(Awork(Iend+1,Jend  )+            &
     &                                 Awork(Iend  ,Jend+1))
        END IF
!
!  Pack into export vector.
!
        ij=0
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            ij=ij+1
            OutField(ij)=Awork(i,j)
            range(1)=MIN(range(1),OutField(ij))
            range(2)=MAX(range(2),OutField(ij))
          END DO
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Global reduction for imported field range values.
!-----------------------------------------------------------------------
!
      op_handle(1)='MIN'
      op_handle(2)='MAX'
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      OutFmin=range(1)
      OutFmax=range(2)
      END SUBROUTINE ROMS_export2d
      END MODULE roms_export_mod
