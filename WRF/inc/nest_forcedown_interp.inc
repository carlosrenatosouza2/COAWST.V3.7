!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nest_forcedown_interp.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( SIZE( grid%u_2, 1 ) * SIZE( grid%u_2, 3 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  grid%u_2,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%u_2,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_xstag,         &         ! stencil half width
                  .TRUE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,dummy_xs ,ngrid%u_bxs &
,dummy_xe ,ngrid%u_bxe &
,dummy_ys ,ngrid%u_bys &
,dummy_ye ,ngrid%u_bye &
,dummy_xs ,ngrid%u_btxs &
,dummy_xe ,ngrid%u_btxe &
,dummy_ys ,ngrid%u_btys &
,dummy_ye ,ngrid%u_btye &
,grid%dt,ngrid%dt&
                  ) 
ENDIF
IF ( SIZE( grid%v_2, 1 ) * SIZE( grid%v_2, 3 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  grid%v_2,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%v_2,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_ystag,         &         ! stencil half width
                  .FALSE., .TRUE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,dummy_xs ,ngrid%v_bxs &
,dummy_xe ,ngrid%v_bxe &
,dummy_ys ,ngrid%v_bys &
,dummy_ye ,ngrid%v_bye &
,dummy_xs ,ngrid%v_btxs &
,dummy_xe ,ngrid%v_btxe &
,dummy_ys ,ngrid%v_btys &
,dummy_ye ,ngrid%v_btye &
,grid%dt,ngrid%dt&
                  ) 
ENDIF
IF ( SIZE( grid%w_2, 1 ) * SIZE( grid%w_2, 3 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  grid%w_2,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( ckde, ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%w_2,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( nkde, nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,dummy_xs ,ngrid%w_bxs &
,dummy_xe ,ngrid%w_bxe &
,dummy_ys ,ngrid%w_bys &
,dummy_ye ,ngrid%w_bye &
,dummy_xs ,ngrid%w_btxs &
,dummy_xe ,ngrid%w_btxe &
,dummy_ys ,ngrid%w_btys &
,dummy_ye ,ngrid%w_btye &
,grid%dt,ngrid%dt&
                  ) 
ENDIF
IF ( SIZE( grid%ph_2, 1 ) * SIZE( grid%ph_2, 3 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  grid%ph_2,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( ckde, ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%ph_2,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( nkde, nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,dummy_xs ,ngrid%ph_bxs &
,dummy_xe ,ngrid%ph_bxe &
,dummy_ys ,ngrid%ph_bys &
,dummy_ye ,ngrid%ph_bye &
,dummy_xs ,ngrid%ph_btxs &
,dummy_xe ,ngrid%ph_btxe &
,dummy_ys ,ngrid%ph_btys &
,dummy_ye ,ngrid%ph_btye &
,grid%dt,ngrid%dt&
                  ) 
ENDIF
IF ( SIZE( grid%t_2, 1 ) * SIZE( grid%t_2, 3 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  grid%t_2,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%t_2,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,dummy_xs ,ngrid%t_bxs &
,dummy_xe ,ngrid%t_bxe &
,dummy_ys ,ngrid%t_bys &
,dummy_ye ,ngrid%t_bye &
,dummy_xs ,ngrid%t_btxs &
,dummy_xe ,ngrid%t_btxe &
,dummy_ys ,ngrid%t_btys &
,dummy_ye ,ngrid%t_btye &
,grid%dt,ngrid%dt&
                  ) 
ENDIF
IF ( SIZE( grid%mu_2, 1 ) * SIZE( grid%mu_2, 2 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  grid%mu_2,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%mu_2,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,dummy_xs ,ngrid%mu_bxs &
,dummy_xe ,ngrid%mu_bxe &
,dummy_ys ,ngrid%mu_bys &
,dummy_ye ,ngrid%mu_bye &
,dummy_xs ,ngrid%mu_btxs &
,dummy_xe ,ngrid%mu_btxe &
,dummy_ys ,ngrid%mu_btys &
,dummy_ye ,ngrid%mu_btye &
,grid%dt,ngrid%dt&
                  ) 
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_moist
IF ( SIZE( moist, 1 ) * SIZE( moist, 3 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  moist(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%moist(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,moist_bxs(cjms,1,1,itrace) ,ngrid%moist_bxs(njms,1,1,itrace) &
,moist_bxe(cjms,1,1,itrace) ,ngrid%moist_bxe(njms,1,1,itrace) &
,moist_bys(cims,1,1,itrace) ,ngrid%moist_bys(nims,1,1,itrace) &
,moist_bye(cims,1,1,itrace) ,ngrid%moist_bye(nims,1,1,itrace) &
,moist_btxs(cjms,1,1,itrace) ,ngrid%moist_btxs(njms,1,1,itrace) &
,moist_btxe(cjms,1,1,itrace) ,ngrid%moist_btxe(njms,1,1,itrace) &
,moist_btys(cims,1,1,itrace) ,ngrid%moist_btys(nims,1,1,itrace) &
,moist_btye(cims,1,1,itrace) ,ngrid%moist_btye(nims,1,1,itrace) &
,grid%dt,ngrid%dt&
                  ) 
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
IF ( SIZE( dfi_moist, 1 ) * SIZE( dfi_moist, 3 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  dfi_moist(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%dfi_moist(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,dfi_moist_bxs(cjms,1,1,itrace) ,ngrid%dfi_moist_bxs(njms,1,1,itrace) &
,dfi_moist_bxe(cjms,1,1,itrace) ,ngrid%dfi_moist_bxe(njms,1,1,itrace) &
,dfi_moist_bys(cims,1,1,itrace) ,ngrid%dfi_moist_bys(nims,1,1,itrace) &
,dfi_moist_bye(cims,1,1,itrace) ,ngrid%dfi_moist_bye(nims,1,1,itrace) &
,dfi_moist_btxs(cjms,1,1,itrace) ,ngrid%dfi_moist_btxs(njms,1,1,itrace) &
,dfi_moist_btxe(cjms,1,1,itrace) ,ngrid%dfi_moist_btxe(njms,1,1,itrace) &
,dfi_moist_btys(cims,1,1,itrace) ,ngrid%dfi_moist_btys(nims,1,1,itrace) &
,dfi_moist_btye(cims,1,1,itrace) ,ngrid%dfi_moist_btye(nims,1,1,itrace) &
,grid%dt,ngrid%dt&
                  ) 
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
IF ( SIZE( scalar, 1 ) * SIZE( scalar, 3 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  scalar(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%scalar(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,scalar_bxs(cjms,1,1,itrace) ,ngrid%scalar_bxs(njms,1,1,itrace) &
,scalar_bxe(cjms,1,1,itrace) ,ngrid%scalar_bxe(njms,1,1,itrace) &
,scalar_bys(cims,1,1,itrace) ,ngrid%scalar_bys(nims,1,1,itrace) &
,scalar_bye(cims,1,1,itrace) ,ngrid%scalar_bye(nims,1,1,itrace) &
,scalar_btxs(cjms,1,1,itrace) ,ngrid%scalar_btxs(njms,1,1,itrace) &
,scalar_btxe(cjms,1,1,itrace) ,ngrid%scalar_btxe(njms,1,1,itrace) &
,scalar_btys(cims,1,1,itrace) ,ngrid%scalar_btys(nims,1,1,itrace) &
,scalar_btye(cims,1,1,itrace) ,ngrid%scalar_btye(nims,1,1,itrace) &
,grid%dt,ngrid%dt&
                  ) 
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
IF ( SIZE( dfi_scalar, 1 ) * SIZE( dfi_scalar, 3 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  dfi_scalar(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%dfi_scalar(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,dfi_scalar_bxs(cjms,1,1,itrace) ,ngrid%dfi_scalar_bxs(njms,1,1,itrace) &
,dfi_scalar_bxe(cjms,1,1,itrace) ,ngrid%dfi_scalar_bxe(njms,1,1,itrace) &
,dfi_scalar_bys(cims,1,1,itrace) ,ngrid%dfi_scalar_bys(nims,1,1,itrace) &
,dfi_scalar_bye(cims,1,1,itrace) ,ngrid%dfi_scalar_bye(nims,1,1,itrace) &
,dfi_scalar_btxs(cjms,1,1,itrace) ,ngrid%dfi_scalar_btxs(njms,1,1,itrace) &
,dfi_scalar_btxe(cjms,1,1,itrace) ,ngrid%dfi_scalar_btxe(njms,1,1,itrace) &
,dfi_scalar_btys(cims,1,1,itrace) ,ngrid%dfi_scalar_btys(nims,1,1,itrace) &
,dfi_scalar_btye(cims,1,1,itrace) ,ngrid%dfi_scalar_btye(nims,1,1,itrace) &
,grid%dt,ngrid%dt&
                  ) 
ENDIF
ENDDO
IF ( SIZE( grid%o3rad, 1 ) * SIZE( grid%o3rad, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%o3rad,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%o3rad,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_aerod
IF ( SIZE( aerod, 1 ) * SIZE( aerod, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  aerod(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%aerod(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_aerocu
IF ( SIZE( aerocu, 1 ) * SIZE( aerocu, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  aerocu(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%aerocu(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
ENDDO
IF ( SIZE( grid%om_tmp, 1 ) * SIZE( grid%om_tmp, 3 ) .GT. 1 ) THEN 
CALL c2f_interp (  &         
                  grid%om_tmp,   &       ! CD field
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%om_tmp,  &   ! ND field
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%grid_id,ngrid%grid_id&
                  ) 
ENDIF
IF ( SIZE( grid%om_s, 1 ) * SIZE( grid%om_s, 3 ) .GT. 1 ) THEN 
CALL c2f_interp (  &         
                  grid%om_s,   &       ! CD field
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%om_s,  &   ! ND field
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%grid_id,ngrid%grid_id&
                  ) 
ENDIF
IF ( SIZE( grid%om_u, 1 ) * SIZE( grid%om_u, 3 ) .GT. 1 ) THEN 
CALL c2f_interp (  &         
                  grid%om_u,   &       ! CD field
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%om_u,  &   ! ND field
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%grid_id,ngrid%grid_id&
                  ) 
ENDIF
IF ( SIZE( grid%om_v, 1 ) * SIZE( grid%om_v, 3 ) .GT. 1 ) THEN 
CALL c2f_interp (  &         
                  grid%om_v,   &       ! CD field
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%om_v,  &   ! ND field
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%grid_id,ngrid%grid_id&
                  ) 
ENDIF
IF ( SIZE( grid%om_ml, 1 ) * SIZE( grid%om_ml, 2 ) .GT. 1 ) THEN 
CALL c2f_interp (  &         
                  grid%om_ml,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%om_ml,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%grid_id,ngrid%grid_id&
                  ) 
ENDIF
IF ( SIZE( grid%ht_shad, 1 ) * SIZE( grid%ht_shad, 2 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  grid%ht_shad,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ht_shad,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,dummy_xs ,ngrid%ht_shad_bxs &
,dummy_xe ,ngrid%ht_shad_bxe &
,dummy_ys ,ngrid%ht_shad_bys &
,dummy_ye ,ngrid%ht_shad_bye &
,dummy_xs ,ngrid%ht_shad_btxs &
,dummy_xe ,ngrid%ht_shad_btxe &
,dummy_ys ,ngrid%ht_shad_btys &
,dummy_ye ,ngrid%ht_shad_btye &
,grid%dt,ngrid%dt&
                  ) 
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_tracer
IF ( SIZE( tracer, 1 ) * SIZE( tracer, 3 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  tracer(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%tracer(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,tracer_bxs(cjms,1,1,itrace) ,ngrid%tracer_bxs(njms,1,1,itrace) &
,tracer_bxe(cjms,1,1,itrace) ,ngrid%tracer_bxe(njms,1,1,itrace) &
,tracer_bys(cims,1,1,itrace) ,ngrid%tracer_bys(nims,1,1,itrace) &
,tracer_bye(cims,1,1,itrace) ,ngrid%tracer_bye(nims,1,1,itrace) &
,tracer_btxs(cjms,1,1,itrace) ,ngrid%tracer_btxs(njms,1,1,itrace) &
,tracer_btxe(cjms,1,1,itrace) ,ngrid%tracer_btxe(njms,1,1,itrace) &
,tracer_btys(cims,1,1,itrace) ,ngrid%tracer_btys(nims,1,1,itrace) &
,tracer_btye(cims,1,1,itrace) ,ngrid%tracer_btye(nims,1,1,itrace) &
,grid%dt,ngrid%dt&
                  ) 
ENDIF
ENDDO
IF ( SIZE( grid%field_sf, 1 ) * SIZE( grid%field_sf, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%field_sf,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%field_sf,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%field_pbl, 1 ) * SIZE( grid%field_pbl, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%field_pbl,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%field_pbl,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%field_conv, 1 ) * SIZE( grid%field_conv, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%field_conv,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%field_conv,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ru_tendf_stoch, 1 ) * SIZE( grid%ru_tendf_stoch, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%ru_tendf_stoch,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%ru_tendf_stoch,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_xstag,         &         ! stencil half width
                  .TRUE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rv_tendf_stoch, 1 ) * SIZE( grid%rv_tendf_stoch, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%rv_tendf_stoch,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%rv_tendf_stoch,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_ystag,         &         ! stencil half width
                  .FALSE., .TRUE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rt_tendf_stoch, 1 ) * SIZE( grid%rt_tendf_stoch, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%rt_tendf_stoch,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%rt_tendf_stoch,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rand_pert, 1 ) * SIZE( grid%rand_pert, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%rand_pert,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%rand_pert,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%pattern_spp_conv, 1 ) * SIZE( grid%pattern_spp_conv, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%pattern_spp_conv,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%pattern_spp_conv,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%pattern_spp_pbl, 1 ) * SIZE( grid%pattern_spp_pbl, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%pattern_spp_pbl,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%pattern_spp_pbl,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%pattern_spp_lsm, 1 ) * SIZE( grid%pattern_spp_lsm, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%pattern_spp_lsm,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%pattern_spp_lsm,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rstoch, 1 ) * SIZE( grid%rstoch, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%rstoch,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%rstoch,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%field_u_tend_perturb, 1 ) * SIZE( grid%field_u_tend_perturb, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%field_u_tend_perturb,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%field_u_tend_perturb,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_xstag,         &         ! stencil half width
                  .TRUE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%field_v_tend_perturb, 1 ) * SIZE( grid%field_v_tend_perturb, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%field_v_tend_perturb,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%field_v_tend_perturb,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_ystag,         &         ! stencil half width
                  .FALSE., .TRUE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%field_t_tend_perturb, 1 ) * SIZE( grid%field_t_tend_perturb, 3 ) .GT. 1 ) THEN 
CALL p2c (  &         
                  grid%field_t_tend_perturb,   &       ! CD field
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%field_t_tend_perturb,  &   ! ND field
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%pc_2, 1 ) * SIZE( grid%pc_2, 2 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  grid%pc_2,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%pc_2,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,dummy_xs ,ngrid%pc_bxs &
,dummy_xe ,ngrid%pc_bxe &
,dummy_ys ,ngrid%pc_bys &
,dummy_ye ,ngrid%pc_bye &
,dummy_xs ,ngrid%pc_btxs &
,dummy_xe ,ngrid%pc_btxe &
,dummy_ys ,ngrid%pc_btys &
,dummy_ye ,ngrid%pc_btye &
,grid%dt,ngrid%dt&
                  ) 
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
