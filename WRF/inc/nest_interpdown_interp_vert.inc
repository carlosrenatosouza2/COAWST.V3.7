!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nest_interpdown_interp_vert.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( SIZE( grid%u_2, 1 ) * SIZE( grid%u_2, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%u_2, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%v_2, 1 ) * SIZE( grid%v_2, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%v_2, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%w_2, 1 ) * SIZE( grid%w_2, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting_w( &
                                  grid%w_2, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  alt_w_c, alt_w_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%ph_2, 1 ) * SIZE( grid%ph_2, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting_w( &
                                  grid%ph_2, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  alt_w_c, alt_w_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%phb, 1 ) * SIZE( grid%phb, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting_w( &
                                  grid%phb, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  alt_w_c, alt_w_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%th_phy_m_t0, 1 ) * SIZE( grid%th_phy_m_t0, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%th_phy_m_t0, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%t_2, 1 ) * SIZE( grid%t_2, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%t_2, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%t_init, 1 ) * SIZE( grid%t_init, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%t_init, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%alb, 1 ) * SIZE( grid%alb, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%alb, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%pb, 1 ) * SIZE( grid%pb, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%pb, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_moist
IF ( SIZE( moist, 1 ) * SIZE( moist, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  moist(grid%sm31,grid%sm32,grid%sm33,itrace), & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
IF ( SIZE( dfi_moist, 1 ) * SIZE( dfi_moist, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  dfi_moist(grid%sm31,grid%sm32,grid%sm33,itrace), & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
ENDDO
IF ( SIZE( grid%qvold, 1 ) * SIZE( grid%qvold, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%qvold, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_scalar
IF ( SIZE( scalar, 1 ) * SIZE( scalar, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  scalar(grid%sm31,grid%sm32,grid%sm33,itrace), & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
IF ( SIZE( dfi_scalar, 1 ) * SIZE( dfi_scalar, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  dfi_scalar(grid%sm31,grid%sm32,grid%sm33,itrace), & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
ENDDO
IF ( SIZE( grid%o3rad, 1 ) * SIZE( grid%o3rad, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%o3rad, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_aerod
IF ( SIZE( aerod, 1 ) * SIZE( aerod, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  aerod(grid%sm31,grid%sm32,grid%sm33,itrace), & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_aerocu
IF ( SIZE( aerocu, 1 ) * SIZE( aerocu, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  aerocu(grid%sm31,grid%sm32,grid%sm33,itrace), & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
ENDDO
IF ( SIZE( grid%f_ice_phy, 1 ) * SIZE( grid%f_ice_phy, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%f_ice_phy, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%f_rain_phy, 1 ) * SIZE( grid%f_rain_phy, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%f_rain_phy, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%f_rimef_phy, 1 ) * SIZE( grid%f_rimef_phy, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%f_rimef_phy, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%h_diabatic, 1 ) * SIZE( grid%h_diabatic, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%h_diabatic, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%qv_diabatic, 1 ) * SIZE( grid%qv_diabatic, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%qv_diabatic, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%qc_diabatic, 1 ) * SIZE( grid%qc_diabatic, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%qc_diabatic, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%refl_10cm, 1 ) * SIZE( grid%refl_10cm, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%refl_10cm, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%mskf_refl_10cm, 1 ) * SIZE( grid%mskf_refl_10cm, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%mskf_refl_10cm, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%th_old, 1 ) * SIZE( grid%th_old, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%th_old, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%qv_old, 1 ) * SIZE( grid%qv_old, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%qv_old, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%vmi3d, 1 ) * SIZE( grid%vmi3d, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%vmi3d, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%di3d, 1 ) * SIZE( grid%di3d, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%di3d, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%rhopo3d, 1 ) * SIZE( grid%rhopo3d, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%rhopo3d, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%phii3d, 1 ) * SIZE( grid%phii3d, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%phii3d, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%vmi3d_2, 1 ) * SIZE( grid%vmi3d_2, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%vmi3d_2, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%di3d_2, 1 ) * SIZE( grid%di3d_2, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%di3d_2, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%rhopo3d_2, 1 ) * SIZE( grid%rhopo3d_2, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%rhopo3d_2, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%phii3d_2, 1 ) * SIZE( grid%phii3d_2, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%phii3d_2, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%vmi3d_3, 1 ) * SIZE( grid%vmi3d_3, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%vmi3d_3, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%di3d_3, 1 ) * SIZE( grid%di3d_3, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%di3d_3, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%rhopo3d_3, 1 ) * SIZE( grid%rhopo3d_3, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%rhopo3d_3, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%phii3d_3, 1 ) * SIZE( grid%phii3d_3, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%phii3d_3, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%itype, 1 ) * SIZE( grid%itype, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%itype, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%itype_2, 1 ) * SIZE( grid%itype_2, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%itype_2, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%itype_3, 1 ) * SIZE( grid%itype_3, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%itype_3, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%w_up, 1 ) * SIZE( grid%w_up, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%w_up, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%rthraten, 1 ) * SIZE( grid%rthraten, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%rthraten, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_tracer
IF ( SIZE( tracer, 1 ) * SIZE( tracer, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  tracer(grid%sm31,grid%sm32,grid%sm33,itrace), & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
ENDDO
IF ( SIZE( grid%vertstrucc, 1 ) * SIZE( grid%vertstrucc, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%vertstrucc, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%vertstrucs, 1 ) * SIZE( grid%vertstrucs, 3 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%vertstrucs, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
