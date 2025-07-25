!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_r.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_rain_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_graupel_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_hail_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqrcuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqscuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqicuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rh_old_mp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rimi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.13
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.13
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.16
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.16
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.16
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.16
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.16
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.16
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.17
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.17
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.17
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.17
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.17
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.17
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.18
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.18
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.18
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.18
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.18
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.18
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.22
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.22
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.22
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.22
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.22
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.22
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.26
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.26
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.26
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.26
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.26
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.26
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.50
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.50
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.50
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.50
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rhopo3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.50
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.50
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'refl_10cm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.50
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.50
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.51
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.51
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.51
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.51
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rhopo3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.51
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.51
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'refl_10cm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.51
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.51
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.52
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.52
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.52
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.52
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rhopo3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.52
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.52
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rhopo3d_2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.52
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.52
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'refl_10cm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.52
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.52
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqrcuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.40
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.40
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqscuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.40
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.40
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqicuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.40
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.40
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.40
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.40
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.40
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.40
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.40
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.40
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rhopo3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rhopo3d_2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rhopo3d_3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'refl_10cm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'refl_10cm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%compute_radar_ref.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%compute_radar_ref.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'refd_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%compute_radar_ref.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%compute_radar_ref.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_rain_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_graupel_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_hail_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_rain_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_graupel_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_hail_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rhosnf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rtmassxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'runsfxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'runsbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rssunxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rsshaxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rechxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rivercondxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'riverbedxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rechclim')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rn_urb2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rs_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rv_up')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rt_up')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rc_up')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rc_mf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rprddp3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rliq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rucuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rvcuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rthcuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqvcuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqrcuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqccuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqscuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqicuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqcncuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqincuten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rcwp_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rlwp_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'riwp_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rprdsh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rliq2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rliq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rdcashten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqcdcshten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rainshvb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'radsave')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rushten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rvshten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rthshten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqvshten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqrshten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqcshten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqsshten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqishten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqgshten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqcnshten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqinshten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rundgdten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rvndgdten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rthndgdten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rphndgdten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqvndgdten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rmundgdten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rundgdten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rvndgdten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rthndgdten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rphndgdten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqvndgdten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rmundgdten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rh_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rh_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rh_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rh_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'raincvmax')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rainncvmax')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'raincvmean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rainncvmean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'raincvstd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rainncvstd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ru_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%fft_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%fft_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rv_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%fft_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%fft_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'r_0')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rain_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rh_fire')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ros')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ros_front')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rqvfrten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rthfrten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rain_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%fmoisti_run(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%fmoisti_run(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rh_fire')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%fmoisti_run(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%fmoisti_run(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rstoch')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sppt_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sppt_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_real')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sppt_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sppt_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_imag')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sppt_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sppt_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_real_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sppt_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sppt_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_real_yyy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sppt_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sppt_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_imag_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sppt_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sppt_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_imag_yyy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sppt_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sppt_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ru_tendf_stoch')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%skebs_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%skebs_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rv_tendf_stoch')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%skebs_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%skebs_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rt_tendf_stoch')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%skebs_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%skebs_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_real')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%skebs_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%skebs_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_imag')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%skebs_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%skebs_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_real_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%skebs_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%skebs_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_real_yyy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%skebs_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%skebs_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_imag_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%skebs_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%skebs_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_imag_yyy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%skebs_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%skebs_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_pert')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%rand_perturb_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%rand_perturb_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_real')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%rand_perturb_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%rand_perturb_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_imag')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%rand_perturb_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%rand_perturb_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_real_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%rand_perturb_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%rand_perturb_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_real_yyy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%rand_perturb_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%rand_perturb_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_imag_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%rand_perturb_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%rand_perturb_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_imag_yyy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%rand_perturb_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%rand_perturb_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_real')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%spp_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%spp_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_imag')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%spp_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%spp_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_real_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%spp_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%spp_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_real_yyy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%spp_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%spp_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_imag_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%spp_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%spp_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rand_imag_yyy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%spp_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%spp_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rh_pl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%p_lev_diags.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%p_lev_diags.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rh_zl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%z_lev_diags.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%z_lev_diags.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'radarvil')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_vil_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_vil_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'refd_com')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_radar_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_radar_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'refd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_radar_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_radar_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rscghis_2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%elec_physics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%elec_physics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rscghis_2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%elec_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%elec_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rscghis_2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%elec_physics.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%elec_physics.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rain')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%diag_nwp2.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%diag_nwp2.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%diag_nwp2.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%diag_nwp2.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_qc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_qi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_qs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_qc_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_qi_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rh_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rh_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
