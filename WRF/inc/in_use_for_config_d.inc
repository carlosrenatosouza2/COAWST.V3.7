!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_d.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'dp3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlf2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'di3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.50
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.50
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'di3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.51
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.51
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'di3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.52
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.52
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'di3d_2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.52
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.52
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'di3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'di3d_2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'di3d_3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_rain_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_graupel_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_hail_gsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.16
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.16
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.16
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.16
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.16
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.16
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.17
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.17
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.17
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.17
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.17
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.17
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.18
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.18
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.18
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.18
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.18
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.18
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.26
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.26
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.26
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.26
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.26
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.26
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.50
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.50
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.50
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.50
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.51
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.51
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.51
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.51
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.52
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.52
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.52
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.52
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlg_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dl_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'drelr_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'drelb_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'drelg_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlg_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dl_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlg_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dl_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'deeprechxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzsnow1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzsnow2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzsnow3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzsnow4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzsnow5')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzo1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzo2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzo3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzo4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'detr_edkf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dqke')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_mynn_tkebudget(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_mynn_tkebudget(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'det_thl3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_mynn_output(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_mynn_output(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'det_sqv3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_mynn_output(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_mynn_output(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%km_opt(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%km_opt(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dp3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'du3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dsubcld2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ddr_kf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%kf_edrates(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%kf_edrates(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'der_kf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%kf_edrates(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%kf_edrates(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlf2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dwten_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'diten_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfgdp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grav_settling(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grav_settling(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfgdp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grav_settling(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grav_settling(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dif_analysis')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dif_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dif_yyy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_fdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_fdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dtaux3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dtauy3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dusfcg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dvsfcg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_w')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ph')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_phb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ph0')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_php')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_p')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ww')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_mu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tke')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_al')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_alt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tslb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smois')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smfr3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_keepfr3dflag')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_soilt1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tsnav')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_qvg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tten_rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_w')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ph')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_phb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ph0')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_php')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_p')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ww')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_mu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tke')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_al')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_alt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tslb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smois')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smfr3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_keepfr3dflag')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_soilt1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tsnav')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_qvg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tten_rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_w')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ph')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_phb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ph0')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_php')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_p')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ww')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_mu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tke')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_al')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_alt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tslb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smois')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smfr3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_keepfr3dflag')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_soilt1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tsnav')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_qvg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tten_rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dum_yyy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%fft_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%fft_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzdxf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzdyf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfu1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfd1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dz_lake3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dz3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dir')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%diag_nwp2.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%diag_nwp2.EQ.1
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
