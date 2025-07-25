!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_o.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'o3rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'o3rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'o3rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'o3rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'o3rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'o3rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'odis_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'odis_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'obs_savwt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%obs_nudge_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%obs_nudge_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'odis_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'odis_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'oc12d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'oa1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'oa2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'oa3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'oa4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ol1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ol2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ol3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ol4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'om_tmp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'om_s')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'om_depth')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'om_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'om_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'om_lat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'om_lon')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'om_ml')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'om_tini')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'om_sini')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
