!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_t.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'turbtype3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taod5503d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taod5502d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.50
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.50
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.51
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.51
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.52
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.52
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taucldc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taucldi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tlwdn')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tlwup')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taucldc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taucldi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taucldc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taucldi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taucldc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taucldi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tswdn')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tswup')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taucldc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taucldi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_sfclay_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_sfclay_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trb_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw1_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw2_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgb_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgr_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgrl_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trl_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgl_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tbl_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trb_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw1_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw2_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgb_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trl_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgl_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tbl_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsk_rural')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trb_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw1_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw2_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgb_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tlev_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw1lev_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw2lev_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tglev_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tflev_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trl_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgl_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tbl_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsk_rural')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tahxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsnoxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taussxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2mvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2mbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tradxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_grnd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_veg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2m_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2m_min')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2clm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_ref2m')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno_s1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno_s2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno_s3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno_s4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno_s5')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno5')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno6')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno7')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno8')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno9')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno10')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake5')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake6')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake7')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake8')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake9')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake10')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2obs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tkair')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssn1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssno1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssn2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssno2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssn3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssno3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssn4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssno4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsk_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tslb_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tr_urb2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tb_urb2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_urb2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tc_urb2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_urb2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_rul2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trl_urb3d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tbl_urb3d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgl_urb3d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thl_up')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thv_up')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tauresx2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tauresy2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tpert2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'turbtype3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'te_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thup_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.12
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.12
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tpert2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'timec_kf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%kf_edrates(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%kf_edrates(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thlsrc_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thvlsrc_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tkeavg_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tophgt_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thlu_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thvu_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thlu_emf_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tkeavg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th2_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th2_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tob_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tob_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th2_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th2_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tob_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tob_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tml')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t0ml')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tmoml')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tml')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t0ml')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tmoml')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_upstream_x')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_upstream_x_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_upstream_y')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_upstream_y_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_x')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_x_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_y')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_y_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_largescale')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_largescale_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_largescale')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_largescale_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soil_forcing_val')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soil_forcing_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_soil')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_t_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2min')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tt2min')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tt2max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2std')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tq2min')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tq2max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tskintempmin')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tskintempmax')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tspduv10max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'traincvmax')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trainncvmax')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_max_p')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nest_interp_coord.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nest_interp_coord.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_min_p')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nest_interp_coord.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nest_interp_coord.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'traj_i')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%traj_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%traj_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'traj_j')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%traj_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%traj_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'traj_k')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%traj_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%traj_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'traj_lat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%traj_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%traj_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'traj_long')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%traj_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%traj_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_hour')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_q')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_psfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_glw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_gsw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_hfx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_lh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_tslb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_clw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_rainc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_rainnc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_u_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_v_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_gph_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_th_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_p_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_w_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_hour')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_q')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_psfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_glw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_gsw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_hfx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_lh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_tslb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_clw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_rainc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_rainnc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_u_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_v_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_gph_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_th_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_cldfrac2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_wvp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_lwp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_iwp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_swp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_lwp_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_iwp_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_re_qc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_re_qi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_re_qs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_re_qc_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_re_qi_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_tau_qc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_tau_qi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_tau_qs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_tau_qc_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_tau_qi_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_cbaseht')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_ctopht')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_cbaseht_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_ctopht_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_clrnidx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_p_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_w_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_swdown')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_swddni')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_swddif')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_swdownc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_swddnic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_swdown2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_swddni2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_swddif2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_swdownc2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_swddnic2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%fft_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%fft_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tign_g')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%fmoisti_run(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%fmoisti_run(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_grnd2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tkmg3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tkdry3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tksatu3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.30
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.30
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tempc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.30
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.30
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tempc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_pl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%p_lev_diags.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%p_lev_diags.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'td_pl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%p_lev_diags.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%p_lev_diags.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_zl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%z_lev_diags.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%z_lev_diags.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'td_zl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%z_lev_diags.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%z_lev_diags.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tcoli_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tornado_mask')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tornado_dur')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsk_mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mean_diag.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mean_diag.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mean_diag.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mean_diag.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th2_mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mean_diag.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mean_diag.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsk_diurn')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%diurnal_diag.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%diurnal_diag.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_diurn')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%diurnal_diag.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%diurnal_diag.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th2_diurn')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%diurnal_diag.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%diurnal_diag.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'temperature')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%diag_nwp2.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%diag_nwp2.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tpw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%diag_nwp2.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%diag_nwp2.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_qc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_qi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_qs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_qc_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_qi_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsk_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tavgsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tmn_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tmaxw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ttrop')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsk_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tavgsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tmn_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
