!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_k.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'keepfr3dflag')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kh_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'km_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ktop_plume')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_mynn_edmf(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_mynn_edmf(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'k22_shallow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kbcon_shallow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ktop_shallow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'k22_shallow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kbcon_shallow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ktop_shallow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kbcon_deep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_diag(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_diag(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ktop_deep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_diag(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_diag(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'k22_deep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_diag(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_diag(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kdcldtop')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kdcldbas')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kext_ql')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kext_qic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kext_qip')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kext_qid')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kext_qs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kext_qg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kext_qh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kext_qa')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kext_ft_qic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kext_ft_qip')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kext_ft_qid')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kext_ft_qs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kext_ft_qg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.32
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.32
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
