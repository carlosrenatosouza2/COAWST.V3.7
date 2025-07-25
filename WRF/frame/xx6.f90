










!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nl_config.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE nl_set_auxinput17_end ( id_id , auxinput17_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_end
  INTEGER id_id
  model_config_rec%auxinput17_end(id_id) = auxinput17_end
  RETURN
END SUBROUTINE nl_set_auxinput17_end
SUBROUTINE nl_set_io_form_auxinput17 ( id_id , io_form_auxinput17 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput17
  INTEGER id_id
  model_config_rec%io_form_auxinput17 = io_form_auxinput17 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput17
SUBROUTINE nl_set_frames_per_auxinput17 ( id_id , frames_per_auxinput17 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput17
  INTEGER id_id
  model_config_rec%frames_per_auxinput17(id_id) = frames_per_auxinput17
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput17
SUBROUTINE nl_set_auxinput18_inname ( id_id , auxinput18_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput18_inname
  INTEGER id_id
  model_config_rec%auxinput18_inname = trim(auxinput18_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput18_inname
SUBROUTINE nl_set_auxinput18_outname ( id_id , auxinput18_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput18_outname
  INTEGER id_id
  model_config_rec%auxinput18_outname = trim(auxinput18_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput18_outname
SUBROUTINE nl_set_auxinput18_interval_y ( id_id , auxinput18_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_interval_y
  INTEGER id_id
  model_config_rec%auxinput18_interval_y(id_id) = auxinput18_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput18_interval_y
SUBROUTINE nl_set_auxinput18_interval_d ( id_id , auxinput18_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_interval_d
  INTEGER id_id
  model_config_rec%auxinput18_interval_d(id_id) = auxinput18_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput18_interval_d
SUBROUTINE nl_set_auxinput18_interval_h ( id_id , auxinput18_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_interval_h
  INTEGER id_id
  model_config_rec%auxinput18_interval_h(id_id) = auxinput18_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput18_interval_h
SUBROUTINE nl_set_auxinput18_interval_m ( id_id , auxinput18_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_interval_m
  INTEGER id_id
  model_config_rec%auxinput18_interval_m(id_id) = auxinput18_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput18_interval_m
SUBROUTINE nl_set_auxinput18_interval_s ( id_id , auxinput18_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_interval_s
  INTEGER id_id
  model_config_rec%auxinput18_interval_s(id_id) = auxinput18_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput18_interval_s
SUBROUTINE nl_set_auxinput18_interval ( id_id , auxinput18_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_interval
  INTEGER id_id
  model_config_rec%auxinput18_interval(id_id) = auxinput18_interval
  RETURN
END SUBROUTINE nl_set_auxinput18_interval
SUBROUTINE nl_set_auxinput18_begin_y ( id_id , auxinput18_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_begin_y
  INTEGER id_id
  model_config_rec%auxinput18_begin_y(id_id) = auxinput18_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput18_begin_y
SUBROUTINE nl_set_auxinput18_begin_d ( id_id , auxinput18_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_begin_d
  INTEGER id_id
  model_config_rec%auxinput18_begin_d(id_id) = auxinput18_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput18_begin_d
SUBROUTINE nl_set_auxinput18_begin_h ( id_id , auxinput18_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_begin_h
  INTEGER id_id
  model_config_rec%auxinput18_begin_h(id_id) = auxinput18_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput18_begin_h
SUBROUTINE nl_set_auxinput18_begin_m ( id_id , auxinput18_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_begin_m
  INTEGER id_id
  model_config_rec%auxinput18_begin_m(id_id) = auxinput18_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput18_begin_m
SUBROUTINE nl_set_auxinput18_begin_s ( id_id , auxinput18_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_begin_s
  INTEGER id_id
  model_config_rec%auxinput18_begin_s(id_id) = auxinput18_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput18_begin_s
SUBROUTINE nl_set_auxinput18_begin ( id_id , auxinput18_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_begin
  INTEGER id_id
  model_config_rec%auxinput18_begin(id_id) = auxinput18_begin
  RETURN
END SUBROUTINE nl_set_auxinput18_begin
SUBROUTINE nl_set_auxinput18_end_y ( id_id , auxinput18_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_end_y
  INTEGER id_id
  model_config_rec%auxinput18_end_y(id_id) = auxinput18_end_y
  RETURN
END SUBROUTINE nl_set_auxinput18_end_y
SUBROUTINE nl_set_auxinput18_end_d ( id_id , auxinput18_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_end_d
  INTEGER id_id
  model_config_rec%auxinput18_end_d(id_id) = auxinput18_end_d
  RETURN
END SUBROUTINE nl_set_auxinput18_end_d
SUBROUTINE nl_set_auxinput18_end_h ( id_id , auxinput18_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_end_h
  INTEGER id_id
  model_config_rec%auxinput18_end_h(id_id) = auxinput18_end_h
  RETURN
END SUBROUTINE nl_set_auxinput18_end_h
SUBROUTINE nl_set_auxinput18_end_m ( id_id , auxinput18_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_end_m
  INTEGER id_id
  model_config_rec%auxinput18_end_m(id_id) = auxinput18_end_m
  RETURN
END SUBROUTINE nl_set_auxinput18_end_m
SUBROUTINE nl_set_auxinput18_end_s ( id_id , auxinput18_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_end_s
  INTEGER id_id
  model_config_rec%auxinput18_end_s(id_id) = auxinput18_end_s
  RETURN
END SUBROUTINE nl_set_auxinput18_end_s
SUBROUTINE nl_set_auxinput18_end ( id_id , auxinput18_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_end
  INTEGER id_id
  model_config_rec%auxinput18_end(id_id) = auxinput18_end
  RETURN
END SUBROUTINE nl_set_auxinput18_end
SUBROUTINE nl_set_io_form_auxinput18 ( id_id , io_form_auxinput18 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput18
  INTEGER id_id
  model_config_rec%io_form_auxinput18 = io_form_auxinput18 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput18
SUBROUTINE nl_set_frames_per_auxinput18 ( id_id , frames_per_auxinput18 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput18
  INTEGER id_id
  model_config_rec%frames_per_auxinput18(id_id) = frames_per_auxinput18
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput18
SUBROUTINE nl_set_auxinput19_inname ( id_id , auxinput19_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput19_inname
  INTEGER id_id
  model_config_rec%auxinput19_inname = trim(auxinput19_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput19_inname
SUBROUTINE nl_set_auxinput19_outname ( id_id , auxinput19_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput19_outname
  INTEGER id_id
  model_config_rec%auxinput19_outname = trim(auxinput19_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput19_outname
SUBROUTINE nl_set_auxinput19_interval_y ( id_id , auxinput19_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_interval_y
  INTEGER id_id
  model_config_rec%auxinput19_interval_y(id_id) = auxinput19_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput19_interval_y
SUBROUTINE nl_set_auxinput19_interval_d ( id_id , auxinput19_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_interval_d
  INTEGER id_id
  model_config_rec%auxinput19_interval_d(id_id) = auxinput19_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput19_interval_d
SUBROUTINE nl_set_auxinput19_interval_h ( id_id , auxinput19_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_interval_h
  INTEGER id_id
  model_config_rec%auxinput19_interval_h(id_id) = auxinput19_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput19_interval_h
SUBROUTINE nl_set_auxinput19_interval_m ( id_id , auxinput19_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_interval_m
  INTEGER id_id
  model_config_rec%auxinput19_interval_m(id_id) = auxinput19_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput19_interval_m
SUBROUTINE nl_set_auxinput19_interval_s ( id_id , auxinput19_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_interval_s
  INTEGER id_id
  model_config_rec%auxinput19_interval_s(id_id) = auxinput19_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput19_interval_s
SUBROUTINE nl_set_auxinput19_interval ( id_id , auxinput19_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_interval
  INTEGER id_id
  model_config_rec%auxinput19_interval(id_id) = auxinput19_interval
  RETURN
END SUBROUTINE nl_set_auxinput19_interval
SUBROUTINE nl_set_auxinput19_begin_y ( id_id , auxinput19_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_begin_y
  INTEGER id_id
  model_config_rec%auxinput19_begin_y(id_id) = auxinput19_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput19_begin_y
SUBROUTINE nl_set_auxinput19_begin_d ( id_id , auxinput19_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_begin_d
  INTEGER id_id
  model_config_rec%auxinput19_begin_d(id_id) = auxinput19_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput19_begin_d
SUBROUTINE nl_set_auxinput19_begin_h ( id_id , auxinput19_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_begin_h
  INTEGER id_id
  model_config_rec%auxinput19_begin_h(id_id) = auxinput19_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput19_begin_h
SUBROUTINE nl_set_auxinput19_begin_m ( id_id , auxinput19_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_begin_m
  INTEGER id_id
  model_config_rec%auxinput19_begin_m(id_id) = auxinput19_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput19_begin_m
SUBROUTINE nl_set_auxinput19_begin_s ( id_id , auxinput19_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_begin_s
  INTEGER id_id
  model_config_rec%auxinput19_begin_s(id_id) = auxinput19_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput19_begin_s
SUBROUTINE nl_set_auxinput19_begin ( id_id , auxinput19_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_begin
  INTEGER id_id
  model_config_rec%auxinput19_begin(id_id) = auxinput19_begin
  RETURN
END SUBROUTINE nl_set_auxinput19_begin
SUBROUTINE nl_set_auxinput19_end_y ( id_id , auxinput19_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_end_y
  INTEGER id_id
  model_config_rec%auxinput19_end_y(id_id) = auxinput19_end_y
  RETURN
END SUBROUTINE nl_set_auxinput19_end_y
SUBROUTINE nl_set_auxinput19_end_d ( id_id , auxinput19_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_end_d
  INTEGER id_id
  model_config_rec%auxinput19_end_d(id_id) = auxinput19_end_d
  RETURN
END SUBROUTINE nl_set_auxinput19_end_d
SUBROUTINE nl_set_auxinput19_end_h ( id_id , auxinput19_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_end_h
  INTEGER id_id
  model_config_rec%auxinput19_end_h(id_id) = auxinput19_end_h
  RETURN
END SUBROUTINE nl_set_auxinput19_end_h
SUBROUTINE nl_set_auxinput19_end_m ( id_id , auxinput19_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_end_m
  INTEGER id_id
  model_config_rec%auxinput19_end_m(id_id) = auxinput19_end_m
  RETURN
END SUBROUTINE nl_set_auxinput19_end_m
SUBROUTINE nl_set_auxinput19_end_s ( id_id , auxinput19_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_end_s
  INTEGER id_id
  model_config_rec%auxinput19_end_s(id_id) = auxinput19_end_s
  RETURN
END SUBROUTINE nl_set_auxinput19_end_s
SUBROUTINE nl_set_auxinput19_end ( id_id , auxinput19_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_end
  INTEGER id_id
  model_config_rec%auxinput19_end(id_id) = auxinput19_end
  RETURN
END SUBROUTINE nl_set_auxinput19_end
SUBROUTINE nl_set_io_form_auxinput19 ( id_id , io_form_auxinput19 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput19
  INTEGER id_id
  model_config_rec%io_form_auxinput19 = io_form_auxinput19 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput19
SUBROUTINE nl_set_frames_per_auxinput19 ( id_id , frames_per_auxinput19 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput19
  INTEGER id_id
  model_config_rec%frames_per_auxinput19(id_id) = frames_per_auxinput19
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput19
SUBROUTINE nl_set_auxinput20_inname ( id_id , auxinput20_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput20_inname
  INTEGER id_id
  model_config_rec%auxinput20_inname = trim(auxinput20_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput20_inname
SUBROUTINE nl_set_auxinput20_outname ( id_id , auxinput20_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput20_outname
  INTEGER id_id
  model_config_rec%auxinput20_outname = trim(auxinput20_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput20_outname
SUBROUTINE nl_set_auxinput20_interval_y ( id_id , auxinput20_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_interval_y
  INTEGER id_id
  model_config_rec%auxinput20_interval_y(id_id) = auxinput20_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput20_interval_y
SUBROUTINE nl_set_auxinput20_interval_d ( id_id , auxinput20_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_interval_d
  INTEGER id_id
  model_config_rec%auxinput20_interval_d(id_id) = auxinput20_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput20_interval_d
SUBROUTINE nl_set_auxinput20_interval_h ( id_id , auxinput20_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_interval_h
  INTEGER id_id
  model_config_rec%auxinput20_interval_h(id_id) = auxinput20_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput20_interval_h
SUBROUTINE nl_set_auxinput20_interval_m ( id_id , auxinput20_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_interval_m
  INTEGER id_id
  model_config_rec%auxinput20_interval_m(id_id) = auxinput20_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput20_interval_m
SUBROUTINE nl_set_auxinput20_interval_s ( id_id , auxinput20_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_interval_s
  INTEGER id_id
  model_config_rec%auxinput20_interval_s(id_id) = auxinput20_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput20_interval_s
SUBROUTINE nl_set_auxinput20_interval ( id_id , auxinput20_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_interval
  INTEGER id_id
  model_config_rec%auxinput20_interval(id_id) = auxinput20_interval
  RETURN
END SUBROUTINE nl_set_auxinput20_interval
SUBROUTINE nl_set_auxinput20_begin_y ( id_id , auxinput20_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_begin_y
  INTEGER id_id
  model_config_rec%auxinput20_begin_y(id_id) = auxinput20_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput20_begin_y
SUBROUTINE nl_set_auxinput20_begin_d ( id_id , auxinput20_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_begin_d
  INTEGER id_id
  model_config_rec%auxinput20_begin_d(id_id) = auxinput20_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput20_begin_d
SUBROUTINE nl_set_auxinput20_begin_h ( id_id , auxinput20_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_begin_h
  INTEGER id_id
  model_config_rec%auxinput20_begin_h(id_id) = auxinput20_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput20_begin_h
SUBROUTINE nl_set_auxinput20_begin_m ( id_id , auxinput20_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_begin_m
  INTEGER id_id
  model_config_rec%auxinput20_begin_m(id_id) = auxinput20_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput20_begin_m
SUBROUTINE nl_set_auxinput20_begin_s ( id_id , auxinput20_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_begin_s
  INTEGER id_id
  model_config_rec%auxinput20_begin_s(id_id) = auxinput20_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput20_begin_s
SUBROUTINE nl_set_auxinput20_begin ( id_id , auxinput20_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_begin
  INTEGER id_id
  model_config_rec%auxinput20_begin(id_id) = auxinput20_begin
  RETURN
END SUBROUTINE nl_set_auxinput20_begin
SUBROUTINE nl_set_auxinput20_end_y ( id_id , auxinput20_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_end_y
  INTEGER id_id
  model_config_rec%auxinput20_end_y(id_id) = auxinput20_end_y
  RETURN
END SUBROUTINE nl_set_auxinput20_end_y
SUBROUTINE nl_set_auxinput20_end_d ( id_id , auxinput20_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_end_d
  INTEGER id_id
  model_config_rec%auxinput20_end_d(id_id) = auxinput20_end_d
  RETURN
END SUBROUTINE nl_set_auxinput20_end_d
SUBROUTINE nl_set_auxinput20_end_h ( id_id , auxinput20_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_end_h
  INTEGER id_id
  model_config_rec%auxinput20_end_h(id_id) = auxinput20_end_h
  RETURN
END SUBROUTINE nl_set_auxinput20_end_h
SUBROUTINE nl_set_auxinput20_end_m ( id_id , auxinput20_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_end_m
  INTEGER id_id
  model_config_rec%auxinput20_end_m(id_id) = auxinput20_end_m
  RETURN
END SUBROUTINE nl_set_auxinput20_end_m
SUBROUTINE nl_set_auxinput20_end_s ( id_id , auxinput20_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_end_s
  INTEGER id_id
  model_config_rec%auxinput20_end_s(id_id) = auxinput20_end_s
  RETURN
END SUBROUTINE nl_set_auxinput20_end_s
SUBROUTINE nl_set_auxinput20_end ( id_id , auxinput20_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_end
  INTEGER id_id
  model_config_rec%auxinput20_end(id_id) = auxinput20_end
  RETURN
END SUBROUTINE nl_set_auxinput20_end
SUBROUTINE nl_set_io_form_auxinput20 ( id_id , io_form_auxinput20 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput20
  INTEGER id_id
  model_config_rec%io_form_auxinput20 = io_form_auxinput20 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput20
SUBROUTINE nl_set_frames_per_auxinput20 ( id_id , frames_per_auxinput20 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput20
  INTEGER id_id
  model_config_rec%frames_per_auxinput20(id_id) = frames_per_auxinput20
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput20
SUBROUTINE nl_set_auxinput21_inname ( id_id , auxinput21_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput21_inname
  INTEGER id_id
  model_config_rec%auxinput21_inname = trim(auxinput21_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput21_inname
SUBROUTINE nl_set_auxinput21_outname ( id_id , auxinput21_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput21_outname
  INTEGER id_id
  model_config_rec%auxinput21_outname = trim(auxinput21_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput21_outname
SUBROUTINE nl_set_auxinput21_interval_y ( id_id , auxinput21_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_interval_y
  INTEGER id_id
  model_config_rec%auxinput21_interval_y(id_id) = auxinput21_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput21_interval_y
SUBROUTINE nl_set_auxinput21_interval_d ( id_id , auxinput21_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_interval_d
  INTEGER id_id
  model_config_rec%auxinput21_interval_d(id_id) = auxinput21_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput21_interval_d
SUBROUTINE nl_set_auxinput21_interval_h ( id_id , auxinput21_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_interval_h
  INTEGER id_id
  model_config_rec%auxinput21_interval_h(id_id) = auxinput21_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput21_interval_h
SUBROUTINE nl_set_auxinput21_interval_m ( id_id , auxinput21_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_interval_m
  INTEGER id_id
  model_config_rec%auxinput21_interval_m(id_id) = auxinput21_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput21_interval_m
SUBROUTINE nl_set_auxinput21_interval_s ( id_id , auxinput21_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_interval_s
  INTEGER id_id
  model_config_rec%auxinput21_interval_s(id_id) = auxinput21_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput21_interval_s
SUBROUTINE nl_set_auxinput21_interval ( id_id , auxinput21_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_interval
  INTEGER id_id
  model_config_rec%auxinput21_interval(id_id) = auxinput21_interval
  RETURN
END SUBROUTINE nl_set_auxinput21_interval
SUBROUTINE nl_set_auxinput21_begin_y ( id_id , auxinput21_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_begin_y
  INTEGER id_id
  model_config_rec%auxinput21_begin_y(id_id) = auxinput21_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput21_begin_y
SUBROUTINE nl_set_auxinput21_begin_d ( id_id , auxinput21_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_begin_d
  INTEGER id_id
  model_config_rec%auxinput21_begin_d(id_id) = auxinput21_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput21_begin_d
SUBROUTINE nl_set_auxinput21_begin_h ( id_id , auxinput21_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_begin_h
  INTEGER id_id
  model_config_rec%auxinput21_begin_h(id_id) = auxinput21_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput21_begin_h
SUBROUTINE nl_set_auxinput21_begin_m ( id_id , auxinput21_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_begin_m
  INTEGER id_id
  model_config_rec%auxinput21_begin_m(id_id) = auxinput21_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput21_begin_m
SUBROUTINE nl_set_auxinput21_begin_s ( id_id , auxinput21_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_begin_s
  INTEGER id_id
  model_config_rec%auxinput21_begin_s(id_id) = auxinput21_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput21_begin_s
SUBROUTINE nl_set_auxinput21_begin ( id_id , auxinput21_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_begin
  INTEGER id_id
  model_config_rec%auxinput21_begin(id_id) = auxinput21_begin
  RETURN
END SUBROUTINE nl_set_auxinput21_begin
SUBROUTINE nl_set_auxinput21_end_y ( id_id , auxinput21_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_end_y
  INTEGER id_id
  model_config_rec%auxinput21_end_y(id_id) = auxinput21_end_y
  RETURN
END SUBROUTINE nl_set_auxinput21_end_y
SUBROUTINE nl_set_auxinput21_end_d ( id_id , auxinput21_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_end_d
  INTEGER id_id
  model_config_rec%auxinput21_end_d(id_id) = auxinput21_end_d
  RETURN
END SUBROUTINE nl_set_auxinput21_end_d
SUBROUTINE nl_set_auxinput21_end_h ( id_id , auxinput21_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_end_h
  INTEGER id_id
  model_config_rec%auxinput21_end_h(id_id) = auxinput21_end_h
  RETURN
END SUBROUTINE nl_set_auxinput21_end_h
SUBROUTINE nl_set_auxinput21_end_m ( id_id , auxinput21_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_end_m
  INTEGER id_id
  model_config_rec%auxinput21_end_m(id_id) = auxinput21_end_m
  RETURN
END SUBROUTINE nl_set_auxinput21_end_m
SUBROUTINE nl_set_auxinput21_end_s ( id_id , auxinput21_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_end_s
  INTEGER id_id
  model_config_rec%auxinput21_end_s(id_id) = auxinput21_end_s
  RETURN
END SUBROUTINE nl_set_auxinput21_end_s
SUBROUTINE nl_set_auxinput21_end ( id_id , auxinput21_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_end
  INTEGER id_id
  model_config_rec%auxinput21_end(id_id) = auxinput21_end
  RETURN
END SUBROUTINE nl_set_auxinput21_end
SUBROUTINE nl_set_io_form_auxinput21 ( id_id , io_form_auxinput21 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput21
  INTEGER id_id
  model_config_rec%io_form_auxinput21 = io_form_auxinput21 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput21
SUBROUTINE nl_set_frames_per_auxinput21 ( id_id , frames_per_auxinput21 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput21
  INTEGER id_id
  model_config_rec%frames_per_auxinput21(id_id) = frames_per_auxinput21
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput21
SUBROUTINE nl_set_auxinput22_inname ( id_id , auxinput22_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput22_inname
  INTEGER id_id
  model_config_rec%auxinput22_inname = trim(auxinput22_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput22_inname
SUBROUTINE nl_set_auxinput22_outname ( id_id , auxinput22_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput22_outname
  INTEGER id_id
  model_config_rec%auxinput22_outname = trim(auxinput22_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput22_outname
SUBROUTINE nl_set_auxinput22_interval_y ( id_id , auxinput22_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_interval_y
  INTEGER id_id
  model_config_rec%auxinput22_interval_y(id_id) = auxinput22_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput22_interval_y
SUBROUTINE nl_set_auxinput22_interval_d ( id_id , auxinput22_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_interval_d
  INTEGER id_id
  model_config_rec%auxinput22_interval_d(id_id) = auxinput22_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput22_interval_d
SUBROUTINE nl_set_auxinput22_interval_h ( id_id , auxinput22_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_interval_h
  INTEGER id_id
  model_config_rec%auxinput22_interval_h(id_id) = auxinput22_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput22_interval_h
SUBROUTINE nl_set_auxinput22_interval_m ( id_id , auxinput22_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_interval_m
  INTEGER id_id
  model_config_rec%auxinput22_interval_m(id_id) = auxinput22_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput22_interval_m
SUBROUTINE nl_set_auxinput22_interval_s ( id_id , auxinput22_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_interval_s
  INTEGER id_id
  model_config_rec%auxinput22_interval_s(id_id) = auxinput22_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput22_interval_s
SUBROUTINE nl_set_auxinput22_interval ( id_id , auxinput22_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_interval
  INTEGER id_id
  model_config_rec%auxinput22_interval(id_id) = auxinput22_interval
  RETURN
END SUBROUTINE nl_set_auxinput22_interval
SUBROUTINE nl_set_auxinput22_begin_y ( id_id , auxinput22_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_begin_y
  INTEGER id_id
  model_config_rec%auxinput22_begin_y(id_id) = auxinput22_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput22_begin_y
SUBROUTINE nl_set_auxinput22_begin_d ( id_id , auxinput22_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_begin_d
  INTEGER id_id
  model_config_rec%auxinput22_begin_d(id_id) = auxinput22_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput22_begin_d
SUBROUTINE nl_set_auxinput22_begin_h ( id_id , auxinput22_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_begin_h
  INTEGER id_id
  model_config_rec%auxinput22_begin_h(id_id) = auxinput22_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput22_begin_h
SUBROUTINE nl_set_auxinput22_begin_m ( id_id , auxinput22_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_begin_m
  INTEGER id_id
  model_config_rec%auxinput22_begin_m(id_id) = auxinput22_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput22_begin_m
SUBROUTINE nl_set_auxinput22_begin_s ( id_id , auxinput22_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_begin_s
  INTEGER id_id
  model_config_rec%auxinput22_begin_s(id_id) = auxinput22_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput22_begin_s
SUBROUTINE nl_set_auxinput22_begin ( id_id , auxinput22_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_begin
  INTEGER id_id
  model_config_rec%auxinput22_begin(id_id) = auxinput22_begin
  RETURN
END SUBROUTINE nl_set_auxinput22_begin
SUBROUTINE nl_set_auxinput22_end_y ( id_id , auxinput22_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_end_y
  INTEGER id_id
  model_config_rec%auxinput22_end_y(id_id) = auxinput22_end_y
  RETURN
END SUBROUTINE nl_set_auxinput22_end_y
SUBROUTINE nl_set_auxinput22_end_d ( id_id , auxinput22_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_end_d
  INTEGER id_id
  model_config_rec%auxinput22_end_d(id_id) = auxinput22_end_d
  RETURN
END SUBROUTINE nl_set_auxinput22_end_d
SUBROUTINE nl_set_auxinput22_end_h ( id_id , auxinput22_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_end_h
  INTEGER id_id
  model_config_rec%auxinput22_end_h(id_id) = auxinput22_end_h
  RETURN
END SUBROUTINE nl_set_auxinput22_end_h
SUBROUTINE nl_set_auxinput22_end_m ( id_id , auxinput22_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_end_m
  INTEGER id_id
  model_config_rec%auxinput22_end_m(id_id) = auxinput22_end_m
  RETURN
END SUBROUTINE nl_set_auxinput22_end_m
SUBROUTINE nl_set_auxinput22_end_s ( id_id , auxinput22_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_end_s
  INTEGER id_id
  model_config_rec%auxinput22_end_s(id_id) = auxinput22_end_s
  RETURN
END SUBROUTINE nl_set_auxinput22_end_s
SUBROUTINE nl_set_auxinput22_end ( id_id , auxinput22_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_end
  INTEGER id_id
  model_config_rec%auxinput22_end(id_id) = auxinput22_end
  RETURN
END SUBROUTINE nl_set_auxinput22_end
SUBROUTINE nl_set_io_form_auxinput22 ( id_id , io_form_auxinput22 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput22
  INTEGER id_id
  model_config_rec%io_form_auxinput22 = io_form_auxinput22 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput22
SUBROUTINE nl_set_frames_per_auxinput22 ( id_id , frames_per_auxinput22 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput22
  INTEGER id_id
  model_config_rec%frames_per_auxinput22(id_id) = frames_per_auxinput22
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput22
SUBROUTINE nl_set_auxinput23_inname ( id_id , auxinput23_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput23_inname
  INTEGER id_id
  model_config_rec%auxinput23_inname = trim(auxinput23_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput23_inname
SUBROUTINE nl_set_auxinput23_outname ( id_id , auxinput23_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput23_outname
  INTEGER id_id
  model_config_rec%auxinput23_outname = trim(auxinput23_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput23_outname
SUBROUTINE nl_set_auxinput23_interval_y ( id_id , auxinput23_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_interval_y
  INTEGER id_id
  model_config_rec%auxinput23_interval_y(id_id) = auxinput23_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput23_interval_y
SUBROUTINE nl_set_auxinput23_interval_d ( id_id , auxinput23_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_interval_d
  INTEGER id_id
  model_config_rec%auxinput23_interval_d(id_id) = auxinput23_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput23_interval_d
SUBROUTINE nl_set_auxinput23_interval_h ( id_id , auxinput23_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_interval_h
  INTEGER id_id
  model_config_rec%auxinput23_interval_h(id_id) = auxinput23_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput23_interval_h
SUBROUTINE nl_set_auxinput23_interval_m ( id_id , auxinput23_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_interval_m
  INTEGER id_id
  model_config_rec%auxinput23_interval_m(id_id) = auxinput23_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput23_interval_m
SUBROUTINE nl_set_auxinput23_interval_s ( id_id , auxinput23_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_interval_s
  INTEGER id_id
  model_config_rec%auxinput23_interval_s(id_id) = auxinput23_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput23_interval_s
SUBROUTINE nl_set_auxinput23_interval ( id_id , auxinput23_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_interval
  INTEGER id_id
  model_config_rec%auxinput23_interval(id_id) = auxinput23_interval
  RETURN
END SUBROUTINE nl_set_auxinput23_interval
SUBROUTINE nl_set_auxinput23_begin_y ( id_id , auxinput23_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_begin_y
  INTEGER id_id
  model_config_rec%auxinput23_begin_y(id_id) = auxinput23_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput23_begin_y
SUBROUTINE nl_set_auxinput23_begin_d ( id_id , auxinput23_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_begin_d
  INTEGER id_id
  model_config_rec%auxinput23_begin_d(id_id) = auxinput23_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput23_begin_d
SUBROUTINE nl_set_auxinput23_begin_h ( id_id , auxinput23_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_begin_h
  INTEGER id_id
  model_config_rec%auxinput23_begin_h(id_id) = auxinput23_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput23_begin_h
SUBROUTINE nl_set_auxinput23_begin_m ( id_id , auxinput23_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_begin_m
  INTEGER id_id
  model_config_rec%auxinput23_begin_m(id_id) = auxinput23_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput23_begin_m
SUBROUTINE nl_set_auxinput23_begin_s ( id_id , auxinput23_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_begin_s
  INTEGER id_id
  model_config_rec%auxinput23_begin_s(id_id) = auxinput23_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput23_begin_s
SUBROUTINE nl_set_auxinput23_begin ( id_id , auxinput23_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_begin
  INTEGER id_id
  model_config_rec%auxinput23_begin(id_id) = auxinput23_begin
  RETURN
END SUBROUTINE nl_set_auxinput23_begin
SUBROUTINE nl_set_auxinput23_end_y ( id_id , auxinput23_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_end_y
  INTEGER id_id
  model_config_rec%auxinput23_end_y(id_id) = auxinput23_end_y
  RETURN
END SUBROUTINE nl_set_auxinput23_end_y
SUBROUTINE nl_set_auxinput23_end_d ( id_id , auxinput23_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_end_d
  INTEGER id_id
  model_config_rec%auxinput23_end_d(id_id) = auxinput23_end_d
  RETURN
END SUBROUTINE nl_set_auxinput23_end_d
SUBROUTINE nl_set_auxinput23_end_h ( id_id , auxinput23_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_end_h
  INTEGER id_id
  model_config_rec%auxinput23_end_h(id_id) = auxinput23_end_h
  RETURN
END SUBROUTINE nl_set_auxinput23_end_h
SUBROUTINE nl_set_auxinput23_end_m ( id_id , auxinput23_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_end_m
  INTEGER id_id
  model_config_rec%auxinput23_end_m(id_id) = auxinput23_end_m
  RETURN
END SUBROUTINE nl_set_auxinput23_end_m
SUBROUTINE nl_set_auxinput23_end_s ( id_id , auxinput23_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_end_s
  INTEGER id_id
  model_config_rec%auxinput23_end_s(id_id) = auxinput23_end_s
  RETURN
END SUBROUTINE nl_set_auxinput23_end_s
SUBROUTINE nl_set_auxinput23_end ( id_id , auxinput23_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_end
  INTEGER id_id
  model_config_rec%auxinput23_end(id_id) = auxinput23_end
  RETURN
END SUBROUTINE nl_set_auxinput23_end
SUBROUTINE nl_set_io_form_auxinput23 ( id_id , io_form_auxinput23 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput23
  INTEGER id_id
  model_config_rec%io_form_auxinput23 = io_form_auxinput23 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput23
SUBROUTINE nl_set_frames_per_auxinput23 ( id_id , frames_per_auxinput23 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput23
  INTEGER id_id
  model_config_rec%frames_per_auxinput23(id_id) = frames_per_auxinput23
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput23
SUBROUTINE nl_set_auxinput24_inname ( id_id , auxinput24_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput24_inname
  INTEGER id_id
  model_config_rec%auxinput24_inname = trim(auxinput24_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput24_inname
SUBROUTINE nl_set_auxinput24_outname ( id_id , auxinput24_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput24_outname
  INTEGER id_id
  model_config_rec%auxinput24_outname = trim(auxinput24_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput24_outname
SUBROUTINE nl_set_auxinput24_interval_y ( id_id , auxinput24_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_interval_y
  INTEGER id_id
  model_config_rec%auxinput24_interval_y(id_id) = auxinput24_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput24_interval_y
SUBROUTINE nl_set_auxinput24_interval_d ( id_id , auxinput24_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_interval_d
  INTEGER id_id
  model_config_rec%auxinput24_interval_d(id_id) = auxinput24_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput24_interval_d
SUBROUTINE nl_set_auxinput24_interval_h ( id_id , auxinput24_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_interval_h
  INTEGER id_id
  model_config_rec%auxinput24_interval_h(id_id) = auxinput24_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput24_interval_h
SUBROUTINE nl_set_auxinput24_interval_m ( id_id , auxinput24_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_interval_m
  INTEGER id_id
  model_config_rec%auxinput24_interval_m(id_id) = auxinput24_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput24_interval_m
SUBROUTINE nl_set_auxinput24_interval_s ( id_id , auxinput24_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_interval_s
  INTEGER id_id
  model_config_rec%auxinput24_interval_s(id_id) = auxinput24_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput24_interval_s
SUBROUTINE nl_set_auxinput24_interval ( id_id , auxinput24_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_interval
  INTEGER id_id
  model_config_rec%auxinput24_interval(id_id) = auxinput24_interval
  RETURN
END SUBROUTINE nl_set_auxinput24_interval
SUBROUTINE nl_set_auxinput24_begin_y ( id_id , auxinput24_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_begin_y
  INTEGER id_id
  model_config_rec%auxinput24_begin_y(id_id) = auxinput24_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput24_begin_y
SUBROUTINE nl_set_auxinput24_begin_d ( id_id , auxinput24_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_begin_d
  INTEGER id_id
  model_config_rec%auxinput24_begin_d(id_id) = auxinput24_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput24_begin_d
SUBROUTINE nl_set_auxinput24_begin_h ( id_id , auxinput24_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_begin_h
  INTEGER id_id
  model_config_rec%auxinput24_begin_h(id_id) = auxinput24_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput24_begin_h
SUBROUTINE nl_set_auxinput24_begin_m ( id_id , auxinput24_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_begin_m
  INTEGER id_id
  model_config_rec%auxinput24_begin_m(id_id) = auxinput24_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput24_begin_m
SUBROUTINE nl_set_auxinput24_begin_s ( id_id , auxinput24_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_begin_s
  INTEGER id_id
  model_config_rec%auxinput24_begin_s(id_id) = auxinput24_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput24_begin_s
SUBROUTINE nl_set_auxinput24_begin ( id_id , auxinput24_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_begin
  INTEGER id_id
  model_config_rec%auxinput24_begin(id_id) = auxinput24_begin
  RETURN
END SUBROUTINE nl_set_auxinput24_begin
SUBROUTINE nl_set_auxinput24_end_y ( id_id , auxinput24_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_end_y
  INTEGER id_id
  model_config_rec%auxinput24_end_y(id_id) = auxinput24_end_y
  RETURN
END SUBROUTINE nl_set_auxinput24_end_y
SUBROUTINE nl_set_auxinput24_end_d ( id_id , auxinput24_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_end_d
  INTEGER id_id
  model_config_rec%auxinput24_end_d(id_id) = auxinput24_end_d
  RETURN
END SUBROUTINE nl_set_auxinput24_end_d
SUBROUTINE nl_set_auxinput24_end_h ( id_id , auxinput24_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_end_h
  INTEGER id_id
  model_config_rec%auxinput24_end_h(id_id) = auxinput24_end_h
  RETURN
END SUBROUTINE nl_set_auxinput24_end_h
SUBROUTINE nl_set_auxinput24_end_m ( id_id , auxinput24_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_end_m
  INTEGER id_id
  model_config_rec%auxinput24_end_m(id_id) = auxinput24_end_m
  RETURN
END SUBROUTINE nl_set_auxinput24_end_m
SUBROUTINE nl_set_auxinput24_end_s ( id_id , auxinput24_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_end_s
  INTEGER id_id
  model_config_rec%auxinput24_end_s(id_id) = auxinput24_end_s
  RETURN
END SUBROUTINE nl_set_auxinput24_end_s
SUBROUTINE nl_set_auxinput24_end ( id_id , auxinput24_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_end
  INTEGER id_id
  model_config_rec%auxinput24_end(id_id) = auxinput24_end
  RETURN
END SUBROUTINE nl_set_auxinput24_end
SUBROUTINE nl_set_io_form_auxinput24 ( id_id , io_form_auxinput24 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput24
  INTEGER id_id
  model_config_rec%io_form_auxinput24 = io_form_auxinput24 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput24
SUBROUTINE nl_set_frames_per_auxinput24 ( id_id , frames_per_auxinput24 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput24
  INTEGER id_id
  model_config_rec%frames_per_auxinput24(id_id) = frames_per_auxinput24
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput24
SUBROUTINE nl_set_history_interval ( id_id , history_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval
  INTEGER id_id
  model_config_rec%history_interval(id_id) = history_interval
  RETURN
END SUBROUTINE nl_set_history_interval
SUBROUTINE nl_set_frames_per_outfile ( id_id , frames_per_outfile )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_outfile
  INTEGER id_id
  model_config_rec%frames_per_outfile(id_id) = frames_per_outfile
  RETURN
END SUBROUTINE nl_set_frames_per_outfile
SUBROUTINE nl_set_restart ( id_id , restart )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: restart
  INTEGER id_id
  model_config_rec%restart = restart 
  RETURN
END SUBROUTINE nl_set_restart
SUBROUTINE nl_set_restart_interval ( id_id , restart_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval
  INTEGER id_id
  model_config_rec%restart_interval = restart_interval 
  RETURN
END SUBROUTINE nl_set_restart_interval
SUBROUTINE nl_set_io_form_input ( id_id , io_form_input )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_input
  INTEGER id_id
  model_config_rec%io_form_input = io_form_input 
  RETURN
END SUBROUTINE nl_set_io_form_input
SUBROUTINE nl_set_io_form_history ( id_id , io_form_history )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_history
  INTEGER id_id
  model_config_rec%io_form_history = io_form_history 
  RETURN
END SUBROUTINE nl_set_io_form_history
SUBROUTINE nl_set_io_form_restart ( id_id , io_form_restart )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_restart
  INTEGER id_id
  model_config_rec%io_form_restart = io_form_restart 
  RETURN
END SUBROUTINE nl_set_io_form_restart
SUBROUTINE nl_set_io_form_boundary ( id_id , io_form_boundary )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_boundary
  INTEGER id_id
  model_config_rec%io_form_boundary = io_form_boundary 
  RETURN
END SUBROUTINE nl_set_io_form_boundary
SUBROUTINE nl_set_debug_level ( id_id , debug_level )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: debug_level
  INTEGER id_id
  model_config_rec%debug_level = debug_level 
  RETURN
END SUBROUTINE nl_set_debug_level
SUBROUTINE nl_set_self_test_domain ( id_id , self_test_domain )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: self_test_domain
  INTEGER id_id
  model_config_rec%self_test_domain = self_test_domain 
  RETURN
END SUBROUTINE nl_set_self_test_domain
SUBROUTINE nl_set_history_outname ( id_id , history_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: history_outname
  INTEGER id_id
  model_config_rec%history_outname = trim(history_outname) 
  RETURN
END SUBROUTINE nl_set_history_outname
SUBROUTINE nl_set_history_inname ( id_id , history_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: history_inname
  INTEGER id_id
  model_config_rec%history_inname = trim(history_inname) 
  RETURN
END SUBROUTINE nl_set_history_inname
SUBROUTINE nl_set_use_netcdf_classic ( id_id , use_netcdf_classic )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: use_netcdf_classic
  INTEGER id_id
  model_config_rec%use_netcdf_classic = use_netcdf_classic 
  RETURN
END SUBROUTINE nl_set_use_netcdf_classic
SUBROUTINE nl_set_history_interval_d ( id_id , history_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval_d
  INTEGER id_id
  model_config_rec%history_interval_d(id_id) = history_interval_d
  RETURN
END SUBROUTINE nl_set_history_interval_d
SUBROUTINE nl_set_history_interval_h ( id_id , history_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval_h
  INTEGER id_id
  model_config_rec%history_interval_h(id_id) = history_interval_h
  RETURN
END SUBROUTINE nl_set_history_interval_h
SUBROUTINE nl_set_history_interval_m ( id_id , history_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval_m
  INTEGER id_id
  model_config_rec%history_interval_m(id_id) = history_interval_m
  RETURN
END SUBROUTINE nl_set_history_interval_m
SUBROUTINE nl_set_history_interval_s ( id_id , history_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval_s
  INTEGER id_id
  model_config_rec%history_interval_s(id_id) = history_interval_s
  RETURN
END SUBROUTINE nl_set_history_interval_s
SUBROUTINE nl_set_inputout_interval_d ( id_id , inputout_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval_d
  INTEGER id_id
  model_config_rec%inputout_interval_d(id_id) = inputout_interval_d
  RETURN
END SUBROUTINE nl_set_inputout_interval_d
SUBROUTINE nl_set_inputout_interval_h ( id_id , inputout_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval_h
  INTEGER id_id
  model_config_rec%inputout_interval_h(id_id) = inputout_interval_h
  RETURN
END SUBROUTINE nl_set_inputout_interval_h
SUBROUTINE nl_set_inputout_interval_m ( id_id , inputout_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval_m
  INTEGER id_id
  model_config_rec%inputout_interval_m(id_id) = inputout_interval_m
  RETURN
END SUBROUTINE nl_set_inputout_interval_m
SUBROUTINE nl_set_inputout_interval_s ( id_id , inputout_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval_s
  INTEGER id_id
  model_config_rec%inputout_interval_s(id_id) = inputout_interval_s
  RETURN
END SUBROUTINE nl_set_inputout_interval_s
SUBROUTINE nl_set_inputout_interval ( id_id , inputout_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval
  INTEGER id_id
  model_config_rec%inputout_interval(id_id) = inputout_interval
  RETURN
END SUBROUTINE nl_set_inputout_interval
SUBROUTINE nl_set_restart_interval_d ( id_id , restart_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval_d
  INTEGER id_id
  model_config_rec%restart_interval_d = restart_interval_d 
  RETURN
END SUBROUTINE nl_set_restart_interval_d
SUBROUTINE nl_set_restart_interval_h ( id_id , restart_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval_h
  INTEGER id_id
  model_config_rec%restart_interval_h = restart_interval_h 
  RETURN
END SUBROUTINE nl_set_restart_interval_h
SUBROUTINE nl_set_restart_interval_m ( id_id , restart_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval_m
  INTEGER id_id
  model_config_rec%restart_interval_m = restart_interval_m 
  RETURN
END SUBROUTINE nl_set_restart_interval_m
SUBROUTINE nl_set_restart_interval_s ( id_id , restart_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval_s
  INTEGER id_id
  model_config_rec%restart_interval_s = restart_interval_s 
  RETURN
END SUBROUTINE nl_set_restart_interval_s
SUBROUTINE nl_set_history_begin_y ( id_id , history_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_y
  INTEGER id_id
  model_config_rec%history_begin_y(id_id) = history_begin_y
  RETURN
END SUBROUTINE nl_set_history_begin_y
SUBROUTINE nl_set_history_begin_d ( id_id , history_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_d
  INTEGER id_id
  model_config_rec%history_begin_d(id_id) = history_begin_d
  RETURN
END SUBROUTINE nl_set_history_begin_d
SUBROUTINE nl_set_history_begin_h ( id_id , history_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_h
  INTEGER id_id
  model_config_rec%history_begin_h(id_id) = history_begin_h
  RETURN
END SUBROUTINE nl_set_history_begin_h
SUBROUTINE nl_set_history_begin_m ( id_id , history_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_m
  INTEGER id_id
  model_config_rec%history_begin_m(id_id) = history_begin_m
  RETURN
END SUBROUTINE nl_set_history_begin_m
SUBROUTINE nl_set_history_begin_s ( id_id , history_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_s
  INTEGER id_id
  model_config_rec%history_begin_s(id_id) = history_begin_s
  RETURN
END SUBROUTINE nl_set_history_begin_s
SUBROUTINE nl_set_history_begin ( id_id , history_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin
  INTEGER id_id
  model_config_rec%history_begin(id_id) = history_begin
  RETURN
END SUBROUTINE nl_set_history_begin
SUBROUTINE nl_set_inputout_begin_y ( id_id , inputout_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_y
  INTEGER id_id
  model_config_rec%inputout_begin_y(id_id) = inputout_begin_y
  RETURN
END SUBROUTINE nl_set_inputout_begin_y
SUBROUTINE nl_set_inputout_begin_d ( id_id , inputout_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_d
  INTEGER id_id
  model_config_rec%inputout_begin_d(id_id) = inputout_begin_d
  RETURN
END SUBROUTINE nl_set_inputout_begin_d
SUBROUTINE nl_set_inputout_begin_h ( id_id , inputout_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_h
  INTEGER id_id
  model_config_rec%inputout_begin_h(id_id) = inputout_begin_h
  RETURN
END SUBROUTINE nl_set_inputout_begin_h
SUBROUTINE nl_set_inputout_begin_m ( id_id , inputout_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_m
  INTEGER id_id
  model_config_rec%inputout_begin_m(id_id) = inputout_begin_m
  RETURN
END SUBROUTINE nl_set_inputout_begin_m
SUBROUTINE nl_set_inputout_begin_s ( id_id , inputout_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_s
  INTEGER id_id
  model_config_rec%inputout_begin_s(id_id) = inputout_begin_s
  RETURN
END SUBROUTINE nl_set_inputout_begin_s
SUBROUTINE nl_set_restart_begin_y ( id_id , restart_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_y
  INTEGER id_id
  model_config_rec%restart_begin_y = restart_begin_y 
  RETURN
END SUBROUTINE nl_set_restart_begin_y
SUBROUTINE nl_set_restart_begin_d ( id_id , restart_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_d
  INTEGER id_id
  model_config_rec%restart_begin_d = restart_begin_d 
  RETURN
END SUBROUTINE nl_set_restart_begin_d
SUBROUTINE nl_set_restart_begin_h ( id_id , restart_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_h
  INTEGER id_id
  model_config_rec%restart_begin_h = restart_begin_h 
  RETURN
END SUBROUTINE nl_set_restart_begin_h
SUBROUTINE nl_set_restart_begin_m ( id_id , restart_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_m
  INTEGER id_id
  model_config_rec%restart_begin_m = restart_begin_m 
  RETURN
END SUBROUTINE nl_set_restart_begin_m
SUBROUTINE nl_set_restart_begin_s ( id_id , restart_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_s
  INTEGER id_id
  model_config_rec%restart_begin_s = restart_begin_s 
  RETURN
END SUBROUTINE nl_set_restart_begin_s
SUBROUTINE nl_set_restart_begin ( id_id , restart_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin
  INTEGER id_id
  model_config_rec%restart_begin = restart_begin 
  RETURN
END SUBROUTINE nl_set_restart_begin
SUBROUTINE nl_set_history_end_y ( id_id , history_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_y
  INTEGER id_id
  model_config_rec%history_end_y(id_id) = history_end_y
  RETURN
END SUBROUTINE nl_set_history_end_y
SUBROUTINE nl_set_history_end_d ( id_id , history_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_d
  INTEGER id_id
  model_config_rec%history_end_d(id_id) = history_end_d
  RETURN
END SUBROUTINE nl_set_history_end_d
SUBROUTINE nl_set_history_end_h ( id_id , history_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_h
  INTEGER id_id
  model_config_rec%history_end_h(id_id) = history_end_h
  RETURN
END SUBROUTINE nl_set_history_end_h
SUBROUTINE nl_set_history_end_m ( id_id , history_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_m
  INTEGER id_id
  model_config_rec%history_end_m(id_id) = history_end_m
  RETURN
END SUBROUTINE nl_set_history_end_m
SUBROUTINE nl_set_history_end_s ( id_id , history_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_s
  INTEGER id_id
  model_config_rec%history_end_s(id_id) = history_end_s
  RETURN
END SUBROUTINE nl_set_history_end_s
SUBROUTINE nl_set_history_end ( id_id , history_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end
  INTEGER id_id
  model_config_rec%history_end(id_id) = history_end
  RETURN
END SUBROUTINE nl_set_history_end
SUBROUTINE nl_set_inputout_end_y ( id_id , inputout_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_y
  INTEGER id_id
  model_config_rec%inputout_end_y(id_id) = inputout_end_y
  RETURN
END SUBROUTINE nl_set_inputout_end_y
SUBROUTINE nl_set_inputout_end_d ( id_id , inputout_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_d
  INTEGER id_id
  model_config_rec%inputout_end_d(id_id) = inputout_end_d
  RETURN
END SUBROUTINE nl_set_inputout_end_d
SUBROUTINE nl_set_inputout_end_h ( id_id , inputout_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_h
  INTEGER id_id
  model_config_rec%inputout_end_h(id_id) = inputout_end_h
  RETURN
END SUBROUTINE nl_set_inputout_end_h
SUBROUTINE nl_set_inputout_end_m ( id_id , inputout_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_m
  INTEGER id_id
  model_config_rec%inputout_end_m(id_id) = inputout_end_m
  RETURN
END SUBROUTINE nl_set_inputout_end_m
SUBROUTINE nl_set_inputout_end_s ( id_id , inputout_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_s
  INTEGER id_id
  model_config_rec%inputout_end_s(id_id) = inputout_end_s
  RETURN
END SUBROUTINE nl_set_inputout_end_s
SUBROUTINE nl_set_simulation_start_year ( id_id , simulation_start_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_year
  INTEGER id_id
  model_config_rec%simulation_start_year = simulation_start_year 
  RETURN
END SUBROUTINE nl_set_simulation_start_year
SUBROUTINE nl_set_simulation_start_month ( id_id , simulation_start_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_month
  INTEGER id_id
  model_config_rec%simulation_start_month = simulation_start_month 
  RETURN
END SUBROUTINE nl_set_simulation_start_month
SUBROUTINE nl_set_simulation_start_day ( id_id , simulation_start_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_day
  INTEGER id_id
  model_config_rec%simulation_start_day = simulation_start_day 
  RETURN
END SUBROUTINE nl_set_simulation_start_day
SUBROUTINE nl_set_simulation_start_hour ( id_id , simulation_start_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_hour
  INTEGER id_id
  model_config_rec%simulation_start_hour = simulation_start_hour 
  RETURN
END SUBROUTINE nl_set_simulation_start_hour
SUBROUTINE nl_set_simulation_start_minute ( id_id , simulation_start_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_minute
  INTEGER id_id
  model_config_rec%simulation_start_minute = simulation_start_minute 
  RETURN
END SUBROUTINE nl_set_simulation_start_minute
SUBROUTINE nl_set_simulation_start_second ( id_id , simulation_start_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_second
  INTEGER id_id
  model_config_rec%simulation_start_second = simulation_start_second 
  RETURN
END SUBROUTINE nl_set_simulation_start_second
SUBROUTINE nl_set_reset_simulation_start ( id_id , reset_simulation_start )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: reset_simulation_start
  INTEGER id_id
  model_config_rec%reset_simulation_start = reset_simulation_start 
  RETURN
END SUBROUTINE nl_set_reset_simulation_start
SUBROUTINE nl_set_sr_x ( id_id , sr_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sr_x
  INTEGER id_id
  model_config_rec%sr_x(id_id) = sr_x
  RETURN
END SUBROUTINE nl_set_sr_x
SUBROUTINE nl_set_sr_y ( id_id , sr_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sr_y
  INTEGER id_id
  model_config_rec%sr_y(id_id) = sr_y
  RETURN
END SUBROUTINE nl_set_sr_y
SUBROUTINE nl_set_sgfdda_inname ( id_id , sgfdda_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: sgfdda_inname
  INTEGER id_id
  model_config_rec%sgfdda_inname = trim(sgfdda_inname) 
  RETURN
END SUBROUTINE nl_set_sgfdda_inname
SUBROUTINE nl_set_gfdda_inname ( id_id , gfdda_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: gfdda_inname
  INTEGER id_id
  model_config_rec%gfdda_inname = trim(gfdda_inname) 
  RETURN
END SUBROUTINE nl_set_gfdda_inname
SUBROUTINE nl_set_sgfdda_interval_d ( id_id , sgfdda_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_interval_d
  INTEGER id_id
  model_config_rec%sgfdda_interval_d(id_id) = sgfdda_interval_d
  RETURN
END SUBROUTINE nl_set_sgfdda_interval_d
SUBROUTINE nl_set_sgfdda_interval_h ( id_id , sgfdda_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_interval_h
  INTEGER id_id
  model_config_rec%sgfdda_interval_h(id_id) = sgfdda_interval_h
  RETURN
END SUBROUTINE nl_set_sgfdda_interval_h
SUBROUTINE nl_set_sgfdda_interval_m ( id_id , sgfdda_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_interval_m
  INTEGER id_id
  model_config_rec%sgfdda_interval_m(id_id) = sgfdda_interval_m
  RETURN
END SUBROUTINE nl_set_sgfdda_interval_m
SUBROUTINE nl_set_sgfdda_interval_s ( id_id , sgfdda_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_interval_s
  INTEGER id_id
  model_config_rec%sgfdda_interval_s(id_id) = sgfdda_interval_s
  RETURN
END SUBROUTINE nl_set_sgfdda_interval_s
SUBROUTINE nl_set_sgfdda_interval_y ( id_id , sgfdda_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_interval_y
  INTEGER id_id
  model_config_rec%sgfdda_interval_y(id_id) = sgfdda_interval_y
  RETURN
END SUBROUTINE nl_set_sgfdda_interval_y
SUBROUTINE nl_set_sgfdda_interval ( id_id , sgfdda_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_interval
  INTEGER id_id
  model_config_rec%sgfdda_interval(id_id) = sgfdda_interval
  RETURN
END SUBROUTINE nl_set_sgfdda_interval
SUBROUTINE nl_set_gfdda_interval_d ( id_id , gfdda_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_interval_d
  INTEGER id_id
  model_config_rec%gfdda_interval_d(id_id) = gfdda_interval_d
  RETURN
END SUBROUTINE nl_set_gfdda_interval_d
SUBROUTINE nl_set_gfdda_interval_h ( id_id , gfdda_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_interval_h
  INTEGER id_id
  model_config_rec%gfdda_interval_h(id_id) = gfdda_interval_h
  RETURN
END SUBROUTINE nl_set_gfdda_interval_h
SUBROUTINE nl_set_gfdda_interval_m ( id_id , gfdda_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_interval_m
  INTEGER id_id
  model_config_rec%gfdda_interval_m(id_id) = gfdda_interval_m
  RETURN
END SUBROUTINE nl_set_gfdda_interval_m
SUBROUTINE nl_set_gfdda_interval_s ( id_id , gfdda_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_interval_s
  INTEGER id_id
  model_config_rec%gfdda_interval_s(id_id) = gfdda_interval_s
  RETURN
END SUBROUTINE nl_set_gfdda_interval_s
SUBROUTINE nl_set_gfdda_interval_y ( id_id , gfdda_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_interval_y
  INTEGER id_id
  model_config_rec%gfdda_interval_y(id_id) = gfdda_interval_y
  RETURN
END SUBROUTINE nl_set_gfdda_interval_y
SUBROUTINE nl_set_gfdda_interval ( id_id , gfdda_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_interval
  INTEGER id_id
  model_config_rec%gfdda_interval(id_id) = gfdda_interval
  RETURN
END SUBROUTINE nl_set_gfdda_interval
SUBROUTINE nl_set_sgfdda_begin_y ( id_id , sgfdda_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_begin_y
  INTEGER id_id
  model_config_rec%sgfdda_begin_y(id_id) = sgfdda_begin_y
  RETURN
END SUBROUTINE nl_set_sgfdda_begin_y
SUBROUTINE nl_set_sgfdda_begin_d ( id_id , sgfdda_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_begin_d
  INTEGER id_id
  model_config_rec%sgfdda_begin_d(id_id) = sgfdda_begin_d
  RETURN
END SUBROUTINE nl_set_sgfdda_begin_d
SUBROUTINE nl_set_sgfdda_begin_h ( id_id , sgfdda_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_begin_h
  INTEGER id_id
  model_config_rec%sgfdda_begin_h(id_id) = sgfdda_begin_h
  RETURN
END SUBROUTINE nl_set_sgfdda_begin_h
SUBROUTINE nl_set_sgfdda_begin_m ( id_id , sgfdda_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_begin_m
  INTEGER id_id
  model_config_rec%sgfdda_begin_m(id_id) = sgfdda_begin_m
  RETURN
END SUBROUTINE nl_set_sgfdda_begin_m
SUBROUTINE nl_set_sgfdda_begin_s ( id_id , sgfdda_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_begin_s
  INTEGER id_id
  model_config_rec%sgfdda_begin_s(id_id) = sgfdda_begin_s
  RETURN
END SUBROUTINE nl_set_sgfdda_begin_s
SUBROUTINE nl_set_gfdda_begin_y ( id_id , gfdda_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_begin_y
  INTEGER id_id
  model_config_rec%gfdda_begin_y(id_id) = gfdda_begin_y
  RETURN
END SUBROUTINE nl_set_gfdda_begin_y
SUBROUTINE nl_set_gfdda_begin_d ( id_id , gfdda_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_begin_d
  INTEGER id_id
  model_config_rec%gfdda_begin_d(id_id) = gfdda_begin_d
  RETURN
END SUBROUTINE nl_set_gfdda_begin_d
SUBROUTINE nl_set_gfdda_begin_h ( id_id , gfdda_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_begin_h
  INTEGER id_id
  model_config_rec%gfdda_begin_h(id_id) = gfdda_begin_h
  RETURN
END SUBROUTINE nl_set_gfdda_begin_h
SUBROUTINE nl_set_gfdda_begin_m ( id_id , gfdda_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_begin_m
  INTEGER id_id
  model_config_rec%gfdda_begin_m(id_id) = gfdda_begin_m
  RETURN
END SUBROUTINE nl_set_gfdda_begin_m
SUBROUTINE nl_set_gfdda_begin_s ( id_id , gfdda_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_begin_s
  INTEGER id_id
  model_config_rec%gfdda_begin_s(id_id) = gfdda_begin_s
  RETURN
END SUBROUTINE nl_set_gfdda_begin_s
SUBROUTINE nl_set_sgfdda_end_y ( id_id , sgfdda_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_end_y
  INTEGER id_id
  model_config_rec%sgfdda_end_y(id_id) = sgfdda_end_y
  RETURN
END SUBROUTINE nl_set_sgfdda_end_y
SUBROUTINE nl_set_sgfdda_end_d ( id_id , sgfdda_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_end_d
  INTEGER id_id
  model_config_rec%sgfdda_end_d(id_id) = sgfdda_end_d
  RETURN
END SUBROUTINE nl_set_sgfdda_end_d
SUBROUTINE nl_set_sgfdda_end_h ( id_id , sgfdda_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_end_h
  INTEGER id_id
  model_config_rec%sgfdda_end_h(id_id) = sgfdda_end_h
  RETURN
END SUBROUTINE nl_set_sgfdda_end_h
SUBROUTINE nl_set_sgfdda_end_m ( id_id , sgfdda_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_end_m
  INTEGER id_id
  model_config_rec%sgfdda_end_m(id_id) = sgfdda_end_m
  RETURN
END SUBROUTINE nl_set_sgfdda_end_m
SUBROUTINE nl_set_sgfdda_end_s ( id_id , sgfdda_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_end_s
  INTEGER id_id
  model_config_rec%sgfdda_end_s(id_id) = sgfdda_end_s
  RETURN
END SUBROUTINE nl_set_sgfdda_end_s
SUBROUTINE nl_set_gfdda_end_y ( id_id , gfdda_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_end_y
  INTEGER id_id
  model_config_rec%gfdda_end_y(id_id) = gfdda_end_y
  RETURN
END SUBROUTINE nl_set_gfdda_end_y
SUBROUTINE nl_set_gfdda_end_d ( id_id , gfdda_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_end_d
  INTEGER id_id
  model_config_rec%gfdda_end_d(id_id) = gfdda_end_d
  RETURN
END SUBROUTINE nl_set_gfdda_end_d
SUBROUTINE nl_set_gfdda_end_h ( id_id , gfdda_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_end_h
  INTEGER id_id
  model_config_rec%gfdda_end_h(id_id) = gfdda_end_h
  RETURN
END SUBROUTINE nl_set_gfdda_end_h
SUBROUTINE nl_set_gfdda_end_m ( id_id , gfdda_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_end_m
  INTEGER id_id
  model_config_rec%gfdda_end_m(id_id) = gfdda_end_m
  RETURN
END SUBROUTINE nl_set_gfdda_end_m
SUBROUTINE nl_set_gfdda_end_s ( id_id , gfdda_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_end_s
  INTEGER id_id
  model_config_rec%gfdda_end_s(id_id) = gfdda_end_s
  RETURN
END SUBROUTINE nl_set_gfdda_end_s
SUBROUTINE nl_set_io_form_sgfdda ( id_id , io_form_sgfdda )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_sgfdda
  INTEGER id_id
  model_config_rec%io_form_sgfdda = io_form_sgfdda 
  RETURN
END SUBROUTINE nl_set_io_form_sgfdda
SUBROUTINE nl_set_io_form_gfdda ( id_id , io_form_gfdda )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_gfdda
  INTEGER id_id
  model_config_rec%io_form_gfdda = io_form_gfdda 
  RETURN
END SUBROUTINE nl_set_io_form_gfdda
SUBROUTINE nl_set_iofields_filename ( id_id , iofields_filename )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: iofields_filename
  INTEGER id_id
  model_config_rec%iofields_filename(id_id) = iofields_filename
  RETURN
END SUBROUTINE nl_set_iofields_filename
SUBROUTINE nl_set_ignore_iofields_warning ( id_id , ignore_iofields_warning )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: ignore_iofields_warning
  INTEGER id_id
  model_config_rec%ignore_iofields_warning = ignore_iofields_warning 
  RETURN
END SUBROUTINE nl_set_ignore_iofields_warning
SUBROUTINE nl_set_ncd_nofill ( id_id , ncd_nofill )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: ncd_nofill
  INTEGER id_id
  model_config_rec%ncd_nofill = ncd_nofill 
  RETURN
END SUBROUTINE nl_set_ncd_nofill
SUBROUTINE nl_set_nfmc ( id_id , nfmc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nfmc
  INTEGER id_id
  model_config_rec%nfmc = nfmc 
  RETURN
END SUBROUTINE nl_set_nfmc
SUBROUTINE nl_set_fmoist_run ( id_id , fmoist_run )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: fmoist_run
  INTEGER id_id
  model_config_rec%fmoist_run(id_id) = fmoist_run
  RETURN
END SUBROUTINE nl_set_fmoist_run
SUBROUTINE nl_set_fmoist_interp ( id_id , fmoist_interp )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: fmoist_interp
  INTEGER id_id
  model_config_rec%fmoist_interp(id_id) = fmoist_interp
  RETURN
END SUBROUTINE nl_set_fmoist_interp
SUBROUTINE nl_set_fmoisti_run ( id_id , fmoisti_run )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: fmoisti_run
  INTEGER id_id
  model_config_rec%fmoisti_run(id_id) = fmoisti_run
  RETURN
END SUBROUTINE nl_set_fmoisti_run
SUBROUTINE nl_set_fmoisti_interp ( id_id , fmoisti_interp )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: fmoisti_interp
  INTEGER id_id
  model_config_rec%fmoisti_interp(id_id) = fmoisti_interp
  RETURN
END SUBROUTINE nl_set_fmoisti_interp
SUBROUTINE nl_set_fmoist_only ( id_id , fmoist_only )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: fmoist_only
  INTEGER id_id
  model_config_rec%fmoist_only(id_id) = fmoist_only
  RETURN
END SUBROUTINE nl_set_fmoist_only
SUBROUTINE nl_set_fmoist_freq ( id_id , fmoist_freq )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: fmoist_freq
  INTEGER id_id
  model_config_rec%fmoist_freq(id_id) = fmoist_freq
  RETURN
END SUBROUTINE nl_set_fmoist_freq
SUBROUTINE nl_set_fmoist_dt ( id_id , fmoist_dt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: fmoist_dt
  INTEGER id_id
  model_config_rec%fmoist_dt(id_id) = fmoist_dt
  RETURN
END SUBROUTINE nl_set_fmoist_dt
SUBROUTINE nl_set_fmep_decay_tlag ( id_id , fmep_decay_tlag )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: fmep_decay_tlag
  INTEGER id_id
  model_config_rec%fmep_decay_tlag = fmep_decay_tlag 
  RETURN
END SUBROUTINE nl_set_fmep_decay_tlag
SUBROUTINE nl_set_ifire ( id_id , ifire )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ifire
  INTEGER id_id
  model_config_rec%ifire(id_id) = ifire
  RETURN
END SUBROUTINE nl_set_ifire
!ENDOFREGISTRYGENERATEDINCLUDE
