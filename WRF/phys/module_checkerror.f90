

























 module module_checkerror
 implicit none















































 private             
 public  checkerror  





  interface checkerror
    module procedure checkerror_single
    module procedure checkerror_double

  end interface

 contains




 subroutine checkerror_single(subroutine_name, param_id,i,k,j,input_real)
 use, intrinsic :: ieee_arithmetic
 implicit none
 character*(*),intent(in) :: subroutine_name
 character*(*),intent(in) :: param_id
 integer,intent(in) :: i,k,j  
 real(4),intent(in) :: input_real

 character(len=132) :: string

 select case(trim(param_id))
 case('temperature_K')

  if(input_real < 0. .or. input_real > 1000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",103,&
'Terminate run.')
  endif

 case('temperature_degC')

  if(input_real < -274. .or. input_real > 1000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",116,&
'Terminate run.')
  endif

 case('pressure_Pa')

  if(input_real < 0. .or. input_real > 200000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",129,&
'Terminate run.')
  endif

 case('radiationflux_W/m2')

  if(input_real < -10000. .or. input_real > 10000. ) then  



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",142,&
'Terminate run.')
  endif

 case('condensate_g/m3')

  if(input_real < 0. .or. input_real > 10000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",155,&
'Terminate run.')
  endif

 case('condensate_kg/kg')

  if(input_real < 0. .or. input_real > 10000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",168,&
'Terminate run.') 
  endif

 case('aerosol_g/m3')

  if(input_real < 0. .or. input_real > 1000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",181,&
'Terminate run.')
  endif

 case('aerosol_ug/kg')

  if(input_real < 0. .or. input_real > 1000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",194,&
'Terminate run.')
  endif

 case('albedo')

  if(input_real < 0. .or. input_real > 1. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",207,&
'Terminate run.')
  endif

 case('emissivity')

  if(input_real < 0. .or. input_real > 1. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",220,&
'Terminate run.')
  endif


 case default


      write(string,*) 'MSG checkerror_float: There is no such param_id',trim(param_id)
      call wrf_message(string)
      call wrf_error_fatal3("<stdin>",230,&
'Terminate run.')
 end select


 if (abs(input_real) >= huge(input_real)) then
    write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' Infinity at grid(i,k,j) =',i,k,j
    call wrf_message(string)
    call wrf_error_fatal3("<stdin>",239,&
'Terminate run.')
 end if

 if (ieee_is_nan(input_real)) then
    write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' NaN at grid(i,k,j) =',i,k,j
    call wrf_message(string)
    call wrf_error_fatal3("<stdin>",247,&
'Terminate run.')
 end if

 end subroutine checkerror_single




 subroutine checkerror_double(subroutine_name, param_id,i,k,j,input_real)
 use, intrinsic :: ieee_arithmetic
 implicit none
 character*(*),intent(in) :: subroutine_name
 character*(*),intent(in) :: param_id
 integer,intent(in) :: i,k,j  
 real(8),intent(in) :: input_real

 character(len=132) :: string

 select case(trim(param_id))
 case('temperature_K')

  if(input_real < 0. .or. input_real > 1000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",276,&
'Terminate run.')
  endif

 case('temperature_degC')

  if(input_real < -274. .or. input_real > 1000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",289,&
'Terminate run.')
  endif

 case('pressure_Pa')

  if(input_real < 0. .or. input_real > 200000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",302,&
'Terminate run.')
  endif

 case('radiationflux_W/m2')

  if(input_real < -10000. .or. input_real > 10000. ) then  



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",315,&
'Terminate run.')

  endif

 case('condensate_g/m3')

  if(input_real < 0. .or. input_real > 10000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",329,&
'Terminate run.')
  endif

 case('condensate_kg/kg')

  if(input_real < 0. .or. input_real > 10000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",342,&
'Terminate run.')
  endif

 case('aerosol_g/m3')

  if(input_real < 0. .or. input_real > 1000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",355,&
'Terminate run.')
  endif

 case('aerosol_ug/kg')

  if(input_real < 0. .or. input_real > 1000. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",368,&
'Terminate run.')
  endif

 case('albedo')

  if(input_real < 0. .or. input_real > 1. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",381,&
'Terminate run.')
  endif

 case('emissivity')

  if(input_real < 0. .or. input_real > 1. ) then



       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal3("<stdin>",394,&
'Terminate run.')
  endif

 case default


      write(string,*) 'MSG checkerror_double: There is no such param_id',trim(param_id)
      call wrf_message(string)
       call wrf_error_fatal3("<stdin>",403,&
'Terminate run.')
 end select


 if (abs(input_real) >= huge(input_real)) then
    write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' Infinity at grid(i,k,j) =',i,k,j
    call wrf_message(string)
    call wrf_error_fatal3("<stdin>",412,&
'Terminate run.')
 end if

 if (ieee_is_nan(input_real)) then
    write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' NaN at grid(i,k,j) =',i,k,j
    call wrf_message(string)
    call wrf_error_fatal3("<stdin>",420,&
'Terminate run.')
 end if

 end subroutine checkerror_double




 end module module_checkerror



