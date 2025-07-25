!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! PROGRAM ARWpost
!
! Initial version: Cindy Bruyere - November 2006 
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM ARWpost

   USE gridinfo_module
   USE module_debug
   USE process_domain_module

   CALL mprintf(.true.,STDOUT,'  ')
   CALL mprintf(.true.,STDOUT,'!!!!!!!!!!!!!!!!')
   CALL mprintf(.true.,STDOUT,'  ARWpost v3.1  ')    !!! March 2011   
   CALL mprintf(.true.,STDOUT,'!!!!!!!!!!!!!!!!')

   ! Get info about how many nests there are to process, etc.
   CALL get_namelist_params()

   ! Now begin the processing work
   CALL mprintf(.true.,STDOUT,'START PROCESSING DATA')
   CALL process_domain()

   ! If we are back here - all went well
   CALL mprintf(.true.,STDOUT,'  ')
   CALL mprintf(.true.,STDOUT,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
   CALL mprintf(.true.,STDOUT,'!  Successful completion of ARWpost  !')
   CALL mprintf(.true.,STDOUT,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
   CALL mprintf(.true.,STDOUT,'  ')

END PROGRAM ARWpost
