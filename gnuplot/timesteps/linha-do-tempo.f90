program linhadtempo
   implicit none
   
   integer,          parameter            :: dp=kind(1.0d0), r=0, nciclos=6
   integer                                :: exp=0, unit, ios, i=0, j, ia, id
   real(dp)                               :: tempo_ini=0.0, tempo_fim=0.0, tempo
   real(dp),         dimension(100)       :: t_antes_cpl_atm=0.0, t_depois_cpl_atm=0.0
   real(dp),         dimension(100)       :: t_antes_ws_atm=0.0, t_depois_ws_atm=0.0
   real(dp),         dimension(100)       :: t_antes_wr_atm=0.0, t_depois_wr_atm=0.0
   real(dp),         dimension(100)       :: t_antes_cpl_ocn=0.0, t_depois_cpl_ocn=0.0
   real(dp),         dimension(100)       :: t_antes_wr_ocn=0.0, t_depois_wr_ocn=0.0
   real(dp),         dimension(100)       :: t_antes_ws_ocn=0.0, t_depois_ws_ocn=0.0
   character(len=1)                       :: letra
   character(len=200)                     :: sufixo, dirname, linha , filename, tag
   
   
   sufixo=""
   letra ="l"
   
   ! ----------------------------------------------------------------------------------------
   ! Lendo fort.10000 (tempo total)
   print*,'fort.10000 tempo total'
   write(dirname,"('/mnt/beegfs/carlos.souza/Doutorado/COAWST.V3.7/Work/ATLSW12/wr_17022023-',A1,I1,A,'/')") letra,exp,trim(sufixo)
   
   open(newunit=unit,file=trim(dirname)//'fort.10000',status='old',action='read',iostat=ios)
   if (ios /= 0) then
      print *,"Erro ao abrir ",trim(dirname)//"fort.10000"
      stop
   end if
   
   read(unit,'(A)',iostat=ios) linha
   read(linha(index(linha,'=')+1:),*,iostat=ios) tempo_ini
   print *,"      Tempo inicial=",tempo_ini
   read(unit,'(A)',iostat=ios) linha
   read(linha(index(linha,'=')+1:),*,iostat=ios) tempo_fim
   print *,"      Tempo final=",tempo_fim
   
   
   ! ----------------------------------------------------------------------------------------
   ! Lendo fort.20000 ... fort.(20000+ATM_ncores-1)
   print*,'fort.20000 tempos ciclos entre acpl ATM'
   write(filename,'("fort.",I0)') 20000 + r
   open(newunit=unit,file=trim(dirname)//trim(filename),status='old',action='read',iostat=ios)
   if (ios /= 0) then
      print *,"Erro ao abrir ",trim(dirname)//trim(filename)
      stop
   end if
   ia=0
   id=0
   do
      read(unit, * , end=10) tag, tempo
      if (trim(tag) == 'antes_cpl_atm') then
         ia=ia+1
         t_antes_cpl_atm(ia)=tempo-tempo_ini
      else
         id=id+1
         t_depois_cpl_atm(id)=tempo-tempo_ini
      endif
   enddo
10 close(unit)  

   ! imprimindo arquivo para gnuplot:
   open(newunit=unit,file='linha-do-tempo_gnuplot_input_acpl_atm.txt',status='unknown',action='write',iostat=ios)
   write(unit, "('antes_cpl_atm   depois_cpl_atm')")
   do i=1, nciclos
      write(unit, "(f10.2, 2x, f10.2)") t_antes_cpl_atm(i), t_depois_cpl_atm(i)
   enddo
   close(unit)  
   ! ----------------------------------------------------------------------------------------



   ! ----------------------------------------------------------------------------------------
   ! Lendo fort.30000 ... fort.(30000+OCN_ncores-1)
   print*,'fort.30000 tempos ciclos entre acpl OCN'
   write(filename,'("fort.",I0)') 30000 + r
   open(newunit=unit,file=trim(dirname)//trim(filename),status='old',action='read',iostat=ios)
   if (ios /= 0) then
      print *,"Erro ao abrir ",trim(dirname)//trim(filename)
      stop
   end if
   ia=0
   id=0
   do
      read(unit, * , end=20) tag, tempo
      if (trim(tag) == 'antes_cpl_ocn') then
         ia=ia+1
         t_antes_cpl_ocn(ia)=tempo-tempo_ini
      else
         id=id+1
         t_depois_cpl_ocn(id)=tempo-tempo_ini
      endif
   enddo
20 close(unit)    

   ! imprimindo arquivo para gnuplot:
   open(newunit=unit,file='linha-do-tempo_gnuplot_input_acpl_ocn.txt',status='unknown',action='write',iostat=ios)
   write(unit, "('antes_cpl_ocn   depois_cpl_ocn')")
   do i=1, nciclos
      write(unit, "(f10.2, 2x, f10.2)") t_antes_cpl_ocn(i), t_depois_cpl_ocn(i)
   enddo
   close(unit)  
   ! ----------------------------------------------------------------------------------------



   ! ----------------------------------------------------------------------------------------
   ! Lendo fort.40000 ... fort.(40000+ATM_ncores-1)
   print*,'fort.40000 tempos waitS acpl ATM'
   write(filename,'("fort.",I0)') 40000 + r
   open(newunit=unit,file=trim(dirname)//trim(filename),status='old',action='read',iostat=ios)
   if (ios /= 0) then
      print *,"Erro ao abrir ",trim(dirname)//trim(filename)
      stop
   end if
   ia=0
   id=0
   do
      read(unit, * , end=30) tag, tempo
      if (trim(tag) == 'antes_atm_waits') then
         ia=ia+1
         t_antes_ws_atm(ia)=tempo-tempo_ini
      else
         id=id+1
         t_depois_ws_atm(id)=tempo-tempo_ini
      endif
   enddo
30 close(unit)    

   ! imprimindo arquivo para gnuplot:
   open(newunit=unit,file='linha-do-tempo_gnuplot_input_ws_atm.txt',status='unknown',action='write',iostat=ios)
   write(unit, "('antes_ws_stm   depois_ws_atmn')")
   do i=1, nciclos
      write(unit, "(f10.2, 2x, f10.2)") t_antes_ws_atm(i), t_depois_ws_atm(i)
   enddo
   close(unit)  
      
   ! ----------------------------------------------------------------------------------------



   ! ----------------------------------------------------------------------------------------
   ! Lendo fort.50000 ... fort.(50000+ATM_ncores-1)
   print*,'fort.50000 tempos waitR acpl ATM'
   write(filename,'("fort.",I0)') 50000 + r
   open(newunit=unit,file=trim(dirname)//trim(filename),status='old',action='read',iostat=ios)
   if (ios /= 0) then
      print *,"Erro ao abrir ",trim(dirname)//trim(filename)
      stop
   end if
   ia=0
   id=0
   do
      read(unit, * , end=40) tag, tempo
      if (trim(tag) == 'antes_atm_waitr') then
         ia=ia+1
         t_antes_wr_atm(ia)=tempo-tempo_ini
      else
         id=id+1
         t_depois_wr_atm(id)=tempo-tempo_ini
      endif
   enddo
40 close(unit)     

   ! imprimindo arquivo para gnuplot:
   open(newunit=unit,file='linha-do-tempo_gnuplot_input_wr_atm.txt',status='unknown',action='write',iostat=ios)
   write(unit, "('antes_wr_stm   depois_wr_atmn')")
   do i=1, nciclos
      write(unit, "(f10.2, 2x, f10.2)") t_antes_wr_atm(i), t_depois_wr_atm(i)
   enddo
   close(unit)     
   ! ----------------------------------------------------------------------------------------



   ! ----------------------------------------------------------------------------------------
   ! Lendo fort.60000 ... fort.(60000+ATM_ncores-1)
   print*,'fort.60000 tempos waitR acpl OCN'
   write(filename,'("fort.",I0)') 60000 + r
   open(newunit=unit,file=trim(dirname)//trim(filename),status='old',action='read',iostat=ios)
   if (ios /= 0) then
      print *,"Erro ao abrir ",trim(dirname)//trim(filename)
      stop
   end if
   ia=0
   id=0
   do
      i=i+1
      read(unit, * , end=50) tag, tempo
      if (trim(tag) == 'antes_waitr_ocn') then
         ia=ia+1
         t_antes_wr_ocn(ia)=tempo-tempo_ini
      else
         id=id+1
         t_depois_wr_ocn(id)=tempo-tempo_ini
      endif
   enddo
50 close(unit)      

   ! imprimindo arquivo para gnuplot:
   open(newunit=unit,file='linha-do-tempo_gnuplot_input_wr_ocn.txt',status='unknown',action='write',iostat=ios)
   write(unit, "('antes_wr_ocn   depois_wr_ocn')")
   do i=1, nciclos
      write(unit, "(f10.2, 2x, f10.2)") t_antes_wr_ocn(i), t_depois_wr_ocn(i)
   enddo
   close(unit)     
   ! ----------------------------------------------------------------------------------------



   ! ----------------------------------------------------------------------------------------
   ! Lendo fort.70000 ... fort.(70000+ATM_ncores-1)
   print*,'fort.70000 tempos waitS acpl OCN'
   write(filename,'("fort.",I0)') 70000 + r
   open(newunit=unit,file=trim(dirname)//trim(filename),status='old',action='read',iostat=ios)
   if (ios /= 0) then
      print *,"Erro ao abrir ",trim(dirname)//trim(filename)
      stop
   end if
   ia=0
   id=0
   do
      i=i+1
      read(unit, * , end=60) tag, tempo
      if (trim(tag) == 'antes_waits_ocn') then
         ia=ia+1
         t_antes_ws_ocn(i)=tempo-tempo_ini
      else
         id=id+1
         t_depois_ws_ocn(i)=tempo-tempo_ini
      endif
   enddo
60 close(unit)      

   ! imprimindo arquivo para gnuplot:
   open(newunit=unit,file='linha-do-tempo_gnuplot_input_ws_ocn.txt',status='unknown',action='write',iostat=ios)
   write(unit, "('antes_ws_ocn   depois_ws_ocn')")
   do i=1, nciclos
      write(unit, "(f10.2, 2x, f10.2)") t_antes_ws_ocn(i), t_depois_ws_ocn(i)
   enddo
   close(unit)     
   ! ----------------------------------------------------------------------------------------






end program linhadtempo
