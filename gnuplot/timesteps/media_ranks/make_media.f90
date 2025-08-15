program media

   implicit none
   
   integer,    parameter                     :: unid_ATM=2000, n_ATM_cores=128
   integer,    parameter                     :: unid_OCN=3000, n_OCN_cores=128
   integer                                   :: rank, cpl_cycle, t
   
   real                                      :: stime 
   real,       dimension(:),     allocatable :: mtime, omtime, amaxtime, amintime, omaxtime, omintime
   real,       dimension(:,:),   allocatable :: time, otime
   
   character(len=4)                          :: cunid
   character(len=100)                        :: fname, fname1, fname2
   
   logical                                   :: existe
   
   
   allocate(time(0:n_ATM_cores-1, 0:49))
   time=0.0
   
   allocate(mtime(0:49))
   mtime=0.0
   
   allocate(otime(0:n_OCN_cores-1, 0:49))
   otime=0.0
   
   allocate(omtime(0:49))
   omtime=0.0
   
   allocate(amaxtime(0:49))
   amaxtime=0.0
   
   allocate(amintime(0:49))
   amintime=0.0
   
   allocate(omaxtime(0:49))
   omaxtime=0.0
   
   allocate(omintime(0:49))
   omintime=0.0
   
   
   ! ATM:
   do rank=0, n_ATM_cores-1
      !do rank=0, 0
      write(cunid, '(i4)') unid_ATM+rank
      
      
      inquire(file='com_IO/fort.'//cunid, exist=existe)
      
      if (existe) then
         !write(*, "('Arq. fort.',i4,' aberto. Rank ATM ', i4)") unid_ATM+rank, rank
         open(unid_ATM+rank, file='com_IO/fort.'//cunid, status='old', action='read')
      else
         print*, 'Arquivo ', unid_ATM+rank, ' ATM_com_IO nao pode ser aberto.'
         exit
      endif
      
      do 
         read(unid_ATM+rank, *, end=10) fname, fname1, fname2, stime, cpl_cycle
         time(rank,cpl_cycle)=stime
         !write(*, *)  stime, rank, cpl_cycle, time(rank,cpl_cycle)
      enddo
10    continue         
      
      close(unid_ATM+rank)
   enddo
   
   
   
   !OCN:
   do rank=0, n_OCN_cores-1
   !do rank=0, 1
      write(cunid, '(i4)') unid_OCN+rank
      inquire(file='com_IO/fort.'//cunid, exist=existe)
      if (existe) then
         open(unid_OCN+rank, file='com_IO/fort.'//cunid, status='old', action='read')
      else
         print*, 'Arquivo ', unid_OCN+rank, ' OCN_com_IO nao pode ser aberto.'
         exit
      endif
   
      do 
         read(unid_OCN+rank, *, end=20) fname, fname1, fname2, stime, cpl_cycle
         otime(rank,cpl_cycle)=stime
         !write(*, *)  stime, rank, cpl_cycle, otime(rank,cpl_cycle)
      enddo
20    continue         
   
   
   enddo
   
   
   
   

      
   ! Media entre os cores para cada ciclo de acolamento:
   do t=0, cpl_cycle
      mtime(t)=(sum(time(:,t))/size(time(:,t)))
      omtime(t)=(sum(otime(:,t))/size(otime(:,t)))
      amaxtime(t)=maxval(time(:,t))
      amintime(t)=minval(time(:,t))
      omaxtime(t)=maxval(otime(:,t))
      omintime(t)=minval(otime(:,t))
      !print*, t, mtime(t), omtime(t), amaxtime(t), amintime(t)
      !print*, t,time(0,t), otime(0,t)
   enddo
   

   
   mtime(0)=0.0
   omtime(0)=0.0
   amaxtime(0)=0.0
   amintime(0)=0.0
   omaxtime(0)=0.0
   omintime(0)=0.0
   
   mtime(48)=0.0
   omtime(48)=0.0
   amaxtime(48)=0.0
   amintime(48)=0.0
   omaxtime(48)=0.0
   omintime(48)=0.0
   
   omtime(1)=omtime(3)
   omaxtime(1)=omaxtime(3)
   omintime(1)=omintime(3)


   open(10, file='med.min.max.graftimestep.OCN+ATM.input_EGEON_24h_comIO', status='unknown', action='write')
   write(*, *) " c  tmed_ATM_comIO   tmax_ATM_comIO tmin_ATM_comIO tmed_OCN_comIO  tmax_OCN_comIO   tmin_OCN_comIO"
   write(10, *) " c  tmed_ATM_comIO   tmax_ATM_comIO tmin_ATM_comIO tmed_OCN_comIO  tmax_OCN_comIO   tmin_OCN_comIO"
   
   do t=0, cpl_cycle
      write(*, "(i3, 3x, 6(f10.5, 5x))") t, mtime(t), amaxtime(t), amintime(t), omtime(t),omaxtime(t),omintime(t) 
      write(10, "(i3, 3x, 6(f10.5, 5x))") t, mtime(t), amaxtime(t), amintime(t), omtime(t),omaxtime(t),omintime(t) 
   enddo
   
   
   
   
   
   
   

end program
