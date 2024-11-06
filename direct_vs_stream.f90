program ram_vs_ssd

implicit none

integer,parameter :: RSP = selected_real_kind(6,20)  ! Real Single (binary32) Precision 
integer,parameter :: REP = selected_real_kind(14,27) ! Real Extended (binary64) Precision 
integer,parameter :: ISP = selected_int_kind(9)  ! Integer Single (binary32) Precision 
integer,parameter :: IEP = selected_int_kind(12) ! Integer Extendend (binary64) Precision 

integer (kind=IEP) :: n_el,i,j,el_file,jj
integer (kind=IEP), dimension(:), allocatable :: vet_ram
integer(kind=ISP) :: dt_start(8), dt_end(8), dt_start_tot(8), dt_end_tot(8)
integer(kind=ISP) :: fc,bs_rsp,bs_rep,bs_isp,bs_iep
integer(kind=ISP) :: nbrl

integer(kind=ISP),parameter :: nbb_rsp=3
integer(kind=ISP),parameter :: nbb_rep=0
integer(kind=ISP),parameter :: nbb_isp=9
integer(kind=ISP),parameter :: nbb_iep=1

real (kind=RSP) :: start_time, finish_time, elapsed_time, hours, minutes, seconds
real (kind=RSP) :: start_time_tot, finish_time_tot, elapsed_time_tot
real (kind=RSP) :: sph=3600, ms=60

real (kind=RSP) :: cnt_perc, step=10
real (kind=RSP) :: perc


type dr_pt_type
    integer (ISP) :: i                 ! target di vettori di pointer di dr_net
    integer (ISP) :: j                 ! target di vettori di pointer di dr_net
    !real (RSP), pointer :: Z           ! una volta riempito dealloco DTM
    real (RSP) :: Z                    ! una volta riempito dealloco DTM
    integer (ISP) :: id_pnt            ! mat_id => dr_pt()%id_pnt
    integer (ISP) :: fldir    ! flow direction, index of outflow point
    integer (ISP) :: fldir_ss ! flow direction endorheic saddle spill, index of outflow point
    integer (ISP) :: A_in              ! inflow area expressed in cells number;  verificare se serve effettivamente
    real (RSP) :: upl                  ! upslope path lenght;
    real (RSP) :: dpl                  ! downslope path lenght;
    integer (ISP) :: id_ch    ! id of the channel to which it belongs;   ! EX id_net
    integer (ISP) :: id_endo ! id in the endo_pt ;  => endo_pt%id_eo
    integer (ISP) :: ninf=0_ISP        ! number of inflow points;  L'ho spostato da dr_pt_outflow_type perchè mi serve per saddle spill
    integer (IEP) :: pos_cum 
end type

type dr_pt_vl_type
    integer (ISP), dimension(:), allocatable :: id_pnts  ! array of pointer, each points to the point in the dr_pt  
end type


type(dr_pt_type) :: dr_pt, dr_pt_str
type(dr_pt_vl_type) :: dr_pt_vl

! n_el=100000000_IEP
! n_el=100000_IEP
n_el=10_IEP
! n_el=250000000_IEP
!n_el=1000000*100
!n_el=100000_IEP*100000_IEP


WRITE(6,*) 'n_el = ', n_el


bs_rsp=4
bs_rep=8  ! bs_rep=16 for quadruple (binary128) precision 
bs_isp=4
bs_iep=8
! nbrl=nbb_rsp*bs_rsp+nbb_rep*bs_rep+nbb_isp*bs_isp+nbb_iep*bs_iep+4
nbrl=nbb_rsp*bs_rsp+nbb_rep*bs_rep+nbb_isp*bs_isp+nbb_iep*bs_iep

WRITE(6,*) 'nbrl = ',nbrl

!goto 1875

! open(2239,file='ssd.dat',status='unknown')
open(2239,file='ddt_pos',access='direct',status='replace',form='unformatted',recl=nbrl)
open(1875,file='stream',access='stream',status='replace',form='unformatted')
! For temporary files:
open(2240,access='direct',status='scratch',form='unformatted',recl=nbrl)
open(1876,access='stream',status='scratch',form='unformatted')


WRITE(6,*)
WRITE(6,*) '-----------------------------'
WRITE(6,*) 'Scrittura file'

call cpu_time(start_time)
call date_and_time(values = dt_start)
cnt_perc=10

do i=1,n_el
    !write(2239,*) i
    
    dr_pt%i=i
    dr_pt%j=i*10
    dr_pt%Z=i+1475
    dr_pt%id_pnt=i+10
    dr_pt%fldir=i+100
    dr_pt%fldir_ss=i+100
    dr_pt%A_in=i+100
    dr_pt%upl=i+1000
    dr_pt%dpl=i+5
    dr_pt%id_ch=i+80
    dr_pt%id_endo=i+7
    dr_pt%ninf=0_ISP
    ! inquire(1875,pos=dr_pt%pos_cum)
    !WRITE(6,*) 'dr_pt%pos_cum = ',dr_pt%pos_cum
    !allocate(dr_pt_vl%id_pnts(i))
    !do jj=1,i
    !   dr_pt_vl%id_pnts(jj)=jj+10
    !end do

    write(2239,rec=i) dr_pt

    !deallocate(dr_pt_vl%id_pnts)

    perc=real(i)/real(n_el)
    if (floor(perc*100) >= cnt_perc) then   
       write(6,'(a,i6,a)') ''//achar(27)//'[38;2;253;252;187m ',int(cnt_perc),'% completed'//achar(27)//'[0m'
       cnt_perc=cnt_perc+step       
    end if  

end do

call cpu_time(finish_time)
call date_and_time(values = dt_end)

elapsed_time=finish_time-start_time
hours=elapsed_time/sph
minutes=mod(elapsed_time, sph)/ms
seconds=mod(minutes*ms, ms)
write(6,'(a,i4,a1,i3,a1,f6.2,a)') ''//achar(27)//'[38;2;10;156;253m cpu time :', int(hours),'h',int(minutes), & 
    'm',seconds,'s'//achar(27)//'[0m'

start_time=real(dt_start(7))+real(dt_start(8))/1000_RSP+real(dt_start(6))*ms+real(dt_start(5))*sph
finish_time=real(dt_end(7))+real(dt_end(8))/1000_RSP+real(dt_end(6))*ms+real(dt_end(5))*sph
elapsed_time=finish_time-start_time
hours=elapsed_time/sph
minutes=mod(elapsed_time, sph)/ms
seconds=mod(minutes*ms, ms)
write(6,'(a,i4,a1,i3,a1,f6.2,a)') ''//achar(27)//'[38;2;10;156;253m wall-clock time :', int(hours),'h',int(minutes), &
    'm',seconds,'s'//achar(27)//'[0m' 


call cpu_time(start_time)
call date_and_time(values = dt_start)
cnt_perc=10

do i=1,n_el
    !write(2239,*) i
    
    dr_pt_str%i=i
    dr_pt_str%j=i*10
    dr_pt_str%Z=i+1475
    dr_pt_str%id_pnt=i+10
    dr_pt_str%fldir=i+100
    dr_pt_str%fldir_ss=i+100
    dr_pt_str%A_in=i+100
    dr_pt_str%upl=i+1000
    dr_pt_str%dpl=i+5
    dr_pt_str%id_ch=i+80
    dr_pt_str%id_endo=i+7
    dr_pt_str%ninf=0_ISP
    ! inquire(1875,pos=dr_pt_str%pos_cum)
    !WRITE(6,*) 'dr_pt%pos_cum = ',dr_pt%pos_cum
    !allocate(dr_pt_vl%id_pnts(i))
    !do jj=1,i
    !   dr_pt_vl%id_pnts(jj)=jj+10
    !end do

    write(1875) dr_pt_str

    !deallocate(dr_pt_vl%id_pnts)

    perc=real(i)/real(n_el)
    if (floor(perc*100) >= cnt_perc) then   
       write(6,'(a,i6,a)') ''//achar(27)//'[38;2;253;252;187m ',int(cnt_perc),'% completed'//achar(27)//'[0m'
       cnt_perc=cnt_perc+step       
    end if  

end do


call cpu_time(finish_time)
call date_and_time(values = dt_end)

elapsed_time=finish_time-start_time
hours=elapsed_time/sph
minutes=mod(elapsed_time, sph)/ms
seconds=mod(minutes*ms, ms)
write(6,'(a,i4,a1,i3,a1,f6.2,a)') ''//achar(27)//'[38;2;10;156;253m cpu time :', int(hours),'h',int(minutes), & 
    'm',seconds,'s'//achar(27)//'[0m'

start_time=real(dt_start(7))+real(dt_start(8))/1000_RSP+real(dt_start(6))*ms+real(dt_start(5))*sph
finish_time=real(dt_end(7))+real(dt_end(8))/1000_RSP+real(dt_end(6))*ms+real(dt_end(5))*sph
elapsed_time=finish_time-start_time
hours=elapsed_time/sph
minutes=mod(elapsed_time, sph)/ms
seconds=mod(minutes*ms, ms)
write(6,'(a,i4,a1,i3,a1,f6.2,a)') ''//achar(27)//'[38;2;10;156;253m wall-clock time :', int(hours),'h',int(minutes), &
    'm',seconds,'s'//achar(27)//'[0m' 

WRITE(6,*)
WRITE(6,*) '-----------------------------'
WRITE(6,*) 'lettura e scrittura file'

call cpu_time(start_time)
call date_and_time(values = dt_start)
cnt_perc=10


rewind(1875)

do i=1,n_el
! do i=n_el,1,-1
    read(2239,rec=i) dr_pt
    ! allocate(dr_pt_vl%id_pnts(dr_pt%i))
    
    WRITE(6,*) ' '
    WRITE(6,*) '--------------------'
    WRITE(6,*) 'dr_pt = ',dr_pt    

    ! deallocate(dr_pt_vl%id_pnts)

    ! read(2239,*) el_file
    !write(2239,*) el_file+i
    ! dr_pt%i=dr_pt%i+1
    ! write(2239,rec=i) dr_pt
    ! WRITE(6,*) ' '
    ! WRITE(6,*) '--------------------'
    ! WRITE(6,*) 'i = ',i
    ! WRITE(6,*) 'dr_pt = ',dr_pt


    perc=real(i)/real(n_el)
    if (floor(perc*100) >= cnt_perc) then   
       write(6,'(a,i6,a)') ''//achar(27)//'[38;2;253;252;187m ',int(cnt_perc),'% completed'//achar(27)//'[0m'
       cnt_perc=cnt_perc+step       
    end if  

end do

call cpu_time(finish_time)
call date_and_time(values = dt_end)

elapsed_time=finish_time-start_time
hours=elapsed_time/sph
minutes=mod(elapsed_time, sph)/ms
seconds=mod(minutes*ms, ms)
write(6,'(a,i4,a1,i3,a1,f6.2,a)') ''//achar(27)//'[38;2;10;156;253m cpu time :', int(hours),'h',int(minutes), & 
    'm',seconds,'s'//achar(27)//'[0m'

start_time=real(dt_start(7))+real(dt_start(8))/1000_RSP+real(dt_start(6))*ms+real(dt_start(5))*sph
finish_time=real(dt_end(7))+real(dt_end(8))/1000_RSP+real(dt_end(6))*ms+real(dt_end(5))*sph
elapsed_time=finish_time-start_time
hours=elapsed_time/sph
minutes=mod(elapsed_time, sph)/ms
seconds=mod(minutes*ms, ms)
write(6,'(a,i4,a1,i3,a1,f6.2,a)') ''//achar(27)//'[38;2;10;156;253m wall-clock time :', int(hours),'h',int(minutes), &
    'm',seconds,'s'//achar(27)//'[0m' 



call cpu_time(start_time)
call date_and_time(values = dt_start)
cnt_perc=10

do i=1,n_el
    !inquire(1875,pos=)
    read(1875) dr_pt_str
    
    WRITE(6,*) ' '
    WRITE(6,*) '--------------------'
    WRITE(6,*) 'dr_pt_str = ',dr_pt_str

    ! deallocate(dr_pt_vl%id_pnts)

    ! read(2239,*) el_file
    !write(2239,*) el_file+i
    ! dr_pt%i=dr_pt%i+1
    ! write(2239,rec=i) dr_pt
    ! WRITE(6,*) ' '
    ! WRITE(6,*) '--------------------'
    ! WRITE(6,*) 'i = ',i
    ! WRITE(6,*) 'dr_pt = ',dr_pt


    perc=real(i)/real(n_el)
    if (floor(perc*100) >= cnt_perc) then   
       write(6,'(a,i6,a)') ''//achar(27)//'[38;2;253;252;187m ',int(cnt_perc),'% completed'//achar(27)//'[0m'
       cnt_perc=cnt_perc+step       
    end if  

end do


call cpu_time(finish_time)
call date_and_time(values = dt_end)

elapsed_time=finish_time-start_time
hours=elapsed_time/sph
minutes=mod(elapsed_time, sph)/ms
seconds=mod(minutes*ms, ms)
write(6,'(a,i4,a1,i3,a1,f6.2,a)') ''//achar(27)//'[38;2;10;156;253m cpu time :', int(hours),'h',int(minutes), & 
    'm',seconds,'s'//achar(27)//'[0m'

start_time=real(dt_start(7))+real(dt_start(8))/1000_RSP+real(dt_start(6))*ms+real(dt_start(5))*sph
finish_time=real(dt_end(7))+real(dt_end(8))/1000_RSP+real(dt_end(6))*ms+real(dt_end(5))*sph
elapsed_time=finish_time-start_time
hours=elapsed_time/sph
minutes=mod(elapsed_time, sph)/ms
seconds=mod(minutes*ms, ms)
write(6,'(a,i4,a1,i3,a1,f6.2,a)') ''//achar(27)//'[38;2;10;156;253m wall-clock time :', int(hours),'h',int(minutes), &
    'm',seconds,'s'//achar(27)//'[0m' 


call cpu_time(start_time)
call date_and_time(values = dt_start)

!1875 continue

WRITE(6,*)
WRITE(6,*) 'RAM test'

allocate(vet_ram(n_el))
do i=1,n_el
    vet_ram(i)=i
end do

do j=1,10
   do i=1,n_el
     vet_ram(i)=vet_ram(i)+i
   end do
end do


call cpu_time(finish_time)
call date_and_time(values = dt_end)

elapsed_time=finish_time-start_time
hours=elapsed_time/sph
minutes=mod(elapsed_time, sph)/ms
seconds=mod(minutes*ms, ms)
write(6,'(a,i4,a1,i3,a1,f6.2,a)') ''//achar(27)//'[38;2;10;156;253m cpu time :', int(hours),'h',int(minutes), & 
    'm',seconds,'s'//achar(27)//'[0m'

start_time=real(dt_start(7))+real(dt_start(8))/1000_RSP+real(dt_start(6))*ms+real(dt_start(5))*sph
finish_time=real(dt_end(7))+real(dt_end(8))/1000_RSP+real(dt_end(6))*ms+real(dt_end(5))*sph
elapsed_time=finish_time-start_time
hours=elapsed_time/sph
minutes=mod(elapsed_time, sph)/ms
seconds=mod(minutes*ms, ms)
write(6,'(a,i4,a1,i3,a1,f6.2,a)') ''//achar(27)//'[38;2;10;156;253m wall-clock time :', int(hours),'h',int(minutes), &
    'm',seconds,'s'//achar(27)//'[0m'

end program ram_vs_ssd