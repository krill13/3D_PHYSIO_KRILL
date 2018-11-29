program main_model_parallel

  !====================================================================72
  !
  ! This program advects particles into OPA currents fields.
  !
  ! MapsF 2011, from ChasseJ & LambertN
  !
  !====================================================================72

  use krill_motion_mod
  use listKrill_mod
  use netcdf
  use netcdf_gestion
  use part_def
  use utility_krill_mod
#ifdef PAR
  use omp_lib
#endif

  implicit none


  ! Indices

  integer :: i, j, k, it, it2, ii

  ! Time

  integer :: timer_start, timer_end, clock_rate
  real :: timer

  integer :: nloop, daily_freq

  ! Advection & interpolation

  real*8, dimension(m,n) :: e1v, e2u

  real, dimension(46) :: gdepw, gdept, e3w, e3t

  ! I/O

  integer :: ioerr

  ! I/O netcdf

  integer :: ncid, nc_fid, nc_backid, ndims, nvars, nglobalatts, unlimdimid

  integer :: timeid, lonid, latid, uid, vid, wid, tid, sid

  integer :: diatom_id, flag_id, mesozoo_id, microzoo_id, pon_id, trn_id
 
  integer :: xpobackid, ypobackid, zpobackid
  
  integer :: mnc, nnc, ilonc, tnc, mnc_f, nnc_f, tnc_f, ilonc_f, group_f, partback, seqback, timeback

  integer, dimension(2) :: timenc

  integer, allocatable :: dim(:)

  character(len=20), allocatable :: dim_name(:)

  ! Particles

  type(listKrill) :: mylist

  class(*), pointer :: curr

  integer :: npart_init

  real :: xgrid, ygrid, zgrid
  real :: tp, diat, flag, micz, mesz, max_chlA
  real, dimension(ilo) :: phy2
  real, dimension(1) :: max_chlA_2

  ! Pointer array declaration for parallelization

  integer :: thrid, thrs, nptr

  type parray
     class(Krill), pointer :: ptr
  end type parray

  type(parray), allocatable :: ptrarray(:)

  !====================================================================72
  ! Date & time

  call system_clock(timer_start)

  call idate(today) ! today(1)=month, (2)=day, (3)=year
   
  call itime(now)   ! now(1)=hour, (2)=minute, (3)=second

  write(*,'(/a,i2,1x,i2,1x,i4,a,i2,a,i2,a,i2,a)'),' Date: ',(today(i),i=1,3), ' : ', now(1),'h ',now(2),'m ',now(3),'s'
  print*,''

#ifdef PHYSIO
  dt = 7200 ! 2 hours in sec
#else  
  dt = 7200 ! 30 minutes in sec
#endif 

  ! First kstep of the run
  initial = 1

  kstep   = 0 ! used to update time

  ! Useful daily period
  daily_freq = int(86400/dt)

  ! Start at 0h00 -> nighttime
  light_day = .false.

  print*, '! HAVE TO PROVIDE THE START & END DATES IN THE NAMELIST FILE "Run/run.list" !'
  print*,''
  print*, '! HAVE TO PROVIDE SOME PARTICLES RELATED VARIABLES IN THE NAMELIST TOO      !'
  print*,''
  

  !====================================================================72
  ! Initialization

  call zeros ! Initialize arrays (mostly to zeros)

  open(10,file='run.list',status='old',action='read',err=1,iostat=ioerr)
1 if(ioerr/=0) stop '!!! Problem opening file run.list !!!'

  read(10, nml=run_nml)
  
  close(10)

  open(10,file='run.list',status='old',action='read',err=25,iostat=ioerr)
25 if(ioerr/=0) stop '!!! Problem opening file run.list !!!'
  read(10, nml=part_nml)

  close(10)

  if (part_spec==2) then
     print*,'Krill species = Thysanoessa raschii'
  elseif (part_spec==1) then
     print*,'Krill species = Meganychtiphanes norvegica'
  else
     print*,'Generic particles'
  endif
  print*,''
  
  ! Output frequency; convert unit from h to dt (=kstep)
  output_freq = int(output_freq*3600/dt)

  !--------------------------------------------------------------------72
  ! Define the domain

  ! Get cells dx and dy

  call get_dx_dy(e1v,e2u,m,n)

  dlx = e1v ! Array used in trajectory

  dly = e2u ! Array used in trajectory

  ! Get thickness of layers
  !!! WARNING: hard-coded the 46 original layers of NEMO here, otherwise the
  !layers thickness would be all wrong !!!

  call gdep(46,gdepw,gdept,e3w,e3t)

  dz(1:ilo-1)  = gdepw(2:ilo)

  dz(ilo)      = gdepw(ilo)+e3w(ilo)

  ! Compute vertical grid size

  dd(1)        = dz(1)

  up_depth(1)  = 0.

  mid_depth(1) = dz(1)*.5 !Joel


  do j = 2,ilo

     dd(j)        = dz(j)-dz(j-1)

     up_depth(j)  = dz(j-1) ! Upper interface depth (positive)

     mid_depth(j) = (dz(j)+dz(j-1))*.5 !Joel

  enddo

  ! Read nlayer

  open(10,file='data/bathy_level2.dat',status='old',action='read',err=2,iostat=ioerr)
2 if(ioerr/=0) stop ' !!! Problem opening file data/bathy_level2.dat !!!'

  do i = 1,m
     read(10,*) (nlayer(i,j),j=1,n)
  enddo

  close(10)

  where (nlayer>int2(ilo)) nlayer = int2(ilo)

  !--------------------------------------------------------------------72
  ! Open NetCDF physical input file
  
  ioerr = nf90_open(trim(infile),nf90_nowrite,ncid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem opening NetCDF physical input file '
  endif

  ioerr = nf90_inquire(ncid, ndims, nvars, nglobalatts, unlimdimid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' Problem getting information from NetCDF physical input file'
  endif

  allocate (dim(ndims), dim_name(ndims))

  ! Getting array dimensions and check against declaration in module part_def

  mnc   = 0 ! Initialisation of dimensions for 3D fields

  nnc   = 0

  ilonc = 0

  tnc   = 0


  do i = 1,ndims

     ioerr = nf90_inquire_dimension(ncid,i,dim_name(i),dim(i))
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting dimension name from NetCDF physical input file'
     endif

     if(dim_name(i)=='x') mnc = dim(i) ! dimensions are backward in netcdf

     if(dim_name(i)=='y') nnc = dim(i)

     if(dim_name(i)=='depthu') ilonc = dim(i)

     if(dim_name(i)=='time_counter') tnc = dim(i)

  enddo

  deallocate (dim, dim_name)

  if (mnc==0.or.nnc==0.or.ilonc==0) stop '!!! A dimension (m|n|ilo) is missing !!!'

  if (mnc<m.or.nnc<n.or.ilonc<ilo) stop '!!! A dimension (m|n|ilo) is wrong !!!'

  if (tnc==0) stop ' !!! Problem with time_counter !!!'

  ! Get lon/lat IDs

  ioerr = nf90_inq_varid(ncid,"nav_lon",lonid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting lon ID from NetCDF physical input file'
  endif

  ioerr = nf90_inq_varid(ncid,"nav_lat",latid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting lat ID from NetCDF physical input file'
  endif

  ! Get lon/lat values

  ioerr = nf90_get_var(ncid,lonid,lon)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting lon value from NetCDF physical input file'
  endif

  ioerr = nf90_get_var(ncid,latid,lat)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting lat value from NetCDF physical input file'
  endif

  ! Get u/v/w IDs

  ioerr = nf90_inq_varid(ncid,"vozocrtx",uid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting u ID from NetCDF physical input file'
  endif

  ioerr = nf90_inq_varid(ncid,"vomecrty",vid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting v ID from NetCDF physical input file'
  endif

  !ioerr = nf90_inq_varid(ncid,"vovecrtz",wid)
  !if(ioerr/=nf90_noerr) then
  !   print*, trim(nf90_strerror(ioerr))
  !   stop ' !!! Problem getting w ID from NetCDF physical input file'
  !endif

 ! Get salinity ID

  ioerr = nf90_inq_varid(ncid,"vosaline",sid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting S ID from NetCDF physical input file'
  endif

  ! Get temperature ID

  ioerr = nf90_inq_varid(ncid,"votemper",tid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting Temp ID from NetCDF physical input file'
  endif

  !--------------------------------------------------------------------72
  ! Open NetCDF food input file

#ifdef PHYSIO
  if(FORC_PMZA.eqv..true.) then
        ioerr = nf90_open(trim(infile_food), nf90_nowrite, nc_fid)
          if(ioerr/=nf90_noerr) then
             print*, trim(nf90_strerror(ioerr))
             stop ' !!! Problem opening NetCDF food input file '
          endif

          ioerr = nf90_inquire(nc_fid, ndims, nvars, nglobalatts, unlimdimid)
          if(ioerr/=nf90_noerr) then
             print*, trim(nf90_strerror(ioerr))
             stop ' !!! Problem getting information from NetCDF food input file'
          endif

          allocate (dim(ndims), dim_name(ndims))

          ! Array dimensions MUST be the same than for the physical forcing

          do i = 1,ndims

             ioerr = nf90_inquire_dimension(nc_fid,i,dim_name(i),dim(i))
             if(ioerr/=nf90_noerr) then
                print*, trim(nf90_strerror(ioerr))
                stop ' !!! Problem getting dimension name from NetCDF food input file'
             endif

             if(dim_name(i)=='x') mnc_f = dim(i) ! dimensions are backward in netcdf

             if(dim_name(i)=='y') nnc_f = dim(i)

             if(dim_name(i)=='z') ilonc_f = dim(i)

             if(dim_name(i)=='time_counter') tnc_f = dim(i)

             if(dim_name(i)=='jptra') group_f = dim(i)

          enddo

          deallocate (dim, dim_name)

          print*, mnc_f, mnc, nnc_f, nnc, ilonc_f, ilonc, tnc_f, tnc

          if (mnc_f/=mnc.or.nnc_f/=nnc.or.ilonc_f/=ilonc.or.tnc_f/=tnc.or.group_f==0) &
             stop '!!! A dimension (m|n|ilo) is wrong !!!'

          ! Get diat / flag / meso / micro IDs

          ioerr = nf90_inq_varid(nc_fid,"trn",trn_id)
          if(ioerr/=nf90_noerr) then
             print*, trim(nf90_strerror(ioerr))
             stop ' !!! Problem getting trn ID from NetCDF food input file'
          endif
 else

          ioerr = nf90_open(trim(infile_food),nf90_nowrite,nc_fid)
          if(ioerr/=nf90_noerr) then
          print*, trim(nf90_strerror(ioerr))
             stop ' !!! Problem opening NetCDF food input file '
          endif

          ioerr = nf90_inquire(nc_fid, ndims, nvars, nglobalatts, unlimdimid)
          if(ioerr/=nf90_noerr) then
             print*, trim(nf90_strerror(ioerr))
             stop ' !!! Problem getting information from NetCDF food input file'
          endif

          allocate (dim(ndims), dim_name(ndims))

          ! Array dimensions MUST be the same than for the physical forcing

          do i = 1,ndims

             ioerr = nf90_inquire_dimension(nc_fid,i,dim_name(i),dim(i))
             if(ioerr/=nf90_noerr) then
                print*, trim(nf90_strerror(ioerr))
                stop ' !!! Problem getting dimension name from NetCDF food input file'
             endif

             if(dim_name(i)=='x') mnc_f = dim(i) ! dimensions are backward in netcdf

             if(dim_name(i)=='y') nnc_f = dim(i)

             if(dim_name(i)=='z') ilonc_f = dim(i)

             if(dim_name(i)=='time_counter') tnc_f = dim(i)

          enddo

          deallocate (dim, dim_name)

          print*, mnc_f, mnc, nnc_f, nnc, ilonc_f, ilonc, tnc_f, tnc

          if (mnc_f/=mnc.or.nnc_f/=nnc.or.ilonc_f/=ilonc.or.tnc_f/=tnc) &
             stop '!!! A dimension (m|n|ilo) is wrong !!!'

          ! Get diat / flag / meso / micro IDs

          ioerr = nf90_inq_varid(nc_fid,"diat",diatom_id)
          if(ioerr/=nf90_noerr) then
             print*, trim(nf90_strerror(ioerr))
             stop ' !!! Problem getting diat ID from NetCDF food input file'
          endif

          ioerr = nf90_inq_varid(nc_fid,"flag",flag_id)
          if(ioerr/=nf90_noerr) then
             print*, trim(nf90_strerror(ioerr))
             stop ' !!! Problem getting flag ID from NetCDF food input file'
          endif

          ioerr = nf90_inq_varid(nc_fid,"meso",mesozoo_id)
          if(ioerr/=nf90_noerr) then
             print*, trim(nf90_strerror(ioerr))
             stop ' !!! Problem getting meso ID from NetCDF food input file'
          endif

          ioerr = nf90_inq_varid(nc_fid,"micro",microzoo_id)
          if(ioerr/=nf90_noerr) then
             print*, trim(nf90_strerror(ioerr))
             stop ' !!! Problem getting micro ID from NetCDF food input file'
          endif

          !ioerr = nf90_inq_varid(nc_fid,"pon",pon_id)
          !if(ioerr/=nf90_noerr) then
          !   print*, trim(nf90_strerror(ioerr))
          !   stop ' !!! Problem getting pon ID from NetCDF food input file'
          !endif
 endif

#endif
  !---------------------------------------------------------------------
  ! Define inner time loop according to input file record frequency

  ! Get first 2 time axis values to find input file timestep (must be constant!)

  ioerr = nf90_inq_varid(ncid,"time_counter",timeid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting time ID from NetCDF input file'
  endif
  
  ioerr = nf90_get_var(ncid,timeid,timenc,(/1/),(/2/))
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting first 2 time value2 from NetCDF input file'
  endif

  ! Assuming time_counter unit is always in second

  dtnc  = timenc(2)-timenc(1)

  nloop = int(dtnc/dt)

  print*,'Input file timestep =',dtnc,' s. Inner time loop =',nloop

  print*, 'jour départ:', dstart, mstart, ystart
  !--------------------------------------------------------------------72
  ! Fix the problem with date in OPA

  ! CAUTION ! HAVE TO CHANGE TIME ORIGIN IN THE NAMELIST ACCORDING TO INPUT FILE
  call cday2(tnc_origin(1),tnc_origin(2),tnc_origin(3),kd_ini)

  ! Get the first and last simulation day
  call cday2(dstart,mstart,ystart,istart)
  print*, 'istart=', istart, 'initial jour=', kd_ini

  istart = (istart-kd_ini)*86400/dtnc ! unit = input file record period
  print*, 'istart=', istart

#ifdef PHYSIO
  if (istart<365) stop '!!! Initial time must be 2006 01 01 or more !!!'
#endif


  call cday2(dend,mend,yend,iend)

  iend = (iend-kd_ini)*86400/dtnc

  ! Enables computation backward in time: currents read backward and sign
  ! changed
#ifdef BACK

  if (iend>istart) stop ' !!! CHECK run.list FILE -> END DATE SHOULD BE < START DATE IN BACKWARD COMPUTATION !!!'

  final = (istart-iend+1)*nloop ! unit = dt (kstep)

#else

  if (iend<istart) stop ' !!! CHECK run.list FILE -> END DATE SHOULD BE > START DATE IN FORWARD COMPUTATION !!!'

  final = (iend-istart+1)*nloop

#endif

  print*, 'final =', final, 'iend =', iend, 'istart =', istart, 'outputfreq =', output_freq

  !--------------------------------------------------------------------72
  ! Get initial u/v/w values

  ioerr = nf90_get_var(ncid,uid,u2,(/1,1,1,istart/))
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting u value from NetCDF input file'
  endif

  ioerr = nf90_get_var(ncid,vid,v2,(/1,1,1,istart/))
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting v value from NetCDF input file'
  endif

  !ioerr = nf90_get_var(ncid,wid,w2,(/1,1,1,istart/))
  !if(ioerr/=nf90_noerr) then
  !   print*, trim(nf90_strerror(ioerr))
  !   stop ' !!! Problem getting v value from NetCDF input file'
  !endif
  
  ! -------------------------------------------------------------------72
  ! Get initial particles position from backward simulation

#ifdef INITB

  ! Open NetCDF backward position input file
  ioerr = nf90_open(trim(initback),nf90_nowrite,nc_backid)

  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem opening NetCDF backward input file '
  endif

  ioerr = nf90_inquire(nc_backid, ndims, nvars, nglobalatts, unlimdimid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' Problem getting information from NetCDF backward input file'
  endif

  allocate (dim(ndims), dim_name(ndims))

  ! Getting array dimensions and check against declaration in module part_def

  partback = 0 ! Initialisation of dimensions for 3D fields

  seqback  = 0

  timeback = 0


  do i = 1,ndims

     ioerr = nf90_inquire_dimension(nc_backid,i,dim_name(i),dim(i))
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting dimension name from NetCDF backward input file'
     endif

     if(dim_name(i)=='krill')    partback = dim(i) ! dimensions are backward in netcdf

     if(dim_name(i)=='sequence') seqback  = dim(i)

     if(dim_name(i)=='time')     timeback = dim(i)

  enddo

  deallocate (dim, dim_name)

  print*, partback, seqback, timeback

  if (partback==0.or.seqback==0.or.timeback==0) stop '!!! A dimension (m|n|ilo) is missing !!!'

  ! Get xpo/ypo/zpo back IDs

  ioerr = nf90_inq_varid(nc_backid,"xpo",xpobackid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting xpo ID from NetCDF backward input file'
  endif

  ioerr = nf90_inq_varid(nc_backid,"ypo",ypobackid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting ypo ID from NetCDF backward input file'
  endif

  ioerr = nf90_inq_varid(nc_backid,"zpo",zpobackid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting zpo ID from NetCDF backward input file'
  endif

  ! Get xpo/ypo/zpo back values
  ioerr = nf90_get_var(nc_backid,xpobackid,xpoback, (/1,1,1/),(/nbpb,1,nb_back_time/))
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting xpo_back value from NetCDF backward input file'
  endif

  ioerr = nf90_get_var(nc_backid,ypobackid,ypoback,(/1,1,1/),(/nbpb,1,nb_back_time/))
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting ypo_back value from NetCDF backward input file'
  endif

  ioerr = nf90_get_var(nc_backid,zpobackid,zpoback,(/1,1,1/),(/nbpb,1,nb_back_time/))
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem getting zpo_back value from NetCDF backward input file'
  endif

  print*, xpoback(1,1,1), xpoback(1,1,nb_back_time)
#endif

 
#ifdef BACK
  u2 = -1.*u2
  v2 = -1.*v2
  !w2 = -1.*w2
#endif


  !====================================================================72
  ! MAIN LOOP
#ifdef BACK
  do it = istart,iend+1,-1 ! Main loop in time, following input records frequency
#else
  do it = istart,iend-1
#endif
#ifdef DEBUG
     print*, 'Read physical forcing'
#endif

     !-----------------------------------------------------------------72
     ! Linear interpolation ! MapsF

     u1 = u2

     v1 = v2

     !w1 = w2

     u = u1 ! For output purpose

     v = v1

     !w = w1

#ifdef BACK
     ioerr = nf90_get_var(ncid,uid,u2,(/1,1,1,it-1/),(/m,n,ilo,1/))
#else
     ioerr = nf90_get_var(ncid,uid,u2,(/1,1,1,it+1/),(/m,n,ilo,1/))
#endif
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting u2 value from NetCDF input file'
     endif

#ifdef BACK
     ioerr = nf90_get_var(ncid,vid,v2,(/1,1,1,it-1/),(/m,n,ilo,1/))
#else
     ioerr = nf90_get_var(ncid,vid,v2,(/1,1,1,it+1/),(/m,n,ilo,1/))
#endif
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting v2 value from NetCDF input file'
     endif

!#ifdef BACK
!     ioerr = nf90_get_var(ncid,wid,w2,(/1,1,1,it-1/),(/m,n,ilo,1/)) 
!#else
!     ioerr = nf90_get_var(ncid,wid,w2,(/1,1,1,it+1/),(/m,n,ilo,1/)) 
!#endif
!     if(ioerr/=nf90_noerr) then
!        print*, trim(nf90_strerror(ioerr))
!        stop ' !!! Problem getting w2 value from NetCDF input file'
!     endif

#ifdef BACK
     u2 = -1.*u2
     v2 = -1.*v2
!     w2 = -1.*w2
#endif
#ifdef DEBUG
     print*,'  Horizontal components U & V'
#endif

     ioerr = nf90_get_var(ncid,sid,salt,(/1,1,1,it/),(/m,n,2,1/)) ! Need only the first 2 layers
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting S value from NetCDF input file'
     endif

     salt2 = sum(salt,3)*0.5 ! Average of the first 2 layers
#ifdef DEBUG
     print*,'  Salinity'
#endif

     where (nlayer==0) salt2 = nf90_fill_real ! For later use when recording values in netcdf file

     ioerr = nf90_get_var(ncid,tid,temp,(/1,1,1,it/),(/m,n,ilo,1/))
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting Temp value from NetCDF input file'
     endif
#ifdef DEBUG
     print*,'  Temperature'
#endif

#ifdef DEBUG
     print*,'Read food forcing'
#endif
     ! Get diat/flag/meso/micro and convert into food values

#ifdef PHYSIO
 if(FORC_PMZA.eqv..true.) then

     ioerr = nf90_get_var(nc_fid,trn_id,diatom,(/1,1,1,1,it/),(/m,n,ilo,1,1/))
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting diat value from NetCDF food input file'
     endif
     diatom = C2N(1) * 14 * diatom / 1000 ! conversion from mMol N m^-3 -> mg C L^-1

#ifdef DEBUG
     print*,'  Diatom', diatom(140,170,:)
     print*,'istart',  istart 
#endif

     ioerr = nf90_get_var(nc_fid,trn_id,flagel,(/1,1,1,2,it/),(/m,n,ilo,1,1/))
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting flag value from NetCDF food input file'
     endif
     flagel = C2N(2) * 14 * flagel / 1000
#ifdef DEBUG
     print*,'  Flagellates', flagel(140,170,:)
     print*,'istart',  istart 
#endif

     ioerr = nf90_get_var(nc_fid,trn_id,microzoo,(/1,1,1,6,it/),(/m,n,ilo,1,1/))
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting micro value from NetCDF food input file'
     endif
     microzoo = C2N(3) * 14 * microzoo / 1000
#ifdef DEBUG
     print*,'  Microzooplankton', microzoo(140,170,:)
     print*,'istart',  istart 
#endif

     !PHYTO = diatom + flagel + microzoo
 
     ioerr = nf90_get_var(nc_fid,trn_id,mesozoo,(/1,1,1,5,it/),(/m,n,ilo,1,1/))
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting meso value from NetCDF food input file'
     endif
     mesozoo = C2N(4) * 14 * mesozoo / 1000

#ifdef DEBUG
     print*,'  Mesozooplankton', mesozoo(140,170,:)
     print*,'istart',  istart 
#endif
else 

     ioerr = nf90_get_var(nc_fid,diatom_id,diatom,(/1,1,1,it/),(/m,n,ilo,1/))
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting diat value from NetCDF food input file'
     endif
     diatom = C2N(1) * 14 * diatom / 1000 ! conversion from mMol N m^-3 -> mg C L^-1

#ifdef DEBUG
     print*,'  Diatom'
#endif

     ioerr = nf90_get_var(nc_fid,flag_id,flagel,(/1,1,1,it/),(/m,n,ilo,1/))
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting flag value from NetCDF food input file'
     endif
     flagel = C2N(2) * 14 * flagel / 1000
#ifdef DEBUG
     print*,'  Flagellates'
#endif

     ioerr = nf90_get_var(nc_fid,microzoo_id,microzoo,(/1,1,1,it/),(/m,n,ilo,1/))
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting micro value from NetCDF food input file'
     endif
     microzoo = C2N(3) * 14 * microzoo / 1000
#ifdef DEBUG
     print*,'  Microzooplankton'
#endif
 
     !PHYTO = diatom + flagel + microzoo
 
     ioerr = nf90_get_var(nc_fid,mesozoo_id,mesozoo,(/1,1,1,it/),(/m,n,ilo,1/))
     if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' !!! Problem getting meso value from NetCDF food input file'
     endif
     mesozoo = C2N(4) * 14 * mesozoo / 1000
#ifdef DEBUG
     print*,'  Mesozooplankton'
#endif
endif     
#endif

     !-----------------------------------------------------------------72

print*, 'it', it
print*,'istart', istart
     if(it==istart) then
 
        tzero    = 0

        tzero(4) = dstart

        tzero(5) = mstart

        tzero(6) = ystart

        time     = tzero

        call sun_time(time_sunrise,time_sunset,48.5,-63.) ! NL -69.3

#ifdef PHYSIO
        p_night   = (24 - (time_sunset - time_sunrise))/24
        ratio_ing = 1 / p_night
#endif

        ! Initialize particles

        call generateKrill(mylist,'run.list')
#ifdef DEBUG
     print*,'Generate krill particles'
#endif

        call mylist%netcdfKrill(0) ! Create netCDF file & record initial particles data 
#ifdef DEBUG
     print*,'Create NetCDF output file for particles'
#endif

!        call output_netcdf(0)
#ifdef DEBUG
     print*,'Output of initial particles conditions'
#endif

        npart_init = npart

     endif

     if (mod(kstep,daily_freq)==0) then 
        write(*,'(/a,3(1x,i4))') &
        'Time: ',(time(ii),ii=6,4,-1)
     endif       

    !------------------------------------------------------------------72
    ! Inner time loop

#ifdef DEBUG
     print*,'Inner time loop'
#endif

     du = u2-u1

     dv = v2-v1

     !dw = w2-w1

     do it2 = 0,nloop-1

        u = u1+du*it2/nloop

        v = v1+dv*it2/nloop

        !w = w1+dw*it2/nloop
        
        call optimal_depth(mylist)

        nptr = mylist%n_krill()
        allocate(ptrarray(nptr))

        call mylist%reset()
        i = 1

        do while(mylist%moreValues())

           select type(curr => mylist%currentValue())
           class is (Krill)
              ptrarray(i)%ptr => curr
           end select

           i = i + 1

           call mylist%next()

        enddo

        call mylist%reset()


        do i = 1, nptr

              xpo  = ptrarray(i)%ptr%get_xpo()
              ypo  = ptrarray(i)%ptr%get_ypo()
              zpo  = ptrarray(i)%ptr%get_zpo()

              tp   = update(temp,     m, n, ilo, 's', xpo, ypo, zpo)
#ifdef PHYSIO
              diat = update(diatom,   m, n, ilo, 's', xpo, ypo, zpo)
              flag = update(flagel,   m, n, ilo, 's', xpo, ypo, zpo)
              micz = update(microzoo, m, n, ilo, 's', xpo, ypo, zpo)
              mesz = update(mesozoo,  m, n, ilo, 's', xpo, ypo, zpo)
              phy2 = diatom(xpo,ypo,:) + flagel(xpo,ypo,:)
              max_chlA_2 = maxval(phy2)
              max_chlA = max_chlA_2(1)

#endif
              call ptrarray(i)%ptr%set_tp(tp)
#ifdef PHYSIO
              call ptrarray(i)%ptr%set_diat(diat)
              call ptrarray(i)%ptr%set_flag(flag)
              call ptrarray(i)%ptr%set_micz(micz)
              call ptrarray(i)%ptr%set_mesz(mesz) 
              call ptrarray(i)%ptr%set_max_chlA(max_chlA) 
              call ptrarray(i)%ptr%set_light_day(light_day)
#endif 
        enddo

        deallocate(ptrarray)


        call trajectory(mylist)
#ifdef DEBUG
        print*,'Particles trajectory'
#endif

        !--------------------------------------------------------------72
        ! Spatial interpolation of forcing fields for each particle

        ! Create pointer array for OMP parallelization

        nptr = mylist%n_krill()
        allocate(ptrarray(nptr))
        call mylist%reset()
        i = 1

        do while(mylist%moreValues())

           select type(curr => mylist%currentValue())
           class is (Krill)
              ptrarray(i)%ptr => curr
           end select

           i = i + 1

           call mylist%next()

        enddo

        call mylist%reset()

#ifdef PAR
#ifdef DEBUG
        print*, 'Number of processors      = ', omp_get_num_procs()
        print*, 'Maximum number of threads = ', omp_get_max_threads()
#endif
#endif

#ifdef PAR
#ifdef DEBUG
!$OMP PARALLEL DO &
!$OMP PRIVATE(i,k,thrs,thrid,xpo,ypo,zpo,xgrid,ygrid,zgrid) &
!$OMP PRIVATE(tp,diat,flag,mesz,micz) &
!$OMP SHARED(nptr,ptrarray)
#else
!$OMP PARALLEL DO &
!$OMP PRIVATE(i,k,xpo,ypo,zpo,xgrid,ygrid,zgrid) &
!$OMP PRIVATE(tp,diat,flag,mesz,micz) &
!$OMP SHARED(nptr,ptrarray)
#endif
#endif
        do i = 1, nptr

#ifdef PAR
#ifdef DEBUG
           thrs  = omp_get_num_threads()
           thrid = omp_get_thread_num()
           print*, 'Number of threads = ', thrs, &
                   ';   Thread ID = ', thrid, &
                   ';   DO loop index = ',  i
#endif
#endif
           if( ptrarray(i)%ptr%get_cell_out() .eqv. .false.) then  

              xpo  = ptrarray(i)%ptr%get_xpo()
              ypo  = ptrarray(i)%ptr%get_ypo()
              zpo  = ptrarray(i)%ptr%get_zpo()

              tp   = update(temp,     m, n, ilo, 's', xpo, ypo, zpo)
#ifdef PHYSIO
              diat = update(diatom,   m, n, ilo, 's', xpo, ypo, zpo)
              flag = update(flagel,   m, n, ilo, 's', xpo, ypo, zpo)
              micz = update(microzoo, m, n, ilo, 's', xpo, ypo, zpo)
              mesz = update(mesozoo,  m, n, ilo, 's', xpo, ypo, zpo)
#endif
              call ptrarray(i)%ptr%set_tp(tp)
#ifdef PHYSIO
              call ptrarray(i)%ptr%set_diat(diat)
              call ptrarray(i)%ptr%set_flag(flag)
              call ptrarray(i)%ptr%set_micz(micz)
              call ptrarray(i)%ptr%set_mesz(mesz) 
              call ptrarray(i)%ptr%set_light_day(light_day)
#endif 
              !call ptrarray(i)%ptr%set_ratio_ing(ratio_ing)
 
           end if

        enddo
#ifdef PAR
!$OMP END PARALLEL DO
#endif

        deallocate(ptrarray)
#ifdef DEBUG
        print*, 'Particle forcing update'
#endif

        !--------------------------------------------------------------72
        ! Physiological routine
#ifdef PHYSIO  
        call evolveList(mylist)
#endif

#ifdef DEBUG
        print*, 'Krill biology'
#endif

        if (kstep>0.and.mod(kstep,output_freq)==0) then
           call mylist%netcdfKrill(1)
        endif

!        call output_netcdf(1)

        kstep = kstep+1

        call update2(kstep)

     enddo

  enddo ! MAIN LOOP
      
  call mylist%netcdfKrill(2) ! Close file
  !call output_netcdf(2)


  ioerr = nf90_close(ncid)
  if(ioerr/=nf90_noerr) then
     print*, trim(nf90_strerror(ioerr))
     stop ' !!! Problem closing forcing files'
  endif


   ! Print computation time
  call system_clock(timer_end, clock_rate)
  timer = real(timer_end-timer_start) / real(clock_rate)

  print*, ' '
  print*, '***   THE END   ***'
  write(*,'(/A,I10,A$,I10,A$,F10.2,A)') 'Computation time = ', &
                                        floor(timer/3600.),'h', &
                                        floor(amod(timer/60.,60.)),'m', &
                                        amod(timer,60.),'s'
  print*, ' '

!======================================================================72

end program main_model_parallel
