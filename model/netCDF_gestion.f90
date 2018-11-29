module netcdf_gestion

  use netcdf

  implicit none

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine output_netcdf(action)

  ! Output environmental variables in netcdf
  ! file
  !   Argument :
  !   action = 0 => initialization
  !   action = 1 => Record data
  !   action = 2 => Finalization

  use part_def

  use netcdf

  implicit none


  integer, intent(in) :: action

  integer :: ioerr, xdimid, ydimid, zdimid, timedimid, zedgesdimid, &
             zid, zedgesid, lonid, latid, xid, yid, nlayerid

  integer, save :: ncid, timeid, srid, ssid, sid, tid, fxid, fyid,  &
                   ncinc, nctlen

  integer :: i, j, k

  real, dimension(m,n,20) :: dummy ! Only record the first 20 layers; below irrelevent

  real, save :: ncfreq, nctime

  character(len=100) :: date_print, filename_nc

  character(len=3), dimension(12) :: month_nc =(/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/)


  select case (action)

  case (0)

     ! Create file

     filename_nc = trim(outfile)//'.nc'

     ioerr = nf90_create(trim(filename_nc),nf90_clobber,ncid)
     if (ioerr/=nf90_noerr) print*,'Error opening netcdf file',trim(nf90_strerror(ioerr))

     ! Dimensions

     ioerr = nf90_def_dim(ncid,"x",m,xdimid)
     if (ioerr/=nf90_noerr) print*,'Error defining X dim in netcdf file',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_dim(ncid,"y",n,ydimid)
     if (ioerr/=nf90_noerr) print*,'Error defining Y dim in netcdf file',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_dim(ncid,"depth",20,zdimid) ! Only record the first 20 layers; below irrelevent
     if (ioerr/=nf90_noerr) print*,'Error defining Z dim in netcdf file',trim(nf90_strerror(ioerr))

     ! Edges for ferret use
     ioerr = nf90_def_dim(ncid,"depth_edges",21,zedgesdimid) ! Only record the first 20 layers; below irrelevent
     if (ioerr/=nf90_noerr) print*,'Error defining Zedges dim in netcdf file',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_dim(ncid,"time",nf90_unlimited,timedimid)
     if (ioerr/=nf90_noerr) print*,'Error defining T dim in netcdf file',trim(nf90_strerror(ioerr))


     ! Variables

     ioerr = nf90_def_var(ncid,"time",nf90_real,(/timedimid/),timeid)
     if (ioerr/=nf90_noerr) print*,'Error defining T var',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_var(ncid,"sunrise",nf90_real,(/timedimid/),srid)
     if (ioerr/=nf90_noerr) print*,'Error defining SR var',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_var(ncid,"sunset",nf90_real,(/timedimid/),ssid)
     if (ioerr/=nf90_noerr) print*,'Error defining SS var',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_var(ncid,"depth",nf90_real,(/zdimid/),zid)
     if (ioerr/=nf90_noerr) print*,'Error defining Z var',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_var(ncid,"depth_edges",nf90_real,(/zedgesdimid/),zedgesid)
     if (ioerr/=nf90_noerr) print*,'Error defining Zedges var',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_var(ncid,"lon",nf90_real,(/xdimid,ydimid/),lonid)
     if (ioerr/=nf90_noerr) print*,'Error defining Lon var',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_var(ncid,"lat",nf90_real,(/xdimid,ydimid/),latid)
     if (ioerr/=nf90_noerr) print*,'Error defining Lat var',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_var(ncid,"dx",nf90_real,(/xdimid,ydimid/),xid)
     if (ioerr/=nf90_noerr) print*,'Error defining dX var',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_var(ncid,"dy",nf90_real,(/xdimid,ydimid/),yid)
     if (ioerr/=nf90_noerr) print*,'Error defining dY var',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_var(ncid,"nlayer",nf90_int2,(/xdimid,ydimid/),nlayerid)
     if (ioerr/=nf90_noerr) print*,'Error defining nlayer var',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_var(ncid,"salt",nf90_real,(/xdimid,ydimid,timedimid/),sid)
     if (ioerr/=nf90_noerr) print*,'Error defining S var',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_var(ncid,"temp",nf90_real,(/xdimid,ydimid,zdimid,timedimid/),tid)
     if (ioerr/=nf90_noerr) print*,'Error defining Temp var',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_var(ncid,"flux",nf90_real,(/xdimid,ydimid,timedimid/),fxid)
     if (ioerr/=nf90_noerr) print*,'Error defining Flux var',trim(nf90_strerror(ioerr))

     ioerr = nf90_def_var(ncid,"fluy",nf90_real,(/xdimid,ydimid,timedimid/),fyid)
     if (ioerr/=nf90_noerr) print*,'Error defining Fluy var',trim(nf90_strerror(ioerr))


     ! Attributes

     ioerr = nf90_put_att(ncid,NF90_GLOBAL,"File_name",filename_nc)
     if (ioerr/=nf90_noerr) print*,'Error defining FileName att',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,NF90_GLOBAL,"Description",'Lagrangian experiment')
     if (ioerr/=nf90_noerr) print*,'Error defining Description att',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,NF90_GLOBAL,"Version",'1.0')
     if (ioerr/=nf90_noerr) print*,'Error defining Version att',trim(nf90_strerror(ioerr))

     write(date_print,'(i3,i3,i5,a,3(i2,a1))'),today(2),today(1),today(3),' -',now(1),'h',now(2),'m',now(3),'s' ! JJ MM AAAA -- HHhMMmSSs

     ioerr = nf90_put_att(ncid,NF90_GLOBAL,"Date",trim(date_print))
     if (ioerr/=nf90_noerr) print*,'Error defining Date att',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,timedimid,"point_spacing",'even')
     if (ioerr/=nf90_noerr) print*,'Error defining point_spacing att for T var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,timeid,"Long_name",'Days')
     if (ioerr/=nf90_noerr) print*,'Error defining Long_name att for T var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,timeid,"Units",trim(tnc_units))
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for T var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,timeid,"time_origin",trim(tnc_origin_out))
     if (ioerr/=nf90_noerr) print*,'Error defining time_origin att for T var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,srid,"Long_name",'Sun rise')
     if (ioerr/=nf90_noerr) print*,'Error defining Long_name att for SR var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,srid,"Units",'hour')
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for SR var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,ssid,"Long_name",'Sun set')
     if (ioerr/=nf90_noerr) print*,'Error defining Long_name att for SS var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,ssid,"Units",'hour')
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for SS var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,zid,"Positive",'down')
     if (ioerr/=nf90_noerr) print*,'Error defining Positive att for Z var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,zid,"edges",'depth_edges')
     if (ioerr/=nf90_noerr) print*,'Error defining edges att for Z var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,zid,"Long_name",'Layers depth')
     if (ioerr/=nf90_noerr) print*,'Error defining Long_name att for Z var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,zid,"Units",'m')
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for Z var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,lonid,"Long_name",'Longitude')
     if (ioerr/=nf90_noerr) print*,'Error defining Long_name att for Lon var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,lonid,"Units",'degrees_east')
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for Lon var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,lonid,"valid_min",-7.138476e+01)
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for Lon var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,lonid,"valid_max",-6.299908e+01)
     if (ioerr/=nf90_noerr) print*,'Error defining valid_max att for Lon var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,latid,"Long_name",'Latitude')
     if (ioerr/=nf90_noerr) print*,'Error defining Long_name att for Lat var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,latid,"Units",'degrees_north')
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for Lat var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,latid,"valid_min",3.852767e+01)
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for Lat var',trim(nf90_strerror(ioerr))

ioerr = nf90_put_att(ncid,latid,"valid_max",4.223735e+01)
     if (ioerr/=nf90_noerr) print*,'Error defining valid_max att for Lat var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,xid,"Long_name",'Zonal cell width')
     if (ioerr/=nf90_noerr) print*,'Error defining Long_name att for dX var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,xid,"Units",'m')
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for dX var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,yid,"Long_name",'Meridional cell width')
     if (ioerr/=nf90_noerr) print*,'Error defining Long_name att for dY var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,yid,"Units",'m')
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for dY var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,nlayerid,"Long_name",'Number of vertical layer - Max 20')
     if (ioerr/=nf90_noerr) print*,'Error defining Long_name att for nlayer var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,nlayerid,"Units",'count')
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for nlayer var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,nlayerid,"_FillValue",nf90_fill_int2)
     if (ioerr/=nf90_noerr) print*,'Error defining fillvalue att for nlayer var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,fxid,"Long_name",'Zonal currents at day-time depth')
     if (ioerr/=nf90_noerr) print*,'Error defining Long_name att for Flux var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,fxid,"Units",'m/s')
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for Flux var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,fxid,"_FillValue",nf90_fill_real)
     if (ioerr/=nf90_noerr) print*,'Error defining fillvalue att for Flux var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,fyid,"Long_name",'Meridional currents at day-time depth')
     if (ioerr/=nf90_noerr) print*,'Error defining Long_name att for Fluy var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,fyid,"Units",'m/s')
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for Fluy var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,fyid,"_FillValue",nf90_fill_real)
     if (ioerr/=nf90_noerr) print*,'Error defining fillvalue att for Fluy var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,sid,"Long_name",'Surface (K=1:2@AVE) salinity')
     if (ioerr/=nf90_noerr) print*,'Error defining Long_name att for S var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,sid,"Units",'PSU')
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for S var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,sid,"_FillValue",nf90_fill_real)
     if (ioerr/=nf90_noerr) print*,'Error defining fillvalue att for S var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,tid,"Long_name",'Temperature')
     if (ioerr/=nf90_noerr) print*,'Error defining Long_name att for Temp var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,tid,"Units",'°C')
     if (ioerr/=nf90_noerr) print*,'Error defining Units att for Temp var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_att(ncid,tid,"_FillValue",nf90_fill_real)
     if (ioerr/=nf90_noerr) print*,'Error defining fillvalue att for Temp var',trim(nf90_strerror(ioerr))



    ! End definition

     ioerr = nf90_enddef(ncid)
     if (ioerr/=nf90_noerr) print*,'Error exiting define mode in netcdf file',trim(nf90_strerror(ioerr))


     ! Put time

     ncinc = 1

     ncfreq = output_freq*dt/86400.

     nctime = istart*dtnc/86400.

     ioerr = nf90_put_var(ncid,timeid,nctime,(/ncinc/))
     if (ioerr/=nf90_noerr) print*,'Error writing T var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_var(ncid,srid,time_sunrise,(/ncinc/))
     if (ioerr/=nf90_noerr) print*,'Error writing SR var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_var(ncid,ssid,time_sunset,(/ncinc/))
     if (ioerr/=nf90_noerr) print*,'Error writing SS var',trim(nf90_strerror(ioerr))


     ! Put mid-layer depth with edges (for ferret use)

     ioerr = nf90_put_var(ncid,zid,mid_depth(1:20))
     if (ioerr/=nf90_noerr) print*,'Error writing Z var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_var(ncid,zedgesid,(/ 0., dz(1:20) /))
     if (ioerr/=nf90_noerr) print*,'Error writing Zedges var',trim(nf90_strerror(ioerr))

     ! Put lon/lat

     ioerr = nf90_put_var(ncid,lonid,lon,(/ 1, 1 /),(/ m, n /))
     if (ioerr/=nf90_noerr) print*,'Error writing Lon var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_var(ncid,latid,lat,(/ 1, 1 /),(/ m, n /))
     if (ioerr/=nf90_noerr) print*,'Error writing Lat var',trim(nf90_strerror(ioerr))

     ! Put cells width

     ioerr = nf90_put_var(ncid,xid,dlx,(/ 1, 1 /),(/ m, n /))
     if (ioerr/=nf90_noerr) print*,'Error writing dX var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_var(ncid,yid,dly,(/ 1, 1 /),(/ m, n /))
     if (ioerr/=nf90_noerr) print*,'Error writing dY var',trim(nf90_strerror(ioerr))

     ! Put nlayer

     where (nlayer==0) nlayer = nf90_fill_int2

     ioerr = nf90_put_var(ncid,nlayerid,nlayer,(/ 1, 1 /),(/ m, n /))
     if (ioerr/=nf90_noerr) print*,'Error writing nlayer var',trim(nf90_strerror(ioerr))

     where (nlayer==nf90_fill_int2) nlayer = 0

     ! Put initial salinity

     ioerr = nf90_put_var(ncid,sid,salt2,(/ 1, 1, ncinc /),(/ m, n, 1 /))
     if (ioerr/=nf90_noerr) print*,'Error writing S var',trim(nf90_strerror(ioerr))

     ! Put initial temperature

     dummy = nf90_fill_real

     do i = 1,m
        do j = 1,n

           if (nlayer(i,j)>0) then

              do k = 1,nlayer(i,j)
                 dummy(i,j,k) = temp(i,j,k)
              enddo

           endif

        enddo
     enddo

     ioerr = nf90_put_var(ncid,tid,dummy,(/ 1, 1, 1, ncinc /),(/ m, n, ilo, 1/))
     if (ioerr/=nf90_noerr) print*,'Error writing Temp var',trim(nf90_strerror(ioerr))

     ! Put initial fluxes

     ioerr = nf90_put_var(ncid,fxid,flux,(/ 1, 1, ncinc /),(/ m, n, 1 /))
     if (ioerr/=nf90_noerr) print*,'Error writing Flux var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_var(ncid,fyid,fluy,(/ 1, 1, ncinc /),(/ m, n, 1 /))
     if (ioerr/=nf90_noerr) print*,'Error writing Fluy var',trim(nf90_strerror(ioerr))


  case (1)

     ncinc = ncinc+1

     ! Put timestep

#ifdef BACK
     nctime = nctime-ncfreq
#else
     nctime = nctime+ncfreq
#endif

     ioerr = nf90_put_var(ncid,timeid,nctime,(/ncinc/))
     if (ioerr/=nf90_noerr) print*,'Error writing T var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_var(ncid,srid,time_sunrise,(/ncinc/))
     if (ioerr/=nf90_noerr) print*,'Error writing SR var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_var(ncid,ssid,time_sunset,(/ncinc/))
     if (ioerr/=nf90_noerr) print*,'Error writing SS var',trim(nf90_strerror(ioerr))

     ! Put salinity

     ioerr = nf90_put_var(ncid,sid,salt2,(/ 1, 1, ncinc /),(/ m, n, 1 /))
     if (ioerr/=nf90_noerr) print*,'Error writing S var',trim(nf90_strerror(ioerr))

     
     ! Put temperature

     dummy = nf90_fill_real

     do i = 1,m
        do j = 1,n

           if (nlayer(i,j)>0) then

              do k = 1,nlayer(i,j)
                 dummy(i,j,k) = temp(i,j,k)
              enddo

           endif

        enddo
     enddo

     ioerr = nf90_put_var(ncid,tid,dummy,(/ 1, 1, 1, ncinc /),(/ m, n, ilo, 1/))
     if (ioerr/=nf90_noerr) print*,'Error writing Temp var',trim(nf90_strerror(ioerr))

     ! Put fluxes

     ioerr = nf90_put_var(ncid,fxid,flux,(/ 1, 1, ncinc /),(/ m, n, 1 /))
     if (ioerr/=nf90_noerr) print*,'Error writing Flux var',trim(nf90_strerror(ioerr))

     ioerr = nf90_put_var(ncid,fyid,fluy,(/ 1, 1, ncinc /),(/ m, n, 1 /))
     if (ioerr/=nf90_noerr) print*,'Error writing Fluy var',trim(nf90_strerror(ioerr))


  case (2) ! Close

     ioerr = nf90_close(ncid)

  end select


  return

end subroutine output_netcdf

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Fonction for open a netCDF file 

subroutine get_information(ncid)

        integer :: ncid, ndims, nvars, nglobalatts, unlimdimid
        integer :: ioerr

        ioerr = nf90_inquire(ncid, ndims, nvars, nglobalatts, unlimdimid)
        if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop ' Problem getting information from NetCDF physical input file'
        endif

end subroutine 

     subroutine open_netcdf_file(file_way)
  
      integer :: ncid, fileid, ndims, nvars, nglobalatts, unlimdimid
      integer :: ioerr
      character(len=100) :: file_way

      !print*, 'file_way', file_way 

      ioerr = nf90_open(trim(file_way),nf90_nowrite,fileid)
      if(ioerr/=nf90_noerr) then
        print*, trim(nf90_strerror(ioerr))
        stop "!!! Problem opening NetCDF physical input file"
      endif
      
      ncid = fileid

      !ioerr = nf90_inquire(fileid, ndims, nvars, nglobalatts, unlimdimid)
      !if(ioerr/=nf90_noerr) then
      !  print*, trim(nf90_strerror(ioerr))
      !  stop "!!! Problem getting information from NetCDF physical input file"
      !endif

      end subroutine

end module netcdf_gestion 
