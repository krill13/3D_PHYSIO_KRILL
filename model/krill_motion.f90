! krill_motion_mod.f90

module krill_motion_mod

  use class_krill
  use listKrill_mod
  use netcdf
  use part_def
  use ran_mod
#ifdef PAR
  use omp_lib
#endif

  implicit none 

contains
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real function forcing_value(forcing_type, x_grid, y_grid, z_grid)

    character(len=15), intent(in) :: forcing_type
    integer, intent(in) :: x_grid, y_grid, z_grid

    if(forcing_type == 'temperature') then

        forcing_value = temp(x_grid, y_grid, z_grid)

    endif

    return

  end function forcing_value
                                 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real function zmig(species, surf)

    real, intent(in) :: surf 

    integer, intent(in) :: species 


    if (species == 0) then 

       zmig = 10.

    elseif (species == 1) then 

       zmig = 7.53  * max(24.,surf) - 64.2   ! M. norvegica

    elseif (species == 2) then 

       zmig = 11.43 * max(24.,surf) - 200.87 ! T. raschii. New accoustic-based relationship. Limit at 24 PSU 

    endif

    return

  end function zmig
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  real function OU(damp, mu, sigma2, dt, var)

    ! OU(damp,mean,sigma2,dt,var) returns a value following an Ornstein-Uhlenbeck
    ! stochastic process.
    !
    ! Input:
    !   damp(>0)  : rate of mean reversion
    !   mu        : long-term mean of the process
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !   sigma2(>0) : volatility or average magnitude of the random fluctuations 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !               modelled as Brownian motions
    !   dt        : time-step
    !   var       : value of the variable at time t
    !
    ! Output      : var updated at time t+dt
    !
    ! http://planetmath.org/encyclopedia/OrnsteinUhlenbeckProcess.html
    !
    ! MapsF 2011

    integer, intent(in) :: dt

    real,intent(in) :: damp, mu, sigma2, var

    real :: mean, sd


    mean = mu+(var-mu)*exp(-damp*dt)

    sd   = sqrt( (0.5*sigma2/damp)*(1-exp(-2*damp*dt)) )

    OU   = normal(mean,sd)

    return

  end function OU

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real function vert_mig(z, zmig, bottom, dvm, opt_depth)

    ! Return depth of particle according to a diel cycle
    ! Includes a random component (Ornstein-Uhlenbeck distribution)
    !
    ! Input:
    !   z        = depth of the particule at the previous timestep
    !   zmig     = target depth of the particule during daytime
    !   bottom   = bottom depth
    !   dvm      = particle migrate (.true.) or not (.false.)
    !
    ! Output:
    !   vert_mig = vertical position of the particle
    !
    ! MapsF 2011

    real, intent(in) :: z, zmig, bottom, opt_depth

    logical, intent(in) :: dvm

!!! See OU.f90 function for description of the following 2 parameters

    real, parameter :: damp=0.0005 ! -> swimming speed

    real :: sigma2                 ! -> spread (SD): 0.1->SD~10m | 0.25->SD~15m 

    real :: zopt, sd


    if (light_day) then ! Daytime

       zopt = min(bottom, zmig)   ! Function of salinity for migrating particles

    else

       if (dvm) then
         ! zopt = min(bottom, 10.) ! Forage close to the surface at nightime
          zopt = min(bottom, opt_depth)
       else
          zopt = min(bottom, zmig)
       endif

    endif

    ! SD of the vertical distribution is function of zopt & bottom
    ! For a diff zopt-bottom=100m -> sd = sd/3 = 5m

    sd = 10.*50./(50+max(0.,zopt-bottom))

    ! change sigma2 accordingly

    sigma2 = 2*damp*sd**2/(1-exp(-2*damp*dt))

    ! no particles above the surface | below bottom

    vert_mig = max(0.001, bottom-abs(bottom-abs(OU(damp,zopt,sigma2,dt,z))))

    return

  end function vert_mig

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real function update(A3d, nm, nn, nl, index, xp, yp, zp)

    !---------------------------------------------------------------------
    !
    ! This routine interpolates linearly 3D array at a given point
    ! (x,y,z) where (x,y) are in grid units (i,k) and z is in meter
    ! (positive downward to keep the depths positives).
    !
    !---------------------------------------------------------------------
    !
    ! index can be u,v,w or s(sigmat). One can use s for temperature and 
    ! salinity. 
    !
    !---------------------------------------------------------------------

    integer,intent(in) :: nm, nn, nl

    integer :: i, k, j, j1, j2, ii, kk

    real,intent(in) :: xp, yp, zp, A3d(nm,nn,nl)

    real :: dx, dy, d_z, thick, val1, val2

    character(len=1),intent(in) :: index


    ! Find local coordinates (dx,dy)

    if(index=='w'.or.index=='s') then

       ii = min(max(int(xp+0.5),1),nm-1)

       kk = min(max(int(yp+0.5),1),nn-1)

       dx = xp-(float(ii)-0.5)

       dy = yp-(float(kk)-0.5)

    elseif (index=='u') then

       ii = min(max(int(xp),1),nm-1)

       kk = min(max(int(yp+0.5),1),nn-1)

       dx = xp-float(ii)

       dy = yp-(float(kk)-.5)

    elseif (index=='v') then

       ii = min(max(int(xp+0.5),1),nm-1)

       kk = min(max(int(yp),1),nn-1)

       dx = xp-(float(ii)-.5)

       dy = yp-float(kk)

    else

       stop 'Index problems when calling value'

    endif

    update = 0.0

    d_z    = 0.0

    j1     = 1

    j2     = 1

    ! Find local vertical coordinate
    i = min(max(int(xp)+1,1),nm)

    k = min(max(int(yp)+1,1),nn)

    if(nlayer(i,k)==0) then ! below the bottom or value = 0.
       return

    elseif(zp>dz(nlayer(i,k))) then

       return

    endif

    if(index=='w') then ! Fields at the top of each layer(ex: w)

       if(zp<=0.0) then ! If above the surface

          d_z = 0.

          j1  = 1

          j2  = 1

       else

          loop1 : do j = 1,nlayer(i,k) ! Find the bracketing layers

             if(zp>up_depth(j).and.zp<=dz(j)) then

                d_z = (zp-up_depth(j))/(dz(j)-up_depth(j)) ! Thickness fraction

                j1  = j 

                j2  = j+1

                exit loop1  ! Exit the loop to save time

             endif

          enddo loop1

          if(j1==nlayer(i,k)) then

             j2  = j1 ! Constante value in the bottom layer

             d_z = 0.

          endif

       endif

       ! Fields at the center of each layer (ex:u)

    elseif(index=='u'.or.index=='v'.or.index=='s') then

       ! Constant value if above middle of top layer

       if(zp<=mid_depth(1)) then

          d_z = 0.

          j1  = 1

          j2  = 1

          ! Constant value if if below the middle of bottom layer

       elseif (nlayer(i,k)>0.and.zp>mid_depth(nlayer(i,k))) then 

          d_z =  0.

          j1  = nlayer(i,k)

          j2  = nlayer(i,k)

       else 

          loop2 : do j = 1,nlayer(i,k)-1 ! Find the bracketing layers

             if(zp>mid_depth(j).and.zp<=mid_depth(j+1)) then

                thick = (dz(j)-up_depth(j))*0.5+(dz(j+1)-up_depth(j+1))*0.5

                d_z   = (zp-mid_depth(j))/thick ! Thickness fraction

                j1    = j 

                j2    = j+1

                exit loop2

             endif

          enddo loop2

       endif

    endif

    !--- Horizontal interpolation (bilinear)

    !    First horizontal plan

    val1 = dx *     dy * A3d(ii+1,kk+1,j1) + (1.-dx) *     dy * A3d(ii,kk+1,j1) + &
         dx * (1.-dy)* A3d(ii+1,kk,j1)   + (1.-dx) * (1.-dy)* A3d(ii,kk,j1)

    !    Second horizontal plan

    val2 = dx *    dy * A3d(ii+1,kk+1,j2) + (1.-dx) *     dy * A3d(ii,kk+1,j2) + &
         dx *(1.-dy)* A3d(ii+1,kk,j2)   + (1.-dx) * (1.-dy)* A3d(ii,kk,j2)

    !    Vertical interpolation (linear)

    update =  d_z*val2 + (1.-d_z)*val1

    return

  end function update

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real function update2d(A2d,nm,nn,index,xp,yp)

    !---------------------------------------------------------------------
    !
    ! This routine interpolates linearly 2D arraye at a given point
    ! where (x,y) are in grid units (i,k)
    !
    !---------------------------------------------------------------------
    !
    ! index can be u,v,w or s(sigmat). One can use s for temperature and 
    ! salinity. Use w for eta.
    !
    !---------------------------------------------------------------------

    integer,intent(in) :: nm,nn

    integer :: j,j1,j2,ii,kk

    real,intent(in) :: xp,yp,A2d(nm,nn)

    real :: dx,dy,thick,val1,val2

    character(len=1),intent(in) :: index


    ! Find local coordinates (dx,dy)
    if(index=='w'.or.index=='s') then

       ii = min(max(int(xp+0.5),1),nm-1)

       kk = min(max(int(yp+0.5),1),nn-1)

       dx = xp-(float(ii)-.5)

       dy = yp-(float(kk)-.5)

    elseif (index=='u') then

       ii = min(max(int(xp),1),nm-1)

       kk = min(max(int(yp+0.5),1),nn-1)

       dx = xp-float(ii)

       dy = yp-(float(kk)-.5)

    elseif (index=='v') then

       ii = min(max(int(xp+0.5),1),nm-1)

       kk = min(max(int(yp),1),nn-1)

       dx = xp-(float(ii)-.5)

       dy = yp-float(kk)

    endif

    update2d = 0.0

    !--- Horizontal interpolation (bilinear)

    val1 = dx *    dy * A2d(ii+1,kk+1) + (1.-dx) *     dy * A2d(ii,kk+1) + &
         dx *(1.-dy)* A2d(ii+1,kk)   + (1.-dx) * (1.-dy)* A2d(ii,kk)

    update2d = val1

    return

  end function update2d


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine optimal_depth(mylist)

    class(listKrill) :: mylist
    class(*), pointer :: curr

    integer :: i, k
    
    real :: tp, diat, flag, micz, mesz, dp, opt_depth
    real, dimension(5) :: balance
    real, dimension(1) :: opt_depth_2

    ! Pointer array declaration for parallelization
    type parray
       class(Krill), pointer :: ptr
    end type parray

    type(parray), allocatable :: ptrarray(:)

    integer :: nptr, thrid, thrs, j

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

    do j = 1, nptr

       xpo = ptrarray(j)%ptr%get_xpo()
       ypo = ptrarray(j)%ptr%get_ypo()

       do k = 1, 5
        dp = mid_depth(k)
         
        tp   = update(temp,     m, n, ilo, 's', xpo, ypo, dp)
        diat = update(diatom,   m, n, ilo, 's', xpo, ypo, dp)
        flag = update(flagel,   m, n, ilo, 's', xpo, ypo, dp)
        micz = update(microzoo, m, n, ilo, 's', xpo, ypo, dp)
        mesz = update(mesozoo,  m, n, ilo, 's', xpo, ypo, dp)
   
        call ptrarray(j)%ptr%set_tp(tp)
        call ptrarray(j)%ptr%set_diat(diat)
        call ptrarray(j)%ptr%set_flag(flag)
        call ptrarray(j)%ptr%set_micz(micz)
        call ptrarray(j)%ptr%set_mesz(mesz) 
        
        call ptrarray(j)%ptr%balance()

        balance(k) = ptrarray(j)%ptr%get_balance_ind()

       end do
       
       opt_depth_2 = maxloc(balance)
       opt_depth = opt_depth_2(1)
       opt_depth = mid_depth(opt_depth)

       call ptrarray(j)%ptr%set_opt_depth(opt_depth)

    end do
end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine trajectory(mylist)

    class(listKrill) :: mylist
    class(*), pointer :: curr

    integer :: it, i, k, ii, kk, dt2

    integer :: kstep_diff, istart, isteps

    real :: up, vp, wp

    real :: xp_temp, yp_temp, zp_temp

    real :: kdiff = 20 !m2/s; ramdom walk

    real :: xdiffusion, ydiffusion

    real :: dayhour

    real :: xpo,ypo, zpo, zz, &
            xk1, xk2, xk3, xk4, &
            yk1, yk2, yk3, yk4, &
            zk1, zk2, zk3, zk4, opt_depth 

    logical :: light_now, dvm, cell_out

    ! Pointer array declaration for parallelization
    type parray
       class(Krill), pointer :: ptr
    end type parray

    type(parray), allocatable :: ptrarray(:)

    integer :: nptr, thrid, thrs, j


    !---------------------------------------------------------------------

    ! Compute hour of day (hh.hhh)
    dayhour = time(3)+time(2)/60
 
    ! Identify daytime | nighttime
    if (dayhour>=time_sunrise.and.dayhour<=time_sunset) then

       light_now = .true. ! day

    else

       light_now = .false. ! night

    endif

    if (light_now.eqv.light_day) then ! Fancy output

       write(*,'($a1)') '-'

    else

       if (light_now) then

#ifdef BACK
          write(*,'($a1)') 'o'
#else
          write(*,'($a1)') '*'
#endif

    ! M. norvegica synchronous development and migrations
    !if (mod(kstep,14*86400/dt)<86400/dt) then
    !   part_dvm = .false.
    !elseif (mod(kstep,14*86400/dt)<3*86400/dt) then
    !   part_dvm = .true.
    !endif

       else

#ifdef BACK
          write(*,'($a1)') '*'
#else
          write(*,'($a1)') 'o'
#endif

    !if (mod(kstep,14*86400/dt)<86400/dt) then
    !   part_dvm = .true.
    !elseif (mod(kstep,14*86400/dt)<2*86400/dt) then
    !   part_dvm = .false.
    !endif

       endif

    ! T. raschii asynchronous development and migrations
    !do part = 1,npart ! Determine now which particle will migrate|stay
    !   part_dvm(part) = ran1()>1./14.
    !enddo

    endif

    !---------------------------------------------------------------------
 
    dt2 = 2.*dt

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
!$OMP PARALLEL DO &
!$OMP PRIVATE(xpo,ypo,zpo,xk1,xk2,xk3,xk4,yk1,yk2,yk3,yk4,zk1,zk2,zk3,zk4) &
!$OMP PRIVATE(up,vp,wp,xp_temp,yp_temp,zp_temp,xdiffusion,ydiffusion) &
!$OMP PRIVATE(dvm,zz,istart,isteps,cell_out,i,k,kstep_diff) &
!$OMP PRIVATE(j,thrid,thrs) &
!$OMP SHARED(nptr,ptrarray)
#else
!$OMP PARALLEL DO &
!$OMP PRIVATE(xpo,ypo,zpo,xk1,xk2,xk3,xk4,yk1,yk2,yk3,yk4,zk1,zk2,zk3,zk4) &
!$OMP PRIVATE(up,vp,wp,xp_temp,yp_temp,zp_temp,xdiffusion,ydiffusion) &
!$OMP PRIVATE(dvm,zz,istart,isteps,cell_out,i,k,kstep_diff) &
!$OMP PRIVATE(j) &
!$OMP SHARED(nptr,ptrarray)
#endif
#endif

    do j = 1, nptr

#ifdef PAR
#ifdef DEBUG
       thrs  = omp_get_num_threads()
       thrid = omp_get_thread_num()
       print*, 'Number of threads = ', thrs, &
               ';   Thread ID = ', thrid, &
               ';   DO loop index = ', j
#endif
#endif


       xpo = ptrarray(j)%ptr%get_xpo()
       ypo = ptrarray(j)%ptr%get_ypo()
       xk1 = ptrarray(j)%ptr%get_xk1()
       xk2 = ptrarray(j)%ptr%get_xk2()
       xk3 = ptrarray(j)%ptr%get_xk3()
       xk4 = ptrarray(j)%ptr%get_xk4()
       yk1 = ptrarray(j)%ptr%get_yk1()
       yk2 = ptrarray(j)%ptr%get_yk2()
       yk3 = ptrarray(j)%ptr%get_yk3()
       yk4 = ptrarray(j)%ptr%get_yk4()       
       zpo = ptrarray(j)%ptr%get_zpo()
       zk1 = ptrarray(j)%ptr%get_zk1()
       zk2 = ptrarray(j)%ptr%get_zk2()
       zk3 = ptrarray(j)%ptr%get_zk3()
       zk4 = ptrarray(j)%ptr%get_zk4()

       dvm = ptrarray(j)%ptr%get_dvm()
       zz  = ptrarray(j)%ptr%get_zmig()
       opt_depth = ptrarray(j)%ptr%get_opt_depth()

       istart = ptrarray(j)%ptr%get_istart()
       isteps = ptrarray(j)%ptr%get_isteps()


       i = int(xpo) + 1
       if (i > m-1 .or. i < 1) then 
          cell_out = .true.
          call ptrarray(j)%ptr%set_cell_out(cell_out)
       end if

       k = int(ypo) + 1
       if (k > n-1 .or. k < 1) then 
          cell_out = .true.
          call ptrarray(j)%ptr%set_cell_out(cell_out)
       end if
          
       if (ptrarray(j)%ptr%get_cell_out() .eqv. .false.) then 

          i = min(m, max(1,i))
          k = min(n, max(1,k))

          kstep_diff = kstep - istart

          !----------------------------------------------------------------
          ! Vertical distribution updated at each inner timestep

          if (kstep_diff == 0) then 

             ! Initialisation ok k1s
#ifdef ADV
             up =  update(u, m, n, ilo, 'u', xpo, ypo, zpo)
             vp =  update(v, m, n, ilo, 'v', xpo, ypo, zpo)
#endif
             wp = -update(w, m, n, ilo, 'w', xpo, ypo, zpo)

#ifdef ADV
             xk1 = dt2 * up / dlx (i,k) 
             call ptrarray(j)%ptr%set_xk1(xk1)

             yk1 = dt2 * vp / dlx (i,k)
             call ptrarray(j)%ptr%set_yk1(yk1)
#endif
             zk1 = dt2 * wp
             call ptrarray(j)%ptr%set_zk1(zk1)

          elseif (kstep_diff>0.and.kstep_diff<=isteps) then

             if (nlayer(i,k) > 0) then

                zz = zmig(part_spec, salt2(i,k))
                call ptrarray(j)%ptr%set_zmig(zz)

                zpo = vert_mig(zpo, zz, mid_depth(nlayer(i,k)), dvm, opt_depth)
             endif

             if (mod(kstep_diff,2)==0) then 
                ! Avanced time : computation of k4s and  the new positions, then k1s 

#ifdef ADV
                up =  update(u, m, n, ilo, 'u', xpo + xk3, ypo + yk3, zpo + zk3)
                vp =  update(v, m, n, ilo, 'v', xpo + xk3, ypo + yk3, zpo + zk3)
#endif
                wp = -update(w, m, n, ilo, 'w', xpo + xk3, ypo + yk3, zpo + zk3)
#ifdef ADV
                xk4 = dt2 * up / dlx (i,k) 
                call ptrarray(j)%ptr%set_xk4(xk4)

                yk4 = dt2 * vp / dlx (i,k)
                call ptrarray(j)%ptr%set_yk4(yk4)
#endif
                zk4 = dt2 * wp
                call ptrarray(j)%ptr%set_zk4(zk4)
#ifdef ADV
                xp_temp = xpo + (xk1+2.*xk2+2*xk3+xk4)/6.

                ! Random walk

                xdiffusion = normal(0.,1.)/dlx(i,k)*(2.*kdiff*dt2)**.5 ! normal() instead of Gasdev3()

                xp_temp = xp_temp+xdiffusion
                yp_temp = ypo + (yk1+2.*yk2+2.*yk3+yk4)/6.


                ydiffusion = normal(0.,1.)/dly(i,k)*(2.*kdiff*dt2)**.5

                yp_temp = yp_temp+ydiffusion
#endif
                zp_temp = zpo + (zk1+2.*zk2+2.*zk3+zk4)/6.

                ! Move wet particles

                ii = int(xp_temp) + 1 ! Destination cell
                kk = int(yp_temp) + 1

                ii = min(m, max(1,ii)) ! Necessary with diffusion to avoid out of bounds
                kk = min(n, max(1,kk)) 

                if(nlayer(ii,kk)>0) then 
                   if(zp_temp<dz(nlayer(ii,kk))) then ! NL
#ifdef ADV
                      xpo = xp_temp
                      call ptrarray(j)%ptr%set_xpo(xpo)

                      ypo = yp_temp
                      call ptrarray(j)%ptr%set_ypo(ypo)
#endif
                      zpo = max(0., zp_temp)
                      call ptrarray(j)%ptr%set_zpo(zpo)

                   endif
                endif
#ifdef ADV
                up  =  update(u, m, n, ilo, 'u', xpo, ypo, zpo)
                vp  =  update(v, m, n, ilo, 'v', xpo, ypo, zpo)
#endif
                ! WARNING : wp = 0
                wp  = -update(w, m, n, ilo, 'w', xpo, ypo, zpo)
#ifdef ADV
                xk1 = dt2 * up / dlx (i,k)
                call ptrarray(j)%ptr%set_xk1(xk1)

                yk1 = dt2 * vp / dlx (i,k)
                call ptrarray(j)%ptr%set_yk1(yk1)
#endif
                zk1 = dt2 * wp
                call ptrarray(j)%ptr%set_zk1(zk1)

             else 
                ! Half time : computation of the k2s and k3s
#ifdef ADV
                up  =  update(u, m, n, ilo, 'u', xpo+0.5*xk1, ypo+0.5*yk1, zpo+0.5*zk1)
                vp  =  update(v, m, n, ilo, 'v', xpo+0.5*xk1, ypo+0.5*yk1, zpo+0.5*zk1)
#endif
                ! WARNING : wp = 0
                wp  = -update(w, m, n, ilo, 'w',xpo+0.5*xk1, ypo+0.5*yk1, zpo+0.5*zk1) 
#ifdef ADV
                xk2 = dt2 * up / dlx (i,k)
                call ptrarray(j)%ptr%set_xk2(xk2)

                yk2 = dt2 * vp / dlx (i,k)
                call ptrarray(j)%ptr%set_yk2(yk2)
#endif
                zk2 = dt2 * wp
                call ptrarray(j)%ptr%set_zk2(zk2)

#ifdef ADV
                up  =  update(u, m, n, ilo, 'u', xpo+0.5*xk2, ypo+0.5*yk2, zpo+0.5*zk2)
                vp  =  update(v, m, n, ilo, 'v', xpo+0.5*xk2, ypo+0.5*yk2, zpo+0.5*zk2)
#endif
                ! WARNING : wp = 0
                wp  = -update(w, m, n, ilo, 'w',xpo+0.5*xk2, ypo+0.5*yk2, zpo+0.5*zk2) 
#ifdef ADV
                xk3 = dt2 * up / dlx (i,k)
                call ptrarray(j)%ptr%set_xk3(xk3)

                yk3 = dt2 * vp / dlx (i,k)
                call ptrarray(j)%ptr%set_yk3(yk3)
#endif
                zk3 = dt2 * wp
                call ptrarray(j)%ptr%set_zk3(zk3)

             endif

          endif

       endif

    enddo

#ifdef PAR
!$OMP END PARALLEL DO
#endif

    deallocate(ptrarray)

    ! Update light_day
    light_day = light_now


  end subroutine trajectory

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module krill_motion_mod
