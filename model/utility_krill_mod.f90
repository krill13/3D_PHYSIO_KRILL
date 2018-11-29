! utility_krill_mod.f90

module utility_krill_mod

  use class_krill
  use listKrill_mod
  use netcdf
  use part_def
  use ran_mod
  use krill_motion_mod
#ifdef PAR
  use omp_lib
#endif

  implicit none 

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine evolveList(mylist)
    class(listKrill) :: mylist
    class(*), pointer :: curr

    ! Pointer array declaration for parallelization
    type parray
       class(Krill), pointer :: ptr
    end type parray

    type(parray), allocatable :: ptrarray(:)

    integer :: i, nptr, thrid, thrs


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
!$OMP PRIVATE(i,thrs,thrid) &
!$OMP SHARED(nptr,ptrarray)
#else
!$OMP PARALLEL DO &
!$OMP PRIVATE(i) &
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

          call ptrarray(i)%ptr%grow()
          call ptrarray(i)%ptr%molt()
          call ptrarray(i)%ptr%reproduction()
  
       end if

    enddo

#ifdef PAR
!$OMP END PARALLEL DO
#endif

    deallocate(ptrarray)

  end subroutine evolveList

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine generateKrill(list, runList)
    type(Krill) :: mylist
    class(listKrill) :: list


    character(*), intent(in) :: runList

    integer :: i, j, ii, k, l, ioerr, t, IOstatus !most of them are used as counter or iterator

    integer, dimension(6) :: part_start, part_end
    
    real, dimension(1) :: max_chlA_2

    real, dimension(ilo) :: phy2 
    
    integer, dimension(max_number_part) :: xgrid, ygrid, zgrid

    integer :: kd_start, kd_end, kd

    real :: r1, r2, opt_depth

    real, dimension(max_number_part) :: zz, xpo, ypo, zpo, getlength, &
                                        tp, diat, flag, micz, mesz, max_chlA

    real, dimension(14,6576) :: pars

    open(10,file = runList, status = 'old', action = 'read', err = 1, iostat = ioerr)
1   if(ioerr /= 0) STOP 'problem opening file'
    read(10, nml = part_nml)
    close(10)

    !---------------------------------------------------------------------
    ! Initial particles distribution (spatial)
    !
    ! Works with input_particles (below) to provide the # of particles (nxy)
    ! added at specific time steps in case of periodic seeding of particles

    open(10,file = partfile, status = 'old', action = 'read', err = 2, iostat = ioerr)
2   if(ioerr /= 0) STOP 'problem opening file, particle input'

    do i = 1, m
       read(10, *) (part_grid_init(i, j), j = 1, n)
    end do
     
#ifdef INITB
    nxy  = nbpb
#else  
    nxy  = sum(part_grid_init) !nxy is the number of cells with particle

    allocate(ipart(nxy), jpart(nxy))

    ii = 0

    do j = 1, n    !n is a horizontal dimension of the grid
       do i = 1, m !m is the other horizontal dimension of the grid
          if(part_grid_init(i, j) == 1) then
             ii = min(ii + 1, nxy)
             ipart(ii) = i
             jpart(ii) = j
          endif
       end do
    end do

    close(10)
#endif

    ! Number of individuals generated
    print*,'Cells with particles = ',nxy,'; generating all the particles in the grid'

    !---------------------------------------------------------------------
    ! Initialization of individual particles

    ! Open namelist that contains physiological parameters  
    open(10, file = 'run.list', status = 'old', action = 'read', err = 3, iostat = ioerr)
3   if(ioerr/=0) stop '!!! Problem opening file physio.list !!!'
    read(10, nml = physio_nml)

    close(10)
    
 
    ! Open matrix of individual parameters
    if(INTRA_SPE .eqv. .true.) then 
    open(10, file=parsfile, status = 'old', action = 'read', err = 15, iostat = ioerr)
15  if (ioerr/=0) stop '!!! Problem opening parameters matrix !!!'

    do i = 1, nb_pars 
        read(10,*) (pars(i,j), j = 1, nxy)
    end do

    print*, pars(:,4)

    close(10)
    
    print*, 'okok'
    end if 
 
    ! Initializing time (time would be tzero + one dt)
    call update2(initial)

    ! Give values of physiological parameters according to the species 
    call par_particle 

    ! Initialize time loop for periodic release of particles:
    ! start of particles release -> end of release (by frequency of release)

    part_start = (/ 0, 0, 0, d_Astart, m_Astart, y_Astart /) ! Start of particles release
    call cday2(part_start(4),part_start(5),part_start(6),kd_start)

    part_end   = (/ 0, 0, 0, d_Aend, m_Aend, y_Aend /)       ! End of particles release
    call cday2(part_end(4),part_end(5),part_end(6),kd_end)

#ifdef DEBUG
    print*, 'part start ', part_start(4), part_start(5), part_start(6), kd_start    
    print*, 'part end   ', part_end(4), part_end(5), part_end(6), kd_end
    print*, 'dtnc', dtnc
#endif

    ! Duration of particles advection; same for all particles
    isteps_part = part_duration * 86400/dt

#ifdef DEBUG                 
    print*, 'part advection duration ', isteps_part
#endif

    ! Periodic initialization
#ifdef BACK
    tpart = ceiling( real(kd_start - kd_end+1)/part_freq)
#else
    tpart = ceiling( real(kd_end-kd_start+1)/part_freq )
#endif    
    print*, 'kd_end', kd_end, 'kd_start', kd_start, 'part_freq', part_freq


#ifdef DEBUG                 
    print*, '# of part seeding events ', tpart
#endif
    
    ! NOW we initialize our krill

    ! FIRST open sequence of repeated initialization...
    do t = 1, tpart

       i = 0

#ifdef INITB
       do k = 1, nxy         ! Cell position
             i = i + 1
                xpo(i) = xpoback(i,1,nb_back_time)
                ypo(i) = ypoback(i,1,nb_back_time)
                zpo(i) = zpoback(i,1,nb_back_time)

                xgrid(i) = max(1, min(m, ceiling(xpo(i))))
                ygrid(i) = max(1, min(n, ceiling(ypo(i))))
                zgrid(i) = ceiling(zpo(i)) 

                ! Days from the run start
                kd = kd_start - kd_ini - istart*dtnc/86400
                
                ! Initial particle timestep (kstep); use dt in sec
                istart_part = int( (kd*86400 + part_start(3)*3600 + part_start(2)*60 + part_start(1))/dt )

                tp(i)   = temp(xgrid(i),ygrid(i),zgrid(i))

                diat(i) = diatom(xgrid(i),ygrid(i),zgrid(i))
                flag(i) = flagel(xgrid(i),ygrid(i),zgrid(i))
                micz(i) = microzoo(xgrid(i),ygrid(i),zgrid(i))
                mesz(i) = mesozoo(xgrid(i),ygrid(i),zgrid(i))
                phy2    = diatom(xgrid(i),ygrid(i),:) + flagel(xgrid(i),ygrid(i),:) 
                max_chlA_2 = maxval(phy2)
                max_chlA(i) = max_chlA_2(1)

                if (clone .eqv. .true.) then 
                   getlength(i) = mean_length
                else 
                   getlength(i) = spread(min_length, max_length)
                end if

                call mylist%init_krill(dtnc, istart_part, isteps_part,  sex, part_spec, getlength(i), mature, &
                     xpo(i), ypo(i), zpo(i), zz(i), xk1, xk2, xk3, xk4, yk1, yk2, yk3, yk4, zk1, zk2, zk3, zk4, & 
                     dvm, cell_out, light_day, aw, bw, a_molt, b_molt, w_molt, ei, er, t_lim, A, k0, k0_phy, k0_zoo, h0, &
                     ratio_ing, r0, rb, a_rep, b_rep, mass_egg, p_gonad, grow_gonad, tp(i),diat(i),flag(i), micz(i), &
                     mesz(i), max_chlA(i), phyto_threshold, prop_mez) 

                call list%addKrill(mylist)
        enddo
print*, 'initback is OK'

#else               
    ! ... then get random positions on the grid...
       do k = 1, nxy         ! Cell position
          do l = 1, part_fac ! Individual particle within cell

             i = i + 1

             if (t==1) then
                !initialize position
4               r1 = ran1() - 0.5
                r2 = ran1() - 0.5
                
                xpo(i) = real(ipart(k)) - 0.5 + r1
                ypo(i) = real(jpart(k)) - 0.5 + r2
                
                xgrid(i) = max(1, min(m, ceiling(xpo(i))))
                ygrid(i) = max(1, min(n, ceiling(ypo(i))))
                
                do while (nlayer(xgrid(i), ygrid(i)) == 0)
                   goto 4 ! check that the particle is in a wet cell
                end do
                
                opt_depth = 10.
                zz(i)  = zmig(part_spec, salt2(xgrid(i),ygrid(i)))
                zpo(i) = 15.
                zpo(i) = vert_mig(zpo(i), zz(i), mid_depth(nlayer(xgrid(i), ygrid(i))), dvm, opt_depth)

                zloop: do j = 1, min( 20, nlayer(xgrid(i),ygrid(i)) )
                   zgrid(i) = j
                   if (zpo(i)<=dz(j)) exit zloop
                enddo zloop

#ifdef BACK
                ! Days from the run start
                kd = kd_ini + istart*dtnc/86400 - (kd_start+(t-1)*part_freq)
                
                ! Initial particle timestep (kstep); use dt in sec
                istart_part = int( (kd*86400 - part_start(3)*3600 - part_start(2)*60 - part_start(1))/dt )
#else
                ! Days from the run start
                kd = kd_start - kd_ini - istart*dtnc/86400
                
                ! Initial particle timestep (kstep); use dt in sec
                istart_part = int( (kd*86400 + part_start(3)*3600 + part_start(2)*60 + part_start(1))/dt )
#endif
                tp(i)   = temp(xgrid(i),ygrid(i),zgrid(i))

#ifdef PHYSIO                
                diat(i) = diatom(xgrid(i),ygrid(i),zgrid(i))
                flag(i) = flagel(xgrid(i),ygrid(i),zgrid(i))
                micz(i) = microzoo(xgrid(i),ygrid(i),zgrid(i))
                mesz(i) = mesozoo(xgrid(i),ygrid(i),zgrid(i))
                phy2    = diatom(xgrid(i),ygrid(i),:) + flagel(xgrid(i),ygrid(i),:) 
                max_chlA_2 = maxval(phy2)
                max_chlA(i) = max_chlA_2(1)

                if (clone .eqv. .true.) then 
                   getlength(i) = mean_length
                else 
                   getlength(i) = spread(min_length, max_length)
                end if


                if(INTRA_SPE .eqv. .true.) then 
                call mylist%init_krill(dtnc, istart_part, isteps_part,  sex, part_spec, getlength(i), mature, &
                     xpo(i), ypo(i), zpo(i), zz(i), xk1, xk2, xk3, xk4, yk1, yk2, yk3, yk4, zk1, zk2, zk3, zk4, & 
                     dvm, cell_out, light_day, pars(13,k), pars(14,k), pars(1,k), pars(2,k), w_molt, pars(6,k), pars(9,k), &
                     t_lim, A, k0, pars(3,k)/3600, pars(4,k)/3600, pars(5,k)*3600 , ratio_ing, pars(7,k)/3600, pars(8,k), & 
                     pars(10,k), pars(11,k), mass_egg, p_gonad, pars(12,k), tp(i),diat(i),flag(i), micz(i), &
                     mesz(i),max_chlA(i), phyto_threshold, prop_mez) 

                call list%addKrill(mylist)
                else
                call mylist%init_krill(dtnc, istart_part, isteps_part,  sex, part_spec, getlength(i), mature, &
                     xpo(i), ypo(i), zpo(i), zz(i), xk1, xk2, xk3, xk4, yk1, yk2, yk3, yk4, zk1, zk2, zk3, zk4, & 
                     dvm, cell_out, light_day, aw, bw, a_molt, b_molt, w_molt, ei, er, t_lim, A, k0, k0_phy, k0_zoo, h0, &
                     ratio_ing, r0, rb, a_rep, b_rep, mass_egg, p_gonad, grow_gonad, tp(i),diat(i),flag(i), micz(i), &
                     mesz(i),max_chlA(i), phyto_threshold, prop_mez) 

                call list%addKrill(mylist)
                end if 

    ! ... and finally, duplicate particles for each sequence of initialization.
             else

                call mylist%init_krill(dtnc, istart_part, isteps_part,  sex, part_spec, getlength(i), mature, &
                     xpo(i), ypo(i), zpo(i), zz(i), xk1, xk2, xk3, xk4, yk1, yk2, yk3, yk4, zk1, zk2, zk3, zk4, & 
                     dvm, cell_out, light_day, aw, bw, a_molt, b_molt, w_molt, ei, er, t_lim, A, k0, k0_phy, k0_zoo, h0, &
                     ratio_ing, r0, rb, a_rep, b_rep, mass_egg, p_gonad, grow_gonad, tp(i),diat(i),flag(i), micz(i), &
                     mesz(i),max_chlA(i), phyto_threshold, prop_mez) 

                call list%addKrill(mylist)

             endif
#else

                call mylist%init_krill(dtnc, istart_part, isteps_part,  sex, part_spec, &
                     xpo(i), ypo(i), zpo(i), zz(i), xk1, xk2, xk3, xk4, yk1, yk2, yk3, yk4, zk1, zk2, zk3, zk4, & 
                     dvm, cell_out, light_day) 

                call list%addKrill(mylist)

    ! ... and finally, duplicate particles for each sequence of initialization.
             else

                call mylist%init_krill(dtnc, istart_part, isteps_part,  sex, part_spec, &
                     xpo(i), ypo(i), zpo(i), zz(i), xk1, xk2, xk3, xk4, yk1, yk2, yk3, yk4, zk1, zk2, zk3, zk4, & 
                     dvm, cell_out, light_day) 

                call list%addKrill(mylist)

             endif
#endif 
          end do
       end do
#endif
    end do

#ifdef INITB
print*, 'initback is OK', 'nxy', nxy
    npart = nxy
#else
    npart = nxy * part_fac
#endif

    print*,'Generated ',npart,' particles'
    print*,'Sequences of initialization = ',tpart

    !---------------------------------------------------------------------
    ! Compute initial particles 3D density

    density_part = 0

  end subroutine generateKrill

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module utility_krill_mod
