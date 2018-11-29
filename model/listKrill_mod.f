# 1 "listKrill_mod.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 1 "<command-line>" 2
# 1 "listKrill_mod.f90"
!! listKrill_mod.f90

module listKrill_mod

  use class_krill
  use abstList_mod
  use netcdf

  private
  public :: listKrill
  type, extends(list) :: listKrill


contains

  ! Functions
  procedure :: n_krill
  procedure, public :: current => currentKrill ! get krill pointed by iterator
  !procedure, public :: getkrill

  ! Subroutines
  procedure, public :: addKrill ! add krill in list
  procedure, public :: printList => printListKrill ! print the attributes of all krills in list
  procedure :: thisKrill ! allow acces to a specific krill
  procedure, public :: netcdfKrill ! generate a netcdf file for list of krills "much fun! so many Krills!"

  end type listKrill

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer function n_krill(this)
    class(listKrill) :: this
    integer :: n

    n = 0

    call this%reset()
    do while(this%moreValues())
       n = n + 1
       call this%next()
    end do

    n_krill = n
  end function n_krill

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine thisKrill(this, krillPosition)
    class(listKrill) :: this
    integer :: krillPosition

    call this%reset()
    do i = 1, (krillPosition - 1)
       call this%next()
    end do
    !print*, krillPosition
  end subroutine thisKrill

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine printListKrill(this)
    class(listKrill) :: this
    class(*), pointer :: curr

    call this%reset()

    do while(this%moreValues())
       curr => this%currentValue()

       select type(curr)
       type is (Krill)
          call curr%debug()
       end select

       call this%next()
    end do

    call this%reset()
  end subroutine printListKrill

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine addKrill(this, value)
    class(listKrill) :: this
    class(Krill) :: value
    class(*), allocatable :: v

    allocate(v, source = value)
    call this%addValue(v)
  end subroutine addKrill

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function currentKrill(this)
    type(Krill) :: currentKrill
    class(listKrill) :: this
    class(*), pointer :: v

    v => this%currentValue()
    select type(v)
    type is (Krill)
       currentKrill = v
    end select
  end function currentKrill

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine netcdfKrill(this, action)

    ! Output particles trajectories in netcdf format
    ! Argument :
    ! action = 0 => initialization
    ! action = 1 => Record data
    ! action = 2 => Finalization

    use netcdf

    use part_def

    implicit none

    integer, intent(in) :: action

    class(listKrill) :: this
    class(*), pointer :: curr

    ! variables for the netcdf procedures

    character(100) :: filename_nc, date_print

    integer :: ierr

    integer :: i, j, nb

    integer, save :: xdimid, ydimid, zdimid, timedimid

    integer, save :: file_id, attributesKrillid, speciesid, sexid, lengthid, massid, &
         dev_freqid, molt_lengthid, awid, bwid, eiid, a_moltid, b_moltid, k0_phyid, k0_zooid, h0id, &
         Aid, r0id, w_moltid, a_repid, b_repid, mass_eggid, ingest_id, breath_id, arr_id, enc_phy_id, enc_zoo_id, &
         hand_id, timeid, xpoid, ypoid, zpoid, dummyid, lonid, latid, ncinc, nctlen, &
         tpid, diatid, flagid, miczid, meszid, nb_moltid, grow_cumid, egg_costid, nb_eggid, &
         gonadid, max_gonadid, ingest_phyid, ingest_zooid, grow_wayid, rbid, p_gonadid, grow_gonadid

    character(len=3), dimension(12) :: month_nc = (/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/)


    ! integer, dimension(2) :: matrixDim

    ! buffer arrays to allocate values to netcdf variables

    integer, dimension(:), allocatable :: all_species
    integer, dimension(:), allocatable :: all_sex

    !real, dimension(1), save :: nctime
    real, save :: ncfreq, nctime

    real, dimension(:), allocatable :: all_xpo
    real, dimension(:), allocatable :: all_lon
    real, dimension(:), allocatable :: all_ypo
    real, dimension(:), allocatable :: all_lat
    real, dimension(:), allocatable :: all_zpo
# 208 "listKrill_mod.f90"
    select case (action)

    case (0)

       ! First check that the # of krill in the list is correct
       nb = this%n_krill()
       if (nb/=npart*tpart) then
          print*, 'ERROR: krill particles number not correct !'
          print*, 'nb = ', nb, ' npart = ', npart, ' tpart = ', tpart
          stop
       endif

       ! Creation of the netcdf dataset
       print *, "Creation of netcdf file "
       filename_nc = trim(outfile)





       ierr = nf90_create(trim(filename_nc), nf90_clobber, file_id)
       if (ierr/=nf90_noerr) print*, 'ERROR NETCDF CREATION ', trim(nf90_strerror(ierr))

       ! Dimensions

       ! X axis here is for each particles
       ierr = nf90_def_dim(file_id, "krill", npart, xdimid)
       if (ierr/=nf90_noerr) print*, 'ERROR KRILL DIMENSION DEFINITION', trim(nf90_strerror(ierr))

       ! Y axis here is for each sequence of particles (initialized at a given period)
       ierr = nf90_def_dim(file_id, "sequence", tpart, ydimid)
       if (ierr/=nf90_noerr) print*, 'ERROR SEQUENCE DIMENSION DEFINITION', trim(nf90_strerror(ierr))

       ! Z axis here is for the 4 values of "pref" only
       !ierr = nf90_def_dim(file_id, "pref length", 4, zdimid)
       !if (ierr/=nf90_noerr) print*, 'ERROR PREF LENGTH DIMENSION DEFINTION', trim(nf90_strerror(ierr))

       ! output_freq not always = input_freq
       nctlen = final/output_freq

       print*, 'nctlen', nctlen

       ! Time axis (unlimited)
       ierr = nf90_def_dim(file_id, 'time', nf90_unlimited, timedimid)
       if (ierr/=nf90_noerr) print*, 'ERROR TIME DIMENSION DEFINITION', trim(nf90_strerror(ierr))

       ! creation of the variables for the files

       ierr = nf90_def_var(file_id, "species", nf90_int, (/xdimid, ydimid/), speciesid)
       if (ierr/=nf90_noerr) print*, 'ERROR SPECIES VARIABLE DEFINITION ', nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "sex", nf90_int, (/xdimid, ydimid/), sexid)
       if (ierr/=nf90_noerr) print*, 'ERROR SEX VARIABLE DEFINITION ', nf90_strerror(ierr)
# 314 "listKrill_mod.f90"
       !ierr = nf90_def_var(file_id, "pref", nf90_float, (/xdimid, ydimid, zdimid/), prefid)
       !if (ierr/=nf90_noerr) print*, 'ERROR PREF VARIABLE DEFINITION ', nf90_strerror(ierr)

       ! Time-dependent variables

       ierr = nf90_def_var(file_id, "time", nf90_float, (/timedimid/), timeid)
       if (ierr/=nf90_noerr) print*, "ERROR TIME VARIABLE DEFINITION ", nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "lon", nf90_float, (/xdimid, ydimid, timedimid/), lonid)
       if (ierr/=nf90_noerr) print*, 'ERROR LON VARIABLE DEFINITION ', trim(nf90_strerror(ierr))

       ierr = nf90_def_var(file_id, "lat", nf90_float, (/xdimid, ydimid, timedimid/), latid)
       if (ierr/=nf90_noerr) print*, 'ERROR LAT VARIABLE DEFINITION ', trim(nf90_strerror(ierr))

       ierr = nf90_def_var(file_id, "xpo", nf90_float, (/xdimid, ydimid, timedimid/), xpoid)
       if (ierr/=nf90_noerr) print*, "ERROR XPO VARIABLE DEFINITION ", nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "ypo", nf90_float, (/xdimid, ydimid, timedimid/), ypoid)
       if (ierr/=nf90_noerr) print*, "ERROR YPO VARIABLE DEFINITION ", nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "zpo", nf90_float, (/xdimid, ydimid, timedimid/), zpoid)
       if (ierr/=nf90_noerr) print*, "ERROR ZPO VARIABLE DEFINITION ", nf90_strerror(ierr)
# 410 "listKrill_mod.f90"
       ! Attributes

       ierr = nf90_put_att(file_id, NF90_GLOBAL, "File_name", filename_nc)
       if (ierr/=nf90_noerr) print*, 'Error defining FileName att ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, NF90_GLOBAL, "Description", 'D. Benkort Ph.D. experiments')
       if (ierr/=nf90_noerr) print*, 'Error defining Description att ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, NF90_GLOBAL, "Version", '2.0')
       if (ierr/=nf90_noerr) print*, 'Error defining Version att ', trim(nf90_strerror(ierr))

       ! JJ MM AAAA -- HHhMMmSSs
       write(date_print, '(i3,i3,i5,a,3(i2,a1))'), today(2), today(1), today(3), ' - ', now(1), &
            'h', now(2), 'm', now(3), 's'

       ierr = nf90_put_att(file_id, NF90_GLOBAL, "Date", trim(date_print))
       if (ierr/=nf90_noerr) print*, 'Error defining Date att ', trim(nf90_strerror(ierr))


       ierr = nf90_put_att(file_id, timeid, "Long_name", 'Days')
       if (ierr/=nf90_noerr) print*, 'Error defining Long_name att for T var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, timeid, "Units", trim(tnc_units))
       if (ierr/=nf90_noerr) print*,'Error defining Units att for T var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,timeid, "time_origin", trim(tnc_origin_out))
       if (ierr/=nf90_noerr) print*,'Error defining time_origin att for T var ', trim(nf90_strerror(ierr))


       ierr = nf90_put_att(file_id, xpoid, "Long_name", 'Zonal index')
       if (ierr/=nf90_noerr) print*, 'Error defining Long_name att for XPO var ', trim(nf90_strerror(ierr))


       ierr = nf90_put_att(file_id, ypoid, "Long_name", 'Meridional index')
       if (ierr/=nf90_noerr) print*, 'Error defining Long_name att for YPO var ', trim(nf90_strerror(ierr))


       ierr = nf90_put_att(file_id, lonid, "Long_name", 'Longitude')
       if (ierr/=nf90_noerr) print*, 'Error defining Long_name att for LON var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, lonid, "Units", 'degrees_east')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for LON var ', trim(nf90_strerror(ierr))


       ierr = nf90_put_att(file_id, latid, "Long_name", 'Latitude')
       if (ierr/=nf90_noerr) print*, 'Error defining Long_name att for LAT var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, latid, "Units", 'degrees_north')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for LAT var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, zpoid, "Long_name", 'Depth')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for ZPO var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, zpoid, "Units", 'm')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for ZPO var ', trim(nf90_strerror(ierr))
# 559 "listKrill_mod.f90"
       ierr = nf90_put_att(file_id, speciesid, "Long_name", 'Krill species')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for SPECIES var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, sexid, "Long_name", 'Krill sex')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for SEX var ',trim(nf90_strerror(ierr))
# 674 "listKrill_mod.f90"
       ! end of file definition
       ierr = nf90_enddef(file_id)
       if (ierr/=nf90_noerr) print*, 'ERROR END NETCDF FILE ', nf90_strerror(ierr)


       ! Put first timestep
       ncinc = 1

       ncfreq = output_freq*dt/86400.

       nctime = istart*dtnc/86400.

       ierr = nf90_put_var(file_id, timeid, nctime, (/ncinc/))
       if (ierr/=nf90_noerr) print*, ' Error writing T var ', trim(nf90_strerror(ierr))


       ! Allocate size of list variables
       allocate(all_species(npart))
       allocate(all_sex(npart))
# 727 "listKrill_mod.f90"
       allocate(all_xpo(npart))
       allocate(all_lon(npart))
       allocate(all_ypo(npart))
       allocate(all_lat(npart))
       allocate(all_zpo(npart))
# 744 "listKrill_mod.f90"
       call this%reset()

       i = 1
       j = 0

       do while(this%moreValues())
          curr => this%currentValue()

          select type(curr)

          type is (Krill)
             all_species(i) = curr%get_species()
             all_sex(i) = curr%get_sex()
# 779 "listKrill_mod.f90"
             all_xpo(i) = curr%get_xpo()
             all_ypo(i) = curr%get_ypo()
             all_zpo(i) = curr%get_zpo()
# 805 "listKrill_mod.f90"
          end select

          ! Compute lat/lon coordinates
          call xy_to_ll_NEMO(all_xpo(i), all_ypo(i), all_lon(i), all_lat(i))

          ! Write values to variables
          if ( mod(i, npart)==0 ) then

             j = j + 1

             ierr = nf90_put_var(file_id, speciesid, all_species, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF SPECIES VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, sexid, all_sex, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF SEX VARIABLE ',nf90_strerror(ierr)
# 874 "listKrill_mod.f90"
             ierr = nf90_put_var(file_id, xpoid, all_xpo, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF XPO VARIABLE ',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, lonid, all_lon, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF LON VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, ypoid, all_ypo, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF YPO VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, latid, all_lat, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF LAT VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, zpoid, all_zpo, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF ZPO VARIABLE',nf90_strerror(ierr)
# 962 "listKrill_mod.f90"
          endif

          i = mod(i+1, npart)
          if (i==0) i = npart

          call this%next()
       end do

       ! Deallocating all allocatable arrays
       deallocate(all_species)
       deallocate(all_sex)
# 986 "listKrill_mod.f90"
       deallocate(all_xpo)
       deallocate(all_lon)
       deallocate(all_ypo)
       deallocate(all_lat)
       deallocate(all_zpo)
# 1024 "listKrill_mod.f90"
    case (1)

       ncinc = ncinc + 1

       ! Put timestep




       nctime = nctime + ncfreq


       ierr = nf90_put_var(file_id, timeid, nctime, (/ncinc/))
       if (ierr/=nf90_noerr) print*, 'Error writing T var : ', trim(nf90_strerror(ierr))

       allocate(all_xpo(npart))
       allocate(all_lon(npart))
       allocate(all_ypo(npart))
       allocate(all_lat(npart))
       allocate(all_zpo(npart))
# 1070 "listKrill_mod.f90"
       call this%reset()

       i = 1
       j = 0

       do while(this%moreValues())
          curr => this%currentValue()

          select type(curr)

          type is (Krill)
             all_xpo(i) = curr%get_xpo()
             all_ypo(i) = curr%get_ypo()
             all_zpo(i) = curr%get_zpo()
# 1110 "listKrill_mod.f90"
          end select

          ! Compute lat/lon coordinates
          call xy_to_ll_NEMO(all_xpo(i), all_ypo(i), all_lon(i), all_lat(i))

          ! Write values to variables
          if ( mod(i, npart)==0 ) then

             j = j + 1

             ierr = nf90_put_var(file_id, xpoid, all_xpo, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF XPO VARIABLE ',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, lonid, all_lon, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF LON VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, ypoid, all_ypo, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF YPO VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, latid, all_lat, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF LAT VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, zpoid, all_zpo, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF ZPO VARIABLE',nf90_strerror(ierr)
# 1207 "listKrill_mod.f90"
          endif

          i = mod(i+1, npart)
          if (i==0) i = npart

          call this%next()
       end do

       ! Deallocating all allocatable arrays
       deallocate(all_xpo)
       deallocate(all_lon)
       deallocate(all_ypo)
       deallocate(all_lat)
       deallocate(all_zpo)
# 1247 "listKrill_mod.f90"
    case(2) ! Close

       ierr = nf90_close(file_id)

       print*, 'Close the netcdf file'

    end select

    return

  end subroutine netcdfKrill

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module listKrill_mod
