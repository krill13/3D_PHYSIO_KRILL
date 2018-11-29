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
    do i =  1, (krillPosition - 1)
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
    !   Argument :
    !   action = 0 => initialization
    !   action = 1 => Record data
    !   action = 2 => Finalization

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
         dev_freqid, molt_lengthid, awid, bwid, eiid, erid, a_moltid, b_moltid, k0_phyid, k0_zooid, h0id, prop_mezid, &
         Aid, r0id, w_moltid, a_repid, b_repid, mass_eggid, ingest_id, breath_id, arr_id, enc_phy_id, enc_zoo_id, &
         hand_id, timeid, xpoid, ypoid, zpoid, opt_depthid, dummyid, lonid, latid, ncinc, nctlen, &
         tpid, diatid, flagid, miczid, meszid,max_chlAid, nb_moltid, grow_cumid, egg_costid, nb_eggid, &
         gonadid, max_gonadid, ingest_phyid, ingest_zooid, grow_wayid, rbid, p_gonadid, grow_gonadid, cp_mezid

    character(len=3), dimension(12) :: month_nc = (/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/)


    !  integer, dimension(2) :: matrixDim

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
#ifdef PHYSIO
    real, dimension(:), allocatable :: all_opt_depth
    real, dimension(:), allocatable :: all_length
    real, dimension(:), allocatable :: all_mass
    real, dimension(:), allocatable :: all_dev_freq
    real, dimension(:), allocatable :: all_molt_length
    real, dimension(:), allocatable :: all_gonad
    real, dimension(:), allocatable :: all_max_gonad
    real, dimension(:), allocatable :: all_aw
    real, dimension(:), allocatable :: all_bw
    real, dimension(:), allocatable :: all_ei
    real, dimension(:), allocatable :: all_er
    real, dimension(:), allocatable :: all_a_molt
    real, dimension(:), allocatable :: all_b_molt
    real, dimension(:), allocatable :: all_k0_phy
    real, dimension(:), allocatable :: all_k0_zoo
    real, dimension(:), allocatable :: all_h0
    real, dimension(:), allocatable :: all_A
    real, dimension(:), allocatable :: all_prop_mez
    real, dimension(:), allocatable :: all_r0
    real, dimension(:), allocatable :: all_rb
    real, dimension(:), allocatable :: all_a_rep
    real, dimension(:), allocatable :: all_b_rep
    real, dimension(:), allocatable :: all_mass_egg
    real, dimension(:), allocatable :: all_w_molt
    real, dimension(:), allocatable :: all_nb_molt
    real, dimension(:), allocatable :: all_p_gonad
    real, dimension(:), allocatable :: all_grow_gonad
    real, dimension(:), allocatable :: all_grow_cum
    real, dimension(:), allocatable :: all_grow_way
    real, dimension(:), allocatable :: all_cp_mez
    real, dimension(:), allocatable :: all_ingest
    real, dimension(:), allocatable :: all_ingest_phy
    real, dimension(:), allocatable :: all_ingest_zoo
    real, dimension(:), allocatable :: all_breath 
    real, dimension(:), allocatable :: all_arr 
    real, dimension(:), allocatable :: all_enc_phy
    real, dimension(:), allocatable :: all_enc_zoo
    real, dimension(:), allocatable :: all_hand 
    real, dimension(:), allocatable :: all_nb_egg
    real, dimension(:), allocatable :: all_egg_cost 
    real, dimension(:), allocatable :: all_tp
    real, dimension(:), allocatable :: all_diat
    real, dimension(:), allocatable :: all_flag
    real, dimension(:), allocatable :: all_micz
    real, dimension(:), allocatable :: all_mesz
    real, dimension(:), allocatable :: all_max_chlA
#endif

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
       filename_nc = trim(outfile)//"_traj.nc"

#ifdef DEBUG
       print*, trim(filename_nc)
#endif

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
       if (ierr/=nf90_noerr) print*, 'ERROR TIME DIMENSION DEFINITION HERE', trim(nf90_strerror(ierr))

       ! creation of the variables for the files
   
       ierr = nf90_def_var(file_id, "species",  nf90_int,  (/xdimid, ydimid/), speciesid)
       if (ierr/=nf90_noerr) print*, 'ERROR SPECIES VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "sex",     nf90_int,   (/xdimid, ydimid/), sexid)
       if (ierr/=nf90_noerr) print*, 'ERROR SEX VARIABLE DEFINITION ',    nf90_strerror(ierr)
#ifdef PHYSIO
       ierr = nf90_def_var(file_id, "aw",      nf90_float, (/xdimid, ydimid/), awid)
       if (ierr/=nf90_noerr) print*, 'ERROR AW VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "bw",      nf90_float, (/xdimid, ydimid/), bwid)
       if (ierr/=nf90_noerr) print*, 'ERROR BW VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "ei",      nf90_float, (/xdimid, ydimid/), eiid)
       if (ierr/=nf90_noerr) print*, 'ERROR EI VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "er",      nf90_float, (/xdimid, ydimid/), erid)
       if (ierr/=nf90_noerr) print*, 'ERROR ER VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "a_molt",  nf90_float, (/xdimid, ydimid/), a_moltid)
       if (ierr/=nf90_noerr) print*, 'ERROR A_MOLT VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "b_molt",  nf90_float, (/xdimid, ydimid/), b_moltid)
       if (ierr/=nf90_noerr) print*, 'ERROR B_MOLT VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "k0_phy",  nf90_float, (/xdimid, ydimid/), k0_phyid)
       if (ierr/=nf90_noerr) print*, 'ERROR K0_phy VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "k0_zoo",  nf90_float, (/xdimid, ydimid/), k0_zooid)
       if (ierr/=nf90_noerr) print*, 'ERROR K0_zoo VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "h0",      nf90_float, (/xdimid, ydimid/), h0id)
       if (ierr/=nf90_noerr) print*, 'ERROR H0 VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "A",       nf90_float, (/xdimid, ydimid/), Aid)
       if (ierr/=nf90_noerr) print*, 'ERROR A VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "prop_mez",nf90_float, (/xdimid, ydimid/), prop_mezid)
       if (ierr/=nf90_noerr) print*, 'ERROR PROP_MEZ VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "r0",      nf90_float, (/xdimid, ydimid/), r0id)
       if (ierr/=nf90_noerr) print*,'ERROR R0 VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "rb",      nf90_float, (/xdimid, ydimid/), rbid)
       if (ierr/=nf90_noerr) print*,'ERROR Rb VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "w_molt",  nf90_float, (/xdimid, ydimid/), w_moltid)
       if (ierr/=nf90_noerr) print*, 'ERROR W_MOLT VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "a_rep",  nf90_float, (/xdimid, ydimid/), a_repid)
       if (ierr/=nf90_noerr) print*, 'ERROR A_REP VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "b_rep",  nf90_float, (/xdimid, ydimid/), b_repid)
       if (ierr/=nf90_noerr) print*, 'ERROR B_REP VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "mass_egg",  nf90_float, (/xdimid, ydimid/), mass_eggid)
       if (ierr/=nf90_noerr) print*, 'ERROR EGG_MASS VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "p_gonad",  nf90_float, (/xdimid, ydimid/), p_gonadid)
       if (ierr/=nf90_noerr) print*, 'ERROR P_GONAD VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "grow_gonad",  nf90_float, (/xdimid, ydimid/), grow_gonadid)
       if (ierr/=nf90_noerr) print*, 'ERROR GROW_GONAD VARIABLE DEFINITION ',    nf90_strerror(ierr)
#endif

       !ierr = nf90_def_var(file_id, "pref",   nf90_float, (/xdimid, ydimid, zdimid/), prefid)
       !if (ierr/=nf90_noerr) print*, 'ERROR PREF VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ! Time-dependent variables

       ierr = nf90_def_var(file_id, "time", nf90_float, (/timedimid/), timeid)
       if (ierr/=nf90_noerr) print*, "ERROR TIME VARIABLE DEFINITION ", nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "lon", nf90_float, (/xdimid, ydimid, timedimid/), lonid)
       if (ierr/=nf90_noerr) print*, 'ERROR LON VARIABLE DEFINITION ', trim(nf90_strerror(ierr))

       ierr = nf90_def_var(file_id, "lat", nf90_float, (/xdimid, ydimid, timedimid/), latid)
       if (ierr/=nf90_noerr) print*, 'ERROR LAT VARIABLE DEFINITION ', trim(nf90_strerror(ierr))

       ierr = nf90_def_var(file_id, "xpo",  nf90_float, (/xdimid, ydimid, timedimid/), xpoid)
       if (ierr/=nf90_noerr) print*, "ERROR XPO VARIABLE DEFINITION ", nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "ypo",  nf90_float, (/xdimid, ydimid, timedimid/), ypoid)
       if (ierr/=nf90_noerr) print*, "ERROR YPO VARIABLE DEFINITION ", nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "zpo",  nf90_float, (/xdimid, ydimid, timedimid/), zpoid)
       if (ierr/=nf90_noerr) print*, "ERROR ZPO VARIABLE DEFINITION ", nf90_strerror(ierr)
#ifdef PHYSIO
       ierr = nf90_def_var(file_id, "opt_depth",  nf90_float, (/xdimid, ydimid, timedimid/), opt_depthid)
       if (ierr/=nf90_noerr) print*, "ERROR OPT_DEPTH VARIABLE DEFINITION ", nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "size", nf90_float, (/xdimid, ydimid, timedimid/), lengthid)
       if (ierr/=nf90_noerr) print*, 'ERROR LENGTH VARIABLE DEFINITION ', nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "mass", nf90_float, (/xdimid, ydimid,timedimid/), massid)
       if (ierr/=nf90_noerr) print*, 'ERROR MASS VARIABLE DEFINITION ', nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "dev_freq",  nf90_float, (/xdimid, ydimid, timedimid/), dev_freqid)
       if (ierr/=nf90_noerr) print*, 'ERROR DEV_FREQ VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "molt_size", nf90_float, (/xdimid, ydimid, timedimid/), molt_lengthid)
       if (ierr/=nf90_noerr) print*, 'ERROR MOLT_LENGTH VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "nb_molt", nf90_float, (/xdimid, ydimid, timedimid/), nb_moltid)
       if (ierr/=nf90_noerr) print*, 'ERROR NB_MOLT VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "grow_cum", nf90_float, (/xdimid, ydimid, timedimid/), grow_cumid)
       if (ierr/=nf90_noerr) print*, 'ERROR GROW_CUM VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "grow_way", nf90_float, (/xdimid, ydimid, timedimid/), grow_wayid)
       if (ierr/=nf90_noerr) print*, 'ERROR GROW_WAY VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "cp_mez", nf90_float, (/xdimid, ydimid, timedimid/), cp_mezid)
       if (ierr/=nf90_noerr) print*, 'ERROR CP_MEZ VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "ingestion", nf90_float, (/xdimid, ydimid, timedimid/), ingest_id)
       if (ierr/=nf90_noerr) print*, 'ERROR INGESTION VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "ingestion_phy", nf90_float, (/xdimid, ydimid, timedimid/), ingest_phyid)
       if (ierr/=nf90_noerr) print*, 'ERROR INGESTION_PHY VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "ingestion_zoo", nf90_float, (/xdimid, ydimid, timedimid/), ingest_zooid)
       if (ierr/=nf90_noerr) print*, 'ERROR INGESTION_ZOO VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "breath", nf90_float, (/xdimid, ydimid, timedimid/), breath_id)
       if (ierr/=nf90_noerr) print*, 'ERROR BREATH VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "arr", nf90_float, (/xdimid, ydimid, timedimid/), arr_id)
       if (ierr/=nf90_noerr) print*, 'ERROR BREATH VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "enc_phy", nf90_float, (/xdimid, ydimid, timedimid/), enc_phy_id)
       if (ierr/=nf90_noerr) print*, 'ERROR ENC_PHY VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "enc_zoo", nf90_float, (/xdimid, ydimid, timedimid/), enc_zoo_id)
       if (ierr/=nf90_noerr) print*, 'ERROR ENC_ZOO VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "hand", nf90_float, (/xdimid, ydimid, timedimid/), hand_id)
       if (ierr/=nf90_noerr) print*, 'ERROR HAND VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "nb_egg", nf90_float, (/xdimid, ydimid, timedimid/), nb_eggid)
       if (ierr/=nf90_noerr) print*, 'ERROR NB_EGG VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "egg_cost", nf90_float, (/xdimid, ydimid, timedimid/), egg_costid)
       if (ierr/=nf90_noerr) print*, 'ERROR EGG_PROD VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "gonad", nf90_float, (/xdimid, ydimid, timedimid/), gonadid)
       if (ierr/=nf90_noerr) print*, 'ERROR GONAD VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "max_gonad", nf90_float, (/xdimid, ydimid, timedimid/), max_gonadid)
       if (ierr/=nf90_noerr) print*, 'ERROR MAX_GONAD VARIABLE DEFINITION ',     nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "tp",  nf90_float,   (/xdimid, ydimid, timedimid/), tpid)
       if (ierr/=nf90_noerr) print*, 'ERROR TP VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "diat",  nf90_float,   (/xdimid, ydimid, timedimid/), diatid)
       if (ierr/=nf90_noerr) print*, 'ERROR DIAT VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "flag",  nf90_float,   (/xdimid, ydimid, timedimid/), flagid)
       if (ierr/=nf90_noerr) print*, 'ERROR FLAG VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "micz",  nf90_float,   (/xdimid, ydimid, timedimid/), miczid)
       if (ierr/=nf90_noerr) print*, 'ERROR MICZ VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "mesz",  nf90_float,   (/xdimid, ydimid, timedimid/), meszid)
       if (ierr/=nf90_noerr) print*, 'ERROR MESZ VARIABLE DEFINITION ',    nf90_strerror(ierr)

       ierr = nf90_def_var(file_id, "max_chlA",  nf90_float,   (/xdimid, ydimid, timedimid/), max_chlAid)
       if (ierr/=nf90_noerr) print*, 'ERROR MAX_CHLA VARIABLE DEFINITION ',    nf90_strerror(ierr)
#endif

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
#ifdef PHYSIO
       ierr = nf90_put_att(file_id, opt_depthid, "Long_name", 'Surface depth optimal')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for OPT_DEPTH var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, opt_depthid, "Units", 'm')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for OPT_DEPTH var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, massid, "Long_name", 'Body mass')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for MASS var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, massid, "Units", 'mgC')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for MASS var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, lengthid, "Long_name", 'Body lenght')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for SIZE var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, lengthid, "Units", 'mm')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for SIZE var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, dev_freqid, "Long_name", 'Development frequency')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for DEV_FREQ var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, molt_lengthid, "Long_name", 'Next molt body lenght')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for MOULT_SIZE var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, molt_lengthid, "Units", 'mm')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for SIZE var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, nb_moltid, "Long_name", 'Number of molt')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for NB_MOLT var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, grow_cumid, "Long_name", 'Cumulative growth of the individual')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for GROW_CUM var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, grow_cumid, "Units", 'mgC')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for GROW_CUM var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, grow_wayid, "Long_name", 'Instantaneous net growth of the individual')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for GROW_WAY var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, grow_wayid, "Units", 'mgC')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for GROW_WAY var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, cp_mezid, "Long_name", 'Proportion of carbon from mesozooplankton')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for CP_MEZ var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, ingest_id, "Long_name", 'Gain due to the ingestion mechanism')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for INGESTION var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, ingest_id,"Units",'mg C/ mg ind')
       if (ierr/=nf90_noerr) print*,'Error defining Units att for INGESTION var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, ingest_phyid, "Long_name", 'Gain due to the ingestion on phytoplankton')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for INGESTION_PHY var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,ingest_phyid,"Units",'mg C/ mg ind')
       if (ierr/=nf90_noerr) print*,'Error defining Units att for INGESTION_PHY var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, ingest_zooid, "Long_name", 'Gain due to the ingestion on zooplankton')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for INGESTION_ZOO var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,ingest_zooid,"Units",'mg C/ mg ind')
       if (ierr/=nf90_noerr) print*,'Error defining Units att for INGESTION_ZOO var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, breath_id, "Long_name", 'Loss due to the respiration mechanism')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for BREATH var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, breath_id, "Units", 'mg C / mg ind')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for BREATH var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, arr_id, "Long_name", 'Arrhenius relationship to the temperature')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for BREATH var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, enc_phy_id, "Long_name", 'Encounter Kernel rate for phytoplankton')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for ENC_PHY var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, enc_zoo_id, "Long_name", 'Encounter Kernel rate for zooplankton')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for ENC_ZOO var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, hand_id, "Long_name", 'Handling time')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for HAND var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, egg_costid, "Long_name", 'Costs of the production of one egg batch')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for EGG_PROD var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, egg_costid, "Units", 'mgC')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for EGG_PROD var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, gonadid, "Long_name", 'Gonad mass in the individual')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for GONAD var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, gonadid, "Units", 'mgC')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for GONAD var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, max_gonadid, "Long_name", 'Maximal gonad mass for the individual')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for MAX_GONAD var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, max_gonadid, "Units", 'mgC')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for MAX_GONAD var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, nb_eggid, "Long_name", 'Number od egg per female')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for NB_EGG var ',trim(nf90_strerror(ierr))
#endif
       ierr = nf90_put_att(file_id, speciesid, "Long_name", 'Krill species')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for SPECIES var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, sexid, "Long_name", 'Krill sex')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for SEX var ',trim(nf90_strerror(ierr))
#ifdef PHYSIO
       ierr = nf90_put_att(file_id, a_moltid, "Long_name", ' Constant of IMP relationship')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for A_MOLT var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, b_moltid, "Long_name", 'Coefficient of IMP relationship')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for B_MOLT var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, w_moltid, "Long_name", 'Molt mass lost')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for W_MOLT var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, w_moltid, "Units", '%')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for W_MOLT var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, eiid, "Long_name", 'Activation energy for ingestion')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for EI var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, eiid, "Units", 'eV')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for EI var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, erid, "Long_name", 'Activation energy for respiration')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for ER var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, erid, "Units", 'eV')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for ER var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, k0_phyid, "Long_name", 'Scaling constant')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for K0_PHY var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, k0_phyid, "Units", 'l.h^-1.mgC^-3/4')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for K0_PHY var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, k0_zooid, "Long_name", 'Scaling constant')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for K0_ZOO var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, k0_zooid, "Units", 'l.h^-1.mgC^-3/4')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for K0_ZOO var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, h0id, "Long_name", 'Scaling constant')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for H0 var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, h0id, "Units", 'h.mgCfood^-1.mgC^-3/4')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for H0 var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, r0id, "Long_name", 'Scaling constant')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for R0 var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, r0id, "Units", 'mgC^3/4.h^-1')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for R0 var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, rbid, "Long_name", 'Scaling exponent')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for RB var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, Aid, "Long_name", 'Assimilation efficiency coefficient')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for A var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, Aid, "Units", '%')
       if (ierr/=nf90_noerr) print*, 'Error defining Units att for A var ', trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, prop_mezid, "Long_name", 'Initial proportion of carbon from mesozooplankton')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for PROP_MEZ var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, awid, "Long_name", 'Constant length/mass relationship')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for AW var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, bwid, "Long_name", 'Coefficient length/mass relationship')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for BW var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, a_repid, "Long_name", 'Constant length/number egg relationship')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for A_REP var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, b_repid, "Long_name", 'Coefficient length/number egg relationship')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for B_REP var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, mass_eggid, "Long_name", 'Mass of one egg')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for EGG_MASS var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, mass_eggid, "Long_name", 'mgC')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for EGG_MASS var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, p_gonadid, "Long_name", 'Proportion of the gonad organ on the total body mass')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for P_GONAD var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id, grow_gonadid, "Long_name", 'Percentage of growth send to gonads')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for GROW_GONAD var ',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,tpid,"Long_name",'Temperature')
       if (ierr/=nf90_noerr) print*,'Error defining Long_name att for Temp var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,tpid,"Units",'°C')
       if (ierr/=nf90_noerr) print*,'Error defining Units att for Temp var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,tpid,"_FillValue",nf90_fill_real)
       if (ierr/=nf90_noerr) print*,'Error defining fillvalue att for Temp var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,diatid,"Units",'mg C m^-3')
       if (ierr/=nf90_noerr) print*,'Error defining Units att for Dia var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,diatid,"_FillValue",nf90_fill_real)
       if (ierr/=nf90_noerr) print*,'Error defining fillvalue att for Diat var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,flagid,"Units",'mg C m^-3')
       if (ierr/=nf90_noerr) print*,'Error defining Units att for Flag var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,flagid,"_FillValue",nf90_fill_real)
       if (ierr/=nf90_noerr) print*,'Error defining fillvalue att for Flag var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,miczid,"Units",'mg C m^-3')
       if (ierr/=nf90_noerr) print*,'Error defining Units att for Micz var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,miczid,"_FillValue",nf90_fill_real)
       if (ierr/=nf90_noerr) print*,'Error defining fillvalue att for Micz var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,meszid,"Units",'mg C m^-3')
       if (ierr/=nf90_noerr) print*,'Error defining Units att for Mesz var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,meszid,"_FillValue",nf90_fill_real)
       if (ierr/=nf90_noerr) print*,'Error defining fillvalue att for Mesz var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,max_chlAid,"Units",'mg C m^-3')
       if (ierr/=nf90_noerr) print*,'Error defining Units att for max_chlA var',trim(nf90_strerror(ierr))

       ierr = nf90_put_att(file_id,max_chlAid,"_FillValue",nf90_fill_real)
       if (ierr/=nf90_noerr) print*,'Error defining fillvalue att for max_chlA var',trim(nf90_strerror(ierr))
#endif

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
#ifdef PHYSIO
       allocate(all_aw(npart))
       allocate(all_bw(npart))
       allocate(all_ei(npart))
       allocate(all_er(npart))
       allocate(all_a_molt(npart))
       allocate(all_b_molt(npart))
       allocate(all_k0_phy(npart))
       allocate(all_k0_zoo(npart))
       allocate(all_h0(npart))
       allocate(all_A(npart))
       allocate(all_prop_mez(npart))
       allocate(all_r0(npart))
       allocate(all_rb(npart))
       allocate(all_w_molt(npart))
       allocate(all_nb_molt(npart))
       allocate(all_a_rep(npart))
       allocate(all_b_rep(npart))
       allocate(all_mass_egg(npart))
       allocate(all_p_gonad(npart))
       allocate(all_grow_gonad(npart))
       allocate(all_grow_cum(npart))
       allocate(all_grow_way(npart))
       allocate(all_cp_mez(npart))
       allocate(all_ingest(npart))
       allocate(all_ingest_phy(npart))
       allocate(all_ingest_zoo(npart))
       allocate(all_breath(npart))
       allocate(all_arr(npart))
       allocate(all_enc_phy(npart))
       allocate(all_enc_zoo(npart))
       allocate(all_hand(npart))
       allocate(all_nb_egg(npart))
       allocate(all_egg_cost(npart))
       allocate(all_gonad(npart))
       allocate(all_max_gonad(npart))
#endif
       allocate(all_xpo(npart))
       allocate(all_lon(npart))
       allocate(all_ypo(npart))
       allocate(all_lat(npart))
       allocate(all_zpo(npart))
#ifdef PHYSIO
       allocate(all_opt_depth(npart))
       allocate(all_length(npart))
       allocate(all_mass(npart))
       allocate(all_dev_freq(npart))
       allocate(all_molt_length(npart))
       allocate(all_tp(npart))
       allocate(all_diat(npart))
       allocate(all_flag(npart))
       allocate(all_micz(npart))
       allocate(all_mesz(npart))
       allocate(all_max_chlA(npart))
#endif

       call this%reset()

       i = 1
       j = 0

       do while(this%moreValues())
          curr => this%currentValue()

          select type(curr)

          type is (Krill)
             all_species(i)     = curr%get_species()
             all_sex(i)         = curr%get_sex()
#ifdef PHYSIO
             all_aw(i)          = curr%get_aw()
             all_bw(i)          = curr%get_bw()
             all_ei(i)          = curr%get_ei()
             all_er(i)          = curr%get_er()
             all_a_molt(i)      = curr%get_a_molt()
             all_b_molt(i)      = curr%get_b_molt()
             all_k0_phy(i)      = curr%get_k0_phy()
             all_k0_zoo(i)      = curr%get_k0_zoo()
             all_h0(i)          = curr%get_h0()
             all_A(i)           = curr%get_A()
             all_prop_mez(i)    = curr%get_prop_mez()
             all_r0(i)          = curr%get_r0()
             all_rb(i)          = curr%get_rb()
             all_w_molt(i)      = curr%get_w_molt()
             all_a_rep(i)       = curr%get_a_rep()
             all_b_rep(i)       = curr%get_b_rep()
             all_mass_egg(i)    = curr%get_mass_egg()
             all_p_gonad(i)     = curr%get_p_gonad()
             all_grow_gonad(i)  = curr%get_grow_gonad()
             all_nb_molt(i)     = curr%get_nb_molt()
             all_grow_cum(i)    = curr%get_grow_cum()
             all_grow_way(i)    = curr%get_grow_way()
             all_cp_mez(i)      = curr%get_cp_mez()
#endif
             all_xpo(i)         = curr%get_xpo()
             all_ypo(i)         = curr%get_ypo()
             all_zpo(i)         = curr%get_zpo()
#ifdef PHYSIO
             all_opt_depth(i)   = curr%get_opt_depth()
             all_length(i)      = curr%get_length()
             all_mass(i)        = curr%get_mass()
             all_dev_freq(i  )  = curr%get_dev_freq()
             all_molt_length(i) = curr%get_molt_length()
             all_ingest(i)      = curr%get_ingestion()
             all_ingest_phy(i)  = curr%get_ingestion_phy()
             all_ingest_zoo(i)  = curr%get_ingestion_zoo()
             all_breath(i)      = curr%get_breath()
             all_arr(i)         = curr%get_arr()
             all_enc_phy(i)     = curr%get_enc_phy()
             all_enc_zoo(i)     = curr%get_enc_zoo()
             all_hand(i)        = curr%get_hand()
             all_nb_egg(i)      = curr%get_nb_egg()
             all_egg_cost(i)    = curr%get_egg_cost()
             all_gonad(i)       = curr%get_gonad()
             all_max_gonad(i)   = curr%get_max_gonad()
             all_tp(i)          = curr%get_tp()
             all_diat(i)        = curr%get_diat()
             all_flag(i)        = curr%get_flag()
             all_micz(i)        = curr%get_micz()
             all_mesz(i)        = curr%get_mesz()
             all_max_chlA(i)    = curr%get_max_chlA()
#endif
          end select
          
          ! Write values to variables
          if ( mod(i, npart)==0 ) then

             j = j + 1

             ierr = nf90_put_var(file_id, speciesid, all_species, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF SPECIES VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, sexid, all_sex, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF SEX VARIABLE ',nf90_strerror(ierr)
#ifdef PHYSIO
             ierr = nf90_put_var(file_id, awid, all_aw, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF AW VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, bwid, all_bw, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF BW VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, eiid, all_ei, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF EI VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, erid, all_er, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF ER VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, a_moltid, all_a_molt, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF A_MOLT VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, b_moltid, all_b_molt, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF B_MOLT VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, k0_phyid, all_k0_phy, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF K0_phy VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, k0_zooid, all_k0_zoo, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF K0 VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, h0id, all_h0, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF H0 VARIABLE ',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, Aid, all_A, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF A VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, prop_mezid, all_prop_mez, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF PROP_MEZ VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, r0id, all_r0, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF R0 VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, rbid, all_rb, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF RB VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, w_moltid, all_w_molt, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF W_MOLT VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, a_repid, all_a_rep, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF A_REP VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, b_repid, all_b_rep, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF B_REP VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, mass_eggid, all_mass_egg, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF EGG_MASS VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, p_gonadid, all_p_gonad, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF P_GONAD VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, grow_gonadid, all_grow_gonad, (/1, j/), (/npart, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF GROW_GONAD VARIABLE',nf90_strerror(ierr)
#endif


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
#ifdef PHYSIO
             ierr = nf90_put_var(file_id, opt_depthid, all_opt_depth, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF OPT_DEPTH VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, lengthid, all_length, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF SIZE VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, massid, all_mass, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF MASS VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, dev_freqid, all_dev_freq, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF DEV_FREQ VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, molt_lengthid, all_molt_length, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF MOLT_SIZE VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, nb_moltid, all_nb_molt, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF NB_MOLT VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, grow_cumid, all_grow_cum, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF GROW_CUM VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, grow_wayid, all_grow_way, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF GROW_WAY VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, cp_mezid, all_cp_mez, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF CP_MEZ VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, ingest_id, all_ingest, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF INGEST VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, ingest_phyid, all_ingest_phy, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF INGEST_PHY VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, ingest_zooid, all_ingest_zoo, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF INGEST_ZOO VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, breath_id, all_breath, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF BREATH VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, arr_id, all_arr, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF ARRHENIUS VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, enc_phy_id, all_enc_phy, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF ENC_PHY VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, enc_zoo_id, all_enc_zoo, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF ENC_ZOO VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, hand_id, all_hand, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF HAND VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, nb_eggid, all_nb_egg, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF NB_EGG VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, egg_costid, all_egg_cost, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF EGG_PROD VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, gonadid, all_gonad, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF GONAD VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, max_gonadid, all_max_gonad, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF MAX_GONAD VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, tpid, all_tp, (/1, j,ncinc/), (/npart,1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF TP VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, diatid, all_diat, (/1, j,ncinc/), (/npart,1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF DIAT VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, flagid, all_flag, (/1, j,ncinc/), (/npart,1 , 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF FLAG VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, miczid, all_micz, (/1, j,ncinc/), (/npart,1 , 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF MICZ VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, meszid, all_mesz, (/1, j,ncinc/), (/npart,1 , 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF MESZ VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, max_chlAid, all_max_chlA, (/1, j,ncinc/), (/npart,1 , 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF MAX_CHLA VARIABLE',nf90_strerror(ierr)
#endif

          endif

          i = mod(i+1, npart)
          if (i==0) i = npart

          call this%next()
       end do

       ! Deallocating all allocatable arrays
       deallocate(all_species)
       deallocate(all_sex)
#ifdef PHYSIO
       deallocate(all_aw)
       deallocate(all_bw)
       deallocate(all_ei)
       deallocate(all_er)
       deallocate(all_a_molt)
       deallocate(all_b_molt)
       deallocate(all_k0_phy)
       deallocate(all_k0_zoo)
       deallocate(all_h0)
       deallocate(all_A)
       deallocate(all_prop_mez)
       deallocate(all_r0)
       deallocate(all_rb)
#endif
       deallocate(all_xpo)
       deallocate(all_lon)
       deallocate(all_ypo)
       deallocate(all_lat)
       deallocate(all_zpo)
#ifdef PHYSIO
       deallocate(all_opt_depth)
       deallocate(all_length)
       deallocate(all_mass)
       deallocate(all_dev_freq)
       deallocate(all_molt_length)
       deallocate(all_w_molt)
       deallocate(all_a_rep)
       deallocate(all_b_rep)
       deallocate(all_mass_egg)
       deallocate(all_p_gonad)
       deallocate(all_grow_gonad)
       deallocate(all_nb_molt)
       deallocate(all_grow_cum)
       deallocate(all_grow_way)
       deallocate(all_cp_mez)
       deallocate(all_ingest)
       deallocate(all_ingest_phy)
       deallocate(all_ingest_zoo)
       deallocate(all_breath)
       deallocate(all_arr)
       deallocate(all_enc_phy)
       deallocate(all_enc_zoo)
       deallocate(all_hand)
       deallocate(all_nb_egg)
       deallocate(all_egg_cost)
       deallocate(all_gonad)
       deallocate(all_max_gonad)
       deallocate(all_tp)
       deallocate(all_diat)
       deallocate(all_flag)
       deallocate(all_micz)
       deallocate(all_mesz)
       deallocate(all_max_chlA)
#endif 

    case (1)

       ncinc = ncinc + 1

       ! Put timestep

#ifdef BACK
       nctime = nctime - ncfreq
#else
       nctime = nctime + ncfreq
#endif

       ierr = nf90_put_var(file_id, timeid, nctime, (/ncinc/))
       if (ierr/=nf90_noerr) print*, 'Error writing T var : ', trim(nf90_strerror(ierr))

       allocate(all_xpo(npart))
       allocate(all_lon(npart))
       allocate(all_ypo(npart))
       allocate(all_lat(npart))
       allocate(all_zpo(npart))
#ifdef PHYSIO
       allocate(all_opt_depth(npart))
       allocate(all_length(npart))
       allocate(all_mass(npart))
       allocate(all_dev_freq(npart))
       allocate(all_molt_length(npart))
       allocate(all_nb_molt(npart))
       allocate(all_grow_cum(npart))
       allocate(all_grow_way(npart))
       allocate(all_cp_mez(npart))
       allocate(all_ingest(npart))
       allocate(all_ingest_phy(npart))
       allocate(all_ingest_zoo(npart))
       allocate(all_breath(npart))
       allocate(all_arr(npart))
       allocate(all_enc_phy(npart))
       allocate(all_enc_zoo(npart))
       allocate(all_hand(npart))
       allocate(all_nb_egg(npart))
       allocate(all_egg_cost(npart))
       allocate(all_gonad(npart))
       allocate(all_max_gonad(npart))
       allocate(all_tp(npart))
       allocate(all_diat(npart))
       allocate(all_flag(npart))
       allocate(all_micz(npart))
       allocate(all_mesz(npart))
       allocate(all_max_chlA(npart))
#endif
       call this%reset()

       i = 1
       j = 0

       do while(this%moreValues())
          curr => this%currentValue()

          select type(curr)

          type is (Krill)
             all_xpo(i)         = curr%get_xpo()
             all_ypo(i)         = curr%get_ypo()
             all_zpo(i)         = curr%get_zpo()
#ifdef PHYSIO
             all_opt_depth(i)   = curr%get_opt_depth()
             all_length(i)      = curr%get_length()
             all_mass(i)        = curr%get_mass()
             all_dev_freq(i)    = curr%get_dev_freq()
             all_molt_length(i) = curr%get_molt_length()
             all_nb_molt(i)     = curr%get_nb_molt()
             all_grow_cum(i)    = curr%get_grow_cum()
             all_grow_way(i)    = curr%get_grow_way()
             all_cp_mez(i)      = curr%get_cp_mez()
             all_ingest(i)      = curr%get_ingestion()
             all_ingest_phy(i)  = curr%get_ingestion_phy()
             all_ingest_zoo(i)  = curr%get_ingestion_zoo()
             all_breath(i)      = curr%get_breath()
             all_arr(i)         = curr%get_arr()
             all_enc_phy(i)     = curr%get_enc_phy()
             all_enc_zoo(i)     = curr%get_enc_zoo()
             all_hand(i)        = curr%get_hand()
             all_nb_egg(i)      = curr%get_nb_egg()
             all_egg_cost(i)    = curr%get_egg_cost()
             all_gonad(i)       = curr%get_gonad()
             all_max_gonad(i)   = curr%get_max_gonad()
             all_tp(i)          = curr%get_tp()
             all_diat(i)        = curr%get_diat()
             all_flag(i)        = curr%get_flag()
             all_micz(i)        = curr%get_micz()
             all_mesz(i)        = curr%get_mesz()
             all_max_chlA(i)    = curr%get_max_chlA()
#endif 
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
#ifdef PHYSIO
             ierr = nf90_put_var(file_id, opt_depthid, all_opt_depth, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF OPT_DEPTH VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, lengthid, all_length, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF SIZE VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, massid, all_mass, (/1, j, ncinc/), (/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF MASS VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, dev_freqid, all_dev_freq, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF DEV_FREQ VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, molt_lengthid, all_molt_length, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF MOLT_SIZE VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, nb_moltid, all_nb_molt, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF NB_MOLT VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, grow_cumid, all_grow_cum, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF GROW_CUM VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, grow_wayid, all_grow_way, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF GROW_WAY VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, cp_mezid, all_cp_mez, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF CP_MEZ VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, ingest_id, all_ingest, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF INGEST VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, ingest_phyid, all_ingest_phy, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF INGEST_PHY VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, ingest_zooid, all_ingest_zoo, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF INGEST_ZOO VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, breath_id, all_breath, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF BREATH VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, arr_id, all_arr, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF ARRHENIUS VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, enc_phy_id, all_enc_phy, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF ENC_PHY VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, enc_zoo_id, all_enc_zoo, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF ENC_ZOO VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, hand_id, all_hand, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF HAND VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, nb_eggid, all_nb_egg, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF NB_EGG VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, egg_costid, all_egg_cost, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF EGG_PROD VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, gonadid, all_gonad, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF GONAD VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, max_gonadid, all_max_gonad, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF MAX_GONAD VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, tpid, all_tp, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF TP VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, diatid, all_diat, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF DIAT  VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, flagid, all_flag, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF FLAG VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, miczid, all_micz, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF MICZ VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, meszid, all_mesz, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF MESZ VARIABLE',nf90_strerror(ierr)

             ierr = nf90_put_var(file_id, max_chlAid, all_max_chlA, (/1, j, ncinc/),(/npart, 1, 1/))
             if (ierr/=nf90_noerr) print*,'ERROR IN THE SAVING OF MAX_CHLA VARIABLE',nf90_strerror(ierr)
#endif
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
#ifdef PHYSIO
       deallocate(all_opt_depth)
       deallocate(all_length)
       deallocate(all_mass)
       deallocate(all_dev_freq)
       deallocate(all_molt_length)
       deallocate(all_nb_molt)
       deallocate(all_grow_cum)
       deallocate(all_grow_way)
       deallocate(all_cp_mez)
       deallocate(all_ingest)
       deallocate(all_ingest_phy)
       deallocate(all_ingest_zoo)
       deallocate(all_breath)
       deallocate(all_arr)
       deallocate(all_enc_phy)
       deallocate(all_enc_zoo)
       deallocate(all_hand)
       deallocate(all_nb_egg)
       deallocate(all_egg_cost)
       deallocate(all_gonad)
       deallocate(all_max_gonad)
       deallocate(all_tp)
       deallocate(all_diat)
       deallocate(all_flag)
       deallocate(all_micz)
       deallocate(all_mesz)
       deallocate(all_max_chlA)
#endif
    case(2) ! Close

       ierr = nf90_close(file_id)
       
       print*, 'Close the netcdf file'

    end select

    return

  end subroutine netcdfKrill

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module listKrill_mod
