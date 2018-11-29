module part_def

  !---------------------------------------------------------------------
  ! Grid dimensions
  ! WARNING: here only interested in the first 20 layers (~550m) !!!
  ! ilo = 46 in NEMO


  integer, parameter :: m=197, n=234, ilo=20, nb_pars=14

  ! Topography/domain

  integer*2, dimension(m,n) :: nlayer
  
  real, dimension(ilo) :: dd, dz, up_depth, mid_depth

  real, dimension(m,n) :: dlx, dly, lon, lat, lon_f, lat_f

  ! Time steps and dates

  integer, dimension(6) :: time, tzero

  integer, dimension(3) :: today, now, tnc_origin

  integer :: initial, final, kstep, dt, dtnc, kd_ini

  integer :: istart, iend, ystart, mstart, dstart, yend, mend, dend

  real, parameter :: pi = 3.141592653589

  real :: time_sunrise, time_sunset, p_night

  logical :: light_day, FORC_PMZA, INTRA_SPE ! true=day | false=night

  character(len=100) :: tnc_origin_out, tnc_units

  ! Dynamic variables

  real, dimension(m,n,ilo) :: u, v, w, u1, v1, w1, u2, v2, w2, du, dv, dw, temp

  real, dimension(m,n,ilo) :: diatom, flagel, mesozoo, microzoo, food2

  real, dimension(m,n,2) :: salt

  ! Input file

  character(len=150) :: infile, infile_food

  !---------------------------------------------------------------------
  ! Particle tracking

  integer, parameter :: max_number_part=20000000, max_grid_zone=600

  integer , parameter   :: nbpb=1880, nb_back_time = 912

  integer, dimension(m,n,20) :: density_part ! Only record the first 20 layers; below irrelevent

  integer :: istart_part, iend_part

  integer, dimension(2,max_grid_zone) :: produce_zone

  integer :: part_spec, part_fac, part_freq, part_duration, &
             npart, nzone, nxy, tpart, new_part, isteps_part, output_freq, &
             y_Astart, m_Astart, d_Astart, y_Aend, m_Aend, d_Aend, n_ind

  integer, allocatable :: ipart(:), jpart(:)

  real, dimension(nbpb,1,nb_back_time) :: xpoback, ypoback, zpoback

  real, dimension(m,n) :: salt2, part_grid_init, flux, fluy

  real :: xpo, ypo, zpo, part_head, phyto_threshold

  real :: xk1, xk2, xk3, xk4, yk1, yk2, yk3, yk4, zk1, zk2, zk3, zk4
  
  real :: ratio_ing

  real, dimension(max_number_part) :: cpart

  logical :: dvm, clone, cell_out

  logical :: part_traj_out, part_ftle_out, part_nc_out, part_prod

  character(len=400) :: partfile, parsfile, outfile

  character(len=250) :: initback

  !---------------------------------------------------------------------
  ! Particle physiology

  integer :: sex, nb_ind

  integer, dimension (2) :: sex_ini

  real, dimension(4) :: C2N
 
  real, dimension(2) :: mean_length_ini, min_length_ini, max_length_ini, aw_ini, bw_ini, ei_ini, er_ini, t_lim_ini, &
                        a_molt_ini, b_molt_ini, k0_ini, k0_phy_ini, k0_zoo_ini, h0_ini, A_ini, & 
                        r0_ini, rb_ini, w_molt_ini, a_rep_ini, b_rep_ini, mass_egg_ini, p_gonad_ini, grow_gonad_ini, prop_mez_ini
  
  real :: mean_length, min_length, max_length, aw, bw, ei, er, t_lim, a_molt, b_molt, k0, k0_phy, k0_zoo, h0, &
          A, r0, rb, w_molt, a_rep, b_rep, mass_egg, p_gonad, grow_gonad, prop_mez
  
  real :: length, mass

  logical :: mature

  !---------------------------------------------------------------------
  ! I/O

  namelist /run_nml/ ystart, mstart, dstart, yend, mend, dend, infile, &
                     infile_food, FORC_PMZA, INTRA_SPE, tnc_origin, & 
                     tnc_origin_out, tnc_units

  namelist /part_nml/ part_spec, clone, cpart, part_fac, part_freq, part_duration, dvm, cell_out, output_freq, mature, &
                      part_traj_out, part_ftle_out, part_nc_out, part_prod, phyto_threshold, &
                      partfile, parsfile, outfile, y_Astart, m_Astart, d_Astart, y_Aend, m_Aend, d_Aend,initback

  namelist /physio_nml/ C2N, sex_ini, mean_length_ini, min_length_ini, max_length_ini, aw_ini, bw_ini, ei_ini,er_ini, t_lim_ini, & 
                        a_molt_ini, b_molt_ini, k0_ini, h0_ini, A_ini, r0_ini, w_molt_ini, & 
                        a_rep_ini, b_rep_ini, mass_egg_ini, p_gonad_ini, grow_gonad_ini, k0_phy_ini, k0_zoo_ini, & 
                        rb_ini, prop_mez_ini

  !--------------------------------------------------------------------

  save

end module part_def
