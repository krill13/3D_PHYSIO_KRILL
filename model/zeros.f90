subroutine zeros

  use part_def

  use ran_mod

  implicit none

  !---------------------------------------------------------------------
  ! 1-D arrays

  dd = 0.

  dz = 0.

  up_depth = 0.

  mid_depth = 0.

  !---------------------------------------------------------------------
  ! 2-D arrays

  dlx = 0.

  dly = 0.

  nlayer = 0

  !---------------------------------------------------------------------
  ! 3-D arrays

  u = 0.0

  v = 0.0

  w = 0.0

  salt = 0.0

  temp = 0.0

  u1 = 0.0

  v1 = 0.0

  w1 = 0.0

  u2 = 0.0

  v2 = 0.0

  w2 = 0.0

  !---------------------------------------------------------------------
  ! Particules data

  xpo = 0.

  ypo = 0.

  zpo = 0.

  xk1 = 0.

  yk1 = 0.

  zk1 = 0.

  xk2 = 0.

  yk2 = 0.

  zk2 = 0.

  xk3 = 0.

  yk3 = 0.

  zk3 = 0.

  xk4 = 0.

  yk4 = 0.

  zk4 = 0.

  dvm = .true.

  part_head = 2*pi*(ran1()-0.5)

  new_part = 0.

  !---------------------------------------------------------------------
  ! Default particules parameter: see part_nml namelist

  part_spec     = 0

  part_fac      = 1

  output_freq   = 1

  part_nc_out   = .false.

  part_traj_out = .false.

  part_prod     = .false.

  mature        = .false.

  partfile      = 'data/part_domain_unif.dat'

  outfile       = '/home/dbenkort/Documents/MODELS/git_krill/NEMO_GSL/trunk/run/output'

  !---------------------------------------------------------------------
  ! Default phyisiological parameters: see physio_nml namelist

  !---------------------------------------------------------------------

  return

end subroutine zeros
