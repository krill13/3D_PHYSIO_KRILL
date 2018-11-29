subroutine par_particle

  use part_def

  implicit none

  !------------------------------------------------------------
  ! Parameters values for each species 
 print*,' part_spec', part_spec
  sex         = sex_ini(part_spec) 
  mean_length = mean_length_ini(part_spec)
  min_length  = min_length_ini(part_spec)
  max_length  = min_length_ini(part_spec)
  aw          = aw_ini(part_spec)
  bw          = bw_ini(part_spec)
  ei          = ei_ini(part_spec)
  er          = er_ini(part_spec)
  t_lim       = t_lim_ini(part_spec)
  a_molt      = a_molt_ini(part_spec)
  b_molt      = b_molt_ini(part_spec)
  k0          = k0_ini(part_spec)
  k0_phy      = k0_phy_ini(part_spec)
  k0_zoo      = k0_zoo_ini(part_spec)
  h0          = h0_ini(part_spec)
  A           = A_ini(part_spec)
  r0          = r0_ini(part_spec)
  rb          = rb_ini(part_spec)
  w_molt      = w_molt_ini(part_spec)
  a_rep       = a_rep_ini(part_spec)
  b_rep       = b_rep_ini(part_spec)
  mass_egg    = mass_egg_ini(part_spec)  
  p_gonad     = p_gonad_ini(part_spec)  
  grow_gonad  = grow_gonad_ini(part_spec)
  prop_mez    = prop_mez_ini(part_spec)
  
  print*,  sex, mean_length, aw, ei, k0_phy, rb, mass_egg, p_gonad, grow_gonad, prop_mez 
end subroutine                         
