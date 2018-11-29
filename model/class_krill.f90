module class_krill

  implicit none

  ! hide the type-bound procedure implementation procedures
  private

  ! constants used within classes
  real, parameter :: EPS = 10e-6
  real, parameter :: LENGTH_MAX = 50.0


  ! allows acces to the krill class
  public :: Krill
  type Krill
     private

     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
     !! Description of caracteristic of individual krill !!
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !! Parameters of different physiological equation :

     !! Time
     integer :: dt 
     integer :: istart
     integer :: isteps

     !! General
     integer :: sex
     integer :: part_spec

     real :: length
     real :: mass

     !! Molt
     real :: dev_freq
     real :: molt_length

     logical :: mature, quota_molt

     real :: a_molt    
     real :: b_molt    
     real :: w_molt    
     
     !! Grow
     real :: grow_cum
     real :: grow_way
     real :: balance_ind
     real :: cp_mez

     !! Reproduction
     real :: gonad
     real :: max_gonad
     real :: nb_molt

     real :: a_rep     
     real :: b_rep     
     real :: mass_egg  
     real :: p_gonad
     real :: grow_gonad
     real :: nb_egg
     real :: egg_cost

     ! Length / Mass 
     real :: aw        
     real :: bw        

     ! Arrhenius
     real :: ei       
     real :: er 
     real :: t_lim

     ! Ingestion
     real :: A         
     real :: k0        
     real :: k0_phy    
     real :: k0_zoo    
     real :: h0        
     real :: ratio_ing
     real :: prop_mez
 
     ! Respiration
     real :: r0       
     real :: rb

     !! Transport 
     real :: xpo
     real :: ypo
     real :: zpo
     real :: zmig
     real :: opt_depth
     
     logical :: dvm, cell_out, light_day

     !! Advection
     real :: xk1, xk2, xk3, xk4, yk1, yk2, yk3, yk4, zk1, zk2, zk3, zk4

     !! Environment
     real :: tp
     real :: diat
     real :: flag
     real :: micz
     real :: mesz
     real :: max_chlA
     real :: phyto_threshold

   contains

     private

     ! Methods
     procedure :: develop    

     procedure :: arrhenius
     procedure :: adj_arr

     procedure :: enc_k
     procedure :: enc_k_phy
     procedure :: enc_k_zoo
     procedure :: hand
     procedure :: ingest
     procedure :: ingest_phy
     procedure :: ingest_zoo
     procedure :: alpha_hol_phy
     procedure :: alpha_hol_zoo

     procedure :: breath

     procedure :: egg_prod

     procedure, public :: molt
     procedure, public :: grow
     procedure, public :: balance 
     procedure, public :: reproduction
     procedure, public :: debug

     ! getters 
     procedure, public :: get_dt
     procedure, public :: get_istart
     procedure, public :: get_isteps

     procedure, public :: get_sex
     procedure, public :: get_species
     procedure, public :: get_length
     procedure, public :: get_mass

     procedure, public :: get_dev_freq
     procedure, public :: get_molt_length
     procedure, public :: get_mature
     procedure, public :: get_grow_cum
     procedure, public :: get_grow_way
     procedure, public :: get_balance_ind
     procedure, public :: get_cp_mez

     procedure, public :: get_gonad
     procedure, public :: get_max_gonad
     procedure, public :: get_nb_molt

     procedure, public :: get_xpo
     procedure, public :: get_ypo
     procedure, public :: get_zpo
     procedure, public :: get_zmig
     procedure, public :: get_opt_depth

     procedure, public :: get_xk1
     procedure, public :: get_xk2
     procedure, public :: get_xk3
     procedure, public :: get_xk4
     procedure, public :: get_yk1
     procedure, public :: get_yk2
     procedure, public :: get_yk3
     procedure, public :: get_yk4
     procedure, public :: get_zk1
     procedure, public :: get_zk2
     procedure, public :: get_zk3
     procedure, public :: get_zk4

     procedure, public :: get_dvm
     procedure, public :: get_cell_out
     procedure, public :: get_light_day

     procedure, public :: get_aw
     procedure, public :: get_bw
     procedure, public :: get_a_molt
     procedure, public :: get_b_molt
     procedure, public :: get_w_molt
     procedure, public :: get_ei
     procedure, public :: get_er
     procedure, public :: get_t_lim
     procedure, public :: get_A
     procedure, public :: get_k0
     procedure, public :: get_k0_phy
     procedure, public :: get_k0_zoo
     procedure, public :: get_h0
     procedure, public :: get_ratio_ing
     procedure, public :: get_prop_mez
     procedure, public :: get_r0
     procedure, public :: get_rb
     procedure, public :: get_a_rep
     procedure, public :: get_b_rep
     procedure, public :: get_mass_egg
     procedure, public :: get_p_gonad
     procedure, public :: get_grow_gonad
     procedure, public :: get_nb_egg
     procedure, public :: get_egg_cost
    
     procedure, public :: get_tp
     procedure, public :: get_diat
     procedure, public :: get_flag
     procedure, public :: get_micz
     procedure, public :: get_mesz
     procedure, public :: get_max_chlA
     procedure, public :: get_phyto_threshold

     procedure, public :: get_develop

     procedure, public :: get_arr
     procedure, public :: get_adj_arr

     procedure, public :: get_enc
     procedure, public :: get_enc_phy
     procedure, public :: get_enc_zoo
     procedure, public :: get_hand
     procedure, public :: get_ingestion
     procedure, public :: get_ingestion_phy
     procedure, public :: get_ingestion_zoo

     procedure, public :: get_breath

     ! setters
     procedure, public :: set_istart
     procedure, public :: set_isteps

     procedure, public :: set_mass

     procedure, public :: set_zmig
     procedure, public :: set_dvm
     procedure, public :: set_opt_depth
     procedure, public :: set_cell_out
     procedure, public :: set_light_day

     procedure, public :: set_ratio_ing

     procedure, public :: set_xpo
     procedure, public :: set_xk1
     procedure, public :: set_xk2
     procedure, public :: set_xk3
     procedure, public :: set_xk4

     procedure, public :: set_ypo
     procedure, public :: set_yk1
     procedure, public :: set_yk2
     procedure, public :: set_yk3
     procedure, public :: set_yk4

     procedure, public :: set_zpo
     procedure, public :: set_zk1
     procedure, public :: set_zk2
     procedure, public :: set_zk3
     procedure, public :: set_zk4

     procedure, public :: set_tp
     procedure, public :: set_diat
     procedure, public :: set_flag
     procedure, public :: set_micz
     procedure, public :: set_mesz
     procedure, public :: set_max_chlA

     ! Constructor
     procedure, public :: init_krill    ! initialisator for a krill object

  end type Krill

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! develop returns the develop time (intermoult period) in days
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real function develop(this)
    class(Krill) :: this

    develop = 86400. * ( this%a_molt + this%b_molt * this%tp )

    if (develop < 3. * 86400.) then
       develop = 3. * 86400.
    end if

  end function develop


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Arrhenius function of temperature : Universal Temperature Dependance (Gillooly et al. 2001)
  ! Returns a temperature dependance coefficient (unitless)
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real function arrhenius(this,e_act)
    class(Krill)    :: this
    real :: e_act
    real, parameter :: k=8.62e-5       ! The Boltzmann's constant in eV.K-1
    real, parameter :: T0=273.15       ! Reference temperature (the frozing point of water) in K
    arrhenius = exp( ( e_act * this%tp ) / ( k * (this%tp + T0) * T0 ) )
  end function arrhenius


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Arrhenius adjustement function
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real function adj_arr(this, alpha_temp)
    class(Krill)    :: this
    real :: alpha_temp, dtemp

    dtemp    = this%t_lim - this%tp

    if(dtemp > 0) then 
        adj_arr = 1
    else 
        adj_arr  = exp(dtemp * alpha_temp)
    endif

  end function adj_arr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! ingest returns the energy won by ingestion in mg C.s^-1
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real function enc_k(this)
    real,parameter :: alpha = 0.5
  ! Encounter kernel in L. s^-1. Increases with temperature

    class(Krill) :: this
    enc_k = (this%mass**0.75 * this%k0 * (this%arrhenius(this%ei) * this%adj_arr(alpha))) * this%dt

  end function enc_k

!---------------------------------------------------------------------------------------------

  real function enc_k_phy(this)
  ! Encounter kernel in L. s^-1. Increases with temperature
    real,parameter :: alpha = 0.5
    class(Krill) :: this
    
    enc_k_phy = (this%mass**0.75 * this%k0_phy * this%arrhenius(this%ei) * this%adj_arr(alpha))
    
  end function enc_k_phy

!---------------------------------------------------------------------------------------------

  real function enc_k_zoo(this)
  ! Encounter kernel in L. s^-1. Increases with temperature
    real,parameter :: alpha = 0.5
    class(Krill) :: this
    
    enc_k_zoo = (this%mass**0.75 * this%k0_zoo * this%arrhenius(this%ei) * this%adj_arr(alpha))
    
  end function enc_k_zoo

!---------------------------------------------------------------------------------------------

  real function hand(this)
  ! Handling time in s. Decreases with temperature.
    real,parameter :: alpha = 0.2
    class(Krill) :: this

    hand = (this%mass**(-0.75) * this%h0 * this%arrhenius(-1*this%ei) /  this%adj_arr(alpha)) 
    
  end function hand

!---------------------------------------------------------------------------------------------
  real function alpha_hol_phy(this)
    class(Krill) :: this
    real :: phy
    real :: clear_phy

    phy       = this%diat + this%flag + this%micz

    alpha_hol_phy  = (this%enc_k_phy() * phy) / (1 + (this%enc_k_phy() * this%hand() * phy))  


  end function alpha_hol_phy

!---------------------------------------------------------------------------------------------
  real function alpha_hol_zoo(this)
    class(Krill) :: this
    real :: zoo
    real :: clear_zoo

    zoo       = this%mesz

    alpha_hol_zoo = (this%enc_k_zoo() * zoo) / (1 + (this%enc_k_zoo() * this%hand() * zoo)) 

  end function alpha_hol_zoo

!---------------------------------------------------------------------------------------------
  real function ingest_phy(this)
    class(Krill) :: this
    real :: zoo, phy, zz, pp

     phy = this%diat + this%flag + this%micz
     zoo = this%mesz
     pp  = this%alpha_hol_phy() * this%hand() * phy
     zz  = this%alpha_hol_zoo() * this%hand() * zoo
     
     ingest_phy = (this%alpha_hol_phy() * phy) / (1 + (pp  + zz ))

  end function ingest_phy
   
!---------------------------------------------------------------------------------------------
  real function ingest_zoo(this)
    class(Krill) :: this
    real :: zoo, phy, zz, pp

     phy = this%diat + this%flag + this%micz
     zoo = this%mesz
     pp  = this%alpha_hol_phy() * this%hand() * phy
     zz  = this%alpha_hol_zoo() * this%hand() * zoo
     
     ingest_zoo = (this%alpha_hol_zoo() * zoo) / (1 + (pp + zz))

  end function ingest_zoo

!---------------------------------------------------------------------------------------------
  real function ingest(this)
    class(Krill) :: this

     ingest   = (this%ingest_phy() + this%ingest_zoo()) * this%dt

  end function ingest


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! breath returns the energy lost by respiration in mg C.s^-1
  !
  ! Respiration: basal metabolism + activity (swimming) metabolism
  ! but NOT specific dynamic action as it is included in assimilation coeffcient
  ! Data from Angelique
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real function breath(this)
    real,parameter :: alpha = 0.2
    class(Krill) :: this
   
    breath  = this%r0 * (this%mass**this%rb) * (this%arrhenius(this%er) * this%adj_arr(alpha)) * this%dt

  end function breath


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! nb_egg returns the numbers of egg release by the female
  ! egg_prod returns the energy costs by egg produciton in mg C.s^-1
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real function egg_prod(this)
    class(Krill) :: this

       egg_prod = (this%a_rep * (this%length ** this%b_rep)) * this%mass_egg

  end function egg_prod


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! molt computes and updates the development freq of the individual. Also, when the freq hits 0.4 (or above), 
  ! computes the molt_factor which fixes the next length of the individual. Thus, when the freq reaches 1 (and above), 
  ! it updates the individual's length with the previously computed molt_factor.
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine molt(this)
    class(Krill) :: this

    if (this%cell_out .eqv. .true.) then 

        this%dev_freq = 0.0
    
    else 
   
        this%dev_freq = this%dev_freq + (this%dt / this%develop())
        this%nb_molt  = this%nb_molt  + (this%dt / this%develop())

    ! NOTE: moulting is irreversibly engaged after 40% of the IMP.
    ! The new individuals length is completely decided at this time
    ! according to the allometric relationship
        if (this%dev_freq > 0.4 .and. this%molt_length < EPS) then
           this%molt_length = (this%mass / this%aw)** (1 / this%bw)
        end if

    ! At the end of the development phase, the individual molts
        if (this%dev_freq >  1.0) then
           this%mass        = this%mass * (1 - this%w_molt)
           this%length      = this%molt_length
           this%dev_freq    = this%dev_freq - 1.0
           this%molt_length = 0
           if (this%part_spec == 2) then
                this%quota_molt = .true.
                this%nb_molt    = this%nb_molt - 1.0
                this%grow_cum   = 0
           end if  

        end if
        
        if(this%part_spec == 1) then 
                if(this%nb_molt > 2.0) then
                this%quota_molt  = .true. 
                this%nb_molt     = this%nb_molt - 2.0
                this%grow_cum    = 0
                end if
        end if  
   end if 

  end subroutine molt


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! grow computes and updates the new mass of the individual
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine grow(this)
    class(Krill) :: this
    real :: ingest, breath
    
    if (this%cell_out .eqv. .true.) then 
        
        this%mass = this%mass
    else 

        this%grow_way = 0.0
        this%grow_way = (this%A * this%ingest()) - this%breath()

        this%grow_cum = this%grow_cum + (this%A * this%ingest()) - this%breath()

        this%mass     = this%mass + (this%A * this%ingest()) - this%breath()

        this%cp_mez   = (this%cp_mez * this%mass + this%A * (this%ingest_zoo() * this%dt))/ (this%mass + this%A * this%ingest())

        this%p_gonad   = this%gonad / this%mass
        this%max_gonad = 0.1 * this%mass
 
        if (this%grow_way > 0.0) then 
                if (this%mature .eqv. .false.) then
                        this%gonad = this%gonad + (this%grow_gonad * this%grow_way)
   
                        if (this%gonad > this%max_gonad) then
                                this%gonad = this%max_gonad
                        endif
                else 
                        this%gonad = this%gonad
                endif  
        else 
           this%gonad = this%p_gonad * this%mass
        endif
        
        if (this%gonad >= this%max_gonad) then 
                if (this%mature .eqv. .false.) then
                        this%mature = .true.
                endif
        endif 
    endif

  end subroutine grow

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine balance(this)
    class(Krill) :: this 

    this%balance_ind = 0
    this%balance_ind =  (this%A * this%ingest()) - this%breath()

  end subroutine balance

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! reproduction computes and updates the new mass of the individual after an
  ! eggs release event it occurs when the nb_molt reaches 2 (or above), 
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine reproduction(this)
    class(Krill) :: this
    real :: phy

     phy = this%diat + this%flag + this%micz
    
    if (this%cell_out .eqv. .true.) then 
        
        this%mature = .false.

    else 
        this%mature   = this%mature
        this%egg_cost = 0.0
        this%nb_egg   = 0.0

        if(this%quota_molt .eqv. .true.) then 
                if (this%mature .eqv. .true.) then 
                       if(this%max_chlA >= this%phyto_threshold) then            
                               this%egg_cost = this%egg_prod()
                               if(this%egg_cost > this%gonad) then 
                                        this%egg_cost = this%gonad
                               endif 
                               this%nb_egg   = this%egg_cost / this%mass_egg
                               this%mass     = this%mass - this%egg_cost
                               this%gonad    = this%gonad - this%egg_cost
                               if(this%gonad <= 0.0) then 
                                        this%gonad = 0.0
                                        this%mature = .false.
                               endif
                               this%quota_molt = .false.
                        else
                               this%mature = .false.
                        endif 
                endif
        endif 
    endif

  end subroutine reproduction

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! debug prints a representation of the Krill in the console
         !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine debug(this)
    class(Krill) :: this

    write(*, *) "== Krill =="
    select case (this%part_spec)
    case (1)
       write(*, '(T4, A, T17, A)'), "part_spec: ", "M. norvegica"
    case (2)
       write(*, '(T4, A, T17, A)'), "part_spec:", "T. raschii"
    end select
    select case (this%sex)
    case (0)
       write(*, '(T4, A, T17, A)'), "sex: ", "F"
    case (1)
       write(*, '(T4, A, T17, A)'), "sex: ", "M"
    end select
    write(*, '(T4, A, T16, ES10.3)'), "length: ", this%length
    write(*, '(T4, A, T16, ES10.3)'), "mass: ", this%mass
    write(*, '(T4, A, T16, ES10.3)'), "dev_freq: ", this%dev_freq
    write(*, '(T4, A, T16, ES10.3)'), "molt_length: ", this%molt_length
    write(*, '(T4, A, T16, ES10.3)'), "xpo: ", this%xpo
    write(*, '(T4, A, T16, ES10.3)'), "ypo: ", this%ypo
    write(*, '(T4, A, T16, ES10.3)'), "zpo: ", this%zpo
    write(*, '(T4, A, T16, ES10.3)'), "aw: ", this%aw
    write(*, '(T4, A, T16, ES10.3)'), "bw: ", this%bw
    write(*, '(T4, A, T16, ES10.3)'), "ei: ", this%ei
    write(*, '(T4, A, T16, ES10.3)'), "a_molt: ", this%a_molt
    write(*, '(T4, A, T16, ES10.3)'), "b_molt: ", this%b_molt
    !write(*, '(T4, A, T16, ES10.3)'), "k0: ", this%k0
    write(*, '(T4, A, T16, ES10.3)'), "h0: ", this%h0
    write(*, '(T4, A, T16, ES10.3)'), "A: ", this%A
    write(*, '(T4, A, T16, ES10.3)'), "r0: ", this%r0
    !write(*, '(T4, A, T16, ES10.3)'), "pref: ", this%pref
    write(*, '(T4, A, T16, ES10.3/)'), "w_molt: ", this%w_molt
  end subroutine debug


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! getters
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     !! Time variables 
  real function get_dt(this)
    class(Krill) :: this
    get_dt = this%dt
  end function get_dt

  integer function get_istart(this)
    class(Krill) :: this
    get_istart = this%istart
  end function get_istart

  integer function get_isteps(this)
    class(Krill) :: this
    get_isteps = this%isteps
  end function get_isteps

     !! General 
  integer function get_species(this)
    class(Krill) :: this
    get_species = this%part_spec
  end function get_species

  integer function get_sex(this)
    class(Krill) :: this
    get_sex = this%sex
  end function get_sex

  real function get_length(this)
    class(Krill) :: this
    get_length = this%length
  end function get_length

  real function get_mass(this)
    class(Krill) :: this
    get_mass = this%mass
  end function get_mass

     !! Molt process 
  real function get_dev_freq(this)
    class(Krill) :: this
    get_dev_freq = this%dev_freq
  end function get_dev_freq

  real function get_molt_length(this)
    class(Krill) :: this
    get_molt_length = this%molt_length
  end function get_molt_length

  logical function get_mature(this)
    class(Krill) :: this
    get_mature = this%mature
  end function get_mature

     !! Grow process 
  real function get_grow_cum(this)
    class(Krill) :: this
    get_grow_cum = this%grow_cum
  end function get_grow_cum

  real function get_grow_way(this)
    class(Krill) :: this
    get_grow_way = this%grow_way
  end function get_grow_way

  real function get_balance_ind(this)
    class(Krill) :: this
    get_balance_ind = this%balance_ind
  end function get_balance_ind

  real function get_cp_mez(this)
    class(Krill) :: this
    get_cp_mez = this%cp_mez
  end function get_cp_mez

     !! Reproduction process 
  real function get_gonad(this)
    class(Krill) :: this
    get_gonad = this%gonad
  end function get_gonad

  real function get_max_gonad(this)
    class(Krill) :: this
    get_max_gonad = this%max_gonad
  end function get_max_gonad

  real function get_nb_molt(this)
    class(Krill) :: this
    get_nb_molt = this%nb_molt
  end function get_nb_molt

  real function get_nb_egg(this)
    class(Krill) :: this
    get_nb_egg = this%nb_egg
  end function get_nb_egg

  real function get_egg_cost(this)
    class(Krill) :: this
    get_egg_cost = this%egg_cost
  end function get_egg_cost

     !! Transport process 
  real function get_xpo(this)
    class(Krill) :: this
    get_xpo = this%xpo
  end function get_xpo

  real function get_ypo(this)
    class(Krill) :: this
    get_ypo = this%ypo
  end function get_ypo

  real function get_zpo(this)
    class(Krill) :: this
    get_zpo = this%zpo
  end function get_zpo

  real function get_zmig(this)
    class(Krill) :: this
    get_zmig = this%zmig
  end function get_zmig

  real function get_opt_depth(this)
    class(Krill) :: this
    get_opt_depth = this%opt_depth
  end function get_opt_depth


     !! Krill advection process
  real function get_xk1(this)
    class(Krill) :: this
    get_xk1 = this%xk1
  end function get_xk1

  real function get_xk2(this)
    class(Krill) :: this
    get_xk2 = this%xk2
  end function get_xk2

  real function get_xk3(this)
    class(Krill) :: this
    get_xk3 = this%xk3
  end function get_xk3

  real function get_xk4(this)
    class(Krill) :: this
    get_xk4 = this%xk4
  end function get_xk4

  real function get_yk1(this)
    class(Krill) :: this
    get_yk1 = this%yk1
  end function get_yk1

  real function get_yk2(this)
    class(Krill) :: this
    get_yk2 = this%yk2
  end function get_yk2

  real function get_yk3(this)
    class(Krill) :: this
    get_yk3 = this%yk3
  end function get_yk3

  real function get_yk4(this)
    class(Krill) :: this
    get_yk4 = this%yk4
  end function get_yk4

  real function get_zk1(this)
    class(Krill) :: this
    get_zk1 = this%zk1
  end function get_zk1

  real function get_zk2(this)
    class(Krill) :: this
    get_zk2 = this%zk2
  end function get_zk2

  real function get_zk3(this)
    class(Krill) :: this
    get_zk3 = this%zk3
  end function get_zk3

  real function get_zk4(this)
    class(Krill) :: this
    get_zk4 = this%zk4
  end function get_zk4

     !! Krill migration behaviour process 
  logical function get_dvm(this)
    class(Krill) :: this
    get_dvm = this%dvm
  end function get_dvm

  logical function get_cell_out(this)
    class(Krill) :: this
    get_cell_out = this%cell_out
  end function get_cell_out

  logical function get_light_day(this)
    class(Krill) :: this
    get_light_day = this%light_day
  end function get_light_day

     !! Obtain parameters of different physiological equation :
     ! Length / Mass relationship 
  real function get_aw(this)
    class(Krill) :: this
    get_aw = this%aw
  end function get_aw

  real function get_bw(this)
    class(Krill) :: this
    get_bw = this%bw
  end function get_bw

     ! Molt process
  real function get_a_molt(this)
    class(Krill) :: this
    get_a_molt = this%a_molt
  end function get_a_molt

  real function get_b_molt(this)
    class(Krill) :: this
    get_b_molt = this%b_molt
  end function get_b_molt

  real function get_w_molt(this)
    class(Krill) :: this
    get_w_molt = this%w_molt
  end function get_w_molt

     ! Arrhenius relationship
  real function get_ei(this)
    class(Krill) :: this
    get_ei = this%ei
  end function get_ei

  real function get_er(this)
    class(Krill) :: this
    get_er = this%er
  end function get_er

  real function get_t_lim(this)
    class(Krill) :: this
    get_t_lim = this%t_lim
  end function get_t_lim

     ! Ingestion process
  real function get_A(this)
    class(Krill) :: this
    get_A = this%A
  end function get_A

  real function get_k0(this)
    class(Krill) :: this
    get_k0 = this%k0
  end function get_k0

  real function get_k0_phy(this)
    class(Krill) :: this
    get_k0_phy = this%k0_phy
  end function get_k0_phy

  real function get_k0_zoo(this)
    class(Krill) :: this
    get_k0_zoo = this%k0_zoo
  end function get_k0_zoo

  real function get_h0(this)
    class(Krill) :: this
    get_h0 = this%h0
  end function get_h0

  real function get_ratio_ing(this)
    class(Krill) :: this
    get_ratio_ing = this%ratio_ing
  end function get_ratio_ing

  real function get_prop_mez(this)
    class(Krill) :: this
    get_prop_mez = this%prop_mez
  end function get_prop_mez

     ! Respiration process
  real function get_r0(this)
    class(Krill) :: this
    get_r0= this%r0
  end function get_r0

  real function get_rb(this)
    class(Krill) :: this
    get_rb= this%rb
  end function get_rb

     ! Reproduction process
  real function get_a_rep(this)
    class(Krill) :: this
    get_a_rep = this%a_rep
  end function get_a_rep

  real function get_b_rep(this)
    class(Krill) :: this
    get_b_rep = this%b_rep
  end function get_b_rep

  real function get_mass_egg(this)
    class(Krill) :: this
    get_mass_egg = this%mass_egg
  end function get_mass_egg

  real function get_p_gonad(this)
    class(Krill) :: this
    get_p_gonad = this%p_gonad
  end function get_p_gonad

  real function get_grow_gonad(this)
    class(Krill) :: this
    get_grow_gonad = this%grow_gonad
  end function get_grow_gonad

     ! Environment variables
  real function get_tp(this)
    class(Krill) :: this
    get_tp = this%tp
  end function get_tp

  real function get_diat(this)
    class(Krill) :: this
    get_diat = this%diat
  end function get_diat

  real function get_flag(this)
    class(Krill) :: this
    get_flag = this%flag
  end function get_flag

  real function get_micz(this)
    class(Krill) :: this
    get_micz = this%micz
  end function get_micz

  real function get_mesz(this)
    class(Krill) :: this
    get_mesz = this%mesz
  end function get_mesz

  real function get_max_chlA(this)
    class(Krill) :: this
    get_max_chlA = this%max_chlA
  end function get_max_chlA

  real function get_phyto_threshold(this)
    class(Krill) :: this
    get_phyto_threshold = this%phyto_threshold
  end function get_phyto_threshold

     ! Physiological functions
  real function get_develop(this)
    class(Krill) :: this
    get_develop = this%develop()
  end function get_develop

  real function get_arr(this)
    class(Krill) :: this
    real :: e_act
    get_arr = this%arrhenius(e_act)
  end function get_arr

  real function get_adj_arr(this)
    class(Krill) :: this
    real :: alpha_temp
    get_adj_arr = this%adj_arr(alpha_temp)
  end function get_adj_arr

  real function get_enc(this)
    class(Krill) :: this
    get_enc = this%enc_k()
  end function get_enc

  real function get_enc_phy(this)
    class(Krill) :: this
    get_enc_phy = this%enc_k_phy()
  end function get_enc_phy

  real function get_enc_zoo(this)
    class(Krill) :: this
    get_enc_zoo = this%enc_k_zoo()
  end function get_enc_zoo

  real function get_hand(this)
    class(Krill) :: this
    get_hand = this%hand()
  end function get_hand

  real function get_ingestion(this)
    class(Krill) :: this
    get_ingestion = this%ingest()
  end function get_ingestion

  real function get_ingestion_phy(this)
    class(Krill) :: this
    get_ingestion_phy = this%ingest_phy()
  end function get_ingestion_phy

  real function get_ingestion_zoo(this)
    class(Krill) :: this
    get_ingestion_zoo = this%ingest_zoo()
  end function get_ingestion_zoo

  real function get_breath(this)
    class(Krill) :: this
    get_breath = this%breath()
  end function get_breath


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! setters
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine set_istart(this, istart)
    class(Krill) :: this
    integer :: istart
    this%istart = istart
  end subroutine set_istart

  subroutine set_isteps(this, isteps)
    class(Krill) :: this
    integer :: isteps
    this%isteps = isteps
  end subroutine set_isteps

  subroutine set_mass(this, mass)
    class(Krill) :: this
    real :: mass
    this%mass = mass
  end subroutine set_mass

  subroutine set_zmig(this, zmig)
    class(Krill) :: this
    real :: zmig
    this%zmig = zmig
  end subroutine set_zmig

  subroutine set_dvm(this, dvm)
    class(Krill) :: this
    logical :: dvm
    this%dvm = dvm
  end subroutine set_dvm

  subroutine set_opt_depth(this, opt_depth)
    class(Krill) :: this
    real :: opt_depth
    this%opt_depth = opt_depth
  end subroutine set_opt_depth

  subroutine set_cell_out(this, cell_out)
    class(Krill) :: this
    logical :: cell_out
    this%cell_out = cell_out
  end subroutine set_cell_out

  subroutine set_light_day(this, light_day)
    class(Krill) :: this
    logical :: light_day
    this%light_day = light_day
  end subroutine set_light_day

  subroutine set_ratio_ing(this, ratio_ing)
    class(Krill) :: this
    real :: ratio_ing
    this%ratio_ing = ratio_ing
  end subroutine set_ratio_ing

  subroutine set_xpo(this, x)
    class(Krill) :: this
    real :: x
    this%xpo = x
  end subroutine set_xpo

  subroutine set_xk1(this, xk1)
    class(Krill) :: this
    real :: xk1
    this%xk1 = xk1
  end subroutine set_xk1

  subroutine set_xk2(this, xk2)
    class(Krill) :: this
    real :: xk2
    this%xk2 = xk2
  end subroutine set_xk2

  subroutine set_xk3(this, xk3)
    class(Krill) :: this
    real :: xk3
    this%xk3 = xk3
  end subroutine set_xk3

  subroutine set_xk4(this, xk4)
    class(Krill) :: this
    real :: xk4
    this%xk4 = xk4
  end subroutine set_xk4

  subroutine set_ypo(this, y)
    class(Krill) :: this
    real :: y
    this%ypo = y
  end subroutine set_ypo

  subroutine set_yk1(this, yk1)
    class(Krill) :: this
    real :: yk1
    this%yk1 = yk1
  end subroutine set_yk1

  subroutine set_yk2(this, yk2)
    class(Krill) :: this
    real :: yk2
    this%yk2 = yk2
  end subroutine set_yk2

  subroutine set_yk3(this, yk3)
    class(Krill) :: this
    real :: yk3
    this%yk3 = yk3
  end subroutine set_yk3

  subroutine set_yk4(this, yk4)
    class(Krill) :: this
    real :: yk4
    this%yk4 = yk4
  end subroutine set_yk4

  subroutine set_zpo(this, z)
    class(Krill) :: this
    real :: z
    this%zpo = z
  end subroutine set_zpo

  subroutine set_zk1(this, zk1)
    class(Krill) :: this
    real :: zk1
    this%zk1 = zk1
  end subroutine set_zk1

  subroutine set_zk2(this, zk2)
    class(Krill) :: this
    real :: zk2
    this%zk2 = zk2
  end subroutine set_zk2

  subroutine set_zk3(this, zk3)
    class(Krill) :: this
    real :: zk3
    this%zk3 = zk3
  end subroutine set_zk3

  subroutine set_zk4(this, zk4)
    class(Krill) :: this
    real :: zk4
    this%zk4 = zk4
  end subroutine set_zk4

  subroutine set_tp(this, tp)
    class(Krill) :: this
    real :: tp
    this%tp = tp
  end subroutine set_tp

  subroutine set_diat(this, diat)
    class(Krill) :: this
    real :: diat
    this%diat = diat
  end subroutine set_diat

  subroutine set_flag(this, flag)
    class(Krill) :: this
    real :: flag
    this%flag = flag
  end subroutine set_flag

  subroutine set_micz(this, micz)
    class(Krill) :: this
    real :: micz
    this%micz = micz
  end subroutine set_micz

  subroutine set_mesz(this, mesz)
    class(Krill) :: this
    real :: mesz
    this%mesz = mesz
  end subroutine set_mesz

  subroutine set_max_chlA(this, max_chlA)
    class(Krill) :: this
    real :: max_chlA
    this%max_chlA = max_chlA
  end subroutine set_max_chlA

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! new_krill initializes a new Krill object
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef PHYSIO

  subroutine init_krill(this, dt, istart, isteps, sex, part_spec, length, mature, xpo, ypo, zpo, zmig, &
                        xk1, xk2, xk3, xk4, yk1, yk2, yk3, yk4, zk1, zk2, zk3, zk4, dvm, cell_out, light_day, &
                        aw, bw, a_molt, b_molt, w_molt, ei, er, t_lim, A, k0, k0_phy, k0_zoo, h0, ratio_ing, r0, rb, &
                        a_rep, b_rep, mass_egg, p_gonad, grow_gonad, tp, diat, flag, micz, mesz, max_chlA,& 
                        phyto_threshold, prop_mez)

    ! creating a krill object

    class(Krill) :: this

     !! Time variable
     integer :: dt 
     integer :: istart
     integer :: isteps

     !! General
     integer :: sex
     integer :: part_spec

     real :: length

     !! Molt process
     logical :: mature, quota_molt

     !! Transport process 
     real :: xpo
     real :: ypo
     real :: zpo
     real :: zmig
     
     !! Krill advection variables
     real :: xk1, xk2, xk3, xk4, yk1, yk2, yk3, yk4, zk1, zk2, zk3, zk4

     !! Krill migration behaviour
     logical :: dvm, cell_out, light_day

     !! Parameters of different physiological equation :
     ! Length / Mass relationship 
     real :: aw        
     real :: bw        

     ! Molt process
     real :: a_molt    
     real :: b_molt    
     real :: w_molt    
     
     ! Arrhenius relationship
     real :: ei        
     real :: er
     real :: t_lim

     ! Ingestion process
     real :: A         
     real :: k0        
     real :: k0_phy    
     real :: k0_zoo    
     real :: h0        
     real :: prop_mez
 
     ! Respiration process
     real :: r0       
     real :: rb

     ! Reproduction process
     real :: a_rep     
     real :: b_rep     
     real :: mass_egg  
     real :: p_gonad
     real :: grow_gonad
     real :: ratio_ing

     !! Environment variables
     real :: tp
     real :: diat
     real :: flag
     real :: micz
     real :: mesz
     real :: max_chlA
     real :: phyto_threshold

    ! preconditions
    if (sex /= 0 .and. sex /= 1) then
       stop 'SEX_ERROR'
    endif

    if (length < 0.0 .and. length > LENGTH_MAX) then
       stop 'LENGTH_ERROR'
    endif


    ! Parameters for allometric relationship, arrhenius,
    ! ingestion, respiration, development and moult equations
    
    this%aw = aw
    this%bw = bw

    this%a_molt = a_molt
    this%b_molt = b_molt
    this%w_molt = w_molt
    
    this%ei = ei
    this%er = er
    this%t_lim = t_lim

    this%A         = A
    this%k0        = k0
    this%k0_phy    = k0_phy
    this%k0_zoo    = k0_zoo
    this%h0        = h0
    this%ratio_ing = ratio_ing
    this%prop_mez  = prop_mez

    this%r0 = r0
    this%rb = rb

    this%a_rep      = a_rep
    this%b_rep      = b_rep
    this%mass_egg   = mass_egg
    this%p_gonad    = p_gonad
    this%grow_gonad = grow_gonad

    ! giving values to attributes

    this%dt     = dt
    this%istart = istart
    this%isteps = isteps

    this%sex       = sex
    this%part_spec = part_spec
    this%length    = length
    this%mass      = this%aw * (this%length ** this%bw) 
    this%cp_mez    = this%prop_mez + this%mass*0

    this%dev_freq    = 0.0
    this%molt_length = 0.0
    this%mature      = mature 
    this%quota_molt  = .false.

    this%grow_cum  = 0.0
    this%grow_way  = 0.0

    this%max_gonad = 0.1 * this%mass
    this%gonad     = 0.1 * this%mass * this%grow_gonad
    this%nb_molt   = 0.0
    this%nb_egg    = 0.0
    this%egg_cost  = 0.0
 
    this%xpo  = xpo
    this%ypo  = ypo
    this%zpo  = zpo
    this%zmig = zmig

    this%xk1 = xk1
    this%xk2 = xk2
    this%xk3 = xk3
    this%xk4 = xk4
    this%yk1 = yk1
    this%yk2 = yk2
    this%yk3 = yk3
    this%yk4 = yk4
    this%zk1 = zk1
    this%zk2 = zk2
    this%zk3 = zk3
    this%zk4 = zk4

    this%dvm       = dvm
    this%cell_out  = cell_out
    this%light_day = light_day


    ! Environmental forcing

    this%tp   = tp
    this%diat = diat
    this%flag = flag
    this%micz = micz
    this%mesz = mesz
    this%max_chlA = max_chlA
    this%phyto_threshold = phyto_threshold

  end subroutine init_krill

#else

  subroutine init_krill(this, dt, istart, isteps, sex, part_spec, xpo, ypo, zpo, zmig, &
                        xk1, xk2, xk3, xk4, yk1, yk2, yk3, yk4, zk1, zk2, zk3, zk4, dvm, cell_out, light_day)

    ! creating a krill object

    class(Krill) :: this

     !! Time variable
     integer :: dt 
     integer :: istart
     integer :: isteps

     !! General
     integer :: sex
     integer :: part_spec

     !! Transport process 
     real :: xpo
     real :: ypo
     real :: zpo
     real :: zmig
     
     !! Krill advection variables
     real :: xk1, xk2, xk3, xk4, yk1, yk2, yk3, yk4, zk1, zk2, zk3, zk4

     !! Krill migration behaviour
     logical :: dvm, cell_out, light_day

    ! giving values to attributes

    this%dt     = dt
    this%istart = istart
    this%isteps = isteps

    this%sex       = sex
    this%part_spec = part_spec
 
    this%xpo  = xpo
    this%ypo  = ypo
    this%zpo  = zpo
    this%zmig = zmig

    this%xk1 = xk1
    this%xk2 = xk2
    this%xk3 = xk3
    this%xk4 = xk4
    this%yk1 = yk1
    this%yk2 = yk2
    this%yk3 = yk3
    this%yk4 = yk4
    this%zk1 = zk1
    this%zk2 = zk2
    this%zk3 = zk3
    this%zk4 = zk4

    this%dvm       = dvm
    this%cell_out  = cell_out
    this%light_day = light_day


  end subroutine init_krill

#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module class_krill
