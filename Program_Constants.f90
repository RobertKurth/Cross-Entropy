module Program_Constants
    implicit none
    private
    real*8, public, parameter   ::  Tref_CISCC = 288.75D+00             !   SNL crack growth model for CISCC, K
    real*8, public, parameter   ::  Tref_ASME = 313.15D+00              !   SNL crack growth model for CISCC, K
    real*8, public, parameter   ::  alpha_ASME = 1.25D+00               !   SNL crack growth model for CISCC, mm / year
    
    real*8, public, parameter   ::  Sec_per_year = 31557600D+00
    integer, public, parameter  ::  Number_of_crack_outputs = 10
    integer, public, parameter  ::  number_of_inputs = 12
    integer, public, parameter  ::  Maximum_cycles = 10000000
    integer, public, parameter  ::  number_of_random_variables = 15
    integer, public, parameter  ::  Number_of_distribution_types = 5
    integer, public, parameter  ::  number_of_steps = 50
    integer, public, parameter  ::  Increment_da = 2500                  ! Emc J-R burst mewthod
    real*8, public, parameter   ::  G_weight = 1.0D+00
    logical, public, parameter  ::  Plane_stress = .true.
    integer, public, parameter  ::  NData = 26
   
    real*8, public, parameter   ::  OD_t_ref = 60.0D+00
    real*8, public, parameter   ::  E = 3.0D+04
    real*8, public, parameter   ::	Poisson_ratio = 3.0D-001
    real*8, public, parameter   ::	OD_over_t_Ref = 60.0D-000
    real*8, public, parameter   ::	n_ref = 11.278D+00
    real*8, public, parameter   ::	s0_ref = 53.13D+00
    real*8, public, parameter   ::	thickness_ref = 0.2667D+00
    logical, public, parameter  ::	FullSize = .true.
    logical, public, parameter  ::	Inside = .true.
    
    real*8, public, parameter   ::	Delta_Length = 0.25D+00             ! Used for J exceeds toughness calculation s
    real*8, public, parameter   ::	Thickness_transition = 0.95D+00
    
    real*8, public, parameter   ::  API_579_a = 0.961697697D+00
    real*8, public, parameter   ::  API_579_b = -2.09343676D+00
    real*8, public, parameter   ::  API_579_c = -0.63894811D+00
    real*8, public, parameter   ::  API_579_d = -0.04622649D+00
    real*8, public, parameter   ::  API_579_e = 0.043419375D+00
    real*8, public, parameter   ::  API_579_f = 1.130828102D+00
    real*8, public, parameter   ::  API_579_g = -0.08364952D+00
    real*8, public, parameter   ::  API_579_h = 1.246354412D+00
    real*8, public, parameter   ::  API_579_i = 1.373425213D+00
    real*8, public, parameter   ::  API_579_j = 0.307510752D+00
    real*8, public, parameter   ::  API_579_k = -0.26317107D+00
    real*8, public, parameter   ::  Sum_ai = 0.02D+00
    real*8, public, parameter   ::  Delta_a = 0.04D+00
    
    real*8, public, parameter   ::  Number_of_model_parameters = 50
    ! 1 to 50 Emc2 FE model parameters
    real*8, public, parameter   ::  Model_parameter_1 = 0.192461908580593D+00
    real*8, public, parameter   ::  Model_parameter_2 = 1.255D+00
    real*8, public, parameter   ::  Model_parameter_3 = 0.0135D+00
    !NB ECF is a constant only for fized pipe dimensions
		!real*8, public, parameter   ::  Model_parameter_4 = ECFD+00
    real*8, public, parameter   ::  Model_parameter_5 = 0.961697697D+00
    real*8, public, parameter   ::  Model_parameter_6 = 0.63894811D+00
    real*8, public, parameter   ::  Model_parameter_7 = 0.043419375D+00
    real*8, public, parameter   ::  Model_parameter_8 = 0.08364952D+00
    real*8, public, parameter   ::  Model_parameter_9 = 1.373425213D+00
    real*8, public, parameter   ::  Model_parameter_10 = 0.26317107D+00
    real*8, public, parameter   ::  Model_parameter_11 = 2.09343676D+00
    real*8, public, parameter   ::  Model_parameter_12 = 0.04622649D+00
    real*8, public, parameter   ::  Model_parameter_13 = 1.130828102D+00
    real*8, public, parameter   ::  Model_parameter_14 = 1.246354412D+00
    real*8, public, parameter   ::  Model_parameter_15 = 0.307510752D+00
    real*8, public, parameter   ::  Model_parameter_16 = 1.341D+00
    real*8, public, parameter   ::  Model_parameter_17 = -0.027274D+00
    real*8, public, parameter   ::  Model_parameter_18 = -0.31374D+00
    real*8, public, parameter   ::  Model_parameter_19 = -0.61538D+00
    real*8, public, parameter   ::  Model_parameter_20 = 0.76923D+00
    real*8, public, parameter   ::  Model_parameter_21 = 1.6154D+00
    real*8, public, parameter   ::  Model_parameter_22 = -0.46154D+00
    real*8, public, parameter   ::  Model_parameter_23 = 0.76923D+00
    real*8, public, parameter   ::  Model_parameter_24 = -0.46154D+00
    real*8, public, parameter   ::  Model_parameter_25 = -0.05576583D+00
    real*8, public, parameter   ::  Model_parameter_26 = -0.53017732D+00
    real*8, public, parameter   ::  Model_parameter_27 = -0.37521595D+00
    real*8, public, parameter   ::  Model_parameter_28 = -0.45197098D+00
    real*8, public, parameter   ::  Model_parameter_29 = 0.120330221D+00
    real*8, public, parameter   ::  Model_parameter_30 = -0.01079015D+00
    real*8, public, parameter   ::  Model_parameter_31 = -0.5514605D+00
    real*8, public, parameter   ::  Model_parameter_32 = -5.37060454D+00
    real*8, public, parameter   ::  Model_parameter_33 = -5.82539297D+00
    real*8, public, parameter   ::  Model_parameter_34 = 0.014659216D+00
    real*8, public, parameter   ::  Model_parameter_35 = -11.515D+00
    real*8, public, parameter   ::  Model_parameter_36 = 88.14D+00
    real*8, public, parameter   ::  Model_parameter_37 = -173.84D+00
    real*8, public, parameter   ::  Model_parameter_38 = 118.02D+00
    real*8, public, parameter   ::  Model_parameter_39 = 8.5589D+00
    real*8, public, parameter   ::  Model_parameter_40 = -85.841D+00
    real*8, public, parameter   ::  Model_parameter_41 = 180.26D+00
    real*8, public, parameter   ::  Model_parameter_42 = -127.4D+00
    real*8, public, parameter   ::  Model_parameter_43 = 0.028875D+00
    real*8, public, parameter   ::  Model_parameter_44 = -0.22604D+00
    real*8, public, parameter   ::  Model_parameter_45 = 0.60247D+00
    real*8, public, parameter   ::  Model_parameter_46 = -0.46408D+00
    real*8, public, parameter   ::  Model_parameter_47 = 0.4038D+00
    real*8, public, parameter   ::  Model_parameter_48 = 2.5594D+00
    real*8, public, parameter   ::  Model_parameter_49 = -5.1912D+00
    real*8, public, parameter   ::  Model_parameter_50 = 3.3796D+00

		real*8, public, parameter   ::	Zero = 0.D+00
    real*8, public, parameter   ::	One_half = 0.5D+00
    real*8, public, parameter   ::	Two_thirds = 2.0D+00 / 3.0D+00
    real*8, public, parameter   ::	One = 1.0D+00
    real*8, public, parameter   ::	Two = 2.0D+00
    real*8, public, parameter   ::	Three = 3.0D+00
    real*8, public, parameter   ::	Four = 4.0D+00
    real*8, public, parameter   ::	Five = 5.0D+00
    real*8, public, parameter   ::	Six = 6.0D+00
    real*8, public, parameter   ::	Seven = 7.0D+00
    real*8, public, parameter   ::	Eight = 8.0D+00
    real*8, public, parameter   ::	Nine = 9.0D+00
    real*8, public, parameter   ::	Ten = 1.0D+01
    real*8, public, parameter   ::	Hundred = 1.0D+02
    real*8, public, parameter   ::	Thousand = 1.0D+03
    real*8, public, parameter   ::	pi = 3.14159265358979D+00
    
    real*8, public, parameter   ::	Molar_mass_H2 = 1.0074
    real*8, public, parameter   ::	Gravitional_constant = 32.174048695D+00         !   lbf_sec^2 / lbm_ft
    real*8, public, parameter   ::	Universal_Gas_constant = 8.31446261815324D+00   !   J⋅K−1⋅mol−1             
                                                                                    !   m3⋅Pa⋅K−1⋅mol−1
                                                                                    !   kg⋅m2⋅s−2⋅K−1⋅mol−1
    !
    !   Ratio of specific heats
    !                           https://www.engineeringtoolbox.com/specific_heat_ratio_d_608.html
    !
    real*8, public, parameter   :: R_specific_heat_Acetylene = 1.3D+00
    real*8, public, parameter   :: R_specific_heat_Air = 1.4D+00
    real*8, public, parameter   :: R_specific_heat_Ammonia = 1.32D+00
    real*8, public, parameter   :: R_specific_heat_Argon = 1.66D+00
    real*8, public, parameter   :: R_specific_heat_Benzene = 1.12D+00
    real*8, public, parameter   :: R_specific_heat_N_butane = 1.18D+00
    real*8, public, parameter   :: R_specific_heat_Iso_butane = 1.19D+00
    real*8, public, parameter   :: R_specific_heat_Carbon_Dioxide = 1.28D+00
    real*8, public, parameter   :: R_specific_heat_Carbon_Disulphide = 1.21D+00
    real*8, public, parameter   :: R_specific_heat_Carbon_Monoxide = 1.4D+00
    real*8, public, parameter   :: R_specific_heat_Chlorine = 1.33D+00
    real*8, public, parameter   :: R_specific_heat_Ethane = 1.18D+00
    real*8, public, parameter   :: R_specific_heat_Ethyl_alcohol = 1.13D+00
    real*8, public, parameter   :: R_specific_heat_Ethyl_chloride = 1.19D+00
    real*8, public, parameter   :: R_specific_heat_Ethylene = 1.24D+00
    real*8, public, parameter   :: R_specific_heat_Helium = 1.66D+00
    real*8, public, parameter   :: R_specific_heat_N_heptane = 1.05D+00
    real*8, public, parameter   :: R_specific_heat_Hexane = 1.06D+00
    real*8, public, parameter   :: R_specific_heat_Hydrochloric_acid = 1.41D+00
    real*8, public, parameter   :: R_specific_heat_Hydrogen = 1.41D+00
    real*8, public, parameter   :: R_specific_heat_Hydrogen_chloride = 1.41D+00
    real*8, public, parameter   :: R_specific_heat_Hydrogen_sulphide = 1.32D+00
    real*8, public, parameter   :: R_specific_heat_Methane = 1.32D+00
    real*8, public, parameter   :: R_specific_heat_Methyl_alcohol = 1.2D+00
    real*8, public, parameter   :: R_specific_heat_Methyl_butane = 1.08D+00
    real*8, public, parameter   :: R_specific_heat_Methyl_chloride = 1.2D+00
    real*8, public, parameter   :: R_specific_heat_Natural_Gas_Methane = 1.32D+00
    real*8, public, parameter   :: R_specific_heat_Nitric_oxide = 1.4D+00
    real*8, public, parameter   :: R_specific_heat_Nitrogen = 1.4D+00
    real*8, public, parameter   :: R_specific_heat_Nitrous_oxide = 1.31D+00
    real*8, public, parameter   :: R_specific_heat_N_octane = 1.05D+00
    real*8, public, parameter   :: R_specific_heat_Oxygen = 1.4D+00
    real*8, public, parameter   :: R_specific_heat_N_pentane = 1.08D+00
    real*8, public, parameter   :: R_specific_heat_Iso_pentane = 1.08D+00
    real*8, public, parameter   :: R_specific_heat_Propane = 1.13D+00
    real*8, public, parameter   :: R_specific_heat_R_11 = 1.14D+00
    real*8, public, parameter   :: R_specific_heat_R_12 = 1.14D+00
    real*8, public, parameter   :: R_specific_heat_R_22 = 1.18D+00
    real*8, public, parameter   :: R_specific_heat_R_114 = 1.09D+00
    real*8, public, parameter   :: R_specific_heat_R_123 = 1.1D+00
    real*8, public, parameter   :: R_specific_heat_R_134a = 1.2D+00
    real*8, public, parameter   :: R_specific_heat_Steam_water = 1.33D+00
    real*8, public, parameter   :: R_specific_heat_Sulphur_dioxide = 1.26D+00
    real*8, public, parameter   :: R_specific_heat_Toulene = 1.09D+00
    !
    !   gas properties
    !                   https://www.engineeringtoolbox.com/specific-heat-capacity-gases-d_159.html
    !
    real*8, public, parameter   :: Acetone_cP = 1.47D+00
    real*8, public, parameter   :: Acetone_cV = 1.32D+00
    real*8, public, parameter   :: Acetone_gamma = 1.11D+00
    real*8, public, parameter   :: Acetone_cP_minus_cV = 0.15D+00
    real*8, public, parameter   :: Acetylene_cP = 1.69D+00
    real*8, public, parameter   :: Acetylene_cV = 1.37D+00
    real*8, public, parameter   :: Acetylene_gamma = 1.232D+00
    real*8, public, parameter   :: Acetylene_cP_minus_cV = 0.319D+00
    real*8, public, parameter   :: Air_cP = 1.01D+00
    real*8, public, parameter   :: Air_cV = 0.718D+00
    real*8, public, parameter   :: Air_gamma = 1.4D+00
    real*8, public, parameter   :: Air_cP_minus_cV = 0.287D+00
    real*8, public, parameter   :: Alcohol_ethanol_cP = 1.88D+00
    real*8, public, parameter   :: Alcohol_ethanol_cV = 1.67D+00
    real*8, public, parameter   :: Alcohol_ethanol_gamma = 1.13D+00
    real*8, public, parameter   :: Alcohol_ethanol_cP_minus_cV = 0.22D+00
    real*8, public, parameter   :: Alcohol_methanol_cP = 1.93D+00
    real*8, public, parameter   :: Alcohol_methanol_cV = 1.53D+00
    real*8, public, parameter   :: Alcohol_methanol_gamma = 1.26D+00
    real*8, public, parameter   :: Alcohol_methanol_cP_minus_cV = 0.39D+00
    real*8, public, parameter   :: Ammonia_cP = 2.19D+00
    real*8, public, parameter   :: Ammonia_cV = 1.66D+00
    real*8, public, parameter   :: Ammonia_gamma = 1.31D+00
    real*8, public, parameter   :: Ammonia_cP_minus_cV = 0.53D+00
    real*8, public, parameter   :: Argon_cP = 0.52D+00
    real*8, public, parameter   :: Argon_cV = 0.312D+00
    real*8, public, parameter   :: Argon_gamma = 1.667D+00
    real*8, public, parameter   :: Argon_cP_minus_cV = 0.208D+00
    real*8, public, parameter   :: Benzene_cP = 1.09D+00
    real*8, public, parameter   :: Benzene_cV = 0.99D+00
    real*8, public, parameter   :: Benzene_gamma = 1.12D+00
    real*8, public, parameter   :: Benzene_cP_minus_cV = 0.1D+00
    real*8, public, parameter   :: Blast_furnace_gas_cP = 1.03D+00
    real*8, public, parameter   :: Blast_furnace_gas_cV = 0.73D+00
    real*8, public, parameter   :: Blast_furnace_gas_gamma = 1.41D+00
    real*8, public, parameter   :: Blast_furnace_gas_cP_minus_cV = 0.3D+00
    real*8, public, parameter   :: Bromine_cP = 0.25D+00
    real*8, public, parameter   :: Bromine_cV = 0.2D+00
    real*8, public, parameter   :: Bromine_gamma = 1.28D+00
    real*8, public, parameter   :: Bromine_cP_minus_cV = 0.05D+00
    real*8, public, parameter   :: Butane_cP = 1.67D+00
    real*8, public, parameter   :: Butane_cV = 1.53D+00
    real*8, public, parameter   :: Butane_gamma = 1.094D+00
    real*8, public, parameter   :: Butane_cP_minus_cV = 0.143D+00
    real*8, public, parameter   :: Carbon_dioxide_cP = 0.844D+00
    real*8, public, parameter   :: Carbon_dioxide_cV = 0.655D+00
    real*8, public, parameter   :: Carbon_dioxide_gamma = 1.289D+00
    real*8, public, parameter   :: Carbon_dioxide_cP_minus_cV = 0.189D+00
    real*8, public, parameter   :: Carbon_monoxide_cP = 1.02D+00
    real*8, public, parameter   :: Carbon_monoxide_cV = 0.72D+00
    real*8, public, parameter   :: Carbon_monoxide_gamma = 1.4D+00
    real*8, public, parameter   :: Carbon_monoxide_cP_minus_cV = 0.297D+00
    real*8, public, parameter   :: Carbon_disulphide_cP = 0.67D+00
    real*8, public, parameter   :: Carbon_disulphide_cV = 0.55D+00
    real*8, public, parameter   :: Carbon_disulphide_gamma = 1.21D+00
    real*8, public, parameter   :: Carbon_disulphide_cP_minus_cV = 0.12D+00
    real*8, public, parameter   :: Chlorine_cP = 0.48D+00
    real*8, public, parameter   :: Chlorine_cV = 0.36D+00
    real*8, public, parameter   :: Chlorine_gamma = 1.34D+00
    real*8, public, parameter   :: Chlorine_cP_minus_cV = 0.12D+00
    real*8, public, parameter   :: Chloroform_cP = 0.63D+00
    real*8, public, parameter   :: Chloroform_cV = 0.55D+00
    real*8, public, parameter   :: Chloroform_gamma = 1.15D+00
    real*8, public, parameter   :: Chloroform_cP_minus_cV = 0.08D+00
    real*8, public, parameter   :: Coal_gas_cP = 2.14D+00
    real*8, public, parameter   :: Coal_gas_cV = 1.59D+00
!    Coal_gas_gamma value not in refrence
!    Coal_gas_cP_minus_cV value not in refrence
    real*8, public, parameter   :: Combustion_products_cP = 1D+00
!    Combustion_products_cV value not in refrence
!    Combustion_products_gamma value not in refrence
!    Combustion_products_cP_minus_cV value not in refrence
    real*8, public, parameter   :: Ethane_cP = 1.75D+00
    real*8, public, parameter   :: Ethane_cV = 1.48D+00
    real*8, public, parameter   :: Ethane_gamma = 1.187D+00
    real*8, public, parameter   :: Ethane_cP_minus_cV = 0.276D+00
    real*8, public, parameter   :: Ether_diethyl_ether_cP = 2.01D+00
    real*8, public, parameter   :: Ether_diethyl_ether_cV = 1.95D+00
    real*8, public, parameter   :: Ether_diethyl_ether_gamma = 1.03D+00
    real*8, public, parameter   :: Ether_diethyl_ether_cP_minus_cV = 0.06D+00
    real*8, public, parameter   :: Ethylene_cP = 1.53D+00
    real*8, public, parameter   :: Ethylene_cV = 1.23D+00
    real*8, public, parameter   :: Ethylene_gamma = 1.24D+00
    real*8, public, parameter   :: Ethylene_cP_minus_cV = 0.296D+00
!    Chlorodifluoromethane_R_22_cP value not in refrence
!    Chlorodifluoromethane_R_22_cV value not in refrence
    real*8, public, parameter   :: Chlorodifluoromethane_R_22_gamma = 1.18D+00
!    Chlorodifluoromethane_R_22_cP_minus_cV value not in refrence
    real*8, public, parameter   :: Helium_cP = 5.19D+00
    real*8, public, parameter   :: Helium_cV = 3.12D+00
    real*8, public, parameter   :: Helium_gamma = 1.667D+00
    real*8, public, parameter   :: Helium_cP_minus_cV = 2.08D+00
!    Hexane_cP value not in refrence
!    Hexane_cV value not in refrence
    real*8, public, parameter   :: Hexane_gamma = 1.06D+00
!    Hexane_cP_minus_cV value not in refrence
    real*8, public, parameter   :: Hydrochloric_acid_cP = 0.795D+00
    real*8, public, parameter   :: Hydrochloric_acid_cV = 0.567D+00
!    Hydrochloric_acid_gamma value not in refrence
!    Hydrochloric_acid_cP_minus_cV value not in refrence
    real*8, public, parameter   :: Hydrogen_cP = 14.32D+00
    real*8, public, parameter   :: Hydrogen_cV = 10.16D+00
    real*8, public, parameter   :: Hydrogen_gamma = 1.405D+00
    real*8, public, parameter   :: Hydrogen_cP_minus_cV = 4.12D+00
    real*8, public, parameter   :: Hydrogen_Chloride_cP = 0.8D+00
    real*8, public, parameter   :: Hydrogen_Chloride_cV = 0.57D+00
    real*8, public, parameter   :: Hydrogen_Chloride_gamma = 1.41D+00
    real*8, public, parameter   :: Hydrogen_Chloride_cP_minus_cV = 0.23D+00
!    Hydrogen_Sulfide_cP value not in refrence
!    Hydrogen_Sulfide_cV value not in refrence
    real*8, public, parameter   :: Hydrogen_Sulfide_gamma = 1.32D+00
!    Hydrogen_Sulfide_cP_minus_cV value not in refrence
    real*8, public, parameter   :: Hydroxyl_cP = 1.76D+00
    real*8, public, parameter   :: Hydroxyl_cV = 1.27D+00
    real*8, public, parameter   :: Hydroxyl_gamma = 1.384D+00
    real*8, public, parameter   :: Hydroxyl_cP_minus_cV = 0.489D+00
    real*8, public, parameter   :: Krypton_cP = 0.25D+00
    real*8, public, parameter   :: Krypton_cV = 0.151D+00
!    Krypton_gamma value not in refrence
!    Krypton_cP_minus_cV value not in refrence
    real*8, public, parameter   :: Methane_cP = 2.22D+00
    real*8, public, parameter   :: Methane_cV = 1.7D+00
    real*8, public, parameter   :: Methane_gamma = 1.304D+00
    real*8, public, parameter   :: Methane_cP_minus_cV = 0.518D+00
!    Methyl_Chloride_cP value not in refrence
!    Methyl_Chloride_cV value not in refrence
    real*8, public, parameter   :: Methyl_Chloride_gamma = 1.2D+00
!    Methyl_Chloride_cP_minus_cV value not in refrence
    real*8, public, parameter   :: Natural_Gas_cP = 2.34D+00
    real*8, public, parameter   :: Natural_Gas_cV = 1.85D+00
    real*8, public, parameter   :: Natural_Gas_gamma = 1.27D+00
    real*8, public, parameter   :: Natural_Gas_cP_minus_cV = 0.5D+00
    real*8, public, parameter   :: Neon_cP = 1.03D+00
    real*8, public, parameter   :: Neon_cV = 0.618D+00
    real*8, public, parameter   :: Neon_gamma = 1.667D+00
    real*8, public, parameter   :: Neon_cP_minus_cV = 0.412D+00
    real*8, public, parameter   :: Nitric_Oxide_cP = 0.995D+00
    real*8, public, parameter   :: Nitric_Oxide_cV = 0.718D+00
    real*8, public, parameter   :: Nitric_Oxide_gamma = 1.386D+00
    real*8, public, parameter   :: Nitric_Oxide_cP_minus_cV = 0.277D+00
    real*8, public, parameter   :: Nitrogen_cP = 1.04D+00
    real*8, public, parameter   :: Nitrogen_cV = 0.743D+00
    real*8, public, parameter   :: Nitrogen_gamma = 1.4D+00
    real*8, public, parameter   :: Nitrogen_cP_minus_cV = 0.297D+00
    real*8, public, parameter   :: Nitrogen_tetroxide_cP = 4.69D+00
    real*8, public, parameter   :: Nitrogen_tetroxide_cV = 4.6D+00
    real*8, public, parameter   :: Nitrogen_tetroxide_gamma = 1.02D+00
    real*8, public, parameter   :: Nitrogen_tetroxide_cP_minus_cV = 0.09D+00
    real*8, public, parameter   :: Nitrous_oxide_cP = 0.88D+00
    real*8, public, parameter   :: Nitrous_oxide_cV = 0.69D+00
    real*8, public, parameter   :: Nitrous_oxide_gamma = 1.27D+00
    real*8, public, parameter   :: Nitrous_oxide_cP_minus_cV = 0.18D+00
    real*8, public, parameter   :: Oxygen_cP = 0.919D+00
    real*8, public, parameter   :: Oxygen_cV = 0.659D+00
    real*8, public, parameter   :: Oxygen_gamma = 1.395D+00
    real*8, public, parameter   :: Oxygen_cP_minus_cV = 0.26D+00
!    Pentane_cP value not in refrence
!    Pentane_cV value not in refrence
    real*8, public, parameter   :: Pentane_gamma = 1.07D+00
!    Pentane_cP_minus_cV value not in refrence
    real*8, public, parameter   :: Propane_cP = 1.67D+00
    real*8, public, parameter   :: Propane_cV = 1.48D+00
    real*8, public, parameter   :: Propane_gamma = 1.13D+00
    real*8, public, parameter   :: Propane_cP_minus_cV = 0.189D+00
    real*8, public, parameter   :: Propene_propylene_cP = 1.5D+00
    real*8, public, parameter   :: Propene_propylene_cV = 1.31D+00
    real*8, public, parameter   :: Propene_propylene_gamma = 1.15D+00
    real*8, public, parameter   :: Propene_propylene_cP_minus_cV = 0.18D+00
!    Water_Vapor_cP value not in refrence
!    Water_Vapor_cV value not in refrence
!    Water_Vapor_gamma value not in refrence
!    Water_Vapor_cP_minus_cV value not in refrence
    real*8, public, parameter   :: Steam_1psia_120_600oF_cP = 1.93D+00
    real*8, public, parameter   :: Steam_1psia_120_600oF_cV = 1.46D+00
    real*8, public, parameter   :: Steam_1psia_120_600oF_gamma = 1.32D+00
    real*8, public, parameter   :: Steam_1psia_120_600oF_cP_minus_cV = 0.462D+00
    real*8, public, parameter   :: Steam_1atm_220_600_oF_cP = 1.97D+00
    real*8, public, parameter   :: Steam_1atm_220_600_oF_cV = 1.5D+00
    real*8, public, parameter   :: Steam_1atm_220_600_oF_gamma = 1.31D+00
    real*8, public, parameter   :: Steam_1atm_220_600_oF_cP_minus_cV = 0.46D+00
    real*8, public, parameter   :: Steam_150psia_360_600oF_cP = 2.26D+00
    real*8, public, parameter   :: Steam_150psia_360_600oF_cV = 1.76D+00
    real*8, public, parameter   :: Steam_150psia_360_600oF_gamma = 1.28D+00
    real*8, public, parameter   :: Steam_150psia_360_600oF_cP_minus_cV = 0.5D+00
    real*8, public, parameter   :: Sulfurdioxide_Sulphurdioxide_cP = 0.64D+00
    real*8, public, parameter   :: Sulfurdioxide_Sulphurdioxide_cV = 0.51D+00
    real*8, public, parameter   :: Sulfurdioxide_Sulphurdioxide_gamma = 1.29D+00
    real*8, public, parameter   :: Sulfurdioxide_Sulphurdioxide_cP_minus_cV = 0.13D+00
    real*8, public, parameter   :: Xenon_cP = 0.16D+00
    real*8, public, parameter   :: Xenon_cV = 0.097D+00
        
end module Program_Constants