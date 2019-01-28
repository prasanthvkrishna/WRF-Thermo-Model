module module_param
  ! This module defines all the paramters of DRM model
  implicit none
  integer,parameter 				:: NAN_value=-9.99E8
  real   ,parameter				:: gc = 9.81  		! acceleration due to gravity, m/s2
  real	 ,parameter				:: Rd = 287.0 		! J/K.kg
  real   ,parameter				:: Rv = 461.6 		! J/K.kg
  real   ,parameter				:: Cpd = 7.0*Rd/2.0 	! J/K.kg
  real	 ,parameter				:: Cpv = 4*Rv 		! J/K.kg sp ht of water vapor
  real	 ,parameter				:: Cpl = 4190.0		! J/K.kg
  real	 ,parameter				:: PR  = 100000.0 	! Pa
  real   ,parameter  				:: TR  = 273.16 	! K
  real	 ,parameter				:: ALIQ = 613.3
  real	 ,parameter				:: BLIQ = 17.502
  real	 ,parameter				:: CLIQ = 4780.8
  real	 ,parameter				:: DLIQ = 32.19
  real   ,parameter				:: LVo  = 2.501         ! KJ/Kg
  real   ,parameter				:: Po   = 1.0E5
end module module_param
