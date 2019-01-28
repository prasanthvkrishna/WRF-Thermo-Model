module convert_0

!#########################################################################

!THIS MODULE IMPLEMENTS CONVERSION BETWEEN MASS FRACTIONS AND MOLE FRACTIONS
!AND PROVIDES A CONVERSION FROM PRACTICAL SALINITY TO ABSOLUTE SALINITY

!#########################################################################

!IMPLEMENTATION IN FORTRAN BY D.G. WRIGHT
!FOR PUBLICATION IN OCEAN SCIENCE, AS DESCRIBED IN THE PAPERS

!FEISTEL, R., WRIGHT, D.G., JACKETT, D.R., MIYAGAWA, K., REISSMANN, J.H.,
!WAGNER, W., OVERHOFF, U., GUDER, C., FEISTEL, A., MARION, G.M.:
!NUMERICAL IMPLEMENTATION AND OCEANOGRAPHIC APPLICATION OF THE THERMODYNAMIC
!POTENTIALS OF WATER, VAPOUR, ICE, SEAWATER AND AIR. PART I: BACKGROUND AND EQUATIONS. 
!OCEAN SCIENCES, 2009, IN PREPARATION.

!WRIGHT, D.G., FEISTEL, R., JACKETT, D.R., MIYAGAWA, K., REISSMANN, J.H., 
!WAGNER, W., OVERHOFF, U., GUDER, C., FEISTEL, A., MARION, G.M.:
!NUMERICAL IMPLEMENTATION AND OCEANOGRAPHIC APPLICATION OF THE THERMODYNAMIC
!POTENTIALS OF WATER, VAPOUR, ICE, SEAWATER AND AIR. PART II: THE LIBRARY ROUTINES, 
!OCEAN SCIENCES., 2009, IN PREPARATION.

!FEISTEL, R., KRETZSCHMAR, H.-J., SPAN, R., HAGEN, E., !WRIGHT, D.G., JACKETT, D.R.:
!THERMODYNAMIC PROPERTIES OF SEA AIR.
!OCEAN SCIENCE DISCUSSION 6(2009)2193-2325.

!#########################################################################

!THIS MODULE REQUIRES THE LIBRARY MODULES
!     CONSTANTS_0, FILE CONSTANTS_0.F90

!#########################################################################

use constants_0

implicit none
private

character*11, private :: version = '31 Jan 2012'

public :: air_massfraction_air_si, air_massfraction_vap_si, &
          air_molar_mass_si, air_molfraction_air_si, &
          air_molfraction_vap_si, asal_from_psal, psal_from_asal

real(dbl) ::  mw = molar_mass_h2o_si      !MOLAR MASS OF H2O IN KG/MOL
real(dbl) ::  ma = molar_mass_air_L2000   !MOLAR MASS OF AIR IN KG/MOL USED BY LEMMON et al. 2000
integer :: flag_saar



contains

!==========================================================================
function air_molar_mass_si(a_si)
!==========================================================================

!OUTPUT:  
!air_molar_mass_si = MOLAR MASS OF HUMID AIR IN KG MOL-1

!INPUT:  
!A_SI = AIR MASS FRACTION, IN KG/KG

!CHECK VALUE: air_molar_mass_si(0.5) = 2.22122197774E-02

real(dbl) :: air_molar_mass_si, a_si

air_molar_mass_si = errorreturn

if (a_si < 0d0 .or. a_si > 1d0) return

air_molar_mass_si = 1d0 / ((1d0 - a_si) / mw + a_si / ma)

return
end function

!==========================================================================
function air_molfraction_vap_si(a_si)
!==========================================================================

!OUTPUT: 
!MOLE FRACTION OF VAPOUR IN HUMID AIR IN MOL MOL-1

!INPUT: 
!A_SI = AIR MASS FRACTION, IN KG/KG

!CHECK VALUE: air_molfraction_vap_si(0.5) = 0.616483190186

real(dbl) :: air_molfraction_vap_si, a_si

air_molfraction_vap_si = errorreturn

if (a_si < 0d0 .or. a_si > 1d0) return

air_molfraction_vap_si = (1d0 - a_si) / (1d0 - a_si * (1d0 - mw / ma))

return
end function

!==========================================================================
function air_molfraction_air_si(a_si)
!==========================================================================

!OUTPUT: 
!MOLE FRACTION OF DRY AIR IN HUMID AIR IN MOL MOL-1

!INPUT: 
!A_SI = AIR MASS FRACTION, IN KG/KG

!CHECK VALUE: air_molfraction_air_si(0.5) = 0.383516809814

real(dbl) :: air_molfraction_air_si, a_si

air_molfraction_air_si = errorreturn

if (a_si < 0d0 .or. a_si > 1d0) return

air_molfraction_air_si = (a_si * mw / ma) / (1d0 - a_si * (1d0 - mw / ma))

return
end function

!==========================================================================
function air_massfraction_air_si(x_si)
!==========================================================================

!OUTPUT: 
!A_SI = MASS FRACTION OF AIR IN HUMID AIR IN KG/KG

!INPUT: 
!X_SI = MOLE FRACTION OF AIR IN HUMID AIR IN MOL/MOL

!CHECK VALUE: air_massfraction_air_si(0.5) = 0.616483190186

real(dbl):: air_massfraction_air_si, x_si

air_massfraction_air_si = errorreturn

if (x_si < 0d0 .or. x_si > 1d0) return

air_massfraction_air_si = x_si / (x_si + (1d0 - x_si) * mw / ma)

return
end function

!==========================================================================
function air_massfraction_vap_si(x_si)
!==========================================================================

!OUTPUT: 
!AIR_MASSFRACTION_VAP_SI = MASS FRACTION OF VAPOUR IN HUMID AIR IN KG/KG

!INPUT: 
!X_SI = DRY AIR MOLE FRACTION, IN MOL MOL-1

!CHECK VALUE: air_massfraction_vap_si(0.5) = 0.383516809814

real(dbl):: air_massfraction_vap_si, x_si

air_massfraction_vap_si = errorreturn

if (x_si < 0d0 .or. x_si > 1d0) return

air_massfraction_vap_si = (1d0 - x_si) / (1d0 - x_si * (1d0 - ma / mw))

return
end function

!=====================================================
function asal_from_psal(sp,lon0,lat0,p_si)
!=====================================================

! CONVERT PRACTICAL SALINITY TO ABSOLUTE SALINITY 
!
! SP                  : PRACTICAL SALINITY                 [PSU]
! P_SI                : ABSOLUTE PRESSURE                  [PA]
! LON0                : LONGITUDE                          [DEG E]     
! LAT0                : LATITUDE                           [DEG N]
!
! RESULT              : ABSOLUTE SALINITY                  [KG/KG]
!
!CHECK VALUE:
!ASAL_FROM_PSAL(0.0357, 201, -21, 101325+1023d4) = 0.035873322343341172
!ASAL_FROM_PSAL(0.035, 180, 40, 101325 + 2d7) =   0.035189093288995807  !NORTH PACIFIC SILICATE
!ASAL_FROM_PSAL(0.008, 20, 57, 101325) =     0.0081048377142857131      !CENTRAL BALTIC SEA

implicit none

real(dbl) :: sa0, sp, lon0, lat0, p_si, p0, asal_from_psal

p0 = (p_si - 101325d0)/1d4
sa0 = gsw_sa_from_sp(sp,p0,lon0,lat0)
if (sa0.eq.9d15) then
   asal_from_psal = errorreturn 
else
   asal_from_psal = sa0*1d-3
end if

return
end function

!=====================================================
function psal_from_asal(sa_si,lon0,lat0,p_si)
!=====================================================

! CONVERT ABSOLUTE SALINITY TO PRACTICAL SALINITY 
!
! SA_SI               : ABSOLUTE SALINITY                  [KG/KG]
! P_SI                : ABSOLUTE PRESSURE                  [PA]
! LON0                : LONGITUDE                          [DEG E]     
! LAT0                : LATITUDE                           [DEG N]
!
! PSAL_FROM_ASAL  : PRACTICAL SALINITY                     [PSU]

!CHECK VALUE
!PSAL_FROM_ASAL(0.035873322343341172, 201, -21, 101325+1023d4) =   0.035700000000000010

implicit none

real(dbl) :: sa_si, sa0, lon0, lat0, p_si, p0, psal_from_asal

p0 = (p_si - 101325d0)/1d4
sa0 = sa_si*1d3
psal_from_asal = gsw_sp_from_sa(sa0,p0,lon0,lat0)
if (psal_from_asal.eq.9d15) then
   psal_from_asal = errorreturn
end if

return
end function

!=====================================================
! include GSW Absolute Salinity Algorithm
!=====================================================

! NEEDED FUNCTIONS:

! gsw_sa_from_sp          - Absolute Salinity from Practical Salinity
! gsw_sp_from_sa          - Practical Salinity from Absolute Salinity
! gsw_saar                - Absolute Salinity Anomaly Ratio (excluding the Baltic Sea)
! gsw_sa_from_sp_baltic   - Absolute Salinity Anomaly from Practical Salinity in the Baltic Sea
! gsw_sp_from_sa_baltic   - Practical Salinity from Absolute Salinity in the Baltic Sea
! xinterp1                - Linearly interpolate a real array   

! NEEDED SUBROUTINES:
! indx                    - Finds the index of the value in a monotonically increasing array
! gsw_add_barrier         - Adds a barrier through Central America (Panama) and then averages
!                           over the appropriate side of the barrier
! gsw_add_mean            - Replaces NaN's with non-nan mean of the 4 adjacent neighbours

! TAKEN FROM:

!==========================================================================
! Gibbs SeaWater (GSW) Oceanographic Toolbox of TEOS-10 version 3.0 (Fortran)
!==========================================================================

!==========================================================================
function gsw_sa_from_sp(sp,p,long,lat)       
!==========================================================================

! Calculates Absolute Salinity, SA, from Practical Salinity, SP
!
! sp     : Practical Salinity                              [unitless]
! p      : sea pressure                                    [dbar]
! long   : longitude                                       [DEG E]     
! lat    : latitude                                        [DEG N]
!
! gsw_sa_from_sp   : Absolute Salinity                     [g/kg]

implicit none

real(dbl) :: sp, long, lat, p, gsw_sa_from_sp, saar
real(dbl) :: gsw_sa_baltic

saar = gsw_saar(p,long,lat)

gsw_sa_from_sp = (35.16504d0/35.d0)*sp*(1.d0 + saar)

gsw_sa_baltic = gsw_sa_from_sp_baltic(sp,long,lat)

if (gsw_sa_baltic.lt.1d10) then
   gsw_sa_from_sp = gsw_sa_baltic
end if

if (saar.eq.9d15) then
   gsw_sa_from_sp = 9d15
end if

return
end function

!--------------------------------------------------------------------------

!==========================================================================
function gsw_sp_from_sa(sa,p,long,lat) 
!==========================================================================

! Calculates Practical salinity, sp, from Absolute salinity, sa  
!
! sa     : Absolute Salinity                               [g/kg]
! p      : sea pressure                                    [dbar]
! long   : longitude                                       [DEG E]     
! lat    : latitude                                        [DEG N]
!
! gsw_sp_from_sa      : Practical Salinity                 [unitless]

implicit none

real(dbl) :: sa, long, lat, p, gsw_sp_from_sa, saar
real(dbl) :: gsw_sp_baltic

saar = gsw_saar(p,long,lat)

gsw_sp_from_sa = (35.d0/35.16504d0)*sa/(1d0 + saar)

gsw_sp_baltic = gsw_sp_from_sa_baltic(sa,long,lat);

if (gsw_sp_baltic.lt.1d10) then
   gsw_sp_from_sa = gsw_sp_baltic
end if

if (saar.eq.9d15) then
   gsw_sp_from_sa = 9d15
end if

return
end function

!--------------------------------------------------------------------------

!==========================================================================
function gsw_saar(p,long,lat)
!==========================================================================

! Calculates the Absolute Salinity Anomaly Ratio, SAAR.
!
! p      : sea pressure                                    [dbar]
! long   : longitude                                       [deg E]     
! lat    : latitude                                        [deg N]
!
! gsw_saar : Absolute Salinity Anomaly Ratio               [unitless]

implicit none

integer, parameter :: nx=91, ny=45, nz=45

integer :: indx0, indy0, indz0, i, j, icalled, k
integer :: nmean, flag_saar
integer, dimension(4) :: deli, delj

real(dbl), dimension(4) :: saar, saar_old
real(dbl), dimension(nz) :: p_ref
real(dbl), dimension(ny) :: lats_ref
real(dbl), dimension(nx) :: longs_ref
real(dbl), dimension(ny,nx) :: ndepth_ref 
real(dbl), dimension(nz,ny,nx) :: saar_ref
real(dbl) :: p, lat, long, dlong, dlat
real(dbl) :: gsw_saar, p0_original, lon0_in, sa_upper, sa_lower 
real(dbl) :: r1, s1, t1, saar_mean, ndepth_max

data deli/0,1,1,0/, delj/0,0,1,1/

data icalled/0/, flag_saar/0/

save icalled, flag_saar, longs_ref, lats_ref, p_ref, ndepth_ref, saar_ref

gsw_saar = 9d15

if(lat .lt. -86d0 .or. lat .gt. 90d0) then
 gsw_saar = 9d15
 return
end if

if(long .lt. 0) then
 long = long + 360
end if

if(icalled.eq.0) then
  icalled = 1
   open(10,file='GSW_Data_v3_0.dat',status='old',err=1)
   flag_saar = 1
   read(10,*) (longs_ref(i), i=1,nx)
   read(10,*) (lats_ref(i), i=1,ny)
   read(10,*) (p_ref(i), i=1,nz)
   read(10,*) ((ndepth_ref(j,i), j=1,ny), i=1,nx)
   read(10,*) (((saar_ref(k,j,i), k=1,nz), j=1,ny), i=1,nx)
   close(10)
   go to 2
1  saar_ref = 9d15
   flag_saar = 0
2  continue
end if

if (flag_saar.eq.0) then
   write(*,*) "*** GSW_Data_v3_0.dat is missing !!! ***"
   write(*,*) "Set the full path of GSW_Data_v3_0.dat in function gsw_saar of module Convert_0!"
end if

!Set gsw_saar = 9d15 and return if there is no data file present
if(flag_saar .eq. 0) then
 gsw_saar = 9d15
 return
endif

dlong = longs_ref(2)-longs_ref(1)
dlat = lats_ref(2)-lats_ref(1)

indx0 = floor(1 + (nx-1)*(long-longs_ref(1))/(longs_ref(nx)-longs_ref(1)))
if(indx0.eq.nx) then
   indx0 = nx-1
end if

indy0 = floor(1 + (ny-1)*(lat-lats_ref(1))/(lats_ref(ny)-lats_ref(1)))
if(indy0.eq.ny) then
   indy0 = ny-1
end if

ndepth_max = -1
do k = 1,4
   if(ndepth_ref(indy0+delj(k),indx0+deli(k)).gt.0.d0) then
      ndepth_max = max(ndepth_max,ndepth_ref(indy0+delj(k),indx0+deli(k)))
   end if
end do

if(ndepth_max.eq.-1.d0) then
  gsw_saar = 0d0 
   return
end if 

p0_original = p
if(p.gt.p_ref(int(ndepth_max))) then
 p = p_ref(int(ndepth_max))
end if
call indx(p_ref,nz,p,indz0)
    
r1 = (long-longs_ref(indx0))/(longs_ref(indx0+1)-longs_ref(indx0));
s1 = (lat-lats_ref(indy0))/(lats_ref(indy0+1)-lats_ref(indy0));
t1 = (p-p_ref(indz0))/(p_ref(indz0+1)-p_ref(indz0));

do k = 1,4
   saar(k) = saar_ref(indz0,indy0+delj(k),indx0+deli(k))
end do

if(260.d0.le.long.and.long.le.291.999d0.and.3.4d0.le.lat.and.lat.le.19.55d0) then
  saar_old = saar
  call gsw_add_barrier(saar_old,long,lat,longs_ref(indx0),lats_ref(indy0),dlong,dlat,saar)
else if(abs(sum(saar)) .ge. 1d10) then
  saar_old = saar
  call gsw_add_mean(saar_old,long,lat,saar)
end if

sa_upper = (1.d0-s1)*(saar(1) + r1*(saar(2)-saar(1))) + s1*(saar(4) + r1*(saar(3)-saar(4)))

do k = 1,4
   saar(k) = saar_ref(indz0+1,indy0+delj(k),indx0+deli(k))
end do

if(260.d0.le.long.and.long.le.291.999d0.and.3.4d0.le.lat.and.lat.le.19.55d0) then
   saar_old = saar
   call gsw_add_barrier(saar_old,long,lat,longs_ref(indx0),lats_ref(indy0),dlong,dlat,saar)
else if(abs(sum(saar)) .ge. 1d10) then 
   saar_old = saar
   call gsw_add_mean(saar_old,long,lat,saar)
end if

sa_lower = (1.d0-s1)*(saar(1) + r1*(saar(2)-saar(1))) + s1*(saar(4) + r1*(saar(3)-saar(4)))
if(abs(sa_lower) .ge. 1d10) then
  sa_lower = sa_upper
end if
gsw_saar = sa_upper + t1*(sa_lower-sa_upper)

if(abs(gsw_saar).ge.1d10) then
   gsw_saar = 9d15
endif

p = p0_original
  
return
end function

!--------------------------------------------------------------------------

!==========================================================================
function gsw_sa_from_sp_baltic(sp,long,lat)
!==========================================================================

! For the Baltic Sea, calculates Absolute Salinity with a value
! computed analytically from Practical Salinity
!
! sp     : Practical Salinity                              [unitless]
! long   : longitude                                       [deg E]     
! lat    : latitude                                        [deg N]
! p      : sea pressure                                    [dbar]
!
! gsw_sa_from_sp_baltic : Absolute Salinity                [g/kg]                         

implicit none

real(dbl), dimension(2) :: xb_right, yb_right
real(dbl), dimension(3) :: xb_left, yb_left
real(dbl) :: sp, long, lat, gsw_sa_from_sp_baltic, xx_left, xx_right

data xb_left/12.6d0, 7.d0, 26.d0/, yb_left/50.d0, 59.d0, 69.d0/
data xb_right/45.d0, 26.d0/, yb_right/50.d0, 69.d0/

if(xb_left(2).lt.long .and. long.lt.xb_right(1) .and. yb_left(1).lt.lat .and. lat.lt.yb_left(3)) then
  
    xx_left = xinterp1(yb_left, xb_left, 3, lat)
    
    xx_right = xinterp1(yb_right, xb_right, 2, lat)
    
    if(xx_left.le.long .and. long.le.xx_right) then
        gsw_sa_from_sp_baltic =((35.16504d0 - 0.087d0)/35d0)*sp + 0.087d0
    else
        gsw_sa_from_sp_baltic = 9d15
    end if

else
    gsw_sa_from_sp_baltic = 9d15
end if

return
end function

!--------------------------------------------------------------------------

!==========================================================================
function gsw_sp_from_sa_baltic(sa,long,lat)
!==========================================================================

! For the Baltic Sea, calculates Practical Salinity with a value
! computed analytically from Absolute Salinity
!
! sa     : Absolute Salinity                               [g/kg]
! long   : longitude                                       [deg E]     
! lat    : latitude                                        [deg N]
! p      : sea pressure                                    [dbar]
!
! gsw_sp_from_sa_baltic  : Practical Salinity              [unitless]

implicit none

real(dbl), dimension(2) :: xb_right, yb_right
real(dbl), dimension(3) :: xb_left, yb_left
real(dbl) :: sa, long, lat, gsw_sp_from_sa_baltic, xx_left, xx_right

data xb_left/12.6d0, 7.d0, 26.d0/, yb_left/50.d0, 59.d0, 69.d0/
data xb_right/45.d0, 26.d0/, yb_right/50.d0, 69.d0/

if(xb_left(2).lt.long .and. long.lt.xb_right(1) .and. yb_left(1).lt.lat .and. lat.lt.yb_left(3)) then
  
    xx_left = xinterp1(yb_left, xb_left, 3, lat)
    
    xx_right = xinterp1(yb_right, xb_right, 2, lat)
    
    if(xx_left.le.long .and. long.le.xx_right) then
        gsw_sp_from_sa_baltic = (35.d0/(35.16504d0 - 0.087d0))*(sa - 0.087d0)
    else
        gsw_sp_from_sa_baltic = 9d15
    end if
     
else
    gsw_sp_from_sa_baltic = 9d15
end if

return
end function

!--------------------------------------------------------------------------

!==========================================================================
subroutine indx(x,n,z,k)
!==========================================================================

!  Finds the index of the value in a monotonically increasing array
!
!  x	 :  array of monotonically increasing values
!  n     :  length of the array
!  z     :  value to be indexed
!
!  K      : index K - if X(K) <= Z < X(K+1), or
!  N-1     		    - if Z = X(N)
!

integer :: n, k, ku, kl, km

real(dbl), dimension(n) :: x
real(dbl) :: z

if(z.gt.x(1).and.z.lt.x(n)) then
   kl=1
   ku=n
   do while (ku-kl.gt.1)
      km=0.5d0*(ku+kl)
      if(z.gt.x(km)) then
         kl=km
      else
         ku=km
      endif
   end do
   k=kl
   if(z.eq.x(k+1)) then 
     k = k+1
   end if
elseif (z.le.x(1)) then
      k = 1
elseif (z.ge.x(n)) then
      k = n-1
else
      write(*,*) 'ERROR in subroutine indx : out of range'
      write(*,*) 'z = ', z, 'n = ', n, 'x = ',x
end if

return
end subroutine

!--------------------------------------------------------------------------

!==========================================================================
subroutine gsw_add_barrier(input_data,long,lat,long_grid,lat_grid,dlong_grid,dlat_grid,output_data)
!==========================================================================

!  Adds a barrier through Central America (Panama) and then averages
!  over the appropriate side of the barrier
! 
!  data_in      :  data                                                     [unitless]
!  long         :  Longitudes of data in decimal degrees east               [ 0 ... +360 ]
!  lat          :  Latitudes of data in decimal degrees north               [ -90 ... +90 ]
!  longs_grid   :  Longitudes of regular grid in decimal degrees east       [ 0 ... +360 ]
!  lats_grid    :  Latitudes of regular grid in decimal degrees north       [ -90 ... +90 ]
!  dlongs_grid  :  Longitude difference of regular grid in decimal degrees  [ deg longitude ]
!  dlats_grid   :  Latitude difference of regular grid in decimal degrees   [ deg latitude ]
!
! gsw_add_barrier  : average of data depending on which side of the 
!                    Panama cannal it is on                                 [unitless]

implicit none

integer, dimension(4) :: above_line
integer k, nmean, above_line0, kk
real(dbl), dimension(4) :: input_data, output_data
real(dbl), dimension(6) :: longs_pan, lats_pan
real(dbl) :: long, lat, r, lats_line, long_grid, lat_grid
real(dbl) :: dlong_grid, dlat_grid, data_mean

data longs_pan/260.0000d0, 272.5900d0, 276.5000d0, 278.6500d0, 280.7300d0, 292.000d0/ 
data  lats_pan/ 19.5500d0,  13.9700d0,   9.6000d0,   8.1000d0,   9.3300d0,   3.400d0/ 

call indx(longs_pan,6,long,k)                            !   the long/lat point
r = (long-longs_pan(k))/(longs_pan(k+1)-longs_pan(k))
lats_line = lats_pan(k) + r*(lats_pan(k+1)-lats_pan(k))

if(lats_line.le.lat) then
   above_line0 = 1
else
   above_line0 = 0
end if

call indx(longs_pan,6,long_grid,k)                                     !  the 1 and 4 long/lat points 
r = (long_grid-longs_pan(k))/(longs_pan(k+1)-longs_pan(k))
lats_line = lats_pan(k) + r*(lats_pan(k+1)-lats_pan(k))

if(lats_line.le.lat_grid) then
   above_line(1) = 1
else
   above_line(1) = 0
end if

if(lats_line.le.lat_grid+dlat_grid) then
   above_line(4) = 1
else
   above_line(4) = 0
end if

call indx(longs_pan,6,long_grid+dlong_grid,k)                              !  the 2 and 3 long/lat points 
r = (long_grid+dlong_grid-longs_pan(k))/(longs_pan(k+1)-longs_pan(k))
lats_line = lats_pan(k) + r*(lats_pan(k+1)-lats_pan(k))

if(lats_line.le.lat_grid) then
   above_line(2) = 1
else
   above_line(2) = 0
end if

if(lats_line.le.lat_grid+dlat_grid) then
   above_line(3) = 1
else
   above_line(3) = 0
end if

nmean = 0 
data_mean = 0.d0

do kk = 1,4
   if ((abs(input_data(kk)).le.100d0).and.above_line0.eq.above_line(kk)) then
      nmean = nmean+1
      data_mean = data_mean+input_data(kk)
   end if
end do

if(nmean .eq. 0)then
   data_mean = 0d0    !errorreturn
else
   data_mean = data_mean/nmean
endif

do kk = 1,4
   if((abs(input_data(kk)).ge.1d10).or.above_line0.ne.above_line(kk)) then
      output_data(kk) = data_mean
   else
      output_data(kk) = input_data(kk)
   end if
end do

return
end subroutine

!--------------------------------------------------------------------------

!==========================================================================
subroutine gsw_add_mean(data_in,long,lat,data_out)
!==========================================================================

! Replaces NaN's with non-nan mean of the 4 adjacent neighbours
!
! data_in   : data set of the 4 adjacent neighbours   
! long      : longitude
! lat       : latitude
!
! data_out : non-nan mean of the 4 adjacent neighbours     [unitless]

implicit none

integer :: k, nmean

real(dbl), dimension(4) :: data_in, data_out 
real(dbl) :: data_mean, long, lat

nmean = 0
data_mean = 0.d0

do k = 1,4
   if (abs(data_in(k)).le.100d0) then
      nmean = nmean+1
      data_mean = data_mean+data_in(k)
   end if
end do

if(nmean.eq.0)then
   data_mean = 0d0    !errorreturn
else
   data_mean = data_mean/nmean
endif

do k = 1,4
   if(abs(data_in(k)).ge.100d0) then
      data_out(k) = data_mean
   else
      data_out(k) = data_in(k)
   end if
end do

return
end subroutine

!--------------------------------------------------------------------------

!==========================================================================
function xinterp1(x,y,n,x0)
!==========================================================================

! Linearly interpolate a real array   
!
! x      : y array (Must be monotonic)               
! y      : y array     
! n      : length of X and Y arrays
! x0     : value to be interpolated
!
! xinterp1 : Linearly interpolated value

implicit none

integer :: n, k

real(dbl), dimension(n) :: x, y
real(dbl) :: x0, r, xinterp1

call indx(x,n,x0,k)
r = (x0-x(k))/(x(k+1)-x(k))
xinterp1 = y(k) + r*(y(k+1)-y(k))

return
end function

!--------------------------------------------------------------------------

end module convert_0
