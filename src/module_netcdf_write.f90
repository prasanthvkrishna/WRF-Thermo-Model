module module_netcdf_write
use netcdf
use module_param
implicit none
contains

 subroutine write_netcdf_output(filename,nx,ny,nz,SIRR_D,&
 				MINEF_MA,MINEF_DA,WORK_MAIR,&
 				WORK_DAIR,SIRR_ET,ENTHALPY_DA,&
 				ENTHALPY_MA,ENTHALPYDEL_D,ENTHALPYDEL_M,&
 				ENTROPY_DA,ENTROPY_MA,ENTROPYDEL_D,&
 				ENTROPYDEL_M)
	use netcdf
	integer, 	    				intent(in):: nx, ny, nz
        character(len=*), 				intent(in):: filename
        
        character (len = *), 				parameter :: LVL_NAME = "bottom_top"
  	character (len = *), 				parameter :: LAT_NAME = "south_north"
  	character (len = *), 				parameter :: LON_NAME = "west_east"
  	character (len = *), 				parameter :: REC_NAME = "time"
        integer, 					parameter :: ndims = 3, nrec = 1
	!real, dimension(1:nx,   1:ny,   1:nz),		intent(in)   	:: P, PH, QV, QC, RHO, T       ! Pressure
	real, dimension(1:nx,   1:ny),			intent(in)   	:: SIRR_D,    &
									   MINEF_MA,  & 
									   MINEF_DA,  &
									   WORK_MAIR, & 
									   WORK_DAIR, &
									   SIRR_ET,& 
									   ENTHALPY_DA, &
									   ENTHALPY_MA, &
									   ENTHALPYDEL_D, &
									   ENTHALPYDEL_M, &
									   ENTROPY_DA, &
									   ENTROPY_MA, &
									   ENTROPYDEL_D, &
									   ENTROPYDEL_M
	
	integer, parameter :: nxx=485, nyy=324
	
	integer	:: ncid, is_err, varid,i
	integer :: lvl_dimid, lon_dimid, lat_dimid, rec_dimid
	integer :: start(ndims), count(ndims)
	integer    :: lats(nyy) = (/(i, i=1,nyy, 1)/)
	integer    :: lons(nxx) = (/(i, i=1,nxx, 1)/)
        integer :: lon_varid, lat_varid
  
  	character (len = *), parameter :: SIRR_D_NAME    ="SIRR_D"
  	character (len = *), parameter :: MINEF_MA_NAME  ="MINEF_MA"
  	character (len = *), parameter :: MINEF_DA_NAME  ="MINEF_DA"
  	character (len = *), parameter :: WORK_MAIR_NAME ="WORK_MAIR"
  	character (len = *), parameter :: WORK_DAIR_NAME ="WORK_DAIR"
  	character (len = *), parameter :: SIRR_ET_NAME   ="SIRR_ET"
  	
  	character (len = *), parameter :: ENTHALPY_DA_NAME    ="ENTHALPY_DA"
  	character (len = *), parameter :: ENTHALPY_MA_NAME    ="ENTHALPY_MA"
  	character (len = *), parameter :: ENTHALPYDEL_D_NAME  ="ENTHALPYDEL_D"
  	character (len = *), parameter :: ENTHALPYDEL_M_NAME  ="ENTHALPYDEL_M"
  	character (len = *), parameter :: ENTROPY_DA_NAME     ="ENTROPY_DA"
  	character (len = *), parameter :: ENTROPY_MA_NAME     ="ENTROPY_MA"
  	character (len = *), parameter :: ENTROPYDEL_D_NAME   ="ENTROPYDEL_D"
  	character (len = *), parameter :: ENTROPYDEL_M_NAME   ="ENTROPYDEL_M"
  	
  	character (len = *), parameter :: UNITS = "units"
  	character (len = *), parameter :: VAR_UNITS = "W.m^-2"
  	character (len = *), parameter :: LAT_UNITS = "degrees_north"
  	character (len = *), parameter :: LON_UNITS = "degrees_east"
  	
  	integer :: SIRR_D_varid, MINEF_MA_varid, MINEF_DA_varid, WORK_MAIR_varid, WORK_DAIR_varid, SIRR_ET_varid,&
  		   ENTHALPY_DA_varid, ENTHALPY_MA_varid, ENTHALPYDEL_D_varid, ENTHALPYDEL_M_varid, ENTROPY_DA_varid, &
  		   ENTROPY_MA_varid,ENTROPYDEL_D_varid,ENTROPYDEL_M_varid
  		   
  	integer :: dimids(ndims)

        !-------------------------------------------------------------------------------------------------------------
        call check( nf90_create(filename, nf90_clobber, ncid) )
        
        call check( nf90_def_dim(ncid, LON_NAME, nx, lon_dimid) )  
  	call check( nf90_def_dim(ncid, LAT_NAME, ny, lat_dimid) )
  	call check( nf90_def_dim(ncid, REC_NAME, NF90_UNLIMITED, rec_dimid) )

     
  	call check( nf90_def_var(ncid, LON_NAME, NF90_REAL, lon_dimid, lon_varid) )
 	call check( nf90_def_var(ncid, LAT_NAME, NF90_REAL, lat_dimid, lat_varid) )
 
  	call check( nf90_put_att(ncid, lon_varid, UNITS, LON_UNITS) )
  	call check( nf90_put_att(ncid, lat_varid, UNITS, LAT_UNITS) )
        
 
        dimids = (/ lon_dimid, lat_dimid, rec_dimid /)
   	
   	
  	call check( nf90_def_var(ncid, SIRR_D_NAME,      NF90_REAL, dimids, SIRR_D_varid) )
  	
  	call check( nf90_def_var(ncid, MINEF_MA_NAME,    NF90_REAL, dimids, MINEF_MA_varid) ) 
  	call check( nf90_def_var(ncid, MINEF_DA_NAME,    NF90_REAL, dimids, MINEF_DA_varid) ) 
  	call check( nf90_def_var(ncid, WORK_MAIR_NAME,   NF90_REAL, dimids, WORK_MAIR_varid) ) 
  	call check( nf90_def_var(ncid, WORK_DAIR_NAME,   NF90_REAL, dimids, WORK_DAIR_varid) ) 
  	call check( nf90_def_var(ncid, SIRR_ET_NAME,     NF90_REAL, dimids, SIRR_ET_varid) )   	
  	
  	call check( nf90_def_var(ncid, ENTHALPY_DA_NAME,     NF90_REAL, dimids, ENTHALPY_DA_varid) ) 
  	call check( nf90_def_var(ncid, ENTHALPY_MA_NAME,     NF90_REAL, dimids, ENTHALPY_MA_varid) ) 
  	call check( nf90_def_var(ncid, ENTHALPYDEL_D_NAME,   NF90_REAL, dimids, ENTHALPYDEL_D_varid) ) 
  	call check( nf90_def_var(ncid, ENTHALPYDEL_M_NAME,   NF90_REAL, dimids, ENTHALPYDEL_M_varid) ) 
  	call check( nf90_def_var(ncid, ENTROPY_DA_NAME,      NF90_REAL, dimids, ENTROPY_DA_varid) ) 
  	call check( nf90_def_var(ncid, ENTROPY_MA_NAME,      NF90_REAL, dimids, ENTROPY_MA_varid) ) 
  	call check( nf90_def_var(ncid, ENTROPYDEL_D_NAME,    NF90_REAL, dimids, ENTROPYDEL_D_varid) ) 
  	call check( nf90_def_var(ncid, ENTROPYDEL_M_NAME,    NF90_REAL, dimids, ENTROPYDEL_M_varid) ) 
  	
  		
  		
  	call check( nf90_put_att(ncid, SIRR_D_varid,   	UNITS, VAR_UNITS) )
  	call check( nf90_put_att(ncid, MINEF_MA_varid, 	UNITS, VAR_UNITS) ) 
  	call check( nf90_put_att(ncid, MINEF_DA_varid, 	UNITS, VAR_UNITS) ) 
  	call check( nf90_put_att(ncid, WORK_MAIR_varid,	UNITS, VAR_UNITS) ) 
  	call check( nf90_put_att(ncid, WORK_DAIR_varid,	UNITS, VAR_UNITS) ) 
  	call check( nf90_put_att(ncid, SIRR_ET_varid,  	UNITS, VAR_UNITS) )        
        
        call check( nf90_put_att(ncid, ENTHALPY_DA_varid,    UNITS, VAR_UNITS) )
        call check( nf90_put_att(ncid, ENTHALPY_MA_varid,    UNITS, VAR_UNITS) )
        call check( nf90_put_att(ncid, ENTHALPYDEL_D_varid,  UNITS, VAR_UNITS) )
        call check( nf90_put_att(ncid, ENTHALPYDEL_M_varid,  UNITS, VAR_UNITS) )
        call check( nf90_put_att(ncid, ENTROPY_DA_varid,     UNITS, VAR_UNITS) )
        call check( nf90_put_att(ncid, ENTROPY_MA_varid,     UNITS, VAR_UNITS) )
        call check( nf90_put_att(ncid, ENTROPYDEL_D_varid,   UNITS, VAR_UNITS) )
        call check( nf90_put_att(ncid, ENTROPYDEL_M_varid,   UNITS, VAR_UNITS) )
        
        
        
        call check( nf90_enddef(ncid) )
        call check( nf90_put_var(ncid, lon_varid, lons) ) 
    	call check( nf90_put_var(ncid, lat_varid, lats) )
 

  	count = (/ nx, ny,1 /)
        start = (/ 1, 1, 1 /)	
  	
     	call check( nf90_put_var(ncid, SIRR_D_varid,     SIRR_D,  &
     	   			start=(/1,1/),count = count ))
     	call check( nf90_put_var(ncid, MINEF_MA_varid,   MINEF_MA, &
     				start=(/1,1/),count = count ))
     	call check( nf90_put_var(ncid, MINEF_DA_varid,   MINEF_DA, & 
     				start=(/1,1/),count = count ))
     	call check( nf90_put_var(ncid, WORK_MAIR_varid,  WORK_MAIR, &
     				start=(/1,1/),count = count ))
     	call check( nf90_put_var(ncid, WORK_DAIR_varid,  WORK_DAIR, & 
     				start=(/1,1/),count = count ))
     	call check( nf90_put_var(ncid, SIRR_ET_varid,    SIRR_ET,   &
     				start=(/1,1/),count = count ))


     	call check( nf90_put_var(ncid, ENTHALPY_DA_varid,       ENTHALPY_DA,   &
     				start=(/1,1/),count = count ))
     	call check( nf90_put_var(ncid, ENTHALPY_MA_varid,       ENTHALPY_MA,   &
     				start=(/1,1/),count = count ))
     	call check( nf90_put_var(ncid, ENTHALPYDEL_D_varid,    ENTHALPYDEL_D,   &
     				start=(/1,1/),count = count ))
     	call check( nf90_put_var(ncid, ENTHALPYDEL_M_varid,    ENTHALPYDEL_M,   &
     				start=(/1,1/),count = count ))
     	call check( nf90_put_var(ncid, ENTROPY_DA_varid,        ENTROPY_DA,   &
     				start=(/1,1/),count = count ))
     	call check( nf90_put_var(ncid, ENTROPY_MA_varid,        ENTROPY_MA,   &
     				start=(/1,1/),count = count ))
     	call check( nf90_put_var(ncid, ENTROPYDEL_D_varid,     ENTROPYDEL_D,   &
     				start=(/1,1/),count = count ))     				
     	call check( nf90_put_var(ncid, ENTROPYDEL_M_varid,     ENTROPYDEL_M,   &
     				start=(/1,1/),count = count ))
     				
     				
     				     					 
	call check( nf90_close(ncid) )      
        
  print *,"*** SUCCESS writing example file ", filename, "!"

 end subroutine write_netcdf_output
 
 
 subroutine check(status)
           integer, intent ( in) :: status
    
           if(status /= nf90_noerr) then 
            print *, trim(nf90_strerror(status))
            stop "Stopped"
           end if
  end subroutine check  
   
end module module_netcdf_write
