module module_netcdf_read
use netcdf
use module_param
implicit none
contains

 subroutine rd_3d_var(filename,nx,ny,nz,P,PH,QV,QC,RHO,T,QFX)
	use netcdf
	integer, 	    				intent(in)      :: nx, ny, nz
        character(len=256), 				intent(in)      :: filename
	real, dimension(1:nx,   1:ny,   1:nz),		intent(out)   	:: P, PH, QV, QC, RHO, T       ! Pressure
	real, dimension(1:nx,   1:ny),			intent(out)   	:: QFX
	
	integer						        	:: ncid, is_err, varid
	real, dimension(1:nx, 	1:ny, 	1:nz)                       	:: PB, PHB

        
	! initializwe the arrays
	P       = 0.      
	PB      = 0.
	PH      = 0.
	PHB     = 0.
	QV      = 0.
	RHO     = 0.
	T       = 0.

	!Open NetCDF file
	!Print*,"Program is reading ",filename
	is_err = nf90_open(filename, NF90_NOWRITE, ncid)
	if (is_err /= 0) then
	   write(*,'("module_netcdf_read 3d_var:  Problem opening NetCDF file: ''", A, "''")') trim(filename)
	   stop
	endif
	
	!------------------------------------------------------------------------------------------
	! 1. Perturbation Pressure, P      
	is_err = nf90_inq_varid(ncid,  "P",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var P:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=P, start=(/1,1,1,1/), count=(/nx,ny,nz,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable P in input NetCDF file."
	endif
	!Print*,"P reading finished" 
	
	!------------------------------------------------------------------------------------------
	! 2. Base-state Pressure, PB      
	is_err = nf90_inq_varid(ncid,  "PB",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var PB:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=PB, start=(/1,1,1,1/), count=(/nx,ny,nz,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable PB in input NetCDF file."
	endif

	!Print*,"PB reading finished"
	
	!Total Pressure	
	P  =  P+PB	
	
	!------------------------------------------------------------------------------------------
	! 3. Perturabation Geopotantial, PH      
	is_err = nf90_inq_varid(ncid,  "PH",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var PH:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=PH, start=(/1,1,1,1/), count=(/nx,ny,nz,1/))
	if (is_err /= 0) then
	   Print*, 'ncid = ', ncid, "Error Reading Variable PH in input NetCDF file."
	endif

	!!Print*,"PH reading finished"
	
	!------------------------------------------------------------------------------------------
	! 1. Base-State Geopotantial, PHB      
	is_err = nf90_inq_varid(ncid,  "PHB",  varid)
	!!Print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var PHB:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=PHB, start=(/1,1,1,1/), count=(/nx,ny,nz,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable PHB in input NetCDF file."
	endif

	!!Print*,"PHB reading finished"
 
        PH = (PH + PHB)/gc
 
	!------------------------------------------------------------------------------------------
	! 1. QVAPOR    
	is_err = nf90_inq_varid(ncid,  "QVAPOR",  varid)
	!!Print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var QVAPOR:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=QV, start=(/1,1,1,1/), count=(/nx,ny,nz,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable QVAPOR in input NetCDF file."
	endif

	!!Print*,"QVAPOR reading finished"
	
	!------------------------------------------------------------------------------------------
	! 1. QVAPOR    
	is_err = nf90_inq_varid(ncid,  "QCLOUD",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var QCLOUD:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=QC, start=(/1,1,1,1/), count=(/nx,ny,nz,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable QCLOUD in input NetCDF file."
	endif

	!Print*,"QCLOUD reading finished"
	
	!------------------------------------------------------------------------------------------
	! 1. RHO   
	is_err = nf90_inq_varid(ncid,  "RHO",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var RHO:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=RHO, start=(/1,1,1,1/), count=(/nx,ny,nz,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable RHO in input NetCDF file."
	endif

	!!Print*,"RHO reading finished"	
	
	!------------------------------------------------------------------------------------------
	! 1. T   
	is_err = nf90_inq_varid(ncid,  "T",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var T:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=T, start=(/1,1,1,1/), count=(/nx,ny,nz,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable T in input NetCDF file."
	endif

	!Print*,"T reading finished"	
	
	!------------------------------------------------------------------------------------------
	
	! 1. QFX  
	is_err = nf90_inq_varid(ncid,  "QFX",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var QFX:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=QFX, start=(/1,1,1/), count=(/nx,ny,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable QFX in input NetCDF file."
	endif

	!Print*,"QFX reading finished"
	
	! Close the opened netcdf file
        is_err = nf90_close(ncid)	             
 end subroutine rd_3d_var
 
 
 subroutine rd_3D_UV_SFC_var(filename,nx,ny,nz,U,V,W,T2,PSFC,Q2)
	use netcdf
	integer, 	    				intent(in)    :: nx, ny, nz
        character(len=256), 				intent(in)    :: filename
	real, dimension(1:nx,   1:ny),			intent(out)   :: T2, Q2, PSFC
	real, dimension(1:nx+1, 1:ny,   1:nz),		intent(out)   :: U
	real, dimension(1:nx,   1:ny+1, 1:nz),		intent(out)   :: V
	real, dimension(1:nx,   1:ny,   1:nz+1),	intent(out)   :: W
	
	integer						        	:: ncid, is_err, varid
	
	!Open NetCDF file
	!Print*,"rd_3D_UV_SFC_var Program is reading ",filename
	is_err = nf90_open(filename, NF90_NOWRITE, ncid)
	if (is_err /= 0) then
	   write(*,'("module_netcdf_read 3d_var:  Problem opening NetCDF file: ''", A, "''")') trim(filename)
	   stop
	endif
	
	!------------------------------------------------------------------------------------------
	! 1. U   
	is_err = nf90_inq_varid(ncid,  "U",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var U:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=U, start=(/1,1,1,1/), count=(/nx+1,ny,nz,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable U in input NetCDF file."
	endif

	!Print*,"U reading finished"	

	!------------------------------------------------------------------------------------------
	! 1. V   
	is_err = nf90_inq_varid(ncid,  "V",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var V:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=V, start=(/1,1,1,1/), count=(/nx,ny+1,nz,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable V in input NetCDF file."
	endif

	!Print*,"V reading finished"
	
	!------------------------------------------------------------------------------------------
	! 1. W   
	is_err = nf90_inq_varid(ncid,  "W",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var W:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=W, start=(/1,1,1,1/), count=(/nx,ny,nz+1,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable W in input NetCDF file."
	endif

	!Print*,"W reading finished"
	
	!------------------------------------------------------------------------------------------
	! 1. T2   
	is_err = nf90_inq_varid(ncid,  "T2",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var T2:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=T2, start=(/1,1,1/), count=(/nx,ny,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable T2 in input NetCDF file."
	endif

	!Print*,"T2 reading finished"
	
	!------------------------------------------------------------------------------------------
	! 1. Q2   
	is_err = nf90_inq_varid(ncid,  "Q2",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var Q2:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=Q2, start=(/1,1,1/), count=(/nx,ny,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable Q2 in input NetCDF file."
	endif

	!Print*,"Q2 reading finished"
	
	!------------------------------------------------------------------------------------------
	! 1. PSFC   
	is_err = nf90_inq_varid(ncid,  "PSFC",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_read 3d_var PSFC:  variable not found: ''", A, "''")') trim(filename)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=PSFC, start=(/1,1,1/), count=(/nx,ny,1/))
	if (is_err /= 0) then
	   print*, 'ncid = ', ncid, "Error Reading Variable PSFC in input NetCDF file."
	endif

	!Print*,"PSFC reading finished"		
	
        is_err = nf90_close(ncid)	             
 end subroutine rd_3D_UV_SFC_var	
	

 
 ! Sub regions
 subroutine rd_2d_regions(zonefile, nx, ny, BCC, MAPFAC)
	use netcdf
    	character(len=256),		intent(in) 	:: zonefile
 	integer, 	    		intent(in) 	:: nx,ny      
	integer, dimension(1:NX,1:NY),	intent(out) 	:: BCC 
	integer					 	:: ncid,is_err,varid
	real,    dimension(1:NX,1:NY),	intent(out) 	:: MAPFAC

	!Open NetCDF file
	!!Print*,"Program is reading ",zonefile
	is_err = nf90_open(zonefile, NF90_NOWRITE, ncid)
	if (is_err /= 0) then
	    write(*,'("module_netcdf_io regions map:  Problem opening NetCDF file: ''", A, "''")') trim(zonefile)
	    stop
	endif
	!--------------------------------------------------------------------------------------------------------      
	is_err = nf90_inq_varid(ncid,  "B_C_C",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_io B_C_C:  variable not found: ''", A, "''")') trim(zonefile)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=BCC, start=(/1,1,1/), count=(/nx,ny,1/))
	if (is_err /= 0) then
	    print*, 'ncid = ', ncid, "Error Reading Variable B_C_C in input NetCDF file."
	endif 
	
	
	!--------------------------------------------------------------------------------------------------------      
	is_err = nf90_inq_varid(ncid,  "MAPFAC_M",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_io MAPFAC_M:  variable not found: ''", A, "''")') trim(zonefile)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=MAPFAC, start=(/1,1,1/), count=(/nx,ny,1/))
	if (is_err /= 0) then
	    print*, 'ncid = ', ncid, "Error Reading Variable MAPFAC_M in input NetCDF file."
	endif 	
	
	is_err = nf90_close(ncid)             
 end subroutine rd_2d_regions

end module module_netcdf_read
