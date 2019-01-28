module module_rd_namelist
implicit none
contains 
  subroutine rd_namelist(indir,outdir,fname,	&
  			 zonefile,nx,ny,nz,dx,dy,  &
                         dt,num_threads)
  character(len=256)	:: indir,outdir
  character(len=256) 	:: fname
  character(len=256) 	:: zonefile
  integer	     	:: nx,ny,nz,num_threads
  real	 	     	:: dx,dy
  integer		:: dt,iserr

  
  namelist / THERMO/ INDIR,OUTDIR,FNAME, &
  		  ZONEFILE,NX,NY,NZ,     &
                  DT,DX,DY,NUM_THREADS
                  
  
  open(20, file="namelist.thermo", form="FORMATTED")
  read(20, THERMO, iostat=iserr)
  if (iserr /= 0) then
     write(*,'(/," ERROR THERMO: Problem reading namelist.thermo",/)')
     rewind(20)
     read(20, THERMO)
     stop " ERROR THERMO: Problem reading namelist.thermo"
  endif              
  close(20)
  
  end subroutine rd_namelist

end module module_rd_namelist
