! This is the main program of dynamic precipitation recycling (DYNAMIC RECYCLING MODEL-DRM)
! By Prasanth Valaymkunnath, Virginia Tech

program thermo
 use netcdf
 use module_param
 use module_rd_namelist
 use module_listfile
 use module_netcdf_read
 use module_netcdf_write
 use flu_1
 use air_1
 use flu_2
 use air_2
 use flu_3a
 use air_3a
 use flu_3b
 use air_3b
 use liq_air_4b
 use Liq_Air_4a 
 use Liq_Vap_4
 use omp_lib
 
 implicit none
  integer 		:: i, j, k, nf
! Global (/namelist) Variables
  character(len=256)	:: indir
  character(len=256)	:: outdir
  character(len=256) 	:: fname, fname1,fname2
  character(len=256) 	:: zonefile
  integer	     	:: nx
  integer	     	:: ny
  integer	     	:: nz  
  real			:: dx, dy
  integer		:: dt
  integer		:: locx,locy,locz

  
  !Data File variables
  character(len=256), dimension(:),allocatable 	:: filelists
   integer 					:: NFiles,LL,L1, num_threads,istart, iend,&
						   jstart,jend,thread_num,pptx,ppty
   real 					:: time1, time2

  
  ! Data Arrays
  real, allocatable, dimension(:,:,:)	:: P1, P2 
  real, allocatable, dimension(:,:,:) 	:: PH1, PH2
  real, allocatable, dimension(:,:,:) 	:: QV1, QV2, QC1, QC2
  real, allocatable, dimension(:,:,:) 	:: RHO1, RHO2
  real, allocatable, dimension(:,:,:)   :: TH1, TH2,TEMP1,TEMP2
  real, allocatable, dimension(:,:,:)   :: U, V, W, UU,VV,WW
    
  integer,allocatable, dimension(:,:) 	:: BCC
  real,   allocatable, dimension(:,:) 	:: QFX1,QFX2,MAPFAC,T2,Q2,Q22,PSFC,DSX,DSY, &
  					   WORK_DAIR, WORK_MAIR, SIRR_ET, SIRR_D, MINEF_MA, &
  					   MINEF_DA, ENTHALPY_DA,ENTHALPY_MA,ENTHALPYDEL_D, &
  					   ENTHALPYDEL_M,ENTROPY_DA,ENTROPY_MA,ENTROPYDEL_D,ENTROPYDEL_M
  
  real 					:: QFX, ES, QVS, QV0, RH, EA_in,ES_in,QVS_in,QV0_in,&
  					   RH_in,hd_in,hl_in,hv_in, sd_in,sl_in,sv_in,GW_in,GV_in,&
  					   EA_out,ES_out,QVS_out,QV0_out,RH_out,hd_out,hl_out,hv_out,&
  					   sd_out,sl_out,sv_out,GW_out,GV_out,&
  					   QT_in, SPT_in,SPH_in,SPL_in,HHd_in ,SSd_in,HHm_in,SSm_in,&
  					   QT_out, SPT_out,SPH_out,SPL_out,HHd_out ,SSd_out,HHm_out,SSm_out

  
!-------------------------------------------------------------------------
  call rd_namelist(indir,outdir,fname,zonefile,nx,ny,nz,dx,dy,dt,num_threads)


  !Allocate  arrays for the dimensions
  allocate(WORK_DAIR(nx,ny))
  allocate(WORK_MAIR(nx,ny))
  allocate(SIRR_ET(nx,ny))
  allocate(SIRR_D(nx,ny))
  allocate(MINEF_MA(nx,ny))
  allocate(MINEF_DA(nx,ny))
  allocate(ENTHALPY_DA(nx,ny))
  allocate(ENTHALPY_MA(nx,ny))
  allocate(ENTHALPYDEL_D(nx,ny))
  allocate(ENTHALPYDEL_M(nx,ny))
  allocate(ENTROPY_DA(nx,ny))
  allocate(ENTROPY_MA(nx,ny))
  allocate(ENTROPYDEL_D(nx,ny))
  allocate(ENTROPYDEL_M(nx,ny))
      
  allocate(P1	(nx,ny,nz))
  allocate(P2	(nx,ny,nz))
  allocate(PH1	(nx,ny,nz))
  allocate(PH2	(nx,ny,nz))
  allocate(QV1	(nx,ny,nz))
  allocate(QV2	(nx,ny,nz))
  allocate(QC1	(nx,ny,nz))
  allocate(QC2	(nx,ny,nz))
  allocate(RHO1	(nx,ny,nz))
  allocate(RHO2	(nx,ny,nz))
  allocate(TH1	(nx,ny,nz))
  allocate(TH2	(nx,ny,nz))
  allocate(U	(nx+1,ny,nz))
  allocate(V	(nx,ny+1,nz))
  allocate(W	(nx,ny,nz+1))
  allocate(UU	(nx,ny,nz))
  allocate(VV	(nx,ny,nz))
  allocate(WW	(nx,ny,nz))  
      
  allocate(QFX1	(nx,ny))
  allocate(QFX2	(nx,ny))
  allocate(BCC	(nx,ny))
  allocate(MAPFAC(nx,ny))
  allocate(T2	(nx,ny))
  allocate(Q2	(nx,ny))
  allocate(Q22	(nx,ny))
  allocate(PSFC	(nx,ny))
  allocate(DSX	(nx,ny))  
  allocate(DSY	(nx,ny))
  allocate(TEMP1(nx,ny,nz))
  allocate(TEMP2(nx,ny,nz))
  !Initialize arrays with zero
  P1 	= 0.
  P2 	= 0.
  PH1	= 0.
  PH2	= 0.
  QV1	= 0.
  QV2	= 0.
  QC1	= 0.
  QC2	= 0.
  RHO1	= 0.
  RHO2	= 0.
  TH1	= 0.
  TH2	= 0.
  U	= 0.
  V	= 0.
  W	= 0.
  T2    = 0.
  PSFC  = 0.
  Q2    = 0.
  Q22    = 0.
  TEMP1 = 0.
  TEMP1 = 0.
  DSX   = 0.
  DSY   = 0.
  WORK_DAIR = 0.
  WORK_MAIR = 0.
  SIRR_ET = 0.
  SIRR_D = 0.
  MINEF_MA = 0.
  MINEF_DA = 0.
  ENTHALPY_DA = 0.
  ENTHALPY_MA = 0.
  ENTHALPYDEL_D = 0.
  ENTHALPYDEL_M = 0.
  ENTROPY_DA = 0.
  ENTROPY_MA = 0.
  ENTROPYDEL_D = 0.
  ENTROPYDEL_M = 0.

  
  call rd_2d_regions(zonefile, nx, ny, BCC, MAPFAC) 
  call list_files(indir,fname,filelists,NFiles)
  !write(*,*) NFiles,filelists

  write(*,*)'Stage 1 THERMO Model Started!' 
  do nf=1,Nfiles-1
        call cpu_time(time1)
  	fname1=trim(filelists(nf))
  	fname2=trim(filelists(nf+1))
  	LL = LEN_TRIM(fname2)
  	L1 = LL-29
  	
  	
  	call rd_3d_var(fname1,nx,ny,nz,P1,PH1,QV1,QC2,RHO1,TH1,QFX1)
  	call rd_3d_var(fname2,nx,ny,nz,P2,PH2,QV2,QC2,RHO2,TH2,QFX2)
  	call rd_3D_UV_SFC_var(fname1,nx,ny,nz,U,V,W,T2,PSFC,Q2)

! 1. Temperature from potential temperature
	TEMP1 = (TH1+300.) * (P1/PR)**(Rd/Cpd)
        TEMP2 = (TH2+300.) * (P2/PR)**(Rd/Cpd)
        
        DSX   = dx / MAPFAC
        DSY   = dy / MAPFAC
        pptx  = nx/(num_threads-1)
        ppty  = ny/(num_threads-1)  
       !$ call omp_set_num_threads(num_threads)

       !$omp parallel private(istart,iend,thread_num,ES,QVS,QV0,RH,QFX,locx,locy,locz,&
       !$omp EA_in,ES_in,QVS_in,QV0_in,RH_in,hd_in,hl_in,hv_in, &
       !$omp sd_in,sl_in,sv_in,GW_in,GV_in, &
       !$omp EA_out,ES_out,QVS_out,QV0_out,RH_out,hd_out,hl_out,hv_out, &
       !$omp sd_out,sl_out,sv_out,GW_out,GV_out, &
       !$omp QT_in,SPT_in,SPH_in,SPL_in, QT_out,SPT_out,SPH_out,SPL_out, &
       !$omp HHd_in,SSd_in,HHd_out,SSd_out,HHm_in,HHm_out,SSm_in,SSm_out,i,j,k)!,&
       !!$omp ENTHALPY_DA,ENTHALPY_MA,ENTHALPYDEL_D,ENTHALPYDEL_M,ENTROPY_DA,ENTROPY_MA,ENTROPYDEL_D,ENTROPYDEL_M)
	
       thread_num = omp_get_thread_num()
       istart     = thread_num*pptx+1
       iend       = min(nx,thread_num*pptx+pptx)
       !jstart     = thread_num*ppty+1
       !jend       = min(ny,thread_num*ppty+ppty)

  	do i=istart,iend

       !$omp parallel private(jstart,jend, thread_num,ES,QVS,QV0,RH,QFX,locx,locy,locz,&
       !$omp EA_in,ES_in,QVS_in,QV0_in,RH_in,hd_in,hl_in,hv_in, &
       !$omp sd_in,sl_in,sv_in,GW_in,GV_in, &
       !$omp EA_out,ES_out,QVS_out,QV0_out,RH_out,hd_out,hl_out,hv_out, &
       !$omp sd_out,sl_out,sv_out,GW_out,GV_out, &
       !$omp QT_in,SPT_in,SPH_in,SPL_in, QT_out,SPT_out,SPH_out,SPL_out, &
       !$omp HHd_in,SSd_in,HHd_out,SSd_out,HHm_in,HHm_out,SSm_in,SSm_out,j,k)!,&
       !!$omp ENTHALPY_DA,ENTHALPY_MA,ENTHALPYDEL_D,ENTHALPYDEL_M,ENTROPY_DA,ENTROPY_MA,ENTROPYDEL_D,ENTROPYDEL_M)
                  thread_num = omp_get_thread_num()
                  jstart     = thread_num*ppty+1
                  jend       = min(ny,thread_num*ppty+ppty)

  	   do j=jstart,jend
	    if (BCC(i,j).gt.0) then
	      ! Moistening inefficiency due to evapotranspiration
	      !----------------------------------------------------------------------------------------------------------------  	      
  	      ES  = ALIQ*EXP((BLIQ*T2(i,j)-CLIQ)/(T2(i,j)-DLIQ))
  	      QVS = (0.622 * ES) / (PSFC(i,j) - ES)
  	      QV0 = AMIN1(QVS,Q2(i,j))
              QV0 = AMAX1(0.000001,QV0)
              RH  = QV0/QVS
  	      
  	      QFX  	= (QFX1(i,j)) * 60 * 60  	      
  	      SIRR_ET(i,j) = -Rv*QFX*LOG(RH)
  	      !print*,'V2', SIRR_ET(i,j)
	      ! ----------------------------------------------------------------------------------------------------------------	
  	      do k=1,39
		 ! Find locations
  		 UU (i,j,k) = 0.5*( U (i,j,k) + U (i+1,j,k) )
  		 VV (i,j,k) = 0.5*( V (i,j,k) + V (i,j+1,k) )
  		 WW (i,j,k) = 0.5*( W (i,j,k) + W (i,j,k+1) )
  		 
  		 locx       = i + NINT(((UU (i,j,k) * 60.0 * 60.0)+DSX(i,j)*0.5)/DSX(i,j))
  		 locy       = j + NINT(((VV (i,j,k) * 60.0 * 60.0)+DSY(i,j)*0.5)/DSY(i,j))
  		 locz       = k + NINT(((WW (i,j,k) * 60.0 * 60.0)+PH1(i,j,k)*0.5)/PH1(i,j,k))
  		 
  		 ! Locations
  		 if (locx.lt.1)  locx=1
  		 if (locx.gt.nx) locx=nx 
  		 if (locy.lt.1)  locy=1
  		 if (locy.gt.ny) locy=ny 		 
  		 if (locz.lt.1)  locz=1
  		 if (locz.gt.nz) locz=nz
		 ! End find locations  		 
  		 !------------------------------------------------------------
  		 
  		 !--------in---------------------
  		 EA_in   = P1(i,j,k)*QV1(i,j,k)/0.622
  		 ES_in   = ALIQ*EXP((BLIQ*TEMP1(i,j,k)-CLIQ)/(TEMP1(i,j,k)-DLIQ))
		 QVS_in  = (0.622 * ES_in) / (P1(i,j,k) - (0.38*ES_in))
		 QV0_in  = AMIN1(QVS_in,QV1(i,j,k))
		 QV0_in  = AMAX1(0.000001,QV0_in)
		 RH_in   = AMIN1(QV0_in/QVS_in,1.0)
		 RH_in   = AMAX1(RH_in,0.05) ! if any value go negetive to make it positive
  		 
		 hd_in = Cpd*(TEMP1(i,j,k)-TR)		 				! enthalpy of dry air per unit weight
		 hl_in = Cpl*(TEMP1(i,j,k)-TR)						! enthalpy of liquid per unit weight
		 hv_in = Cpl*(TEMP1(i,j,k)-TR) + ((LVo*1E6) + (Cpv-Cpl)*(TEMP1(i,j,k)-TR))! enthalpy of vapor per unit weight
		 
		 sd_in = Cpd*LOG(TEMP1(i,j,k)/TR) - (Rd*LOG((P1(i,j,k)-EA_in)/Po) )
		 sl_in = Cpl*LOG(TEMP1(i,j,k)/TR)
		 sv_in = Cpl*LOG(TEMP1(i,j,k)/TR) + (((LVo*1E6) + & 
		 	 (Cpv-Cpl)*(TEMP1(i,j,k)-TR))/TEMP1(i,j,k)) - (Rv*LOG(RH_in)) 
		 
		 
		 GW_in = Cpl *((TEMP1(i,j,k)-TR)-(TEMP1(i,j,k)* LOG(TEMP1(i,j,k)/TR)))
		 GV_in = Cpl *((TEMP1(i,j,k)-TR)-(TEMP1(i,j,k)* LOG(TEMP1(i,j,k)/TR))) &
		 	 + (Rv*TEMP1(i,j,k)*LOG(RH_in))
		 
		 
		 !---------out----------------------------
		 EA_out   = P2(locx,locy,locz)*QV2(locx,locy,locz)/0.622	 
		 ES_out   = ALIQ*EXP((BLIQ*TEMP2(locx,locy,locz)-CLIQ)/(TEMP2(locx,locy,locz)-DLIQ))
		 QVS_out  = (0.622 * ES_out) / (P2(locx,locy,locz) - (0.38*ES_out))
		 QV0_out  = AMIN1(QVS_out,QV2(locx,locy,locz))
		 QV0_out  = AMAX1(0.000001,QV0_out)
		 RH_out   = QV0_out/QVS_out
		 
		 hd_out = Cpd*(TEMP2(locx,locy,locz)-TR)		 				! enthalpy of dry air per unit weight
		 hl_out = Cpl*(TEMP2(locx,locy,locz)-TR)						! enthalpy of liquid per unit weight
		 hv_out = Cpl*(TEMP2(locx,locy,locz)-TR) + ((LVo*1E6) + &
			  (Cpv-Cpl)*(TEMP2(locx,locy,locz)-TR))! enthalpy of vapor per unit weight
		 
		 sd_out = Cpd*LOG(TEMP2(locx,locy,locz)/TR) - &
		 	  Rd*LOG((P2(locx,locy,locz)-EA_out)/Po) 
		 sl_out = Cpl*LOG(TEMP2(locx,locy,locz)/TR)
		 sv_out = Cpl*LOG(TEMP2(locx,locy,locz)/TR) + (((LVo*1E6) + &
		 	 (Cpv-Cpl)*(TEMP2(locx,locy,locz)-TR))&
		 	 /TEMP2(locx,locy,locz)) - (Rv*LOG(RH_out)) 
		 
		 
		 GW_out = Cpl *((TEMP2(locx,locy,locz)-TR)- &
		 	  (TEMP2(locx,locy,locz)*LOG(TEMP2(locx,locy,locz)/TR)))
		 GV_out = Cpl *((TEMP2(locx,locy,locz)-TR)- &
		 	  (TEMP2(locx,locy,locz)*LOG(TEMP2(locx,locy,locz)/TR))) &
		 		+ Rv*TEMP2(locx,locy,locz)*LOG(RH_out)
		 
		 QT_in     = QV1(i,j,k) + QC1(i,j,k)
		 SPT_in    = QT_in/(1.0 + QT_in)
		 SPH_in    = QV1(i,j,k)/(1.0+QV1(i,j,k))
		 SPL_in    = QC1(i,j,k)/(1.0+QC1(i,j,k))
		 
		 
		 QT_out    = QV2(locx,locy,locz) + QC2(locx,locy,locz)
		 SPT_out   = QT_out/(1.0 + QT_out)
		 SPH_out   = QV2(locx,locy,locz)/(1.0+QV2(locx,locy,locz))
		 SPL_out   = QC2(locx,locy,locz)/(1.0+QC2(locx,locy,locz))
		 		
		 HHd_in    = hd_in + QV1(i,j,k)*hv_in + QC1(i,j,k)*hl_in ! total enthalpy per unit dry air 
		 SSd_in    = sd_in + QV1(i,j,k)*sv_in + QC1(i,j,k)*sl_in ! total entropy per unit dry air 
		 
		 HHd_out    = hd_out + QV2(locx,locy,locz)*hv_out + QC2(locx,locy,locz)*hl_out ! total enthalpy per unit dry air 
		 SSd_out    = sd_out + QV2(locx,locy,locz)*sv_out + QC2(locx,locy,locz)*sl_out ! total entropy per unit dry air 
		 		
		 WORK_DAIR(i,j) = WORK_DAIR(i,j)+(HHd_in-HHd_out) &
		 		  -(TEMP1(i,j,k)-TEMP2(locx,locy,locz))*(SSd_in-SSd_out) &
		 		  -(GV_in-GV_out)*(QV1(i,j,k)-QV2(locx,locy,locz)) &
		 		  -( (GV_in-GW_in) - (GV_out-GW_out)  )* (QC1(i,j,k)-QC2(locx,locy,locz))
		 		  
		 HHm_in   = (1-SPT_in)*hd_in + SPH_in*hv_in + SPL_in*hl_in
		 SSm_in   = (1-SPT_in)*sd_in + SPH_in*sv_in + SPL_in*sl_in

		 HHm_out   = (1-SPT_out)*hd_out + SPH_out*hv_out + SPL_out*hl_out
		 SSm_out   = (1-SPT_out)*sd_out + SPH_out*sv_out + SPL_out*sl_out
		 
		 WORK_MAIR(i,j) = WORK_MAIR(i,j) + (HHm_in-HHm_out) &
		 		 -(TEMP1(i,j,k)-TEMP2(locx,locy,locz))*(SSm_in-SSm_out) &
		 		 -(GV_in-GV_out)*(SPH_in-SPH_out)&
		 		 -( (GV_in-GW_in) - (GV_out-GW_out)  )*(SPL_in-SPL_out) 
		 		 
		 MINEF_DA(i,j) = MINEF_DA(i,j)-(GV_in-GV_out)*(QV1(i,j,k)-QV2(locx,locy,locz)) &
		 		 -( (GV_in-GW_in) - (GV_out-GW_out)  )* (QC1(i,j,k)-QC2(locx,locy,locz))
		 		  	
		 MINEF_MA(i,j) = MINEF_MA(i,j)-(GV_in-GV_out)*(SPH_in-SPH_out)&
		 		 -( (GV_in-GW_in) - (GV_out-GW_out)  )*(SPL_in-SPL_out) 
		 
		 SIRR_D(i,j)   = SIRR_D(i,j) + Rv*(QV1(i,j,k)-QV2(locx,locy,locz))
		 
		  
		ENTHALPY_DA (i,j)  = ENTHALPY_DA (i,j)+ HHd_out
  		ENTHALPY_MA (i,j)  = ENTHALPY_MA (i,j)+ HHm_out
  		ENTHALPYDEL_D (i,j)= ENTHALPYDEL_D (i,j)+ (HHd_in-HHd_out)
  		ENTHALPYDEL_M (i,j)= ENTHALPYDEL_M (i,j)+(HHm_in-HHm_out)
  		ENTROPY_DA (i,j)   = ENTROPY_DA (i,j)+ SSd_out 
  		ENTROPY_MA (i,j)   = ENTROPY_MA (i,j)+ SSm_out 
  		ENTROPYDEL_D (i,j) = ENTROPYDEL_D (i,j)+ (SSd_in-SSd_out) 
  		ENTROPYDEL_M (i,j) = ENTROPYDEL_M (i,j)+ (SSm_in-SSm_out) 
		 
  	     end do
  	       !!print*, WORK_MAIR(i,j)/3600. ,WORK_DAIR(i,j)/3600., MINEF_DA(i,j)/3600.,MINEF_MA(i,j)/3600., SIRR_ET(i,j)/3600.
  	    end if
  	  end do

           !!$omp critical
           !      !$ print*, "thread_num = ", thread_num,istart,iend,jstart,jend
           !!$omp end critical
          
         !$omp end parallel
  	end do
       
      !$omp end parallel
  call write_netcdf_output(trim(outdir)//trim(fname2(L1:LL)),nx,ny,nz,SIRR_D/3600,&
 				MINEF_MA/3600,MINEF_DA/3600,WORK_MAIR/3600,&
 				WORK_DAIR/3600,SIRR_ET/3600,ENTHALPY_DA/3600,&
 				ENTHALPY_MA/3600,ENTHALPYDEL_D/3600,ENTHALPYDEL_M/3600,&
 				ENTROPY_DA/3600,ENTROPY_MA/3600,ENTROPYDEL_D/3600,&
 				ENTROPYDEL_M/3600)
 				
	SIRR_ET=0.
	SIRR_D =0.
	MINEF_MA=0
	MINEF_DA=0.
	WORK_MAIR=0.
	WORK_DAIR=0.
	SIRR_ET=0.
	ENTHALPY_DA = 0.
  	ENTHALPY_MA = 0.
  	ENTHALPYDEL_D = 0.
  	ENTHALPYDEL_M = 0.
  	ENTROPY_DA = 0.
  	ENTROPY_MA = 0.
  	ENTROPYDEL_D = 0.
  	ENTROPYDEL_M = 0.
	call cpu_time(time2)
	print*, "time elapsed",(time2-time1)/num_threads
 end do
 
   !estimate number of grids in each zone
   


    write(*,*)'THERMO Model Finished!'
      !exit
     !IF( ALLOCATED(domainij) )  DEALLOCATE(domainij)
    
end program thermo
