# Make file for THERMO model

######## EDIT HERE ##########
    # Compiler and Flags
#FC 		= gfortran
#FFLAGS 		= -c #-Wall -fcheck=all
#NETCDF 		= /home/pvk03/Cascades/local/netcdf-4.4.0/
#NETCDFMOD	= -I$(NETCDF)/include
#NETCDFLIB	= -L$(NETCDF)/lib -lnetcdf -lnetcdff
#LINKER		= $(FC) -o

FC              = gfortran
OMP             = -fopenmp
FFLAGS          = -c -Wall -fcheck=all -Wno-tabs -Wno-unused-value -Wno-unused-parameter -Wno-unused-variable -Wno-unused-dummy-argument -Wno-conversion
NETCDF          = /home/pvk03/local/gcc-5.4/netcdf-4.4.1.1/
NETCDFMOD       = -I$(NETCDF)/include
NETCDFLIB       = -L$(NETCDF)/lib -lnetcdf -lnetcdff
LINKER          = $(FC) -o
LIB		=  #-L/home/pvk03/Codes/thermo/src/sia_teos10/ -lsiateos10

##################################################
######## DON' CHANGE ANYTHING BELOW###############
OBJS = thermo.o module_netcdf_write.o module_netcdf_read.o module_param.o module_listfile.o module_rd_namelist.o Liq_Air_4b.o Liq_Air_4a.o Liq_Vap_4.o Air_3b.o Flu_3b.o Air_3a.o Flu_3a.o Air_2.o Air_1.o Flu_2.o Flu_1.o  Maths_0.o Convert_0.o Constants_0.o
#Programe Name
THERMO = thermo.exe

# Source File are in
VPATH = src

# Object folder
OBJDIR = objs

# Module Folder
MODDIR = mods

# Excecutable
EXEDIR=run


model : $(THERMO)

# Create the model
$(THERMO): $(OBJS)
	@echo "--------------------------------------"
	@echo "--------Compiling THERMO Model-----------"
	@echo "--------------------------------------"
	$(LINKER) $(THERMO) $(OMP) $(OBJS) $(NETCDFMOD) $(NETCDFLIB) $(LIB)
	mv *.o   $(OBJDIR)
	mv *.mod $(MODDIR)
	mv *.exe $(EXEDIR)
	@echo "**************************************"
	@echo "*      Compiling THERMO:Success         *"
	@echo "**************************************"
	
%.o:   %.f90 
	@echo "--------------------------------------"
	@echo "--------Compiling files $<"
	@echo "--------------------------------------"
	$(FC) $(OMP) $(FFLAGS) $(NETCDFMOD) $(NETCDFLIB) $(LIB)  $<
	
# Clean compilation
clean:
	@echo "--------------------------------------"
	@echo "--------Make Clean Model--------------"
	@echo "--------------------------------------"
	rm -rf *~ *.exe *.o *.mod
	rm -rf $(OBJDIR)/*.o $(OBJDIR)/*~
	rm -rf $(MODDIR)/*.mod $(MODDIR)/*~
	rm -rf $(EXEDIR)/*.exe $(EXEDIR)/*~
	rm -rf $(VPATH)/*~

thermo.o 		: thermo.f90 module_netcdf_write.o module_netcdf_read.o module_param.o module_listfile.o module_rd_namelist.o Liq_Air_4b.o Liq_Air_4a.o Liq_Vap_4.o Air_3b.o Flu_3b.o Air_3a.o Flu_3a.o Air_2.o Air_1.o Flu_2.o Flu_1.o  Maths_0.o Convert_0.o Constants_0.o
module_netcdf_write.o   : module_netcdf_write.f90 module_param.o
module_netcdf_read.o    : module_netcdf_read.f90 module_param.o
module_param.o 		: module_param.f90 
module_listfile.o 	: module_listfile.f90 
module_rd_namelist.o    : module_rd_namelist.f90
Liq_Air_4b.o		: Liq_Air_4b.f90 Constants_0.o Air_3a.o Flu_3a.o Liq_Air_4a.o
Liq_Air_4a.o		: Liq_Air_4a.f90 Constants_0.o Convert_0.o Maths_0.o Air_1.o Flu_1.o Air_2.o Flu_2.o Air_3a.o Air_3b.o Flu_3a.o Liq_Vap_4.o
Liq_Vap_4.o		: Liq_Vap_4.f90 Constants_0.o Maths_0.o Flu_1.o Flu_2.o Flu_3a.o
Air_3b.o                : Air_3b.f90 Constants_0.o Convert_0.o Air_1.o Air_2.o Air_3a.o
Flu_3b.o		: Flu_3b.f90 Constants_0.o Flu_2.o Flu_3a.o
Air_3a.o		: Air_3a.f90 Constants_0.o Convert_0.o Maths_0.o Air_1.o Air_2.o
Flu_3a.o		: Flu_3a.f90 Constants_0.o Convert_0.o Maths_0.o Flu_1.o
Air_2.o			: Air_2.f90 Constants_0.o Air_1.o Flu_1.o
Air_1.o			: Air_1.f90 Constants_0.o
Flu_2.o			: Flu_2.f90 Constants_0.o Flu_1.o
Flu_1.o			: Flu_1.f90 Constants_0.o
Maths_0.o		: Maths_0.f90 Constants_0.o
Convert_0.o		: Convert_0.f90 Constants_0.o
Constants_0.o		: Constants_0.f90
