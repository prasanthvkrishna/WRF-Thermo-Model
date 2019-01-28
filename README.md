# WRF-Thermo-Model

***********CAUSION: This is not the final version of the model!!!!**************

This Fortran90 model can estimate centain thermodynamic state variables of the atmosphere 
from WRF-ARW netcdf output data. The updated version (next version) of the model will use Thermodynamic 
Equation of SeaWater 2010 (TEOS-10). Eventhough there exists some of the TEOS-10 modules (Air*, Liq*,Flu*) 
available in the src directory. The source description of these module are commented inside 
the modules. However they are not used in this version. The surrent version uses constants and methods 
described in the WRF ARW technical description.

Output Includes
    Enthalpy, Entropy, Irreversible entropy and some other important thermodyncamic variables.


Parallel run with Openmp

Openmp usually comes with GCC/GFORTRAN compiler. This model uses openmp to reduce the simulation time. 
Any user can define the number of threads using the namelist.thermo in the run directory. To compile the 
model the user should have preinstalled netcdf fortran libraries. 

To compile for 1D parallel run

./compile_1d.sh

To compile for 2D parallel run

./compile_2d.sh

It will give you excecutable in the run directory

More information coming soon....
