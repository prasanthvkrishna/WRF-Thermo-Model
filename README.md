# WRF-Thermo-Model

***********CAUSION: This is not the final version of the model!!!!**************

This Fortran90 model can estimate certain thermodynamic state variables of the atmosphere from WRF-ARW NetCDF output data. The updated version (next version) of the model will use Thermodynamic 
equation of SeaWater 2010 (TEOS-10). Even though there exists some of the TEOS-10 modules (Air*, Liq*, Flu*) 
available in the src directory. The source description of these modules are commented inside 
the modules. However, they are not used in this version. The current version uses constants and methods described in the WRF ARW technical description.

Output Includes
    Enthalpy, Entropy, Irreversible entropy and some other important thermodynamic variables.


Parallel run with OpenMP

OpenMP usually comes with GCC/gfortran compiler. This model uses OpenMP to reduce the simulation time. 
Any user can define the number of threads using the namelist.thermo in the run directory. To compile the model the user should have preinstalled NetCDF Fortran libraries. 

To compile for the 1D parallel run

./compile_1d.sh

To compile for the 2D parallel run

./compile_2d.sh

It will give you executable in the run directory

More information is coming soon...

Contact: Prasanth Valayamkunnath, Virginia Tech (pvk03@vt.edu)
