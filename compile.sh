gfortran -c -std=f2008 mo_parameters.f08
gfortran -c -std=f2008 mo_structured_datatypes.f08
gfortran -c -std=f2008 test_procedures.f08
gfortran -c -std=f2008 classArrayElement.f08
gfortran -c -std=f2008 classFieldArray.f08
gfortran -c -std=f2008 classOStreamNCDF.f08 -I/usr/include
gfortran -c -std=f2008 classOStreamHDF5.f08 -I /usr/lib/x86_64-linux-gnu/hdf5/serial/include/
gfortran -c -std=f2008 main.f08
gfortran *.o -std=f2008 -L/usr/lib/x86_64-linux-gnu/ -lnetcdf -lnetcdff -L/usr/lib/x86_64-linux-gnu/hdf5/serial/ -lhdf5_fortran -lhdf5hl_fortran
