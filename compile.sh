gfortran -c mo_parameters.f08
gfortran -c mo_structured_datatypes.f08
gfortran -c test_procedures.f08
gfortran -c classArrayElement.f08
gfortran -c classFieldArray.f08
gfortran -c classOStreamNCDF.f08 -I/usr/include
gfortran -c main.f08
gfortran *.o -L/usr/lib -lnetcdf -lnetcdff
