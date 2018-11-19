F90       = ifort
F90FLAGS  = -heap-arrays 1600
EXEC      = cfs_decode
GRIB_API_INC = -I/opt/software/grib_api/1.19.0/intel/include
GRIB_API_LIB = -L/opt/software/grib_api/1.19.0/intel/lib -lgrib_api_f90 -lgrib_api
#GRIB_API_INC = -I/opt/software/libs/grib_api/1.13.0/intel/include
#GRIB_API_LIB = -L/opt/software/libs/grib_api/1.13.0/intel/lib -lgrib_api_f90 -lgrib_api
#GRIB_API_INC = -I/opt/software/eccodes/include
#GRIB_API_LIB = -L/opt/software/eccodes/lib -leccodes_f90 -leccodes

all: $(EXEC)
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "Successfully created $(EXEC) binary file"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"

OBJS   = params_mod.o date_link_mod.o get_cfs_field_mod.o output_mod.o cfs_decode.o

.SUFFIXES:

.SUFFIXES: .f90 .o

$(EXEC): $(OBJS)
	$(F90) -o $@ $(OBJS) $(GRIB_API_LIB)

.f90.o:
	$(F90) $(F90FLAGS) -c -o $@ $(GRIB_API_INC) -c $<

clean:
	rm -f *.mod *.o $(EXEC)

neat:
	-rm -f *.mod *.o
