cmake_minimum_required(VERSION 3.10)

project(cfs_decode Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
  if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan")
  else ()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffpe-summary=none -Ofast")
  endif ()
endif ()

set (CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}")
include_directories(${CMAKE_BINARY_DIR})

set(sources
  cfs_decode.f90
  date_link_mod.f90
  get_cfs_field_mod.f90
  output_mod.f90
  params_mod.f90
)

set(ENV{ECCODES_ROOT} "/home/em/software/intel")
set(ECCODES_INCLUDEDIR "$ENV{ECCODES_ROOT}/include")
set(ECCODES_LIBDIR "$ENV{ECCODES_ROOT}/lib")
set(EXECUTABLE "cfs_decode.exe")

include_directories(${ECCODES_INCLUDEDIR})
link_directories(${ECCODES_LIBDIR})
link_libraries(eccodes_f90)
add_executable(${EXECUTABLE} ${sources})
# target_link_libraries(${EXECUTABLE} eccodes_f90)