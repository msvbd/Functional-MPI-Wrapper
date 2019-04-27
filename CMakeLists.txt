# cmake version, project name, language
cmake_minimum_required(VERSION 3.0.2 FATAL_ERROR)
project(functional_mpi_wrapper Fortran)

find_package(MPI COMPONENTS Fortran)

set(CMAKE_REQUIRED_FLAGS ${MPI_Fortran_COMPILE_OPTIONS})
set(CMAKE_REQUIRED_INCLUDES ${MPI_Fortran_INCLUDE_DIRS})
set(CMAKE_REQUIRED_LIBRARIES ${MPI_Fortran_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})

include(CheckFortranSourceCompiles)

# set output paths for modules, archives, and executables
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/../lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/../lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# if build type not specified, default to release
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "release")
endif()

# if build type not specified, default to release
if(NOT MPI_Fortran_FOUND)
  message( FATAL_ERROR "MPI Fortran compiler not found" )
endif()

# compiler flags for gfortran
if(MPI_Fortran_COMPILER MATCHES mpifort)
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -C -fbacktrace")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

# compiler flags for ifort
if(MPI_Fortran_COMPILER MATCHES mpiifort)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -assume realloc_lhs -heap-arrays")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -C -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

add_library(mpifw src/mod_common.f90
				  src/mod_reduce.f90
				  src/mod_send.f90
				  src/mod_broadcast.f90
				  src/mod_allreduce.f90)
target_link_libraries(mpifw  MPI::MPI_Fortran)

#install(DIRECTORY "${CMAKE_Fortran_MODULE_DIRECTORY}" DESTINATION "${CMAKE_INSTALL_PREFIX}")
#install(DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}" DESTINATION "${CMAKE_INSTALL_PREFIX}")
