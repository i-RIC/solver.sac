cmake_minimum_required(VERSION 3.12)

project(SAC Fortran)

include(GNUInstallDirs)

# set CMAKE_PREFIX_PATH to the directory containing iriclibConfig.cmake
# ie -DCMAKE_PREFIX_PATH=d:/Downloads/iriclib-artifacts/iricsdk/iriclib-4.0.0/lib/cmake/iriclib
find_package(iriclib 4 REQUIRED)

# Change this to ON if solver requires OpenMP
option(IRIC_SOLVER_USE_OPENMP "Solver requires OpenMP" OFF)
if(IRIC_SOLVER_USE_OPENMP)
  find_package(OpenMP REQUIRED)
endif()

add_executable(SAC
    src/convert.f
    src/culout.f
    src/sac.f
    src/sac_cgns.f
    src/shydie.f
    src/shydie_cgns.f
)

target_link_libraries(SAC PRIVATE iriclib::iriclib)

if(IRIC_SOLVER_USE_OPENMP AND OpenMP_Fortran_FOUND)
  target_link_libraries(SAC PRIVATE OpenMP::OpenMP_Fortran)
endif()

# set stack on windows
# if(WIN32)
#   set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /HEAP:100000000,100000000 /STACK:1000000000,1000000000")
# #else()
# #  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fmax-stack-var-size=100000000")
# endif()

install(TARGETS SAC DESTINATION "${CMAKE_INSTALL_BINDIR}")

install(FILES solverdef/definition.xml DESTINATION "${CMAKE_INSTALL_BINDIR}")

include(CTest)

# add_subdirectory(test) # TODO

# POST_BUILD
if (WIN32)
  # ie "-DCMAKE_PREFIX_PATH=d:/Downloads/iriclib-artifacts/iricsdk-4.0.9/iriclib-4.0.9/lib/cmake/iriclib;d:/Downloads/iriclib-artifacts/iricsdk-4.0.9/hdf5-1.8.21/cmake/hdf5;d:/Downloads/iriclib-artifacts/iricsdk-4.0.9/poco-1.9.4/lib/cmake/Poco"
  find_package(HDF5 1.8 REQUIRED)
  find_package(PocoFoundation REQUIRED)
  # copy reqd dlls to test1 directory
  add_custom_command(TARGET SAC POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "$<TARGET_FILE:iriclib::iriclib>"   "$<TARGET_FILE_DIR:SAC>"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "$<TARGET_FILE:Poco::Foundation>"   "$<TARGET_FILE_DIR:SAC>"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "$<TARGET_FILE:hdf5::hdf5-shared>"  "$<TARGET_FILE_DIR:SAC>"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "$<TARGET_FILE:zlib-shared>"        "$<TARGET_FILE_DIR:SAC>"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "$<TARGET_FILE:szip-shared>"        "$<TARGET_FILE_DIR:SAC>"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "C:\\Program Files (x86)\\Intel\\oneAPI\\compiler\\2023.1.0\\windows\\redist\\intel64_win\\compiler\\$<IF:$<CONFIG:Release>,libifcoremd.dll,libifcoremdd.dll>" "$<TARGET_FILE_DIR:SAC>"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "C:\\Program Files (x86)\\Intel\\oneAPI\\compiler\\2023.1.0\\windows\\redist\\intel64_win\\compiler\\$<IF:$<CONFIG:Release>,libmmd.dll,libmmdd.dll>"           "$<TARGET_FILE_DIR:SAC>"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "C:\\Program Files (x86)\\Intel\\oneAPI\\compiler\\2023.1.0\\windows\\redist\\intel64_win\\compiler\\svml_dispmd.dll"                                          "$<TARGET_FILE_DIR:SAC>"
    )
endif()

# message(STATUS "HDF5_Fortran_INCLUDE_DIR = ${HDF5_Fortran_INCLUDE_DIR}")
# message(STATUS "HDF5_Fortran_LIBRARIES = ${HDF5_Fortran_LIBRARIES}")
# message(STATUS "HDF5_Fortran_HL_LIBRARIES = ${HDF5_Fortran_HL_LIBRARIES}")
# message(STATUS "CMAKE_Fortran_MODULE_DIRECTORY = ${CMAKE_Fortran_MODULE_DIRECTORY}")
# message(STATUS "HDF5_INCLUDE_DIRS = ${HDF5_INCLUDE_DIRS}")
# message(STATUS "hdf5::h5diff = $<TARGET_FILE_DIR:hdf5::h