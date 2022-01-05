cmake_minimum_required(VERSION 3.15)

project(CAP Fortran)

include(GNUInstallDirs)

option(BUILD_FOR_IRIC "Build i-RIC solver version" OFF)
if(BUILD_FOR_IRIC)
  # set CMAKE_PREFIX_PATH to the directory containing iriclibConfig.cmake
  # ie -DCMAKE_PREFIX_PATH=d:/Downloads/iricsdk/iriclib-4.0.x/lib/cmake/iriclib
  # or iriclib_DIR:PATH=C:/Users/charlton/iRIC_v4/sdk/c/lib/cmake/iriclib
  find_package(iriclib 4 REQUIRED)
endif()

# Change this to ON if solver requires OpenMP
option(IRIC_SOLVER_USE_OPENMP "Solver requires OpenMP" OFF)
if(IRIC_SOLVER_USE_OPENMP)
  find_package(OpenMP REQUIRED)
endif()

# No need to add iric.f90 it will be added automatically by target_link_libraries
add_executable(CAP
  src/cinmod.F
  src/convert.f90
  src/culdmp.f
  src/culrat.f
  src/culvert.F
  src/discharg.f
  src/ellip.f
  src/icvprp.f
  src/ixsprp.f
  src/pipearc.f
  src/shydie.f
)

# add *.F files to the Source Files group in VS IDE
source_group("Source Files" REGULAR_EXPRESSION *.F)

# build standalone version statically (reqs cmake 3.15)
if(NOT BUILD_FOR_IRIC)
  set_property(TARGET CAP PROPERTY MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
endif()

if(BUILD_FOR_IRIC)
  target_sources(CAP
    PRIVATE
    src/icvprp_cgns.f
    src/ixsprp_cgns.f
    src/shydie_cgns.f
  )
endif()

if(BUILD_FOR_IRIC)
  target_compile_definitions(CAP PRIVATE __BUILD_FOR_IRIC__)
  target_link_libraries(CAP PRIVATE iriclib::iriclib)
endif()

if(IRIC_SOLVER_USE_OPENMP AND OpenMP_Fortran_FOUND)
  target_link_libraries(CAP PRIVATE OpenMP::OpenMP_Fortran)
endif()

install(
  TARGETS
    CAP
  DESTINATION
    "${CMAKE_INSTALL_BINDIR}"
)

if(BUILD_FOR_IRIC)
  install(
    FILES
      solverdef/alignment_mitered.png
      solverdef/alignment_ncul.png
      solverdef/alignment_skewed.png
      solverdef/alignment_types.png
      solverdef/alignment_unskewed.png
      solverdef/definition.xml
      solverdef/edging_beveled.png
      solverdef/edging_rounded.png
      solverdef/edging_types.png
      solverdef/projection_equal.png
      solverdef/projection_types.png
      solverdef/projection_unequal.png
      solverdef/shape_box.png
      solverdef/shape_circular.png
      solverdef/shape_elliptical.png
      solverdef/shape_pipearch.png
      solverdef/shape_types.png
      solverdef/wingwalls_equal.png
      solverdef/wingwalls_types.png
      solverdef/wingwalls_unequal.png
    DESTINATION
      "${CMAKE_INSTALL_BINDIR}"
  )
endif()

# include(CTest)

# add_subdirectory(tests)