cmake_minimum_required(VERSION 2.6)

########### global settings ###############

set(H_SRCS)

set(CXX_SRCS ParserTest.cc)

set(F90_SRCS FParserTest.F90 AssertStuff.F90)

include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_BINARY_DIR}/..)

#link_directories(${CMAKE_BINARY_DIR})

########### ParserTest target ###############
set(ParserTest_SRCS ${CXX_SRCS} ${H_SRCS})

add_executable(ParserTest EXCLUDE_FROM_ALL ${ParserTest_SRCS})

find_library(GENMALLOC_LIBRARY genmalloc DIRS
   ${CMAKE_BINARY_DIR}/genmalloc
   ${CMAKE_BINARY_DIR}
   ${CMAKE_BINARY_DIR}/../genmalloc
   ../../genmalloc)
#message("GENMALLOC directory is ${GENMALLOC_LIBRARY}")
if (GENMALLOC-FOUND)
   set_target_properties(ParserTest PROPERTIES COMPILE_DEFINITIONS HAVE_GENMALLOC)
endif (GENMALLOC-FOUND)

set_target_properties(ParserTest PROPERTIES EXCLUDE_FROM_ALL TRUE)
set_target_properties(ParserTest PROPERTIES EXCLUDE_FROM_DEFAULT_BUILD TRUE)
target_link_libraries(ParserTest PowerParser)
if (GENMALLOC-FOUND)
   target_link_libraries(ParserTest PowerParser ${GENMALLOC_LIBRARY})
endif (GENMALLOC-FOUND)

########### pParserTest target ###############
set(pParserTest_SRCS ${CXX_SRCS} ${H_SRCS})

add_executable(pParserTest EXCLUDE_FROM_ALL ${ParserTest_SRCS})

find_library(GENMALLOC_LIBRARY genmalloc DIRS
   ${CMAKE_BINARY_DIR}/genmalloc
   ${CMAKE_BINARY_DIR}
   ${CMAKE_BINARY_DIR}/../genmalloc
   ../../genmalloc)
#message("GENMALLOC directory is ${GENMALLOC_LIBRARY}")
if (GENMALLOC-FOUND)
   set_target_properties(pParserTest PROPERTIES COMPILE_DEFINITIONS HAVE_GENMALLOC)
endif (GENMALLOC-FOUND)

set_target_properties(pParserTest PROPERTIES COMPILE_DEFINITIONS HAVE_MPI)
set_target_properties(pParserTest PROPERTIES EXCLUDE_FROM_ALL TRUE)
set_target_properties(pParserTest PROPERTIES EXCLUDE_FROM_DEFAULT_BUILD TRUE)
target_link_libraries(pParserTest pPowerParser ${MPI_LIBRARIES})
if (GENMALLOC-FOUND)
   target_link_libraries(ParserTest pPowerParser ${GENMALLOC_LIBRARY})
endif (GENMALLOC-FOUND)

########### FParserTest target ###############
set(FParserTest_SRCS ${F90_SRCS})

add_executable(FParserTest EXCLUDE_FROM_ALL ${FParserTest_SRCS})

find_library(GENMALLOC_LIBRARY genmalloc DIRS
   ${CMAKE_BINARY_DIR}/genmalloc
   ${CMAKE_BINARY_DIR}
   ${CMAKE_BINARY_DIR}/../genmalloc
   ../../genmalloc)
#message("GENMALLOC directory is ${GENMALLOC_LIBRARY}")
if (GENMALLOC-FOUND)
   set_target_properties(FParserTest PROPERTIES COMPILE_DEFINITIONS HAVE_GENMALLOC)
endif (GENMALLOC-FOUND)

set_target_properties(FParserTest PROPERTIES EXCLUDE_FROM_ALL TRUE)
set_target_properties(FParserTest PROPERTIES EXCLUDE_FROM_DEFAULT_BUILD TRUE)
set_target_properties(FParserTest PROPERTIES LINKER_LANGUAGE Fortran)
target_link_libraries(FParserTest PowerParser)
if (GENMALLOC-FOUND)
   target_link_libraries(FParserTest PowerParser ${GENMALLOC_LIBRARY})
endif (GENMALLOC-FOUND)

########### pFParserTest target ###############
set(FParserTest_SRCS ${F90_SRCS})

add_executable(pFParserTest EXCLUDE_FROM_ALL ${FParserTest_SRCS})

find_library(GENMALLOC_LIBRARY genmalloc DIRS
   ${CMAKE_BINARY_DIR}/genmalloc
   ${CMAKE_BINARY_DIR}
   ${CMAKE_BINARY_DIR}/../genmalloc
   ../../genmalloc)
#message("GENMALLOC directory is ${GENMALLOC_LIBRARY}")
if (GENMALLOC-FOUND)
   set_target_properties(pFParserTest PROPERTIES COMPILE_DEFINITIONS HAVE_GENMALLOC)
endif (GENMALLOC-FOUND)

set_target_properties(pFParserTest PROPERTIES COMPILE_DEFINITIONS HAVE_MPI)
set_target_properties(pFParserTest PROPERTIES EXCLUDE_FROM_ALL TRUE)
set_target_properties(pFParserTest PROPERTIES EXCLUDE_FROM_DEFAULT_BUILD TRUE)
target_link_libraries(pFParserTest pPowerParser ${MPI_LIBRARIES})
if (GENMALLOC-FOUND)
   target_link_libraries(pFParserTest PowerParser ${GENMALLOC_LIBRARY})
endif (GENMALLOC-FOUND)

########### install files ###############

################# check/test ##################

add_test(ParserTest   ParserTest)
add_test(FParserTest  FParserTest)
add_test(pParserTest  mpirun -n 2 pParserTest)
add_test(pFParserTest mpirun -n 2 pFParserTest)

if (${CMAKE_PROJECT_NAME} MATCHES ${PROJECT_NAME})
  set(test_prefix "")
else (${CMAKE_PROJECT_NAME} MATCHES ${PROJECT_NAME})
  set(test_prefix PowerParser_)
endif (${CMAKE_PROJECT_NAME} MATCHES ${PROJECT_NAME})

set(CMAKE_CHECK_COMMAND ctest && cat Testing/Temporary/LastTest.log)

add_custom_target(${test_prefix}check COMMAND ${CMAKE_CHECK_COMMAND}
                  DEPENDS ParserTest FParserTest pParserTest pFParserTest)

########### clean files ################
SET_DIRECTORY_PROPERTIES(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "ParserTest")

