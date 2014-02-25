cmake_minimum_required(VERSION 2.8)

project(libparser)

set(libparser 0.1)

set(libparser_CXX_SRCS
	ast.cpp
	parser.cpp
)
set(libparser_CXX_SRCS
	ast.cpp
	parser.cpp
)

add_library(parser SHARED ${libparser_CXX_SRCS})
add_library(parser-static STATIC ${libparser_CXX_SRCS})
set_target_properties(parser-static PROPERTIES
	POSITION_INDEPENDENT_CODE true
	OUTPUT_NAME "parser")

list(APPEND CMAKE_CXX_FLAGS "-std=c++11")

option(BUILD_DOCUMENTATION "Use Doxygen to create the HTML based API documentation" OFF)
if(BUILD_DOCUMENTATION)
	FIND_PACKAGE(Doxygen)
	if (NOT DOXYGEN_FOUND)
		message(FATAL_ERROR 
			"Doxygen is needed to build the documentation. Please install it correctly")
	endif()
	#-- Configure the Template Doxyfile for our specific project
	configure_file(Doxyfile.in 
	               ${PROJECT_BINARY_DIR}/Doxyfile @ONLY IMMEDIATE)
	#-- Add a custom target to run Doxygen when ever the project is built
	add_custom_target (Docs ALL 
	                   COMMAND ${DOXYGEN_EXECUTABLE} ${PROJECT_BINARY_DIR}/Doxyfile
	                   SOURCES ${PROJECT_BINARY_DIR}/Doxyfile)
endif()

