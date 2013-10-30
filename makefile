ifeq ($(GNATMAKE),)
GPRMAKE		=	gprbuild
endif

ifeq ($(GNATCLEAN),)
GPRCLEAN	=	gprclean
endif

ifeq ($(D),1)
DEBUG		=	-gnatd.n
endif

SRCS		=	src/sdl.ads

all: sdl_build.gpr test_maths_build.gpr test.gpr

# TODO: Fix the compiler so we actually get shared libs!
#

#####################################################################################
# SDL library

sdl_build.gpr: lib/libadasdl.a
	$(GPRMAKE) $(DEBUG) -p -gnat2012 -Psdl_build.gpr -cargs `sdl2-config --cflags`

#####################################################################################
# Maths library

test_maths_build.gpr: build_test/libmaths.so
	$(GPRMAKE) $(DEBUG) -p -gnat2012 -Ptest_maths_build.gpr

#####################################################################################
# Tests

test.gpr: build_test/test
	$(GPRMAKE) $(DEBUG) -p -gnat2012 -Ptest.gpr -largs `sdl2-config --static-libs`

#####################################################################################

.PHONY: lib/libadasdl.a build_test/libmaths.so build_test/test clean clean_test

clean: clean_test
	$(GPRCLEAN) -Psdl_build.gpr
	$(GPRCLEAN) -Ptest_maths_build.gpr
	$(GPRCLEAN) -Ptest.gpr
