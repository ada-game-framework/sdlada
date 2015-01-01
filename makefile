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

ifeq ($(STATIC),)
STATIC		=	yes
endif

ifeq ($(STATIC),yes)
SDL2_LIBS	=	`sdl2-config --libs`
else
SDL2_LIBS	=	`sdl2-config --static-libs`
endif

all: sdl_build.gpr test_maths_build.gpr test.gpr

# TODO: Fix the compiler so we actually get shared libs!
#

#####################################################################################
# SDL library

sdl_build.gpr: lib/libadasdl.a
	$(GPRMAKE) $(DEBUG) -p -gnat2012 -XPLATFORM=$(PLATFORM) -Psdl_build.gpr -cargs `sdl2-config --cflags`

#####################################################################################
# Maths library

test_maths_build.gpr: build_test/libmaths.so
	$(GPRMAKE) $(DEBUG) -p -gnat2012 -XPLATFORM=$(PLATFORM) -Ptest_maths_build.gpr

#####################################################################################
# Tests

test.gpr: build_test/test
	$(GPRMAKE) $(DEBUG) -p -gnat2012 -XPLATFORM=$(PLATFORM) -Ptest.gpr -largs $(SDL2_LIBS)

#####################################################################################

.PHONY: lib/libadasdl.a build_test/libmaths.so build_test/test clean clean_test

clean: clean_test
	$(GPRCLEAN) -Psdl_build.gpr -XPLATFORM=$(PLATFORM)
	$(GPRCLEAN) -Ptest_maths_build.gpr -XPLATFORM=$(PLATFORM)
	$(GPRCLEAN) -Ptest.gpr -XPLATFORM=$(PLATFORM)
