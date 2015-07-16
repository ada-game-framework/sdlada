DESTDIR		?=	/opt

GPRMAKE		=	gprbuild
GPRINSTALL	=	gprinstall
GPRCLEAN	=	gprclean

SDL_MODE	?=	debug

SRCS		=	src/sdl.ads

ifeq ($(SDL_BUILD),)
SDL_BUILD	=	static
endif

ifeq ($(SDL_BUILD),static)
SDL2_LIBS	=	`sdl2-config --static-libs`
else
SDL2_LIBS	=	`sdl2-config --libs`
endif

all: sdl_build.gpr test_maths_build.gpr test.gpr

# TODO: Fix the compiler so we actually get shared libs!
#

#####################################################################################
# SDL library

sdl_build.gpr: lib/libadasdl.a
	$(GPRMAKE) -p -gnat2012 -XSDL_BUILD=$(SDL_BUILD) -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) \
		-Psdl_build.gpr -cargs `sdl2-config --cflags`

#####################################################################################
# Maths library

test_maths_build.gpr: build_test/libmaths.so
	$(GPRMAKE) -p -gnat2012 -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) -Ptest_maths_build.gpr

#####################################################################################
# Tests

test.gpr: build_test/test
	$(GPRMAKE) -p -gnat2012 -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) -Ptest.gpr -largs $(SDL2_LIBS)

#####################################################################################
# Unit tests

unit_tests: unit_tests.gpr

unit_tests.gpr: build_unit_tests/unit_tests
	$(GPRMAKE) -p -gnat2012 -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) -Punit_tests.gpr \
		-largs $(SDL2_LIBS)

#####################################################################################
.PHONY: install

install:
	$(GPRINSTALL) --prefix=$(DESTDIR)/sdlada -p \
		-XSDL_BUILD=$(SDL_BUILD) -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) \
		-Psdl_build.gpr

#####################################################################################

.PHONY: lib/libadasdl.a build_test/libmaths.so build_test/test build_unit_tests/unit_tests clean clean_test

clean: clean_test
	$(GPRCLEAN) -Psdl_build.gpr -XSDL_PLATFORM=$(SDL_PLATFORM)
	$(GPRCLEAN) -Ptest_maths_build.gpr -XSDL_PLATFORM=$(SDL_PLATFORM)
	$(GPRCLEAN) -Ptest.gpr -XSDL_PLATFORM=$(SDL_PLATFORM)
	$(GPRCLEAN) -Punit_tests.gpr -XSDL_PLATFORM=$(SDL_PLATFORM)

