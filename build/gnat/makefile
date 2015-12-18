DESTDIR		?=	/opt

GPRMAKE		=	gprbuild
GPRINSTALL	=	gprinstall
GPRCLEAN	=	gprclean
INSTALL		=	install

SDL_MODE	?=	debug

SRCS		=	src/sdl.ads

ifeq ($(SDL_BUILD),)
SDL_BUILD	=	static
endif

ifeq ($(SDL_BUILD),static)
SDL2_LIBS	=	`sdl2-config --static-libs`
LIB_NAME	=	lib/libadasdl.a
else
SDL2_LIBS	=	`sdl2-config --libs`
LIB_NAME	=	lib/libadasdl.so.2.1.2
endif

.PHONY: sdlada.gpr test_maths_build.gpr test.gpr tools.gpr

all: tools $(LIB_NAME) tests

# TODO: Fix the compiler so we actually get shared libs!
#

#####################################################################################
# SDL library

$(LIB_NAME): sdlada.gpr gen/sdl-events-keyboards.ads
	$(GPRMAKE) -p -gnat2012 -XSDL_BUILD=$(SDL_BUILD) -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) \
		-Psdlada.gpr -cargs `sdl2-config --cflags`

#####################################################################################
# Tools

tools: gen/sdl-events-keyboards.ads

gen/sdl-events-keyboards.ads: build_tools/gen_keyboard
	./build_tools/gen_keyboard > $@

build_tools/gen_keyboard:
	$(GPRMAKE) -p -gnat2012 -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) -Ptools.gpr

#####################################################################################
# Tests

tests: $(LIB_NAME) build_test/test

build_test/test: build_test/libmaths.so
	$(GPRMAKE) -p -gnat2012 -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) -Ptest.gpr -largs $(SDL2_LIBS)

# Maths library

build_test/libmaths.so:
	$(GPRMAKE) -p -gnat2012 -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) -Ptest_maths_build.gpr

#####################################################################################
# Unit tests

unit_tests: $(LIB_NAME) build_unit_tests/unit_tests

build_unit_tests/unit_tests:
	$(GPRMAKE) -p -gnat2012 -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) -Punit_tests.gpr \
		-largs $(SDL2_LIBS)

#####################################################################################
.PHONY: install

install: all
	$(GPRINSTALL) --prefix=$(DESTDIR)/sdlada --build-name=$(SDL_MODE).$(SDL_BUILD) -p -f \
		-XSDL_BUILD=$(SDL_BUILD) -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) \
		-Psdlada.gpr
	$(INSTALL) --mode=0644 sdl_version.gpr $(DESTDIR)/sdlada/share/gpr/

#####################################################################################

clean:
	$(GPRCLEAN) -Psdlada.gpr -XSDL_PLATFORM=$(SDL_PLATFORM)
	$(GPRCLEAN) -Ptest_maths_build.gpr -XSDL_PLATFORM=$(SDL_PLATFORM)
	$(GPRCLEAN) -Ptest.gpr -XSDL_PLATFORM=$(SDL_PLATFORM)
	$(GPRCLEAN) -Punit_tests.gpr -XSDL_PLATFORM=$(SDL_PLATFORM)
	$(GPRCLEAN) -Ptools.gpr -XSDL_PLATFORM=$(SDL_PLATFORM)
