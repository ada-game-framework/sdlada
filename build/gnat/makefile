DESTDIR		?=	/opt

GPRMAKE		=	gprbuild
GPRINSTALL	=	gprinstall
GPRCLEAN	=	gprclean
INSTALL		=	install

SDL_MODE	?=	debug
SDL_PLATFORM	?=	linux

SRC_DIR		=	../..

SRCS		=	src/sdl.ads

GEN_KB		=	gen/src/sdl-events-keyboards.ads

ifeq (,$(filter $(SDL_PLATFORM), "linux" "windows" "bsd" "android"))
SDL2_CONFIG	=	-cargs `sdl2-config --cflags` -largs `sdl2-config --libs` -lSDL2_ttf -lSDL2_image -lSDL2_mixer
else ifeq ($(SDL_PLATFORM),macosx)
SDL2_CONFIG	=
endif

# With GNAT, all static libs end with .a
LIB_NAME	=	lib/libadasdl.a

.PHONY: sdlada.gpr test_maths_build.gpr test.gpr tools.gpr

all: tools $(LIB_NAME)

# TODO: Fix the compiler so we actually get shared libs!
#

#####################################################################################
# SDL library

$(LIB_NAME): sdlada.gpr $(GEN_KB)
	$(GPRMAKE) -p -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) \
		-Psdlada.gpr

.PHONY: clean-lib

clean-lib:
	$(GPRCLEAN) -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) \
		-Psdlada.gpr


#####################################################################################
# Tools

tools: mk_dirs $(GEN_KB)

.PHONY: gen/src

mk_dirs: gen/src
	mkdir -p gen/src/

$(GEN_KB): gen/$(SDL_MODE)/tools/gen_keyboard
	./gen/$(SDL_MODE)/tools/gen_keyboard > $@

gen/$(SDL_MODE)/tools/gen_keyboard:
	$(GPRMAKE) -p -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) \
		-Ptools.gpr

.PHONY: clean-kb clean-tools

clean-kb:
	-rm $(GEN_KB)

clean-tools:

	$(GPRCLEAN) -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) -Ptools.gpr

#####################################################################################
# Unit tests

unit_tests: $(LIB_NAME) build_unit_tests/unit_tests

build_unit_tests/unit_tests:
	$(GPRMAKE) -p -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) \
		-Punit_tests.gpr ${SDL2_CONFIG}

.PHONY: clean-unit_tests

clean-unit_tests:
	-$(GPRCLEAN) -XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) -Punit_tests.gpr

#####################################################################################
.PHONY: install

install: all
	$(GPRINSTALL) --prefix=$(DESTDIR) --build-name=$(SDL_MODE) -p -f \
		-XSDL_MODE=$(SDL_MODE) -XSDL_PLATFORM=$(SDL_PLATFORM) \
		-Psdlada.gpr

#####################################################################################

.PHONY: clean

clean: clean-kb clean-tools clean-unit_tests clean-lib
