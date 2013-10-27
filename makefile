ifeq ($(GNATMAKE),)
GNATMAKE	=	gnatmake
endif

ifeq ($(GNATCLEAN),)
GNATCLEAN	=	gnatclean
endif

ifeq ($(D),1)
DEBUG		=	-gnatd.n
endif

C_OBJS		=	lib/.obj/version.o
#			obj/pixels.o

MATHS_C_OBJS	=	build_test/.obj/maths_sub.o

SRCS		=	src/sdl.ads

all: sdl_build.gpr  test.gpr

# TODO: Fix the compiler so we actually get shared libs!
#maths_build.gpr

#####################################################################################
# SDL library

sdl_build.gpr: $(C_OBJS) lib/libadasdl.a
	$(GNATMAKE) $(DEBUG) -p -gnat2012 -Psdl_build.gpr -largs $(C_OBJS)

lib/.obj/version.o: src/version.c
	gcc `sdl2-config --cflags` -c src/version.c -o lib/.obj/version.o

#obj/pixels.o: src/pixels.c
#	gcc `sdl2-config --cflags` -c src/pixels.c -o obj/pixels.o

#####################################################################################
# Maths library

maths_build.gpr: $(MATHS_C_OBJS) build_test/libmaths.so
	$(GNATMAKE) $(DEBUG) -p -gnat2012 -Pmaths_build.gpr
#-largs $(MATHS_C_OBJS)

build_test/.obj/maths_sub.o: src/maths/maths_sub.c
	gcc `sdl2-config --cflags` -c src/maths/maths_sub.c -o build_test/.obj/maths_sub.o

#####################################################################################
# Tests

test.gpr: build_test/test
	$(GNATMAKE) $(DEBUG) -p -gnat2012 -Ptest.gpr -largs `sdl2-config --static-libs`

#####################################################################################

.PHONY: lib/libadasdl.a build_test/libmaths.so build_test/test clean clean_test

clean: clean_test
	$(GNATCLEAN) -Psdl_build.gpr
	rm $(C_OBJS)

clean_test:
	$(GNATCLEAN) -Ptest.gpr
