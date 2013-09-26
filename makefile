C_OBJS	=	obj/version.o

SRCS	=	src/sdl.ads

all: sdl_build.gpr test.gpr

sdl_build.gpr: $(C_OBJS) lib/libadasdl.a
	gnatmake -p -gnat2012 -Psdl_build.gpr -largs $(C_OBJS)

test.gpr: build_test/test
	gnatmake -p -gnat2012 -Ptest.gpr -largs `sdl2-config --static-libs`

obj/version.o: src/version.c
	gcc `sdl2-config --cflags` -c src/version.c -o obj/version.o

.PHONY: lib/libadasdl.a build_test/test clean

clean:
	gnatclean -Psdl_build.gpr
	gnatclean -Ptest.gpr
