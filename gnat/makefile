# Portable Makefile for SDLAda.

# This part should be kept in sync with README.md.

# These variables are intended for override on the Make command line.
SDL_PLATFORM                  = linux
SDL_MODE                      = debug
GPRBUILD                      = gprbuild
GPRBUILD_OPTIONS              =
GPRINSTALL                    = gprinstall
GPRINSTALL_OPTIONS            =
PKG_CONFIG                    = pkg-config
DESTDIR                       = /opt

# Build flags and shared object names only affect projects. If they
# exist in the environment, gprbuild will see them too.  If the Make
# command line contains overrides, Make will export them.

.PHONY: all all-dynamic check check-dynamic install install-dynamic clean

######################################################################
# This script provides the traditional Make interface, but might as
# well be implemented in any script language, since all dependency
# handling is delegated to gprbuild.

# The purpose of aggregate projects is to avoid unneeded executions of
# gprbuild, with the slow initialization phase.

pkgconfig_modules = sdl2 SDL2_image SDL2_ttf
assignments = \
  -XSDL_PLATFORM='$(SDL_PLATFORM)' \
  -XSDL_MODE='$(SDL_MODE)' \
  -Xpkgconfig_cflags="`$(PKG_CONFIG) --cflags $(pkgconfig_modules)`" \
  -Xpkgconfig_libs="`$(PKG_CONFIG) --libs $(pkgconfig_modules)`"

# -m matters because we always rewrite the generated source.
build_opts = -m -p $(GPRBUILD_OPTIONS) $(assignments)
all: generate-keys
	$(GPRBUILD) all_libraries.gpr -Xlibrary_kind=static $(build_opts)
all-dynamic: generate-keys
	$(GPRBUILD) all_libraries.gpr -Xlibrary_kind=dynamic $(build_opts)
check: generate-keys
	$(GPRBUILD) all_tests.gpr -Xlibrary_kind=static $(build_opts) \
	  -largs `$(PKG_CONFIG) --libs --static $(pkgconfig_modules)`

check-dynamic: generate-keys
	$(GPRBUILD) all_tests.gpr -Xlibrary_kind=dynamic $(build_opts)

generator = gen/$(SDL_MODE)-$(SDL_PLATFORM)-static-exe-tools/gen_keyboard
generated = gen/sdl-events-keyboards.ads
.PHONY: generate-keys
generate-keys:
	$(GPRBUILD) tools.gpr -Xlibrary_kind=static $(build_opts)
	$(generator) > $(generated)

install_opts = -f -p --prefix=$(DESTDIR) --build-name=$(SDL_MODE) \
  $(GPRINSTALL_OPTIONS) $(assignments)
install-dynamic: all-dynamic
	$(GPRINSTALL) all_libraries.gpr -Xlibrary_kind=dynamic $(install_opts)
install: all
	$(GPRINSTALL) all_libraries.gpr -Xlibrary_kind=static $(install_opts)

clean:
	rm -fr gen
