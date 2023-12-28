/***********************************************************************************************************************
 *  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
 **********************************************************************************************************************/
#ifdef __APPLE__
    #if defined(SDL_HOMEBREW) || defined(SDL_MACPORTS)
        #include <SDL2/SDL_ttf.h>
    #else
        #include <SDL2_ttf/SDL_ttf.h>
    #endif
#else
#include <SDL_ttf.h>
#endif

/* We need to define some constants here so we can get access to the values that in #define form from Ada.
 */
const Uint8 SDL_Ada_TTF_Major_Version = SDL_TTF_MAJOR_VERSION;
const Uint8 SDL_Ada_TTF_Minor_Version = SDL_TTF_MINOR_VERSION;
const Uint8 SDL_Ada_TTF_Patch_Version = SDL_TTF_PATCHLEVEL;
