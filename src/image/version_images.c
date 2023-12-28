/***********************************************************************************************************************
 * This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
 **********************************************************************************************************************/
#ifdef __APPLE__
    #if defined(SDL_HOMEBREW) || defined(SDL_MACPORTS)
        #include <SDL2/SDL_image.h>
    #else
        #include <SDL2_image/SDL_image.h>
    #endif
#else
#include <SDL_image.h>
#endif

/* We need to define some constants here so we can get access to the values that in #define form from Ada.
 */
const Uint8 SDL_Ada_Image_Major_Version = SDL_IMAGE_MAJOR_VERSION;
const Uint8 SDL_Ada_Image_Minor_Version = SDL_IMAGE_MINOR_VERSION;
const Uint8 SDL_Ada_Image_Patch_Version = SDL_IMAGE_PATCHLEVEL;
