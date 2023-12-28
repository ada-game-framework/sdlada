/***********************************************************************************************************************
 *  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
 **********************************************************************************************************************/
#ifdef __APPLE__
#include <SDL2/SDL.h>
#else
#include <SDL.h>
#endif

/* We need to define some constants here so we can get access to the values that in #define form from Ada.
 */
const Uint8 SDL_Ada_Major_Version = SDL_MAJOR_VERSION;
const Uint8 SDL_Ada_Minor_Version = SDL_MINOR_VERSION;
const Uint8 SDL_Ada_Patch_Version = SDL_PATCHLEVEL;
