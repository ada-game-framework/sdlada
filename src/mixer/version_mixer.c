/***********************************************************************************************************************
 *  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
 **********************************************************************************************************************/
#ifdef __APPLE__
#include <SDL2_mixer/SDL_mixer.h>
#else
#include <SDL_mixer.h>
#endif

/* We need to define some constants here so we can get access to the values that in #define form from Ada.
 */
const Uint8 SDL_Ada_Mixer_Major_Version = SDL_MIXER_MAJOR_VERSION;
const Uint8 SDL_Ada_Mixer_Minor_Version = SDL_MIXER_MINOR_VERSION;
const Uint8 SDL_Ada_Mixer_Patch_Version = SDL_MIXER_PATCHLEVEL;
