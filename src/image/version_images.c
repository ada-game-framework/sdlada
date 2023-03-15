/***********************************************************************************************************************
 * Copyright (c) 2013-2020, Luke A. Guest
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 *    1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 *
 *    2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 *
 *    3. This notice may not be removed or altered from any source
 *    distribution.
 **********************************************************************************************************************/
#ifdef __APPLE__
    #ifdef SDL_HOMEBREW
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
