--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL_Linker
--------------------------------------------------------------------------------------------------------------------
package SDL_Linker is
   pragma Pure;

   Options : constant String := "-Wl,-F/Library/Frameworks,-framework,SDL2";
end SDL_Linker;
