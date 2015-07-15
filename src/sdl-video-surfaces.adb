--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014-2015 Luke A. Guest
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
--------------------------------------------------------------------------------------------------------------------
package body SDL.Video.Surfaces is
   use type SDL.C_Pointers.Surface_Pointer;

   overriding
   procedure Finalize (Self : in out Surface) is
      --        procedure SDL_Destroy_Renderer (R : in SDL.C_Pointers.Renderer_Pointer) with
      --          Import        => True,
      --          Convention    => C,
      --          External_Name => "SDL_DestroyRenderer";
   begin
      if Self.Internal /= null and then Self.Owns then
         --           SDL_Destroy_Renderer (Self.Internal);

         Self.Internal := null;
      end if;
   end Finalize;

   function Get_Internal_Surface (Self : in Surface) return SDL.C_Pointers.Surface_Pointer is
   begin
      return Self.Internal;
   end Get_Internal_Surface;
end SDL.Video.Surfaces;
