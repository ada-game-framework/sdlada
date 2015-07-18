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
with Interfaces.C;
private with SDL.C_Pointers;
with SDL.Error;

package body SDL.Video.Renderers.Makers is
   package C renames Interfaces.C;

   use type C.int;
   use type SDL.C_Pointers.Texture_Pointer;

   function Get_Internal_Window (Self : in SDL.Video.Windows.Window) return SDL.C_Pointers.Windows_Pointer with
     Convention => Ada,
     Import     => True;

   function Get_Internal_Surface (Self : in SDL.Video.Surfaces.Surface) return SDL.C_Pointers.Surface_Pointer with
     Import     => True,
     Convention => Ada;

   procedure Create
     (Rend   : in out Renderer;
      Window : in out SDL.Video.Windows.Window;
      Driver : in Positive;
      Flags  : in Renderer_Flags := Default_Renderer_Flags) is

      function SDL_Create_Renderer (W : in SDL.C_Pointers.Windows_Pointer; Index : in C.int; Flags : in Renderer_Flags)
                                    return SDL.C_Pointers.Renderer_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateRenderer";
   begin
      Rend.Internal := SDL_Create_Renderer (Get_Internal_Window (Window), C.int (Driver), Flags);
   end Create;

   procedure Create
     (Rend   : in out Renderer;
      Window : in out SDL.Video.Windows.Window;
      Flags  : in Renderer_Flags := Default_Renderer_Flags) is

      function SDL_Create_Renderer (W : in SDL.C_Pointers.Windows_Pointer; Index : in C.int; Flags : in Renderer_Flags)
                                    return SDL.C_Pointers.Renderer_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateRenderer";
   begin
      Rend.Internal := SDL_Create_Renderer (Get_Internal_Window (Window), -1, Flags);
      Rend.Owns     := True;
   end Create;

   procedure Create
     (Rend    : in out Renderer;
      Surface : in SDL.Video.Surfaces.Surface) is

      function SDL_Create_Software_Renderer (S : in SDL.C_Pointers.Surface_Pointer)
                                             return SDL.C_Pointers.Renderer_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateSoftwareRenderer";
   begin
      Rend.Internal := SDL_Create_Software_Renderer (Get_Internal_Surface (Surface));
      Rend.Owns     := True;
   end Create;
end SDL.Video.Renderers.Makers;
