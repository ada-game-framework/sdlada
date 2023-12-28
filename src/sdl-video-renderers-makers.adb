--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
private with SDL.C_Pointers;

package body SDL.Video.Renderers.Makers is
   package C renames Interfaces.C;

   function Get_Internal_Window (Self : in SDL.Video.Windows.Window) return SDL.C_Pointers.Windows_Pointer with
     Convention => Ada,
     Import     => True;

   function Get_Internal_Surface (Self : in SDL.Video.Surfaces.Surface)
                                  return SDL.Video.Surfaces.Internal_Surface_Pointer with
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

      function SDL_Create_Software_Renderer (S : in SDL.Video.Surfaces.Internal_Surface_Pointer)
                                             return SDL.C_Pointers.Renderer_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateSoftwareRenderer";
   begin
      Rend.Internal := SDL_Create_Software_Renderer (Get_Internal_Surface (Surface));
      Rend.Owns     := True;
   end Create;
end SDL.Video.Renderers.Makers;
