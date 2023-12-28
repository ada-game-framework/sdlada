--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.C_Pointers
--
--  This private package contains all the types representing the internal C pointers for various objects.
--------------------------------------------------------------------------------------------------------------------
private package SDL.C_Pointers is
   pragma Preelaborate;

   type Windows is null record;
   type Windows_Pointer is access all Windows with
     Convention => C;

   type Renderers is null record;
   type Renderer_Pointer is access all Renderers with
     Convention => C;

   type Textures is null record;
   type Texture_Pointer is access all Textures with
     Convention => C;

   type GL_Contexts is null record;
   type GL_Context_Pointer is access all GL_Contexts with
     Convention => C;

   type Joysticks is null record;
   type Joystick_Pointer is access all Joysticks with
     Convention => C;

   type Game_Controller is null record;
   type Game_Controller_Pointer is access all Game_Controller with
     Convention => C;

   type Cursors is null record;
   type Cursor_Pointer is access all Cursors with
     Convention => C;
end SDL.C_Pointers;
