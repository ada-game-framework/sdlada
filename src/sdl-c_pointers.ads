--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2020, Luke A. Guest
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
