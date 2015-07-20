--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014 Luke A. Guest
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
--  SDL.Inputs.Keyboards
--------------------------------------------------------------------------------------------------------------------
with SDL.Events.Keyboards;
with SDL.Video.Rectangles;
with SDL.Video.Windows;

package SDL.Inputs.Keyboards is
   function Get_Focus return SDL.Video.Windows.ID with
     Inline => True;

   --  TODO:
   --     type Key_State_Array is array () of SDL.Video.Windows.Scan_Codes;
   --     function Keys return

   function Get_Modifiers return SDL.Events.Keyboards.Key_Modifiers with
     Inline => True;

   procedure Set_Modifiers (Modifiers : in SDL.Events.Keyboards.Key_Modifiers) with
     Inline => True;

   --  Screen keyboard.
   function Supports_Screen_Keyboard return Boolean;
   function Is_Screen_Keyboard_Visible (Window : in SDL.Video.Windows.Window) return Boolean;

   --  Text input.
   function Is_Text_Input_Enabled return Boolean;
   procedure Set_Text_Input_Rectangle (Rectangle : in SDL.Video.Rectangles.Rectangle) with
     Inline => True;

   procedure Start_Text_Input with
     Inline => True;

   procedure Stop_Text_Input with
     Inline => True;
end SDL.Inputs.Keyboards;
