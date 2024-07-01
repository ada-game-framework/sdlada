--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Inputs.Keyboards
--------------------------------------------------------------------------------------------------------------------
with SDL.Events.Keyboards;
with SDL.Video.Rectangles;
with SDL.Video.Windows;

package SDL.Inputs.Keyboards is
   pragma Preelaborate;

   procedure Clear_Composition with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ClearComposition";

   function Get_Focus return SDL.Video.Windows.ID with
     Inline => True;

   type Key_State_Array is array (SDL.Events.Keyboards.Scan_Codes) of Boolean with
     Convention => C;

   type Key_State_Access is access constant Key_State_Array with
     Convention => C;

   function Get_State return Key_State_Access;

   function Get_Modifiers return SDL.Events.Keyboards.Key_Modifiers with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GetModState";

   procedure Set_Modifiers (Modifiers : in SDL.Events.Keyboards.Key_Modifiers) with
      Import        => True,
      Convention    => C,
      External_Name => "SDL_SetModState";

   --  Screen keyboard.
   function Supports_Screen_Keyboard return Boolean with
     Inline => True;

   function Is_Screen_Keyboard_Visible (Window : in SDL.Video.Windows.Window) return Boolean with
     Inline => True;

   --  Text input.
   function Is_Text_Input_Enabled return Boolean with
     Inline => True;

   procedure Set_Text_Input_Rectangle (Rectangle : in SDL.Video.Rectangles.Rectangle) with
     Inline => True;

   procedure Start_Text_Input with
      Import        => True,
      Convention    => C,
      External_Name => "SDL_StartTextInput";

   procedure Stop_Text_Input with
      Import        => True,
      Convention    => C,
      External_Name => "SDL_StopTextInput";
end SDL.Inputs.Keyboards;
