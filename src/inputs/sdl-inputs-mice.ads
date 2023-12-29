--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Inputs.Mice
--------------------------------------------------------------------------------------------------------------------

with SDL.Events.Mice;
with SDL.Video.Windows;

package SDL.Inputs.Mice is
   pragma Preelaborate;

   Mice_Error : exception;

   type Cursor_Toggles is (Off, On);

   for Cursor_Toggles use (Off => 0, On => 1);

   type Supported is (Yes, No);

   function Capture (Enabled : in Boolean) return Supported with
     Inline;

   function Get_Global_State (X_Relative, Y_Relative : out SDL.Events.Mice.Movement_Values) return
     SDL.Events.Mice.Button_Masks;

   --  SDL_GetMouseFocus

   function Get_State (X_Relative, Y_Relative : out SDL.Events.Mice.Movement_Values) return
     SDL.Events.Mice.Button_Masks;

   function In_Relative_Mode return Boolean with
     Inline;

   function Get_Relative_State (X_Relative, Y_Relative : out SDL.Events.Mice.Movement_Values) return
     SDL.Events.Mice.Button_Masks;

   --  SDL_SetCursor

   procedure Set_Relative_Mode (Enable : in Boolean := True) with
     Inline;

   procedure Show_Cursor (Enable : in Boolean := True) with
     Inline;

   function Is_Cursor_Shown return Boolean;

   --  Move the mouse to (x, y) on the screen.
   --  If in relative mode, the co-ordinates can be negative.
   procedure Warp (To : in SDL.Coordinates);
   --
   --  Move the mouse to (x, y) in the specified window.
   procedure Warp (Window : in SDL.Video.Windows.Window; To : in SDL.Coordinates);
end SDL.Inputs.Mice;
