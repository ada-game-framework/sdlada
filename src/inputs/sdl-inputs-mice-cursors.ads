--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Inputs.Mice.Cursors
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
private with SDL.C_Pointers;

package SDL.Inputs.Mice.Cursors is
   pragma Preelaborate;

   --  Don't confuse this package with any type of Ada iterator, this is for visual mouse cursors.

   type Cursor is new Ada.Finalization.Limited_Controlled with private;

   overriding
   procedure Finalize (Self : in out Cursor);

   type System_Cursors is
     (Arrow,
      I_Beam,
      Wait,
      Cross_Hair,
      Wait_Arrow,
      Size_NWSE,
      Size_NESW,
      Size_WE,
      size_NS,
      Size_All,
      No,
      Hand) with
     Convention => C;

   --  SDL_CreateColorCursor
   --  SDL_CreateCursor

   procedure Create_System_Cursor (Self : in out Cursor; Cursor_Name : System_Cursors);

   procedure Get_Cursor (Self : in out Cursor);

   procedure Set_Cursor (Self : in Cursor);
private
   type Cursor is new Ada.Finalization.Limited_Controlled with
      record
         Internal : SDL.C_Pointers.Cursor_Pointer := null;
         Owns     : Boolean                       := True;
      end record;
end SDL.Inputs.Mice.Cursors;
