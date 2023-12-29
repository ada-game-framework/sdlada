--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Events
--
--  Mapping to the underlying event handling system.
--
--  WARNING!!!!
--    I wanted to experiment with the event system and possibly hide all this and create an abstraction in another
--    task so as to separate out the events from the main window. This could change. I really don't know yet.
--------------------------------------------------------------------------------------------------------------------
package SDL.Events is
   pragma Pure;

   type Event_Types is mod 2 ** 32 with
     Convention => C;

   type Button_State is (Released, Pressed) with
     Convention => C;

   for Button_State use (Released => 0, Pressed => 1);

   -----------------------------------------------------------------------------------------------------------------
   --  Event types.
   -----------------------------------------------------------------------------------------------------------------

   --  Handled via 'First attribute.
   First_Event                : constant Event_Types := 16#0000_0000#;

   --  Application events.
   Quit                       : constant Event_Types := 16#0000_0100#;

   --  Mobile events.
   App_Terminating            : constant Event_Types := Quit + 1;
   App_Low_Memory             : constant Event_Types := Quit + 2;
   App_Will_Enter_Background  : constant Event_Types := Quit + 3;
   App_Did_Enter_Background   : constant Event_Types := Quit + 4;
   App_Will_Enter_Foreground  : constant Event_Types := Quit + 5;
   App_Did_Enter_Foreground   : constant Event_Types := Quit + 6;

   --  Clipboard events.
   Clipboard_Update           : constant Event_Types := 16#0000_0900#;

   --  TODO: Audio hot plug events for 2.0.4

   --  User events.
   User                       : constant Event_Types := 16#0000_8000#;

   Last_Event                 : constant Event_Types := 16#0000_FFFF#;

   type Padding_8 is mod 2 ** 8 with
     Convention => C,
     Size       => 8;

   type Padding_16 is mod 2 ** 16 with
     Convention => C,
     Size       => 16;

   type Time_Stamps is mod 2 ** 32 with
     Convention => C;

   type Common_Events is
      record
         Event_Type : Event_Types;
         Time_Stamp : Time_Stamps;
      end record with
     Convention => C;

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Touch finger events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Multi gesture events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Dollar gesture events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Drop events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: User events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: System window manager events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Audio events - 2.0.4
   -----------------------------------------------------------------------------------------------------------------

private
   for Common_Events use
      record
         Event_Type at 0 * SDL.Word range  0 .. 31;
         Time_Stamp at 1 * SDL.Word range  0 .. 31;
      end record;

   --     for Text_Editing_Events use
   --        record
   --           Event_Type at 0 * SDL.Word range  0  .. 31;
   --           Time_Stamp at 1 * SDL.Word range  0  .. 31;
   --
   --           ID         at 2 * SDL.Word range  0  .. 31;
   --           State      at 3 * SDL.Word range  0  ..  7;
   --           Repeat     at 3 * SDL.Word range  8  .. 15;
   --           Padding_2  at 3 * SDL.Word range  16 .. 23;
   --           Padding_3  at 3 * SDL.Word range  24 .. 31;
   --        end record;
end SDL.Events;
