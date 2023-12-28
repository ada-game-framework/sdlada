--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Events.Windows
--
--  Window specific events.
--------------------------------------------------------------------------------------------------------------------
with SDL.Video.Windows;

package SDL.Events.Windows is
   pragma Preelaborate;

   --  Window events.
   Window                     : constant Event_Types := 16#0000_0200#;
   System_Window_Manager      : constant Event_Types := Window + 1;

   type Window_Event_ID is
     (None,
      Shown,
      Hidden,
      Exposed,
      Moved,
      Resized,
      Size_Changed,
      Minimised,
      Maximised,
      Restored,
      Enter,
      Leave,
      Focus_Gained,
      Focus_Lost,
      Close,
      Take_Focus,
      Hit_Test) with
     Convention => C;

   type Window_Events is
      record
         Event_Type : Event_Types;           --  Will be set to Window.
         Time_Stamp : Time_Stamps;

         ID         : SDL.Video.Windows.ID;
         Event_ID   : Window_Event_ID;
         Padding_1  : Padding_8;
         Padding_2  : Padding_8;
         Padding_3  : Padding_8;
         Data_1     : Interfaces.Integer_32;
         Data_2     : Interfaces.Integer_32;
      end record with
     Convention => C;
private
   for Window_Events use
      record
         Event_Type at 0 * SDL.Word range  0 .. 31;
         Time_Stamp at 1 * SDL.Word range  0 .. 31;

         ID         at 2 * SDL.Word range  0 .. 31;
         Event_ID   at 3 * SDL.Word range  0 ..  7;
         Padding_1  at 3 * SDL.Word range  8 .. 15;
         Padding_2  at 3 * SDL.Word range 16 .. 23;
         Padding_3  at 3 * SDL.Word range 24 .. 31;
         Data_1     at 4 * SDL.Word range  0 .. 31;
         Data_2     at 5 * SDL.Word range  0 .. 31;
      end record;
end SDL.Events.Windows;
