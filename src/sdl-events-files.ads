--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Events.Files
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C.Strings;
with System;

package SDL.Events.Files is
   pragma Preelaborate;

   --  Drag and drop events.
   Drop_File : constant Event_Types := 16#0000_1000#;

   type Drop_Events is
      record
         Event_Type : Event_Types;                     --  Will be set to Drop_File.
         Time_Stamp : Time_Stamps;

         File_Name  : Interfaces.C.Strings.chars_ptr;  -- User *must* call Free on this.
      end record with
     Convention => C;
private
   for Drop_Events use
      record
         Event_Type at  0 * SDL.Word range  0  .. 31;
         Time_Stamp at  1 * SDL.Word range  0  .. 31;

         File_Name  at  2 * SDL.Word range  0  .. System.Word_Size - 1;  --  This will depend on platform.
      end record;
end SDL.Events.Files;
