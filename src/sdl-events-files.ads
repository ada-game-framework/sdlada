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
--  SDL.Events.Files
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C.Strings;
with System;

package SDL.Events.Files is
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
