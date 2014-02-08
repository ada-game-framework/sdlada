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
--  SDL.Libraries
--
--  Mechanism for accessing shared objects (libraries).
--------------------------------------------------------------------------------------------------------------------
with System;
with Ada.Finalization;

package SDL.Libraries is
   Library_Error : exception;

   type Handles is new Ada.Finalization.Limited_Controlled with private;

   Null_Handle : constant Handles;

   procedure Load (Self : out Handles; Name : in String);
   procedure Unload (Self : in out Handles);-- with
--     Static_Predicate => Handle /= Null_Handle;

   generic
      type Access_To_Sub_Program is private;

      Name : String;
   function Load_Sub_Program (From_Library : in Handles) return Access_To_Sub_Program;
private
   type Handles is new Ada.Finalization.Limited_Controlled with
      record
         Internal : System.Address;
      end record;

   overriding
   procedure Finalize (Self : in out Handles);

   Null_Handle : constant Handles := (Ada.Finalization.Limited_Controlled with Internal => System.Null_Address);
end SDL.Libraries;
