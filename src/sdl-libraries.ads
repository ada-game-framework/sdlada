--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Libraries
--
--  Mechanism for accessing shared objects (libraries).
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;

package SDL.Libraries is
   pragma Preelaborate;

   Library_Error : exception;

   type Handles is new Ada.Finalization.Limited_Controlled with private;

   Null_Handle : constant Handles;

   procedure Load (Self : out Handles; Name : in String);
   procedure Unload (Self : in out Handles); -- with
   --     Static_Predicate => Handle /= Null_Handle;

   generic
      type Access_To_Sub_Program is private;

      Name : String;
   function Load_Sub_Program (From_Library : in Handles) return Access_To_Sub_Program;
private
   type Internal_Handle is null record with
     Convention => C;

   type Internal_Handle_Access is access all Internal_Handle with
     Convention => C;

   type Handles is new Ada.Finalization.Limited_Controlled with
      record
         Internal : Internal_Handle_Access;
      end record;

   overriding
   procedure Finalize (Self : in out Handles);

   Null_Handle : constant Handles := (Ada.Finalization.Limited_Controlled with Internal => null);
end SDL.Libraries;
