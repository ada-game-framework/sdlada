--                              -*- Mode: Ada -*-
--  Filename        : sdl-libraries.ads
--  Description     : Mechanism for accessing shared objects (libraries).
--  Author          : Luke A. Guest
--  Created On      : Sat Oct 12 21:50:00 2013
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

      From_Library : Handles;
      Name         : String;
   function Load_Sub_Program return Access_To_Sub_Program;
private
   type Handles is new Ada.Finalization.Limited_Controlled with
      record
         Internal : System.Address;
      end record;

   overriding
   procedure Finalize (Self : in out Handles);

   Null_Handle : constant Handles := (Ada.Finalization.Limited_Controlled with Internal => System.Null_Address);
end SDL.Libraries;
