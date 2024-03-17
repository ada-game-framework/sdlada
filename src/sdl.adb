--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
package body SDL is
   function Initialise (Flags : in Init_Flags := Enable_Everything) return Boolean is
      function SDL_Init (Flags : in Init_Flags := Enable_Everything) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Init";

      Result : constant C.int := SDL_Init (Flags);
   begin
      return (Result = Success);
   end Initialise;


   function Initialise_Sub_System (Flags : in Init_Flags) return Boolean is
      function SDL_Init_Sub_System (Flags : in Init_Flags) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_InitSubSystem";

      Result : constant C.int := SDL_Init_Sub_System (Flags);
   begin
      return (Result = Success);
   end Initialise_Sub_System;


   function SDL_Was_Initialised (Flags : in Init_Flags := Null_Init_Flags) return Init_Flags with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_WasInit";


   function What_Was_Initialised return Init_Flags is
   begin
      return SDL_Was_Initialised;
   end What_Was_Initialised;


   function Was_Initialised (Flags : in Init_Flags) return Boolean is
   begin
      return (SDL_Was_Initialised (Flags) = Flags);
   end Was_Initialised;


   function To_Bool (Value : in Boolean) return SDL_Bool is
   begin
      return (if Value then SDL_True else SDL_False);
   end To_Bool;
end SDL;
