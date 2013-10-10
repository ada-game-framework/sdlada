package body SDL is
   package C renames Interfaces.C;

   use type C.int;

   function Initialise (Flags : in Init_Flags := Everything) return Boolean is
      function SDL_Init (Flags : in Init_Flags := Everything) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Init";

      Result : C.int := SDL_Init (Flags);
   begin
      return (Result = Success);
   end Initialise;

   function Initialise_Sub_System (Flags : in Init_Flags) return Boolean is
      function SDL_Init_Sub_System (Flags : in Init_Flags) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_InitSubSystem";

      Result : C.int := SDL_Init_Sub_System (Flags);
   begin
      return (Result = Success);
   end Initialise_Sub_System;
end SDL;
