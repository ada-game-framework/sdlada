with SDL;
with SDL.Libraries;
with SDL.Log;

--  Run with: LD_LIBRARY_PATH=./gen/debug/test:$LD_LIBRARY_PATH ./gen/debug/test/libraries

procedure Libraries is
   Lib : SDL.Libraries.Handles;
begin
   SDL.Libraries.Load (Lib, "libtestmaths.so");

   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   declare
      type Access_To_Add is access function (A, B : in Integer) return Integer;
      type Access_To_Sub is access function (A, B : in Integer) return Integer with
        Convention => C;

      function Load is new SDL.Libraries.Load_Sub_Program
        (Access_To_Sub_Program => Access_To_Add,
         Name                  => "Add");

      function Load is new SDL.Libraries.Load_Sub_Program
        (Access_To_Sub_Program => Access_To_Sub,
         Name                  => "sub");

      Add : constant Access_To_Add := Load (From_Library => Lib);
      Sub : constant Access_To_Sub := Load (From_Library => Lib);
   begin
      SDL.Log.Put_Debug ("1 + 2 = " & Integer'Image (Add (1, 2)));
      SDL.Log.Put_Debug ("5 - 1 = " & Integer'Image (Sub (5, 1)));
   end;

   SDL.Finalise;
end Libraries;
