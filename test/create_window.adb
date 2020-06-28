with Ada.Strings.Unbounded;

with SDL;
with SDL.Timers;
with SDL.Video.Pixel_Formats;
with SDL.Video.Surfaces;
with SDL.Video.Windows;
with SDL.Video.Windows.Makers;

procedure Create_Window is

   package US renames Ada.Strings.Unbounded;
   function "+" (S : String) return US.Unbounded_String
      renames US.To_Unbounded_String;
   function "+" (S : US.Unbounded_String) return String
      renames US.To_String;

   type Testcase is record
      Label    : US.Unbounded_String;
      Position : SDL.Natural_Coordinates;
   end record;

   Tests : constant array (Positive range <>) of Testcase :=
     ((+"X: Undefined position (0)",
       (SDL.Video.Windows.Undefined_Window_Position (0), 100)),
      (+"Y: Undefined position (1)",
       (100, SDL.Video.Windows.Undefined_Window_Position (1))),
      (+"Y: Centered position (0)",
       (100, SDL.Video.Windows.Centered_Window_Position (0))),
      (+"X: Centered position (1)",
       (SDL.Video.Windows.Centered_Window_Position (1), 100)));

begin
   if not SDL.Initialise then
      raise Program_Error;
   end if;

   for T of Tests loop
      declare
         Width  : constant := 400;
         Height : constant := 300;
         W      : SDL.Video.Windows.Window;
         S      : SDL.Video.Surfaces.Surface;
      begin
         SDL.Video.Windows.Makers.Create
           (W, +T.Label, T.Position, (Width, Height));
         S := W.Get_Surface;
         for I in 1 .. 10 loop
            S.Fill (Area   => (0, 0, Width, Height),
                    Colour => SDL.Video.Pixel_Formats.To_Pixel
                                (S.Pixel_Format, 255, 255, 255));
            W.Update_Surface;
            SDL.Timers.Wait_Delay (100);
         end loop;
      end;
   end loop;

   SDL.Finalise;
end Create_Window;
