with Ada.Text_IO.Text_Streams;
with SDL.Log;

package body Moose is
   procedure Load_Moose_Data (Data : out Moose_Frame_Data_Array) is
      Actual_Name : constant String := "../../test/moose.dat";
      Data_File   : Ada.Text_IO.File_Type;
      Stream      : Ada.Text_IO.Text_Streams.Stream_Access := null;

      use type Ada.Text_IO.File_Mode;
   begin
      Ada.Text_IO.Open (File => Data_File, Mode => Ada.Text_IO.In_File, Name => Actual_Name);

      Stream := Ada.Text_IO.Text_Streams.Stream (File => Data_File);

      Moose_Frame_Data_Array'Read (Stream, Data);

      Ada.Text_IO.Close (File => Data_File);
   exception
      when others =>
         SDL.Log.Put_Error ("Error, reading source file, " & Actual_Name);

         raise;
   end Load_Moose_Data;


   procedure Cache_Moose (Cache   : in out Cached_Moose_Frame_Array;
                          Indices : in Moose_Frame_Data_Array;
                          Palette : in Moose_Palette_Array) is

      Colour : SDL.Video.Palettes.RGB_Colour;
   begin
      for Frame in Moose_Frames loop
         for Y in 1 .. Moose_Size.Height loop
            for X in 1 .. Moose_Size.Width loop
               Colour := Palette (Indices (Frame, ((Y - 1) * Moose_Size.Width) + (X - 1)));

               Cache (Frame) (Y, X) := SDL.Video.Pixels.ARGB_8888'(Red   => Colour.Red,
                                                                   Green => Colour.Green,
                                                                   Blue  => Colour.Blue,
                                                                   Alpha => SDL.Video.Palettes.Colour_Component'Last);
            end loop;
         end loop;
      end loop;
   end Cache_Moose;
end Moose;
