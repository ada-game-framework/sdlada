with Ada.Directories;
with Ada.Text_IO;
with SDL.RWops.Streams;

procedure Rwops is
   package Text_IO renames Ada.Text_IO;
   package Directories renames Ada.Directories;

   procedure RWops_Tests is
   begin
      declare
         use type SDL.RWops.Offsets;

         Op     : constant SDL.RWops.RWops := SDL.RWops.From_File ("test", SDL.RWops.Read_Binary);
         Offset : SDL.RWops.Offsets        := SDL.RWops.Error_Offset;
      begin
         Text_IO.Put_Line ("Testing RW_Seek, RW_Tell and RW_Size:");

         --  Moving the file offset in steps of size 10.
         for I in 1 .. 10 loop
            Offset := SDL.RWops.Seek (Context => Op,
                                      Offset  => 10,
                                      Whence  => SDL.RWops.RW_Seek_Cur);

            Text_IO.Put ("Current offset (should be" & Integer'Image (10 * I) & "):");
            Text_IO.Put (SDL.RWops.Offsets'Image (Offset) & " (returned value) ");

            Offset := SDL.RWops.Tell (Context => Op);

            Text_IO.Put (SDL.RWops.Offsets'Image (Offset) & " (from RW_Tell)");
            Text_IO.New_Line;
         end loop;

         Text_IO.New_Line;

         Offset := SDL.RWops.Seek
           (Context => Op,
            Offset  => -10,
            Whence  => SDL.RWops.RW_Seek_End);

         Text_IO.Put_Line ("offset 10 before end of file:" & SDL.RWops.Offsets'Image (Offset));

         Offset := SDL.RWops.Size (Op);

         Text_IO.Put_Line ("offset Rw_Size (should be previous value + 10):" & SDL.RWops.Offsets'Image (Offset));

         SDL.RWops.Close (Op);
      end;

      declare
         Op : constant SDL.RWops.RWops := SDL.RWops.From_File ("test.bin", SDL.RWops.Create_To_Write);
      begin
         Text_IO.New_Line;
         Text_IO.Put_Line ("Writing some numbers as Uint8 into a file...");

         for I in 1 .. 16 loop
            SDL.RWops.Write_U_8 (Op, SDL.RWops.Uint8 (I));
         end loop;

         SDL.RWops.Close (Op);
      end;

      declare
         Op       : SDL.RWops.RWops  := SDL.RWops.From_File ("test.bin", SDL.RWops.Read_Binary);
         Result8  : SDL.RWops.Uint8  := 0;
         Result32 : SDL.RWops.Uint32 := 0;
      begin
         Text_IO.Put_Line ("Reading Uint8 from file...");

         for I in 1 .. 16 loop
            Result8 := SDL.RWops.Read_U_8 (Op);

            Text_IO.Put_Line ("read Uint8:" & SDL.RWops.Uint8'Image (Result8));
         end loop;

         SDL.RWops.Close (Op);

         Text_IO.New_Line;
         Text_IO.Put_Line ("Reading Uint32 from file...");

         SDL.RWops.From_File ("test.bin", SDL.RWops.Read_Binary, Op);

         for I in 1 .. 4 loop
            Result32 := SDL.RWops.Read_LE_32 (Op);

            Text_IO.Put_Line ("read Uint32:" & SDL.RWops.Uint32'Image (Result32));
         end loop;

         SDL.RWops.Close (Op);
      end;

      Text_IO.New_Line;
      Text_IO.Put_Line ("Base path: " & SDL.RWops.Base_Path);

      declare
         Tmp_Path  : constant String := Directories.Current_Directory;
         Pref_Path : constant String := SDL.RWops.Preferences_Path ("org", "app");
      begin
         Text_IO.Put_Line ("Pref path: " & Pref_Path);

         if Directories.Exists (Pref_Path) then
            --  Remove app/org if it was created.
            Directories.Set_Directory (Pref_Path);
            Directories.Set_Directory ("..");
            Directories.Delete_Directory ("app");
            Directories.Set_Directory ("..");
            Directories.Delete_Directory ("org");
            Directories.Set_Directory (Tmp_Path);
         end if;
      end;
   end RWops_Tests;

   procedure RWops_Streams_Test is
      --  Dummy type to write into and read from stream.
      type Complicated_Union (B : Boolean := True) is
         record
            I : Integer         := 0;
            S : String (1 .. 5) := "hello";

            case B is
               when True =>
                  T : String (1 .. 4) := "true";
                  F : Long_Float      := 3.14159265;
               when False =>
                  C : Character       := 'X';
            end case;
         end record;
   begin
      Text_IO.Put_Line ("Testing RWops.Streams:");
      Text_IO.Put_Line ("Creating file-stream ""out.bin""...");

      declare
         W_Stream : aliased SDL.RWops.Streams.RWops_Stream := SDL.RWops.Streams.Open
           (SDL.RWops.From_File
              ("out.bin",
               SDL.RWops.Create_To_Write_Binary));

         I        : constant Integer := 42;
         S        : constant String  := "Hello, world!!!";
         T_True   : Complicated_Union;
         T_False  : Complicated_Union (False);
      begin
         Text_IO.Put_Line ("Writing:" & I'Img);

         Integer'Write (W_Stream'Access, I);

         Text_IO.Put_Line ("Writing: " & S);

         String'Write (W_Stream'Access, S);

         Text_IO.Put_Line ("Writing two objects of a more complex type...");

         Complicated_Union'Output (W_Stream'Access, T_True);
         Complicated_Union'Output (W_Stream'Access, T_False);

         Text_IO.Put_Line ("Closing stream.");

         W_Stream.Close;
      end;

      declare
         R_Stream : aliased SDL.RWops.Streams.RWops_Stream := SDL.RWops.Streams.Open
           (SDL.RWops.From_File
              ("out.bin",
               SDL.RWops.Read_Binary));

         I        : Integer := 0;
         S        : String  := "xxxxxxxxxxxxxxx";
         T_True   : Complicated_Union;
         T_False  : Complicated_Union;
      begin
         Text_IO.Put_Line ("Now reading from file-stream...");

         Integer'Read (R_Stream'Access, I);
         String'Read (R_Stream'Access, S);

         T_True := Complicated_Union'Input (R_Stream'Access);
         T_False := Complicated_Union'Input (R_Stream'Access);

         R_Stream.Close;

         Text_IO.Put_Line (I'Img);
         Text_IO.Put_Line (S);
         Text_IO.Put_Line (T_True.F'Img);
         Text_IO.Put_Line (T_False.C'Img);
      end;

      declare
         Op      : SDL.RWops.RWops;
         Stream  : aliased SDL.RWops.Streams.RWops_Stream;
         I       : Integer := 0;
         S       : String  := "xxxxxxxxxxxxxxx";
         T_True  : Complicated_Union;
         T_False : Complicated_Union;
      begin
         SDL.RWops.From_File (File_Name => "out.bin",
                              Mode      => SDL.RWops.Read_Binary,
                              Ops       => Op);

         SDL.RWops.Streams.Open
           (Op     => Op,
            Stream => Stream);

         Integer'Read (Stream'Access, I);
         String'Read (Stream'Access, S);

         T_True := Complicated_Union'Input (Stream'Access);
         T_False := Complicated_Union'Input (Stream'Access);

         Stream.Close;

         Text_IO.Put_Line (I'Img);
         Text_IO.Put_Line (S);
         Text_IO.Put_Line (T_True.F'Img);
         Text_IO.Put_Line (T_False.C'Img);
      end;
   end RWops_Streams_Test;
begin
   RWops_Tests;
   RWops_Streams_Test;
exception
   when SDL.RWops.RWops_Error =>
      Text_IO.Put_Line ("Make sure you have a file named 'test' in this directory, it can be any file.");
end Rwops;
