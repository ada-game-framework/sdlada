--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with SDL.Error;

package body SDL.TTFs.Makers is

   procedure Create (Font       : in out Fonts;
                     File_Name  : in String;
                     Point_Size : in Point_Sizes;
                     Font_Index : in Font_Faces := Font_Faces'First) is
      Ops : constant RWops.RWops := RWops.From_File (File_Name => File_Name,
                                                     Mode      => RWops.Read_Binary);
   begin
      if RWops.Is_Null (Ops) then
         raise TTF_Error with "Unable to open font """ & File_Name & '"';
      end if;

      Create (Font        => Font,
              Source      => Ops,
              Point_Size  => Point_Size,
              Font_Index  => Font_Index,
              Free_Source => True);
   end Create;

   procedure Create (Font        : in out Fonts;
                     Source      : in RWops.RWops;
                     Point_Size  : in Point_Sizes;
                     Font_Index  : in Font_Faces := Font_Faces'First;
                     Free_Source : in Boolean      := True) is
      function TTF_Open_Font_Index_RW (Ops   : in RWops.RWops;
                                       Free  : in C.int;
                                       Size  : in Point_Sizes;
                                       Index : in Font_Faces) return Fonts_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_OpenFontIndexRW";
   begin
      Font.Internal := TTF_Open_Font_Index_RW (Ops   => Source,
                                               Free  => (if Free_Source then 1 else 0),
                                               Size  => Point_Size,
                                               Index => Font_Index);

      if Font.Internal = null then
         raise TTF_Error with Error.Get;
      else
         Font.Source_Freed := Free_Source;
      end if;
   end Create;
end SDL.TTFs.Makers;
