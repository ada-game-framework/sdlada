--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.TTFs.Makers
--
--  Constructor subprograms which don't dispatch for font types.
--------------------------------------------------------------------------------------------------------------------

with SDL.RWops;

package SDL.TTFs.Makers is
   pragma Preelaborate;

   procedure Create (Font       : in out Fonts;
                     File_Name  : in String;
                     Point_Size : in Point_Sizes;
                     Font_Index : in Font_Faces := Font_Faces'First);

   procedure Create (Font        : in out Fonts;
                     Source      : in RWops.RWops;
                     Point_Size  : in Point_Sizes;
                     Font_Index  : in Font_Faces := Font_Faces'First;
                     Free_Source : in Boolean      := True);
end SDL.TTFs.Makers;
