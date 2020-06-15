--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2020, Luke A. Guest
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
--------------------------------------------------------------------------------------------------------------------
--  SDL.TTFs.Makers
--
--  Constructor subprograms which don't dispatch for font types.
--------------------------------------------------------------------------------------------------------------------

with SDL.RWops;

package SDL.TTFs.Makers is
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
