--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014-2015 Luke A. Guest
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
--  SDL.Video.Surfaces.Internal_Makers
--
--  Functions to create surface objects.
--------------------------------------------------------------------------------------------------------------------
private with SDL.C_Pointers;

private package SDL.Video.Surfaces.Internal_Makers is
private
   --  Create a new Surface passing in the internal C pointer and whether this particular object owns it's pointer.
   function Make (S : in SDL.C_Pointers.Surface_Pointer; Owns : in Boolean) return Surface with
     Convention    => Ada,
     Export        => True,
     External_Name => "Make_Surface_From_Pointer";
end SDL.Video.Surfaces.Internal_Makers;
