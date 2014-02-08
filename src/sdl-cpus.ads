--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014 Luke A. Guest
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
--  SDL.CPUS
--
--  Platform CPU information retrieval.
--------------------------------------------------------------------------------------------------------------------
package SDL.CPUS is
   --  TODO: Add a subprogram to return CPU architecture as not all platforms will have the same one, i.e.
   --  Android on ARM, MIPS, x86.

   function Count return Positive;

   function Cache_Line_Size return Positive with
     Inline => True;

   function Has_3DNow return Boolean with
     Inline => True;

   function Has_AltiVec return Boolean with
     Inline => True;

   function Has_MMX return Boolean with
     Inline => True;

   function Has_RDTSC return Boolean with
     Inline => True;

   function Has_SSE return Boolean with
     Inline => True;

   function Has_SSE_2 return Boolean with
     Inline => True;

   function Has_SSE_3 return Boolean with
     Inline => True;

   function Has_SSE_4_1 return Boolean with
     Inline => True;

   function Has_SSE_4_2 return Boolean with
     Inline => True;
end SDL.CPUS;
