--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.CPUS
--
--  Platform CPU information retrieval.
--------------------------------------------------------------------------------------------------------------------
package SDL.CPUS is
   pragma Pure;

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
