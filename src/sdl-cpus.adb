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
with Interfaces.C;

package body SDL.CPUS is
   package C renames Interfaces.C;

   use type C.int;

   function Count return Positive is
      function SDL_Get_CPU_Count return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetCPUCount";
   begin
      return Positive (SDL_Get_CPU_Count);
   end Count;

   function Cache_Line_Size return Positive is
      function SDL_Cache_Line_Size return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetCPUCacheLineSize";
   begin
      return Positive (SDL_Cache_Line_Size);
   end Cache_Line_Size;

   function Has_3DNow return Boolean is
      function SDL_Has_3DNow return C.Int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Has3DNow";
   begin
      return (if SDL_Has_3DNow = 1 then True else False);
   end Has_3DNow;

   function Has_AltiVec return Boolean is
      function SDL_Has_AltiVec return C.Int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasAltiVec";
   begin
      return (if SDL_Has_AltiVec = 1 then True else False);
   end Has_AltiVec;

   function Has_MMX return Boolean is
      function SDL_Has_MMX return C.Int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasMMX";
   begin
      return (if SDL_Has_MMX = 1 then True else False);
   end Has_MMX;

   function Has_RDTSC return Boolean is
      function SDL_Has_RDTSC return C.Int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasRDTSC";
   begin
      return (if SDL_Has_RDTSC = 1 then True else False);
   end Has_RDTSC;

   function Has_SSE return Boolean is
      function SDL_Has_SSE return C.Int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasSSE";
   begin
      return (if SDL_Has_SSE = 1 then True else False);
   end Has_SSE;

   function Has_SSE_2 return Boolean is
      function SDL_Has_SSE_2 return C.Int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasSSE2";
   begin
      return (if SDL_Has_SSE_2 = 1 then True else False);
   end Has_SSE_2;

   function Has_SSE_3 return Boolean is
      function SDL_Has_SSE_3 return C.Int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasSSE3";
   begin
      return (if SDL_Has_SSE_3 = 1 then True else False);
   end Has_SSE_3;

   function Has_SSE_4_1 return Boolean is
      function SDL_Has_SSE_4_1 return C.Int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasSSE41";
   begin
      return (if SDL_Has_SSE_4_1 = 1 then True else False);
   end Has_SSE_4_1;

   function Has_SSE_4_2 return Boolean is
      function SDL_Has_SSE_4_2 return C.Int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasSSE42";
   begin
      return (if SDL_Has_SSE_4_2 = 1 then True else False);
   end Has_SSE_4_2;
end SDL.CPUS;
