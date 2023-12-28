--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

package body SDL.CPUS is
   package C renames Interfaces.C;

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
      function SDL_Has_3DNow return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Has3DNow";
   begin
      return (if SDL_Has_3DNow = 1 then True else False);
   end Has_3DNow;

   function Has_AltiVec return Boolean is
      function SDL_Has_AltiVec return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasAltiVec";
   begin
      return (if SDL_Has_AltiVec = 1 then True else False);
   end Has_AltiVec;

   function Has_MMX return Boolean is
      function SDL_Has_MMX return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasMMX";
   begin
      return (if SDL_Has_MMX = 1 then True else False);
   end Has_MMX;

   function Has_RDTSC return Boolean is
      function SDL_Has_RDTSC return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasRDTSC";
   begin
      return (if SDL_Has_RDTSC = 1 then True else False);
   end Has_RDTSC;

   function Has_SSE return Boolean is
      function SDL_Has_SSE return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasSSE";
   begin
      return (if SDL_Has_SSE = 1 then True else False);
   end Has_SSE;

   function Has_SSE_2 return Boolean is
      function SDL_Has_SSE_2 return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasSSE2";
   begin
      return (if SDL_Has_SSE_2 = 1 then True else False);
   end Has_SSE_2;

   function Has_SSE_3 return Boolean is
      function SDL_Has_SSE_3 return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasSSE3";
   begin
      return (if SDL_Has_SSE_3 = 1 then True else False);
   end Has_SSE_3;

   function Has_SSE_4_1 return Boolean is
      function SDL_Has_SSE_4_1 return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasSSE41";
   begin
      return (if SDL_Has_SSE_4_1 = 1 then True else False);
   end Has_SSE_4_1;

   function Has_SSE_4_2 return Boolean is
      function SDL_Has_SSE_4_2 return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasSSE42";
   begin
      return (if SDL_Has_SSE_4_2 = 1 then True else False);
   end Has_SSE_4_2;
end SDL.CPUS;
