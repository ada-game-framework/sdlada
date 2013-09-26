with Interfaces.C;

package body SDL.CPUS is
   package C renames Interfaces.C;

   use type C.int;

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
        External_Name => "SDL_Has";
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
