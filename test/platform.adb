with SDL;
with SDL.CPUS;
with SDL.Error;
with SDL.Log;
with SDL.Platform;
with System;

use type System.Bit_Order;

procedure Platform is
   Endian : System.Bit_Order renames System.Default_Bit_Order;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   SDL.Log.Put_Debug ("This system is running : " & SDL.Platform.Platforms'Image (SDL.Platform.Get));

   --  Endian-ness.
   SDL.Log.Put_Debug ("Bit Order              : " & System.Bit_Order'Image (Endian));
   SDL.Log.Put_Debug ("...in other words      : " & (if Endian = System.High_Order_First then "Big-endian" else "Little-endian"));

   --  CPU Info.
   SDL.Log.Put_Debug ("CPU count              : "  & Positive'Image (SDL.CPUS.Count));
   SDL.Log.Put_Debug ("CPU cache line size    : "  & Positive'Image (SDL.CPUS.Cache_Line_Size));
   SDL.Log.Put_Debug ("RDTSC                  : "  & Boolean'Image (SDL.CPUS.Has_RDTSC));
   SDL.Log.Put_Debug ("AltiVec                : "  & Boolean'Image (SDL.CPUS.Has_AltiVec));
   SDL.Log.Put_Debug ("3DNow!                 : "  & Boolean'Image (SDL.CPUS.Has_3DNow));
   SDL.Log.Put_Debug ("SSE                    : "  & Boolean'Image (SDL.CPUS.Has_SSE));
   SDL.Log.Put_Debug ("SSE2                   : "  & Boolean'Image (SDL.CPUS.Has_SSE_2));
   SDL.Log.Put_Debug ("SSE3                   : "  & Boolean'Image (SDL.CPUS.Has_SSE_3));
   SDL.Log.Put_Debug ("SSE4.1                 : "  & Boolean'Image (SDL.CPUS.Has_SSE_4_1));
   SDL.Log.Put_Debug ("SSE4.2                 : "  & Boolean'Image (SDL.CPUS.Has_SSE_4_2));

   SDL.Finalise;
end Platform;
