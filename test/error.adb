with SDL;
with SDL.Error;
with SDL.Log;
with System;

use type System.Bit_Order;

procedure Error is
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   SDL.Error.Set ("No worries");

   SDL.Log.Put_Debug ("Error string : " & SDL.Error.Get);

   SDL.Finalise;
end Error;
