with SDL;
with SDL.Error;
with SDL.Log;
with System;

use type System.Bit_Order;

procedure Libraries is
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);


   SDL.Finalise;
end Libraries;
