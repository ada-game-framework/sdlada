package body SDL.Timers is
   procedure Wait_Delay (MS : Milliseconds_Long) is
   begin
      Wait_Delay (Milliseconds (MS));
   end Wait_Delay;
end SDL.Timers;
