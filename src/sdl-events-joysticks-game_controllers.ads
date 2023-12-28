--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Events.Joysticks.Game_Controllers
--
--  Game controller specific events.
--------------------------------------------------------------------------------------------------------------------

package SDL.Events.Joysticks.Game_Controllers is
   pragma Pure;

   type Axes is (Invalid,
                 Left_X,
                 Left_Y,
                 Right_X,
                 Right_Y,
                 Trigger_Left,
                 Trigger_Right) with
     Convention => C;

   for Axes use (Invalid       => -1,
                 Left_X        =>  0,
                 Left_Y        =>  1,
                 Right_X       =>  2,
                 Right_Y       =>  3,
                 Trigger_Left  =>  4,
                 Trigger_Right =>  5);

   subtype LR_Axes      is Axes range Left_X .. Right_Y;
   subtype Trigger_Axes is Axes range Trigger_Left .. Trigger_Right;

   type LR_Axes_Values is range -32768 .. 32767 with
     Convention => C,
     Size       => 16;

   type Trigger_Axes_Values is range 0 .. 32767 with
     Convention => C,
     Size       => 16;

   type Buttons is (Invalid,
                    A,
                    B,
                    X,
                    Y,
                    Back,
                    Guide,
                    Start,
                    Left_Stick,
                    Right_Stick,
                    Left_Shoulder,
                    Right_Shoulder,
                    D_Pad_Up,
                    D_Pad_Down,
                    D_Pad_Left,
                    D_Pad_Right) with
     Convention => C;

   for Buttons use (Invalid        => -1,
                    A              =>  0,
                    B              =>  1,
                    X              =>  2,
                    Y              =>  3,
                    Back           =>  4,
                    Guide          =>  5,
                    Start          =>  6,
                    Left_Stick     =>  7,
                    Right_Stick    =>  8,
                    Left_Shoulder  =>  9,
                    Right_Shoulder => 10,
                    D_Pad_Up       => 11,
                    D_Pad_Down     => 12,
                    D_Pad_Left     => 13,
                    D_Pad_Right    => 14);

   --  Update the game controller event data. This is called by the event loop.
   procedure Update with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GameControllerUpdate";

   function Is_Polling_Enabled return Boolean with
     Inline => True;

   procedure Enable_Polling with
     Inline => True;

   procedure Disable_Polling with
     Inline => True;
end SDL.Events.Joysticks.Game_Controllers;
