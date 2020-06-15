with SDL;
with SDL.Error;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Events.Joysticks;
with SDL.Events.Windows;
with SDL.Inputs.Joysticks.Makers;
with SDL.Inputs.Joysticks.Game_Controllers;
with SDL.Events.Mice;
with SDL.Log;
with SDL.Video.Displays;
with SDL.Video.Windows;
with SDL.Video.Windows.Makers;
with SDL.Video.Windows.Manager;
with SDL.Versions;
with System;

procedure Test is
   W              : SDL.Video.Windows.Window;
   Total_Drivers  : constant Positive := SDL.Video.Total_Drivers;
   Linked_Version : SDL.Versions.Version;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   SDL.Versions.Linked_With (Info => Linked_Version);

   SDL.Log.Put_Debug ("System.Word_Size: " & Integer'Image (System.Word_Size));
   SDL.Log.Put_Debug ("Revision        : " & SDL.Versions.Revision);
   SDL.Log.Put_Debug ("Linked with     : " & SDL.Versions.Version_Level'Image (Linked_Version.Major) &
                        "." & SDL.Versions.Version_Level'Image (Linked_Version.Minor) &
                        "." & SDL.Versions.Version_Level'Image (Linked_Version.Patch));
   SDL.Log.Put_Debug ("Compiled with   : " & SDL.Versions.Version_Level'Image (SDL.Versions.Compiled_Major) &
                        "." & SDL.Versions.Version_Level'Image (SDL.Versions.Compiled_Minor) &
                        "." & SDL.Versions.Version_Level'Image (SDL.Versions.Compiled_Patch));
   SDL.Log.Put_Debug ("Bit Order       : " & System.Bit_Order'Image (SDL.Video.Windows.Window'Bit_Order));
   SDL.Log.Put_Debug ("Total drivers   : " & Positive'Image (Total_Drivers));

   for Index in Positive'First .. Total_Drivers loop
      SDL.Log.Put_Debug ("Driver (" & Positive'Image (Index) & ")     : " & SDL.Video.Driver_Name (Natural (Index)));
   end loop;

   if SDL.Initialise = True then
      SDL.Log.Put_Debug ("Current driver  : " & SDL.Video.Current_Driver_Name);
      SDL.Log.Put_Debug ("Total displays  : " & SDL.Video.Displays.Display_Indices'Image (SDL.Video.Displays.Total));

      SDL.Error.Clear;

      SDL.Log.Put_Debug ("Error           : " & SDL.Error.Get);

      SDL.Video.Windows.Makers.Create (Win      => W,
                                       Title    => "Test SDLAda 2.0 - हिन्दी समाचार",
                                       Position => SDL.Natural_Coordinates'(X => 100, Y => 100),
                                       Size     => SDL.Positive_Sizes'(800, 640));

      SDL.Log.Put_Debug ("Window Grabbed  : " & Boolean'Image (W.Is_Grabbed));

      --  W.Set_Grabbed;

      SDL.Log.Put_Debug ("Window Grabbed  : " & Boolean'Image (W.Is_Grabbed));
      SDL.Log.Put_Debug ("Window ID       : " & SDL.Video.Windows.ID'Image (W.Get_ID));
      SDL.Log.Put_Debug ("Window Title    : " & W.Get_Title);

      SDL.Log.Put_Debug ("Window on display : " & SDL.Video.Displays.Display_Indices'Image (W.Display_Index));

      --  W.Set_Mode (SDL.Video.Windows.Full_Screen);

      declare
         ID : constant SDL.Video.Windows.ID     := SDL.Video.Windows.Get_ID (W);
         W2 : constant SDL.Video.Windows.Window := SDL.Video.Windows.From_ID (ID);
      begin
         SDL.Video.Windows.Set_Title (W2, "Grabbed second window!");
      end;

      --  Joysticks.
      declare
         Total_Sticks : constant SDL.Inputs.Joysticks.All_Devices := SDL.Inputs.Joysticks.Total;
         Stick        : SDL.Inputs.Joysticks.Joystick;
         GUID_1       : SDL.Inputs.Joysticks.GUIDs;
         GUID_2       : SDL.Inputs.Joysticks.GUIDs;

         use type SDL.Inputs.Joysticks.Devices;
         use type SDL.Inputs.Joysticks.Joystick;
         use type SDL.Inputs.Joysticks.GUIDs;
      begin
         if Total_Sticks = 0 then
            SDL.Log.Put_Debug ("No Joysticks    : ");
         else
            SDL.Log.Put_Debug ("Joystick polling: " & Boolean'Image (SDL.Events.Joysticks.Is_Polling_Enabled));
            SDL.Log.Put_Debug ("Total Joysticks : " & SDL.Inputs.Joysticks.Devices'Image (Total_Sticks));

            for Joystick in SDL.Inputs.Joysticks.Devices'First .. Total_Sticks loop
               GUID_1 := SDL.Inputs.Joysticks.GUID (Joystick);
               GUID_2 := SDL.Inputs.Joysticks.Value (SDL.Inputs.Joysticks.Image (GUID_1));

               if GUID_1 = GUID_2 then
                  SDL.Log.Put_Debug ("GUID Image<->Value works");
               end if;

               SDL.Log.Put_Debug ("Joystick              : (" & SDL.Inputs.Joysticks.Devices'Image (Joystick) & ") - " &
                                    SDL.Inputs.Joysticks.Name (Joystick) & " - " &
                                    Integer'Image (SDL.Inputs.Joysticks.GUIDs'Size) & " - GUID: " & ' ' &
                                    SDL.Inputs.Joysticks.Image (GUID_1));

               SDL.Inputs.Joysticks.Makers.Create (Device => Joystick, Actual_Stick => Stick);

               if Stick /= SDL.Inputs.Joysticks.Null_Joystick then
                  SDL.Log.Put_Debug ("  Name               : " & Stick.Name);
                  SDL.Log.Put_Debug ("  Axes               : " & SDL.Events.Joysticks.Axes'Image (Stick.Axes));
                  SDL.Log.Put_Debug ("  Balls              : " & SDL.Events.Joysticks.Balls'Image (Stick.Balls));
                  SDL.Log.Put_Debug ("  Buttons            : " & SDL.Events.Joysticks.Buttons'Image (Stick.Buttons));
                  SDL.Log.Put_Debug ("  Hats               : " & SDL.Events.Joysticks.Hats'Image (Stick.Hats));
                  SDL.Log.Put_Debug ("  Haptic             : " & Boolean'Image (Stick.Is_Haptic));
                  SDL.Log.Put_Debug ("  Attached           : " & Boolean'Image (Stick.Is_Attached));
                  SDL.Log.Put_Debug ("  GUID               : " & SDL.Inputs.Joysticks.Image (Stick.GUID));
                  SDL.Log.Put_Debug ("  Instance           : " & SDL.Inputs.Joysticks.Instances'Image (Stick.Instance));

                  Stick.Close;
               end if;

               if SDL.Inputs.Joysticks.Game_Controllers.Is_Game_Controller (Joystick) = True then
                  SDL.Log.Put_Debug ("  Is game controller : Yes");
               else
                  SDL.Log.Put_Debug ("  Is game controller : No");
               end if;
            end loop;
         end if;
      end;

      --  Window manager.
      declare
         Info : SDL.Video.Windows.Manager.WM_Info;
      begin
         Info.Version := SDL.Versions.Compiled;

         if SDL.Video.Windows.Manager.Get_WM_Info (W, Info) = True then
            SDL.Log.Put_Debug ("Window manager in use: " & SDL.Video.Windows.Manager.WM_Types'Image (Info.Sub_System));
         else
            SDL.Log.Put_Debug ("Cannot get window manager info.");
            SDL.Log.Put_Debug ("Error                : " & SDL.Error.Get);
         end if;
      end;

      --  Main loop.
      declare
         Event    : SDL.Events.Events.Events;
         Finished : Boolean := False;

         use type SDL.Events.Keyboards.Key_Codes;
         use type SDL.Events.Windows.Window_Event_ID;
      begin
         loop
            while SDL.Events.Events.Poll (Event) loop
               case Event.Common.Event_Type is
                  when SDL.Events.Quit =>
                     Finished := True;

                  when SDL.Events.Keyboards.Key_Up =>
                     SDL.Log.Put_Debug ("Key up event    : " &
                                          SDL.Events.Keyboards.Key_Codes'Image (Event.Keyboard.Key_Sym.Key_Code) &
                                          "    Scan code: " &
                                          SDL.Events.Keyboards.Scan_Codes'Image (Event.Keyboard.Key_Sym.Scan_Code));

                     if Event.Keyboard.Key_Sym.Key_Code = SDL.Events.Keyboards.Code_Escape then
                        Finished := True;
                     end if;

                  when SDL.Events.Joysticks.Axis_Motion =>
                     SDL.Log.Put_Debug
                       ("Joystick axis event (ID = " & SDL.Events.Joysticks.IDs'Image (Event.Joystick_Axis.Which) &
                          "): Axis: " & SDL.Events.Joysticks.Axes'Image (Event.Joystick_Axis.Axis) &
                          "    Value: " & SDL.Events.Joysticks.Axes_Values'Image (Event.Joystick_Axis.Value));

                  when SDL.Events.Mice.Motion =>
                     SDL.Log.Put_Debug
                       ("Mouse motion event (ID = " & SDL.Events.Mice.IDs'Image (Event.Mouse_Motion.Which) &
                          "): (X => " & SDL.Coordinate'Image (Event.Mouse_Motion.X) &
                          "): (Y => " & SDL.Coordinate'Image (Event.Mouse_Motion.Y) &
                          "): (X Rel => " & SDL.Events.Mice.Movement_Values'Image (Event.Mouse_Motion.X_Relative) &
                          ", Y Rel => " & SDL.Events.Mice.Movement_Values'Image (Event.Mouse_Motion.Y_Relative) &
                          ")");

                  when SDL.Events.Mice.Button_Up | SDL.Events.Mice.Button_Down =>
                     SDL.Log.Put_Debug
                       ("Mouse button event (ID = " & SDL.Events.Mice.IDs'Image (Event.Mouse_Motion.Which) &
                          "): Button = " & SDL.Events.Mice.Buttons'Image (Event.Mouse_Button.Button) &
                          "    State: " & SDL.Events.Button_State'Image (Event.Mouse_Button.State) &
                          "    Clicks: " & SDL.Events.Mice.Button_Clicks'Image (Event.Mouse_Button.Clicks) &
                          "    (X => " & SDL.Coordinate'Image (Event.Mouse_Button.X) &
                          ", Y => " & SDL.Coordinate'Image (Event.Mouse_Button.Y) &
                          ")");

                  when SDL.Events.Mice.Wheel =>
                     SDL.Log.Put_Debug
                       ("Mouse wheel event (ID = " & SDL.Events.Mice.IDs'Image (Event.Mouse_Wheel.Which) &
                          "): (X => " & SDL.Events.Mice.Wheel_Values'Image (Event.Mouse_Wheel.X) &
                          "): (Y => " & SDL.Events.Mice.Wheel_Values'Image (Event.Mouse_Wheel.Y) &
                          "): (Direction => " & SDL.Events.Mice.Wheel_Directions'Image (Event.Mouse_Wheel.Direction) &
                          ")");

                  when SDL.Events.Windows.Window =>
                     if Event.Window.Event_ID = SDL.Events.Windows.Moved then
                        SDL.Log.Put_Debug ("Window on display : " &
                          SDL.Video.Displays.Display_Indices'Image (W.Display_Index));
                     end if;

                  when others =>
                     null;
               end case;
            end loop;

            exit when Finished;
         end loop;
      end;

      W.Finalize;
      SDL.Finalise;
   end if;
end Test;
