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
--  SDL.Events.Controllers
--
--  Game controller specific events.
package SDL.Events.Controllers is
   type Controller_Axes is (Invalid,
                            Left_X,
                            Left_Y,
                            Right_X,
                            Right_Y,
                            Left_Trigger,
                            Right_Trigger) with
     Convention => C;

   for Controller_Axes use (Invalid       => -1,
                            Left_X        => 0,
                            Left_Y        => 1,
                            Right_X       => 2,
                            Right_Y       => 3,
                            Left_Trigger  => 4,
                            Right_Trigger => 5);

   type Controller_Events is
      record
         Event_Type : Event_Types;           --  Will be set to Controller_Axis_Motion.
         Time_Stamp : Time_Stamps;

      end record with
     Convention => C;
   --private
end SDL.Events.Controllers;
