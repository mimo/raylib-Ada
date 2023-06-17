with raylib;
with Ada.Strings.Fixed;
with Interfaces.C;

procedure core_input_gamepad is
   use raylib;
   use raylib.core;
   use type raylib.int;

   screenWidth  : constant := 800;
   screenHeight : constant := 450;

   texPs3Pad, texXboxPad : Texture2D;

   type Gamepad_Type is (UNAVAILABLE, UNKNOWN, XBOX, PLAYSTATION, OTHER);
   --my_gamepad : Gamepad_Type := UNKNOWN;
   --my_gamepad : Gamepad_Type := XBOX;
   --my_gamepad : Gamepad_Type := PLAYSTATION;
   my_gamepad : Gamepad_Type := OTHER;
   GAMEPAD_PLAYER1 : int := 0;

   procedure detect_gamepad is
      use type Interfaces.C.C_bool;
   begin
      if raylib.core.is_gamepad_available (GAMEPAD_PLAYER1) = Interfaces.C.False then
         my_gamepad := UNAVAILABLE;
         return;
      end if;

      declare
         gamepad_name : string := raylib.core.get_gamepad_name (GAMEPAD_PLAYER1);
         package FS renames Ada.Strings.Fixed;
      begin
         if gamepad_name'length = 0 then
            my_gamepad := UNKNOWN;
            return;
         end if;

         if
            FS.Index (gamepad_name, "Xbox") /= 0
            or FS.Index (gamepad_name,"Microsoft") /= 0
            then my_gamepad := XBOX;
         elsif
            FS.Index (gamepad_name, "PLAYSTATION") /= 0
            or FS.Index (gamepad_name,"Sense") /= 0
            or FS.Index (gamepad_name,"Wireless") /= 0
            then my_gamepad := PLAYSTATION;
         else my_gamepad := OTHER;
         end if;

      end;
   end detect_gamepad;

   procedure Draw_Xbox_Gamepad is
   begin
      textures.draw_texture (texXboxPad, 0, 0, DARKGRAY);

      -- Draw axis: left joystick
      shapes.draw_circle (259, 152, 39.0, BLACK);
      shapes.draw_circle (259, 152, 34.0, LIGHTGRAY);
      shapes.draw_circle (
         259
         + int (get_gamepad_axis_movement(
               GAMEPAD_PLAYER1,
               GAMEPAD_AXIS_LEFT_X) * 20.0),
         152 + int (get_gamepad_axis_movement (GAMEPAD_PLAYER1, GAMEPAD_AXIS_LEFT_Y) * 20.0), 25.0, BLACK);

         --  Draw axis: right joystick
         raylib.shapes.draw_circle (461, 237, 38.0, BLACK);
         raylib.shapes.draw_circle (461, 237, 33.0, LIGHTGRAY);
         raylib.shapes.draw_circle (
            461 + int(raylib.core.get_gamepad_axis_movement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_RIGHT_X)*20.0),
            237 + int(raylib.core.get_gamepad_axis_movement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_RIGHT_Y)*20.0), 25.0, BLACK);

         -- Draw axis: left-right triggers
         shapes.draw_rectangle (170, 30, 15, 70, GRAY);
         shapes.draw_rectangle (604, 30, 15, 70, GRAY);
         shapes.draw_rectangle (170, 30, 15, int(((1.0 +
            get_gamepad_axis_movement(GAMEPAD_PLAYER1,
               GAMEPAD_AXIS_LEFT_TRIGGER))/2.0)*70.0), RED);
         shapes.draw_rectangle (604, 30, 15, int(((1.0 +
         get_gamepad_axis_movement(GAMEPAD_PLAYER1,
         GAMEPAD_AXIS_RIGHT_TRIGGER))/2.0)*70.0), RED);
   end Draw_xbox_Gamepad;

   procedure draw_playstation_gamepad is
   begin
      textures.draw_texture (texPs3Pad, 0, 0, DARKGRAY);
   end;

begin

   --SetConfigFlags(FLAG_MSAA_4X_HINT);  // Set MSAA 4X hint before windows creation

   window.init (screenWidth, screenHeight, "raylib [core] example - gamepad input");

   texPs3Pad  := textures.load ("resources/ps3.png");
   texXboxPad := textures.load ("resources/xbox.png");

   raylib.set_target_FPS (30);

   while not raylib.window.should_close loop
      begin_drawing;
      clear_background (raylib.RAYWHITE);

      raylib.text.draw ("My gamepad is " & (if my_gamepad = OTHER then raylib.core.get_gamepad_name (GAMEPAD_PLAYER1) else my_gamepad'Img), 30, 8, 15, DARKGREEN);

      case my_gamepad is
      when XBOX => Draw_Xbox_Gamepad;
      when UNKNOWN | UNAVAILABLE => detect_gamepad;
      when PLAYSTATION | OTHER => draw_playstation_gamepad;
      end case;

      end_drawing;

   end loop;

   textures.unload (texXboxPad);
   textures.unload (texPs3Pad);

   raylib.window.close;

end core_input_gamepad;
