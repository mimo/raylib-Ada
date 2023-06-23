with raylib;
with Ada.Strings.Fixed;
with Interfaces.C;

procedure core_input_gamepad is
   use raylib;
   use raylib.core;
   use type raylib.int;

   package IC renames Interfaces.C;
   use type IC.C_bool;

   screenWidth  : constant := 800;
   screenHeight : constant := 450;

   texPs3Pad, texXboxPad : Texture2D;

   type Gamepad_Type is (UNAVAILABLE, UNKNOWN, XBOX, PLAYSTATION, OTHER);
   my_gamepad : Gamepad_Type := UNKNOWN;
   gamepad_number : int := 0;

   procedure detect_gamepad is
   begin
      if raylib.core.is_gamepad_available (gamepad_number) = Interfaces.C.False then
         my_gamepad := UNAVAILABLE;
         return;
      end if;

      declare
         gamepad_name : string := raylib.core.get_gamepad_name (gamepad_number);
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

   function check_down (button_id : Gamepad_Button) return Boolean is
   begin
      if is_gamepad_button_down (gamepad_number, button_id) = IC.True
      then return True; else return False; end if;
   end check_down;

   procedure Draw_Xbox_Gamepad is
      use raylib.shapes;

      left_thumb_color, right_thumb_color : Color;
      left_trigger_pos, right_trigger_pos: int;
   begin
      textures.draw_texture (texXboxPad, 0, 0, DARKGRAY);

      -- Draw buttons: xbox home
      if check_down (GAMEPAD_BUTTON_MIDDLE) then draw_circle (394, 89, 19.0, RED); end if;

      -- Draw buttons: basic
      if check_down (GAMEPAD_BUTTON_MIDDLE_RIGHT)     then draw_circle (436, 150, 9.0, RED); end if;
      if check_down (GAMEPAD_BUTTON_MIDDLE_LEFT)      then draw_circle (352, 150, 9.0, RED); end if;
      if check_down (GAMEPAD_BUTTON_RIGHT_FACE_LEFT)  then draw_circle (501, 151, 15.0, BLUE); end if;
      if check_down (GAMEPAD_BUTTON_RIGHT_FACE_DOWN)  then draw_circle (536, 187, 15.0, LIME); end if;
      if check_down (GAMEPAD_BUTTON_RIGHT_FACE_RIGHT) then draw_circle (572, 151, 15.0, MAROON); end if;
      if check_down (GAMEPAD_BUTTON_RIGHT_FACE_UP)    then draw_circle (536, 115, 15.0, GOLD); end if;

      -- Draw buttons: d-pad
      shapes.draw_rectangle (317, 202, 19, 71, BLACK);
      shapes.draw_rectangle (293, 228, 69, 19, BLACK);
      if check_down (GAMEPAD_BUTTON_LEFT_FACE_UP)    then draw_rectangle (317, 202, 19, 26, RED); end if;
      if check_down (GAMEPAD_BUTTON_LEFT_FACE_DOWN)  then draw_rectangle (317, 202+45, 19, 26, RED); end if;
      if check_down (GAMEPAD_BUTTON_LEFT_FACE_LEFT)  then draw_rectangle (292, 228, 25, 19, RED); end if;
      if check_down (GAMEPAD_BUTTON_LEFT_FACE_RIGHT) then draw_rectangle (292+44, 228, 26, 19, RED); end if;

      -- Draw buttons: left-right back buttons
      if check_down (GAMEPAD_BUTTON_LEFT_TRIGGER_1)  then shapes.draw_circle (259, 61, 20.0, RED); end if;
      if check_down (GAMEPAD_BUTTON_RIGHT_TRIGGER_1) then shapes.draw_circle (536, 61, 20.0, RED); end if;

      -- Draw axis: left joystick
      left_thumb_color := (if check_down (GAMEPAD_BUTTON_LEFT_THUMB) then RED else BLACK);
      draw_circle (259, 152, 39.0, BLACK);
      draw_circle (259, 152, 34.0, LIGHTGRAY);
      draw_circle (
         259 + int (get_gamepad_axis_movement (gamepad_number, GAMEPAD_AXIS_LEFT_X) * 20.0),
         152 + int (get_gamepad_axis_movement (gamepad_number, GAMEPAD_AXIS_LEFT_Y) * 20.0),
         25.0,
         left_thumb_color);

         --  Draw axis: right joystick
         right_thumb_color := (if check_down (GAMEPAD_BUTTON_RIGHT_THUMB) then RED else BLACK);
         raylib.shapes.draw_circle (461, 237, 38.0, BLACK);
         raylib.shapes.draw_circle (461, 237, 33.0, LIGHTGRAY);
         raylib.shapes.draw_circle (
            461 + int (raylib.core.get_gamepad_axis_movement (gamepad_number, GAMEPAD_AXIS_RIGHT_X) * 20.0),
            237 + int (raylib.core.get_gamepad_axis_movement (gamepad_number, GAMEPAD_AXIS_RIGHT_Y) * 20.0),
            25.0,
            right_thumb_color);

         -- Draw axis: left-right triggers
         left_trigger_pos := int ((1.0 + get_gamepad_axis_movement (gamepad_number, GAMEPAD_AXIS_LEFT_TRIGGER)) / 2.0 * 70.0);
         right_trigger_pos := int ((1.0 + get_gamepad_axis_movement(gamepad_number, GAMEPAD_AXIS_RIGHT_TRIGGER)) / 2.0 * 70.0);
         shapes.draw_rectangle (170, 30, 15, 70, GRAY);
         shapes.draw_rectangle (604, 30, 15, 70, GRAY);
         shapes.draw_rectangle (170, 30, 15, left_trigger_pos, RED);
         shapes.draw_rectangle (604, 30, 15, right_trigger_pos, RED);
   end Draw_xbox_Gamepad;

   procedure draw_playstation_gamepad is
      use raylib.core;
      use raylib.shapes;

      left_thumb_color, right_thumb_color : Color;
      left_trigger_pos, right_trigger_pos: int;
   begin
      textures.draw_texture (texPs3Pad, 0, 0, DARKGRAY);

      -- Draw buttons: ps
      if check_down (GAMEPAD_BUTTON_MIDDLE) then draw_circle (396, 222, 13.0, RED); end if;

      -- Draw buttons: basic
      if check_down (GAMEPAD_BUTTON_MIDDLE_LEFT)      then draw_rectangle (328, 170, 32, 13, RED); end if;
      if check_down (GAMEPAD_BUTTON_MIDDLE_RIGHT)     then draw_triangle (Vector2'(436.0, 168.0), Vector2'(436.0, 185.0), Vector2'(464.0, 177.0), RED); end if;
      if check_down (GAMEPAD_BUTTON_RIGHT_FACE_UP)    then draw_circle (557, 144, 13.0, LIME); end if;
      if check_down (GAMEPAD_BUTTON_RIGHT_FACE_RIGHT) then draw_circle (586, 173, 13.0, RED); end if;
      if check_down (GAMEPAD_BUTTON_RIGHT_FACE_DOWN)  then draw_circle (557, 203, 13.0, VIOLET); end if;
      if check_down (GAMEPAD_BUTTON_RIGHT_FACE_LEFT)  then draw_circle (527, 173, 13.0, PINK); end if;

      -- Draw buttons: d-pad
      draw_rectangle(225, 132, 24, 84, BLACK);
      draw_rectangle(195, 161, 84, 25, BLACK);
      if check_down (GAMEPAD_BUTTON_LEFT_FACE_UP)    then draw_rectangle (225, 132, 24, 29, RED); end if;
      if check_down (GAMEPAD_BUTTON_LEFT_FACE_DOWN)  then draw_rectangle (225, 132+54, 24, 29, RED); end if;
      if check_down (GAMEPAD_BUTTON_LEFT_FACE_LEFT)  then draw_rectangle (195, 161, 30, 25, RED); end if;
      if check_down (GAMEPAD_BUTTON_LEFT_FACE_RIGHT) then draw_rectangle (195+54, 161, 30, 25, RED); end if;

      -- Draw buttons: left-right back buttons
      if check_down (GAMEPAD_BUTTON_LEFT_TRIGGER_1)  then shapes.draw_circle (239, 82, 20.0, RED); end if;
      if check_down (GAMEPAD_BUTTON_RIGHT_TRIGGER_1) then shapes.draw_circle (557, 82, 20.0, RED); end if;

      -- Draw axis: left joystick
      left_thumb_color := (if check_down (GAMEPAD_BUTTON_LEFT_THUMB) then RED else BLACK);
      draw_circle (319, 255, 35.0, left_thumb_color);
      draw_circle (319, 255, 31.0, LIGHTGRAY);
      draw_circle (319 + int(get_gamepad_axis_movement (gamepad_number, GAMEPAD_AXIS_LEFT_X) * 20.0),
                   255 + int(get_gamepad_axis_movement (gamepad_number, GAMEPAD_AXIS_LEFT_Y) * 20.0),
                   25.0,
                   left_thumb_color);

      -- Draw axis: right joystick
      right_thumb_color := (if check_down (GAMEPAD_BUTTON_RIGHT_THUMB) then RED else BLACK);
      draw_circle (475, 255, 35.0, right_thumb_color);
      draw_circle (475, 255, 31.0, LIGHTGRAY);
      draw_circle (475 + int(get_gamepad_axis_movement (gamepad_number, GAMEPAD_AXIS_RIGHT_X) * 20.0),
                   255 + int(get_gamepad_axis_movement (gamepad_number, GAMEPAD_AXIS_RIGHT_Y) * 20.0),
                   25.0,
                   right_thumb_color);

      -- Draw axis: left-right triggers
      left_trigger_pos := int ((1.0 - get_gamepad_axis_movement (gamepad_number, GAMEPAD_AXIS_LEFT_TRIGGER))/2.0 * 70.0);
      right_trigger_pos := int ((1.0 - get_gamepad_axis_movement (gamepad_number, GAMEPAD_AXIS_RIGHT_TRIGGER))/2.0 * 70.0);
      draw_rectangle (169, 48, 15, 70, GRAY);
      draw_rectangle (611, 48, 15, 70, GRAY);
      draw_rectangle (169, 48, 15, left_trigger_pos, RED);
      draw_rectangle (611, 48, 15, right_trigger_pos, RED);
   end draw_playstation_gamepad;

   use type IC.unsigned;
begin

   set_config_flags (FLAG_MSAA_4X_HINT);  -- Set MSAA 4X hint before windows creation

   window.init (screenWidth, screenHeight, "raylib [core] example - gamepad input");

   texPs3Pad  := textures.load ("resources/ps3.png");
   texXboxPad := textures.load ("resources/xbox.png");

   raylib.set_target_FPS (30);

   while not raylib.window.should_close loop
      begin_drawing;
      clear_background (raylib.RAYWHITE);

      if gamepad_number > 0 and raylib.core.is_key_pressed (KEY_LEFT) = IC.True
      then
         gamepad_number := gamepad_number - 1;
         detect_gamepad;
      elsif raylib.core.is_key_pressed (KEY_RIGHT)
      then
         gamepad_number := gamepad_number + 1;
         detect_gamepad;
      end if;

      if my_gamepad = UNKNOWN or my_gamepad = UNAVAILABLE then
         raylib.text.draw ("Gamepad " & gamepad_number'Img & " not detected", 10, 10, 15, GRAY);
         detect_gamepad;
      else
         raylib.text.draw ("GAMEPAD " & gamepad_number'Img & " : " & raylib.core.get_gamepad_name (gamepad_number), 10, 10, 15, BLACK);

         case my_gamepad is
         when XBOX => draw_Xbox_gamepad;
         when PLAYSTATION | OTHER => draw_playstation_gamepad;
         when others => null;
         end case;

         declare
            axis_count : int := get_gamepad_axis_count (gamepad_number);
            axis : Gamepad_Axis;
            axis_level: Float;
         begin
            text.draw ("DETECTED AXIS [" & axis_count'Img  &"]", 10, 50, 10, MAROON);
            for I in 0..(axis_count-1) loop
               axis := Gamepad_Axis'Val (I);
               axis_level := get_gamepad_axis_movement (gamepad_number, axis);
               text.draw ("AXIS " & I'Img & ": " & axis_level'Img, 20, 70 + 20*I, 10, MAROON);
            end loop;
         end;
      end if;

      end_drawing;

   end loop;

   textures.unload (texXboxPad);
   textures.unload (texPs3Pad);

   raylib.window.close;

end core_input_gamepad;
