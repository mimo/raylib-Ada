with Raylib.Window;
with Ada.Strings.Fixed;

procedure core_input_gamepad is
    use raylib;
    use raylib.input;
    use type raylib.int;

    screenWidth  : constant := 800;
    screenHeight : constant := 450;

    texPs3Pad, texXboxPad : Texture2D;

    type Gamepad_Type is (UNAVAILABLE, UNKNOWN, XBOX, PLAYSTATION, OTHER);
    my_gamepad     : Gamepad_Type := UNKNOWN;
    gamepad_number : int := 0;

    procedure detect_gamepad is
    begin
        if not raylib.Input.Is_Gamepad_Available (gamepad_number) then
            my_gamepad := UNAVAILABLE;
            return;
        end if;

        declare
            gamepad_name : String := Get_Gamepad_Name (gamepad_number);
            package FS renames Ada.Strings.Fixed;
        begin
            if gamepad_name'length = 0 then
                my_gamepad := UNKNOWN;
                return;
            end if;

            if FS.Index (gamepad_name, "Xbox") /= 0
               or FS.Index (gamepad_name, "Microsoft") /= 0
            then
                my_gamepad := XBOX;
            elsif FS.Index (gamepad_name, "PLAYSTATION") /= 0
               or FS.Index (gamepad_name, "Sense") /= 0
               or FS.Index (gamepad_name, "Wireless") /= 0
            then
                my_gamepad := PLAYSTATION;
            else
                my_gamepad := OTHER;
            end if;

        end;
    end detect_gamepad;

    function check_down (button_id : Gamepad_Button) return Boolean is
    begin
        return Is_Gamepad_Button_Down (gamepad_number, button_id);
    end check_down;

    procedure Draw_Xbox_Gamepad is
        use raylib.shapes;

        left_thumb_color, right_thumb_color : Color;
        left_trigger_pos, right_trigger_pos : int;
    begin
        Textures.Draw_Texture (texXboxPad, 0, 0, DARKGRAY);

        -- Draw buttons: xbox home
        if check_down (GAMEPAD_BUTTON_MIDDLE) then
            Draw_Circle (394, 89, 19.0, RED);
        end if;

        -- Draw buttons: basic
        if check_down (GAMEPAD_BUTTON_MIDDLE_RIGHT) then
            Draw_Circle (436, 150, 9.0, RED);
        end if;
        if check_down (GAMEPAD_BUTTON_MIDDLE_LEFT) then
            Draw_Circle (352, 150, 9.0, RED);
        end if;
        if check_down (GAMEPAD_BUTTON_RIGHT_FACE_LEFT) then
            Draw_Circle (501, 151, 15.0, BLUE);
        end if;
        if check_down (GAMEPAD_BUTTON_RIGHT_FACE_DOWN) then
            Draw_Circle (536, 187, 15.0, LIME);
        end if;
        if check_down (GAMEPAD_BUTTON_RIGHT_FACE_RIGHT) then
            Draw_Circle (572, 151, 15.0, MAROON);
        end if;
        if check_down (GAMEPAD_BUTTON_RIGHT_FACE_UP) then
            Draw_Circle (536, 115, 15.0, GOLD);
        end if;

        -- Draw buttons: d-pad
        Shapes.Draw_Rectangle (317, 202, 19, 71, BLACK);
        Shapes.Draw_Rectangle (293, 228, 69, 19, BLACK);
        if check_down (GAMEPAD_BUTTON_LEFT_FACE_UP) then
            Draw_Rectangle (317, 202, 19, 26, RED);
        end if;
        if check_down (GAMEPAD_BUTTON_LEFT_FACE_DOWN) then
            Draw_Rectangle (317, 202 + 45, 19, 26, RED);
        end if;
        if check_down (GAMEPAD_BUTTON_LEFT_FACE_LEFT) then
            Draw_Rectangle (292, 228, 25, 19, RED);
        end if;
        if check_down (GAMEPAD_BUTTON_LEFT_FACE_RIGHT) then
            Draw_Rectangle (292 + 44, 228, 26, 19, RED);
        end if;

        -- Draw buttons: left-right back buttons
        if check_down (GAMEPAD_BUTTON_LEFT_TRIGGER_1) then
            Shapes.Draw_Circle (259, 61, 20.0, RED);
        end if;
        if check_down (GAMEPAD_BUTTON_RIGHT_TRIGGER_1) then
            Shapes.Draw_Circle (536, 61, 20.0, RED);
        end if;

        -- Draw axis: left joystick
        left_thumb_color :=
           (if check_down (GAMEPAD_BUTTON_LEFT_THUMB) then RED else BLACK);
        Draw_Circle (259, 152, 39.0, BLACK);
        Draw_Circle (259, 152, 34.0, LIGHTGRAY);
        Draw_Circle
           (259
            + int
                 (Get_Gamepad_Axis_Movement
                     (gamepad_number, GAMEPAD_AXIS_LEFT_X)
                  * 20.0),
            152
            + int
                 (Get_Gamepad_Axis_Movement
                     (gamepad_number, GAMEPAD_AXIS_LEFT_Y)
                  * 20.0),
            25.0,
            left_thumb_color);

        --  Draw axis: right joystick
        right_thumb_color :=
           (if check_down (GAMEPAD_BUTTON_RIGHT_THUMB) then RED else BLACK);
        raylib.Shapes.Draw_Circle (461, 237, 38.0, BLACK);
        raylib.Shapes.Draw_Circle (461, 237, 33.0, LIGHTGRAY);
        raylib.Shapes.Draw_Circle
           (461
            + int
                 (Get_Gamepad_Axis_Movement
                     (gamepad_number, GAMEPAD_AXIS_RIGHT_X)
                  * 20.0),
            237
            + int
                 (Get_Gamepad_Axis_Movement
                     (gamepad_number, GAMEPAD_AXIS_RIGHT_Y)
                  * 20.0),
            25.0,
            right_thumb_color);

        -- Draw axis: left-right triggers
        left_trigger_pos :=
           int
              ((1.0
                + Get_Gamepad_Axis_Movement
                     (gamepad_number, GAMEPAD_AXIS_LEFT_TRIGGER))
               / 2.0
               * 70.0);
        right_trigger_pos :=
           int
              ((1.0
                + Get_Gamepad_Axis_Movement
                     (gamepad_number, GAMEPAD_AXIS_RIGHT_TRIGGER))
               / 2.0
               * 70.0);
        Shapes.Draw_Rectangle (170, 30, 15, 70, GRAY);
        Shapes.Draw_Rectangle (604, 30, 15, 70, GRAY);
        Shapes.Draw_Rectangle (170, 30, 15, left_trigger_pos, RED);
        Shapes.Draw_Rectangle (604, 30, 15, right_trigger_pos, RED);
    end Draw_xbox_Gamepad;

    procedure draw_playstation_gamepad is
        use raylib.shapes;

        left_thumb_color, right_thumb_color : Color;
        left_trigger_pos, right_trigger_pos : int;
    begin
        Textures.Draw_Texture (texPs3Pad, 0, 0, DARKGRAY);

        -- Draw buttons: ps
        if check_down (GAMEPAD_BUTTON_MIDDLE) then
            Draw_Circle (396, 222, 13.0, RED);
        end if;

        -- Draw buttons: basic
        if check_down (GAMEPAD_BUTTON_MIDDLE_LEFT) then
            Draw_Rectangle (328, 170, 32, 13, RED);
        end if;
        if check_down (GAMEPAD_BUTTON_MIDDLE_RIGHT) then
            Draw_Triangle
               (Vector2'(436.0, 168.0),
                Vector2'(436.0, 185.0),
                Vector2'(464.0, 177.0),
                RED);
        end if;
        if check_down (GAMEPAD_BUTTON_RIGHT_FACE_UP) then
            Draw_Circle (557, 144, 13.0, LIME);
        end if;
        if check_down (GAMEPAD_BUTTON_RIGHT_FACE_RIGHT) then
            Draw_Circle (586, 173, 13.0, RED);
        end if;
        if check_down (GAMEPAD_BUTTON_RIGHT_FACE_DOWN) then
            Draw_Circle (557, 203, 13.0, VIOLET);
        end if;
        if check_down (GAMEPAD_BUTTON_RIGHT_FACE_LEFT) then
            Draw_Circle (527, 173, 13.0, PINK);
        end if;

        -- Draw buttons: d-pad
        Draw_Rectangle (225, 132, 24, 84, BLACK);
        Draw_Rectangle (195, 161, 84, 25, BLACK);
        if check_down (GAMEPAD_BUTTON_LEFT_FACE_UP) then
            Draw_Rectangle (225, 132, 24, 29, RED);
        end if;
        if check_down (GAMEPAD_BUTTON_LEFT_FACE_DOWN) then
            Draw_Rectangle (225, 132 + 54, 24, 29, RED);
        end if;
        if check_down (GAMEPAD_BUTTON_LEFT_FACE_LEFT) then
            Draw_Rectangle (195, 161, 30, 25, RED);
        end if;
        if check_down (GAMEPAD_BUTTON_LEFT_FACE_RIGHT) then
            Draw_Rectangle (195 + 54, 161, 30, 25, RED);
        end if;

        -- Draw buttons: left-right back buttons
        if check_down (GAMEPAD_BUTTON_LEFT_TRIGGER_1) then
            Shapes.Draw_Circle (239, 82, 20.0, RED);
        end if;
        if check_down (GAMEPAD_BUTTON_RIGHT_TRIGGER_1) then
            Shapes.Draw_Circle (557, 82, 20.0, RED);
        end if;

        -- Draw axis: left joystick
        left_thumb_color :=
           (if check_down (GAMEPAD_BUTTON_LEFT_THUMB) then RED else BLACK);
        Draw_Circle (319, 255, 35.0, left_thumb_color);
        Draw_Circle (319, 255, 31.0, LIGHTGRAY);
        Draw_Circle
           (319
            + int
                 (Get_Gamepad_Axis_Movement
                     (gamepad_number, GAMEPAD_AXIS_LEFT_X)
                  * 20.0),
            255
            + int
                 (Get_Gamepad_Axis_Movement
                     (gamepad_number, GAMEPAD_AXIS_LEFT_Y)
                  * 20.0),
            25.0,
            left_thumb_color);

        -- Draw axis: right joystick
        right_thumb_color :=
           (if check_down (GAMEPAD_BUTTON_RIGHT_THUMB) then RED else BLACK);
        Draw_Circle (475, 255, 35.0, right_thumb_color);
        Draw_Circle (475, 255, 31.0, LIGHTGRAY);
        Draw_Circle
           (475
            + int
                 (Get_Gamepad_Axis_Movement
                     (gamepad_number, GAMEPAD_AXIS_RIGHT_X)
                  * 20.0),
            255
            + int
                 (Get_Gamepad_Axis_Movement
                     (gamepad_number, GAMEPAD_AXIS_RIGHT_Y)
                  * 20.0),
            25.0,
            right_thumb_color);

        -- Draw axis: left-right triggers
        left_trigger_pos :=
           int
              ((1.0
                - Get_Gamepad_Axis_Movement
                     (gamepad_number, GAMEPAD_AXIS_LEFT_TRIGGER))
               / 2.0
               * 70.0);
        right_trigger_pos :=
           int
              ((1.0
                - Get_Gamepad_Axis_Movement
                     (gamepad_number, GAMEPAD_AXIS_RIGHT_TRIGGER))
               / 2.0
               * 70.0);
        Draw_Rectangle (169, 48, 15, 70, GRAY);
        Draw_Rectangle (611, 48, 15, 70, GRAY);
        Draw_Rectangle (169, 48, 15, left_trigger_pos, RED);
        Draw_Rectangle (611, 48, 15, right_trigger_pos, RED);
    end draw_playstation_gamepad;

begin

    Window.Set_Config_Flags
       (FLAG_MSAA_4X_HINT);  -- Set MSAA 4X hint before windows creation

    Window.Init (screenWidth, screenHeight, "raylib [core] example - gamepad input");

    texPs3Pad := Textures.Load ("core/resources/ps3.png");
    texXboxPad := Textures.Load ("core/resources/xbox.png");

    Window.Set_Target_FPS (30);

    while not raylib.Window.Should_Close loop
        Window.Begin_Drawing;
        Window.Clear_Background (raylib.RAYWHITE);

        if gamepad_number > 0 and Is_Key_Pressed (KEY_LEFT) then
            gamepad_number := gamepad_number - 1;
            detect_gamepad;
        elsif Is_Key_Pressed (KEY_RIGHT) then
            gamepad_number := gamepad_number + 1;
            detect_gamepad;
        end if;

        if my_gamepad = UNKNOWN or my_gamepad = UNAVAILABLE then
            raylib.Text.Draw ("Gamepad " & gamepad_number'Img & " not detected",
                10,
                10,
                15,
                GRAY);
            detect_gamepad;
        else
            raylib.Text.Draw ("GAMEPAD "
                & gamepad_number'Img
                & " : "
                & Get_Gamepad_Name (gamepad_number),
                10,
                10,
                15,
                BLACK);

            case my_gamepad is
                when XBOX                =>
                    draw_Xbox_gamepad;

                when PLAYSTATION | OTHER =>
                    draw_playstation_gamepad;

                when others              =>
                    null;
            end case;

            declare
                axis_count : int := Get_Gamepad_Axis_Count (gamepad_number);
                axis       : Gamepad_Axis;
                axis_level : Float;
            begin
                Text.Draw ("DETECTED AXIS [" & axis_count'Img & "]",
                    10,
                    50,
                    10,
                    MAROON);
                for I in 0 .. (axis_count - 1) loop
                    axis := Gamepad_Axis'Val (I);
                    axis_level :=
                       Get_Gamepad_Axis_Movement (gamepad_number, axis);
                    Text.Draw ("AXIS " & I'Img & ": " & axis_level'Img,
                        20,
                        70 + 20 * I,
                        10,
                        MAROON);
                end loop;
            end;
        end if;

        Window.End_Drawing;

    end loop;

    Textures.Unload (texXboxPad);
    Textures.Unload (texPs3Pad);

    raylib.Window.Close;

end core_input_gamepad;
