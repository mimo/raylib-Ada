with Raylib.Window;
with Ada.Strings.Unbounded;

procedure Input_Box is
    screen_height : Integer := 450;
    screen_width  : Integer := 800;

    use Raylib;

    use type Raylib.int;
    use Ada.Strings.Unbounded;

    textBox : Rectangle :=
       (x      => Float (screen_width) / 2.0 - 100.0,
        y      => 180.0,
        width  => 225.0,
        height => 50.0);

    frameCounter : Integer := 0;

    inputString : String := "";

    maxInputChars : constant Integer := 9;
    name          : Unbounded_String := Null_Unbounded_String;

    mouseOnText : Boolean := False;
    letterCount : Natural := 0;
    char        : int;

    function isAnyKeyPressed return Boolean is
        key : int := raylib.input.get_char_pressed;
    begin
        return (key >= 32 and key <= 126);
    end isAnyKeyPressed;

begin
    --
    --  Initialization
    -------------------------------------
    raylib.window.init
       (screen_width, screen_height, "raylib [text] example - input box");

    window.set_target_FPS (10);

    while not raylib.window.should_close loop
        --
        --  Update
        -------------------------------------

        if raylib.shapes.check_collision_point_rec
              (raylib.input.get_mouse_position, textBox)
        then
            mouseOnText := True;
        else
            mouseOnText := False;
        end if;

        if mouseOnText = True then
            Raylib.Input.Set_Mouse_Cursor (raylib.MOUSE_CURSOR_IBEAM);
            char := Raylib.Input.Get_Char_Pressed; --get_key_pressed;

            while char > 0 loop
                if (char >= 32 and char <= 125)
                   and then (letterCount < maxInputChars)
                then
                    Append (name, Character'val (char));
                    letterCount := letterCount + 1;
                end if;

                char := raylib.input.get_char_pressed;
            end loop;

            if Raylib.Input.Is_Key_Pressed (Raylib.KEY_BACKSPACE) then
                if Length (name) > 0 and letterCount > 0 then
                    Delete (name, letterCount, letterCount);
                    letterCount := letterCount - 1;
                else
                    -- reset count
                    letterCount := 0;
                end if;
            end if;
        else
            Raylib.Input.Set_Mouse_Cursor (Raylib.MOUSE_CURSOR_DEFAULT);
        end if;

        if mouseOnText = True then
            frameCounter := frameCounter + 1;
        else
            frameCounter := 0;
        end if;

        --
        --  Draw
        -------------------------------------
        Window.Begin_Drawing;
        Window.clear_background (RAYWHITE);
        Raylib.Text.Draw ("PLACE MOUSE OVER INPUT BOX!", 240, 140, 20, GRAY);
        Raylib.Shapes.Draw_Rectangle_Rec (textBox, LIGHTGRAY);

        if mouseOnText then
            Raylib.Shapes.Draw_Rectangle_Lines
               (int (textBox.x),
                int (textBox.y),
                int (textBox.width),
                int (textBox.height),
                RED);
        else
            Raylib.Shapes.Draw_Rectangle_Lines
               (int (textBox.x),
                int (textBox.y),
                int (textBox.width),
                int (textBox.height),
                DARKGRAY);
        end if;

        Raylib.Text.Draw
           (To_String (name),
            Integer (textBox.x) + 5,
            Integer (textBox.y) + 8,
            40,
            MAROON);
        Raylib.Text.Draw
           ("Input Chars: " & Natural'image (letterCount) & "/9",
            315,
            250,
            20,
            DARKGRAY);

        if mouseOnText then
            if letterCount < maxInputChars then
                ---
                -- Draw blinking underscore char
                if ((frameCounter / 20) mod 2) = 0 then
                    Raylib.Text.Draw
                       ("_",
                        Integer (textBox.x)
                        + 8
                        + Integer(Raylib.Text.Measure (To_String (name), 40)),
                        Integer (textBox.y) + 12,
                        40,
                        MAROON);
                end if;
            else
                Raylib.Text.Draw
                   ("Please BACKSPACE to delete chars...", 230, 300, 20, GRAY);
            end if;
        end if;

        window.end_drawing;
    end loop;
    raylib.window.close;
end input_box;
