with raylib;
with ada.Strings.Unbounded;

procedure input_box is
    screen_height: Integer := 450;
    screen_width: Integer := 800;

    use raylib;

    use type raylib.int;
    use type raylib.bool;
    use Ada.Strings.Unbounded;

    textBox: Rectangle := (x => float(screen_width)/2.0 - 100.0, y => 180.0, width => 225.0, height => 50.0);

    frameCounter : Integer := 0;

    inputString: string := "";


    maxInputChars : constant Integer := 9;
    name : Unbounded_String := Null_Unbounded_String;

    mouseOnText: Boolean := false;
    letterCount: integer := 0;
    char : int;
    
    function isAnyKeyPressed return boolean is
        key: int := raylib.core.get_key_pressed;
    begin
        return (key >= 32 and key <= 126);
    end isAnyKeyPressed; 

begin
    raylib.window.init(
        screen_width,
        screen_height,
        "raylib [text] example - input box"
    );

    raylib.set_target_FPS(10);

    while not raylib.window.should_close loop

       if raylib.shapes.check_collision_point_rec (raylib.core.get_mouse_position, textBox) = bool(true) then
            mouseOnText := true;   
        else 
            mouseOnText := false;
        end if;

        if mouseOnText = true then 
            raylib.core.set_mouse_cursor(raylib.MOUSE_CURSOR_IBEAM);
            char := raylib.core.get_char_pressed; --get_key_pressed;
             
            while char > 0 loop 
                if (char >= 32 and char <= 125) and then (letterCount < maxInputChars) then 
                    Append(name,Character'val(char));
                    letterCount := letterCount + 1;
                end if;

                char := raylib.core.get_char_pressed;
            end loop;

            if raylib.core.is_key_pressed(raylib.KEY_BACKSPACE) then
                if Length(name) > 0 and letterCount > 0 then
                    Delete (name,letterCount,letterCount);
                    letterCount := letterCount - 1;
                else     -- reset count
                    letterCount := 0;
                end if;
            end if;
       else 
            raylib.core.set_mouse_cursor (raylib.MOUSE_CURSOR_DEFAULT); 
       end if;
       
       if mouseOnText = true then 
            frameCounter := frameCounter + 1;
       else 
            frameCounter := 0;
       end if; 

       raylib.begin_drawing;
       raylib.clear_background(RAYWHITE);
       raylib.text.draw("PLACE MOUSE OVER INPUT BOX!",240,140,20,GRAY);
       raylib.shapes.draw_rectangle_rec (textBox, LIGHTGRAY);

       if mouseOnText then 
            raylib.shapes.draw_rectangle_lines(int(textBox.x),int(textBox.y),int(textBox.width), int(textBox.height),red);
       else 
            raylib.shapes.draw_rectangle_lines(int(textBox.x),int(textBox.y),int(textBox.width), int(textBox.height),DARKGRAY);
       end if;

       raylib.text.draw(To_String(name),int(textBox.x)+5, int(textBox.y) + 8, 40, MAROON);
       raylib.text.draw("Input Chars: " & Natural'image(letterCount) & "/9",315, 250, 20, DARKGRAY);

       if mouseOnText then
            if letterCount < maxInputChars then
                if ((frameCounter/20) mod 2) = 0 then
                    raylib.text.draw(
                        "_",
                        int(textBox.x) + 8 + raylib.text.measure(To_String(name),40), 
                        int(textBox.y) + 12, 40, MAROON);
                end if;
            else 
                raylib.text.draw("Please BACKSPACE to delete chars...",230, 300, 20, GRAY);
            end if;
        end if;

       raylib.end_drawing; 
    end loop;
    raylib.window.close;
end input_box;