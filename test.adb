with Raylib.Window;
with Raylib.UI;
with Ada.Strings.Fixed;
with Ada.Text_IO;

procedure Test is
    use Raylib;
    use Raylib.Input;

    package FS renames Ada.Strings.Fixed;
    package TIO renames Ada.Text_IO;

    FPS                             : constant := 30;
    BOTTOM_MSG_CENTER, BOTTOM_MSG_Y : Float;
    BOTTOM_MSG_FONT_SIZE            : constant int := 24;

    padding   : constant int := 10;
    font_size : constant int := 18;

    type tests is (Lines, Perspective, GUI);
    current_test : tests := GUI;
    procedure select_next_test is
    begin
        current_test :=
           (if current_test = tests'Last
            then tests'First
            else tests'Succ (current_test));
    end select_next_test;

    keep_dialog_box   : Boolean := False;
    show_dialog_box   : Boolean := False;
    gui_ctrl_toggle   : Boolean := False;
    gui_ctrl_checkbox : Boolean := True;
    gui_label_text    : String (1 .. 25) := (others => ' ');
    gui_value_textbox : String (gui_label_text'Range);
    gui_textbox_edit  : Boolean := False;

    is_editing_text_area : Boolean := False;
    cursor_text_area     : Vector2;
    buffer_text_area     : String (1 .. 1024);

    use type raylib.UI.Control_State;

    function bottom_msg_text return String
    is ("Press [TAB] to change test - " & current_test'Img);
begin
    Ada.Strings.Fixed.Move ("Hello !", gui_value_textbox);
    Ada.Strings.Fixed.Move ("Hello !", buffer_text_area);

    Raylib.Window.Init (800, 400, "raylib Ada binding - tests");

    Raylib.Window.Set_Target_FPS (FPS);
    --  raylib.Window.Hide_Cursor;
    raylib.UI.Set_Alpha (0.90);

    BOTTOM_MSG_CENTER := Float (Window.Get_Render_Width) / 2.0;
    BOTTOM_MSG_Y := Float (Window.Get_Render_Height) - 32.0;

    Raylib.Window.Set_Window_Opacity (1.0);

    while not Raylib.Window.Should_Close loop

        if Input.Is_Key_Pressed (Raylib.KEY_TAB) then
            select_next_test;
        end if;

        Raylib.Window.Begin_Drawing;
        Raylib.Window.Clear_Background (Raylib.RAYWHITE);

        Text.Draw_Pro
           (F        => Text.Get_Font_Default,
            text     => bottom_msg_text,
            position => Vector2'(BOTTOM_MSG_CENTER, BOTTOM_MSG_Y),
            origin   =>
               (Float (Text.Measure (bottom_msg_text, BOTTOM_MSG_FONT_SIZE))
                / 2.0,
                0.0),
            rotation => 0.0,
            fontSize => Float (BOTTOM_MSG_FONT_SIZE),
            spacing  => 1.0,
            tint     => BROWN);

        case current_test is
            when Lines       =>
                Text.Draw ("raylib.Shapes.Draw_Line",
                    padding,
                    10,
                    font_size,
                    raylib.BLACK);

                Shapes.Draw_Line
                   (start_posX => padding,
                    start_posY => 28,
                    end_posX   => 300,
                    end_posy   => 28,
                    c          => raylib.RED);
                --
                Text.Draw ("raylib.Shapes.Draw_Line_V",
                    padding,
                    38,
                    font_size,
                    BLACK);
                raylib.Shapes.Draw_Line_V
                   (start_pos => (Float (padding), 56.0),
                    end_pos   => (300.0, 56.0),
                    c         => raylib.RED);

                --
                Text.Draw ("raylib.Shapes.Draw_Line_Ex",
                    padding,
                    66,
                    font_size,
                    BLACK);
                raylib.Shapes.Draw_Line_Ex
                   (start_pos => (Float (padding), 84.0),
                    end_pos   => (300.0, 84.0),
                    thick     => 2.5,
                    c         => RED);

            when Perspective =>
                Text.Draw ("Perspective test not yet implemented",
                    1,
                    38,
                    font_size,
                    BLACK);

            when GUI         =>
                declare
                    --  type Cell is record
                    --     bounds : Rectangle;
                    --     pos   : Vector2;
                    --     size  : Vector2;
                    --  end record;
                    --  Cells : array (1 .. 10) of Cell;

                    --
                    Left_Align      : constant Float := 100.0;
                    Right_Align     : constant Float := 350.0;
                    Vertical_Margin : constant Float := 4.0;
                    Cell_Height     : constant Float := 32.0;
                    Cell_Spacing    : constant Float :=
                       Cell_Height + Vertical_Margin;

                    status_area : Rectangle := (0.0, 0.0, 800.0, 32.0);

                    toggle_area   : Rectangle :=
                       (Left_Align, Cell_Spacing * 2.0, 140.0, Cell_Height);
                    button_area   : Rectangle :=
                       (Left_Align, Cell_Spacing * 4.0, 140.0, Cell_Height);
                    checkbox_area : Rectangle :=
                       (Left_Align,
                        Cell_Spacing * 6.0,
                        Cell_Height,
                        Cell_Height);

                    label_area    : Rectangle :=
                       (Right_Align, Cell_Spacing, 0.0, Cell_Height);
                    textbox_area  : Rectangle :=
                       (Right_Align, Cell_Spacing * 2.0, 140.0, Cell_Height);
                    label2_area   : Rectangle :=
                       (Right_Align, Cell_Spacing * 3.0, 140.0, Cell_Height);
                    textarea_area : Rectangle :=
                       (Right_Align,
                        Cell_Spacing * 4.0,
                        200.0,
                        Cell_Spacing * 5.0);

                begin
                    UI.Statusbar (status_area, "UI test");

                    UI.Toggle
                       (toggle_area,
                        (if gui_ctrl_toggle
                         then "Activate UI"
                         else "Deactivate UI"),
                        gui_ctrl_toggle);
                    if gui_ctrl_toggle then
                        raylib.UI.Set_State (raylib.UI.DISABLED);
                    end if;

                    show_dialog_box :=
                       raylib.UI.Button (button_area, "Show Dialog Box");
                    raylib.UI.Checkbox
                       (bounds  => checkbox_area,
                        text    =>
                           (if gui_ctrl_checkbox
                            then "Checked"
                            else "Unchecked"),
                        checked => gui_ctrl_checkbox);
                    UI.Label (label_area, "Input : " & gui_label_text);
                    if UI.Textbox
                          (textbox_area, gui_value_textbox, gui_textbox_edit)
                    then
                        Ada.Strings.Fixed.Move
                           (gui_value_textbox, gui_label_text);
                    end if;

                    if UI.Button (label2_area, "Copy to clipboard") then
                        raylib.Window.Set_Clipboard_Text (buffer_text_area);
                    end if;
                    cursor_text_area :=
                       UI.Textbox_Multi
                          (textarea_area,
                           buffer_text_area,
                           is_editing_text_area);

                    if show_dialog_box or keep_dialog_box then
                        keep_dialog_box := TRUE;
                        UI.Panel ((200.0, 60.0, 300.0, 240.0));
                        UI.Label
                           ((200.0 + 300.0 / 2.0 - 70.0, 70.0, 140.0, 32.0),
                            "Dialog Box");

                        if UI.Button
                              ((200.0 + 300.0 / 2.0 - 50.0,
                                60.0 + 240.0 - 30.0 - 10.0,
                                100.0,
                                30.0),
                               "Cancel")
                        then
                            keep_dialog_box := False;
                        end if;
                    end if;

                    raylib.UI.Set_State (raylib.UI.NORMAL);
                end;
        end case;

        Raylib.Window.End_Drawing;
    end loop;

    Raylib.Window.Close;

end Test;
