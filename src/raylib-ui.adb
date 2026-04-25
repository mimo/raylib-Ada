
with Ada.Strings.Fixed;

package body raylib.UI is

   global_state : Control_State := NORMAL;
   global_locked : Boolean := False;
   global_frame_counter : Integer := 0;
   global_alpha : Float := 1.0;
   global_style_loaded : Boolean := False;
   global_style : array (Controls'Range, Properties'Range) of unsigned;
   global_font : Font;

   function Get_State return Control_State is
   begin
      return global_state;
   end Get_State;

   procedure Set_State (state : Control_State) is
   begin
      global_state := state;
   end Set_State;

   procedure Lock is
   begin
      global_locked := True;
   end Lock;

   procedure Unlock is
   begin
      global_locked := False;
   end Unlock;

   function Get_Alpha return Transparency is
   begin
      return global_alpha;
   end Get_Alpha;

   procedure Set_Alpha (alpha : Transparency) is
   begin
      global_alpha := Float (alpha);
   end Set_Alpha;

   function To_Text_Alignment (position : unsigned)
      return Text_Alignment_Type is
   begin
      return Text_Alignment_Type'Val (position);
   end To_Text_Alignment;

   function To_Unsigned (value : Text_Alignment_Type) return unsigned is
   begin
      return Text_Alignment_Type'Pos (value);
   end To_Unsigned;

   function Get_Property_By_State (
      C : Property_Element;
      state : Control_State)
      return Properties
   is
      index : Integer;
   begin
      index := Property_Element'Pos (C) + Control_State'Pos (state) * 3;
      return Properties'Val (index);
   end Get_Property_By_State;

   ---
   --
   procedure Draw_Text (
      text : String;
      bounds : Rectangle;
      alignment : Text_Alignment_Type;
      tint : Color)
   is
      use type int;
      ICON_TEXT_PADDING : constant := 4;
      --  Vertical alignment for pixel perfect
      TEXT_VALIGN_PIXEL_OFFSET : constant int := int (bounds.height) mod 2;
      iconId : int := 0;
      position : Vector2;
      textWidth, textHeight : int;
   begin
      if text'Length > 0 then
         --  Check text for icon and move cursor
         --  text = GetTextIcon(text, &iconId);

         --  Get text position depending on alignment and iconId
         ----------------------------------------------------------------------
         position := (bounds.x, bounds.y);
         --  NOTE: We get text size after icon been processed
         textWidth  := Get_Text_Width (text);
         textHeight := int (Get_Style (DEFAULT, TEXT_SIZE));

         --  #if defined(RAYGUI_SUPPORT_ICONS)
         --  if (iconId >= 0)
         --  {
         --    textWidth += RICON_SIZE;
         --    // WARNING: If only icon provided,
         --    // text could be pointing to eof character!
         --    if ((text != NULL) && (text[0] != '\0'))
         --      textWidth += ICON_TEXT_PADDING;
         --  }
         --  #endif

         --  Check guiTextAlign global variables
         case alignment is
         when TEXT_ALIGN_LEFT =>
            position.x := bounds.x;
            position.y := bounds.y + bounds.height / 2.0
                                   - Float (textHeight / 2)
                                   + Float (TEXT_VALIGN_PIXEL_OFFSET);
         when TEXT_ALIGN_CENTER =>
            position.x := bounds.x + bounds.width / 2.0
                                    - Float (textWidth / 2);
            position.y := bounds.y + bounds.height / 2.0
                                   - Float (textHeight / 2)
                                   + Float (TEXT_VALIGN_PIXEL_OFFSET);
         when TEXT_ALIGN_RIGHT =>
            position.x := bounds.x + bounds.width - Float (textWidth);
            position.y := bounds.y + bounds.height / 2.0
                                   - Float (textHeight / 2)
                                   + Float (TEXT_VALIGN_PIXEL_OFFSET);
         end case;

         --  NOTE: Make sure we get pixel-perfect coordinates,
         --  In case of decimals we got weird text positioning
         position.x := Float (int (position.x));
         position.y := Float (int (position.y));

         ----------------------------------------------------------------------
         --  Draw text (with icon if available)
         ----------------------------------------------------------------------
         if iconId >= 0 then
            --  NOTE: We consider icon height,probably different than text size
            --  GuiDrawIcon(iconId, RAYGUI_CLITERAL(Vector2){ position.x, bounds.y + bounds.height/2 - RICON_SIZE/2 + TEXT_VALIGN_PIXEL_OFFSET(bounds.height) }, 1, tint);
            --  position.x += (RICON_SIZE + ICON_TEXT_PADDING);
            null;
         end if;

         DRAW_TEXT : declare
            size    : constant unsigned := Get_Style (DEFAULT, TEXT_SIZE);
            spacing : constant unsigned := Get_Style (DEFAULT, TEXT_SPACING);
         begin
            raylib.text.draw_ex (
               global_font,
               text,
               position,
               Float (size),
               Float (spacing),
               tint);
         end DRAW_TEXT;
         ----------------------------------------------------------------------
      end if;

   end Draw_Text;

   procedure Draw_Border (
      rec : Rectangle;
      border_width : int;
      border_color : Color)
   is
      use type int;
      use raylib.shapes;
      height : constant int := int (rec.height);
      width  : constant int := int (rec.width);
      left   : constant int := int (rec.x);
      right  : constant int := left + int (rec.width);
      top    : constant int := int (rec.y);
      bottom : constant int := top + height;
   begin
      Draw_Rectangle (left, top, width, border_width, border_color);
      Draw_Rectangle (
         left,
         top + border_width,
         border_width,
         height - 2 * border_width,
         border_color);
      Draw_Rectangle (
         right - border_width,
         top + border_width,
         border_width,
         height - 2 * border_width,
         border_color);
      Draw_Rectangle (
         left,
         bottom - border_width,
         width,
         border_width,
         border_color);
   end Draw_Border;

   procedure Draw_Rectangle (
      rec : Rectangle;
      border_width : int;
      border_color : Color;
      bg : Color)
   is
      use raylib.shapes;
   begin
      if Integer (bg.a) > 0 then
         draw_rectangle_rec (rec, bg);
      end if;

      if Integer (border_width) > 0 then
         Draw_Border (rec, border_width, border_color);
      end if;
   end Draw_Rectangle;

   function Get_Property_Index (P : Properties)
      return Integer is
   begin
      return (case P is
         when BORDER_COLOR_NORMAL   => 0,
         when BASE_COLOR_NORMAL     => 1,
         when TEXT_COLOR_NORMAL     => 2,
         when BORDER_COLOR_FOCUSED  => 3,
         when BASE_COLOR_FOCUSED    => 4,
         when TEXT_COLOR_FOCUSED    => 5,
         when BORDER_COLOR_PRESSED  => 6,
         when BASE_COLOR_PRESSED    => 7,
         when TEXT_COLOR_PRESSED    => 8,
         when BORDER_COLOR_DISABLED => 9,
         when BASE_COLOR_DISABLED   => 10,
         when TEXT_COLOR_DISABLED   => 11,
         when BORDER_WIDTH     => 12,
         when TEXT_PADDING     => 13,
         when TEXT_ALIGNMENT   => 14,
         when RESERVED         => 15,
         when TEXT_SIZE        => 16,
         when TEXT_SPACING     => 17,
         when LINE_COLOR       => 18,
         when BACKGROUND_COLOR => 19,
         when GROUP_PADDING    => 16,
         when SLIDER_WIDTH     => 16,
         when SLIDER_PADDING   => 17,
         when PROGRESS_PADDING     => 16,
         when CHECK_PADDING        => 16,
         when COMBO_BUTTON_WIDTH   => 16,
         when COMBO_BUTTON_PADDING => 17,
         when TEXT_INNER_PADDING   => 16,
         when TEXT_LINES_PADDING   => 17,
         when COLOR_SELECTED_FG    => 18,
         when COLOR_SELECTED_BG    => 19);
   end Get_Property_Index;

   --  Get text bounds considering control bounds
   function Get_Text_Bounds (
      control : Controls;
      bounds : Rectangle)
      return Rectangle
   is
      control_border_width : Float;
      textBounds : Rectangle := bounds;
      alignment : Text_Alignment_Type;
   begin
      alignment := To_Text_Alignment (Get_Style (control, TEXT_ALIGNMENT));
      control_border_width := Float (Get_Style (control, BORDER_WIDTH));
      textBounds.x := bounds.x + control_border_width;
      textBounds.y := bounds.y + control_border_width;
      textBounds.width  := bounds.width  - 2.0 * control_border_width;
      textBounds.height := bounds.height - 2.0 * control_border_width;

      --  Consider TEXT_PADDING properly,
      --  depends on control type and TEXT_ALIGNMENT
      case control is
      when COMBOBOX =>
         textBounds.width := textBounds.width
                         - Float (Get_Style (control, COMBO_BUTTON_WIDTH))
                         + Float (Get_Style (control, COMBO_BUTTON_PADDING));
      when VALUEBOX =>
         --  NOTE: ValueBox text value always centered,
         --  text padding applies to Label
         null;
      when others =>
         if alignment = TEXT_ALIGN_RIGHT
         then textBounds.x := textBounds.x
                              - Float (Get_Style (control, TEXT_PADDING));
         else
            textBounds.x := textBounds.x + Float (Get_Style (control, TEXT_PADDING));
            textBounds.width := textBounds.width - 2.0 * Float (Get_Style (control, TEXT_PADDING));
         end if;
      end case;

      --  TODO: Special cases (no Label):
      --    COMBOBOX, DROPDOWNBOX, LISTVIEW (scrollbar?)
      --  More special cases (Label side):
      --    CHECKBOX, SLIDER, VALUEBOX, SPINNER

      return textBounds;
   end Get_Text_Bounds;

   --  Gui get text width using default font
   function Get_Text_Width (text : String)
      return int
   is
      size : Vector2 := (0.0, 0.0);
   begin
      if text'Length > 0 then
         size := raylib.text.measure_ex (
            global_font,
            text,
            Float (Get_Style (DEFAULT, TEXT_SIZE)),
            Float (Get_Style (DEFAULT, TEXT_SPACING)));
      end if;
      --  TODO: Consider text icon width here???
      return int (size.x);
   end Get_Text_Width;

   ---
   --  UI elements
   ----------------------------------------------------------------------------

procedure Label (
      bounds : Rectangle;
      text   : String) is
   begin
         Draw_Text
           (text      => text,
            bounds    => Get_Text_Bounds (LABEL, bounds),
            alignment => Text_Alignment_Type'Val (Get_Style (LABEL, TEXT_ALIGNMENT)),
            tint      => colors.get_color (Get_Style (LABEL, Get_Property_By_State (UI.TEXT, global_state)))
           );
      end Label;

   function Button (
      bounds : raylib.Rectangle;
      Label : String)
      return Boolean
   is
      button_state : Control_State := global_state;
      pressed : Boolean := False;
      mouse_point : Vector2;
   begin
      if global_state /= DISABLED and not global_locked then
         mouse_point := raylib.input.get_mouse_position;
         --  Check Button state
         if raylib.shapes.check_collision_point_rec (mouse_point, bounds) then
            --
            if raylib.input.is_mouse_button_down (raylib.MOUSE_BUTTON_LEFT)
            then button_state := raylib.UI.PRESSED;
            else button_state := FOCUSED;
            end if;

            if raylib.input.is_mouse_button_released (raylib.MOUSE_BUTTON_LEFT)
            then
               pressed := True;
            end if;
         end if;
      end if;

      --  Draw control
      --------------------------------------------------------------------
      declare
         border_property : Properties;
         fill_property : Properties;
         text_property : Properties;

         border_color : Color;
         fill_color : Color;
         text_color : Color;

         border_width : float;

         use type unsigned;
         use type int;
         use colors;
      begin
         border_property := Get_Property_By_State (BORDER, button_state);
         fill_property := Get_Property_By_State (BASE, button_state);
         text_property := Get_Property_By_State (TEXT, button_state);

         border_color := get_color (Get_Style (DEFAULT, border_property));
         fill_color   := get_color (Get_Style (DEFAULT, fill_property));
         text_color   := get_color (Get_Style (BUTTON, text_property));

         border_width := float (Get_Style (BUTTON, UI.BORDER_WIDTH));

         raylib.shapes.draw_rectangle_lines_ex (
            bounds,
            border_width,
            fade (border_color, global_alpha));

         raylib.shapes.Draw_Rectangle (
            int (bounds.x + border_width),
            int (bounds.y + border_width),
            int (bounds.width)  - int(2.0 * border_width),
            int (bounds.height) - int(2.0 * border_width),
            fade (fill_color, global_alpha));

         Draw_Text (
            Label,
            Get_Text_Bounds (BUTTON, bounds),
            To_Text_Alignment (Get_Style (BUTTON, TEXT_ALIGNMENT)),
            fade (text_color, global_alpha));
      end;
      ------------------------------------------------------------------
      return pressed;
   end Button;

   -----------------------------------------------------------------------
   --  Check Box control, returns true when active
   --
   procedure Checkbox (
      bounds  : in Rectangle;
      text    : in String;
      checked : in out Boolean)
   is
      state : Control_State := global_state;
      total_bounds : Rectangle := (0.0, 0.0, 0.0, 0.0);
      text_bounds  : Rectangle := (0.0, 0.0, 0.0, 0.0);
      mouse_point  : Vector2;
      alignment  : Text_Alignment_Type;
      padding    : Float;
      text_size  : Float;
      text_color : Color;
      check_padding : unsigned;
      border_width  : unsigned;
   begin
      border_width  := Get_Style (CHECKBOX, UI.BORDER_WIDTH);
      check_padding := Get_Style (CHECKBOX, UI.CHECK_PADDING);

      alignment  := To_Text_Alignment (Get_Style (CHECKBOX, TEXT_ALIGNMENT));
      padding    := Float (Get_Style (CHECKBOX, TEXT_PADDING));
      text_size  := Float (Get_Style (DEFAULT, UI.TEXT_SIZE));
      text_color := colors.get_color (
                       Get_Style (LABEL,
                          Get_Property_By_State (UI.TEXT, state)));

      if text'Length > 0 then
         text_bounds.width  := Float (Get_Text_Width (text));
         text_bounds.height := Float (Get_Style (DEFAULT, UI.TEXT_SIZE));

         text_bounds.x := (if alignment = TEXT_ALIGN_LEFT
                           then bounds.x - text_bounds.width - padding
                           else bounds.x + bounds.width + padding);

         text_bounds.y := bounds.y + bounds.height / 2.0 - text_size / 2.0;
      end if;

      ---
      --  Update control
      --
      if state /= DISABLED and not global_locked
      then
         mouse_point := input.get_mouse_position;

         total_bounds.x := (if alignment = TEXT_ALIGN_LEFT
                            then text_bounds.x else bounds.x);
         total_bounds.y := bounds.y;
         total_bounds.width := bounds.width + text_bounds.width + padding;
         total_bounds.height := bounds.height;

         --  Check Checkbox state
         if shapes.check_collision_point_rec (mouse_point, total_bounds)
         then
            state := (if input.is_mouse_button_down (MOUSE_BUTTON_LEFT)
                      then PRESSED else FOCUSED);

            if input.is_mouse_button_released (MOUSE_BUTTON_LEFT)
            then checked := not checked;
            end if;
         end if;
      end if;

      drawing : declare
         check : Rectangle;
         check_color : Color;
         use type unsigned;
      begin
         alignment := (if alignment = TEXT_ALIGN_RIGHT
                        then TEXT_ALIGN_LEFT else TEXT_ALIGN_RIGHT);

         Draw_Rectangle (
            bounds,
            int (border_width),
            colors.fade (
               colors.get_color (
                  Get_Style (CHECKBOX, Get_Property_By_State (BORDER, state))),
               global_alpha),
            BLANK);

         if checked then
            check_color := colors.get_color (
                  Get_Style (
                     CHECKBOX, Get_Property_By_State (UI.TEXT, state)));

            check := (
               bounds.x + Float (border_width + check_padding),
               bounds.y + Float (border_width + check_padding),
               bounds.width - Float (2 * (border_width + check_padding)),
               bounds.height - Float (2 * (border_width + check_padding)));

            UI.Draw_Rectangle (
               check,
               0,
               BLANK,
               colors.fade (check_color, global_alpha));
         end if;

         if text'Length > 0 then
            Draw_Text (
               text,
               text_bounds,
               alignment,
               colors.fade (text_color, global_alpha));

         end if;
      end drawing;
      -------------------------------------------------------------------------
   end Checkbox;

   ----------------------------------------------------------------------
   procedure Panel (bounds : Rectangle) is
      panel_state  : constant Control_State := global_state;
      BG_ELEMENT, BORDER_ELEMENT : Properties;
      bg_color, border_color : Color;
      border_width : int;

      use colors;
   begin
      border_width := int (Get_Style (DEFAULT, UI.BORDER_WIDTH));
      BG_ELEMENT := (if panel_state = DISABLED
                     then BASE_COLOR_DISABLED
                     else BACKGROUND_COLOR);
      BORDER_ELEMENT := (if panel_state = DISABLED
                         then BORDER_COLOR_DISABLED
                         else LINE_COLOR);
      ---
      --  Draw control
      --
      bg_color := colors.get_color (Get_Style (DEFAULT, BG_ELEMENT));
      border_color := colors.get_color (Get_Style (DEFAULT, BORDER_ELEMENT));

      UI.Draw_Rectangle (
         bounds,
         border_width,
         fade (border_color, global_alpha),
         fade (bg_color, global_alpha));
   end Panel;

   -------------------------------------------------------------------------
   procedure Statusbar (
      bounds : Rectangle;
      text : String)
   is
      state : constant Control_State := global_state;
      border_color : Color;
      border_width : unsigned;
      text_color : Color;
      bg_color : Color;
      bg_bounds : Rectangle;
      use type raylib.unsigned;
   begin
      border_width := Get_Style (STATUSBAR, UI.BORDER_WIDTH);
      bg_bounds.x := bounds.x + Float (border_width);
      bg_bounds.y := bounds.y + Float (border_width);
      bg_bounds.width  := bounds.width  - Float (border_width * 2);
      bg_bounds.height := bounds.height - Float (border_width * 2);

      text_color := colors.get_color (
                       Get_Style (STATUSBAR, (if state /= DISABLED
                                              then TEXT_COLOR_NORMAL
                                              else TEXT_COLOR_DISABLED)));

      border_color := colors.get_color (
                         Get_Style (STATUSBAR,
                                    (if state /= DISABLED
                                     then BORDER_COLOR_NORMAL
                                     else BORDER_COLOR_DISABLED)));

      bg_color := colors.get_color (
                     Get_Style (STATUSBAR,
                                (if state /= DISABLED
                                 then BASE_COLOR_NORMAL
                                 else BASE_COLOR_DISABLED)));

      shapes.draw_rectangle_lines_ex (
         bounds,
         float (border_width),
         colors.fade (border_color, global_alpha));

      shapes.draw_rectangle_rec (
         bg_bounds,
         colors.fade (bg_color, global_alpha));

      Draw_Text (
         text,
         Get_Text_Bounds (STATUSBAR, bounds),
         Text_Alignment_Type'Val (Get_Style (STATUSBAR, TEXT_ALIGNMENT)),
         colors.fade (text_color, global_alpha));

   end Statusbar;

   procedure Toggle (
      bounds : in Rectangle;
      text   : in String;
      active : in out Boolean)
   is
      toggle_state : Control_State := global_state;
      mouse_point  : Vector2;
   begin
      if global_state /= DISABLED and not global_locked then
         mouse_point := input.get_mouse_position;

         if shapes.check_collision_point_rec (mouse_point, bounds) then

            if input.is_mouse_button_down (MOUSE_BUTTON_LEFT) then
               toggle_state := PRESSED;
            elsif input.is_mouse_button_released (MOUSE_BUTTON_LEFT) then
               toggle_state := NORMAL;
               active := not active;
            else
               toggle_state := FOCUSED;
            end if;
         end if;
      end if;

      drawing : declare
         line_thickness : unsigned;
         color_property : Properties;
         bg_color_property : Properties;
         text_color_property : Properties;
         outline, background, text_color : Color;

         use type int;
         use colors;
      begin
         line_thickness :=  Get_Style (TOGGLE, BORDER_WIDTH);
         color_property := (if active then BORDER_COLOR_PRESSED
                            else Get_Property_By_State (BORDER, toggle_state));

         bg_color_property := (if active then BASE_COLOR_PRESSED
                               else Get_Property_By_State (BASE, toggle_state));

         text_color_property := (if active then TEXT_COLOR_PRESSED
                                 else Get_Property_By_State (raylib.ui.TEXT, toggle_state));

         background := get_color (Get_Style (TOGGLE, bg_color_property));
         outline    := get_color (Get_Style (TOGGLE, color_property));
         text_color := get_color (Get_Style (TOGGLE, text_color_property));
         shapes.draw_rectangle_lines_ex (
            bounds,
            float(line_thickness),
            colors.fade (outline, global_alpha));

         shapes.Draw_Rectangle (
            posX => int (bounds.x) + int(line_thickness),
            posY => int (bounds.y) + int(line_thickness),
            width  => int (bounds.width)  - 2 * int(line_thickness),
            height => int (bounds.height) - 2 * int(line_thickness),
            c => colors.fade (background, global_alpha));

         Draw_Text (
            text,
            bounds,
            Text_Alignment_Type'Val (Get_Style (TOGGLE, TEXT_ALIGNMENT)),
            colors.fade (text_color, global_alpha));
      end drawing;
   end Toggle;

   -- return true when validated (enter pressed)
   function Textbox (
      bounds    : in Rectangle;
      text      : in out String;
      edit_mode : in out Boolean)
      return Boolean
   is
      use type int;

      state : Control_State := global_state;
      mouse_point : Vector2 := input.get_mouse_position;
      mouse_inside : Boolean := Boolean(shapes.check_collision_point_rec (mouse_point, bounds));
      is_validated : Boolean := False;

      text_color, border_color, bg_color : Color;
      border_width : int;

      FAKE_SPACE : constant Character := ASCII.CR;
      pressed_char : int;
      text_length : integer;

      function transform (S : in String) return String is
         -- TODO: transform TAB to spaces
         transformed : String := Ada.Strings.Fixed.Trim (S, Ada.Strings.Right);
         last_character : Character;
         index : Integer;
      begin
         index := Ada.Strings.Fixed.Index_Non_Blank (transformed, Ada.Strings.Backward);
         if index = 0 then return "";
         end if;

         last_character := transformed (index);
         if last_character = FAKE_SPACE then
            transformed(transformed'Last) := ' ';
         end if;

         return transformed;
      end transform;

      use colors;
   begin
      text_length := Ada.Strings.Fixed.Index_Non_Blank (Text, Ada.Strings.Backward);

      if state = DISABLED or global_locked then
         goto DRAW;
      end if;

      if Boolean (input.is_mouse_button_pressed (MOUSE_BUTTON_LEFT)) then
         if mouse_inside then
            state := PRESSED;
            edit_mode := true;
         else
            state := NORMAL;
         end if;
      end if;

      if edit_mode then

         global_frame_counter := global_frame_counter + 1;

         loop
            if Boolean (input.is_key_pressed (KEY_ENTER)) then
               is_validated := True;
               state := NORMAL;
               exit;
            end if;
            -- Handle backspace key and a string terminator if the last character is a space
            if Boolean(input.is_key_pressed(KEY_BACKSPACE)) and text_length >= 1 then
               text (text_length) := ' ';

               if text_length > 1 and then text (text_length - 1) = ' ' then
                  text (text_length - 1) := FAKE_SPACE;
               end if;
               
               exit;
            end if;

            pressed_char := raylib.input.get_char_pressed;
            exit when pressed_char = 0 or text_length = text'Last;

            -- Replace added space by a fake because fixed strings length
            -- is determined by the last non-blank character, ui being stateless
            -- input length would be lost
            if text_length > 0 and then Text (text_length) = FAKE_SPACE then
               Text (text_length) := ' ';
            end if;
            case pressed_char is
            when 32 =>-- ASCII value of space
               -- FAKE_SPACE play a similar role to the null character in C strings
               -- only when the last character is a space
               text_length := text_length + 1;
               text (text_length) := FAKE_SPACE;
            when 33..125 => --  ASCII printable characters
               text_length := text_length + 1;
               Text (text_length) := Character'val (pressed_char);
            when others =>
               utils.trace_log (LOG_INFO, "Not handled: " & Character'Val (pressed_char) & ", pos "  & pressed_char'Img);
            end case;
         end loop;
      elsif mouse_inside then
         state := FOCUSED;
      end if;

<<DRAW>>
      text_color   := get_color (Get_Style (TEXTBOX, UI.TEXT, state));
      border_color := get_color (Get_Style (TEXTBOX, BORDER,  state));
      bg_color     := get_color (Get_Style (TEXTBOX, BASE,    state));
      border_width := int (Get_Style (TEXTBOX, UI.BORDER_WIDTH));

      -- Draw border and background
      Draw_Rectangle (
         bounds,
         border_width,
         fade (border_color, global_alpha),
         fade (bg_color, global_alpha));

      --  Draw blinking cursor
      if edit_mode and (global_frame_counter / 20) mod 2 = 0 then
         declare
            cursor : Rectangle;
            text_width : int := Get_Text_Width (transform (text));
         begin
            cursor.x := Get_Text_Bounds (TEXTBOX, bounds).x + Float (text_width) + 2.0; -- 2px cursor offset is an approximation for text spacing
            cursor.y := bounds.y + Float (Get_Style (TEXTBOX, TEXT_INNER_PADDING));
            cursor.width := 1.5;
            cursor.height := Float (int (Get_Style (DEFAULT, TEXT_SIZE)) * 2);

            Draw_Rectangle (
               cursor,
               0,
                 text_color,
                 fade (text_color, global_alpha));
         end;
      end if;

      --  Draw input text
      Draw_Text (
         transform (text),
         Get_Text_Bounds (TEXTBOX, bounds),
         To_Text_Alignment (Get_Style (TEXTBOX, TEXT_ALIGNMENT)),
         colors.fade (text_color, global_alpha));

      -- TODO: Would be better to return the cursor position
      -- as edit_mode is an in out parameter
      return is_validated;
   end Textbox;

   function Is_Activated (bounds : Rectangle)
      return Boolean
   is
      mouse_point  : Vector2 := input.get_mouse_position;
      left_click   : Boolean := Boolean(input.is_mouse_button_pressed (MOUSE_BUTTON_LEFT));
      mouse_inside : Boolean := Boolean(shapes.check_collision_point_rec (mouse_point, bounds));
   begin
      return mouse_inside and left_click;
   end Is_Activated;

   function Textbox_Multi (
      bounds : Rectangle;
      text : in out String;
      edit_mode : in out Boolean)
      return Vector2
   is
      state : Control_State := global_state;
      isactive : Boolean := Is_Activated (bounds);
      cursor_position : Vector2 := (0.0, 0.0);

      style_inner_padding : Float := Float (Get_Style (TEXTBOX, TEXT_INNER_PADDING));
      textAreaBounds : Rectangle := (
         bounds.x + style_inner_padding,
         bounds.y + style_inner_padding,
         bounds.width  - 2.0 * style_inner_padding,
         bounds.height - 2.0 * style_inner_padding
      );
   begin
      shapes.draw_rectangle_lines_ex (bounds, 0.8, DARKGRAY);
      shapes.draw_rectangle_lines_ex (textAreaBounds, 0.8, LIME);

      return cursor_position;
   end Textbox_Multi;

   procedure Set_Style (
      control : Controls;
      property : Properties;
      value : unsigned) is
   begin
      global_style (control, property) := value;
   end Set_Style;

   function Get_Style (control : Controls; property : Properties)
      return unsigned is
   begin
      if not global_style_loaded then
         Load_Style_Default;
      end if;

      return global_style (control, property);
   end Get_Style;

   function Get_Style (
      control : Controls;
      element : Property_Element;
      state : Control_State)
      return unsigned is
   begin
      return Get_Style (control, Get_Property_By_State (element, state));
   end Get_Style;

   procedure Load_Style_Default is
      blank_color : unsigned;
   begin
      blank_color := unsigned (colors.color_to_int (BLANK));

      Set_Style (DEFAULT, BORDER_COLOR_NORMAL,  16#838383ff#);
      Set_Style (DEFAULT, BASE_COLOR_NORMAL,    16#c9c9c9ff#);
      Set_Style (DEFAULT, TEXT_COLOR_NORMAL,    16#686868ff#);
      Set_Style (DEFAULT, BORDER_COLOR_FOCUSED, 16#5bb2d9ff#);
      Set_Style (DEFAULT, BASE_COLOR_FOCUSED,   16#c9effeff#);
      Set_Style (DEFAULT, TEXT_COLOR_FOCUSED,   16#6c9bbcff#);
      Set_Style (DEFAULT, BORDER_COLOR_PRESSED, 16#0492c7ff#);
      Set_Style (DEFAULT, BASE_COLOR_PRESSED,   16#97e8ffff#);
      Set_Style (DEFAULT, TEXT_COLOR_PRESSED,   16#368bafff#);
      Set_Style (DEFAULT, BORDER_COLOR_DISABLED, 16#b5c1c2ff#);
      Set_Style (DEFAULT, BASE_COLOR_DISABLED,  16#e6e9e9ff#);
      Set_Style (DEFAULT, TEXT_COLOR_DISABLED,  16#aeb7b8ff#);
      Set_Style (DEFAULT, BORDER_WIDTH, 1);
      Set_Style (DEFAULT, TEXT_PADDING, 0);
      Set_Style (DEFAULT, TEXT_ALIGNMENT, To_Unsigned (TEXT_ALIGN_CENTER));

      for Ctrl in LABEL .. Controls'Last loop
         for Property in Properties'Range loop
            global_style (Ctrl, Property) := global_style (DEFAULT, Property);
         end loop;
      end loop;

      --  Default extended properties
      Set_Style (DEFAULT, TEXT_SIZE, 14);
      Set_Style (DEFAULT, TEXT_SPACING, 1);
      Set_Style (DEFAULT, BACKGROUND_COLOR, 16#f5f5f5ff#);
      Set_Style (DEFAULT, LINE_COLOR, 16#90abb5ff#);

      Set_Style (LABEL,    TEXT_ALIGNMENT, To_Unsigned (TEXT_ALIGN_LEFT));
      Set_Style (BUTTON,   TEXT_ALIGNMENT, To_Unsigned (TEXT_ALIGN_CENTER));
      Set_Style (BUTTON,   BORDER_WIDTH, 2);
      Set_Style (SLIDER,   TEXT_PADDING, 5);
      Set_Style (SLIDER,   SLIDER_WIDTH, 15);
      Set_Style (SLIDER,   SLIDER_PADDING, 1);
      Set_Style (CHECKBOX, TEXT_PADDING, 5);
      Set_Style (CHECKBOX, TEXT_ALIGNMENT, To_Unsigned (TEXT_ALIGN_RIGHT));
      Set_Style (CHECKBOX, CHECK_PADDING, 1);
      Set_Style (CHECKBOX, BORDER_WIDTH, 1);
      Set_Style (TOGGLE,   GROUP_PADDING, 2);
      Set_Style (TEXTBOX,  TEXT_PADDING, 5);
      Set_Style (TEXTBOX,  TEXT_ALIGNMENT, To_Unsigned (TEXT_ALIGN_LEFT));
      Set_Style (TEXTBOX,  TEXT_LINES_PADDING, 5);
      Set_Style (TEXTBOX,  TEXT_INNER_PADDING, 4);
      Set_Style (TEXTBOX,  COLOR_SELECTED_FG, 16#f0fffeff#);
      Set_Style (TEXTBOX,  COLOR_SELECTED_BG, 16#839affe0#);
      Set_Style (TEXTBOX,  BASE_COLOR_PRESSED, blank_color);

      global_font := raylib.text.get_font_default;
      global_style_loaded := True;
   end Load_Style_Default;

   ---------------------------------------------------------------------------
   --  Theme system implementation
   ---------------------------------------------------------------------------

   Active_Theme : Theme := Default_Theme;
   Loaded_Fonts : array (Font_Role) of Font;
   Active_Font_Loaded : Boolean := False;
   Override_Color : Color_Role := Primary;
   Override_Font : Font_Role := Default;
   Use_Override_Color : Boolean := False;
   Use_Override_Font : Boolean := False;

   procedure Load (T : Theme := Default_Theme) is
      use Ada.Strings.Fixed;
      font_path : String (1 .. 256);
      path_length : Natural;
   begin
      --  Unload previous fonts if any
      if Active_Font_Loaded then
         Unload;
      end if;

      Active_Theme := T;

      --  Load fonts into VRAM
      for F in Font_Role loop
         font_path := T.Fonts (F);
         path_length := Index_Non_Blank (font_path, Ada.Strings.Backward);

         if path_length > 0 then
            Loaded_Fonts (F) := raylib.text.load_font (
               Trim (font_path (1 .. path_length), Ada.Strings.Both));
         else
            --  Use default font if path is empty
            Loaded_Fonts (F) := raylib.text.get_font_default;
         end if;
      end loop;

      Active_Font_Loaded := True;

      --  Set the default font as global font
      global_font := Loaded_Fonts (Default);

      --  Apply theme colors to raygui style
      Apply_Theme_To_Style;
   end Load;

   procedure Unload is
      default_font : constant Font := raylib.text.get_font_default;
   begin
      if not Active_Font_Loaded then
         return;
      end if;

      --  Free fonts from VRAM (except default font)
      for F in Font_Role loop
         --  Don't unload the default font (it's managed by raylib)
         if Loaded_Fonts (F).baseSize /= default_font.baseSize or else
            Loaded_Fonts (F).glyphCount /= default_font.glyphCount
         then
            raylib.text.unload_font (Loaded_Fonts (F));
         end if;
      end loop;

      Active_Font_Loaded := False;
      global_font := default_font;
   end Unload;

   procedure Use_Color (C : Color_Role) is
   begin
      Override_Color := C;
      Use_Override_Color := True;
   end Use_Color;

   procedure Use_Font (F : Font_Role) is
   begin
      Override_Font := F;
      Use_Override_Font := True;
   end Use_Font;

   procedure Stop_Override is
   begin
      Use_Override_Color := False;
      Use_Override_Font := False;
   end Stop_Override;

   function Color_Of (C : Color_Role) return Color is
   begin
      return Active_Theme.Colors (C);
   end Color_Of;

   function Font_Of (F : Font_Role) return Font is
   begin
      if not Active_Font_Loaded then
         return raylib.text.get_font_default;
      end if;
      return Loaded_Fonts (F);
   end Font_Of;

   function Size_Of (S : Size_Role) return Positive is
   begin
      return Active_Theme.Sizes (S);
   end Size_Of;

   --  Apply theme colors to raygui global style
   procedure Apply_Theme_To_Style is
      use colors;

      primary : constant Color := Active_Theme.Colors (Primary);
      secondary : constant Color := Active_Theme.Colors (Secondary);
      text_primary : constant Color := Active_Theme.Colors (Text_Primary);
      text_secondary : constant Color := Active_Theme.Colors (Text_Secondary);
      background : constant Color := Active_Theme.Colors (Background);
      border : constant Color := Active_Theme.Colors (Border);

      function To_Unsigned (C : Color) return unsigned is
      begin
         return unsigned (color_to_int (C));
      end To_Unsigned;

      --  Generate lighter/darker variants for focused/pressed states
      function Lighten (C : Color; Amount : Float := 0.2) return Color is
         Result : Color;
      begin
         Result.r := unsigned_char (Float'Min (255.0, Float (C.r) * (1.0 + Amount)));
         Result.g := unsigned_char (Float'Min (255.0, Float (C.g) * (1.0 + Amount)));
         Result.b := unsigned_char (Float'Min (255.0, Float (C.b) * (1.0 + Amount)));
         Result.a := C.a;
         return Result;
      end Lighten;

      function Darken (C : Color; Amount : Float := 0.2) return Color is
         Result : Color;
      begin
         Result.r := unsigned_char (Float (C.r) * (1.0 - Amount));
         Result.g := unsigned_char (Float (C.g) * (1.0 - Amount));
         Result.b := unsigned_char (Float (C.b) * (1.0 - Amount));
         Result.a := C.a;
         return Result;
      end Darken;
   begin
      --  Map theme colors to raygui 4-state color system
      --  NORMAL state
      Set_Style (DEFAULT, BASE_COLOR_NORMAL, To_Unsigned (primary));
      Set_Style (DEFAULT, TEXT_COLOR_NORMAL, To_Unsigned (text_primary));
      Set_Style (DEFAULT, BORDER_COLOR_NORMAL, To_Unsigned (border));

      --  FOCUSED state (lighter)
      Set_Style (DEFAULT, BASE_COLOR_FOCUSED, To_Unsigned (Lighten (primary)));
      Set_Style (DEFAULT, TEXT_COLOR_FOCUSED, To_Unsigned (Darken (text_primary)));
      Set_Style (DEFAULT, BORDER_COLOR_FOCUSED, To_Unsigned (Lighten (border)));

      --  PRESSED state (use secondary color)
      Set_Style (DEFAULT, BASE_COLOR_PRESSED, To_Unsigned (secondary));
      Set_Style (DEFAULT, TEXT_COLOR_PRESSED, To_Unsigned (text_secondary));
      Set_Style (DEFAULT, BORDER_COLOR_PRESSED, To_Unsigned (Darken (border)));

      --  DISABLED state (desaturated)
      Set_Style (DEFAULT, BASE_COLOR_DISABLED, To_Unsigned (Lighten (primary, 0.3)));
      Set_Style (DEFAULT, TEXT_COLOR_DISABLED, To_Unsigned (Lighten (text_primary, 0.4)));
      Set_Style (DEFAULT, BORDER_COLOR_DISABLED, To_Unsigned (Lighten (border, 0.3)));

      --  Background and line colors
      Set_Style (DEFAULT, BACKGROUND_COLOR, To_Unsigned (background));
      Set_Style (DEFAULT, LINE_COLOR, To_Unsigned (border));

      --  Copy DEFAULT colors to all controls
      for Ctrl in LABEL .. Controls'Last loop
         for Prop in BORDER_COLOR_NORMAL .. TEXT_COLOR_DISABLED loop
            global_style (Ctrl, Prop) := global_style (DEFAULT, Prop);
         end loop;
      end loop;
   end Apply_Theme_To_Style;

end raylib.UI;
