
with Ada.Strings.Fixed;

package body raylib.UI is

   global_state : Control_State := NORMAL;
   global_locked : Boolean := False;
   global_frame_counter : Integer := 0;
   global_alpha : Float := 1.0;
   global_style_loaded : Boolean := False;
   global_style : array (Controls'Range, Properties'Range) of unsigned;
   global_font : Font;

   function get_state return Control_State is
   begin
      return global_state;
   end get_state;

   procedure set_state (state : Control_State) is
   begin
      global_state := state;
   end set_state;

   procedure lock is
   begin
      global_locked := True;
   end lock;

   procedure unlock is
   begin
      global_locked := False;
   end unlock;

   function get_alpha return Transparency is
   begin
      return global_alpha;
   end get_alpha;

   procedure set_alpha (alpha : Transparency) is
   begin
      global_alpha := Float (alpha);
   end set_alpha;

   function to_text_alignment (position : unsigned)
      return Text_Alignment_Type is
   begin
      return Text_Alignment_Type'Val (position);
   end to_text_alignment;

   function to_unsigned (value : Text_Alignment_Type) return unsigned is
   begin
      return Text_Alignment_Type'Pos (value);
   end to_unsigned;

   function get_property_by_state (
      C : Property_Element;
      state : Control_State)
      return Properties
   is
      index : Integer;
   begin
      index := Property_Element'Pos (C) + Control_State'Pos (state) * 3;
      return Properties'Val (index);
   end get_property_by_state;

   ---
   --
   procedure draw_text (
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
         textWidth  := get_text_width (text);
         textHeight := int (get_style (DEFAULT, TEXT_SIZE));

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
            size    : constant unsigned := get_style (DEFAULT, TEXT_SIZE);
            spacing : constant unsigned := get_style (DEFAULT, TEXT_SPACING);
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

   end draw_text;

   procedure draw_border (
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
      draw_rectangle (left, top, width, border_width, border_color);
      draw_rectangle (
         left,
         top + border_width,
         border_width,
         height - 2 * border_width,
         border_color);
      draw_rectangle (
         right - border_width,
         top + border_width,
         border_width,
         height - 2 * border_width,
         border_color);
      draw_rectangle (
         left,
         bottom - border_width,
         width,
         border_width,
         border_color);
   end draw_border;

   procedure draw_rectangle (
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
         draw_border (rec, border_width, border_color);
      end if;
   end draw_rectangle;

   function get_property_index (P : Properties)
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
   end get_property_index;

   --  Get text bounds considering control bounds
   function get_text_bounds (
      control : Controls;
      bounds : Rectangle)
      return Rectangle
   is
      control_border_width : Float;
      textBounds : Rectangle := bounds;
      alignment : Text_Alignment_Type;
   begin
      alignment := to_text_alignment (get_style (control, TEXT_ALIGNMENT));
      control_border_width := Float (get_style (control, BORDER_WIDTH));
      textBounds.x := bounds.x + control_border_width;
      textBounds.y := bounds.y + control_border_width;
      textBounds.width  := bounds.width  - 2.0 * control_border_width;
      textBounds.height := bounds.height - 2.0 * control_border_width;

      --  Consider TEXT_PADDING properly,
      --  depends on control type and TEXT_ALIGNMENT
      case control is
      when COMBOBOX =>
         textBounds.width := textBounds.width
                         - Float (get_style (control, COMBO_BUTTON_WIDTH))
                         + Float (get_style (control, COMBO_BUTTON_PADDING));
      when VALUEBOX =>
         --  NOTE: ValueBox text value always centered,
         --  text padding applies to label
         null;
      when others =>
         if alignment = TEXT_ALIGN_RIGHT
         then textBounds.x := textBounds.x
                              - Float (get_style (control, TEXT_PADDING));
         else
            textBounds.x := textBounds.x + Float (get_style (control, TEXT_PADDING));
            textBounds.width := textBounds.width - 2.0 * Float (get_style (control, TEXT_PADDING));
         end if;
      end case;

      --  TODO: Special cases (no label):
      --    COMBOBOX, DROPDOWNBOX, LISTVIEW (scrollbar?)
      --  More special cases (label side):
      --    CHECKBOX, SLIDER, VALUEBOX, SPINNER

      return textBounds;
   end get_text_bounds;

   --  Gui get text width using default font
   function get_text_width (text : String)
      return int
   is
      size : Vector2 := (0.0, 0.0);
   begin
      if text'Length > 0 then
         size := raylib.text.measure_ex (
            global_font,
            text,
            Float (get_style (DEFAULT, TEXT_SIZE)),
            Float (get_style (DEFAULT, TEXT_SPACING)));
      end if;
      --  TODO: Consider text icon width here???
      return int (size.x);
   end get_text_width;

   ---
   --  UI elements
   ----------------------------------------------------------------------------

   function button (
      bounds : raylib.Rectangle;
      label : String)
      return Boolean
   is
      button_state : Control_State := global_state;
      pressed : Boolean := False;
      mouse_point : Vector2;
   begin
      if global_state /= DISABLED and not global_locked then
         mouse_point := raylib.input.get_mouse_position;
         --  Check button state
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

         border_width : int;

         use type unsigned;
         use type int;
         use colors;
      begin
         border_property := get_property_by_state (BORDER, button_state);
         fill_property := get_property_by_state (BASE, button_state);
         text_property := get_property_by_state (TEXT, button_state);

         border_color := get_color (get_style (DEFAULT, border_property));
         fill_color   := get_color (get_style (DEFAULT, fill_property));
         text_color   := get_color (get_style (BUTTON, text_property));

         border_width := int (get_style (BUTTON, UI.BORDER_WIDTH));

         raylib.shapes.draw_rectangle_lines_ex (
            bounds,
            border_width,
            fade (border_color, global_alpha));

         raylib.shapes.draw_rectangle (
            int (bounds.x) + border_width,
            int (bounds.y) + border_width,
            int (bounds.width)  - 2 * border_width,
            int (bounds.height) - 2 * border_width,
            fade (fill_color, global_alpha));

         draw_text (
            label,
            get_text_bounds (BUTTON, bounds),
            to_text_alignment (get_style (BUTTON, TEXT_ALIGNMENT)),
            fade (text_color, global_alpha));
      end;
      ------------------------------------------------------------------
      return pressed;
   end button;

   -----------------------------------------------------------------------
   --  Check Box control, returns true when active
   --
   procedure checkbox (
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
      border_width  := get_style (CHECKBOX, UI.BORDER_WIDTH);
      check_padding := get_style (CHECKBOX, UI.CHECK_PADDING);

      alignment  := to_text_alignment (get_style (CHECKBOX, TEXT_ALIGNMENT));
      padding    := Float (get_style (CHECKBOX, TEXT_PADDING));
      text_size  := Float (get_style (DEFAULT, UI.TEXT_SIZE));
      text_color := colors.get_color (
                       get_style (LABEL,
                          get_property_by_state (UI.TEXT, state)));

      if text'Length > 0 then
         text_bounds.width  := Float (get_text_width (text));
         text_bounds.height := Float (get_style (DEFAULT, UI.TEXT_SIZE));

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

         --  Check checkbox state
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

         draw_rectangle (
            bounds,
            int (border_width),
            colors.fade (
               colors.get_color (
                  get_style (CHECKBOX, get_property_by_state (BORDER, state))),
               global_alpha),
            BLANK);

         if checked then
            check_color := colors.get_color (
                  get_style (
                     CHECKBOX, get_property_by_state (UI.TEXT, state)));

            check := (
               bounds.x + Float (border_width + check_padding),
               bounds.y + Float (border_width + check_padding),
               bounds.width - Float (2 * (border_width + check_padding)),
               bounds.height - Float (2 * (border_width + check_padding)));

            UI.draw_rectangle (
               check,
               0,
               BLANK,
               colors.fade (check_color, global_alpha));
         end if;

         if text'Length > 0 then
            draw_text (
               text,
               text_bounds,
               alignment,
               colors.fade (text_color, global_alpha));

         end if;
      end drawing;
      -------------------------------------------------------------------------
   end checkbox;

   ----------------------------------------------------------------------
   procedure panel (bounds : Rectangle) is
      panel_state  : constant Control_State := global_state;
      BG_ELEMENT, BORDER_ELEMENT : Properties;
      bg_color, border_color : Color;
      border_width : int;

      use colors;
   begin
      border_width := int (get_style (DEFAULT, UI.BORDER_WIDTH));
      BG_ELEMENT := (if panel_state = DISABLED
                     then BASE_COLOR_DISABLED
                     else BACKGROUND_COLOR);
      BORDER_ELEMENT := (if panel_state = DISABLED
                         then BORDER_COLOR_DISABLED
                         else LINE_COLOR);
      ---
      --  Draw control
      --
      bg_color := colors.get_color (get_style (DEFAULT, BG_ELEMENT));
      border_color := colors.get_color (get_style (DEFAULT, BORDER_ELEMENT));

      UI.draw_rectangle (
         bounds,
         border_width,
         fade (border_color, global_alpha),
         fade (bg_color, global_alpha));
   end panel;

   -------------------------------------------------------------------------
   procedure statusbar (
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
      border_width := get_style (STATUSBAR, UI.BORDER_WIDTH);
      bg_bounds.x := bounds.x + Float (border_width);
      bg_bounds.y := bounds.y + Float (border_width);
      bg_bounds.width  := bounds.width  - Float (border_width * 2);
      bg_bounds.height := bounds.height - Float (border_width * 2);

      text_color := colors.get_color (
                       get_style (STATUSBAR, (if state /= DISABLED
                                              then TEXT_COLOR_NORMAL
                                              else TEXT_COLOR_DISABLED)));

      border_color := colors.get_color (
                         get_style (STATUSBAR,
                                    (if state /= DISABLED
                                     then BORDER_COLOR_NORMAL
                                     else BORDER_COLOR_DISABLED)));

      bg_color := colors.get_color (
                     get_style (STATUSBAR,
                                (if state /= DISABLED
                                 then BASE_COLOR_NORMAL
                                 else BASE_COLOR_DISABLED)));

      shapes.draw_rectangle_lines_ex (
         bounds,
         int (border_width),
         colors.fade (border_color, global_alpha));

      shapes.draw_rectangle_rec (
         bg_bounds,
         colors.fade (bg_color, global_alpha));

      draw_text (
         text,
         get_text_bounds (STATUSBAR, bounds),
         Text_Alignment_Type'Val (get_style (STATUSBAR, TEXT_ALIGNMENT)),
         colors.fade (text_color, global_alpha));

   end statusbar;

   procedure toggle (
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
         line_thickness : int;
         color_property : Properties;
         bg_color_property : Properties;
         text_color_property : Properties;
         outline, background, text_color : Color;

         use type int;
         use colors;
      begin
         line_thickness :=  int (get_style (TOGGLE, BORDER_WIDTH));
         color_property := (if active then BORDER_COLOR_PRESSED
                            else get_property_by_state (BORDER, toggle_state));

         bg_color_property := (if active then BASE_COLOR_PRESSED
                               else get_property_by_state (BASE, toggle_state));

         text_color_property := (if active then TEXT_COLOR_PRESSED
                                 else get_property_by_state (raylib.ui.TEXT, toggle_state));

         background := get_color (get_style (TOGGLE, bg_color_property));
         outline    := get_color (get_style (TOGGLE, color_property));
         text_color := get_color (get_style (TOGGLE, text_color_property));
         shapes.draw_rectangle_lines_ex (
            bounds,
            line_thickness,
            colors.fade (outline, global_alpha));

         shapes.draw_rectangle (
            posX => int (bounds.x) + line_thickness,
            posY => int (bounds.y) + line_thickness,
            width  => int (bounds.width)  - 2 * line_thickness,
            height => int (bounds.height) - 2 * line_thickness,
            c => colors.fade (background, global_alpha));

         draw_text (
            text,
            bounds,
            Text_Alignment_Type'Val (get_style (TOGGLE, TEXT_ALIGNMENT)),
            colors.fade (text_color, global_alpha));
      end drawing;
   end toggle;

   function textbox (
      bounds    : in Rectangle;
      text      : in out String;
      edit_mode : in Boolean)
      return Boolean
   is
      use type int;

      state : Control_State := global_state;
      mouse_point : Vector2 := input.get_mouse_position;
      mouse_inside : Boolean := Boolean(shapes.check_collision_point_rec (mouse_point, bounds));
      is_pressed : Boolean := Boolean (input.is_mouse_button_pressed (MOUSE_BUTTON_LEFT)) and mouse_inside;

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

      if Boolean (input.is_mouse_button_pressed (MOUSE_BUTTON_LEFT)) and not mouse_inside
         then state := NORMAL;

      elsif edit_mode then
         state := PRESSED;
         is_pressed := True;
         global_frame_counter := global_frame_counter + 1;

         loop
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
      text_color   := get_color (get_style (TEXTBOX, UI.TEXT, state));
      border_color := get_color (get_style (TEXTBOX, BORDER,  state));
      bg_color     := get_color (get_style (TEXTBOX, BASE,    state));
      border_width := int (get_style (TEXTBOX, UI.BORDER_WIDTH));

      -- Draw border and background
      draw_rectangle (
         bounds,
         border_width,
         fade (border_color, global_alpha),
         fade (bg_color, global_alpha));

      --  Draw blinking cursor
      if edit_mode and (global_frame_counter / 20) mod 2 = 0 then
         declare
            cursor : Rectangle;
            text_width : int := get_text_width (transform (text));
         begin
            cursor.x := get_text_bounds (TEXTBOX, bounds).x + Float (text_width) + 2.0; -- 2px cursor offset is an approximation for text spacing
            cursor.y := bounds.y + Float (get_style (TEXTBOX, TEXT_INNER_PADDING));
            cursor.width := 1.5;
            cursor.height := Float (int (get_style (DEFAULT, TEXT_SIZE)) * 2);

            draw_rectangle (
               cursor,
               0,
                 text_color,
                 fade (text_color, global_alpha));
         end;
      end if;

      --  Draw input text
      draw_text (
         transform (text),
         get_text_bounds (TEXTBOX, bounds),
         to_text_alignment (get_style (TEXTBOX, TEXT_ALIGNMENT)),
         colors.fade (text_color, global_alpha));

      -- TODO: Would be better to return the cursor position
      -- as edit_mode is an in out parameter
      return is_pressed;
   end textbox;

   function textbox_multi (
      bounds : Rectangle;
      text : in out String;
      edit_mode : Boolean)
      return Boolean
   is
      state : Control_State := global_state;
      ispressed : Boolean := False;

      textAreaBounds : Rectangle := (
         bounds.x + Float (get_style (TEXTBOX, TEXT_INNER_PADDING)),
         bounds.y + Float (get_style (TEXTBOX, TEXT_INNER_PADDING)),
         bounds.width  - 2.0 * Float (get_style (TEXTBOX, TEXT_INNER_PADDING)),
         bounds.height - 2.0 * Float (get_style (TEXTBOX, TEXT_INNER_PADDING))
      );
   begin
      return ispressed;
   end textbox_multi;

   procedure set_style (
      control : Controls;
      property : Properties;
      value : unsigned) is
   begin
      global_style (control, property) := value;
   end set_style;

   function get_style (control : Controls; property : Properties)
      return unsigned is
   begin
      if not global_style_loaded then
         load_style_default;
      end if;

      return global_style (control, property);
   end get_style;

   function get_style (
      control : Controls;
      element : Property_Element;
      state : Control_State)
      return unsigned is
   begin
      return get_style (control, get_property_by_state (element, state));
   end get_style;

   procedure load_style_default is
      blank_color : unsigned;
   begin
      blank_color := unsigned (colors.color_to_int (BLANK));

      set_style (DEFAULT, BORDER_COLOR_NORMAL,  16#838383ff#);
      set_style (DEFAULT, BASE_COLOR_NORMAL,    16#c9c9c9ff#);
      set_style (DEFAULT, TEXT_COLOR_NORMAL,    16#686868ff#);
      set_style (DEFAULT, BORDER_COLOR_FOCUSED, 16#5bb2d9ff#);
      set_style (DEFAULT, BASE_COLOR_FOCUSED,   16#c9effeff#);
      set_style (DEFAULT, TEXT_COLOR_FOCUSED,   16#6c9bbcff#);
      set_style (DEFAULT, BORDER_COLOR_PRESSED, 16#0492c7ff#);
      set_style (DEFAULT, BASE_COLOR_PRESSED,   16#97e8ffff#);
      set_style (DEFAULT, TEXT_COLOR_PRESSED,   16#368bafff#);
      set_style (DEFAULT, BORDER_COLOR_DISABLED, 16#b5c1c2ff#);
      set_style (DEFAULT, BASE_COLOR_DISABLED,  16#e6e9e9ff#);
      set_style (DEFAULT, TEXT_COLOR_DISABLED,  16#aeb7b8ff#);
      set_style (DEFAULT, BORDER_WIDTH, 1);
      set_style (DEFAULT, TEXT_PADDING, 0);
      set_style (DEFAULT, TEXT_ALIGNMENT, to_unsigned (TEXT_ALIGN_CENTER));

      for Ctrl in LABEL .. Controls'Last loop
         for Property in Properties'Range loop
            global_style (Ctrl, Property) := global_style (DEFAULT, Property);
         end loop;
      end loop;

      --  Default extended properties
      set_style (DEFAULT, TEXT_SIZE, 14);
      set_style (DEFAULT, TEXT_SPACING, 1);
      set_style (DEFAULT, BACKGROUND_COLOR, 16#f5f5f5ff#);
      set_style (DEFAULT, LINE_COLOR, 16#90abb5ff#);

      set_style (LABEL,    TEXT_ALIGNMENT, to_unsigned (TEXT_ALIGN_LEFT));
      set_style (BUTTON,   TEXT_ALIGNMENT, to_unsigned (TEXT_ALIGN_CENTER));
      set_style (BUTTON,   BORDER_WIDTH, 2);
      set_style (SLIDER,   TEXT_PADDING, 5);
      set_style (SLIDER,   SLIDER_WIDTH, 15);
      set_style (SLIDER,   SLIDER_PADDING, 1);
      set_style (CHECKBOX, TEXT_PADDING, 5);
      set_style (CHECKBOX, TEXT_ALIGNMENT, to_unsigned (TEXT_ALIGN_RIGHT));
      set_style (CHECKBOX, CHECK_PADDING, 1);
      set_style (CHECKBOX, BORDER_WIDTH, 1);
      set_style (TOGGLE,   GROUP_PADDING, 2);
      set_style (TEXTBOX,  TEXT_PADDING, 5);
      set_style (TEXTBOX,  TEXT_ALIGNMENT, to_unsigned (TEXT_ALIGN_LEFT));
      set_style (TEXTBOX,  TEXT_LINES_PADDING, 5);
      set_style (TEXTBOX,  TEXT_INNER_PADDING, 4);
      set_style (TEXTBOX,  COLOR_SELECTED_FG, 16#f0fffeff#);
      set_style (TEXTBOX,  COLOR_SELECTED_BG, 16#839affe0#);
      set_style (TEXTBOX,  BASE_COLOR_PRESSED, blank_color);

      global_font := raylib.text.get_font_default;
      global_style_loaded := True;
   end load_style_default;

end raylib.UI;
