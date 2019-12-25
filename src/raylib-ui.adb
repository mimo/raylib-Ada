
package body raylib.UI is

   NUM_CONTROLS : constant := 6;
   NUM_PROPS_DEFAULT  : constant := 16;
   NUM_PROPS_EXTENDED : constant := 8;  -- Number of extended properties

   global_state : Control_State := NORMAL;
   global_locked : Boolean := False;
   global_alpha : Float := 1.0;
   global_style_loaded : Boolean := False;
   global_style : array (Controls'Range, 0 .. 23) of unsigned;
   global_font : Font;

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

         --  #if defined(RAYGUI_SUPPORT_ICONS)
         if iconId >= 0 then
            --  NOTE: We consider icon height,probably different than text size
            --  GuiDrawIcon(iconId, RAYGUI_CLITERAL(Vector2){ position.x, bounds.y + bounds.height/2 - RICON_SIZE/2 + TEXT_VALIGN_PIXEL_OFFSET(bounds.height) }, 1, tint);
            --  position.x += (RICON_SIZE + ICON_TEXT_PADDING);
            null;
         end if;
         --  #endif

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

   function get_property_index (P : Properties) return Integer is
   begin
      return (case P is
         when BORDER_COLOR_NORMAL  => 0,
         when BASE_COLOR_NORMAL    => 1,
         when TEXT_COLOR_NORMAL    => 2,
         when BORDER_COLOR_FOCUSED => 3,
         when BASE_COLOR_FOCUSED   => 4,
         when TEXT_COLOR_FOCUSED   => 5,
         when BORDER_COLOR_PRESSED => 6,
         when BASE_COLOR_PRESSED   => 7,
         when TEXT_COLOR_PRESSED   => 8,
         when BORDER_COLOR_DISABLED => 9,
         when BASE_COLOR_DISABLED   => 10,
         when TEXT_COLOR_DISABLED   => 11,
         when BORDER_WIDTH    => 12,
         when TEXT_PADDING    => 13,
         when TEXT_ALIGNMENT => 14,
         when RESERVED        => 15,
         when TEXT_SIZE       => 16,
         when TEXT_SPACING    => 17,
         when LINE_COLOR      => 18,
         when BACKGROUND_COLOR => 19,
         when GROUP_PADDING   => 16,
         when SLIDER_WIDTH    => 16,
         when SLIDER_PADDING  => 17,
         when PROGRESS_PADDING     => 16,
         when CHECK_PADDING        => 16,
         when COMBO_BUTTON_WIDTH   => 16,
         when COMBO_BUTTON_PADDING => 17);
   end get_property_index;

   --  Get text bounds considering control bounds
   function get_text_bounds (control : Controls; bounds : Rectangle)
      return Rectangle
   is
      CONTROL_BORDER_WIDTH : Float;
      textBounds : Rectangle := bounds;
   begin
      CONTROL_BORDER_WIDTH := Float (get_style (control, BORDER_WIDTH));
      textBounds.x := bounds.x + CONTROL_BORDER_WIDTH;
      textBounds.y := bounds.y + CONTROL_BORDER_WIDTH;
      textBounds.width  := bounds.width  - 2.0 * CONTROL_BORDER_WIDTH;
      textBounds.height := bounds.height - 2.0 * CONTROL_BORDER_WIDTH;

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
         if Text_Alignment_Type'Val (get_style (control, TEXT_ALIGNMENT)) = TEXT_ALIGN_RIGHT
         then textBounds.x := textBounds.x
                              - Float (get_style (control, TEXT_PADDING));
         else textBounds.x := textBounds.x
                              + Float (get_style (control, TEXT_PADDING));
         end if;
      end case;

      --  TODO: Special cases (no label):
      --    COMBOBOX, DROPDOWNBOX, LISTVIEW (scrollbar?)
      --  More special cases (label side):
      --    CHECKBOX, SLIDER, VALUEBOX, SPINNER

      return textBounds;
   end get_text_bounds;

   --  Gui get text width using default font
   function get_text_width (text : String) return int is
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

   function button (bounds : raylib.Rectangle; label : String)
      return Boolean
   is
      button_state : Control_State := global_state;
      pressed : Boolean := False;
      mouse_point : Vector2;
      style_border_width : constant unsigned := get_style (BUTTON, BORDER_WIDTH);
   begin
      if global_state /= DISABLED and not global_locked then
         mouse_point := raylib.core.get_mouse_position;
         --  Check button state
         if raylib.shapes.check_collision_point_rec (mouse_point, bounds) then
            --
            if raylib.core.is_mouse_button_down (raylib.MOUSE_LEFT_BUTTON)
            then button_state := raylib.UI.PRESSED;
            else button_state := FOCUSED;
            end if;

            if raylib.core.is_mouse_button_released (raylib.MOUSE_LEFT_BUTTON)
            then
               pressed := True;
            end if;
         end if;
      end if;

      --  Draw control
      --------------------------------------------------------------------
      declare
         border_property_index : Integer := property_element'Pos (BORDER)
                                            + control_state'Pos (button_state) * 3;
         border_property : Properties := Properties'Val (border_property_index);

         fill_property_index : integer := property_element'pos(BASE)
                                          + (control_state'pos(button_state) * 3);
         fill_property : Properties := Properties'Val (fill_property_index);

         style_bordercolor : Color := colors.fade (raylib.colors.get_color (
                                            get_style (DEFAULT,
                                            border_property)),
                                            global_alpha);

         style_fillcolor : Color := colors.fade (raylib.colors.get_color (
                                          get_style (DEFAULT, fill_property)),
                                          global_alpha);

         text_color_index : Integer := Property_Element'Pos (TEXT)
                                       + Control_State'Pos (button_state) * 3;
         text_color_property : Properties := Properties'Val (text_color_index);
         text_color_val : unsigned := get_style (BUTTON, text_color_property);

         text_color : Color := raylib.colors.get_color (text_color_val);

         use type unsigned;
         use type int;
      begin
         raylib.shapes.draw_rectangle_lines_ex (
            bounds,
            int (style_border_width),
            style_bordercolor);

         raylib.shapes.draw_rectangle (
            int (bounds.x) + int (style_border_width),
            int (bounds.y) + int (style_border_width),
            int (bounds.width)  - int (2 * style_border_width),
            int (bounds.height) - int (2 * style_border_width),
            style_fillcolor);

         draw_text (
            label,
            get_text_bounds (BUTTON, bounds),
            Text_Alignment_Type'Val (get_style (BUTTON, TEXT_ALIGNMENT)),
            raylib.colors.fade (text_color, global_alpha));
      end;
      ------------------------------------------------------------------
      return pressed;
   end button;

   procedure panel (bounds : Rectangle) is
      PANEL_BORDER_WIDTH : constant := 1;
      panel_state : constant Control_State := global_state;
      BG_ELEMENT, BORDER_ELEMENT : Properties;
   begin
      BG_ELEMENT := (if panel_state = DISABLED
                     then BASE_COLOR_DISABLED
                     else BACKGROUND_COLOR);
      BORDER_ELEMENT := (if panel_state = DISABLED
                         then BORDER_COLOR_DISABLED
                         else LINE_COLOR);
      --  Draw control
      -------------------------------------------------------------------------
      shapes.draw_rectangle_rec (
            bounds,
            colors.fade (
               colors.get_color (get_style (DEFAULT, BG_ELEMENT)),
               global_alpha));
      shapes.draw_rectangle_lines_ex (
         bounds, PANEL_BORDER_WIDTH,
         colors.fade (
            colors.get_color (get_style (DEFAULT, BORDER_ELEMENT)),
            global_alpha));
      -------------------------------------------------------------------------
   end panel;
   
   function toggle (bounds : Rectangle ; text : String ; active : Boolean) return Boolean is
      toggle_state : Control_State := global_state;
      mouse_point : Vector2;
      is_active : Boolean := active;
   begin
      if global_state /= DISABLED and not global_locked then
         mouse_point := core.get_mouse_position;
         
         if shapes.check_collision_point_rec (mouse_point, bounds) then
         
            if core.is_mouse_button_down (MOUSE_LEFT_BUTTON) then
               toggle_state := PRESSED;
            elsif core.is_mouse_button_released (MOUSE_LEFT_BUTTON) then
               toggle_state := NORMAL;
               is_active := not active;
            else
               toggle_state := FOCUSED;
            end if;
         end if;
      end if;
      
      drawing : declare
         line_thickness : int := int (get_style (TOGGLE, BORDER_WIDTH));
         color_property : Properties;
         color_prop_index : Integer;
         bg_color_property : Properties;
         bg_color_prop_index : Integer;
         text_color_property : Properties;
         text_color_prop_index : Integer;
         outline, background, text_color : Color;

         use type int;
      begin
         color_prop_index      := Property_Element'Pos (BORDER) + Control_State'Pos (toggle_state) * 3;
         bg_color_prop_index   := Property_Element'Pos (BASE)   + Control_State'Pos (toggle_state) * 3;
         text_color_prop_index := Property_Element'Pos (raylib.ui.TEXT) + Control_State'Pos (toggle_state) * 3;
         
         if toggle_state = NORMAL then
            color_property := (if is_active then BORDER_COLOR_PRESSED
                                            else Properties'Val (color_prop_index));
            bg_color_property := (if is_active then BASE_COLOR_PRESSED
                                               else Properties'Val (bg_color_prop_index));
            text_color_property := (if is_active then TEXT_COLOR_PRESSED
                                                 else Properties'Val (text_color_prop_index));
         else
            color_property := Properties'Val (color_prop_index);
            bg_color_property := Properties'Val (bg_color_prop_index);
            text_color_property := Properties'Val (text_color_prop_index);
         end if;

         background := colors.get_color (get_style (TOGGLE, bg_color_property));
         outline    := colors.get_color (get_style (TOGGLE, color_property));
         text_color := colors.get_color (get_style (TOGGLE, text_color_property));
         shapes.draw_rectangle_lines_ex (bounds, line_thickness, colors.fade (outline, global_alpha));
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
      
      return is_active;
   end toggle;

   procedure set_style (
      control : Controls;
      property : Properties;
      value : unsigned) is
   begin
      global_style (control, get_property_index (property)) := value;
   end set_style;

   function get_style (control : Controls; property : Properties)
      return unsigned is
      ctrl : Controls;
   begin
      if not global_style_loaded then
         load_style_default;
      end if;
      
      if control = TOGGLE
      then ctrl := DEFAULT;
      end if;
      
      return global_style (ctrl, get_property_index (property));
   end get_style;

   procedure load_style_default is
   begin
      set_style (DEFAULT, BORDER_COLOR_NORMAL,  16#838383ff#);
      set_style (DEFAULT, BASE_COLOR_NORMAL,    16#c9c9c9ff#);
      set_style (DEFAULT, TEXT_COLOR_NORMAL,    16#686868ff#);
      set_style (DEFAULT, BORDER_COLOR_FOCUSED, 16#5bb2d9ff#);
      set_style (DEFAULT, BASE_COLOR_FOCUSED,   16#c9effeff#);
      set_style (DEFAULT, TEXT_COLOR_FOCUSED,   16#6c9bbcff#);
      set_style (DEFAULT, BORDER_COLOR_PRESSED, 16#0492c7ff#);
      set_style (DEFAULT, BASE_COLOR_PRESSED,   16#97e8ffff#);
      set_style (DEFAULT, TEXT_COLOR_PRESSED,   16#368bafff#);
      set_style (DEFAULT, BORDER_WIDTH, 1);
      set_style (DEFAULT, TEXT_PADDING, 0);
      set_style (
         DEFAULT,
         TEXT_ALIGNMENT,
         Text_Alignment_Type'Pos (TEXT_ALIGN_CENTER));

      --  Initialize control-specific property values
      --  NOTE: Those properties are in default list but require specific values by control type
      set_style (LABEL,  TEXT_ALIGNMENT,
                 Text_Alignment_Type'Pos (TEXT_ALIGN_LEFT));
      set_style (BUTTON, TEXT_ALIGNMENT,
                 Text_Alignment_Type'Pos (TEXT_ALIGN_CENTER));
      set_style (BUTTON, BORDER_WIDTH, 2);

      set_style (DEFAULT, TEXT_SIZE, 10);
      set_style (DEFAULT, TEXT_SPACING, 1);
      set_style (DEFAULT, BACKGROUND_COLOR, 16#f5f5f5ff#);
      
      --  Initialize extended property values
      set_style (DEFAULT, TEXT_SIZE, 10);
      set_style (DEFAULT, TEXT_SPACING, 1);
      set_style (DEFAULT, LINE_COLOR, 16#90abb5ff#);
      set_style (DEFAULT, BACKGROUND_COLOR, 16#f5f5f5ff#);
      set_style (TOGGLE, GROUP_PADDING, 2);
      
      global_font := raylib.text.get_font_default;
      global_style_loaded := TRUE;
   end load_style_default;

end raylib.UI;
