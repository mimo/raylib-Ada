with raylib;

package body raylib.UI is

   NUM_CONTROLS : constant := 6;
   NUM_PROPS_DEFAULT : constant := 2;
   --#define NUM_PROPS_EXTENDED               8      // Number of extended properties

   type Control_State is (NORMAL, FOCUSED, PRESSED, DISABLED);
   global_state : Control_State := NORMAL;
   global_locked : Boolean := FALSE;
   global_alpha : float := 1.0;
   global_style_loaded : Boolean := FALSE;
   global_style : array (Controls'range, Properties'range) of unsigned;

   ---
   --
   procedure draw_text (text : String) is
   begin
     null;
   end draw_text;

   ---
   -- UI elements
   function button (bounds : raylib.Rectangle ; label : String) return Boolean is
      state : Control_State := global_state;
      pressed : Boolean := FALSE;
      mouse_point : Vector2;
      -- TODO :
      style_border_width : unsigned := get_style (BUTTON, BORDER_WIDTH);
   begin
      if global_state /= DISABLED and not global_locked then
         mouse_point := raylib.core.get_mouse_position;
         -- Check button state
         if raylib.shapes.check_collision_point_rec (mouse_point, bounds) then
            --
            if raylib.core.is_mouse_button_down (raylib.MOUSE_LEFT_BUTTON)
            then state := raylib.ui.PRESSED;
            else state := FOCUSED;
            end if;

            if raylib.core.is_mouse_button_released (raylib.MOUSE_LEFT_BUTTON) then
              pressed := TRUE;
            end if;
         end if;
      end if;

      -- Draw control
      --------------------------------------------------------------------
      declare
        border_property_index : integer := property_element'pos(BORDER) + (control_state'pos(state) * 3);
        border_property : Properties := Properties'val (border_property_index);
        fill_property_index : integer := property_element'pos(BASE) + (control_state'pos(state) * 3);
        fill_property : Properties := Properties'val (fill_property_index);

        style_bordercolor : Color := fade(raylib.colors.get_color(get_style (DEFAULT, border_property)), global_alpha);
        style_fillcolor  : Color := fade(raylib.colors.get_color(get_style (DEFAULT, fill_property)), global_alpha);
      begin
        raylib.shapes.draw_rectangle_lines_ex (bounds, int(style_border_width), style_bordercolor);
        raylib.shapes.draw_rectangle (int(bounds.x) + int(style_border_width), int(bounds.y) + int(style_border_width), int(bounds.width) - int(2*style_border_width), int(bounds.height) - int(2*style_border_width), style_fillcolor);
        --GuiDrawText(text, GetTextBounds(BUTTON, bounds), GuiGetStyle(BUTTON, TEXT_ALIGNMENT), Fade(GetColor(GuiGetStyle(BUTTON, TEXT + (state*3))), guiAlpha));
      end;
      ------------------------------------------------------------------
      return pressed;
   end button;

   procedure set_style (control : Controls ; property : Properties; value : unsigned) is
   begin
     global_style(control, property) := value;

   end set_style;

   function get_style (control : Controls ; property : Properties) return unsigned is
   begin
      if not global_style_loaded then
         load_style_default;
       end if;
      return global_style(control, property);
   end get_style;

   procedure load_style_default is
   begin
      set_style (DEFAULT, BORDER_COLOR_NORMAL, 16#838383ff#);
      set_style (DEFAULT, BASE_COLOR_NORMAL,   16#c9c9c9ff#);
      set_style (DEFAULT, TEXT_COLOR_NORMAL,   16#686868ff#);
      set_style (DEFAULT, BORDER_COLOR_FOCUSED, 16#5bb2d9ff#);
      set_style (DEFAULT, BASE_COLOR_FOCUSED, 16#c9effeff#);
      set_style (DEFAULT, TEXT_COLOR_FOCUSED, 16#6c9bbcff#);
      set_style (DEFAULT, BORDER_COLOR_PRESSED, 16#0492c7ff#);
      set_style (DEFAULT, BASE_COLOR_PRESSED, 16#97e8ffff#);
      set_style (DEFAULT, TEXT_COLOR_PRESSED, 16#368bafff#);

      -- Initialize control-specific property values
      -- NOTE: Those properties are in default list but require specific values by control type
      --set_style (LABEL, TEXT_ALIGNMENT, GUI_TEXT_ALIGN_LEFT);
      set_style (BUTTON, BORDER_WIDTH, 2);

      global_style_loaded := TRUE;
   end load_style_default;

end raylib.UI;
