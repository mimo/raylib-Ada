
package raylib.UI is

   ---
   --  UI state
   type Controls is (
      DEFAULT,
      LABEL,
      BUTTON,
      TOGGLE,
      SLIDER,
      PROGRESSBAR,
      CHECKBOX,
      COMBOBOX,
      DROPDOWNBOX,
      TEXTBOX,
      VALUEBOX,
      SPINNER,
      LISTVIEW,
      COLORPICKER,
      SCROLLBAR,
      STATUSBAR);

   type Properties is (
      BORDER_COLOR_NORMAL,
      BASE_COLOR_NORMAL,
      TEXT_COLOR_NORMAL,
      BORDER_COLOR_FOCUSED,
      BASE_COLOR_FOCUSED,
      TEXT_COLOR_FOCUSED,
      BORDER_COLOR_PRESSED,
      BASE_COLOR_PRESSED,
      TEXT_COLOR_PRESSED,
      BORDER_COLOR_DISABLED,
      BASE_COLOR_DISABLED,
      TEXT_COLOR_DISABLED,
      BORDER_WIDTH,
      TEXT_PADDING,
      TEXT_ALIGNMENT,
      RESERVED,
      --  Default Properties
      TEXT_SIZE,
      TEXT_SPACING,
      LINE_COLOR,
      BACKROUND_COLOR,
      --  Toggle Properties
      GROUP_PADDING,
      --  Slider / SliderBar
      SLIDER_WIDTH,
      SLIDER_PADDING,
      --  ProgressBar
      PROGRESS_PADDING,
      --  CheckBox
      CHECK_PADDING,
      --  ComboBox
      COMBO_BUTTON_WIDTH,
      COMBO_BUTTON_PADDING);

   procedure set_style (
      control : Controls;
      property : Properties;
      value : unsigned);

   function get_style (control : Controls; property : Properties)
      return unsigned;

   function get_text_width (text : String) return int;

   function button (bounds : raylib.Rectangle; label : String) return Boolean;

private

   function get_property_index (P : Properties) return Integer;

   type Property_Element is (BORDER, BASE, TEXT, OTHER);
   type Control_State is (NORMAL, FOCUSED, PRESSED, DISABLED);

   procedure load_style_default;

   type Text_Alignment_Type is (
      TEXT_ALIGN_LEFT, -- 0
      TEXT_ALIGN_CENTER,
      TEXT_ALIGN_RIGHT);
   for Text_Alignment_Type'Size use unsigned'Size;

   procedure draw_text (
      text : String;
      bounds : Rectangle;
      alignment : Text_Alignment_Type;
      tint : Color);
   function get_text_bounds (control : Controls; bounds : Rectangle)
      return Rectangle;

end raylib.UI;
