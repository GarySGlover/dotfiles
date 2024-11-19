{
  config,
  lib,
  ...
}:
with lib;
let
  removeHashPrefix =
    str:
    if builtins.substring 0 1 str == "#" then
      builtins.substring 1 (builtins.stringLength str - 1) str
    else
      str;
in
{
  config.wolf.theme = mkIf (config.wolf.theme.name == "ef-elea-dark") {
    font = {
      name = "0xProto Nerd Font";
      size = 10;
    };

    border = 2;

    faces =
      with config.wolf.theme;
      builtins.mapAttrs (name: value: removeHashPrefix value) {
        fgAnsiColorBlack = "#000000";
        bgAnsiColorBlack = "#000000";
        fgAnsiColorBlue = "#57aff6";
        bgAnsiColorBlue = "#57aff6";
        fgAnsiColorBrightBlack = "#595959";
        bgAnsiColorBrightBlack = "#595959";
        fgAnsiColorBrightBlue = "#78afff";
        bgAnsiColorBrightBlue = "#78afff";
        fgAnsiColorBrightCyan = "#60d5c2";
        bgAnsiColorBrightCyan = "#60d5c2";
        fgAnsiColorBrightGreen = "#50cf89";
        bgAnsiColorBrightGreen = "#50cf89";
        fgAnsiColorBrightMagenta = "#cfaaff";
        bgAnsiColorBrightMagenta = "#cfaaff";
        fgAnsiColorBrightRed = "#ff7a5f";
        bgAnsiColorBrightRed = "#ff7a5f";
        fgAnsiColorBrightWhite = "#ffffff";
        bgAnsiColorBrightWhite = "#ffffff";
        fgAnsiColorBrightYellow = "#e0b02f";
        bgAnsiColorBrightYellow = "#e0b02f";
        fgAnsiColorCyan = "#7fcfdf";
        bgAnsiColorCyan = "#7fcfdf";
        fgAnsiColorGreen = "#7fc87f";
        bgAnsiColorGreen = "#7fc87f";
        fgAnsiColorMagenta = "#f59acf";
        bgAnsiColorMagenta = "#f59acf";
        fgAnsiColorRed = "#ff656a";
        bgAnsiColorRed = "#ff656a";
        fgAnsiColorWhite = "#a6a6a6";
        bgAnsiColorWhite = "#a6a6a6";
        fgAnsiColorYellow = "#cac85f";
        bgAnsiColorYellow = "#cac85f";
        fgBorder = "#eaf2ef";
        bgBorder = "#222524";
        fgCursor = "#eaf2ef";
        bgCursor = "#ef7fa8";
        fgCustomButtonPressedUnraised = "#ee82ee";
        bgCustomButtonPressedUnraised = "#222524";
        fgCustomButtonUnraised = "#eaf2ef";
        bgCustomButtonUnraised = "#222524";
        fgDefault = "#eaf2ef";
        bgDefault = "#222524";
        fgFontLockBracketFace = "#eaf2ef";
        bgFontLockBracketFace = "#222524";
        fgFontLockBuiltinFace = "#d0b9f0";
        bgFontLockBuiltinFace = "#222524";
        fgFontLockCommentDelimiterFace = "#cac89f";
        bgFontLockCommentDelimiterFace = "#222524";
        fgFontLockCommentFace = "#cac89f";
        bgFontLockCommentFace = "#222524";
        fgFontLockConstantFace = "#cfaaff";
        bgFontLockConstantFace = "#222524";
        fgFontLockDelimiterFace = "#eaf2ef";
        bgFontLockDelimiterFace = "#222524";
        fgFontLockDocFace = "#99bfcf";
        bgFontLockDocFace = "#222524";
        fgFontLockDocMarkupFace = "#cfaaff";
        bgFontLockDocMarkupFace = "#222524";
        fgFontLockEscapeFace = "#cfaaff";
        bgFontLockEscapeFace = "#222524";
        fgFontLockFunctionCallFace = "#7fca5a";
        bgFontLockFunctionCallFace = "#222524";
        fgFontLockFunctionNameFace = "#7fca5a";
        bgFontLockFunctionNameFace = "#222524";
        fgFontLockKeywordFace = "#eba8a8";
        bgFontLockKeywordFace = "#222524";
        fgFontLockMiscPunctuationFace = "#eaf2ef";
        bgFontLockMiscPunctuationFace = "#222524";
        fgFontLockNegationCharFace = "#eaf2ef";
        bgFontLockNegationCharFace = "#222524";
        fgFontLockNumberFace = "#eaf2ef";
        bgFontLockNumberFace = "#222524";
        fgFontLockOperatorFace = "#eaf2ef";
        bgFontLockOperatorFace = "#222524";
        fgFontLockPreprocessorFace = "#fa90ea";
        bgFontLockPreprocessorFace = "#222524";
        fgFontLockPropertyNameFace = "#f59acf";
        bgFontLockPropertyNameFace = "#222524";
        fgFontLockPropertyUseFace = "#f59acf";
        bgFontLockPropertyUseFace = "#222524";
        fgFontLockPunctuationFace = "#eaf2ef";
        bgFontLockPunctuationFace = "#222524";
        fgFontLockRegexpFace = "#50cf89";
        bgFontLockRegexpFace = "#222524";
        fgFontLockRegexpGroupingBackslash = "#cfaaff";
        bgFontLockRegexpGroupingBackslash = "#222524";
        fgFontLockRegexpGroupingConstruct = "#ff656a";
        bgFontLockRegexpGroupingConstruct = "#222524";
        fgFontLockStringFace = "#50cf89";
        bgFontLockStringFace = "#222524";
        fgFontLockTypeFace = "#6fcfd2";
        bgFontLockTypeFace = "#222524";
        fgFontLockVariableNameFace = "#f59acf";
        bgFontLockVariableNameFace = "#222524";
        fgFontLockVariableUseFace = "#f59acf";
        bgFontLockVariableUseFace = "#222524";
        fgFontLockWarningFace = "#e0b02f";
        bgFontLockWarningFace = "#222524";
        fgHighlight = "#ffffff";
        bgHighlight = "#894f7a";
        fgLink = "#7fca5a";
        bgLink = "#222524";
        fgMinibufferPrompt = "#cfaaff";
        bgMinibufferPrompt = "#222524";
        fgOrderlessMatchFace0 = "#50cf89";
        bgOrderlessMatchFace0 = "#222524";
        fgRegion = "#eaf2ef";
        bgRegion = "#543040";
        fgShadow = "#969faf";
        bgShadow = "#222524";
        fgTabBarTab = "#eaf2ef";
        bgTabBarTab = "#222524";
        fgTabBarTabInactive = "#eaf2ef";
        bgTabBarTabInactive = "#5e6160";
        fgVerticalBorder = "#5d5f63";
        bgVerticalBorder = "#222524";
        fgWarning = "#e0b02f";
        bgWarning = "#222524";
      }
      // {
        fgBorderInactive = faces.fgVerticalBorder;
        bgBorderInactive = faces.bgVerticalBorder;
        fgUnset = "#ff1493";
        bgUnset = "#ff1493";
      };

    dracula = with config.wolf.theme.faces; {
      background = bgDefault;
      foreground = fgDefault;
      currentLine = bgHighlight;
      comment = fgFontLockCommentFace;
      cyan = fgAnsiColorCyan;
      green = fgAnsiColorGreen;
      orange = fgWarning;
      pink = fgAnsiColorMagenta;
      purple = fgAnsiColorBlue;
      red = fgAnsiColorRed;
      yellow = fgAnsiColorYellow;
    };
  };
}
