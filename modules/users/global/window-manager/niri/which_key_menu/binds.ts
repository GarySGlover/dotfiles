import { Action, MenuAction } from "./menu";

function niri(action: string) {
  return Action.exec(`niri msg action ${action}`);
}

export const apps: MenuAction[] = [
  { key: "b", label: "Brave", action: Action.exec("brave") },
  {
    key: "B",
    label: "Bluetuith",
    action: Action.exec("kitty bluetuith"),
  },
  {
    key: "c",
    label: "Chromium",
    action: Action.exec("chromium"),
  },
  {
    key: "e",
    label: "Emacs",
    action: Action.exec(
      "emacsclient --create-frame -no-wait --alternate-editor 'emacs'",
    ),
  },
  {
    key: "f",
    label: "Firefox",
    action: Action.exec("firefox"),
  },
  {
    key: "l",
    label: "Lock",
    action: Action.exec("hyprlock"),
  },
  { key: "n", label: "Nyxt", action: Action.exec("nyxt") },
  { key: "s", label: "Steam", action: Action.exec("steam") },
  {
    key: "t",
    label: "Terminal",
    action: Action.exec("kitty"),
    keepOpen: false,
  },
  {
    key: "w",
    label: "Work Browser",
    action: Action.exec("floorp"),
  },
];

export const main: MenuAction[] = [
  {
    key: "a",
    label: "Apps →",
    action: Action.set_menu(apps),
    keepOpen: true,
  },
  {
    key: "f",
    label: "Focus Column →",
    action: niri("focus-column-right-or-first"),
    keepOpen: true,
  },
  {
    key: "F",
    label: "Focus Monitor →",
    action: niri("focus-monitor-right"),
    keepOpen: true,
  },
  {
    key: "Ctrl+f",
    label: "Move Column →",
    action: niri("move-column-right"),
    keepOpen: true,
  },
  {
    key: "Ctrl+F",
    label: "Move Column ⇨ Monitor →",
    action: niri("move-column-to-monitor-right"),
    keepOpen: true,
  },
  {
    key: "Alt+f",
    label: "Move Workspace ⇨ Monitor →",
    action: niri("move-workspace-to-monitor-right"),
    keepOpen: true,
  },
  {
    key: "b",
    label: "Focus Column ←",
    action: niri("focus-column-left-or-last"),
    keepOpen: true,
  },
  {
    key: "B",
    label: "Focus Monitor ←",
    action: niri("focus-monitor-left"),
    keepOpen: true,
  },
  {
    key: "Ctrl+b",
    label: "Move Column ←",
    action: niri("move-column-left"),
    keepOpen: true,
  },
  {
    key: "Ctrl+B",
    label: "Move Column ⇨ Monitor ←",
    action: niri("move-column-to-monitor-left"),
    keepOpen: true,
  },
  {
    key: "Alt+b",
    label: "Move Workspace ⇨ Monitor ←",
    action: niri("move-workspace-to-monitor-left"),
    keepOpen: true,
  },
  {
    key: "p",
    label: "Focus Workspace ↑",
    action: niri("focus-workspace-up"),
    keepOpen: true,
  },
  {
    key: "P",
    label: "Focus Monitor ↑",
    action: niri("focus-monitor-up"),
    keepOpen: true,
  },
  {
    key: "Ctrl+p",
    label: "Move Column ⇧ Workspace ↑",
    action: niri("move-column-to-workspace-up"),
    keepOpen: true,
  },
  {
    key: "Ctrl+P",
    label: "Move Column ⇨ Monitor ↑",
    action: niri("move-column-to-monitor-up"),
    keepOpen: true,
  },
  {
    key: "Alt+p",
    label: "Move Workspace ⇨ Monitor ↑",
    action: niri("move-workspace-to-monitor-up"),
    keepOpen: true,
  },
  {
    key: "n",
    label: "Focus Workspace ↓",
    action: niri("focus-workspace-down"),
    keepOpen: true,
  },
  {
    key: "N",
    label: "Focus Monitor ↓",
    action: niri("focus-monitor-down"),
    keepOpen: true,
  },
  {
    key: "Ctrl+n",
    label: "Move Column ⇧ Workspace ↓",
    action: niri("move-column-to-workspace-down"),
    keepOpen: true,
  },
  {
    key: "Ctrl+N",
    label: "Move Column ⇨ Monitor ↓",
    action: niri("move-column-to-monitor-down"),
    keepOpen: true,
  },
  {
    key: "Alt+n",
    label: "Move Workspace ⇨ Monitor ↓",
    action: niri("move-workspace-to-monitor-down"),
    keepOpen: true,
  },
  {
    key: "v",
    label: "Move Workspace ↑",
    action: niri("move-workspace-up"),
    keepOpen: true,
  },
  {
    key: "V",
    label: "Move Workspace ↓",
    action: niri("move-workspace-down"),
    keepOpen: true,
  },
  {
    key: "e",
    label: "Toggle Focus ↔ Floating",
    action: niri("switch-focus-between-floating-and-tiling"),
    keepOpen: true,
  },
  {
    key: "E",
    label: "Toggle Floating",
    action: niri("toggle-window-floating"),
    keepOpen: true,
  },
  {
    key: "s",
    label: "📸 Screenshot Window",
    action: niri("screenshot-window"),
  },
  {
    key: "S",
    label: "📸 Screenshot Screen",
    action: niri("screenshot-screen"),
  },
  {
    key: "Ctrl+s",
    label: "📸 Screenshot",
    action: niri("screenshot"),
  },
  {
    key: "c",
    label: "📺 Cast Window",
    action: niri("set-dynamic-cast-window"),
  },
  {
    key: "C",
    label: "📺 Cast Monitor",
    action: niri("set-dynamic-cast-monitor"),
  },
  {
    key: "Ctrl+C",
    label: "❌ Clear Cast",
    action: niri("clear-dynamic-cast-target"),
  },
  {
    key: "x",
    label: "↔ Switch Column Width",
    action: niri("switch-preset-column-width"),
    keepOpen: true,
  },
  {
    key: "X",
    label: "⬌ Expand Column",
    action: niri("expand-column-to-available-width"),
    keepOpen: true,
  },
  {
    key: "Ctrl+x",
    label: "⬛ Maximize Column",
    action: niri("maximize-column"),
    keepOpen: true,
  },
  {
    key: "m",
    label: "⬛ Maximize Column",
    action: niri("maximize-column"),
    keepOpen: true,
  },
  {
    key: "M",
    label: "⛶ Fullscreen",
    action: niri("fullscreen-window"),
    keepOpen: true,
  },
  {
    key: "Ctrl+m",
    label: "⛶ Fake Fullscreen",
    action: niri("toggle-windowed-fullscreen"),
    keepOpen: true,
  },
  {
    key: "c",
    label: "🧲 Center Column",
    action: niri("center-column"),
  },
  {
    key: "C",
    label: "🧲 Center Visible",
    action: niri("center-visible-columns"),
  },
  {
    key: "w",
    label: "❌ Close Window",
    action: niri("close-window"),
  },
  { key: "W", label: "💣 Quit", action: niri("quit") },
];
