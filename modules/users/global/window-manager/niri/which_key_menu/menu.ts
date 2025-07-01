// src/keybindingMenu.ts — Gtk-native, multi-monitor-aware Astal menu
import { App, Astal, Gtk, Gdk, Widget } from "astal/gtk4";
import { execAsync } from "astal/process";
import { Variable } from "astal";

export type MenuAction = {
  key: string;
  label: string;
  action: () => void;
  keepOpen?: boolean;
};

export const currentMap = Variable<MenuAction[]>([]);
export var Menus = Variable<Astal.Window[]>(
  App.get_monitors().map((monitor, i) => Menu(monitor, i)),
);

export class Action {
  public static set_menu(map: MenuAction[]) {
    return () => currentMap.set(map);
  }
  public static exec(cmd: string | string[]) {
    return () => execAsync(cmd);
  }
  public static toggle_menus() {
    return () => {
      const anyVisible = Menus.get().some((win) => win.is_visible());
      if (anyVisible) {
        this.hide_menus()();
      } else {
        this.show_menus()();
      }
    };
  }
  public static show_menus() {
    return () => {
      Menus.set(App.get_monitors().map((monitor, i) => Menu(monitor, i)));
      Menus.get().forEach((win) => {
        win.show();
      });
    };
  }
  public static hide_menus() {
    return () =>
      Menus.get().forEach((win) => {
        win.hide();
      });
  }
}

function buildMenuItems(): Gtk.Box {
  const box = new Gtk.Box({
    orientation: Gtk.Orientation.VERTICAL,
    spacing: 6,
  });

  const grid = new Gtk.Grid({ column_spacing: 12, row_spacing: 6 });

  let rowIdx = 0;
  for (const item of currentMap.get()) {
    const keyLabel = new Gtk.Label({
      label: item.key,
      halign: Gtk.Align.END,
      hexpand: true,
      xalign: 1.0,
    });

    const separatorLabel = new Gtk.Label({
      label: "⇒",
      halign: Gtk.Align.CENTER,
      hexpand: false,
    });

    const descLabel = new Gtk.Label({
      label: item.label,
      halign: Gtk.Align.START,
      hexpand: true,
      xalign: 0.0,
    });

    grid.attach(keyLabel, 0, rowIdx, 1, 1);
    grid.attach(separatorLabel, 1, rowIdx, 1, 1);
    grid.attach(descLabel, 2, rowIdx, 1, 1);

    rowIdx += 1;
  }

  box.append(grid);
  return box;
}

export default function Menu(gdkMonitor: Gdk.Monitor, index: number) {
  const container = new Gtk.Box({
    orientation: Gtk.Orientation.VERTICAL,
    spacing: 8,
  });
  container.append(buildMenuItems());

  const content = new Gtk.Revealer({
    reveal_child: true,
    transition_type: Gtk.RevealerTransitionType.SLIDE_DOWN,
    transition_duration: 150,
  });
  content.set_child(container);

  currentMap.subscribe(() => {
    const newItems = buildMenuItems();
    const currentChild = container.get_first_child();
    if (currentChild) container.remove(currentChild);
    container.append(newItems);
    if (window.is_visible()) {
      window.hide();
      window.show();
    }
  });

  function handleKey(keyval: number, state: Gdk.ModifierType) {
    const keyname = Gdk.keyval_name(keyval) ?? "";

    const hasCtrl = (state & Gdk.ModifierType.CONTROL_MASK) !== 0;
    const hasAlt = (state & Gdk.ModifierType.ALT_MASK) !== 0;
    const hasShift = (state & Gdk.ModifierType.SHIFT_MASK) !== 0;

    let keyString = "";
    if (hasCtrl) keyString += "Ctrl+";
    if (hasAlt) keyString += "Alt+";
    if (hasShift && keyname.length === 1 && keyname !== keyname.toUpperCase()) {
      keyString += "Shift+";
    }

    keyString += keyname;

    const item = currentMap.get().find((i) => i.key === keyString);

    if (item) {
      if (!item.keepOpen) Action.hide_menus(Menus.get())();
      item.action();
    } else if (["Escape", "g", "["].includes(keyname)) {
      Action.hide_menus(Menus.get())();
    }
  }

  const { TOP, LEFT, RIGHT, BOTTOM, NONE } = Astal.WindowAnchor;

  const window = Widget.Window({
    name: "keybindingMenu-" + index,
    gdkmonitor: gdkMonitor,
    anchor: NONE,
    keymode: Astal.Keymode.EXCLUSIVE,
    focusable: true,
    layer: Astal.Layer.OVERLAY,
    child: content,
    onKeyPressed: (_, event, __, mod) => {
      handleKey(event, mod);
      return Gdk.EVENT_STOP;
    },
  });

  return window;
}
