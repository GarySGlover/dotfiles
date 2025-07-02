import { Astal, Gtk, Gdk, Widget } from "astal/gtk4";
import { execAsync } from "astal/process";
import { Variable } from "astal";

export namespace Menu {
  export type Action = {
    key: string;
    label: string;
    action: () => void;
    keepOpen?: boolean;
  };

  export const map = Variable<Action[]>([]);
  export const window = Variable<Astal.Window | null>(null);

  export function exec(cmd: string | string[]) {
    return () => execAsync(cmd);
  }

  function buildMenuItems(): Gtk.Box {
    const box = new Gtk.Box({
      orientation: Gtk.Orientation.VERTICAL,
      spacing: 6,
    });

    const grid = new Gtk.Grid({ column_spacing: 12, row_spacing: 6 });

    let rowIdx = 0;
    for (const item of map.get()) {
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

  let container: Gtk.Box | null = null;
  let content: Gtk.Revealer | null = null;

  export function createOrUpdateMenuWindow(gdkMonitor: Gdk.Monitor) {
    if (window.get()) {
      // Window already exists; update monitor and items.
      window.get()!.gdkmonitor = gdkMonitor;
      forceRefreshMenuItems();
      return;
    }

    container = new Gtk.Box({
      orientation: Gtk.Orientation.VERTICAL,
      spacing: 8,
    });
    container.append(buildMenuItems());

    content = new Gtk.Revealer({
      reveal_child: true,
      transition_type: Gtk.RevealerTransitionType.SLIDE_DOWN,
      transition_duration: 150,
    });
    content.set_child(container);

    map.subscribe(() => {
      forceRefreshMenuItems();
      if (window.get() && window.get()!.is_visible()) {
        window.get()!.hide();
        window.get()!.show();
      }
    });

    const { NONE } = Astal.WindowAnchor;

    const win = Widget.Window({
      name: "keybindingMenu",
      gdkmonitor: gdkMonitor,
      anchor: NONE,
      keymode: Astal.Keymode.EXCLUSIVE,
      focusable: true,
      layer: Astal.Layer.OVERLAY,
      child: content,
      onKeyPressed: (_: any, event: number, __: any, mod: Gdk.ModifierType) => {
        handleKey(event, mod);
        return Gdk.EVENT_STOP;
      },
    });

    window.set(win);
  }

  function forceRefreshMenuItems() {
    if (!container) return;
    const newItems = buildMenuItems();
    const currentChild = container.get_first_child();
    if (currentChild) container.remove(currentChild);
    container.append(newItems);
  }

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

    const item = map.get().find((i: any) => i.key === keyString);

    if (item) {
      if (!item.keepOpen) hide();
      item.action();
    } else if (["Escape", "g", "["].includes(keyname)) {
      hide();
    }
  }

  export function toggle() {
    if (!window.get()) return;
    if (window.get()!.is_visible()) {
      hide();
    } else {
      show();
    }
  }

  export function show() {
    if (window.get()) {
      window.get()!.show();
    }
  }

  export function hide() {
    if (window.get()) {
      window.get()!.hide();
    }
  }
}
