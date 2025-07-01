import { App, Gtk } from "astal/gtk4";
import { Menus, Action } from "./which_key_menu/menu";
import { main } from "./which_key_menu/binds";

Action.set_menu(main)();

App.start({
  requestHandler(request: string, res: (response: any) => void) {
    switch (request) {
      case "showMenus":
        Action.set_menu(main)();
        Action.show_menus()();
        res("ok");
        break;
      case "hideMenus":
        Action.hide_menus()();
        res("ok");
        break;
      case "toggleMenus":
        Action.set_menu(main)();
        Action.toggle_menus()();
        res("ok");
        break;
    }
  },
  main() {
    return Menus;
  },
});
