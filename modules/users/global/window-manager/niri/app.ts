import { App } from "astal/gtk4";
import { Menu } from "./which_key_menu/menu";
import { main } from "./which_key_menu/binds";
import { Niri } from "./niri";

App.start({
  // instanceName: "astal-test",
  requestHandler(request: string, res: (response: any) => void) {
    switch (request) {
      case "showMenu":
        Menu.map.set(main);
        Menu.show();
        res("ok");
        break;
      case "hideMenu":
        Menu.hide();
        res("ok");
        break;
      case "toggleMenu":
        Menu.map.set(main);
        Menu.toggle();
        res("ok");
        break;
    }
  },
  main() {
    Niri.updateNiriActiveMonitor();
    Niri.startNiriStream();

    Niri.activeMonitor.subscribe((monitor) => {
      Menu.createOrUpdateMenuWindow(monitor);
    });

    Menu.map.set(main);
    Menu.createOrUpdateMenuWindow(Niri.activeMonitor.get());
  },
});
