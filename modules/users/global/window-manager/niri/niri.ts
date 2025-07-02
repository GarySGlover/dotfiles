import { subprocess, execAsync } from "astal/process";
import { Variable } from "astal";
import { App, Gdk } from "astal/gtk4";

export namespace Niri {
  export const activeMonitor = Variable<Gdk.Monitor>(App.get_monitors()[0]);

  export function updateNiriActiveMonitor() {
    execAsync(["niri", "msg", "--json", "focused-output"])
      .then((out) => {
        try {
          const data = JSON.parse(out);
          const name = data?.name;
          if (!name) return;
          const monitors = App.get_monitors();
          const matchedMonitor = monitors.find(
            (monitor) => monitor.get_connector() === name,
          );
          if (matchedMonitor && activeMonitor.get() !== matchedMonitor) {
            activeMonitor.set(matchedMonitor);
          }
        } catch (e) {
          console.error("Failed to parse output or find monitor:", e);
        }
      })
      .catch((err) => console.error(err));
  }

  export function startNiriStream() {
    const proc = subprocess(
      ["niri", "msg", "--json", "event-stream"],
      (out: string) => {
        let message: any;
        try {
          message = JSON.parse(out);
        } catch (e) {
          console.error("Failed to parse message:", out, e);
          return;
        }

        if (typeof message === "object" && message !== null) {
          if (message.WorkspaceActivated) {
            updateNiriActiveMonitor();
          }
          // In future: handle other message types here
        }
      },
    );

    return proc;
  }
}
