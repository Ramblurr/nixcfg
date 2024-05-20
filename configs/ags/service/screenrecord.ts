import GLib from "gi://GLib";
import icons from "lib/icons";
import { dependencies, sh, bash } from "lib/utils";

const now = () => GLib.DateTime.new_now_local().format("%Y-%m-%d_%H-%M-%S");

const screenshotDir = () => {
  let maybeScrDir = GLib.getenv("XDG_SCREENSHOTS_DIR");
  if (maybeScrDir) {
    return maybeScrDir;
  }
  maybeScrDir = GLib.get_user_special_dir(
    GLib.UserDirectory.DIRECTORY_PICTURES,
  );
  if (maybeScrDir) {
    return maybeScrDir;
  }
  return Utils.HOME + "/Pictures/Screencasting";
};
class Recorder extends Service {
  static {
    Service.register(
      this,
      {},
      {
        timer: ["int"],
        recording: ["boolean"],
      },
    );
  }

  #recordings = screenshotDir();
  #screenshots = screenshotDir();
  #file = "";
  #interval = 0;

  recording = false;
  timer = 0;

  async start() {
    if (!dependencies("slurp", "wf-recorder")) return;

    if (this.recording) return;

    Utils.ensureDirectory(this.#recordings);
    this.#file = `${this.#recordings}/${now()}.mp4`;
    sh(
      `wf-recorder -g ${await sh("slurp")} -f ${this.#file} --pixel-format yuv420p`,
    );

    this.recording = true;
    this.changed("recording");

    this.timer = 0;
    this.#interval = Utils.interval(1000, () => {
      this.changed("timer");
      this.timer++;
    });
  }

  async stop() {
    if (!this.recording) return;

    await bash("killall -INT wf-recorder");
    this.recording = false;
    this.changed("recording");
    GLib.source_remove(this.#interval);

    Utils.notify({
      iconName: icons.fallback.video,
      summary: "Screenrecord",
      body: this.#file,
      actions: {
        "Show in Files": () => sh(`xdg-open ${this.#recordings}`),
        View: () => sh(`xdg-open ${this.#file}`),
      },
    });
  }

  async screenshot(full = false) {
    if (!dependencies("slurp", "wayshot")) return;

    const file = `${this.#screenshots}/${now()}.png`;
    Utils.ensureDirectory(this.#screenshots);

    if (full) {
      await sh(`wayshot -f ${file}`);
    } else {
      const size = await sh("slurp");
      if (!size) return;

      await sh(`wayshot -f ${file} -s "${size}"`);
    }

    bash(`wl-copy < ${file}`);

    Utils.notify({
      image: file,
      summary: "Screenshot",
      body: file,
      actions: {
        "Show in Files": () => {
          if (dependencies("re.sonny.Junction")) {
            sh(`re.sonny.Junction ${this.#screenshots}`);
          } else {
            sh(`xdg-open ${this.#screenshots}`);
          }
        },
        View: () => {
          if (dependencies("re.sonny.Junction")) {
            sh(`re.sonny.Junction ${file}`);
          } else {
            sh(`xdg-open ${file}`);
          }
        },
        Copy: () => sh(`cat ${file} | wl-copy`),
        Edit: () => {
          if (dependencies("satty")) sh(`satty -f ${file}`);
        },
      },
    });
  }
}

const recorder = new Recorder();
Object.assign(globalThis, { recorder });
export default recorder;
