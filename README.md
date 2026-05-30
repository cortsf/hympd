Hympd - Simple [MPD](https://mpd.readthedocs.io/en/latest/) web interface

- Minimalistic responsive design with dark and light color schemes.
- Vim-like browser friendly.
- [Easy to hack/customize with userscripts (greasemonkey, tampermonkey, etc).](https://github.com/cortsf/hympd/wiki/Hacking-with-userscripts)
- No runtime deps: Compiles into a single and independent statically linked executable file containing and providing <ins>**all**</ins> the static resources (css, js, icons) over http.


## Screenshots
Desktop (dark color scheme):
<p float="left">
<img src="https://cortsf.github.io/hympd/v5_desktop_queue.png" width="140" />
<img src="https://cortsf.github.io/hympd/v5_desktop_root.png" width="140" />
<img src="https://cortsf.github.io/hympd/v5_desktop_album.png" width="140" />
<img src="https://cortsf.github.io/hympd/v5_desktop_search.png" width="140" />
<img src="https://cortsf.github.io/hympd/v5_desktop_hints.png" width="140" />
</p>

Mobile (light color scheme)
<p>
<img src="https://cortsf.github.io/hympd/v5_mobile_queue.jpeg" width="140" />
<img src="https://cortsf.github.io/hympd/v5_mobile_root.jpeg" width="140" />
<img src="https://cortsf.github.io/hympd/v5_mobile_album.jpeg" width="140" />
<img src="https://cortsf.github.io/hympd/v5_mobile_search.jpeg" width="140" />
</p>

## Build/usage

1. Download latest binary [release](https://github.com/cortsf/hympd/releases), or build a statically linked binary using `nix build .#x86_64-unknown-linux-musl:hympd:exe:hympd` (linux only, build dynamically with `nix build` otherwise)

2. Execute with `./result/bin/hympd --port <port_number> [--mpd-host STRING] [--mpd-port INT] [--mpd-password STRING]`. Since there are no runtime deps, the relative location of the static resources (css, js and icons) is not relevant.

3. Navigate to `http://localhost:<port_number>`

Note: Building on darwing has not been tested by the author of this package. With some nix tweaks, it should/could be possible to cross-compile a windows executable.

#### Nixos service

``` nix
  systemd.user.services.hympd = {
    enable = true;
    requires = [ "mpd.service" ];
    wantedBy = [ "default.target" ];
    script = ''/path/to/hympd/result/bin/hympd --port 3003'';
  };
  networking.firewall.allowedTCPPorts = [ 3003 ];.
```

You may want to use a static IP so you can bookmark the url or create a native-like app in your phone. I personally use `networking.networkmanager.enable = true;` and set a static IP with `nmtui`.

#### Android

Both [Native alpha](https://play.google.com/store/search?q=native%20alpha&c=apps) (open source) and [hermit](https://play.google.com/store/search?q=hermit&c=apps) allow to use any webpage like a standalone application (with a home screen icon, no search bar, etc).

