Hympd - Simple [MPD](https://mpd.readthedocs.io/en/latest/) web interface

- Minimalistic responsive design with dark and light color schemes.
- Vim-like browser friendly.
- [Easy to hack/customize with userscripts (greasemonkey, tampermonkey, etc).](https://github.com/cortsf/hympd/wiki/Hacking-with-userscripts)
- Self contained: Single binary file containing and providing <ins>**all**</ins> the static resources (css, js, icons) over http.


## Screenshots
Desktop (light and dark color schemes):
<p float="left">
<img src="https://cortsf.github.io/hympd/v2_desktop_queue_light.png" width="140" />
<img src="https://cortsf.github.io/hympd/v2_desktop_root_dark.png" width="140" />
<img src="https://cortsf.github.io/hympd/v2_desktop_album_dark.png" width="140" />
<img src="https://cortsf.github.io/hympd/v2_desktop_hints_dark2.png" width="140" />
</p>

Mobile (light color scheme)
<p>
<img src="https://cortsf.github.io/hympd/v2_mobile_queue_light.jpeg" width="140" />
<img src="https://cortsf.github.io/hympd/v2_mobile_root_light.jpeg" width="140" />
<img src="https://cortsf.github.io/hympd/v2_mobile_album_light.jpeg" width="140" />
</p>

## Build/usage

``` bash
nix build 
./result/bin/hympd --port <port_number> [--mpd-host STRING] [--mpd-port INT] [--mpd-password STRING]

```

Then navigate in your browser to `http://localhost:<port_number>`

#### Nixos service

``` nix
  systemd.user.services.hympd = {
    enable = true;
    requires = [ "mpd.service" ];
    wantedBy = [ "default.target" ];
    script = ''/path/to/hympd/result/bin/hympd --port 3003'';
  };
  networking.firewall.allowedTCPPorts = [ 3003 ]; # If you want access from other devices in you local network.
```

You may want to use a static IP so you can bookmark the url or create a native-like app in your phone. I personally use `networking.networkmanager.enable = true;` and set a static IP with `nmtui`.

#### Android

Both [Native alpha](https://play.google.com/store/search?q=native%20alpha&c=apps) (open source) and [hermit](https://play.google.com/store/search?q=hermit&c=apps) allow to use any webpage like a standalone application (with a home screen icon, no search bar, etc).

