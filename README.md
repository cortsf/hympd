Hympd - Simple [MPD](https://mpd.readthedocs.io/en/latest/) web interface

- Minimalistic responsive design with dark and light color schemes.
- Vim-like browser friendly.
- [Easy to hack/customize with greasemonkey.](https://github.com/cortsf/hympd/wiki/Hacking-with-greasemonkey)


## Screenshots
Desktop (light and dark color schemes):
<p float="left">
<img src="https://cortsf.github.io/hympd/desktop_queue_light.png" width="140" />
<img src="https://cortsf.github.io/hympd/desktop_root_dark.png" width="140" />
<img src="https://cortsf.github.io/hympd/desktop_album_dark.png" width="140" />
<img src="https://cortsf.github.io/hympd/desktop_hints_dark2.png" width="140" />
</p>

Mobile (light color scheme)
<p>
<img src="https://cortsf.github.io/hympd/mobile_queue_light.jpeg" width="140" />
<img src="https://cortsf.github.io/hympd/mobile_root_light.jpeg" width="140" />
<img src="https://cortsf.github.io/hympd/mobile_album_light.jpeg" width="140" />
</p>

## Build/usage

``` bash
nix build 
./result/bin/hympd --port <port_number> [--mpd-host STRING] [--mpd-port INT] [--mpd-password STRING]

```

Then navigate in your browser to `http://localhost:<port_number>`

#### Nixos service

Hopefully Hympd will get into nixpkgs with service options. See [#5](/../../issues/5). In the mean time you can use:

``` nix
  systemd.user.services.hympd = {
    enable = true;
    requires = [ "mpd.service" ];
    wantedBy = [ "default.target" ];
    script = ''/home/cortsf/Documents/Work/Projects/hympd/result/bin/hympd --port 3003'';
  };
  networking.firewall.allowedTCPPorts = [ 3003 ]; # If you want access from other devices in you local network.
```

You may want to use a static IP so you can bookmark the url or create a native-like app in your phone. I personally use `networking.networkmanager.enable = true;` and set a static IP with `nmtui`.

#### Android

Both [Native alpha](https://play.google.com/store/search?q=native%20alpha&c=apps) (open source) and [hermit](https://play.google.com/store/search?q=hermit&c=apps) allow to use any webpage like a standalone application (with a home screen icon, no search bar, etc).

## Contributing

Feedback is welcome so feel free to open issues or start a [discussion](https://github.com/cortsf/hympd/discussions).

Consider making a donation to support my work:

- USDC/ETH: 0x8d9894B71a7508FA5eEa2982b0faA56FF77716d2
- USDT (tron): TUnQysgZd77YjKqgKkNw39Ec8aYzSsU9W4
- BTC: bc1qqf5kn0g7lskwkgxgtc070vtj0xlhn42dg6q3r3
- Paypal: [Link](https://www.paypal.com/donate/?business=P5UL6TKK9P5YL&no_recurring=0&currency_code=USD)
