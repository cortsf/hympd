Hympd - Simple [MPD](https://mpd.readthedocs.io/en/latest/) web interface

- Minimalistic responsive design with dark and light color schemes.
- Vim-like browser friendly.
- Easy to hack/customize with greasemonkey.


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

## Hacking with greasemonkey

Use `socket.send()` to control MPD:

``` javascript
socket.send('toggle');
socket.send('stop');
socket.send('next');
socket.send('previous');
socket.send('status');
socket.send('clear');
socket.send('random');
socket.send('repeat');
socket.send('single');
socket.send('consume');
socket.send('volume,' + int_number); 
socket.send('playId,' + int_number);
socket.send('deleteId,' +  int_number);
socket.send('playPath,' + string);
socket.send('addPath,' + string);
socket.send('seekCur,' + int_number);
```


## Contributing

Feedback is welcome so feel free to open issues or start a [discussion](https://github.com/cortsf/hympd/discussions).

Consider making a donation to support my work:

- USDC/ETH: 0x8d9894B71a7508FA5eEa2982b0faA56FF77716d2
- USDT (tron): TUnQysgZd77YjKqgKkNw39Ec8aYzSsU9W4
- BTC: bc1qqf5kn0g7lskwkgxgtc070vtj0xlhn42dg6q3r3
- Paypal: [Link](https://www.paypal.com/donate/?business=P5UL6TKK9P5YL&no_recurring=0&currency_code=USD)
