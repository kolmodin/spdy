The SPDY Protocol
=================

[SPDY](http://dev.chromium.org/spdy) is an experiment with protocols for the
web. Its goal is to reduce the latency of web pages.

This package attempts to implement the protocol in the Haskell programming
language.

See [the SPDY tool page](http://dev.chromium.org/spdy/spdy-tools-and-debugging)
for debugging tools.

So far no support for NPN (Next Protocol Negotiation) in the tls package,
which means we cannot use alternate-protocol npn-spdy/2.
For some reason I haven't managed to get alternate-protocol spdy/2 to work
in Chrome, looks like it's not supported at the moment.
This limits the use of this library, as we need to force ssl in Chrome for
all pages, which means it breaks pretty much the rest of the web.

This is why it's useful to keep your current Chrome instance, while forcing
ssl in another instance where you develop. Here's how I do it;

> google-chrome --use-spdy=ssl --user-data-dir=/tmp/spdy-chrome
