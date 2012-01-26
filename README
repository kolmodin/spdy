The SPDY Protocol
=================

[SPDY](http://dev.chromium.org/spdy) is an experiment with protocols for the
web. Its goal is to reduce the latency of web pages.

This package attempts to implement the protocol in the Haskell programming
language.

See [the SPDY tool page](http://dev.chromium.org/spdy/spdy-tools-and-debugging)
for debugging tools.

So far no support for TLS or NPN (Next Protocol Negotiation).
It's useful to keep your current Chrome instance, while forcing no-ssl in
another instance. Here's how I do it;

> google-chrome --use-spdy=no-ssl --user-data-dir=/tmp/spdy-chrome
