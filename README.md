The SPDY Protocol
=================

[SPDY](http://dev.chromium.org/spdy) is an experiment with protocols for the
web. Its goal is to reduce the latency of web pages.

This package attempts to implement the protocol in the Haskell programming
language.

See [the SPDY tool page](http://dev.chromium.org/spdy/spdy-tools-and-debugging)
for debugging tools.

SPDY requires TLS to have support for NPN (Next Protocol Negotiation).
There is ongoing work in adding NPN to the Haskell TLS package, you'll need
it to compile this package.
See [my tls branch npn](https://github.com/kolmodin/hs-tls/tree/npn).
Documentation of [npn here](http://technotes.googlecode.com/git/nextprotoneg.html).
