golare
=====

A sentry SDK for erlang

Build
-----

    $ rebar3 compile

Transport states
-----

```mermaid
flowchart TD
    started --> connecting
    connecting --> available
    available --> sending
    available --> connecting
    sending --> available
    sending --> rate-limited
    sending --> connecting
    rate-limited --> available
    rate-limited --> sending
    rate-limited --> connecting
```

