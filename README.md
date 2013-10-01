app-arch-sandbox
================

This is a toy/sandbox/experiment project to explore the design space
of a web application.

Background
---------

A little while ago, I posted [this question]
(https://groups.google.com/forum/#!searchin/yesodweb/tdox/yesodweb/z8NViwlYA3Y/WwRo6-lsAvUJ)
to the Yesod Web Framework Google
group. This code is some stuff I hacked together to explore possible solutions.


To compile and run
-------------------

To compile and run it, you need to first get the Haskell platform, either from
your package manager (e.g., `apt-get install haskell-platform`), or from
[Haskell.org](http://www.haskell.org/platform/). Then do the following in
your shell:

    $ git clone git://github.com/tdox/app-arch-sandbox
    $ cd app-arch-sandbox
    $ cabal configure
    $ cabal build

This creates three executable web servers, which you can start with:

    $ dist/build/system/system
    $ dist/build/app/app
    $ dist/build/proxy/proxy

Then point your browser to http:/localhost:3000.


Overview
--------

system and app are Yesod apps and proxy is a WAI app.
The proxy sits in front, and sends requests to either system or app, depending
on the path. system and app both check for credentials, but only system provides
the hooks for authentication. (It might be better if proxy handled
authentication, but Yesod's authentication system is requires all of the
machinery of a full Yesod app,
and I couldn't see a simple way to do the same with the simpler proxy WAI app.)

The localhost:3000/system/documentList page contains links to open specific
docs on app.  app doesn't really do anything except show the name of the
organization, the document name and twice the doc ID.  (In the real system,
the proxy will send the request to the app server that is serving the doc
with the given doc ID.)

Note that all links (except for authentication) show localhost:3000, which
is actually proxyinig to both localhost:3001 (system) and localhost:3002 (app).

This was an interesting exercise that taught me a lot and I see room for
further experimentation and improvement.
