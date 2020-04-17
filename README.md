Some material for talks, built website with [Hakyll](https://jaspervdj.be/hakyll/), show slides with [patat](https://github.com/jaspervdj/patat), template forked from [acganesh](https://gitlab.com/acganesh/acganesh.gitlab.io).

Build commands:

- `stack build`: Initial compilation of site builder.
- `stack exec site build`: Compile markdown source files to static site.
- `stack exec site watch`: Serve on local host.

Show slide commands:

- `patat xxx.org`: Show slides.
- `nvim ~/.patat.yaml`: Modify patat settings.
