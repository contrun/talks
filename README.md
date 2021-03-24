Some materials for talks, built website with [Hakyll](https://jaspervdj.be/hakyll/), show slides with [patat](https://github.com/jaspervdj/patat), template forked from [acganesh](https://gitlab.com/acganesh/acganesh.gitlab.io).

Build commands:

- `stack build`: Initial compilation of site builder.
- `stack exec site  build`: Compile source files to static site.
- `stack exec site  build`: To generate correct link for org files, compile twice.
- `stack exec site watch`: Serve on local host.

Show slide commands:

- `patat xxx.org`: Show slides.
- `nvim ~/.patat.yaml`: Modify patat settings.
