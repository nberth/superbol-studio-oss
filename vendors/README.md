# Vendored dependencies for SuperBOL Studio OSS

Please update `opam-fix` and `opam-cross` rules in [`../Makefile.header`](../Makefile.header) when adding or removing a vendored *opam package* (e.g. `ANSITerminal`, `goblint-cil`).

## `model-explorer`

Unlike the opam packages above, [`model-explorer`](model-explorer) is a git submodule (pinned to the `model-explorer-visualizer-npm-v0.1.2` tag of [`google-ai-edge/model-explorer`](https://github.com/google-ai-edge/model-explorer)) holding the *source* of the `ai-edge-model-explorer-visualizer` npm package: the Angular app that becomes the `<model-explorer-visualizer>` custom element used by [`Superbol_model_explorer`](../src/vscode/superbol-vscode-lib/superbol_model_explorer.ml).  It is not built as part of the normal SuperBOL build (it isn't an opam/dune package, and building it needs Node/npm and the Angular CLI, not just `esbuild`), and it isn't itself shipped: what SuperBOL Studio actually bundles is the pre-built output already checked in under [`../assets/vendor/model-explorer/`](../assets/vendor/model-explorer) (plus [`../assets/vendor/model-explorer.LICENSE`](../assets/vendor/model-explorer.LICENSE)).

The submodule exists so that bundle can be refreshed from source -- to pick up a newer upstream commit, or to carry small local patches (e.g. to the hard-coded "Model Explorer" title, or to forward a light/dark theme option) that the published npm package doesn't expose as configuration. Its checkout is sparse (`src/ui` only, via `git sparse-checkout`), since the rest of the upstream monorepo (Python package, adapters, demos, CI) isn't needed to build the custom element.

To refresh `assets/vendor/model-explorer/` from the submodule, type in the following commands from the root of the source tree:

```
make update-model-explorer-vendor
make build-model-explorer
```

This requires network access, plus Node.js/npm to install the Angular toolchain the submodule's own build uses -- none of that is otherwise a build dependency of SuperBOL Studio, so this is a deliberate, occasional maintenance step, not something run as part of `make` or CI.
