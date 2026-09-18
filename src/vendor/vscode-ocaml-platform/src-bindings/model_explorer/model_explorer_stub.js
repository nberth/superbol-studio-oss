// This binding targets webview (browser) content, not the extension host:
// unlike node_stub.js / vscode_stub.js, there is nothing to `require` here.
// The actual `<model-explorer-visualizer>` custom element and the
// `modelExplorer` global object are registered by the vendored
// "ai-edge-model-explorer-visualizer" script, which must already be loaded
// on the page (e.g. via a <script src="..."> tag) before this code runs.
//
// We only make sure `modelExplorer` exists so that Model_explorer.Global's
// getters/setters have somewhere to read from/write to even if consulted
// before that vendored script has run.
if (globalThis.modelExplorer === undefined) {
  globalThis.modelExplorer = new Object();
}
