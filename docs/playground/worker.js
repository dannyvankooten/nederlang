importScripts('./pkg/playground.js');
const { nederlang_eval } = wasm_bindgen;

async function init_wasm() {
    // Load the wasm file by awaiting the Promise returned by `wasm_bindgen`.
    await wasm_bindgen('./pkg/playground_bg.wasm');

    // Set callback to handle messages passed to the worker.
    self.onmessage = async event => {
        let start = performance.now();
        let response = nederlang_eval(event.data);
        let elapsed_time = Math.round(performance.now() - start);

        // Send response back to be handled by callback in main thread.
        self.postMessage([response.success, response.message, elapsed_time]);
    };
};

init_wasm();