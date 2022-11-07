importScripts('../pkg/playground.js');
const { nederlang_eval } = wasm_bindgen;

async function init_wasm() {
    await wasm_bindgen('../pkg/playground_bg.wasm');

    self.onmessage = async event => {
        let start = performance.now();
        let response = nederlang_eval(event.data);
        let elapsed_time = Math.round(performance.now() - start);

        self.postMessage([response.success, response.message, elapsed_time]);
    };
};

init_wasm();