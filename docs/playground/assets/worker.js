importScripts('../pkg/playground.js');
const { nederlang_eval } = wasm_bindgen;

async function init_wasm() {
    await wasm_bindgen('../pkg/playground_bg.wasm');

    self.onmessage = async event => {
        let start = performance.now();
        let response;
        try {
            response = nederlang_eval(event.data);
        } catch(e) {
            response = {
                success: false,
                message: "Fout: Er ging iets mis... 🤷 Zou je je programmacode als <a href=\"https://github.com/dannyvankooten/nederlang/issues/new?labels=bug&title=Kritieke%20fout\">GitHub issue</a> kunnen plaatsen?"
            }
        }

        let elapsed_time = Math.round(performance.now() - start);
        self.postMessage([response.success, response.message, elapsed_time]);
    };
};

init_wasm();