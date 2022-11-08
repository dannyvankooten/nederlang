use wasm_bindgen::prelude::*;
extern crate nederlang;

use nederlang::object::Error;
use nederlang::object::NlObject;
use nederlang::vm::run_str;

#[wasm_bindgen(getter_with_clone)]
pub struct NlResponse {
    pub success: bool,
    pub message: String,
}

#[wasm_bindgen]
pub fn nederlang_eval(code: &str) -> NlResponse {
    match run_str(code) {
        Ok(object) => {
            let result = match object {
                NlObject::Int(v) => v.to_string(),
                NlObject::Float(v) => v.to_string(),
                NlObject::Bool(v) => v.to_string(),
                _ => "".to_string(),
            };
            NlResponse {
                success: true,
                message: result,
            }
        }
        Err(e) => {
            let message = match e {
                Error::SyntaxError(message) => format!("SyntaxError: {message}"),
                Error::TypeError(message) => format!("TypeError: {message}"),
                Error::ReferenceError(message) => format!("ReferenceError: {message}"),
                Error::IndexError(message) => format!("IndexError: {message}"),
                _ => format!("Fout: iets ging er mis.."),
            };
            NlResponse {
                success: false,
                message,
            }
        }
    }
}