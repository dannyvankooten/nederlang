#![feature(internal_output_capture)]

use wasm_bindgen::prelude::*;
extern crate nederlang;

use nederlang::object::Error;
use nederlang::object::{Object, Type};
use nederlang::vm::run_str;
use std::sync::Arc;

#[wasm_bindgen(getter_with_clone)]
pub struct NlResponse {
    pub success: bool,
    pub message: String,
}

#[wasm_bindgen]
pub fn nederlang_eval(code: &str) -> NlResponse {
    std::io::set_output_capture(Some(Default::default()));
    
    let program_result = run_str(code);

    let captured = std::io::set_output_capture(None);
    let captured = captured.unwrap();
    let captured = Arc::try_unwrap(captured).unwrap();
    let captured = captured.into_inner().unwrap();
    let captured = String::from_utf8(captured).unwrap();
  
    match program_result {
        Ok(object) => {
            NlResponse {
                success: true,
                message: format!("{}{}", captured, object),
            }
        }
        Err(e) => {
            let message = match e {
                Error::SyntaxError(message) => format!("SyntaxError: {message}"),
                Error::TypeError(message) => format!("TypeError: {message}"),
                Error::ReferenceError(message) => format!("ReferenceError: {message}"),
                Error::IndexError(message) => format!("IndexError: {message}"),
                _ => format!("Fout: iets ging er mis... 🤷"),
            };
            NlResponse {
                success: false,
                message,
            }
        }
    }
}
