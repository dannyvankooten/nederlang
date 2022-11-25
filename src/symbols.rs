pub(crate) struct SymbolTable {
    /// A vector of contexts
    /// The context at index 0 will always be the global context,
    /// any context that follows is a local (to a function) context.
    /// There can be more than one local context as functions can be nested inside other functions.
    contexts: Vec<Context>,
}

pub(crate) struct Symbol {
    pub scope: Scope,
    pub index: u16,
}

#[derive(PartialEq, Copy, Clone)]
pub(crate) enum Scope {
    Local,
    Global,
}

/// A context is a type of environment to store values in. This can be either a global context or a local (to a function) context.
pub(crate) struct Context {
    scope: Scope,
    max_size: usize,
    symbols: Vec<Vec<String>>,
}

impl Context {
    fn new(scope: Scope) -> Self {
        Context {
            scope: scope,
            max_size: 0,
            symbols: vec![Vec::new()],
        }
    }

    /// The maximum number of symbols defined in this context.
    /// Not all of these symbols may still be in scope once this context is destroyed.
    fn max_size(&self) -> usize {
        self.max_size
    }

    /// The (current) number of defined symbols in this context.
    #[inline]
    fn total_len(&self) -> usize {
        self.symbols.iter().fold(0, |acc, s| acc + s.len())
    }

    /// Defines a new symbol in the current context its inner-most scope.
    fn define(&mut self, name: &str) -> Symbol {
        let current_scope = self.symbols.last_mut().unwrap();
        current_scope.push(name.to_string());
        self.max_size += 1;

        Symbol {
            index: (self.total_len() - 1).try_into().unwrap(),
            scope: self.scope,
        }
    }

    /// Resolves a symbol in this context along with its absolute index (relative to the context its top scope)
    fn resolve(&self, name: &str) -> Option<Symbol> {
        let mut abs_index = self.total_len();
        for scope in self.symbols.iter().rev() {
            abs_index -= scope.len();
            if let Some(index) = scope.iter().position(|n| n == name) {
                return Some(Symbol {
                    index: (abs_index + index).try_into().unwrap(),
                    scope: self.scope,
                });
            }
        }

        None
    }
}

impl SymbolTable {
    /// Creates a new symbol table with a globally scoped context
    pub fn new() -> Self {
        SymbolTable {
            contexts: vec![Context::new(Scope::Global)],
        }
    }

    /// Returns a mutable reference to the current context
    #[inline(always)]
    fn current_context(&mut self) -> &mut Context {
        self.contexts.last_mut().unwrap()
    }

    /// Create a new context to define symbols in.
    /// This will always be a local context (as there is only one global context).
    #[inline]
    pub fn new_context(&mut self) {
        self.contexts.push(Context::new(Scope::Local));
    }

    /// Destroys the current context and returns the maximum number of symbols it had at some point in time.
    #[inline]
    pub fn leave_context(&mut self) -> usize {
        self.contexts.pop().unwrap().max_size()
    }

    /// Enter a new scope in the current context
    /// For example, at the start of a block statement.
    #[inline]
    pub fn enter_scope(&mut self) {
        self.current_context().symbols.push(Vec::new());
    }

    /// Leave scope in the current context.
    /// For example, at the end of a block statement.
    #[inline]
    pub fn leave_scope(&mut self) {
        self.current_context().symbols.pop().unwrap();
    }

    /// Define a symbol in the current context (and current scope within that context).
    #[inline]
    pub fn define(&mut self, name: &str) -> Symbol {
        self.current_context().define(name)
    }

    /// Resolve a symbol in either the current context or the global context if no local was found.
    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        let symbol = self.current_context().resolve(name);
        if symbol.is_some() {
            return symbol;
        }

        if self.contexts.len() > 1 {
            self.contexts[0].resolve(name)
        } else {
            None
        }
    }
}
