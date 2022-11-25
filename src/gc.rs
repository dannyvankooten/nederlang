use crate::object::{Header, Object, Type};

// TODO: Change visibility of GC to crate-private
pub struct GC {
    /// Vector of all currently alive heap-allocated objects in the universe
    objects: Vec<Object>,
}

impl GC {
    /// Create a new Garbage Collector to manage heap-allocated objects
    pub fn new() -> GC {
        Self {
            objects: Vec::with_capacity(8),
        }
    }

    #[inline]
    pub fn maybe_trace(&mut self, o: Object) {
        if o.is_heap_allocated() {
            self.objects.push(o);
        }
    }

    /// Adds the given object to the list of objects to manage
    #[inline]
    pub fn trace(&mut self, o: Object) {
        self.objects.push(o);
    }

    /// Removes the given object (and everything it refers) from this garbage collector so it is no longer managed by it
    pub fn untrace(&mut self, o: Object) {
        if let Some(pos) = self
            .objects
            .iter()
            .position(|a| std::ptr::eq(a.as_ptr(), o.as_ptr()))
        {
            self.objects.swap_remove(pos);

            if o.tag() == Type::Array {
                // Safety: We've already checked the type
                unsafe {
                    for val in o.as_vec_unchecked() {
                        self.untrace(*val);
                    }
                }
            }
        }
    }

    /// Sweeps all objects
    /// This is automatically called once the Garbage Collector is dropped
    pub fn destroy(&mut self) {
        self.sweep();
    }

    /// Runs a full mark & sweep cycle
    /// Only objects in the given roots are kept alive
    pub fn run(&mut self, roots: &[&[Object]]) {
        // Don't traverse roots if we have no traced objects
        if self.objects.is_empty() {
            return;
        }

        // mark all reachable objects
        for root in roots.iter() {
            for obj in root.iter() {
                mark(obj);
            }
        }

        // sweep unmarked objects
        self.sweep();
    }

    /// Sweep all unmarked objects
    pub fn sweep(&mut self) {
        let mut i = 0;
        while i < self.objects.len() {
            let obj = self.objects[i];

            // Immediate values should not end up on the traced objects list
            debug_assert!(obj.is_heap_allocated());

            // Object is heap allocated
            // Read its header to check if its marked
            // If its marked, clear flag & continue
            let mut header = unsafe { Header::read(&obj) };
            if header.marked {
                header.marked = false;
                i += 1;
                continue;
            }

            // Remove object from tracing list
            self.objects.swap_remove(i);

            // Drop and deallocate object
            obj.free();
        }
    }
}

/// Implement Drop trait so that GC::destroy() is automatically called once the Garbage Collector goes out of scope
impl Drop for GC {
    fn drop(&mut self) {
        self.destroy();
    }
}

/// Marks the given object as reachable
#[inline]
fn mark(o: &Object) {
    if !o.is_heap_allocated() {
        return;
    }

    let mut header = unsafe { Header::read(o) };
    header.marked = true;

    if o.tag() == Type::Array {
        for v in o.as_vec() {
            mark(v);
        }
    }
}
