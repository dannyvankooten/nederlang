use crate::object::{Header, Object};

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

    /// Adds the given object to the list of objects to manage
    #[inline]
    pub fn trace(&mut self, o: Object) {
        self.objects.push(o);
    }

    /// Removes the given object from this garbage collector so it is no longer managed by it
    #[inline]
    pub fn untrace(&mut self, o: Object) {
        if let Some(pos) = self
            .objects
            .iter()
            .position(|a| std::ptr::eq(a.as_ptr(), o.as_ptr()))
        {
            self.objects.swap_remove(pos);
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
        #[cfg(debug_assertions)]
        let time_start = std::time::Instant::now();
        #[cfg(debug_assertions)]
        let size_start = self.objects.len();

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

        #[cfg(debug_assertions)]
        println!(
            "GC SUMMARY: \n  {:4} objects cleaned\n  {:4} objects remaining:\n  {:4} ns elapsed",
            size_start - self.objects.len(),
            self.objects.len(),
            time_start.elapsed().as_nanos()
        );
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
}
