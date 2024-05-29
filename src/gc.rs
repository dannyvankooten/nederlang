use crate::object::{Object, Type};
use bitvec::prelude as bv;

// TODO: Change visibility of GC to crate-private (not directly possible because of pub Object type)
pub struct GC {
    /// Vector of all currently alive heap-allocated objects in the universe
    objects: Vec<Object>,

    /// All marked objects during a run.
    mark_bitmap: bv::BitVec,
}

impl GC {
    /// Create a new Garbage Collector to manage heap-allocated objects
    pub fn new() -> GC {
        Self {
            objects: Vec::new(),
            mark_bitmap: bv::BitVec::new(),
        }
    }

    #[inline]
    pub fn maybe_trace(&mut self, o: Object) {
        if o.is_heap_allocated() {
            self.objects.push(o);
            self.mark_bitmap.reserve(1);
        }
    }

    /// Adds the given object to the list of objects to manage
    #[inline]
    pub fn trace(&mut self, o: Object) {
        self.objects.push(o);
        self.mark_bitmap.reserve(1);
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

            self.mark_bitmap.truncate(self.objects.len());
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

        self.mark_bitmap.clear();

        // Mark all reachable objects
        for root in roots.iter() {
            for obj in root.iter() {
                self.mark(obj);
            }
        }

        // Sweep all unreachable objects
        self.sweep();
    }

    /// Sweep all unmarked objects
    pub fn sweep(&mut self) {
        // Sweep in reverse unmarked order to preserve the index as
        // elements are removed from the objects vector.
        for unmarked in self.mark_bitmap.iter_zeros().rev() {
            let object = self.objects.swap_remove(unmarked);
            debug_assert!(object.is_heap_allocated());
            object.free();
        }

        self.mark_bitmap.truncate(self.objects.len());
    }

    /// Marks the given object as reachable
    #[inline(always)]
    fn mark(&mut self, o: &Object) {
        if !o.is_heap_allocated() {
            return;
        }

        let index = unsafe {
            let object_ptr: *mut Object = o.as_ptr().cast();
            let universe_ptr: *const Object = self.objects.as_ptr().cast();
            object_ptr.offset_from(universe_ptr) as usize
        };
        debug_assert!(index < self.objects.len());

        if o.tag() == Type::Array {
            // Safety: we know the size of mark_bitmap.
            unsafe {
                // No need to mark recursively on arrays if this one was
                // already marked (e.g. because the same object was found
                // in multiple places such as the stack and the result of
                // a function call).
                if !self.mark_bitmap.get_unchecked(index) {
                    self.mark_bitmap.set_unchecked(index, true);

                    // Safety: we already checked the type.
                    for v in o.as_vec_unchecked() {
                        self.mark(v);
                    }
                }
            }
        } else {
            unsafe {
                // Safety: we know the size of mark_bitmap.
                self.mark_bitmap.set_unchecked(index, true);
            }
        }
    }
}

/// Implement Drop trait so that GC::destroy() is automatically called once the Garbage Collector goes out of scope
impl Drop for GC {
    fn drop(&mut self) {
        self.destroy();
    }
}
