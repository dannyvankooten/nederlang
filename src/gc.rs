use crate::object::{Object, Type};
use bitvec::prelude as bv;

/// How many spare items to keep in the objects list after a sweep?
const SHRINK_TO_FIT_THRESH: usize = 64;

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
    pub fn run(&mut self, roots: &mut [&mut [Object]]) {
        // Don't traverse roots if we have no traced objects
        if self.objects.is_empty() {
            return;
        }

        // Mark all reachable objects
        for root in roots.iter_mut() {
            for obj in root.iter_mut() {
                self.mark(obj);
            }
        }

        // Sweep all unreachable objects
        self.sweep();

        self.mark_bitmap.clear();
    }

    /// Sweep all unmarked objects
    pub fn sweep(&mut self) {
        let sweep_range = |objects: &mut Vec<Object>, min, max| {
            for collected in objects.drain(min..max) {
                debug_assert!(collected.is_heap_allocated());
                collected.free();
            }
        };

        let mut last_marked_found: usize;
        // We mark only the index of a reachable object, so sweep backwards to
        // preserve them when the sweep range is drained from the objects vector.
        let mut iter = self.mark_bitmap.iter_ones().rev();
        if let Some(last) = iter.next() {
            last_marked_found = last;

            for marked in iter {
                if marked < last_marked_found {
                    sweep_range(&mut self.objects, marked + 1, last_marked_found);
                    last_marked_found = marked;
                }
            }
        } else {
            // No reachable objects were found.  Collect all the things!
            last_marked_found = self.objects.len();
        }

        // If 0 wasn't marked, we need to sweep from the first to the last one we found.
        if last_marked_found != 0 {
            sweep_range(&mut self.objects, 0, last_marked_found)
        }

        // Draining won't resize the allocation so try to curb it if it goes above
        // a threshold.
        if self.objects.capacity() - self.objects.len() > SHRINK_TO_FIT_THRESH {
            self.objects.shrink_to_fit();
        }
    }

    /// Marks the given object as reachable
    #[inline(always)]
    fn mark(&mut self, o: &mut Object) {
        if !o.is_heap_allocated() {
            return;
        }

        let index = unsafe {
            let object_ptr: *mut Object = o.as_ptr().cast();
            let universe_ptr: *const Object = self.objects.as_ptr().cast();
            object_ptr.offset_from(universe_ptr) as usize
        };
        debug_assert!(index < self.objects.len());
        self.mark_bitmap.set(index, true);

        if o.tag() == Type::Array {
            // Safety: we already checked the type.
            unsafe {
                for v in o.as_vec_unchecked_mut() {
                    self.mark(v);
                }
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

