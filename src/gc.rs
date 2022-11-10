use crate::object::{Header, Object};

pub(crate) struct GC {
    objects: Vec<Object>,
}

impl GC {
    pub fn new() -> GC {
        Self {
            objects: Vec::new(),
        }
    }
    pub fn tracing(&self) -> &Vec<Object> {
        &self.objects
    }

    pub fn init(&mut self) {
        // self.run(&[]);
        // self.objects.clear();
    }

    /// Adds the given object to the list of objects to manage
    #[inline]
    pub fn trace(&mut self, o: Object) {
        self.objects.push(o);
    }

    /// Runs a full mark & sweep cycle
    pub fn run(&mut self, roots: &[&[Object]]) {
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

            // Remove non-heap allocated objects from list
            if !obj.is_heap_allocated() {
                self.objects.swap_remove(i);
                continue;
            }

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
            if self.objects.len() > 0 {
                self.objects.swap_remove(i);
            } else {
                self.objects.pop();
            }

            // Drop and deallocate object
            obj.free();
        }

        #[cfg(debug_assertions)]
        println!(
            "GC SUMMARY: \n\tObjects cleaned: {}\n\tObjects remaining: {}\n\tElapsed time: {}ns",
            size_start - self.objects.len(),
            self.objects.len(),
            time_start.elapsed().as_nanos()
        );
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
