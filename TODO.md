**To Implement:**
- **FIRST** Update package declarations from #: to :
> In ASDF system definition files, symbols like :name, :depends-on, and other system attributes are conventionally keyword symbols (using :). These keywords
> are interned and recognized globally, making the system definition clear, standard, and compatible with tooling around ASDF. Using uninterned symbols (#:) can cause issues with symbol recognition in ASDF, as ASDF expects keywords for system properties.
- **SECOND**  Add p0, p25, p50, p90, p99, p99.9, pmax numbers for the time taken between an item being committed to the queue, and the item being read under varying NUMA/architecture setups to benchmarks.
- **THIRD** Expand the docker-jenkins CI to test against other lisp implementations.

---

- MPSC (not to be confused with MPMC).
- Implement SPMC (not to be confused with SPSC).
- Move source files to an src/ subdirectory.
- add verbose debug logging for docker-jenkins CI errors.
- Benchmark on other hardware via google cloud VM's, and document extensively in the Wiki.
