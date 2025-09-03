# cl-freelock

[![License MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![Quicklisp](https://img.shields.io/badge/Quicklisp-available-brightgreen.svg)](http://quicklisp.org/)

A lock-free concurrent data structures library for Common Lisp.

## Overview

`cl-freelock` provides thread-safe, lock-free queues optimized for different use cases and hardware. The library offers three queue types, each designed for specific concurrency patterns and performance requirements.

Lock-free algorithms excel on multi-core systems, providing superior scalability compared to traditional mutex-based approaches. On systems with many cores, cl-freelock demonstrates up to 3.2x performance improvements over competing libraries.

## Features

- **Lock-free algorithms** for multi-core scalability
- **Multiple queue types** optimized for different use cases
- **Batch operations** for maximum throughput
- **Single-threaded optimization** mode for enhanced ST performance
- **Hardware-aware design** that adapts to different CPU configurations
- **Cross-Platform:** Works on Linux, macOS, and Windows
- **Full CI/CD Setup:** Includes a complete testing and benchmarking pipeline using Docker and Jenkins

---

![bounded_queue_comparison](https://i.imgur.com/00fBZeA.png)
*Figure 1. A visual comparison of bounded queue Intel(R) Core(TM) Ultra 7 155H (16 Cores, 22 Logical Processors)*


## Performance

| **Benchmark (Multi-Threaded)** | **Throughput (ops/sec)** | **GC Pressure (1M items)** | **Key Advantage** |
| --- | --- | --- | --- |
| **Bounded Queue (1P/1C)** | **~3.8M** | **~131 KB** | High throughput with virtually no memory allocation. |
| **Bounded Queue (4P/4C)** | **~2.9M** | **~262 KB** | **2.9x faster** than `oconnore/queues` under contention. |
| **SPSC Queue** | **~7.2M** | **~131 KB** | The fastest and most memory-efficient option for 2-thread pipelines. |
| **Batch API (8P/8C)** | **~34.1M** | ~52 MB | Extreme throughput for bulk processing. |

**Specialized APIs:**

- **SPSC Queue (1P/1C):** ~7.9M ops/sec
  
- **Bounded Queue (Batch of 64, 8P/8C):** ~34.1M ops/sec (An incredible order-of-magnitude speedup)

**Single-Threaded Optimization Mode:**

- **SPSC Queue (1P/1C):** ~7.2M ops/sec
  
- **Bounded Queue (1P/1C, Batch of 64):** ~14.6M ops/sec

*Your results may vary based on hardware*

## Installation (for Users)

Once available in Quicklisp, you can load it with:

```lisp
(ql:quickload :cl-freelock)
```

Until then, clone the repository into your Quicklisp `local-projects` directory:

```bash
cd ~/quicklisp/local-projects/
git clone https://github.com/ItsMeForLua/cl-freelock.git
```

Then, load the system in your REPL:

```lisp
(ql:quickload :cl-freelock)
```

## Usage Examples

```lisp
(use-package :cl-freelock)

;; Unbounded MPMC Queue - General purpose
(defvar *q* (cl-freelock:make-queue))
(cl-freelock:queue-push *q* 100)
(multiple-value-bind (obj success) (cl-freelock:queue-pop *q*)
  (when success
    (print obj)))
;; => 100

;; Bounded MPMC Queue - With backpressure control
(defvar *bq* (cl-freelock:make-bounded-queue 1024))
(cl-freelock:bounded-queue-push *bq* :hello)
(cl-freelock:bounded-queue-pop *bq*)
;; => :HELLO, T

;; Batch operations for maximum throughput
(cl-freelock:bounded-queue-push-batch *bq* '(1 2 3))
(cl-freelock:bounded-queue-pop-batch *bq* 3)
;; => (1 2 3), T

;; SPSC Queue - Maximum performance for two-thread pipelines
(defvar *spsc-q* (cl-freelock:make-spsc-queue 2048))
;; Must only be called from designated producer thread
(cl-freelock:spsc-push *spsc-q* "world")
;; Must only be called from designated consumer thread
(cl-freelock:spsc-pop *spsc-q*)
;; => "world", T
```

---

## Queue Types

### Unbounded MPMC Queue

A multi-producer, multi-consumer queue that can grow indefinitely. Perfect for general-purpose thread-safe communication when capacity is not a concern.

**When to use:** General-purpose, thread-safe communication where capacity is not a concern.

### Bounded MPMC Queue

A multi-producer, multi-consumer queue with fixed capacity (must be a power of two). Ideal for applying backpressure and achieving better memory locality.

**When to use:** When you need to apply backpressure (i.e., prevent producers from getting too far ahead of consumers) or when you have predictable capacity requirements.

### Bounded SPSC Queue

A highly specialized single-producer, single-consumer queue.

**When to use:** For better performance in a two-thread pipeline. It is significantly faster than the general purpose MPMC queues.

## Hardware Considerations

- **Multi-core systems (4+ cores):** Lock-free queues excel and provide significant performance advantages
- **Limited cores (1-2 cores):** Consider lock-based alternatives as atomic operations may have higher overhead
- **Thread count:** Match active threads to available CPU cores for optimal performance
- **CPU oversubscription:** Avoid running more threads than available cores to prevent context switching thrashing

## Developer Setup (for Contributors)

This project uses **Qlot** for reproducible dependency management and a **Makefile** for automating common tasks.

### Prerequisites

- Install [Roswell](https://github.com/roswell/roswell/wiki) (for managing Lisp implementations)
- Install [Qlot](https://github.com/fukamachi/qlot) (`ros install qlot`)
- Install [Docker](https://www.docker.com/) and [Docker Compose](https://docs.docker.com/compose/install/) (for running the CI pipeline)

### Dependency Management with Qlot

1. **Install Project Dependencies**: This command reads the `qlfile` and installs the exact versions of all dependencies into a local `.qlot/` directory.

```bash
# From the project's root directory
make deps
```

2. **Start a REPL**: To work on the project, start your REPL using `qlot exec`. This ensures your Lisp session uses the project's local dependencies.

```bash
qlot exec ros run
```

### Continuous Integration with Docker & Jenkins

The repository includes a complete CI setup to automate testing and ensure code quality.

- **`Dockerfile`**: Defines a build environment with Roswell, SBCL, and Qlot pre-installed
- **`docker-compose.yml` & `Dockerfile.jenkins`**: These files set up a local Jenkins instance, configured to run the project's pipeline
- **`Jenkinsfile`**: This file defines the CI pipeline stages: build, test, and benchmark

To run the full CI pipeline locally:

1. **Start the Jenkins server**:

```bash
# Ensure your user can access the Docker daemon
docker-compose up -d
```

2. **Access Jenkins** at `http://localhost:8180`
  
3. **Create a new "Pipeline" job** and configure it to use the `Jenkinsfile` from SCM (pointing to your local git repository). This will replicate the exact environment used for automated testing.
  

## Running Tests and Benchmarks

The `Makefile` provides the easiest way to run the test and benchmark suites.

- **To run the test suite**:

```bash
# Using the qlot environment
make test

# Or using your local system's Lisp environment
make test-local
```

- **To run the benchmarks**:

```bash
# Using the qlot environment (multi-threaded default)
make benchmark

# Single-threaded optimized benchmarks
make benchmark-st
```

- **To Generate Graphs**

``` bash
# Make sure you have R installed, and ran benchmarks with CSV_LOG=filename.csv set
make graphs # or: make graphs CSV_FILE=Custom.csv
```
> ***NOTE:** the initial auto-deps will be a few minutes.*

### Available Make Targets

- `make deps` - Install dependencies using qlot.
  
- `make test` - Run tests in a reproducible qlot environment.
  
- `make test-local` - Run tests with system Lisp (no qlot).

- `make benchmark` - Run default benchmarks. Add `CSV_LOG=file.csv` to save results to a file.
  
- `make benchmark-st` - Run single-threaded benchmarks. Add `CSV_LOG=file.csv` to save results.
  
- `make benchmark-all` - Run all benchmarks and save to a file (`CSV_LOG` is required).
  
- `make graphs` - Generate plots from a CSV file (e.g., `make graphs CSV_FILE=results.csv`).
  
- `make install-dev` - Symlink the project to your local-projects directory for development.
  
- `make clean` - Clean all build artifacts.
  
- `make help` - Show a list of all available commands and their descriptions.

## Wiki
If you're interested in a more thorough deep-dive regarding this project, please refer to the [Wiki](https://github.com/ItsMeForLua/cl-freelock/wiki).

## Contributing

Bug reports and pull requests are welcome on GitHub. Please ensure the test suite passes by at least running `make test` before submitting a pull request. However, it is preferable you use the Jenkins CI/CD pipeline before submitting a PR, as this will prevent silly mistakes (happens to all of us), and will reduce the GitHub actions usage.

## License

This project is licensed under the **MIT License**.
