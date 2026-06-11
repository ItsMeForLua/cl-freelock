**To Implement:**
- **SOON?** I want to use nix-shell for CI/CD --- for reproducibility.
- **SOON** Finally update the wiki to include a full reference manual of the 20+ userfacing functions in cl-freelock.
- **SOON** I'm probably going to change the github wiki and its reference guide to LaTeX/Tex, and use lwarp to automate converting it to HTML.
Document class in this case will be article 12pt, chicago style. The index will be top level, and there will be included footnotes.
The reference guide and the other portions currently in the wiki will be seperated into different web pages.
A good reference for this proposed change is [here](https://itsmeforlua.github.io/Directed-Dimensions-Basic-and-Fundamental-Transformations-in-n-dimensional-Space/). I can just copy the Makefile targets and such. Though that leads me to wonder if I should actually seperate the latex to html stuff (including the Makefile targets) into a seperate web branch.
- **SOON** When vale linter finally supports common lisp, I'll add it in the dev branch as a CI/CD target.
> I'm going to add the vale linter for actual prose proof reading of the documentations, readme, and maybe even code comments.

- I'm going to include more math and CS reflections for this library in the html web page(s). As currently the reflection writing in the wiki is very ugly.
- I need to update the README, because right now I do not like the prose, and parts of it is a bit too salesman-like. Presumably due to excitement of release.
- I need to revisit the r code that makes the graphs just to see if there's anything I want to change, remove, etc.
- I also need to update the way the latency benchmarks are added to the csv file. I might actually have it be saved as a seperate CSV...now that I think about it, it's probably more reliable to just have each particular type of benchmark be in its own csv, as I remember running into issues with appending and whitespace when I made the logging code. Additionally, extracting particular data for graphing or other analysis will be as simple as just pointing to its file; ergo, any needed parsing code can be much simpler.

- Setup testing for *ccl* and *ecl*.

---
- MPSC (not to be confused with MPMC).
- Implement SPMC (not to be confused with SPSC).
- Benchmark on other hardware via google cloud VM's, and document extensively in the Wiki.

---

#### DONE

~~I'm going to remove the staging branch, as it's been quite useless for me thus far. If I decide I need it in the future, I can just remake it.~~

~~**SOON** Expand the +github actions (EDIT: we're not using jenkins anymore) to test against other lisp implementations.~~
> We've setup testing for CLASP, and now we just need to setup testing for ccl and ecl.

~~**SOON** Staging branch needs to be updated with the updates that are currently present in main, while retaining the updates made to benchmarks in staging (latency stats).~~

~~**FIRST** Update package declarations from #: to :
In ASDF system definition files, symbols like :name, :depends-on, and other system attributes are conventionally keyword symbols (using :). These keywords
are interned and recognized globally, making the system definition clear, standard, and compatible with tooling around ASDF. Using uninterned symbols (#:) can cause issues with symbol recognition in ASDF, as ASDF expects keywords for system properties.~~

~~Move source files to an src/ subdirectory.~~

~~**SECOND**  Add p0, p25, p50, p90, p99, p99.9, pmax numbers for the time taken between an item being committed to the queue, and the item being read under varying NUMA/architecture setups to benchmarks.~~