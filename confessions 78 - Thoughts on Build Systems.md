![header](https://filebox.tymoon.eu//file/TVRRMU5nPT0=)  
In an effort to write some more articles I thought I might as well write one where I lay down my current thoughts on build systems. I've been thinking about writing my own for a good while now, and there's a few conclusions I've come to. I expect that these thoughts will eventually crystallise into a new general build system in Lisp.

Before I dive into anything at all, I want to describe what I understand by "build system". A build system is a piece of software that takes an abstract description of a project and uses this to perform a variety of tasks. That's pretty general, so let me attempt to clarify with a non-exhaustive listing of tasks that I consider being the responsibilities of a build system:

* Running some other software like a compiler.
* Generally transforming and digesting files.
* Producing a complete installation of a project, ready for deployment.
* Analysing a project's contents to generate data about it.
* Testing the project for correctness.
* Resolving version and dependency relations.

And here are some things I do *not* consider tasks of a build system:

* Creating a project skeleton on your file system.
* Publishing, distributing, or fetching software.

Generally there are a couple of important notes I'd like to make to combat the preconceptions and ideas people might have about build systems:

* A build system is not about files.
* The management and installation of software or other components is not a part of building projects.
* Projects can include anything, not just software. A project could be an article like this.
* A build system is *not* about files.
* The build system is not necessarily tied to a particular language or environment.
* A task executed by the build system does not have to happen on the local machine.
* *A build system is not about files.*

Now that we have all of this out of the way, let's continue on to the thoughts I've had so far about what this future build system might look like. Let's call that system "Forge" for now.

The most recent and most important conclusion I've come to for Forge is that there are two, mostly independent, systems with almost entirely reversed semantics. The first system is a general execution engine. It is a system that takes a description of steps, where each step has a number of input requirements and output results, and then tries to execute the steps in such an order that each step's input requirements are fulfilled before the step can be executed.

While it will often be the case that the result of an event will fulfil the requirement of another, it is important to remember that they are more general than that. This decoupling of requirements allows a distancing from focusing on "output and input files," and allows the execution to be distributed across machines without breaking semantics.

The second system is a dependency manager. In this system, projects are modelled as collections of components, where each component has a set of dependencies on actions done on other components. Each dependency can have additional constraints to qualify the relationship. The important part is that the flow of information is reversed and more coupled compared to the execution engine. This is because generally we have an easier time thinking of relationships in projects in terms of dependencies. A project description combined with a desired task can then be transformed into an execution plan to be run by the plan executor.

In order to achieve reproducibility and stability, the generation of a plan for a project and task must be entirely deterministic, regardless of the current state of the system Forge is run on. It is the responsibility of the execution engine to figure out which steps need to actually be performed, in which order they should be performed, and on which machine they should be performed.

For plan generation to be deterministic, the project description cannot include anything that alters that global system state in any way. This is a harsh constraint and has some serious implications, but I think it is an absolutely necessary one. It is harsh because it means the build system must account for a large variety of cases where the modification of global state would be reasonable. Let me give you a real-life example for one of my projects.

[Qt-libs](https://github.com/Shinmera/qt-libs)' job is to provide the system with a usable installation of the Qt libraries for CommonQt without requiring any compiler or other kind of toolchain to be installed on the system. Unfortunately, CommonQt includes a step in its build process that invokes GCC to try and automatically build one of its dependencies. Qt-libs must now suppress this step. With ASDF, this can only be done through a [gross hack](https://github.com/Shinmera/qt-libs/blob/master/qt-libs.asd#L20)[\[1\]](#1)[\[2\]](#2). CommonQt's build step is reasonable in itself, but so is Qt-libs' requirement. In order to provide a clean solution for this, Forge must therefore allow projects to influence how the plan is computed beyond components that are part of the project itself.

Note though that I mentioned it cannot alter *global* state. Local state modification is fine, and thus I don't think I'll have to go the purely functional way with Forge. My current intention in this regard is to allow a subset of Common Lisp as the scripting language for Forge projects, one that has no access to the global environment of the host running Forge. This will require a custom reader and possibly evaluator, so it will be quite a bit of work regardless.

A side benefit of determinism and the execution engine segregation is that it should be much easier to debug builds, and it should be possible to create separated scripts that perform a task without the need for Forge to be present at all. The much more general view on projects also means it should be possible to use Forge for continuous integration, general build management, and system management.

While that all sounds well and good, none of it exists yet. As mentioned, this is just me trying to figure out the requirements and design decisions for the system. At this point I am quite convinced that Forge will become real at some point, but there are a lot of things that I haven't yet been able to answer:

* If a project would like to supply information about itself for a kind of task that is not part of the set of tasks Forge currently knows about, how does the system deal with it?
* Should it be possible to distribute extensions dynamically to other machines that are running the execution engine?
* Since the execution engine and dependency manager are decoupled, how is a failure during execution correlated back to the component in the dependency graph?
* How are projects specified by the user?
* How would a project communicate information about its dependencies for things that cannot be managed by Forge?
* For CL specifically, should Forge just be a shim that spins up an external process to control the dependency computation and plan execution in order to avoid projects being loaded contaminating it?

I'm sure more questions will pop up as I go on as well. I tried to steer clear of syntax questions so far, since that's sure to be a debate more about personal taste than anything else, so I'm not too interested in it. If you have any questions or comments on the stuff I've talked about so far, or have ideas of your own about build systems, be sure to [let me know](https://everything.shinmera.com/) or hop onto Freenode/#shirakumo.

Finally, I hope this article doesn't read as much of an incoherent mess as I think it is. Hopefully I'll be able to write some more comprehensible things once I'm further along.

------

<a id="1">[1]</a> This recently broke when the ASDF folks decided to change the public interface by turning `asdf/plan:traverse-action` from a generic function into a regular function. It used to be just another method definition rather than a function replacement.

<a id="2">[2]</a> The reason why I can't modify the system definition of CommonQt is because ASDF might reload the ASD file from disk at any time, which would replace the modified definition, undoing my changes. To prevent that I'd have to hook into that mechanism, so we'd be back at a horrible hack solution again.
