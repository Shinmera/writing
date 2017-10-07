![header](https://filebox.tymoon.eu//file/TVRRek1BPT0=)  
This is a listing of projects that I've started, some of which I've completed. The intent is to spread awareness about the work I've done, as I speculate that a lot of people don't know about most of it, even though it might prove useful to their own projects. So, hopefully this article will help a bit in that regard.

I won't go into much detail in the descriptions, as that would take too much of both your and my own time. You can however click on the title of each project to get to its "homepage" if you want to find out more. Naturally, you're also free to [contact me](https://everything.shinmera.com/) if you're interested.

## Major Projects
Major projects are ones that have no completion in sight. There are always improvements and additions that could be made to improve the project. Generally they serve as a launch pad for other, minor projects that are completable.

### [Lichat](https://shirakumo.github.io/lichat-protocol/)
Lichat is an attempt at a simple, light-weight chat protocol. There's currently full implementations of the protocol available that allow you to host a TCP or WebSockets server, and to write clients in JavaScript and CL. A Java library, with the intent of writing an Android client is planned.

### [Maiden](https://shirakumo.github.io/maiden/)
Maiden is an event coordination framework and bot construction toolkit. It includes a plethora of pre-made modules to provide common chat bot functionality, as well as a couple of different backends for various chat protocols.

### [Parasol](https://github.com/shinmera/parasol)
Parasol is a native painting application for graphics tablet users. It has been dormant for some years now as I've still not figured out a good way to architecture everything.

### [Portacle](https://portacle.github.io/)
Portacle is the portable development environment for Common Lisp. It gives you an easy-to-deploy, contained development studio that you can use to program. It is especially suited and geared towards beginners that have not used Lisp before, but it's also convenient for quick setups on unconfigured machines.

### [Radiance](https://shirakumo.github.io/radiance-homepage/)
Radiance is a web application environment, allowing you to easily deploy and run different web applications in the same lisp instance, increasing sharing of common resources. This article is actually hosted on an application running in Radiance. Imagine that!

### [Trial](https://github.com/shirakumo/trial)
Trial is an OpenGL game engine with a heavy focus on modularity. It is supposed to provide a large toolkit of useful bits and pieces from which you can create a game. I use this engine together with some of my co-conspirators to write games for the Ludum Dare game jam. Hopefully the engine will at some point also give birth to bigger games.

## Minor Projects
These projects all either spawned out of the requirements of the major projects, or happened simply for fun. Most of them are already complete and thus ready for use.

### [3d-matrices](https://shinmera.github.io/3d-matrices)
A library implementing common matrix calculations, with an emphasis on 2x2,3x3, and 4x4 matrices as commonly used in graphics. It provides some numerical functions as well, but those are not the focus. The library is heavily optimised, so it is not made of pretty code.

### [3d-vectors](https://shinmera.github.io/3d-vectors)
This is the counter-piece to 3d-matrices, providing vector operations optimised for 2, 3, and 4-component vectors. Also just like 3d-matrices, the library is heavily optimised and thus not pretty on the eyes.

### [CLSS](https://shinmera.github.io/CLSS)
CSS-Like Simple Selectors implements a DOM search engine using the CSS selectors as the query format. It is reasonably optimised, but only usable with the DOM provided the Plump system.

### [LASS](https://shinmera.github.io/LASS)
Lisp Augmented Style Sheets is a compiler for a custom CSS syntax. It allows you to write CSS files in a much more convenient and homely-feeling syntax. I've been using this to write pretty much all of my CSS for the past couple of years.

### [array-utils](https://shinmera.github.io/array-utils)
A small library to provide vector manipulation functions that are sorely missing from the standard. Allows to push to any place in the array while maintaining the proper shifting logic.

### [autobuild](https://shinmera.github.io/autobuild)
A Continuous Integration system with a focus on running directly on your machine, rather than in a container or otherwise segregated environment. This is currently being rewritten from scratch.

### [chatlog](https://shinmera.github.io/chatlog)
A Radiance application for a web interface to a chatlog database. The database is recorded through the Colleen or Maiden chatlog modules.

### [chatter](https://shinmera.github.io/chatter)
A chat application based on the Twitter direct messages system. Allows you to chat directly with your twitter friends as if it were a regular chat room. Easy to set up, and runs on all major desktop platforms.

### [chirp](https://shinmera.github.io/chirp)
A client library implementing the full Twitter REST API. If you want to interact with Twitter, this is your best bet.

### [cl-fond](https://shirakumo.github.io/cl-fond)
A bindings library to libfond, allowing you to use its functionality easily from Lisp out. Libfond allows the rendering of TrueType fonts onto OpenGL textures.

### [cl-gamepad](https://shirakumo.github.io/cl-gamepad)
A bindings library to libstem_gamepad, providing easy gamepad and joystick event processing from Lisp. This is useful if you don't want to use some kind of framework that brings all sorts of other baggage with it, not just gamepad processing.

### [cl-gpio](https://shinmera.github.io/cl-gpio)
A wrapper library for the Linux General Purpose IO device present on embedded systems such as the Raspberry Pi. It allows you to conveniently access and control the IO pins on the board.

### [cl-k8055](https://shinmera.github.io/cl-k8055)
A bindings library for the k8055 analog input board. Allows you to read its various values and set outputs.

### [cl-mixed](https://shirakumo.github.io/cl-mixed)
A bindings library to libmixed, allowing you to use its functionality from Lisp. Libmixed allows you to perform digital audio mixing and processing. Thus, with this, you can do efficient DSP from Lisp.

### [cl-monitors](https://shirakumo.github.io/cl-monitors)
A bindings library to libmonitors, providing convenient access to information about the currently attached monitors, and giving you the ability to control the resolution thereof.

### [cl-mpg123](https://shirakumo.github.io/cl-mpg123)
A bindings library to libmpg123, giving you fast and easy to use MP3 decoding. This covers the complete API exposed by libmpg123, and thus easily your best bet for MP3 processing.

### [cl-out123](https://shirakumo.github.io/cl-out123)
A bindings library to libout123, giving you cross-platform audio output. The API is very simple to use, and can thus give you a quick start if you need to play some audio.

### [cl-soloud](https://shirakumo.github.io/cl-soloud)
A bindings library to SoLoud, an open source C++ sound engine for the use in video games. I've completed this, but dropped it, as it was too hostile to extension from CL. I've since developed Harmony (see below).

### [cl-spidev](https://shinmera.github.io/cl-spidev)
A wrapper library for the Linux Serial Port Interface device. With this you can do serial port input/output, which is present on some embedded devices like the Raspberry Pi.

### [clip](https://shinmera.github.io/clip)
Clip is an alternative approach to templating, expressing the template within valid HTML. This allows a different development approach, wherein you can hammer out a mock-up for a website in an HTML document, and then simply add templating logic through further tags and attributes, maintaining browser-viewability.

### [colleen](https://shinmera.github.io/colleen)
This is the predecessor to Maiden, with a more narrow focus and feature base. Since it has been superseded, and the code is quite crusty, I heavily urge you to look at the Maiden project instead.

### [crypto-shortcuts](https://shinmera.github.io/crypto-shortcuts)
A tiny library to provide commonly used cryptography functions in a more accessible format, as some of the tools provided by Ironclad & co. can be a bit cumbersome to use.

### [deeds](https://shinmera.github.io/deeds)
Deeds is an Extensible and Event Delivery System. It offers both flexible and performant creation of event systems. Deeds is used heavily in Maiden.

### [deferred](https://shinmera.github.io/deferred)
This was an attempt at making optional dependency wrangling more convenient. It gives you a few tools that attempt to make it possible to write code that is only considered once another system becomes available.

### [deploy](https://shinmera.github.io/deploy)
With Qtools I developed a very convenient mechanism to generate deployments of my systems. This is the evolution of that, allowing you to use it independent from Qt. It takes care of your foreign libraries and the general shutdown and boot sequence, making the whole binary deployment process much smoother.

### [dissect](https://shinmera.github.io/dissect)
Sadly a lot of projects use the "trivial-backtrace" system that just gives them a string with a backtrace. Dissect allows you to capture, step, and completely inspect the stack trace on a variety of Lisp implementations. The introspection abilities allow you to write a good chunk of a portable debugger. It's also very useful for logging and other situations where execution is automatically continued, but the information of the current stack is still useful to store somewhere.

### [documentation-utils](https://shinmera.github.io/documentation-utils)
I like to keep my code nice and clean, and as such docstrings are quite cumbersome clutter. This library allows you to easily and conveniently put all the docstrings in a file outside of the rest of your code.

### [filebox](https://shinmera.github.io/filebox)
This is a Radiance application that provides you with a very simple file storage. Coupled with the filebox-client, you get a Dropbox-like system.

### [flare](https://shinmera.github.io/flare)
Flare is a particle simulation framework. Unlike most particle systems, it does not focus on the emission of small dots, but rather on the precise coordination of a hierarchy of entities. It allows you to describe sequences of events, and then lets you play those sequences back, performing the actual transformations. You can even rewind time and interactively work on your sequences.

### [flow](https://shinmera.github.io/flow)
This is a flowchart-like graph library. It gives you access to nodes which, unlike in mathematical graphs, have dedicated ports from which connections are made. These ports can have semantic meaning and syntactic properties. Thus, this gives you a toolkit to make flowchart-like graphs and compute with them.

### [for](https://shinmera.github.io/for)
Since I couldn't come to terms with Iterate, I decided to write my own extensible iteration construct. Unlike Loop or Iterate, For has a particular syntax to it that makes extensions feel much more integrated.

### [form-fiddle](https://shinmera.github.io/form-fiddle)
This small library allows you to wrangle lambda forms and destructure them into their individual components (docstring, declarations, arguments, etc).

### [glsl-toolkit](https://shinmera.github.io/glsl-toolkit)
A toolkit to allow you to process and manipulate OpenGL Shader Language code. It includes a full GLSL4 parser, printer, and code-walker. Using this you can even do stuff like merge separate shaders together automatically, preserving input/output semantics.

### [halftone](https://github.com/Shinmera/halftone)
A sample application for the Qtools system, providing you with a minimal, but pretty image viewer. Works on all major desktop platforms.

### [harmony](https://shinmera.github.io/harmony)
Harmony is a fully-fledged audio system, allowing you to control playback of multiple sources, and even to position them in 3D space. It also allows you to build your own sound processing pipelines, to add in effects and other sound processing capabilities.

### [humbler](https://shinmera.github.io/humbler)
This is a client library for the Tumblr REST API. It has full coverage of the documented features and properly wrangles all the oddities and inconsistencies of the API for you.

### [kanji-tree](https://github.com/Shirakumo/kanji-tree)
This is still in the works, but is intended to become a website (using Radiance) that provides useful information about Kanji, as well as an optimised sequence by which to learn them. Hopefully this will help me and other people to learn Japanese.

### [keyword-reviews](https://github.com/Shirakumo/keyword-reviews)
Another Radiance application that provides a very minimalist site for product reviews. The twist of this site is that your review should be very short, if possible reduced to keywords only. The idea is that this should make for interesting descriptions and interpretations.

### [lambda-fiddle](https://shinmera.github.io/lambda-fiddle)
The counterpart to form-fiddle, this allows you to wrangle and destructure lambda-lists (argument lists).

### [legit](https://shinmera.github.io/legit)
An interface to the Git binary. Using this library you can run *all* the available Git commands using a more convenient and streamlined function interface. An object-oriented high-level interface is also available, but doesn't cover the full API.

### [libfond](https://github.com/Shirakumo/libfond)
A small C library to allow you to render TrueType fonts to OpenGL textures. Text rendering is something that's often left out of minimal game engines, and so libfond can provide you with that aspect.

### [libmixed](https://github.com/Shirakumo/libmixed)
A small C library to allow you to mix and process digital audio. It is reasonably optimised and comes with a set of processing and mixer components out of the box. One of the components also allows you to integrate LADSPA plugins, so you can use those directly as well.

### [libmonitors](https://github.com/Shirakumo/libmonitors)
A small C library to handle the management and information retrieval of connected Monitors. Currently Linux, Windows, and OS X are supported.

### [lionchat](https://shinmera.github.io/lionchat)
A native GUI for the Lichat system. While this works well enough as it currently stands, I'd like to rewrite it at some point to use Maiden, and thus allow connecting to other chat systems as well.

### [lquery](https://shinmera.github.io/lquery)
A library modelled after jQuery to allow you to conveniently and succinctly wrangle HTML and XML documents. This is Particularly useful for web scraping tasks.

### [modularize](https://shinmera.github.io/modularize)
This is a system that gives you an extension to the package system, by allowing you to add other metadata to it. This should facilitate the construction of "modules," individual components of a larger system. The metadata can be used to give meaning to the modules and model their relations to the whole system.

### [modularize-hooks](https://shinmera.github.io/modularize-hooks)
This augments the modularize system by giving you hooks and triggers. Thus, modules can provide opaque entry points for other modules to provide additional functionality.

### [modularize-interfaces](https://shinmera.github.io/modularize-interfaces)
This augments the modularize system by giving you "interfaces"-- contract-like descriptions of the functionality provided through a package. While the interface description is abstract and only includes the signatures of functions, another module can then opt to implement the actual functionality behind the interface.

### [north](https://shinmera.github.io/north)
The successor to the South (Simple OaUTH) library, implementing the full oAuth 1.0a protocol, both client and server sides. Using North you can easily become an oAuth provider or consumer.

### [parachute](https://shinmera.github.io/parachute)
Parachute is a testing framework with an emphasis on being extensible. As proof of this, it includes "compatibility layers" for a couple of other popular testing frameworks. Using such a layer you can immediately convert to using Parachute by just changing a package :use and system :depends-on.

### [pathname-utils](https://shinmera.github.io/pathname-utils)
A small library to help with common pathname wrangling tasks. If you need to work with pathnames a lot, you'll probably find one or two things in here that will prove useful to you. Note that the library is restricted to pathnames, so it won't provide anything that actually touches the file system.

### [pi-plates](https://shinmera.github.io/pi-plates)
A library implementing the public interface for the PiPlates DAQ plates that you can use in combination with the Raspberry Pi. The library is currently untested, but "should work" as it is a fairly straightforward translation of the official Python code. I haven't yet found the will to actually test it myself.

### [piping](https://shinmera.github.io/piping)
Piping allows you to write "pipelines." Pipelines are systems of pipe segments that pass along and transform or compute based on the object currently being passed through the pipeline.

### [plaster](https://github.com/Shirakumo/plaster)
Plaster is another Radiance application. It gives you a usable paste service. The [Radiance tutorial](https://github.com/Shirakumo/radiance-tutorial/blob/master/Part%200.md) even shows you how to write the application from scratch.

### [plump-bundle](https://shinmera.github.io/plump-bundle)
An implementation of a binary storage format for the Plump DOM. It allows you to save a DOM into a more efficiently parseable representation on disk.

### [plump-sexp](https://shinmera.github.io/plump-sexp)
A parser and printer for an s-expression based syntax of an HTML DOM, using the Plump DOM as a back-end.

### [plump-tex](https://shinmera.github.io/plump-tex)
A parser and printer for a TeX based syntax, using the Plump DOM as a back-end. With this you can parse TeX sources into a DOM and render them as HTML.

### [plump](https://shinmera.github.io/plump)
A Practically Lenient and Unimpressive Markup Parser for XML and HTML documents. It provides a fast and lenient parser, allowing you to chuck all sorts of malformed data at it. Since it integrates with a bunch of my other systems, it's a pretty good choice for HTML.

### [post-all](https://shinmera.github.io/post-all)
This is a small library to allow you to post content to multiple services at once. I use this to post my art online, as there's a couple of different places I'd otherwise have to upload to manually every time.

### [purplish](https://github.com/Shirakumo/purplish)
Purplish is yet another Radiance application. It provides you with a slick and simple image board software. If you ever want to run a chan, this could be a good choice.

### [qt-libs](https://shinmera.github.io/qt-libs)
This system provides you with the Qt4 library binaries. Usually all you have to do is load this system, and you'll be set up and ready to go writing Qt applications. It also includes tools to automatically build the libraries from scratch.

### [qtools](https://shinmera.github.io/qtools)
Qtools allows you to write Qt GUIs in a syntax and manner much more similar to how you'd write any other Lisp code. It provides all sorts of conveniences and abstractions to make life a whole lot easier.

### [qtools-ui](https://shinmera.github.io/qtools-ui)
This is a collection of UI components and systems that Qt does not provide on its own. Being completely written in Lisp, it is also ripe for extension and adaptation in your own projects. If you have a reusable component that you wrote, it would be a great idea to integrate it here, so that others can benefit as well.

### [random-state](https://shinmera.github.io/random-state)
Random-state gives you access to a bunch of different random number generation algorithms, and allows you to portably seed them. This is primarily useful where number generation needs to be controllable.

### [ratify](https://shinmera.github.io/ratify)
This system allows you to verify and parse a variety of string-based formats. It is primarily geared towards validating input from web forms, or other unauthorised sources.

### [reader](https://github.com/Shirakumo/reader)
A Radiance application providing you with a simple blogging platform with tags and Atom feeds. This article was published on Reader!

### [redirect-stream](https://shinmera.github.io/redirect-stream)
A tiny library implementing a gray stream that redirects the output written to it to another stream. This is useful when you want to switch out the stream of a particular system on the fly.

### [simple-tasks](https://shinmera.github.io/simple-tasks)
A small library to do simple task issuing and processing. You create tasks that execute some code, and then send them off to be processed on a dedicated background thread.

### [trivial-arguments](https://shinmera.github.io/trivial-arguments)
This gives you a single function, which returns the lambda-list of a function, if the list is known. Useful for introspective and descriptive tasks.

### [trivial-benchmark](https://shinmera.github.io/trivial-benchmark)
A small library to do simple benchmarking work. This library uses CLOS to be easy to extend, which incurs a bit of overhead for the benchmarks themselves. Thus, it is sadly not suitable for micro-benchmarking.

### [trivial-indent](https://shinmera.github.io/trivial-indent)
If you make a macro with a bit of a more advanced syntax, it's likely Slime will not pick up the proper indentation for it. With this, you can help it out by declaring the proper indentation form manually.

### [trivial-main-thread](https://shinmera.github.io/trivial-main-thread)
Sometimes it's necessary to ensure that code is run in the main thread, especially when you want to do graphics on OS X. This library helps you with that.

### [trivial-mimes](https://shinmera.github.io/trivial-mimes)
The detection and handling of mime-types for files is sometimes necessary to validate the correctness of a specified content type. This library implements both a binary lookup, and a file-name lookup.

### [trivial-thumbnail](https://shinmera.github.io/trivial-thumbnail)
This tiny library uses the ImageMagick binaries to create thumbnails of images for you.

### [ubiquitous](https://shinmera.github.io/ubiquitous)
Ubiquitous provides a persistent configuration storage. It gives you convenient traversal through the configuration and offers easy file-based serialisation for a good range of Lisp object types. If you need your application to be configurable through external files, or just need a simple storage, check a look!

## Other Stuff
That's about it. I have a bunch of other projects that I haven't mentioned here, either because they're old, abandoned, not even close to finishing, or simply uninteresting.

Since I'm constantly doing things, this list is bound to become outdated before long. So, please be mindful of the date. When in doubt, just look at the project's own page, or [contact me directly](https://everything.shinmera.com/). I love to chat, so even if you don't care about any of this, I definitely wouldn't mind if you stopped by at #shirakumo on Freenode some time.
