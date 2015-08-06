![header](http://shinmera.tymoon.eu/public/tumblr_nc481xFman1qhttpto1_1280.jpg)
It's been too long since my last entry. I just haven't had much that I felt safe talking about. But, now that I'm mostly done with everything that occupied me for a while (Radiance and Purplish), I have more time available for other things. One of these things happens to be [Parasol](http://blog.tymoon.eu/article/257).

<img style="position:absolute;max-width:180px;right:0;" src="http://shinmera.tymoon.eu/public/46211445_p0.jpg" />As you may or may not know, Parasol is a Common Lisp painting application that was born out of curiosity and wasn't really meant to be anything big, but then quickly exploded in size. But as it is with these things, at some point I hit a big roadblock: It couldn't process events fast enough and was thus not getting enough feedback from the tablet pen. This happened because the drawing operations took too long and clogged up the event loop.

To solve this problem, we need to put the operations that take a long time into a separate thread and queue up events while it runs. I tried to hack this in, but the results were less than fantastic. It could go either one of two ways; either the entire CL environment had a segmentation fault at a random time, or the painting would be littered with strange drawing artefacts.

The only thing that I could guess from this was that Qt wasn't happy with me using CL threads. But that wasn't the only issue. I really didn't like what I had built over the weeks of working on Parasol, as it reeked of patchwork design, without much of an ulterior architecture. So I put the project to rest for the time being, hoping to return to it some day and rewrite it anew.

In preparation for this day I recently wrote [Qtools](http://shinmera.github.io/qtools/), a collection of utilities that should make working with Qt easier. Writing that library alone caused me quite some amounts of grief, so I'm not very enthusiastic about diving deeper into the sea of tears that is C++ interaction.

Regardless, I couldn't keep my mind off of it and used some lecture time yesterday to write together a [crude design document](https://twitter.com/Shinmera/status/529253441772994560) that should lay out a basic plan of action and architecture for the new Parasol version. I have started to set this plan into motion today.

First in line is building a skeleton main window with a pane for "gizmos" and a main area that can hold a range of documents or other forms of tabs. With that I'll have a minimal environment to test things out on. After that there's a large chunk of internal structure that needs to be completed: the document representation.

As I've laid it out so far, a document is composed of a set of layer-type objects, a history stack, and metadata such as the size and file associated with it. Layer objects themselves are composed of a position, size, drawables list, and an image buffer. A drawable itself is not specified to be anything fixed. Unlike before, it does not have to be a stroke, but simply an object that has the general methods a drawable has. This allows me to later add things that behave differently, such as images and 3D objects.

This change away from Parasol's original model necessitates two further, drastic alterations: First we need to have tools aside from a general brush that allow manipulating other kinds of objects, so we need to have a way to define 'tools' and their effects when used, in a uniform fashion. Secondly, we need a proper, generalised history that allows us to un/re-do any kind of operation on the document. Both of those things offer significant design challenges and I really hope I can pull it off.

Fortunately however, integrating these is a long while off, as after implementing a basic document I first need to add a way to publish these --so far purely virtual-- documents to the UI. To tie these two worlds together, we need a view object that defines where to and how we're currently looking at the document. The view's job will also be to relay input events, such as tablet or mouse movement, to the actual document (and preserve the correct coordinate translation while doing so).

At this point I'd like to quickly talk about how I'm intending on solving the issue that initially brought Parasol to halt. After thinking things over I came to the realisation that my previous attempt of adding complex splines to the strokes to make smooth lines possible was an artefact of never getting enough tablet input to begin with. With enough events, a simple linear interpolation is a feasible approache. Knowing this it becomes apparent that we do not need to precalculate the spline's effective points (as linear interpolation is dirt cheap). From this it follows that I do not need to put the actual stroke updates into a separate thread, but can simply add points to the stroke in the event loop. The only thing that does have to be outsourced is the drawing of the buffers in the document.

In order to solve the drawing artefacts problem that arose in my previous attempt, I thought I'd take a look at a solution of threading from Qt out. After all, it might just be that Qt isn't happy with being called from threads it doesn't know about. After looking around I found out that the library CommonQt uses to bind to Qt (smokeqt) does not include these classes by default, even though they could be parsed.

Adding these classes to the library was easy enough, a simple XML config change and a recompilation made it available. But whether it actually worked or not was a different question. At first it seemed that it would not work at all. A callback from a QThread back into lisp would always cause [SBCL](http://www.sbcl.org/) to segfault, which was a rather devastating sign. Fortunately enough it seems that [CCL](http://ccl.clozure.com/) instead works just fine with foreign threads. Halleluyah!

I haven't tested whether everything works just fine and dandy with this idea yet, but hopefully I'll get to that soon enough. However, threading always comes at the price of an immense complexity increase. Object access needs to be carefully synchronised with mutexes or other techniques. Thanks to the fact that interaction between the two threads is minimal, this doesn't pose too big of an issue. Once the drawing thread is done it only needs a single atomic call to set the buffer of the object to its finished value and otherwise it only needs to read things from vectors, where it won't make a big difference if it misses an element or two ahead.

Or at least so I hope. I'm sure that I'll get plenty of headaches with threading once I get to it. For now I'll be content with imagining that it'll all work out perfectly fine.

So, when this is all well and done I can move on to writing the architecture for the tools and history operations and with that the general brushes tool, which I hope I can mostly copy over from the previous version.

With image operations, threaded drawing, history and document presentation all sorted out the final step will then be to make a front-end for it all: Layer management, colour picking, brush configuration toolbar, a pluggable gizmos panel and so on and so forth.

If all goes according to my dreams I'll end up with a painting application that is extensible in every aspect imaginable, exhibits the most powerful user-configurable brush-engine I've seen, offers support for integrating and rendering 3D models as well as plenty of other constructs, and allows rudimentary image manipulation.

All of this is left up to your and my imagination for the time being, but I certainly hope to make it a reality at some point.

I might talk more about my progress as I advance, but maybe I'll keep on thinking I have nothing to talk about, irregardless of how much truth there is to that in actuality.

...

Until the sun shines again.
