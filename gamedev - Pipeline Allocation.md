![header](https://filebox.tymoon.eu//file/TVRVeE5BPT0=)  
This is a follow-up to my previous entry, where I talked about the shader pipeline system. However, I specifically excluded the way resources for shaders are allocated in that article. We'll get to that now, since I finally got around to rewriting that part of the engine to be more complete.

As you may recall, each rendering step is segregated into a shader stage. This stage may either render all on its own, render a scene directly, or influence the render behaviour of the scene. However, each stage will need buffers to draw to that can act as the inputs to future stages. This means that we have an allocation problem on our hands: ideally we'd like to minimise the number of buffers needed to render the entire pipeline, without having any of the stages accidentally draw into a buffer it shouldn't.

Let's look at the following pipeline, which consists of five stages:

![pipeline](https://filebox.tymoon.eu//file/TVRVeE1nPT0=)

The first stage renders the scene as normal, and produces a colour and depth buffer as a result. The second stage renders the scene but every object is drawn pitch black. It only produces a colour buffer, as depth is irrelevant for this. We then blend the two colour buffers together while radially blurring the black one. This results in a "god ray" effect. The next stage only renders transparent objects. This is usually done in order to avoid having to perform expensive transparency calculations on opaque objects. Finally the two stages are rendered together, using the depth and alpha information to blend. This produces the final image.

In a naive implementation, this would require seven buffers, five colour buffers and two depth buffers. However, you can do with just three colour buffers instead, by re-using the colour buffers from the first two stages for the fourth and fifth stages. This kind of allocation problem, where a graph consists of nodes with discrete input and output ports, has cropped up in multiple places for me. It is a form of a [graph colouring](https://en.wikipedia.org/wiki/Graph_coloring) problem, though I've had to invent my own algorithm as I couldn't find anything suitable. It works as follows:

1. The nodes in the DAG are topologically sorted. We need to do this anyway to figure out the execution order, so this step is free.
2. Count the number of ports in the whole graph and create a list of colours, where each colour is marked as being available.
3. Iterate through the nodes in reverse order, meaning we start with the last node in the execution order.
4. For each already coloured port in the node, if the port is noted as performing in-place updates, mark the colour as available. This is never the case for OpenGL textures, but it is useful for other allocation problems.
5. For each input port in the node:
    1. If the port on the other side of the connection isn't coloured yet, pick the next available colour from the colour list.
    2. Set this colour as the colour of the other port.
    3. Mark the colour as unavailable.
6. For each output port in the node:
    1. If the port isn't coloured yet, pick the next available colour from the colour list.
    2. Set this colour as the colour of the port.
    3. Mark the colour as unavailable.
7. For each port in the node:
    1. If the node is coloured, mark the colour as available again.

This algorithm needs to be repeated separately for each incompatible buffer type. In our case, we need to run it once for the depth buffers, and once for the colour buffers. Since the algorithm might be hard to understand in text, here's a small animation showing it in action:

![allocation](https://filebox.tymoon.eu//file/TVRVeE13PT0=)

As far as I can tell, this algorithm performs the optimal allocation strategy in every case. I don't think it's amazingly fast, but since our graphs will be relatively small, and won't change very often, this is a negligible cost.

Unfortunately this allocation is not everything yet. In order to be able to figure out which buffers are compatible, we need to perform another analysis step beforehand. Namely, we need to figure out which texture specifications are "joinable". Some shader passes might have special requirements about their buffers, such as specific resolutions, internal formats, or interpolation behaviour. Thus, in order to know whether two buffers can actually potentially be shared, we first need to figure out whether the requirements can be joined together. This turns out to be a tricky problem.

Before we can get into this, I think it's worth explaining a bit more about how this stuff works in OpenGL. Two components are essential to this, namely textures and frame buffers. Any OpenGL context has a default frame buffer, which is typically the window you draw to. This frame buffer has a colour buffer, and a depth and stencil buffer if you activate those features. However, since the output of this frame buffer is internal to OpenGL, you can't capture it. If you need to re-process the contents, you'll have to use custom frame buffers. Each frame buffer has a number of attachments, to which you need to bind fitting textures. When the frame buffer is bound, a rendering call will then draw into the bound textures, rather than to the window.

Previously Trial simply automatically determined what kind of texture to use based on the window dimensions and the attachment the output was supposed to be for. Thus allocation was trivial as I just needed to run it once for each kind of attachment. However, this is not optimal. Certain effects, such as [HDR](https://en.wikipedia.org/wiki/High-dynamic-range_imaging) require other features in textures, such as a specific internal format. Once you give the user the ability to completely customise all texture options things become difficult. Textures have the following attributes:

* `width` The width of the texture. This applies to all texture targets.
* `height` The height of the texture. This only applies to 2D, 3D, or array textures.
* `depth` The depth of the texture. This only applies to 3D, or 2D array textures.
* `target` The texture target, which encodes the number of dimensions, whether it's an array, and whether it has multisampling.
* `samples` If it is a multisample texture, how many samples it supports.
* `internal format` The internal pixel storage format. I'll mention this specifically later.
* `pixel format` The external pixel format when loading pixel data into the texture. Closely related to the internal format.
* `pixel type` The external pixel type when loading pixel data into the texture. Closely related to the pixel format.
* `magnification filter` What algorithm to use when the texture is scaled up.
* `minification filter` What algorithm to use when the texture is scaled down.
* `anisotropy` What level of downscaling anisotropy to use.
* `wrapping` How access outside of the normalised texture coordinates is handled.
* `storage` Whether the texture storage can be resized or not.

That's quite a few properties! Fortunately most properties are either trivially joinable, or simply incompatible. For instance, the following are incompatible:

    target, pixel format, pixel type, magnification filter, minification filter, wrapping, storage

And the following are trivially joinable by just choosing the maximum:

    samples, anisotropy

The biggest problems are the dimensions, and the internal format. I'm as of yet undecided what to do with the dimension constraints, since in some cases the constraint the user writes down might just be a lower bound advice and could be upgraded to a bigger size, but in others an exact size might be expected.

The internal format however is the real kick in the dick. OpenGL has a plethora of different internal formats that may or may not be joinable. Here's a list of some:

    red, r8i, r16-snorm, rgb32ui, rgba16f

Not... *too* bad, right? Well, unfortunately that's not all. How about these formats?

    rgba2, rgb5-a1, r3-g3-b2, rgb9-e5, rgb10-a2ui, srgb8-alpha8

Or how about these?

    compressed-srgb-alpha-bptc-unorm, compressed-signed-rg-rgtc2

That's not even all of them, and even worse, the complete list does not cover all possible combinations! For instance, there is no `rgb2` even though there's an `rgba2`. There's also no `r6-g6-b5`, and no `rgb16-e9`, etc. Some of these formats are also simply insane to consider. I mean, how is a `r11f-g11f-b10f` format supposed to work? That's supposedly 11 bits for red and green each, and 10 bits for blue, but as *floats*. **What??**

Putting the insanity of the formats aside, the task of joining them is, fortunately, somewhat clear: a format can be joined with another of equal or greater number of colour components. A format can be joined with another of equal or greater bits for each shared component. A format can be joined with another if the required colour type matches, and the required features such as compression and srgb match. To put this into examples:

* `red`⋁`red` = `red`
* `red`⋁`rg` = `rg`
* `red`⋁`r16` = `r16`
* `rg`⋁`r32` = `rg32`
* `rgb32f`⋁`rgb8i` = ∅
* `compressed-red`⋁`red` = ∅

You could argue that a non-compressed format could be joined with a compressed one, but for now I've taken the stance that, if the user really requests such a thing, the use is probably pretty special-purpose, so I'd rather not muck with it.

In order to do this in a somewhat sane manner that isn't explicitly listing all possible upgrades, I've [written a machinery](https://github.com/Shirakumo/trial/blob/d83cc50669c0345b2cbafe0bdbccc1fcfb614086/resources/texture.lisp#L154-L317) that can decompose the internal formats into a sensible plist that documents the precise requirements of the format. Based on this, a joining can then be performed much more easily. Finally, the resulting joined format is then recomposed into a matching internal format again. This last step is currently not optimal, as sometimes a join might result in an inexistent format, in which case further upgrading of the join might be necessary. However, for my current needs it is good enough, and I honestly doubt I'll ever reach the point where I need to touch this again.

Alright, so now that we know how to join internal formats, we can join texture specifications. Thus, as a step before the allocations, the system gathers all texture specifications from the ports, normalises them, and then repeatedly joins every spec with every other spec, replacing the two inputs with the result if it succeeds, or keeping it if it doesn't. At some point the set of specs won't change anymore, at which point we have the minimal texture spec set.

We then run the algorithm for each spec in this set, only considering ports whose spec can be joined with it. This results in a perfect allocation that respects the texture constraints each pass might have. A last, additional step could be performed here that I do not do (yet): since some ports might not get shared due to the allocation constraints, their texture specs could be relaxed again, so we would need to recompute the join for all ports in the same colouring group.

I've rewritten this system as part of the asset rewrite, since I thought that if I break things I might as well break them hard. Trial is not quite yet back on its feet right now, but I can at least compile a minimal pipeline and run a window, so it's very close to being functional again.

As a next step from here, I think I'll work some more on UI stuff. I know I promised an article on geometry clipmaps, and that is coming, but it'll be another while. Things have been surprisingly busy recently, and I've also been surprisingly lazy as of late. Hopefully both of those will improve soon!
