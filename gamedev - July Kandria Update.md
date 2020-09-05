![header](https://filebox.tymoon.eu//file/TWpBeE5RPT0=)  
[Last month](https://reader.tymoon.eu/article/384) I outlined a very rough timeline for the future development of Kandria. In the past month I managed to implement the first two of the tasks listed there, namely some very deep fixes to the game engine, and the improvement of the pathfinding AI. I'll try to boil these changes down to make them easier to understand.

If you're subscribed to the [mailing list](https://kandria.com#subscribe), you should already be familiar with the AI pathfinding problem. If not, I'll give you a freebie this time: you can read the article [here](https://reader.tymoon.eu/article/384). I publish similar articles on developments, backstory, and other things every week on the mailing list, so if you're interested, it's a great way to keep up to date!

What I didn't really touch on in the article is the problem of executing a plan once you've computed it from the navigation mesh. This turned out to be a bit more tricky than I had given it credit for, and took up most of my time. It's working really well now, though, so I think I can move on to actual enemy AI.

<iframe width="560" height="315" src="https://www.youtube.com/embed/nnjG06mBiOo" frameborder="0" allow="encrypted-media; picture-in-picture" allowfullscreen></iframe>

As for the game engine changes, those are more numerous and much more involved still. The engine itself is [open source](https://github.com/shirakumo/trial), and available to anyone. I'll try my best to outline the changes without having to explain everything about Trial, and without having to go too deep into it.

[The first change](https://github.com/Shirakumo/trial/pull/25) relates to how assets and resources are managed in Trial. A resource here means an abstract representation of something that needs to be manually managed in memory, like textures and vertex data. Previously it used to be the case that assets were special variants of resources that could be loaded from a file. In that system, an `image` asset would be a `texture` resource that could load its contents from a file. This system works fine for many cases, but it breaks down as soon as a file should expand to more than one resource, such as for a 3D model that contains both mesh data and textures.

The new system provides a clear split between assets and resources. Assets are now resource generators that, when loaded, read whatever input data you feed it, and turns it into appropriate resource instances. This solves the previous problem, but introduces a new one: when you need to refer to a resource in order to, for example, set the texture of an object, you now cannot do so anymore before the associated asset is loaded, and this loading should only occur at specific, controlled points in time.

This is where a rare feature of Lisp makes itself very useful: `change-class`. Assets can offer access to the resources it would generate before actually generating them by providing a `placeholder-resource` instance instead. Once the asset is actually loaded, this instance is then changed into one of the appropriate resource type. This allows us to reference resources before loading them, without having to perform any patching after loading, or expensive repeated runtime lookup.

[The second change](https://github.com/Shirakumo/trial/pull/26) relates to the actual loading operation. Previously there was a system that would try to automatically traverse objects to find all referenced assets and resources. This system was convenient, but also slow and... well, to be honest, it just made me uncomfortable. The new system only automatically traverses the scene-graph, for everything else you need to write explicit methods that the resources you need for loading. 

The system also takes care of a problem that was introduced by the new asset system. Since resources can now be placeholders, they won't know their dependencies before their generating asset is loaded. This is a problem when determining the order in which to load assets and resources, since parts of the dependency information is now deferred. The solution adopted so far is that the load order is recomputed when a resource is encountered that used to be a placeholder. This works fine, but might induce a lot of load order recomputations if the initial order is unfavourable. At the moment though I'm not losing any sleep over this potentially slow corner case.

Finally, the new loader also handles failures better. If an error occurs during the load operation, the state can be rolled back smoothly so that the game can continue running. This isn't too useful on a user's machine, but it is very useful during development, so that the game doesn't just crash and you lose whatever you were doing before.

[The third and final big change](https://github.com/Shirakumo/trial/pull/24) relates to the way objects are rendered in the engine. Trial allows creating rather involved pipelines with different passes of shaders. In order to allow a lot of flexibility, these passes need to have control over how objects are rendered, but also which objects are rendered. Previously this was accomplished by a `paint` function that would traverse the scene graph and perform render operations. Transformations such as translations and rotations were accomplished by defining methods on that function that would dynamically bind the transform matrices and change them. However, this system made it very complicated and error-prone when a pass needed to be selective about which objects it should render. It also forced repeated lookup of the shader program appropriate for a given combination of pass and object, which could be quite slow.

The new system separates the scene and the shader passes entirely. In order to run a shader pass that should render objects in a scene, the scene must first be 'compiled' to the shader pass. This compilation would traverse the scene graph and flatten it into a sequence of render actions. These actions would include management of the transform matrix stack, application of necessary transforms, and ultimately the rendering of objects. Selecting which objects to render could be done at this stage as well, simply omitting the actions of irrelevant objects.

This system makes controlling the render behaviour much easier for the user, but is a lot more complex on the engine side, especially when paired with dynamic updates where objects can enter and leave the scene at will. The way it's currently implemented is very much sub-optimal in that sense, mostly because I have not yet figured out a good protocol on how to communicate where exactly the actions of a new entity should be placed in the action sequence. Containers may not always append a new entity at the end, so there has to be a way for the pass to know where to insert. The option of just recomputing all actions of the container may be prohibitively expensive.

There were other, more minor changes all over as well of course, but I think this entry is already long enough as it is. After getting all of these changes to the engine in, I had to go back and fix a ton of things in Kandria to work again. While at it, I also ripped out a bunch of systems that sucked in Kandria itself and replaced them with cleaner, more simplified variants.

All in all this took up pretty much the entire month. I'm done now, though, and pretty happy with the changes, so I should be able to focus on working on Kandria itself again. I've also already begun work on the next big rewrite that's listed: fixing up the sound engine. I'll put that on the back-burner until the next demo release, though.

Anyway, that's it for this month. Hopefully next month will have a lot more Kandria-specific updates!