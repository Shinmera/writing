![header](https://filebox.tymoon.eu//file/TVRRNU9BPT0=)  
Welcome to a new article series! I've decided to try to record my thoughts on various matters about game development. This will include observations, discoveries, explanations of existing systems, and proposals for improvements in related projects. Most of this will be directly related to Shirakumo's game engine [Trial](https://shirakumo.org/projects/trial) and the [Treehouse](https://events.tymoon.eu/1) gamedev streams. This first entry is a proposal for a new resource management system in Trial.

Any game engine needs to deal with the management of assets and resources. To eliminate any possibility for confusion, I'll define what I mean by those two terms here:

* A **resource** is a representation of some kind of allocated object. Typically this object resides on the GPU, is costly to allocate, and has a variety of associated properties that determine its state.
* An **asset** is a representation of an external, static object. Typically this object is a file of a particular format, needs to be decoded to be useful, can be very large, and can be very costly to decode.

Right now Trial mushes both of these concepts into one. For a while this seemed like a sane thing to do -- after all, both assets and resources encapsulate some kind of loading logic, represent some kind of thing that you would like to load/allocate into foreign memory, and let stick around until you deallocate it again.

However, as the engine grows more complex and the demands for capabilities rise, this is no longer appropriate. For instance, not every texture you need will be loaded from an image. Frame buffers, complex shader pipelines, and other systems require textures without an external image attached. This has resulted in a hybrid approach, where a texture could take a "spec" as an input which would construct a texture without loading it.

Unfortunately, textures have a lot of properties: width, height, internal format, wrapping, min-filter, max-filter, attachment, depth, mipmap levels, structure, and probably others I'm forgetting about. The spec thus grew into a very weird format that not only didn't suit all needs, but was generally a pain to deal with. Textures aren't the only area where this combination of concerns hurts either. A similar problem arises for meshes and vertex buffers and arrays. There's probably others still that I haven't come across yet.

So, now that we have established that this separation of concerns is indeed a Good Idea™, we need to move on to the actual proposal. I will formulate this using my favourite modelling language, CLOS. Here goes:

```commonlisp
(defclass resource ()
  ())

(defgeneric allocate (resource))
(defgeneric deallocate (resource))
(defgeneric allocated-p (resource))
```

So far so good. The `resource` is a relatively opaque type, which merely gives you operations to allocate, deallocate, or test for allocation. It is so opaque on purpose, to allow it to be as generic as possible. Double allocation or double deallocation should be automatically prevented by the protocol.

```commonlisp
(defclass foreign-resource (resource)
  ((data-pointer :initform NIL
                 :initarg :data-pointer
                 :reader data-pointer)))
```

Most actual resources will represent a foreign data block of some kind, for which this `foreign-resource` subtype is useful. For GL resources for instance, the `data-pointer` would return the integer naming the specific resource.
    
```commonlisp
(defclass asset (resource)
  ((pool :initarg :pool :reader pool)
   (name :initarg :name :reader name)
   (input :initarg :input :accessor input)))

(defgeneric load (asset context))
(defgeneric reload (asset context))
(defgeneric offload (asset context))
(defgeneric loaded-p (asset context))
```

Onward to the `asset`, where things get a bit more complicated. Trial employs something called "asset pools" that group assets under a common name. Each pool is associated with a base pathname, from which every asset within it is derived. This allows writing libraries for Trial that ship their own assets, without having to require the user to take care to properly deploy the assets into their own game when it's time to ship. Each asset also has a unique name within the pool, and an input of some kind. The operations we can perform on an asset should be fairly self-explanatory.

Note that `asset` is a subtype of `resource`. The idea here is that, while resources are useful on their own, assets are not. Every asset is going to translate to loading into some kind of resource. Thus it makes perfect sense to retain the capabilities of the underlying resource with the associated asset type. To illustrate this a bit better, let's create an image asset.

```commonlisp
(defclass texture (foreign-resource)
  (wrapping min-filter mag-filter anisotropy ...))

(defmethod allocate ((image image))
  #| Use glCreateTextures and so forth to allocate. |#)

(defclass image (texture asset)
  ())

(defmethod load ((image image))
  (allocate image)
  #| Load image data and use glTexSubImage* to transfer. |#)
```

In order to back an image we need textures. Textures come with all the wonderful aforementioned properties. We can encode the creation of the texture and the setting of these properties in the allocation of the texture resource. Then, we simply create a subclass of both `texture` and `asset`, inheriting all the texture properties, and gaining the ability to transfer the data into the texture on loading.

This may seem like we're back at the beginning with the mushing of the two concepts, since assets are also resources now. However, remember that the primary ache with that approach was that resources were assets. This is no longer the case. You can now create resources completely independently, and interact with their various properties and requirements in a much more sensible manner. This is in addition to the major advantage that now anything dealing with resources does not have to care whether it is passed an asset or not -- regardless, it will work as expected.

By having the assets be direct instances of resources as well, we also eliminate another layer of indirection we would have to follow otherwise. If the resource had been separate, we would need a slot on the asset to track it, requiring two dereferences to reach the `data-pointer` contents. This might not seem like a big deal, but in tight render loops this sort of thing can add up. Ideally you'd be able to inline the data-pointer, but doing so is quite tricky. I hope to talk about my ideas for that kind of optimisation in a future entry.

I could go into a lot more detail here about the specific background details of how this asset/resource system should be implemented, particularly in the way of invariants, but I think the crux of it should be clear. Once again the fact that multiple inheritance is so easily supported by CLOS makes a very convenient, sensible, and efficient design possible.

I'll update this entry with a link to the relevant code once I have implemented it in Trial.

I have some other ideas for entries in this series ready to go, so hopefully it won't be long before another article rolls out.
