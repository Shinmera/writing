![header](https://filebox.tymoon.eu//file/TVRVd013PT0=)  
In the last entry I talked about Trial's way of handling shaders and how they are integrated into the object system to allow inheritance and composition. This time we'll take a look at how that system is extended to allow entire pipelines of effects and rendering systems.

Back in the olden days you could get away with a single pass to render everything. In fact, the fixed-function pipeline of GL 2.1 works that way, as do many simple rendering systems like primitive forward rendering, or most 2D games. However, more complex systems and effects require multiple passes. For instance, a deferred rendering system requires at least two passes, one that emits a "gbuffer", and another that combines the information from it to produce the final image. Another example would be a "god-rays" effect, which requires rendering the scene with all objects black, applying a radial blur to that, and then finally blending it with a rendering of the scene as normal.

These kinds of pipelines can become quite complex, especially when you start to consider that each pipeline might require multiple kinds of textures to use as buffers, inputs, and outputs. The management and allocation of these resources is a pain to do manually, so Trial includes a system to reduce the process to a graph definition. This allows you to write complicated pipelines without having to worry about any of the underlying buffers. The pipeline part is only one hand of the equation, but we'll get to the rest later.

To do the rendering, a stage will require a set of textures to output to, and a set of textures as inputs to the stage. A stage might offer additional parameters aside from that to tweak its behaviour, but those do not require allocation, so we can disregard them for now.

To give a bit of context I'll explain how these textures are attached to the shaders in OpenGL. I've glossed over that in the previous entry as it wasn't directly relevant. Let's consider a simple fragment shader that simply copies the colour from an input texture to an output texture:

```C
uniform sampler2D input;
out vec4 color;

void main(){
  color = texelFetch(input, ivec2(int(gl_FragCoord.x), int(gl_FragCoord.y)));
}
```

Every GLSL file begins with a set of variable declarations. Each of these global variables has a storage qualifier that tells OpenGL where it comes from. `uniform`s are parameters that can be set from the CPU before the rendering is performed and are the same for all of the stages in the rendering. `out`s are variables that are sent to the next stage. The last important qualifier that isn't mentioned above is `in`, which takes variables from a previous stage. Every file must also contain a `main` function that acts as the entry point for the computation. In this case we perform a texture read at the current fragment position and store it into the output color -- effectively copying it. 

The way in which outputs and inputs differ should become apparent here. Inputs are declared as `uniform`s, which need to be set by the application before each rendering call. The outputs on the other hand are part of the framebuffer target, which is much more implicit. A frame buffer is a special kind of object that points to a set of textures to which the rendering is output. The default frame buffer will draw to your window so that things can actually be seen. This discrepancy between inputs and outputs is a bit of a pain, but it is simply a consequence of how OpenGL evolved. If I had to design a graphics API like this today I would definitely treat input and output buffers in a much more similar manner.

Whatever the case may be, we have the ability to fix this in Trial by giving the user a uniform view over both inputs and outputs to a shader stage. For instance, it would be nice if we could simply say something like the following:

```commonlisp
(define-shader-pass renderer ()
  ((:output color)))

(define-shader-pass blur ()
  ((:input previous)
   (:output color)))
```

Then when we need to define an actual shader pipeline we could just do something like this:

```commonlisp
(let ((pipeline (make-pipeline))
      (renderer (make-instance 'renderer))
      (blur (make-instance 'blur)))
  (connect (output color renderer) (input previous blur) pipeline)
  (pack pipeline))
```

What you see above, you can do in Trial, almost exactly. The syntax is a bit different, but the concept is much the same. The `pack` step analyses the graph formed by the pipeline and automatically allocates the minimal number of texture buffers needed, re-using them where possible. This is especially useful if you have a long chain of post-effects.

As the texture allocation system is implemented right now it's unfortunately pretty suboptimal. There's some severe limitations that I need to fix, but I have a good idea on how to do that, so I'll probably get around to doing that soon. Still, for most of my work so far the current approach has sufficed, and the general idea is clear.

With the pipeline and allocation system in place, one half of the equation is solved. The other half requires a bit more ingenuity. So far the passes we've looked at did one of two things: either simply render the scene to a buffer, or perform some kind of buffer to buffer transformation like blurring. However, most actually interesting shaders will want to do more complex things than that -- they'll want to not only render objects, but also influence *how* they are rendered.

But, hold on, don't the objects themselves decide how they are rendered? And now the pass should suddenly decide what's going on? Well yes, and yes. Some passes will want to completely influence how every object is drawn, some will want to influence how objects are drawn, and yet some will not care how the objects are drawn at all. Allowing this is the second part of the shader pipeline protocol. As you know, modelling protocols with CLOS is my favourite thing to do, so strap in.

```commonlisp
(define-shader-entity shader-pass () ())

(defgeneric register-object-for-pass (pass object))
(defgeneric shader-program-for-pass (pass object))
(defgeneric make-pass-shader-program (pass class))
(defgeneric coerce-pass-shader (pass class type spec))
```

The above class definition is not quite accurate, but for simplicity's sake I've shortened it to something that will be clear enough for our purposes here, and will link back to the previous entry appropriately. Each shader pass is a shader entity, and as such allows you to attach shader code to the class definition.

The first function, `register-object-for-pass`, is used to notify a shader pass about an object that would like to draw in the current scene. This is necessary for any shader pass that would like to influence the drawing of an object, or leave it up to the object entirely. In both of those cases, the pass will compute a new effective shader for the object and compile that into a shader program to use. This program can then be retrieved by the object with `shader-program-for-pass`. Thus, whenever an object is painted onto a stage, it can use this function to retrieve the shader program and set the necessary uniforms with it. Stages that would like to supplant their own rendering method can simply ignore these two functions.

The `make-pass-shader-program` function is responsible for combining the shader programs from the given class and pass, and to produce a useful shader program resource object as a result. This function, by default, simply does some book keeping and calls `coerce-pass-shader` to do the actual combination. `coerce-pass-shader` in turn merely retrieves the corresponding shader code from the pass and once again uses glsl-toolkit's `merge-shader-sources` to combine it with the existing shader.

That might be a bit much to put together in your head, so let's look at an example. We'll have an object that simply draws itself textured, and a shader pass that should apply a sort of fade out effect where things farther away become more transparent.

```commonlisp
(define-shader-entity cube (vertex-entity textured-entity) ())

(define-shader-pass fade (render-pass) ())

(define-class-shader (fade :fragment-shader)
  "out vec4 color;
  
void main(){
  color.a *= (1 - gl_FragCoord.z);
}")

(defmethod setup-scene ((main main) scene)
  (enter (make-instance 'fade) scene)
  (enter (make-instance 'cube) scene))
```

Similar to how we did it in the previous entry, we define a new entity that is composed of vertices and has textured surfaces. I omitted some details like what vertex data and textures to use to make it simpler. Then we define a new shader pass, which is based on the existing `render-pass` that is a `per-object-pass` with a default color and depth output. Next we have to add the shader logic to our pass. Doing what we want is actually quite simple -- we simply multiply the colour's alpha value with the depth value. The depth needs to be inverted, as by default `1` is far away and `0` is close by. Thus, with this shader, the farther away the fragment is, the more transparent it will now become.

Finally we define a method that hooks into the rest of Trial and establishes a scene to be drawn. We first enter a new instance of our fade pass into it, which will automatically get added to the underlying shader pipeline. We then add an instance of the cube to it, at which point several things will happen:

1. The cube is added to the scene graph
2. `register-object-for-pass` is called on the pass and cube instances.
3. This will call `determine-effective-shader-class` on the cube. This function is used as a caching strategy to avoid duplicating shaders for classes that don't change anything from their superclass.
4. If the determined class is not yet registered, `make-pass-shader-program` is called.
5. `make-pass-shader-program` computes the effective shader definitions for each shader type.
6. `coerce-pass-shader` is called for each type of shader definition that is used, and combines the object's shader source with that of the pass.
7. The resulting shader program resource object is returned and stored in the pass.

The fragment shader produced by `coerce-pass-shader` will look something like this:

```C
in vec2 texcoord;
out vec4 color;
uniform sampler2D texture_image;

void _GLSLTK_main_1(){
  color *= texture(texture_image, texcoord);
}

void _GLSLTK_main_2(){
  color.a *= (1 - gl_FragCoord.z);
}

void main(){
  _GLSLTK_main_1();
  _GLSLTK_main_2();
}
```

Again, this neatly combines the two effects into one source file that can be transparently used in the system. Once it comes to actually drawing the scene, something like the following course of events is going to take place:

1. `paint` is called on the scene with the pipeline.
2. `paint-with` is called on the fade pass with the scene.
3. `paint` is called on the cube with the fade pass.
4. `shader-program-for-pass` is called on the fade pass and cube. The previously cached program is retrieved and returned.
5. The `textured-entity` part of the cube uses the shader program to set the texture uniform.
6. The `vertex-entity` part of the cube uses the shader program to set the transformation matrices.
7. The `vertex-entity` part of the cube calls the OpenGL vertex drawing function to finally run the rendering process.
8. The color output from the fade pass is blitted onto the window for the user to marvel at.

And that is pretty much the entire process. Obviously some steps will be repeated as needed if you add more objects and passes. The pipeline system ensures that the passes are run in the correct order to satisfy dependencies. In case you're wondering about the `paint` and `paint-with` inversion, the latter function is useful so that the shader pass gets to have absolute control over what is rendered and how if it so desires. For instance, it could completely ignore the scene and draw something else entirely.

That about concludes the explanation of Trial's shader system as it stands today. As I've mentioned over these two entries there are a few minor places where improvements have to be made, but overall the design seems quite solid. At least I hope I won't suddenly realise a severe limitation somewhere down the line, forcing me to rethink and rewrite everything again.

Next I think I will take a bit of a break from talking about engine details, and instead talk about geometry clipmaps and the adventure I had trying to implement them. Until then~
