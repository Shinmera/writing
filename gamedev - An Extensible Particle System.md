![header](https://filebox.tymoon.eu//file/TVRjMk9BPT0=)  
This article was originally published on [GameDev.NET](https://www.gamedev.net/blogs/entry/2267665-seven-weeks-later/). In it, I illustrate a new particle system that was developed for my Lisp game engine, Trial. It contains quite a bit of graphics stuff, but also a lot of Lisp. I thought it would be worthwhile to share it here as well. For those unfamiliar, a particle system deals in orchestrating a lot of very similar things (particles). The challenge is to efficiently draw and update these particles.

For the drawing we consider two separate parts -- the geometry used for each particle, and the data used to distinguish one particle from another. We pack both of these two parts into a singular vertex array, using instancing for the vertex attributes of the latter part. This allows us to use instanced drawing and draw all of the particles in one draw call. In the particle shader we then need to make sure to add the particle's location offset, and to do whatever is necessary to render the geometry appropriately as usual. This can be done easily enough in any game engine, though it would be much more challenging to create a generic system that can easily work with any particle geometry and any rendering logic. In Trial this is almost free.

There's two parts in Trial that allow me to do this: first, the ability to inherit and combine opaque shader parts along the class hierarchy, and second, the ability to create structures that are backed by an opaque memory region, while retaining the type information. The latter part is not that surprising for languages where you can cast memory and control the memory layout precisely, but nonetheless in Trial you can combine these structures through inheritance, something not typically possible without significant hassle. Trial also allows you to describe the memory layout precisely. For instance, this same system is used to represent uniform buffer objects, as well as what we're using here, which is attributes in a vertex buffer.

If you'll excuse the code dump, we'll now take a look at the actual particle system implementation:

``` common-lisp
(define-gl-struct (particle (:layout-standard :vertex-buffer))
  (lifetime :vec2 :accessor lifetime))

(define-shader-subject particle-emitter ()
  ((live-particles :initform 0 :accessor live-particles)
   (vertex-array :accessor vertex-array)
   (particle-buffer :initarg :particle-buffer :accessor particle-buffer)))

(defmethod initialize-instance :after ((emitter particle-emitter) &key particle-mesh particle-buffer)
  (setf (vertex-array emitter)
        (add-vertex-bindings
         particle-buffer
         (change-class particle-mesh 'vertex-array))))

(defmethod paint ((emitter particle-emitter) pass)
  (let ((vao (vertex-array emitter)))
    (gl:bind-vertex-array (gl-name vao))
    (%gl:draw-elements-instanced (vertex-form vao) (size vao) :unsigned-int 0 (live-particles emitter))))

(defgeneric initial-particle-state (emitter tick particle))
(defgeneric update-particle-state (emitter tick input output))
(defgeneric new-particle-count (emitter tick)) ; => N

(define-handler (particle-emitter tick) (ev)
  (let ((vbo (particle-buffer particle-emitter))
        (write-offset 0))
    (let ((data (struct-vector vbo)))
      (declare (type simple-vector data))
      (loop for read-offset from 0 below (live-particles particle-emitter)
            for particle = (aref data read-offset)
            do (when (< (vx2 (lifetime particle)) (vy2 (lifetime particle)))
                 (when (update-particle-state particle-emitter ev particle (aref data write-offset))
                   (incf write-offset))))
      (loop repeat (new-particle-count particle-emitter ev)
            while (< write-offset (length data))
            do (initial-particle-state particle-emitter ev (aref data write-offset))
               (incf write-offset))
      (setf (live-particles particle-emitter) write-offset)
      (update-buffer-data vbo T))))
```

Let's go over this real quick. We first define a base class for all particles. This only mandates the lifetime field, which is a vector composed of the current age and the max age. This is used by the emitter to check liveness. Any other attribute of a particle is specific to the use-case, so we leave that up to the user.

Next we define our main particle-emitter class. It's called a "shader subject" in Trial, which means that it has shader code attached to the class, and can react to events in separate handler functions. Anyway, all we need for this class is to keep track of the number of live particles, the vertex array for all the particles, and the buffer we use to keep the per-particle data. In our constructor we construct the vertex array be combining the vertex attribute bindings of the particle buffer and the particle mesh.

The painting logic is very light, as we just need to bind the vertex array and do an instanced draw call, using the live-particles count for our current number of instances.

The three functions defined afterwards specify the protocol users need to follow to actually create and update the particles throughout their lifetime. The first function fills the initial state into the passed particle instance, the second uses the info from the input particle instance to fill the update into the output particle info, and the final function determines the number of new particles per update. These particle instances are instances of the particle class the user specifies through the particle-buffer, but their fields are backed by a common byte array. This allows us to make manipulation of the particles feel native and remain extensible, without requiring complex and expensive marshalling.

Finally we come to the bulk of the code, which is the tick update handler. This does not do too much in terms of logic, however. We simply iterate over the particle vector, checking the current lifetime. If the particle is still alive, we call the update-particle-state function. If this succeeds, we increase the write-offset into the particle vector. If it does not succeed, or the particle is dead, the write-offset remains the same, and the particle at that position will be overwritten by the next live, successful update. This in effect means that live particles are always at the beginning of the vector, allowing us to cut off the dead ones with the live-particles count. Then, we simply construct as many new particles as we should without overrunning the array, and finally we upload the buffer data from RAM to the GPU by using update-buffer-data, which in effect translates to a glBufferSubData call.

Now that we have this base protocol in place we can define a simple standard emitter, which should provide a much easier interface.

``` common-lisp
(define-gl-struct (simple-particle (:include particle)
                                   (:layout-standard :vertex-buffer))
  (location :vec3 :accessor location)
  (velocity :vec3 :accessor velocity))

(define-shader-subject simple-particle-emitter (particle-emitter)
  ())

(defmethod initial-particle-state :before ((emitter simple-particle-emitter) tick particle)
  (setf (location particle) (vec 0 0 0)))

(defmethod update-particle-state ((emitter simple-particle-emitter) tick particle output)
  (setf (location output) (v+ (location particle) (velocity particle)))
  (let ((life (lifetime particle)))
    (incf (vx2 life) (dt tick))
    (setf (lifetime output) life)
    (< (vx2 life) (vy2 life))))

(defmethod paint :before ((emitter simple-particle-emitter) (pass shader-pass))
  (let ((program (shader-program-for-pass pass emitter)))
    (setf (uniform program "view_matrix") (view-matrix))
    (setf (uniform program "projection_matrix") (projection-matrix))
    (setf (uniform program "model_matrix") (model-matrix))))

(define-class-shader (simple-particle-emitter :vertex-shader)
  "layout (location = 0) in vec3 vtx_location;
layout (location) in vec3 location;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

void main(){
  vec3 position = vtx_location + location;
  gl_Position = projection_matrix * view_matrix * model_matrix * vec4(position, 1.0f);
}")
```

Okey! Again we define a new structure, this time including the base particle so that we get the lifetime field as well. We add a location and velocity on to this, which we'll provide for basic movement. Then we define a subclass of our emitter, to provide the additional defaults. Using this subclass we can provide some basic updates that most particle systems based on it will expect: an initial location at the origin, updating the location by the velocity, increasing the lifetime by the delta time of the tick, and returning whether the particle is still live after that.

On the painting side we provide the default handling of the position. To do so, we first pass the three standard transform matrices used in Trial as uniforms, and then define a vertex shader snippet that handles the vertex transformation. You might notice here that the second vertex input, the one for the per-particle location, does not have a location assigned. This is because we cannot know where this binding lies ahead of time. The user might have additional vertex attributes for their per-particle mesh that we don't know about. The user must later provide an additional vertex-shader snippet that does define this.

So, finally, let's look at an actual use-case of this system.

``` common-lisp
(define-asset (workbench particles) vertex-struct-buffer
    'simple-particle
  :struct-count 1024)

(define-shader-subject fireworks (simple-particle-emitter)
  ()
  (:default-initargs :particle-mesh (change-class (make-sphere 1) 'vertex-array :vertex-attributes '(location))
                     :particle-buffer (asset 'workbench 'particles)))

(defmethod initial-particle-state ((fireworks fireworks) tick particle)
  (let ((dir (polar->cartesian (vec2 (/ (sxhash (fc tick)) (ash 2 60)) (mod (sxhash (fc tick)) 100)))))
    (setf (velocity particle) (vec (vx dir) (+ 2.5 (mod (sxhash (fc tick)) 2)) (vy dir))))
  (setf (lifetime particle) (vec 0 (+ 3.0 (random 1.0)))))

(defmethod update-particle-state :before ((fireworks fireworks) tick particle output)
  (let ((vel (velocity particle)))
    (decf (vy3 vel) 0.005)
    (when (< (abs (- (vx (lifetime particle)) 2.5)) 0.05)
      (let ((dir (polar->cartesian (vec3 (+ 1.5 (random 0.125)) (random (* 2 PI)) (random (* 2 PI))))))
        (vsetf vel (vx dir) (vy dir) (vz dir))))
    (setf (velocity output) vel)))

(defmethod new-particle-count ((fireworks fireworks) tick)
  (if (= 0 (mod (fc tick) (* 10 1)))
      128 0))

(define-class-shader (fireworks :vertex-shader 1)
  "layout (location = 1) in vec2 in_lifetime;
layout (location = 2) in vec3 location;

out vec2 lifetime;

void main(){
  lifetime = in_lifetime;
}")

(define-class-shader (fireworks :fragment-shader)
  "out vec4 color;

in vec2 lifetime;

void main(){
  if(lifetime.x <= 2.5)
    color = vec4(1);
  else{
    float lt = lifetime.y-lifetime.x;
    color = vec4(lt*2, lt, 0, 1);
  }
}")
```

First we define an asset that holds our per-particle buffer data. To do this we simply pass along the name of the particle class we want to use, as well as the number of such instances to allocate in the buffer. We then use this, as well as a simple sphere mesh, to initialize our own particle emitter. Then come the particle update methods. For the initial state we calculate a random velocity within a cone region, using polar coordinates. This will cause the particles to shoot out at various angles. We use a hash on the current frame counter here to ensure that particles generated in the same frame get bunched together with the same initial values. We also set the lifetime to be between three and four seconds, randomly for each particle.

In the update, we only take care of the velocity change, as the rest of the work is already done for us. For this we apply some weak gravity, and then check the lifetime of the particle. If it is within a certain range, we radically change the velocity of the particle in a random, spherical direction. In effect this will cause the particles, which were bunched together until now, to spread out randomly.

For our generator, we simply create a fixed number of particles every 10 frames or so. In a fixed frame-rate, this should look mean a steady generation of particle batches.

Finally, in the two shader code snippets we provide the aforementioned vertex attribute binding location, and some simple colouring logic to make the particles look more like fireworks. The final result of this exercise is this:

![fireworks](https://filebox.tymoon.eu//file/TVRjMk53PT0=)

Quite nice, I would say. With this we have a system that allows us to create very different particle effects, with relatively little code. For Leaf, I intend on using this to create 2D sprite-based particle effects, such as sparks, dust clouds, and so forth. I'm sure I'll revisit this at a later date to explore these different application possibilities.
