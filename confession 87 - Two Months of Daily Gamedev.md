![header](https://filebox.tymoon.eu//file/TVRjNE1BPT0=)  
It's been two months now since I started to do [daily game development streams](https://reader.tymoon.eu/article/373). I've been trying my best, but it is time for this to come to a close. In this article I'll talk about the various things that happened, why I'm stopping, and the future of the Leaf game. Strap in!

It's actually been slightly longer than two months, but since I missed some days due to being sick, and some others because I didn't feel like streaming -- more on that later -- I'll just count it as two months. In any case, in this time I've done [56 streams](https://www.youtube.com/playlist?list=PLkDl6Irujx9NUeqnEkRsFZ6bLS24B-6lT), almost all of them two hours long. That's a lot of hours, and I'm truly impressed that some people stuck around for almost all of them. Thank you very much! A lot happened in that time too, and I think it would be interesting to go over some of the major features and talk about them briefly.

## New Features in Leaf
### Slopes and Collision
Collision detection was heavily revised from the previous version. The general procedure is to scan the current chunk for hits until there are no more hits to be found. If we have more than ten hits we assume that the player is in a wall somehow and just die. The number ten is obviously arbitrary, but somehow it seems sufficient and I haven't had any accidental deaths yet.

When a hit is detected, it dispatches on the type of tile or entity that was collided with. It does so in two steps, the first is a test whether the collision will happen at all, to allow sub-tile precision, and the second is the actual collision resolution, should a full hit have been detected. The first test can be used to elide collisions with jump-through platforms or slopes if the player moves above the actual slope surface. The actual collision resolution is typically comprised of moving the player to the collision point, updating velocity along the hit normal, and finally zipping out of the ground if necessary to avoid floating point precision issues.

The collision detection of the slopes itself is [surprisingly simple](https://shinmera.com/project/leaf/blob/master/moving.lisp#L80) and works on the same principle as swept AABB tests: we can enlarge the slope triangle by simply moving the line towards the player by the player's half-size. Once this shift is done we only need to do a ray-line collision test. During resolution there's some slight physics cheating going on to make the player stick to the ground when going down a slope, rather than flying off, but that's it.

### Packets and File Formats
Leaf defines a multitude of [file formats](https://shinmera.com/project/leaf/tree/master/doc). These formats are typically all defined around the idea of a packet -- a collection of files in a directory hierarchy. The idea of a packet allows me to define these formats as both directly on disk, in-memory as some data structure, or encapsulated within an archive. The [packet protocol](https://shinmera.com/project/leaf/blob/master/packet.lisp) isn't that complicated and I intend on either at least putting it into Trial, or putting it into its own library altogether. Either way, it allows the transparent implementation of these formats regardless of backing storage.

The actual formats themselves also follow a very similar file structure: a ``meta.lisp`` file for a brief metadata header, which identifies the format, the version, and some authoring metadata fields. This file is in typical s-expression form and can be used to [create a version object](https://shinmera.com/project/leaf/blob/master/serialization.lisp), which controls the loading and writing process of the rest of the format. In the [current `v0`](https://shinmera.com/project/leaf/blob/master/versions/v0.lisp), this usually means an extra ``data.lisp`` payload file, and a number of other associated payload files like texture images.

The beauty of using generic functions with methods that specialise both on the version and object at the same time is that it allows me to define new versions in terms of select overrides, so that I can specify new behaviour for select classes, rather than having to redo the entire de/serialisation process, or breaking compatibility altogether.

### Dialogue and Quests
The dialogue and quests are implemented as very generic systems that should have the flexibility (I hope) to deal with all the story needs I might have in the future. Dialogue is written in an extended dialect of [Markless](https://shirakumo.org/docs/markless). For instance, the following is a valid dialogue snippet:

```
~ Fi
| (:happy) Well isn't this a sight for sore eyes!
| Finally a bit of sunshine!

- I don't like rain
  ~ Player
  | I don't mind the rain, actually.
  | Makes it easier to think.
- Yeah!
  ~ Player
  | Yeah, it's been too long! Hopefully this isn't announcing the coming of a sandstorm.
  ! incf (favour 'fi)
- ...
  ! decf (favour 'fi)

~ Fi
| ? (< 3 (favour 'fi))
| | So, what's our next move?
| |?
| | Alright, good luck out there!
```

The list is translated into a choice for the player to make, which can impact the dialogue later. The [way this is implemented](https://shinmera.com/project/leaf/tree/master/dialogue) is through a syntax extension in the [cl-markless](https://shirakumo.org/docs/cl-markless) parser, followed by a compiler from the Markless AST to an assembly language, and a virtual machine to execute the assembly. The user of the dialogue system only needs to implement the evaluation of commands, the display of text, and the presentation of choices.

The [quest system](https://shinmera.com/project/leaf/tree/master/quest) on the other hand is based on node graphs. Each quest is represented as a directed graph of task nodes, each describing a task the player must fulfil through an invariant and a success condition. On success, one or more successor tasks can be unlocked. Tasks can also spawn dialogue pieces to become available as interactions with NPCs or items. The system is smart enough to allow different, competing branches, as well as parallel branches to complete a quest. I intend on building a graph editor UI for this once Alloy is further along.

Both of these systems are, again, detached enough that I'll either put them into Trial, or put them into a completely separate library altogether. I'm sure I'll need to adjust things once I actually have some written story on hand to use these systems with.

### Platforming AI
The platforming AI allows characters to move along the terrain just like the player would. This is extremely useful for story reasons, so that characters can naturally move to select points, or idle around places rather than just standing still. The way this is [implemented](https://shinmera.com/project/leaf/blob/master/move-to.lisp) is through a node graph that describes the possible movement options from one valid position to the next. This graph is built through a number of scanline passes over the tile map that either add new nodes or connect existing nodes together in new ways.

The result is a graph with nodes that can connect through walk, crawl, fall, or jump edges. A character can be moved along this graph by first running A* to find a shortest path to the target node, and then performing a real-time movement through the calculated path. Generally the idea is to always move the player in the direction of the next target node until that node has been reached, in which case it's popped off the path. The jump edges already encode the necessary jump parameters to use, so when reaching a jump node the character just needs to assume the initial velocity and let standard physics do the rest.

The implementation includes a simple visualiser so that you can see how characters would move across the chunk terrain. When the chunk terrain changes, the node graph is currently just recomputed from scratch which isn't fast, but then again during gameplay the chunk isn't going to change anyway so it's only really annoying during editing. I'll think about whether I want to implement incremental updates.

### Lighting
Leaf has gone through two lighting systems. The [old one](https://shinmera.com/project/leaf/blob/866b392e3ec90630875566190f23c37288998619/lighting.lisp) worked through signed distance fields that were implicitly computed through a light description. New light types required new shader code to evaluate the SDF, and each light required many operations in the fragment stage, which is costly.

 The [new system](https://shinmera.com/project/leaf/blob/master/lighting.lisp) uses two passes, in the first lights are rendered to a separate buffer. The lights are rendered like regular geometry, so we can use discrete polygons to define light areas, and use other fancy tricks like textured lights. In the second pass the fragment shader simply looks up the current fragment position in the light texture and mixes the colours together.

In effect this new system is easier to implement, more expressive, and much faster to run. Overall it's a massive win in almost every way I can imagine. There's further improvements I want to make still, such as shadow casting, dynamic daylights, and light absorption mapping to allow the light to dissipate into the ground gradually.

### Alloy
[Alloy](https://shirakumo.org/project/alloy) is a new user interface toolkit that I've been working on as part of Leaf's development. I've been in need for a good UI toolkit that I can use within GL (and otherwise) for a while, and a lot of Leaf's features had to be stalled because I didn't have one yet. However, a lot of Alloy's development is also only very distantly related to game development itself, and hardly at all related to the game itself. Thus I think I'll talk more about Alloy in other articles sometime.

## Why I'm Stopping
I initially started this daily stuff to get myself out of a rut. At the time I wasn't doing much at all, and that bothered me a lot, so committing to a daily endeavour seemed like a good way to kick myself out of it. And it was! For a long time it worked really well. I enjoyed the streams and made good progress with the game.

Unfortunately I have the tendency to turn things like this into enormous burdens for myself. The stream turned from something I wanted to do into something I felt I had to do, and then ultimately into something I dreaded doing. This has happened before with all of my projects, especially streaming ones. With streams I quickly feel a lot of pressure because I get the idea that people aren't enjoying the content, that it's just a boring waste of time. Maybe it is, or maybe it isn't, I don't know. Either way, having to worry about the viewers and not just the project I'm working on, especially trying to constrain tasks to interesting little features that can fit into two hours turns into a big constraint that I can't keep up anymore.

There's a lot of interesting work left to be done, sure, but I just can't bear things anymore at the moment. Dreading the stream poisoned a lot of the rest of my days and ultimately started to hurt my productivity and well-being over the past two weeks. Maybe I'll do more streams again at some point in the future, but for now I need a break for an indeterminate amount of time.

## The Future of Leaf
Leaf isn't dead, though. I intend to keep working on it on my own, and I really do want to see it finished one day, however far away that day may be. Currently I feel like I need to focus on writing, which is a big challenge for me. I'm a very, very inexperienced writer, especially when it comes to long-form stories and world-building. There I have practically no idea on how to do anything. If you are a writer, or are interested in talking shop about stories, please [contact me](https://everything.shinmera.com).

Other than writing I'm probably going to mostly work on Alloy in the immediate future. I hope to have a better idea of the writing once I'm done, and that should give rise to more features to implement in Leaf directly. I'll try to keep posting updates on the blog here as things progress in any case, and there's a few systems I'd like to elaborate on in technical articles as well.

Thanks to everyone who read my summaries, watched the streams or recordings, and chatted live during this time. It means a lot to me to see people genuinely interested in what I do.
