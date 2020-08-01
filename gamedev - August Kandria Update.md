![header](https://studio.tymoon.eu/api/studio/file?id=1754)  
This month marks the release of the Kandria 0.0.2 demo! It also marks a change in focus, but I'll talk about all of that in a second. First, if you want to give the demo a try, you can get a copy here: <https://kandria.com/prototype>

Now, [last month](https://reader.tymoon.eu/article/385) I talked at length about the engine changes and briefly touched on the AI pathfinding stuff I had been working on. The rewrite forced me to deactivate a bunch of features for the time being as they had broken. This is still the case now, there's a couple of things that still need to be brought back up to snuff, such as item interactions and the dialogue box. I left things as they are as I wanted to focus on making the game more fun to play immediately, without delving into story stuff.

Unfortunately my plan for working out a simple AI for the wolf enemy did not come to fruition. I tried for a while, but I realised that there were still too many problems that I didn't know how to solve. Allowing the enemy to move anywhere on the terrain is only one part of the solution. A poor AI that instructs these movements however will lead to it flip-flopping between different targets too quickly, selecting places that aren't usable for an attack, or running into walls.

These are all problems that exist in 3D environments to a limited extent, too, but are made much worse by the fact that this is a 2D side-scrolling platformer, where verticality is an important aspect. In a game with planar movement, these issues can be mostly disregarded, since there's enough space to move in and make the gameplay engaging without much verticality, simplifying the necessary enemy AI dramatically.

So from there I decided it might be better to just add a static enemy and ensure the player movement is fun. Unfortunately at this point I quickly became demotivated for a couple of reasons, prime of which was that I realised the attack animations I had made earlier in the year just weren't going to cut it. They look weird, and aren't fun to use. The animations I'd like should look smooth, have weight to them, and in general have a lot more spectacle and pizzazz to them. Making animations takes a lot of time and work though, and I'm still not entirely sure what I want out of them, so I haven't started on that yet.

Instead I focused on doing what I could do with code alone. This lead to a number of improvements to the general movement. There's now two different top speeds, one normal running speed, and a dash run that allows you to cover long distances very quickly. I think this'll be important to reduce the burden of backtracking for large exploration regions in the game.

The dash was also reworked, allowing you to dash for a variable time depending on how long you hold it down. The behaviour there is also intricate, making it possible to reach lots of places quickly with a bit of skill and practise. The dash now also behaves slightly different on ground and in air, making the ground behaviour a bit better for quick combat movements, while allowing more precise manoeuvres in air. I'm sure I'll have to tweak all of the variables involved in movement some more once I have a vertical slice, but for now it seems pretty good.

I then added a couple of small toys to play around with, namely balloons that provide you an extra upwards boost, which is quite fun to chain together. I don't think having carnival balloons just hanging out is going to fit the rest of the game's atmosphere, but if I can find something thematically fitting, working that into the level design should make traversal a lot more fun. I've also thought about dashing off of enemies and stuff, but I think that might end up being a bit too precise to really work out for platforming.

A small feature that was added is the ability to transition between chunks. In the demo there's a door you can enter to the left of the starting area, which leads to a testing stage I put together. Right now it uses a pretty generic swipe transition. I didn't have time for anything more involved -- putting effects like that together is unfortunately something that takes quite a bit of time and work in a custom engine. In big commercial engines this kind of thing is typically pretty easy to do. One of the many downsides of doing everything yourself.

I also made a bunch of improvements to the editor, most notably its auto-tiling capabilities:

![auto-tile](https://filebox.tymoon.eu//file/TWpBeU1nPT0=)

and the new line drawing tool, which simplifies making new platforms a ton, and in combination with the auto-tiler makes creating levels a breeze:

![line-draw](https://filebox.tymoon.eu//file/TWpBeU13PT0=)

And then the last week was spent mostly worrying about deployment: automating creating packages and uploading them to itch and steam. I'm currently distributing the demo through itch, but I do have a steam app already as well. It'll be a long time before that goes public, though. I also made some small performance optimisations that should hopefully provide better framerates on low-end systems. I'm sure there's still a ton left that I could optimise, but I'll keep that for later.

I've also been busy working on [libmixed](https://github.com/shirakumo/libmixed) on the side. A bunch of important changes were necessary to make it work better with more complex usage scenarios, and to allow proper resampling, something that's necessary for output on some platforms. The rewrite of the base library is now pretty much done, though I have a few extras I want to add. After that I need to patch up the lisp bindings library [cl-mixed](https://github.com/shirakumo/cl-mixed), and the high-level sound server library [Harmony](https://github.com/shirakumo/harmony). Once that's all done I'll finally be able to put some sound into the game, too.

There were other projects I was busy with on the side as well, like [Zippy](https://github.com/shinmera/zippy) and university work, so this month was filled with a ton of coding work for me.

I think now would be a good time to shift gears and stop coding for a while. I want to work on a design and story document for Kandria, and once that's done start looking into recruiting other people and getting the word out there a bit more.

So, if everything goes well, next month I should have a clearer plan on what I want to do with the game and its story, and perhaps even some news about team additions, though I wouldn't count on that yet.

As always, please [subscribe to the newsletter](https://kandria.com/#subscribe) to get access to the demo, and to exclusive weekly updates on the game's development!
