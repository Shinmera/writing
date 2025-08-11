![header](https://filebox.tymoon.eu//file/TVRrNE9BPT0=)  
Another month already gone by. Unlike [last month](https://reader.tymoon.eu/article/383), I have managed to work on a few things this month, though it isn't much yet. So, in order to bolster the content a bit, I thought I'd also talk about the rough roadmap I have for [Kandria](https://kandria.com).

But first, let's go over the progress for the month. I started working on the first real enemy of the game: a wolf fit for dark, underground caverns.

![wolf](https://filebox.tymoon.eu//file/TVRrNE9BPT0=)

All of the animations I need for it (for now) are done, and I added a *really* primitive AI for it. In the process of this I also started to work on an optimisation and cleanup to revise the sprite system in the engine. Specifically, I wanted to be able to remove some of the kludges I had introduced in Kandria to extend the frame information, and allow the system to use packed sprite atlases. So far, I had been using a very primitive implementation where each sprite was a horizontal strip of frames, each frame a fixed size.

![horizontal sprite sheet](https://filebox.tymoon.eu//file/TVRrNE5nPT0=)

That's the player sprite sheet up there.

However, this is very, very wasteful. Despite the low resolution of the player sprite (16x32 for many frames), this atlas ended up being 13'760x50! With a packed atlas however, the size can be reduced to 441x383, that's a total pixel reduction of 4x! A packed atlas is also much more square, so it is much more likely to fit into the texture size limits of older graphics cards.

![packed sprite atlas](https://filebox.tymoon.eu//file/TVRrNE53PT0=)

Anyway, I completely rewrote the sprite system in Trial to work with sprite atlases and generally to allow having each frame be of a different size. However, ripping things out like this and rewriting them usually causes a lot of headaches to fix everything up to work with the new system. And so it did. Once I got started with the cleanup though I couldn't help myself and started ripping out other components that were shoddy, too.

And now I'm in the uncomfortable position of all my past technical debt staring me in the face. There's a lot that I need to rip out and replace to make less of a hack. I probably should have done that long ago, but you know, hindsight is 20/20. Anyway, there's some big engine things that I feel I need to rewrite now, before it gets any worse. Despite this ostensibly being a good thing, it makes me feel really terrible, because it means there won't be any tangible progress for a while longer. Seeing people on Twitter make great progress with their games doesn't help either, and makes me regret trying to roll my own engine quite a bit. Despite all that, I'm not going to switch to something else quite yet.

Another thing that happened this month is that I finally got the [news back from ProHelvetia](https://reader.tymoon.eu/article/381). Unsurprisingly to me, I did not get the funding. That's a bummer, but it honestly doesn't change anything really. I was never intending on relying on their grant anyway, so I'm just going to keep moving like before. I'll just have to dig a bit deeper into my own pockets for when I start hiring help during production. I might also try applying again next year -- we'll see.

This neatly brings us to the current roadmap for Kandria. Now, I've never been any good at long-term planning. In fact, I typically try to avoid it, since I believe I won't have any idea how things turn out anyway, so planning too far ahead is just futile. That being said, I do have a task list that goes from very precise to very imprecise as time goes on:

- Fix Trial's scene graph and shader pipeline mess. This will also affect other projects using Trial in a big way. Explaining what exactly this entails is a bit out of scope of this, but I'll probably write a separate [gamedev article](https://reader.tymoon.eu/tagged/s:gamedev) about it.
- Fix and improve the node graph AI. This is what allows NPCs to move like a normal character across the 2D terrain. Currently it's a very primitive and limited implementation that needs to be fixed up to work better for the enemy AI.
- Implement a better AI for the wolf enemy. Currently the AI does not fit well with the behaviour I had imagined for the wolf. This should be a very minor and simple task.
- Fine tune the combat to be more fluid and fun to play. This will involve a lot of manual adjustment and testing. I'll also release another prototype demo version at this point to get some feedback from others.
- Rewrite [libmixed](https://shirakumo.org/project/libmixed) to work with bip-buffers, and ultimately to work with proper varying sample counts and allow resampling. This is necessary to make it work on all platforms, and will lead to allowing sound and music in Kandria.
- Implement simple sound and graphical effects for the combat. This should again make it a lot more fun to play and should improve the perceived quality a lot. This is another public demo milestone.
- Fix and reinstate the dialogue and quest systems. This should allow adding NPCs to talk to, and should add a simple quest system to track progression.
- Write an editor for the quest system. The quest system is a flow graph, which is unbearably tedious to manipulate without specialised UI.
- Begin production. With all of the prerequisites now done, production can begin in earnest.
- Develop a full vertical slice of the central hub area. This should include NPCs that move around, a set of quests to fulfil, as well as a few combat encounters and locations to explore.
- Another demo might be released here, to gather more feedback about the overall picture.
- Develop a full horizontal slice. This will require the story to be fully realised, and most of the rough level design as well.
- Publish a 'first chapter' type of demo to gather attention and market the game some more.
- Finish the full game.

Of course, the further down the items are, the less certain I am that I'll be able to get to them in that order, let alone when exactly I'll get to them. Initially, before all the Corona crap, the plan was to reach the production stage by August or September this year. Now I'm not too sure that'll be possible to reach, but here's to hoping anyway.

Whatever the case, I'll keep doing the weekly posts on my [mailing list](https://kandria.com#subscribe), as well as the public monthlies, and keep you up to date on what's going on that way. In case you're not already subscribed: the mailing list weeklies also have other juicy info on the game besides the rough development updates of the monthlies! Don't forget, if you want to talk about Kandria with me or others that are interested, there's also a [Discord](https://discord.gg/WNTygau).

Anyway, that's about it for this month. I'll get back to thinking about that first task now...
