![header](https://studio.tymoon.eu/api/studio/file?id=1796)  
October somehow flew by really quickly for me. It's already November, and we're nearing the end of the year, too. Just thinking about that is making me reminiscent, but I'll have to hold off on doing my yearly wrap-up for another two months! Who knows, a lot more can still happen in that time. [Last month](https://reader.tymoon.eu/article/389) marked another release for [Kandria](https://kandria.com), and this month marked the start of Kandria being an actual team effort!

I'm really glad that it's no longer just me working on things. Fred already introduced himself in the last monthly, and by now he has already started work and delivered some really great stuff:

![new light attack](https://filebox.tymoon.eu//file/TWpBNU9BPT0=)
![player idle](https://filebox.tymoon.eu//file/TWpFd053PT0=)

As a result, the game already feels a lot more fun to play. The step up from the combat animations I had made early in the year is huge!

<iframe width="560" height="315" src="https://www.youtube.com/embed/3Nv_WhN0p48" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

We're still not done with it though, there's a few more moves missing, and a lot more left to adjust and fine-tune of course. We'll also have to get started on some real enemy designs soon and implement those to have some interesting encounters to test things with.

I can now also finally announce the third team member, Tim White, who'll be working on characters, story, and dialogue for the game:

> Hey there! I’m Tim, a games writer from the UK. I’ve been in the industry for ten years now (where did the time go?!), and have been lucky enough to work at Jagex on Transformers Universe, and most recently with Brightrock Games on War for the Overworld and an unannounced game.
> 
> Kandria jumped out the screen at me straight away, with its detailed world and story, custom-made dev tools, and strong creative and artistic direction. I also have a real soft spot for post-apocalyptic worlds, and the ethics surrounding artificial life. Applying was a no brainer, and I can’t wait to start!

You can find Tim on Twitter at [@TimAlanWhite](https://twitter.com/timalanwhite), or on the [official Kandria Discord](https://discord.gg/WNTygau).

Both Tim and Fred will be giving quick updates on what's happening in the [weekly newsletter](https://kandria.com/#subscribe) from now on. The newsletter has now also been moved away from Mailchimp to my own mailing list service called [Courier](https://shirakumo.org/project/courier). I'm glad to finally have made the switch, freeing me from Mailchimp's slow and clunky interface!

On the engine side, I reworked the lighting and background systems to allow changing the lighting and parallax background to fit the current environment. As part of this I also changed the shadow casting to work properly so that it no longer contains the weird corner case glitches it used to.

<iframe width="560" height="315" src="https://www.youtube.com/embed/xP7efR30gbs" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

I also had to make some fixes to the animation system to make it more capable and to make it less of a hassle to use when animations are changed or added. Previously the tooling there would easily mess up your data.

Then, in order to prepare for Tim, I reworked the quest system to be much easier to manage and control, and added a couple of additional features that should be very useful to control branching. To test it I made some quick draft animations for Fi and jotted her down in the test level.

![fi](https://filebox.tymoon.eu//file/TWpFeE1nPT0=)

She'll now comment on things you can find throughout the level.

I also wrote a bunch of [documentation](https://kandria.com/getting%20started) to help Tim and Fred get set up and running with the game, introduced some very useful tooling like hot-reloading to make it faster to iterate on animations and textures, and improved the editor, especially for the in-game animation properties.

With all of this now in, we are very, very close to ending post-production. There's a few not-so-small things that I still need to do, like an animation system for the UI that I started working on yesterday, and one very nasty bug that popped up on Windows systems with surround sound configured. Still, with all of this in mind, I think we're well on track for the vertical slice release in March.

I hope there'll be a 0.0.4 demo release by the end of this month, which will be the last public demo until the vertical slice 0.1.0 demo. After that-- I don't know yet how things will go. A lot about the game is going to become much clearer in the coming months as we decide on stuff like the core plot and work out the first area of the game for the vertical slice.

Aside from putting out whatever fires Fred and Tim stumble across this month, I'll be focusing on two things: first, fix surround sound on Windows. This is important to me as having the game crash and burn because of something so... tangential, is really terrible. Second, implement a UI animation system. The UI toolkit I'm using, [Alloy](https://shirakumo.org/project/alloy), does not currently have a way to animate things. This is fine for tools and other UI like that, but in games you really want to spruce things up by tweening and animating to make your UI more interesting to look at. That's the last major addition to Alloy that's needed to have everything we need.

If time permits, I'll also work on some more platforming challenge levels to give the 0.0.4 demo some more content.

Anyway, I'm really happy to have a team together now, and I'm very excited to see how quickly things develop from here! To be fair, I'm also quite a bit worried what with being, I suppose, my own boss now, and the responsibilities that brings. I suppose time will tell whether I can figure out a good schedule and manage things well. For now I'm cautiously optimistic.

Alright, back to thinking about the animation system now, and see you next month, or [next week if you're on the mailing list](https://kandria.com/#subscribe)!
