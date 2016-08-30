With just about one hour and a half to spare we managed to submit our [entry for Ludum Dare 36](http://ludumdare.com/compo/ludum-dare-36/?action=preview&uid=55159). [Ludum Dare](http://ludumdare.com/compo/about-ludum-dare/) is a regularly occurring, fairly well-known game jam the idea of which is to create a game from scratch in 48 by yourself or 72 hours in a team. Given that, unlike last time [we](https://shirakumo.org) tried to participate we actually managed to finish making something that resembles a game, I think it's worth noting what my experience was as well as my general thoughts on game programming in Lisp.

On the [first day](https://www.youtube.com/watch?v=s5ITQe8sK0s), I actually still had a university exam to partake in, so I couldn't start in the morning right away and I didn't have much time at all to prepare anything. This in turn lead to several hours being squandered on the first day trying to get collision detection working and fixing other graphical bugs. Most of that time was spent looking through hundreds of completely useless tutorials and code samples on the web. In general I find the quality of material in the game and engine areas to be absolutely atrocious. Most of the things you can find are either too sweeping, badly written, or assume some ridiculous game framework that makes the code very hard to decipher even if it were applicable as a general, decoupled algorithm. I'm not sure why this field in particular is so bad, or if I'm just searching wrong. Either way, it seems that every time I stumble upon a problem in that domain I have to invest a lot of time in finding the right material-- not a very productive experience as you might guess.

All difficulties aside, after the first day we had a [running game](https://www.youtube.com/watch?v=dU2GConBVVw) that let you interact with objects and pick things up into an inventory and so forth. Pretty much all of the progress you see here is my doing. My partner-in-crime was busy working on a [random map generator](https://www.youtube.com/watch?v=dU2GConBVVw) using perlin noise maps, which only got integrated later. Naturally, the reason why we could move on to working on actual game related features much sooner than on our previous attempt at a Ludum Dare is that our engine, [Trial](https://shirakumo.org/projects/trial) has advanced a lot since then and is much more ready for use. But it is still not there yet by a long shot. Especially a general collision detection and resolution system is a vital aspect that we just did not have the time and energy to incorporate yet. Collision is one of those notorious game development aspects because there's so many ways in which to get it slightly wrong, not to mention that depending on your geometry it can get quite complex quite quickly, not only in terms of performance, but also in terms of the necessary algorithms involved. We've set up a [trello board](https://trello.com/b/dCT0R5BM/trial) now that should allow us to more easily track ideas we have for Trial and keep things organised.

I'm also thinking of focusing future Ludum Dares on one particular core component of the engine. For example next time we could work on a general dialogue tree system and an adventure or visual novel would lend itself very well to that. Or we could start working on a more powerful animation system with something like a puzzle game, and so forth. Another thing we need to get into is sound and music design. Just before the jam I was heavily focused on [getting](https://shirakumo.org/projects/cl-out123) [sound](https://shirakumo.org/projects/cl-mpg123) [support](https://shirakumo.org/projects/audio-blender) incorporated, but especially the mixer part was giving me a lot of unprecedented design issues that I could not satisfactorily resolve in the small amount of time I had available to myself with exams still going strong. I hope to return to that and finish it off soon though. That should lay the path to adding sound to our games. Unfortunately it won't actually help with teaching how to make effects and music. I expect that despite my 8 years of violin and 3 years of saxophone practise I won't actually have much of a clue at all on how to compose music. Either way, there always has to be a start somewhere to get onto the road.

By the end of the [second day](https://www.youtube.com/watch?v=jtR6acPFyIU) we had finally gotten in some good work on the game aspects, having shed pretty much all of the engine troubles. Random map generation worked, you could place items and interact, and there were even some, albeit flowery-looking, mice running around. Things were much more smooth-sailing now thanks to Common Lisp and Trial's incremental and dynamic development capabilities. We did uncover some more issues with the underlying engine system that proved rather "interesting". I'll have to investigate solutions over the coming days. Most prominently one problem is that of accessing important structures such as the scene when it isn't explicitly passed as a parameter. Currently depending on where you are you can reach this through a special variable or through the global window registry. Both approaches feel not so great and I'd like to see if I can come up with a cleaner solution. We also found some problems in other libraries in the ecosystem such as [qtools](https://buttcode.com/qtools) and [flare](https://buttcode.com/flare). It's really great to get some good use out of these libraries and get an opportunity to improve them.

And then the [third day]() marched on. We got some pretty good last-minute features in, namely actually being able to pick up the mice, cook them, and eat them to fill your stomach. Things were getting pretty tight on time towards the end as we were rushing to fix problems in the map generation and gameplay mechanics. Fortunately enough I planned in a lot of buffer time (~6 hours) for the deployment of the game too, as that actually proved to be a lot more problematic than I had anticipated. Not even a single platform was able to deploy right away and it took me [until 2 in the morning](https://www.youtube.com/watch?v=GO8kXODLLJA) to figure everything out. One of the mechanisms that Qtools offers in order to [tie in custom libraries into the automated deploy process](http://shinmera.github.io/qtools/#QTOOLS:DEFINE-USER-LIBS) was not coded right and never tested fully, so that bug only showed up now. On Windows we had some access violation problems that were probably caused by [Trial's asset tracking system](https://github.com/Shirakumo/trial/blob/master/asset-watcher.lisp) constructing Qt objects before dumping. Fortunately I had anticipated such a thing and with a simple `(push :trial-optimize-pool-watching *features*)` before compilation that disabled itself and things worked smoothly from there. 

On Linux the issues were much more curious. Running it from SLIME worked fine. Deploying worked fine. Launching the binary from my host worked fine. But as soon as I tried to launch it from my VM, it would complain about not finding `libsmokebase.so` despite the file sitting right there next to the binary, the system using an absolute path to it that was correctly constructed, and other libraries before it being loaded the same way actually working. I'm still not sure why that exactly happened, but I finally remembered that [qt-libs augments your `LD_LIBRARY_PATH`](https://github.com/Shinmera/qt-libs/blob/master/qt-libs.lisp#L184) in order to ensure that, should a library want to automatically load another for some dumb reason --despite the load order being already exactly and properly done in-code-- it would still resolve to our own custom files first. However, since environment variables aren't saved when the binary is dumped, this did not carry over to the binary, so I had to make sure that Qtools automatically corrects those [on warm boot](https://github.com/Shinmera/qtools/blob/master/deploy.lisp#L175). And as if by magic, now everything did indeed work!

And so I sleep-deprivedly [submitted our entry](http://ludumdare.com/compo/ludum-dare-36/?action=preview&uid=55159) and went off to sleep. Today I then finally got to catch up with some other things that had started to pile up because I didn't have any time to spare at all over the weekend and now I'm here writing this entry. A riveting recount of a tale, to be sure.

---

Now I want to take some time to talk about my general impression on writing games --or applications in general-- in Common Lisp. This is going to be rough and you might hate me for what I'll say. That's fine. Hatred is a good emotion if you can divert its energy into something productive. So put your passion to it and go ahead, brave reader.

I'll start with my biggest gripe: libraries. There's just not enough of them around, and a lot of things are missing. Sure, for games there's bindings to SDL, but if you don't want to use that you're pretty much shit out of luck. There wasn't any library to handle gamepad input, monitor resolution managing, 3d model file loading, or complex audio mixing until I [added](https://shirakumo.org/projects/cl-gamepad) [them](https://shirakumo.org/projects/cl-monitors) [all](https://shirakumo.org/projects/wavefront-loader) [recently](https://shirakumo.org/projects/audio-blender). But there's still so much more missing. Font loading and rendering for example. We're using Qt for that right now but it sucks. It sucks big time. I want something that works.

Whenever I look for a library I have a few criteria that decide for me whether it is actually usable. First and foremost, it must have a non-viral license that allows me to use it freely, and potentially commercially, without repercussion. I don't intend on selling my crap any time soon, but someone might want to that might want to use my software to do it. I cannot accept something that would restrict them from doing so. Second, it must work natively cross-platform on at the very least Linux, Windows, and OS X. If you cannot deploy your application to all of those platforms you can forget about it-- it might be a nice toy for your personal use, but don't have any illusions that everyone is using Linux or that people would bother to switch operating systems just for your program. Third, it must be easy to deploy. This includes but is not limited to minimal C dependencies. Deploying C dependencies is a [bloody](https://github.com/Shinmera/portacle/issues/4) [nightmare](https://github.com/Shinmera/qt-libs/issues?q=) and the version mismatches will ruin your life. Sometimes C dependencies are unavoidable, but anything that creates a gigantic dependency tree is practically impossible to deploy on Linux across distributions without requiring people to install and potentially compile packages on their own system, which is such a ludicrous suggestion that you should feel ashamed for even considering it. End-users will often not know, nor care about how to do that, and certainly won't go through the trouble of finding out just to use your thing. Finally, it should have a nice interface for me, the programmer. I don't want to use a library that is just a bare-bones CFFI wrapper, or merely some magic "run" function and nothing else. If it's something like that I can probably write it better myself and quicker at that than it would take me to figure out how to use that library.

Libraries aside, Common Lisp is not a magic bullet. Most of the problems are still the exact same as in any other environment. The algorithms are the same, the implementations are roughly the same, the problems are about equivalent. Sure, Lisp is different and it *is* really cool, but again, don't make yourself any illusions of grandeur about it. Being such a small community there's just all the more pressure on everyone to put as much into it as possible to bring it up to par with the rest of the world. Just because Lisp has many convenient features that ease coding a lot doesn't remedy the fact that it is dwarfed utterly in man-power. I know [man-power isn't everything](https://en.wikipedia.org/wiki/The_Mythical_Man-Month) but pretending the effects of thousands upon thousands of people working on things all over the world just aren't there is just as insane as expecting your productivity to increase hundred-fold if you add a hundred people to a project. So please, stay open and accepting about the issues that Lisp still has as an ecosystem. The easiest way to start would be by [making sure that libraries have adequate documentation](https://blog.tymoon.eu/article/330).

If you're fine with Lisp staying small, then that's absolutely alright by me. After all, I'm fine with that too, and actually don't really care about it growing either. What I do care about is the idiocy of pretending that somehow Lisp's advantages can trump the size of other ecosystems. That is plain lunacy. No matter how high the convenience, writing any amount of code is going to take time.

I really like Lisp, I have probably around a hundred projects written in it by now and I have not touched any other language for personal projects in years. But I don't merely want to like Lisp, I want to be able to tell other people about it with a good conscience. I want to be able to tell them "yeah sure, you can do that *no problemo*!" without having to fall into the [Turing tar pit](https://en.wikipedia.org/wiki/Turing_tarpit). I want to be able to show them my projects and tell them "man, that was *fun*!", not "man, I had to write like twenty libraries to get here, and I'm now finally done after many weeks of hard work, but it was kinda fun *I guess*."

As mentioned in the other article linked above I really don't want to come off as pushy. I don't want to tell anyone what to do. You do what you like. This is merely me venting my frustration about some of the attitudes I've seen around the place, and some of the problems I seem to be constantly dealing with when working on my projects. I don't like being frustrated, and that's why I'm writing about it. But that's all there is to it; I definitely wouldn't expect this to change anyone's mind or force them to do anything different. It's just another voice in the wind.

So how about mentioning some good aspects? Well, they were already buried in the above, I'd say. Incremental development is awesome, especially for games where a lot of small tweaks and extensive, interactive testing are necessary. Lisp is a very pretty language and I like it a lot more than anything else I've ever worked with so far. It has the potential to be fantastic... but it isn't there yet.

Now I think I'll go back to thinking on how to get it just a sliver more in that direction. Ludum Dare gave me a lot to think about and there's exciting changes ahead. Until next time.