![header](https://filebox.tymoon.eu//file/TVRreU13PT0=)  
It's April! ... already. Man alive, March shaped up to be quite the month, didn't it. For reasons you can probably already guess at, not too much progress was made this month, but after the splash of excitement that was [last month](https://reader.tymoon.eu/article/381), I don't feel too bad about that. Well, actually I do, but I'll try my best to tamper my disappointment and just move on.

Last month I promised to work on combat, and that's primarily what I did. To this end, I've added a bunch of new player animations and started work on an animation based movement and interaction system, as well as a combat system based on that.

<iframe width="560" height="315" src="https://www.youtube.com/embed/ozvW1MpRFc8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Attacks come in two variants, light and heavy. Light attacks can be chained up to three times, and heavy attacks up to two times. Each variant and step of the chain has different hurtboxes, iframes, cooldown, etc. Currently everything is just set up a bit willy-nilly, and will definitely require intensive tuning to make it feel smoother.

<iframe width="560" height="315" src="https://www.youtube.com/embed/Pzh4fy7L5rA" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

When hit, entities can get staggered for a brief amount of time, or thrown back when a hard attack lands. This system works for both the player and enemies, but there's currently no enemy AI to really play with that. I'm gonna have to start working on that fairly soon, too.

All of this information about hitboxes, damage, frame velocity, and so forth is stored in a per-frame data structure that can be edited with a home-grown animation editor.

![animation editor](https://filebox.tymoon.eu//file/TVRreU1nPT0=)

The editor works, but it's pretty unwieldy and could use a lot of improvements in usability right now. Part of the reason for that is that it's written using [Alloy](https://shirakumo.org/project/alloy), my own UI toolkit. Alloy is also still very young and rough around the edges, which plays into that. Progress on Alloy has been very slow for a while now, and given how big of a task it is to make a UI toolkit I do wonder a lot whether I'll ever bring it to a point where I'm happy with it. I would definitely appreciate some help in that area! If you're interested in a pure-lisp UI toolkit that can integrate with OpenGL (among others), please [do let me know](mailto:shinmera@tymoon.eu).

Then I did some minor stuff that's not really visible to the user, like [porting to MacOS](https://twitter.com/Shinmera/status/1235661386597847040), rewriting the save-state system, and so forth. That's about it on the technical side, I'm afraid.

I have been doing some non-code related things for Kandria as well, though! There's now a proper domain for it, at <https://kandria.com>, and I've started a weekly update series [on the mailing list](https://kandria.com/index.html#subscribe). So far there have been two entries, one talking about the kinds of philosophical and ethical questions I want to tackle in the game, and the other describing Kandria's world building and backstory leading up to the game. I'll try to find something interesting to talk about every week!

And that's about it for March. Not much else got done, I'm afraid. Since I can no longer go to the office during the week or to the Zürindies meetup on Saturdays, motivation has been a bit in the dumps. I've been spending most of my time either depressed, working for university, working on my own mailing list service, or playing Splatoon and Animal Crossing. I'm sure I'm not the only one that's been struggling with keeping motivation up. Hopefully April is going to go better!

See you then.
