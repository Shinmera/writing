![header](https://shinmera.com/images/games.png)  
It has [been a while](https://reader.tymoon.eu/article/376) since I last wrote an update on my game project, [Leaf](https://www.gamedev.net/projects/1608-leaf/). It's also now been a year since I first started to work on it! I think it's about high time I talked about what's been happening, and what I plan to happen in the near future, too.

Since my last update I have not had a lot of time to work on the project. I've been rather busy with university, so I've mostly constrained working on the games to Saturdays, when I go to [Zürindies](https://www.gamedev.net/projects/1608-leaf/) to hack on my game along with some other indie developers. Because of this, Saturday has become my favourite day of the week. So much so that I kind of dread every other day now. Hopefully things will get better next semester.

A lot of that time, and other time during the week as well, has been spent on things tangential to the game, though. I'm mainly talking about the new GUI toolkit that I started working on, [Alloy](https://shirakumo.org/projects/alloy). Alloy has been a massive timesink, and still continues to be. However, it is now finally starting to be usable, so in recent weeks I've been working on rewriting the editor in Leaf to use Alloy for UI.

![leaf editor using Alloy](https://filebox.tymoon.eu//file/TVRnek53PT0=)

It's still slow going though, because there's a lot of features I need to add as the need comes up, and a lot of bugs to fix as new corners are detected. Either way, I'm beginning to not hate the way things work, so I guess that's a good sign. Having a real project to use Alloy with is also a good opportunity to eat my own dog food.

Aside from the editor, I'll also be able to make use of Alloy for all the in-game UI stuff like menus, inventory, and so forth. Though all of that will have to wait a bit longer until I actually have the mechanics for that laid out more clearly.

Outside of Alloy, I've only worked on small things here and there I'm afraid. I did some world building and general thinking about the locations and characters in the game, though I have not settled on anything yet. One fundamental problem I keep hitting against is that I want to limit the scope as much as possible to avoid making too much work for me, but the current setting kind of mandates a large amount of characters and places in order to feel real and fleshed out. Perhaps this means I should rethink things from the beginning. I'm not sure.

I also wrote a [short story](https://github.com/Shinmera/leaf/blob/master/world/story/sewage.mess) set in the universe that might correspond to an imaginary mission in the game. However, doing that took me way too long, and also once again made me painfully aware of my inability to write a proper tension arc and to write a proper conclusion to things. Those two are problems I've had with writing as far back as I can remember. Perhaps I should outsource the writing, but given that this entire deal is pretty much my brain child, I'm naturally hesitant to do so. I wish I knew writers so I could talk to them about this kind of stuff.

Visually I've improved the graphical capabilities of the game by quite a bit by adding special shadow mapping, absorption maps, and a preliminary day-night cycle.

<video controls width="100%" autoplay="true"><source src="https://www.zurindies.ch/leaf.webm" type="video/webm"><a href="https://www.zurindies.ch/leaf.webm">Webm of the day-night cycle</a></video>

The cycle is not quite complete yet, I still need a way to blend between the background parallax for the day and night, and make sure the colour values with the lighting actually work out fine.

In regards to art, I did a complete redo of the main character's design a while back, and went through a [number](https://studio.tymoon.eu/view/1449) of [different](https://studio.tymoon.eu/view/1451) design [proposals](https://studio.tymoon.eu/view/1452) until I finally arrived at something I liked. The new design fits much better with the overall theme of the game and the character that I had envisioned, so I'm quite happy with it.

![The Stranger's new character design](https://studio.tymoon.eu/api/studio/file?id=1601)

After that I had to go back and change all the animations to fit with the new design, which turned out to be quite troublesome and painful. I'm still unsure whether it would have been easier to just redo them from scratch.

![New animations](https://studio.tymoon.eu/api/studio/file?id=1624)

Hopefully I won't have to redo the design again a second time.

So, that about catches us up to where I am now. From here on out my immediate concern is still the editor. The idea being that once I can make levels more conveniently, I'll be able to prototype ideas and story parts much more effectively, which in turn will hopefully give me an easier time figuring that stuff out.

Unfortunately I'll have to do a lot of studying during the "holidays" once the semester ends, so I'm not sure I'll get a lot of additional time to work on things until February. I'm also still worried about writing, but I guess I'll just have to grit my teeth and keep writing more stories and scenarios to get over that.

Until next time.
