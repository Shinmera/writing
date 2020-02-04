![header](https://filebox.tymoon.eu//file/TVRnM053PT0=)  
I think it's time for another quick update on the game! There isn't all that much to go through, as I've been rather occupied with other matters, such as university exams. Besides that I've also been struggling with figuring out the core story, setting, and direction I want the game to go in.

In any case, here's what I've got: I've made some good improvements to the editor, and it's now possible to fully edit and change everything in the game world. A generic inspector for objects allows you to tune things without having to code up special menus or anything like that.

<video autoplay loop style="width:100%"><source src="https://filebox.tymoon.eu//file/TVRnM05nPT0="></video>

The editor is also now "tools" based, meaning that there's several different interaction tools to manipulate the objects, such as a free transform to move and scale, or a paint tool to draw tiles in.

<video autoplay loop style="width:100%"><source src="https://filebox.tymoon.eu//file/TVRnM05RPT0="></video>

Aside from that I've been working on figuring out the setting. I haven't fully crystallised that yet, so I'd like to hold off talking about it too much before I'm just a slight bit more certain on what I want it to look like.

After much deliberation I've also decided to change the core set of mechanics in the game from a platformer + adventure/detective mashup to a more regular hack and slash set. This change happened mostly because I feel like it's easier to provide engaging content as a hack and slash, and it works much better with an exploration heavy world that I had in mind. I still want the player character to be a detective, and I'll let the story guide things in that way, but I don't think I'll have any explicit adventure game mechanics to support that.

Now, being a hack and slash means there's a lot of new stuff that needs to be done: enemies and enemy AI, an animation based attack system, and of course a lot of actual pixel animations for all the various moves of the various entities in the game. I also needed to work out what I wanted the move set to be like. That part, at least, is worked out.

![concepts](https://pbs.twimg.com/media/EO9Y57iWsAAVHqR?format=jpg&name=4096x4096)

I spent some quality time drawing up various swords and guns, which turned out to be a lot more fun than I thought. I managed to get something that I think I like quite a lot.

![weapons](https://pbs.twimg.com/media/EPDgAi-WAAIX9gz?format=jpg&name=large)

After that I started work on animating some of the new moves for the player.

Backwards evade  
![evade back](https://filebox.tymoon.eu//file/TVRnM01RPT0=)

Forwards evade  
![evade forward](https://filebox.tymoon.eu//file/TVRnM01nPT0=)

Shooting  
![shoot](https://filebox.tymoon.eu//file/TVRnMU9RPT0=)

Light attack  
![light](https://filebox.tymoon.eu//file/TVRnMk1RPT0=)

Heavy attack  
![heavy](https://filebox.tymoon.eu//file/TVRnMk1nPT0=)

I'm still far from done, though. For instance I'd like to be able to cancel attack animations at certain points, and I'd like to have alternate versions for mid-air attacks. I should also start working on a basic enemy concept so I can start work on a sparring partner of sorts.

The astute reader might now note that changing from an adventure to a hack and slash merely shifted the bulk effort from writing to animating. And you might be right about that! I don't know which path would end up costing more effort in the end, but I think I enjoy this a lot more already. Animation is hard work, but it does add a lot of pizzazz!

Integrating the animations into the gameplay is quite difficult though. There's a lot of per-frame state that the engine needs to know about (attack strength, interruptable, hurtbox, velocity), and writing this information down manually is a tremendous pain. So I've decided to write another editor, specifically geared towards writing down this animation metadata. That's what I'll focus on next.

Hopefully by the time I get to write another update, I'll have completed that as well as a rudimentary combat system.

See you then!
