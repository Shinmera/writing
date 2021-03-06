![header](https://studio.tymoon.eu/api/studio/file?id=1871)  
I can't believe it's been two months already since the year started. Time moves extremely quickly these days. Anyway, we have some solid progress to show, and some important announcements to make this month, so strap in!

## Overall progress
[Last month](https://reader.tymoon.eu/article/393) was a big update with a lot of new content, particularly all the custom buildings Fred and I had put together to build the surface camp. This month involved a lot more of that, but for the first underground region. This region is still very close to the surface, so it'll be composed out of a mix of ruins of modern corporate architecture, and natural caves.

![office](https://filebox.tymoon.eu//file/TWpJek5RPT0=)
![dorms](https://filebox.tymoon.eu//file/TWpJek53PT0=)

As before, figuring out a fitting style was very challenging, even disregarding the fact that it has to be in ruins, as well. Still, I think what we put together, especially combined with Kandria's lighting system, creates a great amount atmosphere and evokes that feeling of eerie wonder that I've always wanted to hit.

Mushrooms are a big part of the ecosystem in Kandria, being the primary food source for the underground dwellers, so I couldn't resist adding giant mushrooms to the caves.

![cave](https://filebox.tymoon.eu//file/TWpJek5nPT0=)

On the coding side there's been a bunch of bugfixing and general improvement going on. The movement AI can now traverse the deep underground regions seemingly without problem. Game startup speed is massively improved thanks to some caching of the movement data, and NPCs can now climb ropes and use teleporters when navigating.

We've also spent some time working on the combat again, adding some extra bits that, while seemingly small, change the feel quite a lot. Attacks now have a cooldown that forces you to consider the timing, and inputs are no longer buffered for the entire duration of an animation, which eliminates the feeling of lag that was prevalent before. Fred also tuned some of the player's attack animations some more and while I couldn't tell you what exactly changed, when I first tried it out I immediately noticed that it felt a lot better!

All of this just further reaffirms my belief that making a good combat system involves a ton of extremely subtle changes that you wouldn't notice at all unless you did a frame-by-frame analysis. It all lies in the intuition the system builds up within you, which makes it hard to tune. I'm sure we'll need to do more rounds of tuning like that as we progress.

![wolf](https://filebox.tymoon.eu//file/TWpJek9BPT0=)

Then I've also reinstated the wolf enemy that I first worked on close to a year ago. The AI is a lot simpler now, but it also actually works a lot better. It's still a bit weird though, especially when interacting with slopes and obstacles, but it does make for a nice change of pace compared to the zombie enemy. We'll have to see how things turn out when they're placed in the context of actual exploration and quests, though.

<iframe width="560" height="315" src="https://www.youtube.com/embed/TU-xy5Y07So" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Another feature I resurrected and finally got to work right is the ability to save and load regions from zip files. This makes it easy to exchange custom levels. The editor used for the game is shipped with the game and always available at the press of a button, so we're hoping to use that in combination with the zip capability to organise a small level design contest within the community. We'll probably launch that in April, once the new demo hits. If you like building or playing levels, keep an eye out!

The biggest chunk of work this month went into doing level design. I've been putting that off for ages and ages, because it's one of those things that I'm not very familiar with myself, so it seems very daunting. I don't really know where to start or how to effectively break down all the constraints and requirements and actually start building a level around them, let alone a level that's also fun to traverse and interesting to look at! It was so daunting to me in fact, that I couldn't work on anything at all for one day because I was just stuck in a sort of stupor.

Whatever the case though, the only way to break this mould and get experience, and thus some confidence and ability in making levels, is to actually do it. I've put together the first part of the first region now, though it's all still very rough and needs a ton more detail and playtesting.

![region1 upper](https://filebox.tymoon.eu//file/TWpJek9RPT0=)

The part above is the surface settlement, with the city ruins to the right. Below the camp lies the central hub of the first region, which links up to a variety of different rooms -- an office, a market, an apartment complex, and several natural caves that formed during the calamity. The sections below the city ruins don't belong to the slice, but will be part of the full "first act demo" that we plan to release some months after the slice.

Even with all the tooling I built that allows you to easily drag out geometry and automatically tile a large chunk of it, it still takes a ton of time to place all the little details like chairs, doors, railings, machines, plants, broken rubble, background elements, to vary the elements and break up repetition, etc. It also takes a lot of extra effort to ensure that the tiles work correctly in this pseudo-isometric view we have going on for the rooms. Still, the rooms do look a lot better like this than they did with my initial heads-on view, so I think we'll stick with it even if it costs us more time to build.

## Speechless
I've finally gotten around to documenting the dialogue system I've developed for Kandria. I've given it the name [Speechless](https://shirakumo.github.io/speechless), since it's based on [Markless](https://shirakumo.github.io/markless). It's designed to be engine-independent, so if you have your own game in Lisp and need a capable dialogue logic system, you should be able to make use of it. If you do, please [tell me about it](mailto:shinmera@tymoon.eu), I'd be all ears!

 I'd also be interested to hear from other narrative designers on what they think of it. I can't say I'm familiar with the tools that are used in other engines -- a lot of it seems to be in-house, and frequently based around flow-charts from what I can tell. Having things completely in text does remove some of the visual clarity, but I think it also makes it a lot quicker to put things together.

Now, I know that the Lisp scene is very small, and the games scene within it even smaller, so I don't think Speechless will gain much traction, but even if it itself won't, I hope that seeing something like it will at least inspire some to build similar systems, as I think this text based workflow can be extremely effective.

## Hiring a musician
[I'm hiring again](https://kandria.com/team-search)! Now that Kandria's world is properly coming together it's time to look at a composer to start with a soundtrack to really bring the world to life. Music is extremely important to me, so I wanted to wait until we had enough of the visuals together to properly inform the mood and atmosphere. I'm still having a lot of trouble imagining what the world should actually sound like, and there's a broad range of music I like, so I hope that I can find someone that can not only produce a quality score, but also help figure out the exact sound aesthetics to go for.

If you are a musician, or know musicians that are looking for work, have a look at the [listing](https://kandria.com/team-search)!

## Tim's recount
Quests quests and quests! I’ve got the core gameplay scripting done for most of the vertical slice quests now. The last couple are still using placeholder dialogue, but for the others I’ve done several drafts in the voice of the characters, sprinkling in player choices here and there and yeah - it feels like it’s coming together. Hopefully it’s familiarising the player with the characters, their unique voices, and their motivations, whilst keeping the gameplay and plot momentum moving forwards. I’ve now written in anger for all of the main hub characters, and feel like I’m getting into their headspace.

Some of the scripting functionality has been more complex than I anticipated - but with help from Nick creating new convenience functions, and showing me the best way to structure things, I feel like I’ve gotten most of the design patterns down now that I’m going to need going forwards.

The rest of the month will involve rounding out these quests, iterating on feedback, and transposing the triggers (which are still using debug locations) into the main region layout

## Fred's recount
Quite a lot of character anims in! It'll be exciting to see the camp characters come to life in the game and not just in my animation software. 🙂

This feels like this month was an important milestone at making Kandria's world more immersive. There's still more work to do on the buildings and getting convincing yet fun to explore ruins but overall it feels like a lot of stuff is coming together.

## The future
This is the last month we had in our plan for the vertical slice. Unfortunately it turns out that we had way underestimated the amount of time it would take to create the required tilesets and design the levels. Still, it seems much more important to avoid crunch, and to deliver a quality slice, so we're looking to extend the deadline.

We'll still try to release an early slice for our testers by the end of this month, but then we'll take two additional weeks for bugfixing and polish, so the updated public demo should be out mid-April. We'll be sure to make an announcement when it comes out or if there's other problems that'll further delay it. Please bear with us!

The remainder of April though we're planning to completely switch gears away from Kandria and catch a mental breather. We'll instead work on a new, very small jam project, that we hope to build and release within the two weeks. We're not entirely certain yet what exactly we'll do, but it should be a lot of fun to do a jam again one of these days.

As always, thank you very much for reading and in general for your interest in Kandria! Starting from scratch like we are (in multiple ways at that!) isn't easy, and it's been really nice to see people respond and support the project.

If you'd like to support us, it would help a lot to [wishlist Kandria on Steam](https://kandria.com/steam), and to [join the Discord](https://kandria.com/discord)! There's also a lot of additional information on the development and our current thoughts in the weekly [mailing list updates](https://kandria.com/#subscribe) and [my Twitter](https://twitter.shinmera.com).
