![header](https://studio.tymoon.eu/api/studio/file?id=1933)  
What a hell of a month! We got a lot done, all of it culminating in the release of the new vertical slice demo! This demo is now live, and you can [check it out for free!](https://kandria.com/prototype). This slice includes an hour or more of content for you to explore, so we hope you enjoy it!

## Visuals and Level Design
Like last month, a good chunk of this month was spent designing the remaining areas we needed for the slice. However, this is also the part that got the most shafted compared to how much time I should be investing in it. I'm going to have to dedicate a month or two at some point to just doing rough levels and figuring out what works, both for platforming challenges and for combat. So far I've never actually taken the time to do this, so I still feel very uncertain when it comes to designing stuff.

Still, I'm fairly happy with at least the visual look of things. Fred has done some excellent work with the additional tile work I've requested from him, and I'm starting to learn how to mash different tiles together to create new environments without having to create new assets all the time.

![areas](https://filebox.tymoon.eu//file/TWpJMk9BPT0=)

I've also spent some time on the side making new palettes for the stranger. This was mostly for fun, but I think allowing this kind of customisation for the player is also genuinely valuable. At least I always enjoy changing the looks of the characters I play to my liking. There's 32 palettes already, but I'm still open for more ideas if you have any, by the way!

![palettes](https://filebox.tymoon.eu//file/TWpJM01RPT0=)

We're not quite sure yet how we want to present the palettes in-game. Probably allowing you to pick between a few in the settings, and having some others as items you have to discover first.

## Gameplay changes
We've gone over the combat some more and tweaked it further. It's still a good shot away from what I'd like it to be, and I'll probably have to spend a full month at some point to improve it. Whatever the case, what we have now is already miles ahead of how things started out.

The player movement has also been slightly tweaked to fit better for the exploration and kinds of levels we've built, and to overall feel a bit smoother. The exact changes are very subtle, though I hope you'll still notice them, even if just subconsciously!

![roll](https://filebox.tymoon.eu//file/TWpJMk9RPT0=)

I've also added elevators back into the game. That lead to a bunch of days of frustrated collision problem fixes again, but still, elevators are an important part of the game, so I'm glad I've gotten around to adding them back in.

There's also been a bunch of improvements and fixes to the movement AI so NPCs can find their way better through the complicated mess of underground tunnels and caved in complexes.

## Optimisation
Due to a number of people reporting problems with stutter, and generally the game showing slowdown even on my beefy machine, I put a bit of time into various optimisations. Chief among those is the reduction of produced garbage, which means the garbage collector will be invoked far less often, leading to fever GC pauses stuttering up the framerate. There's still a lot left to be done for that, but I'll do that another time.

I also finally got around to implementing a spatial query data structure -- this is extremely useful as it massively reduces the time needed to do collision testing and so forth. What I've gone with is a much simplified bounding volume hierarchy tree (BVH), mostly because the concept is very simple to understand: every object in the scene you put into a box that encompasses it. You then group two such boxes at a time into another box that encompasses both. You keep doing that until you get one last box that encompasses everything.

![bvh](https://filebox.tymoon.eu//file/TWpJM01nPT0=)

If you now want to know which objects are contained in a region, you start testing the biggest box, and descend into the smaller boxes as long as that region is still a part of the box. If this tree of boxes is well balanced (meaning the closest objects are grouped together), it should reduce the number of tests you need to make drastically.

Implementing this was a surprisingly painless task that only took me a bout a day. Even if the BVH I have is most definitely not ideally balanced at every point in time, it's still good enough for now.

## Editor
As you may or may not know, Kandria is built with a [custom engine](https://shirakumo.org/project/trial), and includes a fully featured editor of its own. This editor is shipped along with every version of the game, and you can open it up at any time by pressing the section key (below Escape).

This month I've made a number of improvements to the editor to add extra tools and fix a lot of issues to its stability. This was necessary to make my own life designing levels not completely miserable, but I think the editor is now also approaching a level of usability that should make it approachable by people not in the dev team, like you!

<iframe width="560" height="315" src="https://www.youtube.com/embed/G-4OUobpc-k" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

There's a bit of [public documentation](https://kandria.com/editor) on the editor, so if you're interested in messing around with the existing levels, or even building your own, check it out! We're still intending on organising a level design contest as well, though for that I want to take some time to polish the editor even more, so you'll have to wait a bit longer for that. If that sounds exciting to you though, be sure to [join our Discord](https://kandria.com/discord), as we'll be organising the event through there whenever it comes to be.

## UI
There's been a number of improvements to the game's user interface. Chief among them being that dialogue choices are now displayed in a less confusing manner, but there's also been some additions to the main menu to allow you to save & quit the game, check your quest log and inventory, and check the button mappings.

We've also included some more accessibility options so that you can change the UI scaling to your liking, pick between different fonts if the default is hard to read, and to disable or tweak things like the gamepad rumble strength or the camera shake intensity.

![controls](https://filebox.tymoon.eu//file/TWpJM01BPT0=)

Unfortunately we haven't had time to build a button remapping UI yet, though the game is already capable of doing the remapping for you. We'll definitely build such a UI in time for the full first act demo, though.

If you have other suggestions for accessibility improvements, please do [let me know](mailto:shinmera@tymoon.eu). Accessibility is very important to me, and I'd like to make Kandria a good example in that domain.

## Composers
[Last month](https://reader.tymoon.eu/article/394) we put out a listing for a composer for Kandria. The response to that was frankly astounding. Within two days we had gotten over a hundred applications, and within the week I had to close the listing down again as we were getting close to three hundred in total!

I knew there were going to be lots of applications, but still, I didn't expect this big of a response. Processing everything and evaluating all the applications took a fair amount of time out of the month, and it was really, really hard, too. So many of the pieces I listened to over the course of doing this were really fantastic!

We're still not quite done with the evaluation, though. We managed to whittle the list down to 10 for interviews, and from there to 3 for a third round. This third round is still going on now; the three were paid to produce a one minute track of music for a specific section of Kandria. The production process, communication, and how well the piece ultimately fits to our vision are going to help us decide who to pick.

The three finalists, [Jacob Lincke](https://jacob-lincke.com), [João Luís](https://www.musicbyjoao.com), and [Mikel Dale](https://www.mikeldale.com), have all agreed to be named publicly, and to have their pieces published once they're done. The deadline for that is 18th of April, so you'll get to hear what they made in the next monthly update! After the deadline we hope to also finalise a contract with our pick until the end of the month, so that they can start with us in May, or shortly after.

I've heard some drafts from each of them already, and what they've produced is really good stuff. It has made me so excited to finally be able to not only see, but also properly hear Kandria!

## Events
Gamedev isn't all about just developing though, as you also have to worry about organisation, management, planning, marketing, and funding. The last is another thing that ate some days' worth of time this month. We were [chosen by ProHelvetia](https://swissgames.ch/2021/03/30/international-online-platforms-2021-selected-studios-announced/) to participate in the Global Games Pitch and Pocket Gamer Connects Digital. We're of course very grateful for these opportunities, and it's fantastic to be able to present Kandria at some events despite Corona!

Still, pitching is a very stressful affair for me, so preparing for GGP and actually executing it took a good bite out of me. On the flipside, we now have some good quality pitching material that we can much more easily adapt and re-use in the future as well. I haven't heard back from anyone about the pitch I did, so I don't have any feedback on what was good or bad about it, which is a shame. I didn't really expect to get any feedback from it though, so I can't say I'm upset about it either.

In any case, PG Connects Digital is happening in a little less than two weeks from now, so I'll have to make sure to be ready for that whenever it comes about.

## Tim's recount
We've reached the vertical slice deadline - the quests are done now and feeling pretty good I think. The dialogue and structures have been refined with feedback from Nick; there's also been a fair amount of self-testing, and a couple of week's testing from our Discord, which has all helped tighten things up. I feel like there's a good balance between plot, character development, player expression, and non-linearity, while also teasing aspects of the wider setting and story. I'm still not totally sure how much playtime the quests constitute right now; I think it largely depends on how fast a player is at the gameplay, and how much they want to engage with the dialogue - but they do take them to the four corners of the current map, and there's some replayability in there too. It feels like a good chunk of content and a major part of the first act. I'm looking forward to seeing how people get on with them, and to learn from their feedback to tweak things further.

I've learnt lots of new scripting tricks in the dialogue engine to bring this together, which will be useful going forwards, and should make generating this amount of content much quicker in the future. Nick and I also have some ideas to improve the current quests, which we should be able to do alongside the next milestone's work.

This month I also helped Nick prepare for the Global Games Pitch event; it was great to watch the stream, and see how other developers pitched their projects. Hopefully this leads to some new opportunities for Kandria too!

## Fred's recount
Added a lot of little things this month. Happy with the new content we got, though I wish I had been able to finish polishing the animations and attack moves on the Stranger for the vertical slice. I had kinda left those anims behind for a while, but I feel it's pretty helpful to gauge the combat feel better.

Otherwise, I am really stoked to get started with the game jam coming up. I love those, last one I did was for my birthday in 2019 and it was the best birthday present ever. :D

## Going forward
As Fred mentioned, the next two weeks we'll be working on a new, secret project! But don't worry, it won't stay secret for very long, and we won't be putting Kandria off for long either. It's going to be a short two-week jam-type project, which we'll release at the end of the month, so you'll know what it is and get to play it by the next monthly update! If you're really curious though, you should [sign up to our mailing list](https://kandria.com/#subscribe) where we'll talk about the project next week already!

If you want to try out the new demo release, you'll get a download link when you [subscribe](https://kandria.com/#subscribe), as well. I hope you enjoy it!
