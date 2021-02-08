![header](https://studio.tymoon.eu/api/studio/file?id=1863)  
I hope you've all started well into the new year! We're well into production now, with the vertical slice slowly taking shape. Much of the work in January has been on concept and background work, which is now done, so we are moving forward on the implementation of the new features, assets, and writing. This entry will have a lot of pictures and videos to gander at, so be warned!

The vertical slice will include three areas -- the central camp, or hub location, the first underground area, and the desert ruins. We're now mostly done implementing the central camp. Doing so was a lot of work, since it requires a lot of unique assets. It still requires a good amount of polish before it can be called well done, but for the vertical slice I think we're good at the point we are now.

![camp-1](https://filebox.tymoon.eu//file/TWpJeU1BPT0=)
![camp-3](https://filebox.tymoon.eu//file/TWpJeU1RPT0=)

The camp is where all the main cast are (Fi, Jack, Catherine, and Alex), and where you'll return to after most missions. As such, it's important that it looks nice, since this is where you'll spend a lot of your time. It also has to look believable and reasonable for the cast to try and live here, so we spent a good amount of time thinking about what buildings there would be, what purpose they should fulfil, and so forth.

We also spent a good deal of time figuring out the visual look. Since Kandria is set quite far into the future, with that future also having undergone a calamity, the buildings both have to look suitably modern for a future society to have built, but at the same time ruined and destroyed, to fit the calamity event.

![camp-2](https://filebox.tymoon.eu//file/TWpJeU1nPT0=)
![camp-4](https://filebox.tymoon.eu//file/TWpJeU13PT0=)

I also finished the character redesign for Fi. Her previous design no longer really fit with her current character, so I really wanted to get that done.

![fi-draft](https://filebox.tymoon.eu//file/TWpJeU5BPT0=)
![fi](https://filebox.tymoon.eu//file/TWpJeU5RPT0=)

On the gameplay side the movement AI has been revised to be able to deal with far more complicated scenarios. Characters can now follow you along, move to various points on the map independently, or lead the player to a destination.

<iframe width="560" height="315" src="https://www.youtube.com/embed/h0ZiZnS_2WQ" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Quests now also automatically track your time to completion, which allows us both to do some nice tracking for score and speedrun purposes, but also to implement a 'race' quest. We have a few ideas on those, and it should serve as a nice challenge to try and traverse the various areas as quickly as possible.

<iframe width="560" height="315" src="https://www.youtube.com/embed/THJF6OifDSo" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

We're also thinking of setting up leaderboards or replays for this, but that's gonna have to wait until after the vertical slice.

For look and feel there's also been a bunch of changes. First, there's now a dedicated particle system for effects like explosions, sparks, and so forth. Adding such details really enhances the feel of the combat, and gives a nice, crunchy, oomph to your actions. I still have a few more ideas for additional effects to pile on top, and I'll see that I can get to those in due time.

![particles](https://filebox.tymoon.eu//file/TWpJeU5nPT0=)

Also on the combat side, there's now a quick-use menu so you can access your healing items and so forth easily during combat. It even has a nice slow-mo effect!

<iframe width="560" height="315" src="https://www.youtube.com/embed/F4EUe8V_QAk" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Since we're not making a procedural game, we do have to have a way of gating off far areas in a way that feels at least somewhat natural. To do this I've implemented a shader effect that renders a sandstorm on top of everything. The strength of the effect can be fine-tuned, so we could also use it for certain setpieces or events.

<iframe width="560" height="315" src="https://www.youtube.com/embed/hIwZM7A4eVk" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

The effect looks a lot better in-game. Video compression does not take kindly to very noisy and detailed effects like this. Having the sand howl around really adds a lot to the feel of the game. In a similar vein, there's also grass and other foliage that can be placed now, which reacts to the wind and characters stepping on it. You can see that in action in this quick run-down of the camp area:

<iframe width="560" height="315" src="https://www.youtube.com/embed/_PQ_5eaVIXw" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

There's a bunch of other things we can't show off quite yet, especially a bunch of excellent animations by Fred. I haven't had the time to integrate all of those yet!

We've also been thinking more about how to handle the marketing side of things. I'm now doing a weekly screenshotsaturday thing on Twitter, and semi-regularly post quick progress gifs and images as well. Give me a [follow](https://twitter.com/shinmera) if you haven't yet!

Then I took advantage of Rami Ismail's [excellent consulting service](https://calendly.com/tha_rami) and had a talk with him about what we should do to improve the first impressions for Kandria and how to handle the general strategy. He gave some really excellent advice, though I wish I had had more time to ask other questions, too! I'll probably schedule a consultancy hour with him later this year to catch up with all of that.

Anyway, I think a lot of the advice he gave us isn't necessarily specific to Kandria, so I thought it would be good to share it here, in case you're a fellow developer, or just interested in marketing in general:

- Make sure to keep a consistent tone throughout your paragraph or trailer. This means that you want to avoid going back and forth between advertising game features or narrative elements, for instance. In Kandria's case we had a lot of back and forth in our press kit and steam page texts, which we've now gone over and revised to be more consistent.
- Marketing is as much about attracting as many people as possible as it is about pushing people away. You want to be as efficient as possible at advertising to your target group. This also means being as up-front as possible about what your game is and who it is for, so you immediately pull in the people that would care about it, and push away the people that would not.
- You need to figure out which part of your game best appeals to your core audience, and how you need to put it to make it attractive. Having an advertisement platform that gives you plenty of statistics and targeting features is tremendously helpful for this. Rami specifically suggested using short Facebook ads, since those can be targeted towards very specific groups. Do many small ads using different copy texts and trailers to see which work the best at attracting people to your Steam page.
- Always use a call to action at the end of your top of the funnel (exposure) marketing. In fact, don't just use one link, use one for every way people have to interact with your game, if you have several. For us in specific this means I'll now include a link to our [mailing list](https://kandria.com/#subscribe), our [discord](https://kandria.com/discord), and our [steam page](https://kandria.com/steam) in our material.
- Only use community/marketing platforms that you're actually comfortable with engaging with yourself. This means don't force yourself to make a Discord or whatever if you're not going to really engage with it. I'm fairly comfortable with where we are now, though I'm considering also branching out to imgur for more top of the funnel marketing. We'll see.
- Two years is plenty of time to get marketing going. Generally you want to really up the hype train about three months before release. The wishlist peak about one month before release should give you a rough idea of whether the game is going to be successful or not -- 5-10k is good, 15-20k should be very good.
- Three weeks before release is when you want to start contacting press -- write emails to people that have reviewed the games that inspired yours and seem to generally fit the niche you're targeting. Let them know you'll send a final build a week before release.
- Actually do that exactly a week before release. Ideally your game will be *done* and you won't fudge with it until after release.
- On the day before release, log onto gamespress.com and submit your game. Actual journalists don't tend to look there it seems, since they already get way more than enough mail, but third parties and independent people might!

And that's about what we managed to discuss in the 20 minutes we had. As mentioned, I'll probably schedule another consultancy later in the year. I'll be sure to let you know how it went!

Alright, I've run my mouth for long enough now, here's some words from Tim about his experience for January:

> It’s been a documentation-heavy month for me: designing the vertical slice quests on paper (which will become the first act of the narrative), making some tweaks to the characters and plots to fit the game’s pillars, and also tweaking the press kit and marketing copy from Rami’s feedback.
> 
> The last two weeks I’ve also started implementing the first quest, reminding myself how to use the scripting language and editor (it’s amazing how much you forget after a couple of weeks away from it). This has also involved familiarising myself with the “proper” quest structure, using the hierarchy of quest > task > trigger (for the demo quest it was more like task > trigger, trigger, trigger, etc. you get the idea). What’s been most fun though is getting into the headspace for Jack and Catherine, writing their initial dialogues, and threading in some player choice. Catherine is quickly becoming my favourite character.
> 
> It’s also been great to see the level design and art coming along - Nick’s sketched layouts, and now the pixel art for the ruined buildings which he and Fred have been working on. Oh, and seeing the AI in action, with Catherine bounding along after The Stranger blew my mind.

Well, that's about it for this month. It's been exciting to finally see a change in the visuals, and I'm excited to start tackling the first underground area. I see a lot more pixel work ahead of us...

Anyway, in the meantime until the next monthly update, do consider checking out the [mailing list](https://kandria.com/#subscribe) if you want more in-depth, weekly updates on things. We cover a lot of stuff there that never makes it into the monthlies, too! If you want to get involved in discussions and feedback around the game, hop onto the [discord](https://kandria.com/discord). We're slowly building a community of fans there, and are trying to post more actively about the process. For a more casual thing, there's also [my twitter](https://twitter.com/shinmera) with plenty of gifs and images. Finally, please do [wishlist Kandria on Steam](https://kandria.com/steam)! It might seem like it isn't much, but it really does help out a lot!

Thanks for reading, and see you next time!
