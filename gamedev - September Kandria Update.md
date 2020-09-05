![header](https://studio.tymoon.eu/api/studio/file?id=1767)  
[Another month](https://reader.tymoon.eu/article/386) already. Fortunately there's a lot of stuff to talk about for [Kandria](https://kandria.com) this time around, so I hope you're ready for a beefy summary!

This month was originally intended to be focused on marketing and recruitment, but for more reasons than one that's not entirely how it went down after all. The biggest reason among the bunch being that it has proven really hard for me to concentrate on that -- they're topics I have no experience with, so I don't feel very comfortable dealing with them at all. Regardless, there's been a few things I've done to help in that regard:

There is now a lengthy [game design document](https://kandria.com/design.html) that should hopefully give a good idea of what the game should be and should be about. It's mostly meant as a communication tool for future team members, to help them get up to speed on the project, and for potential team members to evaluate whether this project is something for them. If you have a read through it, I would [appreciate your thoughts](mailto:shinmera@tymoon.eu) on it a lot! Being in an opinion vacuum doesn't help with creativity.

Next I started working on an official job listing. It's not quite done yet and I want to run it by a few people before I get it out there, but if you are or know pixel artists or writers, I'd appreciate it tremendously if you could keep an eye open for the listing. I'll make another announcement about it on all the channels once it's out.

I've also started a thread on some forums to try and spread awareness of the game, but I'm worrying that I'm spreading myself too thin and can't tend to all of the outlets as much as I'd like. There's already email, Twitter, and Discord to take care of besides that. With Kandria not being a full-time job yet, I'm not sure how much time I should be spending on the social media channels, rather than spending it on development. Maybe I should reserve a day for it every week? Either way, top priority for the coming few weeks will be finding new team members, and I'll definitely have to invest some more energy to get that going.

I've also made some good progress on the code and art side: first I've implemented some more dynamic interactions to make the environment feel more alive:

<iframe width="560" height="315" src="https://www.youtube.com/embed/05ul8jlkRAU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
<iframe width="560" height="315" src="https://www.youtube.com/embed/Yl0vYef7rJs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

These, especially the under water physics, still need some work in the future, but for only having spent a single day on them they already look quite promising. I have a few more ideas for dynamic interactions like that that I want to try and implement soon, too. All in all this should really help to make the world feel more alive and real, rather than being just a static map of tiles.

Sort of adjacent to this I created a custom distortion effect that'll be useful for indicating damage and death:

<iframe width="560" height="315" src="https://www.youtube.com/embed/BnmfaX5lkL4" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Then there were mechanical changes to the game: so far you could climb up walls indefinitely. I've decided to change this and implement a simple stamina system, as this will allow greater control over where the player can or cannot go, thus making it possible to block off certain routes and regions until later. It should also provide for more interesting platforming challenges and interactions with other elements like the rope.

Finally there were a bunch of good bugfixes [thanks to public feedback](https://github.com/Shinmera/kandria/blob/master/CHANGES.mess)! These aren't rolled out in the latest downloadable prototype yet, but they'll be in the next one, which I hope to release sometime this month. I've also automated deployment almost fully, allowing me to upload new updates at the push of a button. For now I'm keeping the public prototypes at a staggered release schedule, with a separate rolling release tester group on Steam. I'll provide more info about the Steam testing group once I have a better bug reporting system in place.

Then I've gotten back to re-integrating the dialog system. This is now pretty much done, the only missing component is the quest system that controls what dialog can be active at what point and things like that. I'll probably get to that next once I've rounded up some more issues with audio, though more on that in a second. The dialog system I have in Kandria is pretty powerful, and I've written a lengthy bit about it in the [weekly newsletter](https://kandria.com/#media). I also started working on profile animations for that:

![profile-1](https://filebox.tymoon.eu//file/TWpBMU5BPT0=)
![profile-2](https://filebox.tymoon.eu//file/TWpBMU5RPT0=)
![profile-3](https://filebox.tymoon.eu//file/TWpBMU13PT0=)
![profile-4](https://filebox.tymoon.eu//file/TWpBMU1nPT0=)

They'll need some more work though, as I'm not convinced they fit very well into the dialog box as I've got it so far.

![dialogbox](https://filebox.tymoon.eu//file/TWpBMU5nPT0=)

I think I'll have to try out anti-aliasing to smoothen the animations out some more. Maybe that'll make it feel more at home with the pretty crisp textbox. That's not very high on my list of priorities though, so I'll keep it for another time.

Finally, after months of pretty painful debugging and coding, I've made a breakthrough with my audio engine! It's now conceptually complete and just needs some good integration testing within Kandria. The good news is that, aside from a missing reverb implementation, it should offer everything I need for Kandria, and more, so using it for all of my future projects is definitely on the table. Since this has been such a long and arduous journey, I wanted to take some good time to explain the system. If you're interested in that, you can read about it [here](https://reader.tymoon.eu/article/387). Suffice to say, the next prototype release will finally have sound!

It'll be a bit before the sound in Kandria will be custom and accompanied by nice, composed music specifically for the game, though. Music and sound is something I've been thinking about for a long time, but I'm purposefully putting it off for much later, as I know it needs to fit the places, story, and characters, all of which have not been sufficiently worked out yet.

Looking back at the [roadmap](https://reader.tymoon.eu/article/384) that I published in June, it seems like I'm getting ahead pretty well despite the many issues that propped up along the way. A lot of the big blockers have already been fixed, and with a bigger team the rest should get ahead pretty well, too. I don't want to jinx it, but it's looking like the March deadline for the vertical slice is doable!

Alright, so to summarise the plan for this month: complete the job listing, re-integrate the quest system, add some preliminary sounds and music, improve the bug reporting, and release the 0.0.3 demo. We'll see how much of that, if not more, I get done by next month.

If you want to get the weekly updates with more details on what's going on until then, [subscribe to the newsletter](https://kandria.com/#subscribe)!
