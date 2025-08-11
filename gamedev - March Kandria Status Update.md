![header](https://filebox.tymoon.eu//file/TVRnNU1BPT0=)  
Another month has flown by, and this one has turned out to be much more exciting than I ever anticipated! Primarily this is due to me making a surprise decision to try and apply for a grant to help get this project funded. This prompted a lot of changes, which I'll talk about today.

If you're new to Kandria (formerly named Leaf), you can find a brief description of the game in the [trailer embedded below](#trailer), and a longer description in the document linked after.

Now, before we get to talking about the development, I'll talk a bit about the grant and how it came to me applying for it. It starts in the late summer of 2019, when I finally decided to push myself a bit outside my usual social boundaries and visited the [Zürich Game Developers](https://www.meetup.com/Game-Developers-Zurich/) meetup. From there I learned about [Zürindies](https://www.meetup.com/zurindies/) a weekly get together, which I started to attend every Saturday. This has been absolutely fantastic for my morale and my interest in working seriously on the game. Fast forward a couple of months, and some people banded together to open a shared space for game developers in Zürich called the [Swiss Game Hub](http://www.swissgamehub.com/). After hearing that most of my fellow Zürindies people decided to take up the offer and work there a couple of days every week, I made the jump as well. I've been there twice a week since January now, and it has been pretty good!

Sometimes there's also small events being held at the game hub, one of which was about the [Pro Helvetia](https://prohelvetia.ch/en/) institution, a Swiss culture fund with a division for interactive media, or simply: games. They offer grants for both studios and indies alike, with different categories for pre-production, production, and post-production. With a little bit of a push from the other Zürindies attendees, I made the decision to try for the pre-production grant as well. The past two weeks have then been a rush to get things ready and done for the grant application, which is due today.

In order to apply for the grant there's a bunch of restrictions, but also a bunch of things you need to hand in:

* A game design document pitching the game
* A budget showing your expected expenses and income
* Market analysis to show what you're targeting and what sales you project
* A short trailer to describe your game
* A playable demo of the game
* A CV for every person on the core development team
* A website (Optional)

That's a lot of stuff to get going in two weeks, especially when you don't work full time on the project and there's a lot about the game that's still very brittle. I also had no experience with budgeting at all, so that part caused me a lot of confusion.

Nevertheless, the application was completed! Now it's all up to the jury to see whether they like my pitch well enough over the surely many others to fund it. According to what others have said, it'll take a few months for the jury to make the decision. I don't intend on relying on the grant for the development of the game though, so I won't wait with bated breath and just proceed as I would have anyway.

One of the most important changes I made along the application preparation was to finally pick a proper name for the project. Some of you may still remember the game with its prototype name "Leaf", which was only picked because I had to use anything at all to name the project. After lots of deliberation and indecision I finally settled on "Kandria". The name itself has no meaning, it was picked to sound good and avoid collision with other games and projects. I think it does a pretty good job at that.

Now that the application is complete, I decided it would be best to publish all of that material I had to make for it too, and use it to do my first serious attempt at marketing Kandria a bit. So, without further ado, here's the pitch trailer for the game:

<iframe id="trailer" width="560" height="315" src="https://www.youtube.com/embed/H2ySVyMybeQ" frameborder="0" allow="encrypted-media; picture-in-picture" allowfullscreen></iframe>

I'm quite happy with what I managed to put together in less than a week of time, though I'm especially happy that I decided to hire [Alex Gray](https://www.fiverr.com/starkyrie) to act out the couple of lines in the video. It works so much better than my initial attempt of just having subtitles.

I'm much less happy with the gameplay scenes in the video, since those were literally only put together yesterday, and the short level I made for it is lacking in a lot of ways. I would have especially loved to include some combat or dialogue interaction, but unfortunately neither of those systems were ready in time. I'll focus on those very soon, though, so hopefully I'll be able to get a much better demo together before long.

The game design document with the pitch, budgeting, and market analysis stuff is also available publicly if you're interested in what I put together for that:

<https://shinmera.com/project/kandria/blob/master/doc/pitch/pitch.pdf>

I'm sure that someone experienced in finance would tear their hair out over what I threw in for the budget, but hopefully it'll be good enough for the Pro Helvetia jury. Another important thing the document includes though is a rough schedule: I hope to finish pre-production and enter the full production phase in Summer 2020 (that's this year). This is not a lot of time, especially with me still having to attend lectures for my Master's, but I think it should be doable. There's not that much that still needs to be added for the engine, and most of it is bugfixes and reintegrating older systems that have grown stale.

Once production starts I gave the very optimistic estimate of a year until release, meaning Summer 2021, which I don't think is realistic. Depending on how things go, how much help I'll be able to afford, and so forth I think it might be possible, but a more reasonable target would be 2022.

Making such a large game is probably not the best of ideas for a first title of an indie --- in fact I know it's a terrible idea --- but it is just what I want to do, and damn it, I will try and fail to do what I want first instead of cutting my standards preemptively.

Another thing that came out of the for the grant application is a website! I don't have a dedicated domain for that yet, so it's currently just living on Github:

<https://shinmera.com/docs/kandria>

I think that'll be fine enough until I enter production at least. On the site you can also get a copy of the prototype demo, and sign up for a mailing list. I'll definitely put these monthly updates onto the list, but I'll probably also share some exclusive content there.

Finally there's now also a [Discord for Kandria](https://discord.gg/WNTygau) in case you want to hang out and chat about the game and its development with other interested people, and myself, of course.

There wasn't a lot of time left besides the grant application to work on the game itself. What I did do was focused on improving the editor, fixing bugs, [allowing steam integration](https://twitter.com/Shinmera/status/1231568995519737861), [making the gamepad library lisp-native](https://shirakumo.org/docs/cl-gamepad), testing deployment on Windows and Linux, and getting started on the animation editor I [talked about last month](https://reader.tymoon.eu/article/380). There's not a lot of visual stuff to show off for all of that either, I'm afraid.

Once I get some more collision and UI bugs fixed I'll continue with the animation editor, so I can finally start working on the combat system. If things go well I should have a very interesting update for you next month!

If you want to stay in touch with news about Kandria, you can use the [mailing list](https://shinmera.com/docs/kandria#subscribe), [Discord](https://discord.gg/WNTygau), [twitter](https://twitter.com/shinmera), [gamedev.net](https://gamedev.net/projects/1608-kandria/), or the [Atom feed](https://tymoon.eu/api/reader/atom?tag=kandria).

As always, thanks a *lot* to everyone who shows interest in this project and the other things I do, it really means the world to me to see that people care!
