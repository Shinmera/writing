![header](http://shinmera.tymoon.eu/public/tumblr_n0nceqaQgj1qhttpto4_1280.jpg)
It's apparently been just 508 days since I first joined [github](http://buttcode.com). In that time I've written a lot of Common Lisp code and apparently made around 4000-5000 commits. I now want to make a retrospective and go over all the projects I've started. I'll omit some of the smaller, uninteresting ones though.

The projects are very roughly in the order I remember creating them. I can't recall exactly what it was, so things might be all over the place, but it matters not. An approximate order like this is sufficient.

## [lquery](https://shinmera.github.io/lquery)
This was my first big CL project that I started as I was investigating tools for radiance. Radiance already began conceptually before this, but I didn't write significant enough code for it to count. lQuery tries to bring the very convenient jQuery syntax for manipulating the DOM to CL. I did this because I knew jQuery and I did not find the alternatives very appealing. Initially it was supposed to help with templating for the most part, but it turned out to be more useful for other tasks in the end.

The first version of lQuery was written in a hotel room in Japan during my one-week holiday there. Time well spent! Don't worry though, I got out often enough as well.

## [lquery-doc](https://shinmera.github.io/lquery-doc)
lQuery was also my first library to be published and distributed via Quicklisp, so I needed it to have easy to read documentation. Docstrings are great, but I wanted that information to be on the documentation page as well, so I looked for libraries that allowed me to bundle that somehow. Given that I couldn't find anything I liked, I quickly wrote up my own thing that used lQuery to generate the page. It was a matter of some hours and made me very pleased at the time.

## [radiance](https://github.com/Shirakumo/radiance)
Radiance is sort of the reason I really got into CL to begin with. The previous versions of the TyNET framework that my websites run on were written in PHP and I got really sick of the sources, so I never really went to fix bugs or make necessary improvements. Things worked, but they didn't work great.

As I picked up CL I had to look for a project to really get used to the language and rewriting my framework seemed like the first, obvious step. I wanted Radiance to become a mature, stable and usable system that other people could profit from as well. So, unlike in previous attempts I tried to take good care to do things right, even if my understanding of the language at that point was questionable at best.

One and a half years and almost a complete re-write (again) later, I still don't regret choosing this as my major project, as I'm now fairly confident that it will become something that people can use in the future. It's not quite there yet, but well on its way.

## [piping](https://shinmera.github.io/piping)
I dislike breakpoints and love good logging, so the next step that Radiance demanded was a good logging solution. I first tried my hands on [log4cl](https://github.com/7max/log4cl), but didn't quite like it, mostly for a lack of being able to figure out how to make it work the way I wanted. So, rolling my own it was. I wanted something very flexible, so I though up a pipeline system for log message processing and distribution.

That was this library; a very small thing that allowed you to create (albeit in a cumbersome fashion) pipelines that could be used to process and distribute arbitrary messages.

## [verbose](https://shinmera.github.io/verbose)
From there on out I went to write the actual logger mechanisms, including threading support. Verbose was the result, and I still use and like it today.

## [cl-wiki](https://github.com/Shinmera/cl-wiki)
For a while then I was occupied with the task of writing a bot for the Encyclopedia Dramatica wiki that should handle new registrations and bannings by adding templates to the user pages. In order to make this possible I checked out a few IRC libraries and wrote a crude thing that would sit on a channel and accept simple commands.

In order for it to actually do its thing though, I had to interact with the mediawiki API, so I wrote a tiny wrapper library around some of the calls that I needed. I never put this on Quicklisp because it was never fleshed-out enough to be there and it still isn't. Maybe some day I'll revise this to be a properly usable thing.

## [xencl](https://github.com/Shinmera/xencl)
After I finished the bot I wanted to extend it to be able to interact with the forums of ED, which ran on XenForo. Unfortunately that forum offered absolutely zero APIs to access. There was a plugin, but I couldn't get the admins to install it as the forum was apparently so twisted that doing anything could make it crash and burn. Oh well.

So, I set out the classic way of parsing webpage content. Thanks to lQuery this was not that huge of a pain in the butt, but it still took a lot of fiddling to get things to run. This library too is not on QL as it is a big hack and far from complete as well.

## [colleen](https://shinmera.github.io/colleen)
At this point I'm really unsure about the order of the projects. Either way, the little bot project I made for ED was a mess and I wanted a proper bot framework to replace my previous bot, [Kizai](https://github.com/Shinmera/kizai). As I wasn't impressed by the available IRC libraries either, I wrote Colleen from scratch.

Colleen is still being worked on every now and again today, but (with some bigger and smaller rewrites along the way) it has proven to be a very good framework that I am very glad I took the time to write.

## [plaster](https://github.com/Shirakumo/plaster)
In order to test out Radiance and because I was sick of pastebin as a paste service, I set out to write my own. This, too, has proven to be a good investment of my time as I still use [plaster](https://plaster.tymoon.eu) as my primary pasting service today. There's a few things I'd like to improve about it whenever I do get the time to, but for the most part it just works.

## [chirp](https://shinmera.github.io/chirp)
At some point I noticed that I'd like to have twitter interaction for some of my web-services, so I looked around for API interfaces for that. However there wasn't anything that really worked well. So, once more I went to write something that fit my needs.

This was my first really frustrating project to get going, mostly because figuring out how oAuth is supposed to work is a huge pain. Web-APIs are some of the worst things to interact with, as often enough there is absolutely no way to figure out what exactly went wrong, so you're left stumbling in the dark until you find something that works.

Even though I haven't really used Chirp much myself, it seems to have been of use to a couple of people at least, if Github stars are anything to go by.

## [south](https://shinmera.github.io/south)
Since oAuth is a repeating pattern on the web and it was sufficiently painful to figure out for Chirp, I segregated that part out into its own library. I'm not sure if anyone aside from myself has used South for anything though.

## [Universal-Config](https://shinmera.github.io/Universal-Config)
During one of my rewriting iterations of Colleen I noticed that a very common pattern was to save and load some kind of storage. Moving that pattern out into the framework and thus automating configuration and storage seemed like a good idea. However, since Colleen was also an end-user application, I needed to make sure that the configuration could be saved in a format that the user wanted, rather than simple sexprs.

And that's what Universal-Config is supposed to do: Generalise the access of configuration as well as the storage. It works really well on the code side; accessing parts and changing the config is very simple and convenient. It only works so-so on the configuration storage side of things though, as I needed to strike some gross compromises in the serialisation of the objects to ensure compatibility between formats.

Maybe some day I'll figure out a smarter solution to the problems UC has.

## [deferred](https://shinmera.github.io/deferred)
Deferred was an attempt at providing mechanisms for optional features of your code. Meaning that your could would work depending on what kind of libraries are loaded at the time. Therefore I could for example provide a local server based authentication with South without explicitly requiring Hunchentoot or some other webserver. Deferred is more a proof-of-concept than anything though, as I haven't actually utilised it in any of my projects.

However, the problem is an interesting one and whenever I do return to it, I want to try to tackle it from a different angle (extending ASDF to allow something like optional dependencies and conditional components).

## [plump](https://shinmera.github.io/plump)
The first version of lQuery used [Closure-HTML](http://common-lisp.net/project/closure/closure-html/), [CXML](http://common-lisp.net/project/cxml/), and [css-selectors](https://github.com/AccelerationNet/css-selectors) to do most of the work. However, CHTML and CXML suffered from big problems: CXML would not parse regular HTML (of course) and CHTML would not parse HTML5 as it required a strict DTD to conform to. Also, css-selectors' performance wasn't the greatest either.

So, in order to clean up all these issues I set out to write my own HT/X/ML parser that should both be fast and lenient towards butchered documents. Well, fast it is, and lenient it is as well. Plump is probably so far my best project in my opinion, as its code is straight-forward, extensible, and just does its job very well.

## [CLSS](https://shinmera.github.io/CLSS)
The next step was to build a CSS-selectors DOM search engine on top of Plump. This turned out to be quite simple, as I could re-use the tools from Plump to parse the selectors and searching the DOM efficiently was not that big of a deal either.

After these two were done, the last job was to re-write lQuery to work with the new systems Plump and CLSS provided. The re-write was a very good idea, as it made lQuery a lot more extensible and easier to read and test. It was quite funny to read such old code, after having worked with CL for about a year by then.

## [clip](https://shinmera.github.io/clip)
The templating engine I used in Radiance so far had been a combination of lQuery and "uibox", which provided some very crude tools to fill in fields of nodes on the DOM. I didn't like this approach very much as there was too much lQuery clutter in the code that should've been in the template.

Clip now provides a templating system that hasn't been done in CL before and I don't think has really been done ever. All the code that manipulates your template is in the template itself, but the template is a valid HTML5 document at all times. The trick is to take advantage what HTML already allows you to do: custom tags and attributes. Clip picks those up, parses them and then modifies the DOM according to their instructions. All you have to do in your CL code is to pass in the data the page needs.

## [staple](https://shinmera.github.io/staple)
lQuery-Doc left a lot to wish for, so another rewrite was in order. This time I took advantage of Clip's capabilities to provide a very straight-forward, no-bullshit tool to generate documentation.

The only drawback it has currently is that its default template doesn't have the greatest stylesheet in the world, but that hardly bothers me. Maybe I'll get to writing a fancy one some day.

## [parasol](https://github.com/Shinmera/parasol)
I always wanted to write my own painting application, mostly because [MyPaint](http://mypaint.intilinux.com/) and others were never completely to my liking. I even took attempts at this before in Java. At some point, out of curiosity, I looked into how I would go about grabbing tablet input. Investigating the jPen library brought me absolutely nothing but confusion, so I looked for other ways. Luckily enough it turned out that [Qt](http://qt-project.org/) already provided a built-in way to grab events from tablets and from previous experience with a minor project I knew that [CommonQt](http://common-lisp.net/project/commonqt/) allowed me to use Qt rather easily from CL out.

So, what started out as a quick test to see whether it would even be possible to make a painting application quickly turned into a big thing that had a lot of potential. You can read more about it [here](http://blog.tymoon.eu/article/294).

## [modularize](https://shinmera.github.io/modularize)
A lot of time had passed since I last worked on Radiance. I took time off as I noticed that the framework had turned into something uncanny and I needed to fix that. And the way to fix it was to write a lot of design drafts and work out all the issues that came to mind on paper.

My conclusion after all this was: Radiance needed a complete, from scratch, rewrite. Oh boy. The first part that needed to be done is a proper library to provide the encapsulation into modules. Modules are Radiance's primary abstraction that allow you to neatly separate parts, but also unify the access and interaction between them.

Modularize was the solution for this and it works pretty well. In fact, it works so well that I don't even think about it anymore nowadays, it just does its job as I expect it to. Aside from Modularize itself I wrote two extensions that tucker on support for triggers and the much-needed interfaces and implementations mechanism that is vital to Radiance. I won't explain what these do exactly right now, that'll be for when I write the comprehensive guide to Radiance.

## [reader](https://github.com/Shirakumo/reader)
After a long time of rewriting Radiance's core and contribs, it was time to rewrite another component from the old version of TyNET: my blog. This time I tried to focus on simplicity and getting it done well. Simple it is indeed, it's barely 200 lines of code. And as you can probably see as you read this, it works quite nicely.

## [LASS](https://shinmera.github.io/LASS)
Writing CSS is a pain in the butt, as it involves a lot of duplication and other annoyances. At some point I had the idea of writing a Lisp to CSS compiler. Taking inspiration from [Sass](http://sass-lang.com/) this idea grew into LASS in a matter of.. a day or two, I think?

I now use LASS for all of my style sheet writing concerns as it just works very well and with some minor emacs fiddling I don't even have to worry about compiling it to CSS myself.

## [humbler](https://shinmera.github.io/humbler)
Sometimes [Xach](http://xach.com/) would talk on IRC about wanting to interact with Tumblr through CL. As Tumblr is a service I use too and the biggest hurdle (oAuth) was already handled by South I took the challenge of writing yet another web-API client.

Humbler turned out a lot nicer than Chirp did in terms of end-user experience, I would say. However, I cannot at all say the same about my own experience while writing it. Tumblr's API "documentation" is quite bad, to be blunt. A lot of the returned fields are not noted on the page, some things are plain wrong (probably out of date) and in general there's just not enough things actually being documented. The worst part about it all was the audacity that the staff had to proclaim in a blog post that they wanted to encourage experimentation!, as if having to figure out the API by yourself was actually a great thing.

Anyway, I haven't actually used Humbler for anything myself, but some people seem to be using it and that's good enough to me.

## [ratify](https://shinmera.github.io/ratify)
Returning back to Radiance problems, one of the recurring issues was validating user input. There didn't seem to be a library that did this in any way. And so the same old game of 'write it yourself' began. Ratify's development mostly included reading RFCs and trying to translate them into tests and finally bundling it all together in some easy to use macros.

## [dissect](https://shinmera.github.io/dissect)
On twitter I encountered a really nice screenshot of an error page on some Clojure project. I couldn't find the tweet again later, so I don't know what features it had exactly, but suffice to say it was better than anything I'd seen up to that point.

That lead me to wonder how I could actually get the stack trace myself if an error occurred. There was already a library that provided rudimentary support for that, [trivial-backtrace](http://www.cliki.net/trivial-backtrace). Taking a look at its source code filled me with everything else but esteem though, so I headed out to write something that would allow people to inspect the stack, restarts, and accompanied source code easily.

## [softdrink](https://shinmera.github.io/softdrink)
A quick question by [eudoxia](https://twitter.com/eudxa) on twitter inspired me to write a very quick toolkit to extract and infuse CSS from/into HTML. The main use case for the former would be to turn HTML into HTML+CSS and the latter to reverse the process (for, say, emails). Using lQuery and LASS this turned out to be a super easy thing to do and I had it done in no time.

Hooray for great code re-use!

## [purplish](https://github.com/Shirakumo/purplish)
Aside from the blog, the only really actively used component of TyNET was the imageboard, Stevenchan. Stevenchan ran on my own software called Purplish. In order to be able to dump everything of the old-code base I was driven to re-write Purplish for Radiance.

However, Purplish now takes a much different approach. A lot of traditional imageboard features are missing and a couple of unconventional features were added. Plus, having it written in CL has the advantage of being much easier to maintain, so if anything ever does crop up I'll tend much more towards wanting to fix it than I did before with PHP.

## [keyword-reviews](https://github.com/Shirakumo/keyword-reviews)
I like language a lot. I also like to try and reduce things to their minimum. So, the idea came to me of a site that allowed people to review things with only a single keyword. The idea behind that was to, with sufficient data, see what kind of patterns emerge and find out what people think the essence of an experience is.

Certainly it wouldn't be useful for an actual 'review' of anything, but it's nevertheless an interesting experiment. I don't know if I'll ever get enough data to find patterns in this or anything that could lead to scientifically significant correlations, but it's a fun enough thing on its own.

## [qtools](https://shinmera.github.io/qtools)
Having completed pretty much everything that I wanted to work on and stalling on some major issues with Radiance I was on the lookout for things to do. Parasol was still on hold and nothing else really picked my interest. In an attempt to start out right and not dive head over heels into it again, I first considered ways in which to make the C++-ness of Qt more lispy.

Born out of this was Qtools, a collection of tools to aid development with CommonQt and make it all feel a bit more homely. Of course, some major issues still remain; you still need to pay attention to garbage and there's still C++ calls lingering about, but all in all the heavy hackery of Qtools does make it more pleasing to the eye.

Qtools forced me to go deeper into the guts of CLOS and MOP than I've ever gone before and I had to spend a lot of time in implementation code to figure out how to make the things I needed work. I wouldn't advise Qtools as a good use of MOP, but it could be considered an impressive work in exercising the flexibility Lisp offers.

--

So, that's it then for now. I'd like to amend here that during the most part of all these projects I should've been studying for university. I'm not sure if working on these projects was the right choice, but I have learned a huge bunch and I hope that my produce of my efforts has been of use to other people. If not, then it certainly was not the right choice to indulge myself this much in programming.

Before I go into another long rant about my questionable situation in university I'll cap this here. Until another time!

Post scriptum: If you have ideas for features or new projects for me to work on, please let me know! More ideas is always better.

![footer](http://shinmera.tymoon.eu/public/tumblr_n74mdco2VV1qhttpto4_1280.jpg)
