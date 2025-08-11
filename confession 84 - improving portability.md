![header](https://filebox.tymoon.eu//file/TVRjek5nPT0=)  
Common Lisp has a big standard, but sometimes that standard still doesn't cover everything one might need to be able to do. And so, various implementations offer various extensions in various ways to help people get their work done. A lot of these extensions are similar and compatible with each other, just with different interfaces. Thus, to make libraries work on as many implementations as possible, portability libraries have been made, and are still being made.

Unfortunately, writing one of these libraries to work across all common implementations, let alone all existing implementations, is a tremendous amount of work, especially since the implementations that are still maintained change over time as well to add, remove, and improve their extensions. Thus, quite a few of the portability libraries that exist do not support all implementations that they could, or not as well as they could. Similarly, not all implementations provide all extensions that they could, and probably should (like [Package-Local Nicknames](http://sbcl.org/manual/#Package_002dLocal-Nicknames)).

In hopes of aiding this process, I have decided to create a small project that documents the state of the various portability libraries and implementations in a way that shows the information at a glance. You can see the state [**here**](https://shinmera.com/docs/portability/) and help improve it if you see a missing or incorrectly labelled library [on GitHub](https://shinmera.com/project/portability).

It would make me overjoyed to see people help out the various projects to support more implementations, and to lobby their favourite implementation to take up support for new extensions*. I'm the maintainer of a few of the libraries listed on the page as well (atomics, definitions, dissect, float-features, trivial-arguments), and naturally I would very much welcome improvements to them. It would be especially great if the support for LispWorks in specific could be added, as I can't seem to get it to run on my system.

In any case, I hope that this project will nudge people towards caring a bit more about portability in the Lisp ecosystem. Maybe one day we'll be able to just pick and choose our implementations and libraries without having to worry about whether that specific combination will work or not. I think that would be fantastic.

*I'm looking at you, Allegro and LispWorks. Please support Package-Local Nicknames!

---

On a side note, I know I haven't been writing a lot of stuff here lately. I hope to correct that with a couple of entries coming in the next few days. As always, thanks for reading!

