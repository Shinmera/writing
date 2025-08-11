![header](https://filebox.tymoon.eu//file/TVRjNE5RPT0=)  
Recently there's been a few changes to the libraries I maintain, as well as to the new libraries I publish. I thought that these changes would be of general interest, even if you might not use my libraries at all.

## License Switch from Artistic 2 to Zlib
Previously all of my libraries were published under the [Artistic 2](https://opensource.org/licenses/Artistic-2.0) license. However, a problem was brought to my attention in that license, specifically prevailing to Lisp distributions. Namely, §8 in the license text:

> 8) You are permitted to link Modified and Standard Versions with other works, to embed the Package in a larger work of your own, or to build stand-alone binary or bytecode versions of applications that include the Package, and Distribute the result without restriction, provided the result does not expose a direct interface to the Package.

The problem being this, in specific:

> provided the result does not expose a direct interface to the Package.

This seems to prohibit distributing an application that would expose something like a REPL or a scripting interface that allows the user to interface with the library. I'm not sure what "direct" means in this context, nor what "linking" really means for Lisp -- the GPL licenses have a similar issue. Either way, the implications of this restriction are severe enough that I was convinced to abandon the license.

I have since changed the license of all of my libraries and projects to [Zlib](https://opensource.org/licenses/Zlib). Everyone is also hereby permitted to use any prior versions of my projects that were licensed under Artistic 2 under the zlib license terms.

Why Zlib and not MIT or BSD? Simply because I like the clauses that prohibit claiming credit for the software. In any case, I hope that alleviates potential concerns people had about using my software due to the license terms.

## Fully Qualified Domain Names for Packages
Since the [package local nicknames](http://sbcl.org/manual/#Package_002dLocal-Nicknames) extension to Common Lisp is now [supported widely enough](https://shinmera.com/docs/portability/#trivial-package-local-nicknames) for my tastes, I have decided to drop the short nickname I used to include for packages so far. All of my packages have always included a FQDN name, but also included a shorter nickname for convenience. Newly published libraries will not do this anymore.

You are now encouraged to use the packages by adding local nicknames to your own package definitions. For instance, to use the new [Classowary](https://shinmera.com/docs/classowary) library, you would do something like this:

```commonlisp
(defpackage #:org.my.stuff.package
  (:use #:cl)
  (:local-nicknames
    (#:cass #:org.shirakumo.classowary)))
```

If you use an implementation -- like Allegro or LispWorks -- that does not support package local nicknames yet, please contact support and request the addition of this feature. It should not be a difficult feature to add, and there is a [comprehensive test suite](https://github.com/phoe/trivial-package-local-nicknames) available to aid the process.

If you are a user of my packages and have so far been using the short name, please update your own packages to use a nickname. You won't have to change any other code as long as you do that. You should do this because I would like to alleviate the package namespace pollution problem by removing the short, global aliases from my libraries in the future. Thus, consider the short names to be officially *deprecated*.

## Closing Thoughts
I hope that with these changes I can help push the ecosystem in a positive direction. I would also like to remind everyone of the [portability effort](https://shinmera.com/docs/portability). I think the variety of implementations on offer is a big asset to Common Lisp, and I would very much like it to be possible for people to write high quality libraries that work on a variety of implementations without having to expel a huge amount of time repeating the porting work. As such, it would be amazing if people could help to improve existing portability libraries and implementations to extend support coverage.

More articles and Lispy things coming soon, I hope!
