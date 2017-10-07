![header](https://filebox.tymoon.eu//file/TVRReE53PT0=)  
I've written about [Portacle](https://portacle.github.io) on a [previous occasion](https://reader.tymoon.eu/article/350), where I talked mostly about the issues I've faced. This time, however, I'm excited to announce that Portacle has finally reached version 1.0. This means that there are no obvious remaining issues that I am aware of. Everything should Just Work™.

In case you're confused about what Portacle even is, it stands for the Portable Common Lisp development Environment. It's a combination of Emacs, SBCL, Quicklisp, GIT, and a variety of other, smaller components that together bring you a fully-fledged IDE that runs on the three major operating systems in use today. It is installable with a simple extraction and fully contained in its own directory. It can thus be loaded onto a USB stick for use on the go as well.

Portacle is primarily intended to target both complete newcomers, for which the installation procedure of a full Emacs setup otherwise involves a lot of confusing and complicated steps, and advanced users that simply need a quick way to set up a running environment on a machine. Portacle is especially convenient to test your libraries on different systems.

I have personally tested Portacle to run properly on the following platforms:

* Windows 7
* Windows 10
* OS X 10.11
* OS X 10.12
* Ubuntu 16.04
* Linux Mint 17.3
* Debian 8
* Fedora 25
* Arch Linux

Note that currently the following platform versions are supported:

* Windows 7+ x64
* OS X 10.11+ x64
* Linux 3.13+ x64

You can download the current release [here](https://github.com/portacle/portacle/releases/tag/1.0). If your system falls within these constraints and Portacle won't run properly for you, please do [file an issue](https://github.com/portacle/portacle/issues/new) so that I can see what else needs fixing.

If you otherwise have suggestions regarding documentation extension, adding features, or smoothing out rough edges, please file an issue as well, or hop onto the #shirakumo IRC channel on Freenode to chat directly with me. I'd be happy to hear your thoughts.
