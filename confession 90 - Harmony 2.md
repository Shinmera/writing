![header](https://filebox.tymoon.eu//file/TWpBMk1BPT0=)  
It turns out that sound processing is pretty difficult. I've been hacking away at an almost from-scratch rewrite of Shirakumo's sound systems for the past few months and it's caused a lot of anguish.

Now, [Harmony](https://github.com/shirakumo/harmony) is not a new system. It dates back quite a few years, and I had even written another article on it [previously](https://reader.tymoon.eu/article/358). However, this previous version had several serious flaws and problems, some of which penetrating all the way through the audio stack. With the rewrite I'm however now much more confident in the quality and usability of the system.

First though, a bit of terminology: in digital audio processing, audio is represented as a sequence of samples; regularly recorded amplitudes of the audio signal. These samples are recorded at a constant rate, the "sample rate," which often is either 44.1kHz or 48kHz. Often each sample is represented as a float going from -1 to +1, and multiple such sample sequences are combined to form the signal for as many channels as you need (stereo, surround, etc.) When processing audio data, a limited sequence of samples is kept in a buffer, which processors can then operate on.

The initial problem with the system was one of resampling: the system was written with the assumption that one could keep a constant samplerate throughout the entire audio processing pipeline. This, however, turned out to not be suitable. The issue manifested itself on Windows, where the output backend could require a different samplerate to the one the system was initially configured for. Thus, at least at the end-points, resampling would be required.

This immediately lead to another problem though: the system was also written with the assumption that every part of the pipeline could consume and produce a full audio buffer every time it was run. However, with resampling, border issues appear and it's not always possible to consume the full input buffer. This issue permeates throughout the processing pipeline, as now the final processor cannot consume all data, and so when the system is run next, the processor before the last cannot produce a full buffer as it would overwrite data.

Ultimately though, the fixed samplerate and fixed buffer size design lead to a restriction that made it impossible to represent certain effects like a speed change, which would produce and consume samples at different rates. And so, pretty much everything had to be rewritten to work with this in mind. To spare you the troublesome process of figuring out a design, let's just jump to what the system is like now:

At the most basic level resides the `bip-buffer` interface, which implements a lockless bipartite buffer. It's lockless so that one thread can write, and another can read from it simultaneously. It's bipartite so that the regions it hands out are always consecutive regions of memory, rather than wrapping around like in a ring buffer. This interface is implemented by ``buffer``s and ``pack``s. ``buffer``s represent internal audio samples of one channel in ``float`` format, whereas ``pack``s represent external audio samples in any format, with any number of channels.

Then there's the parts that actually perform audio processing. These are called `segment`s, and follow a generic interface that allows them to do their work, and also allows them to be introspected. Namely they each have a number of input fields, a number of output fields, and a number of parameter fields. To the input and output fields you can attach a ``buffer``, which will cause the ``segment``s to exchange data. Assembling a network is then just a matter of creating the ``segment``s, creating a ``buffer`` for each connection, and then setting them at the appropriate in/out fields.

At the endpoints, where you need to exchange data with other systems such as file decoders or device drivers, you'll probably want to make use of the ``unpacker`` and ``packer`` segments, which perform the necessary encoding to translate between the float ``buffer``s and the compact ``pack``s. These segments will also perform sample rate conversion as necessary.

Since we have proper bip buffers connecting everything, a ``segment`` can now consume and produce at a variable rate without needing to be aware of the rates going on in the rest of the system. The rates will automatically propagate through the system as the buffers are updated.

Now, all of this behaviour, including many practical standard ``segment``s are implemented in a C library called [libmixed](https://github.com/shirakumo/libmixed). Audio has some pretty severe latency restrictions, and that's why, with great pain, I decided to implement the bulk of the audio processing in C, rather than Lisp. This has cost me a lot of time, but I still think the performance gains are worth it, or I would have had to spend similar, if not more time, trying to match the performance with Lisp code. I hope that this kind of thing will no longer be necessary at some point in the future, but for now this is where we are.

Anyway, being implemented in C also means it can be useful for people outside of Lisp, and I really do hope that others will take advantage of libmixed, as I think it has a lot of useful work behind it. To my knowledge there's currently no free (as in BSD) and capable audio processing system out there. The library also offers a plugin and reflection/introspection API so that one could build a GUI that can represent segments and buffers in a very generic fashion, allowing users to easily plug together processing networks.

Now, one level above libmixed sits [cl-mixed](https://github.com/shirakumo/cl-mixed), the Lisp bindings library that takes care of the low level stuff and wraps it all in a nice Lisp interface. It also takes care of offering some support structures where needed, such as managing the input locations when dealing with variable input segments such as mixers. It also offers a ton of extension systems for interacting with various file formats and playback backends:

- ``ALSA`` Linux playback
- ``CoreAudio`` macOS playback
- ``FLAC`` FLAC file decoding
- ``Jack`` JackAudio playback
- ``OSS`` OSS playback (BSD)
- ``PulseAudio`` Linux desktop playback
- ``SDL2`` SDL2 integration if you're already using SDL2
- ``WASAPI`` Windows Vista+ playback
- ``WAV`` WAV file decoding
- ``WinMM`` Windows 3.0+ playback
- ``XAudio2`` Windows 8+ playback
- ``mpg123`` MP3 decoding
- ``out123`` Cross-platform playback (C blob)

I'd like to add more decoders, and at some point also input for the various operating system backends, but for now this is more than plenty. Some of the backends still have issues (WinMM, XAudio2, CoreAudio), which I have spent a long time trying to figure out already, so far unsuccessful. I'm not too bothered about WinMM and XAudio2, but CoreAudio definitely needs to be made to work properly soon.

The reason these backends are implemented in Lisp is so that there's no additional dependencies on shared libraries that might be versioned and interact poorly when deployed. Since the actual work performed in their respective segment amounts to requesting a buffer region and performing one call, the performance impact from it should also be entirely negligible.

cl-mixed also offers a ``virtual`` segment that allows you to implement a segment in Lisp and integrate it into a standard pipeline. This is possible thanks to the standardised architecture in libmixed, and can be very useful to experiment with effects very quickly. If I ever intend on developing a new effects segment, I'll definitely implement it in Lisp first to take advantage of rapid prototyping, before lowering it down to C if performance should become an issue.

On that note, cl-mixed actually uses static-vectors to implement the backing storage of ``pack``s and ``buffer``s, as well as all of the bip-buffer protocol. This means that you can interact with packs and buffers from Lisp as if they were normal Lisp arrays, without ever having to worry about FFI.

That said, cl-mixed will not do buffer management or resource management in general for you. You'll still have to manually create and free segments and buffers and make sure they're connected. You'll also have to run the mixing loop yourself and make sure you do that often enough to not cause stuttering.

This is where [Harmony](https://github.com/shirakumo/harmony) steps in. Being the high-level component, it imposes a bit of architecture on you, but in turn takes care of a lot of lower level plumbing. In effect, with Harmony you can perform playback as easily as:

```
(harmony:start (harmony:make-simple-server))
(harmony:play "music.mp3" :mixer :music :loop T)
(harmony:play "effect.wav" :mixer :effect :location '(10 0 0))
```

It'll take care of detecting the appropriate backend for your platform, setting up channel conversion and basic mixing infrastructure, allocating and re-using buffers, automatically cleaning up when a sound source ends, and performing low-latency audio processing in the background.

It can also do fun stuff like automatically creating a network to apply effects to a source.

```
(harmony:play "music.wav" :mixer :music :effects
  '((mixed:speed-change :speed-factor 2.0)
    (mixed:pitch :pitch 0.5)))
```

Which would play the music at double the speed, but with a pitch correction applied so that the notes should still be the correct frequency.

Hopefully this will make it easy enough to use for games without having to worry about all the low level detail aspects. I'm going to find out how well this all works soon, as it's now at a stable enough state that I can start working it into [Kandria](https://kandria.com).

If you're interested in using these systems or contributing to them, [let me know](mailto:shinmera@tymoon.eu)! I'd be happy to provide assistance.

If you like my work in general and want to donate, you can do that too, either on [GitHub Sponsors](https://github.com/sponsors/Shinmera) for recurring donations, or on [Ko-Fi](https://ko-fi.com/shinmera) for one-time donations.

Thanks for reading!
