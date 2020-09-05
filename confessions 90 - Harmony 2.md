![header]()  
It turns out that sound processing is pretty difficult. I've been hacking away at an almost from-scratch rewrite of Shirakumo's sound systems for the past few months and it's caused a lot of anguish. In this article I don't want to talk about all that miserable stuff though, and instead talk about how the system works and how you could use it.

Now, [Harmony](https://github.com/shirakumo/harmony) is not a new system. It dates back quite a few years, and I had even written another article on it [previously](https://reader.tymoon.eu/article/358). However, this previous version had several serious flaws and problems, some of which penetrating all the way through the audio stack. With the rewrite I'm however now much more confident in the quality and usability of the system.

First though, a bit of terminology: in digital audio processing, audio is represented as a sequence of samples; regularly recorded amplitudes of the audio signal. These samples are recorded at a constant rate, the "sample rate," which often is either 44.1kHz or 48kHz. Often each sample is represented as a float going from -1 to +1, and multiple such sample sequences are combined to form the signal for as many channels as you need (stereo, surround, etc.) A sample for each audio channel is then called a "frame." When processing audio data, a limited sequence of samples is kept in a buffer, which processors can then operate on.

The initial problem with the system was one of resampling: the system was written with the assumption that one could keep a constant samplerate throughout the entire audio processing pipeline. This, however, turned out to not be suitable. The issue manifested itself on Windows, where the output backend could require a different samplerate to the one the system was initially configured for. Thus, at least at the end-points, resampling would be required.

This immediately lead to another problem though: the system was also written with the assumption that every part of the pipeline could consume and produce a full audio buffer every time it was run. However, with resampling, border issues appear and it's not always possible to consume the full input buffer. This issue permeates throughout the processing pipeline, as now the final processor cannot consume all data, and so when the system is run next, the processor before the last cannot produce a full buffer as it would overwrite data.

Ultimately though, the fixed samplerate and fixed buffer size design lead to a restriction that made it impossible to represent certain effects like a speed change, which would produce and consume samples at different rates. And so, pretty much everything had to be rewritten to work with this in mind.


