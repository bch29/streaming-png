streaming-png
=============

`streaming-png` is a streaming PNG decoding library written in pure Haskell (without libpng) and based on [streaming](https://github.com/michaelt/streaming) and [streaming-bytestring](https://github.com/michaelt/streaming-bytestring) by [michaelt](https://github.com/michaelt). I plan to implement encoding and better support for PNG metadata soon too (see GOALS.md).

Based on current benchmarks, `streaming-png` consistently beats [JuicyPixels](https://github.com/Twinside/Juicy.Pixels) by about 5-10%, when decoding PNGs files from strict `ByteString`s to storable vectors of pixel data. Over a network, this difference should be much greater since `streaming-png` can decode incrementally as the file is downloaded, rather than having to wait until the whole file is in memory before starting decoding.

This library comes with a small interface to `zlib` through `streaming-bytestring` too. See [Streaming.Zlib](https://github.com/bch29/streaming-png/blob/master/src/Streaming/Zlib.hs). This may be factored out into a separate package in the future.
