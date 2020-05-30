module Aornota.Cribbage.Common.DebugOrRelease

let debugOrRelease =
#if DEBUG
    "Debug"
#else
    "Release"
#endif
