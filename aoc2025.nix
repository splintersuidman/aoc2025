{ mkDerivation, attoparsec, base, bytestring, containers, criterion
, directory, exceptions, filepath, fin, fused-effects, lens, lib
, modular-arithmetic, mtl, optparse-applicative, streamly
, streamly-bytestring, streamly-core, text, time
, unordered-containers, vec, vector
}:
mkDerivation {
  pname = "aoc2025";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers criterion directory
    exceptions filepath fin fused-effects lens modular-arithmetic mtl
    optparse-applicative streamly streamly-bytestring streamly-core
    text time unordered-containers vec vector
  ];
  executableHaskellDepends = [
    attoparsec base bytestring containers criterion directory
    exceptions filepath fin fused-effects lens modular-arithmetic mtl
    optparse-applicative streamly streamly-bytestring streamly-core
    text time unordered-containers vec vector
  ];
  homepage = "https://github.com/splintersuidman/aoc2025";
  description = "Advent of Code 2025";
  license = lib.licenses.gpl3Plus;
  mainProgram = "aoc2025";
}
