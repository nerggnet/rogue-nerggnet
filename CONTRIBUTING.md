# Contributing

Thanks for looking. This is a small, deliberately old-fashioned project: a
terminal Rogue-like where **the dungeon is data and the code is a machine for
playing it**. Most of what you might want to change — a new floor, a new
monster, a new item, a new scripted event — is an edit to `world.json` and
needs no Haskell at all.

`README.md` is the reference for the game and for the whole `world.json`
schema. This file is about how to work on the repository.

## What the project is trying to be

Three intentions shape almost every decision here, and a change that works
against one of them is likely to be sent back:

1. **The dungeon is authored, not generated.** The world is fixed and
   hand-drawn so that two people's runs can be compared: the same floors, the
   same monsters, the same treasure. Procedural generation would make the
   score meaningless. New content means new floors, not a new generator.
2. **Content mistakes are caught before the game starts.** A hand-drawn map is
   easy to get wrong — one wall in the wrong place seals off a room and nobody
   notices until a player is stuck in it. So `world.json` is validated at load
   and every problem is reported together. A silent failure at run time is a
   missing validator.
3. **The dungeon is proved beatable by machine.** `Game.Autoplay` plays the
   shipped dungeon in the test-suite. It is deliberately unclever and never
   retreats, so a dungeon it beats is beatable by a person. An impossible
   floor fails the build, and so does a trivial one.

## Getting set up

* GHC 9.12 or 9.14 and `cabal`, easiest via [ghcup](https://www.haskell.org/ghcup/)
* A terminal with 24-bit colour

```bash
cabal build all
cabal test --test-show-details=direct
cabal run rogue-nerggnet
```

All three must be run **from the repository root**: the game reads
`world.json` and reads/writes `save.json` relative to the working directory,
and the specs load the real `world.json`.

Haskell Language Server works out of the box; `cabal.project` already enables
tests so HLS sees the test-suite.

## Before you open a pull request

CI builds on Linux and macOS across GHC 9.12.2 and 9.14.1, runs the suite,
checks the package metadata and runs hlint. Reproduce all of it locally:

```bash
cabal build all --ghc-options=-Werror   # CI builds this package with -Werror
cabal test                              # the full suite, from the root
cabal check                             # package metadata
hlint src test app                      # must print "No hints"
```

The hlint job uses `fail-on: suggestion`, so a suggestion is as fatal as a
warning. There is no lint config carving out exceptions, on purpose.

## Code conventions

* **Brick and Vty appear only under `src/UI/`.** `Game.*` and `File.*` are
  UI-independent, which is what lets the logic be tested without a terminal.
  Do not reach for a Brick type in `Game.Logic`.
* `-Wall` everywhere, `OverloadedRecordDot` on by default.
* Comments explain *why*, and are written in plain prose. There are few of
  them; the ones that exist mark something that was got wrong once.
* Every source module has a matching `test/…Spec.hs`. New behaviour arrives
  with specs. `test/Fixtures.hs` has small hand-built worlds, items, monsters
  and JSON values to build on — prefer extending it to hand-rolling a world
  inside a spec.
* Test names read as sentences: `it "counts a locked door as passable, since
  a key opens it"`.

## Adding to the game

### A new dungeon floor, monster or item

Edit `world.json`; see the schema reference in `README.md`. Then:

1. `cabal test` — the validator will tell you about unreachable rooms,
   misaligned stairs, doors with no findable key, and trigger actions that
   reach for something that is not there.
2. Check that autoplay still gets out alive, and that it is still made to
   suffer on the way. If the run finishes at full health the floor is too
   soft; if the bot dies it is probably too hard, since a person can retreat
   and the bot cannot.
3. Delete your `save.json` — editing `world.json` invalidates existing saves.

Coordinates are `[x, y]`: column then row, with `[0, 0]` at the top left.

### A new Special item effect

`Special` items say what they do with `itemEffect`, so most new abilities are
JSON plus one function. Touch, in order: the `ItemEffect` constructor and its
JSON instances in `src/Game/Types.hs` (and `spentOnUse` alongside), the string
that parses to it in `src/Game/State.hs`, what it does in `useSpecial` in
`src/Game/Logic.hs`, the table in `README.md`, and a spec.

### A new trigger type or action

`TriggerCondition` and `Action` in `src/Game/Types.hs`; the parser in
`src/Game/State.hs` (`transformJSONTrigger` and the action parser); the
interpreter — `evalTriggerCondition` or `executeAction` — in
`src/Game/Logic.hs`.

**Then add its validator**, in `src/Game/State.hs`. Ask what the new action
does when its target does not exist. If the answer is "nothing, quietly", the
validator has to catch it: that is exactly the bug class the checks exist for.
Finish with the table in `README.md` and a spec for each rule.

## Commits and changelog

Commit subjects are imperative and describe the intent, not the files:

```
Have monsters find their way round walls
Check that a level can be played, not just parsed
Roll damage instead of calculating it
```

Bodies are prose paragraphs explaining why the change was worth making and
what it changes for a player or a dungeon author. Add an entry under
`## Unreleased` in `CHANGELOG.md` for anything either of them would notice;
purely internal refactors do not need one.

## License

By contributing you agree that your work is licensed under the MIT license,
the same as the rest of the project.
