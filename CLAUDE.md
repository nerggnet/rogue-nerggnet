# Working in this repository

A terminal Rogue-like in Haskell (Brick/Vty). The dungeon is data, not code:
maps, monsters, items, NPCs, doors and scripted events all live in
`world.json`. `README.md` documents the game and the full `world.json`
schema — read it before changing content, and update it when the schema
changes.

`CONTRIBUTING.md` covers the workflow in full. This file is the short version
plus the things that are easy to get wrong.

## Commands

```bash
cabal build all                       # library, executable and test-suite
cabal test --test-show-details=direct # 595 examples
cabal run rogue-nerggnet              # play it
hlint src test app                    # must print "No hints"
cabal check                           # package metadata
```

**Everything must run from the repository root.** The game reads `world.json`
and reads/writes `save.json` relative to the working directory, and the specs
load the real `world.json`.

## What has to be true before committing

CI runs on Linux and macOS across GHC 9.12.2 and 9.14.1, so a break shows up
four times. Check all four locally:

1. `cabal build all --ghc-options=-Werror` — CI builds this package with
   `-Werror`, so any warning is a failure. **`touch` the files you changed
   first.** Cabal will happily report success without recompiling anything,
   and a warning that is not re-emitted is a warning you will not see; a
   green run here on a cached build has already let a `-Wx-partial` failure
   reach CI. A build that prints nothing at all has checked nothing.
2. `hlint src test app` prints **No hints**. The hlint job uses
   `fail-on: suggestion`, so even a suggestion breaks the build.
3. `cabal test` — all green. The suite is randomized and uses QuickCheck,
   so a property can pass locally and fail on CI's seed. When you change
   what a total function returns, go looking for the property that says
   what it used to return; do not trust one green run.
4. `cabal check` — clean.

Commit and push finished work without being asked. Commit subjects are
imperative and say what the change is for, not what it touches: *"Have
monsters find their way round walls"*, not *"Update Logic.hs"*. Bodies are
prose paragraphs explaining why. Add a `CHANGELOG.md` entry under
`## Unreleased` for anything a player or a dungeon author would notice.

## Invariants worth protecting

- **Brick and Vty appear only under `src/UI/`.** `Game.*` and `File.*` are
  UI-independent, which is what lets the logic be tested without a terminal.
- **`world.json` is validated at load**, in `Game.State`, and every problem is
  reported together rather than one per run. When you add a field, a trigger
  type or an action, add its validator in the same change. The rule: a content
  mistake that would otherwise fail silently at run time — a misspelled
  monster name that simply never spawns anything — must be caught before the
  game starts.
- **`Game.Autoplay` is the balance oracle.** It plays through the same entry
  points the keyboard drives. `Game.AutoplaySpec` plays the shipped dungeon on
  six seeds and requires it to reach floor 12 and get back out alive, having
  been brought below 60% health on the way. Any change to `world.json` or to
  combat means re-running it; an impossible dungeon and a trivial one both
  fail the build.
- **Coordinates are `[x, y]`** — column then row, `[0, 0]` top left.
- **Editing `world.json` invalidates `save.json`.** Delete the save after
  changing the dungeon; the map grid is re-read from `world.json` on load.

## Gotchas already paid for

- `evaluate` forces only WHNF. To assert that a pure transform throws, force
  the specific field (`evaluate (triggerCondition …)`), not the record.
- A dead monster is removed from the level's `monsters` list, but
  `defeatedMonsters` holds **distinct names**, so it is not a kill count.
- Scratchpad binaries go stale. Rebuild before believing what one tells you.
- `cabal run --project-dir` resets the working directory, so the program picks
  up the real `world.json` rather than a fixture.
