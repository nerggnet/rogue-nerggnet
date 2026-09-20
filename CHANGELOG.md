# Changelog

All notable changes to this project are documented in this file.

## Unreleased

### Added

- A real `README.md`, including a reference for the `world.json` schema
  (tile characters, item categories, trigger types and trigger actions).
- A `spec` test-suite covering grid utilities, visibility and line of
  sight, world construction, trigger serialization, turn logic, combat,
  inventory handling and the save/load round-trip.

### Changed

- The game code now lives in a `library` stanza; the executable is a thin
  wrapper around it. This is what lets the test-suite import the modules.
- Triggers are now plain data. A trigger's firing condition is a
  `TriggerCondition` value interpreted by `evalTriggerCondition`, instead of
  a `GameState -> Bool` function. Because a function cannot be serialized,
  saving used to render each trigger as an English sentence and parse that
  sentence back on load; all of that is gone.

  **This changes the save format**, so saves written by earlier versions no
  longer load. `world.json` is unaffected — dungeons do not need editing.

### Fixed

- `validateTriggers` now actually rejects triggers that name an item or NPC
  the level does not define. Its guards previously tested for strings that
  never appeared in a description, so nothing was ever rejected.
- `posAndItems` triggers are no longer lost when a game is loaded. They had
  no case in the trigger serializer and degraded to `"Unknown trigger type"`;
  only an accident in `initGame` kept the first level's win condition working.
- A save file that cannot be read no longer crashes the game on startup;
  `loadSavedGame` reports the problem and the game starts a new dungeon.
- Killing a monster no longer destroys the tile it was standing on. The `†`
  marker is kept in a separate list of corpse positions rather than written
  over the map, so a monster that dies on a staircase or a door no longer
  breaks it. Corpses also survive a save and reload, which they did not
  before. The `Death` tile type is gone.
- The map and the ranged-targeting logic now share one list of visible
  monsters, so the letter shown on the map always selects the monster you
  expect. The map filtered out inactive spawn templates and the targeting
  code did not, which could shift every letter by one. The lookup is also
  bounds-checked, so a monster outside the map no longer crashes the game.
- Combat now resolves its target from the world by position instead of
  matching the passed-in monster by value. A caller holding a copy whose
  health or flags had already changed silently hit nothing, while the player
  still took the counterattack. Ranged attacks are fixed the same way, and
  no longer sweep up any other monster that happens to be at zero health.
- Consumable items can no longer be used forever. `itemUses` is spent through
  a `Maybe`, so an item without one was never removed; `Healing`, `Key` and
  `Range` items must now declare a use count and loading fails with a message
  naming the item if one does not.

### Tooling

- Builds and tests on GHC 9.14.1 as well as 9.12.2; both are in the CI
  matrix and in `tested-with`. No source change was needed.

- A GitHub Actions workflow builds and tests on Linux and macOS with
  `-Werror`, checks the package metadata, and runs hlint over the sources.
- Dependency version bounds, a `maintainer`, a `homepage` and a
  `source-repository` stanza. `cabal check` is now clean; it previously
  reported that Hackage would reject the package.
- `.gitignore` is a Haskell one. It was the Visual Studio template with a
  few Haskell lines appended, 381 lines for a project with no C# in it.

### Removed

- The `lens` dependency. It was pulled in for six uses of `^. _x` and
  `^. _y`; `updateTile` and `gridLookup` take a `V2 Int` directly now, and
  `goUp`/`goDown` read the tile under the player through the bounds-checked
  `gridLookup` instead of indexing twice with `!!`.
- The `mtl`, `split` and `extra` dependencies, which nothing imported any
  more once the trigger-description parsing was deleted.
- `applyTileOverrides`, which was a duplicate of the copy in `File.MapIO`
  that is actually used, `monsterList`, which had no callers, and
  `restoreWorld`, which could only ever return its argument unchanged
  because the map grid is empty at the point it ran.

### Fixed

- Toggling the legend with `?` or opening command mode with `:` no longer
  spends a turn. Both used to let monsters move and attack, and every
  keystroke typed in command mode advanced the clock that decides when NPCs
  step, so typing `:q` moved the world three times. The clock now ticks in
  `processTurn`, where a turn actually happens.
- Combat no longer writes blank lines to the message log. Messages for
  things that did not happen were added as empty strings, taking up lines
  out of the five the pane shows.
- The player's maximum health is found by XP level number rather than by
  indexing the level table, which silently assumed it was ordered and
  started at one.

### Internal

- `Game.State` gained `currentWorld`, `setCurrentWorld` and `withCurrentWorld`.
  `levels state !! currentLevel state` appeared 25 times and was paired with
  `replaceLevel state (currentLevel state) …` at 17 more; both are gone.
- Every module now imports `Game.Types` unqualified and `File.Types` as `FT`,
  instead of `Game.Logic`, `File.MapIO` and `UI.MainUI` writing `Game.` in
  front of roughly every other identifier. `OverloadedRecordDot` is on, so
  nested reads are `state.player.health` rather than `health (player state)`.
- The attributes `wall`, `floor` and `log` were defined but never drawn
  with; the message-log line counts are named constants rather than two
  magic numbers with a comment giving a third; and `replace` is now called
  `replaceFirst`, which is what it does.
- `processTriggers` applies its bookkeeping to the state the trigger actions
  produced rather than rebasing on the state from before them. Nothing
  currently writes to another level from a trigger, so this changes no
  behaviour, but the old form would have silently discarded it.

### Performance

- Rebuilding the discovered-tiles grid on load puts the coordinates in a
  `Set` instead of scanning a list once per cell. For a level the size of
  the first one, with a mid-game amount explored, that is 6.1 ms down to
  0.1 ms, and it happens for every level on every load.

- Drawing a tile no longer rescans the level. `UI.Draw` builds one `MapView`
  per frame holding the monster, item, NPC and corpse positions as sets and
  the targeting letters as a map, and walks the visibility and discovered
  grids alongside the tiles instead of indexing into them with `!!` for
  every cell. Rendering the first level measured 188 us per frame before and
  22 us after, with identical output.

### Content

- `world.json`: Greater Health Potion, Healing Potion, Silver Key, Scroll of
  Fireball and Scroll of Lightning now declare one use each, and the Elven
  Longbow is a `Weapon` rather than a `Range` item, so it is equipped for its
  +20 attack instead of being fired.

## 1

- Initial version.
