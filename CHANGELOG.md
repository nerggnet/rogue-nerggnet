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
- Consumable items can no longer be used forever. `itemUses` is spent through
  a `Maybe`, so an item without one was never removed; `Healing`, `Key` and
  `Range` items must now declare a use count and loading fails with a message
  naming the item if one does not.

### Content

- `world.json`: Greater Health Potion, Healing Potion, Silver Key, Scroll of
  Fireball and Scroll of Lightning now declare one use each, and the Elven
  Longbow is a `Weapon` rather than a `Range` item, so it is equipped for its
  +20 attack instead of being fired.

## 1

- Initial version.
