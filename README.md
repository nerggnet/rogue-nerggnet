# rogue-nerggnet

[![CI](https://github.com/nerggnet/rogue-nerggnet/actions/workflows/ci.yml/badge.svg)](https://github.com/nerggnet/rogue-nerggnet/actions/workflows/ci.yml)

A small Rogue-like dungeon crawler for the terminal, written in Haskell with
[Brick](https://hackage.haskell.org/package/brick) and
[Vty](https://hackage.haskell.org/package/vty).

The dungeon is **entirely data-driven**: maps, monsters, items, NPCs, locked
doors and scripted events all live in `world.json`, so you can build your own
dungeon without touching a line of Haskell.

```
                    Rogue nerggnet (press ? for help)

┌───────────────────────────────────────────────────┐   ┌────Stats───────────┐
│##############################################     │   │Level: 2            │
│#S....>#......#.....................#........#     │   │HP: 84              │
│#.N#####......#..####......#####....#.....#..###   │   │Attack: 27 (Base: 7)│
│#..#..##...####..#!.###.####...#....##....#....#   │   │Resistance: 4 ...   │
│#..#...+...#..#+##...#.........#....#.....##...####│   │XP: 145             │
│#..#.M!#.........#...#.........##...#......#......#│   └────────────────────┘
│#..#####.........##+##..........#...########......#│   ┌────Inventory (3/15)┐
│#....@....!.....................##..#.............#│   │a) Dark Sword (W)   │
│...                                                │   │b) Health Potion (3)│
└───────────────────────────────────────────────────┘   │c) Iron Key (2)     │
                                                        └────────────────────┘
You picked up: Health Potion
You attacked Goblin for 27 damage!
You defeated the Goblin and gained 25 XP!

Command:
```

## Requirements

* GHC 9.12 or 9.14 and `cabal` — easiest via [ghcup](https://www.haskell.org/ghcup/)
* A terminal that supports 24-bit colour

## Build and run

```bash
cabal build
cabal run rogue-nerggnet
```

The game reads `world.json` and reads/writes `save.json` **relative to the
current working directory**, so run it from the repository root.

## Tests

```bash
cabal test
```

The suite also has to run from the repository root, because the save/load
specs load the real `world.json`. Use `--test-show-details=direct` to see the
individual examples.

CI builds and tests on Linux and macOS with `-Werror`, and runs
[hlint](https://github.com/ndmitchell/hlint) over `src/`, `test/` and `app/`.

## Saving

* On exit the full game state is written to `save.json`.
* On startup, if `save.json` exists it is loaded and you resume where you left
  off; otherwise a new game is started from `world.json`.
* To start over, either type `:restart` in-game or delete `save.json`.

`save.json` stores only what cannot be recovered from `world.json` (player,
entity state, discovered tiles, tile overrides); the map grid itself is
re-read from `world.json` on load. **Editing `world.json` therefore invalidates
existing saves** — delete `save.json` after changing the dungeon.

If the save file cannot be read at all, the game reports why and starts a new
game rather than failing.

## Controls

| Key | Action |
| --- | --- |
| `w` / `k` | Move up |
| `s` / `j` | Move down |
| `a` / `h` | Move left |
| `d` / `l` | Move right |
| `<` | Ascend stairs (standing on `<`) |
| `>` | Descend stairs (standing on `>`) |
| `g` | Pick up the item you are standing on |
| `u` | Use / equip an item from the inventory |
| `x` | Drop an item from the inventory |
| `?` | Toggle the help popup |
| `:` | Enter command mode |
| `Esc` | Cancel inventory selection, aiming, or command mode |

Moving into a monster attacks it. Moving into an NPC talks to it.

### Commands

| Command | Effect |
| --- | --- |
| `:q` | Quit (and save) |
| `:restart` | Start a new game from `world.json` |
| `:heal` | Cheat: restore full health |
| `:super` | Cheat: 1000 HP, 100 attack, 100 resistance |

### Using items

`u` lists the inventory by letter (equipped items first) and waits for a key:

* **Weapon** / **Armor** — toggles equipping it; its effect value is added to
  your base attack / resistance.
* **Healing** — restores its effect value in HP, capped at the maximum for your
  current XP level.
* **Key** — unlocks an adjacent locked door, if the key's name matches the
  door's `doorKeyName`.
* **Range** — enters aiming mode. Visible monsters are relabelled `a`, `b`,
  `c`… on the map; press a letter to fire, or `Esc` to cancel.

Items with an `itemUses` count are consumed one use at a time and stack in the
inventory when you pick up another of the same kind. Weapons, armor and
special items have no use count and are never consumed.

## Map legend

| Symbol | Meaning |
| --- | --- |
| `@` | You |
| `M` | Monster |
| `N` | NPC |
| `!` | Item on the floor |
| `#` | Wall |
| `.` | Floor |
| `+` | Door (yellow; locked doors block movement until unlocked) |
| `<` / `>` | Stairs up / down |
| `†` | A monster died here (the tile underneath is unchanged) |

Unexplored tiles are blank. Tiles you have seen before but cannot currently see
are drawn dimmed and without their contents.

## Gameplay notes

* **Fog of war** — you see 5 tiles (Manhattan distance) with line of sight;
  walls and locked doors block sight.
* **Monsters** — chase you when within 4 tiles. An adjacent monster attacks
  every other turn. Damage to you is `monster attack − your resistance`.
* **Combat** — attacking a monster also provokes an immediate counterattack.
  Where a monster falls is marked with `†`, which does not disturb the tile
  underneath.
* **XP levels** — defined in `world.json`. Crossing a threshold raises your
  base attack and resistance and restores you to the new maximum health.
* **Inventory** — limited to 15 slots.

## Designing your own dungeon

Everything lives in `world.json`:

```json
{
  "xpLevels": [ ... ],
  "levels":   [ ... ]
}
```

> **Coordinates are `[x, y]`** — i.e. `[column, row]`, with `[0, 0]` at the top
> left of `mapGrid`.

### `xpLevels`

One entry per player level, in ascending order:

```json
{ "xpLevel": 2, "xpThreshold": 100, "xpHealth": 100, "xpAttack": 7, "xpResistance": 4 }
```

### `levels`

Each dungeon floor, from top to bottom. `<` on one floor takes you to the floor
before it in the list, `>` to the next one.

```json
{
  "levelNumber": 1,
  "mapGrid": [ "###...", "#S...." ],
  "monsters": [],
  "doors":    [],
  "items":    [],
  "npcs":     [],
  "triggers": []
}
```

#### `mapGrid`

A list of equal-length strings, one per row:

| Char | Tile |
| --- | --- |
| `#` | Wall |
| `.` | Floor |
| `+` | Door |
| `<` | Up stairs |
| `>` | Down stairs |
| `S` | Player start (first floor only) |

Any other character is treated as floor (a space makes a handy "outside the
dungeon" filler, since walls surround the playable area anyway).

#### `monsters`

```json
{ "name": "Goblin", "position": [5, 5], "health": 10, "attack": 3, "xp": 25, "inactive": false }
```

A monster with `"inactive": true` is not placed in the world; it acts as a
**template** that a `spawnMonster` trigger action can bring to life.

#### `doors`

```json
{ "doorPosition": [7, 4], "doorLocked": true, "doorKeyName": "Iron Key" }
```

A door entity should sit on a `+` tile. Locked doors block both movement and
line of sight until opened with the matching key or by an `unlockDoor` action.

#### `items`

```json
{ "itemName": "Iron Key", "itemPosition": [35, 5], "itemDescription": "Sturdy key",
  "itemCategory": "Key", "itemEffectValue": 0, "itemHidden": false,
  "itemInactive": false, "itemUses": 1 }
```

| Field | Meaning |
| --- | --- |
| `itemCategory` | One of `Armor`, `Weapon`, `Range`, `Healing`, `Special`, `Key` |
| `itemEffectValue` | Attack/resistance bonus, healing amount, or ranged damage bonus |
| `itemHidden` | Not drawn on the map, but still pickable with `g` |
| `itemInactive` | Not in the world yet — revealed by a `spawnItem` or `addToInventory` action |
| `itemUses` | Charges, spent one at a time. Required for `Healing`, `Key` and `Range`; `null` (never consumed) is only for equipment |

#### `npcs`

```json
{ "npcName": "Friendly NPC", "npcPosition": [2, 2], "npcMessage": "Welcome to the dungeon!" }
```

NPCs wander the level and repeat their message when you walk into them.

#### `triggers`

A trigger fires when its condition holds at the end of a turn and then runs its
actions in order. `"recurring": false` means it fires only once.

```json
{
  "triggerType": "position",
  "target": [5, 9],
  "requiredItems": null,
  "triggerItemName": null,
  "triggerNpcName": null,
  "message": "You stepped on a special tile!",
  "recurring": false,
  "actions": [ ... ]
}
```

| `triggerType` | Fires when | Uses |
| --- | --- | --- |
| `position` | The player stands on `target` | `target` |
| `posAndItems` | The player stands on `target` **and** carries every item in `requiredItems` | `target`, `requiredItems` |
| `itemPickup` | `triggerItemName` is in the player's inventory | `triggerItemName` |
| `npcTalked` | The player last talked to `triggerNpcName` | `triggerNpcName` |
| `allMonstersDefeated` | No active monsters remain on the level | — |

All fields must be present; set the unused ones to `null`.

#### Trigger actions

```json
{ "actionType": "spawnItem", "actionPosition": [49, 25], "actionItemName": "Dark Sword",
  "actionMonsterName": null, "actionTileType": null, "actionMessage": null }
```

| `actionType` | Effect | Uses |
| --- | --- | --- |
| `spawnItem` | Makes a pre-placed `itemInactive` item at that position real | `actionItemName`, `actionPosition` |
| `spawnMonster` | Places an `inactive` monster template at that position | `actionMonsterName`, `actionPosition` |
| `unlockDoor` | Unlocks the door at that position | `actionPosition` |
| `shiftTile` | Replaces the map tile (using a `mapGrid` character) | `actionPosition`, `actionTileType` |
| `transportPlayer` | Teleports the player | `actionPosition` |
| `consumeItem` | Removes a named item from the inventory | `actionItemName` |
| `addToInventory` | Moves a named `itemInactive` item into the inventory | `actionItemName` |
| `displayMessage` | Writes a line to the message log | `actionMessage` |
| `setGameWon` | Wins the game | — |

Like triggers, every field must be present; unused ones are `null`.

## Project layout

```
app/Main.hs           Entry point; a thin wrapper around the library
src/Game/Types.hs     Core domain types and their JSON instances
src/Game/State.hs     World construction from config, visibility, trigger wiring
src/Game/Logic.hs     Turn processing: movement, combat, items, monster/NPC AI
src/Game/GridUtils.hs Grid and inventory helpers
src/File/Types.hs     The on-disk `world.json` schema
src/File/MapIO.hs     Loading and saving
src/UI/MainUI.hs      Brick application, event handling, colours
src/UI/Draw.hs        Rendering
test/Spec.hs          Test-suite entry point
test/Fixtures.hs      Small hand-built worlds and entities for the specs
test/*/…Spec.hs        One spec module per source module
```

All the game code lives in a library stanza so that both the executable and
the test-suite can import it.

Brick and Vty are confined to the `UI` modules; `Game` and `File` are
UI-independent.

## License

MIT — see [LICENSE](LICENSE).
