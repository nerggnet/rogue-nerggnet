# rogue-nerggnet

[![CI](https://github.com/nerggnet/rogue-nerggnet/actions/workflows/ci.yml/badge.svg)](https://github.com/nerggnet/rogue-nerggnet/actions/workflows/ci.yml)

A small Rogue-like dungeon crawler for the terminal, written in Haskell with
[Brick](https://hackage.haskell.org/package/brick) and
[Vty](https://hackage.haskell.org/package/vty).

The dungeon is **entirely data-driven**: maps, monsters, items, NPCs, locked
doors and scripted events all live in `world.json`, so you can build your own
dungeon without touching a line of Haskell.

Twelve floors deep. The way out is at the bottom, past the Dungeon Lord, and
a run is judged on how far down it got and what it carried back up.

```
              Rogue nerggnet - Floor 2 of 12 (press ? for help)

┌───────────────────────────────────────────────────┐   ┌────Stats───────────┐
│##############################################     │   │XP level: 2         │
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

:restart
```

## Dungeons

`world.json` is the course this game ships with, and the one scores are
compared on. It is not the only one the engine can play:

```bash
cabal run rogue-nerggnet -- --world example.json
```

`example.json` is a single floor with a locked door, an archer and a way
out — small enough to read in one sitting, and a starting point for writing
your own. The schema is documented below, and the validator is what makes
hand-authoring safe: it will not start a dungeon with a room nobody can
reach, stairs that do not line up, a locked door whose key is nowhere, or a
trigger that reaches for something that is not there.

Everything a run leaves behind belongs to the dungeon it was played in.
`world.json` keeps `save.json`, `scores.json`, `graves.json` and `replays/`
where they have always been; any other dungeon gets a corner of its own:

```
packs/example/save.json
packs/example/scores.json
packs/example/graves.json
packs/example/replays/
```

Scores from two different dungeons are not comparable, so they are not kept
together, and a body from one has no business in the other.

## Requirements

* GHC 9.12 or 9.14 and `cabal` — easiest via [ghcup](https://www.haskell.org/ghcup/)
* A terminal with a terminfo entry and cursor addressing. 256 colours gets
  you everything the game draws; it runs on an 8-colour or monochrome
  terminal too, and Vty quietly drops what the terminal cannot render.
  Every entity has a glyph of its own (`@ M A N ! + < > ^ †`), so nothing
  is told by colour alone

The map scrolls to follow the player, so the window does not need to be tall
enough to show a whole dungeon level at once. It takes 37 rows to see a whole
floor of the shipped dungeon without scrolling; below that the map scrolls
under the player and everything else stays where it is.

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

## Scores

Every finished run is written to `scores.json` in the working directory, and
the end-of-run screen shows where it placed:

```
    when              how     depth    treasure      xp   turns    score
 1. 2026-09-21 15:09  out     F12         13100   69207    3166    14300
 2. 2026-09-21 15:09  out     F12         13100   69207    3169    14300
>3. 2026-09-21 15:14  died    F8              0   12480    1902      800
```

The score is the treasure you carried out plus 100 for each floor reached.
Dying forfeits the treasure but keeps the depth, so getting to floor 11 and
dying is not the same as drowning on floor 2 -- and getting out is always
worth more than the same haul lost. Ties go to the shorter run.

This is what the fixed dungeon is for: everyone plays the same twelve floors,
so two scoreboards can be set side by side. `:scores` shows the table at any
time, and the last hundred runs are kept.

A missing `scores.json` is simply an empty board. One that cannot be read is
reported and then ignored, because losing the history is not a reason to
refuse to play.

## The dead

A run that ends badly leaves a body where it fell, and the next run through
the same dungeon will find it -- with everything it was carrying still on
it. Standing on one says whose it was:

```
Here you died, 2026-09-22 14:00, carrying 4290 in treasure.
```

Getting your own kit back is the point of it: the blade, the armour and the
treasure that was nearly worth something are all still there, on the floor
you did not get off. The last eight deaths are kept, in `graves.json`.

A body belongs to the dungeon it died in -- a grave carries the same
fingerprint of `world.json` that a replay does, and one from a different
dungeon is left out, since the same coordinates elsewhere are a different
place.

## Replays

Every finished run is written to `replays/`, as the seed it started from and
the keys that were pressed. That is the whole run: the game is deterministic,
so the same seed and the same keys against the same dungeon give the same
result every time.

```bash
cabal run rogue-nerggnet -- --replay replays/2026-09-22-12-00.json
```
```
Verified. GotOut on floor 12 with 9340 in treasure, 3200 turns,
scoring 10540 (seed 4242, 3259 keys).
```

You can also watch one play itself:

```bash
cabal run rogue-nerggnet -- --watch replays/2026-09-22-12-00.json
```
```
[replay] 57/3259  playing  speed 5   space hold   + - speed   . step   q stop
```

Space holds it, `+` and `-` change the pace, `.` steps one key at a time
while it is held, and `q` gives up on it. The keys go through the same
handler the keyboard does, so what you are watching is the run and not a
reconstruction of it.

This is what makes a score worth comparing. The scoreboard says somebody
carried 9,340 out of the bottom; the replay is the proof, and anybody with
the same dungeon can check it rather than take their word.

A replay records a fingerprint of `world.json` and is refused if the dungeon
has changed since — the keys would still be pressed, but they would be
pressed at different things:

```
This run was played on a different world.json
(it wants 93f77698377d6e4f, this one is 8e81e02893003a58).
```

A game resumed from a save is not recorded: the keys that got it there are
gone, so there is no run to write down. Playing straight through from a new
dungeon is.

## Saving

* On exit the full game state is written to `save.json`.
* On startup, if `save.json` exists it is loaded and you resume where you left
  off; otherwise a new game is started from `world.json`.
* **Death is permanent.** Dying or winning clears `save.json`, so the next
  launch begins a new dungeon. You cannot undo a death by quitting. Both ends
  of a run show a screen saying so.
* To start over at any other time, type `:restart` in-game or delete
  `save.json`.

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
| `c` | Pull shut a door you are standing next to |
| `g` | Pick up the item you are standing on |
| `u` | Use / equip an item from the inventory |
| `x` | Drop an item from the inventory |
| `?` | Open the help, then step through its pages and close it |
| `:` | Enter command mode |
| `Esc` | Cancel inventory selection, aiming, or command mode |

Moving into a monster attacks it. Moving into an NPC talks to it and swaps
places with it.

### Commands

| Command | Effect |
| --- | --- |
| `:q` | Quit (and save) |
| `:restart` | Start a new game from `world.json` |
| `:scores` | Show the runs finished so far (any key closes it) |
| `:log` | Look back through the messages (`j`/`k` scrolls, any other key closes it) |

The message pane shows the last five lines. `:log` shows the history behind
them -- two hundred lines are kept -- which is where a trigger's message or
an NPC's answer has gone by the time you wonder about it.

The popup holds sixteen lines at a time and says which of them it is showing,
so `Messages (23-38 of 38)` means the newest sixteen of thirty-eight. `k`
scrolls back a line, `j` forward again, `g` jumps to the oldest message kept
and `G` back to the newest; any other key closes it. None of it costs a turn,
and it always opens on the newest line.

`:` opens the command line at the bottom of the screen, which is blank until
then. What you type appears there as you type it, `Enter` runs it, `Backspace`
rubs out a character and `Esc` abandons it. All of this is in the in-game help
under `?`.

### Using items

`u` opens a chooser listing the inventory by letter (equipped items first),
with what each one does beside it, and waits for a key:

```
┌────────────────Use which item?────────────────┐
│ a) Voidsteel Edge (W)      +38 attack while wielded    │
│ c) Ashen Scroll            60 damage to all in sight (once) │
│ e) Elixir of the Deep (3)  heals 200                   │
│ j) Whetstone of the Forge  +6 attack, for good (once)  │
└───────────────────────────────────────────────┘
```

An item's `itemDescription` is shown when you pick it up, and standing on one
says what it is without picking it up at all. `Esc` closes it without using anything. The same list is
always in the sidebar, but the chooser is shown over the map so the keys are
readable even on a small terminal.

* **Weapon** / **Armor** — toggles equipping it; its effect value is added to
  your base attack / resistance.
* **Healing** — restores its effect value in HP, capped at the maximum for your
  current XP level.
* **Key** — unlocks an adjacent locked door, if the key's name matches the
  door's `doorKeyName`.
* **Special** — does whatever its `itemEffect` says. Some fire once and are
  used up; others work quietly while you carry them.
* **Range** — enters aiming mode. Visible monsters are relabelled `a`, `b`,
  `c`… on the map; press a letter to fire, or `Esc` to cancel.

Items with an `itemUses` count are consumed one use at a time and stack in the
inventory when you pick up another of the same kind -- same name, category and
effect value. A stack adds up both the uses and what it is worth carried out.
Two items sharing a name but differing in anything else would sit in separate
rows, so the world file is checked for that. Weapons, armor and
special items have no use count and are never consumed.

## Map legend

| Symbol | Meaning |
| --- | --- |
| `@` | You |
| `M` | Monster |
| `A` | Monster that strikes from a distance |
| `N` | NPC |
| `!` | Item on the floor |
| `#` | Wall |
| `.` | Floor |
| `+` | A door that is shut, or locked |
| `'` | A doorway standing open |
| `<` / `>` | Stairs up / down |
| `^` | A shaft, with daylight behind it |
| `†` | A monster died here (the tile underneath is unchanged) |
| `‡` | An earlier run of yours ended here |
| `*` | Something in the floor went off here |

Unexplored tiles are blank. Tiles you have seen before but cannot currently see
are drawn dimmed and without their contents -- but stairs, doors and shafts
stay drawn as themselves, since they are what you would remember about a room.
A corpse, and the mark a sprung trap leaves, are only ever drawn on plain
floor, so neither can hide a way out. While you are standing in a trap that
has just gone off, your own `@` is drawn in its colours, since you are
covering the mark.

## Gameplay notes

* **Fog of war** — you see 5 tiles (Manhattan distance) with line of sight;
  walls and locked doors block sight.
* **Doors** — `c` pulls a door shut if you are standing beside one and the
  doorway is clear; you cannot shut a door through a monster. Walking into a
  shut door pushes it open and costs the turn. Monsters cannot open doors at
  all, so a door pulled shut behind you is a wall to them.
* **Monsters** — chase you when they are within 4 steps of you, counted
  along the way they would have to walk, so one behind a wall or a locked
  door stays put. One with a `range` (drawn `A`) does not chase at all while
  it has a clear line to you: it stands where it is and shoots, and closing
  that distance is your problem. They follow the shortest route and go round corners. An
  adjacent monster attacks every other turn. Damage to you is based on `monster attack − your
  resistance`.
* **Damage is rolled**, landing within a quarter either side of the
  attacker's strength, so the same fight does not always go the same way. An
  attack that cannot beat the defender's resistance still does nothing. The
  average is the attacker's strength, so the numbers in `world.json` mean
  what they always did.
* **Combat** — attacking a monster also provokes an immediate counterattack.
  Where a monster falls is marked with `†`, which does not disturb the tile
  underneath.
* **XP levels** — defined in `world.json`. Crossing a threshold raises your
  base attack and resistance and restores you to the new maximum health. The
  stats box says how much more experience the next one wants.
* **Inventory** — limited to 15 slots.
* **Getting out** — `^` is a shaft with daylight behind it. Standing on one
  costs nothing and says so; using an item with the `Escape` effect there
  throws a rope and climbs you to the floor above, landing beside the stairs
  you came down. Shafts sit a long way from the stairs, so the rope buys you
  the walk back. From the first floor there is no floor above, and climbing
  is leaving: the run ends and is scored. The other ending is at the bottom,
  past the Dungeon Lord and the door his sigil opens.
* **Score** — a run is measured by how deep you got and what you carried out.
  Every item has an `itemValue`, and the sidebar shows the running total, so
  the decision to press on or turn back is made with the numbers in view.
  Getting out is what turns treasure carried into treasure kept: dying loses
  the lot, and both endings show the same summary so two runs can be set
  against each other.

## Playing it through by machine

`Game.Autoplay` plays the dungeon with the same functions the keyboard
drives. It heals when hurt, banks a permanent gain the moment it finds one,
burns a scroll on a crowd, wears the best thing it is carrying, fights what
is in the way, picks up what it passes, keeps what it knows how to use,
unlocks what it can, walks down, and takes a way out once there is nowhere
deeper to go. When it is nearly dead with nothing left to drink it stops
fighting: it vanishes, or blinks, or walks away from whatever can reach it.

It is still deliberately unclever -- it does not lure, does not fight in a
doorway, and does not break an archer's line on purpose -- so a dungeon it
beats is beatable.

The test-suite plays the shipped dungeon through on several seeds and
checks that it gets to the bottom and back out alive, with the treasure,
having been brought below 60% health somewhere on the way. That is what
keeps a hand-drawn dungeon honest: an impossible level, or a trivial one,
fails the build.

## When the dungeon file is wrong

`world.json` is checked when the game starts, and every problem found is
reported together rather than one per run:

```
Could not start a game from world.json:
  - level 0: item "Health Potion": unknown "itemCategory" "Sandwich"; expected one of "Armor", ...
  - level 0: trigger 1: action 0: a "transportPlayer" action needs "actionPosition"
  - level 1: the "mapGrid" is ragged: row 0 is 51 characters wide, but row(s) 3 are not
  - level 4: trigger 1: needs item "Nonexistent Relic", which this level does not define
```

The checks cover unknown categories, trigger types and action types, missing
required fields, consumables with no use count, maps whose rows are not all
the same length, a first level with no `S` tile, and triggers naming items or
NPCs the level does not define.

They also check that a level can actually be played:

* every monster, item and NPC can be walked to from where the player arrives,
  and so can the stairs down. Locked doors count as open, since a key opens
  them; only walls strand things. Inactive items are left out, because a
  trigger may hand one straight to the player wherever it sits;
* no door is drawn inside a wall;
* the stairs line up. Going up or down leaves you where you are and only
  changes which level that is, so level 2's `<` has to be exactly where
  level 1's `>` was;
* every locked door has a key that can be found on that level or an earlier
  one, or a trigger that opens it;
* a dungeon with a `^` shaft in it has something with the `Escape` effect to
  climb it with. The check is of the dungeon and not of each floor, because
  a shaft is climbed on the way back as readily as on the way down;
* no name is used for two different items. Copies of a name must agree on
  category, effect value, effect and value, since the inventory stacks on
  those; only `itemUses` may differ, being what a stack adds up;
* every trigger action reaches for something that is really there: a
  `spawnMonster` names an inactive monster of that level and aims at floor,
  a `spawnItem` names an item the level places at exactly that spot, an
  `addToInventory` or `consumeItem` names an item that exists, an
  `unlockDoor` points at a door, a `transportPlayer` lands the player on
  floor, and a `shiftTile` stays on the map. These are silent failures at
  run time — a misspelled monster name simply never spawns anything — so
  they are worth catching before the game starts.

This is what makes drawing a map by hand safe: one wall in the wrong place
otherwise seals off a room and nobody notices.

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

One entry per XP level, in ascending order:

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
| `^` | Shaft (a rope climbs it to the floor above) |

Any other character is treated as floor (a space makes a handy "outside the
dungeon" filler, since walls surround the playable area anyway).

#### `monsters`

```json
{ "name": "Goblin", "position": [5, 5], "health": 10, "attack": 3, "xp": 25, "inactive": false, "range": null }
```

A monster with `"inactive": true` is not placed in the world; it acts as a
**template** that a `spawnMonster` trigger action can bring to life.

`range` is how far it can strike, in steps, counted the way sight is. A
monster with a range holds its ground while it has a clear line to you and
shoots instead of closing, and is drawn `A` rather than `M`. Omit it, or set
it to `null`, for something that fights at arm's length.

A shot costs the shooter nothing — you cannot swing back at what you are not
standing beside — so a monster that gains a range and keeps its attack is
simply a better monster. Take a few points off its `attack` to pay for the
reach, and bear in mind that damage is attack minus your resistance: a third
off the attack of something that hits for 44 against a player resisting 24
takes three quarters off its damage, not a third.

The range may not exceed the player's sight (5), because being shot by
something you cannot see, cannot find and cannot reach is not a difficulty
setting. The world file is checked for it.

#### `doors`

```json
{ "doorPosition": [7, 4], "doorLocked": true, "doorKeyName": "Iron Key" }
```

A door entity should sit on a `+` tile. `doorLocked` starts it locked, which
also starts it shut.

Shut and locked are different things. A locked door wants its key; a shut one
only wants pushing, and pushing it is your turn. Either one stops movement
and line of sight, which is what makes `c` worth having: pull a door shut
behind you and the archer on the other side loses its shot, and whatever was
chasing you has to come the long way -- monsters do not open doors.

#### `items`

```json
{ "itemName": "Iron Key", "itemPosition": [35, 5], "itemDescription": "Sturdy key",
  "itemCategory": "Key", "itemEffectValue": 0, "itemHidden": false,
  "itemInactive": false, "itemUses": 1 }
```

| Field | Meaning |
| --- | --- |
| `itemCategory` | One of `Armor`, `Weapon`, `Range`, `Healing`, `Special`, `Key` |
| `itemEffect` | Required for `Special`, and only for `Special`. See the table below |
| `itemEffectValue` | Attack/resistance bonus, healing amount, or ranged damage bonus |
| `itemHidden` | Not drawn on the map, but still pickable with `g` |
| `itemInactive` | Not in the world yet — revealed by a `spawnItem` or `addToInventory` action |
| `itemUses` | Charges, spent one at a time. Required for `Healing`, `Key` and `Range`; `null` (never consumed) is only for equipment |
| `itemValue` | What it scores if carried out of the dungeon. Omit it for something worthless |

Special items say what they do with `itemEffect`, so a new one can be written
in `world.json` rather than in Haskell. `itemEffectValue` is its strength.

| `itemEffect` | What it does | Spent on use? |
| --- | --- | --- |
| `Keepsake` | Nothing. For quest items a trigger asks for | no |
| `Empower` | Raises attack permanently by the effect value | yes |
| `Fortify` | Raises resistance permanently by the effect value | yes |
| `Reveal` | Maps the whole floor | yes |
| `Blink` | Moves you to a random floor tile on the level | yes |
| `Firestorm` | Hurts every monster you can see, by the effect value | yes |
| `Vanish` | Monsters cannot find you for `itemEffectValue` turns | yes |
| `Escape` | On a `^` shaft, climbs to the floor above; from the first floor, out of the dungeon | when it works |
| `Regenerate` | Heals the effect value each turn while carried | no |
| `Lifesteal` | Returns `itemEffectValue`% of the damage you deal, while carried | no |
| `Revive` | Saves you from one death, then burns up | when it saves you |

An item a trigger needs should be a `Keepsake`, so that using it cannot
destroy it before the trigger fires.

#### `npcs`

```json
{ "npcName": "Friendly NPC", "npcPosition": [2, 2], "npcMessage": "Welcome to the dungeon!" }
```

NPCs wander the level. Walking into one talks to them and changes places
with them, so an NPC can never block a corridor.

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
| `monsterDefeated` | A monster called `triggerMonsterName` has been beaten | `triggerMonsterName` |
| `allMonstersDefeated` | No active monsters remain on the level | — |

All fields must be present; set the unused ones to `null`.

`monsterDefeated` remembers an actual defeat rather than asking whether any
monster of that name is currently alive, so it works for a boss that starts
as an `inactive` template and is called up later by a `spawnMonster` action.
It may name such a template. Between them, `position` to raise the boss and
`monsterDefeated` to reward beating it are enough to script an end fight:

```json
{ "triggerType": "monsterDefeated", "triggerMonsterName": "Dungeon Lord",
  "target": null, "requiredItems": null, "triggerItemName": null,
  "triggerNpcName": null, "message": "", "recurring": false,
  "actions": [
    { "actionType": "addToInventory", "actionItemName": "Vault Sigil", "actionPosition": null,
      "actionMonsterName": null, "actionTileType": null, "actionMessage": null },
    { "actionType": "unlockDoor", "actionPosition": [5, 2], "actionItemName": null,
      "actionMonsterName": null, "actionTileType": null, "actionMessage": null }
  ]
}
```

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
| `harmPlayer` | Springs a trap for `actionAmount` damage, which can kill | `actionAmount` |
| `healPlayer` | A fountain or shrine, mending `actionAmount` | `actionAmount` |
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

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for how to build, what CI enforces,
and where to add a new floor, item effect, trigger type or action.

## License

MIT — see [LICENSE](LICENSE).
