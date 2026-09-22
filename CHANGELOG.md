# Changelog

All notable changes to this project are documented in this file.

## Unreleased

### Added

- The game will play a dungeon other than its own. `--world <file>` picks
  one, and everything that run leaves behind goes with it: `world.json`
  keeps the file names it has always had, and any other dungeon gets a
  corner of its own under `packs/`. Scores from two dungeons are not
  comparable, so they are not kept together, and a body from one has no
  business in the other.

  `example.json` ships as a starting point -- one floor, a locked door, an
  archer and a way out -- and the test-suite plays it through, so an example
  that has rotted cannot ship.
- A run that ends badly leaves a body, and the next run finds it. The grave
  is marked `‡`, distinct from the `†` a kill leaves, and everything the
  dead run was carrying is on the floor with it -- the blade, the armour and
  the treasure that was nearly worth something. Standing on it says when it
  happened and what it was worth. The last eight are kept in `graves.json`,
  and a body from a different `world.json` is left out, since the same
  coordinates in another dungeon are a different place.
- Doors can be pulled shut, with `c`. Shut and locked have come apart: a
  locked door wants its key, a shut one only wants pushing, and pushing it
  costs the turn. Either stops movement and line of sight, and monsters
  cannot open doors -- so shutting one behind you takes an archer's shot
  away and makes whatever is chasing you come the long way. A doorway
  standing open is drawn `'` and one that is shut `+`, because which it is
  decides whether a corridor is a way out or a wall.
- Recorded runs. The game is deterministic, so a run is its seed and its
  keystrokes and nothing else; both are written to `replays/` when a run
  ends, and `--replay <file>` plays them back against the dungeon and says
  whether it comes out as recorded. A replay carries a fingerprint of
  `world.json` and is refused if the dungeon has changed since.

  This is what the fixed dungeon was for. A score was previously a number
  somebody typed at you; it is now something anybody holding the same twelve
  floors can check.

  It also buys a kind of test that could not be written before: play a
  recorded winning run against the dungeon as it stands and see whether it
  still wins.
- `--watch <file>` plays a recorded run back on screen, on a clock rather
  than a keyboard. Space holds it, `+` and `-` change the pace, `.` steps a
  key at a time while it is held, `q` gives up. The keys go through the same
  handler the keyboard uses, so it is the run and not a reconstruction.
- `:log` looks back through the messages. The pane shows five lines and only
  ten were kept, so a trigger's message, an NPC's answer or what a fight
  cost had scrolled away by the time anyone wondered about it. Two hundred
  lines are kept now and the last sixteen are shown, oldest at the top the
  way the pane reads.
- A trap that goes off leaves a mark. `harmPlayer` only wrote a line to the
  log, which scrolls away, so the map said nothing about what had just taken
  a tenth of the player's health. The tile is marked `*` from then on, and
  while the player is standing on it -- covering it -- their own `@` is
  drawn in its colours instead. Like a corpse, the mark is only drawn on
  plain floor, so it cannot hide a way out.
- Monsters that strike from a distance. Every monster fought the same way --
  walk up, swing -- so the Bone Archer walked up and punched you. A monster
  may now have a `range`: it holds its ground while it has a clear line to
  the player, shoots instead of closing, and is drawn `A` rather than `M`.
  The line is the same one the player sees by, so nothing shoots through a
  wall, and a range wider than the player's sight is refused by the
  validator -- being shot by something you cannot see or reach is not a
  difficulty setting.

  Bone Archers strike from 4 and Ashen Wyrms from 3, at some cost to their
  attack, since a shot costs the shooter nothing.
- The Waystone, the Widow's Knot and the Thirsting Band, on floors 4, 6 and
  7. `Blink`, `Lifesteal` and `Revive` were implemented, specced and written
  up in the README, and no item in the dungeon had any of them: three
  abilities the game had and nobody could reach. A spec now insists every
  effect the game implements is on some item.

  They are found in the first half on purpose. By floor 8 the pack is full
  of things worth more than they are, so the decision worth having is not
  whether to pick one up but whether to keep carrying it once vault treasure
  starts competing for the slot.
- The game says what an item does. Every item has carried a description
  since the beginning and the game showed it nowhere, so an inventory was a
  list of names: nothing said what the Whetstone of the Forge was for, and a
  Special could only be found out by using it, which for half of them spends
  it. The item chooser now has a second column saying what each one does in
  a few words, picking something up prints that plus its description, and
  standing on an item says what it is without picking it up.
- A scoreboard. Every finished run is written to `scores.json` -- when it
  ended, how it ended, how deep it got, what it carried out, experience and
  turns -- and the end-of-run screen shows where it placed among the rest.
  `:scores` shows the table at any time; the last hundred runs are kept.

  The score is the treasure carried out plus 100 a floor. Dying forfeits the
  treasure and keeps the depth, so the decision the dungeon is built around
  -- press on or turn back -- is the one the score rewards. Ties go to the
  shorter run.

  This is what a fixed, hand-drawn dungeon was for: everyone plays the same
  twelve floors, so two scoreboards can be compared. Until now a run's score
  vanished when the game exited.
- `GameState` counts the turns a run has lasted. `keyPressCount` was modulo
  the NPC interval and never a total.
- A real `README.md`, including a reference for the `world.json` schema
  (tile characters, item categories, trigger types and trigger actions).
- A `spec` test-suite covering grid utilities, visibility and line of
  sight, world construction, trigger serialization, turn logic, combat,
  inventory handling and the save/load round-trip.
- Level 2 is a hand-cut maze, drawn on paper rather than generated: dead
  ends, an irregular outline, eight doors and seventeen monsters spread
  across it. It replaces the rooms-and-corridors floor that was there, and
  is a deliberate change of character so early that the dungeon does not
  read as one machine's work all the way down. Level 3's up-stair moves to
  meet its exit, on two tiles of new corridor.
- The world validator now checks what each trigger action reaches for:
  `spawnMonster` must name an inactive monster of that level and aim at a
  tile that is not a wall, `spawnItem` must name an item the level places at
  exactly that position, `addToInventory` and `consumeItem` must name items
  that exist, `unlockDoor` must point at a door, `transportPlayer` must land
  the player on floor, and `shiftTile` must stay on the map. Every one of
  these fails silently at run time — a misspelled monster name simply never
  spawns anything — so the mistake used to survive all the way into play.

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

### Changed

- The armour progression is redrawn, and the first half of the dungeon with
  it. Damage is attack minus resistance, so what a fight costs turns on the
  gap between them; Scale Mail was worth +8 against a base resistance of 4,
  so one item nearly tripled what the player resisted and the floors under
  it had nothing left to say. Scale Mail is worth 3 now and Chain Hauberk 8.
  The deep pieces are untouched; they were never the problem.

  Every floor's monster attack is then derived from what a hit ought to cost
  at that depth rather than set by hand, and blade traps are scaled to the
  player's health the same way. Floors 3 and 4 carry six and seven monsters
  rather than eight and nine, each worth the experience of the ones removed:
  a floor crowded with things that cannot hurt you is not a floor.

      a trap used to be worth   20, 24, 14, 10 monster hits on floors 3-6
      it is now worth           4, 3, 2, 3 -- and 1 to 2 below that

      before   90 72 70 58 53 50 52 39 46 37 37 37
      after    90 81 70 57 56 55 57 40 46 36 37 37
- Blade traps on floors 3 to 6 are cut by about two fifths, and those floors'
  monsters hit a little harder. A trap on floor 3 was taking a seventh of the
  player's health while a monster took a fiftieth, so the only real danger in
  the first half of the dungeon was the floor. The curve below floor 2 now
  descends where it used to sag:

      before   90 72 65 66 72 75 52 39 44 37 37 37
      after    90 72 70 58 53 50 52 39 46 37 37 37
- The stats box says how much more experience the next level wants:
  `XP: 3877 (123 to next)`, and `(top level)` at the top of the table. It is
  read off the same rung `levelUp` will fire on, so the figure shown is the
  one that counts however the table is written.
- Remembered ground is marked with the dim style as well as the dark shade.
  The shade is the only 24-bit colour in the game and the only thing telling
  ground you have seen from ground you can see, so on an 8-colour terminal,
  where vty drops it, there was no cue at all. `xterm` shows the dim style
  and now has one.
- The README asked for a terminal with 24-bit colour, which was never true.
  Vty clamps what it cannot render: the shade comes out as 256-colour 59 on
  `xterm-256color`, is dropped on `xterm`, and on `vt100` the game draws in
  plain monochrome and plays perfectly well. What is actually needed is a
  terminfo entry and cursor addressing.
- Floors 3 to 12 are caves. They were rectangles joined by straight lines,
  which read as machine-made next to the hand-cut maze on floor 2. The rooms
  are grown a tile at a time instead of stamped, the corridors between them
  wander rather than turning twice, and a couple of cellular-automata passes
  rough the edges; the floors are joined in a ring so there is more than one
  way round.

  The content did not change. Every monster, item, door, NPC and trigger
  keeps its name, its numbers and its job, and was given a new place to
  stand on the new map. Three things are held exactly: a floor is entered
  where the floor above leaves off; a locked door stands where the floor
  cannot be got past without it, with its key on the near side and the
  stairs beyond; and the vault at the bottom is a room with one door and the
  Dungeon Lord outside it.

      before   90 72 60 67 81 81 57 46 40 38 39 33
      after    90 72 65 66 72 75 52 38 44 37 37 37
- Floors 7 to 12 are retuned against a player who opens their pack. They
  were measured against a bot that carried its Empower and Fortify charms to
  the surface unopened, and banking those is eight more attack and eight
  more resistance -- which, on a floor whose monsters hit for 30 against a
  resistance of 25, is most of the fight. Monster attack rises by 3 to 12
  depending on the floor, and the bottom keeps four fewer doses of elixir.
  The Dungeon Lord is back to 130, near the 138 he was cut from when a
  weaker bot could not survive him.

      before this pass   72 82 69 49 40 38   (floors 7-12)
      after              57 46 40 38 39 33

  The second half descends again instead of flattening out.
- Autoplay retreats and uses what it is carrying. It used to walk past every
  permanent upgrade in its pack and never fire a scroll, so every difficulty
  figure this project has quoted came from a player who could not do two
  things a person does constantly.

  It now uses an Empower or Fortify charm the moment it has one, burns a
  Firestorm scroll when three things are in sight, and, when it is nearly
  dead with nothing left to drink, vanishes or blinks or walks away from
  whatever can reach it. A tool is also no longer shed as dead weight: it
  was being weighed against the treasure competing for its slot, which it
  loses every time, being worth less than a crown.

  The dungeon turns out to be a good deal softer than it looked. Floors 7
  to 12 all read easier now, because a player who opens what they are
  carrying is a stronger player than the one the numbers were measured
  against:

      with the new habits   90 72 60 67 81 81 72 82 69 49 40 38
      without them          90 72 60 69 81 80 40 47 44 37 38 21
- The title bar says which floor you are on and how deep the dungeon goes:
  `Rogue nerggnet - Floor 3 of 12 (press ? for help)`. The stats box already
  calls the player's experience level "Level", so depth is named "Floor"
  and never abbreviated.
- The in-game help and the README no longer list `:heal` and `:super`. Both
  commands still work for anyone who knows them; documenting them made a
  cheat look like a feature.
- Floors 2 to 12 have something to do besides bleed. They were generated
  from two ideas -- a blade trap and a fountain -- scaled by depth, and used
  5 of the 11 trigger actions and 2 of the 6 trigger types; everything else
  the trigger system can do sat unused below level 1. All 11 actions and all
  6 trigger types are now in play down there: ambushes that spring from a
  dead end or from picking up the thing they were guarding, caches behind
  loose stones, chutes that drop you somewhere else, rock that shifts, locks
  that open for clearing a floor or for asking the right person, a floor
  boss that drops what it was carrying, and five people still alive down
  there, one of whom wants an errand run.

  Nothing on the way down is gated behind an NPC or an errand, because
  autoplay proves the dungeon beatable and autoplay never talks to anyone.
  Where a trigger opens a locked door it is an alternative to a key that
  still exists.
- Walking into an NPC now changes places with them as well as talking.
- Getting out of the dungeon is something the player does, rather than
  something that happens to them. There used to be four tiles that ended the
  run the instant they were stood on while carrying a Coil of Rope, placed
  one step from the stairs the player arrived by, so a single step in the
  wrong direction finished a run that had barely started.

  Those tiles are `^` shafts now. Standing on one says there is daylight
  overhead and costs nothing; the rope has an `Escape` effect, and using it
  on a shaft climbs to the floor above, landing beside the stairs down. The
  shafts have moved to the far side of their floors, which is what makes a
  rope worth carrying: it buys the walk back. A rope used anywhere else is a
  wasted keypress, not a lost rope.

  Climbing from the first floor is leaving, and ends the run. The other
  ending is new and is where the plot always pointed: a way home at the back
  of the vault on floor 12, behind the Dungeon Lord and the door his sigil
  opens.
- The command line is vi's, rather than a labelled field. It is blank until
  `:` opens it, and then shows the command exactly as typed. It used to be
  prefixed with `Command: `, which put a second colon on the screen next to
  the one the player had just pressed: `Command: :restart`.

### Fixed

- Every door below floor 2 is drawn again. Redrawing those floors as caves
  moved each door entity to a new position without putting a door tile under
  it, so the door still locked, still wanted its key and still stopped the
  player dead -- with plain floor on the screen and nothing to say why. The
  validator now refuses a door the map does not draw as one.
- Blinking can no longer cross a locked door, which means it can no longer
  strand the player. A blink went anywhere on the floor, so it could put you
  inside the vault at the bottom -- whose door wants a sigil carried by the
  thing standing outside it. There was no way back through and nothing left
  to do; the run was over where it stood. A blink now goes anywhere you
  could have walked to, which is also the difference between a blink and a
  key.
- A corpse no longer lies on top of the way out. The marker was drawn over
  whatever tile it fell on, so something dying on a staircase hid it for the
  rest of the run: on floor 7 you could stand on the stairs down and be told
  nothing at all. Corpses are drawn on floor now, and nowhere else.
- A staircase or door you have seen is remembered as one. Both were drawn as
  plain floor once out of sight, so a floor mapped by a Miner's Lantern --
  "maps the whole floor" -- showed everything except the one thing a map is
  for.
- A `Revive` charm catches a blade in the floor as well as a monster's blow.
  `harmPlayer` killed outright without consulting it, so "saves you from one
  death" quietly meant "unless the floor does it". Every death the charm can
  reach now goes through one place.
- Two Health Potions no longer sit in the inventory as separate rows. The
  inventory stacks by name, category and effect value, and level 4's potion
  healed 70 where every other one healed 60 -- one name on two different
  items, which looked for all the world like a display fault. It heals 60
  now, and the validator rejects a dungeon that gives one name two meanings.
  Doses stay exempt: they are what stacking adds up, so the same potion may
  be found in twos and threes.
- Stacking no longer throws away what the second one was worth. Only the
  doses were added, so a second flask scored nothing, and two potions that
  stacked were worth less carried out than two that did not -- which made
  the bug above look like a bonus and fixing it look like a loss.
- An NPC could seal a corridor permanently. Walking into one talked instead
  of moving, and NPCs step aside only on their own clock, so an NPC in a
  one-tile passage with the player on one side and a monster on the other
  had nowhere to go and never moved again. The passage was shut for the rest
  of the run. Found by autoplay, which spent 58,000 turns pressing east into
  a locksmith.
- The border around the map is drawn around the map, rather than around
  whatever space the terminal had going spare. A viewport takes all the room
  it is offered, so on a window taller or wider than the dungeon the border
  stood well clear of it, with a field of empty rows between the last wall
  and the bottom edge. It is limited to the size of the grid now. The limits
  only take room away, and the map is still measured last, so a window too
  small for the level scrolls as it did and the log and the command prompt
  keep their rows.
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
- `CONTRIBUTING.md`, setting out what the project is trying to be — an
  authored dungeon rather than a generated one, content mistakes caught
  before the game starts, and beatability proved by machine — and where to
  add a new floor, Special item effect, trigger type or trigger action.
- `CLAUDE.md`, the same ground in short form for coding agents.

### Removed

- `compile_haskell.sh`, which concatenated `src/` and `app/` into one
  markdown file for pasting into a chat window. Tooling reads the repository
  directly now.

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

### Changed

- A bad `world.json` is now reported instead of crashing the game. Reading
  the configuration used to call `error` in twenty-five places, so a single
  typo killed the process with a bare message and no idea which level, item
  or trigger it came from. Every one of those is now a described problem,
  and independent checks are combined so one run lists everything wrong
  rather than stopping at the first. Messages name the level, the entity and
  the field, and unknown values list what was expected.
- Two problems that previously went unreported are now caught: a map whose
  rows are not all the same length, which used to fail only when the player
  walked into the short part, and a first level with no `S` tile, which
  silently started the player at the top-left corner.
- `:restart` reports a world file that no longer loads in the message log
  instead of taking the running game down with it.

### Added

- The dungeon goes twelve floors down. The first level is untouched except
  that finding the Gold Coin and the Magic Ring no longer ends the game; the
  way out is at the bottom now. Below it are eleven new floors with 24 kinds
  of monster, 34 items, four sorts of key, traps, shrines, and escape shafts
  on the third, sixth and ninth floors for anyone who would rather bank what
  they are carrying than go deeper. At the bottom the Dungeon Lord stands in
  front of a vault holding 7000 in treasure, and the sigil that opens it is
  what falls when the Lord does.
- `Game.Autoplay`, which plays the dungeon through the same functions the
  keyboard drives, so a hand-drawn dungeon can be checked for being
  finishable at all. The test-suite plays the shipped one on several seeds
  and fails if it cannot be beaten, or if it is beaten without the player
  ever being brought into danger.
- `harmPlayer` and `healPlayer` trigger actions, for traps and for fountains.
  A trap can kill, so it is a real cost rather than scenery.
- A run has a score. Items carry an `itemValue`, the sidebar shows what is
  being carried, and both endings report the same three figures: how deep
  the run got, what it was worth, and the experience earned. Getting out
  keeps the treasure and dying loses it, so pressing on is a wager rather
  than a formality, and two attempts at the same dungeon can be compared.
  Every item already in `world.json` has been given a value.
- A `monsterDefeated` trigger, which fires once a monster of the given name
  has been beaten. It remembers defeats rather than asking whether one is
  currently alive, so it works for a boss that waits as an `inactive`
  template: "none alive" would otherwise be true before the boss ever
  appeared. Melee, ranged attacks and Firestorm all record a kill, and a
  trigger may name a template the level has not yet called up.
- An item a trigger asks the player to carry is now looked for across every
  level at or above it, rather than only the one the trigger is on. A rope
  found on the second floor is what opens the way out of the sixth, and the
  old rule called that an error.
- Loading checks that a level can be played, not just that it parses. Every
  monster, item and NPC has to be reachable from where the player arrives,
  and so do the stairs down; no door may be drawn inside a wall; the stairs
  between levels have to line up, since going down leaves the player where
  they are; and every locked door needs a key findable by then, or a trigger
  that opens it. A map drawn by hand is one wall away from sealing off a
  room, and nothing used to say so.
- Special items do something. All eleven of them printed "Its effect is
  mysterious." and stopped, and several carried an `itemEffectValue` that
  nothing read. They now declare an `itemEffect` in `world.json`, so a new
  one can be written as data rather than as Haskell: `Empower`, `Fortify`,
  `Reveal`, `Blink`, `Firestorm` and `Vanish` fire once and are used up,
  while `Regenerate`, `Lifesteal` and `Revive` work away as long as the item
  is carried. `Keepsake` is the one that deliberately does nothing, for the
  quest items a trigger asks for.
- Loading rejects a Special item with no effect, since it would be inert,
  and an effect on any other category, since nothing would read it.
- Damage is rolled rather than calculated. The game had no randomness in it
  at all, so every fight was arithmetic with an answer you could work out in
  advance. A blow now lands within a quarter either side of the attacker's
  strength, in melee and at range alike; an attack that cannot beat the
  defender's resistance still does nothing rather than scraping a point
  through. The average is the attacker's strength, so the existing balance
  is untouched.
- A generator lives in the game state and is saved with it, restored exactly,
  so reloading carries the sequence on instead of starting it again. A new
  game seeds from the system, and tests seed a fixed one so they stay
  repeatable.
- The help covers every key the game responds to. `Esc`, `Enter`, `Backspace`,
  the letters that choose an item or a ranged target, and the `:heal` and
  `:super` cheats were all undocumented in game, though the last two were in
  the README. That does not fit an 80x24 screen in one popup, so `?` now
  steps through three pages and then closes.
- Dying shows a screen, as winning already did. It previously only wrote a
  line to the log, which scrolls away, leaving no hint as to why the keys
  had stopped responding.
- Choosing an item to use or drop shows the inventory over the map. The
  sidebar cannot list a full inventory on a short terminal (it needs 27 rows
  and an 80x24 screen has 13 to spare), so the later keys were unreachable
  at exactly the moment the player needed to press one.

### Fixed

- `world.json`: a corridor on the third level was sealed by a single wall,
  stranding the Silver Key and the Tome of Arcane Knowledge where no route
  could reach them. One character opens it. This is the bug the new checks
  were written to catch, and the first thing they caught.
- Monsters find their way to the player instead of walking into walls. They
  used to step in whichever direction shortened the straight line, so a wall
  between them and the player pinned them against it for as long as the
  player stayed there. A breadth-first search out from the player gives each
  one the real distance to follow downhill, so they round corners and take
  the shortest way. One search serves every monster on the level.
- Because that distance is the walk rather than the straight line, a monster
  four tiles away through a wall no longer gives chase, and one behind a
  locked door waits for it to be opened.
- A save is read field by field, with everything to do with what is on
  screen defaulting when absent. Adding the help page counter would
  otherwise have made saves from earlier versions unreadable, as the
  trigger rewrite did.
- Escape now closes the item chooser rather than only leaving command mode,
  which used to leave the game believing a choice was still pending.
- The message pane is always the same height, blank rows included. It used to
  be as tall as the log was long, and since the map takes whatever vertical
  space is left, the map shrank by up to seven rows as messages arrived and
  grew again as they aged out.
- The map is drawn in a viewport that scrolls to follow the player, so the
  screen fits the terminal. It previously laid the whole dungeon out at full
  size, which needed a window 38 to 40 rows tall depending on how full the
  message log was. Anything shorter had its bottom rows quietly cut off, and
  the bottom is where the command prompt and the newest log line are, so the
  symptoms looked like three separate bugs:
    - the prompt was missing entirely;
    - "You see: ..." for the item underfoot never appeared, leaving an older
      line such as "There is nothing to pick up here." as the last one
      visible;
    - "You picked up: ..." only showed up after the next keypress had pushed
      it up a row into view.
- Dying or winning now clears the save file instead of writing it. The game
  saved unconditionally on exit, so a death was stored and every later launch
  dropped the player straight back onto the game over screen, needing
  `:restart` to escape. Not writing would have been worse than it sounds:
  the save from earlier in the run would still be there, so quitting after a
  death would have undone it.
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
