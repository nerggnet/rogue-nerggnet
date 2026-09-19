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

## 1

- Initial version.
