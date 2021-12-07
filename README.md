## Prolog text adventure

This prolog program is a text adventure game programmed on prolog. Great care was taken to use logical programming as
much as possible.

The game rules are as follows:

### User commands

- `north(), south(), east(), west()` moves player one room in the indicated direction. This occurs only if a door
  between this rooms exists and it is either unlocked, or the player has a valid key (more about this on the key
  section).
- `teleport()` if the player has a teleport device with him, this allows hims to move to the direction the teleport is
  pointing
- `exit()` did mom finish dinner? are you tired? Every game must end once
- `write_inventory()` shows the player the items he currently possesses. 

## WIP