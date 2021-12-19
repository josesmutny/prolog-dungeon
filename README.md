## Prolog text adventure

This prolog program is a text adventure game programmed on prolog. Great care was taken to use logical programming as
much as possible.

The game rules are as follows:

### User commands

- `north.`, `south.`, `east.`, `west.` moves player one room in the indicated direction. This occurs only if a door
  between this rooms exists and it is either unlocked, or the player has a valid key (more about this on the key
  section).
- `teleport.` if the player has a teleport device with him, this allows him to move to the direction the teleport is
  pointing
- `exit.` Exit the game
- `write_status.` write player inventory and life state. 

### Door security

Two types exist (Gold, Iron). You can carry many keys of the same type, but no more than 5 items in total.
One key opens its door.

Once unlocked a door cannot be locked again, and it remains open until restarting the game.

Attempting to move towards a locked door does not result in additional damage from live enemies in the room. 

### Attack mechanics

Three enemies exist:

- Orc, which can be killed with any weapon
- Ogre, which can be killed with an axe
- Ghost, which cannot be killed

Once picked, a weapon cannot be dropped, and it counts towards the 5 item limit.

The player at the beginning of the game has 3 lives. Every attack (from any entity) removes one of them, eventually killing the player.
A dead player cannot move, teleport, or pick objects.

## Testing

Use `set_instant.` to get rid of delays, making testing much less tedious.

The default map can be represented by this image:

![Dungeon map](/map.png)

## WIP
- Properly test whole application
- Add mechanic for beating the game (King is found, take him back to the beginning)
- Add more enemies, and more than one enemy per room
- Make status printing more orderly
- Add more weapons
- Add a Z dimension for rooms (`theres a locked trapdoor leading on the floor/ceiling`)