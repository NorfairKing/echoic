# Echoic

A blind computing environment for eyes-free computer use.

## Project overview

### Vision

Enable near-normal computing efficiency without vision. Build and learn
the system now while sighted, so the skills and tooling exist if ever needed.

### Why "Echoic"

Named after echoic memory - the auditory sensory memory that briefly
holds sound. The system echoes back what you need to know.

### Principles

- **Offline-first**: Must work without internet. Resource usage is
  acceptable, network dependency is not.
- **Efficiency over accessibility theater**: "Possible" isn't enough.
  Must be practical for real, sustained work.
- **Learn while sighted**: Usable and worth using now, building muscle
  memory for the future.
- **Audio is primary**: Everything meaningful must be audible.
- **Keyboard-only**: Touch typing assumed. No mouse dependency.
- **Hybrid integration**: Wrap existing tools where muscle memory exists,
  build new where audio-native design wins.

### Technical Stack

- **Claude**: Claude will be writing echoic for the most part. Make sure to update this file when something changes that claude should be aware of every time.
- **Haskell**: Strong types catch errors at compile time (critical when
  you can't see error messages easily)
- **Nix**: Reproducible builds, declarative dependencies
- **Piper TTS**: Neural text-to-speech, runs locally offline

### Essential Workflows (Priority Order)

1. **Coding** - editing, navigating, building, debugging, version control
2. **AI assistance** - Claude conversations (potentially the primary interface)
3. **Communication** - email, chat
4. **Shell/sysadmin** - terminal, processes, logs
5. **Browser** (future) - reading, forms, navigation

### Current State

Bootstrap phase - establishing project structure.

First big milestone: be able to develop echoic in itself.

## Feedback loop

These feedback loops are designed to give fast feedback during development.
They let you define when you can move on to the next step, and what to run to get there.

Make sure to always decide which feedback you will be using before starting a task.
No plan is complete before the feedback loops and checklists are defined.

### General

Every change MUST pass CI.
CI is defined as `nix flake check` passing successfully.

For quicker feedback during development, get each of the following to pass
locally, in order, before moving on to the next.
Make sure they all pass before pushing code.

1. stack test --fast --no-run-tests --pedantic
2. pre-commit run -a
3. nix build
4. nix flake check
