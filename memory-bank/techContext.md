# Tech Context

## Technologies Used
- [Fill in technologies used here]

## Development Setup
- [Fill in development environment setup here]

## Technical Constraints
- [Fill in technical constraints here]

## Dependencies
- [Fill in project dependencies here]

### Haskell Cabal Mixins Configuration Summary
With `mixins: base hiding (Prelude), relude (Relude as Prelude, ...)` configured in `.cabal`, `Prelude` in your code should refer to `Relude` (from the `relude` package) instead of the default `base` package's `Prelude`. This means your code should utilize `Relude`'s more modern and ergonomic alternative, and modules like `Relude.Container.One` should be directly available.

## Tool Usage Patterns
- *(Important)* After modifying `.cabal` or `.nix` files, user confirmation is required before proceeding.
- Memory bank updates should be confined to a single appropriate file; duplicate information across multiple locations is unnecessary.
- After each coding session, `cabal build` must be run for verification.
