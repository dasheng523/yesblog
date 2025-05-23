# Active Context

## Current work focus
- Developing Beam database examples.
- Resolving Haskell/Beam compilation issues.

## Recent changes
- Successfully read Beam documentation tutorials.
- Defined Beam Schema and example code in `src/BeamExample.hs`.
- Modified `app/Main.hs` to call the Beam example.
- Updated `yesblog.cabal` to include Beam dependencies and expose the `BeamExample` module.
- Resolved compilation errors for the `BeamExample` module, including `hidden module` and `deriving strategy` issues.

## Next steps
- Complete the development and integration of the Beam example.

## Active decisions and considerations
- Emphasizing clear and precise tool parameter usage.
- Reinforcing the importance of step-by-step verification of tool execution results.
- Maintaining comprehensive Memory Bank documentation for future sessions.
- Decided to temporarily abandon Beam's automatic migration feature and switch to manually creating database tables to simplify the compilation process and prioritize successful code compilation.
- Clarified the importance of `deriving` strategies in Haskell and learned how to correctly use `stock` and `anyclass`.
- Recognized the necessity of `UndecidableInstances` when dealing with complex type families (like Beam's `Columnar`).

## Important patterns and preferences
- Prioritizing the use of specialized MCP tools when available for specific tasks.
- **Documentation Lookup Preference**: Prioritize `Context7` (`github.com/upstash/context7-mcp`) for general documentation lookup.
- **API Lookup Preference**: Prioritize `haskell-hackage-mcp` for Haskell API specific documentation.
- Adhering strictly to tool usage formatting (XML tags for parameters).

## Learnings and project insights
- Beam's type system and deriving mechanisms require precise language extensions and deriving strategies.
- The `UndecidableInstances` extension is necessary when dealing with `Columnar` type families.
- `PrimaryKey`'s `Show` and `Eq` instances need to be derived separately for the `Identity` version.
- The configuration of `exposed-modules` in the `.cabal` file is crucial for module visibility.
- Database migration functionality in Beam requires the `beam-migrate` package, but its usage may need adjustment based on version and specific requirements.
