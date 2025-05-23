# Active Context

## Current work focus
- Documenting the experience of using Hackage tools and general tool interaction best practices.

## Recent changes
- Successfully retrieved `rel8` API documentation using `haskell-hackage-mcp`.
- Corrected `plan_mode_respond` tool usage: Ensured response content is always encapsulated within `<response>` tags.
- Created `memory-bank/progress.md` with task summary and lessons learned.

## Next steps
- Complete documentation of current session's experience in `activeContext.md`.

## Active decisions and considerations
- Emphasizing clear and precise tool parameter usage.
- Reinforcing the importance of step-by-step verification of tool execution results.
- Maintaining comprehensive Memory Bank documentation for future sessions.

## Important patterns and preferences
- Prioritizing the use of specialized MCP tools when available for specific tasks.
- **Documentation Lookup Preference**: Prioritize `Context7` (`github.com/upstash/context7-mcp`) for general documentation lookup.
- **API Lookup Preference**: Prioritize `haskell-hackage-mcp` for Haskell API specific documentation.
- Adhering strictly to tool usage formatting (XML tags for parameters).

## Learnings and project insights
- The `haskell-hackage-mcp` tool is effective for retrieving Haskell package documentation.
- Proper tool parameter formatting is crucial for successful execution.
- Documenting operational experiences helps in continuous improvement.
