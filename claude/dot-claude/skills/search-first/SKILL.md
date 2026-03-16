---
description: Research-before-coding workflow. Search for existing tools, libraries, and patterns before writing custom code.
---

# Search First — Research Before You Code

Systematizes the "search for existing solutions before implementing" workflow.

## Trigger

Use this skill when:
- Starting a new feature that likely has existing solutions
- Adding a dependency or integration
- The user asks "add X functionality" and you're about to write code
- Before creating a new utility, helper, or abstraction

## Workflow

1. **Need Analysis** — Define what functionality is needed, identify language/framework constraints
2. **Parallel Search** — Search package registries (RubyGems, npm, PyPI), MCP servers, GitHub
3. **Evaluate** — Score candidates (functionality, maintenance, community, docs, license, deps)
4. **Decide** — Adopt as-is / Extend with wrapper / Build custom
5. **Implement** — Install package / Configure / Write minimal custom code

## Decision Matrix

| Signal | Action |
|--------|--------|
| Exact match, well-maintained, MIT/Apache | **Adopt** — install and use directly |
| Partial match, good foundation | **Extend** — install + write thin wrapper |
| Multiple weak matches | **Compose** — combine 2-3 small packages |
| Nothing suitable found | **Build** — write custom, but informed by research |

## Quick Checklist

Before writing a utility or adding functionality:

0. Does this already exist in the repo? → search through relevant modules/tests first
1. Is this a common problem? → Search RubyGems/npm/PyPI
2. Is there an MCP for this? → Check configured MCP servers
3. Is there a skill for this? → Check `~/.claude/skills/`
4. Is there a GitHub implementation/template? → Search for maintained OSS

## Anti-Patterns

- **Jumping to code**: Writing a utility without checking if one exists
- **Ignoring MCP**: Not checking if an MCP server already provides the capability
- **Over-customizing**: Wrapping a library so heavily it loses its benefits
- **Dependency bloat**: Installing a massive package for one small feature
