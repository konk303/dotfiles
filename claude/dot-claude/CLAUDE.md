# Personal CLAUDE.md

## Devcontainer Projects

When working with devcontainer-based projects from outside the container:

```bash
devcontainer exec --workspace-folder . <command>
```

Common patterns:
- Setup: `devcontainer exec --workspace-folder . bin/setup`
- Dev server: `devcontainer exec --workspace-folder . bin/dev`
- Console: `devcontainer exec --workspace-folder . bin/rails console`
- Tests: `devcontainer exec --workspace-folder . bundle exec rspec`
- Linting: `devcontainer exec --workspace-folder . bin/rubocop`

## Git Commands

**Run git locally, not through devcontainer:**

```bash
# Correct
git status
git commit -m "message"

# Incorrect
devcontainer exec --workspace-folder . git status
```

Reason: Git needs local configuration (user.name, user.email).