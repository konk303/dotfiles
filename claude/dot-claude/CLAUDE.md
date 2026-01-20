# Personal CLAUDE.md

This file contains personal configuration for Claude Code usage.

## Devcontainer Usage (Personal Preference)

When working with the manager project from outside the devcontainer, always use:

```bash
# All commands should be run with devcontainer exec
devcontainer exec --workspace-folder . <command>

# Examples:
# Initial setup
devcontainer exec --workspace-folder . bin/setup

# Start development server (Rails + Tailwind watcher)
devcontainer exec --workspace-folder . bin/dev

# Run console
devcontainer exec --workspace-folder . bin/rails console

# Database operations (uses Ridgepole, not Rails migrations)
devcontainer exec --workspace-folder . bin/rake ridgepole:apply
devcontainer exec --workspace-folder . bin/rake ridgepole:dry-run
devcontainer exec --workspace-folder . DRYRUN=true bin/rake ridgepole:apply      # Dry run with environment flag
devcontainer exec --workspace-folder . VERBOSE=true bin/rake ridgepole:apply     # Verbose output
devcontainer exec --workspace-folder . DROP_TABLE=true bin/rake ridgepole:apply  # Allow table drops
devcontainer exec --workspace-folder . bin/rake ridgepole:export                 # Export current schema

# Database seeding (creates default roles and accounting subjects)
devcontainer exec --workspace-folder . bin/rails db:seed

# Code generation (auto-generates scaffolds from Schemafile)
devcontainer exec --workspace-folder . bin/generate_with_attributes [model_name]

# Background jobs
devcontainer exec --workspace-folder . bundle exec sidekiq

# Testing & Quality
devcontainer exec --workspace-folder . bundle exec rspec
devcontainer exec --workspace-folder . bundle exec rspec spec/path/to/spec.rb
devcontainer exec --workspace-folder . COVERAGE=true bundle exec rspec
devcontainer exec --workspace-folder . bin/rubocop
devcontainer exec --workspace-folder . bin/rubocop -a
devcontainer exec --workspace-folder . bin/brakeman
```

**IMPORTANT: Always execute commands inside the devcontainer using the above patterns**

## Git Commands (Personal Preference)

**Git commands should be executed LOCALLY, not through devcontainer:**

```bash
# Correct - Run git commands locally
git status
git add .
git commit -m "message"
git push
git pull

# Incorrect - Do NOT run git through devcontainer
devcontainer exec --workspace-folder . git status  # ❌ DON'T DO THIS
```

**Reason**: Git operations need to use the local git configuration (user.name, user.email) and should not be run inside the devcontainer environment.