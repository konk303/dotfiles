---
description: Comprehensive verification system - run build, lint, test, security scan, and diff review before PR.
---

# Verification Loop

A comprehensive verification system for Claude Code sessions.

## When to Use

Invoke this skill:
- After completing a feature or significant code change
- Before creating a PR
- When you want to ensure quality gates pass
- After refactoring

## Verification Phases

### Phase 1: Build Verification
```bash
# Check if project builds
# Adapt to project: bundle exec rails assets:precompile, npm run build, etc.
```

If build fails, STOP and fix before continuing.

### Phase 2: Type / Static Analysis
```bash
# Ruby: rubocop, sorbet
# TypeScript: tsc --noEmit
# Python: pyright, mypy
```

Report all errors. Fix critical ones before continuing.

### Phase 3: Lint Check
```bash
# Ruby: rubocop
# JavaScript/TypeScript: eslint
# Python: ruff check
```

### Phase 4: Test Suite
```bash
# Run tests with coverage
# Target: 80% minimum
```

Report:
- Total tests: X
- Passed: X
- Failed: X
- Coverage: X%

### Phase 5: Security Scan
```bash
# Check for secrets
# Ruby: brakeman, bundler-audit
# JS: npm audit
# General: grep for hardcoded keys/tokens
```

### Phase 6: Diff Review
```bash
# Show what changed
git diff --stat
git diff HEAD~1 --name-only
```

Review each changed file for:
- Unintended changes
- Missing error handling
- Potential edge cases

## Output Format

After running all phases, produce a verification report:

```
VERIFICATION REPORT
==================

Build:     [PASS/FAIL]
Types:     [PASS/FAIL] (X errors)
Lint:      [PASS/FAIL] (X warnings)
Tests:     [PASS/FAIL] (X/Y passed, Z% coverage)
Security:  [PASS/FAIL] (X issues)
Diff:      [X files changed]

Overall:   [READY/NOT READY] for PR

Issues to Fix:
1. ...
2. ...
```

## Integration with Hooks

This skill complements PostToolUse hooks but provides deeper verification.
Hooks catch issues immediately; this skill provides comprehensive review.
