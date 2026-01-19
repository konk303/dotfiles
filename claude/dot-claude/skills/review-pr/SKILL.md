---
name: review-pr
description: Reviews pull requests for code quality, security, style, and test coverage. Use when asked to review PRs, check code changes, or assess merge readiness.
allowed-tools:
  - Read
  - Grep
  - Glob
  - Bash(gh:*)
  - Bash(git:*)
---

# Pull Request Review

## Overview

Perform comprehensive PR reviews covering code quality, security, style, and test coverage.

## Review Process

### 1. Gather Context

First, understand what the PR is about:

```bash
# Get PR details (if PR number provided)
gh pr view <number> --json title,body,files,additions,deletions

# Get the diff
gh pr diff <number>

# Or for current branch
git diff main...HEAD
```

### 2. Code Quality Review

Check for:
- **Logic correctness** - Does the code do what it's supposed to?
- **Edge cases** - Are boundary conditions handled?
- **Error handling** - Are errors caught and handled appropriately?
- **Performance** - Any N+1 queries, expensive operations, or memory issues?
- **Maintainability** - Is the code readable and well-structured?
- **DRY principle** - Is there unnecessary duplication?

### 3. Security Review

Check for:
- **SQL injection** - Raw SQL queries with user input
- **XSS vulnerabilities** - Unescaped user content in views
- **Authentication gaps** - Missing auth checks
- **Authorization issues** - Missing permission checks
- **Mass assignment** - Unprotected attributes
- **Sensitive data exposure** - Secrets, PII in logs or responses
- **CSRF protection** - Missing tokens on state-changing requests

### 4. Style & Conventions

Check for:
- Consistent naming conventions
- Proper code organization
- Documentation where needed
- Adherence to project style guides

### 5. Test Coverage

Check for:
- Tests for new functionality
- Tests for edge cases and error conditions
- Tests for security-sensitive code paths
- Proper test descriptions and organization

## Output Format

Provide feedback in Japanese using this structure:

```
## 概要
[PRの概要と全体的な評価]

## 良い点
- [良くできている点]

## 指摘事項

### 重大（必ず修正）
- [セキュリティ問題、バグ、破壊的変更]

### 提案（検討推奨）
- [改善点、より良いアプローチ]

### 軽微（任意）
- [細かいスタイルの問題、好みの範囲]

## 質問
- [明確化が必要な点]

## 判定
[承認 / 要修正 / 要議論]
```

## Usage Examples

- "Review PR #123"
- "Review the current PR"
- "Check if this PR is ready to merge"
- "Review PR #456 focusing on security"
