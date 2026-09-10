---
name: create-release-pr
description: develop -> main のリリースPRを作成するスキル。「リリースPRを作って」「develop -> mainのPR作成」などのタスクで発動する。
---

# リリースPR作成（develop -> main）

`{OWNER}/{REPO}` は対象のGitHubリポジトリに読み替える。

## 手順

### Step 1: origin の最新を取得し差分を確認
- `git fetch origin main develop` で最新を取得
- `git log origin/main..origin/develop --oneline` で含まれるコミットを確認
- `git diff origin/main...origin/develop --stat` で差分ファイルを確認
- 差分がない場合はユーザーに報告して終了

### Step 2: 含まれるPRを特定
- コミットメッセージから含まれるPR番号を抽出する（`(#XXXX)` パターン）
- リバート→再リバートなど相殺されるものは最終的な状態で判断する

### Step 3: Notionリンクの収集
- 各PRの本文を `gh pr view {PR番号} --repo {OWNER}/{REPO} --json body` で取得
- 本文中のNotionリンク（`https://www.notion.so/...`）を収集する
- リリース承認資料のリンクがない場合はユーザーに確認する

### Step 4: PR本文の作成
以下のフォーマットでPR本文を組み立てる:

```
リリース承認資料:
- {Notionリンク1}
- {Notionリンク2}
...

{PR番号リスト（#1234 #1235 形式）}

{変更の要約（日本語、1-2行）}
```

### Step 5: ユーザーに確認
- タイトル: `develop -> main`
- 本文を提示してユーザーの承認を得る

### Step 6: PR作成
- `gh pr create --repo {OWNER}/{REPO} --base main --head develop` で作成
- 作成後のPR URLをユーザーに報告する
