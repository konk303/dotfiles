---
name: dependabot-release-approval
description: Dependabot の gem / GitHub Actions 更新 PR を確認し、develop に追従させて最新版まで上げ（gem は bundle update --conservative）、PR description を整えてからリリース承認資料を作成するスキル。「Dependabot PR をリリースできる状態に」「dependabot PR のリリース承認資料」などのタスクで発動する。引数に PR 番号（複数可）を取る。
---

# Dependabot PR の更新とリリース承認資料の作成

引数: PR 番号（例: `/dependabot-release-approval 1266`、複数なら `/dependabot-release-approval 1266 1267 1268`）

複数 PR を渡された場合は、Step 1〜6 を PR ごとに順に行い、Step 7 でリリース承認資料を 1 つにまとめる。

## 前提

- `{OWNER}/{REPO}`、ベースブランチ（例: `develop`）、devcontainer の有無は対象プロジェクトの `CLAUDE.md` から読み取る
- git / gh はローカルで実行し、bundle / rspec / rake は devcontainer 内で実行する（`devcontainer exec --workspace-folder . <command>`）
- devcontainer が起動していなければユーザーに起動を依頼して待つ
- 作業開始時の現在ブランチを控えておき、最後に必ず戻す。作業ツリーに未コミットの変更があれば先にユーザーに確認する

## 手順

### Step 1: PR の状態確認
- `gh pr view {PR} --json title,headRefName,baseRefName,commits,files,body` で以下を確認する
  - 対象 gem と、Dependabot が上げたバージョン
  - ベースブランチに追従しているか（`git merge-base --is-ancestor origin/{base} origin/{head}`）
  - コミットの作成者（Dependabot 以外のコミットがあるか）
  - 2 コミット以上ある場合は、ベースとの正味の差分（`git diff $(git merge-base origin/{base} origin/{head}) origin/{head}`）で実際の変更を確認する
- 最新版を確認する: `curl -s https://rubygems.org/api/v1/versions/{gem}/latest.json`（GitHub Actions は後述）
- 追従のしかたを決める:
  - **コミットが Dependabot のものだけ** かつ **Dependabot の版が最新**: `gh pr comment {PR} --body "@dependabot rebase"` で Dependabot に rebase させる。Dependabot が引き続き PR を管理できるため、手動で作り直すより良い。rebase は非同期なので、`git merge-base --is-ancestor` でベースに追従したことを確認してから先へ進む（数分かかる）。この場合 Step 3〜5 は行わない
  - **Dependabot 以外のコミットがある**、または **さらに新しい版まで上げる**: Step 3〜5 で手動で作り直す。人の手が入った PR に `@dependabot rebase` をコメントしても "edited by someone other than Dependabot" で拒否される
- `@dependabot rebase` は PR description を Dependabot の本文で上書きする。先頭に付けたリリース承認資料のリンク等は消えるので、rebase 後に Step 6 で付け直す

### GitHub Actions の PR（`dependabot/github_actions/...`）
gem の手順のうち、以下を読み替える:
- 最新版: `gh api repos/{owner}/{action}/releases/latest --jq .tag_name`。ワークフローはメジャータグ（`@v7` 等）で指定しているため、PR のメジャーが最新なら追加の更新は不要
- 対象箇所: `git grep -n "{owner}/{action}@" origin/{base} -- .github` で、ベースブランチ上の全ての使用箇所が PR の差分に含まれているか確認する
- 影響範囲: メジャー更新の破壊的変更を、使っているトリガー（`pull_request_target` / `workflow_run` 等）や `with:` の入力と突き合わせる。旧メジャータグが既に最新のマイナーを指していれば（`gh api repos/{owner}/{action}/git/ref/tags/v3` と `v3.x.y` の SHA が同じ）、今回変わるのは新メジャー分だけ
- Step 3〜4 の bundle・spec は行わない。動作確認は PR の CI と、マージ後のワークフロー実行（デプロイ・通知など）で行い、それを staging の確認項目の代わりにする
- 差分リンク: `https://github.com/{owner}/{action}/compare/{旧タグの実際の版}...{新しい版}`（例: `v3.0.5...v4.0.0`）

### Step 2: リリースノートと影響範囲の確認
- 現在のロックのバージョン → 最新版までのリリースノートを読む（GitHub Releases、無ければ CHANGELOG.md）
- アプリ内での使われ方を grep で特定し、各変更点が実際に影響するかを判断する。例:
  - 最低 Ruby バージョン（`.ruby-version` と比較）
  - アプリで使っている API・オプションに関係する変更か
  - JS を同梱する gem は、その JS がどのレイアウト・エントリポイントで読み込まれているか
  - 依存 gem の追加・削除・バージョン上限
- セキュリティ修正を含むか確認し、含む場合は明記する
- リリースノートと差分のリンクを控えておく（Step 6・7 で使う）
  - リリースノート: GitHub Releases のページ、無ければ CHANGELOG.md のファイル URL
  - 差分: `https://github.com/{gem の repo}/compare/{旧タグ}...{新タグ}`（旧はベースのロックのバージョン、新は最終バージョン）。タグ名はリポジトリごとに違う（例: `v1.2.3`、モノレポは `google-cloud-storage/v1.62.1`）
  - `curl -s -o /dev/null -w '%{http_code}' {URL}` が 200 になることを確認する

### Step 3: ベースブランチ上でバージョンを上げる
- 既存のコミットを rebase するのではなく、ベースブランチから作り直す（Dependabot が他の gem まで上げて人が戻した、のような履歴を引き継がないため）:
  ```bash
  git fetch origin {base} {head}
  git switch -c {作業ブランチ} origin/{base}
  ```
- `Gemfile` のバージョン制約: Dependabot が `Gemfile` を変更していた場合は、下限を最新版に合わせる（例: `~> 1.59` → `~> 1.62`）。変更していなければ `Gemfile` は触らない
- `devcontainer exec --workspace-folder . bundle update {gem} --conservative`
- `git diff Gemfile.lock` で、対象 gem と、その gem が必要とする依存以外が変わっていないことを確認する。想定外の変更があればユーザーに報告して止める

### Step 4: テスト
- Step 2 で特定した、影響を受ける機能に関係する spec だけを実行する（全件は CI に任せる）
- spec がモック（OmniAuth のテストモード、test 環境の Disk ストレージ等）で実際の経路を通らない場合は、その旨を控えておき staging の確認項目にする
- スキーマ管理系の gem（ridgepole 等）は development DB に対してドライランを実行し、差分が出ないことを確認する（例: `DRYRUN=true bin/rake ridgepole:apply`）。ドライランで書き換わるファイル（`db/schema.rb` 等）があれば元に戻す
- `Gemfile` を変更した場合は `bin/rubocop Gemfile`

### Step 5: コミットと push
- コミットメッセージ: `chore(deps): bump {gem} from {ベースのバージョン} to {最新版}`
- 既存のブランチを置き換えるため、取得時のコミットを指定して push する:
  ```bash
  git push --force-with-lease={head}:{取得時の head の SHA} origin {作業ブランチ}:{head}
  ```
- 作業開始時のブランチに戻し、作業ブランチを削除する。devcontainer の gem をそのブランチのロックに合わせるため `bundle install` を実行する

### Step 6: PR タイトルと description の更新（ユーザー承認後に実行）
- タイトル: `chore(deps): bump {gem} from {ベースのバージョン} to {最新版}`
- description は Dependabot の英語の本文を残さず、以下の構成で日本語に書き直す。リリース承認資料が既にあればリンクを先頭に置く:
  ```markdown
  ## リリース承認資料
  {Notion URL（Step 7 で作成する場合はその後に追加）}

  ---

  ## 概要
  {gem} を {旧} → {新} に更新する。
  （Dependabot の PR より新しい版まで上げた場合はその旨）

  ## 変更内容
  - `Gemfile`: ...（変更した場合のみ）
  - `Gemfile.lock`: ...（依存の追加・削除も書く。他の gem の変更が無いことを明記）

  ## リリースノート
  - {バージョン}: {要点}
  差分: {compare URL}

  ## 影響範囲
  {アプリでの使われ方と、変更点が影響するかどうか}

  ## テスト
  - [x] {実行した spec・ドライランと件数}
  - [ ] CI がパス
  - [ ] staging: {確認項目}

  🤖 Generated with [Claude Code](https://claude.com/claude-code)
  ```
- staging の確認項目は少なくする。実際にリスクのある経路だけを、1 gem あたり 1〜2 項目にまとめる
- **Dependabot が管理を続ける PR（`@dependabot rebase` で追従させた PR）** の場合は書き直さない。タイトルはそのままにし、上の構成の「リリース承認資料」〜「テスト」と区切りの `---` を Dependabot の本文の先頭に追加するだけにする（次の rebase で上書きされるため。既に先頭にリンク等があれば置き換える）
- 更新前にタイトルと description をユーザーに提示し、承認を得る

### Step 7: リリース承認資料
- 既存の承認資料が PR description にリンクされている場合（複数 gem をまとめた資料など）は、新規作成せずにその資料を更新する:
  - 変更内容の表・個別の変更点・PR 一覧のバージョンを最新版に合わせる
  - 個別の変更点の各 gem に、Step 2 で控えたリリースノートと差分のリンクを付ける（例: `- **ridgepole 3.2.7**（[CHANGELOG](...) / [差分](.../compare/v3.1.3...v3.2.7)）: ...`）。既にリンクがあれば差分の範囲を最終バージョンに合わせる
  - テストケースを Step 6 の staging 確認項目に合わせて絞る
  - 更新内容をユーザーに提示し、承認を得てから反映する
- 承認資料が無い場合は `/create-release-approval {PR}` に従って作成する。複数 PR を渡された場合は 1 つの資料にまとめる（名前の例: `[タスク]リリース承認資料：gem依存関係のアップデート（gem1, gem2, ...）`）。作成時も、各 gem の変更点にリリースノートと差分のリンクを付ける

### Step 8: 完了報告
PR ごとに以下を報告する:
- 最終バージョン（`旧 → 新`）と、Dependabot の PR より上げたかどうか
- 実行したテストと結果、モックのため実経路を通っていない部分
- push したコミットと、PR・承認資料の更新内容
- 作業ブランチから元のブランチに戻したこと
