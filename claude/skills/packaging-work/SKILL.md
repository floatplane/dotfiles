---
name: packaging-work
description: >-
  Package a feature into commits and PRs the way Brian wants. Trigger when about to commit,
  split changes, push, or open a PR — especially when work spans architectural layers or
  multiple branches, or when Brian asks for a stack.
---

# Packaging work into commits and PRs

Follow these unless Brian says otherwise for this task.

## Commits
- Split commits along architectural layers, not by chronology. Typical split: one commit for
  prompt/model changes, a separate commit for the plumbing/wiring that carries a new field or
  flag through the code.
- Each commit should stand on its own and read as one deliberate step. The commit *sequence is
  a narrative* Brian preserves — do not squash it away, and never `git commit --amend`
  (make a new commit instead).

## Stacks and PRs
- For a multi-part feature, build it as a stacked series of branches, one phase per branch.
- **Hold PRs until Brian has seen the whole thing.** Commit each phase, but do not open PRs
  across the stack until he asks — he wants to review the full shape first.
- When he does want a PR, default to a draft PR unless told otherwise.

## Clean boundaries (verify before pushing a stack)
- No unrelated files in a branch. Run `git --no-pager diff --stat <base>..HEAD` and confirm
  every file belongs to this phase.
- No unintended reversions leaking in — especially do not let a branch revert files (e.g.
  one-off scripts) that another branch owns or that you have no intent to commit.
- Infra/tooling fixes (e.g. a prepush fix) are their own change, not smuggled into a feature
  stack.

## Before you commit
- Show Brian the diff before committing significant work.
