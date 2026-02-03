# xb5

Erlang library providing B-tree (order 5) replacements for `gb_trees` and `gb_sets`,
plus a multiset (`xb5_bag`) with order-statistic operations. MIT licensed, by Guilherme Andrade.

Requires OTP 27+ (for `-moduledoc` and `-doc` attributes).

## Build and test

```bash
make compile          # compile
make test             # eunit → ct → cover
make check            # erlfmt + xref + hank + lint + dialyzer
make format           # auto-format with erlfmt
make doc              # generate docs (ex_doc via rebar3_ex_doc)
```

Individual targets: `make eunit`, `make ct`, `make cover`, `make check-formatted`,
`make xref`, `make dialyzer`, `make lint`, `make find-unused-code`.

## Project layout

```
src/
  xb5_sets.erl          Public API: ordered set (like gb_sets)
  xb5_trees.erl         Public API: ordered key-value store (like gb_trees)
  xb5_bag.erl           Public API: ordered multiset with rank/percentile ops
  xb5_sets_node.erl     Internal node ops for sets
  xb5_trees_node.erl    Internal node ops for trees
  xb5_bag_node.erl      Internal node ops for bags
  xb5_structural_stats.erl  Debugging/testing stats
  xb5_utils.erl         Dialyzer utility
test/
  xb5_sets_test_SUITE.erl   Common Test suite for sets
  xb5_trees_test_SUITE.erl  Common Test suite for trees
  xb5_bag_test_SUITE.erl    Common Test suite for bags
  xb5_test_utils.erl        Test helpers (foreach_tested_size, etc.)
```

## Architecture

- B-tree of order 5: up to 4 keys per node, 5 children.
- Each public module (xb5_sets, xb5_trees, xb5_bag) delegates to a corresponding
  `*_node` module for internal tree operations.
- Node structures are represented as tuples using macros (INTERNAL2/3, LEAF2/3).
- Trees auto-balance on insert and delete.
- Elements ordered using Erlang term order (`==`, not `=:=`).
- All public types are opaque.

## Code style

- **Formatter:** erlfmt (`make format` / `make check-formatted`).
- **Linter:** Elvis via `elvis.config`. The `*_node` modules have many rule exceptions
  (god_modules, dont_repeat_yourself, nesting_level, etc.) because B-tree node
  operations inherently involve large, repetitive pattern matching.
- **Unused code:** Hank via `make find-unused-code`.
- **Dialyzer:** `make dialyzer`. `xb5_utils:dialyzer_opaque/1` exists solely to
  help Dialyzer with opaque types.

## Documentation conventions

All public functions use `-doc` and `-moduledoc` attributes (OTP 27+).

- `-doc` comes before `-spec`.
- Two styles:
  1. **Full doc:** prose description, optionally with `## Examples` section.
  2. **Equiv doc:** `#{equiv => target/N}` for aliases (e.g., `empty/0` → `new/0`).
- Show opaque types via `to_list/1`, `size/1`, etc. in examples since they can't be
  pattern-matched directly.
- **Reference implementation:** `xb5_bag.erl` is the most complete — use as style template.

## Testing

- Common Test suites with groups: `basic_api`, `smaller_and_larger`, `set_operations`,
  `iterators`, `additional_functions`, `structure`.
- Structure tests run 1000+ randomized iterations with statistical assertions
  (Z-score confidence intervals).
- `xb5_test_utils:foreach_tested_size/1` runs tests across diverse dataset sizes
  (0–200, 200–500, 500–1000).

## CI

GitHub Actions (`.github/workflows/ci.yml`): tests on OTP 27.3 and 28.3,
runs `make check` then `make test`.

## Pre-release status

- CHANGELOG.md has only an `[Unreleased]` section.
- `xb5.app.src` description is still `"FIXME"`.
- `wrap/unwrap` functions exist for cross-language interop (e.g., converting
  to/from an Elixir struct that shares the same underlying node module).
