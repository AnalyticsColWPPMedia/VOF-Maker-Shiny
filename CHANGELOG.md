# VOF Maker — Changelog

## Phase 3g, revision 2 — Dummy conflicts: block only identical dummies (2026-11-11)
- Generation is blocked only when two dummies switch on in exactly the same periods and entities.
- Any other overlap (event weeks in common, even every year, or a ramp / decay running into
  another dummy) is a warning that lists the shared weeks, e.g. "event periods coincide in
  2026-06-14". Season windows stay informative.

## Phase 3h — Visible messages and metadata download everywhere (2026-11-08)
- Messages (new `R/modules/mod_messages.R`, `app_notify()` used by every tab):
  - errors open a modal in the middle of the screen that has to be closed ("OK");
  - warnings: orange card at the top centre that stays until it is closed;
  - confirmations: blue card at the top centre that closes after a few seconds;
  - progress bars also at the top centre. All cards are larger and coloured.
- "Download VOF Metadata" (new `R/modules/mod_metadata_download.R`) next to the Generate
  button of Media, KPI, External Sources and Dummies, and in Data Management. All the buttons
  download the whole history and show how many VOFs are new or changed since the last download
  ("2 new"). Metadata loaded from a file or from this browser does not count as new.
- Leaving or reloading the page with VOFs not downloaded: the browser asks first.
- Generated code boxes show `<-` as typed (no font ligatures).

## Phase 3g — Dummy conflicts (2026-11-08)
New `R/core/15_dummy_conflicts.R`. Every pending dummy is compared with the other pending dummies
and with the dummies saved in the metadata, on the column it produces in the Analytical (periods x
entities): dates on different days of the same week collide, different CS scopes do not.
- **Blocked**: the event periods coincide in every year both dummies have, or both switch on in
  exactly the same rows (e.g. Thanksgiving Day / Black Friday in weekly data). "Generate" stops
  and asks to remove one of them.
- **Warning**: the event periods coincide only in some years (the years are listed, e.g.
  Christmas Eve / Christmas Day in 2024, 2025), or a ramp / decay runs into another dummy (the
  weeks are listed). Generation is allowed.
- **Info**: a Season window contains another dummy (usual: the holiday models the peak on top of
  the season), unless both columns are identical (blocked).
- New "Conflicts" column in "Dummies to generate" (red / orange / blue), updated as dummies are
  added or edited, and a note above the table with the blocked pairs.
- A saved dummy with the name of a pending one is being replaced, so they are not compared.

## Phase 3f, revision 2 — Metric suffix spacing and CS Groups tidy-up (2026-11-08)
- The metric suffix has a space before it, to keep it apart from the tags:
  `Channel --p 20250101- ---Imps` (was `Channel --p 20250101----Imps`), `TV --g Hub ---Spend`.
  Applies to new VOFs; older names without the space are still read everywhere (Restore,
  Legacy, Purify, QA twins / sisters, metric and flag detection).
- CS Groups: the All Others name box only appears when some entities are left out of the
  groups (it shows how many); a hidden name is ignored.
- "Filter writing" (write the shortest list) moved from the CS section to VOF Settings.

## Phase 3f — Custom CS group names (2026-11-08)
- Optional name for every CS group: each CS Group (and All Others) and, without sisters, each
  dimension filter of a Component Row ("Group name (optional)", next to Include / Exclude).
  `--g MyGroup` replaces the automatic flag and goes to `<Dim>Group`; the filter itself
  (`<Dim>`, `<Dim>Filter`, code) does not change. Empty = automatic flag as before.
- Validation: special characters become spaces, extra spaces are removed, the case is kept;
  `topN`, `groupN` and names starting with "not" are reserved; a name used by two groups of the
  same configuration, or by rows that give different names to one dimension, is blocked.
- A name that already names a different set of entities in the metadata blocks the generation;
  another name for the same set is allowed (with a note). Exact lists of the same set reuse
  the custom name (as any flag).
- Restore: custom flags of the name go back to the "Group name" box of the rows, so
  regenerating keeps them; automatic flags are recomputed.
- TimeBreaks keep their dates (not customizable).

## Phase 3e, revision 5 — Pattern flags always win (2026-11-08)
- A CS filter written with patterns (CS Children: starts with / contains / ends with) is always
  named by its pattern (`--g Mobile Ews And Uis`, `--g not Mobile Ews And Uis`). Before, if the
  history already had the same set of entities with another flag (e.g. an exact list named
  "Healthcare - Mobile Ews And Uis etc"), that flag was reused. Exact lists still reuse the flag
  of the same set, and a flag that names a different set still becomes groupK.

## Phase 3e, revision 4 — Sister names follow the CS flag rules (2026-11-07)
- The All Others sister is named with the same rules as any CS filter (no more `--g All Others`):
  `--g not San` (excluded pattern), `--g not San-Las-Los` (several excluded patterns),
  `--g not Atlanta etc` (excluded list), `--g topX` when the entities left are the X largest,
  and `--g groupK` when the flag already names a different set in the history (the same set
  reuses its flag). "All Others" stays only as the label of the checkbox / preview line.
- The All Others conflict check was removed: the groupK rule covers it.

## Phase 3e, revision 3 — All Others only with residual entities (2026-11-07)
- All Others is offered only when some entities are left out of the groups. When the groups
  cover every entity it is not shown (checkboxes, rows, preview) and the app says
  "Every <dim> is already in a group: no All Others VOF."

## Phase 3e, revision 2 — CS Groups with CS Children; optional TimeBreaks ends (2026-11-07)
- CS Groups are defined like the CS filter of a Component Row: Include / Exclude, and with CS
  Children enabled one picker per child (starts with / contains / ends with patterns, exact list
  when the selection cannot be written as patterns). Names use the usual flags (`--g San`,
  `--g Las-Los`).
- All Others keeps the patterns: one group -> the same filter with Include / Exclude switched;
  groups of the same kind -> one Exclude with all their values; otherwise the exact list of the
  grouped entities (with a note). The summary shows the code of every group.
- TimeBreaks: start and end are hidden by default; "Start date" and/or "End date" checkboxes
  close TimeBreak 1 / the last TimeBreak.
- QA sister check: without the split, the period is the window covered by the sisters (closed
  only when every sister is closed on that side), so an end date is not reported as a gap.

## Phase 3e — Sister VOFs: TimeBreaks and CS Groups (2026-11-07)
One configuration of the Media tab now generates a set of sister VOFs (new `R/core/14_splits.R`).
- **TimeBreaks** (Configuration Options, with Period): number of TimeBreaks, cut dates and an
  optional start / end. A cut date is the last day of the TimeBreak on its left; the next one
  starts the day after. Any calendar date works, so there are no gaps between TimeBreaks.
  Names keep open ends: `--p -20240630`, `--p 20240701-20250630`, `--p 20250701-`. The block
  shows the periods of the Analytical in each TimeBreak.
- **CS Groups** (Configuration Options, with Cross Sectional): one dimension, groups 1..n of
  entities (exact Include) and **All Others** (Exclude of every grouped entity, `--g All Others`).
  Other dimensions keep the filters of each row.
- Both can be combined: every combination is generated (e.g. 2 TimeBreaks x 2 groups = 4
  sisters, each with its twin). "Generate" checkboxes choose which TimeBreaks / groups to create.
- Every Component Row shows the TimeBreaks / CS Groups it follows. "Custom dates" / "Custom
  filter" give the row its own dates / filter (exclusive with the split): then no sisters are
  created, the other rows use the first selected part, and the app says so. "Use TimeBreaks" /
  "Use CS Groups" bring the row back.
- Backcasting applies only to the oldest TimeBreak (only that name gets "Backcasted"); the
  calibration window must be inside it.
- An "All Others" VOF already in the history with a different set of entities blocks the
  generation (it is not replaced silently).
- The live name preview lists every sister and its twin.
- QA: new check "Sister VOFs add up to the roots": VOFs with the same name except the period /
  CS tags and the same metric must add up to their roots without the split (reports the share
  covered when a sister is missing or they overlap).
- Restore keeps loading VOFs one by one (with their own dates and filters).
- Rows are inserted immediately in the page (fixes rows whose dates showed/hid late).

## Phase 3d, revision 4 — Live VOF name preview (2026-11-07)
- Media tab: bar fixed at the bottom of the screen with the name the VOF will get (and its
  twin), updated while rows are added and configured (VariableName, Complement, Effect, period,
  CS flags, backcasting, metric). Nothing is saved until "Generate".
- It says what is missing (e.g. "Row 2: VariableName is empty"), when a name is already in the
  history (generating replaces it) and when the activity twin will be chosen in the dialog
  (the preview shows the pre-selected one). Rows without a variable are ignored.
- Core: `preview_media_vof()` (04_media_codegen.R) reuses the generation rules, so the preview
  and the generated name are always the same.

## Phase 3d, revision 3 — Pre-filled VariableName: special characters and Pmax (2026-11-07)
- Special characters of the root (- _ ( ) , . / & + ...) become spaces; apostrophes are dropped
  ("Search (Brand)" -> "Search Brand", "Macy's" -> "Macys").
- Abbreviations match any separator between words and run before the cleaning:
  "ONLINE VIDEO-CTV" / "OLV_CTV" -> "CTV"; "P-MAX" / "PMAX" / "Performance Max" -> "Pmax"
  (one word, first letter in capitals; PMAX removed from the acronym list); "E-mail" -> "Email".
- Acronyms of the list stay in capitals whatever case they come in (OLV, CTV, OOH, ...).

## Phase 3d, revision 2 — Pre-filled VariableName casing and abbreviations (2026-11-06)
- The pre-filled VariableName is written with the first letter of each word in capitals and the
  rest in lower case ("DIRECT TV" -> "Direct TV"); acronyms stay in capitals (`NAME_ACRONYMS`),
  brands keep their casing (YouTube, TikTok, LinkedIn: `NAME_SPECIAL_CASING`), words with digits
  in capitals (GA4), and/of/the/... in lower case.
- Usual channel abbreviations (`CHANNEL_ABBREVIATIONS` in 00_constants.R): Online Video -> OLV,
  Connected TV / Connected Video -> CTV, Advanced TV -> ATV, Over The Top -> OTT,
  (Digital) Out of Home -> (D)OOH, (B/A/S)VOD, Television -> TV, SEM, SEO, PPC, CRM, PR, POS, DTC.
  Whole words, any case, spaces or hyphens between words.
- "Metric dictionary" is now "Naming dictionary" and also shows the abbreviations and acronyms.
- Only the pre-fill changes: names typed by the analyst are kept as written.

## Phase 3d — Media tab details (2026-11-06)
- VariableName is pre-filled with the root of the selected variable (VariableName part of the
  MFF name without the metric: "Display Impressions_Watches_..." -> "Display"). When the rows
  have different roots that start with the same words, every row gets "Total <shared words>"
  (Social Video + Social Display -> "Total Social"); otherwise each row keeps its root. The app
  only rewrites names it filled itself: a name typed by the analyst, or restored, is kept.
- VOF names have no leading, trailing or repeated spaces (VariableName, Complement, Effect,
  media / external names).
- A variable selected in one Component Row is not offered in the other rows.
- "Show variables" filter in the Variable Builder: Activity (default) / Spend / All, by the
  metric dictionary; variables without a known metric are listed under All.
- Metric detection is vectorised (`detect_metrics()`): classifying all variables takes
  milliseconds.

## Phase 3c, revision 2 — Pattern detection (2026-11-06)
New methodology in `R/core/13_pattern_detection.R` (replaces the STL peak detection of 12_dummies.R).
- Effects are the deviation of the KPI from a robust trend (13-period running median):
  relative (% of trend, default for KPIs >= 0) or additive (difference / median level, for
  KPIs that can be negative or do not move proportionally). Selector in the app.
- Holidays: event study around every calendar date, aligned on the real date of each year
  (moving holidays included). Effect week (event or +-1 week), lift, consistency across years,
  suggested ramp/decay from the median profile. Holidays that hit the same weeks (Christmas Eve /
  Christmas Day) are merged ("Same weeks as"). Chart per holiday: one line per year + median.
- Seasonality by windows: weeks of the year with a consistent deviation across years
  (+-1 week tolerance), grouped into windows such as "November to December Peak" or
  "January Drop". Heatmap week of the year x year; table says which holidays fall inside.
- Sub-annual patterns (after removing the yearly profile): month turn, first/last full week of
  the month, 4-4-5 quarter week and the strongest periodicity of the autocorrelation
  ("Every 6 Weeks Drop").
- Atypical: single-year points out of range (robust z) outside holidays and windows found.
- Overview chart: KPI, trend, shaded holiday / season weeks and atypical points.
- Entities: on demand, every pattern found on the selected series measured in each entity
  (table with entities with / without the effect + entity x pattern heatmap). "Add only for the
  entities with the effect" creates the dummy with that Geography (CS) scope. The series selector
  keeps the total and every single entity.
- Thresholds: minimum effect (%), minimum consistency, atypical z. Rows that pass are
  pre-selected; "Add selected to the builder" in every tab.

## Phase 3c — Dummies (2026-11-06)
New "Dummies" tab (`R/core/12_dummies.R`, `R/modules/mod_dummies_tab.R`).
- KPI & market: any variable of the Analytical (MFF or VOF). The log scale is detected from the
  name and the values and confirmed by the analyst; peaks are always searched in levels (exp).
  Holiday calendar of the market (27 markets, 2015-2035, `data/holiday_calendar.csv`: public
  holidays + commercial dates by country) plus optional extra dates (csv: Date, Name).
- Peak detection on the total of all entities or one entity, or a scan of every entity:
  STL (2+ years) residual peaks/drops with a robust z-score threshold, plus strong points of the
  yearly pattern (regular peaks such as Christmas, absorbed by the seasonality). Classes:
  Holiday / special date, Yearly seasonality, Recurring (not a known date), Atypical. Suggested
  names follow the naming convention. Max-suggestions cap and dummy counter to avoid overfitting.
- Builder: dummies from suggestions, the calendar (all years), custom dates or periods, or an
  events file (template provided). Editor per dummy: name, category (Holiday, Season, Business,
  Macro, Dummy), ramp before / decay after (linear, exponential or custom weights) with a
  preview of the weights, and CS scope (include/exclude entities).
- Code in the team format (`Data[,"X"] = 0` + one `Data[which(...), "X"] = w` line per period);
  metadata Type "Dummy" (category in VofVariableName, event in VofComplement), reviewed in QA.
- `tools/build_holiday_calendar.py` regenerates the calendar (Python `holidays` package).

## Phase 3b, revision 2 — QA by twins (2026-11-06)
- The QA unit is the VOF without its metric: activity and spend twins are reviewed together,
  with a check that both have the same components (labels, periods, filters; roots that differ
  only in the metric).
- Type of VOF shown as tags: Sum of variables / Date filter / Cross-sectional filter.
- Charts: one per twin. VOFs that sum variables default to stacked areas (each root's
  contribution) with the VOF line on top - the line must follow the edge of the stack; "Lines"
  view available. Period cuts drawn as vertical lines. Notice when the VOF filters entities.
  The dotted "full root" lines were removed.
- Summary: "How this VOF is built" (code runs, no NAs, same as AWB, VOF = sum of components,
  date filter respected with its cuts, entity coverage) + components table; entity table
  (kept by the filter or not, data or not, totals of each twin); status legend.
- Metadata: `<Dim>Flag` renamed to `<Dim>Group` (files with `Flag` or `_Group` are migrated).

## Phase 3b — VOF QA (2026-11-06)
New "VOF QA" tab (packages: plotly, DT). Every VOF of the history is computed on a copy of the
Analytical with the same base-R code AWB will run (media code rebuilt from the metadata), each
VOF on its own so one error does not stop the rest.
- Selection: VOF, cross-sectional entities (none = total of all) and a status badge
  (OK / Check / All zero / Differs from AWB / Error, icon + label).
- Charts: VOF vs each root variable (part that enters the VOF, solid; full root, dotted), AWB
  values when the VOF already exists in the Analytical, and the twin VOF in its own chart.
- Summary: table by entity (total, share, mean, non-zero periods, first/last non-zero, NAs and,
  per root, raw vs in-VOF totals with notes such as "roots have data but the filters leave this
  entity out"), and checks of all VOFs (click a row to open it). Tables live in fixed-height
  cards with scrollbars.
- Core: `R/core/11_qa.R`; `vof_code_blocks()` now feeds both QA and the project code download.

## Phase 3a, revision 2 — Metadata schema (2026-11-05)
- All metadata columns are CamelCase: `BackcastingStart`, `BackcastingEnd`, `<Dim>Filter`,
  `<Dim>Flag` (was `<Dim>_Group`).
- Single `Metric` column with the metric itself (Impressions, Clicks, ..., Spend; KPI, Weight,
  External for non-media). `MetricName` removed.
- `InitCode` and `FeedCode` removed from the file. Media code is rebuilt from the metadata columns
  (download "Project Code"); the code of KPI / weight / external VOFs is kept internally
  (app + browser copy) because it cannot be rebuilt from the metadata.
- Old metadata files are migrated on upload (renamed columns, metric detected from the root for
  old "Activity" rows, FeedCode of non-media rows kept as internal code).

## Phase 3a — Naming, filters and twins (2026-11-05)

### New media VOF name structure
`[Backcasted ]<VariableName and ...>[ - <Complement and ...>][ --Effect][ --p start-end][ --g flag]---<Metric>`
e.g. `Display and OLV - Watches --Direct --g top5---Imps`, `Paid Search---Clicks` / `Paid Search---Spend`.
- Several VariableNames / Complements are joined with " and "; " - " separates both parts.
- The metric is always the suffix (`---Imps`, `---Clicks`, ..., every spend synonym -> `---Spend`).
  It may not appear in the VariableName or Complement (generation is blocked).
- Components must share one metric. Unknown metrics can be chosen in VOF Settings.
- `Backcasted` is now a name prefix (no longer appended to the media channel).
- Metric dictionary (`METRICS` in 00_constants.R) visible in the app (VOF Settings > Metric dictionary).
- Applies to new VOFs only; existing names are kept (Restore/Legacy/Purify work with both).

### Twins in both directions
- Activity VOF -> spend twin (as before). Spend VOF -> activity twin: one candidate is used
  directly; several candidates open a dialog pre-selected with `CHANNEL_METRIC_RULES`
  (Search -> Clicks, Social/Display/OOH -> Impressions, OLV/Video -> Views, TV/Radio -> GRPs, ...).

### Cross-sectional filters
- Include / Exclude per dimension (`!(...)` in the code). Optional "write the shortest list".
- With CS Children the code uses positional patterns that keep the separator:
  child A -> `startsWith(x, 'Boston - ')`, middle -> `grepl(' - North - ', x, fixed = TRUE)`,
  last -> `endsWith(x, ' - Existing')`. If patterns cannot reproduce the selection exactly
  (a value in several positions), the exact list is used and the user is told.
- Each row shows what its filters keep before generating.
- Name flags: `top<N>` (selection = N largest entities of the root variables), CS Children
  patterns (`--g uis-ews`, `--g not uis`), otherwise `<first> etc` / `not <first> etc`. A flag
  already used for a different set becomes `group<K>`; the same set reuses its flag.

### Metadata
- `MediaChannel` -> `VofVariableName`, `SubChannel` -> `VofComplement` (old files are mapped).
- Per CS dimension: `<Dim>` (values; positional patterns quoted, levels separated by "; "),
  `<Dim>_Filter` (`Include|Exact`, `Exclude|StartsWith;EndsWith`, ...), `<Dim>_Group` (flag).
- `Metric` (Activity/Spend) + new `MetricName` (Impressions, Clicks, Spend, ...).
- Purify keeps the twin of every used VOF (same name without the metric suffix).

### Other
- KPI log name follows the naming convention: `<KPI> Log` (suffix).
- Title Case keeps and/of/the/... in lower case.
- Restore and Legacy import read exclusions and positional filters.

## Phase 1 + 2, revision 2 (2026-11-04)
- App code moved to the tidyverse (dplyr, purrr, stringr, tibble, readr, readxl, lubridate,
  tidyr). Only the GENERATED VOF code stays base R (AWB runs R 3.6).
- Backcasting is now a VOF-level setting ("VOF Settings" card) instead of a per-row checkbox.
  It applies to the activity and spend VOFs; an incomplete window blocks generation. Restore
  loads it back into the card.
- Business Cycle (KPI tab), three methods:
  1. Trailing 52-week cycles from the last date with KPI > 0 (default). Only complete cycles;
     optional cap on the number of cycles.
  2. Calendar years (first/last year may be incomplete; the preview flags them).
  3. Custom table (manual / CSV upload), pre-filled with calendar years.
  The generated column keeps the name `FiscalYear` for compatibility with existing projects.
- Text files (metadata, Model Details) are read as-is (no whitespace trimming).

## Phase 1 + 2 — Refactor and bug fixes (2026-11-04)

### Structure (team Shiny standard)
- `global.R` (packages + sourcing), `ui.R` / `server.R` as thin orchestrators.
- `R/core/` — pure business logic, no Shiny, numbered by dependency:
  `00_constants` · `01_helpers` · `02_analytical` · `03_media_naming` · `04_media_codegen` ·
  `05_kpi_codegen` · `06_external_codegen` · `07_metadata` · `08_code_parsers` · `09_cs_children`.
- `R/modules/` — one module per area: `mod_data_upload`, `mod_project_actions`, `mod_media_tab`,
  `mod_variable_row`, `mod_kpi_tab`, `mod_external_tab`, `mod_history` (metadata store).
- Removed duplicated definitions (module, helpers) and the stale `mod_variable_row.R`.
- Inline styles moved to `www/styles.css`.
- `tests/testthat` — unit tests of the core (`Rscript tests/run_tests.R`).

### Bug fixes
- Variable selector offers only **MFF variables** (name = VariableName + one token per longitudinal
  dimension) instead of the `_Total` rule; VOFs can no longer be nested.
- Analytical as CSV: column names are no longer mangled (`check.names = FALSE`).
- Purify: the "missing VOFs" warning never showed (regex ended in `|`).
- Restoring browser history then generating crashed (Date vs character columns). All metadata is
  now character with ISO dates; old files with `m/d/Y` dates are normalised.
- Browser history is stored per project (signature of the Analytical columns), not globally.
- External Sources: daily data is assigned to the week that **contains** each date (was: nearest
  week, spilling half of each week into the next). External VOFs are now saved to the metadata.
- Backcasting: generated code no longer fails when the calibration window has no data; restored
  from code and from metadata.
- Quotes/backslashes in values and names are escaped in generated code.
- `Type` column was dropped for media rows.
- Removed rows destroyed their observers; delete observers are no longer re-registered.
- Toggling any configuration checkbox no longer resets CS selections of existing rows.
- Restore Project loaded rows under the wrong module namespace (CS pickers did not render).
- CS Children: a space is a valid separator; restored rows derive the children selection.
- Auto-Spend: keyword match is whole-word in the VariableName part ("Actions" ≠ "Transactions");
  components without a spend pair are reported.
- Trailing cycles had 53 weekly periods each; now exactly 52 (any week-start day).
- Re-generating a VOF (media, KPI, external) replaces its previous rows instead of duplicating them.

### Behaviour changes
- Old VOF Metadata is merged into the history on upload (only VOFs not already there).
- Restore / Legacy parsers read R's parse tree instead of regexes (multi-line statements, commas,
  parentheses and quotes inside values). Negated filters are reported, not misread as inclusions.
- Project actions (Restore, Legacy, Purify, downloads, Reset) moved to the Data Management card.
- New download: full project code (.R) rebuilt from the metadata.
