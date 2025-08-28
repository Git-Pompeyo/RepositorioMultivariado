# AGENTS.md — Project Rules for Codex

This repository is an **R project** managed with **renv** for reproducibility, includes a **local utility package** at `src/krulRutils`, and uses **Quarto** to generate lecture notes.

---

## 0) What you (Codex) should know first

- **Never assume global packages.** All R execution must respect the project's `renv` environment.
- **This repo contains a local R package** at `src/krulRutils`. Treat it like a package: roxygen docs, tests, versioning.
- **We build lecture notes with Quarto.** Prefer rendering from the command line (non-interactive).

---

## 1) Repository layout (conventions)

- `src/krulRutils/` — local R package (R code in `R/`, tests in `tests/testthat/`, docs via roxygen2).
- `data/` — small example datasets (do **not** commit large/raw private data).
- `quarto/` — Quarto sources for lecture notes (`*.qmd`).
- `docs/` — This is where the rendered Quarto pdf outputs, together with other documentation files.
- `images/` — This is where images used in the Quarto notes are stored.
- `renv.lock`, `renv/` — renv state (do not hand-edit `renv.lock`).
- `tests/` — project-level tests (if not inside the package).
- `README.md` — overview and quickstart.

If a directory is missing, propose it in a patch **before** using it.

---

## 2) R execution & dependency management

- **Always run R code via `renv`.**
  - For shell commands inside your sandbox:
    - Prefer `Rscript -e "<expr>"` and ensure packages exist via `renv`.
  - For package installation:
    - Use `renv::install("<pkg>")` (from CRAN/GitHub as appropriate).
  - When you add/remove deps:
    - Run `renv::snapshot(prompt = FALSE)` afterward to update `renv.lock`.

**Examples**
- Install and snapshot:
  - `Rscript -e 'renv::activate(); renv::install(c("tidyverse","testthat")); renv::snapshot(prompt=FALSE)'`
- Run a script:
  - `Rscript -e 'renv::activate(); source("R/prepare_data.R")'`

**Do not:**
- Modify `renv.lock` by hand.
- Suggest `install.packages()` outside of `renv`.

---

## 3) Local package: `src/krulRutils`

- Treat `src/krulRutils` as a proper R package.
- Load for development:
  - `Rscript -e 'renv::activate(); devtools::load_all("src/krulRutils")'`
- Generate docs:
  - `Rscript -e 'renv::activate(); devtools::document("src/krulRutils")'`
- Run package tests:
  - `Rscript -e 'renv::activate(); devtools::test("src/krulRutils")'`

**Coding rules for this package**
- Document every exported function with **roxygen2**.
- Include **type-stable** interfaces and examples in roxygen.
- Prefer **tidyverse** style (pipes OK), with explicit imports in `DESCRIPTION` + `NAMESPACE`.
- Add `tests/testthat/` with clear unit tests.

---

## 4) Project style & quality

- **Formatting:** use `{styler}` with tidyverse style.
  - `Rscript -e 'renv::activate(); styler::style_dir()'`
- **Linting:** use `{lintr}`; fix issues it flags.
  - `Rscript -e 'renv::activate(); lintr::lint_package("src/krulRutils")'`
- **Testing:** use `{testthat}` for both the local package and any project-level tests.
- **Error handling:** prefer explicit, informative errors (`rlang::abort()`), not silent failures.
- **Performance:** write clear code first; only optimize with evidence (profiling).

---

## 5) Quarto lecture notes

- Quarto sources live under `quarto/`.
- Build from the command line; do not assume RStudio GUI.
- Render **PDF** by default. 

**Examples**
- Render all notes:
  - `quarto render notes/`
- Render a specific file to pdf:
  - `quarto render "quarto/reports/interdependence_relationships.qmd" --to pdf`

**Code chunks in notes**
- Use reproducible examples that run under `renv`.
- Prefer tidyverse idioms; keep chunks minimal and pedagogical.
- If a chunk needs special packages, list them explicitly.

---

## 6) Data handling

- Keep **small, de-identified sample data** in `data/` for examples.
- Do **not** commit large or sensitive datasets.
- If a dataset is needed for an example, create a small synthetic version.

---

## 7) What to change vs not change (patch policy)

- ✅ It’s okay to:
  - Add roxygen docs, tests, lintr/styler configs.
  - Create/modify Quarto `.qmd` files and supporting assets.
  - Introduce new functions in `src/krulRutils` with tests and docs.
- ❌ Do **not**:
  - Rewrite public APIs without a strong reason.
  - Break renv reproducibility.
  - Commit large binaries or raw confidential data.

Always propose **minimal diffs**; keep commits focused and well-titled.

---

## 8) Useful commands (for automation)

- **Install dev deps (once):**
  - `Rscript -e 'renv::activate(); renv::install(c("devtools","roxygen2","styler","lintr","testthat","rmarkdown","quarto"))'`
- **Document + Test package:**
  - `Rscript -e 'renv::activate(); devtools::document("src/krulRutils"); devtools::test("src/krulRutils")'`
- **Style + Lint repo:**
  - `Rscript -e 'renv::activate(); styler::style_dir(); lintr::lint_dir()'`
- **Render notes:**
  - `quarto render notes/`

---

## 9) How to talk to Codex (examples)

- “Summarize the structure of this project and list missing pieces (tests, docs, lint config).”
- “Add roxygen2 docs to all exported functions in `src/krulRutils/R/`.”
- “Write `tests/testthat/test-<name>.R` covering edge cases for `<function>`.”
- “Run lintr and propose a patch to fix issues.”
- “Render all Quarto notes; if rendering fails, explain and propose fixes.”
- “Add a small synthetic dataset to `data/` for the lecture on PCA and update the corresponding `.qmd`.”

---

## 10) CI/CD (if applicable)

If a CI config exists (e.g., GitHub Actions):
- Use `Rscript -e 'renv::restore()'` in setup steps.
- Cache `renv` library between runs.
- Run `devtools::check("src/krulRutils")` and render Quarto as part of the pipeline if feasible.

---

## 11) Security & privacy

- Do **not** fetch remote data or call external APIs unless explicitly requested.
- Assume **no network access** during automated runs.
- Strip secrets from examples; never write tokens to disk.

---

## 12) Deliverables expectations

- Clear, minimal diffs.
- Passing tests.
- Updated docs (roxygen and README).
- Reproducible Quarto builds under `renv`.


