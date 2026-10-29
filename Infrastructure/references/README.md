# Reference Store

This folder is the canonical store for **reusable knowledge** that should survive across sessions, projects, and contributors. It is organized by **reference category**, with one subdirectory per category.

## Current Categories

| Category | Path | Purpose |
|---|---|---|
| Literature | [`literature/`](literature/) | Papers, technical notes, data dictionaries, codebooks, and agent-generated summaries that inform analysis. |

Additional categories (for example `datasets/`, `policies/`, `methodology/`) can be added as siblings under this folder. See **Adding a New Category** below.

## Layout Pattern

Every category folder follows the same shape:

```
references/
└── <category>/
    ├── README.md         # category-specific purpose, conventions, validation
    ├── catalog.yaml      # machine-readable index used by agents for discovery
    └── <subfolders>/     # category-specific content folders (e.g., papers/, summaries/)
```

This shape exists so that:

1. **Agents discover content by category.** A skill or sub-agent that handles literature reads `literature/catalog.yaml`; a future "datasets" skill would read `datasets/catalog.yaml`. No agent needs to scan the whole tree.
2. **Categories evolve independently.** Each category owns its own catalog schema, validation rules, and subfolder conventions. Adding `datasets/` does not change anything about `literature/`.
3. **Validators are category-agnostic.** The catalog validators in [`Infrastructure/scripts/`](../scripts/) accept a `--catalog` flag, so the same scripts work against any category catalog that follows the same YAML schema.

## Operating Rule

When adding content, always follow the category's own playbook. For literature, see [`Infrastructure/commands/literature-intake.md`](../commands/literature-intake.md) and [`literature/README.md`](literature/README.md).

## Validation

Each category's catalog should be validated after edits. For the literature catalog:

```bash
python3 Infrastructure/scripts/validate_literature_catalog.py
python3 Infrastructure/scripts/check_catalog_staleness.py
```

`make maintenance-check` runs these as part of the routine hygiene pass.

## Adding a New Category

To add a new reference category (for example `datasets/`):

1. **Create the folder structure** following the layout pattern above:
   ```bash
   mkdir -p Infrastructure/references/datasets/{papers,summaries}
   touch Infrastructure/references/datasets/papers/.gitkeep
   touch Infrastructure/references/datasets/summaries/.gitkeep
   ```
2. **Add a category README** at `references/datasets/README.md` describing the category's purpose, conventions for what belongs in each subfolder, and any required catalog fields beyond the base schema.
3. **Add a catalog stub** at `references/datasets/catalog.yaml`:
   ```yaml
   version: 1
   last_updated: YYYY-MM-DD
   entries: []
   ```
4. **Register the category in this README's table** (above) so future contributors discover it.
5. **Wire validation.** If the new catalog follows the same schema as the literature catalog, point the existing validators at it via `--catalog`. If the schema diverges, add a category-specific validator under `Infrastructure/scripts/` and add a Makefile target so the new check runs as part of `maintenance-check`.
6. **Add an intake playbook** (optional but recommended) at `Infrastructure/commands/<category>-intake.md` so agents know how to add new entries to the catalog consistently.

This pattern keeps the reference store extensible without changing how existing categories work or how agents discover content.
