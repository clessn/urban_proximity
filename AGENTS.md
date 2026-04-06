# urban_proximity — Project State (2026-04-05)

## What This Project Is
**Research question:** Does geographic proximity to Indian Reserves affect non-Indigenous Canadians' perceptions of Indigenous peoples? Specifically: do urban Canadians living near *prosperous* (high-CWB) reserves develop a distorted perception of Indigenous privilege?

**Repo:** `clessn/urban_proximity`
**Authors:** Laurence-Olivier M. Foisy + Camille Pelletier (capel201)
**Local path:** `/home/ral/.openclaw/workspace/urban_proximity/`
**Data path:** `data/ces2021.rds` (raw CES 2021, never modified)

---

## Where We Left Off (2026-04-05)

### ✅ Completed Today

**Step 1 — CES 2021 DV: Perception Scale**
- 5 items, all rescaled [0,1], higher = more negative perception:
  - `therm_neg` = (100 - raw) / 100 → reversed thermometer
  - `groupdiscrim_neg` = (x-1)/4 → less perceived discrimination (as-is)
  - `ab_favors_neg` = (x-1)/4 → "bootstrap" resentment (as-is)
  - `ab_deserve_neg` = ((6-x)-1)/4 → reversed (sympathy item)
  - `ab_col_neg` = ((6-x)-1)/4 → reversed (structural attribution item)
- Factor analysis: **α = 0.847, λ₁ = 3.115** → unidimensional, all loadings >0.59
- Three sub-scales: `dv_affect` (thermometer only), `dv_discrim` (3 perception items), `dv_resentment` (3 resentment items), `dv_perception` (all 5)
- Script: `R/03_scale_perception.R`

**Step 2 — Analysis**
- Key finding (2-part):
  1. **Urban → more positive overall** (β ≈ −0.05 on resentment/discrimination, clustered SEs)
  2. **Urban × proximity to high-CWB reserve → more negative perception** (β_interaction ≈ −0.018, p<0.01)
  3. Effect strongest on discrimination perception and resentment, weakest on thermometer → cognitive distortion mechanism, not pure affect
- Main proximity variable: `log_dist_cwb_q4_km` (log distance to nearest top-quartile CWB reserve)
- All models: OLS with clustered SEs by `feduid` (federal riding), province FE, controls: age, female, educ, born_canada
- Script: `R/04_analysis_and_viz.R`

**Step 3 — Figures (AJPS black-and-white style)**
- `output/figures/` — all as PDF + PNG
- Fig 1: Urban effect coefficient plot (3 DVs)
- Fig 2a–c: Marginal prediction lines (urban vs non-urban × distance to high-CWB)
- Fig 3: Interaction coefficients across all 4 DVs
- Fig 4: Choropleth — mean negative perception by riding (+ urban insets)
- Fig 5: Choropleth — distance to high-CWB reserve by riding
- Fig 6: Reserves colored by CWB index over riding boundaries (+ urban insets)
- Fig 7: Scale item distributions
- Fig 8: Factor loadings bar chart
- Scripts: `R/05_maps.R` (basic maps), `R/06_maps_enhanced.R` (insets + reserves)

**Map technical notes (important for future debugging):**
- **NEVER use `st_crop()` on SC Lambert EPSG:3347** — it fails silently (returns 0 features or throws NA coordinate error). Use `st_filter()` with `st_buffer()` instead.
- **Always `sf_use_s2(FALSE)`** before any StatCan shapefile operations — topology errors in their polygons
- Ottawa-Gatineau CMA is split into 2 rows (ON + QC) in the CMA shapefile — must `group_by(CMAUID) %>% summarise(geometry = st_union(geometry))` before iterating
- Territory exclusion: `filter(pruid < 60)` drops Yukon (60), NWT (61), Nunavut (62)

**Camille's contributions (pushed before us, pulled + rebased):**
- `pub/main.tex` — full article skeleton + abstract already written
- `output/model_summary_revised.csv/.txt` — revised model summaries
- `output/interaction_diagnostics.csv`
- `R/06_interaction_diagnostics.R`, `R/generate_model_summary.R`

---

## Step 4 — What's Left

### Immediate next steps
1. **Read Camille's LaTeX article** (`pub/main.tex`) and align figures/results with her structure
2. **Regression tables** — need formatted tables (probably `modelsummary` or `stargazer`) for the Data & Methods / Results sections
3. **Province-level robustness** — consider whether province FE is over-controlling (discuss with Laurence)
4. **Three-way interaction plot** — Fig for the urban × proximity × CWB three-way (Model E)
5. **Clustered SE note** — add to figure captions consistently

### Article structure (from Camille's tex)
- Introduction → paradox: urban positivity + proximity distortion
- Theoretical framework → contact theory + group position + reference point / subtyping
- Canadian context → reserve proximity to cities + CWB asymmetry (3.3× finding)
- Data & Methods → CES 2021, scale (α=0.847), proximity measures, OLS clustered
- Results → 3 findings (urban positive, interaction, three-way)
- Discussion → Denis (2015) subtyping connection, policy implications
- Conclusion → extend contact theory re: representativeness

---

## Key Variable Reference

| Variable | Source | Description |
|---|---|---|
| `feduid` | CES | Federal riding ID (join key) |
| `log_dist_cwb_q4_km` | proximity_fed_enhanced.csv | Log dist to nearest top-quartile CWB reserve |
| `log_dist_nearest_km` | proximity_fed_enhanced.csv | Log dist to nearest reserve (any) |
| `log_reserves_100km` | proximity_fed_enhanced.csv | Log count of reserves within 100km |
| `nearest_reserve_cwb` | proximity_fed_enhanced.csv | CWB score of nearest reserve |
| `urban_binary` | CES pes21_rural_urban | 1 = suburb/large city (4-5), 0 = rural/small town (1-3) |
| `dv_perception` | CES (5 items) | Full scale [0,1], higher = more negative |
| `dv_discrim` | CES (3 items) | Discrimination denial sub-scale |
| `dv_resentment` | CES (3 items) | Resentment sub-scale |
| `dv_affect` | CES (1 item) | Thermometer only |

## R Library Path
Always add at top of scripts: `.libPaths(c('/home/ral/R/library', .libPaths()))`
Required packages: `dplyr`, `tidyr`, `haven`, `ggplot2`, `patchwork`, `sf`, `lmtest`, `sandwich`, `broom`, `fixest`, `psych`, `clessnverse` (contains `sondr`)
