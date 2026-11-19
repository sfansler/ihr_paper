# ihr_paper

Analysis code for "Diurnal Heart Rate Range and Heart Rate Excursions" (BLSA, `ihr` R package).
Everything here produces a figure or number in the manuscript or supplement.

## Pipeline (run from the project root)
| Order | Script | Reads | Produces |
|---|---|---|---|
| 1 | `R/reading_combining_data.R` | `data/raw/*.csv` | `data/intermediate/data_combined.rds`, `data/processed/data_clean.rds` |
| 2 | `R/get_mahe.R` | `data_clean.rds` | `data/processed/mahe.rds`, `mahe_exc.rds` |
| 3 | `R/table_1.R` | `data_clean.rds` | Sample descriptives (Data section) |
| 4 | `R/paper_example.R` | `data_clean.rds` | Figure 2 (both panels), Supp Fig 1 |
| 5 | `R/age_trends.R` | `data_clean.rds`, `mahe*.rds` | Figure 3, `data/intermediate/exc_comparison.Rdata` |
| 6 | `R/fosr.R` | `data_clean.rds`, `exc_comparison.Rdata` | Figures 4–5, Supp Figs 2–5 |
| 7 | `R/excursion_distributions.R` | `data_clean.rds`, `mahe_exc.rds` | Supp Figs 6–7 and the excursion counts/medians in the Results |
| — | `python/figure_1.py` | — | Figure 1 (conceptual; needs numpy and matplotlib) |

All figures are written to `figures/` as `fig<N>_*.png` or `supp_fig<N>_*.png`.

## Notes
- The analytic sample (750 participants: ≥3 days with ≥1296/1440 minutes) is re-applied inside each script.
- Times are stored as UTC clock time. `paper_example.R` and `age_trends.R` use UTC so resting HR
  is taken from 3:00–7:00 AM. ihr's default uses the computer's local time zone and gives the wrong window.
- Figure 2's bottom panel uses a 15-minute moving average of HR for that one participant. The
  shading, arrows and "Excursion 1–3" labels in the manuscript were added by hand.
- `mahe.rds`/`mahe_exc.rds` date from Aug 2025. Re-running `get_mahe.R` regenerates them from the current `data_clean.rds`.
