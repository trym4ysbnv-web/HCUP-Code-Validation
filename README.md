# HCUP 2019 Rare Disease Analysis — Code Validation

This repository contains the final archived R analysis scripts plus an automated validation harness.

**No restricted HCUP data are included or required. Do not upload HCUP raw data to GitHub.**

## Automated checks

The workflow checks:
- R syntax of all archived final scripts.
- Exact ICD-10 matching.
- E83.42 exclusion from final RareMed.
- Locked mental-health PCS prefixes.
- Procedure bins: 0, 1–2, 3–5, 6+.
- Weighted calculations.
- Correct percentage calculations and 100% bin totals.
- NIS age boundaries: 18–64.
- KID hospital-type classification.
- MH DRG definition: 880–887.

The archived scripts are in `inst/analysis_scripts/`.
Their SHA-256 hashes are in `validation/SHA256_MANIFEST.txt`.

A successful GitHub Actions run provides automated reproducibility testing in a fresh GitHub-hosted R environment. It is not a substitute for independent human statistical or clinical review.
