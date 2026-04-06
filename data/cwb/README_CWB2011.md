# CWB 2011 — Manual Download Required

The Statistics Canada server is not accessible from the research server.
To obtain the 2011 CWB data (needed to impute values for Kahnawake, Kanesatake, and
other incompletely-enumerated reserves that were counted in 2011):

## Download steps (2 minutes)

1. Open this URL in your browser:
   https://www150.statcan.gc.ca/n1/pub/89-645-x/2015001/tbl/tbl-eng.htm

2. Alternative: search "Community Well-Being Index 2011" on:
   https://www150.statcan.gc.ca/n1/

3. Save the file as: data/cwb/CWB_2011.csv

4. Re-run this script: Rscript R/02d_proximity_expanded.R

## What this fixes
- Kahnawake (~7,000 on-reserve, CWB ~75 in 2011, near Montreal)
- Kanesatake (~2,000, CWB ~65 in 2011, near Montreal)
- Akwesasne (~10,000, CWB ~65 in 2011, near Ontario/QC border)
- Listuguj (~2,500, CWB ~60 in 2011, Gaspésie)
- ~10 other communities

## Expected impact
Without 2011 imputation: 577/1002 communities have CWB (57.6%)
With 2011 imputation: ~591/1002 estimated (59%)
The big QC communities are the key gain.

