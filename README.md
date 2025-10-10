# Desert–Montane Contemporary Ecology Lab (DM-CEL)

A modern, reproducible R + GIS + AI workspace for desert and montane ecology across Eastern California
(Eureka Dunes, Owens Valley, White Mountains, Tuolumne County).

## What you get

- **R project structure** (data, scripts, reports, outputs)
- **Quarto** report template
- **GitHub Actions** CI that can (optionally) build reports and **sync to OSF**
- **OSF sync script** using the `osfr` package

## Quickstart

1. **Create a new GitHub repository** and push this scaffold.
2. **Create an OSF project** (or use an existing one).
3. **Configure OSF**:
   - In OSF, go to *Add-ons* → connect **GitHub** (optional but recommended).
   - Create two components: `01_Data` and `05_Reports` (or update names below).
4. **Set GitHub secrets** (in your repo → *Settings* → *Secrets and variables* → *Actions* → *New repository secret*):
   - `OSF_TOKEN` — your personal access token from OSF
   - `OSF_PROJECT` — the OSF project ID (e.g., `abc12`)
   - Optional: `OSF_DATA_COMPONENT` (e.g., `01_Data`) and `OSF_REPORTS_COMPONENT` (e.g., `05_Reports`)

5. **Run locally**:
   ```bash
   # Install R deps (first time)
   Rscript scripts/setup_packages.R

   # Render the Quarto report
   quarto render reports/quarto/dmcel_overview.qmd

   # Sync to OSF (uploads reports and selected outputs)
   Rscript scripts/osf_sync.R
   ```

6. **CI/CD (optional)**: On every push to `main`, GitHub Actions will
   - Set up R
   - Install needed packages
   - Render Quarto
   - Run the OSF sync script

## Customize

- Update coordinates/regions in `scripts/example_analysis.R`
- Add datasets to `data/raw/` (ignored by Git by default)
- Change which directories are uploaded in `scripts/osf_sync.R`

## Licensing

You control licensing. Consider an open license for code (MIT) and data (CC-BY or CC0).