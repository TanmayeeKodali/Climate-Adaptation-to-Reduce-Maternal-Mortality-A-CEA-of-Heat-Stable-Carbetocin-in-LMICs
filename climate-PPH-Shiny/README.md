# Climate CEA Dashboard: Heat-Stable Carbetocin vs Oxytocin for PPH in LMICs

Interactive Shiny dashboard exploring how **climate change and cold-chain fragility** affect oxytocin performance for postpartum hemorrhage (PPH) prevention, and the **cost-effectiveness of heat-stable carbetocin (HSC)** as an alternative in low- and middle-income countries (LMICs).[][][]

**Live app:**  
  https://kzja3g-tanmayee-kodali.shinyapps.io/climate-pph-dashboard/
  
  ---
  
  ## 1. Scientific context
  
  - PPH is a leading cause of maternal mortality worldwide, with the burden concentrated in LMICs.[]
- Oxytocin is the guideline-recommended first-line uterotonic for PPH prevention and treatment, but its potency is highly temperature-sensitive and requires a reliable cold chain (2–8°C).[][]
- Heat-stable carbetocin (HSC) is an oxytocin analogue that retains ≥95% purity at 30°C for extended periods and has been developed specifically for hot, resource-limited settings.[][]
- WHO and other global actors have recognized HSC as a suitable option for PPH prevention where cold-chain infrastructure is weak, while oxytocin remains standard for treatment and labour induction/augmentation.[][]

This dashboard operationalizes a **decision-analytic cost-effectiveness framework** to ask:
  
  > How many women are at risk of receiving degraded oxytocin under climate change, and when does switching to HSC become cost-effective across LMICs?
  
  ---
  
  ## 2. Overview of the app
  
  The dashboard is implemented as an R Shiny application with a standard `global.R`, `server.R`, and UI layout. Key visualizations include:
  
  1. **Viz 1 – Choropleth: Projected Temperature Warming**
  - Uses CMIP6 SSP2‑4.5 projections to show warming vs a 1995–2014 baseline for LMICs.[]
- Two panels:
  - 2020–2039 (near term)
- 2040–2059 (mid-century)
- Color scale emphasizes policy-relevant thresholds around 1.5°C and 2.0°C warming.

2. **Viz 2 – Bubble Chart: Women at Risk of Inadequately Treated PPH**
  - Scatter/bubble plot where:
  - X-axis: projected mean temperature (by period).
- Y-axis: estimated number of women **receiving a clinically failed oxytocin dose** (heat‑compromised).
- Bubble size: projected births.
- Bubble color: region (Sub-Saharan Africa, South Asia, East Asia & Pacific, Latin America & Caribbean, MENA+Afghanistan+Pakistan, Europe & Central Asia), using a named `region_colors` palette from `global.R`.
- Two panels toggled by buttons:
  - Panel 1: 2020–2039
- Panel 2: 2040–2059
- Hover tooltip decomposes:
  - Women at risk of PPH needing oxytocin.
- Estimated women exposed to degraded oxytocin (W_risk).
- Warming, facility births, cold-chain failure probability, Arrhenius-based degradation probability, and births in each period.

3. **Viz 3 – ICER Threshold-Crossing Year Map**
  - Choropleth showing, for each country:
  - Dominant (HSC cheaper and more effective).
- Cost-effective by 2035.
- Cost-effective by 2059.
- Not cost-effective (at a $100/DALY threshold).
- Based on cumulative incremental cost and DALYs averted from a decision-tree cost-effectiveness analysis.[][]
- Uses one choropleth trace per category with fixed colors and dummy scatter traces to produce a clean categorical legend.

Additional sections in the dashboard provide explanatory callouts on how each map/chart is constructed and how to interpret dominant vs cost-effective vs non-cost-effective classifications.

---
  
  ## 3. Data & model structure
  
  ### 3.1 Data sources
  
  - **Climate data:** CMIP6 SSP2‑4.5 projections and baseline temperature for LMICs (processed to country-level mean warming by period).[]
- **Epidemiologic inputs:** PPH incidence, proportion of births in facilities, and oxytocin utilization, informed by LMIC literature (e.g., WHO reports and maternal health studies).[][]
- **Drug characteristics:**
  - Oxytocin: temperature-sensitive, requires cold chain (2–8°C), potency degrades at higher temperatures.[][][]
- Heat-stable carbetocin: stable up to 30°C, non-inferior to oxytocin for prevention in large trials, suitable where cold chain is unreliable.[][][]
- **Cost and DALY inputs:** Unit costs, cold-chain costs, syringe/administration, disability weights, and discount rates drawn from trial-based and modelling CEA literature.[][][]

(Exact datasets are pre-processed and loaded in `global.R` / data scripts.)

### 3.2 Core model logic

At high level:
  
  - **Temperature–degradation relationship:**  
  Oxytocin degradation probability \(D(c,t)\) is modelled using an Arrhenius-type relationship anchored to lab stability data, combined with location-specific warming and baseline temperature.
- **Cold chain failure:**  
  A baseline regional cold-chain failure probability \(P_{CCF,reg}\) is scaled by hazard ratios per °C warming, producing \(P_{CCF}(c,t)\) over time.
- **Women at risk of degraded oxytocin (W_risk):**  
  For each country-year, women exposed to clinically failed oxytocin are approximated by:  
  \( W_{risk}(c,t) = B_t \times PF(c,t) \times P_{PPH} \times P_{oxy} \times P_{CCF}(c,t) \times D(c,t) \)  
where:
  - \(B_t\): births
- \(PF(c,t)\): facility delivery rate
- \(P_{PPH}\): incidence of PPH
- \(P_{oxy}\): proportion treated with oxytocin
- \(P_{CCF}(c,t)\): cold-chain failure probability
- \(D(c,t)\): temperature-driven degradation probability

- **Cost-effectiveness model:**
  - A decision-tree compares oxytocin vs HSC for a large cohort of births, capturing:
  - Drug and delivery costs.
- Cold-chain and logistics costs for oxytocin vs non-cold-chain HSC.
- PPH events, treatment patterns, and health outcomes.
- Incremental cost, incremental DALYs, and ICER are accumulated over time and discounted.
- Countries are classified by the first year in which the ICER drops below $100 per DALY averted or by dominance if HSC is cheaper and more effective overall.[][][]

---
  
  ## 4. Code structure
  
  The app follows a standard Shiny structure:
  
  - `global.R`
- Loads required packages (e.g., `shiny`, `plotly`, `dplyr`, `readr`, etc.).
- Loads and pre-processes climate, epidemiologic, and cost-effectiveness datasets.
- Defines **regional color palette** `region_colors`:
  
  ```r
region_colors <- c(
  "Sub-Saharan Africa"                                = "#27ae60",
  "South Asia"                                        = "#e74c3c",
  "East Asia & Pacific"                               = "#1abc9c",
  "Latin America & Caribbean"                         = "#8e44ad",
  "Middle East, North Africa, Afghanistan & Pakistan" = "#f39c12",
  "Europe & Central Asia"                             = "#2980b9"
)
```

- Pre-computes objects like `warming_near`, `warming_mid`, `plot_p1`, `plot_p2`, `threshold_map` used by the server to build visuals.

- `server.R`
- `output$choropleth`: builds the two-panel warming map with a custom colorscale and button toggles for periods.
- `output$bubble_chart`: builds the women-at-risk bubble chart with:
  - Traces split by region for Panel 1 and Panel 2.
- Legend driven by Panel 1 traces, with `legendgroup` linking panels.
- `updatemenus` buttons that toggle visibility of P1 vs P2 traces while updating axis titles.
- `output$viz3_map`: builds the ICER threshold-crossing map with:
  - One choropleth trace per ICER category.
- Dummy scatter traces to produce a simple categorical legend.
- Rich hover text explaining ICER, DALYs, and cost-effectiveness status.

- `ui.R` (or the app UI file)
- Defines the layout of the dashboard (e.g., sidebar + main panel or multi-tab layout).
- For Viz 2, it includes:
  - A section header.
- An explanatory “How to read this chart” callout.
- An optional static color legend row showing region–color mapping.
- `plotlyOutput("bubble_chart", ...)`.

---
  
  ## 5. Recent refinements
  
  Several iterations and bug fixes were applied to make the dashboard robust and interpretable:
  
  - **Legend clarity for Viz 2 (women-at-risk bubble chart):**
  - Moved away from dummy legend traces, back to **trace-based legend** driven by Panel 1 traces only.
- Ensured `name = reg` and `legendgroup = reg` use region labels from `region_colors`, so legend entries match the regions exactly.
- Adjusted Plotly `updatemenus` visibility vectors to match the actual number of traces (Panel 1 + Panel 2), avoiding panel overlap and Shiny errors.

- **Tooltip enrichment:**
  - Viz 2 tooltips now show both:
  - Women at risk of PPH needing oxytocin.
- Estimated women receiving degraded oxytocin.
- Added climate and health system context: warming, facility births, cold-chain failure probability, degradation probability, and births per period.

- **ICER threshold map (Viz 3) fixes:**
  - Corrected Plotly colorscale specification to use `list(list(0, color), list(1, color))` instead of `c()`, ensuring categories render correctly.
- Suppressed unwanted continuous colorbars (`showscale = FALSE` on each choropleth trace).
- Implemented square-symbol dummy scatter traces to create a categorical legend that shows the four ICER status classes clearly.

- **Unicode and text rendering:**
  - Standardized unicode escapes (e.g., en dash `\u2013`, degree symbol `\u00b0`) across titles, subtitles, and labels so text renders correctly in the browser.
- Cleaned up escaping inside `updatemenus` to avoid malformed JSON and Shiny status‑1 errors.

---
  
  ## 6. How to run locally
  
  1. **Clone the repository:**
  
  ```bash
git clone https://github.com/your-username/your-repo-name.git
cd your-repo-name
```

2. **Open in R / RStudio:**
  - Ensure R version and package dependencies are installed (`shiny`, `plotly`, `dplyr`, `readr`, etc.).
- Install any missing packages:
  
  ```r
install.packages(c("shiny", "plotly", "dplyr", "readr"))
```

3. **Run the app:**
  
  ```r
library(shiny)
runApp(".")
```

4. Navigate to the local URL shown in the R console (usually `http://127.0.0.1:xxxx/`).

---
  
  ## 7. Reproducibility & limitations
  
  - The model uses **aggregated LMIC region-level and country-level inputs** and climate projections; results are not intended as exact forecasts but as **policy-relevant scenario analysis**.
- Cold-chain performance and health system parameters are simplified into a small set of key parameters (e.g., hazard ratio per °C, baseline cold-chain failure, discount rate). This is transparent in the parameter tables and documented downstream in the accompanying methods.
- Cost-effectiveness classifications assume a **$100 per DALY averted threshold** and standard discounting, consistent with global health CEA norms.[][]
- The ICER map classification does not capture all dimensions of implementation feasibility (regulatory readiness, training, supply, etc.), which need to be considered alongside the quantitative outputs.[][]

---
  
  ## 8. Citation / further reading
  
  Key background readings on heat-stable carbetocin, oxytocin, and cost-effectiveness in LMICs include:
  
  - Trial and stability data and non-inferiority of HSC vs oxytocin for PPH prevention.[][][]
- Decision-analytic cost-effectiveness modelling of HSC vs oxytocin, including dynamic ICER trajectories and dominance scenarios.[]
- WHO and global guidance on HSC as an option for PPH prevention in settings where cold chain is unreliable.[][][]
- General discussions of heat-stable pharmaceutical formulations for climate-vulnerable health systems.[][]