# =============================================================================
# ui.R — Climate CEA Dashboard: HSC vs Oxytocin for PPH in LMICs
# =============================================================================

library(shiny)
library(plotly)
library(bslib)

# =============================================================================
# CUSTOM CSS
# =============================================================================
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=Playfair+Display:wght@600;700&display=swap');

body {
  font-family: 'Inter', sans-serif;
  font-size: 15px;
  background-color: #f7f6f2;
  color: #28251d;
}

.navbar,
.navbar-default,
.navbar-inverse,
.navbar-expand-lg,
.navbar-expand-md,
nav.navbar {
  background-color: #01696f !important;
  background-image: none !important;
  border: none !important;
  box-shadow: 0 2px 8px rgba(0,0,0,0.10);
}

.navbar-brand,
.navbar-default .navbar-brand,
.navbar-nav > li > a,
.navbar-default .navbar-nav > li > a,
.navbar-nav .nav-link {
  color: #ffffff !important;
  font-weight: 500;
}

.navbar-nav > li > a:hover,
.navbar-default .navbar-nav > li > a:hover,
.navbar-nav .nav-link:hover {
  color: #cedcd8 !important;
  background-color: #0c4e54 !important;
}

.navbar-nav > .active > a,
.navbar-nav > .active > a:hover,
.navbar-nav > .active > a:focus,
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus,
.navbar-nav .nav-link.active {
  color: #ffffff !important;
  background-color: #0c4e54 !important;
}

h1, h2, h3 {
  font-family: 'Playfair Display', Georgia, serif;
  color: #01696f;
}

h4, h5 {
  font-family: 'Inter', sans-serif;
  font-weight: 600;
  color: #28251d;
}

.page-intro {
  background: #ffffff;
  border: 1px solid #d4d1ca;
  border-left: 5px solid #01696f;
  border-radius: 8px;
  padding: 18px 22px;
  margin: 18px 0 20px 0;
  box-shadow: 0 2px 8px rgba(0,0,0,0.05);
  font-size: 14px;
  line-height: 1.75;
}
.page-intro p { margin-bottom: 10px; }
.page-intro p:last-child { margin-bottom: 0; }

.definition-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  gap: 14px;
  margin: 10px 0 22px 0;
}

.definition-card {
  background-color: #ffffff;
  border: 1px solid #d4d1ca;
  border-top: 3px solid #01696f;
  border-radius: 8px;
  padding: 14px 16px;
  box-shadow: 0 2px 6px rgba(0,0,0,0.05);
  font-size: 13px;
  line-height: 1.6;
}
.definition-card h4 { margin-top: 0; margin-bottom: 7px; font-size: 14px; color: #01696f; }
.definition-card p { margin-bottom: 0; }

.section-header {
  font-family: 'Playfair Display', serif;
  font-size: 20px;
  color: #01696f;
  border-bottom: 2px solid #cedcd8;
  padding-bottom: 6px;
  margin-top: 28px;
  margin-bottom: 14px;
}

.callout-box {
  background-color: #e8f4f5;
  border-left: 4px solid #01696f;
  border-radius: 6px;
  padding: 14px 18px;
  margin: 16px 0;
  font-size: 14px;
  color: #28251d;
  line-height: 1.65;
}

.impact-statement {
  background-color: #0c4e54;
  color: #ffffff;
  padding: 20px 30px;
  border-radius: 8px;
  margin-top: 30px;
  font-size: 15px;
  line-height: 1.7;
}
.impact-statement strong { color: #cedcd8; }

.eq-box {
  background-color: #ffffff;
  border: 1px solid #d4d1ca;
  border-radius: 8px;
  padding: 22px 28px;
  margin: 18px 0;
  box-shadow: 0 2px 8px rgba(0,0,0,0.06);
  overflow-x: auto;
}

.var-table { width: 100%; border-collapse: collapse; }
.var-table th {
  background-color: #01696f; color: white;
  padding: 8px 12px; font-weight: 500; font-size: 13px; text-align: left;
}
.var-table td {
  padding: 7px 12px; font-size: 13px;
  border-bottom: 1px solid #dcd9d5; vertical-align: top;
}
.var-table tr:nth-child(even) { background-color: #f3f0ec; }

.data-source-card {
  background: #ffffff; border: 1px solid #d4d1ca;
  border-radius: 8px; padding: 18px 20px;
  margin-bottom: 16px; box-shadow: 0 1px 4px rgba(0,0,0,0.04);
}
.data-source-card h4 { color: #01696f; font-size: 15px; margin-bottom: 8px; margin-top: 0; }
.data-source-card p { font-size: 13.5px; margin-bottom: 4px; line-height: 1.6; }

.github-link {
  display: inline-block; margin-top: 10px;
  color: #01696f; font-weight: 500;
  text-decoration: none; border-bottom: 1px solid #cedcd8;
}
.github-link:hover { color: #0c4e54; }

.ssp-badge {
  display: inline-block; background-color: #01696f; color: white;
  padding: 3px 10px; border-radius: 20px;
  font-size: 12px; font-weight: 600; margin-right: 6px;
}

@media (max-width: 768px) {
  .navbar-brand { font-size: 13px !important; }
  .plotly { width: 100% !important; }
  .gtitle { font-size: 12px !important; }
  .navbar-nav > li > a { font-size: 12px !important; padding: 6px 8px !important; }
  h2 { font-size: 20px !important; }
  h3, h4 { font-size: 15px !important; }
  .page-intro { padding: 13px 14px; font-size: 13px; }
  .definition-grid { grid-template-columns: 1fr; gap: 10px; }
  .definition-card { padding: 12px 14px; }
  .callout-box { padding: 11px 14px; font-size: 13px; }
  .impact-statement { padding: 16px 18px; font-size: 13.5px; }
  .var-table th, .var-table td { font-size: 12px; padding: 6px 8px; }
  .data-source-card { padding: 14px 14px; }
  .eq-box { padding: 14px 12px; }
}
"

# =============================================================================
# UI
# =============================================================================
navbarPage(
  title = "Climate Adaptation & Maternal Mortality",
  theme = bs_theme(
    version      = 5,
    bg           = "#f7f6f2",
    fg           = "#28251d",
    primary      = "#01696f",
    base_font    = font_google("Inter"),
    heading_font = font_google("Playfair Display")
  ),
  header = tags$head(
    tags$style(HTML(custom_css)),
    tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  
  # ===========================================================================
  # TAB 1: DASHBOARD
  # ===========================================================================
  tabPanel(
    "Dashboard",
    fluidPage(
      tags$div(
        style = "padding: 20px 10px 0 10px;",
        
        tags$h2("Climate Adaptation to Reduce Maternal Mortality"),
        tags$p(
          style = "color:#7a7974; font-size:14px; margin-top:-6px;",
          "A Cost-Effectiveness Analysis of Heat-Stable Carbetocin in LMICs | SSP2-4.5 | CMIP6"
        ),
        tags$a(
          class  = "github-link",
          href   = "https://github.com/TanmayeeKodali/Climate-Adaptation-to-Reduce-Maternal-Mortality-A-CEA-of-Heat-Stable-Carbetocin-in-LMICs",
          target = "_blank",
          "\U0001f4c2 View Source Code on GitHub"
        ),
        
        tags$div(
          class = "page-intro",
          tags$p("Climate change is driving global temperatures to increasingly exceed 40\u00b0C across low- and middle-income countries (LMICs), directly undermining the effectiveness of life-saving maternal health interventions. Oxytocin, the most critical drug used in labor and delivery, requires stable storage between 2\u20138\u00b0C to maintain its potency, making climate adaptation in drug policy an urgent public health priority."),
          tags$p("Studies from LMICs show that oxytocin potency falls below 50% by the time doses reach health facilities, dropping even further in rural areas. This degradation weakens the ability to prevent postpartum hemorrhage (PPH), the leading cause of maternal mortality worldwide, with 94% of deaths concentrated in LMICs."),
          tags$p("This dashboard examines the climate\u2013health pathway by visualizing projected temperature warming and estimating the number of women at risk of inadequately treated PPH, while motivating a climate-adaptive policy shift from oxytocin to heat-stable carbetocin in high-temperature LMIC settings.")
        ),
        
        tags$div(
          class = "definition-grid",
          tags$div(class = "definition-card",
                   tags$h4("Postpartum Hemorrhage (PPH)"),
                   tags$p("Excessive blood loss (>500 mL) after childbirth, affecting ~11% of births globally. PPH is the single leading cause of maternal mortality, with the highest burden in LMICs where emergency care is least accessible.")
          ),
          tags$div(class = "definition-card",
                   tags$h4("Oxytocin"),
                   tags$p("Oxytocin is the first-line uterotonic for PPH prevention, but requires refrigeration at 2\u20138\u00b0C. Exposure to heat degrades potency rapidly via Arrhenius kinetics \u2014 a 5\u00b0C rise roughly doubles the degradation rate.")
          ),
          tags$div(class = "definition-card",
                   tags$h4("Heat-Stable Carbetocin"),
                   tags$p("A WHO-recommended alternative to oxytocin that retains full potency across a wide temperature range with no cold-chain requirement, making it especially valuable for remote communities in high-temperature LMICs.")
          ),
          tags$div(class = "definition-card",
                   tags$h4("Why LMICs?"),
                   tags$p("LMICs face the sharpest projected warming under SSP2-4.5, the weakest cold-chain infrastructure, and the greatest maternal mortality burden, making them the central focus of climate-adaptive drug policy.")
          )
        ),
        
        tags$hr(style = "border-color: #dcd9d5; margin: 16px 0;"),
        
        # Viz 1
        tags$div(class = "section-header", "Visualization 1: Projected Temperature Warming in LMICs"),
        tags$div(
          class = "callout-box",
          tags$span(class = "ssp-badge", "SSP2-4.5"),
          tags$strong("Why SSP2-4.5?"),
          " The IPCC\u2019s \u2018middle of the road\u2019 pathway assumes moderate emissions reductions. It avoids the optimism of SSP1-2.6 and the extreme pessimism of SSP5-8.5."
        ),
        plotlyOutput("choropleth", height = "580px", width = "100%"),
        
        tags$hr(style = "border-color: #dcd9d5; margin: 24px 0;"),
        
        # Viz 2
        tags$div(class = "section-header", "Visualization 2: Women at Risk of Inadequately Treated PPH"),
        tags$div(
          class = "callout-box",
          tags$strong("How to read this chart: "),
          "Each bubble is a country. The x-axis shows projected mean temperature; the y-axis shows estimated women at risk of receiving degraded oxytocin. Bubble size reflects total births. Use the ",
          tags$strong("Panel 1 / Panel 2 buttons"),
          " to compare 2020\u20132039 vs 2040\u20132059. Hover for country-level detail."
        ),
        plotlyOutput("bubble_chart", height = "620px", width = "100%"),
        
        tags$hr(style = "border-color: #dcd9d5; margin: 24px 0;"),
        
        # Viz 3
        tags$div(class = "section-header", "Visualization 3: Threshold-Crossing Year Map for Cost-Effectiveness"),
        tags$div(
          class = "callout-box",
          tags$strong("What this map shows: "),
          "Each country is classified by the earliest year heat-stable carbetocin becomes cost-effective vs oxytocin, using a willingness-to-pay threshold of ",
          tags$strong("$100 per DALY averted"),
          ". Countries marked ", tags$strong("Dominant"),
          " are where HSC is both more effective and cost-saving. Hover for the threshold-crossing year, cumulative ICER, and discounted DALYs averted."
        ),
        plotlyOutput("viz3_map", height = "620px", width = "100%"),
        
        tags$div(
          class = "impact-statement",
          tags$strong("Why this matters: "),
          "Postpartum hemorrhage is the leading cause of maternal mortality globally, and oxytocin \u2014 the primary drug used to prevent it \u2014 requires a 2\u20138\u00b0C cold chain that is increasingly compromised by rising temperatures across LMICs.",
          tags$br(), tags$br(),
          tags$strong("Policy implication: "),
          "Transitioning to heat-stable carbetocin represents a climate-adaptive, cost-effective intervention to protect maternal health in a warming world, particularly in remote settings where maintaining oxytocin potency is least reliable."
        ),
        
        tags$p(
          style = "font-size:11px; color:#bab9b4; text-align:center; padding: 16px 0;",
          "Dashboard developed by Tanmayee Kodali | Emory University | 2026"
        )
      )  # close tags$div padding
    )    # close fluidPage
  ),     # close tabPanel Dashboard
  
  # ===========================================================================
  # TAB 2: METHODS
  # ===========================================================================
  tabPanel(
    "Methods",
    fluidPage(
      tags$div(
        style = "max-width: 900px; margin: auto; padding: 30px 20px;",
        
        tags$h2("Methods: Risk and Cost-Effectiveness Model"),
        tags$p("This dashboard uses a sequential climate-health-economic framework. First, it estimates women exposed to compromised oxytocin under projected warming. It then translates that into inadequately treated PPH events, DALYs, cumulative costs, and ICERs comparing oxytocin with heat-stable carbetocin."),
        
        tags$div(class = "section-header", "Risk Model: Women at Risk"),
        tags$div(
          class = "eq-box",
          withMathJax(tags$p(style = "font-size:16px; text-align:center;",
                             "$$W_{\\text{risk},c}^t = B_c^t \\cdot PF_{c,t} \\cdot P_{\\text{PPH}} \\cdot P_{\\text{oxy}} \\cdot PCCF_{c,t} \\cdot D(T_c, \\tau)$$"
          )),
          tags$p(style = "font-size:13px; color:#7a7974; text-align:center; margin-top:10px;",
                 "Institutional delivery coverage uses a logistic growth model:"),
          withMathJax(tags$p(style = "font-size:14px; text-align:center;",
                             "$$PF_{c,t} = \\frac{K_c}{1 + e^{-r_c(t - t_{0,c})}}$$"
          )),
          tags$p(style = "font-size:13px; color:#7a7974; text-align:center; margin-top:10px;",
                 "Cold-chain failure probability:"),
          withMathJax(tags$p(style = "font-size:14px; text-align:center;",
                             "$$PCCF_{c,t} = \\min\\left(PCCF_{\\text{reg},c} \\times HR^{\\Delta T_{c,t}},\\ 1.0\\right)$$"
          )),
          tags$p(style = "font-size:13px; color:#7a7974; text-align:center; margin-top:10px;",
                 "Degradation probability (Arrhenius first-order decay):"),
          withMathJax(tags$p(style = "font-size:14px; text-align:center; margin-bottom:0;",
                             "$$D(T_c, \\tau) = 1 - e^{-k(T_c)\\,\\tau} \\quad \\text{where} \\quad k(T_c) = k_{\\text{anch}} \\cdot \\exp\\!\\left[\\frac{E_a}{R}\\left(\\frac{1}{303} - \\frac{1}{T_c + 273.15}\\right)\\right]$$"
          ))
        ),
        
        # ---------------------------------------------------------------------
        # CEA equations
        # ---------------------------------------------------------------------
        tags$div(class = "section-header", "CEA Equations"),
        tags$div(
          class = "eq-box",
          tags$p(
            style = "font-size:13px; color:#7a7974; text-align:center; margin-bottom:12px;",
            "The annual cost-effectiveness model follows a simple sequence: first estimate the number of women at risk of postpartum haemorrhage, then narrow to those exposed to degraded oxytocin, convert that exposure into DALYs, and finally compare oxytocin with heat-stable carbetocin using an incremental cost-effectiveness ratio (ICER)."
          ),
          
          withMathJax(tags$p(style = "font-size:15px; text-align:center;",
                             "$$W_{\\text{riskPPH},c,t} = B_{c,t} \\cdot PF_{c,t} \\cdot P_{\\text{PPH}} \\cdot P_{\\text{oxy}}$$"
          )),
          tags$p(
            style = "font-size:13px; color:#7a7974; text-align:center; margin-top:4px;",
            "This gives the number of women in country c and year t who deliver in facilities, experience postpartum haemorrhage, and are treated with oxytocin."
          ),
          
          withMathJax(tags$p(style = "font-size:15px; text-align:center;",
                             "$$W_{\\text{risk},c,t} = W_{\\text{riskPPH},c,t} \\cdot PCCF_{c,t} \\cdot D(T_c,\\tau)$$"
          )),
          tags$p(
            style = "font-size:13px; color:#7a7974; text-align:center; margin-top:4px;",
            "This narrows the pool to those who receive a clinically compromised oxytocin dose after cold-chain failure and temperature-driven degradation."
          ),
          
          withMathJax(tags$p(style = "font-size:15px; text-align:center;",
                             "$$DALY_{\\text{oxy},c,t} = W_{\\text{risk},c,t} \\cdot DW \\cdot L$$"
          )),
          tags$p(
            style = "font-size:13px; color:#7a7974; text-align:center; margin-top:4px;",
            "Health loss is expressed in DALYs by applying a disability weight and duration to the number of women exposed to ineffective oxytocin."
          ),
          
          withMathJax(tags$p(style = "font-size:15px; text-align:center;",
                             "$$DALY_{\\text{oxy},c,t}^{disc} = \\frac{DALY_{\\text{oxy},c,t}}{(1+r)^{t-t_0}}$$"
          )),
          tags$p(
            style = "font-size:13px; color:#7a7974; text-align:center; margin-top:4px;",
            "Future health losses are discounted back to the base year using a 3\\% annual rate."
          ),
          
          withMathJax(tags$p(style = "font-size:15px; text-align:center;",
                             "$$Cost_{c,t}^{disc,oxy} = \\frac{W_{\\text{riskPPH},c,t} \\cdot C_{oxy}^{adj}}{(1+r)^{t-t_0}}$$"
          )),
          tags$p(
            style = "font-size:13px; color:#7a7974; text-align:center; margin-top:4px;",
            "The oxytocin arm includes a climate-adjusted per-use cost that rises as cold-chain failure and degradation become more likely."
          ),
          
          withMathJax(tags$p(style = "font-size:15px; text-align:center;",
                             "$$Cost_{c,t}^{disc,HSC} = \\frac{W_{\\text{riskPPH},c,t} \\cdot C_{HSC}}{(1+r)^{t-t_0}}$$"
          )),
          tags$p(
            style = "font-size:13px; color:#7a7974; text-align:center; margin-top:4px;",
            "The heat-stable carbetocin arm assumes a fixed per-use cost because it does not require refrigeration."
          ),
          
          withMathJax(tags$p(style = "font-size:15px; text-align:center;",
                             "$$\\Delta Cost_{c,t}^{disc} = Cost_{c,t}^{disc,HSC} - Cost_{c,t}^{disc,oxy}$$"
          )),
          tags$p(
            style = "font-size:13px; color:#7a7974; text-align:center; margin-top:4px;",
            "Incremental cost is positive when HSC is more expensive and negative when HSC is cost-saving."
          ),
          
          withMathJax(tags$p(style = "font-size:15px; text-align:center; margin-bottom:0;",
                             "$$ICER_{c,t} = \\frac{\\Delta Cost_{c,t}^{disc}}{DALY_{\\text{oxy},c,t}^{disc}}$$"
          )),
          tags$p(
            style = "font-size:13px; color:#7a7974; text-align:center; margin-top:6px; margin-bottom:0;",
            "These annual ICERs are then used to identify the earliest year in which heat-stable carbetocin becomes cost-effective under the $100 per DALY threshold."
          )
        ),
        
        # ---------------------------------------------------------------------
        # Variable definitions
        # ---------------------------------------------------------------------
        tags$div(class = "section-header", "Variable Definitions & Sources"),
        tags$div(
          style = "overflow-x:auto;",
          tags$table(
            class = "var-table",
            tags$thead(
              tags$tr(
                tags$th("Symbol"), tags$th("Meaning"), tags$th("Input / Source")
              )
            ),
            tags$tbody(
              tags$tr(tags$td(HTML("\\(B_{c,t}\\)")), tags$td("Annual live births in country c and year t"), tags$td("UN WPP 2024, Medium Variant")),
              tags$tr(tags$td(HTML("\\(PF_{c,t}\\)")), tags$td("Institutional delivery coverage in country c and year t"), tags$td("Country-specific logistic model fit to WDI SH.STA.BRTC.ZS")),
              tags$tr(tags$td(HTML("\\(P_{\\text{PPH}}\\)")), tags$td("Probability of postpartum haemorrhage per birth"), tags$td("0.1125 base case")),
              tags$tr(tags$td(HTML("\\(P_{\\text{oxy}}\\)")), tags$td("Probability that oxytocin is used as the primary uterotonic"), tags$td("0.80 base case")),
              tags$tr(tags$td(HTML("\\(W_{\\text{riskPPH},c,t}\\)")), tags$td("Women at risk of postpartum haemorrhage who are treated with oxytocin"), tags$td("Derived from births, facility delivery, PPH probability, and oxytocin use")),
              tags$tr(tags$td(HTML("\\(PCCF_{c,t}\\)")), tags$td("Probability of cold-chain failure in country c and year t"), tags$td("Regional baseline scaled by warming using hazard-ratio adjustment")),
              tags$tr(tags$td(HTML("\\(P_{CCF,reg}\\)")), tags$td("Baseline regional cold-chain failure probability"), tags$td("Regional literature-based input")),
              tags$tr(tags$td(HTML("\\(HR\\)")), tags$td("Hazard ratio used to scale cold-chain failure with warming"), tags$td("1.60 per degree C, base case")),
              tags$tr(tags$td(HTML("\\(T_c\\)")), tags$td("Projected temperature used in the degradation function"), tags$td("World Bank CCKP CMIP6 SSP2-4.5")),
              tags$tr(tags$td(HTML("\\(\\tau\\)")), tags$td("Duration of heat exposure under failed cold chain"), tags$td("6 months in annual CEA; 3 months in panel model")),
              tags$tr(tags$td(HTML("\\(D(T_c,\\tau)\\)")), tags$td("Probability that oxytocin degrades at temperature T over duration tau"), tags$td("Arrhenius first-order degradation model; Hawe et al. 2009")),
              tags$tr(tags$td(HTML("\\(W_{\\text{risk},c,t}\\)")), tags$td("Women exposed to degraded oxytocin in country c and year t"), tags$td("Derived from W_riskPPH, cold-chain failure, and degradation probability")),
              tags$tr(tags$td(HTML("\\(DW\\)")), tags$td("Disability weight used to convert exposure into DALYs"), tags$td("0.29 base case")),
              tags$tr(tags$td(HTML("\\(L\\)")), tags$td("Duration of health loss used in DALY calculation"), tags$td("0.024 years")),
              tags$tr(tags$td(HTML("\\(DALY_{\\text{oxy},c,t}\\)")), tags$td("Undiscounted DALYs in the oxytocin arm due to degraded treatment exposure"), tags$td("Derived from W_risk × disability weight × duration")),
              tags$tr(tags$td(HTML("\\(r\\)")), tags$td("Annual discount rate"), tags$td("3% base case")),
              tags$tr(tags$td(HTML("\\(t_0\\)")), tags$td("Base year for discounting"), tags$td("2020")),
              tags$tr(tags$td(HTML("\\(DALY_{\\text{oxy},c,t}^{disc}\\)")), tags$td("Discounted DALYs in the oxytocin arm"), tags$td("DALYs discounted at 3% per year")),
              tags$tr(tags$td(HTML("\\(C_{oxy}^{adj}\\)")), tags$td("Climate-adjusted per-use cost of oxytocin"), tags$td("Base oxytocin cost adjusted for degradation-related penalty")),
              tags$tr(tags$td(HTML("\\(C_{HSC}\\)")), tags$td("Per-use cost of heat-stable carbetocin"), tags$td("Base case intervention cost")),
              tags$tr(tags$td(HTML("\\(Cost_{c,t}^{disc,oxy}\\)")), tags$td("Discounted oxytocin arm cost in country c and year t"), tags$td("Women at risk on oxytocin multiplied by climate-adjusted oxytocin cost")),
              tags$tr(tags$td(HTML("\\(Cost_{c,t}^{disc,HSC}\\)")), tags$td("Discounted HSC arm cost in country c and year t"), tags$td("Women at risk on oxytocin multiplied by HSC cost")),
              tags$tr(tags$td(HTML("\\(\\Delta Cost_{c,t}^{disc}\\)")), tags$td("Discounted incremental cost of HSC relative to oxytocin"), tags$td("HSC discounted cost minus oxytocin discounted cost")),
              tags$tr(tags$td(HTML("\\(ICER_{c,t}\\)")), tags$td("Annual incremental cost-effectiveness ratio in country c and year t"), tags$td("Discounted incremental cost divided by discounted DALYs"))
            )
          )
        ),
        
        tags$div(class = "section-header", "How Women-at-Risk Bubble Chart is Interpreted"),
        tags$div(class = "callout-box",
                 tags$p("Each bubble represents a ",
                        tags$strong("country-year observation"),
                        ". The bubble’s position is based on the selected x- and y-axis variables, while the bubble size reflects the estimated number of ",
                        tags$strong("women at risk"),
                        " (", tags$code("W_risk"), "). Bubble charts are commonly used when the size of each point represents a third numeric variable [web:32]."),
                 tags$p("Larger bubbles indicate a greater number of women exposed to the risk of degraded oxytocin at the point of use, while smaller bubbles indicate lower burden."),
                 tags$p(tags$code("W_risk"), " is computed as ",
                        tags$strong("B_t × PF(c,t) × P_PPH × P_oxy × PCCF(c,t) × D(c,t)"),
                        ", combining births, facility delivery, PPH treatment exposure, cold-chain failure risk, and oxytocin degradation probability.")
        ),
        
        tags$div(class = "section-header", "How Threshold-Crossing Year Map is Classified"),
        tags$div(class = "callout-box",
                 tags$p("Each country is assigned: ",
                        tags$strong("Dominant"), " (HSC cheaper and more effective), ",
                        tags$strong("Cost-effective by 2035"), ", ",
                        tags$strong("Cost-effective by 2059"), ", or ",
                        tags$strong("Not cost-effective"),
                        " based on the first year the annual ICER falls below $100/DALY."),
                 tags$p("If cumulative incremental cost is negative while cumulative DALYs averted are positive, the country is labelled Dominant.")
        ),
        
        tags$div(class = "section-header", "SSP2-4.5: Scenario Rationale"),
        tags$div(class = "callout-box",
                 tags$p(tags$span(class = "ssp-badge", "SSP2-4.5"), tags$strong(" Middle of the Road Scenario")),
                 tags$ol(
                   tags$li(tags$strong("Policy relevance: "), "Consistent with current nationally determined contributions (NDCs)."),
                   tags$li(tags$strong("Avoids extremes: "), "More realistic for 2020\u20132059 than SSP1-2.6 or SSP5-8.5."),
                   tags$li(tags$strong("CEA defensibility: "), "Credible baseline for climate-adaptive drug substitution analysis.")
                 )
        )
        
      )    # close tags$div max-width
    )      # close fluidPage
  ),       # close tabPanel Methods
  
  # ===========================================================================
  # TAB 3: ABOUT THE DATA
  # ===========================================================================
  tabPanel(
    "About the Data",
    fluidPage(
      tags$div(
        style = "max-width: 860px; margin: auto; padding: 30px 20px;",
        
        tags$h2("About the Data"),
        tags$p(style = "color:#7a7974;",
               "This dashboard integrates climate, demographic, health systems, and cost-effectiveness inputs. No individual-level data was collected \u2014 all projections are modelled estimates derived from country-level aggregates."
        ),
        
        tags$div(class = "data-source-card",
                 tags$h4("\U0001f321\ufe0f World Bank Climate Change Knowledge Portal (CCKP) \u2014 CMIP6"),
                 tags$p(tags$strong("Source: "), tags$a(href = "https://climateknowledgeportal.worldbank.org", target = "_blank", "climateknowledgeportal.worldbank.org")),
                 tags$p(tags$strong("Variables: "), "Annual mean surface temperature (tas), SSP2-4.5 CMIP6 ensemble median."),
                 tags$p(tags$strong("Coverage: "), "LMICs | Climatology windows: 1995\u20132014 baseline, 2020\u20132039, 2040\u20132059."),
                 tags$p(tags$strong("Use in model: "), "Feeds dashboard warming panels and annual interpolation for threshold-crossing CEA.")
        ),
        
        tags$div(class = "data-source-card",
                 tags$h4("\U0001f469\u200d\U0001f37c UN World Population Prospects 2024 (UN WPP)"),
                 tags$p(tags$strong("Source: "), tags$a(href = "https://population.un.org/wpp/", target = "_blank", "population.un.org/wpp")),
                 tags$p(tags$strong("Variables: "), "Annual live births by country (Medium Variant)."),
                 tags$p(tags$strong("Use in model: "), "Panel totals for Visualizations 1\u20132 and annual births for the country-year CEA.")
        ),
        
        tags$div(class = "data-source-card",
                 tags$h4("\U0001f3e5 World Bank World Development Indicators (WDI)"),
                 tags$p(tags$strong("Source: "), tags$a(href = "https://databank.worldbank.org/source/world-development-indicators", target = "_blank", "World Bank WDI")),
                 tags$p(tags$strong("Indicator: "), "SH.STA.BRTC.ZS \u2014 Births attended by skilled health staff (% of total)."),
                 tags$p(tags$strong("Use in model: "), "Logistic curves for institutional delivery coverage, evaluated at panel midpoints and interpolated for the annual CEA.")
        ),
        
        tags$div(class = "data-source-card",
                 tags$h4("\U0001f4da Literature-Derived Model Constants"),
                 tags$p(tags$strong("Arrhenius parameters: "), "Hawe et al. (2009). Ea = 122,000 J/mol; k = 0.151 yr\u207b\u00b9 at 30\u00b0C."),
                 tags$p(tags$strong("PPH incidence: "), "Lancet (2025). P_PPH = 0.1125."),
                 tags$p(tags$strong("Cold chain hazard ratio: "), "Mullany et al. (2014). HR = 1.60 per \u00b0C warming."),
                 tags$p(tags$strong("Oxytocin use rate: "), "P_oxy = 0.80 base case."),
                 tags$p(tags$strong("Costs: "), "Oxytocin $0.385/birth (climate-adjusted); HSC $0.40/birth. Discount rate 3%.")
        ),
        
        tags$div(class = "data-source-card",
                 tags$h4("\U0001f5fa Incremental Cost-Effectiveness Analysis"),
                 tags$p(tags$strong("Metric: "), "Earliest year cumulative ICER for HSC falls below $100/DALY averted vs oxytocin."),
                 tags$p(tags$strong("Classification: "), "Dominant, Cost-effective by 2035, Cost-effective by 2059, or Not cost-effective."),
                 tags$p(tags$strong("Interpretation: "), "Earlier threshold-crossing = greater urgency for climate-adaptive drug policy.")
        ),
        
        tags$hr(style = "border-color:#dcd9d5; margin: 28px 0;"),
        tags$p(style = "font-size:12px; color:#bab9b4;",
               "Data: World Bank CCKP CMIP6 | UN WPP 2024 | WDI | Hawe et al. 2009 | Mullany et al. 2014"
        )
        
      )    # close tags$div max-width
    )      # close fluidPage
  )        # close tabPanel About the Data
  
)          # close navbarPage
