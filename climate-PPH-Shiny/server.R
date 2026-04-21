# =============================================================================
# server.R — Climate CEA Dashboard: HSC vs Oxytocin for PPH in LMICs
# Fixes applied:
#   Bug 1  — Viz 3 colorscale: c() → list() for each stop
#   Bug 2  — Viz 2 W_risk_pct_change: guarded with is.finite() fallback
#   Bug 3  — All Unicode escapes: \\u → \u inside paste0() throughout
#   Bug 4  — Viz 3 colorscale uses list(list(0,...), list(1,...)) consistently
# =============================================================================

function(input, output, session) {
  
  # ===========================================================================
  # VIZ 1 — Choropleth: Projected Temperature Warming
  # ===========================================================================
  output$choropleth <- renderPlotly({
    
    color_scale_warming <- list(
      list(0,    "#ffffb2"),
      list(0.33, "#fecc5c"),
      list(0.50, "#fd8d3c"),
      list(0.67, "#e31a1c"),
      list(1,    "#800026")
    )
    
    panel1_data <- warming_near %>%
      mutate(
        hover_text = paste0(
          "<b>", country, "</b><br>",
          "Projected temp: ", round(tas_future, 1), "\u00b0C<br>",
          "Change vs baseline: +", round(warming, 2), "\u00b0C"
        )
      )
    
    panel2_data <- warming_mid %>%
      mutate(
        hover_text = paste0(
          "<b>", country, "</b><br>",
          "Projected temp: ", round(tas_future, 1), "\u00b0C<br>",
          "Change vs baseline: +", round(warming, 2), "\u00b0C"
        )
      )
    
    plot_ly() %>%
      add_trace(
        data       = panel1_data,
        type       = "choropleth",
        locations  = ~iso3,
        z          = ~warming,
        text       = ~hover_text,
        hoverinfo  = "text",
        colorscale = color_scale_warming,
        zmin = 0, zmax = 3,
        colorbar = list(
          title    = "Warming vs\nBaseline (\u00b0C)",
          tickvals = c(0, 1, 1.5, 2, 3),
          ticktext = c("0\u00b0C", "+1\u00b0C", "+1.5\u00b0C (Paris)", "+2\u00b0C (Paris)", "+3\u00b0C"),
          lenmode  = "fraction", len = 0.75
        ),
        marker  = list(line = list(color = "white", width = 0.3)),
        visible = TRUE,
        name    = "Panel 1"
      ) %>%
      add_trace(
        data       = panel2_data,
        type       = "choropleth",
        locations  = ~iso3,
        z          = ~warming,
        text       = ~hover_text,
        hoverinfo  = "text",
        colorscale = color_scale_warming,
        zmin = 0, zmax = 3,
        colorbar = list(
          title    = "Warming vs\nBaseline (\u00b0C)",
          tickvals = c(0, 1, 1.5, 2, 3),
          ticktext = c("0\u00b0C", "+1\u00b0C", "+1.5\u00b0C (Paris)", "+2\u00b0C (Paris)", "+3\u00b0C"),
          lenmode  = "fraction", len = 0.75
        ),
        marker  = list(line = list(color = "white", width = 0.3)),
        visible = FALSE,
        name    = "Panel 2"
      ) %>%
      layout(
        title = list(
          text = paste0(
            "<b>Projected Temperature Warming in LMICs vs. 1995\u20132014 Baseline</b><br>",
            "<sup>SSP2-4.5 CMIP6 | Source: World Bank CCKP | Hover for country details</sup>"
          ),
          x = 0.5, font = list(size = 17)
        ),
        updatemenus = list(list(
          type       = "buttons", direction = "right",
          x = 0.5, xanchor = "center", y = 1.08, yanchor = "top",
          showactive = TRUE,
          bgcolor = "#f8f9fa", bordercolor = "#cccccc", font = list(size = 13),
          buttons = list(
            list(
              label  = "Panel 1: 2020\u20132039",
              method = "update",
              args   = list(list(visible = c(TRUE, FALSE)))
            ),
            list(
              label  = "Panel 2: 2040\u20132059",
              method = "update",
              args   = list(list(visible = c(FALSE, TRUE)))
            )
          )
        )),
        geo = list(
          showframe      = FALSE,
          showcoastlines = TRUE,
          projection     = list(type = "natural earth"),
          bgcolor        = "#f0f0f0"
        ),
        margin        = list(l = 60, r = 60, t = 120, b = 60),
        paper_bgcolor = "#ffffff"
      )
  })
  
  # ===========================================================================
  # VIZ 2 — Bubble Chart: Women at Risk
  # Legend: dummy scatter traces (always visible) rendered first.
  # visible vector order: [dummy x n_regions] [P1 x n_regions] [P2 x n_regions]
  # ===========================================================================
  output$bubble_chart <- renderPlotly({
    
    n_regions <- length(region_colors)
    fig       <- plot_ly()
    
    # --- Panel 1 traces (visible by default) ---
    for (reg in names(region_colors)) {
      sub <- plot_p1 %>% filter(region == reg)
      if (nrow(sub) == 0) next
      fig <- fig %>%
        add_trace(
          data        = sub,
          x           = ~T_proj,
          y           = ~W_risk,
          type        = "scatter",
          mode        = "markers",
          name        = reg,
          legendgroup = reg,
          visible     = TRUE,
          marker = list(
            size        = ~bubble_size,
            color       = region_colors[[reg]],
            opacity     = 0.78,
            sizemode    = "diameter",
            line        = list(color = "white", width = 1.2)
          ),
          text = ~paste0(
            "<b>", country, "</b><br>",
            "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500<br>",
            "At risk of PPH needing oxytocin: <b>",
            formatC(round(W_riskPPH), big.mark = ",", format = "d"), "</b><br>",
            "\u21b3 Est. clinically failed dose: ",
            formatC(round(W_risk), big.mark = ",", format = "d"), "<br>",
            "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500<br>",
            "Proj. mean temp: ", round(T_proj, 1), "\u00b0C<br>",
            "Warming vs baseline: +", round(warming, 2), "\u00b0C<br>",
            "Facility birth rate: ", round(P_F * 100, 1), "%<br>",
            "Cold-chain failure P: ", round(P_CCF, 3), "<br>",
            "Degradation prob. (D): ", round(D_oxy, 3), "<br>",
            "Total births (P1): ", formatC(round(total_births_p1), big.mark = ",", format = "d")
          ),
          hoverinfo = "text"
        )
    }
    
    # --- Panel 2 traces -------------------------------------------------------
    for (reg in names(region_colors)) {
      sub <- plot_p2 %>% filter(region == reg)
      if (nrow(sub) == 0) next
      fig <- fig %>%
        add_trace(
          data        = sub,
          x           = ~T_proj,
          y           = ~W_risk,
          type        = "scatter",
          mode        = "markers",
          name        = reg,
          legendgroup = reg,
          showlegend  = FALSE,
          visible     = FALSE,
          marker = list(
            size        = ~bubble_size,
            color       = region_colors[[reg]],
            opacity     = 0.78,
            sizemode    = "diameter",
            line        = list(color = "white", width = 1.2)
          ),
          text = ~paste0(
            "<b>", country, "</b><br>",
            "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500<br>",
            "At risk of PPH needing oxytocin: <b>",
            formatC(round(W_riskPPH), big.mark = ",", format = "d"), "</b><br>",
            "\u21b3 Est. clinically failed dose: ",
            formatC(round(W_risk), big.mark = ",", format = "d"), "<br>",
            "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500<br>",
            "Proj. mean temp: ", round(T_proj, 1), "\u00b0C<br>",
            "Warming vs baseline: +", round(warming, 2), "\u00b0C<br>",
            "Facility birth rate: ", round(P_F * 100, 1), "%<br>",
            "Facility birth rate change: ",
            round(P_F_prev * 100, 1), "% \u2192 ", round(P_F * 100, 1), "%<br>",
            "Cold-chain failure P: ", round(P_CCF, 3), "<br>",
            "Degradation prob. (D): ", round(D_oxy, 3), "<br>",
            "Total births (P2): ",
            formatC(round(total_births_p2), big.mark = ",", format = "d"), "<br>",
            # BUG 2 FIX: guard W_risk_pct_change against NaN / Inf
            "Change P1 \u2192 P2: ",
            ifelse(
              is.finite(W_risk_pct_change),
              paste0(ifelse(W_risk_pct_change > 0, "+", ""), round(W_risk_pct_change, 1), "%"),
              "N/A"
            )
          ),
          hoverinfo = "text"
        )
    }
    
    fig %>%
      layout(
        title = list(
          text = paste0(
            "<b>Women at Risk: Climate-Compromised Oxytocin in LMICs</b><br>",
            "<sup>SSP2-4.5 CMIP6 | Bubble size = projected births | ",
            "Base case: P_PPH = 11.25%, P_oxy = 0.80, \u03c4 = 6 mo | ",
            "Hover to see false-safety breakdown</sup>"
          ),
          x = 0.5, xanchor = "center", font = list(size = 17)
        ),
        updatemenus = list(list(
          type = "buttons", direction = "right",
          x = 0.5, xanchor = "center", y = 1.08, yanchor = "top",
          showactive = TRUE,
          bgcolor = "#f8f9fa", bordercolor = "#cccccc", font = list(size = 13),
          buttons = list(
            list(
              label  = "Panel 1: 2020\\u20132039",
              method = "update",
              args   = list(
                list(
                  visible = c(
                    rep(TRUE,  n_regions),  # P1 on
                    rep(FALSE, n_regions)   # P2 off
                  )
                ),
                list(
                  xaxis = list(
                    title    = "Projected Mean Temperature 2020\\u20132039 (\\u00b0C)",
                    showgrid = TRUE, gridcolor = "#eeeeee",
                    zeroline = FALSE, showline = TRUE, linecolor = "#cccccc"
                  )
                )
              )
            ),
            list(
              label  = "Panel 2: 2040\\u20132059",
              method = "update",
              args   = list(
                list(
                  visible = c(
                    rep(FALSE, n_regions),  # P1 off
                    rep(TRUE,  n_regions)   # P2 on
                  )
                ),
                list(
                  xaxis = list(
                    title    = "Projected Mean Temperature 2040\\u20132059 (\\u00b0C)",
                    showgrid = TRUE, gridcolor = "#eeeeee",
                    zeroline = FALSE, showline = TRUE, linecolor = "#cccccc"
                  )
                )
              )
            )
          )
        )),
        xaxis = list(
          title    = "Projected Mean Surface Temperature 2020\u20132039 (\u00b0C), SSP2-4.5",
          showgrid = TRUE, gridcolor = "#eeeeee",
          zeroline = FALSE, showline = TRUE, linecolor = "#cccccc",
          tickfont = list(size = 11)
        ),
        yaxis = list(
          title      = "Estimated Women Receiving a Clinically Failed Oxytocin Dose",
          showgrid   = TRUE, gridcolor = "#eeeeee",
          zeroline   = FALSE, showline = TRUE, linecolor = "#cccccc",
          tickformat = ",", tickfont = list(size = 11)
        ),
        legend = list(
          title       = list(text = "Region"),
          orientation = "v", x = 1.01, y = 0.99,
          bgcolor     = "rgba(255,255,255,0.9)",
          bordercolor = "#dddddd", borderwidth = 1,
          font        = list(size = 11)
        ),
        margin        = list(l = 80, r = 60, t = 120, b = 80),
        paper_bgcolor = "white", plot_bgcolor = "white",
        hovermode     = "closest",
        hoverlabel    = list(
          bgcolor     = "#2c3e50", bordercolor = "#2c3e50",
          font        = list(size = 12, family = "Georgia", color = "#ffffff")
        )
      )
  })
  
  # ===========================================================================
  # VIZ 3 — ICER Threshold-Crossing Year Map
  #
  # BUG 1 FIX — colorscale stops must be list(number, string), NOT c(number, string)
  #             c() coerces both to character; Plotly JS then ignores the scale
  #             and renders all fills as transparent/missing.
  #
  # BUG 3 FIX — showscale = FALSE on EVERY choropleth trace (default is TRUE),
  #             otherwise a continuous colourbar overlaps the categorical legend.
  #
  # BUG 4 FIX — dummy scatter legend traces use mode = "markers", symbol = "square"
  #             so the legend swatches are square-filled and clearly visible.
  # ===========================================================================
  output$viz3_map <- renderPlotly({
    
    cat_order <- c(
      "Dominant",
      "Cost-effective by 2035",
      "Cost-effective by 2059",
      "Not cost-effective"
    )
    
    cat_colors <- c(
      "Dominant"                  = "#01696f",
      "Cost-effective by 2035"    = "#4f98a3",
      "Cost-effective by 2059"    = "#a8cdd0",
      "Not cost-effective"        = "#bab9b4"
    )
    
    map_data <- threshold_map %>%
      mutate(
        ce_label   = factor(ce_category, levels = cat_order),
        hover_text = paste0(
          "<b>", country, "</b><br>",
          "Region: ", region, "<br>",
          dplyr::case_when(
            ce_category == "Dominant" ~ paste0(
              "\U0001f7e2 Dominant \u2014 HSC cheaper & more effective<br>",
              "Cumulative ICER: $", round(ICER_cumulative, 2), "/DALY<br>",
              "DALYs averted (disc.): ",
              formatC(round(total_DALYs_disc, 1), big.mark = ",", format = "f"), "<br>",
              "Net savings vs oxytocin: $",
              formatC(round(-total_delta_cost_disc), big.mark = ",", format = "d")
            ),
            !is.na(first_ce_year) ~ paste0(
              "\U0001f4c5 First cost-effective year: ", first_ce_year, "<br>",
              "Cumulative ICER: $", round(ICER_cumulative, 2), "/DALY<br>",
              "DALYs averted (disc.): ",
              formatC(round(total_DALYs_disc, 1), big.mark = ",", format = "f")
            ),
            TRUE ~ paste0(
              "\u26d4 Not cost-effective at $100/DALY threshold<br>",
              "Cumulative ICER: ",
              ifelse(
                is.na(ICER_cumulative),
                "N/A",
                paste0("$", round(ICER_cumulative, 2), "/DALY")
              )
            )
          )
        )
      )
    
    fig <- plot_ly()
    
    # --- One choropleth trace per category (fixed single colour) --------------
    for (cat in cat_order) {
      sub <- map_data %>% filter(ce_category == cat)
      if (nrow(sub) == 0) next
      fig <- fig %>%
        add_trace(
          data       = sub,
          type       = "choropleth",
          locations  = ~iso3,
          z          = ~ce_z,
          text       = ~hover_text,
          hoverinfo  = "text",
          # BUG 1 FIX: list(list(...), list(...)) not list(c(...), c(...))
          colorscale = list(list(0, cat_colors[cat]), list(1, cat_colors[cat])),
          showscale  = FALSE,   # BUG 3 FIX: must be FALSE on every trace
          zmin = 1, zmax = 4,
          marker      = list(line = list(color = "white", width = 0.4)),
          name        = cat,
          legendgroup = cat,
          showlegend  = FALSE
        )
    }
    
    # --- Dummy scatter traces for clean categorical legend --------------------
    for (cat in cat_order) {
      fig <- fig %>%
        add_trace(
          type        = "scatter",
          x           = list(NULL), y = list(NULL),
          mode        = "markers",
          # BUG 4 FIX: symbol = "square" for filled legend swatches
          marker      = list(color = cat_colors[cat], size = 12, symbol = "square"),
          name        = cat,
          legendgroup = cat,
          showlegend  = TRUE
        )
    }
    
    fig %>%
      layout(
        title = list(
          text = paste0(
            "<b>When Does Heat-Stable Carbetocin Become Cost-Effective? (ICER < $100/DALY)</b>",
            "<br><sup>Countries coloured by urgency \u2014 dark teal = act now, ",
            "grey = not cost-effective under base case \u2014 Hover for detail</sup>"
          ),
          x = 0.5, font = list(size = 14)
        ),
        geo = list(
          showframe      = FALSE,
          showcoastlines = TRUE,
          projection     = list(type = "natural earth"),
          bgcolor        = "#f0f0f0"
        ),
        legend = list(
          title       = list(text = "Cost-Effectiveness Status"),
          orientation = "h",
          x = 0.5, xanchor = "center", y = -0.06,
          bgcolor     = "rgba(255,255,255,0.9)",
          bordercolor = "#dddddd", borderwidth = 1,
          font        = list(size = 12)
        ),
        margin        = list(l = 60, r = 60, t = 110, b = 90),
        paper_bgcolor = "#ffffff",
        hoverlabel    = list(
          bgcolor     = "#2c3e50",
          bordercolor = "#2c3e50",
          font        = list(size = 12, family = "Georgia", color = "white")
        )
      )
  })
  
} # end server
