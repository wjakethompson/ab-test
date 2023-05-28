# input <- list(lb = 0.05, ub = 0.95,
#               a1_vis = 1000, a1_con = 700,
#               b1_vis =  700, b1_con = 600,
#               a2_vis =  600, a2_con = 300,
#               b2_vis =  500, b2_con = 175)

posteriorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    draws <- eventReactive(input$calculate, reactive({
      dat <- tibble::tibble(group = c("1", "1", "2", "2"),
                            cond = c("A", "B", "A", "B"),
                            vis = c(isolate(input$a1_vis), isolate(input$b1_vis),
                                    isolate(input$a2_vis), isolate(input$b2_vis)),
                            con = c(isolate(input$a1_con), isolate(input$b1_con),
                                    isolate(input$a2_con), isolate(input$b2_con)))
      beta_dist <- calc_beta(x1 = isolate(input$lb), p1 = 0.025,
                             x2 = isolate(input$ub), p2 = 0.975)
      logit_prior <- logit_beta(beta_dist$shape1, beta_dist$shape2)
      mod_prior <- rstanarm::normal(location = logit_prior$location,
                          scale = logit_prior$scale)
      
      # fit model -----
      full_dat <- dat |> 
        dplyr::mutate(full_group = paste0(.data$cond, "_", .data$group),
                      full_group = factor(.data$full_group, levels = c("A_1", "B_1", "A_2", "B_2")))
      full_mod <- rstanarm::stan_glm(cbind(con, vis - con) ~ 0 + full_group, data = full_dat,
                                     family = "binomial", iter = 4000, warmup = 2000,
                                     chains = 4, refresh = 0,
                                     prior = mod_prior)
      full_preds <- rstanarm::posterior_epred(full_mod,
                                              new_data = tibble::tibble(cond = c("A_1", "B_1",
                                                                                 "A_2", "B_2"))) |>
        tibble::as_tibble(.name_repair = ~c("A_1", "B_1", "A_2", "B_2"))
      
      marg_preds <- full_preds |> 
        tibble::rowid_to_column(var = "draw") |> 
        tidyr::pivot_longer(-"draw") |> 
        tidyr::separate(.data$name, c("cond", "group"), sep = "_") |> 
        dplyr::left_join(dat, by = c("group", "cond")) |> 
        dplyr::mutate(vis = dplyr::case_when(vis == 0L ~ 1L, TRUE ~ .data$vis))
      
      cond_preds <- marg_preds |> 
        dplyr::summarize(avg = weighted.mean(.data$value, .data$vis),
                         .by = c("draw", "cond")) |> 
        tidyr::pivot_wider(names_from = "cond", values_from = "avg") |> 
        dplyr::select(-"draw")
        
      group_preds <- marg_preds |> 
        dplyr::summarize(avg = weighted.mean(.data$value, .data$vis),
                         .by = c("draw", "group")) |> 
        tidyr::pivot_wider(names_from = "group", values_from = "avg") |> 
        dplyr::select(-"draw")
      
      # return -----
      list(cond_preds = cond_preds,
           group_preds = group_preds,
           full_preds = full_preds)
    }))
    
    output$marg_cond_posteriors <- renderPlot({
      draws_dat <- draws()
      draws_dat <- draws_dat()
      all_draws <- draws_dat$cond_preds|>
        tidyr::pivot_longer(dplyr::everything()) |> 
        dplyr::mutate(name = paste0("Condition ", .data$name))
      
      x_limits <- c(min(all_draws$value) - 0.1,
                    max(all_draws$value) + 0.1)
      
      mean_a <- mean(draws_dat$cond_preds$A)
      mean_b <- mean(draws_dat$cond_preds$B)
      
      subtitle <- if (mean_a > mean_b) {
        glue::glue("Condition A ({wjake::fmt_prop_pct(mean_a)}%) converted ",
             "<b style='color:#FED766'>{wjake::fmt_prop_pct((mean_a - mean_b) / mean_b)}% better",
             "</b> than Condition B ({wjake::fmt_prop_pct(mean_b)}%).")
      } else {
        glue::glue("Condition B ({wjake::fmt_prop_pct(mean_b)}%) converted ",
             "<b style='color:#009FB7'>{wjake::fmt_prop_pct((mean_b - mean_a) / mean_a)}% better",
             "</b> than Condition A ({wjake::fmt_prop_pct(mean_a)}%).")
      }
      
      showtext::showtext_begin()
      all_draws |>
        ggplot(aes(x = .data$value, y = forcats::fct_rev(.data$name), fill = .data$name)) +
        ggdist::stat_halfeye(show.legend = FALSE) +
        scale_fill_manual(values = c("Condition A" = wjake::palette_wjake[2],
                                     "Condition B" = wjake::palette_wjake[1])) +
        expand_limits(x = x_limits) +
        wjake::scale_x_percent() +
        labs(x = "Estimated Conversion Rate",
             y = NULL,
             subtitle = subtitle) +
        wjake::theme_wjake() +
        theme(axis.title.x = ggtext::element_markdown(margin = margin(10, 0, 0, 0)),
              axis.title.y = ggtext::element_markdown(margin = margin(0, 10, 0, 0)),
              plot.subtitle = ggtext::element_markdown()) -> p
      print(p)
      showtext::showtext_end()
    }, res = 100)
    
    output$marg_cond_contrast <- renderPlot({
      draws_dat <- draws()
      draws_dat <- draws_dat()
      draws_contrast <- draws_dat$cond_preds |>
        dplyr::mutate(contrast = .data$A - .data$B)
      
      a_best <- mean(draws_contrast$contrast > 0)
      
      subtitle <- if (a_best > 0.5) {
        glue::glue("I'm {wjake::fmt_prop_pct(a_best)}% sure that ",
             "<b style='color:#FED766'>Condition A</b> ",
             "has a better conversation rate.")
      } else {
        glue::glue("I'm {wjake::fmt_prop_pct(1 - a_best)}% sure that ",
             "<b style='color:#009FB7'>Condition B</b> ",
             "has a better conversation rate.")
      }
      
      showtext::showtext_begin()
      draws_contrast |>
        ggplot(aes(x = .data$contrast, fill = after_stat(.data$x > 0))) +
        ggdist::stat_halfeye(show.legend = FALSE) +
        scale_fill_manual(values = c("TRUE" = wjake::palette_wjake[2],
                                     "FALSE" = wjake::palette_wjake[1])) +
        labs(x = "Difference (A &minus; B)",
             y = "Density",
             subtitle = subtitle) +
        wjake::theme_wjake() +
        theme(axis.title.x = ggtext::element_markdown(margin = margin(10, 0, 0, 0)),
              axis.title.y = ggtext::element_markdown(margin = margin(0, 10, 0, 0)),
              plot.subtitle = ggtext::element_markdown()) -> c
      print(c)
      showtext::showtext_end()
    }, res = 100)
    
    output$marg_group_posteriors <- renderPlot({
      draws_dat <- draws()
      draws_dat <- draws_dat()
      all_draws <- draws_dat$group_preds |>
        tidyr::pivot_longer(dplyr::everything()) |> 
        dplyr::mutate(name = paste0("Group ", .data$name))
      
      x_limits <- c(min(all_draws$value) - 0.1,
                    max(all_draws$value) + 0.1)
      
      mean_1 <- mean(draws_dat$group_preds$`1`)
      mean_2 <- mean(draws_dat$group_preds$`2`)
      
      subtitle <- if (mean_1 > mean_2) {
        glue::glue("Group 1 ({wjake::fmt_prop_pct(mean_1)}%) converted ",
             "<b style='color:#FED766'>{wjake::fmt_prop_pct((mean_1 - mean_2) / mean_2)}% better",
             "</b> than Group 2 ({wjake::fmt_prop_pct(mean_2)}%).")
      } else {
        glue::glue("Group 2 ({wjake::fmt_prop_pct(mean_2)}%) converted ",
             "<b style='color:#009FB7'>{wjake::fmt_prop_pct((mean_2 - mean_1) / mean_1)}% better",
             "</b> than Group 1 ({wjake::fmt_prop_pct(mean_1)}%).")
      }
      
      showtext::showtext_begin()
      all_draws |>
        ggplot(aes(x = .data$value, y = forcats::fct_rev(.data$name), fill = .data$name)) +
        ggdist::stat_halfeye(show.legend = FALSE) +
        scale_fill_manual(values = c("Group 1" = wjake::palette_wjake[2],
                                     "Group 2" = wjake::palette_wjake[1])) +
        expand_limits(x = x_limits) +
        wjake::scale_x_percent() +
        labs(x = "Estimated Conversion Rate",
             y = NULL,
             subtitle = subtitle) +
        wjake::theme_wjake() +
        theme(axis.title.x = ggtext::element_markdown(margin = margin(10, 0, 0, 0)),
              axis.title.y = ggtext::element_markdown(margin = margin(0, 10, 0, 0)),
              plot.subtitle = ggtext::element_markdown()) -> p
      print(p)
      showtext::showtext_end()
    }, res = 100)
    
    output$marg_group_contrast <- renderPlot({
      draws_dat <- draws()
      draws_dat <- draws_dat()
      draws_contrast <- draws_dat$group_preds |>
        dplyr::mutate(contrast = .data$`1` - .data$`2`)
      
      a_best <- mean(draws_contrast$contrast > 0)
      
      subtitle <- if (a_best > 0.5) {
        glue::glue("I'm {wjake::fmt_prop_pct(a_best)}% sure that ",
             "<b style='color:#FED766'>Group 1</b> ",
             "has a better conversation rate.")
      } else {
        glue::glue("I'm {wjake::fmt_prop_pct(1 - a_best)}% sure that ",
             "<b style='color:#009FB7'>Group 2</b> ",
             "has a better conversation rate.")
      }
      
      showtext::showtext_begin()
      draws_contrast |>
        ggplot(aes(x = .data$contrast, fill = after_stat(.data$x > 0))) +
        ggdist::stat_halfeye(show.legend = FALSE) +
        scale_fill_manual(values = c("TRUE" = wjake::palette_wjake[2],
                                     "FALSE" = wjake::palette_wjake[1])) +
        labs(x = "Difference (A &minus; B)",
             y = "Density",
             subtitle = subtitle) +
        wjake::theme_wjake() +
        theme(axis.title.x = ggtext::element_markdown(margin = margin(10, 0, 0, 0)),
              axis.title.y = ggtext::element_markdown(margin = margin(0, 10, 0, 0)),
              plot.subtitle = ggtext::element_markdown()) -> c
      print(c)
      showtext::showtext_end()
    }, res = 100)
    
    output$conditional_posteriors <- renderPlot({
      draws_dat <- draws()
      draws_dat <- draws_dat()
      all_draws <- draws_dat$full_preds |>
        tidyr::pivot_longer(dplyr::everything()) |> 
        tidyr::separate(.data$name, c("cond", "grp"), sep = "_") |> 
        dplyr::mutate(name = paste0("Group ", .data$grp, ", Condition ", .data$cond),
               cond = paste0("Condition ", .data$cond))
      
      x_limits <- c(min(all_draws$value) - 0.1,
                    max(all_draws$value) + 0.1)
      
      mean_1a <- mean(draws_dat$full_preds$A_1)
      mean_1b <- mean(draws_dat$full_preds$B_1)
      mean_2a <- mean(draws_dat$full_preds$A_2)
      mean_2b <- mean(draws_dat$full_preds$B_2)
      
      subtitle_1 <- if (mean_1a > mean_1b) {
        glue::glue("For Group 1, Condition A ({wjake::fmt_prop_pct(mean_1a)}%) converted ",
             "<b style='color:#FED766'>{wjake::fmt_prop_pct((mean_1a - mean_1b) / mean_1b)}% better</b> ",
             "<br>than Condition B ({wjake::fmt_prop_pct(mean_1b)}%).")
      } else {
        glue::glue("For Group 1, Condition B ({wjake::fmt_prop_pct(mean_1b)}%) converted ",
             "<b style='color:#009FB7'>{wjake::fmt_prop_pct((mean_1b - mean_1a) / mean_1a)}% better</b> ",
             "<br>than Condition A ({wjake::fmt_prop_pct(mean_1a)}%).")
      }
      
      subtitle_2 <- if (mean_2a > mean_2b) {
        glue::glue("For Group 2, Condition A ({wjake::fmt_prop_pct(mean_2a)}%) converted ",
             "<b style='color:#FED766'>{wjake::fmt_prop_pct((mean_2a - mean_2b) / mean_2b)}% better</b> ",
             "<br>than Condition B ({wjake::fmt_prop_pct(mean_2b)}%).")
      } else {
        glue::glue("For Group 2, Condition B ({wjake::fmt_prop_pct(mean_2b)}%) converted ",
             "<b style='color:#009FB7'>{wjake::fmt_prop_pct((mean_2b - mean_2a) / mean_2a)}% better</b> ",
             "<br>than Condition A ({wjake::fmt_prop_pct(mean_2a)}%).")
      }
      
      all_draws <- all_draws |> 
        dplyr::mutate(grp = factor(.data$grp, levels = c("1", "2"),
                            labels = c(subtitle_1, subtitle_2)))
      
      showtext::showtext_begin()
      all_draws |>
        ggplot(aes(x = .data$value, y = forcats::fct_rev(.data$cond), fill = .data$cond)) +
        facet_wrap(~grp, ncol = 1) +
        ggdist::stat_halfeye(show.legend = FALSE) +
        scale_fill_manual(values = c("Condition A" = wjake::palette_wjake[2],
                                     "Condition B" = wjake::palette_wjake[1])) +
        expand_limits(x = x_limits) +
        wjake::scale_x_percent() +
        labs(x = "Estimated Conversion Rate",
             y = NULL) +
        wjake::theme_wjake() +
        theme(axis.title.x = ggtext::element_markdown(margin = margin(10, 0, 0, 0)),
              axis.title.y = ggtext::element_markdown(margin = margin(0, 10, 0, 0)),
              plot.subtitle = ggtext::element_markdown()) -> p
      print(p)
      showtext::showtext_end()
    }, res = 100)
    
    output$conditional_contrast <- renderPlot({
      draws_dat <- draws()
      draws_dat <- draws_dat()
      draws_contrast <- draws_dat$full_preds |>
        dplyr::mutate(contrast_1 = .data$A_1 - .data$B_1,
               contrast_2 = .data$A_2 - .data$B_2)
      
      a1_best <- mean(draws_contrast$contrast_1 > 0)
      a2_best <- mean(draws_contrast$contrast_2 > 0)
      
      subtitle_1 <- if (a1_best > 0.5) {
        glue::glue("I'm {wjake::fmt_prop_pct(a1_best)}% sure that ",
             "<b style='color:#FED766'>Condition A</b> ",
             "has a better conversation rate<br> for Group 1.")
      } else {
        glue::glue("I'm {wjake::fmt_prop_pct(1 - a1_best)}% sure that ",
             "<b style='color:#009FB7'>Condition B</b> ",
             "has a better conversation rate<br> for Group 1.")
      }
      
      subtitle_2 <- if (a2_best > 0.5) {
        glue::glue("I'm {wjake::fmt_prop_pct(a2_best)}% sure that ",
             "<b style='color:#FED766'>Condition A</b> ",
             "has a better conversation rate<br> for Group 2.")
      } else {
        glue::glue("I'm {wjake::fmt_prop_pct(1 - a2_best)}% sure that ",
             "<b style='color:#009FB7'>Condition B</b> ",
             "has a better conversation rate<br> for Group 2.")
      }
      
      draws_contrast <- draws_contrast |> 
        dplyr::select(.data$contrast_1, .data$contrast_2) |> 
        tidyr::pivot_longer(dplyr::everything()) |> 
        dplyr::mutate(name = factor(.data$name, levels = c("contrast_1", "contrast_2"),
                             labels = c(subtitle_1, subtitle_2)))
      
      showtext::showtext_begin()
      draws_contrast |>
        ggplot(aes(x = .data$value, fill = after_stat(.data$x > 0))) +
        facet_wrap(~name, ncol = 1) +
        ggdist::stat_halfeye(show.legend = FALSE) +
        scale_fill_manual(values = c("TRUE" = wjake::palette_wjake[2],
                                     "FALSE" = wjake::palette_wjake[1])) +
        labs(x = "Difference (A &minus; B)",
             y = "Density") +
        wjake::theme_wjake() +
        theme(axis.title.x = ggtext::element_markdown(margin = margin(10, 0, 0, 0)),
              axis.title.y = ggtext::element_markdown(margin = margin(0, 10, 0, 0)),
              plot.subtitle = ggtext::element_markdown()) -> c
      print(c)
      showtext::showtext_end()
    }, res = 100)
  })
}
