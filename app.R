library(shiny)
library(ggplot2)

theme_set(theme_classic())


# Auxiliary functions -----------------------------------------------------

dbern <- function(x, prob, ...) {
    dbinom(x = x, size = 1, prob = prob, ...)
}
pbern <- function(q, prob, ...) {
    pbinom(q = q, size = 1, prob = prob, ...)
}

plot_distribution <- function(dist, dist_args, range) {
    
    if (any(pdf_list$Discrete == gsub("^.", "d", dist))) {
        values <- range[1]:range[2]
        if (grepl("^d", dist)) {
            dist_args$x <- values
            tibble::tibble(x = values,
                           y = do.call(dist, dist_args)) |> 
                ggplot(aes(x, y)) +
                geom_col(fill = "#ff4d4d") +
                scale_x_continuous(breaks = values) + # , limits = range
                labs(y = "f(x)")
        } else {
            dist_args$q <- values
            tibble::tibble(x = values,
                           y = do.call(dist, dist_args)) |> 
                ggplot(aes(x, y)) +
                geom_step(color = "#4d79ff", size = 1.5) +
                scale_x_continuous(breaks = values, limits = range) +
                labs(y = "F(x)")
        }
    } else {
        values <- seq(from = range[1], to = range[2], length.out = 100)
        if (grepl("^d", dist)) {
            dist_args$x <- values
            color <- "#ff4d4d"
            ylab <- "f(x)"
        } else {
            dist_args$q <- values
            color <- "#4d79ff"
            ylab <- "F(x)"
        }
        tibble::tibble(x = values,
                       y = do.call(dist, dist_args)) |> 
            ggplot(aes(x, y)) +
            geom_line(color = color, size = 1.5) +
            scale_x_continuous(limits = range) +
            labs(y = ylab)
    }
}

pdf_list <- list(
    "Continuous" = c(
        "Beta" = "dbeta",
        "Cauchy" = "dcauchy",
        "Chi-squared" = "dchisq",
        "Exponential" = "dexp",
        "Fisher's F" = "df",
        "Gamma" = "dgamma",
        "Logistic" = "dlogis",
        "Log-normal" = "dlnorm",
        "Normal" = "dnorm",
        "Student's t" = "dt",
        "Uniform" = "dunif",
        "Weibull" = "dweibull"
    ),
    "Discrete" = c(
        "Bernoulli" = "dbern",
        "Binomial" = "dbinom",
        "Geometric" = "dgeom",
        # "Hypergeometric" = "dhyper",
        # "Multinomial" = "dmultinom",
        "Negative binomial" = "dnbinom",
        "Poisson" = "dpois"
    )
)

# App ---------------------------------------------------------------------

ui <- fluidPage(

    titlePanel("Probability Distributions"),

    sidebarLayout(
        sidebarPanel(
            selectInput("distrib", "Choose a distribution:", 
                        choices = pdf_list,
                        selected = "dt"),
            conditionalPanel(
                condition = "input.distrib == 'dbeta'",
                sliderInput("shape1",
                            "a",
                            min = 1,
                            max = 20,
                            value = 1),
                sliderInput("shape2",
                            "b",
                            min = 1,
                            max = 20,
                            value = 1),
                sliderInput("ncp_beta",
                            "Non-centrality parameter",
                            min = -10,
                            max = 10,
                            value = 0)
            ),
            conditionalPanel(
                condition = "input.distrib == 'dcauchy'",
                sliderInput("loc_cauchy",
                            "Location",
                            min = -20,
                            max = 20,
                            value = 0),
                sliderInput("scale_cauchy",
                            "Scale",
                            min = 1,
                            max = 10,
                            value = 1)
            ),
            conditionalPanel(
                condition = "input.distrib == 'dchisq'",
                sliderInput("ncp_chisq",
                            "Non-centrality parameter",
                            min = 1,
                            max = 20,
                            value = 1),
                sliderInput("df_chisq",
                            "Degrees of freedom",
                            min = 1,
                            max = 10,
                            value = 1)
            ),
            conditionalPanel(
                condition = "input.distrib == 'dexp'",
                sliderInput("rate_exp",
                            "Rate",
                            min = 0.01,
                            max = 20,
                            value = 1)
            ),
            conditionalPanel(
                condition = "input.distrib == 'df'",
                sliderInput("df1",
                            "Degrees of Freedom (1)",
                            min = 1,
                            max = 10,
                            value = 1),
                sliderInput("df2",
                            "Degrees of Freedom (2)",
                            min = 1,
                            max = 10,
                            value = 1),
                sliderInput("ncp_f",
                            "Non-centrality parameter",
                            min = -20,
                            max = 20,
                            value = 0)
            ),
            conditionalPanel(
                condition = "input.distrib == 'dgamma'",
                sliderInput("shape_gamma",
                            "Shape",
                            min = 0.01,
                            max = 10,
                            value = 1),
                sliderInput("rate_gamma",
                            "Rate",
                            min = 0.01,
                            max = 10,
                            value = 1)
            ),
            conditionalPanel(
                condition = "input.distrib == 'dlogis'",
                sliderInput("loc_logis",
                            "Location",
                            min = -20,
                            max = 20,
                            value = 0),
                sliderInput("scale_logis",
                            "Scale",
                            min = 1,
                            max = 10,
                            value = 1)
            ),
            conditionalPanel(
                condition = "input.distrib.includes('norm')",
                sliderInput("mean",
                            "Mean",
                            min = -20,
                            max = 20,
                            value = 0),
                sliderInput("sd",
                            "Standard deviation",
                            min = 0.001,
                            max = 10,
                            value = 1)
            ),
            conditionalPanel(
                condition = "input.distrib == 'dt'",
                sliderInput("df_t",
                            "Degrees of freedom",
                            min = 1,
                            max = 10,
                            value = 1),
                sliderInput("ncp_t",
                            "Non-centrality parameter",
                            min = -20,
                            max = 20,
                            value = 0)
            ),
            conditionalPanel(
                condition = "input.distrib == 'dunif'",
                sliderInput("min",
                            "Minimum",
                            min = -20,
                            max = 20,
                            value = 0),
                sliderInput("max",
                            "Maximum",
                            min = -20,
                            max = 20,
                            value = 1)
            ),
            conditionalPanel(
                condition = "input.distrib == 'dweibull'",
                sliderInput("shape_weib",
                            "Shape",
                            min = 0.01,
                            max = 10,
                            value = 1),
                sliderInput("scale_weib",
                            "Scale",
                            min = 0.01,
                            max = 10,
                            value = 1)
            ),
            conditionalPanel(
                condition = "input.distrib == 'dbern'",
                sliderInput("prob_bern",
                            "Probability",
                            min = 0,
                            max = 1,
                            value = 0.5)
            ),
            conditionalPanel(
                condition = "input.distrib == 'dbinom'",
                sliderInput("size_binom",
                            "Size",
                            min = 1,
                            max = 20,
                            value = 5),
                sliderInput("prob_binom",
                            "Probability",
                            min = 0,
                            max = 1,
                            value = 0.5)
            ),
            conditionalPanel(
                condition = "input.distrib == 'dgeom'",
                sliderInput("prob_geom",
                            "Probability",
                            min = 0,
                            max = 1,
                            value = 0.5)
            ),
            conditionalPanel(
                condition = "input.distrib == 'dnbinom'",
                sliderInput("size_nbinom",
                            "Size",
                            min = 1,
                            max = 20,
                            value = 5),
                sliderInput("prob_nbinom",
                            "Probability",
                            min = 0,
                            max = 1,
                            value = 0.5)
            ),
            conditionalPanel(
                condition = "input.distrib == 'dpois'",
                sliderInput("lambda",
                            "Lambda",
                            min = 0,
                            max = 20,
                            value = 1)
            ),
        ),
        mainPanel(
           plotOutput("pdf"),
           plotOutput("cdf")
        )
    )
)

server <- function(input, output) {
    get_args <- function(distribution) {
        switch (distribution,
                dbeta = list(shape1 = input$shape1, 
                             shape2 = input$shape2, 
                             ncp = input$ncp_beta),
                dcauchy = list(location = input$loc_cauchy, 
                               scale = input$scale_cauchy),
                dchisq = list(df = input$df_chisq, 
                              ncp = input$ncp_chisq),
                dexp = list(rate = input$rate_exp),
                df = list(df1 = input$df1, 
                          df2 = input$df2, 
                          ncp = input$ncp_f),
                dgamma = list(shape = input$shape_gamma, 
                              rate = input$rate_gamma),
                dlogis = list(location = input$loc_logis, 
                              scale = input$scale_logis),
                dlnorm = list(meanlog = input$mean, 
                              sdlog = input$sd),
                dnorm = list(mean = input$mean, 
                             sd = input$sd),
                dt = list(df = input$df_t, 
                          ncp = input$ncp_t),
                dunif = list(min = input$min, 
                             max = input$max),
                dweibull = list(shape = input$shape_weib, 
                                scale = input$scale_weib),
                dbern = list(prob = input$prob_bern),
                dbinom = list(size = input$size_binom, 
                              prob = input$prob_binom),
                dgeom = list(prob = input$prob_geom),
                dhyper = list(),
                dnbinom = list(size = input$size_nbinom, 
                               prob = input$prob_nbinom),
                dpois = list(lambda = input$lambda)
        )
    }
    
    args <- reactive(get_args(input$distrib))

    output$pdf <- renderPlot({
        plot_distribution(dist = input$distrib,
                          dist_args = args(),
                          range = c(-10, 10))
    })
    
    output$cdf <- renderPlot({
        plot_distribution(dist = gsub("^.", "p", input$distrib),
                          dist_args = args(),
                          range = c(-10, 10))
    })
}

shinyApp(ui = ui, server = server)
