library(shiny)

shinyUI(
  fluidPage(
    withMathJax(),
    # section below allows in-line LaTeX via $ in mathjax
    tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$']]}
                });
                </script>
                ")),
    title = "Introduction to Evolutionary Quantitative Genetics",
    titlePanel(
      h6(
        "Introduction to Evolutionary Quantitative Genetics - Genotype $\\times$ Environment interactions"
      )
    ),
    sidebarLayout(
      position = "left",
      sidebarPanel(
        h3("Parameters"),
        width = 3,
        helpText("Use the sliders to change the parameters:"),
        sliderInput(
          "N",
          label = "Number of indivuals",
          min = 10,
          max = 1000,
          value = 100,
          step = 1
        ),
        sliderInput(
          "n",
          label = "Number of measurements per environment",
          min = 1,
          max = 25,
          value = 5,
          step = 1
        ),
        HTML("</br>"),
        sliderInput(
          "mu.1",
          label = "Mean phenotype in environment 1",
          min = 0,
          max = 10,
          value = 1,
          step = 0.1
        ),
        sliderInput(
          "mu.2",
          label = "Mean phenotype in environment 2",
          min = 0,
          max = 10,
          value = 2,
          step = 0.1
        ),
        HTML("</br>"),
        sliderInput(
          "VG.1",
          label = "Genetic variance in environment 1",
          min = 0,
          max = 10,
          value = 1,
          step = 0.1
        ),
        sliderInput(
          "VG.2",
          label = "Genetic variance in environment 2",
          min = 0,
          max = 10,
          value = 2,
          step = 0.1
        ),
        sliderInput(
          "rG.12",
          label = "Genetic correlation",
          min = -1,
          max = 1,
          value = 0.5,
          step = 0.1
        ),
        HTML("</br>"),
        sliderInput(
          "VE",
          label = "Environmental variance",
          min = 0,
          max = 10,
          value = 1,
          step = 0.1
        )
      ),
      
      mainPanel(
        h3("Genotype x Environment interactions", align = "center"),
        HTML(
          "<b>How does the expression of genotypic variation depend on the 
          environment, and how can we quantify these so-called 
          genotype-environment interactions, or G $\\times$ E?</b><br><br>"
          ),
        HTML("Here you will simulate phenotypic and genotypic 
          values in two environments and use two different formulations of a
          mixed model. The first estimates the variance in genotypic values ($V_{G}$) in 
          two discrete environments and their correlation across the two 
          environments. This is also called the <i>character-state</i> (CS) approach. 
          The second estimates the genetic variance in the intercept and slope 
          for the regression of genotypic values against a continuous 
          environmental variable. This is called the <i>reaction norm</i>
          (RN) approach.</br></br>"
             ),
          HTML("
          Although both approaches have there own strengths and weaknesses, you 
          will find that in the case of discrete environments, both approaches
          provide equivalent results.</br></br>"
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            HTML("</br>To keep things simple, you will here assume that the only source of individual 
              differences is genetic variation. In other words, there are no
              permanent environment, maternal, brood, litter, nest, etc. 
              effects. </br>"),
            HTML("</br>Because of these assumptions, you are able to estimate 
            genetic variances by applying a mixed model with <i>ID</i> as a 
            random effect to repeated measures data. This will capture systematic
            differences among individuals, which we asssume is the result of
            genetic variation. In biologically more realistic datasets,
              we would instead fit for example a <i>sire</i>, <i>nest of origin
              </i> or <i>animal</i> effect. </br></br>"),
            HTML("Below you see a plot of the simulated data. Each individual is 
                 depicted with a different colour. Individual mean phenotypes 
                 in the two environments are connected by a line in the same 
                 colour. Although you might want reduce the number of individuals
                 somewhat, you should be able to see that, with the default 
                 parameter settings, repeated measurements of the
                 same individual tend to be relatively similar, both within and 
                 across environments."),
            plotOutput("data.plot", height=600),
            HTML("<b>Exercise</b></br>"),
            HTML("Vary the parameters on the left, and see how this affects this
                 plot.</br></br></br>")
          ),
          
          tabPanel(
            "Character-state approach",
            
            HTML(
              "</br>The CS approach estimates the genetic variance in both 
              environments, as well as the correlation across environments. 
              It is particularly well-suited to a limited number of discrete
              environments, for example in an experimental setting. Similarly,
              this approach is useful when testing for genotype $\\times$ sex
              interactions.</br></br>"
            ),
            h4("Plots"),
            checkboxInput("hide.plots", label = "Hide plots", value=FALSE),
            conditionalPanel(
              condition = 'input["hide.plots"]==false',
            HTML("Below are some plots that visualise the main results for a
                 character-state type of analysis."),
            plotOutput("cs.plot.means", height = 400),
            HTML("This is a plot of the <i>mean</i> phenotype in both environments
                 and its standard error. All genotypic values in the plots below
                 are expressed relative to these environment-specific means."),
            plotOutput("cs.plot.var.cor", height = 400),
            HTML("The left figure is a plot of the distribution of the
                 genotypic values in both environments, and the scatter plot on
                 the right correlates environment-specific genotypic values."),
            
            plotOutput("cs.plot.lines", height = 400),
            HTML("This plot combines the two plots above into one: It plots the
                 genotypic values in each environment, and connects those 
                 belonging to the same individual with a line.</br></br></br>")
            ),
            
            h4("Model output"),
            checkboxInput("hide.output", label = "Hide output", value=FALSE),
            conditionalPanel(
              condition = 'input["hide.output"]==false',
              HTML("This is the output from the mixed model model:"),
            verbatimTextOutput("cs.lmer")),
            
            HTML("This is the $\\mathbf{G}$ matrix obtained from the variance and covariance
                 estimates provided by the mixed model. On the diagonal you find
                 the genetic variance in environments 1 and 2, respectively, and
                 on the off-diagonal you find the genetic covariance across the
                 two environments."),
            verbatimTextOutput("cs.G"),
            HTML("<b>Important</b></br>
            Note that output of the mixed model fitted using <code>lme4</code> 
            only provides one estimate for 
            the residual variance. In this case this is not a problem as
            we simulated the residual (i.e. environmental) variance to 
            be the same in both environments, and the residual correlation
            was assumed to be zero. In real-world data, this assumption is very
            like to be violated and we would need to model a so-called
            <i>heterogeneous</i> residual variance. Unfortunately, this
            cannot be implemented in <code>lme4</code>. However, other packages, 
            including <code>asreml-r</code> and <code>MCMCglmm</code>, allow you 
                 to do this.</br></br>")
            
          ),
          
          tabPanel(
            "Reaction norm approach",
            h4("Plots"),
              HTML(
              "</br>The RN approach regresses genotypic values against the 
              environmental values and subsequently estimates the variance in intercepts and 
              slopes. This approach is particularly well-suited when our environment
              is continuous, or we have a very large number of discrete 
              environments that can be ordered along a gradient. 
              Furthermore, a reaction norm approach may be most
              appropriate when testing for genotype $\\times$ age interactions 
              (if age is a continuous variable). However, note that this assumes 
              that the relationship between genotype (and phenotype) and the 
              environment can be described by a straight line.<br><br>"
            ),
            
            radioButtons(
              "intercept",
              label = "Rescale environment to set intercept to environment:",
              choices= c(0,1,2), selected = 0,
              inline = TRUE
            ),
            HTML("The variance in the reaction norm intercepts is the genotypic 
                 variance in environment 0. Because you coded your environments as
                 1 and 2, the variance in the intercepts is for an environment that doesn't
                 exist. Therefore you might want to rescale your environment in 
                 such a way that the intercepts gives you the variance in either 
                 environment 1 (by subtracting 1) or 2 (by subtracting 2)."),
            
            checkboxInput("hide.plots.2", label = "Hide plots", value=FALSE),
            conditionalPanel(
              condition = 'input["hide.plots.2"]==false',
              HTML("This is a plot of the <i>mean</i> reaction norm."),
            plotOutput("rn.plot.reg", height = 400),
            HTML("On the left, a plot depicting the genotypic variance in 
                 reaction norm intercepts and slopes. On the right, a plot of the slope 
                 of each reaction norm against its intercept."),
            plotOutput("rn.plot.var.cor", height = 400),
            plotOutput("rn.plot.lines", height = 400)),
           
            h4("Model output"),
            checkboxInput("hide.output.2", label = "Hide output", value=FALSE),
            conditionalPanel(
              condition = 'input["hide.output.2"]==false',
              
                        verbatimTextOutput("rn.lmer")),
            HTML("By rescaling the environmental variable, you can obtain the
            genotypic variance in either of the two environments, and this should
            provide you with the same variances as provided by the character-state
            approach.<br><br>"),
            HTML("However, it is also possible to calculate the variance in
           any environment from from the variance in the intercept and slope.<br><br>"),
            HTML("It turns out that the genotypic variance in environment 1
                 is given by<br>"),
            HTML("$$\\sigma^2_1 = \\sigma^2_{a,a} + x^2_1 \\sigma^2_{b,b} + 2x_1 \\sigma_{a,b}$$<br>"),
            HTML("where $a$ is the intercept and $b$ is the slope.<br>"),
            HTML("Similarly, the variance in environment 2 is given by:"),
            HTML("$$\\sigma^2_2 = \\sigma^2_{a,a} + x^2_2 \\sigma^2_{b,b} + 2x_2 \\sigma_{a,b}$$<br>"),
            
            HTML("And the covariance is given by:<br>"),
            
            HTML("$$\\sigma_{12} = \\sigma^2_{a,a} + x_1 x_2 \\sigma^2_{b,b} + (x_1 +x_2) \\sigma_{a,b}$$<br>"),
            HTML("Indeed, if you perform these calculations you do indeed retrieve the same $\\mathbf{G}$ matrix
                 as you obtained using the CS approach, but only if you don't rescale the environmental variable:"),
            verbatimTextOutput("cs.G2"),
            HTML("Do you find the same?")
            
          )
        ),
        HTML("</br><p style=\"font-size:10px; text-align:right\">Written by Erik Postma | e.postma@exeter.ac.uk</p>")

        
      )
    )
    
  )
)
