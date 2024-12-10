##load libraries
library(shiny)
library(tidyverse)
library(pals)
library(shinythemes)
library(RColorBrewer)
library(markdown)
library(janitor)




##read in data
data <- read_csv('MCR_LTER_cleaned.csv')
line_plot_data <- read_csv("MCR_line_plot.csv")
fish_abund_data <- read_csv("fish_lollipop.csv")

##data wrangling

##output #1
#MCR_LTER_cleaned <- MCR_LTER %>%
#mutate(Habitat= fct_relevel(Habitat, "Fringing", "Backreef", "Outer 10", "Outer 17")) %>%
  # Group by location, habitat, and substrate type
#  group_by(Year, Site, Habitat, Taxonomy_Substrate_Functional_Group) %>%
  # Calculate mean percent cover across quadrats and transects
#  summarise(mean_percent_cover = mean(Percent_Cover, na.rm = TRUE)) %>%
  # Ungroup to remove grouping structure
#  ungroup()

##output #2
##data for change in just coral & algal turf by site over the sample period 
#MCR_line_plot <- MCR_LTER %>%
#  filter(Taxonomy_Substrate_Functional_Group == "Coral" |  #Taxonomy_Substrate_Functional_Group == "Algal Turf") %>% 
#  group_by(Year, Site, Taxonomy_Substrate_Functional_Group) %>%
#  summarise(mean_percent_cover = mean(Percent_Cover, na.rm = TRUE)) %>%
  # Ungroup to remove grouping structure
#  ungroup()

##output #3
##subset for just site 1 since all sites appear to follow relatively similar patterns and pivot longer
#fish_abund_1 <- fish_abund_data %>%
#  filter(Site == "LTER 1") %>%
#  pivot_longer(cols = Coral_Dweller:Territorial_Herbivore,
#               names_to = "Fish_group",
#               values_to = "Change")

##clean names in fish data set 
unique_fish_groups <- unique(fish_abund_data$Fish_group) ##pull out all unique values in Fish_group column

##remove underscores using str
clean_fish_groups <- str_replace_all(unique_fish_groups, "_", " ")

##update the Fish_group column with clean names
fish_abund_data$Fish_group <- factor(fish_abund_data$Fish_group, levels = unique_fish_groups, labels = clean_fish_groups)


##re-ordering habitat
data %>%
  mutate(Habitat= factor(Habitat, levels = c("Fringing", "Backreef", "Outer 10", "Outer 17")))




                               #####UI chunk######

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Long-Term Community Dynamics of Mo'orea, French Polynesia"),
  tabsetPanel(
    tabPanel("Introduction",
             tags$div(style = "margin: 20px;",
               markdown("### The study site
               For my final project I analyzed data from the public **Mo'orea Coral Reef Long-Term Ecological Research (MCR LTER)** data base.
               
                        - 6 sites (LTER 1-6)
                        - 4 Habitats: Fringing, back, forereef (10m), forereef (17m)
                        - 5 permanent 10 m long transects
                        
            Data sets I explored
            
                        1) Moorea Coral Reef LTER and R. Carpenter. 2023. MCR LTER: Coral Reef: Long-term Population and Community Dynamics:Benthic Algae and Other Community Components, ongoing since 2005 ver 36. Environmental Data Initiative.
                        2) Moorea Coral Reef LTER, T. Adam, A. Brooks, and P. Edmunds. 2016. MCR LTER: Coral Reef: Changes in the abundance of fish functional groups: Adam et al. 2014 Oecologia ver 2. Environmental Data Initiative.")
             ),
    imageOutput("static_figure"),
  ),
     tabPanel("Question 1",
              tags$p("How has the benthic community of Mo'orea has changed over the last two years? Has coral cover remained relatively consistent or has there been periods of loss and recovery?"),
             selectInput( ##select box for year
               inputId = "Year", 
               label = "Year:",
               choices = unique(data$Year)
             ), 
             selectInput( ##select box for site
               inputId= "Site",  
               label= "Site:", 
               choices=unique(data$Site)
             ), 
             plotOutput("barplot") ##create space for bar plot 
    ),
    tabPanel("Question 2",
             tags$p("How has hard coral cover and turf algae varied across the last two decades? Is there variability among sites and habitats?"),
             selectInput( ##select box for site
               inputId= "Site",  
               label= "LTER Site:", 
               choices=unique(line_plot_data$Site)
             ), 
             selectInput( ##check box for site
               inputId = "Habitat",
               label = "Habitat:",
               choices = unique(line_plot_data$Habitat),
             ),
             plotOutput("linePlot") ##create space for line plot
    ),
    tabPanel("Question 3",
             tags$p("How has the loss of coral cover and increase in turf algae impacted functional groups of fish on the north shore from 2007-2012?"),
             selectInput(        ##select box for year
               inputId = "Year", 
               label = "Year:",
               choices = unique(fish_abund_data$Year)),
             plotOutput("lollipop")
  ),
  tabPanel("Questions",
           tags$p("Thank you!"),
  imageOutput("static_figure2"))
))



##now server chunk
server<-function(input,output) {
  output$static_figure <- renderImage ({
    list(
      src = "sample_sites.png", ##name of my figure
      width = "80%", ##reduce size of image
      style = "display: block; margin-left: auto; margin-right: auto;" ##make it centered
      
    )
  }, deleteFile = FALSE)

  filtered_data <- reactive({ ##reactive= code will re-run when inputs (site, title) are changed
    data %>%
      filter(Year == input$Year, 
             Site == input$Site) ##data in plot must be for year and site selected in the select box
  })
  output$barplot <- renderPlot({ ##inserted "barplot" here to match above. Below is all my normal ggplot code
     substrate_colors <- pals::cols25(length(unique(filtered_data()$Taxonomy_Substrate_Functional_Group)))
    
     ggplot(filtered_data (), aes(x = Taxonomy_Substrate_Functional_Group, ##calling my data like a function 
                                 y = mean_percent_cover,
                                 fill = Taxonomy_Substrate_Functional_Group)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      facet_wrap(~ Habitat) +
      scale_fill_manual(values= substrate_colors)+
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=11),
            legend.position = "none",
            panel.grid.major.x = element_blank(),
            strip.text = element_text(face = "bold", size=14),
            axis.text.y = element_text(size = 12),
            axis.title.x= element_text(size = 15),
            axis.title.y= element_text(size = 15),
            plot.caption = element_text(size=10)) +
      labs(title = input$title,
           x = "Cover Type",
           y = "Percent Cover (%)",
           caption= "Long-term Population and Community Dynamics: Benthic Algae and Other Community Components, ongoing since 2005. Carpenter et al., 2023") 
  })
  
  line_data_filtered <- reactive({ 
    line_plot_data %>% 
      filter(Site == input$Site,
             Habitat ==input$Habitat)

})
  output$linePlot <- renderPlot ({
    ggplot(line_data_filtered(), aes(x = Year,
               y = mean_percent_cover,
               color = Taxonomy_Substrate_Functional_Group)) +
      geom_line(size=1.5) +
      scale_color_brewer(palette= "Set2") +
      facet_wrap(~ Taxonomy_Substrate_Functional_Group, ncol=1) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      theme_bw() +
      theme(strip.text = element_text(face = "bold", size=14),
            legend.position = "none",
            axis.text.x = element_text(size = 12),  
            axis.text.y = element_text(size = 12),,  
            axis.title.y = element_text(size = 15),
            plot.caption = element_text(size=10))+
      labs(x = "",
           y = "Percent Cover (%)",
           caption= "Long-term Population and Community Dynamics: Benthic Algae and Other Community Components, ongoing since 2005. Carpenter et al., 2023")
  })

  fish_abund_filtered <- reactive({ 
    fish_abund_data %>% 
      filter(Year == input$Year)
    
  })
  output$lollipop <- renderPlot ({
    ggplot(fish_abund_filtered (),aes(x = Change, 
               y = Fish_group)) +
      geom_segment(aes(x = 0, xend = Change, y = Fish_group, yend = Fish_group), 
                   color = "gray", size=0.8) +
      geom_point(aes(color = ifelse(Change < 0, "deepskyblue", "tomato1")), size= 4) +
      scale_color_manual(values = c("tomato1", "deepskyblue")) +
      theme_bw() +
      theme(legend.position = "none",
            plot.title = element_text(size = 16, hjust = 0.5),
            panel.grid.major.y = element_blank(),
            axis.text.x = element_text(size = 12),  
            axis.text.y = element_text(size = 12),  
            axis.title.x = element_text(size = 15),
            plot.caption = element_text(size=10)) +
      labs(
        x = "Change in Abundance",
        y = "",
        caption= "Coral Reef: Changes in the abundance of fish functional groups: Adam et al. 2014 Oecologia"
      )
    
  })
  
  output$static_figure2 <- renderImage ({
    list(
      src = "happy_coral.png", ##name of my figure
      width = "70%", ##reduce size of image
      style = "display: block; margin-left: auto; margin-right: auto;" ##make it centered
      
    )
  }, deleteFile = FALSE)
}

##can't forget this part or the run app button goes away
shinyApp(ui = ui, server = server)
