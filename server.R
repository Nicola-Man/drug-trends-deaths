#For ABS COD 2018 data received in Sept 2019
#N. Man
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(plotly)
library(shinycustomloader)
#for drug type plots
#https://stackoverflow.com/questions/47062532/multiple-lines-for-text-per-legend-label-in-ggplot2
library(stringr)

#reading RDS file format is apparently more efficient
#https://appsilon.com/fast-data-loading-from-files-to-r/
df <- readRDS("ABS_COD2018_All.rds")

agecodcols <- c(
  "15-24" = "#ff0000",
  "25-34" = "#aa0055",
  "35-44" = "#5500aa",
  "45-54" = "#0000ff",
  "55-64" = "#00aa80",
  "65-74" = "#00ff00",
  "75-84" = "#ffcc00",
  "All ages" = "#000000",
  "15-64" = "gray50",

  "All" = "#000000",
  "15-24 All" = "#ff0000",
  "25-34 All" = "#aa0055",
  "35-44 All" = "#5500aa",
  "45-54 All" = "#0000ff",
  "55-64 All" = "#00aa80",
  "65-74 All" = "#00ff00",
  "75-84 All" = "#ffcc00",
  "All ages All" = "#000000",
  "15-64 All" = "gray50",

  "15-24 Male" = "#ff0000",
  "25-34 Male" = "#aa0055",
  "35-44 Male" = "#5500aa",
  "45-54 Male" = "#0000ff",
  "55-64 Male" = "#00aa80",
  "65-74 Male" = "#00ff00",
  "75-84 Male" = "#ffcc00",
  "All ages Male" = "#000000",
  "15-64 Male" = "808080",
  
  "15-24 Female" = "#ff0000",
  "25-34 Female" = "#aa0055",
  "35-44 Female" = "#5500aa",
  "45-54 Female" = "#0000ff",
  "55-64 Female" = "#00aa80",
  "65-74 Female" = "#00ff00",
  "75-84 Female" = "#ffcc00",
  "All ages Female" = "#000000",
  "15-64 Female" = "808080",
  
  "Accidental" = "#000000",
  "15-24 Accidental" = "#ff0000",
  "25-34 Accidental" = "#aa0055",
  "35-44 Accidental" = "#5500aa",
  "45-54 Accidental" = "#0000ff",
  "55-64 Accidental" = "#00aa80",
  "65-74 Accidental" = "#00ff00",
  "75-84 Accidental" = "#ffcc00",
  "All ages Accidental" = "#000000",
  "15-64 Accidental" = "#808080",
  
  "Intentional" = "#000000",
  "15-24 Intentional" = "#ff0000",
  "25-34 Intentional" = "#aa0055",
  "35-44 Intentional" = "#5500aa",
  "45-54 Intentional" = "#0000ff",
  "55-64 Intentional" = "#00aa80",
  "65-74 Intentional" = "#00ff00",
  "75-84 Intentional" = "#ffcc00",
  "All ages Intentional" = "#000000",
  "15-64 Intentional" = "#808080",
  
  "Undetermined" = "#000000",
  "15-24 Undetermined" = "#ff0000",
  "25-34 Undetermined" = "#aa0055",
  "35-44 Undetermined" = "#5500aa",
  "45-54 Undetermined" = "#0000ff",
  "55-64 Undetermined" = "#00aa80",
  "65-74 Undetermined" = "#00ff00",
  "75-84 Undetermined" = "#ffcc00",
  "All ages Undetermined" = "#000000",
  "15-64 Undetermined" = "#808080",
  
  "Other" = "#000000",
  "15-24 Other" = "#ff0000",
  "25-34 Other" = "#aa0055",
  "35-44 Other" = "#5500aa",
  "45-54 Other" = "#0000ff",
  "55-64 Other" = "#00aa80",
  "65-74 Other" = "#00ff00",
  "75-84 Other" = "#ffcc00",
  "All ages Other" = "#000000",
  "15-64 Other" = "#808080"
)

agecodtype <- c(
  "All" = 1,
  "15-24 All" = 1,
  "25-34 All" = 1,
  "35-44 All" = 1,
  "45-54 All" = 1,
  "55-64 All" = 1,
  "65-74 All" = 1,
  "75-84 All" = 1,
  "All ages All" = 1,
  "15-64 All" = 1,
  
  "15-24 Male" = 2,
  "25-34 Male" = 2,
  "35-44 Male" = 2,
  "45-54 Male" = 2,
  "55-64 Male" = 2,
  "65-74 Male" = 2,
  "75-84 Male" = 2,
  "All ages Male" = 2,
  "15-64 Male" = 2,
  
  "15-24 Female" = 3,
  "25-34 Female" = 3,
  "35-44 Female" = 3,
  "45-54 Female" = 3,
  "55-64 Female" = 3,
  "65-74 Female" = 3,
  "75-84 Female" = 3,
  "All ages Female" = 3,
  "15-64 Female" = 3,
  
  "Accidental" = 2,
  "15-24 Accidental" = 2,
  "25-34 Accidental" = 2,
  "35-44 Accidental" = 2,
  "45-54 Accidental" = 2,
  "55-64 Accidental" = 2,
  "65-74 Accidental" = 2,
  "75-84 Accidental" = 2,
  "All ages Accidental" = 2,
  "15-64 Accidental" = 2,

  "Intentional" = 3,
  "15-24 Intentional" = 3,
  "25-34 Intentional" = 3,
  "35-44 Intentional" = 3,
  "45-54 Intentional" = 3,
  "55-64 Intentional" = 3,
  "65-74 Intentional" = 3,
  "75-84 Intentional" = 3,
  "All ages Intentional" = 3,
  "15-64 Intentional" = 3,

  "Undetermined" = 4,
  "15-24 Undetermined" = 4,
  "25-34 Undetermined" = 4,
  "35-44 Undetermined" = 4,
  "45-54 Undetermined" = 4,
  "55-64 Undetermined" = 4,
  "65-74 Undetermined" = 4,
  "75-84 Undetermined" = 4,
  "All ages Undetermined" = 4,
  "15-64 Undetermined" = 4,
  
  "15-24 Other" = 5,
  "25-34 Other" = 5,
  "35-44 Other" = 5,
  "45-54 Other" = 5,
  "55-64 Other" = 5,
  "65-74 Other" = 5,
  "75-84 Other" = 5,
  "All ages Other" = 5,
  "15-64 Other" = 5
)

codtype <- c(
  "All" = 1,
  "Accidental" = 2,
  "Intentional" = 3,
  "Undetermined" = 4,
  "Other" = 5
)

regcols <- c(
  "Major Cities" = "red",
  "Inner Regional" = "purple",
  "Outer Regional" = "blue",
  "Remote and Very Remote" = "forestgreen",
  "Regional and Remote" = "orange"
)

regcodcols <- c(
  "Major Cities,All" = "red",
  "Inner Regional,All" = "purple",
  "Outer Regional,All" = "blue",
  "Remote and Very Remote,All" = "forestgreen",
  "Regional and Remote,All" = "orange",

  "Major Cities,Accidental" = "red",
  "Inner Regional,Accidental" = "purple",
  "Outer Regional,Accidental" = "blue",
  "Remote and Very Remote,Accidental" = "forestgreen",
  "Regional and Remote,Accidental" = "orange"
)

regcodtype <- c(
  "Major Cities,All" = 1,
  "Inner Regional,All" = 1,
  "Outer Regional,All" = 1,
  "Remote and Very Remote,All" = 1,
  "Regional and Remote,All" = 1,
  
  "Major Cities,Accidental" = 2,
  "Inner Regional,Accidental" = 2,
  "Outer Regional,Accidental" = 2,
  "Remote and Very Remote,Accidental" = 2,
  "Regional and Remote,Accidental" = 2
)

sextype <- c(
  "All" = 1,
  "Male" = 2,
  "Female" = 3
)

sexcodtype <- c(
  "All All" = 1,
  "All Accidental" = 2,
  "All Intentional" = 3,
  "All Undetermined" = 4,
  "Male All" = 1,
  "Male Accidental" = 2,
  "Male Intentional" = 3,
  "Male Undetermined" = 4,
  "Female All" = 1,
  "Female Accidental" = 2,
  "Female Intentional" = 3,
  "Female Undetermined" = 4
)

sexcols <- c(
  "All" = "#000000",
  "All All" = "#000000",
  "All Accidental" = "#000000",
  "All Intentional" = "#000000",
  "All Undetermined" = "#000000",
  "Male" = "#0000ff",
  "Male All" = "#0000ff",
  "Male Accidental" = "#0000ff",
  "Male Intentional" = "#0000ff",
  "Male Undetermined" = "#0000ff",
  "Female" = "#ff0000",
  "Female All" = "#ff0000",
  "Female Accidental" = "#ff0000",
  "Female Intentional" = "#ff0000",
  "Female Undetermined" = "#ff0000"
)

opcols <- c(
  "Exclusive illicit opioids"="orange",
  "Exclusive pharmaceutical opioids"="#e34a33", #red
  "Illicit & pharmaceutical opioids"="#3300dd", #blue
  "Other & unspecified opioids"="#00bb33") #green

opwcodcols <- c(
  "Alcohol"="purple",
  "Amphetamines"="hotpink",
  "Antidepressants"="orange",
  "Antipsychotics & neuroleptics"="blue",
  "Benzodiazepines"="chartreuse",
  "4-aminophenol derivatives"="cyan",
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified"="forestgreen",

  "Alcohol,All"="purple",
  "Amphetamines,All"="hotpink",
  "Antidepressants,All"="orange",
  "Antipsychotics & neuroleptics,All"="blue",
  "Benzodiazepines,All"="chartreuse",
  "4-aminophenol derivatives,All"="cyan",
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,All"="forestgreen",
  
  "Alcohol,Female"="purple",
  "Amphetamines,Female"="hotpink",
  "Antidepressants,Female"="orange",
  "Antipsychotics & neuroleptics,Female"="blue",
  "Benzodiazepines,Female"="chartreuse",
  "4-aminophenol derivatives,Female"="cyan",
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Female"="forestgreen",

  "Alcohol,Male"="purple",
  "Amphetamines,Male"="hotpink",
  "Antidepressants,Male"="orange",
  "Antipsychotics & neuroleptics,Male"="blue",
  "Benzodiazepines,Male"="chartreuse",
  "4-aminophenol derivatives,Male"="cyan",
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Male"="forestgreen",
  
  "Alcohol,Accidental"="purple",
  "Amphetamines,Accidental"="hotpink",
  "Antidepressants,Accidental"="orange",
  "Antipsychotics & neuroleptics,Accidental"="blue",
  "Benzodiazepines,Accidental"="chartreuse",
  "4-aminophenol derivatives,Accidental"="cyan",
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Accidental"="forestgreen",

  "Alcohol,Intentional"="purple",
  "Amphetamines,Intentional"="hotpink",
  "Antidepressants,Intentional"="orange",
  "Antipsychotics & neuroleptics,Intentional"="blue",
  "Benzodiazepines,Intentional"="chartreuse",
  "4-aminophenol derivatives,Intentional"="cyan",
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Intentional"="forestgreen",
  
  "Alcohol,Undetermined"="purple",
  "Antidepressants,Undetermined"="orange",
  "Antipsychotics & neuroleptics,Undetermined"="blue",
  "Benzodiazepines,Undetermined"="chartreuse",
  "4-aminophenol derivatives,Undetermined"="cyan")

opcodcols <- c("All opioids" = "#000000",
  "Heroin" = "orange",
  "Opium" = "brown",
  "Methadone" = "red",
  "Synthetic opioids" = "blue",
  "Natural & semi-synthetic opioids" = "purple",
  "Other & unspecified opioids"="#00bb33",
  
  "All opioids,All" = "#000000",
  "Heroin,All" = "orange",
  "Opium,All" = "brown",
  "Methadone,All" = "red",
  "Synthetic opioids,All" = "blue",
  "Natural & semi-synthetic opioids,All" = "purple",
  "Other & unspecified opioids,All"="#00bb33",
  
  "All opioids,Female" = "#000000",
  "Heroin,Female" = "orange",
  "Opium,Female" = "brown",
  "Methadone,Female" = "red",
  "Synthetic opioids,Female" = "blue",
  "Natural & semi-synthetic opioids,Female" = "purple",
  "Other & unspecified opioids,Female"="#00bb33",
  
  "All opioids,Male" = "#000000",
  "Heroin,Male" = "orange",
  "Opium,Male" = "brown",
  "Methadone,Male" = "red",
  "Synthetic opioids,Male" = "blue",
  "Natural & semi-synthetic opioids,Male" = "purple",
  "Other & unspecified opioids,Male"="#00bb33",
  
  "All opioids,Accidental" = "#000000",
  "Heroin,Accidental" = "orange",
  "Methadone,Accidental" = "red",
  "Synthetic opioids,Accidental" = "blue",
  "Natural & semi-synthetic opioids,Accidental" = "purple",
  "Other & unspecified opioids,Accidental"="#00bb33",
  
  "All opioids,Intentional" = "#000000",
  "Heroin,Intentional" = "orange",
  "Methadone,Intentional" = "red",
  "Synthetic opioids,Intentional" = "blue",
  "Natural & semi-synthetic opioids,Intentional" = "purple",

  "All opioids,Undetermined" = "#000000",
  "Heroin,Undetermined" = "orange",
  "Methadone,Undetermined" = "red",
  "Synthetic opioids,Undetermined" = "blue",
  "Natural & semi-synthetic opioids,Undetermined" = "purple",
  
  "Exclusive illicit opioids,All"="orange",
  "Exclusive pharmaceutical opioids,All"="#e34a33", #red
  "Illicit & pharmaceutical opioids,All"="#3300dd", #blue
  
  "Exclusive illicit opioids,Female"="orange",
  "Exclusive pharmaceutical opioids,Female"="#e34a33", #red
  "Illicit & pharmaceutical opioids,Female"="#3300dd", #blue
  
  "Exclusive illicit opioids,Male"="orange",
  "Exclusive pharmaceutical opioids,Male"="#e34a33", #red
  "Illicit & pharmaceutical opioids,Male"="#3300dd", #blue
  
  "Exclusive illicit opioids,Accidental"="orange",
  "Exclusive pharmaceutical opioids,Accidental"="#e34a33", #red
  "Illicit & pharmaceutical opioids,Accidental"="#3300dd", #blue
  
  "Exclusive illicit opioids,Intentional"="orange",
  "Exclusive pharmaceutical opioids,Intentional"="#e34a33", #red
  "Illicit & pharmaceutical opioids,Intentional"="#3300dd" #blue
#,"Other & unspecified opioids"="#00bb33" #green
  )

opwcodtype <- c("Alcohol"=1,
  "Amphetamines"=1,
  "Antidepressants"=1,
  "Antipsychotics & neuroleptics"=1,
  "Benzodiazepines"=1,
  "4-aminophenol derivatives"=1,
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified"=1,
  
  "Alcohol,All"=1,
  "Amphetamines,All"=1,
  "Antidepressants,All"=1,
  "Antipsychotics & neuroleptics,All"=1,
  "Benzodiazepines,All"=1,
  "4-aminophenol derivatives,All"=1,
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,All"=1,
  
  "Alcohol,Female"=2,
  "Amphetamines,Female"=2,
  "Antidepressants,Female"=2,
  "Antipsychotics & neuroleptics,Female"=2,
  "Benzodiazepines,Female"=2,
  "4-aminophenol derivatives,Female"=2,
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Female"=2,
  
  "Alcohol,Male"=3,
  "Amphetamines,Male"=3,
  "Antidepressants,Male"=3,
  "Antipsychotics & neuroleptics,Male"=3,
  "Benzodiazepines,Male"=3,
  "4-aminophenol derivatives,Male"=3,
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Male"=3,

  "Alcohol,Accidental"=2,
  "Amphetamines,Accidental"=2,
  "Antidepressants,Accidental"=2,
  "Antipsychotics & neuroleptics,Accidental"=2,
  "Benzodiazepines,Accidental"=2,
  "4-aminophenol derivatives,Accidental"=2,
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Accidental"=2,
  
  "Alcohol,Intentional"=3,
  "Amphetamines,Intentional"=3,
  "Antidepressants,Intentional"=3,
  "Antipsychotics & neuroleptics,Intentional"=3,
  "Benzodiazepines,Intentional"=3,
  "4-aminophenol derivatives,Intentional"=3,
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Intentional"=3,
  
  "Alcohol,Undetermined"=4,
  "Antidepressants,Undetermined"=4,
  "Antipsychotics & neuroleptics,Undetermined"=4,
  "Benzodiazepines,Undetermined"=4,
  "4-aminophenol derivatives,Undetermined"=4)

opcodtype <- c("All opioids,All" = 1,
  "Heroin,All" = 1,
  "Opium,All" = 1,
  "Methadone,All" = 1,
  "Synthetic opioids,All" = 1,
  "Natural & semi-synthetic opioids,All" = 1,
  "Other & unspecified opioids,All" = 1,
  
  "All opioids,Female" = 2,
  "Heroin,Female" = 2,
  "Opium,Female" = 2,
  "Methadone,Female" = 2,
  "Synthetic opioids,Female" = 2,
  "Natural & semi-synthetic opioids,Female" = 2,
  "Other & unspecified opioids,Female" = 2,
  
  "All opioids,Male" = 3,
  "Heroin,Male" = 3,
  "Opium,Male" = 3,
  "Methadone,Male" = 3,
  "Synthetic opioids,Male" = 3,
  "Natural & semi-synthetic opioids,Male" = 3,
  "Other & unspecified opioids,Male" = 3,
  
  "All opioids,Accidental" = 2,
  "Heroin,Accidental" = 2,
  "Methadone,Accidental" = 2,
  "Synthetic opioids,Accidental" = 2,
  "Natural & semi-synthetic opioids,Accidental" = 2,
  "Other & unspecified opioids,Accidental" = 2,
  
  "All opioids,Intentional" = 3,
  "Heroin,Intentional" = 3,
  "Methadone,Intentional" = 3,
  "Synthetic opioids,Intentional" = 3,
  "Natural & semi-synthetic opioids,Intentional" = 3,

  "All opioids,Undetermined" = 4,
  "Heroin,Undetermined" = 4,
  "Methadone,Undetermined" = 4,
  "Synthetic opioids,Undetermined" = 4,
  "Natural & semi-synthetic opioids,Undetermined" = 4,

  "Exclusive illicit opioids,All"=1,
  "Exclusive pharmaceutical opioids,All"=1,
  "Illicit & pharmaceutical opioids,All"=1,
  
  "Exclusive illicit opioids,Female"=2,
  "Exclusive pharmaceutical opioids,Female"=2,
  "Illicit & pharmaceutical opioids,Female"=2,
  
  "Exclusive illicit opioids,Male"=3,
  "Exclusive pharmaceutical opioids,Male"=3,
  "Illicit & pharmaceutical opioids,Male"=3,
  
  "Exclusive illicit opioids,Accidental"=2,
  "Exclusive pharmaceutical opioids,Accidental"=2,
  "Illicit & pharmaceutical opioids,Accidental"=2,
  
  "Exclusive illicit opioids,Intentional"=3,
  "Exclusive pharmaceutical opioids,Intentional"=3,
  "Illicit & pharmaceutical opioids,Intentional"=3
)

dtcols <- c(
  "ALCOHOL"="purple",
  "OPIOIDS"="#000000",
  "heroin"="#000000",
  "natural & semi-synthetic opioids"="#000000",
  "methadone"="#000000",
  "synthetic opioids"="#000000",
  "ANTIDEPRESSANTS"="orange",
  "tricyclic & tetracyclic antidepressants"="orange",
  "other & unspecified antidepressants"="orange",
  "CANNABINOIDS"="forestgreen",
  "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS"="chartreuse",
  "barbiturates"="chartreuse",
  "benzodiazepines"="chartreuse",
  "antiepileptic & sedative-hypnotic drugs,\nunspecified"="chartreuse",
  "ANTIPSYCHOTICS & NEUROLEPTICS"="blue",
  "other & unspecified antipsychotics"="blue",
  "COCAINE"="brown",
  "NONOPIOID ANALGESICS"="cyan",
  "4-aminophenol derivatives"="cyan",
  "other nonsteroidal anti-inflammatory drugs"="cyan",
  "AMPHETAMINES"="hotpink"
)

dttype <- c(
  "ALCOHOL"=1,
  "OPIOIDS"=1,
  "heroin"=2,
  "natural & semi-synthetic opioids"=3,
  "methadone"=4,
  "synthetic opioids"=5,
  "ANTIDEPRESSANTS"=1,
  "tricyclic & tetracyclic antidepressants"=2,
  "other & unspecified antidepressants"=3,
  "CANNABINOIDS"=1,
  "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS"=1,
  "barbiturates"=2,
  "benzodiazepines"=3,
  "antiepileptic & sedative-hypnotic drugs,\nunspecified"=4,
  "ANTIPSYCHOTICS & NEUROLEPTICS"=1,
  "other & unspecified antipsychotics"=2,
  "COCAINE"=1,
  "NONOPIOID ANALGESICS"=1,
  "4-aminophenol derivatives"=2,
  "other nonsteroidal anti-inflammatory drugs"=3,
  "AMPHETAMINES"=1
)

# Allow for site's state to be bookmarked via the url
# See https://shiny.rstudio.com/articles/bookmarking-state.html for details
enableBookmarking("url")

server <- function(input, output, session) {

# Allow direct linking to specific tabs (with default configs)  
  observe({
# https://shiny.rstudio.com/reference/shiny/0.14/parseQueryString.html
    query <- parseQueryString(session$clientData$url_search)
# Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })

  # Amphetamine plot (Table 2) --------------------------------------------------------
  output$AmPlot <- renderPlotly({
    df_Stim <- readRDS("ABS_COD2018_Stim.rds")
    sub <- subset(df_Stim, subset = (jurisdiction == "Australia" & sex == "All" & 
           drug == "Amphetamines" & nature == "Underlying" & intent %in% input$Amcod & 
           age_group %in% input$Amage & (year >= input$Amyr[[1]] & year <= input$Amyr[[2]])))
    p <- ggplot(sub) + geom_line() + labs(x = "Year") +
      aes(x = year, group = 1) +
      scale_colour_manual(values = agecodcols) +
      scale_x_continuous(breaks = seq(input$Amyr[[1]],input$Amyr[[2]],2) )
    if (dim.data.frame(input$Amcod)[2]==1) {
      p <- p + aes(colour = age_group) +
        labs(title = paste0("Intent: ",input$Amcod) )
      Legend <- "Age"
    }
    else {
      p <- p + aes(colour = age_intent, linetype = age_intent) +
        scale_linetype_manual(values = agecodtype)
      Legend <- "Age by intent"
    }

    if (input$Amyax == "num") {
      p <- p + aes(y = n, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Intent: ", str_to_title(intent),
#          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits = c(0, max(sub$n, 250)))+
        labs(y = "Number of deaths")
    }
    
    else if (input$Amyax == "r5" | input$Amyax == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
#          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$Amyax == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$Amyax == "r6" | input$Amyax == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
#          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$Amyax == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))  })
  
  # Cocaine plot (Table 3) ------------------------------------------------------------
  output$CPlot <- renderPlotly({
    df_Stim <- readRDS("ABS_COD2018_Stim.rds")
    sub <- subset(df_Stim, subset = (drug == "Cocaine" & intent %in% input$Ccod & nature == "Underlying" &
                                       age_group %in% input$Cage & sex == "All" & jurisdiction == "Australia" &
                                       (year >= input$Cyr[[1]] & year <= input$Cyr[[2]])))
    
    p <- ggplot(sub) + aes(x = year, linetype=age_intent, colour=age_intent, group=1) +
      geom_line() + labs(x = "Year") +
      scale_linetype_manual(values = agecodtype) +
      scale_colour_manual(values = agecodcols) +
      scale_x_continuous(breaks = seq(input$Cyr[[1]],input$Cyr[[2]],2) )
    
    if (input$Cyax == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Intent: ", str_to_title(intent),
#        "<br>Nature: ", str_to_title(nature),
        "<br>Age group: ", age_group
      )
      ) +
        scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }
    
    else if (input$Cyax == "r5" | input$Cyax == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Intent: ", str_to_title(intent),
#        "<br>Nature: ", str_to_title(nature),
        "<br>Age group: ", age_group
      ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$Cyax == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$Cyax == "r6" | input$Cyax == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Intent: ", str_to_title(intent),
#        "<br>Nature: ", str_to_title(nature),
        "<br>Age group: ", age_group
      ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$Cyax == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = "Age by intent", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
  # All drug deaths by intent, jurisdiction and sex (Table 1a, 1b, 1c) -----------------------------------------------------------------
  output$allPlot <- renderPlotly({
    #Based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html  
    if (input$AllDrop == "Intent") {
      sub <- filter(df, age_group %in% input$Allage & jurisdiction == input$Alljur &
                                  intent == input$AllIcod & sex %in% input$AllIsex &
                                  (year >= input$Allyr[[1]] & year <= input$Allyr[[2]]) )
      p <- ggplot(sub) + aes(x = year) + geom_line() +
        scale_colour_manual(values = agecodcols) +
        scale_x_continuous(breaks = seq(input$Allyr[[1]],input$Allyr[[2]],2) ) #function(x) unique(floor(pretty(x,high.u.bias = 0,u5.bias = .1))))
      if (dim.data.frame(input$AllIsex)[2]==1) {
        p <- p + aes(colour = age_group, group = 1) + 
          labs(title=paste0(input$Alljur,", Intent: ",input$AllIcod,", Sex: ",input$AllIsex) )
        Legend <- "Age"
      }
      else {
        p <- p + aes(colour = age_sex, linetype = age_sex, group = 1) +
          scale_linetype_manual(values = agecodtype) +
          labs(title=paste0(input$Alljur,", Intent: ",input$AllIcod) )
        Legend <- "Age & sex"
      }
    }
    else if (input$AllDrop == "Sex") {
      if (input$AllSsex != "MF") {
        sub <- subset(df, subset = (age_group %in% input$Allage & jurisdiction == input$Alljur &
                                      intent %in% input$AllScod & sex == input$AllSsex &
                                      (year >= input$Allyr[[1]] & year <= input$Allyr[[2]]) ) )
        p <- ggplot(sub)
        Title <- paste0(input$Alljur,", Sex: ",input$AllSsex)
      }
      else {
        sub <- subset(df, subset = (age_group %in% input$Allage & jurisdiction == input$Alljur &
                                      intent %in% input$AllScod & sex != "All" &
                                      (year >= input$Allyr[[1]] & year <= input$Allyr[[2]]) ) )
        p <- ggplot(sub) + labs(title=input$Alljur) +
          facet_grid(cols = vars(sex))
        Title <- input$Alljur
      }

      if (dim.data.frame(input$AllScod)[2]==1) {
        p <- p + aes(colour = age_group, group = 1) + 
          scale_colour_manual(values = agecodcols) +
          labs(title=paste0(Title,", Intent: ",input$AllScod) )
        Legend <- "Age"
      }
      else if (dim.data.frame(input$Allage)[2]==1) {
        p <- p + aes(linetype = intent, group = 1) + 
          scale_linetype_manual(values = agecodtype) +
          labs(title=paste0(Title,", Age: ",input$Allage) )
        Legend <- "Intent"
      }
      else {
        p <- p + aes(colour = age_intent, linetype = age_intent, group = 1) + 
          scale_colour_manual(values = agecodcols) +
          scale_linetype_manual(values = agecodtype) +
          labs(title= Title )
        Legend <- "Age by intent"
      }
      p <- p + aes(x = year) + geom_line() +
        scale_x_continuous(breaks = seq(input$Allyr[[1]],input$Allyr[[2]],2))
    }
    
    if (input$Allyax == "num") {
      p <- p + aes(y = n, text = paste0(
              "Year: ", year,
              "<br>Deaths: ", n,
              "<br>Intent: ", str_to_title(intent),
#             "<br>Jurisdiction: ", location,
              "<br>Age: ", age_group,
              "<br>Sex: ", sex
            )
      ) + scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(x = "Year", y = "Number of deaths")
    }
    
    else if (input$Allyax == "r5" | input$Allyax == "r5ci") {
      p <- p + aes(y = rate_ht,
             text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
               "<br>Intent: ", str_to_title(intent), 
  #            "<br>Jurisdiction: ", location,
               "<br>Age: ", age_group,
               "<br>Sex: ", sex
             )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(x = "Year", y = "Deaths per 100,000")
      if (input$Allyax == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$Allyax == "r6" | input$Allyax == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
               "<br>Intent: ", str_to_title(intent), 
    #          "<br>Jurisdiction: ", location,
               "<br>Age: ", age_group,
               "<br>Sex: ", sex
             )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(x = "Year", y = "Deaths per 1,000,000")
      if (input$Allyax == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }

    validate(need(nrow(sub) > 0, "No data selected"))

    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
      strip.background = element_rect(fill="#a9e7bb"),
      strip.text = element_text(color="#000000", face = "bold") )
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "#000000")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top", traceorder = "normal"), margin = list(b = 80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
  # Remoteness by jurisdiction, intent and sex (Table R) ------------------------------------------
  output$remotePlot <- renderPlotly({
    df_R <- readRDS("ABS_COD2018_remoteness.rds")
    if (input$Rjur=="Australia") {
      regR <- input$RAra
    }
    else {
      regR <- input$Rra
    }
    sub <- filter(df_R, jurisdiction == input$Rjur & age_group == "All ages" &
                    sex == "All" & intent %in% input$Rcod & region %in% regR &
                    (year >= input$Ryr[[1]] & year <= input$Ryr[[2]]) )

    p <- ggplot(sub) + aes(x = year, colour = reg_intent, linetype = reg_intent , group = 1) +
        geom_line() + labs(x = "Year", title=paste0(input$Rjur,", All ages") ) +
        scale_colour_manual(values = regcodcols) +
        scale_linetype_manual(values = regcodtype) +
        scale_x_continuous(breaks = seq(input$Ryr[[1]],input$Ryr[[2]],2) )
      Legend <- "Region by intent"

    if (input$Ryax == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Region: ", str_to_title(region),
        "<br>Intent: ", str_to_title(intent)
#        ,"<br>Sex: ", sex
      )) + scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }
    
    if (input$Ryax == "r5" | input$Ryax == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Region: ", str_to_title(region),
        "<br>Intent: ", str_to_title(intent)
#        ,"<br>Sex: ", sex
      )) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$Ryax == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    if (input$Ryax == "r6" | input$Ryax == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), "% (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Region: ", str_to_title(region),
        "<br>Intent: ", str_to_title(intent)
#        ,"<br>Sex: ", sex
      )) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl,.1))) +
        labs(y = "Percentage of drug-induced deaths among all deaths")
      if (input$Ryax == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })

  # Remoteness area as percentage (Tables R) ------------------------------------------
  output$remotePlotP <- renderPlotly({
    #needs to be sorted [order(...)] & made distinct
    df_R <- readRDS("ABS_COD2018_remoteness.rds")
#    sub <- filter(df_R,DID>=50)
    if (input$RPjur == "Australia") {
      sub <- filter(df_R, intent == input$RPcod & 
            age_group == input$RPage &
            (year >= input$RPyr[[1]] & year <= input$RPyr[[2]]) & 
            sex == input$RPsex & jurisdiction == input$RPjur)
    }
    else {
      sub <- filter(df_R, intent == input$RPcod & 
            age_group == input$RPage & 
            (year >= input$RPyr[[1]] & year <= input$RPyr[[2]]) & 
            sex == "All" &
            jurisdiction == input$RPjur)
    }

#    if (input$RPyax != "num") {
      if (input$RPjur == "Australia" & input$RPsex == "All" & input$RPage == "All ages" ) {
        sub <- filter(sub, region!="Regional and Remote") %>%
          group_by(year, intent, sex, jurisdiction, age_group) %>% 
          distinct() %>%
          mutate(alldeaths = sum(n),
                 percent = round(n/sum(n)*100, 2),
                 region = factor(region, levels = c( "Remote and Very Remote",
                                                     "Outer Regional",
                                                     "Inner Regional",
                                                     "Major Cities"
                 )))
      }
      else {
        sub <- filter(sub, region=="Regional and Remote" | region=="Major Cities" ) %>%
          group_by(year, intent, sex, jurisdiction, age_group) %>% 
          distinct() %>%
          mutate(alldeaths = sum(n),
                 percent = round(n/sum(n)*100, 2),
                 region = factor(region, levels = c( "Regional and Remote",
                                                     "Major Cities"
                 )))
      }
      
      p <- ggplot(sub, aes(x=year, y=percent, fill=region, group=1, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Percent: ", percent, "%",
        "<br>Area: ", str_to_title(region),
        "<br>Jurisdiction: ", jurisdiction,
        "<br>Age: ", age_group,
        "<br>Sex: ", sex,
        "<br>Intent: ", str_to_title(intent)
      ))) + geom_area() + labs(y = "Percent of drug-induced deaths") +
      scale_fill_manual(values = regcols) #c("#d3d798", "#b3bd50", "#95a327", "#6a7d14"))

      p <- p + labs(x = "Year") +
      scale_x_continuous(breaks = seq(input$RPyr[[1]],input$RPyr[[2]],2) )
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
    p <- p + theme_light() + theme(legend.title = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly(p,  tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = "Remoteness area", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
    
  })

  # All drugs by type plot (Table 12, 12b & 12c) ----------------------------------------------------------
  output$DTPlot <- renderPlotly({
    df_DT <- readRDS("ABS_COD2018_DT.rds")
    if (input$DTDrop == "IntSx") {
      if (input$DTjur == "Australia") {
          if (input$DTIsex=="All") {
            DTIcod=input$DTIcod
          }
          else {
            DTIcod=input$DTIScod
          }
          if (input$DTIsex!="MF") {
            sub <- subset(df_DT, subset = (intent==DTIcod & drug %in% input$DTIdrug
                          & age_group == input$DTage & sex == input$DTIsex & jurisdiction == input$DTjur
                          & (year >= input$DTyr[[1]] & year <= input$DTyr[[2]] ) ) )
            p <- ggplot(sub) + labs(title=paste0(
            input$DTjur,", Age: ",input$DTage,", Sex: ",input$DTIsex,", Intent: ",DTIcod) )
          }
          else {
            sub <- subset(df_DT, subset = (intent==DTIcod & drug %in% input$DTIdrug
                          & age_group == input$DTage & sex != "All" & jurisdiction == input$DTjur
                          & (year >= input$DTyr[[1]] & year <= input$DTyr[[2]] ) ) )
            p <- ggplot(sub) + facet_grid(cols = vars(sex) ) + labs(title=paste0(
              input$DTjur,", Age: ",input$DTage,", Intent: ",DTIcod) )
          }
      }
      if (input$DTjur != "Australia") {
          sub <- subset(df_DT, subset = (intent==input$DTIJcod & drug %in% input$DTIdrug
                             & age_group == "All ages" & sex == "All" & jurisdiction == input$DTjur
                             & (year >= input$DTyr[[1]] & year <= input$DTyr[[2]] ) ) )
          p <- ggplot(sub) + labs(title=paste0(
            input$DTjur,", Age: All ages, Sex: All persons, Intent: ",input$DTIJcod) )
      }
        p <- p + aes(x = year, colour = drug, linetype = drug, group = 1) +
        geom_line() + labs(x = "Year") +
        scale_colour_manual(values = dtcols) +
        scale_linetype_manual(values = dttype) +
        scale_x_continuous(breaks = seq(input$DTyr[[1]],input$DTyr[[2]],2) )
      Legend <- "" #"Related drug"
      LO <- "v"
      LY <- 0.99
      if (input$DTIsex!="All") {
        validate(need(nrow(sub) > 0, "Please select All ages for age range for data by male and/or female."))
      }
    }
    
    if (input$DTDrop == "Drug") {
      if (input$DTjur == "Australia") {
        sub <- subset(df_DT, subset = (intent %in% input$DTDcod & drug == input$DTDdrug & 
                    age_group == input$DTage & sex %in% input$DTDsex & jurisdiction == input$DTjur &
                    (year >= input$DTyr[[1]] & year <= input$DTyr[[2]]) ) )
        p <- ggplot(sub) + labs(title=paste0(
          input$DTjur,", Age: ",input$DTage,", Drug: ",input$DTDdrug) ) +
          aes(x = year, colour = sex_intent, linetype = sex_intent, group = 1) +
          scale_colour_manual(values = sexcols) +
          scale_linetype_manual(values = sexcodtype)
        Legend <- "Sex by intent"
        if (input$DTDsex[[1]]!="All") {
          validate(need(nrow(sub) > 0, "Please select All ages for age range for data by male and/or female."))
        }
      }
      if (input$DTjur != "Australia") {
        sub <- subset(df_DT, subset = (intent %in% input$DTDcod & drug == input$DTDdrug & 
                     age_group == "All ages" & sex == "All" & jurisdiction == input$DTjur &
                     (year >= input$DTyr[[1]] & year <= input$DTyr[[2]]) ) )
        p <- ggplot(sub) + labs(title=paste0(
          input$DTjur,", Age: All ages, Sex: All persons, Drug: ",input$DTDdrug) ) +
          aes(x = year, linetype = intent, group = 1) +
          scale_linetype_manual(values = codtype)
        Legend <- "Intent"
      }
      p <- p + geom_line() + labs(x = "Year") +
        scale_x_continuous(breaks = seq(input$DTyr[[1]],input$DTyr[[2]],2) )
      LO <- "v"
      LY <- 0.99
    }
    
    if (input$DTyax == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Drug: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Sex: ", sex,
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }
    
    else if (input$DTyax == "r5" | input$DTyax == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Drug: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Sex: ", sex,
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$DTyax == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$DTyax == "r6" | input$DTyax == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Drug: ", drug,
        "<br>Intent: ", str_to_title(intent),
        "<br>Sex: ", sex,
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$DTyax == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_rect(fill="#a9e7bb"),
            strip.text = element_text(color="#000000", face = "bold") )
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = .99, yanchor = "bottom",
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      #      layout(legend = list(orientation = "h", y = -0.15, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
      layout(legend = list(orientation = LO, y = LY, yanchor = "top"), margin = list(b = 80) ) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })

  # All drugs by type plot (Tables 12 & 12a) ----------------------------------------------------------
  output$DTAPlot <- renderPlotly({
    df_DT <- readRDS("ABS_COD2018_DT.rds")

    if (input$DTADrop == "Age_Intent") {
      sub <- subset(df_DT, subset = (intent==input$DTAIcod & nature=="Underlying" & age_group == input$DTAIage
                    & sex == "All" & jurisdiction == "Australia" & drug %in% input$DTAIdrug
                    & (year >= input$DTAyr[[1]] & year <= input$DTAyr[[2]] ) ) )
    p <- ggplot(sub) + aes(x = year, colour = str_wrap(drug,50), linetype = str_wrap(drug,50), group = 1) +
      geom_line() + labs(x = "Year") +
      scale_colour_manual(values = dtcols) +
      scale_linetype_manual(values = dttype) +
      scale_x_continuous(breaks = seq(input$DTAyr[[1]],input$DTAyr[[2]],2) )
    Legend <- "Related drug"
    LO <- "v"
    LY <- 0.99
    }
    if (input$DTADrop == "Drug") {
      sub <- subset(df_DT, subset = (intent %in% input$DTADcod & nature=="Underlying" & drug == input$DTADdrug &
                       age_group %in% input$DTADage & sex == "All" & jurisdiction == "Australia" &
                       (year >= input$DTAyr[[1]] & year <= input$DTAyr[[2]]) ) )
      p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        geom_line() + labs(x = "Year") +
        scale_colour_manual(values = agecodcols) +
        scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = seq(input$DTAyr[[1]],input$DTAyr[[2]],2) )
      Legend <- "Age by intent"
      LO <- "v"
      LY <- 0.99
    }

    if (input$DTAyax == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Drug: ", str_to_title(drug),
#        "<br>Nature: ", str_to_title(nature),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }

    else if (input$DTAyax == "r5" | input$DTAyax == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Drug: ", str_to_title(drug),
#        "<br>Nature: ", str_to_title(nature),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$DTAyax == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }

    else if (input$DTAyax == "r6" | input$DTAyax == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Drug: ", drug,
#        "<br>Nature: ", str_to_title(nature),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$DTAyax == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }

    validate(need(nrow(sub) > 0, "No data selected"))

    # Remove vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
            theme(panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank())

    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper",
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99 , yanchor = "bottom",
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
#      layout(legend = list(orientation = "h", y = -0.15, yanchor = "top"), margin = list(b = 100, l = 100)) %>%
      layout(legend = list(orientation = LO, y = LY, yanchor = "top"), margin = list(b = 80) ) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                            "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                            "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })

  # Plot O4 (Table 4) -----------------------------------------------------------------
  output$O4Plot <- renderPlotly({
    df_Op <- readRDS("ABS_COD2018_Op.rds")
    #Based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html  
    if (input$O4Drop == "Opioid") {
      sub <- subset(df_Op, subset = (sex == "All" & location == "Aus" & drug == input$O4Odrug &
                 intent %in% input$O4cod & age_group %in% input$O4Oage &
                 (year >= input$O4yr[[1]] & year <= input$O4yr[[2]])))
  ####For user-defined year intervals
  #     yr <- as.numeric(input$O4xax)
  #     yr <- (input$O4yr[[2]]-input$O4yr[[1]])/yr
      p <- ggplot(sub) + aes(x = year, group = 1) +
        geom_line() + labs(x = "Year") +
        scale_colour_manual(values = agecodcols) +
        scale_x_continuous(breaks = seq(input$O4yr[[1]],input$O4yr[[2]],2) )
  ####For user-defined year intervals
  #     function(x) unique(floor( pretty(x,n=yr) ) )
      Title <- input$O4Odrug

      if (dim.data.frame(input$O4cod)[2]==1 | (dim.data.frame(input$O4cod)[2]==2 & input$O4cod2==2) ) {
        p <- p + aes(colour = age_group)
        Legend <- "Age"
      }
      else {
        p <- p + aes(colour = age_intent, linetype = age_intent) +
          scale_linetype_manual(values = agecodtype)
        Legend <- "Age by intent"
      }
    }
    
    else if (input$O4Drop == "Age") {
      sub <- subset(df_Op, subset = (sex == "All" & location == "Aus" & drug %in% input$O4Adrug &
                 intent %in% input$O4cod & age_group == input$O4Aage &
                 (year >= input$O4yr[[1]] & year <= input$O4yr[[2]])))
      p <- ggplot(sub) + aes(x = year, group = 1) +
        geom_line() + labs(x = "Year") +
        scale_colour_manual(values = opcodcols) +
        scale_x_continuous(breaks = seq(input$O4yr[[1]],input$O4yr[[2]],2) )
  #     function(x) unique(floor( pretty(x,n=yr) ) )
      Title <- paste0("Age: ",input$O4Aage)

      if (dim.data.frame(input$O4cod)[2]==1 | (dim.data.frame(input$O4cod)[2]==2 & input$O4cod2==2) ) {
        p <- p + aes(colour = drug)
        Legend <- "Drug"
      }
      else {
        p <- p + aes(colour = op_intent, linetype = op_intent) +
          scale_linetype_manual(values = opcodtype)
        Legend <- "Drug by intent"
      }
    }
    
    if (dim.data.frame(input$O4cod)[2]==1) {
      Title <- paste0(Title,"; Intent: ",input$O4cod)
    }
    if (dim.data.frame(input$O4cod)[2]==2 & input$O4cod2==2) {
      p <- p + facet_grid(cols = vars(intent) )
    }

    if (input$O4yax == "num") {
      p <- p + aes(y = n,
                   text = paste0(
                     "Year: ", year,
                     "<br>Deaths: ", n,
                     "<br>Opioid: ", str_to_title(drug),
                     "<br>Intent: ", str_to_title(intent),
                     "<br>Age group: ", age_group)
      ) +  scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }
    
    else if (input$O4yax == "r5" | input$O4yax == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Opioid: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$O4yax == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$O4yax == "r6" | input$O4yax == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Opioid: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$O4yax == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + labs(title = Title) + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_rect(fill="#a9e7bb"),
            strip.text = element_text(color="#000000", face = "bold") )
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  # Plot O5 (Table 5)-----------------------------------------------------------------
  output$O5Plot <- renderPlotly({
    df_Op <- readRDS("ABS_COD2018_Op.rds")
    # sub <- subset(df_Op, subset = (age_group == input$O5age & location == "Aus" &
    #    drug %in% input$O5drug & intent == input$O5cod & sex %in% input$O5sex  &
    #     (year >= input$O5yr[[1]] & year <= input$O5yr[[2]])))
    
    if (input$O5Drop == "Opioid") {
      sub <- subset(df_Op, subset = (age_group == input$O5age & location == "Aus" &
                                       drug == input$O5Odrug & intent %in% input$O5Ocod & sex %in% input$O5Osex  &
                                       (year >= input$O5yr[[1]] & year <= input$O5yr[[2]])))

      p <- ggplot(sub) + aes(x = year, colour = sex_intent, linetype = sex_intent, group = 1) +
        geom_line() +
        scale_colour_manual(values = sexcols) +
        scale_linetype_manual(values = sexcodtype) +
        scale_x_continuous(breaks = seq(input$O5yr[[1]],input$O5yr[[2]],2) )
      Legend <- "Sex by intent"
    }
    else if (input$O5Drop == "Intent") {
      sub <- subset(df_Op, subset = (age_group == input$O5age & location == "Aus" &
                                       drug %in% input$O5Idrug & sex %in% input$O5Isex & intent ==input$O5Icod & 
                                       (year >= input$O5yr[[1]] & year <= input$O5yr[[2]])))
      
      p <- ggplot(sub) + aes(x = year, colour = op_sex, linetype = op_sex, group = 1) +
        geom_line() +
        scale_colour_manual(values = opcodcols) +
        scale_linetype_manual(values = opcodtype) +
        scale_x_continuous(breaks = seq(input$O5yr[[1]],input$O5yr[[2]],2) )
      Legend <- "Drug by sex"
    }
    else if (input$O5Drop == "Sex") {
      if (input$O5Ssex != "MF") {
        sub <- subset(df_Op, subset = (age_group == input$O5age & location == "Aus" &
             drug %in% input$O5Sdrug & intent %in% input$O5Scod & sex == input$O5Ssex &
             (year >= input$O5yr[[1]] & year <= input$O5yr[[2]])))
        
        p <- ggplot(sub)
      }
      else {
        sub <- subset(df_Op, subset = (age_group == input$O5age & location == "Aus" &
             drug %in% input$O5Sdrug & intent %in% input$O5Scod & sex != "All" &
             (year >= input$O5yr[[1]] & year <= input$O5yr[[2]])))
        
        p <- ggplot(sub) + facet_grid(cols = vars(sex) )
      }
      p <- p + aes(x = year, colour = op_intent, linetype = op_intent, group = 1) +
        geom_line() +
        scale_colour_manual(values = opcodcols) +
        scale_linetype_manual(values = opcodtype) +
        scale_x_continuous(breaks = seq(input$O5yr[[1]],input$O5yr[[2]],2) )
      Legend <- "Drug by intent"
    }
    
    if (input$O5yax == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Age group: ", age_group,
        "<br>Intent: ", str_to_title(intent),
        "<br>Opioid: ", drug,
        "<br>Sex: ", sex
      ) ) +
        scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(x = "Year", y = "Number of deaths")
    }
    
    else if (input$O5yax == "r5" | input$O5yax == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Age group: ", age_group,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Intent: ", str_to_title(intent),
        "<br>Opioid: ", drug,
        "<br>Sex: ", sex
      ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(x = "Year", y = "Deaths per 100,000")
      if (input$O5yax == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$O5yax == "r6" | input$O5yax == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Age group: ", age_group,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Intent: ", str_to_title(intent),
        "<br>Opioid: ", drug,
        "<br>Sex: ", sex
      ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(x = "Year", y = "Deaths per 1,000,000")
      if (input$O5yax == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_rect(fill="#a9e7bb"),
            strip.text = element_text(color="#000000", face = "bold") )
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
  # Plot O6 (Table 6) -----------------------------------------------------------------
  output$O6Plot <- renderPlotly({
    df_Op <- readRDS("ABS_COD2018_Op.rds")
    sub <- subset(df_Op, subset = (age_group == input$O6age & drug == "All opioids" & jurisdiction == input$O6jur &
                                     intent %in% input$O6cod & sex %in% input$O6sex &
                                     (year >= input$O6yr[[1]] & year <= input$O6yr[[2]])))
    #    sub$sex_intent <- paste(sub$sex,sub$intent,sep=",")
    
    p <- ggplot(sub) + aes(x = year, 
                           colour = sex_intent, linetype = sex_intent, group = 1) + #colour = location, linetype = sex,
      geom_line() + labs(x = "Year", title=paste0(input$O6jur,", ",input$O6age) ) +
      scale_colour_manual(values = sexcols) + #statecols
      scale_linetype_manual(values = sexcodtype) + #sextype
      scale_x_continuous(breaks = seq(input$O6yr[[1]],input$O6yr[[2]],2) )
    
    if (input$O6yax == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Intent: ", str_to_title(intent), 
        "<br>Jurisdiction: ", location,
        "<br>Sex: ", sex
      )
      ) + labs(y = "Number of deaths") +
        scale_y_continuous(limits = c(0, max(sub$n, 250)))
    }
    
    else if (input$O6yax == "r5" | input$O6yax == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Intent: ", str_to_title(intent), 
        "<br>Jurisdiction: ", location,
        "<br>Sex: ", sex
      )
      ) + labs(y = "Deaths per 100,000") +
        scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5)))
      if (input$O6yax == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$O6yax == "r6" | input$O6yax == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Intent: ", str_to_title(intent), 
        "<br>Jurisdiction: ", location,
        "<br>Sex: ", sex
      )
      ) + labs(y = "Deaths per 1,000,000") +
        scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25)))
      if (input$O6yax == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = "Sex by intent", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
# Opioids with other drugs (Table 7) -----------------------------------------------------------------
  output$W7Plot <- renderPlotly({
    df_OpW <- readRDS("ABS_COD2018_OpW.rds")
    sub <- filter(df_OpW, #jurisdiction == "Australia" & word(drug, start = 1, end = 3) == "All opioids with" &
            intent %in% input$W7cod & (year >= input$W7yr[[1]] & year <= input$W7yr[[2]]) & sex=="All")
    if ( is.null(input$W7show) ) {
      sub <- filter(sub, set == "OpioidW")
    }

    if (input$W7Drop == "Drug") {
    sub <- filter(sub, drug == input$W7Ddrug & age_group %in% input$W7Dage)

    p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        labs(x = "Year", title=paste0("All opioids with ",input$W7Ddrug) ) + geom_line() +
        scale_colour_manual(values = agecodcols) + scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = seq(input$W7yr[[1]],input$W7yr[[2]],2) )
    Legend <- "Age by intent"
    }
    if (input$W7Drop == "Age") {
      sub <- filter(sub, drug %in% input$W7Adrug & age_group == input$W7Aage)
      
      p <- ggplot(sub) + aes(x = year, colour = op_intent, linetype = op_intent, group = 1) +
        labs(x = "Year") + geom_line() +
        scale_colour_manual(values = opwcodcols) + scale_linetype_manual(values = opwcodtype) +
        scale_x_continuous(breaks = seq(input$W7yr[[1]],input$W7yr[[2]],2) )
      Legend <- "Drug by intent"
    }
    
    if ( is.character(input$W7show) ) {
      p <- p + aes(alpha=primary) +
        scale_alpha_manual(values = c(0.3 , 1) )
      Legend <- paste0(Legend,"<br> by death data type")
    }

    if (input$W7yax == "num") {
        p <- p + aes(y = n, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Drug: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Sex: ", sex,
               "<br>Age group: ", age_group ) ) + 
              scale_y_continuous(limits = c(0, max(sub$n, 250) ) ) +
              labs(y = "Number of deaths")
    }
    if (input$W7yax == "r5" | input$W7yax == "r5ci") {
        p <- p + aes(y = rate_ht, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
               "<br>Drug: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Sex: ", sex,
               "<br>Age group: ", age_group
            )) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
            labs(y = "Deaths per 100,000")

        if (input$W7yax == "r5ci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }
    
    if (input$W7yax == "r6" | input$W7yax == "r6ci") {
        p <- p + aes(y = rate_m, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
               "<br>Drug: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Sex: ", sex,
               "<br>Age group: ", age_group) ) + 
               scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) + labs(y = "Deaths per 1,000,000")
        if (input$W7yax == "r6ci") {
            p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
        }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
            theme(panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99 , yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                      "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                      "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
  # Opioids and other drugs by sex (Table 8) ------------------------------------------
  output$W8Plot <- renderPlotly({
    df_OpW <- readRDS("ABS_COD2018_OpW.rds")
    sub <- filter(df_OpW, drug %in% input$W8drug & age_group == input$W8age & 
          (year >= input$W8yr[[1]] & year <= input$W8yr[[2]]) )
    if ( is.null(input$W8show) ) {
      sub <- filter(sub, set == "OpioidW")
    }

    if (input$W8Drop == "Sex") {
      if (input$W8Ssex != "MF") {
        sub <- filter(sub, intent %in% input$W8Scod & sex == input$W8Ssex )
        p <- ggplot(sub)
      }
      else {
        sub <- filter(sub, intent %in% input$W8Scod & sex != "All" )
        p <- ggplot(sub) + facet_grid(cols = vars(sex))
      }
      p <- p + aes(x = year, colour = str_wrap(op_intent,50), 
            linetype = str_wrap(op_intent,50), group = 1) + geom_line() + 
            labs(x = "Year", title = paste0("Age group:",input$W8age,"  Sex: ",input$W8Ssex) ) +
            scale_colour_manual(values = opwcodcols) +
            scale_linetype_manual(values = opwcodtype) +
            scale_x_continuous(breaks = seq(input$W8yr[[1]],input$W8yr[[2]],2)  )
      Legend <- "Drug by intent"
    }
    if (input$W8Drop == "Intent") {
        sub <- filter(sub, intent == input$W8Icod & sex %in% input$W8Isex )
        p <- ggplot(sub) + aes(x = year, colour = op_sex, linetype = op_sex, group = 1) + 
              geom_line() + 
              labs(x = "Year", title = paste0("Age group:",input$W8age,"  Intent: ",input$W8Icod) ) +
              scale_colour_manual(values = opwcodcols) +
              scale_linetype_manual(values = opwcodtype) +
              scale_x_continuous(breaks = seq(input$W8yr[[1]],input$W8yr[[2]],2) )
        Legend <- "Drug by sex"
    }

    if ( is.character(input$W8show) ) {
      p <- p + aes(alpha=primary) +
        scale_alpha_manual(values = c(0.3 , 1) )
      Legend <- paste0(Legend,"<br> by death data type")
    }

    if (input$W8yax == "num") {
      p <- p + aes(y = n, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Drug: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Age group: ", age_group,
               "<br>Sex: ", sex
            )) +
            scale_y_continuous(limits = c(0, max(sub$n, 250))) + labs(y = "Number of deaths")
    }
    
    if (input$W8yax == "r5" | input$W8yax == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
               "<br>Drug: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Age group: ", age_group,
               "<br>Sex: ", sex
           )) + geom_line() +
          scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
          labs(y = "Deaths per 100,000")
        if (input$W8yax == "r5ci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }
    
    if (input$W8yax == "r6" | input$W8yax == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
               "<br>Drug: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Sex: ", sex
            )) + geom_line() +
            scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
            labs(y = "Deaths per 1,000,000")
        if (input$W8yax == "r6ci") {
          p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
        }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
          theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background = element_rect(fill="#a9e7bb"),
          strip.text = element_text(color="#000000", face = "bold") )
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                    "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                    "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
    
  })

  # Exclusive Opioids by age and intent (Table 10) -----------------------------------------------------------------
  output$E0Plot <- renderPlotly({
    df_OpE <- readRDS("ABS_COD2018_OpE.rds")
      if (input$E0Drop == "Opioid") {
      sub <- filter(df_OpE, sex == "All" & location == "Aus" &
                  drug == input$E0Odrug & intent %in% input$E0cod & age_group %in% input$E0Oage &
                  (year >= input$E0yr[[1]] & year <= input$E0yr[[2]])) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)
#        sub$age_intent <- paste(sub$age_group,sub$intent,sep=",")
      
      p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        geom_line() + labs(x = "Year", title=input$E0Odrug) +
        scale_colour_manual(values = agecodcols) +
        scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = seq(input$E0yr[[1]],input$E0yr[[2]],2) )
      Legend <- "Age by intent"
    }
    
    else if (input$E0Drop == "Age") {
      sub <- subset(df_OpE, subset = (sex == "All" & location == "Aus" &
                  drug %in% input$E0Adrug & intent %in% input$E0cod & age_group == input$E0Aage &
                  (year >= input$E0yr[[1]] & year <= input$E0yr[[2]]))) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)
      
      p <- ggplot(sub) + aes(x = year, colour = op_intent, linetype = op_intent, group = 1) +
        geom_line() + labs(x = "Year", title=paste0("Age: ",input$E0Aage)) +
        scale_colour_manual(values = opcodcols) +
        scale_linetype_manual(values = opcodtype) +
        scale_x_continuous(breaks = seq(input$E0yr[[1]],input$E0yr[[2]],2) )
      Legend <- "Drug by intent"
    }
    
    if (input$E0yax == "num") {
      p <- p + aes(y = n,
                   text = paste0(
                     "Year: ", year,
                     "<br>Deaths: ", n,
                     "<br>Opioid: ", str_to_title(drug),
                     "<br>Intent: ", str_to_title(intent),
                     "<br>Age group: ", age_group)
      ) +  scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }
    
    else if (input$E0yax == "r5" | input$E0yax == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Opioid: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$E0yax == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$E0yax == "r6" | input$E0yax == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Opioid: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$E0yax == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })

  # Exclusive opioids by jurisdiction, intent and sex (Table 9 & 11) ------------------------------------------
  output$E9Plot <- renderPlotly({
    df_OpE <- readRDS("ABS_COD2018_OpE.rds")
    if (input$E9Drop == "Intent") {
      sub <- filter(df_OpE, jurisdiction == input$E9jur & age_group == input$E9age &
                    sex %in% input$E9Isex & intent == input$E9Icod & drug %in% input$E9drug & 
                    (year >= input$E9yr[[1]] & year <= input$E9yr[[2]]) ) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)

      p <- ggplot(sub) + aes(x = year, colour = op_sex, linetype = op_sex, group = 1) +
        geom_line() + labs(x = "Year", title=paste0(input$E9jur,", Age: ",input$E9age," Intent: ",input$E9Icod) ) +
        scale_colour_manual(values = opcodcols) +
        scale_linetype_manual(values = opcodtype) +
        scale_x_continuous(breaks = seq(input$E9yr[[1]],input$E9yr[[2]],2) )
      Legend <- "Drug by sex"
    }
    if (input$E9Drop == "Sex") {
      if (input$E9Ssex != "MF") {
        sub <- filter(df_OpE, jurisdiction == input$E9jur & age_group == input$E9age &
                        sex == input$E9Ssex & intent %in% input$E9Scod & drug %in% input$E9drug & 
                        (year >= input$E9yr[[1]] & year <= input$E9yr[[2]]) ) %>%
          distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)
        p <- ggplot(sub) + labs(x = "Year", title=paste0(
          input$E9jur,", Age: ",input$E9age," Sex: ",input$E9Ssex) )
      }
      else {
        sub <- filter(df_OpE, jurisdiction == input$E9jur & age_group == input$E9age &
                        sex != "All" & intent %in% input$E9Scod & drug %in% input$E9drug & 
                        (year >= input$E9yr[[1]] & year <= input$E9yr[[2]]) ) %>%
          distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)
        p <- ggplot(sub) + facet_grid(cols = vars(sex)) +
          labs(x = "Year", title=paste0(input$E9jur,", Age: ",input$E9age) )
      }
      p <- p + aes(x = year, colour = op_intent, linetype = op_intent, group = 1) +
        geom_line() +
        scale_colour_manual(values = opcodcols) +
        scale_linetype_manual(values = opcodtype) +
        scale_x_continuous(breaks = seq(input$E9yr[[1]],input$E9yr[[2]],2) )
      Legend <- "Drug by intent"
    }


    if (input$E9yax == "num") {
      p <- p + aes(y = n, text = paste0(
                       "Year: ", year,
                       "<br>Deaths: ", n,
                       "<br>Drug: ", str_to_title(drug),
                       "<br>Intent: ", str_to_title(intent),
                       "<br>Sex: ", sex
          )) + scale_y_continuous(limits = c(0, max(sub$n, 250))) +
          labs(y = "Number of deaths")
    }
    
    if (input$E9yax == "r5" | input$E9yax == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
                       "Year: ", year,
                       "<br>Deaths: ", n,
                       "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
                       "<br>Drug: ", str_to_title(drug),
                       "<br>Intent: ", str_to_title(intent),
                       "<br>Sex: ", sex
            )) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
            labs(y = "Deaths per 100,000")
        if (input$E9yax == "r5ci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }

    if (input$E9yax == "r6" | input$E9yax == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
                       "Year: ", year,
                       "<br>Deaths: ", n,
                       "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
                       "<br>Drug: ", str_to_title(drug),
                       "<br>Intent: ", str_to_title(intent),
                       "<br>Sex: ", sex
            )) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
            labs(y = "Deaths per 1,000,000")
        if (input$E9yax == "r6ci") {
          p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
        }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
  # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_rect(fill="#a9e7bb"),
            strip.text = element_text(color="#000000", face = "bold") )
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                             "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                             "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
  # Exclusive opioids as percents ------------------------------------------
  output$EPPlot <- renderPlotly({
    #needs to be sorted [order(...)]
    #- need to make distinct
    df_OpE <- readRDS("ABS_COD2018_OpE.rds")
    sub <- filter(df_OpE, drug %in% c( "Exclusive illicit opioids",
                                   "Exclusive pharmaceutical opioids",
                                   "Illicit & pharmaceutical opioids",
                                   "Other & unspecified opioids") &
                    intent == input$EPcod & 
                    age_group == input$EPage & 
                    (year >= input$EPyr[[1]] & year <= input$EPyr[[2]]) & 
                    sex == input$EPsex &
                    jurisdiction == input$EPjur) %>% # & table!="10" & table!="18"
      group_by(year, intent, sex, jurisdiction, age_group) %>% 
      distinct() %>%
      mutate(alldeaths = sum(n),
             percent = round(n/sum(n)*100, 2),
             drug = factor(drug, levels = c( "Other & unspecified opioids",
                                             "Illicit & pharmaceutical opioids",
                                             "Exclusive pharmaceutical opioids",
                                             "Exclusive illicit opioids"
                                             )))

    p <- ggplot(sub, aes(x=year, y=percent, fill=drug, group=1, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Percent: ", percent, "%",
        "<br>Drug: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Sex: ", sex
    ))) +
      geom_area() +
      labs(x = "Year", y = "Percent of opioid induced deaths") +
      theme_light() + 
      theme(legend.title = element_blank()) + 
      scale_x_continuous(breaks = seq(input$EPyr[[1]],input$EPyr[[2]],2) ) +
      scale_fill_manual(values = opcols) #c("#d3d798", "#b3bd50", "#95a327", "#6a7d14"))
#      scale_fill_manual(values = c("#c1c870", "#748a34", "#465d02", "#1f3300"))
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())

    ggplotly(p,  tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = "Drug", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                        "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                        "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
    
  })
  
}
