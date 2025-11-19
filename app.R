#Sommaire : poc FFS#####################

# 0. Chargement des librairies requises
# 1. Chargement des données
# 2. Création de fonction
# 3. Définition user interface et affichage
# 4. Définition serveur et contenu
# 5. Lancement de l'application

#0. Libraries###################################################################
# Chargement des librairies requises

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(ggplot2)
library(shinyjs)
library(tidyr)
library(lubridate)

#1. Données#####################################################################
# Chargement des données, création dataframe, ajout données, nettoyage
#Création des dataframes (import Excel)
df_courses <- read_excel("C:/Users/laura/Documents/UGA M1 STAPS ATDM/Fédé Ski POC/Courses.xlsx")
df_sections <- read_excel("C:/Users/laura/Documents/UGA M1 STAPS ATDM/Fédé Ski POC/Sections.xlsx")
df_pistes <- read_excel("C:/Users/laura/Documents/UGA M1 STAPS ATDM/Fédé Ski POC/Pistes.xlsx")
df_athletes <- read_excel("C:/Users/laura/Documents/UGA M1 STAPS ATDM/Fédé Ski POC/Athletes.xlsx")
df_Saalbach_D_fev_2025 <- read_excel("C:/Users/laura/Documents/UGA M1 STAPS ATDM/Fédé Ski POC/Saalbach_D_fev_2025.xlsx")
df_Saalbach_G_fev_2025_R1 <- read_excel("C:/Users/laura/Documents/UGA M1 STAPS ATDM/Fédé Ski POC/Saalbach_G_fev_2025_R1.xlsx")
df_Saalbach_G_fev_2025_R2 <- read_excel("C:/Users/laura/Documents/UGA M1 STAPS ATDM/Fédé Ski POC/Saalbach_G_fev_2025_R2.xlsx")
df_Saalbach_S_fev_2025_R1 <- read_excel("C:/Users/laura/Documents/UGA M1 STAPS ATDM/Fédé Ski POC/Saalbach_S_fev_2025_R1.xlsx")
df_Saalbach_S_fev_2025_R2 <- read_excel("C:/Users/laura/Documents/UGA M1 STAPS ATDM/Fédé Ski POC/Saalbach_S_fev_2025_R2.xlsx")
df_Saalbach_SG_fev_2025 <- read_excel("C:/Users/laura/Documents/UGA M1 STAPS ATDM/Fédé Ski POC/Saalbach_SG_fev_2025.xlsx")

#2. Fonctions#####################################################################
convert_time <- function(t) {
  if (is.na(t) || t == "") return(NA_real_)
  if (grepl("^\\d+:\\d{2}\\.\\d+$", t)) {
    parts <- strsplit(t, ":", fixed = TRUE)[[1]]
    return(as.numeric(parts[1]) * 60 + as.numeric(parts[2]))
  } else if (grepl("^\\d{1,2}\\.\\d+$", t)) {
    return(as.numeric(t))
  } else {
    return(NA_real_)
  }
}


#3.User interface###############################################################

# Définition user interface et affichage du header, des menus sur le côté, et 
# des différentes pages
# Création du RShinyDashboard

ui <- dashboardPage(
  skin = "blue",
  
  ##3.1 Header----
  dashboardHeader(title = "FFS"),
  
  ##3.2 Sidebar ----
  dashboardSidebar(
    tags$div(
      tags$img(src = "logo.png", height = "100px", style = "display: block; margin-left: auto; margin-right: auto;"),
      style = "padding: 10px;"
    ),
    ##3.3 Personnalisation des menus ----
    sidebarMenu(
      menuItem("Analyse par piste", tabName = "page1", icon = icon("person-skiing")),
      menuItem("Analyse par course", tabName = "page2", icon = icon("snowflake"))
    )
  ),
  
  ## 3.4 Personnalisation du body ----
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "page1",
        fluidRow(
          column(4, selectizeInput(
            "skieur_1", 
            "Choisir un skieur :", 
            choices = c("Aucun" = "", unique(df_athletes$Name)),
            options = list(server = TRUE)
          )),
          column(4, uiOutput("piste_ui_1")),
          column(4, uiOutput("course_ui_1"))
        ),
        fluidRow(
          tabBox(
            width = 12,
            tabPanel("Classement inter", plotOutput("classement_inter")),
            tabPanel("Temps inter", plotOutput("temps_inter")),
            tabPanel("Ecart inter", plotOutput("gap_inter"))
          )
        ),
        fluidRow(
          tabBox(
            width = 12,
            tabPanel("Classement section", plotOutput("classement_section")),
            tabPanel("Temps section", plotOutput("temps_section")),
            tabPanel("Différence section", plotOutput("diff_section")),
            tabPanel("Vitesse section", plotOutput("vitesse_section")),
            tabPanel("Ecart de vitesse section", plotOutput("vitesse_gap_section"))
          )
        )
      )
      ,
      tabItem(
        tabName = "page2",
        fluidRow(
          column(3, uiOutput("piste_ui_2")),
          column(3, uiOutput("course_ui_2")),
          column(3, uiOutput("skieur_ui_2"))
        ),
        fluidRow(
          column(12, checkboxInput("show_first", "Afficher le premier", value = FALSE))
        )
        ,
        fluidRow(
          tabBox(
            width = 12,
            tabPanel("Classement inter", plotOutput("classement_inter_2")),
            tabPanel("Temps inter", plotOutput("temps_inter_2")),
            tabPanel("Ecart inter", plotOutput("gap_inter_2")))
          ),
        fluidRow(
          tabBox(
            width = 12,
            tabPanel("Classement section", plotOutput("classement_section_2")),
            tabPanel("Temps section", plotOutput("temps_section_2")),
            tabPanel("Différence section", plotOutput("diff_section_2")),
            tabPanel("Vitesse section", plotOutput("vitesse_section_2")),
            tabPanel("Ecart de vitesse section", plotOutput("vitesse_gap_section_2"))
          )
        )
      )
    )
  )
)

#4. Définition serveur #########################################################
#Création du serveur et création du contenu

server <- function(input, output, session) {
  
  ##-----------4.0 Manipulation des filtres ---------------------------------------
  
  # Liste des courses disponibles
  toutes_les_courses <- list(
    "Saalbach_D_fev_2025" = df_Saalbach_D_fev_2025,
    "Saalbach_G_fev_2025_R1" = df_Saalbach_G_fev_2025_R1,
    "Saalbach_G_fev_2025_R2" = df_Saalbach_G_fev_2025_R2,
    "Saalbach_S_fev_2025_R1" = df_Saalbach_S_fev_2025_R1,
    "Saalbach_S_fev_2025_R2" = df_Saalbach_S_fev_2025_R2,
    "Saalbach_SG_fev_2025" = df_Saalbach_SG_fev_2025
  )
  
  #Filtres page 1
  # Sélection des courses auxquelles le coureur a participé
  selection_courses_skieur <- reactive({
    req(input$skieur_1)
    courses_skieur <- c()
    
    for (nom_course in names(toutes_les_courses)) {
      df_temp_course <- toutes_les_courses[[nom_course]]
      if (input$skieur_1 %in% df_temp_course$Name) {
        courses_skieur <- c(courses_skieur, nom_course)
      }
    }
    courses_skieur
  })
  
  #Sélection des pistes où le coureur a déjà fait une course (on part des courses pour trouver les pistes)
  selection_pistes_skieur <- reactive({
    req(selection_courses_skieur())
    df_filtre_skieur <- df_courses[df_courses$Course %in% selection_courses_skieur(), ]
    unique(df_filtre_skieur$Piste)
  })
  
  # Mise en place du filtre (choix de la piste)
  output$piste_ui_1 <- renderUI({
    pistes <- if (!is.null(input$skieur_1) && input$skieur_1 != "") {
      df_filtre_skieur <- df_courses %>%
        filter(Course %in% selection_courses_skieur())
      unique(df_filtre_skieur$Piste)
    } else {
      unique(df_courses$Piste)
    }
    selectInput("piste_1", "Choisir une piste", choices = pistes)
  })
  
  # Mise en place du filtre (choix de la course)
  output$course_ui_1 <- renderUI({
    df_filtre <- df_courses
    if (!is.null(input$skieur_1) && input$skieur_1 != "") {
      df_filtre <- df_filtre %>% filter(Course %in% selection_courses_skieur())
    }
    if (!is.null(input$piste_1) && input$piste_1 != "") {
      df_filtre <- df_filtre %>% filter(Piste == input$piste_1)
    }
    choix_courses <- unique(df_filtre$Course)
    selectInput("course_1", "Choisir une ou plusieurs courses", choices = choix_courses, multiple = TRUE)
  })
  #Filtre page 2
  # Choix de la piste parmi les pistes disponibles
  selection_pistes_dispos <- reactive({
    unique(df_pistes$Piste)
  })
  
  
  # UI piste (choix unique)
  output$piste_ui_2 <- renderUI({
    req(selection_pistes_dispos())
    selectInput("piste_2", "Choisir une piste", choices = selection_pistes_dispos(), multiple = FALSE)
  })
  
  # Sélection des courses associées aux pistes
  selection_courses_par_piste <- reactive({
    req(input$piste_2)
    df_courses %>%
      filter(Piste == input$piste_2) %>%
      pull(Course) %>%
      unique()
  })
  
  #UI course (choix unique)
  output$course_ui_2 <- renderUI({
    req(selection_courses_par_piste())
    selectInput("course_2", "Choisir une course", choices = selection_courses_par_piste(), multiple = FALSE)
  })
  
  #Filtre sur un ou plusieurs skieurs
  selection_skieurs_2 <- reactive({
    req(input$course_2)
    df_course_dispo <- toutes_les_courses[[input$course_2]]
    unique(df_course_dispo$Name)
  })
  
  #UI skieur (choix multiples)
  output$skieur_ui_2 <- renderUI({
    req(selection_skieurs_2())
    selectInput("skieur_2_selection", "Choisir un ou plusieurs skieurs", choices = selection_skieurs_2(), multiple = TRUE)
  })

  
  ##----------4.1 Contenu page 1-------------------------------------------------
  
  ###4.1.1. Graphique classements intermédiaires (par course)-------------------------------
  output$classement_inter <- renderPlot({
    req(input$skieur_1, input$course_1)
    
    df_concat <- bind_rows(lapply(input$course_1, function(cours) {
      df <- toutes_les_courses[[cours]]
      df %>% filter(Name == input$skieur_1) %>% mutate(Course = cours)
    }))
    if (nrow(df_concat) == 0) return(NULL)
    
    cols <- colnames(df_concat)
    cols_sel <- cols[
      grepl("rank", cols, ignore.case = TRUE) &
        grepl("Inter", cols, ignore.case = TRUE) &
        grepl("Run", cols, ignore.case = TRUE)
    ]
    
    finish_cols <- grep("Run [0-9]+ finish rank", cols, value = TRUE)
    if (length(finish_cols) == 0) {
      return(NULL)
    }
    
    if (length(finish_cols) == 1) {
      df_concat <- df_concat %>%
        mutate(
          Finish_Rank = .data[[finish_cols[1]]],
          Finish_Run  = gsub(".*(Run [0-9]+).*", "\\1", finish_cols[1])
        )
    } else {
      df_concat$Finish_Rank <- apply(df_concat[finish_cols], 1, function(r) {
        idx <- which(!is.na(r))
        if (length(idx) == 0) NA_real_ else r[idx[1]]
      })
      df_concat$Finish_Run <- apply(df_concat[finish_cols], 1, function(r) {
        idx <- which(!is.na(r))
        if (length(idx) == 0) NA_character_ else gsub(".*(Run [0-9]+).*", "\\1", finish_cols[idx[1]])
      })
    }
    
    df_long <- df_concat %>%
      select(all_of(cols_sel), Course, Finish_Rank, Finish_Run) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Rank"
      ) %>%
      mutate(
        Run = gsub(".*(Run [0-9]+).*", "\\1", Section),                       
        Section = gsub(".*(Inter[ ]?[0-9]+).*", "\\1", Section)               
      )
    
    inter_levels <- sort(unique(df_long$Section))
    section_levels <- c(inter_levels, "Finish")
    df_long$Section <- factor(df_long$Section, levels = section_levels)
    
    df_finish <- df_concat %>%
      distinct(Course, Finish_Run, Finish_Rank) %>%
      filter(!is.na(Finish_Rank) & !is.na(Finish_Run)) %>%
      mutate(
        Section = factor("Finish", levels = section_levels),
        Rank = Finish_Rank,
        Run = Finish_Run
      ) %>%
      select(names(df_long))   
    
    df_plot <- bind_rows(df_long, df_finish)
    df_plot <- df_plot %>% filter(!is.na(Rank) & Rank > 0)
    
    max_rank <- ceiling(max(df_plot$Rank, na.rm = TRUE))
    
    ggplot(df_plot, aes(x = Section, y = Rank, color = Course, group = interaction(Course, Run))) +
      geom_line() +
      geom_point(size = 3) +
      scale_y_reverse(breaks = seq(1, max_rank)) +
      labs(
        title = paste("Classements intermédiaires de", input$skieur_1, "sur", input$piste_1),
        x = "Inter",
        y = "Place"
      ) +
      theme_minimal()
  })
  
  
  ###4.1.2. Graphique temps intermédiaires (par course)--------
  
  output$temps_inter <- renderPlot({
    req(input$skieur_1, input$course_1)
    
      df_concat <- bind_rows(lapply(input$course_1, function(cours) {
      df <- toutes_les_courses[[cours]]
      df %>% filter(Name == input$skieur_1) %>% mutate(Course = cours)
    }))
    
    if (nrow(df_concat) == 0) return(NULL)
    
    cols <- colnames(df_concat)
    cols_sel <- cols[
      grepl("time", cols, ignore.case = TRUE) &
        grepl("Inter", cols, ignore.case = TRUE) &
        grepl("Run", cols, ignore.case = TRUE)
    ]
    
    finish_cols <- grep("Run [0-9]+ finish time", cols, value = TRUE)
    
    df_long <- df_concat %>%
      select(all_of(cols_sel), Course, all_of(finish_cols)) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Time_raw"
      ) %>%
      mutate(
        Run = gsub(".*(Run [0-9]+).*", "\\1", Section),
        Section = gsub(".*(Inter[ ]?[0-9]+).*", "\\1", Section),
        Section = factor(Section, levels = sort(unique(Section))),
        Time_sec = as.numeric(NA)  # Initialisation pour éviter warning
      )
    
    df_long$Time_sec <- sapply(df_long$Time_raw, convert_time)
    
    df_finish_list <- lapply(finish_cols, function(col) {
      df_concat %>%
        select(Course, all_of(col)) %>%
        rename(Time_raw = all_of(col)) %>%
        mutate(
          Run = gsub("Run ([0-9]+) finish time", "Run \\1", col),
          Section = "Finish",
          Time_sec = sapply(Time_raw, convert_time),
          Section = factor(Section, levels = c(levels(df_long$Section), "Finish"))
        )
    })
    
    df_finish <- bind_rows(df_finish_list)
    
    df_plot <- bind_rows(df_long, df_finish)
    
    max_time <- max(df_plot$Time_sec, na.rm = TRUE)
    
    ggplot(df_plot, aes(x = Section, y = Time_sec, color = Course, group = interaction(Course, Run))) +
      geom_line() +
      geom_point(size = 3) +
      geom_text(
        aes(label = ifelse(is.na(Time_sec), "", sprintf("%d:%05.2f", floor(Time_sec / 60), Time_sec %% 60))),
        vjust = -1, size = 3, show.legend = FALSE
      ) +
      scale_y_continuous(
        labels = function(sec) sprintf("%d:%05.2f", floor(sec / 60), sec %% 60),
        breaks = scales::pretty_breaks()
      ) +
      labs(
        title = paste("Temps intermédiaires de", input$skieur_1, "sur", input$piste_1),
        x = "Inter",
        y = "Temps (min:sec:cent)"
      ) +
      theme_minimal()
  })

  ###4.1.3. Graphique avec écarts intermédiaires (par course)----
  
  output$gap_inter <- renderPlot({
    req(input$skieur_1, input$course_1)
    
    df_concat <- bind_rows(lapply(input$course_1, function(cours) {
      toutes_les_courses[[cours]] %>%
        filter(Name == input$skieur_1) %>%
        mutate(Course = cours)
    }))
    
    if (nrow(df_concat) == 0) return(NULL)
    
    cols <- colnames(df_concat)
    
    cols_sel <- cols[
      grepl("gap", cols, ignore.case = TRUE) &
        grepl("Inter", cols, ignore.case = TRUE) &
        grepl("Run", cols, ignore.case = TRUE)
    ]
    
    df_long <- df_concat %>%
      select(all_of(cols_sel), Course) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Time_raw"
      ) %>%
      mutate(
        Run = str_extract(Section, "Run [0-9]+"),
        Section_num = as.numeric(str_extract(Section, "(?<=Inter\\s?)\\d+")), # 🔹 Corrigé
        Section_clean = paste0("Inter ", Section_num),
        Time_clean = gsub("^\\+", "", Time_raw),
        Time_sec = sapply(Time_clean, convert_time)
      )
    
    finish_cols <- grep("^Run.*finish gap$", cols, value = TRUE, ignore.case = TRUE)
    
    df_finish <- df_concat %>%
      select(Course, all_of(finish_cols)) %>%
      pivot_longer(
        cols = all_of(finish_cols),
        names_to = "Finish_Section",
        values_to = "Time_raw"
      ) %>%
      mutate(
        Run = str_extract(Finish_Section, "Run [0-9]+"),
        Section_num = max(df_long$Section_num, na.rm = TRUE) + 1,
        Section_clean = "Finish",
        Time_clean = gsub("^\\+", "", Time_raw),
        Time_sec = sapply(Time_clean, convert_time)
      ) %>%
      select(Course, Run, Section_num, Section_clean, Time_sec)
    
    df_plot <- bind_rows(
      df_long %>% select(Course, Run, Section_num, Section_clean, Time_sec),
      df_finish
    ) %>%
      arrange(Course, Run, Section_num) %>%
      mutate(
        Section_clean = factor(Section_clean, 
                               levels = c(paste0("Inter ", sort(unique(df_long$Section_num))), "Finish"))
      ) %>%
      filter(!is.na(Time_sec))
    
    ggplot(df_plot, aes(x = Section_clean, y = Time_sec, color = Course, group = interaction(Course, Run))) +
      geom_line() +
      geom_point(size = 3) +
      geom_text(
        aes(label = ifelse(is.na(Time_sec), "", sprintf("%d:%05.2f", floor(Time_sec / 60), Time_sec %% 60))),
        vjust = -1, size = 3, show.legend = FALSE
      ) +
      scale_y_continuous(
        labels = function(sec) sprintf("%d:%05.2f", floor(sec / 60), sec %% 60),
        breaks = scales::pretty_breaks()
      ) +
      labs(
        title = paste("Écarts intermédiaires de", input$skieur_1, "sur", input$piste_1),
        x = "Inter",
        y = "Écart (min:sec:cent)"
      ) +
      theme_minimal()
  })
  
  
  ### 4.1.4. Graphique classements par section (par course) ----
  output$classement_section <- renderPlot({
    req(input$skieur_1, input$course_1)
    
    df_concat <- bind_rows(lapply(input$course_1, function(cours) {
      df <- toutes_les_courses[[cours]]
      df %>% filter(Name == input$skieur_1) %>% mutate(Course = cours)
    }))
    if (nrow(df_concat) == 0) return(NULL)
    
    cols <- colnames(df_concat)
    cols_sel <- cols[
      grepl("rank", cols, ignore.case = TRUE) &
        grepl("sector", cols, ignore.case = TRUE)
    ]
    
    finish_cols <- grep("Run [0-9]+ finish rank", cols, value = TRUE)
    if (length(finish_cols) == 0) return(NULL)
    
    if (length(finish_cols) == 1) {
      df_concat <- df_concat %>%
        mutate(
          Finish_Rank = .data[[finish_cols[1]]],
          Finish_Run  = gsub(".*(Run [0-9]+).*", "\\1", finish_cols[1])
        )
    } else {
      df_concat$Finish_Rank <- apply(df_concat[finish_cols], 1, function(r) {
        idx <- which(!is.na(r))
        if (length(idx) == 0) NA_real_ else r[idx[1]]
      })
      df_concat$Finish_Run <- apply(df_concat[finish_cols], 1, function(r) {
        idx <- which(!is.na(r))
        if (length(idx) == 0) NA_character_ else gsub(".*(Run [0-9]+).*", "\\1", finish_cols[idx[1]])
      })
    }
    
    df_long <- df_concat %>%
      select(all_of(cols_sel), Course, Finish_Rank, Finish_Run) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Rank"
      ) %>%
      mutate(
        Run = gsub(".*(Run [0-9]+).*", "\\1", Section),
        Section_num = as.numeric(gsub(".*sector[ ]?([0-9]+).*", "\\1", Section, ignore.case = TRUE)),
        Section = paste0("Sector ", Section_num)
      )
    
    sector_levels <- paste0("Sector ", sort(unique(df_long$Section_num)))
    section_levels <- c(sector_levels, "Finish")
    df_long$Section <- factor(df_long$Section, levels = section_levels)
    
    df_finish <- df_concat %>%
      distinct(Course, Finish_Run, Finish_Rank) %>%
      filter(!is.na(Finish_Rank) & !is.na(Finish_Run)) %>%
      mutate(
        Section = factor("Finish", levels = section_levels),
        Rank = Finish_Rank,
        Run = Finish_Run,
        Section_num = NA_real_  
      ) %>%
      select(names(df_long))
    
    df_plot <- bind_rows(df_long, df_finish) %>%
      filter(!is.na(Rank) & Rank > 0)
    
    max_rank <- ceiling(max(df_plot$Rank, na.rm = TRUE))
    
    ggplot(df_plot, aes(x = Section, y = Rank, color = Course, group = Course)) +
      geom_line() +
      geom_point(size = 3) +
      scale_y_reverse(breaks = seq(1, max_rank)) +
      labs(
        title = paste("Classement par section de", input$skieur_1, "sur", input$piste_1),
        x = "Section",
        y = "Place"
      ) +
      theme_minimal()
  })
  
  ###4.1.5 Graphique temps par section (par course) ----
  output$temps_section <- renderPlot({
    req(input$skieur_1, input$course_1)
    
    
    df_concat <- bind_rows(lapply(input$course_1, function(cours) {
      toutes_les_courses[[cours]] %>%
        filter(Name == input$skieur_1) %>%
        mutate(Course = cours)
    }))
    
    if (nrow(df_concat) == 0) return(NULL)
    
    cols <- colnames(df_concat)
    
    cols_sel <- cols[
      grepl("time", cols, ignore.case = TRUE) &
        grepl("sector", cols, ignore.case = TRUE)
    ]
    
    df_long <- df_concat %>%
      select(all_of(cols_sel), Course) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Time_raw"
      ) %>%
      mutate(
        Run = str_extract(Section, "Run [0-9]+"),
        Section_num = as.numeric(str_extract(Section, "(?<=sector )\\d+")),
        Section = paste0("Sector ", Section_num),
        Section = factor(Section, levels = paste0("Sector ", sort(unique(Section_num)))),
        Time_clean = gsub("^\\+", "", Time_raw),
        Time_sec = sapply(Time_clean, convert_time)
      ) %>%
      filter(!is.na(Time_sec))
    
    ggplot(df_long, aes(x = Section, y = Time_sec, color = Course, group = interaction(Course, Run))) +
      geom_line() +
      geom_point(size = 3) +
      geom_text(
        aes(label = sprintf("%d:%05.2f", floor(Time_sec / 60), Time_sec %% 60)),
        vjust = -1, size = 3, show.legend = FALSE
      ) +
      scale_y_continuous(
        labels = function(sec) sprintf("%d:%05.2f", floor(sec / 60), sec %% 60),
        breaks = scales::pretty_breaks()
      ) +
      labs(
        title = paste("Temps par section de", input$skieur_1, "sur", input$piste_1),
        x = "Section",
        y = "Temps (min:sec:cent)"
      ) +
      theme_minimal()
  })
  
  
  ### 4.1.6. Graphique différence de temps par section (par course)----
  
  output$diff_section <- renderPlot({
    req(input$skieur_1, input$course_1)
    
    df_concat <- bind_rows(lapply(input$course_1, function(cours) {
      toutes_les_courses[[cours]] %>%
        filter(Name == input$skieur_1) %>%
        mutate(Course = cours)
    }))
    
    if (nrow(df_concat) == 0) return(NULL)
    
    cols <- colnames(df_concat)
    cols_sel <- cols[
      grepl("diff", cols, ignore.case = TRUE) &
        grepl("sector", cols, ignore.case = TRUE) &
        grepl("Run", cols, ignore.case = TRUE)
    ]
    
    if (length(cols_sel) == 0) return(NULL)
    
    df_long <- df_concat %>%
      select(all_of(cols_sel), Course) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Time_raw"
      ) %>%
      mutate(
        Run = str_extract(Section, "Run [0-9]+"),
        Sector_num = as.numeric(str_extract(Section, "(?<=sector )\\d+")),
        Section = paste0("Sector ", Sector_num),
        Section = factor(Section, levels = paste0("Sector ", sort(unique(Sector_num)))),
        Time_clean = gsub("^\\+", "", Time_raw),
        Time_sec = sapply(Time_clean, convert_time)
      ) %>%
      filter(!is.na(Time_sec))
    
    ggplot(df_long, aes(x = Section, y = Time_sec, color = Course, group = interaction(Course, Run))) +
      geom_line() +
      geom_point(size = 3) +
      geom_text(
        aes(label = sprintf("%d:%05.2f", floor(Time_sec / 60), Time_sec %% 60)),
        vjust = -1, size = 3, show.legend = FALSE
      ) +
      scale_y_continuous(
        labels = function(sec) sprintf("%d:%05.2f", floor(sec / 60), sec %% 60),
        breaks = scales::pretty_breaks()
      ) +
      labs(
        title = paste("Écarts par section de", input$skieur_1, "sur piste", input$piste_1),
        x = "Section",
        y = "Différence (min:sec:cent)"
      ) +
      theme_minimal()
  })
  
  ### 4.1.7. Graphique vitesse par section (par course) ----
  output$vitesse_section <- renderPlot({
    req(input$skieur_1, input$course_1)
    
    df_concat <- bind_rows(lapply(input$course_1, function(cours) {
      df <- toutes_les_courses[[cours]]
      df %>% filter(Name == input$skieur_1) %>% mutate(Course = cours)
    }))
    
    if (nrow(df_concat) == 0) return(NULL)
    
    cols <- colnames(df_concat)
    
    cols_sel <- cols[
      grepl("Run", cols, ignore.case = TRUE) &
        grepl("Speed", cols, ignore.case = TRUE) &
        !grepl("gap", cols, ignore.case = TRUE)
    ]
    
    if (length(cols_sel) == 0) return(NULL)
    
    df_long <- df_concat %>%
      select(all_of(cols_sel), Course) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Speed"
      ) %>%
      mutate(
        Run = str_extract(Section, "Run [0-9]+"),
        Section_num = as.numeric(str_extract(Section, "(?<=Speed )\\d+")),
        Section = paste0("Sector ", Section_num),
        Section = factor(Section, levels = paste0("Sector ", sort(unique(Section_num))))
      ) %>%
      filter(!is.na(Speed))
    
    ggplot(df_long, aes(x = Section, y = Speed, color = Course, group = interaction(Course, Run))) +
      geom_line() +
      geom_point(size = 3) +
      geom_text(
        aes(label = sprintf("%s km/h", Speed)),
        vjust = -1, size = 3, show.legend = FALSE
      ) +
      labs(
        title = paste("Vitesse par section de", input$skieur_1, "sur", input$piste_1),
        x = "Section",
        y = "Vitesse (km/h)"
      ) +
      theme_minimal()
  })
  
  ### 4.1.8 Graphique écarts de vitesse par section (par course) ----
  
  output$vitesse_gap_section <- renderPlot({
    req(input$skieur_1, input$course_1)
    
    df_concat <- bind_rows(lapply(input$course_1, function(cours) {
      df <- toutes_les_courses[[cours]]
      df %>% filter(Name == input$skieur_1) %>% mutate(Course = cours)
    }))
    
    if (nrow(df_concat) == 0) return(NULL)
    
    cols <- colnames(df_concat)
    
    cols_sel <- cols[
      grepl("Run", cols, ignore.case = TRUE) &
        grepl("Speed", cols, ignore.case = TRUE) &
        grepl("gap", cols, ignore.case = TRUE)
    ]
    
    if (length(cols_sel) == 0) return(NULL)
    
    df_long <- df_concat %>%
      select(all_of(cols_sel), Course) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Speed_gap"
      ) %>%
      mutate(
        Speed_gap = as.numeric(Speed_gap),
        Run = str_extract(Section, "Run [0-9]+"),
        Section_num = as.numeric(str_extract(Section, "(?<=Speed )\\d+")),
        Section = paste0("Sector ", Section_num),
        Section = factor(Section, levels = paste0("Sector ", sort(unique(Section_num))))
      ) %>%
      filter(!is.na(Speed_gap))
    
    ggplot(df_long, aes(x = Section, y = Speed_gap, color = Course, group = interaction(Course, Run))) +
      geom_line() +
      geom_point(size = 3) +
      geom_text(
        aes(label = sprintf("%+0.1f km/h", Speed_gap)),
        vjust = -1, size = 3, show.legend = FALSE
      ) +
      labs(
        title = paste("Écart de vitesse par section de", input$skieur_1, "sur", input$piste_1),
        x = "Section",
        y = "Écart de vitesse (km/h)"
      ) +
      theme_minimal()
  })
  
  
  ## 4.2. Contenu page 2 -----
  ### 4.2.1 Graphique classements intermédiaires (par skieur) ----
  
  output$classement_inter_2 <- renderPlot({
    req(input$piste_2, input$course_2, input$skieur_2_selection)
    
    df_course_selection <- toutes_les_courses[[input$course_2]]
    
    cols <- colnames(df_course_selection)
    cols_sel <- cols[
      grepl("rank", cols, ignore.case = TRUE) & 
        grepl("Inter", cols, ignore.case = TRUE) & 
        grepl("Run 1", cols, ignore.case = TRUE)
    ]
    
    finish_col <- grep("finish rank", cols, value = TRUE, ignore.case = TRUE)[1]
    
    df_filtered <- df_course_selection %>% 
      filter(Name %in% input$skieur_2_selection)
    
    if (nrow(df_filtered) == 0 && !isTRUE(input$show_first)) return(NULL)
    
    df_long <- df_filtered %>%
      select(Name, all_of(cols_sel), all_of(finish_col)) %>% 
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Rank"
      ) %>%
      mutate(
        Section = str_extract(Section, "Inter [0-9]+"),
        Section = factor(Section, levels = paste0("Inter ", sort(unique(as.numeric(str_extract(Section, "\\d+")))))),
        Finish_Rank = .data[[finish_col]]
      )
    
    df_finish <- df_filtered %>%
      select(Name, all_of(finish_col)) %>%
      rename(Rank = all_of(finish_col)) %>%
      mutate(
        Section = factor("Finish", levels = c(levels(df_long$Section), "Finish"))
      )
    
    df_plot <- bind_rows(df_long %>% select(Name, Section, Rank), df_finish)
    
    first_name <- NULL
    df_first_plot <- NULL
    if (isTRUE(input$show_first)) {
      skieur_first <- df_course_selection %>%
        filter(!is.na(.data[[finish_col]])) %>%
        arrange(.data[[finish_col]], Name) %>%
        slice(1)
      
      first_name <- skieur_first$Name[1]
      
      df_first_long <- skieur_first %>%
        select(all_of(cols_sel), Name) %>%
        pivot_longer(
          cols = all_of(cols_sel),
          names_to = "Section",
          values_to = "Rank"
        ) %>%
        mutate(
          Section = str_extract(Section, "Inter [0-9]+"),
          Section = factor(Section, levels = levels(df_long$Section)),
          Name = paste0(Name, " (premier)")
        )
      
      df_first_finish <- tibble(
        Name = paste0(first_name, " (premier)"),
        Section = factor("Finish", levels = levels(df_finish$Section)),
        Rank = skieur_first[[finish_col]][1]
      )
      
      df_first_plot <- bind_rows(df_first_long, df_first_finish)
      df_plot <- bind_rows(df_plot, df_first_plot)
    }
    
    max_rank <- ceiling(max(df_plot$Rank, na.rm = TRUE))
    
    ggplot(df_plot, aes(x = Section, y = Rank, group = Name)) +
      geom_line(aes(color = Name)) +
      geom_point(aes(color = Name), size = 3) +
      scale_y_reverse(breaks = seq(1, max_rank)) +
      labs(
        title = paste("Classements intermédiaires pour", input$course_2),
        x = "Section",
        y = "Place",
        color = "Skieur"
      ) +
      theme_minimal() + 
      scale_color_manual(values = {
        skieurs <- unique(df_plot$Name)
        vals <- rainbow(length(skieurs))
        if (!is.null(first_name)) {
          vals[skieurs == paste0(first_name, " (premier)")] <- "red"
        }
        setNames(vals, skieurs)
      }) +
      {
        if (!is.null(first_name)) {
          geom_text(
            data = df_first_finish,
            aes(x = Section, y = Rank, label = Name),
            vjust = -0.8,
            color = "red",
            fontface = "bold",
            inherit.aes = FALSE
          )
        }
      }
  })
  
  
  ### 4.2.2 Graphique temps intermédiaires (par skieur) ----
  
  output$temps_inter_2 <- renderPlot({
    req(input$skieur_2_selection, input$course_2)
    
    df_course <- toutes_les_courses[[input$course_2]]
    
    df_concat <- df_course %>%
      filter(Name %in% input$skieur_2_selection) %>%
      mutate(Course = input$course_2)
    
    if (nrow(df_concat) == 0) return(NULL)
    
    cols <- colnames(df_course)
    cols_sel <- cols[
      grepl("time", cols, ignore.case = TRUE) &
        grepl("Inter", cols, ignore.case = TRUE) &
        grepl("Run", cols, ignore.case = TRUE)
    ]
    finish_cols <- grep("Run [0-9]+ finish time", cols, value = TRUE)
    
    df_long <- df_concat %>%
      select(Name, Course, all_of(cols_sel), all_of(finish_cols)) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Time_raw"
      ) %>%
      mutate(
        Run = gsub(".*(Run [0-9]+).*", "\\1", Section),
        Section = gsub(".*(Inter[ ]?[0-9]+).*", "\\1", Section),
        Section = factor(Section, levels = sort(unique(Section))),
        Time_sec = sapply(Time_raw, convert_time)
      )
    
    df_finish_list <- lapply(finish_cols, function(col) {
      df_concat %>%
        select(Name, Course, Time_raw = all_of(col)) %>%
        mutate(
          Run = gsub("Run ([0-9]+) finish time", "Run \\1", col),
          Section = "Finish",
          Time_sec = sapply(Time_raw, convert_time),
          Section = factor(Section, levels = c(levels(df_long$Section), "Finish"))
        )
    })
    
    df_finish <- bind_rows(df_finish_list)
    df_plot <- bind_rows(df_long, df_finish)
    
    first_name <- NULL
    if (isTRUE(input$show_first)) {
      sk_first <- df_course %>%
        filter(!is.na(.data[[finish_cols[1]]])) %>%
        arrange(.data[[finish_cols[1]]], Name) %>%
        slice(1)
      
      if (nrow(sk_first) > 0) {
        first_name <- sk_first$Name[1]
        
        # Ajout du premier à df_plot si pas déjà présent
        if (!(first_name %in% df_plot$Name)) {
          df_first_concat <- df_course %>%
            filter(Name == first_name) %>%
            mutate(Course = input$course_2)
          
          df_first_long <- df_first_concat %>%
            select(Name, Course, all_of(cols_sel), all_of(finish_cols)) %>%
            pivot_longer(
              cols = all_of(cols_sel),
              names_to = "Section",
              values_to = "Time_raw"
            ) %>%
            mutate(
              Run = gsub(".*(Run [0-9]+).*", "\\1", Section),
              Section = gsub(".*(Inter[ ]?[0-9]+).*", "\\1", Section),
              Section = factor(Section, levels = sort(unique(Section))),
              Time_sec = sapply(Time_raw, convert_time)
            )
          
          df_first_finish_list <- lapply(finish_cols, function(col) {
            df_first_concat %>%
              select(Name, Course, Time_raw = all_of(col)) %>%
              mutate(
                Run = gsub("Run ([0-9]+) finish time", "Run \\1", col),
                Section = "Finish",
                Time_sec = sapply(Time_raw, convert_time),
                Section = factor(Section, levels = c(levels(df_first_long$Section), "Finish"))
              )
          })
          
          df_first_finish <- bind_rows(df_first_finish_list)
          df_plot <- bind_rows(df_plot, bind_rows(df_first_long, df_first_finish))
        }
        
        df_plot <- df_plot %>%
          mutate(
            Name = ifelse(Name == first_name, paste0(Name, " (premier)"), Name),
            IsFirst = Name == paste0(first_name, " (premier)")
          )
      } else {
        df_plot$IsFirst <- FALSE
      }
    } else {
      df_plot$IsFirst <- FALSE
    }
    
    max_time <- max(df_plot$Time_sec, na.rm = TRUE)
    
    if (!is.null(first_name)) {
      legend_names <- levels(factor(df_plot$Name))
      unique_colors <- setNames(RColorBrewer::brewer.pal(max(3, length(legend_names)), "Set1")[seq_along(legend_names)], legend_names)
      unique_colors[paste0(first_name, " (premier)")] <- "red"
      color_scale <- scale_color_manual(values = unique_colors)
    } else {
      color_scale <- scale_color_brewer(palette = "Set1")
    }
    
    p <- ggplot(df_plot, aes(x = Section, y = Time_sec, color = Name, group = interaction(Name, Run))) +
      geom_line(aes(size = IsFirst)) +
      geom_point(size = 3) +
      geom_text(
        aes(label = ifelse(is.na(Time_sec), "", sprintf("%d:%05.2f", floor(Time_sec / 60), Time_sec %% 60))),
        vjust = -1, size = 3, show.legend = FALSE
      ) +
      scale_size_manual(values = c("TRUE" = 1.2, "FALSE" = 0.8), guide = "none") +
      scale_y_continuous(
        labels = function(sec) sprintf("%d:%05.2f", floor(sec / 60), sec %% 60),
        breaks = scales::pretty_breaks(),
        limits = c(0, max_time * 1.05)
      ) +
      scale_color_manual(values = {
        skieurs <- unique(df_plot$Name)
        vals <- rainbow(length(skieurs))
        if (!is.null(first_name)) {
          vals[skieurs == paste0(first_name, " (premier)")] <- "red"
        }
        setNames(vals, skieurs)
      })+
      labs(
        title = paste("Temps intermédiaires sur", input$course_2),
        x = "Inter", y = "Temps (min:sec:cent)", color = "Skieur"
      ) +
      theme_minimal()
    
    if (!is.null(first_name)) {
      df_first_finish <- df_plot %>%
        filter(IsFirst, Section == "Finish") %>%
        distinct(Name, Section, Time_sec, Run)
      
      p <- p +
        geom_text(
          data = df_first_finish,
          aes(label = Name, x = Section, y = Time_sec),
          inherit.aes = FALSE,
          vjust = -0.8,
          color = "red",
          fontface = "bold"
        )
    }
    
    p
  })
  
### 4.2.3 Graphique écart intermédiaires (par skieur) ----
  output$gap_inter_2 <- renderPlot({
    req(input$course_2, input$skieur_2_selection)
    
    df_course <- toutes_les_courses[[input$course_2]]
    finish_col <- grep("finish rank", colnames(df_course), value = TRUE, ignore.case = TRUE)[1]
    
    skieur_first <- df_course %>% slice_min(.data[[finish_col]], n = 1)
    first_name <- skieur_first$Name[1]
    
    df_selected <- df_course %>% filter(Name %in% input$skieur_2_selection)
    
    cols_gap <- colnames(df_course)[
      grepl("gap", colnames(df_course), ignore.case = TRUE) &
        grepl("Inter", colnames(df_course), ignore.case = TRUE) &
        grepl("Run", colnames(df_course), ignore.case = TRUE)
    ]
    
    df_long <- df_selected %>%
      select(Name, all_of(cols_gap)) %>%
      pivot_longer(cols = all_of(cols_gap), names_to = "Section", values_to = "Time_raw") %>%
      mutate(
        Run = str_extract(Section, "Run [0-9]+"),
        Section_num = as.numeric(str_extract(Section, "(?<=Inter\\s?)\\d+")),
        Section_clean = factor(paste0("Inter ", Section_num)),
        Time_clean = gsub("^\\+", "", Time_raw),
        Time_sec = as.numeric(sapply(Time_clean, convert_time))
      ) %>%
      select(Name, Run, Section_num, Section_clean, Time_sec)
    
    df_finish <- df_selected %>%
      select(Name, all_of(finish_col)) %>%
      pivot_longer(cols = all_of(finish_col), names_to = "Section", values_to = "Time_raw") %>%
      mutate(
        Run = str_extract(Section, "Run [0-9]+"),
        Section_num = max(df_long$Section_num, na.rm = TRUE) + 1,
        Section_clean = factor("Finish", levels = c(levels(df_long$Section_clean), "Finish")),
        Time_clean = gsub("^\\+", "", Time_raw),
        Time_sec = as.numeric(sapply(Time_clean, convert_time))
      ) %>%
      select(Name, Run, Section_num, Section_clean, Time_sec)
    
    df_plot <- bind_rows(df_long, df_finish)
    
    if (isTRUE(input$show_first)) {
      df_first_long <- skieur_first %>%
        select(Name, all_of(cols_gap)) %>%
        pivot_longer(cols = all_of(cols_gap), names_to = "Section", values_to = "Time_raw") %>%
        mutate(
          Run = str_extract(Section, "Run [0-9]+"),
          Section_num = as.numeric(str_extract(Section, "(?<=Inter\\s?)\\d+")),
          Section_clean = factor(paste0("Inter ", Section_num), levels = levels(df_long$Section_clean)),
          Time_clean = gsub("^\\+", "", Time_raw),
          Time_sec = as.numeric(sapply(Time_clean, convert_time)),
          Name = paste0(Name, " (premier)")
        ) %>%
        select(Name, Run, Section_num, Section_clean, Time_sec)
      
      df_first_finish <- skieur_first %>%
        select(Name, all_of(finish_col)) %>%
        pivot_longer(cols = all_of(finish_col), names_to = "Section", values_to = "Time_raw") %>%
        mutate(
          Run = str_extract(Section, "Run [0-9]+"),
          Section_num = max(df_long$Section_num, na.rm = TRUE) + 1,
          Section_clean = factor("Finish", levels = c(levels(df_long$Section_clean), "Finish")),
          Time_clean = gsub("^\\+", "", Time_raw),
          Time_sec = as.numeric(sapply(Time_clean, convert_time)),
          Name = paste0(Name, " (premier)")
        ) %>%
        select(Name, Run, Section_num, Section_clean, Time_sec)
      
      df_plot <- bind_rows(df_plot, df_first_long, df_first_finish)
    }
    
    skieurs_unique <- unique(df_plot$Name)
    n_colors <- max(length(skieurs_unique), 3)
    colors <- RColorBrewer::brewer.pal(n_colors, "Set1")
    names(colors) <- skieurs_unique
    if (isTRUE(input$show_first)) colors[paste0(first_name, " (premier)")] <- "red"
    
    ggplot(df_plot, aes(x = Section_clean, y = Time_sec, group = Name)) +
      geom_line(aes(color = Name), size = 1) +
      geom_point(aes(color = Name), size = 3) +
      scale_y_continuous(
        labels = function(sec) sprintf("%d:%05.2f", floor(sec / 60), sec %% 60),
        breaks = scales::pretty_breaks()
      ) +
      labs(
        title = paste("Écarts intermédiaires pour", input$course_2),
        x = "Section",
        y = "Écart (min:sec:cent)",
        color = "Skieur"
      ) +
      scale_color_manual(values = {
        skieurs <- unique(df_plot$Name)
        vals <- rainbow(length(skieurs))
        if (!is.null(first_name)) {
          vals[skieurs == paste0(first_name, " (premier)")] <- "red"
        }
        setNames(vals, skieurs)
      })+
      theme_minimal()
  })
  
  
  ### 4.2.4. Graphique classements par section (par skieur) ----
  output$classement_section_2 <- renderPlot({
    req(input$skieur_2_selection, input$course_2)
    
    df_course_selection <- toutes_les_courses[[input$course_2]]
    
    cols <- colnames(df_course_selection)
    cols_sel <- cols[
      grepl("rank", cols, ignore.case = TRUE) & 
        grepl("sector", cols, ignore.case = TRUE)
    ]
    
    finish_col <- grep("finish rank", cols, value = TRUE, ignore.case = TRUE)[1]
    
    df_filtered <- df_course_selection %>%
      filter(Name %in% input$skieur_2_selection)
    
    if (nrow(df_filtered) == 0 && !isTRUE(input$show_first)) return(NULL)
    
    df_long <- df_filtered %>%
      select(Name, all_of(cols_sel), all_of(finish_col)) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Rank"
      ) %>%
      mutate(
        Section_num = as.numeric(gsub(".*sector[ ]?([0-9]+).*", "\\1", Section, ignore.case = TRUE)),
        Section = paste0("Sector ", Section_num)
      )
    
    sector_levels <- paste0("Sector ", sort(unique(df_long$Section_num)))
    section_levels <- c(sector_levels, "Finish")
    df_long$Section <- factor(df_long$Section, levels = section_levels)
    
    df_finish <- df_filtered %>%
      select(Name, all_of(finish_col)) %>%
      rename(Rank = all_of(finish_col)) %>%
      mutate(
        Section = factor("Finish", levels = section_levels)
      )
    
    df_plot <- bind_rows(df_long %>% select(Name, Section, Rank), df_finish)
    
    first_name <- NULL
    if (isTRUE(input$show_first)) {
      skieur_first <- df_course_selection %>%
        filter(!is.na(.data[[finish_col]])) %>%
        arrange(.data[[finish_col]], Name) %>%
        slice(1)
      
      first_name <- skieur_first$Name[1]
      
      df_first_long <- skieur_first %>%
        select(all_of(cols_sel), Name) %>%
        pivot_longer(
          cols = all_of(cols_sel),
          names_to = "Section",
          values_to = "Rank"
        ) %>%
        mutate(
          Section_num = as.numeric(gsub(".*sector[ ]?([0-9]+).*", "\\1", Section, ignore.case = TRUE)),
          Section = paste0("Sector ", Section_num),
          Section = factor(Section, levels = sector_levels),
          Name = paste0(Name, " (premier)")
        )
      
      df_first_finish <- tibble(
        Name = paste0(first_name, " (premier)"),
        Section = factor("Finish", levels = section_levels),
        Rank = skieur_first[[finish_col]][1]
      )
      
      df_first_plot <- bind_rows(df_first_long, df_first_finish)
      df_plot <- bind_rows(df_plot, df_first_plot)
    }
    
    max_rank <- ceiling(max(df_plot$Rank, na.rm = TRUE))
    
    ggplot(df_plot, aes(x = Section, y = Rank, group = Name)) +
      geom_line(aes(color = Name)) +
      geom_point(aes(color = Name), size = 3) +
      scale_y_reverse(breaks = seq(1, max_rank)) +
      labs(
        title = paste("Classements par section pour", input$course_2),
        x = "Section",
        y = "Place",
        color = "Skieur"
      ) +
      theme_minimal() +
      scale_color_manual(values = {
        skieurs <- unique(df_plot$Name)
        vals <- rainbow(length(skieurs))
        if (!is.null(first_name)) {
          vals[skieurs == paste0(first_name, " (premier)")] <- "red"
        }
        setNames(vals, skieurs)
      }) +
      {
        if (!is.null(first_name)) {
          geom_text(
            data = df_first_finish,
            aes(x = Section, y = Rank, label = Name),
            vjust = -0.8,
            color = "red",
            fontface = "bold",
            inherit.aes = FALSE
          )
        }
      }
  })
  
 ### 4.2.5 Graphique temps par section (par skieur) ----
  output$temps_section_2 <- renderPlot({
    req(input$skieur_2_selection, input$course_2)
    
    df_course <- toutes_les_courses[[input$course_2]]
    
    cols <- colnames(df_course)
    cols_sel <- cols[
      grepl("time", cols, ignore.case = TRUE) &
        grepl("sector", cols, ignore.case = TRUE)
    ]
    
    finish_col <- grep("finish time", cols, value = TRUE, ignore.case = TRUE)[1]
    
    df_filtered <- df_course %>%
      filter(Name %in% input$skieur_2_selection)
    
    if (nrow(df_filtered) == 0 && !isTRUE(input$show_first)) return(NULL)
    
    df_long <- df_filtered %>%
      select(Name, all_of(cols_sel), all_of(finish_col)) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Time_raw"
      ) %>%
      mutate(
        Section_num = as.numeric(gsub(".*sector[ ]?([0-9]+).*", "\\1", Section, ignore.case = TRUE)),
        Section = paste0("Sector ", Section_num),
        Time_clean = gsub("^\\+", "", Time_raw),
        Time_sec = sapply(Time_clean, convert_time)
      ) %>%
      filter(!is.na(Time_sec))
    
    sector_levels <- paste0("Sector ", sort(unique(df_long$Section_num)))
    section_levels <- c(sector_levels, "Finish")
    
    df_finish <- df_filtered %>%
      select(Name, all_of(finish_col)) %>%
      rename(Time_sec = all_of(finish_col)) %>%
      mutate(
        Section = factor("Finish", levels = section_levels),
        Time_sec = sapply(Time_sec, convert_time)
      )
    
    df_plot <- bind_rows(df_long %>% select(Name, Section, Time_sec), df_finish)
    
    first_name <- NULL
    if (isTRUE(input$show_first)) {
      skieur_first <- df_course %>%
        filter(!is.na(.data[[finish_col]])) %>%
        arrange(.data[[finish_col]], Name) %>%
        slice(1)
      
      first_name <- skieur_first$Name[1]
      
      df_first_long <- skieur_first %>%
        select(all_of(cols_sel), Name) %>%
        pivot_longer(
          cols = all_of(cols_sel),
          names_to = "Section",
          values_to = "Time_raw"
        ) %>%
        mutate(
          Section_num = as.numeric(gsub(".*sector[ ]?([0-9]+).*", "\\1", Section, ignore.case = TRUE)),
          Section = paste0("Sector ", Section_num),
          Time_clean = gsub("^\\+", "", Time_raw),
          Time_sec = sapply(Time_clean, convert_time),
          Name = paste0(Name, " (premier)")
        )
      
      df_first_finish <- tibble(
        Name = paste0(first_name, " (premier)"),
        Section = factor("Finish", levels = section_levels),
        Time_sec = convert_time(skieur_first[[finish_col]][1])
      )
      
      df_first_plot <- bind_rows(df_first_long %>% select(Name, Section, Time_sec), df_first_finish)
      df_plot <- bind_rows(df_plot, df_first_plot)
    }
    
    df_plot$Section <- factor(df_plot$Section, levels = section_levels)
    
    ggplot(df_plot, aes(x = Section, y = Time_sec, group = Name)) +
      geom_line(aes(color = Name)) +
      geom_point(aes(color = Name), size = 3) +
      geom_text(aes(label = sprintf("%d:%05.2f", floor(Time_sec / 60), Time_sec %% 60)),
                vjust = -1, size = 3, show.legend = FALSE) +
      scale_color_manual(values = {
        skieurs <- unique(df_plot$Name)
        vals <- rainbow(length(skieurs))
        if (!is.null(first_name)) {
          vals[skieurs == paste0(first_name, " (premier)")] <- "red"
        }
        setNames(vals, skieurs)
      }) +
      scale_y_continuous(
        labels = function(sec) sprintf("%d:%05.2f", floor(sec / 60), sec %% 60),
        breaks = scales::pretty_breaks()
      ) +
      labs(
        title = paste("Temps par section pour", input$course_2),
        x = "Section",
        y = "Temps (min:sec:cent)",
        color = "Skieur"
      ) +
      theme_minimal()
  })
  
  ### 4.2.6 Graphique écarts par section (par skieur) ----
  output$diff_section_2 <- renderPlot({
    req(input$skieur_2_selection, input$course_2)
    
    df_course <- toutes_les_courses[[input$course_2]]
    cols <- colnames(df_course)
    cols_sel <- cols[
      grepl("diff", cols, ignore.case = TRUE) &
        grepl("sector", cols, ignore.case = TRUE)
    ]
    df_filtered <- df_course %>%
      filter(Name %in% input$skieur_2_selection)
    if (nrow(df_filtered) == 0 && !isTRUE(input$show_first)) return(NULL)
    
    df_long <- df_filtered %>%
      select(Name, all_of(cols_sel)) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Time_raw"
      ) %>%
      mutate(
        Section_num = as.numeric(gsub(".*sector[ ]?([0-9]+).*", "\\1", Section, ignore.case = TRUE)),
        Section = paste0("Sector ", Section_num),
        Time_clean = gsub("^\\+", "", Time_raw),
        Time_sec = sapply(Time_clean, convert_time)
      ) %>%
      filter(!is.na(Time_sec))
    
    sector_levels <- paste0("Sector ", sort(unique(df_long$Section_num)))
    df_plot <- df_long %>% select(Name, Section, Time_sec)
    
    finish_col <- grep("finish rank", cols, value = TRUE, ignore.case = TRUE)[1]
    first_name <- NULL
    
    if (isTRUE(input$show_first)) {
      skieur_first <- df_course %>%
        filter(!is.na(.data[[finish_col]])) %>%
        arrange(.data[[finish_col]], Name) %>%
        slice(1)
      first_name <- skieur_first$Name[1]
      
      df_first_long <- skieur_first %>%
        select(Name, all_of(cols_sel)) %>%
        pivot_longer(
          cols = all_of(cols_sel),
          names_to = "Section",
          values_to = "Time_raw"
        ) %>%
        mutate(
          Section_num = as.numeric(gsub(".*sector[ ]?([0-9]+).*", "\\1", Section, ignore.case = TRUE)),
          Section = paste0("Sector ", Section_num),
          Time_clean = gsub("^\\+", "", Time_raw),
          Time_sec = sapply(Time_clean, convert_time),
          Name = paste0(Name, " (premier)")
        )
      
      df_plot <- bind_rows(df_plot, df_first_long %>% select(Name, Section, Time_sec))
    }
    
    df_plot$Section <- factor(df_plot$Section, levels = sector_levels)
    
    ggplot(df_plot, aes(x = Section, y = Time_sec, group = Name)) +
      geom_line(aes(color = Name)) +
      geom_point(aes(color = Name), size = 3) +
      geom_text(aes(label = sprintf("%d:%05.2f", floor(Time_sec / 60), Time_sec %% 60)),
                vjust = -1, size = 3, show.legend = FALSE) +
      scale_color_manual(values = {
        skieurs <- unique(df_plot$Name)
        vals <- rainbow(length(skieurs))
        if (!is.null(first_name)) {
          vals[skieurs == paste0(first_name, " (premier)")] <- "red"
        }
        setNames(vals, skieurs)
      }) +
      scale_y_continuous(
        labels = function(sec) sprintf("%d:%05.2f", floor(sec / 60), sec %% 60),
        breaks = scales::pretty_breaks()
      ) +
      labs(
        title = paste("Écarts par section pour", input$course_2),
        x = "Section",
        y = "Différence (min:sec:cent)",
        color = "Skieur"
      ) +
      theme_minimal()
  })
  
  ### 4.2.7 Graphique vitesse par section (par skieur) ----
  output$vitesse_section_2 <- renderPlot({
    req(input$skieur_2_selection, input$course_2)
    
      df_concat <- bind_rows(lapply(input$course_2, function(cours) {
      df <- toutes_les_courses[[cours]]
      df %>% filter(Name %in% input$skieur_2_selection)%>% mutate(Course = cours)
    }))
    if (nrow(df_concat) == 0) return(NULL)
    
    cols <- colnames(df_concat)
    cols_sel <- cols[
      grepl("Run", cols, ignore.case = TRUE) &
        grepl("Speed", cols, ignore.case = TRUE) &
        !grepl("gap", cols, ignore.case = TRUE)
    ]
    if (length(cols_sel) == 0) return(NULL)
    
    df_long <- df_concat %>%
      select(all_of(cols_sel), Course, Name) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Speed"
      ) %>%
      mutate(
        Run = str_extract(Section, "Run [0-9]+"),
        Section_num = as.numeric(str_extract(Section, "(?<=Speed )\\d+")),
        Section = paste0("Sector ", Section_num),
        Section = factor(Section, levels = paste0("Sector ", sort(unique(Section_num))))
      ) %>%
      filter(!is.na(Speed))
    
    first_name <- NULL
    if (isTRUE(input$show_first)) {
      df_course_all <- lapply(input$course_2, function(cours) toutes_les_courses[[cours]]) %>%
        bind_rows()
      finish_col <- grep("finish rank", colnames(df_course_all), value = TRUE, ignore.case = TRUE)[1]
      skieur_first <- df_course_all %>%
        filter(!is.na(.data[[finish_col]])) %>%
        arrange(.data[[finish_col]], Name) %>%
        slice(1)
      first_name <- skieur_first$Name[1]
      
      df_first_long <- skieur_first %>%
        select(Name, all_of(cols_sel)) %>%
        pivot_longer(
          cols = all_of(cols_sel),
          names_to = "Section",
          values_to = "Speed"
        ) %>%
        mutate(
          Run = str_extract(Section, "Run [0-9]+"),
          Section_num = as.numeric(str_extract(Section, "(?<=Speed )\\d+")),
          Section = paste0("Sector ", Section_num),
          Section = factor(Section, levels = levels(df_long$Section)),
          Name = paste0(Name, " (premier)")
        )
      
      df_long <- bind_rows(df_long, df_first_long)
    }
    
      ggplot(df_long, aes(x = Section, y = Speed, color = Name, group = interaction(Name, Run))) +
      geom_line() +
      geom_point(size = 3) +
      geom_text(
        aes(label = sprintf("%s km/h", Speed)),
        vjust = -1, size = 3, show.legend = FALSE
      ) +
      scale_color_manual(values = {
        skieurs <- unique(df_long$Name)
        vals <- rainbow(length(skieurs))
        if (!is.null(first_name)) vals[skieurs == paste0(first_name, " (premier)")] <- "red"
        setNames(vals, skieurs)
      }) +
      labs(
        title = paste("Vitesse par section de", input$skieur_2, "sur", paste(input$course_2, collapse = ", ")),
        x = "Section",
        y = "Vitesse (km/h)",
        color = "Skieur"
      ) +
      theme_minimal()
  })
  
  ### 4.2.8 Graphique écart vitesse par section (par skieur)----
  
  output$vitesse_gap_section_2 <- renderPlot({
    req(input$skieur_2_selection, input$course_2)
    
    df_concat <- bind_rows(lapply(input$course_2, function(cours) {
      df <- toutes_les_courses[[cours]]
      df %>% filter(Name %in% input$skieur_2_selection) %>% mutate(Course = cours)
    }))
    
    if (nrow(df_concat) == 0) return(NULL)
    
    cols <- colnames(df_concat)
    
    cols_sel <- cols[
      grepl("Run", cols, ignore.case = TRUE) &
        grepl("Speed", cols, ignore.case = TRUE) &
        grepl("gap", cols, ignore.case = TRUE)
    ]
    if (length(cols_sel) == 0) return(NULL)
    
    df_long <- df_concat %>%
      select(all_of(cols_sel), Course, Name) %>%
      pivot_longer(
        cols = all_of(cols_sel),
        names_to = "Section",
        values_to = "Speed_gap"
      ) %>%
      mutate(
        Speed_gap = as.numeric(Speed_gap),
        Run = str_extract(Section, "Run [0-9]+"),
        Section_num = as.numeric(str_extract(Section, "(?<=Speed )\\d+")),
        Section = paste0("Sector ", Section_num),
        Section = factor(Section, levels = paste0("Sector ", sort(unique(Section_num))))
      ) %>%
      filter(!is.na(Speed_gap))
    
    first_name <- NULL
    if (isTRUE(input$show_first)) {
      df_course_all <- lapply(input$course_2, function(cours) toutes_les_courses[[cours]]) %>%
        bind_rows()
      finish_col <- grep("finish rank", colnames(df_course_all), value = TRUE, ignore.case = TRUE)[1]
      skieur_first <- df_course_all %>%
        filter(!is.na(.data[[finish_col]])) %>%
        arrange(.data[[finish_col]], Name) %>%
        slice(1)
      first_name <- skieur_first$Name[1]
      
      df_first_long <- skieur_first %>%
        select(Name, all_of(cols_sel)) %>%
        pivot_longer(
          cols = all_of(cols_sel),
          names_to = "Section",
          values_to = "Speed_gap"
        ) %>%
        mutate(
          Speed_gap = as.numeric(Speed_gap),
          Run = str_extract(Section, "Run [0-9]+"),
          Section_num = as.numeric(str_extract(Section, "(?<=Speed )\\d+")),
          Section = paste0("Sector ", Section_num),
          Section = factor(Section, levels = levels(df_long$Section)),
          Name = paste0(Name, " (premier)")
        )
      
      df_long <- bind_rows(df_long, df_first_long)
    }
    
    ggplot(df_long, aes(x = Section, y = Speed_gap, color = Name, group = interaction(Name, Run))) +
      geom_line() +
      geom_point(size = 3) +
      geom_text(
        aes(label = sprintf("%+0.1f km/h", Speed_gap)),
        vjust = -1, size = 3, show.legend = FALSE
      ) +
      scale_color_manual(values = {
        skieurs <- unique(df_long$Name)
        vals <- rainbow(length(skieurs))
        if (!is.null(first_name)) vals[skieurs == paste0(first_name, " (premier)")] <- "red"
        setNames(vals, skieurs)
      }) +
      labs(
        title = paste("Écart de vitesse par section sur", input$course_2),
        x = "Section",
        y = "Écart de vitesse (km/h)",
        color = "Skieur"
      ) +
      theme_minimal()
  })
  
}

############# 5. Lancement de l'application ####################################
shinyApp(ui = ui, server = server)
