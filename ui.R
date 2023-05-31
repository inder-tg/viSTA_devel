
source(paste0(getwd(), "/auxPackages.R"))
source(paste0(getwd(), "/auxFunctions.R"))
source(paste0(getwd(), "/auxObjects.R"))

ui <- dashboardPage(
  
  dashboardHeader(title="viSTA - Mexico"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome!", tabName = "generalView", icon = icon("home")),
      menuItem("Territories", tabName = "Maps", icon = icon("chart-line")),
      menuItem("Contact", tabName = "contactUs", icon = icon("envelope")),
      menuItem("Local exploration", tabName="local", icon=icon("user"),
               selectInput(inputId="localInput", 
                           label="Choose a ROI",
                           choices=c("Calakmul")#,
                                     # "Hauxamanaka",
                                     # "Tatie Matinieri", "Teekata",
                                     # "Wirikuta", "Xapawiyeme",
                                     # "Xurahue Muyaca")
                           )#,
               # bsButton("start", "START",  icon = icon("play"),
               #              style="color: #fff; background-color: #2ca25f; border-color: #2e6da4")
               )
      
    )
  ),
  
  dashboardBody(

    useShinyjs(),
    introjsUI(),

    fluidRow(
      column(12,
        introBox(
          bsButton("map",
            label = "Etno",
            icon = icon("user", class="opt", verify_fa=FALSE),
            style = "success"
          ),
          bsButton("plots",
            label = "Graphs",
            icon = icon("user", class="opt", verify_fa=FALSE),
            style = "success"
          ),
          bsButton("sta",
            label = "SeasonalTrends",
            icon = icon("user", class="opt", verify_fa=FALSE),
            style = "success"
          )#,
          # bsButton("sephora",
          #   label = "PhenologicalParameters",
          #   icon = icon("user"),
          #   style = "success"
          # )
          # data.step = 2#, data.intro = "This is a test"
        )
      )
    ),

    fluid_design("plots_panel", "box1", "box2", "box3", "box4"),
    fluid_design("sta_panel", "sta1", "sta2", "sta3", "sta4"),
    # fluid_design("sephora_panel", "seph1", "seph2", "seph3", "seph4"),

    fluidRow(
      div(
        id="map_panel",
        column(12,
          uiOutput("box_map")
        )
      )
    )

  )

)