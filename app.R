library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)

# ------------------------------------------
# DATA PREPARATION (WHO Growth Standards)
# ------------------------------------------

# Data WHO (simplified)
who_data <- data.frame(
  age_months = rep(c(0, 3, 6, 9, 12, 15, 18, 24, 36, 48, 60), 2),
  sex = rep(c("Male", "Female"), each = 11),
  median_tb = c(
    # Laki-laki
    49.9, 58.4, 65.7, 70.6, 75.7, 78.4, 80.7, 87.8, 94.9, 102.9, 110.0,
    # Perempuan
    49.1, 57.3, 64.3, 69.8, 74.0, 76.6, 79.2, 86.4, 93.9, 101.6, 108.4
  ),
  sd_tb = rep(c(1.8, 2.4, 2.7, 2.9, 3.3, 3.2, 3.3, 3.4, 3.8, 4.3, 4.9), 2)
)

# Complete data stunting di Indonesia dengan jumlah balita dan kasus stunting
# Complete data stunting di Indonesia dengan jumlah balita dan kasus stunting
stunting_data_indonesia <- data.frame(
  provinsi = c("ACEH", "SUMATERA UTARA", "SUMATERA BARAT", "RIAU", "JAMBI", "SUMATERA SELATAN", 
               "BENGKULU", "LAMPUNG", "KEPULAUAN BANGKA BELITUNG", "KEPULAUAN RIAU", "DKI JAKARTA", 
               "JAWA BARAT", "JAWA TENGAH", "DI YOGYAKARTA", "JAWA TIMUR", "BANTEN", "BALI", 
               "NUSA TENGGARA BARAT", "NUSA TENGGARA TIMUR", "KALIMANTAN BARAT", "KALIMANTAN TENGAH", 
               "KALIMANTAN SELATAN", "KALIMANTAN TIMUR", "KALIMANTAN UTARA", "SULAWESI UTARA", 
               "SULAWESI TENGAH", "SULAWESI SELATAN", "SULAWESI TENGGARA", "GORONTALO", "SULAWESI BARAT", 
               "MALUKU", "MALUKU UTARA", "PAPUA BARAT", "PAPUA", "PAPUA PEGUNUNGAN", "PAPUA SELATAN", 
               "PAPUA TENGAH", "PAPUA BARAT DAYA"),
  jml_balita = c(394129, 946365, 377410, 368260, 209492, 588096, 107916, 532246, 97246, 118145, 
                 361697, 3087192, 1940103, 163458, 2213827, 760984, 176827, 441000, 421347, 357582, 
                 113911, 259391, 196073, 45742, 131656, 167399, 564938, 157178, 84718, 89544, 
                 132847, 80164, 28128, 45583, 35146, 24201, 55031, 29252),
  jml_stunting = c(24603, 28089, 30624, 9178, 5709, 8178, 4206, 14714, 3164, 3720, 5645, 
                   152308, 167234, 12531, 128673, 22010, 6153, 55810, 62386, 31135, 12594, 
                   21201, 17633, 3058, 2204, 17398, 41191, 16143, 4539, 21432, 7205, 6962, 
                   2621, 4495, 1554, 3062, 5575, 2282),
  pendek = c(18641, 19631, 23481, 6877, 4058, 5818, 3386, 11047, 2363, 2858, 3858, 
             114430, 132359, 9728, 99776, 15707, 4871, 42365, 44058, 22686, 9480, 
             15915, 13151, 2222, 1638, 11740, 30222, 12291, 3277, 15818, 5561, 5335, 
             1926, 3481, 895, 2141, 3877, 1591),
  sangat_pendek = c(5962, 8458, 7143, 2301, 1651, 2360, 820, 3667, 801, 862, 1787, 
                    37878, 34875, 2803, 28897, 6303, 1282, 13445, 18328, 8449, 3114, 
                    5286, 4482, 836, 566, 5658, 10969, 3852, 1262, 5614, 1644, 1627, 
                    695, 1014, 659, 921, 1698, 691),
  prevalensi = c(6.2, 3.0, 8.1, 2.5, 2.7, 1.4, 3.9, 2.8, 3.3, 3.1, 1.6, 4.9, 8.6, 7.7, 
                 5.8, 2.9, 3.5, 12.7, 14.8, 8.7, 11.1, 8.2, 9.0, 6.7, 1.7, 10.4, 7.3, 
                 10.3, 5.4, 23.9, 5.4, 8.7, 9.3, 9.9, 4.4, 12.7, 10.1, 7.8),
  tahun = rep(2023, 38)
)

# Calculate total row and append to the dataframe
total_row <- data.frame(
  provinsi = "TOTAL",
  jml_balita = 15904224,
  jml_stunting = 728559 + 238660, # Sum of pendek and sangat pendek
  pendek = 728559,
  sangat_pendek = 238660,
  prevalensi = 6.1,
  tahun = 2023
)

stunting_data_indonesia <- rbind(stunting_data_indonesia, total_row)


# ------------------------------------------
# UI DEFINITION
# ------------------------------------------

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(icon("child-reaching"), "NutriTrack | Deteksi Dini Stunting"),
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 300,
    tags$div(
      class = "sidebar-logo", 
      tags$img(src = "logo_saya.png", width = "80px"),  # Perhatikan path nya berubah
      tags$h4("NutriTrack", class = "logo-text")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Kalkulator Stunting", tabName = "calculator", icon = icon("calculator")),
      menuItem("Rekomendasi Gizi", tabName = "nutrition", icon = icon("bowl-food")),
      menuItem("Data Stunting Indonesia", tabName = "data", icon = icon("map")),
      menuItem("Tentang Aplikasi", tabName = "about", icon = icon("circle-info")),
      
      # Only show this panel in Calculator tab
      conditionalPanel(
        condition = "input.tabs == 'calculator'",
        hr(),
        div(class = "sidebar-header", "Data Anak"),
        textInput("name", "Nama Anak", placeholder = "Masukkan nama anak"),
        textInput("age_input", "Usia (bulan)", placeholder = "Contoh: 12 atau usia anak 12"),
        awesomeRadio("sex", "Jenis Kelamin", 
                     choices = c("Laki-laki" = "Male", "Perempuan" = "Female"),
                     selected = "Male", status = "primary"),
        textInput("height_input", "Tinggi Badan (cm)", placeholder = "Contoh: 60 atau tb anak 60"),
        actionBttn("calculate", "Analisis Status Gizi", 
                   style = "gradient", color = "primary", size = "md",
                   block = TRUE, icon = icon("magnifying-glass-chart"))
      )
    )
  ),
  
  dashboardBody(
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Nunito:wght@300;400;600;700&display=swap"),
      tags$link(rel = "stylesheet", href = "custom.css"),
    ),
    
    tabItems(
      # Tab Kalkulator
      tabItem(tabName = "calculator",
              fluidRow(
                column(width = 12,
                       h2("Kalkulator Status Gizi Anak", class = "section-title"),
                       p("Masukkan data anak di panel samping, lalu klik tombol 'Analisis Status Gizi' untuk mengetahui status pertumbuhan anak berdasarkan standar WHO.")
                )
              ),
              fluidRow(
                box(
                  title = span(icon("clipboard-check"), "Analisis Status Gizi Anak"), 
                  status = "primary", solidHeader = TRUE,
                  width = 8,
                  div(
                    class = "results-header",
                    h3(textOutput("child_name"))
                  ),
                  div(
                    class = "results-content",
                    fluidRow(
                      column(6,
                             h4("Data Pengukuran"),
                             tableOutput("results_table") %>% 
                               withSpinner(color = "#3c8dbc", type = 8)
                      ),
                      column(6,
                             h4("Hasil Analisis & Rekomendasi"),
                             uiOutput("recommendation")
                      )
                    )
                  )
                ),
                box(
                  title = span(icon("info-circle"), "Panduan Pengukuran"), 
                  status = "info", solidHeader = TRUE,
                  width = 4,
                  h4(icon("ruler-vertical"), " Cara Mengukur Tinggi:"),
                  tags$ul(
                    tags$li("Pastikan anak berdiri tegak dengan kepala, punggung, dan tumit menempel dinding"),
                    tags$li("Ukur dari ujung kaki hingga puncak kepala"),
                    tags$li("Untuk bayi < 2 tahun, ukur dalam posisi berbaring")
                  ),
                  hr(),
                  h4(icon("key"), " Kategori Hasil:"),
                  tags$ul(
                    tags$li(tags$span(class = "label label-success", "Normal"), " : Z-Score ≥ -2"),
                    tags$li(tags$span(class = "label label-warning", "Stunting"), " : -3 ≤ Z-Score < -2"),
                    tags$li(tags$span(class = "label label-danger", "Stunting Berat"), " : Z-Score < -3")
                  )
                )
              ),
              fluidRow(
                box(
                  title = span(icon("chart-line"), "Kurva Pertumbuhan Tinggi Badan menurut Umur (TB/U)"), 
                  status = "success", solidHeader = TRUE,
                  width = 12,
                  div(class = "chart-container",
                      plotlyOutput("growth_plot") %>% 
                        withSpinner(color = "#3c8dbc", type = 8)
                  ),
                  tags$div(
                    class = "help-block",
                    "Kurva di atas menunjukkan perbandingan tinggi anak dengan standar WHO. Titik merah menunjukkan posisi anak pada kurva pertumbuhan."
                  )
                )
              )
      ),
      
      # Tab Rekomendasi Gizi
      tabItem(tabName = "nutrition",
              fluidRow(
                column(width = 12,
                       h2("Rekomendasi Gizi untuk Pencegahan Stunting", class = "section-title"),
                       p("Panduan gizi seimbang berdasarkan kelompok usia untuk mendukung pertumbuhan optimal dan pencegahan stunting pada anak.")
                )
              ),
              fluidRow(
                box(
                  title = span(icon("utensils"), "Panduan Gizi Berdasarkan Usia"), 
                  status = "primary", solidHeader = TRUE,
                  width = 12,
                  tabBox(
                    width = 12,
                    id = "nutritionTabs",
                    tabPanel(
                      title = span(icon("baby"), "0-6 Bulan"),
                      div(class = "nutrition-content",
                          div(class = "row",
                              div(class = "col-md-7",
                                  h4(icon("heart"), " Rekomendasi Utama:", class = "text-primary"),
                                  tags$ul(class = "recommendation-list",
                                          tags$li(icon("check"), strong(" ASI Eksklusif"), " - Berikan hanya ASI tanpa tambahan makanan atau minuman apapun selama 6 bulan pertama kehidupan"),
                                          tags$li(icon("check"), " Pastikan ibu mendapatkan nutrisi yang cukup dan seimbang selama menyusui untuk menjaga kualitas ASI"),
                                          tags$li(icon("check"), " Lakukan IMD (Inisiasi Menyusu Dini) dalam satu jam pertama setelah kelahiran"),
                                          tags$li(icon("check"), " Berikan ASI minimal 8-12 kali dalam 24 jam atau sesuai kebutuhan bayi")
                                  ),
                                  hr(),
                                  h4(icon("circle-info"), " Mengapa ASI Eksklusif Penting:"),
                                  tags$ul(class = "recommendation-list",
                                          tags$li("Mengandung semua nutrisi yang dibutuhkan bayi untuk 6 bulan pertama"),
                                          tags$li("Berisi antibodi yang memperkuat sistem kekebalan tubuh bayi"),
                                          tags$li("Meningkatkan ikatan emosional ibu dan bayi"),
                                          tags$li("Mengurangi risiko infeksi saluran pencernaan dan pernapasan")
                                  )
                              ),
                              div(class = "col-md-5",
                                  h4(icon("shield-heart"), " Nutrisi Penting untuk Ibu Menyusui:"),
                                  div(class = "row",
                                      lapply(list(
                                        list("Protein", "egg", "Telur, ikan, ayam, daging, tempe, tahu, kacang-kacangan"),
                                        list("Kalsium", "bone", "Susu, keju, yogurt, ikan teri, sayuran hijau"),
                                        list("Zat Besi", "leaf", "Daging merah, bayam, kacang-kacangan, sayuran hijau"),
                                        list("Cairan", "glass-water", "Minimal 8-10 gelas air putih per hari")
                                      ), function(x) {
                                        div(class = "col-md-6",
                                            div(class = "food-card",
                                                icon(x[[2]], class = "fa-2x food-icon"),
                                                h5(x[[1]]),
                                                p(x[[3]])
                                            )
                                        )
                                      })
                                  )
                              )
                          )
                      )
                    ),
                    tabPanel(
                      title = span(icon("baby-carriage"), "6-12 Bulan"),
                      div(class = "nutrition-content",
                          div(class = "row",
                              div(class = "col-md-7",
                                  h4(icon("heart"), " Rekomendasi Utama:", class = "text-primary"),
                                  tags$ul(class = "recommendation-list",
                                          tags$li(icon("check"), strong(" ASI + MPASI"), " - Lanjutkan pemberian ASI dengan ditambah makanan pendamping yang sesuai"),
                                          tags$li(icon("check"), " Mulai MPASI pada usia 6 bulan dengan makanan bertekstur lumat (puree)"),
                                          tags$li(icon("check"), " Secara bertahap tingkatkan tekstur makanan sesuai perkembangan (dari lumat, lembik, hingga padat)"),
                                          tags$li(icon("check"), " Berikan makanan kaya zat besi seperti daging merah, hati, dan kuning telur"),
                                          tags$li(icon("check"), " Perkaya MPASI dengan protein hewani, buah, sayur, dan makanan pokok")
                                  ),
                                  hr(),
                                  h4(icon("clock"), " Frekuensi Pemberian Makan:"),
                                  tags$ul(class = "recommendation-list",
                                          tags$li("6-8 bulan: 2-3 kali makan utama + 1-2 kali camilan + ASI"),
                                          tags$li("9-12 bulan: 3-4 kali makan utama + 1-2 kali camilan + ASI"),
                                          tags$li("Tingkatkan porsi makan secara bertahap sesuai usia")
                                  )
                              ),
                              div(class = "col-md-5",
                                  h4(icon("utensils"), " Contoh Menu MPASI:"),
                                  div(class = "row",
                                      lapply(list(
                                        list("Bubur susu + ikan", "fish", "Beras/tepung beras + ASI/susu formula + ikan hancur + wortel"),
                                        list("Pure buah", "apple-whole", "Pisang, alpukat, atau pepaya yang dihaluskan"),
                                        list("Bubur ayam", "drumstick-bite", "Beras + ayam cincang + bayam + wortel + kaldu ayam"),
                                        list("Bubur kacang hijau", "seedling", "Kacang hijau + gula aren sedikit + santan")
                                      ), function(x) {
                                        div(class = "col-md-6",
                                            div(class = "food-card",
                                                icon(x[[2]], class = "fa-2x food-icon"),
                                                h5(x[[1]]),
                                                p(x[[3]])
                                            )
                                        )
                                      })
                                  )
                              )
                          )
                      )
                    ),
                    tabPanel(
                      title = span(icon("child"), "1-3 Tahun"),
                      div(class = "nutrition-content",
                          div(class = "row",
                              div(class = "col-md-7",
                                  h4(icon("heart"), " Rekomendasi Utama:", class = "text-primary"),
                                  tags$ul(class = "recommendation-list",
                                          tags$li(icon("check"), " Berikan makanan keluarga yang bervariasi dengan tekstur yang sesuai"),
                                          tags$li(icon("check"), " Sediakan protein hewani setiap hari (telur, ikan, ayam, daging)"),
                                          tags$li(icon("check"), " Sajikan sayuran berwarna-warni dan buah segar setiap hari"),
                                          tags$li(icon("check"), " Berikan karbohidrat kompleks (beras merah, ubi, jagung)"),
                                          tags$li(icon("check"), " Pastikan asupan zat besi, seng, kalsium, dan vitamin A tercukupi"),
                                          tags$li(icon("check"), " Batasi konsumsi makanan manis, asin, dan berlemak")
                                  ),
                                  hr(),
                                  h4(icon("hand-holding-hand"), " Prinsip Responsive Feeding:"),
                                  tags$ul(class = "recommendation-list",
                                          tags$li("Perhatikan tanda lapar dan kenyang pada anak"),
                                          tags$li("Berikan makanan dengan sabar dan penuh perhatian"),
                                          tags$li("Ajak anak berbicara selama waktu makan"),
                                          tags$li("Jadikan waktu makan sebagai momen yang menyenangkan")
                                  )
                              ),
                              div(class = "col-md-5",
                                  h4(icon("bowl-food"), " Contoh Menu Seimbang:"),
                                  div(class = "row",
                                      lapply(list(
                                        list("Nasi + ikan + sayur", "plate-wheat", "Nasi putih/merah + ikan/daging cincang + sayur berwarna + buah"),
                                        list("Bubur ayam lengkap", "bowl-rice", "Bubur + ayam suwir + telur + bayam + wortel + bawang goreng"),
                                        list("Sup ikan + sayuran", "fish", "Ikan fillet + brokoli + wortel + kentang + nasi"),
                                        list("Nasi tim daging", "bowl-food", "Nasi lembut + daging giling + tomat + wortel + buncis")
                                      ), function(x) {
                                        div(class = "col-md-6",
                                            div(class = "food-card",
                                                icon(x[[2]], class = "fa-2x food-icon"),
                                                h5(x[[1]]),
                                                p(x[[3]])
                                            )
                                        )
                                      })
                                  )
                              )
                          )
                      )
                    ),
                    tabPanel(
                      title = span(icon("children"), "3-5 Tahun"),
                      div(class = "nutrition-content",
                          div(class = "row",
                              div(class = "col-md-7",
                                  h4(icon("heart"), " Rekomendasi Utama:", class = "text-primary"),
                                  tags$ul(class = "recommendation-list",
                                          tags$li(icon("check"), " Terapkan pola makan teratur 3x sehari + 2x camilan sehat"),
                                          tags$li(icon("check"), " Berikan protein berkualitas tinggi (ikan, telur, daging, susu)"),
                                          tags$li(icon("check"), " Sediakan buah dan sayur dari berbagai warna setiap hari"),
                                          tags$li(icon("check"), " Batasi makanan cepat saji, junk food, dan minuman manis"),
                                          tags$li(icon("check"), " Libatkan anak dalam pemilihan dan persiapan makanan"),
                                          tags$li(icon("check"), " Jadilah contoh yang baik dengan menerapkan pola makan sehat")
                                  ),
                                  hr(),
                                  h4(icon("child-reaching"), " Tips Mengatasi Anak Susah Makan:"),
                                  tags$ul(class = "recommendation-list",
                                          tags$li("Buat makanan terlihat menarik dengan bentuk dan warna"),
                                          tags$li("Berikan pilihan makanan sehat yang bervariasi"),
                                          tags$li("Ajak anak makan bersama keluarga"),
                                          tags$li("Hindari menggunakan makanan sebagai hadiah atau hukuman")
                                  )
                              ),
                              div(class = "col-md-5",
                                  h4(icon("calendar-day"), " Contoh Menu Sehari:"),
                                  div(class = "row",
                                      lapply(list(
                                        list("Sarapan", "sun", "Nasi + telur dadar/orak-arik + sayur + buah + susu"),
                                        list("Makan Siang", "clock", "Nasi + ikan/ayam + tahu/tempe + sayur bersantan + buah"),
                                        list("Makan Malam", "moon", "Nasi + sup daging + sayuran + buah"),
                                        list("Camilan Sehat", "carrot", "Buah segar, susu, pudding buah, roti gandum, kacang-kacangan")
                                      ), function(x) {
                                        div(class = "col-md-6",
                                            div(class = "food-card",
                                                icon(x[[2]], class = "fa-2x food-icon"),
                                                h5(x[[1]]),
                                                p(x[[3]])
                                            )
                                        )
                                      })
                                  )
                              )
                          )
                      )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = span(icon("lightbulb"), "Tips Pemberian Makan untuk Mencegah Stunting"), 
                  status = "info", solidHeader = TRUE,
                  width = 6,
                  div(class = "info-box-content",
                      h4(icon("hand-holding-heart"), " Prinsip Responsive Feeding:"),
                      tags$ul(class = "recommendation-list enhanced",
                              tags$li(icon("smile"), " Berikan makan dengan penuh perhatian, kesabaran, dan kehangatan"),
                              tags$li(icon("comments"), " Ajak anak berbicara dan bercerita saat waktu makan"),
                              tags$li(icon("face-laugh-beam"), " Ciptakan suasana makan yang menyenangkan dan positif"),
                              tags$li(icon("ban"), " Hindari memaksa anak untuk menghabiskan makanan"),
                              tags$li(icon("stopwatch"), " Berikan waktu yang cukup untuk anak makan tanpa terburu-buru")
                      ),
                      hr(),
                      h4(icon("shield"), " Faktor Pendukung Pertumbuhan Optimal:"),
                      tags$ul(class = "recommendation-list enhanced",
                              tags$li(icon("pills"), " Suplemen vitamin A dan zat besi sesuai rekomendasi tenaga kesehatan"),
                              tags$li(icon("broom"), " Jaga kebersihan makanan, peralatan makan, dan kebersihan diri"),
                              tags$li(icon("person-running"), " Dorong anak untuk aktif bergerak dan bermain di luar ruangan"),
                              tags$li(icon("moon"), " Pastikan anak mendapatkan waktu tidur yang cukup dan berkualitas"),
                              tags$li(icon("hospital"), " Rutin memeriksakan kesehatan dan pertumbuhan anak ke Posyandu/Puskesmas")
                      )
                  )
                ),
                
                box(
                  title = span(icon("apple-whole"), "Makanan Pencegah Stunting"), 
                  status = "success", solidHeader = TRUE,
                  width = 6,
                  div(class = "info-box-content",
                      h4(icon("star"), " Sumber Protein Berkualitas:"),
                      fluidRow(
                        lapply(list(
                          list("Telur", "egg", "#FFD700", "Kaya protein & nutrisi otak"),
                          list("Ikan", "fish", "#1E90FF", "Sumber omega-3 & protein"),
                          list("Tempe", "seedling", "#8B4513", "Protein nabati berkualitas")
                        ), function(x) {
                          column(4,
                                 div(class = "food-card enhanced",
                                     icon(x[[2]], class = "fa-2x", style = paste0("color: ", x[[3]], ";")),
                                     h5(x[[1]]),
                                     p(x[[4]])
                                 )
                          )
                        })
                      ),
                      hr(),
                      h4(icon("plus"), " Suplemen Nutrisi Penting:"),
                      fluidRow(
                        lapply(list(
                          list("Vitamin A", "carrot", "#FFA500", "Pendukung penglihatan & imunitas"),
                          list("Zat Besi", "drumstick-bite", "#8B0000", "Mencegah anemia & dukung pertumbuhan"),
                          list("Zinc", "leaf", "#32CD32", "Mendukung pertumbuhan & imunitas")
                        ), function(x) {
                          column(4,
                                 div(class = "food-card enhanced",
                                     icon(x[[2]], class = "fa-2x", style = paste0("color: ", x[[3]], ";")),
                                     h5(x[[1]]),
                                     p(x[[4]])
                                 )
                          )
                        })
                      )
                  )
                )
              )
      ),
      
      # Tab Data Stunting Indonesia
      # Tab Data Stunting Indonesia
      tabItem(tabName = "data",
              fluidRow(
                column(width = 12,
                       h2("Data Prevalensi Stunting di Indonesia", class = "section-title"),
                       p("Visualisasi dan analisis data prevalensi stunting di berbagai provinsi di Indonesia berdasarkan data terbaru tahun 2024.")
                )
              ),
              
              # Summary Statistics Cards
              fluidRow(
                column(width = 3,
                       div(class = "stat-box",
                           icon("child-reaching", class = "stat-icon text-primary"),
                           h3("6,1%"),
                           p("Rata-rata prevalensi stunting nasional")
                       )
                ),
                column(width = 3,
                       div(class = "stat-box",
                           icon("chart-line", class = "stat-icon text-success"),
                           h3("14.0%"),
                           p("Target penurunan stunting 2024")
                       )
                ),
                column(width = 3,
                       div(class = "stat-box",
                           icon("map-location-dot", class = "stat-icon text-warning"),
                           h3("SULAWESI BARAT"),
                           p("Provinsi dengan prevalensi tertinggi (23.9%)")
                       )
                ),
                column(width = 3,
                       div(class = "stat-box",
                           icon("children", class = "stat-icon text-info"),
                           h3("15,904,224"),
                           p("Total balita di Indonesia")
                       )
                )
              ),
              
              # Interactive Data Visualization
              fluidRow(
                # Bar chart visualization
                box(
                  title = span(icon("chart-column"), "Prevalensi Stunting per Provinsi"), 
                  status = "primary", solidHeader = TRUE,
                  width = 8,
                  div(class = "chart-container",
                      plotlyOutput("prov_stunting_chart") %>% 
                        withSpinner(color = "#3c8dbc", type = 8)
                  ),
                  div(
                    class = "chart-controls",
                    fluidRow(
                      column(width = 6,
                             selectInput("chart_type", "Tampilkan Berdasarkan:",
                                         choices = c("Persentase Stunting" = "percentage", 
                                                     "Jumlah Kasus Stunting" = "cases",
                                                     "Jumlah Balita" = "toddlers"),
                                         selected = "percentage")
                      ),
                      column(width = 6,
                             checkboxInput("sort_data", "Urutkan dari Tertinggi", value = TRUE)
                      )
                    )
                  ),
                  tags$div(
                    class = "help-block",
                    "Data prevalensi stunting berdasarkan sumber Kementerian Kesehatan RI. Klik pada batang diagram untuk melihat detail."
                  )
                ),
                
                # Top provinces with highest prevalence
                box(
                  title = span(icon("ranking-star"), "Provinsi dengan Prevalensi Tertinggi"), 
                  status = "warning", solidHeader = TRUE,
                  width = 4,
                  div(class = "ranking-container",
                      uiOutput("top_stunting_provinces")
                  )
                )
              ),
              
              # Data Table with Search
              fluidRow(
                box(
                  title = span(icon("table"), "Tabel Data Stunting Indonesia"), 
                  status = "info", solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(width = 6,
                           textInput("table_search", "Cari Provinsi:", placeholder = "Ketik nama provinsi...")
                    ),
                    column(width = 6, align = "right", style = "margin-top: 25px;",
                           downloadButton("download_data", "Unduh Data", class = "btn-sm btn-primary")
                    )
                  ),
                  hr(),
                  div(style = "overflow-x: auto;",
                      DT::dataTableOutput("stunting_table") %>% 
                        withSpinner(color = "#3c8dbc", type = 8)
                  )
                )
              ),
              
              # Analysis and Recommendations
              fluidRow(
                box(
                  title = span(icon("magnifying-glass-chart"), "Analisis dan Rekomendasi"), 
                  status = "info", solidHeader = TRUE,
                  width = 12,
                  div(class = "analysis-box",
                      fluidRow(
                        column(width = 6,
                               h4(icon("eye"), " Temuan Utama:"),
                               tags$ul(class = "recommendation-list enhanced",
                                       tags$li("Terdapat disparitas yang signifikan antar provinsi dengan rentang prevalensi stunting 10-25%"),
                                       tags$li("Provinsi NTT, Aceh, Papua, dan NTB memiliki angka stunting yang lebih tinggi (≥20%)"),
                                       tags$li("Provinsi dengan jumlah balita tinggi seperti Jawa Barat dan Jawa Timur memerlukan perhatian khusus untuk pengurangan stunting"),
                                       tags$li("DKI Jakarta, DI Yogyakarta, dan Bali menunjukkan angka prevalensi yang lebih rendah (10%)")
                               )
                        ),
                        column(width = 6,
                               h4(icon("lightbulb"), " Rekomendasi Kebijakan:"),
                               tags$ul(class = "recommendation-list enhanced",
                                       tags$li("Prioritaskan intervensi gizi spesifik dan sensitif di provinsi dengan prevalensi >20%"),
                                       tags$li("Penguatan program 1000 Hari Pertama Kehidupan di seluruh Indonesia"),
                                       tags$li("Peningkatan koordinasi lintas sektor untuk mengatasi stunting secara komprehensif"),
                                       tags$li("Edukasi masyarakat tentang pentingnya gizi seimbang dan pola asuh yang responsif"),
                                       tags$li("Monitoring dan evaluasi berkala program intervensi stunting di setiap provinsi")
                               )
                        )
                      )
                  )
                )
              ),
              
              # Comparison over time (placeholder for future development)
              fluidRow(
                box(
                  title = span(icon("chart-line"), "Tren Prevalensi Stunting (2018-2022)"), 
                  status = "success", solidHeader = TRUE,
                  width = 12,
                  div(class = "chart-container",
                      plotlyOutput("stunting_trend") %>% 
                        withSpinner(color = "#3c8dbc", type = 8)
                  ),
                  tags$div(
                    class = "help-block text-center",
                    "Tren prevalensi stunting nasional dari tahun 2018 hingga 2022, menunjukkan penurunan dari 30.8% menjadi 16.5%."
                  )
                )
              )
      ),
      
      # Tab Tentang Aplikasi
      tabItem(tabName = "about",
              fluidRow(
                column(width = 12,
                       h2("Tentang NutriTrack", class = "section-title"),
                       p("Aplikasi pemantauan pertumbuhan anak dan pencegahan stunting berbasis data.")
                )
              ),
              fluidRow(
                box(
                  title = span(icon("circle-info"), "Apa itu NutriTrack?"), 
                  status = "primary", solidHeader = TRUE,
                  width = 6,
                  div(class = "about-box",
                      h4("Tujuan Aplikasi:"),
                      p("NutriTrack adalah aplikasi berbasis dashboard yang dirancang untuk membantu orang tua, kader kesehatan, dan tenaga medis dalam memantau pertumbuhan anak dan mencegah stunting di Indonesia."),
                      tags$br(),
                      h4("Fitur Utama:"),
                      tags$ul(class = "about-list",
                              tags$li(icon("calculator"), " Kalkulator Status Gizi - Analisis pertumbuhan anak berdasarkan standar WHO"),
                              tags$li(icon("bowl-food"), " Rekomendasi Gizi - Panduan pemberian makan sesuai usia anak"),
                              tags$li(icon("map"), " Data Stunting - Visualisasi data prevalensi stunting di Indonesia"),
                              tags$li(icon("mobile-screen"), " Mudah Digunakan - Antarmuka yang sederhana dan responsif")
                      )
                  )
                ),
                box(
                  title = span(icon("question-circle"), "Apa itu Stunting?"), 
                  status = "warning", solidHeader = TRUE,
                  width = 6,
                  div(class = "about-box",
                      h4("Definisi:"),
                      p("Stunting adalah kondisi gagal tumbuh pada anak balita akibat kekurangan gizi kronis sehingga anak memiliki panjang atau tinggi badan yang lebih pendek dari standar usianya."),
                      tags$br(),
                      h4("Dampak Stunting:"),
                      tags$ul(class = "about-list",
                              tags$li(icon("brain"), " Penurunan perkembangan kognitif dan kecerdasan"),
                              tags$li(icon("shield-virus"), " Penurunan sistem kekebalan tubuh"),
                              tags$li(icon("graduation-cap"), " Penurunan kapasitas belajar dan prestasi sekolah"),
                              tags$li(icon("hand-holding-dollar"), " Penurunan produktivitas dan kemampuan ekonomi di masa dewasa")
                      )
                  )
                )
              ),
              fluidRow(
                box(
                  title = span(icon("code"), "Informasi Teknis"), 
                  status = "info", solidHeader = TRUE,
                  width = 6,
                  div(class = "about-box",
                      h4("Teknologi yang Digunakan:"),
                      tags$ul(class = "about-list tech",
                              tags$li(icon("r-project"), " R & Shiny - Framework aplikasi web interaktif"),
                              tags$li(icon("chart-line"), " ggplot2 & plotly - Visualisasi data interaktif"),
                              tags$li(icon("table"), " dplyr - Manipulasi dan analisis data"),
                              tags$li(icon("palette"), " shinydashboard - Komponen UI dashboard")
                      ),
                      tags$br(),
                      h4("Sumber Data:"),
                      p("Data pertumbuhan berdasarkan standar WHO Child Growth Standards. Data prevalensi stunting di Indonesia berdasarkan data Kementerian Kesehatan RI tahun 2022.")
                  )
                ),
                box(
                  title = span(icon("users"), "Tim Pengembang"), 
                  status = "success", solidHeader = TRUE,
                  width = 6,
                  div(class = "about-box",
                      div(class = "team-info",
                          div(class = "team-photo", icon("user-doctor", class = "fa-4x")),
                          div(class = "team-details",
                              h4("Tim Gizi & Kesehatan Masyarakat"),
                              p("Pengembangan konten gizi dan rekomendasi kesehatan")
                          )
                      ),
                      hr(),
                      div(class = "team-info",
                          div(class = "team-photo", icon("laptop-code", class = "fa-4x")),
                          div(class = "team-details",
                              h4("Tim Pengembang Aplikasi"),
                              p("Desain, pengembangan, dan analisis data")
                          )
                      )
                  )
                )
              )
      )
    )
  )
)

# ------------------------------------------
# SERVER DEFINITION
# ------------------------------------------

server <- function(input, output, session) {
  
  # Reactive values for results
  results <- reactiveValues(
    zscore = NULL,
    status = NULL,
    color = NULL,
    status_text = NULL
  )
  
  # Parse numeric inputs
  age <- reactive({
    if (is.null(input$age_input) || input$age_input == "") return(NULL)
    # Extract numeric value, supporting formats like "12" or "usia anak 12"
    age_str <- input$age_input
    age_num <- as.numeric(gsub("[^0-9.]", "", age_str))
    return(age_num)
  })
  
  height <- reactive({
    if (is.null(input$height_input) || input$height_input == "") return(NULL)
    # Extract numeric value, supporting formats like "60" or "tb anak 60"
    height_str <- input$height_input
    height_num <- as.numeric(gsub("[^0-9.]", "", height_str))
    return(height_num)
  })
  
  # Calculate Z-Score and status when user clicks "Analisis" button
  observeEvent(input$calculate, {
    req(age(), height())
    
    # Get WHO standard for child's age and sex
    age_idx <- which(abs(who_data$age_months - age()) == min(abs(who_data$age_months - age())))
    sex_idx <- which(who_data$sex == input$sex)
    
    # Get closest age in WHO data
    closest_age_idx <- intersect(age_idx, sex_idx)
    
    if(length(closest_age_idx) > 0) {
      closest_age_idx <- closest_age_idx[1]
      
      # Get median and SD for the age and sex
      median_height <- who_data$median_tb[closest_age_idx]
      sd_height <- who_data$sd_tb[closest_age_idx]
      
      # Calculate Z-Score
      z_score <- (height() - median_height) / sd_height
      results$zscore <- round(z_score, 2)
      
      # Determine status
      if(z_score >= -2) {
        results$status <- "Normal"
        results$color <- "#28a745"  # green
        results$status_text <- "Tinggi badan anak sesuai dengan standar WHO untuk usia ini."
      } else if(z_score >= -3 && z_score < -2) {
        results$status <- "Stunting"
        results$color <- "#ffc107"  # yellow/warning
        results$status_text <- "Tinggi badan anak sedikit di bawah standar WHO untuk usia ini. Konsultasikan dengan tenaga kesehatan."
      } else {
        results$status <- "Stunting Berat"
        results$color <- "#dc3545"  # red/danger
        results$status_text <- "Tinggi badan anak signifikan di bawah standar WHO untuk usia ini. Segera konsultasikan dengan tenaga kesehatan."
      }
    }
  })
  
  # Output child's name
  output$child_name <- renderText({
    if(input$name == "") {
      return("Status Gizi Anak")
    } else {
      return(paste("Status Gizi:", input$name))
    }
  })
  
  # Results table
  output$results_table <- renderTable({
    req(results$zscore, age(), height())
    
    closest_age_idx <- which(who_data$age_months == who_data$age_months[which.min(abs(who_data$age_months - age()))] & 
                               who_data$sex == input$sex)[1]
    
    data.frame(
      "Parameter" = c("Usia", "Jenis Kelamin", "Tinggi Badan", "Tinggi Badan Standar", "Z-Score", "Status"),
      "Nilai" = c(
        paste(age(), "bulan"),
        ifelse(input$sex == "Male", "Laki-laki", "Perempuan"),
        paste(height(), "cm"),
        paste(who_data$median_tb[closest_age_idx], "cm"),
        as.character(results$zscore),
        results$status
      )
    )
  })
  
  # Recommendation based on status
  output$recommendation <- renderUI({
    req(results$zscore)
    
    tagList(
      div(
        class = "status-badge",
        style = paste0("background-color: ", results$color, ";"),
        results$status
      ),
      p(results$status_text),
      hr(),
      h4("Rekomendasi:"),
      if(results$status == "Normal") {
        tags$ul(
          tags$li("Pertahankan pola makan seimbang dengan makanan bergizi"),
          tags$li("Lanjutkan pemantauan pertumbuhan secara rutin"),
          tags$li("Pastikan anak mendapatkan stimulasi yang adekuat")
        )
      } else if(results$status == "Stunting") {
        tags$ul(
          tags$li(strong("Konsultasikan dengan tenaga kesehatan")),
          tags$li("Tingkatkan asupan protein berkualitas (ikan, telur, susu)"),
          tags$li("Berikan suplemen zat gizi mikro sesuai anjuran"),
          tags$li("Pantau pertumbuhan secara lebih sering (1-2 minggu sekali)")
        )
      } else {
        tags$ul(
          tags$li(strong("Segera bawa anak ke fasilitas kesehatan terdekat")),
          tags$li("Ikuti program pemulihan gizi yang direkomendasikan"),
          tags$li("Tingkatkan asupan kalori, protein, dan zat gizi mikro"),
          tags$li("Pantau berat dan tinggi badan mingguan"),
          tags$li("Ikuti program Pemberian Makanan Tambahan (PMT)")
        )
      }
    )
  })
  
  # Growth chart
  output$growth_plot <- renderPlotly({
    # Create data for growth chart lines
    age_range <- seq(0, 60, by = 1)
    
    # Filter WHO data for selected sex
    who_filtered <- who_data[who_data$sex == input$sex, ]
    
    # Median line
    median_line <- approx(who_filtered$age_months, who_filtered$median_tb, xout = age_range)
    
    # -2 SD line (Stunting threshold)
    minus2sd_line <- approx(who_filtered$age_months, who_filtered$median_tb - 2*who_filtered$sd_tb, xout = age_range)
    
    # -3 SD line (Severe Stunting threshold)
    minus3sd_line <- approx(who_filtered$age_months, who_filtered$median_tb - 3*who_filtered$sd_tb, xout = age_range)
    
    # Create plot with plotly
    plot_data <- plot_ly(showlegend = TRUE)
    
    # Add lines
    plot_data <- plot_data %>%
      add_trace(x = age_range, y = median_line$y, type = 'scatter', mode = 'lines',
                line = list(color = 'green', width = 2, dash = 'solid'),
                name = 'Median WHO')
    
    plot_data <- plot_data %>%
      add_trace(x = age_range, y = minus2sd_line$y, type = 'scatter', mode = 'lines',
                line = list(color = 'orange', width = 2, dash = 'dash'),
                name = '-2 SD (Stunting)')
    
    plot_data <- plot_data %>%
      add_trace(x = age_range, y = minus3sd_line$y, type = 'scatter', mode = 'lines',
                line = list(color = 'red', width = 2, dash = 'dash'),
                name = '-3 SD (Stunting Berat)')
    
    # Add point for current child if data is available
    if(!is.null(results$zscore)) {
      plot_data <- plot_data %>%
        add_trace(x = age(), y = height(), type = 'scatter', mode = 'markers',
                  marker = list(color = results$color, size = 12, symbol = 'circle'),
                  name = ifelse(input$name == "", "Anak", input$name))
    }
    
    # Layout
    plot_data <- plot_data %>%
      layout(
        title = "Kurva Tinggi Badan menurut Umur",
        xaxis = list(
          title = "Usia (bulan)",
          range = c(0, 60)
        ),
        yaxis = list(
          title = "Tinggi Badan (cm)",
          range = c(40, 120)
        ),
        shapes = list(
          # Add area between -2SD and -3SD (Stunting)
          list(
            type = "rect", 
            fillcolor = "rgba(255, 193, 7, 0.2)", 
            line = list(color = "transparent"), 
            x0 = 0, x1 = 60, 
            xref = "x", 
            y0 = minus3sd_line$y[1], y1 = minus2sd_line$y[1], 
            yref = "y"
          ),
          # Add area below -3SD (Severe Stunting)
          list(
            type = "rect", 
            fillcolor = "rgba(220, 53, 69, 0.2)", 
            line = list(color = "transparent"), 
            x0 = 0, x1 = 60, 
            xref = "x", 
            y0 = 40, y1 = minus3sd_line$y[1], 
            yref = "y"
          )
        )
      )
    
    return(plot_data)
  })
  
  # Province chart
  output$prov_chart <- renderPlotly({
    # Sort data by prevalence
    sorted_data <- stunting_data_indonesia[order(-stunting_data_indonesia$prevalensi), ]
    
    # Create color scale based on prevalence
    color_scale <- ifelse(sorted_data$prevalensi >= 30, 'red',
                          ifelse(sorted_data$prevalensi >= 25, 'orange', 'yellow'))
    
    # Create plot with plotly
    plot_data <- plot_ly(data = sorted_data, 
                         x = ~provinsi, 
                         y = ~prevalensi,
                         type = 'bar',
                         marker = list(color = color_scale),
                         text = ~paste0(prevalensi, "%"),
                         hoverinfo = 'text')
    
    # Layout
    plot_data <- plot_data %>%
      layout(
        title = "Prevalensi Stunting per Provinsi (2022)",
        xaxis = list(
          title = "Provinsi",
          categoryorder = "total descending"
        ),
        yaxis = list(
          title = "Prevalensi (%)",
          range = c(0, 40)
        ),
        shapes = list(
          # Add line for target 2024
          list(
            type = "line", 
            line = list(color = "green", width = 2, dash = "dash"), 
            x0 = -0.5, x1 = length(sorted_data$provinsi) - 0.5, 
            xref = "x", 
            y0 = 14, y1 = 14, 
            yref = "y"
          )
        ),
        annotations = list(
          list(
            x = length(sorted_data$provinsi) / 2,
            y = 14,
            xref = "x",
            yref = "y",
            text = "Target 2024: 14%",
            showarrow = FALSE,
            font = list(color = "green"),
            bgcolor = "white",
            bordercolor = "green",
            borderwidth = 1
          )
        )
      )
    
    return(plot_data)
  })
  
  # Provincial stunting chart with different views (percentage, cases, toddlers)
  output$prov_stunting_chart <- renderPlotly({
    # Determine what data to display based on input
    if(input$chart_type == "percentage") {
      y_data <- stunting_data_indonesia$prevalensi
      y_title <- "Prevalensi Stunting (%)"
      hover_text <- paste0(
        "<b>", stunting_data_indonesia$provinsi, "</b><br>",
        "Prevalensi: ", stunting_data_indonesia$prevalensi, "%<br>",
        "Jumlah Kasus: ", format(stunting_data_indonesia$jml_stunting, big.mark="."), "<br>",
        "Jumlah Balita: ", format(stunting_data_indonesia$jml_balita, big.mark=".")
      )
    } else if(input$chart_type == "cases") {
      y_data <- stunting_data_indonesia$jml_stunting
      y_title <- "Jumlah Kasus Stunting"
      hover_text <- paste0(
        "<b>", stunting_data_indonesia$provinsi, "</b><br>",
        "Jumlah Kasus: ", format(stunting_data_indonesia$jml_stunting, big.mark="."), "<br>",
        "Prevalensi: ", stunting_data_indonesia$prevalensi, "%<br>",
        "Jumlah Balita: ", format(stunting_data_indonesia$jml_balita, big.mark=".")
      )
    } else {
      y_data <- stunting_data_indonesia$jml_balita
      y_title <- "Jumlah Balita"
      hover_text <- paste0(
        "<b>", stunting_data_indonesia$provinsi, "</b><br>",
        "Jumlah Balita: ", format(stunting_data_indonesia$jml_balita, big.mark="."), "<br>",
        "Jumlah Kasus: ", format(stunting_data_indonesia$jml_stunting, big.mark="."), "<br>",
        "Prevalensi: ", stunting_data_indonesia$prevalensi, "%"
      )
    }
    
    # Sort data if requested
    if(input$sort_data) {
      # Sort the data
      sort_idx <- order(y_data, decreasing = TRUE)
      
      # Apply sorting
      sorted_provinsi <- stunting_data_indonesia$provinsi[sort_idx]
      sorted_y_data <- y_data[sort_idx]
      sorted_hover <- hover_text[sort_idx]
      
      # Create color scale based on prevalence
      if(input$chart_type == "percentage") {
        color_scale <- ifelse(stunting_data_indonesia$prevalensi[sort_idx] >= 20, '#dc3545',
                              ifelse(stunting_data_indonesia$prevalensi[sort_idx] >= 15, '#ffc107', '#28a745'))
      } else {
        # For cases and balita, use prevalence for coloring still
        color_scale <- ifelse(stunting_data_indonesia$prevalensi[sort_idx] >= 20, '#dc3545',
                              ifelse(stunting_data_indonesia$prevalensi[sort_idx] >= 15, '#ffc107', '#28a745'))
      }
    } else {
      # Use original order
      sorted_provinsi <- stunting_data_indonesia$provinsi
      sorted_y_data <- y_data
      sorted_hover <- hover_text
      
      # Create color scale based on prevalence
      if(input$chart_type == "percentage") {
        color_scale <- ifelse(stunting_data_indonesia$prevalensi >= 20, '#dc3545',
                              ifelse(stunting_data_indonesia$prevalensi >= 15, '#ffc107', '#28a745'))
      } else {
        # For cases and balita, use prevalence for coloring still
        color_scale <- ifelse(stunting_data_indonesia$prevalensi >= 20, '#dc3545',
                              ifelse(stunting_data_indonesia$prevalensi >= 15, '#ffc107', '#28a745'))
      }
    }
    
    # Create plot with plotly
    plot_data <- plot_ly(
      x = sorted_provinsi, 
      y = sorted_y_data,
      type = 'bar',
      marker = list(color = color_scale),
      hoverinfo = 'text',
      text = sorted_hover
    )
    
    # Format y-axis for large numbers
    y_format <- if(input$chart_type != "percentage") ",.0f" else ".1f"
    
    # Layout
    plot_data <- plot_data %>%
      layout(
        title = paste0("Data Stunting per Provinsi (", 2024, ")"),
        xaxis = list(
          title = "Provinsi",
          tickangle = 45
        ),
        yaxis = list(
          title = y_title,
          tickformat = y_format
        ),
        margin = list(b = 120) # Increase bottom margin for rotated labels
      )
    
    return(plot_data)
  })
  
  # Top provinces with highest prevalence
  output$top_stunting_provinces <- renderUI({
    # Order data by selected metric
    if(input$chart_type == "percentage") {
      ordered_data <- stunting_data_indonesia[order(-stunting_data_indonesia$prevalensi), ]
      value_format <- function(x) paste0(x, "%")
      value_column <- ordered_data$prevalensi
    } else if(input$chart_type == "cases") {
      ordered_data <- stunting_data_indonesia[order(-stunting_data_indonesia$jml_stunting), ]
      value_format <- function(x) format(x, big.mark=".")
      value_column <- ordered_data$jml_stunting
    } else {
      ordered_data <- stunting_data_indonesia[order(-stunting_data_indonesia$jml_balita), ]
      value_format <- function(x) format(x, big.mark=".")
      value_column <- ordered_data$jml_balita
    }
    
    # Take top 5
    top_5 <- ordered_data[1:5, ]
    
    # Create UI elements for top provinces
    top_provinces_ui <- lapply(1:5, function(i) {
      province <- top_5$provinsi[i]
      value <- value_column[i]
      prevalensi <- top_5$prevalensi[i]
      
      # Determine badge color
      badge_color <- ifelse(prevalensi >= 20, "danger",
                            ifelse(prevalensi >= 15, "warning", "success"))
      
      div(
        class = "ranking-item",
        div(
          class = "ranking-number",
          span(i)
        ),
        div(
          class = "ranking-content",
          h4(province),
          div(
            class = "ranking-details",
            span(value_format(value), class = "ranking-value"),
            tags$span(class = paste0("label label-", badge_color), 
                      paste0(prevalensi, "%"))
          )
        )
      )
    })
    
    # Combine all UI elements
    title_text <- switch(input$chart_type,
                         "percentage" = "Provinsi dengan Prevalensi Tertinggi",
                         "cases" = "Provinsi dengan Jumlah Kasus Terbanyak",
                         "toddlers" = "Provinsi dengan Jumlah Balita Terbanyak")
    
    tagList(
      h4(icon("trophy"), title_text),
      div(class = "ranking-container", top_provinces_ui)
    )
  })
  
  # Data table
  output$stunting_table <- DT::renderDataTable({
    # Filter data if search is used
    data_filtered <- stunting_data_indonesia
    if(input$table_search != "") {
      search_term <- tolower(input$table_search)
      data_filtered <- stunting_data_indonesia[grepl(search_term, tolower(stunting_data_indonesia$provinsi)), ]
    }
    
    # Format data for display
    display_data <- data_filtered
    display_data$jml_balita <- format(display_data$jml_balita, big.mark = ".")
    display_data$jml_stunting <- format(display_data$jml_stunting, big.mark = ".")
    display_data$prevalensi <- paste0(display_data$prevalensi, "%")
    
    # Rename columns for display
    display_data <- display_data[, c("provinsi", "jml_balita", "jml_stunting", "prevalensi", "tahun")]
    colnames(display_data) <- c("Provinsi", "Jumlah Balita", "Kasus Stunting", "Prevalensi", "Tahun")
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20),
        searching = FALSE,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Indonesian.json'
        )
      ),
      rownames = FALSE
    )
  })
  
  # Download handler for data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data-stunting-indonesia-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Create a data frame for download that includes all the original numeric values
      download_data <- stunting_data_indonesia
      colnames(download_data) <- c("Provinsi", "Prevalensi (%)", "Jumlah Balita", "Jumlah Kasus Stunting", "Tahun")
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  # Trend chart for national data
  output$stunting_trend <- renderPlotly({
    # National trend data (2018-2022, 2020 data not available)
    trend_data <- data.frame(
      tahun = c(2018, 2019, 2021, 2022),
      prevalensi = c(30.8, 27.7, 24.4, 21.6)
    )
    
    # Create chart
    plot_data <- plot_ly(
      data = trend_data,
      x = ~tahun,
      y = ~prevalensi,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#3c8dbc", width = 3),
      marker = list(size = 10, color = "#3c8dbc"),
      name = "Prevalensi Stunting"
    ) %>%
      add_annotations(
        x = trend_data$tahun,
        y = trend_data$prevalensi,
        text = paste0(trend_data$prevalensi, "%"),
        showarrow = TRUE,
        arrowhead = 0,
        arrowsize = 0.3,
        arrowwidth = 1,
        arrowcolor = "#3c8dbc",
        ax = 0,
        ay = -30
      )
    
    # Add target line for 2024
    plot_data <- plot_data %>%
      add_trace(
        x = c(2022, 2024),
        y = c(21.6, 14.0),  # Start from the 2022 actual value
        type = "scatter",
        mode = "lines",
        line = list(color = "#28a745", width = 2, dash = "dash"),
        name = "Target 2024"
      ) %>%
      add_annotations(
        x = 2024,
        y = 14.0,
        text = "Target 2024: 14%",
        showarrow = TRUE,
        arrowhead = 0,
        arrowsize = 0.3,
        arrowwidth = 1,
        arrowcolor = "#28a745",
        ax = 0,
        ay = -30
      )
    
    # Layout
    plot_data <- plot_data %>%
      layout(
        title = list(
          text = "Tren Prevalensi Stunting Nasional (2018-2022)",
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Tahun",
          dtick = 1,
          tickvals = c(2018, 2019, 2021, 2022, 2024),
          range = c(2017.5, 2024.5)
        ),
        yaxis = list(
          title = "Prevalensi (%)",
          range = c(0, 35)
        ),
        legend = list(orientation = "h", x = 0.5, xanchor = "center"),
        hovermode = "closest"
      )
    
    return(plot_data)
  })
}
# ------------------------------------------
# RUN THE APPLICATION
# ------------------------------------------
shinyApp(ui = ui, server = server)
