#############################################################################################################################################
# packages
#############################################################################################################################################

require(shiny)
require(leaflet)
require(dplyr)
require(DT)
library(shinydashboard)

#############################################################################################################################################
# load data
#############################################################################################################################################

#setwd("C:/Users/Alex/Documents/R/codingdurer/dutch_church")
artists = read.table("artistList.csv", header = T, sep = ",")
paintings = read.csv("paintingList.csv", header = T, sep = "\t")
locations = read.table("locationList.csv", header = T, sep = ",")
transfer = read.table("transferList.csv", header = T, sep = ",")
church = read.table("churchList.csv", header = T, sep = ",")

transfer = merge(transfer, locations, by = "location_id")
transfer = merge(transfer, artists, by = "artist_id")
transfer = select(transfer, name.y, lat, long, date_from, date_until)
colnames(transfer) = c("artist_name","lat","long","date_from","date_until")

artists_all = merge(artists, locations, by.x = "location_id_birth", by.y = "location_id", all.x = T)
artists_all = merge(artists_all, locations, by.x = "location_id_death", by.y = "location_id", all.x = T)
artists_all = filter(artists_all, !grepl('Follower|Circle', name.x))
artists_all = select(artists_all, artist_id, name.x, name.y, lat.x, long.x, name, lat.y, long.y)
colnames(artists_all) = c("artist_id","artist_name","birthplace","birthplace_lat","birthplace_long","deathplace","deathplace_lat","deathplace_long")
rm(artists, locations)

paintings_all = merge(paintings, church, by = "church_id")
paintings_all = select(paintings_all, artist_id, name.x, date, Image.Link, Painting.Page.Link, name.y, church_lat, church_long)
colnames(paintings_all) = c("artist_id","painting_name","painting_year","painting_image_link","painting_page_link","church","church_lat","church_long")
paintings_all$painting_name2 <- paste0("<a href='",paintings_all$painting_page_link,"'>",paintings_all$painting_name,"</a>")

monster = merge(paintings_all, artists_all, by = "artist_id", all.x = T)
rm(paintings, church, artists_all, paintings_all)

monster = monster[!is.na(monster$artist_name),]
monster = monster[!is.na(monster$birthplace_lat),]

monster = monster[monster$artist_name %in% unique(transfer$artist_name), ]

#############################################################################################################################################
# ui
#############################################################################################################################################

ui <- dashboardPage(skin = "red",
                    
  dashboardHeader(title = "Church Interior Paintings", titleWidth = 300),
  
  # sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Artist Analysis", icon = icon("dashboard"),
      menuItem("Map", tabName = "dashboard1", selected = T),
      menuItem("Data Explorer", tabName = "data1")),
      
      menuItem("Church Analysis", icon = icon("dashboard"),
      menuItem("Map", tabName = "dashboard2"),
      menuItem("Data Explorer", tabName = "data2")),
      
      menuItem("Timeline", tabName = "timeline", icon = icon("dashboard")),
      
      menuItem("About the Project", tabName = "about",
      menuItem("Background", tabName = "about"),
      menuItem("Learn more", tabName = "learn"),
      menuItem("About the Team", tabName = "team"),
      menuItem("Contact", tabName = "contact")))),
  
  #
  dashboardBody(
    tabItems(
      
      # First tab content
      tabItem(tabName = "dashboard1",
              fluidRow(
                
                # main panel
                tags$style(type = "text/css", "#MapPlot1 {height: calc(100vh - 80px) !important;}"),
                leafletOutput("MapPlot1", height = 900),
                
                tags$head(tags$style(
                  HTML('
                       #sidebar {
                       background-color: rgba(255, 255, 255, 0.7);
                       }'))),
                
                # sidebar
                absolutePanel(id = "sidebar", fixed = TRUE,
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                              width = 200, height = 870,
                  
                # slider
                sliderInput(inputId = "time", 
                            label = "Select a time period:", 
                            min = 1600, max = 1700, value = c(1600,1700), step = 10),
                # chechbox
                checkboxGroupInput(inputId = "artist",
                                   label = "Choose an Artist", 
                                   choices = sort(unique(monster$artist_name)), selected = sort(unique(monster$artist_name))),
                                   actionButton("Uncheck", label="Unselect"),
                br(),br(),
                
                # actionbutton
                actionButton("go", "Click through Images"),
                
                # output
                br(),br(),
                uiOutput('logo'),
                uiOutput("logo_text")))),
      
      
      # second tab content
      tabItem(tabName = "data1",
              fluidRow(
                
                # data table
                DT::dataTableOutput('x4'))),
      
      #  tab content
      tabItem(tabName = "dashboard2",
              fluidRow(
                
                # main panel
                tags$style(type = "text/css", "#MapPlot2 {height: calc(100vh - 80px) !important;}"),
                leafletOutput("MapPlot2", height = 900),
                
                # sidebar
                absolutePanel(id = "sidebar", fixed = TRUE,
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                              width = 200, height = 870,
                              
                              # slider
                              sliderInput(inputId = "time2", 
                                          label = "Select a time period:", 
                                          min = 1600, max = 1700, value = c(1600,1700), step = 10),
                              # chechbox
                              checkboxGroupInput(inputId = "church",
                                                 label = "Choose a Church", 
                                                 choices = sort(unique(monster$church))),
                              actionButton("Uncheck2", label="Unselect"),
                              br(),
                              
                              # actionbutton
                              actionButton("go2", "Click through Images"),
                              
                              # output
                              br(),
                              uiOutput('logo2'),
                              uiOutput("logo_text2")))),
      
      
      # second tab content
      tabItem(tabName = "data2",
              fluidRow(
                
                # data table
                DT::dataTableOutput('x42'))),
      
      # second tab content
      tabItem(tabName = "timeline",
              fluidRow(
                
                tags$iframe(src = "https://cdn.knightlab.com/libs/timeline3/latest/embed/index.html?source=1ZyqFS-yCi0RWCzUHdtYvloBFMGTwsVWXk8IQ9KdiHlI", 
                            seamless=NA, height = "600", width = "100%")
              )),
                
                

      #  tab content
      tabItem(tabName = "about",
              fluidRow(
                
                h3("Download the data:"),
                downloadButton('downloadData', 'Download'),
                
                tags$div(
                  HTML("<p>&nbsp;</p>
                       <p><strong>Dutch Church Interior Paintings</strong></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>The genre of church interior paintings developed in the Netherlands in the middle of the 17th century and lasted only a few decades. It is represented by a relatively small group of specialized artists, including Pieter Jansz Saenredam (1597-1665), Emanuel de Witte (1616-1692), Hendrick Cornelisz Van Vliet (1611-1675), Gerard Houckgeest (ca.1600&ndash;1661), Anthonie De Lorme (c.1610-1673) and others. In many cases, the same church&rsquo;s interior was depicted by the same artists dozens of times; however, the iconography, composition and vantage point (a position from which the interior is viewed) varied. </span></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>One of the main factors in the development of this type of painting was the Reformation and its consequences, particularly the Calvinist approach to art. The so-called Beeldenstorm in 1566, a series of events during which churches were plundered and their Catholic decorations removed or destroyed, was a starting point of this far-reaching transformation of church interiors in the Netherlands. The churches became obsolete civic spaces filled with everyday activities, not exclusively restricted to preaching God&rsquo;s word any more. The altars, statues and other decorative elements were replaced by white-washed walls and simple panels filled with biblical excerpts instead of representations of saints and miracles. This is reflected in church interior paintings, where we can see, for example, a woman breastfeeding, children at play, groups of gentlemen involved in conversations about business, couples strolling down the aisles, beggars and even dogs urinating. The latter subject was perhaps the strongest symbol of this transition of the church as a building: from a holy temple to a civic, urban and mundane space. </span></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>There are hundreds of church interior paintings scattered across collections around the world. The research of this subject to date has focused mainly on particular artists or churches, rather than the overall genre and its network of artists and places. This project, born during the </span><a href=http://codingdurer.de/><span style=font-weight: 400;>Coding Durer 2017</span></a><span style=font-weight: 400;>, addresses this issue by providing a platform for further research on the paintings and creating an insight into the bigger picture of the genre for the first time. This visualisation of over 200 paintings of 26 different churches by 16 different artists was created with the following research questions in mind:</span></p>
                       <p>&nbsp;</p>
                       <p>&nbsp;</p>
                       <ul>
                       <li><strong>In what places the artists were active and in what places did they depict church interior(s)? </strong></li>
                       </ul>
                       <ul>
                       <li>Did the artists have &lsquo;favourite&rsquo; church interiors?</li>
                       </ul>
                       <ul>
                       <li>In what places and when would the artists possibly meet?</li>
                       </ul>
                       <ul>
                       <li>What church interiors were depicted the most?</li>
                       </ul>
                       <ul>
                       <li>What church interiors were depicted by most artists?</li>
                       </ul>
                       <p>&nbsp;</p>
                       <p><br /><br /></p>
                       <p><strong>DATASET</strong></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>The starting point of the project was a spreadsheet listing the paintings, artists, collections, etc. that was created for research purposes two years ago. This re-purposed data needed cleaning and additional information, e.g. IDs (artists, churches, paintings), locations (longitude, latitude), and stable URLs for images. The dataset is available to download here</span><span style=font-weight: 400;> [add link later]</span><span style=font-weight: 400;>.</span></p>
                       <p>&nbsp;</p>
                       <p><strong>Goal:</strong></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>To create a map/visualisation that shows:</span></p>
                       <ol>
                       <li style=font-weight: 400;><span style=font-weight: 400;>Dutch churches depicted in the paintings (25)</span></li>
                       <li style=font-weight: 400;><span style=font-weight: 400;>Artists&rsquo; activity (16+)</span></li>
                       </ol>
                       <p><br /><br /></p>
                       <p><strong>TOOLS / METHODS / DATABASES ETC.</strong></p>
                       <p><span style=font-weight: 400;>Add here anything you used in this project: things that worked or didn&rsquo;t work, challenges, etc.</span></p>
                       <p>&nbsp;</p>
                       <ul>
                       <li style=font-weight: 400;><a href=https://www.microsoft.com/cognitive-services><span style=font-weight: 400;>Microsoft Cognitive Services</span></a><span style=font-weight: 400;> - Computer Vision API: Calling to get more data for Images like colors, tags and alike</span></li>
                       <li style=font-weight: 400;><a href=https://en.wikipedia.org/wiki/Bash_(Unix_shell)><span style=font-weight: 400;>Bash shell</span></a><span style=font-weight: 400;> scripts, </span><a href=https://en.wikipedia.org/wiki/CURL><span style=font-weight: 400;>cURL</span></a><span style=font-weight: 400;>: Iterating over data entries and fetching Computer Vision API responses and saving as well as transforming data</span></li>
                       <li style=font-weight: 400;><a href=https://en.wikipedia.org/wiki/JSON><span style=font-weight: 400;>JSON</span></a><span style=font-weight: 400;>, </span><a href=https://en.wikipedia.org/wiki/Comma-separated_values><span style=font-weight: 400;>CSV</span></a><span style=font-weight: 400;>, plain text files, </span><a href=https://www.google.com/sheets/about/><span style=font-weight: 400;>Google Sheets</span></a><span style=font-weight: 400;>: Saving API responses and </span></li>
                       <li style=font-weight: 400;><a href=https://www.wikidata.org/wiki/Wikidata:Main_Page><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> data entries: Filling gaps in data references</span></li>
                       <li style=font-weight: 400;><a href=https://shiny.rstudio.com><span style=font-weight: 400;>Shiny by RStudio</span></a><span style=font-weight: 400;>: A web application framework for the R programming language which turns data analysis into interactive web applications</span></li>
                       <li style=font-weight: 400;><a href=https://rkd.nl/en/explore/artists><span style=font-weight: 400;>The RKD (Netherlands Institute for Art History) Artists</span></a><span style=font-weight: 400;> database was used for biographical information of the artists, particularly the details about their activity (location, years).</span></li>
                       <li style=font-weight: 400;><span style=font-weight: 400;>Google Drive applications: used for collaboration and project management </span></li>
                       <li style=font-weight: 400;><a href=http://www.getty.edu/research/tools/vocabularies/ulan/index.html><span style=font-weight: 400;>ULAN (Union List of Artist Names&reg;)</span></a><span style=font-weight: 400;>, the Getty&rsquo;s structured vocabulary, used for artists names</span></li>
                       <li style=font-weight: 400;><span style=font-weight: 400;>Museum collection pages (various sites): are the most reliable for image quality and metadata, including accession/object numbers</span></li>
                       </ul>")
                ))),
      
      tabItem(tabName = "learn",
              fluidRow(
                
                tags$div(
                  HTML("<p><strong>ARTISTS</strong></p>
                       <p>&nbsp;</p>
                       <p><strong>Job Adriaensz. Berckheyde (b. 1630, Haarlem, d. 1693, Haarlem)</strong></p>
                       <p><a href=https://rkd.nl/explore/artists/6744><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q576120><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/b/berckhey/job/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Dutch painter, the elder brother of Gerrit Adriaensz. Berckheyde (1638-1698). His work is similar to his brother's, it is also rarer and more varied, including genre and biblical scenes.</span></p>
                       <p>&nbsp;</p>
                       <p><strong><br /></strong><strong>Daniel de Blieck (b. ca. 1610, Middelburg, d. 1673, Middelburg)</strong></p>
                       <p><a href=https://rkd.nl/explore/artists/9039><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q15702751><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/b/blieck/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Dutch painter and architect who specialised in architectural paintings. He was probably a pupil of architectural painter Dirck van Delen. He became a member of the Middelburg Guild of Saint Luke c. 1648 and was deacon of the Guild in 1664-1665 and 1668. He stayed in England from 1658 to 1661.</span></p>
                       <p><span style=font-weight: 400;>When the governing body of Zeeland decided to mint coins mechanically, de Blieck was commissioned to produce drawings on the production of the necessary equipment. He knew the equipment in Paris and had made drawings of the minting equipment in Dordrecht. The building of the minting machines was not without problems, and de Blieck made a number of trips to Antwerp, Rotterdam and The Hague as supervisor for repairs and parts.</span></p>
                       <p><span style=font-weight: 400;>He principally painted architectural studies, but is also known for some portraits. His imaginary architectural paintings of church interiors were in the style of the Flemish painter Hendrick Aerts (active c. 1600), but under the influence of architectural painters of the Delft school such as Hendrick Cornelisz. van Vliet, he developed a more realistic style around 1650.</span></p>
                       <p><span style=font-weight: 400;>De Blieck was also an architect but little is known about his work in this field. He designed a new warehouse for the Dutch East India Company.</span></p>
                       <p>&nbsp;</p>
                       <p><strong><br /></strong><strong>Anthonie de Lorme (b. ca. 1610, Tournai, d. 1673-76, Rotterdam)</strong></p>
                       <p><a href=https://rkd.nl/explore/artists/50947><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q2219148><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/l/lorme/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Dutch painter, specialized in church interiors and painted mostly in Rotterdam. His earliest known picture, a small panel signed and dated 1639, shows a coffered barrel vault reflecting candlelight in Rotterdam's Laurenskerk. His imaginary interiors of the 1640s often betrayed an enthusiasm for flashy effects like candlelit vaults and pools of shade thrown by glowing chandeliers.</span></p>
                       <p><span style=font-weight: 400;>Imaginary church views were De Lorme's specialty for the first twenty years of his career, but around 1652 he turned suddenly to painting simple and accurate renditions of local churches with a few figures, probably inspired by painters in nearby Delft. During the 1650s, he painted church interiors close-up, bathed in light and atmosphere. He painted the Laurenskerk at least seventeen times, so accurately that his paintings were used by the church's restorers after World War II.</span></p>
                       <p><span style=font-weight: 400;>In the 1660s De Lorme refined his simple, realistic style into a more decorative manner, using architecture to create an elegant geometric pattern rather than an objective view.</span></p>
                       <p>&nbsp;</p>
                       <p><strong><br /></strong><strong>Cornelis de Man (b. 1621, Delft, d. 1706, Delft)</strong></p>
                       <p><a href=https://rkd.nl/explore/artists/52219><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q2282191><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/m/man/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Dutch painter. He spent several years in Italy and France, an experience from which he did not greatly benefit for when he returned home he produced works with an abundance of detail, chiefly conversation pieces depicting middle-class life, in the manner of Vermeer and de Hooch. Although Pieter de Hooch had no recorded pupils he influenced and was imitated by other painters in Delft and Amsterdam, and since his own original works decline in his later period, his followers, among them de Man, come at times pretty close to him.</span></p>
                       <p>&nbsp;</p>
                       <p><strong><br /></strong><strong>Wolfgang de Smet</strong></p>
                       <p><a href=https://rkd.nl/explore/artists/73242><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q19288384><span style=font-weight: 400;>Wikidata</span></a></p>
                       <p><br /><br /></p>
                       <p><strong>Emanuel de Witte (b. 1617, Alkmaar, d. 1692, Amsterdam)</strong></p>
                       <p><a href=https://rkd.nl/explore/artists/85180><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q711203><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/w/witte/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Dutch painter, active in his native Alkmaar, then in Rotterdam (by 1639), Delft (by 1641), and Amsterdam (by 1652). His range was wide, including history paintings, genre scenes (notably of markets) and portraits, but after he settled in Amsterdam he concentrated on architectural paintings (primarily church interiors, both real and imaginary).</span></p>
                       <p><span style=font-weight: 400;>His paintings are very different in spirit from the sober views of most Dutch architectural specialists, making powerful use of the dramatic play of light and shadow in the lofty interiors. His life was unhappy (he was constantly in debt) and when his body was found in an Amsterdam canal it was suspected that he had committed suicide.</span></p>
                       <p>&nbsp;</p>
                       <p><strong><br /></strong><strong>Louwijs Aernouts Elsevier (b. 1618, Leiden, d. 1675, Delft)</strong></p>
                       <p><a href=https://rkd.nl/explore/artists/26074><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q12012818><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/e/elsevier/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Dutch painter, son of a painter and innkeeper in Leiden. He joined the Leiden guild in 1645. In the same year he married Helena Waelpot, the daughter of a prominent printer in the university town. In 1646 he registered in the painters' guild in Delft, he served as headman in 1669 and 1673. In 1648 the artist, who also dealt in dyes and pigments, bought a house called 'The Blue Dog' just beside the choir of the Oude Kerk in Delft. He was buried in the Oude Kerk in 1675.</span></p>
                       <p><span style=font-weight: 400;>Only four paintings by Elsevier are known: a view of the interior of a stable (1647), a landscape (1647), a landscape (1648), and a view of the interior of the Oude Kerk (1653).</span></p>
                       <p><br /><br /><br /></p>
                       <p><strong>Gerard Houckgeest (b. ca. 1600, The Hague, d. 1661, Delft)</strong></p>
                       <p><a href=https://rkd.nl/explore/artists/39958><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q1935857><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/h/houckgee/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Dutch painter who worked in Delft and specialized in church interiors. He was probably a van Bassen pupil and began as a painter of imaginary church interiors and renaissance buildings. Most likely he was in England during the 1630s; Charles I owned at least five 'prospectives' by or partly by him. His fictive Palace Interior of 1635, his earliest existing dated painting, is still at Hampton Court. Houckgeest is documented in Delft in the 1640s. His first known depiction of an actual church interior is his unexpected New Church in Delft with the Tomb of Willem the Silent, dated 1650, now at Hamburg. During the following 4-5 years he painted about half-dozen pictures of both the New and Old Church of Delft using the innovative diagonal perspective of the Hamburg painting.</span></p>
                       <p><span style=font-weight: 400;>After these radical innovations he did not remain in Delft for very long. By 1651 he is recorded as a resident of Steebergen, a town about forty kilometres south of Rotterdam, and two or three years later he settled in Bergen op Zoom in North Brabant.</span></p>
                       <p><br /><br /></p>
                       <p><strong>Pieter Neefs I (the Elder) (c. 1578, Antwerp - c. 1656, Antwerp)</strong><strong><br /></strong><a href=https://rkd.nl/explore/artists/59042><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q596352><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/n/neeffs/peeter_e/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Flemish painter, part of a family of painters active in Antwerp. All three members of the family, Peeter Neefs the Elder and his sons Ludovicus Neeffs and Peeter Neeffs the Younger, specialized in paintings of architectural interiors. Their most frequent subject was the interior of Antwerp Cathedral; the details of sculpture, altars and paintings vary in accuracy, and sometimes the subject seems to be very freely interpreted. The Neefs also liked to depict the effects of artificial illumination in crypt-like spaces (in the manner of Hendrick van Steenwijck the Younger). Iconographic and stylistic similarities make the works of Peeter the Elder and Peeter the Younger often difficult to distinguish. Their paintings are generally small, painted on copper, and executed in a precise, neat way.</span></p>
                       <p><span style=font-weight: 400;>On a few occasions the father signed his works DEN AUDEN NEEFS. Generally speaking, those works dated before c. 1640 (when Peeter the Younger would have become involved in the workshop) are superior in quality. It is also possible that works attributed to either Peeter the Elder or Peeter the Younger are, in fact, by Ludovicus, the least-known member of the family. The figures in the architectural views by the various Neeffs were painted by such artists as Frans Francken II and Frans Francken III, Jan Breughel I, Sebastiaen Vrancx, Adriaen van Stalbemt, David Teniers the Younger, Gonzales Coques and Bonaventura Peeters.</span></p>
                       <p><br /><br /></p>
                       <p><strong>Pieter Neefs II (the Younger) (b. 1620, Antwerp, d. ca. 1675, Antwerp)</strong><strong><br /></strong><a href=https://rkd.nl/explore/artists/59043><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q3304139><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/n/neeffs/peeter_y/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Flemish painter, son of Pieter Neefs the Elder. By 1640 he was collaborating with his father and was never apparently enrolled as an independent master in the Guild of St Luke. He was still active in 1675, the year that appears on his last known dated painting (Vaduz, Sammlung Liechtenstein). He painted the same subjects as his father and it is very difficult to distinguish between their hands.</span></p>
                       <p><br /><br /></p>
                       <p><strong>Pieter Jansz. Saenredam (b. 1597, Assendelft, d. 1665, Haarlem)</strong><strong><br /></strong><a href=https://rkd.nl/explore/artists/69237><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q433804><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/s/saenreda/pieter/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Dutch painter of architectural subjects, particularly church interiors, active in Haarlem. Saenredam, the son of an engraver, was a hunchback and a recluse, but he was acquainted with the great architect Jacob van Campen, who may have played a part in determining his choice of subject. He was the first painter to concentrate on accurate depictions of real buildings rather than the fanciful inventions of the Mannerist tradition. His pictures were based on painstaking drawings and are scrupulously accurate and highly finished, but they never seem pedantic or niggling and are remarkable for their delicacy of colour and airy grace. The Cathedral of St Bavo (where he is buried) in Haarlem was favourite subject, but he also travelled to other Dutch towns to make drawings, and Utrecht is represented in several of his paintings. He also made a few views of Rome based on drawings in a sketchbook by Marten van Heemskerck that he owned. His work had great influence on Dutch painting.</span></p>
                       <p><br /><br /></p>
                       <p><strong>Bartholomeus van Bassen (b. ca. 1590, the Hague, d. 1652, the Hague)</strong><strong><br /></strong><a href=https://rkd.nl/explore/artists/4912><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q613336><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/b/bassen/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Van Bassen was an architect to the courts of Orange and Bohemia, he had a reputation as one of the most important architectural painter of the first half of the seventeenth century. Nothing is known about his training. The first known document related to him is his registration with the Guild of St Luke in Delft in 1613. By 1624 he was a member of the guild at The Hague, of which he became dean in 1627 and headman twice, in 1636 and 1640. He married Aaltgen Pietersdr van Gilst at The Hague in 1624. From 1629 until 1634 he was occupied with commissions from the stadholder Frederick Hendrick for the Honselaarsdijk and Ter Nieuburch palaces near The Hague. In 1630-31 he worked as the principal architect on the rebuilding of the monastery of St Agnes in Rhenen as a residence for Frederick V, Elector Palatine and king of Bohemia, and his wife, Elizabeth Stuart. Van Bassen was also involved in a number of architectural projects at The Hague and elsewhere.</span></p>
                       <p><span style=font-weight: 400;>Van Bassen died shortly after his wife and was buried in the Jacobskerk at The Hague in 1652. His son in 1651 married Adriana, the daughter of the painter Cornelis van Poelenburgh. Only the architectural painter Gerard Houckgeest can be identified with some degree of certainty as Van Bassen's pupil.</span></p>
                       <p><br /><br /></p>
                       <p><strong>Isaak van Nickelen (b. ca. 1632, Haarlem, d. 1703, Haarlem)</strong><strong><br /></strong><a href=https://rkd.nl/explore/artists/59390><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q6077508><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/n/nickelen/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Dutch painter of church interiors. There is little surviving information about his life. He was a businessman as well as an artist, and had interests in silk trading and glass manufacturing. Based on a not in an auction catalogue of 1711, stating that he had painted figures in an interior by Pieter Jansz Saenredam, and on the fact that his paintings are strongly influenced by the older master, it was presumed that he was Saenredam's pupil. There is, however, no documentary evidence in support of this hypothesis.</span></p>
                       <p><span style=font-weight: 400;>He joined the Haarlem Guild of St. Luke in 1660. All known and dated church interiors by Van Nickelen depict Saint Bavo's Church in Haarlem and all but one were painted in the 1690s. It is not known what kind of painting he produced during the thirty-year period between the two dates.</span></p>
                       <p><span style=font-weight: 400;>His son, Jan van Nickelen (1656-1721) was a landscape painter.</span></p>
                       <p><br /><br /></p>
                       <p><strong>Hendrick van Streek (b. 1659, Amsterdam, d. ca. 1719, Amsterdam)</strong><strong><br /></strong><a href=https://rkd.nl/explore/artists/75694><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q5713748><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/s/streeck/hendrick/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Dutch painter, part of a family of painters, the son of Juriaen van Streeck. After learning to draw from his father, he studied sculpting with Willem van der Hoeven. In 1683 he married Maria van Hokkum. When his father died in 1687, Hendrick was advised by Melchior de Hondecoeter to stick to painting, and so he became a student of Emmanuel de Witte. He learned to paint church interiors so well that his art was hard to tell apart from De Witte's.</span></p>
                       <p><span style=font-weight: 400;>Van Streeck also painted landscapes. In 1696 he was involved in the decoration of the townhall in Alkmaar.</span></p>
                       <p><br /><br /></p>
                       <p><strong>Hendrik Cornelisz. van Vliet (b. ca. 1611, Delft, d. 1675, Delft)</strong><strong><br /></strong><a href=https://rkd.nl/explore/artists/81475><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q864173><span style=font-weight: 400;>Wikidata</span></a><span style=font-weight: 400;> | </span><a href=http://www.wga.hu/bio_m/v/vliet/biograph.html><span style=font-weight: 400;>WGA</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Dutch painter. He is the only living artist discussed in Dirck van Bleyswijck's contemporary description of Delft, where he is said to have studied with his uncle, the portrait painter Willem van Vliet, and then with Michiel van Mierevelt.</span></p>
                       <p><span style=font-weight: 400;>Hendrick entered the Guild in 1632, later specializing in painting portraits, history and perspective views, notably of church interiors. He lived in an alley on Kromstraat in 1633; opposite the Bagijnhof on Oude Delft (probably at) number 202, in 1639 and 1645. In 1653 and 1675 he lived on the east side of Oude Delft.</span></p>
                       <p><span style=font-weight: 400;>From 1632 to c. 1650 van Vliet practised portraiture in a conservative South Holland style. Around 1651 he turned to painting interior views of actual churches, mostly the Oude Kerk or the Nieuwe Kerk in Delft. His earliest known dated architectural picture, the Pieterskerk in Leiden (1652; Brunswick, Herzog Anton Ulrich-Museum), is one of about 20 paintings representing churches in Leiden, Haarlem and Dutch towns other than Delft. In 1658 he painted a view of the new Admiral Tromp funerary monument in the Oude Kerk (Old Church), commissioned by Tromps's widow.</span></p>
                       <p><span style=font-weight: 400;>Van Vliet died in poverty; his widow was still living on Oude Delft opposite the Bagijnhof in 1689.</span></p>
                       <p><br /><br /></p>
                       <p><strong>Lieve Verschuier (b. 1627, Rotterdam, d. 1686, Rotterdam)</strong></p>
                       <p><a href=https://rkd.nl/explore/artists/80634><span style=font-weight: 400;>RKD</span></a><span style=font-weight: 400;> | </span><a href=https://www.wikidata.org/wiki/Q3832152><span style=font-weight: 400;>Wikidata</span></a></p>
                       <p><br /><br /><br /><br /><br /></p>
                       <p><strong>SELECTED BIBLIOGRAPHY</strong></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/4104349><span style=font-weight: 400;>Blade, Timothy Trent, &lsquo;Two Interior Views of the Old Church in Delft&rsquo;, Art Institute of Chicago Museum Studies, 6 (1971)</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Carter, B.A.R., 'The use of perspective in Saenredam', Burlington Magazine, 109 (1967), 594-595</span></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/3780767><span style=font-weight: 400;>Giltaij, Jeroen, Gary Schwartz, and Marten Jan Bok, &lsquo;Pieter Saenredam: The Painter and His Time&rsquo;, Simiolus: Netherlands Quarterly for the History of Art, 20 (1990)</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=http://www.jstor.org/stable/20605637><span style=font-weight: 400;>Gregerson, Linda, &lsquo;Interior of the Oude Kerk, Delft, with Open Grave&rsquo;, Poetry, 2002, 338&ndash;340 </span></a></p>
                       <p><a href=https://doi.org/10.2307/3050934><span style=font-weight: 400;>Kemp, Martin, &lsquo;Simon Stevin and Pieter Saenredam: A Study of Mathematics and Vision in Dutch Science and Art&rsquo;, The Art Bulletin, 68 (1986), 237&ndash;52</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/40382315><span style=font-weight: 400;>Lammertse, Friso, &lsquo;Van Schets Tot Schilderij. Naar Aanleiding van Infraroodreflectografisch Onderzoek Op Twee Schilderijen van Saenredam in Het Rijksmuseum&rsquo;, Bulletin van Het Rijksmuseum, 35 (1987), 80&ndash;90</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/3051056><span style=font-weight: 400;>Liedtke, Walter, &lsquo;Architectural Painting in Delft&rsquo;, The Art Bulletin, 68 (1986), 690</span></a></p>
                       <p><span style=font-weight: 400;>&mdash;&mdash;&mdash;, &lsquo;De Witte and Houckgeest: Two New Paintings from Their Years in Delft&rsquo;, The Burlington Magazine, 1986, 802&ndash;805</span></p>
                       <p><span style=font-weight: 400;>Liedtke, Walter A., Architectural Painting in Delft (Davaco, 1982)</span></p>
                       <p><span style=font-weight: 400;>&mdash;&mdash;&mdash;, &lsquo;Saenredam&rsquo;s Space&rsquo;, Oud Holland, 86, 1 (1971), 116&ndash;141</span></p>
                       <p><span style=font-weight: 400;>&mdash;&mdash;&mdash;,</span><a href=https://doi.org/10.2307/3780448><span style=font-weight: 400;> &lsquo;The New Church in Haarlem Series: Saenredam&rsquo;s Sketching Style in Relation to Perspective&rsquo;, Simiolus: Netherlands Quarterly for the History of Art, 8 (1975), 145</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/3049926><span style=font-weight: 400;>Liedtke, Walter A., and Arthur K. Wheelock Jr., &lsquo;Perspective, Optics, and Delft Artists Around 1650 (Outstanding Dissertations in the Fine Arts)&rsquo;, The Art Bulletin, 61 (1979), 490</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/1483706><span style=font-weight: 400;>Michalski, Sergiusz, &lsquo;Rembrandt and the Church Interiors of the Delft School&rsquo;, Artibus et Historiae, 23 (2002), 183</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/3780525><span style=font-weight: 400;>Montias, John Michael, &lsquo;Painters in Delft, 1613-1680&rsquo;, Simiolus: Netherlands Quarterly for the History of Art, 10 (1978), 84</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=http://www.dalnet.lib.mi.us/dia/collections/diaBulletins/16-7.pdf><span style=font-weight: 400;>Richardson, E.P., &lsquo;Architectural Painting in the Netherlands&rsquo;, Bulletin of the Detroit Institute of Arts, 16 (1937), 106&ndash;13</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=http://www.ingentaconnect.com/content/brill/oh/1983/00000097/00000002/art00002?crawler=true><span style=font-weight: 400;>Ruurs, Rob, &lsquo;Pieter Saenredam: Zijn Boekenbezit En Zijn Relatie Met de Landmeter Pieter Wils&rsquo;, Oud Holland, 97 (1983), 59&ndash;67 </span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/3780492><span style=font-weight: 400;>Schwartz, Gary, &lsquo;Saenredam, Huygens and the Utrecht Bull&rsquo;, Simiolus: Netherlands Quarterly for the History of Art, 1 (1966), 69</span></a></p>
                       <p><span style=font-weight: 400;>&mdash;&mdash;&mdash;, </span><a href=http://www.dbnl.org/tekst/_low001199301_01/_low001199301_01_0032.php><span style=font-weight: 400;>&lsquo;Warmth in Cold Stone &nbsp;The Architectural Paintings of Pieter Saenredam&rsquo;, The Low Countries. Jaargang 1, 1 (1993), 231&ndash;38 </span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Schwartz, Gary, and Marten Jan Bok, Pieter Saenredam: The Painter and His Time (London: Thames and Hudson, 1990)</span></p>
                       <p>&nbsp;</p>
                       <p><a href=http://www.shiftjournal.org/archives/articles/2009/scribner.pdf><span style=font-weight: 400;>Scribner, Matthew, &lsquo;Illusion and Iconoclasm in Emmanuel de Witte&rsquo;s A Sermon in the Old Church in Delft&rsquo;, Shift. Queen&rsquo;s Journal of Visual and Material Culture., 2009 </span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/3169631><span style=font-weight: 400;>Sprunger, Keith L., &lsquo;Puritan Church Architecture and Worship in a Dutch Context&rsquo;, Church History, 66 (1997), 36</span></a></p>
                       <p>&nbsp;</p>
                       <p><span style=font-weight: 400;>Stechow, Wolfgang, &lsquo;A Church Interior by Emanuel de Witte&rsquo;, The Bulletin of the Cleveland Museum of Art, 1972, 228&ndash;235</span></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/4115992><span style=font-weight: 400;>Sweet, Frederick A., &lsquo;A Church Interior by Emanuel De Witte&rsquo;, Bulletin of the Art Institute of Chicago (1907-1951), 36 (1942), 65</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.1093/oxartj/kcn029><span style=font-weight: 400;>Vanhaelen, A., &lsquo;Recomposing the Body Politic in Seventeenth-Century Delft&rsquo;, Oxford Art Journal, 31 (2008), 361&ndash;81</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/25067171><span style=font-weight: 400;>Vanhaelen, Angela, &lsquo;Iconoclasm and the Creation of Images in Emanuel de Witte&rsquo;s &ldquo;Old Church in Amsterdam&rdquo;&rsquo;, The Art Bulletin, 87 (2005), 249&ndash;64</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/3780622><span style=font-weight: 400;>Veltman, Kim H., and Rob Ruurs, &lsquo;Saenredam: The Art of Perspective&rsquo;, Simiolus: Netherlands Quarterly for the History of Art, 17 (1987), 275</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=http://www.jstor.org/stable/884983><span style=font-weight: 400;>de Vries, Lyckle, 'Saenredam and Seventeenth-Century Architectural Painting. Rotterdam', The Burlington Magazine, 134 (1992), 51-54</span></a></p>
                       <p><span style=font-weight: 400;>&mdash;&mdash;&mdash;, </span><a href=http://www.jstor.org/stable/889189><span style=font-weight: 400;>&lsquo;Saenredam. Utrecht&rsquo;, The Burlington Magazine, 143 (2001), 108&ndash;10</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=http://www.nga.gov/content/dam/ngaweb/collection/catalogue/17th-century-dutch-paintings/versions/2014-04-24_17th-century-dutch-paintings.pdf><span style=font-weight: 400;>Wheelock Jr., Arthur K., Dutch Paintings of the Seventeenth Century, NGA Online Editions (National Gallery of Art, Washington, DC, 2014)</span></a></p>
                       <p><span style=font-weight: 400;>&mdash;&mdash;&mdash;,</span><a href=https://doi.org/10.2307/3780449><span style=font-weight: 400;> &lsquo;Gerard Houckgeest and Emanuel de Witte: Architectural Painting in Delft around 1650&rsquo;, Simiolus: Netherlands Quarterly for the History of Art, 8 (1975), 167</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/1483203><span style=font-weight: 400;>Wheelock Jr., Arthur K., and C. J. Kaldenbach, &lsquo;&ldquo;Vermeer&rsquo;s View of Delft&rdquo; and His Vision of Reality&rsquo;, Artibus et Historiae, 3 (1982), 9</span></a></p>
                       <p>&nbsp;</p>
                       <p><a href=https://doi.org/10.2307/3050878><span style=font-weight: 400;>Wheelock Jr., Arthur K., and Walter A. Liedtke, &lsquo;Architectural Painting in Delft&rsquo;, The Art Bulletin, 68 (1986), 169</span></a></p>")
      ))),
    
    tabItem(tabName = "contact",
            fluidRow(
              
              tags$div(
                HTML("<p><span style=font-weight: 400;>Please email </span><a href=mailto:karolina.badz@gmail.com><span style=font-weight: 400;>karolina.badz@gmail.com</span></a><span style=font-weight: 400;> if you have any questions regarding the project, want to report a bug or suggest corrections in the dataset.</span></p>"))
    
    
    
)),


tabItem(tabName = "team",
        fluidRow(
          
          tags$div(
            HTML("<p><span style=font-weight: 400;>The team got together at the </span><a href=http://codingdurer.de/><span style=font-weight: 400;>Coding Durer 2017</span></a><span style=font-weight: 400;> in Munich, Germany, and worked on the project between 13-17 March 2017.</span></p>
  <p>&nbsp;</p>
  <p><strong>Karolina Badzmierowska </strong></p>
  <p><span style=font-weight: 400;>PhD Candidate in Digital Humanities &amp; History of Art, Trinity College Dublin</span></p>
    <p><span style=font-weight: 400;>Email: </span><a href=mailto:karolina.badz@gmail.com><span style=font-weight: 400;>karolina.badz@gmail.com</span></a><span style=font-weight: 400;> Twitter: </span><a href=https://twitter.com/karolinabadz><span style=font-weight: 400;>@karolinabadz </span></a><span style=font-weight: 400;>Web: </span><a href=http://www.karolinabadz.com><span style=font-weight: 400;>www.karolinabadz.com</span></a></p>
      <p>&nbsp;</p>
      <p><strong>Luca Beisel</strong></p>
      <p><span style=font-weight: 400;>Graduate student of art history at Freie Universit&auml;t Berlin, student research assistant at MPI for the history of science Berlin</span></p>
        <p><span style=font-weight: 400;>Email: </span><a href=mailto:mail@lucabeisel.de><span style=font-weight: 400;>mail@lucabeisel.de</span></a><span style=font-weight: 400;> Web: www.lucabeisel.de</span></p>
          <p>&nbsp;</p>
          <p><strong>Jacquelyn Clements</strong><span style=font-weight: 400;> - CLIR/Mellon Postdoctoral Fellow in Data Curation for Visual Studies, University of Toronto</span></p>
            <p><span style=font-weight: 400;>Email: </span><a href=mailto:jacquelynhelene@gmail.com><span style=font-weight: 400;>jacquelynhelene@gmail.com</span></a><span style=font-weight: 400;> Twitter: </span><a href=https://twitter.com/peripatesis><span style=font-weight: 400;>@peripatesis </span></a><span style=font-weight: 400;>Web: </span><a href=http://www.jacquelynclements.com><span style=font-weight: 400;>www.jacquelynclements.com</span></a></p>
              <p>&nbsp;</p>
              <p><strong>Alex Kruse</strong></p>
              <p><span style=font-weight: 400;>Interested in Data Analysis and Visualization. Always searching for interesting Datasets. Hamburg</span></p>
                <p><span style=font-weight: 400;>Twitter: </span><a href=https://twitter.com/krusealex2013><span style=font-weight: 400;>@alexkruse2013</span></a><span style=font-weight: 400;> Github: </span><a href=https://github.com/kruse-alex><span style=font-weight: 400;>https://github.com/kruse-alex</span></a></p>
                  <p>&nbsp;</p>
                  <p><strong>Nils Windisch</strong></p>
                  <p><span style=font-weight: 400;>rock climbing all-things-web-frontend. happy husband and probably a coffee addict. G&ouml;ttingen, Germany</span></p>
                    <p><span style=font-weight: 400;>Email </span><a href=mailto:nils@windisch.me><span style=font-weight: 400;>nils@windisch.me</span></a><span style=font-weight: 400;> Twitter: </span><a href=https://twitter.com/nilswindisch><span style=font-weight: 400;>@nilswindisch</span></a><span style=font-weight: 400;> |&nbsp;Web: </span><a href=http://nilswindisch.de><span style=font-weight: 400;>http://nilswindisch.de</span></a></p>"))
          

)))))

#############################################################################################################################################
# server
#############################################################################################################################################

  server = function(input, output, session) {
    
    # download data
    output$downloadData <-  downloadHandler(
      filename = function() {
        paste("churches_-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(monster, file)
      }
    )
    
    # Tiles
    tcu_map = "https://api.mapbox.com/styles/v1/lucabeisel/cj0c6te5y007t2smqu0pk2ag1/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibHVjYWJlaXNlbCIsImEiOiJjajBiMDRldTYwMnM2MnFvM2w2YWN5em13In0.7rytREJjeRqTXBshlEEC5Q"
    
    map_attr = "<a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> Own Dutch Design"
    
    # plot basic map
    output$MapPlot1 <- renderLeaflet({
      leaflet() %>% 
        addTiles(urlTemplate = tcu_map, attribution = map_attr) %>% 
        setView(4.895168,	52.370216, 8)
    })
    
    # plot basic map
    output$MapPlot2 <- renderLeaflet({
      leaflet() %>% 
        addTiles(urlTemplate = tcu_map, attribution = map_attr) %>% 
        setView(4.895168,	52.370216, 8)
    })
    
#############################################################################################################################################
# dynamic marker 1
#############################################################################################################################################
    
    observe({
      
      # filter dataset
      monster_dynamic = monster %>%
        filter(monster$painting_year >= input$time[1] & monster$painting_year <= input$time[2] & 
               monster$artist_name %in% input$artist)
      
      church_dynamic = monster %>%
        filter(monster$painting_year >= input$time[1] & monster$painting_year <= input$time[2] & 
                 monster$artist_name %in% input$artist)
      
      church_dynamic = church_dynamic %>% group_by(church) %>% summarise(count = n(),
                                                                                 church_long = church_long[1],
                                                                                 church_lat = church_lat[1],
                                                                                 artist_name = list(as.character(unique(artist_name))))
      church_dynamic$artist_name = gsub("c\\(|)|\"|([\n])","", church_dynamic$artist_name)
      
      transfer_dynamic = transfer %>%
        filter(transfer$date_until >= input$time[1] &
                 transfer$artist_name %in% input$artist)
      
      # create pop ups
      artist_popup = paste0("<strong>Residence: </strong>", 
                            transfer_dynamic$artist_name,
                            br(),"<strong>From: </strong>", 
                            transfer_dynamic$date_from,
                            br(),"<strong>Until: </strong>", 
                            transfer_dynamic$date_until)
      
      churches_popup = paste0("<strong>Church: </strong>",
                              monster_dynamic$church,
                              br(),"<strong>Patined by: </strong>", 
                              church_dynamic$artist_name)
                              
      # plot map
      if(nrow(monster_dynamic) > 0 & nrow(transfer_dynamic) > 0) {
      leafletProxy("MapPlot1") %>% 
          clearMarkers() %>% 
          clearShapes() %>%
          clearControls() %>%
          addCircleMarkers(lng = transfer_dynamic$long,lat = transfer_dynamic$lat, color = "green", popup = artist_popup, fillOpacity = 1, stroke = F) %>%
          addCircleMarkers(lng = church_dynamic$church_long,lat = church_dynamic$church_lat, color = "red", popup = churches_popup, fillOpacity = 1, stroke = F) %>%
          addLegend(position = 'bottomleft',colors = c("red","green"),labels = c("Churches","Painters Locations"),title = '',opacity = 100) %>%
          addLabelOnlyMarkers(data = church_dynamic,
                              lng=~church_long, lat=~church_lat,
                              label=~as.character(count),
                              labelOptions = labelOptions(noHide = T))
      }
      else{
        leafletProxy("MapPlot1") %>% 
          clearMarkers() %>%
          clearShapes()
      }
      
      # table
      tabledata = select(monster_dynamic, painting_name2, painting_year, church, artist_name)
      output$x4 = DT::renderDataTable(tabledata, server = TRUE, escape = FALSE)
      
    })
    
#############################################################################################################################################
# image gallery 1
#############################################################################################################################################
    
    observe({
      
      # filter dataset
      monster_dynamic = monster %>%
        filter(monster$painting_year >= input$time[1] & monster$painting_year <= input$time[2] & 
                 monster$artist_name %in% input$artist)
      
      # image gallery
      randomVals <- eventReactive(input$go,  {
        sample(length(monster_dynamic$painting_image_link), 1)
      })
      
      src = paste(monster_dynamic$painting_image_link[randomVals()])
      
      output$logo <- renderUI({
        img(src = src, width = 200, height = 200)})
      
      output$logo_text <- renderUI({
        paste0("Painting: ",monster_dynamic$painting_name,", Artist: ", monster_dynamic$artist_name, ", Year: ",monster_dynamic$painting_year)})
    })
    
#############################################################################################################################################
# UNCHECKER 1
#############################################################################################################################################
    
    observe({
      if (input$Uncheck > 0) {
        updateCheckboxGroupInput(session=session, inputId="artist", choices=sort(unique(monster$artist_name)), selected=NULL)
      }
    })
    
#############################################################################################################################################
# dynamic marker 2
#############################################################################################################################################
    
    observe({
      
      # filter dataset
      monster_dynamic2 = monster %>%
        filter(monster$painting_year >= input$time2[1] & monster$painting_year <= input$time2[2] & 
                 monster$church %in% input$church)
      
      church_dynamic2 = monster %>%
        filter(monster$painting_year >= input$time2[1] & monster$painting_year <= input$time2[2] & 
                 monster$church %in% input$church)
      
      church_dynamic2 = church_dynamic2 %>% group_by(church) %>% summarise(count = n(),
                                                                         church_long = church_long[1],
                                                                         church_lat = church_lat[1],
                                                                         artist_name = list(as.character(unique(artist_name))))
      church_dynamic2$artist_name = gsub("c\\(|)|\"|([\n])","", church_dynamic2$artist_name)
      
      
      
      transfer_dynamic2 = transfer %>%
        filter(transfer$date_until >= input$time2[1] &
                 transfer$artist_name %in% monster$artist_name[monster$church %in% input$church])
      
      # create pop ups
      artist_popup2 = paste0("<strong>Residence: </strong>", 
                            transfer_dynamic2$artist_name,
                            br(),"<strong>From: </strong>", 
                            transfer_dynamic2$date_from,
                            br(),"<strong>Until: </strong>", 
                            transfer_dynamic2$date_until)
      
      churches_popup2 = paste0("<strong>Church: </strong>",
                               monster_dynamic2$church,
                               br(),"<strong>Patined by: </strong>", 
                               church_dynamic2$artist_name)
      
      # plot map
      if(nrow(monster_dynamic2) > 0 & nrow(transfer_dynamic2) > 0) {
        leafletProxy("MapPlot2") %>% 
          clearMarkers() %>% 
          clearShapes() %>%
          clearControls() %>%
          addCircleMarkers(lng = transfer_dynamic2$long,lat = transfer_dynamic2$lat, color = "green", popup = artist_popup2, fillOpacity = 1, stroke = F) %>%
          addCircleMarkers(lng = church_dynamic2$church_long,lat = church_dynamic2$church_lat, color = "red", popup = churches_popup2, fillOpacity = 1, stroke = F) %>%
          addLegend(position = 'bottomleft',colors = c("red","green"),labels = c("Churches","Painters Locations"),title = '',opacity = 100) %>%
          addLabelOnlyMarkers(data = church_dynamic2,
                              lng=~church_long, lat=~church_lat,
                              label=~as.character(count),
                              labelOptions = labelOptions(noHide = T))
      }
      else{
        leafletProxy("MapPlot2") %>% 
          clearMarkers() %>%
          clearShapes()
      }
      
      # table
      tabledata2 = select(monster_dynamic2, painting_name2, painting_year, church, artist_name)
      output$x42 = DT::renderDataTable(tabledata2, server = TRUE, escape = FALSE)
      
    })
    
#############################################################################################################################################
# image gallery 2
#############################################################################################################################################
    
    observe({
      
      # filter dataset
      monster_dynamic2 = monster %>%
        filter(monster$painting_year >= input$time2[1] & monster$painting_year <= input$time2[2] & 
                 monster$church %in% input$church)
      
      # image gallery
      randomVals2 <- eventReactive(input$go2,  {
        sample(length(monster_dynamic2$painting_image_link), 1)
      })
      
      src2 = paste(monster_dynamic2$painting_image_link[randomVals2()])
      
      output$logo2 <- renderUI({
        img(src = src2, width = 200, height = 200)})
      
      output$logo_text2 <- renderUI({
        paste0("Painting: ",monster_dynamic2$painting_name,", Artist: ", monster_dynamic2$artist_name, ", Year: ",monster_dynamic2$painting_year)})
    })
    
#############################################################################################################################################
# UNCHECKER 2
#############################################################################################################################################
    
    observe({
      if (input$Uncheck2 > 0) {
        updateCheckboxGroupInput(session=session, inputId="artist2", choices=sort(unique(monster$artist_name)), selected=NULL)
      }
    })
    
    
    

#############################################################################################################################################
# CLOSE
#############################################################################################################################################

  }

shinyApp(ui, server)
