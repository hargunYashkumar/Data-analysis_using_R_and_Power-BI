
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(heatmaply)
library(treemap)
library(caret)
library(dplyr)
library(httr)
library(tidyr)
library(RSQLite)
library(digest)
library(shinyjs)
library(bslib)
library(googleAuthR) # Add Google Auth library
library(waiter) # For loading animations
library(shinyWidgets) # For enhanced UI elements
library(shiny)
library(DBI)
library(dplyr)
library(RSQLite)

# Google OAuth Credentials
# google_client_id <- "663108994897-vtd667mntdbudcjh74hog59famrnesam.apps.googleusercontent.com"
# google_client_secret <- "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"
Sys.setenv(OPENAI_API_KEY = "-proj-U2O6hzCBojSlCIGP6kVfowgxP0KDEHGLTnIYSLXSzxRJt_ImJZDFMMeMsJexk2NVWKlThj6i46T3BlbkFJa8WoS9c_0MxsTavrQZuIr7xUezWAoSX9iYmMJPJ7ffvsTg5bFhYJcwoIBFaB13tbploo4RvhcA")

# Define OAuth Scopes
scopes <- c(
  "https://www.googleapis.com/auth/userinfo.profile",
  "https://www.googleapis.com/auth/userinfo.email"
)

# Database initialization for user tracking
con <- dbConnect(SQLite(), "user_tracking.db")
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS user_activity (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL,
    email TEXT NOT NULL,
    login_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    logout_time TIMESTAMP
  )
")

# Initialize database with Google auth support
init_db <- function() {
  con <- dbConnect(SQLite(), "user_auth.db")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password TEXT,
      email TEXT UNIQUE NOT NULL,
      google_id TEXT UNIQUE,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  
  
  dbDisconnect(con)
}

init_db()


# UI
ui <- fluidPage(
  useShinyjs(),
  use_waiter(),
  
  # Enhanced CSS
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap", rel="stylesheet"),
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
    tags$style(HTML("



    /* Info Panel Styling */
    .info-panel {
      background: linear-gradient(rgba(0, 0, 0, 0.05), rgba(0, 0, 0, 0.35)), 
                  url('https://raw.githubusercontent.com/hargunYashkumar/InsightFlow/main/x10.jpg');
      background-size: cover;
      background-position: center;
      padding: 50px 0;
      display: none;
    }
    .hero-title.h1{
    }

    .info-card {
      background: rgba(255, 255, 255, 0.95);
      border-radius: 15px;
      padding: 30px;
      margin-bottom: 20px;
      box-shadow: 0 8px 32px rgba(0,0,0,0.1);
      backdrop-filter: blur(10px);
    }

    .info-card h3 {
      color: #2193b0;
      margin-bottom: 20px;
      text-align: center;
    }
    .row {
    display: -webkit-flex;
    flex-wrap: wrap;
    margin-right: -15px;
    margin-left: -15px;
}
    .info-card h4 {
      color: #2c3e50;
      margin-top: 15px;
    }

    .info-content ul {
      padding-left: 20px;
    }

    .info-content li {
      margin-bottom: 10px;
    }
    /* Video Background Styling */
      .video-background {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        z-index: -1;
        overflow: hidden;
        opacity: 0.3;
      }

      .video-background video {
        min-width: 100%;
        min-height: 100%;
        width: auto;
        height: auto;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
      }

      /* Transparent Horizontal Navbar */
      .top-navbar {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        background: rgba(255, 255, 255, 0.1);
        backdrop-filter: blur(10px);
        z-index: 1000;
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 10px 20px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }

      .top-navbar-links {
        display: flex;
        gap: 20px;
      }

      .top-navbar a {
        color: white;
        text-decoration: none;
        transition: color 0.3s ease;
      }

      .top-navbar a:hover {
        color: #6dd5ed;
      }
      .team-section{
        height:100vh;
      }
      /* Sidebar Navigation Dots */
      .sidebar-nav-dots {
        position: fixed;
        right: 20px;
        top: 50%;
        transform: translateY(-50%);
        display: flex;
        flex-direction: column;
        gap: 15px;
        z-index: 1100;
      }

      .nav-dot {
        width: 12px;
        height: 12px;
        background: rgba(255,255,255,0.5);
        border-radius: 50%;
        cursor: pointer;
        transition: all 0.3s ease;
      }

      .nav-dot:hover, .nav-dot.active {
        background: #2193b0;
        transform: scale(1.2);
      }
     /* Google Auth Button Styling */
      .google-btn {
        width: 100%;
        margin-top: 15px;
        padding: 10px;
        border-radius: 25px;
        background-color: white;
        border: 1px solid #dadce0;
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 10px;
        cursor: pointer;
        transition: background-color 0.3s ease;
        font-family: 'Roboto', sans-serif;
      }
      /* General Styling */
      body {
        font-family: 'Poppins', sans-serif;
        background-color: #f5f7fa;
        margin: 0;
        padding: 0;
      }
      
      /* Hero Section */
      .hero-section {
      background: linear-gradient(rgba(0, 0, 0, 0.05), rgba(0, 0, 0, 0.35)),
                  url('https://raw.githubusercontent.com/hargunYashkumar/InsightFlow/main/x12.jpg');
      background-size: cover;
      background-position: center;
      height: 100vh;
      display: flex;
          margin: 0;
          padding: 0;
      align-items: center;
      justify-content: center;
      text-align: center;
      color: white;
      padding: 0 20px;
    }
      
      .hero-content {
        max-width: 800px;
      }
      
      .hero-title {
        font-size: 3.5em;
        margin-bottom: 20px;
        animation: fadeInDown 1s ease-out;
      }
      .google-btn:hover {
        background-color: #f8f9fa;
      }
      
      .google-btn img {
        width: 18px;
        height: 18px;
      }
      
      .hero-subtitle {
        font-size: 1.5em;
        margin-bottom: 30px;
        animation: fadeIn 1s ease-out 0.5s both;
      }
      
      /* Features Section */
      .features-section {
      padding: 80px 20px;
      height:100vh;
      background: linear-gradient(rgba(0, 0, 0, 0.05), rgba(0, 0, 0, 0.35)),
                  url('https://raw.githubusercontent.com/hargunYashkumar/InsightFlow/main/x9.jpg');
      background-size: cover;
      padding-top:17vh;
      background-position: center;
    }
      
      .feature-card {
        background: white;
        border-radius: 10px;
        padding: 30px;
        margin: 15px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        transition: transform 0.3s ease;
        animation: fadeInUp 1s ease-out;
      }
      
      .feature-card:hover {
        transform: translateY(-10px);
      }
      


      
      
      /* Team Section */
      .team-section {
      background: linear-gradient(rgba(0, 0, 0, 0.05), rgba(0, 0, 0, 0.35)),
                  url('https://raw.githubusercontent.com/hargunYashkumar/InsightFlow/main/x5.jpg');
      padding: 50px 50px;
      background-size: cover;
      height: 100vh;
          padding-top: 10vh;
      background-position: center;
    }
      .tab-1040-6{
        padding:15px;
      }
      .team-card {
        background: white;
        border-radius: 15px;
        padding: 30px;
        margin: 15px;
        text-align: center;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        transition: transform 0.3s ease;
      }
      
      .team-card:hover {
        transform: translateY(-5px);
      }
      
      .team-avatar {
        width: 150px;
        height: 150px;
        border-radius: 50%;
        margin: 0 auto 20px;
        object-fit: cover;
      }

      /* Features Section */
      .features-section1 {
      padding: 80px 20px;
      height:100vh;
      background: linear-gradient(rgba(0, 0, 0, 0.05), rgba(0, 0, 0, 0.35)),
                  url('https://raw.githubusercontent.com/hargunYashkumar/InsightFlow/main/x7.jpg');
      background-size: cover;
          padding-top: 20vh;
      background-position: center;
    }
      
      .feature-card1 {
        background: white;
        border-radius: 10px;
        padding: 30px;
        margin: 15px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        transition: transform 0.3s ease;
        animation: fadeInUp 1s ease-out;
      }
      
      .feature-card1:hover {
        transform: translateY(-10px);
      }
      .div#tab-9564-6 {
        padding: 16px;
      }
      
      /* Auth Panel Styling */
      .auth-panel {
        max-width: 400px;
        margin: 50px auto;
        padding: 30px;
        border-radius: 15px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        background: white;
        animation: fadeInUp 1s ease-out;
      }
      
      /* Enhanced Button Styling */
      .btn-primary {
        background: linear-gradient(45deg, #2193b0, #6dd5ed);
        border: none;
        padding: 12px 25px;
        border-radius: 25px;
        transition: all 0.3s ease;
      }
      
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 5px 15px rgba(0,0,0,0.2);
      }
      body h2 {
        color: white;
      }

      
      /* Animations */
      @keyframes fadeInUp {
        from {
          opacity: 0;
          transform: translateY(20px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      /* Enhanced Auth Panel */
        .auth-panel {
          background: rgba(255, 255, 255, 0.95);
          backdrop-filter: blur(10px);
          border: none;
          border-radius: 20px;
          box-shadow: 0 8px 32px rgba(0,0,0,0.1);
        }
      
      /* Smooth Navigation */
        .navbar {
          background: linear-gradient(90deg, #2c3e50, #3498db);
                                      border: none;
                                      padding: 1rem 2rem;
        }
        
        
        
       /* New navbar tab content margin and padding */
      .navbar:not(.fixed-bottom):not(.navbar-fixed-bottom):not(.navbar-fixed-bottom)+div>.tab-content>.tab-pane {
        --bslib-navbar-margin: 20px;
        margin-top: var(--bslib-navbar-margin);
        padding: 15px;
      }
  
      /* Container full width and zero padding */
      .container, .container-fluid, .container-xl, .container-lg, .container-md, .container-sm {
        width: 100%;
        padding-right: 0px;
        padding-left: 0px;
        margin-right: auto;
        margin-left: auto;
      }
      
            
      

      /* Seamless Feature Cards */
        .feature-card {
          background: linear-gradient(145deg, #ffffff, #f3f3f3);
                                      border: none;
                                      border-radius: 15px;
                                      padding: 2rem;
                                      margin: 1rem 0;
                                      transition: all 0.3s ease;
                                      box-shadow: 7px 2px 5px rgba(0,0,0,0.05), -3px -1px 19px rgb(255 255 255 / 91%);
        }

      .feature-card:hover {
        transform: translateY(-5px);
        box-shadow: 8px 8px 20px rgba(0,0,0,0.1),
        -8px -8px 20px rgba(255,255,255,0.9);
      }

      /* Enhanced Auth Panel */
        .auth-panel {
          background: rgba(255, 255, 255, 0.95);
          backdrop-filter: blur(10px);
          border: none;
          border-radius: 20px;
          box-shadow: 0 8px 32px rgba(0,0,0,0.1);
        }

      /* Smooth Navigation */
        .navbar {
          background: linear-gradient(90deg, #2c3e50, #3498db);
                                      border: none;
                                      padding: 1rem 2rem;
        }

      .navbar-nav .nav-link {
        color: white !important;
        transition: all 0.3s ease;
        padding: 0.5rem 1rem;
        margin: 0 0.25rem;
        border-radius: 5px;
      }

      .navbar-nav .nav-link:hover {
        background: rgba(255,255,255,0.1);
      }

      /* Enhanced Data Tables */
        .dataTables_wrapper {
          background: transparent;
          padding: 1.5rem !important;
          border-radius: 15px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.05);
        }

      .table {
        background: transparent;
        border-collapse: separate;
        border-spacing: 0 0.5rem;
      }

      .table thead th {
        background: #f8f9fa;
          border: none;
        padding: 1rem;
      }

      .table tbody td {
        background: white;
        border: none;
        padding: 1rem;
      }

      /* Enhanced Plot Containers */
        .plotly, .plot-container {
          background: transparent !important;
          border-radius: 15px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.05);
          padding: 1rem;
        }

      /* Custom Scrollbar */
        ::-webkit-scrollbar {
          width: 8px;
          height: 8px;
        }

      ::-webkit-scrollbar-track {
        background: #f1f1f1;
      }

      ::-webkit-scrollbar-thumb {
        background: #888;
          border-radius: 4px;
      }

      ::-webkit-scrollbar-thumb:hover {
        background: #555;
      }

      /* Enhanced Input Fields */
        .form-control {
          border: none;
          border-radius: 8px;
          padding: 0.75rem 1rem;
          background: #f8f9fa;
            box-shadow: inset 2px 2px 5px rgba(0,0,0,0.05);
          transition: all 0.3s ease;
        }

      .form-control:focus {
        box-shadow: inset 2px 2px 5px rgba(0,0,0,0.1);
        background: white;
      }

      /* Loading Animation Enhancement */
        .waiter-overlay {
          backdrop-filter: blur(5px);
          background: rgba(255,255,255,0.8) !important;
        }

      /* Team Section Enhancement */
        .team-card {
          background: linear-gradient(145deg, #ffffff, #f3f3f3);
                                      border: none;
                                      padding: 2rem;
                                      border-radius: 20px;
                                      box-shadow: 5px 5px 15px rgba(0,0,0,0.05),
                                      -5px -5px 15px rgba(255,255,255,0.8);
        }

      .team-avatar {
        border: 5px solid white;
        box-shadow: 0 5px 15px rgba(0,0,0,0.1);
      }
      .info-panel{
        height:100vh;
      }

      /* Enhanced Buttons */
        .btn {
          border: none;
          padding: 0.75rem 1.5rem;
          border-radius: 10px;
          transition: all 0.3s ease;
          text-transform: uppercase;
          letter-spacing: 1px;
          font-weight: 600;
          border-radius: 25px
        }

      .btn-primary {
        background: linear-gradient(45deg, #2193b0, #6dd5ed);
                                    box-shadow: 0 4px 15px rgba(33,147,176,0.3);
      }

      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(33,147,176,0.4);
      }


      /* Notification Enhancement */
        .shiny-notification {
          border-radius: 10px;
          border: none;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
          background: white;
          padding: 1rem;
        }

        .element {
          height: 100px;
          width: 100px;
          background-image: linear-gradient(rgba(0, 0, 0, 0.0), rgba(0, 0, 0, 0.0)), 
                            url(https://raw.githubusercontent.com/hargunYashkumar/InsightFlow/main/logo4.png);
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
          display: flex;
          justify-content: center;
          align-items: center;
          margin: 0 auto;
}
      
    "))
  ),
  
  # Replace the existing tags$script for navigation dots with this
  
  tags$script(HTML("
document.addEventListener('DOMContentLoaded', function() {
  // Get all navigation dots
  var navDots = document.querySelectorAll('.nav-dot');
  
  // Add click event listener to each dot
  navDots.forEach(function(dot) {
    dot.addEventListener('click', function() {
      // Get the target section ID from data-section attribute
      var sectionId = this.getAttribute('data-section');
      
      // Find the target section
      var section = document.getElementById(sectionId);
      
      // If section exists, scroll to it smoothly
      if (section) {
        section.scrollIntoView({
          behavior: 'smooth',
          block: 'start'
        });
        
        // Remove active class from all dots
        navDots.forEach(function(d) {
          d.classList.remove('active');
        });
        
        // Add active class to clicked dot
        this.classList.add('active');
      }
    });
  });
});
")),
  
  tags$script(HTML("
    $(document).ready(function() {
      // Initially hide the auth panel
      $('.auth-panel').hide();
      
      // Scroll and show auth panel when button is clicked
      $('#scroll_to_auth').click(function() {
        $('.auth-panel').show();
        $('html, body').animate({
          scrollTop: $('.auth-panel').offset().top
        }, 1000);
      });
    });
  ")),
  
  # Homepage/Auth UI
  div(
    id = "auth_page",
    class = "fade-in",
    
    
    # Video Background
    div(
      class = "video-background",
      tags$video(
        autoplay = NA, 
        loop = NA, 
        muted = NA,
        tags$source(
          src = "https://www.example.com/background-video.mp4", 
          type = "video/mp4"
        )
      )
    ),
    
    # Add this to your top navbar
    div(
      class = "top-navbar",
      div(
        class = "top-navbar-links",
        tags$a(href = "#", id = "login-nav-btn", "Login"),
        tags$a(href = "#", id = "info-panel-btn", "About")
      )
    ),
    
    # Add a new section for the information panel
    div(
      id = "info-panel",
      class = "info-panel",
      div(
        class = "container",
        div(
          class = "row",
          div(
            class = "col-md-6",
            div(
              class = "info-card",
              h3("Copyrights & Licenses"),
              tags$hr(),
              div(
                class = "info-content",
                HTML("
              <h4>Copyrights</h4>
              <p>© 2024 InsightFlow Dashboard. All Rights Reserved.</p>
              
              <h4>Software Licenses</h4>
              <ul>
                <li><strong>Dashboard Software:</strong> MIT License</li>
                <li><strong>Data Visualization:</strong> Apache 2.0 License</li>
                <li><strong>Machine Learning Components:</strong> Open-Source</li>
              </ul>
              
              <h4>Usage Terms</h4>
              <p>Free for non-commercial use. Commercial licensing available upon request.</p>
            ")
              )
            )
          ),
          div(
            class = "col-md-6",
            div(
              class = "info-card",
              h3("Tools & Technologies"),
              tags$hr(),
              div(
                class = "info-content",
                HTML("
              <h4>Core Technologies</h4>
              <ul>
                <li><strong>Frontend:</strong> R Shiny, HTML5, CSS3</li>
                <li><strong>Backend:</strong> R, Python</li>
                <li><strong>Data Processing:</strong> 
                  <ul>
                    <li>Pandas</li>
                    <li>Tidyverse</li>
                  </ul>
                </li>
                <li><strong>Machine Learning:</strong> 
                  <ul>
                    <li>Scikit-learn</li>
                    <li>TensorFlow</li>
                  </ul>
                </li>
                <li><strong>Visualization:</strong> 
                  <ul>
                    <li>Plotly</li>
                    <li>ggplot2</li>
                  </ul>
                </li>
              </ul>
              
              <h4>Infrastructure</h4>
              <p>Cloud-enabled, scalable architecture supporting advanced analytics.</p>
            ")
              )
            )
          )
        )
      )
    ),
    
    # Add JavaScript to toggle info panel
    tags$script(HTML("
document.addEventListener('DOMContentLoaded', function() {
  var infoPanel = document.getElementById('info-panel');
  var infoPanelBtn = document.getElementById('info-panel-btn');
  var loginNavBtn = document.getElementById('login-nav-btn');
  
  // Initially hide the info panel
  infoPanel.style.display = 'none';
  
  // Toggle info panel
  infoPanelBtn.addEventListener('click', function() {
    if (infoPanel.style.display === 'none') {
      infoPanel.style.display = 'block';
      loginNavBtn.closest('.auth-panel').style.display = 'none';
    } else {
      infoPanel.style.display = 'none';
    }
  });
  
  // Login button functionality
  loginNavBtn.addEventListener('click', function() {
    infoPanel.style.display = 'none';
    var authPanel = document.querySelector('.auth-panel');
    if (authPanel) {
      authPanel.style.display = 'block';
      authPanel.scrollIntoView({
        behavior: 'smooth',
        block: 'center'
      });
    }
  });
});
")),
    
    # Sidebar Navigation Dots
    div(
      class = "sidebar-nav-dots",
      div(class = "nav-dot", `data-section` = "hero"),
      div(class = "nav-dot", `data-section` = "features"),
      div(class = "nav-dot", `data-section` = "team"),
      div(class = "nav-dot", `data-section` = "how-it-works")
    ),
    
    # Hero Section
    div(
      id = "hero",
      class = "hero-section",
      div(
        class = "hero-content",
        h1("Welcome to InsightFlow Dashboard", class = "hero-title"),
        
        p("Transforming Data into Actionable Insights", class = "hero-subtitle"),
        actionButton("scroll_to_auth", "Get Started", 
                     class = "btn btn-primary btn-lg animate__animated animate__pulse animate__infinite")
      )
    ),
    
    # Features Section
    div(
      id = "features", 
      class = "features-section",
      h2("What We Provide", class = "text-center mb-5"),
      div(
        class = "row",
        div(
          class = "col-md-4",
          div(
            id='z9',
            class = "feature-card",
            tags$i(class = "fas fa-chart-line fa-3x mb-4"),
            h4("Advanced Analytics"),
            p("Comprehensive data analysis tools with interactive visualizations and real-time processing capabilities.")
          )
        ),
        div(
          class = "col-md-4",
          div(
            id='z10',
            class = "feature-card",
            tags$i(class = "fas fa-brain fa-3x mb-4"),
            h4("Machine Learning"),
            p("Predictive analytics powered by sophisticated machine learning algorithms.")
          )
        ),
        div(
          class = "col-md-4",
          div(
            id='z3',
            class = "feature-card",
            tags$i(class = "fas fa-project-diagram fa-3x mb-4"),
            h4("3D Visualization"),
            p("Advanced 3D plotting capabilities for complex data representation.")
          )
        )
      )
    ),
    
    div(
      id = "team",
      class = "team-section",
      h2("Meet Our Team", class = "text-center mb-5"),
      div(
        class = "row",
        div(
          class = "col-md-6",
          div(
            id="z1",
            class = "team-card text-center p-4",
            img(src = "https://via.placeholder.com/150", class = "team-avatar rounded-circle mb-3"),
            h4("Gunjan Bhojwani", class = "mb-2"),
            p(class = "text-muted mb-2", "Lead Developer"),
            p(class = "mb-3",
              "Specializes in machine learning and data visualization. Passionate about creating intuitive user experiences. ",
              "Alumni of Swami Keshvanand Institute of Technology (SKIT)."
            ),
            div(
              class = "social-links d-flex justify-content-between px-5 mt-3",
              a(
                href = "#",
                class = "social-link text-decoration-none",
                img(src = "https://cdn.jsdelivr.net/npm/simple-icons@v5/icons/linkedin.svg", 
                    width = "24", 
                    height = "24", 
                    alt = "LinkedIn")
              ),
              a(
                href = "#",
                class = "social-link text-decoration-none",
                img(src = "https://cdn.jsdelivr.net/npm/simple-icons@v5/icons/github.svg", 
                    width = "24", 
                    height = "24", 
                    alt = "GitHub")
              ),
              a(
                href = "mailto:gunjanbhojwani22@example.com",
                class = "social-link text-decoration-none",
                img(src = "https://cdn.jsdelivr.net/npm/simple-icons@v5/icons/gmail.svg", 
                    width = "24", 
                    height = "24", 
                    alt = "Email")
              )
            )
          )
        ),
        div(
          class = "col-md-6",
          div(
            id="z2",
            class = "team-card text-center p-4",
            img(src = "https://via.placeholder.com/150", class = "team-avatar rounded-circle mb-3"),
            h4("Hargun YashKumar", class = "mb-2"),
            p(class = "text-muted mb-2", "Senior Developer"),
            p(class = "mb-3",
              "Expert in statistical analysis and backend development. Focuses on system architecture and performance optimization. ",
              "Alumni of Swami Keshvanand Institute of Technology (SKIT)."
            ),
            div(
              class = "social-links d-flex justify-content-between px-5 mt-3",
              a(
                href = "https://www.linkedin.com/in/hargun-yashkumar2004/",
                class = "social-link text-decoration-none",
                img(src = "https://cdn.jsdelivr.net/npm/simple-icons@v5/icons/linkedin.svg", 
                    width = "24", 
                    height = "24", 
                    alt = "LinkedIn")
              ),
              a(
                href = "https://github.com/hargunYashkumar/",
                class = "social-link text-decoration-none",
                img(src = "https://cdn.jsdelivr.net/npm/simple-icons@v5/icons/github.svg", 
                    width = "24", 
                    height = "24", 
                    alt = "GitHub")
              ),
              a(
                href = "mailto:hargunyashkumar@gmail.com",
                class = "social-link text-decoration-none",
                img(src = "https://cdn.jsdelivr.net/npm/simple-icons@v5/icons/gmail.svg", 
                    width = "24", 
                    height = "24", 
                    alt = "Email")
              )
            )
          )
        )
      )
    ),
    div(
      id = "how-it-works",
      class = "features-section1",
      h2("How It Works", class = "text-center mb-5"),
      div(
        class = "row g-4",
        div(
          id = 'z3',
          class = "col-md-3",
          div(
            class = "feature-card1 text-center p-4 h-100 d-flex flex-column align-items-center",
            div(
              class = "icon-wrapper mb-4",
              img(src = "https://img.icons8.com/fluency/48/upload.png",
                  width = "48",
                  height = "48",
                  alt = "Upload")
            ),
            h4("1. Upload Data", class = "mb-3"),
            p("Simply upload your CSV file containing sales data."),
            div(class = "features-list text-start mt-3",
                p(class = "mb-2", "• Support for CSV files"),
                p(class = "mb-2", "• Easy file upload"),
                p(class = "mb-0", "• Secure data handling")
            )
          )
        ),
        div(
          id = 'z4',
          class = "col-md-3",
          div(
            class = "feature-card1 text-center p-4 h-100 d-flex flex-column align-items-center",
            div(
              class = "icon-wrapper mb-4",
              img(src = "https://img.icons8.com/fluency/48/combo-chart.png",
                  width = "48",
                  height = "48",
                  alt = "Analysis")
            ),
            h4("2. Choose Analysis", class = "mb-3"),
            p("Select from various visualization and analysis options."),
            div(class = "features-list text-start mt-3",
                p(class = "mb-2", "• Interactive charts"),
                p(class = "mb-2", "• Custom filters"),
                p(class = "mb-0", "• Live preview")
            )
          )
        ),
        div(
          id = 'z5',
          class = "col-md-3",
          div(
            class = "feature-card1 text-center p-4 h-100 d-flex flex-column align-items-center",
            div(
              class = "icon-wrapper mb-4",
              img(src = "https://img.icons8.com/color/48/data-configuration.png",
                  width = "48",
                  height = "48",
                  alt = "Insights")
            ),
            h4("3. Generate Insights", class = "mb-3"),
            p("Get automatic insights and predictions from your data."),
            div(class = "features-list text-start mt-3",
                p(class = "mb-2", "• Data analysis"),
                p(class = "mb-2", "• Smart predictions"),
                p(class = "mb-0", "• Performance metrics")
            )
          )
        ),
        div(
          id = 'z6',
          class = "col-md-3",
          div(
            class = "feature-card1 text-center p-4 h-100 d-flex flex-column align-items-center",
            div(
              class = "icon-wrapper mb-4",
              img(src = "https://img.icons8.com/fluency/48/download.png",
                  width = "48",
                  height = "48",
                  alt = "Export")
            ),
            h4("4. Export Results", class = "mb-3"),
            p("Download reports and visualizations for your use."),
            div(class = "features-list text-start mt-3",
                p(class = "mb-2", "• PDF reports"),
                p(class = "mb-2", "• Excel exports"),
                p(class = "mb-0", "• One-click sharing")
            )
          )
        )
      )
    )
    ,
    # Auth Panel
    div(
      class = "auth-panel",
      tabsetPanel(
        id = "auth_tabs",
        tabPanel(
          "Login",
          div(
            style = "padding: 20px 0;",
            textInput("login_username", "Username", 
                      placeholder = "Enter your username"),
            passwordInput("login_password", "Password",
                          placeholder = "Enter your password"),
            actionButton("login_btn", "Login", 
                         class = "btn-primary", 
                         style = "width: 100%; margin-top: 15px;"),
            div(
              style = "text-align: center; margin-top: 15px;",
              "OR"
            ),
            actionButton(
              inputId = "google_login_btn",
              class = "google-btn",
              icon = icon("google"),
              label = "Login with Google"
            )
          )
        ),
        tabPanel(
          "Register",
          div(
            style = "padding: 20px 0;",
            textInput("register_username", "Username",
                      placeholder = "Choose a username"),
            textInput("register_email", "Email",
                      placeholder = "Enter your email"),
            passwordInput("register_password", "Password",
                          placeholder = "Choose a password"),
            passwordInput("register_password_confirm", "Confirm Password",
                          placeholder = "Confirm your password"),
            actionButton("register_btn", "Register", 
                         class = "btn-primary",
                         style = "width: 100%; margin-top: 15px;")
          )
        )
      )
    )
  ),
  
  # Main Dashboard UI (remains the same)
  shinyjs::hidden(
    div(
      id = "main_app",
      class = "fade-in",
      
      
      # Enhanced Dashboard UI
      navbarPage(
        " Analytics Dashboard",
        theme = bs_theme(
          version = 4,
          bootswatch = "flatly",
          primary = "#2c3e50",
          secondary = "#95a5a6"
        ),
        # Main Dashboard
        tabPanel("Dashboard",
                 fluidRow(
                   column(12,
                          wellPanel(
                            fileInput("file", "Upload CSV File"),
                            DTOutput("data_table"),
                            hr(),
                            h3("Summary"),
                            textOutput("summary_text")
                          )
                   )
                 )
        ),
        # Basic Graphs
        tabPanel("Basic Graphs",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("basic_graph_type", "Select Graph Type",
                                 choices = c("Bar Graph", "Box Plot", "Scatter Plot", "Line Graph"),
                                 selected = "Bar Graph"
                     ),
                     selectInput("basic_x_var", "Select X Variable", choices = ""),
                     selectInput("basic_y_var", "Select Y Variable", choices = ""),
                     conditionalPanel(
                       condition = "input.basic_graph_type == 'Bar Graph' || input.basic_graph_type == 'Line Graph'",
                       selectInput("basic_agg_method", "Aggregation Method",
                                   choices = c("Sum", "Mean", "Count"),
                                   selected = "Sum"
                       )
                     )
                   ),
                   mainPanel(
                     plotlyOutput("basic_plot", height = "600px")
                   )
                 )
        ),
        
        # Advanced Visualization
        tabPanel("Advanced Visualization",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("adv_graph_type", "Select Graph Type",
                                 choices = c("Heatmap", "Treemap"),
                                 selected = "Heatmap"
                     ),
                     selectInput("adv_x_var", "Select X Variable", choices = ""),
                     selectInput("adv_y_var", "Select Y Variable", choices = ""),
                     selectInput("adv_value_var", "Select Value Variable", choices = "")
                   ),
                   mainPanel(
                     plotlyOutput("advanced_plot", height = "600px")
                   )
                 )
        ),
        
        # Machine Learning
        tabPanel("Machine Learning",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("ml_target", "Select Target Variable", choices = ""),
                     selectInput("ml_predictors", "Select Predictor Variables", choices = "", multiple = TRUE),
                     actionButton("run_ml", "Run Analysis", class = "btn-primary")
                   ),
                   mainPanel(
                     verbatimTextOutput("ml_summary"),
                     plotOutput("ml_plot", height = "400px")
                   )
                 )
        ),
        
        # 3D Visualization
        tabPanel("3D Visualization",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("three_d_graph_type", "Select 3D Graph Type",
                                 choices = c("3D Scatter", "3D Surface"),
                                 selected = "3D Scatter"
                     ),
                     selectInput("three_d_x_var", "Select X Variable", choices = ""),
                     selectInput("three_d_y_var", "Select Y Variable", choices = ""),
                     selectInput("three_d_z_var", "Select Z Variable", choices = ""),
                     conditionalPanel(
                       condition = "input.three_d_graph_type == '3D Scatter'",
                       selectInput("three_d_color_var", "Select Color Variable", choices = "")
                     )
                   ),
                   mainPanel(
                     plotlyOutput("three_d_plot", height = "600px")
                   )
                 )
        ),
        # 🔥 AI Assistant Tab
        tabPanel("AI Assistant",
                 sidebarLayout(
                   sidebarPanel(
                     textInput("user_prompt", 
                               "Ask AI a question about the dataset", 
                               placeholder = "e.g., 'Summarize the data' or 'List the column names'"),
                     actionButton("ask_ai", "Ask AI", class = "btn-primary")
                   ),
                   mainPanel(
                     h3("AI Assistant Response"),
                     verbatimTextOutput("ai_response"),
                     h4("Dataset Preview"),
                     tableOutput("data_preview")
                   )
                 )
        ),
        
        tabPanel(
          "Logout",
          div(
            class = "navbar-btn",
            actionButton("logout_btn", "Logout",
                         class = "btn-danger",
                         icon = icon("sign-out-alt"))
          )
        )
      )
      
    )
  )
)



# Server logic for Google Authentication


server <- function(input, output, session) {
  
  auth <- reactiveValues(
    logged_in = FALSE,
    user_id = NULL,
    username = NULL,
    token = NULL
  )
  
  # Reactive value to store user information
  user_data <- reactiveVal(NULL)
  
  # Validation Functions
  validate_email <- function(email) {
    email_regex <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
    return(grepl(email_regex, email))
  }
  
  validate_password <- function(password) {
    return(nchar(password) >= 7)
  }
  
  validate_username <- function(username) {
    # First character must be a letter, minimum 6 characters
    username_regex <- "^[a-zA-Z][a-zA-Z0-9]{5,}$"
    return(grepl(username_regex, username))
  }
  
  # Google Authentication Handler
  observeEvent(input$google_login_btn, {
    # Configure OAuth Endpoint
    google_oauth <- oauth_endpoint(
      authorize = "https://accounts.google.com/o/oauth2/auth",
      access = "https://accounts.google.com/o/oauth2/token"
    )
    
    # Create OAuth App
    myapp <- oauth_app(
      appname = "google", 
      key = google_client_id, 
      secret = google_client_secret
    )
    
    # Get OAuth Token
    goog_auth <- oauth2.0_token(
      endpoint = google_oauth, 
      app = myapp, 
      scope = scopes, 
      cache = FALSE
    )
    
    # Fetch User Information
    user_info <- GET(
      "https://www.googleapis.com/oauth2/v2/userinfo", 
      config(token = goog_auth)
    )
    
    # Parse User Info
    user_info_data <- content(user_info, "parsed")
    
    # Store User Data
    user_data(user_info_data)
    auth$logged_in<- TRUE;
    # Switch Panels
    shinyjs::hide("auth_page")
    shinyjs::show("main_app")
  })
  
  
  # Login Handler
  observeEvent(input$login_btn, {
    req(input$login_username, input$login_password)
    
    # Connect to database
    con <- dbConnect(SQLite(), "user_auth.db")
    
    # Check credentials
    user <- dbGetQuery(con, "
      SELECT id, username, password 
      FROM users 
      WHERE username = ?
    ", params = list(input$login_username))
    
    dbDisconnect(con)
    
    if (nrow(user) == 1 && user$password == digest(input$login_password, algo = "sha256")) {
      auth$logged_in <- TRUE
      auth$user_id <- user$id
      auth$username <- user$username
      
      # Hide login page and show main app
      shinyjs::hide("auth_page")
      shinyjs::show("main_app")
      
      # Clear login form
      updateTextInput(session, "login_username", value = "")
      updateTextInput(session, "login_password", value = "")
    } else {
      shinyjs::html("login_error", "Invalid username or password")
    }
  })
  
  # Register Handler
  observeEvent(input$register_btn, {
    req(input$register_username, input$register_email,
        input$register_password, input$register_password_confirm)
    
    # Validate inputs
    if (input$register_password != input$register_password_confirm) {
      shinyjs::html("register_error", "Passwords do not match")
      return()
    }
    
    # Additional Validations
    if (!validate_username(input$register_username)) {
      shinyjs::html("register_error", "Username must start with a letter and be at least 6 characters long")
      return()
    }
    
    if (!validate_email(input$register_email)) {
      shinyjs::html("register_error", "Please enter a valid email address")
      return()
    }
    
    if (!validate_password(input$register_password)) {
      shinyjs::html("register_error", "Password must be at least 7 characters long")
      return()
    }
    
    # Connect to database
    con <- dbConnect(SQLite(), "user_auth.db")
    
    # Check if username or email already exists
    existing <- dbGetQuery(con, "
      SELECT username, email 
      FROM users 
      WHERE username = ? OR email = ?
    ", params = list(input$register_username, input$register_email))
    
    if (nrow(existing) > 0) {
      shinyjs::html("register_error", "Username or email already exists")
      dbDisconnect(con)
      return()
    }
    
    # Insert new user
    tryCatch({
      dbExecute(con, "
        INSERT INTO users (username, email, password)
        VALUES (?, ?, ?)
      ", params = list(
        input$register_username,
        input$register_email,
        digest(input$register_password, algo = "sha256")
      ))
      
      # Clear registration form
      updateTextInput(session, "register_username", value = "")
      updateTextInput(session, "register_email", value = "")
      updateTextInput(session, "register_password", value = "")
      updateTextInput(session, "register_password_confirm", value = "")
      
      # Switch to login tab
      updateTabsetPanel(session, "auth_tabs", selected = "Login")
      
    }, error = function(e) {
      shinyjs::html("register_error", "Registration failed. Please try again.")
    })
    
    dbDisconnect(con)
  })
  
  # Logout Handler
  observeEvent(input$logout_btn, {
    auth$logged_in <- FALSE
    auth$user_id <- NULL
    auth$username <- NULL
    
    # Hide main app and show login page
    shinyjs::hide("main_app")
    shinyjs::show("auth_page")
  })
  
  # Original server logic - wrap in observe block to ensure user is logged in
  observe({
    req(auth$logged_in)
    
    # Data handling
    data <- reactive({
      req(input$file)
      df <- read.csv(input$file$datapath)
      df[] <- lapply(df, function(x) if(is.character(x)) as.factor(x) else x)
      return(df)
    })
    
    # Update dropdown choices when data is loaded
    observe({
      req(data())
      choices <- names(data())
      numeric_choices <- names(which(sapply(data(), is.numeric)))
      
      updateSelectInput(session, "basic_x_var", choices = c("", choices))
      updateSelectInput(session, "basic_y_var", choices = c("", numeric_choices))
      updateSelectInput(session, "adv_x_var", choices = c("", choices))
      updateSelectInput(session, "adv_y_var", choices = c("", choices))
      updateSelectInput(session, "adv_value_var", choices = c("", numeric_choices))
      updateSelectInput(session, "ml_target", choices = c("", numeric_choices))
      updateSelectInput(session, "three_d_x_var", choices = c("", numeric_choices))
      updateSelectInput(session, "three_d_y_var", choices = c("", numeric_choices))
      updateSelectInput(session, "three_d_z_var", choices = c("", numeric_choices))
      updateSelectInput(session, "three_d_color_var", choices = c("", choices))
    })
    
    # Original outputs
    output$data_table <- renderDT({
      req(data())
      datatable(data(), options = list(pageLength = 5, scrollX = TRUE))
    })
    
    output$summary_text <- renderText({
      req(data())
      paste("Total rows:", nrow(data()), 
            "| Total columns:", ncol(data()),
            "| Numeric columns:", sum(sapply(data(), is.numeric)))
    })
    
    # [Rest of your original server code remains the same]
    
    
    # Basic Plots
    output$basic_plot <- renderPlotly({
      if (input$basic_x_var == "" || input$basic_y_var == "") {
        return(plot_ly() %>% 
                 add_annotations(text = "Please select fields to continue",
                                 showarrow = FALSE,
                                 font = list(size = 20)))
      }
      
      req(data(), input$basic_x_var, input$basic_y_var)
      
      df <- data()
      
      if(input$basic_graph_type %in% c("Bar Graph", "Line Graph")) {
        df <- switch(input$basic_agg_method,
                     "Sum" = df %>% group_by_at(input$basic_x_var) %>% 
                       summarise(y = sum(get(input$basic_y_var)), .groups = 'drop'),
                     "Mean" = df %>% group_by_at(input$basic_x_var) %>% 
                       summarise(y = mean(get(input$basic_y_var)), .groups = 'drop'),
                     "Count" = df %>% group_by_at(input$basic_x_var) %>% 
                       summarise(y = n(), .groups = 'drop')
        )
        colnames(df)[2] <- input$basic_y_var
      }
      
      p <- switch(input$basic_graph_type,
                  "Bar Graph" = ggplot(df, aes_string(x = input$basic_x_var, y = input$basic_y_var)) +
                    geom_bar(stat = "identity", fill = "steelblue") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
                  
                  "Box Plot" = ggplot(df, aes_string(x = input$basic_x_var, y = input$basic_y_var)) +
                    geom_boxplot(fill = "steelblue") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
                  
                  "Scatter Plot" = ggplot(df, aes_string(x = input$basic_x_var, y = input$basic_y_var)) +
                    geom_point(color = "steelblue") +
                    theme_minimal(),
                  
                  "Line Graph" = ggplot(df, aes_string(x = input$basic_x_var, y = input$basic_y_var, group = 1)) +
                    geom_line(color = "steelblue") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
      
      ggplotly(p)
    })
    
    # Advanced Plots
    output$advanced_plot <- renderPlotly({
      if (input$adv_x_var == "" || input$adv_y_var == "" || input$adv_value_var == "") {
        return(plot_ly() %>% 
                 add_annotations(text = "Please select fields to continue",
                                 showarrow = FALSE,
                                 font = list(size = 20)))
      }
      
      req(data(), input$adv_x_var, input$adv_y_var, input$adv_value_var)
      
      if (input$adv_graph_type == "Heatmap") {
        df_summary <- data() %>%
          group_by_at(vars(input$adv_x_var, input$adv_y_var)) %>%
          summarise(value = sum(get(input$adv_value_var)), .groups = 'drop') %>%
          spread(key = !!sym(input$adv_y_var), value = value)
        
        matrix_data <- as.matrix(df_summary[,-1])
        rownames(matrix_data) <- df_summary[[input$adv_x_var]]
        
        heatmaply(matrix_data,
                  main = "Heatmap Analysis",
                  xlab = input$adv_y_var,
                  ylab = input$adv_x_var)
      } else {
        df_summary <- data() %>%
          group_by_at(vars(input$adv_x_var, input$adv_y_var)) %>%
          summarise(value = sum(get(input$adv_value_var)), .groups = 'drop')
        
        plot_ly(
          type = "treemap",
          labels = paste(df_summary[[input$adv_x_var]], df_summary[[input$adv_y_var]]),
          parents = rep("", nrow(df_summary)),
          values = df_summary$value
        )
      }
    })
    
    # ML-related server logic
    observeEvent(input$ml_target, {
      req(data(), input$ml_target)
      numeric_cols <- names(which(sapply(data(), is.numeric)))
      predictor_choices <- setdiff(numeric_cols, input$ml_target)
      updateSelectInput(session, "ml_predictors",
                        choices = predictor_choices,
                        selected = predictor_choices[1:min(3, length(predictor_choices))])
    })
    
    # Improved ML model with validation and diagnostics
    ml_model <- eventReactive(input$run_ml, {
      req(data(), input$ml_target, length(input$ml_predictors) > 0)
      
      # Prepare data
      model_data <- data() %>%
        select(c(input$ml_target, input$ml_predictors)) %>%
        na.omit()
      
      # Split data into training and testing sets
      set.seed(123)
      train_index <- createDataPartition(model_data[[input$ml_target]], p = 0.8, list = FALSE)
      train_data <- model_data[train_index, ]
      test_data <- model_data[-train_index, ]
      
      # Fit model
      formula_str <- paste(input$ml_target, "~", paste(input$ml_predictors, collapse = " + "))
      model <- lm(as.formula(formula_str), data = train_data)
      
      # Make predictions on test set
      predictions <- predict(model, newdata = test_data)
      
      # Calculate performance metrics
      rmse <- sqrt(mean((test_data[[input$ml_target]] - predictions)^2))
      r2 <- cor(test_data[[input$ml_target]], predictions)^2
      mae <- mean(abs(test_data[[input$ml_target]] - predictions))
      
      # Return list with model and metrics
      list(
        model = model,
        metrics = list(
          rmse = rmse,
          r2 = r2,
          mae = mae
        ),
        test_actual = test_data[[input$ml_target]],
        test_predicted = predictions
      )
    })
    
    # Render ML results
    output$ml_summary <- renderPrint({
      req(ml_model())
      
      # Get model results
      model_results <- ml_model()
      model <- model_results$model
      metrics <- model_results$metrics
      
      # Create summary
      cat("=== MODEL SUMMARY ===\n\n")
      print(summary(model))
      
      cat("\n=== MODEL PERFORMANCE ===\n")
      cat(sprintf("R-squared (Test Set): %.3f\n", metrics$r2))
      cat(sprintf("RMSE: %.3f\n", metrics$rmse))
      cat(sprintf("MAE: %.3f\n", metrics$mae))
      
      cat("\n=== VARIABLE IMPORTANCE ===\n")
      importance <- abs(coef(model)[-1])  # Exclude intercept
      var_importance <- data.frame(
        Variable = names(importance),
        Importance = importance
      )
      var_importance <- var_importance[order(-var_importance$Importance), ]
      print(var_importance)
    })
    
    # Add diagnostic plots
    output$ml_plot <- renderPlot({
      req(ml_model())
      
      model_results <- ml_model()
      
      par(mfrow = c(2, 2))
      
      # Actual vs Predicted Plot
      plot(model_results$test_actual, model_results$test_predicted,
           xlab = "Actual Values",
           ylab = "Predicted Values",
           main = "Actual vs Predicted",
           pch = 16, col = "steelblue")
      abline(0, 1, col = "red", lty = 2)
      
      # Residuals vs Fitted
      plot(model_results$model, which = 1)
      
      # Normal Q-Q
      plot(model_results$model, which = 2)
      
      # Scale-Location
      plot(model_results$model, which = 3)
    })
    
    # 3D Plots
    output$three_d_plot <- renderPlotly({
      if (input$three_d_x_var == "" || input$three_d_y_var == "" || input$three_d_z_var == "") {
        return(plot_ly() %>% 
                 add_annotations(text = "Please select fields to continue",
                                 showarrow = FALSE,
                                 font = list(size = 20)))
      }
      
      req(data(), input$three_d_x_var, input$three_d_y_var, input$three_d_z_var)
      
      if (input$three_d_graph_type == "3D Scatter") {
        plot <- plot_ly(data(),
                        x = as.formula(paste0("~", input$three_d_x_var)),
                        y = as.formula(paste0("~", input$three_d_y_var)),
                        z = as.formula(paste0("~", input$three_d_z_var)),
                        type = "scatter3d",
                        mode = "markers"
        )
        
        if (!is.null(input$three_d_color_var) && input$three_d_color_var != "") {
          plot <- plot %>% add_trace(
            color = as.formula(paste0("~", input$three_d_color_var)),
            marker = list(size = 5)
          )
        }
        
        plot %>% layout(scene = list(
          xaxis = list(title = input$three_d_x_var),
          yaxis = list(title = input$three_d_y_var),
          zaxis = list(title = input$three_d_z_var)
        ))
      } else {
        df_summary <- data() %>%
          group_by_at(vars(input$three_d_x_var, input$three_d_y_var)) %>%
          summarise(z = sum(get(input$three_d_z_var)), .groups = 'drop') %>%
          spread(key = !!sym(input$three_d_y_var), value = z)
        
        matrix_data <- as.matrix(df_summary[,-1])
        
        plot_ly(z = ~matrix_data, type = "surface")
      }
    })
    
    observeEvent(input$ask_ai, {
      # 🛂 Ensure user has entered a prompt and uploaded a file
      req(input$user_prompt)  # User must enter a query
      req(input$file)  # Ensure a file is uploaded
      
      # 🗂️ Load the dataset
      data <- reactive({
        read.csv(input$file$datapath)
      })
      
      # 🗂️ Extract column names and preview the dataset
      dataset_preview <- paste0(
        "The dataset has the following columns: ",
        paste(names(data()), collapse = ", "),
        ". Here are the first few rows: \n",
        paste(capture.output(head(data())), collapse = "\n")
      )
      
      # 🧮 Generate summary statistics for numeric columns
      summary_stats <- data() %>%
        summarise(across(where(is.numeric),
                         list(mean = mean, median = median, min = min, max = max),
                         na.rm = TRUE))
      
      stats_summary <- paste(
        "Here are the summary statistics for numeric columns:\n",
        paste(capture.output(summary_stats), collapse = "\n")
      )
      
      # 🧠 Create the prompt for the AI
      prompt_text <- paste(
        "You are a data analysis assistant. Here is the context of the dataset:\n",
        dataset_preview,
        "\n\n",
        stats_summary,
        "\n\nUser query: ",
        input$user_prompt,
        "\n\nPlease provide a step-by-step, useful, and concise response."
      )
      
      # 🌐 Call the OpenAI API
      response <- tryCatch({
        openai::create_chat_completion(
          model = "gpt-3.5-turbo",
          messages = list(
            list(role = "system", content = "You are a helpful data analysis assistant."),
            list(role = "user", content = prompt_text)
          )
        )
      }, error = function(e) {
        # Create an error message if the API call fails
        NULL
      })
      
      # 📢 Process and display the AI response
      output$ai_response <- renderText({
        # Check if response is valid and contains data
        if (!is.null(response) && 
            is.data.frame(response$choices) && 
            "message.content" %in% names(response$choices)) {
          # Extract the assistant's response
          response$choices$message.content
        } else {
          # Return an error message if response is invalid
          "Error: Unable to retrieve a valid response from the AI."
        }
      })
    })
    
  })
}

# Run the application
shinyApp(ui = ui, server = server,options = list(port = 2000))