use tokio::process::Child;
use clap::{Command, Parser, Subcommand};
use serde::{Serialize, Deserialize};
use thirtyfour::prelude::*;
use tokio;
use serde_json::json;
use serde_json::to_string_pretty;

/// Interacts with Muse
#[derive(Parser)]
#[clap(name = "Muse", about = "Manage Youtube music", version = "0", author = "someone")]
struct MuseApp {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Searches for a muse
    Search {
        /// The search query
        query: String,
    },
    /// Downloads a muse by id
    Download {
        /// The id of the muse to download
        id: String,
        directory: String,
    },
    /// Views a muse's detail by id
    View {
        /// The id of the muse to view
        id: String,
    },
}
// TODO https://github.com/nabijaczleweli/termimage

#[derive(Serialize, Deserialize, Debug)]
struct VideoAttributes {
    title: String,
    id: String,
    url: String,
}


struct WebDriverManager {
    driver_process: Option<Child>,
}

impl WebDriverManager {
    /// Starts the chromedriver process and returns an instance of WebDriverManager
    pub fn new() -> WebDriverResult<Self> {
        let driver_process = Some(tokio::process::Command::new("chromedriver")
            .arg("--port=4444")  // Specify the port chromedriver should listen on; match with WebDriver URL
            .spawn()
            .expect("Failed to start chromedriver. Is it installed and in PATH?"));

        Ok(WebDriverManager { driver_process })
    }

    /// Stops the chromedriver process
    pub async fn stop(&mut self) {
        if let Some(mut child) = self.driver_process.take() {
            child.kill().await.expect("Failed to kill chromedriver process");
        }
    }
}


struct Muse {
    driver: Option<WebDriver>,
    driver_process: Option<WebDriverManager>
}

impl Muse {
    pub async fn new() -> Muse {
        let web_driver_manager = WebDriverManager::new().unwrap();
        let mut caps = DesiredCapabilities::chrome();
        caps.add_chrome_arg("--headless").unwrap();
        caps.add_chrome_arg("--disable-gpu").unwrap();

        let driver = WebDriver::new("http://localhost:4444", caps).await.unwrap();
        Muse { driver: Some(driver), driver_process: Some(web_driver_manager) }
    }

    // Perform a search operation using the stored WebDriver
    pub async fn search(&self, query: &str) -> WebDriverResult<Vec<VideoAttributes>> {
        // Ensure driver is initialized
        let driver = self.driver.as_ref().expect("Driver not initialized");

        let formatted_query = query.replace(" ", "+");
        let search_url = format!("https://www.youtube.com/results?search_query={}", formatted_query);
        driver.get(search_url).await?;

        let video_elements = driver.find_all(By::Css("a#video-title")).await?;

        let mut video_attrs = Vec::new();

        for video_element in video_elements {
            let title = video_element.attr("title").await?.unwrap_or_default();
            let href = video_element.attr("href").await?.unwrap_or_default();
            let id = href.split("v=").nth(1)
                         .and_then(|v| v.split('&').next())
                         .unwrap_or_default()
                         .to_string();
            let url = format!("https://www.youtube.com/watch?v={}", id);

            video_attrs.push(VideoAttributes { title, id, url });
        }

        Ok(video_attrs)
    }

    // Stop the WebDriver
    pub async fn stop_webdriver(&mut self) -> WebDriverResult<()> {
        if let Some(mut driver) = self.driver_process.take() {
            driver.stop();
        }
        Ok(())
    }

    // Implement `download` and `view` similarly.


    pub async fn download(&self, video_id: &str, download_directory: &str) -> WebDriverResult<()> {
        // Construct the full YouTube video URL
        let video_url = format!("https://www.youtube.com/watch?v={}", video_id);

        // Define the output template, placing the downloaded file in the specified directory
        let output_template = format!("{}/%(title)s.%(ext)s", download_directory);

        // Execute the youtube-dl command with the desired options for audio extraction
        let status = std::process::Command::new("youtube-dl")
            .arg(video_url.as_str())
            .arg("--extract-audio")
            .arg("--audio-format")
            .arg("mp3")
            .arg("--audio-quality")
            .arg("0") // Best quality
            .arg("--embed-thumbnail")
            .arg("-o")
            .arg(output_template.as_str())
            .status()
            .expect("Failed to start youtube-dl process");

        if status.success() {
            Ok(())
        } else {
            panic!("{}", format!("youtube-dl failed with exit code: {:?}", status.code()))
        }
    }

    fn view(id: &str) {
        println!("Viewing '{}'", id);
        // Implement view logic here
    }
}



#[tokio::main]
async fn main() -> WebDriverResult<()> {


    let muse_app = MuseApp::parse();
    let mut muse = Muse::new().await;

    let output = match muse_app.command {
        Commands::Search { query } => {
            let video_attrs = muse.search(&query).await?;
            json!({
                "command": "search",
                "data": video_attrs
            })
        },
        Commands::Download { id, directory } => {
            let saved_file_path = muse.download(&id, &directory);
            json!({
                "id": id,
            })
        },
        Commands::View { id } => {
            json!({
                "command": "view",
                "message": "View functionality is under construction.",
                "id": id
            })
        },
    };

    muse.stop_webdriver().await.expect("Failed to stop WebDriver");

    // Serializes the output to a string of JSON and prints it.
    println!("{}", to_string_pretty(&output).expect("Failed to serialize output"));

    Ok(())
}
