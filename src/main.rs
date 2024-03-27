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


struct Muse {
    driver: Option<WebDriver>,
}

impl Muse {
    pub async fn new() -> Muse {

        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

        let mut caps = DesiredCapabilities::firefox();
        caps.add_firefox_arg("-headless").unwrap();

        let driver = WebDriver::new("http://localhost:4444", caps).await.unwrap();
        Muse { driver: Some(driver) }
    }

    pub async fn quit_driver(&mut self) -> WebDriverResult<()> {
        // Attempt to quit the WebDriver session
        if let Some(driver) = self.driver.take() {
            driver.quit().await?;
        }

        Ok(())
    }

    // Perform a search operation using the stored WebDriver
    pub async fn search(&mut self, query: &str) -> WebDriverResult<Vec<VideoAttributes>> {
        // Ensure driver is initialized

        let formatted_query = query.replace(" ", "+");
        let search_url = format!("https://www.youtube.com/results?search_query={}", formatted_query);
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

        let driver = self.driver.as_ref().expect("Driver not initialized");
        driver.goto(search_url).await?;


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
        self.quit_driver().await;

        Ok(video_attrs)
    }

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

    pub async fn view(video_url: &str) {
        println!("Viewing '{}'", video_url);
    }
}



#[tokio::main]
async fn main() -> WebDriverResult<()> {


    let muse_app = MuseApp::parse();
    let mut muse = Muse::new().await;

    let output = match muse_app.command {
        Commands::Search { query } => {
            let video_attrs = muse.search(&query).await?;
            json!(video_attrs)
        },
        Commands::Download { id, directory } => {
            let saved_file_path = muse.download(&id, &directory).await;
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

    muse.quit_driver().await;

    // Serializes the output to a string of JSON and prints it.
    println!("{}", to_string_pretty(&output).expect("Failed to serialize output"));

    Ok(())
}
