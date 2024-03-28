use tokio::process::Child;
use std::io::Write;
use regex::Regex;
use clap::{Command, Parser, Subcommand};
use serde::{Serialize, Deserialize};
use thirtyfour::prelude::*;
use tokio;
use reqwest;
use serde_json::json;
use serde_json::to_string_pretty;
use std::error::Error;
use reqwest::Url;
use tempfile::Builder;
use std::fs::File;
use std::io::copy;

fn sanitize_filename(filename: &str) -> String {
    let reserved_chars = ['/', '?', '<', '>', '\\', ':', '*', '|', '"'];
    filename
        .chars()
        .filter(|c| !reserved_chars.contains(c))
        .collect()
}


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
struct VideoListItem {
    title: String,
    id: String,
    url: String,
}

#[derive(Serialize, Deserialize, Debug)]
struct VideoPage {
    id: String,
    title: String,
    image: String,
    channel: String,
    url: String,
    sidebar: Vec<VideoListItem>
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
    pub async fn search(&mut self, query: &str) -> WebDriverResult<Vec<VideoListItem>> {
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

            video_attrs.push(VideoListItem { title, id, url });
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

    async fn download_image_to_tmp(&self, url: &str) -> Result<(String, tempfile::TempDir), Box<dyn Error>> {
        // Parse the URL and extract the file name
        let parsed_url = Url::parse(url)?;
        let path_segments = parsed_url.path_segments().ok_or("Cannot extract path segments")?;
        let file_name = sanitize_filename(path_segments.last().ok_or("Cannot extract file name")?);
        println!("got filename {}", file_name);

        // Create a temporary file where the image will be saved
        let dir = Builder::new().prefix("muse_image_download").tempdir()?;
        let temp_file_path = dir.path().join(file_name);
        println!("got tempdir");

        // Use reqwest to download the image data
        let response = reqwest::get(url).await?.error_for_status()?;
        let mut dest = File::create(temp_file_path.clone())?;
        println!("got request ");

        // Use `std::io::copy` to copy the image data to the file
        let content = response.bytes().await?;
        dest.write_all(&content)?;
        println!("saved file ");

        // Return the path to the saved image as a String
        Ok((temp_file_path.to_string_lossy().into_owned(), dir))
    }

    async fn get_ascii_from_image(&self, image_path: &str) -> Result<String, Box<dyn Error>> {
        // Replace `your_application` with the name of the actual application
        let output = std::process::Command::new("ascii-image-converter")
            .arg(image_path)
            .arg("-b")
            .arg("--dither")
            .args(&["-d", "30,13"])
            .arg("-c")
            .output()?;

        if !output.status.success() {
            let error_message = std::str::from_utf8(&output.stderr)?;
            panic!("ascii-image-converter failed with: {}", error_message)
        }
        println!("got output ");

        // Convert the captured stdout bytes to a UTF-8 encoded String
        let result_string = std::str::from_utf8(&output.stdout)?.to_string();
        println!("got ascii in function ");

        Ok(result_string)
    }

    pub async fn view(&self, video_id: &str) -> WebDriverResult<VideoPage> {
        let video_url = format!("https://www.youtube.com/watch?v={}", video_id);
        let driver = self.driver.as_ref().expect("Driver not initialized");

        driver.goto(video_url.clone()).await?;

        println!("got to video page");
        let title = driver.query(By::Css("h1.style-scope.ytd-watch-metadata"))
                          .first()
                          .await
                          .unwrap()
                          .wait_until()
                          .displayed()
                          .await
            ;
        let title = driver.find_element(
            By::Css(
                "h1.style-scope.ytd-watch-metadata > yt-formatted-string.style-scope.ytd-watch-metadata"
            )
        ).await.unwrap();
        let title_text = title.text().await.unwrap();
        println!("got title {}", title_text);

        // Wait to ensure the page has loaded. Adjust the delay as necessary.
        let channel = driver.query(By::Css("a.yt-simple-endpoint.style-scope.yt-formatted-string"))
                          .first()
                          .await
                          .unwrap()
                          .wait_until()
                          .displayed()
                          .await
            ;
        let channel = driver.find_element(
            By::Css(
                "a.yt-simple-endpoint.style-scope.yt-formatted-string"
            )
        ).await.unwrap();
        let channel_text = channel.text().await.unwrap();
        println!("got channel {}", channel_text);

        let thumbnail_image = driver.query(By::Css(".ytp-cued-thumbnail-overlay-image"))
                                    .first()
                                    .await
                                    .unwrap()
                                    .wait_until()
                                    .displayed()
                                    .await
            ;
        let thumbnail_style_attr = driver.find_element(
            By::Css(
                ".ytp-cued-thumbnail-overlay-image"
            )
        ).await.unwrap().attr("style").await.expect("Style attribute not found");
        println!("got thumbnail {}", thumbnail_style_attr.clone().expect("no attr").clone());


        let downloaded_file_path;
        let tempdir;
        // Using a regular expression to extract the URL from the style attribute
        let re = Regex::new(r#"url\("([^"]+)"\)"#).unwrap();
        if let Some(caps) = re.captures(&thumbnail_style_attr.clone().expect("no attr").clone()) {
            let url = &caps[1];
            (downloaded_file_path, tempdir) = self.download_image_to_tmp(url).await.unwrap();
        } else {
            (downloaded_file_path, tempdir) = self.download_image_to_tmp("https://upload.wikimedia.org/wikipedia/commons/4/4a/Youtube-.png").await.unwrap();
        };
        println!("filepath: {}", downloaded_file_path.clone());

        let ascii_art = self.get_ascii_from_image(&downloaded_file_path).await.unwrap();
        println!("got ascii:\n {}", ascii_art.clone());

        let video_elements = driver.find_all(By::Css("ytd-compact-video-renderer.style-scope.ytd-watch-next-secondary-results-renderer")).await?;
        let mut video_attrs = Vec::new();
        for video_element in video_elements {
            let title = video_element.find(By::Css("span#video-title")).await.unwrap().attr("title").await?.unwrap_or_default();
            let href = video_element.find(By::Css("a.yt-simple-endpoint.style-scope.ytd-compact-video-renderer")).await.unwrap().attr("href").await?.unwrap_or_default();
            let url = format!("https://www.youtube.com{}", href);
            println!("title: {}", title.clone());
            println!("href: {}", href.clone());


            let id = href.split("v=").nth(1)
                         .and_then(|v| v.split('&').next())
                         .unwrap_or_default()
                         .to_string();

            video_attrs.push(VideoListItem { title, id, url });
        }
        println!("got related videos");

        Ok(
            VideoPage {
                id: video_id.to_string(),
                title: title_text,
                channel: channel_text,
                image: ascii_art,
                url: video_url,
                sidebar: video_attrs
            }
        )

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
            let video_page = muse.view(&id).await.unwrap();
            json!(video_page)
        },
    };

    muse.quit_driver().await;

    // Serializes the output to a string of JSON and prints it.
    println!("{}", to_string_pretty(&output).expect("Failed to serialize output"));

    Ok(())
}
