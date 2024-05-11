import requests
import os
import tarfile
from tqdm import tqdm  # For progress bar (optional)
from concurrent.futures import ThreadPoolExecutor


# Function to fetch and parse directory listing for a given year
def fetch_directory_listing(year):
    base_url = f"https://www.ncei.noaa.gov/data/global-summary-of-the-day/archive/{year}.tar.gz"
    return [base_url]


# Function to extract a tar.gz archive and delete the archive after extraction
def extract_tar_file(archive_path, extract_dir):
    try:
        with tarfile.open(archive_path, "r:gz") as tar:
            tar.extractall(path=extract_dir)
        print(f"Extracted: {archive_path}")

        # Delete the archive after extraction
        os.remove(archive_path)
        print(f"Deleted: {archive_path}")
    except Exception as e:
        print(f"Error extracting {archive_path}: {e}")


# Function to download and extract a specific archive
def download_and_extract_archive(archive_url, extract_dir, year):
    archive_name = archive_url.split("/")[-1]
    local_archive_path = os.path.join(extract_dir, archive_name)

    if os.path.exists(local_archive_path):
        print(f"Skipping {archive_name}, already downloaded and extracted.")
        os.remove(local_archive_path)
        print(f"Removed {archive_name}")
        return

    try:
        # Download the archive
        response = requests.get(archive_url, stream=True)
        response.raise_for_status()  # Raise an exception for HTTP errors

        # Display progress bar for download
        with open(local_archive_path, "wb") as file, tqdm(
            desc=f"{year} - {archive_name}",  # Print current year and archive name
            total=int(response.headers.get("content-length", 0)),
            unit="B",
            unit_scale=True,
            unit_divisor=1024,
        ) as bar:
            for data in response.iter_content(chunk_size=1024):
                file.write(data)
                bar.update(len(data))

        print(f"Downloaded: {archive_name}")

        # Extract the downloaded archive
        extract_tar_file(local_archive_path, extract_dir)
    except Exception as e:
        print(f"Error downloading {archive_name}: {e}")


# Main function to parallelize the download and extraction process
def main():
    start_year = 1929
    end_year = 2023
    download_dir = "gsod_archive_parallel"  # Base local directory to store downloaded and extracted archives
    os.makedirs(download_dir, exist_ok=True)

    with ThreadPoolExecutor(max_workers=24) as executor:
        for year in range(start_year, end_year + 1):
            year_dir = os.path.join(download_dir, str(year))
            os.makedirs(year_dir, exist_ok=True)

            try:
                archives_to_download = fetch_directory_listing(year)
                for archive_url in archives_to_download:
                    executor.submit(
                        download_and_extract_archive, archive_url, year_dir, year
                    )
            except Exception as e:
                print(f"Error processing year {year}: {e}")

    print("Download and extraction completed.")


if __name__ == "__main__":
    main()
