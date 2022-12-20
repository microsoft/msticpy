def get_sentinel_queries_from_github(
    git_url: Optional[
        str
    ] = "https://github.com/Azure/Azure-Sentinel/archive/master.zip",
    outputdir: Optional[str] = None,
) -> bool:
    """
    Download Microsoft Sentinel Github archive and extract detection and hunting queries.

    Parameters
    ----------
    git_url : str, optional
        URL of the GIT Repository to be downloaded, by default "https://github.com/Azure/Azure-Sentinel/archive/master.zip"
    outputdir : str, optional
        Provide absolute path to the output folder to save downloaded archive (e.g. '/usr/home' or 'C:\downloads'),
        If no path provided, it will download to .msticpy dir under Azure-Sentinel directory.
    """

    if outputdir is None:
        outputdir = Path.joinpath(Path("~").expanduser(), ".msticpy", "Azure-Sentinel")

    try:
        with requests.get(git_url, stream=True) as response:
            response = requests.get(git_url, stream=True)
            total_size_in_bytes= int(response.headers.get('content-length', 0))
            block_size = 1024
            progress_bar = tqdm(desc="Downloading from Microsoft Sentinel Github" , initial= 0, unit='iB', unit_scale=True)
            response.raise_for_status()
            repo_zip = Path.joinpath(Path(outputdir),"Azure-Sentinel.zip")
            with open(repo_zip, 'wb') as file:
                for data in response.iter_content(chunk_size=10000):
                    progress_bar.update(len(data))
                    file.write(data)
            progress_bar.close()

            archive = zipfile.ZipFile(repo_zip, mode="r")

        # Only extract Detections and Hunting Queries Folder
        for file in archive.namelist():
            if file.startswith(
                (
                    "Azure-Sentinel-master/Detections/",
                    "Azure-Sentinel-master/Hunting Queries/",
                )
            ):
                archive.extract(file, path=outputdir)
        print("Downloaded and Extracted Files successfully")

    except HTTPError as http_err:
        warnings.warn(f"HTTP error occurred trying to download from Github: {http_err}")
