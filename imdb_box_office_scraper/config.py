"""
Configuration settings for IMDB Box Office Scraper
"""

# Scraping settings
DEFAULT_DELAY = 1.0  # Delay between requests in seconds
USE_SELENIUM = False  # Whether to use Selenium by default
MAX_RETRIES = 3  # Maximum number of retries for failed requests
TIMEOUT = 30  # Request timeout in seconds

# Export settings
DEFAULT_EXPORT_FORMAT = 'csv'  # Default export format: csv, json, excel
EXPORT_DIRECTORY = 'data'  # Directory to save exported files

# IMDB URLs
IMDB_BASE_URL = "https://www.imdb.com"
WEEKEND_BOX_OFFICE_URL = "/chart/boxoffice/"
TOP_GROSSING_WORLDWIDE_URL = "/chart/boxoffice-alltime-world/"
YEARLY_SEARCH_URL = "/search/title/?title_type=feature&year={year}&sort=boxoffice_gross_us,desc"

# Headers for requests
USER_AGENTS = [
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36',
    'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
]

# Logging settings
LOG_LEVEL = 'INFO'
LOG_FORMAT = '%(asctime)s - %(levelname)s - %(message)s'
LOG_FILE = 'imdb_scraper.log'

# Scheduling settings
DEFAULT_SCHEDULE_INTERVAL = 24  # hours