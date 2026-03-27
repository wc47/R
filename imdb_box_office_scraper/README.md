# IMDB Box Office Scraper Agent

A comprehensive Python agent for scraping box office data from IMDB.com. This tool provides multiple scraping methods, robust error handling, rate limiting, and flexible export options.

## Features

- **Multiple Scraping Methods**:
  - Current weekend box office
  - Yearly box office data
  - Top grossing movies worldwide
  
- **Robust Architecture**:
  - Rate limiting to respect IMDB's servers
  - Error handling and retry mechanisms
  - Support for both requests and Selenium
  - Comprehensive logging
  
- **Export Options**:
  - CSV format
  - JSON format
  - Excel format
  
- **Advanced Features**:
  - Scheduled automatic scraping
  - Data validation and cleaning
  - Configurable settings
  - Progress tracking

## Installation

1. **Clone or download the scraper**:
   ```bash
   # If you have the files, navigate to the directory
   cd imdb_box_office_scraper
   ```

2. **Install dependencies**:
   ```bash
   pip install -r requirements.txt
   ```

3. **Install Chrome driver** (optional, for Selenium):
   ```bash
   # The scraper will automatically download the driver when needed
   # Or manually install chromedriver and add to PATH
   ```

## Quick Start

### Basic Usage

```python
from imdb_scraper import IMDBBoxOfficeScraper

# Initialize the scraper
scraper = IMDBBoxOfficeScraper(delay=1.0, use_selenium=False)

# Scrape current weekend box office
weekend_data = scraper.scrape_weekend_box_office()

# Export to CSV
scraper.export_to_csv(weekend_data, 'weekend_boxoffice.csv')
```

### Command Line Interface

Run the interactive CLI:

```bash
python imdb_scraper.py
```

### Run Examples

```bash
python example_usage.py
```

## Usage Examples

### 1. Weekend Box Office

```python
from imdb_scraper import IMDBBoxOfficeScraper

scraper = IMDBBoxOfficeScraper()
data = scraper.scrape_weekend_box_office()

# Data structure:
# [
#   {
#     'title': 'Movie Title',
#     'year': '2023',
#     'weekend_gross': '$50,000,000',
#     'imdb_url': 'https://www.imdb.com/title/...',
#     'scraped_date': '2023-12-07T10:30:00'
#   }
# ]
```

### 2. Yearly Box Office

```python
# Scrape 2023 box office data
data = scraper.scrape_yearly_box_office(2023)

# Data includes: title, year, rating, genre, gross, director, imdb_url
```

### 3. Top Grossing Movies

```python
# Get top 50 grossing movies worldwide
data = scraper.scrape_top_movies_by_gross(limit=50)

# Data includes: rank, title, year, worldwide_gross, imdb_url
```

### 4. Scheduled Scraping

```python
# Schedule automatic scraping every 24 hours
scraper.schedule_scraping(
    lambda: scraper.scrape_weekend_box_office(),
    interval_hours=24
)
```

### 5. Multiple Export Formats

```python
# Export to different formats
scraper.export_to_csv(data, 'boxoffice.csv')
scraper.export_to_json(data, 'boxoffice.json')
scraper.export_to_excel(data, 'boxoffice.xlsx')
```

## Configuration

Modify `config.py` to customize settings:

```python
# Scraping settings
DEFAULT_DELAY = 1.0  # Delay between requests
USE_SELENIUM = False  # Use Selenium for JS-heavy pages
MAX_RETRIES = 3      # Maximum retries for failed requests

# Export settings
DEFAULT_EXPORT_FORMAT = 'csv'
EXPORT_DIRECTORY = 'data'
```

## API Reference

### IMDBBoxOfficeScraper Class

#### Constructor
```python
IMDBBoxOfficeScraper(delay=1.0, use_selenium=False)
```

**Parameters:**
- `delay` (float): Delay between requests in seconds
- `use_selenium` (bool): Whether to use Selenium WebDriver

#### Methods

##### `scrape_weekend_box_office()`
Scrapes current weekend box office data.

**Returns:** List of dictionaries with movie data

##### `scrape_yearly_box_office(year)`
Scrapes yearly box office data for a specific year.

**Parameters:**
- `year` (int): Year to scrape data for

**Returns:** List of dictionaries with movie data

##### `scrape_top_movies_by_gross(limit=100)`
Scrapes top grossing movies worldwide.

**Parameters:**
- `limit` (int): Number of movies to scrape

**Returns:** List of dictionaries with movie data

##### Export Methods
- `export_to_csv(data, filename)`
- `export_to_json(data, filename)`
- `export_to_excel(data, filename)`

##### `schedule_scraping(scrape_function, interval_hours=24)`
Schedules automatic scraping.

**Parameters:**
- `scrape_function`: Function to call for scraping
- `interval_hours` (int): Hours between scraping sessions

## Data Structure

### Weekend Box Office
```json
{
  "title": "Movie Title",
  "year": "2023",
  "weekend_gross": "$25,000,000",
  "imdb_url": "https://www.imdb.com/title/tt1234567/",
  "scraped_date": "2023-12-07T10:30:00"
}
```

### Yearly Box Office
```json
{
  "title": "Movie Title",
  "year": 2023,
  "rating": "7.5",
  "genre": "Action, Adventure",
  "gross": "$100,000,000",
  "director": "Director Name",
  "imdb_url": "https://www.imdb.com/title/tt1234567/",
  "scraped_date": "2023-12-07T10:30:00"
}
```

### Top Grossing Movies
```json
{
  "rank": 1,
  "title": "Movie Title",
  "year": "2023",
  "worldwide_gross": "$2,000,000,000",
  "imdb_url": "https://www.imdb.com/title/tt1234567/",
  "scraped_date": "2023-12-07T10:30:00"
}
```

## Error Handling

The scraper includes comprehensive error handling:

- **Network errors**: Automatic retries with exponential backoff
- **Rate limiting**: Built-in delays between requests
- **Data validation**: Checks for missing or malformed data
- **Logging**: Detailed logs for debugging

## Best Practices

1. **Respect Rate Limits**: Use appropriate delays (1+ seconds)
2. **Handle Errors**: Always check if data was successfully scraped
3. **Monitor Logs**: Check `imdb_scraper.log` for issues
4. **Update Selectors**: IMDB may change their HTML structure
5. **Legal Compliance**: Ensure your usage complies with IMDB's terms of service

## Troubleshooting

### Common Issues

1. **No data returned**:
   - Check internet connection
   - Verify IMDB URLs are accessible
   - Try increasing delay between requests

2. **Selenium issues**:
   - Ensure Chrome/Chromium is installed
   - Check if chromedriver is compatible with your Chrome version

3. **Rate limiting**:
   - Increase delay between requests
   - Use random delays
   - Consider using proxy rotation

### Debug Mode

Enable debug logging:

```python
import logging
logging.getLogger().setLevel(logging.DEBUG)
```

## License

This project is for educational purposes. Please respect IMDB's terms of service and rate limits.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## Disclaimer

This tool is for educational and research purposes only. Users are responsible for complying with IMDB's terms of service and applicable laws. The authors are not responsible for any misuse of this tool.

## Support

For issues and questions:
1. Check the troubleshooting section
2. Review the logs in `imdb_scraper.log`
3. Open an issue with detailed error information