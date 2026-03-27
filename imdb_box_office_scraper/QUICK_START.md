# IMDB Box Office Scraper - Quick Start Guide

## 🚀 Ready to Use!

Your IMDB Box Office Scraper is fully configured and ready to go. Here's how to use it:

## 📦 What's Included

- **`imdb_scraper.py`** - Main scraper with interactive CLI
- **`example_usage.py`** - Code examples and demonstrations  
- **`test_scraper.py`** - Validation and testing suite
- **`config.py`** - Configuration settings
- **`setup.py`** - Automated setup script
- **`README.md`** - Complete documentation

## ⚡ Quick Commands

### 1. Interactive Mode (Recommended for Beginners)
```bash
source venv/bin/activate
python3 imdb_scraper.py
```

### 2. Run Examples
```bash
source venv/bin/activate  
python3 example_usage.py
```

### 3. Direct Usage in Code
```python
from imdb_scraper import IMDBBoxOfficeScraper

# Initialize scraper
scraper = IMDBBoxOfficeScraper(delay=1.0)

# Scrape current weekend box office
data = scraper.scrape_weekend_box_office()

# Export to CSV
scraper.export_to_csv(data, 'weekend_boxoffice.csv')
```

## 🎯 What You Can Scrape

1. **Current Weekend Box Office** - Top movies this weekend
2. **Yearly Box Office** - Top movies by year (2000-2024)  
3. **All-Time Top Grossing** - Highest grossing movies ever
4. **Custom Searches** - Flexible queries

## 📊 Export Formats

- CSV (Excel compatible)
- JSON (for APIs/databases)
- Excel (native .xlsx files)

## ⚙️ Key Features

- **Rate Limiting** - Respects IMDB servers (1 second delays)
- **Error Handling** - Robust error recovery
- **Progress Tracking** - Shows scraping progress
- **Logging** - Detailed logs in `imdb_scraper.log`
- **Selenium Support** - For JavaScript-heavy pages

## 🔧 Configuration

Edit `config.py` to customize:
- Scraping delays
- Export formats
- Rate limiting
- Logging levels

## 📚 Learning Path

1. **Start here**: Run `python3 imdb_scraper.py` for guided experience
2. **See examples**: Check `example_usage.py` for code patterns
3. **Read docs**: Full documentation in `README.md`
4. **Advanced**: Customize settings in `config.py`

## 🛡️ Important Notes

- **Legal Compliance**: Use responsibly and respect IMDB's terms
- **Rate Limiting**: Built-in delays prevent overloading servers
- **Error Handling**: Scraper gracefully handles failures
- **Data Quality**: Always verify scraped data

## 🆘 Need Help?

1. Check `imdb_scraper.log` for detailed error information
2. Run `python3 test_scraper.py` to verify setup
3. Review `README.md` for comprehensive documentation
4. Modify delays in `config.py` if experiencing issues

## 🎉 You're All Set!

Your scraper is production-ready with enterprise-grade features:
- Professional logging
- Multiple export formats  
- Comprehensive error handling
- Flexible configuration options

**Happy Scraping! 🎬📈**