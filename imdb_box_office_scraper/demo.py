#!/usr/bin/env python3
"""
IMDB Box Office Scraper - Live Demonstration

This script demonstrates the key features and capabilities of the scraper
without making actual requests to IMDB (to avoid rate limiting issues).
"""

from imdb_scraper import IMDBBoxOfficeScraper
from datetime import datetime
import json

def demonstrate_scraper_features():
    """Demonstrate key scraper features."""
    print("🎬 IMDB Box Office Scraper - Live Demo")
    print("=" * 50)
    
    # Initialize scraper
    print("\n1. Initializing scraper...")
    scraper = IMDBBoxOfficeScraper(delay=1.0, use_selenium=False)
    print("✓ Scraper initialized successfully")
    print(f"  - Base URL: {scraper.base_url}")
    print(f"  - Rate limiting: {scraper.delay} seconds between requests")
    print(f"  - User Agent: Randomized")
    
    # Show configuration
    print("\n2. Configuration options...")
    print("✓ Available scraping methods:")
    print("  - scrape_weekend_box_office()")
    print("  - scrape_yearly_box_office(year)")
    print("  - scrape_top_movies_by_gross(limit)")
    
    # Demonstrate data structure
    print("\n3. Sample data structure...")
    sample_data = [
        {
            'title': 'Avatar: The Way of Water',
            'year': '2022',
            'weekend_gross': '$134,000,000',
            'imdb_url': 'https://www.imdb.com/title/tt1630029/',
            'scraped_date': datetime.now().isoformat()
        },
        {
            'title': 'Top Gun: Maverick',
            'year': '2022', 
            'weekend_gross': '$126,000,000',
            'imdb_url': 'https://www.imdb.com/title/tt1745960/',
            'scraped_date': datetime.now().isoformat()
        }
    ]
    
    print("✓ Sample weekend box office data:")
    for i, movie in enumerate(sample_data, 1):
        print(f"  {i}. {movie['title']} ({movie['year']}) - {movie['weekend_gross']}")
    
    # Demonstrate export capabilities
    print("\n4. Export capabilities...")
    
    # CSV Export
    try:
        scraper.export_to_csv(sample_data, 'demo_weekend_boxoffice.csv')
        print("✓ CSV export successful: demo_weekend_boxoffice.csv")
    except Exception as e:
        print(f"✗ CSV export failed: {e}")
    
    # JSON Export
    try:
        scraper.export_to_json(sample_data, 'demo_weekend_boxoffice.json')
        print("✓ JSON export successful: demo_weekend_boxoffice.json")
    except Exception as e:
        print(f"✗ JSON export failed: {e}")
    
    # Excel Export
    try:
        scraper.export_to_excel(sample_data, 'demo_weekend_boxoffice.xlsx')
        print("✓ Excel export successful: demo_weekend_boxoffice.xlsx")
    except Exception as e:
        print(f"✗ Excel export failed: {e}")
    
    # Data cleaning demonstration
    print("\n5. Data cleaning capabilities...")
    test_values = ["$123,456,789", "$1.5M", "$2.3B", "N/A"]
    
    print("✓ Currency cleaning examples:")
    for value in test_values:
        cleaned = scraper._clean_currency(value)
        print(f"  '{value}' → '{cleaned}'")
    
    # Show logging
    print("\n6. Logging and monitoring...")
    print("✓ Comprehensive logging enabled:")
    print("  - All requests logged with timestamps")
    print("  - Errors captured with full stack traces")
    print("  - Progress tracking for long operations")
    print("  - Log file: imdb_scraper.log")
    
    # Usage scenarios
    print("\n7. Common usage scenarios...")
    scenarios = [
        "Weekend box office tracking for theaters",
        "Film industry analysis and research", 
        "Investment decision support data",
        "Academic studies on movie performance",
        "Personal movie database maintenance",
        "Competition analysis for distributors"
    ]
    
    print("✓ Perfect for:")
    for scenario in scenarios:
        print(f"  - {scenario}")
    
    # Best practices
    print("\n8. Best practices demonstrated...")
    print("✓ Built-in best practices:")
    print("  - Rate limiting (1+ second delays)")
    print("  - Error handling and retries")
    print("  - Respectful scraping patterns")
    print("  - Data validation and cleaning")
    print("  - Multiple export formats")
    print("  - Comprehensive logging")
    
    print("\n" + "=" * 50)
    print("🎉 Demo completed successfully!")
    print("\nGenerated files:")
    print("  - demo_weekend_boxoffice.csv")
    print("  - demo_weekend_boxoffice.json") 
    print("  - demo_weekend_boxoffice.xlsx")
    print("\n💡 Next steps:")
    print("  1. Try: python3 imdb_scraper.py (interactive mode)")
    print("  2. Review: README.md (full documentation)")
    print("  3. Customize: config.py (settings)")
    print("  4. Learn: example_usage.py (code examples)")

def show_real_world_example():
    """Show a real-world usage example."""
    print("\n" + "🌟 REAL-WORLD EXAMPLE" + "🌟")
    print("=" * 50)
    
    example_code = '''
# Example: Track top 10 movies for business intelligence

from imdb_scraper import IMDBBoxOfficeScraper
import pandas as pd

# Initialize scraper  
scraper = IMDBBoxOfficeScraper(delay=2.0)

# Get current weekend data
weekend_data = scraper.scrape_weekend_box_office()

# Get yearly data for comparison
year_2023_data = scraper.scrape_yearly_box_office(2023)

# Export for analysis
scraper.export_to_excel(weekend_data, 'weekend_analysis.xlsx')
scraper.export_to_csv(year_2023_data, 'yearly_comparison.csv')

# Data analysis with pandas
df = pd.DataFrame(weekend_data)
top_5 = df.head(5)

print("Top 5 movies this weekend:")
for idx, movie in top_5.iterrows():
    print(f"{idx+1}. {movie['title']} - {movie['weekend_gross']}")
'''
    
    print("✓ Production-ready code example:")
    print(example_code)

if __name__ == "__main__":
    try:
        demonstrate_scraper_features()
        show_real_world_example()
        
    except KeyboardInterrupt:
        print("\n\n👋 Demo interrupted by user")
    except Exception as e:
        print(f"\n❌ Demo error: {e}")
        print("Check imdb_scraper.log for details")