#!/usr/bin/env python3
"""
Example usage of the IMDB Box Office Scraper Agent
"""

from imdb_scraper import IMDBBoxOfficeScraper
from datetime import datetime
import os

def example_weekend_scraping():
    """Example: Scrape current weekend box office"""
    print("Example 1: Weekend Box Office Scraping")
    print("-" * 40)
    
    # Initialize scraper
    scraper = IMDBBoxOfficeScraper(delay=1.0, use_selenium=False)
    
    # Scrape weekend box office
    data = scraper.scrape_weekend_box_office()
    
    if data:
        print(f"Found {len(data)} movies in weekend box office")
        
        # Show first few results
        for i, movie in enumerate(data[:3]):
            print(f"{i+1}. {movie['title']} ({movie['year']}) - {movie['weekend_gross']}")
        
        # Export data
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        scraper.export_to_csv(data, f"weekend_box_office_{timestamp}.csv")
        print(f"Data exported to weekend_box_office_{timestamp}.csv")
    else:
        print("No data found")

def example_yearly_scraping():
    """Example: Scrape yearly box office data"""
    print("\nExample 2: Yearly Box Office Scraping")
    print("-" * 40)
    
    # Initialize scraper
    scraper = IMDBBoxOfficeScraper(delay=1.0, use_selenium=False)
    
    # Scrape 2023 box office
    year = 2023
    data = scraper.scrape_yearly_box_office(year)
    
    if data:
        print(f"Found {len(data)} movies from {year}")
        
        # Show top 3 grossing movies
        for i, movie in enumerate(data[:3]):
            print(f"{i+1}. {movie['title']} - {movie['gross']} (Rating: {movie['rating']})")
        
        # Export data
        scraper.export_to_json(data, f"yearly_box_office_{year}.json")
        print(f"Data exported to yearly_box_office_{year}.json")
    else:
        print("No data found")

def example_top_grossing():
    """Example: Scrape top grossing movies"""
    print("\nExample 3: Top Grossing Movies")
    print("-" * 40)
    
    # Initialize scraper
    scraper = IMDBBoxOfficeScraper(delay=1.0, use_selenium=False)
    
    # Scrape top 20 grossing movies
    data = scraper.scrape_top_movies_by_gross(limit=20)
    
    if data:
        print(f"Found {len(data)} top grossing movies")
        
        # Show top 5
        for movie in data[:5]:
            print(f"#{movie['rank']}. {movie['title']} ({movie['year']}) - {movie['worldwide_gross']}")
        
        # Export data
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        scraper.export_to_csv(data, f"top_grossing_movies_{timestamp}.csv")
        print(f"Data exported to top_grossing_movies_{timestamp}.csv")
    else:
        print("No data found")

def example_multiple_years():
    """Example: Scrape multiple years and combine data"""
    print("\nExample 4: Multiple Years Scraping")
    print("-" * 40)
    
    # Initialize scraper
    scraper = IMDBBoxOfficeScraper(delay=1.0, use_selenium=False)
    
    # Scrape multiple years
    years = [2021, 2022, 2023]
    all_data = []
    
    for year in years:
        print(f"Scraping {year}...")
        data = scraper.scrape_yearly_box_office(year)
        all_data.extend(data)
    
    if all_data:
        print(f"Total movies scraped: {len(all_data)}")
        
        # Group by year and show counts
        year_counts = {}
        for movie in all_data:
            year = movie['year']
            year_counts[year] = year_counts.get(year, 0) + 1
        
        print("Movies by year:")
        for year, count in sorted(year_counts.items()):
            print(f"  {year}: {count} movies")
        
        # Export combined data
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        scraper.export_to_csv(all_data, f"combined_box_office_{timestamp}.csv")
        print(f"Combined data exported to combined_box_office_{timestamp}.csv")

def main():
    """Run all examples"""
    print("IMDB Box Office Scraper - Usage Examples")
    print("=" * 50)
    
    # Create data directory if it doesn't exist
    os.makedirs('data', exist_ok=True)
    
    try:
        # Run examples
        example_weekend_scraping()
        example_yearly_scraping()
        example_top_grossing()
        example_multiple_years()
        
        print("\n" + "=" * 50)
        print("All examples completed successfully!")
        print("Check the generated CSV/JSON files for the scraped data.")
        
    except Exception as e:
        print(f"Error running examples: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()