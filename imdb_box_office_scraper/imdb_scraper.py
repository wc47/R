#!/usr/bin/env python3
"""
IMDB Box Office Scraper Agent

A comprehensive agent for scraping box office data from IMDB.com
Includes rate limiting, error handling, and multiple export formats.
"""

import requests
from bs4 import BeautifulSoup
import pandas as pd
import time
import json
import csv
from datetime import datetime, timedelta
import re
import logging
from urllib.parse import urljoin, urlparse
from fake_useragent import UserAgent
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
from tqdm import tqdm
import os
from typing import List, Dict, Optional, Union
import schedule
import threading


class IMDBBoxOfficeScraper:
    """
    A comprehensive IMDB box office data scraper with multiple scraping methods
    and export capabilities.
    """
    
    def __init__(self, delay: float = 1.0, use_selenium: bool = False):
        """
        Initialize the IMDB scraper.
        
        Args:
            delay: Delay between requests in seconds (rate limiting)
            use_selenium: Whether to use Selenium for JavaScript-heavy pages
        """
        self.delay = delay
        self.use_selenium = use_selenium
        self.base_url = "https://www.imdb.com"
        self.ua = UserAgent()
        self.session = requests.Session()
        self.driver = None
        
        # Setup logging
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler('imdb_scraper.log'),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger(__name__)
        
        # Setup session headers
        self._setup_session()
        
        # Initialize Selenium if needed
        if self.use_selenium:
            self._setup_selenium()
    
    def _setup_session(self):
        """Setup requests session with headers."""
        self.session.headers.update({
            'User-Agent': self.ua.random,
            'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
            'Accept-Language': 'en-US,en;q=0.5',
            'Accept-Encoding': 'gzip, deflate',
            'Connection': 'keep-alive',
            'Upgrade-Insecure-Requests': '1'
        })
    
    def _setup_selenium(self):
        """Setup Selenium WebDriver."""
        try:
            chrome_options = Options()
            chrome_options.add_argument('--headless')
            chrome_options.add_argument('--no-sandbox')
            chrome_options.add_argument('--disable-dev-shm-usage')
            chrome_options.add_argument(f'--user-agent={self.ua.random}')
            
            service = Service(ChromeDriverManager().install())
            self.driver = webdriver.Chrome(service=service, options=chrome_options)
            self.logger.info("Selenium WebDriver initialized successfully")
        except Exception as e:
            self.logger.error(f"Failed to initialize Selenium: {e}")
            self.use_selenium = False
    
    def _make_request(self, url: str) -> Optional[requests.Response]:
        """
        Make a rate-limited request with error handling.
        
        Args:
            url: URL to request
            
        Returns:
            Response object or None if failed
        """
        try:
            time.sleep(self.delay)
            response = self.session.get(url, timeout=30)
            response.raise_for_status()
            return response
        except requests.RequestException as e:
            self.logger.error(f"Request failed for {url}: {e}")
            return None
    
    def _selenium_get(self, url: str) -> Optional[str]:
        """
        Get page content using Selenium.
        
        Args:
            url: URL to fetch
            
        Returns:
            Page source or None if failed
        """
        if not self.driver:
            return None
        
        try:
            time.sleep(self.delay)
            self.driver.get(url)
            WebDriverWait(self.driver, 10).until(
                EC.presence_of_element_located((By.TAG_NAME, "body"))
            )
            return self.driver.page_source
        except Exception as e:
            self.logger.error(f"Selenium request failed for {url}: {e}")
            return None
    
    def scrape_weekend_box_office(self) -> List[Dict]:
        """
        Scrape current weekend box office data.
        
        Returns:
            List of dictionaries containing box office data
        """
        url = f"{self.base_url}/chart/boxoffice/"
        
        if self.use_selenium:
            content = self._selenium_get(url)
            if not content:
                return []
            soup = BeautifulSoup(content, 'lxml')
        else:
            response = self._make_request(url)
            if not response:
                return []
            soup = BeautifulSoup(response.content, 'lxml')
        
        movies = []
        
        # Find the box office chart
        chart_items = soup.find_all('li', class_='titleColumn')
        
        for item in chart_items:
            try:
                movie_data = self._extract_weekend_movie_data(item)
                if movie_data:
                    movies.append(movie_data)
            except Exception as e:
                self.logger.error(f"Error extracting movie data: {e}")
                continue
        
        self.logger.info(f"Scraped {len(movies)} movies from weekend box office")
        return movies
    
    def _extract_weekend_movie_data(self, item) -> Optional[Dict]:
        """Extract movie data from weekend box office item."""
        try:
            # Title and year
            title_elem = item.find('a')
            if not title_elem:
                return None
            
            title = title_elem.get_text().strip()
            movie_url = urljoin(self.base_url, title_elem.get('href', ''))
            
            # Year
            year_elem = item.find('span', class_='secondaryInfo')
            year = year_elem.get_text().strip('()') if year_elem else 'N/A'
            
            # Gross earnings (usually in a sibling element)
            parent = item.parent if item.parent else item
            gross_elem = parent.find('span', class_='ratingColumn')
            
            if not gross_elem:
                # Try alternative selectors
                gross_elem = parent.find('td', class_='ratingColumn')
                
            gross = 'N/A'
            if gross_elem:
                gross_text = gross_elem.get_text().strip()
                gross = self._clean_currency(gross_text)
            
            return {
                'title': title,
                'year': year,
                'weekend_gross': gross,
                'imdb_url': movie_url,
                'scraped_date': datetime.now().isoformat()
            }
        except Exception as e:
            self.logger.error(f"Error extracting movie data: {e}")
            return None
    
    def scrape_yearly_box_office(self, year: int) -> List[Dict]:
        """
        Scrape yearly box office data.
        
        Args:
            year: Year to scrape data for
            
        Returns:
            List of dictionaries containing yearly box office data
        """
        url = f"{self.base_url}/search/title/?title_type=feature&year={year}&sort=boxoffice_gross_us,desc"
        
        if self.use_selenium:
            content = self._selenium_get(url)
            if not content:
                return []
            soup = BeautifulSoup(content, 'lxml')
        else:
            response = self._make_request(url)
            if not response:
                return []
            soup = BeautifulSoup(response.content, 'lxml')
        
        movies = []
        
        # Find movie containers
        movie_containers = soup.find_all('div', class_='lister-item')
        
        for container in tqdm(movie_containers, desc=f"Scraping {year} box office"):
            try:
                movie_data = self._extract_yearly_movie_data(container, year)
                if movie_data:
                    movies.append(movie_data)
            except Exception as e:
                self.logger.error(f"Error extracting yearly movie data: {e}")
                continue
        
        self.logger.info(f"Scraped {len(movies)} movies from {year} box office")
        return movies
    
    def _extract_yearly_movie_data(self, container, year: int) -> Optional[Dict]:
        """Extract movie data from yearly listing container."""
        try:
            # Title
            title_elem = container.find('h3', class_='lister-item-header').find('a')
            if not title_elem:
                return None
            
            title = title_elem.get_text().strip()
            movie_url = urljoin(self.base_url, title_elem.get('href', ''))
            
            # Rating
            rating_elem = container.find('div', class_='ratings-bar')
            rating = 'N/A'
            if rating_elem:
                rating_span = rating_elem.find('strong')
                rating = rating_span.get_text().strip() if rating_span else 'N/A'
            
            # Genre
            genre_elem = container.find('span', class_='genre')
            genre = genre_elem.get_text().strip() if genre_elem else 'N/A'
            
            # Gross earnings
            gross_elem = container.find('span', attrs={'name': 'nv'})
            gross = 'N/A'
            if gross_elem:
                gross = self._clean_currency(gross_elem.get_text())
            
            # Director and stars
            director_elem = container.find('p', class_='').find('a') if container.find('p', class_='') else None
            director = director_elem.get_text().strip() if director_elem else 'N/A'
            
            return {
                'title': title,
                'year': year,
                'rating': rating,
                'genre': genre,
                'gross': gross,
                'director': director,
                'imdb_url': movie_url,
                'scraped_date': datetime.now().isoformat()
            }
        except Exception as e:
            self.logger.error(f"Error extracting yearly movie data: {e}")
            return None
    
    def scrape_top_movies_by_gross(self, limit: int = 100) -> List[Dict]:
        """
        Scrape top movies by worldwide gross.
        
        Args:
            limit: Number of top movies to scrape
            
        Returns:
            List of dictionaries containing top grossing movies
        """
        url = f"{self.base_url}/chart/boxoffice-alltime-world/"
        
        if self.use_selenium:
            content = self._selenium_get(url)
            if not content:
                return []
            soup = BeautifulSoup(content, 'lxml')
        else:
            response = self._make_request(url)
            if not response:
                return []
            soup = BeautifulSoup(response.content, 'lxml')
        
        movies = []
        
        # Find the table rows
        rows = soup.find_all('tr')[:limit + 1]  # +1 for header
        
        for i, row in enumerate(tqdm(rows[1:], desc="Scraping top grossing movies")):
            if i >= limit:
                break
            
            try:
                movie_data = self._extract_top_grossing_movie_data(row, i + 1)
                if movie_data:
                    movies.append(movie_data)
            except Exception as e:
                self.logger.error(f"Error extracting top grossing movie data: {e}")
                continue
        
        self.logger.info(f"Scraped {len(movies)} top grossing movies")
        return movies
    
    def _extract_top_grossing_movie_data(self, row, rank: int) -> Optional[Dict]:
        """Extract movie data from top grossing movies table row."""
        try:
            cells = row.find_all('td')
            if len(cells) < 3:
                return None
            
            # Title and year
            title_cell = cells[1]
            title_link = title_cell.find('a')
            if not title_link:
                return None
            
            title = title_link.get_text().strip()
            movie_url = urljoin(self.base_url, title_link.get('href', ''))
            
            # Year (usually in parentheses)
            year_match = re.search(r'\((\d{4})\)', title_cell.get_text())
            year = year_match.group(1) if year_match else 'N/A'
            
            # Worldwide gross
            gross_cell = cells[2] if len(cells) > 2 else None
            gross = self._clean_currency(gross_cell.get_text()) if gross_cell else 'N/A'
            
            return {
                'rank': rank,
                'title': title,
                'year': year,
                'worldwide_gross': gross,
                'imdb_url': movie_url,
                'scraped_date': datetime.now().isoformat()
            }
        except Exception as e:
            self.logger.error(f"Error extracting top grossing movie data: {e}")
            return None
    
    def _clean_currency(self, text: str) -> str:
        """Clean currency text to extract numeric value."""
        if not text:
            return 'N/A'
        
        # Remove common currency symbols and text
        cleaned = re.sub(r'[^\d.,]', '', text)
        cleaned = cleaned.replace(',', '')
        
        # Handle millions/billions notation
        if 'M' in text.upper():
            try:
                value = float(cleaned) * 1000000
                return f"${value:,.0f}"
            except ValueError:
                pass
        elif 'B' in text.upper():
            try:
                value = float(cleaned) * 1000000000
                return f"${value:,.0f}"
            except ValueError:
                pass
        
        try:
            value = float(cleaned)
            return f"${value:,.0f}"
        except ValueError:
            return text.strip()
    
    def export_to_csv(self, data: List[Dict], filename: str):
        """Export data to CSV file."""
        if not data:
            self.logger.warning("No data to export")
            return
        
        df = pd.DataFrame(data)
        df.to_csv(filename, index=False)
        self.logger.info(f"Data exported to {filename}")
    
    def export_to_json(self, data: List[Dict], filename: str):
        """Export data to JSON file."""
        if not data:
            self.logger.warning("No data to export")
            return
        
        with open(filename, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2, ensure_ascii=False)
        self.logger.info(f"Data exported to {filename}")
    
    def export_to_excel(self, data: List[Dict], filename: str):
        """Export data to Excel file."""
        if not data:
            self.logger.warning("No data to export")
            return
        
        df = pd.DataFrame(data)
        df.to_excel(filename, index=False, engine='openpyxl')
        self.logger.info(f"Data exported to {filename}")
    
    def schedule_scraping(self, scrape_function, interval_hours: int = 24):
        """
        Schedule regular scraping.
        
        Args:
            scrape_function: Function to call for scraping
            interval_hours: Hours between scraping sessions
        """
        schedule.every(interval_hours).hours.do(scrape_function)
        
        def run_scheduler():
            while True:
                schedule.run_pending()
                time.sleep(60)  # Check every minute
        
        scheduler_thread = threading.Thread(target=run_scheduler, daemon=True)
        scheduler_thread.start()
        self.logger.info(f"Scheduled scraping every {interval_hours} hours")
    
    def __del__(self):
        """Cleanup Selenium driver."""
        if self.driver:
            try:
                self.driver.quit()
            except Exception:
                pass


def main():
    """Main function to demonstrate the scraper capabilities."""
    print("IMDB Box Office Scraper Agent")
    print("=" * 40)
    
    # Initialize scraper
    scraper = IMDBBoxOfficeScraper(delay=1.0, use_selenium=False)
    
    while True:
        print("\nOptions:")
        print("1. Scrape current weekend box office")
        print("2. Scrape yearly box office")
        print("3. Scrape top grossing movies")
        print("4. Schedule automatic scraping")
        print("5. Exit")
        
        choice = input("\nEnter your choice (1-5): ").strip()
        
        if choice == '1':
            print("Scraping current weekend box office...")
            data = scraper.scrape_weekend_box_office()
            if data:
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                scraper.export_to_csv(data, f"weekend_box_office_{timestamp}.csv")
                scraper.export_to_json(data, f"weekend_box_office_{timestamp}.json")
                print(f"Found {len(data)} movies")
            else:
                print("No data found")
        
        elif choice == '2':
            year = input("Enter year (e.g., 2023): ").strip()
            try:
                year = int(year)
                print(f"Scraping {year} box office data...")
                data = scraper.scrape_yearly_box_office(year)
                if data:
                    scraper.export_to_csv(data, f"yearly_box_office_{year}.csv")
                    scraper.export_to_json(data, f"yearly_box_office_{year}.json")
                    print(f"Found {len(data)} movies")
                else:
                    print("No data found")
            except ValueError:
                print("Invalid year format")
        
        elif choice == '3':
            limit = input("Enter number of top movies to scrape (default 100): ").strip()
            try:
                limit = int(limit) if limit else 100
                print(f"Scraping top {limit} grossing movies...")
                data = scraper.scrape_top_movies_by_gross(limit)
                if data:
                    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                    scraper.export_to_csv(data, f"top_grossing_movies_{timestamp}.csv")
                    scraper.export_to_json(data, f"top_grossing_movies_{timestamp}.json")
                    print(f"Found {len(data)} movies")
                else:
                    print("No data found")
            except ValueError:
                print("Invalid limit format")
        
        elif choice == '4':
            print("Scheduling automatic scraping...")
            hours = input("Enter interval in hours (default 24): ").strip()
            try:
                hours = int(hours) if hours else 24
                scraper.schedule_scraping(
                    lambda: scraper.scrape_weekend_box_office(),
                    hours
                )
                print(f"Scheduled scraping every {hours} hours")
                print("Scraper will run in the background. Press Ctrl+C to stop.")
                
                try:
                    while True:
                        time.sleep(1)
                except KeyboardInterrupt:
                    print("\nStopping scheduled scraping...")
                    break
            except ValueError:
                print("Invalid interval format")
        
        elif choice == '5':
            print("Goodbye!")
            break
        
        else:
            print("Invalid choice. Please try again.")


if __name__ == "__main__":
    main()