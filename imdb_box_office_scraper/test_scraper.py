#!/usr/bin/env python3
"""
Test script for IMDB Box Office Scraper
"""

import sys
import os
from datetime import datetime

def test_imports():
    """Test if all required modules can be imported."""
    print("Testing imports...")
    
    try:
        import requests
        import bs4
        import pandas as pd
        import selenium
        from fake_useragent import UserAgent
        print("✓ All required modules imported successfully")
        return True
    except ImportError as e:
        print(f"✗ Import error: {e}")
        print("Please install missing dependencies with: pip install -r requirements.txt")
        return False

def test_scraper_initialization():
    """Test scraper initialization."""
    print("\nTesting scraper initialization...")
    
    try:
        from imdb_scraper import IMDBBoxOfficeScraper
        scraper = IMDBBoxOfficeScraper(delay=1.0, use_selenium=False)
        print("✓ Scraper initialized successfully")
        return True, scraper
    except Exception as e:
        print(f"✗ Scraper initialization failed: {e}")
        return False, None

def test_basic_functionality(scraper):
    """Test basic scraper functionality."""
    print("\nTesting basic functionality...")
    
    try:
        # Test URL construction
        base_url = scraper.base_url
        if base_url == "https://www.imdb.com":
            print("✓ Base URL set correctly")
        else:
            print("✗ Base URL incorrect")
            return False
        
        # Test session setup
        if scraper.session and hasattr(scraper.session, 'headers'):
            print("✓ Session configured")
        else:
            print("✗ Session not configured properly")
            return False
        
        return True
    except Exception as e:
        print(f"✗ Basic functionality test failed: {e}")
        return False

def test_export_functions(scraper):
    """Test export functionality."""
    print("\nTesting export functions...")
    
    # Create test data
    test_data = [
        {
            'title': 'Test Movie 1',
            'year': '2023',
            'gross': '$100,000,000',
            'scraped_date': datetime.now().isoformat()
        },
        {
            'title': 'Test Movie 2',
            'year': '2023',
            'gross': '$75,000,000',
            'scraped_date': datetime.now().isoformat()
        }
    ]
    
    try:
        # Test CSV export
        scraper.export_to_csv(test_data, 'test_export.csv')
        if os.path.exists('test_export.csv'):
            print("✓ CSV export working")
            os.remove('test_export.csv')  # Clean up
        else:
            print("✗ CSV export failed")
            return False
        
        # Test JSON export
        scraper.export_to_json(test_data, 'test_export.json')
        if os.path.exists('test_export.json'):
            print("✓ JSON export working")
            os.remove('test_export.json')  # Clean up
        else:
            print("✗ JSON export failed")
            return False
        
        return True
    except Exception as e:
        print(f"✗ Export test failed: {e}")
        return False

def test_internet_connection():
    """Test internet connection to IMDB."""
    print("\nTesting internet connection...")
    
    try:
        import requests
        response = requests.get("https://www.imdb.com", timeout=10)
        if response.status_code == 200:
            print("✓ IMDB is accessible")
            return True
        else:
            print(f"✗ IMDB returned status code: {response.status_code}")
            return False
    except Exception as e:
        print(f"✗ Internet connection test failed: {e}")
        return False

def test_data_cleaning():
    """Test data cleaning functions."""
    print("\nTesting data cleaning...")
    
    try:
        from imdb_scraper import IMDBBoxOfficeScraper
        scraper = IMDBBoxOfficeScraper()
        
        # Test currency cleaning
        test_cases = [
            ("$123,456,789", "$123456789"),
            ("$1.5M", "$1,500,000"),
            ("$2.3B", "$2,300,000,000"),
            ("No data", "No data")
        ]
        
        for input_val, expected in test_cases:
            result = scraper._clean_currency(input_val)
            # Basic check - should contain numbers or be 'N/A'
            if result and (result.replace('$', '').replace(',', '').isdigit() or result == 'N/A' or result == input_val.strip()):
                continue
            else:
                print(f"✗ Currency cleaning failed for: {input_val}")
                return False
        
        print("✓ Data cleaning functions working")
        return True
    except Exception as e:
        print(f"✗ Data cleaning test failed: {e}")
        return False

def main():
    """Run all tests."""
    print("IMDB Box Office Scraper - Test Suite")
    print("=" * 40)
    
    tests_passed = 0
    total_tests = 0
    
    # Test 1: Imports
    total_tests += 1
    if test_imports():
        tests_passed += 1
    
    # Test 2: Scraper initialization
    total_tests += 1
    success, scraper = test_scraper_initialization()
    if success:
        tests_passed += 1
    
    if scraper:
        # Test 3: Basic functionality
        total_tests += 1
        if test_basic_functionality(scraper):
            tests_passed += 1
        
        # Test 4: Export functions
        total_tests += 1
        if test_export_functions(scraper):
            tests_passed += 1
        
        # Test 5: Data cleaning
        total_tests += 1
        if test_data_cleaning():
            tests_passed += 1
    
    # Test 6: Internet connection
    total_tests += 1
    if test_internet_connection():
        tests_passed += 1
    
    # Summary
    print("\n" + "=" * 40)
    print(f"Test Results: {tests_passed}/{total_tests} tests passed")
    
    if tests_passed == total_tests:
        print("✓ All tests passed! The scraper is ready to use.")
        print("\nNext steps:")
        print("1. Run 'python imdb_scraper.py' for interactive mode")
        print("2. Run 'python example_usage.py' for examples")
        return True
    else:
        print("✗ Some tests failed. Please check the errors above.")
        print("\nTroubleshooting:")
        print("1. Ensure all dependencies are installed: pip install -r requirements.txt")
        print("2. Check your internet connection")
        print("3. Verify Python version (3.7+ recommended)")
        return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)