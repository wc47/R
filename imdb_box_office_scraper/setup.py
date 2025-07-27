#!/usr/bin/env python3
"""
Setup script for IMDB Box Office Scraper
"""

import os
import sys
import subprocess
import platform

def check_python_version():
    """Check if Python version is compatible."""
    version = sys.version_info
    if version.major == 3 and version.minor >= 7:
        print(f"✓ Python {version.major}.{version.minor}.{version.micro} is compatible")
        return True
    else:
        print(f"✗ Python {version.major}.{version.minor}.{version.micro} is not compatible")
        print("Please upgrade to Python 3.7 or higher")
        return False

def install_dependencies():
    """Install required dependencies."""
    print("Installing dependencies...")
    
    try:
        subprocess.check_call([sys.executable, "-m", "pip", "install", "-r", "requirements.txt"])
        print("✓ Dependencies installed successfully")
        return True
    except subprocess.CalledProcessError as e:
        print(f"✗ Failed to install dependencies: {e}")
        return False

def create_directories():
    """Create necessary directories."""
    print("Creating directories...")
    
    directories = ['data', 'logs', 'exports']
    
    for directory in directories:
        try:
            os.makedirs(directory, exist_ok=True)
            print(f"✓ Created directory: {directory}")
        except Exception as e:
            print(f"✗ Failed to create directory {directory}: {e}")
            return False
    
    return True

def setup_chrome_driver():
    """Setup Chrome driver for Selenium (optional)."""
    print("Setting up Chrome driver...")
    
    try:
        # Check if Chrome is available
        chrome_path = None
        system = platform.system().lower()
        
        if system == "linux":
            chrome_paths = [
                "/usr/bin/google-chrome",
                "/usr/bin/google-chrome-stable",
                "/usr/bin/chromium-browser",
                "/usr/bin/chromium"
            ]
        elif system == "darwin":  # macOS
            chrome_paths = [
                "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
            ]
        elif system == "windows":
            chrome_paths = [
                "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe",
                "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe"
            ]
        else:
            chrome_paths = []
        
        for path in chrome_paths:
            if os.path.exists(path):
                chrome_path = path
                break
        
        if chrome_path:
            print(f"✓ Chrome found at: {chrome_path}")
        else:
            print("⚠ Chrome not found. Selenium features may not work.")
            print("Please install Google Chrome or Chromium for full functionality.")
        
        return True
    except Exception as e:
        print(f"⚠ Chrome driver setup warning: {e}")
        return True  # Non-critical, return True to continue

def run_tests():
    """Run basic tests to verify setup."""
    print("Running setup tests...")
    
    try:
        result = subprocess.run([sys.executable, "test_scraper.py"], 
                              capture_output=True, text=True)
        
        if result.returncode == 0:
            print("✓ All tests passed")
            return True
        else:
            print("✗ Some tests failed")
            print("Test output:")
            print(result.stdout)
            print(result.stderr)
            return False
    except Exception as e:
        print(f"✗ Failed to run tests: {e}")
        return False

def create_sample_config():
    """Create a sample configuration file."""
    print("Creating sample configuration...")
    
    config_content = '''# IMDB Box Office Scraper Configuration
# Copy this file to .env and modify as needed

# Scraping settings
SCRAPER_DELAY=1.0
USE_SELENIUM=False
MAX_RETRIES=3

# Export settings
EXPORT_FORMAT=csv
EXPORT_DIRECTORY=data

# Logging
LOG_LEVEL=INFO
LOG_FILE=imdb_scraper.log

# Rate limiting
REQUESTS_PER_MINUTE=30
'''
    
    try:
        with open('config.env.sample', 'w') as f:
            f.write(config_content)
        print("✓ Sample configuration created: config.env.sample")
        return True
    except Exception as e:
        print(f"✗ Failed to create sample config: {e}")
        return False

def display_usage_info():
    """Display usage information."""
    print("\n" + "=" * 50)
    print("IMDB Box Office Scraper Setup Complete!")
    print("=" * 50)
    print("\nUsage:")
    print("1. Interactive mode:")
    print("   python imdb_scraper.py")
    print("\n2. Run examples:")
    print("   python example_usage.py")
    print("\n3. Run tests:")
    print("   python test_scraper.py")
    print("\n4. Import in your code:")
    print("   from imdb_scraper import IMDBBoxOfficeScraper")
    print("\nFiles created:")
    print("- data/ (for exports)")
    print("- logs/ (for log files)")
    print("- config.env.sample (sample configuration)")
    print("\nFor more information, see README.md")

def main():
    """Main setup function."""
    print("IMDB Box Office Scraper - Setup Script")
    print("=" * 40)
    
    setup_steps = [
        ("Checking Python version", check_python_version),
        ("Installing dependencies", install_dependencies),
        ("Creating directories", create_directories),
        ("Setting up Chrome driver", setup_chrome_driver),
        ("Creating sample config", create_sample_config),
        ("Running tests", run_tests)
    ]
    
    failed_steps = []
    
    for step_name, step_func in setup_steps:
        print(f"\n{step_name}...")
        if not step_func():
            failed_steps.append(step_name)
    
    print("\n" + "=" * 40)
    print("Setup Summary")
    print("=" * 40)
    
    if not failed_steps:
        print("✓ Setup completed successfully!")
        display_usage_info()
        return True
    else:
        print("✗ Setup completed with errors:")
        for step in failed_steps:
            print(f"  - {step}")
        print("\nPlease fix the errors above and run setup again.")
        return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)