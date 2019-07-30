from optparse import OptionParser
from time import sleep
import json
import urllib.parse as urlparse
import os

import progressbar
from selenium import webdriver



class Error(Exception):
    """Base class for exceptions in this module."""
    pass


class InputError(Error):
    """Exception raised for errors in the input.

    Attributes:
        msg  -- explanation of the error
    """

    def __init__(self, msg):
        self.msg = msg


parser = OptionParser()
parser.add_option("-u", "--username", dest="user", help="DHS username", metavar="STRING")
parser.add_option("-p", "--password", dest="password", default=False, help="DHS password", metavar="STRING")
parser.add_option("-r", "--project", dest="proj", default=1, help="Project index", metavar="INTEGER")
parser.add_option("-d", "--download", dest="download", default="/home/alex/git/p20_indicator_time_trends/data/mislist.txt", help="Bulk download PATH", metavar="STRING")
parser.add_option("-o", "--output", dest="output", default="/home/alex/Survey Microdata/MISauto", help="Output path.", metavar="FOLDER")
(options, args) = parser.parse_args()


def input_text(browser, inputs):
    # Fills a list of text boxes
    #
    # inputs: [{"input_id": "someId", "input_str": "some string"}, ... ]

    # This will cause Selenium to wait until the element is found.
    browser.find_element_by_xpath('//*[@id="{}"]'.format(inputs[0]["input_id"]))
    # browser.find_element_by_id(inputs[0]["input_id"])

    # Selenium is very slow at traversing the DOM.
    # To quickly input text in many boxes, we inject a
    # javacript function into the iframe. The collection
    # of textbox ids and strings is serialized
    # as a Javascript object literal using the json module.
    inputs = json.dumps(inputs)
    js = "var inputs = {};".format(inputs)
    js += """
    console.log(inputs)
    for (var k = 0; k < inputs.length; k++) {
        var inputStr = inputs[k]["input_str"];
        var input = document.getElementById(inputs[k]["input_id"]);
        input.value = inputStr;
    }
    return true;"""
    browser.execute_script(js)


if not options.password:
    raise InputError("A valid password was not supplied.")

chrome_options = webdriver.ChromeOptions()
chrome_options.binary_location = "/usr/lib/chromium-browser/chromium-browser"
# chrome_options.add_argument("--headless") # Runs Chrome in headless mode.
chrome_options.add_argument('--no-sandbox')  # Bypass OS security model
chrome_options.add_argument('--disable-gpu')  # applicable to windows os only
chrome_options.add_argument('start-maximized')
chrome_options.add_argument('disable-infobars')
chrome_options.add_argument("--disable-extensions")
chrome_options.add_argument('--disable-gpu')
prefs = {"download.default_directory": options.output, "directory_upgrade": True, "extensions_to_open": "", "profile.default_content_settings.popups": 0}
chrome_options.add_experimental_option("prefs", prefs)

browser = webdriver.Chrome("/home/alex/chromedriver", chrome_options=chrome_options)
browser.implicitly_wait(30)  # Configure the WebDriver to wait up to 30 seconds for each page to load

browser.get("https://dhsprogram.com/data/dataset_admin")
queries = []
userInput = {}
userInput["input_id"] = "UserName"
userInput["input_str"] = options.user
queries.append(userInput)
passInput = {}
passInput["input_id"] = "Password"
passInput["input_str"] = options.password
queries.append(passInput)
input_text(browser, queries)
browser.find_element_by_xpath('//*[@name="submit"]').click()  # Click the submit button
browser.find_element_by_xpath("//*[@name='proj_id']/option[{}]".format(options.proj+1)).click()  # Click on the project option in the drop down

links = open(options.download).read().splitlines()

for link in progressbar.progressbar(links):
    if link != "":
        link_parsed = urlparse.urlparse(link)
        link_filename = urlparse.parse_qs(link_parsed.query)['Filename'][0]
        link_filename_base, link_filename_ext = os.path.splitext(link_filename)
        link_filename_upper = link_filename_base + link_filename_ext.upper()
        link_filename_lower = link_filename_base + link_filename_ext.lower()
        link_filepath_upper = os.path.join(options.output, link_filename_upper)
        link_filepath_lower = os.path.join(options.output, link_filename_lower)
        if not os.path.isfile(link_filepath_upper) and not os.path.isfile(link_filepath_lower):
            browser.get(link)
            sleep(1)

browser.close()
