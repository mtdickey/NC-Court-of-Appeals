# -*- coding: utf-8 -*-
"""
Created on Thu Apr 21 10:46:46 2016

@author: Michael
"""

from bs4 import BeautifulSoup
import urllib2
from urllib2 import Request
from pdfminer.pdfparser import PDFParser
from pdfminer.pdfdocument import PDFDocument
from pdfminer.pdfpage import PDFPage
from pdfminer.pdfinterp import PDFResourceManager
from pdfminer.pdfinterp import PDFPageInterpreter
from pdfminer.layout import LAParams
from pdfminer.converter import  TextConverter # , XMLConverter, HTMLConverter
import re
from StringIO import StringIO
import pandas as pd

#Set up connection to base page and gather all links to calendars
url = 'https://appellate.nccourts.org/calendar.php?court=2'
response = urllib2.urlopen(url)
html = response.read()
soup = BeautifulSoup(html)

# Gather links for all calendars
tds = soup.find_all('td', {'style': 'text-align:center; padding: 1px; font-size: 8pt'})
links = []
for td in tds:
    link = td.find('a')
    if link is not None:
        links.append(link['href'])

allData = pd.DataFrame(columns = ['case', 'date', 'scheduled'])        
# Go through each link and store info for which cases are and are not scheduled for an oral argument
for link in links:
       
    # Open the url provided as an argument to the function and read the content
    open = urllib2.urlopen(Request(link)).read()
    
    # Cast to StringIO object
    memory_file = StringIO(open)
    
    # Create a PDF parser object associated with the StringIO object
    parser = PDFParser(memory_file)
    
    # Create a PDF document object that stores the document structure
    document = PDFDocument(parser)
    
    # Define parameters to the PDF device objet 
    rsrcmgr = PDFResourceManager()
    retstr = StringIO()
    laparams = LAParams()
    codec = 'utf-8'
    
    # Create a PDF device object
    device = TextConverter(rsrcmgr, retstr, codec = codec, laparams = laparams)
    
    # Create a PDF interpreter object
    interpreter = PDFPageInterpreter(rsrcmgr, device)
    
    # Process each page contained in the document
    for page in PDFPage.create_pages(document):
        interpreter.process_page(page)
        data =  retstr.getvalue()
    
     # Get case names matching the regex pattern # (PA) #
    reg = '\d+-\d+'
    cases = re.findall(reg, data)
    caseIter = [(m.start(0), m.end(0)) for m in re.finditer(reg, data)]
    
    dates = []
    for i in range(len(cases)):    
        dates.append(re.findall('\d+ [A-Z][a-z]+ 20\d{2}',data[0:caseIter[i][1]])[len(re.findall('\d+ [A-Z][a-z]+ 20\d{2}',data[0:caseIter[i][1]]))-1])

    scheduled = []
    for i in range(len(cases)):
        scheduled.append(re.findall('(ARE SCHEDULED)|(WITHOUT)',data[0:caseIter[i][1]])[len(re.findall('(ARE SCHEDULED)|(WITHOUT)',data[0:caseIter[i][1]]))-1])

    linkData = {'case': cases, 'date': dates, 'scheduled': scheduled}
    linkData = pd.DataFrame(linkData)
    
    allData = allData.append(linkData)
    print(dates[0])

allData.to_csv('calendars.csv')