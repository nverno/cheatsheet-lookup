#!/usr/bin/env python

"""
Create index of cheatsheets from http://cheat-sheets.org
"""

import sys
from bs4 import BeautifulSoup

if sys.version_info[0] == 3:
    import urllib.request as urllib
    from urllib.error import HTTPError
else:
    import urllib2 as urllib
    from urllib2 import HTTPError


class IndexProcessor:
    """
    Extract cheatsheet index from cheat-sheets.org.
    """

    def __init__(self, url):
        self.sheets = {}
        self.soup = None
        try:
            html = urllib.urlopen(url).read()
            self.soup = BeautifulSoup(html)
            self.get_sheets()

        except HTTPError as e:
            print(e)

    def get_sheets(self):
        menu = self.soup.find("ul", {"class": "menu"})
        for l in menu.findAll("a"):
            if 'href' in l.attrs:
                name = l.get_text()
                refid = l.attrs['href'][1:]

                # ------------------------------------------------------------
                # get the cheetsheet refs
                try:
                    table = self.soup.find('a', {'id': refid}).parent.parent
                    self.sheets[name] = {'id': refid, 'sheets': {}}

                    # print 'getting sheets for %s' % name
                    # print table

                    for item in table.find('ul', {'class': 'items'}).findAll('li'):
                        if item.span.text is not None:
                            hrefs = [i.attrs['href']
                                     for i in item.findAll('a')]
                            types = [i.attrs['alt']
                                     for i in item.findAll('img')]
                            for i in range(len(types)):
                                if types[i] == 'saved':
                                    hrefs[
                                        i] = "http://www.cheat-sheets.org/" + hrefs[i]

                            self.sheets[name]['sheets'][item.span.text] = {
                                'hrefs': hrefs,
                                'types': types
                            }
                except:
                    print("Failed for %s" % name)


if __name__ == '__main__':
    # import optparse
    # parser = optparse.OptionParser(__doc__.strip())
    # parser.add_option("-o", "--output",
    #                   help="Output file", default="cheatsheets-org.json",
    #                   dest="outfile")
    # (opts, args) = parser.parse_args()

    if len(sys.argv) < 3:
        print(f"usage: {sys.argv[0]} <output file> <input uri>")
        sys.exit(1)

    # ip = IndexProcessor("http://www.cheat-sheets.org/")
    ip = IndexProcessor(sys.argv[2])
    import json
    with open(sys.argv[1], "w") as f:
        json.dump(ip.sheets, f)
