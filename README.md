# Rapid Policy Analysis Tool (RPAT)

RPAT is part of AASHTO's [TravelWorks](https://planningtools.transportation.org/10/travelworks.html) advanced travel analysis tools.  To begin, download and run the RPAT installer [here](https://planningtools.transportation.org/551/rapid-policy-analysis-tool.html).  RPAT is open source [(GPL)] (https://en.wikipedia.org/wiki/GNU_General_Public_License).

#Dependencies

To build the Python executable, you will need python 3 and the libraries pyinstaller, cherrypy, setuptools, psutil and simplejson.

To build the installer, you will need to have Inno Setup installed.

#Create Installer

To create the installer for RPAT, you will need to first create the python executable then create the main package which includes an R installer, R scripts.

To create the python executable, edit the app.spec file to point to the location of your file and run pyinstaller app.spec.

To create the installer, move the python distribution files, so it matches the location in the RPAT_Installer.iss and build the installer using Inno Setup.

#Related

VisionEval - https://github.com/gregorbj/VisionEval

RSPM - https://github.com/gregorbj/RSPM

RPAT Viewer Pilot - https://github.com/gregorbj/RPAT_Viewer_Pilot

EERPAT - https://github.com/RSGInc/EERPAT
