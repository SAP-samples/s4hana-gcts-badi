<!--- Register repository https://api.reuse.software/register, then add REUSE badge:
[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/REPO-NAME)](https://api.reuse.software/info/github.com/SAP-samples/REPO-NAME)
-->
[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/s4hana-gcts-badi)](https://api.reuse.software/info/github.com/SAP-samples/s4hana-gcts-badi)

# Integrating gCTS with Transport Organizer processes

## Description
Git-enabled Change and Transport System (gCTS) enables you to manage your ABAP change and transport management processes using Git as an external version management system. You can use it to leverage the functions of Git repositories and to set up continuous integration and delivery processes (CI/CD) for ABAP. The aim of gCTS is to allow DevOps processes for ABAP, as well.

* You can find more information about gCTS in general on the SAP Help Portal at [Git-Enabled Change and Transport System](https://help.sap.com/docs/ABAP_PLATFORM_NEW/4a368c163b08418890a406d413933ba7/f319b168e87e42149e25e13c08d002b9.html)
* You can use the coding provided in this repository if you would like to integrate your ABAP development processes with gCTS to enable task-based committing and to integrate the gCTS Regsitry in the process of releasing a task
* You can find a detailed description of the BAdI implemetation, its options and functionality in this SAP Community blog post [Integrating gCTS with Transport Organizer processes](https://blogs.sap.com/2022/05/24/integrating-gcts-with-transport-organizer-processes/)

## Requirements
- your ABAP system is on SAP S/4HANA 2020 SPS2 at least
- you have gCTS configured
- you have SAP Note 3204481 - [gCTS: Fix for missing objects when importing into a repository with a subdirectory](https://launchpad.support.sap.com/#/notes/3204481) implemented 

## Download and Installation
- fork this repository
- connect your ABAP development system to the forked repository via gCTS
- clone it to your ABAP development system
A detailed description is provided in the blog post [Integrating gCTS with Transport Organizer processes](https://blogs.sap.com/2022/05/24/integrating-gcts-with-transport-organizer-processes/) in SAP Community

## Known Issues
None currently known

## How to obtain support
[Create an issue](https://github.com/SAP-samples/s4hana-gcts-badi/issues) in this repository if you find a bug or have questions about the content.
 
For additional support, [ask a question in SAP Community](https://answers.sap.com/questions/ask.html). Use the tag 'Software Logistics - Change Control and Transport'

## Contributing
If you wish to contribute code, offer fixes or improvements, please send a pull request. Due to legal reasons, contributors will be asked to accept a DCO when they create the first pull request to this project. This happens in an automated fashion during the submission process. SAP uses [the standard DCO text of the Linux Foundation](https://developercertificate.org/).

## License
Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSES/Apache-2.0.txt) file.
