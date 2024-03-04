[![stability-experimental](https://img.shields.io/badge/stability-experimental-orange.svg)](https://github.com/emersion/stability-badges#experimental)

# DEPRECATED/NOT WORKING ADT_Backend

Backend implementation of ABAP Development Tools (ADT) REST endpoints. These endpoints are required to connect the [abapGit repositories plugin](https://eclipse.abapgit.org/updatesite/) to the abapgit backend.

SAP has also published it's [official sources](https://github.com/SAP/project-odense) as they are provided in the SAP Cloud Platform ABAP Environment product.

**Highly Experimental**

### Requirements

1. [abapGit](https://github.com/larshp/abapGit) version with source code installed (report only won't work)
2. NW release ≥ 750

To fulfill requirement no. 1 you have to import [this repository](https://github.com/abapGit/abapGit/) via abapGit into your SAP system and activate the imported objects. A step by step instruction can be found [here](https://docs.abapgit.org/guide-online-install.html). Installation and execution of the abapGit report (non-developer version of abapGit) won't be sufficient.

### Links

- https://github.com/SAP/project-odense
- https://www.sap.com/documents/2013/04/12289ce1-527c-0010-82c7-eda71af511fa.html
- https://wiki.scn.sap.com/wiki/display/ABAP/SAPlink+plugin+for+ABAP+in+Eclipse
- https://blogs.sap.com/2014/08/12/an-example-to-help-you-understand-how-does-adt-work/
