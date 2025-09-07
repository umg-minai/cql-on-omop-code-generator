## Introduction

This repository contains a code generator that consumes a CSV-formatted description of the [OMOP Common Data Model (CDM)](https://www.ohdsi.org/data-standardization/) and produces code and data artifacts that can be used with the [reference implementation](https://github.com/cqframework/clinical_quality_language) of the [Clinical Query Language (CQL)](https://cql.hl7.org/).
The generated code in combination with hand-written glue-code allows a variant of the above CQL engine, namely [cql-on-omop](https://github.com/umg-minai/cql-on-omop), to process clinical data which is stored in an SQL database system and conforms to the OMOP CDM.
The code generator works with versions 5.3 and 5.4 of the OMOP CDM and there is additional functionality for adapting the respective OMOP CDM schema so that the database can hold [MIMIC IV](https://github.com/OHDSI/MIMIC) data.
The contents of the repository is provided under the Apache 2.0 license (see `COPYING` file).

At the moment, the code generator can produce three kinds of artifacts

1. Code and data artifacts that allow the [cql-on-omop](https://github.com/umg-minai/cql-on-omop) CQL engine to work with OMOP-formatted SQL databases

2. Work-in-progress: Code artifacts that allow [Google's experimental CQL engine](https://github.com/google/cql) to work with OMOP-formatted SQL databases

3. SQL files which change the schema of an existing OMOP-formatted SQL database such that MIMIC IV data base be loaded into the database (the changes are relatively minor, mainly widening the types of certain columns)

## Usage

1. Obtain the definition of the OMOP CDM

   ```sh
   git clone https://github.com/OHDSI/CommonDataModel
   ```

   The relevant CSV-formatted descriptions of the data model can be found in the `inst/csv` sub-directory.

2. Make the code of this repository available for use

   Assuming [Quicklisp](https://beta.quicklisp.org) is installed in `${HOME}/quicklisp`

   ```sh
   cd ${HOME}/quicklisp/local-projects
   git clone https://github.com/umg-minai/cql-on-omop-code-generator
   ```

3. Start Lisp and invoke the code generator

   The code generator needs the following pieces of information

   * The location and desired version of the OMOP CDM

   * The kind of output that should be produced (TODO explain)

   * The target location into which the generated output should be placed

   ```sh
   sbcl
   * (ql:quickload "model-info-generator")
   [...]
   * (model-info-generator:generate-code (model-info-generator:omop-cdm "/tmp/commondatamodel" "v5.3") :java-project "/tmp/test/")
   ;; Loading OMOP CDM v5.4 from #P"/tmp/commondatamodel/"
   ;; Applying REMOVE-COHORT
   ;; Applying MANUAL-COMPOUND-KEYS
   ;; Applying ADD-COMPOUND-KEYS
   ;; Applying ADD-EXTRA-RELATIONS
   ;; Applying ADD-CONVERSIONS
   ;; Output in #P"/tmp/test/"
   ```

   To generate MIMIC-compatible artifacts, load the MIMIC extensions with

   ```cl
   (ql:quickload "model-info-generator/mimic")
   ```

   and change the above invocation to

   ```cl
   (model-info-generator:generate-code-for-mimic (model-info-generator:omop-cdm "/tmp/commondatamodel" "v5.3") :java-project "/tmp/test/")
   ```

   To generate SQL statements that change the schema of an OMOP CDM-formatted database to be compatible with MIMIC data, use

   ```cl
   (model-info-generator:generate-schema-changes-for-mimic (model-info-generator:omop-cdm "/tmp/commondatamodel" "v5.3") "/tmp/test/")
   ```
