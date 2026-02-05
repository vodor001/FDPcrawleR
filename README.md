# **FDPcrawleR**

**Assessing metadata completeness of FAIR Data Points (FDPs) registered in the ERDERA Virtual Platform.**

**FDPcrawleR** is an **R-based crawler** that evaluates how completely FAIR Data Points expose **DCAT-2 metadata**, from registration in the VP Index down to **datasets, distributions, and data services**.  
Results are summarized as a **Sankey diagram** showing metadata depth across FDPs.

---

## **What it does**

Starting from the **ERDERA VP Index**, the tool:

- selects **valid FDPs** (**Active** and **Inactive**)
- crawls hierarchical **DCAT-2 metadata**  
  (**FDP → Catalog → Dataset → Distribution → Data Service**)
- checks whether **each metadata layer is present**
- quantifies **metadata completeness** across the FDP ecosystem
- visualizes results as a **Sankey diagram**

The analysis focuses on **metadata presence and structure**, not on **data quality** or **endpoint availability**.

---

## **Metadata explanation**

FDPs adopt **DCAT-2** metadata structure:

- **Datasets** represent abstract data assets 
- **Distributions** represent concrete dataset representations** (files, APIs)  
- **Data services** provide access via endpoints (e.g. Beacon, SPARQL)

Traversal is implemented using **DCAT-2** and **FDP Ontology (FDP-O)** predicates.

---

## **Input**

- **ERDERA Virtual Platform FDP Index** (via API)  
- **RDF metadata** exposed by FDPs (**Turtle format**)

---

## **Output**

- Counts of FDPs exposing:
  - **catalogs**
  - **datasets**
  - **distributions**
  - **dataset-dependent data services**
- **Complementary gap metrics** (e.g. catalogs without datasets)
- A **Sankey diagram** summarizing **metadata completeness**

---

## **How to run**

### **Requirements**

- **R ≥ 4.0**

### **R packages**

```r
install.packages(c("httr", "jsonlite", "rdflib", "dplyr", "plotly"))
