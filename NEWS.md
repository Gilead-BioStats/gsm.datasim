# gsm.datasim v1.1.1
This release adds new contributor guidelines and standardized issue templates.

# gsm.datasim v1.1.0

## Notable Changes:
**New Mapping Workflows Support:**
- Additional workflow support for "IE" domain
- dates/timestamp support for many domains now available.

# gsm.datasim v1.0.0

We are excited to announce the first major release of the `gsm.datasim` package, 
a collection of functions to generate synthetic test data for the RBQM of Clinical Trials based on several parameters.

## Notable Changes:
**User-facing functions**
- `generate_rawdata_for_single_study()` can create snapshot(s) for a single study when provided with the proper parameters and appropriate mapping specifications.
- `raw_data_generator()` is a wrapper to run `generate_rawdata_for_single_study()` to create multiple studies if providing a template/dataset containing a variety of these parameters.

**New Mapping Workflows Support:**
- All workflows that exist in `gsm.mapping`'s `inst/workflow/1_mappings` are now supported, 
with "STUDY", "SUBJ", "SITE" and "ENROLL" always being required. 
- There are many `gsm.endpoints` specific domains, such as `Anticancer` and `OverallResponse` 
that now have support as well.

**Replacing `clindata` with `gsm.datasim`:**
- The object `gsm.core::lSource` was created using `gsm.datasim` for examples, tests, and vignettes across the `gsm` ecosystem.
This object is based on "core mappings" which include: "AE", "COUNTRY", "DATACHG", "DATAENT", "ENROLL", "LB", "PD", "PK", "QUERY", "STUDY", "STUDCOMP", "SDRGCOMP", "SITE", "SUBJ" 
