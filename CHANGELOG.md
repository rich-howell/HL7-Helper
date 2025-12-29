# Changelog

All notable changes to this project will be documented here.  
This project follows [Keep a Changelog](https://keepachangelog.com/) style.

---

## [0.0.9] - 2025-12-29
### Maintenance
- Internal maintenance and release workflow.

## [0.0.8] - 2025-12-29
### Maintenance
- Internal maintenance and release workflow.

## [0.0.7] - 2025-12-29
### Maintenance
- Internal maintenance and release workflow.

## [0.0.6] - 2025-09-10
### Added
- Hover support for short-form checks like `segmentExists("AL1", 0)`.  
  - Hovering the quoted segment (e.g., `"AL1"`) shows the segment name with a spec link.  
  - Hovering the index (e.g., `0`) shows the segment occurrence (e.g., `AL1[0]`).
- Recognition of alphanumeric 3-character segment IDs (e.g., `AL1`, `Z99`) in hovers.

### Changed
- No changes to existing `getValue(...)`/path hover or CodeLens behaviour.

### Fixed
- Improved tolerance around chained string operations (e.g., `.substring(...)`, `.slice(...)`) when hovering HL7 calls, reducing missed hovers in complex expressions.

## [0.0.2] – 2025-09-06
### Added
- Autocomplete now triggers when **moving the cursor into** a segment/field/component slot, not just when typing.
- Completion items for fields and components:
  - Sorted numerically (`1, 2, 3, ...` instead of `1, 10, 11, ...`).
  - Replace the current token under the cursor instead of appending.
  - Always show the **full list** of options (no weird “3,30,31…” filtering).
  - Preselect the current value if present.
- More robust arg index detection using `getEffIndexAtPosition`.

### Fixed
- Segment completions inside quotes now insert correctly as `"PID"` (no more `""PID""`).

---

## [0.0.1] – 2025-09-01
### Added
- Initial release.
- Hover tooltips for segments, fields, and components.
- CodeLens links to HL7 spec pages.
- Diagnostics for invalid field/component indices.
- Basic autocomplete:
  - Segment names (PID, PV1, …).
  - Field numbers with labels.
  - Component numbers with labels.
- Support for both:
  - `msg.getValue(...)` (member form).
  - `hl7.getValue(msg, ...)` (wrapper form).
- Multi-line call support.
