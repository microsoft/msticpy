# Release Notes for v2.17.2

## Summary

This is a critical maintenance release that addresses the deprecation of the Microsoft 365 Defender/Defender XDR Advanced Hunting API endpoint. The release ensures continued functionality by consolidating M365 Defender queries to use the Microsoft Defender for Endpoint (MDE) Advanced Queries API, while providing clear guidance for users to migrate to the Microsoft Graph Security Hunting API (`M365DGraph`) for future implementations.

Additionally, this release includes important bug fixes for Azure Monitor datetime handling and configuration management improvements for Microsoft Defender providers.

## ⚠️ Breaking Changes & Migration Guide

### M365 Defender API Endpoint Deprecation (Action Required)

**What Changed:**
Microsoft has deprecated the M365 Defender/Defender XDR Advanced Hunting API endpoint. As of this release, all queries for the `M365D` environment will automatically revert to using the MDE Advanced Queries API endpoint.

**Impact:**
- Users querying with the `M365D` data environment will see a deprecation warning
- Queries will continue to work but will use the MDE endpoint instead
- No immediate action required for basic functionality

**Recommended Migration Path:**
For users currently using the `M365D` environment, we recommend migrating to the **Microsoft Graph Security Hunting API** (`M365DGraph`) for optimal performance and future-proofing:

```python
# Instead of:
qry_prov = mp.QueryProvider("M365D")

# Use:
qry_prov = mp.QueryProvider("M365DGraph")
```

**Why Migrate:**
- Microsoft Graph Security API is the recommended and supported endpoint
- Better feature support and performance
- Future updates and improvements will focus on the Graph API

## 🐛 Bug Fixes

### Microsoft Defender Provider Configuration
- **Fixed:** Removed spurious warning when using "Default" MicrosoftDefender configuration in `msticpyconfig.yaml`
- **Details:** The configuration logic was incorrectly appending "-Default" to configuration keys, causing incorrect lookups and unnecessary warnings. This has been resolved by improving the logic in `_get_driver_settings` in the OData driver.
- **Files affected:** `msticpy/data/drivers/odata_driver.py`

### Azure Monitor Driver Datetime Handling
- **Fixed:** Datetime formatting issues in Azure Monitor driver
- **Details:** Removed deprecated datetime formatting method and updated to use standard formatters, improving compatibility and reliability
- **Files affected:** `msticpy/data/drivers/azure_monitor_driver.py`

### Microsoft 365 Defender Queries
- **Fixed:** Updated several M365D queries to work correctly with the new endpoint configuration
- **Details:** Test cases and mock data updated to reflect that `M365D` now uses the same endpoints and parameters as `MDE`, ensuring consistency across environments

## 🛠 Code Quality & Maintenance

### Code Cleanup
- Removed unused `get_m365d_endpoint` import from `mdatp_driver.py`
- Updated log messages for improved clarity and actionable guidance
- Fixed typo in docstring (`_value_or_default` method: "emtpy" → "empty")
- Removed unused imports and methods (e.g., `datetime` import in Azure Monitor driver)

### Test Coverage
- Updated test cases to reflect new M365D→MDE endpoint consolidation
- Ensured mock data consistency across MDE and M365D environments
- Removed references to deprecated API endpoints in tests

## 📋 Technical Details

### Files Modified (26 files changed)
- **Core Driver Updates:**
  - `msticpy/data/drivers/mdatp_driver.py` - M365D API deprecation logic
  - `msticpy/data/drivers/odata_driver.py` - Configuration handling fix
  - `msticpy/data/drivers/azure_monitor_driver.py` - Datetime formatting fix
  - `msticpy/data/core/query_source.py` - Documentation fix

- **Test Updates:**
  - Multiple test files updated to reflect new endpoint behavior
  - Mock data standardized across MDE/M365D tests

- **Version:**
  - `msticpy/_version.py` - Version bumped to 2.17.2

### Statistics
- **95 additions, 111 deletions** across 26 files
- 7 commits addressing various aspects of the deprecation and fixes

## 📚 Additional Resources

- **PR:** [#863 - Fix for removal of Defender XDR endpoint](https://github.com/microsoft/msticpy/pull/863)
- **Documentation:** [MSTICPy Documentation](https://msticpy.readthedocs.io/)
- **Migration Guide:** See the [Data Providers documentation](https://msticpy.readthedocs.io/en/latest/data_acquisition/DataProviders.html) for details on using M365DGraph

## 🙏 Contributors

Special thanks to:
- @ianhelle for implementing the deprecation handling and fixes
- @ryan-detect-dot-dev for review

## 📅 Release Information

- **Release Date:** October 31, 2025
- **Previous Version:** v2.17.1
- **Next Planned Version:** TBD

## 🔗 Links

- **GitHub Release:** https://github.com/microsoft/msticpy/releases/tag/v2.17.2
- **PyPI Package:** https://pypi.org/project/msticpy/2.17.2/
- **Full Changelog:** https://github.com/microsoft/msticpy/compare/v2.17.1...v2.17.2
