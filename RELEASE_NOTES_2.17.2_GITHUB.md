# v2.17.2 - M365 Defender API Deprecation & Bug Fixes

## Summary

This critical maintenance release addresses the deprecation of the Microsoft 365 Defender/Defender XDR Advanced Hunting API endpoint. The release ensures continued functionality while providing clear guidance for users to migrate to the Microsoft Graph Security Hunting API.

## ⚠️ Action Required for M365D Users

Microsoft has deprecated the M365 Defender/Defender XDR Advanced Hunting API endpoint. While your queries will continue to work (they'll automatically use the MDE endpoint), **we recommend migrating to the Microsoft Graph Security Hunting API** (`M365DGraph`):

```python
# Instead of:
qry_prov = mp.QueryProvider("M365D")

# Use:
qry_prov = mp.QueryProvider("M365DGraph")
```

**Why migrate:** The Microsoft Graph Security API is the recommended and supported endpoint with better features and performance.

## 🐛 Bug Fixes

### Microsoft Defender Provider Configuration
- Fixed spurious warning when using "Default" MicrosoftDefender configuration in `msticpyconfig.yaml`
- Improved configuration lookup logic in OData driver to prevent incorrect key handling

### Azure Monitor Driver
- Fixed datetime formatting issues by removing deprecated methods
- Improved compatibility and reliability of datetime handling

### M365 Defender Queries
- Updated multiple M365D queries to work correctly with the new endpoint configuration
- Ensured consistency between MDE and M365D test cases and mock data

## 🛠 Code Quality Improvements

- Removed unused imports and methods
- Updated log messages for better clarity and actionable guidance
- Fixed documentation typos
- Enhanced test coverage and consistency

## 📋 Technical Details

- **26 files changed:** 95 additions, 111 deletions
- **Primary Changes:**
  - `msticpy/data/drivers/mdatp_driver.py` - M365D API deprecation logic
  - `msticpy/data/drivers/odata_driver.py` - Configuration handling fix
  - `msticpy/data/drivers/azure_monitor_driver.py` - Datetime formatting fix
  - Multiple test files updated for new endpoint behavior

## 📚 Resources

- **Full Release Notes:** See [RELEASE_NOTES_2.17.2.md](https://github.com/microsoft/msticpy/blob/main/RELEASE_NOTES_2.17.2.md) for detailed information
- **PR:** [#863 - Fix for removal of Defender XDR endpoint](https://github.com/microsoft/msticpy/pull/863)
- **Documentation:** [MSTICPy Docs](https://msticpy.readthedocs.io/)
- **Migration Guide:** [Data Providers Documentation](https://msticpy.readthedocs.io/en/latest/data_acquisition/DataProviders.html)

## 🙏 Contributors

- @ianhelle - Implementation and fixes
- @ryan-detect-dot-dev - Review

**Full Changelog**: https://github.com/microsoft/msticpy/compare/v2.17.1...v2.17.2
